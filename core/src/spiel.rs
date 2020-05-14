use crate::karte::Karte;
use crate::regelsatz::{SoloArt, Regelsatz, RegelsatzRegistry, RegelVariante, kartenwert};
use std::sync::Arc;
use std::error::Error;
use crate::spiel::SpielerAktionError::{NichtAnDerReihe, SoloAnsagenNochNichtVorbei, SoloAnsagenVorbei, KarteNichtAufDerHand, NichtBedient};
use crate::karte::Hoehe::*;
use crate::karte::Farbe::*;
use std::ops::Add;


#[derive(Copy,Clone,Eq,PartialEq)]
pub struct SpielerNummer {
    pub idx: u8,
}
impl SpielerNummer {
    pub fn naechster(&self) -> SpielerNummer {
        SpielerNummer { idx: (self.idx + 1) & 3 }
    }
    pub fn as_usize(&self) -> usize {
        self.idx as usize
    }
}
impl Add<usize> for SpielerNummer {
    type Output = SpielerNummer;

    fn add(self, rhs: usize) -> Self::Output {
        SpielerNummer {idx: (self.idx + rhs as u8) & 3}
    }
}



pub enum SpielerAktion {
    /// 'kein Solo' muss ausdrücklich gemeldet werden - 'None' bedeuetet kein Solo
    Solo(SpielerNummer, Option<SoloArt>),
    //TODO Hochzeit
    Karte(SpielerNummer, Karte),
    Contra(SpielerNummer),
    Re(SpielerNummer),
    ControKeine90(SpielerNummer),
    ReKeine90(SpielerNummer),
    //TODO weitere Ansagen
}

pub enum SpielerAktionError {
    NichtAnDerReihe,
    SoloAnsagenNochNichtVorbei,
    SoloAnsagenVorbei,
    KarteNichtAufDerHand,
    NichtBedient,
}

pub struct Spiel  {
    regelsatz: Box<dyn Regelsatz>,
    regelsatz_registry: Arc<RegelsatzRegistry>,

    solo_ansagen_vorbei: bool,
    /// Solospieler ist bei angesagtem Solo in self.erster_spieler
    angesagtes_solo: Option<SoloArt>,

    is_spiel_beendet: bool,

    is_re: [bool;4],

    /// Wer spielt den ersten Stich (regulaer) auf (Ausnahme bei Solo)
    erster_spieler: SpielerNummer,
    journal: Vec<SpielerAktion>,
    handkarten: [Vec<Karte>;4],
    /// abgelegte Karten sind nicht mehr nach Stichen gruppiert, sondern einfach ein Vec je Spieler
    stiche: [Vec<Karte>;4],

    /// direkt nach einem Stich der Spieler, der den Stich bekommen hat, ansonsten im Stich der 'nächste' Spieler in der Reihenfolge
    naechster_spieler: SpielerNummer,
    aktueller_stich: Vec<Karte>,
}

impl Spiel {
    const KREUZ_DAME: Karte = Karte {farbe: Kreuz, hoehe: Dame};

    pub fn new(regel_registry: Arc<RegelsatzRegistry>, erster_spieler: u8) -> Spiel {
        assert!(erster_spieler < 4);

        use rand::thread_rng;
        use rand::seq::SliceRandom;
        let mut kartensatz = regel_registry.variante.kartensatz();
        (&mut kartensatz).shuffle(&mut thread_rng()); //TODO Seed zum Reproduzieren?

        let mut chunks = kartensatz.chunks(kartensatz.len()/4);
        let k1 = chunks.next().unwrap().to_vec();
        let k2 = chunks.next().unwrap().to_vec();
        let k3 = chunks.next().unwrap().to_vec();
        let k4 = chunks.next().unwrap().to_vec();

        Spiel {
            regelsatz: regel_registry.normal(),
            regelsatz_registry: regel_registry,

            solo_ansagen_vorbei: false,
            angesagtes_solo: None,

            is_spiel_beendet: false,

            is_re: [
                k1.contains(&Spiel::KREUZ_DAME),
                k2.contains(&Spiel::KREUZ_DAME),
                k3.contains(&Spiel::KREUZ_DAME),
                k4.contains(&Spiel::KREUZ_DAME)],

            erster_spieler: SpielerNummer {idx: erster_spieler},
            journal: vec![],
            handkarten: [k1, k2, k3, k4],
            stiche: [vec![], vec![], vec![], vec![]],
            naechster_spieler: SpielerNummer {idx: erster_spieler},
            aktueller_stich: vec![]
        }
    }

    /// Diese Methode ist das komplette API, durch das Spieler (echte oder KIs) mit einem
    ///  laufenden Spiel interagieren können. Aktionen werden darauf überprüft, ob sie regelkonform
    ///  sind, zum aktuellen Zeitpunkt möglich sind und der Spieler gerade an der Reihe ist.
    pub fn spieler_aktion(&mut self, aktion: SpielerAktion) -> Result<(),SpielerAktionError>{
        if !self.solo_ansagen_vorbei {
            self.spieler_aktion_solo_ansagen(aktion)
        }
        else {
            self.spieler_aktion_regulaer(aktion)
        }
    }

    fn naechster_spieler(&mut self) {
        self.naechster_spieler = self.naechster_spieler.naechster();
    }

    fn spieler_aktion_solo_ansagen(&mut self, aktion: SpielerAktion) -> Result<(),SpielerAktionError> {
        match aktion {
            SpielerAktion::Solo(spieler, Some(solo_art)) => {
                self.check_an_der_reihe(spieler)?;

                self.solo_ansagen_vorbei = true;
                self.angesagtes_solo = Some(solo_art);
                self.erster_spieler = spieler;
                self.naechster_spieler = spieler;
                self.regelsatz = self.regelsatz_registry.solo(solo_art);

                self.is_re = [false, false, false, false];
                self.is_re[spieler.as_usize()] = true;

                Ok(())
            }
            SpielerAktion::Solo(spieler, None) => {
                self.check_an_der_reihe(spieler)?;
                self.naechster_spieler();
                if self.naechster_spieler == self.erster_spieler {
                    self.solo_ansagen_vorbei = true;
                }
                Ok(())
            },
            _ => {
                Err(SoloAnsagenNochNichtVorbei)
            }
        }
    }

    fn check_an_der_reihe(&self, spieler: SpielerNummer) -> Result<(), SpielerAktionError> {
        if spieler == self.naechster_spieler {
            Ok(())
        }
        else {
            Err(NichtAnDerReihe)
        }
    }

    fn index_of(karten: &Vec<Karte>, karte: Karte) -> Result<usize, SpielerAktionError> {
        if let Some(idx) = karten.iter().position(|&el| el == karte) {
            Ok(idx)
        }
        else {
            Err(KarteNichtAufDerHand)
        }
    }

    fn spieler_aktion_regulaer(&mut self, aktion: SpielerAktion) -> Result<(), SpielerAktionError> {
        use SpielerAktion::*;

        match aktion {
            Solo(_,_) => Err(SoloAnsagenVorbei),
            Karte(spieler, karte) => {
                self.check_an_der_reihe(spieler)?;

                let mut handkarten = &mut self.handkarten[spieler.as_usize()];

                if self.aktueller_stich.len() > 0 {
                    let stich_karte = *self.aktueller_stich.first().unwrap();

                    let regelsatz = &self.regelsatz;

                    if !regelsatz.bedient(stich_karte, karte)
                        && handkarten.iter().find(|k| regelsatz.bedient(stich_karte, **k)).is_some() {

                        return Err(NichtBedient);
                    }
                }

                let idx = Spiel::index_of(handkarten, karte)?;
                handkarten.remove(idx);

                self.aktueller_stich.push(karte);

                self.naechster_spieler();
                if self.aktueller_stich.len() < 4 {
                    Ok(())
                }
                else {
                    // Stich ist komplett --> wer bekommt ihn?

                    // 'naechster_spieler' ist wieder der Spieler, der die erste Karte des Stichs gespielt hat

                    let mut hoechster_idx = 0usize;
                    let mut hoechste_karte = *self.aktueller_stich.first().unwrap();

                    for i in 1..4 {
                        if self.regelsatz.ist_hoeher_als(hoechste_karte, *self.aktueller_stich.get(i).unwrap()) {
                            hoechster_idx = i;
                            hoechste_karte = *self.aktueller_stich.get(i).unwrap();
                        }
                    }

                    // der Gewinnder des Stichs kommt im nächsten Stich heraus
                    self.naechster_spieler = self.naechster_spieler + hoechster_idx;

                    self.stiche[self.naechster_spieler.as_usize()].extend_from_slice(&self.aktueller_stich);
                    self.aktueller_stich.clear();

                    if self.handkarten[0].is_empty() {
                        self.is_spiel_beendet = true;
                    }

                    Ok(())
                }
            },
            _ => {
                Ok(()) //TODO Ansagen
            }
        }
    }

    fn karten_summe(&self, spieler: SpielerNummer) -> u32 {
        let mut result = 0u32;
        for karte in &self.stiche[spieler.as_usize()] {
            result += kartenwert(*karte);
        }
        result
    }
}
