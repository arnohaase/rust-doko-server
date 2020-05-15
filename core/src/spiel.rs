use crate::karte::Karte;
use crate::regelsatz::{SoloArt, Regelsatz, RegelsatzRegistry, RegelVariante, kartenwert};
use std::sync::Arc;
use std::error::Error;
use crate::spiel::SpielerAktionError::{NichtAnDerReihe, SoloAnsagenNochNichtVorbei, SoloAnsagenVorbei, KarteNichtAufDerHand, NichtBedient, SpielerIstNichtContra};
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

#[derive(Copy,Clone)]
pub enum AnsageHoehe {
    Sieg,
    Keine90,
    Keine60,
    Keine30,
    Schwarz,
}
impl AnsageHoehe {
    pub fn limit(&self) -> u32 {
        use AnsageHoehe::*;

        match *self {
            Sieg => 120,
            Keine90=> 90,
            Keine60 => 60,
            Keine30 => 30,
            Schwarz => 1,
        }
    }
}

#[derive(Copy,Clone)]
pub enum SpielerAktion {
    /// 'kein Solo' muss ausdrücklich gemeldet werden - 'None' bedeuetet kein Solo
    Solo(SpielerNummer, Option<SoloArt>),
    //TODO Hochzeit
    Karte(SpielerNummer, Karte),
    AnsageRe(SpielerNummer, AnsageHoehe),
    AnsageContra(SpielerNummer, AnsageHoehe),
}

pub enum SpielerAktionError {
    NichtAnDerReihe,
    SoloAnsagenNochNichtVorbei,
    SoloAnsagenVorbei,
    KarteNichtAufDerHand,
    NichtBedient,
    SpielerIstNichtRe,
    SpielerIstNichtContra,
    AnsageZuSpaet,
    AnsageErhoehtNicht,
}

pub struct Spiel  {
    regelsatz: Box<dyn Regelsatz>,
    regelsatz_registry: Arc<RegelsatzRegistry>,

    solo_ansagen_vorbei: bool,
    /// Solospieler ist bei angesagtem Solo in self.erster_spieler
    angesagtes_solo: Option<SoloArt>,
    solo_spieler: Option<SpielerNummer>,

    ansage_re: Option<AnsageHoehe>,
    ansage_contra: Option<AnsageHoehe>,

    anzahl_gespielte_karten: usize,
    /// Maximalzahl gespielter Karten, bis zu der eine (weitere) Ansage möglich ist
    naechste_ansage_moeglich_bis: usize,
    is_spiel_beendet: bool,

    is_re: [bool;4],

    gefangene_fuechse: [Vec<SpielerNummer>;4],

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
    const FUCHS: Karte = Karte {farbe: Karo, hoehe: As};

    pub fn new(regel_registry: Arc<RegelsatzRegistry>, erster_spieler: u8) -> Spiel {
        assert!(erster_spieler < 4);

        use rand::thread_rng;
        use rand::seq::SliceRandom;
        let mut kartensatz = regel_registry.variante.kartensatz();
        kartensatz.shuffle(&mut thread_rng()); //TODO Seed zum Reproduzieren

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
            solo_spieler: None,

            ansage_re: None,
            ansage_contra: None,
            anzahl_gespielte_karten: 0,
            naechste_ansage_moeglich_bis: 5,

            is_spiel_beendet: false,

            is_re: [
                k1.contains(&Spiel::KREUZ_DAME),
                k2.contains(&Spiel::KREUZ_DAME),
                k3.contains(&Spiel::KREUZ_DAME),
                k4.contains(&Spiel::KREUZ_DAME)],

            gefangene_fuechse: [vec![], vec![], vec![], vec![]],

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

    fn push_aktion(&mut self, aktion: SpielerAktion) -> Result<(), SpielerAktionError> {
        self.journal.push(aktion);
        Ok(())
    }

    fn spieler_aktion_solo_ansagen(&mut self, aktion: SpielerAktion) -> Result<(),SpielerAktionError> {
        match aktion {
            SpielerAktion::Solo(spieler, Some(solo_art)) => {
                self.check_an_der_reihe(spieler)?;

                self.solo_ansagen_vorbei = true;
                self.angesagtes_solo = Some(solo_art);
                self.solo_spieler = Some(spieler);
                self.erster_spieler = spieler;
                self.naechster_spieler = spieler;
                self.regelsatz = self.regelsatz_registry.solo(solo_art);

                self.is_re = [false, false, false, false];
                self.is_re[spieler.as_usize()] = true;

                self.push_aktion(aktion)
            }
            SpielerAktion::Solo(spieler, None) => {
                self.check_an_der_reihe(spieler)?;
                self.naechster_spieler();
                if self.naechster_spieler == self.erster_spieler {
                    self.solo_ansagen_vorbei = true;
                }
                self.push_aktion(aktion)
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
                self.anzahl_gespielte_karten += 1;

                if self.aktueller_stich.len() < 4 {
                    self.push_aktion(aktion)
                }
                else {
                    // Stich ist komplett --> wer bekommt ihn?

                    // 'naechster_spieler' ist jetzt wieder der Spieler, der die erste Karte des Stichs gespielt hat

                    let mut hoechster_idx = 0usize;
                    let mut hoechste_karte = *self.aktueller_stich.first().unwrap();

                    for i in 1..4 {
                        if self.regelsatz.ist_hoeher_als(hoechste_karte, *self.aktueller_stich.get(i).unwrap()) {
                            hoechster_idx = i;
                            hoechste_karte = *self.aktueller_stich.get(i).unwrap();
                        }
                    }

                    // der Gewinnder des Stichs kommt im nächsten Stich heraus
                    let stich_gewinner = self.naechster_spieler + hoechster_idx;

                    self.stiche[stich_gewinner.as_usize()].extend_from_slice(&self.aktueller_stich);

                    if self.regelsatz_registry.variante.fuchs_gefangen {
                        for i in 0..4 {
                            if self.aktueller_stich[i] == Spiel::FUCHS {
                                self.gefangene_fuechse[stich_gewinner.as_usize()].push(self.naechster_spieler + i);
                            }
                        }
                    }

                    self.aktueller_stich.clear();
                    self.naechster_spieler = stich_gewinner;

                    if self.handkarten[0].is_empty() {
                        self.is_spiel_beendet = true;
                    }

                    self.push_aktion(aktion)
                }
            },
            AnsageRe(spieler, hoehe) =>  {
                self.check_is_re(spieler)?;
                self.check_ansage_moeglich()?;

                if let Some(alte_hoehe) = self.ansage_re {
                    if alte_hoehe.limit() <= hoehe.limit() {
                        return Err(SpielerAktionError::AnsageErhoehtNicht);
                    }
                }
                self.ansage_re = Some(hoehe);
                self.naechste_ansage_moeglich_bis = self.anzahl_gespielte_karten + 4;

                self.push_aktion(aktion)

            },
            AnsageContra(spieler, hoehe) =>  {
                self.check_is_contra(spieler)?;
                self.check_ansage_moeglich()?;

                if let Some(alte_hoehe) = self.ansage_contra {
                    if alte_hoehe.limit() <= hoehe.limit() {
                        return Err(SpielerAktionError::AnsageErhoehtNicht);
                    }
                }
                self.ansage_contra = Some(hoehe);
                self.naechste_ansage_moeglich_bis = self.anzahl_gespielte_karten + 4;

                self.push_aktion(aktion)
            },
        }
    }

    fn check_is_re(&self, spieler: SpielerNummer) -> Result<(), SpielerAktionError> {
        if  self.is_re[spieler.as_usize()] {
            Ok(())
        }
        else {
            Err(SpielerAktionError::SpielerIstNichtRe)
        }
    }

    fn check_is_contra(&self, spieler: SpielerNummer) -> Result<(), SpielerAktionError> {
        if  self.is_re[spieler.as_usize()] {
            Ok(())
        }
        else {
            Err(SpielerAktionError::SpielerIstNichtContra)
        }
    }

    fn check_ansage_moeglich(&self) -> Result<(), SpielerAktionError> {
        if self.anzahl_gespielte_karten <= self.naechste_ansage_moeglich_bis {
            Ok(())
        }
        else {
            Err(SpielerAktionError::AnsageZuSpaet)
        }
    }

    fn karten_summe_re(&self) -> u32 {
        let mut result = 0u32;

        for i in 0..4 {
            if self.is_re[i] {
                result += self.karten_summe(SpielerNummer { idx: i as u8});
            }
        }

        result
    }

    /// "sollte" am Spielende 240-karten_summe_re sein. Separate Implementierung als Debugging-Hilfe
    ///  und für Zwischenstände
    fn karten_summe_contra(&self) -> u32 {
        let mut result = 0u32;

        for i in 0..4 {
            if !self.is_re[i] {
                result += self.karten_summe(SpielerNummer { idx: i as u8});
            }
        }

        result
    }

    fn karten_summe(&self, spieler: SpielerNummer) -> u32 {
        let mut result = 0u32;
        for karte in &self.stiche[spieler.as_usize()] {
            result += kartenwert(*karte);
        }
        result
    }


    //TODO reichere Abstraktion 'Stich' mit gespielter Karte je Spieler, 'wer hat aufgespielt', 'wer hat den Stich bekommen'?
    fn fuchs_gefangen_punkte(&self, hat_re_gewonnen: bool) -> i32 {
        let mut result = 0;
        
        if self.regelsatz_registry.variante.fuchs_gefangen {
            for i in 0..4 {
                let spieler = SpielerNummer {idx: i as u8};

                for fuchs_quelle in self.gefangene_fuechse[i].iter() {
                    if self.is_re[i] != self.is_re[fuchs_quelle.as_usize()] {
                        if hat_re_gewonnen == self.is_re[i] {
                            result += 1;
                        }
                        else {
                            result -= 1;
                        }
                    }
                }
            }
        }
        result
    }

    /// None wenn das Spiel noch nicht beendet ist
    pub fn ergebnis(&self) -> Option<SpielErgebnis> {
        if !self.is_spiel_beendet {
            return None;
        }

        let kartensumme_re = self.karten_summe_re();
        let kartensumme_contra = self.karten_summe_contra();

        let hat_re_gewonnen = hat_re_gewonnen(kartensumme_re, kartensumme_contra, self.ansage_re, self.ansage_contra);

        let mut punkte = (
            Spiel::punkte_durch_ansagen(self.ansage_re)
          + Spiel::punkte_durch_ansagen(self.ansage_contra)
        ) as i32;
        
        if !hat_re_gewonnen {
            punkte += 1;
        }

        punkte += self.fuchs_gefangen_punkte(hat_re_gewonnen);
        //TODO Sonderpunkte

        let mut punkte_re = 0i32;
        let mut punkte_contra = 0i32;

        //TODO Punkte für kartensumme - wie rechnet man das bei Ansagen der Gegner?

        if self.solo_spieler.is_none() {
            // normales Spiel

            if self.regelsatz_registry.variante.nomale_spiele_as_nullsumme {
                punkte_re = punkte as i32;
                if ! hat_re_gewonnen {
                    punkte_re = - punkte_re
                }
                punkte_contra = -punkte_re;
            }
            else {
                if hat_re_gewonnen {
                    punkte_re = punkte as i32;
                }
                else {
                    punkte_contra = punkte as i32;
                }
            }
        }
        else {
            // Solo
            punkte_contra = punkte as i32;

            if hat_re_gewonnen {
                punkte_contra = -(punkte as i32);
            }
            else {
                punkte_contra = punkte as i32;
            }

            punkte_re = 3*punkte_contra;
        }

        let mut re_spieler = vec![];
        let mut contra_spieler = vec![];

        for i in 0..4 {
            let s = SpielerNummer {idx: i as u8 };

            if self.is_re[i] {
                re_spieler.push(s);
            }
            else {
                contra_spieler.push(s);
            }
        }

        Some(SpielErgebnis {
            hat_re_gewonnen,
            re_spieler,
            contra_spieler,
            solo_spieler: self.solo_spieler,
            kartensumme_re,
            kartensumme_contra,
            punkte_re,
            punkte_contra,
        })

    }

    fn punkte_durch_ansagen(ansage: Option<AnsageHoehe>) -> u32 {
        use AnsageHoehe::*;

        match ansage {
            None => 0,
            Some(Sieg) => 1,
            Some(Keine90) => 2,
            Some(Keine60) => 3,
            Some(Keine30) => 4,
            Some(Schwarz) => 5,
        }
    }
}

fn hat_re_gewonnen(kartensumme_re: u32, kartensumme_contra: u32, ansage_re: Option<AnsageHoehe>, ansage_contra: Option<AnsageHoehe>) -> bool {
    assert!(kartensumme_re + kartensumme_contra == 240);

    match (ansage_re, ansage_contra) {
        (None, None) => kartensumme_re > 120,
        (Some(AnsageHoehe::Sieg), None) => kartensumme_re > 120,
        (None, Some(AnsageHoehe::Sieg)) => kartensumme_re > 120,
        (Some(AnsageHoehe::Sieg), Some(AnsageHoehe::Sieg)) => kartensumme_re > 120,
        (Some(hoehe), None) => {
            kartensumme_contra < hoehe.limit()
        },
        (Some(hoehe), Some(AnsageHoehe::Sieg)) => {
            kartensumme_contra < hoehe.limit()
        },
        (None, Some(hoehe)) => {
            kartensumme_re >= hoehe.limit()
        },
        (Some(AnsageHoehe::Sieg), Some(hoehe)) => {
            kartensumme_re >= hoehe.limit()
        },
        (Some(hoehe_re), Some(hoehe_contra)) => {
            true  //TODO wie sind hier die Regeln?
        }
    }
}

pub struct SpielErgebnis {
    /// false wenn Contra gewonnen hat
    pub hat_re_gewonnen: bool,

    pub re_spieler: Vec<SpielerNummer>,
    pub contra_spieler: Vec<SpielerNummer>,
    pub solo_spieler: Option<SpielerNummer>,

    pub kartensumme_re: u32,
    pub kartensumme_contra: u32,

    pub punkte_re: i32,
    pub punkte_contra: i32,
}

#[cfg(test)]
mod test {
    use crate::spiel::{hat_re_gewonnen, AnsageHoehe};

    #[test]
    pub fn test_hat_re_gewonnen_ohne_ansage() {
        assert_eq!(true, hat_re_gewonnen(240, 0, None, None));
        assert_eq!(true, hat_re_gewonnen(155, 85, None, None));
        assert_eq!(true, hat_re_gewonnen(121, 119, None, None));
        assert_eq!(false, hat_re_gewonnen(120, 120, None, None));
        assert_eq!(false, hat_re_gewonnen(119, 121, None, None));
        assert_eq!(false, hat_re_gewonnen(41, 199, None, None));
        assert_eq!(false, hat_re_gewonnen(0, 240, None, None));
    }

    #[test]
    pub fn test_hat_re_gewonnen_ansage_re() {
        _test_hat_re_gewonnen_ansage_re(None);
    }
    #[test]
    pub fn test_hat_re_gewonnen_ansage_re_mit_contra_sieg() {
        _test_hat_re_gewonnen_ansage_re(Some(AnsageHoehe::Sieg));
    }

    fn _test_hat_re_gewonnen_ansage_re(ansage_contra: Option<AnsageHoehe>) {
        use AnsageHoehe::*;

        assert_eq!(true, hat_re_gewonnen(240, 0, Some(Sieg), ansage_contra));
        assert_eq!(true, hat_re_gewonnen(155, 85, Some(Sieg), ansage_contra));
        assert_eq!(true, hat_re_gewonnen(121, 119, Some(Sieg), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(120, 120, Some(Sieg), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(119, 121, Some(Sieg), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(41, 199, Some(Sieg), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(0, 240, Some(Sieg), ansage_contra));

        assert_eq!(true, hat_re_gewonnen(240, 0, Some(Keine90), ansage_contra));
        assert_eq!(true, hat_re_gewonnen(151, 89, Some(Keine90), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(150, 90, Some(Keine90), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(121, 119, Some(Keine90), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(120, 120, Some(Keine90), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(119, 121, Some(Keine90), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(41, 199, Some(Keine90), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(0, 240, Some(Keine90), ansage_contra));

        assert_eq!(true, hat_re_gewonnen(240, 0, Some(Keine60), ansage_contra));
        assert_eq!(true, hat_re_gewonnen(181, 59, Some(Keine60), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(180, 60, Some(Keine60), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(155, 85, Some(Keine60), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(121, 119, Some(Keine60), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(120, 120, Some(Keine60), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(119, 121, Some(Keine60), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(41, 199, Some(Keine60), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(0, 240, Some(Keine60), ansage_contra));

        assert_eq!(true, hat_re_gewonnen(240, 0, Some(Keine30), ansage_contra));
        assert_eq!(true, hat_re_gewonnen(211, 29, Some(Keine30), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(210, 30, Some(Keine30), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(185, 55, Some(Keine30), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(155, 85, Some(Keine30), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(121, 119, Some(Keine30), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(120, 120, Some(Keine30), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(119, 121, Some(Keine30), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(41, 199, Some(Keine30), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(0, 240, Some(Keine30), ansage_contra));

        assert_eq!(true, hat_re_gewonnen(240, 0, Some(Schwarz), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(239, 1, Some(Schwarz), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(215, 25, Some(Schwarz), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(185, 55, Some(Schwarz), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(155, 85, Some(Schwarz), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(121, 119, Some(Schwarz), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(120, 120, Some(Schwarz), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(119, 121, Some(Schwarz), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(41, 199, Some(Schwarz), ansage_contra));
        assert_eq!(false, hat_re_gewonnen(0, 240, Some(Schwarz), ansage_contra));
    }

    #[test]
    pub fn test_hat_re_gewonnen_ansage_contra() {
        _test_hat_re_gewonnen_ansage_contra(None);
    }
    #[test]
    pub fn test_hat_re_gewonnen_ansage_contra_mit_re_sieg() {
        _test_hat_re_gewonnen_ansage_contra(Some(AnsageHoehe::Sieg));
    }

    fn _test_hat_re_gewonnen_ansage_contra(ansage_re: Option<AnsageHoehe>) {
        use AnsageHoehe::*;

        assert_eq!(true, hat_re_gewonnen(240, 0, ansage_re, Some(Sieg)));
        assert_eq!(true, hat_re_gewonnen(155, 85, ansage_re, Some(Sieg)));
        assert_eq!(true, hat_re_gewonnen(121, 119, ansage_re, Some(Sieg)));
        assert_eq!(false, hat_re_gewonnen(120, 120, ansage_re, Some(Sieg)));
        assert_eq!(false, hat_re_gewonnen(119, 121, ansage_re, Some(Sieg)));
        assert_eq!(false, hat_re_gewonnen(41, 199, ansage_re, Some(Sieg)));
        assert_eq!(false, hat_re_gewonnen(0, 240, ansage_re, Some(Sieg)));

        assert_eq!(true, hat_re_gewonnen(240, 0, ansage_re, Some(Keine90)));
        assert_eq!(true, hat_re_gewonnen(155, 85, ansage_re, Some(Keine90)));
        assert_eq!(true, hat_re_gewonnen(121, 119, ansage_re, Some(Keine90)));
        assert_eq!(true, hat_re_gewonnen(120, 120, ansage_re, Some(Keine90)));
        assert_eq!(true, hat_re_gewonnen(119, 121, ansage_re, Some(Keine90)));
        assert_eq!(true, hat_re_gewonnen(90, 150, ansage_re, Some(Keine90)));
        assert_eq!(false, hat_re_gewonnen(89, 151, ansage_re, Some(Keine90)));
        assert_eq!(false, hat_re_gewonnen(41, 199, ansage_re, Some(Keine90)));
        assert_eq!(false, hat_re_gewonnen(0, 240, ansage_re, Some(Keine90)));

        assert_eq!(true, hat_re_gewonnen(240, 0, ansage_re, Some(Keine60)));
        assert_eq!(true, hat_re_gewonnen(185, 55, ansage_re, Some(Keine60)));
        assert_eq!(true, hat_re_gewonnen(155, 85, ansage_re, Some(Keine60)));
        assert_eq!(true, hat_re_gewonnen(121, 119, ansage_re, Some(Keine60)));
        assert_eq!(true, hat_re_gewonnen(120, 120, ansage_re, Some(Keine60)));
        assert_eq!(true, hat_re_gewonnen(119, 121, ansage_re, Some(Keine60)));
        assert_eq!(true, hat_re_gewonnen(60, 180, ansage_re, Some(Keine60)));
        assert_eq!(false, hat_re_gewonnen(59, 181, ansage_re, Some(Keine60)));
        assert_eq!(false, hat_re_gewonnen(41, 199, ansage_re, Some(Keine60)));
        assert_eq!(false, hat_re_gewonnen(0, 240, ansage_re, Some(Keine60)));

        assert_eq!(true, hat_re_gewonnen(240, 0, ansage_re, Some(Keine30)));
        assert_eq!(true, hat_re_gewonnen(215, 25, ansage_re, Some(Keine30)));
        assert_eq!(true, hat_re_gewonnen(185, 55, ansage_re, Some(Keine30)));
        assert_eq!(true, hat_re_gewonnen(155, 85, ansage_re, Some(Keine30)));
        assert_eq!(true, hat_re_gewonnen(121, 119, ansage_re, Some(Keine30)));
        assert_eq!(true, hat_re_gewonnen(120, 120, ansage_re, Some(Keine30)));
        assert_eq!(true, hat_re_gewonnen(119, 121, ansage_re, Some(Keine30)));
        assert_eq!(true, hat_re_gewonnen(41, 199, ansage_re, Some(Keine30)));
        assert_eq!(true, hat_re_gewonnen(30, 210, ansage_re, Some(Keine30)));
        assert_eq!(false, hat_re_gewonnen(29, 211, ansage_re, Some(Keine30)));
        assert_eq!(false, hat_re_gewonnen(0, 240, ansage_re, Some(Keine30)));

        assert_eq!(true, hat_re_gewonnen(240, 0, ansage_re, Some(Schwarz)));
        assert_eq!(true, hat_re_gewonnen(215, 25, ansage_re, Some(Schwarz)));
        assert_eq!(true, hat_re_gewonnen(185, 55, ansage_re, Some(Schwarz)));
        assert_eq!(true, hat_re_gewonnen(155, 85, ansage_re, Some(Schwarz)));
        assert_eq!(true, hat_re_gewonnen(121, 119, ansage_re, Some(Schwarz)));
        assert_eq!(true, hat_re_gewonnen(120, 120, ansage_re, Some(Schwarz)));
        assert_eq!(true, hat_re_gewonnen(119, 121, ansage_re, Some(Schwarz)));
        assert_eq!(true, hat_re_gewonnen(41, 199, ansage_re, Some(Schwarz)));
        assert_eq!(true, hat_re_gewonnen(1, 239, ansage_re, Some(Schwarz)));
        assert_eq!(false, hat_re_gewonnen(0, 240, ansage_re, Some(Schwarz)));
    }
}
