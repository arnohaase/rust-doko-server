use crate::karte::Karte;
use crate::regelsatz::{SoloArt, Regelsatz, RegelsatzRegistry, RegelVariante, kartenwert};
use std::sync::Arc;
use std::error::Error;
use crate::spiel::SpielerAktionError::{NichtAnDerReihe, SoloAnsagenNochNichtVorbei, SoloAnsagenVorbei, KarteNichtAufDerHand, NichtBedient, SpielerIstNichtContra, SpielIstAbgeschlossen};
use crate::karte::Hoehe::*;
use crate::karte::Farbe::*;
use std::ops::Add;


#[derive(Copy,Clone,Eq,PartialEq)]
pub struct SpielerNummer {
    idx: u8,
}
impl SpielerNummer {
    pub fn new(idx: u8) -> SpielerNummer {
        assert! (idx < 4);
        SpielerNummer {idx}
    }

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
    SpielIstAbgeschlossen,
}

#[derive(Copy,Clone,Debug,Eq,PartialEq)]
pub enum SiegerPartei {
    Re,
    Contra,
    Unentschieden,
}

#[derive(Clone)]
struct Stich {
    aufgespielt_von: SpielerNummer,
    karten: Vec<Karte>,
    gewonnen_von: SpielerNummer,
}
impl Stich {
    pub fn new(aufgespielt_von: SpielerNummer) -> Stich {
        Stich {
            aufgespielt_von,
            karten: vec![],
            gewonnen_von: aufgespielt_von, // beliebiger Wert - hat erst Bedeutung, wenn der Stich fertig ist
        }
    }

    pub fn naechster_spieler(&self) -> SpielerNummer {
        self.aufgespielt_von + self.karten.len()
    }

    pub fn is_komplett(&self) -> bool {
        self.karten.len() == 4
    }

    pub fn punktwert(&self) -> u32 {
        let mut result = 0;
        for karte in &self.karten {
            result += kartenwert(*karte);
        }
        result
    }
}

trait SpielPhase {
    fn spieler_aktion(&mut self, aktion: SpielerAktion) -> Result<Option<Box<dyn SpielPhase>>, SpielerAktionError>;
}

struct VorbehaltPhase {
    regelsatz_registry: Arc<RegelsatzRegistry>,
    handkarten: [Vec<Karte>;4],
    erster_spieler: SpielerNummer,
    an_der_reihe: SpielerNummer,
}
impl VorbehaltPhase {
    fn check_an_der_reihe(&self, spieler: SpielerNummer) -> Result<(), SpielerAktionError> {
        if spieler == self.an_der_reihe {
            Ok(())
        }
        else {
            Err(NichtAnDerReihe)
        }
    }
}
impl SpielPhase for VorbehaltPhase {
    fn spieler_aktion(&mut self, aktion: SpielerAktion) -> Result<Option<Box<dyn SpielPhase>>, SpielerAktionError> {
        match aktion {
            SpielerAktion::Solo(spieler, Some(solo_art)) => {
                self.check_an_der_reihe(spieler)?;

                let regelsatz = self.regelsatz_registry.solo(solo_art);

                let mut is_re = [false, false, false, false];
                is_re[spieler.as_usize()] = true;

                let neue_phase = LaufendesSpiel::new(regelsatz,
                                                     self.regelsatz_registry.variante,
                                                     &self.handkarten, Some(solo_art), Some(spieler), is_re, spieler);
                Ok(Some(Box::new(neue_phase)))
            }
            SpielerAktion::Solo(spieler, None) => {
                self.check_an_der_reihe(spieler)?;

                self.an_der_reihe = self.an_der_reihe.naechster();

                if self.an_der_reihe == self.erster_spieler {
                    let is_re = [
                        self.handkarten[0].contains(&Spiel::KREUZ_DAME),
                        self.handkarten[1].contains(&Spiel::KREUZ_DAME),
                        self.handkarten[2].contains(&Spiel::KREUZ_DAME),
                        self.handkarten[3].contains(&Spiel::KREUZ_DAME)];


                    let neue_phase = LaufendesSpiel::new(self.regelsatz_registry.normal(),
                                                         self.regelsatz_registry.variante,
                                                         &self.handkarten,
                                                         None,
                                                         None,
                                                         is_re,
                                                         self.erster_spieler);
                    Ok(Some(Box::new(neue_phase)))
                }
                else {
                    Ok(None)
                }
            },
            _ => {
                Err(SoloAnsagenNochNichtVorbei)
            }
        }
    }
}


struct LaufendesSpiel {
    regelsatz: Box<dyn Regelsatz>,
    regelvariante: RegelVariante,

    angesagtes_solo: Option<SoloArt>,
    solo_spieler: Option<SpielerNummer>,

    ansage_re: Option<AnsageHoehe>,
    ansage_contra: Option<AnsageHoehe>,

    anzahl_gespielte_karten: usize,
    /// Maximalzahl gespielter Karten, bis zu der eine (weitere) Ansage möglich ist
    naechste_ansage_moeglich_bis: usize,

    is_re: [bool;4],

    handkarten: [Vec<Karte>;4],
    stiche: Vec<Stich>,

    aktueller_stich: Stich,
}
impl LaufendesSpiel {
    fn new(regelsatz: Box<dyn Regelsatz>,
           regelvariante: RegelVariante,
           handkarten: &[Vec<Karte>;4],
           angesagtes_solo: Option<SoloArt>, solo_spieler: Option<SpielerNummer>, //TODO zusammenfassen in 'solo'-Datenstruktur
           is_re: [bool;4],
           erstes_aufspiel: SpielerNummer
    ) -> LaufendesSpiel {
        LaufendesSpiel {
            regelsatz,
            regelvariante,
            angesagtes_solo,
            solo_spieler,
            ansage_re: None,
            ansage_contra: None,
            anzahl_gespielte_karten: 0,
            naechste_ansage_moeglich_bis: 5,
            is_re,
            handkarten: handkarten.clone(),
            stiche: vec![],
            aktueller_stich: Stich::new(erstes_aufspiel),
        }
    }

    fn check_an_der_reihe(&self, spieler: SpielerNummer) -> Result<(), SpielerAktionError> {
        if spieler == self.aktueller_stich.naechster_spieler() {
            Ok(())
        }
        else {
            Err(NichtAnDerReihe)
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

    fn schliesse_stich_ab(&mut self) {
        let mut hoechster_idx = 0usize;
        let mut hoechste_karte = *self.aktueller_stich.karten.first().unwrap();

        for i in 1..4 {
            if self.regelsatz.ist_hoeher_als(hoechste_karte, *self.aktueller_stich.karten.get(i).unwrap()) {
                hoechster_idx = i;
                hoechste_karte = *self.aktueller_stich.karten.get(i).unwrap();
            }
        }

        self.aktueller_stich.gewonnen_von = self.aktueller_stich.aufgespielt_von + hoechster_idx;
        self.stiche.push(self.aktueller_stich.clone());

        self.aktueller_stich = Stich::new(self.aktueller_stich.gewonnen_von);
    }
}
impl SpielPhase for LaufendesSpiel {
    fn spieler_aktion(&mut self, aktion: SpielerAktion) -> Result<Option<Box<dyn SpielPhase>>, SpielerAktionError> {
        use SpielerAktion::*;

        match aktion {
            Solo(_,_) => return Err(SoloAnsagenVorbei),
            Karte(spieler, karte) => {
                self.check_an_der_reihe(spieler)?;

                let mut handkarten = &mut self.handkarten[spieler.as_usize()];

                if self.aktueller_stich.karten.len() > 0 {
                    let stich_karte = *self.aktueller_stich.karten.first().unwrap();

                    let regelsatz = &self.regelsatz;

                    if !regelsatz.bedient(stich_karte, karte)
                        && handkarten.iter().find(|k| regelsatz.bedient(stich_karte, **k)).is_some() {

                        return Err(NichtBedient);
                    }
                }

                let idx = Spiel::index_of(handkarten, karte)?;
                handkarten.remove(idx);

                self.aktueller_stich.karten.push(karte);
                self.anzahl_gespielte_karten += 1;

                if self.aktueller_stich.is_komplett() {
                    self.schliesse_stich_ab();

                    if self.handkarten[0].is_empty() {
                        let new_phase = Abgeschlossen::new(self);
                        return Ok(Some(Box::new(new_phase)));
                    }
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
            },
        }

        Ok(None)
    }
}

struct SpielScore {
    punkte_re: i32,
    punkte_contra: i32,
    punkte_sieger: i32,
}
impl SpielScore {
    fn new() -> SpielScore {
        SpielScore {
            punkte_re: 0,
            punkte_contra: 0,
            punkte_sieger: 0,
        }
    }

    fn add_sieger(&mut self, punkte: i32) {
        self.punkte_sieger += punkte;
    }
    fn add_re(&mut self, punkte: i32) {
        self.punkte_re += punkte;
    }
    fn add_contra(&mut self, punkte: i32) {
        self.punkte_contra += punkte;
    }

    fn ergebnis(self, sieger: SiegerPartei, is_solo: bool, normale_spiele_als_nullsumme: bool) -> (i32,i32) {
        use SiegerPartei::*;

        match sieger {
            Unentschieden => (0, 0),
            Re => {
                let punkte = self.punkte_sieger + self.punkte_re - self.punkte_contra;
                match (is_solo, normale_spiele_als_nullsumme) {
                    (true, _) => (3*punkte, -punkte),
                    (false, true) => (punkte, -punkte),
                    (false, false) => (punkte, 0),
                }
            },
            Contra => {
                let punkte = self.punkte_sieger - self.punkte_re + self.punkte_contra + 1;
                match (is_solo, normale_spiele_als_nullsumme) {
                    (true, _) => (-3*punkte, punkte),
                    (false, true) => (-punkte, punkte),
                    (false, false) => (0, punkte),
                }
            },
        }
    }
}

pub struct Abgeschlossen {
    pub sieger: SiegerPartei,

    pub re_spieler: Vec<SpielerNummer>,
    pub contra_spieler: Vec<SpielerNummer>,
    pub solo_spieler: Option<SpielerNummer>,

    pub kartensumme_re: u32,
    pub kartensumme_contra: u32,

    pub punkte_re: i32,
    pub punkte_contra: i32,
}
impl Abgeschlossen {
    const FUCHS: Karte = Karte { farbe: Karo, hoehe: As };

    fn new(spiel: &LaufendesSpiel) -> Abgeschlossen {
        let kartensumme_re = Abgeschlossen::karten_summe_re(&spiel.stiche, spiel.is_re);
        let kartensumme_contra = Abgeschlossen::karten_summe_contra(&spiel.stiche, spiel.is_re);

        let sieger = sieger(kartensumme_re, kartensumme_contra, spiel.ansage_re, spiel.ansage_contra);

        let mut score = SpielScore::new();
        score.add_sieger(Self::punkte_durch_ansagen(spiel.ansage_re));
        score.add_sieger(Self::punkte_durch_ansagen(spiel.ansage_contra));

        let (punkte_fuer_karten_re, punkte_fuer_karten_contra) = Self::punkte_fuer_kartensumme(kartensumme_re, kartensumme_contra);
        score.add_re(punkte_fuer_karten_re);
        score.add_contra(punkte_fuer_karten_contra);

        // Sonderpunkte
        if spiel.regelvariante.fuchs_gefangen {
            Self::score_fuchs_gefangen(&spiel.stiche, spiel.is_re, &mut score);
        }
        //TODO Weitere Sonderpunkte

        let (punkte_re, punkte_contra) = score.ergebnis(
            sieger,
            spiel.solo_spieler.is_some(),
            spiel.regelvariante.nomale_spiele_as_nullsumme);

        let mut re_spieler = vec![];
        let mut contra_spieler = vec![];

        for i in 0..4 {
            let s = SpielerNummer {idx: i as u8 };

            if spiel.is_re[i] {
                re_spieler.push(s);
            }
            else {
                contra_spieler.push(s);
            }
        }

        Abgeschlossen {
            sieger,
            re_spieler,
            contra_spieler,
            solo_spieler: spiel.solo_spieler,
            kartensumme_re,
            kartensumme_contra,
            punkte_re,
            punkte_contra,
        }
    }

    fn punkte_fuer_kartensumme(kartensumme_re: u32, kartensumme_contra: u32) -> (i32,i32) {
        let mut punkte_re = 0;
        let mut punkte_contra = 0;

        if kartensumme_re < 90 {punkte_contra += 1;}
        if kartensumme_re < 60 {punkte_contra += 1;}
        if kartensumme_re < 30 {punkte_contra += 1;}
        if kartensumme_re == 0 {punkte_contra += 1;}

        if kartensumme_contra < 90 {punkte_re += 1;}
        if kartensumme_contra < 60 {punkte_re += 1;}
        if kartensumme_contra < 30 {punkte_re += 1;}
        if kartensumme_contra == 0 {punkte_re += 1;}

        (punkte_re, punkte_contra)
    }

    fn score_fuchs_gefangen(stiche: &Vec<Stich>, is_re: [bool;4], score: &mut SpielScore) {
        for stich in stiche.iter() {
            let is_stich_re = is_re[stich.gewonnen_von.as_usize()];

            for i in 0..4 {
                if stich.karten[i] == Self::FUCHS {
                    let is_fuchs_re = is_re[(stich.aufgespielt_von + i).as_usize()];

                    match (is_stich_re, is_fuchs_re) {
                        (true, false) => score.add_re(1),
                        (false, true) => score.add_contra(1),
                        _ => {}
                    }
                }
            }
        }
    }

    fn karten_summe_re(stiche: &Vec<Stich>, is_re: [bool;4]) -> u32 {
        let mut result = 0u32;

        for i in 0..4 {
            if is_re[i] {
                result += Abgeschlossen::karten_summe(stiche, SpielerNummer { idx: i as u8});
            }
        }

        result
    }

    /// "sollte" am Spielende 240-karten_summe_re sein. Separate Implementierung als Debugging-Hilfe
    ///  und für Zwischenstände
    fn karten_summe_contra(stiche: &Vec<Stich>, is_re: [bool;4]) -> u32 {
        let mut result = 0u32;

        for i in 0..4 {
            if !is_re[i] {
                result += Abgeschlossen::karten_summe(stiche, SpielerNummer { idx: i as u8});
            }
        }

        result
    }

    fn karten_summe(stiche: &Vec<Stich>, spieler: SpielerNummer) -> u32 {
        let mut result = 0u32;
        for stich in stiche.iter() {
            if stich.gewonnen_von == spieler {
                result += stich.punktwert();
            }
        }
        result
    }

    fn punkte_durch_ansagen(ansage: Option<AnsageHoehe>) -> i32 {
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

impl SpielPhase for Abgeschlossen {
    fn spieler_aktion(&mut self, aktion: SpielerAktion) -> Result<Option<Box<dyn SpielPhase>>, SpielerAktionError> {
        Err(SpielerAktionError::SpielIstAbgeschlossen)
    }
}


pub struct Spiel {
    // regelsatz_registry: Arc<RegelsatzRegistry>,
    erster_spieler: SpielerNummer,
    phase: Box<dyn SpielPhase>,

    journal: Vec<SpielerAktion>,
}
impl Spiel {
    const KREUZ_DAME: Karte = Karte { farbe: Kreuz, hoehe: Dame };

    pub fn new(regelsatz_registry: Arc<RegelsatzRegistry>, erster_spieler: SpielerNummer) -> Spiel {
        use rand::thread_rng;
        use rand::seq::SliceRandom;
        let mut kartensatz = regelsatz_registry.variante.kartensatz();
        kartensatz.shuffle(&mut thread_rng()); //TODO Seed zum Reproduzieren

        let mut chunks = kartensatz.chunks(kartensatz.len() / 4);
        let k1 = chunks.next().unwrap().to_vec();
        let k2 = chunks.next().unwrap().to_vec();
        let k3 = chunks.next().unwrap().to_vec();
        let k4 = chunks.next().unwrap().to_vec();

        let phase = VorbehaltPhase {
            regelsatz_registry,
            handkarten: [k1, k2, k3, k4],
            erster_spieler,
            an_der_reihe: erster_spieler,
        };

        Spiel {
            erster_spieler,
            phase: Box::new(phase),
            journal: vec![],
        }
    }

    pub fn spieler_aktion(&mut self, aktion: SpielerAktion) -> Result<(), SpielerAktionError> {
        let a = self.phase.spieler_aktion(aktion)?;
        self.journal.push(aktion);

        match self.phase.spieler_aktion(aktion)? {
            None => {
            },
            Some(new_phase) => {
                self.phase = new_phase;
            }
        }

        Ok(())
    }

    fn index_of(karten: &Vec<Karte>, karte: Karte) -> Result<usize, SpielerAktionError> {
        if let Some(idx) = karten.iter().position(|&el| el == karte) {
            Ok(idx)
        }
        else {
            Err(KarteNichtAufDerHand)
        }
    }
}

fn sieger(kartensumme_re: u32, kartensumme_contra: u32, ansage_re: Option<AnsageHoehe>, ansage_contra: Option<AnsageHoehe>) -> SiegerPartei {
    use SiegerPartei::*;

    assert!(kartensumme_re + kartensumme_contra == 240);

    fn by_limit(kartensumme_re: u32, limit: u32) -> SiegerPartei {
        if kartensumme_re > limit {Re} else {Contra}
    }

    let re_limit =  match (ansage_re, ansage_contra) {
        (None, None) => 120,
        (Some(AnsageHoehe::Sieg), None) => 120,
        (None, Some(AnsageHoehe::Sieg)) => 120,
        (Some(AnsageHoehe::Sieg), Some(AnsageHoehe::Sieg)) => 120,

        (Some(hoehe), None) => 240 - hoehe.limit(),
        (Some(hoehe), Some(AnsageHoehe::Sieg)) => 240 - hoehe.limit(),
        (None, Some(hoehe)) => hoehe.limit() - 1,
        (Some(AnsageHoehe::Sieg), Some(hoehe)) => hoehe.limit() - 1,
        (Some(hoehe_re), Some(hoehe_contra)) => {
            if kartensumme_re < hoehe_contra.limit() {
                return Contra;
            }
            if kartensumme_contra < hoehe_re.limit() {
                return Re;
            }

            return Unentschieden;
        }
    };

    if kartensumme_re > re_limit {Re} else {Contra}
}


#[cfg(test)]
mod test {
    use crate::spiel::{sieger, AnsageHoehe, SiegerPartei};
    use AnsageHoehe::*;
    use SiegerPartei::*;

    #[test]
    pub fn test_sieger_ohne_ansage() {
        assert_eq!(Re, sieger(240, 0, None, None));
        assert_eq!(Re, sieger(155, 85, None, None));
        assert_eq!(Re, sieger(121, 119, None, None));
        assert_eq!(Contra, sieger(120, 120, None, None));
        assert_eq!(Contra, sieger(119, 121, None, None));
        assert_eq!(Contra, sieger(41, 199, None, None));
        assert_eq!(Contra, sieger(0, 240, None, None));
    }

    #[test]
    pub fn test_sieger_ansage_re() {
        _test_sieger_ansage_re(None);
    }
    #[test]
    pub fn test_sieger_ansage_re_mit_contra_sieg() {
        _test_sieger_ansage_re(Some(AnsageHoehe::Sieg));
    }

    fn _test_sieger_ansage_re(ansage_contra: Option<AnsageHoehe>) {
        assert_eq!(Re, sieger(240, 0, Some(Sieg), ansage_contra));
        assert_eq!(Re, sieger(155, 85, Some(Sieg), ansage_contra));
        assert_eq!(Re, sieger(121, 119, Some(Sieg), ansage_contra));
        assert_eq!(Contra, sieger(120, 120, Some(Sieg), ansage_contra));
        assert_eq!(Contra, sieger(119, 121, Some(Sieg), ansage_contra));
        assert_eq!(Contra, sieger(41, 199, Some(Sieg), ansage_contra));
        assert_eq!(Contra, sieger(0, 240, Some(Sieg), ansage_contra));

        assert_eq!(Re, sieger(240, 0, Some(Keine90), ansage_contra));
        assert_eq!(Re, sieger(151, 89, Some(Keine90), ansage_contra));
        assert_eq!(Contra, sieger(150, 90, Some(Keine90), ansage_contra));
        assert_eq!(Contra, sieger(121, 119, Some(Keine90), ansage_contra));
        assert_eq!(Contra, sieger(120, 120, Some(Keine90), ansage_contra));
        assert_eq!(Contra, sieger(119, 121, Some(Keine90), ansage_contra));
        assert_eq!(Contra, sieger(41, 199, Some(Keine90), ansage_contra));
        assert_eq!(Contra, sieger(0, 240, Some(Keine90), ansage_contra));

        assert_eq!(Re, sieger(240, 0, Some(Keine60), ansage_contra));
        assert_eq!(Re, sieger(181, 59, Some(Keine60), ansage_contra));
        assert_eq!(Contra, sieger(180, 60, Some(Keine60), ansage_contra));
        assert_eq!(Contra, sieger(155, 85, Some(Keine60), ansage_contra));
        assert_eq!(Contra, sieger(121, 119, Some(Keine60), ansage_contra));
        assert_eq!(Contra, sieger(120, 120, Some(Keine60), ansage_contra));
        assert_eq!(Contra, sieger(119, 121, Some(Keine60), ansage_contra));
        assert_eq!(Contra, sieger(41, 199, Some(Keine60), ansage_contra));
        assert_eq!(Contra, sieger(0, 240, Some(Keine60), ansage_contra));

        assert_eq!(Re, sieger(240, 0, Some(Keine30), ansage_contra));
        assert_eq!(Re, sieger(211, 29, Some(Keine30), ansage_contra));
        assert_eq!(Contra, sieger(210, 30, Some(Keine30), ansage_contra));
        assert_eq!(Contra, sieger(185, 55, Some(Keine30), ansage_contra));
        assert_eq!(Contra, sieger(155, 85, Some(Keine30), ansage_contra));
        assert_eq!(Contra, sieger(121, 119, Some(Keine30), ansage_contra));
        assert_eq!(Contra, sieger(120, 120, Some(Keine30), ansage_contra));
        assert_eq!(Contra, sieger(119, 121, Some(Keine30), ansage_contra));
        assert_eq!(Contra, sieger(41, 199, Some(Keine30), ansage_contra));
        assert_eq!(Contra, sieger(0, 240, Some(Keine30), ansage_contra));

        assert_eq!(Re, sieger(240, 0, Some(Schwarz), ansage_contra));
        assert_eq!(Contra, sieger(239, 1, Some(Schwarz), ansage_contra));
        assert_eq!(Contra, sieger(215, 25, Some(Schwarz), ansage_contra));
        assert_eq!(Contra, sieger(185, 55, Some(Schwarz), ansage_contra));
        assert_eq!(Contra, sieger(155, 85, Some(Schwarz), ansage_contra));
        assert_eq!(Contra, sieger(121, 119, Some(Schwarz), ansage_contra));
        assert_eq!(Contra, sieger(120, 120, Some(Schwarz), ansage_contra));
        assert_eq!(Contra, sieger(119, 121, Some(Schwarz), ansage_contra));
        assert_eq!(Contra, sieger(41, 199, Some(Schwarz), ansage_contra));
        assert_eq!(Contra, sieger(0, 240, Some(Schwarz), ansage_contra));
    }

    #[test]
    pub fn test_sieger_ansage_contra() {
        _test_sieger_ansage_contra(None);
    }
    #[test]
    pub fn test_sieger_ansage_contra_mit_re_sieg() {
        _test_sieger_ansage_contra(Some(AnsageHoehe::Sieg));
    }

    fn _test_sieger_ansage_contra(ansage_re: Option<AnsageHoehe>) {
        assert_eq!(Re, sieger(240, 0, ansage_re, Some(Sieg)));
        assert_eq!(Re, sieger(155, 85, ansage_re, Some(Sieg)));
        assert_eq!(Re, sieger(121, 119, ansage_re, Some(Sieg)));
        assert_eq!(Contra, sieger(120, 120, ansage_re, Some(Sieg)));
        assert_eq!(Contra, sieger(119, 121, ansage_re, Some(Sieg)));
        assert_eq!(Contra, sieger(41, 199, ansage_re, Some(Sieg)));
        assert_eq!(Contra, sieger(0, 240, ansage_re, Some(Sieg)));

        assert_eq!(Re, sieger(240, 0, ansage_re, Some(Keine90)));
        assert_eq!(Re, sieger(155, 85, ansage_re, Some(Keine90)));
        assert_eq!(Re, sieger(121, 119, ansage_re, Some(Keine90)));
        assert_eq!(Re, sieger(120, 120, ansage_re, Some(Keine90)));
        assert_eq!(Re, sieger(119, 121, ansage_re, Some(Keine90)));
        assert_eq!(Re, sieger(90, 150, ansage_re, Some(Keine90)));
        assert_eq!(Contra, sieger(89, 151, ansage_re, Some(Keine90)));
        assert_eq!(Contra, sieger(41, 199, ansage_re, Some(Keine90)));
        assert_eq!(Contra, sieger(0, 240, ansage_re, Some(Keine90)));

        assert_eq!(Re, sieger(240, 0, ansage_re, Some(Keine60)));
        assert_eq!(Re, sieger(185, 55, ansage_re, Some(Keine60)));
        assert_eq!(Re, sieger(155, 85, ansage_re, Some(Keine60)));
        assert_eq!(Re, sieger(121, 119, ansage_re, Some(Keine60)));
        assert_eq!(Re, sieger(120, 120, ansage_re, Some(Keine60)));
        assert_eq!(Re, sieger(119, 121, ansage_re, Some(Keine60)));
        assert_eq!(Re, sieger(60, 180, ansage_re, Some(Keine60)));
        assert_eq!(Contra, sieger(59, 181, ansage_re, Some(Keine60)));
        assert_eq!(Contra, sieger(41, 199, ansage_re, Some(Keine60)));
        assert_eq!(Contra, sieger(0, 240, ansage_re, Some(Keine60)));

        assert_eq!(Re, sieger(240, 0, ansage_re, Some(Keine30)));
        assert_eq!(Re, sieger(215, 25, ansage_re, Some(Keine30)));
        assert_eq!(Re, sieger(185, 55, ansage_re, Some(Keine30)));
        assert_eq!(Re, sieger(155, 85, ansage_re, Some(Keine30)));
        assert_eq!(Re, sieger(121, 119, ansage_re, Some(Keine30)));
        assert_eq!(Re, sieger(120, 120, ansage_re, Some(Keine30)));
        assert_eq!(Re, sieger(119, 121, ansage_re, Some(Keine30)));
        assert_eq!(Re, sieger(41, 199, ansage_re, Some(Keine30)));
        assert_eq!(Re, sieger(30, 210, ansage_re, Some(Keine30)));
        assert_eq!(Contra, sieger(29, 211, ansage_re, Some(Keine30)));
        assert_eq!(Contra, sieger(0, 240, ansage_re, Some(Keine30)));

        assert_eq!(Re, sieger(240, 0, ansage_re, Some(Schwarz)));
        assert_eq!(Re, sieger(215, 25, ansage_re, Some(Schwarz)));
        assert_eq!(Re, sieger(185, 55, ansage_re, Some(Schwarz)));
        assert_eq!(Re, sieger(155, 85, ansage_re, Some(Schwarz)));
        assert_eq!(Re, sieger(121, 119, ansage_re, Some(Schwarz)));
        assert_eq!(Re, sieger(120, 120, ansage_re, Some(Schwarz)));
        assert_eq!(Re, sieger(119, 121, ansage_re, Some(Schwarz)));
        assert_eq!(Re, sieger(41, 199, ansage_re, Some(Schwarz)));
        assert_eq!(Re, sieger(1, 239, ansage_re, Some(Schwarz)));
        assert_eq!(Contra, sieger(0, 240, ansage_re, Some(Schwarz)));
    }

    #[test]
    fn test_sieger_beide_angesagt() {
        assert_eq!(Re, sieger(151, 89, Some(Keine90), Some(Keine90)));
        assert_eq!(Unentschieden, sieger(150, 90, Some(Keine90), Some(Keine90)));
        assert_eq!(Unentschieden, sieger(120, 120, Some(Keine90), Some(Keine90)));
        assert_eq!(Unentschieden, sieger(90, 150, Some(Keine90), Some(Keine90)));
        assert_eq!(Contra, sieger(89, 151, Some(Keine90), Some(Keine90)));

        assert_eq!(Re, sieger(181, 59, Some(Keine60), Some(Keine30)));
        assert_eq!(Unentschieden, sieger(180, 60, Some(Keine60), Some(Keine30)));
        assert_eq!(Unentschieden, sieger(120, 120, Some(Keine60), Some(Keine30)));
        assert_eq!(Unentschieden, sieger(60, 180, Some(Keine60), Some(Keine30)));
        assert_eq!(Unentschieden, sieger(59, 181, Some(Keine60), Some(Keine30)));
        assert_eq!(Unentschieden, sieger(30, 210, Some(Keine60), Some(Keine30)));
        assert_eq!(Contra, sieger(29, 211, Some(Keine60), Some(Keine30)));

        assert_eq!(Re, sieger(240, 0, Some(Schwarz), Some(Schwarz)));
        assert_eq!(Unentschieden, sieger(239, 1, Some(Schwarz), Some(Schwarz)));
        assert_eq!(Unentschieden, sieger(120, 120, Some(Schwarz), Some(Schwarz)));
        assert_eq!(Unentschieden, sieger(1, 239, Some(Schwarz), Some(Schwarz)));
        assert_eq!(Contra, sieger(0, 240, Some(Schwarz), Some(Schwarz)));
    }
}

