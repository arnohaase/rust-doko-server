use crate::karte::*;
use crate::karte::Farbe::*;
use crate::karte::Hoehe::*;
use std::collections::HashMap;
use std::sync::Arc;
use std::rc::Rc;

#[derive(Copy,Clone)]
pub enum SoloArt {
    Trumpf,
    FarbsoloKreuz,
    FarbsoloPik,
    FarbsoloHerz,
    FarbsoloKaro,
}

#[derive(Copy,Clone)]
pub enum HochzeitMitWem {
    ErsterFremder,
    // ErsterFremderTrumpf,
    // ErsterFremderFehl,
}

#[derive(Copy,Clone)]
pub struct RegelVariante {
    pub mit_neunen: bool,
    pub zweite_sticht_erste: bool,
    pub fuchs_gefangen: bool,

    pub nomale_spiele_as_nullsumme: bool,
}
impl RegelVariante {
    pub fn kartensatz(&self) -> Vec<Karte> {
        let farben = vec!(Kreuz, Pik, Herz, Karo);
        let mut hoehen = vec!(Zehn, Bube, Dame, Koenig, As);

        if self.mit_neunen {
            hoehen.push(Neun);
        }

        let mut result = Vec::new();
        for &farbe in farben.iter() {
            for &hoehe in hoehen.iter() {
                result.push(Karte {farbe, hoehe});
                result.push(Karte {farbe, hoehe});
            }
        }
        result
    }
}

pub struct RegelsatzRegistry {
    pub variante: RegelVariante,
}
impl RegelsatzRegistry {
    pub fn normal(&self) -> Rc<dyn StichRegelsatz> {
        Rc::new(NormalesSpiel::new (self.variante.zweite_sticht_erste))
    }

    pub fn solo(&self, solo_art: SoloArt) -> Rc<dyn StichRegelsatz> {
        use SoloArt::*;
        //TODO gegen 'RegelVariante' prüfen, ob dieses Solo hier vorgesehen ist


        match solo_art {
            Trumpf => Rc::new(NormalesSpiel::new(self.variante.zweite_sticht_erste)),
            FarbsoloKreuz => Rc::new(Farbsolo{farbe: Kreuz}),
            FarbsoloPik => Rc::new(Farbsolo{farbe: Pik}),
            FarbsoloHerz => Rc::new(Farbsolo{farbe: Herz}),
            FarbsoloKaro => Rc::new(Farbsolo{farbe: Karo}),
        }
    }
}

pub trait StichRegelsatz {
    fn is_trumpf(&self, karte: Karte) -> bool;
    fn bedient(&self, karte1: Karte, karte2: Karte) -> bool;
    fn ist_hoeher_als(&self, karte1: Karte, karte2: Karte) -> bool;
}

pub struct NormalesSpiel {
    pub zweite_sticht_erste: bool,
    trumpf_ranking: HashMap<Karte, u32>,
}
impl NormalesSpiel {
    const HERZ_ZEHN: Karte = Karte { farbe: Herz, hoehe: Zehn};

    pub fn new(zweite_sticht_erste: bool) -> NormalesSpiel {
        let mut trumpf_ranking = HashMap::new();
        trumpf_ranking.insert(Karte {farbe: Karo, hoehe: Neun}, 0);
        trumpf_ranking.insert(Karte {farbe: Karo, hoehe: Koenig}, 1);
        trumpf_ranking.insert(Karte {farbe: Karo, hoehe: Zehn}, 2);
        trumpf_ranking.insert(Karte {farbe: Karo, hoehe: As}, 3);
        trumpf_ranking.insert(Karte {farbe: Karo, hoehe: Bube}, 4);
        trumpf_ranking.insert(Karte {farbe: Herz, hoehe: Bube}, 5);
        trumpf_ranking.insert(Karte {farbe: Pik, hoehe: Bube}, 6);
        trumpf_ranking.insert(Karte {farbe: Kreuz, hoehe: Bube}, 7);
        trumpf_ranking.insert(Karte {farbe: Karo, hoehe: Dame}, 8);
        trumpf_ranking.insert(Karte {farbe: Herz, hoehe: Dame}, 9);
        trumpf_ranking.insert(Karte {farbe: Pik, hoehe: Dame}, 10);
        trumpf_ranking.insert(Karte {farbe: Kreuz, hoehe: Dame}, 11);
        trumpf_ranking.insert(Karte {farbe: Herz, hoehe: As}, 12);

        NormalesSpiel {zweite_sticht_erste, trumpf_ranking}
    }
}
impl StichRegelsatz for NormalesSpiel {
    fn is_trumpf(&self, karte: Karte) -> bool {
        karte.farbe == Karo ||
            karte.hoehe == Bube ||
            karte.hoehe == Dame ||
            karte == NormalesSpiel::HERZ_ZEHN
    }

    fn bedient(&self, karte1: Karte, karte2: Karte) -> bool {
        self.is_trumpf(karte1) == self.is_trumpf(karte2) ||
            karte1.farbe == karte2.farbe
    }

    fn ist_hoeher_als(&self, karte1: Karte, karte2: Karte) -> bool {
        if karte1 == karte2 {
            if karte1 == NormalesSpiel::HERZ_ZEHN {
                return self.zweite_sticht_erste;
            }
            else {
                return false;
            }
        }

        if self.is_trumpf(karte1) != self.is_trumpf(karte2) {
            return self.is_trumpf(karte2);
        }

        if !self.bedient(karte1, karte2) {
            return false;
        }

        if self.is_trumpf(karte1) {
            self.trumpf_ranking.get(&karte2) > self.trumpf_ranking.get(&karte1)
        }
        else {
            kartenwert(karte2) > kartenwert(karte1)
        }
    }
}

pub struct Farbsolo {
    pub farbe: Farbe
}
impl StichRegelsatz for Farbsolo {
    fn is_trumpf(&self, karte: Karte) -> bool {
        self.farbe == karte.farbe
    }

    fn bedient(&self, karte1: Karte, karte2: Karte) -> bool {
        karte1.farbe == karte2.farbe
    }

    fn ist_hoeher_als(&self, karte1: Karte, karte2: Karte) -> bool {
        kartenwert(karte2) > kartenwert(karte1)
    }
}


pub fn kartenwert(karte: Karte) -> u32 {
    match karte.hoehe {
        Neun => 0,
        Bube => 2,
        Dame => 3,
        Koenig => 4,
        Zehn => 10,
        As => 11,
    }
}
