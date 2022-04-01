
#[derive(Copy,Clone,PartialEq,Eq,Hash,Debug)]
pub enum Farbe {
    Kreuz,
    Pik,
    Herz,
    Karo
}

#[derive(Copy,Clone,PartialEq,Eq,Hash,Debug)]
pub enum Hoehe {
    Neun,
    Zehn,
    Bube,
    Dame,
    Koenig,
    As
}

#[derive(Copy,Clone,PartialEq,Eq,Hash,Debug)]
pub struct Karte {
    pub farbe: Farbe,
    pub hoehe: Hoehe,
}
impl Karte {
    pub fn wert(&self) -> u32 {
        use Hoehe::*;
        match self.hoehe {
            Neun => 0,
            Bube => 2,
            Dame => 3,
            Koenig => 4,
            Zehn => 10,
            As => 11,
        }
    }

}
