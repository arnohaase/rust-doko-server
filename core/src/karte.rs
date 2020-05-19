
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

