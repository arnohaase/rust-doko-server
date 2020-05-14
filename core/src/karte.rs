
#[derive(Copy,Clone,PartialEq,Eq,Hash)]
pub enum Farbe {
    Kreuz,
    Pik,
    Herz,
    Karo
}

#[derive(Copy,Clone,PartialEq,Eq,Hash)]
pub enum Hoehe {
    Neun,
    Zehn,
    Bube,
    Dame,
    Koenig,
    As
}

#[derive(Copy,Clone,PartialEq,Eq,Hash)]
pub struct Karte {
    pub farbe: Farbe,
    pub hoehe: Hoehe,
}

