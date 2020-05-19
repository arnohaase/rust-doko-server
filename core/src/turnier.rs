use std::sync::Arc;

use crate::regelsatz::RegelsatzRegistry;
use crate::spiel::*;

/// Dies ist ein Turnier aus Sicht der Regln. Verwaltung von Spielernamen, Netzwerkverbindungen
///  etc. erfolgen separat.
pub struct Turnier {
    regelsatz_registry: Arc<RegelsatzRegistry>,
    pub gesamtzahl_normale_spiele: u32,
    pub anzahl_normale_spiele_gespielt: u32,
    pub erster_spieler_naechstes_spiel: SpielerNummer,
    pub punkte: [i32;4],
    pub solo_gespielt: [bool;4],

    pub history: Vec<Spiel>,
}
impl Turnier {
    pub fn new(regelsatz_registry: Arc<RegelsatzRegistry>, gesamtzahl_normale_runden: u32) -> Turnier {
        Turnier {
            regelsatz_registry,
            gesamtzahl_normale_spiele: gesamtzahl_normale_runden*4,
            anzahl_normale_spiele_gespielt: 0,
            erster_spieler_naechstes_spiel: SpielerNummer::new(0),
            punkte: [0;4],
            solo_gespielt: [false;4],
            history: vec![],
        }
    }

    pub fn starte_spiel(&self) -> Spiel {
        let pflichtsolo_jetzt = match self.regelsatz_registry.variante.pflichtsolo_alle_n_runden {
            Some(pflichtsolo_runden) => {
                if self.anzahl_normale_spiele_gespielt == 4*pflichtsolo_runden - 1 {
                    [
                        !self.solo_gespielt[0],
                        !self.solo_gespielt[1],
                        !self.solo_gespielt[2],
                        !self.solo_gespielt[3],
                    ]
                }
                else {
                    [false;4]
                }
            },
            None => [false;4]
        };

        Spiel::new(self.regelsatz_registry.clone(), self.erster_spieler_naechstes_spiel, pflichtsolo_jetzt)
    }

    /// arbeitet das Spielergebnis in die Turnierdaten ein und liefert true genau dann, wenn das
    ///  Turnier mit diesem Spiel beendet ist
    pub fn on_spiel_ergebnis(&mut self, spiel: Spiel) -> bool {
        match &spiel.phase {
            SpielPhase::Abgeschlossen(data) => {
                match data.besonderheit {
                    Some(SpielBesonderheit::Solo(_, spieler)) => {
                        self.solo_gespielt[spieler.as_usize()] = true;
                    },
                    _ => {
                        self.anzahl_normale_spiele_gespielt += 1;

                        if let Some(pflichtsolo_runden) = self.regelsatz_registry.variante.pflichtsolo_alle_n_runden {
                            if self.anzahl_normale_spiele_gespielt == pflichtsolo_runden*4 {
                                self.solo_gespielt = [false;4];
                            }
                        }
                    }
                }

                for s in data.re_spieler.iter() {
                    self.punkte[s.as_usize()] += data.punkte_re;
                }
                for s in data.contra_spieler.iter() {
                    self.punkte[s.as_usize()] += data.punkte_contra;
                }

                self.history.push(spiel);
                self.erster_spieler_naechstes_spiel = self.erster_spieler_naechstes_spiel.naechster();

                self.anzahl_normale_spiele_gespielt >= self.gesamtzahl_normale_spiele
            },
            _ => {
                panic!(format!("nicht abgeschlossenes Spiel als Ergebnis übergeben: {:?}", spiel));
            }
        }
    }
}
