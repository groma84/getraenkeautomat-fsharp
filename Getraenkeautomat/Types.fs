namespace Getraenkeautomat

open ErrorHandling
open FSharpx.Collections

module Types = 

    // Moegliche Fehler
    type AdministrationError =
        | DoppelteFachnummerError
        | FachIstSchonLeerError
        | UnterschiedlicheProdukteImGleichenFachError
        | FachExistiertGarNichtError

    type BenutzungError =
        | NichtGenuegendWechselgeldError
        | FachExistiertNichtError
        | ZuWenigGeldEingeworfenError
        | FachIstLeerError

    type Error = 
        | AdministrationError
        | BenutzungError

    // Typen
    type Cent = Cent of int
    type Fachnummer = int
    type Muenze = 
        | EinCent
        | ZweiCent
        | FuenfCent
        | ZehnCent
        | ZwanzigCent
        | FuenfzigCent
        | EinEuro
        | ZweiEuro

    type Preis = Preis of Cent

    type Produkt = 
        | Cola
        | Fanta
        | Sprite

    type Dose = Dose of Produkt

    type FachZustand = 
        | Leer
        | Gefuellt of NonEmptyList<Dose>

    type Fach = {
        preis: Preis
        zustand: FachZustand
    }

    type Getraenkeautomat = {
        faecher: Map<Fachnummer, Fach>
        muenzen: Muenze list
    }

    type EingeworfenesGeld = Muenze list
    type Wechselgeld = Muenze list

    // Funktionen
    // Administration
    type InitialeKonfiguration = NonEmptyList<Fachnummer * Fach> -> NonEmptyList<Muenze> -> Either<Getraenkeautomat, AdministrationError>

    type GeldNachfuellen = Getraenkeautomat -> NonEmptyList<Muenze> -> Getraenkeautomat
    type GeldEntnehmen = Getraenkeautomat -> Muenze list * Getraenkeautomat

    type FachKonfigurationAendern = Getraenkeautomat -> (Fachnummer * Preis) -> Either<Getraenkeautomat, AdministrationError>
    type FachLeeren = Getraenkeautomat -> Fachnummer -> Either<NonEmptyList<Dose> * Getraenkeautomat, AdministrationError>
    type FachFuellen = Getraenkeautomat -> Fachnummer -> NonEmptyList<Dose> -> Either<Getraenkeautomat, AdministrationError>

    // Benutzung
    type EinkaufsergebnisErfolgreich = {
        getraenkeautomat: Getraenkeautomat
        wechselgeld: Muenze list
        gegenstand: Dose
    }
    type EinkaufsergebnisMisserfolg = {
        eingeworfenesGeld: EingeworfenesGeld
        fehler: BenutzungError
    }
    type Einkaufsergebnis = 
        | Erfolg of EinkaufsergebnisErfolgreich 
        | Fehler of EinkaufsergebnisMisserfolg
        
    type ProduktKaufen = Getraenkeautomat -> Fachnummer -> EingeworfenesGeld -> Einkaufsergebnis