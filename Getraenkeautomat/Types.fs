namespace Getraenkeautomat

open ErrorHandling
open FSharpx.Collections

module Types = 

    // Moegliche Fehler
    type AdministrationError =
        | FachIstSchonLeerError
        | UnterschiedlicheProdukteImGleichenFachError
        | FachExistiertGarNichtError

    type BenutzungError =
        | NichtGenuegendWechselgeldError
        | FachExistiertNichtError
        | ZuWenigGeldEingeworfenError

    type Error = 
        | AdministrationError
        | BenutzungError

    // Typen
    type Anzahl = int
    type Groesse = int
    type Cent = int
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

    type FachKonfiguration = {
        nummer: Fachnummer
        preis: Preis
    }

    type Fach = {
        konfiguration: FachKonfiguration
        zustand: FachZustand
    }

    type Getraenkeautomat = {
        faecher: NonEmptyList<Fach>
        muenzen: Muenze list
    }

    type EingeworfenesGeld = Muenze list
    type Wechselgeld = Muenze list

    // Funktionen
    // Administration
    type InitialeKonfiguration = NonEmptyList<Fach> -> NonEmptyList<Muenze> -> Getraenkeautomat

    type GeldNachfuellen = Getraenkeautomat -> NonEmptyList<Muenze> -> Getraenkeautomat
    type GeldEntnehmen = Getraenkeautomat -> Muenze list * Getraenkeautomat

    type FachKonfigurationAendern = Getraenkeautomat -> FachKonfiguration -> Either<Getraenkeautomat, AdministrationError>
    type FachLeeren = Getraenkeautomat -> Fachnummer -> Either<Getraenkeautomat, AdministrationError>
    type FachFuellen = Getraenkeautomat -> Fachnummer -> Anzahl * Dose -> Either<Getraenkeautomat, AdministrationError>

    // Benutzung
    type ProduktKaufen = Getraenkeautomat -> Fachnummer -> EingeworfenesGeld -> Either<Wechselgeld * Dose, EingeworfenesGeld * BenutzungError>