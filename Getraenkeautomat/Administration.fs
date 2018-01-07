namespace Getraenkeautomat

open Types
open ErrorHandling
open FSharpx.Collections
open FSharpx.Collections

module Administration =
    let sort faecher = 
        faecher
        |> NonEmptyList.toList
        |> List.sortBy (fun fach -> fach.konfiguration.nummer) 
        |> NonEmptyList.ofList

    let initialeKonfiguration : InitialeKonfiguration =
        fun faecher muenzen ->
            {
                faecher = faecher |> sort
                muenzen = NonEmptyList.toList muenzen
            }

    let geldNachfuellen : GeldNachfuellen =
        fun getraenkeautomat muenzen ->
            { getraenkeautomat with muenzen = getraenkeautomat.muenzen @ (muenzen |> NonEmptyList.toList) } 

    let geldEntnehmen : GeldEntnehmen =
        fun getraenkeautomat ->
            (getraenkeautomat.muenzen, { getraenkeautomat with muenzen = []})
    
    let fachKonfigurationAendern : FachKonfigurationAendern =
        fun getraenkeautomat neueFachKonfiguration -> 
            let filtereFaecher faecher = 
                faecher
                |> NonEmptyList.toList
                |> List.filter (fun fach -> fach.konfiguration.nummer = neueFachKonfiguration.nummer) 

            let altesFach = 
                getraenkeautomat.faecher |> filtereFaecher
            
            match altesFach with 
                | [] -> fail FachExistiertGarNichtError
                | fach::_ -> 
                    let listeOhneAltesFach = getraenkeautomat.faecher |> NonEmptyList.toList |> List.except [fach]
                    let geaendertesFach = { fach with konfiguration = neueFachKonfiguration }
                    ok  { getraenkeautomat with 
                           faecher = geaendertesFach::listeOhneAltesFach |> NonEmptyList.ofList |> sort
                        }


(*
    type FachLeeren = Getraenkeautomat -> Fachnummer -> Either<Getraenkeautomat, AdministrationError>
    type FachFuellen = Getraenkeautomat -> Fachnummer -> Anzahl * Dose -> Either<Getraenkeautomat, AdministrationError>
     *)