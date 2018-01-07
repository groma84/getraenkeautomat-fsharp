namespace Getraenkeautomat

open Types
open ErrorHandling

module Administration =
    let sort faecher = 
        faecher |> List.sortBy (fun fach -> fach.konfiguration.nummer) 

    let initialeKonfiguration : InitialeKonfiguration =
        fun faecher muenzen ->
            {
                faecher = faecher |> sort
                geld = muenzen
            }

    let geldNachfuellen : GeldNachfuellen =
        fun getraenkeautomat muenzen ->
            { getraenkeautomat with geld = getraenkeautomat.geld @ muenzen } 

    let geldEntnehmen : GeldEntnehmen =
        fun getraenkeautomat ->
            (getraenkeautomat.geld, { getraenkeautomat with geld = []})
    
    let fachKonfigurationAendern : FachKonfigurationAendern =
        fun getraenkeautomat neueFachKonfiguration -> 
            let altesFach = 
                getraenkeautomat.faecher |> List.filter (fun fach -> fach.konfiguration.nummer = neueFachKonfiguration.nummer) 
            
            match altesFach with 
                
                | [] -> fail FachExistiertGarNichtError
                | fach::_ -> 
                    let listeOhneAltesFach = getraenkeautomat.faecher |> List.except [fach]
                    let geaendertesFach = { fach with konfiguration = neueFachKonfiguration }
                    ok  { getraenkeautomat with 
                           faecher = geaendertesFach::listeOhneAltesFach |> sort
                        }


(*
    type FachLeeren = Getraenkeautomat -> Fachnummer -> Either<Getraenkeautomat, AdministrationError>
    type FachFuellen = Getraenkeautomat -> Fachnummer -> Anzahl * Dose -> Either<Getraenkeautomat, AdministrationError>
     *)