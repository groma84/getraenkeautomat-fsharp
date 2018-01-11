namespace Getraenkeautomat

open Types
open ErrorHandling
open FSharpx.Collections

module Administration =
    let initialeKonfiguration : InitialeKonfiguration =
        fun faecher muenzen ->
            match (NonEmptyList.length faecher) = (faecher |> NonEmptyList.toList |> List.distinctBy fst |> List.length) with
                | false -> fail DoppelteFachnummerError
                | true -> 
                    let f =
                        faecher
                        |> NonEmptyList.toList
                        |> Map.ofList
                    
                    ok {
                        faecher = f
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
            let fachnummer, neuerPreis = neueFachKonfiguration

            match Map.containsKey fachnummer getraenkeautomat.faecher with
                | false -> fail FachExistiertGarNichtError
                | true ->
                    let altesFach = Map.find fachnummer getraenkeautomat.faecher
                    let geaendertesFach = { altesFach with preis = neuerPreis }
                    let changedFaecher = Map.updateWith (fun _ -> Some geaendertesFach) fachnummer getraenkeautomat.faecher
                    ok  { getraenkeautomat with faecher = changedFaecher }
            
    let fachLeeren : FachLeeren =
        fun getraenkeautomat fachnummer ->
            match Map.containsKey fachnummer getraenkeautomat.faecher with
                | false -> fail FachExistiertGarNichtError
                | true ->
                    let zuLeerendesFach = Map.find fachnummer getraenkeautomat.faecher
                    match zuLeerendesFach.zustand with
                    | Leer -> fail FachIstSchonLeerError
                    | Gefuellt inhalt -> 
                        let geleertesFach = { zuLeerendesFach with zustand = Leer } 
                        let changedFaecher = Map.updateWith (fun _ -> Some geleertesFach) fachnummer getraenkeautomat.faecher

                        ok (inhalt, { getraenkeautomat with faecher = changedFaecher })

    let fachFuellen : FachFuellen =
        failwith "TODO"
(*
    type FachLeeren = Getraenkeautomat -> Fachnummer -> Either<Getraenkeautomat, AdministrationError>
    type FachFuellen = Getraenkeautomat -> Fachnummer -> Anzahl * Dose -> Either<Getraenkeautomat, AdministrationError>
     *)