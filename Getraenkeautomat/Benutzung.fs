namespace Getraenkeautomat

open Types
open ErrorHandling
open FSharpx.Collections

module Benutzung =
    let getMuenzwert muenze =
        match muenze with
            | EinCent -> 1
            | ZweiCent -> 2
            | FuenfCent -> 5
            | ZehnCent -> 10
            | ZwanzigCent -> 20
            | FuenfzigCent -> 50
            | EinEuro -> 100
            | ZweiEuro -> 200

    let produktKaufen : ProduktKaufen =
        fun getraenkeautomat fachnummer eingeworfenesGeld ->    
            let muenzenSumme muenzen =
                List.sumBy getMuenzwert muenzen 

            let rec getWechselgeld nochZuWechseln restlicheMuenzen auszugebendeMuenzen =
                let removeAt index list =
                    list |> List.indexed |> List.filter (fun (i, _) -> i <> index) |> List.map snd

                printfn "nochZuWechseln %d -- restlicheMuenzen %O -- auszugebendeMuenzen %O" nochZuWechseln restlicheMuenzen auszugebendeMuenzen
                match nochZuWechseln with                  
                    | 0 -> ok (restlicheMuenzen, auszugebendeMuenzen)
                    | _ -> 
                        let passendeWechselmuenzeIndex = List.tryFindIndex (fun m -> getMuenzwert m <= nochZuWechseln) restlicheMuenzen
                        match passendeWechselmuenzeIndex with
                        | None -> fail NichtGenuegendWechselgeldError
                        | Some index ->
                            let wechselmuenze = restlicheMuenzen.Item index
                            let bisherigesWechselgeld = wechselmuenze::auszugebendeMuenzen
                            let restlichesWechselgeld = removeAt index restlicheMuenzen
                            let restlicherWechselbetrag = nochZuWechseln - getMuenzwert wechselmuenze
                            getWechselgeld restlicherWechselbetrag restlichesWechselgeld bisherigesWechselgeld
                            

            match Map.containsKey fachnummer getraenkeautomat.faecher with
                | false -> { eingeworfenesGeld = eingeworfenesGeld; fehler = FachExistiertNichtError } |> Fehler
                | true ->
                    let fach = Map.find fachnummer getraenkeautomat.faecher

                    match fach.zustand with
                        | Leer -> { eingeworfenesGeld = eingeworfenesGeld; fehler = FachIstLeerError } |> Fehler
                        | Gefuellt inhalt ->
                            let sortGeld muenzen =
                                List.sortByDescending getMuenzwert muenzen

                            let (Preis preis') = fach.preis
                            let (Cent preis'') = preis'
                            if (preis'' > muenzenSumme eingeworfenesGeld) then
                                { eingeworfenesGeld = eingeworfenesGeld; fehler = ZuWenigGeldEingeworfenError } |> Fehler
                            else
                                let sortedGesamtgeld = sortGeld (getraenkeautomat.muenzen @ eingeworfenesGeld)
                                let notwendigerWechselbetrag = (List.sumBy getMuenzwert eingeworfenesGeld) - preis'' 
                                let wechselgeld = getWechselgeld notwendigerWechselbetrag sortedGesamtgeld [] 

                                match wechselgeld with
                                | Bad err ->
                                    { eingeworfenesGeld = eingeworfenesGeld; fehler = List.head err } |> Fehler
                                | Ok (restlichesGeldUndWechselgeld, _) ->
                                    let (restlichesGeld, wechselgeld) = restlichesGeldUndWechselgeld
                                    let gekaufterGegenstand = NonEmptyList.head inhalt
                                    let neuerInhalt = NonEmptyList.tail inhalt
                                    let neuerZustand =
                                        match inhalt.Length with
                                        | 1 ->
                                            Leer
                                        | _ ->
                                            Gefuellt (NonEmptyList.create (List.head neuerInhalt) (List.tail neuerInhalt))

                                    let geaendertesFach = { fach with zustand = neuerZustand }
                                    let geanderterAutomat = { getraenkeautomat with faecher = Map.add fachnummer geaendertesFach getraenkeautomat.faecher; muenzen = restlichesGeld }
                                    {
                                        getraenkeautomat = geanderterAutomat
                                        wechselgeld = sortGeld wechselgeld
                                        gegenstand = gekaufterGegenstand
                                    } |> Erfolg
