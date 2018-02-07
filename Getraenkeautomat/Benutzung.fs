namespace Getraenkeautomat

open Types
open ErrorHandling

module Benutzung =
    let produktKaufen : ProduktKaufen =
        fun getraenkeautomat fachnummer eingeworfenesGeld ->
            let getMuenzwertInCent muenze =
                match muenze with
                    | EinCent -> Cent 1
                    | ZweiCent -> Cent 2
                    | FuenfCent -> Cent 5
                    | ZehnCent -> Cent 10
                    | ZwanzigCent -> Cent 20
                    | FuenfzigCent -> Cent 50
                    | EinEuro -> Cent 100
                    | ZweiEuro -> Cent 200

            let addiereCent betrag =
                let (Cent betrag') = betrag
                betrag'

            let muenzenSumme muenzen =
                List.map getMuenzwertInCent muenzen 
                |> List.sumBy addiereCent


            match Map.containsKey fachnummer getraenkeautomat.faecher with
                | false -> fail (eingeworfenesGeld, FachExistiertNichtError)
                | true ->
                    let fach = Map.find fachnummer getraenkeautomat.faecher

                    match fach.zustand with
                        | Leer -> fail (eingeworfenesGeld, FachIstLeerError)
                        | Gefuellt inhalt ->
                            let (Preis preis') = fach.preis
                            let (Cent preis'') = preis'
                            if (preis'' > muenzenSumme eingeworfenesGeld) then
                                fail (eingeworfenesGeld, ZuWenigGeldEingeworfenError)
                            else
                                failwith "tbd"

