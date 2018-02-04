module BenutzungTest

open Expecto
open FSharpx.Collections
open Getraenkeautomat.ErrorHandling
open Getraenkeautomat.Types
open Getraenkeautomat.Benutzung

[<Tests>]
let tests = 
    let fach1Leer = {
        preis = Preis 100
        zustand = Leer
      }
    let fach2gefuelltMitCola = {
        preis = Preis 100
        zustand =  NonEmptyList.create (Dose Cola) [] |> Gefuellt 
      }

    let generateTest testName wechselgeldImAutomat eingeworfenesGeld erwartetesWechselgeld =  
        testCase testName <| fun _ ->
            let automat = { faecher = [(2, fach2gefuelltMitCola)] |> Map.ofList; muenzen = wechselgeldImAutomat}
            
            let actual = produktKaufen automat 2 eingeworfenesGeld

            match actual with
                | Ok (gekauft, _) ->
                    Expect.equal (fst gekauft) erwartetesWechselgeld "Wechselgeld stimmt nicht"
                    Expect.equal (snd gekauft) (Dose Cola) "Produkt nicht erhalten"

                | Bad _ -> Expect.isTrue false "Unerwartet im Error Case"


    testList "BenutzungTest" [
        testList "produktKaufen" [
          // Getraenkeautomat -> Fachnummer -> EingeworfenesGeld -> Either<Wechselgeld * Dose, EingeworfenesGeld * BenutzungError>
            testCase "Ungültiges Fach führt zu Fehlermeldung" <| fun _ ->
                let automat = { faecher = [(1, fach1Leer)] |> Map.ofList; muenzen = []}

                let actual = produktKaufen automat 2 []

                Expect.equal (fail ([], FachExistiertNichtError)) actual "Error erwartet"

            testCase "Leeres Fach führt zu Fehlermeldung" <| fun _ ->
                let automat = { faecher = [(1, fach1Leer)] |> Map.ofList; muenzen = []}

                let actual = produktKaufen automat 1 []

                Expect.equal (fail ([], FachIstLeerError)) actual "Error erwartet"

            testCase "Zuwenig Geld einwerfen führt zu Fehlermeldung" <| fun _ ->
                let automat = { faecher = [(2, fach2gefuelltMitCola)] |> Map.ofList; muenzen = []}

                let actual = produktKaufen automat 2 []

                Expect.equal (fail ([], ZuWenigGeldEingeworfenError)) actual "Error erwartet"

            testCase "Geld passend eingeworfen führt zu Dose und keinem Wechselgeld" <| fun _ ->
                let automat = { faecher = [(2, fach2gefuelltMitCola)] |> Map.ofList; muenzen = []}

                let actual = produktKaufen automat 2 [EinEuro]

                match actual with
                | Ok (gekauft, _) ->
                    Expect.equal (fst gekauft) [] "Wechselgeld stimmt nicht"
                    Expect.equal (snd gekauft) (Dose Cola) "Produkt nicht erhalten"

                | Bad _ -> Expect.isTrue false "Unerwartet im Error Case"

            testCase "Zuviel Geld & kein Wechselgeld führt zu Fehlermeldung und Rückgabe des eingeworfenen Gelds" <| fun _ ->
                let automat = { faecher = [(2, fach2gefuelltMitCola)] |> Map.ofList; muenzen = []}

                let actual = produktKaufen automat 2 [ZweiEuro]

                Expect.equal (fail ([ZweiEuro], NichtGenuegendWechselgeldError)) actual "Error erwartet"

            testCase "Zuviel Geld & unpassendes Wechselgeld führt zu Fehlermeldung und Rückgabe des eingeworfenen Gelds" <| fun _ ->
                let automat = { faecher = [(2, fach2gefuelltMitCola)] |> Map.ofList; muenzen = [ZehnCent; ZehnCent; FuenfzigCent;]}

                let actual = produktKaufen automat 2 [ZweiEuro]

                Expect.equal (fail ([ZweiEuro], NichtGenuegendWechselgeldError)) actual "Error erwartet"

            testList "Diverse Wechselgeld-Fälle" [
                generateTest "Eine Münze vorhanden, passendes Wechselgeld" [EinEuro] [ZweiEuro] [EinEuro]
                generateTest "Kleine Münzen vorhanden, passendes Wechselgeld" [ZwanzigCent; ZwanzigCent; ZwanzigCent; ZwanzigCent; ZwanzigCent;] [ZweiEuro] [ZwanzigCent; ZwanzigCent; ZwanzigCent; ZwanzigCent; ZwanzigCent;]
                generateTest "Mehr Münze vorhanden als nötig, passendes Wechselgeld" [ZwanzigCent; ZwanzigCent; FuenfzigCent; EinEuro; ZweiEuro;] [ZweiEuro] [EinEuro]
                generateTest "Mehr Münze vorhanden als nötig und vorher in falscher Reihenfolge, passendes Wechselgeld" [FuenfzigCent; ZehnCent; ZwanzigCent; ZehnCent; FuenfCent; FuenfCent;] [ZweiEuro] [FuenfzigCent; ZehnCent; ZwanzigCent; ZehnCent; FuenfCent; FuenfCent;]
            ]
        ]
    ]