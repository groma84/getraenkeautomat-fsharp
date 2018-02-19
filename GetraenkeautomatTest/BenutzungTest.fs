module BenutzungTest

open Expecto
open FSharpx.Collections
open Getraenkeautomat.ErrorHandling
open Getraenkeautomat.Types
open Getraenkeautomat.Benutzung

[<Tests>]
let tests = 
    let fach1Leer = {
        preis = Preis <| Cent 100
        zustand = Leer
      }
    let fach2gefuelltMitCola = {
        preis = Preis <| Cent 100
        zustand =  NonEmptyList.create (Dose Cola) [] |> Gefuellt 
      }

    let generateTest testName wechselgeldImAutomat eingeworfenesGeld erwartetesWechselgeld =  
        testCase testName <| fun _ ->
            let removeAt index list =
                    list |> List.indexed |> List.filter (fun (i, _) -> i <> index) |> List.map snd

            let rec removeAusgespucktesWechselgeld nochDrin nochZuEntfernen =
                match nochZuEntfernen with
                | [] -> nochDrin
                | now::later -> 
                    let idx = List.findIndex (fun m -> getMuenzwert m = getMuenzwert now) nochDrin
                    let jetztNochDrin = removeAt idx nochDrin
                    removeAusgespucktesWechselgeld jetztNochDrin later

            let automat = { faecher = [(2, fach2gefuelltMitCola)] |> Map.ofList; muenzen = wechselgeldImAutomat}
            
            let muenzenDavor = wechselgeldImAutomat @ eingeworfenesGeld 
            let automatDanach = { 
                faecher = [(2, {fach2gefuelltMitCola with zustand = Leer})]  |> Map.ofList;
                muenzen = removeAusgespucktesWechselgeld muenzenDavor erwartetesWechselgeld |> List.sortByDescending getMuenzwert
                }
            let actual = produktKaufen automat 2 eingeworfenesGeld

            match actual with
                | Erfolg erf ->
                    Expect.equal erf.wechselgeld erwartetesWechselgeld "Wechselgeld stimmt nicht"
                    Expect.equal erf.gegenstand (Dose Cola) "Produkt nicht erhalten"
                    Expect.equal erf.getraenkeautomat automatDanach "Automat nicht korrekt modifiziert"

                | Fehler _ -> Expect.isTrue false "Unerwartet im Error Case"


    testList "BenutzungTest" [
        testList "produktKaufen" [
            testList "Fälle, die in einem Fehler enden sollen" [
                testCase "Ungültiges Fach führt zu Fehlermeldung" <| fun _ ->
                    let automat = { faecher = [(1, fach1Leer)] |> Map.ofList; muenzen = []}

                    let actual = produktKaufen automat 2 []

                    Expect.equal ({eingeworfenesGeld = []; fehler = FachExistiertNichtError} |> Fehler) actual "Error erwartet"

                testCase "Leeres Fach führt zu Fehlermeldung" <| fun _ ->
                    let automat = { faecher = [(1, fach1Leer)] |> Map.ofList; muenzen = []}

                    let actual = produktKaufen automat 1 []

                    Expect.equal ({eingeworfenesGeld = []; fehler = FachIstLeerError}  |> Fehler) actual "Error erwartet"

                testCase "Zuwenig Geld einwerfen führt zu Fehlermeldung" <| fun _ ->
                    let automat = { faecher = [(2, fach2gefuelltMitCola)] |> Map.ofList; muenzen = []}

                    let actual = produktKaufen automat 2 []

                    Expect.equal ({eingeworfenesGeld = []; fehler =  ZuWenigGeldEingeworfenError} |> Fehler) actual "Error erwartet"

                testCase "Zuviel Geld & kein Wechselgeld führt zu Fehlermeldung und Rückgabe des eingeworfenen Gelds" <| fun _ ->
                    let automat = { faecher = [(2, fach2gefuelltMitCola)] |> Map.ofList; muenzen = []}

                    let actual = produktKaufen automat 2 [ZweiEuro]

                    Expect.equal ({eingeworfenesGeld = [ZweiEuro]; fehler = NichtGenuegendWechselgeldError} |> Fehler) actual "Error erwartet"

                testCase "Zuviel Geld & unpassendes Wechselgeld führt zu Fehlermeldung und Rückgabe des eingeworfenen Gelds" <| fun _ ->
                    let automat = { faecher = [(2, fach2gefuelltMitCola)] |> Map.ofList; muenzen = [ZehnCent; ZehnCent; FuenfzigCent;]}

                    let actual = produktKaufen automat 2 [ZweiEuro]

                    Expect.equal ({eingeworfenesGeld = [ZweiEuro]; fehler = NichtGenuegendWechselgeldError} |> Fehler) actual "Error erwartet" 
                ]

            testList "Diverse Wechselgeld-Fälle mit positivem Ergebnis" [
                generateTest "Geld passend eingeworfen führt zu Dose und keinem Wechselgeld" [] [EinEuro] []
                generateTest "Eine Münze vorhanden, passendes Wechselgeld" [EinEuro] [ZweiEuro] [EinEuro]
                generateTest "Kleine Münzen vorhanden, passendes Wechselgeld" [ZwanzigCent; ZwanzigCent; ZwanzigCent; ZwanzigCent; ZwanzigCent;] [ZweiEuro] [ZwanzigCent; ZwanzigCent; ZwanzigCent; ZwanzigCent; ZwanzigCent;]
                generateTest "Mehr Münze vorhanden als nötig, passendes Wechselgeld" [ZwanzigCent; ZwanzigCent; FuenfzigCent; EinEuro; ZweiEuro;] [ZweiEuro] [EinEuro]
                generateTest "Mehr Münze vorhanden als nötig und vorher in falscher Reihenfolge, passendes Wechselgeld" [FuenfzigCent; ZehnCent; ZwanzigCent; ZehnCent; FuenfCent; FuenfCent;] [ZweiEuro] [FuenfzigCent; ZwanzigCent; ZehnCent; ZehnCent; FuenfCent; FuenfCent;]
            ]
        ]
    ]