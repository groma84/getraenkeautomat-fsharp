module AdministrationTest

open Expecto
open FSharpx.Collections
open Getraenkeautomat.Types
open Getraenkeautomat.Administration

[<Tests>]
let tests = 
  let fach1 = {
    konfiguration = { nummer = 1; preis = Preis 100 }
    zustand = Leer
  }
  let fach2 = {
    konfiguration = { nummer = 2; preis = Preis 200 }
    zustand = Leer
  }

  let muenze1 = EinCent
  let muenze2 = ZweiCent

  testList "AdministrationTest" [
      testList "initialeKonfiguration" [
        testCase "Automat wird mit korrektem Fach initialisiert" <| fun _ ->
          let faecher = [fach1] |> NonEmptyList.ofList
          let muenzen = [muenze1] |> NonEmptyList.ofList
          
          let actual = initialeKonfiguration faecher muenzen
          
          Expect.equal actual.faecher faecher "Fächer"
        

        testCase "Automat wird mit korrekter Münze initialisiert" <| fun _ ->
          let faecher = [fach1] |> NonEmptyList.ofList
          let muenzen = [muenze1] |> NonEmptyList.ofList
          
          let actual = initialeKonfiguration faecher muenzen
          
          Expect.equal actual.muenzen (muenzen |> NonEmptyList.toList) "Münzen"


        testCase "Fächer werden korrekt sortiert" <| fun _ ->
          let faecher = [fach2; fach1] |> NonEmptyList.ofList
          let muenzen = [muenze1] |> NonEmptyList.ofList
          
          let actual = initialeKonfiguration faecher muenzen
          
          Expect.equal actual.faecher ([fach1; fach2] |> NonEmptyList.ofList) "sortierte Fächer"
      ]

      testList "geldNachfuellen" [
        testCase "Leerer Geldautomat hat danach Münze" <| fun _ ->
          let keineMuenzenDrin = { faecher = [fach1] |> NonEmptyList.ofList; muenzen = []}
          let neueMuenze = [muenze1] |> NonEmptyList.ofList
          
          let actual = geldNachfuellen keineMuenzenDrin neueMuenze

          Expect.equal actual.muenzen (neueMuenze |> NonEmptyList.toList) "Münzen"

        testCase "Leerer Geldautomat hat danach mehrere Münzen" <| fun _ ->
          let keineMuenzenDrin = { faecher = [fach1] |> NonEmptyList.ofList; muenzen = []}
          let neueMuenzen = [muenze1; muenze2] |> NonEmptyList.ofList
          
          let actual = geldNachfuellen keineMuenzenDrin neueMuenzen

          Expect.equal actual.muenzen (neueMuenzen |> NonEmptyList.toList) "Münzen"
        
        testCase "Automat mit Münzen drin hat danach noch mehr Münzen drin" <| fun _ ->
          let muenzen = [muenze1; muenze2]
          let schonMuenzenDrin = { faecher = [fach1] |> NonEmptyList.ofList; muenzen = muenzen}
          let neueMuenzen = muenzen |> NonEmptyList.ofList
          
          let actual = geldNachfuellen schonMuenzenDrin neueMuenzen

          Expect.equal actual.muenzen (muenzen @ muenzen) "Münzen"
      ]
  ]