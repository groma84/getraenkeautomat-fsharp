module AdministrationTest

open Expecto
open FSharpx.Collections
open Getraenkeautomat.ErrorHandling
open Getraenkeautomat.Types
open Getraenkeautomat.Administration
open FSharpx.Collections

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

  let muenze1 = EinCent
  let muenze2 = ZweiCent

  let compareListAndMap l m s =
    let mapAsList = Map.toList m
    let listAsList = NonEmptyList.toList l
    Expect.equal mapAsList listAsList s

  testList "AdministrationTest" [
      testList "initialeKonfiguration" [
        testCase "Automat wird mit korrektem Fach initialisiert" <| fun _ ->
          let faecher = [(1, fach1Leer)] |> NonEmptyList.ofList
          let muenzen = [muenze1] |> NonEmptyList.ofList
          
          let actual = initialeKonfiguration faecher muenzen
          
          

          compareListAndMap faecher (returnOrFail actual).faecher "Fächer"

        testCase "Automat wird mit korrekter Münze initialisiert" <| fun _ ->
          let faecher = [(1, fach1Leer)] |> NonEmptyList.ofList
          let muenzen = [muenze1] |> NonEmptyList.ofList
          
          let actual = initialeKonfiguration faecher muenzen
          
          Expect.equal (returnOrFail actual).muenzen (muenzen |> NonEmptyList.toList) "Münzen"

        testCase "doppelte Fachnummer führt zu Error" <| fun _ ->
          let faecher = [(1, fach1Leer); (1, fach1Leer)] |> NonEmptyList.ofList
          let muenzen = [muenze1] |> NonEmptyList.ofList
          
          let actual = initialeKonfiguration faecher muenzen
          
          Expect.equal (fail DoppelteFachnummerError) actual "Error erwartet"
      ]

      testList "geldNachfuellen" [
        testCase "Leerer Geldautomat hat danach Münze" <| fun _ ->
          let keineMuenzenDrin = { faecher = [(1, fach1Leer)] |> Map.ofList; muenzen = []}
          let neueMuenze = [muenze1] |> NonEmptyList.ofList
          
          let actual = geldNachfuellen keineMuenzenDrin neueMuenze

          Expect.equal actual.muenzen (neueMuenze |> NonEmptyList.toList) "Münzen"

        testCase "Leerer Geldautomat hat danach mehrere Münzen" <| fun _ ->
          let keineMuenzenDrin = { faecher = [(1, fach1Leer)] |> Map.ofList; muenzen = []}
          let neueMuenzen = [muenze1; muenze2] |> NonEmptyList.ofList
          
          let actual = geldNachfuellen keineMuenzenDrin neueMuenzen

          Expect.equal actual.muenzen (neueMuenzen |> NonEmptyList.toList) "Münzen"
        
        testCase "Automat mit Münzen drin hat danach noch mehr Münzen drin" <| fun _ ->
          let muenzen = [muenze1; muenze2]
          let schonMuenzenDrin = { faecher = [(1, fach1Leer)] |> Map.ofList; muenzen = muenzen}
          let neueMuenzen = muenzen |> NonEmptyList.ofList
          
          let actual = geldNachfuellen schonMuenzenDrin neueMuenzen

          Expect.equal actual.muenzen (muenzen @ muenzen) "Münzen"
      ]

      testList "geldEntnehmen" [
        testCase "Leerer Automat -> kein Geld und weiterhin leerer Automat" <| fun _ ->
          let keineMuenzenDrin = { faecher = [(1, fach1Leer)] |> Map.ofList; muenzen = []}

          let actualM, actualG = geldEntnehmen keineMuenzenDrin

          Expect.equal actualM [] "Münzen"
          Expect.equal actualG.muenzen [] "Münzen im Automat"

        testCase "Automat mit Münzen drin hat danach keine Münzen mehr - denn die habe dann ich" <| fun _ ->
          let muenzen = [muenze1; muenze2]
          let schonMuenzenDrin = { faecher = [(1, fach1Leer)] |> Map.ofList; muenzen = muenzen}

          let actualM, actualG = geldEntnehmen schonMuenzenDrin
          
          Expect.equal actualM muenzen "Münzen"
          Expect.equal actualG.muenzen [] "Münzen im Automat"
      ]

      testList "fachKonfigurationAendern" [
        testCase "Fach existiert nicht" <| fun _ ->
          let automat = { faecher = [(1, fach1Leer)] |> Map.ofList; muenzen = []}

          let actual = fachKonfigurationAendern automat (2, Preis 200)
          
          Expect.equal (fail FachExistiertGarNichtError) actual "Error erwartet"

        testCase "Fachkonfiguration wird geändert" <| fun _ ->
          let automat = { faecher = [(1, fach1Leer)] |> Map.ofList; muenzen = []}

          let actual = fachKonfigurationAendern automat (1, Preis 1337)
          
          match actual with
            | Ok (changed, _) -> 
                let fach = Map.find 1 changed.faecher 
                Expect.equal fach {fach1Leer with preis = Preis 1337} "Geänderte Fach-Konfiguration"
            | Bad _ -> Expect.isTrue false "Unerwartet im Error Case"
      ]

      testList "fachLeeren" [
        testCase "Leeres Fach kann nicht geleert werden" <| fun _ ->
          let automat = { faecher = [(1, fach1Leer)] |> Map.ofList; muenzen = []}

          let actual = fachLeeren automat 1

          Expect.equal (fail FachIstSchonLeerError) actual "Error erwartet"

        testCase "Gefülltes Fach ist danach leer" <| fun _ ->
          let automat = { faecher = [(3, fach2gefuelltMitCola)] |> Map.ofList; muenzen = []}

          let actual = fachLeeren automat 3
          
          match actual with
            | Ok (changed, _) -> 
              let _, automat = changed
              let fach = Map.find 3 automat.faecher
              Expect.equal fach.zustand Leer "Leeres Fach"
            | Bad _ -> Expect.isTrue false "Unerwartet im Error Case"

        testCase "Gefülltes Fach gibt Inhalt zurück" <| fun _ ->
          let automat = { faecher = [(3, fach2gefuelltMitCola)] |> Map.ofList; muenzen = []}

          let actual = fachLeeren automat 3
          
          match actual with
            | Ok (changed, _) -> 
              let inhalt, _ = changed
              Expect.equal inhalt (NonEmptyList.create (Dose Cola) []) "Leeres Fach"
            | Bad _ -> Expect.isTrue false "Unerwartet im Error Case"

        testCase "Ein nicht existierendes Fach kann nicht geleert werden" <| fun _ ->
          let automat = { faecher = [(3, fach2gefuelltMitCola)] |> Map.ofList; muenzen = []}
          
          let actual = fachLeeren automat 2
          
          Expect.equal (fail FachExistiertGarNichtError) actual "Error erwartet"
          
      ]

      testList "fachFuellen" [
        testCase "Leeres Fach kann gefüllt werden" <| fun _ ->
          let fachnummer = 1
          let automat = { faecher = [(fachnummer, fach1Leer)] |> Map.ofList; muenzen = []}
          let coladose = Dose Cola
          let neueDosen = NonEmptyList.create coladose []

          // Automat -> Fachnummer -> NonEmptyList<Dose> -> Either<Getraenkeautomat, AdministrationError>
          let actual  = fachFuellen automat 1 neueDosen

          match actual with
            | Ok (automat, _) -> 
              let fach = Map.find fachnummer automat.faecher
              Expect.equal fach.zustand (Gefuellt neueDosen) "Gefülltes Fach"
            | Bad _ -> Expect.isTrue false "Unerwartet im Error Case"

        testCase "Gefülltes Fach kann mit gleichem Produkt gefüllt werden" <| fun _ ->
          let fachnummer = 1
          let automat = { faecher = [(fachnummer, fach2gefuelltMitCola)] |> Map.ofList; muenzen = []}
          let coladose = Dose Cola
          let neueDosen = NonEmptyList.create coladose []
          
          let actual  = fachFuellen automat 1 neueDosen

          match actual with
            | Ok (automat, _) -> 
              let (Gefuellt alteDosen) = fach2gefuelltMitCola.zustand
              let alteUndNeueDose = NonEmptyList.append alteDosen neueDosen

              let fach = Map.find fachnummer automat.faecher
              Expect.equal fach.zustand (Gefuellt alteUndNeueDose) "Gefülltes Fach"
            | Bad _ -> Expect.isTrue false "Unerwartet im Error Case"

        testCase "Gefülltes Fach kann nicht mit anderem Produkt gefüllt werden" <| fun _ ->
          let fachnummer = 1
          let automat = { faecher = [(fachnummer, fach2gefuelltMitCola)] |> Map.ofList; muenzen = []}
          let fantadose = Dose Fanta
          let neueDosen = NonEmptyList.create fantadose []
          
          let actual  = fachFuellen automat 1 neueDosen

          Expect.equal (fail UnterschiedlicheProdukteImGleichenFachError) actual "Error erwartet"

        testCase "Ein nicht existierendes Fach kann nicht gefüllt werden" <| fun _ ->
          let automat = { faecher = [(3, fach1Leer)] |> Map.ofList; muenzen = []}
          let coladose = Dose Cola
          let neueDosen = NonEmptyList.create coladose []

          let actual = fachFuellen automat 2 neueDosen
          
          Expect.equal (fail FachExistiertGarNichtError) actual "Error erwartet"
      ]
  ]