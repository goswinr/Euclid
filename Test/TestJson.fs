module TestJson

open Euclid

#if FABLE_COMPILER
open Fable.Mocha

let tests =
    testList "Pnt JSON serialization" []
#else
open Expecto

let private pnt = Pnt(1.5, -2.25, 3.75)
let private json = """{"X":1.5,"Y":-2.25,"Z":3.75}"""

let private expectPnt (expected:Pnt) (actual:Pnt) =
    Expect.equal actual.X expected.X "X coordinate"
    Expect.equal actual.Y expected.Y "Y coordinate"
    Expect.equal actual.Z expected.Z "Z coordinate"

let tests =
    testList "Pnt JSON serialization" [

        testCase "System.Text.Json serializes with default configuration" <| fun _ ->
            let actual = System.Text.Json.JsonSerializer.Serialize(pnt)
            Expect.equal actual json "serialized JSON"

        testCase "System.Text.Json deserializes with default configuration" <| fun _ ->
            let actual = System.Text.Json.JsonSerializer.Deserialize<Pnt>(json)
            expectPnt pnt actual

        testCase "Newtonsoft.Json serializes with default configuration" <| fun _ ->
            let actual = Newtonsoft.Json.JsonConvert.SerializeObject(pnt)
            Expect.equal actual json "serialized JSON"

        testCase "Newtonsoft.Json deserializes with default configuration" <| fun _ ->
            let actual = Newtonsoft.Json.JsonConvert.DeserializeObject<Pnt>(json)
            expectPnt pnt actual
    ]
#endif
