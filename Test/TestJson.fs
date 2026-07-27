module TestJson

open Euclid

#if FABLE_COMPILER
open Fable.Mocha

let tests =
    testList "Pnt JSON serialization" []
#else
open System
open System.Runtime.Serialization
open System.Text.Json
open System.Text.Json.Serialization
open Expecto

let private pnt = Pnt(1.5, -2.25, 3.75)
let private json = """{"X":1.5,"Y":-2.25,"Z":3.75}"""

let private expectPnt (expected:Pnt) (actual:Pnt) =
    Expect.equal actual.X expected.X "X coordinate"
    Expect.equal actual.Y expected.Y "Y coordinate"
    Expect.equal actual.Z expected.Z "Z coordinate"

let rec private expectJsonElementsEqual context (expected:JsonElement) (actual:JsonElement) =
    Expect.equal actual.ValueKind expected.ValueKind $"{context}: JSON value kind"

    match expected.ValueKind with
    | JsonValueKind.Object ->
        let toMap (element:JsonElement) =
            element.EnumerateObject()
            |> Seq.map (fun property -> property.Name, property.Value.Clone())
            |> Map.ofSeq

        let expectedProperties = toMap expected
        let actualProperties = toMap actual
        Expect.equal (actualProperties |> Map.keys |> Set.ofSeq) (expectedProperties |> Map.keys |> Set.ofSeq)
            $"{context}: JSON property names"

        for KeyValue(name, expectedValue) in expectedProperties do
            expectJsonElementsEqual $"{context}.{name}" expectedValue actualProperties.[name]
    | JsonValueKind.Array ->
        let expectedItems = expected.EnumerateArray() |> Seq.toArray
        let actualItems = actual.EnumerateArray() |> Seq.toArray
        Expect.equal actualItems.Length expectedItems.Length $"{context}: JSON array length"
        Array.iter2 (fun expectedItem actualItem ->
            expectJsonElementsEqual context expectedItem actualItem) expectedItems actualItems
    | JsonValueKind.Number ->
        Expect.floatClose Accuracy.high (actual.GetDouble()) (expected.GetDouble()) $"{context}: JSON number"
    | JsonValueKind.String ->
        Expect.equal (actual.GetString()) (expected.GetString()) $"{context}: JSON string"
    | JsonValueKind.True
    | JsonValueKind.False ->
        Expect.equal (actual.GetBoolean()) (expected.GetBoolean()) $"{context}: JSON Boolean"
    | JsonValueKind.Null
    | JsonValueKind.Undefined ->
        ()
    | valueKind ->
        failwithf "Unsupported JSON value kind in test: %A" valueKind

let private expectJsonEqual context (expectedJson:string) (actualJson:string) =
    use expectedDocument = JsonDocument.Parse(expectedJson)
    use actualDocument = JsonDocument.Parse(actualJson)
    expectJsonElementsEqual context expectedDocument.RootElement actualDocument.RootElement

let private matrix =
    Matrix(
        1.5, 2.5, 3.5, 4.5,
        5.5, 6.5, 7.5, 8.5,
        9.5, 10.5, 11.5, 12.5,
        13.5, 14.5, 15.5, 16.5)

let private boxValue =
    Box.createUnchecked(
        1.5, -2.25, 3.75,
        2.5, 0.0, 0.0,
        0.0, 3.5, 0.0,
        0.0, 0.0, 4.5)

// One representative value per [<DataContract>] type. This table drives the
// compatibility and round-trip tests; the reflection test below ensures it
// stays in sync when another attributed type is added to the Euclid assembly.
let private contractValues : (string * obj) list = [
    "Pt", box (Pt(1.5, -2.25))
    "Vc", box (Vc(1.5, -2.25))
    "UnitVc", box (UnitVc.createUnchecked(1.0, 0.0))
    "Vec", box (Vec(1.5, -2.25, 3.75))
    "UnitVec", box (UnitVec.createUnchecked(1.0, 0.0, 0.0))
    "Pnt", box pnt
    "Rotation2D", box (Rotation2D.createUnchecked(0.0, 1.0))
    "Quaternion", box (Quaternion.createUnchecked(0.0, 0.0, 0.0, 1.0))
    "PPlane", box (PPlane.createUnchecked(
        1.5, -2.25, 3.75,
        1.0, 0.0, 0.0,
        0.0, 1.0, 0.0,
        0.0, 0.0, 1.0))
    "Matrix", box matrix
    "RigidMatrix", box (RigidMatrix.createTranslation(1.5, -2.25, 3.75))
    "Line2D", box (Line2D(1.5, -2.25, 3.75, 4.5))
    "Line3D", box (Line3D(1.5, -2.25, 3.75, 4.5, 5.5, 6.5))
    "NPlane", box (NPlane.createUnchecked(1.5, -2.25, 3.75, 0.0, 0.0, 1.0))
    "Rect2D", box (Rect2D.createUnchecked(1.5, -2.25, 2.5, 0.0, 0.0, 3.5))
    "Rect3D", box (Rect3D.createUnchecked(1.5, -2.25, 3.75, 2.5, 0.0, 0.0, 0.0, 3.5, 0.0))
    "Box", box boxValue
    "FreeBox", box (FreeBox.createFromBox boxValue)
    "BRect", box (BRect.createUnchecked(-2.25, -1.5, 3.75, 4.5))
    "BBox", box (BBox.createUnchecked(-3.75, -2.25, -1.5, 4.5, 5.5, 6.5))
    "Polyline2D", box (Polyline2D.createFromPts [Pt(1.5, -2.25); Pt(3.75, 4.5)])
    "Polyline3D", box (Polyline3D.createFromPts [Pnt(1.5, -2.25, 3.75); Pnt(4.5, 5.5, 6.5)])
]

let private roundTripTests =
    contractValues
    |> List.map (fun (_, value) ->
        let contractType = value.GetType()
        testCase contractType.Name <| fun _ ->
            let expectedJson = JsonSerializer.Serialize(value, contractType)
            let roundTripped = JsonSerializer.Deserialize(expectedJson, contractType)
            let actualJson = JsonSerializer.Serialize(roundTripped, contractType)
            expectJsonEqual contractType.Name expectedJson actualJson)

let tests =
    testList "JSON serialization" [

        testList "Pnt compatibility" [

            testCase "System.Text.Json serializes with default configuration" <| fun _ ->
                let actual = JsonSerializer.Serialize(pnt)
                Expect.equal actual json "serialized JSON"

            testCase "System.Text.Json deserializes with default configuration" <| fun _ ->
                let actual = JsonSerializer.Deserialize<Pnt>(json)
                expectPnt pnt actual

            testCase "Newtonsoft.Json serializes with default configuration" <| fun _ ->
                let actual = Newtonsoft.Json.JsonConvert.SerializeObject(pnt)
                Expect.equal actual json "serialized JSON"

            testCase "Newtonsoft.Json deserializes with default configuration" <| fun _ ->
                let actual = Newtonsoft.Json.JsonConvert.DeserializeObject<Pnt>(json)
                expectPnt pnt actual
        ]

        testCase "every DataContract type has a System.Text.Json converter and fixture" <| fun _ ->
            // [<DataContract>] is the source of truth: discover all contract
            // types at runtime instead of maintaining a second list of types.
            let dataContractTypes =
                typeof<Pnt>.Assembly.GetTypes()
                |> Array.filter (fun contractType ->
                    contractType.GetCustomAttributes(typeof<DataContractAttribute>, false).Length > 0)

            // Comparing full type names catches both a missing fixture and a
            // stale fixture. The count check also prevents duplicate fixtures
            // from being hidden when the list is converted to a set.
            let actualTypes = dataContractTypes |> Array.map (fun contractType -> contractType.FullName) |> Set.ofArray
            let fixtureTypes = contractValues |> List.map (fun (_, value) -> value.GetType().FullName)
            let expectedTypes = fixtureTypes |> Set.ofList
            Expect.equal expectedTypes.Count fixtureTypes.Length "one unique fixture per DataContract type"
            Expect.equal actualTypes expectedTypes "DataContract fixtures"

            // Every discovered contract must opt into its System.Text.Json
            // converter through exactly one [<JsonConverter>] attribute.
            for contractType in dataContractTypes do
                let converters = contractType.GetCustomAttributes(typeof<JsonConverterAttribute>, false)
                Expect.equal converters.Length 1 $"{contractType.FullName}: JsonConverter attribute"

        testCase "System.Text.Json matches every Newtonsoft.Json DataContract shape" <| fun _ ->
            for name, value in contractValues do
                let newtonsoftJson = Newtonsoft.Json.JsonConvert.SerializeObject(value)
                let systemTextJson = JsonSerializer.Serialize(value, value.GetType())
                expectJsonEqual name newtonsoftJson systemTextJson

        testList "System.Text.Json round-trips every DataContract type" roundTripTests

        testCase "System.Text.Json rejects invalid geometry during deserialization" <| fun _ ->
            let invalidValues : (string * Type * string) list = [
                "UnitVc", typeof<UnitVc>, """{"X":0,"Y":0}"""
                "UnitVec", typeof<UnitVec>, """{"X":0,"Y":0,"Z":0}"""
                "Rotation2D", typeof<Rotation2D>, """{"Sin":0,"Cos":0}"""
                "Quaternion", typeof<Quaternion>, """{"X":0,"Y":0,"Z":0,"W":0}"""
                "PPlane", typeof<PPlane>,
                    """{"OriginX":0,"OriginY":0,"OriginZ":0,"XaxisX":1,"XaxisY":0,"XaxisZ":0,"YaxisX":1,"YaxisY":0,"YaxisZ":0,"ZaxisX":0,"ZaxisY":0,"ZaxisZ":1}"""
                "RigidMatrix", typeof<RigidMatrix>,
                    """{"M11":2,"M21":0,"M31":0,"X41":0,"M12":0,"M22":1,"M32":0,"Y42":0,"M13":0,"M23":0,"M33":1,"Z43":0}"""
                "NPlane", typeof<NPlane>,
                    """{"OriginX":0,"OriginY":0,"OriginZ":0,"NormalX":0,"NormalY":0,"NormalZ":0}"""
                "Rect2D", typeof<Rect2D>,
                    """{"OriginX":0,"OriginY":0,"XaxisX":1,"XaxisY":0,"YaxisX":1,"YaxisY":0}"""
                "Rect3D", typeof<Rect3D>,
                    """{"OriginX":0,"OriginY":0,"OriginZ":0,"XaxisX":1,"XaxisY":0,"XaxisZ":0,"YaxisX":1,"YaxisY":0,"YaxisZ":0}"""
                "Box", typeof<Box>,
                    """{"OriginX":0,"OriginY":0,"OriginZ":0,"XaxisX":1,"XaxisY":0,"XaxisZ":0,"YaxisX":1,"YaxisY":0,"YaxisZ":0,"ZaxisX":0,"ZaxisY":0,"ZaxisZ":1}"""
            ]

            for name, contractType, invalidJson in invalidValues do
                Expect.throws
                    (fun () -> JsonSerializer.Deserialize(invalidJson, contractType) |> ignore)
                    $"{name}: invalid JSON should be rejected"
    ]
#endif
