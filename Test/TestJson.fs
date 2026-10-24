module TestJson

open Euclid

#if FABLE_COMPILER
open EuclidTestSupport

let tests =
    testList "Pnt JSON serialization" []
#else
open System
open System.Buffers
open System.Runtime.Serialization
open System.Text.Json
open System.Text.Json.Serialization
open EuclidTestSupport

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
    "RigidMatrix", box (RigidMatrix.createTranslationXYZ 1.5 (-2.25) 3.75)
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
    "Polyline3D", box (Polyline3D.createFromPnts [Pnt(1.5, -2.25, 3.75); Pnt(4.5, 5.5, 6.5)])
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

// Reading them allocates the coordinate buffer of the result.
let private collectionTypes = set [ "FreeBox"; "Polyline2D"; "Polyline3D" ]

/// Returns the bytes allocated by 100 calls of the action, after one warm-up call.
let private allocatedBytes (action:unit -> unit) =
    action ()
    let before = GC.GetAllocatedBytesForCurrentThread()
    for _ in 1 .. 100 do
        action ()
    GC.GetAllocatedBytesForCurrentThread() - before

type private ConverterAllocations =

    /// Returns the bytes allocated by 100 writes and by 100 reads with the converter of 'T.
    static member Measure<'T>(value:'T) : int64 * int64 =
        let options = JsonSerializerOptions()
        let converter = options.GetConverter(typeof<'T>) :?> JsonConverter<'T>
        let json = JsonSerializer.SerializeToUtf8Bytes(value, options)
        let buffer = ArrayBufferWriter<byte>(1024)
        use writer = new Utf8JsonWriter(buffer)
        let write () =
            buffer.Clear()
            writer.Reset()
            converter.Write(writer, value, options)
            writer.Flush()
        let read () =
            let mutable reader = Utf8JsonReader(ReadOnlySpan<byte>(json))
            reader.Read() |> ignore
            converter.Read(&reader, typeof<'T>, options) |> ignore
        allocatedBytes write, allocatedBytes read

/// Returns the bytes allocated by 100 writes and by 100 reads with the converter of the value's type.
let private converterAllocations (value:obj) : int64 * int64 =
    let flags = Reflection.BindingFlags.Static ||| Reflection.BindingFlags.NonPublic // a private F# type has internal members
    let measure = typeof<ConverterAllocations>.GetMethod("Measure", flags).MakeGenericMethod(value.GetType())
    measure.Invoke(null, [| value |]) :?> int64 * int64

let private fixture name =
    contractValues |> List.find (fun (fixtureName, _) -> fixtureName = name) |> snd

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

        testCase "System.Text.Json round-trips every DataContract type with a camelCase naming policy" <| fun _ ->
            let options = JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase)
            Expect.equal (JsonSerializer.Serialize(pnt, options)) """{"x":1.5,"y":-2.25,"z":3.75}""" "camelCase JSON"
            for name, value in contractValues do
                let contractType = value.GetType()
                let expectedJson = JsonSerializer.Serialize(value, contractType, options)
                let roundTripped = JsonSerializer.Deserialize(expectedJson, contractType, options)
                let actualJson = JsonSerializer.Serialize(roundTripped, contractType, options)
                expectJsonEqual name expectedJson actualJson

        testCase "System.Text.Json reads properties only in the order they are written" <| fun _ ->
            let rejected = [
                """{"Y":-2.25,"X":1.5,"Z":3.75}""", "swapped properties"
                """{"X":1.5,"Y":-2.25}""", "a missing property"
                """{"Extra":0,"X":1.5,"Y":-2.25,"Z":3.75}""", "an extra property at the start"
                """{"X":1.5,"Y":-2.25,"Z":3.75,"Extra":0}""", "an extra property at the end"
                """{"X":1.5,"Y":"-2.25","Z":3.75}""", "a string instead of a number"
                """[1.5,-2.25,3.75]""", "an array instead of an object"
            ]
            for rejectedJson, description in rejected do
                Expect.throws (fun () -> JsonSerializer.Deserialize<Pnt>(rejectedJson) |> ignore) $"Pnt with {description}"

        testCase "System.Text.Json reads property names case-insensitively only when configured" <| fun _ ->
            let lowerCase = """{"x":1.5,"y":-2.25,"z":3.75}"""
            let options = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
            expectPnt pnt (JsonSerializer.Deserialize<Pnt>(lowerCase, options))
            Expect.throws (fun () -> JsonSerializer.Deserialize<Pnt>(lowerCase) |> ignore) "lower-case names without PropertyNameCaseInsensitive"

        testCase "System.Text.Json float property converters write and read without allocating" <| fun _ ->
            for name, value in contractValues do
                if not (collectionTypes.Contains name) then
                    let writeBytes, readBytes = converterAllocations value
                    Expect.equal writeBytes 0L $"{name}: bytes allocated by 100 writes"
                    Expect.equal readBytes 0L $"{name}: bytes allocated by 100 reads"

        testCase "System.Text.Json collection converters write without allocating and read only their result" <| fun _ ->
            let freeBox = fixture "FreeBox" :?> FreeBox
            let polyline2D = fixture "Polyline2D" :?> Polyline2D
            let polyline3D = fixture "Polyline3D" :?> Polyline3D
            // Each function creates an object like the one a read returns, with a coordinate buffer of the same size.
            let results : (string * obj * (unit -> unit)) list = [
                "FreeBox", box freeBox, fun () -> FreeBox.createFromXYZs (Array.zeroCreate freeBox.XYZs.Length) |> ignore
                "Polyline2D", box polyline2D, fun () -> Polyline2D.createFromXYs (ResizeArray<float>(polyline2D.XYs.Count)) |> ignore
                "Polyline3D", box polyline3D, fun () -> Polyline3D.createFromXYZs (ResizeArray<float>(polyline3D.XYZs.Count)) |> ignore
            ]
            Expect.equal (results |> List.map (fun (name, _, _) -> name) |> Set.ofList) collectionTypes "collection types"
            for name, value, createResult in results do
                let writeBytes, readBytes = converterAllocations value
                Expect.equal writeBytes 0L $"{name}: bytes allocated by 100 writes"
                Expect.equal readBytes (allocatedBytes createResult) $"{name}: bytes allocated by 100 reads, the same as by creating the result 100 times"

        testCase "System.Text.Json reads coordinate arrays into buffers of exactly their length" <| fun _ ->
            // 6 floats, since a growing ResizeArray would end with a capacity of 8, but one of 4 floats with a capacity of 4.
            let polyline = JsonSerializer.Deserialize<Polyline2D>("""{"XYs":[1.5,-2.25,3.75,4.5,5.5,6.5]}""")
            Expect.equal (List.ofSeq polyline.XYs) [1.5; -2.25; 3.75; 4.5; 5.5; 6.5] "coordinates"
            Expect.equal polyline.XYs.Capacity 6 "capacity"

            let empty = JsonSerializer.Deserialize<Polyline3D>("""{"XYZs":[]}""")
            Expect.equal empty.PointCount 0 "point count of an empty array"

            let options = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
            let lowerCase = JsonSerializer.Deserialize<Polyline2D>("""{"xys":[1.5,-2.25,3.75,4.5]}""", options)
            Expect.equal (List.ofSeq lowerCase.XYs) [1.5; -2.25; 3.75; 4.5] "coordinates under a lower-case name"

            Expect.throws (fun () -> JsonSerializer.Deserialize<Polyline2D>("""{"Extra":[1,2],"XYs":[1.5,-2.25]}""") |> ignore) "an extra property at the start"
            Expect.throws (fun () -> JsonSerializer.Deserialize<Polyline2D>("""{"XYs":[1.5,-2.25],"Extra":[1,2]}""") |> ignore) "an extra property at the end"
            Expect.throws (fun () -> JsonSerializer.Deserialize<Polyline2D>("""{"XYs":[1.5,"2"]}""") |> ignore) "a string in the array"
            Expect.throws (fun () -> JsonSerializer.Deserialize<Polyline2D>("""{"XYs":{}}""") |> ignore) "an object instead of an array"
            Expect.throws (fun () -> JsonSerializer.Deserialize<Polyline2D>("""{}""") |> ignore) "a missing array"
            Expect.throws (fun () -> JsonSerializer.Deserialize<FreeBox>("""{"XYZs":[1,2,3]}""") |> ignore) "a FreeBox with 3 instead of 24 floats"

        testCase "System.Text.Json writes the names of the DataMember fields and properties" <| fun _ ->
            for name, value in contractValues do
                let contractType = value.GetType()
                let memberNames =
                    contractType.GetMembers()
                    |> Array.filter (fun m -> m.GetCustomAttributes(typeof<DataMemberAttribute>, false).Length > 0)
                    |> Array.map (fun m -> m.Name)
                    |> Set.ofArray
                use document = JsonDocument.Parse(JsonSerializer.Serialize(value, contractType))
                let jsonNames = document.RootElement.EnumerateObject() |> Seq.map (fun p -> p.Name) |> Set.ofSeq
                Expect.equal jsonNames memberNames $"{name}: JSON property names"

        testCase "System.Text.Json rejects the axis property names of earlier versions" <| fun _ ->
            let legacyValues : (string * Type * string) list = [
                "PPlane", typeof<PPlane>,
                    """{"OriginX":0,"OriginY":0,"OriginZ":0,"XaxisX":1,"XaxisY":0,"XaxisZ":0,"YaxisX":0,"YaxisY":1,"YaxisZ":0,"ZaxisX":0,"ZaxisY":0,"ZaxisZ":1}"""
                "Rect2D", typeof<Rect2D>,
                    """{"OriginX":0,"OriginY":0,"XaxisX":1,"XaxisY":0,"YaxisX":0,"YaxisY":1}"""
                "Rect3D", typeof<Rect3D>,
                    """{"OriginX":0,"OriginY":0,"OriginZ":0,"XaxisX":1,"XaxisY":0,"XaxisZ":0,"YaxisX":0,"YaxisY":1,"YaxisZ":0}"""
                "Box", typeof<Box>,
                    """{"OriginX":0,"OriginY":0,"OriginZ":0,"XaxisX":1,"XaxisY":0,"XaxisZ":0,"YaxisX":0,"YaxisY":1,"YaxisZ":0,"ZaxisX":0,"ZaxisY":0,"ZaxisZ":1}"""
            ]
            for name, contractType, legacyJson in legacyValues do
                Expect.throws
                    (fun () -> JsonSerializer.Deserialize(legacyJson, contractType) |> ignore)
                    $"{name}: JSON with Xaxis names should be rejected"
                let current = legacyJson.Replace("axis", "Axis")
                Expect.isTrue (not (isNull (JsonSerializer.Deserialize(current, contractType)))) $"{name}: the same JSON with XAxis names is read"

        testCase "System.Text.Json rejects invalid geometry during deserialization" <| fun _ ->
            let invalidValues : (string * Type * string) list = [
                "UnitVc", typeof<UnitVc>, """{"X":0,"Y":0}"""
                "UnitVec", typeof<UnitVec>, """{"X":0,"Y":0,"Z":0}"""
                "Rotation2D", typeof<Rotation2D>, """{"Sin":0,"Cos":0}"""
                "Quaternion", typeof<Quaternion>, """{"X":0,"Y":0,"Z":0,"W":0}"""
                "PPlane", typeof<PPlane>,
                    """{"OriginX":0,"OriginY":0,"OriginZ":0,"XAxisX":1,"XAxisY":0,"XAxisZ":0,"YAxisX":1,"YAxisY":0,"YAxisZ":0,"ZAxisX":0,"ZAxisY":0,"ZAxisZ":1}"""
                "RigidMatrix", typeof<RigidMatrix>,
                    """{"M11":2,"M21":0,"M31":0,"X41":0,"M12":0,"M22":1,"M32":0,"Y42":0,"M13":0,"M23":0,"M33":1,"Z43":0}"""
                "NPlane", typeof<NPlane>,
                    """{"OriginX":0,"OriginY":0,"OriginZ":0,"NormalX":0,"NormalY":0,"NormalZ":0}"""
                "Rect2D", typeof<Rect2D>,
                    """{"OriginX":0,"OriginY":0,"XAxisX":1,"XAxisY":0,"YAxisX":1,"YAxisY":0}"""
                "Rect3D", typeof<Rect3D>,
                    """{"OriginX":0,"OriginY":0,"OriginZ":0,"XAxisX":1,"XAxisY":0,"XAxisZ":0,"YAxisX":1,"YAxisY":0,"YAxisZ":0}"""
                "Box", typeof<Box>,
                    """{"OriginX":0,"OriginY":0,"OriginZ":0,"XAxisX":1,"XAxisY":0,"XAxisZ":0,"YAxisX":1,"YAxisY":0,"YAxisZ":0,"ZAxisX":0,"ZAxisY":0,"ZAxisZ":1}"""
            ]

            for name, contractType, invalidJson in invalidValues do
                Expect.throws
                    (fun () -> JsonSerializer.Deserialize(invalidJson, contractType) |> ignore)
                    $"{name}: invalid JSON should be rejected"
    ]
#endif
