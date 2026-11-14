module TestOffset3D

open System
open System.IO
open System.Text
open Euclid

open EuclidTestSupport

let inline eqPnt a b = Pnt.distanceTo a b < 1e-6

let tol = Accuracy.veryHigh

/// A rotation about an oblique axis, so that test shapes are not axis aligned.
let tilt = Quaternion.createFromDegreesUnitVec (UnitVec.create 1.0 2.0 3.0) 37.0

/// Rotates a point with tilt and moves it away from the origin.
let toWorld (p:Pnt) = Pnt.rotate tilt p + Vec(120.0, -40.0, 15.0)

/// The world Z axis rotated with tilt, the normal of shapes made in the XY plane.
let worldZ = UnitVec.rotate tilt UnitVec.zAxis

/// Creates a polyline from points in the XY plane, rotated and moved with toWorld.
let tilted (pts: Pnt list) = pts |> List.map toWorld |> Polyline3D.createFromPnts

/// The largest deviation of the offset points from the distance |d| to the infinite lines of their adjacent input segments.
/// Meaningful for planar polylines offset in their plane, and for any polyline where every point is a corner,
/// because a corner is offset in the local plane of its two segments.
let maxSegmentDistanceError (input:Polyline3D) (output:Polyline3D) (d:float) =
    let pts = input.AsPoints
    let res = output.AsPoints
    let mutable err = 0.0
    for i = 0 to pts.Count - 2 do
        let ln = Line3D(pts.[i], pts.[i + 1])
        err <- max err (abs (Line3D.distanceRayPoint res.[i] ln - abs d))
        err <- max err (abs (Line3D.distanceRayPoint res.[i + 1] ln - abs d))
    err

// ============ Test data recording ============

/// Set to true to save input/output pairs to an F# file during test runs
let saveTestData = false

/// Path for the generated F# file with test data
let testDataFilePath =
    #if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
    ".NET only" //in Fable writeTestDataFile() does nothing
    #else
    Path.Combine(__SOURCE_DIRECTORY__, "GeneratedOffset3DTestData.fsx")
    #endif

/// Collects test case data during test runs
let testDataCollector = ResizeArray<string>()

/// Records a test case with input polyline, parameters, and output polyline
let recordTestCase (testName: string) (input: Polyline3D) (inPlane: float) (perp: float) (refNormal: UnitVec) (output: Polyline3D) =
    if saveTestData then
        let sb = StringBuilder()
        sb.AppendLine($"    {{ Name = \"{testName}\"") |> ignore
        sb.AppendLine($"      Input = {input.AsFSharpCode}") |> ignore
        sb.AppendLine($"      RefNormal = UnitVec.create({refNormal.X}, {refNormal.Y}, {refNormal.Z})") |> ignore
        sb.AppendLine($"      InPlane = {inPlane}") |> ignore
        sb.AppendLine($"      Perp = {perp}") |> ignore
        sb.AppendLine($"      Output = {output.AsFSharpCode} }}") |> ignore
        lock testDataCollector (fun () -> testDataCollector.Add(sb.ToString()))

/// Writes all collected test data to the F# file
let writeTestDataFile () =
    #if !FABLE_COMPILER
    if saveTestData && testDataCollector.Count > 0 then
        let sb = StringBuilder()
        sb.AppendLine("// Auto-generated test data from Offset3D tests") |> ignore
        sb.AppendLine($"// Generated: {DateTime.Now}") |> ignore
        sb.AppendLine() |> ignore
        sb.AppendLine("#r \"nuget: Euclid\"") |> ignore
        sb.AppendLine("open Euclid") |> ignore
        sb.AppendLine() |> ignore
        sb.AppendLine("/// Record type for Offset3D test case data") |> ignore
        sb.AppendLine("type Offset3DTestCase = {") |> ignore
        sb.AppendLine("    Name: string") |> ignore
        sb.AppendLine("    Input: Polyline3D") |> ignore
        sb.AppendLine("    RefNormal: UnitVec") |> ignore
        sb.AppendLine("    InPlane: float") |> ignore
        sb.AppendLine("    Perp: float") |> ignore
        sb.AppendLine("    Output: Polyline3D") |> ignore
        sb.AppendLine("}") |> ignore
        sb.AppendLine() |> ignore
        sb.AppendLine("/// All Offset3D test cases for visualization") |> ignore
        sb.AppendLine("let offset3DTestCases = [|") |> ignore
        for i = 0 to testDataCollector.Count - 1 do
            sb.Append(testDataCollector.[i]) |> ignore
            if i < testDataCollector.Count - 1 then
                sb.AppendLine() |> ignore
        sb.AppendLine("|]") |> ignore
        sb.AppendLine() |> ignore
        sb.AppendLine($"// Total test cases: {testDataCollector.Count}") |> ignore
        sb.AppendLine() |> ignore
        sb.AppendLine("// Example: iterate and visualize all test cases") |> ignore
        sb.AppendLine("// for tc in offset3DTestCases do") |> ignore
        sb.AppendLine("//     printfn \"Test: %s\" tc.Name") |> ignore
        sb.AppendLine("//     // Draw tc.Input and tc.Output with your visualization tool") |> ignore
        File.WriteAllText(testDataFilePath, sb.ToString())
        printfn $"Test data written to: {testDataFilePath}"
    #endif
    () // do nothing in Fable

let tests =
    testList "Offset3D " [
        test "duplicate point failure message lists the polyline" {
            let failMessage (coords: float list) =
                try Offset3D.getSegmentUnitVectors (ResizeArray coords) |> ignore; "" with e -> e.Message
            let openMsg = failMessage [13.;27.;39.; 13.;27.;39.; 14.;29.;42.]
            "location" |> Expect.stringContains openMsg "point[1] and point[0] are the same at (13, 27, 39)"
            "open" |> Expect.stringContains openMsg "Open polyline with 3 points"
            "points listed" |> Expect.stringContains openMsg "14  29  42"
            let closedMsg = failMessage [13.;27.;39.; 14.;29.;42.; 14.;29.;42.; 13.;27.;39.]
            "closed" |> Expect.stringContains closedMsg "Closed polyline with 4 points"
            let almostMsg = failMessage [13.;27.;39.; 14.;29.;42.; 14.;29.;42.; 13.;27.;39.0000001]
            "Z participates in distance" |> Expect.stringContains almostMsg "Almost closed polyline"
            let longMsg = failMessage [ yield! [0.;0.;0.; 0.;0.;0.]; for i = 6 to 110 do float i ]
            "long buffer summary" |> Expect.stringContains longMsg "Last point: (108, 109, 110)"
        }

        testCase "Polyline3D.offset open L-shape in XY plane with Z-up normal" <| fun _ ->
            // L-shape: (0,0,0) -> (10,0,0) -> (10,10,0)
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let inPlane, perp = 1.0, 0.0
            let result = Polyline3D.offsetByNormal refNormal inPlane perp pl
            recordTestCase "open L-shape in XY plane" pl inPlane perp refNormal result
            // With inPlane offset of 1.0 and Z-up normal, offset should go to the "inside" of the L
            // Start point: offset perpendicular to first segment (along +Y)
            // Corner point: bisector offset
            // End point: offset perpendicular to last segment (along -X)
            Expect.equal result.PointCount 3 "Should have 3 points"
            Expect.isTrue (eqPnt result.AsPoints.[0] (Pnt(0, 1, 0))) $"Start point should be Pnt(0, 1, 0), got {result.AsPoints.[0]}"
            Expect.isTrue (eqPnt result.AsPoints.[1] (Pnt(9, 1, 0))) $"Corner should be Pnt(9, 1, 0), got {result.AsPoints.[1]}"
            Expect.isTrue (eqPnt result.AsPoints.[2] (Pnt(9, 10, 0))) $"End point should be Pnt(9, 10, 0), got {result.AsPoints.[2]}"

        testCase "Polyline3D.offset open L-shape with perpendicular offset" <| fun _ ->
            // L-shape: (0,0,0) -> (10,0,0) -> (10,10,0)
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let inPlane, perp = 0.0, 2.0
            let result = Polyline3D.offsetByNormal refNormal inPlane perp pl
            recordTestCase "open L-shape with perpendicular offset" pl inPlane perp refNormal result
            // With only perpendicular offset of 2.0 and Z-up normal, all points should move up in Z
            Expect.equal result.PointCount 3 "Should have 3 points"
            Expect.isTrue (eqPnt result.AsPoints.[0] (Pnt(0, 0, 2))) $"Start point should be at Z=2, got {result.AsPoints.[0]}"
            Expect.isTrue (eqPnt result.AsPoints.[1] (Pnt(10, 0, 2))) $"Corner should be at Z=2, got {result.AsPoints.[1]}"
            Expect.isTrue (eqPnt result.AsPoints.[2] (Pnt(10, 10, 2))) $"End point should be at Z=2, got {result.AsPoints.[2]}"

        testCase "Polyline3D.offset open straight line (collinear)" <| fun _ ->
            // Straight line: all points collinear
            let pts = ResizeArray [Pnt(0,0,0); Pnt(5,0,0); Pnt(10,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let inPlane, perp = 1.0, 0.0
            let result = Polyline3D.offsetByNormal refNormal inPlane perp pl
            recordTestCase "open straight line (collinear)" pl inPlane perp refNormal result
            // Collinear points should all be offset in the same direction (perpendicular to line, in plane with refNormal)
            Expect.equal result.PointCount 3 "Should have 3 points"
            Expect.isTrue (eqPnt result.AsPoints.[0] (Pnt(0, 1, 0))) $"Start should be at Y=1, got {result.AsPoints.[0]}"
            Expect.isTrue (eqPnt result.AsPoints.[1] (Pnt(5, 1, 0))) $"Middle should be at Y=1, got {result.AsPoints.[1]}"
            Expect.isTrue (eqPnt result.AsPoints.[2] (Pnt(10, 1, 0))) $"End should be at Y=1, got {result.AsPoints.[2]}"

        testCase "Polyline3D.offset closed triangle" <| fun _ ->
            // Equilateral-ish triangle in XY plane, closed (first = last)
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(5,8.66,0); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let inPlane, perp = 1.0, 0.0
            let result = Polyline3D.offsetByNormal refNormal inPlane perp pl
            recordTestCase "closed triangle" pl inPlane perp refNormal result
            // Closed polygon should remain closed after offset
            Expect.equal result.PointCount 4 "Should have 4 points (closed)"
            Expect.isTrue (eqPnt result.AsPoints.[0] result.AsPoints.[3]) $"Should remain closed, start={result.AsPoints.[0]}, end={result.AsPoints.[3]}"
            Expect.isTrue (maxSegmentDistanceError pl result inPlane < 1e-9) "every point is at the offset distance from its adjacent segments"

        testCase "Polyline3D.offset closed square inward" <| fun _ ->
            // Square in XY plane, CCW when viewed from +Z
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let inPlane, perp = 1.0, 0.0
            let result = Polyline3D.offsetByNormal refNormal inPlane perp pl
            recordTestCase "closed square inward" pl inPlane perp refNormal result
            // Inward offset of 1.0 should shrink the square
            Expect.equal result.PointCount 5 "Should have 5 points (closed)"
            Expect.isTrue (eqPnt result.AsPoints.[0] (Pnt(1, 1, 0))) $"Corner 0 should be at (1,1,0), got {result.AsPoints.[0]}"
            Expect.isTrue (eqPnt result.AsPoints.[1] (Pnt(9, 1, 0))) $"Corner 1 should be at (9,1,0), got {result.AsPoints.[1]}"
            Expect.isTrue (eqPnt result.AsPoints.[2] (Pnt(9, 9, 0))) $"Corner 2 should be at (9,9,0), got {result.AsPoints.[2]}"
            Expect.isTrue (eqPnt result.AsPoints.[3] (Pnt(1, 9, 0))) $"Corner 3 should be at (1,9,0), got {result.AsPoints.[3]}"

        testCase "Polyline3D.offset closed square outward" <| fun _ ->
            // Square in XY plane, CCW when viewed from +Z
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let inPlane, perp = -1.0, 0.0
            let result = Polyline3D.offsetByNormal refNormal inPlane perp pl
            recordTestCase "closed square outward" pl inPlane perp refNormal result
            // Outward offset of -1.0 should expand the square
            Expect.equal result.PointCount 5 "Should have 5 points (closed)"
            Expect.isTrue (eqPnt result.AsPoints.[0] (Pnt(-1, -1, 0))) $"Corner 0 should be at (-1,-1,0), got {result.AsPoints.[0]}"
            Expect.isTrue (eqPnt result.AsPoints.[1] (Pnt(11, -1, 0))) $"Corner 1 should be at (11,-1,0), got {result.AsPoints.[1]}"
            Expect.isTrue (eqPnt result.AsPoints.[2] (Pnt(11, 11, 0))) $"Corner 2 should be at (11,11,0), got {result.AsPoints.[2]}"
            Expect.isTrue (eqPnt result.AsPoints.[3] (Pnt(-1, 11, 0))) $"Corner 3 should be at (-1,11,0), got {result.AsPoints.[3]}"

        testCase "Polyline3D.offset with loop=true on open polyline" <| fun _ ->
            // Open L-shape that should be treated as closed loop
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let result = Polyline3D.offsetByNormalCustom(pl, 1.0, 0.0, refNormal, loop=true)
            recordTestCase "open L-shape with loop=true" pl 1.0 0.0 refNormal result
            // With loop=true, the polyline is treated as if first and last are connected
            Expect.equal result.PointCount 3 "Should still have 3 points (loop closes and opens)"
            Expect.isTrue (maxSegmentDistanceError pl result 1.0 < 1e-9) "every point is at the offset distance from its adjacent segments"
            let closing = Line3D(pts.[2], pts.[0])
            Expect.floatClose tol (Line3D.distanceRayPoint result.AsPoints.[0] closing) 1.0 "the start point is offset from the closing segment too"

        testCase "Polyline3D.offset 3D polyline not in XY plane" <| fun _ ->
            // L-shape in XZ plane
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,0,10)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.yAxis
            let result = Polyline3D.offsetByNormal refNormal 1.0 0.0 pl
            recordTestCase "L-shape in XZ plane" pl 1.0 0.0 refNormal result
            // With Y-up reference normal for XZ plane L-shape
            Expect.equal result.PointCount 3 "Should have 3 points"
            // seen from +Y the turn from +X to +Z is clockwise, so a positive distance offsets to the outside
            Expect.isTrue (eqPnt result.AsPoints.[1] (Pnt(11, 0, -1))) $"Corner should be Pnt(11, 0, -1), got {result.AsPoints.[1]}"

        testCase "Polyline3D.offset combined in-plane and perpendicular" <| fun _ ->
            // Simple 2-point line along X axis
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let result = Polyline3D.offsetByNormal refNormal 2.0 3.0 pl
            recordTestCase "combined in-plane and perpendicular" pl 2.0 3.0 refNormal result
            // In-plane offset of 2.0 should move in +Y, perpendicular offset of 3.0 should move in +Z
            Expect.equal result.PointCount 2 "Should have 2 points"
            Expect.isTrue (eqPnt result.AsPoints.[0] (Pnt(0, 2, 3))) $"Start should be at (0,2,3), got {result.AsPoints.[0]}"
            Expect.isTrue (eqPnt result.AsPoints.[1] (Pnt(10, 2, 3))) $"End should be at (10,2,3), got {result.AsPoints.[1]}"

        testCase "Polyline3D.offset zero distances returns original shape" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let result = Polyline3D.offsetByNormal refNormal 0.0 0.0 pl
            recordTestCase "zero distances returns original" pl 0.0 0.0 refNormal result
            Expect.equal result.PointCount 3 "Should have 3 points"
            Expect.isTrue (eqPnt result.AsPoints.[0] pts.[0]) "Point 0 should be unchanged"
            Expect.isTrue (eqPnt result.AsPoints.[1] pts.[1]) "Point 1 should be unchanged"
            Expect.isTrue (eqPnt result.AsPoints.[2] pts.[2]) "Point 2 should be unchanged"

        testCase "Polyline3D.offset minimum 2 points" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let result = Polyline3D.offsetByNormal refNormal 1.0 0.0 pl
            recordTestCase "minimum 2 points" pl 1.0 0.0 refNormal result
            Expect.equal result.PointCount 2 "Should have 2 points"

        testCase "Polyline3D.offset fails with less than 2 points" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            Expect.throws (fun () -> Polyline3D.offsetByNormal refNormal 1.0 0.0 pl |> ignore) "Should fail with less than 2 points"

        testCase "Polyline3D.offset negative perpendicular offset" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let result = Polyline3D.offsetByNormal refNormal 0.0 (-2.0) pl
            recordTestCase "negative perpendicular offset" pl 0.0 -2.0 refNormal result
            // Negative perpendicular offset should go opposite to refNormal (down in Z)
            Expect.isTrue (eqPnt result.AsPoints.[0] (Pnt(0, 0, -2))) $"Start should be at Z=-2, got {result.AsPoints.[0]}"
            Expect.isTrue (eqPnt result.AsPoints.[1] (Pnt(10, 0, -2))) $"End should be at Z=-2, got {result.AsPoints.[1]}"

        // ============ Roundtrip tests ============

        testCase "Polyline3D.offset positive then negative returns to original (closed square)" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let d = 1.0
            let offset1 = Polyline3D.offsetByNormal refNormal d 0.0 pl
            let offset2 = Polyline3D.offsetByNormal refNormal (-d) 0.0 offset1
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.AsPoints.[i] pts.[i]) $"pt[{i}] returns to original after +d then -d"

        testCase "Polyline3D.offset negative then positive returns to original (closed square)" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let d = 1.5
            let offset1 = Polyline3D.offsetByNormal refNormal (-d) 0.0 pl
            let offset2 = Polyline3D.offsetByNormal refNormal d 0.0 offset1
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.AsPoints.[i] pts.[i]) $"pt[{i}] returns to original after -d then +d"

        testCase "Polyline3D.offset positive then negative returns to original (open L-shape)" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let d = 1.0
            let offset1 = Polyline3D.offsetByNormal refNormal d 0.0 pl
            let offset2 = Polyline3D.offsetByNormal refNormal (-d) 0.0 offset1
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.AsPoints.[i] pts.[i]) $"pt[{i}] returns to original after +d then -d (open L-shape)"

        testCase "Polyline3D.offset positive then negative returns to original (triangle)" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(5,8.66,0); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let d = 0.5
            let offset1 = Polyline3D.offsetByNormal refNormal d 0.0 pl
            let offset2 = Polyline3D.offsetByNormal refNormal (-d) 0.0 offset1
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (triangle)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.AsPoints.[i] pts.[i]) $"pt[{i}] returns to original (triangle)"

        testCase "Polyline3D.offset roundtrip with perpendicular offset" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let d = 2.0
            let offset1 = Polyline3D.offsetByNormal refNormal 0.0 d pl
            let offset2 = Polyline3D.offsetByNormal refNormal 0.0 (-d) offset1
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.AsPoints.[i] pts.[i]) $"pt[{i}] returns to original after perpendicular roundtrip"

        testCase "Polyline3D.offset roundtrip with combined offsets" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let dIn = 1.0
            let dPerp = 2.0
            let offset1 = Polyline3D.offsetByNormal refNormal dIn dPerp pl
            let offset2 = Polyline3D.offsetByNormal refNormal (-dIn) (-dPerp) offset1
            Expect.equal offset2.PointCount pts.Count "same count after combined roundtrip"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.AsPoints.[i] pts.[i]) $"pt[{i}] returns to original after combined roundtrip"

        // ============ Different shapes ============

        testCase "Polyline3D.offset pentagon inward" <| fun _ ->
            // Regular pentagon in XY plane
            let r = 10.0
            let pts = ResizeArray [
                for i = 0 to 4 do
                    let angle = float i * 2.0 * Math.PI / 5.0 - Math.PI / 2.0
                    Pnt(r * cos angle, r * sin angle, 0.0)
            ]
            pts.Add(pts.[0]) // close
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let d = 1.0
            let offset1 = Polyline3D.offsetByNormal refNormal d 0.0 pl
            let offset2 = Polyline3D.offsetByNormal refNormal (-d) 0.0 offset1
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (pentagon)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.AsPoints.[i] pts.[i]) $"pt[{i}] returns to original (pentagon)"

        testCase "Polyline3D.offset hexagon roundtrip" <| fun _ ->
            // Regular hexagon in XY plane
            let r = 10.0
            let pts = ResizeArray [
                for i = 0 to 5 do
                    let angle = float i * Math.PI / 3.0
                    Pnt(r * cos angle, r * sin angle, 0.0)
            ]
            pts.Add(pts.[0]) // close
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let inPlane, perp = 1.5, 0.0
            let offset1 = Polyline3D.offsetByNormal refNormal inPlane perp pl
            recordTestCase "hexagon" pl inPlane perp refNormal offset1
            let offset2 = Polyline3D.offsetByNormal refNormal (-inPlane) perp offset1
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (hexagon)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.AsPoints.[i] pts.[i]) $"pt[{i}] returns to original (hexagon)"

        // ============ Open polyline patterns ============

        testCase "Polyline3D.offset open zigzag roundtrip" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,10,0); Pnt(20,0,0); Pnt(30,10,0); Pnt(40,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let d = 1.0
            let offset1 = Polyline3D.offsetByNormal refNormal d 0.0 pl
            let offset2 = Polyline3D.offsetByNormal refNormal (-d) 0.0 offset1
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (zigzag)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.AsPoints.[i] pts.[i]) $"pt[{i}] returns to original (zigzag)"

        testCase "Polyline3D.offset open staircase pattern" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(20,10,0); Pnt(20,20,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let d = 1.0
            let offset1 = Polyline3D.offsetByNormal refNormal d 0.0 pl
            let offset2 = Polyline3D.offsetByNormal refNormal (-d) 0.0 offset1
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (staircase)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.AsPoints.[i] pts.[i]) $"pt[{i}] returns to original (staircase)"

        // ============ Collinear points ============

        testCase "Polyline3D.offset with collinear points in middle" <| fun _ ->
            // Square with an extra collinear point on one edge
            let pts = ResizeArray [Pnt(0,0,0); Pnt(5,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let d = 1.0
            let offset1 = Polyline3D.offsetByNormal refNormal d 0.0 pl
            recordTestCase "square with collinear point" pl d 0.0 refNormal offset1
            let offset2 = Polyline3D.offsetByNormal refNormal (-d) 0.0 offset1
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (collinear)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.AsPoints.[i] pts.[i]) $"pt[{i}] returns to original (collinear)"

        testCase "Polyline3D.offset open line with multiple collinear points" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(5,0,0); Pnt(10,0,0); Pnt(15,0,0); Pnt(20,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let result = Polyline3D.offsetByNormal refNormal 2.0 0.0 pl
            recordTestCase "open line with multiple collinear points" pl 2.0 0.0 refNormal result
            Expect.equal result.PointCount 5 "Should have 5 points"
            // All points should be offset by 2 in Y direction
            for i = 0 to result.PointCount - 1 do
                Expect.isTrue (eqPnt result.AsPoints.[i] (Pnt(pts.[i].X, 2.0, 0.0))) $"pt[{i}] should be at Y=2"

        // ============ Different planes ============

        testCase "Polyline3D.offset in XZ plane" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,0,10); Pnt(0,0,10); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.yAxis
            let inPlane, perp = 1.0, 0.0
            let offset1 = Polyline3D.offsetByNormal refNormal inPlane perp pl
            recordTestCase "XZ plane square" pl inPlane perp refNormal offset1
            let offset2 = Polyline3D.offsetByNormal refNormal (-inPlane) perp offset1
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (XZ plane)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.AsPoints.[i] pts.[i]) $"pt[{i}] returns to original (XZ plane)"

        testCase "Polyline3D.offset in YZ plane" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(0,10,0); Pnt(0,10,10); Pnt(0,0,10); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.xAxis
            let inPlane, perp = 1.0, 0.0
            let offset1 = Polyline3D.offsetByNormal refNormal inPlane perp pl
            recordTestCase "YZ plane square" pl inPlane perp refNormal offset1
            let offset2 = Polyline3D.offsetByNormal refNormal (-inPlane) perp offset1
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (YZ plane)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.AsPoints.[i] pts.[i]) $"pt[{i}] returns to original (YZ plane)"

        testCase "Polyline3D.offset tilted plane (45 deg)" <| fun _ ->
            // Triangle in a tilted plane
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,10); Pnt(5,8.66,5); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            // Normal perpendicular to the tilted plane
            let refNormal = UnitVec.create 1.0 0.0 (-1.0)
            let inPlane, perp = 0.5, 0.0
            let offset1 = Polyline3D.offsetByNormal refNormal inPlane perp pl
            recordTestCase "tilted plane triangle" pl inPlane perp refNormal offset1
            let offset2 = Polyline3D.offsetByNormal refNormal (-inPlane) perp offset1
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (tilted plane)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.AsPoints.[i] pts.[i]) $"pt[{i}] returns to original (tilted plane)"

        // ============ 3D helix-like polyline ============

        testCase "Polyline3D.offset 3D spiral staircase produces valid result" <| fun _ ->
            // A 3D spiral going up - note: roundtrip doesn't hold for non-planar polylines
            // because local planes differ at each vertex
            let pts = ResizeArray [
                Pnt(10, 0, 0)
                Pnt(0, 10, 5)
                Pnt(-10, 0, 10)
                Pnt(0, -10, 15)
                Pnt(10, 0, 20)
            ]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let d = 1.0
            let result = Polyline3D.offsetByNormal refNormal d 0.0 pl
            recordTestCase "3D spiral staircase" pl d 0.0 refNormal result
            Expect.equal result.PointCount pts.Count "Should have same point count"
            // Verify offset produced different points (not just a copy)
            let totalDist = [for i in 0..pts.Count-1 -> Pnt.distanceTo result.AsPoints.[i] pts.[i]] |> List.sum
            Expect.isTrue (totalDist > 0.1) "Offset should produce different points"

        // ============ Specific value checks ============

        testCase "Polyline3D.offset closed rectangle specific values" <| fun _ ->
            // Rectangle 20x10 in XY plane
            let pts = ResizeArray [Pnt(0,0,0); Pnt(20,0,0); Pnt(20,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let result = Polyline3D.offsetByNormal refNormal 2.0 0.0 pl
            recordTestCase "closed rectangle 20x10" pl 2.0 0.0 refNormal result
            Expect.equal result.PointCount 5 "Should have 5 points"
            Expect.isTrue (eqPnt result.AsPoints.[0] (Pnt(2, 2, 0))) $"Corner 0 should be at (2,2,0), got {result.AsPoints.[0]}"
            Expect.isTrue (eqPnt result.AsPoints.[1] (Pnt(18, 2, 0))) $"Corner 1 should be at (18,2,0), got {result.AsPoints.[1]}"
            Expect.isTrue (eqPnt result.AsPoints.[2] (Pnt(18, 8, 0))) $"Corner 2 should be at (18,8,0), got {result.AsPoints.[2]}"
            Expect.isTrue (eqPnt result.AsPoints.[3] (Pnt(2, 8, 0))) $"Corner 3 should be at (2,8,0), got {result.AsPoints.[3]}"

        testCase "Polyline3D.offset open 3-point line distance check" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let d = 3.0
            let result = Polyline3D.offsetByNormal refNormal d 0.0 pl
            recordTestCase "open 3-point line distance check" pl d 0.0 refNormal result
            // First and last points should be exactly d away from original
            let dist0 = Pnt.distanceTo result.AsPoints.[0] pts.[0]
            let dist2 = Pnt.distanceTo result.AsPoints.[2] pts.[2]
            Expect.floatClose tol dist0 d $"First point should be {d} away, got {dist0}"
            Expect.floatClose tol dist2 d $"Last point should be {d} away, got {dist2}"

        testCase "Polyline3D.offset with loop flag on open L returns closed behavior" <| fun _ ->
            // Open L-shape treated as loop
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let d = 1.0
            // With loop=true
            let resultLoop = Polyline3D.offsetByNormalCustom(pl, d, 0.0, refNormal, loop=true)
            recordTestCase "open L with loop=true" pl d 0.0 refNormal resultLoop
            // Without loop
            let resultOpen = Polyline3D.offsetByNormalCustom(pl, d, 0.0, refNormal, loop=false)
            recordTestCase "open L with loop=false" pl d 0.0 refNormal resultOpen
            // The corner point should be different because with loop, it accounts for the closing segment
            Expect.equal resultLoop.PointCount 3 "Loop result should have 3 points"
            Expect.equal resultOpen.PointCount 3 "Open result should have 3 points"
            // Corner points should differ
            Expect.isFalse (eqPnt resultLoop.AsPoints.[0] resultOpen.AsPoints.[0]) "Start points should differ between loop and open"

        testCase "Polyline3D.offset large offset value" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(100,0,0); Pnt(100,100,0); Pnt(0,100,0); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let d = 10.0
            let offset1 = Polyline3D.offsetByNormal refNormal d 0.0 pl
            let offset2 = Polyline3D.offsetByNormal refNormal (-d) 0.0 offset1
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (large offset)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.AsPoints.[i] pts.[i]) $"pt[{i}] returns to original (large offset)"

        testCase "Polyline3D.offset very small offset value" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let refNormal = UnitVec.zAxis
            let d = 0.001
            let offset1 = Polyline3D.offsetByNormal refNormal d 0.0 pl
            let offset2 = Polyline3D.offsetByNormal refNormal (-d) 0.0 offset1
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (small offset)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.AsPoints.[i] pts.[i]) $"pt[{i}] returns to original (small offset)"

        // ============ Orientation independence tests ============

        testCase "Polyline3D.offset positive inPlane offsets inward for CCW square" <| fun _ ->
            // CCW square in XY plane when viewed from +Z
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let result = Polyline3D.offset 1.0 0.0 pl
            // Positive inPlane should shrink the square (inward offset)
            Expect.isTrue (eqPnt result.AsPoints.[0] (Pnt(1, 1, 0))) $"CCW: Corner 0 should be inward at (1,1,0), got {result.AsPoints.[0]}"
            Expect.isTrue (eqPnt result.AsPoints.[1] (Pnt(9, 1, 0))) $"CCW: Corner 1 should be inward at (9,1,0), got {result.AsPoints.[1]}"
            Expect.isTrue (eqPnt result.AsPoints.[2] (Pnt(9, 9, 0))) $"CCW: Corner 2 should be inward at (9,9,0), got {result.AsPoints.[2]}"
            Expect.isTrue (eqPnt result.AsPoints.[3] (Pnt(1, 9, 0))) $"CCW: Corner 3 should be inward at (1,9,0), got {result.AsPoints.[3]}"

        testCase "Polyline3D.offset positive inPlane offsets inward for CW square" <| fun _ ->
            // CW square in XY plane when viewed from +Z (reversed winding)
            let pts = ResizeArray [Pnt(0,0,0); Pnt(0,10,0); Pnt(10,10,0); Pnt(10,0,0); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let result = Polyline3D.offset 1.0 0.0 pl
            // Positive inPlane should still shrink the square (inward offset) regardless of winding
            Expect.isTrue (eqPnt result.AsPoints.[0] (Pnt(1, 1, 0))) $"CW: Corner 0 should be inward at (1,1,0), got {result.AsPoints.[0]}"
            Expect.isTrue (eqPnt result.AsPoints.[1] (Pnt(1, 9, 0))) $"CW: Corner 1 should be inward at (1,9,0), got {result.AsPoints.[1]}"
            Expect.isTrue (eqPnt result.AsPoints.[2] (Pnt(9, 9, 0))) $"CW: Corner 2 should be inward at (9,9,0), got {result.AsPoints.[2]}"
            Expect.isTrue (eqPnt result.AsPoints.[3] (Pnt(9, 1, 0))) $"CW: Corner 3 should be inward at (9,1,0), got {result.AsPoints.[3]}"

        testCase "Polyline3D.offset negative inPlane offsets outward for CCW square" <| fun _ ->
            // CCW square in XY plane
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let result = Polyline3D.offset (-1.0) 0.0 pl
            // Negative inPlane should expand the square (outward offset)
            Expect.isTrue (eqPnt result.AsPoints.[0] (Pnt(-1, -1, 0))) $"CCW: Corner 0 should be outward at (-1,-1,0), got {result.AsPoints.[0]}"
            Expect.isTrue (eqPnt result.AsPoints.[1] (Pnt(11, -1, 0))) $"CCW: Corner 1 should be outward at (11,-1,0), got {result.AsPoints.[1]}"
            Expect.isTrue (eqPnt result.AsPoints.[2] (Pnt(11, 11, 0))) $"CCW: Corner 2 should be outward at (11,11,0), got {result.AsPoints.[2]}"
            Expect.isTrue (eqPnt result.AsPoints.[3] (Pnt(-1, 11, 0))) $"CCW: Corner 3 should be outward at (-1,11,0), got {result.AsPoints.[3]}"

        testCase "Polyline3D.offset negative inPlane offsets outward for CW square" <| fun _ ->
            // CW square in XY plane (reversed winding)
            let pts = ResizeArray [Pnt(0,0,0); Pnt(0,10,0); Pnt(10,10,0); Pnt(10,0,0); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let result = Polyline3D.offset (-1.0) 0.0 pl
            // Negative inPlane should still expand the square (outward offset) regardless of winding
            Expect.isTrue (eqPnt result.AsPoints.[0] (Pnt(-1, -1, 0))) $"CW: Corner 0 should be outward at (-1,-1,0), got {result.AsPoints.[0]}"
            Expect.isTrue (eqPnt result.AsPoints.[1] (Pnt(-1, 11, 0))) $"CW: Corner 1 should be outward at (-1,11,0), got {result.AsPoints.[1]}"
            Expect.isTrue (eqPnt result.AsPoints.[2] (Pnt(11, 11, 0))) $"CW: Corner 2 should be outward at (11,11,0), got {result.AsPoints.[2]}"
            Expect.isTrue (eqPnt result.AsPoints.[3] (Pnt(11, -1, 0))) $"CW: Corner 3 should be outward at (11,-1,0), got {result.AsPoints.[3]}"

        testCase "Polyline3D.offset positive perp offsets upward for CCW square in XY plane" <| fun _ ->
            // CCW square in XY plane - computed normal should point +Z
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let result = Polyline3D.offset 0.0 2.0 pl
            // Positive perpendicular offset should go in direction of computed normal (+Z for CCW)
            for i = 0 to 3 do
                Expect.isTrue (abs(result.AsPoints.[i].Z - 2.0) < 1e-6) $"CCW: Point {i} should be at Z=2, got Z={result.AsPoints.[i].Z}"

        testCase "Polyline3D.offset positive perp offsets downward for CW square in XY plane" <| fun _ ->
            // CW square in XY plane - computed normal should point -Z
            let pts = ResizeArray [Pnt(0,0,0); Pnt(0,10,0); Pnt(10,10,0); Pnt(10,0,0); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let result = Polyline3D.offset 0.0 2.0 pl
            // Positive perpendicular offset should go in direction of computed normal (-Z for CW)
            for i = 0 to 3 do
                Expect.isTrue (abs(result.AsPoints.[i].Z - (-2.0)) < 1e-6) $"CW: Point {i} should be at Z=-2, got Z={result.AsPoints.[i].Z}"

        testCase "Polyline3D.offset negative perp offsets downward for CCW square in XY plane" <| fun _ ->
            // CCW square in XY plane
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let result = Polyline3D.offset 0.0 (-2.0) pl
            // Negative perpendicular offset should go opposite to computed normal (-Z for CCW)
            for i = 0 to 3 do
                Expect.isTrue (abs(result.AsPoints.[i].Z - (-2.0)) < 1e-6) $"CCW: Point {i} should be at Z=-2, got Z={result.AsPoints.[i].Z}"

        testCase "Polyline3D.offset negative perp offsets upward for CW square in XY plane" <| fun _ ->
            // CW square in XY plane
            let pts = ResizeArray [Pnt(0,0,0); Pnt(0,10,0); Pnt(10,10,0); Pnt(10,0,0); Pnt(0,0,0)]
            let pl = Polyline3D.createFromPnts(pts)
            let result = Polyline3D.offset 0.0 (-2.0) pl
            // Negative perpendicular offset should go opposite to computed normal (+Z for CW)
            for i = 0 to 3 do
                Expect.isTrue (abs(result.AsPoints.[i].Z - 2.0) < 1e-6) $"CW: Point {i} should be at Z=2, got Z={result.AsPoints.[i].Z}"

        testList "getSegmentUnitVectors (interleaved floats)" [
            testCase "returns one interleaved unit vector per segment" <| fun _ ->
                let xyzs = (Polyline3D.createFromPnts [Pnt(0,0,0); Pnt(2,0,0); Pnt(2,0,3)]).XYZs
                let uvs = Offset3D.getSegmentUnitVectors xyzs
                Expect.equal uvs.Count 6 "two segments -> 6 interleaved floats"
                Expect.floatClose tol uvs.[0] 1.0 "seg0 x = 1"
                Expect.floatClose tol uvs.[1] 0.0 "seg0 y = 0"
                Expect.floatClose tol uvs.[2] 0.0 "seg0 z = 0"
                Expect.floatClose tol uvs.[3] 0.0 "seg1 x = 0"
                Expect.floatClose tol uvs.[4] 0.0 "seg1 y = 0"
                Expect.floatClose tol uvs.[5] 1.0 "seg1 z = 1 (already unitized)"

            testCase "fails on duplicate consecutive points" <| fun _ ->
                let xyzs = (Polyline3D.createFromPnts [Pnt(0,0,0); Pnt(0,0,0); Pnt(1,0,0)]).XYZs
                Expect.throws (fun () -> Offset3D.getSegmentUnitVectors xyzs |> ignore) "duplicate points should fail"

            testCase "fails with fewer than 2 points" <| fun _ ->
                let xyzs = (Polyline3D.createFromPnts [Pnt(0,0,0)]).XYZs
                Expect.throws (fun () -> Offset3D.getSegmentUnitVectors xyzs |> ignore) "single point should fail"
        ]

        testList "Offset3D.removeUTurns" [
            testCase "no U-turns returns the original buffer unchanged" <| fun _ ->
                let xyzs = (Polyline3D.createFromPnts [Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0); Pnt(0,1,0)]).XYZs
                let uvs = Offset3D.getSegmentUnitVectors xyzs
                let res = Offset3D.removeUTurns(xyzs, uvs, Cosine.``179.0``)
                Expect.equal res.Count xyzs.Count "unchanged count"
                Expect.isTrue (System.Object.ReferenceEquals(res, xyzs)) "returns the original buffer when nothing to remove"

            testCase "simple open U-turn point removed" <| fun _ ->
                let xyzs = (Polyline3D.createFromPnts [Pnt(0,0,0); Pnt(5,0,0); Pnt(5,5,0); Pnt(5,1,0); Pnt(9,1,0)]).XYZs
                let uvs = Offset3D.getSegmentUnitVectors xyzs
                let res = Offset3D.removeUTurns(xyzs, uvs, Cosine.``179.99``)
                Expect.equal res.Count (xyzs.Count - 3) "one point (3 floats) removed"
                "removed the U-turn vertex (5,5,0), so index 2 is now (5,1,0)" |> Expect.floatClose tol res.[7] 1.0

            testCase "U-turn in a non-axis-aligned (XZ) plane is detected" <| fun _ ->
                let xyzs = (Polyline3D.createFromPnts [Pnt(0,0,0); Pnt(2,0,0); Pnt(2,0,2); Pnt(2,0,0.5); Pnt(5,0,0.5)]).XYZs
                let uvs = Offset3D.getSegmentUnitVectors xyzs
                let res = Offset3D.removeUTurns(xyzs, uvs, Cosine.``179.99``)
                Expect.equal res.Count (xyzs.Count - 3) "the out-of-XY-plane U-turn vertex is removed"

            testCase "closed seam U-turn keeps polyline closed" <| fun _ ->
                let xyzs = (Polyline3D.createFromPnts [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(5,0.001,0); Pnt(0,0,0)]).XYZs
                let uvs = Offset3D.getSegmentUnitVectors xyzs
                let res = Offset3D.removeUTurns(xyzs, uvs, Cosine.``179.9``)
                let n = res.Count
                let stillClosed = abs(res.[0]-res.[n-3]) < 1e-9 && abs(res.[1]-res.[n-2]) < 1e-9 && abs(res.[2]-res.[n-1]) < 1e-9
                Expect.isTrue stillClosed "result still closed after removing the seam U-turn"

            testCase "fails on segment vector count mismatch" <| fun _ ->
                let xyzs = (Polyline3D.createFromPnts [Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)]).XYZs
                let uvs = ResizeArray([1.0; 0.0; 0.0]) // only 1 vector, needs 2
                Expect.throws (fun () -> Offset3D.removeUTurns(xyzs, uvs, Cosine.``179.0``) |> ignore) "count mismatch should fail"

            testCase "fails on segment vectors that are not a multiple of 3" <| fun _ ->
                let xyzs = (Polyline3D.createFromPnts [Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)]).XYZs
                let uvs = ResizeArray([1.0; 0.0]) // not a multiple of 3
                Expect.throws (fun () -> Offset3D.removeUTurns(xyzs, uvs, Cosine.``179.0``) |> ignore) "non multiple-of-3 should fail"
        ]

        testList "Offset3D directions without corners" [
            let rad (deg:float) = deg * Math.PI / 180.0

            testCase "finely sampled open arc is offset at every point" <| fun _ ->
                // 2 degrees per point, below the 2.5 degree collinear threshold, so no point is a corner
                let arc = tilted [ for i in 0 .. 45 -> let a = rad (2.0 * float i) in Pnt(10.0 * cos a, 10.0 * sin a, 0.0) ]
                let res = Polyline3D.offsetByNormal worldZ 1.0 0.0 arc
                "point count" |> Expect.equal res.PointCount arc.PointCount
                "distance to the segments" |> Expect.isTrue (maxSegmentDistanceError arc res 1.0 < 1e-9)
                for p in res.AsPoints do
                    "offset towards the center" |> Expect.isTrue (Pnt.distanceTo p (toWorld Pnt.Origin) < 9.01)

            testCase "finely sampled closed circle is offset and stays closed" <| fun _ ->
                let n = 200 // 1.8 degrees per point, before it failed because no point had a valid direction
                let circle = tilted [ for i in 0 .. n -> let a = 2.0 * Math.PI * float (i % n) / float n in Pnt(10.0 * cos a, 10.0 * sin a, 0.0) ]
                let res = Polyline3D.offset 1.0 0.0 circle
                "point count" |> Expect.equal res.PointCount circle.PointCount
                "closed" |> Expect.isTrue (Pnt.equalsExact res.FirstPoint res.LastPoint)
                "distance to the segments" |> Expect.isTrue (maxSegmentDistanceError circle res 1.0 < 1e-9)
                let up = Polyline3D.offset 0.0 0.5 circle
                for i = 0 to n do
                    "perpendicular offset along the normal" |> Expect.isTrue (eqPnt up.AsPoints.[i] (circle.AsPoints.[i] + worldZ * 0.5))

            testCase "closed teardrop with just one corner is offset at every point" <| fun _ ->
                // an arc with straight sides that are tangent to it, so only the tip turns by more than 2.5 degrees
                let tear = tilted [ yield Pnt(20.0, 0.0, 0.0)
                                    for i in 0 .. 120 -> let a = rad (60.0 + 2.0 * float i) in Pnt(10.0 * cos a, 10.0 * sin a, 0.0)
                                    yield Pnt(20.0, 0.0, 0.0) ]
                let res = Polyline3D.offset 1.0 0.0 tear // before, this failed because the line between the corner and itself has no length
                "point count" |> Expect.equal res.PointCount tear.PointCount
                "closed" |> Expect.isTrue (Pnt.equalsExact res.FirstPoint res.LastPoint)
                "distance to the segments" |> Expect.isTrue (maxSegmentDistanceError tear res 1.0 < 1e-9)

            testCase "offsets that compute the average normal fail with a clear message without it" <| fun _ ->
                let msg (f: unit -> 'T) = try f () |> ignore; "" with e -> e.Message
                let line = tilted [Pnt(0.0, 0.0, 0.0); Pnt(5.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0)]
                "Offset3D.offset" |> Expect.stringContains (msg (fun () -> Offset3D.offset 1.0 0.0 line.XYZs)) "Use offsetByNormal with an explicit reference normal"
                "Offset3D.offsetVariable" |> Expect.stringContains (msg (fun () -> Offset3D.offsetVariable (ResizeArray [1.0; 1.0]) (ResizeArray [0.0; 0.0]) line.XYZs)) "Use offsetVariableByNormal with an explicit reference normal"
                "Offset3D.offset empty" |> Expect.stringContains (msg (fun () -> Offset3D.offset 1.0 0.0 (ResizeArray<float>()))) "point count 0 must be at least 2"
                // a symmetric open S-curve, the polygon through its points has no area
                let sCurve = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 10.0, 0.0); Pnt(20.0, 0.0, 0.0); Pnt(30.0, -10.0, 0.0); Pnt(40.0, 0.0, 0.0)]
                "Polyline3D.offset" |> Expect.stringContains (msg (fun () -> Polyline3D.offset 1.0 0.0 sCurve)) "has no area"
                "with a reference normal it works" |> Expect.equal (Polyline3D.offsetByNormal worldZ 1.0 0.0 sCurve).PointCount 5

            testCase "straight oblique line moves perpendicular to itself" <| fun _ ->
                let line = Polyline3D.createFromPnts [Pnt(3.0, 4.0, 5.0); Pnt(8.0, 4.0, 10.0); Pnt(13.0, 4.0, 15.0)] // 45 degrees to the Z axis
                let s = sqrt 0.5
                let perp = Polyline3D.offsetByNormal UnitVec.zAxis 0.0 1.0 line
                let inPlane = Polyline3D.offsetByNormal UnitVec.zAxis 1.0 0.0 line
                for i = 0 to 2 do
                    "perpendicular: refNormal made perpendicular to the line" |> Expect.isTrue (eqPnt perp.AsPoints.[i] (line.AsPoints.[i] + Vec(-s, 0.0, s)))
                    "in-plane: perpendicular to the line and to refNormal" |> Expect.isTrue (eqPnt inPlane.AsPoints.[i] (line.AsPoints.[i] + Vec(0.0, 1.0, 0.0)))

            testCase "fails with a clear message if refNormal is parallel to a straight polyline" <| fun _ ->
                let line = Polyline3D.createFromPnts [Pnt(1.0, 2.0, 3.0); Pnt(1.0, 2.0, 13.0)]
                let msg = try Polyline3D.offsetByNormal UnitVec.zAxis 1.0 0.0 line |> ignore; "" with e -> e.Message
                "message" |> Expect.stringContains msg "is parallel to the polyline at pts.[0]"

            testCase "open end points are offset perpendicular to their own segment" <| fun _ ->
                // pts.[1] turns by about 1.1 degrees, so it is collinear, and the first corner is pts.[2]
                let pl = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.1, 0.0); Pnt(20.0, 0.0, 0.0); Pnt(20.0, 10.0, 0.0)]
                let res = Polyline3D.offsetByNormal worldZ 1.0 0.0 pl
                let p = pl.AsPoints
                let r = res.AsPoints
                "start distance" |> Expect.floatClose tol (Line3D.distanceRayPoint r.[0] (Line3D(p.[0], p.[1]))) 1.0
                "start move is perpendicular to the first segment" |> Expect.isTrue (abs ((r.[0] - p.[0]) *** (p.[1] - p.[0])) < 1e-9)
                "end distance" |> Expect.floatClose tol (Line3D.distanceRayPoint r.[3] (Line3D(p.[2], p.[3]))) 1.0
                "end move is perpendicular to the last segment" |> Expect.isTrue (abs ((r.[3] - p.[3]) *** (p.[3] - p.[2])) < 1e-9)
        ]

        testList "Offset3D invariants" [
            let lShape = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 4.0, 0.0); Pnt(4.0, 4.0, 0.0); Pnt(4.0, 10.0, 0.0); Pnt(0.0, 10.0, 0.0); Pnt(0.0, 0.0, 0.0)]
            let skewQuad = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 3.0); Pnt(10.0, 10.0, 0.0); Pnt(0.0, 10.0, 3.0); Pnt(0.0, 0.0, 0.0)]
            let spiral = tilted [Pnt(10.0, 0.0, 0.0); Pnt(0.0, 10.0, 5.0); Pnt(-10.0, 0.0, 10.0); Pnt(0.0, -10.0, 15.0); Pnt(10.0, 0.0, 20.0)]
            let reversed (pl:Polyline3D) = Polyline3D.createFromPnts (Seq.rev pl.AsPoints)
            let shiftStart k (pl:Polyline3D) = // for closed polylines, start at point k
                let open' = pl.AsPoints |> Seq.take (pl.PointCount - 1) |> Seq.toList
                let s = List.skip k open' @ List.take k open'
                Polyline3D.createFromPnts (s @ [s.Head])

            testCase "every point is at the offset distance from its adjacent segments" <| fun _ ->
                for d in [1.25; -0.75] do
                    "concave L-shape" |> Expect.isTrue (maxSegmentDistanceError lShape (Polyline3D.offset d 0.0 lShape) d < 1e-9)
                    "non-planar quad" |> Expect.isTrue (maxSegmentDistanceError skewQuad (Polyline3D.offsetByNormal worldZ d 0.0 skewQuad) d < 1e-9)
                    "non-planar spiral" |> Expect.isTrue (maxSegmentDistanceError spiral (Polyline3D.offsetByNormal worldZ d 0.0 spiral) d < 1e-9)
                // the perpendicular offset is perpendicular to both adjacent segments too
                let both = Polyline3D.offsetByNormal worldZ 0.6 0.8 spiral
                "in-plane and perpendicular" |> Expect.isTrue (maxSegmentDistanceError spiral both 1.0 < 1e-9)

            testCase "positive distances offset the concave L-shape inwards" <| fun _ ->
                let res = Polyline3D.offset 1.0 0.0 lShape
                let expected = [Pnt(1.0, 1.0, 0.0); Pnt(9.0, 1.0, 0.0); Pnt(9.0, 3.0, 0.0); Pnt(3.0, 3.0, 0.0); Pnt(3.0, 9.0, 0.0); Pnt(1.0, 9.0, 0.0); Pnt(1.0, 1.0, 0.0)]
                expected |> List.iteri (fun i e -> Expect.isTrue (eqPnt res.AsPoints.[i] (toWorld e)) $"pt[{i}]")

            testCase "reversing the polyline and the normal gives the reversed result" <| fun _ ->
                // the in-plane inside stays inside, the perpendicular side follows the normal, so it flips
                for pl in [lShape; skewQuad; spiral] do
                    let a = Polyline3D.offsetByNormal worldZ 1.0 0.5 pl
                    let b = Polyline3D.offsetByNormal -worldZ 1.0 -0.5 (reversed pl)
                    Expect.isTrue (Polyline3D.equalsWithin 1e-9 a (reversed b)) $"reversed {pl}"
                // the average normal flips with the polyline
                let c = Polyline3D.offset 1.0 0.5 lShape
                let d = Polyline3D.offset 1.0 -0.5 (reversed lShape)
                "average normal" |> Expect.isTrue (Polyline3D.equalsWithin 1e-9 c (reversed d))

            testCase "the start point of a closed polyline does not change the result" <| fun _ ->
                for pl in [lShape; skewQuad] do
                    let a = Polyline3D.offsetByNormal worldZ 1.0 0.5 pl
                    for k in 1 .. pl.PointCount - 2 do
                        let b = Polyline3D.offsetByNormal worldZ 1.0 0.5 (shiftStart k pl)
                        for i = 0 to pl.PointCount - 2 do
                            Expect.isTrue (eqPnt b.AsPoints.[i] a.AsPoints.[(i + k) % (pl.PointCount - 1)]) $"shift {k}, pt[{i}]"

            testCase "variable offset: reversing the polyline and the in-plane distances gives the reversed result" <| fun _ ->
                let ds = [0.5; 1.0; 1.5; 0.75; 1.25; 0.25]
                let zeros = ResizeArray (List.replicate ds.Length 0.0)
                let a = Polyline3D.offsetVariableByNormal worldZ (ResizeArray ds) zeros lShape
                let b = Polyline3D.offsetVariableByNormal -worldZ (ResizeArray (List.rev ds)) zeros (reversed lShape)
                "reversed" |> Expect.isTrue (Polyline3D.equalsWithin 1e-9 a (reversed b))
        ]

        testList "Polyline3D offset thresholds" [
            let rad (deg:float) = deg * Math.PI / 180.0
            // a 177 degree U-turn at pts.[1]
            let uTurn = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0 + 10.0 * cos (rad 177.0), 10.0 * sin (rad 177.0), 0.0)]

            testCase "all constant offsets fail at a 177 degree U-turn by default" <| fun _ ->
                Expect.throws (fun () -> Polyline3D.offset 1.0 0.0 uTurn |> ignore) "Polyline3D.offset"
                Expect.throws (fun () -> Polyline3D.offsetByNormal worldZ 1.0 0.0 uTurn |> ignore) "Polyline3D.offsetByNormal"
                Expect.throws (fun () -> Offset3D.offsetByNormal worldZ 1.0 0.0 uTurn.XYZs |> ignore) "Offset3D.offsetByNormal"

            testCase "failAtUTurnAbove allows a sharper U-turn" <| fun _ ->
                let a = Polyline3D.offsetByNormalCustom(uTurn, 1.0, 0.0, worldZ, failAtUTurnAbove = Cosine.``179.0``)
                let b = Polyline3D.offsetCustom(uTurn, 1.0, 0.0, failAtUTurnAbove = Cosine.``179.0``)
                "point count" |> Expect.equal a.PointCount 3
                "distance to the segments" |> Expect.isTrue (maxSegmentDistanceError uTurn a 1.0 < 1e-9)
                "same as with the average normal" |> Expect.isTrue (Polyline3D.equalsWithin 1e-9 a b)

            testCase "a fine arc with a sharp corner is offset exactly, with the default and a smaller considerCollinearBelow" <| fun _ ->
                // a quarter arc with 2 degrees per point, then a sharp corner: before, the default placed the arc points on a chord
                let pts = [ for i in 0 .. 45 -> let a = rad (2.0 * float i) in Pnt(10.0 * cos a, 10.0 * sin a, 0.0) ] @ [Pnt(-5.0, 5.0, 0.0)]
                let pl = tilted pts
                let coarse = Polyline3D.offsetByNormal worldZ 1.0 0.0 pl
                let fine = Polyline3D.offsetByNormalCustom(pl, 1.0, 0.0, worldZ, considerCollinearBelow = Cosine.``0.5``)
                "default is exact" |> Expect.isTrue (maxSegmentDistanceError pl coarse 1.0 < 1e-9)
                "smaller threshold is exact" |> Expect.isTrue (maxSegmentDistanceError pl fine 1.0 < 1e-9)
        ]

        testList "Polyline3D.offsetVariable" [
            let ds (xs: float list) = ResizeArray xs
            let expectPoints (res:Polyline3D) (expected: Pnt list) =
                "point count" |> Expect.equal res.PointCount expected.Length
                expected |> List.iteri (fun i e ->
                    let p = res.AsPoints.[i]
                    Expect.isTrue (eqPnt p (toWorld e)) $"pt[{i}] should be {toWorld e} but is {p}")

            testCase "equal distances give the same result as the constant offset" <| fun _ ->
                let shapes = [
                    tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 10.0, 0.0)]
                    tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 4.0, 0.0); Pnt(4.0, 4.0, 0.0); Pnt(4.0, 10.0, 0.0); Pnt(0.0, 10.0, 0.0); Pnt(0.0, 0.0, 0.0)]
                    tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 3.0); Pnt(10.0, 10.0, 0.0); Pnt(0.0, 10.0, 3.0); Pnt(0.0, 0.0, 0.0)] ] // not planar
                for pl in shapes do
                    let n = pl.SegmentCount
                    let m = if pl.IsClosed then n else n + 1 // one perpendicular distance per point, the last point of a closed one is the first
                    let v = Polyline3D.offsetVariableByNormal worldZ (ds (List.replicate n 1.5)) (ds (List.replicate m 0.5)) pl
                    let c = Polyline3D.offsetByNormal worldZ 1.5 0.5 pl
                    Expect.isTrue (Polyline3D.equalsWithin 1e-9 v c) $"variable {v} should equal constant {c}"

            testCase "open polyline end points take the distances of their own segment" <| fun _ ->
                let pl = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 10.0, 0.0)]
                let res = Polyline3D.offsetVariableByNormal worldZ (ds [1.0; 2.0]) (ds [0.0; 0.0; 0.0]) pl
                expectPoints res [Pnt(0.0, 1.0, 0.0); Pnt(8.0, 1.0, 0.0); Pnt(8.0, 10.0, 0.0)]

            testCase "closed polyline seam takes the distances of the first and last segment" <| fun _ ->
                let pl = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 10.0, 0.0); Pnt(0.0, 10.0, 0.0); Pnt(0.0, 0.0, 0.0)]
                let res = Polyline3D.offsetVariableByNormal worldZ (ds [1.0; 2.0; 3.0; 4.0]) (ds [0.0; 0.0; 0.0; 0.0]) pl
                expectPoints res [Pnt(4.0, 1.0, 0.0); Pnt(8.0, 1.0, 0.0); Pnt(8.0, 7.0, 0.0); Pnt(4.0, 7.0, 0.0); Pnt(4.0, 1.0, 0.0)]
                "closed" |> Expect.isTrue (Pnt.equalsExact res.FirstPoint res.LastPoint)

            testCase "each point takes its own perpendicular distance" <| fun _ ->
                let closed = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 10.0, 0.0); Pnt(0.0, 10.0, 0.0); Pnt(0.0, 0.0, 0.0)]
                let res = Polyline3D.offsetVariableByNormal worldZ (ds [1.0; 1.0; 1.0; 1.0]) (ds [1.0; 2.0; 3.0; 4.0]) closed
                expectPoints res [Pnt(1.0, 1.0, 1.0); Pnt(9.0, 1.0, 2.0); Pnt(9.0, 9.0, 3.0); Pnt(1.0, 9.0, 4.0); Pnt(1.0, 1.0, 1.0)] // the last point is the first
                let opened = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 3.0, 0.0); Pnt(20.0, 0.0, 0.0); Pnt(30.0, 3.0, 0.0)]
                let res = Polyline3D.offsetVariableByNormal worldZ (ds [0.0; 0.0; 0.0]) (ds [0.5; 1.0; 2.0; 3.0]) opened
                expectPoints res [Pnt(0.0, 0.0, 0.5); Pnt(10.0, 3.0, 1.0); Pnt(20.0, 0.0, 2.0); Pnt(30.0, 3.0, 3.0)] // both end points have their own distance

            testCase "reversing the polyline and both distance lists gives the reversed result" <| fun _ ->
                let opened = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 3.0, 0.0); Pnt(20.0, 0.0, 0.0); Pnt(30.0, 3.0, 2.0)] // not planar
                let a = Polyline3D.offsetVariableByNormal worldZ (ds [0.5; 1.0; 1.5]) (ds [0.25; 1.0; 2.0; 3.0]) opened
                let reversedPl = Polyline3D.createFromPnts (Seq.rev opened.AsPoints)
                let b = Polyline3D.offsetVariableByNormal -worldZ (ds [1.5; 1.0; 0.5]) (ds [-3.0; -2.0; -1.0; -0.25]) reversedPl
                "reversed" |> Expect.isTrue (Polyline3D.equalsWithin 1e-9 a (Polyline3D.createFromPnts (Seq.rev b.AsPoints)))

            testCase "offsetVariable takes arrays and uses the average normal" <| fun _ ->
                let pl = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 10.0, 0.0); Pnt(0.0, 10.0, 0.0); Pnt(0.0, 0.0, 0.0)]
                let a = Polyline3D.offsetVariable [|1.0; 2.0; 3.0; 4.0|] [|0.5; 0.5; 0.5; 0.5|] pl
                let b = Polyline3D.offsetVariableByNormal worldZ [|1.0; 2.0; 3.0; 4.0|] [|0.5; 0.5; 0.5; 0.5|] pl
                "same as with worldZ" |> Expect.isTrue (Polyline3D.equalsWithin 1e-9 a b)

            testCase "fails with a clear message on wrong distance counts" <| fun _ ->
                let pl = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 10.0, 0.0)]
                let msg (f: unit -> Polyline3D) = try f () |> ignore; "" with e -> e.Message
                "too few perpendicular" |> Expect.stringContains (msg (fun () -> Polyline3D.offsetVariableByNormal worldZ (ds [1.0; 1.0]) (ds [0.0; 0.0]) pl)) "perpendicular distances count must be 3 for the 3 points of an open polyline"
                "too many perpendicular" |> Expect.stringContains (msg (fun () -> Polyline3D.offsetVariableByNormal worldZ (ds [1.0; 1.0]) (ds [0.0; 0.0; 0.0; 5.0]) pl)) "perpendicular distances count must be 3 for the 3 points of an open polyline"
                "too few in-plane" |> Expect.stringContains (msg (fun () -> Polyline3D.offsetVariableByNormal worldZ (ds [1.0]) (ds [0.0; 0.0; 0.0]) pl)) "in-plane distances count must be 2"
                let closed = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 10.0, 0.0); Pnt(0.0, 0.0, 0.0)]
                "closed, one per point" |> Expect.stringContains (msg (fun () -> Polyline3D.offsetVariableByNormal worldZ (ds [1.0; 1.0; 1.0]) (ds [0.0; 0.0; 0.0; 0.0]) closed)) "perpendicular distances count must be 3 for the 4 points of a closed polyline"
                "closed, too few" |> Expect.stringContains (msg (fun () -> Polyline3D.offsetVariableByNormal worldZ (ds [1.0; 1.0; 1.0]) (ds [0.0; 0.0]) closed)) "perpendicular distances count must be 3 for the 4 points of a closed polyline"
                let looped (perp: float list) () = Polyline3D.offsetVariableByNormalCustom(pl, ds [1.0; 1.0; 1.0], ds perp, worldZ, loop = true)
                "loop = true, one per point" |> Expect.equal (looped [0.0; 0.0; 0.0] ()).PointCount 3
                "loop = true, too few" |> Expect.stringContains (msg (looped [0.0; 0.0])) "perpendicularOffsetDistances list must have the same number of items as the polyline has points"

            testCase "kernel fails on a wrong offset directions count" <| fun _ ->
                let xyzs = (tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 10.0, 0.0)]).XYZs
                let uvs = Offset3D.getSegmentUnitVectors xyzs
                let dirs = Offset3D.getOffsetDirections(xyzs, uvs, worldZ, Cosine.``2.5``, Cosine.``175.0``)
                dirs.RemoveAt 0
                let msg = try Offset3D.offsetVariableWithDirections(xyzs, uvs, dirs, ds [1.0; 1.0], ds [0.0; 0.0; 0.0], Offset3D.VarDistParallel.Fail, Cosine.``2.5``) |> ignore; "" with e -> e.Message
                "message" |> Expect.stringContains msg "offset directions count must be 3"
                let constMsg = try Offset3D.offsetWithDirections(xyzs, dirs, 1.0, 0.0) |> ignore; "" with e -> e.Message
                "constant" |> Expect.stringContains constMsg "point count must equal offset directions count, but they are 3 and 2"
                let dirMsg = try Offset3D.getOffsetDirections(xyzs, ResizeArray [1.0; 0.0; 0.0], worldZ, Cosine.``2.5``, Cosine.``175.0``) |> ignore; "" with e -> e.Message
                "directions" |> Expect.stringContains dirMsg "point count must be one greater than segment vector count but they are 3 and 1"
        ]

        testList "Offset3D variable offset at collinear points" [
            let ds (xs: float list) = ResizeArray xs
            let zeros n = ResizeArray (List.replicate n 0.0)
            let offsetWith behavior (dists: float list) (pl:Polyline3D) =
                let perpCount = if pl.IsClosed then dists.Length else dists.Length + 1
                Polyline3D.offsetVariableByNormalCustom(pl, ds dists, zeros perpCount, worldZ, varDistParallelBehavior = behavior)
            let expectPoints (res:Polyline3D) (expected: Pnt list) =
                "point count" |> Expect.equal res.PointCount expected.Length
                expected |> List.iteri (fun i e ->
                    let p = res.AsPoints.[i]
                    Expect.isTrue (eqPnt p (toWorld e)) $"pt[{i}] should be {toWorld e} but is {p}")
            // a square with an extra point on the first edge
            let square = tilted [Pnt(0.0, 0.0, 0.0); Pnt(5.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 10.0, 0.0); Pnt(0.0, 10.0, 0.0); Pnt(0.0, 0.0, 0.0)]
            // a square that starts in the middle of an edge, so the seam is collinear
            let seamSquare = tilted [Pnt(5.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 10.0, 0.0); Pnt(0.0, 10.0, 0.0); Pnt(0.0, 0.0, 0.0); Pnt(5.0, 0.0, 0.0)]
            let line = tilted [Pnt(0.0, 0.0, 0.0); Pnt(5.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0)]

            testCase "equal distances at a collinear point do not use VarDistParallel" <| fun _ ->
                for behavior in [Offset3D.VarDistParallel.Fail; Offset3D.VarDistParallel.Skip] do
                    let res = offsetWith behavior [1.0; 1.0; 1.0; 1.0; 1.0] square
                    expectPoints res [Pnt(1.0, 1.0, 0.0); Pnt(5.0, 1.0, 0.0); Pnt(9.0, 1.0, 0.0); Pnt(9.0, 9.0, 0.0); Pnt(1.0, 9.0, 0.0); Pnt(1.0, 1.0, 0.0)]
                let seam = offsetWith Offset3D.VarDistParallel.Fail [1.0; 1.0; 1.0; 1.0; 1.0] seamSquare
                expectPoints seam [Pnt(5.0, 1.0, 0.0); Pnt(9.0, 1.0, 0.0); Pnt(9.0, 9.0, 0.0); Pnt(1.0, 9.0, 0.0); Pnt(1.0, 1.0, 0.0); Pnt(5.0, 1.0, 0.0)]

            testCase "different distances at a collinear point fail with a clear message" <| fun _ ->
                let msg = try offsetWith Offset3D.VarDistParallel.Fail [1.0; 2.0; 1.0; 1.0; 1.0] square |> ignore; "" with e -> e.Message
                "message" |> Expect.stringContains msg "point[1] is between collinear segments with the different in-plane distances 1 and 2"

            testCase "Proportional and Skip at a collinear point" <| fun _ ->
                let prop = offsetWith Offset3D.VarDistParallel.Proportional [1.0; 2.0; 1.0; 1.0; 1.0] square
                expectPoints prop [Pnt(1.0, 1.0, 0.0); Pnt(5.0, 1.5, 0.0); Pnt(9.0, 2.0, 0.0); Pnt(9.0, 9.0, 0.0); Pnt(1.0, 9.0, 0.0); Pnt(1.0, 1.0, 0.0)]
                let skip = offsetWith Offset3D.VarDistParallel.Skip [1.0; 2.0; 1.0; 1.0; 1.0] square
                expectPoints skip [Pnt(1.0, 1.0, 0.0); Pnt(9.0, 2.0, 0.0); Pnt(9.0, 9.0, 0.0); Pnt(1.0, 9.0, 0.0); Pnt(1.0, 1.0, 0.0)]

            testCase "a collinear seam stays closed with Proportional and Skip" <| fun _ ->
                let prop = offsetWith Offset3D.VarDistParallel.Proportional [2.0; 1.0; 1.0; 1.0; 1.0] seamSquare
                expectPoints prop [Pnt(5.0, 1.5, 0.0); Pnt(9.0, 2.0, 0.0); Pnt(9.0, 9.0, 0.0); Pnt(1.0, 9.0, 0.0); Pnt(1.0, 1.0, 0.0); Pnt(5.0, 1.5, 0.0)]
                "Proportional closed" |> Expect.isTrue (Pnt.equalsExact prop.FirstPoint prop.LastPoint)
                let skip = offsetWith Offset3D.VarDistParallel.Skip [2.0; 1.0; 1.0; 1.0; 1.0] seamSquare
                expectPoints skip [Pnt(9.0, 2.0, 0.0); Pnt(9.0, 9.0, 0.0); Pnt(1.0, 9.0, 0.0); Pnt(1.0, 1.0, 0.0); Pnt(9.0, 2.0, 0.0)]

            testCase "Project gives the same points as in 2D" <| fun _ ->
                let pts2D = [Pt(0.0, 0.0); Pt(5.0, 0.0); Pt(10.0, 0.0); Pt(10.0, 10.0); Pt(0.0, 10.0)] // open, closed with loop = true
                let dists = [1.0; 2.0; 1.0; 1.0; 1.0]
                let res2D = Polyline2D.offsetVariableCustom(Polyline2D.createFromPts pts2D, ds dists, loop = true, varDistParallelBehavior = Offset2D.VarDistParallel.Project)
                let pl = tilted [ for p in pts2D -> Pnt(p.X, p.Y, 0.0) ]
                let res3D = Polyline3D.offsetVariableByNormalCustom(pl, ds dists, zeros dists.Length, worldZ, loop = true, varDistParallelBehavior = Offset3D.VarDistParallel.Project)
                expectPoints res3D [ for p in res2D.AsPoints -> Pnt(p.X, p.Y, 0.0) ]
                expectPoints res3D [Pnt(1.0, 1.0, 0.0); Pnt(5.0, 1.5, 0.0); Pnt(9.0, 2.0, 0.0); Pnt(9.0, 9.0, 0.0); Pnt(1.0, 9.0, 0.0)]
                let seam = offsetWith Offset3D.VarDistParallel.Project [2.0; 1.0; 1.0; 1.0; 1.0] seamSquare
                expectPoints seam [Pnt(5.0, 1.5, 0.0); Pnt(9.0, 2.0, 0.0); Pnt(9.0, 9.0, 0.0); Pnt(1.0, 9.0, 0.0); Pnt(1.0, 1.0, 0.0); Pnt(5.0, 1.5, 0.0)]
                expectPoints (offsetWith Offset3D.VarDistParallel.Project [1.0; 2.0] line) [Pnt(0.0, 1.0, 0.0); Pnt(5.0, 1.5, 0.0); Pnt(10.0, 2.0, 0.0)]

            testCase "a straight polyline with different distances uses VarDistParallel" <| fun _ ->
                // no point is a corner, so all points get directions from refNormal, before this gave infinite points
                Expect.throws (fun () -> offsetWith Offset3D.VarDistParallel.Fail [1.0; 2.0] line |> ignore) "Fail"
                expectPoints (offsetWith Offset3D.VarDistParallel.Proportional [1.0; 2.0] line) [Pnt(0.0, 1.0, 0.0); Pnt(5.0, 1.5, 0.0); Pnt(10.0, 2.0, 0.0)]
                expectPoints (offsetWith Offset3D.VarDistParallel.Skip [1.0; 2.0] line) [Pnt(0.0, 1.0, 0.0); Pnt(10.0, 2.0, 0.0)]
                expectPoints (offsetWith Offset3D.VarDistParallel.Fail [1.0; 1.0] line) [Pnt(0.0, 1.0, 0.0); Pnt(5.0, 1.0, 0.0); Pnt(10.0, 1.0, 0.0)]

            testCase "a smaller considerCollinearBelow offsets a shallow corner with different distances" <| fun _ ->
                let a = 2.0 * Math.PI / 180.0 // a 2 degree turn, collinear at the default of 2.5 degrees
                let pl = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0 + 10.0 * cos a, 10.0 * sin a, 0.0)]
                Expect.throws (fun () -> offsetWith Offset3D.VarDistParallel.Fail [1.0; 1.1] pl |> ignore) "collinear at the default"
                let res = Polyline3D.offsetVariableByNormalCustom(pl, ds [1.0; 1.1], zeros 3, worldZ, considerCollinearBelow = Cosine.``0.5``)
                let p = pl.AsPoints
                let r = res.AsPoints
                "corner to first segment" |> Expect.floatClose tol (Line3D.distanceRayPoint r.[1] (Line3D(p.[0], p.[1]))) 1.0
                "corner to second segment" |> Expect.floatClose tol (Line3D.distanceRayPoint r.[1] (Line3D(p.[1], p.[2]))) 1.1
                let viaKernel = Offset3D.offsetVariableByNormalCustom(pl.XYZs, ds [1.0; 1.1], zeros 3, worldZ, Offset3D.VarDistParallel.Fail, Cosine.``0.5``, Cosine.``175.0``)
                "Offset3D gives the same" |> Expect.isTrue (Polyline3D.equalsExact res (Polyline3D.createFromXYZs viaKernel))

            testCase "a collinear point takes its own perpendicular distance" <| fun _ ->
                let offsetPerp (perp: float list) (inPlane: float list) behavior (pl:Polyline3D) =
                    Polyline3D.offsetVariableByNormalCustom(pl, ds inPlane, ds perp, worldZ, varDistParallelBehavior = behavior)
                let fail = Offset3D.VarDistParallel.Fail
                let ones = [1.0; 1.0; 1.0; 1.0; 1.0]
                expectPoints (offsetPerp [0.0; 2.0; 0.0; 0.0; 0.0] ones fail square) [Pnt(1.0, 1.0, 0.0); Pnt(5.0, 1.0, 2.0); Pnt(9.0, 1.0, 0.0); Pnt(9.0, 9.0, 0.0); Pnt(1.0, 9.0, 0.0); Pnt(1.0, 1.0, 0.0)]
                // not on the line between the neighboring offset corners
                expectPoints (offsetPerp [1.0; 0.0; 3.0; 0.0; 0.0] ones fail square) [Pnt(1.0, 1.0, 1.0); Pnt(5.0, 1.0, 0.0); Pnt(9.0, 1.0, 3.0); Pnt(9.0, 9.0, 0.0); Pnt(1.0, 9.0, 0.0); Pnt(1.0, 1.0, 1.0)]
                // with different in-plane distances
                let different = [1.0; 2.0; 1.0; 1.0; 1.0]
                let prop = offsetPerp [0.0; 1.0; 0.0; 0.0; 0.0] different Offset3D.VarDistParallel.Proportional square
                expectPoints prop [Pnt(1.0, 1.0, 0.0); Pnt(5.0, 1.5, 1.0); Pnt(9.0, 2.0, 0.0); Pnt(9.0, 9.0, 0.0); Pnt(1.0, 9.0, 0.0); Pnt(1.0, 1.0, 0.0)]
                let proj = offsetPerp [0.0; 1.0; 0.0; 0.0; 0.0] different Offset3D.VarDistParallel.Project square
                expectPoints proj [Pnt(1.0, 1.0, 0.0); Pnt(5.0, 1.5, 1.0); Pnt(9.0, 2.0, 0.0); Pnt(9.0, 9.0, 0.0); Pnt(1.0, 9.0, 0.0); Pnt(1.0, 1.0, 0.0)]
                let skip = offsetPerp [0.0; 1.0; 0.0; 0.0; 0.0] different Offset3D.VarDistParallel.Skip square
                expectPoints skip [Pnt(1.0, 1.0, 0.0); Pnt(9.0, 2.0, 0.0); Pnt(9.0, 9.0, 0.0); Pnt(1.0, 9.0, 0.0); Pnt(1.0, 1.0, 0.0)]
                // at a collinear seam, the last point is the first
                let seam = offsetPerp [3.0; 0.0; 0.0; 0.0; 0.0] ones fail seamSquare
                expectPoints seam [Pnt(5.0, 1.0, 3.0); Pnt(9.0, 1.0, 0.0); Pnt(9.0, 9.0, 0.0); Pnt(1.0, 9.0, 0.0); Pnt(1.0, 1.0, 0.0); Pnt(5.0, 1.0, 3.0)]
                "seam closed" |> Expect.isTrue (Pnt.equalsExact seam.FirstPoint seam.LastPoint)
                // on a straight polyline every point has a direction from refNormal
                let straight = Polyline3D.offsetVariableByNormal worldZ (ds [1.0; 1.0]) (ds [1.0; -2.0; 3.0]) line
                expectPoints straight [Pnt(0.0, 1.0, 1.0); Pnt(5.0, 1.0, -2.0); Pnt(10.0, 1.0, 3.0)]

            testCase "a collinear point between corners in different planes interpolates their perpendicular directions" <| fun _ ->
                // the corner at pts.[1] is in the XY plane, the one at pts.[3] is tilted up, pts.[2] is collinear in the middle between them
                let pl = tilted [Pnt(0.0, 10.0, 0.0); Pnt(0.0, 0.0, 0.0); Pnt(5.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 5.0, 10.0)]
                let d = 2.0
                let res = Polyline3D.offsetVariableByNormal worldZ (ds [0.0; 0.0; 0.0; 0.0]) (ds [0.0; 0.0; d; 0.0; 0.0]) pl
                let p = pl.AsPoints
                let r = res.AsPoints
                for i in [0; 1; 3; 4] do
                    Expect.isTrue (eqPnt r.[i] p.[i]) $"pt[{i}] has no offset"
                // the perpendicular directions at the two corners, before tilting
                let n1 = Vec(0.0, 0.0, 1.0)
                let n3 = Vec(0.0, -10.0, 5.0).Unitized.AsVec
                let expected = toWorld (Pnt(5.0, 0.0, 0.0) + ((n1 + n3) * 0.5).Unitized.AsVec * d)
                Expect.isTrue (eqPnt r.[2] expected) $"pt[2] should be {expected} but is {r.[2]}"
                let v = r.[2] - p.[2]
                "own distance" |> Expect.floatClose tol v.Length d
                "perpendicular to the polyline" |> Expect.floatClose tol (v *** (p.[3] - p.[1]).Unitized) 0.0
        ]

        testList "Offset3D curves between corners" [
            let rad (deg:float) = deg * Math.PI / 180.0
            let toLocal (p:Pnt) = Pnt.rotate tilt.Conjugate (p - Vec(120.0, -40.0, 15.0))
            // a half circle sampled at 2 degrees, closed by its diameter, so it has just two sharp corners
            let dShape = tilted [ for i in 0 .. 90 -> let a = rad (2.0 * float i) in Pnt(10.0 * cos a, 10.0 * sin a, 0.0)
                                  yield Pnt(10.0, 0.0, 0.0) ]

            testCase "a fine arc between two corners keeps its shape" <| fun _ ->
                for d in [1.0; -0.5] do
                    let res = Polyline3D.offset d 0.0 dShape // before, the arc points were placed on the chord between the two corners
                    "distance to the segments" |> Expect.isTrue (maxSegmentDistanceError dShape res d < 1e-9)
                    // the miter of a vertex that turns by 2 degrees
                    Expect.isTrue (eqPnt res.AsPoints.[45] (toWorld (Pnt(0.0, 10.0 - d / cos (rad 1.0), 0.0)))) $"apex for {d}"
                let n = dShape.SegmentCount
                let v = Polyline3D.offsetVariable (Array.create n 1.0) (Array.create n 0.5) dShape
                "variable with equal distances" |> Expect.isTrue (Polyline3D.equalsWithin 1e-9 v (Polyline3D.offset 1.0 0.5 dShape))

            testCase "a fine circle stays in its plane with an approximate refNormal" <| fun _ ->
                // the circle is tilted by 30 degrees to refNormal. Before, with 1.8 degrees per point no point was a corner, and its offset left the plane
                let q = Quaternion.createFromDegreesUnitVec UnitVec.xAxis 30.0
                let normal = UnitVec.rotate tilt (UnitVec.rotate q UnitVec.zAxis)
                let center = toWorld Pnt.Origin
                for n in [36; 200] do
                    let circle = tilted [ for i in 0 .. n -> let a = 2.0 * Math.PI * float (i % n) / float n in Pnt.rotate q (Pnt(10.0 * cos a, 10.0 * sin a, 0.0)) ]
                    let res = Polyline3D.offsetByNormal worldZ 1.0 0.0 circle
                    Expect.isTrue (maxSegmentDistanceError circle res 1.0 < 1e-9) $"distance to the segments, {n} points"
                    for p in res.AsPoints do
                        Expect.isTrue (abs ((p - center) *** normal) < 1e-9) $"in the plane, {n} points"

            testCase "a finely sampled helix gets its planes from the curve" <| fun _ ->
                // 1 degree per point, so every point is collinear by itself, but the angle from the last corner accumulates
                let helix = tilted [ for i in 0 .. 360 -> let a = rad (float i) in Pnt(10.0 * cos a, 10.0 * sin a, 0.02 * float i) ]
                let res = Polyline3D.offsetByNormal worldZ 1.0 0.0 helix
                "distance to the segments" |> Expect.isTrue (maxSegmentDistanceError helix res 1.0 < 1e-6)
                for i in 1 .. 359 do
                    let p = toLocal res.AsPoints.[i]
                    Expect.isTrue (abs (sqrt (p.X * p.X + p.Y * p.Y) - 9.0) < 1e-3) $"pt[{i}] towards the axis"

            testCase "a jittery straight run keeps the plane of its corners" <| fun _ ->
                // the segments between the two corners deviate by less than 2.5 degrees from the first one, so their points take the plane of the corners
                let pl = tilted [Pnt(0.0, 10.0, 0.0); Pnt(0.0, 0.0, 0.0); Pnt(2.0, 0.0, 0.01); Pnt(4.0, 0.0, -0.015); Pnt(6.0, 0.0, 0.02); Pnt(8.0, 0.0, -0.01); Pnt(10.0, 0.0, 0.0); Pnt(12.0, 0.0, 0.0); Pnt(12.0, 10.0, 0.0)]
                let res = Polyline3D.offsetByNormal worldZ 0.0 1.0 pl
                for i = 0 to pl.PointCount - 1 do
                    let v = res.AsPoints.[i] - pl.AsPoints.[i]
                    Expect.isTrue (v *** worldZ > float Cosine.``2.5``) $"pt[{i}] moves within 2.5 degrees of the normal of the corners: {v}"
        ]

        testList "Offset3D orientation" [
            // X, Y, Z, then X again: the planes of consecutive bends are perpendicular
            let route = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 10.0, 0.0); Pnt(10.0, 10.0, 10.0); Pnt(20.0, 10.0, 10.0)]
            // a closed path along six edges of a cube, all its bends are in perpendicular planes
            let hexagon = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 10.0, 0.0); Pnt(10.0, 10.0, 10.0); Pnt(0.0, 10.0, 10.0); Pnt(0.0, 0.0, 10.0); Pnt(0.0, 0.0, 0.0)]
            let spiral = tilted [Pnt(10.0, 0.0, 0.0); Pnt(0.0, 10.0, 5.0); Pnt(-10.0, 0.0, 10.0); Pnt(0.0, -10.0, 15.0); Pnt(10.0, 0.0, 20.0)]
            let rotate (q:Quaternion) (pl:Polyline3D) = Polyline3D.createFromPnts [ for p in pl.AsPoints -> Pnt.rotate q p ]

            testCase "rotating the polyline rotates the offset, also by tiny angles" <| fun _ ->
                // before, the side of the offset at bends in perpendicular planes depended on rounding
                let axis = UnitVec.create 3.0 -1.0 2.0
                for deg in [1e-7; 37.0; 37.0000001; 80.0; 211.0] do
                    let q = Quaternion.createFromDegreesUnitVec axis deg
                    for pl in [route; hexagon; spiral] do
                        let a = Polyline3D.offsetByNormal (UnitVec.rotate q worldZ) 1.0 0.5 (rotate q pl)
                        let b = rotate q (Polyline3D.offsetByNormal worldZ 1.0 0.5 pl)
                        Expect.isTrue (Polyline3D.equalsWithin 1e-9 a b) $"offsetByNormal, rotated by {deg} degrees: {pl}"
                        let c = Polyline3D.offset 1.0 0.5 (rotate q pl)
                        let d = rotate q (Polyline3D.offset 1.0 0.5 pl)
                        Expect.isTrue (Polyline3D.equalsWithin 1e-9 c d) $"offset, rotated by {deg} degrees: {pl}"

            testCase "an orthogonal route keeps the side of each bend" <| fun _ ->
                // the first bend is oriented by refNormal, the next ones by the in-plane direction of the bend before
                let res = Polyline3D.offsetByNormal worldZ 0.0 1.0 route
                let expected = [Vec(0.0, 0.0, 1.0); Vec(0.0, 0.0, 1.0); Vec(-1.0, 0.0, 0.0); Vec(0.0, 1.0, 0.0); Vec(0.0, 1.0, 0.0)]
                for i = 0 to 4 do
                    let v = Vec.rotate tilt.Conjugate (res.AsPoints.[i] - route.AsPoints.[i])
                    Expect.isTrue (Vec.equalsWithin 1e-9 v expected.[i]) $"pt[{i}] should move along {expected.[i]} but moves along {v}"

            testCase "reversing an orthogonal route and its normal gives the reversed result" <| fun _ ->
                let reversed (pl:Polyline3D) = Polyline3D.createFromPnts (Seq.rev pl.AsPoints)
                let a = Polyline3D.offsetByNormal worldZ 1.0 0.5 route
                let b = Polyline3D.offsetByNormal -worldZ 1.0 -0.5 (reversed route)
                "reversed" |> Expect.isTrue (Polyline3D.equalsWithin 1e-9 a (reversed b))

            testCase "fails with a clear message if refNormal is perpendicular to every corner" <| fun _ ->
                let pl = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 10.0, 0.0)]
                let msg = try Polyline3D.offsetByNormal (UnitVec.rotate tilt UnitVec.xAxis) 1.0 0.0 pl |> ignore; "" with e -> e.Message
                "message" |> Expect.stringContains msg "is perpendicular to the plane of every corner"
        ]

        testList "Offset3D variable offsets like in 2D" [
            let rad (deg:float) = deg * Math.PI / 180.0
            let ds (xs: float list) = ResizeArray xs
            let zeros n = ResizeArray (List.replicate n 0.0)
            let inWorld (p:Pt) = toWorld (Pnt(p.X, p.Y, 0.0))
            let behaviors = [ Offset2D.VarDistParallel.Proportional, Offset3D.VarDistParallel.Proportional
                              Offset2D.VarDistParallel.Project, Offset3D.VarDistParallel.Project
                              Offset2D.VarDistParallel.Skip, Offset3D.VarDistParallel.Skip ]
            let expectLike2D (msg:string) (r2:Polyline2D) (r3:Polyline3D) =
                Expect.equal r3.PointCount r2.PointCount $"{msg} point count"
                for i = 0 to r2.PointCount - 1 do
                    Expect.isTrue (eqPnt r3.AsPoints.[i] (inWorld r2.AsPoints.[i])) $"{msg} pt[{i}] should be {inWorld r2.AsPoints.[i]} but is {r3.AsPoints.[i]}"

            testCase "a run with equal and different distances gives the same points as in 2D" <| fun _ ->
                // before, the point between two segments of distance 1 was placed on the line between the corners, at 1.108
                let pts2D = [Pt(0.0, 0.0); Pt(2.0, 0.0); Pt(5.0, 0.0); Pt(10.0, 0.0); Pt(10.0, 10.0); Pt(0.0, 10.0); Pt(0.0, 0.0)]
                let dists = [1.0; 1.0; 2.0; 1.0; 1.0; 1.0]
                let pl = tilted [ for p in pts2D -> Pnt(p.X, p.Y, 0.0) ]
                for b2, b3 in behaviors do
                    let r2 = Polyline2D.offsetVariableCustom(Polyline2D.createFromPts pts2D, ds dists, varDistParallelBehavior = b2)
                    let r3 = Polyline3D.offsetVariableByNormalCustom(pl, ds dists, zeros 6, worldZ, varDistParallelBehavior = b3)
                    expectLike2D $"{b3}" r2 r3
                    if b3 <> Offset3D.VarDistParallel.Skip then
                        "pt[1] is between two segments of distance 1" |> Expect.isTrue (eqPnt r3.AsPoints.[1] (toWorld (Pnt(2.0, 1.0, 0.0))))
                Expect.throws (fun () -> Polyline3D.offsetVariableByNormal worldZ (ds dists) (zeros 6) pl |> ignore) "Fail"

            testCase "a fine arc with a different distance on every segment keeps its shape" <| fun _ ->
                // 2 degrees per point, so every point is collinear by itself, but the angle from the last exact point accumulates.
                // Before, Proportional placed all points on the chord between the end points, in 2D too.
                let n = 45
                let dists = [ for i in 0 .. n - 1 -> 1.0 + 0.01 * float i ]
                let pts2D = [ for i in 0 .. n -> let a = rad (2.0 * float i) in Pt(10.0 * cos a, 10.0 * sin a) ]
                let pl = tilted [ for p in pts2D -> Pnt(p.X, p.Y, 0.0) ]
                for b2, b3 in behaviors do
                    let r2 = Polyline2D.offsetVariableCustom(Polyline2D.createFromPts pts2D, ds dists, checkOrientation = false, varDistParallelBehavior = b2)
                    let r3 = Polyline3D.offsetVariableByNormalCustom(pl, ds dists, zeros (n + 1), worldZ, varDistParallelBehavior = b3)
                    expectLike2D $"{b3}" r2 r3
                    for p in r3.AsPoints do
                        let r = Pnt.distanceTo p (toWorld Pnt.Origin)
                        Expect.isTrue (r > 8.5 && r < 9.01) $"{b3}: radius {r}"

            testCase "a closed fine circle with a different distance on every segment can be offset" <| fun _ ->
                // before, this failed in 3D because no point was a corner
                let n = 180
                let dists = [ for i in 0 .. n - 1 -> 1.0 + 0.2 * sin (2.0 * Math.PI * float i / float n) ]
                let pts2D = [ for i in 0 .. n -> let a = 2.0 * Math.PI * float (i % n) / float n in Pt(10.0 * cos a, 10.0 * sin a) ]
                let pl = tilted [ for p in pts2D -> Pnt(p.X, p.Y, 0.0) ]
                let r2 = Polyline2D.offsetVariableCustom(Polyline2D.createFromPts pts2D, ds dists, varDistParallelBehavior = Offset2D.VarDistParallel.Proportional)
                let r3 = Polyline3D.offsetVariableCustom(pl, ds dists, zeros n, varDistParallelBehavior = Offset3D.VarDistParallel.Proportional)
                expectLike2D "Proportional" r2 r3
                "closed" |> Expect.isTrue (Pnt.equalsExact r3.FirstPoint r3.LastPoint)
                for i = 0 to n - 1 do
                    let r = Pnt.distanceTo r3.AsPoints.[i] (toWorld Pnt.Origin)
                    Expect.isTrue (abs (r - (10.0 - dists.[i])) < 0.05) $"pt[{i}] radius {r}"

            testCase "a teardrop with a different distance on every segment of the arc can be offset" <| fun _ ->
                // before, Proportional failed with an error about a line that is too short, because the tip was the only corner
                let tear = tilted [ yield Pnt(20.0, 0.0, 0.0)
                                    for i in 0 .. 120 -> let a = rad (60.0 + 2.0 * float i) in Pnt(10.0 * cos a, 10.0 * sin a, 0.0)
                                    yield Pnt(20.0, 0.0, 0.0) ]
                let n = tear.SegmentCount
                let dists = ds [ for i in 0 .. n - 1 -> 1.0 + 0.001 * float i ]
                let res = Polyline3D.offsetVariableByNormalCustom(tear, dists, zeros n, worldZ, varDistParallelBehavior = Offset3D.VarDistParallel.Proportional)
                "point count" |> Expect.equal res.PointCount tear.PointCount
                "closed" |> Expect.isTrue (Pnt.equalsExact res.FirstPoint res.LastPoint)

            testCase "constant and variable offsets agree at a collinear point between corners in different planes" <| fun _ ->
                // the corner at pts.[1] is in the XY plane, the one at pts.[3] is tilted up, pts.[2] is collinear between them
                let pl = tilted [Pnt(0.0, 10.0, 0.0); Pnt(0.0, 0.0, 0.0); Pnt(5.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 5.0, 10.0)]
                let c = Polyline3D.offsetByNormal worldZ 1.0 1.0 pl
                let v = Polyline3D.offsetVariableByNormal worldZ (ds [1.0; 1.0; 1.0; 1.0]) (ds [1.0; 1.0; 1.0; 1.0; 1.0]) pl
                "same" |> Expect.isTrue (Polyline3D.equalsWithin 1e-9 c v)
                let ln = Line3D(pl.AsPoints.[1], pl.AsPoints.[3])
                "at its own distances" |> Expect.floatClose tol (Line3D.distanceRayPoint c.AsPoints.[2] ln) (sqrt 2.0)
                let inPlane = Polyline3D.offsetByNormal worldZ 1.0 0.0 pl
                "in-plane distance" |> Expect.floatClose tol (Line3D.distanceRayPoint inPlane.AsPoints.[2] ln) 1.0
        ]

        testList "Offset3D edge cases" [
            let rad (deg:float) = deg * Math.PI / 180.0

            testCase "an almost closed polyline gives an exactly closed offset" <| fun _ ->
                let pl = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 10.0, 0.0); Pnt(0.0, 10.0, 0.0); Pnt(5e-7, 0.0, 0.0)]
                let c = Polyline3D.offset 1.0 0.5 pl
                "constant" |> Expect.isTrue (Pnt.equalsExact c.FirstPoint c.LastPoint)
                let v = Polyline3D.offsetVariable [|1.0; 2.0; 1.0; 2.0|] [|0.5; 0.5; 0.5; 0.5|] pl
                "variable" |> Expect.isTrue (Pnt.equalsExact v.FirstPoint v.LastPoint)

            testCase "the average normal check is the one of Polyline3D.AverageNormal" <| fun _ ->
                // before, a square of 0.005 units failed as having no area
                for s in [0.005; 0.001; 0.0005] do
                    let sq = tilted [Pnt(0.0, 0.0, 0.0); Pnt(s, 0.0, 0.0); Pnt(s, s, 0.0); Pnt(0.0, s, 0.0); Pnt(0.0, 0.0, 0.0)]
                    let works f = try f () |> ignore; true with _ -> false
                    Expect.equal (works (fun () -> Polyline3D.offset (s * 0.1) 0.0 sq)) sq.TryAverageNormal.IsSome $"Polyline3D.offset, size {s}"
                    Expect.equal (works (fun () -> Offset3D.offset (s * 0.1) 0.0 sq.XYZs)) sq.TryAverageNormal.IsSome $"Offset3D.offset, size {s}"

            testCase "very short segments are offset exactly, duplicate points fail" <| fun _ ->
                let pl = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 2e-6, 0.0); Pnt(15.0, 8.0, 0.0)]
                let res = Polyline3D.offsetByNormal worldZ 1.0 0.0 pl
                "distance to the segments" |> Expect.isTrue (maxSegmentDistanceError pl res 1.0 < 1e-6)
                let dup = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 5e-7, 0.0); Pnt(15.0, 8.0, 0.0)]
                let msg = try Polyline3D.offsetByNormal worldZ 1.0 0.0 dup |> ignore; "" with e -> e.Message
                "duplicate" |> Expect.stringContains msg "point[2] and point[1] are the same"

            testCase "turns just below and above 2.5 degrees are offset exactly" <| fun _ ->
                for deg in [2.4; 2.6] do
                    let angles = [0.0; deg; 0.0; deg; 0.0; deg]
                    let pts = angles |> List.scan (fun (p:Pnt) a -> p + Vec(10.0 * cos (rad a), 10.0 * sin (rad a), 0.0)) Pnt.Origin
                    let pl = tilted pts
                    let res = Polyline3D.offsetByNormal worldZ 1.0 0.5 pl
                    Expect.isTrue (maxSegmentDistanceError pl res (sqrt 1.25) < 1e-9) $"distance to the segments at {deg} degrees"
                    let v = Polyline3D.offsetVariableByNormal worldZ (Array.create 6 1.0) (Array.create 7 0.5) pl
                    Expect.isTrue (Polyline3D.equalsWithin 1e-9 res v) $"variable at {deg} degrees"
        ]

        testList "Offset3D module functions" [
            let rad (deg:float) = deg * Math.PI / 180.0
            let pl = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 3.0); Pnt(10.0, 10.0, 0.0); Pnt(5.0, 12.0, 1.0); Pnt(0.0, 10.0, 3.0); Pnt(0.0, 0.0, 0.0)]
            let same (a: ResizeArray<float>) (b:Polyline3D) msg = Expect.isTrue (Polyline3D.equalsWithin 1e-12 (Polyline3D.createFromXYZs a) b) msg

            testCase "the Offset3D functions give the same results as the Polyline3D members" <| fun _ ->
                let c1, u170 = Cosine.``1.0``, Cosine.``170.0``
                same (Offset3D.offset 1.0 0.5 pl.XYZs) (Polyline3D.offset 1.0 0.5 pl) "offset"
                same (Offset3D.offsetByNormal worldZ 1.0 0.5 pl.XYZs) (Polyline3D.offsetByNormal worldZ 1.0 0.5 pl) "offsetByNormal"
                same (Offset3D.offsetCustom(pl.XYZs, 1.0, 0.5, c1, u170)) (Polyline3D.offsetCustom(pl, 1.0, 0.5, considerCollinearBelow = c1, failAtUTurnAbove = u170)) "offsetCustom"
                same (Offset3D.offsetByNormalCustom(pl.XYZs, 1.0, 0.5, worldZ, c1, u170)) (Polyline3D.offsetByNormalCustom(pl, 1.0, 0.5, worldZ, considerCollinearBelow = c1, failAtUTurnAbove = u170)) "offsetByNormalCustom"
                let ins = [| 0.5; 1.0; 1.5; 1.0; 0.5 |]
                let perps = [| 0.1; 0.2; 0.3; 0.4; 0.5 |]
                same (Offset3D.offsetVariable ins perps pl.XYZs) (Polyline3D.offsetVariable ins perps pl) "offsetVariable"
                same (Offset3D.offsetVariableByNormal worldZ ins perps pl.XYZs) (Polyline3D.offsetVariableByNormal worldZ ins perps pl) "offsetVariableByNormal"
                let prop = Offset3D.VarDistParallel.Proportional
                same (Offset3D.offsetVariableCustom(pl.XYZs, ins, perps, prop, c1, u170)) (Polyline3D.offsetVariableCustom(pl, ins, perps, varDistParallelBehavior = prop, considerCollinearBelow = c1, failAtUTurnAbove = u170)) "offsetVariableCustom"
                same (Offset3D.offsetVariableByNormalCustom(pl.XYZs, ins, perps, worldZ, prop, c1, u170)) (Polyline3D.offsetVariableByNormalCustom(pl, ins, perps, worldZ, varDistParallelBehavior = prop, considerCollinearBelow = c1, failAtUTurnAbove = u170)) "offsetVariableByNormalCustom"
                let msg = try Offset3D.offsetCustom(ResizeArray [1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0], 1.0, 0.0, c1, u170) |> ignore; "" with e -> e.Message
                "offsetCustom without area" |> Expect.stringContains msg "Use offsetByNormalCustom with an explicit reference normal"

            testCase "failAtUTurnAbove applies to the variable offsets" <| fun _ ->
                let uTurn = tilted [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0 + 10.0 * cos (rad 177.0), 10.0 * sin (rad 177.0), 0.0)]
                Expect.throws (fun () -> Polyline3D.offsetVariableByNormal worldZ [| 1.0; 2.0 |] [| 0.0; 0.0; 0.0 |] uTurn |> ignore) "fails by default"
                let res = Polyline3D.offsetVariableByNormalCustom(uTurn, [| 1.0; 2.0 |], [| 0.0; 0.0; 0.0 |], worldZ, failAtUTurnAbove = Cosine.``179.0``)
                let p = uTurn.AsPoints
                "corner to first segment" |> Expect.floatClose tol (Line3D.distanceRayPoint res.AsPoints.[1] (Line3D(p.[0], p.[1]))) 1.0
                "corner to second segment" |> Expect.floatClose tol (Line3D.distanceRayPoint res.AsPoints.[1] (Line3D(p.[1], p.[2]))) 2.0
        ]

        // ============ Write test data file ============

        testCase "Write test data file (when saveTestData=true)" <| fun _ ->
            writeTestDataFile ()
            Expect.isTrue true "Test data file written if saveTestData was enabled"

        testList "With forms keep the old settings" [
            test "offsetVariableByNormal uses VarDistParallel.Fail" {
                let pts = ResizeArray [ Pnt(12.0, -7.0, 3.0); Pnt(20.0, -1.0, 3.5); Pnt(17.0, 3.0, 4.0); Pnt(9.0, -3.0, 3.5) ]
                let xyzs = ResizeArray<float>()
                for p in pts do
                    xyzs.Add p.X; xyzs.Add p.Y; xyzs.Add p.Z
                let n = UnitVec.zAxis
                let a = ResizeArray [ 0.5; 1.0; 0.75 ]
                let b = ResizeArray [ 0.1; 0.2; 0.3; 0.4 ] // one per point of the open polyline
                let r1 = Offset3D.offsetVariableByNormal n a b xyzs
                let r2 = Offset3D.offsetVariableByNormalCustom(xyzs, a, b, n, Offset3D.VarDistParallel.Fail, Cosine.``2.5``, Cosine.``175.0``)
                "same count" |> Expect.equal r1.Count r2.Count
                for i = 0 to r1.Count - 1 do
                    "same values" |> Expect.isTrue (r1.[i] = r2.[i])
                let pl = Polyline3D.createFromPnts pts
                let p1 = Polyline3D.offsetByNormal n 0.5 0.1 pl
                let p2 = Polyline3D.offsetByNormalCustom(pl, 0.5, 0.1, n, loop = false)
                "Polyline3D.offsetByNormal" |> Expect.isTrue (Polyline3D.equalsExact p1 p2)
            }
        ]
    ]
