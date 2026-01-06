module TestOffset3D

open System
open System.IO
open System.Text
open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let inline eqPnt a b = Pnt.distance a b < 1e-6

let tol = Accuracy.veryHigh

// ============ Test data recording ============

/// Set to true to save input/output pairs to an F# file during test runs
let saveTestData = false

/// Path for the generated F# file with test data
let testDataFilePath =
    #if FABLE_COMPILER
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

        testCase "Polyline3D.offset open L-shape in XY plane with Z-up normal" <| fun _ ->
            // L-shape: (0,0,0) -> (10,0,0) -> (10,10,0)
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let inPlane, perp = 1.0, 0.0
            let result = Polyline3D.offsetWithRef(pl, inPlane, perp, refNormal)
            recordTestCase "open L-shape in XY plane" pl inPlane perp refNormal result
            // With inPlane offset of 1.0 and Z-up normal, offset should go to the "inside" of the L
            // Start point: offset perpendicular to first segment (along +Y)
            // Corner point: bisector offset
            // End point: offset perpendicular to last segment (along -X)
            Expect.equal result.PointCount 3 "Should have 3 points"
            Expect.isTrue (eqPnt result.Points.[0] (Pnt(0, 1, 0))) $"Start point should be Pnt(0, 1, 0), got {result.Points.[0]}"
            Expect.isTrue (eqPnt result.Points.[1] (Pnt(9, 1, 0))) $"Corner should be Pnt(9, 1, 0), got {result.Points.[1]}"
            Expect.isTrue (eqPnt result.Points.[2] (Pnt(9, 10, 0))) $"End point should be Pnt(9, 10, 0), got {result.Points.[2]}"

        testCase "Polyline3D.offset open L-shape with perpendicular offset" <| fun _ ->
            // L-shape: (0,0,0) -> (10,0,0) -> (10,10,0)
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let inPlane, perp = 0.0, 2.0
            let result = Polyline3D.offsetWithRef(pl, inPlane, perp, refNormal)
            recordTestCase "open L-shape with perpendicular offset" pl inPlane perp refNormal result
            // With only perpendicular offset of 2.0 and Z-up normal, all points should move up in Z
            Expect.equal result.PointCount 3 "Should have 3 points"
            Expect.isTrue (eqPnt result.Points.[0] (Pnt(0, 0, 2))) $"Start point should be at Z=2, got {result.Points.[0]}"
            Expect.isTrue (eqPnt result.Points.[1] (Pnt(10, 0, 2))) $"Corner should be at Z=2, got {result.Points.[1]}"
            Expect.isTrue (eqPnt result.Points.[2] (Pnt(10, 10, 2))) $"End point should be at Z=2, got {result.Points.[2]}"

        testCase "Polyline3D.offset open straight line (colinear)" <| fun _ ->
            // Straight line: all points colinear
            let pts = ResizeArray [Pnt(0,0,0); Pnt(5,0,0); Pnt(10,0,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let inPlane, perp = 1.0, 0.0
            let result = Polyline3D.offsetWithRef(pl, inPlane, perp, refNormal)
            recordTestCase "open straight line (colinear)" pl inPlane perp refNormal result
            // Colinear points should all be offset in the same direction (perpendicular to line, in plane with refNormal)
            Expect.equal result.PointCount 3 "Should have 3 points"
            Expect.isTrue (eqPnt result.Points.[0] (Pnt(0, 1, 0))) $"Start should be at Y=1, got {result.Points.[0]}"
            Expect.isTrue (eqPnt result.Points.[1] (Pnt(5, 1, 0))) $"Middle should be at Y=1, got {result.Points.[1]}"
            Expect.isTrue (eqPnt result.Points.[2] (Pnt(10, 1, 0))) $"End should be at Y=1, got {result.Points.[2]}"

        testCase "Polyline3D.offset closed triangle" <| fun _ ->
            // Equilateral-ish triangle in XY plane, closed (first = last)
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(5,8.66,0); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let inPlane, perp = 1.0, 0.0
            let result = Polyline3D.offsetWithRef(pl, inPlane, perp, refNormal)
            recordTestCase "closed triangle" pl inPlane perp refNormal result
            // Closed polygon should remain closed after offset
            Expect.equal result.PointCount 4 "Should have 4 points (closed)"
            Expect.isTrue (eqPnt result.Points.[0] result.Points.[3]) $"Should remain closed, start={result.Points.[0]}, end={result.Points.[3]}"

        testCase "Polyline3D.offset closed square inward" <| fun _ ->
            // Square in XY plane, CCW when viewed from +Z
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let inPlane, perp = 1.0, 0.0
            let result = Polyline3D.offsetWithRef(pl, inPlane, perp, refNormal)
            recordTestCase "closed square inward" pl inPlane perp refNormal result
            // Inward offset of 1.0 should shrink the square
            Expect.equal result.PointCount 5 "Should have 5 points (closed)"
            Expect.isTrue (eqPnt result.Points.[0] (Pnt(1, 1, 0))) $"Corner 0 should be at (1,1,0), got {result.Points.[0]}"
            Expect.isTrue (eqPnt result.Points.[1] (Pnt(9, 1, 0))) $"Corner 1 should be at (9,1,0), got {result.Points.[1]}"
            Expect.isTrue (eqPnt result.Points.[2] (Pnt(9, 9, 0))) $"Corner 2 should be at (9,9,0), got {result.Points.[2]}"
            Expect.isTrue (eqPnt result.Points.[3] (Pnt(1, 9, 0))) $"Corner 3 should be at (1,9,0), got {result.Points.[3]}"

        testCase "Polyline3D.offset closed square outward" <| fun _ ->
            // Square in XY plane, CCW when viewed from +Z
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let inPlane, perp = -1.0, 0.0
            let result = Polyline3D.offsetWithRef(pl, inPlane, perp, refNormal)
            recordTestCase "closed square outward" pl inPlane perp refNormal result
            // Outward offset of -1.0 should expand the square
            Expect.equal result.PointCount 5 "Should have 5 points (closed)"
            Expect.isTrue (eqPnt result.Points.[0] (Pnt(-1, -1, 0))) $"Corner 0 should be at (-1,-1,0), got {result.Points.[0]}"
            Expect.isTrue (eqPnt result.Points.[1] (Pnt(11, -1, 0))) $"Corner 1 should be at (11,-1,0), got {result.Points.[1]}"
            Expect.isTrue (eqPnt result.Points.[2] (Pnt(11, 11, 0))) $"Corner 2 should be at (11,11,0), got {result.Points.[2]}"
            Expect.isTrue (eqPnt result.Points.[3] (Pnt(-1, 11, 0))) $"Corner 3 should be at (-1,11,0), got {result.Points.[3]}"

        testCase "Polyline3D.offset with loop=true on open polyline" <| fun _ ->
            // Open L-shape that should be treated as closed loop
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let result = Polyline3D.offsetWithRef(pl, 1.0, 0.0, refNormal, loop=true)
            recordTestCase "open L-shape with loop=true" pl 1.0 0.0 refNormal result
            // With loop=true, the polyline is treated as if first and last are connected
            Expect.equal result.PointCount 3 "Should still have 3 points (loop closes and opens)"

        testCase "Polyline3D.offset 3D polyline not in XY plane" <| fun _ ->
            // L-shape in XZ plane
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,0,10)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Yaxis
            let result = Polyline3D.offsetWithRef(pl, 1.0, 0.0, refNormal)
            recordTestCase "L-shape in XZ plane" pl 1.0 0.0 refNormal result
            // With Y-up reference normal for XZ plane L-shape
            Expect.equal result.PointCount 3 "Should have 3 points"

        testCase "Polyline3D.offset combined in-plane and perpendicular" <| fun _ ->
            // Simple 2-point line along X axis
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let result = Polyline3D.offsetWithRef(pl, 2.0, 3.0, refNormal)
            recordTestCase "combined in-plane and perpendicular" pl 2.0 3.0 refNormal result
            // In-plane offset of 2.0 should move in +Y, perpendicular offset of 3.0 should move in +Z
            Expect.equal result.PointCount 2 "Should have 2 points"
            Expect.isTrue (eqPnt result.Points.[0] (Pnt(0, 2, 3))) $"Start should be at (0,2,3), got {result.Points.[0]}"
            Expect.isTrue (eqPnt result.Points.[1] (Pnt(10, 2, 3))) $"End should be at (10,2,3), got {result.Points.[1]}"

        testCase "Polyline3D.offset zero distances returns original shape" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let result = Polyline3D.offsetWithRef(pl, 0.0, 0.0, refNormal)
            recordTestCase "zero distances returns original" pl 0.0 0.0 refNormal result
            Expect.equal result.PointCount 3 "Should have 3 points"
            Expect.isTrue (eqPnt result.Points.[0] pts.[0]) "Point 0 should be unchanged"
            Expect.isTrue (eqPnt result.Points.[1] pts.[1]) "Point 1 should be unchanged"
            Expect.isTrue (eqPnt result.Points.[2] pts.[2]) "Point 2 should be unchanged"

        testCase "Polyline3D.offset minimum 2 points" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let result = Polyline3D.offsetWithRef(pl, 1.0, 0.0, refNormal)
            recordTestCase "minimum 2 points" pl 1.0 0.0 refNormal result
            Expect.equal result.PointCount 2 "Should have 2 points"

        testCase "Polyline3D.offset fails with less than 2 points" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            Expect.throws (fun () -> Polyline3D.offsetWithRef(pl, 1.0, 0.0, refNormal) |> ignore) "Should fail with less than 2 points"

        testCase "Polyline3D.offset negative perpendicular offset" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let result = Polyline3D.offsetWithRef(pl, 0.0, -2.0, refNormal)
            recordTestCase "negative perpendicular offset" pl 0.0 -2.0 refNormal result
            // Negative perpendicular offset should go opposite to refNormal (down in Z)
            Expect.isTrue (eqPnt result.Points.[0] (Pnt(0, 0, -2))) $"Start should be at Z=-2, got {result.Points.[0]}"
            Expect.isTrue (eqPnt result.Points.[1] (Pnt(10, 0, -2))) $"End should be at Z=-2, got {result.Points.[1]}"

        // ============ Roundtrip tests ============

        testCase "Polyline3D.offset positive then negative returns to original (closed square)" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let d = 1.0
            let offset1 = Polyline3D.offsetWithRef(pl, d, 0.0, refNormal)
            let offset2 = Polyline3D.offsetWithRef(offset1, -d, 0.0, refNormal)
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.Points.[i] pts.[i]) $"pt[{i}] returns to original after +d then -d"

        testCase "Polyline3D.offset negative then positive returns to original (closed square)" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let d = 1.5
            let offset1 = Polyline3D.offsetWithRef(pl, -d, 0.0, refNormal)
            let offset2 = Polyline3D.offsetWithRef(offset1, d, 0.0, refNormal)
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.Points.[i] pts.[i]) $"pt[{i}] returns to original after -d then +d"

        testCase "Polyline3D.offset positive then negative returns to original (open L-shape)" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let d = 1.0
            let offset1 = Polyline3D.offsetWithRef(pl, d, 0.0, refNormal)
            let offset2 = Polyline3D.offsetWithRef(offset1, -d, 0.0, refNormal)
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.Points.[i] pts.[i]) $"pt[{i}] returns to original after +d then -d (open L-shape)"

        testCase "Polyline3D.offset positive then negative returns to original (triangle)" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(5,8.66,0); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let d = 0.5
            let offset1 = Polyline3D.offsetWithRef(pl, d, 0.0, refNormal)
            let offset2 = Polyline3D.offsetWithRef(offset1, -d, 0.0, refNormal)
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (triangle)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.Points.[i] pts.[i]) $"pt[{i}] returns to original (triangle)"

        testCase "Polyline3D.offset roundtrip with perpendicular offset" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let d = 2.0
            let offset1 = Polyline3D.offsetWithRef(pl, 0.0, d, refNormal)
            let offset2 = Polyline3D.offsetWithRef(offset1, 0.0, -d, refNormal)
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.Points.[i] pts.[i]) $"pt[{i}] returns to original after perpendicular roundtrip"

        testCase "Polyline3D.offset roundtrip with combined offsets" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let dIn = 1.0
            let dPerp = 2.0
            let offset1 = Polyline3D.offsetWithRef(pl, dIn, dPerp, refNormal)
            let offset2 = Polyline3D.offsetWithRef(offset1, -dIn, -dPerp, refNormal)
            Expect.equal offset2.PointCount pts.Count "same count after combined roundtrip"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.Points.[i] pts.[i]) $"pt[{i}] returns to original after combined roundtrip"

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
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let d = 1.0
            let offset1 = Polyline3D.offsetWithRef(pl, d, 0.0, refNormal)
            let offset2 = Polyline3D.offsetWithRef(offset1, -d, 0.0, refNormal)
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (pentagon)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.Points.[i] pts.[i]) $"pt[{i}] returns to original (pentagon)"

        testCase "Polyline3D.offset hexagon roundtrip" <| fun _ ->
            // Regular hexagon in XY plane
            let r = 10.0
            let pts = ResizeArray [
                for i = 0 to 5 do
                    let angle = float i * Math.PI / 3.0
                    Pnt(r * cos angle, r * sin angle, 0.0)
            ]
            pts.Add(pts.[0]) // close
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let inPlane, perp = 1.5, 0.0
            let offset1 = Polyline3D.offsetWithRef(pl, inPlane, perp, refNormal)
            recordTestCase "hexagon" pl inPlane perp refNormal offset1
            let offset2 = Polyline3D.offsetWithRef(offset1, -inPlane, perp, refNormal)
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (hexagon)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.Points.[i] pts.[i]) $"pt[{i}] returns to original (hexagon)"

        // ============ Open polyline patterns ============

        testCase "Polyline3D.offset open zigzag roundtrip" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,10,0); Pnt(20,0,0); Pnt(30,10,0); Pnt(40,0,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let d = 1.0
            let offset1 = Polyline3D.offsetWithRef(pl, d, 0.0, refNormal)
            let offset2 = Polyline3D.offsetWithRef(offset1, -d, 0.0, refNormal)
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (zigzag)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.Points.[i] pts.[i]) $"pt[{i}] returns to original (zigzag)"

        testCase "Polyline3D.offset open staircase pattern" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(20,10,0); Pnt(20,20,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let d = 1.0
            let offset1 = Polyline3D.offsetWithRef(pl, d, 0.0, refNormal)
            let offset2 = Polyline3D.offsetWithRef(offset1, -d, 0.0, refNormal)
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (staircase)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.Points.[i] pts.[i]) $"pt[{i}] returns to original (staircase)"

        // ============ Colinear points ============

        testCase "Polyline3D.offset with colinear points in middle" <| fun _ ->
            // Square with an extra colinear point on one edge
            let pts = ResizeArray [Pnt(0,0,0); Pnt(5,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let d = 1.0
            let offset1 = Polyline3D.offsetWithRef(pl, d, 0.0, refNormal)
            recordTestCase "square with colinear point" pl d 0.0 refNormal offset1
            let offset2 = Polyline3D.offsetWithRef(offset1, -d, 0.0, refNormal)
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (colinear)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.Points.[i] pts.[i]) $"pt[{i}] returns to original (colinear)"

        testCase "Polyline3D.offset open line with multiple colinear points" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(5,0,0); Pnt(10,0,0); Pnt(15,0,0); Pnt(20,0,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let result = Polyline3D.offsetWithRef(pl, 2.0, 0.0, refNormal)
            recordTestCase "open line with multiple colinear points" pl 2.0 0.0 refNormal result
            Expect.equal result.PointCount 5 "Should have 5 points"
            // All points should be offset by 2 in Y direction
            for i = 0 to result.PointCount - 1 do
                Expect.isTrue (eqPnt result.Points.[i] (Pnt(pts.[i].X, 2.0, 0.0))) $"pt[{i}] should be at Y=2"

        // ============ Different planes ============

        testCase "Polyline3D.offset in XZ plane" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,0,10); Pnt(0,0,10); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Yaxis
            let inPlane, perp = 1.0, 0.0
            let offset1 = Polyline3D.offsetWithRef(pl, inPlane, perp, refNormal)
            recordTestCase "XZ plane square" pl inPlane perp refNormal offset1
            let offset2 = Polyline3D.offsetWithRef(offset1, -inPlane, perp, refNormal)
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (XZ plane)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.Points.[i] pts.[i]) $"pt[{i}] returns to original (XZ plane)"

        testCase "Polyline3D.offset in YZ plane" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(0,10,0); Pnt(0,10,10); Pnt(0,0,10); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Xaxis
            let inPlane, perp = 1.0, 0.0
            let offset1 = Polyline3D.offsetWithRef(pl, inPlane, perp, refNormal)
            recordTestCase "YZ plane square" pl inPlane perp refNormal offset1
            let offset2 = Polyline3D.offsetWithRef(offset1, -inPlane, perp, refNormal)
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (YZ plane)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.Points.[i] pts.[i]) $"pt[{i}] returns to original (YZ plane)"

        testCase "Polyline3D.offset tilted plane (45 deg)" <| fun _ ->
            // Triangle in a tilted plane
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,10); Pnt(5,8.66,5); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            // Normal perpendicular to the tilted plane
            let refNormal = UnitVec.create(1.0, 0.0, -1.0)
            let inPlane, perp = 0.5, 0.0
            let offset1 = Polyline3D.offsetWithRef(pl, inPlane, perp, refNormal)
            recordTestCase "tilted plane triangle" pl inPlane perp refNormal offset1
            let offset2 = Polyline3D.offsetWithRef(offset1, -inPlane, perp, refNormal)
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (tilted plane)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.Points.[i] pts.[i]) $"pt[{i}] returns to original (tilted plane)"

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
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let d = 1.0
            let result = Polyline3D.offsetWithRef(pl, d, 0.0, refNormal)
            recordTestCase "3D spiral staircase" pl d 0.0 refNormal result
            Expect.equal result.PointCount pts.Count "Should have same point count"
            // Verify offset produced different points (not just a copy)
            let totalDist = [for i in 0..pts.Count-1 -> Pnt.distance result.Points.[i] pts.[i]] |> List.sum
            Expect.isTrue (totalDist > 0.1) "Offset should produce different points"

        // ============ Specific value checks ============

        testCase "Polyline3D.offset closed rectangle specific values" <| fun _ ->
            // Rectangle 20x10 in XY plane
            let pts = ResizeArray [Pnt(0,0,0); Pnt(20,0,0); Pnt(20,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let result = Polyline3D.offsetWithRef(pl, 2.0, 0.0, refNormal)
            recordTestCase "closed rectangle 20x10" pl 2.0 0.0 refNormal result
            Expect.equal result.PointCount 5 "Should have 5 points"
            Expect.isTrue (eqPnt result.Points.[0] (Pnt(2, 2, 0))) $"Corner 0 should be at (2,2,0), got {result.Points.[0]}"
            Expect.isTrue (eqPnt result.Points.[1] (Pnt(18, 2, 0))) $"Corner 1 should be at (18,2,0), got {result.Points.[1]}"
            Expect.isTrue (eqPnt result.Points.[2] (Pnt(18, 8, 0))) $"Corner 2 should be at (18,8,0), got {result.Points.[2]}"
            Expect.isTrue (eqPnt result.Points.[3] (Pnt(2, 8, 0))) $"Corner 3 should be at (2,8,0), got {result.Points.[3]}"

        testCase "Polyline3D.offset open 3-point line distance check" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let d = 3.0
            let result = Polyline3D.offsetWithRef(pl, d, 0.0, refNormal)
            recordTestCase "open 3-point line distance check" pl d 0.0 refNormal result
            // First and last points should be exactly d away from original
            let dist0 = Pnt.distance result.Points.[0] pts.[0]
            let dist2 = Pnt.distance result.Points.[2] pts.[2]
            Expect.floatClose tol dist0 d $"First point should be {d} away, got {dist0}"
            Expect.floatClose tol dist2 d $"Last point should be {d} away, got {dist2}"

        testCase "Polyline3D.offset with loop flag on open L returns closed behavior" <| fun _ ->
            // Open L-shape treated as loop
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let d = 1.0
            // With loop=true
            let resultLoop = Polyline3D.offsetWithRef(pl, d, 0.0, refNormal, loop=true)
            recordTestCase "open L with loop=true" pl d 0.0 refNormal resultLoop
            // Without loop
            let resultOpen = Polyline3D.offsetWithRef(pl, d, 0.0, refNormal, loop=false)
            recordTestCase "open L with loop=false" pl d 0.0 refNormal resultOpen
            // The corner point should be different because with loop, it accounts for the closing segment
            Expect.equal resultLoop.PointCount 3 "Loop result should have 3 points"
            Expect.equal resultOpen.PointCount 3 "Open result should have 3 points"
            // Corner points should differ
            Expect.isFalse (eqPnt resultLoop.Points.[0] resultOpen.Points.[0]) "Start points should differ between loop and open"

        testCase "Polyline3D.offset large offset value" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(100,0,0); Pnt(100,100,0); Pnt(0,100,0); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let d = 10.0
            let offset1 = Polyline3D.offsetWithRef(pl, d, 0.0, refNormal)
            let offset2 = Polyline3D.offsetWithRef(offset1, -d, 0.0, refNormal)
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (large offset)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.Points.[i] pts.[i]) $"pt[{i}] returns to original (large offset)"

        testCase "Polyline3D.offset very small offset value" <| fun _ ->
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let refNormal = UnitVec.Zaxis
            let d = 0.001
            let offset1 = Polyline3D.offsetWithRef(pl, d, 0.0, refNormal)
            let offset2 = Polyline3D.offsetWithRef(offset1, -d, 0.0, refNormal)
            Expect.equal offset2.PointCount pts.Count "same count after roundtrip (small offset)"
            for i = 0 to pts.Count - 1 do
                Expect.isTrue (eqPnt offset2.Points.[i] pts.[i]) $"pt[{i}] returns to original (small offset)"

        // ============ Orientation independence tests ============

        testCase "Polyline3D.offset positive inPlane offsets inward for CCW square" <| fun _ ->
            // CCW square in XY plane when viewed from +Z
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let result = Polyline3D.offset(pl, 1.0, 0.0)
            // Positive inPlane should shrink the square (inward offset)
            Expect.isTrue (eqPnt result.Points.[0] (Pnt(1, 1, 0))) $"CCW: Corner 0 should be inward at (1,1,0), got {result.Points.[0]}"
            Expect.isTrue (eqPnt result.Points.[1] (Pnt(9, 1, 0))) $"CCW: Corner 1 should be inward at (9,1,0), got {result.Points.[1]}"
            Expect.isTrue (eqPnt result.Points.[2] (Pnt(9, 9, 0))) $"CCW: Corner 2 should be inward at (9,9,0), got {result.Points.[2]}"
            Expect.isTrue (eqPnt result.Points.[3] (Pnt(1, 9, 0))) $"CCW: Corner 3 should be inward at (1,9,0), got {result.Points.[3]}"

        testCase "Polyline3D.offset positive inPlane offsets inward for CW square" <| fun _ ->
            // CW square in XY plane when viewed from +Z (reversed winding)
            let pts = ResizeArray [Pnt(0,0,0); Pnt(0,10,0); Pnt(10,10,0); Pnt(10,0,0); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let result = Polyline3D.offset(pl, 1.0, 0.0)
            // Positive inPlane should still shrink the square (inward offset) regardless of winding
            Expect.isTrue (eqPnt result.Points.[0] (Pnt(1, 1, 0))) $"CW: Corner 0 should be inward at (1,1,0), got {result.Points.[0]}"
            Expect.isTrue (eqPnt result.Points.[1] (Pnt(1, 9, 0))) $"CW: Corner 1 should be inward at (1,9,0), got {result.Points.[1]}"
            Expect.isTrue (eqPnt result.Points.[2] (Pnt(9, 9, 0))) $"CW: Corner 2 should be inward at (9,9,0), got {result.Points.[2]}"
            Expect.isTrue (eqPnt result.Points.[3] (Pnt(9, 1, 0))) $"CW: Corner 3 should be inward at (9,1,0), got {result.Points.[3]}"

        testCase "Polyline3D.offset negative inPlane offsets outward for CCW square" <| fun _ ->
            // CCW square in XY plane
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let result = Polyline3D.offset(pl, -1.0, 0.0)
            // Negative inPlane should expand the square (outward offset)
            Expect.isTrue (eqPnt result.Points.[0] (Pnt(-1, -1, 0))) $"CCW: Corner 0 should be outward at (-1,-1,0), got {result.Points.[0]}"
            Expect.isTrue (eqPnt result.Points.[1] (Pnt(11, -1, 0))) $"CCW: Corner 1 should be outward at (11,-1,0), got {result.Points.[1]}"
            Expect.isTrue (eqPnt result.Points.[2] (Pnt(11, 11, 0))) $"CCW: Corner 2 should be outward at (11,11,0), got {result.Points.[2]}"
            Expect.isTrue (eqPnt result.Points.[3] (Pnt(-1, 11, 0))) $"CCW: Corner 3 should be outward at (-1,11,0), got {result.Points.[3]}"

        testCase "Polyline3D.offset negative inPlane offsets outward for CW square" <| fun _ ->
            // CW square in XY plane (reversed winding)
            let pts = ResizeArray [Pnt(0,0,0); Pnt(0,10,0); Pnt(10,10,0); Pnt(10,0,0); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let result = Polyline3D.offset(pl, -1.0, 0.0)
            // Negative inPlane should still expand the square (outward offset) regardless of winding
            Expect.isTrue (eqPnt result.Points.[0] (Pnt(-1, -1, 0))) $"CW: Corner 0 should be outward at (-1,-1,0), got {result.Points.[0]}"
            Expect.isTrue (eqPnt result.Points.[1] (Pnt(-1, 11, 0))) $"CW: Corner 1 should be outward at (-1,11,0), got {result.Points.[1]}"
            Expect.isTrue (eqPnt result.Points.[2] (Pnt(11, 11, 0))) $"CW: Corner 2 should be outward at (11,11,0), got {result.Points.[2]}"
            Expect.isTrue (eqPnt result.Points.[3] (Pnt(11, -1, 0))) $"CW: Corner 3 should be outward at (11,-1,0), got {result.Points.[3]}"

        testCase "Polyline3D.offset positive perp offsets upward for CCW square in XY plane" <| fun _ ->
            // CCW square in XY plane - computed normal should point +Z
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let result = Polyline3D.offset(pl, 0.0, 2.0)
            // Positive perpendicular offset should go in direction of computed normal (+Z for CCW)
            for i = 0 to 3 do
                Expect.isTrue (abs(result.Points.[i].Z - 2.0) < 1e-6) $"CCW: Point {i} should be at Z=2, got Z={result.Points.[i].Z}"

        testCase "Polyline3D.offset positive perp offsets downward for CW square in XY plane" <| fun _ ->
            // CW square in XY plane - computed normal should point -Z
            let pts = ResizeArray [Pnt(0,0,0); Pnt(0,10,0); Pnt(10,10,0); Pnt(10,0,0); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let result = Polyline3D.offset(pl, 0.0, 2.0)
            // Positive perpendicular offset should go in direction of computed normal (-Z for CW)
            for i = 0 to 3 do
                Expect.isTrue (abs(result.Points.[i].Z - (-2.0)) < 1e-6) $"CW: Point {i} should be at Z=-2, got Z={result.Points.[i].Z}"

        testCase "Polyline3D.offset negative perp offsets downward for CCW square in XY plane" <| fun _ ->
            // CCW square in XY plane
            let pts = ResizeArray [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,10,0); Pnt(0,10,0); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let result = Polyline3D.offset(pl, 0.0, -2.0)
            // Negative perpendicular offset should go opposite to computed normal (-Z for CCW)
            for i = 0 to 3 do
                Expect.isTrue (abs(result.Points.[i].Z - (-2.0)) < 1e-6) $"CCW: Point {i} should be at Z=-2, got Z={result.Points.[i].Z}"

        testCase "Polyline3D.offset negative perp offsets upward for CW square in XY plane" <| fun _ ->
            // CW square in XY plane
            let pts = ResizeArray [Pnt(0,0,0); Pnt(0,10,0); Pnt(10,10,0); Pnt(10,0,0); Pnt(0,0,0)]
            let pl = Polyline3D(pts)
            let result = Polyline3D.offset(pl, 0.0, -2.0)
            // Negative perpendicular offset should go opposite to computed normal (+Z for CW)
            for i = 0 to 3 do
                Expect.isTrue (abs(result.Points.[i].Z - 2.0) < 1e-6) $"CW: Point {i} should be at Z=2, got Z={result.Points.[i].Z}"

        // ============ Write test data file ============

        testCase "Write test data file (when saveTestData=true)" <| fun _ ->
            writeTestDataFile ()
            Expect.isTrue true "Test data file written if saveTestData was enabled"

    ]
