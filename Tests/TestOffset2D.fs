module TestOffset2D

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let inline eq a b = Pt.distance a b < 1e-6

let tol = Accuracy.veryHigh

let tests =
    testList "Offset2D " [

        test "offset distance 0 returns original points (simple square)" {
          let pts = ResizeArray([Pt(0,0); Pt(1,0); Pt(1,1); Pt(0,1); Pt(0,0)])
          let result = Offset2D.offset 0.0 pts
          "count is one less (removes duplicate)" |> Expect.equal result.Count (pts.Count - 1)
          for i = 0 to result.Count - 1 do
            sprintf "pt[%d] matches" i |> Expect.isTrue (eq result.[i] pts.[i])
        }

        test "offset distance 0 returns original points (triangle)" {
          let pts = ResizeArray([Pt(0,0); Pt(2,0); Pt(1,2); Pt(0,0)])
          let result = Offset2D.offset 0.0 pts
          "count is one less (removes duplicate)" |> Expect.equal result.Count (pts.Count - 1)
          for i = 0 to result.Count - 1 do
            sprintf "pt[%d] matches" i |> Expect.isTrue (eq result.[i] pts.[i])
        }

        test "offset positive then negative returns to original (square CCW)" {
          let pts = ResizeArray([Pt(0,0); Pt(1,0); Pt(1,1); Pt(0,1); Pt(0,0)])
          let d = 0.2
          // First offset by positive distance (inward for CCW)
          let offset1 = Offset2D.offset d pts
          // Then offset the result by negative distance to return to original
          let offset2 = Offset2D.offset (-d) offset1
          "same count after roundtrip" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after +d then -d" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "offset negative then positive returns to original (square CCW)" {
          let pts = ResizeArray([Pt(0,0); Pt(1,0); Pt(1,1); Pt(0,1); Pt(0,0)])
          let d = 0.2
          // First offset by negative distance (outward for CCW)
          let offset1 = Offset2D.offset (-d) pts
          // Then offset the result by positive distance to return to original
          let offset2 = Offset2D.offset d offset1
          "same count after roundtrip" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after -d then +d" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "offset positive then negative returns to original (square CW)" {
          let pts = ResizeArray([Pt(0,0); Pt(0,1); Pt(1,1); Pt(1,0); Pt(0,0)])
          let d = 0.15
          // First offset by positive distance (outward for CW)
          let offset1 = Offset2D.offset d pts
          // Then offset the result by negative distance to return to original
          let offset2 = Offset2D.offset (-d) offset1
          "same count after roundtrip (CW)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after +d then -d (CW)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "offset negative then positive returns to original (square CW)" {
          let pts = ResizeArray([Pt(0,0); Pt(0,1); Pt(1,1); Pt(1,0); Pt(0,0)])
          let d = 0.15
          // First offset by negative distance (inward for CW)
          let offset1 = Offset2D.offset (-d) pts
          // Then offset the result by positive distance to return to original
          let offset2 = Offset2D.offset d offset1
          "same count after roundtrip (CW)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after -d then +d (CW)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "offset positive then negative returns to original (triangle)" {
          let pts = ResizeArray([Pt(0,0); Pt(3,0); Pt(1.5,3); Pt(0,0)])
          let d = 0.25
          // First offset by positive distance
          let offset1 = Offset2D.offset d pts
          // Then offset the result by negative distance to return to original
          let offset2 = Offset2D.offset (-d) offset1
          "same count after roundtrip (triangle)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after +d then -d (triangle)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "offset negative then positive returns to original (triangle)" {
          let pts = ResizeArray([Pt(0,0); Pt(3,0); Pt(1.5,3); Pt(0,0)])
          let d = 0.25
          // First offset by negative distance
          let offset1 = Offset2D.offset (-d) pts
          // Then offset the result by positive distance to return to original
          let offset2 = Offset2D.offset d offset1
          "same count after roundtrip (triangle)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after -d then +d (triangle)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "offset positive then negative returns to original (pentagon)" {
          let sqrt5 = sqrt 5.0
          let phi = (1.0 + sqrt5) / 2.0 // golden ratio
          let pts = ResizeArray([
            Pt(1,0)
            Pt(phi/2.0, sqrt(10.0 + 2.0*sqrt5)/4.0)
            Pt(-phi/2.0, sqrt(10.0 + 2.0*sqrt5)/4.0)
            Pt(-1,0)
            Pt(0, -sqrt(10.0 - 2.0*sqrt5)/4.0)
            Pt(1,0)
          ])
          let d = 0.1
          // First offset by positive distance
          let offset1 = Offset2D.offset d pts
          // Then offset the result by negative distance to return to original
          let offset2 = Offset2D.offset (-d) offset1
          "same count after roundtrip (pentagon)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after +d then -d (pentagon)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "offset positive then negative returns to original (square)" {
          let pts = ResizeArray([Pt(0,0); Pt(1,0); Pt(1,1); Pt(0,1); Pt(0,0)])
          let d = 0.2
          // First offset by positive distance
          let offset1 = Offset2D.offset d pts
          // Then offset the result by negative distance to return to original
          let offset2 = Offset2D.offset (-d) offset1
          "same count after roundtrip (chamfered)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after +d then -d (chamfered)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "offset negative then positive returns to original (square)" {
          let pts = ResizeArray([Pt(0,0); Pt(1,0); Pt(1,1); Pt(0,1); Pt(0,0)])
          let d = 0.2
          // First offset by negative distance
          let offset1 = Offset2D.offset (-d) pts
          // Then offset the result by positive distance to return to original
          let offset2 = Offset2D.offset d offset1
          "same count after roundtrip (chamfered)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after -d then +d (chamfered)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "offsetVariable equal distances positive then negative returns to original" {
          let pts = ResizeArray([Pt(0,0); Pt(2,0); Pt(2,2); Pt(0,2); Pt(0,0)])
          let d = 0.15
          let dists = ResizeArray([d; d; d; d]) // equal distances for all segments
          // First offset by positive distances
          let offset1 = Offset2D.offsetVariable dists pts
          // Create negative distances for return trip
          let negDists = ResizeArray(dists |> Seq.map (fun x -> -x))
          // Then offset the result by negative distances to return to original
          let offset2 = Offset2D.offsetVariable negDists offset1
          "same count after roundtrip (variable equal)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after +d then -d (variable equal)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "offsetVariable equal distances negative then positive returns to original" {
          let pts = ResizeArray([Pt(0,0); Pt(2,0); Pt(2,2); Pt(0,2); Pt(0,0)])
          let d = 0.15
          let dists = ResizeArray([d; d; d; d]) // equal distances for all segments
          let negDists = ResizeArray(dists |> Seq.map (fun x -> -x))
          // First offset by negative distances
          let offset1 = Offset2D.offsetVariable negDists pts
          // Then offset the result by positive distances to return to original
          let offset2 = Offset2D.offsetVariable dists offset1
          "same count after roundtrip (variable equal)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after -d then +d (variable equal)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "offsetVariable different distances positive then negative returns to original" {
          let pts = ResizeArray([Pt(0,0); Pt(2,0); Pt(2,2); Pt(0,2); Pt(0,0)])
          let dists = ResizeArray([0.1; 0.2; 0.15; 0.25]) // different distances for each segment
          // First offset by positive distances
          let offset1 = Offset2D.offsetVariable dists pts
          // Create negative distances for return trip
          let negDists = ResizeArray(dists |> Seq.map (fun x -> -x))
          // Then offset the result by negative distances to return to original
          let offset2 = Offset2D.offsetVariable negDists offset1
          "same count after roundtrip (variable different)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after +d then -d (variable different)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "offsetVariable different distances negative then positive returns to original" {
          let pts = ResizeArray([Pt(0,0); Pt(2,0); Pt(2,2); Pt(0,2); Pt(0,0)])
          let dists = ResizeArray([0.1; 0.2; 0.15; 0.25]) // different distances for each segment
          let negDists = ResizeArray(dists |> Seq.map (fun x -> -x))
          // First offset by negative distances
          let offset1 = Offset2D.offsetVariable negDists pts
          // Then offset the result by positive distances to return to original
          let offset2 = Offset2D.offsetVariable dists offset1
          "same count after roundtrip (variable different)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after -d then +d (variable different)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "offset with larger distances maintains roundtrip property" {
          let pts = ResizeArray([Pt(0,0); Pt(4,0); Pt(4,4); Pt(0,4); Pt(0,0)])
          let d = 0.8 // larger offset relative to shape size
          // First offset by positive distance
          let offset1 = Offset2D.offset d pts
          // Then offset the result by negative distance to return to original
          let offset2 = Offset2D.offset (-d) offset1
          "same count after roundtrip (large offset)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after +d then -d (large offset)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "open polyline offset positive then negative returns to original" {
          let pts = ResizeArray([Pt(0,0); Pt(1,0); Pt(2,1); Pt(3,1); Pt(4,2)]) // open polyline
          let d = 0.25
          // First offset by positive distance
          let offset1 = Offset2D.offset d pts
          // Then offset the result by negative distance to return to original
          let offset2 = Offset2D.offset (-d) offset1
          "same count after roundtrip (open)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after +d then -d (open)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "open polyline offset negative then positive returns to original" {
          let pts = ResizeArray([Pt(0,0); Pt(1,0); Pt(2,1); Pt(3,1); Pt(4,2)]) // open polyline
          let d = 0.25
          // First offset by negative distance
          let offset1 = Offset2D.offset (-d) pts
          // Then offset the result by positive distance to return to original
          let offset2 = Offset2D.offset d offset1
          "same count after roundtrip (open)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after -d then +d (open)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }


        test "offsetWithNormals positive then negative returns to original" {
          let pts = ResizeArray([Pt(0,0); Pt(1,0); Pt(1,1); Pt(0,1); Pt(0,0)])
          let d = 0.2
          // First offset by positive distance
          let offset1 = Offset2D.offset d pts
          // Then offset the result by negative distance to return to original
          let offset2 = Offset2D.offset  -d offset1
          "same count after roundtrip (with normals)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after +d then -d (with normals)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "open polyline L-shape offset positive then negative returns to original" {
          let pts = ResizeArray([Pt(0,0); Pt(3,0); Pt(3,2); Pt(1,2); Pt(1,4)]) // L-shaped open polyline
          let d = 0.3
          // First offset by positive distance
          let offset1 = Offset2D.offset d pts

          "distance of first points is d" |> Expect.floatClose tol (Pt.distance offset1.[0] pts.[0]) d
          "distance of second points is not d" |> Expect.isTrue ((Pt.distance offset1.[1] pts.[1]) > (d+0.01))
          "distance of last point is d" |> Expect.floatClose tol (Pt.distance offset1.[offset1.Count - 1] pts.[pts.Count - 1]) d

          // Then offset the result by negative distance to return to original
          let offset2 = Offset2D.offset (-d) offset1
          "same count after roundtrip (L-shape)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after +d then -d (L-shape)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "open polyline L-shape offset negative then positive returns to original" {
          let pts = ResizeArray([Pt(0,0); Pt(3,0); Pt(3,2); Pt(1,2); Pt(1,4)]) // L-shaped open polyline
          let d = 0.3
          // First offset by negative distance
          let offset1 = Offset2D.offset (-d) pts
          // Then offset the result by positive distance to return to original
          let offset2 = Offset2D.offset d offset1
          "same count after roundtrip (L-shape)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after -d then +d (L-shape)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "open polyline zigzag offset positive then negative returns to original" {
          let pts = ResizeArray([Pt(0,0); Pt(1,1); Pt(2,0); Pt(3,1); Pt(4,0); Pt(5,1)]) // zigzag pattern
          let d = 0.2
          // First offset by positive distance
          let offset1 = Offset2D.offset d pts
          // Then offset the result by negative distance to return to original
          let offset2 = Offset2D.offset (-d) offset1
          "same count after roundtrip (zigzag)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after +d then -d (zigzag)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "open polyline zigzag offset negative then positive returns to original" {
          let pts = ResizeArray([Pt(0,0); Pt(1,1); Pt(2,0); Pt(3,1); Pt(4,0); Pt(5,1)]) // zigzag pattern
          let d = 0.2
          // First offset by negative distance
          let offset1 = Offset2D.offset (-d) pts
          // Then offset the result by positive distance to return to original
          let offset2 = Offset2D.offset d offset1
          "same count after roundtrip (zigzag)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after -d then +d (zigzag)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "open polyline arc-like offset positive then negative returns to original" {
          let pts = ResizeArray([Pt(0,0); Pt(1,0.5); Pt(2,0.8); Pt(3,0.9); Pt(4,0.8); Pt(5,0.5); Pt(6,0)]) // arc-like curve
          let d = 0.15
          // First offset by positive distance
          let offset1 = Offset2D.offset d pts
          // Then offset the result by negative distance to return to original
          let offset2 = Offset2D.offset (-d) offset1
          "same count after roundtrip (arc-like)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after +d then -d (arc-like)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "open polyline single segment offset positive then negative returns to original" {
          let pts = ResizeArray([Pt(0,0); Pt(3,4)]) // single line segment
          let d = 0.5
          // First offset by positive distance
          let offset1 = Offset2D.offset d pts
          // Then offset the result by negative distance to return to original
          let offset2 = Offset2D.offset (-d) offset1
          "same count after roundtrip (single segment)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after +d then -d (single segment)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "open polyline offset positive then negative (approximate roundtrip)" {
          let pts = ResizeArray([Pt(0,0); Pt(2,0); Pt(2,2); Pt(4,2)]) // open polyline with right angle
          let d = 0.1 // smaller offset for better stability
          // First offset by positive distance
          let offset1 = Offset2D.offset d pts
          // Then offset the result by negative distance to return to original
          let offset2 = Offset2D.offset (-d) offset1
          // Note: chamfered offset may change point count and exact positions due to corner handling
          "has reasonable point count (open chamfered)" |> Expect.isTrue (offset2.Count >= 2)
          // Check that we get back to approximately the right area (first and last points should be reasonable)
          let firstDist = Pt.distance offset2.[0] pts.[0]
          let lastDist = Pt.distance offset2.[offset2.Count-1] pts.[pts.Count-1]
          "first point approximately matches (open chamfered)" |> Expect.isTrue (firstDist < 0.5)
          "last point approximately matches (open chamfered)" |> Expect.isTrue (lastDist < 0.5)
        }

        test "open polyline offset negative then positive (approximate roundtrip)" {
          let pts = ResizeArray([Pt(0,0); Pt(2,0); Pt(2,2); Pt(4,2)]) // open polyline with right angle
          let d = 0.1 // smaller offset for better stability
          // First offset by negative distance
          let offset1 = Offset2D.offset (-d) pts
          // Then offset the result by positive distance to return to original
          let offset2 = Offset2D.offset d offset1
          // Note: chamfered offset may change point count and exact positions due to corner handling
          "has reasonable point count (open chamfered)" |> Expect.isTrue (offset2.Count >= 2)
          // Check that we get back to approximately the right area (first and last points should be reasonable)
          let firstDist = Pt.distance offset2.[0] pts.[0]
          let lastDist = Pt.distance offset2.[offset2.Count-1] pts.[pts.Count-1]
          "first point approximately matches (open chamfered)" |> Expect.isTrue (firstDist < 0.5)
          "last point approximately matches (open chamfered)" |> Expect.isTrue (lastDist < 0.5)
        }

        test "open polyline offsetVariable equal distances positive then negative returns to original" {
          let pts = ResizeArray([Pt(0,0); Pt(2,0); Pt(2,2); Pt(4,2); Pt(4,4)]) // open polyline
          let d = 0.2
          let dists = ResizeArray([d; d; d; d]) // equal distances for all segments
          // First offset by positive distances
          let offset1 = Offset2D.offsetVariable dists pts
          // Create negative distances for return trip
          let negDists = ResizeArray(dists |> Seq.map (fun x -> -x))
          // Then offset the result by negative distances to return to original
          let offset2 = Offset2D.offsetVariable negDists offset1
          "same count after roundtrip (open variable equal)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after +d then -d (open variable equal)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "open polyline offsetVariable different distances positive then negative returns to original" {
          let pts = ResizeArray([Pt(0,0); Pt(2,0); Pt(2,2); Pt(4,2); Pt(4,4)]) // open polyline
          let dists = ResizeArray([0.1; 0.3; 0.2; 0.25]) // different distances for each segment
          // First offset by positive distances
          let offset1 = Offset2D.offsetVariable dists pts
          // Create negative distances for return trip
          let negDists = ResizeArray(dists |> Seq.map (fun x -> -x))
          // Then offset the result by negative distances to return to original
          let offset2 = Offset2D.offsetVariable negDists offset1
          "same count after roundtrip (open variable different)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after +d then -d (open variable different)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "open polyline offsetVariable different distances negative then positive returns to original" {
          let pts = ResizeArray([Pt(0,0); Pt(2,0); Pt(2,2); Pt(4,2); Pt(4,4)]) // open polyline
          let dists = ResizeArray([0.15; 0.25; 0.1; 0.35]) // different distances for each segment
          let negDists = ResizeArray(dists |> Seq.map (fun x -> -x))
          // First offset by negative distances
          let offset1 = Offset2D.offsetVariable negDists pts
          // Then offset the result by positive distances to return to original
          let offset2 = Offset2D.offsetVariable dists offset1
          "same count after roundtrip (open variable different)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after -d then +d (open variable different)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        test "open polyline distance 0 returns original points" {
          let pts = ResizeArray([Pt(0,0); Pt(2,1); Pt(4,0); Pt(6,2)]) // open polyline
          let result = Offset2D.offset 0.0 pts
          // For open polylines with zero distance, it appears to clone without the last point
          "count matches behavior (open zero distance)" |> Expect.isTrue (result.Count = pts.Count || result.Count = pts.Count - 1)
          for i = 0 to (min result.Count pts.Count) - 1 do
            sprintf "pt[%d] matches (open)" i |> Expect.isTrue (eq result.[i] pts.[i])
        }

        // this below test builds to invalid js: fails with "Invalid left-hand side expression in prefix operation"
        // https://github.com/fable-compiler/Fable/issues/4251

        test "open polyline with sharp turns offset positive then negative returns to original" {
          let pts = ResizeArray([Pt(0,0); Pt(1,0); Pt(1.1,1); Pt(2,1); Pt(2.1,0)]) // sharp turns
          let d = -0.3 // small offset for sharp turns
          // First offset by positive distance
          let offset1 = Offset2D.offset d pts
          // for i = 0 to pts.Count - 1 do printfn $"Pt({offset1[i].X}, {offset1[i].Y})"
          let o1 = ResizeArray[
            Pt(0, -0.3)
            Pt(1.2714962686336266, -0.3)
            Pt(1.3714962686336267, 0.7)
            Pt(1.7285037313663734, 0.7)
            Pt(1.8014888429370033, -0.029851115706299704)
          ]
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] matches precalculation" i |> Expect.isTrue (eq offset1.[i] o1.[i])

          let dd = d * -1.0
          // Then offset the result by negative distance to return to original
          let offset2 = Offset2D.offset dd offset1
          "same count after roundtrip (sharp turns)" |> Expect.equal offset2.Count pts.Count
          for i = 0 to pts.Count - 1 do
            sprintf "pt[%d] returns to original after +d then -d (sharp turns)" i |> Expect.isTrue (eq offset2.[i] pts.[i])
        }

        // Tests generated via D:\Git\_Euclid_\Euclid\Tests\TestInRhino/offsetConstant.fsx
        // and D:\Git\_Euclid_\Euclid\Tests\TestInRhino\offsetConstant.3dm

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(-6, 5.89285714285714); Pt(13.9998687263502, 5.96532043534392); Pt(19.0714285714286, -8.03571428571429); Pt(6.93589623809369, -12.0565231913373); Pt(-2.36998930241467, -15.1397985210238); Pt(-8.87775543012746, -16.690455270762); Pt(-14.6428571428571, -2.67857142857143); Pt(-6, 5.89285714285714) |]))
        test "StepWithTwoPoints-1" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-5.54538287863022, 4.79449708624224); Pt(13.2293915010154, 4.86252163109603); Pt(17.6520795398397, -7.34717532280679); Pt(6.58993333459473, -11.0123442462313); Pt(-2.67101110517481, -14.0807294521791); Pt(-8.21820636167114, -15.4025034062202); Pt(-13.3453930986188, -2.94105023936624); Pt(-5.54538287863022, 4.79449708624224) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer -1.1 pli.Points |> Polyline2D
            "offset 1: dist -1.1 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        test "StepWithTwoPoints-2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-6.39262387754662, 6.84144082811547); Pt(14.6652808755031, 6.9177375844671); Pt(20.2972300078007, -8.63036157186167); Pt(7.2346823820246, -12.9583140984742); Pt(-2.11001592730364, -16.0544490804806); Pt(-9.44736598924883, -17.8027773355936); Pt(-15.7633942719721, -2.451885182885); Pt(-6.39262387754662, 6.84144082811547) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer 0.95 pli.Points |> Polyline2D
            "offset 2: dist 0.95 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(-4.15630372020485, 41.2303691722641); Pt(15.8435650061454, 41.3028324647508); Pt(20.9151248512237, 27.3017977436926); Pt(8.77959251788884, 23.2809888380696); Pt(-0.526293022619517, 20.1977135083831); Pt(-8.28604599902698, 22.2397146722904); Pt(-12.799160863062, 32.6589406008355); Pt(-4.15630372020485, 41.2303691722641) |]))
        test "StepWithTwoPoints-3" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-3.70168659883507, 40.1320091156492); Pt(15.0730877808105, 40.2000336605029); Pt(19.4957758196349, 27.9903367066001); Pt(8.43362961438988, 24.3251677831756); Pt(-0.562216150162633, 21.3446164756431); Pt(-7.48914149317371, 23.1674566685891); Pt(-11.4912127737654, 32.4068591900984); Pt(-3.70168659883507, 40.1320091156492) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer -1.1 pli.Points |> Polyline2D
            "offset 3: dist -1.1 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        test "StepWithTwoPoints-4" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-4.54892759775147, 42.1789528575224); Pt(16.5089771552982, 42.255249613874); Pt(22.1409262875959, 26.7071504575452); Pt(9.07837866181976, 22.3791979309327); Pt(-0.495268503377735, 19.2072064002949); Pt(-8.97428170862754, 21.4384829482143); Pt(-13.9287523947272, 32.8766472737448); Pt(-4.54892759775147, 42.1789528575224) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer 0.95 pli.Points |> Polyline2D
            "offset 4: dist 0.95 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(-4.42363968077515, 75.6675428517043); Pt(15.5762290455751, 75.7400061441911); Pt(20.6477888906534, 61.7389714231329); Pt(8.51225655731853, 57.7181625175099); Pt(-0.79362898318982, 54.6348871878234); Pt(-7.0121093966169, 52.7081590095137); Pt(-13.0664968236323, 67.0961142802758); Pt(-4.42363968077515, 75.6675428517043) |]))
        test "StepWithTwoPoints-5" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-3.96902255940538, 74.5691827950894); Pt(14.8057518202402, 74.6372073399432); Pt(19.2284398590646, 62.4275103860404); Pt(8.16629365381957, 58.7623414626158); Pt(-1.1294194885771, 55.6824365057977); Pt(-6.38503047253363, 54.0540428945292); Pt(-11.7645124589045, 66.8381184319498); Pt(-3.96902255940538, 74.5691827950894) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer -1.1 pli.Points |> Polyline2D
            "offset 5: dist -1.1 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        test "StepWithTwoPoints-6" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-4.81626355832178, 76.6161265369627); Pt(16.2416411947279, 76.6924232933143); Pt(21.8735903270256, 61.1443241369855); Pt(8.81104270124945, 56.8163716103729); Pt(-0.503628092173531, 53.7301855041183); Pt(-7.55367755832518, 51.5458047451822); Pt(-14.1909378658972, 67.3189288765573); Pt(-4.81626355832178, 76.6161265369627) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer 0.95 pli.Points |> Polyline2D
            "offset 6: dist 0.95 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(0.836834692090122, -36.2604140142501); Pt(6.16306838927609, -36.2604140142501); Pt(13.025715652958, -36.2604140142501); Pt(17.7373839235456, -39.5380962894415); Pt(17.3276736391467, -48.6541501173175); Pt(9.95288851996611, -58.692052085091); Pt(-12.5811771219745, -60.4333207937864); Pt(-22.5166515186484, -46.9128814086221); Pt(-15.8588593971659, -36.2604140142501); Pt(-9.61077756008235, -36.2604140142501); Pt(0.836834692090122, -36.2604140142501) |]))
        test "StepWithTwoPoints-7" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(0.836834692090122, -37.3604140142501); Pt(6.16306838927609, -37.3604140142501); Pt(12.6807383656162, -37.3604140142501); Pt(16.6112581270357, -40.0946886308897); Pt(16.2437558436512, -48.2716144361956); Pt(9.3651283762457, -57.6341907112753); Pt(-12.056640224475, -59.2895091940583); Pt(-21.1882749413445, -46.8629547339884); Pt(-15.2491869915081, -37.3604140142501); Pt(-9.61077756008235, -37.3604140142501); Pt(0.836834692090122, -37.3604140142501) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer -1.1 pli.Points |> Polyline2D
            "offset 7: dist -1.1 on 11 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        test "StepWithTwoPoints-8" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(0.836834692090122, -35.3104140142501); Pt(6.16306838927609, -35.3104140142501); Pt(13.323650582935, -35.3104140142501); Pt(18.7099471114405, -39.0574029036453); Pt(18.2637844625292, -48.9845218419227); Pt(10.4604995531792, -59.6056596352046); Pt(-13.0341862607241, -61.4211580844607); Pt(-23.6638858354107, -46.9559999003511); Pt(-16.3853946565976, -35.3104140142501); Pt(-9.61077756008235, -35.3104140142501); Pt(0.836834692090122, -35.3104140142501) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer 0.95 pli.Points |> Polyline2D
            "offset 8: dist 0.95 on 11 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(58.0465376145519, -43.7722240678395); Pt(47.5989253623794, -43.7722240678395); Pt(41.3508435252959, -43.7722240678395); Pt(34.6930514038134, -54.4246914622115); Pt(44.6285258004872, -67.9451308473758); Pt(67.1625914424279, -66.2038621386804); Pt(74.5373765616085, -56.1659601709069); Pt(74.9470868460074, -47.0499063430309); Pt(70.2354185754198, -43.7722240678395); Pt(63.3727713117379, -43.7722240678395); Pt(58.0465376145519, -43.7722240678395) |]))
        test "StepWithTwoPoints-9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(58.0465376145519, -42.6722240678395); Pt(47.5989253623794, -42.6722240678395); Pt(40.7411711196381, -42.6722240678395); Pt(33.3646748265096, -54.4746181368451); Pt(44.1039889029877, -69.088942447104); Pt(67.7503515861483, -67.2617235124961); Pt(75.6212943571039, -56.5484958520287); Pt(76.0732126425173, -46.4933140015826); Pt(70.5803958627615, -42.6722240678395); Pt(63.3727713117379, -42.6722240678395); Pt(58.0465376145519, -42.6722240678395) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer -1.1 pli.Points |> Polyline2D
            "offset 9: dist -1.1 on 11 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        test "StepWithTwoPoints-10" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(58.0465376145519, -44.7222240678395); Pt(47.5989253623794, -44.7222240678395); Pt(41.8773787847276, -44.7222240678395); Pt(35.8402857205758, -54.3815729704824); Pt(45.0815349392368, -66.9572935567015); Pt(66.6549804092148, -65.2902545885669); Pt(73.601265738226, -55.8355884463017); Pt(73.9745236581125, -47.5305997288271); Pt(69.9374836454428, -44.7222240678395); Pt(63.3727713117379, -44.7222240678395); Pt(58.0465376145519, -44.7222240678395) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer 0.95 pli.Points |> Polyline2D
            "offset 10: dist 0.95 on 11 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(60.3045787716737, 3.72302516341453); Pt(51.6617216288165, -4.84840340801404); Pt(57.4268233415462, -18.8602872502046); Pt(63.934589469259, -17.3096305004664); Pt(73.2404750097674, -14.2263551707799); Pt(85.3760073431022, -10.2055462651569); Pt(80.3044474980239, 3.79548845590132); Pt(60.3045787716737, 3.72302516341453) |]))
        test "StepWithTwoPoints-11" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(59.8499616503039, 4.82138522002944); Pt(50.3642575845782, -4.58592459721922); Pt(56.7672742730899, -20.1482391147465); Pt(64.2356112720192, -18.3686995693111); Pt(73.5864379132663, -15.2705341158858); Pt(86.7953563746911, -10.8940852280644); Pt(81.0749247233588, 4.89828726014921); Pt(59.8499616503039, 4.82138522002944) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer -1.1 pli.Points |> Polyline2D
            "offset 11: dist -1.1 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        test "StepWithTwoPoints-12" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(60.6972026492203, 2.77444147815621); Pt(52.7822587579315, -5.07508965370047); Pt(57.9964339006676, -17.7479651853731); Pt(63.674616094148, -16.3949799410096); Pt(72.9416888658364, -13.3245642636429); Pt(84.1502059067301, -9.61089897900952); Pt(79.6390353488711, 2.84307130677814); Pt(60.6972026492203, 2.77444147815621) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer 0.95 pli.Points |> Polyline2D
            "offset 12: dist 0.95 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(-7.0546699517994, -85.0922098858384); Pt(1.53169860556255, -85.0922098858384); Pt(9.18137241121229, -85.0922098858384); Pt(9.18137241121229, -108.353462886692); Pt(-28.1303018653242, -108.353462886692); Pt(-21.7295543952907, -85.0922098858384); Pt(-16.2655016769695, -85.0922098858384); Pt(-11.4259121264564, -85.0922098858384); Pt(-7.0546699517994, -85.0922098858384) |]))
        test "StepWithTwoPoints-13" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-7.0546699517994, -86.1922098858384); Pt(1.53169860556255, -86.1922098858384); Pt(8.08137241121229, -86.1922098858384); Pt(8.08137241121229, -107.253462886692); Pt(-26.6867325812116, -107.253462886692); Pt(-20.8913542386949, -86.1922098858384); Pt(-16.2655016769695, -86.1922098858384); Pt(-11.4259121264564, -86.1922098858384); Pt(-7.0546699517994, -86.1922098858384) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer -1.1 pli.Points |> Polyline2D
            "offset 13: dist -1.1 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        test "StepWithTwoPoints-14" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-7.0546699517994, -84.1422098858384); Pt(1.53169860556255, -84.1422098858384); Pt(10.1313724112123, -84.1422098858384); Pt(10.1313724112123, -109.303462886692); Pt(-29.3770207925123, -109.303462886692); Pt(-22.4534545305325, -84.1422098858384); Pt(-16.2655016769695, -84.1422098858384); Pt(-11.4259121264564, -84.1422098858384); Pt(-7.0546699517994, -84.1422098858384) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer 0.95 pli.Points |> Polyline2D
            "offset 14: dist 0.95 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(74.2740052906831, -151.87972174524); Pt(94.7039731263815, -151.87972174524); Pt(112.050172232163, -151.87972174524); Pt(130.938255702903, -151.87972174524); Pt(144.044272805049, -151.87972174524); Pt(161.872310774881, -134.051683775409); Pt(132.672875613481, -134.051683775409); Pt(113.784792142741, -134.051683775409); Pt(103.569808224892, -134.051683775409); Pt(94.5112375807617, -134.051683775409); Pt(83.1398403891937, -134.051683775409); Pt(43.6290535371354, -134.051683775409); Pt(35.8714478259386, -141.809289486606); Pt(74.2740052906831, -151.87972174524) |]))
        test "StepWithTwoPoints-15" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(74.1321746913071, -152.97972174524); Pt(94.7039731263815, -152.97972174524); Pt(112.050172232163, -152.97972174524); Pt(130.938255702903, -152.97972174524); Pt(144.49990772366, -152.97972174524); Pt(164.527945693491, -132.951683775409); Pt(132.672875613481, -132.951683775409); Pt(113.784792142741, -132.951683775409); Pt(103.569808224892, -132.951683775409); Pt(94.5112375807617, -132.951683775409); Pt(83.1398403891937, -132.951683775409); Pt(43.173418618525, -132.951683775409); Pt(33.7380645004892, -142.387037893445); Pt(74.1321746913071, -152.97972174524) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer -1.1 pli.Points |> Polyline2D
            "offset 15: dist -1.1 on 14 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        test "StepWithTwoPoints-16" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(74.3964953537805, -150.92972174524); Pt(94.7039731263815, -150.92972174524); Pt(112.050172232163, -150.92972174524); Pt(130.938255702903, -150.92972174524); Pt(143.650769920795, -150.92972174524); Pt(159.578807890626, -135.001683775409); Pt(132.672875613481, -135.001683775409); Pt(113.784792142741, -135.001683775409); Pt(103.569808224892, -135.001683775409); Pt(94.5112375807617, -135.001683775409); Pt(83.1398403891937, -135.001683775409); Pt(44.0225564213898, -135.001683775409); Pt(37.7139152433721, -141.310324953427); Pt(74.3964953537805, -150.92972174524) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer 0.95 pli.Points |> Polyline2D
            "offset 16: dist 0.95 on 14 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(50.5176391658783, 90.7847922388769); Pt(58.2671145011905, 87.7195157591458); Pt(57.5484280413359, 80.809581078954); Pt(62.9358477045969, 77.3576077085419); Pt(71.663209914399, 89.1463010786428); Pt(69.700808028773, 98.0841855333523); Pt(60.2740670044814, 86.986670430503); Pt(50.5176391658783, 90.7847922388769) |]))
        test "StepWithTwoPoints-17" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(49.8943993223609, 92.2078292657785); Pt(49.0929190941436, 90.1654094818705); Pt(57.086709167629, 87.0034952056216); Pt(56.3842020858627, 80.2491199411159); Pt(63.2081427926353, 75.876699919396); Pt(72.8448652437579, 88.8937333984761); Pt(70.2992693380939, 100.487812185222); Pt(59.9429648550915, 88.2959799163959); Pt(49.8943993223609, 92.2078292657785) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer -1.1 pli.Points |> Polyline2D
            "offset 17: dist -1.1 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        test "StepWithTwoPoints-18" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(51.0558917580069, 89.5558057156437); Pt(51.748079227831, 91.319713710837); Pt(59.2865554710845, 88.3378971462804); Pt(58.5538959119718, 81.293615697996); Pt(62.7006837649274, 78.6365735264407); Pt(70.6426894026798, 89.364427711514); Pt(69.183955079814, 96.0083261521921); Pt(60.5600188607727, 85.8559031472319); Pt(51.0558917580069, 89.5558057156437) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer 0.95 pli.Points |> Polyline2D
            "offset 18: dist 0.95 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(49.8419500966157, 126.725639284709); Pt(57.5914254319279, 123.660362804978); Pt(56.8727389720734, 116.750428124786); Pt(62.2601586353344, 113.298454754374); Pt(70.9875208451364, 125.087148124475); Pt(69.0251189595104, 134.025032579185); Pt(59.6018364196137, 123.956922142335); Pt(49.8419500966157, 126.725639284709) |]))
        test "StepWithTwoPoints-19" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(49.135666646819, 128.069405679956); Pt(48.464486979348, 126.087564213093); Pt(56.4110200983664, 122.944342251454); Pt(55.7085130166002, 116.189966986948); Pt(62.5324537233728, 111.817546965228); Pt(72.1691761744954, 124.834580444308); Pt(69.6510518590153, 136.303538122759); Pt(59.2569428006128, 125.198168263627); Pt(49.135666646819, 128.069405679956) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer -1.1 pli.Points |> Polyline2D
            "offset 19: dist -1.1 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        test "StepWithTwoPoints-20" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(50.4519221668948, 125.565113761541); Pt(51.0315773342561, 127.276704119287); Pt(58.610866401822, 124.278744192113); Pt(57.8782068427093, 117.234462743828); Pt(62.0249946956649, 114.577420572273); Pt(69.9670003334173, 125.305274757346); Pt(68.4845405463017, 132.057232337007); Pt(59.8996990905691, 122.884936855765); Pt(50.4519221668948, 125.565113761541) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer 0.95 pli.Points |> Polyline2D
            "offset 20: dist 0.95 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(58.040911764728, 37.4163889774333); Pt(49.3980546218708, 28.8449604060047); Pt(53.9111694859058, 18.4257344774596); Pt(61.6709224623133, 16.3837333135523); Pt(70.9768080028216, 19.4670086432388); Pt(83.1123403361565, 23.4878175488618); Pt(78.0407804910782, 37.48885226992); Pt(58.040911764728, 37.4163889774333) |]))
        test "StepWithTwoPoints-21" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(57.5862946433582, 38.5147490340482); Pt(48.0901065325742, 29.0970418167417); Pt(53.1142649800525, 17.497992481161); Pt(61.7068455898564, 15.2368303462923); Pt(71.3227709063206, 18.4228296981329); Pt(84.5316893677454, 22.7992785859543); Pt(78.8112577164131, 38.5916510741679); Pt(57.5862946433582, 38.5147490340482) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer -1.1 pli.Points |> Polyline2D
            "offset 21: dist -1.1 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        test "StepWithTwoPoints-22" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(58.4335356422746, 36.4678052921749); Pt(50.5276461535361, 28.6272537330954); Pt(54.5994051955064, 19.2269662015358); Pt(61.6398979430715, 17.3742404216405); Pt(70.6780218588907, 20.3687995503758); Pt(81.8865388997843, 24.0824648350092); Pt(77.3753683419254, 36.5364351207969); Pt(58.4335356422746, 36.4678052921749) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer 0.95 pli.Points |> Polyline2D
            "offset 22: dist 0.95 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(57.7735758041577, 71.8535626568735); Pt(49.1307186613005, 63.282134085445); Pt(55.1851060883159, 48.8941788146829); Pt(61.403586501743, 50.8209069929926); Pt(70.7094720422513, 53.9041823226791); Pt(82.8450043755862, 57.9249912283021); Pt(77.7734445305079, 71.9260259493603); Pt(57.7735758041577, 71.8535626568735) |]))
        test "StepWithTwoPoints-23" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(57.3189586827879, 72.9519227134884); Pt(47.8287342965727, 63.5401299337709); Pt(54.5580271642326, 47.5482949296675); Pt(61.7393770071303, 49.7733576750183); Pt(71.0554349457503, 52.8600033775732); Pt(84.2643534071751, 57.2364522653946); Pt(78.5439217558428, 73.0288247536082); Pt(57.3189586827879, 72.9519227134884) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer -1.1 pli.Points |> Polyline2D
            "offset 23: dist -1.1 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        test "StepWithTwoPoints-24" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(58.1661996817043, 70.9049789716152); Pt(50.2551597035654, 63.0593194891634); Pt(55.7266742500242, 50.0565330790145); Pt(61.1135856107267, 51.7256086766977); Pt(70.4106858983204, 54.8059732298161); Pt(81.619202939214, 58.5196385144495); Pt(77.108032381355, 70.9736088002371); Pt(58.1661996817043, 70.9049789716152) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer 0.95 pli.Points |> Polyline2D
            "offset 24: dist 0.95 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(55.1425455331334, -88.9061900806692); Pt(50.7713033584764, -88.9061900806692); Pt(45.9317138079633, -88.9061900806692); Pt(40.4676610896421, -88.9061900806692); Pt(34.0669136196086, -112.167443081522); Pt(71.3785878961451, -112.167443081522); Pt(71.3785878961451, -88.9061900806692); Pt(63.7289140904954, -88.9061900806692); Pt(55.1425455331334, -88.9061900806692) |]))
        test "StepWithTwoPoints-25" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(55.1425455331334, -87.8061900806692); Pt(50.7713033584764, -87.8061900806692); Pt(45.9317138079633, -87.8061900806692); Pt(39.6294609330463, -87.8061900806692); Pt(32.6233443354961, -113.267443081522); Pt(72.4785878961451, -113.267443081522); Pt(72.4785878961451, -87.8061900806692); Pt(63.7289140904954, -87.8061900806692); Pt(55.1425455331334, -87.8061900806692) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer -1.1 pli.Points |> Polyline2D
            "offset 25: dist -1.1 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        test "StepWithTwoPoints-26" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(55.1425455331334, -89.8561900806692); Pt(50.7713033584764, -89.8561900806692); Pt(45.9317138079633, -89.8561900806692); Pt(41.1915612248839, -89.8561900806692); Pt(35.3136325467968, -111.217443081522); Pt(70.4285878961451, -111.217443081522); Pt(70.4285878961451, -89.8561900806692); Pt(63.7289140904954, -89.8561900806692); Pt(55.1425455331334, -89.8561900806692) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer 0.95 pli.Points |> Polyline2D
            "offset 26: dist 0.95 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(-58.6285353445744, -151.87972174524); Pt(-97.0310928093188, -141.809289486606); Pt(-89.273487098122, -134.051683775409); Pt(-49.7627002460637, -134.051683775409); Pt(-38.3913030544957, -134.051683775409); Pt(-29.3327324103653, -134.051683775409); Pt(-19.1177484925161, -134.051683775409); Pt(-0.229665021775986, -134.051683775409); Pt(28.9697701396233, -134.051683775409); Pt(11.141732169792, -151.87972174524); Pt(-1.96428493235413, -151.87972174524); Pt(-20.8523684030942, -151.87972174524); Pt(-38.1985675088759, -151.87972174524); Pt(-58.6285353445744, -151.87972174524) |]))
        test "StepWithTwoPoints-27" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-58.4867047451984, -150.77972174524); Pt(-94.8977094838694, -141.231541079767); Pt(-88.8178521795116, -135.151683775409); Pt(-49.7627002460637, -135.151683775409); Pt(-38.3913030544957, -135.151683775409); Pt(-29.3327324103653, -135.151683775409); Pt(-19.1177484925161, -135.151683775409); Pt(-0.229665021775986, -135.151683775409); Pt(26.3141352210128, -135.151683775409); Pt(10.6860972511816, -150.77972174524); Pt(-1.96428493235413, -150.77972174524); Pt(-20.8523684030942, -150.77972174524); Pt(-38.1985675088759, -150.77972174524); Pt(-58.4867047451984, -150.77972174524) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer -1.1 pli.Points |> Polyline2D
            "offset 27: dist -1.1 on 14 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        test "StepWithTwoPoints-28" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-58.7510254076718, -152.82972174524); Pt(-98.8735602267524, -142.308254019785); Pt(-89.6669899823765, -133.101683775409); Pt(-49.7627002460637, -133.101683775409); Pt(-38.3913030544957, -133.101683775409); Pt(-29.3327324103653, -133.101683775409); Pt(-19.1177484925161, -133.101683775409); Pt(-0.229665021775986, -133.101683775409); Pt(31.2632730238777, -133.101683775409); Pt(11.5352350540465, -152.82972174524); Pt(-1.96428493235413, -152.82972174524); Pt(-20.8523684030942, -152.82972174524); Pt(-38.1985675088759, -152.82972174524); Pt(-58.7510254076718, -152.82972174524) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer 0.95 pli.Points |> Polyline2D
            "offset 28: dist 0.95 on 14 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(-9.71481756112657, 133.18006730673); Pt(0.0450687618714056, 130.411350164357); Pt(9.46835130176811, 140.479460601206); Pt(11.4307531873941, 131.541576146496); Pt(2.70339097759208, 119.752882776395); Pt(-2.68402868566895, 123.204856146808); Pt(-1.96534222581438, 130.114790826999); Pt(-9.71481756112657, 133.18006730673) |]))
        test "StepWithTwoPoints-29" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-8.19205583817846, 133.885448295335); Pt(-8.93403338663952, 131.694556728905); Pt(0.389962380872315, 129.170104043065); Pt(8.84241840226326, 138.200955057632); Pt(10.2490978580351, 131.794143826663); Pt(2.43109588955371, 121.233790565541); Pt(-1.51980273019576, 123.765317284646); Pt(-0.784936892252859, 130.830811380524); Pt(-8.19205583817846, 133.885448295335) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer -1.1 pli.Points |> Polyline2D
            "offset 29: dist -1.1 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        test "StepWithTwoPoints-30" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-11.0299299582181, 132.570874634754); Pt(-10.3891311663654, 134.463008260307); Pt(-0.252793909083925, 131.483335450927); Pt(10.0089297149768, 142.447260843384); Pt(12.4512736991132, 131.323449513625); Pt(2.93855491726158, 118.473916958497); Pt(-3.68949655630489, 122.720821527766); Pt(-2.98478319570842, 129.496409439865); Pt(-11.0299299582181, 132.570874634754) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer 0.95 pli.Points |> Polyline2D
            "offset 30: dist 0.95 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(-9.03912849186402, 97.2392202608982); Pt(0.717299346739111, 93.4410984525243); Pt(10.1440403710307, 104.538613555374); Pt(12.1064422566566, 95.6007291006641); Pt(3.37908004685463, 83.8120357305632); Pt(-2.0083396164064, 87.2640091009753); Pt(-1.28965315655184, 94.1739437811671); Pt(-9.03912849186402, 97.2392202608982) |]))
        test "StepWithTwoPoints-31" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-7.60666873646086, 97.8619677677274); Pt(-8.41250294519408, 95.8084526933415); Pt(1.04840149612905, 92.1317889666314); Pt(9.54557906170971, 102.134986903504); Pt(10.9247869272977, 95.8532967808308); Pt(3.10678495881626, 85.2929435197091); Pt(-0.844113660933211, 87.8244702388134); Pt(-0.109247822990313, 94.8899643346913); Pt(-7.60666873646086, 97.8619677677274) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer -1.1 pli.Points |> Polyline2D
            "offset 31: dist -1.1 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        test "StepWithTwoPoints-32" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-10.2762528260758, 96.7013928686366); Pt(-9.5803051003517, 98.4748831601518); Pt(0.431347490447801, 94.5718657357955); Pt(10.6608933199897, 106.614472936534); Pt(13.1269627683758, 95.3826024677929); Pt(3.61424398652413, 82.5330699126645); Pt(-3.01380748704234, 86.7799744819333); Pt(-2.30909412644588, 93.5555623940326); Pt(-10.2762528260758, 96.7013928686366) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer 0.95 pli.Points |> Polyline2D
            "offset 32: dist 0.95 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(69.6107035472615, 209.541610417257); Pt(69.0251189595104, 212.208692742297); Pt(59.6018364196137, 202.140582305448); Pt(49.8419500966157, 204.909299447822); Pt(57.5914254319279, 201.84402296809); Pt(56.8727389720734, 194.934088287899); Pt(62.2601586353344, 191.482114917487); Pt(68.8084032481212, 200.327313042283); Pt(69.6107035472615, 209.541610417257) |]))
        test "StepWithTwoPoints-33" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(70.7211238467643, 209.613487220479); Pt(69.6510518590153, 214.487198285871); Pt(59.2569428006128, 203.38182842674); Pt(49.135666646819, 206.253065843068); Pt(48.464486979348, 204.271224376205); Pt(56.4110200983664, 201.128002414566); Pt(55.7085130166002, 194.373627150061); Pt(62.5324537233728, 190.001207128341); Pt(69.8773134025589, 199.922452118848); Pt(70.7211238467643, 209.613487220479) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer -1.1 pli.Points |> Polyline2D
            "offset 33: dist -1.1 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        test "StepWithTwoPoints-34" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(68.6517041976909, 209.479534996293); Pt(68.4845405463017, 210.240892500119); Pt(59.8996990905691, 201.068597018878); Pt(50.4519221668948, 203.748773924654); Pt(51.0315773342561, 205.4603642824); Pt(58.610866401822, 202.462404355225); Pt(57.8782068427093, 195.418122906941); Pt(62.0249946956649, 192.761080735385); Pt(67.8852535692886, 200.676965657976); Pt(68.6517041976909, 209.479534996293) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer 0.95 pli.Points |> Polyline2D
            "offset 34: dist 0.95 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(66.3723353284777, 172.349442086377); Pt(60.2740670044814, 165.170330593615); Pt(50.5176391658783, 168.968452401989); Pt(58.2671145011905, 165.903175922258); Pt(57.5484280413359, 158.993241242066); Pt(62.9358477045969, 155.541267871654); Pt(65.171805743779, 158.561541704534); Pt(66.3723353284777, 172.349442086377) |]))
        test "StepWithTwoPoints-35" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(67.7674430642141, 175.690910960282); Pt(59.9429648550915, 166.479640079508); Pt(49.8943993223609, 170.391489428891); Pt(49.0929190941436, 168.349069644983); Pt(57.086709167629, 165.187155368734); Pt(56.3842020858627, 158.432780104228); Pt(63.2081427926353, 154.060360082508); Pt(66.2407158982167, 158.1566807811); Pt(67.7674430642141, 175.690910960282) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer -1.1 pli.Points |> Polyline2D
            "offset 35: dist -1.1 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        test "StepWithTwoPoints-36" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(65.1674695567053, 169.463628058913); Pt(60.5600188607727, 164.039563310344); Pt(51.0558917580069, 167.739465878756); Pt(51.748079227831, 169.503373873949); Pt(59.2865554710845, 166.521557309393); Pt(58.5538959119718, 159.477275861108); Pt(62.7006837649274, 156.820233689553); Pt(64.2486560649464, 158.911194320228); Pt(65.1674695567053, 169.463628058913) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer 0.95 pli.Points |> Polyline2D
            "offset 36: dist 0.95 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(6.40124100798643, 166.077952138966); Pt(3.37908004685463, 161.995695893676); Pt(-2.0083396164064, 165.447669264088); Pt(-1.28965315655184, 172.35760394428); Pt(-9.03912849186402, 175.422880424011); Pt(0.717299346739111, 171.624758615637); Pt(7.63926147181746, 179.773553520096); Pt(6.40124100798643, 166.077952138966) |]))
        test "StepWithTwoPoints-37" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(5.33349574890914, 166.484386572687); Pt(3.10678495881626, 163.476603682822); Pt(-0.844113660933212, 166.008130401926); Pt(-0.109247822990313, 173.073624497804); Pt(-7.60666873646086, 176.04562793084); Pt(-8.41250294519408, 173.992112856454); Pt(1.04840149612905, 170.315449129744); Pt(6.2313613684136, 176.417024988455); Pt(5.33349574890914, 166.484386572687) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer -1.1 pli.Points |> Polyline2D
            "offset 37: dist -1.1 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        test "StepWithTwoPoints-38" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(7.32338464082591, 165.72694058257); Pt(3.61424398652412, 160.716730075777); Pt(-3.01380748704234, 164.963634645046); Pt(-2.30909412644588, 171.739222557145); Pt(-10.2762528260758, 174.885053031749); Pt(-9.58030510035169, 176.658543323264); Pt(0.431347490447801, 172.755525898908); Pt(8.85517519748442, 182.672373615605); Pt(7.32338464082591, 165.72694058257) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer 0.95 pli.Points |> Polyline2D
            "offset 38: dist 0.95 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(10.1959887150057, 208.057348647867); Pt(2.70339097759208, 197.936542939508); Pt(-2.68402868566895, 201.38851630992); Pt(-1.96534222581438, 208.298450990112); Pt(-9.71481756112657, 211.363727469843); Pt(0.0450687618714056, 208.595010327469); Pt(9.46835130176811, 218.663120764318); Pt(10.662894739718, 213.222496546246); Pt(10.1959887150057, 208.057348647867) |]))
        test "StepWithTwoPoints-39" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(9.12824345592844, 208.463783081588); Pt(2.43109588955371, 199.417450728654); Pt(-1.51980273019576, 201.948977447758); Pt(-0.784936892252859, 209.014471543636); Pt(-8.19205583817846, 212.069108458448); Pt(-8.93403338663952, 209.878216892017); Pt(0.389962380872315, 207.353764206178); Pt(8.84241840226326, 216.384615220744); Pt(9.55207624742154, 213.15243333748); Pt(9.12824345592844, 208.463783081588) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer -1.1 pli.Points |> Polyline2D
            "offset 39: dist -1.1 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }

        test "StepWithTwoPoints-40" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(11.1181323478452, 207.706337091471); Pt(2.93855491726158, 196.657577121609); Pt(-3.68949655630489, 200.904481690878); Pt(-2.98478319570842, 207.680069602977); Pt(-11.0299299582181, 210.754534797866); Pt(-10.3891311663654, 212.64666842342); Pt(-0.252793909083925, 209.666995614039); Pt(10.0089297149768, 220.630921006496); Pt(11.6222379830649, 213.283005681089); Pt(11.1181323478452, 207.706337091471) |]))
            let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurn.Chamfer 0.95 pli.Points |> Polyline2D
            "offset 40: dist 0.95 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)
            }


        let pli = Polyline2D(ResizeArray<Pt>([| Pt(-6, 5.89285714285714); Pt(13.9998687263502, 5.96532043534392); Pt(19.0714285714286, -8.03571428571429); Pt(6.93589623809369, -12.0565231913373); Pt(-2.36998930241467, -15.1397985210238); Pt(-8.87775543012746, -16.690455270762); Pt(-14.6428571428571, -2.67857142857143); Pt(-6, 5.89285714285714) |]))
        test "VarDistParallelBehavior.Skip-1-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-5.50405404941479, 4.69464617200452); Pt(13.7966614854475, 4.76457630075403); Pt(18.0928167079919, -7.09580201978812); Pt(4.09467132368906, -12.3658173237221); Pt(-8.74912052166877, -15.4262088814987); Pt(-13.6882239047829, -3.42188591596383); Pt(-5.50405404941479, 4.69464617200452) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 1 VarDistParallelBehavior.Skip: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-2-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-5.50405404941479, 4.69464617200452); Pt(13.7966614854475, 4.76457630075403); Pt(18.0928167079919, -7.09580201978812); Pt(10.1700689252922, -10.0785544428953); Pt(4.09467132368906, -12.3658173237221); Pt(-8.74912052166877, -15.4262088814987); Pt(-13.6882239047829, -3.42188591596383); Pt(-5.50405404941479, 4.69464617200452) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 2 VarDistParallelBehavior.Proportional: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-3-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-5.50405404941479, 4.69464617200452); Pt(13.7966614854475, 4.76457630075403); Pt(18.0928167079919, -7.09580201978812); Pt(6.55848216154937, -10.9174188875853); Pt(6.74718919982153, -11.4869710394613); Pt(4.09467132368906, -12.3658173237221); Pt(-8.74912052166877, -15.4262088814987); Pt(-13.6882239047829, -3.42188591596383); Pt(-5.50405404941479, 4.69464617200452) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 3 VarDistParallelBehavior.StepWithTwoPoints: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Skip-4-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-6.37195946293891, 6.79151537099661); Pt(14.1522741570273, 6.86587853628635); Pt(19.8053874690061, -8.74064848515891); Pt(-7.21848477199245, -17.2202844190001); Pt(-8.97423161147148, -17.6386400627095); Pt(-15.3588320714128, -2.12108556302713); Pt(-6.37195946293891, 6.79151537099661) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 4 VarDistParallelBehavior.Skip: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-5-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-6.37195946293891, 6.79151537099661); Pt(14.1522741570273, 6.86587853628635); Pt(19.8053874690061, -8.74064848515891); Pt(4.51026672269482, -13.5399997526688); Pt(-7.21848477199245, -17.2202844190001); Pt(-8.97423161147148, -17.6386400627095); Pt(-15.3588320714128, -2.12108556302713); Pt(-6.37195946293891, 6.79151537099661) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 5 VarDistParallelBehavior.Proportional: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-6-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-6.37195946293891, 6.79151537099661); Pt(14.1522741570273, 6.86587853628635); Pt(19.8053874690061, -8.74064848515891); Pt(7.21895679550192, -12.9108514191512); Pt(7.07742651679781, -12.4836873052443); Pt(-7.21848477199245, -17.2202844190001); Pt(-8.97423161147148, -17.6386400627095); Pt(-15.3588320714128, -2.12108556302713); Pt(-6.37195946293891, 6.79151537099661) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 6 VarDistParallelBehavior.StepWithTwoPoints: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(-4.15630372020485, 41.2303691722641); Pt(15.8435650061454, 41.3028324647508); Pt(20.9151248512237, 27.3017977436926); Pt(8.77959251788884, 23.2809888380696); Pt(-0.526293022619517, 20.1977135083831); Pt(-8.28604599902698, 22.2397146722904); Pt(-12.799160863062, 32.6589406008355); Pt(-4.15630372020485, 41.2303691722641) |]))
        test "VarDistParallelBehavior.Skip-7-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-3.66035776961964, 40.0321582014114); Pt(15.6403577652427, 40.1020883301609); Pt(19.9365129877871, 28.2417100096188); Pt(0.497762503796897, 21.1690845822945); Pt(-8.15468365016976, 23.4460005061342); Pt(-11.8296955225494, 31.9303356365224); Pt(-3.66035776961964, 40.0321582014114) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 7 VarDistParallelBehavior.Skip: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-8-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-3.66035776961964, 40.0321582014114); Pt(15.6403577652427, 40.1020883301609); Pt(19.9365129877871, 28.2417100096188); Pt(8.93446142542887, 24.2387063342151); Pt(0.497762503796897, 21.1690845822945); Pt(-8.15468365016976, 23.4460005061342); Pt(-11.8296955225494, 31.9303356365224); Pt(-3.66035776961964, 40.0321582014114) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 8 VarDistParallelBehavior.Proportional: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-9-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-3.66035776961964, 40.0321582014114); Pt(15.6403577652427, 40.1020883301609); Pt(19.9365129877871, 28.2417100096188); Pt(8.40217844134452, 24.4200931418216); Pt(8.59088547961668, 23.8505409899456); Pt(0.497762503796897, 21.1690845822945); Pt(-8.15468365016976, 23.4460005061342); Pt(-11.8296955225494, 31.9303356365224); Pt(-3.66035776961964, 40.0321582014114) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 9 VarDistParallelBehavior.StepWithTwoPoints: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Skip-10-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-4.52826318314376, 42.1290274004035); Pt(15.9959704368224, 42.2033905656933); Pt(21.6490837488012, 26.596863544248); Pt(-1.29433466743183, 19.4691852029496); Pt(-8.3845677606699, 21.3350002969076); Pt(-13.5262598684464, 33.2053943240703); Pt(-4.52826318314376, 42.1290274004035) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 10 VarDistParallelBehavior.Skip: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-11-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-4.52826318314376, 42.1290274004035); Pt(15.9959704368224, 42.2033905656933); Pt(21.6490837488012, 26.596863544248); Pt(8.66344083723381, 22.5627007159605); Pt(-1.29433466743183, 19.4691852029496); Pt(-8.3845677606699, 21.3350002969076); Pt(-13.5262598684464, 33.2053943240703); Pt(-4.52826318314376, 42.1290274004035) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 11 VarDistParallelBehavior.Proportional: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-12-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-4.52826318314376, 42.1290274004035); Pt(15.9959704368224, 42.2033905656933); Pt(21.6490837488012, 26.596863544248); Pt(9.06265307529708, 22.4266606102557); Pt(8.92112279659296, 22.8538247241626); Pt(-1.29433466743183, 19.4691852029496); Pt(-8.3845677606699, 21.3350002969076); Pt(-13.5262598684464, 33.2053943240703); Pt(-4.52826318314376, 42.1290274004035) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 12 VarDistParallelBehavior.StepWithTwoPoints: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(-4.42363968077515, 75.6675428517043); Pt(15.5762290455751, 75.7400061441911); Pt(20.6477888906534, 61.7389714231329); Pt(8.51225655731853, 57.7181625175099); Pt(-0.79362898318982, 54.6348871878234); Pt(-7.0121093966169, 52.7081590095137); Pt(-13.0664968236323, 67.0961142802758); Pt(-4.42363968077515, 75.6675428517043) |]))
        test "VarDistParallelBehavior.Skip-13-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-3.92769373018994, 74.4693318808517); Pt(15.3730218046724, 74.5392620096012); Pt(19.6691770272168, 62.6788836890591); Pt(-6.90389819646943, 53.9979674327959); Pt(-12.1054380560827, 66.3591722188093); Pt(-3.92769373018994, 74.4693318808517) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 13 VarDistParallelBehavior.Skip: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-14-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-3.92769373018994, 74.4693318808517); Pt(15.3730218046724, 74.5392620096012); Pt(19.6691770272168, 62.6788836890591); Pt(7.99386242973371, 58.8647813530247); Pt(-0.959114523337988, 55.9400147417221); Pt(-6.90389819646943, 53.9979674327959); Pt(-12.1054380560827, 66.3591722188093); Pt(-3.92769373018994, 74.4693318808517) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 14 VarDistParallelBehavior.Proportional: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-15-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-3.92769373018994, 74.4693318808517); Pt(15.3730218046724, 74.5392620096012); Pt(19.6691770272168, 62.6788836890591); Pt(8.13484248077422, 58.8572668212618); Pt(8.32354951904637, 58.2877146693859); Pt(-0.982336021461979, 55.2044393396994); Pt(-1.14877921132698, 55.7811281378222); Pt(-6.90389819646943, 53.9979674327959); Pt(-12.1054380560827, 66.3591722188093); Pt(-3.92769373018994, 74.4693318808517) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 15 VarDistParallelBehavior.StepWithTwoPoints: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Skip-16-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-4.79559914371406, 76.5662010798438); Pt(15.7286344762521, 76.6405642451335); Pt(21.3817477882309, 61.0340372236883); Pt(-7.0932677967275, 51.7408026920521); Pt(-13.7872908992945, 67.6488208263756); Pt(-4.79559914371406, 76.5662010798438) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 16 VarDistParallelBehavior.Skip: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-17-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-4.79559914371406, 76.5662010798438); Pt(15.7286344762521, 76.6405642451335); Pt(21.3817477882309, 61.0340372236883); Pt(8.87078463006828, 56.9509028528372); Pt(-0.722992383863851, 53.8198345108838); Pt(-7.0932677967275, 51.7408026920521); Pt(-13.7872908992945, 67.6488208263756); Pt(-4.79559914371406, 76.5662010798438) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 17 VarDistParallelBehavior.Proportional: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-18-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-4.79559914371406, 76.5662010798438); Pt(15.7286344762521, 76.6405642451335); Pt(21.3817477882309, 61.0340372236883); Pt(8.79531711472677, 56.8638342896959); Pt(8.65378683602265, 57.2909984036029); Pt(-0.652098704485701, 54.2077230739164); Pt(-0.527266312086947, 53.7752064753243); Pt(-7.0932677967275, 51.7408026920521); Pt(-13.7872908992945, 67.6488208263756); Pt(-4.79559914371406, 76.5662010798438) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 18 VarDistParallelBehavior.StepWithTwoPoints: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(0.836834692090122, -36.2604140142501); Pt(6.16306838927609, -36.2604140142501); Pt(13.025715652958, -36.2604140142501); Pt(17.7373839235456, -39.5380962894415); Pt(17.3276736391467, -48.6541501173175); Pt(9.95288851996611, -58.692052085091); Pt(-12.5811771219745, -60.4333207937864); Pt(-22.5166515186484, -46.9128814086221); Pt(-15.8588593971659, -36.2604140142501); Pt(-9.61077756008235, -36.2604140142501); Pt(0.836834692090122, -36.2604140142501) |]))
        test "VarDistParallelBehavior.Skip-19-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(11.7868767940397, -36.8604140142501); Pt(17.0912799393531, -40.5504335935986); Pt(16.784958751057, -47.366080033187); Pt(8.84295388163862, -58.176031105451); Pt(-11.5904933906593, -59.7549793037649); Pt(-21.4498655944045, -46.3381016656786); Pt(-15.9013108122617, -37.4604140142501); Pt(11.7868767940397, -36.8604140142501) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6 |])  pli.Points |> Polyline2D
            "offset 19 VarDistParallelBehavior.Skip: dist -1.2 on 11 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-20-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(0.102854364430277, -37.1136055036118); Pt(5.2084776109823, -37.0029672057395); Pt(11.7868767940397, -36.8604140142501); Pt(17.0912799393531, -40.5504335935986); Pt(16.784958751057, -47.366080033187); Pt(8.84295388163862, -58.176031105451); Pt(-11.5904933906593, -59.7549793037649); Pt(-21.4498655944045, -46.3381016656786); Pt(-15.9013108122617, -37.4604140142501); Pt(-9.91202200380639, -37.3306267802076); Pt(0.102854364430277, -37.1136055036118) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6 |])  pli.Points |> Polyline2D
            "offset 20 VarDistParallelBehavior.Proportional: dist -1.2 on 11 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-21-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(0.836834692090122, -36.8604140142501); Pt(0.836834692090122, -37.4604140142501); Pt(6.16306838927609, -37.4604140142501); Pt(6.16306838927609, -36.8604140142501); Pt(11.7868767940397, -36.8604140142501); Pt(17.0912799393531, -40.5504335935986); Pt(16.784958751057, -47.366080033187); Pt(8.84295388163862, -58.176031105451); Pt(-11.5904933906593, -59.7549793037649); Pt(-21.4498655944045, -46.3381016656786); Pt(-15.9013108122617, -37.4604140142501); Pt(-9.61077756008235, -37.4604140142501); Pt(-9.61077756008235, -36.8604140142501); Pt(0.836834692090122, -36.8604140142501) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6 |])  pli.Points |> Polyline2D
            "offset 21 VarDistParallelBehavior.StepWithTwoPoints: dist -1.2 on 11 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Skip-22-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(13.9548447971467, -35.8104140142501); Pt(18.22196191169, -38.7788433113237); Pt(17.7347098052139, -49.6202026804153); Pt(10.7853394987117, -59.0790678198211); Pt(-13.324189920461, -60.9420769113026); Pt(-23.3167409618313, -47.3439662158296); Pt(-15.8270208358441, -35.3604140142501); Pt(13.9548447971467, -35.8104140142501) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45 |])  pli.Points |> Polyline2D
            "offset 22 VarDistParallelBehavior.Skip: dist 0.9 on 11 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-23-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(1.387319937835, -35.6205203972288); Pt(6.87901147299643, -35.7034991206331); Pt(13.9548447971467, -35.8104140142501); Pt(18.22196191169, -38.7788433113237); Pt(17.7347098052139, -49.6202026804153); Pt(10.7853394987117, -59.0790678198211); Pt(-13.324189920461, -60.9420769113026); Pt(-23.3167409618313, -47.3439662158296); Pt(-15.8270208358441, -35.3604140142501); Pt(-9.38484422728933, -35.457754439782); Pt(1.387319937835, -35.6205203972288) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45 |])  pli.Points |> Polyline2D
            "offset 23 VarDistParallelBehavior.Proportional: dist 0.9 on 11 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-24-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(0.836834692090122, -35.8104140142501); Pt(0.836834692090122, -35.3604140142501); Pt(6.16306838927609, -35.3604140142501); Pt(6.16306838927609, -35.8104140142501); Pt(13.9548447971467, -35.8104140142501); Pt(18.22196191169, -38.7788433113237); Pt(17.7347098052139, -49.6202026804153); Pt(10.7853394987117, -59.0790678198211); Pt(-13.324189920461, -60.9420769113026); Pt(-23.3167409618313, -47.3439662158296); Pt(-15.8270208358441, -35.3604140142501); Pt(-9.61077756008235, -35.3604140142501); Pt(-9.61077756008235, -35.8104140142501); Pt(0.836834692090122, -35.8104140142501) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45 |])  pli.Points |> Polyline2D
            "offset 24 VarDistParallelBehavior.StepWithTwoPoints: dist 0.9 on 11 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(58.0465376145519, -43.7722240678395); Pt(47.5989253623794, -43.7722240678395); Pt(41.3508435252959, -43.7722240678395); Pt(34.6930514038134, -54.4246914622115); Pt(44.6285258004872, -67.9451308473758); Pt(67.1625914424279, -66.2038621386804); Pt(74.5373765616085, -56.1659601709069); Pt(74.9470868460074, -47.0499063430309); Pt(70.2354185754198, -43.7722240678395); Pt(63.3727713117379, -43.7722240678395); Pt(58.0465376145519, -43.7722240678395) |]))
        test "VarDistParallelBehavior.Skip-25-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(40.3107463554874, -43.1722240678395); Pt(33.5861302015602, -53.9316099141231); Pt(44.7608764268033, -69.1384810660003); Pt(67.0144461301883, -67.418887043466); Pt(75.7683453388751, -55.5038575649758); Pt(76.1437341651947, -47.1514561793635); Pt(69.5610880048789, -42.5722240678395); Pt(40.3107463554874, -43.1722240678395) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6 |])  pli.Points |> Polyline2D
            "offset 25 VarDistParallelBehavior.Skip: dist -1.2 on 11 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-26-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(57.2178587272988, -42.8254155572012); Pt(46.6379479179444, -43.042436833797); Pt(40.3107463554874, -43.1722240678395); Pt(33.5861302015602, -53.9316099141231); Pt(44.7608764268033, -69.1384810660003); Pt(67.0144461301883, -67.418887043466); Pt(75.7683453388751, -55.5038575649758); Pt(76.1437341651947, -47.1514561793635); Pt(69.5610880048789, -42.5722240678395); Pt(62.611538747754, -42.7147772593289); Pt(57.2178587272988, -42.8254155572012) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6 |])  pli.Points |> Polyline2D
            "offset 26 VarDistParallelBehavior.Proportional: dist -1.2 on 11 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-27-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(58.0465376145519, -43.1722240678395); Pt(58.0465376145519, -42.5722240678395); Pt(47.5989253623794, -42.5722240678395); Pt(47.5989253623794, -43.1722240678395); Pt(40.3107463554874, -43.1722240678395); Pt(33.5861302015602, -53.9316099141231); Pt(44.7608764268033, -69.1384810660003); Pt(67.0144461301883, -67.418887043466); Pt(75.7683453388751, -55.5038575649758); Pt(76.1437341651947, -47.1514561793635); Pt(69.5610880048789, -42.5722240678395); Pt(63.3727713117379, -42.5722240678395); Pt(63.3727713117379, -43.1722240678395); Pt(58.0465376145519, -43.1722240678395) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6 |])  pli.Points |> Polyline2D
            "offset 27 VarDistParallelBehavior.StepWithTwoPoints: dist -1.2 on 11 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Skip-28-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(42.1309164026522, -44.2222240678395); Pt(35.5232423055033, -54.7945026232778); Pt(44.5292628307502, -67.0501181834074); Pt(67.2737004266075, -65.2925934600912); Pt(73.6141499786585, -56.6625371253552); Pt(74.0496013566169, -46.9737439657814); Pt(70.7411665033254, -44.6722240678395); Pt(42.1309164026522, -44.2222240678395) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45 |])  pli.Points |> Polyline2D
            "offset 28 VarDistParallelBehavior.Skip: dist 0.9 on 11 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-29-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(58.6680467799917, -44.4823304508182); Pt(48.3196584457057, -44.3195644933714); Pt(42.1309164026522, -44.2222240678395); Pt(35.5232423055033, -54.7945026232778); Pt(44.5292628307502, -67.0501181834074); Pt(67.2737004266075, -65.2925934600912); Pt(73.6141499786585, -56.6625371253552); Pt(74.0496013566169, -46.9737439657814); Pt(70.7411665033254, -44.6722240678395); Pt(63.9436957347258, -44.5653091742225); Pt(58.6680467799917, -44.4823304508182) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45 |])  pli.Points |> Polyline2D
            "offset 29 VarDistParallelBehavior.Proportional: dist 0.9 on 11 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-30-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(58.0465376145519, -44.2222240678395); Pt(58.0465376145519, -44.6722240678395); Pt(47.5989253623794, -44.6722240678395); Pt(47.5989253623794, -44.2222240678395); Pt(42.1309164026522, -44.2222240678395); Pt(35.5232423055033, -54.7945026232778); Pt(44.5292628307502, -67.0501181834074); Pt(67.2737004266075, -65.2925934600912); Pt(73.6141499786585, -56.6625371253552); Pt(74.0496013566169, -46.9737439657814); Pt(70.7411665033254, -44.6722240678395); Pt(63.3727713117379, -44.6722240678395); Pt(63.3727713117379, -44.2222240678395); Pt(58.0465376145519, -44.2222240678395) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45 |])  pli.Points |> Polyline2D
            "offset 30 VarDistParallelBehavior.StepWithTwoPoints: dist 0.9 on 11 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(60.3045787716737, 3.72302516341453); Pt(51.6617216288165, -4.84840340801404); Pt(57.4268233415462, -18.8602872502046); Pt(63.934589469259, -17.3096305004664); Pt(73.2404750097674, -14.2263551707799); Pt(85.3760073431022, -10.2055462651569); Pt(80.3044474980239, 3.79548845590132); Pt(60.3045787716737, 3.72302516341453) |]))
        test "VarDistParallelBehavior.Skip-31-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(59.8086328210885, 4.92123613426716); Pt(50.7070883907423, -4.10508892062164); Pt(57.2981884330875, -20.1245336394679); Pt(57.4699288431552, -20.0836116977681); Pt(86.3546192065389, -11.1454585310831); Pt(80.5076547389266, 4.99623259049122); Pt(59.8086328210885, 4.92123613426716) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 31 VarDistParallelBehavior.Skip: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-32-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(59.8086328210885, 4.92123613426716); Pt(50.7070883907423, -4.10508892062164); Pt(57.2981884330875, -20.1245336394679); Pt(57.4699288431552, -20.0836116977681); Pt(70.0063023225688, -16.2043239192219); Pt(86.3546192065389, -11.1454585310831); Pt(80.5076547389266, 4.99623259049122); Pt(59.8086328210885, 4.92123613426716) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 32 VarDistParallelBehavior.Proportional: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-33-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(59.8086328210885, 4.92123613426716); Pt(50.7070883907423, -4.10508892062164); Pt(57.2981884330875, -20.1245336394679); Pt(57.4699288431552, -20.0836116977681); Pt(73.4291820480395, -14.7959073226559); Pt(73.6178890863117, -15.3654594745318); Pt(86.3546192065389, -11.1454585310831); Pt(80.5076547389266, 4.99623259049122); Pt(59.8086328210885, 4.92123613426716) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 33 VarDistParallelBehavior.StepWithTwoPoints: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Skip-34-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(60.6765382346126, 2.82436693527507); Pt(52.3776965573722, -5.40588927355834); Pt(57.5232995228902, -17.9121024582572); Pt(68.7830849388368, -15.2291446024901); Pt(84.6420484455248, -9.50061206571227); Pt(80.1520420673469, 2.8949303549589); Pt(60.6765382346126, 2.82436693527507) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 34 VarDistParallelBehavior.Skip: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-35-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(60.6765382346126, 2.82436693527507); Pt(52.3776965573722, -5.40588927355834); Pt(57.5232995228902, -17.9121024582572); Pt(68.7830849388368, -15.2291446024901); Pt(75.6661045251662, -12.7428786094484); Pt(84.6420484455248, -9.50061206571227); Pt(80.1520420673469, 2.8949303549589); Pt(60.6765382346126, 2.82436693527507) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 35 VarDistParallelBehavior.Proportional: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-36-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(60.6765382346126, 2.82436693527507); Pt(52.3776965573722, -5.40588927355834); Pt(57.5232995228902, -17.9121024582572); Pt(68.7830849388368, -15.2291446024901); Pt(73.0989447310632, -13.7991910568729); Pt(72.9574144523591, -13.3720269429659); Pt(84.6420484455248, -9.50061206571227); Pt(80.1520420673469, 2.8949303549589); Pt(60.6765382346126, 2.82436693527507) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 36 VarDistParallelBehavior.StepWithTwoPoints: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(-7.0546699517994, -85.0922098858384); Pt(1.53169860556255, -85.0922098858384); Pt(9.18137241121229, -85.0922098858384); Pt(9.18137241121229, -108.353462886692); Pt(-28.1303018653242, -108.353462886692); Pt(-21.7295543952907, -85.0922098858384); Pt(-16.2655016769695, -85.0922098858384); Pt(-11.4259121264564, -85.0922098858384); Pt(-7.0546699517994, -85.0922098858384) |]))
        test "VarDistParallelBehavior.Skip-37-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(7.98137241121229, -85.6922098858384); Pt(7.98137241121229, -107.753462886692); Pt(-26.7205996810696, -107.753462886692); Pt(-20.650053553318, -85.6922098858384); Pt(7.98137241121229, -85.6922098858384) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6 |])  pli.Points |> Polyline2D
            "offset 37 VarDistParallelBehavior.Skip: dist -1.2 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-38-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-7.05735637823798, -85.6922098858384); Pt(0.895817500798223, -85.6922098858384); Pt(7.98137241121229, -85.6922098858384); Pt(7.98137241121229, -107.753462886692); Pt(-26.7205996810696, -107.753462886692); Pt(-20.650053553318, -85.6922098858384); Pt(-15.5889429030222, -85.6922098858384); Pt(-11.1062448984746, -85.6922098858384); Pt(-7.05735637823798, -85.6922098858384) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6 |])  pli.Points |> Polyline2D
            "offset 38 VarDistParallelBehavior.Proportional: dist -1.2 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-39-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-7.0546699517994, -85.6922098858384); Pt(-7.0546699517994, -86.2922098858384); Pt(1.53169860556255, -86.2922098858384); Pt(1.53169860556255, -85.6922098858384); Pt(7.98137241121229, -85.6922098858384); Pt(7.98137241121229, -107.753462886692); Pt(-26.7205996810696, -107.753462886692); Pt(-20.650053553318, -85.6922098858384); Pt(-16.2655016769695, -85.6922098858384); Pt(-16.2655016769695, -86.2922098858384); Pt(-11.4259121264564, -86.2922098858384); Pt(-11.4259121264564, -85.6922098858384); Pt(-7.0546699517994, -85.6922098858384) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6 |])  pli.Points |> Polyline2D
            "offset 39 VarDistParallelBehavior.StepWithTwoPoints: dist -1.2 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Skip-40-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(10.0813724112123, -84.6422098858384); Pt(10.0813724112123, -108.803462886692); Pt(-29.1875785035151, -108.803462886692); Pt(-22.5391800267702, -84.6422098858384); Pt(10.0813724112123, -84.6422098858384) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45 |])  pli.Points |> Polyline2D
            "offset 40 VarDistParallelBehavior.Skip: dist 0.9 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-41-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-7.05265513197046, -84.6422098858384); Pt(2.0086094341358, -84.6422098858384); Pt(10.0813724112123, -84.6422098858384); Pt(10.0813724112123, -108.803462886692); Pt(-29.1875785035151, -108.803462886692); Pt(-22.5391800267702, -84.6422098858384); Pt(-16.7729207574299, -84.6422098858384); Pt(-11.6656625474427, -84.6422098858384); Pt(-7.05265513197046, -84.6422098858384) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45 |])  pli.Points |> Polyline2D
            "offset 41 VarDistParallelBehavior.Proportional: dist 0.9 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-42-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-7.0546699517994, -84.6422098858384); Pt(-7.0546699517994, -84.1922098858384); Pt(1.53169860556255, -84.1922098858384); Pt(1.53169860556255, -84.6422098858384); Pt(10.0813724112123, -84.6422098858384); Pt(10.0813724112123, -108.803462886692); Pt(-29.1875785035151, -108.803462886692); Pt(-22.5391800267702, -84.6422098858384); Pt(-16.2655016769695, -84.6422098858384); Pt(-16.2655016769695, -84.1922098858384); Pt(-11.4259121264564, -84.1922098858384); Pt(-11.4259121264564, -84.6422098858384); Pt(-7.0546699517994, -84.6422098858384) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45 |])  pli.Points |> Polyline2D
            "offset 42 VarDistParallelBehavior.StepWithTwoPoints: dist 0.9 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(74.2740052906831, -151.87972174524); Pt(94.7039731263815, -151.87972174524); Pt(112.050172232163, -151.87972174524); Pt(130.938255702903, -151.87972174524); Pt(144.044272805049, -151.87972174524); Pt(161.872310774881, -134.051683775409); Pt(132.672875613481, -134.051683775409); Pt(113.784792142741, -134.051683775409); Pt(103.569808224892, -134.051683775409); Pt(94.5112375807617, -134.051683775409); Pt(83.1398403891937, -134.051683775409); Pt(43.6290535371354, -134.051683775409); Pt(35.8714478259386, -141.809289486606); Pt(74.2740052906831, -151.87972174524) |]))
        test "VarDistParallelBehavior.Skip-43-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(74.1192810004547, -153.07972174524); Pt(145.141329079897, -152.47972174524); Pt(164.169367049728, -133.451683775409); Pt(43.9805253997115, -132.851683775409); Pt(34.2163640265504, -142.61584514857); Pt(74.1192810004547, -153.07972174524) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 43 VarDistParallelBehavior.Skip: dist -1.2 on 14 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-44-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(74.1192810004547, -153.07972174524); Pt(94.9157923165345, -152.904031137506); Pt(112.573207584904, -152.754859866787); Pt(131.800170877129, -152.592428927561); Pt(145.141329079897, -152.47972174524); Pt(164.169367049728, -133.451683775409); Pt(134.48948194789, -133.303517516241); Pt(115.290612377065, -133.207673995458); Pt(104.90755026223, -133.155840254627); Pt(95.6999291415281, -133.109874484456); Pt(84.1414260325615, -133.052172772964); Pt(43.9805253997115, -132.851683775409); Pt(34.2163640265504, -142.61584514857); Pt(74.1192810004547, -153.07972174524) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 44 VarDistParallelBehavior.Proportional: dist -1.2 on 14 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-45-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(74.1192810004547, -153.07972174524); Pt(94.7039731263815, -153.07972174524); Pt(94.7039731263815, -152.47972174524); Pt(112.050172232163, -152.47972174524); Pt(112.050172232163, -153.07972174524); Pt(130.938255702903, -153.07972174524); Pt(130.938255702903, -152.47972174524); Pt(145.141329079897, -152.47972174524); Pt(164.169367049728, -133.451683775409); Pt(132.672875613481, -133.451683775409); Pt(132.672875613481, -132.851683775409); Pt(113.784792142741, -132.851683775409); Pt(113.784792142741, -133.451683775409); Pt(103.569808224892, -133.451683775409); Pt(103.569808224892, -132.851683775409); Pt(94.5112375807617, -132.851683775409); Pt(94.5112375807617, -133.451683775409); Pt(83.1398403891937, -133.451683775409); Pt(83.1398403891937, -132.851683775409); Pt(43.9805253997115, -132.851683775409); Pt(34.2163640265504, -142.61584514857); Pt(74.1192810004547, -153.07972174524) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 45 VarDistParallelBehavior.StepWithTwoPoints: dist -1.2 on 14 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Skip-46-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(74.3900485083543, -150.97972174524); Pt(143.221480598914, -151.42972174524); Pt(160.149518568745, -134.501683775409); Pt(43.3654496402033, -134.951683775409); Pt(37.1127606754798, -141.204372740133); Pt(74.3900485083543, -150.97972174524) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 46 VarDistParallelBehavior.Skip: dist 0.9 on 14 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-47-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(74.3900485083543, -150.97972174524); Pt(94.5451087337667, -151.111489701042); Pt(111.657895717607, -151.22336815408); Pt(130.291819322234, -151.3451913585); Pt(143.221480598914, -151.42972174524); Pt(160.149518568745, -134.501683775409); Pt(131.310420862675, -134.612808469786); Pt(112.655426966999, -134.684691110373); Pt(102.566501696889, -134.723566415996); Pt(93.619718910187, -134.758040743624); Pt(82.3886511566679, -134.801317027243); Pt(43.3654496402033, -134.951683775409); Pt(37.1127606754798, -141.204372740133); Pt(74.3900485083543, -150.97972174524) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 47 VarDistParallelBehavior.Proportional: dist 0.9 on 14 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-48-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(74.3900485083543, -150.97972174524); Pt(94.7039731263815, -150.97972174524); Pt(94.7039731263815, -151.42972174524); Pt(112.050172232163, -151.42972174524); Pt(112.050172232163, -150.97972174524); Pt(130.938255702903, -150.97972174524); Pt(130.938255702903, -151.42972174524); Pt(143.221480598914, -151.42972174524); Pt(160.149518568745, -134.501683775409); Pt(132.672875613481, -134.501683775409); Pt(132.672875613481, -134.951683775409); Pt(113.784792142741, -134.951683775409); Pt(113.784792142741, -134.501683775409); Pt(103.569808224892, -134.501683775409); Pt(103.569808224892, -134.951683775409); Pt(94.5112375807617, -134.951683775409); Pt(94.5112375807617, -134.501683775409); Pt(83.1398403891937, -134.501683775409); Pt(83.1398403891937, -134.951683775409); Pt(43.3654496402033, -134.951683775409); Pt(37.1127606754798, -141.204372740133); Pt(74.3900485083543, -150.97972174524) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 48 VarDistParallelBehavior.StepWithTwoPoints: dist 0.9 on 14 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(50.5176391658783, 90.7847922388769); Pt(58.2671145011905, 87.7195157591458); Pt(57.5484280413359, 80.809581078954); Pt(62.9358477045969, 77.3576077085419); Pt(71.663209914399, 89.1463010786428); Pt(69.700808028773, 98.0841855333523); Pt(60.2740670044814, 86.986670430503); Pt(50.5176391658783, 90.7847922388769) |]))
        test "VarDistParallelBehavior.Skip-49-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(49.8377411547684, 92.3371962682241); Pt(48.9633990876223, 90.1091019585063); Pt(57.5587997005249, 86.7092228450727); Pt(56.8439103721026, 79.8357963386602); Pt(62.7265547377726, 76.0665091906073); Pt(72.7815292048893, 89.6485065899935); Pt(70.5153656699316, 99.9698928357404); Pt(60.5044767733418, 88.1846968240359); Pt(49.8377411547684, 92.3371962682241) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 49 VarDistParallelBehavior.Skip: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-50-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(49.8377411547684, 92.3371962682241); Pt(48.9633990876223, 90.1091019585063); Pt(57.5587997005249, 86.7092228450727); Pt(56.8439103721026, 79.8357963386602); Pt(62.7265547377726, 76.0665091906073); Pt(72.7815292048893, 89.6485065899935); Pt(70.5153656699316, 99.9698928357404); Pt(60.5044767733418, 88.1846968240359); Pt(49.8377411547684, 92.3371962682241) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 50 VarDistParallelBehavior.Proportional: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-51-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(49.8377411547684, 92.3371962682241); Pt(48.9633990876223, 90.1091019585063); Pt(57.5587997005249, 86.7092228450727); Pt(56.8439103721026, 79.8357963386602); Pt(62.7265547377726, 76.0665091906073); Pt(72.7815292048893, 89.6485065899935); Pt(70.5153656699316, 99.9698928357404); Pt(60.5044767733418, 88.1846968240359); Pt(49.8377411547684, 92.3371962682241) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 51 VarDistParallelBehavior.StepWithTwoPoints: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Skip-52-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(51.0275626742107, 89.6204892168665); Pt(51.6833192245703, 91.2915599491549); Pt(58.7983506016896, 88.4772354447006); Pt(58.0768162932609, 81.5399196341743); Pt(63.0928174297152, 78.3259315969929); Pt(70.8244704465312, 88.7696469451298); Pt(69.089889797904, 96.6699050565613); Pt(60.1012596778361, 86.0881506353534); Pt(51.0275626742107, 89.6204892168665) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 52 VarDistParallelBehavior.Skip: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-53-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(51.0275626742107, 89.6204892168665); Pt(51.6833192245703, 91.2915599491549); Pt(58.7983506016896, 88.4772354447006); Pt(58.0768162932609, 81.5399196341743); Pt(63.0928174297152, 78.3259315969929); Pt(70.8244704465312, 88.7696469451298); Pt(69.089889797904, 96.6699050565613); Pt(60.1012596778361, 86.0881506353534); Pt(51.0275626742107, 89.6204892168665) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 53 VarDistParallelBehavior.Proportional: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-54-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(51.0275626742107, 89.6204892168665); Pt(51.6833192245703, 91.2915599491549); Pt(58.7983506016896, 88.4772354447006); Pt(58.0768162932609, 81.5399196341743); Pt(63.0928174297152, 78.3259315969929); Pt(70.8244704465312, 88.7696469451298); Pt(69.089889797904, 96.6699050565613); Pt(60.1012596778361, 86.0881506353534); Pt(51.0275626742107, 89.6204892168665) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 54 VarDistParallelBehavior.StepWithTwoPoints: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(49.8419500966157, 126.725639284709); Pt(57.5914254319279, 123.660362804978); Pt(56.8727389720734, 116.750428124786); Pt(62.2601586353344, 113.298454754374); Pt(70.9875208451364, 125.087148124475); Pt(69.0251189595104, 134.025032579185); Pt(59.6018364196137, 123.956922142335); Pt(49.8419500966157, 126.725639284709) |]))
        test "VarDistParallelBehavior.Skip-55-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(27.1551270429985, 134.408864511195); Pt(56.8831106312624, 122.650069890905); Pt(56.1682213028401, 115.776643384492); Pt(62.05086566851, 112.00735623644); Pt(72.1058401356267, 125.589353635826); Pt(69.8641066187144, 135.799471807426); Pt(59.8749728590362, 125.126789391814); Pt(27.1551270429985, 134.408864511195) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 55 VarDistParallelBehavior.Skip: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-56-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(27.1551270429985, 134.408864511195); Pt(56.8831106312624, 122.650069890905); Pt(56.1682213028401, 115.776643384492); Pt(62.05086566851, 112.00735623644); Pt(72.1058401356267, 125.589353635826); Pt(69.8641066187144, 135.799471807426); Pt(59.8749728590362, 125.126789391814); Pt(27.1551270429985, 134.408864511195) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 56 VarDistParallelBehavior.Proportional: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-57-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(27.1551270429985, 134.408864511195); Pt(56.8831106312624, 122.650069890905); Pt(56.1682213028401, 115.776643384492); Pt(62.05086566851, 112.00735623644); Pt(72.1058401356267, 125.589353635826); Pt(69.8641066187144, 135.799471807426); Pt(59.8749728590362, 125.126789391814); Pt(27.1551270429985, 134.408864511195) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 57 VarDistParallelBehavior.StepWithTwoPoints: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Skip-58-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(66.8570673868287, 120.963220364845); Pt(58.1226615324271, 124.418082490533); Pt(57.4011272239983, 117.480766680007); Pt(62.4171283604527, 114.266778642825); Pt(70.1487813772687, 124.710493990962); Pt(68.3958782151074, 132.694203158004); Pt(59.3969840900469, 123.079521705227); Pt(66.8570673868287, 120.963220364845) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 58 VarDistParallelBehavior.Skip: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-59-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(66.8570673868287, 120.963220364845); Pt(58.1226615324271, 124.418082490533); Pt(57.4011272239983, 117.480766680007); Pt(62.4171283604527, 114.266778642825); Pt(70.1487813772687, 124.710493990962); Pt(68.3958782151074, 132.694203158004); Pt(59.3969840900469, 123.079521705227); Pt(66.8570673868287, 120.963220364845) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 59 VarDistParallelBehavior.Proportional: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-60-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(66.8570673868287, 120.963220364845); Pt(58.1226615324271, 124.418082490533); Pt(57.4011272239983, 117.480766680007); Pt(62.4171283604527, 114.266778642825); Pt(70.1487813772687, 124.710493990962); Pt(68.3958782151074, 132.694203158004); Pt(59.3969840900469, 123.079521705227); Pt(66.8570673868287, 120.963220364845) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 60 VarDistParallelBehavior.StepWithTwoPoints: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(58.040911764728, 37.4163889774333); Pt(49.3980546218708, 28.8449604060047); Pt(53.9111694859058, 18.4257344774596); Pt(61.6709224623133, 16.3837333135523); Pt(70.9768080028216, 19.4670086432388); Pt(83.1123403361565, 23.4878175488618); Pt(78.0407804910782, 37.48885226992); Pt(58.040911764728, 37.4163889774333) |]))
        test "VarDistParallelBehavior.Skip-61-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(57.5449658141427, 38.6145999482859); Pt(48.4285892813582, 29.5735653703178); Pt(53.7798071370486, 17.2194486436158); Pt(60.6468669358969, 15.412362239641); Pt(84.0909521995932, 22.5479052829357); Pt(78.2439877319809, 38.6895964045099); Pt(57.5449658141427, 38.6145999482859) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 61 VarDistParallelBehavior.Skip: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-62-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(57.5449658141427, 38.6145999482859); Pt(48.4285892813582, 29.5735653703178); Pt(53.7798071370486, 17.2194486436158); Pt(60.6468669358969, 15.412362239641); Pt(70.8219390952816, 18.5092911470934); Pt(84.0909521995932, 22.5479052829357); Pt(78.2439877319809, 38.6895964045099); Pt(57.5449658141427, 38.6145999482859) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 62 VarDistParallelBehavior.Proportional: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-63-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(57.5449658141427, 38.6145999482859); Pt(48.4285892813582, 29.5735653703178); Pt(53.7798071370486, 17.2194486436158); Pt(60.6468669358969, 15.412362239641); Pt(71.1655150410938, 18.8974564913629); Pt(71.354222079366, 18.3279043394869); Pt(84.0909521995932, 22.5479052829357); Pt(78.2439877319809, 38.6895964045099); Pt(57.5449658141427, 38.6145999482859) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 63 VarDistParallelBehavior.StepWithTwoPoints: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Skip-64-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(58.4128712276669, 36.5177307492938); Pt(50.1251536272552, 28.2985066827699); Pt(54.0096912475487, 19.3304488528425); Pt(62.4389641071256, 17.1122616189858); Pt(82.378381438579, 24.1927517483065); Pt(77.8883750604012, 36.5882941689776); Pt(58.4128712276669, 36.5177307492938) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 64 VarDistParallelBehavior.Skip: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-65-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(58.4128712276669, 36.5177307492938); Pt(50.1251536272552, 28.2985066827699); Pt(54.0096912475487, 19.3304488528425); Pt(62.4389641071256, 17.1122616189858); Pt(71.0929596834767, 20.1852967653479); Pt(82.378381438579, 24.1927517483065); Pt(77.8883750604012, 36.5882941689776); Pt(58.4128712276669, 36.5177307492938) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 65 VarDistParallelBehavior.Proportional: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-66-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(58.4128712276669, 36.5177307492938); Pt(50.1251536272552, 28.2985066827699); Pt(54.0096912475487, 19.3304488528425); Pt(62.4389641071256, 17.1122616189858); Pt(70.8352777241175, 19.8941727571458); Pt(70.6937474454134, 20.3213368710528); Pt(82.378381438579, 24.1927517483065); Pt(77.8883750604012, 36.5882941689776); Pt(58.4128712276669, 36.5177307492938) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 66 VarDistParallelBehavior.StepWithTwoPoints: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(57.7735758041577, 71.8535626568735); Pt(49.1307186613005, 63.282134085445); Pt(55.1851060883159, 48.8941788146829); Pt(61.403586501743, 50.8209069929926); Pt(70.7094720422513, 53.9041823226791); Pt(82.8450043755862, 57.9249912283021); Pt(77.7734445305079, 71.9260259493603); Pt(57.7735758041577, 71.8535626568735) |]))
        test "VarDistParallelBehavior.Skip-67-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(57.2776298535724, 73.0517736277262); Pt(48.1696598937509, 64.0190761469115); Pt(55.0768948881684, 47.6043703914008); Pt(83.8236162390229, 56.9850789623759); Pt(77.9766517714106, 73.1267700839502); Pt(57.2776298535724, 73.0517736277262) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 67 VarDistParallelBehavior.Skip: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-68-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(57.2776298535724, 73.0517736277262); Pt(48.1696598937509, 64.0190761469115); Pt(55.0768948881684, 47.6043703914008); Pt(61.5079548352795, 49.7029714259333); Pt(71.1932747150489, 52.8635114436939); Pt(83.8236162390229, 56.9850789623759); Pt(77.9766517714106, 73.1267700839502); Pt(57.2776298535724, 73.0517736277262) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 68 VarDistParallelBehavior.Proportional: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-69-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(57.2776298535724, 73.0517736277262); Pt(48.1696598937509, 64.0190761469115); Pt(55.0768948881684, 47.6043703914008); Pt(61.7587367298801, 49.6746660429938); Pt(61.5922935400151, 50.2513548411166); Pt(70.8981790805235, 53.3346301708031); Pt(71.0868861187956, 52.7650780189272); Pt(83.8236162390229, 56.9850789623759); Pt(77.9766517714106, 73.1267700839502); Pt(57.2776298535724, 73.0517736277262) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 69 VarDistParallelBehavior.StepWithTwoPoints: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Skip-70-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(58.1455352670966, 70.9549044287341); Pt(49.8515127369627, 62.7294275393451); Pt(55.2662644884265, 49.8615351321445); Pt(82.1110454780087, 58.6299254277467); Pt(77.6210390998309, 71.0254678484179); Pt(58.1455352670966, 70.9549044287341) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 70 VarDistParallelBehavior.Skip: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-71-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(58.1455352670966, 70.9549044287341); Pt(49.8515127369627, 62.7294275393451); Pt(55.2662644884265, 49.8615351321445); Pt(61.2718326958054, 51.8231516567716); Pt(70.3163525147143, 54.7773899438814); Pt(82.1110454780087, 58.6299254277467); Pt(77.6210390998309, 71.0254678484179); Pt(58.1455352670966, 70.9549044287341) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 71 VarDistParallelBehavior.Proportional: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-72-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(58.1455352670966, 70.9549044287341); Pt(49.8515127369627, 62.7294275393451); Pt(55.2662644884265, 49.8615351321445); Pt(61.1372238306401, 51.6805877054917); Pt(61.2620562230389, 51.2480711068996); Pt(70.5679417635472, 54.3313464365861); Pt(70.4264114848431, 54.7585105504931); Pt(82.1110454780087, 58.6299254277467); Pt(77.6210390998309, 71.0254678484179); Pt(58.1455352670966, 70.9549044287341) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 72 VarDistParallelBehavior.StepWithTwoPoints: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(55.1425455331334, -88.9061900806692); Pt(50.7713033584764, -88.9061900806692); Pt(45.9317138079633, -88.9061900806692); Pt(40.4676610896421, -88.9061900806692); Pt(34.0669136196086, -112.167443081522); Pt(71.3785878961451, -112.167443081522); Pt(71.3785878961451, -88.9061900806692); Pt(63.7289140904954, -88.9061900806692); Pt(55.1425455331334, -88.9061900806692) |]))
        test "VarDistParallelBehavior.Skip-73-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(40.1755616753672, -87.7061900806692); Pt(33.1144115207699, -113.367443081522); Pt(71.9785878961451, -113.367443081522); Pt(71.9785878961451, -87.7061900806692); Pt(40.1755616753672, -87.7061900806692) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6 |])  pli.Points |> Polyline2D
            "offset 73 VarDistParallelBehavior.Skip: dist -1.2 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-74-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(55.2739680630092, -87.7061900806692); Pt(50.7765704156265, -87.7061900806692); Pt(45.7973087345956, -87.7061900806692); Pt(40.1755616753672, -87.7061900806692); Pt(33.1144115207699, -113.367443081522); Pt(71.9785878961451, -113.367443081522); Pt(71.9785878961451, -87.7061900806692); Pt(64.1081420132253, -87.7061900806692); Pt(55.2739680630092, -87.7061900806692) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6 |])  pli.Points |> Polyline2D
            "offset 74 VarDistParallelBehavior.Proportional: dist -1.2 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-75-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(55.1425455331334, -88.3061900806692); Pt(55.1425455331334, -87.7061900806692); Pt(50.7713033584764, -87.7061900806692); Pt(50.7713033584764, -88.3061900806692); Pt(45.9317138079633, -88.3061900806692); Pt(45.9317138079633, -87.7061900806692); Pt(40.1755616753672, -87.7061900806692); Pt(33.1144115207699, -113.367443081522); Pt(71.9785878961451, -113.367443081522); Pt(71.9785878961451, -87.7061900806692); Pt(63.7289140904954, -87.7061900806692); Pt(63.7289140904954, -88.3061900806692); Pt(55.1425455331334, -88.3061900806692) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6 |])  pli.Points |> Polyline2D
            "offset 75 VarDistParallelBehavior.StepWithTwoPoints: dist -1.2 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Skip-76-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(40.6867356503483, -89.8061900806692); Pt(34.7812901937377, -111.267443081522); Pt(70.9285878961451, -111.267443081522); Pt(70.9285878961451, -89.8061900806692); Pt(40.6867356503483, -89.8061900806692) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45 |])  pli.Points |> Polyline2D
            "offset 76 VarDistParallelBehavior.Skip: dist 0.9 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-77-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(55.0439786357266, -89.8061900806692); Pt(50.7673530656139, -89.8061900806692); Pt(46.0325176129892, -89.8061900806692); Pt(40.6867356503483, -89.8061900806692); Pt(34.7812901937377, -111.267443081522); Pt(70.9285878961451, -111.267443081522); Pt(70.9285878961451, -89.8061900806692); Pt(63.4444931484479, -89.8061900806692); Pt(55.0439786357266, -89.8061900806692) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45 |])  pli.Points |> Polyline2D
            "offset 77 VarDistParallelBehavior.Proportional: dist 0.9 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-78-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(55.1425455331334, -89.3561900806692); Pt(55.1425455331334, -89.8061900806692); Pt(50.7713033584764, -89.8061900806692); Pt(50.7713033584764, -89.3561900806692); Pt(45.9317138079633, -89.3561900806692); Pt(45.9317138079633, -89.8061900806692); Pt(40.6867356503483, -89.8061900806692); Pt(34.7812901937377, -111.267443081522); Pt(70.9285878961451, -111.267443081522); Pt(70.9285878961451, -89.8061900806692); Pt(63.7289140904954, -89.8061900806692); Pt(63.7289140904954, -89.3561900806692); Pt(55.1425455331334, -89.3561900806692) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45 |])  pli.Points |> Polyline2D
            "offset 78 VarDistParallelBehavior.StepWithTwoPoints: dist 0.9 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(-58.6285353445744, -151.87972174524); Pt(-97.0310928093188, -141.809289486606); Pt(-89.273487098122, -134.051683775409); Pt(-49.7627002460637, -134.051683775409); Pt(-38.3913030544957, -134.051683775409); Pt(-29.3327324103653, -134.051683775409); Pt(-19.1177484925161, -134.051683775409); Pt(-0.229665021775986, -134.051683775409); Pt(28.9697701396233, -134.051683775409); Pt(11.141732169792, -151.87972174524); Pt(-1.96428493235413, -151.87972174524); Pt(-20.8523684030942, -151.87972174524); Pt(-38.1985675088759, -151.87972174524); Pt(-58.6285353445744, -151.87972174524) |]))
        test "VarDistParallelBehavior.Skip-79-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-58.473811054346, -150.67972174524); Pt(-95.3760090099306, -141.002733824642); Pt(-89.6249589606982, -135.251683775409); Pt(26.6727138647755, -134.651683775409); Pt(10.0446758949443, -151.27972174524); Pt(-58.473811054346, -150.67972174524) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 79 VarDistParallelBehavior.Skip: dist -1.2 on 14 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-80-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-58.473811054346, -150.67972174524); Pt(-95.3760090099306, -141.002733824642); Pt(-89.6249589606982, -135.251683775409); Pt(-50.7642858894315, -135.051194777854); Pt(-39.5799946152621, -134.993493066363); Pt(-30.6704744477034, -134.947527296192); Pt(-20.6235687268393, -134.89569355536); Pt(-2.04627135618499, -134.799850034578); Pt(26.6727138647755, -134.651683775409); Pt(10.0446758949443, -151.27972174524); Pt(-2.82620010657984, -151.16701456292); Pt(-21.3754037558352, -151.004583623693); Pt(-38.410386699029, -150.855412352975); Pt(-58.473811054346, -150.67972174524) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 80 VarDistParallelBehavior.Proportional: dist -1.2 on 14 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-81-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-58.473811054346, -150.67972174524); Pt(-95.3760090099306, -141.002733824642); Pt(-89.6249589606982, -135.251683775409); Pt(-49.7627002460637, -135.251683775409); Pt(-49.7627002460637, -134.651683775409); Pt(-38.3913030544957, -134.651683775409); Pt(-38.3913030544957, -135.251683775409); Pt(-29.3327324103653, -135.251683775409); Pt(-29.3327324103653, -134.651683775409); Pt(-19.1177484925161, -134.651683775409); Pt(-19.1177484925161, -135.251683775409); Pt(-0.229665021775986, -135.251683775409); Pt(-0.229665021775986, -134.651683775409); Pt(26.6727138647755, -134.651683775409); Pt(10.0446758949443, -151.27972174524); Pt(-1.96428493235413, -151.27972174524); Pt(-1.96428493235413, -150.67972174524); Pt(-20.8523684030942, -150.67972174524); Pt(-20.8523684030942, -151.27972174524); Pt(-38.1985675088759, -151.27972174524); Pt(-38.1985675088759, -150.67972174524); Pt(-58.473811054346, -150.67972174524) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 81 VarDistParallelBehavior.StepWithTwoPoints: dist -1.2 on 14 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Skip-82-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-58.7445785622456, -152.77972174524); Pt(-98.27240565886, -142.414206233079); Pt(-89.00988320119, -133.151683775409); Pt(30.692562345759, -133.601683775409); Pt(11.9645243759278, -152.32972174524); Pt(-58.7445785622456, -152.77972174524) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 82 VarDistParallelBehavior.Skip: dist 0.9 on 14 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-83-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-58.7445785622456, -152.77972174524); Pt(-98.27240565886, -142.414206233079); Pt(-89.00988320119, -133.151683775409); Pt(-49.0115110135379, -133.302050523576); Pt(-37.499784383921, -133.345326807194); Pt(-28.3294258823617, -133.379801134822); Pt(-17.9883833167736, -133.418676440446); Pt(1.13278972903076, -133.490559081033); Pt(30.692562345759, -133.601683775409); Pt(11.9645243759278, -152.32972174524); Pt(-1.31784855168486, -152.414252131981); Pt(-20.4600918885385, -152.536075336401); Pt(-38.0397031162611, -152.647953789439); Pt(-58.7445785622456, -152.77972174524) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 83 VarDistParallelBehavior.Proportional: dist 0.9 on 14 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-84-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-58.7445785622456, -152.77972174524); Pt(-98.27240565886, -142.414206233079); Pt(-89.00988320119, -133.151683775409); Pt(-49.7627002460637, -133.151683775409); Pt(-49.7627002460637, -133.601683775409); Pt(-38.3913030544957, -133.601683775409); Pt(-38.3913030544957, -133.151683775409); Pt(-29.3327324103653, -133.151683775409); Pt(-29.3327324103653, -133.601683775409); Pt(-19.1177484925161, -133.601683775409); Pt(-19.1177484925161, -133.151683775409); Pt(-0.229665021775986, -133.151683775409); Pt(-0.229665021775986, -133.601683775409); Pt(30.692562345759, -133.601683775409); Pt(11.9645243759278, -152.32972174524); Pt(-1.96428493235413, -152.32972174524); Pt(-1.96428493235413, -152.77972174524); Pt(-20.8523684030942, -152.77972174524); Pt(-20.8523684030942, -152.32972174524); Pt(-38.1985675088759, -152.32972174524); Pt(-38.1985675088759, -152.77972174524); Pt(-58.7445785622456, -152.77972174524) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 84 VarDistParallelBehavior.StepWithTwoPoints: dist 0.9 on 14 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(-9.71481756112657, 133.18006730673); Pt(0.0450687618714056, 130.411350164357); Pt(9.46835130176811, 140.479460601206); Pt(11.4307531873941, 131.541576146496); Pt(2.70339097759208, 119.752882776395); Pt(-2.68402868566895, 123.204856146808); Pt(-1.96534222581438, 130.114790826999); Pt(-9.71481756112657, 133.18006730673) |]))
        test "VarDistParallelBehavior.Skip-85-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(12.9720054924896, 125.496842080245); Pt(-0.228067677551052, 129.241482914879); Pt(8.62936364256408, 138.705021372965); Pt(10.3124338969038, 131.039370635146); Pt(2.91268394441642, 121.04398129433); Pt(-1.97951101643568, 124.178640887101); Pt(-1.25702742514882, 131.125083741072); Pt(12.9720054924896, 125.496842080245) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 85 VarDistParallelBehavior.Skip: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-86-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(12.9720054924896, 125.496842080245); Pt(-0.228067677551052, 129.241482914879); Pt(8.62936364256408, 138.705021372965); Pt(10.3124338969038, 131.039370635146); Pt(2.91268394441642, 121.04398129433); Pt(-1.97951101643568, 124.178640887101); Pt(-1.25702742514882, 131.125083741072); Pt(12.9720054924896, 125.496842080245) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 86 VarDistParallelBehavior.Proportional: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-87-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(12.9720054924896, 125.496842080245); Pt(-0.228067677551052, 129.241482914879); Pt(8.62936364256408, 138.705021372965); Pt(10.3124338969038, 131.039370635146); Pt(2.91268394441642, 121.04398129433); Pt(-1.97951101643568, 124.178640887101); Pt(-1.25702742514882, 131.125083741072); Pt(12.9720054924896, 125.496842080245) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 87 VarDistParallelBehavior.StepWithTwoPoints: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Skip-88-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-26.7299348513387, 138.942486226594); Pt(0.249921091438265, 131.288750601465); Pt(10.0975920461711, 141.810290022387); Pt(12.2694926552619, 131.918230280009); Pt(2.54642125247382, 118.784558887944); Pt(-3.2124169375939, 122.474517591587); Pt(-2.49657832631355, 129.357071141445); Pt(-26.7299348513387, 138.942486226594) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 88 VarDistParallelBehavior.Skip: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-89-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-26.7299348513387, 138.942486226594); Pt(0.249921091438265, 131.288750601465); Pt(10.0975920461711, 141.810290022387); Pt(12.2694926552619, 131.918230280009); Pt(2.54642125247382, 118.784558887944); Pt(-3.2124169375939, 122.474517591587); Pt(-2.49657832631355, 129.357071141445); Pt(-26.7299348513387, 138.942486226594) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 89 VarDistParallelBehavior.Proportional: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-90-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-26.7299348513387, 138.942486226594); Pt(0.249921091438265, 131.288750601465); Pt(10.0975920461711, 141.810290022387); Pt(12.2694926552619, 131.918230280009); Pt(2.54642125247382, 118.784558887944); Pt(-3.2124169375939, 122.474517591587); Pt(-2.49657832631355, 129.357071141445); Pt(-26.7299348513387, 138.942486226594) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 90 VarDistParallelBehavior.StepWithTwoPoints: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(-9.03912849186402, 97.2392202608982); Pt(0.717299346739111, 93.4410984525243); Pt(10.1440403710307, 104.538613555374); Pt(12.1064422566566, 95.6007291006641); Pt(3.37908004685463, 83.8120357305632); Pt(-2.0083396164064, 87.2640091009753); Pt(-1.28965315655184, 94.1739437811671); Pt(-9.03912849186402, 97.2392202608982) |]))
        test "VarDistParallelBehavior.Skip-91-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-7.4764451223333, 97.9185811774392); Pt(-8.3555369864059, 95.6783829144727); Pt(0.486889577878712, 92.2430720589915); Pt(9.329482729872, 102.652906252986); Pt(10.9881229661663, 95.0985235893134); Pt(3.58837301367897, 85.1031342484979); Pt(-1.30382194717313, 88.2377938412691); Pt(-0.581338355886275, 95.1842366952402); Pt(-7.4764451223333, 97.9185811774392) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 91 VarDistParallelBehavior.Skip: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-92-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-7.4764451223333, 97.9185811774392); Pt(-8.3555369864059, 95.6783829144727); Pt(0.486889577878712, 92.2430720589915); Pt(9.329482729872, 102.652906252986); Pt(10.9881229661663, 95.0985235893134); Pt(3.58837301367897, 85.1031342484979); Pt(-1.30382194717313, 88.2377938412691); Pt(-0.581338355886275, 95.1842366952402); Pt(-7.4764451223333, 97.9185811774392) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 92 VarDistParallelBehavior.Proportional: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-93-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-7.4764451223333, 97.9185811774392); Pt(-8.3555369864059, 95.6783829144727); Pt(0.486889577878712, 92.2430720589915); Pt(9.329482729872, 102.652906252986); Pt(10.9881229661663, 95.0985235893134); Pt(3.58837301367897, 85.1031342484979); Pt(-1.30382194717313, 88.2377938412691); Pt(-0.581338355886275, 95.1842366952402); Pt(-7.4764451223333, 97.9185811774392) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 93 VarDistParallelBehavior.StepWithTwoPoints: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Skip-94-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-10.2111410190121, 96.7296995734925); Pt(-9.55182212095761, 98.4098482707174); Pt(0.890106673384422, 94.339618247674); Pt(10.7549586018996, 105.952894032165); Pt(12.9451817245244, 95.9773832341771); Pt(3.22211032173637, 82.8437118421123); Pt(-2.53672786833136, 86.533670545755); Pt(-1.82088925705101, 93.4162240956123); Pt(-10.2111410190121, 96.7296995734925) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 94 VarDistParallelBehavior.Skip: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-95-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-10.2111410190121, 96.7296995734925); Pt(-9.55182212095761, 98.4098482707174); Pt(0.890106673384422, 94.339618247674); Pt(10.7549586018996, 105.952894032165); Pt(12.9451817245244, 95.9773832341771); Pt(3.22211032173637, 82.8437118421123); Pt(-2.53672786833136, 86.533670545755); Pt(-1.82088925705101, 93.4162240956123); Pt(-10.2111410190121, 96.7296995734925) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 95 VarDistParallelBehavior.Proportional: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-96-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(-10.2111410190121, 96.7296995734925); Pt(-9.55182212095761, 98.4098482707174); Pt(0.890106673384422, 94.339618247674); Pt(10.7549586018996, 105.952894032165); Pt(12.9451817245244, 95.9773832341771); Pt(3.22211032173637, 82.8437118421123); Pt(-2.53672786833136, 86.533670545755); Pt(-1.82088925705101, 93.4162240956123); Pt(-10.2111410190121, 96.7296995734925) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 96 VarDistParallelBehavior.StepWithTwoPoints: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(69.6107035472615, 209.541610417257); Pt(69.0251189595104, 212.208692742297); Pt(59.6018364196137, 202.140582305448); Pt(49.8419500966157, 204.909299447822); Pt(57.5914254319279, 201.84402296809); Pt(56.8727389720734, 194.934088287899); Pt(62.2601586353344, 191.482114917487); Pt(68.8084032481212, 200.327313042283); Pt(69.6107035472615, 209.541610417257) |]))
        test "VarDistParallelBehavior.Skip-97-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(70.3908221054622, 211.584172717828); Pt(69.8641066187144, 213.983131970538); Pt(59.8749728590362, 203.310449554926); Pt(32.9231844347621, 210.956222892683); Pt(56.3681678685837, 201.682645885488); Pt(55.6721596232596, 194.990754802639); Pt(62.9150253825851, 190.349909780455); Pt(69.2919401558909, 198.963680024268); Pt(70.3908221054622, 211.584172717828) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6 |])  pli.Points |> Polyline2D
            "offset 97 VarDistParallelBehavior.Skip: dist -1.2 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-98-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(70.3908221054622, 211.584172717828); Pt(69.8641066187144, 213.983131970538); Pt(59.8749728590362, 203.310449554926); Pt(32.9231844347621, 210.956222892683); Pt(56.3681678685837, 201.682645885488); Pt(55.6721596232596, 194.990754802639); Pt(62.9150253825851, 190.349909780455); Pt(69.2919401558909, 198.963680024268); Pt(70.3908221054622, 211.584172717828) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6 |])  pli.Points |> Polyline2D
            "offset 98 VarDistParallelBehavior.Proportional: dist -1.2 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-99-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(70.3908221054622, 211.584172717828); Pt(69.8641066187144, 213.983131970538); Pt(59.8749728590362, 203.310449554926); Pt(32.9231844347621, 210.956222892683); Pt(56.3681678685837, 201.682645885488); Pt(55.6721596232596, 194.990754802639); Pt(62.9150253825851, 190.349909780455); Pt(69.2919401558909, 198.963680024268); Pt(70.3908221054622, 211.584172717828) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6 |])  pli.Points |> Polyline2D
            "offset 99 VarDistParallelBehavior.StepWithTwoPoints: dist -1.2 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Skip-100-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(69.0256146286109, 208.009688691829); Pt(68.3958782151074, 210.877863321116); Pt(59.3969840900469, 201.263181868339); Pt(62.5310243430059, 200.374106864176); Pt(58.5088686044361, 201.965055780043); Pt(57.7731734836837, 194.891588401843); Pt(61.7690085748963, 192.33126877026); Pt(68.4457505672939, 201.350037805794); Pt(69.0256146286109, 208.009688691829) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45 |])  pli.Points |> Polyline2D
            "offset 100 VarDistParallelBehavior.Skip: dist 0.9 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-101-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(69.0256146286109, 208.009688691829); Pt(68.3958782151074, 210.877863321116); Pt(59.3969840900469, 201.263181868339); Pt(62.5310243430059, 200.374106864176); Pt(58.5088686044361, 201.965055780043); Pt(57.7731734836837, 194.891588401843); Pt(61.7690085748963, 192.33126877026); Pt(68.4457505672939, 201.350037805794); Pt(69.0256146286109, 208.009688691829) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45 |])  pli.Points |> Polyline2D
            "offset 101 VarDistParallelBehavior.Proportional: dist 0.9 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-102-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(69.0256146286109, 208.009688691829); Pt(68.3958782151074, 210.877863321116); Pt(59.3969840900469, 201.263181868339); Pt(62.5310243430059, 200.374106864176); Pt(58.5088686044361, 201.965055780043); Pt(57.7731734836837, 194.891588401843); Pt(61.7690085748963, 192.33126877026); Pt(68.4457505672939, 201.350037805794); Pt(69.0256146286109, 208.009688691829) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45 |])  pli.Points |> Polyline2D
            "offset 102 VarDistParallelBehavior.StepWithTwoPoints: dist 0.9 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(66.3723353284777, 172.349442086377); Pt(60.2740670044814, 165.170330593615); Pt(50.5176391658783, 168.968452401989); Pt(58.2671145011905, 165.903175922258); Pt(57.5484280413359, 158.993241242066); Pt(62.9358477045969, 155.541267871654); Pt(65.171805743779, 158.561541704534); Pt(66.3723353284777, 172.349442086377) |]))
        test "VarDistParallelBehavior.Skip-103-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(67.8942710401902, 175.99468085791); Pt(59.5018537184375, 166.114810631544); Pt(49.8981260368447, 169.853487138207); Pt(49.2423694864851, 168.182416405919); Pt(57.5587997005249, 164.892883008185); Pt(56.8439103721026, 158.019456501773); Pt(62.7265547377726, 154.25016935372); Pt(66.437394543271, 159.262675029656); Pt(67.8942710401902, 175.99468085791) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 103 VarDistParallelBehavior.Skip: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-104-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(67.8942710401902, 175.99468085791); Pt(59.5018537184375, 166.114810631544); Pt(49.8981260368447, 169.853487138207); Pt(49.2423694864851, 168.182416405919); Pt(57.5587997005249, 164.892883008185); Pt(56.8439103721026, 158.019456501773); Pt(62.7265547377726, 154.25016935372); Pt(66.437394543271, 159.262675029656); Pt(67.8942710401902, 175.99468085791) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 104 VarDistParallelBehavior.Proportional: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-105-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(67.8942710401902, 175.99468085791); Pt(59.5018537184375, 166.114810631544); Pt(49.8981260368447, 169.853487138207); Pt(49.2423694864851, 168.182416405919); Pt(57.5587997005249, 164.892883008185); Pt(56.8439103721026, 158.019456501773); Pt(62.7265547377726, 154.25016935372); Pt(66.437394543271, 159.262675029656); Pt(67.8942710401902, 175.99468085791) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 105 VarDistParallelBehavior.StepWithTwoPoints: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Skip-106-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(65.2308835446933, 169.615513007727); Pt(60.8532269690144, 164.461970565169); Pt(50.9822740126535, 168.304676349826); Pt(51.4740914254232, 169.557979399042); Pt(58.7983506016896, 166.660895607813); Pt(58.0768162932609, 159.723579797287); Pt(63.0928174297152, 156.509591760105); Pt(64.22261414416, 158.035691710693); Pt(65.2308835446933, 169.615513007727) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 106 VarDistParallelBehavior.Skip: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-107-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(65.2308835446933, 169.615513007727); Pt(60.8532269690144, 164.461970565169); Pt(50.9822740126535, 168.304676349826); Pt(51.4740914254232, 169.557979399042); Pt(58.7983506016896, 166.660895607813); Pt(58.0768162932609, 159.723579797287); Pt(63.0928174297152, 156.509591760105); Pt(64.22261414416, 158.035691710693); Pt(65.2308835446933, 169.615513007727) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 107 VarDistParallelBehavior.Proportional: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-108-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(65.2308835446933, 169.615513007727); Pt(60.8532269690144, 164.461970565169); Pt(50.9822740126535, 168.304676349826); Pt(51.4740914254232, 169.557979399042); Pt(58.7983506016896, 166.660895607813); Pt(58.0768162932609, 159.723579797287); Pt(63.0928174297152, 156.509591760105); Pt(64.22261414416, 158.035691710693); Pt(65.2308835446933, 169.615513007727) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 108 VarDistParallelBehavior.StepWithTwoPoints: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(6.40124100798643, 166.077952138966); Pt(3.37908004685463, 161.995695893676); Pt(-2.0083396164064, 165.447669264088); Pt(-1.28965315655184, 172.35760394428); Pt(-9.03912849186402, 175.422880424011); Pt(0.717299346739111, 171.624758615637); Pt(7.63926147181746, 179.773553520096); Pt(6.40124100798643, 166.077952138966) |]))
        test "VarDistParallelBehavior.Skip-109-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(5.23642799808393, 166.521335157571); Pt(2.72421329960385, 163.127901030707); Pt(-0.807760267592639, 165.391002749347); Pt(-0.0663955932076335, 172.518981026882); Pt(-7.98303805387318, 175.650379848496); Pt(-8.63879460423282, 173.979309116208); Pt(0.486889577878703, 170.426732222104); Pt(6.19712409008214, 177.149035675301); Pt(5.23642799808393, 166.521335157571) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 109 VarDistParallelBehavior.Skip: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-110-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(5.23642799808393, 166.521335157571); Pt(2.72421329960385, 163.127901030707); Pt(-0.807760267592639, 165.391002749347); Pt(-0.0663955932076335, 172.518981026882); Pt(-7.98303805387318, 175.650379848496); Pt(-8.63879460423282, 173.979309116208); Pt(0.486889577878703, 170.426732222104); Pt(6.19712409008214, 177.149035675301); Pt(5.23642799808393, 166.521335157571) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 110 VarDistParallelBehavior.Proportional: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-111-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(5.23642799808393, 166.521335157571); Pt(2.72421329960385, 163.127901030707); Pt(-0.807760267592639, 165.391002749347); Pt(-0.0663955932076335, 172.518981026882); Pt(-7.98303805387318, 175.650379848496); Pt(-8.63879460423282, 173.979309116208); Pt(0.486889577878703, 170.426732222104); Pt(6.19712409008214, 177.149035675301); Pt(5.23642799808393, 166.521335157571) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2 |])  pli.Points |> Polyline2D
            "offset 111 VarDistParallelBehavior.StepWithTwoPoints: dist -1.2 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Skip-112-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(7.2748507654133, 165.745414875012); Pt(3.8702301072927, 161.146542040902); Pt(-2.90877412801673, 165.490169150143); Pt(-2.20709632905999, 172.236571132327); Pt(-9.83119632035715, 175.252255855647); Pt(-9.33937890758743, 176.505558904863); Pt(0.890106673384412, 172.523278410786); Pt(8.72086450811895, 181.741941903693); Pt(7.2748507654133, 165.745414875012) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 112 VarDistParallelBehavior.Skip: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-113-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(7.2748507654133, 165.745414875012); Pt(3.8702301072927, 161.146542040902); Pt(-2.90877412801673, 165.490169150143); Pt(-2.20709632905999, 172.236571132327); Pt(-9.83119632035715, 175.252255855647); Pt(-9.33937890758743, 176.505558904863); Pt(0.890106673384412, 172.523278410786); Pt(8.72086450811895, 181.741941903693); Pt(7.2748507654133, 165.745414875012) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 113 VarDistParallelBehavior.Proportional: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-114-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(7.2748507654133, 165.745414875012); Pt(3.8702301072927, 161.146542040902); Pt(-2.90877412801673, 165.490169150143); Pt(-2.20709632905999, 172.236571132327); Pt(-9.83119632035715, 175.252255855647); Pt(-9.33937890758743, 176.505558904863); Pt(0.890106673384412, 172.523278410786); Pt(8.72086450811895, 181.741941903693); Pt(7.2748507654133, 165.745414875012) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9 |])  pli.Points |> Polyline2D
            "offset 114 VarDistParallelBehavior.StepWithTwoPoints: dist 0.9 on 8 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        let pli = Polyline2D(ResizeArray<Pt>([| Pt(10.1959887150057, 208.057348647867); Pt(2.70339097759208, 197.936542939508); Pt(-2.68402868566895, 201.38851630992); Pt(-1.96534222581438, 208.298450990112); Pt(-9.71481756112657, 211.363727469843); Pt(0.0450687618714056, 208.595010327469); Pt(9.46835130176811, 218.663120764318); Pt(10.662894739718, 213.222496546246); Pt(10.1959887150057, 208.057348647867) |]))
        test "VarDistParallelBehavior.Skip-115-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(9.71741452854379, 209.427685180457); Pt(2.04852423034132, 199.068748076539); Pt(-1.48344933685518, 201.331849795179); Pt(-0.742084662470179, 208.459828072715); Pt(7.20394810072715, 205.316804024982); Pt(-0.228067677551052, 207.425143077991); Pt(8.62936364256408, 216.888681536077); Pt(9.87784156498968, 211.20240927114); Pt(9.71741452854379, 209.427685180457) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6 |])  pli.Points |> Polyline2D
            "offset 115 VarDistParallelBehavior.Skip: dist -1.2 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-116-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(9.71741452854379, 209.427685180457); Pt(2.04852423034132, 199.068748076539); Pt(-1.48344933685518, 201.331849795179); Pt(-0.742084662470179, 208.459828072715); Pt(7.20394810072715, 205.316804024982); Pt(-0.228067677551052, 207.425143077991); Pt(8.62936364256408, 216.888681536077); Pt(9.87784156498968, 211.20240927114); Pt(9.71741452854379, 209.427685180457) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6 |])  pli.Points |> Polyline2D
            "offset 116 VarDistParallelBehavior.Proportional: dist -1.2 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-117-dist:-1.2" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(9.71741452854379, 209.427685180457); Pt(2.04852423034132, 199.068748076539); Pt(-1.48344933685518, 201.331849795179); Pt(-0.742084662470179, 208.459828072715); Pt(7.20394810072715, 205.316804024982); Pt(-0.228067677551052, 207.425143077991); Pt(8.62936364256408, 216.888681536077); Pt(9.87784156498968, 211.20240927114); Pt(9.71741452854379, 209.427685180457) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| -1.2;-0.6;-1.2;-0.6;-1.2;-0.6;-1.2;-0.6 |])  pli.Points |> Polyline2D
            "offset 117 VarDistParallelBehavior.StepWithTwoPoints: dist -1.2 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Skip-118-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(10.5549193548522, 207.029596248424); Pt(3.19454103803015, 197.087389086734); Pt(-3.58446319727928, 201.431016195975); Pt(-2.88278539832253, 208.17741817816); Pt(-22.4038918075168, 215.898920053489); Pt(0.249921091438265, 209.472410764578); Pt(10.0975920461711, 219.993950185499); Pt(11.2516846207642, 214.737562002576); Pt(10.5549193548522, 207.029596248424) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Skip (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45 |])  pli.Points |> Polyline2D
            "offset 118 VarDistParallelBehavior.Skip: dist 0.9 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.Proportional-119-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(10.5549193548522, 207.029596248424); Pt(3.19454103803015, 197.087389086734); Pt(-3.58446319727928, 201.431016195975); Pt(-2.88278539832253, 208.17741817816); Pt(-22.4038918075168, 215.898920053489); Pt(0.249921091438265, 209.472410764578); Pt(10.0975920461711, 219.993950185499); Pt(11.2516846207642, 214.737562002576); Pt(10.5549193548522, 207.029596248424) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.Proportional (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45 |])  pli.Points |> Polyline2D
            "offset 119 VarDistParallelBehavior.Proportional: dist 0.9 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }

        test "VarDistParallelBehavior.StepWithTwoPoints-120-dist:0.9" {
            let plExp = Polyline2D(ResizeArray<Pt>([| Pt(10.5549193548522, 207.029596248424); Pt(3.19454103803015, 197.087389086734); Pt(-3.58446319727928, 201.431016195975); Pt(-2.88278539832253, 208.17741817816); Pt(-22.4038918075168, 215.898920053489); Pt(0.249921091438265, 209.472410764578); Pt(10.0975920461711, 219.993950185499); Pt(11.2516846207642, 214.737562002576); Pt(10.5549193548522, 207.029596248424) |]))
            let plr = Offset2D.offsetVariable' Offset2D.UTurn.Chamfer Offset2D.VarDistParallel.StepWithTwoPoints (ResizeArray[| 0.9;0.45;0.9;0.45;0.9;0.45;0.9;0.45 |])  pli.Points |> Polyline2D
            "offset 120 VarDistParallelBehavior.StepWithTwoPoints: dist 0.9 on 9 points" |> Expect.isTrue (Polyline2D.equals 0.00001 plr plExp)
            }




    ]