module TestPolyline

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

// let inline eq a b =
//     let r = Pt.distance a b < 1e-9
//     // if r then
//     //     printfn "Points are equal: %A and %A" a b
//     // else
//     //     printfn "Points are NOT equal: %A and %A" a b
//     r


let inline expectEqPts (a:Pt) (b:Pt) : string -> unit=
      let d = Pt.distance a b
      let r = d < 1e-9
    #if FABLE_COMPILER
      Expect.equal r true
    #else
      Expect.equalWithDiffPrinter
          (fun _ _ -> sprintf "Points are NOT equal by %f \n  given:    %A\n  expected: %A" d a b)
          r true
    #endif

let plClosed = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.); Pt(0.,10.); Pt(0.,0.)]
let plOpen = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.); Pt(0.,10.)]

// Additional test polylines
let plTriangle = Polyline2D.create [Pt(0.,0.); Pt(5.,0.); Pt(2.5,4.33); Pt(0.,0.)]
let plLine = Polyline2D.create [Pt(0.,0.); Pt(10.,0.)]
let plLShape = Polyline2D.create [Pt(0.,0.); Pt(5.,0.); Pt(5.,3.); Pt(2.,3.); Pt(2.,5.); Pt(0.,5.)]
let plSinglePoint = Polyline2D.create [Pt(1.,1.)]
let plEmpty = Polyline2D()


let tests =
  testList "Polyline2D" [

    testList "Basic Properties" [

      test "point count" {
        Expect.equal plClosed.PointCount 5 "closed polyline has 5 points"
        Expect.equal plOpen.PointCount 4 "open polyline has 4 points"
        Expect.equal plSinglePoint.PointCount 1 "single point polyline has 1 point"
        Expect.equal plEmpty.PointCount 0 "empty polyline has 0 points"
      }

      test "segment count" {
        Expect.equal plClosed.SegmentCount 4 "closed polyline has 4 segments"
        Expect.equal plOpen.SegmentCount 3 "open polyline has 3 segments"
        Expect.equal plLine.SegmentCount 1 "line has 1 segment"
        Expect.equal plEmpty.SegmentCount 0 "empty polyline has 0 segments"
      }

      test "start and end points" {
        let p = plOpen
        "start point" |> (expectEqPts  p.Start (Pt(0.,0.)))
        "end point" |> (expectEqPts  p.End (Pt(0.,10.)))
        "first point same as start" |> (expectEqPts  p.FirstPoint p.Start)
        "last point same as end" |> (expectEqPts  p.LastPoint p.End)
      }

      test "is closed" {
        Expect.isTrue plClosed.IsClosed "closed polyline should be closed"
        Expect.isFalse plOpen.IsClosed "open polyline should not be closed"
        Expect.isTrue plTriangle.IsClosed "triangle should be closed"
      }

      test "bounding rectangle" {
        let bbox = plOpen.BoundingRectangle
        "min x" |> Expect.isTrue (abs(bbox.MinX - 0.0) < 1e-9)
        "max x" |> Expect.isTrue (abs(bbox.MaxX - 10.0) < 1e-9)
        "min y" |> Expect.isTrue (abs(bbox.MinY - 0.0) < 1e-9)
        "max y" |> Expect.isTrue (abs(bbox.MaxY - 10.0) < 1e-9)
      }

    ]

    testList "Geometric Operations" [

      test "length calculation" {
        let length = plOpen.Length
        let expected = 30.0 // 10 + 10 + 10
        "length" |> Expect.isTrue (abs(length - expected) < 1e-9)
      }

      test "area calculation" {
        let area = plClosed.Area
        let expected = 100.0 // 10x10 square
        "area" |> Expect.isTrue (abs(area - expected) < 1e-9)
      }

      test "signed area" {
        let signedArea = plClosed.SignedArea
        let expected = 100.0 // counter-clockwise square
        "signed area" |> Expect.isTrue (abs(signedArea - expected) < 1e-9)
      }

      test "is counter clockwise" {
        Expect.isTrue plClosed.IsCounterClockwise "square should be counter-clockwise"
        Expect.isFalse plClosed.IsClockwise "square should not be clockwise"
      }

      test "center point" {
        let center = plOpen.Center
        let expected = Pt(5.0, 5.0)
        "center" |> (expectEqPts center expected)
      }

    ]

    testList "Point Access and Modification" [

      test "get segments" {
        let seg0 = plOpen.GetSegment(0)
        "first segment start" |> (expectEqPts  seg0.From (Pt(0.,0.)))
        "first segment end" |> (expectEqPts  seg0.To (Pt(10.,0.)))

        let lastSeg = plOpen.LastSegment
        "last segment start" |> (expectEqPts  lastSeg.From (Pt(10.,10.)))
        "last segment end" |> (expectEqPts  lastSeg.To (Pt(0.,10.)))

        let firstSeg = plOpen.FirstSegment
        "first segment matches GetSegment(0).From" |> (expectEqPts  firstSeg.From seg0.From )
        "first segment matches GetSegment(0).To" |> (expectEqPts firstSeg.To seg0.To)
      }

      test "evaluate at parameter" {
        let pt0 = plOpen.EvaluateAt(0.0)
        "at parameter 0" |> (expectEqPts  pt0 (Pt(0.,0.)))

        let pt1 = plOpen.EvaluateAt(1.0)
        "at parameter 1" |> (expectEqPts  pt1 (Pt(10.,0.)))

        let ptMid = plOpen.EvaluateAt(0.5)
        "at parameter 0.5" |> (expectEqPts  ptMid (Pt(5.,0.)))
      }

      test "tangent at parameter" {
        let tan0 = plOpen.TangentAt(0.0)
        let expectedTan = UnitVc.create(Pt(0.,0.), Pt(10.,0.))
        "tangent at start" |> Expect.isTrue (abs(tan0.X - expectedTan.X) < 1e-9 && abs(tan0.Y - expectedTan.Y) < 1e-9)
      }

      test "closest point and parameter" {
        let testPt = Pt(5., -1.)
        let closestPt = plOpen.ClosestPoint(testPt)
        let expectedClosest = Pt(5., 0.)
        "closest point" |> (expectEqPts  closestPt expectedClosest)

        let param = plOpen.ClosestParameter(testPt)
        "closest parameter" |> Expect.isTrue (abs(param - 0.5) < 1e-9)

        let distance = plOpen.DistanceTo(testPt)
        "distance" |> Expect.isTrue (abs(distance - 1.0) < 1e-9)
      }

    ]

    testList "Transformations" [

      test "scaling" {
        let scaled = plOpen.Scale(2.0)
        "first point scaled" |> (expectEqPts  scaled.Points.[0] (Pt(0.,0.)))
        "second point scaled" |> (expectEqPts  scaled.Points.[1] (Pt(20.,0.)))
        "third point scaled" |> (expectEqPts  scaled.Points.[2] (Pt(20.,20.)))
      }

      test "scale on center" {
        let center = Pt(5., 5.)
        let scaled = plOpen.ScaleOn center 2.0
        "first point scaled on center" |> (expectEqPts  scaled.Points.[0] (Pt(-5.,-5.)))
        "second point scaled on center" |> (expectEqPts  scaled.Points.[1] (Pt(15.,-5.)))
      }

      test "translation" {
        let vec = Vc(5., 3.)
        let moved = Polyline2D.translate vec plOpen
        "first point translated" |> (expectEqPts  moved.Points.[0] (Pt(5.,3.)))
        "second point translated" |> (expectEqPts  moved.Points.[1] (Pt(15.,3.)))
      }

      test "move X and Y" {
        let movedX = Polyline2D.moveX 5. plOpen
        "moved X" |> (expectEqPts  movedX.Points.[0] (Pt(5.,0.)))

        let movedY = Polyline2D.moveY 3. plOpen
        "moved Y" |> (expectEqPts  movedY.Points.[0] (Pt(0.,3.)))
      }

      test "reverse" {
        let reversed = plOpen.Reverse()
        "first point after reverse" |> (expectEqPts  reversed.Points.[0] (Pt(0.,10.)))
        "last point after reverse" |> (expectEqPts  reversed.Points.[3] (Pt(0.,0.)))
        "original unchanged" |> (expectEqPts  plOpen.Points.[0] (Pt(0.,0.)))
      }

      test "reverse in place" {
        let copy = plOpen.Clone()
        copy.ReverseInPlace()
        "first point after reverse in place" |> (expectEqPts  copy.Points.[0] (Pt(0.,10.)))
        "last point after reverse in place" |> (expectEqPts  copy.Points.[3] (Pt(0.,0.)))
      }

    ]

    testList "Point Containment" [

      test "winding number and contains" {
        let insidePoint = Pt(5., 5.)
        let outsidePoint = Pt(15., 5.)

        let windingInside = plClosed.WindingNumber(insidePoint)
        let windingOutside = plClosed.WindingNumber(outsidePoint)

        Expect.isTrue (windingInside <> 0) "inside point should have non-zero winding"
        Expect.isTrue (windingOutside = 0) "outside point should have zero winding"

        Expect.isTrue (plClosed.Contains(insidePoint)) "should contain inside point"
        Expect.isFalse (plClosed.Contains(outsidePoint)) "should not contain outside point"
      }

    ]

    testList "Creation and Manipulation" [

      test "clone and duplicate" {
        let cloned = plOpen.Clone()
        let duplicated = plOpen.Duplicate()

        "cloned point count" |> Expect.equal cloned.PointCount plOpen.PointCount
        "duplicated point count" |> Expect.equal duplicated.PointCount plOpen.PointCount
        "cloned first point" |> (expectEqPts  cloned.Points.[0] plOpen.Points.[0])
        "duplicated first point" |> (expectEqPts  duplicated.Points.[0] plOpen.Points.[0])
      }

      test "close if open" {
        let copy = plOpen.Clone()
        copy.CloseIfOpen(1e-6)
        "closed polyline point count" |> Expect.equal copy.PointCount (plOpen.PointCount + 1)
        "last point equals first" |> (expectEqPts  copy.Points.[copy.PointCount-1] copy.Points.[0])
      }

      test "sub polyline" {
        let sub = Polyline2D.subPolyline 0.5 2.5 plOpen
        "sub polyline point count" |> Expect.equal sub.PointCount 4
        "sub polyline start" |> (expectEqPts  sub.Points.[0] (Pt(5.,0.)))
        "sub polyline end" |> (expectEqPts  sub.Points.[3] (Pt(5.,10.)))
      }

    ]

    testList "Error Handling" [

      test "empty polyline errors" {
        Expect.throws (fun () -> plEmpty.Start |> ignore) "start on empty should throw"
        Expect.throws (fun () -> plEmpty.End |> ignore) "end on empty should throw"
        "length on empty =0" |> Expect.equal plEmpty.Length 0.0
        Expect.throws (fun () -> plEmpty.FirstSegment |> ignore) "first segment on empty should throw"
      }

      test "single point polyline errors" {
        Expect.throws (fun () -> plSinglePoint.FirstSegment |> ignore) "first segment on single point should throw"
        Expect.throws (fun () -> plSinglePoint.IsClosed |> ignore) "is closed on single point should throw"
      }

      test "invalid parameters" {
        Expect.throws (fun () -> plOpen.EvaluateAt(-1.0) |> ignore) "negative parameter should throw"
        Expect.throws (fun () -> plOpen.EvaluateAt(10.0) |> ignore) "too large parameter should throw"
        Expect.throws (fun () -> plOpen.GetSegment(-1) |> ignore) "negative segment index should throw"
        Expect.throws (fun () -> plOpen.GetSegment(10) |> ignore) "too large segment index should throw"
      }

    ]

    testList "Static Methods" [

      test "create methods" {
        let fromSeq = Polyline2D.create [Pt(0.,0.); Pt(1.,1.)]
        let empty = Polyline2D.createEmpty(10)

        Expect.equal fromSeq.PointCount 2 "created from sequence has correct count"
        Expect.equal empty.PointCount 0 "empty has no points"
      }

      test "map function" {
        let scaled = Polyline2D.map (fun pt -> pt * 2.0) plOpen
        "mapped first point" |> (expectEqPts  scaled.Points.[0] (Pt(0.,0.)))
        "mapped second point" |> (expectEqPts  scaled.Points.[1] (Pt(20.,0.)))
      }

      test "rotation" {
        let rotation = Rotation2D.createFromRadians(System.Math.PI / 2.0) // 90 degrees
        let rotated = Polyline2D.rotate rotation plLine
        "rotated point" |> (expectEqPts  rotated.Points.[1] (Pt(0.,10.)))
      }

    ]

    testList "Offsetting" [

      test "5 points" {
          let o = Polyline2D.offset (plClosed, 2.)
          "pt 0 ok" |> (expectEqPts  o.Points.[0] (Pt(2,2)))
          "pt 1 ok" |> (expectEqPts  o.Points.[1] (Pt(8,2)))
          "pt 2 ok" |> (expectEqPts  o.Points.[2] (Pt(8,8)))
          "pt 3 ok" |> (expectEqPts  o.Points.[3] (Pt(2,8)))
          "pt 4 ok" |> (expectEqPts  o.Points.[4] (Pt(2,2)))
      }

      test "4 points open" {
          let o = Polyline2D.offset (plOpen, 2.)
          "pt 0 ok" |> (expectEqPts  o.Points.[0] (Pt(0,2)))
          "pt 1 ok" |> (expectEqPts  o.Points.[1] (Pt(8,2)))
          "pt 2 ok" |> (expectEqPts  o.Points.[2] (Pt(8,8)))
          "pt 3 ok" |> (expectEqPts  o.Points.[3] (Pt(0,8)))
      }

      test "4 points looped" {
          let o = Polyline2D.offset (plOpen, 2., loop=true)
          "pt 0 ok" |> (expectEqPts  o.Points.[0] (Pt(2,2)))
          "pt 1 ok" |> (expectEqPts  o.Points.[1] (Pt(8,2)))
          "pt 2 ok" |> (expectEqPts  o.Points.[2] (Pt(8,8)))
          "pt 3 ok" |> (expectEqPts  o.Points.[3] (Pt(2,8)))
      }

      test "4 points looped referenceOrient = 1" {
          let o = Polyline2D.offset (plOpen, 2., loop=true, referenceOrient = 1.)
          "pt 0 ok" |> (expectEqPts  o.Points.[0] (Pt(2,2)))
          "pt 1 ok" |> (expectEqPts  o.Points.[1] (Pt(8,2)))
          "pt 2 ok" |> (expectEqPts  o.Points.[2] (Pt(8,8)))
          "pt 3 ok" |> (expectEqPts  o.Points.[3] (Pt(2,8)))
      }

      test "4 points looped referenceOrient = -1" {
          // bad orient, so offset is in the other direction
          let o = Polyline2D.offset (plOpen, -2., loop=true, referenceOrient = -1.)
          "pt 0 ok" |> (expectEqPts  o.Points.[0] (Pt(2,2)))
          "pt 1 ok" |> (expectEqPts  o.Points.[1] (Pt(8,2)))
          "pt 2 ok" |> (expectEqPts  o.Points.[2] (Pt(8,8)))
          "pt 3 ok" |> (expectEqPts  o.Points.[3] (Pt(2,8)))
      }

      test "wrong dist count " {
          Expect.throws (fun () -> Polyline2D.offset (plOpen, [| 0.;0.|]) |> ignore ) " just two distances"
          Expect.throws (fun () -> Polyline2D.offset (plOpen, [| 0.;0.;0.;0.|]) |> ignore ) " four but 3 wanted distances"
      }

      test "4 points looped, 4 params" {
          let o = Polyline2D.offset (plOpen,  [| 4.;2.;2; 2 |] , loop=true)
          "pt 0 ok" |> (expectEqPts  o.Points.[0] (Pt(2,4)))
          "pt 1 ok" |> (expectEqPts  o.Points.[1] (Pt(8,4)))
          "pt 2 ok" |> (expectEqPts  o.Points.[2] (Pt(8,8)))
          "pt 3 ok" |> (expectEqPts  o.Points.[3] (Pt(2,8)))
      }

      test "4 points open, 3 params" {
          let o = Polyline2D.offset (plOpen, [| 4.;2.;2 |] )
          "pt 0 ok" |> (expectEqPts  o.Points.[0] (Pt(0,4)))
          "pt 1 ok" |> (expectEqPts  o.Points.[1] (Pt(8,4)))
          "pt 2 ok" |> (expectEqPts  o.Points.[2] (Pt(8,8)))
          "pt 3 ok" |> (expectEqPts  o.Points.[3] (Pt(0,8)))
      }

      test "5 points, 4 params" {
          let o = Polyline2D.offset (plClosed, [| 4.;2.;2; 2 |], loop=true)
          "pt 0 ok" |> (expectEqPts  o.Points.[0] (Pt(2,4)))
          "pt 1 ok" |> (expectEqPts  o.Points.[1] (Pt(8,4)))
          "pt 2 ok" |> (expectEqPts  o.Points.[2] (Pt(8,8)))
          "pt 3 ok" |> (expectEqPts  o.Points.[3] (Pt(2,8)))
          "pt 4 ok" |> (expectEqPts  o.Points.[4] (Pt(2,4)))
      }

      test "5 points outwards" {
          let o = Polyline2D.offset (plClosed, -2.)
          "pt 0 ok" |> (expectEqPts  o.Points.[0] (Pt(-2,-2)))
          "pt 1 ok" |> (expectEqPts  o.Points.[1] (Pt(12,-2)))
          "pt 2 ok" |> (expectEqPts  o.Points.[2] (Pt(12,12)))
          "pt 3 ok" |> (expectEqPts  o.Points.[3] (Pt(-2,12)))
          "pt 4 ok" |> (expectEqPts  o.Points.[4] (Pt(-2,-2)))
      }

      test "triangle offset inward" {
          let o = Polyline2D.offset (plTriangle, 0.5)
          Expect.isTrue (o.PointCount = 4) "triangle offset should have 4 points"
          Expect.isTrue (o.IsClosed) "triangle offset should be closed"
      }

      test "L-shape offset" {
          let o = Polyline2D.offset (plLShape, 0.5, loop=true)
          Expect.isTrue (o.PointCount = 6) "L-shape offset should have 6 points"
      }

      test "zero offset" {
          let o = Polyline2D.offset (plClosed, 0.0)
          "zero offset preserves shape" |> Expect.isTrue (o.PointCount = plClosed.PointCount)
          "zero offset first point" |> (expectEqPts  o.Points.[0] plClosed.Points.[0])
      }

      test "small offset validation" {
          let o = Polyline2D.offset (plClosed, 0.1)
          Expect.isTrue (o.PointCount = plClosed.PointCount) "small offset maintains point count"
          Expect.isTrue (o.IsClosed) "small offset maintains closure"
      }

      test "large offset validation" {
          let o = Polyline2D.offset (plClosed, 4.5) // almost half the size
          Expect.isTrue (o.PointCount = plClosed.PointCount) "large offset maintains point count"
          Expect.isTrue (o.IsClosed) "large offset maintains closure"
      }

      test "colinear segments offset" {
          // Create a polyline with colinear segments (zigzag on same line)
          let plColinear = Polyline2D.create [Pt(0.,0.); Pt(5.,0.); Pt(10.,0.); Pt(15.,0.); Pt(20.,0.)]
          let o = Polyline2D.offset (plColinear, 2.)
          "colinear offset point count" |> Expect.equal o.PointCount plColinear.PointCount
          "colinear offset pt 0" |> (expectEqPts o.Points.[0] (Pt(0.,2.)))
          "colinear offset pt 1" |> (expectEqPts o.Points.[1] (Pt(5.,2.)))
          "colinear offset pt 2" |> (expectEqPts o.Points.[2] (Pt(10.,2.)))
          "colinear offset pt 3" |> (expectEqPts o.Points.[3] (Pt(15.,2.)))
          "colinear offset pt 4" |> (expectEqPts o.Points.[4] (Pt(20.,2.)))
      }


      test "colinear segments with different offset distances" {
          // Test colinear segments with different offset distances per segment
          let plColinear = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(20.,0.); Pt(30.,0.)]
          let distances = [| 1.; 2.; 3. |] // Different distance for each segment
          let o = Polyline2D.offset (plColinear, distances)
          "colinear different offsets point count" |> Expect.equal o.PointCount plColinear.PointCount
          "colinear different offsets pt 0" |> (expectEqPts o.Points.[0] (Pt(0., 1.)))
          "colinear different offsets pt 1" |> (expectEqPts o.Points.[1] (Pt(10.,2.)))
          "colinear different offsets pt 2" |> (expectEqPts o.Points.[2] (Pt(20.,3.)))
          "colinear different offsets pt 3" |> (expectEqPts o.Points.[3] (Pt(30.,3.)))
      }




      test "L-shape with colinear extension" {
          // Create an L-shape with colinear segments extending the arms
          let plLExtended = Polyline2D.create [
              Pt(0.,0.); Pt(5.,0.); Pt(10.,0.);  // Horizontal colinear segments
              Pt(10.,5.); Pt(10.,10.)            // Vertical segments
          ]
          let o = Polyline2D.offset (plLExtended, 1.)
          "L-extended offset point count" |> Expect.equal o.PointCount plLExtended.PointCount
          "L-extended offset maintains structure" |> Expect.isTrue (o.PointCount = 5)
      }


      test "rect with colinear segments offset" {

        let plOpen = Polyline2D.create [
            Pt(0.,0.)
            Pt(5.,0.)
            Pt(10.,0.)
            Pt(10.,10.)
            Pt(0.,10.)
            Pt(0.,0.)
            ]
        let o = Polyline2D.offset (plOpen, 2.)
        "point count" |> Expect.equal o.PointCount plOpen.PointCount
        "colinear different offsets pt 0" |> expectEqPts o.Points.[0]  (Pt(2.,2.))
        "colinear different offsets pt 1" |> expectEqPts o.Points.[1]  (Pt(5.,2.))
        "colinear different offsets pt 2" |> expectEqPts o.Points.[2]  (Pt(8.,2.))
        "colinear different offsets pt 3" |> expectEqPts o.Points.[3]  (Pt(8.,8.))
        "colinear different offsets pt 4" |> expectEqPts o.Points.[4]  (Pt(2.,8.))
        "colinear different offsets pt 5" |> expectEqPts o.Points.[5]  (Pt(2.,2.))
      }



    ]

  ]


// Test polylines with duplicate points
let plWithDuplicates = Polyline2D.create [Pt(0.,0.); Pt(0.,0.); Pt(10.,0.); Pt(10.,0.); Pt(10.,10.); Pt(0.,10.); Pt(0.,10.); Pt(0.,0.)]
let plConsecutiveDuplicates = Polyline2D.create [Pt(0.,0.); Pt(5.,5.); Pt(5.,5.); Pt(5.,5.); Pt(10.,10.)]
let plAllDuplicates = Polyline2D.create [Pt(1.,1.); Pt(1.,1.); Pt(1.,1.); Pt(1.,1.)]
let plStartEndDuplicate = Polyline2D.create [Pt(0.,0.); Pt(5.,0.); Pt(5.,5.); Pt(0.,5.); Pt(0.,0.); Pt(0.,0.)]

let testsDup =
  testList "Polyline2D Extended Tests" [

    testList "Duplicate Points Handling" [

      test "polyline with duplicate points - basic properties" {
        Expect.equal plWithDuplicates.PointCount 8 "should preserve all points including duplicates"
        Expect.isTrue plWithDuplicates.IsClosed "should be closed when start/end are same"
        "start point" |> expectEqPts plWithDuplicates.Start (Pt(0.,0.))
        "end point" |> expectEqPts plWithDuplicates.End (Pt(0.,0.))
      }

      test "consecutive duplicate points - segment count" {
        // Segments with zero length should still be counted
        Expect.equal plConsecutiveDuplicates.SegmentCount 4 "should count all segments including zero-length"
        let seg1 = plConsecutiveDuplicates.GetSegment(1)
        let seg2 = plConsecutiveDuplicates.GetSegment(2)
        "zero length segment start" |> expectEqPts seg1.From (Pt(5.,5.))
        "zero length segment end" |> expectEqPts seg1.To (Pt(5.,5.))
        "consecutive zero length segment" |> expectEqPts seg2.From (Pt(5.,5.))
      }

      test "all duplicate points polyline" {
        Expect.equal plAllDuplicates.PointCount 4 "should preserve all duplicate points"
        Expect.equal plAllDuplicates.SegmentCount 3 "should have segments even if zero length"
        "length should be zero" |> Expect.isTrue (abs(plAllDuplicates.Length) < 1e-9)
        "all points same as start" |> expectEqPts plAllDuplicates.Points.[2] plAllDuplicates.Start
      }

      test "duplicate points at start and end" {
        Expect.isTrue plStartEndDuplicate.IsClosed "should be closed with duplicate start/end"
        Expect.equal plStartEndDuplicate.PointCount 6 "should count all points including duplicates"
        "first and last points equal" |> expectEqPts plStartEndDuplicate.Points.[0] plStartEndDuplicate.Points.[5]
      }

      // test "duplicate points - geometric operations" {
      //   let area = plWithDuplicates.Area
      //   "area calculation with duplicates" |> Expect.isTrue (abs(area - 100.0) < 1e-9)

      //   let center = plWithDuplicates.Center
      //   "center calculation with duplicates" |> expectEqPts center (Pt(5.,5.))
      // }

      // test "duplicate points - offsetting" {
      //   let offset = Polyline2D.offset(plWithDuplicates, 1.0)
      //   "offset with duplicates should work" |> Expect.isTrue (offset.PointCount > 0)
      //   "offset should maintain closure" |> Expect.isTrue offset.IsClosed
      // }

      test "duplicate points - closest point operations" {
        let testPt = Pt(2., 2.)
        let closest = plConsecutiveDuplicates.ClosestPoint(testPt)
        let param = plConsecutiveDuplicates.ClosestParameter(testPt)
        let distance = plConsecutiveDuplicates.DistanceTo(testPt)

        "closest point with duplicates" |> Expect.isTrue (closest.X >= 0. && closest.Y >= 0.)
        "closest parameter with duplicates" |> Expect.isTrue (param >= 0. && param <= float plConsecutiveDuplicates.SegmentCount)
        "distance with duplicates" |> Expect.isTrue (distance >= 0.)
      }

      test "duplicate points - evaluation at parameter" {
        let pt0 = plConsecutiveDuplicates.EvaluateAt(0.0)
        let pt1 = plConsecutiveDuplicates.EvaluateAt(1.0)
        let ptMid = plConsecutiveDuplicates.EvaluateAt(1.5) // Should be on duplicate segment

        "evaluate at start with duplicates" |> expectEqPts pt0 (Pt(0.,0.))
        "evaluate at segment 1 with duplicates" |> expectEqPts pt1 (Pt(5.,5.))
        "evaluate in duplicate segment range" |> expectEqPts ptMid (Pt(5.,5.))
      }

      test "duplicate points - winding number" {
        if plWithDuplicates.IsClosed then
          let insidePoint = Pt(5., 5.)
          let winding = plWithDuplicates.WindingNumber(insidePoint)
          "winding number with duplicates should work" |> Expect.isTrue (winding <> 0)
          "contains with duplicates" |> Expect.isTrue (plWithDuplicates.Contains(insidePoint))
      }

      test "duplicate points - transformations" {
        let scaled = plWithDuplicates.Scale(2.0)
        "scaled polyline preserves duplicate count" |> Expect.equal scaled.PointCount plWithDuplicates.PointCount
        "scaled duplicate points remain duplicates" |> expectEqPts scaled.Points.[0] scaled.Points.[1]

        let translated = Polyline2D.translate (Vc(1., 1.)) plAllDuplicates
        "translated all duplicates remain same" |> expectEqPts translated.Points.[0] translated.Points.[3]
        "translated duplicate points moved correctly" |> expectEqPts translated.Points.[0] (Pt(2., 2.))
      }

      test "duplicate points - sub polyline" {
        let sub = Polyline2D.subPolyline 0.5 2.5 plConsecutiveDuplicates
        "sub polyline with duplicates should work" |> Expect.isTrue (sub.PointCount > 0)

        // Test sub polyline that spans duplicate points
        let subAcrossDuplicates = Polyline2D.subPolyline 1.0 3.0 plConsecutiveDuplicates
        "sub polyline across duplicates" |> Expect.isTrue (subAcrossDuplicates.PointCount > 0)
      }

      test "duplicate points - edge cases" {
        // Test polyline with only two identical points
        let plTwoDuplicates = Polyline2D.create [Pt(1.,1.); Pt(1.,1.)]
        Expect.equal plTwoDuplicates.PointCount 2 "two duplicate points"
        Expect.equal plTwoDuplicates.SegmentCount 1 "one zero-length segment"
        "length is zero" |> Expect.isTrue (abs(plTwoDuplicates.Length) < 1e-9)

        // Test behavior with single point repeated many times
        let plManyDuplicates = Polyline2D.create (List.replicate 10 (Pt(3.,3.)))
        Expect.equal plManyDuplicates.PointCount 10 "many duplicate points preserved"
        "all points are same" |> expectEqPts plManyDuplicates.Points.[0] plManyDuplicates.Points.[9]
      }

      // test "duplicate points - offset with various parameters" {
      //   // Test offset with different distance arrays
      //   let distances = Array.replicate plWithDuplicates.SegmentCount 0.5
      //   let offsetWithDistances = Polyline2D.offset(plWithDuplicates, distances)
      //   "offset with distance array and duplicates" |> Expect.isTrue (offsetWithDistances.PointCount > 0)

      //   // Test offset with loop=true
      //   let offsetLooped = Polyline2D.offset(plConsecutiveDuplicates, 1.0, loop=true)
      //   "offset looped with duplicates" |> Expect.isTrue (offsetLooped.PointCount > 0)
      // }


      test "nearly duplicate points" {
        // Points that are very close but not exactly the same
        let plNearlyDup = Polyline2D.create [
          Pt(0.,0.); Pt(0.,1e-12); Pt(10.,0.); Pt(10.,1e-12); Pt(0.,0.)
        ]
        Expect.equal plNearlyDup.PointCount 5 "nearly duplicate points preserved"
        "should be closed" |> Expect.isTrue plNearlyDup.IsClosed
      }

      test "duplicate points with different operations" {
        // Test reverse with duplicates
        let reversed = plWithDuplicates.Reverse()
        "reversed preserves duplicates" |> Expect.equal reversed.PointCount plWithDuplicates.PointCount

        // Test clone with duplicates
        let cloned = plWithDuplicates.Clone()
        "cloned preserves duplicates" |> Expect.equal cloned.PointCount plWithDuplicates.PointCount
        "cloned duplicate points match" |> expectEqPts cloned.Points.[0] cloned.Points.[1]
      }

      test "bounding rectangle with duplicates" {
        let bbox = plAllDuplicates.BoundingRectangle
        "bbox min X with duplicates" |> Expect.isTrue (abs(bbox.MinX - 1.0) < 1e-9)
        "bbox max X with duplicates" |> Expect.isTrue (abs(bbox.MaxX - 1.0) < 1e-9)
        "bbox min Y with duplicates" |> Expect.isTrue (abs(bbox.MinY - 1.0) < 1e-9)
        "bbox max Y with duplicates" |> Expect.isTrue (abs(bbox.MaxY - 1.0) < 1e-9)
      }

    ]

  ]