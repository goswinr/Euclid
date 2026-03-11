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

let inline expectNotEqPts (a:Pt) (b:Pt) : string -> unit=
      let d = Pt.distance a b
      let r = d < 1e-9
    #if FABLE_COMPILER
      Expect.equal r false
    #else
      Expect.equalWithDiffPrinter
          (fun _ _ -> sprintf "Points are NOT equal by %f \n  given:    %A\n  expected: %A" d a b)
          r false
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




            test "WindingNumber - various cases" {
                let square = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.); Pt(0.,10.); Pt(0.,0.)]

                // Inside point
                let inside = Pt(5., 5.)
                let windingInside = square.WindingNumber(inside)
                "winding number for inside point should be non-zero" |> Expect.isTrue (windingInside <> 0)

                // Outside point
                let outside = Pt(15., 5.)
                let windingOutside = square.WindingNumber(outside)
                "winding number for outside point should be zero" |> Expect.equal windingOutside 0

                // Empty polyline
                let empty = Polyline2D()
                let windingEmpty = empty.WindingNumber(Pt(5., 5.))
                "winding number for empty polyline should be zero" |> Expect.equal windingEmpty 0
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
                copy.CloseInPlace(1e-6)
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
                Expect.isFalse plSinglePoint.IsClosed  "is closed on single point should be false"
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

            test "4 points looped  2 loop" {
                let o = Polyline2D.offset (plOpen, 2., loop=true)
                "pt 0 ok" |> (expectEqPts  o.Points.[0] (Pt(2,2)))
                "pt 1 ok" |> (expectEqPts  o.Points.[1] (Pt(8,2)))
                "pt 2 ok" |> (expectEqPts  o.Points.[2] (Pt(8,8)))
                "pt 3 ok" |> (expectEqPts  o.Points.[3] (Pt(2,8)))
            }

            test "4 points looped -2 loop" {
                // bad orient, so offset is in the other direction
                let o = Polyline2D.offset (plOpen, -2., loop=true)
                "pt 0 ok" |> (expectNotEqPts  o.Points.[0] (Pt(2,2)))
                "pt 1 ok" |> (expectNotEqPts  o.Points.[1] (Pt(8,2)))
                "pt 2 ok" |> (expectNotEqPts  o.Points.[2] (Pt(8,8)))
                "pt 3 ok" |> (expectNotEqPts  o.Points.[3] (Pt(2,8)))
            }

            test "wrong dist count " {
                Expect.throws (fun () -> Polyline2D.offsetVar (plOpen, [| 0.;0.|]) |> ignore ) " just two distances"
                Expect.throws (fun () -> Polyline2D.offsetVar (plOpen, [| 0.; 0.; 0.; 0.|]) |> ignore ) " four but 3 wanted distances"
            }

            test "4 points looped, 4 params" {
                let o = Polyline2D.offsetVar (plOpen,  [| 4.; 2.; 2.; 2. |] , loop=true)
                "pt 0 ok" |> (expectEqPts  o.Points.[0] (Pt(2,4)))
                "pt 1 ok" |> (expectEqPts  o.Points.[1] (Pt(8,4)))
                "pt 2 ok" |> (expectEqPts  o.Points.[2] (Pt(8,8)))
                "pt 3 ok" |> (expectEqPts  o.Points.[3] (Pt(2,8)))
            }

            test "4 points open, 3 params" {
                let o = Polyline2D.offsetVar (plOpen, [| 4.; 2.; 2. |] )
                "pt 0 ok" |> (expectEqPts  o.Points.[0] (Pt(0,4)))
                "pt 1 ok" |> (expectEqPts  o.Points.[1] (Pt(8,4)))
                "pt 2 ok" |> (expectEqPts  o.Points.[2] (Pt(8,8)))
                "pt 3 ok" |> (expectEqPts  o.Points.[3] (Pt(0,8)))
            }

            test "5 points, 4 params" {
                let o = Polyline2D.offsetVar (plClosed, [| 4.; 2.; 2.; 2. |], loop=true)
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


            // test "colinear segments with different offset distances" {
            //     // Test colinear segments with different offset distances per segment
            //     let plColinear = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(20.,0.); Pt(30.,0.)]
            //     let distances = [| 1.; 2.; 3. |] // Different distance for each segment
            //     let o = Polyline2D.offset (plColinear, distances)
            //     "colinear different offsets point count" |> Expect.equal o.PointCount plColinear.PointCount
            //     "colinear different offsets pt 0" |> (expectEqPts o.Points.[0] (Pt(0., 1.)))
            //     "colinear different offsets pt 1" |> (expectEqPts o.Points.[1] (Pt(10.,2.)))
            //     "colinear different offsets pt 2" |> (expectEqPts o.Points.[2] (Pt(20.,3.)))
            //     "colinear different offsets pt 3" |> (expectEqPts o.Points.[3] (Pt(30.,3.)))
            // }




            test "L-shape with colinear extension" {
                // Create an L-shape with colinear segments extending the arms
                let plLExtended = Polyline2D.create [
                    Pt(0.,0.); Pt(5.,0.); Pt(10.,0.);  // Horizontal colinear segments
                    Pt(10.,5.); Pt(10.,10.)            // Vertical segments
                ]
                let o = Polyline2D.offset (plLExtended, 1.)
                "L-extended offset point count" |> Expect.equal o.Points.Count plLExtended.Points.Count
                "L-extended offset maintains structure" |> Expect.isTrue (o.Points.Count = 5)
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
            let plNearlyDup = Polyline2D.create [Pt(0.,0.); Pt(0.,1e-12); Pt(10.,0.); Pt(10.,1e-12); Pt(0.,0.)  ]
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

        // New tests for duplicate removal
        testList "Remove Duplicate / Colinear" [
            test "removeDuplicatePoints open" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(0.,0.); Pt(1.,0.); Pt(1.,0.); Pt(2.,0.)]
                let cleaned = Polyline2D.removeDuplicatePoints 1e-9 pl
                Expect.equal cleaned.PointCount 3 "duplicates removed keeping endpoints"
                Expect.isFalse cleaned.IsClosed "open polyline remains open"
            }
            test "removeDuplicatePoints closed" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,0.); Pt(1.,1.); Pt(0.,1.); Pt(0.,1.); Pt(0.,0.)]
                let cleaned = Polyline2D.removeDuplicatePoints 1e-9 pl
                // last duplicate before closing removed but closure preserved
                Expect.equal cleaned.PointCount 5 "one duplicate removed"
                Expect.isTrue cleaned.IsClosed "should stay closed"
            }
            test "removeColinearAndDuplicatePoints square" {
                let pl = Polyline2D.create [ Pt(2.,0.); Pt(4.,0.); Pt(4.,2.); Pt(4.,4.); Pt(2.,4.); Pt(0.,4.); Pt(0.,2.); Pt(0.,0.); Pt(2.,0.)]
                let simplified = Polyline2D.removeColinearAndDuplicatePoints Cosine.``0.1`` 1e-9  pl
                Expect.equal simplified.PointCount 5 $"no colinear simplification with loose angle tolerance {simplified.AsFSharpCode}"
                Expect.isTrue simplified.IsClosed "remains closed"
            }
            test "removeColinearAndDuplicatePoints open line with repeats" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,0.); Pt(2.,0.); Pt(3.,0.); Pt(3.,0.)]
                let simplified = Polyline2D.removeColinearAndDuplicatePoints Cosine.``0.1`` 1e-9 pl
                Expect.equal simplified.PointCount 2 "duplicate end removed; interior colinear points removed"
                "start" |> (expectEqPts simplified.Points.[0] (Pt(0.,0.)))
                "end" |> (expectEqPts simplified.Points.[simplified.Points.Count - 1] (Pt(3.,0.)))
            }
            test "colinear start/end segments closed polygon" {
                // square with duplicated first edge points and last edge points before closure
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,0.); Pt(2.,0.); Pt(2.,2.); Pt(2.,4.); Pt(1.,4.); Pt(0.,4.); Pt(0.,2.); Pt(0.,0.)]
                let simplified = Polyline2D.removeColinearAndDuplicatePoints Cosine.``0.1`` 1e-9 pl
                // currently interior colinear points retained
                Expect.isTrue simplified.IsClosed "closed retained"
                Expect.equal simplified.PointCount 5 " colinear removal at this tolerance"
            }
            test "almost colinear tiny angle retained" {
                // create slight bend ~0.01 degrees so cosine almost 1
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(20.,0.2); Pt(30.,0.)]
                let simplified = Polyline2D.removeColinearAndDuplicatePoints Cosine.``0.01`` 1e-9 pl
                Expect.equal simplified.PointCount 4 "almost straight kept"
            }
            test "open U-turn 180 collapse prevented (since method only removes colinear not u-turn)" {
                // a U-turn creates 180 change; algorithm compares cosine; using points making a sharp reversal
                let pl = Polyline2D.create [Pt(0.,0.); Pt(5.,0.); Pt(5.,0.); Pt(0.,0.); Pt(-5.,0.)]
                let simplified = Polyline2D.removeColinearAndDuplicatePoints Cosine.``0.1`` 1e-9 pl
                // current implementation removes duplicates and also collapses colinear first half leaving endpoints
                Expect.equal simplified.PointCount 3 "collapsed to endpoints after duplicate removal"
            }


            test "pointin polyline special cases" {

                let failPoly =
                    [|
                    Pt(0.0, 0.0)
                    Pt(1.0, 0.0)
                    Pt(2.0, 0.0)
                    Pt(3.0, 0.0)
                    Pt(4.0, 0.0)
                    Pt(3.0, 1.0)
                    Pt(1.0, 1.0)
                    Pt(0.0, 2.0)
                    Pt(0.0, 3.0)
                    Pt(0.0, 4.0)
                    Pt(0.0, 5.0)
                    Pt(14.0, 0.0)
                    Pt(12.0, 0.0)
                    Pt(10.0, 0.0)
                    Pt(9.0, 0.0)
                    Pt(0.0, -0.741577)
                    Pt(0.0, -3.0)
                    Pt(0.0, -4.0)
                    Pt(0.0, -5.0)
                    Pt(0.0, -6.0)
                    Pt(-1.47302, 0.0)
                    Pt(-3.0, 0.0)
                    Pt(-5.0, -3.0)
                    Pt(-9.10595, -3.0)
                    Pt(-12.0, -3.0)
                    Pt(-13.0, 0.0)
                    Pt(-10.0, 0.0)
                    Pt(-9.0, 0.0)
                    Pt(-7.0, 0.0)
                    Pt(-5.0, 0.0)
                    Pt(0.0, 14.0)
                    Pt(0.0, 13.0)
                    Pt(0.0, 11.0)
                    Pt(0.0, 10.0)
                    Pt(0.0, 9.0)
                    Pt(-2.0, 5.0)
                    Pt(-2.0, 3.0)
                    Pt(-1.71199, 1.28105)
                    Pt(0.0, 0.0)
                    |]
                    //|> Array.map ( fun p ->
                        //let v = (rand.NextDouble() - 0.5) * 1e-3 // add some small numerical jiggle
                        //Pt(p.X+v, p.Y+v)
                        //)
                    |> Polyline2D.create

                let expectedInside =
                    [|
                    Pt(-3.1, 0)
                    Pt(0, -0.1)
                    Pt(0, -0.2)
                    Pt(0, -0.3)
                    Pt(0, -0.4)
                    Pt(0, -0.5)
                    Pt(0, -0.6)
                    Pt(-0.1, 0)
                    Pt(-0.2, 0)
                    Pt(-0.3, 0)
                    Pt(-0.4, 0)
                    Pt(-0.5, 0)
                    Pt(-0.6, 0)
                    Pt(-0.7, 0)
                    Pt(-0.8, 0)
                    Pt(-0.9, 0)
                    Pt(-1, 0)
                    Pt(-1.1, 0)
                    Pt(-1.2, 0)
                    Pt(-1.3, 0)
                    Pt(-1.4, 0)
                    Pt(-3.2, 0)
                    Pt(-3.3, 0)
                    Pt(-3.4, 0)
                    Pt(-3.5, 0)
                    Pt(-3.6, 0)
                    Pt(-3.7, 0)
                    Pt(-3.8, 0)
                    Pt(-3.9, 0)
                    Pt(-4, 0)
                    Pt(-4.1, 0)
                    Pt(-4.2, 0)
                    Pt(-4.3, 0)
                    Pt(-4.4, 0)
                    Pt(-4.5, 0)
                    Pt(-4.6, 0)
                    Pt(-4.7, 0)
                    Pt(-4.8, 0)
                    Pt(-4.9, 0)
                    |]

                let expectedOutside=
                    [|
                    Pt(0, 0.6)
                    Pt(0, 0.5)
                    Pt(0, 0.4)
                    Pt(0, 0.3)
                    Pt(0, 0.2)
                    Pt(0, 0.1)
                    |]


                let allInside = expectedInside |> Array.forall failPoly.Contains
                "all expected inside points should be inside, winding" |> Expect.isTrue allInside


                let allOutside = expectedOutside |> Array.forall failPoly.Contains |> not
                "all expected outside points should be outside, winding" |> Expect.isTrue allOutside


                // let rand = System.Random()
                // let jigPoly =
                //   failPoly.Points
                //   |> Seq.map ( fun p ->
                //       let v = (rand.NextDouble() - 0.5) * 1e-6 // add some small numerical jiggle
                //       Pt(p.X+v, p.Y+v)
                //       )
                //   |> Polyline2D.create


                // let allInside = expectedInside |> Array.forall jigPoly.Contains
                // "jiggled all expected inside points should be inside, winding" |> Expect.isTrue allInside

                // let allOutside = expectedOutside |> Array.forall jigPoly.Contains |> not
                // "jiggled all expected outside points should be outside, winding" |> Expect.isTrue allOutside

                // let allInside =  expectedInside   |> Array.forall jigPoly.Contains'
                // "jiggled all expected inside points should be inside, ray" |> Expect.isTrue allInside

                // let allOutside =  expectedOutside  |> Array.forall jigPoly.Contains'  |> not
                // "jiggled all expected outside points should be outside, ray" |> Expect.isTrue allOutside

            }

            ]

    ]


let testsComprehensive =
    testList "Polyline2D Comprehensive" [

        testList "SetVertex" [
            test "set vertex on open polyline" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,0.); Pt(2.,0.); Pt(3.,0.)]
                pl.SetVertex 1 (Pt(1.,5.))
                "vertex 1 changed" |> expectEqPts pl.Points.[1] (Pt(1.,5.))
                "vertex 0 unchanged" |> expectEqPts pl.Points.[0] (Pt(0.,0.))
            }
            test "set first vertex on closed polyline updates last" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.); Pt(0.,10.); Pt(0.,0.)]
                pl.SetVertex 0 (Pt(1.,1.))
                "first vertex updated" |> expectEqPts pl.Points.[0] (Pt(1.,1.))
                "last vertex updated too" |> expectEqPts pl.Points.[4] (Pt(1.,1.))
            }
            test "set last vertex on closed polyline updates first" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.); Pt(0.,10.); Pt(0.,0.)]
                pl.SetVertex 4 (Pt(2.,2.))
                "last vertex updated" |> expectEqPts pl.Points.[4] (Pt(2.,2.))
                "first vertex updated too" |> expectEqPts pl.Points.[0] (Pt(2.,2.))
            }
            test "set vertex out of range throws" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,0.)]
                Expect.throws (fun () -> pl.SetVertex -1 (Pt(0.,0.))) "negative index"
                Expect.throws (fun () -> pl.SetVertex 2 (Pt(0.,0.))) "too large index"
            }
        ]

        testList "SecondPoint and SecondLastPoint" [
            test "get second point" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,2.); Pt(3.,4.)]
                "second point" |> expectEqPts pl.SecondPoint (Pt(1.,2.))
            }
            test "set second point" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,2.); Pt(3.,4.)]
                pl.SecondPoint <- Pt(9.,9.)
                "second point set" |> expectEqPts pl.Points.[1] (Pt(9.,9.))
            }
            test "get second last point" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,2.); Pt(3.,4.)]
                "second last point" |> expectEqPts pl.SecondLastPoint (Pt(1.,2.))
            }
            test "set second last point" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,2.); Pt(3.,4.)]
                pl.SecondLastPoint <- Pt(7.,7.)
                "second last point set" |> expectEqPts pl.Points.[1] (Pt(7.,7.))
            }
            test "second point fails on single point" {
                Expect.throws (fun () -> plSinglePoint.SecondPoint |> ignore) "get second point on single"
                Expect.throws (fun () -> plSinglePoint.SecondLastPoint |> ignore) "get second last on single"
            }
        ]

        testList "LastPointIndex and LastSegmentIndex" [
            test "last point index" {
                Expect.equal plOpen.LastPointIndex 3 "open polyline last point index"
                Expect.equal plClosed.LastPointIndex 4 "closed polyline last point index"
            }
            test "last segment index" {
                Expect.equal plOpen.LastSegmentIndex 2 "open polyline last segment index"
                Expect.equal plClosed.LastSegmentIndex 3 "closed polyline last segment index"
            }
        ]

        testList "IsAlmostClosed" [
            test "exactly closed is almost closed" {
                Expect.isTrue (plClosed.IsAlmostClosed 1e-6) "exactly closed should be almost closed"
            }
            test "open is not almost closed" {
                Expect.isFalse (plOpen.IsAlmostClosed 1e-6) "open should not be almost closed"
            }
            test "nearly closed within tolerance" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.); Pt(0.,0.0001)]
                Expect.isTrue (pl.IsAlmostClosed 0.001) "nearly closed within tolerance"
                Expect.isFalse (pl.IsAlmostClosed 0.00001) "nearly closed outside tolerance"
            }
            test "too few points" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(0.,0.)]
                Expect.isFalse (pl.IsAlmostClosed 1.0) "two points should not be almost closed"
            }
        ]

        testList "Segments property" [
            test "segments of open polyline" {
                let segs = plOpen.Segments
                Expect.equal segs.Count 3 "3 segments"
                "seg 0 from" |> expectEqPts segs.[0].From (Pt(0.,0.))
                "seg 0 to" |> expectEqPts segs.[0].To (Pt(10.,0.))
                "seg 2 to" |> expectEqPts segs.[2].To (Pt(0.,10.))
            }
            test "segments of empty polyline" {
                let segs = plEmpty.Segments
                Expect.equal segs.Count 0 "empty has no segments"
            }
            test "segments of single point" {
                let segs = plSinglePoint.Segments
                Expect.equal segs.Count 0 "single point has no segments"
            }
        ]

        testList "SegmentVectors property" [
            test "segment vectors of open polyline" {
                let vecs = plOpen.SegmentVectors
                Expect.equal vecs.Count 3 "3 vectors"
                "vec 0" |> Expect.isTrue (abs(vecs.[0].X - 10.0) < 1e-9 && abs(vecs.[0].Y) < 1e-9)
                "vec 1" |> Expect.isTrue (abs(vecs.[1].X) < 1e-9 && abs(vecs.[1].Y - 10.0) < 1e-9)
            }
            test "segment vectors of empty polyline" {
                let vecs = plEmpty.SegmentVectors
                Expect.equal vecs.Count 0 "empty has no vectors"
            }
        ]

        testList "SignedDistanceTo" [
            test "inside point positive distance" {
                let inside = Pt(5., 5.)
                let sd = plClosed.SignedDistanceTo inside
                "inside positive" |> Expect.isTrue (sd > 0.0)
                "inside distance value" |> Expect.isTrue (abs(sd - 5.0) < 1e-9)
            }
            test "outside point negative distance" {
                let outside = Pt(15., 5.)
                let sd = plClosed.SignedDistanceTo outside
                "outside negative" |> Expect.isTrue (sd < 0.0)
                "outside distance value" |> Expect.isTrue (abs(sd + 5.0) < 1e-9)
            }
            test "point on edge" {
                let onEdge = Pt(5., 0.)
                let sd = plClosed.SignedDistanceTo onEdge
                "on edge distance near zero" |> Expect.isTrue (abs(sd) < 1e-6)
            }
        ]

        testList "ToString and AsString" [
            test "toString on empty" {
                let s = plEmpty.ToString()
                Expect.stringContains s "empty" "should say empty"
            }
            test "toString on closed" {
                let s = plClosed.ToString()
                Expect.stringContains s "closed" "should say closed"
            }
            test "toString on open" {
                let s = plOpen.ToString()
                Expect.stringContains s "open" "should say open"
            }
            test "asString on empty" {
                let s = plEmpty.AsString
                Expect.stringContains s "empty" "should say empty"
            }
            test "asString on closed" {
                let s = plClosed.AsString
                Expect.stringContains s "closed" "should say closed"
            }
        ]

        testList "AsFSharpCode" [
            test "produces valid code string" {
                let code = plLine.AsFSharpCode
                Expect.stringContains code "Polyline2D.create" "should contain create"
            }
        ]

        testList "Static close" [
            test "close open polyline" {
                let closed = Polyline2D.close plOpen
                Expect.isTrue closed.IsClosed "should be closed"
                Expect.equal closed.PointCount (plOpen.PointCount + 1) "one point added"
                "first equals last" |> expectEqPts closed.Points.[0] closed.Points.[closed.PointCount - 1]
            }
            test "close already closed polyline" {
                let closed = Polyline2D.close plClosed
                Expect.isTrue closed.IsClosed "should remain closed"
                Expect.equal closed.PointCount plClosed.PointCount "no extra point added"
            }
            test "close nearly closed polyline snaps last" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.); Pt(0.,0.00000001)]
                let closed = Polyline2D.close pl
                Expect.isTrue closed.IsClosed "should be closed"
                "last snapped to first" |> expectEqPts closed.Points.[0] closed.Points.[closed.PointCount - 1]
            }
            test "close fails with less than 2 points" {
                Expect.throws (fun () -> Polyline2D.close plSinglePoint |> ignore) "single point close should fail"
            }
        ]

        testList "Static closeInPlace" [
            test "closeInPlace on open" {
                let pl = plOpen.Clone()
                Polyline2D.closeInPlace pl
                Expect.isTrue pl.IsClosed "should be closed after closeInPlace"
            }
            test "closeInPlace on nearly closed snaps" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.); Pt(0.,0.00000001)]
                Polyline2D.closeInPlace pl
                "last snapped to first" |> expectEqPts pl.Points.[0] pl.Points.[pl.PointCount - 1]
                // Should not add extra point, just snap
                Expect.equal pl.PointCount 4 "no extra point, just snapped"
            }
        ]

        testList "Static equals" [
            test "same polylines are equal" {
                Expect.isTrue (Polyline2D.equals 1e-9 plOpen (plOpen.Clone())) "clones should be equal"
            }
            test "different polylines are not equal" {
                Expect.isFalse (Polyline2D.equals 1e-9 plOpen plClosed) "open vs closed not equal"
            }
            test "almost equal within tolerance" {
                let pl1 = Polyline2D.create [Pt(0.,0.); Pt(1.,0.)]
                let pl2 = Polyline2D.create [Pt(0.,0.); Pt(1.0001,0.)]
                Expect.isTrue (Polyline2D.equals 0.001 pl1 pl2) "within tolerance"
                Expect.isFalse (Polyline2D.equals 0.00001 pl1 pl2) "outside tolerance"
            }
            test "different counts not equal" {
                let pl1 = Polyline2D.create [Pt(0.,0.); Pt(1.,0.)]
                let pl2 = Polyline2D.create [Pt(0.,0.); Pt(1.,0.); Pt(2.,0.)]
                Expect.isFalse (Polyline2D.equals 1e-9 pl1 pl2) "different counts"
            }
        ]

        testList "Static rotateWithCenter" [
            test "rotate 90 degrees around center" {
                let r = Rotation2D.createFromDegrees 90.0
                let cen = Pt(5., 5.)
                let rotated = Polyline2D.rotateWithCenter cen r plLine
                // plLine is (0,0) to (10,0)
                // (0,0) relative to (5,5) is (-5,-5), rotated 90 CCW is (5,-5), absolute (10,0)
                "rotated first point" |> expectEqPts rotated.Points.[0] (Pt(10., 0.))
                // (10,0) relative to (5,5) is (5,-5), rotated 90 CCW is (5,5), absolute (10,10)
                "rotated second point" |> expectEqPts rotated.Points.[1] (Pt(10., 10.))
            }
        ]

        testList "Static wrappers" [
            test "static start" {
                "static start" |> expectEqPts (Polyline2D.start plOpen) plOpen.Start
            }
            test "static ende" {
                "static ende" |> expectEqPts (Polyline2D.ende plOpen) plOpen.End
            }
            test "static length" {
                "static length" |> Expect.isTrue (abs(Polyline2D.length plOpen - plOpen.Length) < 1e-9)
            }
            test "static pointCount" {
                Expect.equal (Polyline2D.pointCount plOpen) plOpen.PointCount "static pointCount"
            }
            test "static segmentCount" {
                Expect.equal (Polyline2D.segmentCount plOpen) plOpen.SegmentCount "static segmentCount"
            }
            test "static reverseInPlace" {
                let copy = plOpen.Clone()
                Polyline2D.reverseInPlace copy
                "first after reverse" |> expectEqPts copy.Points.[0] (Pt(0.,10.))
            }
            test "static reverse" {
                let rev = Polyline2D.reverse plOpen
                "first after reverse" |> expectEqPts rev.Points.[0] (Pt(0.,10.))
                "original unchanged" |> expectEqPts plOpen.Points.[0] (Pt(0.,0.))
            }
            test "static evaluateAt" {
                let pt = Polyline2D.evaluateAt 0.5 plOpen
                "static evaluateAt" |> expectEqPts pt (Pt(5.,0.))
            }
            test "static closestParameter" {
                let p = Polyline2D.closestParameter plOpen (Pt(5.,-1.))
                "static closestParameter" |> Expect.isTrue (abs(p - 0.5) < 1e-9)
            }
            test "static closestPoint" {
                let pt = Polyline2D.closestPoint plOpen (Pt(5.,-1.))
                "static closestPoint" |> expectEqPts pt (Pt(5.,0.))
            }
            test "static closestVertex" {
                let idx = Polyline2D.closestVertex plOpen (Pt(9.,0.5))
                Expect.equal idx 1 "static closestVertex nearest to (10,0)"
            }
            test "static distanceTo" {
                let d = Polyline2D.distanceTo plOpen (Pt(5.,-3.))
                "static distanceTo" |> Expect.isTrue (abs(d - 3.0) < 1e-9)
            }
            test "static scale" {
                let s = Polyline2D.scale 3.0 plLine
                "static scale" |> expectEqPts s.Points.[1] (Pt(30.,0.))
            }
            test "static windingNumber" {
                let wn = Polyline2D.windingNumber (Pt(5.,5.)) plClosed
                Expect.isTrue (wn <> 0) "static windingNumber inside"
            }
            test "static contains" {
                Expect.isTrue (Polyline2D.contains (Pt(5.,5.)) plClosed) "static contains inside"
                Expect.isFalse (Polyline2D.contains (Pt(15.,5.)) plClosed) "static contains outside"
            }
            test "static start fails on empty" {
                Expect.throws (fun () -> Polyline2D.start plEmpty |> ignore) "start on empty"
            }
            test "static ende fails on empty" {
                Expect.throws (fun () -> Polyline2D.ende plEmpty |> ignore) "ende on empty"
            }
        ]

        testList "createDirectlyUnsafe and pointsUnsafeInternal" [
            test "createDirectlyUnsafe shares array" {
                let ra = ResizeArray [Pt(0.,0.); Pt(1.,1.)]
                let pl = Polyline2D.createDirectlyUnsafe ra
                ra.Add(Pt(2.,2.))
                Expect.equal pl.PointCount 3 "shared array reflects changes"
            }
            test "pointsUnsafeInternal returns same reference" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,1.)]
                let pts = Polyline2D.pointsUnsafeInternal pl
                Expect.equal pts.Count 2 "same count"
                pts.Add(Pt(2.,2.))
                Expect.equal pl.PointCount 3 "mutation reflected in polyline"
            }
        ]

        testList "EvaluateAt edge cases" [
            test "evaluate at near-integer snaps to vertex" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.)]
                let pt = pl.EvaluateAt(0.9999999)  // close to 1.0
                "near integer snaps" |> expectEqPts pt (Pt(10.,0.))
            }
            test "evaluate at exactly last parameter" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.)]
                let pt = pl.EvaluateAt(2.0)
                "at last" |> expectEqPts pt (Pt(10.,10.))
            }
            test "evaluate at near zero negative" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.)]
                let pt = pl.EvaluateAt(-0.0000001)  // within tolerance
                "near zero negative" |> expectEqPts pt (Pt(0.,0.))
            }
            test "evaluate at beyond end throws" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.)]
                Expect.throws (fun () -> pl.EvaluateAt(1.01) |> ignore) "beyond end throws"
            }
        ]

        testList "ClosestPoint edge cases" [
            test "closest point on vertex" {
                let cp = plOpen.ClosestPoint(Pt(10.,0.))
                "on vertex" |> expectEqPts cp (Pt(10.,0.))
            }
            test "closest point single point polyline" {
                let cp = plSinglePoint.ClosestPoint(Pt(5.,5.))
                "single point closest" |> expectEqPts cp (Pt(1.,1.))
            }
            test "closest parameter single point polyline" {
                let cp = plSinglePoint.ClosestParameter(Pt(5.,5.))
                "single point param is 0" |> Expect.isTrue (abs(cp) < 1e-9)
            }
            test "closest vertex single point" {
                let idx = plSinglePoint.ClosestVertex(Pt(5.,5.))
                Expect.equal idx 0 "single point vertex is 0"
            }
            test "distance to single point" {
                let d = plSinglePoint.DistanceTo(Pt(4.,1.))
                "distance to single point" |> Expect.isTrue (abs(d - 3.0) < 1e-9)
            }
            test "closest point/parameter/vertex fails on empty" {
                Expect.throws (fun () -> plEmpty.ClosestPoint(Pt(0.,0.)) |> ignore) "closest point empty"
                Expect.throws (fun () -> plEmpty.ClosestParameter(Pt(0.,0.)) |> ignore) "closest parameter empty"
                Expect.throws (fun () -> plEmpty.ClosestVertex(Pt(0.,0.)) |> ignore) "closest vertex empty"
                Expect.throws (fun () -> plEmpty.DistanceTo(Pt(0.,0.)) |> ignore) "distance to empty"
            }
        ]

        testList "Contains edge cases" [
            test "contains with less than 3 points always false" {
                Expect.isFalse (plLine.Contains(Pt(5.,0.))) "line segment contains false"
                Expect.isFalse (plSinglePoint.Contains(Pt(1.,1.))) "single point contains false"
                Expect.isFalse (plEmpty.Contains(Pt(0.,0.))) "empty contains false"
            }
        ]

        testList "Area and orientation edge cases" [
            test "area fails on open polyline" {
                Expect.throws (fun () -> plOpen.Area |> ignore) "area on open throws"
            }
            test "isCounterClockwise fails on zero area" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,0.); Pt(2.,0.); Pt(0.,0.)]
                Expect.throws (fun () -> pl.IsCounterClockwise |> ignore) "zero area ccw throws"
            }
            test "isClockwise fails on zero area" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,0.); Pt(2.,0.); Pt(0.,0.)]
                Expect.throws (fun () -> pl.IsClockwise |> ignore) "zero area cw throws"
            }
            test "clockwise polyline" {
                // Reverse of CCW square
                let pl = Polyline2D.create [Pt(0.,0.); Pt(0.,10.); Pt(10.,10.); Pt(10.,0.); Pt(0.,0.)]
                Expect.isTrue pl.IsClockwise "CW square"
                Expect.isFalse pl.IsCounterClockwise "CW square not CCW"
                "negative signed area" |> Expect.isTrue (pl.SignedArea < 0.0)
            }
        ]

        testList "Length edge cases" [
            test "length of single point" {
                Expect.equal plSinglePoint.Length 0.0 "single point length is 0"
            }
            test "closed polyline length" {
                "closed length" |> Expect.isTrue (abs(plClosed.Length - 40.0) < 1e-9)
            }
        ]

        testList "Center edge cases" [
            test "center of single point" {
                "single point center" |> expectEqPts plSinglePoint.Center (Pt(1.,1.))
            }
            test "center fails on empty" {
                Expect.throws (fun () -> plEmpty.Center |> ignore) "center on empty"
            }
        ]

        testList "subPolyline edge cases" [
            test "sub polyline reversed when a > b" {
                let sub = Polyline2D.subPolyline 2.5 0.5 plOpen
                // reversed direction
                "reversed sub start" |> expectEqPts sub.Points.[0] (Pt(5.,10.))
                "reversed sub end" |> expectEqPts sub.Points.[sub.PointCount - 1] (Pt(5.,0.))
            }
            test "sub polyline at integer params" {
                let sub = Polyline2D.subPolyline 1.0 2.0 plOpen
                "integer start" |> expectEqPts sub.Points.[0] (Pt(10.,0.))
                "integer end" |> expectEqPts sub.Points.[sub.PointCount - 1] (Pt(10.,10.))
            }
        ]

        testList "tryFindSelfIntersection" [
            test "no intersection in simple polyline" {
                let result = Polyline2D.tryFindSelfIntersection plOpen
                Expect.isNone result "simple polyline has no self intersection"
            }
            test "no intersection in L-shape open polyline" {
                // Open polyline with many segments but no crossings
                let result = Polyline2D.tryFindSelfIntersection plLShape
                Expect.isNone result "L-shape open polyline has no self intersection"
            }
            test "self intersection in figure-8" {
                let fig8 = Polyline2D.create [
                    Pt(0.,0.); Pt(10.,10.); Pt(10.,0.); Pt(0.,10.); Pt(0.,0.)
                ]
                let result = Polyline2D.tryFindSelfIntersection fig8
                Expect.isSome result "figure 8 has self intersection"
                match result with
                | Some (pt, _i, _j) ->
                    "intersection near center" |> expectEqPts pt (Pt(5.,5.))
                | None -> ()
            }
            test "touching segments" {
                // Two segments that share an endpoint but overlap via a separate crossing
                let pl = Polyline2D.create [
                    Pt(0.,0.); Pt(5.,5.); Pt(10.,0.); Pt(5.,5.); Pt(0.,10.)
                ]
                let result = Polyline2D.tryFindSelfIntersection pl
                Expect.isSome result "touching segments should find intersection"
            }
        ]

        testList "CloseInPlace member" [
            test "close in place with close ends snaps" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.); Pt(0.,0.0001)]
                pl.CloseInPlace(0.001)
                "last snapped to first" |> expectEqPts pl.Points.[pl.PointCount - 1] (Pt(0.,0.))
                Expect.equal pl.PointCount 4 "no point added, snapped"
            }
            test "close in place with far ends adds point" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.)]
                pl.CloseInPlace(0.001)
                Expect.equal pl.PointCount 4 "point added"
                "added first as last" |> expectEqPts pl.Points.[3] (Pt(0.,0.))
            }
            test "close in place fails with less than 3 points" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,0.)]
                Expect.throws (fun () -> pl.CloseInPlace(0.001)) "less than 3 points"
            }
        ]

        testList "GetSegment edge cases" [
            test "get segment at last valid index" {
                let seg = plOpen.GetSegment(2)
                "last segment from" |> expectEqPts seg.From (Pt(10.,10.))
                "last segment to" |> expectEqPts seg.To (Pt(0.,10.))
            }
            test "last segment matches GetSegment last" {
                let seg1 = plOpen.LastSegment
                let seg2 = plOpen.GetSegment(plOpen.LastSegmentIndex)
                "from matches" |> expectEqPts seg1.From seg2.From
                "to matches" |> expectEqPts seg1.To seg2.To
            }
        ]

        testList "BoundingRectangle edge cases" [
            test "single point bounding rect" {
                let br = plSinglePoint.BoundingRectangle
                "min x" |> Expect.isTrue (abs(br.MinX - 1.0) < 1e-9)
                "max x" |> Expect.isTrue (abs(br.MaxX - 1.0) < 1e-9)
            }
            test "line bounding rect" {
                let br = plLine.BoundingRectangle
                "min x" |> Expect.isTrue (abs(br.MinX - 0.0) < 1e-9)
                "max x" |> Expect.isTrue (abs(br.MaxX - 10.0) < 1e-9)
                "min y" |> Expect.isTrue (abs(br.MinY - 0.0) < 1e-9)
                "max y" |> Expect.isTrue (abs(br.MaxY - 0.0) < 1e-9)
            }
        ]

        testList "Start and End setters" [
            test "set start" {
                let pl = plOpen.Clone()
                pl.Start <- Pt(99.,99.)
                "start set" |> expectEqPts pl.Points.[0] (Pt(99.,99.))
            }
            test "set end" {
                let pl = plOpen.Clone()
                pl.End <- Pt(88.,88.)
                "end set" |> expectEqPts pl.Points.[pl.PointCount - 1] (Pt(88.,88.))
            }
            test "set first point" {
                let pl = plOpen.Clone()
                pl.FirstPoint <- Pt(77.,77.)
                "first point set" |> expectEqPts pl.Points.[0] (Pt(77.,77.))
            }
            test "set last point" {
                let pl = plOpen.Clone()
                pl.LastPoint <- Pt(66.,66.)
                "last point set" |> expectEqPts pl.Points.[pl.PointCount - 1] (Pt(66.,66.))
            }
        ]

        testList "Offset edge cases" [
            test "offset fails with less than 2 points" {
                Expect.throws (fun () -> Polyline2D.offset(plSinglePoint, 1.0) |> ignore) "single point offset"
            }
            test "offsetVar fails with less than 2 points" {
                Expect.throws (fun () -> Polyline2D.offsetVar(plSinglePoint, [|1.0|]) |> ignore) "single point offsetVar"
            }
        ]

        testList "removeColinearAndDuplicatePoints edge cases" [
            test "single point returns same" {
                let result = Polyline2D.removeColinearAndDuplicatePoints Cosine.``0.1`` 1e-9 plSinglePoint
                Expect.equal result.PointCount 1 "single point unchanged"
            }
            test "empty returns same" {
                let result = Polyline2D.removeColinearAndDuplicatePoints Cosine.``0.1`` 1e-9 plEmpty
                Expect.equal result.PointCount 0 "empty unchanged"
            }
            test "tolerance too tight throws" {
                // Cosine.``0.01`` is the maximum allowed; a value above it (closer to 1.0) should throw
                let tooTight = UtilEuclid.withMeasure 0.9999999999 // cosine of ~0.0 degrees, above Cosine.``0.01``
                Expect.throws
                    (fun () -> Polyline2D.removeColinearAndDuplicatePoints tooTight 1e-9 plOpen |> ignore)
                    "tolerance tighter than 0.01 degrees should throw"
            }
        ]

        testList "removeDuplicatePoints edge cases" [
            test "no duplicates returns same count" {
                let result = Polyline2D.removeDuplicatePoints 1e-9 plOpen
                Expect.equal result.PointCount plOpen.PointCount "no duplicates, same count"
            }
            test "all same points" {
                let pl = Polyline2D.create [Pt(1.,1.); Pt(1.,1.); Pt(1.,1.)]
                let result = Polyline2D.removeDuplicatePoints 1e-9 pl
                Expect.equal result.PointCount 1 "all same reduces to 1"
            }
            test "single point" {
                let result = Polyline2D.removeDuplicatePoints 1e-9 plSinglePoint
                Expect.equal result.PointCount 1 "single point unchanged"
            }
        ]

        testList "WindingNumber edge cases" [
            test "point on vertex" {
                // Boundary behavior - just verify it doesn't crash
                let _wn = plClosed.WindingNumber(Pt(0.,0.))
                Expect.isTrue true "doesn't crash on vertex"
            }
            test "point far away" {
                let wn = plClosed.WindingNumber(Pt(1000.,1000.))
                Expect.equal wn 0 "far away point is outside"
            }
            test "single point polyline" {
                let wn = plSinglePoint.WindingNumber(Pt(1.,1.))
                Expect.equal wn 0 "single point winding is 0"
            }
        ]

        testList "move static same as translate" [
            test "move equals translate" {
                let v = Vc(3., 4.)
                let moved = Polyline2D.move v plOpen
                let translated = Polyline2D.translate v plOpen
                for i = 0 to moved.PointCount - 1 do
                    $"point {i}" |> expectEqPts moved.Points.[i] translated.Points.[i]
            }
        ]
    ]