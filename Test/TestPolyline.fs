module TestPolyline

open Euclid


#if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
open Fable.Mocha
#else
open Expecto
#endif


let inline expectEqPts (a:Pt) (b:Pt) : string -> unit=
      let d = Pt.dist a b
      let r = d < 1e-9
    #if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
      Expect.equal r true
    #else
      Expect.equalWithDiffPrinter
          (fun _ _ -> sprintf "Points are NOT equal by %f \n  given:    %A\n  expected: %A" d a b)
          r true
    #endif

let inline expectNotEqPts (a:Pt) (b:Pt) : string -> unit=
      let d = Pt.dist a b
      let r = d < 1e-9
    #if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
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
                "first point scaled" |> (expectEqPts  scaled.AsPoints.[0] (Pt(0.,0.)))
                "second point scaled" |> (expectEqPts  scaled.AsPoints.[1] (Pt(20.,0.)))
                "third point scaled" |> (expectEqPts  scaled.AsPoints.[2] (Pt(20.,20.)))
            }

            test "scale on center" {
                let center = Pt(5., 5.)
                let scaled = plOpen.ScaleOn center 2.0
                "first point scaled on center" |> (expectEqPts  scaled.AsPoints.[0] (Pt(-5.,-5.)))
                "second point scaled on center" |> (expectEqPts  scaled.AsPoints.[1] (Pt(15.,-5.)))
            }

            test "translation" {
                let vec = Vc(5., 3.)
                let moved = Polyline2D.translate vec plOpen
                "first point translated" |> (expectEqPts  moved.AsPoints.[0] (Pt(5.,3.)))
                "second point translated" |> (expectEqPts  moved.AsPoints.[1] (Pt(15.,3.)))
            }

            test "move X and Y" {
                let movedX = Polyline2D.moveX 5. plOpen
                "moved X" |> (expectEqPts  movedX.AsPoints.[0] (Pt(5.,0.)))

                let movedY = Polyline2D.moveY 3. plOpen
                "moved Y" |> (expectEqPts  movedY.AsPoints.[0] (Pt(0.,3.)))
            }

            test "reverse" {
                let reversed = plOpen.Reverse()
                "first point after reverse" |> (expectEqPts  reversed.AsPoints.[0] (Pt(0.,10.)))
                "last point after reverse" |> (expectEqPts  reversed.AsPoints.[3] (Pt(0.,0.)))
                "original unchanged" |> (expectEqPts  plOpen.AsPoints.[0] (Pt(0.,0.)))
            }

            test "reverse in place" {
                let copy = plOpen.Clone()
                copy.ReverseInPlace()
                "first point after reverse in place" |> (expectEqPts  copy.AsPoints.[0] (Pt(0.,10.)))
                "last point after reverse in place" |> (expectEqPts  copy.AsPoints.[3] (Pt(0.,0.)))
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
                "cloned first point" |> (expectEqPts  cloned.AsPoints.[0] plOpen.AsPoints.[0])
                "duplicated first point" |> (expectEqPts  duplicated.AsPoints.[0] plOpen.AsPoints.[0])
            }

            test "close if open" {
                let copy = plOpen.Clone()
                copy.CloseInPlace(1e-6)
                "closed polyline point count" |> Expect.equal copy.PointCount (plOpen.PointCount + 1)
                "last point equals first" |> (expectEqPts  copy.AsPoints.[copy.PointCount-1] copy.AsPoints.[0])
            }

            test "sub polyline" {
                let sub = Polyline2D.subPolyline 0.5 2.5 plOpen
                "sub polyline point count" |> Expect.equal sub.PointCount 4
                "sub polyline start" |> (expectEqPts  sub.AsPoints.[0] (Pt(5.,0.)))
                "sub polyline end" |> (expectEqPts  sub.AsPoints.[3] (Pt(5.,10.)))
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
                let scaled = Polyline2D.mapPt (fun pt -> pt * 2.0) plOpen
                "mapped first point" |> (expectEqPts  scaled.AsPoints.[0] (Pt(0.,0.)))
                "mapped second point" |> (expectEqPts  scaled.AsPoints.[1] (Pt(20.,0.)))
            }

            test "rotation" {
                let rotation = Rotation2D.createFromRadians(System.Math.PI / 2.0) // 90 degrees
                let rotated = Polyline2D.rotate rotation plLine
                "rotated point" |> (expectEqPts  rotated.AsPoints.[1] (Pt(0.,10.)))
            }

            ]

        testList "Offsetting" [

            test "5 points" {
                let o = Polyline2D.offset (plClosed, 2.)
                "pt 0 ok" |> (expectEqPts  o.AsPoints.[0] (Pt(2,2)))
                "pt 1 ok" |> (expectEqPts  o.AsPoints.[1] (Pt(8,2)))
                "pt 2 ok" |> (expectEqPts  o.AsPoints.[2] (Pt(8,8)))
                "pt 3 ok" |> (expectEqPts  o.AsPoints.[3] (Pt(2,8)))
                "pt 4 ok" |> (expectEqPts  o.AsPoints.[4] (Pt(2,2)))
            }

            test "4 points open" {
                let o = Polyline2D.offset (plOpen, 2.)
                "pt 0 ok" |> (expectEqPts  o.AsPoints.[0] (Pt(0,2)))
                "pt 1 ok" |> (expectEqPts  o.AsPoints.[1] (Pt(8,2)))
                "pt 2 ok" |> (expectEqPts  o.AsPoints.[2] (Pt(8,8)))
                "pt 3 ok" |> (expectEqPts  o.AsPoints.[3] (Pt(0,8)))
            }

            test "4 points looped" {
                let o = Polyline2D.offset (plOpen, 2., loop=true)
                "pt 0 ok" |> (expectEqPts  o.AsPoints.[0] (Pt(2,2)))
                "pt 1 ok" |> (expectEqPts  o.AsPoints.[1] (Pt(8,2)))
                "pt 2 ok" |> (expectEqPts  o.AsPoints.[2] (Pt(8,8)))
                "pt 3 ok" |> (expectEqPts  o.AsPoints.[3] (Pt(2,8)))
            }

            test "4 points looped  2 loop" {
                let o = Polyline2D.offset (plOpen, 2., loop=true)
                "pt 0 ok" |> (expectEqPts  o.AsPoints.[0] (Pt(2,2)))
                "pt 1 ok" |> (expectEqPts  o.AsPoints.[1] (Pt(8,2)))
                "pt 2 ok" |> (expectEqPts  o.AsPoints.[2] (Pt(8,8)))
                "pt 3 ok" |> (expectEqPts  o.AsPoints.[3] (Pt(2,8)))
            }

            test "4 points looped -2 loop" {
                // bad orient, so offset is in the other direction
                let o = Polyline2D.offset (plOpen, -2., loop=true)
                "pt 0 ok" |> (expectNotEqPts  o.AsPoints.[0] (Pt(2,2)))
                "pt 1 ok" |> (expectNotEqPts  o.AsPoints.[1] (Pt(8,2)))
                "pt 2 ok" |> (expectNotEqPts  o.AsPoints.[2] (Pt(8,8)))
                "pt 3 ok" |> (expectNotEqPts  o.AsPoints.[3] (Pt(2,8)))
            }

            test "wrong dist count " {
                Expect.throws (fun () -> Polyline2D.offsetVar (plOpen, ResizeArray [|0.;0.|]) |> ignore ) " just two distances"
                Expect.throws (fun () -> Polyline2D.offsetVar (plOpen, ResizeArray [| 0.; 0.; 0.; 0.|]) |> ignore ) " four but 3 wanted distances"
            }

            test "4 points looped, 4 params" {
                let o = Polyline2D.offsetVar (plOpen,  ResizeArray [| 4.; 2.; 2.; 2. |] , loop=true)
                "pt 0 ok" |> (expectEqPts  o.AsPoints.[0] (Pt(2,4)))
                "pt 1 ok" |> (expectEqPts  o.AsPoints.[1] (Pt(8,4)))
                "pt 2 ok" |> (expectEqPts  o.AsPoints.[2] (Pt(8,8)))
                "pt 3 ok" |> (expectEqPts  o.AsPoints.[3] (Pt(2,8)))
            }

            test "4 points open, 3 params" {
                let o = Polyline2D.offsetVar (plOpen, ResizeArray [| 4.; 2.; 2. |] )
                "pt 0 ok" |> (expectEqPts  o.AsPoints.[0] (Pt(0,4)))
                "pt 1 ok" |> (expectEqPts  o.AsPoints.[1] (Pt(8,4)))
                "pt 2 ok" |> (expectEqPts  o.AsPoints.[2] (Pt(8,8)))
                "pt 3 ok" |> (expectEqPts  o.AsPoints.[3] (Pt(0,8)))
            }

            test "5 points, 4 params" {
                let o = Polyline2D.offsetVar (plClosed, ResizeArray [| 4.; 2.; 2.; 2. |], loop=true)
                "pt 0 ok" |> (expectEqPts  o.AsPoints.[0] (Pt(2,4)))
                "pt 1 ok" |> (expectEqPts  o.AsPoints.[1] (Pt(8,4)))
                "pt 2 ok" |> (expectEqPts  o.AsPoints.[2] (Pt(8,8)))
                "pt 3 ok" |> (expectEqPts  o.AsPoints.[3] (Pt(2,8)))
                "pt 4 ok" |> (expectEqPts  o.AsPoints.[4] (Pt(2,4)))
            }

            test "5 points outwards" {
                let o = Polyline2D.offset (plClosed, -2.)
                "pt 0 ok" |> (expectEqPts  o.AsPoints.[0] (Pt(-2,-2)))
                "pt 1 ok" |> (expectEqPts  o.AsPoints.[1] (Pt(12,-2)))
                "pt 2 ok" |> (expectEqPts  o.AsPoints.[2] (Pt(12,12)))
                "pt 3 ok" |> (expectEqPts  o.AsPoints.[3] (Pt(-2,12)))
                "pt 4 ok" |> (expectEqPts  o.AsPoints.[4] (Pt(-2,-2)))
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
                "zero offset first point" |> (expectEqPts  o.AsPoints.[0] plClosed.AsPoints.[0])
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
                "colinear offset pt 0" |> (expectEqPts o.AsPoints.[0] (Pt(0.,2.)))
                "colinear offset pt 1" |> (expectEqPts o.AsPoints.[1] (Pt(5.,2.)))
                "colinear offset pt 2" |> (expectEqPts o.AsPoints.[2] (Pt(10.,2.)))
                "colinear offset pt 3" |> (expectEqPts o.AsPoints.[3] (Pt(15.,2.)))
                "colinear offset pt 4" |> (expectEqPts o.AsPoints.[4] (Pt(20.,2.)))
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
                "L-extended offset point count" |> Expect.equal o.AsPoints.Count plLExtended.AsPoints.Count
                "L-extended offset maintains structure" |> Expect.isTrue (o.AsPoints.Count = 5)
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
                "colinear different offsets pt 0" |> expectEqPts o.AsPoints.[0]  (Pt(2.,2.))
                "colinear different offsets pt 1" |> expectEqPts o.AsPoints.[1]  (Pt(5.,2.))
                "colinear different offsets pt 2" |> expectEqPts o.AsPoints.[2]  (Pt(8.,2.))
                "colinear different offsets pt 3" |> expectEqPts o.AsPoints.[3]  (Pt(8.,8.))
                "colinear different offsets pt 4" |> expectEqPts o.AsPoints.[4]  (Pt(2.,8.))
                "colinear different offsets pt 5" |> expectEqPts o.AsPoints.[5]  (Pt(2.,2.))
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
            "all points same as start" |> expectEqPts plAllDuplicates.AsPoints.[2] plAllDuplicates.Start
        }

        test "duplicate points at start and end" {
            Expect.isTrue plStartEndDuplicate.IsClosed "should be closed with duplicate start/end"
            Expect.equal plStartEndDuplicate.PointCount 6 "should count all points including duplicates"
            "first and last points equal" |> expectEqPts plStartEndDuplicate.AsPoints.[0] plStartEndDuplicate.AsPoints.[5]
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
            "scaled duplicate points remain duplicates" |> expectEqPts scaled.AsPoints.[0] scaled.AsPoints.[1]

            let translated = Polyline2D.translate (Vc(1., 1.)) plAllDuplicates
            "translated all duplicates remain same" |> expectEqPts translated.AsPoints.[0] translated.AsPoints.[3]
            "translated duplicate points moved correctly" |> expectEqPts translated.AsPoints.[0] (Pt(2., 2.))
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
            "all points are same" |> expectEqPts plManyDuplicates.AsPoints.[0] plManyDuplicates.AsPoints.[9]
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
            "cloned duplicate points match" |> expectEqPts cloned.AsPoints.[0] cloned.AsPoints.[1]
        }

        test "bounding rectangle with duplicates" {
            let bbox = plAllDuplicates.BoundingRectangle
            "bbox min X with duplicates" |> Expect.isTrue (abs(bbox.MinX - 1.0) < 1e-9)
            "bbox max X with duplicates" |> Expect.isTrue (abs(bbox.MaxX - 1.0) < 1e-9)
            "bbox min Y with duplicates" |> Expect.isTrue (abs(bbox.MinY - 1.0) < 1e-9)
            "bbox max Y with duplicates" |> Expect.isTrue (abs(bbox.MaxY - 1.0) < 1e-9)
        }

        ]

        testList "Polyline2D remove Uturns" [
            test "removeUturns simple U" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(5.,0.); Pt(5.,5.); Pt(5.,1.); Pt(9.,1.)]
                let simplified = Polyline2D.removeUTurns Cosine.``179.99`` pl
                Expect.equal simplified.PointCount 4 "should remove the U-turn point"

            }

            test "removeUTurnsDeeply " {
                let input = Polyline2D.create [| Pt(367.51025839920425, 2485.8582078421523); Pt(390.6908203303326, 2934.4702593328134); Pt(378.78095441167375, 2674.6796509622122); Pt(383.9582306518063, 2759.6732692377213); Pt(833.2165539365033, 2783.9037269221644); Pt(1022.2972159629621, 2946.319167380789); Pt(1284.101209538059, 2825.113614799726);
                                Pt(955.7761998003084, 2982.681424979096); Pt(983.9156549202881, 3238.860723379728); Pt(790.6306752886735, 3302.522046036172); Pt(1029.1688842784836, 3230.42368061803); Pt(1138.8504401805505, 3195.1415017963864); Pt(1247.7649921951906, 3159.0923190873154); Pt(894.1762000913242, 3274.909906088799); Pt(941.7304411118008, 3259.569828340258);
                                Pt(1060.4455983546752, 3534.242442693297); Pt(1076.8083479531188, 3602.4205660201455); Pt(1360.4293409928073, 3464.700756899912); Pt(1117.4915124820616, 3584.8646992634817)
                                 |]
                let expected = Polyline2D.create [| Pt(367.51025839920425, 2485.8582078421523); Pt(383.9582306518063, 2759.6732692377213); Pt(833.2165539365033, 2783.9037269221644); Pt(1022.2972159629621, 2946.319167380789); Pt(955.7761998003084, 2982.681424979096); Pt(983.9156549202881, 3238.860723379728); Pt(941.7304411118008, 3259.569828340258);
                                Pt(1060.4455983546752, 3534.242442693297); Pt(1076.8083479531188, 3602.4205660201455); Pt(1117.4915124820616, 3584.8646992634817)
                                |]
                let simplified = Polyline2D.removeUTurnsDeeply Cosine.``170.0`` input
                Expect.equal simplified.PointCount expected.PointCount "removeUTurnsDeeply point count should match expected"
                for i = 0 to expected.PointCount - 1 do
                    let expectedPt = expected.AsPoints.[i]
                    let actualPt = simplified.AsPoints.[i]
                    "point {i} should match expected" |> expectEqPts actualPt expectedPt
            }

            test "removeUTurns U-turn at seam keeps closed polyline closed" {
                // closed polyline whose shared start/end vertex is itself a sharp U-turn
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.); Pt(0.,10.); Pt(5.,0.001); Pt(0.,0.)]
                Expect.isTrue pl.IsClosed "input should be closed"
                let simplified = Polyline2D.removeUTurns Cosine.``179.9`` pl
                Expect.isTrue simplified.IsClosed "closed polyline must stay closed after removing seam U-turn"
            }

            test "removeUTurnsDeeply U-turn at seam keeps closed polyline closed" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.); Pt(0.,10.); Pt(5.,0.001); Pt(0.,0.)]
                Expect.isTrue pl.IsClosed "input should be closed"
                let simplified = Polyline2D.removeUTurnsDeeply Cosine.``179.9`` pl
                Expect.isTrue simplified.IsClosed "closed polyline must stay closed after removing seam U-turn"
            }

        ]

        // New tests for duplicate removal
        testList "RemoveDuplicate" [

            test "removeDuplicatePointsFaithfully3" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(0.993,0.); Pt(1.0,0.002); Pt(1.,1.) ]
                let cleaned = Polyline2D.removeDuplicatePointsFaithfully 0.01 pl
                // printfn $"cleaned points: {cleaned.AsFSharpCode}"
                Expect.equal cleaned.PointCount 3 "one point removed as duplicate within tolerance"
                expectEqPts cleaned.AsPoints.[1] (Pt(1.0,0.0))  "edges re-intersected , not moved to average"
                expectEqPts cleaned.Start pl.Start  "start should be preserved"
                expectEqPts cleaned.End pl.End  "end should be preserved"
            }

            test "removeDuplicatePointsFaithfully4" {
                let pl = Polyline2D.create [Pt(0.,1.); Pt(0.,0.); Pt(0.994,0.); Pt(1.0,0.002); Pt(1.,1.) ]
                let cleaned = Polyline2D.removeDuplicatePointsFaithfully 0.01 pl
                Expect.equal cleaned.PointCount 4 "one point removed as duplicate within tolerance"
                expectEqPts cleaned.AsPoints.[2] (Pt(1.0,0.0))  "edges re-intersected , not moved to average"
                expectEqPts cleaned.Start pl.Start  "start should be preserved"
                expectEqPts cleaned.End pl.End  "end should be preserved"
            }


            test "removeDuplicatePoints" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(0.999,0.); Pt(1.0,0.001); Pt(1.,1.) ]
                let cleaned = Polyline2D.removeDuplicatePoints 0.005 pl
                Expect.equal cleaned.PointCount 3 "first point of duplicates kept"
            }

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
            test "removeDuplicateAndColinearPoints square" {
                let pl = Polyline2D.create [ Pt(2.,0.); Pt(4.,0.); Pt(4.,2.); Pt(4.,4.); Pt(2.,4.); Pt(0.,4.); Pt(0.,2.); Pt(0.,0.); Pt(2.,0.)]
                let simplified = Polyline2D.removeDuplicateAndColinearPoints Cosine.``0.1`` 1e-9  pl
                Expect.equal simplified.PointCount 5 $"no colinear simplification with loose angle tolerance {simplified.AsFSharpCode}"
                Expect.isTrue simplified.IsClosed "remains closed"
            }
            test "removeDuplicateAndColinearPoints open line with repeats" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,0.); Pt(2.,0.); Pt(3.,0.); Pt(3.,0.)]
                let simplified = Polyline2D.removeDuplicateAndColinearPoints Cosine.``0.1`` 1e-9 pl
                Expect.equal simplified.PointCount 2 "duplicate end removed; interior colinear points removed"
                "start" |> (expectEqPts simplified.AsPoints.[0] (Pt(0.,0.)))
                "end" |> (expectEqPts simplified.AsPoints.[simplified.AsPoints.Count - 1] (Pt(3.,0.)))
            }
            test "colinear start/end segments closed polygon" {
                // square with duplicated first edge points and last edge points before closure
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,0.); Pt(2.,0.); Pt(2.,2.); Pt(2.,4.); Pt(1.,4.); Pt(0.,4.); Pt(0.,2.); Pt(0.,0.)]
                let simplified = Polyline2D.removeDuplicateAndColinearPoints Cosine.``0.1`` 1e-9 pl
                // currently interior colinear points retained
                Expect.isTrue simplified.IsClosed "closed retained"
                Expect.equal simplified.PointCount 5 " colinear removal at this tolerance"
            }
            test "almost colinear tiny angle retained" {
                // create slight bend ~0.01 degrees so cosine almost 1
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(20.,0.2); Pt(30.,0.)]
                let simplified = Polyline2D.removeDuplicateAndColinearPoints Cosine.``0.01`` 1e-9 pl
                Expect.equal simplified.PointCount 4 "almost straight kept"
            }
            test "open U-turn 180 collapse prevented (since method only removes colinear not u-turn)" {
                // a U-turn creates 180 change; algorithm compares cosine; using points making a sharp reversal
                let pl = Polyline2D.create [Pt(0.,0.); Pt(5.,0.); Pt(5.,0.); Pt(0.,0.); Pt(-5.,0.)]
                let simplified = Polyline2D.removeDuplicateAndColinearPoints Cosine.``0.1`` 1e-9 pl
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

        testList "SetPoint" [
            test "set vertex on open polyline" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,0.); Pt(2.,0.); Pt(3.,0.)]
                pl.SetPt (1, Pt(1.,5.))
                "vertex 1 changed" |> expectEqPts pl.AsPoints.[1] (Pt(1.,5.))
                "vertex 0 unchanged" |> expectEqPts pl.AsPoints.[0] (Pt(0.,0.))
            }
            test "set first vertex on closed polyline updates last" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.); Pt(0.,10.); Pt(0.,0.)]
                pl.SetPointXYClosed (0, 1., 1.)
                "first vertex updated" |> expectEqPts pl.AsPoints.[0] (Pt(1.,1.))
                "last vertex updated too" |> expectEqPts pl.AsPoints.[4] (Pt(1.,1.))
            }
            test "set last vertex on closed polyline updates first" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.); Pt(0.,10.); Pt(0.,0.)]
                pl.SetPointXYClosed (4, 2., 2.)
                "last vertex updated" |> expectEqPts pl.AsPoints.[4] (Pt(2.,2.))
                "first vertex updated too" |> expectEqPts pl.AsPoints.[0] (Pt(2.,2.))
            }
            // test "point property setters on closed polyline keep endpoints paired" {
            //     let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.); Pt(0.,10.); Pt(0.,0.)]
            //     pl.Start <- Pt(3.,3.)
            //     "start setter updates first" |> expectEqPts pl.Points.[0] (Pt(3.,3.))
            //     "start setter updates last" |> expectEqPts pl.Points.[4] (Pt(3.,3.))
            //     pl.LastPoint <- Pt(4.,4.)
            //     "last point setter updates first" |> expectEqPts pl.Points.[0] (Pt(4.,4.))
            //     "last point setter updates last" |> expectEqPts pl.Points.[4] (Pt(4.,4.))
            // }
            test "set vertex out of range throws" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,0.)]
                Expect.throws (fun () -> pl.SetPt (-1, Pt(0.,0.))) "negative index"
                Expect.throws (fun () -> pl.SetPt (2, Pt(0.,0.))) "too large index"
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
                "second point set" |> expectEqPts pl.AsPoints.[1] (Pt(9.,9.))
            }
            test "get second last point" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,2.); Pt(3.,4.)]
                "second last point" |> expectEqPts pl.SecondLastPoint (Pt(1.,2.))
            }
            test "set second last point" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,2.); Pt(3.,4.)]
                pl.SecondLastPoint <- Pt(7.,7.)
                "second last point set" |> expectEqPts pl.AsPoints.[1] (Pt(7.,7.))
            }
            test "second point fails on single point" {
                Expect.throws (fun () -> plSinglePoint.SecondPoint |> ignore) "get second point on single"
                Expect.throws (fun () -> plSinglePoint.SecondLastPoint |> ignore) "get second last on single"
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
                let closed = Polyline2D.close 1e-6 plOpen
                Expect.isTrue closed.IsClosed "should be closed"
                Expect.equal closed.PointCount (plOpen.PointCount + 1) "one point added"
                "first equals last" |> expectEqPts closed.AsPoints.[0] closed.AsPoints.[closed.PointCount - 1]
            }
            test "close already closed polyline" {
                let closed = Polyline2D.close 1e-6 plClosed
                Expect.isTrue closed.IsClosed "should remain closed"
                Expect.equal closed.PointCount plClosed.PointCount "no extra point added"
            }
            test "close nearly closed polyline snaps last" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.); Pt(0.,0.00000001)]
                let closed = Polyline2D.close 1e-6 pl
                Expect.isTrue closed.IsClosed "should be closed"
                "last snapped to first" |> expectEqPts closed.AsPoints.[0] closed.AsPoints.[closed.PointCount - 1]
            }
            test "close fails with less than 3 points" {
                Expect.throws (fun () -> Polyline2D.close 1e-6 plSinglePoint |> ignore) "single point close should fail"
                Expect.throws (fun () -> Polyline2D.close 1e-6 plLine |> ignore) "two point close should fail"
            }
        ]

        testList "Static closeInPlace" [
            test "closeInPlace on open" {
                let pl = plOpen.Clone()
                Polyline2D.closeInPlace 1e-6 pl |> ignore
                Expect.isTrue pl.IsClosed "should be closed after closeInPlace"
            }
            test "closeInPlace on nearly closed snaps" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.); Pt(0.,0.00000001)]
                Polyline2D.closeInPlace 1e-6 pl |> ignore
                "last snapped to first" |> expectEqPts pl.AsPoints.[0] pl.AsPoints.[pl.PointCount - 1]
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
                "rotated first point" |> expectEqPts rotated.AsPoints.[0] (Pt(10., 0.))
                // (10,0) relative to (5,5) is (5,-5), rotated 90 CCW is (5,5), absolute (10,10)
                "rotated second point" |> expectEqPts rotated.AsPoints.[1] (Pt(10., 10.))
            }
        ]

        testList "Static wrappers" [
            test "static start" {
                "static start" |> expectEqPts (Polyline2D.start plOpen) plOpen.Start
            }
            test "static ende" {
                "static ende" |> expectEqPts (Polyline2D.end' plOpen) plOpen.End
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
                Polyline2D.reverseInPlace copy |> ignore
                "first after reverse" |> expectEqPts copy.AsPoints.[0] (Pt(0.,10.))
            }
            test "static reverse" {
                let rev = Polyline2D.reverse plOpen
                "first after reverse" |> expectEqPts rev.AsPoints.[0] (Pt(0.,10.))
                "original unchanged" |> expectEqPts plOpen.AsPoints.[0] (Pt(0.,0.))
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
            test "static closestPointIndex" {
                let idx = Polyline2D.closestPointIndex plOpen (Pt(9.,0.5))
                Expect.equal idx 1 "static closestPointIndex nearest to (10,0)"
            }
            test "static distanceTo" {
                let d = Polyline2D.distanceTo plOpen (Pt(5.,-3.))
                "static distanceTo" |> Expect.isTrue (abs(d - 3.0) < 1e-9)
            }
            test "static scale" {
                let s = Polyline2D.scale 3.0 plLine
                "static scale" |> expectEqPts s.AsPoints.[1] (Pt(30.,0.))
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
            test "static end' fails on empty" {
                Expect.throws (fun () -> Polyline2D.end' plEmpty |> ignore) "end' on empty"
            }
        ]

        testList "float storage and compatibility" [
            test "createDirectlyUnsafe copies point array" {
                let ra = ResizeArray [Pt(0.,0.); Pt(1.,1.)]
                let pl = Polyline2D ra
                ra.Add(Pt(2.,2.))
                Expect.equal pl.PointCount 2 "point array mutation is not reflected"
            }

            test "coordinate accessors and setters use float storage" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,1.)]
                Expect.equal (pl.GetX 1) 1.0 "x accessor"
                Expect.equal (pl.GetY 1) 1.0 "y accessor"
                pl.SetPointXY (1, 2.0, 3.0)
                "updated point" |> expectEqPts pl.AsPoints.[1] (Pt(2.,3.))
                pl.AddXY (4.0, 5.0)
                Expect.equal pl.PointCount 3 "added coordinate pair"
                "added point" |> expectEqPts pl.AsPoints.[2] (Pt(4.,5.))
            }
            test "coordinatesUnsafeInternal returns live buffer" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,1.)]
                let cs = Polyline2D.getXYs pl
                cs.Add 2.0
                cs.Add 2.0
                Expect.equal pl.PointCount 3 "coordinate mutation reflected in polyline"
                "new point from coordinates" |> expectEqPts pl.AsPoints.[2] (Pt(2.,2.))
            }
            test "createDirectlyUnsafeCoordinates shares coordinate buffer" {
                let cs = ResizeArray [0.0; 0.0; 1.0; 1.0]
                let pl = Polyline2D.createDirectly cs
                cs.Add 2.0
                cs.Add 2.0
                Expect.equal pl.PointCount 3 "shared coordinates reflect changes"
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
            test "closest point index single point" {
                let idx = plSinglePoint.ClosestPointIndex(Pt(5.,5.))
                Expect.equal idx 0 "single point index is 0"
            }
            test "distance to single point" {
                let d = plSinglePoint.DistanceTo(Pt(4.,1.))
                "distance to single point" |> Expect.isTrue (abs(d - 3.0) < 1e-9)
            }
            test "closest point/parameter/index fails on empty" {
                Expect.throws (fun () -> plEmpty.ClosestPoint(Pt(0.,0.)) |> ignore) "closest point empty"
                Expect.throws (fun () -> plEmpty.ClosestParameter(Pt(0.,0.)) |> ignore) "closest parameter empty"
                Expect.throws (fun () -> plEmpty.ClosestPointIndex(Pt(0.,0.)) |> ignore) "closest index empty"
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
                "reversed sub start" |> expectEqPts sub.AsPoints.[0] (Pt(5.,10.))
                "reversed sub end" |> expectEqPts sub.AsPoints.[sub.PointCount - 1] (Pt(5.,0.))
            }
            test "sub polyline at integer params" {
                let sub = Polyline2D.subPolyline 1.0 2.0 plOpen
                "integer start" |> expectEqPts sub.AsPoints.[0] (Pt(10.,0.))
                "integer end" |> expectEqPts sub.AsPoints.[sub.PointCount - 1] (Pt(10.,10.))
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
                | Some (pt, i, j) ->
                    "intersection near center" |> expectEqPts pt (Pt(5.,5.))
                    Expect.equal (i, j) (0, 2) "returns segment indices, not flat coordinate indices"
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
            test "first and last segment intersect in open polyline" {
                let pl = Polyline2D.create [
                    Pt(0.,0.); Pt(10.,0.); Pt(10.,3.); Pt(10.,5.); Pt(2.,-2.)
                ]
                let result = Polyline2D.tryFindSelfIntersection pl
                Expect.isSome result "open polyline should report crossing between first and last segment"
                match result with
                | Some (pt, i, j) ->
                    "intersection point" |> expectEqPts pt (Pt(30. / 7., 0.))
                    Expect.equal (i, j) (0, 3) "returns segment indices, not flat coordinate indices"
                | None -> ()
            }
        ]

        testList "CloseInPlace member" [
            test "close in place with close ends snaps" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.); Pt(0.,0.0001)]
                pl.CloseInPlace(0.001)
                "last snapped to first" |> expectEqPts pl.AsPoints.[pl.PointCount - 1] (Pt(0.,0.))
                Expect.equal pl.PointCount 4 "no point added, snapped"
            }
            test "close in place with far ends adds point" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.)]
                pl.CloseInPlace(0.001)
                Expect.equal pl.PointCount 4 "point added"
                "added first as last" |> expectEqPts pl.AsPoints.[3] (Pt(0.,0.))
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
                "start set" |> expectEqPts pl.AsPoints.[0] (Pt(99.,99.))
            }
            test "set end" {
                let pl = plOpen.Clone()
                pl.End <- Pt(88.,88.)
                "end set" |> expectEqPts pl.AsPoints.[pl.PointCount - 1] (Pt(88.,88.))
            }
            test "set first point" {
                let pl = plOpen.Clone()
                pl.FirstPoint <- Pt(77.,77.)
                "first point set" |> expectEqPts pl.AsPoints.[0] (Pt(77.,77.))
            }
            test "set last point" {
                let pl = plOpen.Clone()
                pl.LastPoint <- Pt(66.,66.)
                "last point set" |> expectEqPts pl.AsPoints.[pl.PointCount - 1] (Pt(66.,66.))
            }
        ]

        testList "Offset edge cases" [
            test "offset fails with less than 2 points" {
                Expect.throws (fun () -> Polyline2D.offset(plSinglePoint, 1.0) |> ignore) "single point offset"
            }
            test "offsetVar fails with less than 2 points" {
                Expect.throws (fun () -> Polyline2D.offsetVar(plSinglePoint, ResizeArray [|1.0|]) |> ignore) "single point offsetVar"
            }
        ]

        testList "removeDuplicateAndColinearPoints edge cases" [
            test "single point returns same" {
                let result = Polyline2D.removeDuplicateAndColinearPoints Cosine.``0.1`` 1e-9 plSinglePoint
                Expect.equal result.PointCount 1 "single point unchanged"
            }
            test "empty returns same" {
                let result = Polyline2D.removeDuplicateAndColinearPoints Cosine.``0.1`` 1e-9 plEmpty
                Expect.equal result.PointCount 0 "empty unchanged"
            }
            test "tolerance too tight throws" {
                // Cosine.``0.01`` is the maximum allowed; a value above it (closer to 1.0) should throw
                let tooTight = UtilEuclid.withMeasure 0.9999999999 // cosine of ~0.0 degrees, above Cosine.``0.01``
                Expect.throws
                    (fun () -> Polyline2D.removeDuplicateAndColinearPoints tooTight 1e-9 plOpen |> ignore)
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
                    $"point {i}" |> expectEqPts moved.AsPoints.[i] translated.AsPoints.[i]
            }
        ]
    ]


let testsSpecial =
    testList "Polyline2D Special Cases" [

        testList "tryFindSelfIntersection special cases" [
            test "simple closed square has no self intersection" {
                // The shared start/end (closure) vertex of a closed polyline is not a self intersection.
                let result = Polyline2D.tryFindSelfIntersection plClosed
                Expect.isNone result "closure seam of a simple closed square must not be reported"
            }
            test "closed triangle has no self intersection" {
                let result = Polyline2D.tryFindSelfIntersection plTriangle
                Expect.isNone result "closed triangle has no self intersection"
            }
            test "open 3-point corner has no self intersection" {
                // first and last segment are adjacent (share the middle vertex), so their corner is not a crossing
                let pl = Polyline2D.create [Pt(0.,0.); Pt(5.,0.); Pt(5.,5.)]
                let result = Polyline2D.tryFindSelfIntersection pl
                Expect.isNone result "an open corner of two adjacent segments is not a self intersection"
            }
            test "degenerate polylines have no self intersection" {
                Expect.isNone (Polyline2D.tryFindSelfIntersection plEmpty) "empty has no self intersection"
                Expect.isNone (Polyline2D.tryFindSelfIntersection plSinglePoint) "single point has no self intersection"
                Expect.isNone (Polyline2D.tryFindSelfIntersection plLine) "single segment has no self intersection"
            }
            test "closed figure-8 is still detected" {
                let fig8 = Polyline2D.create [Pt(0.,0.); Pt(10.,10.); Pt(10.,0.); Pt(0.,10.); Pt(0.,0.)]
                let result = Polyline2D.tryFindSelfIntersection fig8
                Expect.isSome result "a real crossing must still be found"
                match result with
                | Some (pt, _, _) -> "crossing near center" |> expectEqPts pt (Pt(5.,5.))
                | None -> ()
            }
            test "non-adjacent touching segments are reported" {
                // two non-adjacent segments meet at (5,5)
                let pl = Polyline2D.create [Pt(0.,0.); Pt(5.,5.); Pt(10.,0.); Pt(5.,5.); Pt(0.,10.)]
                Expect.isSome (Polyline2D.tryFindSelfIntersection pl) "non-adjacent touching segments should be found"
            }
        ]

        testList "IsClosed special cases" [
            test "three point back-and-forth is closed" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,1.); Pt(0.,0.)]
                Expect.isTrue pl.IsClosed "first equals last with 3 points is closed"
            }
            test "two identical points are not closed" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(0.,0.)]
                Expect.isFalse pl.IsClosed "fewer than 3 points is never closed"
            }
        ]

        testList "SignedArea and Area on open polylines" [
            test "signed area of open CCW triangle includes closing segment" {
                let openTri = Polyline2D.create [Pt(0.,0.); Pt(4.,0.); Pt(0.,3.)]
                "open triangle signed area" |> Expect.isTrue (abs(openTri.SignedArea - 6.0) < 1e-9)
                "open triangle area" |> Expect.isTrue (abs(openTri.Area - 6.0) < 1e-9)
            }
            test "signed area of open CW triangle is negative" {
                let openTri = Polyline2D.create [Pt(0.,0.); Pt(0.,3.); Pt(4.,0.)]
                "open CW triangle signed area" |> Expect.isTrue (openTri.SignedArea < 0.0)
                "open CW triangle area is positive" |> Expect.isTrue (abs(openTri.Area - 6.0) < 1e-9)
            }
        ]

        testList "Center counts stored points" [
            test "center of closed polyline counts the duplicated closure vertex" {
                // plClosed stores 5 points (start vertex twice), so the average is shifted from the geometric centroid (5,5)
                "center is average of all 5 stored points" |> expectEqPts plClosed.Center (Pt(4.,4.))
            }
        ]

        testList "EvaluateAt degenerate" [
            test "evaluate single point polyline at 0" {
                "single point eval at 0" |> expectEqPts (plSinglePoint.EvaluateAt 0.0) (Pt(1.,1.))
            }
        ]

        testList "Reverse degenerate" [
            test "reverse empty stays empty" {
                Expect.equal (plEmpty.Reverse()).PointCount 0 "reversed empty is empty"
            }
            test "reverse single point stays the same" {
                let r = plSinglePoint.Reverse()
                Expect.equal r.PointCount 1 "reversed single is single"
                "same point" |> expectEqPts r.AsPoints.[0] (Pt(1.,1.))
            }
            test "reverseInPlace on empty and single does not throw" {
                plEmpty.Clone().ReverseInPlace()
                plSinglePoint.Clone().ReverseInPlace()
                Expect.isTrue true "no exception on degenerate reverseInPlace"
            }
        ]

        testList "Duplicate and Clone independence" [
            test "mutating a duplicate does not affect the original" {
                let orig = Polyline2D.create [Pt(0.,0.); Pt(1.,1.)]
                let dup = orig.Duplicate()
                dup.SetPointXY(0, 9., 9.)
                "original unchanged" |> expectEqPts orig.AsPoints.[0] (Pt(0.,0.))
                "duplicate changed" |> expectEqPts dup.AsPoints.[0] (Pt(9.,9.))
            }
            test "AsPoints returns a copy" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(1.,1.)]
                let pts = pl.AsPoints
                pts.[0] <- Pt(9.,9.)
                "polyline unchanged after mutating AsPoints copy" |> expectEqPts pl.AsPoints.[0] (Pt(0.,0.))
            }
        ]

        testList "Scale special cases" [
            test "scale by negative factor mirrors through origin" {
                let s = plLine.Scale(-1.0)
                "scaled second point" |> expectEqPts s.AsPoints.[1] (Pt(-10.,0.))
            }
            test "scale by zero collapses to origin" {
                let s = plOpen.Scale(0.0)
                for i = 0 to s.PointCount - 1 do
                    $"point {i} at origin" |> expectEqPts s.AsPoints.[i] (Pt(0.,0.))
            }
            test "scaleOn by zero collapses to center" {
                let cen = Pt(5.,5.)
                let s = Polyline2D.scaleOn cen 0.0 plOpen
                for i = 0 to s.PointCount - 1 do
                    $"point {i} at center" |> expectEqPts s.AsPoints.[i] cen
            }
        ]

        testList "map / iter family" [
            test "map transforms by coordinates" {
                let m = Polyline2D.map (fun x y -> Pt(x + 1., y + 2.)) plLine
                "mapped first" |> expectEqPts m.AsPoints.[0] (Pt(1.,2.))
                "mapped second" |> expectEqPts m.AsPoints.[1] (Pt(11.,2.))
            }
            test "mapi uses point index" {
                let m = Polyline2D.mapi (fun i _ y -> Pt(float i, y)) plOpen
                "mapi point 0 x" |> Expect.isTrue (abs(m.GetX 0 - 0.0) < 1e-9)
                "mapi point 3 x" |> Expect.isTrue (abs(m.GetX 3 - 3.0) < 1e-9)
            }
            test "mapiPt uses point index" {
                let m = Polyline2D.mapiPt (fun i pt -> Pt(pt.X + float i, pt.Y)) plOpen
                // plOpen: (0,0),(10,0),(10,10),(0,10) -> shift x by index
                "mapiPt point 0" |> expectEqPts m.AsPoints.[0] (Pt(0.,0.))
                "mapiPt point 1" |> expectEqPts m.AsPoints.[1] (Pt(11.,0.))
                "mapiPt point 3" |> expectEqPts m.AsPoints.[3] (Pt(3.,10.))
            }
            test "iter visits every point" {
                let mutable sumX = 0.0
                Polyline2D.iter (fun x _ -> sumX <- sumX + x) plOpen
                "sum of x coordinates" |> Expect.isTrue (abs(sumX - 20.0) < 1e-9)
            }
            test "iteri provides indices" {
                let mutable idxSum = 0
                Polyline2D.iteri (fun i _ _ -> idxSum <- idxSum + i) plOpen
                Expect.equal idxSum 6 "indices 0+1+2+3"
            }
        ]

        testList "create helpers" [
            test "createFromXYMembers from points" {
                let pl = Polyline2D.createFromXYMembers [Pt(1.,2.); Pt(3.,4.)]
                Expect.equal pl.PointCount 2 "two points"
                "first" |> expectEqPts pl.AsPoints.[0] (Pt(1.,2.))
                "second" |> expectEqPts pl.AsPoints.[1] (Pt(3.,4.))
            }
            test "createFromBRectCCW is closed and counter-clockwise" {
                let br = BRect.create(Pt(0.,0.), Pt(10.,5.))
                let pl = Polyline2D.createFromBRectCCW br
                Expect.equal pl.PointCount 5 "rect loop has 5 points"
                Expect.isTrue pl.IsClosed "should be closed"
                "starts at min corner" |> expectEqPts pl.AsPoints.[0] (Pt(0.,0.))
                "positive signed area" |> Expect.isTrue (pl.SignedArea > 0.0)
            }
            test "createFromBRectCW is closed and clockwise" {
                let br = BRect.create(Pt(0.,0.), Pt(10.,5.))
                let pl = Polyline2D.createFromBRectCW br
                Expect.equal pl.PointCount 5 "rect loop has 5 points"
                Expect.isTrue pl.IsClosed "should be closed"
                "negative signed area" |> Expect.isTrue (pl.SignedArea < 0.0)
            }
            test "createFromRectCCW is closed" {
                let r = Rect2D.createFromBRect (BRect.create(Pt(0.,0.), Pt(10.,5.)))
                let pl = Polyline2D.createFromRectCCW r
                Expect.equal pl.PointCount 5 "rect loop has 5 points"
                Expect.isTrue pl.IsClosed "should be closed"
            }
        ]

        testList "GetSegment on closed polyline" [
            test "last segment of closed square closes the loop" {
                let seg = plClosed.GetSegment(plClosed.SegmentCount - 1)
                "closed last segment from" |> expectEqPts seg.From (Pt(0.,10.))
                "closed last segment to" |> expectEqPts seg.To (Pt(0.,0.))
            }
        ]

        testList "removeUTurns no-op" [
            test "polyline without U-turns keeps all points" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(5.,0.); Pt(5.,5.); Pt(0.,5.)]
                let r = Polyline2D.removeUTurns Cosine.``179.0`` pl
                Expect.equal r.PointCount pl.PointCount "no U-turns, point count preserved"
                "first preserved" |> expectEqPts r.AsPoints.[0] pl.AsPoints.[0]
                "last preserved" |> expectEqPts r.AsPoints.[r.PointCount - 1] pl.AsPoints.[pl.PointCount - 1]
            }
        ]

        testList "SegmentVectorsXY" [
            test "empty polyline returns empty list" {
                let vs = plEmpty.SegmentVectorsXY
                Expect.equal vs.Count 0 "empty polyline has no segment vectors"
            }
            test "single point returns empty list" {
                let vs = plSinglePoint.SegmentVectorsXY
                Expect.equal vs.Count 0 "single point has no segment vectors"
            }
            test "open polyline returns flat components" {
                let vs = plOpen.SegmentVectorsXY
                Expect.equal vs.Count (plOpen.XYs.Count - 2) "two floats less than xys count"
                Expect.isTrue (abs(vs.[0] - 10.0) < 1e-12 && abs(vs.[1]) < 1e-12) "first vector is (10, 0)"
                Expect.isTrue (abs(vs.[2]) < 1e-12 && abs(vs.[3] - 10.0) < 1e-12) "second vector is (0, 10)"
            }
        ]

        testList "zero tolerance closing" [
            test "close with 0.0 tolerance on exactly closed polyline adds no point" {
                let closed = Polyline2D.close 0.0 plClosed
                Expect.equal closed.PointCount plClosed.PointCount "no extra point added"
                Expect.isTrue closed.IsClosed "still closed"
            }
            test "IsAlmostClosed with 0.0 tolerance on exactly closed polyline" {
                Expect.isTrue (plClosed.IsAlmostClosed 0.0) "exactly closed is almost closed with 0.0 tolerance"
                Expect.isFalse (plOpen.IsAlmostClosed 0.0) "open is not almost closed with 0.0 tolerance"
            }
        ]

        testList "removeDuplicatePoints keeps first point" [
            test "two near-duplicate points collapse to the first point" {
                let pl = Polyline2D.create [Pt(1.,1.); Pt(1.0000001, 1.)]
                let result = Polyline2D.removeDuplicatePoints 1e-6 pl
                Expect.equal result.PointCount 1 "collapses to one point"
                "kept point is the first point" |> expectEqPts result.AsPoints.[0] (Pt(1.,1.))
            }
        ]

        testList "tryFindSelfIntersectionWithBRect" [
            test "bowtie intersection found by both variants" {
                let bowtie = Polyline2D.create [Pt(0.,0.); Pt(10.,10.); Pt(10.,0.); Pt(0.,10.)]
                match Polyline2D.tryFindSelfIntersection bowtie, Polyline2D.tryFindSelfIntersectionWithBRect bowtie with
                | Some (pt1, i1, j1), Some (pt2, i2, j2) ->
                    "same intersection point" |> expectEqPts pt1 pt2
                    Expect.equal (i1, j1) (i2, j2) "same segment indices"
                    "intersection at (5,5)" |> expectEqPts pt1 (Pt(5.,5.))
                | a, b ->
                    failwithf "both variants should find the intersection but got %A and %A" a b
            }
            test "closed square is not a self intersection in both variants" {
                Expect.isNone (Polyline2D.tryFindSelfIntersection plClosed) "closed square has no self intersection"
                Expect.isNone (Polyline2D.tryFindSelfIntersectionWithBRect plClosed) "closed square has no self intersection with BRect"
            }
        ]

        testList "fail fast on degenerate input" [
            test "SignedArea on empty polyline throws" {
                Expect.throws (fun () -> plEmpty.SignedArea |> ignore) "empty polyline has no signed area"
            }
            test "SignedArea on single point returns zero" {
                Expect.floatClose Accuracy.veryHigh plSinglePoint.SignedArea 0.0 "single point has zero area"
            }
            test "removeDuplicateAndColinearPoints fails when all points are duplicates" {
                let pl = Polyline2D.create [Pt(1.,1.); Pt(1.,1.); Pt(1.,1.)]
                Expect.throws (fun () -> Polyline2D.removeDuplicateAndColinearPoints Cosine.``0.1`` 1e-6 pl |> ignore) "all duplicates should fail with a clear error"
            }
            test "FindLabelPoint fails on zero or negative precision" {
                Expect.throws (fun () -> plClosed.FindLabelPoint 0.0 |> ignore) "zero precision should fail"
                Expect.throws (fun () -> plClosed.FindLabelPoint -1.0 |> ignore) "negative precision should fail"
            }
        ]

        testList "instance transform methods match their statics" [
            test "Move, MoveX and MoveY instance methods" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.)]
                "Move matches translate" |> expectEqPts (pl.Move(Vc(5.,3.))).Start (Polyline2D.translate (Vc(5.,3.)) pl).Start
                "Move - last point" |> expectEqPts (pl.Move(Vc(5.,3.))).End (Pt(15.,13.))
                "MoveX - first point" |> expectEqPts (pl.MoveX 5.).Start (Pt(5.,0.))
                "MoveX - last point"  |> expectEqPts (pl.MoveX 5.).End (Pt(15.,10.))
                "MoveY - first point" |> expectEqPts (pl.MoveY 3.).Start (Pt(0.,3.))
            }
            test "Rotate instance method matches static rotate" {
                let pl = Polyline2D.create [Pt(10.,0.); Pt(10.,10.)]
                let r = Rotation2D.createFromDegrees 90.
                "Rotate - first point goes to (0,10)" |> expectEqPts (pl.Rotate r).Start (Pt(0.,10.))
                "Rotate matches static" |> expectEqPts (pl.Rotate r).End (Polyline2D.rotate r pl).End
            }
            test "RotateWithCenter keeps the center fixed and matches the static" {
                let pl = Polyline2D.create [Pt(0.,0.); Pt(2.,0.); Pt(2.,2.); Pt(0.,2.)]
                let cen = pl.Center
                let r = Rotation2D.createFromDegrees 90.
                let rotated = pl.RotateWithCenter(cen, r)
                "center stays put" |> expectEqPts rotated.Center cen
                "matches static rotateWithCenter" |> expectEqPts rotated.Start (Polyline2D.rotateWithCenter cen r pl).Start
                "original not mutated" |> expectEqPts pl.Start (Pt(0.,0.))
            }
        ]
    ]
