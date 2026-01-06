module TestBRect

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let inline eqPt a b = Pt.distance a b < 1e-9
let inline eqVc a b = Vc.length (a - b) < 1e-9

let tests =
    testList "BRect" [

        testList "Constructor and Basic Properties" [
            test "create from two points" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 5.))
                Expect.equal r.MinX 0. "MinX should be 0"
                Expect.equal r.MinY 0. "MinY should be 0"
                Expect.equal r.MaxX 10. "MaxX should be 10"
                Expect.equal r.MaxY 5. "MaxY should be 5"
            }

            test "create from two points with swapped coordinates" {
                let r = BRect.create(Pt(10., 5.), Pt(0., 0.))
                Expect.equal r.MinX 0. "MinX should be 0 after sorting"
                Expect.equal r.MinY 0. "MinY should be 0 after sorting"
                Expect.equal r.MaxX 10. "MaxX should be 10 after sorting"
                Expect.equal r.MaxY 5. "MaxY should be 5 after sorting"
            }

            test "MinPt and MaxPt properties" {
                let r = BRect.create(Pt(1., 2.), Pt(5., 7.))
                Expect.isTrue (eqPt r.MinPt (Pt(1., 2.))) "MinPt should be (1, 2)"
                Expect.isTrue (eqPt r.MaxPt (Pt(5., 7.))) "MaxPt should be (5, 7)"
            }

            test "SizeX and SizeY" {
                let r = BRect.create(Pt(1., 2.), Pt(5., 7.))
                Expect.equal r.SizeX 4. "SizeX should be 4"
                Expect.equal r.SizeY 5. "SizeY should be 5"
            }

            test "Center" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 6.))
                Expect.isTrue (eqPt r.Center (Pt(5., 3.))) "Center should be (5, 3)"
            }

            test "Diagonal" {
                let r = BRect.create(Pt(1., 2.), Pt(5., 7.))
                Expect.isTrue (eqVc r.Diagonal (Vc(4., 5.))) "Diagonal should be (4, 5)"
            }

            test "Area" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 5.))
                Expect.equal r.Area 50. "Area should be 50"
            }
        ]

        testList "Creation Methods" [
            test "createFromSeq with valid points" {
                let pts = [Pt(1., 2.); Pt(5., 3.); Pt(2., 7.); Pt(8., 1.)]
                let r = BRect.createFromSeq pts
                Expect.equal r.MinX 1. "MinX should be 1"
                Expect.equal r.MinY 1. "MinY should be 1"
                Expect.equal r.MaxX 8. "MaxX should be 8"
                Expect.equal r.MaxY 7. "MaxY should be 7"
            }

            test "createFromSeq with single point" {
                let pts = [Pt(5., 3.)]
                let r = BRect.createFromSeq pts
                Expect.equal r.MinX 5. "MinX should be 5"
                Expect.equal r.MinY 3. "MinY should be 3"
                Expect.equal r.MaxX 5. "MaxX should be 5"
                Expect.equal r.MaxY 3. "MaxY should be 3"
            }

            test "createFromIList with valid points" {
                let pts = ResizeArray([Pt(1., 2.); Pt(5., 3.); Pt(2., 7.)])
                let r = BRect.createFromIList pts
                Expect.equal r.MinX 1. "MinX should be 1"
                Expect.equal r.MaxY 7. "MaxY should be 7"
            }

            test "createFromCenter with valid size" {
                let r = BRect.createFromCenter(Pt(5., 5.), 10., 6.)
                Expect.equal r.MinX 0. "MinX should be 0"
                Expect.equal r.MinY 2. "MinY should be 2"
                Expect.equal r.MaxX 10. "MaxX should be 10"
                Expect.equal r.MaxY 8. "MaxY should be 8"
            }

            test "createFromCenter rejects negative sizeX" {
                Expect.throws (fun () -> BRect.createFromCenter(Pt(5., 5.), -10., 6.) |> ignore) "Should throw on negative sizeX"
            }

            test "createFromCenter rejects negative sizeY" {
                Expect.throws (fun () -> BRect.createFromCenter(Pt(5., 5.), 10., -6.) |> ignore) "Should throw on negative sizeY"
            }

            test "createFromLine" {
                let line = Line2D(1., 2., 5., 7.)
                let r = BRect.createFromLine line
                Expect.equal r.MinX 1. "MinX should be 1"
                Expect.equal r.MinY 2. "MinY should be 2"
                Expect.equal r.MaxX 5. "MaxX should be 5"
                Expect.equal r.MaxY 7. "MaxY should be 7"
            }

            test "createFromLine with reversed line" {
                let line = Line2D(5., 7., 1., 2.)
                let r = BRect.createFromLine line
                Expect.equal r.MinX 1. "MinX should be 1 after sorting"
                Expect.equal r.MinY 2. "MinY should be 2 after sorting"
                Expect.equal r.MaxX 5. "MaxX should be 5 after sorting"
                Expect.equal r.MaxY 7. "MaxY should be 7 after sorting"
            }
        ]

        testList "Expand Methods" [
            test "Expand with positive distance" {
                let r = BRect.create(Pt(2., 3.), Pt(8., 9.))
                let expanded = r.Expand(1.)
                Expect.equal expanded.MinX 1. "MinX should be 1"
                Expect.equal expanded.MinY 2. "MinY should be 2"
                Expect.equal expanded.MaxX 9. "MaxX should be 9"
                Expect.equal expanded.MaxY 10. "MaxY should be 10"
            }

            test "Expand with negative distance causing underflow throws" {
                let r = BRect.create(Pt(2., 3.), Pt(8., 9.))
                Expect.throws (fun () -> r.Expand(-10.) |> ignore) "Should throw on underflow"
            }

            test "Expand(xDist, yDist) with positive distances" {
                let r = BRect.create(Pt(2., 3.), Pt(8., 9.))
                let expanded = r.Expand(1., 2.)
                Expect.equal expanded.MinX 1. "MinX should be 1"
                Expect.equal expanded.MinY 1. "MinY should be 1"
                Expect.equal expanded.MaxX 9. "MaxX should be 9"
                Expect.equal expanded.MaxY 11. "MaxY should be 11"
            }

            test "Expand(xDist, yDist) with distances causing underflow throws" {
                let r = BRect.create(Pt(2., 3.), Pt(8., 9.))
                Expect.throws (fun () -> r.Expand(-10., 0.) |> ignore) "Should throw on X underflow"
                Expect.throws (fun () -> r.Expand(0., -10.) |> ignore) "Should throw on Y underflow"
            }

            test "ExpandSafe with positive distance" {
                let r = BRect.create(Pt(2., 3.), Pt(8., 9.))
                let expanded = r.ExpandSafe(1.)
                Expect.equal expanded.MinX 1. "MinX should be 1"
                Expect.equal expanded.MaxX 9. "MaxX should be 9"
            }

            test "ExpandSafe with negative distance causing underflow collapses to midpoint" {
                let r = BRect.create(Pt(2., 3.), Pt(8., 9.))
                let shrunk = r.ExpandSafe(-10.)
                Expect.equal shrunk.MinX 5. "MinX should collapse to center X (5)"
                Expect.equal shrunk.MaxX 5. "MaxX should collapse to center X (5)"
                Expect.equal shrunk.MinY 6. "MinY should collapse to center Y (6)"
                Expect.equal shrunk.MaxY 6. "MaxY should collapse to center Y (6)"
            }

            test "ExpandSafe(xDist, yDist) with different underflow per axis" {
                let r = BRect.create(Pt(0., 0.), Pt(4., 10.))
                let shrunk = r.ExpandSafe(-5., -2.)
                Expect.equal shrunk.MinX 2. "MinX should collapse to center X (2)"
                Expect.equal shrunk.MaxX 2. "MaxX should collapse to center X (2)"
                Expect.equal shrunk.MinY 2. "MinY should be 2 (still valid)"
                Expect.equal shrunk.MaxY 8. "MaxY should be 8 (still valid)"
            }

            test "ExpandXaxis with positive distances" {
                let r = BRect.create(Pt(2., 3.), Pt(8., 9.))
                let expanded = r.ExpandXaxis(1., 2.)
                Expect.equal expanded.MinX 1. "MinX should be 1"
                Expect.equal expanded.MaxX 10. "MaxX should be 10"
                Expect.equal expanded.MinY 3. "MinY should remain 3"
                Expect.equal expanded.MaxY 9. "MaxY should remain 9"
            }

            test "ExpandXaxis with underflow throws" {
                let r = BRect.create(Pt(2., 3.), Pt(8., 9.))
                Expect.throws (fun () -> r.ExpandXaxis(-10., 0.) |> ignore) "Should throw on X underflow"
            }

            test "ExpandYaxis with positive distances" {
                let r = BRect.create(Pt(2., 3.), Pt(8., 9.))
                let expanded = r.ExpandYaxis(1., 2.)
                Expect.equal expanded.MinY 2. "MinY should be 2"
                Expect.equal expanded.MaxY 11. "MaxY should be 11"
                Expect.equal expanded.MinX 2. "MinX should remain 2"
                Expect.equal expanded.MaxX 8. "MaxX should remain 8"
            }

            test "ExpandYaxis with underflow throws" {
                let r = BRect.create(Pt(2., 3.), Pt(8., 9.))
                Expect.throws (fun () -> r.ExpandYaxis(-10., 0.) |> ignore) "Should throw on Y underflow"
            }
        ]

        testList "Overlap and Containment" [
            test "OverlapsWith two overlapping rectangles" {
                let r1 = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let r2 = BRect.create(Pt(5., 5.), Pt(15., 15.))
                Expect.isTrue (r1.OverlapsWith r2) "Should overlap"
            }

            test "OverlapsWith two touching rectangles" {
                let r1 = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let r2 = BRect.create(Pt(10., 0.), Pt(20., 10.))
                Expect.isTrue (r1.OverlapsWith r2) "Should be touching (considered overlapping)"
            }

            test "OverlapsWith two separate rectangles" {
                let r1 = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let r2 = BRect.create(Pt(11., 11.), Pt(20., 20.))
                Expect.isFalse (r1.OverlapsWith r2) "Should not overlap"
            }

            test "OverlapsWith with tolerance - overlapping" {
                let r1 = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let r2 = BRect.create(Pt(9., 9.), Pt(15., 15.))
                Expect.isTrue (r1.OverlapsWith(r2, 1.)) "Should overlap with tolerance"
            }

            test "OverlapsWith with tolerance - just touching" {
                let r1 = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let r2 = BRect.create(Pt(10., 0.), Pt(20., 10.))
                Expect.isFalse (r1.OverlapsWith(r2, 1.)) "Should not overlap with positive tolerance (just touching)"
            }

            test "Contains point inside" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                Expect.isTrue (r.Contains(Pt(5., 5.))) "Point (5,5) should be inside"
            }

            test "Contains point on edge" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                Expect.isTrue (r.Contains(Pt(10., 5.))) "Point on edge should be contained"
            }

            test "Contains point outside" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                Expect.isFalse (r.Contains(Pt(11., 5.))) "Point outside should not be contained"
            }

            test "Contains rectangle inside" {
                let r1 = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let r2 = BRect.create(Pt(2., 2.), Pt(8., 8.))
                Expect.isTrue (r1.Contains r2) "Inner rectangle should be contained"
            }

            test "Contains rectangle partially overlapping" {
                let r1 = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let r2 = BRect.create(Pt(5., 5.), Pt(15., 15.))
                Expect.isFalse (r1.Contains r2) "Partially overlapping rectangle should not be contained"
            }

            test "IsTouching detects edge touching" {
                let r1 = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let r2 = BRect.create(Pt(10., 0.), Pt(20., 10.))
                Expect.isTrue (r1.IsTouching(r2, 0.001)) "Rectangles touching at edge should return true"
            }

            test "IsTouching rejects overlapping" {
                let r1 = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let r2 = BRect.create(Pt(5., 5.), Pt(15., 15.))
                Expect.isFalse (r1.IsTouching(r2, 0.001)) "Overlapping rectangles should not be 'touching'"
            }
        ]

        testList "Intersection" [
            test "Intersection of overlapping rectangles" {
                let r1 = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let r2 = BRect.create(Pt(5., 5.), Pt(15., 15.))
                match r1.Intersection r2 with
                | ValueSome inter ->
                    Expect.equal inter.MinX 5. "Intersection MinX should be 5"
                    Expect.equal inter.MinY 5. "Intersection MinY should be 5"
                    Expect.equal inter.MaxX 10. "Intersection MaxX should be 10"
                    Expect.equal inter.MaxY 10. "Intersection MaxY should be 10"
                | ValueNone ->
                    failtest "Should have intersection"
            }

            test "Intersection of touching rectangles returns zero-area BRect" {
                let r1 = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let r2 = BRect.create(Pt(10., 0.), Pt(20., 10.))
                match r1.Intersection r2 with
                | ValueSome inter ->
                    Expect.equal inter.MinX 10. "Intersection MinX should be 10"
                    Expect.equal inter.MaxX 10. "Intersection MaxX should be 10"
                    Expect.equal inter.Area 0. "Intersection should have zero area"
                | ValueNone ->
                    failtest "Touching rectangles should return zero-area intersection"
            }

            test "Intersection of non-overlapping rectangles returns None" {
                let r1 = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let r2 = BRect.create(Pt(11., 11.), Pt(20., 20.))
                match r1.Intersection r2 with
                | ValueSome _ -> failtest "Should not have intersection"
                | ValueNone -> ()
            }

            test "Static intersection method matches member method" {
                let r1 = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let r2 = BRect.create(Pt(5., 5.), Pt(15., 15.))
                let memberResult = r1.Intersection r2
                let staticResult = BRect.intersection r1 r2
                match memberResult, staticResult with
                | ValueSome m, ValueSome s ->
                    Expect.isTrue (BRect.equals 0.0 m s) "Member and static methods should return same result"
                | ValueNone, ValueNone -> ()
                | _ -> failtest "Member and static methods should match"
            }

            test "Static intersection of touching rectangles matches member" {
                let r1 = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let r2 = BRect.create(Pt(10., 0.), Pt(20., 10.))
                let memberResult = r1.Intersection r2
                let staticResult = BRect.intersection r1 r2
                match memberResult, staticResult with
                | ValueSome m, ValueSome s ->
                    Expect.isTrue (BRect.equals 0.0 m s) "Both should return zero-area intersection"
                | ValueNone, ValueNone -> failtest "Both should return zero-area intersection, not None"
                | _ -> failtest "Member and static intersection methods should behave identically"
            }
        ]

        testList "Union" [
            test "Union of two rectangles" {
                let r1 = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let r2 = BRect.create(Pt(5., 5.), Pt(15., 15.))
                let union = r1.Union r2
                Expect.equal union.MinX 0. "Union MinX should be 0"
                Expect.equal union.MinY 0. "Union MinY should be 0"
                Expect.equal union.MaxX 15. "Union MaxX should be 15"
                Expect.equal union.MaxY 15. "Union MaxY should be 15"
            }

            test "Union with point inside" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let union = r.Union(Pt(5., 5.))
                Expect.isTrue (BRect.equals 0.0 r union) "Union with interior point should not change rectangle"
            }

            test "Union with point outside" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let union = r.Union(Pt(15., 15.))
                Expect.equal union.MinX 0. "Union MinX should be 0"
                Expect.equal union.MaxX 15. "Union MaxX should be 15"
                Expect.equal union.MaxY 15. "Union MaxY should be 15"
            }
        ]

        testList "Scale and Transform" [
            test "scale with positive factor" {
                let r = BRect.create(Pt(1., 2.), Pt(5., 6.))
                let scaled = BRect.scale 2.0 r
                Expect.equal scaled.MinX 2. "MinX should be 2"
                Expect.equal scaled.MinY 4. "MinY should be 4"
                Expect.equal scaled.MaxX 10. "MaxX should be 10"
                Expect.equal scaled.MaxY 12. "MaxY should be 12"
            }

            test "scale with zero factor" {
                let r = BRect.create(Pt(1., 2.), Pt(5., 6.))
                let scaled = BRect.scale 0.0 r
                Expect.equal scaled.MinX 0. "All coordinates should be 0"
                Expect.equal scaled.MaxX 0. "All coordinates should be 0"
            }

            test "scale with negative factor throws" {
                let r = BRect.create(Pt(1., 2.), Pt(5., 6.))
                Expect.throws (fun () -> BRect.scale -2.0 r |> ignore) "Should throw on negative scale factor"
            }

            test "move by vector" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let moved = BRect.move (Vc(5., 3.)) r
                Expect.equal moved.MinX 5. "MinX should be 5"
                Expect.equal moved.MinY 3. "MinY should be 3"
                Expect.equal moved.MaxX 15. "MaxX should be 15"
                Expect.equal moved.MaxY 13. "MaxY should be 13"
            }

            test "translate is same as move" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let moved = BRect.move (Vc(5., 3.)) r
                let translated = BRect.translate (Vc(5., 3.)) r
                Expect.isTrue (BRect.equals 0.0 moved translated) "move and translate should be identical"
            }

            test "moveX" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let moved = BRect.moveX 5. r
                Expect.equal moved.MinX 5. "MinX should be 5"
                Expect.equal moved.MinY 0. "MinY should remain 0"
                Expect.equal moved.MaxX 15. "MaxX should be 15"
            }

            test "moveY" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let moved = BRect.moveY 3. r
                Expect.equal moved.MinX 0. "MinX should remain 0"
                Expect.equal moved.MinY 3. "MinY should be 3"
                Expect.equal moved.MaxY 13. "MaxY should be 13"
            }
        ]

        testList "Relative Expansion" [
            test "expandRel with factor 1.0 keeps same size" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let expanded = BRect.expandRel 1.0 r
                Expect.isTrue (BRect.equals 0.001 r expanded) "Factor 1.0 should keep same size"
            }

            test "expandRel with factor 2.0 doubles size" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let expanded = BRect.expandRel 2.0 r
                Expect.equal expanded.SizeX 20. "SizeX should double"
                Expect.equal expanded.SizeY 20. "SizeY should double"
                Expect.isTrue (eqPt expanded.Center r.Center) "Center should remain the same"
            }

            test "expandRel with factor 0.5 halves size" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let expanded = BRect.expandRel 0.5 r
                Expect.equal expanded.SizeX 5. "SizeX should halve"
                Expect.equal expanded.SizeY 5. "SizeY should halve"
                Expect.isTrue (eqPt expanded.Center r.Center) "Center should remain the same"
            }

            test "expandRel with negative factor throws" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                Expect.throws (fun () -> BRect.expandRel -0.5 r |> ignore) "Should throw on negative factor"
            }

            test "expandRelXY with different factors" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let expanded = BRect.expandRelXY 2.0 0.5 r
                Expect.equal expanded.SizeX 20. "SizeX should double"
                Expect.equal expanded.SizeY 5. "SizeY should halve"
                Expect.isTrue (eqPt expanded.Center r.Center) "Center should remain the same"
            }

            test "expandRelXY with negative factorX throws" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                Expect.throws (fun () -> BRect.expandRelXY -2.0 0.5 r |> ignore) "Should throw on negative factorX"
            }

            test "expandRelXY with negative factorY throws" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                Expect.throws (fun () -> BRect.expandRelXY 2.0 -0.5 r |> ignore) "Should throw on negative factorY"
            }
        ]

        testList "Utility Methods" [
            test "EvaluateAt corners" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 20.))
                Expect.isTrue (eqPt (r.EvaluateAt(0., 0.)) (Pt(0., 0.))) "0,0 should be MinPt"
                Expect.isTrue (eqPt (r.EvaluateAt(1., 1.)) (Pt(10., 20.))) "1,1 should be MaxPt"
                Expect.isTrue (eqPt (r.EvaluateAt(0.5, 0.5)) (Pt(5., 10.))) "0.5,0.5 should be center"
            }

            test "LongestEdge" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 5.))
                Expect.equal r.LongestEdge 10. "Longest edge should be 10"
            }

            test "ShortestEdge" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 5.))
                Expect.equal r.ShortestEdge 5. "Shortest edge should be 5"
            }

            test "IsZero for tiny rectangle" {
                let r = BRect.create(Pt(0., 0.), Pt(1e-20, 1e-20))
                Expect.isTrue r.IsZero "Tiny rectangle should be zero"
            }

            test "IsPoint same as IsZero" {
                let r = BRect.create(Pt(0., 0.), Pt(1e-20, 1e-20))
                Expect.equal r.IsPoint r.IsZero "IsPoint should equal IsZero"
            }

            test "IsLine for line-like rectangle" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 1e-20))
                Expect.isTrue r.IsLine "Rectangle with one tiny dimension should be a line"
            }

            test "HasArea for valid rectangle" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 5.))
                Expect.isTrue r.HasArea "Valid rectangle should have area"
            }

            test "IsValid same as HasArea" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 5.))
                Expect.equal r.IsValid r.HasArea "IsValid should equal HasArea"
            }

            test "Corner points Pt0-Pt3" {
                let r = BRect.create(Pt(1., 2.), Pt(5., 7.))
                Expect.isTrue (eqPt r.Pt0 (Pt(1., 2.))) "Pt0 should be MinX, MinY"
                Expect.isTrue (eqPt r.Pt1 (Pt(5., 2.))) "Pt1 should be MaxX, MinY"
                Expect.isTrue (eqPt r.Pt2 (Pt(5., 7.))) "Pt2 should be MaxX, MaxY"
                Expect.isTrue (eqPt r.Pt3 (Pt(1., 7.))) "Pt3 should be MinX, MaxY"
            }

            test "Points array has 4 corners in CCW order" {
                let r = BRect.create(Pt(1., 2.), Pt(5., 7.))
                let pts = r.Points
                Expect.equal pts.Length 4 "Should have 4 points"
                Expect.isTrue (eqPt pts.[0] r.Pt0) "First point should be Pt0"
                Expect.isTrue (eqPt pts.[1] r.Pt1) "Second point should be Pt1"
                Expect.isTrue (eqPt pts.[2] r.Pt2) "Third point should be Pt2"
                Expect.isTrue (eqPt pts.[3] r.Pt3) "Fourth point should be Pt3"
            }

            test "PointsLooped has 5 points with first and last same" {
                let r = BRect.create(Pt(1., 2.), Pt(5., 7.))
                let pts = r.PointsLooped
                Expect.equal pts.Length 5 "Should have 5 points"
                Expect.isTrue (eqPt pts.[0] pts.[4]) "First and last should be same"
                Expect.isTrue (eqPt pts.[0] r.Pt0) "First should be Pt0"
            }

            test "Edges are correct" {
                let r = BRect.create(Pt(1., 2.), Pt(5., 7.))
                Expect.isTrue (eqPt r.Edge01.From r.Pt0) "Edge01 should start at Pt0"
                Expect.isTrue (eqPt r.Edge01.To r.Pt1) "Edge01 should end at Pt1"
                Expect.isTrue (eqPt r.Edge12.From r.Pt1) "Edge12 should start at Pt1"
                Expect.isTrue (eqPt r.Edge12.To r.Pt2) "Edge12 should end at Pt2"
                Expect.isTrue (eqPt r.Edge23.From r.Pt2) "Edge23 should start at Pt2"
                Expect.isTrue (eqPt r.Edge23.To r.Pt3) "Edge23 should end at Pt3"
                Expect.isTrue (eqPt r.Edge30.From r.Pt3) "Edge30 should start at Pt3"
                Expect.isTrue (eqPt r.Edge30.To r.Pt0) "Edge30 should end at Pt0"
            }
        ]

        testList "Equality" [
            test "equals with exact match" {
                let r1 = BRect.create(Pt(1., 2.), Pt(5., 7.))
                let r2 = BRect.create(Pt(1., 2.), Pt(5., 7.))
                Expect.isTrue (BRect.equals 0.0 r1 r2) "Identical rectangles should be equal"
            }

            test "equals with tolerance" {
                let r1 = BRect.create(Pt(1., 2.), Pt(5., 7.))
                let r2 = BRect.create(Pt(1.001, 2.001), Pt(5.001, 7.001))
                Expect.isTrue (BRect.equals 0.01 r1 r2) "Should be equal within tolerance"
            }

            test "notEquals with different rectangles" {
                let r1 = BRect.create(Pt(1., 2.), Pt(5., 7.))
                let r2 = BRect.create(Pt(2., 3.), Pt(6., 8.))
                Expect.isTrue (BRect.notEquals 0.1 r1 r2) "Different rectangles should not be equal"
            }

            test "equals outside tolerance" {
                let r1 = BRect.create(Pt(1., 2.), Pt(5., 7.))
                let r2 = BRect.create(Pt(1.1, 2.), Pt(5., 7.))
                Expect.isFalse (BRect.equals 0.01 r1 r2) "Should not be equal outside tolerance"
            }
        ]

        testList "Static Methods" [
            test "doOverlap static method" {
                let r1 = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let r2 = BRect.create(Pt(5., 5.), Pt(15., 15.))
                Expect.isTrue (BRect.doOverlap r1 r2) "Should overlap"
            }

            test "doOverlapMoreThan with tolerance" {
                let r1 = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let r2 = BRect.create(Pt(9., 0.), Pt(15., 10.))
                Expect.isTrue (BRect.doOverlapMoreThan 0.5 r1 r2) "Should overlap more than 0.5"
                Expect.isFalse (BRect.doOverlapMoreThan 2.0 r1 r2) "Should not overlap more than 2.0"
            }

            test "contains static method" {
                let outer = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let inner = BRect.create(Pt(2., 2.), Pt(8., 8.))
                Expect.isTrue (BRect.contains inner outer) "Inner should be contained in outer"
                Expect.isFalse (BRect.contains outer inner) "Outer should not be contained in inner"
            }

            test "containsPt static method" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                Expect.isTrue (BRect.containsPt (Pt(5., 5.)) r) "Point inside"
                Expect.isFalse (BRect.containsPt (Pt(15., 5.)) r) "Point outside"
            }

            test "union static method" {
                let r1 = BRect.create(Pt(0., 0.), Pt(5., 5.))
                let r2 = BRect.create(Pt(3., 3.), Pt(10., 10.))
                let u = BRect.union r1 r2
                Expect.equal u.MinX 0. "MinX"
                Expect.equal u.MaxX 10. "MaxX"
                Expect.equal u.MaxY 10. "MaxY"
            }

            test "unionPt static method" {
                let r = BRect.create(Pt(0., 0.), Pt(5., 5.))
                let u = BRect.unionPt (Pt(10., 10.)) r
                Expect.equal u.MaxX 10. "MaxX"
                Expect.equal u.MaxY 10. "MaxY"
            }

            test "area static method" {
                let r = BRect.create(Pt(0., 0.), Pt(4., 5.))
                Expect.equal (BRect.area r) 20. "Area should be 20"
            }

            test "expand static method" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let expanded = BRect.expand 2.0 r
                Expect.equal expanded.MinX -2. "MinX"
                Expect.equal expanded.MaxX 12. "MaxX"
            }

            test "expandSafe static method collapses to center" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let shrunk = BRect.expandSafe -20.0 r
                Expect.equal shrunk.MinX 5. "MinX at center"
                Expect.equal shrunk.MaxX 5. "MaxX at center"
            }

            test "expandXaxis static method" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let expanded = BRect.expandXaxis 1.0 2.0 r
                Expect.equal expanded.MinX -1. "MinX"
                Expect.equal expanded.MaxX 12. "MaxX"
            }

            test "expandYaxis static method" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let expanded = BRect.expandYaxis 1.0 2.0 r
                Expect.equal expanded.MinY -1. "MinY"
                Expect.equal expanded.MaxY 12. "MaxY"
            }
        ]

        testList "Edge Cases" [
            test "createFromSeq with empty sequence throws" {
                Expect.throws (fun () -> BRect.createFromSeq Seq.empty |> ignore) "Should throw on empty sequence"
            }

            test "createFromSeq with null throws" {
                Expect.throws (fun () -> BRect.createFromSeq null |> ignore) "Should throw on null"
            }

            test "createFromIList with empty list throws" {
                let pts = ResizeArray<Pt>()
                Expect.throws (fun () -> BRect.createFromIList pts |> ignore) "Should throw on empty list"
            }

            test "createFromIList with null throws" {
                Expect.throws (fun () -> BRect.createFromIList null |> ignore) "Should throw on null"
            }

            test "CountZeroSides for point (2 zero sides)" {
                let r = BRect.create(Pt(5., 5.), Pt(5., 5.))
                Expect.equal r.CountZeroSides 2 "Point should have 2 zero sides"
            }

            test "CountZeroSides for horizontal line (1 zero side)" {
                let r = BRect.createUnchecked(0., 5., 10., 5.)
                Expect.equal r.CountZeroSides 1 "Horizontal line should have 1 zero side"
            }

            test "CountZeroSides for vertical line (1 zero side)" {
                let r = BRect.createUnchecked(5., 0., 5., 10.)
                Expect.equal r.CountZeroSides 1 "Vertical line should have 1 zero side"
            }

            test "CountZeroSides for normal rect (0 zero sides)" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                Expect.equal r.CountZeroSides 0 "Normal rect should have 0 zero sides"
            }

            test "IsTouching at corner" {
                let r1 = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let r2 = BRect.create(Pt(10., 10.), Pt(20., 20.))
                Expect.isTrue (r1.IsTouching(r2, 0.001)) "Rectangles touching at corner"
            }

            test "IsTouching separate rectangles" {
                let r1 = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let r2 = BRect.create(Pt(20., 20.), Pt(30., 30.))
                Expect.isFalse (r1.IsTouching(r2, 0.001)) "Separate rectangles not touching"
            }

            test "OverlapsWith with negative tolerance counts apart as overlap" {
                let r1 = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let r2 = BRect.create(Pt(11., 0.), Pt(20., 10.))
                Expect.isFalse (r1.OverlapsWith(r2, 0.0)) "No overlap at zero tolerance"
                Expect.isTrue (r1.OverlapsWith(r2, -2.0)) "Should count as overlap with negative tolerance"
            }

            test "Expand negative within bounds" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let shrunk = r.Expand(-2.0)
                Expect.equal shrunk.MinX 2. "MinX"
                Expect.equal shrunk.MaxX 8. "MaxX"
                Expect.equal shrunk.MinY 2. "MinY"
                Expect.equal shrunk.MaxY 8. "MaxY"
            }

            test "createFromCenter with zero size creates point" {
                let r = BRect.createFromCenter(Pt(5., 5.), 0., 0.)
                Expect.isTrue r.IsPoint "Should be a point"
                Expect.equal r.MinX 5. "MinX at center"
                Expect.equal r.MaxX 5. "MaxX at center"
            }

            test "Intersection of one rect inside another" {
                let outer = BRect.create(Pt(0., 0.), Pt(20., 20.))
                let inner = BRect.create(Pt(5., 5.), Pt(15., 15.))
                match outer.Intersection inner with
                | ValueSome i ->
                    Expect.isTrue (BRect.equals 0.0 i inner) "Intersection should equal inner rect"
                | ValueNone -> failtest "Should have intersection"
            }

            test "Union of identical rects returns same rect" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 10.))
                let u = r.Union r
                Expect.isTrue (BRect.equals 0.0 u r) "Union of same rect should be identical"
            }
        ]

        testList "String Representations" [
            test "ToString contains size information" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 5.))
                let s = r.ToString()
                Expect.isTrue (s.Contains("BRect")) "Should contain type name"
                Expect.isTrue (s.Contains("sizeX")) "Should contain sizeX"
            }

            test "AsString contains size information" {
                let r = BRect.create(Pt(0., 0.), Pt(10., 5.))
                let s = r.AsString
                Expect.isTrue (s.Contains("sizeX")) "Should contain sizeX"
                Expect.isFalse (s.Contains("BRect")) "Should not contain full type name"
            }

            test "AsFSharpCode produces valid code string" {
                let r = BRect.createUnchecked(1., 2., 3., 4.)
                let code = r.AsFSharpCode
                Expect.isTrue (code.Contains("BRect.createUnchecked")) "Should contain constructor"
                Expect.isTrue (code.Contains("1")) "Should contain coordinates"
            }
        ]
    ]
