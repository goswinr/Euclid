module TestBBox

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let inline eqPnt a b = Pnt.distance a b < 1e-9
let inline eqVec a b = Vec.length (a - b) < 1e-9
let inline eqFloat a b = abs(a - b) < 1e-9

let tests =
    testList "BBox" [

        testList "Constructor and Basic Properties" [
            test "create from two points" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 5., 3.))
                Expect.equal box.MinX 0. "MinX should be 0"
                Expect.equal box.MinY 0. "MinY should be 0"
                Expect.equal box.MinZ 0. "MinZ should be 0"
                Expect.equal box.MaxX 10. "MaxX should be 10"
                Expect.equal box.MaxY 5. "MaxY should be 5"
                Expect.equal box.MaxZ 3. "MaxZ should be 3"
            }

            test "create from two points with swapped coordinates" {
                let box = BBox.create(Pnt(10., 5., 3.), Pnt(0., 0., 0.))
                Expect.equal box.MinX 0. "MinX should be 0 after sorting"
                Expect.equal box.MinY 0. "MinY should be 0 after sorting"
                Expect.equal box.MinZ 0. "MinZ should be 0 after sorting"
                Expect.equal box.MaxX 10. "MaxX should be 10 after sorting"
                Expect.equal box.MaxY 5. "MaxY should be 5 after sorting"
                Expect.equal box.MaxZ 3. "MaxZ should be 3 after sorting"
            }

            test "create from two points with mixed ordering" {
                let box = BBox.create(Pnt(10., 0., 3.), Pnt(0., 5., 0.))
                Expect.equal box.MinX 0. "MinX should be 0"
                Expect.equal box.MinY 0. "MinY should be 0"
                Expect.equal box.MinZ 0. "MinZ should be 0"
                Expect.equal box.MaxX 10. "MaxX should be 10"
                Expect.equal box.MaxY 5. "MaxY should be 5"
                Expect.equal box.MaxZ 3. "MaxZ should be 3"
            }

            test "MinPnt and MaxPnt properties" {
                let box = BBox.create(Pnt(1., 2., 3.), Pnt(5., 7., 9.))
                Expect.isTrue (eqPnt box.MinPnt (Pnt(1., 2., 3.))) "MinPnt should be (1, 2, 3)"
                Expect.isTrue (eqPnt box.MaxPnt (Pnt(5., 7., 9.))) "MaxPnt should be (5, 7, 9)"
            }

            test "SizeX, SizeY, SizeZ" {
                let box = BBox.create(Pnt(1., 2., 3.), Pnt(5., 7., 9.))
                Expect.equal box.SizeX 4. "SizeX should be 4"
                Expect.equal box.SizeY 5. "SizeY should be 5"
                Expect.equal box.SizeZ 6. "SizeZ should be 6"
            }

            test "Center" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 6., 4.))
                Expect.isTrue (eqPnt box.Center (Pnt(5., 3., 2.))) "Center should be (5, 3, 2)"
            }

            test "Diagonal" {
                let box = BBox.create(Pnt(1., 2., 3.), Pnt(5., 7., 9.))
                Expect.isTrue (eqVec box.Diagonal (Vec(4., 5., 6.))) "Diagonal should be (4, 5, 6)"
            }

            test "Volume" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 5., 2.))
                Expect.equal box.Volume 100. "Volume should be 100"
            }

            test "Volume static method" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 5., 2.))
                Expect.equal (BBox.volume box) 100. "Volume should be 100"
            }
        ]

        testList "Creation Methods" [
            test "createFromSeq with valid points" {
                let pts = [Pnt(1., 2., 3.); Pnt(5., 3., 1.); Pnt(2., 7., 5.); Pnt(8., 1., 2.)]
                let box = BBox.createFromSeq pts
                Expect.equal box.MinX 1. "MinX should be 1"
                Expect.equal box.MinY 1. "MinY should be 1"
                Expect.equal box.MinZ 1. "MinZ should be 1"
                Expect.equal box.MaxX 8. "MaxX should be 8"
                Expect.equal box.MaxY 7. "MaxY should be 7"
                Expect.equal box.MaxZ 5. "MaxZ should be 5"
            }

            test "createFromSeq with single point" {
                let pts = [Pnt(5., 3., 2.)]
                let box = BBox.createFromSeq pts
                Expect.equal box.MinX 5. "MinX should be 5"
                Expect.equal box.MinY 3. "MinY should be 3"
                Expect.equal box.MinZ 2. "MinZ should be 2"
                Expect.equal box.MaxX 5. "MaxX should be 5"
                Expect.equal box.MaxY 3. "MaxY should be 3"
                Expect.equal box.MaxZ 2. "MaxZ should be 2"
            }

            test "createFromSeq throws on empty sequence" {
                Expect.throws (fun () -> BBox.createFromSeq [] |> ignore) "Should throw on empty sequence"
            }

            test "createFromIList with valid points" {
                let pts = ResizeArray([Pnt(1., 2., 3.); Pnt(5., 3., 1.); Pnt(2., 7., 5.)])
                let box = BBox.createFromIList pts
                Expect.equal box.MinX 1. "MinX should be 1"
                Expect.equal box.MaxY 7. "MaxY should be 7"
                Expect.equal box.MaxZ 5. "MaxZ should be 5"
            }

            test "createFromIList throws on empty list" {
                Expect.throws (fun () -> BBox.createFromIList (ResizeArray()) |> ignore) "Should throw on empty list"
            }

            test "createFromCenter with valid size" {
                let box = BBox.createFromCenter(Pnt(5., 5., 5.), 10., 6., 4.)
                Expect.equal box.MinX 0. "MinX should be 0"
                Expect.equal box.MinY 2. "MinY should be 2"
                Expect.equal box.MinZ 3. "MinZ should be 3"
                Expect.equal box.MaxX 10. "MaxX should be 10"
                Expect.equal box.MaxY 8. "MaxY should be 8"
                Expect.equal box.MaxZ 7. "MaxZ should be 7"
            }

            test "createFromCenter rejects negative sizeX" {
                Expect.throws (fun () -> BBox.createFromCenter(Pnt(5., 5., 5.), -10., 6., 4.) |> ignore) "Should throw on negative sizeX"
            }

            test "createFromCenter rejects negative sizeY" {
                Expect.throws (fun () -> BBox.createFromCenter(Pnt(5., 5., 5.), 10., -6., 4.) |> ignore) "Should throw on negative sizeY"
            }

            test "createFromCenter rejects negative sizeZ" {
                Expect.throws (fun () -> BBox.createFromCenter(Pnt(5., 5., 5.), 10., 6., -4.) |> ignore) "Should throw on negative sizeZ"
            }

            test "createFromLine" {
                let line = Line3D(Pnt(1., 2., 3.), Pnt(5., 7., 9.))
                let box = BBox.createFromLine line
                Expect.equal box.MinX 1. "MinX should be 1"
                Expect.equal box.MinY 2. "MinY should be 2"
                Expect.equal box.MinZ 3. "MinZ should be 3"
                Expect.equal box.MaxX 5. "MaxX should be 5"
                Expect.equal box.MaxY 7. "MaxY should be 7"
                Expect.equal box.MaxZ 9. "MaxZ should be 9"
            }

            test "createFromBRect" {
                let rect = BRect.create(Pt(1., 2.), Pt(5., 7.))
                let box = BBox.createFromBRect 3. 9. rect
                Expect.equal box.MinX 1. "MinX should be 1"
                Expect.equal box.MinY 2. "MinY should be 2"
                Expect.equal box.MinZ 3. "MinZ should be 3"
                Expect.equal box.MaxX 5. "MaxX should be 5"
                Expect.equal box.MaxY 7. "MaxY should be 7"
                Expect.equal box.MaxZ 9. "MaxZ should be 9"
            }

            test "createFromBRect rejects invalid Z bounds" {
                let rect = BRect.create(Pt(1., 2.), Pt(5., 7.))
                Expect.throws (fun () -> BBox.createFromBRect 9. 3. rect |> ignore) "Should throw when minZ > maxZ"
            }
        ]

        testList "Expansion Methods" [
            test "Expand by positive distance" {
                let box = BBox.create(Pnt(2., 3., 4.), Pnt(8., 9., 10.))
                let expanded = box.Expand(1.)
                Expect.equal expanded.MinX 1. "MinX should be 1"
                Expect.equal expanded.MinY 2. "MinY should be 2"
                Expect.equal expanded.MinZ 3. "MinZ should be 3"
                Expect.equal expanded.MaxX 9. "MaxX should be 9"
                Expect.equal expanded.MaxY 10. "MaxY should be 10"
                Expect.equal expanded.MaxZ 11. "MaxZ should be 11"
            }

            test "Expand by negative distance (shrink)" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let shrunk = box.Expand(-2.)
                Expect.equal shrunk.MinX 2. "MinX should be 2"
                Expect.equal shrunk.MaxX 8. "MaxX should be 8"
            }

            test "Expand throws on underflow" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(2., 2., 2.))
                Expect.throws (fun () -> box.Expand(-2.) |> ignore) "Should throw when shrinking causes underflow"
            }

            test "Expand with XYZ distances" {
                let box = BBox.create(Pnt(2., 3., 4.), Pnt(8., 9., 10.))
                let expanded = box.Expand(1., 2., 3.)
                Expect.equal expanded.MinX 1. "MinX should be 1"
                Expect.equal expanded.MinY 1. "MinY should be 1"
                Expect.equal expanded.MinZ 1. "MinZ should be 1"
                Expect.equal expanded.MaxX 9. "MaxX should be 9"
                Expect.equal expanded.MaxY 11. "MaxY should be 11"
                Expect.equal expanded.MaxZ 13. "MaxZ should be 13"
            }

            test "ExpandSafe with positive distance" {
                let box = BBox.create(Pnt(2., 3., 4.), Pnt(8., 9., 10.))
                let expanded = box.ExpandSafe(1.)
                Expect.equal expanded.MinX 1. "MinX should be 1"
                Expect.equal expanded.MaxX 9. "MaxX should be 9"
            }

            test "ExpandSafe with negative distance causing underflow" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(2., 2., 2.))
                let shrunk = box.ExpandSafe(-5.)
                // Should collapse to midpoint
                Expect.equal shrunk.MinX 1. "MinX should be midpoint 1"
                Expect.equal shrunk.MaxX 1. "MaxX should be midpoint 1"
                Expect.equal shrunk.MinY 1. "MinY should be midpoint 1"
                Expect.equal shrunk.MaxY 1. "MaxY should be midpoint 1"
            }

            test "ExpandSafe with mixed underflow" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(2., 10., 4.))
                let shrunk = box.ExpandSafe(-3., -1., -5.)
                // X should collapse, Y and Z should shrink normally
                Expect.equal shrunk.MinX 1. "MinX should collapse to midpoint"
                Expect.equal shrunk.MaxX 1. "MaxX should collapse to midpoint"
                Expect.equal shrunk.MinY 1. "MinY should be 1"
                Expect.equal shrunk.MaxY 9. "MaxY should be 9"
                Expect.equal shrunk.MinZ 2. "MinZ should collapse to midpoint"
                Expect.equal shrunk.MaxZ 2. "MaxZ should collapse to midpoint"
            }

            test "ExpandXaxis with positive distances" {
                let box = BBox.create(Pnt(2., 3., 4.), Pnt(8., 9., 10.))
                let expanded = box.ExpandXaxis(1., 2.)
                Expect.equal expanded.MinX 1. "MinX should be 1"
                Expect.equal expanded.MaxX 10. "MaxX should be 10"
                Expect.equal expanded.MinY 3. "MinY should be unchanged"
                Expect.equal expanded.MaxY 9. "MaxY should be unchanged"
            }

            test "ExpandYaxis with positive distances" {
                let box = BBox.create(Pnt(2., 3., 4.), Pnt(8., 9., 10.))
                let expanded = box.ExpandYaxis(1., 2.)
                Expect.equal expanded.MinY 2. "MinY should be 2"
                Expect.equal expanded.MaxY 11. "MaxY should be 11"
                Expect.equal expanded.MinX 2. "MinX should be unchanged"
                Expect.equal expanded.MaxX 8. "MaxX should be unchanged"
            }

            test "ExpandZaxis with positive distances" {
                let box = BBox.create(Pnt(2., 3., 4.), Pnt(8., 9., 10.))
                let expanded = box.ExpandZaxis(1., 2.)
                Expect.equal expanded.MinZ 3. "MinZ should be 3"
                Expect.equal expanded.MaxZ 12. "MaxZ should be 12"
                Expect.equal expanded.MinX 2. "MinX should be unchanged"
            }

            test "expandRel with factor 1.5" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let expanded = BBox.expandRel 1.5 box
                Expect.isTrue (eqPnt expanded.Center (Pnt(5., 5., 5.))) "Center should remain at (5, 5, 5)"
                Expect.equal expanded.SizeX 15. "SizeX should be 15"
                Expect.equal expanded.SizeY 15. "SizeY should be 15"
                Expect.equal expanded.SizeZ 15. "SizeZ should be 15"
            }

            test "expandRel with factor 0.5 (shrink)" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let shrunk = BBox.expandRel 0.5 box
                Expect.isTrue (eqPnt shrunk.Center (Pnt(5., 5., 5.))) "Center should remain at (5, 5, 5)"
                Expect.equal shrunk.SizeX 5. "SizeX should be 5"
            }

            test "expandRel rejects negative factor" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                Expect.throws (fun () -> BBox.expandRel -0.5 box |> ignore) "Should throw on negative factor"
            }

            test "expandRelXYZ with different factors" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let expanded = BBox.expandRelXYZ 1.5 0.5 2.0 box
                Expect.isTrue (eqPnt expanded.Center (Pnt(5., 5., 5.))) "Center should remain at (5, 5, 5)"
                Expect.equal expanded.SizeX 15. "SizeX should be 15"
                Expect.equal expanded.SizeY 5. "SizeY should be 5"
                Expect.equal expanded.SizeZ 20. "SizeZ should be 20"
            }
        ]

        testList "Transformation Methods" [
            test "move by vector" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 5., 3.))
                let moved = BBox.move (Vec(5., 3., 2.)) box
                Expect.equal moved.MinX 5. "MinX should be 5"
                Expect.equal moved.MinY 3. "MinY should be 3"
                Expect.equal moved.MinZ 2. "MinZ should be 2"
                Expect.equal moved.MaxX 15. "MaxX should be 15"
                Expect.equal moved.MaxY 8. "MaxY should be 8"
                Expect.equal moved.MaxZ 5. "MaxZ should be 5"
            }

            test "translate by vector (alias for move)" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 5., 3.))
                let moved = BBox.translate (Vec(5., 3., 2.)) box
                Expect.equal moved.MinX 5. "MinX should be 5"
                Expect.equal moved.MaxX 15. "MaxX should be 15"
            }

            test "moveX" {
                let box = BBox.create(Pnt(2., 3., 4.), Pnt(8., 9., 10.))
                let moved = BBox.moveX 5. box
                Expect.equal moved.MinX 7. "MinX should be 7"
                Expect.equal moved.MaxX 13. "MaxX should be 13"
                Expect.equal moved.MinY 3. "MinY should be unchanged"
                Expect.equal moved.MinZ 4. "MinZ should be unchanged"
            }

            test "moveY" {
                let box = BBox.create(Pnt(2., 3., 4.), Pnt(8., 9., 10.))
                let moved = BBox.moveY 5. box
                Expect.equal moved.MinY 8. "MinY should be 8"
                Expect.equal moved.MaxY 14. "MaxY should be 14"
                Expect.equal moved.MinX 2. "MinX should be unchanged"
            }

            test "moveZ" {
                let box = BBox.create(Pnt(2., 3., 4.), Pnt(8., 9., 10.))
                let moved = BBox.moveZ 5. box
                Expect.equal moved.MinZ 9. "MinZ should be 9"
                Expect.equal moved.MaxZ 15. "MaxZ should be 15"
                Expect.equal moved.MinX 2. "MinX should be unchanged"
            }

            test "scale from world origin" {
                let box = BBox.create(Pnt(1., 2., 3.), Pnt(5., 7., 9.))
                let scaled = BBox.scale 2. box
                Expect.equal scaled.MinX 2. "MinX should be 2"
                Expect.equal scaled.MinY 4. "MinY should be 4"
                Expect.equal scaled.MinZ 6. "MinZ should be 6"
                Expect.equal scaled.MaxX 10. "MaxX should be 10"
                Expect.equal scaled.MaxY 14. "MaxY should be 14"
                Expect.equal scaled.MaxZ 18. "MaxZ should be 18"
            }
        ]

        testList "Overlap and Intersection" [
            test "OverlapsWith - overlapping boxes" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(3., 3., 3.), Pnt(8., 8., 8.))
                Expect.isTrue (a.OverlapsWith b) "Boxes should overlap"
                Expect.isTrue (b.OverlapsWith a) "Overlap should be symmetric"
            }

            test "OverlapsWith - touching boxes" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(5., 0., 0.), Pnt(10., 5., 5.))
                Expect.isTrue (a.OverlapsWith b) "Touching boxes should overlap"
            }

            test "OverlapsWith - separated boxes" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(6., 6., 6.), Pnt(10., 10., 10.))
                Expect.isFalse (a.OverlapsWith b) "Separated boxes should not overlap"
            }

            test "OverlapsWith - one inside the other" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let b = BBox.create(Pnt(2., 2., 2.), Pnt(8., 8., 8.))
                Expect.isTrue (a.OverlapsWith b) "Inner box should overlap"
                Expect.isTrue (b.OverlapsWith a) "Overlap should be symmetric"
            }

            test "OverlapsWith with tolerance - barely overlapping" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(5.5, 0., 0.), Pnt(10., 5., 5.))
                Expect.isFalse (a.OverlapsWith(b, 0.1)) "Should not overlap with small tolerance"
                Expect.isTrue (a.OverlapsWith(b, -0.6)) "Should overlap with negative tolerance (expands boxes)"
            }

            test "doOverlap static method" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(3., 3., 3.), Pnt(8., 8., 8.))
                Expect.isTrue (BBox.doOverlap a b) "Static method should work"
            }

            test "Intersection - overlapping boxes" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(3., 3., 3.), Pnt(8., 8., 8.))
                match a.Intersection b with
                | ValueSome intersect ->
                    Expect.equal intersect.MinX 3. "Intersection MinX should be 3"
                    Expect.equal intersect.MinY 3. "Intersection MinY should be 3"
                    Expect.equal intersect.MinZ 3. "Intersection MinZ should be 3"
                    Expect.equal intersect.MaxX 5. "Intersection MaxX should be 5"
                    Expect.equal intersect.MaxY 5. "Intersection MaxY should be 5"
                    Expect.equal intersect.MaxZ 5. "Intersection MaxZ should be 5"
                | ValueNone -> failwith "Should have intersection"
            }

            test "Intersection - touching boxes" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(5., 0., 0.), Pnt(10., 5., 5.))
                match a.Intersection b with
                | ValueSome intersect ->
                    Expect.isTrue intersect.IsFlat "Intersection should be flat"
                | ValueNone -> failwith "Should have intersection (touching)"
            }

            test "Intersection - separated boxes" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(6., 6., 6.), Pnt(10., 10., 10.))
                match a.Intersection b with
                | ValueSome _ -> failwith "Should not have intersection"
                | ValueNone -> ()
            }

            test "intersection static method" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(3., 3., 3.), Pnt(8., 8., 8.))
                match BBox.intersection a b with
                | ValueSome intersect ->
                    Expect.equal intersect.MinX 3. "Intersection MinX should be 3"
                | ValueNone -> failwith "Should have intersection"
            }

            test "IsTouching - boxes touching on face" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(5., 0., 0.), Pnt(10., 5., 5.))
                Expect.isTrue (a.IsTouching(b, 0.01)) "Boxes should be touching"
            }

            test "IsTouching - boxes overlapping (not just touching)" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(3., 3., 3.), Pnt(8., 8., 8.))
                Expect.isFalse (a.IsTouching(b, 0.01)) "Overlapping boxes are not just touching"
            }

            test "IsTouching - boxes separated" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(6., 6., 6.), Pnt(10., 10., 10.))
                Expect.isFalse (a.IsTouching(b, 0.01)) "Separated boxes are not touching"
            }

            test "IsTouching - touching on Z face (top-bottom)" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(0., 0., 5.), Pnt(5., 5., 10.))
                Expect.isTrue (a.IsTouching(b, 1e-6)) "Boxes touching on Z face should be touching"
                Expect.isTrue (b.IsTouching(a, 1e-6)) "IsTouching should be symmetric"
            }

            test "IsTouching - touching on Y face (front-back)" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(0., 5., 0.), Pnt(5., 10., 5.))
                Expect.isTrue (a.IsTouching(b, 1e-6)) "Boxes touching on Y face should be touching"
                Expect.isTrue (b.IsTouching(a, 1e-6)) "IsTouching should be symmetric"
            }

            test "IsTouching - edge touching (X-edge)" {
                // Boxes touch along an X-aligned edge (sharing Y=5 and Z=5 edge)
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(0., 5., 5.), Pnt(5., 10., 10.))
                Expect.isTrue (a.IsTouching(b, 1e-6)) "Boxes touching along an edge should be touching"
            }

            test "IsTouching - edge touching (Y-edge)" {
                // Boxes touch along a Y-aligned edge (sharing X=5 and Z=5 edge)
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(5., 0., 5.), Pnt(10., 5., 10.))
                Expect.isTrue (a.IsTouching(b, 1e-6)) "Boxes touching along Y-edge should be touching"
            }

            test "IsTouching - edge touching (Z-edge)" {
                // Boxes touch along a Z-aligned edge (sharing X=5 and Y=5 edge)
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(5., 5., 0.), Pnt(10., 10., 5.))
                Expect.isTrue (a.IsTouching(b, 1e-6)) "Boxes touching along Z-edge should be touching"
            }

            test "IsTouching - corner touching (single point)" {
                // Boxes touch at exactly one corner point (5,5,5)
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(5., 5., 5.), Pnt(10., 10., 10.))
                Expect.isTrue (a.IsTouching(b, 1e-6)) "Boxes touching at a corner should be touching"
                Expect.isTrue (b.IsTouching(a, 1e-6)) "Corner touching should be symmetric"
            }

            test "IsTouching - within tolerance (barely touching)" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(5.0000005, 0., 0.), Pnt(10., 5., 5.))  // gap of 0.0000005
                Expect.isTrue (a.IsTouching(b, 1e-6)) "Boxes within tolerance should be touching"
            }

            test "IsTouching - just outside tolerance (not touching)" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(5.00001, 0., 0.), Pnt(10., 5., 5.))  // gap of 0.00001 > 1e-6
                Expect.isFalse (a.IsTouching(b, 1e-6)) "Boxes outside tolerance should not be touching"
            }

            test "IsTouching - identical boxes (overlapping, not touching)" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                Expect.isFalse (a.IsTouching(b, 1e-6)) "Identical boxes overlap, they don't just touch"
            }

            test "IsTouching - one box inside another (overlapping, not touching)" {
                let outer = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let inner = BBox.create(Pnt(2., 2., 2.), Pnt(8., 8., 8.))
                Expect.isFalse (outer.IsTouching(inner, 1e-6)) "Contained box overlaps, doesn't touch"
                Expect.isFalse (inner.IsTouching(outer, 1e-6)) "IsTouching should be symmetric"
            }

            test "IsTouching - partial overlap (not just touching)" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(4., 4., 4.), Pnt(9., 9., 9.))
                Expect.isFalse (a.IsTouching(b, 1e-6)) "Partially overlapping boxes are not just touching"
            }

            test "IsTouching - separated on all axes" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(10., 10., 10.), Pnt(15., 15., 15.))
                Expect.isFalse (a.IsTouching(b, 1e-6)) "Completely separated boxes are not touching"
            }

            test "IsTouching - separated on single axis only" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(0., 0., 6.), Pnt(5., 5., 10.))  // gap on Z only
                Expect.isFalse (a.IsTouching(b, 1e-6)) "Boxes separated on Z axis are not touching"
            }

            test "IsTouching - large tolerance" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(5.5, 0., 0.), Pnt(10., 5., 5.))  // gap of 0.5
                Expect.isTrue (a.IsTouching(b, 1.0)) "Boxes within large tolerance should be touching"
            }

            test "IsTouching - zero-size box touching face" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(5., 2., 2.), Pnt(5., 3., 3.))  // degenerate flat box on face
                Expect.isTrue (a.IsTouching(b, 1e-6)) "Flat box on face should be touching"
            }

            test "IsTouching - default tolerance" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(5., 0., 0.), Pnt(10., 5., 5.))
                Expect.isTrue (a.IsTouching(b)) "Should work with default tolerance"
            }
        ]

        testList "Union" [
            test "Union of two boxes" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(3., 3., 3.), Pnt(8., 8., 8.))
                let union = a.Union b
                Expect.equal union.MinX 0. "Union MinX should be 0"
                Expect.equal union.MinY 0. "Union MinY should be 0"
                Expect.equal union.MinZ 0. "Union MinZ should be 0"
                Expect.equal union.MaxX 8. "Union MaxX should be 8"
                Expect.equal union.MaxY 8. "Union MaxY should be 8"
                Expect.equal union.MaxZ 8. "Union MaxZ should be 8"
            }

            test "Union static method" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(3., 3., 3.), Pnt(8., 8., 8.))
                let union = BBox.union a b
                Expect.equal union.MaxX 8. "Union MaxX should be 8"
            }

            test "Union with point" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let pt = Pnt(8., 3., 2.)
                let union = box.Union pt
                Expect.equal union.MinX 0. "Union MinX should be 0"
                Expect.equal union.MaxX 8. "Union MaxX should be 8"
                Expect.equal union.MinY 0. "Union MinY should be 0"
                Expect.equal union.MaxY 5. "Union MaxY should be 5"
            }

            test "unionPt static method" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let pt = Pnt(8., 3., 2.)
                let union = BBox.unionPt pt box
                Expect.equal union.MaxX 8. "Union MaxX should be 8"
            }
        ]

        testList "Containment" [
            test "Contains point inside" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let pt = Pnt(5., 5., 5.)
                Expect.isTrue (box.Contains pt) "Box should contain point inside"
            }

            test "Contains point on boundary" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let pt = Pnt(10., 5., 5.)
                Expect.isTrue (box.Contains pt) "Box should contain point on boundary"
            }

            test "Contains point outside" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let pt = Pnt(11., 5., 5.)
                Expect.isFalse (box.Contains pt) "Box should not contain point outside"
            }

            test "Contains box inside" {
                let outer = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let inner = BBox.create(Pnt(2., 2., 2.), Pnt(8., 8., 8.))
                Expect.isTrue (outer.Contains inner) "Outer box should contain inner box"
            }

            test "Contains box outside" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(5., 5., 5.))
                let b = BBox.create(Pnt(6., 6., 6.), Pnt(10., 10., 10.))
                Expect.isFalse (a.Contains b) "Box should not contain separated box"
            }

            test "containsPnt static method" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let pt = Pnt(5., 5., 5.)
                Expect.isTrue (BBox.containsPnt pt box) "Static method should work"
            }

            test "contains static method for box" {
                let outer = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let inner = BBox.create(Pnt(2., 2., 2.), Pnt(8., 8., 8.))
                Expect.isTrue (BBox.contains inner outer) "Static method should work"
            }
        ]

        testList "Points and Edges" [
            test "Points array has 8 points" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let pts = box.Points
                Expect.equal pts.Length 8 "Should have 8 points"
            }

            test "Pt0 is MinPnt" {
                let box = BBox.create(Pnt(1., 2., 3.), Pnt(5., 7., 9.))
                Expect.isTrue (eqPnt box.Pt0 box.MinPnt) "Pt0 should equal MinPnt"
            }

            test "Pt6 is MaxPnt" {
                let box = BBox.create(Pnt(1., 2., 3.), Pnt(5., 7., 9.))
                Expect.isTrue (eqPnt box.Pt6 box.MaxPnt) "Pt6 should equal MaxPnt"
            }

            test "Bottom points are counter-clockwise" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let bottom = box.BottomPoints
                Expect.equal bottom.Length 4 "Should have 4 bottom points"
                Expect.isTrue (eqPnt bottom.[0] (Pnt(0., 0., 0.))) "First point should be Pt0"
                Expect.isTrue (eqPnt bottom.[1] (Pnt(10., 0., 0.))) "Second point should be Pt1"
                Expect.isTrue (eqPnt bottom.[2] (Pnt(10., 10., 0.))) "Third point should be Pt2"
                Expect.isTrue (eqPnt bottom.[3] (Pnt(0., 10., 0.))) "Fourth point should be Pt3"
            }

            test "Bottom points looped has 5 points" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let bottom = box.BottomPointsLooped
                Expect.equal bottom.Length 5 "Should have 5 points"
                Expect.isTrue (eqPnt bottom.[0] bottom.[4]) "First and last should be same"
            }

            test "Top points are counter-clockwise" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let top = box.TopPoints
                Expect.equal top.Length 4 "Should have 4 top points"
            }

            test "Top points looped has 5 points" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let top = box.TopPointsLooped
                Expect.equal top.Length 5 "Should have 5 points"
                Expect.isTrue (eqPnt top.[0] top.[4]) "First and last should be same"
            }

            test "Edges array has 12 edges" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let edges = box.Edges
                Expect.equal edges.Length 12 "Should have 12 edges"
            }

            test "Edge01 is X-aligned" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 5., 3.))
                let edge = box.Edge01
                Expect.isTrue (eqPnt edge.From (Pnt(0., 0., 0.))) "From should be Pt0"
                Expect.isTrue (eqPnt edge.To (Pnt(10., 0., 0.))) "To should be Pt1"
            }

            test "Edge37 is Z-aligned" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 5., 3.))
                let edge = box.Edge37
                Expect.isTrue (eqPnt edge.From box.Pt3) "From should be Pt3"
                Expect.isTrue (eqPnt edge.To box.Pt7) "To should be Pt7"
            }
        ]

        testList "Validation Methods" [
            test "IsZero for tiny box" {
                let box = BBox.create(Pnt(5., 5., 5.), Pnt(5. + 1e-13, 5. + 1e-13, 5. + 1e-13))
                Expect.isTrue box.IsZero "Tiny box should be zero"
            }

            test "IsPoint is same as IsZero" {
                let box = BBox.create(Pnt(5., 5., 5.), Pnt(5. + 1e-13, 5. + 1e-13, 5. + 1e-13))
                Expect.equal box.IsPoint box.IsZero "IsPoint should equal IsZero"
            }

            test "IsLine for box with one dimension" {
                let box = BBox.create(Pnt(0., 5., 5.), Pnt(10., 5. + 1e-13, 5. + 1e-13))
                Expect.isTrue box.IsLine "Box with one dimension should be a line"
            }

            test "IsFlat for box with two dimensions" {
                let box = BBox.create(Pnt(0., 0., 5.), Pnt(10., 10., 5. + 1e-13))
                Expect.isTrue box.IsFlat "Box with two dimensions should be flat"
            }

            test "IsValid for normal box" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                Expect.isTrue box.IsValid "Normal box should be valid"
            }

            test "HasVolume for normal box" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                Expect.isTrue box.HasVolume "Normal box should have volume"
            }

            test "HasVolume is same as IsValid" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                Expect.equal box.HasVolume box.IsValid "HasVolume should equal IsValid"
            }

            test "CountZeroSides for normal box" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                Expect.equal box.CountZeroSides 0 "Normal box should have 0 zero sides"
            }

            test "CountZeroSides for flat box" {
                let box = BBox.create(Pnt(0., 0., 5.), Pnt(10., 10., 5.))
                Expect.equal box.CountZeroSides 1 "Flat box should have 1 zero side"
            }

            test "CountZeroSides for line box" {
                let box = BBox.create(Pnt(0., 5., 5.), Pnt(10., 5., 5.))
                Expect.equal box.CountZeroSides 2 "Line box should have 2 zero sides"
            }

            test "CountZeroSides for point box" {
                let box = BBox.create(Pnt(5., 5., 5.), Pnt(5., 5., 5.))
                Expect.equal box.CountZeroSides 3 "Point box should have 3 zero sides"
            }
        ]

        testList "Size Methods" [
            test "LongestEdge" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 5., 3.))
                Expect.equal box.LongestEdge 10. "Longest edge should be 10"
            }

            test "ShortestEdge" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 5., 3.))
                Expect.equal box.ShortestEdge 3. "Shortest edge should be 3"
            }
        ]

        testList "Evaluation" [
            test "EvaluateAt origin (0,0,0)" {
                let box = BBox.create(Pnt(1., 2., 3.), Pnt(11., 12., 13.))
                let pt = box.EvaluateAt(0., 0., 0.)
                Expect.isTrue (eqPnt pt box.MinPnt) "Should return MinPnt at (0,0,0)"
            }

            test "EvaluateAt far corner (1,1,1)" {
                let box = BBox.create(Pnt(1., 2., 3.), Pnt(11., 12., 13.))
                let pt = box.EvaluateAt(1., 1., 1.)
                Expect.isTrue (eqPnt pt box.MaxPnt) "Should return MaxPnt at (1,1,1)"
            }

            test "EvaluateAt center (0.5,0.5,0.5)" {
                let box = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let pt = box.EvaluateAt(0.5, 0.5, 0.5)
                Expect.isTrue (eqPnt pt (Pnt(5., 5., 5.))) "Should return center at (0.5,0.5,0.5)"
            }
        ]

        testList "Conversion Methods" [
            test "asBRect" {
                let box = BBox.create(Pnt(1., 2., 3.), Pnt(5., 7., 9.))
                let rect = box.asBRect
                Expect.equal rect.MinX 1. "BRect MinX should be 1"
                Expect.equal rect.MinY 2. "BRect MinY should be 2"
                Expect.equal rect.MaxX 5. "BRect MaxX should be 5"
                Expect.equal rect.MaxY 7. "BRect MaxY should be 7"
            }

            test "toBRect static method" {
                let box = BBox.create(Pnt(1., 2., 3.), Pnt(5., 7., 9.))
                let rect = BBox.toBRect box
                Expect.equal rect.MinX 1. "BRect MinX should be 1"
            }
        ]

        testList "Equality Methods" [
            test "equals with exact match" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let b = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                Expect.isTrue (BBox.equals 0.0 a b) "Exact boxes should be equal"
            }

            test "equals with tolerance" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let b = BBox.create(Pnt(0.001, 0.001, 0.001), Pnt(10.001, 10.001, 10.001))
                Expect.isTrue (BBox.equals 0.01 a b) "Boxes should be equal within tolerance"
                Expect.isFalse (BBox.equals 0.0001 a b) "Boxes should not be equal with small tolerance"
            }

            test "notEquals" {
                let a = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
                let b = BBox.create(Pnt(1., 1., 1.), Pnt(10., 10., 10.))
                Expect.isTrue (BBox.notEquals 0.5 a b) "Different boxes should not be equal"
            }
        ]
    ]
