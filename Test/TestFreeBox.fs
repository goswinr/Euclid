module TestFreeBox

open Euclid

open EuclidTestSupport

let inline eqPnt a b = Pnt.distanceTo a b < 1e-9
let inline eqFloat a b = abs(a - b) < 1e-9

/// The 8 corners of a rotated box that is off the world origin.
/// The base rectangle starts at (5, 3, 2), with the X-axis (8, 6, 0) (length 10)
/// and the Y-axis (-3, 4, 0) (length 5), extruded by the Z-axis (0, 0, 7).
let rotatedPts = [|
    Pnt( 5.,  3.,  2.) // 0
    Pnt(13.,  9.,  2.) // 1
    Pnt(10., 13.,  2.) // 2
    Pnt( 2.,  7.,  2.) // 3
    Pnt( 5.,  3.,  9.) // 4
    Pnt(13.,  9.,  9.) // 5
    Pnt(10., 13.,  9.) // 6
    Pnt( 2.,  7.,  9.) // 7
    |]

/// The 24 interleaved x, y, and z coordinates of the corners in 'rotatedPts'.
let rotatedXYZs = [|
    5.;  3.;  2.;
    13.; 9.;  2.;
    10.; 13.; 2.;
    2.;  7.;  2.;
    5.;  3.;  9.;
    13.; 9.;  9.;
    10.; 13.; 9.;
    2.;  7.;  9.
    |]

let tests =
    testList "FreeBox" [

        testList "Constructor and Basic Properties" [
            test "createFromEightPoints with 8 points" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 5., 0.); Pnt(0., 5., 0.)
                    Pnt(0., 0., 3.); Pnt(10., 0., 3.); Pnt(10., 5., 3.); Pnt(0., 5., 3.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                Expect.equal box.XYZs.Length 24 "Should have 24 floats"
                Expect.equal box.AsPoints.Length 8 "Should have 8 points"
                Expect.isTrue (eqPnt box.Pnt0 pts.[0]) "Pnt0 should match input"
                Expect.isTrue (eqPnt box.Pnt6 pts.[6]) "Pnt6 should match input"
            }

            test "createFromEightPoints rejects wrong number of points" {
                let pts = [| Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 5., 0.) |]
                Expect.throws (fun () -> FreeBox.createFrom8PntArray pts |> ignore) "Should throw with wrong number of points"
            }

        ]

        testList "Flat coordinate array" [
            test "XYZs holds the 24 interleaved coordinates" {
                let box = FreeBox.createFrom8PntArray rotatedPts
                Expect.equal box.XYZs.Length 24 "Should have 24 floats"
                for i = 0 to 23 do
                    Expect.isTrue (eqFloat box.XYZs.[i] rotatedXYZs.[i]) $"Coordinate {i} should match"
            }

            test "createDirectly uses the array without copying it" {
                let xyzs = Array.copy rotatedXYZs
                let box = FreeBox.createFromXYZs xyzs
                Expect.isTrue (eqPnt box.Pnt0 rotatedPts.[0]) "Pnt0 should match"
                Expect.isTrue (eqPnt box.Pnt6 rotatedPts.[6]) "Pnt6 should match"
                xyzs.[0] <- 99.
                Expect.isTrue (eqFloat box.Pnt0.X 99.) "The array is the live internal buffer"
            }

            test "createDirectly rejects a wrong number of floats" {
                Expect.throws (fun () -> FreeBox.createFromXYZs [| 1.; 2.; 3. |] |> ignore) "Should throw with 3 floats"
                Expect.throws (fun () -> FreeBox.createFromXYZs (Array.zeroCreate 25) |> ignore) "Should throw with 25 floats"
            }

            test "XYZs is the live internal buffer" {
                let box = FreeBox.createFrom8PntArray rotatedPts
                box.XYZs.[21] <- -4.5 // the x of Pnt7
                Expect.isTrue (eqFloat box.Pnt7.X -4.5) "Writing to XYZs should change Pnt7"
            }

            test "AsPoints is a copy, not the live buffer" {
                let box = FreeBox.createFrom8PntArray rotatedPts
                let pts = box.AsPoints
                pts.[0] <- Pnt(99., 99., 99.)
                Expect.isTrue (eqPnt box.Pnt0 rotatedPts.[0]) "Pnt0 should be unchanged"
            }

            test "GetX, GetY and GetZ read single coordinates" {
                let box = FreeBox.createFrom8PntArray rotatedPts
                for i = 0 to 7 do
                    Expect.isTrue (eqFloat (box.GetX i) rotatedPts.[i].X) $"GetX {i} should match"
                    Expect.isTrue (eqFloat (box.GetY i) rotatedPts.[i].Y) $"GetY {i} should match"
                    Expect.isTrue (eqFloat (box.GetZ i) rotatedPts.[i].Z) $"GetZ {i} should match"
            }

            test "GetX, GetY and GetZ throw on an invalid index" {
                let box = FreeBox.createFrom8PntArray rotatedPts
                Expect.throws (fun () -> box.GetX 8 |> ignore) "GetX should throw for index 8"
                Expect.throws (fun () -> box.GetY -1 |> ignore) "GetY should throw for a negative index"
                Expect.throws (fun () -> box.GetZ 8 |> ignore) "GetZ should throw for index 8"
            }

            test "Pnt0X to Pnt7Z getters match the corner points" {
                let box = FreeBox.createFrom8PntArray rotatedPts
                Expect.isTrue (eqFloat box.Pnt0X  5.) "Pnt0X"
                Expect.isTrue (eqFloat box.Pnt0Y  3.) "Pnt0Y"
                Expect.isTrue (eqFloat box.Pnt0Z  2.) "Pnt0Z"
                Expect.isTrue (eqFloat box.Pnt1X 13.) "Pnt1X"
                Expect.isTrue (eqFloat box.Pnt2Y 13.) "Pnt2Y"
                Expect.isTrue (eqFloat box.Pnt3X  2.) "Pnt3X"
                Expect.isTrue (eqFloat box.Pnt4Z  9.) "Pnt4Z"
                Expect.isTrue (eqFloat box.Pnt5Y  9.) "Pnt5Y"
                Expect.isTrue (eqFloat box.Pnt6X 10.) "Pnt6X"
                Expect.isTrue (eqFloat box.Pnt7X  2.) "Pnt7X"
                Expect.isTrue (eqFloat box.Pnt7Y  7.) "Pnt7Y"
                Expect.isTrue (eqFloat box.Pnt7Z  9.) "Pnt7Z"
            }

            test "Pnt0X to Pnt7Z read the same values as the corner points" {
                let box = FreeBox.createFrom8PntArray rotatedPts
                let xs = [| box.Pnt0X; box.Pnt1X; box.Pnt2X; box.Pnt3X; box.Pnt4X; box.Pnt5X; box.Pnt6X; box.Pnt7X |]
                let ys = [| box.Pnt0Y; box.Pnt1Y; box.Pnt2Y; box.Pnt3Y; box.Pnt4Y; box.Pnt5Y; box.Pnt6Y; box.Pnt7Y |]
                let zs = [| box.Pnt0Z; box.Pnt1Z; box.Pnt2Z; box.Pnt3Z; box.Pnt4Z; box.Pnt5Z; box.Pnt6Z; box.Pnt7Z |]
                for i = 0 to 7 do
                    Expect.isTrue (eqFloat xs.[i] rotatedPts.[i].X) $"Pt{i}X should match"
                    Expect.isTrue (eqFloat ys.[i] rotatedPts.[i].Y) $"Pt{i}Y should match"
                    Expect.isTrue (eqFloat zs.[i] rotatedPts.[i].Z) $"Pt{i}Z should match"
            }

            test "Pnt0X to Pnt7Z setters write into the flat array" {
                let box = FreeBox.createFrom8PntArray rotatedPts
                box.Pnt0X <- -1.5
                box.Pnt0Y <- -2.5
                box.Pnt0Z <- -3.5
                box.Pnt7X <- 11.5
                box.Pnt7Y <- 12.5
                box.Pnt7Z <- 13.5
                Expect.isTrue (eqPnt box.Pnt0 (Pnt(-1.5, -2.5, -3.5))) "Pnt0 should hold the new coordinates"
                Expect.isTrue (eqPnt box.Pnt7 (Pnt(11.5, 12.5, 13.5))) "Pnt7 should hold the new coordinates"
                Expect.isTrue (eqFloat box.XYZs.[ 0] -1.5) "XYZs.[0] should be Pnt0X"
                Expect.isTrue (eqFloat box.XYZs.[ 1] -2.5) "XYZs.[1] should be Pnt0Y"
                Expect.isTrue (eqFloat box.XYZs.[ 2] -3.5) "XYZs.[2] should be Pnt0Z"
                Expect.isTrue (eqFloat box.XYZs.[21] 11.5) "XYZs.[21] should be Pnt7X"
                Expect.isTrue (eqFloat box.XYZs.[22] 12.5) "XYZs.[22] should be Pnt7Y"
                Expect.isTrue (eqFloat box.XYZs.[23] 13.5) "XYZs.[23] should be Pnt7Z"
                // the other corners must be untouched
                Expect.isTrue (eqPnt box.Pnt3 rotatedPts.[3]) "Pnt3 should be unchanged"
                Expect.isTrue (eqPnt box.Pnt4 rotatedPts.[4]) "Pnt4 should be unchanged"
            }

            test "static pnt0X to pnt7Z accessors" {
                let box = FreeBox.createFrom8PntArray rotatedPts
                Expect.isTrue (eqFloat (FreeBox.pnt0X box) rotatedPts.[0].X) "pnt0X"
                Expect.isTrue (eqFloat (FreeBox.pnt0Y box) rotatedPts.[0].Y) "pnt0Y"
                Expect.isTrue (eqFloat (FreeBox.pnt0Z box) rotatedPts.[0].Z) "pnt0Z"
                Expect.isTrue (eqFloat (FreeBox.pnt7X box) rotatedPts.[7].X) "pnt7X"
                Expect.isTrue (eqFloat (FreeBox.pnt7Y box) rotatedPts.[7].Y) "pnt7Y"
                Expect.isTrue (eqFloat (FreeBox.pnt7Z box) rotatedPts.[7].Z) "pnt7Z"
            }

            test "Pt setters write into the flat array" {
                let box = FreeBox.createFrom8PntArray rotatedPts
                box.Pnt3 <- Pnt(-1.5, 2.5, -3.5)
                Expect.isTrue (eqFloat box.XYZs.[ 9] -1.5) "XYZs.[9] should be the new x"
                Expect.isTrue (eqFloat box.XYZs.[10]  2.5) "XYZs.[10] should be the new y"
                Expect.isTrue (eqFloat box.XYZs.[11] -3.5) "XYZs.[11] should be the new z"
                Expect.isTrue (eqPnt box.Pnt3 (Pnt(-1.5, 2.5, -3.5))) "Pnt3 should be the new point"
            }

            test "SetPoint and SetXYZ write into the flat array" {
                let box = FreeBox.createFrom8PntArray rotatedPts
                box.SetPoint 5 (Pnt(1., 2., 3.))
                Expect.isTrue (eqPnt box.Pnt5 (Pnt(1., 2., 3.))) "SetPoint should set Pnt5"
                box.SetXYZ 6 4. 5. 6.
                Expect.isTrue (eqPnt box.Pnt6 (Pnt(4., 5., 6.))) "SetXYZ should set Pnt6"
                Expect.throws (fun () -> box.SetXYZ 8 0. 0. 0.) "SetXYZ should throw for index 8"
            }

            test "Duplicate has its own array" {
                let box = FreeBox.createFrom8PntArray rotatedPts
                let copy = box.Duplicate()
                copy.Pnt0 <- Pnt(0., 0., 0.)
                Expect.isTrue (eqPnt box.Pnt0 rotatedPts.[0]) "The original should be unchanged"
                Expect.isTrue (eqPnt copy.Pnt1 rotatedPts.[1]) "The copy should hold the other points"
            }

            test "Transformations mutate the same 24 float array" {
                let box = FreeBox.createFrom8PntArray rotatedPts
                let xyzs = box.XYZs
                let q = Quaternion.createFromDegreesUnitVec UnitVec.xAxis 30.
                let movedPt0 = rotatedPts.[0] + Vec(1., 2., 3.)
                box.Move(Vec(1., 2., 3.))
                Expect.isTrue (obj.ReferenceEquals(xyzs, box.XYZs)) "Move should keep the same array"
                Expect.isTrue (eqPnt box.Pnt0 movedPt0) "Move should change the box in place"
                box.RotateAround movedPt0 q
                Expect.isTrue (obj.ReferenceEquals(xyzs, box.XYZs)) "RotateAround should keep the same array"
                Expect.equal box.XYZs.Length 24 "RotateAround should keep 24 floats"
                Expect.isTrue (eqPnt box.Pnt0 movedPt0) "The rotation center should stay fixed"
                Expect.isFalse (eqPnt box.Pnt6 (rotatedPts.[6] + Vec(1., 2., 3.))) "Pnt6 should be rotated"
            }

            test "Transforming a Duplicate leaves the original unchanged" {
                let box = FreeBox.createFrom8PntArray rotatedPts
                let copy = box.Duplicate()
                copy.RotateAround (Pnt(7., -4., 3.)) (Quaternion.createFromDegreesUnitVec UnitVec.zAxis 35.)
                copy.Move(Vec(-2., 5., 1.))
                for i = 0 to 7 do
                    Expect.isTrue (eqPnt (box.GetPoint i) rotatedPts.[i]) $"Pt{i} of the original should be unchanged"
                Expect.isFalse (eqPnt copy.Pnt0 rotatedPts.[0]) "The copy should be transformed"
            }
        ]

        testList "Creation from Box" [
            test "createFromBox" {
                let box = Box.createFromAxesUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let freeBox = FreeBox.createFromBox box
                Expect.isTrue (eqPnt freeBox.Pnt0 box.Pnt0) "Pnt0 should match"
                Expect.isTrue (eqPnt freeBox.Pnt6 box.Pnt6) "Pnt6 should match"

            }
        ]

        testList "Creation from 2D Points" [
            test "createFromFour2DPoints with valid points" {
                let pts = [| Pt(0., 0.); Pt(10., 0.); Pt(10., 5.); Pt(0., 5.) |]
                let box = FreeBox.createFrom4PtArray 2. 8. pts
                Expect.isTrue (eqPnt box.Pnt0 (Pnt(0., 0., 2.))) "Pnt0 should be at zMin"
                Expect.isTrue (eqPnt box.Pnt4 (Pnt(0., 0., 8.))) "Pnt4 should be at zMax"

            }

            test "createFromFour2DPoints rejects wrong number of points" {
                let pts = [| Pt(0., 0.); Pt(10., 0.); Pt(10., 5.) |]
                Expect.throws (fun () -> FreeBox.createFrom4PtArray 2. 8. pts |> ignore) "Should throw with wrong number of points"
            }

            test "createFrom4Pts" {
                let a = Pt(0., 0.)
                let b = Pt(10., 0.)
                let c = Pt(10., 5.)
                let d = Pt(0., 5.)
                let box = FreeBox.createFrom4Pts(2., 8., a, b, c, d)
                Expect.isTrue (eqPnt box.Pnt0 (Pnt(0., 0., 2.))) "Pnt0 should be at zMin"
                Expect.isTrue (eqPnt box.Pnt4 (Pnt(0., 0., 8.))) "Pnt4 should be at zMax"
            }
        ]

        testList "Point Access" [
            test "GetPoint with valid index" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 5., 0.); Pnt(0., 5., 0.)
                    Pnt(0., 0., 3.); Pnt(10., 0., 3.); Pnt(10., 5., 3.); Pnt(0., 5., 3.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                Expect.isTrue (eqPnt (box.GetPoint 0) pts.[0]) "GetPoint 0 should return Pnt0"
                Expect.isTrue (eqPnt (box.GetPoint 7) pts.[7]) "GetPoint 7 should return Pnt7"
            }

            test "GetPoint with invalid index throws" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 5., 0.); Pnt(0., 5., 0.)
                    Pnt(0., 0., 3.); Pnt(10., 0., 3.); Pnt(10., 5., 3.); Pnt(0., 5., 3.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                Expect.throws (fun () -> box.GetPoint 8 |> ignore) "Should throw for index 8"
                Expect.throws (fun () -> box.GetPoint -1 |> ignore) "Should throw for negative index"
            }

            test "All Pt properties work" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 5., 0.); Pnt(0., 5., 0.)
                    Pnt(0., 0., 3.); Pnt(10., 0., 3.); Pnt(10., 5., 3.); Pnt(0., 5., 3.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                Expect.isTrue (eqPnt box.Pnt0 pts.[0]) "Pnt0 should match"
                Expect.isTrue (eqPnt box.Pnt1 pts.[1]) "Pnt1 should match"
                Expect.isTrue (eqPnt box.Pnt2 pts.[2]) "Pnt2 should match"
                Expect.isTrue (eqPnt box.Pnt3 pts.[3]) "Pnt3 should match"
                Expect.isTrue (eqPnt box.Pnt4 pts.[4]) "Pnt4 should match"
                Expect.isTrue (eqPnt box.Pnt5 pts.[5]) "Pnt5 should match"
                Expect.isTrue (eqPnt box.Pnt6 pts.[6]) "Pnt6 should match"
                Expect.isTrue (eqPnt box.Pnt7 pts.[7]) "Pnt7 should match"
            }

            test "MovePoint moves only the corner at the index" {
                let box = FreeBox.createFrom8PntArray rotatedPts
                let v = Vec(0.3, -1.7, 2.9)
                box.MovePoint 6 v
                Expect.isTrue (eqPnt box.Pnt6 (rotatedPts.[6] + v)) "corner 6 moved"
                for i in [0; 1; 2; 3; 4; 5; 7] do
                    let p = box.GetPoint i
                    Expect.isTrue (p.X = rotatedPts.[i].X && p.Y = rotatedPts.[i].Y && p.Z = rotatedPts.[i].Z) $"corner {i} unchanged"
                Expect.isTrue (eqPnt box.Edge56.To box.Pnt6) "Edge56 ends at the moved corner"
                Expect.isTrue (eqPnt box.Edge26.To box.Pnt6) "Edge26 ends at the moved corner"
            }

            test "MoveXYZ gives the same coordinates as MovePoint" {
                let a = FreeBox.createFrom8PntArray rotatedPts
                let b = FreeBox.createFrom8PntArray rotatedPts
                a.MovePoint 0 (Vec(0.3, -1.7, 2.9))
                b.MoveXYZ 0 0.3 (-1.7) 2.9
                Expect.isTrue (Array.forall2 (=) a.XYZs b.XYZs) "same coordinates"
            }

            test "static movePoint and moveXYZ take the point index first" {
                let box = FreeBox.createFrom8PntArray rotatedPts
                FreeBox.movePoint 7 (Vec(0.5, -2.0, 1.25)) box
                Expect.isTrue (eqPnt box.Pnt7 (Pnt(2.5, 5., 10.25))) "movePoint moved corner 7"
                FreeBox.moveXYZ 1 0.5 (-2.0) 1.25 box
                Expect.isTrue (eqPnt box.Pnt1 (Pnt(13.5, 7., 3.25))) "moveXYZ moved corner 1"
            }

            test "MovePoint by a zero vector leaves the box unchanged" {
                let box = FreeBox.createFrom8PntArray rotatedPts
                box.MovePoint 3 (Vec(0., 0., 0.))
                box.MoveXYZ 4 0. 0. 0.
                Expect.isTrue (Array.forall2 (=) box.XYZs rotatedXYZs) "same coordinates"
            }

            test "MovePoint with invalid index throws" {
                let box = FreeBox.createFrom8PntArray rotatedPts
                for i in [ -1; 8 ] do
                    Expect.throws (fun () -> box.MovePoint i (Vec(1., 1., 1.))) $"MovePoint {i}"
                    Expect.throws (fun () -> box.MoveXYZ i 1. 1. 1.) $"MoveXYZ {i}"
                Expect.isTrue (Array.forall2 (=) box.XYZs rotatedXYZs) "box unchanged"
            }
        ]

        testList "Edges" [
            test "Reverse edges are the reverse of the forward edges" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 5., 0.); Pnt(0., 5., 0.)
                    Pnt(0., 0., 3.); Pnt(10., 0., 3.); Pnt(10., 5., 3.); Pnt(0., 5., 3.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                let isReverseOf (fwd:Line3D) (rev:Line3D) = eqPnt fwd.From rev.To && eqPnt fwd.To rev.From
                Expect.isTrue (isReverseOf box.Edge01 box.Edge10) "Edge10 should be the reverse of Edge01"
                Expect.isTrue (isReverseOf box.Edge12 box.Edge21) "Edge21 should be the reverse of Edge12"
                Expect.isTrue (isReverseOf box.Edge32 box.Edge23) "Edge23 should be the reverse of Edge32"
                Expect.isTrue (isReverseOf box.Edge03 box.Edge30) "Edge30 should be the reverse of Edge03"
                Expect.isTrue (isReverseOf box.Edge04 box.Edge40) "Edge40 should be the reverse of Edge04"
                Expect.isTrue (isReverseOf box.Edge15 box.Edge51) "Edge51 should be the reverse of Edge15"
                Expect.isTrue (isReverseOf box.Edge26 box.Edge62) "Edge62 should be the reverse of Edge26"
                Expect.isTrue (isReverseOf box.Edge37 box.Edge73) "Edge73 should be the reverse of Edge37"
                Expect.isTrue (isReverseOf box.Edge45 box.Edge54) "Edge54 should be the reverse of Edge45"
                Expect.isTrue (isReverseOf box.Edge56 box.Edge65) "Edge65 should be the reverse of Edge56"
                Expect.isTrue (isReverseOf box.Edge76 box.Edge67) "Edge67 should be the reverse of Edge76"
                Expect.isTrue (isReverseOf box.Edge47 box.Edge74) "Edge74 should be the reverse of Edge47"
            }

            test "GetLine returns the line between any two different corners" {
                let box = FreeBox.createFrom8PntArray rotatedPts
                let pts = box.AsPoints
                for i in 0..7 do
                    for j in 0..7 do
                        if i <> j then
                            let ln = box.GetLine i j
                            Expect.isTrue (eqPnt ln.From pts.[i]) $"GetLine({i}, {j}).From should be point {i}"
                            Expect.isTrue (eqPnt ln.To pts.[j]) $"GetLine({i}, {j}).To should be point {j}"
                            Expect.isTrue (eqPnt (FreeBox.getLine i j box).To pts.[j]) $"FreeBox.getLine {i} {j} should match GetLine"
            }

            test "GetLine matches the named edges" {
                let box = FreeBox.createFrom8PntArray rotatedPts
                let same (a:Line3D) (b:Line3D) = eqPnt a.From b.From && eqPnt a.To b.To
                Expect.isTrue (same (box.GetLine 0 1) box.Edge01) "GetLine(0, 1) should be Edge01"
                Expect.isTrue (same (box.GetLine 1 0) box.Edge10) "GetLine(1, 0) should be Edge10"
                Expect.isTrue (same (box.GetLine 3 2) box.Edge32) "GetLine(3, 2) should be Edge32"
                Expect.isTrue (same (box.GetLine 2 6) box.Edge26) "GetLine(2, 6) should be Edge26"
                Expect.isTrue (same (box.GetLine 7 4) box.Edge74) "GetLine(7, 4) should be Edge74"
            }

            test "GetLine fails on equal or out of range indices" {
                let box = FreeBox.createFrom8PntArray rotatedPts
                Expect.throws (fun () -> box.GetLine 3 3 |> ignore) "Equal indices should throw"
                Expect.throws (fun () -> box.GetLine (-1) 2 |> ignore) "Negative start index should throw"
                Expect.throws (fun () -> box.GetLine 2 8 |> ignore) "End index 8 should throw"
                Expect.throws (fun () -> box.GetLine 8 8 |> ignore) "Equal out of range indices should throw"
            }
        ]

        testList "Transformation Methods" [
            test "Scale from world origin" {
                let pts = [|
                    Pnt(1., 2., 3.); Pnt(11., 2., 3.); Pnt(11., 7., 3.); Pnt(1., 7., 3.)
                    Pnt(1., 2., 6.); Pnt(11., 2., 6.); Pnt(11., 7., 6.); Pnt(1., 7., 6.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                box.Scale 2.
                Expect.isTrue (eqPnt box.Pnt0 (Pnt(2., 4., 6.))) "Pnt0 should be scaled"
                Expect.isTrue (eqPnt box.Pnt6 (Pnt(22., 14., 12.))) "Pnt6 should be scaled"
            }

            test "ScaleOn center point" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                let center = Pnt(5., 5., 5.)
                box.ScaleOn center 2.
                Expect.isTrue (eqPnt box.Pnt0 (Pnt(-5., -5., -5.))) "Pnt0 should be scaled around center"
                Expect.isTrue (eqPnt box.Pnt6 (Pnt(15., 15., 15.))) "Pnt6 should be scaled around center"
            }

            test "Scale with factor 0.5 shrinks box" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                box.Scale 0.5
                Expect.isTrue (eqPnt box.Pnt0 (Pnt(0., 0., 0.))) "Pnt0 should remain at origin"
                Expect.isTrue (eqPnt box.Pnt6 (Pnt(5., 5., 5.))) "Pnt6 should be halved"
            }

            test "Move instance method" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                box.Move(Vec(5., 3., 2.))
                Expect.isTrue (eqPnt box.Pnt0 (Pnt(5., 3., 2.))) "Pnt0 should be moved"
                Expect.isTrue (eqPnt box.Pnt6 (Pnt(15., 13., 12.))) "Pnt6 should be moved"
            }

            test "MoveX instance method" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                box.MoveX(5.)
                Expect.isTrue (eqPnt box.Pnt0 (Pnt(5., 0., 0.))) "Pnt0 should be moved in X"
            }

            test "MoveY instance method" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                box.MoveY(3.)
                Expect.isTrue (eqPnt box.Pnt0 (Pnt(0., 3., 0.))) "Pnt0 should be moved in Y"
            }

            test "MoveZ instance method" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                box.MoveZ(2.)
                Expect.isTrue (eqPnt box.Pnt0 (Pnt(0., 0., 2.))) "Pnt0 should be moved in Z"
            }

            test "move static method" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                let moved = FreeBox.move (Vec(5., 3., 2.)) box
                Expect.isTrue (eqPnt moved.Pnt0 (Pnt(5., 3., 2.))) "Pnt0 should be moved"
                Expect.isTrue (eqPnt box.Pnt0 pts.[0]) "input box should be unchanged"
                Expect.isFalse (obj.ReferenceEquals(moved.XYZs, box.XYZs)) "result should have its own buffer"
            }

            test "translate static method" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                let moved = FreeBox.move (Vec(5., 3., 2.)) box
                Expect.isTrue (eqPnt moved.Pnt0 (Pnt(5., 3., 2.))) "Pnt0 should be translated"
                Expect.isTrue (eqPnt box.Pnt0 pts.[0]) "input box should be unchanged"
            }

            test "moveX, moveY, moveZ static methods" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                let movedX = FreeBox.moveX 5. box
                Expect.isTrue (eqPnt movedX.Pnt0 (Pnt(5., 0., 0.))) "moveX should work"
                let movedXY = FreeBox.moveY 3. movedX
                Expect.isTrue (eqPnt movedXY.Pnt0 (Pnt(5., 3., 0.))) "moveY should work"
                let movedXYZ = FreeBox.moveZ 2. movedXY
                Expect.isTrue (eqPnt movedXYZ.Pnt0 (Pnt(5., 3., 2.))) "moveZ should work"
                Expect.isTrue (eqPnt movedXYZ.Pnt6 (Pnt(15., 13., 12.))) "all corners should be moved"
                Expect.isTrue (eqPnt box.Pnt0 pts.[0]) "input box should be unchanged"
                Expect.isTrue (eqPnt movedX.Pnt0 (Pnt(5., 0., 0.))) "intermediate box should be unchanged by moveY"
            }

            test "Transform with identity matrix" {
                let pts = [|
                    Pnt(1., 2., 3.); Pnt(11., 2., 3.); Pnt(11., 7., 3.); Pnt(1., 7., 3.)
                    Pnt(1., 2., 6.); Pnt(11., 2., 6.); Pnt(11., 7., 6.); Pnt(1., 7., 6.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                box.Transform(Matrix.identity)
                Expect.isTrue (eqPnt box.Pnt0 pts.[0]) "Pnt0 should be unchanged with identity"
            }

            test "Transform with translation matrix" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                let m = Matrix.createTranslation(Vec(5., 3., 2.))
                box.Transform(m)
                Expect.isTrue (eqPnt box.Pnt0 (Pnt(5., 3., 2.))) "Pnt0 should be translated"
            }

            test "transform static method" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                let m = Matrix.createTranslation(Vec(5., 3., 2.))
                let transformed = FreeBox.transform m box
                Expect.isTrue (eqPnt transformed.Pnt0 (Pnt(5., 3., 2.))) "Pnt0 should be translated"
                Expect.isTrue (eqPnt box.Pnt0 pts.[0]) "input box should be unchanged"
            }

            test "TransformRigid instance method" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                let m = RigidMatrix.createTranslation(Vec(5., 3., 2.))
                box.TransformRigid(m)
                Expect.isTrue (eqPnt box.Pnt0 (Pnt(5., 3., 2.))) "Pnt0 should be translated"
            }

            test "transformRigid static method" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                let m = RigidMatrix.createTranslation(Vec(5., 3., 2.))
                let transformed = FreeBox.transformRigid m box
                Expect.isTrue (eqPnt transformed.Pnt0 (Pnt(5., 3., 2.))) "Pnt0 should be translated"
                Expect.isTrue (eqPnt box.Pnt0 pts.[0]) "input box should be unchanged"
            }

            test "Rotate with identity quaternion" {
                let pts = [|
                    Pnt(1., 2., 3.); Pnt(11., 2., 3.); Pnt(11., 7., 3.); Pnt(1., 7., 3.)
                    Pnt(1., 2., 6.); Pnt(11., 2., 6.); Pnt(11., 7., 6.); Pnt(1., 7., 6.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                box.Rotate(Quaternion.identity)
                Expect.isTrue (eqPnt box.Pnt0 pts.[0]) "Pnt0 should be unchanged with identity quaternion"
            }

            test "Rotate 90 degrees around Z axis" {
                let pts = [|
                    Pnt(1., 0., 0.); Pnt(2., 0., 0.); Pnt(2., 1., 0.); Pnt(1., 1., 0.)
                    Pnt(1., 0., 1.); Pnt(2., 0., 1.); Pnt(2., 1., 1.); Pnt(1., 1., 1.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                let q = Quaternion.createFromDegreesUnitVec UnitVec.zAxis 90.
                box.Rotate(q)
                Expect.isTrue (eqPnt box.Pnt0 (Pnt(0., 1., 0.))) "Pnt0 should be rotated 90 degrees"
            }

            test "rotate static method" {
                let pts = [|
                    Pnt(1., 0., 0.); Pnt(2., 0., 0.); Pnt(2., 1., 0.); Pnt(1., 1., 0.)
                    Pnt(1., 0., 1.); Pnt(2., 0., 1.); Pnt(2., 1., 1.); Pnt(1., 1., 1.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                let q = Quaternion.createFromDegreesUnitVec UnitVec.zAxis 90.
                let rotated = FreeBox.rotate q box
                Expect.isTrue (eqPnt rotated.Pnt0 (Pnt(0., 1., 0.))) "Pnt0 should be rotated 90 degrees"
                Expect.isTrue (eqPnt box.Pnt0 pts.[0]) "input box should be unchanged"
            }

            test "RotateAround keeps center point fixed" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(2., 0., 0.); Pnt(2., 2., 0.); Pnt(0., 2., 0.)
                    Pnt(0., 0., 2.); Pnt(2., 0., 2.); Pnt(2., 2., 2.); Pnt(0., 2., 2.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                let center = Pnt(1., 1., 1.)
                let q = Quaternion.createFromDegreesUnitVec UnitVec.zAxis 90.
                box.RotateAround center q
                // After rotation, center should still be at (1,1,1)
                // Pnt0 at (0,0,0) rotated 90 degrees around (1,1,1) in Z should go to (2,0,0)
                Expect.isTrue (eqPnt box.Pnt0 (Pnt(2., 0., 0.))) "Pnt0 should be rotated around center"
            }

            test "rotateAround static method" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(2., 0., 0.); Pnt(2., 2., 0.); Pnt(0., 2., 0.)
                    Pnt(0., 0., 2.); Pnt(2., 0., 2.); Pnt(2., 2., 2.); Pnt(0., 2., 2.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                let center = Pnt(1., 1., 1.)
                let q = Quaternion.createFromDegreesUnitVec UnitVec.zAxis 90.
                let rotated = FreeBox.rotateAround center q box
                Expect.isTrue (eqPnt rotated.Pnt0 (Pnt(2., 0., 0.))) "Pnt0 should be rotated around center"
                Expect.isTrue (eqPnt box.Pnt0 pts.[0]) "input box should be unchanged"
            }

            test "scale static method" {
                let pts = [|
                    Pnt(1., 2., 3.); Pnt(11., 2., 3.); Pnt(11., 7., 3.); Pnt(1., 7., 3.)
                    Pnt(1., 2., 6.); Pnt(11., 2., 6.); Pnt(11., 7., 6.); Pnt(1., 7., 6.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                let scaled = FreeBox.scale 2. box
                Expect.isTrue (eqPnt scaled.Pnt0 (Pnt(2., 4., 6.))) "Pnt0 should be scaled"
                Expect.isTrue (eqPnt box.Pnt0 pts.[0]) "input box should be unchanged"
            }

            test "scaleOn static method matches ScaleOn instance method" {
                let pts = [|
                    Pnt(1., 2., 3.); Pnt(11., 2., 3.); Pnt(11., 7., 3.); Pnt(1., 7., 3.)
                    Pnt(1., 2., 6.); Pnt(11., 2., 6.); Pnt(11., 7., 6.); Pnt(1., 7., 6.)
                |]
                let box = FreeBox.createFrom8PntArray pts
                let cen = Pnt(4., -2., 5.)
                let scaled = FreeBox.scaleOn cen 1.5 box
                Expect.isTrue (eqPnt box.Pnt6 pts.[6]) "input box should be unchanged"
                box.ScaleOn cen 1.5
                for i = 0 to 7 do
                    Expect.isTrue (eqPnt (scaled.GetPoint i) (box.GetPoint i)) $"point {i} of static and instance scaleOn should match"
            }
        ]

    ]
