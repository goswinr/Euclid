module TestFreeBox

open Euclid

#if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
open Fable.Mocha
#else
open Expecto
#endif

let inline eqPnt a b = Pnt.dist a b < 1e-9
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
                let box = FreeBox.createFromEightPoints pts
                Expect.equal box.XYZs.Length 24 "Should have 24 floats"
                Expect.equal box.AsPoints.Length 8 "Should have 8 points"
                Expect.isTrue (eqPnt box.Pt0 pts.[0]) "Pt0 should match input"
                Expect.isTrue (eqPnt box.Pt6 pts.[6]) "Pt6 should match input"
            }

            test "createFromEightPoints rejects wrong number of points" {
                let pts = [| Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 5., 0.) |]
                Expect.throws (fun () -> FreeBox.createFromEightPoints pts |> ignore) "Should throw with wrong number of points"
            }

        ]

        testList "Flat coordinate array" [
            test "XYZs holds the 24 interleaved coordinates" {
                let box = FreeBox.createFromEightPoints rotatedPts
                Expect.equal box.XYZs.Length 24 "Should have 24 floats"
                for i = 0 to 23 do
                    Expect.isTrue (eqFloat box.XYZs.[i] rotatedXYZs.[i]) $"Coordinate {i} should match"
            }

            test "createDirectly uses the array without copying it" {
                let xyzs = Array.copy rotatedXYZs
                let box = FreeBox.createDirectly xyzs
                Expect.isTrue (eqPnt box.Pt0 rotatedPts.[0]) "Pt0 should match"
                Expect.isTrue (eqPnt box.Pt6 rotatedPts.[6]) "Pt6 should match"
                xyzs.[0] <- 99.
                Expect.isTrue (eqFloat box.Pt0.X 99.) "The array is the live internal buffer"
            }

            test "createDirectly rejects a wrong number of floats" {
                Expect.throws (fun () -> FreeBox.createDirectly [| 1.; 2.; 3. |] |> ignore) "Should throw with 3 floats"
                Expect.throws (fun () -> FreeBox.createDirectly (Array.zeroCreate 25) |> ignore) "Should throw with 25 floats"
            }

            test "XYZs is the live internal buffer" {
                let box = FreeBox.createFromEightPoints rotatedPts
                box.XYZs.[21] <- -4.5 // the x of Pt7
                Expect.isTrue (eqFloat box.Pt7.X -4.5) "Writing to XYZs should change Pt7"
            }

            test "AsPoints is a copy, not the live buffer" {
                let box = FreeBox.createFromEightPoints rotatedPts
                let pts = box.AsPoints
                pts.[0] <- Pnt(99., 99., 99.)
                Expect.isTrue (eqPnt box.Pt0 rotatedPts.[0]) "Pt0 should be unchanged"
            }

            test "GetX, GetY and GetZ read single coordinates" {
                let box = FreeBox.createFromEightPoints rotatedPts
                for i = 0 to 7 do
                    Expect.isTrue (eqFloat (box.GetX i) rotatedPts.[i].X) $"GetX {i} should match"
                    Expect.isTrue (eqFloat (box.GetY i) rotatedPts.[i].Y) $"GetY {i} should match"
                    Expect.isTrue (eqFloat (box.GetZ i) rotatedPts.[i].Z) $"GetZ {i} should match"
            }

            test "GetX, GetY and GetZ throw on an invalid index" {
                let box = FreeBox.createFromEightPoints rotatedPts
                Expect.throws (fun () -> box.GetX 8 |> ignore) "GetX should throw for index 8"
                Expect.throws (fun () -> box.GetY -1 |> ignore) "GetY should throw for a negative index"
                Expect.throws (fun () -> box.GetZ 8 |> ignore) "GetZ should throw for index 8"
            }

            test "Pt0X to Pt7Z getters match the corner points" {
                let box = FreeBox.createFromEightPoints rotatedPts
                Expect.isTrue (eqFloat box.Pt0X  5.) "Pt0X"
                Expect.isTrue (eqFloat box.Pt0Y  3.) "Pt0Y"
                Expect.isTrue (eqFloat box.Pt0Z  2.) "Pt0Z"
                Expect.isTrue (eqFloat box.Pt1X 13.) "Pt1X"
                Expect.isTrue (eqFloat box.Pt2Y 13.) "Pt2Y"
                Expect.isTrue (eqFloat box.Pt3X  2.) "Pt3X"
                Expect.isTrue (eqFloat box.Pt4Z  9.) "Pt4Z"
                Expect.isTrue (eqFloat box.Pt5Y  9.) "Pt5Y"
                Expect.isTrue (eqFloat box.Pt6X 10.) "Pt6X"
                Expect.isTrue (eqFloat box.Pt7X  2.) "Pt7X"
                Expect.isTrue (eqFloat box.Pt7Y  7.) "Pt7Y"
                Expect.isTrue (eqFloat box.Pt7Z  9.) "Pt7Z"
            }

            test "Pt0X to Pt7Z read the same values as the corner points" {
                let box = FreeBox.createFromEightPoints rotatedPts
                let xs = [| box.Pt0X; box.Pt1X; box.Pt2X; box.Pt3X; box.Pt4X; box.Pt5X; box.Pt6X; box.Pt7X |]
                let ys = [| box.Pt0Y; box.Pt1Y; box.Pt2Y; box.Pt3Y; box.Pt4Y; box.Pt5Y; box.Pt6Y; box.Pt7Y |]
                let zs = [| box.Pt0Z; box.Pt1Z; box.Pt2Z; box.Pt3Z; box.Pt4Z; box.Pt5Z; box.Pt6Z; box.Pt7Z |]
                for i = 0 to 7 do
                    Expect.isTrue (eqFloat xs.[i] rotatedPts.[i].X) $"Pt{i}X should match"
                    Expect.isTrue (eqFloat ys.[i] rotatedPts.[i].Y) $"Pt{i}Y should match"
                    Expect.isTrue (eqFloat zs.[i] rotatedPts.[i].Z) $"Pt{i}Z should match"
            }

            test "Pt0X to Pt7Z setters write into the flat array" {
                let box = FreeBox.createFromEightPoints rotatedPts
                box.Pt0X <- -1.5
                box.Pt0Y <- -2.5
                box.Pt0Z <- -3.5
                box.Pt7X <- 11.5
                box.Pt7Y <- 12.5
                box.Pt7Z <- 13.5
                Expect.isTrue (eqPnt box.Pt0 (Pnt(-1.5, -2.5, -3.5))) "Pt0 should hold the new coordinates"
                Expect.isTrue (eqPnt box.Pt7 (Pnt(11.5, 12.5, 13.5))) "Pt7 should hold the new coordinates"
                Expect.isTrue (eqFloat box.XYZs.[ 0] -1.5) "XYZs.[0] should be Pt0X"
                Expect.isTrue (eqFloat box.XYZs.[ 1] -2.5) "XYZs.[1] should be Pt0Y"
                Expect.isTrue (eqFloat box.XYZs.[ 2] -3.5) "XYZs.[2] should be Pt0Z"
                Expect.isTrue (eqFloat box.XYZs.[21] 11.5) "XYZs.[21] should be Pt7X"
                Expect.isTrue (eqFloat box.XYZs.[22] 12.5) "XYZs.[22] should be Pt7Y"
                Expect.isTrue (eqFloat box.XYZs.[23] 13.5) "XYZs.[23] should be Pt7Z"
                // the other corners must be untouched
                Expect.isTrue (eqPnt box.Pt3 rotatedPts.[3]) "Pt3 should be unchanged"
                Expect.isTrue (eqPnt box.Pt4 rotatedPts.[4]) "Pt4 should be unchanged"
            }

            test "static pt0X to pt7Z accessors" {
                let box = FreeBox.createFromEightPoints rotatedPts
                Expect.isTrue (eqFloat (FreeBox.pt0X box) rotatedPts.[0].X) "pt0X"
                Expect.isTrue (eqFloat (FreeBox.pt0Y box) rotatedPts.[0].Y) "pt0Y"
                Expect.isTrue (eqFloat (FreeBox.pt0Z box) rotatedPts.[0].Z) "pt0Z"
                Expect.isTrue (eqFloat (FreeBox.pt7X box) rotatedPts.[7].X) "pt7X"
                Expect.isTrue (eqFloat (FreeBox.pt7Y box) rotatedPts.[7].Y) "pt7Y"
                Expect.isTrue (eqFloat (FreeBox.pt7Z box) rotatedPts.[7].Z) "pt7Z"
            }

            test "Pt setters write into the flat array" {
                let box = FreeBox.createFromEightPoints rotatedPts
                box.Pt3 <- Pnt(-1.5, 2.5, -3.5)
                Expect.isTrue (eqFloat box.XYZs.[ 9] -1.5) "XYZs.[9] should be the new x"
                Expect.isTrue (eqFloat box.XYZs.[10]  2.5) "XYZs.[10] should be the new y"
                Expect.isTrue (eqFloat box.XYZs.[11] -3.5) "XYZs.[11] should be the new z"
                Expect.isTrue (eqPnt box.Pt3 (Pnt(-1.5, 2.5, -3.5))) "Pt3 should be the new point"
            }

            test "SetPt and SetPtXYZ write into the flat array" {
                let box = FreeBox.createFromEightPoints rotatedPts
                box.SetPt 5 (Pnt(1., 2., 3.))
                Expect.isTrue (eqPnt box.Pt5 (Pnt(1., 2., 3.))) "SetPt should set Pt5"
                box.SetPtXYZ(6, 4., 5., 6.)
                Expect.isTrue (eqPnt box.Pt6 (Pnt(4., 5., 6.))) "SetPtXYZ should set Pt6"
                Expect.throws (fun () -> box.SetPtXYZ(8, 0., 0., 0.)) "SetPtXYZ should throw for index 8"
            }

            test "Duplicate has its own array" {
                let box = FreeBox.createFromEightPoints rotatedPts
                let copy = box.Duplicate()
                copy.Pt0 <- Pnt(0., 0., 0.)
                Expect.isTrue (eqPnt box.Pt0 rotatedPts.[0]) "The original should be unchanged"
                Expect.isTrue (eqPnt copy.Pt1 rotatedPts.[1]) "The copy should hold the other points"
            }

            test "Transformations keep the 24 float array" {
                let box = FreeBox.createFromEightPoints rotatedPts
                let q = Quaternion.createFromDegrees(UnitVec.Xaxis, 30.)
                let moved = box.Move(Vec(1., 2., 3.))
                let rotated = box.RotateWithCenter(Pnt(5., 3., 2.), q)
                Expect.equal moved.XYZs.Length 24 "Move should keep 24 floats"
                Expect.equal rotated.XYZs.Length 24 "RotateWithCenter should keep 24 floats"
                Expect.isTrue (eqPnt rotated.Pt0 rotatedPts.[0]) "The rotation center should stay fixed"
            }
        ]

        testList "Creation from Box" [
            test "createFromBox" {
                let box = Box.createUncheckedVec(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let freeBox = FreeBox.createFromBox box
                Expect.isTrue (eqPnt freeBox.Pt0 box.Pt0) "Pt0 should match"
                Expect.isTrue (eqPnt freeBox.Pt6 box.Pt6) "Pt6 should match"

            }
        ]

        testList "Creation from 2D Points" [
            test "createFromFour2DPoints with valid points" {
                let pts = [| Pt(0., 0.); Pt(10., 0.); Pt(10., 5.); Pt(0., 5.) |]
                let box = FreeBox.createFromFour2DPoints 2. 8. pts
                Expect.isTrue (eqPnt box.Pt0 (Pnt(0., 0., 2.))) "Pt0 should be at zMin"
                Expect.isTrue (eqPnt box.Pt4 (Pnt(0., 0., 8.))) "Pt4 should be at zMax"

            }

            test "createFromFour2DPoints rejects wrong number of points" {
                let pts = [| Pt(0., 0.); Pt(10., 0.); Pt(10., 5.) |]
                Expect.throws (fun () -> FreeBox.createFromFour2DPoints 2. 8. pts |> ignore) "Should throw with wrong number of points"
            }

            test "createFromFour2DPointsArgs" {
                let a = Pt(0., 0.)
                let b = Pt(10., 0.)
                let c = Pt(10., 5.)
                let d = Pt(0., 5.)
                let box = FreeBox.createFromFour2DPointsArgs(a, b, c, d, 2., 8.)
                Expect.isTrue (eqPnt box.Pt0 (Pnt(0., 0., 2.))) "Pt0 should be at zMin"
                Expect.isTrue (eqPnt box.Pt4 (Pnt(0., 0., 8.))) "Pt4 should be at zMax"
            }
        ]

        testList "Point Access" [
            test "GetPt with valid index" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 5., 0.); Pnt(0., 5., 0.)
                    Pnt(0., 0., 3.); Pnt(10., 0., 3.); Pnt(10., 5., 3.); Pnt(0., 5., 3.)
                |]
                let box = FreeBox.createFromEightPoints pts
                Expect.isTrue (eqPnt (box.GetPt 0) pts.[0]) "GetPt 0 should return Pt0"
                Expect.isTrue (eqPnt (box.GetPt 7) pts.[7]) "GetPt 7 should return Pt7"
            }

            test "GetPt with invalid index throws" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 5., 0.); Pnt(0., 5., 0.)
                    Pnt(0., 0., 3.); Pnt(10., 0., 3.); Pnt(10., 5., 3.); Pnt(0., 5., 3.)
                |]
                let box = FreeBox.createFromEightPoints pts
                Expect.throws (fun () -> box.GetPt 8 |> ignore) "Should throw for index 8"
                Expect.throws (fun () -> box.GetPt -1 |> ignore) "Should throw for negative index"
            }

            test "All Pt properties work" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 5., 0.); Pnt(0., 5., 0.)
                    Pnt(0., 0., 3.); Pnt(10., 0., 3.); Pnt(10., 5., 3.); Pnt(0., 5., 3.)
                |]
                let box = FreeBox.createFromEightPoints pts
                Expect.isTrue (eqPnt box.Pt0 pts.[0]) "Pt0 should match"
                Expect.isTrue (eqPnt box.Pt1 pts.[1]) "Pt1 should match"
                Expect.isTrue (eqPnt box.Pt2 pts.[2]) "Pt2 should match"
                Expect.isTrue (eqPnt box.Pt3 pts.[3]) "Pt3 should match"
                Expect.isTrue (eqPnt box.Pt4 pts.[4]) "Pt4 should match"
                Expect.isTrue (eqPnt box.Pt5 pts.[5]) "Pt5 should match"
                Expect.isTrue (eqPnt box.Pt6 pts.[6]) "Pt6 should match"
                Expect.isTrue (eqPnt box.Pt7 pts.[7]) "Pt7 should match"
            }
        ]

        testList "Edges" [
            test "Reverse edges are the reverse of the forward edges" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 5., 0.); Pnt(0., 5., 0.)
                    Pnt(0., 0., 3.); Pnt(10., 0., 3.); Pnt(10., 5., 3.); Pnt(0., 5., 3.)
                |]
                let box = FreeBox.createFromEightPoints pts
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
        ]

        testList "Transformation Methods" [
            test "Scale from world origin" {
                let pts = [|
                    Pnt(1., 2., 3.); Pnt(11., 2., 3.); Pnt(11., 7., 3.); Pnt(1., 7., 3.)
                    Pnt(1., 2., 6.); Pnt(11., 2., 6.); Pnt(11., 7., 6.); Pnt(1., 7., 6.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let scaled = box.Scale 2.
                Expect.isTrue (eqPnt scaled.Pt0 (Pnt(2., 4., 6.))) "Pt0 should be scaled"
                Expect.isTrue (eqPnt scaled.Pt6 (Pnt(22., 14., 12.))) "Pt6 should be scaled"
            }

            test "ScaleOn center point" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let center = Pnt(5., 5., 5.)
                let scaled = box.ScaleOn (center, 2.)
                Expect.isTrue (eqPnt scaled.Pt0 (Pnt(-5., -5., -5.))) "Pt0 should be scaled around center"
                Expect.isTrue (eqPnt scaled.Pt6 (Pnt(15., 15., 15.))) "Pt6 should be scaled around center"
            }

            test "Scale with factor 0.5 shrinks box" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let scaled = box.Scale 0.5
                Expect.isTrue (eqPnt scaled.Pt0 (Pnt(0., 0., 0.))) "Pt0 should remain at origin"
                Expect.isTrue (eqPnt scaled.Pt6 (Pnt(5., 5., 5.))) "Pt6 should be halved"
            }

            test "Move instance method" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let moved = box.Move(Vec(5., 3., 2.))
                Expect.isTrue (eqPnt moved.Pt0 (Pnt(5., 3., 2.))) "Pt0 should be moved"
                Expect.isTrue (eqPnt moved.Pt6 (Pnt(15., 13., 12.))) "Pt6 should be moved"
            }

            test "MoveX instance method" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let moved = box.MoveX(5.)
                Expect.isTrue (eqPnt moved.Pt0 (Pnt(5., 0., 0.))) "Pt0 should be moved in X"
            }

            test "MoveY instance method" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let moved = box.MoveY(3.)
                Expect.isTrue (eqPnt moved.Pt0 (Pnt(0., 3., 0.))) "Pt0 should be moved in Y"
            }

            test "MoveZ instance method" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let moved = box.MoveZ(2.)
                Expect.isTrue (eqPnt moved.Pt0 (Pnt(0., 0., 2.))) "Pt0 should be moved in Z"
            }

            test "move static method" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let moved = FreeBox.move (Vec(5., 3., 2.)) box
                Expect.isTrue (eqPnt moved.Pt0 (Pnt(5., 3., 2.))) "Pt0 should be moved"
            }

            test "translate static method" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let moved = FreeBox.translate (Vec(5., 3., 2.)) box
                Expect.isTrue (eqPnt moved.Pt0 (Pnt(5., 3., 2.))) "Pt0 should be translated"
            }

            test "moveX, moveY, moveZ static methods" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let movedX = FreeBox.moveX 5. box
                let movedY = FreeBox.moveY 3. box
                let movedZ = FreeBox.moveZ 2. box
                Expect.isTrue (eqPnt movedX.Pt0 (Pnt(5., 0., 0.))) "moveX should work"
                Expect.isTrue (eqPnt movedY.Pt0 (Pnt(0., 3., 0.))) "moveY should work"
                Expect.isTrue (eqPnt movedZ.Pt0 (Pnt(0., 0., 2.))) "moveZ should work"
            }

            test "Transform with identity matrix" {
                let pts = [|
                    Pnt(1., 2., 3.); Pnt(11., 2., 3.); Pnt(11., 7., 3.); Pnt(1., 7., 3.)
                    Pnt(1., 2., 6.); Pnt(11., 2., 6.); Pnt(11., 7., 6.); Pnt(1., 7., 6.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let transformed = box.Transform(Matrix.identity)
                Expect.isTrue (eqPnt transformed.Pt0 box.Pt0) "Pt0 should be unchanged with identity"
            }

            test "Transform with translation matrix" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let m = Matrix.createTranslation(Vec(5., 3., 2.))
                let transformed = box.Transform(m)
                Expect.isTrue (eqPnt transformed.Pt0 (Pnt(5., 3., 2.))) "Pt0 should be translated"
            }

            test "transform static method" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let m = Matrix.createTranslation(Vec(5., 3., 2.))
                let transformed = FreeBox.transform m box
                Expect.isTrue (eqPnt transformed.Pt0 (Pnt(5., 3., 2.))) "Pt0 should be translated"
            }

            test "TransformRigid instance method" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let m = RigidMatrix.createTranslation(Vec(5., 3., 2.))
                let transformed = box.TransformRigid(m)
                Expect.isTrue (eqPnt transformed.Pt0 (Pnt(5., 3., 2.))) "Pt0 should be translated"
            }

            test "transformRigid static method" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)
                    Pnt(0., 0., 10.); Pnt(10., 0., 10.); Pnt(10., 10., 10.); Pnt(0., 10., 10.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let m = RigidMatrix.createTranslation(Vec(5., 3., 2.))
                let transformed = FreeBox.transformRigid m box
                Expect.isTrue (eqPnt transformed.Pt0 (Pnt(5., 3., 2.))) "Pt0 should be translated"
            }

            test "Rotate with identity quaternion" {
                let pts = [|
                    Pnt(1., 2., 3.); Pnt(11., 2., 3.); Pnt(11., 7., 3.); Pnt(1., 7., 3.)
                    Pnt(1., 2., 6.); Pnt(11., 2., 6.); Pnt(11., 7., 6.); Pnt(1., 7., 6.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let rotated = box.Rotate(Quaternion.identity)
                Expect.isTrue (eqPnt rotated.Pt0 box.Pt0) "Pt0 should be unchanged with identity quaternion"
            }

            test "Rotate 90 degrees around Z axis" {
                let pts = [|
                    Pnt(1., 0., 0.); Pnt(2., 0., 0.); Pnt(2., 1., 0.); Pnt(1., 1., 0.)
                    Pnt(1., 0., 1.); Pnt(2., 0., 1.); Pnt(2., 1., 1.); Pnt(1., 1., 1.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let q = Quaternion.createFromDegrees(UnitVec.Zaxis, 90.)
                let rotated = box.Rotate(q)
                Expect.isTrue (eqPnt rotated.Pt0 (Pnt(0., 1., 0.))) "Pt0 should be rotated 90 degrees"
            }

            test "rotate static method" {
                let pts = [|
                    Pnt(1., 0., 0.); Pnt(2., 0., 0.); Pnt(2., 1., 0.); Pnt(1., 1., 0.)
                    Pnt(1., 0., 1.); Pnt(2., 0., 1.); Pnt(2., 1., 1.); Pnt(1., 1., 1.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let q = Quaternion.createFromDegrees(UnitVec.Zaxis, 90.)
                let rotated = FreeBox.rotate q box
                Expect.isTrue (eqPnt rotated.Pt0 (Pnt(0., 1., 0.))) "Pt0 should be rotated 90 degrees"
            }

            test "RotateWithCenter keeps center point fixed" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(2., 0., 0.); Pnt(2., 2., 0.); Pnt(0., 2., 0.)
                    Pnt(0., 0., 2.); Pnt(2., 0., 2.); Pnt(2., 2., 2.); Pnt(0., 2., 2.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let center = Pnt(1., 1., 1.)
                let q = Quaternion.createFromDegrees(UnitVec.Zaxis, 90.)
                let rotated = box.RotateWithCenter(center, q)
                // After rotation, center should still be at (1,1,1)
                // Pt0 at (0,0,0) rotated 90 degrees around (1,1,1) in Z should go to (2,0,0)
                Expect.isTrue (eqPnt rotated.Pt0 (Pnt(2., 0., 0.))) "Pt0 should be rotated around center"
            }

            test "rotateWithCenter static method" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(2., 0., 0.); Pnt(2., 2., 0.); Pnt(0., 2., 0.)
                    Pnt(0., 0., 2.); Pnt(2., 0., 2.); Pnt(2., 2., 2.); Pnt(0., 2., 2.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let center = Pnt(1., 1., 1.)
                let q = Quaternion.createFromDegrees(UnitVec.Zaxis, 90.)
                let rotated = FreeBox.rotateWithCenter center q box
                Expect.isTrue (eqPnt rotated.Pt0 (Pnt(2., 0., 0.))) "Pt0 should be rotated around center"
            }

            test "scale static method" {
                let pts = [|
                    Pnt(1., 2., 3.); Pnt(11., 2., 3.); Pnt(11., 7., 3.); Pnt(1., 7., 3.)
                    Pnt(1., 2., 6.); Pnt(11., 2., 6.); Pnt(11., 7., 6.); Pnt(1., 7., 6.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let scaled = FreeBox.scale 2. box
                Expect.isTrue (eqPnt scaled.Pt0 (Pnt(2., 4., 6.))) "Pt0 should be scaled"
            }
        ]

    ]
