module TestFreeBox

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let inline eqPnt a b = Pnt.distance a b < 1e-9
let inline eqFloat a b = abs(a - b) < 1e-9

let tests =
    testList "FreeBox" [

        testList "Constructor and Basic Properties" [
            test "createFromEightPoints with 8 points" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 5., 0.); Pnt(0., 5., 0.)
                    Pnt(0., 0., 3.); Pnt(10., 0., 3.); Pnt(10., 5., 3.); Pnt(0., 5., 3.)
                |]
                let box = FreeBox.createFromEightPoints pts
                Expect.equal box.Points.Length 8 "Should have 8 points"
                Expect.isTrue (eqPnt box.Pt0 pts.[0]) "Pt0 should match input"
                Expect.isTrue (eqPnt box.Pt6 pts.[6]) "Pt6 should match input"
            }

            test "createFromEightPoints rejects wrong number of points" {
                let pts = [| Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 5., 0.) |]
                Expect.throws (fun () -> FreeBox.createFromEightPoints pts |> ignore) "Should throw with wrong number of points"
            }

            test "Origin is first point" {
                let pts = [|
                    Pnt(1., 2., 3.); Pnt(11., 2., 3.); Pnt(11., 7., 3.); Pnt(1., 7., 3.)
                    Pnt(1., 2., 6.); Pnt(11., 2., 6.); Pnt(11., 7., 6.); Pnt(1., 7., 6.)
                |]
                let box = FreeBox.createFromEightPoints pts
                Expect.isTrue (eqPnt box.Origin pts.[0]) "Origin should be first point"
            }

            test "Xaxis is vector from Pt0 to Pt1" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 5., 0.); Pnt(0., 5., 0.)
                    Pnt(0., 0., 3.); Pnt(10., 0., 3.); Pnt(10., 5., 3.); Pnt(0., 5., 3.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let expected = Vec(10., 0., 0.)
                Expect.isTrue (Vec.length (box.Xaxis - expected) < 1e-9) "Xaxis should be Pt1 - Pt0"
            }

            test "Yaxis is vector from Pt0 to Pt3" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 5., 0.); Pnt(0., 5., 0.)
                    Pnt(0., 0., 3.); Pnt(10., 0., 3.); Pnt(10., 5., 3.); Pnt(0., 5., 3.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let expected = Vec(0., 5., 0.)
                Expect.isTrue (Vec.length (box.Yaxis - expected) < 1e-9) "Yaxis should be Pt3 - Pt0"
            }

            test "Zaxis is vector from Pt0 to Pt4" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 5., 0.); Pnt(0., 5., 0.)
                    Pnt(0., 0., 3.); Pnt(10., 0., 3.); Pnt(10., 5., 3.); Pnt(0., 5., 3.)
                |]
                let box = FreeBox.createFromEightPoints pts
                let expected = Vec(0., 0., 3.)
                Expect.isTrue (Vec.length (box.Zaxis - expected) < 1e-9) "Zaxis should be Pt4 - Pt0"
            }

            test "SizeX, SizeY, SizeZ" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 5., 0.); Pnt(0., 5., 0.)
                    Pnt(0., 0., 3.); Pnt(10., 0., 3.); Pnt(10., 5., 3.); Pnt(0., 5., 3.)
                |]
                let box = FreeBox.createFromEightPoints pts
                Expect.equal box.SizeX 10. "SizeX should be 10"
                Expect.equal box.SizeY 5. "SizeY should be 5"
                Expect.equal box.SizeZ 3. "SizeZ should be 3"
            }
        ]

        testList "Creation from Box" [
            test "createFromBox" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let freeBox = FreeBox.createFromBox box
                Expect.isTrue (eqPnt freeBox.Pt0 box.Pt0) "Pt0 should match"
                Expect.isTrue (eqPnt freeBox.Pt6 box.Pt6) "Pt6 should match"
                Expect.equal freeBox.SizeX box.SizeX "SizeX should match"
                Expect.equal freeBox.SizeY box.SizeY "SizeY should match"
                Expect.equal freeBox.SizeZ box.SizeZ "SizeZ should match"
            }
        ]

        testList "Creation from 2D Points" [
            test "createFromFour2DPoints with valid points" {
                let pts = [| Pt(0., 0.); Pt(10., 0.); Pt(10., 5.); Pt(0., 5.) |]
                let box = FreeBox.createFromFour2DPoints 2. 8. pts
                Expect.isTrue (eqPnt box.Pt0 (Pnt(0., 0., 2.))) "Pt0 should be at zMin"
                Expect.isTrue (eqPnt box.Pt4 (Pnt(0., 0., 8.))) "Pt4 should be at zMax"
                Expect.equal box.SizeZ 6. "SizeZ should be zMax - zMin"
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
                let scaled = box.ScaleOn center 2.
                // After scaling by 2 around center (5,5,5), Pt0 at (0,0,0) should move to (-5,-5,-5)
                Expect.isTrue (eqPnt scaled.Pt0 (Pnt(-5., -5., -5.))) "Pt0 should be scaled around center"
                // And Pt6 at (10,10,10) should move to (15,15,15)
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
        ]

        testList "Edge Cases" [
            test "Box with zero volume (all points coincident)" {
                let pts = [|
                    Pnt(5., 5., 5.); Pnt(5., 5., 5.); Pnt(5., 5., 5.); Pnt(5., 5., 5.)
                    Pnt(5., 5., 5.); Pnt(5., 5., 5.); Pnt(5., 5., 5.); Pnt(5., 5., 5.)
                |]
                let box = FreeBox.createFromEightPoints pts
                Expect.equal box.SizeX 0. "SizeX should be 0"
                Expect.equal box.SizeY 0. "SizeY should be 0"
                Expect.equal box.SizeZ 0. "SizeZ should be 0"
            }

            test "Box with one dimension (line)" {
                let pts = [|
                    Pnt(0., 5., 5.); Pnt(10., 5., 5.); Pnt(10., 5., 5.); Pnt(0., 5., 5.)
                    Pnt(0., 5., 5.); Pnt(10., 5., 5.); Pnt(10., 5., 5.); Pnt(0., 5., 5.)
                |]
                let box = FreeBox.createFromEightPoints pts
                Expect.equal box.SizeX 10. "SizeX should be 10"
                Expect.equal box.SizeY 0. "SizeY should be 0"
                Expect.equal box.SizeZ 0. "SizeZ should be 0"
            }

            test "Box with two dimensions (flat)" {
                let pts = [|
                    Pnt(0., 0., 5.); Pnt(10., 0., 5.); Pnt(10., 10., 5.); Pnt(0., 10., 5.)
                    Pnt(0., 0., 5.); Pnt(10., 0., 5.); Pnt(10., 10., 5.); Pnt(0., 10., 5.)
                |]
                let box = FreeBox.createFromEightPoints pts
                Expect.equal box.SizeX 10. "SizeX should be 10"
                Expect.equal box.SizeY 10. "SizeY should be 10"
                Expect.equal box.SizeZ 0. "SizeZ should be 0"
            }

            test "Non-axis-aligned box (rotated)" {
                let pts = [|
                    Pnt(0., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 20., 0.); Pnt(-10., 10., 0.)
                    Pnt(0., 0., 5.); Pnt(10., 10., 5.); Pnt(0., 20., 5.); Pnt(-10., 10., 5.)
                |]
                let box = FreeBox.createFromEightPoints pts
                Expect.isTrue (box.SizeX > 0.) "Should have non-zero SizeX"
                Expect.isTrue (box.SizeY > 0.) "Should have non-zero SizeY"
                Expect.isTrue (box.SizeZ > 0.) "Should have non-zero SizeZ"
            }
        ]
    ]
