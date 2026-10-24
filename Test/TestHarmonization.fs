module TestHarmonization

open Euclid

open EuclidTestSupport

let tests =
    testList "Harmonization Tests" [

        testList "Pnt Squared Distance Methods" [
            test "Pnt.DistanceToSq should match DistanceTo squared" {
                let p1 = Pnt(1.0, 2.0, 3.0)
                let p2 = Pnt(4.0, 6.0, 8.0)
                let sqDist = p1.DistanceToSq(p2)
                let dist = p1.DistanceTo(p2)
                Expect.floatClose Accuracy.high sqDist (dist * dist) "DistanceToSq should equal DistanceTo squared"
            }

            test "Pnt.DistanceFromOriginSq should match DistanceFromOrigin squared" {
                let p = Pnt(3.0, 4.0, 0.0)
                let sqDist = p.DistanceFromOriginSq
                let dist = p.DistanceFromOrigin
                Expect.floatClose Accuracy.high sqDist (dist * dist) "DistanceFromOriginSq should equal 25.0"
                Expect.floatClose Accuracy.high sqDist 25.0 "Should be 25.0"
            }


            test "Pnt.DistanceToSq on identical points" {
                let p = Pnt(1.0, 2.0, 3.0)
                Expect.floatClose Accuracy.high (p.DistanceToSq(p)) 0.0 "Distance to self should be 0"
            }

            test "Pnt.DistanceToSq on very close points" {
                let p1 = Pnt(1.0, 2.0, 3.0)
                let p2 = Pnt(1.0 + 1e-10, 2.0, 3.0)
                let sqDist = p1.DistanceToSq(p2)
                Expect.isTrue (sqDist < 1e-15) "Very close points should have tiny squared distance"
            }
        ]

        testList "Vec.Half" [
            test "Vec.Half should return half length vector" {
                let v = Vec(4.0, 6.0, 8.0)
                let half = v.Half
                Expect.floatClose Accuracy.high half.X 2.0 "X should be halved"
                Expect.floatClose Accuracy.high half.Y 3.0 "Y should be halved"
                Expect.floatClose Accuracy.high half.Z 4.0 "Z should be halved"
            }

            test "Vec.Half preserves direction" {
                let v = Vec(1.0, 2.0, 3.0)
                let half = v.Half
                let unitV = v.Unitized
                let unitHalf = half.Unitized
                Expect.floatClose Accuracy.high (unitV.Dot(unitHalf)) 1.0 "Direction should be preserved"
            }
        ]

        testList "rotateByQuarterCircle 2D" [
            test "Vc.rotateByQuarterCircle 0 quarters" {
                let v = Vc(1.0, 0.0)
                let rotated = v.RotateByQuarterCircle(0)
                Expect.floatClose Accuracy.high rotated.X 1.0 "X unchanged"
                Expect.floatClose Accuracy.high rotated.Y 0.0 "Y unchanged"
            }

            test "Vc.rotateByQuarterCircle 1 quarter CCW" {
                let v = Vc(1.0, 0.0)
                let rotated = v.RotateByQuarterCircle(1)
                Expect.floatClose Accuracy.high rotated.X 0.0 "Should rotate to Y axis"
                Expect.floatClose Accuracy.high rotated.Y 1.0 "Should rotate to Y axis"
            }

            test "Vc.rotateByQuarterCircle 2 quarters" {
                let v = Vc(1.0, 0.0)
                let rotated = v.RotateByQuarterCircle(2)
                Expect.floatClose Accuracy.high rotated.X -1.0 "Should reverse X"
                Expect.floatClose Accuracy.high rotated.Y 0.0 "Y should be 0"
            }

            test "Vc.rotateByQuarterCircle 3 quarters CCW (= 1 CW)" {
                let v = Vc(1.0, 0.0)
                let rotated = v.RotateByQuarterCircle(3)
                Expect.floatClose Accuracy.high rotated.X 0.0 "Should rotate to -Y axis"
                Expect.floatClose Accuracy.high rotated.Y -1.0 "Should rotate to -Y axis"
            }

            test "Vc.rotateByQuarterCircle -1 quarter (CW)" {
                let v = Vc(1.0, 0.0)
                let rotated = v.RotateByQuarterCircle(-1)
                Expect.floatClose Accuracy.high rotated.X 0.0 "Should rotate CW to -Y"
                Expect.floatClose Accuracy.high rotated.Y -1.0 "Should rotate CW to -Y"
            }

            test "Vc.rotateByQuarterCircle 4 quarters = full circle" {
                let v = Vc(3.0, 4.0)
                let rotated = v.RotateByQuarterCircle(4)
                Expect.floatClose Accuracy.high rotated.X v.X "Full rotation returns to start"
                Expect.floatClose Accuracy.high rotated.Y v.Y "Full rotation returns to start"
            }

            test "Vc.rotateByQuarterCircle preserves length" {
                let v = Vc(3.0, 4.0)
                let len = v.Length
                let rotated = v.RotateByQuarterCircle(1)
                Expect.floatClose Accuracy.high rotated.Length len "Length should be preserved"
            }

            test "UnitVc.rotateByQuarterCircle preserves unit length" {
                let v = UnitVc.create 1.0 1.0
                let rotated = v.RotateByQuarterCircle(1)
                // UnitVc is always unit by definition, just verify it doesn't crash
                Expect.floatClose Accuracy.high (rotated.X * rotated.X + rotated.Y * rotated.Y) 1.0 "Should remain unit vector"
            }
        ]

        testList "rotateByQuarterCircle 3D" [
            test "Vec.rotateByQuarterCircle 0 quarters" {
                let v = Vec(1.0, 0.0, 5.0)
                let rotated = v.RotateByQuarterCircle(0)
                Expect.floatClose Accuracy.high rotated.X 1.0 "X unchanged"
                Expect.floatClose Accuracy.high rotated.Y 0.0 "Y unchanged"
                Expect.floatClose Accuracy.high rotated.Z 5.0 "Z unchanged"
            }

            test "Vec.rotateByQuarterCircle 1 quarter around Z-axis" {
                let v = Vec(1.0, 0.0, 5.0)
                let rotated = v.RotateByQuarterCircle(1)
                Expect.floatClose Accuracy.high rotated.X 0.0 "Should rotate in XY plane"
                Expect.floatClose Accuracy.high rotated.Y 1.0 "Should rotate in XY plane"
                Expect.floatClose Accuracy.high rotated.Z 5.0 "Z should be preserved"
            }

            test "Vec.rotateByQuarterCircle preserves Z component" {
                let v = Vec(3.0, 4.0, 7.0)
                let rotated = v.RotateByQuarterCircle(2)
                Expect.floatClose Accuracy.high rotated.Z v.Z "Z should be preserved"
            }

            test "Vec.rotateByQuarterCircle preserves length" {
                let v = Vec(3.0, 4.0, 5.0)
                let len = v.Length
                for quarters in [-2; -1; 0; 1; 2; 3; 4; 5] do
                    let rotated = v.RotateByQuarterCircle(quarters)
                    Expect.floatClose Accuracy.high rotated.Length len $"Length preserved for {quarters} quarters"
            }

            test "UnitVec.rotateByQuarterCircle preserves unit length" {
                let v = UnitVec.create 1.0 1.0 0.0
                let rotated = v.RotateByQuarterCircle(1)
                // UnitVec is always unit by definition, just verify it doesn't crash
                Expect.floatClose Accuracy.high (rotated.X * rotated.X + rotated.Y * rotated.Y + rotated.Z * rotated.Z) 1.0 "Should remain unit vector"
            }

            test "Vec.rotateByQuarterCircle on vertical vector" {
                let v = Vec(0.0, 0.0, 5.0)
                let rotated = v.RotateByQuarterCircle(1)
                Expect.floatClose Accuracy.high rotated.X 0.0 "Vertical stays vertical in X"
                Expect.floatClose Accuracy.high rotated.Y 0.0 "Vertical stays vertical in Y"
                Expect.floatClose Accuracy.high rotated.Z 5.0 "Z unchanged"
            }
        ]

        testList "Pnt, Vec and UnitVec rotation members" [
            test "Pnt.Rotate matches Pnt.rotate and Vec.Rotate" {
                let q = Quaternion.createFromUnitVecDegrees (UnitVec.create 1.0 2.0 3.0) 37.0
                let p = Pnt(3.0, -7.0, 11.0)
                let r = p.Rotate q
                "matches static" |> Expect.isTrue (Pnt.equalsExact r (Pnt.rotate q p))
                let v = Vec(p.X, p.Y, p.Z).Rotate q
                "matches Vec.Rotate" |> Expect.isTrue (Pnt.distanceTo r (Pnt(v.X, v.Y, v.Z)) < 1e-9)
                "keeps distance to origin" |> Expect.floatClose Accuracy.high (Pnt.distanceTo r Pnt.Origin) (Pnt.distanceTo p Pnt.Origin)
            }

            test "Pnt.Rotate by 90 degrees around Z equals Rotate2Dby90CCW" {
                let q = Quaternion.createFromUnitVecDegrees UnitVec.zAxis 90.0
                let p = Pnt(3.0, 1.0, 2.0)
                "rotated" |> Expect.isTrue (Pnt.distanceTo (p.Rotate q) (Pnt(-1.0, 3.0, 2.0)) < 1e-9)
                "same as Rotate2Dby90CCW" |> Expect.isTrue (Pnt.distanceTo (p.Rotate q) p.Rotate2Dby90CCW < 1e-9)
            }

            test "Pnt.RotateAround matches static and rotates the offset from the center" {
                let q = Quaternion.createFromUnitVecDegrees (UnitVec.create (-2.0) 1.0 0.5) 123.0
                let p = Pnt(3.0, -7.0, 11.0)
                let cen = Pnt(-4.0, 5.0, 2.5)
                let r = p.RotateAround cen q
                "matches static" |> Expect.isTrue (Pnt.equalsExact r (Pnt.rotateAround cen q p))
                "equals rotated offset" |> Expect.isTrue (Pnt.distanceTo r (cen + (p - cen).Rotate q) < 1e-9)
                "keeps distance to center" |> Expect.floatClose Accuracy.high (Pnt.distanceTo r cen) (Pnt.distanceTo p cen)
                "the center itself does not move" |> Expect.isTrue (Pnt.distanceTo (cen.RotateAround cen q) cen < 1e-12)
            }

            test "Pnt.Rotate2Dby90CCW and Rotate2Dby90CW" {
                let p = Pnt(3.0, -7.0, 11.0)
                "CCW" |> Expect.isTrue (Pnt.equalsExact p.Rotate2Dby90CCW (Pnt(7.0, 3.0, 11.0)))
                "CW" |> Expect.isTrue (Pnt.equalsExact p.Rotate2Dby90CW (Pnt(-7.0, -3.0, 11.0)))
                "CCW then CW" |> Expect.isTrue (Pnt.equalsExact p.Rotate2Dby90CCW.Rotate2Dby90CW p)
                "static CCW" |> Expect.isTrue (Pnt.equalsExact (Pnt.rotate2Dby90CCW p) p.Rotate2Dby90CCW)
                "static CW" |> Expect.isTrue (Pnt.equalsExact (Pnt.rotate2Dby90CW p) p.Rotate2Dby90CW)
                let v = Vec(p.X, p.Y, p.Z).Rotate2Dby90CCW
                "same as Vec" |> Expect.isTrue (Pnt.equalsExact p.Rotate2Dby90CCW (Pnt(v.X, v.Y, v.Z)))
            }

            test "Pnt.RotateByQuarterCircle" {
                let p = Pnt(3.0, -7.0, 11.0)
                for n in [-5; -4; -3; -2; -1; 0; 1; 2; 3; 4; 5; 1000; -1001] do
                    let r = p.RotateByQuarterCircle n
                    $"static matches member for {n}" |> Expect.isTrue (Pnt.equalsExact r (Pnt.rotateByQuarterCircle n p))
                    $"matches rotateOnZDegrees for {n}" |> Expect.isTrue (Pnt.distanceTo r (Pnt.rotateOnZDegrees (90.0 * float n) p) < 1e-9)
                    $"Z preserved for {n}" |> Expect.equal r.Z p.Z
                "1 is Rotate2Dby90CCW" |> Expect.isTrue (Pnt.equalsExact (p.RotateByQuarterCircle 1) p.Rotate2Dby90CCW)
                "-1 is Rotate2Dby90CW" |> Expect.isTrue (Pnt.equalsExact (p.RotateByQuarterCircle(-1)) p.Rotate2Dby90CW)
                "4 is identity" |> Expect.isTrue (Pnt.equalsExact (p.RotateByQuarterCircle 4) p)
            }

            test "Vec.Rotate and UnitVec.Rotate match their static members" {
                let q = Quaternion.createFromUnitVecDegrees (UnitVec.create 1.0 (-2.0) 0.7) 71.0
                let v = Vec(3.0, -7.0, 11.0)
                "Vec" |> Expect.isTrue (Vec.equalsExact (v.Rotate q) (Vec.rotate q v))
                "Vec length" |> Expect.floatClose Accuracy.high (v.Rotate q).Length v.Length
                let u = UnitVec.create 3.0 (-7.0) 11.0
                "UnitVec" |> Expect.isTrue (UnitVec.equalsExact (u.Rotate q) (UnitVec.rotate q u))
                let ur = u.Rotate q
                "UnitVec stays unit" |> Expect.floatClose Accuracy.high (ur.X*ur.X + ur.Y*ur.Y + ur.Z*ur.Z) 1.0
            }
        ]

        testList "Static vs Member consistency" [
            test "Vc.rotateByQuarterCircle static matches member" {
                let v = Vc(3.0, 4.0)
                let staticResult = Vc.rotateByQuarterCircle 2 v
                let memberResult = v.RotateByQuarterCircle(2)
                Expect.floatClose Accuracy.high staticResult.X memberResult.X "Static and member should match"
                Expect.floatClose Accuracy.high staticResult.Y memberResult.Y "Static and member should match"
            }

            test "Vec.rotateByQuarterCircle static matches member" {
                let v = Vec(3.0, 4.0, 5.0)
                let staticResult = Vec.rotateByQuarterCircle 2 v
                let memberResult = v.RotateByQuarterCircle(2)
                Expect.floatClose Accuracy.high staticResult.X memberResult.X "Static and member should match"
                Expect.floatClose Accuracy.high staticResult.Y memberResult.Y "Static and member should match"
                Expect.floatClose Accuracy.high staticResult.Z memberResult.Z "Static and member should match"
            }
        ]

        testList "Edge cases" [
            test "rotateByQuarterCircle with large numbers" {
                let v = Vc(1.0, 0.0)
                let rotated = v.RotateByQuarterCircle(1000)
                // 1000 mod 4 = 0, so should be unchanged
                Expect.floatClose Accuracy.high rotated.X 1.0 "Large number should wrap"
                Expect.floatClose Accuracy.high rotated.Y 0.0 "Large number should wrap"
            }

            test "rotateByQuarterCircle with negative large numbers" {
                let v = Vc(1.0, 0.0)
                let rotated = v.RotateByQuarterCircle(-1001)
                // -1001 mod 4 = -1 = 3, so 3 quarters CCW = 1 quarter CW
                Expect.floatClose Accuracy.high rotated.X 0.0 "Should rotate correctly"
                Expect.floatClose Accuracy.high rotated.Y -1.0 "Should rotate correctly"
            }
        ]
    ]
