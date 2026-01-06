module TestHarmonization

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let tests =
    testList "Harmonization Tests" [
        
        testList "Pnt Squared Distance Methods" [
            test "Pnt.SqDistanceTo should match DistanceTo squared" {
                let p1 = Pnt(1.0, 2.0, 3.0)
                let p2 = Pnt(4.0, 6.0, 8.0)
                let sqDist = p1.SqDistanceTo(p2)
                let dist = p1.DistanceTo(p2)
                Expect.floatClose Accuracy.high sqDist (dist * dist) "SqDistanceTo should equal DistanceTo squared"
            }
            
            test "Pnt.SqDistanceFromOrigin should match DistanceFromOrigin squared" {
                let p = Pnt(3.0, 4.0, 0.0)
                let sqDist = p.SqDistanceFromOrigin
                let dist = p.DistanceFromOrigin
                Expect.floatClose Accuracy.high sqDist (dist * dist) "SqDistanceFromOrigin should equal 25.0"
                Expect.floatClose Accuracy.high sqDist 25.0 "Should be 25.0"
            }
            
            test "Pnt.SqDistanceToLine should work correctly" {
                let testPt = Pnt(5.0, 5.0, 0.0)
                let fromPt = Pnt(0.0, 0.0, 0.0)
                let uv = UnitVec.Xaxis
                let len = 10.0
                let sqDist = testPt.SqDistanceToLine(fromPt, uv, len)
                Expect.floatClose Accuracy.high sqDist 25.0 "Squared distance should be 25.0 (5^2)"
            }
            
            test "Pnt.SqDistanceTo on identical points" {
                let p = Pnt(1.0, 2.0, 3.0)
                Expect.floatClose Accuracy.high (p.SqDistanceTo(p)) 0.0 "Distance to self should be 0"
            }
            
            test "Pnt.SqDistanceTo on very close points" {
                let p1 = Pnt(1.0, 2.0, 3.0)
                let p2 = Pnt(1.0 + 1e-10, 2.0, 3.0)
                let sqDist = p1.SqDistanceTo(p2)
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
                let v = UnitVc.create(1.0, 1.0)
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
                let v = UnitVec.create(1.0, 1.0, 0.0)
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
