module TestRotation2D

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let inline eqFloat a b = abs(a - b) < 1e-9
let inline eqPt a b = Pt.distance a b < 1e-9
let inline eqVc a b = Vc.length (a - b) < 1e-9

let tests =
    testList "Rotation2D" [

        testList "Creation and Properties" [

            test "createFromDegrees 0" {
                let r = Rotation2D.createFromDegrees 0.0
                Expect.isTrue (eqFloat r.Sin 0.0) "Sin should be 0"
                Expect.isTrue (eqFloat r.Cos 1.0) "Cos should be 1"
            }

            test "createFromDegrees 90" {
                let r = Rotation2D.createFromDegrees 90.0
                Expect.isTrue (eqFloat r.Sin 1.0) "Sin should be 1"
                Expect.isTrue (eqFloat r.Cos 0.0) "Cos should be 0"
            }

            test "createFromDegrees 180" {
                let r = Rotation2D.createFromDegrees 180.0
                Expect.isTrue (eqFloat r.Sin 0.0) "Sin should be 0"
                Expect.isTrue (eqFloat r.Cos -1.0) "Cos should be -1"
            }

            test "createFromDegrees -90" {
                let r = Rotation2D.createFromDegrees -90.0
                Expect.isTrue (eqFloat r.Sin -1.0) "Sin should be -1"
                Expect.isTrue (eqFloat r.Cos 0.0) "Cos should be 0"
            }

            test "createFromRadians π/2" {
                let r = Rotation2D.createFromRadians (System.Math.PI / 2.0)
                Expect.isTrue (eqFloat r.Sin 1.0) "Sin should be 1"
                Expect.isTrue (eqFloat r.Cos 0.0) "Cos should be 0"
            }

            test "createFromRadians π" {
                let r = Rotation2D.createFromRadians System.Math.PI
                Expect.isTrue (eqFloat r.Sin 0.0) "Sin should be 0"
                Expect.isTrue (eqFloat r.Cos -1.0) "Cos should be -1"
            }
        ]

        testList "InRadians and InDegrees full range" [

            test "InDegrees 45°" {
                let r = Rotation2D.createFromDegrees 45.0
                Expect.isTrue (eqFloat r.InDegrees 45.0) "Should return 45°"
            }

            test "InDegrees 90°" {
                let r = Rotation2D.createFromDegrees 90.0
                Expect.isTrue (eqFloat r.InDegrees 90.0) "Should return 90°"
            }

            test "InDegrees 135°" {
                let r = Rotation2D.createFromDegrees 135.0
                Expect.isTrue (eqFloat r.InDegrees 135.0) "Should return 135° not 45°"
            }

            test "InDegrees 180°" {
                let r = Rotation2D.createFromDegrees 180.0
                let deg = abs(r.InDegrees)
                Expect.isTrue (eqFloat deg 180.0) "Should return ±180°"
            }

            test "InDegrees 270°" {
                let r = Rotation2D.createFromDegrees 270.0
                Expect.isTrue (eqFloat r.InDegrees -90.0) "Should return -90° (equivalent to 270°)"
            }

            test "InDegrees -90°" {
                let r = Rotation2D.createFromDegrees -90.0
                Expect.isTrue (eqFloat r.InDegrees -90.0) "Should return -90°"
            }

            test "InDegrees -135°" {
                let r = Rotation2D.createFromDegrees -135.0
                Expect.isTrue (eqFloat r.InDegrees -135.0) "Should return -135°"
            }

            test "InRadians π/4" {
                let r = Rotation2D.createFromRadians (System.Math.PI / 4.0)
                Expect.isTrue (eqFloat r.InRadians (System.Math.PI / 4.0)) "Should return π/4"
            }

            test "InRadians 3π/4" {
                let r = Rotation2D.createFromRadians (3.0 * System.Math.PI / 4.0)
                Expect.isTrue (eqFloat r.InRadians (3.0 * System.Math.PI / 4.0)) "Should return 3π/4"
            }
        ]

        testList "Inverse" [

            test "Inverse of 45°" {
                let r = Rotation2D.createFromDegrees 45.0
                let inv = r.Inverse
                Expect.isTrue (eqFloat inv.InDegrees -45.0) "Inverse should be -45°"
            }

            test "Inverse of -90°" {
                let r = Rotation2D.createFromDegrees -90.0
                let inv = r.Inverse
                Expect.isTrue (eqFloat inv.InDegrees 90.0) "Inverse should be 90°"
            }

            test "Inverse of 135°" {
                let r = Rotation2D.createFromDegrees 135.0
                let inv = r.Inverse
                Expect.isTrue (eqFloat inv.InDegrees -135.0) "Inverse should be -135°"
            }

            test "Double inverse returns original" {
                let r = Rotation2D.createFromDegrees 67.0
                let inv = r.Inverse.Inverse
                Expect.isTrue (eqFloat r.Sin inv.Sin && eqFloat r.Cos inv.Cos) "Double inverse should equal original"
            }
        ]

        testList "Add operations" [

            test "Add 45° + 45° = 90°" {
                let r1 = Rotation2D.createFromDegrees 45.0
                let r2 = Rotation2D.createFromDegrees 45.0
                let result = r1.Add r2
                Expect.isTrue (eqFloat result.InDegrees 90.0) "45° + 45° should be 90°"
            }

            test "Add 90° + 90° = 180°" {
                let r1 = Rotation2D.createFromDegrees 90.0
                let r2 = Rotation2D.createFromDegrees 90.0
                let result = r1.Add r2
                let deg = abs(result.InDegrees)
                Expect.isTrue (eqFloat deg 180.0) "90° + 90° should be ±180°"
            }

            test "Add 135° + 90° = -135°" {
                let r1 = Rotation2D.createFromDegrees 135.0
                let r2 = Rotation2D.createFromDegrees 90.0
                let result = r1.Add r2
                Expect.isTrue (eqFloat result.InDegrees -135.0) "135° + 90° wraps to -135°"
            }

            test "Add negative angles" {
                let r1 = Rotation2D.createFromDegrees -45.0
                let r2 = Rotation2D.createFromDegrees -45.0
                let result = r1.Add r2
                Expect.isTrue (eqFloat result.InDegrees -90.0) "-45° + -45° should be -90°"
            }

            test "AddDegrees 30° + 60° = 90°" {
                let r = Rotation2D.createFromDegrees 30.0
                let result = r.AddDegrees 60.0
                Expect.isTrue (eqFloat result.InDegrees 90.0) "30° + 60° should be 90°"
            }

            test "AddRadians π/4 + π/4 = π/2" {
                let r = Rotation2D.createFromRadians (System.Math.PI / 4.0)
                let result = r.AddRadians (System.Math.PI / 4.0)
                Expect.isTrue (eqFloat result.InRadians (System.Math.PI / 2.0)) "π/4 + π/4 should be π/2"
            }
        ]

        testList "createFromVectors with UnitVc" [

            test "From Xaxis to Yaxis = 90°" {
                let a = UnitVc.Xaxis
                let b = UnitVc.Yaxis
                let r = Rotation2D.createFromVectors(a, b)
                Expect.isTrue (eqFloat r.InDegrees 90.0) "Xaxis to Yaxis should be 90°"
            }

            test "From Yaxis to Xaxis = -90°" {
                let a = UnitVc.Yaxis
                let b = UnitVc.Xaxis
                let r = Rotation2D.createFromVectors(a, b)
                Expect.isTrue (eqFloat r.InDegrees -90.0) "Yaxis to Xaxis should be -90°"
            }

            test "From Xaxis to -Xaxis = 180°" {
                let a = UnitVc.Xaxis
                let b = Vc(-1.0, 0.0) |> Vc.unitize
                let r = Rotation2D.createFromVectors(a, b)
                let deg = abs(r.InDegrees)
                Expect.isTrue (eqFloat deg 180.0) "Xaxis to -Xaxis should be ±180°"
            }

            test "Same vector = 0°" {
                let a = UnitVc.Xaxis
                let b = UnitVc.Xaxis
                let r = Rotation2D.createFromVectors(a, b)
                Expect.isTrue (eqFloat r.InDegrees 0.0) "Same vector should be 0°"
            }

            test "45° rotation" {
                let a = UnitVc.Xaxis
                let b = Vc(1.0, 1.0) |> Vc.unitize
                let r = Rotation2D.createFromVectors(a, b)
                Expect.isTrue (eqFloat r.InDegrees 45.0) "Should be 45°"
            }

            test "135° rotation" {
                let a = UnitVc.Xaxis
                let b = Vc(-1.0, 1.0) |> Vc.unitize
                let r = Rotation2D.createFromVectors(a, b)
                Expect.isTrue (eqFloat r.InDegrees 135.0) "Should be 135°"
            }
        ]

        testList "createFromVectors with Vc" [

            test "From (1,0) to (0,1) = 90°" {
                let a = Vc(1.0, 0.0)
                let b = Vc(0.0, 1.0)
                let r = Rotation2D.createFromVectors(a, b)
                Expect.isTrue (eqFloat r.InDegrees 90.0) "(1,0) to (0,1) should be 90°"
            }

            test "From (2,0) to (0,3) = 90°" {
                let a = Vc(2.0, 0.0)
                let b = Vc(0.0, 3.0)
                let r = Rotation2D.createFromVectors(a, b)
                Expect.isTrue (eqFloat r.InDegrees 90.0) "Magnitude doesn't affect angle"
            }

            test "From (1,1) to (-1,1) = 90°" {
                let a = Vc(1.0, 1.0)
                let b = Vc(-1.0, 1.0)
                let r = Rotation2D.createFromVectors(a, b)
                Expect.isTrue (eqFloat r.InDegrees 90.0) "(1,1) to (-1,1) should be 90°"
            }

            test "Zero-length vector a throws" {
                let a = Vc(0.0, 0.0)
                let b = Vc(1.0, 0.0)
                Expect.throws (fun () -> Rotation2D.createFromVectors(a, b) |> ignore) "Should throw for zero-length vector a"
            }

            test "Zero-length vector b throws" {
                let a = Vc(1.0, 0.0)
                let b = Vc(0.0, 0.0)
                Expect.throws (fun () -> Rotation2D.createFromVectors(a, b) |> ignore) "Should throw for zero-length vector b"
            }

            test "Very small vector a throws" {
                let a = Vc(1e-20, 0.0)
                let b = Vc(1.0, 0.0)
                Expect.throws (fun () -> Rotation2D.createFromVectors(a, b) |> ignore) "Should throw for very small vector a"
            }

            test "Very small vector b throws" {
                let a = Vc(1.0, 0.0)
                let b = Vc(0.0, 1e-20)
                Expect.throws (fun () -> Rotation2D.createFromVectors(a, b) |> ignore) "Should throw for very small vector b"
            }
        ]

        testList "equals method" [

            test "Exact equality with tolerance 0.0" {
                let r1 = Rotation2D.createFromDegrees 45.0
                let r2 = Rotation2D.createFromDegrees 45.0
                Expect.isTrue (Rotation2D.equals 0.0 r1 r2) "Same rotations should be equal with 0.0 tolerance"
            }

            test "Within tolerance" {
                let r1 = Rotation2D.createFromDegrees 45.0
                let r2 = Rotation2D.createFromDegrees 45.001
                Expect.isTrue (Rotation2D.equals 0.001 r1 r2) "Should be equal within tolerance"
            }

            test "Outside tolerance" {
                let r1 = Rotation2D.createFromDegrees 45.0
                let r2 = Rotation2D.createFromDegrees 46.0
                Expect.isFalse (Rotation2D.equals 0.001 r1 r2) "Should not be equal outside tolerance"
            }

            test "Different rotations not equal" {
                let r1 = Rotation2D.createFromDegrees 45.0
                let r2 = Rotation2D.createFromDegrees 90.0
                Expect.isFalse (Rotation2D.equals 0.0 r1 r2) "Different rotations should not be equal"
            }
        ]

        testList "String representations" [

            test "ToString contains degrees" {
                let r = Rotation2D.createFromDegrees 45.0
                let str = r.ToString()
                Expect.stringContains str "45" "ToString should contain angle value"
            }

            test "AsString contains degrees" {
                let r = Rotation2D.createFromDegrees 90.0
                let str = r.AsString
                Expect.stringContains str "90" "AsString should contain angle value"
            }

            test "AsFSharpCode is valid" {
                let r = Rotation2D.createFromDegrees 30.0
                let code = r.AsFSharpCode
                Expect.stringContains code "Rotation2D" "AsFSharpCode should contain type name"
            }
        ]

        testList "Edge cases" [

            test "360° equals 0°" {
                let r360 = Rotation2D.createFromDegrees 360.0
                let r0 = Rotation2D.createFromDegrees 0.0
                Expect.isTrue (Rotation2D.equals 1e-9 r360 r0) "360° should equal 0°"
            }

            test "720° equals 0°" {
                let r720 = Rotation2D.createFromDegrees 720.0
                let r0 = Rotation2D.createFromDegrees 0.0
                Expect.isTrue (Rotation2D.equals 1e-9 r720 r0) "720° should equal 0°"
            }

            test "-360° equals 0°" {
                let rNeg360 = Rotation2D.createFromDegrees -360.0
                let r0 = Rotation2D.createFromDegrees 0.0
                Expect.isTrue (Rotation2D.equals 1e-9 rNeg360 r0) "-360° should equal 0°"
            }

            test "Add with wrapping positive" {
                let r1 = Rotation2D.createFromDegrees 200.0
                let r2 = Rotation2D.createFromDegrees 200.0
                let result = r1.Add r2
                Expect.isTrue (eqFloat result.InDegrees 40.0) "200° + 200° should wrap to 40°"
            }

            test "Add with wrapping negative" {
                let r1 = Rotation2D.createFromDegrees -200.0
                let r2 = Rotation2D.createFromDegrees -200.0
                let result = r1.Add r2
                Expect.isTrue (eqFloat result.InDegrees -40.0) "-200° + -200° should wrap to -40°"
            }
        ]

        testList "Add rotations beyond 360° (multiple full rotations)" [

            test "90° + 90° + 90° + 90° = 0° (full rotation counterclockwise)" {
                let r90 = Rotation2D.createFromDegrees 90.0
                let result = r90.Add(r90).Add(r90).Add(r90).Add(r90).Add(r90).Add(r90).Add(r90)
                Expect.isTrue (eqFloat result.InDegrees 0.0) "Eight 90° rotations should return to 0°"
            }

            test "45° added eight times = 0° (full rotation counterclockwise)" {
                let r45 = Rotation2D.createFromDegrees 45.0
                let result = r45.Add(r45).Add(r45).Add(r45).Add(r45).Add(r45).Add(r45).Add(r45)
                Expect.isTrue (eqFloat result.InDegrees 0.0) "Eight 45° rotations (360°) should return to 0°"
            }

            test "-90° + -90° + -90° + -90° = 0° (full rotation clockwise)" {
                let rNeg90 = Rotation2D.createFromDegrees -90.0
                let result = rNeg90.Add(rNeg90).Add(rNeg90).Add(rNeg90)
                Expect.isTrue (eqFloat result.InDegrees 0.0) "Four -90° rotations should return to 0°"
            }

            test "100° + 100° + 100° + 100° = 40° (wraps counterclockwise)" {
                let r100 = Rotation2D.createFromDegrees 100.0
                let result = r100.Add(r100).Add(r100).Add(r100)
                Expect.isTrue (eqFloat result.InDegrees 40.0) "Four 100° rotations (400°) should wrap to 40°"
            }

            test "Adding 720° worth of rotations: 180° + 180° + 180° + 180° = 0°" {
                let r180 = Rotation2D.createFromDegrees 180.0
                let result = r180.Add(r180).Add(r180).Add(r180)
                Expect.isTrue (eqFloat (abs result.InDegrees) 0.0) "Four 180° rotations (720°) should return to 0°"
            }

            test "Mixing positive and negative: 270° + -90° = 180°" {
                let r270 = Rotation2D.createFromDegrees 270.0
                let rNeg90 = Rotation2D.createFromDegrees -90.0
                let result = r270.Add rNeg90
                let deg = abs(result.InDegrees)
                Expect.isTrue (eqFloat deg 180.0) "270° + -90° should be ±180°"
            }

            test "Large positive accumulation: 250° + 250° = 140°" {
                let r250 = Rotation2D.createFromDegrees 250.0
                let result = r250.Add r250
                Expect.isTrue (eqFloat result.InDegrees 140.0) "500° should wrap to 140° (500 - 360 )"
            }

            test "Large negative accumulation: -250° + -250° = -140°" {
                let rNeg250 = Rotation2D.createFromDegrees -250.0
                let result = rNeg250.Add rNeg250
                Expect.isTrue (eqFloat result.InDegrees -140.0) "-500° should wrap to -140° (-500 + 360 )"
            }

            test "135° added three times = 45° (wraps once)" {
                let r135 = Rotation2D.createFromDegrees 135.0
                let result = r135.Add(r135).Add(r135)
                Expect.isTrue (eqFloat result.InDegrees 45.0) "Three 135° rotations (405°) should wrap to 45°"
            }

            test "-135° added three times = -45° (wraps once clockwise)" {
                let rNeg135 = Rotation2D.createFromDegrees -135.0
                let result = rNeg135.Add(rNeg135).Add(rNeg135)
                Expect.isTrue (eqFloat result.InDegrees -45.0) "Three -135° rotations (-405°) should wrap to -45°"
            }

            test "Very large accumulation: 1000° equivalent to 280°" {
                let r1000 = Rotation2D.createFromDegrees 1000.0
                let r280 = Rotation2D.createFromDegrees 280.0
                Expect.isTrue (Rotation2D.equals 1e-9 r1000 r280) "1000° (2 full rotations + 280°) should equal 280°"
            }

            test "Very large negative accumulation: -1000° equivalent to -280°" {
                let rNeg1000 = Rotation2D.createFromDegrees -1000.0
                let rNeg280 = Rotation2D.createFromDegrees -280.0
                Expect.isTrue (Rotation2D.equals 1e-9 rNeg1000 rNeg280) "-1000° should equal -280°"
            }

            test "Accumulated rotation maintains precision over many adds" {
                // Add 36° ten times to get exactly 360° (full rotation)
                let r36 = Rotation2D.createFromDegrees 36.0
                let mutable result = r36
                for _i in 1..9 do
                    result <- result.Add r36
                Expect.isTrue (eqFloat result.InDegrees 0.0) "Ten 36° rotations should accumulate to 360° = 0°"
            }

            test "AddDegrees with accumulation beyond 360°" {
                let r = Rotation2D.createFromDegrees 100.0
                let result = r.AddDegrees(100.0).AddDegrees(100.0).AddDegrees(100.0)
                Expect.isTrue (eqFloat result.InDegrees 40.0) "100° + 300° via AddDegrees should be 40°"
            }

            test "AddRadians with accumulation beyond 2π" {
                let r = Rotation2D.createFromRadians (System.Math.PI / 2.0)
                let result = r.AddRadians(System.Math.PI / 2.0).AddRadians(System.Math.PI / 2.0).AddRadians(System.Math.PI / 2.0)
                Expect.isTrue (eqFloat result.InRadians 0.0) "Four π/2 rotations via AddRadians should be 0"
            }
        ]

        testList "Half rotation - point transformation tests" [

            // Test helper: rotating a point by half twice should equal rotating by full
            // Uses multiple test points to ensure robustness

            // Quadrant 1: 0° to 90°
            test "Half of 0° transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromDegrees 0.0
                let half = r.Half
                let fullResult = Pt.rotateBy r testPt
                let halfTwiceResult = testPt |> Pt.rotateBy half |> Pt.rotateBy half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for 0°"
            }

            test "Half of 30° transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromDegrees 30.0
                let half = r.Half
                let fullResult = Pt.rotateBy r testPt
                let halfTwiceResult = testPt |> Pt.rotateBy half |> Pt.rotateBy half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for 30°"
            }



            // Quadrant 2: 90° to 180°
            test "Half of 120° transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromDegrees 120.0
                let half = r.Half
                let fullResult = Pt.rotateBy r testPt
                let halfTwiceResult = testPt |> Pt.rotateBy half |> Pt.rotateBy half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for 120°"
            }


            test "Half of -60° transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromDegrees -60.0
                let half = r.Half
                let fullResult = Pt.rotateBy r testPt
                let halfTwiceResult = testPt |> Pt.rotateBy half |> Pt.rotateBy half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for -60°"
            }


            test "Half of -15° transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromDegrees -15.0
                let half = r.Half
                let fullResult = Pt.rotateBy r testPt
                let halfTwiceResult = testPt |> Pt.rotateBy half |> Pt.rotateBy half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for -15°"
            }


            test "Half of -0.1° transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromDegrees -0.1
                let half = r.Half
                let fullResult = Pt.rotateBy r testPt
                let halfTwiceResult = testPt |> Pt.rotateBy half |> Pt.rotateBy half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for -0.1°"
            }

            // Angles close to boundaries
            test "Half of 89° transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromDegrees 89.0
                let half = r.Half
                let fullResult = Pt.rotateBy r testPt
                let halfTwiceResult = testPt |> Pt.rotateBy half |> Pt.rotateBy half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for 89°"
            }

            test "Half of 91° transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromDegrees 91.0
                let half = r.Half
                let fullResult = Pt.rotateBy r testPt
                let halfTwiceResult = testPt |> Pt.rotateBy half |> Pt.rotateBy half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for 91°"
            }

            test "Half of 179° transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromDegrees 179.0
                let half = r.Half
                let fullResult = Pt.rotateBy r testPt
                let halfTwiceResult = testPt |> Pt.rotateBy half |> Pt.rotateBy half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for 179°"
            }

            test "Half of -179° transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromDegrees -179.0
                let half = r.Half
                let fullResult = Pt.rotateBy r testPt
                let halfTwiceResult = testPt |> Pt.rotateBy half |> Pt.rotateBy half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for -179°"
            }

            // Using radians with point transformation
            test "Half of π/2 rad transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromRadians (System.Math.PI / 2.0)
                let half = r.Half
                let fullResult = Pt.rotateBy r testPt
                let halfTwiceResult = testPt |> Pt.rotateBy half |> Pt.rotateBy half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for π/2"
            }

            test "Half of π rad transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromRadians System.Math.PI
                let half = r.Half
                let fullResult = Pt.rotateBy r testPt
                let halfTwiceResult = testPt |> Pt.rotateBy half |> Pt.rotateBy half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for π"
            }

            test "Half of -π/2 rad transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromRadians (-System.Math.PI / 2.0)
                let half = r.Half
                let fullResult = Pt.rotateBy r testPt
                let halfTwiceResult = testPt |> Pt.rotateBy half |> Pt.rotateBy half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for -π/2"
            }

            // Comprehensive property test: half rotation twice equals full for many angles
            test "Half rotation twice equals full rotation for all angles 0° to 360° in 10° steps" {
                let testPts = [| Pt(3.0, 4.0); Pt(1.0, 0.0); Pt(0.0, 1.0); Pt(-2.0, 5.0); Pt(7.0, -3.0) |]
                let angles = [| 0.0; 10.0; 20.0; 30.0; 40.0; 50.0; 60.0; 70.0; 80.0; 90.0;
                                100.0; 110.0; 120.0; 130.0; 140.0; 150.0; 160.0; 170.0; 180.0;
                                -10.0; -20.0; -30.0; -40.0; -50.0; -60.0; -70.0; -80.0; -90.0;
                                -100.0; -110.0; -120.0; -130.0; -140.0; -150.0; -160.0; -170.0; -180.0 |]
                for deg in angles do
                    for testPt in testPts do
                        let r = Rotation2D.createFromDegrees deg
                        let half = r.Half
                        let fullResult = Pt.rotateBy r testPt
                        let halfTwiceResult = testPt |> Pt.rotateBy half |> Pt.rotateBy half
                        if not (eqPt fullResult halfTwiceResult) then
                            failwith $"Half rotation twice should equal full rotation for {deg}° at point {testPt}"
            }

            // Test with vectors too
            test "Half rotation on vector: half twice equals full for 90°" {
                let testVc = Vc(3.0, 4.0)
                let r = Rotation2D.createFromDegrees 90.0
                let half = r.Half
                let fullResult = Vc.rotateBy r testVc
                let halfTwiceResult = testVc |> Vc.rotateBy half |> Vc.rotateBy half
                Expect.isTrue (eqVc fullResult halfTwiceResult) "Vector: half twice should equal full for 90°"
            }

            test "Half rotation on vector: half twice equals full for 180°" {
                let testVc = Vc(3.0, 4.0)
                let r = Rotation2D.createFromDegrees 180.0
                let half = r.Half
                let fullResult = Vc.rotateBy r testVc
                let halfTwiceResult = testVc |> Vc.rotateBy half |> Vc.rotateBy half
                Expect.isTrue (eqVc fullResult halfTwiceResult) "Vector: half twice should equal full for 180°"
            }

            test "Half rotation on vector: half twice equals full for -135°" {
                let testVc = Vc(3.0, 4.0)
                let r = Rotation2D.createFromDegrees -135.0
                let half = r.Half
                let fullResult = Vc.rotateBy r testVc
                let halfTwiceResult = testVc |> Vc.rotateBy half |> Vc.rotateBy half
                Expect.isTrue (eqVc fullResult halfTwiceResult) "Vector: half twice should equal full for -135°"
            }

            // Edge: applying Half multiple times with point verification
            test "Applying Half twice gives quarter rotation - verified by point transformation" {
                let testPt = Pt(1.0, 0.0)
                let r = Rotation2D.createFromDegrees 180.0
                let half = r.Half       // 90°
                let quarter = half.Half // 45°
                let expectedPt = Pt.rotate 45.0 testPt
                let actualPt = Pt.rotateBy quarter testPt
                Expect.isTrue (eqPt expectedPt actualPt) "Half of Half of 180° should rotate point by 45°"
            }

            test "Applying Half three times - verified by point transformation" {
                let testPt = Pt(1.0, 0.0)
                let r = Rotation2D.createFromDegrees 160.0
                let half1 = r.Half       // 80°
                let half2 = half1.Half   // 40°
                let half3 = half2.Half   // 20°
                let expectedPt = Pt.rotate 20.0 testPt
                let actualPt = Pt.rotateBy half3 testPt
                Expect.isTrue (eqPt expectedPt actualPt) "160° / 2 / 2 / 2 = 20° - verified by point position"
            }

            // Test multiple test points at different positions
            test "Half rotation works correctly for points at various positions for 45°" {
                let testPts = [| Pt(1.0, 0.0); Pt(0.0, 1.0); Pt(-1.0, 0.0); Pt(0.0, -1.0);
                                 Pt(1.0, 1.0); Pt(-1.0, 1.0); Pt(-1.0, -1.0); Pt(1.0, -1.0);
                                 Pt(10.0, 0.0); Pt(0.0, 10.0); Pt(5.0, 5.0) |]
                let r = Rotation2D.createFromDegrees 45.0
                let half = r.Half
                for testPt in testPts do
                    let fullResult = Pt.rotateBy r testPt
                    let halfTwiceResult = testPt |> Pt.rotateBy half |> Pt.rotateBy half
                    if not (eqPt fullResult halfTwiceResult) then
                        failwith $"Half rotation twice should equal full rotation for 45° at point {testPt}"
            }

            test "Half rotation works correctly for points at various positions for -120°" {
                let testPts = [| Pt(1.0, 0.0); Pt(0.0, 1.0); Pt(-1.0, 0.0); Pt(0.0, -1.0);
                                 Pt(1.0, 1.0); Pt(-1.0, 1.0); Pt(-1.0, -1.0); Pt(1.0, -1.0);
                                 Pt(10.0, 0.0); Pt(0.0, 10.0); Pt(5.0, 5.0) |]
                let r = Rotation2D.createFromDegrees -120.0
                let half = r.Half
                for testPt in testPts do
                    let fullResult = Pt.rotateBy r testPt
                    let halfTwiceResult = testPt |> Pt.rotateBy half |> Pt.rotateBy half
                    if not (eqPt fullResult halfTwiceResult) then
                        failwith $"Half rotation twice should equal full rotation for -120° at point {testPt}"
            }
        ]
    ]
