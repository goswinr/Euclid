module TestRotation2D

open Euclid

open EuclidTestSupport

let inline eqFloat a b = abs(a - b) < 1e-9
let inline eqPt a b = Pt.distanceTo a b < 1e-9
let inline eqVc a b = Vc.length (a - b) < 1e-9
let eqLn (a:Line2D) (b:Line2D) = eqPt a.From b.From && eqPt a.To b.To
let eqRect (a:Rect2D) (b:Rect2D) = eqPt a.Origin b.Origin && eqVc a.XAxis b.XAxis && eqVc a.YAxis b.YAxis
let eqPl (a:Polyline2D) (b:Polyline2D) =
    a.PointCount = b.PointCount && Seq.forall (fun i -> eqPt (a.GetPoint i) (b.GetPoint i)) [0 .. a.PointCount - 1]

// Shapes for the rotation and scale grammar tests, away from the origin and not axis-aligned.
let gCen = Pt(12.0, -7.0)
let gDeg = 37.0
let gRad = UtilEuclid.toRadians gDeg
let gRot = Rotation2D.createFromDegrees gDeg
let gPt = Pt(15.5, -3.25)
let gLn = Line2D(Pt(14.0, -5.0), Pt(19.0, -2.5))
let gRect = Rect2D.createFromVcs (Vc(4.0, 3.0)) (Vc(-1.5, 2.0)) (Pt(13.0, -4.0))
let gPl () = Polyline2D.createFromPts [Pt(14., -5.); Pt(19., -2.5); Pt(17., 1.); Pt(14., -5.)]
/// Rotates around a center by translating to the origin, rotating with Vc.rotate and translating back.
let refRotAround (cen:Pt) (r:Rotation2D) (p:Pt) = cen + Vc.rotate r (p - cen)

let tests =
    testList "Rotation2D" [
        test "scaleAngle supports fractional negative and extended rotations" {
            let point = Pt(15.5, -3.25)
            let quarter = Rotation2D.scaleAngle 0.25
            for degrees in [-179.999; -90.; -37.; 0.; 37.; 90.; 179.999] do
                let r = Rotation2D.createFromDegrees degrees
                for factor in [-3.; -1.; 0.; 0.25; 0.5; 1.; 1.5; 2.; 4.] do
                    let scaled = r |> Rotation2D.scaleAngle factor
                    let expected = Rotation2D.createFromDegrees (degrees * factor)
                    Expect.isTrue (Rotation2D.equalsWithin 1e-12 scaled expected) $"{degrees} degrees scaled by {factor}"
                    Expect.isTrue (eqPt (point.Rotate scaled) (point.Rotate expected)) "scaled rotation applied to a point"
                    Expect.isTrue (Rotation2D.equalsExact scaled (r.ScaleAngle factor)) "static and instance agree"
                    Expect.floatClose Accuracy.veryHigh (scaled.Sin*scaled.Sin + scaled.Cos*scaled.Cos) 1. "unit length"
                Expect.isTrue (Rotation2D.equalsExact (quarter r) (r.ScaleAngle 0.25)) "partial application"
                Expect.isTrue (Rotation2D.equalsWithin 1e-12 (r.ScaleAngle 0.5) r.Half) "half agrees"
                Expect.isTrue (Rotation2D.equalsExact r (Rotation2D.createFromDegrees degrees)) "input unchanged"
        }

        test "scaleAngle uses the stored signed angle and preserves half-turn direction" {
            let r = Rotation2D.createFromDegrees 270.
            Expect.isTrue (Rotation2D.equalsWithin 1e-12 (r.ScaleAngle 0.25) (Rotation2D.createFromDegrees (-22.5))) "270 degrees is stored as -90 degrees"
            for degrees in [-180.; 180.] do
                let halfTurn = Rotation2D.createFromDegrees degrees
                Expect.isTrue (Rotation2D.equalsWithin 1e-12 (halfTurn.ScaleAngle 0.25) (Rotation2D.createFromDegrees (degrees * 0.25))) "signed half turn"
            let exactHalfTurn = Rotation2D.createUnchecked(0., -1.)
            Expect.isTrue (Rotation2D.equalsWithin 1e-12 (exactHalfTurn.ScaleAngle 0.5) exactHalfTurn.Half) "exact positive half turn"
        }

        test "scaleAngle handles identity and very small angles" {
            for factor in [-2.; 0.; 0.25; 1.; 2.; System.Double.MaxValue] do
                Expect.isTrue (Rotation2D.equalsExact (Rotation2D.identity.ScaleAngle factor) Rotation2D.identity) "identity stays identity"
            for radians in [-1e-200; -1e-20; -1e-10; 1e-10; 1e-20; 1e-200] do
                let r = Rotation2D.createFromRadians radians
                let quarter = r.ScaleAngle 0.25
                Expect.isTrue (abs (quarter.Sin / (radians * 0.25) - 1.) < 1e-12) "tiny angle retained"
                Expect.isTrue (Rotation2D.equalsWithin 1e-12 (r.ScaleAngle (1. / radians)) (Rotation2D.createFromRadians 1.)) "tiny angle can be enlarged"
        }

        test "scaleAngle rejects non-finite factors and overflowing angles" {
            let r = Rotation2D.createFromDegrees 137.
            for factor in [nan; infinity; -infinity] do
                Expect.throws (fun () -> r.ScaleAngle factor |> ignore) "non-finite factor"
                Expect.throws (fun () -> Rotation2D.identity.ScaleAngle factor |> ignore) "non-finite factor on identity"
            Expect.throws (fun () -> r.ScaleAngle System.Double.MaxValue |> ignore) "scaled angle overflows"
        }

        test "identity and half wrappers" {
            let p = Pt(13.,27.)
            Expect.isTrue (eqPt (p.Rotate Rotation2D.identity) p) "identity"
            for angle in [-179.;-37.;0.;37.;180.] do
                let r = Rotation2D.createFromDegrees angle
                let h = Rotation2D.half r
                Expect.isTrue (Rotation2D.equalsWithin 1e-12 (h.Then h) r) "half composed twice"
            let exact = Rotation2D.createUnchecked(0.,-1.)
            Expect.isTrue (exact.Half.Sin > 0.) "CCW tie"
        }

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
                Expect.isTrue (eqFloat r.AngleInDegrees 45.0) "Should return 45°"
            }

            test "InDegrees 90°" {
                let r = Rotation2D.createFromDegrees 90.0
                Expect.isTrue (eqFloat r.AngleInDegrees 90.0) "Should return 90°"
            }

            test "InDegrees 135°" {
                let r = Rotation2D.createFromDegrees 135.0
                Expect.isTrue (eqFloat r.AngleInDegrees 135.0) "Should return 135° not 45°"
            }

            test "InDegrees 180°" {
                let r = Rotation2D.createFromDegrees 180.0
                let deg = abs(r.AngleInDegrees)
                Expect.isTrue (eqFloat deg 180.0) "Should return ±180°"
            }

            test "InDegrees 270°" {
                let r = Rotation2D.createFromDegrees 270.0
                Expect.isTrue (eqFloat r.AngleInDegrees -90.0) "Should return -90° (equivalent to 270°)"
            }

            test "InDegrees -90°" {
                let r = Rotation2D.createFromDegrees -90.0
                Expect.isTrue (eqFloat r.AngleInDegrees -90.0) "Should return -90°"
            }

            test "InDegrees -135°" {
                let r = Rotation2D.createFromDegrees -135.0
                Expect.isTrue (eqFloat r.AngleInDegrees -135.0) "Should return -135°"
            }

            test "InRadians π/4" {
                let r = Rotation2D.createFromRadians (System.Math.PI / 4.0)
                Expect.isTrue (eqFloat r.AngleInRadians (System.Math.PI / 4.0)) "Should return π/4"
            }

            test "InRadians 3π/4" {
                let r = Rotation2D.createFromRadians (3.0 * System.Math.PI / 4.0)
                Expect.isTrue (eqFloat r.AngleInRadians (3.0 * System.Math.PI / 4.0)) "Should return 3π/4"
            }
        ]

        testList "Inverse" [

            test "Inverse of 45°" {
                let r = Rotation2D.createFromDegrees 45.0
                let inv = r.Inverse
                Expect.isTrue (eqFloat inv.AngleInDegrees -45.0) "Inverse should be -45°"
            }

            test "Inverse of -90°" {
                let r = Rotation2D.createFromDegrees -90.0
                let inv = r.Inverse
                Expect.isTrue (eqFloat inv.AngleInDegrees 90.0) "Inverse should be 90°"
            }

            test "Inverse of 135°" {
                let r = Rotation2D.createFromDegrees 135.0
                let inv = r.Inverse
                Expect.isTrue (eqFloat inv.AngleInDegrees -135.0) "Inverse should be -135°"
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
                let result = r1.Then r2
                Expect.isTrue (eqFloat result.AngleInDegrees 90.0) "45° + 45° should be 90°"
            }

            test "Add 90° + 90° = 180°" {
                let r1 = Rotation2D.createFromDegrees 90.0
                let r2 = Rotation2D.createFromDegrees 90.0
                let result = r1.Then r2
                let deg = abs(result.AngleInDegrees)
                Expect.isTrue (eqFloat deg 180.0) "90° + 90° should be ±180°"
            }

            test "Add 135° + 90° = -135°" {
                let r1 = Rotation2D.createFromDegrees 135.0
                let r2 = Rotation2D.createFromDegrees 90.0
                let result = r1.Then r2
                Expect.isTrue (eqFloat result.AngleInDegrees -135.0) "135° + 90° wraps to -135°"
            }

            test "Add negative angles" {
                let r1 = Rotation2D.createFromDegrees -45.0
                let r2 = Rotation2D.createFromDegrees -45.0
                let result = r1.Then r2
                Expect.isTrue (eqFloat result.AngleInDegrees -90.0) "-45° + -45° should be -90°"
            }

            test "AddDegrees 30° + 60° = 90°" {
                let r = Rotation2D.createFromDegrees 30.0
                let result = r.AddDegrees 60.0
                Expect.isTrue (eqFloat result.AngleInDegrees 90.0) "30° + 60° should be 90°"
            }

            test "AddRadians π/4 + π/4 = π/2" {
                let r = Rotation2D.createFromRadians (System.Math.PI / 4.0)
                let result = r.AddRadians (System.Math.PI / 4.0)
                Expect.isTrue (eqFloat result.AngleInRadians (System.Math.PI / 2.0)) "π/4 + π/4 should be π/2"
            }
        ]

        testList "createFromVectors with UnitVc" [

            test "From Xaxis to Yaxis = 90°" {
                let a = UnitVc.xAxis
                let b = UnitVc.yAxis
                let r = Rotation2D.createFromUnitVcs a b
                Expect.isTrue (eqFloat r.AngleInDegrees 90.0) "Xaxis to Yaxis should be 90°"
            }

            test "From Yaxis to Xaxis = -90°" {
                let a = UnitVc.yAxis
                let b = UnitVc.xAxis
                let r = Rotation2D.createFromUnitVcs a b
                Expect.isTrue (eqFloat r.AngleInDegrees -90.0) "Yaxis to Xaxis should be -90°"
            }

            test "From Xaxis to -Xaxis = 180°" {
                let a = UnitVc.xAxis
                let b = Vc(-1.0, 0.0) |> Vc.unitize
                let r = Rotation2D.createFromUnitVcs a b
                let deg = abs(r.AngleInDegrees)
                Expect.isTrue (eqFloat deg 180.0) "Xaxis to -Xaxis should be ±180°"
            }

            test "Same vector = 0°" {
                let a = UnitVc.xAxis
                let b = UnitVc.xAxis
                let r = Rotation2D.createFromUnitVcs a b
                Expect.isTrue (eqFloat r.AngleInDegrees 0.0) "Same vector should be 0°"
            }

            test "45° rotation" {
                let a = UnitVc.xAxis
                let b = Vc(1.0, 1.0) |> Vc.unitize
                let r = Rotation2D.createFromUnitVcs a b
                Expect.isTrue (eqFloat r.AngleInDegrees 45.0) "Should be 45°"
            }

            test "135° rotation" {
                let a = UnitVc.xAxis
                let b = Vc(-1.0, 1.0) |> Vc.unitize
                let r = Rotation2D.createFromUnitVcs a b
                Expect.isTrue (eqFloat r.AngleInDegrees 135.0) "Should be 135°"
            }
        ]

        testList "createFromVectors with Vc" [

            test "From (1,0) to (0,1) = 90°" {
                let a = Vc(1.0, 0.0)
                let b = Vc(0.0, 1.0)
                let r = Rotation2D.createFromVcs a b
                Expect.isTrue (eqFloat r.AngleInDegrees 90.0) "(1,0) to (0,1) should be 90°"
            }

            test "From (2,0) to (0,3) = 90°" {
                let a = Vc(2.0, 0.0)
                let b = Vc(0.0, 3.0)
                let r = Rotation2D.createFromVcs a b
                Expect.isTrue (eqFloat r.AngleInDegrees 90.0) "Magnitude doesn't affect angle"
            }

            test "From (1,1) to (-1,1) = 90°" {
                let a = Vc(1.0, 1.0)
                let b = Vc(-1.0, 1.0)
                let r = Rotation2D.createFromVcs a b
                Expect.isTrue (eqFloat r.AngleInDegrees 90.0) "(1,1) to (-1,1) should be 90°"
            }

            test "Zero-length vector a throws" {
                let a = Vc(0.0, 0.0)
                let b = Vc(1.0, 0.0)
                Expect.throws (fun () -> Rotation2D.createFromVcs a b |> ignore) "Should throw for zero-length vector a"
            }

            test "Zero-length vector b throws" {
                let a = Vc(1.0, 0.0)
                let b = Vc(0.0, 0.0)
                Expect.throws (fun () -> Rotation2D.createFromVcs a b |> ignore) "Should throw for zero-length vector b"
            }

            test "Very small vector a throws" {
                let a = Vc(1e-20, 0.0)
                let b = Vc(1.0, 0.0)
                Expect.throws (fun () -> Rotation2D.createFromVcs a b |> ignore) "Should throw for very small vector a"
            }

            test "Very small vector b throws" {
                let a = Vc(1.0, 0.0)
                let b = Vc(0.0, 1e-20)
                Expect.throws (fun () -> Rotation2D.createFromVcs a b |> ignore) "Should throw for very small vector b"
            }
        ]

        testList "equals method" [

            test "Exact equality with tolerance 0.0" {
                let r1 = Rotation2D.createFromDegrees 45.0
                let r2 = Rotation2D.createFromDegrees 45.0
                Expect.isTrue (Rotation2D.equalsExact r1 r2) "Same rotations should be equal with 0.0 tolerance"
            }

            test "Within tolerance" {
                let r1 = Rotation2D.createFromDegrees 45.0
                let r2 = Rotation2D.createFromDegrees 45.001
                Expect.isTrue (Rotation2D.equalsWithin 0.001 r1 r2) "Should be equal within tolerance"
            }

            test "Outside tolerance" {
                let r1 = Rotation2D.createFromDegrees 45.0
                let r2 = Rotation2D.createFromDegrees 46.0
                Expect.isFalse (Rotation2D.equalsWithin 0.001 r1 r2) "Should not be equal outside tolerance"
            }

            test "Different rotations not equal" {
                let r1 = Rotation2D.createFromDegrees 45.0
                let r2 = Rotation2D.createFromDegrees 90.0
                Expect.isFalse (Rotation2D.equalsExact r1 r2) "Different rotations should not be equal"
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
                Expect.isTrue (Rotation2D.equalsWithin 1e-9 r360 r0) "360° should equal 0°"
            }

            test "720° equals 0°" {
                let r720 = Rotation2D.createFromDegrees 720.0
                let r0 = Rotation2D.createFromDegrees 0.0
                Expect.isTrue (Rotation2D.equalsWithin 1e-9 r720 r0) "720° should equal 0°"
            }

            test "-360° equals 0°" {
                let rNeg360 = Rotation2D.createFromDegrees -360.0
                let r0 = Rotation2D.createFromDegrees 0.0
                Expect.isTrue (Rotation2D.equalsWithin 1e-9 rNeg360 r0) "-360° should equal 0°"
            }

            test "Add with wrapping positive" {
                let r1 = Rotation2D.createFromDegrees 200.0
                let r2 = Rotation2D.createFromDegrees 200.0
                let result = r1.Then r2
                Expect.isTrue (eqFloat result.AngleInDegrees 40.0) "200° + 200° should wrap to 40°"
            }

            test "Add with wrapping negative" {
                let r1 = Rotation2D.createFromDegrees -200.0
                let r2 = Rotation2D.createFromDegrees -200.0
                let result = r1.Then r2
                Expect.isTrue (eqFloat result.AngleInDegrees -40.0) "-200° + -200° should wrap to -40°"
            }
        ]

        testList "Add rotations beyond 360° (multiple full rotations)" [

            test "90° + 90° + 90° + 90° = 0° (full rotation counterclockwise)" {
                let r90 = Rotation2D.createFromDegrees 90.0
                let result = r90.Then(r90).Then(r90).Then(r90).Then(r90).Then(r90).Then(r90).Then(r90)
                Expect.isTrue (eqFloat result.AngleInDegrees 0.0) "Eight 90° rotations should return to 0°"
            }

            test "45° added eight times = 0° (full rotation counterclockwise)" {
                let r45 = Rotation2D.createFromDegrees 45.0
                let result = r45.Then(r45).Then(r45).Then(r45).Then(r45).Then(r45).Then(r45).Then(r45)
                Expect.isTrue (eqFloat result.AngleInDegrees 0.0) "Eight 45° rotations (360°) should return to 0°"
            }

            test "-90° + -90° + -90° + -90° = 0° (full rotation clockwise)" {
                let rNeg90 = Rotation2D.createFromDegrees -90.0
                let result = rNeg90.Then(rNeg90).Then(rNeg90).Then(rNeg90)
                Expect.isTrue (eqFloat result.AngleInDegrees 0.0) "Four -90° rotations should return to 0°"
            }

            test "100° + 100° + 100° + 100° = 40° (wraps counterclockwise)" {
                let r100 = Rotation2D.createFromDegrees 100.0
                let result = r100.Then(r100).Then(r100).Then(r100)
                Expect.isTrue (eqFloat result.AngleInDegrees 40.0) "Four 100° rotations (400°) should wrap to 40°"
            }

            test "Adding 720° worth of rotations: 180° + 180° + 180° + 180° = 0°" {
                let r180 = Rotation2D.createFromDegrees 180.0
                let result = r180.Then(r180).Then(r180).Then(r180)
                Expect.isTrue (eqFloat (abs result.AngleInDegrees) 0.0) "Four 180° rotations (720°) should return to 0°"
            }

            test "Mixing positive and negative: 270° + -90° = 180°" {
                let r270 = Rotation2D.createFromDegrees 270.0
                let rNeg90 = Rotation2D.createFromDegrees -90.0
                let result = r270.Then rNeg90
                let deg = abs(result.AngleInDegrees)
                Expect.isTrue (eqFloat deg 180.0) "270° + -90° should be ±180°"
            }

            test "Large positive accumulation: 250° + 250° = 140°" {
                let r250 = Rotation2D.createFromDegrees 250.0
                let result = r250.Then r250
                Expect.isTrue (eqFloat result.AngleInDegrees 140.0) "500° should wrap to 140° (500 - 360 )"
            }

            test "Large negative accumulation: -250° + -250° = -140°" {
                let rNeg250 = Rotation2D.createFromDegrees -250.0
                let result = rNeg250.Then rNeg250
                Expect.isTrue (eqFloat result.AngleInDegrees -140.0) "-500° should wrap to -140° (-500 + 360 )"
            }

            test "135° added three times = 45° (wraps once)" {
                let r135 = Rotation2D.createFromDegrees 135.0
                let result = r135.Then(r135).Then(r135)
                Expect.isTrue (eqFloat result.AngleInDegrees 45.0) "Three 135° rotations (405°) should wrap to 45°"
            }

            test "-135° added three times = -45° (wraps once clockwise)" {
                let rNeg135 = Rotation2D.createFromDegrees -135.0
                let result = rNeg135.Then(rNeg135).Then(rNeg135)
                Expect.isTrue (eqFloat result.AngleInDegrees -45.0) "Three -135° rotations (-405°) should wrap to -45°"
            }

            test "Very large accumulation: 1000° equivalent to 280°" {
                let r1000 = Rotation2D.createFromDegrees 1000.0
                let r280 = Rotation2D.createFromDegrees 280.0
                Expect.isTrue (Rotation2D.equalsWithin 1e-9 r1000 r280) "1000° (2 full rotations + 280°) should equal 280°"
            }

            test "Very large negative accumulation: -1000° equivalent to -280°" {
                let rNeg1000 = Rotation2D.createFromDegrees -1000.0
                let rNeg280 = Rotation2D.createFromDegrees -280.0
                Expect.isTrue (Rotation2D.equalsWithin 1e-9 rNeg1000 rNeg280) "-1000° should equal -280°"
            }

            test "Accumulated rotation maintains precision over many adds" {
                // Add 36° ten times to get exactly 360° (full rotation)
                let r36 = Rotation2D.createFromDegrees 36.0
                let mutable result = r36
                for _i in 1..9 do
                    result <- result.Then r36
                Expect.isTrue (eqFloat result.AngleInDegrees 0.0) "Ten 36° rotations should accumulate to 360° = 0°"
            }

            test "AddDegrees with accumulation beyond 360°" {
                let r = Rotation2D.createFromDegrees 100.0
                let result = r.AddDegrees(100.0).AddDegrees(100.0).AddDegrees(100.0)
                Expect.isTrue (eqFloat result.AngleInDegrees 40.0) "100° + 300° via AddDegrees should be 40°"
            }

            test "AddRadians with accumulation beyond 2π" {
                let r = Rotation2D.createFromRadians (System.Math.PI / 2.0)
                let result = r.AddRadians(System.Math.PI / 2.0).AddRadians(System.Math.PI / 2.0).AddRadians(System.Math.PI / 2.0)
                Expect.isTrue (eqFloat result.AngleInRadians 0.0) "Four π/2 rotations via AddRadians should be 0"
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
                let fullResult = Pt.rotate r testPt
                let halfTwiceResult = testPt |> Pt.rotate half |> Pt.rotate half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for 0°"
            }

            test "Half of 30° transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromDegrees 30.0
                let half = r.Half
                let fullResult = Pt.rotate r testPt
                let halfTwiceResult = testPt |> Pt.rotate half |> Pt.rotate half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for 30°"
            }



            // Quadrant 2: 90° to 180°
            test "Half of 120° transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromDegrees 120.0
                let half = r.Half
                let fullResult = Pt.rotate r testPt
                let halfTwiceResult = testPt |> Pt.rotate half |> Pt.rotate half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for 120°"
            }


            test "Half of -60° transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromDegrees -60.0
                let half = r.Half
                let fullResult = Pt.rotate r testPt
                let halfTwiceResult = testPt |> Pt.rotate half |> Pt.rotate half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for -60°"
            }


            test "Half of -15° transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromDegrees -15.0
                let half = r.Half
                let fullResult = Pt.rotate r testPt
                let halfTwiceResult = testPt |> Pt.rotate half |> Pt.rotate half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for -15°"
            }


            test "Half of -0.1° transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromDegrees -0.1
                let half = r.Half
                let fullResult = Pt.rotate r testPt
                let halfTwiceResult = testPt |> Pt.rotate half |> Pt.rotate half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for -0.1°"
            }

            // Angles close to boundaries
            test "Half of 89° transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromDegrees 89.0
                let half = r.Half
                let fullResult = Pt.rotate r testPt
                let halfTwiceResult = testPt |> Pt.rotate half |> Pt.rotate half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for 89°"
            }

            test "Half of 91° transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromDegrees 91.0
                let half = r.Half
                let fullResult = Pt.rotate r testPt
                let halfTwiceResult = testPt |> Pt.rotate half |> Pt.rotate half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for 91°"
            }

            test "Half of 179° transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromDegrees 179.0
                let half = r.Half
                let fullResult = Pt.rotate r testPt
                let halfTwiceResult = testPt |> Pt.rotate half |> Pt.rotate half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for 179°"
            }

            test "Half of -179° transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromDegrees -179.0
                let half = r.Half
                let fullResult = Pt.rotate r testPt
                let halfTwiceResult = testPt |> Pt.rotate half |> Pt.rotate half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for -179°"
            }

            // Using radians with point transformation
            test "Half of π/2 rad transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromRadians (System.Math.PI / 2.0)
                let half = r.Half
                let fullResult = Pt.rotate r testPt
                let halfTwiceResult = testPt |> Pt.rotate half |> Pt.rotate half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for π/2"
            }

            test "Half of π rad transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromRadians System.Math.PI
                let half = r.Half
                let fullResult = Pt.rotate r testPt
                let halfTwiceResult = testPt |> Pt.rotate half |> Pt.rotate half
                Expect.isTrue (eqPt fullResult halfTwiceResult) "Rotating by half twice should equal full rotation for π"
            }

            test "Half of -π/2 rad transforms point correctly" {
                let testPt = Pt(3.0, 4.0)
                let r = Rotation2D.createFromRadians (-System.Math.PI / 2.0)
                let half = r.Half
                let fullResult = Pt.rotate r testPt
                let halfTwiceResult = testPt |> Pt.rotate half |> Pt.rotate half
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
                        let fullResult = Pt.rotate r testPt
                        let halfTwiceResult = testPt |> Pt.rotate half |> Pt.rotate half
                        if not (eqPt fullResult halfTwiceResult) then
                            failwith $"Half rotation twice should equal full rotation for {deg}° at point {testPt}"
            }

            // Test with vectors too
            test "Half rotation on vector: half twice equals full for 90°" {
                let testVc = Vc(3.0, 4.0)
                let r = Rotation2D.createFromDegrees 90.0
                let half = r.Half
                let fullResult = Vc.rotate r testVc
                let halfTwiceResult = testVc |> Vc.rotate half |> Vc.rotate half
                Expect.isTrue (eqVc fullResult halfTwiceResult) "Vector: half twice should equal full for 90°"
            }

            test "Half rotation on vector: half twice equals full for 180°" {
                let testVc = Vc(3.0, 4.0)
                let r = Rotation2D.createFromDegrees 180.0
                let half = r.Half
                let fullResult = Vc.rotate r testVc
                let halfTwiceResult = testVc |> Vc.rotate half |> Vc.rotate half
                Expect.isTrue (eqVc fullResult halfTwiceResult) "Vector: half twice should equal full for 180°"
            }

            test "Half rotation on vector: half twice equals full for -135°" {
                let testVc = Vc(3.0, 4.0)
                let r = Rotation2D.createFromDegrees -135.0
                let half = r.Half
                let fullResult = Vc.rotate r testVc
                let halfTwiceResult = testVc |> Vc.rotate half |> Vc.rotate half
                Expect.isTrue (eqVc fullResult halfTwiceResult) "Vector: half twice should equal full for -135°"
            }

            // Edge: applying Half multiple times with point verification
            test "Applying Half twice gives quarter rotation - verified by point transformation" {
                let testPt = Pt(1.0, 0.0)
                let r = Rotation2D.createFromDegrees 180.0
                let half = r.Half       // 90°
                let quarter = half.Half // 45°
                let expectedPt = Pt.rotateDegrees 45.0 testPt
                let actualPt = Pt.rotate quarter testPt
                Expect.isTrue (eqPt expectedPt actualPt) "Half of Half of 180° should rotate point by 45°"
            }

            test "Applying Half three times - verified by point transformation" {
                let testPt = Pt(1.0, 0.0)
                let r = Rotation2D.createFromDegrees 160.0
                let half1 = r.Half       // 80°
                let half2 = half1.Half   // 40°
                let half3 = half2.Half   // 20°
                let expectedPt = Pt.rotateDegrees 20.0 testPt
                let actualPt = Pt.rotate half3 testPt
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
                    let fullResult = Pt.rotate r testPt
                    let halfTwiceResult = testPt |> Pt.rotate half |> Pt.rotate half
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
                    let fullResult = Pt.rotate r testPt
                    let halfTwiceResult = testPt |> Pt.rotate half |> Pt.rotate half
                    if not (eqPt fullResult halfTwiceResult) then
                        failwith $"Half rotation twice should equal full rotation for -120° at point {testPt}"
            }
        ]

        testList "Rotation and scale grammar" [

            test "Pt.rotateAroundDegrees known values" {
                Expect.isTrue (eqPt (Pt.rotateAroundDegrees (Pt(2., 1.)) 90.0 (Pt(3., 1.))) (Pt(2., 2.))) "90° CCW"
                Expect.isTrue (eqPt (Pt.rotateAroundDegrees (Pt(2., 1.)) -90.0 (Pt(3., 1.))) (Pt(2., 0.))) "-90° is CW"
                Expect.isTrue (eqPt (Pt.rotateAroundRadians (Pt(2., 1.)) System.Math.PI (Pt(3., 1.))) (Pt(1., 1.))) "Pi radians"
            }

            test "Pt rotate, rotateDegrees and rotateRadians agree" {
                let expected = Pt.rotate gRot gPt
                Expect.isTrue (eqPt expected (Pt(gPt.X * gRot.Cos - gPt.Y * gRot.Sin, gPt.X * gRot.Sin + gPt.Y * gRot.Cos))) "rotate"
                Expect.isTrue (eqPt expected (Pt.rotateDegrees gDeg gPt)) "rotateDegrees"
                Expect.isTrue (eqPt expected (Pt.rotateRadians gRad gPt)) "rotateRadians"
                Expect.isTrue (eqPt expected (gPt.Rotate gRot)) "Rotate"
                Expect.isTrue (eqPt expected (gPt.RotateDegrees gDeg)) "RotateDegrees"
                Expect.isTrue (eqPt expected (gPt.RotateRadians gRad)) "RotateRadians"
            }

            test "Pt rotateAround variants agree and keep the distance to the center" {
                let expected = refRotAround gCen gRot gPt
                Expect.isTrue (eqPt expected (Pt.rotateAround gCen gRot gPt)) "rotateAround"
                Expect.isTrue (eqPt expected (Pt.rotateAroundDegrees gCen gDeg gPt)) "rotateAroundDegrees"
                Expect.isTrue (eqPt expected (Pt.rotateAroundRadians gCen gRad gPt)) "rotateAroundRadians"
                Expect.isTrue (eqPt expected (gPt.RotateAround gCen gRot)) "RotateAround"
                Expect.isTrue (eqPt expected (gPt.RotateAroundDegrees gCen gDeg)) "RotateAroundDegrees"
                Expect.isTrue (eqPt expected (gPt.RotateAroundRadians gCen gRad)) "RotateAroundRadians"
                Expect.isTrue (eqFloat (Pt.distanceTo gCen expected) (Pt.distanceTo gCen gPt)) "distance to center is kept"
            }

            test "Pt rotateAround identities" {
                Expect.isTrue (eqPt gPt (Pt.rotateAroundDegrees gCen 0.0 gPt)) "0°"
                Expect.isTrue (eqPt gPt (Pt.rotateAroundDegrees gCen 360.0 gPt)) "360°"
                Expect.isTrue (eqPt gPt (Pt.rotateAroundDegrees gPt 123.0 gPt)) "point at the center stays"
            }

            test "Pt.scaleOn" {
                Expect.isTrue (eqPt (Pt.scaleOn gCen 2.5 gPt) (gCen + (gPt - gCen) * 2.5)) "factor 2.5"
                Expect.isTrue (eqPt (Pt.scaleOn gCen 1.0 gPt) gPt) "factor 1 is identity"
                Expect.isTrue (eqPt (Pt.scaleOn gCen 0.0 gPt) gCen) "factor 0 collapses to center"
                Expect.isTrue (eqPt (Pt.scaleOn gCen -1.0 gPt) (gCen - (gPt - gCen))) "factor -1 mirrors through center"
                Expect.isTrue (eqPt (Pt.scaleOn Pt.Origin 3.0 gPt) (Pt.scale 3.0 gPt)) "origin center equals scale"
            }

            test "Pnt.scaleOn" {
                let cen = Pnt(12.0, -7.0, 3.0)
                let p = Pnt(15.5, -3.25, -1.0)
                let expected = Pnt(12.0 + 3.5 * 2.5, -7.0 + 3.75 * 2.5, 3.0 - 4.0 * 2.5)
                Expect.isTrue (Pnt.distanceTo expected (Pnt.scaleOn cen 2.5 p) < 1e-9) "factor 2.5"
                Expect.isTrue (Pnt.distanceTo cen (Pnt.scaleOn cen 0.0 p) < 1e-9) "factor 0 collapses to center"
                Expect.isTrue (Pnt.distanceTo (Pnt.scale 3.0 p) (Pnt.scaleOn Pnt.Origin 3.0 p) < 1e-9) "origin center equals scale"
            }

            test "Vc and UnitVc rotateDegrees and rotateRadians agree with rotate" {
                let v = Vc(3.0, -2.0)
                let expected = Vc.rotate gRot v
                Expect.isTrue (eqVc expected (Vc.rotateDegrees gDeg v)) "Vc.rotateDegrees"
                Expect.isTrue (eqVc expected (Vc.rotateRadians gRad v)) "Vc.rotateRadians"
                Expect.isTrue (eqVc expected (v.RotateDegrees gDeg)) "Vc.RotateDegrees"
                Expect.isTrue (eqVc expected (v.Rotate gRot)) "Vc.Rotate"
                let u = UnitVc.create 3.0 (-2.0)
                let uExpected = UnitVc.rotate gRot u
                Expect.isTrue (eqVc uExpected.AsVc (UnitVc.rotateDegrees gDeg u).AsVc) "UnitVc.rotateDegrees"
                Expect.isTrue (eqVc uExpected.AsVc (UnitVc.rotateRadians gRad u).AsVc) "UnitVc.rotateRadians"
                Expect.isTrue (eqVc uExpected.AsVc (u.RotateDegrees gDeg).AsVc) "UnitVc.RotateDegrees"
                Expect.isTrue (eqFloat (u.RotateDegrees 123.0).AsVc.Length 1.0) "UnitVc stays unit length"
            }

            test "Line2D rotateDegrees and rotateRadians agree with rotate" {
                let expected = Line2D.rotate gRot gLn
                Expect.isTrue (eqLn expected (Line2D(Pt.rotate gRot gLn.From, Pt.rotate gRot gLn.To))) "rotate"
                Expect.isTrue (eqLn expected (Line2D.rotateDegrees gDeg gLn)) "rotateDegrees"
                Expect.isTrue (eqLn expected (Line2D.rotateRadians gRad gLn)) "rotateRadians"
                Expect.isTrue (eqLn expected (gLn.RotateDegrees gDeg)) "RotateDegrees"
                Expect.isTrue (eqLn expected (gLn.RotateRadians gRad)) "RotateRadians"
            }

            test "Line2D rotateAround variants agree and keep the length" {
                let expected = Line2D(refRotAround gCen gRot gLn.From, refRotAround gCen gRot gLn.To)
                Expect.isTrue (eqLn expected (Line2D.rotateAround gCen gRot gLn)) "rotateAround"
                Expect.isTrue (eqLn expected (Line2D.rotateAroundDegrees gCen gDeg gLn)) "rotateAroundDegrees"
                Expect.isTrue (eqLn expected (Line2D.rotateAroundRadians gCen gRad gLn)) "rotateAroundRadians"
                Expect.isTrue (eqLn expected (gLn.RotateAround gCen gRot)) "RotateAround"
                Expect.isTrue (eqLn expected (gLn.RotateAroundDegrees gCen gDeg)) "RotateAroundDegrees"
                Expect.isTrue (eqLn expected (gLn.RotateAroundRadians gCen gRad)) "RotateAroundRadians"
                Expect.isTrue (eqFloat expected.Length gLn.Length) "length is kept"
            }

            test "Line2D rotateAroundDegrees on a zero-length line" {
                let ln = Line2D(gPt, gPt)
                let r = Line2D.rotateAroundDegrees gCen gDeg ln
                let p = refRotAround gCen gRot gPt
                Expect.isTrue (eqPt r.From p && eqPt r.To p) "both ends rotate to the same point"
            }

            test "Line2D.scaleOn" {
                let expected = Line2D(Pt.scaleOn gCen 2.5 gLn.From, Pt.scaleOn gCen 2.5 gLn.To)
                Expect.isTrue (eqLn expected (Line2D.scaleOn gCen 2.5 gLn)) "scaleOn"
                Expect.isTrue (eqLn expected (gLn.ScaleOn gCen 2.5)) "ScaleOn"
                Expect.isTrue (eqLn (Line2D(gCen, gCen)) (Line2D.scaleOn gCen 0.0 gLn)) "factor 0 collapses to center"
            }

            test "Rect2D rotateDegrees and rotateRadians agree with rotate" {
                let expected = Rect2D.rotate gRot gRect
                Expect.isTrue (eqPt expected.Origin (Pt.rotate gRot gRect.Origin)) "origin is rotated"
                Expect.isTrue (eqRect expected (Rect2D.rotateDegrees gDeg gRect)) "rotateDegrees"
                Expect.isTrue (eqRect expected (Rect2D.rotateRadians gRad gRect)) "rotateRadians"
                Expect.isTrue (eqRect expected (gRect.RotateDegrees gDeg)) "RotateDegrees"
                Expect.isTrue (eqRect expected (gRect.RotateRadians gRad)) "RotateRadians"
            }

            test "Rect2D rotateAround variants agree" {
                let expected = Rect2D.rotateAround gCen gRot gRect
                Expect.isTrue (eqPt expected.Origin (refRotAround gCen gRot gRect.Origin)) "origin is rotated around center"
                Expect.isTrue (eqVc expected.XAxis (Vc.rotate gRot gRect.XAxis)) "X-axis is rotated"
                Expect.isTrue (eqRect expected (Rect2D.rotateAroundDegrees gCen gDeg gRect)) "rotateAroundDegrees"
                Expect.isTrue (eqRect expected (Rect2D.rotateAroundRadians gCen gRad gRect)) "rotateAroundRadians"
                Expect.isTrue (eqRect expected (gRect.RotateAround gCen gRot)) "RotateAround"
                Expect.isTrue (eqRect expected (gRect.RotateAroundDegrees gCen gDeg)) "RotateAroundDegrees"
                Expect.isTrue (eqRect expected (gRect.RotateAroundRadians gCen gRad)) "RotateAroundRadians"
                let aroundOwnCenter = Rect2D.rotateAroundDegrees gRect.Center gDeg gRect
                Expect.isTrue (eqPt aroundOwnCenter.Center gRect.Center) "rotating around its own center keeps it fixed"
            }

            test "Polyline2D rotate instance members change it in place, statics return a new one" {
                let expectedOrigin = Polyline2D.createFromPts ((gPl ()).AsPoints |> Seq.map (Pt.rotate gRot))
                let expectedAround = Polyline2D.createFromPts ((gPl ()).AsPoints |> Seq.map (refRotAround gCen gRot))
                let checkInPlace name (expected:Polyline2D) (f:Polyline2D -> unit) =
                    let pl = gPl ()
                    f pl
                    Expect.isTrue (eqPl expected pl) name
                let checkStatic name (expected:Polyline2D) (f:Polyline2D -> Polyline2D) =
                    let pl = gPl ()
                    Expect.isTrue (eqPl expected (f pl)) name
                    Expect.isTrue (eqPl (gPl ()) pl) $"{name} should not change the input"
                checkStatic  "rotateDegrees"        expectedOrigin (Polyline2D.rotateDegrees gDeg)
                checkStatic  "rotateRadians"        expectedOrigin (Polyline2D.rotateRadians gRad)
                checkInPlace "RotateDegrees"        expectedOrigin (fun pl -> pl.RotateDegrees gDeg)
                checkInPlace "RotateRadians"        expectedOrigin (fun pl -> pl.RotateRadians gRad)
                checkStatic  "rotateAround"         expectedAround (Polyline2D.rotateAround gCen gRot)
                checkStatic  "rotateAroundDegrees"  expectedAround (Polyline2D.rotateAroundDegrees gCen gDeg)
                checkStatic  "rotateAroundRadians"  expectedAround (Polyline2D.rotateAroundRadians gCen gRad)
                checkInPlace "RotateAround"         expectedAround (fun pl -> pl.RotateAround gCen gRot)
                checkInPlace "RotateAroundDegrees"  expectedAround (fun pl -> pl.RotateAroundDegrees gCen gDeg)
                checkInPlace "RotateAroundRadians"  expectedAround (fun pl -> pl.RotateAroundRadians gCen gRad)
            }

            test "Polyline2D rotateAroundDegrees with empty, single and duplicate points" {
                let empty = Polyline2D.createFromPts []
                let rotatedEmpty = Polyline2D.rotateAroundDegrees gCen gDeg empty
                Expect.equal rotatedEmpty.PointCount 0 "empty stays empty"
                let single = Polyline2D.createFromPts [gPt]
                single.RotateAroundDegrees gCen gDeg
                Expect.isTrue (eqPt (single.GetPoint 0) (refRotAround gCen gRot gPt)) "single point is rotated"
                let dup = Polyline2D.createFromPts [gPt; gPt]
                let rotatedDup = Polyline2D.rotateAroundRadians gCen gRad dup
                Expect.isTrue (eqPt (rotatedDup.GetPoint 0) (rotatedDup.GetPoint 1)) "duplicate points stay duplicates"
                Expect.isTrue (eqPt (dup.GetPoint 0) gPt) "input is not rotated"
            }

            test "Polyline2D.scaleOn returns a new one, ScaleOn changes it in place" {
                let expected = Polyline2D.createFromPts ((gPl ()).AsPoints |> Seq.map (Pt.scaleOn gCen 1.5))
                let pl = gPl ()
                Expect.isTrue (eqPl expected (Polyline2D.scaleOn gCen 1.5 pl)) "scaleOn"
                Expect.isTrue (eqPl (gPl ()) pl) "scaleOn should not change the input"
                let pl2 = gPl ()
                pl2.ScaleOn gCen 1.5
                Expect.isTrue (eqPl expected pl2) "ScaleOn"
            }
        ]
        test "Rotation2D.andThen is Then in a pipeline" {
            let a = Rotation2D.createFromDegrees 30.0
            let b = Rotation2D.createFromDegrees -75.0
            "same as Then" |> Expect.isTrue (Rotation2D.equalsExact (a |> Rotation2D.andThen b) (a.Then b))
            "angles add" |> Expect.isTrue (abs ((a |> Rotation2D.andThen b).AngleInDegrees - -45.0) < 1e-9)
        }
    ]
