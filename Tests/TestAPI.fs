module TestAPI

open Euclid

#nowarn "44" // Suppress warnings for obsolete members

#if !FABLE_COMPILER // do not run these tests under Fable

// A test that has the full API available of release 16
// This is a compile time only test to find any missing API surface and add them as obsolete members
// No runtime test is performed here


// ===== Vc (2D Vector) =====

module ApiVc =
    // Constructor
    let vc  = Vc(1., 2. )
    let v2 = Vc(3., 4.)

    // Fields
    let (_:float) = vc.X
    let (_:float) = vc.Y

    // Instance members
    let (_:string) = vc.ToString()
    let (_:string) = vc.AsString
    let (_:float) = vc.Length
    let (_:float) = vc.LengthSq
    let (_:Pt) = vc.AsPt
    let (_:Vec) = vc.AsVec
    let (_:Pnt) = vc.AsPnt
    let (_:bool) = vc.IsZero
    let (_:bool) = vc.IsNotZero
    let (_:bool) = vc.IsTiny 0.001
    let (_:bool) = vc.IsTinySq 0.001
    let (_:Vc) = vc.WithX 5.0
    let (_:Vc) = vc.WithY 5.0
    let (_:Vec) = vc.WithZ 5.0
    let (_:Vc) = vc.Half
    let (_:Vc) = vc.WithLength 5.0
    let (_:UnitVc) = vc.Unitized
    let (_:bool) = vc.IsUnit
    let (_:float) = vc.Cross v2
    let (_:float) = vc.Dot v2
    let (_:Vc) = vc.RotateBy (Rotation2D.createFromDegrees 45.0)
    let (_:Vc) = vc.Rotate 45.0
    let (_:Vc) = vc.Rotate90CCW
    let (_:Vc) = vc.Rotate90CW
    let (_:float) = vc.DirectionDiamond
    let (_:float) = vc.Direction2Pi
    let (_:float) = vc.DirectionPi
    let (_:float) = vc.Direction360
    let (_:float) = vc.Direction180
    let (_:float) = vc.AngleDiamondTo v2
    let (_:bool) = vc.MatchesOrientation v2
    let (_:bool) = vc.IsOppositeOrientation (-v2)
    let (_:bool) = vc.IsXAligned
    let (_:bool) = vc.IsYAligned
    let (_:bool) = vc.IsParallelTo v2
    let (_:bool) = vc.IsParallelAndOrientedTo v2
    let (_:bool) = vc.IsPerpendicularTo (vc.Rotate90CCW)

    // Operators
    let (_:Vc) = -vc                 // Negate
    let (_:Vc) = vc + v2             // Add
    let (_:Vc) = vc - v2             // Subtract
    let (_:Vc) = vc * 2.0            // Multiply vector by scalar
    let (_:Vc) = 2.0 * vc            // Multiply scalar by vector
    let (_:Vc) = vc / 2.0            // Divide by scalar
    let (_:float) = vc *** v2        // Dot product operator

    // Static members - Constants
    let (_:Vc) = Vc.Zero
    let (_:Vc) = Vc.Xaxis
    let (_:Vc) = Vc.Yaxis

    // Static members - Creation
    let (_:Vc) = Vc.create(1.0, 2.0)
    let (_:Vc) = Vc.create(Pt(0., 0.), Pt(1., 2.))
    let (_:Vc) = Vc.createFromPt(Pnt(1., 2., 3.))
    let (_:Vc) = Vc.createFromUnitVc(UnitVc.Xaxis)
    let (_:Vc) = Vc.createFromMembersXY (Pt(1., 2.))  // Pt has X and Y members
    // createFromMembersxy requires lowercase x/y members which Euclid types don't have

    // Static members - Conversion
    let (_:Pt) = Vc.asPt vc
    let (_:Pnt) = Vc.asPnt vc
    let (_:Vec) = Vc.asVec vc

    // Static members - Properties
    let (_:float) = Vc.getX vc
    let (_:float) = Vc.getY vc
    let (_:float) = Vc.length vc
    let (_:float) = Vc.lengthSq vc
    let (_:bool) = Vc.isTiny 0.001 vc
    let (_:bool) = Vc.isTinySq 0.001 vc

    // Static members - Modification
    let (_:Vc) = Vc.withX 5.0 vc
    let (_:Vc) = Vc.withY 5.0 vc
    let (_:Vc) = Vc.moveX 1.0 vc
    let (_:Vc) = Vc.moveY 1.0 vc
    let (_:Vc) = Vc.scale 2.0 vc
    let (_:Vc) = Vc.withLength 5.0 vc
    let (_:Vc) = Vc.add vc v2
    let (_:Vc) = Vc.reverse vc
    let (_:Vc) = Vc.flip vc
    let (_:UnitVc) = Vc.unitize vc
    let (_:UnitVc) = Vc.unitizeOrDefault UnitVc.Xaxis vc

    // Static members - Comparison
    let (_:bool) = Vc.equals 0.001 vc v2
    let (_:bool) = Vc.notEquals 0.001 vc v2
    let (_:float) = Vc.difference vc v2
    let (_:float) = Vc.differenceSq vc v2

    // Static members - Cross and Dot products
    let (_:float) = Vc.cross(vc, v2)
    let (_:float) = Vc.dot(vc, v2)

    // Static members - Angles
    let (_:float) = Vc.anglePi vc v2
    let (_:float) = Vc.angleHalfPi vc v2
    let (_:float) = Vc.angle2Pi(vc, v2)
    let (_:float) = Vc.angle90 vc v2
    let (_:float) = Vc.angle180 vc v2
    let (_:float) = Vc.angle360(vc, v2)
    let (_:float) = Vc.angleDiamond(vc, v2)
    let (_:float) = Vc.directionDiamond vc
    let (_:float) = Vc.direction2Pi vc
    let (_:float) = Vc.direction360 vc
    let (_:float) = Vc.slopeRadians vc
    let (_:float) = Vc.slopeDegrees vc
    let (_:float) = Vc.slopePercent vc
    let (_:float) = Vc.angleToXPi vc
    let (_:float) = Vc.angleToX180 vc

    // Static members - Orientation checks
    let (_:bool) = Vc.matchesOrientation vc v2
    let (_:bool) = Vc.isOppositeOrientation vc (-v2)
    let (_:bool) = Vc.areParallel v2 vc
    let (_:bool) = Vc.areParallelAndMatchOrientation v2 vc
    let (_:bool) = Vc.arePerpendicular (vc.Rotate90CCW) vc
    let (_:bool) = Vc.isXAligned (Vc.Xaxis)
    let (_:bool) = Vc.isYAligned (Vc.Yaxis)
    let (_:bool) = Vc.isAngle90Below Cosine.``45.0`` vc v2
    let (_:bool) = Vc.isAngle90Above Cosine.``45.0`` vc v2
    let (_:bool) = Vc.isAngle180Below Cosine.``45.0`` vc v2
    let (_:bool) = Vc.isAngle180Above Cosine.``45.0`` vc v2

    // Static members - Rotation
    let (_:Vc) = Vc.rotateBy (Rotation2D.createFromDegrees 45.0) vc
    let (_:Vc) = Vc.rotate 45.0 vc
    let (_:Vc) = Vc.rotate90CCW vc
    let (_:Vc) = Vc.rotate90CW vc
    let (_:Vc) = Vc.rotateByQuarterCircle 1 vc

    // Static members - Interpolation
    let (_:Vc) = Vc.lerp(vc, v2, 0.5)
    let (_:Vc) = Vc.slerp(vc, v2, 0.5)

    // Static members - Other
    let (_:Vc) = Vc.bisector vc v2
    let (_:Vc) = Vc.matchOrientation vc v2
    let (_:Vc) = Vc.matchUnitVcOrientation UnitVc.Xaxis vc
    let (_:Vc) = Vc.DivideByInt(vc, 2)

    // Static members - Intersection
    let (_:ValueOption<float*float>) = Vc.intersection(Pt(0., 0.), Pt(1., 0.), Vc(1., 1.), Vc(0., 1.))

// ===== UnitVc (2D Unit Vector) =====

module UnitVcAPI =
    // Creation
    let unitvc = UnitVc.create(1., 0.)
    let u2 = UnitVc.create(0., 1.)
    let u3 = UnitVc.create(1., 1.)

    // Fields
    let (_:float) = unitvc.X
    let (_:float) = unitvc.Y

    // Instance members - String
    let (_:string) = unitvc.ToString()
    let (_:string) = unitvc.AsString

    // Instance members - Conversion
    let (_:Pt) = unitvc.AsPt
    let (_:Vc) = unitvc.AsVc
    let (_:Vec) = unitvc.AsVec
    let (_:UnitVec) = unitvc.AsUnitVec
    let (_:Pnt) = unitvc.AsPnt
    let (_:Vc) = unitvc.WithX 2.0
    let (_:Vc) = unitvc.WithY 2.0
    let (_:Vec) = unitvc.WithZ 2.0

    // Instance members - Products
    let (_:float) = unitvc.Cross u2
    let (_:float) = unitvc.Cross (Vc(1., 2.))
    let (_:float) = unitvc.Dot u2
    let (_:float) = unitvc.Dot (Vc(1., 2.))
    let (_:float<Cosine.cosine>) = unitvc.DotCosine u2

    // Instance members - Rotation
    let (_:UnitVc) = unitvc.RotateBy (Rotation2D.createFromDegrees 45.0)
    let (_:UnitVc) = unitvc.Rotate 45.0
    let (_:UnitVc) = unitvc.Rotate90CCW
    let (_:UnitVc) = unitvc.Rotate90CW

    // Instance members - Direction/Angle
    let (_:float) = unitvc.DirectionDiamond
    let (_:float) = unitvc.Direction2Pi
    let (_:float) = unitvc.DirectionPi
    let (_:float) = unitvc.Direction360
    let (_:float) = unitvc.Direction180
    let (_:float) = unitvc.AngleDiamondTo u2

    // Instance members - Orientation checks
    let (_:bool) = unitvc.MatchesOrientation u2
    let (_:bool) = unitvc.MatchesOrientation (Vc(1., 2.))
    let (_:bool) = unitvc.IsOppositeOrientation (-u2)
    let (_:bool) = unitvc.IsOppositeOrientation (Vc(-1., -2.))
    let (_:bool) = unitvc.IsXAligned
    let (_:bool) = unitvc.IsYAligned
    let (_:bool) = unitvc.IsParallelTo u2
    let (_:bool) = unitvc.IsParallelTo (Vc(1., 0.))
    let (_:bool) = unitvc.IsParallelAndOrientedTo u2
    let (_:bool) = unitvc.IsParallelAndOrientedTo (Vc(1., 0.))
    let (_:bool) = unitvc.IsPerpendicularTo u2
    let (_:bool) = unitvc.IsPerpendicularTo (Vc(0., 1.))

    // Operators
    let (_:UnitVc) = -unitvc                     // Negate
    let (_:Vc) = unitvc + u2                     // Add unit + unit
    let (_:Vc) = (Vc(1., 2.)) + unitvc           // Add Vc + unit
    let (_:Vc) = unitvc + (Vc(1., 2.))           // Add unit + Vc
    let (_:Vc) = unitvc - u2                     // Subtract unit - unit
    let (_:Vc) = (Vc(1., 2.)) - unitvc           // Subtract Vc - unit
    let (_:Vc) = unitvc - (Vc(1., 2.))           // Subtract unit - Vc
    let (_:Vc) = unitvc * 2.0                    // Multiply unit by scalar
    let (_:Vc) = 2.0 * unitvc                    // Multiply scalar by unit
    let (_:Vc) = unitvc / 2.0                    // Divide by scalar
    let (_:float) = unitvc *** u2                // Dot product (unit *** unit)
    let (_:float) = unitvc *** (Vc(1., 2.))      // Dot product (unit *** Vc)
    let (_:float) = (Vc(1., 2.)) *** unitvc      // Dot product (Vc *** unit)

    // Static members - Creation
    let (_:UnitVc) = UnitVc.create(1.0, 0.0)
    let (_:UnitVc) = UnitVc.create(Pt(0., 0.), Pt(1., 0.))
    let (_:UnitVc) = UnitVc.createUnchecked(1.0, 0.0)
    // let (_:UnitVc) = UnitVc.createUnchecked(Vc(1., 0.))
    let (_:UnitVc) = UnitVc.createFromPt(Pt(1., 0.))
    let (_:UnitVc) = UnitVc.createFromVec(Vc(1., 0.))
    let (_:UnitVc) = UnitVc.createFromMembersXY (Pt(1., 0.))  // Pt has X and Y members
    // createFromMembersxy requires lowercase x/y members which Euclid types don't have

    // Static members - Constants
    let (_:UnitVc) = UnitVc.Xaxis
    let (_:UnitVc) = UnitVc.Yaxis

    // Static members - Conversion
    let (_:Pt) = UnitVc.asPt unitvc
    let (_:Vec) = UnitVc.asVec unitvc
    let (_:UnitVec) = UnitVc.asUnitVec unitvc
    let (_:Pnt) = UnitVc.asPnt unitvc

    // Static members - Properties
    let (_:float) = UnitVc.getX unitvc
    let (_:float) = UnitVc.getY unitvc

    // Static members - Modification
    let (_:Vc) = UnitVc.withX 2.0 unitvc
    let (_:Vc) = UnitVc.withY 2.0 unitvc
    let (_:Vec) = UnitVc.withZ 2.0 unitvc
    let (_:Vc) = UnitVc.add unitvc u2
    let (_:Vc) = UnitVc.scale 2.0 unitvc
    let (_:Vc) = UnitVc.withLength 5.0 unitvc
    let (_:Vc) = UnitVc.moveX 1.0 unitvc
    let (_:Vc) = UnitVc.moveY 1.0 unitvc
    let (_:UnitVc) = UnitVc.flip unitvc
    let (_:UnitVc) = UnitVc.reverse unitvc

    // Static members - Comparison
    let (_:bool) = UnitVc.equals 0.001 unitvc u2
    let (_:bool) = UnitVc.notEquals 0.001 unitvc u2
    let (_:float) = UnitVc.difference unitvc u2
    let (_:float) = UnitVc.differenceSq unitvc u2

    // Static members - Cross and Dot products
    let (_:float) = UnitVc.cross(unitvc, u2)
    let (_:float) = UnitVc.cross(unitvc, Vc(1., 2.))
    let (_:float) = UnitVc.cross(Vc(1., 2.), unitvc)
    let (_:float) = UnitVc.dot(unitvc, u2)
    let (_:float) = UnitVc.dot(unitvc, Vc(1., 2.))
    let (_:float) = UnitVc.dot(Vc(1., 2.), unitvc)
    let (_:float<Cosine.cosine>) = UnitVc.dotCosine unitvc u2

    // Static members - Angles
    let (_:float) = UnitVc.anglePi unitvc u2
    let (_:float) = UnitVc.angleHalfPi unitvc u2
    let (_:float) = UnitVc.angle2Pi(unitvc, u2)
    let (_:float) = UnitVc.angle90 unitvc u2
    let (_:float) = UnitVc.angle180 unitvc u2
    let (_:float) = UnitVc.angle360(unitvc, u2)
    let (_:float) = UnitVc.angleDiamond(unitvc, u2)
    let (_:float) = UnitVc.directionDiamond unitvc
    let (_:float) = UnitVc.direction2Pi unitvc
    let (_:float) = UnitVc.direction360 unitvc
    let (_:float) = UnitVc.slopeRadians unitvc
    let (_:float) = UnitVc.slopeDegree unitvc
    let (_:float) = UnitVc.slopePercent unitvc
    let (_:float) = UnitVc.angleToXPi unitvc
    let (_:float) = UnitVc.angleToX180 unitvc

    // Static members - Orientation checks
    let (_:bool) = UnitVc.matchesOrientation u2 unitvc
    let (_:bool) = UnitVc.isOppositeOrientation (-u2) unitvc
    let (_:bool) = UnitVc.areParallel u2 unitvc
    let (_:bool) = UnitVc.areParallelAndMatchOrientation u2 unitvc
    let (_:bool) = UnitVc.arePerpendicular u2 unitvc
    let (_:bool) = UnitVc.isXAligned unitvc
    let (_:bool) = UnitVc.isYAligned u2
    let (_:bool) = UnitVc.isAngle90Below Cosine.``45.0`` unitvc u2
    let (_:bool) = UnitVc.isAngle90Above Cosine.``45.0`` unitvc u2
    let (_:bool) = UnitVc.isAngle180Below Cosine.``45.0`` unitvc u2
    let (_:bool) = UnitVc.isAngle180Above Cosine.``45.0`` unitvc u2

    // Static members - Rotation
    let (_:UnitVc) = UnitVc.rotateBy (Rotation2D.createFromDegrees 45.0) unitvc
    let (_:UnitVc) = UnitVc.rotate 45.0 unitvc
    let (_:UnitVc) = UnitVc.rotate90CCW unitvc
    let (_:UnitVc) = UnitVc.rotate90CW unitvc
    let (_:UnitVc) = UnitVc.rotateByQuarterCircle 1 unitvc

    // Static members - Interpolation
    let (_:Vc) = UnitVc.lerp(unitvc, u2, 0.5)
    let (_:UnitVc) = UnitVc.slerp(unitvc, u3, 0.5)

    // Static members - Orientation matching
    let (_:UnitVc) = UnitVc.matchOrientation unitvc u2
    let (_:UnitVc) = UnitVc.matchVcOrientation (Vc(1., 0.)) u2

    // Static members - Intersection
    let (_:ValueOption<float*float>) = UnitVc.intersection(Pt(0., 0.), Pt(1., 0.), UnitVc.Xaxis, UnitVc.Yaxis)

// ===== Pt (2D Point) =====

module PtAPI =
    // Constructor
    let pt = Pt(1., 2.)
    let p2 = Pt(3., 4.)

    // Fields
    let (_:float) = pt.X
    let (_:float) = pt.Y

    // Instance members - String
    let (_:string) = pt.ToString()
    let (_:string) = pt.AsString

    // Instance members - Conversion
    let (_:Vc) = pt.AsVc
    let (_:Vec) = pt.AsVec
    let (_:Pnt) = pt.AsPnt
    let (_:Pt) = pt.WithX 5.0
    let (_:Pt) = pt.WithY 5.0
    let (_:Pnt) = pt.WithZ 5.0

    // Instance members - Properties
    let (_:bool) = pt.IsOrigin
    let (_:bool) = pt.IsNotOrigin
    let (_:bool) = pt.IsAlmostOrigin 0.001

    // Instance members - Distance
    let (_:float) = pt.DistanceTo p2
    let (_:float) = pt.DistanceToSquare p2
    let (_:float) = pt.DistanceFromOrigin
    let (_:float) = pt.DistanceFromOriginSquare
    let (_:Pt) = pt.WithDistanceFromOrigin 5.0

    // Instance members - Angles
    let (_:float) = pt.DirectionDiamondTo p2
    let (_:float) = pt.Angle2PiTo p2
    let (_:float) = pt.Angle360To p2

    // Instance members - Line operations only used on deprecated Loop
    // let (_:Pt) = pt.ClosestPointOnLine(Pt.Origin, p2)
    // let (_:Pt) = pt.ClosestPointOnLine(Pt.Origin, UnitVc.Xaxis, 10.0)
    // let (_:float) = pt.DistanceToLine(Pt.Origin, p2)
    // let (_:float) = pt.DistanceToLine(Pt.Origin, UnitVc.Xaxis, 10.0)
    // let (_:float) = pt.DistanceToLineSquare(Pt.Origin, UnitVc.Xaxis, 10.0)

    // Operators
    let (_:Vc) = pt - p2                     // Subtract points → vector
    let (_:Pt) = pt - Vc(1., 2.)             // Subtract vector from point
    let (_:Pt) = pt - UnitVc.Xaxis           // Subtract unit vector from point
    let (_:Pt) = pt + Vc(1., 2.)             // Add vector to point
    let (_:Pt) = pt + UnitVc.Xaxis           // Add unit vector to point
    let (_:Pt) = pt + p2                     // Add points
    let (_:Pt) = pt * 2.0                    // Multiply point by scalar
    let (_:Pt) = 2.0 * pt                    // Multiply scalar by point
    let (_:Pt) = pt / 2.0                    // Divide by scalar

    // Static members - Constants
    let (_:Pt) = Pt.Zero
    let (_:Pt) = Pt.Origin

    // Static members - Creation
    let (_:Pt) = Pt.create(1.0, 2.0)
    let (_:Pt) = Pt.createFromPnt(Pnt(1., 2., 3.))
    let (_:Pt) = Pt.createFromVc(Vc(1., 2.))
    let (_:Pt) = Pt.createFromUnitVc(UnitVc.Xaxis)
    let (_:Pt) = Pt.createFromMembersXY (Vc(1., 2.))  // Vc has X and Y members
    // createFromMembersxy requires lowercase x/y members which Euclid types don't have

    // Static members - Properties
    let (_:float) = Pt.getX pt
    let (_:float) = Pt.getY pt
    let (_:Pt) = Pt.withX 5.0 pt
    let (_:Pt) = Pt.withY 5.0 pt

    // Static members - Operations
    let (_:Pt) = Pt.add pt p2
    let (_:Pt) = Pt.addVc (Vc(1., 2.)) pt
    let (_:Pt) = Pt.midPt pt p2
    let (_:Pt) = Pt.scale 2.0 pt
    let (_:Pt) = Pt.translate (Vc(1., 2.)) pt
    let (_:Pt) = Pt.move (Vc(1., 2.)) pt
    let (_:Pt) = Pt.moveX 1.0 pt
    let (_:Pt) = Pt.moveY 1.0 pt

    // Static members - Comparison
    let (_:bool) = Pt.equals 0.001 pt p2
    let (_:bool) = Pt.notEquals 0.001 pt p2
    let (_:float) = Pt.distance pt p2
    let (_:float) = Pt.distanceSq pt p2
    let (_:float) = Pt.distanceFromOrigin pt
    let (_:float) = Pt.distanceFromOriginSquare pt
    let (_:Pt) = Pt.setDistanceFromOrigin 5.0 pt

    // Static members - Angles
    let (_:float) = Pt.anglePiPts(pt, Pt.Origin, p2)
    let (_:float) = Pt.angle180Pts(pt, Pt.Origin, p2)
    let (_:Vc) = Pt.bisector(pt, Pt.Origin, p2)

    // Static members - Rotation
    let (_:Pt) = Pt.rotateBy (Rotation2D.createFromDegrees 45.0) pt
    let (_:Pt) = Pt.rotate 45.0 pt
    let (_:Pt) = Pt.rotateWithCenterBy Pt.Origin (Rotation2D.createFromDegrees 45.0) pt
    let (_:Pt) = Pt.rotateWithCenter Pt.Origin 45.0 pt

    // Static members - Interpolation
    let (_:Pt) = Pt.divPt(pt, p2, 0.5)
    let (_:Pt) = Pt.lerp(pt, p2, 0.5)
    let (_:Pt) = Pt.distPt(pt, p2, 5.0)
    let (_:Pt) = Pt.snapIfClose 0.1 pt p2

    // Static members - Other
    let (_:Pt) = Pt.DivideByInt(pt, 2)

// ===== Pnt (3D Point) =====

module PntAPI =
    // Constructor
    let pnt = Pnt(1., 2., 3.)
    let p2 = Pnt(4., 5., 6.)

    // Fields
    let (_:float) = pnt.X
    let (_:float) = pnt.Y
    let (_:float) = pnt.Z

    // Instance members - String
    let (_:string) = pnt.ToString()
    let (_:string) = pnt.AsString

    // Instance members - Conversion
    let (_:Vec) = pnt.AsVec
    let (_:Pt) = pnt.AsPt
    let (_:Pnt) = pnt.WithX 5.0
    let (_:Pnt) = pnt.WithY 5.0
    let (_:Pnt) = pnt.WithZ 5.0

    // Instance members - Properties
    let (_:bool) = pnt.IsOrigin
    let (_:bool) = pnt.IsNotOrigin
    let (_:bool) = pnt.IsAlmostOrigin 0.001

    // Instance members - Distance
    let (_:float) = pnt.DistanceTo p2
    let (_:float) = pnt.DistanceToSquare p2
    let (_:float) = pnt.DistanceFromOrigin
    let (_:float) = pnt.DistanceFromOriginSquare
    let (_:float) = pnt.DistanceInXYFromOrigin
    let (_:float) = pnt.DistanceInXYFromOriginSquare
    let (_:Pnt) = pnt.WithDistanceFromOrigin 5.0

    // Instance members - Angles
    let (_:float) = pnt.DirectionDiamondInXYTo p2
    let (_:float) = pnt.Angle2PiInXYTo p2
    let (_:float) = pnt.Angle360InXYTo p2

    // Instance members - Line operations , only used on deprecated Loop
    // let (_:Pnt) = pnt.ClosestPointOnLine(Pnt.Origin, p2)
    // let (_:Pnt) = pnt.ClosestPointOnLine(Pnt.Origin, UnitVec.Xaxis, 10.0)
    // let (_:float) = pnt.DistanceToLine(Pnt.Origin, p2)
    // let (_:float) = pnt.DistanceToLine(Pnt.Origin, UnitVec.Xaxis, 10.0)
    // let (_:float) = pnt.DistanceToLineSquare(Pnt.Origin, UnitVec.Xaxis, 10.0)

    // Operators
    let (_:Vec) = pnt - p2                    // Subtract points → vector
    let (_:Pnt) = pnt - Vec(1., 2., 3.)       // Subtract vector from point
    let (_:Pnt) = pnt - UnitVec.Xaxis         // Subtract unit vector from point
    let (_:Pnt) = pnt + Vec(1., 2., 3.)       // Add vector to point
    let (_:Pnt) = pnt + UnitVec.Xaxis         // Add unit vector to point
    let (_:Pnt) = pnt + p2                    // Add points
    let (_:Pnt) = pnt * 2.0                   // Multiply point by scalar
    let (_:Pnt) = 2.0 * pnt                   // Multiply scalar by point
    let (_:Pnt) = pnt / 2.0                   // Divide by scalar

    // Static members - Constants
    let (_:Pnt) = Pnt.Zero
    let (_:Pnt) = Pnt.Origin

    // Static members - Creation
    let (_:Pnt) = Pnt.create(1.0, 2.0, 3.0)
    let (_:Pnt) = Pnt.createFromPt(Pt(1., 2.))
    let (_:Pnt) = Pnt.createFromVec(Vec(1., 2., 3.))
    let (_:Pnt) = Pnt.createFromUnitVec(UnitVec.Xaxis)
    let (_:Pnt) = Pnt.createFromPtWithZ 5.0 (Pt(1., 2.))
    let (_:Pnt) = Pnt.projectToXYPlane pnt
    let (_:Pnt) = Pnt.createFromMembersXYZ (Vec(1., 2., 3.))  // Vec has X, Y and Z members
    // createFromMembersxyz requires lowercase x/y/z members which Euclid types don't have

    // Static members - Properties
    let (_:float) = Pnt.getX pnt
    let (_:float) = Pnt.getY pnt
    let (_:float) = Pnt.getZ pnt
    let (_:Pnt) = Pnt.withX 5.0 pnt
    let (_:Pnt) = Pnt.withY 5.0 pnt
    let (_:Pnt) = Pnt.withZ 5.0 pnt

    // Static members - Operations
    let (_:Pnt) = Pnt.add pnt p2
    let (_:Pnt) = Pnt.addVec (Vec(1., 2., 3.)) pnt
    let (_:Pnt) = Pnt.midPt pnt p2
    let (_:Pnt) = Pnt.scale 2.0 pnt
    let (_:Pnt) = Pnt.translate (Vec(1., 2., 3.)) pnt
    let (_:Pnt) = Pnt.move (Vec(1., 2., 3.)) pnt
    let (_:Pnt) = Pnt.moveX 1.0 pnt
    let (_:Pnt) = Pnt.moveY 1.0 pnt
    let (_:Pnt) = Pnt.moveZ 1.0 pnt

    // Static members - Comparison
    let (_:bool) = Pnt.equals 0.001 pnt p2
    let (_:bool) = Pnt.notEquals 0.001 pnt p2
    let (_:float) = Pnt.distance pnt p2
    let (_:float) = Pnt.distanceSq pnt p2
    let (_:float) = Pnt.distanceXY pnt p2
    let (_:float) = Pnt.distanceFromOrigin pnt
    let (_:float) = Pnt.distanceFromOriginSquare pnt
    let (_:Pnt) = Pnt.setDistanceFromOrigin 5.0 pnt

    // Static members - Angles
    let (_:float) = Pnt.anglePiPts(pnt, Pnt.Origin, p2)
    let (_:float) = Pnt.angle180Pts(pnt, Pnt.Origin, p2)
    let (_:Vec) = Pnt.bisector(pnt, Pnt.Origin, p2)
    let (_:Vec) = Pnt.normalOf3Pts(pnt, Pnt.Origin, p2)

    // Static members - Interpolation
    let (_:Pnt) = Pnt.divPt(pnt, p2, 0.5)
    let (_:Pnt) = Pnt.lerp(pnt, p2, 0.5)
    let (_:Pnt) = Pnt.distPt(pnt, p2, 5.0)
    let (_:Pnt) = Pnt.snapIfClose 0.1 pnt p2

    // Static members - Other
    let (_:Pnt) = Pnt.DivideByInt(pnt, 2)
    let (_:Vec) = Pnt.normalOfTwoPointsInXY(pnt, p2)

    // Static members - Rotation (additional)
    let (_:Pnt) = Pnt.rotateX 45.0 pnt
    let (_:Pnt) = Pnt.rotateY 45.0 pnt
    let (_:Pnt) = Pnt.rotateZ 45.0 pnt
    let (_:Pnt) = Pnt.rotateXBy (Rotation2D.createFromDegrees 45.0) pnt
    let (_:Pnt) = Pnt.rotateYBy (Rotation2D.createFromDegrees 45.0) pnt
    let (_:Pnt) = Pnt.rotateZBy (Rotation2D.createFromDegrees 45.0) pnt
    let (_:Pnt) = Pnt.rotateXwithCenter Pnt.Origin 45.0 pnt
    let (_:Pnt) = Pnt.rotateYwithCenter Pnt.Origin 45.0 pnt
    let (_:Pnt) = Pnt.rotateZwithCenter Pnt.Origin 45.0 pnt
    let (_:Pnt) = Pnt.rotateXwithCenterBy Pnt.Origin (Rotation2D.createFromDegrees 45.0) pnt
    let (_:Pnt) = Pnt.rotateYwithCenterBy Pnt.Origin (Rotation2D.createFromDegrees 45.0) pnt
    let (_:Pnt) = Pnt.rotateZwithCenterBy Pnt.Origin (Rotation2D.createFromDegrees 45.0) pnt
    let (_:Pnt) = Pnt.rotateByQuaternion (Quaternion.createFromDegree(UnitVec.Zaxis, 45.0)) pnt
    let (_:Pnt) = Pnt.rotateWithCenterByQuat Pnt.Origin (Quaternion.createFromDegree(UnitVec.Zaxis, 45.0)) pnt

    // Static members - Other (additional)
    let (_:Pnt) = Pnt.snap 0.1 pnt
    let (_:Pnt) = Pnt.extendToZLevel (pnt, p2, 5.0)
    let (_:float) = Pnt.projectedParameter (pnt, UnitVec.Xaxis, p2)
    let (_:float) = Pnt.projectedParameter (pnt, Vec(1., 0., 0.), p2)
    let (_:float) = Pnt.projectedParameter (pnt, p2, Pnt(5., 5., 5.))
    let (_:float) = Pnt.angleInCorner (pnt, Pnt.Origin, p2)

    // Static members - Transform
    let rm = RigidMatrix.identity
    let (_:Pnt) = pnt.TransformRigid rm
    let (_:Pnt) = pnt.TransformRigidRotateOnly rm
    let (_:Pnt) = Pnt.transformRigid rm pnt
    let (_:Pnt) = Pnt.transformRigidRotateOnly rm pnt
    let (_:Pnt*Pnt) = Pnt.offsetTwoPt(pnt, p2, 1.0, 0.5)

// ===== Vec (3D Vector) =====

module VecAPI =
    // Constructor
    let vec = Vec(1., 2., 3.)
    let v2 = Vec(4., 5., 6.)

    // Fields
    let (_:float) = vec.X
    let (_:float) = vec.Y
    let (_:float) = vec.Z

    // Instance members - String
    let (_:string) = vec.ToString()
    let (_:string) = vec.AsString

    // Instance members - Properties
    let (_:float) = vec.Length
    let (_:float) = vec.LengthSq
    let (_:float) = vec.LengthInXY
    let (_:float) = vec.LengthSqInXY
    let (_:bool) = vec.IsZero
    let (_:bool) = vec.IsNotZero
    let (_:bool) = vec.IsTiny 0.001
    let (_:bool) = vec.IsTinySq 0.001
    let (_:bool) = vec.IsUnit

    // Instance members - Conversion
    let (_:Pnt) = vec.AsPnt
    let (_:Vc) = vec.AsVc
    let (_:Vec) = vec.WithX 5.0
    let (_:Vec) = vec.WithY 5.0
    let (_:Vec) = vec.WithZ 5.0
    let (_:Vec) = vec.Half
    let (_:Vec) = vec.WithLength 5.0
    let (_:UnitVec) = vec.Unitized

    // Instance members - Products
    let (_:Vec) = vec.Cross v2
    let (_:Vec) = vec.Cross UnitVec.Xaxis
    let (_:float) = vec.Dot v2
    let (_:float) = vec.Dot UnitVec.Xaxis

    // Instance members - Perpendicular/Rotation
    let (_:Vec) = vec.PerpendicularInXY
    let (_:Vec) = vec.RotateOnZ90CCW
    let (_:Vec) = vec.RotateOnZ90CW

    // Instance members - Direction/Angles
    let (_:float) = vec.DirectionDiamondInXY
    let (_:float) = vec.Direction2PiInXY
    let (_:float) = vec.DirectionPiInXY
    let (_:float) = vec.Direction360InXY
    let (_:float) = vec.Direction180InXY
    let (_:float) = vec.AngleDiamondInXYTo v2

    // Instance members - Orientation checks
    let (_:bool) = vec.MatchesOrientation v2
    let (_:bool) = vec.MatchesOrientation UnitVec.Xaxis
    let (_:bool) = vec.IsOppositeOrientation (-v2)
    let (_:bool) = vec.IsOppositeOrientation (-UnitVec.Xaxis)
    let (_:bool) = vec.IsXAligned
    let (_:bool) = vec.IsYAligned
    let (_:bool) = vec.IsZAligned
    let (_:bool) = vec.IsVertical
    let (_:bool) = vec.IsHorizontal
    let (_:bool) = vec.IsParallelTo v2
    let (_:bool) = vec.IsParallelTo UnitVec.Xaxis
    let (_:bool) = vec.IsParallelAndOrientedTo v2
    let (_:bool) = vec.IsParallelAndOrientedTo UnitVec.Xaxis
    let (_:bool) = vec.IsPerpendicularTo (Vec(0., 0., 1.))
    let (_:bool) = vec.IsPerpendicularTo UnitVec.Zaxis

    // Operators
    let (_:Vec) = -vec                        // Negate
    let (_:Vec) = vec + v2                    // Add
    let (_:Vec) = vec - v2                    // Subtract
    let (_:Vec) = vec * 2.0                   // Multiply vector by scalar
    let (_:Vec) = 2.0 * vec                   // Multiply scalar by vector
    let (_:Vec) = vec / 2.0                   // Divide by scalar
    let (_:float) = vec *** v2                // Dot product

    // Static members - Constants
    let (_:Vec) = Vec.Zero
    let (_:Vec) = Vec.Xaxis
    let (_:Vec) = Vec.Yaxis
    let (_:Vec) = Vec.Zaxis

    // Static members - Creation
    let (_:Vec) = Vec.createFromPnt(Pnt(1., 2., 3.))
    let (_:Vec) = Vec.createFromUnitVec(UnitVec.Xaxis)
    let (_:Vec) = Vec.createFromMembersXYZ (Pnt(1., 2., 3.))  // Pnt has X, Y and Z members
    // createFromMembersxyz requires lowercase x/y/z members which Euclid types don't have

    // Static members - Conversion
    let (_:Pt) = Vec.asPt vec
    let (_:Vc) = Vec.asVc vec
    let (_:Pnt) = Vec.asPnt vec

    // Static members - Comparison
    let (_:bool) = Vec.equals 0.001 vec v2
    let (_:bool) = Vec.notEquals 0.001 vec v2
    let (_:float) = Vec.difference vec v2
    let (_:float) = Vec.differenceSq vec v2

    // Static members - Products
    let (_:Vec) = Vec.cross(vec, v2)
    let (_:float) = Vec.dot(vec, v2)

    // Static members - Other
    let (_:Vec) = Vec.DivideByInt(vec, 2)

    // Static members - Creation (additional)
    let (_:Vec) = Vec.create(1.0, 2.0, 3.0)
    let (_:Vec) = Vec.create(Pnt(0., 0., 0.), Pnt(1., 2., 3.))
    let (_:Vec) = Vec.createFromVcWithZ 5.0 (Vc(1., 2.))

    // Static members - Properties
    let (_:float) = Vec.getX vec
    let (_:float) = Vec.getY vec
    let (_:float) = Vec.getZ vec
    let (_:float) = Vec.length vec
    let (_:float) = Vec.lengthSq vec
    let (_:float) = Vec.lengthInXY vec
    let (_:bool) = Vec.isTiny 0.001 vec
    let (_:bool) = Vec.isTinySq 0.001 vec

    // Static members - Modification
    let (_:Vec) = Vec.withX 5.0 vec
    let (_:Vec) = Vec.withY 5.0 vec
    let (_:Vec) = Vec.withZ 5.0 vec
    let (_:Vec) = Vec.moveX 1.0 vec
    let (_:Vec) = Vec.moveY 1.0 vec
    let (_:Vec) = Vec.moveZ 1.0 vec
    let (_:Vec) = Vec.scale 2.0 vec
    let (_:Vec) = Vec.withLength 5.0 vec
    let (_:Vec) = Vec.add vec v2
    let (_:Vec) = Vec.reverse vec
    let (_:Vec) = Vec.flip vec
    let (_:Vec) = Vec.projectToXYPlane vec
    let (_:UnitVec) = Vec.unitize vec
    let (_:UnitVec) = Vec.unitizeOrDefault UnitVec.Xaxis vec

    // Static members - Comparison
    let (_:bool) = Vec.equals 0.001 vec v2
    let (_:bool) = Vec.notEquals 0.001 vec v2
    let (_:float) = Vec.difference vec v2
    let (_:float) = Vec.differenceSq vec v2

    // Static members - Conversion
    let (_:Pt) = Vec.asPt vec
    let (_:Vc) = Vec.asVc vec
    let (_:Pnt) = Vec.asPnt vec

    // Static members - Angles
    let (_:float) = Vec.anglePi vec v2
    let (_:float) = Vec.angleHalfPi vec v2
    let (_:float) = Vec.angle180 vec v2
    let (_:float) = Vec.angle90 vec v2
    let (_:float) = Vec.angle2PiInXY(vec, v2)
    let (_:float) = Vec.angle360InXY(vec, v2)
    let (_:float) = Vec.angleDiamondInXY(vec, v2)
    let (_:float) = Vec.directionDiamondInXY vec
    let (_:float) = Vec.direction2PiInXY vec
    let (_:float) = Vec.direction360InXY vec
    let (_:float) = Vec.slopeRadians vec
    let (_:float) = Vec.slopeDegrees vec
    let (_:float) = Vec.slopePercent vec
    let (_:float) = Vec.determinant(vec, v2, Vec.Zaxis)

    // Static members - Orientation checks
    let (_:bool) = Vec.matchesOrientation vec v2
    let (_:bool) = Vec.isOppositeOrientation vec (-v2)
    let (_:bool) = Vec.areParallel v2 vec
    let (_:bool) = Vec.areParallelAndMatchOrientation v2 vec
    let (_:bool) = Vec.arePerpendicular Vec.Zaxis vec
    let (_:bool) = Vec.isXAligned Vec.Xaxis
    let (_:bool) = Vec.isYAligned Vec.Yaxis
    let (_:bool) = Vec.isZAligned Vec.Zaxis
    let (_:bool) = Vec.isVertical Vec.Zaxis
    let (_:bool) = Vec.isHorizontal Vec.Xaxis
    let (_:bool) = Vec.isAngle90Below Cosine.``45.0`` vec v2
    let (_:bool) = Vec.isAngle90Above Cosine.``45.0`` vec v2
    let (_:bool) = Vec.isAngle180Below Cosine.``45.0`` vec v2
    let (_:bool) = Vec.isAngle180Above Cosine.``45.0`` vec v2

    // Static members - Rotation
    let (_:Vec) = Vec.rotateXBy (Rotation2D.createFromDegrees 45.0) vec
    let (_:Vec) = Vec.rotateYBy (Rotation2D.createFromDegrees 45.0) vec
    let (_:Vec) = Vec.rotateZBy (Rotation2D.createFromDegrees 45.0) vec
    let (_:Vec) = Vec.rotateX 45.0 vec
    let (_:Vec) = Vec.rotateY 45.0 vec
    let (_:Vec) = Vec.rotateZ 45.0 vec
    let (_:Vec) = Vec.rotateOnZ90CCW vec
    let (_:Vec) = Vec.rotateOnZ90CW vec
    let (_:Vec) = Vec.rotateByQuaternion (Quaternion.createFromDegree(UnitVec.Zaxis, 45.0)) vec
    let (_:Vec) = Vec.perpendicularInXY vec
    let (_:Vec) = Vec.perpendicularInVerticalPlane vec
    let (_:Vec) = Vec.orientUp vec
    let (_:Vec) = Vec.orientDown vec
    let (_:Vec) = Vec.flipToPointUp vec

    // Static members - Interpolation
    let (_:Vec) = Vec.lerp(vec, v2, 0.5)
    let (_:Vec) = Vec.slerp(vec, v2, 0.5)

    // Static members - Other
    let (_:Vec) = Vec.bisector vec v2
    let (_:Vec) = Vec.matchOrientation vec v2
    let (_:Vec) = Vec.matchUnitVecOrientation UnitVec.Xaxis vec

    // Static members - Cross and Dot with UnitVec
    let (_:Vec) = Vec.cross(vec, UnitVec.Xaxis)
    let (_:Vec) = Vec.cross(UnitVec.Xaxis, vec)
    let (_:float) = Vec.dot(vec, UnitVec.Xaxis)
    let (_:float) = Vec.dot(UnitVec.Xaxis, vec)

    // Static members - Intersection
    let (_:ValueOption<float*float>) = Vec.intersection(Pnt(0., 0., 0.), Pnt(1., 0., 0.), Vec(1., 1., 0.), Vec(0., 1., 0.))

    // Static members - Transform
    let rm = RigidMatrix.identity
    let (_:Vec) = vec.TransformRigid rm
    let (_:Vec) = Vec.transformRigid rm vec

// ===== UnitVec (3D Unit Vector) =====

module UnitVecAPI =
    // Creation
    let unitvec = UnitVec.create(1., 0., 0.)
    let u2 = UnitVec.create(0., 1., 0.)
    let u3 = UnitVec.create(0., 0., 1.)

    // Fields
    let (_:float) = unitvec.X
    let (_:float) = unitvec.Y
    let (_:float) = unitvec.Z

    // Instance members - String
    let (_:string) = unitvec.ToString()
    let (_:string) = unitvec.AsString

    // Instance members - Conversion
    let (_:Vec) = unitvec.AsVec
    let (_:Pnt) = unitvec.AsPnt
    let (_:Vc) = unitvec.AsVc
    let (_:float) = unitvec.LengthInXY
    let (_:float) = unitvec.LengthSqInXY
    let (_:Vec) = unitvec.WithX 2.0
    let (_:Vec) = unitvec.WithY 2.0
    let (_:Vec) = unitvec.WithZ 2.0

    // Instance members - Products
    let (_:Vec) = unitvec.Cross u2
    let (_:Vec) = unitvec.Cross (Vec(1., 2., 3.))
    let (_:float) = unitvec.Dot u2
    let (_:float) = unitvec.Dot (Vec(1., 2., 3.))
    let (_:float<Cosine.cosine>) = unitvec.DotCosine u2

    // Instance members - Perpendicular/Rotation
    let (_:Vec) = unitvec.PerpendicularInXY
    let (_:UnitVec) = unitvec.RotateOnZ90CCW
    let (_:UnitVec) = unitvec.RotateOnZ90CW

    // Instance members - Direction/Angles
    let (_:float) = unitvec.DirectionDiamondInXY
    let (_:float) = unitvec.Direction2PiInXY
    let (_:float) = unitvec.DirectionPiInXY
    let (_:float) = unitvec.Direction360InXY
    let (_:float) = unitvec.Direction180InXY
    let (_:float) = unitvec.AngleDiamondInXYTo u2

    // Instance members - Orientation checks
    let (_:bool) = unitvec.MatchesOrientation u2
    let (_:bool) = unitvec.MatchesOrientation (Vec(1., 2., 3.))
    let (_:bool) = unitvec.IsOppositeOrientation (-u2)
    let (_:bool) = unitvec.IsOppositeOrientation (Vec(-1., -2., -3.))
    let (_:bool) = unitvec.IsXAligned
    let (_:bool) = unitvec.IsYAligned
    let (_:bool) = unitvec.IsZAligned
    let (_:bool) = unitvec.IsVertical
    let (_:bool) = unitvec.IsHorizontal
    let (_:bool) = unitvec.IsParallelTo u2
    let (_:bool) = unitvec.IsParallelTo (Vec(1., 0., 0.))
    let (_:bool) = unitvec.IsParallelAndOrientedTo u2
    let (_:bool) = unitvec.IsParallelAndOrientedTo (Vec(1., 0., 0.))
    let (_:bool) = unitvec.IsPerpendicularTo u2
    let (_:bool) = unitvec.IsPerpendicularTo (Vec(0., 1., 0.))

    // Operators
    let (_:UnitVec) = -unitvec                    // Negate
    let (_:Vec) = unitvec + u2                    // Add unit + unit
    let (_:Vec) = (Vec(1., 2., 3.)) + unitvec     // Add Vec + unit
    let (_:Vec) = unitvec + (Vec(1., 2., 3.))     // Add unit + Vec
    let (_:Vec) = unitvec - u2                    // Subtract unit - unit
    let (_:Vec) = (Vec(1., 2., 3.)) - unitvec     // Subtract Vec - unit
    let (_:Vec) = unitvec - (Vec(1., 2., 3.))     // Subtract unit - Vec
    let (_:Vec) = unitvec * 2.0                   // Multiply unit by scalar
    let (_:Vec) = 2.0 * unitvec                   // Multiply scalar by unit
    let (_:Vec) = unitvec / 2.0                   // Divide by scalar
    let (_:float) = unitvec *** u2                // Dot product (unit *** unit)
    let (_:float) = unitvec *** (Vec(1., 2., 3.)) // Dot product (unit *** Vec)
    let (_:float) = (Vec(1., 2., 3.)) *** unitvec // Dot product (Vec *** unit)

    // Static members - Constants
    let (_:UnitVec) = UnitVec.Xaxis
    let (_:UnitVec) = UnitVec.Yaxis
    let (_:UnitVec) = UnitVec.Zaxis

    // Static members - Creation
    let (_:UnitVec) = UnitVec.create(1.0, 0.0, 0.0)
    let (_:UnitVec) = UnitVec.create(Pnt(0., 0., 0.), Pnt(1., 0., 0.))
    let (_:UnitVec) = UnitVec.createUnchecked(1.0, 0.0, 0.0)
    // let (_:UnitVec) = UnitVec.createUnchecked(Vec(1., 0., 0.))
    let (_:UnitVec) = UnitVec.createFromPnt(Pnt(1., 0., 0.))
    let (_:UnitVec) = UnitVec.createFromVec(Vec(1., 0., 0.))

    // Static members - Conversion
    let (_:Pt) = UnitVec.asPt unitvec
    let (_:Vc) = UnitVec.asVc unitvec
    let (_:UnitVc) = UnitVec.asUnitVc unitvec
    let (_:Pnt) = UnitVec.asPnt unitvec
    let (_:Vec) = UnitVec.asVec unitvec

    // Static members - Comparison
    let (_:bool) = UnitVec.equals 0.001 unitvec u2
    let (_:bool) = UnitVec.notEquals 0.001 unitvec u2
    let (_:float) = UnitVec.difference unitvec u2
    let (_:float) = UnitVec.differenceSq unitvec u2

    // Static members - Products
    let (_:Vec) = UnitVec.cross(unitvec, u2)
    let (_:Vec) = UnitVec.cross(unitvec, Vec(1., 2., 3.))
    let (_:Vec) = UnitVec.cross(Vec(1., 2., 3.), unitvec)
    let (_:float) = UnitVec.dot(unitvec, u2)
    let (_:float) = UnitVec.dot(unitvec, Vec(1., 2., 3.))
    let (_:float) = UnitVec.dot(Vec(1., 2., 3.), unitvec)

    // Static members - Creation (additional)
    let (_:UnitVec) = UnitVec.createFromMembersXYZ (Vec(1., 0., 0.))  // Vec has X, Y and Z members
    // createFromMembersxyz requires lowercase x/y/z members which Euclid types don't have

    // Static members - Transform and orientation
    let rm = RigidMatrix.identity
    let (_:UnitVec) = unitvec.TransformRigid rm
    let (_:UnitVec) = UnitVec.transformRigid rm unitvec
    let (_:UnitVec) = UnitVec.matchVecOrientation (Vec(1., 0., 0.)) unitvec

// ===== Line2D (2D Finite Line) =====

module Line2DAPI =
    // Constructors
    let line2d = Line2D(Pt(0., 0.), Pt(10., 0.))
    let ln2 = Line2D(0., 0., 10., 10.)

    // Fields
    let (_:float) = line2d.FromX
    let (_:float) = line2d.FromY
    let (_:float) = line2d.ToX
    let (_:float) = line2d.ToY

    // Instance members - String
    let (_:string) = line2d.ToString()
    let (_:string) = line2d.AsString

    // Instance members - Properties
    let (_:Pt) = line2d.From
    let (_:Pt) = line2d.To
    let (_:float) = line2d.Length
    let (_:float) = line2d.LengthSq
    let (_:Vc) = line2d.Direction
    let (_:Vc) = line2d.Vector
    let (_:Vc) = line2d.Tangent
    let (_:UnitVc) = line2d.UnitTangent
    let (_:Pt) = line2d.Mid
    let (_:bool) = line2d.IsZeroLength
    let (_:bool) = line2d.IsXAligned
    let (_:bool) = line2d.IsYAligned
    let (_:Line2D) = line2d.Reversed

    let pt = Pt(5., 5.)
    let vc = Vc(1., 1.)
    // Instance members - Operations
    let (_:Pt) = line2d.EvaluateAt 0.5
    let (_:bool) = line2d.IsTiny 0.001
    let (_:bool) = line2d.IsTinySq 0.001
    let (_:Line2D) = line2d.SubLine(0.2, 0.8)
    let (_:Line2D) = line2d.Segment(0.2, 0.8)
    let (_:float) = line2d.LengthTillParam 0.5
    let (_:float) = line2d.LengthFromParam 0.5
    let (_:Line2D) = line2d.Extend(1.0, 1.0)
    let (_:Line2D) = line2d.ExtendStart 1.0
    let (_:Line2D) = line2d.ExtendEnd 1.0
    let (_:Line2D) = line2d.ExtendRel(0.1, 0.1)
    let (_:Line2D) = line2d.ExtendStartRel 0.1
    let (_:Line2D) = line2d.ExtendEndRel 0.1
    let (_:Line2D) = line2d.Shrink(1.0, 1.0)
    let (_:Line2D) = line2d.ShrinkStart 1.0
    let (_:Line2D) = line2d.ShrinkEnd 1.0
    let (_:Line2D) = line2d.Move vc
    let (_:Line2D) = line2d.MoveX 1.0
    let (_:Line2D) = line2d.MoveY 1.0
    let (_:float) = line2d.ClosestParameterInfinite pt
    let (_:float) = line2d.ClosestParameter pt
    let (_:Pt) = line2d.ClosestPointInfinite pt
    let (_:Pt) = line2d.ClosestPoint pt
    let (_:float) = line2d.DistanceSqFromPointInfinite pt
    let (_:float) = line2d.DistanceToPtInfinite pt
    let (_:float) = line2d.DistanceSqFromPoint pt
    let (_:float) = line2d.DistanceToPt pt
    let (_:bool) = line2d.MatchesOrientation180 ln2
    let (_:bool) = line2d.MatchesOrientation180  vc
    let (_:bool) = line2d.MatchesOrientation180 UnitVc.Xaxis
    let (_:bool) = line2d.MatchesOrientation90 ln2
    let (_:bool) = line2d.IsParallelTo ln2
    let (_:bool) = line2d.IsParallelTo (Vc(1., 0.))
    let (_:bool) = line2d.IsParallelTo UnitVc.Xaxis
    let (_:bool) = line2d.IsParallelAndOrientedTo ln2
    let (_:bool) = line2d.IsParallelAndOrientedTo (Vc(1., 0.))
    let (_:bool) = line2d.IsParallelAndOrientedTo UnitVc.Xaxis
    let (_:bool) = line2d.IsPerpendicularTo ln2
    let (_:bool) = line2d.IsPerpendicularTo (Vc(0., 1.))
    let (_:bool) = line2d.IsPerpendicularTo UnitVc.Yaxis
    // let (_:bool) = line2d.IsCoincidentTo(ln2, 0.001, 0.001<Cosine.cosine>)
    let (_:bool) = line2d.IsPointOnRight(Pt(5., -5.))
    let (_:bool) = line2d.IsPointOnLeft pt
    let (_:Line2D) = line2d.Scale 2.0
    let (_:Line2D) = line2d.ScaleOn Pt.Origin 2.0

    // Static members - Creation
    let (_:Line2D) = Line2D.createFromVec(Vc(10., 0.))
    let (_:Line2D) = Line2D.createFromPtAndVc(Pt.Origin, Vc(10., 0.))

    // Static members - Properties
    let (_:Pt) = Line2D.start line2d
    let (_:Pt) = Line2D.from line2d
    let (_:float) = Line2D.fromX line2d
    let (_:float) = Line2D.fromY line2d
    let (_:Pt) = Line2D.ende line2d
    let (_:Pt) = Line2D.to' line2d
    let (_:float) = Line2D.toX line2d
    let (_:float) = Line2D.toY line2d
    let (_:Line2D) = Line2D.setStart (Pt(1., 1.)) line2d
    let (_:Line2D) = Line2D.setEnd (Pt(11., 1.)) line2d
    let (_:Vc) = Line2D.direction line2d
    let (_:Vc) = Line2D.vector line2d
    let (_:Vc) = Line2D.tangent line2d
    let (_:UnitVc) = Line2D.unitTangent line2d
    let (_:float) = Line2D.length line2d
    let (_:float) = Line2D.lengthSq line2d
    let (_:bool) = Line2D.isZeroLength line2d
    let (_:bool) = Line2D.isTiny 0.001 line2d
    let (_:bool) = Line2D.isTinySq 0.001 line2d
    let (_:bool) = Line2D.isXAligned line2d
    let (_:bool) = Line2D.isYAligned line2d

    // Static members - Modification
    let (_:Pt) = Line2D.evaluateAt 0.5 line2d
    let (_:Line2D) = Line2D.subLine 0.2 0.8 line2d
    let (_:Line2D) = Line2D.segment 0.2 0.8 line2d
    let (_:Pt) = Line2D.mid line2d
    let (_:Line2D) = Line2D.reverse line2d
    let (_:Line2D) = Line2D.flip line2d
    let (_:Line2D) = Line2D.translate  vc line2d
    let (_:Line2D) = Line2D.moveX 1.0 line2d
    let (_:Line2D) = Line2D.moveY 1.0 line2d
    let (_:Line2D) = Line2D.move  vc line2d
    let (_:Line2D) = Line2D.rotate (Rotation2D.createFromDegrees 45.0) line2d
    let (_:Line2D) = Line2D.rotateWithCenter Pt.Origin (Rotation2D.createFromDegrees 45.0) line2d
    let (_:Line2D) = Line2D.matchOrientation ln2 line2d
    let (_:Line2D) = Line2D.matchVcOrientation (Vc(1., 0.)) line2d
    let (_:Line2D) = Line2D.matchUnitVcOrientation UnitVc.Xaxis line2d
    let (_:bool) = Line2D.matchesOrientation180 line2d ln2
    let (_:bool) = Line2D.matchesOrientation90 line2d ln2
    let (_:bool) = Line2D.areParallel line2d ln2
    let (_:bool) = Line2D.areParallelAndMatchOrientation line2d ln2
    let (_:bool) = Line2D.arePerpendicular line2d ln2

    // Static members - Extension/Shrinking
    let (_:Line2D) = Line2D.extend 1.0 1.0 line2d
    let (_:Line2D) = Line2D.extendStart 1.0 line2d
    let (_:Line2D) = Line2D.extendEnd 1.0 line2d
    let (_:Line2D) = Line2D.extendRel 0.1 0.1 line2d
    let (_:Line2D) = Line2D.extendStartRel 0.1 line2d
    let (_:Line2D) = Line2D.extendEndRel 0.1 line2d
    let (_:Line2D) = Line2D.shrink 1.0 1.0 line2d
    let (_:Line2D) = Line2D.shrinkStart 1.0 line2d
    let (_:Line2D) = Line2D.shrinkEnd 1.0 line2d
    let (_:Pt) = Line2D.pointAtDistance 5.0 line2d
    let (_:Line2D) = Line2D.withLengthFromStart 5.0 line2d
    let (_:Line2D) = Line2D.withLengthToEnd 5.0 line2d
    let (_:Line2D) = Line2D.withLengthFromMid 5.0 line2d
    let (_:Line2D) = Line2D.offset 1.0 line2d
    let (_:Line2D) = Line2D.scale 2.0 line2d

    // Static members - Distance/Closest
    let (_:float) = Line2D.closestParameterInfinite pt line2d
    let (_:float) = Line2D.closestParameter pt line2d
    let (_:Pt) = Line2D.closestPointInfinite pt line2d
    let (_:Pt) = Line2D.closestPoint pt line2d
    let (_:float) = Line2D.distanceSqFromPointInfinite pt line2d
    let (_:float) = Line2D.distanceToPtInfinite pt line2d
    let (_:float) = Line2D.distanceSqFromPoint pt line2d
    let (_:float) = Line2D.distanceToPt pt line2d
    let (_:float) = Line2D.lengthToPtOnLine line2d (Pt(5., 0.))
    let (_:float) = Line2D.distanceBetweenLines (line2d, ln2)

    // Static members - Division
    let (_:Pt[]) = Line2D.divide 3 line2d
    let (_:Pt[]) = Line2D.divideMinLength 2.0 line2d
    let (_:Pt[]) = Line2D.divideMaxLength 2.0 line2d
    let (_:ResizeArray<Pt>) = Line2D.divideEvery 2.0 line2d
    let (_:ResizeArray<Pt>) = Line2D.divideInsideEvery 2.0 line2d
    let (_:Line2D[]) = Line2D.split 0.1 3 line2d
    let (_:Line2D[]) = Line2D.splitMinLength 2.0 0.1 line2d
    let (_:Line2D[]) = Line2D.splitMaxLength 2.0 0.1 line2d

    // Static members - Intersection
    // obsolet warning as error:
    // let (_) = Line2D.intersectionParamInfinite(line2d, ln2)
    // let (_) = Line2D.intersectionInfinite(line2d, ln2)
    // let (_) = Line2D.intersectionPointInfinite(line2d, ln2)
    // let (_) = Line2D.intersectionParam(line2d, ln2)
    // let (_) = Line2D.intersection(line2d, ln2)
    // let (_) = Line2D.intersectionPoint (line2d, ln2)
    // let (_:int) = Line2D.areTouchingAny 0.001 line2d ln2
    // let (_:int) = Line2D.areTouchingEither 0.001 line2d ln2
    let (_:Line2D) = Line2D.projectOn line2d ln2

    // Static members - Comparison
    let (_:bool) = Line2D.equals 0.001 line2d ln2
    let (_:bool) = Line2D.notEquals 0.001 line2d ln2
    let (_:bool) = Line2D.areCoincident line2d ln2

// ===== Line3D (3D Finite Line) =====

module Line3DAPI =
    // Constructors
    let line3d = Line3D(Pnt(0., 0., 0.), Pnt(10., 0., 0.))
    let ln2 = Line3D(0., 0., 0., 10., 10., 10.)

    // Fields
    let (_:float) = line3d.FromX
    let (_:float) = line3d.FromY
    let (_:float) = line3d.FromZ
    let (_:float) = line3d.ToX
    let (_:float) = line3d.ToY
    let (_:float) = line3d.ToZ

    // Instance members - String
    let (_:string) = line3d.ToString()
    let (_:string) = line3d.AsString

    // Instance members - Properties
    let (_:Pnt) = line3d.From
    let (_:Pnt) = line3d.To
    let (_:float) = line3d.Length
    let (_:float) = line3d.LengthSq
    let (_:Vec) = line3d.Direction
    let (_:Vec) = line3d.Vector
    let (_:Vec) = line3d.Tangent
    let (_:UnitVec) = line3d.UnitTangent

    // Instance members - Rotation
    let q = Quaternion.createFromDegree(UnitVec.Zaxis, 45.0)
    let (_:Line3D) = line3d.Rotate q
    let (_:Line3D) = line3d.RotateWithCenter(Pnt.Origin, q)

    // Instance members - Distance
    let testPnt = Pnt(5., 5., 0.)
    let (_:float) = line3d.DistanceToPnt testPnt
    let (_:float) = line3d.DistanceToPntInfinite testPnt
    let (_:float) = line3d.DistanceSqToPnt testPnt
    let (_:float) = line3d.DistanceSqToPntInfinite testPnt

    // Instance members - Move and Transform
    let rm = RigidMatrix.identity
    let (_:Line3D) = line3d.MoveZ 1.0
    let (_:Line3D) = line3d.TransformRigid rm

    // Operators
    let (_:Line3D) = line3d *** q               // Rotate by quaternion

    // Static members - Creation
    let ln2d = Line2D(Pt(0., 0.), Pt(10., 0.))
    let (_:Line3D) = Line3D.createFromLine2D ln2d
    let (_:Line3D) = Line3D.createFromLine2DwithZ 5.0 ln2d
    let (_:Line3D) = Line3D.createFromPntAndVec(Pnt.Origin, Vec(10., 0., 0.))

    // Static members - Properties
    let (_:float) = Line3D.fromZ line3d
    let (_:float) = Line3D.toZ line3d

    // Static members - Distance\n    let (_:float) = Line3D.distanceToPnt testPnt ln\n    let (_:float) = Line3D.distanceToPntInfinite testPnt ln\n    let (_:float) = Line3D.distanceSqToPnt testPnt ln\n    let (_:float) = Line3D.distanceSqToPntInfinite testPnt ln\n    let (_:float) = Line3D.distanceBetweenInfiniteLines(ln, ln2)\n\n    // Static members - Operations\n    let r2d = Rotation2D.createFromDegrees 45.0\n    let (_:Line3D) = Line3D.offsetXY UnitVc.Yaxis 1.0 ln\n    let (_:Line3D) = Line3D.rotate2D r2d ln\n    let (_:Line3D) = Line3D.rotate2dOn Pnt.Origin r2d ln\n    let (_:Line2D) = Line3D.toLine2D ln\n    let (_:Line3D) = Line3D.transformRigid rm ln\n    let (_:Line3D) = Line3D.matchVecOrientation (Vec(1., 0., 0.)) ln\n\n// ===== Rotation2D (2D Counter-Clockwise Rotation) =====

module Rotation2DAPI =
    // Creation
    let rotation2d = Rotation2D.createFromDegrees 45.0
    let r2 = Rotation2D.createFromRadians 0.785

    // Fields
    let (_:float) = rotation2d.Sin
    let (_:float) = rotation2d.Cos

    // Instance members
    let (_:string) = rotation2d.ToString()
    let (_:float) = rotation2d.InRadians
    let (_:float) = rotation2d.InDegrees
    let (_:Rotation2D) = rotation2d.Inverse
    let (_:Rotation2D) = rotation2d.Add r2
    let (_:Rotation2D) = rotation2d.AddDegrees 45.0
    let (_:Rotation2D) = rotation2d.AddRadians 0.785

    // Static members - Creation
    let (_:Rotation2D) = Rotation2D.createFromDegrees 90.0
    let (_:Rotation2D) = Rotation2D.createFromRadians 1.57
    let (_:Rotation2D) = Rotation2D.createUnchecked(0.707, 0.707)
    let (_:Rotation2D) = Rotation2D.createFromVectors(UnitVc.Xaxis, UnitVc.Yaxis)

    // Static members - Comparison
    let (_:bool) = Rotation2D.equals 0.001 rotation2d r2

// ===== BBox (3D Bounding Box) =====

module BBoxAPI =
    // Creation
    let bbox = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
    let box2 = BBox.create(Pnt(5., 5., 5.), Pnt(15., 15., 15.))

    // Fields
    let (_:float) = bbox.MinX
    let (_:float) = bbox.MinY
    let (_:float) = bbox.MinZ
    let (_:float) = bbox.MaxX
    let (_:float) = bbox.MaxY
    let (_:float) = bbox.MaxZ

    // Instance members - String
    let (_:string) = bbox.ToString()
    let (_:string) = bbox.AsString

    // Instance members - Properties
    let (_:Pnt) = bbox.MinPnt
    let (_:Pnt) = bbox.MaxPnt
    let (_:Pnt) = bbox.Center
    let (_:Vec) = bbox.Diagonal
    let (_:float) = bbox.Width
    let (_:float) = bbox.SizeX
    let (_:float) = bbox.Depth
    let (_:float) = bbox.SizeY
    let (_:float) = bbox.Height3D
    let (_:float) = bbox.SizeZ
    let (_:float) = bbox.Volume
    let (_:float) = bbox.LongestEdge
    let (_:float) = bbox.ShortestEdge
    let (_:int) = bbox.CountZeroSides
    let (_:bool) = bbox.IsFlat
    let (_:bool) = bbox.IsLine
    let (_:bool) = bbox.IsPoint
    let (_:bool) = bbox.IsZero
    let (_:bool) = bbox.IsValid
    let (_:bool) = bbox.HasVolume
    let (_:BRect) = bbox.asBRect
    let (_:BRect) = bbox.asBRect // use asBRect instead of deprecated asRect

    // Instance members - Corner points
    let (_:Pnt) = bbox.Pt0
    let (_:Pnt) = bbox.Pt1
    let (_:Pnt) = bbox.Pt2
    let (_:Pnt) = bbox.Pt3
    let (_:Pnt) = bbox.Pt4
    let (_:Pnt) = bbox.Pt5
    let (_:Pnt) = bbox.Pt6
    let (_:Pnt) = bbox.Pt7
    let (_:Pnt[]) = bbox.Points
    let (_:Pnt[]) = bbox.BottomPoints
    let (_:Pnt[]) = bbox.BottomPointsLooped
    let (_:Pnt[]) = bbox.TopPoints
    let (_:Pnt[]) = bbox.TopPointsLooped

    // Instance members - Edges
    let (_:Line3D) = bbox.Edge01
    let (_:Line3D) = bbox.Edge03
    let (_:Line3D) = bbox.Edge04
    let (_:Line3D) = bbox.Edge12
    let (_:Line3D) = bbox.Edge15
    let (_:Line3D) = bbox.Edge26
    let (_:Line3D) = bbox.Edge32
    let (_:Line3D) = bbox.Edge37
    let (_:Line3D) = bbox.Edge45
    let (_:Line3D) = bbox.Edge47
    let (_:Line3D) = bbox.Edge56
    let (_:Line3D) = bbox.Edge76
    let (_:Line3D[]) = bbox.Edges

    // Instance members - Expand
    let (_:BBox) = bbox.Expand 1.0
    let (_:BBox) = bbox.Expand(1.0, 2.0, 3.0)
    let (_:BBox) = bbox.ExpandSafe 1.0
    let (_:BBox) = bbox.ExpandSafe(1.0, 2.0, 3.0)
    let (_:BBox) = bbox.ExpandXaxis(1.0, 2.0)
    let (_:BBox) = bbox.ExpandYaxis(1.0, 2.0)
    let (_:BBox) = bbox.ExpandZaxis(1.0, 2.0)

    // Instance members - Operations
    let (_:Pnt) = bbox.EvaluateAt(0.5, 0.5, 0.5)
    let (_:bool) = bbox.Contains(Pnt(5., 5., 5.))
    let (_:bool) = bbox.Contains box2
    let (_:BBox) = bbox.Union(Pnt(15., 15., 15.))
    let (_:BBox) = bbox.Union box2
    let (_:BBox voption) = bbox.Intersection box2
    let (_:bool) = bbox.IsTouching(box2, 0.001)

    // Instance members - Overlap
    let (_:bool) = bbox.OverlapsWith box2
    let (_:bool) = bbox.OverlapsWith(box2, 0.001)

    // Static members - Creation
    let (_:BBox) = BBox.create(Pnt(0., 0., 0.), Pnt(10., 10., 10.))
    let (_:BBox) = BBox.createUnchecked(0., 0., 0., 10., 10., 10.)
    let (_:BBox) = BBox.createFromCenter(Pnt(5., 5., 5.), 10., 10., 10.)
    let (_:BBox) = BBox.createFromIList(ResizeArray [Pnt(0., 0., 0.); Pnt(10., 10., 10.)])
    let (_:BBox) = BBox.createFromSeq(seq { yield Pnt(0., 0., 0.); yield Pnt(10., 10., 10.) })

    // Static members - Properties
    let (_:float) = BBox.volume bbox
    let (_:BRect) = BBox.toBRect bbox

    // Static members - Operations
    let (_:BBox) = BBox.expand 1.0 bbox
    let (_:BBox) = BBox.expandSafe 1.0 bbox
    let (_:BBox) = BBox.expandRel 0.1 bbox
    let (_:BBox) = BBox.expandRelXYZ 0.1 0.2 0.3 bbox
    let (_:BBox) = BBox.expandXaxis 1.0 2.0 bbox
    let (_:BBox) = BBox.expandYaxis 1.0 2.0 bbox
    let (_:BBox) = BBox.expandZaxis 1.0 2.0 bbox
    let (_:BBox) = BBox.move (Vec(1., 1., 1.)) bbox
    let (_:BBox) = BBox.moveX 1.0 bbox
    let (_:BBox) = BBox.moveY 1.0 bbox
    let (_:BBox) = BBox.moveZ 1.0 bbox
    let (_:BBox) = BBox.scale 2.0 bbox
    let (_:bool) = BBox.containsPnt (Pnt(5., 5., 5.)) bbox
    let (_:bool) = BBox.contains box2 bbox
    let (_:BBox) = BBox.unionPt (Pnt(15., 15., 15.)) bbox
    let (_:BBox) = BBox.union bbox box2
    let (_:BBox voption) = BBox.intersection bbox box2

    // Static members - Comparison
    let (_:bool) = BBox.equals 0.001 bbox box2
    let (_:bool) = BBox.notEquals 0.001 bbox box2
    let (_:bool) = BBox.doOverlap bbox box2
    let (_:bool) = BBox.doOverlapMoreThan 0.001 bbox box2

// ===== BRect (2D Bounding Rectangle) =====

module BRectAPI =
    // Creation
    let brect = BRect.create(Pt(0., 0.), Pt(10., 10.))
    let rect2 = BRect.create(Pt(5., 5.), Pt(15., 15.))

    // Fields
    let (_:float) = brect.MinX
    let (_:float) = brect.MinY
    let (_:float) = brect.MaxX
    let (_:float) = brect.MaxY

    // Instance members - String
    let (_:string) = brect.ToString()
    let (_:string) = brect.AsString

    // Instance members - Properties
    let (_:Pt) = brect.MinPt
    let (_:Pt) = brect.MaxPt
    let (_:Pt) = brect.Center
    let (_:Vc) = brect.Diagonal
    let (_:float) = brect.Width
    let (_:float) = brect.SizeX
    let (_:float) = brect.Height2D
    let (_:float) = brect.SizeY

    // Instance members - Expand
    let (_:BRect) = brect.Expand 1.0
    let (_:BRect) = brect.Expand(1.0, 2.0)
    let (_:BRect) = brect.ExpandSafe 1.0
    let (_:BRect) = brect.ExpandSafe(1.0, 2.0)
    let (_:BRect) = brect.ExpandXaxis(1.0, 2.0)
    let (_:BRect) = brect.ExpandYaxis(1.0, 2.0)

    // Instance members - Overlap
    let (_:bool) = brect.OverlapsWith rect2
    let (_:bool) = brect.OverlapsWith(rect2, 0.001)

    // Static members - Creation
    let (_:BRect) = BRect.create(Pt(0., 0.), Pt(10., 10.))

    // Static members - Containment
    let (_:bool) = BRect.containsPt (Pt(5., 5.)) brect

// ===== Quaternion (3D Rotation) =====

module QuaternionAPI =
    // Creation
    let quaternion = Quaternion.createFromDegree(UnitVec.Zaxis, 45.0)
    let q2 = Quaternion.createFromDegree(UnitVec.Xaxis, 90.0)

    // Fields
    let (_:float) = quaternion.X
    let (_:float) = quaternion.Y
    let (_:float) = quaternion.Z
    let (_:float) = quaternion.W

    // Instance members
    let (_:string) = quaternion.ToString()
    let (_:Quaternion) = quaternion.Conjugate
    let (_:Quaternion) = quaternion.Inverse
    let (_:float) = quaternion.AngleInRadians
    let (_:float) = quaternion.AngleInDegrees
    let (_:Vec) = quaternion.Axis

    // Static members - Constants
    let (_:Quaternion) = Quaternion.identity

    // Static members - Creation
    let (_:Quaternion) = Quaternion.createFromRadians(UnitVec.Zaxis, 0.785)
    let (_:Quaternion) = Quaternion.createFromRadians(Vec(0., 0., 1.), 0.785)
    let (_:Quaternion) = Quaternion.createFromDegree(UnitVec.Zaxis, 45.0)
    let (_:Quaternion) = Quaternion.createFromDegree(Vec(0., 0., 1.), 45.0)
    let (_:Quaternion) = Quaternion.createVecToVec(UnitVec.Xaxis, UnitVec.Yaxis)
    let (_:Quaternion) = Quaternion.createVecToVec(Vec(1., 0., 0.), Vec(0., 1., 0.))
    let (_:Quaternion) = Quaternion.multiply(quaternion, q2)

    // Operators - Apply rotation
    let v = Vec(1., 0., 0.)
    let p = Pnt(1., 0., 0.)
    let u = UnitVec.Xaxis
    let ln = Line3D(Pnt.Origin, Pnt(1., 0., 0.))

    let (_:Vec) = v *** quaternion                   // Rotate vector
    let (_:Pnt) = p *** quaternion                   // Rotate point
    let (_:UnitVec) = u *** quaternion               // Rotate unit vector
    let (_:Line3D) = ln *** quaternion               // Rotate line
    let (_:Quaternion) = quaternion *** q2           // Multiply quaternions

    // Static members - Creation (additional)
    // let (_:Quaternion) = Quaternion.createDirectlyUnchecked(0.0, 0.0, 0.0, 1.0)
    let rm = RigidMatrix.identity
    let (_:Quaternion) = Quaternion.createFromRigidMatrix rm
    let (_:Quaternion) = Quaternion.createFromEulerXYZ(0.1, 0.2, 0.3)
    let (_:Quaternion) = Quaternion.createFromEulerXZY(0.1, 0.2, 0.3)
    let (_:Quaternion) = Quaternion.createFromEulerYXZ(0.1, 0.2, 0.3)
    let (_:Quaternion) = Quaternion.createFromEulerYZX(0.1, 0.2, 0.3)
    let (_:Quaternion) = Quaternion.createFromEulerZXY(0.1, 0.2, 0.3)
    let (_:Quaternion) = Quaternion.createFromEulerZYX(0.1, 0.2, 0.3)

    // Instance members - Manipulation
    let (_:Quaternion) = quaternion.setAngleInDegrees 90.0
    let (_:Quaternion) = quaternion.setAngleInRadians 1.57
    let (_:float * float * float) = Quaternion.toEulerAnglesZYX quaternion

// ===== Matrix (4x4 Transformation Matrix) =====

module MatrixAPI =
    // Creation
    let matrix = Matrix.createTranslation(1.0, 2.0, 3.0)
    let m2 = Matrix.createScale(2.0, 2.0, 2.0)

    // Fields (16 elements)
    let (_:float) = matrix.M11
    let (_:float) = matrix.M12
    let (_:float) = matrix.M13
    let (_:float) = matrix.M14
    let (_:float) = matrix.M21
    let (_:float) = matrix.M22
    let (_:float) = matrix.M23
    let (_:float) = matrix.M24
    let (_:float) = matrix.M31
    let (_:float) = matrix.M32
    let (_:float) = matrix.M33
    let (_:float) = matrix.M34
    let (_:float) = matrix.M44

    // Instance members - String
    let (_:string) = matrix.ToString()
    let (_:string) = matrix.AsString

    // Instance members - Properties
    let (_:Vec) = matrix.Translation
    let (_:Vec) = matrix.ColumnVector1
    let (_:Vec) = matrix.ColumnVector2
    let (_:Vec) = matrix.ColumnVector3
    let (_:float) = matrix.Determinant
    let (_:Matrix) = matrix.Inverse
    let (_:bool) = matrix.IsIdentity
    let (_:bool) = matrix.IsAffine
    let (_:bool) = matrix.IsMirroring
    let (_:bool) = matrix.IsOnlyTranslating
    let (_:bool) = matrix.IsOrthogonal
    let (_:bool) = matrix.IsProjecting
    let (_:bool) = matrix.IsReflecting
    let (_:bool) = matrix.IsScaling
    let (_:bool) = matrix.IsTranslating
    let (_:float[]) = matrix.ToArrayByRows
    let (_:float[]) = matrix.ToArrayByColumns

    // Static members - Constants
    let (_:Matrix) = Matrix.identity

    // Static members - Creation
    let (_:Matrix) = Matrix.createTranslation(1.0, 2.0, 3.0)
    let (_:Matrix) = Matrix.createTranslation(Vec(1., 2., 3.))
    let (_:Matrix) = Matrix.createScale(2.0, 2.0, 2.0)
    let (_:Matrix) = Matrix.createRotationX 45.0
    let (_:Matrix) = Matrix.createRotationY 45.0
    let (_:Matrix) = Matrix.createRotationZ 45.0
    let (_:Matrix) = Matrix.createRotationAxis(UnitVec.Zaxis, 45.0)
    let (_:Matrix) = Matrix.createRotationAxis(Vec(0., 0., 1.), 45.0)
    let (_:Matrix) = Matrix.createRotationAxisCenter(UnitVec.Zaxis, Pnt.Origin, 45.0)
    let (_:Matrix) = Matrix.createRotationAxisCenter(Vec(0., 0., 1.), Pnt.Origin, 45.0)
    let (_:Matrix) = Matrix.createVecToVec(UnitVec.Xaxis, UnitVec.Yaxis)
    let (_:Matrix) = Matrix.createVecToVec(Vec(1., 0., 0.), Vec(0., 1., 0.))
    let (_:Matrix) = Matrix.createFromQuaternion(Quaternion.identity)
    let (_:Matrix) = Matrix.createFromRowMajorArray([|1.;0.;0.;0.; 0.;1.;0.;0.; 0.;0.;1.;0.; 0.;0.;0.;1.|])
    let (_:Matrix) = Matrix.createFromColumMajorArray([|1.;0.;0.;0.; 0.;1.;0.;0.; 0.;0.;1.;0.; 0.;0.;0.;1.|])
    let plane = PPlane.WorldXY
    let (_:Matrix) = Matrix.createToPlane plane
    let (_:Matrix) = Matrix.createPlaneToPlane(plane, plane)
    let (_:Matrix) = Matrix.createMirror plane
    let (_:Matrix) = Matrix.createShear(0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
    let (_:Matrix) = Matrix.createPerspective(1.0, 100.0, 1.0, 2.0)

    // Static members - Operations
    let (_:Matrix) = Matrix.multiply(matrix, m2)
    let (_:Matrix) = Matrix.inverse matrix
    let (_:float) = Matrix.determinant matrix
    let (_:Matrix) = Matrix.transpose matrix
    let (_:Matrix) = Matrix.addTranslation (Vec(1., 2., 3.)) matrix
    let (_:Matrix) = Matrix.addTranslationXYZ 1.0 2.0 3.0 matrix
    let (_:float[]) = Matrix.toArrayByRows matrix
    let (_:float[]) = Matrix.toArrayByColumns matrix

    // Static members - Comparison
    let (_:bool) = Matrix.equals 0.001 matrix m2

    // Operators
    let (_:Pnt) = Pnt(1., 2., 3.) *** matrix        // Transform point
    let (_:Vec) = Vec(1., 2., 3.) *** matrix        // Transform vector
    let (_:Vec) = UnitVec.Xaxis *** matrix          // Transform unit vector (returns Vec)
    let (_:Matrix) = matrix *** m2                  // Multiply matrices

// ===== RigidMatrix (4x4 Rigid Transformation Matrix - no scaling/shearing) =====

module RigidMatrixAPI =
    // Creation
    let rigidmatrix = RigidMatrix.createTranslation(1.0, 2.0, 3.0)
    let rm2 = RigidMatrix.createRotationZ 45.0

    // Instance members - String
    let (_:string) = rigidmatrix.ToString()
    let (_:string) = rigidmatrix.AsString

    // Instance members - Properties
    let (_:Vec) = rigidmatrix.Translation
    let (_:Vec) = rigidmatrix.ColumnVector1
    let (_:Vec) = rigidmatrix.ColumnVector2
    let (_:Vec) = rigidmatrix.ColumnVector3
    let (_:float) = rigidmatrix.Determinant
    let (_:RigidMatrix) = rigidmatrix.Inverse
    let (_:bool) = rigidmatrix.IsIdentity
    let (_:Matrix) = rigidmatrix.Matrix
    let (_:float[]) = rigidmatrix.ToArrayByRows
    let (_:float[]) = rigidmatrix.ToArrayByColumns

    // Static members - Constants
    let (_:RigidMatrix) = RigidMatrix.identity

    // Static members - Creation
    let (_:RigidMatrix) = RigidMatrix.createTranslation(1.0, 2.0, 3.0)
    let (_:RigidMatrix) = RigidMatrix.createTranslation(Vec(1., 2., 3.))
    let (_:RigidMatrix) = RigidMatrix.createTranslationX 1.0
    let (_:RigidMatrix) = RigidMatrix.createTranslationY 2.0
    let (_:RigidMatrix) = RigidMatrix.createTranslationZ 3.0
    let (_:RigidMatrix) = RigidMatrix.createRotationX 45.0
    let (_:RigidMatrix) = RigidMatrix.createRotationY 45.0
    let (_:RigidMatrix) = RigidMatrix.createRotationZ 45.0
    let (_:RigidMatrix) = RigidMatrix.createRotationAxis(UnitVec.Zaxis, 45.0)
    let (_:RigidMatrix) = RigidMatrix.createRotationAxis(Vec(0., 0., 1.), 45.0)
    let (_:RigidMatrix) = RigidMatrix.createRotationAxisCenter(UnitVec.Zaxis, Pnt.Origin, 45.0)
    let (_:RigidMatrix) = RigidMatrix.createRotationAxisCenter(Vec(0., 0., 1.), Pnt.Origin, 45.0)
    let (_:RigidMatrix) = RigidMatrix.createVecToVec(UnitVec.Xaxis, UnitVec.Yaxis)
    let (_:RigidMatrix) = RigidMatrix.createVecToVec(Vec(1., 0., 0.), Vec(0., 1., 0.))
    let (_:RigidMatrix) = RigidMatrix.createFromQuaternion(Quaternion.identity)
    let plane = PPlane.WorldXY
    let (_:RigidMatrix) = RigidMatrix.createToPlane plane
    let (_:RigidMatrix) = RigidMatrix.createPlaneToPlane(plane, plane)
    let (_:RigidMatrix) = RigidMatrix.createFromMatrix(Matrix.identity)
    let (_:RigidMatrix option) = RigidMatrix.tryCreateFromMatrix(Matrix.identity)

    // Static members - Operations
    let (_:RigidMatrix) = RigidMatrix.multiply(rigidmatrix, rm2)
    let (_:RigidMatrix) = RigidMatrix.inverse rigidmatrix
    let (_:float) = RigidMatrix.determinant rigidmatrix
    let (_:RigidMatrix) = RigidMatrix.addTranslation (Vec(1., 2., 3.)) rigidmatrix
    let (_:RigidMatrix) = RigidMatrix.addTranslationXYZ 1.0 2.0 3.0 rigidmatrix
    let (_:RigidMatrix) = RigidMatrix.removeTranslation rigidmatrix
    let (_:Matrix) = RigidMatrix.toMatrix rigidmatrix

    // Static members - Comparison
    let (_:bool) = RigidMatrix.equals 0.001 rigidmatrix rm2

    // Operators
    let (_:Pnt) = Pnt(1., 2., 3.) *** rigidmatrix       // Transform point
    let (_:Vec) = Vec(1., 2., 3.) *** rigidmatrix       // Transform vector
    let (_:UnitVec) = UnitVec.Xaxis *** rigidmatrix      // Transform unit vector (RigidMatrix preserves unit length)
    let (_:RigidMatrix) = rigidmatrix *** rm2           // Multiply rigid matrices

// ===== NPlane (Infinite Plane defined by normal and distance) =====

module NPlaneAPI =
    // Creation
    let nplane = NPlane.create(Pnt.Origin, UnitVec.Zaxis)
    let np2 = NPlane.create(Pnt(0., 0., 5.), Vec(0., 0., 1.))

    // Instance members - String
    let (_:string) = nplane.ToString()

    // Instance members - Properties
    let (_:NPlane) = nplane.Flipped

    // Instance members - Operations
    let (_:Pnt) = nplane.ClosestPoint(Pnt(1., 2., 3.))
    let (_:float) = nplane.DistanceToPt(Pnt(1., 2., 3.))
    let (_:bool) = nplane.IsCoincidentTo(np2, 0.001, 0.001<Cosine.cosine>)
    let (_:float) = nplane.Angle90ToPlane(np2)
    let (_:float) = nplane.Angle90ToLine(Line3D(Pnt.Origin, Pnt(1., 0., 0.)))
    let (_:float) = nplane.Angle90ToVec(UnitVec.Xaxis)
    let (_:float) = nplane.Angle90ToVec(Vec(1., 0., 0.))
    let (_:NPlane) = nplane.PlaneAtClPt(Pnt(1., 2., 3.))

    // Static members - Constants
    let (_:NPlane) = NPlane.xyPlane

    // Static members - Creation
    let (_:NPlane) = NPlane.create(Pnt.Origin, UnitVec.Zaxis)
    let (_:NPlane) = NPlane.create(Pnt.Origin, Vec(0., 0., 1.))
    let (_:NPlane) = NPlane.createFrom3Points Pnt.Origin (Pnt(1., 0., 0.)) (Pnt(0., 1., 0.))

    // Static members - Properties
    let (_:UnitVec) = NPlane.normal nplane
    let (_:Pnt) = NPlane.origin nplane

    // Static members - Operations
    let (_:NPlane) = NPlane.offset 1.0 nplane
    // Note: NPlane.offsetInDir has a bug (uses * instead of *** for UnitVec*Vec dot product)
    // let (_:NPlane) = NPlane.offsetInDir (Pnt(0., 0., 10.)) 1.0 np
    let (_:NPlane) = NPlane.move (Vec(1., 1., 1.)) nplane
    let (_:NPlane) = NPlane.translate (Vec(1., 1., 1.)) nplane
    let (_:NPlane) = NPlane.scale 2.0 nplane
    let (_:float) = NPlane.distToPt (Pnt(1., 2., 3.)) nplane
    let (_:float) = NPlane.angleTo nplane np2
    let (_:bool) = NPlane.areCoincident nplane np2
    let (_:bool) = NPlane.doLinePlaneIntersect (Line3D(Pnt.Origin, Pnt(0., 0., 10.))) nplane
    let (_:Line3D option) = NPlane.intersect nplane np2  // Intersection of two planes is a line
    let (_:Pnt option) = NPlane.intersectLine (Line3D(Pnt.Origin, Pnt(0., 0., 10.))) nplane
    let (_:float option) = NPlane.intersectLineParameter (Line3D(Pnt.Origin, Pnt(0., 0., 10.))) nplane

    // Static members - Comparison
    let (_:bool) = NPlane.equals 0.001 nplane np2

// ===== PPlane (Parametric Plane with origin and two axes) =====

module PPlaneAPI =
    // Creation
    let pplane = PPlane.WorldXY
    let pp2 = PPlane.WorldTop

    // Instance members - String
    let (_:string) = pplane.ToString()

    // Instance members - Properties
    let (_:Pnt) = pplane.Origin
    let (_:UnitVec) = pplane.Xaxis
    let (_:UnitVec) = pplane.Yaxis
    let (_:UnitVec) = pplane.Zaxis

    // Instance members - Operations
    let (_:Pnt) = pplane.EvaluateAt(1.0, 2.0, 0.0)
    let (_:Pnt) = pplane.EvaluateAtXY(1.0, 2.0)
    let (_:float*float*float) = pplane.PointParameters(Pnt(1., 2., 3.))
    let (_:Pnt) = pplane.ClosestPoint(Pnt(1., 2., 3.))
    let (_:float) = pplane.DistanceToPt(Pnt(1., 2., 3.))
    let (_:PPlane) = pplane.PlaneAtClPt(Pnt(1., 2., 3.))
    let (_:float) = pplane.Angle90ToPlane(pp2)
    let (_:float) = pplane.Angle90ToVec(UnitVec.Xaxis)
    let (_:float) = pplane.Angle90ToVec(Vec(1., 0., 0.))
    let (_:float) = pplane.Angle90ToLine(Line3D(Pnt.Origin, Pnt(1., 0., 0.)))
    let (_:bool) = pplane.IsCoincidentTo(pp2, 0.001, 0.001<Cosine.cosine>)

    // Static members - Constants
    let (_:PPlane) = PPlane.WorldXY
    let (_:PPlane) = PPlane.WorldTop
    let (_:PPlane) = PPlane.WorldBottom
    let (_:PPlane) = PPlane.WorldFront
    let (_:PPlane) = PPlane.WorldBack
    let (_:PPlane) = PPlane.WorldLeft
    let (_:PPlane) = PPlane.WorldRight
    let (_:PPlane) = PPlane.WorldXMinusY
    let (_:PPlane) = PPlane.WorldYMinusX
    let (_:PPlane) = PPlane.WorldMinusXMinusY
    let (_:PPlane) = PPlane.WorldMinusYX

    // Static members - Creation
    let (_:PPlane) = PPlane.createThreePoints Pnt.Origin (Pnt(1., 0., 0.)) (Pnt(0., 1., 0.))
    let (_:PPlane) = PPlane.createOriginXaxisYaxis(Pnt.Origin, UnitVec.Xaxis, UnitVec.Yaxis)
    let (_:PPlane) = PPlane.createOriginXaxisYaxis(Pnt.Origin, Vec(1., 0., 0.), Vec(0., 1., 0.))
    let (_:PPlane) = PPlane.createOriginNormal(Pnt.Origin, UnitVec.Zaxis)
    let (_:PPlane) = PPlane.createOriginNormal(Pnt.Origin, Vec(0., 0., 1.))
    let (_:PPlane) = PPlane.createOriginNormalXaxis(Pnt.Origin, UnitVec.Zaxis, UnitVec.Xaxis)
    let (_:PPlane) = PPlane.createOriginNormalXaxis(Pnt.Origin, Vec(0., 0., 1.), Vec(1., 0., 0.))
    let (_:PPlane) = PPlane.createUnchecked(Pnt.Origin, UnitVec.Xaxis, UnitVec.Yaxis, UnitVec.Zaxis)

    // Static members - Modification
    let (_:PPlane) = PPlane.setOrigin (Pnt(1., 2., 3.)) pplane
    let (_:PPlane) = PPlane.setOriginX 1.0 pplane
    let (_:PPlane) = PPlane.setOriginY 2.0 pplane
    let (_:PPlane) = PPlane.setOriginZ 3.0 pplane
    let (_:PPlane) = PPlane.translateBy (Vec(1., 1., 1.)) pplane
    let (_:PPlane) = PPlane.translateByWorldX 1.0 pplane
    let (_:PPlane) = PPlane.translateByWorldY 1.0 pplane
    let (_:PPlane) = PPlane.translateByWorldZ 1.0 pplane
    let (_:PPlane) = PPlane.translateLocalX 1.0 pplane
    let (_:PPlane) = PPlane.translateLocalY 1.0 pplane
    let (_:PPlane) = PPlane.translateLocalZ 1.0 pplane
    let (_:PPlane) = PPlane.offset 1.0 pplane
    let (_:PPlane) = PPlane.rotateZ 45.0 pplane
    let (_:PPlane) = PPlane.flipOnX pplane
    let (_:PPlane) = PPlane.flipOnY pplane
    let (_:PPlane) = PPlane.rotateOnZ180 pplane
    let (_:PPlane) = PPlane.rotateZ180IfYNegative pplane
    let (_:PPlane) = PPlane.transform (RigidMatrix.identity) pplane
    let (_:PPlane) = PPlane.move (Vec(1., 1., 1.)) pplane
    let (_:PPlane) = PPlane.translate (Vec(1., 1., 1.)) pplane
    let (_:PPlane) = PPlane.scale 2.0 pplane

    // Static members - Intersection
    let (_:bool) = PPlane.doLinePlaneIntersect (Line3D(Pnt.Origin, Pnt(0., 0., 10.))) pplane
    let (_:Line3D option) = PPlane.intersect pplane pp2  // Intersection of two planes is a line
    let (_:Pnt option) = PPlane.intersectLine (Line3D(Pnt.Origin, Pnt(0., 0., 10.))) pplane
    let (_:float option) = PPlane.intersectLineParameter (Line3D(Pnt.Origin, Pnt(0., 0., 10.))) pplane
    let (_:(float*float*float) option) = PPlane.intersectLineParameters (Line3D(Pnt.Origin, Pnt(0., 0., 10.))) pplane

    // Static members - Comparison
    let (_:bool) = PPlane.equals 0.001 pplane pp2
    let (_:bool) = PPlane.areCoincident 0.001 pplane pp2

// ===== Rect2D (2D Rectangle with origin and axes) =====

module Rect2DAPI =
    // Creation via static methods
    let rect2d = Rect2D.createFromDirectionAndSizes(Pt.Origin, UnitVc.Xaxis, 10.0, 5.0)
    let rect2 = Rect2D.createFromDirectionAndSizes(Pt(5., 5.), UnitVc.Yaxis, 8.0, 4.0)

    // Instance members - String
    let (_:string) = rect2d.ToString()
    let (_:string) = rect2d.AsString

    // Instance members - Properties
    let (_:Pt) = rect2d.Pt0
    let (_:Pt) = rect2d.Pt1
    let (_:Pt) = rect2d.Pt2
    let (_:Pt) = rect2d.Pt3
    let (_:Pt[]) = rect2d.Points
    let (_:Pt[]) = rect2d.PointsLooped
    let (_:Line2D) = rect2d.Edge01
    let (_:Line2D) = rect2d.Edge12
    let (_:Line2D) = rect2d.Edge23
    let (_:Line2D) = rect2d.Edge30
    let (_:Line2D) = rect2d.EdgeX
    let (_:Line2D) = rect2d.EdgeY
    let (_:Line2D[]) = rect2d.Edges
    let (_:Pt) = rect2d.Center
    let (_:Pt) = rect2d.XCorner
    let (_:Pt) = rect2d.YCorner
    let (_:Pt) = rect2d.FarCorner
    let (_:UnitVc) = rect2d.XaxisUnit
    let (_:UnitVc) = rect2d.YaxisUnit
    let (_:float) = rect2d.SizeX
    let (_:float) = rect2d.SizeY
    let (_:float) = rect2d.SizeXSq
    let (_:float) = rect2d.SizeYSq
    let (_:float) = rect2d.Width
    let (_:float) = rect2d.Height2D
    let (_:float) = rect2d.Area
    let (_:float) = rect2d.AreaSq
    let (_:Vc) = rect2d.Diagonal
    let (_:Line2D) = rect2d.DiagonalLine
    let (_:float) = rect2d.LongestEdge
    let (_:float) = rect2d.LongestEdgeSq
    let (_:float) = rect2d.ShortestEdge
    let (_:float) = rect2d.ShortestEdgeSq
    let (_:int) = rect2d.CountZeroSides
    let (_:bool) = rect2d.IsValid
    let (_:bool) = rect2d.IsZero
    let (_:bool) = rect2d.IsPoint
    let (_:bool) = rect2d.IsLine
    // let (_:bool) = rect2d.HasVolume
    let (_:BRect) = rect2d.BRect
    let (_:Rect2D) = rect2d.RotateOrientation90CW
    let (_:Rect2D) = rect2d.RotateOrientation90CCW
    let (_:Rect2D) = rect2d.RotateOrientation180

    // Instance members - Operations
    let (_:Pt) = rect2d.EvaluateAt(0.5, 0.5)
    let (_:Pt) = rect2d.EvaluateDist(5.0, 2.5)
    let (_:Line2D) = rect2d.GetEdge 0
    let (_:bool) = rect2d.Contains(Pt(5., 2.5))
    let (_:Rect2D) = rect2d.Scale 2.0
    let (_:Rect2D) = rect2d.ScaleOn Pt.Origin 2.0

    // Static members - Creation
    let (_:Rect2D) = Rect2D.createUnchecked(Pt.Origin, Vc(10., 0.), Vc(0., 5.))
    let (_:Rect2D) = Rect2D.createFromBRect(BRect.create(Pt.Origin, Pt(10., 5.)))
    let (_:Rect2D) = Rect2D.createFromLine(Line2D(Pt.Origin, Pt(10., 0.)), 2.5, 2.5)
    let (_:Rect2D) = Rect2D.createFromVectors(Pt.Origin, Vc(10., 0.), Vc(0., 5.))
    let (_:Rect2D) = Rect2D.createFromXVectorAndWidth(Pt.Origin, Vc(10., 0.), 5.0)
    let (_:Rect2D) = Rect2D.createFromCenterAndVector(Pt(5., 2.5), Vc(10., 0.), 5.0)
    let (_:Rect2D) = Rect2D.createFromCenterAndDirection(Pt(5., 2.5), UnitVc.Xaxis, 10.0, 5.0)
    let (_:Rect2D) = Rect2D.createFromDirectionAndSizes(Pt.Origin, UnitVc.Xaxis, 10.0, 5.0)
    let (_:Rect2D) = Rect2D.createFromDirAndPoints (Vc(1., 0.)) (ResizeArray [Pt.Origin; Pt(10., 5.)])
    let (_:Rect2D) = Rect2D.createFrom3Points(Pt.Origin, Pt(10., 0.), Pt(0., 5.))
    let (_:Rect2D option) = Rect2D.tryCreateFrom3Points(Pt.Origin, Pt(10., 0.), Pt(0., 5.))

    // Static members - Operations
    let vc = Vc(1., 1.)
    let (_:Rect2D) = Rect2D.translate  vc rect2d
    let (_:Rect2D) = Rect2D.translateLocalX 1.0 rect2d
    let (_:Rect2D) = Rect2D.translateLocalY 1.0 rect2d
    let (_:Rect2D) = Rect2D.move  vc rect2d
    let (_:Rect2D) = Rect2D.rotate (Rotation2D.createFromDegrees 45.0) rect2d
    let (_:Rect2D) = Rect2D.rotateWithCenter Pt.Origin (Rotation2D.createFromDegrees 45.0) rect2d
    let (_:Rect2D) = Rect2D.scale 2.0 rect2d
    let (_:Rect2D) = Rect2D.expand 1.0 rect2d
    let (_:Rect2D) = Rect2D.expandRel 0.1 rect2d
    let (_:Rect2D) = Rect2D.expandRelXY 0.1 0.2 rect2d
    let (_:Rect2D) = Rect2D.expandXY 1.0 2.0 rect2d
    let (_:Rect2D) = Rect2D.offset 1.0 rect2d
    let (_:Rect2D) = Rect2D.offsetCorner(rect2d, 0, 1.0, 1.0, 8.0, 4.0)
    let (_:Rect2D) = Rect2D.offsetEdge(rect2d, 0, 1.0, 8.0, 0.5, 0.5)
    let (_:Rect2D) = Rect2D.offsetVar [|1.0; 1.0; 1.0; 1.0|] rect2d
    let (_:Pt[][]) = Rect2D.grid(rect2d, 2, 2)
    let (_:Pt[][]) = Rect2D.gridMinLength(rect2d, 3.0, 3.0)
    let (_:Pt[][]) = Rect2D.gridMaxLength(rect2d, 3.0, 3.0)
    let (_:Rect2D[][]) = Rect2D.subDivide(rect2d, 2, 2, 0.1, 0.1)
    let (_:Rect2D[][]) = Rect2D.subDivideMinLength(rect2d, 3.0, 3.0, 0.1, 0.1)
    let (_:Rect2D[][]) = Rect2D.subDivideMaxLength(rect2d, 3.0, 3.0, 0.1, 0.1)
    let (_:bool) = Rect2D.contains (Pt(5., 2.5)) rect2d

    // Static members - Comparison
    let (_:bool) = Rect2D.equals 0.001 rect2d rect2
    let (_:bool) = Rect2D.notEquals 0.001 rect2d rect2

// ===== Rect3D (3D Rectangle with origin and axes) =====

module Rect3DAPI =
    // Creation via static methods
    let rect3d = Rect3D.createFromPlane(PPlane.WorldXY, 10.0, 5.0)
    let rect2 = Rect3D.createFromPlane(PPlane.WorldTop, 8.0, 4.0)

    // Instance members - String
    let (_:string) = rect3d.ToString()
    let (_:string) = rect3d.AsString

    // Instance members - Properties
    let (_:Pnt) = rect3d.Pt0
    let (_:Pnt) = rect3d.Pt1
    let (_:Pnt) = rect3d.Pt2
    let (_:Pnt) = rect3d.Pt3
    let (_:Pnt[]) = rect3d.Points
    let (_:Pnt[]) = rect3d.PointsLooped
    let (_:Line3D) = rect3d.Edge01
    let (_:Line3D) = rect3d.Edge12
    let (_:Line3D) = rect3d.Edge23
    let (_:Line3D) = rect3d.Edge30
    let (_:Line3D) = rect3d.EdgeX
    let (_:Line3D) = rect3d.EdgeY
    let (_:Line3D[]) = rect3d.Edges
    let (_:Pnt) = rect3d.Center
    let (_:Pnt) = rect3d.XCorner
    let (_:Pnt) = rect3d.YCorner
    let (_:Pnt) = rect3d.FarCorner
    let (_:UnitVec) = rect3d.XaxisUnit
    let (_:UnitVec) = rect3d.YaxisUnit
    let (_:UnitVec) = rect3d.NormalUnit
    let (_:Vec) = rect3d.Normal
    let (_:PPlane) = rect3d.Plane
    let (_:float) = rect3d.SizeX
    let (_:float) = rect3d.SizeY
    let (_:float) = rect3d.SizeXSq
    let (_:float) = rect3d.SizeYSq
    let (_:float) = rect3d.Width
    let (_:float) = rect3d.Height2D
    let (_:float) = rect3d.Area
    let (_:float) = rect3d.AreaSq
    let (_:Vec) = rect3d.Diagonal
    let (_:Line3D) = rect3d.DiagonalLine
    let (_:float) = rect3d.LongestEdge
    let (_:float) = rect3d.LongestEdgeSq
    let (_:float) = rect3d.ShortestEdge
    let (_:float) = rect3d.ShortestEdgeSq
    let (_:int) = rect3d.CountZeroSides
    let (_:bool) = rect3d.IsValid
    let (_:bool) = rect3d.IsZero
    let (_:bool) = rect3d.IsPoint
    let (_:bool) = rect3d.IsLine
    // let (_:bool) = rect3d.HasVolume
    let (_:BRect) = rect3d.BRect
    let (_:BBox) = rect3d.BBox
    let (_:Rect3D) = rect3d.Flipped
    let (_:Rect3D) = rect3d.RotateOrientation90CW
    let (_:Rect3D) = rect3d.RotateOrientation90CCW
    let (_:Rect3D) = rect3d.RotateOrientation180

    // Instance members - Operations
    let (_:Pnt) = rect3d.EvaluateAt(0.5, 0.5)
    let (_:Pnt) = rect3d.EvaluateDist(5.0, 2.5)
    let (_:Line3D) = rect3d.GetEdge 0
    let (_:Rect3D) = rect3d.Scale 2.0
    let (_:Rect3D) = rect3d.ScaleOn Pnt.Origin 2.0

    // Static members - Creation
    let (_:Rect3D) = Rect3D.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 5., 0.))
    let (_:Rect3D) = Rect3D.createFromBRect (BRect.create(Pt.Origin, Pt(10., 5.)))
    let (_:Rect3D) = Rect3D.createFromVectors(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 5., 0.))
    let (_:Rect3D) = Rect3D.createFromPlane(PPlane.WorldXY, 10.0, 5.0)
    let (_:Rect3D) = Rect3D.createCenteredFromPlane(PPlane.WorldXY, 10.0, 5.0)
    let (_:Rect3D) = Rect3D.createFrom3Points(Pnt.Origin, Pnt(10., 0., 0.), Pnt(0., 5., 0.))
    let (_:Rect3D option) = Rect3D.tryCreateFrom3Points(Pnt.Origin, Pnt(10., 0., 0.), Pnt(0., 5., 0.))

    // Static members - Operations
    let (_:Rect3D) = Rect3D.translate (Vec(1., 1., 1.)) rect3d
    let (_:Rect3D) = Rect3D.translateLocalX 1.0 rect3d
    let (_:Rect3D) = Rect3D.translateLocalY 1.0 rect3d
    let (_:Rect3D) = Rect3D.move (Vec(1., 1., 1.)) rect3d
    let (_:Rect3D) = Rect3D.transform (Matrix.identity) rect3d
    let (_:Rect3D) = Rect3D.scale 2.0 rect3d
    let (_:Rect3D) = Rect3D.expand 1.0 rect3d
    let (_:Rect3D) = Rect3D.expandRel 0.1 rect3d
    let (_:Rect3D) = Rect3D.expandRelXY 0.1 0.2 rect3d
    let (_:Rect3D) = Rect3D.expandXY 1.0 2.0 rect3d
    let (_:Rect3D) = Rect3D.offset 1.0 rect3d
    let (_:Rect3D) = Rect3D.offsetZ 1.0 rect3d
    let (_:Rect3D) = Rect3D.offsetCorner(rect3d, 0, 1.0, 1.0, 8.0, 4.0)
    let (_:Rect3D) = Rect3D.offsetEdge(rect3d, 0, 1.0, 8.0, 0.5, 0.5)
    let (_:Rect3D) = Rect3D.offsetVar [|1.0; 1.0; 1.0; 1.0|] rect3d
    let (_:Rect3D) = Rect3D.flip rect3d
    let (_:Rect3D) = Rect3D.fitToPoints (ResizeArray [Pnt.Origin; Pnt(10., 5., 0.)]) rect3d
    let (_:Pnt[][]) = Rect3D.grid(rect3d, 2, 2)
    let (_:Pnt[][]) = Rect3D.gridMinLength(rect3d, 3.0, 3.0)
    let (_:Pnt[][]) = Rect3D.gridMaxLength(rect3d, 3.0, 3.0)
    let (_:Rect3D[][]) = Rect3D.subDivide(rect3d, 2, 2, 0.1, 0.1)
    let (_:Rect3D[][]) = Rect3D.subDivideMinLength(rect3d, 3.0, 3.0, 0.1, 0.1)
    let (_:Rect3D[][]) = Rect3D.subDivideMaxLength(rect3d, 3.0, 3.0, 0.1, 0.1)

    // Static members - Intersection
    let ln = Line3D(Pnt(5., 2.5, -10.), Pnt(5., 2.5, 10.))
    let (_:Pnt option) = Rect3D.intersectLine ln rect3d
    let (_:float option) = Rect3D.intersectLineParameterInfinite ln rect3d
    let (_:(float*float*float) option) = Rect3D.intersectLineParameters ln rect3d
    let (_:(float*float*float) option) = Rect3D.intersectLineParametersInfinite ln rect3d

    // Static members - Comparison
    let (_:bool) = Rect3D.equals 0.001 rect3d rect2
    let (_:bool) = Rect3D.notEquals 0.001 rect3d rect2

// ===== Box (3D Oriented Box with origin and three axes) =====

module BoxAPI =
    // Creation
    let box:Box = Box.createFromPlane 10.0 5.0 3.0 PPlane.WorldXY
    let box2:Box = Box.createFromPlane 8.0 4.0 2.0 PPlane.WorldTop

    // Instance members - String
    let (_:string) = box.ToString()
    let (_:string) = box.AsString

    // Instance members - Properties
    let (_:Pnt) = box.Pt0
    let (_:Pnt) = box.Pt1
    let (_:Pnt) = box.Pt2
    let (_:Pnt) = box.Pt3
    let (_:Pnt) = box.Pt4
    let (_:Pnt) = box.Pt5
    let (_:Pnt) = box.Pt6
    let (_:Pnt) = box.Pt7
    let (_:Pnt[]) = box.Points
    let (_:Line3D) = box.Edge0
    let (_:Line3D) = box.Edge1
    let (_:Line3D) = box.Edge2
    let (_:Line3D) = box.Edge3
    let (_:Line3D) = box.Edge4
    let (_:Line3D) = box.Edge5
    let (_:Line3D) = box.Edge6
    let (_:Line3D) = box.Edge7
    let (_:Line3D) = box.Edge8
    let (_:Line3D) = box.Edge9
    let (_:Line3D) = box.Edge10
    let (_:Line3D) = box.Edge11
    let (_:Line3D[]) = box.Edges
    let (_:Rect3D) = box.BottomFace
    let (_:Rect3D) = box.TopFace
    let (_:Rect3D) = box.FrontFace
    let (_:Rect3D) = box.BackFace
    let (_:Rect3D) = box.LeftFace
    let (_:Rect3D) = box.RightFace
    let (_:Rect3D[]) = box.Faces
    let (_:Pnt) = box.Center
    let (_:Pnt) = box.FarCorner
    let (_:UnitVec) = box.XaxisUnit
    let (_:UnitVec) = box.YaxisUnit
    let (_:UnitVec) = box.ZaxisUnit
    let (_:PPlane) = box.Plane
    let (_:float) = box.SizeX
    let (_:float) = box.SizeY
    let (_:float) = box.SizeZ
    let (_:float) = box.SizeXSq
    let (_:float) = box.SizeYSq
    let (_:float) = box.SizeZSq
    let (_:float) = box.Width
    let (_:float) = box.Depth
    let (_:float) = box.Height3D
    let (_:float) = box.Volume
    let (_:Vec) = box.Diagonal
    let (_:float) = box.LongestEdge
    let (_:float) = box.LongestEdgeSq
    let (_:float) = box.ShortestEdge
    let (_:float) = box.ShortestEdgeSq
    let (_:int) = box.CountZeroSides
    let (_:bool) = box.IsValid
    let (_:bool) = box.IsZero
    let (_:bool) = box.IsPoint
    let (_:bool) = box.IsLine
    let (_:bool) = box.IsFlat
    let (_:bool) = box.HasVolume
    let (_:BBox) = box.BBox
    let (_:float) = box.AreaSq

    // Instance members - Operations
    let (_:Pnt) = box.EvaluateAt(0.5, 0.5, 0.5)
    let (_:bool) = box.Contains(Pnt(5., 2.5, 1.5))
    let (_:Box) = box.Scale 2.0
    let (_:float) = box.AreaOfBiggestFace
    let (_:float) = box.AreaOfSmallestFace

    // Static members - Creation
    let pts = ResizeArray [Pnt.Origin; Pnt(10., 0., 0.); Pnt(0., 5., 3.)]
    let (_:Box) = Box.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
    let (_:Box) = Box.createFromPlane 1. 1. 1. PPlane.WorldXY
    let (_:Box) = Box.createFromRect2D 3.0 1. (Rect2D.createFromDirectionAndSizes(Pt.Origin, UnitVc.Xaxis, 10.0, 5.0))
    let (_:Box) = Box.createFromRect3D 3.0 1. (Rect3D.createFromPlane(PPlane.WorldXY, 10.0, 5.0))
    let (_:Box) = Box.createFromBoundingBox (BBox.create(Pnt.Origin, Pnt(10., 5., 3.)))
    let (_:Box) = Box.createFromPlaneAndPoints PPlane.WorldXY pts
    let (_:Box) = Box.createFromDirsAndPoints Vec.Xaxis Vec.Yaxis pts

    // Static members - Operations
    let (_:Box) = Box.translateLocalX 1.0 box
    let (_:Box) = Box.translateLocalY 1.0 box
    let (_:Box) = Box.translateLocalZ 1.0 box
    let (_:Box) = Box.move (Vec(1., 1., 1.)) box
    let (_:Box) = Box.transform (Matrix.identity) box
    let (_:Box) = Box.scale 2.0 box
    let (_:Box) = Box.expand 1.0 box
    let (_:Box) = Box.expandRel 0.1 box
    let (_:Box) = Box.expandRelXYZ 0.1 0.2 0.3 box
    let (_:Box) = Box.expandXYZ 1.0 2.0 3.0 box
    let (_:bool) = Box.contains (Pnt(5., 2.5, 1.5)) box

    // Static members - Comparison
    let (_:bool) = Box.equals 0.001 box box2
    let (_:bool) = Box.notEquals 0.001 box box2

// ===== FreeBox (8 arbitrary corner points defining a box-like shape) =====

module FreeBoxAPI =
    // Creation

    let pts = [| Pt(0., 0.); Pt(10., 0.); Pt(10., 10.); Pt(0., 10.) |]
    let freebox = FreeBox.createFromFour2DPoints 1. 2. pts
    let fb2 = FreeBox.createFromFour2DPointsArgs (Pt(0., 0.), Pt(10., 0.), Pt(10., 10.), Pt(0., 10.), 1., 2.)

    // Instance members - Properties
    let (_:Pnt[]) = freebox.Points

    // Instance members - Operations
    let (_:FreeBox) = freebox.Scale 2.0
    let (_:FreeBox) = freebox.ScaleOn Pnt.Origin 2.0

// ===== Polyline2D (2D Polyline) =====

module Polyline2DAPI =
    // Creation
    let polyline2d = Polyline2D.create(ResizeArray [Pt(0., 0.); Pt(10., 0.); Pt(10., 10.); Pt(0., 10.)])
    let pl2 = Polyline2D.createEmpty(0)

    // Instance members - String
    let (_:string) = polyline2d.ToString()

    // Instance members - Properties
    let (_:Pt) = polyline2d.Start
    let (_:Pt) = polyline2d.End
    let (_:Pt) = polyline2d.FirstPoint
    let (_:Pt) = polyline2d.LastPoint
    let (_:Pt) = polyline2d.SecondPoint
    let (_:Pt) = polyline2d.SecondLastPoint
    let (_:Pt ResizeArray) = polyline2d.Points
    let (_:int) = polyline2d.PointCount
    let (_:int) = polyline2d.LastPointIndex
    let (_:Line2D) = polyline2d.FirstSegment
    let (_:Line2D) = polyline2d.LastSegment
    let (_:Line2D ResizeArray) = polyline2d.Segments
    let (_:int) = polyline2d.SegmentCount
    let (_:int) = polyline2d.LastSegmentIndex
    let (_:float) = polyline2d.Length
    let (_:Pt) = polyline2d.Center
    let (_:float) = polyline2d.Area
    let (_:float) = polyline2d.SignedArea
    let (_:BRect) = polyline2d.BoundingRectangle
    let (_:bool) = polyline2d.IsClosed
    let (_:bool) = polyline2d.IsClockwise
    let (_:bool) = polyline2d.IsCounterClockwise

    // Instance members - Operations
    let pt = Pt(5., 5.)
    let (_:Pt) = polyline2d.EvaluateAt 0.5
    let (_:UnitVc) = polyline2d.TangentAt 0.5
    let (_:Line2D) = polyline2d.GetSegment 0
    let (_:bool) = polyline2d.IsAlmostClosed 0.001
    let (_:float) = polyline2d.DistanceTo pt
    let (_:float) = polyline2d.ClosestParameter pt
    let (_:Pt) = polyline2d.ClosestPoint pt
    let (_:bool) = polyline2d.Contains pt
    let (_:int) = polyline2d.WindingNumber pt
    let (_:Polyline2D) = polyline2d.Duplicate()
    let (_:Polyline2D) = polyline2d.Clone()
    let (_:Polyline2D) = polyline2d.Reverse()
    let (_:unit) = polyline2d.ReverseInPlace()
    let (_:unit) = polyline2d.CloseIfOpen(0.0)
    let (_:Polyline2D) = polyline2d.Scale 2.0
    let (_:Polyline2D) = polyline2d.ScaleOn Pt.Origin 2.0

    // Static members - Creation
    let (_:Polyline2D) = Polyline2D.create(ResizeArray [Pt(0., 0.); Pt(10., 0.); Pt(10., 10.)])
    let (_:Polyline2D) = Polyline2D.createEmpty(0)
    let (_:Polyline2D) = Polyline2D.createDirectlyUnsafe(ResizeArray [Pt(0., 0.); Pt(10., 0.); Pt(10., 10.)])

    // Static members - Properties
    let (_:Pt) = Polyline2D.start polyline2d
    let (_:Pt) = Polyline2D.ende polyline2d

    // Static members - Operations
    let vc = Vc(1., 1.)
    let (_:Pt) = Polyline2D.evaluateAt 0.5 polyline2d
    let (_:Polyline2D) = Polyline2D.translate  vc polyline2d
    let (_:Polyline2D) = Polyline2D.move  vc polyline2d
    let (_:Polyline2D) = Polyline2D.moveX 1.0 polyline2d
    let (_:Polyline2D) = Polyline2D.moveY 1.0 polyline2d
    let (_:Polyline2D) = Polyline2D.rotate (Rotation2D.createFromDegrees 45.0) polyline2d
    let (_:Polyline2D) = Polyline2D.rotateWithCenter Pt.Origin (Rotation2D.createFromDegrees 45.0) polyline2d
    let (_:Polyline2D) = Polyline2D.scale 2.0 polyline2d
    let (_:Polyline2D) = Polyline2D.reverse polyline2d
    let (_:unit) = Polyline2D.reverseInPlace polyline2d
    let (_:Polyline2D) = Polyline2D.close polyline2d
    let (_:unit) = Polyline2D.closeInPlace polyline2d
    let (_:Polyline2D) = Polyline2D.subPolyline 0.2 0.8 polyline2d
    let (_:float) = Polyline2D.closestParameter polyline2d pt
    let (_:Pt) = Polyline2D.closestPoint polyline2d pt
    let (_:float) = Polyline2D.distanceTo polyline2d pt
    let (_:Polyline2D) = Polyline2D.map (fun p -> p + Vc(1., 1.)) polyline2d
    let (_:Pt ResizeArray) = Polyline2D.pointsUnsafeInternal polyline2d // returns accessor function
    let (_:Polyline2D) = Polyline2D.offset (polyline2d, 1.0)
    let (_:Polyline2D) = Polyline2D.offsetVar (polyline2d,[|1.0|])

    // Note: findOuterCornerAndRefNormal and offsetCore are internal/private

    // Static members - Comparison
    let (_:bool) = Polyline2D.equals 0.001 polyline2d polyline2d

// ===== Polyline3D (3D Polyline) =====

module Polyline3DAPI =
    // Creation
    let polyline3d = Polyline3D.create(ResizeArray [Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)])
    let pl2 = Polyline3D.createEmpty(0)

    // Instance members - String
    let (_:string) = polyline3d.ToString()

    // Instance members - Properties
    let (_:Pnt) = polyline3d.Start
    let (_:Pnt) = polyline3d.End
    let (_:Pnt) = polyline3d.FirstPoint
    let (_:Pnt) = polyline3d.LastPoint
    let (_:Pnt) = polyline3d.SecondPoint
    let (_:Pnt) = polyline3d.SecondLastPoint
    let (_:Pnt ResizeArray) = polyline3d.Points
    let (_:int) = polyline3d.PointCount
    let (_:int) = polyline3d.LastPointIndex
    let (_:Line3D) = polyline3d.FirstSegment
    let (_:Line3D) = polyline3d.LastSegment
    let (_:Line3D ResizeArray) = polyline3d.Segments
    let (_:int) = polyline3d.SegmentCount
    let (_:int) = polyline3d.LastSegmentIndex
    let (_:float) = polyline3d.Length
    let (_:Pnt) = polyline3d.Center
    let (_:float) = polyline3d.SignedAreaIn2D
    let (_:BBox) = polyline3d.BoundingBox
    let (_:bool) = polyline3d.IsClosed
    let (_:bool) = polyline3d.IsClockwiseIn2D
    let (_:bool) = polyline3d.IsCounterClockwiseIn2D

    // Instance members - Operations
    let (_:Pnt) = polyline3d.EvaluateAt 0.5
    let (_:UnitVec) = polyline3d.TangentAt 0.5
    let (_:Line3D) = polyline3d.GetSegment 0
    let (_:bool) = polyline3d.IsAlmostClosed 0.001
    let (_:float) = polyline3d.DistanceTo(Pnt(5., 5., 0.))
    let (_:float) = polyline3d.ClosestParameter(Pnt(5., 5., 0.))
    let (_:Pnt) = polyline3d.ClosestPoint(Pnt(5., 5., 0.))
    let (_:Polyline3D) = polyline3d.Duplicate()
    let (_:Polyline3D) = polyline3d.Clone()
    let (_:Polyline3D) = polyline3d.Reverse()
    let (_:unit) = polyline3d.ReverseInPlace()
    let (_:unit) = polyline3d.CloseIfOpen(0.0)
    let (_:Polyline3D) = polyline3d.Scale 2.0
    let (_:Polyline3D) = polyline3d.ScaleOn Pnt.Origin 2.0

    // Static members - Creation
    let (_:Polyline3D) = Polyline3D.create(ResizeArray [Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.)])
    let (_:Polyline3D) = Polyline3D.createEmpty(0)
    let (_:Polyline3D) = Polyline3D.createDirectlyUnsafe(ResizeArray [Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.)])

    // Static members - Properties
    let (_:Pnt) = Polyline3D.start polyline3d
    let (_:Pnt) = Polyline3D.ende polyline3d

    // Static members - Operations
    let (_:Pnt) = Polyline3D.evaluateAt 0.5 polyline3d
    let (_:Polyline3D) = Polyline3D.translate (Vec(1., 1., 1.)) polyline3d
    let (_:Polyline3D) = Polyline3D.transform Matrix.identity polyline3d
    let (_:Polyline3D) = Polyline3D.move (Vec(1., 1., 1.)) polyline3d
    let (_:Polyline3D) = Polyline3D.moveX 1.0 polyline3d
    let (_:Polyline3D) = Polyline3D.moveY 1.0 polyline3d
    let (_:Polyline3D) = Polyline3D.moveZ 1.0 polyline3d
    let (_:Polyline3D) = Polyline3D.rotate (Rotation2D.createFromDegrees 45.0) polyline3d
    let (_:Polyline3D) = Polyline3D.rotateWithCenter Pnt.Origin (Rotation2D.createFromDegrees 45.0) polyline3d
    let (_:Polyline3D) = Polyline3D.scale 2.0 polyline3d
    let (_:Polyline3D) = Polyline3D.reverse polyline3d
    let (_:unit) = Polyline3D.reverseInPlace polyline3d
    let (_:Polyline3D) = Polyline3D.close polyline3d
    let (_:unit) = Polyline3D.closeInPlace polyline3d
    let (_:Polyline3D) = Polyline3D.subPolyline 0.2 0.8 polyline3d
    let (_:float) = Polyline3D.closestParameter polyline3d (Pnt(5., 5., 0.))
    let (_:Pnt) = Polyline3D.closestPoint polyline3d (Pnt(5., 5., 0.))
    let (_:float) = Polyline3D.distanceTo polyline3d (Pnt(5., 5., 0.))
    let (_:Polyline3D) = Polyline3D.map (fun p -> p + Vec(1., 1., 1.)) polyline3d
    let (_:Pnt ResizeArray) = Polyline3D.pointsUnsafeInternal polyline3d
    let (_:Polyline3D) = Polyline3D.offset(polyline3d, 1.0, 0)

    // Note: findOuterCornerAndRefNormal and offsetCore require complex setup or are internal

    // Static members - Comparison
    let (_:bool) = Polyline3D.equals 0.001 polyline3d polyline3d



// ===== Format (Formatting utilities) =====

module FormatAPI =
    // Static members - Formatting
    let (_:string) = Format.float 123.456
    // let (_:string) = Format.addThousandSeparators "1234567"

    // Static members - Thresholds
    let (_:float) = Format.userZeroTolerance

    // Note: Format.Literals is a private module and not accessible


// ===== Points (Point collection utilities) =====

module PointsAPI =
    // 2D points
    let pts2d = ResizeArray [Pt(0., 0.); Pt(10., 0.); Pt(10., 10.); Pt(0., 10.)]
    // 3D points
    let pts3d = ResizeArray [Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(10., 10., 0.); Pnt(0., 10., 0.)]

    // Static members - 2D operations
    let (_:float) = Points.getSignedArea pts2d
    let (_:Pt) = Points.closestPoint(pts2d, Pt(5., 5.))
    let (_:int) = Points.closestPointIdx(pts2d, Pt(5., 5.))
    let (_:(int*int)) = Points.closestPointsIdx(pts2d, pts2d)
    let (_:float) = Points.minDistBetweenPointSets(pts2d, pts2d)
    let (_:(int*int)) = Points.mostDistantPointIdx(pts2d, pts2d)
    let (_:Pt) = Points.mostDistantPoint(pts2d, pts2d)
    let (_:Pnt) = Points.center pts3d
    let (_:ResizeArray<Pt>) = Points.cullDuplicatePointsInSeq(pts2d, 0.001)

    // Static members - 3D operations
    let (_:Pnt) = Points.closestPoint(pts3d, Pnt(5., 5., 5.))
    let (_:int) = Points.closestPointIdx(pts3d, Pnt(5., 5., 5.))
    let (_:(int*int)) = Points.closestPointsIdx(pts3d, pts3d)
    let (_:float) = Points.minDistBetweenPointSets(pts3d, pts3d)
    let (_:(int*int)) = Points.mostDistantPointIdx(pts3d, pts3d)
    let (_:Pnt) = Points.mostDistantPoint(pts3d, pts3d)
    let (_:Pnt) = Points.center pts3d
    let (_:bool) = Points.areInLine(Pnt(0., 0., 0.), Pnt(5., 0., 0.), Pnt(10., 0., 0.), 0.001)
    let (_:float) = Points.areaTriangle(Pnt(0., 0., 0.), Pnt(10., 0., 0.), Pnt(5., 10., 0.))
    let (_:float) = Points.areaTriangleDoubleSq(Pnt(0., 0., 0.), Pnt(10., 0., 0.), Pnt(5., 10., 0.))
    let (_:ResizeArray<Pnt>) = Points.cullDuplicatePointsInSeq(pts3d, 0.001)
    let (_:Vec) = Points.normalOfPoints pts3d

    // Static members - Additional operations
    let (_:Pnt) = Points.closestOfTwo (Pnt(0., 0., 0.)) (Pnt(10., 0., 0.)) (Pnt(3., 0., 0.))
    let (_:bool) = Points.areInLineFast(Pnt(0., 0., 0.), Pnt(5., 0., 0.), Pnt(10., 0., 0.), 0.001)
    // Points.areaTrianglefloatSq doesn't exist, use areaTriangleDoubleSq
    // Points.findContinuousPoints expects ResizeArray<ResizeArray<Pnt>>
    // Points.offsetInCorner/Ex/Ex2D take 5-6 arguments with complex signatures
    // These are used internally for polyline offset calculations

// ===== Util (Math utilities) =====

module UtilAPI =
    // Static members - Tolerance checks
    let (_:bool) = UtilEuclid.isTooSmall 1e-12
    let (_:bool) = UtilEuclid.isTooSmallSq 1e-24
    let (_:bool) = UtilEuclid.isTooTiny 1e-16
    let (_:bool) = UtilEuclid.isTooTinySq 1e-32
    let (_:bool) = UtilEuclid.isNegative -0.001

    // Static members - Angle conversions
    let (_:float) = UtilEuclid.toRadians 180.0
    let (_:float) = UtilEuclid.toDegrees 3.14159

    // Static members - Clamping
    let (_:float) = UtilEuclid.clampBetweenMinusOneAndOne 1.5
    let (_:float) = UtilEuclid.clampBetweenZeroAndOne 1.5

    // Static members - Safe trig
    let (_:float) = UtilEuclid.asinSafe 1.5
    let (_:float) = UtilEuclid.acosSafe 1.5

    // Static members - Value checks
    let (_:bool) = UtilEuclid.isOne 1.0
    let (_:bool) = UtilEuclid.isNotOne 0.5
    let (_:bool) = UtilEuclid.isMinusOne -1.0
    let (_:bool) = UtilEuclid.isZero 0.0
    let (_:bool) = UtilEuclid.isNotZero 0.5
    let (_:bool) = UtilEuclid.isBetweenZeroAndOne 0.5


    // Static members - Other
    let (_:float) = UtilEuclid.matchSign 5.0 (-3.0)
    let (_:int) = UtilEuclid.saveIdx 10 5


// ===== Intersect (Intersection utilities) =====

module IntersectAPI =
    // 3D intersections
    let ln3d = Line3D(Pnt(0., 0., 0.), Pnt(0., 0., 10.))
    let p1 = Pnt(0., 0., 5.)
    let p2 = Pnt(10., 0., 5.)
    let p3 = Pnt(5., 10., 5.)
    let (_:Pnt option) = Intersect.lineTriangle(ln3d, p1, p2, p3)
    let (_) = Intersect.lineCone(ln3d, 5.0, 10.0, 0.0) // coneRadius, coneBaseZ, coneTipZ

    // Note: doIntersectOrOverlapColinear, getRelation, getXPara, getXPointOrMid are internal/private

// ===== Similarity2D (2D similarity transformation) =====

module Similarity2DAPI =
    // Points for similarity testing
    let ptsA = ResizeArray(["cat1", ResizeArray([| Pt(0., 0.); Pt(10., 0.); Pt(10., 10.) |])])
    let ptsB = ResizeArray(["cat1", ResizeArray([| Pt(5., 5.); Pt(15., 5.); Pt(15., 15.) |])])

    // getSimilarityData: (ptss:ResizeArray<string*ResizeArray<Pt>>) -> ObjectToCheck
    let objA = Similarity2D.getSimilarityData ptsA
    let objB = Similarity2D.getSimilarityData ptsB

    // areSimilar: (tol:float) (a:ObjectToCheck) (b:ObjectToCheck) -> bool
    let (_:bool) = Similarity2D.areSimilar 0.001 objA objB

    // getGrouped: (tolerance, items:ResizeArray<'T>, sims:ResizeArray<ObjectToCheck>) -> ResizeArray<ResizeArray<'T>>
    let items = ResizeArray([| 1; 2 |])
    let sims = ResizeArray([| objA; objB |])
    let (_:ResizeArray<ResizeArray<int>>) = Similarity2D.getGrouped(0.001, items, sims)

    // Note: simPt and simPts are private

// ===== Topology (Joining and sorting utilities) =====

module TopologyAPI =
    // 2D lines for topology testing (using ResizeArray for Topology functions)
    let lines2d = ResizeArray([| Line2D(Pt(0., 0.), Pt(10., 0.)); Line2D(Pt(10., 0.), Pt(10., 10.)) |])
    // 3D lines for topology testing (using ResizeArray for Topology functions)
    let lines3d = ResizeArray([| Line3D(Pnt(0., 0., 0.), Pnt(10., 0., 0.)); Line3D(Pnt(10., 0., 0.), Pnt(10., 10., 0.)) |])

    // Note: findIndexByFrom, minIndexByFrom, and swap are private in Topology module

    // Static members - 2D sorting/joining operations
    // sortToLoop2D: (getLine: 'T -> Line2D, xs:ResizeArray<'T>) -> unit
    let () = Topology.sortToLoop2D(id, lines2d)
    // sortToLoopWithReversing2D: (getLine: 'T -> Line2D, reverseInPlace: int -> 'T -> unit, xs:ResizeArray<'T>) -> unit
    let () = Topology.sortToLoopWithReversing2D((fun ln -> ln), (fun _ _ -> ()), lines2d)
    // join2D: (getLine: 'T -> Line2D, splitDistance:float, xs:ResizeArray<'T>) -> ResizeArray<ResizeArray<'T>>
    let (_:ResizeArray<ResizeArray<Line2D>>) = Topology.join2D(id, 0.001, lines2d)
    // joinReversing2D: (getLine: 'T -> Line2D, splitDistance:float, xs:ResizeArray<'T>) -> ResizeArray<ResizeArray<'T * bool>>
    let (_:ResizeArray<ResizeArray<Line2D * bool>>) = Topology.joinReversing2D(id, 0.001, lines2d)

    // Static members - 3D sorting/joining operations
    // sortToLoop3D: (getLine: 'T -> Line3D, xs:ResizeArray<'T>) -> unit
    let () = Topology.sortToLoop3D(id, lines3d)
    // sortToLoopWithReversing3D: (getLine: 'T -> Line3D, reverseInPlace: int -> 'T -> unit, xs:ResizeArray<'T>) -> unit
    let () = Topology.sortToLoopWithReversing3D((fun ln -> ln), (fun _ _ -> ()), lines3d)
    // join3D: (getLine: 'T -> Line3D, splitDistance:float, xs:ResizeArray<'T>) -> ResizeArray<ResizeArray<'T>>
    let (_:ResizeArray<ResizeArray<Line3D>>) = Topology.join3D(id, 0.001, lines3d)
    // joinReversing3D: (getLine: 'T -> Line3D, splitDistance:float, xs:ResizeArray<'T>) -> ResizeArray<ResizeArray<'T * bool>>
    let (_:ResizeArray<ResizeArray<Line3D * bool>>) = Topology.joinReversing3D(id, 0.001, lines3d)

#endif