# Euclid 3D API

All public 3D geometry types of Euclid, with their members and functions.
The general utilities and helper types are left out. See [ApiTable.md](ApiTable.md) for them and for 2D and 3D side by side.

- Instance members and fields start with `.`, e.g. `.DistanceTo`.
- Static members are qualified with their type name. They are listed right below the instance member of the same name.
  The static members without such an instance member follow at the end of each table.
- A property shows only its type. `with get, set` marks a property that can be set. `?name` marks an optional parameter.

Generated from the sources by `Scripts/ApiTable.fsx`. Do not edit by hand.

## Contents

- [Vec](#vec)
- [UnitVec](#unitvec)
- [Pnt](#pnt)
- [Quaternion](#quaternion)
- [PPlane](#pplane)
- [Matrix](#matrix)
- [RigidMatrix](#rigidmatrix)
- [Line3D](#line3d)
- [XLine3D.XCone](#xline3dxcone)
- [XLine3D.XRayParam](#xline3dxrayparam)
- [XLine3D.XRay](#xline3dxray)
- [XLine3D.XParam](#xline3dxparam)
- [XLine3D.XPnt](#xline3dxpnt)
- [XLine3D.ClParams](#xline3dclparams)
- [XLine3D.ClPnts](#xline3dclpnts)
- [XLine3D.XEnds](#xline3dxends)
- [module XLineXYZ](#module-xlinexyz)
- [XLine3D](#xline3d)
- [NPlane](#nplane)
- [Rect3D](#rect3d)
- [Box](#box)
- [FreeBox](#freebox)
- [BBox](#bbox)
- [Tria3D](#tria3d)
- [Points3D](#points3d)
- [module Offset3D](#module-offset3d)
- [Offset3D.VarDistParallel](#offset3dvardistparallel)
- [Offset3D.OffsetDirection](#offset3doffsetdirection)
- [Offset3D.CollinearPoint](#offset3dcollinearpoint)
- [Polyline3D](#polyline3d)
- [Topology3D](#topology3d)

## Vec

| Vec | Signature |
|:--|:--|
| `new` | `x: float * y: float * z: float -> Vec` |
| `.X` | `float` |
| `.Y` | `float` |
| `.Z` | `float` |
| `.AngleDiamondInXYTo` | `b: Vec -> float` |
| `.AsFSharpCode` | `string` |
| `.AsPnt` | `Pnt` |
| `Vec.asPnt` | `v: Vec -> Pnt` |
| `.AsPt` | `Pt` |
| `Vec.asPt` | `v: Vec -> Pt` |
| `.AsString` | `string` |
| `.AsVc` | `Vc` |
| `Vec.asVc` | `v: Vec -> Vc` |
| `.Cross` | `b: UnitVec -> Vec` |
| `.Cross` | `b: Vec -> Vec` |
| `Vec.cross` | `a: Vec * b: Vec -> Vec` |
| `Vec.cross` | `a: Vec * b: UnitVec -> Vec` |
| `Vec.cross` | `a: UnitVec * b: Vec -> Vec` |
| `.Direction180InXY` | `float` |
| `Vec.direction180InXY` | `v: Vec -> float` |
| `.Direction2PiInXY` | `float` |
| `Vec.direction2PiInXY` | `v: Vec -> float` |
| `.Direction360InXY` | `float` |
| `Vec.direction360InXY` | `v: Vec -> float` |
| `.DirectionDiamondInXY` | `float` |
| `Vec.directionDiamondInXY` | `v: Vec -> float` |
| `.DirectionPiInXY` | `float` |
| `Vec.directionPiInXY` | `v: Vec -> float` |
| `.Dot` | `b: Vec -> float` |
| `.Dot` | `b: UnitVec -> float` |
| `Vec.dot` | `a: Vec -> b: Vec -> float` |
| `.Half` | `Vec` |
| `Vec.half` | `v: Vec -> Vec` |
| `.IsHorizontal` | `bool` |
| `Vec.isHorizontal` | `v: Vec -> bool` |
| `.IsNotZero` | `bool` |
| `Vec.isNotZero` | `v: Vec -> bool` |
| `.IsOppositeOrientation` | `other: Vec -> bool` |
| `.IsOppositeOrientation` | `other: UnitVec -> bool` |
| `Vec.isOppositeOrientation` | `v: Vec -> other: Vec -> bool` |
| `.IsParallelAndOrientedTo` | `other: Vec * minCosine: float<cosine> -> bool` |
| `.IsParallelAndOrientedTo` | `other: UnitVec * minCosine: float<cosine> -> bool` |
| `Vec.isParallelAndOrientedTo` | `minCosine: float<cosine> -> a: Vec -> b: Vec -> bool` |
| `.IsParallelTo` | `other: Vec * minCosine: float<cosine> -> bool` |
| `.IsParallelTo` | `other: UnitVec * minCosine: float<cosine> -> bool` |
| `Vec.isParallelTo` | `minCosine: float<cosine> -> a: Vec -> b: Vec -> bool` |
| `.IsPerpendicularTo` | `other: Vec * maxCosine: float<cosine> -> bool` |
| `.IsPerpendicularTo` | `other: UnitVec * maxCosine: float<cosine> -> bool` |
| `Vec.isPerpendicularTo` | `maxCosine: float<cosine> -> a: Vec -> b: Vec -> bool` |
| `.IsTiny` | `tol: float -> bool` |
| `Vec.isTiny` | `tol: float -> v: Vec -> bool` |
| `.IsTinySq` | `tol: float -> bool` |
| `Vec.isTinySq` | `tol: float -> v: Vec -> bool` |
| `.IsUnit` | `bool` |
| `Vec.isUnit` | `v: Vec -> bool` |
| `.IsVertical` | `bool` |
| `Vec.isVertical` | `v: Vec -> bool` |
| `.IsXAligned` | `bool` |
| `Vec.isXAligned` | `v: Vec -> bool` |
| `.IsYAligned` | `bool` |
| `Vec.isYAligned` | `v: Vec -> bool` |
| `.IsZAligned` | `bool` |
| `Vec.isZAligned` | `v: Vec -> bool` |
| `.IsZero` | `bool` |
| `Vec.isZero` | `v: Vec -> bool` |
| `.Length` | `float` |
| `Vec.length` | `v: Vec -> float` |
| `.LengthInXY` | `float` |
| `Vec.lengthInXY` | `v: Vec -> float` |
| `.LengthInXYSq` | `float` |
| `Vec.lengthInXYSq` | `v: Vec -> float` |
| `.LengthSq` | `float` |
| `Vec.lengthSq` | `v: Vec -> float` |
| `.MatchesOrientation` | `other: Vec -> bool` |
| `.MatchesOrientation` | `other: UnitVec -> bool` |
| `Vec.matchesOrientation` | `v: Vec -> other: Vec -> bool` |
| `.PerpendicularInXY` | `Vec` |
| `Vec.perpendicularInXY` | `v: Vec -> Vec` |
| `.Rotate` | `q: Quaternion -> Vec` |
| `Vec.rotate` | `q: Quaternion -> vec: Vec -> Vec` |
| `.Rotate2Dby90CCW` | `Vec` |
| `Vec.rotate2Dby90CCW` | `v: Vec -> Vec` |
| `.Rotate2Dby90CW` | `Vec` |
| `Vec.rotate2Dby90CW` | `v: Vec -> Vec` |
| `.RotateByQuarterCircle` | `numberOfQuarters: int -> Vec` |
| `Vec.rotateByQuarterCircle` | `numberOfQuarters: int -> v: Vec -> Vec` |
| `.ToString` | `unit -> string` |
| `.TransformRigid` | `m: RigidMatrix -> Vec` |
| `Vec.transformRigid` | `m: RigidMatrix -> v: Vec -> Vec` |
| `.Unitized` | `UnitVec` |
| `.WithLength` | `desiredLength: float -> Vec` |
| `Vec.withLength` | `desiredLength: float -> v: Vec -> Vec` |
| `.WithX` | `x: float -> Vec` |
| `Vec.withX` | `x: float -> v: Vec -> Vec` |
| `.WithY` | `y: float -> Vec` |
| `Vec.withY` | `y: float -> v: Vec -> Vec` |
| `.WithZ` | `z: float -> Vec` |
| `Vec.withZ` | `z: float -> v: Vec -> Vec` |
| **Other static members** | |
| `Vec.add` | `a: Vec -> b: Vec -> Vec` |
| `Vec.addX` | `x: float -> v: Vec -> Vec` |
| `Vec.addY` | `y: float -> v: Vec -> Vec` |
| `Vec.addZ` | `z: float -> v: Vec -> Vec` |
| `Vec.angle180` | `a: Vec -> b: Vec -> float` |
| `Vec.angle2PiInXY` | `a: Vec -> b: Vec -> float` |
| `Vec.angle360InXY` | `a: Vec -> b: Vec -> float` |
| `Vec.angle90` | `a: Vec -> b: Vec -> float` |
| `Vec.angleDiamondInXY` | `a: Vec -> b: Vec -> float` |
| `Vec.angleHalfPi` | `a: Vec -> b: Vec -> float` |
| `Vec.anglePi` | `a: Vec -> b: Vec -> float` |
| `Vec.bisector` | `a: Vec -> b: Vec -> Vec` |
| `Vec.create` | `x: float -> y: float -> z: float -> Vec` |
| `Vec.createFromMembersXYZ` | `vec: ^T -> Vec` |
| `Vec.createFromMembersxyz` | `vec: ^T -> Vec` |
| `Vec.createFromPnt` | `pnt: Pnt -> Vec` |
| `Vec.createFromTo` | `start: Pnt -> endValue: Pnt -> Vec` |
| `Vec.createFromUnitVec` | `v: UnitVec -> Vec` |
| `Vec.createFromVcWithZ` | `z: float -> v: Vc -> Vec` |
| `Vec.determinant3` | `u: Vec * v: Vec * w: Vec -> float` |
| `Vec.difference` | `a: Vec -> b: Vec -> float` |
| `Vec.differenceSq` | `a: Vec -> b: Vec -> float` |
| `Vec.DivideByInt` | `v: Vec * i: int -> Vec` |
| `Vec.dotUnitVec` | `a: Vec -> b: UnitVec -> float` |
| `Vec.equalsExact` | `a: Vec -> b: Vec -> bool` |
| `Vec.equalsWithin` | `tol: float -> a: Vec -> b: Vec -> bool` |
| `Vec.flipToPointUp` | `v: Vec -> Vec` |
| `Vec.getX` | `v: Vec -> float` |
| `Vec.getY` | `v: Vec -> float` |
| `Vec.getZ` | `v: Vec -> float` |
| `Vec.isAngleAbove` | `cosineValue: float<cosine> -> a: Vec -> b: Vec -> bool` |
| `Vec.isAngleBelow` | `cosineValue: float<cosine> -> a: Vec -> b: Vec -> bool` |
| `Vec.isNotParallelWithin` | `cosineValue: float<cosine> -> a: Vec -> b: Vec -> bool` |
| `Vec.isParallelAndOrientedToUnitVec` | `minCosine: float<cosine> -> b: UnitVec -> a: Vec -> bool` |
| `Vec.isParallelToUnitVec` | `minCosine: float<cosine> -> b: UnitVec -> a: Vec -> bool` |
| `Vec.isParallelWithin` | `cosineValue: float<cosine> -> a: Vec -> b: Vec -> bool` |
| `Vec.isPerpendicularToUnitVec` | `maxCosine: float<cosine> -> b: UnitVec -> a: Vec -> bool` |
| `Vec.lerp` | `start: Vec -> endValue: Vec -> rel: float -> Vec` |
| `Vec.matchOrientation` | `orientationToMatch: Vec -> vecToFlip: Vec -> Vec` |
| `Vec.matchUnitVecOrientation` | `orientationToMatch: UnitVec -> vecToFlip: Vec -> Vec` |
| `Vec.notEqualsWithin` | `tol: float -> a: Vec -> b: Vec -> bool` |
| `Vec.orientDown` | `v: Vec -> Vec` |
| `Vec.orientUp` | `v: Vec -> Vec` |
| `Vec.perpendicularInVerticalPlane` | `v: Vec -> Vec` |
| `Vec.projectToXYPlane` | `v: Vec -> Vec` |
| `Vec.reverse` | `v: Vec -> Vec` |
| `Vec.rotateOnX` | `r: Rotation2D -> v: Vec -> Vec` |
| `Vec.rotateOnXDegrees` | `angDegree: float -> v: Vec -> Vec` |
| `Vec.rotateOnY` | `r: Rotation2D -> v: Vec -> Vec` |
| `Vec.rotateOnYDegrees` | `angDegree: float -> v: Vec -> Vec` |
| `Vec.rotateOnZ` | `r: Rotation2D -> v: Vec -> Vec` |
| `Vec.rotateOnZDegrees` | `angDegree: float -> v: Vec -> Vec` |
| `Vec.scale` | `f: float -> v: Vec -> Vec` |
| `Vec.slerp` | `start: Vec -> endValue: Vec -> rel: float -> Vec` |
| `Vec.slopeDegrees` | `v: Vec -> float` |
| `Vec.slopePercent` | `v: Vec -> float` |
| `Vec.slopeRadians` | `v: Vec -> float` |
| `Vec.tryUnitize` | `v: Vec -> UnitVec voption` |
| `Vec.unitize` | `v: Vec -> UnitVec` |
| `Vec.unitizeAsVec` | `v: Vec -> Vec` |
| `Vec.unitizeOrDefault` | `defaultUnitVector: UnitVec -> v: Vec -> UnitVec` |
| `Vec.unitizeUnchecked` | `v: Vec -> UnitVec` |
| `Vec.unitizeWithErrorMessage` | `errMsg: string -> v: Vec -> UnitVec` |
| `Vec.xAxis` | `Vec` |
| `Vec.yAxis` | `Vec` |
| `Vec.zAxis` | `Vec` |
| `Vec.Zero` | `Vec` |
| `( *** )` | `a: Vec * b: Vec -> float` |
| `( * )` | `a: Vec * f: float -> Vec` |
| `( * )` | `f: float * a: Vec -> Vec` |
| `( + )` | `a: Vec * b: Vec -> Vec` |
| `( - )` | `a: Vec * b: Vec -> Vec` |
| `( / )` | `v: Vec * f: float -> Vec` |
| `( ~- )` | `v: Vec -> Vec` |

## UnitVec

| UnitVec | Signature |
|:--|:--|
| `.X` | `float` |
| `.Y` | `float` |
| `.Z` | `float` |
| `.AngleDiamondInXYTo` | `b: UnitVec -> float` |
| `.AsFSharpCode` | `string` |
| `.AsPnt` | `Pnt` |
| `UnitVec.asPnt` | `v: UnitVec -> Pnt` |
| `.AsPt` | `Pt` |
| `UnitVec.asPt` | `v: UnitVec -> Pt` |
| `.AsString` | `string` |
| `.AsUnitVc` | `UnitVc` |
| `UnitVec.asUnitVc` | `v: UnitVec -> UnitVc` |
| `.AsVc` | `Vc` |
| `UnitVec.asVc` | `v: UnitVec -> Vc` |
| `.AsVec` | `Vec` |
| `UnitVec.asVec` | `v: UnitVec -> Vec` |
| `.Cross` | `b: UnitVec -> Vec` |
| `.Cross` | `b: Vec -> Vec` |
| `UnitVec.cross` | `a: UnitVec * b: UnitVec -> Vec` |
| `UnitVec.cross` | `a: UnitVec * b: Vec -> Vec` |
| `UnitVec.cross` | `a: Vec * b: UnitVec -> Vec` |
| `.Direction180InXY` | `float` |
| `UnitVec.direction180InXY` | `v: UnitVec -> float` |
| `.Direction2PiInXY` | `float` |
| `UnitVec.direction2PiInXY` | `v: UnitVec -> float` |
| `.Direction360InXY` | `float` |
| `UnitVec.direction360InXY` | `v: UnitVec -> float` |
| `.DirectionDiamondInXY` | `float` |
| `UnitVec.directionDiamondInXY` | `v: UnitVec -> float` |
| `.DirectionPiInXY` | `float` |
| `UnitVec.directionPiInXY` | `v: UnitVec -> float` |
| `.Dot` | `b: UnitVec -> float` |
| `.Dot` | `b: Vec -> float` |
| `UnitVec.dot` | `a: UnitVec -> b: UnitVec -> float` |
| `.DotCosine` | `b: UnitVec -> float<cosine>` |
| `UnitVec.dotCosine` | `a: UnitVec -> b: UnitVec -> float<cosine>` |
| `.IsHorizontal` | `bool` |
| `UnitVec.isHorizontal` | `v: UnitVec -> bool` |
| `.IsOppositeOrientation` | `other: UnitVec -> bool` |
| `.IsOppositeOrientation` | `other: Vec -> bool` |
| `UnitVec.isOppositeOrientation` | `other: UnitVec -> v: UnitVec -> bool` |
| `.IsParallelAndOrientedTo` | `other: UnitVec * minCosine: float<cosine> -> bool` |
| `.IsParallelAndOrientedTo` | `other: Vec * minCosine: float<cosine> -> bool` |
| `UnitVec.isParallelAndOrientedTo` | `minCosine: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `.IsParallelTo` | `other: UnitVec * minCosine: float<cosine> -> bool` |
| `.IsParallelTo` | `other: Vec * minCosine: float<cosine> -> bool` |
| `UnitVec.isParallelTo` | `minCosine: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `.IsPerpendicularTo` | `other: UnitVec * maxCosine: float<cosine> -> bool` |
| `.IsPerpendicularTo` | `other: Vec * maxCosine: float<cosine> -> bool` |
| `UnitVec.isPerpendicularTo` | `maxCosine: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `.IsVertical` | `bool` |
| `UnitVec.isVertical` | `v: UnitVec -> bool` |
| `.IsXAligned` | `bool` |
| `UnitVec.isXAligned` | `v: UnitVec -> bool` |
| `.IsYAligned` | `bool` |
| `UnitVec.isYAligned` | `v: UnitVec -> bool` |
| `.IsZAligned` | `bool` |
| `UnitVec.isZAligned` | `v: UnitVec -> bool` |
| `.LengthInXY` | `float` |
| `UnitVec.lengthInXY` | `v: UnitVec -> float` |
| `.LengthInXYSq` | `float` |
| `UnitVec.lengthInXYSq` | `v: UnitVec -> float` |
| `.MatchesOrientation` | `other: UnitVec -> bool` |
| `.MatchesOrientation` | `other: Vec -> bool` |
| `UnitVec.matchesOrientation` | `other: UnitVec -> v: UnitVec -> bool` |
| `.PerpendicularInXY` | `Vec` |
| `UnitVec.perpendicularInXY` | `v: UnitVec -> Vec` |
| `.Rotate` | `q: Quaternion -> UnitVec` |
| `UnitVec.rotate` | `q: Quaternion -> uVec: UnitVec -> UnitVec` |
| `.Rotate2Dby90CCW` | `UnitVec` |
| `UnitVec.rotate2Dby90CCW` | `v: UnitVec -> UnitVec` |
| `.Rotate2Dby90CW` | `UnitVec` |
| `UnitVec.rotate2Dby90CW` | `v: UnitVec -> UnitVec` |
| `.RotateByQuarterCircle` | `numberOfQuarters: int -> UnitVec` |
| `UnitVec.rotateByQuarterCircle` | `numberOfQuarters: int -> v: UnitVec -> UnitVec` |
| `.ToString` | `unit -> string` |
| `.TransformRigid` | `m: RigidMatrix -> UnitVec` |
| `UnitVec.transformRigid` | `m: RigidMatrix -> v: UnitVec -> UnitVec` |
| `.WithX` | `x: float -> Vec` |
| `UnitVec.withX` | `x: float -> v: UnitVec -> Vec` |
| `.WithY` | `y: float -> Vec` |
| `UnitVec.withY` | `y: float -> v: UnitVec -> Vec` |
| `.WithZ` | `z: float -> Vec` |
| `UnitVec.withZ` | `z: float -> v: UnitVec -> Vec` |
| **Other static members** | |
| `UnitVec.add` | `a: UnitVec -> b: UnitVec -> Vec` |
| `UnitVec.addX` | `x: float -> v: UnitVec -> Vec` |
| `UnitVec.addY` | `y: float -> v: UnitVec -> Vec` |
| `UnitVec.addZ` | `z: float -> v: UnitVec -> Vec` |
| `UnitVec.angle180` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVec.angle2PiInXY` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVec.angle360InXY` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVec.angle90` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVec.angleDiamondInXY` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVec.angleHalfPi` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVec.anglePi` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVec.create` | `x: float -> y: float -> z: float -> UnitVec` |
| `UnitVec.createFromMembersXYZ` | `vec: ^T -> UnitVec` |
| `UnitVec.createFromMembersxyz` | `vec: ^T -> UnitVec` |
| `UnitVec.createFromPnt` | `pnt: Pnt -> UnitVec` |
| `UnitVec.createFromTo` | `fromPnt: Pnt -> toPnt: Pnt -> UnitVec` |
| `UnitVec.createFromVec` | `v: Vec -> UnitVec` |
| `UnitVec.createUnchecked` | `x: float * y: float * z: float -> UnitVec` |
| `UnitVec.createWithErrorMessage` | `msg: string -> x: float -> y: float -> z: float -> UnitVec` |
| `UnitVec.determinant3` | `u: UnitVec * v: UnitVec * w: UnitVec -> float` |
| `UnitVec.difference` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVec.differenceSq` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVec.dotVec` | `a: UnitVec -> b: Vec -> float` |
| `UnitVec.equalsExact` | `a: UnitVec -> b: UnitVec -> bool` |
| `UnitVec.equalsWithin` | `tol: float -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVec.flipToPointUp` | `v: UnitVec -> UnitVec` |
| `UnitVec.getX` | `v: UnitVec -> float` |
| `UnitVec.getY` | `v: UnitVec -> float` |
| `UnitVec.getZ` | `v: UnitVec -> float` |
| `UnitVec.isAngleAbove` | `cosineValue: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVec.isAngleBelow` | `cosineValue: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVec.isNotParallelWithin` | `cosineValue: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVec.isParallelWithin` | `cosineValue: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVec.lerp` | `start: UnitVec -> endValue: UnitVec -> rel: float -> Vec` |
| `UnitVec.matchOrientation` | `orientationToMatch: UnitVec -> vecToFlip: UnitVec -> UnitVec` |
| `UnitVec.matchVecOrientation` | `orientationToMatch: Vec -> vecToFlip: UnitVec -> UnitVec` |
| `UnitVec.notEqualsWithin` | `tol: float -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVec.orientDown` | `v: UnitVec -> UnitVec` |
| `UnitVec.orientUp` | `v: UnitVec -> UnitVec` |
| `UnitVec.perpendicularInVerticalPlane` | `v: UnitVec -> Vec` |
| `UnitVec.projectToXYPlane` | `v: UnitVec -> Vec` |
| `UnitVec.reverse` | `v: UnitVec -> UnitVec` |
| `UnitVec.rotateOnX` | `r: Rotation2D -> v: UnitVec -> UnitVec` |
| `UnitVec.rotateOnXDegrees` | `angDegree: float -> v: UnitVec -> UnitVec` |
| `UnitVec.rotateOnY` | `r: Rotation2D -> v: UnitVec -> UnitVec` |
| `UnitVec.rotateOnYDegrees` | `angDegree: float -> v: UnitVec -> UnitVec` |
| `UnitVec.rotateOnZ` | `r: Rotation2D -> v: UnitVec -> UnitVec` |
| `UnitVec.rotateOnZDegrees` | `angDegree: float -> v: UnitVec -> UnitVec` |
| `UnitVec.scale` | `f: float -> v: UnitVec -> Vec` |
| `UnitVec.sine` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVec.slerp` | `start: UnitVec -> endValue: UnitVec -> rel: float -> UnitVec` |
| `UnitVec.slopeDegrees` | `v: UnitVec -> float` |
| `UnitVec.slopePercent` | `v: UnitVec -> float` |
| `UnitVec.slopeRadians` | `v: UnitVec -> float` |
| `UnitVec.tangent` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVec.withLength` | `f: float -> v: UnitVec -> Vec` |
| `UnitVec.xAxis` | `UnitVec` |
| `UnitVec.yAxis` | `UnitVec` |
| `UnitVec.zAxis` | `UnitVec` |
| `( *** )` | `a: UnitVec * b: UnitVec -> float` |
| `( *** )` | `a: UnitVec * b: Vec -> float` |
| `( *** )` | `a: Vec * b: UnitVec -> float` |
| `( * )` | `a: UnitVec * f: float -> Vec` |
| `( * )` | `f: float * a: UnitVec -> Vec` |
| `( + )` | `a: UnitVec * b: UnitVec -> Vec` |
| `( + )` | `a: Vec * b: UnitVec -> Vec` |
| `( + )` | `a: UnitVec * b: Vec -> Vec` |
| `( - )` | `a: UnitVec * b: UnitVec -> Vec` |
| `( - )` | `a: UnitVec * b: Vec -> Vec` |
| `( - )` | `a: Vec * b: UnitVec -> Vec` |
| `( / )` | `v: UnitVec * f: float -> Vec` |
| `( ~- )` | `v: UnitVec -> UnitVec` |

## Pnt

| Pnt | Signature |
|:--|:--|
| `new` | `x: float * y: float * z: float -> Pnt` |
| `.X` | `float` |
| `.Y` | `float` |
| `.Z` | `float` |
| `.Angle2PiInXYTo` | `o: Pnt -> float` |
| `Pnt.angle2PiInXYTo` | `fromPnt: Pnt -> toPnt: Pnt -> float` |
| `.Angle360InXYTo` | `o: Pnt -> float` |
| `Pnt.angle360InXYTo` | `fromPnt: Pnt -> toPnt: Pnt -> float` |
| `.AsFSharpCode` | `string` |
| `.AsPt` | `Pt` |
| `Pnt.asPt` | `pnt: Pnt -> Pt` |
| `.AsString` | `string` |
| `.AsVc` | `Vc` |
| `Pnt.asVc` | `pnt: Pnt -> Vc` |
| `.AsVec` | `Vec` |
| `Pnt.asVec` | `pnt: Pnt -> Vec` |
| `.ClosestPointOnLine` | `ln: Line3D -> Pnt` |
| `Pnt.closestPointOnLine` | `ln: Line3D -> testPnt: Pnt -> Pnt` |
| `.DirectionDiamondInXYTo` | `o: Pnt -> float` |
| `Pnt.directionDiamondInXYTo` | `fromPnt: Pnt -> toPnt: Pnt -> float` |
| `.DistanceFromOrigin` | `float` |
| `Pnt.distanceFromOrigin` | `pnt: Pnt -> float` |
| `.DistanceFromOriginSq` | `float` |
| `Pnt.distanceFromOriginSq` | `pnt: Pnt -> float` |
| `.DistanceInXYFromOrigin` | `float` |
| `Pnt.distanceInXYFromOrigin` | `pnt: Pnt -> float` |
| `.DistanceInXYFromOriginSq` | `float` |
| `Pnt.distanceInXYFromOriginSq` | `pnt: Pnt -> float` |
| `.DistanceTo` | `b: Pnt -> float` |
| `Pnt.distanceTo` | `b: Pnt -> p: Pnt -> float` |
| `.DistanceToLine` | `ln: Line3D -> float` |
| `Pnt.distanceToLine` | `ln: Line3D -> testPnt: Pnt -> float` |
| `.DistanceToLineSq` | `ln: Line3D -> float` |
| `Pnt.distanceToLineSq` | `ln: Line3D -> testPnt: Pnt -> float` |
| `.DistanceToSq` | `b: Pnt -> float` |
| `Pnt.distanceToSq` | `b: Pnt -> p: Pnt -> float` |
| `.DistanceToXYZ` | `x: float * y: float * z: float -> float` |
| `Pnt.distanceToXYZ` | `x: float -> y: float -> z: float -> p: Pnt -> float` |
| `.DistanceToXYZSq` | `x: float * y: float * z: float -> float` |
| `Pnt.distanceToXYZSq` | `x: float -> y: float -> z: float -> p: Pnt -> float` |
| `.IsAlmostOrigin` | `tol: float -> bool` |
| `Pnt.isAlmostOrigin` | `tol: float -> pnt: Pnt -> bool` |
| `.IsInvalid` | `bool` |
| `Pnt.isInvalid` | `pnt: Pnt -> bool` |
| `.IsNotOrigin` | `bool` |
| `Pnt.isNotOrigin` | `pnt: Pnt -> bool` |
| `.IsOrigin` | `bool` |
| `Pnt.isOrigin` | `pnt: Pnt -> bool` |
| `.IsValid` | `bool` |
| `Pnt.isValid` | `pnt: Pnt -> bool` |
| `.Rotate` | `q: Quaternion -> Pnt` |
| `Pnt.rotate` | `q: Quaternion -> pnt: Pnt -> Pnt` |
| `.Rotate2Dby90CCW` | `Pnt` |
| `Pnt.rotate2Dby90CCW` | `pnt: Pnt -> Pnt` |
| `.Rotate2Dby90CW` | `Pnt` |
| `Pnt.rotate2Dby90CW` | `pnt: Pnt -> Pnt` |
| `.RotateAround` | `cen: Pnt -> q: Quaternion -> Pnt` |
| `Pnt.rotateAround` | `cen: Pnt -> q: Quaternion -> pnt: Pnt -> Pnt` |
| `.RotateByQuarterCircle` | `numberOfQuarters: int -> Pnt` |
| `Pnt.rotateByQuarterCircle` | `numberOfQuarters: int -> pnt: Pnt -> Pnt` |
| `.ToString` | `unit -> string` |
| `.Transform` | `m: Matrix -> Pnt` |
| `Pnt.transform` | `m: Matrix -> p: Pnt -> Pnt` |
| `.TransformRigid` | `m: RigidMatrix -> Pnt` |
| `Pnt.transformRigid` | `m: RigidMatrix -> p: Pnt -> Pnt` |
| `.TransformRigidRotateOnly` | `m: RigidMatrix -> Pnt` |
| `Pnt.transformRigidRotateOnly` | `m: RigidMatrix -> p: Pnt -> Pnt` |
| `.WithDistanceFromOrigin` | `l: float -> Pnt` |
| `Pnt.withDistanceFromOrigin` | `l: float -> pnt: Pnt -> Pnt` |
| `.WithX` | `x: float -> Pnt` |
| `Pnt.withX` | `x: float -> pnt: Pnt -> Pnt` |
| `.WithY` | `y: float -> Pnt` |
| `Pnt.withY` | `y: float -> pnt: Pnt -> Pnt` |
| `.WithZ` | `z: float -> Pnt` |
| `Pnt.withZ` | `z: float -> pnt: Pnt -> Pnt` |
| **Other static members** | |
| `Pnt.add` | `a: Pnt -> b: Pnt -> Pnt` |
| `Pnt.angle180Pnts` | `pntPrev: Pnt -> pntThis: Pnt -> pntNext: Pnt -> float` |
| `Pnt.angleInCorner` | `prevPnt: Pnt -> thisPnt: Pnt -> nextPnt: Pnt -> float` |
| `Pnt.anglePiPnts` | `pntPrev: Pnt -> pntThis: Pnt -> pntNext: Pnt -> float` |
| `Pnt.bisector` | `pntPrev: Pnt -> pntThis: Pnt -> pntNext: Pnt -> Vec` |
| `Pnt.closestOfTwo` | `pnt1: Pnt -> pnt2: Pnt -> referencePoint: Pnt -> Pnt` |
| `Pnt.create` | `x: float -> y: float -> z: float -> Pnt` |
| `Pnt.createFromMembersXYZ` | `pt: ^T -> Pnt` |
| `Pnt.createFromMembersxyz` | `pt: ^T -> Pnt` |
| `Pnt.createFromPt` | `p: Pt -> Pnt` |
| `Pnt.createFromPtWithZ` | `z: float -> p: Pt -> Pnt` |
| `Pnt.createFromUnitVec` | `v: UnitVec -> Pnt` |
| `Pnt.createFromVec` | `v: Vec -> Pnt` |
| `Pnt.distanceInXY` | `a: Pnt -> b: Pnt -> float` |
| `Pnt.distPnt` | `fromPnt: Pnt -> dirPnt: Pnt -> distance: float -> Pnt` |
| `Pnt.DivideByInt` | `pnt: Pnt * i: int -> Pnt` |
| `Pnt.divPnt` | `fromPnt: Pnt -> toPnt: Pnt -> rel: float -> Pnt` |
| `Pnt.equalsExact` | `a: Pnt -> b: Pnt -> bool` |
| `Pnt.equalsWithin` | `tol: float -> a: Pnt -> b: Pnt -> bool` |
| `Pnt.extendToZLevel` | `fromPnt: Pnt -> toPnt: Pnt -> z: float -> Pnt` |
| `Pnt.getX` | `pnt: Pnt -> float` |
| `Pnt.getY` | `pnt: Pnt -> float` |
| `Pnt.getZ` | `pnt: Pnt -> float` |
| `Pnt.lerp` | `fromPnt: Pnt -> toPnt: Pnt -> rel: float -> Pnt` |
| `Pnt.midPnt` | `a: Pnt -> b: Pnt -> Pnt` |
| `Pnt.move` | `shift: Vec -> pnt: Pnt -> Pnt` |
| `Pnt.moveX` | `x: float -> pnt: Pnt -> Pnt` |
| `Pnt.moveY` | `y: float -> pnt: Pnt -> Pnt` |
| `Pnt.moveZ` | `z: float -> pnt: Pnt -> Pnt` |
| `Pnt.normalOf3Pnts` | `a: Pnt -> b: Pnt -> c: Pnt -> Vec` |
| `Pnt.normalOfTwoPointsInXY` | `fromPnt: Pnt -> toPnt: Pnt -> Vec` |
| `Pnt.notEqualsWithin` | `tol: float -> a: Pnt -> b: Pnt -> bool` |
| `Pnt.Origin` | `Pnt` |
| `Pnt.projectedParameterBetweenPnts` | `fromPnt: Pnt -> toPnt: Pnt -> testPnt: Pnt -> float` |
| `Pnt.projectedParameterOnUnitVec` | `fromPnt: Pnt -> uv: UnitVec -> testPnt: Pnt -> float` |
| `Pnt.projectedParameterOnVec` | `fromPnt: Pnt -> v: Vec -> testPnt: Pnt -> float` |
| `Pnt.projectToXYPlane` | `pnt: Pnt -> Pnt` |
| `Pnt.rotateAroundOnX` | `cen: Pnt -> r: Rotation2D -> pnt: Pnt -> Pnt` |
| `Pnt.rotateAroundOnXDegrees` | `cen: Pnt -> angDegree: float -> pnt: Pnt -> Pnt` |
| `Pnt.rotateAroundOnY` | `cen: Pnt -> r: Rotation2D -> pnt: Pnt -> Pnt` |
| `Pnt.rotateAroundOnYDegrees` | `cen: Pnt -> angDegree: float -> pnt: Pnt -> Pnt` |
| `Pnt.rotateAroundOnZ` | `cen: Pnt -> r: Rotation2D -> pnt: Pnt -> Pnt` |
| `Pnt.rotateAroundOnZDegrees` | `cen: Pnt -> angDegree: float -> pnt: Pnt -> Pnt` |
| `Pnt.rotateOnX` | `r: Rotation2D -> p: Pnt -> Pnt` |
| `Pnt.rotateOnXDegrees` | `angDegree: float -> pnt: Pnt -> Pnt` |
| `Pnt.rotateOnY` | `r: Rotation2D -> p: Pnt -> Pnt` |
| `Pnt.rotateOnYDegrees` | `angDegree: float -> pnt: Pnt -> Pnt` |
| `Pnt.rotateOnZ` | `r: Rotation2D -> p: Pnt -> Pnt` |
| `Pnt.rotateOnZDegrees` | `angDegree: float -> pnt: Pnt -> Pnt` |
| `Pnt.scale` | `f: float -> pnt: Pnt -> Pnt` |
| `Pnt.scaleOn` | `cen: Pnt -> factor: float -> pnt: Pnt -> Pnt` |
| `Pnt.snap` | `precision: float -> pnt: Pnt -> Pnt` |
| `Pnt.snapIfClose` | `snapDistance: float -> snapTo: Pnt -> pnt: Pnt -> Pnt` |
| `Pnt.Zero` | `Pnt` |
| `( * )` | `a: Pnt * f: float -> Pnt` |
| `( * )` | `f: float * a: Pnt -> Pnt` |
| `( + )` | `a: Pnt * b: Pnt -> Pnt` |
| `( + )` | `p: Pnt * v: Vec -> Pnt` |
| `( + )` | `p: Pnt * v: UnitVec -> Pnt` |
| `( - )` | `a: Pnt * b: Pnt -> Vec` |
| `( - )` | `p: Pnt * v: UnitVec -> Pnt` |
| `( - )` | `p: Pnt * v: Vec -> Pnt` |
| `( / )` | `p: Pnt * f: float -> Pnt` |

## Quaternion

| Quaternion | Signature |
|:--|:--|
| `.X` | `float` |
| `.Y` | `float` |
| `.Z` | `float` |
| `.W` | `float` |
| `.AngleInDegrees` | `float` |
| `Quaternion.angleInDegrees` | `q: Quaternion -> float` |
| `.AngleInRadians` | `float` |
| `Quaternion.angleInRadians` | `q: Quaternion -> float` |
| `.AsFSharpCode` | `string` |
| `Quaternion.asFSharpCode` | `q: Quaternion -> string` |
| `.AsString` | `string` |
| `Quaternion.asString` | `q: Quaternion -> string` |
| `.Axis` | `Vec` |
| `Quaternion.axis` | `q: Quaternion -> Vec` |
| `.Conjugate` | `Quaternion` |
| `Quaternion.conjugate` | `q: Quaternion -> Quaternion` |
| `.Half` | `Quaternion` |
| `Quaternion.half` | `rotation: Quaternion -> Quaternion` |
| `.Inverse` | `Quaternion` |
| `Quaternion.inverse` | `q: Quaternion -> Quaternion` |
| `.Normalize` | `unit -> Quaternion` |
| `Quaternion.normalize` | `q: Quaternion -> Quaternion` |
| `.Then` | `other: Quaternion -> Quaternion` |
| `.ToString` | `unit -> string` |
| `.WithAngleInDegrees` | `angleInDegrees: float -> Quaternion` |
| `Quaternion.withAngleInDegrees` | `angleInDegrees: float -> q: Quaternion -> Quaternion` |
| `.WithAngleInRadians` | `angleInRadians: float -> Quaternion` |
| `Quaternion.withAngleInRadians` | `angleInRadians: float -> q: Quaternion -> Quaternion` |
| **Other static members** | |
| `Quaternion.andThen` | `next: Quaternion -> q: Quaternion -> Quaternion` |
| `Quaternion.create` | `x: float * y: float * z: float * w: float -> Quaternion` |
| `Quaternion.createFromEulerXYZDegrees` | `degreesX: float -> degreesY: float -> degreesZ: float -> Quaternion` |
| `Quaternion.createFromEulerXYZRadians` | `radiansX: float -> radiansY: float -> radiansZ: float -> Quaternion` |
| `Quaternion.createFromEulerXZYDegrees` | `degreesX: float -> degreesZ: float -> degreesY: float -> Quaternion` |
| `Quaternion.createFromEulerXZYRadians` | `radiansX: float -> radiansZ: float -> radiansY: float -> Quaternion` |
| `Quaternion.createFromEulerYXZDegrees` | `degreesY: float -> degreesX: float -> degreesZ: float -> Quaternion` |
| `Quaternion.createFromEulerYXZRadians` | `radiansY: float -> radiansX: float -> radiansZ: float -> Quaternion` |
| `Quaternion.createFromEulerYZXDegrees` | `degreesY: float -> degreesZ: float -> degreesX: float -> Quaternion` |
| `Quaternion.createFromEulerYZXRadians` | `radiansY: float -> radiansZ: float -> radiansX: float -> Quaternion` |
| `Quaternion.createFromEulerZXYDegrees` | `degreesZ: float -> degreesX: float -> degreesY: float -> Quaternion` |
| `Quaternion.createFromEulerZXYRadians` | `radiansZ: float -> radiansX: float -> radiansY: float -> Quaternion` |
| `Quaternion.createFromEulerZYXDegrees` | `degreesZ: float -> degreesY: float -> degreesX: float -> Quaternion` |
| `Quaternion.createFromEulerZYXRadians` | `radiansZ: float -> radiansY: float -> radiansX: float -> Quaternion` |
| `Quaternion.createFromRigidMatrix` | `orthoMatrix: RigidMatrix -> Quaternion` |
| `Quaternion.createFromUnitVecDegrees` | `axis: UnitVec -> angleInDegrees: float -> Quaternion` |
| `Quaternion.createFromUnitVecRadians` | `axis: UnitVec -> angleInRadians: float -> Quaternion` |
| `Quaternion.createFromUnitVecs` | `vecFrom: UnitVec -> vecTo: UnitVec -> Quaternion` |
| `Quaternion.createFromVecDegrees` | `axis: Vec -> angleInDegrees: float -> Quaternion` |
| `Quaternion.createFromVecRadians` | `axis: Vec -> angleInRadians: float -> Quaternion` |
| `Quaternion.createFromVecs` | `vecFrom: Vec -> vecTo: Vec -> Quaternion` |
| `Quaternion.createUnchecked` | `x: float * y: float * z: float * w: float -> Quaternion` |
| `Quaternion.equalsExact` | `a: Quaternion -> b: Quaternion -> bool` |
| `Quaternion.equalsRotationWithin` | `tolerance: float -> a: Quaternion -> b: Quaternion -> bool` |
| `Quaternion.equalsWithin` | `tolerance: float -> a: Quaternion -> b: Quaternion -> bool` |
| `Quaternion.identity` | `Quaternion` |
| `Quaternion.multiply` | `l: Quaternion * r: Quaternion -> Quaternion` |
| `Quaternion.toEulerAnglesZYX` | `q: Quaternion -> float * float * float` |
| `Quaternion.tryCreateFromMatrix` | `matrix: Matrix -> Quaternion voption` |
| `( *** )` | `l: Quaternion * r: Quaternion -> Quaternion` |
| `( *** )` | `v: Vec * q: Quaternion -> Vec` |
| `( *** )` | `v: UnitVec * q: Quaternion -> UnitVec` |
| `( *** )` | `p: Pnt * q: Quaternion -> Pnt` |

## PPlane

| PPlane | Signature |
|:--|:--|
| `.OriginX` | `float` |
| `.OriginY` | `float` |
| `.OriginZ` | `float` |
| `.XAxisX` | `float` |
| `.XAxisY` | `float` |
| `.XAxisZ` | `float` |
| `.YAxisX` | `float` |
| `.YAxisY` | `float` |
| `.YAxisZ` | `float` |
| `.ZAxisX` | `float` |
| `.ZAxisY` | `float` |
| `.ZAxisZ` | `float` |
| `.Angle90ToLine` | `ln: Line3D -> float` |
| `PPlane.angle90ToLine` | `ln: Line3D -> pl: PPlane -> float` |
| `.Angle90ToPlane` | `pl: PPlane -> float` |
| `PPlane.angle90ToPlane` | `other: PPlane -> pl: PPlane -> float` |
| `.Angle90ToVec` | `v: Vec -> float` |
| `.Angle90ToVec` | `v: UnitVec -> float` |
| `PPlane.angle90ToVec` | `v: Vec -> pl: PPlane -> float` |
| `.AsFSharpCode` | `string` |
| `PPlane.asFSharpCode` | `pl: PPlane -> string` |
| `.AsString` | `string` |
| `PPlane.asString` | `pl: PPlane -> string` |
| `.ClosestParameters` | `p: Pnt -> float * float` |
| `PPlane.closestParameters` | `p: Pnt -> pl: PPlane -> float * float` |
| `.ClosestPoint` | `pnt: Pnt -> Pnt` |
| `PPlane.closestPoint` | `pnt: Pnt -> pl: PPlane -> Pnt` |
| `.DistanceToPnt` | `pnt: Pnt -> float` |
| `PPlane.distanceToPnt` | `pnt: Pnt -> pl: PPlane -> float` |
| `.DistanceToPntSigned` | `pnt: Pnt -> float` |
| `PPlane.distanceToPntSigned` | `pnt: Pnt -> pl: PPlane -> float` |
| `.DistanceToXYZ` | `x: float * y: float * z: float -> float` |
| `PPlane.distanceToXYZ` | `x: float -> y: float -> z: float -> pl: PPlane -> float` |
| `.DistanceToXYZSigned` | `x: float * y: float * z: float -> float` |
| `PPlane.distanceToXYZSigned` | `x: float -> y: float -> z: float -> pl: PPlane -> float` |
| `.EvaluateAt` | `px: float * py: float -> Pnt` |
| `PPlane.evaluateAt` | `px: float -> py: float -> pl: PPlane -> Pnt` |
| `.EvaluateAtXYZ` | `px: float * py: float * pz: float -> Pnt` |
| `PPlane.evaluateAtXYZ` | `px: float -> py: float -> pz: float -> pl: PPlane -> Pnt` |
| `.IsCoincidentTo` | `other: PPlane * distanceTolerance: float * minCosine: float<cosine> -> bool` |
| `PPlane.isCoincidentTo` | `distanceTolerance: float -> minCosine: float<cosine> -> a: PPlane -> b: PPlane -> bool` |
| `.Origin` | `Pnt` |
| `.PlaneAtClPnt` | `pnt: Pnt -> PPlane` |
| `PPlane.planeAtClPnt` | `pnt: Pnt -> pl: PPlane -> PPlane` |
| `.PointParameters` | `pnt: Pnt -> float * float * float` |
| `PPlane.pointParameters` | `pnt: Pnt -> pl: PPlane -> float * float * float` |
| `.ToString` | `unit -> string` |
| `.XAxis` | `UnitVec` |
| `.YAxis` | `UnitVec` |
| `.ZAxis` | `UnitVec` |
| **Other static members** | |
| `PPlane.angle90ToUnitVec` | `v: UnitVec -> pl: PPlane -> float` |
| `PPlane.create` | `xAxis: UnitVec -> yAxis: UnitVec -> zAxis: UnitVec -> origin: Pnt -> PPlane` |
| `PPlane.createFrom3Pnts` | `origin: Pnt -> xPnt: Pnt -> yPnt: Pnt -> PPlane` |
| `PPlane.createFromAxesUnchecked` | `origin: Pnt * xAxis: UnitVec * yAxis: UnitVec * zAxis: UnitVec -> PPlane` |
| `PPlane.createFromNormal` | `normal: Vec -> origin: Pnt -> PPlane` |
| `PPlane.createFromNormalAndXAxis` | `normal: Vec -> xAxis: Vec -> origin: Pnt -> PPlane` |
| `PPlane.createFromUnitNormal` | `normal: UnitVec -> origin: Pnt -> PPlane` |
| `PPlane.createFromUnitNormalAndXAxis` | `normal: UnitVec -> xAxis: UnitVec -> origin: Pnt -> PPlane` |
| `PPlane.createFromUnitXAxisAndYAxis` | `xAxis: UnitVec -> yAxis: UnitVec -> origin: Pnt -> PPlane` |
| `PPlane.createFromXAxisAndYAxis` | `xAxis: Vec -> yAxis: Vec -> origin: Pnt -> PPlane` |
| `PPlane.createUnchecked` | `originX: float * originY: float * originZ: float * xAxisX: float * xAxisY: float * xAxisZ: float * yAxisX: float * yAxisY: float * yAxisZ: float * zAxisX: float * zAxisY: float * zAxisZ: float -> PPlane` |
| `PPlane.doLinePlaneIntersect` | `ln: Line3D -> pl: PPlane -> bool` |
| `PPlane.equalsExact` | `a: PPlane -> b: PPlane -> bool` |
| `PPlane.equalsWithin` | `tol: float -> a: PPlane -> b: PPlane -> bool` |
| `PPlane.flipOnX` | `pl: PPlane -> PPlane` |
| `PPlane.flipOnY` | `pl: PPlane -> PPlane` |
| `PPlane.intersect` | `a: PPlane -> b: PPlane -> Line3D` |
| `PPlane.intersectLine` | `ln: Line3D -> pl: PPlane -> Pnt` |
| `PPlane.intersectRay` | `ln: Line3D -> pl: PPlane -> float` |
| `PPlane.intersectRayAllParameters` | `ln: Line3D -> pl: PPlane -> float * float * float` |
| `PPlane.move` | `translation: Vec -> pl: PPlane -> PPlane` |
| `PPlane.moveAlongLocalX` | `d: float -> pl: PPlane -> PPlane` |
| `PPlane.moveAlongLocalY` | `d: float -> pl: PPlane -> PPlane` |
| `PPlane.moveAlongLocalZ` | `d: float -> pl: PPlane -> PPlane` |
| `PPlane.notEqualsWithin` | `tol: float -> a: PPlane -> b: PPlane -> bool` |
| `PPlane.offset` | `d: float -> pl: PPlane -> PPlane` |
| `PPlane.rotateOnZ180` | `pl: PPlane -> PPlane` |
| `PPlane.rotateZ` | `angDegree: float -> pl: PPlane -> PPlane` |
| `PPlane.rotateZ180IfYNegative` | `pl: PPlane -> PPlane` |
| `PPlane.scale` | `factor: float -> pl: PPlane -> PPlane` |
| `PPlane.transformRigid` | `m: RigidMatrix -> pl: PPlane -> PPlane` |
| `PPlane.translateBy` | `v: Vec -> pl: PPlane -> PPlane` |
| `PPlane.translateByWorldX` | `x: float -> pl: PPlane -> PPlane` |
| `PPlane.translateByWorldY` | `y: float -> pl: PPlane -> PPlane` |
| `PPlane.translateByWorldZ` | `z: float -> pl: PPlane -> PPlane` |
| `PPlane.tryIntersect` | `a: PPlane -> b: PPlane -> Line3D voption` |
| `PPlane.tryIntersectLine` | `ln: Line3D -> pl: PPlane -> Pnt voption` |
| `PPlane.tryIntersectRay` | `ln: Line3D -> pl: PPlane -> float voption` |
| `PPlane.tryIntersectRayAllParameters` | `ln: Line3D -> pl: PPlane -> (float * float * float) voption` |
| `PPlane.withOrigin` | `pnt: Pnt -> pl: PPlane -> PPlane` |
| `PPlane.withOriginX` | `x: float -> pl: PPlane -> PPlane` |
| `PPlane.withOriginY` | `y: float -> pl: PPlane -> PPlane` |
| `PPlane.withOriginZ` | `z: float -> pl: PPlane -> PPlane` |
| `PPlane.WorldBack` | `PPlane` |
| `PPlane.WorldBottom` | `PPlane` |
| `PPlane.WorldFront` | `PPlane` |
| `PPlane.WorldLeft` | `PPlane` |
| `PPlane.WorldMinusXMinusY` | `PPlane` |
| `PPlane.WorldMinusYX` | `PPlane` |
| `PPlane.WorldRight` | `PPlane` |
| `PPlane.WorldTop` | `PPlane` |
| `PPlane.WorldXMinusY` | `PPlane` |
| `PPlane.WorldXY` | `PPlane` |
| `PPlane.WorldYMinusX` | `PPlane` |

## Matrix

| Matrix | Signature |
|:--|:--|
| `new` | `m11: float * m21: float * m31: float * x41: float * m12: float * m22: float * m32: float * y42: float * m13: float * m23: float * m33: float * z43: float * m14: float * m24: float * m34: float * m44: float -> Matrix` |
| `.M11` | `float` |
| `.M21` | `float` |
| `.M31` | `float` |
| `.X41` | `float` |
| `.M12` | `float` |
| `.M22` | `float` |
| `.M32` | `float` |
| `.Y42` | `float` |
| `.M13` | `float` |
| `.M23` | `float` |
| `.M33` | `float` |
| `.Z43` | `float` |
| `.M14` | `float` |
| `.M24` | `float` |
| `.M34` | `float` |
| `.M44` | `float` |
| `.AsFSharpCode` | `string` |
| `Matrix.asFSharpCode` | `m: Matrix -> string` |
| `.AsString` | `string` |
| `Matrix.asString` | `m: Matrix -> string` |
| `.ColumnVector1` | `Vec` |
| `Matrix.columnVector1` | `m: Matrix -> Vec` |
| `.ColumnVector2` | `Vec` |
| `Matrix.columnVector2` | `m: Matrix -> Vec` |
| `.ColumnVector3` | `Vec` |
| `Matrix.columnVector3` | `m: Matrix -> Vec` |
| `.Determinant` | `float` |
| `Matrix.determinant` | `m: Matrix -> float` |
| `.Inverse` | `Matrix` |
| `Matrix.inverse` | `m: Matrix -> Matrix` |
| `.IsAffine` | `bool` |
| `Matrix.isAffine` | `m: Matrix -> bool` |
| `.IsIdentity` | `bool` |
| `Matrix.isIdentity` | `m: Matrix -> bool` |
| `.IsMirroring` | `bool` |
| `Matrix.isMirroring` | `m: Matrix -> bool` |
| `.IsOnlyTranslating` | `bool` |
| `.IsOrthogonal` | `bool` |
| `Matrix.isOrthogonal` | `m: Matrix -> bool` |
| `.IsProjecting` | `bool` |
| `Matrix.isProjecting` | `m: Matrix -> bool` |
| `.IsReflecting` | `bool` |
| `Matrix.isReflecting` | `m: Matrix -> bool` |
| `.IsScaling` | `bool` |
| `Matrix.isScaling` | `m: Matrix -> bool` |
| `.IsTranslating` | `bool` |
| `Matrix.isTranslating` | `m: Matrix -> bool` |
| `.Then` | `other: Matrix -> Matrix` |
| `.ToArrayByColumns` | `float array` |
| `Matrix.toArrayByColumns` | `m: Matrix -> float array` |
| `.ToArrayByRows` | `float array` |
| `Matrix.toArrayByRows` | `m: Matrix -> float array` |
| `.ToString` | `unit -> string` |
| `.Translation` | `Vec` |
| `Matrix.translation` | `m: Matrix -> Vec` |
| **Other static members** | |
| `Matrix.addTranslation` | `v: Vec -> m: Matrix -> Matrix` |
| `Matrix.addTranslationXYZ` | `x: float -> y: float -> z: float -> m: Matrix -> Matrix` |
| `Matrix.andThen` | `next: Matrix -> m: Matrix -> Matrix` |
| `Matrix.createFromColumnMajorArray` | `xs: float array -> Matrix` |
| `Matrix.createFromQuaternion` | `quaternion: Quaternion -> Matrix` |
| `Matrix.createFromRigidMatrix` | `m: RigidMatrix -> Matrix` |
| `Matrix.createFromRowMajorArray` | `xs: float array -> Matrix` |
| `Matrix.createLookAt` | `up: Vec -> eye: Pnt -> target: Pnt -> Matrix` |
| `Matrix.createLookAtZUp` | `eye: Pnt -> target: Pnt -> Matrix` |
| `Matrix.createMirror` | `p: PPlane -> Matrix` |
| `Matrix.createPerspectiveAlongNegZ` | `width: float * height: float * nearPlaneDistance: float * farPlaneDistance: float -> Matrix` |
| `Matrix.createPerspectiveAlongPosY` | `width: float * height: float * nearPlaneDistance: float * farPlaneDistance: float -> Matrix` |
| `Matrix.createPerspectiveLookAt` | `eye: Pnt * target: Pnt * up: Vec * width: float * height: float * nearPlaneDistance: float * farPlaneDistance: float -> Matrix` |
| `Matrix.createPerspectiveLookAtZUp` | `eye: Pnt * target: Pnt * width: float * height: float * nearPlaneDistance: float * farPlaneDistance: float -> Matrix` |
| `Matrix.createPlaneToPlane` | `fromPlane: PPlane -> toPlane: PPlane -> Matrix` |
| `Matrix.createRotationAroundUnitVecDegrees` | `cen: Pnt -> axis: UnitVec -> angleDegrees: float -> Matrix` |
| `Matrix.createRotationAroundUnitVecRadians` | `cen: Pnt -> axis: UnitVec -> angleRadians: float -> Matrix` |
| `Matrix.createRotationAroundVecDegrees` | `cen: Pnt -> axis: Vec -> angleDegrees: float -> Matrix` |
| `Matrix.createRotationAroundVecRadians` | `cen: Pnt -> axis: Vec -> angleRadians: float -> Matrix` |
| `Matrix.createRotationFromUnitVecDegrees` | `axis: UnitVec -> angleDegrees: float -> Matrix` |
| `Matrix.createRotationFromUnitVecRadians` | `axis: UnitVec -> angleRadians: float -> Matrix` |
| `Matrix.createRotationFromUnitVecs` | `vecFrom: UnitVec -> vecTo: UnitVec -> Matrix` |
| `Matrix.createRotationFromVecDegrees` | `axis: Vec -> angleDegrees: float -> Matrix` |
| `Matrix.createRotationFromVecRadians` | `axis: Vec -> angleRadians: float -> Matrix` |
| `Matrix.createRotationFromVecs` | `vecFrom: Vec -> vecTo: Vec -> Matrix` |
| `Matrix.createRotationXDegrees` | `angleDegrees: float -> Matrix` |
| `Matrix.createRotationXRadians` | `angleRadians: float -> Matrix` |
| `Matrix.createRotationYDegrees` | `angleDegrees: float -> Matrix` |
| `Matrix.createRotationYRadians` | `angleRadians: float -> Matrix` |
| `Matrix.createRotationZDegrees` | `angleDegrees: float -> Matrix` |
| `Matrix.createRotationZRadians` | `angleRadians: float -> Matrix` |
| `Matrix.createScale` | `x: float -> y: float -> z: float -> Matrix` |
| `Matrix.createShear` | `xy: float * xz: float * yx: float * yz: float * zx: float * zy: float -> Matrix` |
| `Matrix.createToPlane` | `p: PPlane -> Matrix` |
| `Matrix.createTranslation` | `v: Vec -> Matrix` |
| `Matrix.createTranslationXYZ` | `x: float -> y: float -> z: float -> Matrix` |
| `Matrix.equalsExact` | `a: Matrix -> b: Matrix -> bool` |
| `Matrix.equalsWithin` | `tol: float -> a: Matrix -> b: Matrix -> bool` |
| `Matrix.identity` | `Matrix` |
| `Matrix.multiply` | `matrixA: Matrix * matrixB: Matrix -> Matrix` |
| `Matrix.transformXYZ` | `x: float -> y: float -> z: float -> m: Matrix -> Pnt` |
| `Matrix.transpose` | `m: Matrix -> Matrix` |
| `( *** )` | `matrixA: Matrix * matrixB: Matrix -> Matrix` |
| `( *** )` | `p: Pnt * m: Matrix -> Pnt` |

## RigidMatrix

| RigidMatrix | Signature |
|:--|:--|
| `.M11` | `float` |
| `.M21` | `float` |
| `.M31` | `float` |
| `.X41` | `float` |
| `.M12` | `float` |
| `.M22` | `float` |
| `.M32` | `float` |
| `.Y42` | `float` |
| `.M13` | `float` |
| `.M23` | `float` |
| `.M33` | `float` |
| `.Z43` | `float` |
| `.AsFSharpCode` | `string` |
| `RigidMatrix.asFSharpCode` | `m: RigidMatrix -> string` |
| `.AsString` | `string` |
| `RigidMatrix.asString` | `m: RigidMatrix -> string` |
| `.ColumnVector1` | `Vec` |
| `RigidMatrix.columnVector1` | `m: RigidMatrix -> Vec` |
| `.ColumnVector2` | `Vec` |
| `RigidMatrix.columnVector2` | `m: RigidMatrix -> Vec` |
| `.ColumnVector3` | `Vec` |
| `RigidMatrix.columnVector3` | `m: RigidMatrix -> Vec` |
| `.Inverse` | `RigidMatrix` |
| `RigidMatrix.inverse` | `m: RigidMatrix -> RigidMatrix` |
| `.IsIdentity` | `bool` |
| `RigidMatrix.isIdentity` | `m: RigidMatrix -> bool` |
| `.Then` | `other: RigidMatrix -> RigidMatrix` |
| `.ToArrayByColumns` | `float array` |
| `RigidMatrix.toArrayByColumns` | `m: RigidMatrix -> float array` |
| `.ToArrayByRows` | `float array` |
| `RigidMatrix.toArrayByRows` | `m: RigidMatrix -> float array` |
| `.ToMatrix` | `Matrix` |
| `.ToString` | `unit -> string` |
| `.Translation` | `Vec` |
| `RigidMatrix.translation` | `m: RigidMatrix -> Vec` |
| **Other static members** | |
| `RigidMatrix.addTranslation` | `v: Vec -> m: RigidMatrix -> RigidMatrix` |
| `RigidMatrix.addTranslationXYZ` | `x: float -> y: float -> z: float -> m: RigidMatrix -> RigidMatrix` |
| `RigidMatrix.andThen` | `next: RigidMatrix -> m: RigidMatrix -> RigidMatrix` |
| `RigidMatrix.create` | `m11: float * m21: float * m31: float * x41: float * m12: float * m22: float * m32: float * y42: float * m13: float * m23: float * m33: float * z43: float -> RigidMatrix` |
| `RigidMatrix.createFromMatrix` | `m: Matrix -> RigidMatrix` |
| `RigidMatrix.createFromQuaternion` | `quaternion: Quaternion -> RigidMatrix` |
| `RigidMatrix.createPlaneToPlane` | `fromPlane: PPlane -> toPlane: PPlane -> RigidMatrix` |
| `RigidMatrix.createRotationAroundUnitVecDegrees` | `cen: Pnt -> axis: UnitVec -> angleDegrees: float -> RigidMatrix` |
| `RigidMatrix.createRotationAroundUnitVecRadians` | `cen: Pnt -> axis: UnitVec -> angleRadians: float -> RigidMatrix` |
| `RigidMatrix.createRotationAroundVecDegrees` | `cen: Pnt -> axis: Vec -> angleDegrees: float -> RigidMatrix` |
| `RigidMatrix.createRotationAroundVecRadians` | `cen: Pnt -> axis: Vec -> angleRadians: float -> RigidMatrix` |
| `RigidMatrix.createRotationFromUnitVecDegrees` | `axis: UnitVec -> angleDegrees: float -> RigidMatrix` |
| `RigidMatrix.createRotationFromUnitVecRadians` | `axis: UnitVec -> angleRadians: float -> RigidMatrix` |
| `RigidMatrix.createRotationFromUnitVecs` | `vecFrom: UnitVec -> vecTo: UnitVec -> RigidMatrix` |
| `RigidMatrix.createRotationFromVecDegrees` | `axis: Vec -> angleDegrees: float -> RigidMatrix` |
| `RigidMatrix.createRotationFromVecRadians` | `axis: Vec -> angleRadians: float -> RigidMatrix` |
| `RigidMatrix.createRotationFromVecs` | `vecFrom: Vec -> vecTo: Vec -> RigidMatrix` |
| `RigidMatrix.createRotationXDegrees` | `angleDegrees: float -> RigidMatrix` |
| `RigidMatrix.createRotationXRadians` | `angleRadians: float -> RigidMatrix` |
| `RigidMatrix.createRotationYDegrees` | `angleDegrees: float -> RigidMatrix` |
| `RigidMatrix.createRotationYRadians` | `angleRadians: float -> RigidMatrix` |
| `RigidMatrix.createRotationZDegrees` | `angleDegrees: float -> RigidMatrix` |
| `RigidMatrix.createRotationZRadians` | `angleRadians: float -> RigidMatrix` |
| `RigidMatrix.createToPlane` | `p: PPlane -> RigidMatrix` |
| `RigidMatrix.createTranslation` | `v: Vec -> RigidMatrix` |
| `RigidMatrix.createTranslationX` | `x: float -> RigidMatrix` |
| `RigidMatrix.createTranslationXYZ` | `x: float -> y: float -> z: float -> RigidMatrix` |
| `RigidMatrix.createTranslationY` | `y: float -> RigidMatrix` |
| `RigidMatrix.createTranslationZ` | `z: float -> RigidMatrix` |
| `RigidMatrix.equalsExact` | `a: RigidMatrix -> b: RigidMatrix -> bool` |
| `RigidMatrix.equalsWithin` | `tol: float -> a: RigidMatrix -> b: RigidMatrix -> bool` |
| `RigidMatrix.identity` | `RigidMatrix` |
| `RigidMatrix.multiply` | `matrixA: RigidMatrix * matrixB: RigidMatrix -> RigidMatrix` |
| `RigidMatrix.removeTranslation` | `m: RigidMatrix -> RigidMatrix` |
| `RigidMatrix.transformPntXYZ` | `x: float -> y: float -> z: float -> m: RigidMatrix -> Pnt` |
| `RigidMatrix.transformVecXYZ` | `x: float -> y: float -> z: float -> m: RigidMatrix -> Vec` |
| `RigidMatrix.tryCreateFromMatrix` | `m: Matrix -> RigidMatrix voption` |
| `( *** )` | `matrixA: RigidMatrix * matrixB: RigidMatrix -> RigidMatrix` |
| `( *** )` | `v: Vec * m: RigidMatrix -> Vec` |
| `( *** )` | `v: UnitVec * m: RigidMatrix -> UnitVec` |
| `( *** )` | `v: Pnt * m: RigidMatrix -> Pnt` |

## Line3D

| Line3D | Signature |
|:--|:--|
| `new` | `fromPnt: Pnt * toPnt: Pnt -> Line3D` |
| `new` | `fromX: float * fromY: float * fromZ: float * toX: float * toY: float * toZ: float -> Line3D` |
| `.FromX` | `float` |
| `.FromY` | `float` |
| `.FromZ` | `float` |
| `.ToX` | `float` |
| `.ToY` | `float` |
| `.ToZ` | `float` |
| `.AsFSharpCode` | `string` |
| `Line3D.asFSharpCode` | `ln: Line3D -> string` |
| `.AsString` | `string` |
| `Line3D.asString` | `ln: Line3D -> string` |
| `.ClosestParameter` | `p: Pnt -> float` |
| `Line3D.closestParameter` | `p: Pnt -> ln: Line3D -> float` |
| `.ClosestParameters` | `lnB: Line3D -> float * float` |
| `Line3D.closestParameters` | `lnA: Line3D -> lnB: Line3D -> float * float` |
| `.ClosestParameterXYZ` | `x: float * y: float * z: float -> float` |
| `Line3D.closestParameterXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
| `.ClosestPoint` | `p: Pnt -> Pnt` |
| `Line3D.closestPoint` | `p: Pnt -> ln: Line3D -> Pnt` |
| `.ClosestPoints` | `lnB: Line3D -> Pnt * Pnt` |
| `Line3D.closestPoints` | `lnA: Line3D -> lnB: Line3D -> Pnt * Pnt` |
| `.Direction` | `Vec` |
| `Line3D.direction` | `ln: Line3D -> Vec` |
| `.DistanceRayPoint` | `p: Pnt -> float` |
| `Line3D.distanceRayPoint` | `p: Pnt -> ln: Line3D -> float` |
| `.DistanceRayPointSq` | `p: Pnt -> float` |
| `Line3D.distanceRayPointSq` | `p: Pnt -> ln: Line3D -> float` |
| `.DistanceRayXYZ` | `x: float * y: float * z: float -> float` |
| `Line3D.distanceRayXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
| `.DistanceRayXYZSq` | `x: float * y: float * z: float -> float` |
| `Line3D.distanceRayXYZSq` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
| `.DistanceToPnt` | `p: Pnt -> float` |
| `Line3D.distanceToPnt` | `p: Pnt -> ln: Line3D -> float` |
| `.DistanceToPntSq` | `p: Pnt -> float` |
| `Line3D.distanceToPntSq` | `p: Pnt -> ln: Line3D -> float` |
| `.DistanceToXYZ` | `x: float * y: float * z: float -> float` |
| `Line3D.distanceToXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
| `.DistanceToXYZSq` | `x: float * y: float * z: float -> float` |
| `Line3D.distanceToXYZSq` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
| `.End` | `Pnt` |
| `.EvaluateAt` | `p: float -> Pnt` |
| `Line3D.evaluateAt` | `t: float -> ln: Line3D -> Pnt` |
| `.Extend` | `distAtStart: float * distAtEnd: float -> Line3D` |
| `Line3D.extend` | `distAtStart: float -> distAtEnd: float -> ln: Line3D -> Line3D` |
| `.ExtendEnd` | `distAtEnd: float -> Line3D` |
| `Line3D.extendEnd` | `distAtEnd: float -> ln: Line3D -> Line3D` |
| `.ExtendEndRel` | `relAtEnd: float -> Line3D` |
| `Line3D.extendEndRel` | `relAtEnd: float -> ln: Line3D -> Line3D` |
| `.ExtendRel` | `relAtStart: float * relAtEnd: float -> Line3D` |
| `Line3D.extendRel` | `relAtStart: float -> relAtEnd: float -> ln: Line3D -> Line3D` |
| `.ExtendStart` | `distAtStart: float -> Line3D` |
| `Line3D.extendStart` | `distAtStart: float -> ln: Line3D -> Line3D` |
| `.ExtendStartRel` | `relAtStart: float -> Line3D` |
| `Line3D.extendStartRel` | `relAtStart: float -> ln: Line3D -> Line3D` |
| `.From` | `Pnt` |
| `Line3D.from` | `l: Line3D -> Pnt` |
| `.IsCoincidentAndOpposingToFast` | `other: Line3D * squaredParallelogramAreaTolerance: float * maxDotProduct: float -> bool` |
| `Line3D.isCoincidentAndOpposingToFast` | `squaredParallelogramAreaTolerance: float -> maxDotProduct: float -> lnA: Line3D -> lnB: Line3D -> bool` |
| `.IsCoincidentAndOrientedToFast` | `other: Line3D * squaredParallelogramAreaTolerance: float * minDotProduct: float -> bool` |
| `Line3D.isCoincidentAndOrientedToFast` | `squaredParallelogramAreaTolerance: float -> minDotProduct: float -> other: Line3D -> ln: Line3D -> bool` |
| `.IsCoincidentTo` | `other: Line3D * distanceTolerance: float * minTangent: float<tangent> -> bool` |
| `Line3D.isCoincidentTo` | `distanceTolerance: float -> minTangent: float<tangent> -> a: Line3D -> b: Line3D -> bool` |
| `.IsCoincidentToFast` | `other: Line3D * squaredParallelogramAreaTolerance: float -> bool` |
| `Line3D.isCoincidentToFast` | `squaredParallelogramAreaTolerance: float -> other: Line3D -> ln: Line3D -> bool` |
| `.IsHorizontal` | `bool` |
| `Line3D.isHorizontal` | `l: Line3D -> bool` |
| `.IsNormalTo` | `other: Line3D * maxTangent: float<tangent> -> bool` |
| `.IsNormalTo` | `other: Vec * maxTangent: float<tangent> -> bool` |
| `.IsNormalTo` | `other: UnitVec * maxTangent: float<tangent> -> bool` |
| `Line3D.isNormalTo` | `maxTangent: float<tangent> -> lnA: Line3D -> lnB: Line3D -> bool` |
| `.IsParallelAndOpposingToFast` | `other: Line3D * squaredParallelogramAreaTolerance: float * maxDotProduct: float -> bool` |
| `Line3D.isParallelAndOpposingToFast` | `squaredParallelogramAreaTolerance: float -> maxDotProduct: float -> other: Line3D -> ln: Line3D -> bool` |
| `.IsParallelAndOrientedTo` | `other: Line3D * minTangent: float<tangent> -> bool` |
| `.IsParallelAndOrientedTo` | `other: Vec * minTangent: float<tangent> -> bool` |
| `.IsParallelAndOrientedTo` | `other: UnitVec * minTangent: float<tangent> -> bool` |
| `Line3D.isParallelAndOrientedTo` | `minTangent: float<tangent> -> lnA: Line3D -> lnB: Line3D -> bool` |
| `.IsParallelAndOrientedToFast` | `other: Line3D * squaredParallelogramAreaTolerance: float * minDotProduct: float -> bool` |
| `Line3D.isParallelAndOrientedToFast` | `squaredParallelogramAreaTolerance: float -> minDotProduct: float -> other: Line3D -> ln: Line3D -> bool` |
| `.IsParallelTo` | `other: Line3D * minTangent: float<tangent> -> bool` |
| `.IsParallelTo` | `other: Vec * minTangent: float<tangent> -> bool` |
| `.IsParallelTo` | `other: UnitVec * minTangent: float<tangent> -> bool` |
| `Line3D.isParallelTo` | `minTangent: float<tangent> -> lnA: Line3D -> lnB: Line3D -> bool` |
| `.IsParallelToFast` | `other: Line3D * squaredParallelogramAreaTolerance: float -> bool` |
| `Line3D.isParallelToFast` | `squaredParallelogramAreaTolerance: float -> other: Line3D -> ln: Line3D -> bool` |
| `.IsPerpendicularTo` | `other: Line3D * maxTangent: float<tangent> -> bool` |
| `.IsPerpendicularTo` | `other: Vec * maxTangent: float<tangent> -> bool` |
| `.IsPerpendicularTo` | `other: UnitVec * maxTangent: float<tangent> -> bool` |
| `Line3D.isPerpendicularTo` | `maxTangent: float<tangent> -> lnA: Line3D -> lnB: Line3D -> bool` |
| `.IsTiny` | `tol: float -> bool` |
| `Line3D.isTiny` | `tol: float -> l: Line3D -> bool` |
| `.IsTinySq` | `tol: float -> bool` |
| `Line3D.isTinySq` | `tol: float -> l: Line3D -> bool` |
| `.IsVertical` | `bool` |
| `Line3D.isVertical` | `l: Line3D -> bool` |
| `.IsXAligned` | `bool` |
| `Line3D.isXAligned` | `l: Line3D -> bool` |
| `.IsYAligned` | `bool` |
| `Line3D.isYAligned` | `l: Line3D -> bool` |
| `.IsZAligned` | `bool` |
| `Line3D.isZAligned` | `l: Line3D -> bool` |
| `.IsZeroLength` | `bool` |
| `Line3D.isZeroLength` | `l: Line3D -> bool` |
| `.Length` | `float` |
| `Line3D.length` | `l: Line3D -> float` |
| `.LengthFromParam` | `t: float -> float` |
| `Line3D.lengthFromParam` | `t: float -> ln: Line3D -> float` |
| `.LengthSq` | `float` |
| `Line3D.lengthSq` | `l: Line3D -> float` |
| `.LengthTillParam` | `p: float -> float` |
| `Line3D.lengthTillParam` | `p: float -> ln: Line3D -> float` |
| `.MatchesOrientation` | `otherLn: Line3D -> bool` |
| `.MatchesOrientation` | `v: Vec -> bool` |
| `.MatchesOrientation` | `v: UnitVec -> bool` |
| `Line3D.matchesOrientation` | `l: Line3D -> ln: Line3D -> bool` |
| `.MatchesOrientation45` | `l: Line3D -> bool` |
| `Line3D.matchesOrientation45` | `l: Line3D -> ln: Line3D -> bool` |
| `.Mid` | `Pnt` |
| `Line3D.mid` | `ln: Line3D -> Pnt` |
| `.Move` | `v: Vec -> Line3D` |
| `Line3D.move` | `v: Vec -> ln: Line3D -> Line3D` |
| `.MoveX` | `distance: float -> Line3D` |
| `Line3D.moveX` | `distance: float -> ln: Line3D -> Line3D` |
| `.MoveY` | `distance: float -> Line3D` |
| `Line3D.moveY` | `distance: float -> ln: Line3D -> Line3D` |
| `.MoveZ` | `distance: float -> Line3D` |
| `Line3D.moveZ` | `distance: float -> ln: Line3D -> Line3D` |
| `.RayClosestParameter` | `p: Pnt -> float` |
| `Line3D.rayClosestParameter` | `p: Pnt -> ln: Line3D -> float` |
| `.RayClosestParameters` | `lnB: Line3D -> float * float` |
| `Line3D.rayClosestParameters` | `lnA: Line3D -> lnB: Line3D -> float * float` |
| `.RayClosestParameterXYZ` | `x: float * y: float * z: float -> float` |
| `Line3D.rayClosestParameterXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
| `.RayClosestPoint` | `p: Pnt -> Pnt` |
| `Line3D.rayClosestPoint` | `p: Pnt -> ln: Line3D -> Pnt` |
| `.RayClosestPoints` | `lnB: Line3D -> Pnt * Pnt` |
| `Line3D.rayClosestPoints` | `lnA: Line3D -> lnB: Line3D -> Pnt * Pnt` |
| `.Reversed` | `Line3D` |
| `Line3D.reversed` | `ln: Line3D -> Line3D` |
| `.Rotate` | `q: Quaternion -> Line3D` |
| `Line3D.rotate` | `q: Quaternion -> ln: Line3D -> Line3D` |
| `.RotateAround` | `cen: Pnt -> q: Quaternion -> Line3D` |
| `Line3D.rotateAround` | `cen: Pnt -> q: Quaternion -> ln: Line3D -> Line3D` |
| `.Scale` | `factor: float -> Line3D` |
| `Line3D.scale` | `factor: float -> ln: Line3D -> Line3D` |
| `.ScaleOn` | `cen: Pnt -> factor: float -> Line3D` |
| `Line3D.scaleOn` | `cen: Pnt -> factor: float -> ln: Line3D -> Line3D` |
| `.Segment` | `a: float * b: float -> Line3D` |
| `Line3D.segment` | `a: float -> b: float -> ln: Line3D -> Line3D` |
| `.Shrink` | `distAtStart: float * distAtEnd: float -> Line3D` |
| `Line3D.shrink` | `distAtStart: float -> distAtEnd: float -> ln: Line3D -> Line3D` |
| `.ShrinkEnd` | `distAtEnd: float -> Line3D` |
| `Line3D.shrinkEnd` | `distAtEnd: float -> ln: Line3D -> Line3D` |
| `.ShrinkStart` | `distAtStart: float -> Line3D` |
| `Line3D.shrinkStart` | `distAtStart: float -> ln: Line3D -> Line3D` |
| `.Start` | `Pnt` |
| `Line3D.start` | `l: Line3D -> Pnt` |
| `.SubLine` | `start: float * endParameter: float -> Line3D` |
| `Line3D.subLine` | `start: float -> endParameter: float -> ln: Line3D -> Line3D` |
| `.Tangent` | `Vec` |
| `Line3D.tangent` | `ln: Line3D -> Vec` |
| `.To` | `Pnt` |
| `.ToString` | `unit -> string` |
| `.Transform` | `m: Matrix -> Line3D` |
| `Line3D.transform` | `m: Matrix -> ln: Line3D -> Line3D` |
| `.TransformRigid` | `m: RigidMatrix -> Line3D` |
| `Line3D.transformRigid` | `m: RigidMatrix -> ln: Line3D -> Line3D` |
| `.TryRayClosestParameters` | `lnB: Line3D -> (float * float) voption` |
| `Line3D.tryRayClosestParameters` | `lnA: Line3D -> lnB: Line3D -> (float * float) voption` |
| `.TryRayClosestPoints` | `lnB: Line3D -> (Pnt * Pnt) voption` |
| `Line3D.tryRayClosestPoints` | `lnA: Line3D -> lnB: Line3D -> (Pnt * Pnt) voption` |
| `.UnitTangent` | `UnitVec` |
| `Line3D.unitTangent` | `ln: Line3D -> UnitVec` |
| `.Vector` | `Vec` |
| `Line3D.vector` | `ln: Line3D -> Vec` |
| `.VectorX` | `float` |
| `Line3D.vectorX` | `ln: Line3D -> float` |
| `.VectorY` | `float` |
| `Line3D.vectorY` | `ln: Line3D -> float` |
| `.VectorZ` | `float` |
| `Line3D.vectorZ` | `ln: Line3D -> float` |
| **Other static members** | |
| `Line3D.create` | `start: Pnt -> finish: Pnt -> Line3D` |
| `Line3D.createFromLine2D` | `ln: Line2D -> Line3D` |
| `Line3D.createFromLine2DWithZ` | `zLevel: float -> ln: Line2D -> Line3D` |
| `Line3D.createFromPntAndVec` | `p: Pnt -> v: Vec -> Line3D` |
| `Line3D.createFromVec` | `v: Vec -> Line3D` |
| `Line3D.distanceToLine` | `lnA: Line3D -> lnB: Line3D -> float` |
| `Line3D.distanceToLineSq` | `lnA: Line3D -> lnB: Line3D -> float` |
| `Line3D.divide` | `segments: int -> ln: Line3D -> Pnt array` |
| `Line3D.divideEvery` | `dist: float -> l: Line3D -> ResizeArray<Pnt>` |
| `Line3D.divideInsideEvery` | `dist: float -> l: Line3D -> ResizeArray<Pnt>` |
| `Line3D.divideMaxLength` | `maxSegmentLength: float -> ln: Line3D -> Pnt array` |
| `Line3D.divideMinLength` | `minSegmentLength: float -> ln: Line3D -> Pnt array` |
| `Line3D.doIntersect` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line3D.doIntersectOrOverlap` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line3D.doRaysIntersect` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line3D.end'` | `l: Line3D -> Pnt` |
| `Line3D.equalsExact` | `a: Line3D -> b: Line3D -> bool` |
| `Line3D.equalsWithin` | `tol: float -> a: Line3D -> b: Line3D -> bool` |
| `Line3D.fromX` | `l: Line3D -> float` |
| `Line3D.fromY` | `l: Line3D -> float` |
| `Line3D.fromZ` | `l: Line3D -> float` |
| `Line3D.intersectCone` | `coneRadius: float -> coneBaseZ: float -> coneTipZ: float -> ray: Line3D -> float * float` |
| `Line3D.isNormalTo'` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line3D.isNormalToUnitVec` | `maxTangent: float<tangent> -> other: UnitVec -> ln: Line3D -> bool` |
| `Line3D.isNormalToVec` | `maxTangent: float<tangent> -> other: Vec -> ln: Line3D -> bool` |
| `Line3D.isNotParallelTo` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line3D.isNotParallelTo'` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line3D.isParallelAndOrientedToUnitVec` | `minTangent: float<tangent> -> other: UnitVec -> ln: Line3D -> bool` |
| `Line3D.isParallelAndOrientedToVec` | `minTangent: float<tangent> -> other: Vec -> ln: Line3D -> bool` |
| `Line3D.isParallelTo'` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line3D.isParallelToUnitVec` | `minTangent: float<tangent> -> other: UnitVec -> ln: Line3D -> bool` |
| `Line3D.isParallelToVec` | `minTangent: float<tangent> -> other: Vec -> ln: Line3D -> bool` |
| `Line3D.isPerpendicularTo'` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line3D.isPerpendicularToUnitVec` | `maxTangent: float<tangent> -> other: UnitVec -> ln: Line3D -> bool` |
| `Line3D.isPerpendicularToVec` | `maxTangent: float<tangent> -> other: Vec -> ln: Line3D -> bool` |
| `Line3D.isTouchingEndOf` | `squareTolerance: float -> a: Line3D -> b: Line3D -> bool` |
| `Line3D.lengthToPntOnLine` | `pnt: Pnt -> ln: Line3D -> float` |
| `Line3D.matchesOrientationUnitVec` | `v: UnitVec -> ln: Line3D -> bool` |
| `Line3D.matchesOrientationVec` | `v: Vec -> ln: Line3D -> bool` |
| `Line3D.matchOrientation` | `orientationToMatch: Line3D -> lineToFlip: Line3D -> Line3D` |
| `Line3D.matchUnitVecOrientation` | `orientationToMatch: UnitVec -> lineToFlip: Line3D -> Line3D` |
| `Line3D.matchVecOrientation` | `orientationToMatch: Vec -> lineToFlip: Line3D -> Line3D` |
| `Line3D.notEqualsWithin` | `tol: float -> a: Line3D -> b: Line3D -> bool` |
| `Line3D.offset` | `distHorizontal: float -> distNormal: float -> l: Line3D -> Line3D` |
| `Line3D.offsetXY` | `amount: float -> ln: Line3D -> Line3D` |
| `Line3D.pointAtDistance` | `dist: float -> ln: Line3D -> Pnt` |
| `Line3D.pointAtDistanceFromEnd` | `dist: float -> ln: Line3D -> Pnt` |
| `Line3D.projectOntoRay` | `rayToProjectOnto: Line3D -> lineToProject: Line3D -> Line3D` |
| `Line3D.projectOntoRayParam` | `rayToProjectOnto: Line3D -> lineToProject: Line3D -> float * float` |
| `Line3D.reverse` | `ln: Line3D -> Line3D` |
| `Line3D.rotateAroundOnZ` | `cen: Pnt -> r: Rotation2D -> ln: Line3D -> Line3D` |
| `Line3D.rotateOnZ` | `r: Rotation2D -> ln: Line3D -> Line3D` |
| `Line3D.split` | `gap: float -> segments: int -> ln: Line3D -> Line3D array` |
| `Line3D.splitMaxLength` | `gap: float -> maxSegmentLength: float -> ln: Line3D -> Line3D array` |
| `Line3D.splitMinLength` | `gap: float -> minSegmentLength: float -> ln: Line3D -> Line3D array` |
| `Line3D.to'` | `l: Line3D -> Pnt` |
| `Line3D.toLine2D` | `ln: Line3D -> Line2D` |
| `Line3D.toX` | `l: Line3D -> float` |
| `Line3D.toY` | `l: Line3D -> float` |
| `Line3D.toZ` | `l: Line3D -> float` |
| `Line3D.tryGetOverlap` | `lnA: Line3D -> lnB: Line3D -> (float * float) voption` |
| `Line3D.tryIntersect` | `lnA: Line3D -> lnB: Line3D -> Pnt voption` |
| `Line3D.tryIntersectCone` | `coneRadius: float -> coneBaseZ: float -> coneTipZ: float -> ray: Line3D -> (float * float) voption` |
| `Line3D.tryIntersectOrOverlap` | `lnA: Line3D -> lnB: Line3D -> Pnt voption` |
| `Line3D.tryIntersectRay` | `lineA: Line3D -> lineB: Line3D -> Pnt voption` |
| `Line3D.tryProjectOntoLine` | `onToLine: Line3D -> lineToProject: Line3D -> Line3D voption` |
| `Line3D.tryProjectOntoLineParam` | `onToLine: Line3D -> lineToProject: Line3D -> (float * float) voption` |
| `Line3D.withEnd` | `pnt: Pnt -> ln: Line3D -> Line3D` |
| `Line3D.withFrom` | `pnt: Pnt -> ln: Line3D -> Line3D` |
| `Line3D.withLengthFromMid` | `len: float -> ln: Line3D -> Line3D` |
| `Line3D.withLengthFromStart` | `len: float -> ln: Line3D -> Line3D` |
| `Line3D.withLengthToEnd` | `len: float -> ln: Line3D -> Line3D` |
| `Line3D.withStart` | `pnt: Pnt -> ln: Line3D -> Line3D` |
| `Line3D.withTo` | `pnt: Pnt -> ln: Line3D -> Line3D` |

## XLine3D.XCone

| XCone | Signature |
|:--|:--|
| `NoIntersection` | `XCone` |
| `Intersecting` | `float * float -> XCone` |
| `IntersectingOne` | `float -> XCone` |
| `Touching` | `float -> XCone` |
| `ThroughTip` | `float -> XCone` |
| `LineOnCone` | `tipParam: float * onConeTowardsPositiveT: bool -> XCone` |

## XLine3D.XRayParam

| XRayParam | Signature |
|:--|:--|
| `SkewOrX` | `twoParams: float * float -> XRayParam` |
| `Parallel` | `XRayParam` |
| `TooShortA` | `XRayParam` |
| `TooShortB` | `XRayParam` |
| `TooShortBoth` | `XRayParam` |

## XLine3D.XRay

| XRay | Signature |
|:--|:--|
| `Intersect` | `xPnt: Pnt -> XRay` |
| `Skew` | `closestPntA: Pnt * closestPntB: Pnt * squareDist: float -> XRay` |
| `Parallel` | `XRay` |
| `TooShortA` | `XRay` |
| `TooShortB` | `XRay` |
| `TooShortBoth` | `XRay` |

## XLine3D.XParam

| XParam | Signature |
|:--|:--|
| `Intersect` | `twoParams: float * float -> XParam` |
| `Skew` | `paramA: float * paramB: float * squareDist: float -> XParam` |
| `Apart` | `XParam` |
| `Parallel` | `XParam` |
| `TooShortA` | `XParam` |
| `TooShortB` | `XParam` |
| `TooShortBoth` | `XParam` |

## XLine3D.XPnt

| XPnt | Signature |
|:--|:--|
| `Intersect` | `xPnt: Pnt -> XPnt` |
| `Skew` | `closestPntA: Pnt * closestPntB: Pnt * squareDist: float -> XPnt` |
| `Apart` | `XPnt` |
| `Parallel` | `XPnt` |
| `TooShortA` | `XPnt` |
| `TooShortB` | `XPnt` |
| `TooShortBoth` | `XPnt` |

## XLine3D.ClParams

| ClParams | Signature |
|:--|:--|
| `Intersect` | `paramA: float * paramB: float -> ClParams` |
| `Skew` | `paramA: float * paramB: float * squareDist: float -> ClParams` |
| `Parallel` | `paramA: float * paramB: float -> ClParams` |
| `Apart` | `paramA: float * paramB: float * squareDist: float -> ClParams` |
| `TooShortA` | `ClParams` |
| `TooShortB` | `ClParams` |
| `TooShortBoth` | `ClParams` |

## XLine3D.ClPnts

| ClPnts | Signature |
|:--|:--|
| `Intersect` | `pnt: Pnt -> ClPnts` |
| `Skew` | `pntA: Pnt * pntB: Pnt * squareDist: float -> ClPnts` |
| `Parallel` | `pntA: Pnt * pntB: Pnt -> ClPnts` |
| `Apart` | `pntA: Pnt * pntB: Pnt * squareDist: float -> ClPnts` |
| `TooShortA` | `ClPnts` |
| `TooShortB` | `ClPnts` |
| `TooShortBoth` | `ClPnts` |

## XLine3D.XEnds

| XEnds | Signature |
|:--|:--|
| `NotTouching` | `XEnds` |
| `StartA_StartB` | `XEnds` |
| `EndA_EndB` | `XEnds` |
| `EndA_StartB` | `XEnds` |
| `StartA_EndB` | `XEnds` |
| `Identical` | `XEnds` |
| `IdenticalFlipped` | `XEnds` |

## module XLineXYZ

| XLineXYZ | Signature |
|:--|:--|
| `XLineXYZ.classifyClosestParameters` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * tangent: float<tangent> * tooShortTolerance: float -> ClParams` |
| `XLineXYZ.classifyClosestPoints` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * tangent: float<tangent> * tooShortTolerance: float -> ClPnts` |
| `XLineXYZ.classifyIntersection` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> XPnt` |
| `XLineXYZ.classifyIntersectionParam` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> XParam` |
| `XLineXYZ.classifyRayClosestParam` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * tangent: float<tangent> * tooShortTolerance: float -> XRayParam` |
| `XLineXYZ.classifyRayIntersection` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> XRay` |
| `XLineXYZ.distanceSq` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> float` |
| `XLineXYZ.doIntersect` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> bool` |
| `XLineXYZ.doOverlap` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * tolerance: float -> bool` |
| `XLineXYZ.doRaysIntersect` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float -> bool` |
| `XLineXYZ.parameterA` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> float` |
| `XLineXYZ.parameters` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> float * float` |
| `XLineXYZ.tryClosestParameterRayA` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> float voption` |
| `XLineXYZ.tryClosestPntRayA` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> Pnt voption` |
| `XLineXYZ.tryIntersect` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> Pnt voption` |
| `XLineXYZ.tryIntersectRay` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> Pnt voption` |

## XLine3D

| XLine3D | Signature |
|:--|:--|
| `XLine3D.classifyClosestParameters` | `lineA: Line3D -> lineB: Line3D -> ClParams` |
| `XLine3D.classifyClosestParametersCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` |
| `XLine3D.classifyClosestParametersCustom` | `lineA: Line3D * lineB: Line3D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` |
| `XLine3D.classifyClosestParametersFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> ClParams` |
| `XLine3D.classifyClosestPoints` | `lineA: Line3D -> lineB: Line3D -> ClPnts` |
| `XLine3D.classifyClosestPointsCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPnts` |
| `XLine3D.classifyClosestPointsCustom` | `lineA: Line3D * lineB: Line3D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPnts` |
| `XLine3D.classifyClosestPointsFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> ClPnts` |
| `XLine3D.classifyConeIntersection` | `coneRadius: float -> coneBaseZ: float -> coneTipZ: float -> ray: Line3D -> XCone` |
| `XLine3D.classifyEndsTouching` | `a: Line3D -> b: Line3D -> XEnds` |
| `XLine3D.classifyEndsTouchingCustom` | `a: Line3D * b: Line3D * ?tolerance: float -> XEnds` |
| `XLine3D.classifyIntersection` | `lineA: Line3D -> lineB: Line3D -> XPnt` |
| `XLine3D.classifyIntersectionCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPnt` |
| `XLine3D.classifyIntersectionCustom` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPnt` |
| `XLine3D.classifyIntersectionFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> XPnt` |
| `XLine3D.classifyIntersectionParam` | `lnA: Line3D -> lnB: Line3D -> XParam` |
| `XLine3D.classifyIntersectionParamCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` |
| `XLine3D.classifyIntersectionParamCustom` | `lnA: Line3D * lnB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` |
| `XLine3D.classifyIntersectionParamFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> XParam` |
| `XLine3D.classifyRayClosestParam` | `lineA: Line3D -> lineB: Line3D -> XRayParam` |
| `XLine3D.classifyRayClosestParamCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` |
| `XLine3D.classifyRayClosestParamCustom` | `lineA: Line3D * lineB: Line3D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` |
| `XLine3D.classifyRayClosestParamFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> XRayParam` |
| `XLine3D.classifyRayIntersection` | `lineA: Line3D -> lineB: Line3D -> XRay` |
| `XLine3D.classifyRayIntersectionCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` |
| `XLine3D.classifyRayIntersectionCustom` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` |
| `XLine3D.classifyRayIntersectionFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> XRay` |
| `XLine3D.distanceSq` | `lineA: Line3D -> lineB: Line3D -> float` |
| `XLine3D.distanceSqFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> float` |
| `XLine3D.doIntersect` | `lineA: Line3D -> lineB: Line3D -> bool` |
| `XLine3D.doIntersectCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> bool` |
| `XLine3D.doIntersectCustom` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> bool` |
| `XLine3D.doIntersectFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> bool` |
| `XLine3D.doOverlap` | `lineA: Line3D -> lineB: Line3D -> bool` |
| `XLine3D.doOverlapCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tolerance: float -> bool` |
| `XLine3D.doOverlapCustom` | `lineA: Line3D * lineB: Line3D * ?tolerance: float -> bool` |
| `XLine3D.doOverlapFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> bool` |
| `XLine3D.doRaysIntersect` | `lineA: Line3D -> lineB: Line3D -> bool` |
| `XLine3D.doRaysIntersectCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float -> bool` |
| `XLine3D.doRaysIntersectCustom` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float -> bool` |
| `XLine3D.doRaysIntersectFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> bool` |
| `XLine3D.parameterA` | `lineA: Line3D -> lineB: Line3D -> float` |
| `XLine3D.parameterAFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> float` |
| `XLine3D.parameters` | `lineA: Line3D -> lineB: Line3D -> float * float` |
| `XLine3D.parametersFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> float * float` |
| `XLine3D.tryClosestParameterRayA` | `lineA: Line3D -> lineB: Line3D -> float voption` |
| `XLine3D.tryClosestParameterRayAFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> float voption` |
| `XLine3D.tryClosestPntRayA` | `lineA: Line3D -> lineB: Line3D -> Pnt voption` |
| `XLine3D.tryClosestPntRayAFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> Pnt voption` |
| `XLine3D.tryIntersect` | `lineA: Line3D -> lineB: Line3D -> Pnt voption` |
| `XLine3D.tryIntersectCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` |
| `XLine3D.tryIntersectCustom` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` |
| `XLine3D.tryIntersectFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> Pnt voption` |
| `XLine3D.tryIntersectRay` | `lineA: Line3D -> lineB: Line3D -> Pnt voption` |
| `XLine3D.tryIntersectRayCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` |
| `XLine3D.tryIntersectRayCustom` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` |
| `XLine3D.tryIntersectRayFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> Pnt voption` |

## NPlane

| NPlane | Signature |
|:--|:--|
| `new` | `pnt: Pnt * n: UnitVec -> NPlane` |
| `.OriginX` | `float` |
| `.OriginY` | `float` |
| `.OriginZ` | `float` |
| `.NormalX` | `float` |
| `.NormalY` | `float` |
| `.NormalZ` | `float` |
| `.Angle90ToLine` | `ln: Line3D -> float` |
| `NPlane.angle90ToLine` | `ln: Line3D -> pl: NPlane -> float` |
| `.Angle90ToPlane` | `pl: NPlane -> float` |
| `NPlane.angle90ToPlane` | `pl: NPlane -> other: NPlane -> float` |
| `.Angle90ToVec` | `v: Vec -> float` |
| `.Angle90ToVec` | `v: UnitVec -> float` |
| `NPlane.angle90ToVec` | `v: Vec -> pl: NPlane -> float` |
| `.AsFSharpCode` | `string` |
| `NPlane.asFSharpCode` | `pl: NPlane -> string` |
| `.AsString` | `string` |
| `NPlane.asString` | `pl: NPlane -> string` |
| `.ClosestPoint` | `pnt: Pnt -> Pnt` |
| `NPlane.closestPoint` | `pnt: Pnt -> pl: NPlane -> Pnt` |
| `.DistanceToPnt` | `pnt: Pnt -> float` |
| `NPlane.distanceToPnt` | `pnt: Pnt -> pl: NPlane -> float` |
| `.DistanceToPntSigned` | `pnt: Pnt -> float` |
| `NPlane.distanceToPntSigned` | `pnt: Pnt -> pl: NPlane -> float` |
| `.DistanceToXYZ` | `x: float * y: float * z: float -> float` |
| `NPlane.distanceToXYZ` | `x: float -> y: float -> z: float -> pl: NPlane -> float` |
| `.DistanceToXYZSigned` | `x: float * y: float * z: float -> float` |
| `NPlane.distanceToXYZSigned` | `x: float -> y: float -> z: float -> pl: NPlane -> float` |
| `.Flipped` | `NPlane` |
| `NPlane.flipped` | `pl: NPlane -> NPlane` |
| `.IsCoincidentTo` | `other: NPlane * ?distanceTolerance: float * ?minCosine: float<cosine> -> bool` |
| `.Normal` | `UnitVec` |
| `NPlane.normal` | `p: NPlane -> UnitVec` |
| `.Origin` | `Pnt` |
| `NPlane.origin` | `a: NPlane -> Pnt` |
| `.PlaneAtClPnt` | `pnt: Pnt -> NPlane` |
| `NPlane.planeAtClPnt` | `pnt: Pnt -> pl: NPlane -> NPlane` |
| `.ToString` | `unit -> string` |
| **Other static members** | |
| `NPlane.angle90ToUnitVec` | `v: UnitVec -> pl: NPlane -> float` |
| `NPlane.angleTo` | `a: NPlane -> b: NPlane -> float` |
| `NPlane.areCoincident` | `a: NPlane -> b: NPlane -> bool` |
| `NPlane.createFrom3Pnts` | `a: Pnt -> b: Pnt -> c: Pnt -> NPlane` |
| `NPlane.createFromNormal` | `normal: Vec -> pnt: Pnt -> NPlane` |
| `NPlane.createFromPPlane` | `p: PPlane -> NPlane` |
| `NPlane.createFromUnitNormal` | `normal: UnitVec -> pnt: Pnt -> NPlane` |
| `NPlane.createFromUnitNormalUnchecked` | `origin: Pnt * normal: UnitVec -> NPlane` |
| `NPlane.createUnchecked` | `originX: float * originY: float * originZ: float * normalX: float * normalY: float * normalZ: float -> NPlane` |
| `NPlane.doLinePlaneIntersect` | `ln: Line3D -> pl: NPlane -> bool` |
| `NPlane.equalsExact` | `a: NPlane -> b: NPlane -> bool` |
| `NPlane.equalsWithin` | `tol: float -> a: NPlane -> b: NPlane -> bool` |
| `NPlane.intersect` | `a: NPlane -> b: NPlane -> Line3D` |
| `NPlane.intersectLine` | `ln: Line3D -> pl: NPlane -> Pnt` |
| `NPlane.intersectRay` | `ln: Line3D -> pl: NPlane -> float` |
| `NPlane.move` | `translation: Vec -> pl: NPlane -> NPlane` |
| `NPlane.offset` | `dist: float -> pl: NPlane -> NPlane` |
| `NPlane.offsetInDir` | `dirPnt: Pnt -> dist: float -> pl: NPlane -> NPlane` |
| `NPlane.scale` | `factor: float -> pl: NPlane -> NPlane` |
| `NPlane.tryIntersect` | `a: NPlane -> b: NPlane -> Line3D voption` |
| `NPlane.tryIntersectLine` | `ln: Line3D -> pl: NPlane -> Pnt voption` |
| `NPlane.tryIntersectRay` | `ln: Line3D -> pl: NPlane -> float voption` |
| `NPlane.xyPlane` | `NPlane` |

## Rect3D

| Rect3D | Signature |
|:--|:--|
| `.OriginX` | `float` |
| `.OriginY` | `float` |
| `.OriginZ` | `float` |
| `.XAxisX` | `float` |
| `.XAxisY` | `float` |
| `.XAxisZ` | `float` |
| `.YAxisX` | `float` |
| `.YAxisY` | `float` |
| `.YAxisZ` | `float` |
| `.Area` | `float` |
| `Rect3D.area` | `r: Rect3D -> float` |
| `.AsFSharpCode` | `string` |
| `Rect3D.asFSharpCode` | `r: Rect3D -> string` |
| `.AsString` | `string` |
| `Rect3D.asString` | `r: Rect3D -> string` |
| `.Center` | `Pnt` |
| `Rect3D.center` | `r: Rect3D -> Pnt` |
| `.ClosestParameters` | `p: Pnt -> float * float` |
| `Rect3D.closestParameters` | `p: Pnt -> r: Rect3D -> float * float` |
| `.ClosestPoint` | `p: Pnt -> Pnt` |
| `Rect3D.closestPoint` | `p: Pnt -> r: Rect3D -> Pnt` |
| `.CountZeroSides` | `int` |
| `Rect3D.countZeroSides` | `r: Rect3D -> int` |
| `.Diagonal` | `Vec` |
| `Rect3D.diagonal` | `r: Rect3D -> Vec` |
| `.DiagonalLine` | `Line3D` |
| `Rect3D.diagonalLine` | `r: Rect3D -> Line3D` |
| `.Edge01` | `Line3D` |
| `Rect3D.edge01` | `r: Rect3D -> Line3D` |
| `.Edge03` | `Line3D` |
| `Rect3D.edge03` | `r: Rect3D -> Line3D` |
| `.Edge10` | `Line3D` |
| `Rect3D.edge10` | `r: Rect3D -> Line3D` |
| `.Edge12` | `Line3D` |
| `Rect3D.edge12` | `r: Rect3D -> Line3D` |
| `.Edge21` | `Line3D` |
| `Rect3D.edge21` | `r: Rect3D -> Line3D` |
| `.Edge23` | `Line3D` |
| `Rect3D.edge23` | `r: Rect3D -> Line3D` |
| `.Edge30` | `Line3D` |
| `Rect3D.edge30` | `r: Rect3D -> Line3D` |
| `.Edge32` | `Line3D` |
| `Rect3D.edge32` | `r: Rect3D -> Line3D` |
| `.Edges` | `Line3D array` |
| `Rect3D.edges` | `r: Rect3D -> Line3D array` |
| `.EdgeX` | `Line3D` |
| `Rect3D.edgeX` | `r: Rect3D -> Line3D` |
| `.EdgeY` | `Line3D` |
| `Rect3D.edgeY` | `r: Rect3D -> Line3D` |
| `.EvaluateAt` | `xParameter: float * yParameter: float -> Pnt` |
| `Rect3D.evaluateAt` | `xParameter: float -> yParameter: float -> r: Rect3D -> Pnt` |
| `.EvaluateDistance` | `xDistance: float * yDistance: float -> Pnt` |
| `Rect3D.evaluateDistance` | `xDistance: float -> yDistance: float -> r: Rect3D -> Pnt` |
| `.FarCorner` | `Pnt` |
| `Rect3D.farCorner` | `r: Rect3D -> Pnt` |
| `.Flipped` | `Rect3D` |
| `Rect3D.flipped` | `r: Rect3D -> Rect3D` |
| `.GetEdge` | `i: int -> Line3D` |
| `Rect3D.getEdge` | `i: int -> r: Rect3D -> Line3D` |
| `.HasArea` | `bool` |
| `Rect3D.hasArea` | `r: Rect3D -> bool` |
| `.IsLine` | `bool` |
| `Rect3D.isLine` | `r: Rect3D -> bool` |
| `.IsPoint` | `bool` |
| `Rect3D.isPoint` | `r: Rect3D -> bool` |
| `.IsValid` | `bool` |
| `Rect3D.isValid` | `r: Rect3D -> bool` |
| `.IsZero` | `bool` |
| `Rect3D.isZero` | `r: Rect3D -> bool` |
| `.LongestEdge` | `float` |
| `Rect3D.longestEdge` | `r: Rect3D -> float` |
| `.LongestEdgeSq` | `float` |
| `Rect3D.longestEdgeSq` | `r: Rect3D -> float` |
| `.Move` | `v: Vec -> Rect3D` |
| `Rect3D.move` | `v: Vec -> r: Rect3D -> Rect3D` |
| `.MoveX` | `distance: float -> Rect3D` |
| `Rect3D.moveX` | `distance: float -> r: Rect3D -> Rect3D` |
| `.MoveY` | `distance: float -> Rect3D` |
| `Rect3D.moveY` | `distance: float -> r: Rect3D -> Rect3D` |
| `.MoveZ` | `distance: float -> Rect3D` |
| `Rect3D.moveZ` | `distance: float -> r: Rect3D -> Rect3D` |
| `.Normal` | `Vec` |
| `Rect3D.normal` | `r: Rect3D -> Vec` |
| `.NormalUnit` | `UnitVec` |
| `Rect3D.normalUnit` | `r: Rect3D -> UnitVec` |
| `.Origin` | `Pnt` |
| `Rect3D.origin` | `r: Rect3D -> Pnt` |
| `.Plane` | `PPlane` |
| `Rect3D.plane` | `r: Rect3D -> PPlane` |
| `.Pnt0` | `Pnt` |
| `Rect3D.pnt0` | `r: Rect3D -> Pnt` |
| `.Pnt1` | `Pnt` |
| `Rect3D.pnt1` | `r: Rect3D -> Pnt` |
| `.Pnt2` | `Pnt` |
| `Rect3D.pnt2` | `r: Rect3D -> Pnt` |
| `.Pnt3` | `Pnt` |
| `Rect3D.pnt3` | `r: Rect3D -> Pnt` |
| `.Points` | `Pnt array` |
| `Rect3D.points` | `r: Rect3D -> Pnt array` |
| `.PointsLooped` | `Pnt array` |
| `Rect3D.pointsLooped` | `r: Rect3D -> Pnt array` |
| `.PointsXYZ` | `ResizeArray<float>` |
| `Rect3D.pointsXYZ` | `r: Rect3D -> ResizeArray<float>` |
| `.PointsXYZLooped` | `ResizeArray<float>` |
| `Rect3D.pointsXYZLooped` | `r: Rect3D -> ResizeArray<float>` |
| `.Rotate` | `q: Quaternion -> Rect3D` |
| `Rect3D.rotate` | `q: Quaternion -> r: Rect3D -> Rect3D` |
| `.RotateAround` | `cen: Pnt -> q: Quaternion -> Rect3D` |
| `Rect3D.rotateAround` | `cen: Pnt -> q: Quaternion -> r: Rect3D -> Rect3D` |
| `.RotateOrientation180` | `Rect3D` |
| `Rect3D.rotateOrientation180` | `r: Rect3D -> Rect3D` |
| `.RotateOrientation90CCW` | `Rect3D` |
| `Rect3D.rotateOrientation90CCW` | `r: Rect3D -> Rect3D` |
| `.RotateOrientation90CW` | `Rect3D` |
| `Rect3D.rotateOrientation90CW` | `r: Rect3D -> Rect3D` |
| `.Scale` | `factor: float -> Rect3D` |
| `Rect3D.scale` | `factor: float -> r: Rect3D -> Rect3D` |
| `.ScaleOn` | `cen: Pnt -> factor: float -> Rect3D` |
| `Rect3D.scaleOn` | `cen: Pnt -> factor: float -> r: Rect3D -> Rect3D` |
| `.ShortestEdge` | `float` |
| `Rect3D.shortestEdge` | `r: Rect3D -> float` |
| `.ShortestEdgeSq` | `float` |
| `Rect3D.shortestEdgeSq` | `r: Rect3D -> float` |
| `.SizeX` | `float` |
| `Rect3D.sizeX` | `r: Rect3D -> float` |
| `.SizeXSq` | `float` |
| `Rect3D.sizeXSq` | `r: Rect3D -> float` |
| `.SizeY` | `float` |
| `Rect3D.sizeY` | `r: Rect3D -> float` |
| `.SizeYSq` | `float` |
| `Rect3D.sizeYSq` | `r: Rect3D -> float` |
| `.ToString` | `unit -> string` |
| `.TransformRigid` | `m: RigidMatrix -> Rect3D` |
| `Rect3D.transformRigid` | `m: RigidMatrix -> r: Rect3D -> Rect3D` |
| `.XAxis` | `Vec` |
| `Rect3D.xAxis` | `r: Rect3D -> Vec` |
| `.XAxisUnit` | `UnitVec` |
| `Rect3D.xAxisUnit` | `r: Rect3D -> UnitVec` |
| `.XCorner` | `Pnt` |
| `Rect3D.xCorner` | `r: Rect3D -> Pnt` |
| `.YAxis` | `Vec` |
| `Rect3D.yAxis` | `r: Rect3D -> Vec` |
| `.YAxisUnit` | `UnitVec` |
| `Rect3D.yAxisUnit` | `r: Rect3D -> UnitVec` |
| `.YCorner` | `Pnt` |
| `Rect3D.yCorner` | `r: Rect3D -> Pnt` |
| **Other static members** | |
| `Rect3D.createCenteredFromPlane` | `sizeX: float -> sizeY: float -> pl: PPlane -> Rect3D` |
| `Rect3D.createFrom3Pnts` | `origin: Pnt -> xPnt: Pnt -> yPnt: Pnt -> Rect3D` |
| `Rect3D.createFromBounds2D` | `minX: float * minY: float * maxX: float * maxY: float -> Rect3D` |
| `Rect3D.createFromPlane` | `sizeX: float -> sizeY: float -> pl: PPlane -> Rect3D` |
| `Rect3D.createFromVecs` | `x: Vec -> y: Vec -> origin: Pnt -> Rect3D` |
| `Rect3D.createFromVecsUnchecked` | `origin: Pnt * x: Vec * y: Vec -> Rect3D` |
| `Rect3D.createUnchecked` | `originX: float * originY: float * originZ: float * xAxisX: float * xAxisY: float * xAxisZ: float * yAxisX: float * yAxisY: float * yAxisZ: float -> Rect3D` |
| `Rect3D.equalsExact` | `a: Rect3D -> b: Rect3D -> bool` |
| `Rect3D.equalsWithin` | `tol: float -> a: Rect3D -> b: Rect3D -> bool` |
| `Rect3D.expand` | `dist: float -> r: Rect3D -> Rect3D` |
| `Rect3D.expandRel` | `factor: float -> r: Rect3D -> Rect3D` |
| `Rect3D.expandRelXY` | `factorX: float -> factorY: float -> r: Rect3D -> Rect3D` |
| `Rect3D.expandXY` | `distX: float -> distY: float -> r: Rect3D -> Rect3D` |
| `Rect3D.fitToPoints` | `pts: IList<Pnt> -> refRect: Rect3D -> Rect3D` |
| `Rect3D.fitToPointsXYZ` | `xyzs: IList<float> -> refRect: Rect3D -> Rect3D` |
| `Rect3D.flip` | `r: Rect3D -> Rect3D` |
| `Rect3D.grid` | `xCount: int -> yCount: int -> rect: Rect3D -> Pnt array array` |
| `Rect3D.gridMaxLength` | `xMaxLen: float -> yMaxLen: float -> rect: Rect3D -> Pnt array array` |
| `Rect3D.gridMinLength` | `xMinLen: float -> yMinLen: float -> rect: Rect3D -> Pnt array array` |
| `Rect3D.intersectLine` | `ln: Line3D -> r: Rect3D -> Pnt` |
| `Rect3D.intersectRay` | `ln: Line3D -> r: Rect3D -> float` |
| `Rect3D.iterPointsCCW` | `action: (float -> float -> float -> unit) -> r: Rect3D -> unit` |
| `Rect3D.iterPointsCW` | `action: (float -> float -> float -> unit) -> r: Rect3D -> unit` |
| `Rect3D.iterPointsLoopedCCW` | `action: (float -> float -> float -> unit) -> r: Rect3D -> unit` |
| `Rect3D.iterPointsLoopedCW` | `action: (float -> float -> float -> unit) -> r: Rect3D -> unit` |
| `Rect3D.moveAlongLocalX` | `distX: float -> r: Rect3D -> Rect3D` |
| `Rect3D.moveAlongLocalY` | `distY: float -> r: Rect3D -> Rect3D` |
| `Rect3D.moveAlongLocalZ` | `distZ: float -> r: Rect3D -> Rect3D` |
| `Rect3D.notEqualsWithin` | `tol: float -> a: Rect3D -> b: Rect3D -> bool` |
| `Rect3D.offset` | `dist: float -> rect: Rect3D -> Rect3D` |
| `Rect3D.offsetCorner` | `rect: Rect3D * corner: int * xOffset: float * yOffset: float * xSize: float * ySize: float -> Rect3D` |
| `Rect3D.offsetEdge` | `rect: Rect3D * edgeIdx: int * offEdge: float * width: float * offStart: float * offEnd: float -> Rect3D` |
| `Rect3D.offsetVariable` | `dist: float array -> rect: Rect3D -> Rect3D` |
| `Rect3D.offsetZ` | `offsetDistance: float -> r: Rect3D -> Rect3D` |
| `Rect3D.subDivide` | `xCount: int -> yCount: int -> xGap: float -> yGap: float -> rect: Rect3D -> Rect3D array array` |
| `Rect3D.subDivideMaxLength` | `rect: Rect3D * xMaxLen: float * yMaxLen: float * xGap: float * yGap: float -> Rect3D array array` |
| `Rect3D.subDivideMinLength` | `rect: Rect3D * xMinLen: float * yMinLen: float * xGap: float * yGap: float -> Rect3D array array` |
| `Rect3D.tryCreateFrom3Pnts` | `origin: Pnt -> xPnt: Pnt -> yPnt: Pnt -> Rect3D voption` |
| `Rect3D.tryIntersectLine` | `ln: Line3D -> r: Rect3D -> Pnt voption` |
| `Rect3D.tryIntersectRay` | `ln: Line3D -> r: Rect3D -> float voption` |

## Box

| Box | Signature |
|:--|:--|
| `.OriginX` | `float` |
| `.OriginY` | `float` |
| `.OriginZ` | `float` |
| `.XAxisX` | `float` |
| `.XAxisY` | `float` |
| `.XAxisZ` | `float` |
| `.YAxisX` | `float` |
| `.YAxisY` | `float` |
| `.YAxisZ` | `float` |
| `.ZAxisX` | `float` |
| `.ZAxisY` | `float` |
| `.ZAxisZ` | `float` |
| `.AreaOfBiggestFace` | `float` |
| `Box.areaOfBiggestFace` | `b: Box -> float` |
| `.AreaOfSmallestFace` | `float` |
| `Box.areaOfSmallestFace` | `b: Box -> float` |
| `.AsFSharpCode` | `string` |
| `Box.asFSharpCode` | `b: Box -> string` |
| `.AsString` | `string` |
| `Box.asString` | `b: Box -> string` |
| `.BackFace` | `Rect3D` |
| `Box.backFace` | `b: Box -> Rect3D` |
| `.BottomFace` | `Rect3D` |
| `Box.bottomFace` | `b: Box -> Rect3D` |
| `.Center` | `Pnt` |
| `Box.center` | `b: Box -> Pnt` |
| `.Contains` | `p: Pnt -> bool` |
| `Box.contains` | `p: Pnt -> b: Box -> bool` |
| `.ContainsXYZ` | `x: float * y: float * z: float -> bool` |
| `Box.containsXYZ` | `x: float -> y: float -> z: float -> b: Box -> bool` |
| `.CountZeroSides` | `int` |
| `Box.countZeroSides` | `b: Box -> int` |
| `.Diagonal` | `Vec` |
| `Box.diagonal` | `b: Box -> Vec` |
| `.DoesIntersectPlane` | `pl: NPlane -> bool` |
| `Box.doesIntersectPlane` | `pl: NPlane -> b: Box -> bool` |
| `.Edge01` | `Line3D` |
| `Box.edge01` | `b: Box -> Line3D` |
| `.Edge03` | `Line3D` |
| `Box.edge03` | `b: Box -> Line3D` |
| `.Edge04` | `Line3D` |
| `Box.edge04` | `b: Box -> Line3D` |
| `.Edge10` | `Line3D` |
| `Box.edge10` | `b: Box -> Line3D` |
| `.Edge12` | `Line3D` |
| `Box.edge12` | `b: Box -> Line3D` |
| `.Edge15` | `Line3D` |
| `Box.edge15` | `b: Box -> Line3D` |
| `.Edge21` | `Line3D` |
| `Box.edge21` | `b: Box -> Line3D` |
| `.Edge23` | `Line3D` |
| `Box.edge23` | `b: Box -> Line3D` |
| `.Edge26` | `Line3D` |
| `Box.edge26` | `b: Box -> Line3D` |
| `.Edge30` | `Line3D` |
| `Box.edge30` | `b: Box -> Line3D` |
| `.Edge32` | `Line3D` |
| `Box.edge32` | `b: Box -> Line3D` |
| `.Edge37` | `Line3D` |
| `Box.edge37` | `b: Box -> Line3D` |
| `.Edge40` | `Line3D` |
| `Box.edge40` | `b: Box -> Line3D` |
| `.Edge45` | `Line3D` |
| `Box.edge45` | `b: Box -> Line3D` |
| `.Edge47` | `Line3D` |
| `Box.edge47` | `b: Box -> Line3D` |
| `.Edge51` | `Line3D` |
| `Box.edge51` | `b: Box -> Line3D` |
| `.Edge54` | `Line3D` |
| `Box.edge54` | `b: Box -> Line3D` |
| `.Edge56` | `Line3D` |
| `Box.edge56` | `b: Box -> Line3D` |
| `.Edge62` | `Line3D` |
| `Box.edge62` | `b: Box -> Line3D` |
| `.Edge65` | `Line3D` |
| `Box.edge65` | `b: Box -> Line3D` |
| `.Edge67` | `Line3D` |
| `Box.edge67` | `b: Box -> Line3D` |
| `.Edge73` | `Line3D` |
| `Box.edge73` | `b: Box -> Line3D` |
| `.Edge74` | `Line3D` |
| `Box.edge74` | `b: Box -> Line3D` |
| `.Edge76` | `Line3D` |
| `Box.edge76` | `b: Box -> Line3D` |
| `.Edges` | `Line3D array` |
| `Box.edges` | `b: Box -> Line3D array` |
| `.EvaluateAt` | `xParameter: float * yParameter: float * zParameter: float -> Pnt` |
| `Box.evaluateAt` | `xParameter: float -> yParameter: float -> zParameter: float -> b: Box -> Pnt` |
| `.Faces` | `Rect3D array` |
| `Box.faces` | `b: Box -> Rect3D array` |
| `.FarCorner` | `Pnt` |
| `Box.farCorner` | `b: Box -> Pnt` |
| `.FrontFace` | `Rect3D` |
| `Box.frontFace` | `b: Box -> Rect3D` |
| `.GetEdge` | `edgeIndex: int -> Line3D` |
| `Box.getEdge` | `edgeIndex: int -> b: Box -> Line3D` |
| `.GetFace` | `faceIndex: int -> Rect3D` |
| `Box.getFace` | `faceIndex: int -> b: Box -> Rect3D` |
| `.GetLine` | `startPointIndex: int * endPointIndex: int -> Line3D` |
| `Box.getLine` | `startPointIndex: int -> endPointIndex: int -> b: Box -> Line3D` |
| `.GetPoint` | `pointIndex: int -> Pnt` |
| `Box.getPoint` | `pointIndex: int -> b: Box -> Pnt` |
| `.HasVolume` | `bool` |
| `Box.hasVolume` | `b: Box -> bool` |
| `.IntersectPlane` | `pl: NPlane -> ResizeArray<Pnt>` |
| `Box.intersectPlane` | `pl: NPlane -> b: Box -> ResizeArray<Pnt>` |
| `.IntersectRay` | `ray: Line3D -> float * float` |
| `Box.intersectRay` | `ray: Line3D -> box: Box -> float * float` |
| `.IsFlat` | `bool` |
| `Box.isFlat` | `b: Box -> bool` |
| `.IsLine` | `bool` |
| `Box.isLine` | `b: Box -> bool` |
| `.IsPoint` | `bool` |
| `Box.isPoint` | `b: Box -> bool` |
| `.IsValid` | `bool` |
| `Box.isValid` | `b: Box -> bool` |
| `.IsZero` | `bool` |
| `Box.isZero` | `b: Box -> bool` |
| `.LeftFace` | `Rect3D` |
| `Box.leftFace` | `b: Box -> Rect3D` |
| `.LongestEdge` | `float` |
| `Box.longestEdge` | `b: Box -> float` |
| `.LongestEdgeSq` | `float` |
| `Box.longestEdgeSq` | `b: Box -> float` |
| `.Move` | `v: Vec -> Box` |
| `Box.move` | `v: Vec -> b: Box -> Box` |
| `.MoveX` | `distance: float -> Box` |
| `Box.moveX` | `distance: float -> b: Box -> Box` |
| `.MoveY` | `distance: float -> Box` |
| `Box.moveY` | `distance: float -> b: Box -> Box` |
| `.MoveZ` | `distance: float -> Box` |
| `Box.moveZ` | `distance: float -> b: Box -> Box` |
| `.Origin` | `Pnt` |
| `Box.origin` | `b: Box -> Pnt` |
| `.Plane` | `PPlane` |
| `Box.plane` | `b: Box -> PPlane` |
| `.Pnt0` | `Pnt` |
| `Box.pnt0` | `b: Box -> Pnt` |
| `.Pnt0X` | `float` |
| `.Pnt0Y` | `float` |
| `.Pnt0Z` | `float` |
| `.Pnt1` | `Pnt` |
| `Box.pnt1` | `b: Box -> Pnt` |
| `.Pnt1X` | `float` |
| `.Pnt1Y` | `float` |
| `.Pnt1Z` | `float` |
| `.Pnt2` | `Pnt` |
| `Box.pnt2` | `b: Box -> Pnt` |
| `.Pnt2X` | `float` |
| `.Pnt2Y` | `float` |
| `.Pnt2Z` | `float` |
| `.Pnt3` | `Pnt` |
| `Box.pnt3` | `b: Box -> Pnt` |
| `.Pnt3X` | `float` |
| `.Pnt3Y` | `float` |
| `.Pnt3Z` | `float` |
| `.Pnt4` | `Pnt` |
| `Box.pnt4` | `b: Box -> Pnt` |
| `.Pnt4X` | `float` |
| `.Pnt4Y` | `float` |
| `.Pnt4Z` | `float` |
| `.Pnt5` | `Pnt` |
| `Box.pnt5` | `b: Box -> Pnt` |
| `.Pnt5X` | `float` |
| `.Pnt5Y` | `float` |
| `.Pnt5Z` | `float` |
| `.Pnt6` | `Pnt` |
| `Box.pnt6` | `b: Box -> Pnt` |
| `.Pnt6X` | `float` |
| `.Pnt6Y` | `float` |
| `.Pnt6Z` | `float` |
| `.Pnt7` | `Pnt` |
| `Box.pnt7` | `b: Box -> Pnt` |
| `.Pnt7X` | `float` |
| `.Pnt7Y` | `float` |
| `.Pnt7Z` | `float` |
| `.Points` | `Pnt array` |
| `Box.points` | `b: Box -> Pnt array` |
| `.RightFace` | `Rect3D` |
| `Box.rightFace` | `b: Box -> Rect3D` |
| `.Rotate` | `q: Quaternion -> Box` |
| `Box.rotate` | `q: Quaternion -> b: Box -> Box` |
| `.RotateAround` | `cen: Pnt -> q: Quaternion -> Box` |
| `Box.rotateAround` | `cen: Pnt -> q: Quaternion -> b: Box -> Box` |
| `.Scale` | `factor: float -> Box` |
| `Box.scale` | `factor: float -> b: Box -> Box` |
| `.ScaleOn` | `cen: Pnt -> factor: float -> Box` |
| `Box.scaleOn` | `cen: Pnt -> factor: float -> b: Box -> Box` |
| `.ShortestEdge` | `float` |
| `Box.shortestEdge` | `b: Box -> float` |
| `.ShortestEdgeSq` | `float` |
| `Box.shortestEdgeSq` | `b: Box -> float` |
| `.SizeX` | `float` |
| `Box.sizeX` | `b: Box -> float` |
| `.SizeXSq` | `float` |
| `Box.sizeXSq` | `b: Box -> float` |
| `.SizeY` | `float` |
| `Box.sizeY` | `b: Box -> float` |
| `.SizeYSq` | `float` |
| `Box.sizeYSq` | `b: Box -> float` |
| `.SizeZ` | `float` |
| `Box.sizeZ` | `b: Box -> float` |
| `.SizeZSq` | `float` |
| `Box.sizeZSq` | `b: Box -> float` |
| `.TopFace` | `Rect3D` |
| `Box.topFace` | `b: Box -> Rect3D` |
| `.ToString` | `unit -> string` |
| `.TransformRigid` | `m: RigidMatrix -> Box` |
| `Box.transformRigid` | `m: RigidMatrix -> b: Box -> Box` |
| `.TryIntersectRay` | `ray: Line3D -> (float * float) voption` |
| `Box.tryIntersectRay` | `ray: Line3D -> box: Box -> (float * float) voption` |
| `.Volume` | `float` |
| `Box.volume` | `b: Box -> float` |
| `.XAxis` | `Vec` |
| `Box.xAxis` | `b: Box -> Vec` |
| `.XAxisUnit` | `UnitVec` |
| `Box.xAxisUnit` | `b: Box -> UnitVec` |
| `.YAxis` | `Vec` |
| `Box.yAxis` | `b: Box -> Vec` |
| `.YAxisUnit` | `UnitVec` |
| `Box.yAxisUnit` | `b: Box -> UnitVec` |
| `.ZAxis` | `Vec` |
| `Box.zAxis` | `b: Box -> Vec` |
| `.ZAxisUnit` | `UnitVec` |
| `Box.zAxisUnit` | `b: Box -> UnitVec` |
| **Other static members** | |
| `Box.areBoxesAligned` | `cosineTolerance: float<cosine> -> a: Box -> b: Box -> bool` |
| `Box.create` | `xAxis: Vec -> yAxis: Vec -> zAxis: Vec -> origin: Pnt -> Box` |
| `Box.createFromAxesUnchecked` | `origin: Pnt * xAxis: Vec * yAxis: Vec * zAxis: Vec -> Box` |
| `Box.createFromBounds` | `minX: float * minY: float * minZ: float * maxX: float * maxY: float * maxZ: float -> Box` |
| `Box.createFromPlane` | `sizeX: float -> sizeY: float -> sizeZ: float -> plane: PPlane -> Box` |
| `Box.createFromPlaneAndPnts` | `pl: PPlane -> pts: IList<Pnt> -> Box` |
| `Box.createFromRect2D` | `zLow: float -> zHigh: float -> r: Rect2D -> Box` |
| `Box.createFromRect3D` | `zLow: float -> zHigh: float -> r: Rect3D -> Box` |
| `Box.createFromVecsAndPnts` | `dirX: Vec -> dirY: Vec -> pts: IList<Pnt> -> Box` |
| `Box.createUnchecked` | `originX: float * originY: float * originZ: float * xAxisX: float * xAxisY: float * xAxisZ: float * yAxisX: float * yAxisY: float * yAxisZ: float * zAxisX: float * zAxisY: float * zAxisZ: float -> Box` |
| `Box.equalsExact` | `a: Box -> b: Box -> bool` |
| `Box.equalsWithin` | `tol: float -> a: Box -> b: Box -> bool` |
| `Box.expand` | `dist: float -> b: Box -> Box` |
| `Box.expandRel` | `factor: float -> b: Box -> Box` |
| `Box.expandRelXYZ` | `factorX: float -> factorY: float -> factorZ: float -> b: Box -> Box` |
| `Box.expandXYZ` | `distX: float -> distY: float -> distZ: float -> b: Box -> Box` |
| `Box.failIfBoxesNotAlignedTo` | `a: Box -> msg: string -> b: Box -> Box` |
| `Box.getBoxBetween` | `a: Box -> b: Box -> Box` |
| `Box.getBoxesAlignment` | `a: Box -> b: Box -> float` |
| `Box.moveAlongLocalX` | `distX: float -> b: Box -> Box` |
| `Box.moveAlongLocalY` | `distY: float -> b: Box -> Box` |
| `Box.moveAlongLocalZ` | `distZ: float -> b: Box -> Box` |
| `Box.notEqualsWithin` | `tol: float -> a: Box -> b: Box -> bool` |
| `Box.offset` | `distBottom: float -> distFront: float -> distRight: float -> distBack: float -> distLeft: float -> distTop: float -> b: Box -> Box` |
| `Box.tryGetBoxBetween` | `a: Box -> b: Box -> Box voption` |
| `Box.withOrigin` | `origin: Pnt -> b: Box -> Box` |
| `Box.withSizeX` | `sizeX: float -> b: Box -> Box` |
| `Box.withSizeY` | `sizeY: float -> b: Box -> Box` |
| `Box.withSizeZ` | `sizeZ: float -> b: Box -> Box` |

## FreeBox

| FreeBox | Signature |
|:--|:--|
| `.AsFSharpCode` | `string` |
| `FreeBox.asFSharpCode` | `b: FreeBox -> string` |
| `.AsPoints` | `Pnt array` |
| `FreeBox.asPoints` | `b: FreeBox -> Pnt array` |
| `.AsString` | `string` |
| `FreeBox.asString` | `b: FreeBox -> string` |
| `.Clone` | `unit -> FreeBox` |
| `FreeBox.clone` | `b: FreeBox -> FreeBox` |
| `.Duplicate` | `unit -> FreeBox` |
| `FreeBox.duplicate` | `b: FreeBox -> FreeBox` |
| `.Edge01` | `Line3D` |
| `FreeBox.edge01` | `b: FreeBox -> Line3D` |
| `.Edge03` | `Line3D` |
| `FreeBox.edge03` | `b: FreeBox -> Line3D` |
| `.Edge04` | `Line3D` |
| `FreeBox.edge04` | `b: FreeBox -> Line3D` |
| `.Edge10` | `Line3D` |
| `FreeBox.edge10` | `b: FreeBox -> Line3D` |
| `.Edge12` | `Line3D` |
| `FreeBox.edge12` | `b: FreeBox -> Line3D` |
| `.Edge15` | `Line3D` |
| `FreeBox.edge15` | `b: FreeBox -> Line3D` |
| `.Edge21` | `Line3D` |
| `FreeBox.edge21` | `b: FreeBox -> Line3D` |
| `.Edge23` | `Line3D` |
| `FreeBox.edge23` | `b: FreeBox -> Line3D` |
| `.Edge26` | `Line3D` |
| `FreeBox.edge26` | `b: FreeBox -> Line3D` |
| `.Edge30` | `Line3D` |
| `FreeBox.edge30` | `b: FreeBox -> Line3D` |
| `.Edge32` | `Line3D` |
| `FreeBox.edge32` | `b: FreeBox -> Line3D` |
| `.Edge37` | `Line3D` |
| `FreeBox.edge37` | `b: FreeBox -> Line3D` |
| `.Edge40` | `Line3D` |
| `FreeBox.edge40` | `b: FreeBox -> Line3D` |
| `.Edge45` | `Line3D` |
| `FreeBox.edge45` | `b: FreeBox -> Line3D` |
| `.Edge47` | `Line3D` |
| `FreeBox.edge47` | `b: FreeBox -> Line3D` |
| `.Edge51` | `Line3D` |
| `FreeBox.edge51` | `b: FreeBox -> Line3D` |
| `.Edge54` | `Line3D` |
| `FreeBox.edge54` | `b: FreeBox -> Line3D` |
| `.Edge56` | `Line3D` |
| `FreeBox.edge56` | `b: FreeBox -> Line3D` |
| `.Edge62` | `Line3D` |
| `FreeBox.edge62` | `b: FreeBox -> Line3D` |
| `.Edge65` | `Line3D` |
| `FreeBox.edge65` | `b: FreeBox -> Line3D` |
| `.Edge67` | `Line3D` |
| `FreeBox.edge67` | `b: FreeBox -> Line3D` |
| `.Edge73` | `Line3D` |
| `FreeBox.edge73` | `b: FreeBox -> Line3D` |
| `.Edge74` | `Line3D` |
| `FreeBox.edge74` | `b: FreeBox -> Line3D` |
| `.Edge76` | `Line3D` |
| `FreeBox.edge76` | `b: FreeBox -> Line3D` |
| `.Edges` | `Line3D array` |
| `FreeBox.edges` | `b: FreeBox -> Line3D array` |
| `.GetLine` | `startPointIndex: int -> endPointIndex: int -> Line3D` |
| `FreeBox.getLine` | `startPointIndex: int -> endPointIndex: int -> b: FreeBox -> Line3D` |
| `.GetPoint` | `pointIndex: int -> Pnt` |
| `FreeBox.getPoint` | `pointIndex: int -> b: FreeBox -> Pnt` |
| `.GetX` | `pointIndex: int -> float` |
| `FreeBox.getX` | `pointIndex: int -> b: FreeBox -> float` |
| `.GetY` | `pointIndex: int -> float` |
| `FreeBox.getY` | `pointIndex: int -> b: FreeBox -> float` |
| `.GetZ` | `pointIndex: int -> float` |
| `FreeBox.getZ` | `pointIndex: int -> b: FreeBox -> float` |
| `.Move` | `v: Vec -> unit` |
| `FreeBox.move` | `v: Vec -> b: FreeBox -> FreeBox` |
| `.MovePoint` | `pointIndex: int -> v: Vec -> unit` |
| `FreeBox.movePoint` | `pointIndex: int -> v: Vec -> b: FreeBox -> unit` |
| `.MoveX` | `distance: float -> unit` |
| `FreeBox.moveX` | `distance: float -> b: FreeBox -> FreeBox` |
| `.MoveXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> unit` |
| `FreeBox.moveXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> b: FreeBox -> unit` |
| `.MoveY` | `distance: float -> unit` |
| `FreeBox.moveY` | `distance: float -> b: FreeBox -> FreeBox` |
| `.MoveZ` | `distance: float -> unit` |
| `FreeBox.moveZ` | `distance: float -> b: FreeBox -> FreeBox` |
| `.Pnt0` | `Pnt with get, set` |
| `FreeBox.pnt0` | `b: FreeBox -> Pnt` |
| `.Pnt0X` | `float with get, set` |
| `FreeBox.pnt0X` | `b: FreeBox -> float` |
| `.Pnt0Y` | `float with get, set` |
| `FreeBox.pnt0Y` | `b: FreeBox -> float` |
| `.Pnt0Z` | `float with get, set` |
| `FreeBox.pnt0Z` | `b: FreeBox -> float` |
| `.Pnt1` | `Pnt with get, set` |
| `FreeBox.pnt1` | `b: FreeBox -> Pnt` |
| `.Pnt1X` | `float with get, set` |
| `FreeBox.pnt1X` | `b: FreeBox -> float` |
| `.Pnt1Y` | `float with get, set` |
| `FreeBox.pnt1Y` | `b: FreeBox -> float` |
| `.Pnt1Z` | `float with get, set` |
| `FreeBox.pnt1Z` | `b: FreeBox -> float` |
| `.Pnt2` | `Pnt with get, set` |
| `FreeBox.pnt2` | `b: FreeBox -> Pnt` |
| `.Pnt2X` | `float with get, set` |
| `FreeBox.pnt2X` | `b: FreeBox -> float` |
| `.Pnt2Y` | `float with get, set` |
| `FreeBox.pnt2Y` | `b: FreeBox -> float` |
| `.Pnt2Z` | `float with get, set` |
| `FreeBox.pnt2Z` | `b: FreeBox -> float` |
| `.Pnt3` | `Pnt with get, set` |
| `FreeBox.pnt3` | `b: FreeBox -> Pnt` |
| `.Pnt3X` | `float with get, set` |
| `FreeBox.pnt3X` | `b: FreeBox -> float` |
| `.Pnt3Y` | `float with get, set` |
| `FreeBox.pnt3Y` | `b: FreeBox -> float` |
| `.Pnt3Z` | `float with get, set` |
| `FreeBox.pnt3Z` | `b: FreeBox -> float` |
| `.Pnt4` | `Pnt with get, set` |
| `FreeBox.pnt4` | `b: FreeBox -> Pnt` |
| `.Pnt4X` | `float with get, set` |
| `FreeBox.pnt4X` | `b: FreeBox -> float` |
| `.Pnt4Y` | `float with get, set` |
| `FreeBox.pnt4Y` | `b: FreeBox -> float` |
| `.Pnt4Z` | `float with get, set` |
| `FreeBox.pnt4Z` | `b: FreeBox -> float` |
| `.Pnt5` | `Pnt with get, set` |
| `FreeBox.pnt5` | `b: FreeBox -> Pnt` |
| `.Pnt5X` | `float with get, set` |
| `FreeBox.pnt5X` | `b: FreeBox -> float` |
| `.Pnt5Y` | `float with get, set` |
| `FreeBox.pnt5Y` | `b: FreeBox -> float` |
| `.Pnt5Z` | `float with get, set` |
| `FreeBox.pnt5Z` | `b: FreeBox -> float` |
| `.Pnt6` | `Pnt with get, set` |
| `FreeBox.pnt6` | `b: FreeBox -> Pnt` |
| `.Pnt6X` | `float with get, set` |
| `FreeBox.pnt6X` | `b: FreeBox -> float` |
| `.Pnt6Y` | `float with get, set` |
| `FreeBox.pnt6Y` | `b: FreeBox -> float` |
| `.Pnt6Z` | `float with get, set` |
| `FreeBox.pnt6Z` | `b: FreeBox -> float` |
| `.Pnt7` | `Pnt with get, set` |
| `FreeBox.pnt7` | `b: FreeBox -> Pnt` |
| `.Pnt7X` | `float with get, set` |
| `FreeBox.pnt7X` | `b: FreeBox -> float` |
| `.Pnt7Y` | `float with get, set` |
| `FreeBox.pnt7Y` | `b: FreeBox -> float` |
| `.Pnt7Z` | `float with get, set` |
| `FreeBox.pnt7Z` | `b: FreeBox -> float` |
| `.Rotate` | `q: Quaternion -> unit` |
| `FreeBox.rotate` | `q: Quaternion -> b: FreeBox -> FreeBox` |
| `.RotateAround` | `cen: Pnt -> q: Quaternion -> unit` |
| `FreeBox.rotateAround` | `cen: Pnt -> q: Quaternion -> b: FreeBox -> FreeBox` |
| `.Scale` | `factor: float -> unit` |
| `FreeBox.scale` | `factor: float -> b: FreeBox -> FreeBox` |
| `.ScaleOn` | `cen: Pnt -> factor: float -> unit` |
| `FreeBox.scaleOn` | `cen: Pnt -> factor: float -> b: FreeBox -> FreeBox` |
| `.SetPoint` | `pointIndex: int -> p: Pnt -> unit` |
| `FreeBox.setPoint` | `pointIndex: int -> p: Pnt -> b: FreeBox -> unit` |
| `.SetXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> unit` |
| `FreeBox.setXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> b: FreeBox -> unit` |
| `.ToString` | `unit -> string` |
| `.Transform` | `m: Matrix -> unit` |
| `FreeBox.transform` | `m: Matrix -> b: FreeBox -> FreeBox` |
| `.TransformRigid` | `m: RigidMatrix -> unit` |
| `FreeBox.transformRigid` | `m: RigidMatrix -> b: FreeBox -> FreeBox` |
| `.XYZs` | `float array` |
| `FreeBox.xyzs` | `b: FreeBox -> float array` |
| **Other static members** | |
| `FreeBox.createFrom4PtArray` | `zMin: float -> zMax: float -> pts: Pt array -> FreeBox` |
| `FreeBox.createFrom4Pts` | `zMin: float * zMax: float * a: Pt * b: Pt * c: Pt * d: Pt -> FreeBox` |
| `FreeBox.createFrom8PntArray` | `pts: Pnt array -> FreeBox` |
| `FreeBox.createFromBox` | `box: Box -> FreeBox` |
| `FreeBox.createFromXYZs` | `xyzs: float array -> FreeBox` |

## BBox

| BBox | Signature |
|:--|:--|
| `.MinX` | `float` |
| `.MinY` | `float` |
| `.MinZ` | `float` |
| `.MaxX` | `float` |
| `.MaxY` | `float` |
| `.MaxZ` | `float` |
| `.AsBox` | `Box` |
| `BBox.asBox` | `b: BBox -> Box` |
| `.AsBRect` | `BRect` |
| `BBox.asBRect` | `b: BBox -> BRect` |
| `.AsFSharpCode` | `string` |
| `BBox.asFSharpCode` | `b: BBox -> string` |
| `.AsString` | `string` |
| `BBox.asString` | `b: BBox -> string` |
| `.BackFace` | `Rect3D` |
| `BBox.backFace` | `b: BBox -> Rect3D` |
| `.BottomFace` | `Rect3D` |
| `BBox.bottomFace` | `b: BBox -> Rect3D` |
| `.BottomPoints` | `Pnt array` |
| `BBox.bottomPoints` | `b: BBox -> Pnt array` |
| `.BottomPointsLooped` | `Pnt array` |
| `BBox.bottomPointsLooped` | `b: BBox -> Pnt array` |
| `.Center` | `Pnt` |
| `BBox.center` | `b: BBox -> Pnt` |
| `.Contains` | `o: BBox -> bool` |
| `BBox.contains` | `boxInside: BBox -> surroundingBox: BBox -> bool` |
| `.ContainsPnt` | `p: Pnt -> bool` |
| `BBox.containsPnt` | `pnt: Pnt -> box: BBox -> bool` |
| `.ContainsXYZ` | `x: float * y: float * z: float -> bool` |
| `BBox.containsXYZ` | `x: float -> y: float -> z: float -> box: BBox -> bool` |
| `.CountZeroSides` | `int` |
| `BBox.countZeroSides` | `b: BBox -> int` |
| `.Diagonal` | `Vec` |
| `BBox.diagonal` | `b: BBox -> Vec` |
| `.DoesIntersectPlane` | `pl: NPlane -> bool` |
| `BBox.doesIntersectPlane` | `pl: NPlane -> b: BBox -> bool` |
| `.Edge01` | `Line3D` |
| `BBox.edge01` | `b: BBox -> Line3D` |
| `.Edge03` | `Line3D` |
| `BBox.edge03` | `b: BBox -> Line3D` |
| `.Edge04` | `Line3D` |
| `BBox.edge04` | `b: BBox -> Line3D` |
| `.Edge10` | `Line3D` |
| `BBox.edge10` | `b: BBox -> Line3D` |
| `.Edge12` | `Line3D` |
| `BBox.edge12` | `b: BBox -> Line3D` |
| `.Edge15` | `Line3D` |
| `BBox.edge15` | `b: BBox -> Line3D` |
| `.Edge21` | `Line3D` |
| `BBox.edge21` | `b: BBox -> Line3D` |
| `.Edge23` | `Line3D` |
| `BBox.edge23` | `b: BBox -> Line3D` |
| `.Edge26` | `Line3D` |
| `BBox.edge26` | `b: BBox -> Line3D` |
| `.Edge30` | `Line3D` |
| `BBox.edge30` | `b: BBox -> Line3D` |
| `.Edge32` | `Line3D` |
| `BBox.edge32` | `b: BBox -> Line3D` |
| `.Edge37` | `Line3D` |
| `BBox.edge37` | `b: BBox -> Line3D` |
| `.Edge40` | `Line3D` |
| `BBox.edge40` | `b: BBox -> Line3D` |
| `.Edge45` | `Line3D` |
| `BBox.edge45` | `b: BBox -> Line3D` |
| `.Edge47` | `Line3D` |
| `BBox.edge47` | `b: BBox -> Line3D` |
| `.Edge51` | `Line3D` |
| `BBox.edge51` | `b: BBox -> Line3D` |
| `.Edge54` | `Line3D` |
| `BBox.edge54` | `b: BBox -> Line3D` |
| `.Edge56` | `Line3D` |
| `BBox.edge56` | `b: BBox -> Line3D` |
| `.Edge62` | `Line3D` |
| `BBox.edge62` | `b: BBox -> Line3D` |
| `.Edge65` | `Line3D` |
| `BBox.edge65` | `b: BBox -> Line3D` |
| `.Edge67` | `Line3D` |
| `BBox.edge67` | `b: BBox -> Line3D` |
| `.Edge73` | `Line3D` |
| `BBox.edge73` | `b: BBox -> Line3D` |
| `.Edge74` | `Line3D` |
| `BBox.edge74` | `b: BBox -> Line3D` |
| `.Edge76` | `Line3D` |
| `BBox.edge76` | `b: BBox -> Line3D` |
| `.Edges` | `Line3D array` |
| `BBox.edges` | `b: BBox -> Line3D array` |
| `.EvaluateAt` | `xParameter: float * yParameter: float * zParameter: float -> Pnt` |
| `BBox.evaluateAt` | `xParameter: float -> yParameter: float -> zParameter: float -> b: BBox -> Pnt` |
| `.Expand` | `dist: float -> BBox` |
| `BBox.expand` | `dist: float -> b: BBox -> BBox` |
| `.ExpandClamped` | `dist: float -> BBox` |
| `BBox.expandClamped` | `dist: float -> b: BBox -> BBox` |
| `.ExpandClampedXYZ` | `xDist: float * yDist: float * zDist: float -> BBox` |
| `BBox.expandClampedXYZ` | `xDist: float -> yDist: float -> zDist: float -> b: BBox -> BBox` |
| `.ExpandXAxis` | `startDist: float * endDist: float -> BBox` |
| `BBox.expandXAxis` | `startDist: float -> endDist: float -> b: BBox -> BBox` |
| `.ExpandXYZ` | `xDist: float * yDist: float * zDist: float -> BBox` |
| `BBox.expandXYZ` | `xDist: float -> yDist: float -> zDist: float -> b: BBox -> BBox` |
| `.ExpandYAxis` | `startDist: float * endDist: float -> BBox` |
| `BBox.expandYAxis` | `startDist: float -> endDist: float -> b: BBox -> BBox` |
| `.ExpandZAxis` | `startDist: float * endDist: float -> BBox` |
| `BBox.expandZAxis` | `startDist: float -> endDist: float -> b: BBox -> BBox` |
| `.Faces` | `Rect3D array` |
| `BBox.faces` | `b: BBox -> Rect3D array` |
| `.FrontFace` | `Rect3D` |
| `BBox.frontFace` | `b: BBox -> Rect3D` |
| `.GetEdge` | `edgeIndex: int -> Line3D` |
| `BBox.getEdge` | `edgeIndex: int -> b: BBox -> Line3D` |
| `.GetFace` | `faceIndex: int -> Rect3D` |
| `BBox.getFace` | `faceIndex: int -> b: BBox -> Rect3D` |
| `.GetLine` | `startPointIndex: int * endPointIndex: int -> Line3D` |
| `BBox.getLine` | `startPointIndex: int -> endPointIndex: int -> b: BBox -> Line3D` |
| `.GetPoint` | `pointIndex: int -> Pnt` |
| `BBox.getPoint` | `pointIndex: int -> b: BBox -> Pnt` |
| `.HasVolume` | `bool` |
| `BBox.hasVolume` | `b: BBox -> bool` |
| `.Intersection` | `a: BBox -> BBox` |
| `BBox.intersection` | `a: BBox -> b: BBox -> BBox` |
| `.IntersectPlane` | `pl: NPlane -> ResizeArray<Pnt>` |
| `BBox.intersectPlane` | `pl: NPlane -> b: BBox -> ResizeArray<Pnt>` |
| `.IntersectRay` | `ray: Line3D -> float * float` |
| `BBox.intersectRay` | `ray: Line3D -> b: BBox -> float * float` |
| `.IsFlat` | `bool` |
| `BBox.isFlat` | `b: BBox -> bool` |
| `.IsLine` | `bool` |
| `BBox.isLine` | `b: BBox -> bool` |
| `.IsOverlapping` | `a: BBox -> bool` |
| `BBox.isOverlapping` | `a: BBox -> b: BBox -> bool` |
| `.IsOverlappingOrClose` | `a: BBox * tolerance: float -> bool` |
| `BBox.isOverlappingOrClose` | `tolerance: float -> a: BBox -> b: BBox -> bool` |
| `.IsPoint` | `bool` |
| `BBox.isPoint` | `b: BBox -> bool` |
| `.IsTouching` | `other: BBox * ?tolerance: float -> bool` |
| `BBox.isTouching` | `tolerance: float -> other: BBox -> thisBox: BBox -> bool` |
| `.IsValid` | `bool` |
| `BBox.isValid` | `b: BBox -> bool` |
| `.IsZero` | `bool` |
| `BBox.isZero` | `b: BBox -> bool` |
| `.LeftFace` | `Rect3D` |
| `BBox.leftFace` | `b: BBox -> Rect3D` |
| `.LongestEdge` | `float` |
| `BBox.longestEdge` | `b: BBox -> float` |
| `.MaxPnt` | `Pnt` |
| `BBox.maxPnt` | `b: BBox -> Pnt` |
| `.MinPnt` | `Pnt` |
| `BBox.minPnt` | `b: BBox -> Pnt` |
| `.Pnt0` | `Pnt` |
| `BBox.pnt0` | `b: BBox -> Pnt` |
| `.Pnt1` | `Pnt` |
| `BBox.pnt1` | `b: BBox -> Pnt` |
| `.Pnt2` | `Pnt` |
| `BBox.pnt2` | `b: BBox -> Pnt` |
| `.Pnt3` | `Pnt` |
| `BBox.pnt3` | `b: BBox -> Pnt` |
| `.Pnt4` | `Pnt` |
| `BBox.pnt4` | `b: BBox -> Pnt` |
| `.Pnt5` | `Pnt` |
| `BBox.pnt5` | `b: BBox -> Pnt` |
| `.Pnt6` | `Pnt` |
| `BBox.pnt6` | `b: BBox -> Pnt` |
| `.Pnt7` | `Pnt` |
| `BBox.pnt7` | `b: BBox -> Pnt` |
| `.Points` | `Pnt array` |
| `BBox.points` | `b: BBox -> Pnt array` |
| `.RightFace` | `Rect3D` |
| `BBox.rightFace` | `b: BBox -> Rect3D` |
| `.ShortestEdge` | `float` |
| `BBox.shortestEdge` | `b: BBox -> float` |
| `.SizeX` | `float` |
| `BBox.sizeX` | `b: BBox -> float` |
| `.SizeY` | `float` |
| `BBox.sizeY` | `b: BBox -> float` |
| `.SizeZ` | `float` |
| `BBox.sizeZ` | `b: BBox -> float` |
| `.TopFace` | `Rect3D` |
| `BBox.topFace` | `b: BBox -> Rect3D` |
| `.TopPoints` | `Pnt array` |
| `BBox.topPoints` | `b: BBox -> Pnt array` |
| `.TopPointsLooped` | `Pnt array` |
| `BBox.topPointsLooped` | `b: BBox -> Pnt array` |
| `.ToString` | `unit -> string` |
| `.TouchingSide` | `other: BBox * ?tolerance: float -> int` |
| `BBox.touchingSide` | `tolerance: float -> other: BBox -> thisBox: BBox -> int` |
| `.TryIntersection` | `a: BBox -> BBox voption` |
| `BBox.tryIntersection` | `a: BBox -> b: BBox -> BBox voption` |
| `.TryIntersectRay` | `ray: Line3D -> (float * float) voption` |
| `BBox.tryIntersectRay` | `ray: Line3D -> b: BBox -> (float * float) voption` |
| `.Union` | `a: BBox -> BBox` |
| `BBox.union` | `a: BBox -> b: BBox -> BBox` |
| `.UnionPnt` | `p: Pnt -> BBox` |
| `BBox.unionPnt` | `p: Pnt -> b: BBox -> BBox` |
| `.Volume` | `float` |
| `BBox.volume` | `b: BBox -> float` |
| **Other static members** | |
| `BBox.create` | `a: Pnt -> b: Pnt -> BBox` |
| `BBox.createCentered` | `sizeX: float -> sizeY: float -> sizeZ: float -> center: Pnt -> BBox` |
| `BBox.createFromBox` | `b: Box -> BBox` |
| `BBox.createFromBRect` | `minZ: float -> maxZ: float -> r: BRect -> BBox` |
| `BBox.createFromIList` | `ps: IList<Pnt> -> BBox` |
| `BBox.createFromLine` | `l: Line3D -> BBox` |
| `BBox.createFromRect3D` | `r: Rect3D -> BBox` |
| `BBox.createFromSeq` | `ps: Pnt seq -> BBox` |
| `BBox.createUnchecked` | `minX: float * minY: float * minZ: float * maxX: float * maxY: float * maxZ: float -> BBox` |
| `BBox.createXYZ` | `ax: float * ay: float * az: float * bx: float * by: float * bz: float -> BBox` |
| `BBox.equalsExact` | `a: BBox -> b: BBox -> bool` |
| `BBox.equalsWithin` | `tol: float -> a: BBox -> b: BBox -> bool` |
| `BBox.expandRel` | `factor: float -> b: BBox -> BBox` |
| `BBox.expandRelXYZ` | `factorX: float -> factorY: float -> factorZ: float -> b: BBox -> BBox` |
| `BBox.move` | `v: Vec -> b: BBox -> BBox` |
| `BBox.moveX` | `translation: float -> b: BBox -> BBox` |
| `BBox.moveY` | `translation: float -> b: BBox -> BBox` |
| `BBox.moveZ` | `translation: float -> b: BBox -> BBox` |
| `BBox.notEqualsWithin` | `tol: float -> a: BBox -> b: BBox -> bool` |
| `BBox.scale` | `factor: float -> b: BBox -> BBox` |

## Tria3D

| Tria3D | Signature |
|:--|:--|
| `Tria3D.area` | `a: Pnt -> b: Pnt -> c: Pnt -> float` |
| `Tria3D.areaDouble` | `a: Pnt -> b: Pnt -> c: Pnt -> float` |
| `Tria3D.intersectLine` | `p1: Pnt -> p2: Pnt -> p3: Pnt -> line: Line3D -> Pnt` |
| `Tria3D.intersectRay` | `p1: Pnt -> p2: Pnt -> p3: Pnt -> line: Line3D -> float` |
| `Tria3D.intersectRayXYZ` | `lnFromX: float * lnFromY: float * lnFromZ: float * lnToX: float * lnToY: float * lnToZ: float * ptAx: float * ptAy: float * ptAz: float * ptBx: float * ptBy: float * ptBz: float * ptCx: float * ptCy: float * ptCz: float -> float` |
| `Tria3D.intersectTriaVectorsWithRay` | `lnFromX: float * lnFromY: float * lnFromZ: float * dirX: float * dirY: float * dirZ: float * ptAx: float * ptAy: float * ptAz: float * e1x: float * e1y: float * e1z: float * e2x: float * e2y: float * e2z: float -> float` |
| `Tria3D.isLinear` | `a: Pnt -> b: Pnt -> c: Pnt -> bool` |
| `Tria3D.isLinearCustom` | `a: Pnt * b: Pnt * c: Pnt * ?distanceTolerance: float -> bool` |
| `Tria3D.isLinearFast` | `a: Pnt -> b: Pnt -> c: Pnt -> bool` |
| `Tria3D.isLinearFastCustom` | `a: Pnt * b: Pnt * c: Pnt * ?maxAreaParallelogram: float -> bool` |
| `Tria3D.offset` | `dist: float -> a: Pnt -> b: Pnt -> c: Pnt -> Pnt * Pnt * Pnt` |
| `Tria3D.offsetPnt` | `dist: float -> prev: Pnt -> next: Pnt -> pntToOffset: Pnt -> Pnt` |
| `Tria3D.offsetPntVariable` | `distPrev: float -> distNext: float -> prev: Pnt -> next: Pnt -> pntToOffset: Pnt -> Pnt` |
| `Tria3D.tryIntersectLine` | `p1: Pnt -> p2: Pnt -> p3: Pnt -> line: Line3D -> Pnt voption` |
| `Tria3D.tryIntersectRay` | `p1: Pnt -> p2: Pnt -> p3: Pnt -> line: Line3D -> float voption` |
| `Tria3D.tryIntersectRayXYZ` | `lnFromX: float * lnFromY: float * lnFromZ: float * lnToX: float * lnToY: float * lnToZ: float * ptAx: float * ptAy: float * ptAz: float * ptBx: float * ptBy: float * ptBz: float * ptCx: float * ptCy: float * ptCz: float -> float voption` |
| `Tria3D.tryIntersectTriaVectorsWithRay` | `lnFromX: float * lnFromY: float * lnFromZ: float * dirX: float * dirY: float * dirZ: float * ptAx: float * ptAy: float * ptAz: float * e1x: float * e1y: float * e1z: float * e2x: float * e2y: float * e2z: float -> float voption` |
| `Tria3D.tryOffsetPntVariable` | `distPrev: float -> distNext: float -> prev: Pnt -> next: Pnt -> pntToOffset: Pnt -> Pnt voption` |

## Points3D

| Points3D | Signature |
|:--|:--|
| `Points3D.center` | `pts: IList<Pnt> -> Pnt` |
| `Points3D.closestOfTwo` | `pnt1: Pnt -> pnt2: Pnt -> referencePoint: Pnt -> Pnt` |
| `Points3D.closestPoint` | `pnt: Pnt -> pts: IList<Pnt> -> Pnt` |
| `Points3D.closestPointIndex` | `pnt: Pnt -> pts: IList<Pnt> -> int` |
| `Points3D.closestPointsIndices` | `xs: IList<Pnt> -> ys: IList<Pnt> -> int * int` |
| `Points3D.minDistanceBetweenPointSets` | `xs: IList<Pnt> -> ys: IList<Pnt> -> float` |
| `Points3D.mostDistantPoint` | `checkAgainst: IList<Pnt> -> findPointFrom: IList<Pnt> -> Pnt` |
| `Points3D.mostDistantPointIndices` | `checkAgainst: IList<Pnt> -> findPointFrom: IList<Pnt> -> int * int` |
| `Points3D.normalOfPoints` | `pts: IList<Pnt> -> Vec` |

## module Offset3D

| Offset3D | Signature |
|:--|:--|
| `Offset3D.getCollinearNeighbors` | `dirs: ResizeArray<OffsetDirection voption> -> ResizeArray<CollinearPoint>` |
| `Offset3D.getOffsetDirections` | `uvs: ResizeArray<float> * refNormal: UnitVec * isOpen: bool * considerCollinearBelow: float<cosine> * failAtUTurnAbove: float<cosine> -> ResizeArray<OffsetDirection voption>` |
| `Offset3D.getSegmentUnitVectors` | `xyzs: ResizeArray<float> -> ResizeArray<float>` |
| `Offset3D.offset` | `distInPlane: float -> distPerpendicular: float -> xyzs: ResizeArray<float> -> ResizeArray<float>` |
| `Offset3D.offsetByDirections` | `xyzs: ResizeArray<float> * dirs: ResizeArray<OffsetDirection voption> * distInPlane: float * distPerpendicular: float -> ResizeArray<float>` |
| `Offset3D.offsetByNormal` | `refNormal: UnitVec -> distInPlane: float -> distPerpendicular: float -> xyzs: ResizeArray<float> -> ResizeArray<float>` |
| `Offset3D.offsetVariable` | `distancesInPlane: ResizeArray<float> -> distancesPerpendicular: ResizeArray<float> -> xyzs: ResizeArray<float> -> ResizeArray<float>` |
| `Offset3D.offsetVariableByDirections` | `xyzs: ResizeArray<float> * segmentDirs: ResizeArray<float> * offDirs: ResizeArray<OffsetDirection voption> * distsInPlane: IList<float> * distsPerpendicular: IList<float> * isClosed: bool * varDistParallelBehavior: VarDistParallel -> ResizeArray<float>` |
| `Offset3D.offsetVariableByNormal` | `refNormal: UnitVec -> distancesInPlane: ResizeArray<float> -> distancesPerpendicular: ResizeArray<float> -> xyzs: ResizeArray<float> -> ResizeArray<float>` |
| `Offset3D.offsetVariableByNormalCustom` | `xyzs: ResizeArray<float> * distancesInPlane: ResizeArray<float> * distancesPerpendicular: ResizeArray<float> * refNormal: UnitVec * varDistParallelBehavior: VarDistParallel -> ResizeArray<float>` |
| `Offset3D.openTolerance` | `float` |
| `Offset3D.openToleranceSq` | `float` |
| `Offset3D.removeUTurns` | `xyzs: ResizeArray<float> * uvs: ResizeArray<float> * minCos: float<cosine> -> ResizeArray<float>` |
| `Offset3D.setOffCorner` | `res: ResizeArray<float> * x: float * y: float * z: float * distInPlane: float * distPerp: float * nPrev: UnitVec * nNext: UnitVec * perpDir: UnitVec * cosine: float -> unit` |
| `Offset3D.setOffCornerVar` | `res: ResizeArray<float> * x: float * y: float * z: float * distInPlanePrev: float * distInPlaneNext: float * distPerp: float * nPrev: UnitVec * nNext: UnitVec * perpDir: UnitVec * cosine: float * vNext: UnitVec -> unit` |

## Offset3D.VarDistParallel

| VarDistParallel | Signature |
|:--|:--|
| `Fail` | `VarDistParallel = 1` |
| `Skip` | `VarDistParallel = 2` |
| `Proportional` | `VarDistParallel = 3` |
| `Project` | `VarDistParallel = 4` |

## Offset3D.OffsetDirection

| OffsetDirection | Signature |
|:--|:--|
| `.prevInPlane` | `UnitVec` |
| `.nextInPlane` | `UnitVec` |
| `.perpDir` | `UnitVec` |

## Offset3D.CollinearPoint

| CollinearPoint | Signature |
|:--|:--|
| `.idx` | `int` |
| `.prevOK` | `int` |
| `.nextOK` | `int` |

## Polyline3D

| Polyline3D | Signature |
|:--|:--|
| `new` | `capacity: int -> Polyline3D` |
| `new` | `points: Pnt seq -> Polyline3D` |
| `.AddPoint` | `pnt: Pnt -> unit` |
| `Polyline3D.addPoint` | `pnt: Pnt -> p: Polyline3D -> unit` |
| `.AddXYZ` | `x: float * y: float * z: float -> unit` |
| `Polyline3D.addXYZ` | `x: float -> y: float -> z: float -> p: Polyline3D -> unit` |
| `.AreaIn2D` | `float` |
| `Polyline3D.areaIn2D` | `pl: Polyline3D -> float` |
| `.AsFSharpCode` | `string` |
| `Polyline3D.asFSharpCode` | `pl: Polyline3D -> string` |
| `.AsPoints` | `ResizeArray<Pnt>` |
| `Polyline3D.asPoints` | `p: Polyline3D -> ResizeArray<Pnt>` |
| `.AsString` | `string` |
| `Polyline3D.asString` | `pl: Polyline3D -> string` |
| `.AverageNormal` | `Vec` |
| `Polyline3D.averageNormal` | `pl: Polyline3D -> Vec` |
| `.AveragePlane` | `?tolerance: float -> NPlane` |
| `Polyline3D.averagePlane` | `tolerance: float -> pl: Polyline3D -> NPlane` |
| `.BoundingBox` | `BBox` |
| `Polyline3D.boundingBox` | `pl: Polyline3D -> BBox` |
| `.Center` | `Pnt` |
| `Polyline3D.center` | `pl: Polyline3D -> Pnt` |
| `.Clone` | `unit -> Polyline3D` |
| `Polyline3D.clone` | `pl: Polyline3D -> Polyline3D` |
| `.Close` | `?toleranceForAddingPoint: float -> unit` |
| `Polyline3D.close` | `toleranceForAddingPoint: float -> pl: Polyline3D -> Polyline3D` |
| `.ClosestParameter` | `p: Pnt -> float` |
| `Polyline3D.closestParameter` | `pnt: Pnt -> pl: Polyline3D -> float` |
| `.ClosestParameterXYZ` | `x: float * y: float * z: float -> float` |
| `Polyline3D.closestParameterXYZ` | `x: float -> y: float -> z: float -> pl: Polyline3D -> float` |
| `.ClosestPoint` | `p: Pnt -> Pnt` |
| `Polyline3D.closestPoint` | `pnt: Pnt -> pl: Polyline3D -> Pnt` |
| `.ClosestPointIndex` | `p: Pnt -> int` |
| `Polyline3D.closestPointIndex` | `pnt: Pnt -> pl: Polyline3D -> int` |
| `.DistanceTo` | `p: Pnt -> float` |
| `Polyline3D.distanceTo` | `pnt: Pnt -> pl: Polyline3D -> float` |
| `.DistanceToXYZ` | `x: float * y: float * z: float -> float` |
| `Polyline3D.distanceToXYZ` | `x: float -> y: float -> z: float -> pl: Polyline3D -> float` |
| `.Duplicate` | `unit -> Polyline3D` |
| `Polyline3D.duplicate` | `pl: Polyline3D -> Polyline3D` |
| `.End` | `Pnt with get, set` |
| `.EnsureClockwiseIn2D` | `unit -> unit` |
| `Polyline3D.ensureClockwiseIn2D` | `pl: Polyline3D -> Polyline3D` |
| `.EnsureCounterClockwiseIn2D` | `unit -> unit` |
| `Polyline3D.ensureCounterClockwiseIn2D` | `pl: Polyline3D -> Polyline3D` |
| `.EvaluateAt` | `t: float -> Pnt` |
| `Polyline3D.evaluateAt` | `t: float -> pl: Polyline3D -> Pnt` |
| `.FirstPoint` | `Pnt with get, set` |
| `Polyline3D.firstPoint` | `pl: Polyline3D -> Pnt` |
| `.FirstSegment` | `Line3D` |
| `Polyline3D.firstSegment` | `pl: Polyline3D -> Line3D` |
| `.GetPoint` | `pointIndex: int -> Pnt` |
| `Polyline3D.getPoint` | `pointIndex: int -> p: Polyline3D -> Pnt` |
| `.GetSegment` | `i: int -> Line3D` |
| `Polyline3D.getSegment` | `i: int -> pl: Polyline3D -> Line3D` |
| `.GetX` | `pointIndex: int -> float` |
| `Polyline3D.getX` | `pointIndex: int -> p: Polyline3D -> float` |
| `.GetY` | `pointIndex: int -> float` |
| `Polyline3D.getY` | `pointIndex: int -> p: Polyline3D -> float` |
| `.GetZ` | `pointIndex: int -> float` |
| `Polyline3D.getZ` | `pointIndex: int -> p: Polyline3D -> float` |
| `.IsAlmostClosed` | `tolerance: float -> bool` |
| `Polyline3D.isAlmostClosed` | `tolerance: float -> pl: Polyline3D -> bool` |
| `.IsClockwiseIn2D` | `bool` |
| `Polyline3D.isClockwiseIn2D` | `pl: Polyline3D -> bool` |
| `.IsClosed` | `bool` |
| `Polyline3D.isClosed` | `pl: Polyline3D -> bool` |
| `.IsCounterClockwiseIn2D` | `bool` |
| `Polyline3D.isCounterClockwiseIn2D` | `pl: Polyline3D -> bool` |
| `.IsPlanar` | `?tolerance: float -> bool` |
| `Polyline3D.isPlanar` | `tolerance: float -> pl: Polyline3D -> bool` |
| `.LastPoint` | `Pnt with get, set` |
| `Polyline3D.lastPoint` | `pl: Polyline3D -> Pnt` |
| `.LastSegment` | `Line3D` |
| `Polyline3D.lastSegment` | `pl: Polyline3D -> Line3D` |
| `.Length` | `float` |
| `Polyline3D.length` | `p: Polyline3D -> float` |
| `.Move` | `v: Vec -> unit` |
| `Polyline3D.move` | `v: Vec -> pl: Polyline3D -> Polyline3D` |
| `.MovePoint` | `pointIndex: int * v: Vec -> unit` |
| `Polyline3D.movePoint` | `pointIndex: int -> v: Vec -> p: Polyline3D -> unit` |
| `.MovePointPreservingClosure` | `pointIndex: int * v: Vec -> unit` |
| `Polyline3D.movePointPreservingClosure` | `pointIndex: int -> v: Vec -> p: Polyline3D -> unit` |
| `.MoveX` | `distance: float -> unit` |
| `Polyline3D.moveX` | `distance: float -> pl: Polyline3D -> Polyline3D` |
| `.MoveXYZ` | `pointIndex: int * x: float * y: float * z: float -> unit` |
| `Polyline3D.moveXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> p: Polyline3D -> unit` |
| `.MoveXYZPreservingClosure` | `pointIndex: int * x: float * y: float * z: float -> unit` |
| `Polyline3D.moveXYZPreservingClosure` | `pointIndex: int -> x: float -> y: float -> z: float -> p: Polyline3D -> unit` |
| `.MoveY` | `distance: float -> unit` |
| `Polyline3D.moveY` | `distance: float -> pl: Polyline3D -> Polyline3D` |
| `.MoveZ` | `distance: float -> unit` |
| `Polyline3D.moveZ` | `distance: float -> pl: Polyline3D -> Polyline3D` |
| `.PointCount` | `int` |
| `Polyline3D.pointCount` | `p: Polyline3D -> int` |
| `.RemoveDuplicateAndCollinearPoints` | `angleTolerance: float<cosine> -> distanceTolerance: float -> unit` |
| `Polyline3D.removeDuplicateAndCollinearPoints` | `angleTolerance: float<cosine> -> distanceTolerance: float -> pl: Polyline3D -> Polyline3D` |
| `.RemoveDuplicatePoints` | `distanceTolerance: float -> unit` |
| `Polyline3D.removeDuplicatePoints` | `distanceTolerance: float -> pl: Polyline3D -> Polyline3D` |
| `.RemoveDuplicatePointsAndReport` | `distanceTolerance: float -> ResizeArray<Pnt>` |
| `Polyline3D.removeDuplicatePointsAndReport` | `distanceTolerance: float -> pl: Polyline3D -> Polyline3D * ResizeArray<Pnt>` |
| `.RemoveDuplicatePointsFaithfully` | `distanceTolerance: float -> unit` |
| `Polyline3D.removeDuplicatePointsFaithfully` | `distanceTolerance: float -> pl: Polyline3D -> Polyline3D` |
| `.RemoveUTurns` | `minCos: float<cosine> -> unit` |
| `Polyline3D.removeUTurns` | `minCos: float<cosine> -> polyLine: Polyline3D -> Polyline3D` |
| `.RemoveUTurnsDeeply` | `minCos: float<cosine> -> unit` |
| `Polyline3D.removeUTurnsDeeply` | `minCos: float<cosine> -> polyLine: Polyline3D -> Polyline3D` |
| `.Reverse` | `unit -> unit` |
| `Polyline3D.reverse` | `p: Polyline3D -> Polyline3D` |
| `.Rotate` | `q: Quaternion -> unit` |
| `Polyline3D.rotate` | `q: Quaternion -> pl: Polyline3D -> Polyline3D` |
| `.RotateAround` | `cen: Pnt -> q: Quaternion -> unit` |
| `Polyline3D.rotateAround` | `cen: Pnt -> q: Quaternion -> pl: Polyline3D -> Polyline3D` |
| `.RotateAroundOnZ` | `cen: Pnt -> r: Rotation2D -> unit` |
| `Polyline3D.rotateAroundOnZ` | `cen: Pnt -> r: Rotation2D -> pl: Polyline3D -> Polyline3D` |
| `.RotateOnZ` | `r: Rotation2D -> unit` |
| `Polyline3D.rotateOnZ` | `r: Rotation2D -> pl: Polyline3D -> Polyline3D` |
| `.Scale` | `factor: float -> unit` |
| `Polyline3D.scale` | `factor: float -> pl: Polyline3D -> Polyline3D` |
| `.ScaleOn` | `cen: Pnt -> factor: float -> unit` |
| `Polyline3D.scaleOn` | `cen: Pnt -> factor: float -> pl: Polyline3D -> Polyline3D` |
| `.SecondLastPoint` | `Pnt with get, set` |
| `Polyline3D.secondLastPoint` | `pl: Polyline3D -> Pnt` |
| `.SecondPoint` | `Pnt with get, set` |
| `Polyline3D.secondPoint` | `pl: Polyline3D -> Pnt` |
| `.SegmentCount` | `int` |
| `Polyline3D.segmentCount` | `p: Polyline3D -> int` |
| `.Segments` | `ResizeArray<Line3D>` |
| `Polyline3D.segments` | `pl: Polyline3D -> ResizeArray<Line3D>` |
| `.SegmentVectors` | `ResizeArray<Vec>` |
| `Polyline3D.segmentVectors` | `pl: Polyline3D -> ResizeArray<Vec>` |
| `.SegmentVectorsXYZ` | `ResizeArray<float>` |
| `Polyline3D.segmentVectorsXYZ` | `p: Polyline3D -> ResizeArray<float>` |
| `.SetPoint` | `pointIndex: int * pnt: Pnt -> unit` |
| `Polyline3D.setPoint` | `pointIndex: int -> pnt: Pnt -> p: Polyline3D -> unit` |
| `.SetPointPreservingClosure` | `pointIndex: int * pnt: Pnt -> unit` |
| `Polyline3D.setPointPreservingClosure` | `pointIndex: int -> pnt: Pnt -> p: Polyline3D -> unit` |
| `.SetXYZ` | `pointIndex: int * x: float * y: float * z: float -> unit` |
| `Polyline3D.setXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> p: Polyline3D -> unit` |
| `.SetXYZPreservingClosure` | `pointIndex: int * x: float * y: float * z: float -> unit` |
| `Polyline3D.setXYZPreservingClosure` | `pointIndex: int -> x: float -> y: float -> z: float -> p: Polyline3D -> unit` |
| `.SignedAreaIn2D` | `float` |
| `Polyline3D.signedAreaIn2D` | `pl: Polyline3D -> float` |
| `.Start` | `Pnt with get, set` |
| `Polyline3D.start` | `pl: Polyline3D -> Pnt` |
| `.ToString` | `unit -> string` |
| `.Transform` | `m: Matrix -> unit` |
| `Polyline3D.transform` | `m: Matrix -> pl: Polyline3D -> Polyline3D` |
| `.TransformRigid` | `m: RigidMatrix -> unit` |
| `Polyline3D.transformRigid` | `m: RigidMatrix -> pl: Polyline3D -> Polyline3D` |
| `.TryAverageNormal` | `Vec voption` |
| `Polyline3D.tryAverageNormal` | `pl: Polyline3D -> Vec voption` |
| `.TryAveragePlane` | `?tolerance: float -> NPlane voption` |
| `Polyline3D.tryAveragePlane` | `tolerance: float -> pl: Polyline3D -> NPlane voption` |
| `.TryPlane` | `?tolerance: float -> NPlane voption` |
| `Polyline3D.tryPlane` | `tolerance: float -> pl: Polyline3D -> NPlane voption` |
| `.XYZs` | `ResizeArray<float>` |
| `Polyline3D.xyzs` | `p: Polyline3D -> ResizeArray<float>` |
| **Other static members** | |
| `Polyline3D.createEmpty` | `capacity: int -> Polyline3D` |
| `Polyline3D.createFromMembersXY` | `xyObjs: ^T seq -> Polyline3D` |
| `Polyline3D.createFromMembersxy` | `xyObjs: ^T seq -> Polyline3D` |
| `Polyline3D.createFromMembersXYZ` | `xyzObjs: ^T seq -> Polyline3D` |
| `Polyline3D.createFromMembersxyz` | `xyzObjs: ^T seq -> Polyline3D` |
| `Polyline3D.createFromPnts` | `points: Pnt seq -> Polyline3D` |
| `Polyline3D.createFromRect3D` | `r: Rect3D -> Polyline3D` |
| `Polyline3D.createFromXYZs` | `xyzs: ResizeArray<float> -> Polyline3D` |
| `Polyline3D.end'` | `pl: Polyline3D -> Pnt` |
| `Polyline3D.equalsExact` | `pl1: Polyline3D -> pl2: Polyline3D -> bool` |
| `Polyline3D.equalsWithin` | `tol: float -> pl1: Polyline3D -> pl2: Polyline3D -> bool` |
| `Polyline3D.findSelfIntersection` | `pl: Polyline3D -> Pnt * int * int` |
| `Polyline3D.findSelfIntersectionCustom` | `pl: Polyline3D * ?maxSkewDistance: float -> Pnt * int * int` |
| `Polyline3D.iter` | `action: (float -> float -> float -> unit) -> pl: Polyline3D -> unit` |
| `Polyline3D.iteri` | `action: (int -> float -> float -> float -> unit) -> pl: Polyline3D -> unit` |
| `Polyline3D.iteriPnt` | `action: (int -> Pnt -> unit) -> pl: Polyline3D -> unit` |
| `Polyline3D.iterLineSegments` | `action: (Line3D -> unit) -> pl: Polyline3D -> unit` |
| `Polyline3D.iterPnt` | `action: (Pnt -> unit) -> pl: Polyline3D -> unit` |
| `Polyline3D.iterSegments` | `action: (float -> float -> float -> float -> float -> float -> unit) -> pl: Polyline3D -> unit` |
| `Polyline3D.map` | `mapping: (float -> float -> float -> Pnt) -> pl: Polyline3D -> Polyline3D` |
| `Polyline3D.mapi` | `mapping: (int -> float -> float -> float -> Pnt) -> pl: Polyline3D -> Polyline3D` |
| `Polyline3D.mapiPnt` | `mapping: (int -> Pnt -> Pnt) -> pl: Polyline3D -> Polyline3D` |
| `Polyline3D.mapPnt` | `mapping: (Pnt -> Pnt) -> pl: Polyline3D -> Polyline3D` |
| `Polyline3D.offset` | `inPlaneOffsetDistance: float -> perpendicularOffsetDistance: float -> polyLine: Polyline3D -> Polyline3D` |
| `Polyline3D.offsetByNormal` | `refNormal: UnitVec -> inPlaneOffsetDistance: float -> perpendicularOffsetDistance: float -> polyLine: Polyline3D -> Polyline3D` |
| `Polyline3D.offsetByNormalCustom` | `polyLine: Polyline3D * inPlaneOffsetDistance: float * perpendicularOffsetDistance: float * refNormal: UnitVec * ?loop: bool -> Polyline3D` |
| `Polyline3D.offsetCustom` | `polyLine: Polyline3D * inPlaneOffsetDistance: float * perpendicularOffsetDistance: float * ?loop: bool -> Polyline3D` |
| `Polyline3D.offsetVariable` | `inPlaneOffsetDistances: ResizeArray<float> -> perpendicularOffsetDistances: ResizeArray<float> -> polyLine: Polyline3D -> Polyline3D` |
| `Polyline3D.offsetVariableByNormal` | `refNormal: UnitVec -> inPlaneOffsetDistances: IList<float> -> perpendicularOffsetDistances: IList<float> -> polyLine: Polyline3D -> Polyline3D` |
| `Polyline3D.offsetVariableByNormalCustom` | `polyLine: Polyline3D * inPlaneOffsetDistances: IList<float> * perpendicularOffsetDistances: IList<float> * refNormal: UnitVec * ?loop: bool * ?varDistParallelBehavior: VarDistParallel * ?considerCollinearBelow: float<cosine> * ?failAtUTurnAbove: float<cosine> -> Polyline3D` |
| `Polyline3D.offsetVariableCustom` | `polyLine: Polyline3D * inPlaneOffsetDistances: ResizeArray<float> * perpendicularOffsetDistances: ResizeArray<float> * ?loop: bool * ?varDistParallelBehavior: VarDistParallel * ?considerCollinearBelow: float<cosine> * ?failAtUTurnAbove: float<cosine> -> Polyline3D` |
| `Polyline3D.subPolyline` | `a: float -> b: float -> pl: Polyline3D -> Polyline3D` |
| `Polyline3D.tryFind` | `condition: (float -> float -> float -> bool) -> pl: Polyline3D -> Pnt voption` |
| `Polyline3D.tryFindIndex` | `condition: (float -> float -> float -> bool) -> pl: Polyline3D -> int voption` |
| `Polyline3D.tryFindLast` | `condition: (float -> float -> float -> bool) -> pl: Polyline3D -> Pnt voption` |
| `Polyline3D.tryFindLastIndex` | `condition: (float -> float -> float -> bool) -> pl: Polyline3D -> int voption` |
| `Polyline3D.tryFindSelfIntersection` | `pl: Polyline3D -> (Pnt * int * int) voption` |
| `Polyline3D.tryFindSelfIntersectionCustom` | `pl: Polyline3D * ?maxSkewDistance: float -> (Pnt * int * int) voption` |

## Topology3D

| Topology3D | Signature |
|:--|:--|
| `Topology3D.join` | `getLine: ('T -> Line3D) -> splitDistance: float -> xs: ResizeArray<'T> -> ResizeArray<ResizeArray<'T>>` |
| `Topology3D.joinReversing` | `getLine: ('T -> Line3D) -> splitDistance: float -> xs: ResizeArray<'T> -> ResizeArray<ResizeArray<'T * bool>>` |
| `Topology3D.sortToLoop` | `getLine: ('T -> Line3D) -> ts: ResizeArray<'T> -> unit` |
| `Topology3D.sortToLoopWithReversing` | `getLine: ('T -> Line3D) -> reverseInPlace: (int -> 'T -> unit) -> ts: ResizeArray<'T> -> unit` |
