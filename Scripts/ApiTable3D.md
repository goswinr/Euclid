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
- [XLine3D.ClPts](#xline3dclpts)
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
| `Vec.dot` | `a: Vec * b: Vec -> float` |
| `Vec.dot` | `a: Vec * b: UnitVec -> float` |
| `Vec.dot` | `a: UnitVec * b: Vec -> float` |
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
| `.LengthSq` | `float` |
| `Vec.lengthSq` | `v: Vec -> float` |
| `.LengthSqInXY` | `float` |
| `Vec.lengthSqInXY` | `v: Vec -> float` |
| `.MatchesOrientation` | `other: Vec -> bool` |
| `.MatchesOrientation` | `other: UnitVec -> bool` |
| `Vec.matchesOrientation` | `v: Vec -> other: Vec -> bool` |
| `.PerpendicularInXY` | `Vec` |
| `Vec.perpendicularInXY` | `v: Vec -> Vec` |
| `.RotateByQuarterCircle` | `numberOfQuarters: int -> Vec` |
| `Vec.rotateByQuarterCircle` | `numberOfQuarters: int -> v: Vec -> Vec` |
| `.RotateOnZ90CCW` | `Vec` |
| `Vec.rotateOnZ90CCW` | `v: Vec -> Vec` |
| `.RotateOnZ90CW` | `Vec` |
| `Vec.rotateOnZ90CW` | `v: Vec -> Vec` |
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
| `Vec.angle2PiInXY` | `a: Vec * b: Vec -> float` |
| `Vec.angle360InXY` | `a: Vec * b: Vec -> float` |
| `Vec.angle90` | `a: Vec -> b: Vec -> float` |
| `Vec.angleDiamondInXY` | `a: Vec * b: Vec -> float` |
| `Vec.angleHalfPi` | `a: Vec -> b: Vec -> float` |
| `Vec.anglePi` | `a: Vec -> b: Vec -> float` |
| `Vec.bisector` | `a: Vec -> b: Vec -> Vec` |
| `Vec.create` | `x: float * y: float * z: float -> Vec` |
| `Vec.create` | `start: Pnt * endValue: Pnt -> Vec` |
| `Vec.createFromMembersXYZ` | `vec: ^T -> Vec` |
| `Vec.createFromMembersxyz` | `vec: ^T -> Vec` |
| `Vec.createFromPnt` | `pt: Pnt -> Vec` |
| `Vec.createFromUnitVec` | `v: UnitVec -> Vec` |
| `Vec.createFromVcWithZ` | `z: float -> v: Vc -> Vec` |
| `Vec.determinant3` | `u: Vec * v: Vec * w: Vec -> float` |
| `Vec.difference` | `a: Vec -> b: Vec -> float` |
| `Vec.differenceSq` | `a: Vec -> b: Vec -> float` |
| `Vec.DivideByInt` | `v: Vec * i: int -> Vec` |
| `Vec.equals` | `tol: float -> a: Vec -> b: Vec -> bool` |
| `Vec.flip` | `v: Vec -> Vec` |
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
| `Vec.lerp` | `start: Vec * endValue: Vec * rel: float -> Vec` |
| `Vec.matchOrientation` | `orientationToMatch: Vec -> vecToFlip: Vec -> Vec` |
| `Vec.matchUnitVecOrientation` | `orientationToMatch: UnitVec -> vecToFlip: Vec -> Vec` |
| `Vec.notEquals` | `tol: float -> a: Vec -> b: Vec -> bool` |
| `Vec.orientDown` | `v: Vec -> Vec` |
| `Vec.orientUp` | `v: Vec -> Vec` |
| `Vec.perpendicularInVerticalPlane` | `v: Vec -> Vec` |
| `Vec.projectToXYPlane` | `v: Vec -> Vec` |
| `Vec.reverse` | `v: Vec -> Vec` |
| `Vec.rotate` | `q: Quaternion -> pt: Vec -> Vec` |
| `Vec.rotateOnX` | `r: Rotation2D -> v: Vec -> Vec` |
| `Vec.rotateOnXDeg` | `angDegree: float -> v: Vec -> Vec` |
| `Vec.rotateOnY` | `r: Rotation2D -> v: Vec -> Vec` |
| `Vec.rotateOnYDeg` | `angDegree: float -> v: Vec -> Vec` |
| `Vec.rotateOnZ` | `r: Rotation2D -> v: Vec -> Vec` |
| `Vec.rotateOnZDeg` | `angDegree: float -> v: Vec -> Vec` |
| `Vec.scale` | `f: float -> v: Vec -> Vec` |
| `Vec.slerp` | `start: Vec * endValue: Vec * rel: float -> Vec` |
| `Vec.slopeDegrees` | `v: Vec -> float` |
| `Vec.slopePercent` | `v: Vec -> float` |
| `Vec.slopeRadians` | `v: Vec -> float` |
| `Vec.unitize` | `v: Vec -> UnitVec` |
| `Vec.unitizeAsVec` | `v: Vec -> Vec` |
| `Vec.unitizeOrDefault` | `defaultUnitVector: UnitVec -> v: Vec -> UnitVec` |
| `Vec.unitizeUnchecked` | `v: Vec -> UnitVec` |
| `Vec.unitizeWithErrMsg` | `errMsg: string -> v: Vec -> UnitVec` |
| `Vec.Xaxis` | `Vec` |
| `Vec.Yaxis` | `Vec` |
| `Vec.Zaxis` | `Vec` |
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
| `UnitVec.dot` | `a: UnitVec * b: UnitVec -> float` |
| `UnitVec.dot` | `a: UnitVec * b: Vec -> float` |
| `UnitVec.dot` | `a: Vec * b: UnitVec -> float` |
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
| `.LengthSqInXY` | `float` |
| `UnitVec.lengthSqInXY` | `v: UnitVec -> float` |
| `.MatchesOrientation` | `other: UnitVec -> bool` |
| `.MatchesOrientation` | `other: Vec -> bool` |
| `UnitVec.matchesOrientation` | `other: UnitVec -> v: UnitVec -> bool` |
| `.PerpendicularInXY` | `Vec` |
| `UnitVec.perpendicularInXY` | `v: UnitVec -> Vec` |
| `.RotateByQuarterCircle` | `numberOfQuarters: int -> UnitVec` |
| `UnitVec.rotateByQuarterCircle` | `numberOfQuarters: int -> v: UnitVec -> UnitVec` |
| `.RotateOnZ90CCW` | `UnitVec` |
| `UnitVec.rotateOnZ90CCW` | `v: UnitVec -> UnitVec` |
| `.RotateOnZ90CW` | `UnitVec` |
| `UnitVec.rotateOnZ90CW` | `v: UnitVec -> UnitVec` |
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
| `UnitVec.angle2PiInXY` | `a: UnitVec * b: UnitVec -> float` |
| `UnitVec.angle360InXY` | `a: UnitVec * b: UnitVec -> float` |
| `UnitVec.angle90` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVec.angleDiamondInXY` | `a: UnitVec * b: UnitVec -> float` |
| `UnitVec.angleHalfPi` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVec.anglePi` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVec.create` | `x: float * y: float * z: float -> UnitVec` |
| `UnitVec.create` | `fromPt: Pnt * toPt: Pnt -> UnitVec` |
| `UnitVec.createFor` | `msg: string * x: float * y: float * z: float -> UnitVec` |
| `UnitVec.createFromMembersXYZ` | `vec: ^T -> UnitVec` |
| `UnitVec.createFromMembersxyz` | `vec: ^T -> UnitVec` |
| `UnitVec.createFromPnt` | `pt: Pnt -> UnitVec` |
| `UnitVec.createFromVec` | `v: Vec -> UnitVec` |
| `UnitVec.createUnchecked` | `x: float * y: float * z: float -> UnitVec` |
| `UnitVec.determinant3` | `u: UnitVec * v: UnitVec * w: UnitVec -> float` |
| `UnitVec.difference` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVec.differenceSq` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVec.equals` | `tol: float -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVec.flip` | `v: UnitVec -> UnitVec` |
| `UnitVec.flipToPointUp` | `v: UnitVec -> UnitVec` |
| `UnitVec.getX` | `v: UnitVec -> float` |
| `UnitVec.getY` | `v: UnitVec -> float` |
| `UnitVec.getZ` | `v: UnitVec -> float` |
| `UnitVec.isAngleAbove` | `cosineValue: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVec.isAngleBelow` | `cosineValue: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVec.isNotParallelWithin` | `cosineValue: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVec.isParallelWithin` | `cosineValue: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVec.lerp` | `start: UnitVec * endValue: UnitVec * rel: float -> Vec` |
| `UnitVec.matchOrientation` | `orientationToMatch: UnitVec -> vecToFlip: UnitVec -> UnitVec` |
| `UnitVec.matchVecOrientation` | `orientationToMatch: Vec -> vecToFlip: UnitVec -> UnitVec` |
| `UnitVec.notEquals` | `tol: float -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVec.orientDown` | `v: UnitVec -> UnitVec` |
| `UnitVec.orientUp` | `v: UnitVec -> UnitVec` |
| `UnitVec.perpendicularInVerticalPlane` | `v: UnitVec -> Vec` |
| `UnitVec.projectToXYPlane` | `v: UnitVec -> Vec` |
| `UnitVec.reverse` | `v: UnitVec -> UnitVec` |
| `UnitVec.rotate` | `q: Quaternion -> pt: UnitVec -> UnitVec` |
| `UnitVec.rotateOnX` | `r: Rotation2D -> v: UnitVec -> UnitVec` |
| `UnitVec.rotateOnXDeg` | `angDegree: float -> v: UnitVec -> UnitVec` |
| `UnitVec.rotateOnY` | `r: Rotation2D -> v: UnitVec -> UnitVec` |
| `UnitVec.rotateOnYDeg` | `angDegree: float -> v: UnitVec -> UnitVec` |
| `UnitVec.rotateOnZ` | `r: Rotation2D -> v: UnitVec -> UnitVec` |
| `UnitVec.rotateOnZDeg` | `angDegree: float -> v: UnitVec -> UnitVec` |
| `UnitVec.scale` | `f: float -> v: UnitVec -> Vec` |
| `UnitVec.slerp` | `start: UnitVec * endValue: UnitVec * rel: float -> UnitVec` |
| `UnitVec.slopeDegrees` | `v: UnitVec -> float` |
| `UnitVec.slopePercent` | `v: UnitVec -> float` |
| `UnitVec.slopeRadians` | `v: UnitVec -> float` |
| `UnitVec.withLength` | `f: float -> v: UnitVec -> Vec` |
| `UnitVec.Xaxis` | `UnitVec` |
| `UnitVec.Yaxis` | `UnitVec` |
| `UnitVec.Zaxis` | `UnitVec` |
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
| `Pnt.angle2PiInXYTo` | `fromPt: Pnt * toPt: Pnt -> float` |
| `.Angle360InXYTo` | `o: Pnt -> float` |
| `Pnt.angle360InXYTo` | `fromPt: Pnt * toPt: Pnt -> float` |
| `.AsFSharpCode` | `string` |
| `.AsPt` | `Pt` |
| `Pnt.asPt` | `pt: Pnt -> Pt` |
| `.AsString` | `string` |
| `.AsVc` | `Vc` |
| `Pnt.asVc` | `pt: Pnt -> Vc` |
| `.AsVec` | `Vec` |
| `Pnt.asVec` | `pt: Pnt -> Vec` |
| `.ClosestPointOnLine` | `ln: Line3D -> Pnt` |
| `Pnt.closestPointOnLine` | `ln: Line3D -> testPt: Pnt -> Pnt` |
| `.DirectionDiamondInXYTo` | `o: Pnt -> float` |
| `Pnt.directionDiamondInXYTo` | `fromPt: Pnt * toPt: Pnt -> float` |
| `.DistanceFromOrigin` | `float` |
| `Pnt.distanceFromOrigin` | `pt: Pnt -> float` |
| `.DistanceInXYFromOrigin` | `float` |
| `Pnt.distanceInXYFromOrigin` | `pt: Pnt -> float` |
| `.DistanceTo` | `b: Pnt -> float` |
| `Pnt.distanceTo` | `b: Pnt -> p: Pnt -> float` |
| `.DistanceToLine` | `ln: Line3D -> float` |
| `Pnt.distanceToLine` | `ln: Line3D -> testPt: Pnt -> float` |
| `.DistanceToXYZ` | `x: float * y: float * z: float -> float` |
| `Pnt.distanceToXYZ` | `x: float * y: float * z: float -> p: Pnt -> float` |
| `.IsAlmostOrigin` | `tol: float -> bool` |
| `Pnt.isAlmostOrigin` | `tol: float -> pt: Pnt -> bool` |
| `.IsInvalid` | `bool` |
| `Pnt.isInvalid` | `pt: Pnt -> bool` |
| `.IsNotOrigin` | `bool` |
| `Pnt.isNotOrigin` | `pt: Pnt -> bool` |
| `.IsOrigin` | `bool` |
| `Pnt.isOrigin` | `pt: Pnt -> bool` |
| `.IsValid` | `bool` |
| `Pnt.isValid` | `pt: Pnt -> bool` |
| `.SqDistanceFromOrigin` | `float` |
| `Pnt.sqDistanceFromOrigin` | `pt: Pnt -> float` |
| `.SqDistanceInXYFromOrigin` | `float` |
| `Pnt.sqDistanceInXYFromOrigin` | `pt: Pnt -> float` |
| `.SqDistanceTo` | `b: Pnt -> float` |
| `Pnt.sqDistanceTo` | `b: Pnt -> p: Pnt -> float` |
| `.SqDistanceToLine` | `ln: Line3D -> float` |
| `Pnt.sqDistanceToLine` | `ln: Line3D -> testPt: Pnt -> float` |
| `.SqDistanceToXYZ` | `x: float * y: float * z: float -> float` |
| `Pnt.sqDistanceToXYZ` | `x: float * y: float * z: float -> p: Pnt -> float` |
| `.ToString` | `unit -> string` |
| `.Transform` | `m: Matrix -> Pnt` |
| `Pnt.transform` | `m: Matrix -> p: Pnt -> Pnt` |
| `.TransformRigid` | `m: RigidMatrix -> Pnt` |
| `Pnt.transformRigid` | `m: RigidMatrix -> p: Pnt -> Pnt` |
| `.TransformRigidRotateOnly` | `m: RigidMatrix -> Pnt` |
| `Pnt.transformRigidRotateOnly` | `m: RigidMatrix -> p: Pnt -> Pnt` |
| `.WithDistanceFromOrigin` | `l: float -> Pnt` |
| `Pnt.withDistanceFromOrigin` | `l: float -> pt: Pnt -> Pnt` |
| `.WithX` | `x: float -> Pnt` |
| `Pnt.withX` | `x: float -> pt: Pnt -> Pnt` |
| `.WithY` | `y: float -> Pnt` |
| `Pnt.withY` | `y: float -> pt: Pnt -> Pnt` |
| `.WithZ` | `z: float -> Pnt` |
| `Pnt.withZ` | `z: float -> pt: Pnt -> Pnt` |
| **Other static members** | |
| `Pnt.add` | `a: Pnt -> b: Pnt -> Pnt` |
| `Pnt.addVec` | `v: Vec -> a: Pnt -> Pnt` |
| `Pnt.angle180Pts` | `ptPrev: Pnt * ptThis: Pnt * ptNext: Pnt -> float` |
| `Pnt.angleInCorner` | `prevPt: Pnt * thisPt: Pnt * nextPt: Pnt -> float` |
| `Pnt.anglePiPts` | `ptPrev: Pnt * ptThis: Pnt * ptNext: Pnt -> float` |
| `Pnt.bisector` | `ptPrev: Pnt * ptThis: Pnt * ptNext: Pnt -> Vec` |
| `Pnt.closestOfTwo` | `pt1: Pnt -> pt2: Pnt -> referencePoint: Pnt -> Pnt` |
| `Pnt.createFromMembersXYZ` | `pt: ^T -> Pnt` |
| `Pnt.createFromMembersxyz` | `pt: ^T -> Pnt` |
| `Pnt.createFromPt` | `p: Pt -> Pnt` |
| `Pnt.createFromPtWithZ` | `z: float -> p: Pt -> Pnt` |
| `Pnt.createFromUnitVec` | `v: UnitVec -> Pnt` |
| `Pnt.createFromVec` | `v: Vec -> Pnt` |
| `Pnt.dist` | `b: Pnt -> p: Pnt -> float` |
| `Pnt.distanceInXY` | `a: Pnt -> b: Pnt -> float` |
| `Pnt.distPt` | `fromPt: Pnt * dirPt: Pnt * distance: float -> Pnt` |
| `Pnt.DivideByInt` | `pt: Pnt * i: int -> Pnt` |
| `Pnt.divPt` | `fromPt: Pnt * toPt: Pnt * rel: float -> Pnt` |
| `Pnt.equals` | `tol: float -> a: Pnt -> b: Pnt -> bool` |
| `Pnt.extendToZLevel` | `fromPt: Pnt * toPt: Pnt * z: float -> Pnt` |
| `Pnt.getX` | `pt: Pnt -> float` |
| `Pnt.getY` | `pt: Pnt -> float` |
| `Pnt.getZ` | `pt: Pnt -> float` |
| `Pnt.lerp` | `fromPt: Pnt * toPt: Pnt * rel: float -> Pnt` |
| `Pnt.midPt` | `a: Pnt -> b: Pnt -> Pnt` |
| `Pnt.move` | `shift: Vec -> pt: Pnt -> Pnt` |
| `Pnt.moveX` | `x: float -> pt: Pnt -> Pnt` |
| `Pnt.moveY` | `y: float -> pt: Pnt -> Pnt` |
| `Pnt.moveZ` | `z: float -> pt: Pnt -> Pnt` |
| `Pnt.normalOf3Pts` | `a: Pnt * b: Pnt * c: Pnt -> Vec` |
| `Pnt.normalOfTwoPointsInXY` | `fromPt: Pnt * toPt: Pnt -> Vec` |
| `Pnt.notEquals` | `tol: float -> a: Pnt -> b: Pnt -> bool` |
| `Pnt.Origin` | `Pnt` |
| `Pnt.projectedParameter` | `fromPt: Pnt * uv: UnitVec * testPt: Pnt -> float` |
| `Pnt.projectedParameter` | `fromPt: Pnt * v: Vec * testPt: Pnt -> float` |
| `Pnt.projectedParameter` | `fromPt: Pnt * toPt: Pnt * testPt: Pnt -> float` |
| `Pnt.projectToXYPlane` | `pt: Pnt -> Pnt` |
| `Pnt.rotate` | `q: Quaternion -> pt: Pnt -> Pnt` |
| `Pnt.rotateOnX` | `r: Rotation2D -> p: Pnt -> Pnt` |
| `Pnt.rotateOnXDeg` | `angDegree: float -> pt: Pnt -> Pnt` |
| `Pnt.rotateOnXWithCenter` | `cen: Pnt -> r: Rotation2D -> pt: Pnt -> Pnt` |
| `Pnt.rotateOnXWithCenterDeg` | `cen: Pnt -> angDegree: float -> pt: Pnt -> Pnt` |
| `Pnt.rotateOnY` | `r: Rotation2D -> p: Pnt -> Pnt` |
| `Pnt.rotateOnYDeg` | `angDegree: float -> pt: Pnt -> Pnt` |
| `Pnt.rotateOnYWithCenter` | `cen: Pnt -> r: Rotation2D -> pt: Pnt -> Pnt` |
| `Pnt.rotateOnYWithCenterDeg` | `cen: Pnt -> angDegree: float -> pt: Pnt -> Pnt` |
| `Pnt.rotateOnZ` | `r: Rotation2D -> p: Pnt -> Pnt` |
| `Pnt.rotateOnZDeg` | `angDegree: float -> pt: Pnt -> Pnt` |
| `Pnt.rotateOnZWithCenter` | `cen: Pnt -> r: Rotation2D -> pt: Pnt -> Pnt` |
| `Pnt.rotateOnZWithCenterDeg` | `cen: Pnt -> angDegree: float -> pt: Pnt -> Pnt` |
| `Pnt.rotateWithCenter` | `cen: Pnt -> q: Quaternion -> pt: Pnt -> Pnt` |
| `Pnt.scale` | `f: float -> pt: Pnt -> Pnt` |
| `Pnt.snap` | `precision: float -> pt: Pnt -> Pnt` |
| `Pnt.snapIfClose` | `snapDistance: float -> snapTo: Pnt -> pt: Pnt -> Pnt` |
| `Pnt.sqDist` | `b: Pnt -> p: Pnt -> float` |
| `Pnt.translate` | `shift: Vec -> pt: Pnt -> Pnt` |
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
| `.Inverse` | `Quaternion` |
| `Quaternion.inverse` | `q: Quaternion -> Quaternion` |
| `.Normalize` | `unit -> Quaternion` |
| `Quaternion.normalize` | `q: Quaternion -> Quaternion` |
| `.ToString` | `unit -> string` |
| `.WithAngleInDegrees` | `angleInDegrees: float -> Quaternion` |
| `Quaternion.withAngleInDegrees` | `angleInDegrees: float -> q: Quaternion -> Quaternion` |
| `.WithAngleInRadians` | `angleInRadians: float -> Quaternion` |
| `Quaternion.withAngleInRadians` | `angleInRadians: float -> q: Quaternion -> Quaternion` |
| **Other static members** | |
| `Quaternion.create` | `x: float * y: float * z: float * w: float -> Quaternion` |
| `Quaternion.createFromDegrees` | `axis: Vec * angleInDegrees: float -> Quaternion` |
| `Quaternion.createFromDegrees` | `axis: UnitVec * angleInDegrees: float -> Quaternion` |
| `Quaternion.createFromEulerXYZ` | `degreesX: float * degreesY: float * degreesZ: float -> Quaternion` |
| `Quaternion.createFromEulerXZY` | `degreesX: float * degreesZ: float * degreesY: float -> Quaternion` |
| `Quaternion.createFromEulerYXZ` | `degreesY: float * degreesX: float * degreesZ: float -> Quaternion` |
| `Quaternion.createFromEulerYZX` | `degreesY: float * degreesZ: float * degreesX: float -> Quaternion` |
| `Quaternion.createFromEulerZXY` | `degreesZ: float * degreesX: float * degreesY: float -> Quaternion` |
| `Quaternion.createFromEulerZYX` | `degreesZ: float * degreesY: float * degreesX: float -> Quaternion` |
| `Quaternion.createFromRadians` | `axis: Vec * angleInRadians: float -> Quaternion` |
| `Quaternion.createFromRadians` | `axis: UnitVec * angleInRadians: float -> Quaternion` |
| `Quaternion.createFromRigidMatrix` | `orthoMatrix: RigidMatrix -> Quaternion` |
| `Quaternion.createUnchecked` | `x: float * y: float * z: float * w: float -> Quaternion` |
| `Quaternion.createVecToVec` | `vecFrom: UnitVec * vecTo: UnitVec -> Quaternion` |
| `Quaternion.createVecToVec` | `vecFrom: Vec * vecTo: Vec -> Quaternion` |
| `Quaternion.equals` | `tolerance: float -> a: Quaternion -> b: Quaternion -> bool` |
| `Quaternion.equalsRotation` | `tolerance: float -> a: Quaternion -> b: Quaternion -> bool` |
| `Quaternion.identity` | `Quaternion` |
| `Quaternion.multiply` | `l: Quaternion * r: Quaternion -> Quaternion` |
| `Quaternion.toEulerAnglesZYX` | `q: Quaternion -> float * float * float` |
| `Quaternion.tryCreateFromMatrix` | `matrix: Matrix -> Quaternion option` |
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
| `.XaxisX` | `float` |
| `.XaxisY` | `float` |
| `.XaxisZ` | `float` |
| `.YaxisX` | `float` |
| `.YaxisY` | `float` |
| `.YaxisZ` | `float` |
| `.ZaxisX` | `float` |
| `.ZaxisY` | `float` |
| `.ZaxisZ` | `float` |
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
| `.ClosestPoint` | `pt: Pnt -> Pnt` |
| `PPlane.closestPoint` | `pt: Pnt -> pl: PPlane -> Pnt` |
| `.DistanceToPnt` | `pt: Pnt -> float` |
| `PPlane.distanceToPnt` | `pt: Pnt -> pl: PPlane -> float` |
| `.DistanceToPntSigned` | `pt: Pnt -> float` |
| `PPlane.distanceToPntSigned` | `pt: Pnt -> pl: PPlane -> float` |
| `.DistanceToXYZ` | `x: float * y: float * z: float -> float` |
| `PPlane.distanceToXYZ` | `x: float -> y: float -> z: float -> pl: PPlane -> float` |
| `.DistanceToXYZSigned` | `x: float * y: float * z: float -> float` |
| `PPlane.distanceToXYZSigned` | `x: float -> y: float -> z: float -> pl: PPlane -> float` |
| `.EvaluateAt` | `px: float * py: float -> Pnt` |
| `PPlane.evaluateAt` | `px: float * py: float -> pl: PPlane -> Pnt` |
| `.EvaluateAtXYZ` | `px: float * py: float * pz: float -> Pnt` |
| `PPlane.evaluateAtXYZ` | `px: float * py: float * pz: float -> pl: PPlane -> Pnt` |
| `.IsCoincidentTo` | `other: PPlane * distanceTolerance: float * minCosine: float<cosine> -> bool` |
| `PPlane.isCoincidentTo` | `distanceTolerance: float -> minCosine: float<cosine> -> a: PPlane -> b: PPlane -> bool` |
| `.Origin` | `Pnt` |
| `.PlaneAtClPnt` | `pt: Pnt -> PPlane` |
| `PPlane.planeAtClPnt` | `pt: Pnt -> pl: PPlane -> PPlane` |
| `.PointParameters` | `pt: Pnt -> float * float * float` |
| `PPlane.pointParameters` | `pt: Pnt -> pl: PPlane -> float * float * float` |
| `.ToString` | `unit -> string` |
| `.Xaxis` | `UnitVec` |
| `.Yaxis` | `UnitVec` |
| `.Zaxis` | `UnitVec` |
| **Other static members** | |
| `PPlane.angle90ToUnitVec` | `v: UnitVec -> pl: PPlane -> float` |
| `PPlane.create` | `origin: Pnt * axisX: UnitVec * axisY: UnitVec * axisZ: UnitVec -> PPlane` |
| `PPlane.createOriginNormal` | `origin: Pnt * normal: UnitVec -> PPlane` |
| `PPlane.createOriginNormal` | `origin: Pnt * normal: Vec -> PPlane` |
| `PPlane.createOriginNormalXaxis` | `origin: Pnt * normal: UnitVec * xAxis: UnitVec -> PPlane` |
| `PPlane.createOriginNormalXaxis` | `origin: Pnt * normal: Vec * xAxis: Vec -> PPlane` |
| `PPlane.createOriginXaxisYaxis` | `origin: Pnt * xAxis: UnitVec * yAxis: UnitVec -> PPlane` |
| `PPlane.createOriginXaxisYaxis` | `origin: Pnt * xAxis: Vec * yAxis: Vec -> PPlane` |
| `PPlane.createThreePoints` | `origin: Pnt -> xPt: Pnt -> yPt: Pnt -> PPlane` |
| `PPlane.createUnchecked` | `originX: float * originY: float * originZ: float * xAxisX: float * xAxisY: float * xAxisZ: float * yAxisX: float * yAxisY: float * yAxisZ: float * zAxisX: float * zAxisY: float * zAxisZ: float -> PPlane` |
| `PPlane.createUncheckedVec` | `origin: Pnt * axisX: UnitVec * axisY: UnitVec * axisZ: UnitVec -> PPlane` |
| `PPlane.doLinePlaneIntersect` | `ln: Line3D -> pl: PPlane -> bool` |
| `PPlane.equals` | `tol: float -> a: PPlane -> b: PPlane -> bool` |
| `PPlane.flipOnX` | `pl: PPlane -> PPlane` |
| `PPlane.flipOnY` | `pl: PPlane -> PPlane` |
| `PPlane.intersect` | `a: PPlane -> b: PPlane -> Line3D voption` |
| `PPlane.intersectLine` | `ln: Line3D -> pl: PPlane -> Pnt voption` |
| `PPlane.intersectRay` | `ln: Line3D -> pl: PPlane -> float voption` |
| `PPlane.intersectRayAllParameters` | `ln: Line3D -> pl: PPlane -> (float * float * float) voption` |
| `PPlane.move` | `translation: Vec -> pl: PPlane -> PPlane` |
| `PPlane.notEquals` | `tol: float -> a: PPlane -> b: PPlane -> bool` |
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
| `PPlane.translateLocalX` | `d: float -> pl: PPlane -> PPlane` |
| `PPlane.translateLocalY` | `d: float -> pl: PPlane -> PPlane` |
| `PPlane.translateLocalZ` | `d: float -> pl: PPlane -> PPlane` |
| `PPlane.withOrigin` | `pt: Pnt -> pl: PPlane -> PPlane` |
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
| `Matrix.createFromColumnMajorArray` | `xs: float array -> Matrix` |
| `Matrix.createFromQuaternion` | `quaternion: Quaternion -> Matrix` |
| `Matrix.createFromRigidMatrix` | `m: RigidMatrix -> Matrix` |
| `Matrix.createFromRowMajorArray` | `xs: float array -> Matrix` |
| `Matrix.createLookAt` | `eye: Pnt * target: Pnt * up: Vec -> Matrix` |
| `Matrix.createLookAtZUp` | `eye: Pnt * target: Pnt -> Matrix` |
| `Matrix.createMirror` | `p: PPlane -> Matrix` |
| `Matrix.createPerspectiveAlongNegZ` | `width: float * height: float * nearPlaneDistance: float * farPlaneDistance: float -> Matrix` |
| `Matrix.createPerspectiveAlongPosY` | `width: float * height: float * nearPlaneDistance: float * farPlaneDistance: float -> Matrix` |
| `Matrix.createPerspectiveLookAt` | `eye: Pnt * target: Pnt * up: Vec * width: float * height: float * nearPlaneDistance: float * farPlaneDistance: float -> Matrix` |
| `Matrix.createPerspectiveLookAtZUp` | `eye: Pnt * target: Pnt * width: float * height: float * nearPlaneDistance: float * farPlaneDistance: float -> Matrix` |
| `Matrix.createPlaneToPlane` | `fromPlane: PPlane * toPlane: PPlane -> Matrix` |
| `Matrix.createRotationAxis` | `axis: UnitVec * angleDegrees: float -> Matrix` |
| `Matrix.createRotationAxis` | `axis: Vec * angleDegrees: float -> Matrix` |
| `Matrix.createRotationAxisCenter` | `axis: Vec * cen: Pnt * angleDegrees: float -> Matrix` |
| `Matrix.createRotationAxisCenter` | `axis: UnitVec * cen: Pnt * angleDegrees: float -> Matrix` |
| `Matrix.createRotationX` | `angleDegrees: float -> Matrix` |
| `Matrix.createRotationY` | `angleDegrees: float -> Matrix` |
| `Matrix.createRotationZ` | `angleDegrees: float -> Matrix` |
| `Matrix.createScale` | `x: float * y: float * z: float -> Matrix` |
| `Matrix.createShear` | `xy: float * xz: float * yx: float * yz: float * zx: float * zy: float -> Matrix` |
| `Matrix.createToPlane` | `p: PPlane -> Matrix` |
| `Matrix.createTranslation` | `x: float * y: float * z: float -> Matrix` |
| `Matrix.createTranslation` | `v: Vec -> Matrix` |
| `Matrix.createVecToVec` | `vecFrom: UnitVec * vecTo: UnitVec -> Matrix` |
| `Matrix.createVecToVec` | `vecFrom: Vec * vecTo: Vec -> Matrix` |
| `Matrix.equals` | `tol: float -> a: Matrix -> b: Matrix -> bool` |
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
| `RigidMatrix.create` | `m11: float * m21: float * m31: float * x41: float * m12: float * m22: float * m32: float * y42: float * m13: float * m23: float * m33: float * z43: float -> RigidMatrix` |
| `RigidMatrix.createFromMatrix` | `m: Matrix -> RigidMatrix` |
| `RigidMatrix.createFromQuaternion` | `quaternion: Quaternion -> RigidMatrix` |
| `RigidMatrix.createPlaneToPlane` | `fromPlane: PPlane * toPlane: PPlane -> RigidMatrix` |
| `RigidMatrix.createRotationAxis` | `axis: UnitVec * angleDegrees: float -> RigidMatrix` |
| `RigidMatrix.createRotationAxis` | `axis: Vec * angleDegrees: float -> RigidMatrix` |
| `RigidMatrix.createRotationAxisCenter` | `axis: Vec * cen: Pnt * angleDegrees: float -> RigidMatrix` |
| `RigidMatrix.createRotationAxisCenter` | `axis: UnitVec * cen: Pnt * angleDegrees: float -> RigidMatrix` |
| `RigidMatrix.createRotationX` | `angleDegrees: float -> RigidMatrix` |
| `RigidMatrix.createRotationY` | `angleDegrees: float -> RigidMatrix` |
| `RigidMatrix.createRotationZ` | `angleDegrees: float -> RigidMatrix` |
| `RigidMatrix.createToPlane` | `p: PPlane -> RigidMatrix` |
| `RigidMatrix.createTranslation` | `x: float * y: float * z: float -> RigidMatrix` |
| `RigidMatrix.createTranslation` | `v: Vec -> RigidMatrix` |
| `RigidMatrix.createTranslationX` | `x: float -> RigidMatrix` |
| `RigidMatrix.createTranslationY` | `y: float -> RigidMatrix` |
| `RigidMatrix.createTranslationZ` | `z: float -> RigidMatrix` |
| `RigidMatrix.createVecToVec` | `vecFrom: UnitVec * vecTo: UnitVec -> RigidMatrix` |
| `RigidMatrix.createVecToVec` | `vecFrom: Vec * vecTo: Vec -> RigidMatrix` |
| `RigidMatrix.equals` | `tol: float -> a: RigidMatrix -> b: RigidMatrix -> bool` |
| `RigidMatrix.identity` | `RigidMatrix` |
| `RigidMatrix.multiply` | `matrixA: RigidMatrix * matrixB: RigidMatrix -> RigidMatrix` |
| `RigidMatrix.removeTranslation` | `m: RigidMatrix -> RigidMatrix` |
| `RigidMatrix.transformPntXYZ` | `x: float -> y: float -> z: float -> m: RigidMatrix -> Pnt` |
| `RigidMatrix.transformVecXYZ` | `x: float -> y: float -> z: float -> m: RigidMatrix -> Vec` |
| `RigidMatrix.tryCreateFromMatrix` | `m: Matrix -> RigidMatrix option` |
| `( *** )` | `matrixA: RigidMatrix * matrixB: RigidMatrix -> RigidMatrix` |
| `( *** )` | `v: Vec * m: RigidMatrix -> Vec` |
| `( *** )` | `v: UnitVec * m: RigidMatrix -> UnitVec` |
| `( *** )` | `v: Pnt * m: RigidMatrix -> Pnt` |

## Line3D

| Line3D | Signature |
|:--|:--|
| `new` | `fromPt: Pnt * toPt: Pnt -> Line3D` |
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
| `.DistanceRayXYZ` | `x: float * y: float * z: float -> float` |
| `Line3D.distanceRayXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
| `.DistanceToPnt` | `p: Pnt -> float` |
| `Line3D.distanceToPnt` | `p: Pnt -> ln: Line3D -> float` |
| `.DistanceToXYZ` | `x: float * y: float * z: float -> float` |
| `Line3D.distanceToXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
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
| `.RayClosestParameters` | `lnB: Line3D -> (float * float) option` |
| `Line3D.rayClosestParameters` | `lnA: Line3D -> lnB: Line3D -> (float * float) option` |
| `.RayClosestParameterXYZ` | `x: float * y: float * z: float -> float` |
| `Line3D.rayClosestParameterXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
| `.RayClosestPoint` | `p: Pnt -> Pnt` |
| `Line3D.rayClosestPoint` | `p: Pnt -> ln: Line3D -> Pnt` |
| `.RayClosestPoints` | `lnB: Line3D -> (Pnt * Pnt) option` |
| `Line3D.rayClosestPoints` | `lnA: Line3D -> lnB: Line3D -> (Pnt * Pnt) option` |
| `.Reversed` | `Line3D` |
| `Line3D.reversed` | `ln: Line3D -> Line3D` |
| `.Rotate` | `q: Quaternion -> Line3D` |
| `Line3D.rotate` | `q: Quaternion -> ln: Line3D -> Line3D` |
| `.RotateWithCenter` | `cen: Pnt * q: Quaternion -> Line3D` |
| `Line3D.rotateWithCenter` | `cen: Pnt -> q: Quaternion -> ln: Line3D -> Line3D` |
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
| `.SqDistanceRayPoint` | `p: Pnt -> float` |
| `Line3D.sqDistanceRayPoint` | `p: Pnt -> ln: Line3D -> float` |
| `.SqDistanceRayXYZ` | `x: float * y: float * z: float -> float` |
| `Line3D.sqDistanceRayXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
| `.SqDistanceToPnt` | `p: Pnt -> float` |
| `Line3D.sqDistanceToPnt` | `p: Pnt -> ln: Line3D -> float` |
| `.SqDistanceToXYZ` | `x: float * y: float * z: float -> float` |
| `Line3D.sqDistanceToXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
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
| `Line3D.createFromLine2D` | `ln: Line2D -> Line3D` |
| `Line3D.createFromLine2DWithZ` | `zLevel: float -> ln: Line2D -> Line3D` |
| `Line3D.createFromPntAndVec` | `p: Pnt * v: Vec -> Line3D` |
| `Line3D.createFromVec` | `v: Vec -> Line3D` |
| `Line3D.distanceToLine` | `lnA: Line3D -> lnB: Line3D -> float` |
| `Line3D.divide` | `segments: int -> ln: Line3D -> Pnt array` |
| `Line3D.divideEvery` | `dist: float -> l: Line3D -> ResizeArray<Pnt>` |
| `Line3D.divideInsideEvery` | `dist: float -> l: Line3D -> ResizeArray<Pnt>` |
| `Line3D.divideMaxLength` | `maxSegmentLength: float -> ln: Line3D -> Pnt array` |
| `Line3D.divideMinLength` | `minSegmentLength: float -> ln: Line3D -> Pnt array` |
| `Line3D.doIntersect` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line3D.doIntersectOrOverlap` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line3D.doRaysIntersect` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line3D.end'` | `l: Line3D -> Pnt` |
| `Line3D.equals` | `tol: float -> a: Line3D -> b: Line3D -> bool` |
| `Line3D.flip` | `ln: Line3D -> Line3D` |
| `Line3D.fromX` | `l: Line3D -> float` |
| `Line3D.fromY` | `l: Line3D -> float` |
| `Line3D.fromZ` | `l: Line3D -> float` |
| `Line3D.intersectCone` | `ray: Line3D * coneRadius: float * coneBaseZ: float * coneTipZ: float -> Option<float * float>` |
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
| `Line3D.lengthToPtOnLine` | `ln: Line3D -> pt: Pnt -> float` |
| `Line3D.matchesOrientationUnitVec` | `v: UnitVec -> ln: Line3D -> bool` |
| `Line3D.matchesOrientationVec` | `v: Vec -> ln: Line3D -> bool` |
| `Line3D.matchOrientation` | `orientationToMatch: Line3D -> lineToFlip: Line3D -> Line3D` |
| `Line3D.matchUnitVecOrientation` | `orientationToMatch: UnitVec -> lineToFlip: Line3D -> Line3D` |
| `Line3D.matchVecOrientation` | `orientationToMatch: Vec -> lineToFlip: Line3D -> Line3D` |
| `Line3D.notEquals` | `tol: float -> a: Line3D -> b: Line3D -> bool` |
| `Line3D.offset` | `distHorizontal: float -> distNormal: float -> l: Line3D -> Line3D` |
| `Line3D.offsetXY` | `amount: float -> ln: Line3D -> Line3D` |
| `Line3D.pointAtDistance` | `dist: float -> ln: Line3D -> Pnt` |
| `Line3D.pointAtDistanceFromEnd` | `dist: float -> ln: Line3D -> Pnt` |
| `Line3D.projectOntoRay` | `rayToProjectOnto: Line3D -> lineToProject: Line3D -> Line3D` |
| `Line3D.projectOntoRayParam` | `rayToProjectOnto: Line3D -> lineToProject: Line3D -> float * float` |
| `Line3D.reverse` | `ln: Line3D -> Line3D` |
| `Line3D.rotateOnZ` | `r: Rotation2D -> ln: Line3D -> Line3D` |
| `Line3D.rotateOnZWithCenter` | `cen: Pnt -> r: Rotation2D -> ln: Line3D -> Line3D` |
| `Line3D.split` | `gap: float -> segments: int -> ln: Line3D -> Line3D array` |
| `Line3D.splitMaxLength` | `gap: float -> maxSegmentLength: float -> ln: Line3D -> Line3D array` |
| `Line3D.splitMinLength` | `gap: float -> minSegmentLength: float -> ln: Line3D -> Line3D array` |
| `Line3D.sqDistanceToLine` | `lnA: Line3D -> lnB: Line3D -> float` |
| `Line3D.to'` | `l: Line3D -> Pnt` |
| `Line3D.toLine2D` | `ln: Line3D -> Line2D` |
| `Line3D.toX` | `l: Line3D -> float` |
| `Line3D.toY` | `l: Line3D -> float` |
| `Line3D.toZ` | `l: Line3D -> float` |
| `Line3D.translate` | `v: Vec -> ln: Line3D -> Line3D` |
| `Line3D.tryGetOverlap` | `lnA: Line3D -> lnB: Line3D -> (float * float) option` |
| `Line3D.tryIntersect` | `lnA: Line3D -> lnB: Line3D -> Pnt voption` |
| `Line3D.tryIntersectOrOverlap` | `lnA: Line3D -> lnB: Line3D -> Pnt option` |
| `Line3D.tryIntersectRay` | `lineA: Line3D -> lineB: Line3D -> Pnt voption` |
| `Line3D.tryProjectOntoLine` | `onToLine: Line3D -> lineToProject: Line3D -> Line3D option` |
| `Line3D.tryProjectOntoLineParam` | `onToLine: Line3D -> lineToProject: Line3D -> (float * float) option` |
| `Line3D.withEnd` | `pt: Pnt -> ln: Line3D -> Line3D` |
| `Line3D.withFrom` | `pt: Pnt -> ln: Line3D -> Line3D` |
| `Line3D.withLengthFromMid` | `len: float -> ln: Line3D -> Line3D` |
| `Line3D.withLengthFromStart` | `len: float -> ln: Line3D -> Line3D` |
| `Line3D.withLengthToEnd` | `len: float -> ln: Line3D -> Line3D` |
| `Line3D.withStart` | `pt: Pnt -> ln: Line3D -> Line3D` |
| `Line3D.withTo` | `pt: Pnt -> ln: Line3D -> Line3D` |

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
| `Intersect` | `xPt: Pnt -> XRay` |
| `Skew` | `closestPtA: Pnt * closestPtB: Pnt * squareDist: float -> XRay` |
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

## XLine3D.ClPts

| ClPts | Signature |
|:--|:--|
| `Intersect` | `pt: Pnt -> ClPts` |
| `Skew` | `ptA: Pnt * ptB: Pnt * squareDist: float -> ClPts` |
| `Parallel` | `ptA: Pnt * ptB: Pnt -> ClPts` |
| `Apart` | `ptA: Pnt * ptB: Pnt * squareDist: float -> ClPts` |
| `TooShortA` | `ClPts` |
| `TooShortB` | `ClPts` |
| `TooShortBoth` | `ClPts` |

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
| `XLineXYZ.doIntersect` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> bool` |
| `XLineXYZ.doOverlap` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * tolerance: float -> bool` |
| `XLineXYZ.doRaysIntersect` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float -> bool` |
| `XLineXYZ.getClosestParameters` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * tangent: float<tangent> * tooShortTolerance: float -> ClParams` |
| `XLineXYZ.getClosestPoints` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * tangent: float<tangent> * tooShortTolerance: float -> ClPts` |
| `XLineXYZ.getIntersection` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> XPnt` |
| `XLineXYZ.getIntersectionParam` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> XParam` |
| `XLineXYZ.getRayClosestParam` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * tangent: float<tangent> * tooShortTolerance: float -> XRayParam` |
| `XLineXYZ.getRayIntersection` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> XRay` |
| `XLineXYZ.parameterA` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> float` |
| `XLineXYZ.parameters` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> float * float` |
| `XLineXYZ.sqDistance` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> float` |
| `XLineXYZ.tryClosestParameterRayA` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> float voption` |
| `XLineXYZ.tryClosestPntRayA` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> Pnt voption` |
| `XLineXYZ.tryIntersect` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> Pnt voption` |
| `XLineXYZ.tryIntersectRay` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> Pnt voption` |

## XLine3D

| XLine3D | Signature |
|:--|:--|
| `XLine3D.doIntersect` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> bool` |
| `XLine3D.doIntersect` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> bool` |
| `XLine3D.doOverlap` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tolerance: float -> bool` |
| `XLine3D.doOverlap` | `lineA: Line3D * lineB: Line3D * ?tolerance: float -> bool` |
| `XLine3D.doRaysIntersect` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float -> bool` |
| `XLine3D.doRaysIntersect` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float -> bool` |
| `XLine3D.getClosestParameters` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` |
| `XLine3D.getClosestParameters` | `lineA: Line3D * lineB: Line3D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` |
| `XLine3D.getClosestPoints` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPts` |
| `XLine3D.getClosestPoints` | `lineA: Line3D * lineB: Line3D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPts` |
| `XLine3D.getEndsTouching` | `a: Line3D * b: Line3D * ?tolerance: float -> XEnds` |
| `XLine3D.getIntersection` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPnt` |
| `XLine3D.getIntersection` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPnt` |
| `XLine3D.getIntersectionParam` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` |
| `XLine3D.getIntersectionParam` | `lnA: Line3D * lnB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` |
| `XLine3D.getRayClosestParam` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` |
| `XLine3D.getRayClosestParam` | `lineA: Line3D * lineB: Line3D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` |
| `XLine3D.getRayIntersection` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` |
| `XLine3D.getRayIntersection` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` |
| `XLine3D.getSqDistance` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec -> float` |
| `XLine3D.getSqDistance` | `lineA: Line3D * lineB: Line3D -> float` |
| `XLine3D.intersectCone` | `ray: Line3D * coneRadius: float * coneBaseZ: float * coneTipZ: float -> XCone` |
| `XLine3D.parameterA` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec -> float` |
| `XLine3D.parameterA` | `lineA: Line3D * lineB: Line3D -> float` |
| `XLine3D.parameters` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec -> float * float` |
| `XLine3D.parameters` | `lineA: Line3D * lineB: Line3D -> float * float` |
| `XLine3D.tryClosestParameterRayA` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec -> float voption` |
| `XLine3D.tryClosestParameterRayA` | `lineA: Line3D * lineB: Line3D -> float voption` |
| `XLine3D.tryClosestPntRayA` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec -> Pnt voption` |
| `XLine3D.tryClosestPntRayA` | `lineA: Line3D * lineB: Line3D -> Pnt voption` |
| `XLine3D.tryIntersect` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` |
| `XLine3D.tryIntersect` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` |
| `XLine3D.tryIntersectRay` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` |
| `XLine3D.tryIntersectRay` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` |

## NPlane

| NPlane | Signature |
|:--|:--|
| `new` | `pt: Pnt * n: UnitVec -> NPlane` |
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
| `.ClosestPoint` | `pt: Pnt -> Pnt` |
| `NPlane.closestPoint` | `pt: Pnt -> pl: NPlane -> Pnt` |
| `.DistanceToPnt` | `pt: Pnt -> float` |
| `NPlane.distanceToPnt` | `pt: Pnt -> pl: NPlane -> float` |
| `.DistanceToPntSigned` | `pt: Pnt -> float` |
| `NPlane.distanceToPntSigned` | `pt: Pnt -> pl: NPlane -> float` |
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
| `.PlaneAtClPnt` | `pt: Pnt -> NPlane` |
| `NPlane.planeAtClPnt` | `pt: Pnt -> pl: NPlane -> NPlane` |
| `.ToString` | `unit -> string` |
| **Other static members** | |
| `NPlane.angle90ToUnitVec` | `v: UnitVec -> pl: NPlane -> float` |
| `NPlane.angleTo` | `a: NPlane -> b: NPlane -> float` |
| `NPlane.areCoincident` | `a: NPlane -> b: NPlane -> bool` |
| `NPlane.create` | `pt: Pnt * normal: Vec -> NPlane` |
| `NPlane.create` | `pt: Pnt * normal: UnitVec -> NPlane` |
| `NPlane.createFrom3Points` | `a: Pnt -> b: Pnt -> c: Pnt -> NPlane` |
| `NPlane.createFromPPlane` | `p: PPlane -> NPlane` |
| `NPlane.createUnchecked` | `originX: float * originY: float * originZ: float * normalX: float * normalY: float * normalZ: float -> NPlane` |
| `NPlane.createUncheckedVec` | `origin: Pnt * normal: UnitVec -> NPlane` |
| `NPlane.doLinePlaneIntersect` | `ln: Line3D -> pl: NPlane -> bool` |
| `NPlane.equals` | `tol: float -> a: NPlane -> b: NPlane -> bool` |
| `NPlane.intersect` | `a: NPlane -> b: NPlane -> Line3D option` |
| `NPlane.intersectLine` | `ln: Line3D -> pl: NPlane -> Pnt option` |
| `NPlane.intersectRay` | `ln: Line3D -> pl: NPlane -> float voption` |
| `NPlane.move` | `translation: Vec -> pl: NPlane -> NPlane` |
| `NPlane.offset` | `dist: float -> pl: NPlane -> NPlane` |
| `NPlane.offsetInDir` | `dirPt: Pnt -> dist: float -> pl: NPlane -> NPlane` |
| `NPlane.scale` | `factor: float -> pl: NPlane -> NPlane` |
| `NPlane.translate` | `translation: Vec -> pl: NPlane -> NPlane` |
| `NPlane.xyPlane` | `NPlane` |

## Rect3D

| Rect3D | Signature |
|:--|:--|
| `.OriginX` | `float` |
| `.OriginY` | `float` |
| `.OriginZ` | `float` |
| `.XaxisX` | `float` |
| `.XaxisY` | `float` |
| `.XaxisZ` | `float` |
| `.YaxisX` | `float` |
| `.YaxisY` | `float` |
| `.YaxisZ` | `float` |
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
| `.EvaluateDist` | `xDistance: float * yDistance: float -> Pnt` |
| `Rect3D.evaluateDist` | `xDistance: float -> yDistance: float -> r: Rect3D -> Pnt` |
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
| `.Points` | `Pnt array` |
| `Rect3D.points` | `r: Rect3D -> Pnt array` |
| `.PointsLooped` | `Pnt array` |
| `Rect3D.pointsLooped` | `r: Rect3D -> Pnt array` |
| `.PointsXYZ` | `ResizeArray<float>` |
| `Rect3D.pointsXYZ` | `r: Rect3D -> ResizeArray<float>` |
| `.PointsXYZLooped` | `ResizeArray<float>` |
| `Rect3D.pointsXYZLooped` | `r: Rect3D -> ResizeArray<float>` |
| `.Pt0` | `Pnt` |
| `Rect3D.pt0` | `r: Rect3D -> Pnt` |
| `.Pt1` | `Pnt` |
| `Rect3D.pt1` | `r: Rect3D -> Pnt` |
| `.Pt2` | `Pnt` |
| `Rect3D.pt2` | `r: Rect3D -> Pnt` |
| `.Pt3` | `Pnt` |
| `Rect3D.pt3` | `r: Rect3D -> Pnt` |
| `.Rotate` | `q: Quaternion -> Rect3D` |
| `Rect3D.rotate` | `q: Quaternion -> r: Rect3D -> Rect3D` |
| `.RotateOrientation180` | `Rect3D` |
| `Rect3D.rotateOrientation180` | `r: Rect3D -> Rect3D` |
| `.RotateOrientation90CCW` | `Rect3D` |
| `Rect3D.rotateOrientation90CCW` | `r: Rect3D -> Rect3D` |
| `.RotateOrientation90CW` | `Rect3D` |
| `Rect3D.rotateOrientation90CW` | `r: Rect3D -> Rect3D` |
| `.RotateWithCenter` | `cen: Pnt * q: Quaternion -> Rect3D` |
| `Rect3D.rotateWithCenter` | `cen: Pnt -> q: Quaternion -> r: Rect3D -> Rect3D` |
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
| `.Xaxis` | `Vec` |
| `Rect3D.xAxis` | `r: Rect3D -> Vec` |
| `.XaxisUnit` | `UnitVec` |
| `Rect3D.xAxisUnit` | `r: Rect3D -> UnitVec` |
| `.XCorner` | `Pnt` |
| `Rect3D.xCorner` | `r: Rect3D -> Pnt` |
| `.Yaxis` | `Vec` |
| `Rect3D.yAxis` | `r: Rect3D -> Vec` |
| `.YaxisUnit` | `UnitVec` |
| `Rect3D.yAxisUnit` | `r: Rect3D -> UnitVec` |
| `.YCorner` | `Pnt` |
| `Rect3D.yCorner` | `r: Rect3D -> Pnt` |
| **Other static members** | |
| `Rect3D.createCenteredFromPlane` | `pl: PPlane * sizeX: float * sizeY: float -> Rect3D` |
| `Rect3D.createFrom3Points` | `origin: Pnt * xPt: Pnt * yPt: Pnt -> Rect3D` |
| `Rect3D.createFromBounds2D` | `minX: float * minY: float * maxX: float * maxY: float -> Rect3D` |
| `Rect3D.createFromPlane` | `pl: PPlane * sizeX: float * sizeY: float -> Rect3D` |
| `Rect3D.createFromVectors` | `origin: Pnt * x: Vec * y: Vec -> Rect3D` |
| `Rect3D.createUnchecked` | `originX: float * originY: float * originZ: float * xAxisX: float * xAxisY: float * xAxisZ: float * yAxisX: float * yAxisY: float * yAxisZ: float -> Rect3D` |
| `Rect3D.createUncheckedVec` | `origin: Pnt * x: Vec * y: Vec -> Rect3D` |
| `Rect3D.equals` | `tol: float -> a: Rect3D -> b: Rect3D -> bool` |
| `Rect3D.expand` | `dist: float -> r: Rect3D -> Rect3D` |
| `Rect3D.expandRel` | `factor: float -> r: Rect3D -> Rect3D` |
| `Rect3D.expandRelXY` | `factorX: float -> factorY: float -> r: Rect3D -> Rect3D` |
| `Rect3D.expandXY` | `distX: float -> distY: float -> r: Rect3D -> Rect3D` |
| `Rect3D.fitToPoints` | `pts: IList<Pnt> -> refRect: Rect3D -> Rect3D` |
| `Rect3D.flip` | `r: Rect3D -> Rect3D` |
| `Rect3D.grid` | `rect: Rect3D * xCount: int * yCount: int -> Pnt array array` |
| `Rect3D.gridMaxLength` | `rect: Rect3D * xMaxLen: float * yMaxLen: float -> Pnt array array` |
| `Rect3D.gridMinLength` | `rect: Rect3D * xMinLen: float * yMinLen: float -> Pnt array array` |
| `Rect3D.intersectLine` | `ln: Line3D -> r: Rect3D -> Pnt voption` |
| `Rect3D.intersectRay` | `ln: Line3D -> r: Rect3D -> float voption` |
| `Rect3D.iterPointsCCW` | `action: (float -> float -> float -> unit) -> r: Rect3D -> unit` |
| `Rect3D.iterPointsCW` | `action: (float -> float -> float -> unit) -> r: Rect3D -> unit` |
| `Rect3D.iterPointsLoopedCCW` | `action: (float -> float -> float -> unit) -> r: Rect3D -> unit` |
| `Rect3D.iterPointsLoopedCW` | `action: (float -> float -> float -> unit) -> r: Rect3D -> unit` |
| `Rect3D.notEquals` | `tol: float -> a: Rect3D -> b: Rect3D -> bool` |
| `Rect3D.offset` | `dist: float -> rect: Rect3D -> Rect3D` |
| `Rect3D.offsetCorner` | `rect: Rect3D * corner: int * xOffset: float * yOffset: float * xSize: float * ySize: float -> Rect3D` |
| `Rect3D.offsetEdge` | `rect: Rect3D * edgeIdx: int * offEdge: float * width: float * offStart: float * offEnd: float -> Rect3D` |
| `Rect3D.offsetVar` | `dist: float array -> rect: Rect3D -> Rect3D` |
| `Rect3D.offsetZ` | `offsetDistance: float -> r: Rect3D -> Rect3D` |
| `Rect3D.subDivide` | `rect: Rect3D * xCount: int * yCount: int * xGap: float * yGap: float -> Rect3D array array` |
| `Rect3D.subDivideMaxLength` | `rect: Rect3D * xMaxLen: float * yMaxLen: float * xGap: float * yGap: float -> Rect3D array array` |
| `Rect3D.subDivideMinLength` | `rect: Rect3D * xMinLen: float * yMinLen: float * xGap: float * yGap: float -> Rect3D array array` |
| `Rect3D.translateLocalX` | `distX: float -> r: Rect3D -> Rect3D` |
| `Rect3D.translateLocalY` | `distY: float -> r: Rect3D -> Rect3D` |
| `Rect3D.translateLocalZ` | `distZ: float -> r: Rect3D -> Rect3D` |
| `Rect3D.tryCreateFrom3Points` | `origin: Pnt * xPt: Pnt * yPt: Pnt -> Rect3D option` |

## Box

| Box | Signature |
|:--|:--|
| `.OriginX` | `float` |
| `.OriginY` | `float` |
| `.OriginZ` | `float` |
| `.XaxisX` | `float` |
| `.XaxisY` | `float` |
| `.XaxisZ` | `float` |
| `.YaxisX` | `float` |
| `.YaxisY` | `float` |
| `.YaxisZ` | `float` |
| `.ZaxisX` | `float` |
| `.ZaxisY` | `float` |
| `.ZaxisZ` | `float` |
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
| `.GetPoint` | `pointIndex: int -> Pnt` |
| `Box.getPoint` | `pointIndex: int -> b: Box -> Pnt` |
| `.HasVolume` | `bool` |
| `Box.hasVolume` | `b: Box -> bool` |
| `.IntersectPlane` | `pl: NPlane -> ResizeArray<Pnt>` |
| `Box.intersectPlane` | `pl: NPlane -> b: Box -> ResizeArray<Pnt>` |
| `.IntersectRay` | `ray: Line3D -> (float * float) voption` |
| `Box.intersectRay` | `ray: Line3D -> box: Box -> (float * float) voption` |
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
| `.Points` | `Pnt array` |
| `Box.points` | `b: Box -> Pnt array` |
| `.Pt0` | `Pnt` |
| `Box.pt0` | `b: Box -> Pnt` |
| `.Pt0X` | `float` |
| `.Pt0Y` | `float` |
| `.Pt0Z` | `float` |
| `.Pt1` | `Pnt` |
| `Box.pt1` | `b: Box -> Pnt` |
| `.Pt1X` | `float` |
| `.Pt1Y` | `float` |
| `.Pt1Z` | `float` |
| `.Pt2` | `Pnt` |
| `Box.pt2` | `b: Box -> Pnt` |
| `.Pt2X` | `float` |
| `.Pt2Y` | `float` |
| `.Pt2Z` | `float` |
| `.Pt3` | `Pnt` |
| `Box.pt3` | `b: Box -> Pnt` |
| `.Pt3X` | `float` |
| `.Pt3Y` | `float` |
| `.Pt3Z` | `float` |
| `.Pt4` | `Pnt` |
| `Box.pt4` | `b: Box -> Pnt` |
| `.Pt4X` | `float` |
| `.Pt4Y` | `float` |
| `.Pt4Z` | `float` |
| `.Pt5` | `Pnt` |
| `Box.pt5` | `b: Box -> Pnt` |
| `.Pt5X` | `float` |
| `.Pt5Y` | `float` |
| `.Pt5Z` | `float` |
| `.Pt6` | `Pnt` |
| `Box.pt6` | `b: Box -> Pnt` |
| `.Pt6X` | `float` |
| `.Pt6Y` | `float` |
| `.Pt6Z` | `float` |
| `.Pt7` | `Pnt` |
| `Box.pt7` | `b: Box -> Pnt` |
| `.Pt7X` | `float` |
| `.Pt7Y` | `float` |
| `.Pt7Z` | `float` |
| `.RightFace` | `Rect3D` |
| `Box.rightFace` | `b: Box -> Rect3D` |
| `.Rotate` | `q: Quaternion -> Box` |
| `Box.rotate` | `q: Quaternion -> b: Box -> Box` |
| `.RotateWithCenter` | `cen: Pnt * q: Quaternion -> Box` |
| `Box.rotateWithCenter` | `cen: Pnt -> q: Quaternion -> b: Box -> Box` |
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
| `.Volume` | `float` |
| `Box.volume` | `b: Box -> float` |
| `.Xaxis` | `Vec` |
| `Box.xAxis` | `b: Box -> Vec` |
| `.XaxisUnit` | `UnitVec` |
| `Box.xAxisUnit` | `b: Box -> UnitVec` |
| `.Yaxis` | `Vec` |
| `Box.yAxis` | `b: Box -> Vec` |
| `.YaxisUnit` | `UnitVec` |
| `Box.yAxisUnit` | `b: Box -> UnitVec` |
| `.Zaxis` | `Vec` |
| `Box.zAxis` | `b: Box -> Vec` |
| `.ZaxisUnit` | `UnitVec` |
| `Box.zAxisUnit` | `b: Box -> UnitVec` |
| **Other static members** | |
| `Box.areBoxesAligned` | `cosineTolerance: float<cosine> -> a: Box -> b: Box -> bool` |
| `Box.create` | `origin: Pnt * xAxis: Vec * yAxis: Vec * zAxis: Vec -> Box` |
| `Box.createFromBounds` | `minX: float * minY: float * minZ: float * maxX: float * maxY: float * maxZ: float -> Box` |
| `Box.createFromDirsAndPoints` | `dirX: Vec -> dirY: Vec -> pts: IList<Pnt> -> Box` |
| `Box.createFromPlane` | `x: float -> y: float -> z: float -> pl: PPlane -> Box` |
| `Box.createFromPlaneAndPoints` | `pl: PPlane -> pts: IList<Pnt> -> Box` |
| `Box.createFromRect2D` | `zLow: float -> zHigh: float -> r: Rect2D -> Box` |
| `Box.createFromRect3D` | `zLow: float -> zHigh: float -> r: Rect3D -> Box` |
| `Box.createUnchecked` | `originX: float * originY: float * originZ: float * xAxisX: float * xAxisY: float * xAxisZ: float * yAxisX: float * yAxisY: float * yAxisZ: float * zAxisX: float * zAxisY: float * zAxisZ: float -> Box` |
| `Box.createUncheckedVec` | `origin: Pnt * xAxis: Vec * yAxis: Vec * zAxis: Vec -> Box` |
| `Box.equals` | `tol: float -> a: Box -> b: Box -> bool` |
| `Box.expand` | `dist: float -> b: Box -> Box` |
| `Box.expandRel` | `factor: float -> b: Box -> Box` |
| `Box.expandRelXYZ` | `factorX: float -> factorY: float -> factorZ: float -> b: Box -> Box` |
| `Box.expandXYZ` | `distX: float -> distY: float -> distZ: float -> b: Box -> Box` |
| `Box.failIfBoxesNotAlignedTo` | `a: Box -> msg: string -> b: Box -> Box` |
| `Box.getBoxBetween` | `a: Box -> b: Box -> Box` |
| `Box.getBoxesAlignment` | `a: Box -> b: Box -> float` |
| `Box.notEquals` | `tol: float -> a: Box -> b: Box -> bool` |
| `Box.translateLocalX` | `distX: float -> b: Box -> Box` |
| `Box.translateLocalY` | `distY: float -> b: Box -> Box` |
| `Box.translateLocalZ` | `distZ: float -> b: Box -> Box` |
| `Box.tryGetBoxBetween` | `a: Box -> b: Box -> Box option` |
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
| `.GetPt` | `pointIndex: int -> Pnt` |
| `FreeBox.getPt` | `pointIndex: int -> b: FreeBox -> Pnt` |
| `.GetX` | `pointIndex: int -> float` |
| `FreeBox.getX` | `pointIndex: int -> b: FreeBox -> float` |
| `.GetY` | `pointIndex: int -> float` |
| `FreeBox.getY` | `pointIndex: int -> b: FreeBox -> float` |
| `.GetZ` | `pointIndex: int -> float` |
| `FreeBox.getZ` | `pointIndex: int -> b: FreeBox -> float` |
| `.Move` | `v: Vec -> unit` |
| `FreeBox.move` | `v: Vec -> b: FreeBox -> unit` |
| `.MoveX` | `distance: float -> unit` |
| `FreeBox.moveX` | `distance: float -> b: FreeBox -> unit` |
| `.MoveY` | `distance: float -> unit` |
| `FreeBox.moveY` | `distance: float -> b: FreeBox -> unit` |
| `.MoveZ` | `distance: float -> unit` |
| `FreeBox.moveZ` | `distance: float -> b: FreeBox -> unit` |
| `.Pt0` | `Pnt with get, set` |
| `FreeBox.pt0` | `b: FreeBox -> Pnt` |
| `.Pt0X` | `float with get, set` |
| `FreeBox.pt0X` | `b: FreeBox -> float` |
| `.Pt0Y` | `float with get, set` |
| `FreeBox.pt0Y` | `b: FreeBox -> float` |
| `.Pt0Z` | `float with get, set` |
| `FreeBox.pt0Z` | `b: FreeBox -> float` |
| `.Pt1` | `Pnt with get, set` |
| `FreeBox.pt1` | `b: FreeBox -> Pnt` |
| `.Pt1X` | `float with get, set` |
| `FreeBox.pt1X` | `b: FreeBox -> float` |
| `.Pt1Y` | `float with get, set` |
| `FreeBox.pt1Y` | `b: FreeBox -> float` |
| `.Pt1Z` | `float with get, set` |
| `FreeBox.pt1Z` | `b: FreeBox -> float` |
| `.Pt2` | `Pnt with get, set` |
| `FreeBox.pt2` | `b: FreeBox -> Pnt` |
| `.Pt2X` | `float with get, set` |
| `FreeBox.pt2X` | `b: FreeBox -> float` |
| `.Pt2Y` | `float with get, set` |
| `FreeBox.pt2Y` | `b: FreeBox -> float` |
| `.Pt2Z` | `float with get, set` |
| `FreeBox.pt2Z` | `b: FreeBox -> float` |
| `.Pt3` | `Pnt with get, set` |
| `FreeBox.pt3` | `b: FreeBox -> Pnt` |
| `.Pt3X` | `float with get, set` |
| `FreeBox.pt3X` | `b: FreeBox -> float` |
| `.Pt3Y` | `float with get, set` |
| `FreeBox.pt3Y` | `b: FreeBox -> float` |
| `.Pt3Z` | `float with get, set` |
| `FreeBox.pt3Z` | `b: FreeBox -> float` |
| `.Pt4` | `Pnt with get, set` |
| `FreeBox.pt4` | `b: FreeBox -> Pnt` |
| `.Pt4X` | `float with get, set` |
| `FreeBox.pt4X` | `b: FreeBox -> float` |
| `.Pt4Y` | `float with get, set` |
| `FreeBox.pt4Y` | `b: FreeBox -> float` |
| `.Pt4Z` | `float with get, set` |
| `FreeBox.pt4Z` | `b: FreeBox -> float` |
| `.Pt5` | `Pnt with get, set` |
| `FreeBox.pt5` | `b: FreeBox -> Pnt` |
| `.Pt5X` | `float with get, set` |
| `FreeBox.pt5X` | `b: FreeBox -> float` |
| `.Pt5Y` | `float with get, set` |
| `FreeBox.pt5Y` | `b: FreeBox -> float` |
| `.Pt5Z` | `float with get, set` |
| `FreeBox.pt5Z` | `b: FreeBox -> float` |
| `.Pt6` | `Pnt with get, set` |
| `FreeBox.pt6` | `b: FreeBox -> Pnt` |
| `.Pt6X` | `float with get, set` |
| `FreeBox.pt6X` | `b: FreeBox -> float` |
| `.Pt6Y` | `float with get, set` |
| `FreeBox.pt6Y` | `b: FreeBox -> float` |
| `.Pt6Z` | `float with get, set` |
| `FreeBox.pt6Z` | `b: FreeBox -> float` |
| `.Pt7` | `Pnt with get, set` |
| `FreeBox.pt7` | `b: FreeBox -> Pnt` |
| `.Pt7X` | `float with get, set` |
| `FreeBox.pt7X` | `b: FreeBox -> float` |
| `.Pt7Y` | `float with get, set` |
| `FreeBox.pt7Y` | `b: FreeBox -> float` |
| `.Pt7Z` | `float with get, set` |
| `FreeBox.pt7Z` | `b: FreeBox -> float` |
| `.Rotate` | `q: Quaternion -> unit` |
| `FreeBox.rotate` | `q: Quaternion -> b: FreeBox -> unit` |
| `.RotateWithCenter` | `cen: Pnt * q: Quaternion -> unit` |
| `FreeBox.rotateWithCenter` | `cen: Pnt -> q: Quaternion -> b: FreeBox -> unit` |
| `.Scale` | `factor: float -> unit` |
| `FreeBox.scale` | `factor: float -> b: FreeBox -> unit` |
| `.ScaleOn` | `cen: Pnt * factor: float -> unit` |
| `FreeBox.scaleOn` | `cen: Pnt -> factor: float -> b: FreeBox -> unit` |
| `.SetPt` | `pointIndex: int -> p: Pnt -> unit` |
| `FreeBox.setPt` | `pointIndex: int -> p: Pnt -> b: FreeBox -> unit` |
| `.SetPtXYZ` | `pointIndex: int * x: float * y: float * z: float -> unit` |
| `FreeBox.setPtXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> b: FreeBox -> unit` |
| `.ToString` | `unit -> string` |
| `.Transform` | `m: Matrix -> unit` |
| `FreeBox.transform` | `m: Matrix -> b: FreeBox -> unit` |
| `.TransformRigid` | `m: RigidMatrix -> unit` |
| `FreeBox.transformRigid` | `m: RigidMatrix -> b: FreeBox -> unit` |
| `.XYZs` | `float array` |
| **Other static members** | |
| `FreeBox.createDirectly` | `xyzs: float array -> FreeBox` |
| `FreeBox.createFromBox` | `box: Box -> FreeBox` |
| `FreeBox.createFromEightPoints` | `pts: Pnt array -> FreeBox` |
| `FreeBox.createFromFour2DPoints` | `zMin: float -> zMax: float -> pts: Pt array -> FreeBox` |
| `FreeBox.createFromFour2DPointsArgs` | `a: Pt * b: Pt * c: Pt * d: Pt * zMin: float * zMax: float -> FreeBox` |
| `FreeBox.getXYZs` | `b: FreeBox -> float array` |
| `FreeBox.translate` | `v: Vec -> b: FreeBox -> unit` |

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
| `BBox.containsPnt` | `pt: Pnt -> box: BBox -> bool` |
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
| `.Expand` | `xDist: float * yDist: float * zDist: float -> BBox` |
| `BBox.expand` | `dist: float -> b: BBox -> BBox` |
| `.ExpandSafe` | `xDist: float * yDist: float * zDist: float -> BBox` |
| `.ExpandSafe` | `dist: float -> BBox` |
| `BBox.expandSafe` | `dist: float -> b: BBox -> BBox` |
| `.ExpandXaxis` | `startDist: float * endDist: float -> BBox` |
| `BBox.expandXaxis` | `startDist: float -> endDist: float -> b: BBox -> BBox` |
| `.ExpandYaxis` | `startDist: float * endDist: float -> BBox` |
| `BBox.expandYaxis` | `startDist: float -> endDist: float -> b: BBox -> BBox` |
| `.ExpandZaxis` | `startDist: float * endDist: float -> BBox` |
| `BBox.expandZaxis` | `startDist: float -> endDist: float -> b: BBox -> BBox` |
| `.Faces` | `Rect3D array` |
| `BBox.faces` | `b: BBox -> Rect3D array` |
| `.FrontFace` | `Rect3D` |
| `BBox.frontFace` | `b: BBox -> Rect3D` |
| `.GetEdge` | `edgeIndex: int -> Line3D` |
| `BBox.getEdge` | `edgeIndex: int -> b: BBox -> Line3D` |
| `.GetFace` | `faceIndex: int -> Rect3D` |
| `BBox.getFace` | `faceIndex: int -> b: BBox -> Rect3D` |
| `.GetPoint` | `pointIndex: int -> Pnt` |
| `BBox.getPoint` | `pointIndex: int -> b: BBox -> Pnt` |
| `.HasVolume` | `bool` |
| `BBox.hasVolume` | `b: BBox -> bool` |
| `.Intersection` | `a: BBox -> BBox voption` |
| `BBox.intersection` | `a: BBox -> b: BBox -> BBox voption` |
| `.IntersectPlane` | `pl: NPlane -> ResizeArray<Pnt>` |
| `BBox.intersectPlane` | `pl: NPlane -> b: BBox -> ResizeArray<Pnt>` |
| `.IntersectRay` | `ray: Line3D -> (float * float) voption` |
| `BBox.intersectRay` | `ray: Line3D -> b: BBox -> (float * float) voption` |
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
| `.Points` | `Pnt array` |
| `BBox.points` | `b: BBox -> Pnt array` |
| `.Pt0` | `Pnt` |
| `BBox.pt0` | `b: BBox -> Pnt` |
| `.Pt1` | `Pnt` |
| `BBox.pt1` | `b: BBox -> Pnt` |
| `.Pt2` | `Pnt` |
| `BBox.pt2` | `b: BBox -> Pnt` |
| `.Pt3` | `Pnt` |
| `BBox.pt3` | `b: BBox -> Pnt` |
| `.Pt4` | `Pnt` |
| `BBox.pt4` | `b: BBox -> Pnt` |
| `.Pt5` | `Pnt` |
| `BBox.pt5` | `b: BBox -> Pnt` |
| `.Pt6` | `Pnt` |
| `BBox.pt6` | `b: BBox -> Pnt` |
| `.Pt7` | `Pnt` |
| `BBox.pt7` | `b: BBox -> Pnt` |
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
| `.Union` | `a: BBox -> BBox` |
| `BBox.union` | `a: BBox -> b: BBox -> BBox` |
| `.UnionPnt` | `p: Pnt -> BBox` |
| `BBox.unionPnt` | `p: Pnt -> b: BBox -> BBox` |
| `.Volume` | `float` |
| `BBox.volume` | `b: BBox -> float` |
| **Other static members** | |
| `BBox.create` | `a: Pnt * b: Pnt -> BBox` |
| `BBox.createFromBox` | `b: Box -> BBox` |
| `BBox.createFromBRect` | `minZ: float -> maxZ: float -> r: BRect -> BBox` |
| `BBox.createFromCenter` | `center: Pnt * sizeX: float * sizeY: float * sizeZ: float -> BBox` |
| `BBox.createFromIList` | `ps: IList<Pnt> -> BBox` |
| `BBox.createFromLine` | `l: Line3D -> BBox` |
| `BBox.createFromRect3D` | `r: Rect3D -> BBox` |
| `BBox.createFromSeq` | `ps: Pnt seq -> BBox` |
| `BBox.createUnchecked` | `minX: float * minY: float * minZ: float * maxX: float * maxY: float * maxZ: float -> BBox` |
| `BBox.equals` | `tol: float -> a: BBox -> b: BBox -> bool` |
| `BBox.expandRel` | `factor: float -> b: BBox -> BBox` |
| `BBox.expandRelXYZ` | `factorX: float -> factorY: float -> factorZ: float -> b: BBox -> BBox` |
| `BBox.expandSafeXYZ` | `xDist: float -> yDist: float -> zDist: float -> b: BBox -> BBox` |
| `BBox.expandXYZ` | `xDist: float -> yDist: float -> zDist: float -> b: BBox -> BBox` |
| `BBox.move` | `v: Vec -> b: BBox -> BBox` |
| `BBox.moveX` | `translation: float -> b: BBox -> BBox` |
| `BBox.moveY` | `translation: float -> b: BBox -> BBox` |
| `BBox.moveZ` | `translation: float -> b: BBox -> BBox` |
| `BBox.notEquals` | `tol: float -> a: BBox -> b: BBox -> bool` |
| `BBox.scale` | `factor: float -> b: BBox -> BBox` |
| `BBox.translate` | `v: Vec -> b: BBox -> BBox` |

## Tria3D

| Tria3D | Signature |
|:--|:--|
| `Tria3D.area` | `a: Pnt * b: Pnt * c: Pnt -> float` |
| `Tria3D.areaDouble` | `a: Pnt * b: Pnt * c: Pnt -> float` |
| `Tria3D.intersectLine` | `line: Line3D * p1: Pnt * p2: Pnt * p3: Pnt -> Pnt voption` |
| `Tria3D.intersectRay` | `line: Line3D * p1: Pnt * p2: Pnt * p3: Pnt -> float voption` |
| `Tria3D.intersectRayXYZ` | `lnFromX: float * lnFromY: float * lnFromZ: float * lnToX: float * lnToY: float * lnToZ: float * ptAx: float * ptAy: float * ptAz: float * ptBx: float * ptBy: float * ptBz: float * ptCx: float * ptCy: float * ptCz: float -> float voption` |
| `Tria3D.intersectTriaVectorsWithRay` | `lnFromX: float * lnFromY: float * lnFromZ: float * dirX: float * dirY: float * dirZ: float * ptAx: float * ptAy: float * ptAz: float * e1x: float * e1y: float * e1z: float * e2x: float * e2y: float * e2z: float -> float voption` |
| `Tria3D.isLinear` | `a: Pnt * b: Pnt * c: Pnt * ?distanceTolerance: float -> bool` |
| `Tria3D.isLinearFast` | `a: Pnt * b: Pnt * c: Pnt * ?maxAreaParallelogram: float -> bool` |
| `Tria3D.offset` | `a: Pnt * b: Pnt * c: Pnt * dist: float -> Pnt * Pnt * Pnt` |
| `Tria3D.offsetPnt` | `pntToOffset: Pnt * prev: Pnt * next: Pnt * dist: float -> Pnt` |
| `Tria3D.offsetVar` | `ptToOffset: Pnt * prev: Pnt * next: Pnt * distPrev: float * distNext: float -> ValueOption<Pnt>` |

## Points3D

| Points3D | Signature |
|:--|:--|
| `Points3D.center` | `pts: IList<Pnt> -> Pnt` |
| `Points3D.closestOfTwo` | `pt1: Pnt -> pt2: Pnt -> referencePoint: Pnt -> Pnt` |
| `Points3D.closestPoint` | `pts: IList<Pnt> * pt: Pnt -> Pnt` |
| `Points3D.closestPointIdx` | `pts: IList<Pnt> * pt: Pnt -> int` |
| `Points3D.closestPointsIdx` | `xs: IList<Pnt> * ys: IList<Pnt> -> int * int` |
| `Points3D.minDistBetweenPointSets` | `xs: IList<Pnt> * ys: IList<Pnt> -> float` |
| `Points3D.mostDistantPoint` | `findPointFrom: IList<Pnt> * checkAgainst: IList<Pnt> -> Pnt` |
| `Points3D.mostDistantPointIdx` | `findPointFrom: IList<Pnt> * checkAgainst: IList<Pnt> -> int * int` |
| `Points3D.normalOfPoints` | `pts: IList<Pnt> -> Vec` |

## module Offset3D

| Offset3D | Signature |
|:--|:--|
| `Offset3D.getCollinearNeighbors` | `dirs: ResizeArray<OffsetDirection voption> -> ResizeArray<CollinearPoint>` |
| `Offset3D.getOffsetDirections` | `uvs: ResizeArray<float> * refNormal: UnitVec * isOpen: bool * considerCollinearBelow: float<cosine> * failAtUTurnAbove: float<cosine> -> ResizeArray<OffsetDirection voption>` |
| `Offset3D.getSegmentUnitVectors` | `xyzs: ResizeArray<float> -> ResizeArray<float>` |
| `Offset3D.offset` | `distInPlane: float -> distPerpendicular: float -> xyzs: ResizeArray<float> -> ResizeArray<float>` |
| `Offset3D.offset'` | `refNormal: UnitVec -> distInPlane: float -> distPerpendicular: float -> xyzs: ResizeArray<float> -> ResizeArray<float>` |
| `Offset3D.offsetConstantWithDirections` | `xyzs: ResizeArray<float> * dirs: ResizeArray<OffsetDirection voption> * distInPlane: float * distPerpendicular: float -> ResizeArray<float>` |
| `Offset3D.offsetVariable` | `distancesInPlane: ResizeArray<float> -> distancesPerpendicular: ResizeArray<float> -> xyzs: ResizeArray<float> -> ResizeArray<float>` |
| `Offset3D.offsetVariable'` | `varDistParallelBehavior: VarDistParallel -> refNormal: UnitVec -> distancesInPlane: ResizeArray<float> -> distancesPerpendicular: ResizeArray<float> -> xyzs: ResizeArray<float> -> ResizeArray<float>` |
| `Offset3D.offsetVariableWithDirections` | `xyzs: ResizeArray<float> * segmentDirs: ResizeArray<float> * offDirs: ResizeArray<OffsetDirection voption> * distsInPlane: IList<float> * distsPerpendicular: IList<float> * isClosed: bool * varDistParallelBehavior: VarDistParallel -> ResizeArray<float>` |
| `Offset3D.openTolerance` | `float` |
| `Offset3D.removeUTurns` | `xyzs: ResizeArray<float> * uvs: ResizeArray<float> * minCos: float<cosine> -> ResizeArray<float>` |
| `Offset3D.setOffCorner` | `res: ResizeArray<float> * x: float * y: float * z: float * distInPlane: float * distPerp: float * nPrev: UnitVec * nNext: UnitVec * perpDir: UnitVec * cosine: float -> unit` |
| `Offset3D.setOffCornerVar` | `res: ResizeArray<float> * x: float * y: float * z: float * distInPlanePrev: float * distInPlaneNext: float * distPerp: float * nPrev: UnitVec * nNext: UnitVec * perpDir: UnitVec * cosine: float * vNext: UnitVec -> unit` |
| `Offset3D.sqOpenTolerance` | `float` |

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
| `.AddPoint` | `pt: Pnt -> unit` |
| `Polyline3D.addPoint` | `pt: Pnt -> p: Polyline3D -> unit` |
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
| `Polyline3D.close` | `toleranceForAddingPoint: float -> pl: Polyline3D -> unit` |
| `.ClosestParameter` | `p: Pnt -> float` |
| `Polyline3D.closestParameter` | `pl: Polyline3D -> pt: Pnt -> float` |
| `.ClosestParameterXYZ` | `x: float * y: float * z: float -> float` |
| `Polyline3D.closestParameterXYZ` | `pl: Polyline3D -> x: float -> y: float -> z: float -> float` |
| `.ClosestPoint` | `p: Pnt -> Pnt` |
| `Polyline3D.closestPoint` | `pl: Polyline3D -> pt: Pnt -> Pnt` |
| `.ClosestPointIndex` | `p: Pnt -> int` |
| `Polyline3D.closestPointIndex` | `pl: Polyline3D -> pt: Pnt -> int` |
| `.DistanceTo` | `p: Pnt -> float` |
| `Polyline3D.distanceTo` | `pl: Polyline3D -> pt: Pnt -> float` |
| `.DistanceToXYZ` | `x: float * y: float * z: float -> float` |
| `Polyline3D.distanceToXYZ` | `pl: Polyline3D -> x: float -> y: float -> z: float -> float` |
| `.Duplicate` | `unit -> Polyline3D` |
| `Polyline3D.duplicate` | `pl: Polyline3D -> Polyline3D` |
| `.End` | `Pnt with get, set` |
| `.EvaluateAt` | `t: float -> Pnt` |
| `Polyline3D.evaluateAt` | `t: float -> pl: Polyline3D -> Pnt` |
| `.FirstPoint` | `Pnt with get, set` |
| `Polyline3D.firstPoint` | `pl: Polyline3D -> Pnt` |
| `.FirstSegment` | `Line3D` |
| `Polyline3D.firstSegment` | `pl: Polyline3D -> Line3D` |
| `.GetPt` | `pointIndex: int -> Pnt` |
| `Polyline3D.getPt` | `pointIndex: int -> p: Polyline3D -> Pnt` |
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
| `Polyline3D.move` | `v: Vec -> pl: Polyline3D -> unit` |
| `.MoveX` | `distance: float -> unit` |
| `Polyline3D.moveX` | `distance: float -> pl: Polyline3D -> unit` |
| `.MoveY` | `distance: float -> unit` |
| `Polyline3D.moveY` | `distance: float -> pl: Polyline3D -> unit` |
| `.MoveZ` | `distance: float -> unit` |
| `Polyline3D.moveZ` | `distance: float -> pl: Polyline3D -> unit` |
| `.PointCount` | `int` |
| `Polyline3D.pointCount` | `p: Polyline3D -> int` |
| `.Reverse` | `unit -> unit` |
| `Polyline3D.reverse` | `p: Polyline3D -> unit` |
| `.Rotate` | `q: Quaternion -> unit` |
| `Polyline3D.rotate` | `q: Quaternion -> pl: Polyline3D -> unit` |
| `.RotateOnZ` | `r: Rotation2D -> unit` |
| `Polyline3D.rotateOnZ` | `r: Rotation2D -> pl: Polyline3D -> unit` |
| `.RotateOnZWithCenter` | `cen: Pnt -> r: Rotation2D -> unit` |
| `Polyline3D.rotateOnZWithCenter` | `cen: Pnt -> r: Rotation2D -> pl: Polyline3D -> unit` |
| `.RotateWithCenter` | `cen: Pnt * q: Quaternion -> unit` |
| `Polyline3D.rotateWithCenter` | `cen: Pnt -> q: Quaternion -> pl: Polyline3D -> unit` |
| `.Scale` | `factor: float -> unit` |
| `Polyline3D.scale` | `factor: float -> pl: Polyline3D -> unit` |
| `.ScaleOn` | `cen: Pnt -> factor: float -> unit` |
| `Polyline3D.scaleOn` | `cen: Pnt -> factor: float -> pl: Polyline3D -> unit` |
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
| `.SetPnt` | `pointIndex: int * pt: Pnt -> unit` |
| `Polyline3D.setPnt` | `pointIndex: int -> pt: Pnt -> p: Polyline3D -> unit` |
| `.SetPointXYZ` | `pointIndex: int * x: float * y: float * z: float -> unit` |
| `Polyline3D.setPointXYZ` | `x: float -> y: float -> z: float -> pointIndex: int -> p: Polyline3D -> unit` |
| `.SetPointXYZClosed` | `pointIndex: int * x: float * y: float * z: float -> unit` |
| `Polyline3D.setPointXYZClosed` | `x: float -> y: float -> z: float -> pointIndex: int -> p: Polyline3D -> unit` |
| `.SignedAreaIn2D` | `float` |
| `Polyline3D.signedAreaIn2D` | `pl: Polyline3D -> float` |
| `.Start` | `Pnt with get, set` |
| `Polyline3D.start` | `pl: Polyline3D -> Pnt` |
| `.ToString` | `unit -> string` |
| `.Transform` | `m: Matrix -> unit` |
| `Polyline3D.transform` | `m: Matrix -> pl: Polyline3D -> unit` |
| `.TransformRigid` | `m: RigidMatrix -> unit` |
| `Polyline3D.transformRigid` | `m: RigidMatrix -> pl: Polyline3D -> unit` |
| `.TryAverageNormal` | `Vec option` |
| `Polyline3D.tryAverageNormal` | `pl: Polyline3D -> Vec option` |
| `.TryAveragePlane` | `?tolerance: float -> NPlane option` |
| `Polyline3D.tryAveragePlane` | `tolerance: float -> pl: Polyline3D -> NPlane option` |
| `.TryPlane` | `?tolerance: float -> NPlane option` |
| `Polyline3D.tryPlane` | `tolerance: float -> pl: Polyline3D -> NPlane option` |
| `.XYZs` | `ResizeArray<float>` |
| **Other static members** | |
| `Polyline3D.createDirectly` | `xyzs: ResizeArray<float> -> Polyline3D` |
| `Polyline3D.createEmpty` | `capacity: int -> Polyline3D` |
| `Polyline3D.createFrom2DXYMembers` | `xyObjs: ^T seq -> Polyline3D` |
| `Polyline3D.createFrom2DxyMembers` | `xyObjs: ^T seq -> Polyline3D` |
| `Polyline3D.createFromPts` | `points: Pnt seq -> Polyline3D` |
| `Polyline3D.createFromRect3D` | `r: Rect3D -> Polyline3D` |
| `Polyline3D.createFromXYZMembers` | `xyzObjs: ^T seq -> Polyline3D` |
| `Polyline3D.createFromxyzMembers` | `xyzObjs: ^T seq -> Polyline3D` |
| `Polyline3D.end'` | `pl: Polyline3D -> Pnt` |
| `Polyline3D.ensureClockwiseIn2D` | `pl: Polyline3D -> unit` |
| `Polyline3D.ensureCounterClockwiseIn2D` | `pl: Polyline3D -> unit` |
| `Polyline3D.equals` | `pl1: Polyline3D -> pl2: Polyline3D -> bool` |
| `Polyline3D.equalsTol` | `tol: float -> pl1: Polyline3D -> pl2: Polyline3D -> bool` |
| `Polyline3D.getXYZs` | `p: Polyline3D -> ResizeArray<float>` |
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
| `Polyline3D.offset` | `polyLine: Polyline3D * inPlaneOffsetDistance: float * perpendicularOffsetDistance: float * ?loop: bool -> Polyline3D` |
| `Polyline3D.offset'` | `inPlaneOffsetDistance: float -> perpendicularOffsetDistance: float -> polyLine: Polyline3D -> Polyline3D` |
| `Polyline3D.offsetVar` | `polyLine: Polyline3D * inPlaneOffsetDistances: ResizeArray<float> * perpendicularOffsetDistances: ResizeArray<float> * ?loop: bool * ?varDistParallelBehavior: VarDistParallel * ?considerCollinearBelow: float<cosine> * ?failAtUTurnAbove: float<cosine> -> Polyline3D` |
| `Polyline3D.offsetVarWithRef` | `polyLine: Polyline3D * inPlaneOffsetDistances: IList<float> * perpendicularOffsetDistances: IList<float> * refNormal: UnitVec * ?loop: bool * ?varDistParallelBehavior: VarDistParallel * ?considerCollinearBelow: float<cosine> * ?failAtUTurnAbove: float<cosine> -> Polyline3D` |
| `Polyline3D.offsetWithRef` | `polyLine: Polyline3D * inPlaneOffsetDistance: float * perpendicularOffsetDistance: float * refNormal: UnitVec * ?loop: bool -> Polyline3D` |
| `Polyline3D.removeDuplicateAndCollinearPoints` | `angleTolerance: float<cosine> -> distanceTolerance: float -> pl: Polyline3D -> unit` |
| `Polyline3D.removeDuplicatePoints` | `distanceTolerance: float -> pl: Polyline3D -> unit` |
| `Polyline3D.removeDuplicatePointsAndReport` | `distanceTolerance: float -> pl: Polyline3D -> ResizeArray<Pnt>` |
| `Polyline3D.removeDuplicatePointsFaithfully` | `distanceTolerance: float -> pl: Polyline3D -> unit` |
| `Polyline3D.removeUTurns` | `minCos: float<cosine> -> polyLine: Polyline3D -> unit` |
| `Polyline3D.removeUTurnsDeeply` | `minCos: float<cosine> -> polyLine: Polyline3D -> unit` |
| `Polyline3D.subPolyline` | `a: float -> b: float -> pl: Polyline3D -> Polyline3D` |
| `Polyline3D.translate` | `v: Vec -> pl: Polyline3D -> unit` |
| `Polyline3D.tryFind` | `condition: (float -> float -> float -> bool) -> pl: Polyline3D -> Pnt option` |
| `Polyline3D.tryFindIndex` | `condition: (float -> float -> float -> bool) -> pl: Polyline3D -> int option` |
| `Polyline3D.tryFindLast` | `condition: (float -> float -> float -> bool) -> pl: Polyline3D -> Pnt option` |
| `Polyline3D.tryFindLastIndex` | `condition: (float -> float -> float -> bool) -> pl: Polyline3D -> int option` |

## Topology3D

| Topology3D | Signature |
|:--|:--|
| `Topology3D.join` | `getLine: ('T -> Line3D) * splitDistance: float * xs: ResizeArray<'T> -> ResizeArray<ResizeArray<'T>>` |
| `Topology3D.joinReversing` | `getLine: ('T -> Line3D) * splitDistance: float * xs: ResizeArray<'T> -> ResizeArray<ResizeArray<'T * bool>>` |
| `Topology3D.sortToLoop` | `getLine: ('T -> Line3D) * ts: ResizeArray<'T> -> unit` |
| `Topology3D.sortToLoopCached` | `getLine: ('T -> Line3D) * ts: ResizeArray<'T> -> float array` |
| `Topology3D.sortToLoopWithReversing` | `getLine: ('T -> Line3D) * reverseInPlace: (int -> 'T -> unit) * ts: ResizeArray<'T> -> unit` |
| `Topology3D.sortToLoopWithReversingCached` | `getLine: ('T -> Line3D) * reverseInPlace: (int -> 'T -> unit) * ts: ResizeArray<'T> -> float array` |
