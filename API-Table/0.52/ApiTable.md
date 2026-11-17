# Euclid API

All public types, members and functions of Euclid. Corresponding 2D and 3D types share one table.

- Corresponding members share a row, also when their names differ, like `Pt.midPt` and `Pnt.midPnt`.
- `—` and a reason in italics mark a member that exists in one dimension only on purpose.
- An empty cell marks one of 91 gaps: a member that is missing in one dimension,
  or a member whose name does not match its counterpart. The contents list the gaps per table.
- Instance members and fields start with `.`, e.g. `.DistanceTo`.
- Static members are qualified with their type name. They are listed right below the instance member of the same name.
  The static members without such an instance member follow at the end of each table.
- A property shows only its type. `with get, set` marks a property that can be set. `?name` marks an optional parameter.

Generated from the sources by `API-Table/ApiTable.fsx`. Do not edit by hand.

## Contents

**2D and 3D**

- [Vc / Vec](#vc--vec), 9 gaps
- [UnitVc / UnitVec](#unitvc--unitvec), 8 gaps
- [Pt / Pnt](#pt--pnt), 8 gaps
- [Rotation2D / Quaternion](#rotation2d--quaternion), 19 gaps
- [PPlane](#pplane)
- [Matrix](#matrix)
- [RigidMatrix](#rigidmatrix)
- [Line2D / Line3D](#line2d--line3d), 0 gaps
- [XLine2D.XRayParam / XLine3D.XRayParam](#xline2dxrayparam--xline3dxrayparam), 0 gaps
- [XLine2D.XRay / XLine3D.XRay](#xline2dxray--xline3dxray), 0 gaps
- [XLine2D.XParam / XLine3D.XParam](#xline2dxparam--xline3dxparam), 0 gaps
- [XLine2D.XPt / XLine3D.XPnt](#xline2dxpt--xline3dxpnt), 0 gaps
- [XLine2D.ClParams / XLine3D.ClParams](#xline2dclparams--xline3dclparams), 0 gaps
- [XLine2D.ClPts](#xline2dclpts)
- [XLine2D.XEnds / XLine3D.XEnds](#xline2dxends--xline3dxends), 0 gaps
- [module XLineXY / module XLineXYZ](#module-xlinexy--module-xlinexyz), 3 gaps
- [XLine2D / XLine3D](#xline2d--xline3d), 6 gaps
- [XLine3D.XCone](#xline3dxcone)
- [XLine3D.ClPts](#xline3dclpts)
- [NPlane](#nplane)
- [Rect2D / Rect3D](#rect2d--rect3d), 10 gaps
- [Box](#box)
- [FreeBox](#freebox)
- [BRect / BBox](#brect--bbox), 12 gaps
- [Tria2D / Tria3D](#tria2d--tria3d), 3 gaps
- [Points2D / Points3D](#points2d--points3d), 0 gaps
- [module Offset2D / module Offset3D](#module-offset2d--module-offset3d), 6 gaps
- [Offset2D.UTurn](#offset2duturn)
- [Offset2D.VarDistParallel / Offset3D.VarDistParallel](#offset2dvardistparallel--offset3dvardistparallel), 0 gaps
- [Offset3D.OffsetDirection](#offset3doffsetdirection)
- [Offset3D.CollinearPoint](#offset3dcollinearpoint)
- [Polyline2D / Polyline3D](#polyline2d--polyline3d), 7 gaps
- [Topology2D / Topology3D](#topology2d--topology3d), 0 gaps
- [module Similarity2D](#module-similarity2d)
- [Similarity2D.GroupInsideObjectToCheck](#similarity2dgroupinsideobjecttocheck)
- [Similarity2D.ObjectToCheck](#similarity2dobjecttocheck)

**General**

- [module EuclidErrors](#module-eucliderrors)
- [EuclidErrors.EuclidException](#eucliderrorseuclidexception)
- [EuclidErrors.EuclidDivByZeroException](#eucliderrorseucliddivbyzeroexception)
- [EuclidErrors.EuclidUnitizingException](#eucliderrorseuclidunitizingexception)
- [EuclidErrors.EuclidNanInfinityException](#eucliderrorseuclidnaninfinityexception)
- [EuclidErrors.EuclidTooSmallException](#eucliderrorseuclidtoosmallexception)
- [EuclidErrors.EuclidNullException](#eucliderrorseuclidnullexception)
- [EuclidErrors.EuclidEmptySeqException](#eucliderrorseuclidemptyseqexception)
- [EuclidErrors.EuclidObsoleteException](#eucliderrorseuclidobsoleteexception)
- [module UtilEuclid](#module-utileuclid)
- [module Cosine](#module-cosine)
- [module Tangent](#module-tangent)
- [module Format](#module-format)
- [module Format.ToMaxDigits](#module-formattomaxdigits)
- [module EuclidCollectionUtilities.Arr](#module-euclidcollectionutilitiesarr)
- [module EuclidCollectionUtilities.ResizeArr](#module-euclidcollectionutilitiesresizearr)
- [ResizeArray extensions](#resizearray-extensions)

## Vc / Vec

| Vc | Signature | Vec | Signature |
|:--|:--|:--|:--|
| `new` | `x: float * y: float -> Vc` | `new` | `x: float * y: float * z: float -> Vec` |
| `.X` | `float` | `.X` | `float` |
| `.Y` | `float` | `.Y` | `float` |
| — | *Z axis* | `.Z` | `float` |
| `.AngleDiamondTo` | `b: Vc -> float` | `.AngleDiamondInXYTo` | `b: Vec -> float` |
| `.AsFSharpCode` | `string` | `.AsFSharpCode` | `string` |
| `.AsPt` | `Pt` | `.AsPnt` | `Pnt` |
| `Vc.asPt` | `v: Vc -> Pt` | `Vec.asPnt` | `v: Vec -> Pnt` |
| `.AsPnt` | `Pnt` | `.AsPt` | `Pt` |
| `Vc.asPnt` | `v: Vc -> Pnt` | `Vec.asPt` | `v: Vec -> Pt` |
| `.AsString` | `string` | `.AsString` | `string` |
| `.AsVec` | `Vec` | `.AsVc` | `Vc` |
| `Vc.asVec` | `v: Vc -> Vec` | `Vec.asVc` | `v: Vec -> Vc` |
| `.Cross` | `b: Vc -> float` | `.Cross` | `b: UnitVec -> Vec` |
| `.Cross` | `b: UnitVc -> float` | `.Cross` | `b: Vec -> Vec` |
| `Vc.cross` | `a: Vc * b: Vc -> float` | `Vec.cross` | `a: Vec * b: Vec -> Vec` |
| `Vc.cross` | `a: UnitVc * b: Vc -> float` | `Vec.cross` | `a: Vec * b: UnitVec -> Vec` |
| `Vc.cross` | `a: Vc * b: UnitVc -> float` | `Vec.cross` | `a: UnitVec * b: Vec -> Vec` |
| `.Direction180` | `float` | `.Direction180InXY` | `float` |
| `Vc.direction180` | `v: Vc -> float` | `Vec.direction180InXY` | `v: Vec -> float` |
| `.Direction2Pi` | `float` | `.Direction2PiInXY` | `float` |
| `Vc.direction2Pi` | `v: Vc -> float` | `Vec.direction2PiInXY` | `v: Vec -> float` |
| `.Direction360` | `float` | `.Direction360InXY` | `float` |
| `Vc.direction360` | `v: Vc -> float` | `Vec.direction360InXY` | `v: Vec -> float` |
| `.DirectionDiamond` | `float` | `.DirectionDiamondInXY` | `float` |
| `Vc.directionDiamond` | `a: Vc -> float` | `Vec.directionDiamondInXY` | `v: Vec -> float` |
| `.DirectionPi` | `float` | `.DirectionPiInXY` | `float` |
| `Vc.directionPi` | `v: Vc -> float` | `Vec.directionPiInXY` | `v: Vec -> float` |
| `.Dot` | `b: Vc -> float` | `.Dot` | `b: Vec -> float` |
| `.Dot` | `b: UnitVc -> float` | `.Dot` | `b: UnitVec -> float` |
| `Vc.dot` | `a: Vc * b: Vc -> float` | `Vec.dot` | `a: Vec * b: Vec -> float` |
| `Vc.dot` | `a: Vc * b: UnitVc -> float` | `Vec.dot` | `a: Vec * b: UnitVec -> float` |
| `Vc.dot` | `a: UnitVc * b: Vc -> float` | `Vec.dot` | `a: UnitVec * b: Vec -> float` |
| `.Half` | `Vc` | `.Half` | `Vec` |
| `Vc.half` | `v: Vc -> Vc` | `Vec.half` | `v: Vec -> Vec` |
| — | *vertical direction* | `.IsHorizontal` | `bool` |
| — | *vertical direction* | `Vec.isHorizontal` | `v: Vec -> bool` |
| `.IsNotZero` | `bool` | `.IsNotZero` | `bool` |
| `Vc.isNotZero` | `v: Vc -> bool` | `Vec.isNotZero` | `v: Vec -> bool` |
| `.IsOppositeOrientation` | `other: Vc -> bool` | `.IsOppositeOrientation` | `other: Vec -> bool` |
| `.IsOppositeOrientation` | `other: UnitVc -> bool` | `.IsOppositeOrientation` | `other: UnitVec -> bool` |
| `Vc.isOppositeOrientation` | `v: Vc -> other: Vc -> bool` | `Vec.isOppositeOrientation` | `v: Vec -> other: Vec -> bool` |
| `.IsParallelAndOrientedTo` | `other: Vc * ?minCosine: float<cosine> -> bool` | `.IsParallelAndOrientedTo` | `other: Vec * ?minCosine: float<cosine> -> bool` |
| `.IsParallelAndOrientedTo` | `other: UnitVc * ?minCosine: float<cosine> -> bool` | `.IsParallelAndOrientedTo` | `other: UnitVec * ?minCosine: float<cosine> -> bool` |
| `Vc.isParallelAndOrientedTo` | `minCosine: float<cosine> -> a: Vc -> b: Vc -> bool` | `Vec.isParallelAndOrientedTo` | `minCosine: float<cosine> -> a: Vec -> b: Vec -> bool` |
| `.IsParallelTo` | `other: Vc * ?minCosine: float<cosine> -> bool` | `.IsParallelTo` | `other: Vec * ?minCosine: float<cosine> -> bool` |
| `.IsParallelTo` | `other: UnitVc * ?minCosine: float<cosine> -> bool` | `.IsParallelTo` | `other: UnitVec * ?minCosine: float<cosine> -> bool` |
| `Vc.isParallelTo` | `minCosine: float<cosine> -> a: Vc -> b: Vc -> bool` | `Vec.isParallelTo` | `minCosine: float<cosine> -> a: Vec -> b: Vec -> bool` |
| `.IsPerpendicularTo` | `other: Vc * ?maxCosine: float<cosine> -> bool` | `.IsPerpendicularTo` | `other: Vec * ?maxCosine: float<cosine> -> bool` |
| `.IsPerpendicularTo` | `other: UnitVc * ?maxCosine: float<cosine> -> bool` | `.IsPerpendicularTo` | `other: UnitVec * ?maxCosine: float<cosine> -> bool` |
| `Vc.isPerpendicularTo` | `maxCosine: float<cosine> -> a: Vc -> b: Vc -> bool` | `Vec.isPerpendicularTo` | `maxCosine: float<cosine> -> a: Vec -> b: Vec -> bool` |
| `.IsTiny` | `tol: float -> bool` | `.IsTiny` | `tol: float -> bool` |
| `Vc.isTiny` | `tol: float -> v: Vc -> bool` | `Vec.isTiny` | `tol: float -> v: Vec -> bool` |
| `.IsTinySq` | `tol: float -> bool` | `.IsTinySq` | `tol: float -> bool` |
| `Vc.isTinySq` | `tol: float -> v: Vc -> bool` | `Vec.isTinySq` | `tol: float -> v: Vec -> bool` |
| `.IsUnit` | `bool` | `.IsUnit` | `bool` |
| `Vc.isUnit` | `v: Vc -> bool` | `Vec.isUnit` | `v: Vec -> bool` |
| — | *vertical direction* | `.IsVertical` | `bool` |
| — | *vertical direction* | `Vec.isVertical` | `v: Vec -> bool` |
| `.IsXAligned` | `bool` | `.IsXAligned` | `bool` |
| `Vc.isXAligned` | `v: Vc -> bool` | `Vec.isXAligned` | `v: Vec -> bool` |
| `.IsYAligned` | `bool` | `.IsYAligned` | `bool` |
| `Vc.isYAligned` | `v: Vc -> bool` | `Vec.isYAligned` | `v: Vec -> bool` |
| — | *Z axis* | `.IsZAligned` | `bool` |
| — | *Z axis* | `Vec.isZAligned` | `v: Vec -> bool` |
| `.IsZero` | `bool` | `.IsZero` | `bool` |
| `Vc.isZero` | `v: Vc -> bool` | `Vec.isZero` | `v: Vec -> bool` |
| `.Length` | `float` | `.Length` | `float` |
| `Vc.length` | `v: Vc -> float` | `Vec.length` | `v: Vec -> float` |
| — | *projection to the XY plane* | `.LengthInXY` | `float` |
| — | *projection to the XY plane* | `Vec.lengthInXY` | `v: Vec -> float` |
| `.LengthSq` | `float` | `.LengthSq` | `float` |
| `Vc.lengthSq` | `v: Vc -> float` | `Vec.lengthSq` | `v: Vec -> float` |
| — | *projection to the XY plane* | `.LengthSqInXY` | `float` |
| — | *projection to the XY plane* | `Vec.lengthSqInXY` | `v: Vec -> float` |
| `.MatchesOrientation` | `other: Vc -> bool` | `.MatchesOrientation` | `other: Vec -> bool` |
| `.MatchesOrientation` | `other: UnitVc -> bool` | `.MatchesOrientation` | `other: UnitVec -> bool` |
| `Vc.matchesOrientation` | `v: Vc -> other: Vc -> bool` | `Vec.matchesOrientation` | `v: Vec -> other: Vec -> bool` |
| — | *projection to the XY plane* | `.PerpendicularInXY` | `Vec` |
| — | *projection to the XY plane* | `Vec.perpendicularInXY` | `v: Vec -> Vec` |
| `.Rotate` | `angDegree: float -> Vc` |  |  |
| `Vc.rotate` | `angDegree: float -> vec: Vc -> Vc` | `Vec.rotate` | `q: Quaternion -> pt: Vec -> Vec` |
| `.Rotate90CCW` | `Vc` | — | *orientation in the plane* |
| `Vc.rotate90CCW` | `v: Vc -> Vc` | — | *orientation in the plane* |
| `.Rotate90CW` | `Vc` |  |  |
| `Vc.rotate90CW` | `v: Vc -> Vc` |  |  |
| `.RotateBy` | `r: Rotation2D -> Vc` |  |  |
| `Vc.rotateBy` | `r: Rotation2D -> v: Vc -> Vc` |  |  |
| `.RotateByQuarterCircle` | `numberOfQuarters: int -> Vc` | `.RotateByQuarterCircle` | `numberOfQuarters: int -> Vec` |
| `Vc.rotateByQuarterCircle` | `numberOfQuarters: int -> v: Vc -> Vc` | `Vec.rotateByQuarterCircle` | `numberOfQuarters: int -> v: Vec -> Vec` |
| — | *rotation about a world axis* | `.RotateOnZ90CCW` | `Vec` |
| — | *rotation about a world axis* | `Vec.rotateOnZ90CCW` | `v: Vec -> Vec` |
| — | *rotation about a world axis* | `.RotateOnZ90CW` | `Vec` |
| — | *rotation about a world axis* | `Vec.rotateOnZ90CW` | `v: Vec -> Vec` |
| `.RotateRadians` | `angRadians: float -> Vc` | — | *a 3D rotation also needs an axis* |
| `Vc.rotateRadians` | `angRadians: float -> v: Vc -> Vc` | — | *a 3D rotation also needs an axis* |
| `.ToString` | `unit -> string` | `.ToString` | `unit -> string` |
| — | *transformation matrix* | `.TransformRigid` | `m: RigidMatrix -> Vec` |
| — | *transformation matrix* | `Vec.transformRigid` | `m: RigidMatrix -> v: Vec -> Vec` |
| `.Unitized` | `UnitVc` | `.Unitized` | `UnitVec` |
| `.WithLength` | `desiredLength: float -> Vc` | `.WithLength` | `desiredLength: float -> Vec` |
| `Vc.withLength` | `desiredLength: float -> v: Vc -> Vc` | `Vec.withLength` | `desiredLength: float -> v: Vec -> Vec` |
| `.WithX` | `x: float -> Vc` | `.WithX` | `x: float -> Vec` |
| `Vc.withX` | `x: float -> v: Vc -> Vc` | `Vec.withX` | `x: float -> v: Vec -> Vec` |
| `.WithY` | `y: float -> Vc` | `.WithY` | `y: float -> Vec` |
| `Vc.withY` | `y: float -> v: Vc -> Vc` | `Vec.withY` | `y: float -> v: Vec -> Vec` |
| `.WithZ` | `z: float -> Vec` | `.WithZ` | `z: float -> Vec` |
| `Vc.withZ` | `z: float -> v: Vc -> Vec` | `Vec.withZ` | `z: float -> v: Vec -> Vec` |
| **Other static members** | | | |
| `Vc.add` | `a: Vc -> b: Vc -> Vc` | `Vec.add` | `a: Vec -> b: Vec -> Vec` |
| `Vc.addX` | `x: float -> v: Vc -> Vc` | `Vec.addX` | `x: float -> v: Vec -> Vec` |
| `Vc.addY` | `y: float -> v: Vc -> Vc` | `Vec.addY` | `y: float -> v: Vec -> Vec` |
| — | *Z axis* | `Vec.addZ` | `z: float -> v: Vec -> Vec` |
| `Vc.angle180` | `a: Vc -> b: Vc -> float` | `Vec.angle180` | `a: Vec -> b: Vec -> float` |
| `Vc.angle2Pi` | `a: Vc * b: Vc -> float` | `Vec.angle2PiInXY` | `a: Vec * b: Vec -> float` |
| `Vc.angle360` | `a: Vc * b: Vc -> float` | `Vec.angle360InXY` | `a: Vec * b: Vec -> float` |
| `Vc.angle90` | `a: Vc -> b: Vc -> float` | `Vec.angle90` | `a: Vec -> b: Vec -> float` |
| `Vc.angleDiamond` | `a: Vc * b: Vc -> float` | `Vec.angleDiamondInXY` | `a: Vec * b: Vec -> float` |
| `Vc.angleHalfPi` | `a: Vc -> b: Vc -> float` | `Vec.angleHalfPi` | `a: Vec -> b: Vec -> float` |
| `Vc.anglePi` | `a: Vc -> b: Vc -> float` | `Vec.anglePi` | `a: Vec -> b: Vec -> float` |
| `Vc.angleToX180` | `v: Vc -> float` | — | *signed angle in a fixed 2D plane* |
| `Vc.angleToXPi` | `vc: Vc -> float` | — | *signed angle in a fixed 2D plane* |
| `Vc.bisector` | `a: Vc -> b: Vc -> Vc` | `Vec.bisector` | `a: Vec -> b: Vec -> Vec` |
| `Vc.create` | `x: float * y: float -> Vc` | `Vec.create` | `x: float * y: float * z: float -> Vec` |
| `Vc.create` | `start: Pt * endValue: Pt -> Vc` | `Vec.create` | `start: Pnt * endValue: Pnt -> Vec` |
| `Vc.createFromMembersXY` | `vec: ^T -> Vc` | `Vec.createFromMembersXYZ` | `vec: ^T -> Vec` |
| `Vc.createFromMembersxy` | `vec: ^T -> Vc` | `Vec.createFromMembersxyz` | `vec: ^T -> Vec` |
| `Vc.createFromPnt` | `pt: Pnt -> Vc` | `Vec.createFromPnt` | `pt: Pnt -> Vec` |
| `Vc.createFromUnitVc` | `v: UnitVc -> Vc` | `Vec.createFromUnitVec` | `v: UnitVec -> Vec` |
| — | *Z axis* | `Vec.createFromVcWithZ` | `z: float -> v: Vc -> Vec` |
| — | *triple product* | `Vec.determinant3` | `u: Vec * v: Vec * w: Vec -> float` |
| `Vc.difference` | `a: Vc -> b: Vc -> float` | `Vec.difference` | `a: Vec -> b: Vec -> float` |
| `Vc.differenceSq` | `a: Vc -> b: Vc -> float` | `Vec.differenceSq` | `a: Vec -> b: Vec -> float` |
| `Vc.DivideByInt` | `v: Vc * i: int -> Vc` | `Vec.DivideByInt` | `v: Vec * i: int -> Vec` |
| `Vc.equals` | `tol: float -> a: Vc -> b: Vc -> bool` | `Vec.equals` | `tol: float -> a: Vec -> b: Vec -> bool` |
| `Vc.flip` | `v: Vc -> Vc` | `Vec.flip` | `v: Vec -> Vec` |
| — | *vertical direction* | `Vec.flipToPointUp` | `v: Vec -> Vec` |
| `Vc.getX` | `v: Vc -> float` | `Vec.getX` | `v: Vec -> float` |
| `Vc.getY` | `v: Vc -> float` | `Vec.getY` | `v: Vec -> float` |
| — | *Z axis* | `Vec.getZ` | `v: Vec -> float` |
| `Vc.isAngleAbove` | `cosineValue: float<cosine> -> a: Vc -> b: Vc -> bool` | `Vec.isAngleAbove` | `cosineValue: float<cosine> -> a: Vec -> b: Vec -> bool` |
| `Vc.isAngleBelow` | `cosineValue: float<cosine> -> a: Vc -> b: Vc -> bool` | `Vec.isAngleBelow` | `cosineValue: float<cosine> -> a: Vec -> b: Vec -> bool` |
| `Vc.isNotParallelWithin` | `cosineValue: float<cosine> -> a: Vc -> b: Vc -> bool` | `Vec.isNotParallelWithin` | `cosineValue: float<cosine> -> a: Vec -> b: Vec -> bool` |
|  |  | `Vec.isParallelAndOrientedToUnitVec` | `minCosine: float<cosine> -> b: UnitVec -> a: Vec -> bool` |
|  |  | `Vec.isParallelToUnitVec` | `minCosine: float<cosine> -> b: UnitVec -> a: Vec -> bool` |
| `Vc.isParallelWithin` | `cosineValue: float<cosine> -> a: Vc -> b: Vc -> bool` | `Vec.isParallelWithin` | `cosineValue: float<cosine> -> a: Vec -> b: Vec -> bool` |
|  |  | `Vec.isPerpendicularToUnitVec` | `maxCosine: float<cosine> -> b: UnitVec -> a: Vec -> bool` |
| `Vc.lerp` | `start: Vc * endValue: Vc * rel: float -> Vc` | `Vec.lerp` | `start: Vec * endValue: Vec * rel: float -> Vec` |
| `Vc.matchOrientation` | `orientationToMatch: Vc -> vecToFlip: Vc -> Vc` | `Vec.matchOrientation` | `orientationToMatch: Vec -> vecToFlip: Vec -> Vec` |
| `Vc.matchUnitVcOrientation` | `orientationToMatch: UnitVc -> vecToFlip: Vc -> Vc` | `Vec.matchUnitVecOrientation` | `orientationToMatch: UnitVec -> vecToFlip: Vec -> Vec` |
| `Vc.notEquals` | `tol: float -> a: Vc -> b: Vc -> bool` | `Vec.notEquals` | `tol: float -> a: Vec -> b: Vec -> bool` |
| — | *vertical direction* | `Vec.orientDown` | `v: Vec -> Vec` |
| — | *vertical direction* | `Vec.orientUp` | `v: Vec -> Vec` |
| — | *vertical direction* | `Vec.perpendicularInVerticalPlane` | `v: Vec -> Vec` |
| — | *projection to the XY plane* | `Vec.projectToXYPlane` | `v: Vec -> Vec` |
| `Vc.reverse` | `v: Vc -> Vc` | `Vec.reverse` | `v: Vec -> Vec` |
| — | *rotation about a world axis* | `Vec.rotateOnX` | `r: Rotation2D -> v: Vec -> Vec` |
| — | *rotation about a world axis* | `Vec.rotateOnXDeg` | `angDegree: float -> v: Vec -> Vec` |
| — | *rotation about a world axis* | `Vec.rotateOnY` | `r: Rotation2D -> v: Vec -> Vec` |
| — | *rotation about a world axis* | `Vec.rotateOnYDeg` | `angDegree: float -> v: Vec -> Vec` |
| — | *rotation about a world axis* | `Vec.rotateOnZ` | `r: Rotation2D -> v: Vec -> Vec` |
| — | *rotation about a world axis* | `Vec.rotateOnZDeg` | `angDegree: float -> v: Vec -> Vec` |
| `Vc.scale` | `f: float -> v: Vc -> Vc` | `Vec.scale` | `f: float -> v: Vec -> Vec` |
| `Vc.slerp` | `start: Vc * endValue: Vc * rel: float -> Vc` | `Vec.slerp` | `start: Vec * endValue: Vec * rel: float -> Vec` |
| `Vc.slopeDegrees` | `v: Vc -> float` | `Vec.slopeDegrees` | `v: Vec -> float` |
| `Vc.slopePercent` | `v: Vc -> float` | `Vec.slopePercent` | `v: Vec -> float` |
| `Vc.slopeRadians` | `v: Vc -> float` | `Vec.slopeRadians` | `v: Vec -> float` |
| `Vc.unitize` | `v: Vc -> UnitVc` | `Vec.unitize` | `v: Vec -> UnitVec` |
| `Vc.unitizeAsVc` | `v: Vc -> Vc` | `Vec.unitizeAsVec` | `v: Vec -> Vec` |
| `Vc.unitizeOrDefault` | `defaultUnitVector: UnitVc -> v: Vc -> UnitVc` | `Vec.unitizeOrDefault` | `defaultUnitVector: UnitVec -> v: Vec -> UnitVec` |
| `Vc.unitizeUnchecked` | `v: Vc -> UnitVc` | `Vec.unitizeUnchecked` | `v: Vec -> UnitVec` |
| `Vc.unitizeWithErrMsg` | `errMsg: string -> v: Vc -> UnitVc` | `Vec.unitizeWithErrMsg` | `errMsg: string -> v: Vec -> UnitVec` |
| `Vc.Xaxis` | `Vc` | `Vec.Xaxis` | `Vec` |
| `Vc.Yaxis` | `Vc` | `Vec.Yaxis` | `Vec` |
|  |  | `Vec.Zaxis` | `Vec` |
| `Vc.Zero` | `Vc` | `Vec.Zero` | `Vec` |
| `( *** )` | `a: Vc * b: Vc -> float` | `( *** )` | `a: Vec * b: Vec -> float` |
| `( * )` | `v: Vc * f: float -> Vc` | `( * )` | `a: Vec * f: float -> Vec` |
| `( * )` | `f: float * v: Vc -> Vc` | `( * )` | `f: float * a: Vec -> Vec` |
| `( + )` | `a: Vc * b: Vc -> Vc` | `( + )` | `a: Vec * b: Vec -> Vec` |
| `( - )` | `a: Vc * b: Vc -> Vc` | `( - )` | `a: Vec * b: Vec -> Vec` |
| `( / )` | `v: Vc * f: float -> Vc` | `( / )` | `v: Vec * f: float -> Vec` |
| `( ~- )` | `v: Vc -> Vc` | `( ~- )` | `v: Vec -> Vec` |

## UnitVc / UnitVec

| UnitVc | Signature | UnitVec | Signature |
|:--|:--|:--|:--|
| `.X` | `float` | `.X` | `float` |
| `.Y` | `float` | `.Y` | `float` |
| — | *Z axis* | `.Z` | `float` |
| `.AngleDiamondTo` | `b: UnitVc -> float` | `.AngleDiamondInXYTo` | `b: UnitVec -> float` |
| `.AsFSharpCode` | `string` | `.AsFSharpCode` | `string` |
| `.AsPt` | `Pt` | `.AsPnt` | `Pnt` |
| `UnitVc.asPt` | `v: UnitVc -> Pt` | `UnitVec.asPnt` | `v: UnitVec -> Pnt` |
| `.AsPnt` | `Pnt` | `.AsPt` | `Pt` |
| `UnitVc.asPnt` | `v: UnitVc -> Pnt` | `UnitVec.asPt` | `v: UnitVec -> Pt` |
| `.AsString` | `string` | `.AsString` | `string` |
| `.AsUnitVec` | `UnitVec` | `.AsUnitVc` | `UnitVc` |
| `UnitVc.asUnitVec` | `v: UnitVc -> UnitVec` | `UnitVec.asUnitVc` | `v: UnitVec -> UnitVc` |
| `.AsVec` | `Vec` | `.AsVc` | `Vc` |
| `UnitVc.asVec` | `v: UnitVc -> Vec` | `UnitVec.asVc` | `v: UnitVec -> Vc` |
| `.AsVc` | `Vc` | `.AsVec` | `Vec` |
| `UnitVc.asVc` | `v: UnitVc -> Vc` | `UnitVec.asVec` | `v: UnitVec -> Vec` |
| `.Cross` | `b: Vc -> float` | `.Cross` | `b: UnitVec -> Vec` |
| `.Cross` | `b: UnitVc -> float` | `.Cross` | `b: Vec -> Vec` |
| `UnitVc.cross` | `a: UnitVc * b: UnitVc -> float` | `UnitVec.cross` | `a: UnitVec * b: UnitVec -> Vec` |
| `UnitVc.cross` | `a: UnitVc * b: Vc -> float` | `UnitVec.cross` | `a: UnitVec * b: Vec -> Vec` |
| `UnitVc.cross` | `a: Vc * b: UnitVc -> float` | `UnitVec.cross` | `a: Vec * b: UnitVec -> Vec` |
| `.Direction180` | `float` | `.Direction180InXY` | `float` |
| `UnitVc.direction180` | `v: UnitVc -> float` | `UnitVec.direction180InXY` | `v: UnitVec -> float` |
| `.Direction2Pi` | `float` | `.Direction2PiInXY` | `float` |
| `UnitVc.direction2Pi` | `v: UnitVc -> float` | `UnitVec.direction2PiInXY` | `v: UnitVec -> float` |
| `.Direction360` | `float` | `.Direction360InXY` | `float` |
| `UnitVc.direction360` | `v: UnitVc -> float` | `UnitVec.direction360InXY` | `v: UnitVec -> float` |
| `.DirectionDiamond` | `float` | `.DirectionDiamondInXY` | `float` |
| `UnitVc.directionDiamond` | `a: UnitVc -> float` | `UnitVec.directionDiamondInXY` | `v: UnitVec -> float` |
| `.DirectionPi` | `float` | `.DirectionPiInXY` | `float` |
| `UnitVc.directionPi` | `v: UnitVc -> float` | `UnitVec.directionPiInXY` | `v: UnitVec -> float` |
| `.Dot` | `b: UnitVc -> float` | `.Dot` | `b: UnitVec -> float` |
| `.Dot` | `b: Vc -> float` | `.Dot` | `b: Vec -> float` |
| `UnitVc.dot` | `a: UnitVc * b: UnitVc -> float` | `UnitVec.dot` | `a: UnitVec * b: UnitVec -> float` |
| `UnitVc.dot` | `a: UnitVc * b: Vc -> float` | `UnitVec.dot` | `a: UnitVec * b: Vec -> float` |
| `UnitVc.dot` | `a: Vc * b: UnitVc -> float` | `UnitVec.dot` | `a: Vec * b: UnitVec -> float` |
| `.DotCosine` | `b: UnitVc -> float<cosine>` | `.DotCosine` | `b: UnitVec -> float<cosine>` |
| `UnitVc.dotCosine` | `a: UnitVc -> b: UnitVc -> float<cosine>` | `UnitVec.dotCosine` | `a: UnitVec -> b: UnitVec -> float<cosine>` |
| — | *vertical direction* | `.IsHorizontal` | `bool` |
| — | *vertical direction* | `UnitVec.isHorizontal` | `v: UnitVec -> bool` |
| `.IsOppositeOrientation` | `other: UnitVc -> bool` | `.IsOppositeOrientation` | `other: UnitVec -> bool` |
| `.IsOppositeOrientation` | `other: Vc -> bool` | `.IsOppositeOrientation` | `other: Vec -> bool` |
| `UnitVc.isOppositeOrientation` | `other: UnitVc -> v: UnitVc -> bool` | `UnitVec.isOppositeOrientation` | `other: UnitVec -> v: UnitVec -> bool` |
| `.IsParallelAndOrientedTo` | `other: UnitVc * ?minCosine: float<cosine> -> bool` | `.IsParallelAndOrientedTo` | `other: UnitVec * ?minCosine: float<cosine> -> bool` |
| `.IsParallelAndOrientedTo` | `other: Vc * ?minCosine: float<cosine> -> bool` | `.IsParallelAndOrientedTo` | `other: Vec * ?minCosine: float<cosine> -> bool` |
| `UnitVc.isParallelAndOrientedTo` | `minCosine: float<cosine> -> a: UnitVc -> b: UnitVc -> bool` | `UnitVec.isParallelAndOrientedTo` | `minCosine: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `.IsParallelTo` | `other: UnitVc * ?minCosine: float<cosine> -> bool` | `.IsParallelTo` | `other: UnitVec * ?minCosine: float<cosine> -> bool` |
| `.IsParallelTo` | `other: Vc * ?minCosine: float<cosine> -> bool` | `.IsParallelTo` | `other: Vec * ?minCosine: float<cosine> -> bool` |
| `UnitVc.isParallelTo` | `minCosine: float<cosine> -> a: UnitVc -> b: UnitVc -> bool` | `UnitVec.isParallelTo` | `minCosine: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `.IsPerpendicularTo` | `other: UnitVc * ?maxCosine: float<cosine> -> bool` | `.IsPerpendicularTo` | `other: UnitVec * ?maxCosine: float<cosine> -> bool` |
| `.IsPerpendicularTo` | `other: Vc * ?maxCosine: float<cosine> -> bool` | `.IsPerpendicularTo` | `other: Vec * ?maxCosine: float<cosine> -> bool` |
| `UnitVc.isPerpendicularTo` | `maxCosine: float<cosine> -> a: UnitVc -> b: UnitVc -> bool` | `UnitVec.isPerpendicularTo` | `maxCosine: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| — | *vertical direction* | `.IsVertical` | `bool` |
| — | *vertical direction* | `UnitVec.isVertical` | `v: UnitVec -> bool` |
| `.IsXAligned` | `bool` | `.IsXAligned` | `bool` |
| `UnitVc.isXAligned` | `v: UnitVc -> bool` | `UnitVec.isXAligned` | `v: UnitVec -> bool` |
| `.IsYAligned` | `bool` | `.IsYAligned` | `bool` |
| `UnitVc.isYAligned` | `v: UnitVc -> bool` | `UnitVec.isYAligned` | `v: UnitVec -> bool` |
| — | *Z axis* | `.IsZAligned` | `bool` |
| — | *Z axis* | `UnitVec.isZAligned` | `v: UnitVec -> bool` |
| — | *projection to the XY plane* | `.LengthInXY` | `float` |
| — | *projection to the XY plane* | `UnitVec.lengthInXY` | `v: UnitVec -> float` |
| — | *projection to the XY plane* | `.LengthSqInXY` | `float` |
| — | *projection to the XY plane* | `UnitVec.lengthSqInXY` | `v: UnitVec -> float` |
| `.MatchesOrientation` | `other: UnitVc -> bool` | `.MatchesOrientation` | `other: UnitVec -> bool` |
| `.MatchesOrientation` | `other: Vc -> bool` | `.MatchesOrientation` | `other: Vec -> bool` |
| `UnitVc.matchesOrientation` | `other: UnitVc -> v: UnitVc -> bool` | `UnitVec.matchesOrientation` | `other: UnitVec -> v: UnitVec -> bool` |
| — | *projection to the XY plane* | `.PerpendicularInXY` | `Vec` |
| — | *projection to the XY plane* | `UnitVec.perpendicularInXY` | `v: UnitVec -> Vec` |
| `.Rotate` | `angDegree: float -> UnitVc` |  |  |
| `UnitVc.rotate` | `angDegree: float -> vec: UnitVc -> UnitVc` | `UnitVec.rotate` | `q: Quaternion -> pt: UnitVec -> UnitVec` |
| `.Rotate90CCW` | `UnitVc` | — | *orientation in the plane* |
| `UnitVc.rotate90CCW` | `v: UnitVc -> UnitVc` | — | *orientation in the plane* |
| `.Rotate90CW` | `UnitVc` |  |  |
| `UnitVc.rotate90CW` | `v: UnitVc -> UnitVc` |  |  |
| `.RotateBy` | `r: Rotation2D -> UnitVc` |  |  |
| `UnitVc.rotateBy` | `r: Rotation2D -> v: UnitVc -> UnitVc` |  |  |
| `.RotateByQuarterCircle` | `numberOfQuarters: int -> UnitVc` | `.RotateByQuarterCircle` | `numberOfQuarters: int -> UnitVec` |
| `UnitVc.rotateByQuarterCircle` | `numberOfQuarters: int -> v: UnitVc -> UnitVc` | `UnitVec.rotateByQuarterCircle` | `numberOfQuarters: int -> v: UnitVec -> UnitVec` |
| — | *rotation about a world axis* | `.RotateOnZ90CCW` | `UnitVec` |
| — | *rotation about a world axis* | `UnitVec.rotateOnZ90CCW` | `v: UnitVec -> UnitVec` |
| — | *rotation about a world axis* | `.RotateOnZ90CW` | `UnitVec` |
| — | *rotation about a world axis* | `UnitVec.rotateOnZ90CW` | `v: UnitVec -> UnitVec` |
| `.RotateRadians` | `angRadians: float -> UnitVc` | — | *a 3D rotation also needs an axis* |
| `UnitVc.rotateRadians` | `angRadians: float -> v: UnitVc -> UnitVc` | — | *a 3D rotation also needs an axis* |
| `.ToString` | `unit -> string` | `.ToString` | `unit -> string` |
| — | *transformation matrix* | `.TransformRigid` | `m: RigidMatrix -> UnitVec` |
| — | *transformation matrix* | `UnitVec.transformRigid` | `m: RigidMatrix -> v: UnitVec -> UnitVec` |
| `.WithX` | `x: float -> Vc` | `.WithX` | `x: float -> Vec` |
| `UnitVc.withX` | `x: float -> v: UnitVc -> Vc` | `UnitVec.withX` | `x: float -> v: UnitVec -> Vec` |
| `.WithY` | `y: float -> Vc` | `.WithY` | `y: float -> Vec` |
| `UnitVc.withY` | `y: float -> v: UnitVc -> Vc` | `UnitVec.withY` | `y: float -> v: UnitVec -> Vec` |
| `.WithZ` | `z: float -> Vec` | `.WithZ` | `z: float -> Vec` |
| `UnitVc.withZ` | `z: float -> v: UnitVc -> Vec` | `UnitVec.withZ` | `z: float -> v: UnitVec -> Vec` |
| **Other static members** | | | |
| `UnitVc.add` | `a: UnitVc -> b: UnitVc -> Vc` | `UnitVec.add` | `a: UnitVec -> b: UnitVec -> Vec` |
| `UnitVc.addX` | `x: float -> v: UnitVc -> Vc` | `UnitVec.addX` | `x: float -> v: UnitVec -> Vec` |
| `UnitVc.addY` | `y: float -> v: UnitVc -> Vc` | `UnitVec.addY` | `y: float -> v: UnitVec -> Vec` |
| — | *Z axis* | `UnitVec.addZ` | `z: float -> v: UnitVec -> Vec` |
| `UnitVc.angle180` | `a: UnitVc -> b: UnitVc -> float` | `UnitVec.angle180` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVc.angle2Pi` | `a: UnitVc * b: UnitVc -> float` | `UnitVec.angle2PiInXY` | `a: UnitVec * b: UnitVec -> float` |
| `UnitVc.angle360` | `a: UnitVc * b: UnitVc -> float` | `UnitVec.angle360InXY` | `a: UnitVec * b: UnitVec -> float` |
| `UnitVc.angle90` | `a: UnitVc -> b: UnitVc -> float` | `UnitVec.angle90` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVc.angleDiamond` | `a: UnitVc * b: UnitVc -> float` | `UnitVec.angleDiamondInXY` | `a: UnitVec * b: UnitVec -> float` |
| `UnitVc.angleHalfPi` | `a: UnitVc -> b: UnitVc -> float` | `UnitVec.angleHalfPi` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVc.anglePi` | `a: UnitVc -> b: UnitVc -> float` | `UnitVec.anglePi` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVc.angleToX180` | `v: UnitVc -> float` | — | *signed angle in a fixed 2D plane* |
| `UnitVc.angleToXPi` | `v: UnitVc -> float` | — | *signed angle in a fixed 2D plane* |
| `UnitVc.create` | `x: float * y: float -> UnitVc` | `UnitVec.create` | `x: float * y: float * z: float -> UnitVec` |
| `UnitVc.create` | `fromPt: Pt * toPt: Pt -> UnitVc` | `UnitVec.create` | `fromPt: Pnt * toPt: Pnt -> UnitVec` |
| `UnitVc.createFor` | `msg: string * x: float * y: float -> UnitVc` | `UnitVec.createFor` | `msg: string * x: float * y: float * z: float -> UnitVec` |
| `UnitVc.createFromMembersXY` | `vec: ^T -> UnitVc` | `UnitVec.createFromMembersXYZ` | `vec: ^T -> UnitVec` |
| `UnitVc.createFromMembersxy` | `vec: ^T -> UnitVc` | `UnitVec.createFromMembersxyz` | `vec: ^T -> UnitVec` |
| `UnitVc.createFromPt` | `pt: Pt -> UnitVc` | `UnitVec.createFromPnt` | `pt: Pnt -> UnitVec` |
| `UnitVc.createFromVec` | `v: Vc -> UnitVc` | `UnitVec.createFromVec` | `v: Vec -> UnitVec` |
| `UnitVc.createUnchecked` | `x: float * y: float -> UnitVc` | `UnitVec.createUnchecked` | `x: float * y: float * z: float -> UnitVec` |
| — | *triple product* | `UnitVec.determinant3` | `u: UnitVec * v: UnitVec * w: UnitVec -> float` |
| `UnitVc.difference` | `a: UnitVc -> b: UnitVc -> float` | `UnitVec.difference` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVc.differenceSq` | `a: UnitVc -> b: UnitVc -> float` | `UnitVec.differenceSq` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVc.equals` | `tol: float -> a: UnitVc -> b: UnitVc -> bool` | `UnitVec.equals` | `tol: float -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVc.flip` | `v: UnitVc -> UnitVc` | `UnitVec.flip` | `v: UnitVec -> UnitVec` |
| — | *vertical direction* | `UnitVec.flipToPointUp` | `v: UnitVec -> UnitVec` |
| `UnitVc.getX` | `v: UnitVc -> float` | `UnitVec.getX` | `v: UnitVec -> float` |
| `UnitVc.getY` | `v: UnitVc -> float` | `UnitVec.getY` | `v: UnitVec -> float` |
| — | *Z axis* | `UnitVec.getZ` | `v: UnitVec -> float` |
| `UnitVc.isAngleAbove` | `cosineValue: float<cosine> -> a: UnitVc -> b: UnitVc -> bool` | `UnitVec.isAngleAbove` | `cosineValue: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVc.isAngleBelow` | `cosineValue: float<cosine> -> a: UnitVc -> b: UnitVc -> bool` | `UnitVec.isAngleBelow` | `cosineValue: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVc.isNotParallelWithin` | `cosineValue: float<cosine> -> a: UnitVc -> b: UnitVc -> bool` | `UnitVec.isNotParallelWithin` | `cosineValue: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVc.isParallelWithin` | `cosineValue: float<cosine> -> a: UnitVc -> b: UnitVc -> bool` | `UnitVec.isParallelWithin` | `cosineValue: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVc.lerp` | `start: UnitVc * endValue: UnitVc * rel: float -> Vc` | `UnitVec.lerp` | `start: UnitVec * endValue: UnitVec * rel: float -> Vec` |
| `UnitVc.matchOrientation` | `orientationToMatch: UnitVc -> vecToFlip: UnitVc -> UnitVc` | `UnitVec.matchOrientation` | `orientationToMatch: UnitVec -> vecToFlip: UnitVec -> UnitVec` |
| `UnitVc.matchVcOrientation` | `orientationToMatch: Vc -> vecToFlip: UnitVc -> UnitVc` | `UnitVec.matchVecOrientation` | `orientationToMatch: Vec -> vecToFlip: UnitVec -> UnitVec` |
| `UnitVc.notEquals` | `tol: float -> a: UnitVc -> b: UnitVc -> bool` | `UnitVec.notEquals` | `tol: float -> a: UnitVec -> b: UnitVec -> bool` |
| — | *vertical direction* | `UnitVec.orientDown` | `v: UnitVec -> UnitVec` |
| — | *vertical direction* | `UnitVec.orientUp` | `v: UnitVec -> UnitVec` |
| — | *vertical direction* | `UnitVec.perpendicularInVerticalPlane` | `v: UnitVec -> Vec` |
| — | *projection to the XY plane* | `UnitVec.projectToXYPlane` | `v: UnitVec -> Vec` |
| `UnitVc.reverse` | `v: UnitVc -> UnitVc` | `UnitVec.reverse` | `v: UnitVec -> UnitVec` |
| — | *rotation about a world axis* | `UnitVec.rotateOnX` | `r: Rotation2D -> v: UnitVec -> UnitVec` |
| — | *rotation about a world axis* | `UnitVec.rotateOnXDeg` | `angDegree: float -> v: UnitVec -> UnitVec` |
| — | *rotation about a world axis* | `UnitVec.rotateOnY` | `r: Rotation2D -> v: UnitVec -> UnitVec` |
| — | *rotation about a world axis* | `UnitVec.rotateOnYDeg` | `angDegree: float -> v: UnitVec -> UnitVec` |
| — | *rotation about a world axis* | `UnitVec.rotateOnZ` | `r: Rotation2D -> v: UnitVec -> UnitVec` |
| — | *rotation about a world axis* | `UnitVec.rotateOnZDeg` | `angDegree: float -> v: UnitVec -> UnitVec` |
| `UnitVc.scale` | `scale: float -> v: UnitVc -> Vc` | `UnitVec.scale` | `f: float -> v: UnitVec -> Vec` |
| `UnitVc.sine` | `a: UnitVc -> b: UnitVc -> float` |  |  |
| `UnitVc.slerp` | `start: UnitVc * endValue: UnitVc * rel: float -> UnitVc` | `UnitVec.slerp` | `start: UnitVec * endValue: UnitVec * rel: float -> UnitVec` |
| `UnitVc.slopeDegrees` | `v: UnitVc -> float` | `UnitVec.slopeDegrees` | `v: UnitVec -> float` |
| `UnitVc.slopePercent` | `v: UnitVc -> float` | `UnitVec.slopePercent` | `v: UnitVec -> float` |
| `UnitVc.slopeRadians` | `v: UnitVc -> float` | `UnitVec.slopeRadians` | `v: UnitVec -> float` |
| `UnitVc.tangent` | `a: UnitVc -> b: UnitVc -> float` |  |  |
| `UnitVc.withLength` | `length: float -> v: UnitVc -> Vc` | `UnitVec.withLength` | `f: float -> v: UnitVec -> Vec` |
| `UnitVc.Xaxis` | `UnitVc` | `UnitVec.Xaxis` | `UnitVec` |
| `UnitVc.Yaxis` | `UnitVc` | `UnitVec.Yaxis` | `UnitVec` |
|  |  | `UnitVec.Zaxis` | `UnitVec` |
| `( *** )` | `a: UnitVc * b: UnitVc -> float` | `( *** )` | `a: UnitVec * b: UnitVec -> float` |
| `( *** )` | `a: UnitVc * b: Vc -> float` | `( *** )` | `a: UnitVec * b: Vec -> float` |
| `( *** )` | `a: Vc * b: UnitVc -> float` | `( *** )` | `a: Vec * b: UnitVec -> float` |
| `( * )` | `a: UnitVc * f: float -> Vc` | `( * )` | `a: UnitVec * f: float -> Vec` |
| `( * )` | `f: float * a: UnitVc -> Vc` | `( * )` | `f: float * a: UnitVec -> Vec` |
| `( + )` | `a: UnitVc * b: UnitVc -> Vc` | `( + )` | `a: UnitVec * b: UnitVec -> Vec` |
| `( + )` | `a: Vc * b: UnitVc -> Vc` | `( + )` | `a: Vec * b: UnitVec -> Vec` |
| `( + )` | `a: UnitVc * b: Vc -> Vc` | `( + )` | `a: UnitVec * b: Vec -> Vec` |
| `( - )` | `a: UnitVc * b: UnitVc -> Vc` | `( - )` | `a: UnitVec * b: UnitVec -> Vec` |
| `( - )` | `a: Vc * b: UnitVc -> Vc` | `( - )` | `a: Vec * b: UnitVec -> Vec` |
| `( - )` | `a: UnitVc * b: Vc -> Vc` | `( - )` | `a: UnitVec * b: Vec -> Vec` |
| `( / )` | `v: UnitVc * f: float -> Vc` | `( / )` | `v: UnitVec * f: float -> Vec` |
| `( ~- )` | `v: UnitVc -> UnitVc` | `( ~- )` | `v: UnitVec -> UnitVec` |

## Pt / Pnt

| Pt | Signature | Pnt | Signature |
|:--|:--|:--|:--|
| `new` | `x: float * y: float -> Pt` | `new` | `x: float * y: float * z: float -> Pnt` |
| `.X` | `float` | `.X` | `float` |
| `.Y` | `float` | `.Y` | `float` |
| — | *Z axis* | `.Z` | `float` |
| `.Angle2PiTo` | `o: Pt -> float` | `.Angle2PiInXYTo` | `o: Pnt -> float` |
| `Pt.angle2PiTo` | `fromPt: Pt * toPt: Pt -> float` | `Pnt.angle2PiInXYTo` | `fromPt: Pnt * toPt: Pnt -> float` |
| `.Angle360To` | `o: Pt -> float` | `.Angle360InXYTo` | `o: Pnt -> float` |
| `Pt.angle360To` | `fromPt: Pt * toPt: Pt -> float` | `Pnt.angle360InXYTo` | `fromPt: Pnt * toPt: Pnt -> float` |
| `.AsFSharpCode` | `string` | `.AsFSharpCode` | `string` |
| `.AsPnt` | `Pnt` | `.AsPt` | `Pt` |
| `Pt.asPnt` | `pt: Pt -> Pnt` | `Pnt.asPt` | `pt: Pnt -> Pt` |
| `.AsString` | `string` | `.AsString` | `string` |
| `.AsVec` | `Vec` | `.AsVc` | `Vc` |
| `Pt.asVec` | `pt: Pt -> Vec` | `Pnt.asVc` | `pt: Pnt -> Vc` |
| `.AsVc` | `Vc` | `.AsVec` | `Vec` |
| `Pt.asVc` | `pt: Pt -> Vc` | `Pnt.asVec` | `pt: Pnt -> Vec` |
| `.ClosestPointOnLine` | `ln: Line2D -> Pt` | `.ClosestPointOnLine` | `ln: Line3D -> Pnt` |
| `Pt.closestPointOnLine` | `ln: Line2D -> testPt: Pt -> Pt` | `Pnt.closestPointOnLine` | `ln: Line3D -> testPt: Pnt -> Pnt` |
| `.DirectionDiamondTo` | `o: Pt -> float` | `.DirectionDiamondInXYTo` | `o: Pnt -> float` |
| `Pt.directionDiamondTo` | `o: Pt -> p: Pt -> float` | `Pnt.directionDiamondInXYTo` | `fromPt: Pnt * toPt: Pnt -> float` |
| `.DistanceFromOrigin` | `float` | `.DistanceFromOrigin` | `float` |
| `Pt.distanceFromOrigin` | `pt: Pt -> float` | `Pnt.distanceFromOrigin` | `pt: Pnt -> float` |
| — | *projection to the XY plane* | `.DistanceInXYFromOrigin` | `float` |
| — | *projection to the XY plane* | `Pnt.distanceInXYFromOrigin` | `pt: Pnt -> float` |
| `.DistanceTo` | `b: Pt -> float` | `.DistanceTo` | `b: Pnt -> float` |
| `Pt.distanceTo` | `b: Pt -> p: Pt -> float` | `Pnt.distanceTo` | `b: Pnt -> p: Pnt -> float` |
| `.DistanceToLine` | `ln: Line2D -> float` | `.DistanceToLine` | `ln: Line3D -> float` |
| `Pt.distanceToLine` | `ln: Line2D -> testPt: Pt -> float` | `Pnt.distanceToLine` | `ln: Line3D -> testPt: Pnt -> float` |
| `.DistanceToXY` | `x: float * y: float -> float` | `.DistanceToXYZ` | `x: float * y: float * z: float -> float` |
| `Pt.distanceToXY` | `x: float * y: float -> p: Pt -> float` | `Pnt.distanceToXYZ` | `x: float * y: float * z: float -> p: Pnt -> float` |
| `.IsAlmostOrigin` | `tol: float -> bool` | `.IsAlmostOrigin` | `tol: float -> bool` |
| `Pt.isAlmostOrigin` | `tol: float -> pt: Pt -> bool` | `Pnt.isAlmostOrigin` | `tol: float -> pt: Pnt -> bool` |
| `.IsInvalid` | `bool` | `.IsInvalid` | `bool` |
| `Pt.isInvalid` | `pt: Pt -> bool` | `Pnt.isInvalid` | `pt: Pnt -> bool` |
| `.IsNotOrigin` | `bool` | `.IsNotOrigin` | `bool` |
| `Pt.isNotOrigin` | `pt: Pt -> bool` | `Pnt.isNotOrigin` | `pt: Pnt -> bool` |
| `.IsOrigin` | `bool` | `.IsOrigin` | `bool` |
| `Pt.isOrigin` | `pt: Pt -> bool` | `Pnt.isOrigin` | `pt: Pnt -> bool` |
| `.IsValid` | `bool` | `.IsValid` | `bool` |
| `Pt.isValid` | `pt: Pt -> bool` | `Pnt.isValid` | `pt: Pnt -> bool` |
| `.Rotate` | `angDegree: float -> Pt` |  |  |
| `Pt.rotate` | `angDegree: float -> vec: Pt -> Pt` | `Pnt.rotate` | `q: Quaternion -> pt: Pnt -> Pnt` |
| `.Rotate90CCW` | `Pt` | — | *orientation in the plane* |
| `Pt.rotate90CCW` | `pt: Pt -> Pt` | — | *orientation in the plane* |
| `.Rotate90CW` | `Pt` |  |  |
| `Pt.rotate90CW` | `pt: Pt -> Pt` |  |  |
| `.RotateBy` | `r: Rotation2D -> Pt` |  |  |
| `Pt.rotateBy` | `r: Rotation2D -> p: Pt -> Pt` |  |  |
| `.RotateByQuarterCircle` | `numberOfQuarters: int -> Pt` |  |  |
| `Pt.rotateByQuarterCircle` | `numberOfQuarters: int -> v: Pt -> Pt` |  |  |
| `.RotateRadians` | `angRadians: float -> Pt` | — | *a 3D rotation also needs an axis* |
| `Pt.rotateRadians` | `angRadians: float -> pt: Pt -> Pt` | — | *a 3D rotation also needs an axis* |
| `.SqDistanceFromOrigin` | `float` | `.SqDistanceFromOrigin` | `float` |
| `Pt.sqDistanceFromOrigin` | `pt: Pt -> float` | `Pnt.sqDistanceFromOrigin` | `pt: Pnt -> float` |
| — | *projection to the XY plane* | `.SqDistanceInXYFromOrigin` | `float` |
| — | *projection to the XY plane* | `Pnt.sqDistanceInXYFromOrigin` | `pt: Pnt -> float` |
| `.SqDistanceTo` | `b: Pt -> float` | `.SqDistanceTo` | `b: Pnt -> float` |
| `Pt.sqDistanceTo` | `b: Pt -> p: Pt -> float` | `Pnt.sqDistanceTo` | `b: Pnt -> p: Pnt -> float` |
| `.SqDistanceToLine` | `ln: Line2D -> float` | `.SqDistanceToLine` | `ln: Line3D -> float` |
| `Pt.sqDistanceToLine` | `ln: Line2D -> testPt: Pt -> float` | `Pnt.sqDistanceToLine` | `ln: Line3D -> testPt: Pnt -> float` |
| `.SqDistanceToXY` | `x: float * y: float -> float` | `.SqDistanceToXYZ` | `x: float * y: float * z: float -> float` |
| `Pt.sqDistanceToXY` | `x: float * y: float -> p: Pt -> float` | `Pnt.sqDistanceToXYZ` | `x: float * y: float * z: float -> p: Pnt -> float` |
| `.ToString` | `unit -> string` | `.ToString` | `unit -> string` |
| — | *transformation matrix* | `.Transform` | `m: Matrix -> Pnt` |
| — | *transformation matrix* | `Pnt.transform` | `m: Matrix -> p: Pnt -> Pnt` |
| — | *transformation matrix* | `.TransformRigid` | `m: RigidMatrix -> Pnt` |
| — | *transformation matrix* | `Pnt.transformRigid` | `m: RigidMatrix -> p: Pnt -> Pnt` |
| — | *transformation matrix* | `.TransformRigidRotateOnly` | `m: RigidMatrix -> Pnt` |
| — | *transformation matrix* | `Pnt.transformRigidRotateOnly` | `m: RigidMatrix -> p: Pnt -> Pnt` |
| `.WithDistanceFromOrigin` | `l: float -> Pt` | `.WithDistanceFromOrigin` | `l: float -> Pnt` |
| `Pt.withDistanceFromOrigin` | `l: float -> pt: Pt -> Pt` | `Pnt.withDistanceFromOrigin` | `l: float -> pt: Pnt -> Pnt` |
| `.WithX` | `x: float -> Pt` | `.WithX` | `x: float -> Pnt` |
| `Pt.withX` | `x: float -> pt: Pt -> Pt` | `Pnt.withX` | `x: float -> pt: Pnt -> Pnt` |
| `.WithY` | `y: float -> Pt` | `.WithY` | `y: float -> Pnt` |
| `Pt.withY` | `y: float -> pt: Pt -> Pt` | `Pnt.withY` | `y: float -> pt: Pnt -> Pnt` |
| `.WithZ` | `z: float -> Pnt` | `.WithZ` | `z: float -> Pnt` |
| `Pt.withZ` | `z: float -> pt: Pt -> Pnt` | `Pnt.withZ` | `z: float -> pt: Pnt -> Pnt` |
| **Other static members** | | | |
| `Pt.add` | `a: Pt -> b: Pt -> Pt` | `Pnt.add` | `a: Pnt -> b: Pnt -> Pnt` |
| `Pt.addVc` | `v: Vc -> a: Pt -> Pt` | `Pnt.addVec` | `v: Vec -> a: Pnt -> Pnt` |
| `Pt.angle180Pts` | `ptPrev: Pt * ptThis: Pt * ptNext: Pt -> float` | `Pnt.angle180Pts` | `ptPrev: Pnt * ptThis: Pnt * ptNext: Pnt -> float` |
| `Pt.angleInCorner` | `prevPt: Pt * thisPt: Pt * nextPt: Pt -> float` | `Pnt.angleInCorner` | `prevPt: Pnt * thisPt: Pnt * nextPt: Pnt -> float` |
| `Pt.anglePiPts` | `ptPrev: Pt * ptThis: Pt * ptNext: Pt -> float` | `Pnt.anglePiPts` | `ptPrev: Pnt * ptThis: Pnt * ptNext: Pnt -> float` |
| `Pt.bisector` | `ptPrev: Pt * ptThis: Pt * ptNext: Pt -> Vc` | `Pnt.bisector` | `ptPrev: Pnt * ptThis: Pnt * ptNext: Pnt -> Vec` |
| `Pt.closestOfTwo` | `pt1: Pt -> pt2: Pt -> referencePoint: Pt -> Pt` | `Pnt.closestOfTwo` | `pt1: Pnt -> pt2: Pnt -> referencePoint: Pnt -> Pnt` |
| `Pt.createFromMembersXY` | `pt: ^T -> Pt` | `Pnt.createFromMembersXYZ` | `pt: ^T -> Pnt` |
| `Pt.createFromMembersxy` | `pt: ^T -> Pt` | `Pnt.createFromMembersxyz` | `pt: ^T -> Pnt` |
| `Pt.createFromPnt` | `p: Pnt -> Pt` | `Pnt.createFromPt` | `p: Pt -> Pnt` |
| — | *Z axis* | `Pnt.createFromPtWithZ` | `z: float -> p: Pt -> Pnt` |
| `Pt.createFromUnitVc` | `v: UnitVc -> Pt` | `Pnt.createFromUnitVec` | `v: UnitVec -> Pnt` |
| `Pt.createFromVc` | `v: Vc -> Pt` | `Pnt.createFromVec` | `v: Vec -> Pnt` |
| `Pt.dist` | `b: Pt -> p: Pt -> float` | `Pnt.dist` | `b: Pnt -> p: Pnt -> float` |
| — | *projection to the XY plane* | `Pnt.distanceInXY` | `a: Pnt -> b: Pnt -> float` |
| `Pt.distPt` | `fromPt: Pt * dirPt: Pt * distance: float -> Pt` | `Pnt.distPt` | `fromPt: Pnt * dirPt: Pnt * distance: float -> Pnt` |
| `Pt.DivideByInt` | `pt: Pt * i: int -> Pt` | `Pnt.DivideByInt` | `pt: Pnt * i: int -> Pnt` |
| `Pt.divPt` | `fromPt: Pt * toPt: Pt * rel: float -> Pt` | `Pnt.divPt` | `fromPt: Pnt * toPt: Pnt * rel: float -> Pnt` |
| `Pt.equals` | `tol: float -> a: Pt -> b: Pt -> bool` | `Pnt.equals` | `tol: float -> a: Pnt -> b: Pnt -> bool` |
| — | *Z axis* | `Pnt.extendToZLevel` | `fromPt: Pnt * toPt: Pnt * z: float -> Pnt` |
| `Pt.getX` | `pt: Pt -> float` | `Pnt.getX` | `pt: Pnt -> float` |
| `Pt.getY` | `pt: Pt -> float` | `Pnt.getY` | `pt: Pnt -> float` |
| — | *Z axis* | `Pnt.getZ` | `pt: Pnt -> float` |
| `Pt.lerp` | `fromPt: Pt * toPt: Pt * rel: float -> Pt` | `Pnt.lerp` | `fromPt: Pnt * toPt: Pnt * rel: float -> Pnt` |
| `Pt.midPt` | `a: Pt -> b: Pt -> Pt` | `Pnt.midPt` | `a: Pnt -> b: Pnt -> Pnt` |
| `Pt.move` | `shift: Vc -> pt: Pt -> Pt` | `Pnt.move` | `shift: Vec -> pt: Pnt -> Pnt` |
| `Pt.moveX` | `x: float -> pt: Pt -> Pt` | `Pnt.moveX` | `x: float -> pt: Pnt -> Pnt` |
| `Pt.moveY` | `y: float -> pt: Pt -> Pt` | `Pnt.moveY` | `y: float -> pt: Pnt -> Pnt` |
| — | *Z axis* | `Pnt.moveZ` | `z: float -> pt: Pnt -> Pnt` |
| — | *plane or normal* | `Pnt.normalOf3Pts` | `a: Pnt * b: Pnt * c: Pnt -> Vec` |
| — | *projection to the XY plane* | `Pnt.normalOfTwoPointsInXY` | `fromPt: Pnt * toPt: Pnt -> Vec` |
| `Pt.notEquals` | `tol: float -> a: Pt -> b: Pt -> bool` | `Pnt.notEquals` | `tol: float -> a: Pnt -> b: Pnt -> bool` |
| `Pt.Origin` | `Pt` | `Pnt.Origin` | `Pnt` |
| `Pt.projectedParameter` | `fromPt: Pt * uv: UnitVc * testPt: Pt -> float` | `Pnt.projectedParameter` | `fromPt: Pnt * uv: UnitVec * testPt: Pnt -> float` |
| `Pt.projectedParameter` | `fromPt: Pt * v: Vc * testPt: Pt -> float` | `Pnt.projectedParameter` | `fromPt: Pnt * v: Vec * testPt: Pnt -> float` |
| `Pt.projectedParameter` | `fromPt: Pt * toPt: Pt * testPt: Pt -> float` | `Pnt.projectedParameter` | `fromPt: Pnt * toPt: Pnt * testPt: Pnt -> float` |
| — | *projection to the XY plane* | `Pnt.projectToXYPlane` | `pt: Pnt -> Pnt` |
| — | *rotation about a world axis* | `Pnt.rotateOnX` | `r: Rotation2D -> p: Pnt -> Pnt` |
| — | *rotation about a world axis* | `Pnt.rotateOnXDeg` | `angDegree: float -> pt: Pnt -> Pnt` |
| — | *rotation about a world axis* | `Pnt.rotateOnXWithCenter` | `cen: Pnt -> r: Rotation2D -> pt: Pnt -> Pnt` |
| — | *rotation about a world axis* | `Pnt.rotateOnXWithCenterDeg` | `cen: Pnt -> angDegree: float -> pt: Pnt -> Pnt` |
| — | *rotation about a world axis* | `Pnt.rotateOnY` | `r: Rotation2D -> p: Pnt -> Pnt` |
| — | *rotation about a world axis* | `Pnt.rotateOnYDeg` | `angDegree: float -> pt: Pnt -> Pnt` |
| — | *rotation about a world axis* | `Pnt.rotateOnYWithCenter` | `cen: Pnt -> r: Rotation2D -> pt: Pnt -> Pnt` |
| — | *rotation about a world axis* | `Pnt.rotateOnYWithCenterDeg` | `cen: Pnt -> angDegree: float -> pt: Pnt -> Pnt` |
| — | *rotation about a world axis* | `Pnt.rotateOnZ` | `r: Rotation2D -> p: Pnt -> Pnt` |
| — | *rotation about a world axis* | `Pnt.rotateOnZDeg` | `angDegree: float -> pt: Pnt -> Pnt` |
| — | *rotation about a world axis* | `Pnt.rotateOnZWithCenter` | `cen: Pnt -> r: Rotation2D -> pt: Pnt -> Pnt` |
| — | *rotation about a world axis* | `Pnt.rotateOnZWithCenterDeg` | `cen: Pnt -> angDegree: float -> pt: Pnt -> Pnt` |
| `Pt.rotateWithCenter` | `cen: Pt -> angDegree: float -> pt: Pt -> Pt` | `Pnt.rotateWithCenter` | `cen: Pnt -> q: Quaternion -> pt: Pnt -> Pnt` |
| `Pt.rotateWithCenterBy` | `cen: Pt -> r: Rotation2D -> pt: Pt -> Pt` |  |  |
| `Pt.scale` | `f: float -> pt: Pt -> Pt` | `Pnt.scale` | `f: float -> pt: Pnt -> Pnt` |
| `Pt.snap` | `precision: float -> pt: Pt -> Pt` | `Pnt.snap` | `precision: float -> pt: Pnt -> Pnt` |
| `Pt.snapIfClose` | `snapDistance: float -> snapTo: Pt -> pt: Pt -> Pt` | `Pnt.snapIfClose` | `snapDistance: float -> snapTo: Pnt -> pt: Pnt -> Pnt` |
| `Pt.sqDist` | `b: Pt -> p: Pt -> float` | `Pnt.sqDist` | `b: Pnt -> p: Pnt -> float` |
| `Pt.translate` | `shift: Vc -> pt: Pt -> Pt` | `Pnt.translate` | `shift: Vec -> pt: Pnt -> Pnt` |
| `Pt.Zero` | `Pt` | `Pnt.Zero` | `Pnt` |
| `( * )` | `a: Pt * f: float -> Pt` | `( * )` | `a: Pnt * f: float -> Pnt` |
| `( * )` | `f: float * a: Pt -> Pt` | `( * )` | `f: float * a: Pnt -> Pnt` |
| `( + )` | `p: Pt * v: Vc -> Pt` | `( + )` | `p: Pnt * v: Vec -> Pnt` |
| `( + )` | `p: Pt * v: UnitVc -> Pt` | `( + )` | `p: Pnt * v: UnitVec -> Pnt` |
| `( + )` | `a: Pt * b: Pt -> Pt` | `( + )` | `a: Pnt * b: Pnt -> Pnt` |
| `( - )` | `a: Pt * b: Pt -> Vc` | `( - )` | `a: Pnt * b: Pnt -> Vec` |
| `( - )` | `a: Pt * b: Vc -> Pt` | `( - )` | `p: Pnt * v: Vec -> Pnt` |
| `( - )` | `a: Pt * b: UnitVc -> Pt` | `( - )` | `p: Pnt * v: UnitVec -> Pnt` |
| `( / )` | `p: Pt * f: float -> Pt` | `( / )` | `p: Pnt * f: float -> Pnt` |

## Rotation2D / Quaternion

| Rotation2D | Signature | Quaternion | Signature |
|:--|:--|:--|:--|
| `.Sin` | `float` | — | *sine and cosine* |
| `.Cos` | `float` | — | *sine and cosine* |
| — | *quaternion components* | `.X` | `float` |
| — | *quaternion components* | `.Y` | `float` |
| — | *quaternion components* | `.Z` | `float` |
| — | *quaternion components* | `.W` | `float` |
| `.Add` | `ro: Rotation2D -> Rotation2D` |  |  |
| `Rotation2D.add` | `ro: Rotation2D -> r: Rotation2D -> Rotation2D` |  |  |
| `.AddDegrees` | `deg: float -> Rotation2D` | — | *a 3D rotation also needs an axis* |
| `Rotation2D.addDegrees` | `deg: float -> r: Rotation2D -> Rotation2D` | — | *a 3D rotation also needs an axis* |
| `.AddRadians` | `rad: float -> Rotation2D` | — | *a 3D rotation also needs an axis* |
| `Rotation2D.addRadians` | `rad: float -> r: Rotation2D -> Rotation2D` | — | *a 3D rotation also needs an axis* |
|  |  | `.AngleInDegrees` | `float` |
|  |  | `Quaternion.angleInDegrees` | `q: Quaternion -> float` |
|  |  | `.AngleInRadians` | `float` |
|  |  | `Quaternion.angleInRadians` | `q: Quaternion -> float` |
| `.AsFSharpCode` | `string` | `.AsFSharpCode` | `string` |
| `Rotation2D.asFSharpCode` | `r: Rotation2D -> string` | `Quaternion.asFSharpCode` | `q: Quaternion -> string` |
| `.AsString` | `string` | `.AsString` | `string` |
| `Rotation2D.asString` | `r: Rotation2D -> string` | `Quaternion.asString` | `q: Quaternion -> string` |
| — | *quaternion components* | `.Axis` | `Vec` |
| — | *quaternion components* | `Quaternion.axis` | `q: Quaternion -> Vec` |
| — | *quaternion components* | `.Conjugate` | `Quaternion` |
| — | *quaternion components* | `Quaternion.conjugate` | `q: Quaternion -> Quaternion` |
| `.Half` | `Rotation2D` |  |  |
| `.InDegrees` | `float` |  |  |
| `Rotation2D.inDegrees` | `r: Rotation2D -> float` |  |  |
| `.InRadians` | `float` |  |  |
| `Rotation2D.inRadians` | `r: Rotation2D -> float` |  |  |
| `.Inverse` | `Rotation2D` | `.Inverse` | `Quaternion` |
| `Rotation2D.inverse` | `r: Rotation2D -> Rotation2D` | `Quaternion.inverse` | `q: Quaternion -> Quaternion` |
| — | *quaternion renormalization API* | `.Normalize` | `unit -> Quaternion` |
| — | *quaternion renormalization API* | `Quaternion.normalize` | `q: Quaternion -> Quaternion` |
| `.ToString` | `unit -> string` | `.ToString` | `unit -> string` |
| — | *replace angle while retaining the 3D axis* | `.WithAngleInDegrees` | `angleInDegrees: float -> Quaternion` |
| — | *replace angle while retaining the 3D axis* | `Quaternion.withAngleInDegrees` | `angleInDegrees: float -> q: Quaternion -> Quaternion` |
| — | *replace angle while retaining the 3D axis* | `.WithAngleInRadians` | `angleInRadians: float -> Quaternion` |
| — | *replace angle while retaining the 3D axis* | `Quaternion.withAngleInRadians` | `angleInRadians: float -> q: Quaternion -> Quaternion` |
| **Other static members** | | | |
| `Rotation2D.create` | `sine: float * cosine: float -> Rotation2D` | `Quaternion.create` | `x: float * y: float * z: float * w: float -> Quaternion` |
| `Rotation2D.createFromCosine` | `cos: float -> Rotation2D` | — | *sine and cosine* |
| `Rotation2D.createFromDegrees` | `deg: float -> Rotation2D` | `Quaternion.createFromDegrees` | `axis: Vec * angleInDegrees: float -> Quaternion` |
|  |  | `Quaternion.createFromDegrees` | `axis: UnitVec * angleInDegrees: float -> Quaternion` |
| — | *quaternion components* | `Quaternion.createFromEulerXYZ` | `degreesX: float * degreesY: float * degreesZ: float -> Quaternion` |
| — | *quaternion components* | `Quaternion.createFromEulerXZY` | `degreesX: float * degreesZ: float * degreesY: float -> Quaternion` |
| — | *quaternion components* | `Quaternion.createFromEulerYXZ` | `degreesY: float * degreesX: float * degreesZ: float -> Quaternion` |
| — | *quaternion components* | `Quaternion.createFromEulerYZX` | `degreesY: float * degreesZ: float * degreesX: float -> Quaternion` |
| — | *quaternion components* | `Quaternion.createFromEulerZXY` | `degreesZ: float * degreesX: float * degreesY: float -> Quaternion` |
| — | *quaternion components* | `Quaternion.createFromEulerZYX` | `degreesZ: float * degreesY: float * degreesX: float -> Quaternion` |
| `Rotation2D.createFromRadians` | `rad: float -> Rotation2D` | `Quaternion.createFromRadians` | `axis: Vec * angleInRadians: float -> Quaternion` |
|  |  | `Quaternion.createFromRadians` | `axis: UnitVec * angleInRadians: float -> Quaternion` |
| — | *transformation matrix* | `Quaternion.createFromRigidMatrix` | `orthoMatrix: RigidMatrix -> Quaternion` |
| `Rotation2D.createFromSine` | `sin: float -> Rotation2D` | — | *sine and cosine* |
| `Rotation2D.createFromVectors` | `a: UnitVc * b: UnitVc -> Rotation2D` |  |  |
| `Rotation2D.createFromVectors` | `a: Vc * b: Vc -> Rotation2D` |  |  |
| `Rotation2D.createUnchecked` | `sine: float * cosine: float -> Rotation2D` | `Quaternion.createUnchecked` | `x: float * y: float * z: float * w: float -> Quaternion` |
|  |  | `Quaternion.createVecToVec` | `vecFrom: UnitVec * vecTo: UnitVec -> Quaternion` |
|  |  | `Quaternion.createVecToVec` | `vecFrom: Vec * vecTo: Vec -> Quaternion` |
| `Rotation2D.equals` | `tol: float -> a: Rotation2D -> b: Rotation2D -> bool` | `Quaternion.equals` | `tolerance: float -> a: Quaternion -> b: Quaternion -> bool` |
|  |  | `Quaternion.equalsRotation` | `tolerance: float -> a: Quaternion -> b: Quaternion -> bool` |
|  |  | `Quaternion.identity` | `Quaternion` |
| — | *quaternion composition and application operators* | `Quaternion.multiply` | `l: Quaternion * r: Quaternion -> Quaternion` |
| — | *quaternion components* | `Quaternion.toEulerAnglesZYX` | `q: Quaternion -> float * float * float` |
| — | *transformation matrix* | `Quaternion.tryCreateFromMatrix` | `matrix: Matrix -> Quaternion option` |
| — | *quaternion composition and application operators* | `( *** )` | `l: Quaternion * r: Quaternion -> Quaternion` |
| — | *quaternion composition and application operators* | `( *** )` | `v: Vec * q: Quaternion -> Vec` |
| — | *quaternion composition and application operators* | `( *** )` | `v: UnitVec * q: Quaternion -> UnitVec` |
| — | *quaternion composition and application operators* | `( *** )` | `p: Pnt * q: Quaternion -> Pnt` |

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
| `.IsCoincidentTo` | `other: PPlane * ?distanceTolerance: float * ?minCosine: float<cosine> -> bool` |
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

## Line2D / Line3D

| Line2D | Signature | Line3D | Signature |
|:--|:--|:--|:--|
| `new` | `fromPt: Pt * toPt: Pt -> Line2D` | `new` | `fromPt: Pnt * toPt: Pnt -> Line3D` |
| `new` | `fromX: float * fromY: float * toX: float * toY: float -> Line2D` | `new` | `fromX: float * fromY: float * fromZ: float * toX: float * toY: float * toZ: float -> Line3D` |
| `.FromX` | `float` | `.FromX` | `float` |
| `.FromY` | `float` | `.FromY` | `float` |
| `.ToX` | `float` | `.ToX` | `float` |
| `.ToY` | `float` | `.ToY` | `float` |
| — | *Z axis* | `.FromZ` | `float` |
| — | *Z axis* | `.ToZ` | `float` |
| `.AsFSharpCode` | `string` | `.AsFSharpCode` | `string` |
| `Line2D.asFSharpCode` | `l: Line2D -> string` | `Line3D.asFSharpCode` | `ln: Line3D -> string` |
| `.AsString` | `string` | `.AsString` | `string` |
| `Line2D.asString` | `l: Line2D -> string` | `Line3D.asString` | `ln: Line3D -> string` |
| `.ClosestParameter` | `p: Pt -> float` | `.ClosestParameter` | `p: Pnt -> float` |
| `Line2D.closestParameter` | `p: Pt -> ln: Line2D -> float` | `Line3D.closestParameter` | `p: Pnt -> ln: Line3D -> float` |
| `.ClosestParameters` | `lnB: Line2D -> float * float` | `.ClosestParameters` | `lnB: Line3D -> float * float` |
| `Line2D.closestParameters` | `lnA: Line2D -> lnB: Line2D -> float * float` | `Line3D.closestParameters` | `lnA: Line3D -> lnB: Line3D -> float * float` |
| `.ClosestParameterXY` | `x: float * y: float -> float` | `.ClosestParameterXYZ` | `x: float * y: float * z: float -> float` |
| `Line2D.closestParameterXY` | `x: float -> y: float -> ln: Line2D -> float` | `Line3D.closestParameterXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
| `.ClosestPoint` | `p: Pt -> Pt` | `.ClosestPoint` | `p: Pnt -> Pnt` |
| `Line2D.closestPoint` | `p: Pt -> ln: Line2D -> Pt` | `Line3D.closestPoint` | `p: Pnt -> ln: Line3D -> Pnt` |
| `.ClosestPoints` | `lnB: Line2D -> Pt * Pt` | `.ClosestPoints` | `lnB: Line3D -> Pnt * Pnt` |
| `Line2D.closestPoints` | `lnA: Line2D -> lnB: Line2D -> Pt * Pt` | `Line3D.closestPoints` | `lnA: Line3D -> lnB: Line3D -> Pnt * Pnt` |
| `.Direction` | `Vc` | `.Direction` | `Vec` |
| `Line2D.direction` | `ln: Line2D -> Vc` | `Line3D.direction` | `ln: Line3D -> Vec` |
| `.DistanceRayPoint` | `p: Pt -> float` | `.DistanceRayPoint` | `p: Pnt -> float` |
| `Line2D.distanceRayPoint` | `p: Pt -> ln: Line2D -> float` | `Line3D.distanceRayPoint` | `p: Pnt -> ln: Line3D -> float` |
| `.DistanceRayXY` | `x: float * y: float -> float` | `.DistanceRayXYZ` | `x: float * y: float * z: float -> float` |
| `Line2D.distanceRayXY` | `x: float -> y: float -> ln: Line2D -> float` | `Line3D.distanceRayXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
| `.DistanceToPt` | `p: Pt -> float` | `.DistanceToPnt` | `p: Pnt -> float` |
| `Line2D.distanceToPt` | `p: Pt -> ln: Line2D -> float` | `Line3D.distanceToPnt` | `p: Pnt -> ln: Line3D -> float` |
| `.DistanceToXY` | `x: float * y: float -> float` | `.DistanceToXYZ` | `x: float * y: float * z: float -> float` |
| `Line2D.distanceToXY` | `x: float -> y: float -> ln: Line2D -> float` | `Line3D.distanceToXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
| `.End` | `Pt` | `.End` | `Pnt` |
| `.EvaluateAt` | `p: float -> Pt` | `.EvaluateAt` | `p: float -> Pnt` |
| `Line2D.evaluateAt` | `t: float -> ln: Line2D -> Pt` | `Line3D.evaluateAt` | `t: float -> ln: Line3D -> Pnt` |
| `.Extend` | `distAtStart: float * distAtEnd: float -> Line2D` | `.Extend` | `distAtStart: float * distAtEnd: float -> Line3D` |
| `Line2D.extend` | `distAtStart: float -> distAtEnd: float -> ln: Line2D -> Line2D` | `Line3D.extend` | `distAtStart: float -> distAtEnd: float -> ln: Line3D -> Line3D` |
| `.ExtendEnd` | `distAtEnd: float -> Line2D` | `.ExtendEnd` | `distAtEnd: float -> Line3D` |
| `Line2D.extendEnd` | `distAtEnd: float -> ln: Line2D -> Line2D` | `Line3D.extendEnd` | `distAtEnd: float -> ln: Line3D -> Line3D` |
| `.ExtendEndRel` | `relAtEnd: float -> Line2D` | `.ExtendEndRel` | `relAtEnd: float -> Line3D` |
| `Line2D.extendEndRel` | `relAtEnd: float -> ln: Line2D -> Line2D` | `Line3D.extendEndRel` | `relAtEnd: float -> ln: Line3D -> Line3D` |
| `.ExtendRel` | `relAtStart: float * relAtEnd: float -> Line2D` | `.ExtendRel` | `relAtStart: float * relAtEnd: float -> Line3D` |
| `Line2D.extendRel` | `relAtStart: float -> relAtEnd: float -> ln: Line2D -> Line2D` | `Line3D.extendRel` | `relAtStart: float -> relAtEnd: float -> ln: Line3D -> Line3D` |
| `.ExtendStart` | `distAtStart: float -> Line2D` | `.ExtendStart` | `distAtStart: float -> Line3D` |
| `Line2D.extendStart` | `distAtStart: float -> ln: Line2D -> Line2D` | `Line3D.extendStart` | `distAtStart: float -> ln: Line3D -> Line3D` |
| `.ExtendStartRel` | `relAtStart: float -> Line2D` | `.ExtendStartRel` | `relAtStart: float -> Line3D` |
| `Line2D.extendStartRel` | `relAtStart: float -> ln: Line2D -> Line2D` | `Line3D.extendStartRel` | `relAtStart: float -> ln: Line3D -> Line3D` |
| `.From` | `Pt` | `.From` | `Pnt` |
| `Line2D.from` | `l: Line2D -> Pt` | `Line3D.from` | `l: Line3D -> Pnt` |
| `.IsCoincidentAndOpposingToFast` | `other: Line2D * ?parallelogramAreaTolerance: float * ?maxDotProduct: float -> bool` | `.IsCoincidentAndOpposingToFast` | `other: Line3D * ?squaredParallelogramAreaTolerance: float * ?maxDotProduct: float -> bool` |
| `Line2D.isCoincidentAndOpposingToFast` | `parallelogramAreaTolerance: float -> maxDotProduct: float -> lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isCoincidentAndOpposingToFast` | `squaredParallelogramAreaTolerance: float -> maxDotProduct: float -> lnA: Line3D -> lnB: Line3D -> bool` |
| `.IsCoincidentAndOrientedToFast` | `other: Line2D * ?parallelogramAreaTolerance: float * ?minDotProduct: float -> bool` | `.IsCoincidentAndOrientedToFast` | `other: Line3D * ?squaredParallelogramAreaTolerance: float * ?minDotProduct: float -> bool` |
| `Line2D.isCoincidentAndOrientedToFast` | `parallelogramAreaTolerance: float -> minDotProduct: float -> lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isCoincidentAndOrientedToFast` | `?squaredParallelogramAreaTolerance: float -> ?minDotProduct: float -> other: Line3D -> ln: Line3D -> bool` |
| `.IsCoincidentTo` | `other: Line2D * ?distanceTolerance: float * ?minTangent: float<tangent> -> bool` | `.IsCoincidentTo` | `other: Line3D * ?distanceTolerance: float * ?minTangent: float<tangent> -> bool` |
| `Line2D.isCoincidentTo` | `distanceTolerance: float -> minTangent: float<tangent> -> a: Line2D -> b: Line2D -> bool` | `Line3D.isCoincidentTo` | `distanceTolerance: float -> minTangent: float<tangent> -> a: Line3D -> b: Line3D -> bool` |
| `.IsCoincidentToFast` | `other: Line2D * ?parallelogramAreaTolerance: float -> bool` | `.IsCoincidentToFast` | `other: Line3D * ?squaredParallelogramAreaTolerance: float -> bool` |
| `Line2D.isCoincidentToFast` | `parallelogramAreaTolerance: float -> lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isCoincidentToFast` | `?squaredParallelogramAreaTolerance: float -> other: Line3D -> ln: Line3D -> bool` |
| — | *vertical direction* | `.IsHorizontal` | `bool` |
| — | *vertical direction* | `Line3D.isHorizontal` | `l: Line3D -> bool` |
| `.IsNormalTo` | `other: Line2D * ?maxTangent: float<tangent> -> bool` | `.IsNormalTo` | `other: Line3D * ?maxTangent: float<tangent> -> bool` |
| `.IsNormalTo` | `other: Vc * ?maxTangent: float<tangent> -> bool` | `.IsNormalTo` | `other: Vec * ?maxTangent: float<tangent> -> bool` |
| `.IsNormalTo` | `other: UnitVc * ?maxTangent: float<tangent> -> bool` | `.IsNormalTo` | `other: UnitVec * ?maxTangent: float<tangent> -> bool` |
| `Line2D.isNormalTo` | `maxTangent: float<tangent> -> lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isNormalTo` | `maxTangent: float<tangent> -> lnA: Line3D -> lnB: Line3D -> bool` |
| `.IsParallelAndOpposingToFast` | `other: Line2D * ?parallelogramAreaTolerance: float * ?maxDotProduct: float -> bool` | `.IsParallelAndOpposingToFast` | `other: Line3D * ?squaredParallelogramAreaTolerance: float * ?maxDotProduct: float -> bool` |
| `Line2D.isParallelAndOpposingToFast` | `parallelogramAreaTolerance: float -> maxDotProduct: float -> lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isParallelAndOpposingToFast` | `?squaredParallelogramAreaTolerance: float -> ?maxDotProduct: float -> other: Line3D -> ln: Line3D -> bool` |
| `.IsParallelAndOrientedTo` | `other: Line2D * ?minTangent: float<tangent> -> bool` | `.IsParallelAndOrientedTo` | `other: Line3D * ?minTangent: float<tangent> -> bool` |
| `.IsParallelAndOrientedTo` | `other: Vc * ?minTangent: float<tangent> -> bool` | `.IsParallelAndOrientedTo` | `other: Vec * ?minTangent: float<tangent> -> bool` |
| `.IsParallelAndOrientedTo` | `other: UnitVc * ?minTangent: float<tangent> -> bool` | `.IsParallelAndOrientedTo` | `other: UnitVec * ?minTangent: float<tangent> -> bool` |
| `Line2D.isParallelAndOrientedTo` | `minTangent: float<tangent> -> lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isParallelAndOrientedTo` | `minTangent: float<tangent> -> lnA: Line3D -> lnB: Line3D -> bool` |
| `.IsParallelAndOrientedToFast` | `other: Line2D * ?parallelogramAreaTolerance: float * ?minDotProduct: float -> bool` | `.IsParallelAndOrientedToFast` | `other: Line3D * ?squaredParallelogramAreaTolerance: float * ?minDotProduct: float -> bool` |
| `Line2D.isParallelAndOrientedToFast` | `parallelogramAreaTolerance: float -> minDotProduct: float -> lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isParallelAndOrientedToFast` | `?squaredParallelogramAreaTolerance: float -> ?minDotProduct: float -> other: Line3D -> ln: Line3D -> bool` |
| `.IsParallelTo` | `other: Line2D * ?minTangent: float<tangent> -> bool` | `.IsParallelTo` | `other: Line3D * ?minTangent: float<tangent> -> bool` |
| `.IsParallelTo` | `other: Vc * ?minTangent: float<tangent> -> bool` | `.IsParallelTo` | `other: Vec * ?minTangent: float<tangent> -> bool` |
| `.IsParallelTo` | `other: UnitVc * ?minTangent: float<tangent> -> bool` | `.IsParallelTo` | `other: UnitVec * ?minTangent: float<tangent> -> bool` |
| `Line2D.isParallelTo` | `minTangent: float<tangent> -> lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isParallelTo` | `minTangent: float<tangent> -> lnA: Line3D -> lnB: Line3D -> bool` |
| `.IsParallelToFast` | `other: Line2D * ?parallelogramAreaTolerance: float -> bool` | `.IsParallelToFast` | `other: Line3D * ?squaredParallelogramAreaTolerance: float -> bool` |
| `Line2D.isParallelToFast` | `parallelogramAreaTolerance: float -> lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isParallelToFast` | `?squaredParallelogramAreaTolerance: float -> other: Line3D -> ln: Line3D -> bool` |
| `.IsPerpendicularTo` | `other: Line2D * ?maxTangent: float<tangent> -> bool` | `.IsPerpendicularTo` | `other: Line3D * ?maxTangent: float<tangent> -> bool` |
| `.IsPerpendicularTo` | `other: Vc * ?maxTangent: float<tangent> -> bool` | `.IsPerpendicularTo` | `other: Vec * ?maxTangent: float<tangent> -> bool` |
| `.IsPerpendicularTo` | `other: UnitVc * ?maxTangent: float<tangent> -> bool` | `.IsPerpendicularTo` | `other: UnitVec * ?maxTangent: float<tangent> -> bool` |
| `Line2D.isPerpendicularTo` | `maxTangent: float<tangent> -> lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isPerpendicularTo` | `maxTangent: float<tangent> -> lnA: Line3D -> lnB: Line3D -> bool` |
| `.IsPointOnLeft` | `pt: Pt -> bool` | — | *side of a line* |
| `Line2D.isPointOnLeft` | `pt: Pt -> ln: Line2D -> bool` | — | *side of a line* |
| `.IsPointOnRight` | `pt: Pt -> bool` | — | *side of a line* |
| `Line2D.isPointOnRight` | `pt: Pt -> ln: Line2D -> bool` | — | *side of a line* |
| `.IsTiny` | `tol: float -> bool` | `.IsTiny` | `tol: float -> bool` |
| `Line2D.isTiny` | `tol: float -> l: Line2D -> bool` | `Line3D.isTiny` | `tol: float -> l: Line3D -> bool` |
| `.IsTinySq` | `tol: float -> bool` | `.IsTinySq` | `tol: float -> bool` |
| `Line2D.isTinySq` | `tol: float -> l: Line2D -> bool` | `Line3D.isTinySq` | `tol: float -> l: Line3D -> bool` |
| — | *vertical direction* | `.IsVertical` | `bool` |
| — | *vertical direction* | `Line3D.isVertical` | `l: Line3D -> bool` |
| `.IsXAligned` | `bool` | `.IsXAligned` | `bool` |
| `Line2D.isXAligned` | `l: Line2D -> bool` | `Line3D.isXAligned` | `l: Line3D -> bool` |
| `.IsXYOnLeft` | `x: float * y: float -> bool` | — | *side of a line* |
| `Line2D.isXYOnLeft` | `x: float -> y: float -> ln: Line2D -> bool` | — | *side of a line* |
| `.IsXYOnRight` | `x: float * y: float -> bool` | — | *side of a line* |
| `Line2D.isXYOnRight` | `x: float -> y: float -> ln: Line2D -> bool` | — | *side of a line* |
| `.IsYAligned` | `bool` | `.IsYAligned` | `bool` |
| `Line2D.isYAligned` | `l: Line2D -> bool` | `Line3D.isYAligned` | `l: Line3D -> bool` |
| — | *Z axis* | `.IsZAligned` | `bool` |
| — | *Z axis* | `Line3D.isZAligned` | `l: Line3D -> bool` |
| `.IsZeroLength` | `bool` | `.IsZeroLength` | `bool` |
| `Line2D.isZeroLength` | `l: Line2D -> bool` | `Line3D.isZeroLength` | `l: Line3D -> bool` |
| `.Length` | `float` | `.Length` | `float` |
| `Line2D.length` | `l: Line2D -> float` | `Line3D.length` | `l: Line3D -> float` |
| `.LengthFromParam` | `t: float -> float` | `.LengthFromParam` | `t: float -> float` |
| `Line2D.lengthFromParam` | `t: float -> ln: Line2D -> float` | `Line3D.lengthFromParam` | `t: float -> ln: Line3D -> float` |
| `.LengthSq` | `float` | `.LengthSq` | `float` |
| `Line2D.lengthSq` | `l: Line2D -> float` | `Line3D.lengthSq` | `l: Line3D -> float` |
| `.LengthTillParam` | `p: float -> float` | `.LengthTillParam` | `p: float -> float` |
| `Line2D.lengthTillParam` | `p: float -> ln: Line2D -> float` | `Line3D.lengthTillParam` | `p: float -> ln: Line3D -> float` |
| `.MatchesOrientation` | `otherLn: Line2D -> bool` | `.MatchesOrientation` | `otherLn: Line3D -> bool` |
| `.MatchesOrientation` | `v: Vc -> bool` | `.MatchesOrientation` | `v: Vec -> bool` |
| `.MatchesOrientation` | `v: UnitVc -> bool` | `.MatchesOrientation` | `v: UnitVec -> bool` |
| `Line2D.matchesOrientation` | `l: Line2D -> ln: Line2D -> bool` | `Line3D.matchesOrientation` | `l: Line3D -> ln: Line3D -> bool` |
| `.MatchesOrientation45` | `l: Line2D -> bool` | `.MatchesOrientation45` | `l: Line3D -> bool` |
| `Line2D.matchesOrientation45` | `l: Line2D -> ln: Line2D -> bool` | `Line3D.matchesOrientation45` | `l: Line3D -> ln: Line3D -> bool` |
| `.Mid` | `Pt` | `.Mid` | `Pnt` |
| `Line2D.mid` | `ln: Line2D -> Pt` | `Line3D.mid` | `ln: Line3D -> Pnt` |
| `.Move` | `v: Vc -> Line2D` | `.Move` | `v: Vec -> Line3D` |
| `Line2D.move` | `v: Vc -> ln: Line2D -> Line2D` | `Line3D.move` | `v: Vec -> ln: Line3D -> Line3D` |
| `.MoveX` | `distance: float -> Line2D` | `.MoveX` | `distance: float -> Line3D` |
| `Line2D.moveX` | `distance: float -> ln: Line2D -> Line2D` | `Line3D.moveX` | `distance: float -> ln: Line3D -> Line3D` |
| `.MoveY` | `distance: float -> Line2D` | `.MoveY` | `distance: float -> Line3D` |
| `Line2D.moveY` | `distance: float -> ln: Line2D -> Line2D` | `Line3D.moveY` | `distance: float -> ln: Line3D -> Line3D` |
| — | *Z axis* | `.MoveZ` | `distance: float -> Line3D` |
| — | *Z axis* | `Line3D.moveZ` | `distance: float -> ln: Line3D -> Line3D` |
| `.RayClosestParameter` | `pt: Pt -> float` | `.RayClosestParameter` | `p: Pnt -> float` |
| `Line2D.rayClosestParameter` | `p: Pt -> ln: Line2D -> float` | `Line3D.rayClosestParameter` | `p: Pnt -> ln: Line3D -> float` |
| — | *rays in 3D are mostly skew* | `.RayClosestParameters` | `lnB: Line3D -> (float * float) option` |
| — | *rays in 3D are mostly skew* | `Line3D.rayClosestParameters` | `lnA: Line3D -> lnB: Line3D -> (float * float) option` |
| `.RayClosestParameterXY` | `x: float * y: float -> float` | `.RayClosestParameterXYZ` | `x: float * y: float * z: float -> float` |
| `Line2D.rayClosestParameterXY` | `x: float -> y: float -> ln: Line2D -> float` | `Line3D.rayClosestParameterXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
| `.RayClosestPoint` | `pt: Pt -> Pt` | `.RayClosestPoint` | `p: Pnt -> Pnt` |
| `Line2D.rayClosestPoint` | `p: Pt -> ln: Line2D -> Pt` | `Line3D.rayClosestPoint` | `p: Pnt -> ln: Line3D -> Pnt` |
| — | *rays in 3D are mostly skew* | `.RayClosestPoints` | `lnB: Line3D -> (Pnt * Pnt) option` |
| — | *rays in 3D are mostly skew* | `Line3D.rayClosestPoints` | `lnA: Line3D -> lnB: Line3D -> (Pnt * Pnt) option` |
| `.Reversed` | `Line2D` | `.Reversed` | `Line3D` |
| `Line2D.reversed` | `ln: Line2D -> Line2D` | `Line3D.reversed` | `ln: Line3D -> Line3D` |
| `.Rotate` | `r: Rotation2D -> Line2D` | `.Rotate` | `q: Quaternion -> Line3D` |
| `Line2D.rotate` | `r: Rotation2D -> ln: Line2D -> Line2D` | `Line3D.rotate` | `q: Quaternion -> ln: Line3D -> Line3D` |
| `.RotateWithCenter` | `cen: Pt * r: Rotation2D -> Line2D` | `.RotateWithCenter` | `cen: Pnt * q: Quaternion -> Line3D` |
| `Line2D.rotateWithCenter` | `cen: Pt -> r: Rotation2D -> ln: Line2D -> Line2D` | `Line3D.rotateWithCenter` | `cen: Pnt -> q: Quaternion -> ln: Line3D -> Line3D` |
| `.Scale` | `factor: float -> Line2D` | `.Scale` | `factor: float -> Line3D` |
| `Line2D.scale` | `factor: float -> ln: Line2D -> Line2D` | `Line3D.scale` | `factor: float -> ln: Line3D -> Line3D` |
| `.ScaleOn` | `cen: Pt -> factor: float -> Line2D` | `.ScaleOn` | `cen: Pnt -> factor: float -> Line3D` |
| `Line2D.scaleOn` | `cen: Pt -> factor: float -> ln: Line2D -> Line2D` | `Line3D.scaleOn` | `cen: Pnt -> factor: float -> ln: Line3D -> Line3D` |
| `.Segment` | `start: float * endParameter: float -> Line2D` | `.Segment` | `a: float * b: float -> Line3D` |
| `Line2D.segment` | `start: float -> endParameter: float -> ln: Line2D -> Line2D` | `Line3D.segment` | `a: float -> b: float -> ln: Line3D -> Line3D` |
| `.Shrink` | `distAtStart: float * distAtEnd: float -> Line2D` | `.Shrink` | `distAtStart: float * distAtEnd: float -> Line3D` |
| `Line2D.shrink` | `distAtStart: float -> distAtEnd: float -> ln: Line2D -> Line2D` | `Line3D.shrink` | `distAtStart: float -> distAtEnd: float -> ln: Line3D -> Line3D` |
| `.ShrinkEnd` | `distAtEnd: float -> Line2D` | `.ShrinkEnd` | `distAtEnd: float -> Line3D` |
| `Line2D.shrinkEnd` | `distAtEnd: float -> ln: Line2D -> Line2D` | `Line3D.shrinkEnd` | `distAtEnd: float -> ln: Line3D -> Line3D` |
| `.ShrinkStart` | `distAtStart: float -> Line2D` | `.ShrinkStart` | `distAtStart: float -> Line3D` |
| `Line2D.shrinkStart` | `distAtStart: float -> ln: Line2D -> Line2D` | `Line3D.shrinkStart` | `distAtStart: float -> ln: Line3D -> Line3D` |
| `.SqDistanceRayPoint` | `p: Pt -> float` | `.SqDistanceRayPoint` | `p: Pnt -> float` |
| `Line2D.sqDistanceRayPoint` | `p: Pt -> ln: Line2D -> float` | `Line3D.sqDistanceRayPoint` | `p: Pnt -> ln: Line3D -> float` |
| `.SqDistanceRayXY` | `x: float * y: float -> float` | `.SqDistanceRayXYZ` | `x: float * y: float * z: float -> float` |
| `Line2D.sqDistanceRayXY` | `x: float -> y: float -> ln: Line2D -> float` | `Line3D.sqDistanceRayXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
| `.SqDistanceToPt` | `p: Pt -> float` | `.SqDistanceToPnt` | `p: Pnt -> float` |
| `Line2D.sqDistanceToPt` | `p: Pt -> ln: Line2D -> float` | `Line3D.sqDistanceToPnt` | `p: Pnt -> ln: Line3D -> float` |
| `.SqDistanceToXY` | `x: float * y: float -> float` | `.SqDistanceToXYZ` | `x: float * y: float * z: float -> float` |
| `Line2D.sqDistanceToXY` | `x: float -> y: float -> ln: Line2D -> float` | `Line3D.sqDistanceToXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
| `.Start` | `Pt` | `.Start` | `Pnt` |
| `Line2D.start` | `l: Line2D -> Pt` | `Line3D.start` | `l: Line3D -> Pnt` |
| `.SubLine` | `start: float * endParameter: float -> Line2D` | `.SubLine` | `start: float * endParameter: float -> Line3D` |
| `Line2D.subLine` | `start: float -> endParameter: float -> ln: Line2D -> Line2D` | `Line3D.subLine` | `start: float -> endParameter: float -> ln: Line3D -> Line3D` |
| `.Tangent` | `Vc` | `.Tangent` | `Vec` |
| `Line2D.tangent` | `ln: Line2D -> Vc` | `Line3D.tangent` | `ln: Line3D -> Vec` |
| `.To` | `Pt` | `.To` | `Pnt` |
| `.ToString` | `unit -> string` | `.ToString` | `unit -> string` |
| — | *transformation matrix* | `.Transform` | `m: Matrix -> Line3D` |
| — | *transformation matrix* | `Line3D.transform` | `m: Matrix -> ln: Line3D -> Line3D` |
| — | *transformation matrix* | `.TransformRigid` | `m: RigidMatrix -> Line3D` |
| — | *transformation matrix* | `Line3D.transformRigid` | `m: RigidMatrix -> ln: Line3D -> Line3D` |
| `.UnitTangent` | `UnitVc` | `.UnitTangent` | `UnitVec` |
| `Line2D.unitTangent` | `ln: Line2D -> UnitVc` | `Line3D.unitTangent` | `ln: Line3D -> UnitVec` |
| `.Vector` | `Vc` | `.Vector` | `Vec` |
| `Line2D.vector` | `ln: Line2D -> Vc` | `Line3D.vector` | `ln: Line3D -> Vec` |
| `.VectorX` | `float` | `.VectorX` | `float` |
| `Line2D.vectorX` | `l: Line2D -> float` | `Line3D.vectorX` | `ln: Line3D -> float` |
| `.VectorY` | `float` | `.VectorY` | `float` |
| `Line2D.vectorY` | `l: Line2D -> float` | `Line3D.vectorY` | `ln: Line3D -> float` |
| — | *Z axis* | `.VectorZ` | `float` |
| — | *Z axis* | `Line3D.vectorZ` | `ln: Line3D -> float` |
| **Other static members** | | | |
| `Line2D.createFromLine3D` | `ln: Line3D -> Line2D` | `Line3D.createFromLine2D` | `ln: Line2D -> Line3D` |
| — | *Z axis* | `Line3D.createFromLine2DWithZ` | `zLevel: float -> ln: Line2D -> Line3D` |
| `Line2D.createFromPtAndVc` | `p: Pt * v: Vc -> Line2D` | `Line3D.createFromPntAndVec` | `p: Pnt * v: Vec -> Line3D` |
| `Line2D.createFromVec` | `v: Vc -> Line2D` | `Line3D.createFromVec` | `v: Vec -> Line3D` |
| `Line2D.cross` | `a: Line2D -> b: Line2D -> float` | — | *scalar cross product* |
| `Line2D.det` | `a: Line2D -> b: Line2D -> float` | — | *scalar cross product* |
| `Line2D.distanceToLine` | `lnA: Line2D -> lnB: Line2D -> float` | `Line3D.distanceToLine` | `lnA: Line3D -> lnB: Line3D -> float` |
| `Line2D.divide` | `segments: int -> ln: Line2D -> Pt array` | `Line3D.divide` | `segments: int -> ln: Line3D -> Pnt array` |
| `Line2D.divideEvery` | `dist: float -> l: Line2D -> ResizeArray<Pt>` | `Line3D.divideEvery` | `dist: float -> l: Line3D -> ResizeArray<Pnt>` |
| `Line2D.divideInsideEvery` | `dist: float -> l: Line2D -> ResizeArray<Pt>` | `Line3D.divideInsideEvery` | `dist: float -> l: Line3D -> ResizeArray<Pnt>` |
| `Line2D.divideMaxLength` | `maxSegmentLength: float -> ln: Line2D -> Pt array` | `Line3D.divideMaxLength` | `maxSegmentLength: float -> ln: Line3D -> Pnt array` |
| `Line2D.divideMinLength` | `minSegmentLength: float -> ln: Line2D -> Pt array` | `Line3D.divideMinLength` | `minSegmentLength: float -> ln: Line3D -> Pnt array` |
| `Line2D.doIntersect` | `lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.doIntersect` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line2D.doIntersectOrOverlap` | `lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.doIntersectOrOverlap` | `lnA: Line3D -> lnB: Line3D -> bool` |
| — | *rays in 3D are mostly skew* | `Line3D.doRaysIntersect` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line2D.end'` | `l: Line2D -> Pt` | `Line3D.end'` | `l: Line3D -> Pnt` |
| `Line2D.equals` | `tol: float -> a: Line2D -> b: Line2D -> bool` | `Line3D.equals` | `tol: float -> a: Line3D -> b: Line3D -> bool` |
| `Line2D.flip` | `ln: Line2D -> Line2D` | `Line3D.flip` | `ln: Line3D -> Line3D` |
| `Line2D.fromX` | `l: Line2D -> float` | `Line3D.fromX` | `l: Line3D -> float` |
| `Line2D.fromY` | `l: Line2D -> float` | `Line3D.fromY` | `l: Line3D -> float` |
| — | *Z axis* | `Line3D.fromZ` | `l: Line3D -> float` |
| — | *cone* | `Line3D.intersectCone` | `ray: Line3D * coneRadius: float * coneBaseZ: float * coneTipZ: float -> Option<float * float>` |
| `Line2D.intersectRays` | `lnA: Line2D -> lnB: Line2D -> Pt` | — | *rays in 3D are mostly skew* |
| `Line2D.isNormalTo'` | `lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isNormalTo'` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line2D.isNormalToUnitVc` | `maxTangent: float<tangent> -> other: UnitVc -> ln: Line2D -> bool` | `Line3D.isNormalToUnitVec` | `maxTangent: float<tangent> -> other: UnitVec -> ln: Line3D -> bool` |
| `Line2D.isNormalToVc` | `maxTangent: float<tangent> -> other: Vc -> ln: Line2D -> bool` | `Line3D.isNormalToVec` | `maxTangent: float<tangent> -> other: Vec -> ln: Line3D -> bool` |
| `Line2D.isNotParallelTo` | `lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isNotParallelTo` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line2D.isNotParallelTo'` | `lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isNotParallelTo'` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line2D.isParallelAndOrientedToUnitVc` | `minTangent: float<tangent> -> other: UnitVc -> ln: Line2D -> bool` | `Line3D.isParallelAndOrientedToUnitVec` | `minTangent: float<tangent> -> other: UnitVec -> ln: Line3D -> bool` |
| `Line2D.isParallelAndOrientedToVc` | `minTangent: float<tangent> -> other: Vc -> ln: Line2D -> bool` | `Line3D.isParallelAndOrientedToVec` | `minTangent: float<tangent> -> other: Vec -> ln: Line3D -> bool` |
| `Line2D.isParallelTo'` | `lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isParallelTo'` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line2D.isParallelToUnitVc` | `minTangent: float<tangent> -> other: UnitVc -> ln: Line2D -> bool` | `Line3D.isParallelToUnitVec` | `minTangent: float<tangent> -> other: UnitVec -> ln: Line3D -> bool` |
| `Line2D.isParallelToVc` | `minTangent: float<tangent> -> other: Vc -> ln: Line2D -> bool` | `Line3D.isParallelToVec` | `minTangent: float<tangent> -> other: Vec -> ln: Line3D -> bool` |
| `Line2D.isPerpendicularTo'` | `lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isPerpendicularTo'` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line2D.isPerpendicularToUnitVc` | `maxTangent: float<tangent> -> other: UnitVc -> ln: Line2D -> bool` | `Line3D.isPerpendicularToUnitVec` | `maxTangent: float<tangent> -> other: UnitVec -> ln: Line3D -> bool` |
| `Line2D.isPerpendicularToVc` | `maxTangent: float<tangent> -> other: Vc -> ln: Line2D -> bool` | `Line3D.isPerpendicularToVec` | `maxTangent: float<tangent> -> other: Vec -> ln: Line3D -> bool` |
| `Line2D.isTouchingEndOf` | `squareTolerance: float -> a: Line2D -> b: Line2D -> bool` | `Line3D.isTouchingEndOf` | `squareTolerance: float -> a: Line3D -> b: Line3D -> bool` |
| `Line2D.lengthToPtOnLine` | `ln: Line2D -> pt: Pt -> float` | `Line3D.lengthToPtOnLine` | `ln: Line3D -> pt: Pnt -> float` |
| `Line2D.matchesOrientationUnitVc` | `v: UnitVc -> ln: Line2D -> bool` | `Line3D.matchesOrientationUnitVec` | `v: UnitVec -> ln: Line3D -> bool` |
| `Line2D.matchesOrientationVc` | `v: Vc -> ln: Line2D -> bool` | `Line3D.matchesOrientationVec` | `v: Vec -> ln: Line3D -> bool` |
| `Line2D.matchOrientation` | `orientationToMatch: Line2D -> lineToFlip: Line2D -> Line2D` | `Line3D.matchOrientation` | `orientationToMatch: Line3D -> lineToFlip: Line3D -> Line3D` |
| `Line2D.matchUnitVcOrientation` | `orientationToMatch: UnitVc -> lineToFlip: Line2D -> Line2D` | `Line3D.matchUnitVecOrientation` | `orientationToMatch: UnitVec -> lineToFlip: Line3D -> Line3D` |
| `Line2D.matchVcOrientation` | `orientationToMatch: Vc -> lineToFlip: Line2D -> Line2D` | `Line3D.matchVecOrientation` | `orientationToMatch: Vec -> lineToFlip: Line3D -> Line3D` |
| `Line2D.notEquals` | `tol: float -> a: Line2D -> b: Line2D -> bool` | `Line3D.notEquals` | `tol: float -> a: Line3D -> b: Line3D -> bool` |
| `Line2D.offset` | `distance: float -> ln: Line2D -> Line2D` | `Line3D.offset` | `distHorizontal: float -> distNormal: float -> l: Line3D -> Line3D` |
| — | *projection to the XY plane* | `Line3D.offsetXY` | `amount: float -> ln: Line3D -> Line3D` |
| `Line2D.pointAtDistance` | `dist: float -> ln: Line2D -> Pt` | `Line3D.pointAtDistance` | `dist: float -> ln: Line3D -> Pnt` |
| `Line2D.pointAtDistanceFromEnd` | `dist: float -> ln: Line2D -> Pt` | `Line3D.pointAtDistanceFromEnd` | `dist: float -> ln: Line3D -> Pnt` |
| `Line2D.projectOntoRay` | `rayToProjectOnto: Line2D -> lineToProject: Line2D -> Line2D` | `Line3D.projectOntoRay` | `rayToProjectOnto: Line3D -> lineToProject: Line3D -> Line3D` |
| `Line2D.projectOntoRayParam` | `rayToProjectOnto: Line2D -> lineToProject: Line2D -> float * float` | `Line3D.projectOntoRayParam` | `rayToProjectOnto: Line3D -> lineToProject: Line3D -> float * float` |
| `Line2D.reverse` | `ln: Line2D -> Line2D` | `Line3D.reverse` | `ln: Line3D -> Line3D` |
| — | *rotation about a world axis* | `Line3D.rotateOnZ` | `r: Rotation2D -> ln: Line3D -> Line3D` |
| — | *rotation about a world axis* | `Line3D.rotateOnZWithCenter` | `cen: Pnt -> r: Rotation2D -> ln: Line3D -> Line3D` |
| `Line2D.split` | `gap: float -> segments: int -> ln: Line2D -> Line2D array` | `Line3D.split` | `gap: float -> segments: int -> ln: Line3D -> Line3D array` |
| `Line2D.splitMaxLength` | `gap: float -> maxSegmentLength: float -> ln: Line2D -> Line2D array` | `Line3D.splitMaxLength` | `gap: float -> maxSegmentLength: float -> ln: Line3D -> Line3D array` |
| `Line2D.splitMinLength` | `gap: float -> minSegmentLength: float -> ln: Line2D -> Line2D array` | `Line3D.splitMinLength` | `gap: float -> minSegmentLength: float -> ln: Line3D -> Line3D array` |
| `Line2D.sqDistanceToLine` | `lnA: Line2D -> lnB: Line2D -> float` | `Line3D.sqDistanceToLine` | `lnA: Line3D -> lnB: Line3D -> float` |
| `Line2D.to'` | `l: Line2D -> Pt` | `Line3D.to'` | `l: Line3D -> Pnt` |
| `Line2D.toLine3D` | `?zLevel: float -> ln: Line2D -> Line3D` | `Line3D.toLine2D` | `ln: Line3D -> Line2D` |
| `Line2D.toX` | `l: Line2D -> float` | `Line3D.toX` | `l: Line3D -> float` |
| `Line2D.toY` | `l: Line2D -> float` | `Line3D.toY` | `l: Line3D -> float` |
| — | *Z axis* | `Line3D.toZ` | `l: Line3D -> float` |
| `Line2D.translate` | `v: Vc -> ln: Line2D -> Line2D` | `Line3D.translate` | `v: Vec -> ln: Line3D -> Line3D` |
| `Line2D.tryGetOverlap` | `lnA: Line2D -> lnB: Line2D -> (float * float) option` | `Line3D.tryGetOverlap` | `lnA: Line3D -> lnB: Line3D -> (float * float) option` |
| `Line2D.tryIntersect` | `lnA: Line2D -> lnB: Line2D -> Pt voption` | `Line3D.tryIntersect` | `lnA: Line3D -> lnB: Line3D -> Pnt voption` |
| `Line2D.tryIntersectOrOverlap` | `lnA: Line2D -> lnB: Line2D -> Pt option` | `Line3D.tryIntersectOrOverlap` | `lnA: Line3D -> lnB: Line3D -> Pnt option` |
| `Line2D.tryIntersectRay` | `lineA: Line2D -> lineB: Line2D -> Pt voption` | `Line3D.tryIntersectRay` | `lineA: Line3D -> lineB: Line3D -> Pnt voption` |
| `Line2D.tryProjectOntoLine` | `onToLine: Line2D -> lineToProject: Line2D -> Line2D option` | `Line3D.tryProjectOntoLine` | `onToLine: Line3D -> lineToProject: Line3D -> Line3D option` |
| `Line2D.tryProjectOntoLineParam` | `onToLine: Line2D -> lineToProject: Line2D -> (float * float) option` | `Line3D.tryProjectOntoLineParam` | `onToLine: Line3D -> lineToProject: Line3D -> (float * float) option` |
| `Line2D.withEnd` | `pt: Pt -> ln: Line2D -> Line2D` | `Line3D.withEnd` | `pt: Pnt -> ln: Line3D -> Line3D` |
| `Line2D.withFrom` | `pt: Pt -> ln: Line2D -> Line2D` | `Line3D.withFrom` | `pt: Pnt -> ln: Line3D -> Line3D` |
| `Line2D.withLengthFromMid` | `len: float -> ln: Line2D -> Line2D` | `Line3D.withLengthFromMid` | `len: float -> ln: Line3D -> Line3D` |
| `Line2D.withLengthFromStart` | `len: float -> ln: Line2D -> Line2D` | `Line3D.withLengthFromStart` | `len: float -> ln: Line3D -> Line3D` |
| `Line2D.withLengthToEnd` | `len: float -> ln: Line2D -> Line2D` | `Line3D.withLengthToEnd` | `len: float -> ln: Line3D -> Line3D` |
| `Line2D.withStart` | `pt: Pt -> ln: Line2D -> Line2D` | `Line3D.withStart` | `pt: Pnt -> ln: Line3D -> Line3D` |
| `Line2D.withTo` | `pt: Pt -> ln: Line2D -> Line2D` | `Line3D.withTo` | `pt: Pnt -> ln: Line3D -> Line3D` |

## XLine2D.XRayParam / XLine3D.XRayParam

| XRayParam | Signature | XRayParam | Signature |
|:--|:--|:--|:--|
| `Intersect` | `twoParams: float * float -> XRayParam` | `SkewOrX` | `twoParams: float * float -> XRayParam` |
| `Parallel` | `XRayParam` | `Parallel` | `XRayParam` |
| `TooShortA` | `XRayParam` | `TooShortA` | `XRayParam` |
| `TooShortB` | `XRayParam` | `TooShortB` | `XRayParam` |
| `TooShortBoth` | `XRayParam` | `TooShortBoth` | `XRayParam` |

## XLine2D.XRay / XLine3D.XRay

| XRay | Signature | XRay | Signature |
|:--|:--|:--|:--|
| `Intersect` | `xPt: Pt -> XRay` | `Intersect` | `xPt: Pnt -> XRay` |
| `Parallel` | `XRay` | `Parallel` | `XRay` |
| `TooShortA` | `XRay` | `TooShortA` | `XRay` |
| `TooShortB` | `XRay` | `TooShortB` | `XRay` |
| `TooShortBoth` | `XRay` | `TooShortBoth` | `XRay` |
| — | *rays in 3D are mostly skew* | `Skew` | `closestPtA: Pnt * closestPtB: Pnt * squareDist: float -> XRay` |

## XLine2D.XParam / XLine3D.XParam

| XParam | Signature | XParam | Signature |
|:--|:--|:--|:--|
| `Intersect` | `twoParams: float * float -> XParam` | `Intersect` | `twoParams: float * float -> XParam` |
| `Apart` | `XParam` | `Apart` | `XParam` |
| `Parallel` | `XParam` | `Parallel` | `XParam` |
| `TooShortA` | `XParam` | `TooShortA` | `XParam` |
| `TooShortB` | `XParam` | `TooShortB` | `XParam` |
| `TooShortBoth` | `XParam` | `TooShortBoth` | `XParam` |
| — | *rays in 3D are mostly skew* | `Skew` | `paramA: float * paramB: float * squareDist: float -> XParam` |

## XLine2D.XPt / XLine3D.XPnt

| XPt | Signature | XPnt | Signature |
|:--|:--|:--|:--|
| `Intersect` | `xPt: Pt -> XPt` | `Intersect` | `xPnt: Pnt -> XPnt` |
| `Apart` | `XPt` | `Apart` | `XPnt` |
| `Parallel` | `XPt` | `Parallel` | `XPnt` |
| `TooShortA` | `XPt` | `TooShortA` | `XPnt` |
| `TooShortB` | `XPt` | `TooShortB` | `XPnt` |
| `TooShortBoth` | `XPt` | `TooShortBoth` | `XPnt` |
| — | *rays in 3D are mostly skew* | `Skew` | `closestPntA: Pnt * closestPntB: Pnt * squareDist: float -> XPnt` |

## XLine2D.ClParams / XLine3D.ClParams

| ClParams | Signature | ClParams | Signature |
|:--|:--|:--|:--|
| `Intersect` | `paramA: float * paramB: float -> ClParams` | `Intersect` | `paramA: float * paramB: float -> ClParams` |
| `Parallel` | `paramA: float * paramB: float -> ClParams` | `Parallel` | `paramA: float * paramB: float -> ClParams` |
| `Apart` | `paramA: float * paramB: float -> ClParams` | `Apart` | `paramA: float * paramB: float * squareDist: float -> ClParams` |
| `TooShortA` | `ClParams` | `TooShortA` | `ClParams` |
| `TooShortB` | `ClParams` | `TooShortB` | `ClParams` |
| `TooShortBoth` | `ClParams` | `TooShortBoth` | `ClParams` |
| — | *rays in 3D are mostly skew* | `Skew` | `paramA: float * paramB: float * squareDist: float -> ClParams` |

## XLine2D.ClPts

| ClPts | Signature |
|:--|:--|
| `Intersect` | `pt: Pt -> ClPts` |
| `Parallel` | `ptA: Pt * ptB: Pt -> ClPts` |
| `Apart` | `ptA: Pt * ptB: Pt -> ClPts` |
| `TooShortA` | `ClPts` |
| `TooShortB` | `ClPts` |
| `TooShortBoth` | `ClPts` |

## XLine2D.XEnds / XLine3D.XEnds

| XEnds | Signature | XEnds | Signature |
|:--|:--|:--|:--|
| `NotTouching` | `XEnds` | `NotTouching` | `XEnds` |
| `StartA_StartB` | `XEnds` | `StartA_StartB` | `XEnds` |
| `EndA_EndB` | `XEnds` | `EndA_EndB` | `XEnds` |
| `EndA_StartB` | `XEnds` | `EndA_StartB` | `XEnds` |
| `StartA_EndB` | `XEnds` | `StartA_EndB` | `XEnds` |
| `Identical` | `XEnds` | `Identical` | `XEnds` |
| `IdenticalFlipped` | `XEnds` | `IdenticalFlipped` | `XEnds` |

## module XLineXY / module XLineXYZ

| XLineXY | Signature | XLineXYZ | Signature |
|:--|:--|:--|:--|
| `XLineXY.closestParameters` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> struct (float * float)` |  |  |
| `XLineXY.doIntersect` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> bool` | `XLineXYZ.doIntersect` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> bool` |
| `XLineXY.doOverlap` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tolerance: float -> bool` | `XLineXYZ.doOverlap` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * tolerance: float -> bool` |
| — | *rays in 3D are mostly skew* | `XLineXYZ.doRaysIntersect` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float -> bool` |
| `XLineXY.getClosestParameters` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tangent: float<tangent> * tooShortTolerance: float -> ClParams` | `XLineXYZ.getClosestParameters` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * tangent: float<tangent> * tooShortTolerance: float -> ClParams` |
| `XLineXY.getClosestPoints` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tangent: float<tangent> * tooShortTolerance: float -> ClPts` | `XLineXYZ.getClosestPoints` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * tangent: float<tangent> * tooShortTolerance: float -> ClPts` |
| `XLineXY.getIntersection` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tangent: float<tangent> * tooShortTolerance: float -> XPt` | `XLineXYZ.getIntersection` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> XPnt` |
| `XLineXY.getIntersectionParam` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tangent: float<tangent> * tooShortTolerance: float -> XParam` | `XLineXYZ.getIntersectionParam` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> XParam` |
| — | *rays in 3D are mostly skew* | `XLineXYZ.getRayClosestParam` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * tangent: float<tangent> * tooShortTolerance: float -> XRayParam` |
| `XLineXY.getRayIntersection` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tangent: float<tangent> * tooShortTolerance: float -> XRay` | `XLineXYZ.getRayIntersection` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> XRay` |
| `XLineXY.getRayIntersectionParam` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tangent: float<tangent> * tooShortTolerance: float -> XRayParam` |  |  |
| `XLineXY.intersectRays` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> Pt` | — | *rays in 3D are mostly skew* |
| `XLineXY.isWithinRanges` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> bool` |  |  |
| `XLineXY.parameterA` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> float` | `XLineXYZ.parameterA` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> float` |
| `XLineXY.parameters` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> float * float` | `XLineXYZ.parameters` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> float * float` |
| `XLineXY.sqDistance` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> float` | `XLineXYZ.sqDistance` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> float` |
| `XLineXY.tryParameterA` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> float voption` | `XLineXYZ.tryClosestParameterRayA` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> float voption` |
| — | *rays in 3D are mostly skew* | `XLineXYZ.tryClosestPntRayA` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> Pnt voption` |
| `XLineXY.tryIntersect` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> Pt voption` | `XLineXYZ.tryIntersect` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> Pnt voption` |
| `XLineXY.tryIntersectInRangeA` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * minParameterA: float * maxParameterA: float -> Pt voption` | — | *rays in 3D are mostly skew* |
| `XLineXY.tryIntersectInRanges` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> Pt voption` | — | *rays in 3D are mostly skew* |
| `XLineXY.tryIntersectRay` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tangent: float<tangent> -> Pt voption` | `XLineXYZ.tryIntersectRay` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> Pnt voption` |

## XLine2D / XLine3D

| XLine2D | Signature | XLine3D | Signature |
|:--|:--|:--|:--|
| `XLine2D.closestParameters` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> struct (float * float)` |  |  |
| `XLine2D.closestParameters` | `lineA: Line2D * lineB: Line2D -> struct (float * float)` |  |  |
| `XLine2D.doIntersect` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> bool` | `XLine3D.doIntersect` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> bool` |
| `XLine2D.doIntersect` | `lineA: Line2D * lineB: Line2D -> bool` | `XLine3D.doIntersect` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> bool` |
| `XLine2D.doOverlap` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tolerance: float -> bool` | `XLine3D.doOverlap` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tolerance: float -> bool` |
| `XLine2D.doOverlap` | `lineA: Line2D * lineB: Line2D * ?tolerance: float -> bool` | `XLine3D.doOverlap` | `lineA: Line3D * lineB: Line3D * ?tolerance: float -> bool` |
| — | *rays in 3D are mostly skew* | `XLine3D.doRaysIntersect` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float -> bool` |
| — | *rays in 3D are mostly skew* | `XLine3D.doRaysIntersect` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float -> bool` |
| `XLine2D.getClosestParameters` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` | `XLine3D.getClosestParameters` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` |
| `XLine2D.getClosestParameters` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` | `XLine3D.getClosestParameters` | `lineA: Line3D * lineB: Line3D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` |
| `XLine2D.getClosestPoints` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPts` | `XLine3D.getClosestPoints` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPts` |
| `XLine2D.getClosestPoints` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPts` | `XLine3D.getClosestPoints` | `lineA: Line3D * lineB: Line3D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPts` |
| `XLine2D.getEndsTouching` | `a: Line2D * b: Line2D * ?tolerance: float -> XEnds` | `XLine3D.getEndsTouching` | `a: Line3D * b: Line3D * ?tolerance: float -> XEnds` |
| `XLine2D.getIntersection` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPt` | `XLine3D.getIntersection` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPnt` |
| `XLine2D.getIntersection` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPt` | `XLine3D.getIntersection` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPnt` |
| `XLine2D.getIntersectionParam` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` | `XLine3D.getIntersectionParam` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` |
| `XLine2D.getIntersectionParam` | `lnA: Line2D * lnB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` | `XLine3D.getIntersectionParam` | `lnA: Line3D * lnB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` |
| — | *rays in 3D are mostly skew* | `XLine3D.getRayClosestParam` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` |
| — | *rays in 3D are mostly skew* | `XLine3D.getRayClosestParam` | `lineA: Line3D * lineB: Line3D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` |
| `XLine2D.getRayIntersection` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` | `XLine3D.getRayIntersection` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` |
| `XLine2D.getRayIntersection` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` | `XLine3D.getRayIntersection` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` |
| `XLine2D.getRayIntersectionParam` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` |  |  |
| `XLine2D.getRayIntersectionParam` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` |  |  |
| `XLine2D.getSqDistance` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> float` | `XLine3D.getSqDistance` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec -> float` |
| `XLine2D.getSqDistance` | `lineA: Line2D * lineB: Line2D -> float` | `XLine3D.getSqDistance` | `lineA: Line3D * lineB: Line3D -> float` |
| — | *cone* | `XLine3D.intersectCone` | `ray: Line3D * coneRadius: float * coneBaseZ: float * coneTipZ: float -> XCone` |
| `XLine2D.intersectRays` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> Pt` | — | *rays in 3D are mostly skew* |
| `XLine2D.intersectRays` | `lineA: Line2D * lineB: Line2D -> Pt` | — | *rays in 3D are mostly skew* |
| `XLine2D.isWithinRanges` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> bool` |  |  |
| `XLine2D.isWithinRanges` | `lineA: Line2D * lineB: Line2D * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> bool` |  |  |
| `XLine2D.parameterA` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> float` | `XLine3D.parameterA` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec -> float` |
| `XLine2D.parameterA` | `lineA: Line2D * lineB: Line2D -> float` | `XLine3D.parameterA` | `lineA: Line3D * lineB: Line3D -> float` |
| `XLine2D.parameters` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> float * float` | `XLine3D.parameters` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec -> float * float` |
| `XLine2D.parameters` | `lineA: Line2D * lineB: Line2D -> float * float` | `XLine3D.parameters` | `lineA: Line3D * lineB: Line3D -> float * float` |
| `XLine2D.tryParameterA` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> float voption` | `XLine3D.tryClosestParameterRayA` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec -> float voption` |
| `XLine2D.tryParameterA` | `lineA: Line2D * lineB: Line2D -> float voption` | `XLine3D.tryClosestParameterRayA` | `lineA: Line3D * lineB: Line3D -> float voption` |
| — | *rays in 3D are mostly skew* | `XLine3D.tryClosestPntRayA` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec -> Pnt voption` |
| — | *rays in 3D are mostly skew* | `XLine3D.tryClosestPntRayA` | `lineA: Line3D * lineB: Line3D -> Pnt voption` |
| `XLine2D.tryIntersect` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> Pt voption` | `XLine3D.tryIntersect` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` |
| `XLine2D.tryIntersect` | `lineA: Line2D * lineB: Line2D -> Pt voption` | `XLine3D.tryIntersect` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` |
| `XLine2D.tryIntersectInRangeA` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * minParameterA: float * maxParameterA: float -> Pt voption` | — | *rays in 3D are mostly skew* |
| `XLine2D.tryIntersectInRangeA` | `lineA: Line2D * lineB: Line2D * minParameterA: float * maxParameterA: float -> Pt voption` | — | *rays in 3D are mostly skew* |
| `XLine2D.tryIntersectInRanges` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> Pt voption` | — | *rays in 3D are mostly skew* |
| `XLine2D.tryIntersectInRanges` | `lineA: Line2D * lineB: Line2D * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> Pt voption` | — | *rays in 3D are mostly skew* |
| `XLine2D.tryIntersectRay` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> -> Pt voption` | `XLine3D.tryIntersectRay` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` |
| `XLine2D.tryIntersectRay` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> -> Pt voption` | `XLine3D.tryIntersectRay` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` |

## XLine3D.XCone

| XCone | Signature |
|:--|:--|
| `NoIntersection` | `XCone` |
| `Intersecting` | `float * float -> XCone` |
| `IntersectingOne` | `float -> XCone` |
| `Touching` | `float -> XCone` |
| `ThroughTip` | `float -> XCone` |
| `LineOnCone` | `tipParam: float * onConeTowardsPositiveT: bool -> XCone` |

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

## Rect2D / Rect3D

| Rect2D | Signature | Rect3D | Signature |
|:--|:--|:--|:--|
| `.OriginX` | `float` | `.OriginX` | `float` |
| `.OriginY` | `float` | `.OriginY` | `float` |
| `.XaxisX` | `float` | `.XaxisX` | `float` |
| `.XaxisY` | `float` | `.XaxisY` | `float` |
| `.YaxisX` | `float` | `.YaxisX` | `float` |
| `.YaxisY` | `float` | `.YaxisY` | `float` |
| — | *Z axis* | `.OriginZ` | `float` |
| — | *Z axis* | `.XaxisZ` | `float` |
| — | *Z axis* | `.YaxisZ` | `float` |
| `.Area` | `float` | `.Area` | `float` |
| `Rect2D.area` | `r: Rect2D -> float` | `Rect3D.area` | `r: Rect3D -> float` |
| `.AsFSharpCode` | `string` | `.AsFSharpCode` | `string` |
| `Rect2D.asFSharpCode` | `r: Rect2D -> string` | `Rect3D.asFSharpCode` | `r: Rect3D -> string` |
| `.AsString` | `string` | `.AsString` | `string` |
| `Rect2D.asString` | `r: Rect2D -> string` | `Rect3D.asString` | `r: Rect3D -> string` |
| `.Center` | `Pt` | `.Center` | `Pnt` |
| `Rect2D.center` | `r: Rect2D -> Pt` | `Rect3D.center` | `r: Rect3D -> Pnt` |
|  |  | `.ClosestParameters` | `p: Pnt -> float * float` |
|  |  | `Rect3D.closestParameters` | `p: Pnt -> r: Rect3D -> float * float` |
|  |  | `.ClosestPoint` | `p: Pnt -> Pnt` |
|  |  | `Rect3D.closestPoint` | `p: Pnt -> r: Rect3D -> Pnt` |
| `.Contains` | `p: Pt -> bool` | — | *inside of a planar area* |
| `Rect2D.contains` | `p: Pt -> r: Rect2D -> bool` | — | *inside of a planar area* |
| `.ContainsXY` | `x: float * y: float -> bool` | — | *inside of a planar area* |
| `Rect2D.containsXY` | `x: float -> y: float -> r: Rect2D -> bool` | — | *inside of a planar area* |
| `.CountZeroSides` | `int` | `.CountZeroSides` | `int` |
| `Rect2D.countZeroSides` | `r: Rect2D -> int` | `Rect3D.countZeroSides` | `r: Rect3D -> int` |
| `.Diagonal` | `Vc` | `.Diagonal` | `Vec` |
| `Rect2D.diagonal` | `r: Rect2D -> Vc` | `Rect3D.diagonal` | `r: Rect3D -> Vec` |
| `.DiagonalLine` | `Line2D` | `.DiagonalLine` | `Line3D` |
| `Rect2D.diagonalLine` | `r: Rect2D -> Line2D` | `Rect3D.diagonalLine` | `r: Rect3D -> Line3D` |
| `.Edge01` | `Line2D` | `.Edge01` | `Line3D` |
| `Rect2D.edge01` | `r: Rect2D -> Line2D` | `Rect3D.edge01` | `r: Rect3D -> Line3D` |
| `.Edge03` | `Line2D` | `.Edge03` | `Line3D` |
| `Rect2D.edge03` | `r: Rect2D -> Line2D` | `Rect3D.edge03` | `r: Rect3D -> Line3D` |
| `.Edge10` | `Line2D` | `.Edge10` | `Line3D` |
| `Rect2D.edge10` | `r: Rect2D -> Line2D` | `Rect3D.edge10` | `r: Rect3D -> Line3D` |
| `.Edge12` | `Line2D` | `.Edge12` | `Line3D` |
| `Rect2D.edge12` | `r: Rect2D -> Line2D` | `Rect3D.edge12` | `r: Rect3D -> Line3D` |
| `.Edge21` | `Line2D` | `.Edge21` | `Line3D` |
| `Rect2D.edge21` | `r: Rect2D -> Line2D` | `Rect3D.edge21` | `r: Rect3D -> Line3D` |
| `.Edge23` | `Line2D` | `.Edge23` | `Line3D` |
| `Rect2D.edge23` | `r: Rect2D -> Line2D` | `Rect3D.edge23` | `r: Rect3D -> Line3D` |
| `.Edge30` | `Line2D` | `.Edge30` | `Line3D` |
| `Rect2D.edge30` | `r: Rect2D -> Line2D` | `Rect3D.edge30` | `r: Rect3D -> Line3D` |
| `.Edge32` | `Line2D` | `.Edge32` | `Line3D` |
| `Rect2D.edge32` | `r: Rect2D -> Line2D` | `Rect3D.edge32` | `r: Rect3D -> Line3D` |
| `.Edges` | `Line2D array` | `.Edges` | `Line3D array` |
| `Rect2D.edges` | `r: Rect2D -> Line2D array` | `Rect3D.edges` | `r: Rect3D -> Line3D array` |
| `.EdgeX` | `Line2D` | `.EdgeX` | `Line3D` |
| `Rect2D.edgeX` | `r: Rect2D -> Line2D` | `Rect3D.edgeX` | `r: Rect3D -> Line3D` |
| `.EdgeY` | `Line2D` | `.EdgeY` | `Line3D` |
| `Rect2D.edgeY` | `r: Rect2D -> Line2D` | `Rect3D.edgeY` | `r: Rect3D -> Line3D` |
| `.EvaluateAt` | `xParameter: float * yParameter: float -> Pt` | `.EvaluateAt` | `xParameter: float * yParameter: float -> Pnt` |
| `Rect2D.evaluateAt` | `xParameter: float -> yParameter: float -> r: Rect2D -> Pt` | `Rect3D.evaluateAt` | `xParameter: float -> yParameter: float -> r: Rect3D -> Pnt` |
| `.EvaluateDist` | `xDistance: float * yDistance: float -> Pt` | `.EvaluateDist` | `xDistance: float * yDistance: float -> Pnt` |
| `Rect2D.evaluateDist` | `xDistance: float -> yDistance: float -> r: Rect2D -> Pt` | `Rect3D.evaluateDist` | `xDistance: float -> yDistance: float -> r: Rect3D -> Pnt` |
| `.FarCorner` | `Pt` | `.FarCorner` | `Pnt` |
| `Rect2D.farCorner` | `r: Rect2D -> Pt` | `Rect3D.farCorner` | `r: Rect3D -> Pnt` |
| — | *plane or normal* | `.Flipped` | `Rect3D` |
| — | *plane or normal* | `Rect3D.flipped` | `r: Rect3D -> Rect3D` |
| `.GetEdge` | `i: int -> Line2D` | `.GetEdge` | `i: int -> Line3D` |
| `Rect2D.getEdge` | `i: int -> r: Rect2D -> Line2D` | `Rect3D.getEdge` | `i: int -> r: Rect3D -> Line3D` |
| `.HasArea` | `bool` | `.HasArea` | `bool` |
| `Rect2D.hasArea` | `r: Rect2D -> bool` | `Rect3D.hasArea` | `r: Rect3D -> bool` |
| `.IsLine` | `bool` | `.IsLine` | `bool` |
| `Rect2D.isLine` | `r: Rect2D -> bool` | `Rect3D.isLine` | `r: Rect3D -> bool` |
| `.IsPoint` | `bool` | `.IsPoint` | `bool` |
| `Rect2D.isPoint` | `r: Rect2D -> bool` | `Rect3D.isPoint` | `r: Rect3D -> bool` |
| `.IsValid` | `bool` | `.IsValid` | `bool` |
| `Rect2D.isValid` | `r: Rect2D -> bool` | `Rect3D.isValid` | `r: Rect3D -> bool` |
| `.IsZero` | `bool` | `.IsZero` | `bool` |
| `Rect2D.isZero` | `r: Rect2D -> bool` | `Rect3D.isZero` | `r: Rect3D -> bool` |
| `.LongestEdge` | `float` | `.LongestEdge` | `float` |
| `Rect2D.longestEdge` | `r: Rect2D -> float` | `Rect3D.longestEdge` | `r: Rect3D -> float` |
| `.LongestEdgeSq` | `float` | `.LongestEdgeSq` | `float` |
| `Rect2D.longestEdgeSq` | `r: Rect2D -> float` | `Rect3D.longestEdgeSq` | `r: Rect3D -> float` |
| `.Move` | `v: Vc -> Rect2D` | `.Move` | `v: Vec -> Rect3D` |
| `Rect2D.move` | `v: Vc -> r: Rect2D -> Rect2D` | `Rect3D.move` | `v: Vec -> r: Rect3D -> Rect3D` |
| `.MoveX` | `distance: float -> Rect2D` | `.MoveX` | `distance: float -> Rect3D` |
| `Rect2D.moveX` | `distance: float -> r: Rect2D -> Rect2D` | `Rect3D.moveX` | `distance: float -> r: Rect3D -> Rect3D` |
| `.MoveY` | `distance: float -> Rect2D` | `.MoveY` | `distance: float -> Rect3D` |
| `Rect2D.moveY` | `distance: float -> r: Rect2D -> Rect2D` | `Rect3D.moveY` | `distance: float -> r: Rect3D -> Rect3D` |
| — | *Z axis* | `.MoveZ` | `distance: float -> Rect3D` |
| — | *Z axis* | `Rect3D.moveZ` | `distance: float -> r: Rect3D -> Rect3D` |
| — | *plane or normal* | `.Normal` | `Vec` |
| — | *plane or normal* | `Rect3D.normal` | `r: Rect3D -> Vec` |
| — | *plane or normal* | `.NormalUnit` | `UnitVec` |
| — | *plane or normal* | `Rect3D.normalUnit` | `r: Rect3D -> UnitVec` |
| `.Origin` | `Pt` | `.Origin` | `Pnt` |
| `Rect2D.origin` | `r: Rect2D -> Pt` | `Rect3D.origin` | `r: Rect3D -> Pnt` |
| — | *plane or normal* | `.Plane` | `PPlane` |
| — | *plane or normal* | `Rect3D.plane` | `r: Rect3D -> PPlane` |
| `.Points` | `Pt array` | `.Points` | `Pnt array` |
| `Rect2D.points` | `r: Rect2D -> Pt array` | `Rect3D.points` | `r: Rect3D -> Pnt array` |
| `.PointsLooped` | `Pt array` | `.PointsLooped` | `Pnt array` |
| `Rect2D.pointsLooped` | `r: Rect2D -> Pt array` | `Rect3D.pointsLooped` | `r: Rect3D -> Pnt array` |
| `.PointsXY_CCW` | `ResizeArray<float>` | `.PointsXYZ` | `ResizeArray<float>` |
| `Rect2D.pointsXY_CCW` | `r: Rect2D -> ResizeArray<float>` | `Rect3D.pointsXYZ` | `r: Rect3D -> ResizeArray<float>` |
| `.PointsXYLooped_CCW` | `ResizeArray<float>` | `.PointsXYZLooped` | `ResizeArray<float>` |
| `Rect2D.pointsXYLooped_CCW` | `r: Rect2D -> ResizeArray<float>` | `Rect3D.pointsXYZLooped` | `r: Rect3D -> ResizeArray<float>` |
| `.PointsXYLooped_CW` | `ResizeArray<float>` | — | *orientation in the plane* |
| `Rect2D.pointsXYLooped_CW` | `r: Rect2D -> ResizeArray<float>` | — | *orientation in the plane* |
| `.PointsXY_CW` | `ResizeArray<float>` | — | *orientation in the plane* |
| `Rect2D.pointsXY_CW` | `r: Rect2D -> ResizeArray<float>` | — | *orientation in the plane* |
| `.Pt0` | `Pt` | `.Pt0` | `Pnt` |
| `Rect2D.pt0` | `r: Rect2D -> Pt` | `Rect3D.pt0` | `r: Rect3D -> Pnt` |
| `.Pt1` | `Pt` | `.Pt1` | `Pnt` |
| `Rect2D.pt1` | `r: Rect2D -> Pt` | `Rect3D.pt1` | `r: Rect3D -> Pnt` |
| `.Pt2` | `Pt` | `.Pt2` | `Pnt` |
| `Rect2D.pt2` | `r: Rect2D -> Pt` | `Rect3D.pt2` | `r: Rect3D -> Pnt` |
| `.Pt3` | `Pt` | `.Pt3` | `Pnt` |
| `Rect2D.pt3` | `r: Rect2D -> Pt` | `Rect3D.pt3` | `r: Rect3D -> Pnt` |
| `.Rotate` | `rot: Rotation2D -> Rect2D` | `.Rotate` | `q: Quaternion -> Rect3D` |
| `Rect2D.rotate` | `rot: Rotation2D -> rect: Rect2D -> Rect2D` | `Rect3D.rotate` | `q: Quaternion -> r: Rect3D -> Rect3D` |
| `.RotateOrientation180` | `Rect2D` | `.RotateOrientation180` | `Rect3D` |
| `Rect2D.rotateOrientation180` | `r: Rect2D -> Rect2D` | `Rect3D.rotateOrientation180` | `r: Rect3D -> Rect3D` |
| `.RotateOrientation90CCW` | `Rect2D` | `.RotateOrientation90CCW` | `Rect3D` |
| `Rect2D.rotateOrientation90CCW` | `r: Rect2D -> Rect2D` | `Rect3D.rotateOrientation90CCW` | `r: Rect3D -> Rect3D` |
| `.RotateOrientation90CW` | `Rect2D` | `.RotateOrientation90CW` | `Rect3D` |
| `Rect2D.rotateOrientation90CW` | `r: Rect2D -> Rect2D` | `Rect3D.rotateOrientation90CW` | `r: Rect3D -> Rect3D` |
| `.RotateWithCenter` | `cen: Pt * rot: Rotation2D -> Rect2D` | `.RotateWithCenter` | `cen: Pnt * q: Quaternion -> Rect3D` |
| `Rect2D.rotateWithCenter` | `cen: Pt -> rot: Rotation2D -> rect: Rect2D -> Rect2D` | `Rect3D.rotateWithCenter` | `cen: Pnt -> q: Quaternion -> r: Rect3D -> Rect3D` |
| `.Scale` | `factor: float -> Rect2D` | `.Scale` | `factor: float -> Rect3D` |
| `Rect2D.scale` | `factor: float -> r: Rect2D -> Rect2D` | `Rect3D.scale` | `factor: float -> r: Rect3D -> Rect3D` |
| `.ScaleOn` | `cen: Pt -> factor: float -> Rect2D` | `.ScaleOn` | `cen: Pnt -> factor: float -> Rect3D` |
| `Rect2D.scaleOn` | `cen: Pt -> factor: float -> r: Rect2D -> Rect2D` | `Rect3D.scaleOn` | `cen: Pnt -> factor: float -> r: Rect3D -> Rect3D` |
| `.ShortestEdge` | `float` | `.ShortestEdge` | `float` |
| `Rect2D.shortestEdge` | `r: Rect2D -> float` | `Rect3D.shortestEdge` | `r: Rect3D -> float` |
| `.ShortestEdgeSq` | `float` | `.ShortestEdgeSq` | `float` |
| `Rect2D.shortestEdgeSq` | `r: Rect2D -> float` | `Rect3D.shortestEdgeSq` | `r: Rect3D -> float` |
| `.SizeX` | `float` | `.SizeX` | `float` |
| `Rect2D.sizeX` | `r: Rect2D -> float` | `Rect3D.sizeX` | `r: Rect3D -> float` |
| `.SizeXSq` | `float` | `.SizeXSq` | `float` |
| `Rect2D.sizeXSq` | `r: Rect2D -> float` | `Rect3D.sizeXSq` | `r: Rect3D -> float` |
| `.SizeY` | `float` | `.SizeY` | `float` |
| `Rect2D.sizeY` | `r: Rect2D -> float` | `Rect3D.sizeY` | `r: Rect3D -> float` |
| `.SizeYSq` | `float` | `.SizeYSq` | `float` |
| `Rect2D.sizeYSq` | `r: Rect2D -> float` | `Rect3D.sizeYSq` | `r: Rect3D -> float` |
| `.ToString` | `unit -> string` | `.ToString` | `unit -> string` |
| — | *transformation matrix* | `.TransformRigid` | `m: RigidMatrix -> Rect3D` |
| — | *transformation matrix* | `Rect3D.transformRigid` | `m: RigidMatrix -> r: Rect3D -> Rect3D` |
| `.Xaxis` | `Vc` | `.Xaxis` | `Vec` |
| `Rect2D.xAxis` | `r: Rect2D -> Vc` | `Rect3D.xAxis` | `r: Rect3D -> Vec` |
| `.XaxisUnit` | `UnitVc` | `.XaxisUnit` | `UnitVec` |
| `Rect2D.xAxisUnit` | `r: Rect2D -> UnitVc` | `Rect3D.xAxisUnit` | `r: Rect3D -> UnitVec` |
| `.XCorner` | `Pt` | `.XCorner` | `Pnt` |
| `Rect2D.xCorner` | `r: Rect2D -> Pt` | `Rect3D.xCorner` | `r: Rect3D -> Pnt` |
| `.Yaxis` | `Vc` | `.Yaxis` | `Vec` |
| `Rect2D.yAxis` | `r: Rect2D -> Vc` | `Rect3D.yAxis` | `r: Rect3D -> Vec` |
| `.YaxisUnit` | `UnitVc` | `.YaxisUnit` | `UnitVec` |
| `Rect2D.yAxisUnit` | `r: Rect2D -> UnitVc` | `Rect3D.yAxisUnit` | `r: Rect3D -> UnitVec` |
| `.YCorner` | `Pt` | `.YCorner` | `Pnt` |
| `Rect2D.yCorner` | `r: Rect2D -> Pt` | `Rect3D.yCorner` | `r: Rect3D -> Pnt` |
| **Other static members** | | | |
| `Rect2D.clipLine` | `line: Line2D -> rect: Rect2D -> Line2D voption` | — | *clipping in a fixed plane* |
| — | *plane or normal* | `Rect3D.createCenteredFromPlane` | `pl: PPlane * sizeX: float * sizeY: float -> Rect3D` |
| `Rect2D.createFrom3Points` | `origin: Pt * xPt: Pt * yPt: Pt -> Rect2D` | `Rect3D.createFrom3Points` | `origin: Pnt * xPt: Pnt * yPt: Pnt -> Rect3D` |
| `Rect2D.createFromBounds` | `minX: float * minY: float * maxX: float * maxY: float -> Rect2D` | `Rect3D.createFromBounds2D` | `minX: float * minY: float * maxX: float * maxY: float -> Rect3D` |
| `Rect2D.createFromCenterAndDirection` | `center: Pt * directionX: UnitVc * sizeX: float * sizeY: float -> Rect2D` |  |  |
| `Rect2D.createFromCenterAndVector` | `center: Pt * x: Vc * sizeY: float -> Rect2D` |  |  |
| `Rect2D.createFromDirAndPoints` | `dirX: Vc -> pts: IList<Pt> -> Rect2D` |  |  |
| `Rect2D.createFromDirectionAndSizes` | `origin: Pt * directionX: UnitVc * sizeX: float * sizeY: float -> Rect2D` |  |  |
| `Rect2D.createFromLine` | `line: Line2D * offRight: float * offLeft: float -> Rect2D` | — | *a 3D rectangle also needs a normal* |
| — | *plane or normal* | `Rect3D.createFromPlane` | `pl: PPlane * sizeX: float * sizeY: float -> Rect3D` |
| `Rect2D.createFromVectors` | `origin: Pt * x: Vc * y: Vc -> Rect2D` | `Rect3D.createFromVectors` | `origin: Pnt * x: Vec * y: Vec -> Rect3D` |
| `Rect2D.createFromXVectorAndWidth` | `origin: Pt * x: Vc * sizeY: float -> Rect2D` |  |  |
| `Rect2D.createUnchecked` | `originX: float * originY: float * xaxisX: float * xaxisY: float * yaxisX: float * yaxisY: float -> Rect2D` | `Rect3D.createUnchecked` | `originX: float * originY: float * originZ: float * xAxisX: float * xAxisY: float * xAxisZ: float * yAxisX: float * yAxisY: float * yAxisZ: float -> Rect3D` |
| `Rect2D.createUncheckedVec` | `origin: Pt * x: Vc * y: Vc -> Rect2D` | `Rect3D.createUncheckedVec` | `origin: Pnt * x: Vec * y: Vec -> Rect3D` |
| `Rect2D.equals` | `tol: float -> a: Rect2D -> b: Rect2D -> bool` | `Rect3D.equals` | `tol: float -> a: Rect3D -> b: Rect3D -> bool` |
| `Rect2D.expand` | `dist: float -> r: Rect2D -> Rect2D` | `Rect3D.expand` | `dist: float -> r: Rect3D -> Rect3D` |
| `Rect2D.expandRel` | `factor: float -> r: Rect2D -> Rect2D` | `Rect3D.expandRel` | `factor: float -> r: Rect3D -> Rect3D` |
| `Rect2D.expandRelXY` | `factorX: float -> factorY: float -> r: Rect2D -> Rect2D` | `Rect3D.expandRelXY` | `factorX: float -> factorY: float -> r: Rect3D -> Rect3D` |
| `Rect2D.expandXY` | `distX: float -> distY: float -> r: Rect2D -> Rect2D` | `Rect3D.expandXY` | `distX: float -> distY: float -> r: Rect3D -> Rect3D` |
| `Rect2D.fitToPoints` | `pts: IList<Pt> -> refRect: Rect2D -> Rect2D` | `Rect3D.fitToPoints` | `pts: IList<Pnt> -> refRect: Rect3D -> Rect3D` |
| `Rect2D.fitToPointsXY` | `xys: IList<float> -> refRect: Rect2D -> Rect2D` |  |  |
| — | *plane or normal* | `Rect3D.flip` | `r: Rect3D -> Rect3D` |
| `Rect2D.grid` | `rect: Rect2D * xCount: int * yCount: int -> Pt array array` | `Rect3D.grid` | `rect: Rect3D * xCount: int * yCount: int -> Pnt array array` |
| `Rect2D.gridMaxLength` | `rect: Rect2D * xMaxLen: float * yMaxLen: float -> Pt array array` | `Rect3D.gridMaxLength` | `rect: Rect3D * xMaxLen: float * yMaxLen: float -> Pnt array array` |
| `Rect2D.gridMinLength` | `rect: Rect2D * xMinLen: float * yMinLen: float -> Pt array array` | `Rect3D.gridMinLength` | `rect: Rect3D * xMinLen: float * yMinLen: float -> Pnt array array` |
| `Rect2D.intersectLine` | `line: Line2D -> rect: Rect2D -> Line2D voption` | `Rect3D.intersectLine` | `ln: Line3D -> r: Rect3D -> Pnt voption` |
| `Rect2D.intersectRay` | `ray: Line2D -> rect: Rect2D -> (float * float) voption` | `Rect3D.intersectRay` | `ln: Line3D -> r: Rect3D -> float voption` |
| `Rect2D.iterPointsCCW` | `action: (float -> float -> unit) -> r: Rect2D -> unit` | `Rect3D.iterPointsCCW` | `action: (float -> float -> float -> unit) -> r: Rect3D -> unit` |
| `Rect2D.iterPointsCW` | `action: (float -> float -> unit) -> r: Rect2D -> unit` | `Rect3D.iterPointsCW` | `action: (float -> float -> float -> unit) -> r: Rect3D -> unit` |
| `Rect2D.iterPointsLoopedCCW` | `action: (float -> float -> unit) -> r: Rect2D -> unit` | `Rect3D.iterPointsLoopedCCW` | `action: (float -> float -> float -> unit) -> r: Rect3D -> unit` |
| `Rect2D.iterPointsLoopedCW` | `action: (float -> float -> unit) -> r: Rect2D -> unit` | `Rect3D.iterPointsLoopedCW` | `action: (float -> float -> float -> unit) -> r: Rect3D -> unit` |
| `Rect2D.notEquals` | `tol: float -> a: Rect2D -> b: Rect2D -> bool` | `Rect3D.notEquals` | `tol: float -> a: Rect3D -> b: Rect3D -> bool` |
| `Rect2D.offset` | `dist: float -> rect: Rect2D -> Rect2D` | `Rect3D.offset` | `dist: float -> rect: Rect3D -> Rect3D` |
| `Rect2D.offsetCorner` | `rect: Rect2D * corner: int * xOffset: float * yOffset: float * xWidth: float * yHeight: float -> Rect2D` | `Rect3D.offsetCorner` | `rect: Rect3D * corner: int * xOffset: float * yOffset: float * xSize: float * ySize: float -> Rect3D` |
| `Rect2D.offsetEdge` | `rect: Rect2D * edgeIdx: int * offEdge: float * width: float * offStart: float * offEnd: float -> Rect2D` | `Rect3D.offsetEdge` | `rect: Rect3D * edgeIdx: int * offEdge: float * width: float * offStart: float * offEnd: float -> Rect3D` |
| `Rect2D.offsetVar` | `dist: float array -> rect: Rect2D -> Rect2D` | `Rect3D.offsetVar` | `dist: float array -> rect: Rect3D -> Rect3D` |
| — | *Z axis* | `Rect3D.offsetZ` | `offsetDistance: float -> r: Rect3D -> Rect3D` |
| `Rect2D.subDivide` | `rect: Rect2D * xCount: int * yCount: int * xGap: float * yGap: float -> Rect2D array array` | `Rect3D.subDivide` | `rect: Rect3D * xCount: int * yCount: int * xGap: float * yGap: float -> Rect3D array array` |
| `Rect2D.subDivideMaxLength` | `rect: Rect2D * xMaxLen: float * yMaxLen: float * xGap: float * yGap: float -> Rect2D array array` | `Rect3D.subDivideMaxLength` | `rect: Rect3D * xMaxLen: float * yMaxLen: float * xGap: float * yGap: float -> Rect3D array array` |
| `Rect2D.subDivideMinLength` | `rect: Rect2D * xMinLen: float * yMinLen: float * xGap: float * yGap: float -> Rect2D array array` | `Rect3D.subDivideMinLength` | `rect: Rect3D * xMinLen: float * yMinLen: float * xGap: float * yGap: float -> Rect3D array array` |
| `Rect2D.translateLocalX` | `distX: float -> r: Rect2D -> Rect2D` | `Rect3D.translateLocalX` | `distX: float -> r: Rect3D -> Rect3D` |
| `Rect2D.translateLocalY` | `distY: float -> r: Rect2D -> Rect2D` | `Rect3D.translateLocalY` | `distY: float -> r: Rect3D -> Rect3D` |
| — | *Z axis* | `Rect3D.translateLocalZ` | `distZ: float -> r: Rect3D -> Rect3D` |
| `Rect2D.tryCreateFrom3Points` | `origin: Pt * xPt: Pt * yPt: Pt -> Rect2D option` | `Rect3D.tryCreateFrom3Points` | `origin: Pnt * xPt: Pnt * yPt: Pnt -> Rect3D option` |

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
| `.Pt1` | `Pnt` |
| `Box.pt1` | `b: Box -> Pnt` |
| `.Pt2` | `Pnt` |
| `Box.pt2` | `b: Box -> Pnt` |
| `.Pt3` | `Pnt` |
| `Box.pt3` | `b: Box -> Pnt` |
| `.Pt4` | `Pnt` |
| `Box.pt4` | `b: Box -> Pnt` |
| `.Pt5` | `Pnt` |
| `Box.pt5` | `b: Box -> Pnt` |
| `.Pt6` | `Pnt` |
| `Box.pt6` | `b: Box -> Pnt` |
| `.Pt7` | `Pnt` |
| `Box.pt7` | `b: Box -> Pnt` |
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
| `.AsString` | `string` |
| `FreeBox.asString` | `b: FreeBox -> string` |
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
| `.GetPt` | `i: int -> Pnt` |
| `FreeBox.getPt` | `i: int -> b: FreeBox -> Pnt` |
| `.Move` | `v: Vec -> FreeBox` |
| `FreeBox.move` | `v: Vec -> b: FreeBox -> FreeBox` |
| `.MoveX` | `distance: float -> FreeBox` |
| `FreeBox.moveX` | `distance: float -> b: FreeBox -> FreeBox` |
| `.MoveY` | `distance: float -> FreeBox` |
| `FreeBox.moveY` | `distance: float -> b: FreeBox -> FreeBox` |
| `.MoveZ` | `distance: float -> FreeBox` |
| `FreeBox.moveZ` | `distance: float -> b: FreeBox -> FreeBox` |
| `.Points` | `Pnt array` |
| `FreeBox.points` | `b: FreeBox -> Pnt array` |
| `.Pt0` | `Pnt with get, set` |
| `FreeBox.pt0` | `b: FreeBox -> Pnt` |
| `.Pt1` | `Pnt with get, set` |
| `FreeBox.pt1` | `b: FreeBox -> Pnt` |
| `.Pt2` | `Pnt with get, set` |
| `FreeBox.pt2` | `b: FreeBox -> Pnt` |
| `.Pt3` | `Pnt with get, set` |
| `FreeBox.pt3` | `b: FreeBox -> Pnt` |
| `.Pt4` | `Pnt with get, set` |
| `FreeBox.pt4` | `b: FreeBox -> Pnt` |
| `.Pt5` | `Pnt with get, set` |
| `FreeBox.pt5` | `b: FreeBox -> Pnt` |
| `.Pt6` | `Pnt with get, set` |
| `FreeBox.pt6` | `b: FreeBox -> Pnt` |
| `.Pt7` | `Pnt with get, set` |
| `FreeBox.pt7` | `b: FreeBox -> Pnt` |
| `.Rotate` | `q: Quaternion -> FreeBox` |
| `FreeBox.rotate` | `q: Quaternion -> b: FreeBox -> FreeBox` |
| `.RotateWithCenter` | `cen: Pnt * q: Quaternion -> FreeBox` |
| `FreeBox.rotateWithCenter` | `cen: Pnt -> q: Quaternion -> b: FreeBox -> FreeBox` |
| `.Scale` | `factor: float -> FreeBox` |
| `FreeBox.scale` | `factor: float -> b: FreeBox -> FreeBox` |
| `.ScaleOn` | `cen: Pnt * factor: float -> FreeBox` |
| `FreeBox.scaleOn` | `cen: Pnt -> factor: float -> b: FreeBox -> FreeBox` |
| `.SetPt` | `i: int -> p: Pnt -> unit` |
| `FreeBox.setPt` | `i: int -> p: Pnt -> b: FreeBox -> unit` |
| `.ToString` | `unit -> string` |
| `.Transform` | `m: Matrix -> FreeBox` |
| `FreeBox.transform` | `m: Matrix -> b: FreeBox -> FreeBox` |
| `.TransformRigid` | `m: RigidMatrix -> FreeBox` |
| `FreeBox.transformRigid` | `m: RigidMatrix -> b: FreeBox -> FreeBox` |
| **Other static members** | |
| `FreeBox.createFromBox` | `box: Box -> FreeBox` |
| `FreeBox.createFromEightPoints` | `pts: Pnt array -> FreeBox` |
| `FreeBox.createFromFour2DPoints` | `zMin: float -> zMax: float -> pts: Pt array -> FreeBox` |
| `FreeBox.createFromFour2DPointsArgs` | `a: Pt * b: Pt * c: Pt * d: Pt * zMin: float * zMax: float -> FreeBox` |
| `FreeBox.translate` | `v: Vec -> b: FreeBox -> FreeBox` |

## BRect / BBox

| BRect | Signature | BBox | Signature |
|:--|:--|:--|:--|
| `.MinX` | `float` | `.MinX` | `float` |
| `.MinY` | `float` | `.MinY` | `float` |
| `.MaxX` | `float` | `.MaxX` | `float` |
| `.MaxY` | `float` | `.MaxY` | `float` |
| — | *Z axis* | `.MinZ` | `float` |
| — | *Z axis* | `.MaxZ` | `float` |
| `.AsRect2D` | `Rect2D` | `.AsBox` | `Box` |
|  |  | `BBox.asBox` | `b: BBox -> Box` |
| — | *conversion between 2D and 3D* | `.AsBRect` | `BRect` |
| — | *conversion between 2D and 3D* | `BBox.asBRect` | `b: BBox -> BRect` |
| `.AsFSharpCode` | `string` | `.AsFSharpCode` | `string` |
| `BRect.asFSharpCode` | `r: BRect -> string` | `BBox.asFSharpCode` | `b: BBox -> string` |
| `.AsString` | `string` | `.AsString` | `string` |
| `BRect.asString` | `r: BRect -> string` | `BBox.asString` | `b: BBox -> string` |
| — | *corners, edges and faces of a box* | `.BackFace` | `Rect3D` |
| — | *corners, edges and faces of a box* | `BBox.backFace` | `b: BBox -> Rect3D` |
| — | *corners, edges and faces of a box* | `.BottomFace` | `Rect3D` |
| — | *corners, edges and faces of a box* | `BBox.bottomFace` | `b: BBox -> Rect3D` |
| — | *corners, edges and faces of a box* | `.BottomPoints` | `Pnt array` |
| — | *corners, edges and faces of a box* | `BBox.bottomPoints` | `b: BBox -> Pnt array` |
| — | *corners, edges and faces of a box* | `.BottomPointsLooped` | `Pnt array` |
| — | *corners, edges and faces of a box* | `BBox.bottomPointsLooped` | `b: BBox -> Pnt array` |
| `.Center` | `Pt` | `.Center` | `Pnt` |
| `BRect.center` | `r: BRect -> Pt` | `BBox.center` | `b: BBox -> Pnt` |
| `.Contains` | `o: BRect -> bool` | `.Contains` | `o: BBox -> bool` |
| `BRect.contains` | `rectInside: BRect -> surroundingRect: BRect -> bool` | `BBox.contains` | `boxInside: BBox -> surroundingBox: BBox -> bool` |
| `.ContainsPt` | `p: Pt -> bool` | `.ContainsPnt` | `p: Pnt -> bool` |
| `BRect.containsPt` | `pt: Pt -> rect: BRect -> bool` | `BBox.containsPnt` | `pt: Pnt -> box: BBox -> bool` |
| `.ContainsXY` | `x: float * y: float -> bool` | `.ContainsXYZ` | `x: float * y: float * z: float -> bool` |
| `BRect.containsXY` | `x: float -> y: float -> rect: BRect -> bool` | `BBox.containsXYZ` | `x: float -> y: float -> z: float -> box: BBox -> bool` |
| `.CountZeroSides` | `int` | `.CountZeroSides` | `int` |
| `BRect.countZeroSides` | `r: BRect -> int` | `BBox.countZeroSides` | `b: BBox -> int` |
| `.Diagonal` | `Vc` | `.Diagonal` | `Vec` |
| `BRect.diagonal` | `r: BRect -> Vc` | `BBox.diagonal` | `b: BBox -> Vec` |
| — | *plane or normal* | `.DoesIntersectPlane` | `pl: NPlane -> bool` |
| — | *plane or normal* | `BBox.doesIntersectPlane` | `pl: NPlane -> b: BBox -> bool` |
| `.Edge01` | `Line2D` | `.Edge01` | `Line3D` |
| `BRect.edge01` | `r: BRect -> Line2D` | `BBox.edge01` | `b: BBox -> Line3D` |
| `.Edge03` | `Line2D` | `.Edge03` | `Line3D` |
| `BRect.edge03` | `r: BRect -> Line2D` | `BBox.edge03` | `b: BBox -> Line3D` |
| — | *corners, edges and faces of a box* | `.Edge04` | `Line3D` |
| — | *corners, edges and faces of a box* | `BBox.edge04` | `b: BBox -> Line3D` |
| `.Edge10` | `Line2D` | `.Edge10` | `Line3D` |
| `BRect.edge10` | `r: BRect -> Line2D` | `BBox.edge10` | `b: BBox -> Line3D` |
| `.Edge12` | `Line2D` | `.Edge12` | `Line3D` |
| `BRect.edge12` | `r: BRect -> Line2D` | `BBox.edge12` | `b: BBox -> Line3D` |
| — | *corners, edges and faces of a box* | `.Edge15` | `Line3D` |
| — | *corners, edges and faces of a box* | `BBox.edge15` | `b: BBox -> Line3D` |
| `.Edge21` | `Line2D` | `.Edge21` | `Line3D` |
| `BRect.edge21` | `r: BRect -> Line2D` | `BBox.edge21` | `b: BBox -> Line3D` |
| `.Edge23` | `Line2D` | `.Edge23` | `Line3D` |
| `BRect.edge23` | `r: BRect -> Line2D` | `BBox.edge23` | `b: BBox -> Line3D` |
| — | *corners, edges and faces of a box* | `.Edge26` | `Line3D` |
| — | *corners, edges and faces of a box* | `BBox.edge26` | `b: BBox -> Line3D` |
| `.Edge30` | `Line2D` | `.Edge30` | `Line3D` |
| `BRect.edge30` | `r: BRect -> Line2D` | `BBox.edge30` | `b: BBox -> Line3D` |
| `.Edge32` | `Line2D` | `.Edge32` | `Line3D` |
| `BRect.edge32` | `r: BRect -> Line2D` | `BBox.edge32` | `b: BBox -> Line3D` |
| — | *corners, edges and faces of a box* | `.Edge37` | `Line3D` |
| — | *corners, edges and faces of a box* | `BBox.edge37` | `b: BBox -> Line3D` |
| — | *corners, edges and faces of a box* | `.Edge40` | `Line3D` |
| — | *corners, edges and faces of a box* | `BBox.edge40` | `b: BBox -> Line3D` |
| — | *corners, edges and faces of a box* | `.Edge45` | `Line3D` |
| — | *corners, edges and faces of a box* | `BBox.edge45` | `b: BBox -> Line3D` |
| — | *corners, edges and faces of a box* | `.Edge47` | `Line3D` |
| — | *corners, edges and faces of a box* | `BBox.edge47` | `b: BBox -> Line3D` |
| — | *corners, edges and faces of a box* | `.Edge51` | `Line3D` |
| — | *corners, edges and faces of a box* | `BBox.edge51` | `b: BBox -> Line3D` |
| — | *corners, edges and faces of a box* | `.Edge54` | `Line3D` |
| — | *corners, edges and faces of a box* | `BBox.edge54` | `b: BBox -> Line3D` |
| — | *corners, edges and faces of a box* | `.Edge56` | `Line3D` |
| — | *corners, edges and faces of a box* | `BBox.edge56` | `b: BBox -> Line3D` |
| — | *corners, edges and faces of a box* | `.Edge62` | `Line3D` |
| — | *corners, edges and faces of a box* | `BBox.edge62` | `b: BBox -> Line3D` |
| — | *corners, edges and faces of a box* | `.Edge65` | `Line3D` |
| — | *corners, edges and faces of a box* | `BBox.edge65` | `b: BBox -> Line3D` |
| — | *corners, edges and faces of a box* | `.Edge67` | `Line3D` |
| — | *corners, edges and faces of a box* | `BBox.edge67` | `b: BBox -> Line3D` |
| — | *corners, edges and faces of a box* | `.Edge73` | `Line3D` |
| — | *corners, edges and faces of a box* | `BBox.edge73` | `b: BBox -> Line3D` |
| — | *corners, edges and faces of a box* | `.Edge74` | `Line3D` |
| — | *corners, edges and faces of a box* | `BBox.edge74` | `b: BBox -> Line3D` |
| — | *corners, edges and faces of a box* | `.Edge76` | `Line3D` |
| — | *corners, edges and faces of a box* | `BBox.edge76` | `b: BBox -> Line3D` |
| `.Edges` | `Line2D array` | `.Edges` | `Line3D array` |
| `BRect.edges` | `r: BRect -> Line2D array` | `BBox.edges` | `b: BBox -> Line3D array` |
| `.EvaluateAt` | `xParameter: float * yParameter: float -> Pt` | `.EvaluateAt` | `xParameter: float * yParameter: float * zParameter: float -> Pnt` |
| `BRect.evaluateAt` | `xParameter: float -> yParameter: float -> r: BRect -> Pt` | `BBox.evaluateAt` | `xParameter: float -> yParameter: float -> zParameter: float -> b: BBox -> Pnt` |
| `.Expand` | `dist: float -> BRect` | `.Expand` | `dist: float -> BBox` |
|  |  | `.Expand` | `xDist: float * yDist: float * zDist: float -> BBox` |
| `BRect.expand` | `dist: float -> r: BRect -> BRect` | `BBox.expand` | `dist: float -> b: BBox -> BBox` |
| `.ExpandSafe` | `dist: float -> BRect` | `.ExpandSafe` | `dist: float -> BBox` |
|  |  | `.ExpandSafe` | `xDist: float * yDist: float * zDist: float -> BBox` |
| `BRect.expandSafe` | `dist: float -> r: BRect -> BRect` | `BBox.expandSafe` | `dist: float -> b: BBox -> BBox` |
| `.ExpandSafeXY` | `xDist: float * yDist: float -> BRect` |  |  |
| `BRect.expandSafeXY` | `xDist: float -> yDist: float -> r: BRect -> BRect` | `BBox.expandSafeXYZ` | `xDist: float -> yDist: float -> zDist: float -> b: BBox -> BBox` |
| `.ExpandXaxis` | `startDist: float * endDist: float -> BRect` | `.ExpandXaxis` | `startDist: float * endDist: float -> BBox` |
| `BRect.expandXaxis` | `startDist: float -> endDist: float -> r: BRect -> BRect` | `BBox.expandXaxis` | `startDist: float -> endDist: float -> b: BBox -> BBox` |
| `.ExpandXY` | `xDist: float * yDist: float -> BRect` |  |  |
| `BRect.expandXY` | `xDist: float -> yDist: float -> r: BRect -> BRect` | `BBox.expandXYZ` | `xDist: float -> yDist: float -> zDist: float -> b: BBox -> BBox` |
| `.ExpandYaxis` | `startDist: float * endDist: float -> BRect` | `.ExpandYaxis` | `startDist: float * endDist: float -> BBox` |
| `BRect.expandYaxis` | `startDist: float -> endDist: float -> r: BRect -> BRect` | `BBox.expandYaxis` | `startDist: float -> endDist: float -> b: BBox -> BBox` |
|  |  | `.ExpandZaxis` | `startDist: float * endDist: float -> BBox` |
|  |  | `BBox.expandZaxis` | `startDist: float -> endDist: float -> b: BBox -> BBox` |
| — | *corners, edges and faces of a box* | `.Faces` | `Rect3D array` |
| — | *corners, edges and faces of a box* | `BBox.faces` | `b: BBox -> Rect3D array` |
| — | *corners, edges and faces of a box* | `.FrontFace` | `Rect3D` |
| — | *corners, edges and faces of a box* | `BBox.frontFace` | `b: BBox -> Rect3D` |
|  |  | `.GetEdge` | `edgeIndex: int -> Line3D` |
|  |  | `BBox.getEdge` | `edgeIndex: int -> b: BBox -> Line3D` |
| — | *corners, edges and faces of a box* | `.GetFace` | `faceIndex: int -> Rect3D` |
| — | *corners, edges and faces of a box* | `BBox.getFace` | `faceIndex: int -> b: BBox -> Rect3D` |
|  |  | `.GetPoint` | `pointIndex: int -> Pnt` |
|  |  | `BBox.getPoint` | `pointIndex: int -> b: BBox -> Pnt` |
| `.HasArea` | `bool` | `.HasVolume` | `bool` |
| `BRect.hasArea` | `r: BRect -> bool` | `BBox.hasVolume` | `b: BBox -> bool` |
| `.Intersection` | `a: BRect -> BRect voption` | `.Intersection` | `a: BBox -> BBox voption` |
| `BRect.intersection` | `a: BRect -> b: BRect -> BRect voption` | `BBox.intersection` | `a: BBox -> b: BBox -> BBox voption` |
| — | *plane or normal* | `.IntersectPlane` | `pl: NPlane -> ResizeArray<Pnt>` |
| — | *plane or normal* | `BBox.intersectPlane` | `pl: NPlane -> b: BBox -> ResizeArray<Pnt>` |
| `.IntersectRay` | `ray: Line2D -> (float * float) voption` | `.IntersectRay` | `ray: Line3D -> (float * float) voption` |
| `BRect.intersectRay` | `ray: Line2D -> r: BRect -> (float * float) voption` | `BBox.intersectRay` | `ray: Line3D -> b: BBox -> (float * float) voption` |
| — | *corners, edges and faces of a box* | `.IsFlat` | `bool` |
| — | *corners, edges and faces of a box* | `BBox.isFlat` | `b: BBox -> bool` |
| `.IsLine` | `bool` | `.IsLine` | `bool` |
| `BRect.isLine` | `r: BRect -> bool` | `BBox.isLine` | `b: BBox -> bool` |
| `.IsOverlapping` | `a: BRect -> bool` | `.IsOverlapping` | `a: BBox -> bool` |
| `BRect.isOverlapping` | `a: BRect -> b: BRect -> bool` | `BBox.isOverlapping` | `a: BBox -> b: BBox -> bool` |
| `.IsOverlappingOrClose` | `a: BRect * tolerance: float -> bool` | `.IsOverlappingOrClose` | `a: BBox * tolerance: float -> bool` |
| `BRect.isOverlappingOrClose` | `tolerance: float -> a: BRect -> b: BRect -> bool` | `BBox.isOverlappingOrClose` | `tolerance: float -> a: BBox -> b: BBox -> bool` |
| `.IsPoint` | `bool` | `.IsPoint` | `bool` |
| `BRect.isPoint` | `r: BRect -> bool` | `BBox.isPoint` | `b: BBox -> bool` |
| `.IsTouching` | `other: BRect * ?tolerance: float -> bool` | `.IsTouching` | `other: BBox * ?tolerance: float -> bool` |
| `BRect.isTouching` | `tolerance: float -> other: BRect -> this: BRect -> bool` | `BBox.isTouching` | `tolerance: float -> other: BBox -> thisBox: BBox -> bool` |
| `.IsValid` | `bool` | `.IsValid` | `bool` |
| `BRect.isValid` | `r: BRect -> bool` | `BBox.isValid` | `b: BBox -> bool` |
| `.IsZero` | `bool` | `.IsZero` | `bool` |
| `BRect.isZero` | `r: BRect -> bool` | `BBox.isZero` | `b: BBox -> bool` |
| — | *corners, edges and faces of a box* | `.LeftFace` | `Rect3D` |
| — | *corners, edges and faces of a box* | `BBox.leftFace` | `b: BBox -> Rect3D` |
| `.LongestEdge` | `float` | `.LongestEdge` | `float` |
| `BRect.longestEdge` | `r: BRect -> float` | `BBox.longestEdge` | `b: BBox -> float` |
| `.MaxPt` | `Pt` | `.MaxPnt` | `Pnt` |
| `BRect.maxPt` | `r: BRect -> Pt` | `BBox.maxPnt` | `b: BBox -> Pnt` |
| `.MinPt` | `Pt` | `.MinPnt` | `Pnt` |
| `BRect.minPt` | `r: BRect -> Pt` | `BBox.minPnt` | `b: BBox -> Pnt` |
| `.Points` | `Pt array` | `.Points` | `Pnt array` |
| `BRect.points` | `r: BRect -> Pt array` | `BBox.points` | `b: BBox -> Pnt array` |
| `.PointsLooped` | `Pt array` | — | *a box has no single perimeter* |
| `BRect.pointsLooped` | `r: BRect -> Pt array` | — | *a box has no single perimeter* |
| `.Pt0` | `Pt` | `.Pt0` | `Pnt` |
| `BRect.pt0` | `r: BRect -> Pt` | `BBox.pt0` | `b: BBox -> Pnt` |
| `.Pt1` | `Pt` | `.Pt1` | `Pnt` |
| `BRect.pt1` | `r: BRect -> Pt` | `BBox.pt1` | `b: BBox -> Pnt` |
| `.Pt2` | `Pt` | `.Pt2` | `Pnt` |
| `BRect.pt2` | `r: BRect -> Pt` | `BBox.pt2` | `b: BBox -> Pnt` |
| `.Pt3` | `Pt` | `.Pt3` | `Pnt` |
| `BRect.pt3` | `r: BRect -> Pt` | `BBox.pt3` | `b: BBox -> Pnt` |
| — | *conversion between 2D and 3D* | `.Pt4` | `Pnt` |
| — | *conversion between 2D and 3D* | `BBox.pt4` | `b: BBox -> Pnt` |
| — | *conversion between 2D and 3D* | `.Pt5` | `Pnt` |
| — | *conversion between 2D and 3D* | `BBox.pt5` | `b: BBox -> Pnt` |
| — | *conversion between 2D and 3D* | `.Pt6` | `Pnt` |
| — | *conversion between 2D and 3D* | `BBox.pt6` | `b: BBox -> Pnt` |
| — | *conversion between 2D and 3D* | `.Pt7` | `Pnt` |
| — | *conversion between 2D and 3D* | `BBox.pt7` | `b: BBox -> Pnt` |
| — | *corners, edges and faces of a box* | `.RightFace` | `Rect3D` |
| — | *corners, edges and faces of a box* | `BBox.rightFace` | `b: BBox -> Rect3D` |
| `.ShortestEdge` | `float` | `.ShortestEdge` | `float` |
| `BRect.shortestEdge` | `r: BRect -> float` | `BBox.shortestEdge` | `b: BBox -> float` |
| `.SizeX` | `float` | `.SizeX` | `float` |
| `BRect.sizeX` | `r: BRect -> float` | `BBox.sizeX` | `b: BBox -> float` |
| `.SizeY` | `float` | `.SizeY` | `float` |
| `BRect.sizeY` | `r: BRect -> float` | `BBox.sizeY` | `b: BBox -> float` |
| — | *Z axis* | `.SizeZ` | `float` |
| — | *Z axis* | `BBox.sizeZ` | `b: BBox -> float` |
| — | *corners, edges and faces of a box* | `.TopFace` | `Rect3D` |
| — | *corners, edges and faces of a box* | `BBox.topFace` | `b: BBox -> Rect3D` |
| — | *corners, edges and faces of a box* | `.TopPoints` | `Pnt array` |
| — | *corners, edges and faces of a box* | `BBox.topPoints` | `b: BBox -> Pnt array` |
| — | *corners, edges and faces of a box* | `.TopPointsLooped` | `Pnt array` |
| — | *corners, edges and faces of a box* | `BBox.topPointsLooped` | `b: BBox -> Pnt array` |
| `.ToString` | `unit -> string` | `.ToString` | `unit -> string` |
| `.TouchingSide` | `other: BRect * ?tolerance: float -> int` | `.TouchingSide` | `other: BBox * ?tolerance: float -> int` |
| `BRect.touchingSide` | `other: BRect -> tolerance: float -> this: BRect -> int` | `BBox.touchingSide` | `tolerance: float -> other: BBox -> thisBox: BBox -> int` |
| `.Union` | `b: BRect -> BRect` | `.Union` | `a: BBox -> BBox` |
| `BRect.union` | `a: BRect -> b: BRect -> BRect` | `BBox.union` | `a: BBox -> b: BBox -> BBox` |
| `.UnionPt` | `p: Pt -> BRect` | `.UnionPnt` | `p: Pnt -> BBox` |
| `BRect.unionPt` | `p: Pt -> r: BRect -> BRect` | `BBox.unionPnt` | `p: Pnt -> b: BBox -> BBox` |
| `.Area` | `float` | `.Volume` | `float` |
| `BRect.area` | `r: BRect -> float` | `BBox.volume` | `b: BBox -> float` |
| **Other static members** | | | |
| `BRect.create` | `a: Pt * b: Pt -> BRect` | `BBox.create` | `a: Pnt * b: Pnt -> BBox` |
| — | *conversion from an oriented 3D box* | `BBox.createFromBox` | `b: Box -> BBox` |
| — | *conversion between 2D and 3D* | `BBox.createFromBRect` | `minZ: float -> maxZ: float -> r: BRect -> BBox` |
| `BRect.createFromCenter` | `center: Pt * sizeX: float * sizeY: float -> BRect` | `BBox.createFromCenter` | `center: Pnt * sizeX: float * sizeY: float * sizeZ: float -> BBox` |
| `BRect.createFromIList` | `ps: IList<Pt> -> BRect` | `BBox.createFromIList` | `ps: IList<Pnt> -> BBox` |
| `BRect.createFromLine` | `l: Line2D -> BRect` | `BBox.createFromLine` | `l: Line3D -> BBox` |
| `BRect.createFromRect3D` | `r: Rect3D -> BRect` | — | *conversion between 2D and 3D* |
| `BRect.createFromRect2D` | `r: Rect2D -> BRect` | `BBox.createFromRect3D` | `r: Rect3D -> BBox` |
| `BRect.createFromSeq` | `ps: Pt seq -> BRect` | `BBox.createFromSeq` | `ps: Pnt seq -> BBox` |
| `BRect.createUnchecked` | `minX: float * minY: float * maxX: float * maxY: float -> BRect` | `BBox.createUnchecked` | `minX: float * minY: float * minZ: float * maxX: float * maxY: float * maxZ: float -> BBox` |
| `BRect.createXY` | `ptAx: float * ptAy: float * ptBx: float * ptBy: float -> BRect` |  |  |
| `BRect.equals` | `tol: float -> a: BRect -> b: BRect -> bool` | `BBox.equals` | `tol: float -> a: BBox -> b: BBox -> bool` |
| `BRect.expandRel` | `factor: float -> r: BRect -> BRect` | `BBox.expandRel` | `factor: float -> b: BBox -> BBox` |
| `BRect.expandRelXY` | `factorX: float -> factorY: float -> r: BRect -> BRect` | `BBox.expandRelXYZ` | `factorX: float -> factorY: float -> factorZ: float -> b: BBox -> BBox` |
| `BRect.move` | `v: Vc -> r: BRect -> BRect` | `BBox.move` | `v: Vec -> b: BBox -> BBox` |
| `BRect.moveX` | `translation: float -> r: BRect -> BRect` | `BBox.moveX` | `translation: float -> b: BBox -> BBox` |
| `BRect.moveY` | `translation: float -> r: BRect -> BRect` | `BBox.moveY` | `translation: float -> b: BBox -> BBox` |
| — | *Z axis* | `BBox.moveZ` | `translation: float -> b: BBox -> BBox` |
| `BRect.notEquals` | `tol: float -> a: BRect -> b: BRect -> bool` | `BBox.notEquals` | `tol: float -> a: BBox -> b: BBox -> bool` |
| `BRect.scale` | `factor: float -> r: BRect -> BRect` | `BBox.scale` | `factor: float -> b: BBox -> BBox` |
| `BRect.translate` | `v: Vc -> r: BRect -> BRect` | `BBox.translate` | `v: Vec -> b: BBox -> BBox` |

## Tria2D / Tria3D

| Tria2D | Signature | Tria3D | Signature |
|:--|:--|:--|:--|
| `Tria2D.area` | `a: Pt * b: Pt * c: Pt -> float` | `Tria3D.area` | `a: Pnt * b: Pnt * c: Pnt -> float` |
| `Tria2D.areaDouble` | `a: Pt * b: Pt * c: Pt -> float` | `Tria3D.areaDouble` | `a: Pnt * b: Pnt * c: Pnt -> float` |
| `Tria2D.areaSigned` | `a: Pt * b: Pt * c: Pt -> float` | — | *orientation in the plane* |
| `Tria2D.det` | `a: Pt * b: Pt * c: Pt -> float` | — | *scalar cross product* |
| — | *single-point line-triangle intersection in 3D* | `Tria3D.intersectLine` | `line: Line3D * p1: Pnt * p2: Pnt * p3: Pnt -> Pnt voption` |
| — | *single-point line-triangle intersection in 3D* | `Tria3D.intersectRay` | `line: Line3D * p1: Pnt * p2: Pnt * p3: Pnt -> float voption` |
| — | *single-point line-triangle intersection in 3D* | `Tria3D.intersectRayXYZ` | `lnFromX: float * lnFromY: float * lnFromZ: float * lnToX: float * lnToY: float * lnToZ: float * ptAx: float * ptAy: float * ptAz: float * ptBx: float * ptBy: float * ptBz: float * ptCx: float * ptCy: float * ptCz: float -> float voption` |
| — | *single-point line-triangle intersection in 3D* | `Tria3D.intersectTriaVectorsWithRay` | `lnFromX: float * lnFromY: float * lnFromZ: float * dirX: float * dirY: float * dirZ: float * ptAx: float * ptAy: float * ptAz: float * e1x: float * e1y: float * e1z: float * e2x: float * e2y: float * e2z: float -> float voption` |
| `Tria2D.isLinear` | `a: Pt * b: Pt * c: Pt * ?distanceTolerance: float -> bool` | `Tria3D.isLinear` | `a: Pnt * b: Pnt * c: Pnt * ?distanceTolerance: float -> bool` |
| `Tria2D.isLinearFast` | `a: Pt * b: Pt * c: Pt * ?maxAreaParallelogram: float -> bool` | `Tria3D.isLinearFast` | `a: Pnt * b: Pnt * c: Pnt * ?maxAreaParallelogram: float -> bool` |
| `Tria2D.offset` | `a: Pt * b: Pt * c: Pt * dist: float -> Pt * Pt * Pt` | `Tria3D.offset` | `a: Pnt * b: Pnt * c: Pnt * dist: float -> Pnt * Pnt * Pnt` |
| `Tria2D.offsetPt` | `ptToOffset: Pt * prev: Pt * next: Pt * dist: float -> Pt` | `Tria3D.offsetPnt` | `pntToOffset: Pnt * prev: Pnt * next: Pnt * dist: float -> Pnt` |
| `Tria2D.offsetPtVar` | `ptToOffset: Pt * prev: Pt * next: Pt * distPrev: float * distNext: float -> ValueOption<Pt>` |  |  |
| `Tria2D.offsetPtVarByNormals` | `ptToOffset: Pt * prevN: UnitVc * nextN: UnitVc * prevDist: float * nextDist: float -> ValueOption<Pt>` |  |  |
|  |  | `Tria3D.offsetVar` | `ptToOffset: Pnt * prev: Pnt * next: Pnt * distPrev: float * distNext: float -> ValueOption<Pnt>` |

## Points2D / Points3D

| Points2D | Signature | Points3D | Signature |
|:--|:--|:--|:--|
| `Points2D.center` | `pts: IList<Pt> -> Pt` | `Points3D.center` | `pts: IList<Pnt> -> Pnt` |
| `Points2D.closestOfTwo` | `pt1: Pt -> pt2: Pt -> referencePoint: Pt -> Pt` | `Points3D.closestOfTwo` | `pt1: Pnt -> pt2: Pnt -> referencePoint: Pnt -> Pnt` |
| `Points2D.closestPoint` | `pts: IList<Pt> * pt: Pt -> Pt` | `Points3D.closestPoint` | `pts: IList<Pnt> * pt: Pnt -> Pnt` |
| `Points2D.closestPointIdx` | `pts: IList<Pt> * pt: Pt -> int` | `Points3D.closestPointIdx` | `pts: IList<Pnt> * pt: Pnt -> int` |
| `Points2D.closestPointsIdx` | `xs: IList<Pt> * ys: IList<Pt> -> int * int` | `Points3D.closestPointsIdx` | `xs: IList<Pnt> * ys: IList<Pnt> -> int * int` |
| `Points2D.getSignedArea` | `ps: IList<Pt> -> float` | — | *orientation in the plane* |
| `Points2D.minDistBetweenPointSets` | `xs: IList<Pt> * ys: IList<Pt> -> float` | `Points3D.minDistBetweenPointSets` | `xs: IList<Pnt> * ys: IList<Pnt> -> float` |
| `Points2D.mostDistantPoint` | `findPointFrom: IList<Pt> * checkAgainst: IList<Pt> -> Pt` | `Points3D.mostDistantPoint` | `findPointFrom: IList<Pnt> * checkAgainst: IList<Pnt> -> Pnt` |
| `Points2D.mostDistantPointIdx` | `findPointFrom: IList<Pt> * checkAgainst: IList<Pt> -> int * int` | `Points3D.mostDistantPointIdx` | `findPointFrom: IList<Pnt> * checkAgainst: IList<Pnt> -> int * int` |
| — | *plane or normal* | `Points3D.normalOfPoints` | `pts: IList<Pnt> -> Vec` |

## module Offset2D / module Offset3D

| Offset2D | Signature | Offset3D | Signature |
|:--|:--|:--|:--|
| — | *algorithm-specific 3D normal and direction representation* | `Offset3D.getCollinearNeighbors` | `dirs: ResizeArray<OffsetDirection voption> -> ResizeArray<CollinearPoint>` |
| — | *algorithm-specific 3D normal and direction representation* | `Offset3D.getOffsetDirections` | `uvs: ResizeArray<float> * refNormal: UnitVec * isOpen: bool * considerCollinearBelow: float<cosine> * failAtUTurnAbove: float<cosine> -> ResizeArray<OffsetDirection voption>` |
| `Offset2D.makeUnitTangents` | `xys: ResizeArray<float> -> ResizeArray<float>` | `Offset3D.getSegmentUnitVectors` | `xyzs: ResizeArray<float> -> ResizeArray<float>` |
| `Offset2D.makeOffsetDirections` | `xys: ResizeArray<float> -> ResizeArray<float>` | — | *algorithm-specific flat 2D direction buffer* |
| `Offset2D.offset` | `dist: float -> xys: ResizeArray<float> -> ResizeArray<float>` | `Offset3D.offset` | `distInPlane: float -> distPerpendicular: float -> xyzs: ResizeArray<float> -> ResizeArray<float>` |
| `Offset2D.offset'` | `uTurnBehavior: UTurn -> dist: float -> xys: ResizeArray<float> -> ResizeArray<float>` | `Offset3D.offset'` | `refNormal: UnitVec -> distInPlane: float -> distPerpendicular: float -> xyzs: ResizeArray<float> -> ResizeArray<float>` |
| `Offset2D.offset''` | `useUTurnBehaviorAbove: float<cosine> -> uTurnBehavior: UTurn -> dist: float -> xys: ResizeArray<float> -> ResizeArray<float>` |  |  |
|  |  | `Offset3D.offsetConstantWithDirections` | `xyzs: ResizeArray<float> * dirs: ResizeArray<OffsetDirection voption> * distInPlane: float * distPerpendicular: float -> ResizeArray<float>` |
| `Offset2D.offsetVariable` | `dists: ResizeArray<float> -> xys: ResizeArray<float> -> ResizeArray<float>` | `Offset3D.offsetVariable` | `distancesInPlane: ResizeArray<float> -> distancesPerpendicular: ResizeArray<float> -> xyzs: ResizeArray<float> -> ResizeArray<float>` |
| `Offset2D.offsetVariable'` | `uTurnBehavior: UTurn -> varDistParallelBehavior: VarDistParallel -> dists: ResizeArray<float> -> xys: ResizeArray<float> -> ResizeArray<float>` | `Offset3D.offsetVariable'` | `varDistParallelBehavior: VarDistParallel -> refNormal: UnitVec -> distancesInPlane: ResizeArray<float> -> distancesPerpendicular: ResizeArray<float> -> xyzs: ResizeArray<float> -> ResizeArray<float>` |
| `Offset2D.offsetVariable''` | `useUTurnBehaviorAbove: float<cosine> -> useVarDistParallelBehaviorBelow: float<cosine> -> uTurnBehavior: UTurn -> varDistParallelBehavior: VarDistParallel -> dists: ResizeArray<float> -> xys: ResizeArray<float> -> ResizeArray<float>` |  |  |
| `Offset2D.offsetVariableWithDirections` | `xys: ResizeArray<float> * nDirs: ResizeArray<float> * dists: IList<float> * varDistParallelBehavior: VarDistParallel * uTurnBehavior: UTurn * useVarDistParallelBehaviorBelow: float<cosine> * useUTurnBehaviorAbove: float<cosine> -> ResizeArray<float>` | `Offset3D.offsetVariableWithDirections` | `xyzs: ResizeArray<float> * segmentDirs: ResizeArray<float> * offDirs: ResizeArray<OffsetDirection voption> * distsInPlane: IList<float> * distsPerpendicular: IList<float> * isClosed: bool * varDistParallelBehavior: VarDistParallel -> ResizeArray<float>` |
| `Offset2D.offsetWithDirections` | `xys: ResizeArray<float> * dirs: ResizeArray<float> * dist: float * uTurnBehavior: UTurn * useUTurnBehaviorAbove: float<cosine> -> ResizeArray<float>` |  |  |
|  |  | `Offset3D.openTolerance` | `float` |
| `Offset2D.removeUTurns` | `xys: ResizeArray<float> * ns: ResizeArray<float> * minCos: float<cosine> -> ResizeArray<float>` | `Offset3D.removeUTurns` | `xyzs: ResizeArray<float> * uvs: ResizeArray<float> * minCos: float<cosine> -> ResizeArray<float>` |
| `Offset2D.setOffCorner` | `res: ResizeArray<float> * x: float * y: float * dist: float * nPrevX: float * nPrevY: float * nNextX: float * nNextY: float * cosine: float -> unit` | `Offset3D.setOffCorner` | `res: ResizeArray<float> * x: float * y: float * z: float * distInPlane: float * distPerp: float * nPrev: UnitVec * nNext: UnitVec * perpDir: UnitVec * cosine: float -> unit` |
| — | *algorithm-specific 3D normal and direction representation* | `Offset3D.setOffCornerVar` | `res: ResizeArray<float> * x: float * y: float * z: float * distInPlanePrev: float * distInPlaneNext: float * distPerp: float * nPrev: UnitVec * nNext: UnitVec * perpDir: UnitVec * cosine: float * vNext: UnitVec -> unit` |
| `Offset2D.sqOpenTolerance` | `float` | `Offset3D.sqOpenTolerance` | `float` |
| `Offset2D.toStringXYs` | `xys: ResizeArray<float> -> string` |  |  |

## Offset2D.UTurn

| UTurn | Signature |
|:--|:--|
| `Fail` | `UTurn = 1` |
| `Chamfer` | `UTurn = 2` |
| `UseThreshold` | `UTurn = 3` |

## Offset2D.VarDistParallel / Offset3D.VarDistParallel

| VarDistParallel | Signature | VarDistParallel | Signature |
|:--|:--|:--|:--|
| `Fail` | `VarDistParallel = 1` | `Fail` | `VarDistParallel = 1` |
| `Skip` | `VarDistParallel = 2` | `Skip` | `VarDistParallel = 2` |
| `Proportional` | `VarDistParallel = 3` | `Proportional` | `VarDistParallel = 3` |
| `Project` | `VarDistParallel = 4` | `Project` | `VarDistParallel = 4` |
| `StepWithTwoPoints` | `VarDistParallel = 5` | — | *2D two-point step policy* |

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

## Polyline2D / Polyline3D

| Polyline2D | Signature | Polyline3D | Signature |
|:--|:--|:--|:--|
| `new` | `capacity: int -> Polyline2D` | `new` | `capacity: int -> Polyline3D` |
|  |  | `new` | `points: Pnt seq -> Polyline3D` |
| `.AddPoint` | `pt: Pt -> unit` | `.AddPoint` | `pt: Pnt -> unit` |
| `Polyline2D.addPoint` | `pt: Pt -> p: Polyline2D -> unit` | `Polyline3D.addPoint` | `pt: Pnt -> p: Polyline3D -> unit` |
| `.AddXY` | `x: float * y: float -> unit` | `.AddXYZ` | `x: float * y: float * z: float -> unit` |
| `Polyline2D.addXY` | `x: float -> y: float -> p: Polyline2D -> unit` | `Polyline3D.addXYZ` | `x: float -> y: float -> z: float -> p: Polyline3D -> unit` |
| `.Area` | `float` | `.AreaIn2D` | `float` |
| `Polyline2D.area` | `p: Polyline2D -> float` | `Polyline3D.areaIn2D` | `pl: Polyline3D -> float` |
| `.AsFSharpCode` | `string` | `.AsFSharpCode` | `string` |
| `Polyline2D.asFSharpCode` | `p: Polyline2D -> string` | `Polyline3D.asFSharpCode` | `pl: Polyline3D -> string` |
| `.AsPoints` | `ResizeArray<Pt>` | `.AsPoints` | `ResizeArray<Pnt>` |
| `Polyline2D.asPoints` | `p: Polyline2D -> ResizeArray<Pt>` | `Polyline3D.asPoints` | `p: Polyline3D -> ResizeArray<Pnt>` |
| `.AsString` | `string` | `.AsString` | `string` |
| `Polyline2D.asString` | `p: Polyline2D -> string` | `Polyline3D.asString` | `pl: Polyline3D -> string` |
| — | *plane or normal* | `.AverageNormal` | `Vec` |
| — | *plane or normal* | `Polyline3D.averageNormal` | `pl: Polyline3D -> Vec` |
| — | *plane or normal* | `.AveragePlane` | `?tolerance: float -> NPlane` |
| — | *plane or normal* | `Polyline3D.averagePlane` | `tolerance: float -> pl: Polyline3D -> NPlane` |
| `.BoundingRectangle` | `BRect` | `.BoundingBox` | `BBox` |
| `Polyline2D.boundingRectangle` | `p: Polyline2D -> BRect` | `Polyline3D.boundingBox` | `pl: Polyline3D -> BBox` |
| `.Center` | `Pt` | `.Center` | `Pnt` |
| `Polyline2D.center` | `p: Polyline2D -> Pt` | `Polyline3D.center` | `pl: Polyline3D -> Pnt` |
| `.Clone` | `unit -> Polyline2D` | `.Clone` | `unit -> Polyline3D` |
| `Polyline2D.clone` | `p: Polyline2D -> Polyline2D` | `Polyline3D.clone` | `pl: Polyline3D -> Polyline3D` |
| `.CloseInPlace` | `?toleranceForAddingPoint: float -> unit` | `.CloseInPlace` | `?toleranceForAddingPoint: float -> unit` |
| `Polyline2D.closeInPlace` | `toleranceForAddingPoint: float -> pl: Polyline2D -> Polyline2D` | `Polyline3D.closeInPlace` | `toleranceForAddingPoint: float -> pl: Polyline3D -> Polyline3D` |
| `.ClosestParameter` | `p: Pt -> float` | `.ClosestParameter` | `p: Pnt -> float` |
| `Polyline2D.closestParameter` | `pl: Polyline2D -> pt: Pt -> float` | `Polyline3D.closestParameter` | `pl: Polyline3D -> pt: Pnt -> float` |
| `.ClosestParameterXY` | `x: float * y: float -> float` | `.ClosestParameterXYZ` | `x: float * y: float * z: float -> float` |
| `Polyline2D.closestParameterXY` | `pl: Polyline2D -> x: float -> y: float -> float` | `Polyline3D.closestParameterXYZ` | `pl: Polyline3D -> x: float -> y: float -> z: float -> float` |
| `.ClosestPoint` | `p: Pt -> Pt` | `.ClosestPoint` | `p: Pnt -> Pnt` |
| `Polyline2D.closestPoint` | `pl: Polyline2D -> pt: Pt -> Pt` | `Polyline3D.closestPoint` | `pl: Polyline3D -> pt: Pnt -> Pnt` |
| `.ClosestPointIndex` | `p: Pt -> int` | `.ClosestPointIndex` | `p: Pnt -> int` |
| `Polyline2D.closestPointIndex` | `pl: Polyline2D -> pt: Pt -> int` | `Polyline3D.closestPointIndex` | `pl: Polyline3D -> pt: Pnt -> int` |
| `.Contains` | `pt: Pt -> bool` | — | *inside of a planar area* |
| `Polyline2D.contains` | `pt: Pt -> pl: Polyline2D -> bool` | — | *inside of a planar area* |
| `.ContainsXY` | `x: float * y: float -> bool` | — | *inside of a planar area* |
| `Polyline2D.containsXY` | `x: float -> y: float -> pl: Polyline2D -> bool` | — | *inside of a planar area* |
| `.DistanceTo` | `p: Pt -> float` | `.DistanceTo` | `p: Pnt -> float` |
| `Polyline2D.distanceTo` | `pl: Polyline2D -> pt: Pt -> float` | `Polyline3D.distanceTo` | `pl: Polyline3D -> pt: Pnt -> float` |
| `.DistanceToXY` | `x: float * y: float -> float` | `.DistanceToXYZ` | `x: float * y: float * z: float -> float` |
| `Polyline2D.distanceToXY` | `pl: Polyline2D -> x: float -> y: float -> float` | `Polyline3D.distanceToXYZ` | `pl: Polyline3D -> x: float -> y: float -> z: float -> float` |
| `.Duplicate` | `unit -> Polyline2D` | `.Duplicate` | `unit -> Polyline3D` |
| `Polyline2D.duplicate` | `p: Polyline2D -> Polyline2D` | `Polyline3D.duplicate` | `pl: Polyline3D -> Polyline3D` |
| `.End` | `Pt with get, set` | `.End` | `Pnt with get, set` |
| `.EvaluateAt` | `t: float -> Pt` | `.EvaluateAt` | `t: float -> Pnt` |
| `Polyline2D.evaluateAt` | `t: float -> pl: Polyline2D -> Pt` | `Polyline3D.evaluateAt` | `t: float -> pl: Polyline3D -> Pnt` |
| `.FindLabelPoint` | `precision: float -> Pt * float` | — | *inside of a planar area* |
| `Polyline2D.findLabelPoint` | `precision: float -> pl: Polyline2D -> Pt * float` | — | *inside of a planar area* |
| `.FirstPoint` | `Pt with get, set` | `.FirstPoint` | `Pnt with get, set` |
| `Polyline2D.firstPoint` | `p: Polyline2D -> Pt` | `Polyline3D.firstPoint` | `pl: Polyline3D -> Pnt` |
| `.FirstSegment` | `Line2D` | `.FirstSegment` | `Line3D` |
| `Polyline2D.firstSegment` | `p: Polyline2D -> Line2D` | `Polyline3D.firstSegment` | `pl: Polyline3D -> Line3D` |
| `.GetPt` | `pointIndex: int -> Pt` | `.GetPt` | `pointIndex: int -> Pnt` |
| `Polyline2D.getPt` | `pointIndex: int -> p: Polyline2D -> Pt` | `Polyline3D.getPt` | `pointIndex: int -> p: Polyline3D -> Pnt` |
| `.GetSegment` | `i: int -> Line2D` | `.GetSegment` | `i: int -> Line3D` |
| `Polyline2D.getSegment` | `i: int -> p: Polyline2D -> Line2D` | `Polyline3D.getSegment` | `i: int -> pl: Polyline3D -> Line3D` |
| `.GetX` | `pointIndex: int -> float` | `.GetX` | `pointIndex: int -> float` |
| `Polyline2D.getX` | `pointIndex: int -> p: Polyline2D -> float` | `Polyline3D.getX` | `pointIndex: int -> p: Polyline3D -> float` |
| `.GetY` | `pointIndex: int -> float` | `.GetY` | `pointIndex: int -> float` |
| `Polyline2D.getY` | `pointIndex: int -> p: Polyline2D -> float` | `Polyline3D.getY` | `pointIndex: int -> p: Polyline3D -> float` |
| — | *Z axis* | `.GetZ` | `pointIndex: int -> float` |
| — | *Z axis* | `Polyline3D.getZ` | `pointIndex: int -> p: Polyline3D -> float` |
| `.IsAlmostClosed` | `tolerance: float -> bool` | `.IsAlmostClosed` | `tolerance: float -> bool` |
| `Polyline2D.isAlmostClosed` | `tolerance: float -> p: Polyline2D -> bool` | `Polyline3D.isAlmostClosed` | `tolerance: float -> pl: Polyline3D -> bool` |
| `.IsClockwise` | `bool` | `.IsClockwiseIn2D` | `bool` |
| `Polyline2D.isClockwise` | `p: Polyline2D -> bool` | `Polyline3D.isClockwiseIn2D` | `pl: Polyline3D -> bool` |
| `.IsClosed` | `bool` | `.IsClosed` | `bool` |
| `Polyline2D.isClosed` | `p: Polyline2D -> bool` | `Polyline3D.isClosed` | `pl: Polyline3D -> bool` |
| `.IsCounterClockwise` | `bool` | `.IsCounterClockwiseIn2D` | `bool` |
| `Polyline2D.isCounterClockwise` | `p: Polyline2D -> bool` | `Polyline3D.isCounterClockwiseIn2D` | `pl: Polyline3D -> bool` |
| — | *plane or normal* | `.IsPlanar` | `?tolerance: float -> bool` |
| — | *plane or normal* | `Polyline3D.isPlanar` | `tolerance: float -> pl: Polyline3D -> bool` |
| `.LastPoint` | `Pt with get, set` | `.LastPoint` | `Pnt with get, set` |
| `Polyline2D.lastPoint` | `p: Polyline2D -> Pt` | `Polyline3D.lastPoint` | `pl: Polyline3D -> Pnt` |
| `.LastSegment` | `Line2D` | `.LastSegment` | `Line3D` |
| `Polyline2D.lastSegment` | `p: Polyline2D -> Line2D` | `Polyline3D.lastSegment` | `pl: Polyline3D -> Line3D` |
| `.Length` | `float` | `.Length` | `float` |
| `Polyline2D.length` | `p: Polyline2D -> float` | `Polyline3D.length` | `p: Polyline3D -> float` |
| `.Move` | `v: Vc -> Polyline2D` | `.Move` | `v: Vec -> Polyline3D` |
| `Polyline2D.move` | `v: Vc -> pl: Polyline2D -> Polyline2D` | `Polyline3D.move` | `v: Vec -> pl: Polyline3D -> Polyline3D` |
| `.MoveX` | `distance: float -> Polyline2D` | `.MoveX` | `distance: float -> Polyline3D` |
| `Polyline2D.moveX` | `distance: float -> pl: Polyline2D -> Polyline2D` | `Polyline3D.moveX` | `distance: float -> pl: Polyline3D -> Polyline3D` |
| `.MoveY` | `distance: float -> Polyline2D` | `.MoveY` | `distance: float -> Polyline3D` |
| `Polyline2D.moveY` | `distance: float -> pl: Polyline2D -> Polyline2D` | `Polyline3D.moveY` | `distance: float -> pl: Polyline3D -> Polyline3D` |
| — | *Z axis* | `.MoveZ` | `distance: float -> Polyline3D` |
| — | *Z axis* | `Polyline3D.moveZ` | `distance: float -> pl: Polyline3D -> Polyline3D` |
| `.PointCount` | `int` | `.PointCount` | `int` |
| `Polyline2D.pointCount` | `p: Polyline2D -> int` | `Polyline3D.pointCount` | `p: Polyline3D -> int` |
| `.Reverse` | `unit -> Polyline2D` | `.Reverse` | `unit -> Polyline3D` |
| `Polyline2D.reverse` | `p: Polyline2D -> Polyline2D` | `Polyline3D.reverse` | `p: Polyline3D -> Polyline3D` |
| `.ReverseInPlace` | `unit -> unit` | `.ReverseInPlace` | `unit -> unit` |
| `Polyline2D.reverseInPlace` | `p: Polyline2D -> Polyline2D` | `Polyline3D.reverseInPlace` | `p: Polyline3D -> Polyline3D` |
| `.Rotate` | `r: Rotation2D -> Polyline2D` | `.Rotate` | `q: Quaternion -> Polyline3D` |
| `Polyline2D.rotate` | `r: Rotation2D -> pl: Polyline2D -> Polyline2D` | `Polyline3D.rotate` | `q: Quaternion -> pl: Polyline3D -> Polyline3D` |
| — | *rotation about a world axis* | `.RotateOnZ` | `r: Rotation2D -> Polyline3D` |
| — | *rotation about a world axis* | `Polyline3D.rotateOnZ` | `r: Rotation2D -> pl: Polyline3D -> Polyline3D` |
| — | *rotation about a world axis* | `.RotateOnZWithCenter` | `cen: Pnt -> r: Rotation2D -> Polyline3D` |
| — | *rotation about a world axis* | `Polyline3D.rotateOnZWithCenter` | `cen: Pnt -> r: Rotation2D -> pl: Polyline3D -> Polyline3D` |
| `.RotateWithCenter` | `cen: Pt * r: Rotation2D -> Polyline2D` | `.RotateWithCenter` | `cen: Pnt * q: Quaternion -> Polyline3D` |
| `Polyline2D.rotateWithCenter` | `cen: Pt -> r: Rotation2D -> pl: Polyline2D -> Polyline2D` | `Polyline3D.rotateWithCenter` | `cen: Pnt -> q: Quaternion -> pl: Polyline3D -> Polyline3D` |
| `.Scale` | `factor: float -> Polyline2D` | `.Scale` | `factor: float -> Polyline3D` |
| `Polyline2D.scale` | `factor: float -> pl: Polyline2D -> Polyline2D` | `Polyline3D.scale` | `factor: float -> pl: Polyline3D -> Polyline3D` |
| `.ScaleOn` | `cen: Pt -> factor: float -> Polyline2D` | `.ScaleOn` | `cen: Pnt -> factor: float -> Polyline3D` |
| `Polyline2D.scaleOn` | `cen: Pt -> factor: float -> pl: Polyline2D -> Polyline2D` | `Polyline3D.scaleOn` | `cen: Pnt -> factor: float -> pl: Polyline3D -> Polyline3D` |
| `.SecondLastPoint` | `Pt with get, set` | `.SecondLastPoint` | `Pnt with get, set` |
| `Polyline2D.secondLastPoint` | `p: Polyline2D -> Pt` | `Polyline3D.secondLastPoint` | `pl: Polyline3D -> Pnt` |
| `.SecondPoint` | `Pt with get, set` | `.SecondPoint` | `Pnt with get, set` |
| `Polyline2D.secondPoint` | `p: Polyline2D -> Pt` | `Polyline3D.secondPoint` | `pl: Polyline3D -> Pnt` |
| `.SegmentCount` | `int` | `.SegmentCount` | `int` |
| `Polyline2D.segmentCount` | `p: Polyline2D -> int` | `Polyline3D.segmentCount` | `p: Polyline3D -> int` |
| `.Segments` | `ResizeArray<Line2D>` | `.Segments` | `ResizeArray<Line3D>` |
| `Polyline2D.segments` | `p: Polyline2D -> ResizeArray<Line2D>` | `Polyline3D.segments` | `pl: Polyline3D -> ResizeArray<Line3D>` |
| `.SegmentVectors` | `ResizeArray<Vc>` | `.SegmentVectors` | `ResizeArray<Vec>` |
| `Polyline2D.segmentVectors` | `p: Polyline2D -> ResizeArray<Vc>` | `Polyline3D.segmentVectors` | `pl: Polyline3D -> ResizeArray<Vec>` |
| `.SegmentVectorsXY` | `ResizeArray<float>` | `.SegmentVectorsXYZ` | `ResizeArray<float>` |
| `Polyline2D.segmentVectorsXY` | `p: Polyline2D -> ResizeArray<float>` | `Polyline3D.segmentVectorsXYZ` | `p: Polyline3D -> ResizeArray<float>` |
| `.SetPt` | `pointIndex: int * pt: Pt -> unit` | `.SetPnt` | `pointIndex: int * pt: Pnt -> unit` |
| `Polyline2D.setPt` | `pointIndex: int -> pt: Pt -> p: Polyline2D -> unit` | `Polyline3D.setPnt` | `pointIndex: int -> pt: Pnt -> p: Polyline3D -> unit` |
| `.SetPointXY` | `pointIndex: int * x: float * y: float -> unit` | `.SetPointXYZ` | `pointIndex: int * x: float * y: float * z: float -> unit` |
| `Polyline2D.setPointXY` | `x: float -> y: float -> pointIndex: int -> p: Polyline2D -> unit` | `Polyline3D.setPointXYZ` | `x: float -> y: float -> z: float -> pointIndex: int -> p: Polyline3D -> unit` |
| `.SetPointXYClosed` | `pointIndex: int * x: float * y: float -> unit` | `.SetPointXYZClosed` | `pointIndex: int * x: float * y: float * z: float -> unit` |
| `Polyline2D.setPointXYClosed` | `x: float -> y: float -> pointIndex: int -> p: Polyline2D -> unit` | `Polyline3D.setPointXYZClosed` | `x: float -> y: float -> z: float -> pointIndex: int -> p: Polyline3D -> unit` |
| `.SignedArea` | `float` | `.SignedAreaIn2D` | `float` |
| `Polyline2D.signedArea` | `p: Polyline2D -> float` | `Polyline3D.signedAreaIn2D` | `pl: Polyline3D -> float` |
| `.SignedDistanceTo` | `point: Pt -> float` | — | *orientation in the plane* |
| `Polyline2D.signedDistanceTo` | `point: Pt -> pl: Polyline2D -> float` | — | *orientation in the plane* |
| `.SignedDistanceToXY` | `x: float * y: float -> float` | — | *orientation in the plane* |
| `Polyline2D.signedDistanceToXY` | `x: float -> y: float -> pl: Polyline2D -> float` | — | *orientation in the plane* |
| `.Start` | `Pt with get, set` | `.Start` | `Pnt with get, set` |
| `Polyline2D.start` | `p: Polyline2D -> Pt` | `Polyline3D.start` | `pl: Polyline3D -> Pnt` |
| `.ToString` | `unit -> string` | `.ToString` | `unit -> string` |
| — | *transformation matrix* | `.Transform` | `m: Matrix -> Polyline3D` |
| — | *transformation matrix* | `Polyline3D.transform` | `m: Matrix -> pl: Polyline3D -> Polyline3D` |
| — | *transformation matrix* | `.TransformRigid` | `m: RigidMatrix -> Polyline3D` |
| — | *transformation matrix* | `Polyline3D.transformRigid` | `m: RigidMatrix -> pl: Polyline3D -> Polyline3D` |
| — | *plane or normal* | `.TryAverageNormal` | `Vec option` |
| — | *plane or normal* | `Polyline3D.tryAverageNormal` | `pl: Polyline3D -> Vec option` |
| — | *plane or normal* | `.TryAveragePlane` | `?tolerance: float -> NPlane option` |
| — | *plane or normal* | `Polyline3D.tryAveragePlane` | `tolerance: float -> pl: Polyline3D -> NPlane option` |
| — | *plane or normal* | `.TryPlane` | `?tolerance: float -> NPlane option` |
| — | *plane or normal* | `Polyline3D.tryPlane` | `tolerance: float -> pl: Polyline3D -> NPlane option` |
| `.WindingNumber` | `point: Pt -> int` | — | *orientation in the plane* |
| `Polyline2D.windingNumber` | `point: Pt -> pl: Polyline2D -> int` | — | *orientation in the plane* |
| `.XYs` | `ResizeArray<float>` | `.XYZs` | `ResizeArray<float>` |
| **Other static members** | | | |
| `Polyline2D.checkForDuplicatePoints` | `distanceTolerance: float -> pl: Polyline2D -> Result<Polyline2D,(Polyline2D * ResizeArray<Pt>)>` | `Polyline3D.checkForDuplicatePoints` | `distanceTolerance: float -> pl: Polyline3D -> Result<Polyline3D,(Polyline3D * ResizeArray<Pnt>)>` |
| `Polyline2D.close` | `toleranceForAddingPoint: float -> pl: Polyline2D -> Polyline2D` | `Polyline3D.close` | `toleranceForAddingPoint: float -> pl: Polyline3D -> Polyline3D` |
| `Polyline2D.createDirectly` | `xys: ResizeArray<float> -> Polyline2D` | `Polyline3D.createDirectly` | `xyzs: ResizeArray<float> -> Polyline3D` |
| `Polyline2D.createEmpty` | `capacity: int -> Polyline2D` | `Polyline3D.createEmpty` | `capacity: int -> Polyline3D` |
| — | *conversion between 2D and 3D* | `Polyline3D.createFrom2DXYMembers` | `xyObjs: ^T seq -> Polyline3D` |
| — | *conversion between 2D and 3D* | `Polyline3D.createFrom2DxyMembers` | `xyObjs: ^T seq -> Polyline3D` |
| `Polyline2D.createFromBRectCCW` | `r: BRect -> Polyline2D` | — | *orientation in the plane* |
| `Polyline2D.createFromBRectCW` | `r: BRect -> Polyline2D` | — | *orientation in the plane* |
| `Polyline2D.createFromPts` | `points: Pt seq -> Polyline2D` | `Polyline3D.createFromPts` | `points: Pnt seq -> Polyline3D` |
| `Polyline2D.createFromRectCCW` | `r: Rect2D -> Polyline2D` | `Polyline3D.createFromRect3D` | `r: Rect3D -> Polyline3D` |
| `Polyline2D.createFromRectCW` | `r: Rect2D -> Polyline2D` | — | *orientation in the plane* |
| `Polyline2D.createFromxyMembers` | `xyObjs: ^T seq -> Polyline2D` |  |  |
| `Polyline2D.createFromXYMembers` | `xyObjs: ^T seq -> Polyline2D` | `Polyline3D.createFromXYZMembers` | `xyzObjs: ^T seq -> Polyline3D` |
|  |  | `Polyline3D.createFromxyzMembers` | `xyzObjs: ^T seq -> Polyline3D` |
| `Polyline2D.end'` | `p: Polyline2D -> Pt` | `Polyline3D.end'` | `pl: Polyline3D -> Pnt` |
| `Polyline2D.ensureClockwise` | `pl: Polyline2D -> Polyline2D` | `Polyline3D.ensureClockwiseIn2D` | `pl: Polyline3D -> Polyline3D` |
| `Polyline2D.ensureClockwiseInPlace` | `pl: Polyline2D -> Polyline2D` | `Polyline3D.ensureClockwiseInPlaceIn2D` | `pl: Polyline3D -> Polyline3D` |
| `Polyline2D.ensureCounterClockwise` | `pl: Polyline2D -> Polyline2D` | `Polyline3D.ensureCounterClockwiseIn2D` | `pl: Polyline3D -> Polyline3D` |
| `Polyline2D.ensureCounterClockwiseInPlace` | `pl: Polyline2D -> Polyline2D` | `Polyline3D.ensureCounterClockwiseInPlaceIn2D` | `pl: Polyline3D -> Polyline3D` |
| `Polyline2D.equals` | `pl1: Polyline2D -> pl2: Polyline2D -> bool` | `Polyline3D.equals` | `pl1: Polyline3D -> pl2: Polyline3D -> bool` |
| `Polyline2D.equalsTol` | `tol: float -> pl1: Polyline2D -> pl2: Polyline2D -> bool` | `Polyline3D.equalsTol` | `tol: float -> pl1: Polyline3D -> pl2: Polyline3D -> bool` |
| `Polyline2D.getXYs` | `p: Polyline2D -> ResizeArray<float>` | `Polyline3D.getXYZs` | `p: Polyline3D -> ResizeArray<float>` |
| `Polyline2D.iter` | `action: (float -> float -> unit) -> pl: Polyline2D -> unit` | `Polyline3D.iter` | `action: (float -> float -> float -> unit) -> pl: Polyline3D -> unit` |
| `Polyline2D.iteri` | `action: (int -> float -> float -> unit) -> pl: Polyline2D -> unit` | `Polyline3D.iteri` | `action: (int -> float -> float -> float -> unit) -> pl: Polyline3D -> unit` |
| `Polyline2D.iteriPt` | `action: (int -> Pt -> unit) -> pl: Polyline2D -> unit` | `Polyline3D.iteriPnt` | `action: (int -> Pnt -> unit) -> pl: Polyline3D -> unit` |
| `Polyline2D.iterLineSegments` | `action: (Line2D -> unit) -> pl: Polyline2D -> unit` | `Polyline3D.iterLineSegments` | `action: (Line3D -> unit) -> pl: Polyline3D -> unit` |
| `Polyline2D.iterPt` | `action: (Pt -> unit) -> pl: Polyline2D -> unit` | `Polyline3D.iterPnt` | `action: (Pnt -> unit) -> pl: Polyline3D -> unit` |
| `Polyline2D.iterSegments` | `action: (float -> float -> float -> float -> unit) -> pl: Polyline2D -> unit` | `Polyline3D.iterSegments` | `action: (float -> float -> float -> float -> float -> float -> unit) -> pl: Polyline3D -> unit` |
| `Polyline2D.map` | `mapping: (float -> float -> Pt) -> pl: Polyline2D -> Polyline2D` | `Polyline3D.map` | `mapping: (float -> float -> float -> Pnt) -> pl: Polyline3D -> Polyline3D` |
| `Polyline2D.mapi` | `mapping: (int -> float -> float -> Pt) -> pl: Polyline2D -> Polyline2D` | `Polyline3D.mapi` | `mapping: (int -> float -> float -> float -> Pnt) -> pl: Polyline3D -> Polyline3D` |
| `Polyline2D.mapiPt` | `mapping: (int -> Pt -> Pt) -> pl: Polyline2D -> Polyline2D` | `Polyline3D.mapiPnt` | `mapping: (int -> Pnt -> Pnt) -> pl: Polyline3D -> Polyline3D` |
| `Polyline2D.mapPt` | `mapping: (Pt -> Pt) -> pl: Polyline2D -> Polyline2D` | `Polyline3D.mapPnt` | `mapping: (Pnt -> Pnt) -> pl: Polyline3D -> Polyline3D` |
| `Polyline2D.offset` | `polyLine: Polyline2D * constantOffsetDistance: float * ?loop: bool * ?checkOrientation: bool * ?uTurnBehavior: UTurn * ?useUTurnBehaviorAbove: float<cosine> -> Polyline2D` | `Polyline3D.offset` | `polyLine: Polyline3D * inPlaneOffsetDistance: float * perpendicularOffsetDistance: float * ?loop: bool -> Polyline3D` |
| `Polyline2D.offset'` | `offsetDistance: float -> pl: Polyline2D -> Polyline2D` | `Polyline3D.offset'` | `inPlaneOffsetDistance: float -> perpendicularOffsetDistance: float -> polyLine: Polyline3D -> Polyline3D` |
| `Polyline2D.offsetVar` | `polyLine: Polyline2D * multipleOffsetDistances: IList<float> * ?loop: bool * ?checkOrientation: bool * ?varDistParallelBehavior: VarDistParallel * ?uTurnBehavior: UTurn * ?useVarDistParallelBehaviorBelow: float<cosine> * ?useUTurnBehaviorAbove: float<cosine> -> Polyline2D` | `Polyline3D.offsetVar` | `polyLine: Polyline3D * inPlaneOffsetDistances: ResizeArray<float> * perpendicularOffsetDistances: ResizeArray<float> * ?loop: bool * ?varDistParallelBehavior: VarDistParallel * ?considerCollinearBelow: float<cosine> * ?failAtUTurnAbove: float<cosine> -> Polyline3D` |
|  |  | `Polyline3D.offsetVarWithRef` | `polyLine: Polyline3D * inPlaneOffsetDistances: IList<float> * perpendicularOffsetDistances: IList<float> * refNormal: UnitVec * ?loop: bool * ?varDistParallelBehavior: VarDistParallel * ?considerCollinearBelow: float<cosine> * ?failAtUTurnAbove: float<cosine> -> Polyline3D` |
|  |  | `Polyline3D.offsetWithRef` | `polyLine: Polyline3D * inPlaneOffsetDistance: float * perpendicularOffsetDistance: float * refNormal: UnitVec * ?loop: bool -> Polyline3D` |
| `Polyline2D.removeDuplicateAndCollinearPoints` | `angleTolerance: float<cosine> -> distanceTolerance: float -> pl: Polyline2D -> Polyline2D` | `Polyline3D.removeDuplicateAndCollinearPoints` | `angleTolerance: float<cosine> -> distanceTolerance: float -> pl: Polyline3D -> Polyline3D` |
| `Polyline2D.removeDuplicatePoints` | `distanceTolerance: float -> pl: Polyline2D -> Polyline2D` | `Polyline3D.removeDuplicatePoints` | `distanceTolerance: float -> pl: Polyline3D -> Polyline3D` |
| `Polyline2D.removeDuplicatePointsFaithfully` | `distanceTolerance: float -> pl: Polyline2D -> Polyline2D` | `Polyline3D.removeDuplicatePointsFaithfully` | `distanceTolerance: float -> pl: Polyline3D -> Polyline3D` |
| `Polyline2D.removeUTurns` | `minCos: float<cosine> -> polyLine: Polyline2D -> Polyline2D` | `Polyline3D.removeUTurns` | `minCos: float<cosine> -> polyLine: Polyline3D -> Polyline3D` |
| `Polyline2D.removeUTurnsDeeply` | `minCos: float<cosine> -> polyLine: Polyline2D -> Polyline2D` | `Polyline3D.removeUTurnsDeeply` | `minCos: float<cosine> -> polyLine: Polyline3D -> Polyline3D` |
| `Polyline2D.subPolyline` | `a: float -> b: float -> pl: Polyline2D -> Polyline2D` | `Polyline3D.subPolyline` | `a: float -> b: float -> pl: Polyline3D -> Polyline3D` |
| `Polyline2D.translate` | `v: Vc -> pl: Polyline2D -> Polyline2D` | `Polyline3D.translate` | `v: Vec -> pl: Polyline3D -> Polyline3D` |
| `Polyline2D.tryFind` | `condition: (float -> float -> bool) -> pl: Polyline2D -> Pt option` | `Polyline3D.tryFind` | `condition: (float -> float -> float -> bool) -> pl: Polyline3D -> Pnt option` |
| `Polyline2D.tryFindIndex` | `condition: (float -> float -> bool) -> pl: Polyline2D -> int option` | `Polyline3D.tryFindIndex` | `condition: (float -> float -> float -> bool) -> pl: Polyline3D -> int option` |
| `Polyline2D.tryFindLast` | `condition: (float -> float -> bool) -> pl: Polyline2D -> Pt option` | `Polyline3D.tryFindLast` | `condition: (float -> float -> float -> bool) -> pl: Polyline3D -> Pnt option` |
| `Polyline2D.tryFindLastIndex` | `condition: (float -> float -> bool) -> pl: Polyline2D -> int option` | `Polyline3D.tryFindLastIndex` | `condition: (float -> float -> float -> bool) -> pl: Polyline3D -> int option` |
| `Polyline2D.tryFindSelfIntersection` | `pl: Polyline2D -> Option<Pt * int * int>` |  |  |
| `Polyline2D.tryFindSelfIntersectionWithBRect` | `pl: Polyline2D -> Option<Pt * int * int>` |  |  |

## Topology2D / Topology3D

| Topology2D | Signature | Topology3D | Signature |
|:--|:--|:--|:--|
| `Topology2D.join` | `getLine: ('T -> Line2D) * splitDistance: float * xs: ResizeArray<'T> -> ResizeArray<ResizeArray<'T>>` | `Topology3D.join` | `getLine: ('T -> Line3D) * splitDistance: float * xs: ResizeArray<'T> -> ResizeArray<ResizeArray<'T>>` |
| `Topology2D.joinReversing` | `getLine: ('T -> Line2D) * splitDistance: float * xs: ResizeArray<'T> -> ResizeArray<ResizeArray<'T * bool>>` | `Topology3D.joinReversing` | `getLine: ('T -> Line3D) * splitDistance: float * xs: ResizeArray<'T> -> ResizeArray<ResizeArray<'T * bool>>` |
| `Topology2D.sortToLoop` | `getLine: ('T -> Line2D) * ts: ResizeArray<'T> -> unit` | `Topology3D.sortToLoop` | `getLine: ('T -> Line3D) * ts: ResizeArray<'T> -> unit` |
| `Topology2D.sortToLoopCached` | `getLine: ('T -> Line2D) * ts: ResizeArray<'T> -> float array` | `Topology3D.sortToLoopCached` | `getLine: ('T -> Line3D) * ts: ResizeArray<'T> -> float array` |
| `Topology2D.sortToLoopWithReversing` | `getLine: ('T -> Line2D) * reverseInPlace: (int -> 'T -> unit) * ts: ResizeArray<'T> -> unit` | `Topology3D.sortToLoopWithReversing` | `getLine: ('T -> Line3D) * reverseInPlace: (int -> 'T -> unit) * ts: ResizeArray<'T> -> unit` |
| `Topology2D.sortToLoopWithReversingCached` | `getLine: ('T -> Line2D) * reverseInPlace: (int -> 'T -> unit) * ts: ResizeArray<'T> -> float array` | `Topology3D.sortToLoopWithReversingCached` | `getLine: ('T -> Line3D) * reverseInPlace: (int -> 'T -> unit) * ts: ResizeArray<'T> -> float array` |

## module Similarity2D

| Similarity2D | Signature |
|:--|:--|
| `Similarity2D.areSimilar` | `tol: float -> a: ObjectToCheck -> b: ObjectToCheck -> bool` |
| `Similarity2D.getGrouped` | `tolerance: float * items: ResizeArray<'T> * sims: ResizeArray<ObjectToCheck> -> ResizeArray<ResizeArray<'T>>` |
| `Similarity2D.getSimilarityData` | `ptss: ResizeArray<string * ResizeArray<Pt>> -> ObjectToCheck` |

## Similarity2D.GroupInsideObjectToCheck

| GroupInsideObjectToCheck | Signature |
|:--|:--|
| `.category` | `string` |
| `.bRect` | `BRect` |
| `.points` | `Pt array` |

## Similarity2D.ObjectToCheck

| ObjectToCheck | Signature |
|:--|:--|
| `.extend` | `Pt` |
| `.groups` | `GroupInsideObjectToCheck array` |

## module EuclidErrors

| EuclidErrors | Signature |
|:--|:--|
| `EuclidErrors.fail` | `msg: string -> 'Failed` |
| `EuclidErrors.fail1` | `msg: string -> a: 'T -> 'Failed` |
| `EuclidErrors.fail2` | `msg: string -> a: 'T -> b: 'U -> 'Failed` |
| `EuclidErrors.fail3` | `msg: string -> a: 'T -> b: 'U -> c: 'V -> 'Failed` |
| `EuclidErrors.failCollinear` | `msg: string -> a: 'T -> b: 'U -> c: 'V -> 'Failed` |
| `EuclidErrors.failDivide` | `msg: string -> div: float -> obj: 'T -> 'Failed` |
| `EuclidErrors.failEmptySeq` | `funcName: string -> arg: string -> 'Failed` |
| `EuclidErrors.failEven` | `methodName: 'a -> xys: ResizeArray<float> -> 'b` |
| `EuclidErrors.failMod3` | `methodName: 'a -> xyzs: ResizeArray<float> -> 'b` |
| `EuclidErrors.failNaN2` | `msg: string -> x: float -> y: float -> 'Failed` |
| `EuclidErrors.failNaN3` | `msg: string -> x: float -> y: float -> z: float -> 'Failed` |
| `EuclidErrors.failNotOne2` | `msg: string -> x: float -> y: float -> 'Failed` |
| `EuclidErrors.failNotOne3` | `msg: string -> x: float -> y: float -> z: float -> 'Failed` |
| `EuclidErrors.failNull` | `funcName: string -> arg: string -> 'Failed` |
| `EuclidErrors.failObsolete` | `funName: string -> newFunName: string -> 'Failed` |
| `EuclidErrors.failObsoleteV20` | `funName: string -> newFunName: string -> 'Failed` |
| `EuclidErrors.failQuat` | `w: float -> x: float -> y: float -> z: float -> 'Failed` |
| `EuclidErrors.failRarr` | `msg: string -> xs: ResizeArray<'T> -> 'Failed` |
| `EuclidErrors.failRect2DOffsetEdge` | `offStart: float * offEnd: float * len: float * edgeIdx: int * d: float -> 'Failed` |
| `EuclidErrors.failRect3DOffsetEdge` | `offStart: float * offEnd: float * len: float * edgeIdx: int * d: float -> 'Failed` |
| `EuclidErrors.failRot` | `x: float -> y: float -> 'Failed` |
| `EuclidErrors.failTooClose` | `msg: string -> this: 'T -> other: 'U -> 'Failed` |
| `EuclidErrors.failTooFewPoly2D` | `name: string -> minCount: int -> actual: int -> 'Failed` |
| `EuclidErrors.failTooFewPoly3D` | `name: string -> minCount: int -> actual: int -> 'Failed` |
| `EuclidErrors.failTooSmall` | `msg: string -> this: 'T -> 'Failed` |
| `EuclidErrors.failTooSmall2` | `msg: string -> this: 'T -> other: 'U -> 'Failed` |
| `EuclidErrors.failUnit2` | `msg: string -> x: float -> y: float -> 'Failed` |
| `EuclidErrors.failUnit3` | `msg: string -> x: float -> y: float -> z: float -> 'Failed` |
| `EuclidErrors.failVertical` | `msg: string -> v: 'T -> 'Failed` |

## EuclidErrors.EuclidException

| EuclidException | Signature |
|:--|:--|
| `new` | `s: string -> EuclidException` |

## EuclidErrors.EuclidDivByZeroException

| EuclidDivByZeroException | Signature |
|:--|:--|
| `new` | `s: string -> EuclidDivByZeroException` |

## EuclidErrors.EuclidUnitizingException

| EuclidUnitizingException | Signature |
|:--|:--|
| `new` | `s: string -> EuclidUnitizingException` |

## EuclidErrors.EuclidNanInfinityException

| EuclidNanInfinityException | Signature |
|:--|:--|
| `new` | `s: string -> EuclidNanInfinityException` |

## EuclidErrors.EuclidTooSmallException

| EuclidTooSmallException | Signature |
|:--|:--|
| `new` | `s: string -> EuclidTooSmallException` |

## EuclidErrors.EuclidNullException

| EuclidNullException | Signature |
|:--|:--|
| `new` | `s: string -> EuclidNullException` |

## EuclidErrors.EuclidEmptySeqException

| EuclidEmptySeqException | Signature |
|:--|:--|
| `new` | `s: string -> EuclidEmptySeqException` |

## EuclidErrors.EuclidObsoleteException

| EuclidObsoleteException | Signature |
|:--|:--|
| `new` | `s: string -> EuclidObsoleteException` |

## module UtilEuclid

| UtilEuclid | Signature |
|:--|:--|
| `UtilEuclid.acosSafe` | `a: float -> float` |
| `UtilEuclid.asinSafe` | `a: float -> float` |
| `UtilEuclid.axisAlignmentTolerance` | `float` |
| `UtilEuclid.clamp01` | `x: float -> float` |
| `UtilEuclid.clamp01NaN` | `x: float -> float` |
| `UtilEuclid.clampBetweenMinusOneAndOne` | `x: float -> float` |
| `UtilEuclid.countTooTinyOrNaN` | `x: float -> int` |
| `UtilEuclid.countTooTinySqOrNaN` | `x: float -> int` |
| `UtilEuclid.halfPi` | `float` |
| `UtilEuclid.isBetweenZeroAndOne` | `x: float -> bool` |
| `UtilEuclid.isBetweenZeroAndOneTolerantExcl` | `x: float -> bool` |
| `UtilEuclid.isBetweenZeroAndOneTolerantIncl` | `x: float -> bool` |
| `UtilEuclid.isMinusOne` | `x: float -> bool` |
| `UtilEuclid.isNanInfinity` | `x: float -> bool` |
| `UtilEuclid.isNegative` | `x: float -> bool` |
| `UtilEuclid.isNotOne` | `x: float -> bool` |
| `UtilEuclid.isNotZero` | `x: float -> bool` |
| `UtilEuclid.isOne` | `x: float -> bool` |
| `UtilEuclid.isTooSmall` | `x: float -> bool` |
| `UtilEuclid.isTooSmallSq` | `x: float -> bool` |
| `UtilEuclid.isTooTiny` | `x: float -> bool` |
| `UtilEuclid.isTooTinySq` | `x: float -> bool` |
| `UtilEuclid.isZero` | `x: float -> bool` |
| `UtilEuclid.matchSign` | `signedValue: float -> numToMatch: float -> float` |
| `UtilEuclid.safeIdx` | `i: int -> length: int -> int` |
| `UtilEuclid.toDegrees` | `radians: float -> float` |
| `UtilEuclid.toRadians` | `degrees: float -> float` |
| `UtilEuclid.twoPi` | `float` |
| `UtilEuclid.withMeasure` | `x: float -> float<'measure>` |
| `UtilEuclid.zeroLengthTolerance` | `float` |
| ``` UtilEuclid.``-1.0 + 1e-6`` ``` | `float` |
| ``` UtilEuclid.``-1.0 - 1e-6`` ``` | `float` |
| ``` UtilEuclid.``1.0 + 1e-6`` ``` | `float` |
| ``` UtilEuclid.``1.0 - 1e-6`` ``` | `float` |
| `( !^ )` | `x: float -> float<'measure>` |

## module Cosine

| Cosine | Signature |
|:--|:--|
| `cosine` | `[<Measure>] type` |
| **Functions and values** | |
| `Cosine.inDegrees` | `c: float<cosine> -> float` |
| ``` Cosine.``0.0+`` ``` | `float<cosine>` |
| ``` Cosine.``0.0`` ``` | `float<cosine>` |
| ``` Cosine.``0.01`` ``` | `float<cosine>` |
| ``` Cosine.``0.025`` ``` | `float<cosine>` |
| ``` Cosine.``0.05`` ``` | `float<cosine>` |
| ``` Cosine.``0.1`` ``` | `float<cosine>` |
| ``` Cosine.``0.25`` ``` | `float<cosine>` |
| ``` Cosine.``0.5`` ``` | `float<cosine>` |
| ``` Cosine.``1.0`` ``` | `float<cosine>` |
| ``` Cosine.``10.0`` ``` | `float<cosine>` |
| ``` Cosine.``15.0`` ``` | `float<cosine>` |
| ``` Cosine.``100.0`` ``` | `float<cosine>` |
| ``` Cosine.``105.0`` ``` | `float<cosine>` |
| ``` Cosine.``110.0`` ``` | `float<cosine>` |
| ``` Cosine.``120.0`` ``` | `float<cosine>` |
| ``` Cosine.``135.0`` ``` | `float<cosine>` |
| ``` Cosine.``150.0`` ``` | `float<cosine>` |
| ``` Cosine.``160.0`` ``` | `float<cosine>` |
| ``` Cosine.``165.0`` ``` | `float<cosine>` |
| ``` Cosine.``170.0`` ``` | `float<cosine>` |
| ``` Cosine.``175.0`` ``` | `float<cosine>` |
| ``` Cosine.``177.5`` ``` | `float<cosine>` |
| ``` Cosine.``179.0`` ``` | `float<cosine>` |
| ``` Cosine.``179.5`` ``` | `float<cosine>` |
| ``` Cosine.``179.75`` ``` | `float<cosine>` |
| ``` Cosine.``179.9`` ``` | `float<cosine>` |
| ``` Cosine.``179.95`` ``` | `float<cosine>` |
| ``` Cosine.``179.975`` ``` | `float<cosine>` |
| ``` Cosine.``179.99`` ``` | `float<cosine>` |
| ``` Cosine.``180.0-`` ``` | `float<cosine>` |
| ``` Cosine.``2.5`` ``` | `float<cosine>` |
| ``` Cosine.``5.0`` ``` | `float<cosine>` |
| ``` Cosine.``20.0`` ``` | `float<cosine>` |
| ``` Cosine.``30.0`` ``` | `float<cosine>` |
| ``` Cosine.``45.0`` ``` | `float<cosine>` |
| ``` Cosine.``60.0`` ``` | `float<cosine>` |
| ``` Cosine.``70.0`` ``` | `float<cosine>` |
| ``` Cosine.``75.0`` ``` | `float<cosine>` |
| ``` Cosine.``80.0`` ``` | `float<cosine>` |
| ``` Cosine.``85.0`` ``` | `float<cosine>` |
| ``` Cosine.``87.5`` ``` | `float<cosine>` |
| ``` Cosine.``89.0`` ``` | `float<cosine>` |
| ``` Cosine.``89.5`` ``` | `float<cosine>` |
| ``` Cosine.``89.75`` ``` | `float<cosine>` |
| ``` Cosine.``89.9`` ``` | `float<cosine>` |
| ``` Cosine.``89.95`` ``` | `float<cosine>` |
| ``` Cosine.``89.975`` ``` | `float<cosine>` |
| ``` Cosine.``89.99`` ``` | `float<cosine>` |
| ``` Cosine.``90.01`` ``` | `float<cosine>` |
| ``` Cosine.``90.025`` ``` | `float<cosine>` |
| ``` Cosine.``90.05`` ``` | `float<cosine>` |
| ``` Cosine.``90.1`` ``` | `float<cosine>` |
| ``` Cosine.``90.25`` ``` | `float<cosine>` |
| ``` Cosine.``90.5`` ``` | `float<cosine>` |
| ``` Cosine.``91.0`` ``` | `float<cosine>` |
| ``` Cosine.``92.5`` ``` | `float<cosine>` |
| ``` Cosine.``95.0`` ``` | `float<cosine>` |
| ``` Cosine.``180.0`` ``` | `float<cosine>` |

## module Tangent

| Tangent | Signature |
|:--|:--|
| `tangent` | `[<Measure>] type` |
| **Functions and values** | |
| `Tangent.inDegrees` | `t: float<tangent> -> float` |
| ``` Tangent.``0.01`` ``` | `float<tangent>` |
| ``` Tangent.``0.025`` ``` | `float<tangent>` |
| ``` Tangent.``0.05`` ``` | `float<tangent>` |
| ``` Tangent.``0.1`` ``` | `float<tangent>` |
| ``` Tangent.``0.25`` ``` | `float<tangent>` |
| ``` Tangent.``0.5`` ``` | `float<tangent>` |
| ``` Tangent.``1.0`` ``` | `float<tangent>` |
| ``` Tangent.``2.5`` ``` | `float<tangent>` |
| ``` Tangent.``5.0`` ``` | `float<tangent>` |
| ``` Tangent.``10.0`` ``` | `float<tangent>` |
| ``` Tangent.``15.0`` ``` | `float<tangent>` |
| ``` Tangent.``20.0`` ``` | `float<tangent>` |
| ``` Tangent.``30.0`` ``` | `float<tangent>` |
| ``` Tangent.``45.0`` ``` | `float<tangent>` |
| ``` Tangent.``60.0`` ``` | `float<tangent>` |
| ``` Tangent.``70.0`` ``` | `float<tangent>` |
| ``` Tangent.``75.0`` ``` | `float<tangent>` |
| ``` Tangent.``80.0`` ``` | `float<tangent>` |
| ``` Tangent.``85.0`` ``` | `float<tangent>` |
| ``` Tangent.``87.5`` ``` | `float<tangent>` |
| ``` Tangent.``89.0`` ``` | `float<tangent>` |
| ``` Tangent.``89.5`` ``` | `float<tangent>` |
| ``` Tangent.``89.75`` ``` | `float<tangent>` |
| ``` Tangent.``89.9`` ``` | `float<tangent>` |
| ``` Tangent.``89.95`` ``` | `float<tangent>` |
| ``` Tangent.``89.975`` ``` | `float<tangent>` |
| ``` Tangent.``89.99`` ``` | `float<tangent>` |
| ``` Tangent.``90.01`` ``` | `float<tangent>` |
| ``` Tangent.``90.025`` ``` | `float<tangent>` |
| ``` Tangent.``90.05`` ``` | `float<tangent>` |
| ``` Tangent.``90.1`` ``` | `float<tangent>` |
| ``` Tangent.``90.25`` ``` | `float<tangent>` |
| ``` Tangent.``90.5`` ``` | `float<tangent>` |
| ``` Tangent.``91.0`` ``` | `float<tangent>` |
| ``` Tangent.``92.5`` ``` | `float<tangent>` |
| ``` Tangent.``95.0`` ``` | `float<tangent>` |
| ``` Tangent.``100.0`` ``` | `float<tangent>` |
| ``` Tangent.``105.0`` ``` | `float<tangent>` |
| ``` Tangent.``110.0`` ``` | `float<tangent>` |
| ``` Tangent.``120.0`` ``` | `float<tangent>` |
| ``` Tangent.``135.0`` ``` | `float<tangent>` |
| ``` Tangent.``150.0`` ``` | `float<tangent>` |
| ``` Tangent.``160.0`` ``` | `float<tangent>` |
| ``` Tangent.``165.0`` ``` | `float<tangent>` |
| ``` Tangent.``170.0`` ``` | `float<tangent>` |
| ``` Tangent.``175.0`` ``` | `float<tangent>` |
| ``` Tangent.``177.5`` ``` | `float<tangent>` |
| ``` Tangent.``179.0`` ``` | `float<tangent>` |
| ``` Tangent.``179.5`` ``` | `float<tangent>` |
| ``` Tangent.``179.75`` ``` | `float<tangent>` |
| ``` Tangent.``179.9`` ``` | `float<tangent>` |
| ``` Tangent.``179.95`` ``` | `float<tangent>` |
| ``` Tangent.``179.975`` ``` | `float<tangent>` |
| ``` Tangent.``179.99`` ``` | `float<tangent>` |

## module Format

| Format | Signature |
|:--|:--|
| `Format.addThousandSeparators` | `thousandSeparator: char -> number: string -> string` |
| `Format.float` | `x: float -> string` |
| `Format.floatAsFSharpCode` | `x: float -> string` |
| `Format.floatWithSeparator` | `thousandSeparator: char -> x: float -> string` |
| `Format.globalThousandSeparator` | `char` |
| `Format.nl` | `string` |
| `Format.userZeroTolerance` | `float` |

## module Format.ToMaxDigits

| ToMaxDigits | Signature |
|:--|:--|
| `ToMaxDigits.d0` | `x: float -> string` |
| `ToMaxDigits.d1` | `x: float -> string` |
| `ToMaxDigits.d10` | `x: float -> string` |
| `ToMaxDigits.d2` | `x: float -> string` |
| `ToMaxDigits.d3` | `x: float -> string` |
| `ToMaxDigits.d4` | `x: float -> string` |
| `ToMaxDigits.d5` | `x: float -> string` |
| `ToMaxDigits.d6` | `x: float -> string` |
| `ToMaxDigits.d7` | `x: float -> string` |
| `ToMaxDigits.d8` | `x: float -> string` |
| `ToMaxDigits.d9` | `x: float -> string` |
| `ToMaxDigits.trimZeros` | `n: string -> string` |

## module EuclidCollectionUtilities.Arr

| Arr | Signature |
|:--|:--|
| `Arr.maxIndex` | `xs: 'T array -> int` |
| `Arr.minIndex` | `xs: 'T array -> int` |
| `Arr.undefCreateFloat64` | `count: int -> float array` |

## module EuclidCollectionUtilities.ResizeArr

| ResizeArr | Signature |
|:--|:--|
| `ResizeArr.clear` | `arr: ResizeArray<'T> -> unit` |
| `ResizeArr.clone` | `resizeArray: ResizeArray<'T> -> ResizeArray<'T>` |
| `ResizeArr.closeLoop` | `pts: ResizeArray<'T> -> ResizeArray<'T>` |
| `ResizeArr.find` | `predicate: ('T -> bool) -> xs: ResizeArray<'T> -> 'T` |
| `ResizeArr.findIndex` | `predicate: ('T -> bool) -> xs: ResizeArray<'T> -> int` |
| `ResizeArr.findLast` | `predicate: ('T -> bool) -> xs: ResizeArray<'T> -> 'T` |
| `ResizeArr.findLastIndex` | `predicate: ('T -> bool) -> xs: ResizeArray<'T> -> int` |
| `ResizeArr.getIdx` | `idx: int -> resizeArray: ResizeArray<'T> -> 'T` |
| `ResizeArr.init` | `count: int -> f: (int -> 'T) -> ResizeArray<'T>` |
| `ResizeArr.iPrevThisNext` | `xs: ResizeArray<'T> -> (int * 'T * 'T * 'T) seq` |
| `ResizeArr.lastIdx` | `resizeArray: ResizeArray<'T> -> int` |
| `ResizeArr.len` | `resizeArray: ResizeArray<'T> -> int` |
| `ResizeArr.map` | `mapping: ('T -> 'U) -> resizeArray: ResizeArray<'T> -> ResizeArray<'U>` |
| `ResizeArr.maxIndexBy` | `projection: ('T -> 'Key) -> xs: ResizeArray<'T> -> int` |
| `ResizeArr.minIndexBy` | `projection: ('T -> 'Key) -> xs: ResizeArray<'T> -> int` |
| `ResizeArr.pop` | `arr: ResizeArray<'T> -> 'T` |
| `ResizeArr.popOff` | `arr: ResizeArray<'T> -> unit` |
| `ResizeArr.rev` | `xs: ResizeArray<'T> -> ResizeArray<'T>` |
| `ResizeArr.setIdx` | `idx: int -> value: 'T -> resizeArray: ResizeArray<'T> -> unit` |
| `ResizeArr.singleton` | `x: 'T -> ResizeArray<'T>` |
| `ResizeArr.sortBy` | `projection: ('T -> 'Key) -> xs: ResizeArray<'T> -> ResizeArray<'T>` |
| `ResizeArr.thisNext` | `rarr: ResizeArray<'T> -> ('T * 'T) seq` |
| `ResizeArr.tryFindBack` | `predicate: ('T -> bool) -> resizeArray: ResizeArray<'T> -> 'T option` |
| `ResizeArr.tryFindIndex` | `predicate: ('T -> bool) -> xs: ResizeArray<'T> -> int option` |

## ResizeArray extensions

| ResizeArray | Signature |
|:--|:--|
| `.First` | `'T with get, set` |
| `.GetLooped` | `i: int -> 'T` |
| `.IsEmpty` | `bool` |
| `.Last` | `'T with get, set` |
| `.LastIndex` | `int` |
| `.Pop` | `unit -> 'T` |
| `.PopAt` | `index: int -> 'T` |
| `.Second` | `'T with get, set` |
| `.SecondLast` | `'T with get, set` |
| `.SetIdx` | `i: int -> v: 'T -> unit` |
| `.Third` | `'T with get, set` |
| `.ThirdLast` | `'T with get, set` |
