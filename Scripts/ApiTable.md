# Euclid API

All public types, members and functions of Euclid. Corresponding 2D and 3D types share one table.

- Members of the same name share a row. An empty cell means that the member only exists in one dimension.
- Instance members and fields start with `.`, e.g. `.DistanceTo`.
- Static members are qualified with their type name. They are listed right below the instance member of the same name.
  The static members without such an instance member follow at the end of each table.
- A property shows only its type. `with get, set` marks a property that can be set. `?name` marks an optional parameter.

Generated from the sources by `Scripts/ApiTable.fsx`. Do not edit by hand.

## Contents

**2D and 3D**

- [Vc / Vec](#vc--vec)
- [UnitVc / UnitVec](#unitvc--unitvec)
- [Pt / Pnt](#pt--pnt)
- [Rotation2D / Quaternion](#rotation2d--quaternion)
- [PPlane](#pplane)
- [Matrix](#matrix)
- [RigidMatrix](#rigidmatrix)
- [Line2D / Line3D](#line2d--line3d)
- [XLine2D.XRayParam / XLine3D.XRayParam](#xline2dxrayparam--xline3dxrayparam)
- [XLine2D.XRay / XLine3D.XRay](#xline2dxray--xline3dxray)
- [XLine2D.XParam / XLine3D.XParam](#xline2dxparam--xline3dxparam)
- [XLine2D.XPt / XLine3D.XPnt](#xline2dxpt--xline3dxpnt)
- [XLine2D.ClParams / XLine3D.ClParams](#xline2dclparams--xline3dclparams)
- [XLine2D.ClPts / XLine3D.ClPnts](#xline2dclpts--xline3dclpnts)
- [XLine2D.XEnds / XLine3D.XEnds](#xline2dxends--xline3dxends)
- [module XLineXY / module XLineXYZ](#module-xlinexy--module-xlinexyz)
- [XLine2D / XLine3D](#xline2d--xline3d)
- [XLine3D.XCone](#xline3dxcone)
- [NPlane](#nplane)
- [Rect2D / Rect3D](#rect2d--rect3d)
- [Box](#box)
- [FreeBox](#freebox)
- [BRect / BBox](#brect--bbox)
- [Tria2D / Tria3D](#tria2d--tria3d)
- [Points2D / Points3D](#points2d--points3d)
- [module Offset2D / module Offset3D](#module-offset2d--module-offset3d)
- [Offset2D.UTurn](#offset2duturn)
- [Offset2D.VarDistParallel / Offset3D.VarDistParallel](#offset2dvardistparallel--offset3dvardistparallel)
- [Offset3D.OffsetDirection](#offset3doffsetdirection)
- [Offset3D.CollinearPoint](#offset3dcollinearpoint)
- [Polyline2D / Polyline3D](#polyline2d--polyline3d)
- [Topology2D / Topology3D](#topology2d--topology3d)
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
|  |  | `.Z` | `float` |
|  |  | `.AngleDiamondInXYTo` | `b: Vec -> float` |
| `.AngleDiamondTo` | `b: Vc -> float` |  |  |
| `.AsFSharpCode` | `string` | `.AsFSharpCode` | `string` |
| `.AsPt` | `Pt` | `.AsPnt` | `Pnt` |
| `.AsPnt` | `Pnt` | `.AsPt` | `Pt` |
| `Vc.asPt` | `v: Vc -> Pt` | `Vec.asPnt` | `v: Vec -> Pnt` |
| `Vc.asPnt` | `v: Vc -> Pnt` | `Vec.asPt` | `v: Vec -> Pt` |
| `.AsString` | `string` | `.AsString` | `string` |
|  |  | `.AsVc` | `Vc` |
|  |  | `Vec.asVc` | `v: Vec -> Vc` |
| `.AsVec` | `Vec` |  |  |
| `Vc.asVec` | `v: Vc -> Vec` |  |  |
| `.Cross` | `b: Vc -> float` | `.Cross` | `b: UnitVec -> Vec` |
| `.Cross` | `b: UnitVc -> float` | `.Cross` | `b: Vec -> Vec` |
| `Vc.cross` | `a: Vc * b: Vc -> float` | `Vec.cross` | `a: Vec * b: Vec -> Vec` |
| `Vc.cross` | `a: UnitVc * b: Vc -> float` | `Vec.cross` | `a: Vec * b: UnitVec -> Vec` |
| `Vc.cross` | `a: Vc * b: UnitVc -> float` | `Vec.cross` | `a: UnitVec * b: Vec -> Vec` |
| `.Direction180` | `float` |  |  |
| `Vc.direction180` | `v: Vc -> float` |  |  |
|  |  | `.Direction180InXY` | `float` |
|  |  | `Vec.direction180InXY` | `v: Vec -> float` |
| `.Direction2Pi` | `float` |  |  |
| `Vc.direction2Pi` | `v: Vc -> float` |  |  |
|  |  | `.Direction2PiInXY` | `float` |
|  |  | `Vec.direction2PiInXY` | `v: Vec -> float` |
| `.Direction360` | `float` |  |  |
| `Vc.direction360` | `v: Vc -> float` |  |  |
|  |  | `.Direction360InXY` | `float` |
|  |  | `Vec.direction360InXY` | `v: Vec -> float` |
| `.DirectionDiamond` | `float` |  |  |
| `Vc.directionDiamond` | `a: Vc -> float` |  |  |
|  |  | `.DirectionDiamondInXY` | `float` |
|  |  | `Vec.directionDiamondInXY` | `v: Vec -> float` |
| `.DirectionPi` | `float` |  |  |
| `Vc.directionPi` | `v: Vc -> float` |  |  |
|  |  | `.DirectionPiInXY` | `float` |
|  |  | `Vec.directionPiInXY` | `v: Vec -> float` |
| `.Dot` | `b: Vc -> float` | `.Dot` | `b: Vec -> float` |
| `.Dot` | `b: UnitVc -> float` | `.Dot` | `b: UnitVec -> float` |
| `Vc.dot` | `a: Vc * b: Vc -> float` | `Vec.dot` | `a: Vec * b: Vec -> float` |
| `Vc.dot` | `a: Vc * b: UnitVc -> float` | `Vec.dot` | `a: Vec * b: UnitVec -> float` |
| `Vc.dot` | `a: UnitVc * b: Vc -> float` | `Vec.dot` | `a: UnitVec * b: Vec -> float` |
| `.Half` | `Vc` | `.Half` | `Vec` |
| `Vc.half` | `v: Vc -> Vc` | `Vec.half` | `v: Vec -> Vec` |
|  |  | `.IsHorizontal` | `bool` |
|  |  | `Vec.isHorizontal` | `v: Vec -> bool` |
| `.IsNotZero` | `bool` | `.IsNotZero` | `bool` |
| `Vc.isNotZero` | `v: Vc -> bool` | `Vec.isNotZero` | `v: Vec -> bool` |
| `.IsOppositeOrientation` | `other: Vc -> bool` | `.IsOppositeOrientation` | `other: Vec -> bool` |
| `.IsOppositeOrientation` | `other: UnitVc -> bool` | `.IsOppositeOrientation` | `other: UnitVec -> bool` |
| `Vc.isOppositeOrientation` | `v: Vc -> other: Vc -> bool` | `Vec.isOppositeOrientation` | `v: Vec -> other: Vec -> bool` |
| `.IsParallelAndOrientedTo` | `other: Vc * minCosine: float<cosine> -> bool` | `.IsParallelAndOrientedTo` | `other: Vec * minCosine: float<cosine> -> bool` |
| `.IsParallelAndOrientedTo` | `other: UnitVc * minCosine: float<cosine> -> bool` | `.IsParallelAndOrientedTo` | `other: UnitVec * minCosine: float<cosine> -> bool` |
| `Vc.isParallelAndOrientedTo` | `minCosine: float<cosine> -> a: Vc -> b: Vc -> bool` | `Vec.isParallelAndOrientedTo` | `minCosine: float<cosine> -> a: Vec -> b: Vec -> bool` |
| `.IsParallelTo` | `other: Vc * minCosine: float<cosine> -> bool` | `.IsParallelTo` | `other: Vec * minCosine: float<cosine> -> bool` |
| `.IsParallelTo` | `other: UnitVc * minCosine: float<cosine> -> bool` | `.IsParallelTo` | `other: UnitVec * minCosine: float<cosine> -> bool` |
| `Vc.isParallelTo` | `minCosine: float<cosine> -> a: Vc -> b: Vc -> bool` | `Vec.isParallelTo` | `minCosine: float<cosine> -> a: Vec -> b: Vec -> bool` |
| `.IsPerpendicularTo` | `other: Vc * maxCosine: float<cosine> -> bool` | `.IsPerpendicularTo` | `other: Vec * maxCosine: float<cosine> -> bool` |
| `.IsPerpendicularTo` | `other: UnitVc * maxCosine: float<cosine> -> bool` | `.IsPerpendicularTo` | `other: UnitVec * maxCosine: float<cosine> -> bool` |
| `Vc.isPerpendicularTo` | `maxCosine: float<cosine> -> a: Vc -> b: Vc -> bool` | `Vec.isPerpendicularTo` | `maxCosine: float<cosine> -> a: Vec -> b: Vec -> bool` |
| `.IsTiny` | `tol: float -> bool` | `.IsTiny` | `tol: float -> bool` |
| `Vc.isTiny` | `tol: float -> v: Vc -> bool` | `Vec.isTiny` | `tol: float -> v: Vec -> bool` |
| `.IsTinySq` | `tol: float -> bool` | `.IsTinySq` | `tol: float -> bool` |
| `Vc.isTinySq` | `tol: float -> v: Vc -> bool` | `Vec.isTinySq` | `tol: float -> v: Vec -> bool` |
| `.IsUnit` | `bool` | `.IsUnit` | `bool` |
| `Vc.isUnit` | `v: Vc -> bool` | `Vec.isUnit` | `v: Vec -> bool` |
|  |  | `.IsVertical` | `bool` |
|  |  | `Vec.isVertical` | `v: Vec -> bool` |
| `.IsXAligned` | `bool` | `.IsXAligned` | `bool` |
| `Vc.isXAligned` | `v: Vc -> bool` | `Vec.isXAligned` | `v: Vec -> bool` |
| `.IsYAligned` | `bool` | `.IsYAligned` | `bool` |
| `Vc.isYAligned` | `v: Vc -> bool` | `Vec.isYAligned` | `v: Vec -> bool` |
|  |  | `.IsZAligned` | `bool` |
|  |  | `Vec.isZAligned` | `v: Vec -> bool` |
| `.IsZero` | `bool` | `.IsZero` | `bool` |
| `Vc.isZero` | `v: Vc -> bool` | `Vec.isZero` | `v: Vec -> bool` |
| `.Length` | `float` | `.Length` | `float` |
| `Vc.length` | `v: Vc -> float` | `Vec.length` | `v: Vec -> float` |
|  |  | `.LengthInXY` | `float` |
|  |  | `Vec.lengthInXY` | `v: Vec -> float` |
| `.LengthSq` | `float` | `.LengthSq` | `float` |
| `Vc.lengthSq` | `v: Vc -> float` | `Vec.lengthSq` | `v: Vec -> float` |
|  |  | `.LengthSqInXY` | `float` |
|  |  | `Vec.lengthSqInXY` | `v: Vec -> float` |
| `.MatchesOrientation` | `other: Vc -> bool` | `.MatchesOrientation` | `other: Vec -> bool` |
| `.MatchesOrientation` | `other: UnitVc -> bool` | `.MatchesOrientation` | `other: UnitVec -> bool` |
| `Vc.matchesOrientation` | `v: Vc -> other: Vc -> bool` | `Vec.matchesOrientation` | `v: Vec -> other: Vec -> bool` |
|  |  | `.PerpendicularInXY` | `Vec` |
|  |  | `Vec.perpendicularInXY` | `v: Vec -> Vec` |
| `.Rotate` | `r: Rotation2D -> Vc` |  |  |
| `Vc.rotate` | `r: Rotation2D -> v: Vc -> Vc` | `Vec.rotate` | `q: Quaternion -> vec: Vec -> Vec` |
| `.Rotate90CCW` | `Vc` |  |  |
| `Vc.rotate90CCW` | `v: Vc -> Vc` |  |  |
| `.Rotate90CW` | `Vc` |  |  |
| `Vc.rotate90CW` | `v: Vc -> Vc` |  |  |
| `.RotateByQuarterCircle` | `numberOfQuarters: int -> Vc` | `.RotateByQuarterCircle` | `numberOfQuarters: int -> Vec` |
| `Vc.rotateByQuarterCircle` | `numberOfQuarters: int -> v: Vc -> Vc` | `Vec.rotateByQuarterCircle` | `numberOfQuarters: int -> v: Vec -> Vec` |
| `.RotateDegrees` | `angDegree: float -> Vc` |  |  |
| `Vc.rotateDegrees` | `angDegree: float -> v: Vc -> Vc` |  |  |
|  |  | `.RotateOnZ90CCW` | `Vec` |
|  |  | `Vec.rotateOnZ90CCW` | `v: Vec -> Vec` |
|  |  | `.RotateOnZ90CW` | `Vec` |
|  |  | `Vec.rotateOnZ90CW` | `v: Vec -> Vec` |
| `.RotateRadians` | `angRadians: float -> Vc` |  |  |
| `Vc.rotateRadians` | `angRadians: float -> v: Vc -> Vc` |  |  |
| `.ToString` | `unit -> string` | `.ToString` | `unit -> string` |
|  |  | `.TransformRigid` | `m: RigidMatrix -> Vec` |
|  |  | `Vec.transformRigid` | `m: RigidMatrix -> v: Vec -> Vec` |
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
|  |  | `Vec.addZ` | `z: float -> v: Vec -> Vec` |
| `Vc.angle180` | `a: Vc -> b: Vc -> float` | `Vec.angle180` | `a: Vec -> b: Vec -> float` |
| `Vc.angle2Pi` | `a: Vc * b: Vc -> float` |  |  |
|  |  | `Vec.angle2PiInXY` | `a: Vec * b: Vec -> float` |
| `Vc.angle360` | `a: Vc * b: Vc -> float` |  |  |
|  |  | `Vec.angle360InXY` | `a: Vec * b: Vec -> float` |
| `Vc.angle90` | `a: Vc -> b: Vc -> float` | `Vec.angle90` | `a: Vec -> b: Vec -> float` |
| `Vc.angleDiamond` | `a: Vc * b: Vc -> float` |  |  |
|  |  | `Vec.angleDiamondInXY` | `a: Vec * b: Vec -> float` |
| `Vc.angleHalfPi` | `a: Vc -> b: Vc -> float` | `Vec.angleHalfPi` | `a: Vec -> b: Vec -> float` |
| `Vc.anglePi` | `a: Vc -> b: Vc -> float` | `Vec.anglePi` | `a: Vec -> b: Vec -> float` |
| `Vc.angleToX180` | `v: Vc -> float` |  |  |
| `Vc.angleToXPi` | `vc: Vc -> float` |  |  |
| `Vc.bisector` | `a: Vc -> b: Vc -> Vc` | `Vec.bisector` | `a: Vec -> b: Vec -> Vec` |
| `Vc.create` | `x: float * y: float -> Vc` | `Vec.create` | `x: float * y: float * z: float -> Vec` |
| `Vc.create` | `start: Pt * endValue: Pt -> Vc` | `Vec.create` | `start: Pnt * endValue: Pnt -> Vec` |
| `Vc.createFromMembersXY` | `vec: ^T -> Vc` |  |  |
| `Vc.createFromMembersxy` | `vec: ^T -> Vc` |  |  |
|  |  | `Vec.createFromMembersXYZ` | `vec: ^T -> Vec` |
|  |  | `Vec.createFromMembersxyz` | `vec: ^T -> Vec` |
| `Vc.createFromPnt` | `pnt: Pnt -> Vc` | `Vec.createFromPnt` | `pnt: Pnt -> Vec` |
| `Vc.createFromUnitVc` | `v: UnitVc -> Vc` |  |  |
|  |  | `Vec.createFromUnitVec` | `v: UnitVec -> Vec` |
|  |  | `Vec.createFromVcWithZ` | `z: float -> v: Vc -> Vec` |
|  |  | `Vec.determinant3` | `u: Vec * v: Vec * w: Vec -> float` |
| `Vc.difference` | `a: Vc -> b: Vc -> float` | `Vec.difference` | `a: Vec -> b: Vec -> float` |
| `Vc.differenceSq` | `a: Vc -> b: Vc -> float` | `Vec.differenceSq` | `a: Vec -> b: Vec -> float` |
| `Vc.DivideByInt` | `v: Vc * i: int -> Vc` | `Vec.DivideByInt` | `v: Vec * i: int -> Vec` |
| `Vc.equalsExact` | `a: Vc -> b: Vc -> bool` | `Vec.equalsExact` | `a: Vec -> b: Vec -> bool` |
| `Vc.equalsWithin` | `tol: float -> a: Vc -> b: Vc -> bool` | `Vec.equalsWithin` | `tol: float -> a: Vec -> b: Vec -> bool` |
| `Vc.flip` | `v: Vc -> Vc` | `Vec.flip` | `v: Vec -> Vec` |
|  |  | `Vec.flipToPointUp` | `v: Vec -> Vec` |
| `Vc.getX` | `v: Vc -> float` | `Vec.getX` | `v: Vec -> float` |
| `Vc.getY` | `v: Vc -> float` | `Vec.getY` | `v: Vec -> float` |
|  |  | `Vec.getZ` | `v: Vec -> float` |
| `Vc.isAngleAbove` | `cosineValue: float<cosine> -> a: Vc -> b: Vc -> bool` | `Vec.isAngleAbove` | `cosineValue: float<cosine> -> a: Vec -> b: Vec -> bool` |
| `Vc.isAngleBelow` | `cosineValue: float<cosine> -> a: Vc -> b: Vc -> bool` | `Vec.isAngleBelow` | `cosineValue: float<cosine> -> a: Vec -> b: Vec -> bool` |
| `Vc.isNotParallelWithin` | `cosineValue: float<cosine> -> a: Vc -> b: Vc -> bool` | `Vec.isNotParallelWithin` | `cosineValue: float<cosine> -> a: Vec -> b: Vec -> bool` |
|  |  | `Vec.isParallelAndOrientedToUnitVec` | `minCosine: float<cosine> -> b: UnitVec -> a: Vec -> bool` |
|  |  | `Vec.isParallelToUnitVec` | `minCosine: float<cosine> -> b: UnitVec -> a: Vec -> bool` |
| `Vc.isParallelWithin` | `cosineValue: float<cosine> -> a: Vc -> b: Vc -> bool` | `Vec.isParallelWithin` | `cosineValue: float<cosine> -> a: Vec -> b: Vec -> bool` |
|  |  | `Vec.isPerpendicularToUnitVec` | `maxCosine: float<cosine> -> b: UnitVec -> a: Vec -> bool` |
| `Vc.lerp` | `start: Vc * endValue: Vc * rel: float -> Vc` | `Vec.lerp` | `start: Vec * endValue: Vec * rel: float -> Vec` |
| `Vc.matchOrientation` | `orientationToMatch: Vc -> vecToFlip: Vc -> Vc` | `Vec.matchOrientation` | `orientationToMatch: Vec -> vecToFlip: Vec -> Vec` |
| `Vc.matchUnitVcOrientation` | `orientationToMatch: UnitVc -> vecToFlip: Vc -> Vc` |  |  |
|  |  | `Vec.matchUnitVecOrientation` | `orientationToMatch: UnitVec -> vecToFlip: Vec -> Vec` |
| `Vc.notEqualsWithin` | `tol: float -> a: Vc -> b: Vc -> bool` | `Vec.notEqualsWithin` | `tol: float -> a: Vec -> b: Vec -> bool` |
|  |  | `Vec.orientDown` | `v: Vec -> Vec` |
|  |  | `Vec.orientUp` | `v: Vec -> Vec` |
|  |  | `Vec.perpendicularInVerticalPlane` | `v: Vec -> Vec` |
|  |  | `Vec.projectToXYPlane` | `v: Vec -> Vec` |
| `Vc.reverse` | `v: Vc -> Vc` | `Vec.reverse` | `v: Vec -> Vec` |
|  |  | `Vec.rotateOnX` | `r: Rotation2D -> v: Vec -> Vec` |
|  |  | `Vec.rotateOnXDeg` | `angDegree: float -> v: Vec -> Vec` |
|  |  | `Vec.rotateOnY` | `r: Rotation2D -> v: Vec -> Vec` |
|  |  | `Vec.rotateOnYDeg` | `angDegree: float -> v: Vec -> Vec` |
|  |  | `Vec.rotateOnZ` | `r: Rotation2D -> v: Vec -> Vec` |
|  |  | `Vec.rotateOnZDeg` | `angDegree: float -> v: Vec -> Vec` |
| `Vc.scale` | `f: float -> v: Vc -> Vc` | `Vec.scale` | `f: float -> v: Vec -> Vec` |
| `Vc.slerp` | `start: Vc * endValue: Vc * rel: float -> Vc` | `Vec.slerp` | `start: Vec * endValue: Vec * rel: float -> Vec` |
| `Vc.slopeDegrees` | `v: Vc -> float` | `Vec.slopeDegrees` | `v: Vec -> float` |
| `Vc.slopePercent` | `v: Vc -> float` | `Vec.slopePercent` | `v: Vec -> float` |
| `Vc.slopeRadians` | `v: Vc -> float` | `Vec.slopeRadians` | `v: Vec -> float` |
| `Vc.tryUnitize` | `v: Vc -> UnitVc voption` | `Vec.tryUnitize` | `v: Vec -> UnitVec voption` |
| `Vc.unitize` | `v: Vc -> UnitVc` | `Vec.unitize` | `v: Vec -> UnitVec` |
| `Vc.unitizeAsVc` | `v: Vc -> Vc` |  |  |
|  |  | `Vec.unitizeAsVec` | `v: Vec -> Vec` |
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
|  |  | `.Z` | `float` |
|  |  | `.AngleDiamondInXYTo` | `b: UnitVec -> float` |
| `.AngleDiamondTo` | `b: UnitVc -> float` |  |  |
| `.AsFSharpCode` | `string` | `.AsFSharpCode` | `string` |
| `.AsPt` | `Pt` | `.AsPnt` | `Pnt` |
| `.AsPnt` | `Pnt` | `.AsPt` | `Pt` |
| `UnitVc.asPt` | `v: UnitVc -> Pt` | `UnitVec.asPnt` | `v: UnitVec -> Pnt` |
| `UnitVc.asPnt` | `v: UnitVc -> Pnt` | `UnitVec.asPt` | `v: UnitVec -> Pt` |
| `.AsString` | `string` | `.AsString` | `string` |
|  |  | `.AsUnitVc` | `UnitVc` |
|  |  | `UnitVec.asUnitVc` | `v: UnitVec -> UnitVc` |
| `.AsUnitVec` | `UnitVec` |  |  |
| `UnitVc.asUnitVec` | `v: UnitVc -> UnitVec` |  |  |
| `.AsVc` | `Vc` | `.AsVc` | `Vc` |
| `UnitVc.asVc` | `v: UnitVc -> Vc` | `UnitVec.asVc` | `v: UnitVec -> Vc` |
| `.AsVec` | `Vec` | `.AsVec` | `Vec` |
| `UnitVc.asVec` | `v: UnitVc -> Vec` | `UnitVec.asVec` | `v: UnitVec -> Vec` |
| `.Cross` | `b: Vc -> float` | `.Cross` | `b: UnitVec -> Vec` |
| `.Cross` | `b: UnitVc -> float` | `.Cross` | `b: Vec -> Vec` |
| `UnitVc.cross` | `a: UnitVc * b: UnitVc -> float` | `UnitVec.cross` | `a: UnitVec * b: UnitVec -> Vec` |
| `UnitVc.cross` | `a: UnitVc * b: Vc -> float` | `UnitVec.cross` | `a: UnitVec * b: Vec -> Vec` |
| `UnitVc.cross` | `a: Vc * b: UnitVc -> float` | `UnitVec.cross` | `a: Vec * b: UnitVec -> Vec` |
| `.Direction180` | `float` |  |  |
| `UnitVc.direction180` | `v: UnitVc -> float` |  |  |
|  |  | `.Direction180InXY` | `float` |
|  |  | `UnitVec.direction180InXY` | `v: UnitVec -> float` |
| `.Direction2Pi` | `float` |  |  |
| `UnitVc.direction2Pi` | `v: UnitVc -> float` |  |  |
|  |  | `.Direction2PiInXY` | `float` |
|  |  | `UnitVec.direction2PiInXY` | `v: UnitVec -> float` |
| `.Direction360` | `float` |  |  |
| `UnitVc.direction360` | `v: UnitVc -> float` |  |  |
|  |  | `.Direction360InXY` | `float` |
|  |  | `UnitVec.direction360InXY` | `v: UnitVec -> float` |
| `.DirectionDiamond` | `float` |  |  |
| `UnitVc.directionDiamond` | `a: UnitVc -> float` |  |  |
|  |  | `.DirectionDiamondInXY` | `float` |
|  |  | `UnitVec.directionDiamondInXY` | `v: UnitVec -> float` |
| `.DirectionPi` | `float` |  |  |
| `UnitVc.directionPi` | `v: UnitVc -> float` |  |  |
|  |  | `.DirectionPiInXY` | `float` |
|  |  | `UnitVec.directionPiInXY` | `v: UnitVec -> float` |
| `.Dot` | `b: UnitVc -> float` | `.Dot` | `b: UnitVec -> float` |
| `.Dot` | `b: Vc -> float` | `.Dot` | `b: Vec -> float` |
| `UnitVc.dot` | `a: UnitVc * b: UnitVc -> float` | `UnitVec.dot` | `a: UnitVec * b: UnitVec -> float` |
| `UnitVc.dot` | `a: UnitVc * b: Vc -> float` | `UnitVec.dot` | `a: UnitVec * b: Vec -> float` |
| `UnitVc.dot` | `a: Vc * b: UnitVc -> float` | `UnitVec.dot` | `a: Vec * b: UnitVec -> float` |
| `.DotCosine` | `b: UnitVc -> float<cosine>` | `.DotCosine` | `b: UnitVec -> float<cosine>` |
| `UnitVc.dotCosine` | `a: UnitVc -> b: UnitVc -> float<cosine>` | `UnitVec.dotCosine` | `a: UnitVec -> b: UnitVec -> float<cosine>` |
|  |  | `.IsHorizontal` | `bool` |
|  |  | `UnitVec.isHorizontal` | `v: UnitVec -> bool` |
| `.IsOppositeOrientation` | `other: UnitVc -> bool` | `.IsOppositeOrientation` | `other: UnitVec -> bool` |
| `.IsOppositeOrientation` | `other: Vc -> bool` | `.IsOppositeOrientation` | `other: Vec -> bool` |
| `UnitVc.isOppositeOrientation` | `other: UnitVc -> v: UnitVc -> bool` | `UnitVec.isOppositeOrientation` | `other: UnitVec -> v: UnitVec -> bool` |
| `.IsParallelAndOrientedTo` | `other: UnitVc * minCosine: float<cosine> -> bool` | `.IsParallelAndOrientedTo` | `other: UnitVec * minCosine: float<cosine> -> bool` |
| `.IsParallelAndOrientedTo` | `other: Vc * minCosine: float<cosine> -> bool` | `.IsParallelAndOrientedTo` | `other: Vec * minCosine: float<cosine> -> bool` |
| `UnitVc.isParallelAndOrientedTo` | `minCosine: float<cosine> -> a: UnitVc -> b: UnitVc -> bool` | `UnitVec.isParallelAndOrientedTo` | `minCosine: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `.IsParallelTo` | `other: UnitVc * minCosine: float<cosine> -> bool` | `.IsParallelTo` | `other: UnitVec * minCosine: float<cosine> -> bool` |
| `.IsParallelTo` | `other: Vc * minCosine: float<cosine> -> bool` | `.IsParallelTo` | `other: Vec * minCosine: float<cosine> -> bool` |
| `UnitVc.isParallelTo` | `minCosine: float<cosine> -> a: UnitVc -> b: UnitVc -> bool` | `UnitVec.isParallelTo` | `minCosine: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `.IsPerpendicularTo` | `other: UnitVc * maxCosine: float<cosine> -> bool` | `.IsPerpendicularTo` | `other: UnitVec * maxCosine: float<cosine> -> bool` |
| `.IsPerpendicularTo` | `other: Vc * maxCosine: float<cosine> -> bool` | `.IsPerpendicularTo` | `other: Vec * maxCosine: float<cosine> -> bool` |
| `UnitVc.isPerpendicularTo` | `maxCosine: float<cosine> -> a: UnitVc -> b: UnitVc -> bool` | `UnitVec.isPerpendicularTo` | `maxCosine: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
|  |  | `.IsVertical` | `bool` |
|  |  | `UnitVec.isVertical` | `v: UnitVec -> bool` |
| `.IsXAligned` | `bool` | `.IsXAligned` | `bool` |
| `UnitVc.isXAligned` | `v: UnitVc -> bool` | `UnitVec.isXAligned` | `v: UnitVec -> bool` |
| `.IsYAligned` | `bool` | `.IsYAligned` | `bool` |
| `UnitVc.isYAligned` | `v: UnitVc -> bool` | `UnitVec.isYAligned` | `v: UnitVec -> bool` |
|  |  | `.IsZAligned` | `bool` |
|  |  | `UnitVec.isZAligned` | `v: UnitVec -> bool` |
|  |  | `.LengthInXY` | `float` |
|  |  | `UnitVec.lengthInXY` | `v: UnitVec -> float` |
|  |  | `.LengthSqInXY` | `float` |
|  |  | `UnitVec.lengthSqInXY` | `v: UnitVec -> float` |
| `.MatchesOrientation` | `other: UnitVc -> bool` | `.MatchesOrientation` | `other: UnitVec -> bool` |
| `.MatchesOrientation` | `other: Vc -> bool` | `.MatchesOrientation` | `other: Vec -> bool` |
| `UnitVc.matchesOrientation` | `other: UnitVc -> v: UnitVc -> bool` | `UnitVec.matchesOrientation` | `other: UnitVec -> v: UnitVec -> bool` |
|  |  | `.PerpendicularInXY` | `Vec` |
|  |  | `UnitVec.perpendicularInXY` | `v: UnitVec -> Vec` |
| `.Rotate` | `r: Rotation2D -> UnitVc` |  |  |
| `UnitVc.rotate` | `r: Rotation2D -> v: UnitVc -> UnitVc` | `UnitVec.rotate` | `q: Quaternion -> uVec: UnitVec -> UnitVec` |
| `.Rotate90CCW` | `UnitVc` |  |  |
| `UnitVc.rotate90CCW` | `v: UnitVc -> UnitVc` |  |  |
| `.Rotate90CW` | `UnitVc` |  |  |
| `UnitVc.rotate90CW` | `v: UnitVc -> UnitVc` |  |  |
| `.RotateByQuarterCircle` | `numberOfQuarters: int -> UnitVc` | `.RotateByQuarterCircle` | `numberOfQuarters: int -> UnitVec` |
| `UnitVc.rotateByQuarterCircle` | `numberOfQuarters: int -> v: UnitVc -> UnitVc` | `UnitVec.rotateByQuarterCircle` | `numberOfQuarters: int -> v: UnitVec -> UnitVec` |
| `.RotateDegrees` | `angDegree: float -> UnitVc` |  |  |
| `UnitVc.rotateDegrees` | `angDegree: float -> v: UnitVc -> UnitVc` |  |  |
|  |  | `.RotateOnZ90CCW` | `UnitVec` |
|  |  | `UnitVec.rotateOnZ90CCW` | `v: UnitVec -> UnitVec` |
|  |  | `.RotateOnZ90CW` | `UnitVec` |
|  |  | `UnitVec.rotateOnZ90CW` | `v: UnitVec -> UnitVec` |
| `.RotateRadians` | `angRadians: float -> UnitVc` |  |  |
| `UnitVc.rotateRadians` | `angRadians: float -> v: UnitVc -> UnitVc` |  |  |
| `.ToString` | `unit -> string` | `.ToString` | `unit -> string` |
|  |  | `.TransformRigid` | `m: RigidMatrix -> UnitVec` |
|  |  | `UnitVec.transformRigid` | `m: RigidMatrix -> v: UnitVec -> UnitVec` |
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
|  |  | `UnitVec.addZ` | `z: float -> v: UnitVec -> Vec` |
| `UnitVc.angle180` | `a: UnitVc -> b: UnitVc -> float` | `UnitVec.angle180` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVc.angle2Pi` | `a: UnitVc * b: UnitVc -> float` |  |  |
|  |  | `UnitVec.angle2PiInXY` | `a: UnitVec * b: UnitVec -> float` |
| `UnitVc.angle360` | `a: UnitVc * b: UnitVc -> float` |  |  |
|  |  | `UnitVec.angle360InXY` | `a: UnitVec * b: UnitVec -> float` |
| `UnitVc.angle90` | `a: UnitVc -> b: UnitVc -> float` | `UnitVec.angle90` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVc.angleDiamond` | `a: UnitVc * b: UnitVc -> float` |  |  |
|  |  | `UnitVec.angleDiamondInXY` | `a: UnitVec * b: UnitVec -> float` |
| `UnitVc.angleHalfPi` | `a: UnitVc -> b: UnitVc -> float` | `UnitVec.angleHalfPi` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVc.anglePi` | `a: UnitVc -> b: UnitVc -> float` | `UnitVec.anglePi` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVc.angleToX180` | `v: UnitVc -> float` |  |  |
| `UnitVc.angleToXPi` | `v: UnitVc -> float` |  |  |
| `UnitVc.create` | `x: float * y: float -> UnitVc` | `UnitVec.create` | `x: float * y: float * z: float -> UnitVec` |
| `UnitVc.create` | `fromPt: Pt * toPt: Pt -> UnitVc` | `UnitVec.create` | `fromPnt: Pnt * toPnt: Pnt -> UnitVec` |
| `UnitVc.createFor` | `msg: string * x: float * y: float -> UnitVc` | `UnitVec.createFor` | `msg: string * x: float * y: float * z: float -> UnitVec` |
| `UnitVc.createFromMembersXY` | `vec: ^T -> UnitVc` |  |  |
| `UnitVc.createFromMembersxy` | `vec: ^T -> UnitVc` |  |  |
|  |  | `UnitVec.createFromMembersXYZ` | `vec: ^T -> UnitVec` |
|  |  | `UnitVec.createFromMembersxyz` | `vec: ^T -> UnitVec` |
| `UnitVc.createFromPt` | `pt: Pt -> UnitVc` | `UnitVec.createFromPnt` | `pnt: Pnt -> UnitVec` |
| `UnitVc.createFromVc` | `v: Vc -> UnitVc` |  |  |
|  |  | `UnitVec.createFromVec` | `v: Vec -> UnitVec` |
| `UnitVc.createUnchecked` | `x: float * y: float -> UnitVc` | `UnitVec.createUnchecked` | `x: float * y: float * z: float -> UnitVec` |
|  |  | `UnitVec.determinant3` | `u: UnitVec * v: UnitVec * w: UnitVec -> float` |
| `UnitVc.difference` | `a: UnitVc -> b: UnitVc -> float` | `UnitVec.difference` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVc.differenceSq` | `a: UnitVc -> b: UnitVc -> float` | `UnitVec.differenceSq` | `a: UnitVec -> b: UnitVec -> float` |
| `UnitVc.equalsExact` | `a: UnitVc -> b: UnitVc -> bool` | `UnitVec.equalsExact` | `a: UnitVec -> b: UnitVec -> bool` |
| `UnitVc.equalsWithin` | `tol: float -> a: UnitVc -> b: UnitVc -> bool` | `UnitVec.equalsWithin` | `tol: float -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVc.flip` | `v: UnitVc -> UnitVc` | `UnitVec.flip` | `v: UnitVec -> UnitVec` |
|  |  | `UnitVec.flipToPointUp` | `v: UnitVec -> UnitVec` |
| `UnitVc.getX` | `v: UnitVc -> float` | `UnitVec.getX` | `v: UnitVec -> float` |
| `UnitVc.getY` | `v: UnitVc -> float` | `UnitVec.getY` | `v: UnitVec -> float` |
|  |  | `UnitVec.getZ` | `v: UnitVec -> float` |
| `UnitVc.isAngleAbove` | `cosineValue: float<cosine> -> a: UnitVc -> b: UnitVc -> bool` | `UnitVec.isAngleAbove` | `cosineValue: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVc.isAngleBelow` | `cosineValue: float<cosine> -> a: UnitVc -> b: UnitVc -> bool` | `UnitVec.isAngleBelow` | `cosineValue: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVc.isNotParallelWithin` | `cosineValue: float<cosine> -> a: UnitVc -> b: UnitVc -> bool` | `UnitVec.isNotParallelWithin` | `cosineValue: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVc.isParallelWithin` | `cosineValue: float<cosine> -> a: UnitVc -> b: UnitVc -> bool` | `UnitVec.isParallelWithin` | `cosineValue: float<cosine> -> a: UnitVec -> b: UnitVec -> bool` |
| `UnitVc.lerp` | `start: UnitVc * endValue: UnitVc * rel: float -> Vc` | `UnitVec.lerp` | `start: UnitVec * endValue: UnitVec * rel: float -> Vec` |
| `UnitVc.matchOrientation` | `orientationToMatch: UnitVc -> vecToFlip: UnitVc -> UnitVc` | `UnitVec.matchOrientation` | `orientationToMatch: UnitVec -> vecToFlip: UnitVec -> UnitVec` |
| `UnitVc.matchVcOrientation` | `orientationToMatch: Vc -> vecToFlip: UnitVc -> UnitVc` |  |  |
|  |  | `UnitVec.matchVecOrientation` | `orientationToMatch: Vec -> vecToFlip: UnitVec -> UnitVec` |
| `UnitVc.notEqualsWithin` | `tol: float -> a: UnitVc -> b: UnitVc -> bool` | `UnitVec.notEqualsWithin` | `tol: float -> a: UnitVec -> b: UnitVec -> bool` |
|  |  | `UnitVec.orientDown` | `v: UnitVec -> UnitVec` |
|  |  | `UnitVec.orientUp` | `v: UnitVec -> UnitVec` |
|  |  | `UnitVec.perpendicularInVerticalPlane` | `v: UnitVec -> Vec` |
|  |  | `UnitVec.projectToXYPlane` | `v: UnitVec -> Vec` |
| `UnitVc.reverse` | `v: UnitVc -> UnitVc` | `UnitVec.reverse` | `v: UnitVec -> UnitVec` |
|  |  | `UnitVec.rotateOnX` | `r: Rotation2D -> v: UnitVec -> UnitVec` |
|  |  | `UnitVec.rotateOnXDeg` | `angDegree: float -> v: UnitVec -> UnitVec` |
|  |  | `UnitVec.rotateOnY` | `r: Rotation2D -> v: UnitVec -> UnitVec` |
|  |  | `UnitVec.rotateOnYDeg` | `angDegree: float -> v: UnitVec -> UnitVec` |
|  |  | `UnitVec.rotateOnZ` | `r: Rotation2D -> v: UnitVec -> UnitVec` |
|  |  | `UnitVec.rotateOnZDeg` | `angDegree: float -> v: UnitVec -> UnitVec` |
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
|  |  | `.Z` | `float` |
|  |  | `.Angle2PiInXYTo` | `o: Pnt -> float` |
|  |  | `Pnt.angle2PiInXYTo` | `fromPnt: Pnt * toPnt: Pnt -> float` |
| `.Angle2PiTo` | `o: Pt -> float` |  |  |
| `Pt.angle2PiTo` | `fromPt: Pt * toPt: Pt -> float` |  |  |
|  |  | `.Angle360InXYTo` | `o: Pnt -> float` |
|  |  | `Pnt.angle360InXYTo` | `fromPnt: Pnt * toPnt: Pnt -> float` |
| `.Angle360To` | `o: Pt -> float` |  |  |
| `Pt.angle360To` | `fromPt: Pt * toPt: Pt -> float` |  |  |
| `.AsFSharpCode` | `string` | `.AsFSharpCode` | `string` |
| `.AsPnt` | `Pnt` | `.AsPt` | `Pt` |
| `Pt.asPnt` | `pt: Pt -> Pnt` | `Pnt.asPt` | `pnt: Pnt -> Pt` |
| `.AsString` | `string` | `.AsString` | `string` |
| `.AsVc` | `Vc` | `.AsVc` | `Vc` |
| `Pt.asVc` | `pt: Pt -> Vc` | `Pnt.asVc` | `pnt: Pnt -> Vc` |
| `.AsVec` | `Vec` | `.AsVec` | `Vec` |
| `Pt.asVec` | `pt: Pt -> Vec` | `Pnt.asVec` | `pnt: Pnt -> Vec` |
| `.ClosestPointOnLine` | `ln: Line2D -> Pt` | `.ClosestPointOnLine` | `ln: Line3D -> Pnt` |
| `Pt.closestPointOnLine` | `ln: Line2D -> testPt: Pt -> Pt` | `Pnt.closestPointOnLine` | `ln: Line3D -> testPnt: Pnt -> Pnt` |
|  |  | `.DirectionDiamondInXYTo` | `o: Pnt -> float` |
|  |  | `Pnt.directionDiamondInXYTo` | `fromPnt: Pnt * toPnt: Pnt -> float` |
| `.DirectionDiamondTo` | `o: Pt -> float` |  |  |
| `Pt.directionDiamondTo` | `o: Pt -> p: Pt -> float` |  |  |
| `.DistanceFromOrigin` | `float` | `.DistanceFromOrigin` | `float` |
| `Pt.distanceFromOrigin` | `pt: Pt -> float` | `Pnt.distanceFromOrigin` | `pnt: Pnt -> float` |
|  |  | `.DistanceInXYFromOrigin` | `float` |
|  |  | `Pnt.distanceInXYFromOrigin` | `pnt: Pnt -> float` |
| `.DistanceTo` | `b: Pt -> float` | `.DistanceTo` | `b: Pnt -> float` |
| `Pt.distanceTo` | `b: Pt -> p: Pt -> float` | `Pnt.distanceTo` | `b: Pnt -> p: Pnt -> float` |
| `.DistanceToLine` | `ln: Line2D -> float` | `.DistanceToLine` | `ln: Line3D -> float` |
| `Pt.distanceToLine` | `ln: Line2D -> testPt: Pt -> float` | `Pnt.distanceToLine` | `ln: Line3D -> testPnt: Pnt -> float` |
| `.DistanceToXY` | `x: float * y: float -> float` |  |  |
| `Pt.distanceToXY` | `x: float * y: float -> p: Pt -> float` |  |  |
|  |  | `.DistanceToXYZ` | `x: float * y: float * z: float -> float` |
|  |  | `Pnt.distanceToXYZ` | `x: float * y: float * z: float -> p: Pnt -> float` |
| `.IsAlmostOrigin` | `tol: float -> bool` | `.IsAlmostOrigin` | `tol: float -> bool` |
| `Pt.isAlmostOrigin` | `tol: float -> pt: Pt -> bool` | `Pnt.isAlmostOrigin` | `tol: float -> pnt: Pnt -> bool` |
| `.IsInvalid` | `bool` | `.IsInvalid` | `bool` |
| `Pt.isInvalid` | `pt: Pt -> bool` | `Pnt.isInvalid` | `pnt: Pnt -> bool` |
| `.IsNotOrigin` | `bool` | `.IsNotOrigin` | `bool` |
| `Pt.isNotOrigin` | `pt: Pt -> bool` | `Pnt.isNotOrigin` | `pnt: Pnt -> bool` |
| `.IsOrigin` | `bool` | `.IsOrigin` | `bool` |
| `Pt.isOrigin` | `pt: Pt -> bool` | `Pnt.isOrigin` | `pnt: Pnt -> bool` |
| `.IsValid` | `bool` | `.IsValid` | `bool` |
| `Pt.isValid` | `pt: Pt -> bool` | `Pnt.isValid` | `pnt: Pnt -> bool` |
| `.Rotate` | `r: Rotation2D -> Pt` |  |  |
| `Pt.rotate` | `r: Rotation2D -> p: Pt -> Pt` | `Pnt.rotate` | `q: Quaternion -> pnt: Pnt -> Pnt` |
| `.Rotate90CCW` | `Pt` |  |  |
| `Pt.rotate90CCW` | `pt: Pt -> Pt` |  |  |
| `.Rotate90CW` | `Pt` |  |  |
| `Pt.rotate90CW` | `pt: Pt -> Pt` |  |  |
| `.RotateAround` | `cen: Pt -> r: Rotation2D -> Pt` |  |  |
| `Pt.rotateAround` | `cen: Pt -> r: Rotation2D -> pt: Pt -> Pt` | `Pnt.rotateAround` | `cen: Pnt -> q: Quaternion -> pnt: Pnt -> Pnt` |
| `.RotateAroundDegrees` | `cen: Pt -> angDegree: float -> Pt` |  |  |
| `Pt.rotateAroundDegrees` | `cen: Pt -> angDegree: float -> pt: Pt -> Pt` |  |  |
| `.RotateAroundRadians` | `cen: Pt -> angRadians: float -> Pt` |  |  |
| `Pt.rotateAroundRadians` | `cen: Pt -> angRadians: float -> pt: Pt -> Pt` |  |  |
| `.RotateByQuarterCircle` | `numberOfQuarters: int -> Pt` |  |  |
| `Pt.rotateByQuarterCircle` | `numberOfQuarters: int -> v: Pt -> Pt` |  |  |
| `.RotateDegrees` | `angDegree: float -> Pt` |  |  |
| `Pt.rotateDegrees` | `angDegree: float -> pt: Pt -> Pt` |  |  |
| `.RotateRadians` | `angRadians: float -> Pt` |  |  |
| `Pt.rotateRadians` | `angRadians: float -> pt: Pt -> Pt` |  |  |
| `.SqDistanceFromOrigin` | `float` | `.SqDistanceFromOrigin` | `float` |
| `Pt.sqDistanceFromOrigin` | `pt: Pt -> float` | `Pnt.sqDistanceFromOrigin` | `pnt: Pnt -> float` |
|  |  | `.SqDistanceInXYFromOrigin` | `float` |
|  |  | `Pnt.sqDistanceInXYFromOrigin` | `pnt: Pnt -> float` |
| `.SqDistanceTo` | `b: Pt -> float` | `.SqDistanceTo` | `b: Pnt -> float` |
| `Pt.sqDistanceTo` | `b: Pt -> p: Pt -> float` | `Pnt.sqDistanceTo` | `b: Pnt -> p: Pnt -> float` |
| `.SqDistanceToLine` | `ln: Line2D -> float` | `.SqDistanceToLine` | `ln: Line3D -> float` |
| `Pt.sqDistanceToLine` | `ln: Line2D -> testPt: Pt -> float` | `Pnt.sqDistanceToLine` | `ln: Line3D -> testPnt: Pnt -> float` |
| `.SqDistanceToXY` | `x: float * y: float -> float` |  |  |
| `Pt.sqDistanceToXY` | `x: float * y: float -> p: Pt -> float` |  |  |
|  |  | `.SqDistanceToXYZ` | `x: float * y: float * z: float -> float` |
|  |  | `Pnt.sqDistanceToXYZ` | `x: float * y: float * z: float -> p: Pnt -> float` |
| `.ToString` | `unit -> string` | `.ToString` | `unit -> string` |
|  |  | `.Transform` | `m: Matrix -> Pnt` |
|  |  | `Pnt.transform` | `m: Matrix -> p: Pnt -> Pnt` |
|  |  | `.TransformRigid` | `m: RigidMatrix -> Pnt` |
|  |  | `Pnt.transformRigid` | `m: RigidMatrix -> p: Pnt -> Pnt` |
|  |  | `.TransformRigidRotateOnly` | `m: RigidMatrix -> Pnt` |
|  |  | `Pnt.transformRigidRotateOnly` | `m: RigidMatrix -> p: Pnt -> Pnt` |
| `.WithDistanceFromOrigin` | `l: float -> Pt` | `.WithDistanceFromOrigin` | `l: float -> Pnt` |
| `Pt.withDistanceFromOrigin` | `l: float -> pt: Pt -> Pt` | `Pnt.withDistanceFromOrigin` | `l: float -> pnt: Pnt -> Pnt` |
| `.WithX` | `x: float -> Pt` | `.WithX` | `x: float -> Pnt` |
| `Pt.withX` | `x: float -> pt: Pt -> Pt` | `Pnt.withX` | `x: float -> pnt: Pnt -> Pnt` |
| `.WithY` | `y: float -> Pt` | `.WithY` | `y: float -> Pnt` |
| `Pt.withY` | `y: float -> pt: Pt -> Pt` | `Pnt.withY` | `y: float -> pnt: Pnt -> Pnt` |
| `.WithZ` | `z: float -> Pnt` | `.WithZ` | `z: float -> Pnt` |
| `Pt.withZ` | `z: float -> pt: Pt -> Pnt` | `Pnt.withZ` | `z: float -> pnt: Pnt -> Pnt` |
| **Other static members** | | | |
| `Pt.add` | `a: Pt -> b: Pt -> Pt` | `Pnt.add` | `a: Pnt -> b: Pnt -> Pnt` |
| `Pt.addVc` | `v: Vc -> a: Pt -> Pt` |  |  |
|  |  | `Pnt.addVec` | `v: Vec -> a: Pnt -> Pnt` |
| `Pt.angle180Pts` | `ptPrev: Pt * ptThis: Pt * ptNext: Pt -> float` | `Pnt.angle180Pnts` | `pntPrev: Pnt * pntThis: Pnt * pntNext: Pnt -> float` |
| `Pt.angleInCorner` | `prevPt: Pt * thisPt: Pt * nextPt: Pt -> float` | `Pnt.angleInCorner` | `prevPnt: Pnt * thisPnt: Pnt * nextPnt: Pnt -> float` |
| `Pt.anglePiPts` | `ptPrev: Pt * ptThis: Pt * ptNext: Pt -> float` | `Pnt.anglePiPnts` | `pntPrev: Pnt * pntThis: Pnt * pntNext: Pnt -> float` |
| `Pt.bisector` | `ptPrev: Pt * ptThis: Pt * ptNext: Pt -> Vc` | `Pnt.bisector` | `pntPrev: Pnt * pntThis: Pnt * pntNext: Pnt -> Vec` |
| `Pt.closestOfTwo` | `pt1: Pt -> pt2: Pt -> referencePoint: Pt -> Pt` | `Pnt.closestOfTwo` | `pnt1: Pnt -> pnt2: Pnt -> referencePoint: Pnt -> Pnt` |
| `Pt.createFromMembersXY` | `pt: ^T -> Pt` |  |  |
| `Pt.createFromMembersxy` | `pt: ^T -> Pt` |  |  |
|  |  | `Pnt.createFromMembersXYZ` | `pt: ^T -> Pnt` |
|  |  | `Pnt.createFromMembersxyz` | `pt: ^T -> Pnt` |
| `Pt.createFromPnt` | `p: Pnt -> Pt` | `Pnt.createFromPt` | `p: Pt -> Pnt` |
|  |  | `Pnt.createFromPtWithZ` | `z: float -> p: Pt -> Pnt` |
| `Pt.createFromUnitVc` | `v: UnitVc -> Pt` |  |  |
|  |  | `Pnt.createFromUnitVec` | `v: UnitVec -> Pnt` |
| `Pt.createFromVc` | `v: Vc -> Pt` |  |  |
|  |  | `Pnt.createFromVec` | `v: Vec -> Pnt` |
| `Pt.dist` | `b: Pt -> p: Pt -> float` | `Pnt.dist` | `b: Pnt -> p: Pnt -> float` |
|  |  | `Pnt.distanceInXY` | `a: Pnt -> b: Pnt -> float` |
| `Pt.distPt` | `fromPt: Pt * dirPt: Pt * distance: float -> Pt` | `Pnt.distPnt` | `fromPnt: Pnt * dirPnt: Pnt * distance: float -> Pnt` |
| `Pt.DivideByInt` | `pt: Pt * i: int -> Pt` | `Pnt.DivideByInt` | `pnt: Pnt * i: int -> Pnt` |
| `Pt.divPt` | `fromPt: Pt * toPt: Pt * rel: float -> Pt` | `Pnt.divPnt` | `fromPnt: Pnt * toPnt: Pnt * rel: float -> Pnt` |
| `Pt.equalsExact` | `a: Pt -> b: Pt -> bool` | `Pnt.equalsExact` | `a: Pnt -> b: Pnt -> bool` |
| `Pt.equalsWithin` | `tol: float -> a: Pt -> b: Pt -> bool` | `Pnt.equalsWithin` | `tol: float -> a: Pnt -> b: Pnt -> bool` |
|  |  | `Pnt.extendToZLevel` | `fromPnt: Pnt * toPnt: Pnt * z: float -> Pnt` |
| `Pt.getX` | `pt: Pt -> float` | `Pnt.getX` | `pnt: Pnt -> float` |
| `Pt.getY` | `pt: Pt -> float` | `Pnt.getY` | `pnt: Pnt -> float` |
|  |  | `Pnt.getZ` | `pnt: Pnt -> float` |
| `Pt.lerp` | `fromPt: Pt * toPt: Pt * rel: float -> Pt` | `Pnt.lerp` | `fromPnt: Pnt * toPnt: Pnt * rel: float -> Pnt` |
| `Pt.midPt` | `a: Pt -> b: Pt -> Pt` | `Pnt.midPnt` | `a: Pnt -> b: Pnt -> Pnt` |
| `Pt.move` | `shift: Vc -> pt: Pt -> Pt` | `Pnt.move` | `shift: Vec -> pnt: Pnt -> Pnt` |
| `Pt.moveX` | `x: float -> pt: Pt -> Pt` | `Pnt.moveX` | `x: float -> pnt: Pnt -> Pnt` |
| `Pt.moveY` | `y: float -> pt: Pt -> Pt` | `Pnt.moveY` | `y: float -> pnt: Pnt -> Pnt` |
|  |  | `Pnt.moveZ` | `z: float -> pnt: Pnt -> Pnt` |
|  |  | `Pnt.normalOf3Pnts` | `a: Pnt * b: Pnt * c: Pnt -> Vec` |
|  |  | `Pnt.normalOfTwoPointsInXY` | `fromPnt: Pnt * toPnt: Pnt -> Vec` |
| `Pt.notEqualsWithin` | `tol: float -> a: Pt -> b: Pt -> bool` | `Pnt.notEqualsWithin` | `tol: float -> a: Pnt -> b: Pnt -> bool` |
| `Pt.Origin` | `Pt` | `Pnt.Origin` | `Pnt` |
| `Pt.projectedParameter` | `fromPt: Pt * uv: UnitVc * testPt: Pt -> float` | `Pnt.projectedParameter` | `fromPnt: Pnt * uv: UnitVec * testPnt: Pnt -> float` |
| `Pt.projectedParameter` | `fromPt: Pt * v: Vc * testPt: Pt -> float` | `Pnt.projectedParameter` | `fromPnt: Pnt * v: Vec * testPnt: Pnt -> float` |
| `Pt.projectedParameter` | `fromPt: Pt * toPt: Pt * testPt: Pt -> float` | `Pnt.projectedParameter` | `fromPnt: Pnt * toPnt: Pnt * testPnt: Pnt -> float` |
|  |  | `Pnt.projectToXYPlane` | `pnt: Pnt -> Pnt` |
|  |  | `Pnt.rotateOnX` | `r: Rotation2D -> p: Pnt -> Pnt` |
|  |  | `Pnt.rotateOnXDeg` | `angDegree: float -> pnt: Pnt -> Pnt` |
|  |  | `Pnt.rotateOnXWithCenter` | `cen: Pnt -> r: Rotation2D -> pnt: Pnt -> Pnt` |
|  |  | `Pnt.rotateOnXWithCenterDeg` | `cen: Pnt -> angDegree: float -> pnt: Pnt -> Pnt` |
|  |  | `Pnt.rotateOnY` | `r: Rotation2D -> p: Pnt -> Pnt` |
|  |  | `Pnt.rotateOnYDeg` | `angDegree: float -> pnt: Pnt -> Pnt` |
|  |  | `Pnt.rotateOnYWithCenter` | `cen: Pnt -> r: Rotation2D -> pnt: Pnt -> Pnt` |
|  |  | `Pnt.rotateOnYWithCenterDeg` | `cen: Pnt -> angDegree: float -> pnt: Pnt -> Pnt` |
|  |  | `Pnt.rotateOnZ` | `r: Rotation2D -> p: Pnt -> Pnt` |
|  |  | `Pnt.rotateOnZDeg` | `angDegree: float -> pnt: Pnt -> Pnt` |
|  |  | `Pnt.rotateOnZWithCenter` | `cen: Pnt -> r: Rotation2D -> pnt: Pnt -> Pnt` |
|  |  | `Pnt.rotateOnZWithCenterDeg` | `cen: Pnt -> angDegree: float -> pnt: Pnt -> Pnt` |
| `Pt.scale` | `f: float -> pt: Pt -> Pt` | `Pnt.scale` | `f: float -> pnt: Pnt -> Pnt` |
| `Pt.scaleOn` | `cen: Pt -> factor: float -> pt: Pt -> Pt` | `Pnt.scaleOn` | `cen: Pnt -> factor: float -> pnt: Pnt -> Pnt` |
| `Pt.snap` | `precision: float -> pt: Pt -> Pt` | `Pnt.snap` | `precision: float -> pnt: Pnt -> Pnt` |
| `Pt.snapIfClose` | `snapDistance: float -> snapTo: Pt -> pt: Pt -> Pt` | `Pnt.snapIfClose` | `snapDistance: float -> snapTo: Pnt -> pnt: Pnt -> Pnt` |
| `Pt.sqDist` | `b: Pt -> p: Pt -> float` | `Pnt.sqDist` | `b: Pnt -> p: Pnt -> float` |
| `Pt.translate` | `shift: Vc -> pt: Pt -> Pt` | `Pnt.translate` | `shift: Vec -> pnt: Pnt -> Pnt` |
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
| `.Sin` | `float` |  |  |
| `.Cos` | `float` |  |  |
|  |  | `.X` | `float` |
|  |  | `.Y` | `float` |
|  |  | `.Z` | `float` |
|  |  | `.W` | `float` |
| `.Add` | `ro: Rotation2D -> Rotation2D` |  |  |
| `Rotation2D.add` | `ro: Rotation2D -> r: Rotation2D -> Rotation2D` |  |  |
| `.AddDegrees` | `deg: float -> Rotation2D` |  |  |
| `Rotation2D.addDegrees` | `deg: float -> r: Rotation2D -> Rotation2D` |  |  |
| `.AddRadians` | `rad: float -> Rotation2D` |  |  |
| `Rotation2D.addRadians` | `rad: float -> r: Rotation2D -> Rotation2D` |  |  |
|  |  | `.AngleInDegrees` | `float` |
|  |  | `Quaternion.angleInDegrees` | `q: Quaternion -> float` |
|  |  | `.AngleInRadians` | `float` |
|  |  | `Quaternion.angleInRadians` | `q: Quaternion -> float` |
| `.AsFSharpCode` | `string` | `.AsFSharpCode` | `string` |
| `Rotation2D.asFSharpCode` | `r: Rotation2D -> string` | `Quaternion.asFSharpCode` | `q: Quaternion -> string` |
| `.AsString` | `string` | `.AsString` | `string` |
| `Rotation2D.asString` | `r: Rotation2D -> string` | `Quaternion.asString` | `q: Quaternion -> string` |
|  |  | `.Axis` | `Vec` |
|  |  | `Quaternion.axis` | `q: Quaternion -> Vec` |
|  |  | `.Conjugate` | `Quaternion` |
|  |  | `Quaternion.conjugate` | `q: Quaternion -> Quaternion` |
| `.Half` | `Rotation2D` |  |  |
| `.InDegrees` | `float` |  |  |
| `Rotation2D.inDegrees` | `r: Rotation2D -> float` |  |  |
| `.InRadians` | `float` |  |  |
| `Rotation2D.inRadians` | `r: Rotation2D -> float` |  |  |
| `.Inverse` | `Rotation2D` | `.Inverse` | `Quaternion` |
| `Rotation2D.inverse` | `r: Rotation2D -> Rotation2D` | `Quaternion.inverse` | `q: Quaternion -> Quaternion` |
|  |  | `.Normalize` | `unit -> Quaternion` |
|  |  | `Quaternion.normalize` | `q: Quaternion -> Quaternion` |
|  |  | `.Then` | `other: Quaternion -> Quaternion` |
| `.ToString` | `unit -> string` | `.ToString` | `unit -> string` |
|  |  | `.WithAngleInDegrees` | `angleInDegrees: float -> Quaternion` |
|  |  | `Quaternion.withAngleInDegrees` | `angleInDegrees: float -> q: Quaternion -> Quaternion` |
|  |  | `.WithAngleInRadians` | `angleInRadians: float -> Quaternion` |
|  |  | `Quaternion.withAngleInRadians` | `angleInRadians: float -> q: Quaternion -> Quaternion` |
| **Other static members** | | | |
| `Rotation2D.create` | `sine: float * cosine: float -> Rotation2D` | `Quaternion.create` | `x: float * y: float * z: float * w: float -> Quaternion` |
| `Rotation2D.createFromCosine` | `cos: float -> Rotation2D` |  |  |
| `Rotation2D.createFromDegrees` | `deg: float -> Rotation2D` | `Quaternion.createFromDegrees` | `axis: Vec * angleInDegrees: float -> Quaternion` |
|  |  | `Quaternion.createFromDegrees` | `axis: UnitVec * angleInDegrees: float -> Quaternion` |
|  |  | `Quaternion.createFromEulerXYZ` | `degreesX: float * degreesY: float * degreesZ: float -> Quaternion` |
|  |  | `Quaternion.createFromEulerXZY` | `degreesX: float * degreesZ: float * degreesY: float -> Quaternion` |
|  |  | `Quaternion.createFromEulerYXZ` | `degreesY: float * degreesX: float * degreesZ: float -> Quaternion` |
|  |  | `Quaternion.createFromEulerYZX` | `degreesY: float * degreesZ: float * degreesX: float -> Quaternion` |
|  |  | `Quaternion.createFromEulerZXY` | `degreesZ: float * degreesX: float * degreesY: float -> Quaternion` |
|  |  | `Quaternion.createFromEulerZYX` | `degreesZ: float * degreesY: float * degreesX: float -> Quaternion` |
| `Rotation2D.createFromRadians` | `rad: float -> Rotation2D` | `Quaternion.createFromRadians` | `axis: Vec * angleInRadians: float -> Quaternion` |
|  |  | `Quaternion.createFromRadians` | `axis: UnitVec * angleInRadians: float -> Quaternion` |
|  |  | `Quaternion.createFromRigidMatrix` | `orthoMatrix: RigidMatrix -> Quaternion` |
| `Rotation2D.createFromSine` | `sin: float -> Rotation2D` |  |  |
| `Rotation2D.createFromVectors` | `a: UnitVc * b: UnitVc -> Rotation2D` |  |  |
| `Rotation2D.createFromVectors` | `a: Vc * b: Vc -> Rotation2D` |  |  |
| `Rotation2D.createUnchecked` | `sine: float * cosine: float -> Rotation2D` | `Quaternion.createUnchecked` | `x: float * y: float * z: float * w: float -> Quaternion` |
|  |  | `Quaternion.createVecToVec` | `vecFrom: UnitVec * vecTo: UnitVec -> Quaternion` |
|  |  | `Quaternion.createVecToVec` | `vecFrom: Vec * vecTo: Vec -> Quaternion` |
| `Rotation2D.equalsExact` | `a: Rotation2D -> b: Rotation2D -> bool` | `Quaternion.equalsExact` | `a: Quaternion -> b: Quaternion -> bool` |
|  |  | `Quaternion.equalsRotationWithin` | `tolerance: float -> a: Quaternion -> b: Quaternion -> bool` |
| `Rotation2D.equalsWithin` | `tol: float -> a: Rotation2D -> b: Rotation2D -> bool` | `Quaternion.equalsWithin` | `tolerance: float -> a: Quaternion -> b: Quaternion -> bool` |
|  |  | `Quaternion.identity` | `Quaternion` |
|  |  | `Quaternion.multiply` | `l: Quaternion * r: Quaternion -> Quaternion` |
|  |  | `Quaternion.toEulerAnglesZYX` | `q: Quaternion -> float * float * float` |
|  |  | `Quaternion.tryCreateFromMatrix` | `matrix: Matrix -> Quaternion voption` |
|  |  | `( *** )` | `l: Quaternion * r: Quaternion -> Quaternion` |
|  |  | `( *** )` | `v: Vec * q: Quaternion -> Vec` |
|  |  | `( *** )` | `v: UnitVec * q: Quaternion -> UnitVec` |
|  |  | `( *** )` | `p: Pnt * q: Quaternion -> Pnt` |

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
| `PPlane.evaluateAt` | `px: float * py: float -> pl: PPlane -> Pnt` |
| `.EvaluateAtXYZ` | `px: float * py: float * pz: float -> Pnt` |
| `PPlane.evaluateAtXYZ` | `px: float * py: float * pz: float -> pl: PPlane -> Pnt` |
| `.IsCoincidentTo` | `other: PPlane * distanceTolerance: float * minCosine: float<cosine> -> bool` |
| `PPlane.isCoincidentTo` | `distanceTolerance: float -> minCosine: float<cosine> -> a: PPlane -> b: PPlane -> bool` |
| `.Origin` | `Pnt` |
| `.PlaneAtClPnt` | `pnt: Pnt -> PPlane` |
| `PPlane.planeAtClPnt` | `pnt: Pnt -> pl: PPlane -> PPlane` |
| `.PointParameters` | `pnt: Pnt -> float * float * float` |
| `PPlane.pointParameters` | `pnt: Pnt -> pl: PPlane -> float * float * float` |
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
| `PPlane.createThreePoints` | `origin: Pnt -> xPnt: Pnt -> yPnt: Pnt -> PPlane` |
| `PPlane.createUnchecked` | `originX: float * originY: float * originZ: float * xAxisX: float * xAxisY: float * xAxisZ: float * yAxisX: float * yAxisY: float * yAxisZ: float * zAxisX: float * zAxisY: float * zAxisZ: float -> PPlane` |
| `PPlane.createUncheckedVec` | `origin: Pnt * axisX: UnitVec * axisY: UnitVec * axisZ: UnitVec -> PPlane` |
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
| `PPlane.translateLocalX` | `d: float -> pl: PPlane -> PPlane` |
| `PPlane.translateLocalY` | `d: float -> pl: PPlane -> PPlane` |
| `PPlane.translateLocalZ` | `d: float -> pl: PPlane -> PPlane` |
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

## Line2D / Line3D

| Line2D | Signature | Line3D | Signature |
|:--|:--|:--|:--|
| `new` | `fromPt: Pt * toPt: Pt -> Line2D` | `new` | `fromPnt: Pnt * toPnt: Pnt -> Line3D` |
| `new` | `fromX: float * fromY: float * toX: float * toY: float -> Line2D` | `new` | `fromX: float * fromY: float * fromZ: float * toX: float * toY: float * toZ: float -> Line3D` |
| `.FromX` | `float` | `.FromX` | `float` |
| `.FromY` | `float` | `.FromY` | `float` |
| `.ToX` | `float` | `.ToX` | `float` |
| `.ToY` | `float` | `.ToY` | `float` |
|  |  | `.FromZ` | `float` |
|  |  | `.ToZ` | `float` |
| `.AsFSharpCode` | `string` | `.AsFSharpCode` | `string` |
| `Line2D.asFSharpCode` | `l: Line2D -> string` | `Line3D.asFSharpCode` | `ln: Line3D -> string` |
| `.AsString` | `string` | `.AsString` | `string` |
| `Line2D.asString` | `l: Line2D -> string` | `Line3D.asString` | `ln: Line3D -> string` |
| `.ClosestParameter` | `p: Pt -> float` | `.ClosestParameter` | `p: Pnt -> float` |
| `Line2D.closestParameter` | `p: Pt -> ln: Line2D -> float` | `Line3D.closestParameter` | `p: Pnt -> ln: Line3D -> float` |
| `.ClosestParameters` | `lnB: Line2D -> float * float` | `.ClosestParameters` | `lnB: Line3D -> float * float` |
| `Line2D.closestParameters` | `lnA: Line2D -> lnB: Line2D -> float * float` | `Line3D.closestParameters` | `lnA: Line3D -> lnB: Line3D -> float * float` |
| `.ClosestParameterXY` | `x: float * y: float -> float` |  |  |
| `Line2D.closestParameterXY` | `x: float -> y: float -> ln: Line2D -> float` |  |  |
|  |  | `.ClosestParameterXYZ` | `x: float * y: float * z: float -> float` |
|  |  | `Line3D.closestParameterXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
| `.ClosestPoint` | `p: Pt -> Pt` | `.ClosestPoint` | `p: Pnt -> Pnt` |
| `Line2D.closestPoint` | `p: Pt -> ln: Line2D -> Pt` | `Line3D.closestPoint` | `p: Pnt -> ln: Line3D -> Pnt` |
| `.ClosestPoints` | `lnB: Line2D -> Pt * Pt` | `.ClosestPoints` | `lnB: Line3D -> Pnt * Pnt` |
| `Line2D.closestPoints` | `lnA: Line2D -> lnB: Line2D -> Pt * Pt` | `Line3D.closestPoints` | `lnA: Line3D -> lnB: Line3D -> Pnt * Pnt` |
| `.Direction` | `Vc` | `.Direction` | `Vec` |
| `Line2D.direction` | `ln: Line2D -> Vc` | `Line3D.direction` | `ln: Line3D -> Vec` |
| `.DistanceRayPoint` | `p: Pt -> float` | `.DistanceRayPoint` | `p: Pnt -> float` |
| `Line2D.distanceRayPoint` | `p: Pt -> ln: Line2D -> float` | `Line3D.distanceRayPoint` | `p: Pnt -> ln: Line3D -> float` |
| `.DistanceRayXY` | `x: float * y: float -> float` |  |  |
| `Line2D.distanceRayXY` | `x: float -> y: float -> ln: Line2D -> float` |  |  |
|  |  | `.DistanceRayXYZ` | `x: float * y: float * z: float -> float` |
|  |  | `Line3D.distanceRayXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
| `.DistanceToPt` | `p: Pt -> float` | `.DistanceToPnt` | `p: Pnt -> float` |
| `Line2D.distanceToPt` | `p: Pt -> ln: Line2D -> float` | `Line3D.distanceToPnt` | `p: Pnt -> ln: Line3D -> float` |
| `.DistanceToXY` | `x: float * y: float -> float` |  |  |
| `Line2D.distanceToXY` | `x: float -> y: float -> ln: Line2D -> float` |  |  |
|  |  | `.DistanceToXYZ` | `x: float * y: float * z: float -> float` |
|  |  | `Line3D.distanceToXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
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
| `.IsCoincidentAndOpposingToFast` | `other: Line2D * parallelogramAreaTolerance: float * maxDotProduct: float -> bool` | `.IsCoincidentAndOpposingToFast` | `other: Line3D * squaredParallelogramAreaTolerance: float * maxDotProduct: float -> bool` |
| `Line2D.isCoincidentAndOpposingToFast` | `parallelogramAreaTolerance: float -> maxDotProduct: float -> lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isCoincidentAndOpposingToFast` | `squaredParallelogramAreaTolerance: float -> maxDotProduct: float -> lnA: Line3D -> lnB: Line3D -> bool` |
| `.IsCoincidentAndOrientedToFast` | `other: Line2D * parallelogramAreaTolerance: float * minDotProduct: float -> bool` | `.IsCoincidentAndOrientedToFast` | `other: Line3D * squaredParallelogramAreaTolerance: float * minDotProduct: float -> bool` |
| `Line2D.isCoincidentAndOrientedToFast` | `parallelogramAreaTolerance: float -> minDotProduct: float -> lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isCoincidentAndOrientedToFast` | `squaredParallelogramAreaTolerance: float -> minDotProduct: float -> other: Line3D -> ln: Line3D -> bool` |
| `.IsCoincidentTo` | `other: Line2D * distanceTolerance: float * minTangent: float<tangent> -> bool` | `.IsCoincidentTo` | `other: Line3D * distanceTolerance: float * minTangent: float<tangent> -> bool` |
| `Line2D.isCoincidentTo` | `distanceTolerance: float -> minTangent: float<tangent> -> a: Line2D -> b: Line2D -> bool` | `Line3D.isCoincidentTo` | `distanceTolerance: float -> minTangent: float<tangent> -> a: Line3D -> b: Line3D -> bool` |
| `.IsCoincidentToFast` | `other: Line2D * parallelogramAreaTolerance: float -> bool` | `.IsCoincidentToFast` | `other: Line3D * squaredParallelogramAreaTolerance: float -> bool` |
| `Line2D.isCoincidentToFast` | `parallelogramAreaTolerance: float -> lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isCoincidentToFast` | `squaredParallelogramAreaTolerance: float -> other: Line3D -> ln: Line3D -> bool` |
|  |  | `.IsHorizontal` | `bool` |
|  |  | `Line3D.isHorizontal` | `l: Line3D -> bool` |
| `.IsNormalTo` | `other: Line2D * maxTangent: float<tangent> -> bool` | `.IsNormalTo` | `other: Line3D * maxTangent: float<tangent> -> bool` |
| `.IsNormalTo` | `other: Vc * maxTangent: float<tangent> -> bool` | `.IsNormalTo` | `other: Vec * maxTangent: float<tangent> -> bool` |
| `.IsNormalTo` | `other: UnitVc * maxTangent: float<tangent> -> bool` | `.IsNormalTo` | `other: UnitVec * maxTangent: float<tangent> -> bool` |
| `Line2D.isNormalTo` | `maxTangent: float<tangent> -> lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isNormalTo` | `maxTangent: float<tangent> -> lnA: Line3D -> lnB: Line3D -> bool` |
| `.IsParallelAndOpposingToFast` | `other: Line2D * parallelogramAreaTolerance: float * maxDotProduct: float -> bool` | `.IsParallelAndOpposingToFast` | `other: Line3D * squaredParallelogramAreaTolerance: float * maxDotProduct: float -> bool` |
| `Line2D.isParallelAndOpposingToFast` | `parallelogramAreaTolerance: float -> maxDotProduct: float -> lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isParallelAndOpposingToFast` | `squaredParallelogramAreaTolerance: float -> maxDotProduct: float -> other: Line3D -> ln: Line3D -> bool` |
| `.IsParallelAndOrientedTo` | `other: Line2D * minTangent: float<tangent> -> bool` | `.IsParallelAndOrientedTo` | `other: Line3D * minTangent: float<tangent> -> bool` |
| `.IsParallelAndOrientedTo` | `other: Vc * minTangent: float<tangent> -> bool` | `.IsParallelAndOrientedTo` | `other: Vec * minTangent: float<tangent> -> bool` |
| `.IsParallelAndOrientedTo` | `other: UnitVc * minTangent: float<tangent> -> bool` | `.IsParallelAndOrientedTo` | `other: UnitVec * minTangent: float<tangent> -> bool` |
| `Line2D.isParallelAndOrientedTo` | `minTangent: float<tangent> -> lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isParallelAndOrientedTo` | `minTangent: float<tangent> -> lnA: Line3D -> lnB: Line3D -> bool` |
| `.IsParallelAndOrientedToFast` | `other: Line2D * parallelogramAreaTolerance: float * minDotProduct: float -> bool` | `.IsParallelAndOrientedToFast` | `other: Line3D * squaredParallelogramAreaTolerance: float * minDotProduct: float -> bool` |
| `Line2D.isParallelAndOrientedToFast` | `parallelogramAreaTolerance: float -> minDotProduct: float -> lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isParallelAndOrientedToFast` | `squaredParallelogramAreaTolerance: float -> minDotProduct: float -> other: Line3D -> ln: Line3D -> bool` |
| `.IsParallelTo` | `other: Line2D * minTangent: float<tangent> -> bool` | `.IsParallelTo` | `other: Line3D * minTangent: float<tangent> -> bool` |
| `.IsParallelTo` | `other: Vc * minTangent: float<tangent> -> bool` | `.IsParallelTo` | `other: Vec * minTangent: float<tangent> -> bool` |
| `.IsParallelTo` | `other: UnitVc * minTangent: float<tangent> -> bool` | `.IsParallelTo` | `other: UnitVec * minTangent: float<tangent> -> bool` |
| `Line2D.isParallelTo` | `minTangent: float<tangent> -> lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isParallelTo` | `minTangent: float<tangent> -> lnA: Line3D -> lnB: Line3D -> bool` |
| `.IsParallelToFast` | `other: Line2D * parallelogramAreaTolerance: float -> bool` | `.IsParallelToFast` | `other: Line3D * squaredParallelogramAreaTolerance: float -> bool` |
| `Line2D.isParallelToFast` | `parallelogramAreaTolerance: float -> lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isParallelToFast` | `squaredParallelogramAreaTolerance: float -> other: Line3D -> ln: Line3D -> bool` |
| `.IsPerpendicularTo` | `other: Line2D * maxTangent: float<tangent> -> bool` | `.IsPerpendicularTo` | `other: Line3D * maxTangent: float<tangent> -> bool` |
| `.IsPerpendicularTo` | `other: Vc * maxTangent: float<tangent> -> bool` | `.IsPerpendicularTo` | `other: Vec * maxTangent: float<tangent> -> bool` |
| `.IsPerpendicularTo` | `other: UnitVc * maxTangent: float<tangent> -> bool` | `.IsPerpendicularTo` | `other: UnitVec * maxTangent: float<tangent> -> bool` |
| `Line2D.isPerpendicularTo` | `maxTangent: float<tangent> -> lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isPerpendicularTo` | `maxTangent: float<tangent> -> lnA: Line3D -> lnB: Line3D -> bool` |
| `.IsPointOnLeft` | `pt: Pt -> bool` |  |  |
| `Line2D.isPointOnLeft` | `pt: Pt -> ln: Line2D -> bool` |  |  |
| `.IsPointOnRight` | `pt: Pt -> bool` |  |  |
| `Line2D.isPointOnRight` | `pt: Pt -> ln: Line2D -> bool` |  |  |
| `.IsTiny` | `tol: float -> bool` | `.IsTiny` | `tol: float -> bool` |
| `Line2D.isTiny` | `tol: float -> l: Line2D -> bool` | `Line3D.isTiny` | `tol: float -> l: Line3D -> bool` |
| `.IsTinySq` | `tol: float -> bool` | `.IsTinySq` | `tol: float -> bool` |
| `Line2D.isTinySq` | `tol: float -> l: Line2D -> bool` | `Line3D.isTinySq` | `tol: float -> l: Line3D -> bool` |
|  |  | `.IsVertical` | `bool` |
|  |  | `Line3D.isVertical` | `l: Line3D -> bool` |
| `.IsXAligned` | `bool` | `.IsXAligned` | `bool` |
| `Line2D.isXAligned` | `l: Line2D -> bool` | `Line3D.isXAligned` | `l: Line3D -> bool` |
| `.IsXYOnLeft` | `x: float * y: float -> bool` |  |  |
| `Line2D.isXYOnLeft` | `x: float -> y: float -> ln: Line2D -> bool` |  |  |
| `.IsXYOnRight` | `x: float * y: float -> bool` |  |  |
| `Line2D.isXYOnRight` | `x: float -> y: float -> ln: Line2D -> bool` |  |  |
| `.IsYAligned` | `bool` | `.IsYAligned` | `bool` |
| `Line2D.isYAligned` | `l: Line2D -> bool` | `Line3D.isYAligned` | `l: Line3D -> bool` |
|  |  | `.IsZAligned` | `bool` |
|  |  | `Line3D.isZAligned` | `l: Line3D -> bool` |
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
|  |  | `.MoveZ` | `distance: float -> Line3D` |
|  |  | `Line3D.moveZ` | `distance: float -> ln: Line3D -> Line3D` |
| `.RayClosestParameter` | `pt: Pt -> float` | `.RayClosestParameter` | `p: Pnt -> float` |
| `Line2D.rayClosestParameter` | `p: Pt -> ln: Line2D -> float` | `Line3D.rayClosestParameter` | `p: Pnt -> ln: Line3D -> float` |
|  |  | `.RayClosestParameters` | `lnB: Line3D -> float * float` |
|  |  | `Line3D.rayClosestParameters` | `lnA: Line3D -> lnB: Line3D -> float * float` |
| `.RayClosestParameterXY` | `x: float * y: float -> float` |  |  |
| `Line2D.rayClosestParameterXY` | `x: float -> y: float -> ln: Line2D -> float` |  |  |
|  |  | `.RayClosestParameterXYZ` | `x: float * y: float * z: float -> float` |
|  |  | `Line3D.rayClosestParameterXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
| `.RayClosestPoint` | `pt: Pt -> Pt` | `.RayClosestPoint` | `p: Pnt -> Pnt` |
| `Line2D.rayClosestPoint` | `p: Pt -> ln: Line2D -> Pt` | `Line3D.rayClosestPoint` | `p: Pnt -> ln: Line3D -> Pnt` |
|  |  | `.RayClosestPoints` | `lnB: Line3D -> Pnt * Pnt` |
|  |  | `Line3D.rayClosestPoints` | `lnA: Line3D -> lnB: Line3D -> Pnt * Pnt` |
| `.Reversed` | `Line2D` | `.Reversed` | `Line3D` |
| `Line2D.reversed` | `ln: Line2D -> Line2D` | `Line3D.reversed` | `ln: Line3D -> Line3D` |
| `.Rotate` | `r: Rotation2D -> Line2D` | `.Rotate` | `q: Quaternion -> Line3D` |
| `Line2D.rotate` | `r: Rotation2D -> ln: Line2D -> Line2D` | `Line3D.rotate` | `q: Quaternion -> ln: Line3D -> Line3D` |
| `.RotateAround` | `cen: Pt -> r: Rotation2D -> Line2D` | `.RotateAround` | `cen: Pnt -> q: Quaternion -> Line3D` |
| `Line2D.rotateAround` | `cen: Pt -> r: Rotation2D -> ln: Line2D -> Line2D` | `Line3D.rotateAround` | `cen: Pnt -> q: Quaternion -> ln: Line3D -> Line3D` |
| `.RotateAroundDegrees` | `cen: Pt -> angDegree: float -> Line2D` |  |  |
| `Line2D.rotateAroundDegrees` | `cen: Pt -> angDegree: float -> ln: Line2D -> Line2D` |  |  |
| `.RotateAroundRadians` | `cen: Pt -> angRadians: float -> Line2D` |  |  |
| `Line2D.rotateAroundRadians` | `cen: Pt -> angRadians: float -> ln: Line2D -> Line2D` |  |  |
| `.RotateDegrees` | `angDegree: float -> Line2D` |  |  |
| `Line2D.rotateDegrees` | `angDegree: float -> ln: Line2D -> Line2D` |  |  |
| `.RotateRadians` | `angRadians: float -> Line2D` |  |  |
| `Line2D.rotateRadians` | `angRadians: float -> ln: Line2D -> Line2D` |  |  |
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
| `.SqDistanceRayXY` | `x: float * y: float -> float` |  |  |
| `Line2D.sqDistanceRayXY` | `x: float -> y: float -> ln: Line2D -> float` |  |  |
|  |  | `.SqDistanceRayXYZ` | `x: float * y: float * z: float -> float` |
|  |  | `Line3D.sqDistanceRayXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
| `.SqDistanceToPt` | `p: Pt -> float` | `.SqDistanceToPnt` | `p: Pnt -> float` |
| `Line2D.sqDistanceToPt` | `p: Pt -> ln: Line2D -> float` | `Line3D.sqDistanceToPnt` | `p: Pnt -> ln: Line3D -> float` |
| `.SqDistanceToXY` | `x: float * y: float -> float` |  |  |
| `Line2D.sqDistanceToXY` | `x: float -> y: float -> ln: Line2D -> float` |  |  |
|  |  | `.SqDistanceToXYZ` | `x: float * y: float * z: float -> float` |
|  |  | `Line3D.sqDistanceToXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` |
| `.Start` | `Pt` | `.Start` | `Pnt` |
| `Line2D.start` | `l: Line2D -> Pt` | `Line3D.start` | `l: Line3D -> Pnt` |
| `.SubLine` | `start: float * endParameter: float -> Line2D` | `.SubLine` | `start: float * endParameter: float -> Line3D` |
| `Line2D.subLine` | `start: float -> endParameter: float -> ln: Line2D -> Line2D` | `Line3D.subLine` | `start: float -> endParameter: float -> ln: Line3D -> Line3D` |
| `.Tangent` | `Vc` | `.Tangent` | `Vec` |
| `Line2D.tangent` | `ln: Line2D -> Vc` | `Line3D.tangent` | `ln: Line3D -> Vec` |
| `.To` | `Pt` | `.To` | `Pnt` |
| `.ToString` | `unit -> string` | `.ToString` | `unit -> string` |
|  |  | `.Transform` | `m: Matrix -> Line3D` |
|  |  | `Line3D.transform` | `m: Matrix -> ln: Line3D -> Line3D` |
|  |  | `.TransformRigid` | `m: RigidMatrix -> Line3D` |
|  |  | `Line3D.transformRigid` | `m: RigidMatrix -> ln: Line3D -> Line3D` |
|  |  | `.TryRayClosestParameters` | `lnB: Line3D -> (float * float) voption` |
|  |  | `Line3D.tryRayClosestParameters` | `lnA: Line3D -> lnB: Line3D -> (float * float) voption` |
|  |  | `.TryRayClosestPoints` | `lnB: Line3D -> (Pnt * Pnt) voption` |
|  |  | `Line3D.tryRayClosestPoints` | `lnA: Line3D -> lnB: Line3D -> (Pnt * Pnt) voption` |
| `.UnitTangent` | `UnitVc` | `.UnitTangent` | `UnitVec` |
| `Line2D.unitTangent` | `ln: Line2D -> UnitVc` | `Line3D.unitTangent` | `ln: Line3D -> UnitVec` |
| `.Vector` | `Vc` | `.Vector` | `Vec` |
| `Line2D.vector` | `ln: Line2D -> Vc` | `Line3D.vector` | `ln: Line3D -> Vec` |
| `.VectorX` | `float` | `.VectorX` | `float` |
| `Line2D.vectorX` | `l: Line2D -> float` | `Line3D.vectorX` | `ln: Line3D -> float` |
| `.VectorY` | `float` | `.VectorY` | `float` |
| `Line2D.vectorY` | `l: Line2D -> float` | `Line3D.vectorY` | `ln: Line3D -> float` |
|  |  | `.VectorZ` | `float` |
|  |  | `Line3D.vectorZ` | `ln: Line3D -> float` |
| **Other static members** | | | |
|  |  | `Line3D.createFromLine2D` | `ln: Line2D -> Line3D` |
|  |  | `Line3D.createFromLine2DWithZ` | `zLevel: float -> ln: Line2D -> Line3D` |
| `Line2D.createFromLine3D` | `ln: Line3D -> Line2D` |  |  |
| `Line2D.createFromPtAndVc` | `p: Pt * v: Vc -> Line2D` |  |  |
|  |  | `Line3D.createFromPntAndVec` | `p: Pnt * v: Vec -> Line3D` |
| `Line2D.createFromVc` | `v: Vc -> Line2D` |  |  |
|  |  | `Line3D.createFromVec` | `v: Vec -> Line3D` |
| `Line2D.cross` | `a: Line2D -> b: Line2D -> float` |  |  |
| `Line2D.det` | `a: Line2D -> b: Line2D -> float` |  |  |
| `Line2D.distanceToLine` | `lnA: Line2D -> lnB: Line2D -> float` | `Line3D.distanceToLine` | `lnA: Line3D -> lnB: Line3D -> float` |
| `Line2D.divide` | `segments: int -> ln: Line2D -> Pt array` | `Line3D.divide` | `segments: int -> ln: Line3D -> Pnt array` |
| `Line2D.divideEvery` | `dist: float -> l: Line2D -> ResizeArray<Pt>` | `Line3D.divideEvery` | `dist: float -> l: Line3D -> ResizeArray<Pnt>` |
| `Line2D.divideInsideEvery` | `dist: float -> l: Line2D -> ResizeArray<Pt>` | `Line3D.divideInsideEvery` | `dist: float -> l: Line3D -> ResizeArray<Pnt>` |
| `Line2D.divideMaxLength` | `maxSegmentLength: float -> ln: Line2D -> Pt array` | `Line3D.divideMaxLength` | `maxSegmentLength: float -> ln: Line3D -> Pnt array` |
| `Line2D.divideMinLength` | `minSegmentLength: float -> ln: Line2D -> Pt array` | `Line3D.divideMinLength` | `minSegmentLength: float -> ln: Line3D -> Pnt array` |
| `Line2D.doIntersect` | `lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.doIntersect` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line2D.doIntersectOrOverlap` | `lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.doIntersectOrOverlap` | `lnA: Line3D -> lnB: Line3D -> bool` |
|  |  | `Line3D.doRaysIntersect` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line2D.end'` | `l: Line2D -> Pt` | `Line3D.end'` | `l: Line3D -> Pnt` |
| `Line2D.equalsExact` | `a: Line2D -> b: Line2D -> bool` | `Line3D.equalsExact` | `a: Line3D -> b: Line3D -> bool` |
| `Line2D.equalsWithin` | `tol: float -> a: Line2D -> b: Line2D -> bool` | `Line3D.equalsWithin` | `tol: float -> a: Line3D -> b: Line3D -> bool` |
| `Line2D.flip` | `ln: Line2D -> Line2D` | `Line3D.flip` | `ln: Line3D -> Line3D` |
| `Line2D.fromX` | `l: Line2D -> float` | `Line3D.fromX` | `l: Line3D -> float` |
| `Line2D.fromY` | `l: Line2D -> float` | `Line3D.fromY` | `l: Line3D -> float` |
|  |  | `Line3D.fromZ` | `l: Line3D -> float` |
|  |  | `Line3D.intersectCone` | `ray: Line3D * coneRadius: float * coneBaseZ: float * coneTipZ: float -> float * float` |
| `Line2D.intersectRays` | `lnA: Line2D -> lnB: Line2D -> Pt` |  |  |
| `Line2D.isNormalTo'` | `lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isNormalTo'` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line2D.isNormalToUnitVc` | `maxTangent: float<tangent> -> other: UnitVc -> ln: Line2D -> bool` |  |  |
|  |  | `Line3D.isNormalToUnitVec` | `maxTangent: float<tangent> -> other: UnitVec -> ln: Line3D -> bool` |
| `Line2D.isNormalToVc` | `maxTangent: float<tangent> -> other: Vc -> ln: Line2D -> bool` |  |  |
|  |  | `Line3D.isNormalToVec` | `maxTangent: float<tangent> -> other: Vec -> ln: Line3D -> bool` |
| `Line2D.isNotParallelTo` | `lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isNotParallelTo` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line2D.isNotParallelTo'` | `lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isNotParallelTo'` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line2D.isParallelAndOrientedToUnitVc` | `minTangent: float<tangent> -> other: UnitVc -> ln: Line2D -> bool` |  |  |
|  |  | `Line3D.isParallelAndOrientedToUnitVec` | `minTangent: float<tangent> -> other: UnitVec -> ln: Line3D -> bool` |
| `Line2D.isParallelAndOrientedToVc` | `minTangent: float<tangent> -> other: Vc -> ln: Line2D -> bool` |  |  |
|  |  | `Line3D.isParallelAndOrientedToVec` | `minTangent: float<tangent> -> other: Vec -> ln: Line3D -> bool` |
| `Line2D.isParallelTo'` | `lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isParallelTo'` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line2D.isParallelToUnitVc` | `minTangent: float<tangent> -> other: UnitVc -> ln: Line2D -> bool` |  |  |
|  |  | `Line3D.isParallelToUnitVec` | `minTangent: float<tangent> -> other: UnitVec -> ln: Line3D -> bool` |
| `Line2D.isParallelToVc` | `minTangent: float<tangent> -> other: Vc -> ln: Line2D -> bool` |  |  |
|  |  | `Line3D.isParallelToVec` | `minTangent: float<tangent> -> other: Vec -> ln: Line3D -> bool` |
| `Line2D.isPerpendicularTo'` | `lnA: Line2D -> lnB: Line2D -> bool` | `Line3D.isPerpendicularTo'` | `lnA: Line3D -> lnB: Line3D -> bool` |
| `Line2D.isPerpendicularToUnitVc` | `maxTangent: float<tangent> -> other: UnitVc -> ln: Line2D -> bool` |  |  |
|  |  | `Line3D.isPerpendicularToUnitVec` | `maxTangent: float<tangent> -> other: UnitVec -> ln: Line3D -> bool` |
| `Line2D.isPerpendicularToVc` | `maxTangent: float<tangent> -> other: Vc -> ln: Line2D -> bool` |  |  |
|  |  | `Line3D.isPerpendicularToVec` | `maxTangent: float<tangent> -> other: Vec -> ln: Line3D -> bool` |
| `Line2D.isTouchingEndOf` | `squareTolerance: float -> a: Line2D -> b: Line2D -> bool` | `Line3D.isTouchingEndOf` | `squareTolerance: float -> a: Line3D -> b: Line3D -> bool` |
| `Line2D.lengthToPtOnLine` | `pt: Pt -> ln: Line2D -> float` | `Line3D.lengthToPntOnLine` | `pnt: Pnt -> ln: Line3D -> float` |
| `Line2D.matchesOrientationUnitVc` | `v: UnitVc -> ln: Line2D -> bool` |  |  |
|  |  | `Line3D.matchesOrientationUnitVec` | `v: UnitVec -> ln: Line3D -> bool` |
| `Line2D.matchesOrientationVc` | `v: Vc -> ln: Line2D -> bool` |  |  |
|  |  | `Line3D.matchesOrientationVec` | `v: Vec -> ln: Line3D -> bool` |
| `Line2D.matchOrientation` | `orientationToMatch: Line2D -> lineToFlip: Line2D -> Line2D` | `Line3D.matchOrientation` | `orientationToMatch: Line3D -> lineToFlip: Line3D -> Line3D` |
| `Line2D.matchUnitVcOrientation` | `orientationToMatch: UnitVc -> lineToFlip: Line2D -> Line2D` |  |  |
|  |  | `Line3D.matchUnitVecOrientation` | `orientationToMatch: UnitVec -> lineToFlip: Line3D -> Line3D` |
| `Line2D.matchVcOrientation` | `orientationToMatch: Vc -> lineToFlip: Line2D -> Line2D` |  |  |
|  |  | `Line3D.matchVecOrientation` | `orientationToMatch: Vec -> lineToFlip: Line3D -> Line3D` |
| `Line2D.notEqualsWithin` | `tol: float -> a: Line2D -> b: Line2D -> bool` | `Line3D.notEqualsWithin` | `tol: float -> a: Line3D -> b: Line3D -> bool` |
| `Line2D.offset` | `distance: float -> ln: Line2D -> Line2D` | `Line3D.offset` | `distHorizontal: float -> distNormal: float -> l: Line3D -> Line3D` |
|  |  | `Line3D.offsetXY` | `amount: float -> ln: Line3D -> Line3D` |
| `Line2D.pointAtDistance` | `dist: float -> ln: Line2D -> Pt` | `Line3D.pointAtDistance` | `dist: float -> ln: Line3D -> Pnt` |
| `Line2D.pointAtDistanceFromEnd` | `dist: float -> ln: Line2D -> Pt` | `Line3D.pointAtDistanceFromEnd` | `dist: float -> ln: Line3D -> Pnt` |
| `Line2D.projectOntoRay` | `rayToProjectOnto: Line2D -> lineToProject: Line2D -> Line2D` | `Line3D.projectOntoRay` | `rayToProjectOnto: Line3D -> lineToProject: Line3D -> Line3D` |
| `Line2D.projectOntoRayParam` | `rayToProjectOnto: Line2D -> lineToProject: Line2D -> float * float` | `Line3D.projectOntoRayParam` | `rayToProjectOnto: Line3D -> lineToProject: Line3D -> float * float` |
| `Line2D.reverse` | `ln: Line2D -> Line2D` | `Line3D.reverse` | `ln: Line3D -> Line3D` |
|  |  | `Line3D.rotateOnZ` | `r: Rotation2D -> ln: Line3D -> Line3D` |
|  |  | `Line3D.rotateOnZWithCenter` | `cen: Pnt -> r: Rotation2D -> ln: Line3D -> Line3D` |
| `Line2D.split` | `gap: float -> segments: int -> ln: Line2D -> Line2D array` | `Line3D.split` | `gap: float -> segments: int -> ln: Line3D -> Line3D array` |
| `Line2D.splitMaxLength` | `gap: float -> maxSegmentLength: float -> ln: Line2D -> Line2D array` | `Line3D.splitMaxLength` | `gap: float -> maxSegmentLength: float -> ln: Line3D -> Line3D array` |
| `Line2D.splitMinLength` | `gap: float -> minSegmentLength: float -> ln: Line2D -> Line2D array` | `Line3D.splitMinLength` | `gap: float -> minSegmentLength: float -> ln: Line3D -> Line3D array` |
| `Line2D.sqDistanceToLine` | `lnA: Line2D -> lnB: Line2D -> float` | `Line3D.sqDistanceToLine` | `lnA: Line3D -> lnB: Line3D -> float` |
| `Line2D.to'` | `l: Line2D -> Pt` | `Line3D.to'` | `l: Line3D -> Pnt` |
|  |  | `Line3D.toLine2D` | `ln: Line3D -> Line2D` |
| `Line2D.toLine3D` | `zLevel: float -> ln: Line2D -> Line3D` |  |  |
| `Line2D.toX` | `l: Line2D -> float` | `Line3D.toX` | `l: Line3D -> float` |
| `Line2D.toY` | `l: Line2D -> float` | `Line3D.toY` | `l: Line3D -> float` |
|  |  | `Line3D.toZ` | `l: Line3D -> float` |
| `Line2D.translate` | `v: Vc -> ln: Line2D -> Line2D` | `Line3D.translate` | `v: Vec -> ln: Line3D -> Line3D` |
| `Line2D.tryGetOverlap` | `lnA: Line2D -> lnB: Line2D -> (float * float) voption` | `Line3D.tryGetOverlap` | `lnA: Line3D -> lnB: Line3D -> (float * float) voption` |
| `Line2D.tryIntersect` | `lnA: Line2D -> lnB: Line2D -> Pt voption` | `Line3D.tryIntersect` | `lnA: Line3D -> lnB: Line3D -> Pnt voption` |
|  |  | `Line3D.tryIntersectCone` | `ray: Line3D * coneRadius: float * coneBaseZ: float * coneTipZ: float -> (float * float) voption` |
| `Line2D.tryIntersectOrOverlap` | `lnA: Line2D -> lnB: Line2D -> Pt voption` | `Line3D.tryIntersectOrOverlap` | `lnA: Line3D -> lnB: Line3D -> Pnt voption` |
| `Line2D.tryIntersectRay` | `lineA: Line2D -> lineB: Line2D -> Pt voption` | `Line3D.tryIntersectRay` | `lineA: Line3D -> lineB: Line3D -> Pnt voption` |
| `Line2D.tryProjectOntoLine` | `onToLine: Line2D -> lineToProject: Line2D -> Line2D voption` | `Line3D.tryProjectOntoLine` | `onToLine: Line3D -> lineToProject: Line3D -> Line3D voption` |
| `Line2D.tryProjectOntoLineParam` | `onToLine: Line2D -> lineToProject: Line2D -> (float * float) voption` | `Line3D.tryProjectOntoLineParam` | `onToLine: Line3D -> lineToProject: Line3D -> (float * float) voption` |
| `Line2D.withEnd` | `pt: Pt -> ln: Line2D -> Line2D` | `Line3D.withEnd` | `pnt: Pnt -> ln: Line3D -> Line3D` |
| `Line2D.withFrom` | `pt: Pt -> ln: Line2D -> Line2D` | `Line3D.withFrom` | `pnt: Pnt -> ln: Line3D -> Line3D` |
| `Line2D.withLengthFromMid` | `len: float -> ln: Line2D -> Line2D` | `Line3D.withLengthFromMid` | `len: float -> ln: Line3D -> Line3D` |
| `Line2D.withLengthFromStart` | `len: float -> ln: Line2D -> Line2D` | `Line3D.withLengthFromStart` | `len: float -> ln: Line3D -> Line3D` |
| `Line2D.withLengthToEnd` | `len: float -> ln: Line2D -> Line2D` | `Line3D.withLengthToEnd` | `len: float -> ln: Line3D -> Line3D` |
| `Line2D.withStart` | `pt: Pt -> ln: Line2D -> Line2D` | `Line3D.withStart` | `pnt: Pnt -> ln: Line3D -> Line3D` |
| `Line2D.withTo` | `pt: Pt -> ln: Line2D -> Line2D` | `Line3D.withTo` | `pnt: Pnt -> ln: Line3D -> Line3D` |

## XLine2D.XRayParam / XLine3D.XRayParam

| XRayParam | Signature | XRayParam | Signature |
|:--|:--|:--|:--|
| `Intersect` | `twoParams: float * float -> XRayParam` |  |  |
| `Parallel` | `XRayParam` | `Parallel` | `XRayParam` |
| `TooShortA` | `XRayParam` | `TooShortA` | `XRayParam` |
| `TooShortB` | `XRayParam` | `TooShortB` | `XRayParam` |
| `TooShortBoth` | `XRayParam` | `TooShortBoth` | `XRayParam` |
|  |  | `SkewOrX` | `twoParams: float * float -> XRayParam` |

## XLine2D.XRay / XLine3D.XRay

| XRay | Signature | XRay | Signature |
|:--|:--|:--|:--|
| `Intersect` | `xPt: Pt -> XRay` | `Intersect` | `xPnt: Pnt -> XRay` |
| `Parallel` | `XRay` | `Parallel` | `XRay` |
| `TooShortA` | `XRay` | `TooShortA` | `XRay` |
| `TooShortB` | `XRay` | `TooShortB` | `XRay` |
| `TooShortBoth` | `XRay` | `TooShortBoth` | `XRay` |
|  |  | `Skew` | `closestPntA: Pnt * closestPntB: Pnt * squareDist: float -> XRay` |

## XLine2D.XParam / XLine3D.XParam

| XParam | Signature | XParam | Signature |
|:--|:--|:--|:--|
| `Intersect` | `twoParams: float * float -> XParam` | `Intersect` | `twoParams: float * float -> XParam` |
| `Apart` | `XParam` | `Apart` | `XParam` |
| `Parallel` | `XParam` | `Parallel` | `XParam` |
| `TooShortA` | `XParam` | `TooShortA` | `XParam` |
| `TooShortB` | `XParam` | `TooShortB` | `XParam` |
| `TooShortBoth` | `XParam` | `TooShortBoth` | `XParam` |
|  |  | `Skew` | `paramA: float * paramB: float * squareDist: float -> XParam` |

## XLine2D.XPt / XLine3D.XPnt

| XPt | Signature | XPnt | Signature |
|:--|:--|:--|:--|
| `Intersect` | `xPt: Pt -> XPt` | `Intersect` | `xPnt: Pnt -> XPnt` |
| `Apart` | `XPt` | `Apart` | `XPnt` |
| `Parallel` | `XPt` | `Parallel` | `XPnt` |
| `TooShortA` | `XPt` | `TooShortA` | `XPnt` |
| `TooShortB` | `XPt` | `TooShortB` | `XPnt` |
| `TooShortBoth` | `XPt` | `TooShortBoth` | `XPnt` |
|  |  | `Skew` | `closestPntA: Pnt * closestPntB: Pnt * squareDist: float -> XPnt` |

## XLine2D.ClParams / XLine3D.ClParams

| ClParams | Signature | ClParams | Signature |
|:--|:--|:--|:--|
| `Intersect` | `paramA: float * paramB: float -> ClParams` | `Intersect` | `paramA: float * paramB: float -> ClParams` |
| `Parallel` | `paramA: float * paramB: float -> ClParams` | `Parallel` | `paramA: float * paramB: float -> ClParams` |
| `Apart` | `paramA: float * paramB: float -> ClParams` | `Apart` | `paramA: float * paramB: float * squareDist: float -> ClParams` |
| `TooShortA` | `ClParams` | `TooShortA` | `ClParams` |
| `TooShortB` | `ClParams` | `TooShortB` | `ClParams` |
| `TooShortBoth` | `ClParams` | `TooShortBoth` | `ClParams` |
|  |  | `Skew` | `paramA: float * paramB: float * squareDist: float -> ClParams` |

## XLine2D.ClPts / XLine3D.ClPnts

| ClPts | Signature | ClPnts | Signature |
|:--|:--|:--|:--|
| `Intersect` | `pt: Pt -> ClPts` | `Intersect` | `pnt: Pnt -> ClPnts` |
| `Parallel` | `ptA: Pt * ptB: Pt -> ClPts` | `Parallel` | `pntA: Pnt * pntB: Pnt -> ClPnts` |
| `Apart` | `ptA: Pt * ptB: Pt -> ClPts` | `Apart` | `pntA: Pnt * pntB: Pnt * squareDist: float -> ClPnts` |
| `TooShortA` | `ClPts` | `TooShortA` | `ClPnts` |
| `TooShortB` | `ClPts` | `TooShortB` | `ClPnts` |
| `TooShortBoth` | `ClPts` | `TooShortBoth` | `ClPnts` |
|  |  | `Skew` | `pntA: Pnt * pntB: Pnt * squareDist: float -> ClPnts` |

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
| `XLineXY.classifyClosestParameters` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tangent: float<tangent> * tooShortTolerance: float -> ClParams` | `XLineXYZ.classifyClosestParameters` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * tangent: float<tangent> * tooShortTolerance: float -> ClParams` |
| `XLineXY.classifyClosestPoints` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tangent: float<tangent> * tooShortTolerance: float -> ClPts` | `XLineXYZ.classifyClosestPoints` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * tangent: float<tangent> * tooShortTolerance: float -> ClPnts` |
| `XLineXY.classifyIntersection` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tangent: float<tangent> * tooShortTolerance: float -> XPt` | `XLineXYZ.classifyIntersection` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> XPnt` |
| `XLineXY.classifyIntersectionParam` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tangent: float<tangent> * tooShortTolerance: float -> XParam` | `XLineXYZ.classifyIntersectionParam` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> XParam` |
|  |  | `XLineXYZ.classifyRayClosestParam` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * tangent: float<tangent> * tooShortTolerance: float -> XRayParam` |
| `XLineXY.classifyRayIntersection` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tangent: float<tangent> * tooShortTolerance: float -> XRay` | `XLineXYZ.classifyRayIntersection` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> XRay` |
| `XLineXY.classifyRayIntersectionParam` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tangent: float<tangent> * tooShortTolerance: float -> XRayParam` |  |  |
| `XLineXY.closestParameters` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> struct (float * float)` |  |  |
| `XLineXY.doIntersect` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> bool` | `XLineXYZ.doIntersect` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> bool` |
| `XLineXY.doOverlap` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tolerance: float -> bool` | `XLineXYZ.doOverlap` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * tolerance: float -> bool` |
|  |  | `XLineXYZ.doRaysIntersect` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float -> bool` |
| `XLineXY.intersectRays` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> Pt` |  |  |
| `XLineXY.isWithinRanges` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> bool` |  |  |
| `XLineXY.parameterA` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> float` | `XLineXYZ.parameterA` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> float` |
| `XLineXY.parameters` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> float * float` | `XLineXYZ.parameters` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> float * float` |
| `XLineXY.sqDistance` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> float` | `XLineXYZ.sqDistance` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> float` |
|  |  | `XLineXYZ.tryClosestParameterRayA` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> float voption` |
|  |  | `XLineXYZ.tryClosestPntRayA` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> Pnt voption` |
| `XLineXY.tryIntersect` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> Pt voption` | `XLineXYZ.tryIntersect` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> Pnt voption` |
| `XLineXY.tryIntersectInRangeA` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * minParameterA: float * maxParameterA: float -> Pt voption` |  |  |
| `XLineXY.tryIntersectInRanges` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> Pt voption` |  |  |
| `XLineXY.tryIntersectRay` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tangent: float<tangent> -> Pt voption` | `XLineXYZ.tryIntersectRay` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float * maxSkewDistance: float * tangent: float<tangent> * tooShortTolerance: float -> Pnt voption` |
| `XLineXY.tryParameterA` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> float voption` |  |  |

## XLine2D / XLine3D

| XLine2D | Signature | XLine3D | Signature |
|:--|:--|:--|:--|
| `XLine2D.classifyClosestParameters` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` | `XLine3D.classifyClosestParameters` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` |
| `XLine2D.classifyClosestParameters` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` | `XLine3D.classifyClosestParameters` | `lineA: Line3D * lineB: Line3D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` |
| `XLine2D.classifyClosestPoints` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPts` | `XLine3D.classifyClosestPoints` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPnts` |
| `XLine2D.classifyClosestPoints` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPts` | `XLine3D.classifyClosestPoints` | `lineA: Line3D * lineB: Line3D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPnts` |
|  |  | `XLine3D.classifyConeIntersection` | `ray: Line3D * coneRadius: float * coneBaseZ: float * coneTipZ: float -> XCone` |
| `XLine2D.classifyEndsTouching` | `a: Line2D * b: Line2D * ?tolerance: float -> XEnds` | `XLine3D.classifyEndsTouching` | `a: Line3D * b: Line3D * ?tolerance: float -> XEnds` |
| `XLine2D.classifyIntersection` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPt` | `XLine3D.classifyIntersection` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPnt` |
| `XLine2D.classifyIntersection` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPt` | `XLine3D.classifyIntersection` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPnt` |
| `XLine2D.classifyIntersectionParam` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` | `XLine3D.classifyIntersectionParam` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` |
| `XLine2D.classifyIntersectionParam` | `lnA: Line2D * lnB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` | `XLine3D.classifyIntersectionParam` | `lnA: Line3D * lnB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` |
|  |  | `XLine3D.classifyRayClosestParam` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` |
|  |  | `XLine3D.classifyRayClosestParam` | `lineA: Line3D * lineB: Line3D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` |
| `XLine2D.classifyRayIntersection` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` | `XLine3D.classifyRayIntersection` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` |
| `XLine2D.classifyRayIntersection` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` | `XLine3D.classifyRayIntersection` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` |
| `XLine2D.classifyRayIntersectionParam` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` |  |  |
| `XLine2D.classifyRayIntersectionParam` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` |  |  |
| `XLine2D.closestParameters` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> struct (float * float)` |  |  |
| `XLine2D.closestParameters` | `lineA: Line2D * lineB: Line2D -> struct (float * float)` |  |  |
| `XLine2D.doIntersect` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> bool` | `XLine3D.doIntersect` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> bool` |
| `XLine2D.doIntersect` | `lineA: Line2D * lineB: Line2D -> bool` | `XLine3D.doIntersect` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> bool` |
| `XLine2D.doOverlap` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tolerance: float -> bool` | `XLine3D.doOverlap` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tolerance: float -> bool` |
| `XLine2D.doOverlap` | `lineA: Line2D * lineB: Line2D * ?tolerance: float -> bool` | `XLine3D.doOverlap` | `lineA: Line3D * lineB: Line3D * ?tolerance: float -> bool` |
|  |  | `XLine3D.doRaysIntersect` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float -> bool` |
|  |  | `XLine3D.doRaysIntersect` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float -> bool` |
| `XLine2D.getSqDistance` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> float` | `XLine3D.getSqDistance` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec -> float` |
| `XLine2D.getSqDistance` | `lineA: Line2D * lineB: Line2D -> float` | `XLine3D.getSqDistance` | `lineA: Line3D * lineB: Line3D -> float` |
| `XLine2D.intersectRays` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> Pt` |  |  |
| `XLine2D.intersectRays` | `lineA: Line2D * lineB: Line2D -> Pt` |  |  |
| `XLine2D.isWithinRanges` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> bool` |  |  |
| `XLine2D.isWithinRanges` | `lineA: Line2D * lineB: Line2D * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> bool` |  |  |
| `XLine2D.parameterA` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> float` | `XLine3D.parameterA` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec -> float` |
| `XLine2D.parameterA` | `lineA: Line2D * lineB: Line2D -> float` | `XLine3D.parameterA` | `lineA: Line3D * lineB: Line3D -> float` |
| `XLine2D.parameters` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> float * float` | `XLine3D.parameters` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec -> float * float` |
| `XLine2D.parameters` | `lineA: Line2D * lineB: Line2D -> float * float` | `XLine3D.parameters` | `lineA: Line3D * lineB: Line3D -> float * float` |
|  |  | `XLine3D.tryClosestParameterRayA` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec -> float voption` |
|  |  | `XLine3D.tryClosestParameterRayA` | `lineA: Line3D * lineB: Line3D -> float voption` |
|  |  | `XLine3D.tryClosestPntRayA` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec -> Pnt voption` |
|  |  | `XLine3D.tryClosestPntRayA` | `lineA: Line3D * lineB: Line3D -> Pnt voption` |
| `XLine2D.tryIntersect` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> Pt voption` | `XLine3D.tryIntersect` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` |
| `XLine2D.tryIntersect` | `lineA: Line2D * lineB: Line2D -> Pt voption` | `XLine3D.tryIntersect` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` |
| `XLine2D.tryIntersectInRangeA` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * minParameterA: float * maxParameterA: float -> Pt voption` |  |  |
| `XLine2D.tryIntersectInRangeA` | `lineA: Line2D * lineB: Line2D * minParameterA: float * maxParameterA: float -> Pt voption` |  |  |
| `XLine2D.tryIntersectInRanges` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> Pt voption` |  |  |
| `XLine2D.tryIntersectInRanges` | `lineA: Line2D * lineB: Line2D * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> Pt voption` |  |  |
| `XLine2D.tryIntersectRay` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> -> Pt voption` | `XLine3D.tryIntersectRay` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` |
| `XLine2D.tryIntersectRay` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> -> Pt voption` | `XLine3D.tryIntersectRay` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` |
| `XLine2D.tryParameterA` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> float voption` |  |  |
| `XLine2D.tryParameterA` | `lineA: Line2D * lineB: Line2D -> float voption` |  |  |

## XLine3D.XCone

| XCone | Signature |
|:--|:--|
| `NoIntersection` | `XCone` |
| `Intersecting` | `float * float -> XCone` |
| `IntersectingOne` | `float -> XCone` |
| `Touching` | `float -> XCone` |
| `ThroughTip` | `float -> XCone` |
| `LineOnCone` | `tipParam: float * onConeTowardsPositiveT: bool -> XCone` |

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
| `NPlane.create` | `pnt: Pnt * normal: Vec -> NPlane` |
| `NPlane.create` | `pnt: Pnt * normal: UnitVec -> NPlane` |
| `NPlane.createFrom3Points` | `a: Pnt -> b: Pnt -> c: Pnt -> NPlane` |
| `NPlane.createFromPPlane` | `p: PPlane -> NPlane` |
| `NPlane.createUnchecked` | `originX: float * originY: float * originZ: float * normalX: float * normalY: float * normalZ: float -> NPlane` |
| `NPlane.createUncheckedVec` | `origin: Pnt * normal: UnitVec -> NPlane` |
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
| `NPlane.translate` | `translation: Vec -> pl: NPlane -> NPlane` |
| `NPlane.tryIntersect` | `a: NPlane -> b: NPlane -> Line3D voption` |
| `NPlane.tryIntersectLine` | `ln: Line3D -> pl: NPlane -> Pnt voption` |
| `NPlane.tryIntersectRay` | `ln: Line3D -> pl: NPlane -> float voption` |
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
|  |  | `.OriginZ` | `float` |
|  |  | `.XaxisZ` | `float` |
|  |  | `.YaxisZ` | `float` |
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
| `.Contains` | `p: Pt -> bool` |  |  |
| `Rect2D.contains` | `p: Pt -> r: Rect2D -> bool` |  |  |
| `.ContainsXY` | `x: float * y: float -> bool` |  |  |
| `Rect2D.containsXY` | `x: float -> y: float -> r: Rect2D -> bool` |  |  |
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
|  |  | `.Flipped` | `Rect3D` |
|  |  | `Rect3D.flipped` | `r: Rect3D -> Rect3D` |
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
|  |  | `.MoveZ` | `distance: float -> Rect3D` |
|  |  | `Rect3D.moveZ` | `distance: float -> r: Rect3D -> Rect3D` |
|  |  | `.Normal` | `Vec` |
|  |  | `Rect3D.normal` | `r: Rect3D -> Vec` |
|  |  | `.NormalUnit` | `UnitVec` |
|  |  | `Rect3D.normalUnit` | `r: Rect3D -> UnitVec` |
| `.Origin` | `Pt` | `.Origin` | `Pnt` |
| `Rect2D.origin` | `r: Rect2D -> Pt` | `Rect3D.origin` | `r: Rect3D -> Pnt` |
|  |  | `.Plane` | `PPlane` |
|  |  | `Rect3D.plane` | `r: Rect3D -> PPlane` |
| `.Points` | `Pt array` | `.Points` | `Pnt array` |
| `Rect2D.points` | `r: Rect2D -> Pt array` | `Rect3D.points` | `r: Rect3D -> Pnt array` |
| `.PointsLooped` | `Pt array` | `.PointsLooped` | `Pnt array` |
| `Rect2D.pointsLooped` | `r: Rect2D -> Pt array` | `Rect3D.pointsLooped` | `r: Rect3D -> Pnt array` |
| `.PointsXYLooped_CCW` | `ResizeArray<float>` |  |  |
| `Rect2D.pointsXYLooped_CCW` | `r: Rect2D -> ResizeArray<float>` |  |  |
| `.PointsXYLooped_CW` | `ResizeArray<float>` |  |  |
| `Rect2D.pointsXYLooped_CW` | `r: Rect2D -> ResizeArray<float>` |  |  |
|  |  | `.PointsXYZ` | `ResizeArray<float>` |
|  |  | `Rect3D.pointsXYZ` | `r: Rect3D -> ResizeArray<float>` |
|  |  | `.PointsXYZLooped` | `ResizeArray<float>` |
|  |  | `Rect3D.pointsXYZLooped` | `r: Rect3D -> ResizeArray<float>` |
| `.PointsXY_CCW` | `ResizeArray<float>` |  |  |
| `Rect2D.pointsXY_CCW` | `r: Rect2D -> ResizeArray<float>` |  |  |
| `.PointsXY_CW` | `ResizeArray<float>` |  |  |
| `Rect2D.pointsXY_CW` | `r: Rect2D -> ResizeArray<float>` |  |  |
| `.Pt0` | `Pt` | `.Pnt0` | `Pnt` |
| `Rect2D.pt0` | `r: Rect2D -> Pt` |  |  |
| `.Pt1` | `Pt` | `.Pnt1` | `Pnt` |
| `Rect2D.pt1` | `r: Rect2D -> Pt` |  |  |
| `.Pt2` | `Pt` | `.Pnt2` | `Pnt` |
| `Rect2D.pt2` | `r: Rect2D -> Pt` |  |  |
| `.Pt3` | `Pt` | `.Pnt3` | `Pnt` |
| `Rect2D.pt3` | `r: Rect2D -> Pt` |  |  |
| `.Rotate` | `rot: Rotation2D -> Rect2D` | `.Rotate` | `q: Quaternion -> Rect3D` |
| `Rect2D.rotate` | `rot: Rotation2D -> rect: Rect2D -> Rect2D` | `Rect3D.rotate` | `q: Quaternion -> r: Rect3D -> Rect3D` |
| `.RotateAround` | `cen: Pt -> rot: Rotation2D -> Rect2D` | `.RotateAround` | `cen: Pnt -> q: Quaternion -> Rect3D` |
| `Rect2D.rotateAround` | `cen: Pt -> rot: Rotation2D -> rect: Rect2D -> Rect2D` | `Rect3D.rotateAround` | `cen: Pnt -> q: Quaternion -> r: Rect3D -> Rect3D` |
| `.RotateAroundDegrees` | `cen: Pt -> angDegree: float -> Rect2D` |  |  |
| `Rect2D.rotateAroundDegrees` | `cen: Pt -> angDegree: float -> r: Rect2D -> Rect2D` |  |  |
| `.RotateAroundRadians` | `cen: Pt -> angRadians: float -> Rect2D` |  |  |
| `Rect2D.rotateAroundRadians` | `cen: Pt -> angRadians: float -> r: Rect2D -> Rect2D` |  |  |
| `.RotateDegrees` | `angDegree: float -> Rect2D` |  |  |
| `Rect2D.rotateDegrees` | `angDegree: float -> r: Rect2D -> Rect2D` |  |  |
| `.RotateOrientation180` | `Rect2D` | `.RotateOrientation180` | `Rect3D` |
| `Rect2D.rotateOrientation180` | `r: Rect2D -> Rect2D` | `Rect3D.rotateOrientation180` | `r: Rect3D -> Rect3D` |
| `.RotateOrientation90CCW` | `Rect2D` | `.RotateOrientation90CCW` | `Rect3D` |
| `Rect2D.rotateOrientation90CCW` | `r: Rect2D -> Rect2D` | `Rect3D.rotateOrientation90CCW` | `r: Rect3D -> Rect3D` |
| `.RotateOrientation90CW` | `Rect2D` | `.RotateOrientation90CW` | `Rect3D` |
| `Rect2D.rotateOrientation90CW` | `r: Rect2D -> Rect2D` | `Rect3D.rotateOrientation90CW` | `r: Rect3D -> Rect3D` |
| `.RotateRadians` | `angRadians: float -> Rect2D` |  |  |
| `Rect2D.rotateRadians` | `angRadians: float -> r: Rect2D -> Rect2D` |  |  |
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
|  |  | `.TransformRigid` | `m: RigidMatrix -> Rect3D` |
|  |  | `Rect3D.transformRigid` | `m: RigidMatrix -> r: Rect3D -> Rect3D` |
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
| `Rect2D.clipLine` | `line: Line2D -> rect: Rect2D -> Line2D` |  |  |
|  |  | `Rect3D.createCenteredFromPlane` | `pl: PPlane * sizeX: float * sizeY: float -> Rect3D` |
| `Rect2D.createFrom3Points` | `origin: Pt * xPt: Pt * yPt: Pt -> Rect2D` | `Rect3D.createFrom3Points` | `origin: Pnt * xPnt: Pnt * yPnt: Pnt -> Rect3D` |
| `Rect2D.createFromBounds` | `minX: float * minY: float * maxX: float * maxY: float -> Rect2D` |  |  |
|  |  | `Rect3D.createFromBounds2D` | `minX: float * minY: float * maxX: float * maxY: float -> Rect3D` |
| `Rect2D.createFromCenterAndDirection` | `center: Pt * directionX: UnitVc * sizeX: float * sizeY: float -> Rect2D` |  |  |
| `Rect2D.createFromCenterAndVector` | `center: Pt * x: Vc * sizeY: float -> Rect2D` |  |  |
| `Rect2D.createFromDirAndPoints` | `dirX: Vc -> pts: IList<Pt> -> Rect2D` |  |  |
| `Rect2D.createFromDirectionAndSizes` | `origin: Pt * directionX: UnitVc * sizeX: float * sizeY: float -> Rect2D` |  |  |
| `Rect2D.createFromLine` | `line: Line2D * offRight: float * offLeft: float -> Rect2D` |  |  |
|  |  | `Rect3D.createFromPlane` | `pl: PPlane * sizeX: float * sizeY: float -> Rect3D` |
| `Rect2D.createFromVectors` | `origin: Pt * x: Vc * y: Vc -> Rect2D` | `Rect3D.createFromVectors` | `origin: Pnt * x: Vec * y: Vec -> Rect3D` |
| `Rect2D.createFromXVectorAndWidth` | `origin: Pt * x: Vc * sizeY: float -> Rect2D` |  |  |
| `Rect2D.createUnchecked` | `originX: float * originY: float * xaxisX: float * xaxisY: float * yaxisX: float * yaxisY: float -> Rect2D` | `Rect3D.createUnchecked` | `originX: float * originY: float * originZ: float * xAxisX: float * xAxisY: float * xAxisZ: float * yAxisX: float * yAxisY: float * yAxisZ: float -> Rect3D` |
| `Rect2D.createUncheckedVec` | `origin: Pt * x: Vc * y: Vc -> Rect2D` | `Rect3D.createUncheckedVec` | `origin: Pnt * x: Vec * y: Vec -> Rect3D` |
| `Rect2D.equalsExact` | `a: Rect2D -> b: Rect2D -> bool` | `Rect3D.equalsExact` | `a: Rect3D -> b: Rect3D -> bool` |
| `Rect2D.equalsWithin` | `tol: float -> a: Rect2D -> b: Rect2D -> bool` | `Rect3D.equalsWithin` | `tol: float -> a: Rect3D -> b: Rect3D -> bool` |
| `Rect2D.expand` | `dist: float -> r: Rect2D -> Rect2D` | `Rect3D.expand` | `dist: float -> r: Rect3D -> Rect3D` |
| `Rect2D.expandRel` | `factor: float -> r: Rect2D -> Rect2D` | `Rect3D.expandRel` | `factor: float -> r: Rect3D -> Rect3D` |
| `Rect2D.expandRelXY` | `factorX: float -> factorY: float -> r: Rect2D -> Rect2D` | `Rect3D.expandRelXY` | `factorX: float -> factorY: float -> r: Rect3D -> Rect3D` |
| `Rect2D.expandXY` | `distX: float -> distY: float -> r: Rect2D -> Rect2D` | `Rect3D.expandXY` | `distX: float -> distY: float -> r: Rect3D -> Rect3D` |
| `Rect2D.fitToPoints` | `pts: IList<Pt> -> refRect: Rect2D -> Rect2D` | `Rect3D.fitToPoints` | `pts: IList<Pnt> -> refRect: Rect3D -> Rect3D` |
| `Rect2D.fitToPointsXY` | `xys: IList<float> -> refRect: Rect2D -> Rect2D` |  |  |
|  |  | `Rect3D.flip` | `r: Rect3D -> Rect3D` |
| `Rect2D.grid` | `rect: Rect2D * xCount: int * yCount: int -> Pt array array` | `Rect3D.grid` | `rect: Rect3D * xCount: int * yCount: int -> Pnt array array` |
| `Rect2D.gridMaxLength` | `rect: Rect2D * xMaxLen: float * yMaxLen: float -> Pt array array` | `Rect3D.gridMaxLength` | `rect: Rect3D * xMaxLen: float * yMaxLen: float -> Pnt array array` |
| `Rect2D.gridMinLength` | `rect: Rect2D * xMinLen: float * yMinLen: float -> Pt array array` | `Rect3D.gridMinLength` | `rect: Rect3D * xMinLen: float * yMinLen: float -> Pnt array array` |
| `Rect2D.intersectLine` | `line: Line2D -> rect: Rect2D -> Line2D` | `Rect3D.intersectLine` | `ln: Line3D -> r: Rect3D -> Pnt` |
| `Rect2D.intersectRay` | `ray: Line2D -> rect: Rect2D -> float * float` | `Rect3D.intersectRay` | `ln: Line3D -> r: Rect3D -> float` |
| `Rect2D.iterPointsCCW` | `action: (float -> float -> unit) -> r: Rect2D -> unit` | `Rect3D.iterPointsCCW` | `action: (float -> float -> float -> unit) -> r: Rect3D -> unit` |
| `Rect2D.iterPointsCW` | `action: (float -> float -> unit) -> r: Rect2D -> unit` | `Rect3D.iterPointsCW` | `action: (float -> float -> float -> unit) -> r: Rect3D -> unit` |
| `Rect2D.iterPointsLoopedCCW` | `action: (float -> float -> unit) -> r: Rect2D -> unit` | `Rect3D.iterPointsLoopedCCW` | `action: (float -> float -> float -> unit) -> r: Rect3D -> unit` |
| `Rect2D.iterPointsLoopedCW` | `action: (float -> float -> unit) -> r: Rect2D -> unit` | `Rect3D.iterPointsLoopedCW` | `action: (float -> float -> float -> unit) -> r: Rect3D -> unit` |
| `Rect2D.notEqualsWithin` | `tol: float -> a: Rect2D -> b: Rect2D -> bool` | `Rect3D.notEqualsWithin` | `tol: float -> a: Rect3D -> b: Rect3D -> bool` |
| `Rect2D.offset` | `dist: float -> rect: Rect2D -> Rect2D` | `Rect3D.offset` | `dist: float -> rect: Rect3D -> Rect3D` |
| `Rect2D.offsetCorner` | `rect: Rect2D * corner: int * xOffset: float * yOffset: float * xWidth: float * yHeight: float -> Rect2D` | `Rect3D.offsetCorner` | `rect: Rect3D * corner: int * xOffset: float * yOffset: float * xSize: float * ySize: float -> Rect3D` |
| `Rect2D.offsetEdge` | `rect: Rect2D * edgeIdx: int * offEdge: float * width: float * offStart: float * offEnd: float -> Rect2D` | `Rect3D.offsetEdge` | `rect: Rect3D * edgeIdx: int * offEdge: float * width: float * offStart: float * offEnd: float -> Rect3D` |
| `Rect2D.offsetVar` | `dist: float array -> rect: Rect2D -> Rect2D` | `Rect3D.offsetVar` | `dist: float array -> rect: Rect3D -> Rect3D` |
|  |  | `Rect3D.offsetZ` | `offsetDistance: float -> r: Rect3D -> Rect3D` |
|  |  | `Rect3D.pnt0` | `r: Rect3D -> Pnt` |
|  |  | `Rect3D.pnt1` | `r: Rect3D -> Pnt` |
|  |  | `Rect3D.pnt2` | `r: Rect3D -> Pnt` |
|  |  | `Rect3D.pnt3` | `r: Rect3D -> Pnt` |
| `Rect2D.subDivide` | `rect: Rect2D * xCount: int * yCount: int * xGap: float * yGap: float -> Rect2D array array` | `Rect3D.subDivide` | `rect: Rect3D * xCount: int * yCount: int * xGap: float * yGap: float -> Rect3D array array` |
| `Rect2D.subDivideMaxLength` | `rect: Rect2D * xMaxLen: float * yMaxLen: float * xGap: float * yGap: float -> Rect2D array array` | `Rect3D.subDivideMaxLength` | `rect: Rect3D * xMaxLen: float * yMaxLen: float * xGap: float * yGap: float -> Rect3D array array` |
| `Rect2D.subDivideMinLength` | `rect: Rect2D * xMinLen: float * yMinLen: float * xGap: float * yGap: float -> Rect2D array array` | `Rect3D.subDivideMinLength` | `rect: Rect3D * xMinLen: float * yMinLen: float * xGap: float * yGap: float -> Rect3D array array` |
| `Rect2D.translateLocalX` | `distX: float -> r: Rect2D -> Rect2D` | `Rect3D.translateLocalX` | `distX: float -> r: Rect3D -> Rect3D` |
| `Rect2D.translateLocalY` | `distY: float -> r: Rect2D -> Rect2D` | `Rect3D.translateLocalY` | `distY: float -> r: Rect3D -> Rect3D` |
|  |  | `Rect3D.translateLocalZ` | `distZ: float -> r: Rect3D -> Rect3D` |
| `Rect2D.tryClipLine` | `line: Line2D -> rect: Rect2D -> Line2D voption` |  |  |
| `Rect2D.tryCreateFrom3Points` | `origin: Pt * xPt: Pt * yPt: Pt -> Rect2D voption` | `Rect3D.tryCreateFrom3Points` | `origin: Pnt * xPnt: Pnt * yPnt: Pnt -> Rect3D voption` |
| `Rect2D.tryIntersectLine` | `line: Line2D -> rect: Rect2D -> Line2D voption` | `Rect3D.tryIntersectLine` | `ln: Line3D -> r: Rect3D -> Pnt voption` |
| `Rect2D.tryIntersectRay` | `ray: Line2D -> rect: Rect2D -> (float * float) voption` | `Rect3D.tryIntersectRay` | `ln: Line3D -> r: Rect3D -> float voption` |

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
| `.GetLine` | `startPointIndex: int * endPointIndex: int -> Line3D` |
| `Box.getLine` | `startPointIndex: int -> endPointIndex: int -> b: Box -> Line3D` |
| `.GetPnt` | `pointIndex: int -> Pnt` |
| `Box.getPnt` | `pointIndex: int -> b: Box -> Pnt` |
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
| `.Points` | `Pnt array` |
| `Box.points` | `b: Box -> Pnt array` |
| `.Pnt0` | `Pnt` |
| `.Pnt0X` | `float` |
| `.Pnt0Y` | `float` |
| `.Pnt0Z` | `float` |
| `.Pnt1` | `Pnt` |
| `.Pnt1X` | `float` |
| `.Pnt1Y` | `float` |
| `.Pnt1Z` | `float` |
| `.Pnt2` | `Pnt` |
| `.Pnt2X` | `float` |
| `.Pnt2Y` | `float` |
| `.Pnt2Z` | `float` |
| `.Pnt3` | `Pnt` |
| `.Pnt3X` | `float` |
| `.Pnt3Y` | `float` |
| `.Pnt3Z` | `float` |
| `.Pnt4` | `Pnt` |
| `.Pnt4X` | `float` |
| `.Pnt4Y` | `float` |
| `.Pnt4Z` | `float` |
| `.Pnt5` | `Pnt` |
| `.Pnt5X` | `float` |
| `.Pnt5Y` | `float` |
| `.Pnt5Z` | `float` |
| `.Pnt6` | `Pnt` |
| `.Pnt6X` | `float` |
| `.Pnt6Y` | `float` |
| `.Pnt6Z` | `float` |
| `.Pnt7` | `Pnt` |
| `.Pnt7X` | `float` |
| `.Pnt7Y` | `float` |
| `.Pnt7Z` | `float` |
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
| `Box.equalsExact` | `a: Box -> b: Box -> bool` |
| `Box.equalsWithin` | `tol: float -> a: Box -> b: Box -> bool` |
| `Box.expand` | `dist: float -> b: Box -> Box` |
| `Box.expandRel` | `factor: float -> b: Box -> Box` |
| `Box.expandRelXYZ` | `factorX: float -> factorY: float -> factorZ: float -> b: Box -> Box` |
| `Box.expandXYZ` | `distX: float -> distY: float -> distZ: float -> b: Box -> Box` |
| `Box.failIfBoxesNotAlignedTo` | `a: Box -> msg: string -> b: Box -> Box` |
| `Box.getBoxBetween` | `a: Box -> b: Box -> Box` |
| `Box.getBoxesAlignment` | `a: Box -> b: Box -> float` |
| `Box.notEqualsWithin` | `tol: float -> a: Box -> b: Box -> bool` |
| `Box.offset` | `distBottom: float -> distFront: float -> distRight: float -> distBack: float -> distLeft: float -> distTop: float -> b: Box -> Box` |
| `Box.pnt0` | `b: Box -> Pnt` |
| `Box.pnt1` | `b: Box -> Pnt` |
| `Box.pnt2` | `b: Box -> Pnt` |
| `Box.pnt3` | `b: Box -> Pnt` |
| `Box.pnt4` | `b: Box -> Pnt` |
| `Box.pnt5` | `b: Box -> Pnt` |
| `Box.pnt6` | `b: Box -> Pnt` |
| `Box.pnt7` | `b: Box -> Pnt` |
| `Box.translateLocalX` | `distX: float -> b: Box -> Box` |
| `Box.translateLocalY` | `distY: float -> b: Box -> Box` |
| `Box.translateLocalZ` | `distZ: float -> b: Box -> Box` |
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
| `.GetPnt` | `pointIndex: int -> Pnt` |
| `FreeBox.getPnt` | `pointIndex: int -> b: FreeBox -> Pnt` |
| `.GetX` | `pointIndex: int -> float` |
| `FreeBox.getX` | `pointIndex: int -> b: FreeBox -> float` |
| `.GetY` | `pointIndex: int -> float` |
| `FreeBox.getY` | `pointIndex: int -> b: FreeBox -> float` |
| `.GetZ` | `pointIndex: int -> float` |
| `FreeBox.getZ` | `pointIndex: int -> b: FreeBox -> float` |
| `.Move` | `v: Vec -> unit` |
| `FreeBox.move` | `v: Vec -> b: FreeBox -> FreeBox` |
| `.MovePnt` | `pointIndex: int -> v: Vec -> unit` |
| `FreeBox.movePnt` | `pointIndex: int -> v: Vec -> b: FreeBox -> unit` |
| `.MovePntXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> unit` |
| `FreeBox.movePntXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> b: FreeBox -> unit` |
| `.MoveX` | `distance: float -> unit` |
| `FreeBox.moveX` | `distance: float -> b: FreeBox -> FreeBox` |
| `.MoveY` | `distance: float -> unit` |
| `FreeBox.moveY` | `distance: float -> b: FreeBox -> FreeBox` |
| `.MoveZ` | `distance: float -> unit` |
| `FreeBox.moveZ` | `distance: float -> b: FreeBox -> FreeBox` |
| `.Pnt0` | `Pnt with get, set` |
| `.Pnt0X` | `float with get, set` |
| `.Pnt0Y` | `float with get, set` |
| `.Pnt0Z` | `float with get, set` |
| `.Pnt1` | `Pnt with get, set` |
| `.Pnt1X` | `float with get, set` |
| `.Pnt1Y` | `float with get, set` |
| `.Pnt1Z` | `float with get, set` |
| `.Pnt2` | `Pnt with get, set` |
| `.Pnt2X` | `float with get, set` |
| `.Pnt2Y` | `float with get, set` |
| `.Pnt2Z` | `float with get, set` |
| `.Pnt3` | `Pnt with get, set` |
| `.Pnt3X` | `float with get, set` |
| `.Pnt3Y` | `float with get, set` |
| `.Pnt3Z` | `float with get, set` |
| `.Pnt4` | `Pnt with get, set` |
| `.Pnt4X` | `float with get, set` |
| `.Pnt4Y` | `float with get, set` |
| `.Pnt4Z` | `float with get, set` |
| `.Pnt5` | `Pnt with get, set` |
| `.Pnt5X` | `float with get, set` |
| `.Pnt5Y` | `float with get, set` |
| `.Pnt5Z` | `float with get, set` |
| `.Pnt6` | `Pnt with get, set` |
| `.Pnt6X` | `float with get, set` |
| `.Pnt6Y` | `float with get, set` |
| `.Pnt6Z` | `float with get, set` |
| `.Pnt7` | `Pnt with get, set` |
| `.Pnt7X` | `float with get, set` |
| `.Pnt7Y` | `float with get, set` |
| `.Pnt7Z` | `float with get, set` |
| `.Rotate` | `q: Quaternion -> unit` |
| `FreeBox.rotate` | `q: Quaternion -> b: FreeBox -> FreeBox` |
| `.RotateAround` | `cen: Pnt -> q: Quaternion -> unit` |
| `FreeBox.rotateAround` | `cen: Pnt -> q: Quaternion -> b: FreeBox -> FreeBox` |
| `.Scale` | `factor: float -> unit` |
| `FreeBox.scale` | `factor: float -> b: FreeBox -> FreeBox` |
| `.ScaleOn` | `cen: Pnt -> factor: float -> unit` |
| `FreeBox.scaleOn` | `cen: Pnt -> factor: float -> b: FreeBox -> FreeBox` |
| `.SetPnt` | `pointIndex: int -> p: Pnt -> unit` |
| `FreeBox.setPnt` | `pointIndex: int -> p: Pnt -> b: FreeBox -> unit` |
| `.SetPntXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> unit` |
| `FreeBox.setPntXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> b: FreeBox -> unit` |
| `.ToString` | `unit -> string` |
| `.Transform` | `m: Matrix -> unit` |
| `FreeBox.transform` | `m: Matrix -> b: FreeBox -> FreeBox` |
| `.TransformRigid` | `m: RigidMatrix -> unit` |
| `FreeBox.transformRigid` | `m: RigidMatrix -> b: FreeBox -> FreeBox` |
| `.XYZs` | `float array` |
| **Other static members** | |
| `FreeBox.createDirectly` | `xyzs: float array -> FreeBox` |
| `FreeBox.createFromBox` | `box: Box -> FreeBox` |
| `FreeBox.createFromEightPoints` | `pts: Pnt array -> FreeBox` |
| `FreeBox.createFromFour2DPoints` | `zMin: float -> zMax: float -> pts: Pt array -> FreeBox` |
| `FreeBox.createFromFour2DPointsArgs` | `zMin: float -> zMax: float -> a: Pt -> b: Pt -> c: Pt -> d: Pt -> FreeBox` |
| `FreeBox.getXYZs` | `b: FreeBox -> float array` |
| `FreeBox.pnt0` | `b: FreeBox -> Pnt` |
| `FreeBox.pnt0X` | `b: FreeBox -> float` |
| `FreeBox.pnt0Y` | `b: FreeBox -> float` |
| `FreeBox.pnt0Z` | `b: FreeBox -> float` |
| `FreeBox.pnt1` | `b: FreeBox -> Pnt` |
| `FreeBox.pnt1X` | `b: FreeBox -> float` |
| `FreeBox.pnt1Y` | `b: FreeBox -> float` |
| `FreeBox.pnt1Z` | `b: FreeBox -> float` |
| `FreeBox.pnt2` | `b: FreeBox -> Pnt` |
| `FreeBox.pnt2X` | `b: FreeBox -> float` |
| `FreeBox.pnt2Y` | `b: FreeBox -> float` |
| `FreeBox.pnt2Z` | `b: FreeBox -> float` |
| `FreeBox.pnt3` | `b: FreeBox -> Pnt` |
| `FreeBox.pnt3X` | `b: FreeBox -> float` |
| `FreeBox.pnt3Y` | `b: FreeBox -> float` |
| `FreeBox.pnt3Z` | `b: FreeBox -> float` |
| `FreeBox.pnt4` | `b: FreeBox -> Pnt` |
| `FreeBox.pnt4X` | `b: FreeBox -> float` |
| `FreeBox.pnt4Y` | `b: FreeBox -> float` |
| `FreeBox.pnt4Z` | `b: FreeBox -> float` |
| `FreeBox.pnt5` | `b: FreeBox -> Pnt` |
| `FreeBox.pnt5X` | `b: FreeBox -> float` |
| `FreeBox.pnt5Y` | `b: FreeBox -> float` |
| `FreeBox.pnt5Z` | `b: FreeBox -> float` |
| `FreeBox.pnt6` | `b: FreeBox -> Pnt` |
| `FreeBox.pnt6X` | `b: FreeBox -> float` |
| `FreeBox.pnt6Y` | `b: FreeBox -> float` |
| `FreeBox.pnt6Z` | `b: FreeBox -> float` |
| `FreeBox.pnt7` | `b: FreeBox -> Pnt` |
| `FreeBox.pnt7X` | `b: FreeBox -> float` |
| `FreeBox.pnt7Y` | `b: FreeBox -> float` |
| `FreeBox.pnt7Z` | `b: FreeBox -> float` |
| `FreeBox.translate` | `v: Vec -> b: FreeBox -> FreeBox` |

## BRect / BBox

| BRect | Signature | BBox | Signature |
|:--|:--|:--|:--|
| `.MinX` | `float` | `.MinX` | `float` |
| `.MinY` | `float` | `.MinY` | `float` |
| `.MaxX` | `float` | `.MaxX` | `float` |
| `.MaxY` | `float` | `.MaxY` | `float` |
|  |  | `.MinZ` | `float` |
|  |  | `.MaxZ` | `float` |
| `.Area` | `float` |  |  |
| `BRect.area` | `r: BRect -> float` |  |  |
|  |  | `.AsBox` | `Box` |
|  |  | `BBox.asBox` | `b: BBox -> Box` |
|  |  | `.AsBRect` | `BRect` |
|  |  | `BBox.asBRect` | `b: BBox -> BRect` |
| `.AsFSharpCode` | `string` | `.AsFSharpCode` | `string` |
| `BRect.asFSharpCode` | `r: BRect -> string` | `BBox.asFSharpCode` | `b: BBox -> string` |
| `.AsRect2D` | `Rect2D` |  |  |
| `.AsString` | `string` | `.AsString` | `string` |
| `BRect.asString` | `r: BRect -> string` | `BBox.asString` | `b: BBox -> string` |
|  |  | `.BackFace` | `Rect3D` |
|  |  | `BBox.backFace` | `b: BBox -> Rect3D` |
|  |  | `.BottomFace` | `Rect3D` |
|  |  | `BBox.bottomFace` | `b: BBox -> Rect3D` |
|  |  | `.BottomPoints` | `Pnt array` |
|  |  | `BBox.bottomPoints` | `b: BBox -> Pnt array` |
|  |  | `.BottomPointsLooped` | `Pnt array` |
|  |  | `BBox.bottomPointsLooped` | `b: BBox -> Pnt array` |
| `.Center` | `Pt` | `.Center` | `Pnt` |
| `BRect.center` | `r: BRect -> Pt` | `BBox.center` | `b: BBox -> Pnt` |
| `.Contains` | `o: BRect -> bool` | `.Contains` | `o: BBox -> bool` |
| `BRect.contains` | `rectInside: BRect -> surroundingRect: BRect -> bool` | `BBox.contains` | `boxInside: BBox -> surroundingBox: BBox -> bool` |
| `.ContainsPt` | `p: Pt -> bool` | `.ContainsPnt` | `p: Pnt -> bool` |
| `BRect.containsPt` | `pt: Pt -> rect: BRect -> bool` | `BBox.containsPnt` | `pnt: Pnt -> box: BBox -> bool` |
| `.ContainsXY` | `x: float * y: float -> bool` |  |  |
| `BRect.containsXY` | `x: float -> y: float -> rect: BRect -> bool` |  |  |
|  |  | `.ContainsXYZ` | `x: float * y: float * z: float -> bool` |
|  |  | `BBox.containsXYZ` | `x: float -> y: float -> z: float -> box: BBox -> bool` |
| `.CountZeroSides` | `int` | `.CountZeroSides` | `int` |
| `BRect.countZeroSides` | `r: BRect -> int` | `BBox.countZeroSides` | `b: BBox -> int` |
| `.Diagonal` | `Vc` | `.Diagonal` | `Vec` |
| `BRect.diagonal` | `r: BRect -> Vc` | `BBox.diagonal` | `b: BBox -> Vec` |
|  |  | `.DoesIntersectPlane` | `pl: NPlane -> bool` |
|  |  | `BBox.doesIntersectPlane` | `pl: NPlane -> b: BBox -> bool` |
| `.Edge01` | `Line2D` | `.Edge01` | `Line3D` |
| `BRect.edge01` | `r: BRect -> Line2D` | `BBox.edge01` | `b: BBox -> Line3D` |
| `.Edge03` | `Line2D` | `.Edge03` | `Line3D` |
| `BRect.edge03` | `r: BRect -> Line2D` | `BBox.edge03` | `b: BBox -> Line3D` |
|  |  | `.Edge04` | `Line3D` |
|  |  | `BBox.edge04` | `b: BBox -> Line3D` |
| `.Edge10` | `Line2D` | `.Edge10` | `Line3D` |
| `BRect.edge10` | `r: BRect -> Line2D` | `BBox.edge10` | `b: BBox -> Line3D` |
| `.Edge12` | `Line2D` | `.Edge12` | `Line3D` |
| `BRect.edge12` | `r: BRect -> Line2D` | `BBox.edge12` | `b: BBox -> Line3D` |
|  |  | `.Edge15` | `Line3D` |
|  |  | `BBox.edge15` | `b: BBox -> Line3D` |
| `.Edge21` | `Line2D` | `.Edge21` | `Line3D` |
| `BRect.edge21` | `r: BRect -> Line2D` | `BBox.edge21` | `b: BBox -> Line3D` |
| `.Edge23` | `Line2D` | `.Edge23` | `Line3D` |
| `BRect.edge23` | `r: BRect -> Line2D` | `BBox.edge23` | `b: BBox -> Line3D` |
|  |  | `.Edge26` | `Line3D` |
|  |  | `BBox.edge26` | `b: BBox -> Line3D` |
| `.Edge30` | `Line2D` | `.Edge30` | `Line3D` |
| `BRect.edge30` | `r: BRect -> Line2D` | `BBox.edge30` | `b: BBox -> Line3D` |
| `.Edge32` | `Line2D` | `.Edge32` | `Line3D` |
| `BRect.edge32` | `r: BRect -> Line2D` | `BBox.edge32` | `b: BBox -> Line3D` |
|  |  | `.Edge37` | `Line3D` |
|  |  | `BBox.edge37` | `b: BBox -> Line3D` |
|  |  | `.Edge40` | `Line3D` |
|  |  | `BBox.edge40` | `b: BBox -> Line3D` |
|  |  | `.Edge45` | `Line3D` |
|  |  | `BBox.edge45` | `b: BBox -> Line3D` |
|  |  | `.Edge47` | `Line3D` |
|  |  | `BBox.edge47` | `b: BBox -> Line3D` |
|  |  | `.Edge51` | `Line3D` |
|  |  | `BBox.edge51` | `b: BBox -> Line3D` |
|  |  | `.Edge54` | `Line3D` |
|  |  | `BBox.edge54` | `b: BBox -> Line3D` |
|  |  | `.Edge56` | `Line3D` |
|  |  | `BBox.edge56` | `b: BBox -> Line3D` |
|  |  | `.Edge62` | `Line3D` |
|  |  | `BBox.edge62` | `b: BBox -> Line3D` |
|  |  | `.Edge65` | `Line3D` |
|  |  | `BBox.edge65` | `b: BBox -> Line3D` |
|  |  | `.Edge67` | `Line3D` |
|  |  | `BBox.edge67` | `b: BBox -> Line3D` |
|  |  | `.Edge73` | `Line3D` |
|  |  | `BBox.edge73` | `b: BBox -> Line3D` |
|  |  | `.Edge74` | `Line3D` |
|  |  | `BBox.edge74` | `b: BBox -> Line3D` |
|  |  | `.Edge76` | `Line3D` |
|  |  | `BBox.edge76` | `b: BBox -> Line3D` |
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
| `BRect.expandSafeXY` | `xDist: float -> yDist: float -> r: BRect -> BRect` |  |  |
| `.ExpandXaxis` | `startDist: float * endDist: float -> BRect` | `.ExpandXaxis` | `startDist: float * endDist: float -> BBox` |
| `BRect.expandXaxis` | `startDist: float -> endDist: float -> r: BRect -> BRect` | `BBox.expandXaxis` | `startDist: float -> endDist: float -> b: BBox -> BBox` |
| `.ExpandXY` | `xDist: float * yDist: float -> BRect` |  |  |
| `BRect.expandXY` | `xDist: float -> yDist: float -> r: BRect -> BRect` |  |  |
| `.ExpandYaxis` | `startDist: float * endDist: float -> BRect` | `.ExpandYaxis` | `startDist: float * endDist: float -> BBox` |
| `BRect.expandYaxis` | `startDist: float -> endDist: float -> r: BRect -> BRect` | `BBox.expandYaxis` | `startDist: float -> endDist: float -> b: BBox -> BBox` |
|  |  | `.ExpandZaxis` | `startDist: float * endDist: float -> BBox` |
|  |  | `BBox.expandZaxis` | `startDist: float -> endDist: float -> b: BBox -> BBox` |
|  |  | `.Faces` | `Rect3D array` |
|  |  | `BBox.faces` | `b: BBox -> Rect3D array` |
|  |  | `.FrontFace` | `Rect3D` |
|  |  | `BBox.frontFace` | `b: BBox -> Rect3D` |
|  |  | `.GetEdge` | `edgeIndex: int -> Line3D` |
|  |  | `BBox.getEdge` | `edgeIndex: int -> b: BBox -> Line3D` |
|  |  | `.GetFace` | `faceIndex: int -> Rect3D` |
|  |  | `BBox.getFace` | `faceIndex: int -> b: BBox -> Rect3D` |
|  |  | `.GetLine` | `startPointIndex: int * endPointIndex: int -> Line3D` |
|  |  | `BBox.getLine` | `startPointIndex: int -> endPointIndex: int -> b: BBox -> Line3D` |
|  |  | `.GetPnt` | `pointIndex: int -> Pnt` |
|  |  | `BBox.getPnt` | `pointIndex: int -> b: BBox -> Pnt` |
| `.HasArea` | `bool` |  |  |
| `BRect.hasArea` | `r: BRect -> bool` |  |  |
|  |  | `.HasVolume` | `bool` |
|  |  | `BBox.hasVolume` | `b: BBox -> bool` |
| `.Intersection` | `a: BRect -> BRect` | `.Intersection` | `a: BBox -> BBox` |
| `BRect.intersection` | `a: BRect -> b: BRect -> BRect` | `BBox.intersection` | `a: BBox -> b: BBox -> BBox` |
|  |  | `.IntersectPlane` | `pl: NPlane -> ResizeArray<Pnt>` |
|  |  | `BBox.intersectPlane` | `pl: NPlane -> b: BBox -> ResizeArray<Pnt>` |
| `.IntersectRay` | `ray: Line2D -> float * float` | `.IntersectRay` | `ray: Line3D -> float * float` |
| `BRect.intersectRay` | `ray: Line2D -> r: BRect -> float * float` | `BBox.intersectRay` | `ray: Line3D -> b: BBox -> float * float` |
|  |  | `.IsFlat` | `bool` |
|  |  | `BBox.isFlat` | `b: BBox -> bool` |
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
|  |  | `.LeftFace` | `Rect3D` |
|  |  | `BBox.leftFace` | `b: BBox -> Rect3D` |
| `.LongestEdge` | `float` | `.LongestEdge` | `float` |
| `BRect.longestEdge` | `r: BRect -> float` | `BBox.longestEdge` | `b: BBox -> float` |
| `.MaxPt` | `Pt` | `.MaxPnt` | `Pnt` |
| `BRect.maxPt` | `r: BRect -> Pt` | `BBox.maxPnt` | `b: BBox -> Pnt` |
| `.MinPt` | `Pt` | `.MinPnt` | `Pnt` |
| `BRect.minPt` | `r: BRect -> Pt` | `BBox.minPnt` | `b: BBox -> Pnt` |
| `.Points` | `Pt array` | `.Points` | `Pnt array` |
| `BRect.points` | `r: BRect -> Pt array` | `BBox.points` | `b: BBox -> Pnt array` |
| `.PointsLooped` | `Pt array` |  |  |
| `BRect.pointsLooped` | `r: BRect -> Pt array` |  |  |
| `.Pt0` | `Pt` | `.Pnt0` | `Pnt` |
| `BRect.pt0` | `r: BRect -> Pt` |  |  |
| `.Pt1` | `Pt` | `.Pnt1` | `Pnt` |
| `BRect.pt1` | `r: BRect -> Pt` |  |  |
| `.Pt2` | `Pt` | `.Pnt2` | `Pnt` |
| `BRect.pt2` | `r: BRect -> Pt` |  |  |
| `.Pt3` | `Pt` | `.Pnt3` | `Pnt` |
| `BRect.pt3` | `r: BRect -> Pt` |  |  |
|  |  | `.Pnt4` | `Pnt` |
|  |  | `.Pnt5` | `Pnt` |
|  |  | `.Pnt6` | `Pnt` |
|  |  | `.Pnt7` | `Pnt` |
|  |  | `.RightFace` | `Rect3D` |
|  |  | `BBox.rightFace` | `b: BBox -> Rect3D` |
| `.ShortestEdge` | `float` | `.ShortestEdge` | `float` |
| `BRect.shortestEdge` | `r: BRect -> float` | `BBox.shortestEdge` | `b: BBox -> float` |
| `.SizeX` | `float` | `.SizeX` | `float` |
| `BRect.sizeX` | `r: BRect -> float` | `BBox.sizeX` | `b: BBox -> float` |
| `.SizeY` | `float` | `.SizeY` | `float` |
| `BRect.sizeY` | `r: BRect -> float` | `BBox.sizeY` | `b: BBox -> float` |
|  |  | `.SizeZ` | `float` |
|  |  | `BBox.sizeZ` | `b: BBox -> float` |
|  |  | `.TopFace` | `Rect3D` |
|  |  | `BBox.topFace` | `b: BBox -> Rect3D` |
|  |  | `.TopPoints` | `Pnt array` |
|  |  | `BBox.topPoints` | `b: BBox -> Pnt array` |
|  |  | `.TopPointsLooped` | `Pnt array` |
|  |  | `BBox.topPointsLooped` | `b: BBox -> Pnt array` |
| `.ToString` | `unit -> string` | `.ToString` | `unit -> string` |
| `.TouchingSide` | `other: BRect * ?tolerance: float -> int` | `.TouchingSide` | `other: BBox * ?tolerance: float -> int` |
| `BRect.touchingSide` | `other: BRect -> tolerance: float -> this: BRect -> int` | `BBox.touchingSide` | `tolerance: float -> other: BBox -> thisBox: BBox -> int` |
| `.TryIntersection` | `a: BRect -> BRect voption` | `.TryIntersection` | `a: BBox -> BBox voption` |
| `BRect.tryIntersection` | `a: BRect -> b: BRect -> BRect voption` | `BBox.tryIntersection` | `a: BBox -> b: BBox -> BBox voption` |
| `.TryIntersectRay` | `ray: Line2D -> (float * float) voption` | `.TryIntersectRay` | `ray: Line3D -> (float * float) voption` |
| `BRect.tryIntersectRay` | `ray: Line2D -> r: BRect -> (float * float) voption` | `BBox.tryIntersectRay` | `ray: Line3D -> b: BBox -> (float * float) voption` |
| `.Union` | `b: BRect -> BRect` | `.Union` | `a: BBox -> BBox` |
| `BRect.union` | `a: BRect -> b: BRect -> BRect` | `BBox.union` | `a: BBox -> b: BBox -> BBox` |
| `.UnionPt` | `p: Pt -> BRect` | `.UnionPnt` | `p: Pnt -> BBox` |
| `BRect.unionPt` | `p: Pt -> r: BRect -> BRect` | `BBox.unionPnt` | `p: Pnt -> b: BBox -> BBox` |
|  |  | `.Volume` | `float` |
|  |  | `BBox.volume` | `b: BBox -> float` |
| **Other static members** | | | |
| `BRect.create` | `a: Pt * b: Pt -> BRect` | `BBox.create` | `a: Pnt * b: Pnt -> BBox` |
|  |  | `BBox.createFromBox` | `b: Box -> BBox` |
|  |  | `BBox.createFromBRect` | `minZ: float -> maxZ: float -> r: BRect -> BBox` |
| `BRect.createFromCenter` | `center: Pt * sizeX: float * sizeY: float -> BRect` | `BBox.createFromCenter` | `center: Pnt * sizeX: float * sizeY: float * sizeZ: float -> BBox` |
| `BRect.createFromIList` | `ps: IList<Pt> -> BRect` | `BBox.createFromIList` | `ps: IList<Pnt> -> BBox` |
| `BRect.createFromLine` | `l: Line2D -> BRect` | `BBox.createFromLine` | `l: Line3D -> BBox` |
| `BRect.createFromRect2D` | `r: Rect2D -> BRect` |  |  |
| `BRect.createFromRect3D` | `r: Rect3D -> BRect` | `BBox.createFromRect3D` | `r: Rect3D -> BBox` |
| `BRect.createFromSeq` | `ps: Pt seq -> BRect` | `BBox.createFromSeq` | `ps: Pnt seq -> BBox` |
| `BRect.createUnchecked` | `minX: float * minY: float * maxX: float * maxY: float -> BRect` | `BBox.createUnchecked` | `minX: float * minY: float * minZ: float * maxX: float * maxY: float * maxZ: float -> BBox` |
| `BRect.createXY` | `ptAx: float * ptAy: float * ptBx: float * ptBy: float -> BRect` |  |  |
| `BRect.equalsExact` | `a: BRect -> b: BRect -> bool` | `BBox.equalsExact` | `a: BBox -> b: BBox -> bool` |
| `BRect.equalsWithin` | `tol: float -> a: BRect -> b: BRect -> bool` | `BBox.equalsWithin` | `tol: float -> a: BBox -> b: BBox -> bool` |
| `BRect.expandRel` | `factor: float -> r: BRect -> BRect` | `BBox.expandRel` | `factor: float -> b: BBox -> BBox` |
| `BRect.expandRelXY` | `factorX: float -> factorY: float -> r: BRect -> BRect` |  |  |
|  |  | `BBox.expandRelXYZ` | `factorX: float -> factorY: float -> factorZ: float -> b: BBox -> BBox` |
|  |  | `BBox.expandSafeXYZ` | `xDist: float -> yDist: float -> zDist: float -> b: BBox -> BBox` |
|  |  | `BBox.expandXYZ` | `xDist: float -> yDist: float -> zDist: float -> b: BBox -> BBox` |
| `BRect.move` | `v: Vc -> r: BRect -> BRect` | `BBox.move` | `v: Vec -> b: BBox -> BBox` |
| `BRect.moveX` | `translation: float -> r: BRect -> BRect` | `BBox.moveX` | `translation: float -> b: BBox -> BBox` |
| `BRect.moveY` | `translation: float -> r: BRect -> BRect` | `BBox.moveY` | `translation: float -> b: BBox -> BBox` |
|  |  | `BBox.moveZ` | `translation: float -> b: BBox -> BBox` |
| `BRect.notEqualsWithin` | `tol: float -> a: BRect -> b: BRect -> bool` | `BBox.notEqualsWithin` | `tol: float -> a: BBox -> b: BBox -> bool` |
|  |  | `BBox.pnt0` | `b: BBox -> Pnt` |
|  |  | `BBox.pnt1` | `b: BBox -> Pnt` |
|  |  | `BBox.pnt2` | `b: BBox -> Pnt` |
|  |  | `BBox.pnt3` | `b: BBox -> Pnt` |
|  |  | `BBox.pnt4` | `b: BBox -> Pnt` |
|  |  | `BBox.pnt5` | `b: BBox -> Pnt` |
|  |  | `BBox.pnt6` | `b: BBox -> Pnt` |
|  |  | `BBox.pnt7` | `b: BBox -> Pnt` |
| `BRect.scale` | `factor: float -> r: BRect -> BRect` | `BBox.scale` | `factor: float -> b: BBox -> BBox` |
| `BRect.translate` | `v: Vc -> r: BRect -> BRect` | `BBox.translate` | `v: Vec -> b: BBox -> BBox` |

## Tria2D / Tria3D

| Tria2D | Signature | Tria3D | Signature |
|:--|:--|:--|:--|
| `Tria2D.area` | `a: Pt * b: Pt * c: Pt -> float` | `Tria3D.area` | `a: Pnt * b: Pnt * c: Pnt -> float` |
| `Tria2D.areaDouble` | `a: Pt * b: Pt * c: Pt -> float` | `Tria3D.areaDouble` | `a: Pnt * b: Pnt * c: Pnt -> float` |
| `Tria2D.areaSigned` | `a: Pt * b: Pt * c: Pt -> float` |  |  |
| `Tria2D.det` | `a: Pt * b: Pt * c: Pt -> float` |  |  |
|  |  | `Tria3D.intersectLine` | `line: Line3D * p1: Pnt * p2: Pnt * p3: Pnt -> Pnt` |
|  |  | `Tria3D.intersectRay` | `line: Line3D * p1: Pnt * p2: Pnt * p3: Pnt -> float` |
|  |  | `Tria3D.intersectRayXYZ` | `lnFromX: float * lnFromY: float * lnFromZ: float * lnToX: float * lnToY: float * lnToZ: float * ptAx: float * ptAy: float * ptAz: float * ptBx: float * ptBy: float * ptBz: float * ptCx: float * ptCy: float * ptCz: float -> float` |
|  |  | `Tria3D.intersectTriaVectorsWithRay` | `lnFromX: float * lnFromY: float * lnFromZ: float * dirX: float * dirY: float * dirZ: float * ptAx: float * ptAy: float * ptAz: float * e1x: float * e1y: float * e1z: float * e2x: float * e2y: float * e2z: float -> float` |
| `Tria2D.isLinear` | `a: Pt * b: Pt * c: Pt * ?distanceTolerance: float -> bool` | `Tria3D.isLinear` | `a: Pnt * b: Pnt * c: Pnt * ?distanceTolerance: float -> bool` |
| `Tria2D.isLinearFast` | `a: Pt * b: Pt * c: Pt * ?maxAreaParallelogram: float -> bool` | `Tria3D.isLinearFast` | `a: Pnt * b: Pnt * c: Pnt * ?maxAreaParallelogram: float -> bool` |
| `Tria2D.offset` | `a: Pt * b: Pt * c: Pt * dist: float -> Pt * Pt * Pt` | `Tria3D.offset` | `a: Pnt * b: Pnt * c: Pnt * dist: float -> Pnt * Pnt * Pnt` |
| `Tria2D.offsetPt` | `ptToOffset: Pt * prev: Pt * next: Pt * dist: float -> Pt` | `Tria3D.offsetPnt` | `pntToOffset: Pnt * prev: Pnt * next: Pnt * dist: float -> Pnt` |
| `Tria2D.offsetPtVar` | `ptToOffset: Pt * prev: Pt * next: Pt * distPrev: float * distNext: float -> Pt` |  |  |
| `Tria2D.offsetPtVarByNormals` | `ptToOffset: Pt * prevN: UnitVc * nextN: UnitVc * prevDist: float * nextDist: float -> Pt` |  |  |
|  |  | `Tria3D.offsetVar` | `pntToOffset: Pnt * prev: Pnt * next: Pnt * distPrev: float * distNext: float -> Pnt` |
|  |  | `Tria3D.tryIntersectLine` | `line: Line3D * p1: Pnt * p2: Pnt * p3: Pnt -> Pnt voption` |
|  |  | `Tria3D.tryIntersectRay` | `line: Line3D * p1: Pnt * p2: Pnt * p3: Pnt -> float voption` |
|  |  | `Tria3D.tryIntersectRayXYZ` | `lnFromX: float * lnFromY: float * lnFromZ: float * lnToX: float * lnToY: float * lnToZ: float * ptAx: float * ptAy: float * ptAz: float * ptBx: float * ptBy: float * ptBz: float * ptCx: float * ptCy: float * ptCz: float -> float voption` |
|  |  | `Tria3D.tryIntersectTriaVectorsWithRay` | `lnFromX: float * lnFromY: float * lnFromZ: float * dirX: float * dirY: float * dirZ: float * ptAx: float * ptAy: float * ptAz: float * e1x: float * e1y: float * e1z: float * e2x: float * e2y: float * e2z: float -> float voption` |
| `Tria2D.tryOffsetPtVar` | `ptToOffset: Pt * prev: Pt * next: Pt * distPrev: float * distNext: float -> Pt voption` |  |  |
| `Tria2D.tryOffsetPtVarByNormals` | `ptToOffset: Pt * prevN: UnitVc * nextN: UnitVc * prevDist: float * nextDist: float -> Pt voption` |  |  |
|  |  | `Tria3D.tryOffsetVar` | `pntToOffset: Pnt * prev: Pnt * next: Pnt * distPrev: float * distNext: float -> Pnt voption` |

## Points2D / Points3D

| Points2D | Signature | Points3D | Signature |
|:--|:--|:--|:--|
| `Points2D.center` | `pts: IList<Pt> -> Pt` | `Points3D.center` | `pts: IList<Pnt> -> Pnt` |
| `Points2D.closestOfTwo` | `pt1: Pt -> pt2: Pt -> referencePoint: Pt -> Pt` | `Points3D.closestOfTwo` | `pnt1: Pnt -> pnt2: Pnt -> referencePoint: Pnt -> Pnt` |
| `Points2D.closestPoint` | `pts: IList<Pt> * pt: Pt -> Pt` | `Points3D.closestPoint` | `pts: IList<Pnt> * pnt: Pnt -> Pnt` |
| `Points2D.closestPointIdx` | `pts: IList<Pt> * pt: Pt -> int` | `Points3D.closestPointIdx` | `pts: IList<Pnt> * pnt: Pnt -> int` |
| `Points2D.closestPointsIdx` | `xs: IList<Pt> * ys: IList<Pt> -> int * int` | `Points3D.closestPointsIdx` | `xs: IList<Pnt> * ys: IList<Pnt> -> int * int` |
| `Points2D.getSignedArea` | `ps: IList<Pt> -> float` |  |  |
| `Points2D.minDistBetweenPointSets` | `xs: IList<Pt> * ys: IList<Pt> -> float` | `Points3D.minDistBetweenPointSets` | `xs: IList<Pnt> * ys: IList<Pnt> -> float` |
| `Points2D.mostDistantPoint` | `findPointFrom: IList<Pt> * checkAgainst: IList<Pt> -> Pt` | `Points3D.mostDistantPoint` | `findPointFrom: IList<Pnt> * checkAgainst: IList<Pnt> -> Pnt` |
| `Points2D.mostDistantPointIdx` | `findPointFrom: IList<Pt> * checkAgainst: IList<Pt> -> int * int` | `Points3D.mostDistantPointIdx` | `findPointFrom: IList<Pnt> * checkAgainst: IList<Pnt> -> int * int` |
|  |  | `Points3D.normalOfPoints` | `pts: IList<Pnt> -> Vec` |

## module Offset2D / module Offset3D

| Offset2D | Signature | Offset3D | Signature |
|:--|:--|:--|:--|
|  |  | `Offset3D.getCollinearNeighbors` | `dirs: ResizeArray<OffsetDirection voption> -> ResizeArray<CollinearPoint>` |
|  |  | `Offset3D.getOffsetDirections` | `uvs: ResizeArray<float> * refNormal: UnitVec * isOpen: bool * considerCollinearBelow: float<cosine> * failAtUTurnAbove: float<cosine> -> ResizeArray<OffsetDirection voption>` |
|  |  | `Offset3D.getSegmentUnitVectors` | `xyzs: ResizeArray<float> -> ResizeArray<float>` |
| `Offset2D.makeOffsetDirections` | `xys: ResizeArray<float> -> ResizeArray<float>` |  |  |
| `Offset2D.makeUnitTangents` | `xys: ResizeArray<float> -> ResizeArray<float>` |  |  |
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
|  |  | `Offset3D.setOffCornerVar` | `res: ResizeArray<float> * x: float * y: float * z: float * distInPlanePrev: float * distInPlaneNext: float * distPerp: float * nPrev: UnitVec * nNext: UnitVec * perpDir: UnitVec * cosine: float * vNext: UnitVec -> unit` |
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
| `StepWithTwoPoints` | `VarDistParallel = 5` |  |  |

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
| `.AddPoint` | `pt: Pt -> unit` | `.AddPoint` | `pnt: Pnt -> unit` |
| `Polyline2D.addPoint` | `pt: Pt -> p: Polyline2D -> unit` | `Polyline3D.addPoint` | `pnt: Pnt -> p: Polyline3D -> unit` |
| `.AddXY` | `x: float * y: float -> unit` |  |  |
| `Polyline2D.addXY` | `x: float -> y: float -> p: Polyline2D -> unit` |  |  |
|  |  | `.AddXYZ` | `x: float * y: float * z: float -> unit` |
|  |  | `Polyline3D.addXYZ` | `x: float -> y: float -> z: float -> p: Polyline3D -> unit` |
| `.Area` | `float` |  |  |
| `Polyline2D.area` | `p: Polyline2D -> float` |  |  |
|  |  | `.AreaIn2D` | `float` |
|  |  | `Polyline3D.areaIn2D` | `pl: Polyline3D -> float` |
| `.AsFSharpCode` | `string` | `.AsFSharpCode` | `string` |
| `Polyline2D.asFSharpCode` | `p: Polyline2D -> string` | `Polyline3D.asFSharpCode` | `pl: Polyline3D -> string` |
| `.AsPoints` | `ResizeArray<Pt>` | `.AsPoints` | `ResizeArray<Pnt>` |
| `Polyline2D.asPoints` | `p: Polyline2D -> ResizeArray<Pt>` | `Polyline3D.asPoints` | `p: Polyline3D -> ResizeArray<Pnt>` |
| `.AsString` | `string` | `.AsString` | `string` |
| `Polyline2D.asString` | `p: Polyline2D -> string` | `Polyline3D.asString` | `pl: Polyline3D -> string` |
|  |  | `.AverageNormal` | `Vec` |
|  |  | `Polyline3D.averageNormal` | `pl: Polyline3D -> Vec` |
|  |  | `.AveragePlane` | `?tolerance: float -> NPlane` |
|  |  | `Polyline3D.averagePlane` | `tolerance: float -> pl: Polyline3D -> NPlane` |
|  |  | `.BoundingBox` | `BBox` |
|  |  | `Polyline3D.boundingBox` | `pl: Polyline3D -> BBox` |
| `.BoundingRectangle` | `BRect` |  |  |
| `Polyline2D.boundingRectangle` | `p: Polyline2D -> BRect` |  |  |
| `.Center` | `Pt` | `.Center` | `Pnt` |
| `Polyline2D.center` | `p: Polyline2D -> Pt` | `Polyline3D.center` | `pl: Polyline3D -> Pnt` |
| `.Clone` | `unit -> Polyline2D` | `.Clone` | `unit -> Polyline3D` |
| `Polyline2D.clone` | `p: Polyline2D -> Polyline2D` | `Polyline3D.clone` | `pl: Polyline3D -> Polyline3D` |
| `.Close` | `?toleranceForAddingPoint: float -> unit` | `.Close` | `?toleranceForAddingPoint: float -> unit` |
| `Polyline2D.close` | `toleranceForAddingPoint: float -> pl: Polyline2D -> Polyline2D` | `Polyline3D.close` | `toleranceForAddingPoint: float -> pl: Polyline3D -> Polyline3D` |
| `.ClosestParameter` | `p: Pt -> float` | `.ClosestParameter` | `p: Pnt -> float` |
| `Polyline2D.closestParameter` | `pt: Pt -> pl: Polyline2D -> float` | `Polyline3D.closestParameter` | `pnt: Pnt -> pl: Polyline3D -> float` |
| `.ClosestParameterXY` | `x: float * y: float -> float` |  |  |
| `Polyline2D.closestParameterXY` | `x: float -> y: float -> pl: Polyline2D -> float` |  |  |
|  |  | `.ClosestParameterXYZ` | `x: float * y: float * z: float -> float` |
|  |  | `Polyline3D.closestParameterXYZ` | `x: float -> y: float -> z: float -> pl: Polyline3D -> float` |
| `.ClosestPoint` | `p: Pt -> Pt` | `.ClosestPoint` | `p: Pnt -> Pnt` |
| `Polyline2D.closestPoint` | `pt: Pt -> pl: Polyline2D -> Pt` | `Polyline3D.closestPoint` | `pnt: Pnt -> pl: Polyline3D -> Pnt` |
| `.ClosestPointIndex` | `p: Pt -> int` | `.ClosestPointIndex` | `p: Pnt -> int` |
| `Polyline2D.closestPointIndex` | `pt: Pt -> pl: Polyline2D -> int` | `Polyline3D.closestPointIndex` | `pnt: Pnt -> pl: Polyline3D -> int` |
| `.Contains` | `pt: Pt -> bool` |  |  |
| `Polyline2D.contains` | `pt: Pt -> pl: Polyline2D -> bool` |  |  |
| `.ContainsXY` | `x: float * y: float -> bool` |  |  |
| `Polyline2D.containsXY` | `x: float -> y: float -> pl: Polyline2D -> bool` |  |  |
| `.DistanceTo` | `p: Pt -> float` | `.DistanceTo` | `p: Pnt -> float` |
| `Polyline2D.distanceTo` | `pt: Pt -> pl: Polyline2D -> float` | `Polyline3D.distanceTo` | `pnt: Pnt -> pl: Polyline3D -> float` |
| `.DistanceToXY` | `x: float * y: float -> float` |  |  |
| `Polyline2D.distanceToXY` | `x: float -> y: float -> pl: Polyline2D -> float` |  |  |
|  |  | `.DistanceToXYZ` | `x: float * y: float * z: float -> float` |
|  |  | `Polyline3D.distanceToXYZ` | `x: float -> y: float -> z: float -> pl: Polyline3D -> float` |
| `.Duplicate` | `unit -> Polyline2D` | `.Duplicate` | `unit -> Polyline3D` |
| `Polyline2D.duplicate` | `p: Polyline2D -> Polyline2D` | `Polyline3D.duplicate` | `pl: Polyline3D -> Polyline3D` |
| `.End` | `Pt with get, set` | `.End` | `Pnt with get, set` |
| `.EnsureClockwise` | `unit -> unit` |  |  |
| `Polyline2D.ensureClockwise` | `pl: Polyline2D -> Polyline2D` |  |  |
|  |  | `.EnsureClockwiseIn2D` | `unit -> unit` |
|  |  | `Polyline3D.ensureClockwiseIn2D` | `pl: Polyline3D -> Polyline3D` |
| `.EnsureCounterClockwise` | `unit -> unit` |  |  |
| `Polyline2D.ensureCounterClockwise` | `pl: Polyline2D -> Polyline2D` |  |  |
|  |  | `.EnsureCounterClockwiseIn2D` | `unit -> unit` |
|  |  | `Polyline3D.ensureCounterClockwiseIn2D` | `pl: Polyline3D -> Polyline3D` |
| `.EvaluateAt` | `t: float -> Pt` | `.EvaluateAt` | `t: float -> Pnt` |
| `Polyline2D.evaluateAt` | `t: float -> pl: Polyline2D -> Pt` | `Polyline3D.evaluateAt` | `t: float -> pl: Polyline3D -> Pnt` |
| `.FindLabelPoint` | `precision: float -> Pt * float` |  |  |
| `Polyline2D.findLabelPoint` | `precision: float -> pl: Polyline2D -> Pt * float` |  |  |
| `.FirstPoint` | `Pt with get, set` | `.FirstPoint` | `Pnt with get, set` |
| `Polyline2D.firstPoint` | `p: Polyline2D -> Pt` | `Polyline3D.firstPoint` | `pl: Polyline3D -> Pnt` |
| `.FirstSegment` | `Line2D` | `.FirstSegment` | `Line3D` |
| `Polyline2D.firstSegment` | `p: Polyline2D -> Line2D` | `Polyline3D.firstSegment` | `pl: Polyline3D -> Line3D` |
| `.GetPt` | `pointIndex: int -> Pt` | `.GetPnt` | `pointIndex: int -> Pnt` |
| `Polyline2D.getPt` | `pointIndex: int -> p: Polyline2D -> Pt` | `Polyline3D.getPnt` | `pointIndex: int -> p: Polyline3D -> Pnt` |
| `.GetSegment` | `i: int -> Line2D` | `.GetSegment` | `i: int -> Line3D` |
| `Polyline2D.getSegment` | `i: int -> p: Polyline2D -> Line2D` | `Polyline3D.getSegment` | `i: int -> pl: Polyline3D -> Line3D` |
| `.GetX` | `pointIndex: int -> float` | `.GetX` | `pointIndex: int -> float` |
| `Polyline2D.getX` | `pointIndex: int -> p: Polyline2D -> float` | `Polyline3D.getX` | `pointIndex: int -> p: Polyline3D -> float` |
| `.GetY` | `pointIndex: int -> float` | `.GetY` | `pointIndex: int -> float` |
| `Polyline2D.getY` | `pointIndex: int -> p: Polyline2D -> float` | `Polyline3D.getY` | `pointIndex: int -> p: Polyline3D -> float` |
|  |  | `.GetZ` | `pointIndex: int -> float` |
|  |  | `Polyline3D.getZ` | `pointIndex: int -> p: Polyline3D -> float` |
| `.IsAlmostClosed` | `tolerance: float -> bool` | `.IsAlmostClosed` | `tolerance: float -> bool` |
| `Polyline2D.isAlmostClosed` | `tolerance: float -> p: Polyline2D -> bool` | `Polyline3D.isAlmostClosed` | `tolerance: float -> pl: Polyline3D -> bool` |
| `.IsClockwise` | `bool` |  |  |
| `Polyline2D.isClockwise` | `p: Polyline2D -> bool` |  |  |
|  |  | `.IsClockwiseIn2D` | `bool` |
|  |  | `Polyline3D.isClockwiseIn2D` | `pl: Polyline3D -> bool` |
| `.IsClosed` | `bool` | `.IsClosed` | `bool` |
| `Polyline2D.isClosed` | `p: Polyline2D -> bool` | `Polyline3D.isClosed` | `pl: Polyline3D -> bool` |
| `.IsCounterClockwise` | `bool` |  |  |
| `Polyline2D.isCounterClockwise` | `p: Polyline2D -> bool` |  |  |
|  |  | `.IsCounterClockwiseIn2D` | `bool` |
|  |  | `Polyline3D.isCounterClockwiseIn2D` | `pl: Polyline3D -> bool` |
|  |  | `.IsPlanar` | `?tolerance: float -> bool` |
|  |  | `Polyline3D.isPlanar` | `tolerance: float -> pl: Polyline3D -> bool` |
| `.LastPoint` | `Pt with get, set` | `.LastPoint` | `Pnt with get, set` |
| `Polyline2D.lastPoint` | `p: Polyline2D -> Pt` | `Polyline3D.lastPoint` | `pl: Polyline3D -> Pnt` |
| `.LastSegment` | `Line2D` | `.LastSegment` | `Line3D` |
| `Polyline2D.lastSegment` | `p: Polyline2D -> Line2D` | `Polyline3D.lastSegment` | `pl: Polyline3D -> Line3D` |
| `.Length` | `float` | `.Length` | `float` |
| `Polyline2D.length` | `p: Polyline2D -> float` | `Polyline3D.length` | `p: Polyline3D -> float` |
| `.Move` | `v: Vc -> unit` | `.Move` | `v: Vec -> unit` |
| `Polyline2D.move` | `v: Vc -> pl: Polyline2D -> Polyline2D` | `Polyline3D.move` | `v: Vec -> pl: Polyline3D -> Polyline3D` |
| `.MovePt` | `pointIndex: int * v: Vc -> unit` | `.MovePnt` | `pointIndex: int * v: Vec -> unit` |
| `Polyline2D.movePt` | `pointIndex: int -> v: Vc -> p: Polyline2D -> unit` | `Polyline3D.movePnt` | `pointIndex: int -> v: Vec -> p: Polyline3D -> unit` |
| `.MovePtClosed` | `pointIndex: int * v: Vc -> unit` | `.MovePntClosed` | `pointIndex: int * v: Vec -> unit` |
| `Polyline2D.movePtClosed` | `pointIndex: int -> v: Vc -> p: Polyline2D -> unit` | `Polyline3D.movePntClosed` | `pointIndex: int -> v: Vec -> p: Polyline3D -> unit` |
| `.MovePtXY` | `pointIndex: int * x: float * y: float -> unit` |  |  |
| `Polyline2D.movePtXY` | `pointIndex: int -> x: float -> y: float -> p: Polyline2D -> unit` |  |  |
| `.MovePtXYClosed` | `pointIndex: int * x: float * y: float -> unit` |  |  |
| `Polyline2D.movePtXYClosed` | `pointIndex: int -> x: float -> y: float -> p: Polyline2D -> unit` |  |  |
|  |  | `.MovePntXYZ` | `pointIndex: int * x: float * y: float * z: float -> unit` |
|  |  | `Polyline3D.movePntXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> p: Polyline3D -> unit` |
|  |  | `.MovePntXYZClosed` | `pointIndex: int * x: float * y: float * z: float -> unit` |
|  |  | `Polyline3D.movePntXYZClosed` | `pointIndex: int -> x: float -> y: float -> z: float -> p: Polyline3D -> unit` |
| `.MoveX` | `distance: float -> unit` | `.MoveX` | `distance: float -> unit` |
| `Polyline2D.moveX` | `distance: float -> pl: Polyline2D -> Polyline2D` | `Polyline3D.moveX` | `distance: float -> pl: Polyline3D -> Polyline3D` |
| `.MoveY` | `distance: float -> unit` | `.MoveY` | `distance: float -> unit` |
| `Polyline2D.moveY` | `distance: float -> pl: Polyline2D -> Polyline2D` | `Polyline3D.moveY` | `distance: float -> pl: Polyline3D -> Polyline3D` |
|  |  | `.MoveZ` | `distance: float -> unit` |
|  |  | `Polyline3D.moveZ` | `distance: float -> pl: Polyline3D -> Polyline3D` |
| `.PointCount` | `int` | `.PointCount` | `int` |
| `Polyline2D.pointCount` | `p: Polyline2D -> int` | `Polyline3D.pointCount` | `p: Polyline3D -> int` |
| `.RemoveDuplicateAndCollinearPoints` | `angleTolerance: float<cosine> -> distanceTolerance: float -> unit` | `.RemoveDuplicateAndCollinearPoints` | `angleTolerance: float<cosine> -> distanceTolerance: float -> unit` |
| `Polyline2D.removeDuplicateAndCollinearPoints` | `angleTolerance: float<cosine> -> distanceTolerance: float -> pl: Polyline2D -> Polyline2D` | `Polyline3D.removeDuplicateAndCollinearPoints` | `angleTolerance: float<cosine> -> distanceTolerance: float -> pl: Polyline3D -> Polyline3D` |
| `.RemoveDuplicatePoints` | `distanceTolerance: float -> unit` | `.RemoveDuplicatePoints` | `distanceTolerance: float -> unit` |
| `Polyline2D.removeDuplicatePoints` | `distanceTolerance: float -> pl: Polyline2D -> Polyline2D` | `Polyline3D.removeDuplicatePoints` | `distanceTolerance: float -> pl: Polyline3D -> Polyline3D` |
| `.RemoveDuplicatePointsAndReport` | `distanceTolerance: float -> ResizeArray<Pt>` | `.RemoveDuplicatePointsAndReport` | `distanceTolerance: float -> ResizeArray<Pnt>` |
| `Polyline2D.removeDuplicatePointsAndReport` | `distanceTolerance: float -> pl: Polyline2D -> Polyline2D * ResizeArray<Pt>` | `Polyline3D.removeDuplicatePointsAndReport` | `distanceTolerance: float -> pl: Polyline3D -> Polyline3D * ResizeArray<Pnt>` |
| `.RemoveDuplicatePointsFaithfully` | `distanceTolerance: float -> unit` | `.RemoveDuplicatePointsFaithfully` | `distanceTolerance: float -> unit` |
| `Polyline2D.removeDuplicatePointsFaithfully` | `distanceTolerance: float -> pl: Polyline2D -> Polyline2D` | `Polyline3D.removeDuplicatePointsFaithfully` | `distanceTolerance: float -> pl: Polyline3D -> Polyline3D` |
| `.RemoveUTurns` | `minCos: float<cosine> -> unit` | `.RemoveUTurns` | `minCos: float<cosine> -> unit` |
| `Polyline2D.removeUTurns` | `minCos: float<cosine> -> polyLine: Polyline2D -> Polyline2D` | `Polyline3D.removeUTurns` | `minCos: float<cosine> -> polyLine: Polyline3D -> Polyline3D` |
| `.RemoveUTurnsDeeply` | `minCos: float<cosine> -> unit` | `.RemoveUTurnsDeeply` | `minCos: float<cosine> -> unit` |
| `Polyline2D.removeUTurnsDeeply` | `minCos: float<cosine> -> polyLine: Polyline2D -> Polyline2D` | `Polyline3D.removeUTurnsDeeply` | `minCos: float<cosine> -> polyLine: Polyline3D -> Polyline3D` |
| `.Reverse` | `unit -> unit` | `.Reverse` | `unit -> unit` |
| `Polyline2D.reverse` | `p: Polyline2D -> Polyline2D` | `Polyline3D.reverse` | `p: Polyline3D -> Polyline3D` |
| `.Rotate` | `r: Rotation2D -> unit` | `.Rotate` | `q: Quaternion -> unit` |
| `Polyline2D.rotate` | `r: Rotation2D -> pl: Polyline2D -> Polyline2D` | `Polyline3D.rotate` | `q: Quaternion -> pl: Polyline3D -> Polyline3D` |
| `.RotateAround` | `cen: Pt -> r: Rotation2D -> unit` | `.RotateAround` | `cen: Pnt -> q: Quaternion -> unit` |
| `Polyline2D.rotateAround` | `cen: Pt -> r: Rotation2D -> pl: Polyline2D -> Polyline2D` | `Polyline3D.rotateAround` | `cen: Pnt -> q: Quaternion -> pl: Polyline3D -> Polyline3D` |
| `.RotateAroundDegrees` | `cen: Pt -> angDegree: float -> unit` |  |  |
| `Polyline2D.rotateAroundDegrees` | `cen: Pt -> angDegree: float -> pl: Polyline2D -> Polyline2D` |  |  |
| `.RotateAroundRadians` | `cen: Pt -> angRadians: float -> unit` |  |  |
| `Polyline2D.rotateAroundRadians` | `cen: Pt -> angRadians: float -> pl: Polyline2D -> Polyline2D` |  |  |
| `.RotateDegrees` | `angDegree: float -> unit` |  |  |
| `Polyline2D.rotateDegrees` | `angDegree: float -> pl: Polyline2D -> Polyline2D` |  |  |
|  |  | `.RotateOnZ` | `r: Rotation2D -> unit` |
|  |  | `Polyline3D.rotateOnZ` | `r: Rotation2D -> pl: Polyline3D -> Polyline3D` |
|  |  | `.RotateOnZWithCenter` | `cen: Pnt -> r: Rotation2D -> unit` |
|  |  | `Polyline3D.rotateOnZWithCenter` | `cen: Pnt -> r: Rotation2D -> pl: Polyline3D -> Polyline3D` |
| `.RotateRadians` | `angRadians: float -> unit` |  |  |
| `Polyline2D.rotateRadians` | `angRadians: float -> pl: Polyline2D -> Polyline2D` |  |  |
| `.Scale` | `factor: float -> unit` | `.Scale` | `factor: float -> unit` |
| `Polyline2D.scale` | `factor: float -> pl: Polyline2D -> Polyline2D` | `Polyline3D.scale` | `factor: float -> pl: Polyline3D -> Polyline3D` |
| `.ScaleOn` | `cen: Pt -> factor: float -> unit` | `.ScaleOn` | `cen: Pnt -> factor: float -> unit` |
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
| `.SegmentVectorsXY` | `ResizeArray<float>` |  |  |
| `Polyline2D.segmentVectorsXY` | `p: Polyline2D -> ResizeArray<float>` |  |  |
|  |  | `.SegmentVectorsXYZ` | `ResizeArray<float>` |
|  |  | `Polyline3D.segmentVectorsXYZ` | `p: Polyline3D -> ResizeArray<float>` |
| `.SetPt` | `pointIndex: int * pt: Pt -> unit` | `.SetPnt` | `pointIndex: int * pnt: Pnt -> unit` |
| `Polyline2D.setPt` | `pointIndex: int -> pt: Pt -> p: Polyline2D -> unit` | `Polyline3D.setPnt` | `pointIndex: int -> pnt: Pnt -> p: Polyline3D -> unit` |
| `.SetPtXY` | `pointIndex: int * x: float * y: float -> unit` |  |  |
| `Polyline2D.setPtXY` | `pointIndex: int -> x: float -> y: float -> p: Polyline2D -> unit` |  |  |
| `.SetPtXYClosed` | `pointIndex: int * x: float * y: float -> unit` |  |  |
| `Polyline2D.setPtXYClosed` | `pointIndex: int -> x: float -> y: float -> p: Polyline2D -> unit` |  |  |
|  |  | `.SetPntXYZ` | `pointIndex: int * x: float * y: float * z: float -> unit` |
|  |  | `Polyline3D.setPntXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> p: Polyline3D -> unit` |
|  |  | `.SetPntXYZClosed` | `pointIndex: int * x: float * y: float * z: float -> unit` |
|  |  | `Polyline3D.setPntXYZClosed` | `pointIndex: int -> x: float -> y: float -> z: float -> p: Polyline3D -> unit` |
| `.SignedArea` | `float` |  |  |
| `Polyline2D.signedArea` | `p: Polyline2D -> float` |  |  |
|  |  | `.SignedAreaIn2D` | `float` |
|  |  | `Polyline3D.signedAreaIn2D` | `pl: Polyline3D -> float` |
| `.SignedDistanceTo` | `point: Pt -> float` |  |  |
| `Polyline2D.signedDistanceTo` | `point: Pt -> pl: Polyline2D -> float` |  |  |
| `.SignedDistanceToXY` | `x: float * y: float -> float` |  |  |
| `Polyline2D.signedDistanceToXY` | `x: float -> y: float -> pl: Polyline2D -> float` |  |  |
| `.Start` | `Pt with get, set` | `.Start` | `Pnt with get, set` |
| `Polyline2D.start` | `p: Polyline2D -> Pt` | `Polyline3D.start` | `pl: Polyline3D -> Pnt` |
| `.ToString` | `unit -> string` | `.ToString` | `unit -> string` |
|  |  | `.Transform` | `m: Matrix -> unit` |
|  |  | `Polyline3D.transform` | `m: Matrix -> pl: Polyline3D -> Polyline3D` |
|  |  | `.TransformRigid` | `m: RigidMatrix -> unit` |
|  |  | `Polyline3D.transformRigid` | `m: RigidMatrix -> pl: Polyline3D -> Polyline3D` |
|  |  | `.TryAverageNormal` | `Vec voption` |
|  |  | `Polyline3D.tryAverageNormal` | `pl: Polyline3D -> Vec voption` |
|  |  | `.TryAveragePlane` | `?tolerance: float -> NPlane voption` |
|  |  | `Polyline3D.tryAveragePlane` | `tolerance: float -> pl: Polyline3D -> NPlane voption` |
|  |  | `.TryPlane` | `?tolerance: float -> NPlane voption` |
|  |  | `Polyline3D.tryPlane` | `tolerance: float -> pl: Polyline3D -> NPlane voption` |
| `.WindingNumber` | `point: Pt -> int` |  |  |
| `Polyline2D.windingNumber` | `point: Pt -> pl: Polyline2D -> int` |  |  |
| `.XYs` | `ResizeArray<float>` |  |  |
|  |  | `.XYZs` | `ResizeArray<float>` |
| **Other static members** | | | |
| `Polyline2D.createDirectly` | `xys: ResizeArray<float> -> Polyline2D` | `Polyline3D.createDirectly` | `xyzs: ResizeArray<float> -> Polyline3D` |
| `Polyline2D.createEmpty` | `capacity: int -> Polyline2D` | `Polyline3D.createEmpty` | `capacity: int -> Polyline3D` |
|  |  | `Polyline3D.createFrom2DXYMembers` | `xyObjs: ^T seq -> Polyline3D` |
|  |  | `Polyline3D.createFrom2DxyMembers` | `xyObjs: ^T seq -> Polyline3D` |
| `Polyline2D.createFromBRectCCW` | `r: BRect -> Polyline2D` |  |  |
| `Polyline2D.createFromBRectCW` | `r: BRect -> Polyline2D` |  |  |
| `Polyline2D.createFromPts` | `points: Pt seq -> Polyline2D` | `Polyline3D.createFromPnts` | `points: Pnt seq -> Polyline3D` |
|  |  | `Polyline3D.createFromRect3D` | `r: Rect3D -> Polyline3D` |
| `Polyline2D.createFromRectCCW` | `r: Rect2D -> Polyline2D` |  |  |
| `Polyline2D.createFromRectCW` | `r: Rect2D -> Polyline2D` |  |  |
| `Polyline2D.createFromXYMembers` | `xyObjs: ^T seq -> Polyline2D` |  |  |
| `Polyline2D.createFromxyMembers` | `xyObjs: ^T seq -> Polyline2D` |  |  |
|  |  | `Polyline3D.createFromXYZMembers` | `xyzObjs: ^T seq -> Polyline3D` |
|  |  | `Polyline3D.createFromxyzMembers` | `xyzObjs: ^T seq -> Polyline3D` |
| `Polyline2D.end'` | `p: Polyline2D -> Pt` | `Polyline3D.end'` | `pl: Polyline3D -> Pnt` |
| `Polyline2D.equalsExact` | `pl1: Polyline2D -> pl2: Polyline2D -> bool` | `Polyline3D.equalsExact` | `pl1: Polyline3D -> pl2: Polyline3D -> bool` |
| `Polyline2D.equalsWithin` | `tol: float -> pl1: Polyline2D -> pl2: Polyline2D -> bool` | `Polyline3D.equalsWithin` | `tol: float -> pl1: Polyline3D -> pl2: Polyline3D -> bool` |
| `Polyline2D.getXYs` | `p: Polyline2D -> ResizeArray<float>` |  |  |
|  |  | `Polyline3D.getXYZs` | `p: Polyline3D -> ResizeArray<float>` |
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
| `Polyline2D.subPolyline` | `a: float -> b: float -> pl: Polyline2D -> Polyline2D` | `Polyline3D.subPolyline` | `a: float -> b: float -> pl: Polyline3D -> Polyline3D` |
| `Polyline2D.translate` | `v: Vc -> pl: Polyline2D -> Polyline2D` | `Polyline3D.translate` | `v: Vec -> pl: Polyline3D -> Polyline3D` |
| `Polyline2D.tryFind` | `condition: (float -> float -> bool) -> pl: Polyline2D -> Pt voption` | `Polyline3D.tryFind` | `condition: (float -> float -> float -> bool) -> pl: Polyline3D -> Pnt voption` |
| `Polyline2D.tryFindIndex` | `condition: (float -> float -> bool) -> pl: Polyline2D -> int voption` | `Polyline3D.tryFindIndex` | `condition: (float -> float -> float -> bool) -> pl: Polyline3D -> int voption` |
| `Polyline2D.tryFindLast` | `condition: (float -> float -> bool) -> pl: Polyline2D -> Pt voption` | `Polyline3D.tryFindLast` | `condition: (float -> float -> float -> bool) -> pl: Polyline3D -> Pnt voption` |
| `Polyline2D.tryFindLastIndex` | `condition: (float -> float -> bool) -> pl: Polyline2D -> int voption` | `Polyline3D.tryFindLastIndex` | `condition: (float -> float -> float -> bool) -> pl: Polyline3D -> int voption` |
| `Polyline2D.tryFindSelfIntersection` | `pl: Polyline2D -> (Pt * int * int) voption` |  |  |
| `Polyline2D.tryFindSelfIntersectionWithBRect` | `pl: Polyline2D -> (Pt * int * int) voption` |  |  |

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
| `ResizeArr.tryFindBack` | `predicate: ('T -> bool) -> resizeArray: ResizeArray<'T> -> 'T voption` |
| `ResizeArr.tryFindIndex` | `predicate: ('T -> bool) -> xs: ResizeArray<'T> -> int voption` |

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
