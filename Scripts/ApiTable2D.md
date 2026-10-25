# Euclid 2D API

All public 2D geometry types of Euclid, with their members and functions.
The general utilities and helper types are left out. See [ApiTable.md](ApiTable.md) for them and for 2D and 3D side by side.

- Instance members and fields start with `.`, e.g. `.DistanceTo`.
- Static members are qualified with their type name. They are listed right below the instance member of the same name.
  The static members without such an instance member follow at the end of each table.
- A property shows only its type. `with get, set` marks a property that can be set. `?name` marks an optional parameter.

Generated from the sources by `Scripts/ApiTable.fsx`. Do not edit by hand.

## Contents

- [Vc](#vc)
- [UnitVc](#unitvc)
- [Pt](#pt)
- [Rotation2D](#rotation2d)
- [Line2D](#line2d)
- [XLine2D.XRayParam](#xline2dxrayparam)
- [XLine2D.XRay](#xline2dxray)
- [XLine2D.XParam](#xline2dxparam)
- [XLine2D.XPt](#xline2dxpt)
- [XLine2D.ClParams](#xline2dclparams)
- [XLine2D.ClPts](#xline2dclpts)
- [XLine2D.XEnds](#xline2dxends)
- [module XLineXY](#module-xlinexy)
- [XLine2D](#xline2d)
- [Rect2D](#rect2d)
- [BRect](#brect)
- [Tria2D](#tria2d)
- [Points2D](#points2d)
- [module Offset2D](#module-offset2d)
- [Offset2D.UTurn](#offset2duturn)
- [Offset2D.VarDistParallel](#offset2dvardistparallel)
- [Polyline2D](#polyline2d)
- [Topology2D](#topology2d)
- [module Similarity2D](#module-similarity2d)
- [Similarity2D.GroupInsideObjectToCheck](#similarity2dgroupinsideobjecttocheck)
- [Similarity2D.ObjectToCheck](#similarity2dobjecttocheck)

## Vc

| Vc | Signature |
|:--|:--|
| `new` | `x: float * y: float -> Vc` |
| `.X` | `float` |
| `.Y` | `float` |
| `.AngleDiamondTo` | `b: Vc -> float` |
| `.AsFSharpCode` | `string` |
| `.AsPnt` | `Pnt` |
| `Vc.asPnt` | `v: Vc -> Pnt` |
| `.AsPt` | `Pt` |
| `Vc.asPt` | `v: Vc -> Pt` |
| `.AsString` | `string` |
| `.AsVec` | `Vec` |
| `Vc.asVec` | `v: Vc -> Vec` |
| `.Cross` | `b: Vc -> float` |
| `.Cross` | `b: UnitVc -> float` |
| `Vc.cross` | `a: Vc * b: Vc -> float` |
| `Vc.cross` | `a: UnitVc * b: Vc -> float` |
| `Vc.cross` | `a: Vc * b: UnitVc -> float` |
| `.Direction180` | `float` |
| `Vc.direction180` | `v: Vc -> float` |
| `.Direction2Pi` | `float` |
| `Vc.direction2Pi` | `v: Vc -> float` |
| `.Direction360` | `float` |
| `Vc.direction360` | `v: Vc -> float` |
| `.DirectionDiamond` | `float` |
| `Vc.directionDiamond` | `a: Vc -> float` |
| `.DirectionPi` | `float` |
| `Vc.directionPi` | `v: Vc -> float` |
| `.Dot` | `b: Vc -> float` |
| `.Dot` | `b: UnitVc -> float` |
| `Vc.dot` | `a: Vc -> b: Vc -> float` |
| `.Half` | `Vc` |
| `Vc.half` | `v: Vc -> Vc` |
| `.IsNotZero` | `bool` |
| `Vc.isNotZero` | `v: Vc -> bool` |
| `.IsOppositeOrientation` | `other: Vc -> bool` |
| `.IsOppositeOrientation` | `other: UnitVc -> bool` |
| `Vc.isOppositeOrientation` | `v: Vc -> other: Vc -> bool` |
| `.IsParallelAndOrientedTo` | `other: Vc * minCosine: float<cosine> -> bool` |
| `.IsParallelAndOrientedTo` | `other: UnitVc * minCosine: float<cosine> -> bool` |
| `Vc.isParallelAndOrientedTo` | `minCosine: float<cosine> -> a: Vc -> b: Vc -> bool` |
| `.IsParallelTo` | `other: Vc * minCosine: float<cosine> -> bool` |
| `.IsParallelTo` | `other: UnitVc * minCosine: float<cosine> -> bool` |
| `Vc.isParallelTo` | `minCosine: float<cosine> -> a: Vc -> b: Vc -> bool` |
| `.IsPerpendicularTo` | `other: Vc * maxCosine: float<cosine> -> bool` |
| `.IsPerpendicularTo` | `other: UnitVc * maxCosine: float<cosine> -> bool` |
| `Vc.isPerpendicularTo` | `maxCosine: float<cosine> -> a: Vc -> b: Vc -> bool` |
| `.IsTiny` | `tol: float -> bool` |
| `Vc.isTiny` | `tol: float -> v: Vc -> bool` |
| `.IsTinySq` | `tol: float -> bool` |
| `Vc.isTinySq` | `tol: float -> v: Vc -> bool` |
| `.IsUnit` | `bool` |
| `Vc.isUnit` | `v: Vc -> bool` |
| `.IsXAligned` | `bool` |
| `Vc.isXAligned` | `v: Vc -> bool` |
| `.IsYAligned` | `bool` |
| `Vc.isYAligned` | `v: Vc -> bool` |
| `.IsZero` | `bool` |
| `Vc.isZero` | `v: Vc -> bool` |
| `.Length` | `float` |
| `Vc.length` | `v: Vc -> float` |
| `.LengthSq` | `float` |
| `Vc.lengthSq` | `v: Vc -> float` |
| `.MatchesOrientation` | `other: Vc -> bool` |
| `.MatchesOrientation` | `other: UnitVc -> bool` |
| `Vc.matchesOrientation` | `v: Vc -> other: Vc -> bool` |
| `.Rotate` | `r: Rotation2D -> Vc` |
| `Vc.rotate` | `r: Rotation2D -> v: Vc -> Vc` |
| `.Rotate90CCW` | `Vc` |
| `Vc.rotate90CCW` | `v: Vc -> Vc` |
| `.Rotate90CW` | `Vc` |
| `Vc.rotate90CW` | `v: Vc -> Vc` |
| `.RotateByQuarterCircle` | `numberOfQuarters: int -> Vc` |
| `Vc.rotateByQuarterCircle` | `numberOfQuarters: int -> v: Vc -> Vc` |
| `.RotateDegrees` | `angDegree: float -> Vc` |
| `Vc.rotateDegrees` | `angDegree: float -> v: Vc -> Vc` |
| `.RotateRadians` | `angRadians: float -> Vc` |
| `Vc.rotateRadians` | `angRadians: float -> v: Vc -> Vc` |
| `.ToString` | `unit -> string` |
| `.Unitized` | `UnitVc` |
| `.WithLength` | `desiredLength: float -> Vc` |
| `Vc.withLength` | `desiredLength: float -> v: Vc -> Vc` |
| `.WithX` | `x: float -> Vc` |
| `Vc.withX` | `x: float -> v: Vc -> Vc` |
| `.WithY` | `y: float -> Vc` |
| `Vc.withY` | `y: float -> v: Vc -> Vc` |
| `.WithZ` | `z: float -> Vec` |
| `Vc.withZ` | `z: float -> v: Vc -> Vec` |
| **Other static members** | |
| `Vc.add` | `a: Vc -> b: Vc -> Vc` |
| `Vc.addX` | `x: float -> v: Vc -> Vc` |
| `Vc.addY` | `y: float -> v: Vc -> Vc` |
| `Vc.angle180` | `a: Vc -> b: Vc -> float` |
| `Vc.angle2Pi` | `a: Vc -> b: Vc -> float` |
| `Vc.angle360` | `a: Vc -> b: Vc -> float` |
| `Vc.angle90` | `a: Vc -> b: Vc -> float` |
| `Vc.angleDiamond` | `a: Vc -> b: Vc -> float` |
| `Vc.angleHalfPi` | `a: Vc -> b: Vc -> float` |
| `Vc.anglePi` | `a: Vc -> b: Vc -> float` |
| `Vc.angleToX180` | `v: Vc -> float` |
| `Vc.angleToXPi` | `vc: Vc -> float` |
| `Vc.bisector` | `a: Vc -> b: Vc -> Vc` |
| `Vc.create` | `x: float -> y: float -> Vc` |
| `Vc.createFromMembersXY` | `vec: ^T -> Vc` |
| `Vc.createFromMembersxy` | `vec: ^T -> Vc` |
| `Vc.createFromPnt` | `pnt: Pnt -> Vc` |
| `Vc.createFromTo` | `start: Pt -> endValue: Pt -> Vc` |
| `Vc.createFromUnitVc` | `v: UnitVc -> Vc` |
| `Vc.difference` | `a: Vc -> b: Vc -> float` |
| `Vc.differenceSq` | `a: Vc -> b: Vc -> float` |
| `Vc.DivideByInt` | `v: Vc * i: int -> Vc` |
| `Vc.dotUnitVc` | `a: Vc -> b: UnitVc -> float` |
| `Vc.equalsExact` | `a: Vc -> b: Vc -> bool` |
| `Vc.equalsWithin` | `tol: float -> a: Vc -> b: Vc -> bool` |
| `Vc.getX` | `v: Vc -> float` |
| `Vc.getY` | `v: Vc -> float` |
| `Vc.isAngleAbove` | `cosineValue: float<cosine> -> a: Vc -> b: Vc -> bool` |
| `Vc.isAngleBelow` | `cosineValue: float<cosine> -> a: Vc -> b: Vc -> bool` |
| `Vc.isNotParallelWithin` | `cosineValue: float<cosine> -> a: Vc -> b: Vc -> bool` |
| `Vc.isParallelAndOrientedToUnitVc` | `minCosine: float<cosine> -> other: UnitVc -> vector: Vc -> bool` |
| `Vc.isParallelToUnitVc` | `minCosine: float<cosine> -> b: UnitVc -> a: Vc -> bool` |
| `Vc.isParallelWithin` | `cosineValue: float<cosine> -> a: Vc -> b: Vc -> bool` |
| `Vc.isPerpendicularToUnitVc` | `maxCosine: float<cosine> -> other: UnitVc -> vector: Vc -> bool` |
| `Vc.lerp` | `start: Vc -> endValue: Vc -> rel: float -> Vc` |
| `Vc.matchOrientation` | `orientationToMatch: Vc -> vecToFlip: Vc -> Vc` |
| `Vc.matchUnitVcOrientation` | `orientationToMatch: UnitVc -> vecToFlip: Vc -> Vc` |
| `Vc.notEqualsWithin` | `tol: float -> a: Vc -> b: Vc -> bool` |
| `Vc.reverse` | `v: Vc -> Vc` |
| `Vc.scale` | `f: float -> v: Vc -> Vc` |
| `Vc.slerp` | `start: Vc -> endValue: Vc -> rel: float -> Vc` |
| `Vc.slopeDegrees` | `v: Vc -> float` |
| `Vc.slopePercent` | `v: Vc -> float` |
| `Vc.slopeRadians` | `v: Vc -> float` |
| `Vc.tryUnitize` | `v: Vc -> UnitVc voption` |
| `Vc.unitize` | `v: Vc -> UnitVc` |
| `Vc.unitizeAsVc` | `v: Vc -> Vc` |
| `Vc.unitizeOrDefault` | `defaultUnitVector: UnitVc -> v: Vc -> UnitVc` |
| `Vc.unitizeUnchecked` | `v: Vc -> UnitVc` |
| `Vc.unitizeWithErrorMessage` | `errMsg: string -> v: Vc -> UnitVc` |
| `Vc.xAxis` | `Vc` |
| `Vc.yAxis` | `Vc` |
| `Vc.Zero` | `Vc` |
| `( *** )` | `a: Vc * b: Vc -> float` |
| `( * )` | `v: Vc * f: float -> Vc` |
| `( * )` | `f: float * v: Vc -> Vc` |
| `( + )` | `a: Vc * b: Vc -> Vc` |
| `( - )` | `a: Vc * b: Vc -> Vc` |
| `( / )` | `v: Vc * f: float -> Vc` |
| `( ~- )` | `v: Vc -> Vc` |

## UnitVc

| UnitVc | Signature |
|:--|:--|
| `.X` | `float` |
| `.Y` | `float` |
| `.AngleDiamondTo` | `b: UnitVc -> float` |
| `.AsFSharpCode` | `string` |
| `.AsPnt` | `Pnt` |
| `UnitVc.asPnt` | `v: UnitVc -> Pnt` |
| `.AsPt` | `Pt` |
| `UnitVc.asPt` | `v: UnitVc -> Pt` |
| `.AsString` | `string` |
| `.AsUnitVec` | `UnitVec` |
| `UnitVc.asUnitVec` | `v: UnitVc -> UnitVec` |
| `.AsVc` | `Vc` |
| `UnitVc.asVc` | `v: UnitVc -> Vc` |
| `.AsVec` | `Vec` |
| `UnitVc.asVec` | `v: UnitVc -> Vec` |
| `.Cross` | `b: Vc -> float` |
| `.Cross` | `b: UnitVc -> float` |
| `UnitVc.cross` | `a: UnitVc * b: UnitVc -> float` |
| `UnitVc.cross` | `a: UnitVc * b: Vc -> float` |
| `UnitVc.cross` | `a: Vc * b: UnitVc -> float` |
| `.Direction180` | `float` |
| `UnitVc.direction180` | `v: UnitVc -> float` |
| `.Direction2Pi` | `float` |
| `UnitVc.direction2Pi` | `v: UnitVc -> float` |
| `.Direction360` | `float` |
| `UnitVc.direction360` | `v: UnitVc -> float` |
| `.DirectionDiamond` | `float` |
| `UnitVc.directionDiamond` | `a: UnitVc -> float` |
| `.DirectionPi` | `float` |
| `UnitVc.directionPi` | `v: UnitVc -> float` |
| `.Dot` | `b: UnitVc -> float` |
| `.Dot` | `b: Vc -> float` |
| `UnitVc.dot` | `a: UnitVc -> b: UnitVc -> float` |
| `.DotCosine` | `b: UnitVc -> float<cosine>` |
| `UnitVc.dotCosine` | `a: UnitVc -> b: UnitVc -> float<cosine>` |
| `.IsOppositeOrientation` | `other: UnitVc -> bool` |
| `.IsOppositeOrientation` | `other: Vc -> bool` |
| `UnitVc.isOppositeOrientation` | `other: UnitVc -> v: UnitVc -> bool` |
| `.IsParallelAndOrientedTo` | `other: UnitVc * minCosine: float<cosine> -> bool` |
| `.IsParallelAndOrientedTo` | `other: Vc * minCosine: float<cosine> -> bool` |
| `UnitVc.isParallelAndOrientedTo` | `minCosine: float<cosine> -> a: UnitVc -> b: UnitVc -> bool` |
| `.IsParallelTo` | `other: UnitVc * minCosine: float<cosine> -> bool` |
| `.IsParallelTo` | `other: Vc * minCosine: float<cosine> -> bool` |
| `UnitVc.isParallelTo` | `minCosine: float<cosine> -> a: UnitVc -> b: UnitVc -> bool` |
| `.IsPerpendicularTo` | `other: UnitVc * maxCosine: float<cosine> -> bool` |
| `.IsPerpendicularTo` | `other: Vc * maxCosine: float<cosine> -> bool` |
| `UnitVc.isPerpendicularTo` | `maxCosine: float<cosine> -> a: UnitVc -> b: UnitVc -> bool` |
| `.IsXAligned` | `bool` |
| `UnitVc.isXAligned` | `v: UnitVc -> bool` |
| `.IsYAligned` | `bool` |
| `UnitVc.isYAligned` | `v: UnitVc -> bool` |
| `.MatchesOrientation` | `other: UnitVc -> bool` |
| `.MatchesOrientation` | `other: Vc -> bool` |
| `UnitVc.matchesOrientation` | `other: UnitVc -> v: UnitVc -> bool` |
| `.Rotate` | `r: Rotation2D -> UnitVc` |
| `UnitVc.rotate` | `r: Rotation2D -> v: UnitVc -> UnitVc` |
| `.Rotate90CCW` | `UnitVc` |
| `UnitVc.rotate90CCW` | `v: UnitVc -> UnitVc` |
| `.Rotate90CW` | `UnitVc` |
| `UnitVc.rotate90CW` | `v: UnitVc -> UnitVc` |
| `.RotateByQuarterCircle` | `numberOfQuarters: int -> UnitVc` |
| `UnitVc.rotateByQuarterCircle` | `numberOfQuarters: int -> v: UnitVc -> UnitVc` |
| `.RotateDegrees` | `angDegree: float -> UnitVc` |
| `UnitVc.rotateDegrees` | `angDegree: float -> v: UnitVc -> UnitVc` |
| `.RotateRadians` | `angRadians: float -> UnitVc` |
| `UnitVc.rotateRadians` | `angRadians: float -> v: UnitVc -> UnitVc` |
| `.ToString` | `unit -> string` |
| `.WithX` | `x: float -> Vc` |
| `UnitVc.withX` | `x: float -> v: UnitVc -> Vc` |
| `.WithY` | `y: float -> Vc` |
| `UnitVc.withY` | `y: float -> v: UnitVc -> Vc` |
| `.WithZ` | `z: float -> Vec` |
| `UnitVc.withZ` | `z: float -> v: UnitVc -> Vec` |
| **Other static members** | |
| `UnitVc.add` | `a: UnitVc -> b: UnitVc -> Vc` |
| `UnitVc.addX` | `x: float -> v: UnitVc -> Vc` |
| `UnitVc.addY` | `y: float -> v: UnitVc -> Vc` |
| `UnitVc.angle180` | `a: UnitVc -> b: UnitVc -> float` |
| `UnitVc.angle2Pi` | `a: UnitVc -> b: UnitVc -> float` |
| `UnitVc.angle360` | `a: UnitVc -> b: UnitVc -> float` |
| `UnitVc.angle90` | `a: UnitVc -> b: UnitVc -> float` |
| `UnitVc.angleDiamond` | `a: UnitVc -> b: UnitVc -> float` |
| `UnitVc.angleHalfPi` | `a: UnitVc -> b: UnitVc -> float` |
| `UnitVc.anglePi` | `a: UnitVc -> b: UnitVc -> float` |
| `UnitVc.angleToX180` | `v: UnitVc -> float` |
| `UnitVc.angleToXPi` | `v: UnitVc -> float` |
| `UnitVc.create` | `x: float -> y: float -> UnitVc` |
| `UnitVc.createFromMembersXY` | `vec: ^T -> UnitVc` |
| `UnitVc.createFromMembersxy` | `vec: ^T -> UnitVc` |
| `UnitVc.createFromPt` | `pt: Pt -> UnitVc` |
| `UnitVc.createFromTo` | `fromPt: Pt -> toPt: Pt -> UnitVc` |
| `UnitVc.createFromVc` | `v: Vc -> UnitVc` |
| `UnitVc.createUnchecked` | `x: float * y: float -> UnitVc` |
| `UnitVc.createWithErrorMessage` | `msg: string -> x: float -> y: float -> UnitVc` |
| `UnitVc.difference` | `a: UnitVc -> b: UnitVc -> float` |
| `UnitVc.differenceSq` | `a: UnitVc -> b: UnitVc -> float` |
| `UnitVc.dotVc` | `a: UnitVc -> b: Vc -> float` |
| `UnitVc.equalsExact` | `a: UnitVc -> b: UnitVc -> bool` |
| `UnitVc.equalsWithin` | `tol: float -> a: UnitVc -> b: UnitVc -> bool` |
| `UnitVc.getX` | `v: UnitVc -> float` |
| `UnitVc.getY` | `v: UnitVc -> float` |
| `UnitVc.isAngleAbove` | `cosineValue: float<cosine> -> a: UnitVc -> b: UnitVc -> bool` |
| `UnitVc.isAngleBelow` | `cosineValue: float<cosine> -> a: UnitVc -> b: UnitVc -> bool` |
| `UnitVc.isNotParallelWithin` | `cosineValue: float<cosine> -> a: UnitVc -> b: UnitVc -> bool` |
| `UnitVc.isParallelWithin` | `cosineValue: float<cosine> -> a: UnitVc -> b: UnitVc -> bool` |
| `UnitVc.lerp` | `start: UnitVc -> endValue: UnitVc -> rel: float -> Vc` |
| `UnitVc.matchOrientation` | `orientationToMatch: UnitVc -> vecToFlip: UnitVc -> UnitVc` |
| `UnitVc.matchVcOrientation` | `orientationToMatch: Vc -> vecToFlip: UnitVc -> UnitVc` |
| `UnitVc.notEqualsWithin` | `tol: float -> a: UnitVc -> b: UnitVc -> bool` |
| `UnitVc.reverse` | `v: UnitVc -> UnitVc` |
| `UnitVc.scale` | `scale: float -> v: UnitVc -> Vc` |
| `UnitVc.sine` | `a: UnitVc -> b: UnitVc -> float` |
| `UnitVc.slerp` | `start: UnitVc -> endValue: UnitVc -> rel: float -> UnitVc` |
| `UnitVc.slopeDegrees` | `v: UnitVc -> float` |
| `UnitVc.slopePercent` | `v: UnitVc -> float` |
| `UnitVc.slopeRadians` | `v: UnitVc -> float` |
| `UnitVc.tangent` | `a: UnitVc -> b: UnitVc -> float` |
| `UnitVc.withLength` | `length: float -> v: UnitVc -> Vc` |
| `UnitVc.xAxis` | `UnitVc` |
| `UnitVc.yAxis` | `UnitVc` |
| `( *** )` | `a: UnitVc * b: UnitVc -> float` |
| `( *** )` | `a: UnitVc * b: Vc -> float` |
| `( *** )` | `a: Vc * b: UnitVc -> float` |
| `( * )` | `a: UnitVc * f: float -> Vc` |
| `( * )` | `f: float * a: UnitVc -> Vc` |
| `( + )` | `a: UnitVc * b: UnitVc -> Vc` |
| `( + )` | `a: Vc * b: UnitVc -> Vc` |
| `( + )` | `a: UnitVc * b: Vc -> Vc` |
| `( - )` | `a: UnitVc * b: UnitVc -> Vc` |
| `( - )` | `a: Vc * b: UnitVc -> Vc` |
| `( - )` | `a: UnitVc * b: Vc -> Vc` |
| `( / )` | `v: UnitVc * f: float -> Vc` |
| `( ~- )` | `v: UnitVc -> UnitVc` |

## Pt

| Pt | Signature |
|:--|:--|
| `new` | `x: float * y: float -> Pt` |
| `.X` | `float` |
| `.Y` | `float` |
| `.Angle2PiTo` | `o: Pt -> float` |
| `Pt.angle2PiTo` | `fromPt: Pt -> toPt: Pt -> float` |
| `.Angle360To` | `o: Pt -> float` |
| `Pt.angle360To` | `fromPt: Pt -> toPt: Pt -> float` |
| `.AsFSharpCode` | `string` |
| `.AsPnt` | `Pnt` |
| `Pt.asPnt` | `pt: Pt -> Pnt` |
| `.AsString` | `string` |
| `.AsVc` | `Vc` |
| `Pt.asVc` | `pt: Pt -> Vc` |
| `.AsVec` | `Vec` |
| `Pt.asVec` | `pt: Pt -> Vec` |
| `.ClosestPointOnLine` | `ln: Line2D -> Pt` |
| `Pt.closestPointOnLine` | `ln: Line2D -> testPt: Pt -> Pt` |
| `.DirectionDiamondTo` | `o: Pt -> float` |
| `Pt.directionDiamondTo` | `o: Pt -> p: Pt -> float` |
| `.DistanceFromOrigin` | `float` |
| `Pt.distanceFromOrigin` | `pt: Pt -> float` |
| `.DistanceFromOriginSq` | `float` |
| `Pt.distanceFromOriginSq` | `pt: Pt -> float` |
| `.DistanceTo` | `b: Pt -> float` |
| `Pt.distanceTo` | `b: Pt -> p: Pt -> float` |
| `.DistanceToLine` | `ln: Line2D -> float` |
| `Pt.distanceToLine` | `ln: Line2D -> testPt: Pt -> float` |
| `.DistanceToLineSq` | `ln: Line2D -> float` |
| `Pt.distanceToLineSq` | `ln: Line2D -> testPt: Pt -> float` |
| `.DistanceToSq` | `b: Pt -> float` |
| `Pt.distanceToSq` | `b: Pt -> p: Pt -> float` |
| `.DistanceToXY` | `x: float * y: float -> float` |
| `Pt.distanceToXY` | `x: float -> y: float -> p: Pt -> float` |
| `.DistanceToXYSq` | `x: float * y: float -> float` |
| `Pt.distanceToXYSq` | `x: float -> y: float -> p: Pt -> float` |
| `.IsAlmostOrigin` | `tol: float -> bool` |
| `Pt.isAlmostOrigin` | `tol: float -> pt: Pt -> bool` |
| `.IsInvalid` | `bool` |
| `Pt.isInvalid` | `pt: Pt -> bool` |
| `.IsNotOrigin` | `bool` |
| `Pt.isNotOrigin` | `pt: Pt -> bool` |
| `.IsOrigin` | `bool` |
| `Pt.isOrigin` | `pt: Pt -> bool` |
| `.IsValid` | `bool` |
| `Pt.isValid` | `pt: Pt -> bool` |
| `.Rotate` | `r: Rotation2D -> Pt` |
| `Pt.rotate` | `r: Rotation2D -> p: Pt -> Pt` |
| `.Rotate90CCW` | `Pt` |
| `Pt.rotate90CCW` | `pt: Pt -> Pt` |
| `.Rotate90CW` | `Pt` |
| `Pt.rotate90CW` | `pt: Pt -> Pt` |
| `.RotateAround` | `cen: Pt -> r: Rotation2D -> Pt` |
| `Pt.rotateAround` | `cen: Pt -> r: Rotation2D -> pt: Pt -> Pt` |
| `.RotateAroundDegrees` | `cen: Pt -> angDegree: float -> Pt` |
| `Pt.rotateAroundDegrees` | `cen: Pt -> angDegree: float -> pt: Pt -> Pt` |
| `.RotateAroundRadians` | `cen: Pt -> angRadians: float -> Pt` |
| `Pt.rotateAroundRadians` | `cen: Pt -> angRadians: float -> pt: Pt -> Pt` |
| `.RotateByQuarterCircle` | `numberOfQuarters: int -> Pt` |
| `Pt.rotateByQuarterCircle` | `numberOfQuarters: int -> v: Pt -> Pt` |
| `.RotateDegrees` | `angDegree: float -> Pt` |
| `Pt.rotateDegrees` | `angDegree: float -> pt: Pt -> Pt` |
| `.RotateRadians` | `angRadians: float -> Pt` |
| `Pt.rotateRadians` | `angRadians: float -> pt: Pt -> Pt` |
| `.ToString` | `unit -> string` |
| `.WithDistanceFromOrigin` | `l: float -> Pt` |
| `Pt.withDistanceFromOrigin` | `l: float -> pt: Pt -> Pt` |
| `.WithX` | `x: float -> Pt` |
| `Pt.withX` | `x: float -> pt: Pt -> Pt` |
| `.WithY` | `y: float -> Pt` |
| `Pt.withY` | `y: float -> pt: Pt -> Pt` |
| `.WithZ` | `z: float -> Pnt` |
| `Pt.withZ` | `z: float -> pt: Pt -> Pnt` |
| **Other static members** | |
| `Pt.add` | `a: Pt -> b: Pt -> Pt` |
| `Pt.angle180Pts` | `ptPrev: Pt -> ptThis: Pt -> ptNext: Pt -> float` |
| `Pt.angleInCorner` | `prevPt: Pt -> thisPt: Pt -> nextPt: Pt -> float` |
| `Pt.anglePiPts` | `ptPrev: Pt -> ptThis: Pt -> ptNext: Pt -> float` |
| `Pt.bisector` | `ptPrev: Pt -> ptThis: Pt -> ptNext: Pt -> Vc` |
| `Pt.closestOfTwo` | `pt1: Pt -> pt2: Pt -> referencePoint: Pt -> Pt` |
| `Pt.create` | `x: float -> y: float -> Pt` |
| `Pt.createFromMembersXY` | `pt: ^T -> Pt` |
| `Pt.createFromMembersxy` | `pt: ^T -> Pt` |
| `Pt.createFromPnt` | `p: Pnt -> Pt` |
| `Pt.createFromUnitVc` | `v: UnitVc -> Pt` |
| `Pt.createFromVc` | `v: Vc -> Pt` |
| `Pt.distPt` | `fromPt: Pt -> dirPt: Pt -> distance: float -> Pt` |
| `Pt.DivideByInt` | `pt: Pt * i: int -> Pt` |
| `Pt.divPt` | `fromPt: Pt -> toPt: Pt -> rel: float -> Pt` |
| `Pt.equalsExact` | `a: Pt -> b: Pt -> bool` |
| `Pt.equalsWithin` | `tol: float -> a: Pt -> b: Pt -> bool` |
| `Pt.getX` | `pt: Pt -> float` |
| `Pt.getY` | `pt: Pt -> float` |
| `Pt.lerp` | `fromPt: Pt -> toPt: Pt -> rel: float -> Pt` |
| `Pt.midPt` | `a: Pt -> b: Pt -> Pt` |
| `Pt.move` | `shift: Vc -> pt: Pt -> Pt` |
| `Pt.moveX` | `x: float -> pt: Pt -> Pt` |
| `Pt.moveY` | `y: float -> pt: Pt -> Pt` |
| `Pt.notEqualsWithin` | `tol: float -> a: Pt -> b: Pt -> bool` |
| `Pt.Origin` | `Pt` |
| `Pt.projectedParameterBetweenPts` | `fromPt: Pt -> toPt: Pt -> testPt: Pt -> float` |
| `Pt.projectedParameterOnUnitVc` | `fromPt: Pt -> uv: UnitVc -> testPt: Pt -> float` |
| `Pt.projectedParameterOnVc` | `fromPt: Pt -> v: Vc -> testPt: Pt -> float` |
| `Pt.scale` | `f: float -> pt: Pt -> Pt` |
| `Pt.scaleOn` | `cen: Pt -> factor: float -> pt: Pt -> Pt` |
| `Pt.snap` | `precision: float -> pt: Pt -> Pt` |
| `Pt.snapIfClose` | `snapDistance: float -> snapTo: Pt -> pt: Pt -> Pt` |
| `Pt.Zero` | `Pt` |
| `( * )` | `a: Pt * f: float -> Pt` |
| `( * )` | `f: float * a: Pt -> Pt` |
| `( + )` | `p: Pt * v: Vc -> Pt` |
| `( + )` | `p: Pt * v: UnitVc -> Pt` |
| `( + )` | `a: Pt * b: Pt -> Pt` |
| `( - )` | `a: Pt * b: Pt -> Vc` |
| `( - )` | `a: Pt * b: Vc -> Pt` |
| `( - )` | `a: Pt * b: UnitVc -> Pt` |
| `( / )` | `p: Pt * f: float -> Pt` |

## Rotation2D

| Rotation2D | Signature |
|:--|:--|
| `.Sin` | `float` |
| `.Cos` | `float` |
| `.AddDegrees` | `deg: float -> Rotation2D` |
| `Rotation2D.addDegrees` | `deg: float -> r: Rotation2D -> Rotation2D` |
| `.AddRadians` | `rad: float -> Rotation2D` |
| `Rotation2D.addRadians` | `rad: float -> r: Rotation2D -> Rotation2D` |
| `.AngleInDegrees` | `float` |
| `Rotation2D.angleInDegrees` | `r: Rotation2D -> float` |
| `.AngleInRadians` | `float` |
| `Rotation2D.angleInRadians` | `r: Rotation2D -> float` |
| `.AsFSharpCode` | `string` |
| `Rotation2D.asFSharpCode` | `r: Rotation2D -> string` |
| `.AsString` | `string` |
| `Rotation2D.asString` | `r: Rotation2D -> string` |
| `.Half` | `Rotation2D` |
| `Rotation2D.half` | `rotation: Rotation2D -> Rotation2D` |
| `.Inverse` | `Rotation2D` |
| `Rotation2D.inverse` | `r: Rotation2D -> Rotation2D` |
| `.ScaleAngle` | `factor: float -> Rotation2D` |
| `Rotation2D.scaleAngle` | `factor: float -> rotation: Rotation2D -> Rotation2D` |
| `.Then` | `ro: Rotation2D -> Rotation2D` |
| `.ToString` | `unit -> string` |
| **Other static members** | |
| `Rotation2D.andThen` | `ro: Rotation2D -> r: Rotation2D -> Rotation2D` |
| `Rotation2D.create` | `sine: float -> cosine: float -> Rotation2D` |
| `Rotation2D.createFromCosine` | `cos: float -> Rotation2D` |
| `Rotation2D.createFromDegrees` | `deg: float -> Rotation2D` |
| `Rotation2D.createFromRadians` | `rad: float -> Rotation2D` |
| `Rotation2D.createFromSine` | `sin: float -> Rotation2D` |
| `Rotation2D.createFromUnitVcs` | `a: UnitVc -> b: UnitVc -> Rotation2D` |
| `Rotation2D.createFromVcs` | `a: Vc -> b: Vc -> Rotation2D` |
| `Rotation2D.createUnchecked` | `sine: float * cosine: float -> Rotation2D` |
| `Rotation2D.equalsExact` | `a: Rotation2D -> b: Rotation2D -> bool` |
| `Rotation2D.equalsWithin` | `tol: float -> a: Rotation2D -> b: Rotation2D -> bool` |
| `Rotation2D.identity` | `Rotation2D` |

## Line2D

| Line2D | Signature |
|:--|:--|
| `new` | `fromPt: Pt * toPt: Pt -> Line2D` |
| `new` | `fromX: float * fromY: float * toX: float * toY: float -> Line2D` |
| `.FromX` | `float` |
| `.FromY` | `float` |
| `.ToX` | `float` |
| `.ToY` | `float` |
| `.AsFSharpCode` | `string` |
| `Line2D.asFSharpCode` | `l: Line2D -> string` |
| `.AsString` | `string` |
| `Line2D.asString` | `l: Line2D -> string` |
| `.ClosestParameter` | `p: Pt -> float` |
| `Line2D.closestParameter` | `p: Pt -> ln: Line2D -> float` |
| `.ClosestParameters` | `lnB: Line2D -> float * float` |
| `Line2D.closestParameters` | `lnA: Line2D -> lnB: Line2D -> float * float` |
| `.ClosestParameterXY` | `x: float * y: float -> float` |
| `Line2D.closestParameterXY` | `x: float -> y: float -> ln: Line2D -> float` |
| `.ClosestPoint` | `p: Pt -> Pt` |
| `Line2D.closestPoint` | `p: Pt -> ln: Line2D -> Pt` |
| `.ClosestPoints` | `lnB: Line2D -> Pt * Pt` |
| `Line2D.closestPoints` | `lnA: Line2D -> lnB: Line2D -> Pt * Pt` |
| `.Direction` | `Vc` |
| `Line2D.direction` | `ln: Line2D -> Vc` |
| `.DistanceRayPoint` | `p: Pt -> float` |
| `Line2D.distanceRayPoint` | `p: Pt -> ln: Line2D -> float` |
| `.DistanceRayPointSq` | `p: Pt -> float` |
| `Line2D.distanceRayPointSq` | `p: Pt -> ln: Line2D -> float` |
| `.DistanceRayXY` | `x: float * y: float -> float` |
| `Line2D.distanceRayXY` | `x: float -> y: float -> ln: Line2D -> float` |
| `.DistanceRayXYSq` | `x: float * y: float -> float` |
| `Line2D.distanceRayXYSq` | `x: float -> y: float -> ln: Line2D -> float` |
| `.DistanceToPt` | `p: Pt -> float` |
| `Line2D.distanceToPt` | `p: Pt -> ln: Line2D -> float` |
| `.DistanceToPtSq` | `p: Pt -> float` |
| `Line2D.distanceToPtSq` | `p: Pt -> ln: Line2D -> float` |
| `.DistanceToXY` | `x: float * y: float -> float` |
| `Line2D.distanceToXY` | `x: float -> y: float -> ln: Line2D -> float` |
| `.DistanceToXYSq` | `x: float * y: float -> float` |
| `Line2D.distanceToXYSq` | `x: float -> y: float -> ln: Line2D -> float` |
| `.End` | `Pt` |
| `.EvaluateAt` | `p: float -> Pt` |
| `Line2D.evaluateAt` | `t: float -> ln: Line2D -> Pt` |
| `.Extend` | `distAtStart: float * distAtEnd: float -> Line2D` |
| `Line2D.extend` | `distAtStart: float -> distAtEnd: float -> ln: Line2D -> Line2D` |
| `.ExtendEnd` | `distAtEnd: float -> Line2D` |
| `Line2D.extendEnd` | `distAtEnd: float -> ln: Line2D -> Line2D` |
| `.ExtendEndRel` | `relAtEnd: float -> Line2D` |
| `Line2D.extendEndRel` | `relAtEnd: float -> ln: Line2D -> Line2D` |
| `.ExtendRel` | `relAtStart: float * relAtEnd: float -> Line2D` |
| `Line2D.extendRel` | `relAtStart: float -> relAtEnd: float -> ln: Line2D -> Line2D` |
| `.ExtendStart` | `distAtStart: float -> Line2D` |
| `Line2D.extendStart` | `distAtStart: float -> ln: Line2D -> Line2D` |
| `.ExtendStartRel` | `relAtStart: float -> Line2D` |
| `Line2D.extendStartRel` | `relAtStart: float -> ln: Line2D -> Line2D` |
| `.From` | `Pt` |
| `Line2D.from` | `l: Line2D -> Pt` |
| `.IsCoincidentAndOpposingToFast` | `other: Line2D * parallelogramAreaTolerance: float * maxDotProduct: float -> bool` |
| `Line2D.isCoincidentAndOpposingToFast` | `parallelogramAreaTolerance: float -> maxDotProduct: float -> lnA: Line2D -> lnB: Line2D -> bool` |
| `.IsCoincidentAndOrientedToFast` | `other: Line2D * parallelogramAreaTolerance: float * minDotProduct: float -> bool` |
| `Line2D.isCoincidentAndOrientedToFast` | `parallelogramAreaTolerance: float -> minDotProduct: float -> lnA: Line2D -> lnB: Line2D -> bool` |
| `.IsCoincidentTo` | `other: Line2D * distanceTolerance: float * minTangent: float<tangent> -> bool` |
| `Line2D.isCoincidentTo` | `distanceTolerance: float -> minTangent: float<tangent> -> a: Line2D -> b: Line2D -> bool` |
| `.IsCoincidentToFast` | `other: Line2D * parallelogramAreaTolerance: float -> bool` |
| `Line2D.isCoincidentToFast` | `parallelogramAreaTolerance: float -> lnA: Line2D -> lnB: Line2D -> bool` |
| `.IsNormalTo` | `other: Line2D * maxTangent: float<tangent> -> bool` |
| `.IsNormalTo` | `other: Vc * maxTangent: float<tangent> -> bool` |
| `.IsNormalTo` | `other: UnitVc * maxTangent: float<tangent> -> bool` |
| `Line2D.isNormalTo` | `maxTangent: float<tangent> -> lnA: Line2D -> lnB: Line2D -> bool` |
| `.IsParallelAndOpposingToFast` | `other: Line2D * parallelogramAreaTolerance: float * maxDotProduct: float -> bool` |
| `Line2D.isParallelAndOpposingToFast` | `parallelogramAreaTolerance: float -> maxDotProduct: float -> lnA: Line2D -> lnB: Line2D -> bool` |
| `.IsParallelAndOrientedTo` | `other: Line2D * minTangent: float<tangent> -> bool` |
| `.IsParallelAndOrientedTo` | `other: Vc * minTangent: float<tangent> -> bool` |
| `.IsParallelAndOrientedTo` | `other: UnitVc * minTangent: float<tangent> -> bool` |
| `Line2D.isParallelAndOrientedTo` | `minTangent: float<tangent> -> lnA: Line2D -> lnB: Line2D -> bool` |
| `.IsParallelAndOrientedToFast` | `other: Line2D * parallelogramAreaTolerance: float * minDotProduct: float -> bool` |
| `Line2D.isParallelAndOrientedToFast` | `parallelogramAreaTolerance: float -> minDotProduct: float -> lnA: Line2D -> lnB: Line2D -> bool` |
| `.IsParallelTo` | `other: Line2D * minTangent: float<tangent> -> bool` |
| `.IsParallelTo` | `other: Vc * minTangent: float<tangent> -> bool` |
| `.IsParallelTo` | `other: UnitVc * minTangent: float<tangent> -> bool` |
| `Line2D.isParallelTo` | `minTangent: float<tangent> -> lnA: Line2D -> lnB: Line2D -> bool` |
| `.IsParallelToFast` | `other: Line2D * parallelogramAreaTolerance: float -> bool` |
| `Line2D.isParallelToFast` | `parallelogramAreaTolerance: float -> lnA: Line2D -> lnB: Line2D -> bool` |
| `.IsPerpendicularTo` | `other: Line2D * maxTangent: float<tangent> -> bool` |
| `.IsPerpendicularTo` | `other: Vc * maxTangent: float<tangent> -> bool` |
| `.IsPerpendicularTo` | `other: UnitVc * maxTangent: float<tangent> -> bool` |
| `Line2D.isPerpendicularTo` | `maxTangent: float<tangent> -> lnA: Line2D -> lnB: Line2D -> bool` |
| `.IsPointOnLeft` | `pt: Pt -> bool` |
| `Line2D.isPointOnLeft` | `pt: Pt -> ln: Line2D -> bool` |
| `.IsPointOnRight` | `pt: Pt -> bool` |
| `Line2D.isPointOnRight` | `pt: Pt -> ln: Line2D -> bool` |
| `.IsTiny` | `tol: float -> bool` |
| `Line2D.isTiny` | `tol: float -> l: Line2D -> bool` |
| `.IsTinySq` | `tol: float -> bool` |
| `Line2D.isTinySq` | `tol: float -> l: Line2D -> bool` |
| `.IsXAligned` | `bool` |
| `Line2D.isXAligned` | `l: Line2D -> bool` |
| `.IsXYOnLeft` | `x: float * y: float -> bool` |
| `Line2D.isXYOnLeft` | `x: float -> y: float -> ln: Line2D -> bool` |
| `.IsXYOnRight` | `x: float * y: float -> bool` |
| `Line2D.isXYOnRight` | `x: float -> y: float -> ln: Line2D -> bool` |
| `.IsYAligned` | `bool` |
| `Line2D.isYAligned` | `l: Line2D -> bool` |
| `.IsZeroLength` | `bool` |
| `Line2D.isZeroLength` | `l: Line2D -> bool` |
| `.Length` | `float` |
| `Line2D.length` | `l: Line2D -> float` |
| `.LengthFromParam` | `t: float -> float` |
| `Line2D.lengthFromParam` | `t: float -> ln: Line2D -> float` |
| `.LengthSq` | `float` |
| `Line2D.lengthSq` | `l: Line2D -> float` |
| `.LengthTillParam` | `p: float -> float` |
| `Line2D.lengthTillParam` | `p: float -> ln: Line2D -> float` |
| `.MatchesOrientation` | `otherLn: Line2D -> bool` |
| `.MatchesOrientation` | `v: Vc -> bool` |
| `.MatchesOrientation` | `v: UnitVc -> bool` |
| `Line2D.matchesOrientation` | `l: Line2D -> ln: Line2D -> bool` |
| `.MatchesOrientation45` | `l: Line2D -> bool` |
| `Line2D.matchesOrientation45` | `l: Line2D -> ln: Line2D -> bool` |
| `.Mid` | `Pt` |
| `Line2D.mid` | `ln: Line2D -> Pt` |
| `.Move` | `v: Vc -> Line2D` |
| `Line2D.move` | `v: Vc -> ln: Line2D -> Line2D` |
| `.MoveX` | `distance: float -> Line2D` |
| `Line2D.moveX` | `distance: float -> ln: Line2D -> Line2D` |
| `.MoveY` | `distance: float -> Line2D` |
| `Line2D.moveY` | `distance: float -> ln: Line2D -> Line2D` |
| `.RayClosestParameter` | `pt: Pt -> float` |
| `Line2D.rayClosestParameter` | `p: Pt -> ln: Line2D -> float` |
| `.RayClosestParameterXY` | `x: float * y: float -> float` |
| `Line2D.rayClosestParameterXY` | `x: float -> y: float -> ln: Line2D -> float` |
| `.RayClosestPoint` | `pt: Pt -> Pt` |
| `Line2D.rayClosestPoint` | `p: Pt -> ln: Line2D -> Pt` |
| `.Reversed` | `Line2D` |
| `Line2D.reversed` | `ln: Line2D -> Line2D` |
| `.Rotate` | `r: Rotation2D -> Line2D` |
| `Line2D.rotate` | `r: Rotation2D -> ln: Line2D -> Line2D` |
| `.RotateAround` | `cen: Pt -> r: Rotation2D -> Line2D` |
| `Line2D.rotateAround` | `cen: Pt -> r: Rotation2D -> ln: Line2D -> Line2D` |
| `.RotateAroundDegrees` | `cen: Pt -> angDegree: float -> Line2D` |
| `Line2D.rotateAroundDegrees` | `cen: Pt -> angDegree: float -> ln: Line2D -> Line2D` |
| `.RotateAroundRadians` | `cen: Pt -> angRadians: float -> Line2D` |
| `Line2D.rotateAroundRadians` | `cen: Pt -> angRadians: float -> ln: Line2D -> Line2D` |
| `.RotateDegrees` | `angDegree: float -> Line2D` |
| `Line2D.rotateDegrees` | `angDegree: float -> ln: Line2D -> Line2D` |
| `.RotateRadians` | `angRadians: float -> Line2D` |
| `Line2D.rotateRadians` | `angRadians: float -> ln: Line2D -> Line2D` |
| `.Scale` | `factor: float -> Line2D` |
| `Line2D.scale` | `factor: float -> ln: Line2D -> Line2D` |
| `.ScaleOn` | `cen: Pt -> factor: float -> Line2D` |
| `Line2D.scaleOn` | `cen: Pt -> factor: float -> ln: Line2D -> Line2D` |
| `.Segment` | `start: float * endParameter: float -> Line2D` |
| `Line2D.segment` | `start: float -> endParameter: float -> ln: Line2D -> Line2D` |
| `.Shrink` | `distAtStart: float * distAtEnd: float -> Line2D` |
| `Line2D.shrink` | `distAtStart: float -> distAtEnd: float -> ln: Line2D -> Line2D` |
| `.ShrinkEnd` | `distAtEnd: float -> Line2D` |
| `Line2D.shrinkEnd` | `distAtEnd: float -> ln: Line2D -> Line2D` |
| `.ShrinkStart` | `distAtStart: float -> Line2D` |
| `Line2D.shrinkStart` | `distAtStart: float -> ln: Line2D -> Line2D` |
| `.Start` | `Pt` |
| `Line2D.start` | `l: Line2D -> Pt` |
| `.SubLine` | `start: float * endParameter: float -> Line2D` |
| `Line2D.subLine` | `start: float -> endParameter: float -> ln: Line2D -> Line2D` |
| `.Tangent` | `Vc` |
| `Line2D.tangent` | `ln: Line2D -> Vc` |
| `.To` | `Pt` |
| `.ToString` | `unit -> string` |
| `.UnitTangent` | `UnitVc` |
| `Line2D.unitTangent` | `ln: Line2D -> UnitVc` |
| `.Vector` | `Vc` |
| `Line2D.vector` | `ln: Line2D -> Vc` |
| `.VectorX` | `float` |
| `Line2D.vectorX` | `l: Line2D -> float` |
| `.VectorY` | `float` |
| `Line2D.vectorY` | `l: Line2D -> float` |
| **Other static members** | |
| `Line2D.create` | `start: Pt -> finish: Pt -> Line2D` |
| `Line2D.createFromLine3D` | `ln: Line3D -> Line2D` |
| `Line2D.createFromPtAndVc` | `p: Pt -> v: Vc -> Line2D` |
| `Line2D.createFromVc` | `v: Vc -> Line2D` |
| `Line2D.cross` | `a: Line2D * b: Line2D -> float` |
| `Line2D.distanceToLine` | `lnA: Line2D -> lnB: Line2D -> float` |
| `Line2D.distanceToLineSq` | `lnA: Line2D -> lnB: Line2D -> float` |
| `Line2D.divide` | `segments: int -> ln: Line2D -> Pt array` |
| `Line2D.divideEvery` | `dist: float -> l: Line2D -> ResizeArray<Pt>` |
| `Line2D.divideInsideEvery` | `dist: float -> l: Line2D -> ResizeArray<Pt>` |
| `Line2D.divideMaxLength` | `maxSegmentLength: float -> ln: Line2D -> Pt array` |
| `Line2D.divideMinLength` | `minSegmentLength: float -> ln: Line2D -> Pt array` |
| `Line2D.doIntersect` | `lnA: Line2D -> lnB: Line2D -> bool` |
| `Line2D.doIntersectOrOverlap` | `lnA: Line2D -> lnB: Line2D -> bool` |
| `Line2D.end'` | `l: Line2D -> Pt` |
| `Line2D.equalsExact` | `a: Line2D -> b: Line2D -> bool` |
| `Line2D.equalsWithin` | `tol: float -> a: Line2D -> b: Line2D -> bool` |
| `Line2D.fromX` | `l: Line2D -> float` |
| `Line2D.fromY` | `l: Line2D -> float` |
| `Line2D.intersectRays` | `lnA: Line2D -> lnB: Line2D -> Pt` |
| `Line2D.isNormalTo'` | `lnA: Line2D -> lnB: Line2D -> bool` |
| `Line2D.isNormalToUnitVc` | `maxTangent: float<tangent> -> other: UnitVc -> ln: Line2D -> bool` |
| `Line2D.isNormalToVc` | `maxTangent: float<tangent> -> other: Vc -> ln: Line2D -> bool` |
| `Line2D.isNotParallelTo` | `lnA: Line2D -> lnB: Line2D -> bool` |
| `Line2D.isNotParallelTo'` | `lnA: Line2D -> lnB: Line2D -> bool` |
| `Line2D.isParallelAndOrientedToUnitVc` | `minTangent: float<tangent> -> other: UnitVc -> ln: Line2D -> bool` |
| `Line2D.isParallelAndOrientedToVc` | `minTangent: float<tangent> -> other: Vc -> ln: Line2D -> bool` |
| `Line2D.isParallelTo'` | `lnA: Line2D -> lnB: Line2D -> bool` |
| `Line2D.isParallelToUnitVc` | `minTangent: float<tangent> -> other: UnitVc -> ln: Line2D -> bool` |
| `Line2D.isParallelToVc` | `minTangent: float<tangent> -> other: Vc -> ln: Line2D -> bool` |
| `Line2D.isPerpendicularTo'` | `lnA: Line2D -> lnB: Line2D -> bool` |
| `Line2D.isPerpendicularToUnitVc` | `maxTangent: float<tangent> -> other: UnitVc -> ln: Line2D -> bool` |
| `Line2D.isPerpendicularToVc` | `maxTangent: float<tangent> -> other: Vc -> ln: Line2D -> bool` |
| `Line2D.isTouchingEndOf` | `squareTolerance: float -> a: Line2D -> b: Line2D -> bool` |
| `Line2D.lengthToPtOnLine` | `pt: Pt -> ln: Line2D -> float` |
| `Line2D.matchesOrientationUnitVc` | `v: UnitVc -> ln: Line2D -> bool` |
| `Line2D.matchesOrientationVc` | `v: Vc -> ln: Line2D -> bool` |
| `Line2D.matchOrientation` | `orientationToMatch: Line2D -> lineToFlip: Line2D -> Line2D` |
| `Line2D.matchUnitVcOrientation` | `orientationToMatch: UnitVc -> lineToFlip: Line2D -> Line2D` |
| `Line2D.matchVcOrientation` | `orientationToMatch: Vc -> lineToFlip: Line2D -> Line2D` |
| `Line2D.notEqualsWithin` | `tol: float -> a: Line2D -> b: Line2D -> bool` |
| `Line2D.offset` | `distance: float -> ln: Line2D -> Line2D` |
| `Line2D.pointAtDistance` | `dist: float -> ln: Line2D -> Pt` |
| `Line2D.pointAtDistanceFromEnd` | `dist: float -> ln: Line2D -> Pt` |
| `Line2D.projectOntoRay` | `rayToProjectOnto: Line2D -> lineToProject: Line2D -> Line2D` |
| `Line2D.projectOntoRayParam` | `rayToProjectOnto: Line2D -> lineToProject: Line2D -> float * float` |
| `Line2D.reverse` | `ln: Line2D -> Line2D` |
| `Line2D.split` | `gap: float -> segments: int -> ln: Line2D -> Line2D array` |
| `Line2D.splitMaxLength` | `gap: float -> maxSegmentLength: float -> ln: Line2D -> Line2D array` |
| `Line2D.splitMinLength` | `gap: float -> minSegmentLength: float -> ln: Line2D -> Line2D array` |
| `Line2D.to'` | `l: Line2D -> Pt` |
| `Line2D.toLine3D` | `zLevel: float -> ln: Line2D -> Line3D` |
| `Line2D.toX` | `l: Line2D -> float` |
| `Line2D.toY` | `l: Line2D -> float` |
| `Line2D.tryGetOverlap` | `lnA: Line2D -> lnB: Line2D -> (float * float) voption` |
| `Line2D.tryIntersect` | `lnA: Line2D -> lnB: Line2D -> Pt voption` |
| `Line2D.tryIntersectOrOverlap` | `lnA: Line2D -> lnB: Line2D -> Pt voption` |
| `Line2D.tryIntersectRay` | `lineA: Line2D -> lineB: Line2D -> Pt voption` |
| `Line2D.tryProjectOntoLine` | `onToLine: Line2D -> lineToProject: Line2D -> Line2D voption` |
| `Line2D.tryProjectOntoLineParam` | `onToLine: Line2D -> lineToProject: Line2D -> (float * float) voption` |
| `Line2D.withEnd` | `pt: Pt -> ln: Line2D -> Line2D` |
| `Line2D.withFrom` | `pt: Pt -> ln: Line2D -> Line2D` |
| `Line2D.withLengthFromMid` | `len: float -> ln: Line2D -> Line2D` |
| `Line2D.withLengthFromStart` | `len: float -> ln: Line2D -> Line2D` |
| `Line2D.withLengthToEnd` | `len: float -> ln: Line2D -> Line2D` |
| `Line2D.withStart` | `pt: Pt -> ln: Line2D -> Line2D` |
| `Line2D.withTo` | `pt: Pt -> ln: Line2D -> Line2D` |

## XLine2D.XRayParam

| XRayParam | Signature |
|:--|:--|
| `Intersect` | `twoParams: float * float -> XRayParam` |
| `Parallel` | `XRayParam` |
| `TooShortA` | `XRayParam` |
| `TooShortB` | `XRayParam` |
| `TooShortBoth` | `XRayParam` |

## XLine2D.XRay

| XRay | Signature |
|:--|:--|
| `Intersect` | `xPt: Pt -> XRay` |
| `Parallel` | `XRay` |
| `TooShortA` | `XRay` |
| `TooShortB` | `XRay` |
| `TooShortBoth` | `XRay` |

## XLine2D.XParam

| XParam | Signature |
|:--|:--|
| `Intersect` | `twoParams: float * float -> XParam` |
| `Apart` | `XParam` |
| `Parallel` | `XParam` |
| `TooShortA` | `XParam` |
| `TooShortB` | `XParam` |
| `TooShortBoth` | `XParam` |

## XLine2D.XPt

| XPt | Signature |
|:--|:--|
| `Intersect` | `xPt: Pt -> XPt` |
| `Apart` | `XPt` |
| `Parallel` | `XPt` |
| `TooShortA` | `XPt` |
| `TooShortB` | `XPt` |
| `TooShortBoth` | `XPt` |

## XLine2D.ClParams

| ClParams | Signature |
|:--|:--|
| `Intersect` | `paramA: float * paramB: float -> ClParams` |
| `Parallel` | `paramA: float * paramB: float -> ClParams` |
| `Apart` | `paramA: float * paramB: float -> ClParams` |
| `TooShortA` | `ClParams` |
| `TooShortB` | `ClParams` |
| `TooShortBoth` | `ClParams` |

## XLine2D.ClPts

| ClPts | Signature |
|:--|:--|
| `Intersect` | `pt: Pt -> ClPts` |
| `Parallel` | `ptA: Pt * ptB: Pt -> ClPts` |
| `Apart` | `ptA: Pt * ptB: Pt -> ClPts` |
| `TooShortA` | `ClPts` |
| `TooShortB` | `ClPts` |
| `TooShortBoth` | `ClPts` |

## XLine2D.XEnds

| XEnds | Signature |
|:--|:--|
| `NotTouching` | `XEnds` |
| `StartA_StartB` | `XEnds` |
| `EndA_EndB` | `XEnds` |
| `EndA_StartB` | `XEnds` |
| `StartA_EndB` | `XEnds` |
| `Identical` | `XEnds` |
| `IdenticalFlipped` | `XEnds` |

## module XLineXY

| XLineXY | Signature |
|:--|:--|
| `XLineXY.classifyClosestParameters` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tangent: float<tangent> * tooShortTolerance: float -> ClParams` |
| `XLineXY.classifyClosestPoints` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tangent: float<tangent> * tooShortTolerance: float -> ClPts` |
| `XLineXY.classifyIntersection` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tangent: float<tangent> * tooShortTolerance: float -> XPt` |
| `XLineXY.classifyIntersectionParam` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tangent: float<tangent> * tooShortTolerance: float -> XParam` |
| `XLineXY.classifyRayIntersection` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tangent: float<tangent> * tooShortTolerance: float -> XRay` |
| `XLineXY.classifyRayIntersectionParam` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tangent: float<tangent> * tooShortTolerance: float -> XRayParam` |
| `XLineXY.closestParameters` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> struct (float * float)` |
| `XLineXY.distanceSq` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> float` |
| `XLineXY.doIntersect` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> bool` |
| `XLineXY.doOverlap` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tolerance: float -> bool` |
| `XLineXY.intersectRays` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> Pt` |
| `XLineXY.isWithinRanges` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> bool` |
| `XLineXY.parameterA` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> float` |
| `XLineXY.parameters` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> float * float` |
| `XLineXY.tryIntersect` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> Pt voption` |
| `XLineXY.tryIntersectInRangeA` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * minParameterA: float * maxParameterA: float -> Pt voption` |
| `XLineXY.tryIntersectInRanges` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> Pt voption` |
| `XLineXY.tryIntersectRay` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float * tangent: float<tangent> -> Pt voption` |
| `XLineXY.tryParameterA` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> float voption` |

## XLine2D

| XLine2D | Signature |
|:--|:--|
| `XLine2D.classifyClosestParameters` | `lineA: Line2D -> lineB: Line2D -> ClParams` |
| `XLine2D.classifyClosestParametersCustom` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` |
| `XLine2D.classifyClosestParametersCustom` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` |
| `XLine2D.classifyClosestParametersFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> ClParams` |
| `XLine2D.classifyClosestPoints` | `lineA: Line2D -> lineB: Line2D -> ClPts` |
| `XLine2D.classifyClosestPointsCustom` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPts` |
| `XLine2D.classifyClosestPointsCustom` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPts` |
| `XLine2D.classifyClosestPointsFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> ClPts` |
| `XLine2D.classifyEndsTouching` | `a: Line2D -> b: Line2D -> XEnds` |
| `XLine2D.classifyEndsTouchingCustom` | `a: Line2D * b: Line2D * ?tolerance: float -> XEnds` |
| `XLine2D.classifyIntersection` | `lineA: Line2D -> lineB: Line2D -> XPt` |
| `XLine2D.classifyIntersectionCustom` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPt` |
| `XLine2D.classifyIntersectionCustom` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPt` |
| `XLine2D.classifyIntersectionFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> XPt` |
| `XLine2D.classifyIntersectionParam` | `lnA: Line2D -> lnB: Line2D -> XParam` |
| `XLine2D.classifyIntersectionParamCustom` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` |
| `XLine2D.classifyIntersectionParamCustom` | `lnA: Line2D * lnB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` |
| `XLine2D.classifyIntersectionParamFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> XParam` |
| `XLine2D.classifyRayIntersection` | `lineA: Line2D -> lineB: Line2D -> XRay` |
| `XLine2D.classifyRayIntersectionCustom` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` |
| `XLine2D.classifyRayIntersectionCustom` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` |
| `XLine2D.classifyRayIntersectionFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> XRay` |
| `XLine2D.classifyRayIntersectionParam` | `lineA: Line2D -> lineB: Line2D -> XRayParam` |
| `XLine2D.classifyRayIntersectionParamCustom` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` |
| `XLine2D.classifyRayIntersectionParamCustom` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` |
| `XLine2D.classifyRayIntersectionParamFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> XRayParam` |
| `XLine2D.closestParameters` | `lineA: Line2D -> lineB: Line2D -> struct (float * float)` |
| `XLine2D.closestParametersFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> struct (float * float)` |
| `XLine2D.distanceSq` | `lineA: Line2D -> lineB: Line2D -> float` |
| `XLine2D.distanceSqFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> float` |
| `XLine2D.doIntersect` | `lineA: Line2D -> lineB: Line2D -> bool` |
| `XLine2D.doIntersectFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> bool` |
| `XLine2D.doOverlap` | `lineA: Line2D -> lineB: Line2D -> bool` |
| `XLine2D.doOverlapCustom` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tolerance: float -> bool` |
| `XLine2D.doOverlapCustom` | `lineA: Line2D * lineB: Line2D * ?tolerance: float -> bool` |
| `XLine2D.doOverlapFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> bool` |
| `XLine2D.intersectRays` | `lineA: Line2D -> lineB: Line2D -> Pt` |
| `XLine2D.intersectRaysFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> Pt` |
| `XLine2D.isWithinRanges` | `lineA: Line2D * lineB: Line2D * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> bool` |
| `XLine2D.isWithinRangesFromPtsAndVcs` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> bool` |
| `XLine2D.parameterA` | `lineA: Line2D -> lineB: Line2D -> float` |
| `XLine2D.parameterAFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> float` |
| `XLine2D.parameters` | `lineA: Line2D -> lineB: Line2D -> float * float` |
| `XLine2D.parametersFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> float * float` |
| `XLine2D.tryIntersect` | `lineA: Line2D -> lineB: Line2D -> Pt voption` |
| `XLine2D.tryIntersectFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> Pt voption` |
| `XLine2D.tryIntersectInRangeA` | `minParameterA: float -> maxParameterA: float -> lineA: Line2D -> lineB: Line2D -> Pt voption` |
| `XLine2D.tryIntersectInRangeAFromPtsAndVcs` | `minParameterA: float -> maxParameterA: float -> pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> Pt voption` |
| `XLine2D.tryIntersectInRanges` | `lineA: Line2D * lineB: Line2D * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> Pt voption` |
| `XLine2D.tryIntersectInRangesFromPtsAndVcs` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> Pt voption` |
| `XLine2D.tryIntersectRay` | `lineA: Line2D -> lineB: Line2D -> Pt voption` |
| `XLine2D.tryIntersectRayCustom` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> -> Pt voption` |
| `XLine2D.tryIntersectRayCustom` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> -> Pt voption` |
| `XLine2D.tryIntersectRayFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> Pt voption` |
| `XLine2D.tryParameterA` | `lineA: Line2D -> lineB: Line2D -> float voption` |
| `XLine2D.tryParameterAFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> float voption` |

## Rect2D

| Rect2D | Signature |
|:--|:--|
| `.OriginX` | `float` |
| `.OriginY` | `float` |
| `.XAxisX` | `float` |
| `.XAxisY` | `float` |
| `.YAxisX` | `float` |
| `.YAxisY` | `float` |
| `.Area` | `float` |
| `Rect2D.area` | `r: Rect2D -> float` |
| `.AsFSharpCode` | `string` |
| `Rect2D.asFSharpCode` | `r: Rect2D -> string` |
| `.AsString` | `string` |
| `Rect2D.asString` | `r: Rect2D -> string` |
| `.Center` | `Pt` |
| `Rect2D.center` | `r: Rect2D -> Pt` |
| `.ClosestParameters` | `p: Pt -> float * float` |
| `Rect2D.closestParameters` | `p: Pt -> r: Rect2D -> float * float` |
| `.ClosestPoint` | `p: Pt -> Pt` |
| `Rect2D.closestPoint` | `p: Pt -> r: Rect2D -> Pt` |
| `.Contains` | `p: Pt -> bool` |
| `Rect2D.contains` | `p: Pt -> r: Rect2D -> bool` |
| `.ContainsXY` | `x: float * y: float -> bool` |
| `Rect2D.containsXY` | `x: float -> y: float -> r: Rect2D -> bool` |
| `.CountZeroSides` | `int` |
| `Rect2D.countZeroSides` | `r: Rect2D -> int` |
| `.Diagonal` | `Vc` |
| `Rect2D.diagonal` | `r: Rect2D -> Vc` |
| `.DiagonalLine` | `Line2D` |
| `Rect2D.diagonalLine` | `r: Rect2D -> Line2D` |
| `.Edge01` | `Line2D` |
| `Rect2D.edge01` | `r: Rect2D -> Line2D` |
| `.Edge03` | `Line2D` |
| `Rect2D.edge03` | `r: Rect2D -> Line2D` |
| `.Edge10` | `Line2D` |
| `Rect2D.edge10` | `r: Rect2D -> Line2D` |
| `.Edge12` | `Line2D` |
| `Rect2D.edge12` | `r: Rect2D -> Line2D` |
| `.Edge21` | `Line2D` |
| `Rect2D.edge21` | `r: Rect2D -> Line2D` |
| `.Edge23` | `Line2D` |
| `Rect2D.edge23` | `r: Rect2D -> Line2D` |
| `.Edge30` | `Line2D` |
| `Rect2D.edge30` | `r: Rect2D -> Line2D` |
| `.Edge32` | `Line2D` |
| `Rect2D.edge32` | `r: Rect2D -> Line2D` |
| `.Edges` | `Line2D array` |
| `Rect2D.edges` | `r: Rect2D -> Line2D array` |
| `.EdgeX` | `Line2D` |
| `Rect2D.edgeX` | `r: Rect2D -> Line2D` |
| `.EdgeY` | `Line2D` |
| `Rect2D.edgeY` | `r: Rect2D -> Line2D` |
| `.EvaluateAt` | `xParameter: float * yParameter: float -> Pt` |
| `Rect2D.evaluateAt` | `xParameter: float -> yParameter: float -> r: Rect2D -> Pt` |
| `.EvaluateDistance` | `xDistance: float * yDistance: float -> Pt` |
| `Rect2D.evaluateDistance` | `xDistance: float -> yDistance: float -> r: Rect2D -> Pt` |
| `.FarCorner` | `Pt` |
| `Rect2D.farCorner` | `r: Rect2D -> Pt` |
| `.GetEdge` | `i: int -> Line2D` |
| `Rect2D.getEdge` | `i: int -> r: Rect2D -> Line2D` |
| `.HasArea` | `bool` |
| `Rect2D.hasArea` | `r: Rect2D -> bool` |
| `.IsLine` | `bool` |
| `Rect2D.isLine` | `r: Rect2D -> bool` |
| `.IsPoint` | `bool` |
| `Rect2D.isPoint` | `r: Rect2D -> bool` |
| `.IsValid` | `bool` |
| `Rect2D.isValid` | `r: Rect2D -> bool` |
| `.IsZero` | `bool` |
| `Rect2D.isZero` | `r: Rect2D -> bool` |
| `.LongestEdge` | `float` |
| `Rect2D.longestEdge` | `r: Rect2D -> float` |
| `.LongestEdgeSq` | `float` |
| `Rect2D.longestEdgeSq` | `r: Rect2D -> float` |
| `.Move` | `v: Vc -> Rect2D` |
| `Rect2D.move` | `v: Vc -> r: Rect2D -> Rect2D` |
| `.MoveX` | `distance: float -> Rect2D` |
| `Rect2D.moveX` | `distance: float -> r: Rect2D -> Rect2D` |
| `.MoveY` | `distance: float -> Rect2D` |
| `Rect2D.moveY` | `distance: float -> r: Rect2D -> Rect2D` |
| `.Origin` | `Pt` |
| `Rect2D.origin` | `r: Rect2D -> Pt` |
| `.Points` | `Pt array` |
| `Rect2D.points` | `r: Rect2D -> Pt array` |
| `.PointsLooped` | `Pt array` |
| `Rect2D.pointsLooped` | `r: Rect2D -> Pt array` |
| `.PointsXYLooped_CCW` | `ResizeArray<float>` |
| `Rect2D.pointsXYLooped_CCW` | `r: Rect2D -> ResizeArray<float>` |
| `.PointsXYLooped_CW` | `ResizeArray<float>` |
| `Rect2D.pointsXYLooped_CW` | `r: Rect2D -> ResizeArray<float>` |
| `.PointsXY_CCW` | `ResizeArray<float>` |
| `Rect2D.pointsXY_CCW` | `r: Rect2D -> ResizeArray<float>` |
| `.PointsXY_CW` | `ResizeArray<float>` |
| `Rect2D.pointsXY_CW` | `r: Rect2D -> ResizeArray<float>` |
| `.Pt0` | `Pt` |
| `Rect2D.pt0` | `r: Rect2D -> Pt` |
| `.Pt1` | `Pt` |
| `Rect2D.pt1` | `r: Rect2D -> Pt` |
| `.Pt2` | `Pt` |
| `Rect2D.pt2` | `r: Rect2D -> Pt` |
| `.Pt3` | `Pt` |
| `Rect2D.pt3` | `r: Rect2D -> Pt` |
| `.Rotate` | `rot: Rotation2D -> Rect2D` |
| `Rect2D.rotate` | `rot: Rotation2D -> rect: Rect2D -> Rect2D` |
| `.RotateAround` | `cen: Pt -> rot: Rotation2D -> Rect2D` |
| `Rect2D.rotateAround` | `cen: Pt -> rot: Rotation2D -> rect: Rect2D -> Rect2D` |
| `.RotateAroundDegrees` | `cen: Pt -> angDegree: float -> Rect2D` |
| `Rect2D.rotateAroundDegrees` | `cen: Pt -> angDegree: float -> r: Rect2D -> Rect2D` |
| `.RotateAroundRadians` | `cen: Pt -> angRadians: float -> Rect2D` |
| `Rect2D.rotateAroundRadians` | `cen: Pt -> angRadians: float -> r: Rect2D -> Rect2D` |
| `.RotateDegrees` | `angDegree: float -> Rect2D` |
| `Rect2D.rotateDegrees` | `angDegree: float -> r: Rect2D -> Rect2D` |
| `.RotateOrientation180` | `Rect2D` |
| `Rect2D.rotateOrientation180` | `r: Rect2D -> Rect2D` |
| `.RotateOrientation90CCW` | `Rect2D` |
| `Rect2D.rotateOrientation90CCW` | `r: Rect2D -> Rect2D` |
| `.RotateOrientation90CW` | `Rect2D` |
| `Rect2D.rotateOrientation90CW` | `r: Rect2D -> Rect2D` |
| `.RotateRadians` | `angRadians: float -> Rect2D` |
| `Rect2D.rotateRadians` | `angRadians: float -> r: Rect2D -> Rect2D` |
| `.Scale` | `factor: float -> Rect2D` |
| `Rect2D.scale` | `factor: float -> r: Rect2D -> Rect2D` |
| `.ScaleOn` | `cen: Pt -> factor: float -> Rect2D` |
| `Rect2D.scaleOn` | `cen: Pt -> factor: float -> r: Rect2D -> Rect2D` |
| `.ShortestEdge` | `float` |
| `Rect2D.shortestEdge` | `r: Rect2D -> float` |
| `.ShortestEdgeSq` | `float` |
| `Rect2D.shortestEdgeSq` | `r: Rect2D -> float` |
| `.SizeX` | `float` |
| `Rect2D.sizeX` | `r: Rect2D -> float` |
| `.SizeXSq` | `float` |
| `Rect2D.sizeXSq` | `r: Rect2D -> float` |
| `.SizeY` | `float` |
| `Rect2D.sizeY` | `r: Rect2D -> float` |
| `.SizeYSq` | `float` |
| `Rect2D.sizeYSq` | `r: Rect2D -> float` |
| `.ToString` | `unit -> string` |
| `.XAxis` | `Vc` |
| `Rect2D.xAxis` | `r: Rect2D -> Vc` |
| `.XAxisUnit` | `UnitVc` |
| `Rect2D.xAxisUnit` | `r: Rect2D -> UnitVc` |
| `.XCorner` | `Pt` |
| `Rect2D.xCorner` | `r: Rect2D -> Pt` |
| `.YAxis` | `Vc` |
| `Rect2D.yAxis` | `r: Rect2D -> Vc` |
| `.YAxisUnit` | `UnitVc` |
| `Rect2D.yAxisUnit` | `r: Rect2D -> UnitVc` |
| `.YCorner` | `Pt` |
| `Rect2D.yCorner` | `r: Rect2D -> Pt` |
| **Other static members** | |
| `Rect2D.clipLine` | `line: Line2D -> rect: Rect2D -> Line2D` |
| `Rect2D.createCenteredFromUnitVc` | `sizeX: float -> sizeY: float -> directionX: UnitVc -> center: Pt -> Rect2D` |
| `Rect2D.createCenteredFromVc` | `sizeY: float -> x: Vc -> center: Pt -> Rect2D` |
| `Rect2D.createFrom3Pts` | `origin: Pt -> xPt: Pt -> yPt: Pt -> Rect2D` |
| `Rect2D.createFromBounds` | `minX: float * minY: float * maxX: float * maxY: float -> Rect2D` |
| `Rect2D.createFromLine` | `offRight: float -> offLeft: float -> line: Line2D -> Rect2D` |
| `Rect2D.createFromUnitVc` | `sizeX: float -> sizeY: float -> directionX: UnitVc -> origin: Pt -> Rect2D` |
| `Rect2D.createFromVc` | `sizeY: float -> x: Vc -> origin: Pt -> Rect2D` |
| `Rect2D.createFromVcAndPts` | `dirX: Vc -> pts: IList<Pt> -> Rect2D` |
| `Rect2D.createFromVcs` | `x: Vc -> y: Vc -> origin: Pt -> Rect2D` |
| `Rect2D.createFromVcsUnchecked` | `origin: Pt * x: Vc * y: Vc -> Rect2D` |
| `Rect2D.createUnchecked` | `originX: float * originY: float * xaxisX: float * xaxisY: float * yaxisX: float * yaxisY: float -> Rect2D` |
| `Rect2D.equalsExact` | `a: Rect2D -> b: Rect2D -> bool` |
| `Rect2D.equalsWithin` | `tol: float -> a: Rect2D -> b: Rect2D -> bool` |
| `Rect2D.expand` | `dist: float -> r: Rect2D -> Rect2D` |
| `Rect2D.expandRel` | `factor: float -> r: Rect2D -> Rect2D` |
| `Rect2D.expandRelXY` | `factorX: float -> factorY: float -> r: Rect2D -> Rect2D` |
| `Rect2D.expandXY` | `distX: float -> distY: float -> r: Rect2D -> Rect2D` |
| `Rect2D.fitToPoints` | `pts: IList<Pt> -> refRect: Rect2D -> Rect2D` |
| `Rect2D.fitToPointsXY` | `xys: IList<float> -> refRect: Rect2D -> Rect2D` |
| `Rect2D.grid` | `xCount: int -> yCount: int -> rect: Rect2D -> Pt array array` |
| `Rect2D.gridMaxLength` | `xMaxLen: float -> yMaxLen: float -> rect: Rect2D -> Pt array array` |
| `Rect2D.gridMinLength` | `xMinLen: float -> yMinLen: float -> rect: Rect2D -> Pt array array` |
| `Rect2D.intersectLine` | `line: Line2D -> rect: Rect2D -> Line2D` |
| `Rect2D.intersectRay` | `ray: Line2D -> rect: Rect2D -> float * float` |
| `Rect2D.iterPointsCCW` | `action: (float -> float -> unit) -> r: Rect2D -> unit` |
| `Rect2D.iterPointsCW` | `action: (float -> float -> unit) -> r: Rect2D -> unit` |
| `Rect2D.iterPointsLoopedCCW` | `action: (float -> float -> unit) -> r: Rect2D -> unit` |
| `Rect2D.iterPointsLoopedCW` | `action: (float -> float -> unit) -> r: Rect2D -> unit` |
| `Rect2D.moveAlongLocalX` | `distX: float -> r: Rect2D -> Rect2D` |
| `Rect2D.moveAlongLocalY` | `distY: float -> r: Rect2D -> Rect2D` |
| `Rect2D.notEqualsWithin` | `tol: float -> a: Rect2D -> b: Rect2D -> bool` |
| `Rect2D.offset` | `dist: float -> rect: Rect2D -> Rect2D` |
| `Rect2D.offsetCorner` | `rect: Rect2D * corner: int * xOffset: float * yOffset: float * xWidth: float * yHeight: float -> Rect2D` |
| `Rect2D.offsetEdge` | `rect: Rect2D * edgeIdx: int * offEdge: float * width: float * offStart: float * offEnd: float -> Rect2D` |
| `Rect2D.offsetVariable` | `dist: float array -> rect: Rect2D -> Rect2D` |
| `Rect2D.subDivide` | `xCount: int -> yCount: int -> xGap: float -> yGap: float -> rect: Rect2D -> Rect2D array array` |
| `Rect2D.subDivideMaxLength` | `rect: Rect2D * xMaxLen: float * yMaxLen: float * xGap: float * yGap: float -> Rect2D array array` |
| `Rect2D.subDivideMinLength` | `rect: Rect2D * xMinLen: float * yMinLen: float * xGap: float * yGap: float -> Rect2D array array` |
| `Rect2D.tryClipLine` | `line: Line2D -> rect: Rect2D -> Line2D voption` |
| `Rect2D.tryCreateFrom3Pts` | `origin: Pt -> xPt: Pt -> yPt: Pt -> Rect2D voption` |
| `Rect2D.tryIntersectLine` | `line: Line2D -> rect: Rect2D -> Line2D voption` |
| `Rect2D.tryIntersectRay` | `ray: Line2D -> rect: Rect2D -> (float * float) voption` |

## BRect

| BRect | Signature |
|:--|:--|
| `.MinX` | `float` |
| `.MinY` | `float` |
| `.MaxX` | `float` |
| `.MaxY` | `float` |
| `.Area` | `float` |
| `BRect.area` | `r: BRect -> float` |
| `.AsFSharpCode` | `string` |
| `BRect.asFSharpCode` | `r: BRect -> string` |
| `.AsRect2D` | `Rect2D` |
| `BRect.asRect2D` | `r: BRect -> Rect2D` |
| `.AsString` | `string` |
| `BRect.asString` | `r: BRect -> string` |
| `.Center` | `Pt` |
| `BRect.center` | `r: BRect -> Pt` |
| `.Contains` | `o: BRect -> bool` |
| `BRect.contains` | `rectInside: BRect -> surroundingRect: BRect -> bool` |
| `.ContainsPt` | `p: Pt -> bool` |
| `BRect.containsPt` | `pt: Pt -> rect: BRect -> bool` |
| `.ContainsXY` | `x: float * y: float -> bool` |
| `BRect.containsXY` | `x: float -> y: float -> rect: BRect -> bool` |
| `.CountZeroSides` | `int` |
| `BRect.countZeroSides` | `r: BRect -> int` |
| `.Diagonal` | `Vc` |
| `BRect.diagonal` | `r: BRect -> Vc` |
| `.Edge01` | `Line2D` |
| `BRect.edge01` | `r: BRect -> Line2D` |
| `.Edge03` | `Line2D` |
| `BRect.edge03` | `r: BRect -> Line2D` |
| `.Edge10` | `Line2D` |
| `BRect.edge10` | `r: BRect -> Line2D` |
| `.Edge12` | `Line2D` |
| `BRect.edge12` | `r: BRect -> Line2D` |
| `.Edge21` | `Line2D` |
| `BRect.edge21` | `r: BRect -> Line2D` |
| `.Edge23` | `Line2D` |
| `BRect.edge23` | `r: BRect -> Line2D` |
| `.Edge30` | `Line2D` |
| `BRect.edge30` | `r: BRect -> Line2D` |
| `.Edge32` | `Line2D` |
| `BRect.edge32` | `r: BRect -> Line2D` |
| `.Edges` | `Line2D array` |
| `BRect.edges` | `r: BRect -> Line2D array` |
| `.EvaluateAt` | `xParameter: float * yParameter: float -> Pt` |
| `BRect.evaluateAt` | `xParameter: float -> yParameter: float -> r: BRect -> Pt` |
| `.Expand` | `dist: float -> BRect` |
| `BRect.expand` | `dist: float -> r: BRect -> BRect` |
| `.ExpandClamped` | `dist: float -> BRect` |
| `BRect.expandClamped` | `dist: float -> r: BRect -> BRect` |
| `.ExpandClampedXY` | `xDist: float * yDist: float -> BRect` |
| `BRect.expandClampedXY` | `xDist: float -> yDist: float -> r: BRect -> BRect` |
| `.ExpandXAxis` | `startDist: float * endDist: float -> BRect` |
| `BRect.expandXAxis` | `startDist: float -> endDist: float -> r: BRect -> BRect` |
| `.ExpandXY` | `xDist: float * yDist: float -> BRect` |
| `BRect.expandXY` | `xDist: float -> yDist: float -> r: BRect -> BRect` |
| `.ExpandYAxis` | `startDist: float * endDist: float -> BRect` |
| `BRect.expandYAxis` | `startDist: float -> endDist: float -> r: BRect -> BRect` |
| `.GetEdge` | `edgeIndex: int -> Line2D` |
| `BRect.getEdge` | `edgeIndex: int -> r: BRect -> Line2D` |
| `.GetLine` | `startPointIndex: int * endPointIndex: int -> Line2D` |
| `BRect.getLine` | `startPointIndex: int -> endPointIndex: int -> r: BRect -> Line2D` |
| `.GetPoint` | `pointIndex: int -> Pt` |
| `BRect.getPoint` | `pointIndex: int -> r: BRect -> Pt` |
| `.HasArea` | `bool` |
| `BRect.hasArea` | `r: BRect -> bool` |
| `.Intersection` | `a: BRect -> BRect` |
| `BRect.intersection` | `a: BRect -> b: BRect -> BRect` |
| `.IntersectRay` | `ray: Line2D -> float * float` |
| `BRect.intersectRay` | `ray: Line2D -> r: BRect -> float * float` |
| `.IsLine` | `bool` |
| `BRect.isLine` | `r: BRect -> bool` |
| `.IsOverlapping` | `a: BRect -> bool` |
| `BRect.isOverlapping` | `a: BRect -> b: BRect -> bool` |
| `.IsOverlappingOrClose` | `a: BRect * tolerance: float -> bool` |
| `BRect.isOverlappingOrClose` | `tolerance: float -> a: BRect -> b: BRect -> bool` |
| `.IsPoint` | `bool` |
| `BRect.isPoint` | `r: BRect -> bool` |
| `.IsTouching` | `other: BRect * ?tolerance: float -> bool` |
| `BRect.isTouching` | `tolerance: float -> other: BRect -> this: BRect -> bool` |
| `.IsValid` | `bool` |
| `BRect.isValid` | `r: BRect -> bool` |
| `.IsZero` | `bool` |
| `BRect.isZero` | `r: BRect -> bool` |
| `.LongestEdge` | `float` |
| `BRect.longestEdge` | `r: BRect -> float` |
| `.MaxPt` | `Pt` |
| `BRect.maxPt` | `r: BRect -> Pt` |
| `.MinPt` | `Pt` |
| `BRect.minPt` | `r: BRect -> Pt` |
| `.Points` | `Pt array` |
| `BRect.points` | `r: BRect -> Pt array` |
| `.PointsLooped` | `Pt array` |
| `BRect.pointsLooped` | `r: BRect -> Pt array` |
| `.Pt0` | `Pt` |
| `BRect.pt0` | `r: BRect -> Pt` |
| `.Pt1` | `Pt` |
| `BRect.pt1` | `r: BRect -> Pt` |
| `.Pt2` | `Pt` |
| `BRect.pt2` | `r: BRect -> Pt` |
| `.Pt3` | `Pt` |
| `BRect.pt3` | `r: BRect -> Pt` |
| `.ShortestEdge` | `float` |
| `BRect.shortestEdge` | `r: BRect -> float` |
| `.SizeX` | `float` |
| `BRect.sizeX` | `r: BRect -> float` |
| `.SizeY` | `float` |
| `BRect.sizeY` | `r: BRect -> float` |
| `.ToString` | `unit -> string` |
| `.TouchingSide` | `other: BRect * ?tolerance: float -> int` |
| `BRect.touchingSide` | `other: BRect -> tolerance: float -> this: BRect -> int` |
| `.TryIntersection` | `a: BRect -> BRect voption` |
| `BRect.tryIntersection` | `a: BRect -> b: BRect -> BRect voption` |
| `.TryIntersectRay` | `ray: Line2D -> (float * float) voption` |
| `BRect.tryIntersectRay` | `ray: Line2D -> r: BRect -> (float * float) voption` |
| `.Union` | `b: BRect -> BRect` |
| `BRect.union` | `a: BRect -> b: BRect -> BRect` |
| `.UnionPt` | `p: Pt -> BRect` |
| `BRect.unionPt` | `p: Pt -> r: BRect -> BRect` |
| **Other static members** | |
| `BRect.create` | `a: Pt -> b: Pt -> BRect` |
| `BRect.createCentered` | `sizeX: float -> sizeY: float -> center: Pt -> BRect` |
| `BRect.createFromIList` | `ps: IList<Pt> -> BRect` |
| `BRect.createFromLine` | `l: Line2D -> BRect` |
| `BRect.createFromRect2D` | `r: Rect2D -> BRect` |
| `BRect.createFromRect3D` | `r: Rect3D -> BRect` |
| `BRect.createFromSeq` | `ps: Pt seq -> BRect` |
| `BRect.createUnchecked` | `minX: float * minY: float * maxX: float * maxY: float -> BRect` |
| `BRect.createXY` | `ptAx: float * ptAy: float * ptBx: float * ptBy: float -> BRect` |
| `BRect.equalsExact` | `a: BRect -> b: BRect -> bool` |
| `BRect.equalsWithin` | `tol: float -> a: BRect -> b: BRect -> bool` |
| `BRect.expandRel` | `factor: float -> r: BRect -> BRect` |
| `BRect.expandRelXY` | `factorX: float -> factorY: float -> r: BRect -> BRect` |
| `BRect.move` | `v: Vc -> r: BRect -> BRect` |
| `BRect.moveX` | `translation: float -> r: BRect -> BRect` |
| `BRect.moveY` | `translation: float -> r: BRect -> BRect` |
| `BRect.notEqualsWithin` | `tol: float -> a: BRect -> b: BRect -> bool` |
| `BRect.scale` | `factor: float -> r: BRect -> BRect` |

## Tria2D

| Tria2D | Signature |
|:--|:--|
| `Tria2D.area` | `a: Pt -> b: Pt -> c: Pt -> float` |
| `Tria2D.areaDouble` | `a: Pt -> b: Pt -> c: Pt -> float` |
| `Tria2D.areaSigned` | `a: Pt -> b: Pt -> c: Pt -> float` |
| `Tria2D.det` | `a: Pt * b: Pt * c: Pt -> float` |
| `Tria2D.isLinear` | `a: Pt -> b: Pt -> c: Pt -> bool` |
| `Tria2D.isLinearCustom` | `a: Pt * b: Pt * c: Pt * ?distanceTolerance: float -> bool` |
| `Tria2D.isLinearFast` | `a: Pt -> b: Pt -> c: Pt -> bool` |
| `Tria2D.isLinearFastCustom` | `a: Pt * b: Pt * c: Pt * ?maxAreaParallelogram: float -> bool` |
| `Tria2D.offset` | `dist: float -> a: Pt -> b: Pt -> c: Pt -> Pt * Pt * Pt` |
| `Tria2D.offsetPt` | `dist: float -> prev: Pt -> next: Pt -> ptToOffset: Pt -> Pt` |
| `Tria2D.offsetPtVariable` | `distPrev: float -> distNext: float -> prev: Pt -> next: Pt -> ptToOffset: Pt -> Pt` |
| `Tria2D.offsetPtVariableByNormals` | `prevDist: float -> nextDist: float -> prevN: UnitVc -> nextN: UnitVc -> ptToOffset: Pt -> Pt` |
| `Tria2D.tryOffsetPtVariable` | `distPrev: float -> distNext: float -> prev: Pt -> next: Pt -> ptToOffset: Pt -> Pt voption` |
| `Tria2D.tryOffsetPtVariableByNormals` | `prevDist: float -> nextDist: float -> prevN: UnitVc -> nextN: UnitVc -> ptToOffset: Pt -> Pt voption` |

## Points2D

| Points2D | Signature |
|:--|:--|
| `Points2D.center` | `pts: IList<Pt> -> Pt` |
| `Points2D.closestOfTwo` | `pt1: Pt -> pt2: Pt -> referencePoint: Pt -> Pt` |
| `Points2D.closestPoint` | `pt: Pt -> pts: IList<Pt> -> Pt` |
| `Points2D.closestPointIndex` | `pt: Pt -> pts: IList<Pt> -> int` |
| `Points2D.closestPointsIndices` | `xs: IList<Pt> -> ys: IList<Pt> -> int * int` |
| `Points2D.getSignedArea` | `ps: IList<Pt> -> float` |
| `Points2D.minDistanceBetweenPointSets` | `xs: IList<Pt> -> ys: IList<Pt> -> float` |
| `Points2D.mostDistantPoint` | `checkAgainst: IList<Pt> -> findPointFrom: IList<Pt> -> Pt` |
| `Points2D.mostDistantPointIndices` | `checkAgainst: IList<Pt> -> findPointFrom: IList<Pt> -> int * int` |

## module Offset2D

| Offset2D | Signature |
|:--|:--|
| `Offset2D.makeOffsetDirections` | `xys: ResizeArray<float> -> ResizeArray<float>` |
| `Offset2D.makeUnitTangents` | `xys: ResizeArray<float> -> ResizeArray<float>` |
| `Offset2D.offset` | `dist: float -> xys: ResizeArray<float> -> ResizeArray<float>` |
| `Offset2D.offsetByDirections` | `xys: ResizeArray<float> * dirs: ResizeArray<float> * dist: float * uTurnBehavior: UTurn * useUTurnBehaviorAbove: float<cosine> -> ResizeArray<float>` |
| `Offset2D.offsetCustom` | `xys: ResizeArray<float> * distance: float * uTurnBehavior: UTurn * useUTurnBehaviorAbove: float<cosine> -> ResizeArray<float>` |
| `Offset2D.offsetVariable` | `dists: ResizeArray<float> -> xys: ResizeArray<float> -> ResizeArray<float>` |
| `Offset2D.offsetVariableByDirections` | `xys: ResizeArray<float> * nDirs: ResizeArray<float> * dists: IList<float> * varDistParallelBehavior: VarDistParallel * uTurnBehavior: UTurn * useVarDistParallelBehaviorBelow: float<cosine> * useUTurnBehaviorAbove: float<cosine> -> ResizeArray<float>` |
| `Offset2D.offsetVariableCustom` | `xys: ResizeArray<float> * distances: ResizeArray<float> * varDistParallelBehavior: VarDistParallel * uTurnBehavior: UTurn * useVarDistParallelBehaviorBelow: float<cosine> * useUTurnBehaviorAbove: float<cosine> -> ResizeArray<float>` |
| `Offset2D.removeUTurns` | `xys: ResizeArray<float> * ns: ResizeArray<float> * minCos: float<cosine> -> ResizeArray<float>` |
| `Offset2D.setOffCorner` | `res: ResizeArray<float> * x: float * y: float * dist: float * nPrevX: float * nPrevY: float * nNextX: float * nNextY: float * cosine: float -> unit` |
| `Offset2D.toStringXYs` | `xys: ResizeArray<float> -> string` |

## Offset2D.UTurn

| UTurn | Signature |
|:--|:--|
| `Fail` | `UTurn = 1` |
| `Chamfer` | `UTurn = 2` |
| `UseThreshold` | `UTurn = 3` |

## Offset2D.VarDistParallel

| VarDistParallel | Signature |
|:--|:--|
| `Fail` | `VarDistParallel = 1` |
| `Skip` | `VarDistParallel = 2` |
| `Proportional` | `VarDistParallel = 3` |
| `Project` | `VarDistParallel = 4` |
| `StepWithTwoPoints` | `VarDistParallel = 5` |

## Polyline2D

| Polyline2D | Signature |
|:--|:--|
| `new` | `capacity: int -> Polyline2D` |
| `.AddPoint` | `pt: Pt -> unit` |
| `Polyline2D.addPoint` | `pt: Pt -> p: Polyline2D -> unit` |
| `.AddXY` | `x: float * y: float -> unit` |
| `Polyline2D.addXY` | `x: float -> y: float -> p: Polyline2D -> unit` |
| `.Area` | `float` |
| `Polyline2D.area` | `p: Polyline2D -> float` |
| `.AsFSharpCode` | `string` |
| `Polyline2D.asFSharpCode` | `p: Polyline2D -> string` |
| `.AsPoints` | `ResizeArray<Pt>` |
| `Polyline2D.asPoints` | `p: Polyline2D -> ResizeArray<Pt>` |
| `.AsString` | `string` |
| `Polyline2D.asString` | `p: Polyline2D -> string` |
| `.BoundingRectangle` | `BRect` |
| `Polyline2D.boundingRectangle` | `p: Polyline2D -> BRect` |
| `.Center` | `Pt` |
| `Polyline2D.center` | `p: Polyline2D -> Pt` |
| `.Clone` | `unit -> Polyline2D` |
| `Polyline2D.clone` | `p: Polyline2D -> Polyline2D` |
| `.Close` | `?toleranceForAddingPoint: float -> unit` |
| `Polyline2D.close` | `toleranceForAddingPoint: float -> pl: Polyline2D -> Polyline2D` |
| `.ClosestParameter` | `p: Pt -> float` |
| `Polyline2D.closestParameter` | `pt: Pt -> pl: Polyline2D -> float` |
| `.ClosestParameterXY` | `x: float * y: float -> float` |
| `Polyline2D.closestParameterXY` | `x: float -> y: float -> pl: Polyline2D -> float` |
| `.ClosestPoint` | `p: Pt -> Pt` |
| `Polyline2D.closestPoint` | `pt: Pt -> pl: Polyline2D -> Pt` |
| `.ClosestPointIndex` | `p: Pt -> int` |
| `Polyline2D.closestPointIndex` | `pt: Pt -> pl: Polyline2D -> int` |
| `.Contains` | `pt: Pt -> bool` |
| `Polyline2D.contains` | `pt: Pt -> pl: Polyline2D -> bool` |
| `.ContainsXY` | `x: float * y: float -> bool` |
| `Polyline2D.containsXY` | `x: float -> y: float -> pl: Polyline2D -> bool` |
| `.DistanceTo` | `p: Pt -> float` |
| `Polyline2D.distanceTo` | `pt: Pt -> pl: Polyline2D -> float` |
| `.DistanceToXY` | `x: float * y: float -> float` |
| `Polyline2D.distanceToXY` | `x: float -> y: float -> pl: Polyline2D -> float` |
| `.Duplicate` | `unit -> Polyline2D` |
| `Polyline2D.duplicate` | `p: Polyline2D -> Polyline2D` |
| `.End` | `Pt with get, set` |
| `.EnsureClockwise` | `unit -> unit` |
| `Polyline2D.ensureClockwise` | `pl: Polyline2D -> Polyline2D` |
| `.EnsureCounterClockwise` | `unit -> unit` |
| `Polyline2D.ensureCounterClockwise` | `pl: Polyline2D -> Polyline2D` |
| `.EvaluateAt` | `t: float -> Pt` |
| `Polyline2D.evaluateAt` | `t: float -> pl: Polyline2D -> Pt` |
| `.FindLabelPoint` | `precision: float -> Pt * float` |
| `Polyline2D.findLabelPoint` | `precision: float -> pl: Polyline2D -> Pt * float` |
| `.FirstPoint` | `Pt with get, set` |
| `Polyline2D.firstPoint` | `p: Polyline2D -> Pt` |
| `.FirstSegment` | `Line2D` |
| `Polyline2D.firstSegment` | `p: Polyline2D -> Line2D` |
| `.GetPoint` | `pointIndex: int -> Pt` |
| `Polyline2D.getPoint` | `pointIndex: int -> p: Polyline2D -> Pt` |
| `.GetSegment` | `i: int -> Line2D` |
| `Polyline2D.getSegment` | `i: int -> p: Polyline2D -> Line2D` |
| `.GetX` | `pointIndex: int -> float` |
| `Polyline2D.getX` | `pointIndex: int -> p: Polyline2D -> float` |
| `.GetY` | `pointIndex: int -> float` |
| `Polyline2D.getY` | `pointIndex: int -> p: Polyline2D -> float` |
| `.IsAlmostClosed` | `tolerance: float -> bool` |
| `Polyline2D.isAlmostClosed` | `tolerance: float -> p: Polyline2D -> bool` |
| `.IsClockwise` | `bool` |
| `Polyline2D.isClockwise` | `p: Polyline2D -> bool` |
| `.IsClosed` | `bool` |
| `Polyline2D.isClosed` | `p: Polyline2D -> bool` |
| `.IsCounterClockwise` | `bool` |
| `Polyline2D.isCounterClockwise` | `p: Polyline2D -> bool` |
| `.LastPoint` | `Pt with get, set` |
| `Polyline2D.lastPoint` | `p: Polyline2D -> Pt` |
| `.LastSegment` | `Line2D` |
| `Polyline2D.lastSegment` | `p: Polyline2D -> Line2D` |
| `.Length` | `float` |
| `Polyline2D.length` | `p: Polyline2D -> float` |
| `.Move` | `v: Vc -> unit` |
| `Polyline2D.move` | `v: Vc -> pl: Polyline2D -> Polyline2D` |
| `.MovePoint` | `pointIndex: int * v: Vc -> unit` |
| `Polyline2D.movePoint` | `pointIndex: int -> v: Vc -> p: Polyline2D -> unit` |
| `.MovePointPreservingClosure` | `pointIndex: int * v: Vc -> unit` |
| `Polyline2D.movePointPreservingClosure` | `pointIndex: int -> v: Vc -> p: Polyline2D -> unit` |
| `.MoveX` | `distance: float -> unit` |
| `Polyline2D.moveX` | `distance: float -> pl: Polyline2D -> Polyline2D` |
| `.MoveXY` | `pointIndex: int * x: float * y: float -> unit` |
| `Polyline2D.moveXY` | `pointIndex: int -> x: float -> y: float -> p: Polyline2D -> unit` |
| `.MoveXYPreservingClosure` | `pointIndex: int * x: float * y: float -> unit` |
| `Polyline2D.moveXYPreservingClosure` | `pointIndex: int -> x: float -> y: float -> p: Polyline2D -> unit` |
| `.MoveY` | `distance: float -> unit` |
| `Polyline2D.moveY` | `distance: float -> pl: Polyline2D -> Polyline2D` |
| `.PointCount` | `int` |
| `Polyline2D.pointCount` | `p: Polyline2D -> int` |
| `.RemoveDuplicateAndCollinearPoints` | `angleTolerance: float<cosine> -> distanceTolerance: float -> unit` |
| `Polyline2D.removeDuplicateAndCollinearPoints` | `angleTolerance: float<cosine> -> distanceTolerance: float -> pl: Polyline2D -> Polyline2D` |
| `.RemoveDuplicatePoints` | `distanceTolerance: float -> unit` |
| `Polyline2D.removeDuplicatePoints` | `distanceTolerance: float -> pl: Polyline2D -> Polyline2D` |
| `.RemoveDuplicatePointsAndReport` | `distanceTolerance: float -> ResizeArray<Pt>` |
| `Polyline2D.removeDuplicatePointsAndReport` | `distanceTolerance: float -> pl: Polyline2D -> Polyline2D * ResizeArray<Pt>` |
| `.RemoveDuplicatePointsFaithfully` | `distanceTolerance: float -> unit` |
| `Polyline2D.removeDuplicatePointsFaithfully` | `distanceTolerance: float -> pl: Polyline2D -> Polyline2D` |
| `.RemoveUTurns` | `minCos: float<cosine> -> unit` |
| `Polyline2D.removeUTurns` | `minCos: float<cosine> -> polyLine: Polyline2D -> Polyline2D` |
| `.RemoveUTurnsDeeply` | `minCos: float<cosine> -> unit` |
| `Polyline2D.removeUTurnsDeeply` | `minCos: float<cosine> -> polyLine: Polyline2D -> Polyline2D` |
| `.Reverse` | `unit -> unit` |
| `Polyline2D.reverse` | `p: Polyline2D -> Polyline2D` |
| `.Rotate` | `r: Rotation2D -> unit` |
| `Polyline2D.rotate` | `r: Rotation2D -> pl: Polyline2D -> Polyline2D` |
| `.RotateAround` | `cen: Pt -> r: Rotation2D -> unit` |
| `Polyline2D.rotateAround` | `cen: Pt -> r: Rotation2D -> pl: Polyline2D -> Polyline2D` |
| `.RotateAroundDegrees` | `cen: Pt -> angDegree: float -> unit` |
| `Polyline2D.rotateAroundDegrees` | `cen: Pt -> angDegree: float -> pl: Polyline2D -> Polyline2D` |
| `.RotateAroundRadians` | `cen: Pt -> angRadians: float -> unit` |
| `Polyline2D.rotateAroundRadians` | `cen: Pt -> angRadians: float -> pl: Polyline2D -> Polyline2D` |
| `.RotateDegrees` | `angDegree: float -> unit` |
| `Polyline2D.rotateDegrees` | `angDegree: float -> pl: Polyline2D -> Polyline2D` |
| `.RotateRadians` | `angRadians: float -> unit` |
| `Polyline2D.rotateRadians` | `angRadians: float -> pl: Polyline2D -> Polyline2D` |
| `.Scale` | `factor: float -> unit` |
| `Polyline2D.scale` | `factor: float -> pl: Polyline2D -> Polyline2D` |
| `.ScaleOn` | `cen: Pt -> factor: float -> unit` |
| `Polyline2D.scaleOn` | `cen: Pt -> factor: float -> pl: Polyline2D -> Polyline2D` |
| `.SecondLastPoint` | `Pt with get, set` |
| `Polyline2D.secondLastPoint` | `p: Polyline2D -> Pt` |
| `.SecondPoint` | `Pt with get, set` |
| `Polyline2D.secondPoint` | `p: Polyline2D -> Pt` |
| `.SegmentCount` | `int` |
| `Polyline2D.segmentCount` | `p: Polyline2D -> int` |
| `.Segments` | `ResizeArray<Line2D>` |
| `Polyline2D.segments` | `p: Polyline2D -> ResizeArray<Line2D>` |
| `.SegmentVectors` | `ResizeArray<Vc>` |
| `Polyline2D.segmentVectors` | `p: Polyline2D -> ResizeArray<Vc>` |
| `.SegmentVectorsXY` | `ResizeArray<float>` |
| `Polyline2D.segmentVectorsXY` | `p: Polyline2D -> ResizeArray<float>` |
| `.SetPoint` | `pointIndex: int * pt: Pt -> unit` |
| `Polyline2D.setPoint` | `pointIndex: int -> pt: Pt -> p: Polyline2D -> unit` |
| `.SetPointPreservingClosure` | `pointIndex: int * pt: Pt -> unit` |
| `Polyline2D.setPointPreservingClosure` | `pointIndex: int -> pt: Pt -> p: Polyline2D -> unit` |
| `.SetXY` | `pointIndex: int * x: float * y: float -> unit` |
| `Polyline2D.setXY` | `pointIndex: int -> x: float -> y: float -> p: Polyline2D -> unit` |
| `.SetXYPreservingClosure` | `pointIndex: int * x: float * y: float -> unit` |
| `Polyline2D.setXYPreservingClosure` | `pointIndex: int -> x: float -> y: float -> p: Polyline2D -> unit` |
| `.SignedArea` | `float` |
| `Polyline2D.signedArea` | `p: Polyline2D -> float` |
| `.SignedDistanceTo` | `point: Pt -> float` |
| `Polyline2D.signedDistanceTo` | `point: Pt -> pl: Polyline2D -> float` |
| `.SignedDistanceToXY` | `x: float * y: float -> float` |
| `Polyline2D.signedDistanceToXY` | `x: float -> y: float -> pl: Polyline2D -> float` |
| `.Start` | `Pt with get, set` |
| `Polyline2D.start` | `p: Polyline2D -> Pt` |
| `.ToString` | `unit -> string` |
| `.WindingNumber` | `point: Pt -> int` |
| `Polyline2D.windingNumber` | `point: Pt -> pl: Polyline2D -> int` |
| `.XYs` | `ResizeArray<float>` |
| `Polyline2D.xys` | `p: Polyline2D -> ResizeArray<float>` |
| **Other static members** | |
| `Polyline2D.createEmpty` | `capacity: int -> Polyline2D` |
| `Polyline2D.createFromBRectCCW` | `r: BRect -> Polyline2D` |
| `Polyline2D.createFromBRectCW` | `r: BRect -> Polyline2D` |
| `Polyline2D.createFromMembersXY` | `xyObjs: ^T seq -> Polyline2D` |
| `Polyline2D.createFromMembersxy` | `xyObjs: ^T seq -> Polyline2D` |
| `Polyline2D.createFromPts` | `points: Pt seq -> Polyline2D` |
| `Polyline2D.createFromRectCCW` | `r: Rect2D -> Polyline2D` |
| `Polyline2D.createFromRectCW` | `r: Rect2D -> Polyline2D` |
| `Polyline2D.createFromXYs` | `xys: ResizeArray<float> -> Polyline2D` |
| `Polyline2D.end'` | `p: Polyline2D -> Pt` |
| `Polyline2D.equalsExact` | `pl1: Polyline2D -> pl2: Polyline2D -> bool` |
| `Polyline2D.equalsWithin` | `tol: float -> pl1: Polyline2D -> pl2: Polyline2D -> bool` |
| `Polyline2D.iter` | `action: (float -> float -> unit) -> pl: Polyline2D -> unit` |
| `Polyline2D.iteri` | `action: (int -> float -> float -> unit) -> pl: Polyline2D -> unit` |
| `Polyline2D.iteriPt` | `action: (int -> Pt -> unit) -> pl: Polyline2D -> unit` |
| `Polyline2D.iterLineSegments` | `action: (Line2D -> unit) -> pl: Polyline2D -> unit` |
| `Polyline2D.iterPt` | `action: (Pt -> unit) -> pl: Polyline2D -> unit` |
| `Polyline2D.iterSegments` | `action: (float -> float -> float -> float -> unit) -> pl: Polyline2D -> unit` |
| `Polyline2D.map` | `mapping: (float -> float -> Pt) -> pl: Polyline2D -> Polyline2D` |
| `Polyline2D.mapi` | `mapping: (int -> float -> float -> Pt) -> pl: Polyline2D -> Polyline2D` |
| `Polyline2D.mapiPt` | `mapping: (int -> Pt -> Pt) -> pl: Polyline2D -> Polyline2D` |
| `Polyline2D.mapPt` | `mapping: (Pt -> Pt) -> pl: Polyline2D -> Polyline2D` |
| `Polyline2D.offset` | `offsetDistance: float -> pl: Polyline2D -> Polyline2D` |
| `Polyline2D.offsetCustom` | `polyLine: Polyline2D * constantOffsetDistance: float * ?loop: bool * ?checkOrientation: bool * ?uTurnBehavior: UTurn * ?useUTurnBehaviorAbove: float<cosine> -> Polyline2D` |
| `Polyline2D.offsetVariable` | `multipleOffsetDistances: IList<float> -> pl: Polyline2D -> Polyline2D` |
| `Polyline2D.offsetVariableCustom` | `polyLine: Polyline2D * multipleOffsetDistances: IList<float> * ?loop: bool * ?checkOrientation: bool * ?varDistParallelBehavior: VarDistParallel * ?uTurnBehavior: UTurn * ?useVarDistParallelBehaviorBelow: float<cosine> * ?useUTurnBehaviorAbove: float<cosine> -> Polyline2D` |
| `Polyline2D.subPolyline` | `a: float -> b: float -> pl: Polyline2D -> Polyline2D` |
| `Polyline2D.tryFind` | `condition: (float -> float -> bool) -> pl: Polyline2D -> Pt voption` |
| `Polyline2D.tryFindIndex` | `condition: (float -> float -> bool) -> pl: Polyline2D -> int voption` |
| `Polyline2D.tryFindLast` | `condition: (float -> float -> bool) -> pl: Polyline2D -> Pt voption` |
| `Polyline2D.tryFindLastIndex` | `condition: (float -> float -> bool) -> pl: Polyline2D -> int voption` |
| `Polyline2D.tryFindSelfIntersection` | `pl: Polyline2D -> (Pt * int * int) voption` |

## Topology2D

| Topology2D | Signature |
|:--|:--|
| `Topology2D.join` | `getLine: ('T -> Line2D) -> splitDistance: float -> xs: ResizeArray<'T> -> ResizeArray<ResizeArray<'T>>` |
| `Topology2D.joinReversing` | `getLine: ('T -> Line2D) -> splitDistance: float -> xs: ResizeArray<'T> -> ResizeArray<ResizeArray<'T * bool>>` |
| `Topology2D.sortToLoop` | `getLine: ('T -> Line2D) -> ts: ResizeArray<'T> -> unit` |
| `Topology2D.sortToLoopWithReversing` | `getLine: ('T -> Line2D) -> reverseInPlace: (int -> 'T -> unit) -> ts: ResizeArray<'T> -> unit` |

## module Similarity2D

| Similarity2D | Signature |
|:--|:--|
| `Similarity2D.areSimilar` | `tol: float -> a: ObjectToCheck -> b: ObjectToCheck -> bool` |
| `Similarity2D.getGrouped` | `tolerance: float -> sims: ResizeArray<ObjectToCheck> -> items: ResizeArray<'T> -> ResizeArray<ResizeArray<'T>>` |
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
