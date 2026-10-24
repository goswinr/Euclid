# Migrating to the curried API

This release changes the shape of the public API. Most changes are mechanical: the compiler points to every call that needs an update.
This page lists the rules first and then every changed member, grouped by type.
A machine-readable version of the table is in [Scripts/ApiMigration.tsv](https://github.com/goswinr/Euclid/blob/main/Scripts/ApiMigration.tsv).

## The rules

- **Static functions are curried.** A static member with several arguments takes them one by one, so it works with `|>` and partial application:
  `Vec.create(1.0, 2.0, 3.0)` is now `Vec.create 1.0 2.0 3.0`. Put negative numbers and expressions in parentheses: `Vec.create (-1.0) (a + b) 3.0`.
- **Overloads got their own names.** A curried name can have only one signature, so overloads were split by their input types:
  `Vc.create(ptA, ptB)` is now `Vc.createFromTo ptA ptB`, `Quaternion.createFromDegrees(axis, deg)` is now `createFromVecDegrees` or `createFromUnitVecDegrees`,
  `XLine2D.tryIntersect(pA, pB, vA, vB)` is now `XLine2D.tryIntersectFromPtsAndVcs pA pB vA vB`.
- **Configuration first, geometry last.** Sizes come before a plane, distances before a line, tolerances before inputs:
  `Rect3D.createFromPlane(plane, sizeX, sizeY)` is now `Rect3D.createFromPlane sizeX sizeY plane`, `PPlane.createOriginNormal(origin, normal)` is now `PPlane.createFromNormal normal origin`.
  Natural orders stay: `x y z`, `start finish`, and interpolation keeps the factor last: `Pnt.lerp start finish factor`.
- **Some functions stay tupled.** Functions with more than three arguments of the same type, like `Quaternion.create(...)` or `XLine2D.isWithinRanges(...)`,
  the `create...Unchecked` factories, `cross`, `determinant3`, `Tria2D.det` and the `multiply` functions keep their tuple, because their argument order is easy to mix up.
  `FreeBox.createFrom4Pts` was curried before and is tupled now.
  A function whose last argument, the geometry, has another type may stay curried: `Box.offset distBottom distFront distRight distBack distLeft distTop box` is unchanged.
- **Optional parameters moved to `...Custom` functions.** The plain name is curried and uses the defaults. The name ending in `Custom` keeps the tupled form with the optional parameters, so a call with named settings only needs a new name:
  `Polyline2D.offset(pl, 5.0, loop = true)` is now `Polyline2D.offsetCustom(pl, 5.0, loop = true)`, and `Polyline2D.offset(pl, 5.0)` is now `Polyline2D.offset 5.0 pl`.
  `Offset2D` and `Offset3D` are modules, so their `...Custom` functions take every setting explicitly, with the geometry first.
- **Spelled out names.** `Sq` goes last (`sqDistanceTo` is now `distanceToSq`), abbreviations are spelled out (`Var` is now `Variable`, `Deg` is now `Degrees`, `Idx` is now `Index`, `Dist` is now `Distance`),
  and every factory that takes a raw angle names its unit, with a new `Radians` partner next to each `Degrees` factory.
- **Axes are capitalized.** Instance members and fields are `XAxis`, `XAxisUnit` and `XAxisX`; the world axes are the static values `Vec.xAxis`, `UnitVec.zAxis` and so on.
  The JSON property names follow the field names: `XaxisX` is now `XAxisX` and so on in the JSON of `PPlane`, `Rect2D`, `Rect3D` and `Box`. JSON with the old names is not read.
- **One verb per operation.** `translate` is gone, use `move`. The aliases `flip` (use `reverse`), `dist` and `sqDist` (use `distanceTo` and `distanceToSq`), `Pt.addVc` and `Pnt.addVec` (use `move`) and `Line2D.det` (use `Line2D.cross`) are removed.
- **Shared buffers keep their names.** `Polyline2D.XYs`, `Polyline3D.XYZs` and `FreeBox.XYZs` still give the live coordinate buffer. Their static getters are `xys` and `xyzs`, and the `create...` functions that share their input are `createFromXYs` and `createFromXYZs`. The JSON property names `XYs` and `XYZs` are unchanged.
- **Indexed points say `Point`.** The indexed point accessors of `Polyline2D`, `Polyline3D`, `FreeBox`, `Box` and `BBox` use the same names in 2D and 3D:
  `getPt` and `getPnt` are now `getPoint`, `setPt` and `setPnt` are now `setPoint`, `setPtXY` is now `setXY`, `setPntXYZ` is now `setXYZ`,
  `movePt` and `movePnt` are now `movePoint`, `movePtXY` is now `moveXY` and `movePntXYZ` is now `moveXYZ`.
  The variants that keep a closed polyline closed end in `PreservingClosure`: `setXYPreservingClosure`, `setXYZPreservingClosure`, `movePointPreservingClosure`, `moveXYPreservingClosure` and `moveXYZPreservingClosure`.
  `setPointPreservingClosure` is new. The instance members follow in PascalCase, like `SetPoint` and `MovePointPreservingClosure`. The append functions `addPoint`, `addXY` and `addXYZ` keep their names.
- **Descriptive names.** `ExpandSafe`, `ExpandSafeXY` and `ExpandSafeXYZ` of `BRect` and `BBox` are now `ExpandClamped`, `ExpandClampedXY` and `ExpandClampedXYZ`.
  `rotateOnZ90CCW` and `rotateOnZ90CW` of `Pnt`, `Vec` and `UnitVec` are now `rotate2Dby90CCW` and `rotate2Dby90CW`.
  `translateLocalX` of `Box`, `Rect2D`, `Rect3D` and `PPlane` is now `moveAlongLocalX`, and so on for Y and Z.
  The two-point factories of `Vc`, `UnitVc`, `Vec` and `UnitVec` are `createFromTo start finish`.
- **Composition reads in order.** `a.Then b` and `a |> T.andThen b` first apply `a` and then `b` on `Rotation2D`, `Quaternion`, `Matrix` and `RigidMatrix`. `Rotation2D.Add` is now `Then`.

## Calls that still compile but need attention

These functions changed the order of arguments of the same type. An old call that passes arguments in the old order may still compile after the migration to curried form, so check these by hand:

| Old call | New call |
|:--|:--|
| `Points2D.mostDistantPoint(findPointFrom, checkAgainst)` | `Points2D.mostDistantPoint checkAgainst findPointFrom` (also `Points3D`, and `mostDistantPointIndices`, which still returns the index into findPointFrom first) |
| `Tria2D.offsetPt(pt, prev, next, dist)` | `Tria2D.offsetPt dist prev next pt` (also `Tria3D.offsetPnt`) |
| `Tria2D.offsetPtVar(pt, prev, next, distPrev, distNext)` | `Tria2D.offsetPtVariable distPrev distNext prev next pt` (also `tryOffsetPtVariable`, `offsetPtVariableByNormals` and `Tria3D.offsetPntVariable`) |
| ``Offset2D.offsetVariable'' useUTurnBehaviorAbove useVarDistParallelBehaviorBelow uTurnBehavior varDistParallelBehavior dists xys`` | ``Offset2D.offsetVariableCustom(xys, dists, varDistParallelBehavior, uTurnBehavior, useVarDistParallelBehaviorBelow, useUTurnBehaviorAbove)``, both thresholds are `float<Cosine.cosine>` |
| `Similarity2D.getGrouped(tolerance, items, similarities)` | `Similarity2D.getGrouped tolerance similarities items` |

## Changed members

Instance members are shown with a leading dot. Members that only changed from tupled to curried arguments are listed too.

### BBox

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `.ExpandSafe` | `dist: float -> BBox` | `.ExpandClamped` | `dist: float -> BBox` | renamed |
| `.ExpandSafeXYZ` | `xDist: float * yDist: float * zDist: float -> BBox` | `.ExpandClampedXYZ` | `xDist: float * yDist: float * zDist: float -> BBox` | renamed |
| `.ExpandXaxis` | `startDist: float * endDist: float -> BBox` | `.ExpandXAxis` | `startDist: float * endDist: float -> BBox` | renamed |
| `.ExpandYaxis` | `startDist: float * endDist: float -> BBox` | `.ExpandYAxis` | `startDist: float * endDist: float -> BBox` | renamed |
| `.ExpandZaxis` | `startDist: float * endDist: float -> BBox` | `.ExpandZAxis` | `startDist: float * endDist: float -> BBox` | renamed |
| `.GetPnt` | `pointIndex: int -> Pnt` | `.GetPoint` | `pointIndex: int -> Pnt` | renamed |
| `BBox.create` | `a: Pnt * b: Pnt -> BBox` | `BBox.create` | `a: Pnt -> b: Pnt -> BBox` | curried |
| `BBox.createFromCenter` | `center: Pnt * sizeX: float * sizeY: float * sizeZ: float -> BBox` | `BBox.createCentered` | `sizeX: float -> sizeY: float -> sizeZ: float -> center: Pnt -> BBox` | renamed, curried, reordered. New argument order: sizeX sizeY sizeZ center. |
| `BBox.expandSafe` | `dist: float -> b: BBox -> BBox` | `BBox.expandClamped` | `dist: float -> b: BBox -> BBox` | renamed |
| `BBox.expandSafeXYZ` | `xDist: float -> yDist: float -> zDist: float -> b: BBox -> BBox` | `BBox.expandClampedXYZ` | `xDist: float -> yDist: float -> zDist: float -> b: BBox -> BBox` | renamed |
| `BBox.expandXaxis` | `startDist: float -> endDist: float -> b: BBox -> BBox` | `BBox.expandXAxis` | `startDist: float -> endDist: float -> b: BBox -> BBox` | renamed |
| `BBox.expandYaxis` | `startDist: float -> endDist: float -> b: BBox -> BBox` | `BBox.expandYAxis` | `startDist: float -> endDist: float -> b: BBox -> BBox` | renamed |
| `BBox.expandZaxis` | `startDist: float -> endDist: float -> b: BBox -> BBox` | `BBox.expandZAxis` | `startDist: float -> endDist: float -> b: BBox -> BBox` | renamed |
| `BBox.getPnt` | `pointIndex: int -> b: BBox -> Pnt` | `BBox.getPoint` | `pointIndex: int -> b: BBox -> Pnt` | renamed |
| `BBox.translate` | `v: Vec -> b: BBox -> BBox` | `BBox.move` | `v: Vec -> b: BBox -> BBox` | renamed |

### BRect

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `.ExpandSafe` | `dist: float -> BRect` | `.ExpandClamped` | `dist: float -> BRect` | renamed |
| `.ExpandSafeXY` | `xDist: float * yDist: float -> BRect` | `.ExpandClampedXY` | `xDist: float * yDist: float -> BRect` | renamed |
| `.ExpandXaxis` | `startDist: float * endDist: float -> BRect` | `.ExpandXAxis` | `startDist: float * endDist: float -> BRect` | renamed |
| `.ExpandYaxis` | `startDist: float * endDist: float -> BRect` | `.ExpandYAxis` | `startDist: float * endDist: float -> BRect` | renamed |
| `BRect.create` | `a: Pt * b: Pt -> BRect` | `BRect.create` | `a: Pt -> b: Pt -> BRect` | curried |
| `BRect.createFromCenter` | `center: Pt * sizeX: float * sizeY: float -> BRect` | `BRect.createCentered` | `sizeX: float -> sizeY: float -> center: Pt -> BRect` | renamed, curried, reordered. New argument order: sizeX sizeY center. |
| `BRect.expandSafe` | `dist: float -> r: BRect -> BRect` | `BRect.expandClamped` | `dist: float -> r: BRect -> BRect` | renamed |
| `BRect.expandSafeXY` | `xDist: float -> yDist: float -> r: BRect -> BRect` | `BRect.expandClampedXY` | `xDist: float -> yDist: float -> r: BRect -> BRect` | renamed |
| `BRect.expandXaxis` | `startDist: float -> endDist: float -> r: BRect -> BRect` | `BRect.expandXAxis` | `startDist: float -> endDist: float -> r: BRect -> BRect` | renamed |
| `BRect.expandYaxis` | `startDist: float -> endDist: float -> r: BRect -> BRect` | `BRect.expandYAxis` | `startDist: float -> endDist: float -> r: BRect -> BRect` | renamed |
| `BRect.translate` | `v: Vc -> r: BRect -> BRect` | `BRect.move` | `v: Vc -> r: BRect -> BRect` | renamed |

### Box

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `.GetPnt` | `pointIndex: int -> Pnt` | `.GetPoint` | `pointIndex: int -> Pnt` | renamed |
| `.Xaxis` | `unit -> Vec` | `.XAxis` | `unit -> Vec` | renamed |
| `.XaxisUnit` | `unit -> UnitVec` | `.XAxisUnit` | `unit -> UnitVec` | renamed |
| `.Yaxis` | `unit -> Vec` | `.YAxis` | `unit -> Vec` | renamed |
| `.YaxisUnit` | `unit -> UnitVec` | `.YAxisUnit` | `unit -> UnitVec` | renamed |
| `.Zaxis` | `unit -> Vec` | `.ZAxis` | `unit -> Vec` | renamed |
| `.ZaxisUnit` | `unit -> UnitVec` | `.ZAxisUnit` | `unit -> UnitVec` | renamed |
| `Box.create` | `origin: Pnt * xAxis: Vec * yAxis: Vec * zAxis: Vec -> Box` | `Box.create` | `xAxis: Vec -> yAxis: Vec -> zAxis: Vec -> origin: Pnt -> Box` | curried, reordered. New argument order: xAxis yAxis zAxis origin. |
| `Box.createFromDirsAndPoints` | `dirX: Vec -> dirY: Vec -> pts: IList<Pnt> -> Box` | `Box.createFromVecsAndPnts` | `dirX: Vec -> dirY: Vec -> pts: IList<Pnt> -> Box` | renamed |
| `Box.createFromPlane` | `x: float -> y: float -> z: float -> pl: PPlane -> Box` | `Box.createFromPlane` | `sizeX: float -> sizeY: float -> sizeZ: float -> plane: PPlane -> Box` | signature |
| `Box.createFromPlaneAndPoints` | `pl: PPlane -> pts: IList<Pnt> -> Box` | `Box.createFromPlaneAndPnts` | `pl: PPlane -> pts: IList<Pnt> -> Box` | renamed |
| `Box.createUncheckedVec` | `origin: Pnt * xAxis: Vec * yAxis: Vec * zAxis: Vec -> Box` | `Box.createFromAxesUnchecked` | `origin: Pnt * xAxis: Vec * yAxis: Vec * zAxis: Vec -> Box` | renamed |
| `Box.getPnt` | `pointIndex: int -> b: Box -> Pnt` | `Box.getPoint` | `pointIndex: int -> b: Box -> Pnt` | renamed |
| `Box.translateLocalX` | `distX: float -> b: Box -> Box` | `Box.moveAlongLocalX` | `distX: float -> b: Box -> Box` | renamed |
| `Box.translateLocalY` | `distY: float -> b: Box -> Box` | `Box.moveAlongLocalY` | `distY: float -> b: Box -> Box` | renamed |
| `Box.translateLocalZ` | `distZ: float -> b: Box -> Box` | `Box.moveAlongLocalZ` | `distZ: float -> b: Box -> Box` | renamed |

### EuclidErrors

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `EuclidErrors.failQuat` | `w: float -> x: float -> y: float -> z: float -> 'Failed` | `EuclidErrors.failQuat` | `w: float * x: float * y: float * z: float -> 'Failed` | tupled |

### FreeBox

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `.GetPnt` | `pointIndex: int -> Pnt` | `.GetPoint` | `pointIndex: int -> Pnt` | renamed |
| `.MovePnt` | `pointIndex: int -> v: Vec -> unit` | `.MovePoint` | `pointIndex: int -> v: Vec -> unit` | renamed |
| `.MovePntXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> unit` | `.MoveXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> unit` | renamed |
| `.SetPnt` | `pointIndex: int -> p: Pnt -> unit` | `.SetPoint` | `pointIndex: int -> p: Pnt -> unit` | renamed |
| `.SetPntXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> unit` | `.SetXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> unit` | renamed |
| `FreeBox.createDirectly` | `xyzs: float array -> FreeBox` | `FreeBox.createFromXYZs` | `xyzs: float array -> FreeBox` | renamed |
| `FreeBox.createFromEightPoints` | `pts: Pnt array -> FreeBox` | `FreeBox.createFrom8PntArray` | `pts: Pnt array -> FreeBox` | renamed |
| `FreeBox.createFromFour2DPoints` | `zMin: float -> zMax: float -> pts: Pt array -> FreeBox` | `FreeBox.createFrom4PtArray` | `zMin: float -> zMax: float -> pts: Pt array -> FreeBox` | renamed |
| `FreeBox.createFromFour2DPointsArgs` | `zMin: float -> zMax: float -> a: Pt -> b: Pt -> c: Pt -> d: Pt -> FreeBox` | `FreeBox.createFrom4Pts` | `zMin: float * zMax: float * a: Pt * b: Pt * c: Pt * d: Pt -> FreeBox` | renamed, tupled. Tupled because it takes four points: createFrom4Pts(zMin, zMax, a, b, c, d). |
| `FreeBox.getPnt` | `pointIndex: int -> b: FreeBox -> Pnt` | `FreeBox.getPoint` | `pointIndex: int -> b: FreeBox -> Pnt` | renamed |
| `FreeBox.getXYZs` | `b: FreeBox -> float array` | `FreeBox.xyzs` | `b: FreeBox -> float array` | renamed |
| `FreeBox.movePnt` | `pointIndex: int -> v: Vec -> b: FreeBox -> unit` | `FreeBox.movePoint` | `pointIndex: int -> v: Vec -> b: FreeBox -> unit` | renamed |
| `FreeBox.movePntXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> b: FreeBox -> unit` | `FreeBox.moveXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> b: FreeBox -> unit` | renamed |
| `FreeBox.setPnt` | `pointIndex: int -> p: Pnt -> b: FreeBox -> unit` | `FreeBox.setPoint` | `pointIndex: int -> p: Pnt -> b: FreeBox -> unit` | renamed |
| `FreeBox.setPntXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> b: FreeBox -> unit` | `FreeBox.setXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> b: FreeBox -> unit` | renamed |
| `FreeBox.translate` | `v: Vec -> b: FreeBox -> FreeBox` | `FreeBox.move` | `v: Vec -> b: FreeBox -> FreeBox` | renamed |

### Line2D

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `.SqDistanceRayPoint` | `p: Pt -> float` | `.DistanceRayPointSq` | `p: Pt -> float` | renamed |
| `.SqDistanceRayXY` | `x: float * y: float -> float` | `.DistanceRayXYSq` | `x: float * y: float -> float` | renamed |
| `.SqDistanceToPt` | `p: Pt -> float` | `.DistanceToPtSq` | `p: Pt -> float` | renamed |
| `.SqDistanceToXY` | `x: float * y: float -> float` | `.DistanceToXYSq` | `x: float * y: float -> float` | renamed |
|  |  | `Line2D.create` | `start: Pt -> finish: Pt -> Line2D` | added |
| `Line2D.createFromPtAndVc` | `p: Pt * v: Vc -> Line2D` | `Line2D.createFromPtAndVc` | `p: Pt -> v: Vc -> Line2D` | curried |
| `Line2D.cross` | `a: Line2D -> b: Line2D -> float` | `Line2D.cross` | `a: Line2D * b: Line2D -> float` | tupled |
| `Line2D.det` | `a: Line2D -> b: Line2D -> float` | `Line2D.cross` | `a: Line2D * b: Line2D -> float` | renamed, tupled. Alias removed. Use the tupled Line2D.cross(a, b). |
| `Line2D.flip` | `ln: Line2D -> Line2D` | `Line2D.reverse` | `ln: Line2D -> Line2D` | renamed |
| `Line2D.sqDistanceRayPoint` | `p: Pt -> ln: Line2D -> float` | `Line2D.distanceRayPointSq` | `p: Pt -> ln: Line2D -> float` | renamed |
| `Line2D.sqDistanceRayXY` | `x: float -> y: float -> ln: Line2D -> float` | `Line2D.distanceRayXYSq` | `x: float -> y: float -> ln: Line2D -> float` | renamed |
| `Line2D.sqDistanceToLine` | `lnA: Line2D -> lnB: Line2D -> float` | `Line2D.distanceToLineSq` | `lnA: Line2D -> lnB: Line2D -> float` | renamed |
| `Line2D.sqDistanceToPt` | `p: Pt -> ln: Line2D -> float` | `Line2D.distanceToPtSq` | `p: Pt -> ln: Line2D -> float` | renamed |
| `Line2D.sqDistanceToXY` | `x: float -> y: float -> ln: Line2D -> float` | `Line2D.distanceToXYSq` | `x: float -> y: float -> ln: Line2D -> float` | renamed |
| `Line2D.translate` | `v: Vc -> ln: Line2D -> Line2D` | `Line2D.move` | `v: Vc -> ln: Line2D -> Line2D` | renamed |

### Line3D

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `.SqDistanceRayPoint` | `p: Pnt -> float` | `.DistanceRayPointSq` | `p: Pnt -> float` | renamed |
| `.SqDistanceRayXYZ` | `x: float * y: float * z: float -> float` | `.DistanceRayXYZSq` | `x: float * y: float * z: float -> float` | renamed |
| `.SqDistanceToPnt` | `p: Pnt -> float` | `.DistanceToPntSq` | `p: Pnt -> float` | renamed |
| `.SqDistanceToXYZ` | `x: float * y: float * z: float -> float` | `.DistanceToXYZSq` | `x: float * y: float * z: float -> float` | renamed |
|  |  | `Line3D.create` | `start: Pnt -> finish: Pnt -> Line3D` | added |
| `Line3D.createFromPntAndVec` | `p: Pnt * v: Vec -> Line3D` | `Line3D.createFromPntAndVec` | `p: Pnt -> v: Vec -> Line3D` | curried |
| `Line3D.flip` | `ln: Line3D -> Line3D` | `Line3D.reverse` | `ln: Line3D -> Line3D` | renamed |
| `Line3D.intersectCone` | `ray: Line3D * coneRadius: float * coneBaseZ: float * coneTipZ: float -> float * float` | `Line3D.intersectCone` | `coneRadius: float -> coneBaseZ: float -> coneTipZ: float -> ray: Line3D -> float * float` | curried, reordered. New argument order: coneRadius coneBaseZ coneTipZ ray. |
| `Line3D.rotateOnZWithCenter` | `cen: Pnt -> r: Rotation2D -> ln: Line3D -> Line3D` | `Line3D.rotateAroundOnZ` | `cen: Pnt -> r: Rotation2D -> ln: Line3D -> Line3D` | renamed |
| `Line3D.sqDistanceRayPoint` | `p: Pnt -> ln: Line3D -> float` | `Line3D.distanceRayPointSq` | `p: Pnt -> ln: Line3D -> float` | renamed |
| `Line3D.sqDistanceRayXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` | `Line3D.distanceRayXYZSq` | `x: float -> y: float -> z: float -> ln: Line3D -> float` | renamed |
| `Line3D.sqDistanceToLine` | `lnA: Line3D -> lnB: Line3D -> float` | `Line3D.distanceToLineSq` | `lnA: Line3D -> lnB: Line3D -> float` | renamed |
| `Line3D.sqDistanceToPnt` | `p: Pnt -> ln: Line3D -> float` | `Line3D.distanceToPntSq` | `p: Pnt -> ln: Line3D -> float` | renamed |
| `Line3D.sqDistanceToXYZ` | `x: float -> y: float -> z: float -> ln: Line3D -> float` | `Line3D.distanceToXYZSq` | `x: float -> y: float -> z: float -> ln: Line3D -> float` | renamed |
| `Line3D.translate` | `v: Vec -> ln: Line3D -> Line3D` | `Line3D.move` | `v: Vec -> ln: Line3D -> Line3D` | renamed |
| `Line3D.tryIntersectCone` | `ray: Line3D * coneRadius: float * coneBaseZ: float * coneTipZ: float -> (float * float) voption` | `Line3D.tryIntersectCone` | `coneRadius: float -> coneBaseZ: float -> coneTipZ: float -> ray: Line3D -> (float * float) voption` | curried, reordered. New argument order: coneRadius coneBaseZ coneTipZ ray. |

### Matrix

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
|  |  | `Matrix.andThen` | `next: Matrix -> m: Matrix -> Matrix` | added |
| `Matrix.createLookAt` | `eye: Pnt * target: Pnt * up: Vec -> Matrix` | `Matrix.createLookAt` | `up: Vec -> eye: Pnt -> target: Pnt -> Matrix` | curried, reordered. New argument order: up eye target. |
| `Matrix.createLookAtZUp` | `eye: Pnt * target: Pnt -> Matrix` | `Matrix.createLookAtZUp` | `eye: Pnt -> target: Pnt -> Matrix` | curried |
| `Matrix.createPlaneToPlane` | `fromPlane: PPlane * toPlane: PPlane -> Matrix` | `Matrix.createPlaneToPlane` | `fromPlane: PPlane -> toPlane: PPlane -> Matrix` | curried |
|  |  | `Matrix.createRotationAroundUnitVecRadians` | `cen: Pnt -> axis: UnitVec -> angleRadians: float -> Matrix` | added |
|  |  | `Matrix.createRotationAroundVecRadians` | `cen: Pnt -> axis: Vec -> angleRadians: float -> Matrix` | added |
| `Matrix.createRotationAxis` | `axis: UnitVec * angleDegrees: float -> Matrix` | `Matrix.createRotationFromUnitVecDegrees` | `axis: UnitVec -> angleDegrees: float -> Matrix` | renamed, curried |
| `Matrix.createRotationAxis` | `axis: Vec * angleDegrees: float -> Matrix` | `Matrix.createRotationFromVecDegrees` | `axis: Vec -> angleDegrees: float -> Matrix` | renamed, curried |
| `Matrix.createRotationAxisCenter` | `axis: UnitVec * cen: Pnt * angleDegrees: float -> Matrix` | `Matrix.createRotationAroundUnitVecDegrees` | `cen: Pnt -> axis: UnitVec -> angleDegrees: float -> Matrix` | renamed, curried, reordered. New argument order: cen axis angleDegrees. |
| `Matrix.createRotationAxisCenter` | `axis: Vec * cen: Pnt * angleDegrees: float -> Matrix` | `Matrix.createRotationAroundVecDegrees` | `cen: Pnt -> axis: Vec -> angleDegrees: float -> Matrix` | renamed, curried, reordered. New argument order: cen axis angleDegrees. |
|  |  | `Matrix.createRotationFromUnitVecRadians` | `axis: UnitVec -> angleRadians: float -> Matrix` | added |
|  |  | `Matrix.createRotationFromVecRadians` | `axis: Vec -> angleRadians: float -> Matrix` | added |
| `Matrix.createRotationX` | `angleDegrees: float -> Matrix` | `Matrix.createRotationXDegrees` | `angleDegrees: float -> Matrix` | renamed |
|  |  | `Matrix.createRotationXRadians` | `angleRadians: float -> Matrix` | added |
| `Matrix.createRotationY` | `angleDegrees: float -> Matrix` | `Matrix.createRotationYDegrees` | `angleDegrees: float -> Matrix` | renamed |
|  |  | `Matrix.createRotationYRadians` | `angleRadians: float -> Matrix` | added |
| `Matrix.createRotationZ` | `angleDegrees: float -> Matrix` | `Matrix.createRotationZDegrees` | `angleDegrees: float -> Matrix` | renamed |
|  |  | `Matrix.createRotationZRadians` | `angleRadians: float -> Matrix` | added |
| `Matrix.createScale` | `x: float * y: float * z: float -> Matrix` | `Matrix.createScale` | `x: float -> y: float -> z: float -> Matrix` | curried |
| `Matrix.createTranslation` | `x: float * y: float * z: float -> Matrix` | `Matrix.createTranslationXYZ` | `x: float -> y: float -> z: float -> Matrix` | renamed, curried |
| `Matrix.createVecToVec` | `vecFrom: UnitVec * vecTo: UnitVec -> Matrix` | `Matrix.createRotationFromUnitVecs` | `vecFrom: UnitVec -> vecTo: UnitVec -> Matrix` | renamed, curried |
| `Matrix.createVecToVec` | `vecFrom: Vec * vecTo: Vec -> Matrix` | `Matrix.createRotationFromVecs` | `vecFrom: Vec -> vecTo: Vec -> Matrix` | renamed, curried |

### NPlane

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `NPlane.create` | `pnt: Pnt * normal: Vec -> NPlane` | `NPlane.createFromNormal` | `normal: Vec -> pnt: Pnt -> NPlane` | renamed, curried, reordered. New argument order: normal pnt. |
| `NPlane.create` | `pnt: Pnt * normal: UnitVec -> NPlane` | `NPlane.createFromUnitNormal` | `normal: UnitVec -> pnt: Pnt -> NPlane` | renamed, curried, reordered. New argument order: normal pnt. |
| `NPlane.createFrom3Points` | `a: Pnt -> b: Pnt -> c: Pnt -> NPlane` | `NPlane.createFrom3Pnts` | `a: Pnt -> b: Pnt -> c: Pnt -> NPlane` | renamed |
| `NPlane.createUncheckedVec` | `origin: Pnt * normal: UnitVec -> NPlane` | `NPlane.createFromUnitNormalUnchecked` | `origin: Pnt * normal: UnitVec -> NPlane` | renamed |
| `NPlane.translate` | `translation: Vec -> pl: NPlane -> NPlane` | `NPlane.move` | `translation: Vec -> pl: NPlane -> NPlane` | renamed |

### Offset2D

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `Offset2D.offset'` | `uTurnBehavior: UTurn -> dist: float -> xys: ResizeArray<float> -> ResizeArray<float>` | `Offset2D.offsetCustom` | `xys: ResizeArray<float> * distance: float * uTurnBehavior: UTurn * useUTurnBehaviorAbove: float<cosine> -> ResizeArray<float>` | renamed, tupled. Removed. Use offsetWith(xys, distance, uTurnBehavior, Cosine.``175.0``). |
| `Offset2D.offset''` | `useUTurnBehaviorAbove: float<cosine> -> uTurnBehavior: UTurn -> dist: float -> xys: ResizeArray<float> -> ResizeArray<float>` | `Offset2D.offsetCustom` | `xys: ResizeArray<float> * distance: float * uTurnBehavior: UTurn * useUTurnBehaviorAbove: float<cosine> -> ResizeArray<float>` | renamed, tupled. Tupled, takes the geometry first: offsetWith(xys, distance, uTurnBehavior, useUTurnBehaviorAbove). |
| `Offset2D.offsetVariable'` | `uTurnBehavior: UTurn -> varDistParallelBehavior: VarDistParallel -> dists: ResizeArray<float> -> xys: ResizeArray<float> -> ResizeArray<float>` | `Offset2D.offsetVariableCustom` | `xys: ResizeArray<float> * distances: ResizeArray<float> * varDistParallelBehavior: VarDistParallel * uTurnBehavior: UTurn * useVarDistParallelBehaviorBelow: float<cosine> * useUTurnBehaviorAbove: float<cosine> -> ResizeArray<float>` | renamed, tupled. Removed. Use offsetVariableWith(xys, distances, varDistParallelBehavior, uTurnBehavior, Cosine.``2.5``, Cosine.``175.0``). |
| `Offset2D.offsetVariable''` | `useUTurnBehaviorAbove: float<cosine> -> useVarDistParallelBehaviorBelow: float<cosine> -> uTurnBehavior: UTurn -> varDistParallelBehavior: VarDistParallel -> dists: ResizeArray<float> -> xys: ResizeArray<float> -> ResizeArray<float>` | `Offset2D.offsetVariableCustom` | `xys: ResizeArray<float> * distances: ResizeArray<float> * varDistParallelBehavior: VarDistParallel * uTurnBehavior: UTurn * useVarDistParallelBehaviorBelow: float<cosine> * useUTurnBehaviorAbove: float<cosine> -> ResizeArray<float>` | renamed, tupled. Tupled, takes the geometry first: offsetVariableWith(xys, distances, varDistParallelBehavior, uTurnBehavior, useVarDistParallelBehaviorBelow, useUTurnBehaviorAbove). The two cosine thresholds swapped places, pass them by position carefully. |
| `Offset2D.offsetVariableWithDirections` | `xys: ResizeArray<float> * nDirs: ResizeArray<float> * dists: IList<float> * varDistParallelBehavior: VarDistParallel * uTurnBehavior: UTurn * useVarDistParallelBehaviorBelow: float<cosine> * useUTurnBehaviorAbove: float<cosine> -> ResizeArray<float>` | `Offset2D.offsetVariableByDirections` | `xys: ResizeArray<float> * nDirs: ResizeArray<float> * dists: IList<float> * varDistParallelBehavior: VarDistParallel * uTurnBehavior: UTurn * useVarDistParallelBehaviorBelow: float<cosine> * useUTurnBehaviorAbove: float<cosine> -> ResizeArray<float>` | renamed |
| `Offset2D.offsetWithDirections` | `xys: ResizeArray<float> * dirs: ResizeArray<float> * dist: float * uTurnBehavior: UTurn * useUTurnBehaviorAbove: float<cosine> -> ResizeArray<float>` | `Offset2D.offsetByDirections` | `xys: ResizeArray<float> * dirs: ResizeArray<float> * dist: float * uTurnBehavior: UTurn * useUTurnBehaviorAbove: float<cosine> -> ResizeArray<float>` | renamed |
| `Offset2D.sqOpenTolerance` | `float` | `Offset2D.openToleranceSq` | `float` | renamed |

### Offset3D

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `Offset3D.offset'` | `refNormal: UnitVec -> distInPlane: float -> distPerpendicular: float -> xyzs: ResizeArray<float> -> ResizeArray<float>` | `Offset3D.offsetByNormal` | `refNormal: UnitVec -> distInPlane: float -> distPerpendicular: float -> xyzs: ResizeArray<float> -> ResizeArray<float>` | renamed |
| `Offset3D.offsetConstantWithDirections` | `xyzs: ResizeArray<float> * dirs: ResizeArray<OffsetDirection voption> * distInPlane: float * distPerpendicular: float -> ResizeArray<float>` | `Offset3D.offsetByDirections` | `xyzs: ResizeArray<float> * dirs: ResizeArray<OffsetDirection voption> * distInPlane: float * distPerpendicular: float -> ResizeArray<float>` | renamed |
| `Offset3D.offsetVariable'` | `varDistParallelBehavior: VarDistParallel -> refNormal: UnitVec -> distancesInPlane: ResizeArray<float> -> distancesPerpendicular: ResizeArray<float> -> xyzs: ResizeArray<float> -> ResizeArray<float>` | `Offset3D.offsetVariableByNormalCustom` | `xyzs: ResizeArray<float> * distancesInPlane: ResizeArray<float> * distancesPerpendicular: ResizeArray<float> * refNormal: UnitVec * varDistParallelBehavior: VarDistParallel -> ResizeArray<float>` | renamed, tupled. Tupled: offsetVariableByNormalWith(xyzs, inPlanes, perpendiculars, refNormal, varDistParallelBehavior). For VarDistParallel.Fail use the curried offsetVariableByNormal refNormal inPlanes perpendiculars xyzs. |
|  |  | `Offset3D.offsetVariableByNormal` | `refNormal: UnitVec -> distancesInPlane: ResizeArray<float> -> distancesPerpendicular: ResizeArray<float> -> xyzs: ResizeArray<float> -> ResizeArray<float>` | added |
| `Offset3D.offsetVariableWithDirections` | `xyzs: ResizeArray<float> * segmentDirs: ResizeArray<float> * offDirs: ResizeArray<OffsetDirection voption> * distsInPlane: IList<float> * distsPerpendicular: IList<float> * isClosed: bool * varDistParallelBehavior: VarDistParallel -> ResizeArray<float>` | `Offset3D.offsetVariableByDirections` | `xyzs: ResizeArray<float> * segmentDirs: ResizeArray<float> * offDirs: ResizeArray<OffsetDirection voption> * distsInPlane: IList<float> * distsPerpendicular: IList<float> * isClosed: bool * varDistParallelBehavior: VarDistParallel -> ResizeArray<float>` | renamed |
| `Offset3D.sqOpenTolerance` | `float` | `Offset3D.openToleranceSq` | `float` | renamed |

### PPlane

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `.Xaxis` | `unit -> UnitVec` | `.XAxis` | `unit -> UnitVec` | renamed |
| `.Yaxis` | `unit -> UnitVec` | `.YAxis` | `unit -> UnitVec` | renamed |
| `.Zaxis` | `unit -> UnitVec` | `.ZAxis` | `unit -> UnitVec` | renamed |
| `PPlane.create` | `origin: Pnt * axisX: UnitVec * axisY: UnitVec * axisZ: UnitVec -> PPlane` | `PPlane.create` | `xAxis: UnitVec -> yAxis: UnitVec -> zAxis: UnitVec -> origin: Pnt -> PPlane` | curried, reordered. New argument order: axisX axisY axisZ origin. |
| `PPlane.createOriginNormal` | `origin: Pnt * normal: Vec -> PPlane` | `PPlane.createFromNormal` | `normal: Vec -> origin: Pnt -> PPlane` | renamed, curried, reordered. New argument order: normal origin. |
| `PPlane.createOriginNormal` | `origin: Pnt * normal: UnitVec -> PPlane` | `PPlane.createFromUnitNormal` | `normal: UnitVec -> origin: Pnt -> PPlane` | renamed, curried, reordered. New argument order: normal origin. |
| `PPlane.createOriginNormalXaxis` | `origin: Pnt * normal: Vec * xAxis: Vec -> PPlane` | `PPlane.createFromNormalAndXAxis` | `normal: Vec -> xAxis: Vec -> origin: Pnt -> PPlane` | renamed, curried, reordered. New argument order: normal xAxis origin. |
| `PPlane.createOriginNormalXaxis` | `origin: Pnt * normal: UnitVec * xAxis: UnitVec -> PPlane` | `PPlane.createFromUnitNormalAndXAxis` | `normal: UnitVec -> xAxis: UnitVec -> origin: Pnt -> PPlane` | renamed, curried, reordered. New argument order: normal xAxis origin. |
| `PPlane.createOriginXaxisYaxis` | `origin: Pnt * xAxis: UnitVec * yAxis: UnitVec -> PPlane` | `PPlane.createFromUnitXAxisAndYAxis` | `xAxis: UnitVec -> yAxis: UnitVec -> origin: Pnt -> PPlane` | renamed, curried, reordered. New argument order: xAxis yAxis origin. |
| `PPlane.createOriginXaxisYaxis` | `origin: Pnt * xAxis: Vec * yAxis: Vec -> PPlane` | `PPlane.createFromXAxisAndYAxis` | `xAxis: Vec -> yAxis: Vec -> origin: Pnt -> PPlane` | renamed, curried, reordered. New argument order: xAxis yAxis origin. |
| `PPlane.createThreePoints` | `origin: Pnt -> xPnt: Pnt -> yPnt: Pnt -> PPlane` | `PPlane.createFrom3Pnts` | `origin: Pnt -> xPnt: Pnt -> yPnt: Pnt -> PPlane` | renamed |
| `PPlane.createUncheckedVec` | `origin: Pnt * axisX: UnitVec * axisY: UnitVec * axisZ: UnitVec -> PPlane` | `PPlane.createFromAxesUnchecked` | `origin: Pnt * xAxis: UnitVec * yAxis: UnitVec * zAxis: UnitVec -> PPlane` | renamed |
| `PPlane.evaluateAt` | `px: float * py: float -> pl: PPlane -> Pnt` | `PPlane.evaluateAt` | `px: float -> py: float -> pl: PPlane -> Pnt` | curried |
| `PPlane.evaluateAtXYZ` | `px: float * py: float * pz: float -> pl: PPlane -> Pnt` | `PPlane.evaluateAtXYZ` | `px: float -> py: float -> pz: float -> pl: PPlane -> Pnt` | curried |
| `PPlane.translateLocalX` | `d: float -> pl: PPlane -> PPlane` | `PPlane.moveAlongLocalX` | `d: float -> pl: PPlane -> PPlane` | renamed |
| `PPlane.translateLocalY` | `d: float -> pl: PPlane -> PPlane` | `PPlane.moveAlongLocalY` | `d: float -> pl: PPlane -> PPlane` | renamed |
| `PPlane.translateLocalZ` | `d: float -> pl: PPlane -> PPlane` | `PPlane.moveAlongLocalZ` | `d: float -> pl: PPlane -> PPlane` | renamed |

### Pnt

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `.RotateOnZ90CCW` | `unit -> Pnt` | `.Rotate2Dby90CCW` | `unit -> Pnt` | renamed |
| `.RotateOnZ90CW` | `unit -> Pnt` | `.Rotate2Dby90CW` | `unit -> Pnt` | renamed |
| `.SqDistanceFromOrigin` | `unit -> float` | `.DistanceFromOriginSq` | `unit -> float` | renamed |
| `.SqDistanceInXYFromOrigin` | `unit -> float` | `.DistanceInXYFromOriginSq` | `unit -> float` | renamed |
| `.SqDistanceTo` | `b: Pnt -> float` | `.DistanceToSq` | `b: Pnt -> float` | renamed |
| `.SqDistanceToLine` | `ln: Line3D -> float` | `.DistanceToLineSq` | `ln: Line3D -> float` | renamed |
| `.SqDistanceToXYZ` | `x: float * y: float * z: float -> float` | `.DistanceToXYZSq` | `x: float * y: float * z: float -> float` | renamed |
| `Pnt.addVec` | `v: Vec -> a: Pnt -> Pnt` | `Pnt.move` | `shift: Vec -> pnt: Pnt -> Pnt` | renamed |
| `Pnt.angle180Pnts` | `pntPrev: Pnt * pntThis: Pnt * pntNext: Pnt -> float` | `Pnt.angle180Pnts` | `pntPrev: Pnt -> pntThis: Pnt -> pntNext: Pnt -> float` | curried |
| `Pnt.angle2PiInXYTo` | `fromPnt: Pnt * toPnt: Pnt -> float` | `Pnt.angle2PiInXYTo` | `fromPnt: Pnt -> toPnt: Pnt -> float` | curried |
| `Pnt.angle360InXYTo` | `fromPnt: Pnt * toPnt: Pnt -> float` | `Pnt.angle360InXYTo` | `fromPnt: Pnt -> toPnt: Pnt -> float` | curried |
| `Pnt.angleInCorner` | `prevPnt: Pnt * thisPnt: Pnt * nextPnt: Pnt -> float` | `Pnt.angleInCorner` | `prevPnt: Pnt -> thisPnt: Pnt -> nextPnt: Pnt -> float` | curried |
| `Pnt.anglePiPnts` | `pntPrev: Pnt * pntThis: Pnt * pntNext: Pnt -> float` | `Pnt.anglePiPnts` | `pntPrev: Pnt -> pntThis: Pnt -> pntNext: Pnt -> float` | curried |
| `Pnt.bisector` | `pntPrev: Pnt * pntThis: Pnt * pntNext: Pnt -> Vec` | `Pnt.bisector` | `pntPrev: Pnt -> pntThis: Pnt -> pntNext: Pnt -> Vec` | curried |
|  |  | `Pnt.create` | `x: float -> y: float -> z: float -> Pnt` | added |
| `Pnt.directionDiamondInXYTo` | `fromPnt: Pnt * toPnt: Pnt -> float` | `Pnt.directionDiamondInXYTo` | `fromPnt: Pnt -> toPnt: Pnt -> float` | curried |
| `Pnt.dist` | `b: Pnt -> p: Pnt -> float` | `Pnt.distanceTo` | `b: Pnt -> p: Pnt -> float` | renamed |
| `Pnt.distPnt` | `fromPnt: Pnt * dirPnt: Pnt * distance: float -> Pnt` | `Pnt.distPnt` | `fromPnt: Pnt -> dirPnt: Pnt -> distance: float -> Pnt` | curried |
| `Pnt.distanceToXYZ` | `x: float * y: float * z: float -> p: Pnt -> float` | `Pnt.distanceToXYZ` | `x: float -> y: float -> z: float -> p: Pnt -> float` | curried |
| `Pnt.divPnt` | `fromPnt: Pnt * toPnt: Pnt * rel: float -> Pnt` | `Pnt.divPnt` | `fromPnt: Pnt -> toPnt: Pnt -> rel: float -> Pnt` | curried |
| `Pnt.extendToZLevel` | `fromPnt: Pnt * toPnt: Pnt * z: float -> Pnt` | `Pnt.extendToZLevel` | `fromPnt: Pnt -> toPnt: Pnt -> z: float -> Pnt` | curried |
| `Pnt.lerp` | `fromPnt: Pnt * toPnt: Pnt * rel: float -> Pnt` | `Pnt.lerp` | `fromPnt: Pnt -> toPnt: Pnt -> rel: float -> Pnt` | curried |
| `Pnt.normalOf3Pnts` | `a: Pnt * b: Pnt * c: Pnt -> Vec` | `Pnt.normalOf3Pnts` | `a: Pnt -> b: Pnt -> c: Pnt -> Vec` | curried |
| `Pnt.normalOfTwoPointsInXY` | `fromPnt: Pnt * toPnt: Pnt -> Vec` | `Pnt.normalOfTwoPointsInXY` | `fromPnt: Pnt -> toPnt: Pnt -> Vec` | curried |
| `Pnt.projectedParameter` | `fromPnt: Pnt * toPnt: Pnt * testPnt: Pnt -> float` | `Pnt.projectedParameterBetweenPnts` | `fromPnt: Pnt -> toPnt: Pnt -> testPnt: Pnt -> float` | renamed, curried |
| `Pnt.projectedParameter` | `fromPnt: Pnt * uv: UnitVec * testPnt: Pnt -> float` | `Pnt.projectedParameterOnUnitVec` | `fromPnt: Pnt -> uv: UnitVec -> testPnt: Pnt -> float` | renamed, curried |
| `Pnt.projectedParameter` | `fromPnt: Pnt * v: Vec * testPnt: Pnt -> float` | `Pnt.projectedParameterOnVec` | `fromPnt: Pnt -> v: Vec -> testPnt: Pnt -> float` | renamed, curried |
| `Pnt.rotateOnXDeg` | `angDegree: float -> pnt: Pnt -> Pnt` | `Pnt.rotateOnXDegrees` | `angDegree: float -> pnt: Pnt -> Pnt` | renamed |
| `Pnt.rotateOnXWithCenter` | `cen: Pnt -> r: Rotation2D -> pnt: Pnt -> Pnt` | `Pnt.rotateAroundOnX` | `cen: Pnt -> r: Rotation2D -> pnt: Pnt -> Pnt` | renamed |
| `Pnt.rotateOnXWithCenterDeg` | `cen: Pnt -> angDegree: float -> pnt: Pnt -> Pnt` | `Pnt.rotateAroundOnXDegrees` | `cen: Pnt -> angDegree: float -> pnt: Pnt -> Pnt` | renamed |
| `Pnt.rotateOnYDeg` | `angDegree: float -> pnt: Pnt -> Pnt` | `Pnt.rotateOnYDegrees` | `angDegree: float -> pnt: Pnt -> Pnt` | renamed |
| `Pnt.rotateOnYWithCenter` | `cen: Pnt -> r: Rotation2D -> pnt: Pnt -> Pnt` | `Pnt.rotateAroundOnY` | `cen: Pnt -> r: Rotation2D -> pnt: Pnt -> Pnt` | renamed |
| `Pnt.rotateOnYWithCenterDeg` | `cen: Pnt -> angDegree: float -> pnt: Pnt -> Pnt` | `Pnt.rotateAroundOnYDegrees` | `cen: Pnt -> angDegree: float -> pnt: Pnt -> Pnt` | renamed |
| `Pnt.rotateOnZ90CCW` | `pnt: Pnt -> Pnt` | `Pnt.rotate2Dby90CCW` | `pnt: Pnt -> Pnt` | renamed |
| `Pnt.rotateOnZ90CW` | `pnt: Pnt -> Pnt` | `Pnt.rotate2Dby90CW` | `pnt: Pnt -> Pnt` | renamed |
| `Pnt.rotateOnZDeg` | `angDegree: float -> pnt: Pnt -> Pnt` | `Pnt.rotateOnZDegrees` | `angDegree: float -> pnt: Pnt -> Pnt` | renamed |
| `Pnt.rotateOnZWithCenter` | `cen: Pnt -> r: Rotation2D -> pnt: Pnt -> Pnt` | `Pnt.rotateAroundOnZ` | `cen: Pnt -> r: Rotation2D -> pnt: Pnt -> Pnt` | renamed |
| `Pnt.rotateOnZWithCenterDeg` | `cen: Pnt -> angDegree: float -> pnt: Pnt -> Pnt` | `Pnt.rotateAroundOnZDegrees` | `cen: Pnt -> angDegree: float -> pnt: Pnt -> Pnt` | renamed |
| `Pnt.sqDist` | `b: Pnt -> p: Pnt -> float` | `Pnt.distanceToSq` | `b: Pnt -> p: Pnt -> float` | renamed |
| `Pnt.sqDistanceFromOrigin` | `pnt: Pnt -> float` | `Pnt.distanceFromOriginSq` | `pnt: Pnt -> float` | renamed |
| `Pnt.sqDistanceInXYFromOrigin` | `pnt: Pnt -> float` | `Pnt.distanceInXYFromOriginSq` | `pnt: Pnt -> float` | renamed |
| `Pnt.sqDistanceTo` | `b: Pnt -> p: Pnt -> float` | `Pnt.distanceToSq` | `b: Pnt -> p: Pnt -> float` | renamed |
| `Pnt.sqDistanceToLine` | `ln: Line3D -> testPnt: Pnt -> float` | `Pnt.distanceToLineSq` | `ln: Line3D -> testPnt: Pnt -> float` | renamed |
| `Pnt.sqDistanceToXYZ` | `x: float * y: float * z: float -> p: Pnt -> float` | `Pnt.distanceToXYZSq` | `x: float -> y: float -> z: float -> p: Pnt -> float` | renamed, curried |
| `Pnt.translate` | `shift: Vec -> pnt: Pnt -> Pnt` | `Pnt.move` | `shift: Vec -> pnt: Pnt -> Pnt` | renamed |

### Points2D

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `Points2D.closestPoint` | `pts: IList<Pt> * pt: Pt -> Pt` | `Points2D.closestPoint` | `pt: Pt -> pts: IList<Pt> -> Pt` | curried, reordered. New argument order: pt pts. |
| `Points2D.closestPointIdx` | `pts: IList<Pt> * pt: Pt -> int` | `Points2D.closestPointIndex` | `pt: Pt -> pts: IList<Pt> -> int` | renamed, curried, reordered. New argument order: pt pts. |
| `Points2D.closestPointsIdx` | `xs: IList<Pt> * ys: IList<Pt> -> int * int` | `Points2D.closestPointsIndices` | `xs: IList<Pt> -> ys: IList<Pt> -> int * int` | renamed, curried |
| `Points2D.minDistBetweenPointSets` | `xs: IList<Pt> * ys: IList<Pt> -> float` | `Points2D.minDistanceBetweenPointSets` | `xs: IList<Pt> -> ys: IList<Pt> -> float` | renamed, curried |
| `Points2D.mostDistantPoint` | `findPointFrom: IList<Pt> * checkAgainst: IList<Pt> -> Pt` | `Points2D.mostDistantPoint` | `checkAgainst: IList<Pt> -> findPointFrom: IList<Pt> -> Pt` | curried, reordered. New argument order: checkAgainst findPointFrom. |
| `Points2D.mostDistantPointIdx` | `findPointFrom: IList<Pt> * checkAgainst: IList<Pt> -> int * int` | `Points2D.mostDistantPointIndices` | `checkAgainst: IList<Pt> -> findPointFrom: IList<Pt> -> int * int` | renamed, curried, reordered. New argument order: checkAgainst findPointFrom. |

### Points3D

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `Points3D.closestPoint` | `pts: IList<Pnt> * pnt: Pnt -> Pnt` | `Points3D.closestPoint` | `pnt: Pnt -> pts: IList<Pnt> -> Pnt` | curried, reordered. New argument order: pnt pts. |
| `Points3D.closestPointIdx` | `pts: IList<Pnt> * pnt: Pnt -> int` | `Points3D.closestPointIndex` | `pnt: Pnt -> pts: IList<Pnt> -> int` | renamed, curried, reordered. New argument order: pnt pts. |
| `Points3D.closestPointsIdx` | `xs: IList<Pnt> * ys: IList<Pnt> -> int * int` | `Points3D.closestPointsIndices` | `xs: IList<Pnt> -> ys: IList<Pnt> -> int * int` | renamed, curried |
| `Points3D.minDistBetweenPointSets` | `xs: IList<Pnt> * ys: IList<Pnt> -> float` | `Points3D.minDistanceBetweenPointSets` | `xs: IList<Pnt> -> ys: IList<Pnt> -> float` | renamed, curried |
| `Points3D.mostDistantPoint` | `findPointFrom: IList<Pnt> * checkAgainst: IList<Pnt> -> Pnt` | `Points3D.mostDistantPoint` | `checkAgainst: IList<Pnt> -> findPointFrom: IList<Pnt> -> Pnt` | curried, reordered. New argument order: checkAgainst findPointFrom. |
| `Points3D.mostDistantPointIdx` | `findPointFrom: IList<Pnt> * checkAgainst: IList<Pnt> -> int * int` | `Points3D.mostDistantPointIndices` | `checkAgainst: IList<Pnt> -> findPointFrom: IList<Pnt> -> int * int` | renamed, curried, reordered. New argument order: checkAgainst findPointFrom. |

### Polyline2D

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `.GetPt` | `pointIndex: int -> Pt` | `.GetPoint` | `pointIndex: int -> Pt` | renamed |
| `.MovePt` | `pointIndex: int * v: Vc -> unit` | `.MovePoint` | `pointIndex: int * v: Vc -> unit` | renamed |
| `.MovePtClosed` | `pointIndex: int * v: Vc -> unit` | `.MovePointPreservingClosure` | `pointIndex: int * v: Vc -> unit` | renamed |
| `.MovePtXY` | `pointIndex: int * x: float * y: float -> unit` | `.MoveXY` | `pointIndex: int * x: float * y: float -> unit` | renamed |
| `.MovePtXYClosed` | `pointIndex: int * x: float * y: float -> unit` | `.MoveXYPreservingClosure` | `pointIndex: int * x: float * y: float -> unit` | renamed |
|  |  | `.SetPointPreservingClosure` | `pointIndex: int * pt: Pt -> unit` | added |
| `.SetPt` | `pointIndex: int * pt: Pt -> unit` | `.SetPoint` | `pointIndex: int * pt: Pt -> unit` | renamed |
| `.SetPtXY` | `pointIndex: int * x: float * y: float -> unit` | `.SetXY` | `pointIndex: int * x: float * y: float -> unit` | renamed |
| `.SetPtXYClosed` | `pointIndex: int * x: float * y: float -> unit` | `.SetXYPreservingClosure` | `pointIndex: int * x: float * y: float -> unit` | renamed |
| `Polyline2D.createDirectly` | `xys: ResizeArray<float> -> Polyline2D` | `Polyline2D.createFromXYs` | `xys: ResizeArray<float> -> Polyline2D` | renamed |
| `Polyline2D.createFromXYMembers` | `xyObjs: ^T seq -> Polyline2D` | `Polyline2D.createFromMembersXY` | `xyObjs: ^T seq -> Polyline2D` | renamed |
| `Polyline2D.createFromxyMembers` | `xyObjs: ^T seq -> Polyline2D` | `Polyline2D.createFromMembersxy` | `xyObjs: ^T seq -> Polyline2D` | renamed |
| `Polyline2D.getPt` | `pointIndex: int -> p: Polyline2D -> Pt` | `Polyline2D.getPoint` | `pointIndex: int -> p: Polyline2D -> Pt` | renamed |
| `Polyline2D.getXYs` | `p: Polyline2D -> ResizeArray<float>` | `Polyline2D.xys` | `p: Polyline2D -> ResizeArray<float>` | renamed |
| `Polyline2D.movePt` | `pointIndex: int -> v: Vc -> p: Polyline2D -> unit` | `Polyline2D.movePoint` | `pointIndex: int -> v: Vc -> p: Polyline2D -> unit` | renamed |
| `Polyline2D.movePtClosed` | `pointIndex: int -> v: Vc -> p: Polyline2D -> unit` | `Polyline2D.movePointPreservingClosure` | `pointIndex: int -> v: Vc -> p: Polyline2D -> unit` | renamed |
| `Polyline2D.movePtXY` | `pointIndex: int -> x: float -> y: float -> p: Polyline2D -> unit` | `Polyline2D.moveXY` | `pointIndex: int -> x: float -> y: float -> p: Polyline2D -> unit` | renamed |
| `Polyline2D.movePtXYClosed` | `pointIndex: int -> x: float -> y: float -> p: Polyline2D -> unit` | `Polyline2D.moveXYPreservingClosure` | `pointIndex: int -> x: float -> y: float -> p: Polyline2D -> unit` | renamed |
| `Polyline2D.offset` | `polyLine: Polyline2D * constantOffsetDistance: float * ?loop: bool * ?checkOrientation: bool * ?uTurnBehavior: UTurn * ?useUTurnBehaviorAbove: float<cosine> -> Polyline2D` | `Polyline2D.offset` | `offsetDistance: float -> pl: Polyline2D -> Polyline2D` | curried, reordered. New argument order: constantOffsetDistance polyLine. |
| `Polyline2D.offset` | `polyLine: Polyline2D * constantOffsetDistance: float * ?loop: bool * ?checkOrientation: bool * ?uTurnBehavior: UTurn * ?useUTurnBehaviorAbove: float<cosine> -> Polyline2D` | `Polyline2D.offsetCustom` | `polyLine: Polyline2D * constantOffsetDistance: float * ?loop: bool * ?checkOrientation: bool * ?uTurnBehavior: UTurn * ?useUTurnBehaviorAbove: float<cosine> -> Polyline2D` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `Polyline2D.offset'` | `offsetDistance: float -> pl: Polyline2D -> Polyline2D` | `Polyline2D.offset` | `offsetDistance: float -> pl: Polyline2D -> Polyline2D` | renamed |
| `Polyline2D.offsetVar` | `polyLine: Polyline2D * multipleOffsetDistances: IList<float> * ?loop: bool * ?checkOrientation: bool * ?varDistParallelBehavior: VarDistParallel * ?uTurnBehavior: UTurn * ?useVarDistParallelBehaviorBelow: float<cosine> * ?useUTurnBehaviorAbove: float<cosine> -> Polyline2D` | `Polyline2D.offsetVariable` | `multipleOffsetDistances: IList<float> -> pl: Polyline2D -> Polyline2D` | renamed, curried, reordered. New argument order: multipleOffsetDistances polyLine. |
| `Polyline2D.offsetVar` | `polyLine: Polyline2D * multipleOffsetDistances: IList<float> * ?loop: bool * ?checkOrientation: bool * ?varDistParallelBehavior: VarDistParallel * ?uTurnBehavior: UTurn * ?useVarDistParallelBehaviorBelow: float<cosine> * ?useUTurnBehaviorAbove: float<cosine> -> Polyline2D` | `Polyline2D.offsetVariableCustom` | `polyLine: Polyline2D * multipleOffsetDistances: IList<float> * ?loop: bool * ?checkOrientation: bool * ?varDistParallelBehavior: VarDistParallel * ?uTurnBehavior: UTurn * ?useVarDistParallelBehaviorBelow: float<cosine> * ?useUTurnBehaviorAbove: float<cosine> -> Polyline2D` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
|  |  | `Polyline2D.setPointPreservingClosure` | `pointIndex: int -> pt: Pt -> p: Polyline2D -> unit` | added |
| `Polyline2D.setPt` | `pointIndex: int -> pt: Pt -> p: Polyline2D -> unit` | `Polyline2D.setPoint` | `pointIndex: int -> pt: Pt -> p: Polyline2D -> unit` | renamed |
| `Polyline2D.setPtXY` | `pointIndex: int -> x: float -> y: float -> p: Polyline2D -> unit` | `Polyline2D.setXY` | `pointIndex: int -> x: float -> y: float -> p: Polyline2D -> unit` | renamed |
| `Polyline2D.setPtXYClosed` | `pointIndex: int -> x: float -> y: float -> p: Polyline2D -> unit` | `Polyline2D.setXYPreservingClosure` | `pointIndex: int -> x: float -> y: float -> p: Polyline2D -> unit` | renamed |
| `Polyline2D.translate` | `v: Vc -> pl: Polyline2D -> Polyline2D` | `Polyline2D.move` | `v: Vc -> pl: Polyline2D -> Polyline2D` | renamed |

### Polyline3D

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `.GetPnt` | `pointIndex: int -> Pnt` | `.GetPoint` | `pointIndex: int -> Pnt` | renamed |
| `.MovePnt` | `pointIndex: int * v: Vec -> unit` | `.MovePoint` | `pointIndex: int * v: Vec -> unit` | renamed |
| `.MovePntClosed` | `pointIndex: int * v: Vec -> unit` | `.MovePointPreservingClosure` | `pointIndex: int * v: Vec -> unit` | renamed |
| `.MovePntXYZ` | `pointIndex: int * x: float * y: float * z: float -> unit` | `.MoveXYZ` | `pointIndex: int * x: float * y: float * z: float -> unit` | renamed |
| `.MovePntXYZClosed` | `pointIndex: int * x: float * y: float * z: float -> unit` | `.MoveXYZPreservingClosure` | `pointIndex: int * x: float * y: float * z: float -> unit` | renamed |
| `.RotateOnZWithCenter` | `cen: Pnt -> r: Rotation2D -> unit` | `.RotateAroundOnZ` | `cen: Pnt -> r: Rotation2D -> unit` | renamed |
| `.SetPnt` | `pointIndex: int * pnt: Pnt -> unit` | `.SetPoint` | `pointIndex: int * pnt: Pnt -> unit` | renamed |
| `.SetPntXYZ` | `pointIndex: int * x: float * y: float * z: float -> unit` | `.SetXYZ` | `pointIndex: int * x: float * y: float * z: float -> unit` | renamed |
| `.SetPntXYZClosed` | `pointIndex: int * x: float * y: float * z: float -> unit` | `.SetXYZPreservingClosure` | `pointIndex: int * x: float * y: float * z: float -> unit` | renamed |
|  |  | `.SetPointPreservingClosure` | `pointIndex: int * pnt: Pnt -> unit` | added |
| `Polyline3D.createDirectly` | `xyzs: ResizeArray<float> -> Polyline3D` | `Polyline3D.createFromXYZs` | `xyzs: ResizeArray<float> -> Polyline3D` | renamed |
| `Polyline3D.createFrom2DXYMembers` | `xyObjs: ^T seq -> Polyline3D` | `Polyline3D.createFromMembersXY` | `xyObjs: ^T seq -> Polyline3D` | renamed |
| `Polyline3D.createFrom2DxyMembers` | `xyObjs: ^T seq -> Polyline3D` | `Polyline3D.createFromMembersxy` | `xyObjs: ^T seq -> Polyline3D` | renamed |
| `Polyline3D.createFromXYZMembers` | `xyzObjs: ^T seq -> Polyline3D` | `Polyline3D.createFromMembersXYZ` | `xyzObjs: ^T seq -> Polyline3D` | renamed |
| `Polyline3D.createFromxyzMembers` | `xyzObjs: ^T seq -> Polyline3D` | `Polyline3D.createFromMembersxyz` | `xyzObjs: ^T seq -> Polyline3D` | renamed |
| `Polyline3D.getPnt` | `pointIndex: int -> p: Polyline3D -> Pnt` | `Polyline3D.getPoint` | `pointIndex: int -> p: Polyline3D -> Pnt` | renamed |
| `Polyline3D.getXYZs` | `p: Polyline3D -> ResizeArray<float>` | `Polyline3D.xyzs` | `p: Polyline3D -> ResizeArray<float>` | renamed |
| `Polyline3D.movePnt` | `pointIndex: int -> v: Vec -> p: Polyline3D -> unit` | `Polyline3D.movePoint` | `pointIndex: int -> v: Vec -> p: Polyline3D -> unit` | renamed |
| `Polyline3D.movePntClosed` | `pointIndex: int -> v: Vec -> p: Polyline3D -> unit` | `Polyline3D.movePointPreservingClosure` | `pointIndex: int -> v: Vec -> p: Polyline3D -> unit` | renamed |
| `Polyline3D.movePntXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> p: Polyline3D -> unit` | `Polyline3D.moveXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> p: Polyline3D -> unit` | renamed |
| `Polyline3D.movePntXYZClosed` | `pointIndex: int -> x: float -> y: float -> z: float -> p: Polyline3D -> unit` | `Polyline3D.moveXYZPreservingClosure` | `pointIndex: int -> x: float -> y: float -> z: float -> p: Polyline3D -> unit` | renamed |
| `Polyline3D.offset` | `polyLine: Polyline3D * inPlaneOffsetDistance: float * perpendicularOffsetDistance: float * ?loop: bool -> Polyline3D` | `Polyline3D.offset` | `inPlaneOffsetDistance: float -> perpendicularOffsetDistance: float -> polyLine: Polyline3D -> Polyline3D` | curried, reordered. New argument order: inPlaneOffsetDistance perpendicularOffsetDistance polyLine. |
| `Polyline3D.offset` | `polyLine: Polyline3D * inPlaneOffsetDistance: float * perpendicularOffsetDistance: float * ?loop: bool -> Polyline3D` | `Polyline3D.offsetCustom` | `polyLine: Polyline3D * inPlaneOffsetDistance: float * perpendicularOffsetDistance: float * ?loop: bool -> Polyline3D` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `Polyline3D.offset'` | `inPlaneOffsetDistance: float -> perpendicularOffsetDistance: float -> polyLine: Polyline3D -> Polyline3D` | `Polyline3D.offset` | `inPlaneOffsetDistance: float -> perpendicularOffsetDistance: float -> polyLine: Polyline3D -> Polyline3D` | renamed |
| `Polyline3D.offsetVar` | `polyLine: Polyline3D * inPlaneOffsetDistances: ResizeArray<float> * perpendicularOffsetDistances: ResizeArray<float> * ?loop: bool * ?varDistParallelBehavior: VarDistParallel * ?considerCollinearBelow: float<cosine> * ?failAtUTurnAbove: float<cosine> -> Polyline3D` | `Polyline3D.offsetVariable` | `inPlaneOffsetDistances: ResizeArray<float> -> perpendicularOffsetDistances: ResizeArray<float> -> polyLine: Polyline3D -> Polyline3D` | renamed, curried, reordered. New argument order: inPlaneOffsetDistances perpendicularOffsetDistances polyLine. |
| `Polyline3D.offsetVar` | `polyLine: Polyline3D * inPlaneOffsetDistances: ResizeArray<float> * perpendicularOffsetDistances: ResizeArray<float> * ?loop: bool * ?varDistParallelBehavior: VarDistParallel * ?considerCollinearBelow: float<cosine> * ?failAtUTurnAbove: float<cosine> -> Polyline3D` | `Polyline3D.offsetVariableCustom` | `polyLine: Polyline3D * inPlaneOffsetDistances: ResizeArray<float> * perpendicularOffsetDistances: ResizeArray<float> * ?loop: bool * ?varDistParallelBehavior: VarDistParallel * ?considerCollinearBelow: float<cosine> * ?failAtUTurnAbove: float<cosine> -> Polyline3D` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `Polyline3D.offsetVarWithRef` | `polyLine: Polyline3D * inPlaneOffsetDistances: IList<float> * perpendicularOffsetDistances: IList<float> * refNormal: UnitVec * ?loop: bool * ?varDistParallelBehavior: VarDistParallel * ?considerCollinearBelow: float<cosine> * ?failAtUTurnAbove: float<cosine> -> Polyline3D` | `Polyline3D.offsetVariableByNormal` | `refNormal: UnitVec -> inPlaneOffsetDistances: IList<float> -> perpendicularOffsetDistances: IList<float> -> polyLine: Polyline3D -> Polyline3D` | renamed, curried, reordered. New argument order: refNormal inPlaneOffsetDistances perpendicularOffsetDistances polyLine. |
| `Polyline3D.offsetVarWithRef` | `polyLine: Polyline3D * inPlaneOffsetDistances: IList<float> * perpendicularOffsetDistances: IList<float> * refNormal: UnitVec * ?loop: bool * ?varDistParallelBehavior: VarDistParallel * ?considerCollinearBelow: float<cosine> * ?failAtUTurnAbove: float<cosine> -> Polyline3D` | `Polyline3D.offsetVariableByNormalCustom` | `polyLine: Polyline3D * inPlaneOffsetDistances: IList<float> * perpendicularOffsetDistances: IList<float> * refNormal: UnitVec * ?loop: bool * ?varDistParallelBehavior: VarDistParallel * ?considerCollinearBelow: float<cosine> * ?failAtUTurnAbove: float<cosine> -> Polyline3D` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `Polyline3D.offsetWithRef` | `polyLine: Polyline3D * inPlaneOffsetDistance: float * perpendicularOffsetDistance: float * refNormal: UnitVec * ?loop: bool -> Polyline3D` | `Polyline3D.offsetByNormal` | `refNormal: UnitVec -> inPlaneOffsetDistance: float -> perpendicularOffsetDistance: float -> polyLine: Polyline3D -> Polyline3D` | renamed, curried, reordered. New argument order: refNormal inPlaneOffsetDistance perpendicularOffsetDistance polyLine. |
| `Polyline3D.offsetWithRef` | `polyLine: Polyline3D * inPlaneOffsetDistance: float * perpendicularOffsetDistance: float * refNormal: UnitVec * ?loop: bool -> Polyline3D` | `Polyline3D.offsetByNormalCustom` | `polyLine: Polyline3D * inPlaneOffsetDistance: float * perpendicularOffsetDistance: float * refNormal: UnitVec * ?loop: bool -> Polyline3D` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `Polyline3D.rotateOnZWithCenter` | `cen: Pnt -> r: Rotation2D -> pl: Polyline3D -> Polyline3D` | `Polyline3D.rotateAroundOnZ` | `cen: Pnt -> r: Rotation2D -> pl: Polyline3D -> Polyline3D` | renamed |
| `Polyline3D.setPnt` | `pointIndex: int -> pnt: Pnt -> p: Polyline3D -> unit` | `Polyline3D.setPoint` | `pointIndex: int -> pnt: Pnt -> p: Polyline3D -> unit` | renamed |
| `Polyline3D.setPntXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> p: Polyline3D -> unit` | `Polyline3D.setXYZ` | `pointIndex: int -> x: float -> y: float -> z: float -> p: Polyline3D -> unit` | renamed |
| `Polyline3D.setPntXYZClosed` | `pointIndex: int -> x: float -> y: float -> z: float -> p: Polyline3D -> unit` | `Polyline3D.setXYZPreservingClosure` | `pointIndex: int -> x: float -> y: float -> z: float -> p: Polyline3D -> unit` | renamed |
|  |  | `Polyline3D.setPointPreservingClosure` | `pointIndex: int -> pnt: Pnt -> p: Polyline3D -> unit` | added |
| `Polyline3D.translate` | `v: Vec -> pl: Polyline3D -> Polyline3D` | `Polyline3D.move` | `v: Vec -> pl: Polyline3D -> Polyline3D` | renamed |

### Pt

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `.SqDistanceFromOrigin` | `unit -> float` | `.DistanceFromOriginSq` | `unit -> float` | renamed |
| `.SqDistanceTo` | `b: Pt -> float` | `.DistanceToSq` | `b: Pt -> float` | renamed |
| `.SqDistanceToLine` | `ln: Line2D -> float` | `.DistanceToLineSq` | `ln: Line2D -> float` | renamed |
| `.SqDistanceToXY` | `x: float * y: float -> float` | `.DistanceToXYSq` | `x: float * y: float -> float` | renamed |
| `Pt.addVc` | `v: Vc -> a: Pt -> Pt` | `Pt.move` | `shift: Vc -> pt: Pt -> Pt` | renamed |
| `Pt.angle180Pts` | `ptPrev: Pt * ptThis: Pt * ptNext: Pt -> float` | `Pt.angle180Pts` | `ptPrev: Pt -> ptThis: Pt -> ptNext: Pt -> float` | curried |
| `Pt.angle2PiTo` | `fromPt: Pt * toPt: Pt -> float` | `Pt.angle2PiTo` | `fromPt: Pt -> toPt: Pt -> float` | curried |
| `Pt.angle360To` | `fromPt: Pt * toPt: Pt -> float` | `Pt.angle360To` | `fromPt: Pt -> toPt: Pt -> float` | curried |
| `Pt.angleInCorner` | `prevPt: Pt * thisPt: Pt * nextPt: Pt -> float` | `Pt.angleInCorner` | `prevPt: Pt -> thisPt: Pt -> nextPt: Pt -> float` | curried |
| `Pt.anglePiPts` | `ptPrev: Pt * ptThis: Pt * ptNext: Pt -> float` | `Pt.anglePiPts` | `ptPrev: Pt -> ptThis: Pt -> ptNext: Pt -> float` | curried |
| `Pt.bisector` | `ptPrev: Pt * ptThis: Pt * ptNext: Pt -> Vc` | `Pt.bisector` | `ptPrev: Pt -> ptThis: Pt -> ptNext: Pt -> Vc` | curried |
|  |  | `Pt.create` | `x: float -> y: float -> Pt` | added |
| `Pt.dist` | `b: Pt -> p: Pt -> float` | `Pt.distanceTo` | `b: Pt -> p: Pt -> float` | renamed |
| `Pt.distPt` | `fromPt: Pt * dirPt: Pt * distance: float -> Pt` | `Pt.distPt` | `fromPt: Pt -> dirPt: Pt -> distance: float -> Pt` | curried |
| `Pt.distanceToXY` | `x: float * y: float -> p: Pt -> float` | `Pt.distanceToXY` | `x: float -> y: float -> p: Pt -> float` | curried |
| `Pt.divPt` | `fromPt: Pt * toPt: Pt * rel: float -> Pt` | `Pt.divPt` | `fromPt: Pt -> toPt: Pt -> rel: float -> Pt` | curried |
| `Pt.lerp` | `fromPt: Pt * toPt: Pt * rel: float -> Pt` | `Pt.lerp` | `fromPt: Pt -> toPt: Pt -> rel: float -> Pt` | curried |
| `Pt.projectedParameter` | `fromPt: Pt * toPt: Pt * testPt: Pt -> float` | `Pt.projectedParameterBetweenPts` | `fromPt: Pt -> toPt: Pt -> testPt: Pt -> float` | renamed, curried |
| `Pt.projectedParameter` | `fromPt: Pt * uv: UnitVc * testPt: Pt -> float` | `Pt.projectedParameterOnUnitVc` | `fromPt: Pt -> uv: UnitVc -> testPt: Pt -> float` | renamed, curried |
| `Pt.projectedParameter` | `fromPt: Pt * v: Vc * testPt: Pt -> float` | `Pt.projectedParameterOnVc` | `fromPt: Pt -> v: Vc -> testPt: Pt -> float` | renamed, curried |
| `Pt.sqDist` | `b: Pt -> p: Pt -> float` | `Pt.distanceToSq` | `b: Pt -> p: Pt -> float` | renamed |
| `Pt.sqDistanceFromOrigin` | `pt: Pt -> float` | `Pt.distanceFromOriginSq` | `pt: Pt -> float` | renamed |
| `Pt.sqDistanceTo` | `b: Pt -> p: Pt -> float` | `Pt.distanceToSq` | `b: Pt -> p: Pt -> float` | renamed |
| `Pt.sqDistanceToLine` | `ln: Line2D -> testPt: Pt -> float` | `Pt.distanceToLineSq` | `ln: Line2D -> testPt: Pt -> float` | renamed |
| `Pt.sqDistanceToXY` | `x: float * y: float -> p: Pt -> float` | `Pt.distanceToXYSq` | `x: float -> y: float -> p: Pt -> float` | renamed, curried |
| `Pt.translate` | `shift: Vc -> pt: Pt -> Pt` | `Pt.move` | `shift: Vc -> pt: Pt -> Pt` | renamed |

### Quaternion

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
|  |  | `Quaternion.andThen` | `next: Quaternion -> q: Quaternion -> Quaternion` | added |
| `Quaternion.createFromDegrees` | `axis: UnitVec * angleInDegrees: float -> Quaternion` | `Quaternion.createFromUnitVecDegrees` | `axis: UnitVec -> angleInDegrees: float -> Quaternion` | renamed, curried |
| `Quaternion.createFromDegrees` | `axis: Vec * angleInDegrees: float -> Quaternion` | `Quaternion.createFromVecDegrees` | `axis: Vec -> angleInDegrees: float -> Quaternion` | renamed, curried |
| `Quaternion.createFromEulerXYZ` | `degreesX: float * degreesY: float * degreesZ: float -> Quaternion` | `Quaternion.createFromEulerXYZDegrees` | `degreesX: float -> degreesY: float -> degreesZ: float -> Quaternion` | renamed, curried |
|  |  | `Quaternion.createFromEulerXYZRadians` | `radiansX: float -> radiansY: float -> radiansZ: float -> Quaternion` | added |
| `Quaternion.createFromEulerXZY` | `degreesX: float * degreesZ: float * degreesY: float -> Quaternion` | `Quaternion.createFromEulerXZYDegrees` | `degreesX: float -> degreesZ: float -> degreesY: float -> Quaternion` | renamed, curried |
|  |  | `Quaternion.createFromEulerXZYRadians` | `radiansX: float -> radiansZ: float -> radiansY: float -> Quaternion` | added |
| `Quaternion.createFromEulerYXZ` | `degreesY: float * degreesX: float * degreesZ: float -> Quaternion` | `Quaternion.createFromEulerYXZDegrees` | `degreesY: float -> degreesX: float -> degreesZ: float -> Quaternion` | renamed, curried |
|  |  | `Quaternion.createFromEulerYXZRadians` | `radiansY: float -> radiansX: float -> radiansZ: float -> Quaternion` | added |
| `Quaternion.createFromEulerYZX` | `degreesY: float * degreesZ: float * degreesX: float -> Quaternion` | `Quaternion.createFromEulerYZXDegrees` | `degreesY: float -> degreesZ: float -> degreesX: float -> Quaternion` | renamed, curried |
|  |  | `Quaternion.createFromEulerYZXRadians` | `radiansY: float -> radiansZ: float -> radiansX: float -> Quaternion` | added |
| `Quaternion.createFromEulerZXY` | `degreesZ: float * degreesX: float * degreesY: float -> Quaternion` | `Quaternion.createFromEulerZXYDegrees` | `degreesZ: float -> degreesX: float -> degreesY: float -> Quaternion` | renamed, curried |
|  |  | `Quaternion.createFromEulerZXYRadians` | `radiansZ: float -> radiansX: float -> radiansY: float -> Quaternion` | added |
| `Quaternion.createFromEulerZYX` | `degreesZ: float * degreesY: float * degreesX: float -> Quaternion` | `Quaternion.createFromEulerZYXDegrees` | `degreesZ: float -> degreesY: float -> degreesX: float -> Quaternion` | renamed, curried |
|  |  | `Quaternion.createFromEulerZYXRadians` | `radiansZ: float -> radiansY: float -> radiansX: float -> Quaternion` | added |
| `Quaternion.createFromRadians` | `axis: UnitVec * angleInRadians: float -> Quaternion` | `Quaternion.createFromUnitVecRadians` | `axis: UnitVec -> angleInRadians: float -> Quaternion` | renamed, curried |
| `Quaternion.createFromRadians` | `axis: Vec * angleInRadians: float -> Quaternion` | `Quaternion.createFromVecRadians` | `axis: Vec -> angleInRadians: float -> Quaternion` | renamed, curried |
| `Quaternion.createVecToVec` | `vecFrom: UnitVec * vecTo: UnitVec -> Quaternion` | `Quaternion.createFromUnitVecs` | `vecFrom: UnitVec -> vecTo: UnitVec -> Quaternion` | renamed, curried |
| `Quaternion.createVecToVec` | `vecFrom: Vec * vecTo: Vec -> Quaternion` | `Quaternion.createFromVecs` | `vecFrom: Vec -> vecTo: Vec -> Quaternion` | renamed, curried |

### Rect2D

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `.EvaluateDist` | `xDistance: float * yDistance: float -> Pt` | `.EvaluateDistance` | `xDistance: float * yDistance: float -> Pt` | renamed |
| `.Xaxis` | `unit -> Vc` | `.XAxis` | `unit -> Vc` | renamed |
| `.XaxisUnit` | `unit -> UnitVc` | `.XAxisUnit` | `unit -> UnitVc` | renamed |
| `.Yaxis` | `unit -> Vc` | `.YAxis` | `unit -> Vc` | renamed |
| `.YaxisUnit` | `unit -> UnitVc` | `.YAxisUnit` | `unit -> UnitVc` | renamed |
| `Rect2D.createFrom3Points` | `origin: Pt * xPt: Pt * yPt: Pt -> Rect2D` | `Rect2D.createFrom3Pts` | `origin: Pt -> xPt: Pt -> yPt: Pt -> Rect2D` | renamed, curried |
| `Rect2D.createFromCenterAndDirection` | `center: Pt * directionX: UnitVc * sizeX: float * sizeY: float -> Rect2D` | `Rect2D.createCenteredFromUnitVc` | `sizeX: float -> sizeY: float -> directionX: UnitVc -> center: Pt -> Rect2D` | renamed, curried, reordered. New argument order: sizeX sizeY directionX center. |
| `Rect2D.createFromCenterAndVector` | `center: Pt * x: Vc * sizeY: float -> Rect2D` | `Rect2D.createCenteredFromVc` | `sizeY: float -> x: Vc -> center: Pt -> Rect2D` | renamed, curried, reordered. New argument order: sizeY x center. |
| `Rect2D.createFromDirAndPoints` | `dirX: Vc -> pts: IList<Pt> -> Rect2D` | `Rect2D.createFromVcAndPts` | `dirX: Vc -> pts: IList<Pt> -> Rect2D` | renamed |
| `Rect2D.createFromDirectionAndSizes` | `origin: Pt * directionX: UnitVc * sizeX: float * sizeY: float -> Rect2D` | `Rect2D.createFromUnitVc` | `sizeX: float -> sizeY: float -> directionX: UnitVc -> origin: Pt -> Rect2D` | renamed, curried, reordered. New argument order: sizeX sizeY directionX origin. |
| `Rect2D.createFromLine` | `line: Line2D * offRight: float * offLeft: float -> Rect2D` | `Rect2D.createFromLine` | `offRight: float -> offLeft: float -> line: Line2D -> Rect2D` | curried, reordered. New argument order: offRight offLeft line. |
| `Rect2D.createFromVectors` | `origin: Pt * x: Vc * y: Vc -> Rect2D` | `Rect2D.createFromVcs` | `x: Vc -> y: Vc -> origin: Pt -> Rect2D` | renamed, curried, reordered. New argument order: x y origin. |
| `Rect2D.createFromXVectorAndWidth` | `origin: Pt * x: Vc * sizeY: float -> Rect2D` | `Rect2D.createFromVc` | `sizeY: float -> x: Vc -> origin: Pt -> Rect2D` | renamed, curried, reordered. New argument order: sizeY x origin. |
| `Rect2D.createUncheckedVec` | `origin: Pt * x: Vc * y: Vc -> Rect2D` | `Rect2D.createFromVcsUnchecked` | `origin: Pt * x: Vc * y: Vc -> Rect2D` | renamed |
| `Rect2D.evaluateDist` | `xDistance: float -> yDistance: float -> r: Rect2D -> Pt` | `Rect2D.evaluateDistance` | `xDistance: float -> yDistance: float -> r: Rect2D -> Pt` | renamed |
| `Rect2D.grid` | `rect: Rect2D * xCount: int * yCount: int -> Pt array array` | `Rect2D.grid` | `xCount: int -> yCount: int -> rect: Rect2D -> Pt array array` | curried, reordered. New argument order: xCount yCount rect. |
| `Rect2D.gridMaxLength` | `rect: Rect2D * xMaxLen: float * yMaxLen: float -> Pt array array` | `Rect2D.gridMaxLength` | `xMaxLen: float -> yMaxLen: float -> rect: Rect2D -> Pt array array` | curried, reordered. New argument order: xMaxLen yMaxLen rect. |
| `Rect2D.gridMinLength` | `rect: Rect2D * xMinLen: float * yMinLen: float -> Pt array array` | `Rect2D.gridMinLength` | `xMinLen: float -> yMinLen: float -> rect: Rect2D -> Pt array array` | curried, reordered. New argument order: xMinLen yMinLen rect. |
| `Rect2D.offsetVar` | `dist: float array -> rect: Rect2D -> Rect2D` | `Rect2D.offsetVariable` | `dist: float array -> rect: Rect2D -> Rect2D` | renamed |
| `Rect2D.subDivide` | `rect: Rect2D * xCount: int * yCount: int * xGap: float * yGap: float -> Rect2D array array` | `Rect2D.subDivide` | `xCount: int -> yCount: int -> xGap: float -> yGap: float -> rect: Rect2D -> Rect2D array array` | curried, reordered. New argument order: xCount yCount xGap yGap rect. |
| `Rect2D.translateLocalX` | `distX: float -> r: Rect2D -> Rect2D` | `Rect2D.moveAlongLocalX` | `distX: float -> r: Rect2D -> Rect2D` | renamed |
| `Rect2D.translateLocalY` | `distY: float -> r: Rect2D -> Rect2D` | `Rect2D.moveAlongLocalY` | `distY: float -> r: Rect2D -> Rect2D` | renamed |
| `Rect2D.tryCreateFrom3Points` | `origin: Pt * xPt: Pt * yPt: Pt -> Rect2D voption` | `Rect2D.tryCreateFrom3Pts` | `origin: Pt -> xPt: Pt -> yPt: Pt -> Rect2D voption` | renamed, curried |

### Rect3D

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `.EvaluateDist` | `xDistance: float * yDistance: float -> Pnt` | `.EvaluateDistance` | `xDistance: float * yDistance: float -> Pnt` | renamed |
| `.Xaxis` | `unit -> Vec` | `.XAxis` | `unit -> Vec` | renamed |
| `.XaxisUnit` | `unit -> UnitVec` | `.XAxisUnit` | `unit -> UnitVec` | renamed |
| `.Yaxis` | `unit -> Vec` | `.YAxis` | `unit -> Vec` | renamed |
| `.YaxisUnit` | `unit -> UnitVec` | `.YAxisUnit` | `unit -> UnitVec` | renamed |
| `Rect3D.createCenteredFromPlane` | `pl: PPlane * sizeX: float * sizeY: float -> Rect3D` | `Rect3D.createCenteredFromPlane` | `sizeX: float -> sizeY: float -> pl: PPlane -> Rect3D` | curried, reordered. New argument order: sizeX sizeY pl. |
| `Rect3D.createFrom3Points` | `origin: Pnt * xPnt: Pnt * yPnt: Pnt -> Rect3D` | `Rect3D.createFrom3Pnts` | `origin: Pnt -> xPnt: Pnt -> yPnt: Pnt -> Rect3D` | renamed, curried |
| `Rect3D.createFromPlane` | `pl: PPlane * sizeX: float * sizeY: float -> Rect3D` | `Rect3D.createFromPlane` | `sizeX: float -> sizeY: float -> pl: PPlane -> Rect3D` | curried, reordered. New argument order: sizeX sizeY pl. |
| `Rect3D.createFromVectors` | `origin: Pnt * x: Vec * y: Vec -> Rect3D` | `Rect3D.createFromVecs` | `x: Vec -> y: Vec -> origin: Pnt -> Rect3D` | renamed, curried, reordered. New argument order: x y origin. |
| `Rect3D.createUncheckedVec` | `origin: Pnt * x: Vec * y: Vec -> Rect3D` | `Rect3D.createFromVecsUnchecked` | `origin: Pnt * x: Vec * y: Vec -> Rect3D` | renamed |
| `Rect3D.evaluateDist` | `xDistance: float -> yDistance: float -> r: Rect3D -> Pnt` | `Rect3D.evaluateDistance` | `xDistance: float -> yDistance: float -> r: Rect3D -> Pnt` | renamed |
| `Rect3D.grid` | `rect: Rect3D * xCount: int * yCount: int -> Pnt array array` | `Rect3D.grid` | `xCount: int -> yCount: int -> rect: Rect3D -> Pnt array array` | curried, reordered. New argument order: xCount yCount rect. |
| `Rect3D.gridMaxLength` | `rect: Rect3D * xMaxLen: float * yMaxLen: float -> Pnt array array` | `Rect3D.gridMaxLength` | `xMaxLen: float -> yMaxLen: float -> rect: Rect3D -> Pnt array array` | curried, reordered. New argument order: xMaxLen yMaxLen rect. |
| `Rect3D.gridMinLength` | `rect: Rect3D * xMinLen: float * yMinLen: float -> Pnt array array` | `Rect3D.gridMinLength` | `xMinLen: float -> yMinLen: float -> rect: Rect3D -> Pnt array array` | curried, reordered. New argument order: xMinLen yMinLen rect. |
| `Rect3D.offsetVar` | `dist: float array -> rect: Rect3D -> Rect3D` | `Rect3D.offsetVariable` | `dist: float array -> rect: Rect3D -> Rect3D` | renamed |
| `Rect3D.subDivide` | `rect: Rect3D * xCount: int * yCount: int * xGap: float * yGap: float -> Rect3D array array` | `Rect3D.subDivide` | `xCount: int -> yCount: int -> xGap: float -> yGap: float -> rect: Rect3D -> Rect3D array array` | curried, reordered. New argument order: xCount yCount xGap yGap rect. |
| `Rect3D.translateLocalX` | `distX: float -> r: Rect3D -> Rect3D` | `Rect3D.moveAlongLocalX` | `distX: float -> r: Rect3D -> Rect3D` | renamed |
| `Rect3D.translateLocalY` | `distY: float -> r: Rect3D -> Rect3D` | `Rect3D.moveAlongLocalY` | `distY: float -> r: Rect3D -> Rect3D` | renamed |
| `Rect3D.translateLocalZ` | `distZ: float -> r: Rect3D -> Rect3D` | `Rect3D.moveAlongLocalZ` | `distZ: float -> r: Rect3D -> Rect3D` | renamed |
| `Rect3D.tryCreateFrom3Points` | `origin: Pnt * xPnt: Pnt * yPnt: Pnt -> Rect3D voption` | `Rect3D.tryCreateFrom3Pnts` | `origin: Pnt -> xPnt: Pnt -> yPnt: Pnt -> Rect3D voption` | renamed, curried |

### RigidMatrix

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
|  |  | `RigidMatrix.andThen` | `next: RigidMatrix -> m: RigidMatrix -> RigidMatrix` | added |
| `RigidMatrix.createPlaneToPlane` | `fromPlane: PPlane * toPlane: PPlane -> RigidMatrix` | `RigidMatrix.createPlaneToPlane` | `fromPlane: PPlane -> toPlane: PPlane -> RigidMatrix` | curried |
|  |  | `RigidMatrix.createRotationAroundUnitVecRadians` | `cen: Pnt -> axis: UnitVec -> angleRadians: float -> RigidMatrix` | added |
|  |  | `RigidMatrix.createRotationAroundVecRadians` | `cen: Pnt -> axis: Vec -> angleRadians: float -> RigidMatrix` | added |
| `RigidMatrix.createRotationAxis` | `axis: UnitVec * angleDegrees: float -> RigidMatrix` | `RigidMatrix.createRotationFromUnitVecDegrees` | `axis: UnitVec -> angleDegrees: float -> RigidMatrix` | renamed, curried |
| `RigidMatrix.createRotationAxis` | `axis: Vec * angleDegrees: float -> RigidMatrix` | `RigidMatrix.createRotationFromVecDegrees` | `axis: Vec -> angleDegrees: float -> RigidMatrix` | renamed, curried |
| `RigidMatrix.createRotationAxisCenter` | `axis: UnitVec * cen: Pnt * angleDegrees: float -> RigidMatrix` | `RigidMatrix.createRotationAroundUnitVecDegrees` | `cen: Pnt -> axis: UnitVec -> angleDegrees: float -> RigidMatrix` | renamed, curried, reordered. New argument order: cen axis angleDegrees. |
| `RigidMatrix.createRotationAxisCenter` | `axis: Vec * cen: Pnt * angleDegrees: float -> RigidMatrix` | `RigidMatrix.createRotationAroundVecDegrees` | `cen: Pnt -> axis: Vec -> angleDegrees: float -> RigidMatrix` | renamed, curried, reordered. New argument order: cen axis angleDegrees. |
|  |  | `RigidMatrix.createRotationFromUnitVecRadians` | `axis: UnitVec -> angleRadians: float -> RigidMatrix` | added |
|  |  | `RigidMatrix.createRotationFromVecRadians` | `axis: Vec -> angleRadians: float -> RigidMatrix` | added |
| `RigidMatrix.createRotationX` | `angleDegrees: float -> RigidMatrix` | `RigidMatrix.createRotationXDegrees` | `angleDegrees: float -> RigidMatrix` | renamed |
|  |  | `RigidMatrix.createRotationXRadians` | `angleRadians: float -> RigidMatrix` | added |
| `RigidMatrix.createRotationY` | `angleDegrees: float -> RigidMatrix` | `RigidMatrix.createRotationYDegrees` | `angleDegrees: float -> RigidMatrix` | renamed |
|  |  | `RigidMatrix.createRotationYRadians` | `angleRadians: float -> RigidMatrix` | added |
| `RigidMatrix.createRotationZ` | `angleDegrees: float -> RigidMatrix` | `RigidMatrix.createRotationZDegrees` | `angleDegrees: float -> RigidMatrix` | renamed |
|  |  | `RigidMatrix.createRotationZRadians` | `angleRadians: float -> RigidMatrix` | added |
| `RigidMatrix.createTranslation` | `x: float * y: float * z: float -> RigidMatrix` | `RigidMatrix.createTranslationXYZ` | `x: float -> y: float -> z: float -> RigidMatrix` | renamed, curried |
| `RigidMatrix.createVecToVec` | `vecFrom: UnitVec * vecTo: UnitVec -> RigidMatrix` | `RigidMatrix.createRotationFromUnitVecs` | `vecFrom: UnitVec -> vecTo: UnitVec -> RigidMatrix` | renamed, curried |
| `RigidMatrix.createVecToVec` | `vecFrom: Vec * vecTo: Vec -> RigidMatrix` | `RigidMatrix.createRotationFromVecs` | `vecFrom: Vec -> vecTo: Vec -> RigidMatrix` | renamed, curried |

### Rotation2D

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `.Add` | `ro: Rotation2D -> Rotation2D` | `.Then` | `ro: Rotation2D -> Rotation2D` | renamed |
| `.InDegrees` | `unit -> float` | `.AngleInDegrees` | `unit -> float` | renamed |
| `.InRadians` | `unit -> float` | `.AngleInRadians` | `unit -> float` | renamed |
| `Rotation2D.add` | `ro: Rotation2D -> r: Rotation2D -> Rotation2D` | `Rotation2D.andThen` | `ro: Rotation2D -> r: Rotation2D -> Rotation2D` | renamed |
| `Rotation2D.create` | `sine: float * cosine: float -> Rotation2D` | `Rotation2D.create` | `sine: float -> cosine: float -> Rotation2D` | curried |
| `Rotation2D.createFromVectors` | `a: UnitVc * b: UnitVc -> Rotation2D` | `Rotation2D.createFromUnitVcs` | `a: UnitVc -> b: UnitVc -> Rotation2D` | renamed, curried |
| `Rotation2D.createFromVectors` | `a: Vc * b: Vc -> Rotation2D` | `Rotation2D.createFromVcs` | `a: Vc -> b: Vc -> Rotation2D` | renamed, curried |
| `Rotation2D.inDegrees` | `r: Rotation2D -> float` | `Rotation2D.angleInDegrees` | `r: Rotation2D -> float` | renamed |
| `Rotation2D.inRadians` | `r: Rotation2D -> float` | `Rotation2D.angleInRadians` | `r: Rotation2D -> float` | renamed |

### Similarity2D

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `Similarity2D.getGrouped` | `tolerance: float * items: ResizeArray<'T> * sims: ResizeArray<ObjectToCheck> -> ResizeArray<ResizeArray<'T>>` | `Similarity2D.getGrouped` | `tolerance: float -> sims: ResizeArray<ObjectToCheck> -> items: ResizeArray<'T> -> ResizeArray<ResizeArray<'T>>` | curried, reordered. New argument order: tolerance sims items. |

### Topology2D

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `Topology2D.join` | `getLine: ('T -> Line2D) * splitDistance: float * xs: ResizeArray<'T> -> ResizeArray<ResizeArray<'T>>` | `Topology2D.join` | `getLine: ('T -> Line2D) -> splitDistance: float -> xs: ResizeArray<'T> -> ResizeArray<ResizeArray<'T>>` | curried |
| `Topology2D.joinReversing` | `getLine: ('T -> Line2D) * splitDistance: float * xs: ResizeArray<'T> -> ResizeArray<ResizeArray<'T * bool>>` | `Topology2D.joinReversing` | `getLine: ('T -> Line2D) -> splitDistance: float -> xs: ResizeArray<'T> -> ResizeArray<ResizeArray<'T * bool>>` | curried |
| `Topology2D.sortToLoop` | `getLine: ('T -> Line2D) * ts: ResizeArray<'T> -> unit` | `Topology2D.sortToLoop` | `getLine: ('T -> Line2D) -> ts: ResizeArray<'T> -> unit` | curried |
| `Topology2D.sortToLoopWithReversing` | `getLine: ('T -> Line2D) * reverseInPlace: (int -> 'T -> unit) * ts: ResizeArray<'T> -> unit` | `Topology2D.sortToLoopWithReversing` | `getLine: ('T -> Line2D) -> reverseInPlace: (int -> 'T -> unit) -> ts: ResizeArray<'T> -> unit` | curried |

### Topology3D

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `Topology3D.join` | `getLine: ('T -> Line3D) * splitDistance: float * xs: ResizeArray<'T> -> ResizeArray<ResizeArray<'T>>` | `Topology3D.join` | `getLine: ('T -> Line3D) -> splitDistance: float -> xs: ResizeArray<'T> -> ResizeArray<ResizeArray<'T>>` | curried |
| `Topology3D.joinReversing` | `getLine: ('T -> Line3D) * splitDistance: float * xs: ResizeArray<'T> -> ResizeArray<ResizeArray<'T * bool>>` | `Topology3D.joinReversing` | `getLine: ('T -> Line3D) -> splitDistance: float -> xs: ResizeArray<'T> -> ResizeArray<ResizeArray<'T * bool>>` | curried |
| `Topology3D.sortToLoop` | `getLine: ('T -> Line3D) * ts: ResizeArray<'T> -> unit` | `Topology3D.sortToLoop` | `getLine: ('T -> Line3D) -> ts: ResizeArray<'T> -> unit` | curried |
| `Topology3D.sortToLoopCached` | `getLine: ('T -> Line3D) * ts: ResizeArray<'T> -> float array` | `Topology3D.sortToLoopCached` | `getLine: ('T -> Line3D) -> ts: ResizeArray<'T> -> float array` | curried |
| `Topology3D.sortToLoopWithReversing` | `getLine: ('T -> Line3D) * reverseInPlace: (int -> 'T -> unit) * ts: ResizeArray<'T> -> unit` | `Topology3D.sortToLoopWithReversing` | `getLine: ('T -> Line3D) -> reverseInPlace: (int -> 'T -> unit) -> ts: ResizeArray<'T> -> unit` | curried |
| `Topology3D.sortToLoopWithReversingCached` | `getLine: ('T -> Line3D) * reverseInPlace: (int -> 'T -> unit) * ts: ResizeArray<'T> -> float array` | `Topology3D.sortToLoopWithReversingCached` | `getLine: ('T -> Line3D) -> reverseInPlace: (int -> 'T -> unit) -> ts: ResizeArray<'T> -> float array` | curried |

### Tria2D

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `Tria2D.area` | `a: Pt * b: Pt * c: Pt -> float` | `Tria2D.area` | `a: Pt -> b: Pt -> c: Pt -> float` | curried |
| `Tria2D.areaDouble` | `a: Pt * b: Pt * c: Pt -> float` | `Tria2D.areaDouble` | `a: Pt -> b: Pt -> c: Pt -> float` | curried |
| `Tria2D.areaSigned` | `a: Pt * b: Pt * c: Pt -> float` | `Tria2D.areaSigned` | `a: Pt -> b: Pt -> c: Pt -> float` | curried |
| `Tria2D.isLinear` | `a: Pt * b: Pt * c: Pt * ?distanceTolerance: float -> bool` | `Tria2D.isLinear` | `a: Pt -> b: Pt -> c: Pt -> bool` | curried |
| `Tria2D.isLinear` | `a: Pt * b: Pt * c: Pt * ?distanceTolerance: float -> bool` | `Tria2D.isLinearCustom` | `a: Pt * b: Pt * c: Pt * ?distanceTolerance: float -> bool` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `Tria2D.isLinearFast` | `a: Pt * b: Pt * c: Pt * ?maxAreaParallelogram: float -> bool` | `Tria2D.isLinearFast` | `a: Pt -> b: Pt -> c: Pt -> bool` | curried |
| `Tria2D.isLinearFast` | `a: Pt * b: Pt * c: Pt * ?maxAreaParallelogram: float -> bool` | `Tria2D.isLinearFastCustom` | `a: Pt * b: Pt * c: Pt * ?maxAreaParallelogram: float -> bool` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `Tria2D.offset` | `a: Pt * b: Pt * c: Pt * dist: float -> Pt * Pt * Pt` | `Tria2D.offset` | `dist: float -> a: Pt -> b: Pt -> c: Pt -> Pt * Pt * Pt` | curried, reordered. New argument order: dist a b c. |
| `Tria2D.offsetPt` | `ptToOffset: Pt * prev: Pt * next: Pt * dist: float -> Pt` | `Tria2D.offsetPt` | `dist: float -> prev: Pt -> next: Pt -> ptToOffset: Pt -> Pt` | curried, reordered. New argument order: dist prev next ptToOffset. |
| `Tria2D.offsetPtVar` | `ptToOffset: Pt * prev: Pt * next: Pt * distPrev: float * distNext: float -> Pt` | `Tria2D.offsetPtVariable` | `distPrev: float -> distNext: float -> prev: Pt -> next: Pt -> ptToOffset: Pt -> Pt` | renamed, curried, reordered. New argument order: distPrev distNext prev next ptToOffset. |
| `Tria2D.offsetPtVarByNormals` | `ptToOffset: Pt * prevN: UnitVc * nextN: UnitVc * prevDist: float * nextDist: float -> Pt` | `Tria2D.offsetPtVariableByNormals` | `prevDist: float -> nextDist: float -> prevN: UnitVc -> nextN: UnitVc -> ptToOffset: Pt -> Pt` | renamed, curried, reordered. New argument order: prevDist nextDist prevN nextN ptToOffset. |
| `Tria2D.tryOffsetPtVar` | `ptToOffset: Pt * prev: Pt * next: Pt * distPrev: float * distNext: float -> Pt voption` | `Tria2D.tryOffsetPtVariable` | `distPrev: float -> distNext: float -> prev: Pt -> next: Pt -> ptToOffset: Pt -> Pt voption` | renamed, curried, reordered. New argument order: distPrev distNext prev next ptToOffset. |
| `Tria2D.tryOffsetPtVarByNormals` | `ptToOffset: Pt * prevN: UnitVc * nextN: UnitVc * prevDist: float * nextDist: float -> Pt voption` | `Tria2D.tryOffsetPtVariableByNormals` | `prevDist: float -> nextDist: float -> prevN: UnitVc -> nextN: UnitVc -> ptToOffset: Pt -> Pt voption` | renamed, curried, reordered. New argument order: prevDist nextDist prevN nextN ptToOffset. |

### Tria3D

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `Tria3D.area` | `a: Pnt * b: Pnt * c: Pnt -> float` | `Tria3D.area` | `a: Pnt -> b: Pnt -> c: Pnt -> float` | curried |
| `Tria3D.areaDouble` | `a: Pnt * b: Pnt * c: Pnt -> float` | `Tria3D.areaDouble` | `a: Pnt -> b: Pnt -> c: Pnt -> float` | curried |
| `Tria3D.intersectLine` | `line: Line3D * p1: Pnt * p2: Pnt * p3: Pnt -> Pnt` | `Tria3D.intersectLine` | `p1: Pnt -> p2: Pnt -> p3: Pnt -> line: Line3D -> Pnt` | curried, reordered. New argument order: p1 p2 p3 line. |
| `Tria3D.intersectRay` | `line: Line3D * p1: Pnt * p2: Pnt * p3: Pnt -> float` | `Tria3D.intersectRay` | `p1: Pnt -> p2: Pnt -> p3: Pnt -> line: Line3D -> float` | curried, reordered. New argument order: p1 p2 p3 line. |
| `Tria3D.isLinear` | `a: Pnt * b: Pnt * c: Pnt * ?distanceTolerance: float -> bool` | `Tria3D.isLinear` | `a: Pnt -> b: Pnt -> c: Pnt -> bool` | curried |
| `Tria3D.isLinear` | `a: Pnt * b: Pnt * c: Pnt * ?distanceTolerance: float -> bool` | `Tria3D.isLinearCustom` | `a: Pnt * b: Pnt * c: Pnt * ?distanceTolerance: float -> bool` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `Tria3D.isLinearFast` | `a: Pnt * b: Pnt * c: Pnt * ?maxAreaParallelogram: float -> bool` | `Tria3D.isLinearFast` | `a: Pnt -> b: Pnt -> c: Pnt -> bool` | curried |
| `Tria3D.isLinearFast` | `a: Pnt * b: Pnt * c: Pnt * ?maxAreaParallelogram: float -> bool` | `Tria3D.isLinearFastCustom` | `a: Pnt * b: Pnt * c: Pnt * ?maxAreaParallelogram: float -> bool` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `Tria3D.offset` | `a: Pnt * b: Pnt * c: Pnt * dist: float -> Pnt * Pnt * Pnt` | `Tria3D.offset` | `dist: float -> a: Pnt -> b: Pnt -> c: Pnt -> Pnt * Pnt * Pnt` | curried, reordered. New argument order: dist a b c. |
| `Tria3D.offsetPnt` | `pntToOffset: Pnt * prev: Pnt * next: Pnt * dist: float -> Pnt` | `Tria3D.offsetPnt` | `dist: float -> prev: Pnt -> next: Pnt -> pntToOffset: Pnt -> Pnt` | curried, reordered. New argument order: dist prev next pntToOffset. |
| `Tria3D.offsetPntVar` | `pntToOffset: Pnt * prev: Pnt * next: Pnt * distPrev: float * distNext: float -> Pnt` | `Tria3D.offsetPntVariable` | `distPrev: float -> distNext: float -> prev: Pnt -> next: Pnt -> pntToOffset: Pnt -> Pnt` | renamed, curried, reordered. New argument order: distPrev distNext prev next pntToOffset. |
| `Tria3D.tryIntersectLine` | `line: Line3D * p1: Pnt * p2: Pnt * p3: Pnt -> Pnt voption` | `Tria3D.tryIntersectLine` | `p1: Pnt -> p2: Pnt -> p3: Pnt -> line: Line3D -> Pnt voption` | curried, reordered. New argument order: p1 p2 p3 line. |
| `Tria3D.tryIntersectRay` | `line: Line3D * p1: Pnt * p2: Pnt * p3: Pnt -> float voption` | `Tria3D.tryIntersectRay` | `p1: Pnt -> p2: Pnt -> p3: Pnt -> line: Line3D -> float voption` | curried, reordered. New argument order: p1 p2 p3 line. |
| `Tria3D.tryOffsetPntVar` | `pntToOffset: Pnt * prev: Pnt * next: Pnt * distPrev: float * distNext: float -> Pnt voption` | `Tria3D.tryOffsetPntVariable` | `distPrev: float -> distNext: float -> prev: Pnt -> next: Pnt -> pntToOffset: Pnt -> Pnt voption` | renamed, curried, reordered. New argument order: distPrev distNext prev next pntToOffset. |

### UnitVc

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `UnitVc.Xaxis` | `unit -> UnitVc` | `UnitVc.xAxis` | `unit -> UnitVc` | renamed |
| `UnitVc.Yaxis` | `unit -> UnitVc` | `UnitVc.yAxis` | `unit -> UnitVc` | renamed |
| `UnitVc.angle2Pi` | `a: UnitVc * b: UnitVc -> float` | `UnitVc.angle2Pi` | `a: UnitVc -> b: UnitVc -> float` | curried |
| `UnitVc.angle360` | `a: UnitVc * b: UnitVc -> float` | `UnitVc.angle360` | `a: UnitVc -> b: UnitVc -> float` | curried |
| `UnitVc.angleDiamond` | `a: UnitVc * b: UnitVc -> float` | `UnitVc.angleDiamond` | `a: UnitVc -> b: UnitVc -> float` | curried |
| `UnitVc.create` | `x: float * y: float -> UnitVc` | `UnitVc.create` | `x: float -> y: float -> UnitVc` | curried |
| `UnitVc.create` | `fromPt: Pt * toPt: Pt -> UnitVc` | `UnitVc.createFromTo` | `fromPt: Pt -> toPt: Pt -> UnitVc` | renamed, curried |
| `UnitVc.createFor` | `msg: string * x: float * y: float -> UnitVc` | `UnitVc.createWithErrorMessage` | `msg: string -> x: float -> y: float -> UnitVc` | renamed, curried |
| `UnitVc.dot` | `a: UnitVc * b: UnitVc -> float` | `UnitVc.dot` | `a: UnitVc -> b: UnitVc -> float` | curried |
| `UnitVc.dot` | `a: Vc * b: UnitVc -> float` | `Vc.dotUnitVc` | `a: Vc -> b: UnitVc -> float` | renamed, curried |
| `UnitVc.dot` | `a: UnitVc * b: Vc -> float` | `UnitVc.dotVc` | `a: UnitVc -> b: Vc -> float` | renamed, curried |
| `UnitVc.flip` | `v: UnitVc -> UnitVc` | `UnitVc.reverse` | `v: UnitVc -> UnitVc` | renamed |
| `UnitVc.lerp` | `start: UnitVc * endValue: UnitVc * rel: float -> Vc` | `UnitVc.lerp` | `start: UnitVc -> endValue: UnitVc -> rel: float -> Vc` | curried |
| `UnitVc.slerp` | `start: UnitVc * endValue: UnitVc * rel: float -> UnitVc` | `UnitVc.slerp` | `start: UnitVc -> endValue: UnitVc -> rel: float -> UnitVc` | curried |

### UnitVec

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `.LengthSqInXY` | `unit -> float` | `.LengthInXYSq` | `unit -> float` | renamed |
| `.RotateOnZ90CCW` | `unit -> UnitVec` | `.Rotate2Dby90CCW` | `unit -> UnitVec` | renamed |
| `.RotateOnZ90CW` | `unit -> UnitVec` | `.Rotate2Dby90CW` | `unit -> UnitVec` | renamed |
| `UnitVec.Xaxis` | `unit -> UnitVec` | `UnitVec.xAxis` | `unit -> UnitVec` | renamed |
| `UnitVec.Yaxis` | `unit -> UnitVec` | `UnitVec.yAxis` | `unit -> UnitVec` | renamed |
| `UnitVec.Zaxis` | `unit -> UnitVec` | `UnitVec.zAxis` | `unit -> UnitVec` | renamed |
| `UnitVec.angle2PiInXY` | `a: UnitVec * b: UnitVec -> float` | `UnitVec.angle2PiInXY` | `a: UnitVec -> b: UnitVec -> float` | curried |
| `UnitVec.angle360InXY` | `a: UnitVec * b: UnitVec -> float` | `UnitVec.angle360InXY` | `a: UnitVec -> b: UnitVec -> float` | curried |
| `UnitVec.angleDiamondInXY` | `a: UnitVec * b: UnitVec -> float` | `UnitVec.angleDiamondInXY` | `a: UnitVec -> b: UnitVec -> float` | curried |
| `UnitVec.create` | `x: float * y: float * z: float -> UnitVec` | `UnitVec.create` | `x: float -> y: float -> z: float -> UnitVec` | curried |
| `UnitVec.create` | `fromPnt: Pnt * toPnt: Pnt -> UnitVec` | `UnitVec.createFromTo` | `fromPnt: Pnt -> toPnt: Pnt -> UnitVec` | renamed, curried |
| `UnitVec.createFor` | `msg: string * x: float * y: float * z: float -> UnitVec` | `UnitVec.createWithErrorMessage` | `msg: string -> x: float -> y: float -> z: float -> UnitVec` | renamed, curried |
| `UnitVec.dot` | `a: UnitVec * b: UnitVec -> float` | `UnitVec.dot` | `a: UnitVec -> b: UnitVec -> float` | curried |
| `UnitVec.dot` | `a: Vec * b: UnitVec -> float` | `Vec.dotUnitVec` | `a: Vec -> b: UnitVec -> float` | renamed, curried |
| `UnitVec.dot` | `a: UnitVec * b: Vec -> float` | `UnitVec.dotVec` | `a: UnitVec -> b: Vec -> float` | renamed, curried |
| `UnitVec.flip` | `v: UnitVec -> UnitVec` | `UnitVec.reverse` | `v: UnitVec -> UnitVec` | renamed |
| `UnitVec.lengthSqInXY` | `v: UnitVec -> float` | `UnitVec.lengthInXYSq` | `v: UnitVec -> float` | renamed |
| `UnitVec.lerp` | `start: UnitVec * endValue: UnitVec * rel: float -> Vec` | `UnitVec.lerp` | `start: UnitVec -> endValue: UnitVec -> rel: float -> Vec` | curried |
| `UnitVec.rotateOnXDeg` | `angDegree: float -> v: UnitVec -> UnitVec` | `UnitVec.rotateOnXDegrees` | `angDegree: float -> v: UnitVec -> UnitVec` | renamed |
| `UnitVec.rotateOnYDeg` | `angDegree: float -> v: UnitVec -> UnitVec` | `UnitVec.rotateOnYDegrees` | `angDegree: float -> v: UnitVec -> UnitVec` | renamed |
| `UnitVec.rotateOnZ90CCW` | `v: UnitVec -> UnitVec` | `UnitVec.rotate2Dby90CCW` | `v: UnitVec -> UnitVec` | renamed |
| `UnitVec.rotateOnZ90CW` | `v: UnitVec -> UnitVec` | `UnitVec.rotate2Dby90CW` | `v: UnitVec -> UnitVec` | renamed |
| `UnitVec.rotateOnZDeg` | `angDegree: float -> v: UnitVec -> UnitVec` | `UnitVec.rotateOnZDegrees` | `angDegree: float -> v: UnitVec -> UnitVec` | renamed |
| `UnitVec.slerp` | `start: UnitVec * endValue: UnitVec * rel: float -> UnitVec` | `UnitVec.slerp` | `start: UnitVec -> endValue: UnitVec -> rel: float -> UnitVec` | curried |

### Vc

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `Vc.Xaxis` | `unit -> Vc` | `Vc.xAxis` | `unit -> Vc` | renamed |
| `Vc.Yaxis` | `unit -> Vc` | `Vc.yAxis` | `unit -> Vc` | renamed |
| `Vc.angle2Pi` | `a: Vc * b: Vc -> float` | `Vc.angle2Pi` | `a: Vc -> b: Vc -> float` | curried |
| `Vc.angle360` | `a: Vc * b: Vc -> float` | `Vc.angle360` | `a: Vc -> b: Vc -> float` | curried |
| `Vc.angleDiamond` | `a: Vc * b: Vc -> float` | `Vc.angleDiamond` | `a: Vc -> b: Vc -> float` | curried |
| `Vc.create` | `x: float * y: float -> Vc` | `Vc.create` | `x: float -> y: float -> Vc` | curried |
| `Vc.create` | `start: Pt * endValue: Pt -> Vc` | `Vc.createFromTo` | `start: Pt -> endValue: Pt -> Vc` | renamed, curried |
| `Vc.dot` | `a: Vc * b: Vc -> float` | `Vc.dot` | `a: Vc -> b: Vc -> float` | curried |
| `Vc.dot` | `a: Vc * b: UnitVc -> float` | `Vc.dotUnitVc` | `a: Vc -> b: UnitVc -> float` | renamed, curried |
| `Vc.dot` | `a: UnitVc * b: Vc -> float` | `UnitVc.dotVc` | `a: UnitVc -> b: Vc -> float` | renamed, curried |
| `Vc.flip` | `v: Vc -> Vc` | `Vc.reverse` | `v: Vc -> Vc` | renamed |
| `Vc.lerp` | `start: Vc * endValue: Vc * rel: float -> Vc` | `Vc.lerp` | `start: Vc -> endValue: Vc -> rel: float -> Vc` | curried |
| `Vc.slerp` | `start: Vc * endValue: Vc * rel: float -> Vc` | `Vc.slerp` | `start: Vc -> endValue: Vc -> rel: float -> Vc` | curried |
| `Vc.unitizeWithErrMsg` | `errMsg: string -> v: Vc -> UnitVc` | `Vc.unitizeWithErrorMessage` | `errMsg: string -> v: Vc -> UnitVc` | renamed |

### Vec

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `.LengthSqInXY` | `unit -> float` | `.LengthInXYSq` | `unit -> float` | renamed |
| `.RotateOnZ90CCW` | `unit -> Vec` | `.Rotate2Dby90CCW` | `unit -> Vec` | renamed |
| `.RotateOnZ90CW` | `unit -> Vec` | `.Rotate2Dby90CW` | `unit -> Vec` | renamed |
| `Vec.Xaxis` | `unit -> Vec` | `Vec.xAxis` | `unit -> Vec` | renamed |
| `Vec.Yaxis` | `unit -> Vec` | `Vec.yAxis` | `unit -> Vec` | renamed |
| `Vec.Zaxis` | `unit -> Vec` | `Vec.zAxis` | `unit -> Vec` | renamed |
| `Vec.angle2PiInXY` | `a: Vec * b: Vec -> float` | `Vec.angle2PiInXY` | `a: Vec -> b: Vec -> float` | curried |
| `Vec.angle360InXY` | `a: Vec * b: Vec -> float` | `Vec.angle360InXY` | `a: Vec -> b: Vec -> float` | curried |
| `Vec.angleDiamondInXY` | `a: Vec * b: Vec -> float` | `Vec.angleDiamondInXY` | `a: Vec -> b: Vec -> float` | curried |
| `Vec.create` | `x: float * y: float * z: float -> Vec` | `Vec.create` | `x: float -> y: float -> z: float -> Vec` | curried |
| `Vec.create` | `start: Pnt * endValue: Pnt -> Vec` | `Vec.createFromTo` | `start: Pnt -> endValue: Pnt -> Vec` | renamed, curried |
| `Vec.dot` | `a: Vec * b: Vec -> float` | `Vec.dot` | `a: Vec -> b: Vec -> float` | curried |
| `Vec.dot` | `a: Vec * b: UnitVec -> float` | `Vec.dotUnitVec` | `a: Vec -> b: UnitVec -> float` | renamed, curried |
| `Vec.dot` | `a: UnitVec * b: Vec -> float` | `UnitVec.dotVec` | `a: UnitVec -> b: Vec -> float` | renamed, curried |
| `Vec.flip` | `v: Vec -> Vec` | `Vec.reverse` | `v: Vec -> Vec` | renamed |
| `Vec.lengthSqInXY` | `v: Vec -> float` | `Vec.lengthInXYSq` | `v: Vec -> float` | renamed |
| `Vec.lerp` | `start: Vec * endValue: Vec * rel: float -> Vec` | `Vec.lerp` | `start: Vec -> endValue: Vec -> rel: float -> Vec` | curried |
| `Vec.rotateOnXDeg` | `angDegree: float -> v: Vec -> Vec` | `Vec.rotateOnXDegrees` | `angDegree: float -> v: Vec -> Vec` | renamed |
| `Vec.rotateOnYDeg` | `angDegree: float -> v: Vec -> Vec` | `Vec.rotateOnYDegrees` | `angDegree: float -> v: Vec -> Vec` | renamed |
| `Vec.rotateOnZ90CCW` | `v: Vec -> Vec` | `Vec.rotate2Dby90CCW` | `v: Vec -> Vec` | renamed |
| `Vec.rotateOnZ90CW` | `v: Vec -> Vec` | `Vec.rotate2Dby90CW` | `v: Vec -> Vec` | renamed |
| `Vec.rotateOnZDeg` | `angDegree: float -> v: Vec -> Vec` | `Vec.rotateOnZDegrees` | `angDegree: float -> v: Vec -> Vec` | renamed |
| `Vec.slerp` | `start: Vec * endValue: Vec * rel: float -> Vec` | `Vec.slerp` | `start: Vec -> endValue: Vec -> rel: float -> Vec` | curried |
| `Vec.unitizeWithErrMsg` | `errMsg: string -> v: Vec -> UnitVec` | `Vec.unitizeWithErrorMessage` | `errMsg: string -> v: Vec -> UnitVec` | renamed |

### XLine2D

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `XLine2D.classifyClosestParameters` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` | `XLine2D.classifyClosestParameters` | `lineA: Line2D -> lineB: Line2D -> ClParams` | curried |
| `XLine2D.classifyClosestParameters` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` | `XLine2D.classifyClosestParametersFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> ClParams` | renamed, curried |
| `XLine2D.classifyClosestParameters` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` | `XLine2D.classifyClosestParametersCustom` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine2D.classifyClosestParameters` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` | `XLine2D.classifyClosestParametersCustom` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine2D.classifyClosestPoints` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPts` | `XLine2D.classifyClosestPoints` | `lineA: Line2D -> lineB: Line2D -> ClPts` | curried |
| `XLine2D.classifyClosestPoints` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPts` | `XLine2D.classifyClosestPointsFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> ClPts` | renamed, curried |
| `XLine2D.classifyClosestPoints` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPts` | `XLine2D.classifyClosestPointsCustom` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPts` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine2D.classifyClosestPoints` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPts` | `XLine2D.classifyClosestPointsCustom` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPts` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine2D.classifyEndsTouching` | `a: Line2D * b: Line2D * ?tolerance: float -> XEnds` | `XLine2D.classifyEndsTouching` | `a: Line2D -> b: Line2D -> XEnds` | curried |
| `XLine2D.classifyEndsTouching` | `a: Line2D * b: Line2D * ?tolerance: float -> XEnds` | `XLine2D.classifyEndsTouchingCustom` | `a: Line2D * b: Line2D * ?tolerance: float -> XEnds` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine2D.classifyIntersection` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPt` | `XLine2D.classifyIntersection` | `lineA: Line2D -> lineB: Line2D -> XPt` | curried |
| `XLine2D.classifyIntersection` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPt` | `XLine2D.classifyIntersectionFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> XPt` | renamed, curried |
| `XLine2D.classifyIntersection` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPt` | `XLine2D.classifyIntersectionCustom` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPt` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine2D.classifyIntersection` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPt` | `XLine2D.classifyIntersectionCustom` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPt` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine2D.classifyIntersectionParam` | `lnA: Line2D * lnB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` | `XLine2D.classifyIntersectionParam` | `lnA: Line2D -> lnB: Line2D -> XParam` | curried |
| `XLine2D.classifyIntersectionParam` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` | `XLine2D.classifyIntersectionParamFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> XParam` | renamed, curried |
| `XLine2D.classifyIntersectionParam` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` | `XLine2D.classifyIntersectionParamCustom` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine2D.classifyIntersectionParam` | `lnA: Line2D * lnB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` | `XLine2D.classifyIntersectionParamCustom` | `lnA: Line2D * lnB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine2D.classifyRayIntersection` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` | `XLine2D.classifyRayIntersection` | `lineA: Line2D -> lineB: Line2D -> XRay` | curried |
| `XLine2D.classifyRayIntersection` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` | `XLine2D.classifyRayIntersectionFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> XRay` | renamed, curried |
| `XLine2D.classifyRayIntersection` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` | `XLine2D.classifyRayIntersectionCustom` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine2D.classifyRayIntersection` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` | `XLine2D.classifyRayIntersectionCustom` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine2D.classifyRayIntersectionParam` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` | `XLine2D.classifyRayIntersectionParam` | `lineA: Line2D -> lineB: Line2D -> XRayParam` | curried |
| `XLine2D.classifyRayIntersectionParam` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` | `XLine2D.classifyRayIntersectionParamFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> XRayParam` | renamed, curried |
| `XLine2D.classifyRayIntersectionParam` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` | `XLine2D.classifyRayIntersectionParamCustom` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine2D.classifyRayIntersectionParam` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` | `XLine2D.classifyRayIntersectionParamCustom` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine2D.closestParameters` | `lineA: Line2D * lineB: Line2D -> struct (float * float)` | `XLine2D.closestParameters` | `lineA: Line2D -> lineB: Line2D -> struct (float * float)` | curried |
| `XLine2D.closestParameters` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> struct (float * float)` | `XLine2D.closestParametersFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> struct (float * float)` | renamed, curried |
| `XLine2D.doIntersect` | `lineA: Line2D * lineB: Line2D -> bool` | `XLine2D.doIntersect` | `lineA: Line2D -> lineB: Line2D -> bool` | curried |
| `XLine2D.doIntersect` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> bool` | `XLine2D.doIntersectFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> bool` | renamed, curried |
| `XLine2D.doOverlap` | `lineA: Line2D * lineB: Line2D * ?tolerance: float -> bool` | `XLine2D.doOverlap` | `lineA: Line2D -> lineB: Line2D -> bool` | curried |
| `XLine2D.doOverlap` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tolerance: float -> bool` | `XLine2D.doOverlapFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> bool` | renamed, curried |
| `XLine2D.doOverlap` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tolerance: float -> bool` | `XLine2D.doOverlapCustom` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tolerance: float -> bool` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine2D.doOverlap` | `lineA: Line2D * lineB: Line2D * ?tolerance: float -> bool` | `XLine2D.doOverlapCustom` | `lineA: Line2D * lineB: Line2D * ?tolerance: float -> bool` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine2D.getSqDistance` | `lineA: Line2D * lineB: Line2D -> float` | `XLine2D.distanceSq` | `lineA: Line2D -> lineB: Line2D -> float` | renamed, curried |
| `XLine2D.getSqDistance` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> float` | `XLine2D.distanceSqFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> float` | renamed, curried |
| `XLine2D.intersectRays` | `lineA: Line2D * lineB: Line2D -> Pt` | `XLine2D.intersectRays` | `lineA: Line2D -> lineB: Line2D -> Pt` | curried |
| `XLine2D.intersectRays` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> Pt` | `XLine2D.intersectRaysFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> Pt` | renamed, curried |
| `XLine2D.isWithinRanges` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> bool` | `XLine2D.isWithinRangesFromPtsAndVcs` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> bool` | renamed |
| `XLine2D.parameterA` | `lineA: Line2D * lineB: Line2D -> float` | `XLine2D.parameterA` | `lineA: Line2D -> lineB: Line2D -> float` | curried |
| `XLine2D.parameterA` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> float` | `XLine2D.parameterAFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> float` | renamed, curried |
| `XLine2D.parameters` | `lineA: Line2D * lineB: Line2D -> float * float` | `XLine2D.parameters` | `lineA: Line2D -> lineB: Line2D -> float * float` | curried |
| `XLine2D.parameters` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> float * float` | `XLine2D.parametersFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> float * float` | renamed, curried |
| `XLine2D.tryIntersect` | `lineA: Line2D * lineB: Line2D -> Pt voption` | `XLine2D.tryIntersect` | `lineA: Line2D -> lineB: Line2D -> Pt voption` | curried |
| `XLine2D.tryIntersect` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> Pt voption` | `XLine2D.tryIntersectFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> Pt voption` | renamed, curried |
| `XLine2D.tryIntersectInRangeA` | `lineA: Line2D * lineB: Line2D * minParameterA: float * maxParameterA: float -> Pt voption` | `XLine2D.tryIntersectInRangeA` | `minParameterA: float -> maxParameterA: float -> lineA: Line2D -> lineB: Line2D -> Pt voption` | curried, reordered. New argument order: minParameterA maxParameterA lineA lineB. |
| `XLine2D.tryIntersectInRangeA` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * minParameterA: float * maxParameterA: float -> Pt voption` | `XLine2D.tryIntersectInRangeAFromPtsAndVcs` | `minParameterA: float -> maxParameterA: float -> pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> Pt voption` | renamed, curried, reordered. New argument order: minParameterA maxParameterA pA pB vA vB. |
| `XLine2D.tryIntersectInRanges` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> Pt voption` | `XLine2D.tryIntersectInRangesFromPtsAndVcs` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * minParameterA: float * maxParameterA: float * minParameterB: float * maxParameterB: float -> Pt voption` | renamed |
| `XLine2D.tryIntersectRay` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> -> Pt voption` | `XLine2D.tryIntersectRay` | `lineA: Line2D -> lineB: Line2D -> Pt voption` | curried |
| `XLine2D.tryIntersectRay` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> -> Pt voption` | `XLine2D.tryIntersectRayFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> Pt voption` | renamed, curried |
| `XLine2D.tryIntersectRay` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> -> Pt voption` | `XLine2D.tryIntersectRayCustom` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc * ?tangent: float<tangent> -> Pt voption` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine2D.tryIntersectRay` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> -> Pt voption` | `XLine2D.tryIntersectRayCustom` | `lineA: Line2D * lineB: Line2D * ?tangent: float<tangent> -> Pt voption` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine2D.tryParameterA` | `lineA: Line2D * lineB: Line2D -> float voption` | `XLine2D.tryParameterA` | `lineA: Line2D -> lineB: Line2D -> float voption` | curried |
| `XLine2D.tryParameterA` | `pA: Pt * pB: Pt * vA: Vc * vB: Vc -> float voption` | `XLine2D.tryParameterAFromPtsAndVcs` | `pA: Pt -> pB: Pt -> vA: Vc -> vB: Vc -> float voption` | renamed, curried |

### XLine3D

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `XLine3D.classifyClosestParameters` | `lineA: Line3D * lineB: Line3D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` | `XLine3D.classifyClosestParameters` | `lineA: Line3D -> lineB: Line3D -> ClParams` | curried |
| `XLine3D.classifyClosestParameters` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` | `XLine3D.classifyClosestParametersFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> ClParams` | renamed, curried |
| `XLine3D.classifyClosestParameters` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` | `XLine3D.classifyClosestParametersCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.classifyClosestParameters` | `lineA: Line3D * lineB: Line3D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` | `XLine3D.classifyClosestParametersCustom` | `lineA: Line3D * lineB: Line3D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClParams` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.classifyClosestPoints` | `lineA: Line3D * lineB: Line3D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPnts` | `XLine3D.classifyClosestPoints` | `lineA: Line3D -> lineB: Line3D -> ClPnts` | curried |
| `XLine3D.classifyClosestPoints` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPnts` | `XLine3D.classifyClosestPointsFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> ClPnts` | renamed, curried |
| `XLine3D.classifyClosestPoints` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPnts` | `XLine3D.classifyClosestPointsCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPnts` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.classifyClosestPoints` | `lineA: Line3D * lineB: Line3D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPnts` | `XLine3D.classifyClosestPointsCustom` | `lineA: Line3D * lineB: Line3D * ?tangent: float<tangent> * ?tooShortTolerance: float -> ClPnts` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.classifyConeIntersection` | `ray: Line3D * coneRadius: float * coneBaseZ: float * coneTipZ: float -> XCone` | `XLine3D.classifyConeIntersection` | `coneRadius: float -> coneBaseZ: float -> coneTipZ: float -> ray: Line3D -> XCone` | curried, reordered. New argument order: coneRadius coneBaseZ coneTipZ ray. |
| `XLine3D.classifyEndsTouching` | `a: Line3D * b: Line3D * ?tolerance: float -> XEnds` | `XLine3D.classifyEndsTouching` | `a: Line3D -> b: Line3D -> XEnds` | curried |
| `XLine3D.classifyEndsTouching` | `a: Line3D * b: Line3D * ?tolerance: float -> XEnds` | `XLine3D.classifyEndsTouchingCustom` | `a: Line3D * b: Line3D * ?tolerance: float -> XEnds` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.classifyIntersection` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPnt` | `XLine3D.classifyIntersection` | `lineA: Line3D -> lineB: Line3D -> XPnt` | curried |
| `XLine3D.classifyIntersection` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPnt` | `XLine3D.classifyIntersectionFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> XPnt` | renamed, curried |
| `XLine3D.classifyIntersection` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPnt` | `XLine3D.classifyIntersectionCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPnt` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.classifyIntersection` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPnt` | `XLine3D.classifyIntersectionCustom` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XPnt` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.classifyIntersectionParam` | `lnA: Line3D * lnB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` | `XLine3D.classifyIntersectionParam` | `lnA: Line3D -> lnB: Line3D -> XParam` | curried |
| `XLine3D.classifyIntersectionParam` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` | `XLine3D.classifyIntersectionParamFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> XParam` | renamed, curried |
| `XLine3D.classifyIntersectionParam` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` | `XLine3D.classifyIntersectionParamCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.classifyIntersectionParam` | `lnA: Line3D * lnB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` | `XLine3D.classifyIntersectionParamCustom` | `lnA: Line3D * lnB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XParam` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.classifyRayClosestParam` | `lineA: Line3D * lineB: Line3D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` | `XLine3D.classifyRayClosestParam` | `lineA: Line3D -> lineB: Line3D -> XRayParam` | curried |
| `XLine3D.classifyRayClosestParam` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` | `XLine3D.classifyRayClosestParamFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> XRayParam` | renamed, curried |
| `XLine3D.classifyRayClosestParam` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` | `XLine3D.classifyRayClosestParamCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.classifyRayClosestParam` | `lineA: Line3D * lineB: Line3D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` | `XLine3D.classifyRayClosestParamCustom` | `lineA: Line3D * lineB: Line3D * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRayParam` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.classifyRayIntersection` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` | `XLine3D.classifyRayIntersection` | `lineA: Line3D -> lineB: Line3D -> XRay` | curried |
| `XLine3D.classifyRayIntersection` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` | `XLine3D.classifyRayIntersectionFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> XRay` | renamed, curried |
| `XLine3D.classifyRayIntersection` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` | `XLine3D.classifyRayIntersectionCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.classifyRayIntersection` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` | `XLine3D.classifyRayIntersectionCustom` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> XRay` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.doIntersect` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> bool` | `XLine3D.doIntersect` | `lineA: Line3D -> lineB: Line3D -> bool` | curried |
| `XLine3D.doIntersect` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> bool` | `XLine3D.doIntersectFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> bool` | renamed, curried |
| `XLine3D.doIntersect` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> bool` | `XLine3D.doIntersectCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> bool` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.doIntersect` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> bool` | `XLine3D.doIntersectCustom` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> bool` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.doOverlap` | `lineA: Line3D * lineB: Line3D * ?tolerance: float -> bool` | `XLine3D.doOverlap` | `lineA: Line3D -> lineB: Line3D -> bool` | curried |
| `XLine3D.doOverlap` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tolerance: float -> bool` | `XLine3D.doOverlapFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> bool` | renamed, curried |
| `XLine3D.doOverlap` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tolerance: float -> bool` | `XLine3D.doOverlapCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?tolerance: float -> bool` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.doOverlap` | `lineA: Line3D * lineB: Line3D * ?tolerance: float -> bool` | `XLine3D.doOverlapCustom` | `lineA: Line3D * lineB: Line3D * ?tolerance: float -> bool` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.doRaysIntersect` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float -> bool` | `XLine3D.doRaysIntersect` | `lineA: Line3D -> lineB: Line3D -> bool` | curried |
| `XLine3D.doRaysIntersect` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float -> bool` | `XLine3D.doRaysIntersectFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> bool` | renamed, curried |
| `XLine3D.doRaysIntersect` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float -> bool` | `XLine3D.doRaysIntersectCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float -> bool` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.doRaysIntersect` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float -> bool` | `XLine3D.doRaysIntersectCustom` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float -> bool` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.getSqDistance` | `lineA: Line3D * lineB: Line3D -> float` | `XLine3D.distanceSq` | `lineA: Line3D -> lineB: Line3D -> float` | renamed, curried |
| `XLine3D.getSqDistance` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec -> float` | `XLine3D.distanceSqFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> float` | renamed, curried |
| `XLine3D.parameterA` | `lineA: Line3D * lineB: Line3D -> float` | `XLine3D.parameterA` | `lineA: Line3D -> lineB: Line3D -> float` | curried |
| `XLine3D.parameterA` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec -> float` | `XLine3D.parameterAFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> float` | renamed, curried |
| `XLine3D.parameters` | `lineA: Line3D * lineB: Line3D -> float * float` | `XLine3D.parameters` | `lineA: Line3D -> lineB: Line3D -> float * float` | curried |
| `XLine3D.parameters` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec -> float * float` | `XLine3D.parametersFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> float * float` | renamed, curried |
| `XLine3D.tryClosestParameterRayA` | `lineA: Line3D * lineB: Line3D -> float voption` | `XLine3D.tryClosestParameterRayA` | `lineA: Line3D -> lineB: Line3D -> float voption` | curried |
| `XLine3D.tryClosestParameterRayA` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec -> float voption` | `XLine3D.tryClosestParameterRayAFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> float voption` | renamed, curried |
| `XLine3D.tryClosestPntRayA` | `lineA: Line3D * lineB: Line3D -> Pnt voption` | `XLine3D.tryClosestPntRayA` | `lineA: Line3D -> lineB: Line3D -> Pnt voption` | curried |
| `XLine3D.tryClosestPntRayA` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec -> Pnt voption` | `XLine3D.tryClosestPntRayAFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> Pnt voption` | renamed, curried |
| `XLine3D.tryIntersect` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` | `XLine3D.tryIntersect` | `lineA: Line3D -> lineB: Line3D -> Pnt voption` | curried |
| `XLine3D.tryIntersect` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` | `XLine3D.tryIntersectFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> Pnt voption` | renamed, curried |
| `XLine3D.tryIntersect` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` | `XLine3D.tryIntersectCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.tryIntersect` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` | `XLine3D.tryIntersectCustom` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.tryIntersectRay` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` | `XLine3D.tryIntersectRay` | `lineA: Line3D -> lineB: Line3D -> Pnt voption` | curried |
| `XLine3D.tryIntersectRay` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` | `XLine3D.tryIntersectRayFromPntsAndVecs` | `pA: Pnt -> pB: Pnt -> vA: Vec -> vB: Vec -> Pnt voption` | renamed, curried |
| `XLine3D.tryIntersectRay` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` | `XLine3D.tryIntersectRayCustom` | `pA: Pnt * pB: Pnt * vA: Vec * vB: Vec * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |
| `XLine3D.tryIntersectRay` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` | `XLine3D.tryIntersectRayCustom` | `lineA: Line3D * lineB: Line3D * ?maxSkewDistance: float * ?tangent: float<tangent> * ?tooShortTolerance: float -> Pnt voption` | renamed. Use this tupled form to pass the optional parameters, by name or by position. |

### XLineXY

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `XLineXY.sqDistance` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> float` | `XLineXY.distanceSq` | `pAx: float * pAy: float * pBx: float * pBy: float * vAx: float * vAy: float * vBx: float * vBy: float -> float` | renamed |

### XLineXYZ

| Old | Old signature | New | New signature | Change |
|:--|:--|:--|:--|:--|
| `XLineXYZ.sqDistance` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> float` | `XLineXYZ.distanceSq` | `pAx: float * pAy: float * pAz: float * pBx: float * pBy: float * pBz: float * vAx: float * vAy: float * vAz: float * vBx: float * vBy: float * vBz: float -> float` | renamed |
