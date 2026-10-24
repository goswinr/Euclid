# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- Added `Box.offset distBottom distFront distRight distBack distLeft distTop`, which offsets each of the six faces of a `Box` by its own distance, in the face order of `Box.Faces`. Positive distances offset inwards and negative distances offset outwards, like `Rect3D.offsetVar`. It fails if the box would get inverted.
- Added `FreeBox.XYZs` (and the static `getXYZs`) to access the flat array of 24 floats a `FreeBox` is now made of, `FreeBox.AsPoints` (and the static `asPoints`) to convert it to the 8 corner points, `FreeBox.createDirectly` to build one from an existing array of 24 floats, `FreeBox.GetX/GetY/GetZ` to read single coordinates, `FreeBox.SetPntXYZ` (and the static `setPntXYZ`) to set a corner from x, y and z, and `FreeBox.Duplicate` to copy one.
- Added the settable single coordinate properties `FreeBox.Pnt0X`, `FreeBox.Pnt0Y`, `FreeBox.Pnt0Z` ... up to `FreeBox.Pnt7Z`, one for each of the 24 floats of a `FreeBox`, plus the static getters `FreeBox.pnt0X` ... `FreeBox.pnt7Z`.
- Added `Box.GetLine`, `BBox.GetLine` and `FreeBox.GetLine` (and the static `getLine`), which return the `Line3D` between any two different corner points by their indices 0 to 7. Besides the 12 edges this also gives face and space diagonals.
- Added members that move a single point in place by its index: `Polyline2D.MovePt` (by a `Vc`) and `Polyline2D.MovePtXY` (by x and y), `Polyline3D.MovePnt` (by a `Vec`) and `Polyline3D.MovePntXYZ` (by x, y and z), and `FreeBox.MovePnt` and `FreeBox.MovePntXYZ`, plus the static `movePt`, `movePtXY`, `movePnt` and `movePntXYZ`, which take the point index first. Like `SetPtXY` and `SetPntXYZ`, moving the first or last point of a closed polyline opens it. To keep it closed, use `MovePtClosed`, `MovePtXYClosed`, `MovePntClosed` and `MovePntXYZClosed` (and the static `movePtClosed`, `movePtXYClosed`, `movePntClosed` and `movePntXYZClosed`), which move both the first and the last point.
- Added `Matrix.Then`, `RigidMatrix.Then` and `Quaternion.Then`, which combine two transformations with the same application order everywhere: `a.Then b` first does `a` and then `b`. `Matrix.multiply` and `RigidMatrix.multiply` (and `***`) already worked this way, but `Quaternion.multiply` (and `***`) is the Hamilton product and applies the right operand first, so `q1 *** q2` equals `q2.Then q1`. Their behavior is unchanged, but their docs now state the application order. Be aware that `***` is right-associative in F#, so a chain like `p *** q1 *** q2` is `p *** (q1 *** q2)`: this applies `q2` first for quaternions, but `m1` first for matrices. Use `p *** q1.Then q2`, or `(p *** q1) *** q2`, to be explicit.
- Added the missing members of the rotation and scale grammar (see Changed). `Line2D`, `Polyline2D` and `Rect2D` now have `rotateDegrees` and `rotateRadians`. `Pt`, `Line2D`, `Polyline2D` and `Rect2D` now have `rotateAroundDegrees` and `rotateAroundRadians`. Each comes with its instance member, e.g. `RotateDegrees` and `RotateAroundDegrees`. `Pt` also gets the instance members `RotateAround`, `RotateAroundDegrees` and `RotateAroundRadians`. `Pt.scaleOn` and `Pnt.scaleOn` scale a point around a center point.
- Added in-place instance members for the clean up functions of `Polyline2D` and `Polyline3D`: `RemoveDuplicatePoints`, `RemoveDuplicatePointsAndReport`, `RemoveDuplicatePointsFaithfully`, `RemoveDuplicateAndCollinearPoints`, `RemoveUTurns` and `RemoveUTurnsDeeply` on both, plus `EnsureClockwise` and `EnsureCounterClockwise` on `Polyline2D`, and `EnsureClockwiseIn2D` and `EnsureCounterClockwiseIn2D` on `Polyline3D`. They change the polyline in place and return `unit`, except `RemoveDuplicatePointsAndReport`, which returns the removed points. If one of them raises an error, the polyline is not changed.
- Added `Vc.tryUnitize` and `Vec.tryUnitize`, which return `ValueNone` instead of failing for vectors shorter than 1e-12 or with NaN components, next to `unitize` (fails) and `unitizeOrDefault` (takes a fallback).
- Added `equalsExact` to `Pt`, `Pnt`, `Vc`, `Vec`, `UnitVc`, `UnitVec`, `Line2D`, `Line3D`, `PPlane`, `NPlane`, `BRect`, `BBox`, `Rect2D`, `Rect3D`, `Box`, `Matrix`, `RigidMatrix`, `Quaternion` and `Rotation2D`. It compares each field with `=`, so a NaN value is never equal.
- Added a throwing member under the old name of every member that was renamed with a `try` prefix (see Changed). For example `PPlane.intersectLine` now returns a `Pnt` and raises an `EuclidException` if the line does not reach the plane, while `PPlane.tryIntersectLine` returns `ValueNone`.
- Documented the failure and tolerance contracts in the Readme: `operation` fails, `tryOperation` returns a `voption`, `classify…` returns a discriminated union, `operationOrDefault` takes a fallback and `operationUnchecked` skips the checks. `equalsExact` compares exactly, and `equalsWithin` applies its tolerance to each coordinate separately, not to the Euclidean distance.
- Added the members that were missing on one side of the 2D/3D symmetry. `Pnt` gets the instance members `Rotate`, `RotateAround`, `RotateOnZ90CCW`, `RotateOnZ90CW` and `RotateByQuarterCircle`, and the static `rotateOnZ90CCW`, `rotateOnZ90CW` and `rotateByQuarterCircle`, like `Pt` and `Vec`. `Vec` and `UnitVec` get the instance member `Rotate`, like `Vc` and `UnitVc`. `Vc.isParallelToUnitVc` matches `Vec.isParallelToUnitVec`. `Rect2D` gets `ClosestParameters` and `ClosestPoint` (and the static `closestParameters` and `closestPoint`), like `Rect3D`.
- Added `BBox.ExpandXYZ` and `BBox.ExpandSafeXYZ`, to match `BRect.ExpandXY` and `BRect.ExpandSafeXY` and the static `BBox.expandXYZ` and `BBox.expandSafeXYZ`.

### Changed
- **Breaking:** Rotations and scaling now share one curried naming grammar, with the geometry last: `rotate rotation geometry`, `rotateDegrees degrees geometry`, `rotateRadians radians geometry`, `rotateAround center rotation geometry`, `rotateAroundDegrees center degrees geometry`, `rotateAroundRadians center radians geometry`, `scale factor geometry` and `scaleOn center factor geometry`. The instance members use the same names in PascalCase, and are all curried. A raw angle argument now always names its unit. As a result, `Pt.rotate`, `Vc.rotate` and `UnitVc.rotate` and their instance member `Rotate` now take a `Rotation2D`, like `Line2D.rotate` already did, instead of an angle in degrees. Use `rotateDegrees` or `RotateDegrees` for the old behavior. Old calls with a float fail to compile. The 3D rotations around the world axes (`rotateOnX`, `rotateOnZDeg`, `rotateOnZWithCenter` and so on) keep their names.
- **Breaking:** `Polyline2D`, `Polyline3D` and `FreeBox` now follow one rule: instance members mutate the object in place and return `unit`, and static functions return a new object and leave the input unchanged. The point setters, like `setPt`, `movePt`, `addPoint` or `FreeBox.setPnt`, are the only static functions that still mutate in place.
- **Breaking:** The transformation instance members of `Polyline2D`, `Polyline3D` and `FreeBox` now mutate the object in place and return `unit`, instead of returning a new object. This covers `Scale`, `ScaleOn`, `Move`, `MoveX`, `MoveY`, `MoveZ`, `Transform`, `TransformRigid`, `Rotate`, `RotateWithCenter`, `RotateOnZ` and `RotateOnZWithCenter`, as far as each type has them. They no longer allocate a new coordinate buffer. Their static counterparts `scale`, `scaleOn`, `move`, `translate`, `moveX`, `moveY`, `moveZ`, `transform`, `transformRigid`, `rotate`, `rotateWithCenter`, `rotateOnZ` and `rotateOnZWithCenter` still return a new object.
- **Breaking:** `Reverse()` of `Polyline2D` and `Polyline3D` now reverses in place and returns `unit`, instead of returning a new polyline. The member `CloseInPlace` is renamed to `Close`. The static `reverse` and `close` still return a new polyline.
- The static `ensureClockwise`, `ensureCounterClockwise`, `ensureClockwiseIn2D`, `ensureCounterClockwiseIn2D`, `removeDuplicatePoints`, `removeDuplicatePointsFaithfully`, `removeDuplicateAndCollinearPoints`, `removeUTurns` and `removeUTurnsDeeply` of `Polyline2D` and `Polyline3D` now always return a new polyline with its own coordinate buffer. Before, some of them returned the input itself, or shared its buffer, when there was nothing to change. The resulting points are bit-identical to before.
- **Breaking:** `Polyline2D.checkForDuplicatePoints` and `Polyline3D.checkForDuplicatePoints` are renamed to `removeDuplicatePointsAndReport`. They now return a tuple of the cleaned new polyline and the removed points as a `ResizeArray`, which is empty if there were none, instead of a `Result`.
- The map functions, `subPolyline` and all offset functions of `Polyline2D` and `Polyline3D` still return a new polyline and leave the input unchanged.
- **Breaking (named arguments only):** Parameters and union fields that hold a 3D point are renamed from `Pt` to `Pnt`, to match the `Pnt` type. For example, `fromPt` and `toPt` are now `fromPnt` and `toPnt` in the `Line3D` constructor, `UnitVec.create`, `Pnt.lerp` and `Pnt.projectedParameter`. `testPt` is now `testPnt`, and `xPt` and `yPt` are now `xPnt` and `yPnt` in `PPlane.createThreePoints`, `Rect3D.createFrom3Points` and `Rect3D.tryCreateFrom3Points`. The fields of `XLine3D.XRay` are now `Intersect of xPnt` and `Skew of closestPntA * closestPntB * squareDist`. Every parameter named `pt` of type `Pnt` is now `pnt`, for example in `Pnt.scale`, `Line3D.withStart`, `NPlane.closestPoint` and `PPlane.distanceToPnt`, and so is the field of `XLine3D.ClPnts.Intersect`. Names of type `Pnt` that start with `pt` now start with `pnt`, e.g. `pntPrev`, `pntThis` and `pntNext` in `Pnt.anglePiPnts` and `Pnt.bisector`, `pnt1` and `pnt2` in `closestOfTwo`, `pntToOffset` in `Tria3D.tryOffsetPntVar`, and the fields `pntA` and `pntB` of `XLine3D.ClPnts`. Only code that passes these arguments by name, or matches these fields by name, needs to change.
- **Breaking:** The corner points of the 3D boxes and rectangles are renamed from `Pt` to `Pnt`, without obsolete aliases: `Pt0`-`Pt7` and the static `pt0`-`pt7` of `Box`, `BBox` and `FreeBox` are now `Pnt0`-`Pnt7` and `pnt0`-`pnt7`, and `Pt0`-`Pt3` and `pt0`-`pt3` of `Rect3D` are now `Pnt0`-`Pnt3` and `pnt0`-`pnt3`. The single coordinate properties `Pt0X` ... `Pt7Z` of `Box` are now `Pnt0X` ... `Pnt7Z`. The 2D corners of `Rect2D` and `BRect` keep their `Pt` names.
- **Breaking:** `XLine3D.ClPts` is renamed to `XLine3D.ClPnts`, without an obsolete alias. `XLine2D.ClPts` keeps its name.
- **Breaking:** all curried static members now follow one argument order: configuration, then operation arguments, then the geometry they act on last, so they work with `|>` and match the order of the instance members. These members put the geometry first and have been reordered:
  - `Polyline2D.closestParameter`, `closestParameterXY`, `closestPoint`, `closestPointIndex`, `distanceTo` and `distanceToXY` now take the point (or its coordinates) first and the polyline last, e.g. `Polyline2D.distanceTo pt pl`, like `Polyline2D.signedDistanceTo` and `Polyline2D.contains` already did. The same applies to `Polyline3D.closestParameter`, `closestParameterXYZ`, `closestPoint`, `closestPointIndex`, `distanceTo` and `distanceToXYZ`.
  - `Line2D.lengthToPtOnLine` and `Line3D.lengthToPntOnLine` now take the point first and the line last, like `Line2D.distanceToPt` and `Line2D.closestParameter`. The obsolete `Line3D.lengthToPtOnLine` keeps the line first.

  Old calls fail to compile.
- **Breaking:** all `FreeBox` members that take more than one argument are now curried, like `FreeBox.SetPnt` already was: `b.ScaleOn cen factor` and `b.RotateWithCenter cen q` instead of `b.ScaleOn(cen, factor)` and `b.RotateWithCenter(cen, q)`. The new `SetPntXYZ`, `MovePnt`, `MovePntXYZ` and `GetLine` are curried too: `b.SetPntXYZ index x y z`, `b.MovePnt index v`, `b.MovePntXYZ index x y z` and `b.GetLine startIndex endIndex`. `FreeBox.createFromFour2DPointsArgs` is curried and now takes `zMin` and `zMax` first, like `FreeBox.createFromFour2DPoints`: `FreeBox.createFromFour2DPointsArgs zMin zMax a b c d`. Old calls fail to compile.
- `FreeBox` now stores its 8 corners as a flat array of 24 interleaved floats (`x0, y0, z0, x1, y1, z1, ... x7, y7, z7`), instead of an array of 8 `Pnt`. This matches how `Polyline2D` and `Polyline3D` store their points, and lets all transformations run on the raw floats without allocating intermediate points. The corner accessors `Pnt0`-`Pnt7`, `GetPnt`, `SetPnt` and all edges work as before. JSON serialization of a `FreeBox` now writes the `XYZs` array of 24 floats instead of a `Points` array of 8 points.
- The point index of `GetX`, `GetY`, `GetPt`, `SetPt`, `SetPtXY` and `SetPtXYClosed` of `Polyline2D`, and of `GetX`, `GetY`, `GetZ`, `GetPnt`, `SetPnt`, `SetPntXYZ` and `SetPntXYZClosed` of `Polyline3D`, is now always checked, not only in DEBUG builds, just like on `FreeBox`. An index out of range now raises an `EuclidException` that names the method, the index and the point count, instead of the generic index error of the underlying `ResizeArray`.
- **Breaking:** Every member with a `try` prefix now returns a `voption` instead of an `option`, so that absent results do not allocate. This changes `Line2D` and `Line3D` `tryProjectOntoLineParam`, `tryProjectOntoLine`, `tryIntersectOrOverlap` and `tryGetOverlap`, `Polyline2D` and `Polyline3D` `tryFind`, `tryFindLast`, `tryFindIndex` and `tryFindLastIndex`, `Polyline2D.tryFindSelfIntersection`, `tryFindSelfIntersectionWithBRect` and `tryFindSelfIntersectionWithPtObjects`, `Polyline3D.TryAverageNormal`, `TryAveragePlane` and `TryPlane` (and their static members), `Rect2D.tryCreateFrom3Points`, `Rect3D.tryCreateFrom3Points`, `Box.tryGetBoxBetween`, `RigidMatrix.tryCreateFromMatrix`, `Quaternion.tryCreateFromMatrix`, `ResizeArr.tryFindIndex` and `ResizeArr.tryFindBack`. Old calls that match on `Some` and `None` fail to compile. Match on `ValueSome` and `ValueNone` instead.
- **Breaking:** Members that returned a `voption` or `option` without a `try` prefix now have one, and return a `voption`. Their old names now return the plain result and raise an `EuclidException` when there is none:
  - `PPlane.intersect`, `intersectRay`, `intersectRayAllParameters` and `intersectLine` are now `PPlane.tryIntersect`, `tryIntersectRay`, `tryIntersectRayAllParameters` and `tryIntersectLine`.
  - `NPlane.intersect`, `intersectRay` and `intersectLine` are now `NPlane.tryIntersect`, `tryIntersectRay` and `tryIntersectLine`. `tryIntersect` and `tryIntersectLine` return a `voption`, where `intersect` and `intersectLine` returned an `option`.
  - `Intersection` and `IntersectRay` of `BBox` and `BRect`, and `IntersectRay` of `Box`, are now `TryIntersection` and `TryIntersectRay`, and so are their static members `tryIntersection` and `tryIntersectRay`.
  - `Rect2D.intersectRay`, `intersectLine` and `clipLine` are now `Rect2D.tryIntersectRay`, `tryIntersectLine` and `tryClipLine`. `Rect3D.intersectRay` and `intersectLine` are now `Rect3D.tryIntersectRay` and `tryIntersectLine`.
  - `Tria2D.offsetPtVar` and `offsetPtVarByNormals` are now `Tria2D.tryOffsetPtVar` and `tryOffsetPtVarByNormals`. `Tria3D.offsetVar` is now `Tria3D.tryOffsetPntVar`, to match `Tria2D.tryOffsetPtVar`. Its throwing member is `Tria3D.offsetPntVar`, and the name `offsetVar` is gone. `Tria3D.intersectRay`, `intersectLine`, `intersectRayXYZ` and `intersectTriaVectorsWithRay` are now `Tria3D.tryIntersectRay`, `tryIntersectLine`, `tryIntersectRayXYZ` and `tryIntersectTriaVectorsWithRay`.
  - `Line3D.RayClosestParameters` and `RayClosestPoints` are now `TryRayClosestParameters` and `TryRayClosestPoints`, and so are their static members. `Line3D.intersectCone` is now `Line3D.tryIntersectCone`. All three returned an `option` before.

  Old calls fail to compile, because the result type changed. Use the `try` name for the old behavior.
- `Box.getBoxBetween` now raises an `EuclidException` instead of a generic `Exception` when the boxes overlap or are too far apart.
- **Breaking:** The `System.Text.Json` converters now read the properties of an object only in the order they write them. JSON with properties in another order, or with extra or missing properties, raises a `JsonException`. Before, the order did not matter, extra properties were skipped, and missing ones became 0.0. JSON written by Euclid reads as before. In return, each converter reads its properties one after the other without any lookup.
- The `System.Text.Json` converters of all struct types, from `Pt` to `Matrix`, no longer allocate on the heap. They used to allocate temporary arrays on every read and write, and a string for each property name on every read. With a `PropertyNamingPolicy`, reads and writes still allocate the converted names, and with `PropertyNameCaseInsensitive`, a read allocates a string for a name whose case differs from the written one. The JSON is unchanged.
- The `System.Text.Json` converters of `FreeBox`, `Polyline2D` and `Polyline3D` now write without allocating, and read their coordinates directly into a buffer of exactly the needed length, without the temporary arrays of a growing list. As a result, these converters ignore `JsonSerializerOptions.NumberHandling`: they always write plain JSON numbers and only read plain JSON numbers, like the converters of the struct types already did.

### Fixed
- `Box.createFromPlaneAndPoints` and `Box.createFromDirsAndPoints` skipped the first point, so the resulting box did not always contain it.
- `Tria3D.offsetVar`, now `tryOffsetPntVar`, returned a wrong point: it moved along the offset normal instead of along the previous segment to reach the intersection of the two offset lines.
- `Polyline2D.equalsTol` and `Polyline3D.equalsTol`, now `equalsWithin`, considered a NaN coordinate equal to any value. Now a NaN coordinate is never equal, like in `Pnt.equalsWithin` and all other `equalsWithin` members. The same instance is still always equal to itself.
- The exceptions of `XLine3D.intersectCone`, now `classifyConeIntersection`, showed `%g` and `%O` placeholders instead of the actual values.

### Removed
- **Breaking:** Removed the in-place variants of `Polyline2D` and `Polyline3D`, since the instance members without the suffix now mutate in place: `ReverseInPlace` (now `Reverse`), `reverseInPlace`, `CloseInPlace` (now `Close`), `closeInPlace`, `Polyline2D.ensureClockwiseInPlace` (now `EnsureClockwise`), `Polyline2D.ensureCounterClockwiseInPlace` (now `EnsureCounterClockwise`), `Polyline3D.ensureClockwiseInPlaceIn2D` (now `EnsureClockwiseIn2D`) and `Polyline3D.ensureCounterClockwiseInPlaceIn2D` (now `EnsureCounterClockwiseIn2D`). Also removed the obsolete `CloseIfOpen` and `Polyline3D.closeIfOpen`.

### Deprecated
- `BBox.Expand(x, y, z)` and `BBox.ExpandSafe(x, y, z)` are obsolete. Use `BBox.ExpandXYZ` and `BBox.ExpandSafeXYZ`, which match `BRect.ExpandXY` and `BRect.ExpandSafeXY`. `BBox.Expand(dist)` and `BBox.ExpandSafe(dist)` keep their names.
- `FreeBox.Points` and the static `FreeBox.points` are obsolete. Since `FreeBox` stores a flat array of 24 floats they do not return the live internal buffer any more, but a copy. Use `FreeBox.AsPoints` for the 8 corner points, or `FreeBox.XYZs` for the live buffer.
- Functions that take or return a 3D point are renamed from `Pt` to `Pnt`, to match the `Pnt` type. The old names still work but are obsolete: `Pnt.midPt` is now `Pnt.midPnt`, `Pnt.anglePiPts` is now `Pnt.anglePiPnts`, `Pnt.angle180Pts` is now `Pnt.angle180Pnts`, `Pnt.normalOf3Pts` is now `Pnt.normalOf3Pnts`, `Pnt.distPt` is now `Pnt.distPnt`, `Pnt.divPt` is now `Pnt.divPnt`, `Line3D.lengthToPtOnLine` is now `Line3D.lengthToPntOnLine` and `Polyline3D.createFromPts` is now `Polyline3D.createFromPnts`. `GetPt` and `getPt` of `Polyline3D` and `FreeBox` are now `GetPnt` and `getPnt`, and `SetPt` and `setPt` of `FreeBox` are now `SetPnt` and `setPnt`. `GetPoint` and `getPoint` of `Box` and `BBox` are now `GetPnt` and `getPnt`. The 2D functions on `Pt` keep their names.
- The point setters of `Polyline2D` and `Polyline3D` are renamed to match `GetPt`, `SetPt`, `GetPnt` and `SetPnt`: `SetPointXY` and `SetPointXYClosed` of `Polyline2D` are now `SetPtXY` and `SetPtXYClosed`, and `SetPointXYZ` and `SetPointXYZClosed` of `Polyline3D` are now `SetPntXYZ` and `SetPntXYZClosed`. The new static functions `setPtXY`, `setPtXYClosed`, `setPntXYZ` and `setPntXYZClosed` take the point index first, before the coordinates, like `getPnt`, `setPnt` and `FreeBox.setPntXYZ`. The old names still work but are obsolete, and their static functions keep the point index after the coordinates.
- The rotate members are renamed to the new grammar. The old names still work but are obsolete: `rotateBy` and `RotateBy` of `Pt`, `Vc` and `UnitVc` are now `rotate` and `Rotate`. `Pt.rotateWithCenterBy` is now `Pt.rotateAround`, and `Pt.rotateWithCenter`, which takes degrees, is now `Pt.rotateAroundDegrees`. `rotateWithCenter` and `RotateWithCenter` are now `rotateAround` and `RotateAround` on `Pnt`, `Line2D`, `Line3D`, `Rect2D`, `Rect3D`, `Box`, `FreeBox`, `Polyline2D` and `Polyline3D`. The new `RotateAround` is curried: `ln.RotateAround cen r` instead of `ln.RotateWithCenter(cen, r)`. The obsolete `RotateWithCenter` keeps its tupled arguments.
- The equality members are renamed, so that each name says if it uses a tolerance. The old names still work but are obsolete: `equals tol` is now `equalsWithin tol` and `notEquals tol` is now `notEqualsWithin tol` on `Pt`, `Pnt`, `Vc`, `Vec`, `UnitVc`, `UnitVec`, `Line2D`, `Line3D`, `PPlane`, `NPlane`, `BRect`, `BBox`, `Rect2D`, `Rect3D`, `Box`, `Matrix`, `RigidMatrix`, `Quaternion` and `Rotation2D`, as far as each type has them. `Polyline2D.equalsTol` and `Polyline3D.equalsTol` are now `equalsWithin`, and their exact `equals` is now `equalsExact`. `Quaternion.equalsRotation` is now `Quaternion.equalsRotationWithin`. The tolerance still applies to each coordinate separately, not to the Euclidean distance. Use `equalsExact` instead of a tolerance of 0.0.
- The members of `XLine2D`, `XLine3D`, `XLineXY` and `XLineXYZ` that return a discriminated union are renamed from `get…` to `classify…`. The old names still work but are obsolete: `getIntersection`, `getIntersectionParam`, `getRayIntersection`, `getRayIntersectionParam`, `getRayClosestParam`, `getClosestParameters`, `getClosestPoints` and `getEndsTouching` are now `classifyIntersection`, `classifyIntersectionParam`, `classifyRayIntersection`, `classifyRayIntersectionParam`, `classifyRayClosestParam`, `classifyClosestParameters`, `classifyClosestPoints` and `classifyEndsTouching`. `XLine3D.intersectCone` is now `XLine3D.classifyConeIntersection`.

## [0.52.0] - 2026-10-10
### Added
- Added `with`-prefixed replacements for all members that had a `set` prefix but returned a new value instead of mutating: `Line2D.withStart/withFrom/withEnd/withTo`, `Line3D.withStart/withFrom/withEnd/withTo`, `PPlane.withOrigin/withOriginX/withOriginY/withOriginZ`, and `Quaternion.WithAngleInRadians/WithAngleInDegrees` (plus the static `withAngleInRadians`/`withAngleInDegrees`).
- Added `Box.DoesIntersectPlane` and `Box.IntersectPlane`, plus `BBox.DoesIntersectPlane` and `BBox.IntersectPlane`, and their static counterparts, to test for and get the intersection polygon points of a box with an `NPlane`.
- Added `Box.tryGetBoxBetween` and `Box.getBoxBetween` to get the box filling the gap between two aligned boxes.
- Added `Box.areBoxesAligned`, `Box.getBoxesAlignment`, and `Box.failIfBoxesNotAlignedTo` to check if the axes of two boxes are parallel and equally oriented.
- Added `Box.withSizeX`, `Box.withSizeY`, `Box.withSizeZ`, and `Box.withOrigin`.
- Added `Matrix.createPerspectiveAlongNegZ` and `Matrix.createPerspectiveAlongPosY` for perspective cameras looking along -Z with +Y up, or +Y with +Z up, respectively.
- Added `Matrix.createLookAt` and `Matrix.createPerspectiveLookAt` for cameras with an arbitrary position, target, and preferred up vector.
- Added `Matrix.createLookAtZUp` and `Matrix.createPerspectiveLookAtZUp` to keep world +Z up without camera roll while pitching toward the target. Vertical and nearly vertical views use world +Y as the fallback up direction.
- `Rect2D`, `Rect3D`, `BRect`, `Box`, `BBox`, and `FreeBox` now have a reverse-direction instance and static member for every named edge (e.g. `Edge10` next to `Edge01`, `Edge23` next to `Edge32`), so every edge can be addressed in either direction.
- Added `Line2D.pointAtDistanceFromEnd` and `Line3D.pointAtDistanceFromEnd`, finding a point at a given distance from the line end, going towards the start.
- Added `Polyline2D.checkForDuplicatePoints` and `Polyline3D.checkForDuplicatePoints`, which return the original polyline when unchanged or a cleaned polyline and reported duplicate points.
- Added `Polyline3D.TryPlane` and `Polyline3D.IsPlanar`, plus the static `tryPlane` and `isPlanar`, to get the `NPlane` of a planar Polyline3D within a tolerance.
- Added `Polyline3D.AveragePlane` and `Polyline3D.TryAveragePlane`, plus the static `averagePlane` and `tryAveragePlane`, to get the average `NPlane` of a Polyline3D even when its points are not planar.
- Added `Polyline3D.TryAverageNormal` and the static `tryAverageNormal`, returning `None` instead of failing where `AverageNormal` finds no normal.
- The unsafe internal constructors of `NPlane`, `PPlane`, `Box`, `BRect`, and `BBox`, and therefore all their `createUnchecked` members, now verify their input when compiled in DEBUG mode or with the `CHECKED_EUCLID` symbol defined. This covers unitized normals and axes, perpendicular and right-handed frames, `NaN` and `Infinity` values, and min values not being bigger than max values. The other unsafe constructors already did this.
- Documented the `CHECKED_EUCLID` compiler flag in the Readme.
- Added `Rect3D.translateLocalZ`, an alias of `Rect3D.offsetZ`, for naming consistency with `Box.translateLocalX/Y/Z` and `Rect3D.translateLocalX/Y`.

### Removed
- The obsolete numeric `Edge0`-`Edge11` instance members on `Box` and `FreeBox` (deprecated aliases for the endpoint-named edges) have been removed; `Edge10` is now used for the reverse of `Edge01` instead.
- `Polyline2D.iterSkipLast`, `Polyline2D.iterPtSkipLast`, `Polyline3D.iterSkipLast`, and `Polyline3D.iterPtSkipLast` have been removed.

### Changed
- The `System.Text.Json` converter types (e.g. `BBoxJsonConverter`) are now internal. Serialization with `System.Text.Json` works as before, since the types still reference their converters via attributes.
- Builds use the .NET SDK's built-in SourceLink instead of the `Microsoft.SourceLink.GitHub` package.
- `Polyline3D.AverageNormal` now fails if no normal can be found, instead of silently returning an almost zero length vector. This is the case if the Polyline3D has less than 3 points, if all points are in one line, if the cross products cancel each other out on a self intersecting shape, or if the Polyline3D is very small. Use `Polyline3D.TryAverageNormal` to get `None` instead of an exception.
- The `CHECK_EUCLID` compiler symbol was renamed to `CHECKED_EUCLID`.
- Pinned the `System.Text.Json` package reference (net472 only) to the lowest version providing the required APIs, `4.6.0`, instead of tracking latest, and excluded it from Dependabot version updates.
- Clarified the docstrings of `Box.MoveX/Y/Z`, `Rect2D.MoveX/Y`, and `Rect3D.MoveX/Y/Z` (instance and static) to state that they move along the **world** X/Y/Z axes, not the shape's own (possibly rotated) local axes. Use `translateLocalX/Y/Z` for local-axis moves.

### Deprecated
- All members with a `set` prefix that return a new value instead of mutating are obsolete, use the `with`-prefixed member of the same name instead. The `set` prefix suggested in-place mutation, which these never did. This affects `Pt.setX/setY`, `Pnt.setX/setY/setZ`, `Vc.setX/setY`, `Vec.setX/setY/setZ`, `Line2D.setStart/setFrom/setEnd/setTo`, `Line3D.setStart/setFrom/setEnd/setTo`, `PPlane.setOrigin/setOriginX/setOriginY/setOriginZ`, and `Quaternion.SetAngleInRadians/SetAngleInDegrees` (plus the static `setAngleInRadians`/`setAngleInDegrees`). The genuinely mutating `Set` members on `Polyline2D`, `Polyline3D`, `FreeBox`, and `ResizeArr` are unchanged.
- `Matrix.createPerspective` is obsolete. Use `Matrix.createPerspectiveAlongNegZ` instead; the old name remains a compatibility alias with the same projection behavior.
- `Box.translate`, `Rect2D.translate`, `Rect3D.translate`, and `PPlane.translate` are obsolete. They were plain aliases for `.move` (a world-space vector translation), which is ambiguous alongside the local-axis `translateLocalX/Y/Z` members on the same types. Use `.move` (or `PPlane.translateBy`) for a world-space vector, or `.translateLocalX/Y/Z` to move along the shape's own axes.

### Fixed
- Packaging: the Fable source files are now packed from the Src folder instead of the repository root, so the package no longer ships test sources or generated obj files.
- `XLineXY.sqDistance` and `XLineXYZ.sqDistance` (and the 2D and 3D line distance functions built on them) now return the correct squared distance for nearly parallel segments, where numeric cancellation could give a wrong result.
- `AsFSharpCode` of all types now formats floats with the invariant culture and round-trip precision, so the generated code compiles and recreates the exact values on any system locale.

## [0.51.0] - 2026-07-28
### Added
- Native `System.Text.Json` support for every Euclid type. (Newtonsoft.Json is already supported.)
- Checked `create` functions for `Rotation2D`, `PPlane`, and `Box`.
- JSON tests for deserialization and serialization.

### Changed
- `JsonConverter.Read` implementations use checked constructors and reject invalid unit vectors, rotations, quaternions, planes, rectangles, boxes, bounds, and matrices instead of constructing them unchecked.

## [0.50.0] - 2026-07-12
### Added
- Axis-aligned bounds are now first-class conversion and intersection types:
  - `BBox` has `AsBox`, `GetPoint`, `GetEdge`, `GetFace`, named faces, `Faces`, `IntersectRay`/`intersectRay`, `createFromRect3D`, and `createFromBox`.
  - `BRect` has `AsRect2D`, `IntersectRay`, `intersectRay`, `createFromRect2D`, and `createFromRect3D`.
  - `Box` has `createFromBounds`, `GetPoint`, `GetEdge`, and `GetFace`; `Rect2D` has `createFromBounds`, ray intersection, and finite-line clipping.
- `Box` and `FreeBox` edges can be addressed by endpoint names (`Edge01`, `Edge12`, ... `Edge37`), with matching static members. The old numeric `Edge0`-`Edge11` instance members remain as obsolete aliases.
- `Rect3D.ClosestParameters` and `ClosestPoint`, plus static counterparts.
- `Line2D` and `Line3D` now expose `Start`/`End` aliases and static `setFrom`/`setTo` helpers. `Line2D.intersectRays` returns the unchecked intersection of two non-parallel infinite rays.
- `Tria3D.intersectRay`, `intersectRayXYZ`, and the allocation-free `intersectTriaVectorsWithRay` return the intersection parameter of an infinite line with a triangle.
- Curried `setX`/`setY`/`setZ` helpers for points and vectors, and optimized `ResizeArr.getIdx`/`setIdx` helpers for Fable.
- Missing 2D/3D conversion members: `Vec.AsPt`, `UnitVec.AsPt`, `UnitVec.AsUnitVc` and `Pnt.AsVc`.
- `Points2D.center`, mirroring `Points3D.center`.
- `Rect2D` instance members `Move`, `MoveX`, `MoveY`, `Rotate` and `RotateWithCenter`, plus static `moveX` and `moveY`, mirroring `Rect3D`.
- `Line3D.isCoincidentAndOpposingToFast` static, mirroring `Line2D`.
- `BRect.Edges` and `BRect.edges` returning all 4 edges, mirroring `BBox.Edges`.
- Added `PPlane.asFSharpCode`, `FreeBox.edges`, `RigidMatrix.isIdentity`, and `Rect3D.pointsXYZLooped` static counterparts.

### Changed
- Intersection APIs now consistently use `voption` where absence is expected. This includes the `XLine2D`/`XLine3D` `try*` APIs, line extension intersections, plane intersections, `Box.IntersectRay`, `Rect3D.intersectRay`/`intersectLine`, and `Tria3D.intersectLine`.
- `XLine2D` and `XLine3D` were reorganized: raw coordinate implementations live in the `XLineXY` and `XLineXYZ` modules, while the public types provide point/vector and `Line2D`/`Line3D` overloads. `XLine2D.closestParameters` now returns a struct tuple.
- Plane naming and intersection results were harmonized:
  - `DistanceToPt*` and `PlaneAtClPt` are now `DistanceToPnt*` and `PlaneAtClPnt` on `NPlane` and `PPlane`.
  - `PPlane.EvaluateAt` now takes two in-plane coordinates; the three-coordinate form is `EvaluateAtXYZ`.
  - `NPlane.intersectRay`, `PPlane.intersectRay`, and `Rect3D.intersectRay` return a line parameter. Finite `intersectLine` methods return the intersection point.
- Bounds conversions now live on the target type: use `BBox.createFromBox`/`createFromRect3D` and `BRect.createFromRect2D`/`createFromRect3D`. The old source-type members remain only as obsolete tuple-returning compatibility helpers.
- `RigidMatrix.Matrix` was renamed to `ToMatrix`. `Quaternion.createFromDegree` was renamed to `createFromDegrees`; the singular form remains obsolete.
- Corrected the public spelling `Colinear` to `Collinear`, including `Polyline2D.removeDuplicateAndCollinearPoints`, `Polyline3D.removeDuplicateAndCollinearPoints`, and offset internals.
- `Line3D.createFromLine2DwithZ` was renamed to `createFromLine2DWithZ`.
- Standardized invalid-point checks and squared-distance naming with `Pt.IsInvalid`/`Pt.isInvalid`, `Pnt.IsInvalid`/`Pnt.isInvalid`, and `Pnt.SqDistanceInXYFromOrigin`/`Pnt.sqDistanceInXYFromOrigin`. The previous misspelled or inconsistent names remain as obsolete aliases.
- Corrected the public spelling `Matrix.createFromColumMajorArray` to `createFromColumnMajorArray`; the misspelled name remains as an obsolete alias.
- `BBox.Union(Pnt)` is now `BBox.UnionPnt` and static `BBox.unionPt` is now `unionPnt`, matching `BRect.UnionPt`/`unionPt`; the old names remain as obsolete aliases.
- `Line2D` and `Line3D` squared point distances now use To-naming: `SqDistanceToPt`/`SqDistanceToXY` and `SqDistanceToPnt`/`SqDistanceToXYZ` replace the `SqDistanceFrom*` forms, which remain as obsolete aliases.
- `Rect2D.PointsXYLoopedCCW`/`PointsXYLoopedCW` are now `PointsXYLooped_CCW`/`PointsXYLooped_CW` (with matching statics), consistent with `PointsXY_CCW`; the old names remain as obsolete aliases.
- Removed duplicate static members that were defined on both the `Line2D`/`Line3D` types and their extension modules; the extension-module versions remain. `Line2D.Direction`, `Tangent` and `UnitTangent` are now extension members, like on `Line3D`.
- Unified the spelling of the optional `varDistParallelBehavior` parameter of `Polyline3D` offsets to match `Polyline2D`.
- Renamed the misleading `Rect3D.PointsXY`/`pointsXY` and `PointsLoopedXY` coordinate-buffer APIs to `PointsXYZ`/`pointsXYZ` and `PointsXYZLooped`/`pointsXYZLooped`; the old names remain as obsolete aliases.
- `Format.rarr` and `Format.iList` are now internal implementation helpers.
- Basic `Line3D` static helpers are declared in `AutoOpenLine3D`, consistently with `AutoOpenLine2D`.

### Fixed
- Reworked 2D and 3D line relationships so intersection, closest-point, closest-parameter, overlap, and squared-distance results are invariant under line order and direction. This also fixes near-parallel, anti-parallel, perpendicular, overlapping, and zero-length cases.
- Corrected the 3D closest-line Lagrange numerators for oblique coplanar and skew lines.
- `Tria3D.intersectLine` now uses a two-sided Moller-Trumbore test, is independent of triangle vertex order, and correctly distinguishes a finite segment from its infinite ray.
- `Polyline2D` and `Polyline3D` offsetting now handles straight runs and intermediate collinear points without losing points or changing round-trip geometry.
- Rectangle and box ray intersections now handle rays on faces or edges, corner grazing, rays starting inside, parallel slabs, and zero-length rays consistently.
- `Line2D.lengthToPtOnLine` and `Line3D.lengthToPtOnLine` now return the signed projected distance from the line start, including points before or beyond the segment.
- Audited and corrected the generated API documentation: counterpart summaries consistently describe their shared behavior without requiring explicit “Same as ...” wording, value-option results use `ValueSome`/`ValueNone`, orientation wording consistently uses “counter-clockwise”, and stale type names, parameter names, grammar, and spelling were corrected.
- Corrected copy-paste doc comments: `Rect3D.Yaxis` and `Rect3D.xAxis` described the wrong axis; `Rect3D.isValid`/`hasArea` statics now cross-reference each other; removed doubled words in `Polyline2D.mapiPt`/`iteriPt` docs.
- Corrected and completed API documentation for matrix fields, record fields, containment behavior, box faces, scaling, axes, coordinate buffers, and assorted spelling and grammar issues.

### Removed
- The obsolete `Intersect` module and its `lineTriangle`/`lineCone` helpers.
- Obsolete `Vc.intersection` and `UnitVc.intersection` helpers, redundant `NPlane.distToPt*` aliases, obsolete infinite `Rect3D` intersection aliases, and static `toString` helpers on bounds/box/rectangle types.

## [0.42.0] - 2026-06-20
### Added
- XY/XYZ coordinate overloads for point-taking members that return a `float` or `bool`, each with a curried static counterpart. The original `Pt`/`Pnt` members now delegate to these:
  - `Box.ContainsXYZ`, `Rect2D.ContainsXY`, `Rect3D.ContainsXY`/`ContainsXYZ`, `BBox.ContainsXYZ`, `BRect.ContainsXY`.
  - `Line2D`: `ClosestParameterXY`, `RayClosestParameterXY`, `DistanceToXY`, `SqDistanceFromXY`, `DistanceRayXY`, `SqDistanceRayXY`, `IsXYOnLeft`, `IsXYOnRight`.
  - `Line3D`: `ClosestParameterXYZ`, `RayClosestParameterXYZ`, `DistanceToXYZ`, `SqDistanceFromXYZ`, `DistanceRayXYZ`, `SqDistanceRayXYZ`.
  - `Polyline2D`: `ClosestParameterXY`, `DistanceToXY`, `ContainsXY`, `SignedDistanceToXY`.
  - `Polyline3D`: `ClosestParameterXYZ`, `DistanceToXYZ`.
  - `NPlane` and `PPlane`: `DistanceToXYZ` and `DistanceToXYZSigned`.
- `FreeBox` now has `Points`, the corner accessors `Pt0`–`Pt7`, `SetPt`, the `Edges` array and `Edge0`–`Edge11`, and a `ToString` with ASCII art.
- `Rect3D.PointsXY` and `Rect3D.PointsLoopedXY` returning the corner coordinates as interlaced float `ResizeArray`s.
### Changed
- `Box`, `NPlane` and `PPlane` now store their components as individual floats internally (like `Rect2D`/`Rect3D` since 0.30.0). The float fields (e.g. `OriginX`, `XaxisX`, `NormalX`) are public, and `Origin`, `Xaxis`/`Normal` etc. are now computed members.
- `Box.createUnchecked` now takes 12 floats instead of point/vector tuples; the point/vector form is now `Box.createUncheckedVec`. Likewise `NPlane.createUnchecked` takes 6 floats with the point/vector form moved to `NPlane.createUncheckedVec`, and `PPlane.createUncheckedXYZ` was renamed to `PPlane.createUnchecked` (floats) with the point/vector form now `PPlane.createUncheckedVec`.
- `Box.yaxisUnit` / `zaxisUnit` static accessors renamed to `yAxisUnit` / `zAxisUnit` for naming consistency. New `Box.origin`, `xAxis`, `yAxis`, `zAxis` static accessors were added.
- `Rect3D.xaxis` / `yaxis` static accessors renamed to `xAxis` / `yAxis`; the parameter name in the `Rect3D` line/ray intersection members is now `r` instead of `pl`.

## [0.41.0] - 2026-06-16
### Added
- `Polyline2D` and `Polyline3D` now have `tryFind`, `tryFindLast`, `tryFindIndex`, and `tryFindLastIndex` point-search helpers.
`iterSkipLast` and `iterPtSkipLast` point iteration helpers.
`iterSegments` and `iterLineSegments` segment iteration helpers.
- `Rect2D` and `Rect3D` now has `iterPointsCCW`, `iterPointsLoopedCCW`, `iterPointsCW`, and `iterPointsLoopedCW` corner iteration helpers.
- `distanceToXY` and `distanceToXYZ` functions on `Pt` and `Pnt`

## [0.40.0] - 2026-01-13
### Fixed
- `Polyline2D.SegmentVectorsXY` and `Polyline3D.SegmentVectorsXYZ` no longer throw on an empty polyline (negative ResizeArray capacity).
- `Polyline2D.close`, `Polyline3D.close`, `Polyline2D.IsAlmostClosed` and `Polyline3D.IsAlmostClosed` now work with 0.0 tolerance on exactly closed polylines (tolerance comparison was strict less-than).
- `removeDuplicatePoints` on `Polyline2D` and `Polyline3D` keeps the first point when all points collapse into one duplicate cluster, instead of replacing it with the last point.
- The angle tolerance range error messages of `removeDuplicateAndColinearPoints` now report degrees (previously printed radians labeled as degrees).
- `Polyline2D.offsetVar` error messages now name offsetVar instead of offset.
- `Polyline3D.offsetVar` now checks for at least 2 points before computing the average normal, like `Polyline3D.offset`; the colinear-points error messages of `offset` and `offsetVar` now include the polyline (previously ended in a dangling colon).
- Several `Polyline2D` and `Polyline3D` docstring corrections: `SetPoint` no longer claims to keep closed polylines closed (it never did, use `SetPointXYClosed`), the parameter domain of the static `evaluateAt`/`closestParameter` ends at points.Count - 1.0 (not point count), `subPolyline` documents its actual snapping behavior, stale `Offset2D.UTurnBehavior`/`VarDistParallelBehavior` type names, and typos.
- `Polyline2D.tryFindSelfIntersection` now returns segment indices instead of flat ResizeArray indices.
- `Polyline3D.SignedAreaIn2D` now returns the actual projected signed area instead of double the area.
- `Polyline3D` helper and tolerance inconsistencies: `setPointXYZ` is curried like the 2D setter, `CloseInPlace` works with 0.0 tolerance, XY member creation uses Z=0, XYZ member creation helpers were added, and duplicate-point removal now uses distance tolerance while preserving the endpoint.
- `Rect2D` and `Rect3D`: the perpendicularity check in `createFromVectors` and the internal constructor now scales with the axis lengths, so large but valid rectangles are no longer falsely rejected.
- `Rect2D.fitToPoints`: corrected the misleading "Vc.Unitized" text in the too-short-axis exception messages.
- corrected several `Rect3D` docstrings that mentioned "2D rectangle" and wrong grid/subdivision rounding factors.
### Changed
- `Polyline3D.removeColinearAndDuplicatePoints` renamed to `removeDuplicateAndColinearPoints` for naming consistency with `Polyline2D`. The old name was removed.
- `Polyline2D.close` and `Polyline3D.close` now require at least 3 points (was 2), matching `CloseInPlace`.
- `removeDuplicateAndColinearPoints` on `Polyline2D` and `Polyline3D` now raises a clear error when all points are within the distance tolerance of the first point (previously failed with an unhelpful zero-length-vector error or returned inconsistent results).
- `Polyline2D.SignedArea` now raises a named error on an empty polyline (previously an obscure index error).
- `Polyline2D.FindLabelPoint` now validates that the precision parameter is a positive number.
- `Polyline3D.equals` now compares points by Euclidean distance, matching `Polyline2D.equals`. Previously each axis was compared separately, which accepted point pairs up to sqrt(3) times the tolerance apart.
- `Polyline3D.ToString` now distinguishes open and closed polylines, like `Polyline2D.ToString`.
- `Polyline3D.SignedAreaIn2D` now raises a named error on an empty polyline instead of returning 0.0, matching `Polyline2D.SignedArea`.
- `Polyline3D.addXYZ` is now fully curried like `Polyline2D.addXY`. Previously the coordinates were a tuple: use `Polyline3D.addXYZ x y z pl` instead of `Polyline3D.addXYZ (x, y, z) pl`.
- `Polyline3D.offset'` now takes the in-plane offset distance first and the perpendicular offset distance second, matching the parameter order of `Polyline3D.offset`. The order was previously reversed; since both are floats, existing callers must swap their arguments.
- `Polyline3D.offsetVar` and `offsetVarWithRef`: the list parameter `inPlaneOffsetDistance` is renamed to `inPlaneOffsetDistances`, it was always a list with one distance per segment.
- `Rect3D.xaxisUnit` / `yAxisUnit` renamed to `Rect3D.xAxisUnit` / `yAxisUnit` for naming consistency with `Rect2D`. The old names remain as obsolete aliases.
- `Pnt.normalOf3Pts` now follows the library orientation convention (counter-clockwise points give a normal towards the viewer, e.g. +Z in the XY plane), matching `NPlane.createFrom3Points` and `Points3D.normalOfPoints`.
Previously it returned the negated normal. Callers relying on the old direction must negate the result.
- Angle helpers with a static (from, to) signature now take the arguments in natural order (first = from, second = to),
matching `angle2Pi`: `Vc.angleDiamond`, `UnitVc.angleDiamond`, `Vec.angleDiamondInXY`, `UnitVec.angleDiamondInXY`, `Pt.angle2PiTo`, `Pt.angle360To`, `Pnt.directionDiamondInXYTo`, `Pnt.angle2PiInXYTo` and `Pnt.angle360InXYTo`. The arguments were previously reversed; callers that compensated for the old order must swap their arguments.
- `Polyline2D.FindLablePoint` / `findLablePoint` renamed to `FindLabelPoint` / `findLabelPoint` (corrected spelling). The misspelled names remain as obsolete aliases.
- `moveX`/`moveY`/`moveZ` on `Vc`, `Vec`, `UnitVc` and `UnitVec` renamed to `addX`/`addY`/`addZ`: these are vector additions (a vector has no location to "move"), so the name now matches the "Points vs Vectors" semantic.
The move* names remain as obsolete aliases. The move* members on location types (`Pt`, `Pnt`, `Line`, `Rect`, `Box`, `Polyline`, ...) are unchanged.
- `Line2D.tryGetOverlap` now uses the same coincidence tolerance as `Line2D.tryIntersectOrOverlap`: parallel lines count as coincident when their perpendicular distance is below 1e-6 (1e-12 squared). Previously `tryGetOverlap` used a looser ~3.16e-5 (1e-9 squared).
- `XLine3D.intersectCone` more result cases and better docstring.

### Removed
- Vec.rotateWithCenter and UnitVec.rotateWithCenter: rotating a vector (which is only a direction with magnitude, not a location) around a center point is not a valid operation. Both members were unused. See the "Points vs Vectors" section in README.md.
- Points.cullDuplicatePointsInSeq, use Polyline.removeDuplicatePoints instead

### Added
- Polyline2D.tryFindSelfIntersection optimized

## [0.30.1] - 2026-03-24
### Fixed
- Polyline2D.area doesn't fail on open polylines anymore.
- wrong point count when offset distance is 0.0
- fix close in place with 0.0 tolerance.
- Polyline2D.tryFindSelfIntersection no longer reports adjacent segments as a self intersection: the shared closure vertex of a closed Polyline2D and the corner of an open 3-point Polyline2D are no longer false positives. It also returns None instead of throwing for Polylines with fewer than 3 points.

## [0.30.0] - 2026-03-23
### Added
- add a static member for every instance member
### Changed
- naming consistency , some members marked obsolete and renamed to have more consistent naming
- Polyline2D and 3D use a ResizeArray<float>  of interlaced x0, y0, z0, x1, y1, z1, ... internally , instead of ResizeArray<Pnt>
- Rect2D and 3D use just 6 (or 9) floats instead of 3  points or vectors internally.

## [0.20.0] - 2026-02-08
### Added
- improved docstring
- XLine2D.tryIntersectRay
- RayClosestPoints

## [0.20.0-beta3] - 2026-01-24
### Changed
- renamed lineLineClosedParameters to closestParameters

## [0.20.0-beta2] - 2026-01-23
### Changed
- Deprecate Loop module,
- Make sure all renamed modules and functions have proper deprecation messages
### Added
- Tests for deprecated functions and modules to ensure they still work
- Add more Tests


## [0.20.0-beta1] - 2026-01-17
### Changed
- Reimplement Polyline2D and 3D offsetting more efficiently and with error correction in Offset2D and Offset3D modules
- New XLine2D and XLine3D modules for Line to Line intersections and relationships. deprecate LineIntersectionTypes
- Split Topology module into Topology2D and Topology3D
- Deprecate Points module, use new Tria2D, Tria3D, and Points2D and Points3D for PointClouds instead
### Added
- Polylabel algorithm for finding pole of inaccessibility in polygons.

## [0.16.0] - 2025-06-29
### Added
- more methods for Polyline
### Changed
- Polyline is no longer a struct
### Fixed
- edge case bug in BBox.Contains

## [0.15.0] - 2025-06-14
### Added
- FreeBox type
- Matrix tests
- add Polyline.Segment
- add Scale and ScaleOn methods
### Changed
- args for Box.createFromRect2D are curried now
- Matrix rewrite

## [0.14.0] - 2025-05-24
### Added
- expand relative for Boxes and Rects
- line on line projection
- close Polylines
- add move and translate where missing
- add scale functions
- zero length edge queries for Boxes and Rects
- getting edges of Box
### Fixed
- fix Line3D.ExtendRel error cases

## [0.13.0] - 2024-03-15
### Fixed
- bug in finite line intersection
### Changed
- Rename Plane to NPlane
### Added
- don't offset on zero distance
- lines touching check functions
- more precomputed Cosine values
- more ways to do Dot and Cross products
- add notEquals functions
- docs generation via FsFormatting library

## [0.12.0] - 2024-11-27
### Added
- add AreaSquared on Rects and Boxes
- allow negative offsets in Rect.Offset
- add BoundingBox for Rects and Boxes

## [0.11.1] - 2024-10-01
### Fixed
- fix Polyline2D.Area

## [0.11.0] - 2024-09-25
### Added
- add Box and Rect.contains Pt
### Fixed
- fix bad tolerance in Rect2D.tryCreateFrom3Points

## [0.10.0] - 2024-09-15
### Added
- add Line.divideEvery
- add Rect.createFromDirAndPoints
### Changed
- rename Width and Height to SizeX and SizeY for Rect and Box
- check TS build via Fable

## [0.9.0] - 2024-07-06
### Changed
- add suffix '90' to angle functions in Plane module
- improve precision of Pi literals
### Added
- include NaN check in isTiny functions

## [0.8.0] - 2024-05-02
### Fixed
- fix line splitting
### Changed
- rename Rect.create functions

## [0.7.0] - 2024-04-28
### Changed
- refactor offset
### Added
- add Topology sorting

## [0.6.1] - 2024-02-17
### Changed
- remove reflection form error messages for nicer Fable output
- rename Rect.Rotate to Rect.RotateOrientation
### Added
- add Rect.grid
### Fixed
- fix Rect.subdivide

## [0.5.0] - 2024-01-07
### Added
- allow equality checks to use zero tolerance
- better docstring
- add more angle calculation functions
- add rotateByQuarterCircle
- add ExtendRel functions for Lines
### Changed
- rename distanceFromPoint to distanceToPt
- use *** as operator for dot product, and matrix multiplication
- add splitting with gap to Lines and Rects

## [0.4.1] - 2023-12-02
### Fixed
- fix bug in rect.Edge23
### Added
- add BBox.createFromIList

## [0.4.0] - 2023-11-25
### Added
- add Line.withLengthFromMid
- add spherical interpolation (slerp) for vectors
### Changed
- rename isAngleLessThan functions
- rename Length to Width on Boxes and Rects
- rename Width to Height2D on Rects
- rename Width to Depth on Boxes
- add EdgeMethods on Rects and Boxes

## [0.3.0] - 2023-10-22
### Changed
- renamed OrthoMatrix to RigidMatrix
- ofXZY functions renamed to createFromMembersXYZ
### Added
- add DataMember Attributes for serialization
- add DivideByInt static members for Array.average and Array.sum for functions
### Fixed
- fixed typos, improve documentation

## [0.2.1] - 2023-02-26
### Fixed
- fix fable support

## [0.2.0] - 2023-02-04
### Added
- fable support
- improved xml documentation

## [0.1.0] - 2023-02-04
### Added
- first public release

[Unreleased]: https://github.com/goswinr/Euclid/compare/0.52.0...HEAD
[0.52.0]: https://github.com/goswinr/Euclid/compare/0.51.0...0.52.0
[0.51.0]: https://github.com/goswinr/Euclid/compare/0.50.0...0.51.0
[0.50.0]: https://github.com/goswinr/Euclid/compare/0.42.0...0.50.0
[0.42.0]: https://github.com/goswinr/Euclid/compare/0.41.0...0.42.0
[0.41.0]: https://github.com/goswinr/Euclid/compare/0.40.0...0.41.0
[0.40.0]: https://github.com/goswinr/Euclid/compare/0.30.1...0.40.0
[0.30.1]: https://github.com/goswinr/Euclid/compare/0.30.0...0.30.1
[0.30.0]: https://github.com/goswinr/Euclid/compare/0.20.0...0.30.0
[0.20.0]: https://github.com/goswinr/Euclid/compare/0.20.0-beta3...0.20.0
[0.20.0-beta3]: https://github.com/goswinr/Euclid/compare/0.20.0-beta2...0.20.0-beta3
[0.20.0-beta2]: https://github.com/goswinr/Euclid/compare/0.20.0-beta1...0.20.0-beta2
[0.20.0-beta1]: https://github.com/goswinr/Euclid/compare/0.16.0...0.20.0-beta1
[0.16.0]: https://github.com/goswinr/Euclid/compare/0.15.0...0.16.0
[0.15.0]: https://github.com/goswinr/Euclid/compare/0.14.0...0.15.0
[0.14.0]: https://github.com/goswinr/Euclid/compare/0.13.0...0.14.0
[0.13.0]: https://github.com/goswinr/Euclid/compare/0.12.0...0.13.0
[0.12.0]: https://github.com/goswinr/Euclid/compare/0.11.1...0.12.0
[0.11.1]: https://github.com/goswinr/Euclid/compare/0.11.0...0.11.1
[0.11.0]: https://github.com/goswinr/Euclid/compare/0.10.0...0.11.0
[0.10.0]: https://github.com/goswinr/Euclid/compare/0.9.0...0.10.0
[0.9.0]: https://github.com/goswinr/Euclid/compare/0.8.0...0.9.0
[0.8.0]: https://github.com/goswinr/Euclid/compare/0.7.0...0.8.0
[0.7.0]: https://github.com/goswinr/Euclid/compare/0.6.1...0.7.0
[0.6.1]: https://github.com/goswinr/Euclid/compare/0.5.0...0.6.1
[0.5.0]: https://github.com/goswinr/Euclid/compare/0.4.1...0.5.0
[0.4.1]: https://github.com/goswinr/Euclid/compare/0.4.0...0.4.1
[0.4.0]: https://github.com/goswinr/Euclid/compare/0.3.0...0.4.0
[0.3.0]: https://github.com/goswinr/Euclid/compare/0.2.1...0.3.0
[0.2.1]: https://github.com/goswinr/Euclid/compare/0.2.0...0.2.1
<!-- [0.2.0]: https://github.com/goswinr/Euclid/compare/0.1.0...0.2.0
[0.1.0]: https://github.com/goswinr/Euclid/releases/tag/0.1.0 -->
