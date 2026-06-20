# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

[Unreleased]: https://github.com/goswinr/Euclid/compare/0.42.0...HEAD
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
