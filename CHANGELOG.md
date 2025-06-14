# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

[Unreleased]: https://github.com/goswinr/Euclid/compare/v0.14.0...HEAD
[0.14.0]: https://github.com/goswinr/Euclid/compare/v0.13.0...v0.14.0
[0.13.0]: https://github.com/goswinr/Euclid/compare/v0.12.0...v0.13.0
[0.12.0]: https://github.com/goswinr/Euclid/compare/v0.11.1...v0.12.0
[0.11.1]: https://github.com/goswinr/Euclid/compare/v0.11.0...v0.11.1
[0.11.0]: https://github.com/goswinr/Euclid/compare/v0.10.0...v0.11.0
[0.10.0]: https://github.com/goswinr/Euclid/compare/v0.9.0...v0.10.0
[0.9.0]: https://github.com/goswinr/Euclid/compare/v0.8.0...v0.9.0
[0.8.0]: https://github.com/goswinr/Euclid/compare/v0.7.0...v0.8.0
[0.7.0]: https://github.com/goswinr/Euclid/compare/v0.6.1...v0.7.0
[0.6.1]: https://github.com/goswinr/Euclid/compare/v0.5.0...v0.6.1
[0.5.0]: https://github.com/goswinr/Euclid/compare/v0.4.1...v0.5.0
[0.4.1]: https://github.com/goswinr/Euclid/compare/v0.4.0...v0.4.1
[0.4.0]: https://github.com/goswinr/Euclid/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/goswinr/Euclid/compare/v0.2.1...v0.3.0
[0.2.1]: https://github.com/goswinr/Euclid/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/goswinr/Euclid/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/goswinr/Euclid/releases/tag/v0.1.0

<!--
use to get tag dates:
git log --tags --simplify-by-decoration --pretty="format:%ci %d"
-->