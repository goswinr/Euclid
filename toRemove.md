# Removal candidates

Members of the public API that could be removed before 1.0.
Nothing in this list has been removed yet.
The names are the current ones, after the renames listed in `CHANGELOG.md`.

Row counts are rows of `API-Table/ApiTable.md`, counting 2D and 3D separately.
All groups together are about 380 of the roughly 4000 rows.

## 1. Aliases

Two names for the same result.
Most of them say so in their docstring ("Same as …"), the others have the same body.
"Keep" is a recommendation.

### Line2D and Line3D, about 52 rows

| Remove | Keep | Note |
|--------|------|------|
| `Direction`, `direction`, `Tangent`, `tangent` | `Vector`, `vector` | All three return the full-length vector. `UnitTangent` is the unit vector and stays. The Readme itself called `Direction` a unit vector. |
| `Segment`, `segment` | `SubLine`, `subLine` | Matches `Polyline2D.subPolyline`. On polylines a segment is something else. |
| `IsNormalTo` (3 overloads), `isNormalTo`, `isNormalTo'`, `isNormalToVc` / `isNormalToVec`, `isNormalToUnitVc` / `isNormalToUnitVec` | the `IsPerpendicularTo` family | `IsPerpendicularTo` is the name that the vector types use. |
| `reverse` (static) | `Reversed` (instance), `reversed` (static) | The two statics are identical. |
| `isParallelTo'`, `isNotParallelTo'`, `isPerpendicularTo'` | `isParallelTo`, `isNotParallelTo`, `isPerpendicularTo`, and the `…Fast` tier | A prime is not part of the naming grammar. They are a third tier with a fixed tolerance and no check for short lines. |

### Points and vectors, about 30 rows

| Remove | Keep | Note |
|--------|------|------|
| `Pt.angleInCorner`, `Pnt.angleInCorner` | `Pt.angle180Pts`, `Pnt.angle180Pnts` | Both return the angle in degrees at the middle point. `angle180Pts` has the partner `anglePiPts`. |
| `Points2D.closestOfTwo`, `Points3D.closestOfTwo` | `Pt.closestOfTwo`, `Pnt.closestOfTwo` | Identical bodies. |
| `Vec.flipToPointUp`, `UnitVec.flipToPointUp` | `Vec.orientUp`, `UnitVec.orientUp` | `orientUp` has the partner `orientDown`. |
| `UnitVc.withLength`, `UnitVec.withLength` | `UnitVc.scale`, `UnitVec.scale` | On a unit vector the two are the same. Or the other way round. |
| `isParallelWithin`, `isNotParallelWithin`, `isAngleBelow`, `isAngleAbove` on `Vc`, `Vec`, `UnitVc`, `UnitVec` | `isParallelTo`, `isParallelAndOrientedTo` | The same tests under a second name, written out a second time. Compare the bodies before removing, the checks for short vectors differ slightly. |

### Other types, about 46 rows

| Remove | Keep | Note |
|--------|------|------|
| `PPlane.translateBy` | `PPlane.move` | `move` is the only translation verb. |
| `NPlane.areCoincident` | `NPlane.isCoincidentTo`, `IsCoincidentTo` | `areCoincident a b` is `a.IsCoincidentTo b` with the default tolerances. |
| `NPlane.angleTo` | `NPlane.angle90ToPlane` | |
| `Rect3D.offsetZ` | `Rect3D.moveAlongLocalZ` | |
| `Polyline2D`, `Polyline3D` and `FreeBox`: `Clone`, `clone` | `Duplicate`, `duplicate` | |

Not an alias, but two names that are too close:
`Rect3D.flip` and `Rect3D.flipped` return different rectangles.
One of them needs another name.

## 2. Combinatorial accessors, about 168 rows

| Remove | Use instead | Rows |
|--------|-------------|------|
| The reversed edges `Edge10`, `Edge21`, `Edge32`, `Edge03`, … and their statics on `Rect2D`, `Rect3D`, `BRect`, `BBox`, `Box`, `FreeBox` | `Edge01.Reversed`, or `GetLine(1, 0)` on the box types | 96 |
| `Pnt0X` … `Pnt7Z` on `Box`, and with their statics on `FreeBox` | `Pnt0.X`, `GetPoint`, and on `FreeBox` also `GetX`, `SetXYZ`, `MoveXYZ` and `XYZs` | 72 |

Euclid itself uses `Pnt0X` … `Pnt7Z` to avoid allocating points, see `.claude/commands/findAllocations.md`.
So these would be hidden from IntelliSense and from the tables, not deleted.

The static twins of the remaining edges and corner points, like `Box.edge01` and `Box.pnt3`, are about 85 more rows that few pipelines need.

## 3. Helpers that are public only for inlining, about 55 rows

These are not geometry. They are public because inline members call them.
Hide them from IntelliSense and from the API tables, instead of deleting them.

- The auto-opened `ResizeArray` extension members: `First`, `Second`, `Third`, `Last`, `SecondLast`, `ThirdLast`, `LastIndex`, `IsEmpty`, `Pop`, `PopAt`, `GetLooped`, `SetIdx`.
  With `open Euclid` they appear on every `ResizeArray` in the user's code.
  Their module is hidden from IntelliSense, but the members still apply to every `ResizeArray`.
  Calling the functions of the `ResizeArr` module inside Euclid would avoid the extension members.
- The modules `EuclidCollectionUtilities.Arr`, `ResizeArr` and `IList`.
- The helpers of `EuclidErrors` that serve one type only: `failRarr`, `failQuat`, `failRot`, `failRect2DOffsetEdge`, `failRect3DOffsetEdge`, `failTooFewPoly2D`, `failTooFewPoly3D`, `failObsoleteV20`.

## 4. Camera matrices, 6 rows

`Matrix.createLookAt`, `createLookAtZUp`, `createPerspectiveAlongNegZ`, `createPerspectiveAlongPosY`, `createPerspectiveLookAt` and `createPerspectiveLookAtZUp` serve rendering, not CAD geometry.
They could move to an extension package.

## 5. The `…FromPtsAndVcs` layer of XLine, about 39 rows

`XLine2D` and `XLine3D` offer every query three times: for two lines, for two points and two vectors (`…FromPtsAndVcs`, `…FromPntsAndVecs`), and in `XLineXY` and `XLineXYZ` for raw coordinates.
The middle layer only unpacks points and vectors for the raw kernels.
Callers that have no `Line2D` at hand can call the kernels directly.
