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

### Line2D and Line3D,

| Remove | Keep | Note |
|--------|------|------|
| `Direction`, `direction`, `Tangent`, `tangent` | `Vector`, `vector` | All three return the full-length vector. `UnitTangent` is the unit vector and stays. The Readme itself called `Direction` a unit vector. |
| `Segment`, `segment` | `SubLine`, `subLine` | Matches `Polyline2D.subPolyline`. On polylines a segment is something else. |
| `IsNormalTo` (3 overloads), `isNormalTo`, `isNormalTo'`, `isNormalToVc` / `isNormalToVec`, `isNormalToUnitVc` / `isNormalToUnitVec` | the `IsPerpendicularTo` family | `IsPerpendicularTo` is the name that the vector types use. |
| `reverse` (static) | `Reversed` (instance), `reversed` (static) | The two statics are identical. |
| `isParallelTo'`, `isNotParallelTo'`, `isPerpendicularTo'` | `isParallelTo`, `isNotParallelTo`, `isPerpendicularTo`, and the `…Fast` tier | A prime is not part of the naming grammar. They are a third tier with a fixed tolerance and no check for short lines. |

### Points and vectors,

| Remove | Keep | Note |
|--------|------|------|
| `Pt.angleInCorner`, `Pnt.angleInCorner` | `Pt.angle180Pts`, `Pnt.angle180Pnts` | Both return the angle in degrees at the middle point. `angle180Pts` has the partner `anglePiPts`. |
| `Points2D.closestOfTwo`, `Points3D.closestOfTwo` | `Pt.closestOfTwo`, `Pnt.closestOfTwo` | Identical bodies. |
| `Vec.flipToPointUp`, `UnitVec.flipToPointUp` | `Vec.orientUp`, `UnitVec.orientUp` | `orientUp` has the partner `orientDown`. |
| `UnitVc.withLength`, `UnitVec.withLength` | `UnitVc.scale`, `UnitVec.scale` | On a unit vector the two are the same. Or the other way round. |
| `isParallelWithin`, `isNotParallelWithin`, `isAngleBelow`, `isAngleAbove` on `Vc`, `Vec`, `UnitVc`, `UnitVec` | `isParallelTo`, `isParallelAndOrientedTo` | The same tests under a second name, written out a second time. Compare the bodies before removing, the checks for short vectors differ slightly. |

### Other types,

| Remove | Keep | Note |
|--------|------|------|
| `PPlane.translateBy` | `PPlane.move` | `move` is the only translation verb. |
| `NPlane.areCoincident` | `NPlane.isCoincidentTo`, `IsCoincidentTo` | `areCoincident a b` is `a.IsCoincidentTo b` with the default tolerances. |
| `NPlane.angleTo` | `NPlane.angle90ToPlane` | |


## 2. Helpers that are public only for inlining, about 55 rows

These are not geometry. They are public because inline members call them.
Hide them from IntelliSense and from the API tables, instead of deleting them.

- The auto-opened `ResizeArray` extension members: `First`, `Second`, `Third`, `Last`, `SecondLast`, `ThirdLast`, `LastIndex`, `IsEmpty`, `Pop`, `PopAt`, `GetLooped`, `SetIdx`.
  With `open Euclid` they appear on every `ResizeArray` in the user's code.
  Their module is hidden from IntelliSense, but the members still apply to every `ResizeArray`.
  Calling the functions of the `ResizeArr` module inside Euclid would avoid the extension members.
- The modules `EuclidCollectionUtilities.Arr`, `ResizeArr` and `IList`.
- The helpers of `EuclidErrors` that serve one type only: `failRarr`, `failQuat`, `failRot`, `failRect2DOffsetEdge`, `failRect3DOffsetEdge`, `failTooFewPoly2D`, `failTooFewPoly3D`, `failObsoleteV20`.

