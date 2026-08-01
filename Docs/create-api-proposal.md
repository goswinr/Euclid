# `create...` API unification proposal

## Status

This document is a design proposal. It does not describe an API that has already
been implemented.

The audit covers all 204 source declarations whose static-member name starts
with `create`, including overloads and obsolete compatibility members.

## Goals

- Give every `create...` name exactly one signature. Creation functions must
  never rely on overload resolution.
- Prefer curried F# APIs when the arguments have distinct types.
- Keep calls visually grouped when exchanging two arguments of the same type
  would still compile but silently change the result.
- Preserve tupled unchecked construction.
- Use the same names for equivalent 2D and 3D operations.
- Make the input representation visible in the name when it previously selected
  an overload.

## Signature rules

The following rules are mechanical and are applied in order:

1. **No overloads.** A type may expose only one member with a given
   `create...` name.
2. **Every `createUnchecked...` member stays tupled.** This includes
   `createUncheckedVec` as well as `createUnchecked`.
3. **Repeated argument types stay tupled.** If a type occurs more than once in
   the signature, all arguments stay in one tupled group. This makes ordered
   inputs such as `(fromVec, toVec)`, `(origin, xPoint, yPoint)`, and
   `(minX, minY, maxX, maxY)` visibly inseparable.
4. **Otherwise, multi-argument members are curried.** For example:
   `NPlane.createOriginNormal origin normal` and
   `Line3D.createFromLine2DWithZ zLevel line`.
5. **Unary members are unchanged semantically.** A unary method already has one
   argument group, whether its declaration spells the input as `value` or
   `(value)`.
6. **Obsolete creation members are removed** during the breaking change instead
   of being restyled or renamed.

Rule 3 uses the declared static types, not parameter names. For example,
`(center:Pt, sizeX:float, sizeY:float)` stays tupled because `float` occurs
twice, while `(center:Pt, xAxis:Vc, sizeY:float)` becomes curried because its
three types are distinct.

### Examples

```fsharp
// Distinct argument types: curried
NPlane.createOriginNormal origin normal
Rect2D.createFromCenterAndVector center xAxis sizeY
Line3D.createFromLine2DWithZ zLevel line

// A type occurs more than once: tupled
Vec.createFromPoints(startPoint, endPoint)
NPlane.createFrom3Points(origin, xPoint, yPoint)
Rect3D.createFromPlane(plane, sizeX, sizeY)

// Unchecked construction: always tupled
UnitVec.createUnchecked(x, y, z)
Box.createUncheckedVec(origin, xAxis, yAxis, zAxis)
```

## Naming rules

| Concern | Rule | Examples |
|---|---|---|
| Raw 2D/3D coordinates | Use `XY` and `XYZ` suffixes | `Vc.createXY`, `Vec.createXYZ` |
| Unit-vector input variant | Append the exact unit type to the overloaded base name | `createRotationAxisUnitVec`, `createFromDegreesUnitVec` |
| Ordered point pair | Use `createFromPoints` | `Vec.createFromPoints`, `UnitVc.createFromPoints` |
| Ordered vector mapping | Name both endpoint types explicitly | `createVecToVec`, `createUnitVcToUnitVc` |
| Objects exposing coordinate members | Use `createFromMembersXY` or `createFromMembersXYZ` | Same spelling on points, vectors, and polylines |
| Point collections | Spell `Points`, not `Pts` | `Polyline2D.createFromPoints` |
| Numbered point factories | Use digits consistently | `createFrom3Points`, `createFrom4Points2D`, `createFrom8Points` |
| 2D vector type | Say `Vc`, not `Vec` | `Line2D.createFromVc`, `UnitVc.createFromVc` |
| Collection implementation | Retain established `Seq` and `IList` suffixes where both exist | `BBox.createFromSeq`, `BBox.createFromIList` |
| Compatibility spellings | Remove them in the breaking release | `createFromColumMajorArray`, `createFromDegree` |

For a former `Vec`/`UnitVec` overload pair, the general-vector variant uses the
semantic base name and the unit-vector variant gains a `UnitVec` suffix. The
same rule applies to `Vc`/`UnitVc`. Existing base names are retained unless a
counterpart name is clearer: the deliberate exceptions are the plane normal
factories and `Rotation2D` vector mappings.

## Overload removal

There are 21 overloaded name groups containing 42 declarations.

| Type | Current overloaded name | Proposed unique APIs | Final shape |
|---|---|---|---|
| `Vec` | `create` | `createXYZ(x, y, z)`; `createFromPoints(start, end)` | Tupled; tupled |
| `Vc` | `create` | `createXY(x, y)`; `createFromPoints(start, end)` | Tupled; tupled |
| `UnitVec` | `create` | `createXYZ(x, y, z)`; `createFromPoints(fromPoint, toPoint)` | Tupled; tupled |
| `UnitVc` | `create` | `createXY(x, y)`; `createFromPoints(fromPoint, toPoint)` | Tupled; tupled |
| `NPlane` | `create` | `createOriginNormal origin normal`; `createOriginNormalUnitVec origin normal` | Curried; curried |
| `Matrix` | `createTranslation` | `createTranslationXYZ(x, y, z)`; `createTranslation vector` | Tupled; unary |
| `Matrix` | `createRotationAxis` | `createRotationAxis axis angle`; `createRotationAxisUnitVec axis angle` | Curried; curried |
| `Matrix` | `createRotationAxisCenter` | `createRotationAxisCenter axis center angle`; `createRotationAxisCenterUnitVec axis center angle` | Curried; curried |
| `Matrix` | `createVecToVec` | `createVecToVec(fromVec, toVec)`; `createUnitVecToUnitVec(fromVec, toVec)` | Tupled; tupled |
| `RigidMatrix` | `createTranslation` | `createTranslationXYZ(x, y, z)`; `createTranslation vector` | Tupled; unary |
| `RigidMatrix` | `createRotationAxis` | `createRotationAxis axis angle`; `createRotationAxisUnitVec axis angle` | Curried; curried |
| `RigidMatrix` | `createRotationAxisCenter` | `createRotationAxisCenter axis center angle`; `createRotationAxisCenterUnitVec axis center angle` | Curried; curried |
| `RigidMatrix` | `createVecToVec` | `createVecToVec(fromVec, toVec)`; `createUnitVecToUnitVec(fromVec, toVec)` | Tupled; tupled |
| `Quaternion` | `createFromRadians` | `createFromRadians axis radians`; `createFromRadiansUnitVec axis radians` | Curried; curried |
| `Quaternion` | `createFromDegrees` | `createFromDegrees axis degrees`; `createFromDegreesUnitVec axis degrees` | Curried; curried |
| `Quaternion` | `createFromDegree` | Remove both obsolete overloads | Removed |
| `Quaternion` | `createVecToVec` | `createVecToVec(fromVec, toVec)`; `createUnitVecToUnitVec(fromVec, toVec)` | Tupled; tupled |
| `Rotation2D` | `createFromVectors` | `createVcToVc(fromVec, toVec)`; `createUnitVcToUnitVc(fromVec, toVec)` | Tupled; tupled |
| `PPlane` | `createOriginXaxisYaxis` | `createOriginXaxisYaxis(origin, xAxis, yAxis)`; `createOriginXaxisYaxisUnitVec(origin, xAxis, yAxis)` | Tupled; tupled |
| `PPlane` | `createOriginNormal` | `createOriginNormal origin normal`; `createOriginNormalUnitVec origin normal` | Curried; curried |
| `PPlane` | `createOriginNormalXaxis` | `createOriginNormalXaxis(origin, normal, xAxis)`; `createOriginNormalXaxisUnitVec(origin, normal, xAxis)` | Tupled; tupled |

## Complete type-by-type proposal

The tables below include every current declaration. Multiple unary members with
the same outcome are grouped into a single row for readability.

### `BBox`

| Current | Proposed | Change |
|---|---|---|
| `createUnchecked(minX, minY, minZ, maxX, maxY, maxZ)` | Same | Unchecked stays tupled |
| `create(a, b)` | Same | `Pnt` is repeated; stays tupled |
| `createFromSeq ps` | Same | Unary |
| `createFromIList ps` | Same | Unary |
| `createFromCenter(center, sizeX, sizeY, sizeZ)` | Same | `float` is repeated; stays tupled |
| `createFromLine line` | Same | Unary |
| `createFromBRect minZ maxZ rect` | `createFromBRect(rect, minZ, maxZ)` | Becomes tupled; source-first order matches other `createFrom...` conversions |
| `createFromRect3D rect` | Same | Unary |
| `createFromBox box` | Same | Unary |

### `BRect`

| Current | Proposed | Change |
|---|---|---|
| `createUnchecked(minX, minY, maxX, maxY)` | Same | Unchecked stays tupled |
| `create(a, b)` | Same | `Pt` is repeated; stays tupled |
| `createXY(ax, ay, bx, by)` | Same | `float` is repeated; stays tupled |
| `createFromSeq ps`; `createFromIList ps` | Same | Unary |
| `createFromCenter(center, sizeX, sizeY)` | Same | `float` is repeated; stays tupled |
| `createFromLine line`; `createFromRect2D rect`; `createFromRect3D rect` | Same | Unary |

### `Box`

| Current | Proposed | Change |
|---|---|---|
| `createUnchecked(...)` | Same | Unchecked stays tupled |
| `createUncheckedVec(origin, xAxis, yAxis, zAxis)` | Same | Unchecked stays tupled; `Vec` is also repeated |
| `create(origin, xAxis, yAxis, zAxis)` | Same | `Vec` is repeated; stays tupled |
| `createFromPlane x y z plane` | `createFromPlane(plane, sizeX, sizeY, sizeZ)` | Becomes tupled because `float` repeats; source-first order and parameter names align with `Rect3D.createFromPlane` |
| `createFromBounds(minX, minY, minZ, maxX, maxY, maxZ)` | Same | `float` is repeated; stays tupled |
| `createFromRect2D zLow zHigh rect` | `createFromRect2D(rect, zLow, zHigh)` | Becomes tupled; source-first order |
| `createFromRect3D zLow zHigh rect` | `createFromRect3D(rect, zLow, zHigh)` | Becomes tupled; source-first order |
| `createFromPlaneAndPoints plane points` | Same | Types are distinct; remains curried |
| `createFromDirsAndPoints dirX dirY points` | `createFromDirsAndPoints(dirX, dirY, points)` | `Vec` is repeated; becomes tupled |
| `createFromBoundingBox(...)` | Remove | Obsolete compatibility member |

### `FreeBox`

| Current | Proposed | Change |
|---|---|---|
| `createFromEightPoints points` | `createFrom8Points points` | Numeric naming aligned with `createFrom3Points` |
| `createFromBox box` | Same | Unary |
| `createFromFour2DPoints zMin zMax points` | `createFrom4Points2D(points, zMin, zMax)` | Numeric naming, source-first order, and tupled because `float` repeats |
| `createFromFour2DPointsArgs(a, b, c, d, zMin, zMax)` | `createFrom4Points2DCCW(a, b, c, d, zMin, zMax)` | Removes non-semantic `Args`; exposes documented ordering; stays tupled |

### `Line2D`

| Current | Proposed | Change |
|---|---|---|
| `createFromLine3D line` | Same | Unary |
| `createFromVec vector` | `createFromVc vector` | Correct 2D type name |
| `createFromPtAndVc(point, vector)` | `createFromPtAndVc point vector` | Distinct types; becomes curried |

### `Line3D`

| Current | Proposed | Change |
|---|---|---|
| `createFromLine2D line` | Same | Unary |
| `createFromLine2DWithZ zLevel line` | Same | Already curried and types are distinct |
| `createFromVec vector` | Same | Unary |
| `createFromPntAndVec(point, vector)` | `createFromPntAndVec point vector` | Distinct types; becomes curried |

### `Matrix`

| Current | Proposed | Change |
|---|---|---|
| `createTranslation(x, y, z)` | `createTranslationXYZ(x, y, z)` | Removes overload; repeated `float` stays tupled |
| `createTranslation vector` | Same | Unary; no longer overloaded |
| `createRotationX angle`; `createRotationY angle`; `createRotationZ angle` | Same | Unary |
| `createRotationAxis(axis:Vec, angle)` | `createRotationAxis axis angle` | Distinct types; becomes curried |
| `createRotationAxis(axis:UnitVec, angle)` | `createRotationAxisUnitVec axis angle` | Removes overload; distinct types become curried |
| `createRotationAxisCenter(axis:Vec, center, angle)` | `createRotationAxisCenter axis center angle` | All types distinct; becomes curried |
| `createRotationAxisCenter(axis:UnitVec, center, angle)` | `createRotationAxisCenterUnitVec axis center angle` | Removes overload; becomes curried |
| `createScale(x, y, z)` | Same | `float` is repeated; stays tupled |
| `createVecToVec(fromVec, toVec)` using `Vec` | Same | `Vec` is repeated; stays tupled |
| `createVecToVec(fromVec, toVec)` using `UnitVec` | `createUnitVecToUnitVec(fromVec, toVec)` | Removes overload; `UnitVec` is repeated |
| `createShear(xy, xz, yx, yz, zx, zy)` | Same | `float` is repeated; stays tupled |
| `createToPlane plane` | Same | Unary |
| `createPlaneToPlane(fromPlane, toPlane)` | Same | `PPlane` is repeated; stays tupled |
| `createMirror plane` | Same | Unary |
| `createFromQuaternion quaternion` | Same | Unary |
| `createFromColumnMajorArray values` | Same | Unary |
| `createFromColumMajorArray values` | Remove | Obsolete misspelling |
| `createFromRowMajorArray values` | Same | Unary |
| `createFromRigidMatrix matrix` | Same | Unary |
| `createPerspective(width, height, nearDistance, farDistance)` | Same | `float` is repeated; stays tupled |

### `NPlane`

| Current | Proposed | Change |
|---|---|---|
| `createUnchecked(...)`; `createUncheckedVec(...)` | Same | Unchecked stays tupled |
| `create(origin, normal:Vec)` | `createOriginNormal origin normal` | Aligns with `PPlane`; distinct types become curried |
| `create(origin, normal:UnitVec)` | `createOriginNormalUnitVec origin normal` | Removes overload, aligns with `PPlane`, and becomes curried |
| `createFrom3Points a b c` | `createFrom3Points(a, b, c)` | `Pnt` is repeated; becomes tupled |
| `createFromPPlane plane` | Same | Unary |

### `PPlane`

| Current | Proposed | Change |
|---|---|---|
| `createUnchecked(...)`; `createUncheckedVec(...)` | Same | Unchecked stays tupled |
| `create(origin, xAxis, yAxis, zAxis)` | Same | `UnitVec` is repeated; stays tupled |
| `createThreePoints origin xPoint yPoint` | `createFrom3Points(origin, xPoint, yPoint)` | Aligns naming with `NPlane`, `Rect2D`, and `Rect3D`; repeated `Pnt` becomes tupled |
| `createOriginXaxisYaxis(...)` using `Vec` | Same | `Vec` is repeated; stays tupled |
| `createOriginXaxisYaxis(...)` using `UnitVec` | `createOriginXaxisYaxisUnitVec(...)` | Removes overload; stays tupled |
| `createOriginNormal(origin, normal:Vec)` | `createOriginNormal origin normal` | Distinct types; becomes curried |
| `createOriginNormal(origin, normal:UnitVec)` | `createOriginNormalUnitVec origin normal` | Removes overload; becomes curried |
| `createOriginNormalXaxis(...)` using `Vec` | Same | `Vec` is repeated; stays tupled |
| `createOriginNormalXaxis(...)` using `UnitVec` | `createOriginNormalXaxisUnitVec(...)` | Removes overload; stays tupled |

### `Quaternion`

| Current | Proposed | Change |
|---|---|---|
| `createUnchecked(x, y, z, w)` | Same | Unchecked stays tupled |
| `create(x, y, z, w)` | Same | `float` is repeated; stays tupled |
| `createFromRadians(axis:Vec, radians)` | `createFromRadians axis radians` | Distinct types; becomes curried |
| `createFromRadians(axis:UnitVec, radians)` | `createFromRadiansUnitVec axis radians` | Removes overload; becomes curried |
| `createFromDegrees(axis:Vec, degrees)` | `createFromDegrees axis degrees` | Distinct types; becomes curried |
| `createFromDegrees(axis:UnitVec, degrees)` | `createFromDegreesUnitVec axis degrees` | Removes overload; becomes curried |
| Both `createFromDegree` members | Remove | Obsolete singular spelling |
| `createVecToVec(fromVec, toVec)` using `Vec` | Same | `Vec` is repeated; stays tupled |
| `createVecToVec(fromVec, toVec)` using `UnitVec` | `createUnitVecToUnitVec(fromVec, toVec)` | Removes overload; stays tupled |
| `createFromEulerXYZ(...)`; `createFromEulerYXZ(...)`; `createFromEulerZXY(...)`; `createFromEulerZYX(...)`; `createFromEulerYZX(...)`; `createFromEulerXZY(...)` | Same | Each repeats `float`; stays tupled |
| `createFromRigidMatrix matrix` | Same | Unary |

### `Rect2D`

| Current | Proposed | Change |
|---|---|---|
| `createUnchecked(...)`; `createUncheckedVec(...)` | Same | Unchecked stays tupled |
| `createFromVectors(origin, xAxis, yAxis)` | Same | `Vc` is repeated; stays tupled |
| `createFromXVectorAndWidth(origin, xAxis, sizeY)` | `createFromXVectorAndWidth origin xAxis sizeY` | All types distinct; becomes curried |
| `createFromDirectionAndSizes(origin, direction, sizeX, sizeY)` | Same | `float` is repeated; stays tupled |
| `createFromLine(line, offRight, offLeft)` | Same | `float` is repeated; stays tupled |
| `createFromBounds(minX, minY, maxX, maxY)` | Same | `float` is repeated; stays tupled |
| `createFromCenterAndDirection(center, direction, sizeX, sizeY)` | Same | `float` is repeated; stays tupled |
| `createFromCenterAndVector(center, xAxis, sizeY)` | `createFromCenterAndVector center xAxis sizeY` | All types distinct; becomes curried |
| `createFrom3Points(origin, xPoint, yPoint)` | Same | `Pt` is repeated; stays tupled |
| `createFromDirAndPoints direction points` | Same | Already curried and types are distinct |
| `createFromBRect(...)` | Remove | Obsolete compatibility member |

### `Rect3D`

| Current | Proposed | Change |
|---|---|---|
| `createUnchecked(...)`; `createUncheckedVec(...)` | Same | Unchecked stays tupled |
| `createFromVectors(origin, xAxis, yAxis)` | Same | `Vec` is repeated; stays tupled |
| `createFromPlane(plane, sizeX, sizeY)` | Same | `float` is repeated; stays tupled |
| `createCenteredFromPlane(plane, sizeX, sizeY)` | Same | `float` is repeated; stays tupled |
| `createFromBounds2D(minX, minY, maxX, maxY)` | Same | `float` is repeated; stays tupled |
| `createFrom3Points(origin, xPoint, yPoint)` | Same | `Pnt` is repeated; stays tupled |
| `createFromBRect(...)` | Remove | Obsolete compatibility member |

### `RigidMatrix`

| Current | Proposed | Change |
|---|---|---|
| `createTranslation(x, y, z)` | `createTranslationXYZ(x, y, z)` | Removes overload; repeated `float` stays tupled |
| `createTranslation vector` | Same | Unary; no longer overloaded |
| `createTranslationX value`; `createTranslationY value`; `createTranslationZ value` | Same | Unary |
| `createRotationX angle`; `createRotationY angle`; `createRotationZ angle` | Same | Unary |
| `createRotationAxis(axis:Vec, angle)` | `createRotationAxis axis angle` | Distinct types; becomes curried |
| `createRotationAxis(axis:UnitVec, angle)` | `createRotationAxisUnitVec axis angle` | Removes overload; becomes curried |
| `createRotationAxisCenter(axis:Vec, center, angle)` | `createRotationAxisCenter axis center angle` | All types distinct; becomes curried |
| `createRotationAxisCenter(axis:UnitVec, center, angle)` | `createRotationAxisCenterUnitVec axis center angle` | Removes overload; becomes curried |
| `createVecToVec(fromVec, toVec)` using `Vec` | Same | `Vec` is repeated; stays tupled |
| `createVecToVec(fromVec, toVec)` using `UnitVec` | `createUnitVecToUnitVec(fromVec, toVec)` | Removes overload; stays tupled |
| `createToPlane plane` | Same | Unary |
| `createPlaneToPlane(fromPlane, toPlane)` | Same | `PPlane` is repeated; stays tupled |
| `create(...)` | Same | Matrix-field `float` values repeat; stays tupled |
| `createFromMatrix matrix`; `createFromQuaternion quaternion` | Same | Unary |

### `Rotation2D`

| Current | Proposed | Change |
|---|---|---|
| `createUnchecked(sine, cosine)` | Same | Unchecked stays tupled |
| `create(sine, cosine)` | Same | `float` is repeated; stays tupled |
| `createFromRadians angle`; `createFromDegrees angle`; `createFromCosine value`; `createFromSine value` | Same | Unary |
| `createFromVectors(fromVec, toVec)` using `Vc` | `createVcToVc(fromVec, toVec)` | Removes overload and matches 3D `createVecToVec`; stays tupled |
| `createFromVectors(fromVec, toVec)` using `UnitVc` | `createUnitVcToUnitVc(fromVec, toVec)` | Removes overload and matches 3D `createUnitVecToUnitVec`; stays tupled |

### `Pt` and `Pnt`

| Type | Current | Proposed | Change |
|---|---|---|---|
| Both | `createFromMembersXY/XYZ value`; lowercase-member counterpart | Same | Unary; casing distinguishes source member names rather than overloads |
| `Pt` | `createFromPnt point`; `createFromVc vector`; `createFromUnitVc vector` | Same | Unary |
| `Pnt` | `createFromPt point`; `createFromVec vector`; `createFromUnitVec vector` | Same | Unary |
| `Pnt` | `createFromPtWithZ z point` | Same | Already curried and types are distinct |
| `Pt` | `create(x, y)` | Remove | Obsolete; use the constructor |
| `Pnt` | `create(x, y, z)` | Remove | Obsolete; use the constructor |

### `Vc`, `Vec`, `UnitVc`, and `UnitVec`

| Type | Current | Proposed | Change |
|---|---|---|---|
| `Vc` | `create(x, y)` | `createXY(x, y)` | Removes overload; `float` repeats, so tupled |
| `Vc` | `create(start, end)` | `createFromPoints(start, end)` | Removes overload; `Pt` repeats, so tupled |
| `Vec` | `create(x, y, z)` | `createXYZ(x, y, z)` | Removes overload; `float` repeats, so tupled |
| `Vec` | `create(start, end)` | `createFromPoints(start, end)` | Removes overload; `Pnt` repeats, so tupled |
| `UnitVc` | `create(x, y)` | `createXY(x, y)` | Removes overload; `float` repeats, so tupled |
| `UnitVc` | `create(fromPoint, toPoint)` | `createFromPoints(fromPoint, toPoint)` | Removes overload; `Pt` repeats, so tupled |
| `UnitVec` | `create(x, y, z)` | `createXYZ(x, y, z)` | Removes overload; `float` repeats, so tupled |
| `UnitVec` | `create(fromPoint, toPoint)` | `createFromPoints(fromPoint, toPoint)` | Removes overload; `Pnt` repeats, so tupled |
| `UnitVc` | `createUnchecked(x, y)`; `createFor(message, x, y)` | Same | Unchecked stays tupled; `createFor` repeats `float` |
| `UnitVec` | `createUnchecked(x, y, z)`; `createFor(message, x, y, z)` | Same | Unchecked stays tupled; `createFor` repeats `float` |
| All four | `createFromMembersXY/XYZ value`; lowercase-member counterpart | Same | Unary |
| `Vc` | `createFromPnt point`; `createFromUnitVc vector` | Same | Unary |
| `Vec` | `createFromPnt point`; `createFromUnitVec vector` | Same | Unary |
| `UnitVc` | `createFromPt point` | Same | Unary |
| `UnitVc` | `createFromVec vector` | `createFromVc vector` | Correct 2D type name |
| `UnitVec` | `createFromPnt point`; `createFromVec vector` | Same | Unary |
| `Vec` | `createFromVcWithZ z vector` | Same | Already curried and types are distinct |

### `Polyline2D`

| Current | Proposed | Change |
|---|---|---|
| `createFromXYMembers values` | `createFromMembersXY values` | Aligns with `Pt`, `Vc`, and `UnitVc` |
| `createFromxyMembers values` | `createFromMembersxy values` | Aligns lowercase-member spelling |
| `createFromPts points` | `createFromPoints points` | Expands `Pts` consistently |
| `createDirectly coordinates`; `createEmpty capacity` | Same | Unary |
| `createFromRectCCW rect`; `createFromRectCW rect`; `createFromBRectCCW bounds`; `createFromBRectCW bounds` | Same | Unary and consistently paired |
| `createDirectlyUnsafe points` | Remove | Obsolete compatibility member |
| `create points` | Remove | Obsolete alias for `createFromPts` |

### `Polyline3D`

| Current | Proposed | Change |
|---|---|---|
| `createFromXYZMembers values` | `createFromMembersXYZ values` | Aligns with `Pnt`, `Vec`, and `UnitVec` |
| `createFromxyzMembers values` | `createFromMembersxyz values` | Aligns lowercase-member spelling |
| `createFrom2DXYMembers values` | `createFromMembersXY values` | Same 2D-member naming as `Polyline2D` |
| `createFrom2DxyMembers values` | `createFromMembersxy values` | Same lowercase 2D-member naming |
| `createFromPts points` | `createFromPoints points` | Expands `Pts` consistently |
| `createDirectly coordinates`; `createEmpty capacity`; `createFromRect3D rect` | Same | Unary |
| `createDirectlyUnsafe points` | Remove | Obsolete compatibility member |

### Removed `Loop`

| Current | Proposed | Change |
|---|---|---|
| `Loop.create()` | Remove with the obsolete `Loop` shell | Error-level obsolete compatibility type |

## Resulting 2D/3D counterpart matrix

| Concept | 2D | 3D |
|---|---|---|
| Raw vector coordinates | `Vc.createXY(x, y)` | `Vec.createXYZ(x, y, z)` |
| Raw unit-vector coordinates | `UnitVc.createXY(x, y)` | `UnitVec.createXYZ(x, y, z)` |
| Vector from points | `Vc.createFromPoints(start, end)` | `Vec.createFromPoints(start, end)` |
| Unit vector from points | `UnitVc.createFromPoints(fromPoint, toPoint)` | `UnitVec.createFromPoints(fromPoint, toPoint)` |
| Objects with coordinate members | `createFromMembersXY` / `createFromMembersxy` | `createFromMembersXYZ` / `createFromMembersxyz` |
| Polyline from points | `Polyline2D.createFromPoints` | `Polyline3D.createFromPoints` |
| Line from vector | `Line2D.createFromVc` | `Line3D.createFromVec` |
| Line from point and vector | `Line2D.createFromPtAndVc` | `Line3D.createFromPntAndVec` |
| Checked plane from general normal | — | `NPlane.createOriginNormal`; `PPlane.createOriginNormal` |
| Checked plane from unit normal | — | `NPlane.createOriginNormalUnitVec`; `PPlane.createOriginNormalUnitVec` |

## Compatibility and migration

This proposal is a breaking F# source change:

- Tupled-to-curried calls change from `Type.member(a, b)` to
  `Type.member a b`.
- Curried-to-tupled calls change in the opposite direction.
- Renamed overloads require explicit representation names.
- Reordering source-first tupled conversions affects
  `BBox.createFromBRect`, `Box.createFromPlane`,
  `Box.createFromRect2D`, and `Box.createFromRect3D`.
- Packed Fable sources and README samples must be updated together with the
  library.

Curried static members still compile to ordinary flattened CLR method
parameters. Consequently, the largest compatibility impact is F# source and
F# argument-group metadata. Nevertheless, the rename and parameter-order
changes are ordinary CLR API breaks too and require a major-version-style
migration.

The old and new overload forms generally cannot coexist under the same source
name because their flattened CLR signatures collide. If a compatibility period
is required, differently named obsolete forwarding members are needed.

## Suggested implementation order

1. Remove obsolete creation members and their tests.
2. Rename every overloaded group and update internal calls.
3. Normalize counterpart names (`XY`/`XYZ`, `Vc`/`Vec`, `Points`, and member
   factories).
4. Apply the tupled/curried rules mechanically.
5. Update `TestAPI.fs`, behavioral tests, README samples, XML documentation, and
   changelog entries.
6. Build both target frameworks and run the complete test suite.
7. Inspect the compiled assembly to confirm that no `create...` name is
   overloaded and that only the intended methods carry curried argument-group
   metadata.
