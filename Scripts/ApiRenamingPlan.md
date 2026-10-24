# Euclid API renaming plan

Date: 2026-10-20, revised 2026-10-21. Baseline: commit `4c58454`.

> **Superseded names.** Later renames of 2026-10-23 replace some names in this plan. The optional-parameter forms end in `Custom` instead of `With`. The shared buffers are `XYs` / `XYZs`, built with `createFromXYs` / `createFromXYZs`. The indexed point accessors are `GetPoint`, `SetPoint`, `SetXY`, `MovePoint` and so on, with `…PreservingClosure` variants. `Box.offset` is curried. [Docs/Migration.md](../Docs/Migration.md) and `ApiMigration.tsv` list the current names.

This plan defines a breaking API migration for Euclid, starting with factory names and currying and continuing through the other suitable static operations. The intended result is a predictable F# API with explicit overload names, useful partial application, symmetric 2D/3D names, and visible mutation.

The migration preserves geometric calculations, floating-point evaluation order, validation thresholds, return types, collection ownership, and mutation behavior. Changing those contracts is separate work. This file is a plan; it does not implement the API changes.

## Decisions

The user selected these policies:

| Subject | Decision |
|:--|:--|
| Scope | Apply currying to all suitable static operations, in phases. |
| Compatibility | Remove replaced names and provide a migration map. Do not retain obsolete compatibility aliases for replaced names. |
| Translation vocabulary | Use `move` / `Move`. Remove equivalent `translate` aliases. |
| Interpolation | Use `lerp start finish factor`, and the same order for `slerp`. |
| Dot product | Use curried static methods with distinct mixed-type names. |
| Cross product | Keep tupled static methods and their overloads; operand order is significant. |
| Other ordered algebra | Keep matrix/quaternion multiplication and multi-argument determinants tupled. Add `Then` / `andThen` for consistent transformation composition. |
| Same-type arguments | More than three arguments of the same type use a tupled form, including members that are currently curried. |
| Optional parameters | The plain name is a curried default. A name ending in `With` is always tupled: on types it keeps the current optional parameters; in the `Offset2D` / `Offset3D` modules it takes every setting explicitly. No options records. |
| Extra inputs | Offset variants with an extra geometric input use `By<Noun>`: `offsetByNormal`, `offsetByDirections`. Other `With<Noun>` names keep their plain-English meaning, such as `createFromPtWithZ` and `intersectTriaVectorsWithRay`. |
| Mutation naming | No `InPlace` suffix. Only `Polyline2D`, `Polyline3D` and `FreeBox` have mutable geometry instances. On them, instance transforms mutate and the corresponding statics return a copy. Indexed point setters/movers (`setPt...`, `movePt...`) and append operations (`add...`) mutate and return `unit`; indexed getters only observe. Component-replacement operations returning a copy use `with...`. |
| `Pt` / `Pnt` spelling | Applies to factory names and to names newly created by overload splits. Other existing names keep `Point` in this migration. |
| `Unit` prefix | A leading `Unit` in a factory name means every vector input is a unit vector, as in `createFromUnitNormalAndXAxis`. |
| Rotation factories | `From` names the axis or vector inputs; `Around` replaces it when a center point is given. `Matrix` mirrors `Quaternion`: `createRotationFromVecDegrees` next to `Quaternion.createFromVecDegrees`. |
| Centered factories | Use `createCentered...` on every type, including `BRect` and `BBox`. |
| Line from point and vector | The point comes first: `createFromPtAndVc point vector`, matching the XLine `FromPtsAndVcs` forms. |
| Unchecked vector factories | Rename the five `createUncheckedVec` factories so that `Unchecked` comes last. They stay tupled and keep their argument order. |
| `Radians` factories | Add a `Radians` factory next to every `Degrees` factory that lacks one. |

Continue with the current type and extension-member organization. A companion-module redesign is not required for currying. Keep core structs minimal and respect F# compile order.

Use this file as the proposed rename specification. The selected policies above are settled. The tables below supply concrete names and migration rules; verify each against the implementation when its phase is executed.

## Naming and argument rules

- Static operations use camelCase; instance operations and properties use PascalCase.
- Factory names retain `create`, `createFrom...`, and `create...Unchecked`. Existing one-argument factories stay one-argument functions.
- Multi-argument factories become fully curried, including raw scalar factories, unless a [currying exception](#currying-exceptions) applies. Ordinary `new` constructors retain their tupled signatures.
- Curried static methods have one unambiguous signature per name. Split overloads before currying. Do not use SRTP dispatch, boxing, or runtime type tests merely to preserve an overloaded spelling.
- In factory names, and in names newly created by splitting an overload, spell named point and vector inputs consistently as `Pt` / `Pnt`, `Pts` / `Pnts`, `Vc` / `Vec`, and `Vcs` / `Vecs`. Preserve geometric nouns such as `Plane`, `Normal`, `Axis`, and `Center` where they describe the operation better. Rotation factories that take an axis vector say `Vec` / `UnitVec`, so their overloads split by type. Other existing member names keep `Point` in this migration, even when the plan renames them for another reason, as in `closestPointIndex`; see the follow-up work.
- A leading `Unit` covers every vector input of a factory: `createFromUnitNormalAndXAxis` takes two `UnitVec`s.
- Use `Degrees` and `Radians` in names that accept raw rotation angles. Keep dimension-specific suffixes such as `InXY`, and preserve the specified Euler axis order.
- Put failure-contract suffixes at the end: `Unchecked`, `OrDefault`. Use `try...` only for `voption` results and `classify...` for relationship discriminated unions.
- Compose a name in this order: operation, object or input type, `Sq`, `Degrees` / `Radians`, representation suffix such as `FromPtsAndVcs`, `With`, and finally `Unchecked` / `OrDefault`. Examples: `distanceToLineSq`, `createRotationAroundVecDegrees`, `distanceSqFromPntsAndVecs`, `tryIntersectRayWith`, `createFromAxesUnchecked`.
- Spell out abbreviations in member names: `Dist` becomes `Distance`, `Idx` becomes `Index`, `Var` becomes `Variable`, `Deg` becomes `Degrees`, `ErrMsg` becomes `ErrorMessage`. Parameter names of tupled `With` forms and type names such as `VarDistParallel` keep their spelling, so named arguments keep working.
- Put configuration before source geometry: sizes before a plane, offsets before a line, and tolerances before inputs.
- Keep coordinate and peer order natural: `x y z`, `start finish`, `a b c`, and `from to`. Interpolation explicitly keeps the factor last.
- For frame construction from vectors, use directions or edge vectors followed by the origin. For construction from three points, preserve `origin xPoint yPoint`. A line from a point and a vector keeps the point first, like the XLine `FromPtsAndVcs` forms.
- A static member with optional parameters gets a curried default under its plain name and a tupled `...With` form. See [Optional parameters](#optional-parameters).
- A trailing `With` always marks a tupled form with settings. An extra geometric input in an offset variant is named `By<Noun>`, following the existing `offsetPtVarByNormals` (renamed `offsetPtVariableByNormals`). Other `With<Noun>` names, such as `createFromPtWithZ`, keep their plain-English meaning.
- `set...` and append-style `add...` / `Add...` names mean mutation, as do indexed point movers such as `movePt`. Taking a point index alone does not imply mutation. Component-replacement operations that return a copy use `with...`; transforms keep names such as `move`, `rotate`, `scale` and `reverse`. See [Mutation naming](#mutation-naming).

### Currying exceptions

- Functions with more than three arguments of the same type use a tupled form. Convert already-curried members covered by this rule to a single tupled argument group, preserving their parameter order; existing tupled forms keep their order too. Count types as written in the signature: `float` and `float<Cosine.cosine>` are different types, and so are `Vec` and `UnitVec`, because swapping them does not compile.
- `create...Unchecked` remains tupled.
- A name ending in `With` is tupled. This covers the optional-parameter forms next to a curried default and the explicit-settings forms in `Offset2D` / `Offset3D`. See [Optional parameters](#optional-parameters).
- The `...ByDirections` kernels in `Offset2D` / `Offset3D` stay tupled, like the raw `XLineXY` helpers.
- Constructors, operators, property accessors, overrides, and interface-required signatures retain the forms required by F# and .NET.
- `cross` and its overloads remain tupled, as selected by the user.
- Keep the existing tupled `Quaternion.multiply`, `Matrix.multiply`, and `RigidMatrix.multiply` while their different application-order contracts remain in place. The composition section provides a consistent user-facing alternative.
- Keep alternating triple products such as `determinant3` tupled alongside `cross`; preserve their operand order.
- Do not rename compiler-recognized members such as `Zero` and `DivideByInt`.
- A tuple representing actual data remains a tuple. Do not split tuples inside callbacks, tuple-valued inputs, tuple return values, or discriminated-union cases.
- Other public static operations are curried unless a concrete language or interoperability constraint is recorded.

For example, `Box.offset distBottom distFront distRight distBack distLeft distTop b` becomes the following tupled signature because it takes six `float` arguments:

```fsharp
static member offset(distBottom:float, distFront:float, distRight:float, distBack:float, distLeft:float, distTop:float, b:Box) : Box
```

Keep the face-distance associations and result unchanged. `FreeBox.createFrom4Pts` is another curried-to-tupled conversion, listed under [Polyline and FreeBox factories](#polyline-and-freebox-factories). The four-bound XLine operations remain tupled; their exact target signatures are listed under [Intersection overloads and optional parameters](#intersection-overloads-and-optional-parameters).

F# does not permit overloaded curried methods, and curried methods cannot carry the existing optional-parameter forms unchanged. A curried member and a tupled member also cannot share a name on one type. Ordinary curried members compile to CLR methods with flattened parameter lists. Partial application can create closures; preserve existing `inline` choices and avoid constructing partial applications repeatedly inside numeric loops. See the [F# language specification](https://fsharp.github.io/fslang-spec/type-definitions/#8134-curried-method-members).

### Optional parameters

A static member with optional parameters gets two forms:

- The plain name is curried and applies every default.
- The `...With` name is tupled. It keeps the current parameter names, order, `[<OPT;DEF(...)>]` attributes and defaults. Tupled `With` forms may stay overloaded, for example for line inputs and for point-and-vector inputs.

A configurable call then migrates by renaming only, and named arguments keep working: `Polyline2D.offset(pl, 5.0, loop = true)` becomes `Polyline2D.offsetWith(pl, 5.0, loop = true)`. An old tupled call to the plain name fails to compile instead of silently changing behavior. The configuration-first order applies to curried forms only.

Do not introduce options records. `let`-bound module functions, such as those in `Offset2D` and `Offset3D`, cannot take optional parameters. Their `With` forms are still tupled, take every setting explicitly, and put the geometry first (see [Offset API names](#offset-api-names)).

The rule covers the XLine operations, the offsets, `Tria2D` / `Tria3D` `isLinear` and `isLinearFast`, and `Points3D.areInLine` and `areInLineFast`. Instance members with optional parameters keep their current form.

`Line2D.toLine3D` and the `Line3D` `is...Fast` predicates are already curried, and their `[<OPT;DEF(...)>]` attributes have no effect for F# callers. Keep them curried and remove those attributes.

## Factory inventory and coverage

The baseline [API table](ApiTable.md) contains 198 active `create...` signatures: 98 tupled multi-argument signatures, 14 curried signatures, and 86 one-argument signatures. It also contains four `tryCreate...` signatures: the two rectangle factories and the quaternion/rigid-matrix conversions.

There are 20 overloaded factory names:

- `Vc.create`, `Vec.create`, `UnitVc.create`, `UnitVec.create`.
- `Rotation2D.createFromVectors`.
- `Quaternion.createFromDegrees`, `createFromRadians`, `createVecToVec`.
- `PPlane.createOriginNormal`, `createOriginNormalXaxis`, `createOriginXaxisYaxis`.
- `NPlane.create`.
- On both `Matrix` and `RigidMatrix`: `createTranslation`, `createRotationAxis`, `createRotationAxisCenter`, `createVecToVec`.

The factory tables specify all name changes and meaningful argument reordering. The coverage rule for an unlisted active factory is:

- Keep its name, input types, return type, and input order.
- Convert each existing top-level parameter into its own curried argument, unless a [currying exception](#currying-exceptions) applies.
- If it already has one argument or is already curried, no signature change is needed unless a currying exception requires a tupled form.
- Apply the explicit spelling rules only where documented below.

This rule includes unchanged conversion factories such as `createFromQuaternion`, `createFromRigidMatrix`, `createFromRowMajorArray`, `createFromColumnMajorArray`, and conversions between 2D and 3D types.

In the tables, names following a type are members of that type. Parameters are shown in their final order. Paired names separated by `/` map to the 2D and 3D versions respectively. Types and result types remain those in the API table unless the row explicitly describes a new factory.

## Primitive factories

Files: [Vc extensions](../Src/TypeExtensions/Vc.fs), [Vec extensions](../Src/TypeExtensions/Vec.fs), [Pt extensions](../Src/TypeExtensions/Pt.fs), [Pnt extensions](../Src/TypeExtensions/Pnt.fs), [UnitVc](../Src/UnitVc.fs), [UnitVec](../Src/UnitVec.fs), and the matching unit-vector extension files.

| Type | Current form | Target form |
|:--|:--|:--|
| `Vc` / `Vec` | `create(x, y[, z])` | `create x y [z]` |
| `Vc` / `Vec` | `create(start, finish)` | `createFromPts start finish` / `createFromPnts start finish` |
| `UnitVc` / `UnitVec` | `create(x, y[, z])` | `create x y [z]` |
| `UnitVc` / `UnitVec` | `create(fromPoint, toPoint)` | `createFromPts fromPoint toPoint` / `createFromPnts fromPoint toPoint` |
| `UnitVc` / `UnitVec` | `createFor(message, x, y[, z])` | `createWithErrorMessage message x y [z]` |
| `UnitVc` / `UnitVec` | `createUnchecked(x, y[, z])` | Unchanged; `create...Unchecked` stays tupled |
| `Pt` / `Pnt` | Obsolete `create(x, y[, z])` | Restore as supported `create x y [z]`, replacing the obsolete definition |
| All six primitive types | `createFromMembersXY`, `createFromMembersXYZ` | Keep these spellings |
| All six primitive types | `createFromMembersxy`, `createFromMembersxyz` | Keep these spellings; lowercase source member names are intentional |

Keep `createFromPt`, `createFromPnt`, `createFromVc`, `createFromVec`, `createFromUnitVc`, `createFromUnitVec`, `createFromPtWithZ`, and `createFromVcWithZ` wherever they currently exist. Do not add every possible conversion just to fill a grid.

Unit-vector creation continues to normalize and to reject the same degenerate inputs. Unchecked creation continues to have the existing DEBUG / CHECKED_EUCLID checks. Do not move core factory definitions into later extension files if earlier compilation units require them.

## Rotation factories

Files: [Rotation2D](../Src/Rotation2D.fs), [Quaternion](../Src/Quaternion.fs), [Quaternion extensions](../Src/TypeExtensions/Quaternion.fs).

| Type | Current form | Target form |
|:--|:--|:--|
| `Rotation2D` | `createUnchecked(sine, cosine)` | Unchanged; `create...Unchecked` stays tupled |
| `Rotation2D` | `createFromVectors(a: Vc, b: Vc)` | `createFromVcs a b` |
| `Rotation2D` | `createFromVectors(a: UnitVc, b: UnitVc)` | `createFromUnitVcs a b` |
| `Quaternion` | `create(x, y, z, w)` | Unchanged; stays tupled (more than three `float` arguments) |
| `Quaternion` | `createFromDegrees(axis: Vec, degrees)` | `createFromVecDegrees axis degrees` |
| `Quaternion` | `createFromDegrees(axis: UnitVec, degrees)` | `createFromUnitVecDegrees axis degrees` |
| `Quaternion` | `createFromRadians(axis: Vec, radians)` | `createFromVecRadians axis radians` |
| `Quaternion` | `createFromRadians(axis: UnitVec, radians)` | `createFromUnitVecRadians axis radians` |
| `Quaternion` | `createVecToVec(a: Vec, b: Vec)` | `createFromVecs a b` |
| `Quaternion` | `createVecToVec(a: UnitVec, b: UnitVec)` | `createFromUnitVecs a b` |
| `Quaternion` | `createFromEulerXYZ(x, y, z)` | `createFromEulerXYZDegrees x y z` |
| `Quaternion` | Other five `createFromEuler...` functions | Append `Degrees` and curry in the existing axis order |

Each Euler factory also gets a new `Radians` partner; see [New Radians factories](#new-radians-factories).

Keep unary `Rotation2D.createFromDegrees`, `createFromRadians`, `createFromSine`, and `createFromCosine`. Keep unary quaternion matrix conversions.

Preserve the current difference between `Rotation2D.create`, which validates a unit-length pair, and `Quaternion.create`, which normalizes four components. Document this explicitly. A common normalization policy is a separate behavioral change.

## Plane factories

Files: [PPlane](../Src/PPlane.fs), [PPlane extensions](../Src/TypeExtensions/PPlane.fs), [NPlane](../Src/NPlane.fs).

| Type | Current form | Target form |
|:--|:--|:--|
| `PPlane` | `create(origin, axisX, axisY, axisZ)` | `create xAxis yAxis zAxis origin`; parameters renamed to match `Box.create` |
| `PPlane` | `createOriginNormal(origin, normal: Vec)` | `createFromNormal normal origin` |
| `PPlane` | `createOriginNormal(origin, normal: UnitVec)` | `createFromUnitNormal normal origin` |
| `PPlane` | `createOriginNormalXaxis(origin, normal: Vec, xAxis: Vec)` | `createFromNormalAndXAxis normal xAxis origin` |
| `PPlane` | Same with two `UnitVec` inputs | `createFromUnitNormalAndXAxis normal xAxis origin` |
| `PPlane` | `createOriginXaxisYaxis(origin, xAxis: Vec, yAxis: Vec)` | `createFromXAxisAndYAxis xAxis yAxis origin` |
| `PPlane` | Same with two `UnitVec` inputs | `createFromUnitXAxisAndYAxis xAxis yAxis origin` |
| `PPlane` | `createThreePoints origin xPoint yPoint` | `createFrom3Pnts origin xPoint yPoint` |
| `PPlane` | `createUncheckedVec(origin, axisX, axisY, axisZ)` | `createFromAxesUnchecked(origin, xAxis, yAxis, zAxis)`, tupled, same order |
| `NPlane` | `create(origin, normal: Vec)` | `createFromNormal normal origin` |
| `NPlane` | `create(origin, normal: UnitVec)` | `createFromUnitNormal normal origin` |
| `NPlane` | `createFrom3Points a b c` | `createFrom3Pnts a b c` |
| `NPlane` | `createUncheckedVec(origin, normal: UnitVec)` | `createFromUnitNormalUnchecked(origin, normal)`, tupled, same order |

Keep `NPlane.createFromPPlane`.

A leading `Unit` covers every vector input, so `createFromUnitNormalAndXAxis` and `createFromUnitXAxisAndYAxis` take only `UnitVec`s. Keep those types explicit in signatures and XML parameters. `PPlane.createFromAxesUnchecked` needs no `Unit`, because a `PPlane` has no non-unit axis variant. Preserve the different roles of a Y-axis hint, a complete orthonormal frame, and a normal with a fallback axis.

## Matrix factories

Files: [Matrix](../Src/Matrix.fs), [RigidMatrix](../Src/RigidMatrix.fs), and their existing extensions.

The following mappings apply to both matrix types wherever the member exists.

| Current form | Target form |
|:--|:--|
| `createTranslation(v: Vec)` | `createTranslation v` |
| `createTranslation(x, y, z)` | `createTranslationXYZ x y z` |
| `createRotationAxis(axis: Vec, degrees)` | `createRotationFromVecDegrees axis degrees` |
| `createRotationAxis(axis: UnitVec, degrees)` | `createRotationFromUnitVecDegrees axis degrees` |
| `createRotationAxisCenter(axis: Vec, center, degrees)` | `createRotationAroundVecDegrees center axis degrees` |
| Same with `UnitVec` axis | `createRotationAroundUnitVecDegrees center axis degrees` |
| `createRotationX(degrees)` | `createRotationXDegrees degrees` |
| `createRotationY(degrees)` | `createRotationYDegrees degrees` |
| `createRotationZ(degrees)` | `createRotationZDegrees degrees` |
| `createVecToVec(a: Vec, b: Vec)` | `createRotationFromVecs a b` |
| `createVecToVec(a: UnitVec, b: UnitVec)` | `createRotationFromUnitVecs a b` |
| `createPlaneToPlane(fromPlane, toPlane)` | `createPlaneToPlane fromPlane toPlane` |
| `createToPlane plane` | Keep the name and existing direction of transformation |

`From` names the axis or vector inputs, as in `Quaternion.createFromVecDegrees`. `Around` replaces `From` when a center point is also given, matching `rotateAround`.

Additional `Matrix` mappings:

| Current form | Target form |
|:--|:--|
| `createScale(x, y, z)` | `createScale x y z` |
| `createShear(xy, xz, yx, yz, zx, zy)` | Unchanged; stays tupled (more than three `float` arguments) |
| `createLookAt(eye, target, up)` | `createLookAt up eye target` |
| `createLookAtZUp(eye, target)` | `createLookAtZUp eye target` |
| `createPerspectiveAlongNegZ(width, height, nearDistance, farDistance)` | Unchanged; stays tupled (more than three `float` arguments) |
| `createPerspectiveAlongPosY(width, height, nearDistance, farDistance)` | Unchanged; stays tupled (more than three `float` arguments) |
| `createPerspectiveLookAt(eye, target, up, width, height, nearDistance, farDistance)` | Unchanged; stays tupled (more than three `float` arguments) |
| `createPerspectiveLookAtZUp(eye, target, width, height, nearDistance, farDistance)` | Unchanged; stays tupled (more than three `float` arguments) |

The curried `createLookAt` puts `up` first; the tupled perspective factories keep their current argument order.

`RigidMatrix.create(m11, m21, m31, x41, m12, m22, m32, y42, m13, m23, m33, z43)` stays tupled because it has more than three `float` arguments. Retain this row-major argument order even though the element names can be misread as column-major. Do not change matrix storage, serialization, handedness, projection axes, or multiplication while changing factory syntax.

### New Radians factories

Add a `Radians` factory next to every `Degrees` factory that lacks one. These are new members, not renames. Each takes the same arguments as its `Degrees` partner, in the same order.

| Type | New factories |
|:--|:--|
| `Matrix`, `RigidMatrix` | `createRotationXRadians`, `createRotationYRadians`, `createRotationZRadians`, `createRotationFromVecRadians`, `createRotationFromUnitVecRadians`, `createRotationAroundVecRadians`, `createRotationAroundUnitVecRadians`, wherever the `Degrees` partner exists |
| `Quaternion` | `createFromEulerXYZRadians` and the five other Euler axis orders |

`Rotation2D` and the quaternion axis-angle factories already have both forms.

## Line factories

Files: [Line2D extensions](../Src/TypeExtensions/Line2D.fs), [Line3D extensions](../Src/TypeExtensions/Line3D.fs).

| Type | Current form | Target form |
|:--|:--|:--|
| `Line2D` / `Line3D` | No active endpoint `create` factory | Add `create start finish`, using `Pt` / `Pnt` inputs |
| `Line2D` | `createFromPtAndVc(point, vector)` | `createFromPtAndVc point vector` |
| `Line3D` | `createFromPntAndVec(point, vector)` | `createFromPntAndVec point vector` |
| Both | Existing unary conversion factories | Keep names and behavior |
| `Line3D` | `createFromLine2DWithZ z line` | Keep unchanged |

The endpoint factory is a convenience wrapper around the existing endpoint constructor. It introduces no new zero-length restriction. The point-and-vector factory still uses the vector as a displacement, not as an endpoint. It keeps the point first, like the XLine `FromPtsAndVcs` forms, so partial application fixes the start point.

## Rectangle and box factories

Files: [Rect2D](../Src/Rect2D.fs), [Rect3D](../Src/Rect3D.fs), [Box](../Src/Box.fs), [BRect](../Src/BRect.fs), [BBox](../Src/BBox.fs).

| Type | Current form | Target form |
|:--|:--|:--|
| `Rect2D` / `Rect3D` | `createFrom3Points(origin, xPoint, yPoint)` | `createFrom3Pts origin xPoint yPoint` / `createFrom3Pnts origin xPoint yPoint` |
| `Rect2D` / `Rect3D` | `tryCreateFrom3Points(origin, xPoint, yPoint)` | Matching `tryCreateFrom3Pts` / `tryCreateFrom3Pnts`, same curried order |
| `Rect2D` / `Rect3D` | `createFromVectors(origin, x, y)` | `createFromVcs x y origin` / `createFromVecs x y origin` |
| `Rect2D` / `Rect3D` | `createUncheckedVec(origin, x, y)` | `createFromVcsUnchecked(origin, x, y)` / `createFromVecsUnchecked(origin, x, y)`, tupled, same order |
| `Rect2D` | `createFromXVectorAndWidth(origin, x, sizeY)` | `createFromVc sizeY x origin` |
| `Rect2D` | `createFromDirectionAndSizes(origin, directionX, sizeX, sizeY)` | `createFromUnitVc sizeX sizeY directionX origin` |
| `Rect2D` | `createFromCenterAndVector(center, x, sizeY)` | `createCenteredFromVc sizeY x center` |
| `Rect2D` | `createFromCenterAndDirection(center, directionX, sizeX, sizeY)` | `createCenteredFromUnitVc sizeX sizeY directionX center` |
| `Rect2D` | `createFromDirAndPoints directionX points` | `createFromVcAndPts directionX points` |
| `Rect2D` | `createFromLine(line, offRight, offLeft)` | `createFromLine offRight offLeft line` |
| `Rect3D` | `createFromPlane(plane, sizeX, sizeY)` | `createFromPlane sizeX sizeY plane` |
| `Rect3D` | `createCenteredFromPlane(plane, sizeX, sizeY)` | `createCenteredFromPlane sizeX sizeY plane` |
| `Box` | `create(origin, xAxis, yAxis, zAxis)` | `create xAxis yAxis zAxis origin` |
| `Box` | `createUncheckedVec(origin, xAxis, yAxis, zAxis)` | `createFromAxesUnchecked(origin, xAxis, yAxis, zAxis)`, tupled, same order |
| `Box` | `createFromDirsAndPoints directionX directionY points` | `createFromVecsAndPnts directionX directionY points` |
| `Box` | `createFromPlaneAndPoints plane points` | `createFromPlaneAndPnts plane points` |
| `Box` | `createFromPlane x y z plane` | Same order; rename parameter labels to `sizeX sizeY sizeZ plane` |
| `BRect` / `BBox` | `create(a, b)` | `create a b` |
| `BRect` | `createFromCenter(center, sizeX, sizeY)` | `createCentered sizeX sizeY center` |
| `BBox` | `createFromCenter(center, sizeX, sizeY, sizeZ)` | `createCentered sizeX sizeY sizeZ center` |
| `BRect` | `createXY(ax, ay, bx, by)` | Unchanged; stays tupled (more than three `float` arguments) |

Keep `Rect2D.createFromBounds` and `Rect3D.createFromBounds2D` unchanged: names, component order and tupled form (more than three `float` arguments). Keep existing one-argument collection/conversion factories and already-curried Z-range extrusion factories.

Do not reinterpret bounds as unordered corners. `BRect.create` / `BBox.create` and the raw bounds factories have different contracts. Preserve signed axis vectors, zero-size behavior, and existing orientation checks.

## Polyline and FreeBox factories

Files: [Polyline2D](../Src/Polyline2D.fs), [Polyline3D](../Src/Polyline3D.fs), [FreeBox](../Src/FreeBox.fs).

| Type | Current form | Target form |
|:--|:--|:--|
| `Polyline2D` | `createDirectly coordinates` | `createFromSharedXYBuffer coordinates` |
| `Polyline3D` | `createDirectly coordinates` | `createFromSharedXYZBuffer coordinates` |
| `FreeBox` | `createDirectly coordinates` | `createFromSharedXYZBuffer coordinates` |
| `Polyline2D` | `createFromXYMembers items` | `createFromMembersXY items` |
| `Polyline2D` | `createFromxyMembers items` | `createFromMembersxy items` |
| `Polyline3D` | `createFromXYZMembers items` | `createFromMembersXYZ items` |
| `Polyline3D` | `createFromxyzMembers items` | `createFromMembersxyz items` |
| `Polyline3D` | `createFrom2DXYMembers items` | `createFromMembersXY items` |
| `Polyline3D` | `createFrom2DxyMembers items` | `createFromMembersxy items` |
| `FreeBox` | `createFromEightPoints points` | `createFrom8PntArray points` |
| `FreeBox` | `createFromFour2DPoints zMin zMax points` | `createFrom4PtArray zMin zMax points` |
| `FreeBox` | `createFromFour2DPointsArgs zMin zMax a b c d` | `createFrom4Pts(zMin, zMax, a, b, c, d)`, now tupled because it has four `Pt` arguments |

Keep `createEmpty capacity`, `createFromPts`, `createFromPnts`, `createFromBox`, `createFromRect3D`, and the rectangle/bounding-rectangle CW/CCW factories. Preserve copied versus shared input storage, point order, closure, and the Z=0 projection used by the 2D-member imports into `Polyline3D`.


## Other static operations

### Dot and cross products

Keep the existing vector and unit-vector static `cross` overloads tupled, including mixed unit/non-unit inputs. Keep instance `Cross` overloads and operator behavior.

`Line2D.cross` is currently curried. Change `Line2D.cross a b` to `Line2D.cross(a, b)` so it follows the selected exception. Its `det` member has the same implementation; remove that alias and migrate `Line2D.det a b` to `Line2D.cross(a, b)`. Keep `Tria2D.det(a, b, c)` and `Vec.determinant3(u, v, w)` / `UnitVec.determinant3(u, v, w)` tupled. A single-input determinant such as `Matrix.determinant matrix` remains unary.

For `dot`, choose the owning type from the first argument and name a differing second argument type explicitly:

| Existing 3D call | Target call |
|:--|:--|
| `Vec.dot(a: Vec, b: Vec)` | `Vec.dot a b` |
| `Vec.dot(a: Vec, b: UnitVec)` | `Vec.dotUnitVec a b` |
| `Vec.dot(a: UnitVec, b: Vec)` | `UnitVec.dotVec a b` |
| `UnitVec.dot(a: UnitVec, b: UnitVec)` | `UnitVec.dot a b` |
| `UnitVec.dot(a: UnitVec, b: Vec)` | `UnitVec.dotVec a b` |
| `UnitVec.dot(a: Vec, b: UnitVec)` | `Vec.dotUnitVec a b` |

Apply the exact `Vc` / `UnitVc` mirror in 2D. Rows targeting the same member converge on one implementation. Keep operand order in that implementation, even though the mathematical dot product is commutative. Keep `dotCosine` and its measured result.

### Interpolation and simple numeric operations

Convert `lerp(start, finish, factor)` and `slerp(start, finish, factor)` to `lerp start finish factor` and `slerp start finish factor` on all types where they exist. Preserve `UnitVc.lerp` / `UnitVec.lerp` returning ordinary vectors.

Convert non-overloaded angle functions such as `angle360`, `angle2Pi`, and `angleDiamond` to `a b` argument groups. Keep existing names, ranges, and radians/degrees meanings in this migration. Apply the same currying rule to non-overloaded distance and classification helpers.

The mixed tuple/curried forms `Pt.distanceToXY (x, y) point` and `Pnt.distanceToXYZ (x, y, z) point`, and their squared variants, become `... x y [z] point`.

Split projected-parameter overloads:

| Existing overload | Target name and order |
|:--|:--|
| `Pt.projectedParameter(origin, direction: Vc, testPoint)` | `projectedParameterOnVc origin direction testPoint` |
| Same with `UnitVc` | `projectedParameterOnUnitVc origin direction testPoint` |
| Same with an endpoint `Pt` | `projectedParameterBetweenPts origin endPoint testPoint` |
| Three `Pnt` overloads | Exact `Pnt` / `Vec` / `UnitVec` mirror |

### Point collections and topology

Keep existing collection types and return types. The representative migration map is:

| Family | Current form | Target form |
|:--|:--|:--|
| `Points2D` / `Points3D` | `closestPoint(points, reference)` | `closestPoint reference points` |
| Both | `closestPointIdx(points, reference)` | `closestPointIndex reference points` |
| Both | `closestPointsIdx(xs, ys)` | `closestPointsIndices xs ys` |
| Both | `minDistBetweenPointSets(xs, ys)` | `minDistanceBetweenPointSets xs ys` |
| Both | `mostDistantPoint(findPointFrom, checkAgainst)` | `mostDistantPoint checkAgainst findPointFrom` |
| Both | `mostDistantPointIdx(findPointFrom, checkAgainst)` | `mostDistantPointIndices checkAgainst findPointFrom` |
| `Topology2D` / `Topology3D` | `join(getLine, splitDistance, items)` | `join getLine splitDistance items` |
| Both | `joinReversing(getLine, splitDistance, items)` | `joinReversing getLine splitDistance items` |
| Both | `sortToLoop(getLine, items)` | `sortToLoop getLine items` |
| Both | `sortToLoopCached(getLine, items)` | `sortToLoopCached getLine items` |
| Both | `sortToLoopWithReversing(getLine, reverseInPlace, items)` | `sortToLoopWithReversing getLine reverseInPlace items` |
| Both | `sortToLoopWithReversingCached(getLine, reverseInPlace, items)` | `sortToLoopWithReversingCached getLine reverseInPlace items` |
| `Similarity2D` | `getGrouped(tolerance, items, similarities)` | `getGrouped tolerance similarities items` |

The `mostDistantPointIdx` result is an index pair in the baseline; retain which collection each component indexes. Do not reverse that pair when reordering the inputs.

The `sortToLoop...` functions sort the caller's `ResizeArray` in place and return `unit`. They get no suffix; their XML docs must say that they mutate `items`.

### Triangle operations

Use `area a b c`, `areaDouble a b c`, `areaSigned a b c`, and other suitable static operations in curried form. Keep determinant-like operations tupled under the operand-order exception.

Use `offset distance a b c`. For per-vertex operations, put configuration first and the point being changed last:

```text
offsetPt distance previous next point
offsetPnt distance previous next point
offsetPtVariable previousDistance nextDistance previous next point
offsetPntVariable previousDistance nextDistance previous next point
offsetPtVariableByNormals previousDistance nextDistance previousNormal nextNormal point
```

`Var` becomes `Variable`, matching the polyline and `Offset2D` / `Offset3D` `offsetVariable`. Apply the same names and order to the existing `try...` partners. `offsetPtVariableByNormals` and its `try...` partner exist only in 2D. Keep distance associations with previous/next edges exactly as before.

Use `intersectLine a b c line` and `intersectRay a b c ray`, with matching `try...` forms. Preserve the existing point result for line intersections and parameter result for ray intersections. Do not change Euclid's definition of a ray as a line infinite in both directions.

`isLinear a b c` and `isLinearFast a b c` become curried defaults. `isLinearWith(a, b, c, ?distanceTolerance)` and `isLinearFastWith(a, b, c, ?maxAreaParallelogram)` keep the tupled optional form; see [Optional parameters](#optional-parameters). `Points3D.areInLine` and `areInLineFast` follow the same pattern. Preserve the distinct distance-based and area-based tests.

## Intersection overloads and optional parameters

Files: [XLine2D](../Src/XLine2D.fs), [XLine3D](../Src/XLine3D.fs).

Each overloaded `XLine2D` / `XLine3D` operation receives a unique name for each representation. The target forms are curried unless a [currying exception](#currying-exceptions) applies:

| Current representation | Target representation |
|:--|:--|
| `operation(lineA, lineB)` | `operation lineA lineB` |
| `operation(pA, pB, vA, vB)` in 2D | `operationFromPtsAndVcs pA pB vA vB` |
| Same in 3D | `operationFromPntsAndVecs pA pB vA vB` |
| Either form with optional parameters | Also a tupled `operationWith(...)` with the current parameters, order and defaults. Both representations may share this overloaded `With` name. |

Preserve the point-point-vector-vector argument order in the low-level form. It is intentionally not changed to point-vector-point-vector.

For example, `tryIntersectRay` becomes:

```fsharp
XLine2D.tryIntersectRay lineA lineB                     // curried, default tangent
XLine2D.tryIntersectRayFromPtsAndVcs pA pB vA vB         // curried, default tangent
XLine2D.tryIntersectRayWith(lineA, lineB, tangent = t)   // tupled, optional tangent
XLine2D.tryIntersectRayWith(pA, pB, vA, vB, tangent = t) // tupled overload
```

Apply this rule to all current overload groups: `classifyClosestParameters`, `classifyClosestPoints`, `classifyIntersection`, `classifyIntersectionParam`, `classifyRayIntersectionParam` / `classifyRayClosestParam`, `classifyRayIntersection`, `closestParameters`, `doIntersect`, `doOverlap`, `doRaysIntersect`, `getSqDistance`, `intersectRays`, `isWithinRanges`, `parameterA`, `parameters`, `tryParameterA`, `tryClosestParameterRayA`, `tryClosestPntRayA`, `tryIntersect`, `tryIntersectInRangeA`, `tryIntersectInRanges`, and `tryIntersectRay`, wherever they exist.

Before splitting, remove the obsolete `get...` aliases of the `classify...` operations: `getIntersection`, `getIntersectionParam`, `getRayIntersection`, `getRayIntersectionParam`, `getRayClosestParam`, `getClosestParameters`, `getClosestPoints`, and `getEndsTouching`. They are overloaded and have optional parameters, so splitting them would be wasted work. Migrate their callers to the `classify...` names.

Do not rename a 3D closest-point classification to an intersection classification simply to match 2D. Skew lines make these different contracts.

Do not introduce options records. Each curried default must reproduce the defaults of its own old operation, such as `tangent`, `tooShortTolerance` and, in 3D, `maxSkewDistance`. If operations use different defaults, preserve each one; do not silently pick one common threshold.

A single optional tolerance follows the same pattern, for example `doOverlap lineA lineB` and `doOverlapWith(lineA, lineB, ?tolerance)`. This applies to `classifyEndsTouching` and `doOverlap`. For range-limited curried operations, place minimum/maximum parameters first, followed by the original geometry arguments.

`XLine2D.isWithinRanges` and `tryIntersectInRanges` each take four `float` bounds, so both representations stay tupled with their existing geometry-first parameter order. Split their representation names as above, with these exact target signatures:

| Current overload | Target signature on `XLine2D` |
|:--|:--|
| `isWithinRanges(lineA, lineB, minParameterA, maxParameterA, minParameterB, maxParameterB)` | `isWithinRanges(lineA:Line2D, lineB:Line2D, minParameterA:float, maxParameterA:float, minParameterB:float, maxParameterB:float) : bool` |
| `isWithinRanges(pA, pB, vA, vB, minParameterA, maxParameterA, minParameterB, maxParameterB)` | `isWithinRangesFromPtsAndVcs(pA:Pt, pB:Pt, vA:Vc, vB:Vc, minParameterA:float, maxParameterA:float, minParameterB:float, maxParameterB:float) : bool` |
| `tryIntersectInRanges(lineA, lineB, minParameterA, maxParameterA, minParameterB, maxParameterB)` | `tryIntersectInRanges(lineA:Line2D, lineB:Line2D, minParameterA:float, maxParameterA:float, minParameterB:float, maxParameterB:float) : Pt voption` |
| `tryIntersectInRanges(pA, pB, vA, vB, minParameterA, maxParameterA, minParameterB, maxParameterB)` | `tryIntersectInRangesFromPtsAndVcs(pA:Pt, pB:Pt, vA:Vc, vB:Vc, minParameterA:float, maxParameterA:float, minParameterB:float, maxParameterB:float) : Pt voption` |

These bounds are required parameters, so no `With` forms are added. `tryIntersectInRangeA` has only two `float` bounds and becomes `tryIntersectInRangeA minParameterA maxParameterA lineA lineB` or `tryIntersectInRangeAFromPtsAndVcs minParameterA maxParameterA pA pB vA vB`. Preserve which range belongs to each line in every form.

For `classifyConeIntersection`, use `coneRadius coneBaseZ coneTipZ ray`. Raw-coordinate helpers in `XLineXY` / `XLineXYZ` take more than three `float` arguments, so they stay tupled and keep their coordinate sequence and any genuine tuple outputs. Public wrappers become curried where the same-type rule allows; internal arithmetic remains unchanged.

Normalize `getSqDistance` to `distanceSq` in the same phase. The representation pairs become `distanceSq lineA lineB` with `distanceSqFromPtsAndVcs pA pB vA vB` in 2D, and `distanceSq lineA lineB` with `distanceSqFromPntsAndVecs pA pB vA vB` in 3D.

## Offset API names

Files: [Offset2D](../Src/Offset2D.fs), [Offset3D](../Src/Offset3D.fs), and both polyline files.

Replace the apostrophe variants. A trailing `With` marks the tupled form with settings, and `By<Noun>` marks an extra geometric input; see [Naming and argument rules](#naming-and-argument-rules). The polyline types follow [Optional parameters](#optional-parameters): the plain name is the curried default, and the `...With` name is the tupled form with the current optional parameters. In the table, `?name` marks an existing `[<OPT;DEF(...)>]` parameter, not an F# option type.

| Current | Curried default | Tupled with optional parameters |
|:--|:--|:--|
| `Polyline2D.offset' distance pl`, `Polyline2D.offset(pl, distance, ...)` | `offset distance pl` | `offsetWith(pl, distance, ?loop, ?checkOrientation, ?uTurnBehavior, ?useUTurnBehaviorAbove)` |
| `Polyline2D.offsetVar(pl, distances, ...)` | `offsetVariable distances pl` | `offsetVariableWith(pl, distances, ?loop, ?checkOrientation, ?varDistParallelBehavior, ?uTurnBehavior, ?useVarDistParallelBehaviorBelow, ?useUTurnBehaviorAbove)` |
| `Polyline3D.offset' inPlane perpendicular pl`, `Polyline3D.offset(pl, inPlane, perpendicular, ...)` | `offset inPlane perpendicular pl` | `offsetWith(pl, inPlane, perpendicular, ?loop)` |
| `Polyline3D.offsetWithRef(pl, inPlane, perpendicular, refNormal, ...)` | `offsetByNormal refNormal inPlane perpendicular pl` (new) | `offsetByNormalWith(pl, inPlane, perpendicular, refNormal, ?loop)` |
| `Polyline3D.offsetVar(pl, inPlanes, perpendiculars, ...)` | `offsetVariable inPlanes perpendiculars pl` | `offsetVariableWith(pl, inPlanes, perpendiculars, ?loop, ?varDistParallelBehavior, ?considerCollinearBelow, ?failAtUTurnAbove)` |
| `Polyline3D.offsetVarWithRef(pl, inPlanes, perpendiculars, refNormal, ...)` | `offsetVariableByNormal refNormal inPlanes perpendiculars pl` (new) | `offsetVariableByNormalWith(pl, inPlanes, perpendiculars, refNormal, ?loop, ?varDistParallelBehavior, ?considerCollinearBelow, ?failAtUTurnAbove)` |

The tupled forms keep the parameter names and order, so `Polyline2D.offset(pl, 5.0, loop = true)` migrates to `Polyline2D.offsetWith(pl, 5.0, loop = true)`. The new curried `ByNormal` defaults take the reference normal first, in the same order as `Offset3D.offsetByNormal`.

`Offset2D` and `Offset3D` are modules. Their `let` functions cannot take optional parameters, so their `With` forms take every setting explicitly. They are still tupled, so a trailing `With` means the same thing everywhere. They put the geometry first, followed by the settings in the order of the matching polyline `With` form, without `loop` and `checkOrientation`.

| Current | Target |
|:--|:--|
| `Offset2D.offset distance xys` | Keep |
| `Offset2D.offset'' useUTurnBehaviorAbove uTurnBehavior distance xys` | `offsetWith(xys, distance, uTurnBehavior, useUTurnBehaviorAbove)` |
| `Offset2D.offset' uTurnBehavior distance xys` | Remove; migrate to ```offsetWith(xys, distance, uTurnBehavior, Cosine.``175.0``)``` |
| `Offset2D.offsetVariable distances xys` | Keep |
| `Offset2D.offsetVariable'' useUTurnBehaviorAbove useVarDistParallelBehaviorBelow uTurnBehavior varDistParallelBehavior distances xys` | `offsetVariableWith(xys, distances, varDistParallelBehavior, uTurnBehavior, useVarDistParallelBehaviorBelow, useUTurnBehaviorAbove)` |
| `Offset2D.offsetVariable' uTurnBehavior varDistParallelBehavior distances xys` | Remove; migrate to ```offsetVariableWith(xys, distances, varDistParallelBehavior, uTurnBehavior, Cosine.``2.5``, Cosine.``175.0``)``` |
| `Offset2D.offsetWithDirections(xys, dirs, dist, uTurnBehavior, useUTurnBehaviorAbove)` | `offsetByDirections`, same tupled arguments |
| `Offset2D.offsetVariableWithDirections(...)` | `offsetVariableByDirections`, same tupled arguments |
| `Offset3D.offset inPlane perpendicular xyzs` | Keep |
| `Offset3D.offset' refNormal inPlane perpendicular xyzs` | `offsetByNormal refNormal inPlane perpendicular xyzs`, curried, because it takes no settings |
| `Offset3D.offsetVariable inPlanes perpendiculars xyzs` | Keep |
| `Offset3D.offsetVariable' varDistParallelBehavior refNormal inPlanes perpendiculars xyzs` | `offsetVariableByNormalWith(xyzs, inPlanes, perpendiculars, refNormal, varDistParallelBehavior)`. Also add the curried default `offsetVariableByNormal refNormal inPlanes perpendiculars xyzs`, using `VarDistParallel.Fail` |
| `Offset3D.offsetConstantWithDirections(xyzs, dirs, distInPlane, distPerpendicular)` | `offsetByDirections`, same tupled arguments |
| `Offset3D.offsetVariableWithDirections(...)` | `offsetVariableByDirections`, same tupled arguments |

In `Offset2D.offsetVariableWith`, `useVarDistParallelBehaviorBelow` and `useUTurnBehaviorAbove` are both `float<Cosine.cosine>`, and their order is the reverse of `offsetVariable''`. A call that swaps them still compiles. Migrate these calls by parameter name, and test that each migrated call yields the same result as the old call.

Preserve every current default, including hard-coded constants. They differ between layers; unifying them is follow-up work. Baseline values:

| Operation | Collinear threshold | U-turn threshold | Other defaults |
|:--|:--|:--|:--|
| `Offset2D.offset` | none | 177.5°, `UTurn.Fail` | |
| `Offset2D.offset'` | none | 175° | |
| `Polyline2D.offset` | none | 175°, `UTurn.Fail` | `loop = false`, `checkOrientation = true` |
| `Polyline2D.offsetVar` | 5°, `VarDistParallel.Fail` | 175°, `UTurn.Fail` | `loop = false`, `checkOrientation = true` |
| `Offset2D.offsetVariable`, `offsetVariable'` | 2.5° (`offsetVariable`: `VarDistParallel.Fail`) | 175° (`offsetVariable`: `UTurn.Fail`) | |
| `Polyline3D.offset`, `offsetWithRef` | 2.5°, hard-coded | 179°, hard-coded | `loop = false` |
| `Polyline3D.offsetVar`, `offsetVarWithRef` | 2.5°, `VarDistParallel.Fail` | 175° | `loop = false` |
| `Offset3D.offset`, `offset'`, `offsetVariable`, `offsetVariable'` | 2.5° (`offsetVariable`: `VarDistParallel.Fail`) | 175° | |

Re-extract these in phase 1 from `[<OPT;DEF(...)>]` attributes and hard-coded arguments, and record them alongside each migration entry.

Do not change `IList` into `ResizeArray` or vice versa as part of the rename; `Polyline3D.offsetVar` takes `ResizeArray` while `offsetVarWithRef` takes `IList`. Do not add collection copying merely to share a wrapper.

In phase 5, also fix the `loop` docstrings of the constant `Polyline3D.offset` and `offsetWithRef` (now `offsetWith` and `offsetByNormalWith`). They mention "both distance lists", copied from the variable-distance versions.

## Movement and mutation

### Canonical movement names

Use `move` for the library-wide translation operation. Immutable types return a new value from `Move` and `move`. On the mutable types, instance `Move` mutates and static `move` returns a copy; see [Mutation naming](#mutation-naming).

| Current behavior | Target |
|:--|:--|
| Static `translate vector geometry`, equivalent to `move` | Remove `translate`; migrate callers to `move vector geometry` |
| `Pt.addVc vector point` / `Pnt.addVec vector point`, identical to `move` | Remove; migrate callers to `move vector point` |
| Instance `Move vector` | Keep on all types |
| Mutable static `move vector geometry` returning a copy | Keep |
| Point movers `movePt` / `movePnt` and `MovePt` / `MovePnt`, which mutate | Keep names; they follow the indexed point setter/mover rule in [Mutation naming](#mutation-naming) |
| Existing local-axis `translateLocalX/Y/Z` functions | `moveLocalX/Y/Z` with unchanged behavior |

Do not add movement to vectors or unit vectors. The rule covers the verb only. `Translation` as a noun stays, as in `Matrix.createTranslation`, `createTranslationXYZ` and `addTranslation`.

### Mutation naming

Do not add an `InPlace` suffix. The type and the verb signal mutation, following the convention already documented on `Polyline2D`, `Polyline3D` and `FreeBox`:

- Only `Polyline2D`, `Polyline3D` and `FreeBox` have mutable geometry instances. This restriction does not exclude operations that mutate caller-owned collections, such as the topology `sortToLoop...` functions and offset buffer helpers; document their collection mutation explicitly.
- Their instance transforms and editing operations, such as `Move`, `Rotate`, `RotateAround`, `Scale`, `ScaleOn`, `Transform`, `TransformRigid`, `Reverse`, `Close` and the duplicate, collinear-point and U-turn removals, mutate in place. Keep their names and return types; some also return a report.
- The corresponding static transforms and cleanup operations return a new instance. Indexed point setters and movers mutate and return `unit`: `setPt...`, `movePt...` and their `XY`, `XYZ`, `Closed` and `Pnt` variants. So do the append statics `addPoint`, `addXY` and `addXYZ`. State this rule in the XML docs of each of the three types.
- Taking a point index alone does not imply mutation: `getPt`, `getPnt`, `getX`, `getY`, `getZ` and `getLine` observe the geometry. `set...` and append-style `add...` / `Add...` names mean mutation. Static `move` returns a copy while `movePt` is an indexed point mover and mutates its target.
- Component-replacement operations that return a copy use `with...`, for example `withX`, `withStart`, `withOrigin` and `WithAngleInDegrees`. Transforms and cleanup operations keep their existing verbs, such as `move`, `rotate`, `scale`, `reverse` and `removeDuplicatePoints`, even when they return modified copies. Remove the obsolete `set...` aliases on immutable types, such as `Pt.setX`, `Line2D.setStart`, `PPlane.setOrigin` and `Quaternion.SetAngleInDegrees`, with the other obsolete names in phase 7.
- `add` meaning a sum stays on immutable types and returns a new value, for example `Vec.add`, `Vc.addX`, `Matrix.addTranslation` and `Rotation2D.addDegrees`.

Keep property setters as setters. Document that they mutate and whether they preserve closure.

Do not infer mutation from a `unit` return or an imperative-sounding name alone. Inspect the implementation of every member of the three mutable types and classify it as returning a copy, mutating its target, observing it, or exposing shared storage. Record any member that breaks the convention above in the migration list instead of renaming it silently.

Shared storage should also be visible:

| Type | Current property / accessor | Target |
|:--|:--|:--|
| `Polyline2D` | `XYs` / `getXYs` | `SharedXYBuffer` / `getSharedXYBuffer` |
| `Polyline3D`, `FreeBox` | `XYZs` / `getXYZs` | `SharedXYZBuffer` / `getSharedXYZBuffer` |

Preserve the actual buffer types and aliasing. Retain ordinary coordinate names for members that produce a fresh array.

## Other spelling changes

Apply these changes to both dimensions and to existing instance/static counterparts.

| Current spelling | Target spelling |
|:--|:--|
| `unitizeWithErrMsg` | `unitizeWithErrorMessage` |
| Vector `flip`, when identical to `reverse` | Remove `flip`; migrate to `reverse` |
| `rotateOnXDeg`, `rotateOnYDeg`, `rotateOnZDeg` | `rotateOnXDegrees`, `rotateOnYDegrees`, `rotateOnZDegrees` |
| `rotateOnXWithCenter`, and Y/Z counterparts | `rotateAroundOnX`, and Y/Z counterparts |
| `rotateOnXWithCenterDeg`, and Y/Z counterparts | `rotateAroundOnXDegrees`, and Y/Z counterparts |
| Instance `Xaxis`, `Yaxis`, `Zaxis` | `XAxis`, `YAxis`, `ZAxis` |
| Instance `XaxisUnit`, `YaxisUnit`, `ZaxisUnit` | `XAxisUnit`, `YAxisUnit`, `ZAxisUnit` |
| Static axis values/accessors | `xAxis`, `yAxis`, `zAxis` |
| Axis fields such as `XaxisX` | `XAxisX`, preserving the final coordinate suffix |
| `expandXaxis`, and Y/Z counterparts | `expandXAxis`, and Y/Z counterparts |
| `Rotation2D.InDegrees` / `inDegrees` | `AngleInDegrees` / `angleInDegrees` |
| `Rotation2D.InRadians` / `inRadians` | `AngleInRadians` / `angleInRadians` |
| `dist`, where identical to `distanceTo` | Remove alias; use `distanceTo` |
| `sqDist`, where identical to `sqDistanceTo` | Remove alias; use `distanceToSq` |
| `sqDistanceTo` / `SqDistanceTo` | `distanceToSq` / `DistanceToSq` |
| `sqDistanceFromOrigin` | `distanceFromOriginSq` |
| `sqDistanceInXYFromOrigin` | `distanceInXYFromOriginSq` |
| `sqDistanceToLine`, `sqDistanceToPt`, `sqDistanceToPnt` | `distanceToLineSq`, `distanceToPtSq`, `distanceToPntSq` |
| `sqDistanceToXY`, `sqDistanceToXYZ` | `distanceToXYSq`, `distanceToXYZSq` |
| `sqDistanceRayPoint`, `sqDistanceRayXY`, `sqDistanceRayXYZ` | `distanceRayPointSq`, `distanceRayXYSq`, `distanceRayXYZSq` |
| `lengthSqInXY` | `lengthInXYSq` |
| `XLineXY.sqDistance`, `XLineXYZ.sqDistance` | `distanceSq` |
| `sqOpenTolerance` | `openToleranceSq` |
| `Rect2D` / `Rect3D` `evaluateDist` / `EvaluateDist` | `evaluateDistance` / `EvaluateDistance` |
| `closestPointIdx`, `closestPointsIdx`, `mostDistantPointIdx` | `closestPointIndex`, `closestPointsIndices`, `mostDistantPointIndices`; see [Point collections and topology](#point-collections-and-topology) |
| `minDistBetweenPointSets` | `minDistanceBetweenPointSets` |
| `offsetPtVar`, `offsetPntVar`, `offsetPtVarByNormals`, and their `try...` partners | `offsetPtVariable`, `offsetPntVariable`, `offsetPtVariableByNormals`; see [Triangle operations](#triangle-operations) |

Keep established mathematical names such as `anglePi`, `angleHalfPi`, `angle180`, and `angle90` for now. Their range/orientation contracts need a separate naming review before replacing them with a single generic angle name.

Apply `Sq` to the measured result, not to an unrelated tolerance parameter. Existing `LengthSq`, `SizeXSq`, `differenceSq`, and their correctly spelled counterparts already follow the target rule.

Do not collapse `flip` with `reverse` for planes, rectangles, or other geometry without verifying identical semantics. Do not rename external duck-typed `X`, `Y`, `Z` or `x`, `y`, `z` member constraints.

For public field capitalization, retain serialized member names explicitly where needed, for example by preserving the old `DataMember` name. Update custom converters and serializer tests as necessary. Keep struct layout and serialized values unchanged.

## Transformation composition

Provide `Then` consistently on `Rotation2D`, `Quaternion`, `Matrix`, and `RigidMatrix`:

```fsharp
first.Then second
```

This always means apply `first`, then `second`. Add the static pipeline form:

```fsharp
first |> Matrix.andThen second
```

Use `andThen next current`; `then` alone is an F# keyword. Rename `Rotation2D.Add` / `add` to `Then` / `andThen` while retaining `AddDegrees` / `addDegrees` and their radians counterparts.

Keep mathematical `multiply` methods and multiplication operators with their current semantics. Quaternion multiplication currently applies the right operand first; matrix multiplication applies the first argument first. The new composition API does not silently reverse either operation.

Add new composition members to existing extension files where possible. Validate order using noncommuting rotations and a rotation-plus-translation example, away from the origin.

## Failure contracts and separate follow-up work

Preserve all existing throwing, `voption`, classification, and fallback contracts during renaming. Keep paired `try...` / throwing names together in the map.

The following are explicitly separate additions or behavior reviews, not prerequisites for the mechanical renames:

- Add `Quaternion.createFromMatrix` alongside the existing `tryCreateFromMatrix`.
- Consider useful `tryCreate` / `tryCreateFromPts` / `tryCreateFromPnts` forms for unit vectors and planes.
- Decide whether rotation component factories should normalize or validate consistently.
- Review negative/zero dimensions and bounds validation between rectangle and box factories.
- Review the 80 dimension gaps in the baseline API table individually. Do not invent a 3D orientation to mimic a 2D convenience function.
- Review whether numeric implementation helpers should remain public. Public `inline` callers may require them to remain accessible.
- Review the remaining member names that contain `Point`, about 217 in `Src/`, such as `ClosestPoint`, `closestPoint`, `closestPointIndex`, `addPoint`, `IsPoint` and `distanceRayPoint`, against the `Pt` / `Pnt` rule in AGENTS.md. This migration applies that rule only to factory names and to names newly created by overload splits.
- Unify the offset thresholds that differ between layers; see the defaults table in [Offset API names](#offset-api-names).

When adding a new tolerant factory, add its throwing counterpart in the same change. Reuse the existing `EuclidErrors` helpers and tolerance constants. Ordinary absence returns `ValueNone`; programming errors should retain their documented treatment.

## Execution phases

Implement on the currently checked-out branch. Each phase includes call-site updates, XML documentation, and the matching 2D/3D changes. Do not commit an intermediate state that does not build.

| Phase | Work | Completion evidence |
|:--|:--|:--|
| 1 | Extract the current public API, including obsolete definitions separately; expand these mapping rules into an exact symbol/signature migration list | Every affected overload has a unique target or an explicit tupled exception; no target collisions |
| 2 | Primitive factories, point factory restoration, rotation factories, then planes/matrices in compile order, including the new `Radians` factories | Factory partial application compiles; existing numeric tests pass; each `Radians` factory matches its `Degrees` partner |
| 3 | Lines, rectangles, all box types, polyline factories, and generated F# code strings | All active factory signatures follow the plan; ownership and constructor behavior preserved |
| 4 | Remove the obsolete `Points3D` overloads that take `ResizeArray`; then dot product splits, interpolation, projected parameters, point collections, triangle and topology operations | Operand associations and collection-index contracts preserved |
| 5 | Remove the obsolete XLine `get...` aliases; then intersection overload names, curried defaults and tupled `With` forms; the same for triangles, `Points3D` and offsets, including the `By<Noun>` offset names | Each old configurable call maps to a renamed `With` call with exactly the same effective settings |
| 6 | Movement aliases, mutation naming, shared-buffer names, spelling changes, and composition helpers | Copy/mutation distinctions tested; transform composition order explicit |
| 7 | Documentation, generated API tables, removal of the remaining obsolete names, consumer migration guide, and release checks | Both target-framework builds and .NET/JS/TS checks pass |

The implementation phases may be subdivided into smaller buildable commits. Phase order is constrained by the actual `<Compile Include>` order in [Euclid.fsproj](../Euclid.fsproj), especially factories called from earlier core files.

### Exact migration list

Before source edits, produce a machine-readable mapping keyed by declaring type, member name, static/instance kind, and full old argument signature. A member name alone is insufficient for overloads.

Each entry records:

- Old and new qualified names.
- Old argument groups and new argument groups.
- Argument permutation, with parameter types and names.
- Return type and behavior classification.
- Whether the entry is renamed, merged into an equivalent member, newly added, removed, or retained as an exception.
- Optional defaults and how to express the same settings at the new call site.
- Source location and 2D/3D counterpart, or the reason there is no counterpart.

Use FSharp.Compiler.Service, already used by [ApiTable.fsx](ApiTable.fsx), to inspect top-level argument groups. Do not rewrite nested tuples or function types with a textual `*` to `->` replacement.

Handle all obsolete definitions in the source, even though the generated API table excludes them. Remove an obsolete definition in the phase that would otherwise have to split, curry or rename it, or whose name collides with a new curried member. Examples are the XLine `get...` aliases in phase 5 and the `Points3D` overloads that take `ResizeArray` in phase 4. Remove the remaining obsolete definitions in phase 7. Preserve any deliberately obsolete constructor used as an internal inlining escape hatch unless that mechanism is redesigned separately.

Inspect source-level optional/default attributes as well as the displayed signatures. For example, `Line2D.toLine3D` has an `[<OPT; DEF(0.0)>]` attribute in its source although the table displays an ordinary `zLevel` argument. An API-table text scan alone is not a complete inventory of parameter metadata.

### Call sites and generated text

Update:

- All library calls, including core constructors, operators, JSON converters, and inline helpers.
- Tests in `Test/`, including manual Rhino scripts and benchmarks where they refer to renamed public APIs.
- `AsFSharpCode` implementations and expected strings in [TestAsFSharpCode.fs](../Test/TestAsFSharpCode.fs).
- README examples, XML `<see>` references, error-message operation names, and migration examples.
- The pairing/exception metadata in [ApiTable.fsx](ApiTable.fsx), followed by regeneration of all three API tables.
- The movement, mutation, optional-parameter, `With` / `By`, `Unit` prefix, name-order, abbreviation, same-type-argument, and angle naming rules in [AGENTS.md](../AGENTS.md) and [.claude/commands/checkTransformMethods.md](../.claude/commands/checkTransformMethods.md), so future checks enforce the new API.
- [CHANGELOG.md](../CHANGELOG.md), using the existing versioning mechanism and documenting source-breaking changes.

`Docs/index.md` is copied from README by the build target. Update the authoritative README and let the build refresh its copy. Do not edit generated API Markdown by hand.

Audit textual F# snippets especially carefully. Curried arguments that are negative literals or compound expressions need appropriate parentheses:

```fsharp
Vec.create (-1.0) (2.0 + offset) 3.0
```

Do not use blanket search-and-replace for overloaded calls. Type-check after each coherent batch, because an argument reorder involving several floats can compile while changing geometry.

## Validation

The following checks are required for implementation; saving this plan alone does not require running the geometry test suites.

### API and compilation checks

- Regenerate the API with `dotnet fsi Scripts/ApiTable.fsx`.
- Verify every multi-argument active `create...` / `tryCreate...` factory is curried, unless a recorded currying exception applies.
- Verify all renamed curried static members have unique names within their type.
- Verify each tupled `...With` form on a type keeps the old parameter names, order and defaults, so a named-argument call migrates by renaming only. For the `Offset2D` / `Offset3D` `With` forms, verify that each migrated call passes the same values to the same settings.
- Verify that every public member whose name ends in `With` is tupled, and that no offset variant uses `With<Noun>` for an extra geometric input.
- Verify `cross`, multiplication, constructors, operators, and other recorded exceptions retain their intended argument grouping.
- Verify already-curried members covered by the same-type exception become tupled, including `Box.offset` and `FreeBox.createFrom4Pts`, without reordering their arguments. Verify the four-bound XLine operations retain their tupled grouping and geometry-first order under the target names above.
- Check source and documentation for removed names; allow only intentional old-name entries in the migration guide.
- Compile partial applications for scalar factories, point-pair factories, plane factories, `dot`, and interpolation.
- Run source-generated F# examples through the existing `TestAsFSharpCode` test.
- Inspect generated TypeScript exports and type-check representative new factory calls; method renaming and overload removal may change emitted export names.

### Behavioral regression checks

- Use translated and rotated shapes with unequal dimensions and coordinates to catch valid-but-wrong argument permutations.
- Check cross-product sign and preserve left/right operand order.
- Check `lerp a b 0.0`, `lerp a b 1.0`, and intermediate values, including the existing unit-vector result types.
- Check all old default option combinations against their migrated forms, especially 2D/3D offset thresholds, loop handling, orientation handling, and supplied normals.
- Verify mutable operations change the input, copying operations preserve the input, and shared-buffer factories/accessors retain aliasing.
- Preserve zero-length, very-short-vector, parallel, nearly parallel, empty-input, duplicate-input, and zero-size behavior.
- Use noncommuting transformations to verify `Then` and `andThen`.
- Retain serialization behavior across field-name capitalization.

Extend existing test groups where practical. Add a test file only for a substantial new group, and register it in `Test.fsproj` and `TestMain.fs`. Do not create a separate test for every spelling-only rename.

### Commands from the repository root

```powershell
dotnet tool restore
dotnet build --configuration Release
dotnet run --project ./Test/Test.fsproj
npm ci --prefix ./Test
npm test --prefix ./Test
dotnet fsi Scripts/ApiTable.fsx
git diff --check
```

The build must cover `net6.0` and `net472`. The npm test command runs the configured JavaScript and TypeScript checks. Run .NET tests from the root so `TestAsFSharpCode` resolves project files correctly.

Run appropriate checks as each phase becomes buildable, and run both full test suites before declaring the implementation complete. Broaden performance testing only if a phase changes allocation behavior, dispatch, or hot-path implementation.

## Release deliverables

- Updated source and tests with both dimensions migrated together.
- Regenerated `ApiTable.md`, `ApiTable2D.md`, and `ApiTable3D.md`.
- A consumer-facing migration map, grouped by old type/member, with overload types and argument reorder examples.
- A breaking-change changelog entry and release version selected through the existing changelog mechanism.
- Updated README, XML documentation, and agent conventions.
- Recorded passing build, .NET test, JavaScript test, and TypeScript-check results.

Do not assume a source-level currying change is harmless to every consumer just because the CLR can flatten its arguments. Renames, extension-method placement, overload removal, generated Fable exports, and parameter metadata all require the release to be treated as a breaking migration.

## Implementation notes

Implemented on the `newAPI` branch, one commit per phase. The consumer guide is [Docs/Migration.md](../Docs/Migration.md), and the machine-readable map of every old and new signature is [ApiMigration.tsv](ApiMigration.tsv).
The call sites were migrated with a rewriter based on FSharp.Compiler.Service that resolves each call to its exact overload before it renames, curries or reorders the arguments.

Already done before this migration started, so nothing was left to change:

- The obsolete XLine `get...` aliases, the obsolete `Points3D` overloads that take `ResizeArray` and the other obsolete members, except the constructors kept for inlining, were removed in `b1a6028`. This includes `Points3D.areInLine` and `areInLineFast`, so they got no `With` forms.
- `Topology2D.sortToLoopCached` was removed in `604b6cc`. `Topology3D.sortToLoopCached` and `sortToLoopWithReversingCached` remain and are curried. They show as a gap in the API table.

Changes beyond the tables, made to follow the rules:

- `Rect2D` / `Rect3D` `grid`, `gridMinLength`, `gridMaxLength` and `subDivide` are curried with the rectangle last. `offsetCorner`, `offsetEdge`, `subDivideMinLength` and `subDivideMaxLength` stay tupled because they take more than three floats.
- `Line3D.intersectCone` and `tryIntersectCone` take the cone first and the ray last, like `XLine3D.classifyConeIntersection`.
- `Rect2D` / `Rect3D` `offsetVar` is now `offsetVariable`; `translateLocalX/Y/Z` of `Box`, `PPlane`, `Rect2D` and `Rect3D` is now `moveLocalX/Y/Z`.
- `Line2D` / `Line3D` `flip` was removed after checking that it was identical to `reverse`. `Rect3D.flip` is different and stays.
- `EuclidErrors.failQuat` is tupled because it takes four floats.
- `Offset2D` / `Offset3D` `removeUTurns` and `getOffsetDirections` stay tupled, like the other offset kernels.
- `Format.floatAsFSharpArgument` is a new internal helper, so `AsFSharpCode` can emit curried calls with negative numbers.

Left for follow-up work, besides the list above:

- Abbreviations that no table listed: `Pt.distPt`, `Pnt.distPnt`, `ResizeArr.getIdx`, `setIdx`, `lastIdx`, `UtilEuclid.safeIdx` and `Offset3D.setOffCornerVar`.
- The release version. The changes are listed under `[Unreleased]` in the changelog.
