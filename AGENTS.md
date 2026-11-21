# AGENTS.md

Euclid is a 2D and 3D geometry library in F# for precision engineering and computational design.
It runs on .NET and, through Fable, on JavaScript and TypeScript.
It ships to NuGet, so its public API is versioned.

Ask the user before you proceed if a prompt, task or guideline is unclear,
or if there are several reasonable ways to design the solution.
In that case, list the options with their trade-offs and let the user choose.

## Commands

Requires the .NET SDK 10.0 or later. Node.js is only needed for the JS tests.
Run every command from the repo root.

| Task | Command |
|------|---------|
| Restore tools (Fable, fsdocs) | `dotnet tool restore` |
| Build (net6.0 + net472) | `dotnet build --configuration Release` |
| Test on .NET | `dotnet run --project ./Test/Test.fsproj` |
| Test on JS + TS type check | `npm ci --prefix ./Test && npm test --prefix ./Test` |

- Run the .NET tests from the root, not from inside `Test/`. `TestAsFSharpCode` resolves project files relative to the root.
- CI (`.github/workflows/build.yml`, `test.yml`) runs both test suites on every PR to `main`. Run both before you call a change done.
- The root `Tests/` folder holds ignored build output. The test sources are in `Test/`.

### Cloud sessions (Claude Code on the web)
The .NET SDK is not preinstalled in cloud containers.
The `SessionStart` hook `.claude/hooks/session-start.sh` installs it **asynchronously**, so `dotnet` may be missing from `PATH` for the first few minutes.
If you get "command not found", the environment is not broken: wait and retry.

## Layout

- `Euclid.fsproj`: the single library project. F# compile order is the `<Compile Include>` order, so register each new file in the right position.
- `Src/`: one file per domain unit, with no global service layer.
  - Primitive structs: 2D `Pt`, `Vc`, `UnitVc` and 3D `Pnt`, `Vec`, `UnitVec`.
  - Higher-order types: lines and `XLine*`, planes (`PPlane`, `NPlane`), rectangles, boxes (`BBox`, `BRect`, `Box`, `FreeBox`), triangles, polylines, `Matrix`, `RigidMatrix`, `Quaternion`, `Rotation2D`, `Similarity2D`, topology, and `PolyLabel`.
  - `Offset2D.fs`: constant and variable-distance polyline offsets with miters, chamfers and U-turn handling. `Offset3D.fs` adapts them to 3D with local vertex normals.
- `Src/TypeExtensions/`: behavior for the core structs, added as extension members.
- `Src/EuclidErrors.fs`: all exception helpers.
- `Src/UtilEuclid.fs`: numeric tolerance helpers.
- `Test/`: the test project (`Test.fsproj`, net10.0) and `TestMain.fs`, the entry point that lists every test group.
- `Test/TestInRhino/`, `Test/Benchmarks/`: manual visual checks and benchmarks. They are not part of the test run.
- `Docs/`: fsdocs API documentation.
- `CHANGELOG.md`: drives the package version through Ionide.KeepAChangelog.

## Coordinate system

The coordinate system is right-handed with Z up, as in Rhino, Blender and Revit.
In 2D, +X points right and +Y points up.
In 3D, +X points right, +Y points forward and +Z points up.

## Code style

- Indent with 4 spaces and no tabs. F# scope depends on indentation.
- Use PascalCase for types, discriminated unions, instance members and modules.
- Use camelCase for variables, functions and static members.
- Static constant values are the exception, they are PascalCase: `Vec.XAxis`, `Pnt.Origin`, `PPlane.WorldXY`, `Quaternion.Identity`.
- For otherwise invalid identifiers, use double-backtick names such as ``` ``some name`` ```.
- Use interpolated strings for messages: `$"... {value} ..."`.
- Keep diffs tight. Do not reformat unrelated code.

## Design rules

- **2D/3D symmetry.** Mirror names across dimensions: `Pt`/`Pnt`, `Vc`/`Vec`, `UnitVc`/`UnitVec`, `Line2D`/`Line3D`. Before you add a feature, find the counterpart in the other dimension and copy its style.
  - Member names follow the same split: a 2D member that takes or returns points or vectors says `Pt` or `Vc` in its name, and its 3D counterpart says `Pnt` or `Vec`. Examples: `BRect.MinPt`/`BBox.MinPnt`, `Tria2D.offsetPt`/`Tria3D.offsetPnt`, `Line2D.isParallelToVc`/`Line3D.isParallelToVec`.
  - A plain `Contains` takes a geometry of the same type, as in `BRect.Contains otherRect`. A point test says `ContainsPt` or `ContainsPnt`, also on `Rect2D`, `Polyline2D` and `Box`.
  - The indexed point accessors of polylines and boxes are the exception. They say `Point` in both dimensions: `GetPoint`, `SetPoint` and `MovePoint` on `Polyline2D`, `Polyline3D`, `FreeBox`, `Box` and `BBox`.
  - Some features exist in one dimension on purpose. `API-Table/ApiTable.fsx` lists them with their reasons, so do not add them to the other dimension. For example, the 3D offsets have no U-turn behaviors like `Offset2D.UTurn` and no `VarDistParallel.StepWithTwoPoints`, because they cannot be handled consistently with local 3D planes.
- **Keep the structs minimal.** Do not put behavior on the core struct types. Add it as extension members in the matching file under `Src/TypeExtensions/`.
- **Points and vectors are different types.** Vectors carry no location, so they get no `move`, `rotateAround` or `scaleOn` members.
- **Only polylines and `FreeBox` are mutable.** `Polyline2D`, `Polyline3D` and `FreeBox` can be changed in place. Every other type is immutable, so do not add in-place mutation to it.
- **Rays are infinite lines.** A ray is a `Line2D` or `Line3D` extended infinitely in both directions, not a half-line. Name and document `*Ray*` functions this way. A function that intersects two rays says `Rays`, as in `intersectRays`, `tryIntersectRays` and `doRaysIntersect`. A function that takes one ray and another geometry says `Ray`, as in `PPlane.tryIntersectRay`.
- **No new dependencies.** The library has no third-party runtime dependencies. It only uses `System.Text.Json` on net472.
- **Preserve public signatures** unless you are explicitly extending the API.
- **Handle degenerate cases.** Provide tolerant variants for operations that can degenerate, such as zero length, colinearity or parallel inputs.
- **One failure contract.** The name of a member says how it handles a missing result:

  | Form | Contract |
  |------|----------|
  | `operation` | Returns the result. Throws through `EuclidErrors` when its preconditions fail. |
  | `tryOperation` | Returns a `voption`, never an `option`. Returns `ValueNone` when an ordinary geometric result may be absent. |
  | `classify…` | Returns a discriminated union that describes the relationship. |
  | `operationOrDefault` | Returns an explicit fallback value. |
  | `operationUnchecked` | Skips the documented checks. |

  Instance members use the same prefixes in PascalCase, for example `b.TryIntersectRay` and `b.IntersectRay`.
  When you add a `tryOperation`, add the throwing `operation` next to it.
  The intersections of two finite lines are the exception: they only have the `try` and `classify` forms, such as `Line2D.tryIntersect` and `XLine2D.classifyIntersection`. Only the functions on rays also get a throwing partner, such as `intersectRays` next to `tryIntersectRays`.
- **One equality contract.** `equalsExact a b` compares each field with `=`. `equalsWithin tol a b` and `notEqualsWithin tol a b` apply the tolerance to each coordinate separately, not to the Euclidean distance. Say so in the docstring. Do not add `equals` members that take a tolerance.
- **Serialized names are API names.** A JSON property has the name of the field or property it stores, such as `XAxisX` or `XYs`. Mark the member with a plain `[<DataMember>]`, without `Name = …`, and write and read the same name in the System.Text.Json converter. Do not accept other names when reading.

## API shape

- **Curried static members.** A static member with more than one argument is curried, so it works with `|>` and partial application. A curried name has exactly one signature: split overloads into explicit names, such as `Vc.create x y` and `Vc.createFromTo start finish`. These stay tupled:
  - members with more than three arguments of the same type, such as `Quaternion.create` or `FreeBox.createFrom4Pts`. `float` and `float<Cosine.cosine>` count as different types, and so do `Vec` and `UnitVec`. If the last argument, the geometry, has another type, the member may still be curried, as in `Box.offset distBottom distFront distRight distBack distLeft distTop box`;
  - `create…Unchecked` factories;
  - `cross`, `determinant3`, `Tria2D.det` and the `multiply` methods, because their operand order matters;
  - names ending in `Custom`;
  - the raw kernels in `XLineXY` and `XLineXYZ`, and the offset kernels `…WithDirections`, `removeUTurns` and `setOffCorner` in `Offset2D` and `Offset3D`, and `getOffsetDirections` in `Offset3D`.
- **Argument order.** Put configuration first and the geometry last: sizes before a plane, distances before a line, tolerances before inputs. Keep natural orders such as `x y z`, `start finish` and `from to`. Interpolation keeps the factor last: `lerp start finish factor`. A line from a point and a vector takes the point first: `Line2D.createFromPtAndVc point vector`. Tupled members follow the same order, as in `XLine2D.isWithinRanges(minParameterA, maxParameterA, minParameterB, maxParameterB, lineA, lineB)`. Only the `…Custom` forms take the geometry first, because optional parameters must come last, and the raw kernels in `XLineXY` and `XLineXYZ` take the coordinates first and the settings last.
- **Optional parameters.** A static member with more than one optional parameter gets a curried default under its plain name and a tupled `…Custom` form that keeps the optional parameters, for example `XLine2D.classifyIntersection lineA lineB` and `XLine2D.classifyIntersectionCustom(lineA, lineB, tangent = t)`. A static member with only one setting, a tolerance, gets no `…Custom` form. It gets a curried default under its plain name and a curried `…Within` form that takes the tolerance first, like `equalsWithin`: `XLine2D.doOverlap lineA lineB` and `XLine2D.doOverlapWithin tolerance lineA lineB`. Instance members keep their optional parameters, as in `ln.IsParallelTo(other, minTangent = t)`. Module functions cannot have optional parameters, so their `…Custom` form takes every setting explicitly. Do not add options records. An extra geometric input of an offset variant is named `By<Noun>`, as in `offsetByNormal`. Other names keep `With` in its plain meaning, as in `createFromPtWithZ` or `unitizeWithErrorMessage`.
- **Names.** Compose a name as operation, object or input type, `Sq`, `Degrees` or `Radians`, representation such as `FromPtsAndVcs`, `Custom` or `Within`, and finally `Unchecked` or `OrDefault`: `distanceToLineSq`, `createRotationFromDegreesVecAround`, `tryIntersectRaysCustom`, `tryFindSelfIntersectionWithin`. A name that takes a center point ends in `Around`, after every other part: `rotateAround`, `rotateDegreesAround`, `rotateOnZAround`. The parts that name curried arguments come in reverse argument order, so the last argument is named first: `createRotationFromDegreesVecAround center axis degrees`, `Box.createFromPntsAndPlane plane points`. The geometry that a function acts on is usually not named. Fused sequences keep their natural order, such as `XY`, `XYZ`, `EulerXYZ`, `createFromTo` and `createPlaneToPlane`. A name also says a point before a vector and the X axis before the Y axis, whatever the argument order: `Line2D.createFromPtAndVc point vector`, `Box.createFromPntsAndVecs dirX dirY points`, `PPlane.createFromXAxisAndYAxis xAxis yAxis origin`. Spell out abbreviations: `Distance`, `Index`, `Variable`, `Degrees`, `ErrorMessage`, `Parameter`, `Relative`, `Closest`, `Direction`. The existing type and union names `XParam`, `XRayParam`, `ClParams`, `ClPts`, `ClPnts` and `VarDistParallel` keep their short form. A member that moves along or rotates on an axis of the geometry itself says `Local`, as in `moveAlongLocalX` and `rotateOnLocalZDegrees`; `rotateOnZ` always means the world Z-axis. A raw angle argument names its unit, and every `Degrees` factory has a `Radians` partner.
- **Relations to another input.** An instance member that relates its geometry to another input ends in `To`: `a.IsParallelTo b`, `p.DistanceTo q`.
  - A static member of a symmetric relation between two geometries of the same type has no `To`. A predicate starts with `are`: `Vc.areParallel minCosine a b`, `Line2D.areCoincidentFast tolerance lnA lnB`. Other functions just drop the `To`: `Pt.distance a b`.
  - A static member whose other input has another type keeps `To` and names that type: `Line2D.isParallelToVc minTangent vc ln`, `Pt.distanceToLine ln pt`.
  - A direction between two points says `FromTo` and takes the points in that order: `Pt.direction2PiFromTo fromPt toPt`.
  - When the static name differs from the instance name, the docstring of each says "The static counterpart is …" or "The instance counterpart is …". If no rule in `instanceNames` in `API-Table/ApiTable.fsx` pairs them, add one, so that the API table lists the static member below its instance counterpart.
- **Factories.** Factories start with `create`. `From` names the inputs. When a center point is also given, `Around` is appended as the last word, as in `createRotationFromDegreesVecAround center axis degrees`. A leading `Unit` means every vector input is a unit vector, as in `createFromUnitXAxisAndNormal`. Use `createCentered…` for factories that take a center point.
- **Axes.** Instance members and fields spell axes `XAxis`, `XAxisUnit` and `XAxisX`; the static world axes are `Vec.XAxis`, `UnitVec.ZAxis` and so on.
- **Movement and composition.** `move` and `Move` are the only translation verbs; do not add `translate` aliases. `a.Then b` and `a |> T.andThen b` first apply `a` and then `b`, on `Rotation2D`, `Quaternion`, `Matrix` and `RigidMatrix`.
- **Mutation.** On `Polyline2D`, `Polyline3D` and `FreeBox`, instance transforms mutate in place and return `unit`, and the matching static members return a copy. The indexed point setters and movers (`setPoint…`, `setXY…`, `setXYZ…`, `movePoint…`, `moveXY…`, `moveXYZ…`) and `addPoint`, `addXY` and `addXYZ` mutate as static members too. The indexed movers take the point index first, as in `moveXY pointIndex x y polyline`, unlike the whole-geometry `moveX`. Their variants that keep a closed polyline closed end in `PreservingClosure`, as in `setXYPreservingClosure`. A point index alone does not mean mutation: `getPoint` only reads. The live buffer is `XYs` or `XYZs` (static `xys` or `xyzs`), and `createFromXYs` and `createFromXYZs` build an instance that shares the given buffer. On immutable types, an operation that replaces one component and returns a copy is named `with…`, never `set…`. Do not use an `InPlace` suffix.

## Performance

- Inline small numeric functions and hot-path math.
- Avoid intermediate allocations. That includes temporary arrays, tuple deconstruction that creates closures, and throwaway `Pnt`/`Vec`/`Pt`/`Vc` values. Prefer `ResizeArray` reuse patterns that match the existing modules.
- Public members that return several values return a plain F# tuple, such as `Quaternion * Vec`, not a struct tuple. Struct tuples are awkward to use in F#, so the allocation is acceptable there. Private helpers on hot paths may still use struct tuples.
- To remove temporaries without changing results, follow `.claude/commands/findAllocations.md`.
- Use squared lengths (`LengthSq`) when they are enough, and avoid unnecessary `sqrt`.

## Numerics and errors

- Use the tolerance helpers in `UtilEuclid.fs`, such as `isTooSmall`, or `isTooSmallSq` for squared values. Reuse the constants that are already in use; do not invent new thresholds.
- Express angle thresholds as `Cosine` or `Tangent` measures, for example ``Cosine.``175.0`` ``. To compare against a raw dot product, lift the float with `UtilEuclid.withMeasure` or the `!^` operator.
- Throw every exception through `EuclidErrors`, using helpers such as `failDivide` and `failTooSmall`. Check every denominator for zero.
- Fail fast with context in the message: indices, values and thresholds, in the existing message style.
- On hot paths, guard only essential invariants such as zero length, NaN and Infinity. Expensive checks belong behind `DEBUG` or `CHECKED_EUCLID`.

## Testing

- Tests use Scriptorium.Quill and Scriptorium.Nib. `Test/ScriptoriumTestCompat.fs` provides Expecto-style `Expect.*` helpers and the `Accuracy` levels.
- A new test file must be added to `Test.fsproj` and its tests registered in `TestMain.fs`.
- Cover the edge cases:
  - zero-length and very short vectors
  - parallel and almost parallel lines or vectors
  - empty, single-element and duplicate-element collections
- Place test shapes away from the origin and rotate them. Shapes that are axis-aligned at the origin hide translation and rotation bugs.
- If a test fails that should pass according to the function's docstring, fix the implementation, not the test. If you are unsure which is wrong, report it.

## Documentation

- Every public member gets XML doc comments (`///`) with `<summary>`, `<param>` and `<returns>`.
- Keep comments concise and technical: describe behavior, parameters and return values.
- Write booleans in capitals in doc comments: TRUE, FALSE.
- Put ASCII diagrams inside `<code>` blocks so their formatting is preserved.

## Git

- Commit to the branch that is currently checked out. Do not create or switch branches for your commits.

## Agent commands

Reusable workflows live in `.claude/commands/`:

- `checkFile`: verify that docstrings match the implementation.
- `checkTests`: add tests for a source file.
- `checkTransformMethods`: check that a type has the standard transform methods.
- `findAllocations`: remove intermediate allocations while keeping results bit-identical.
- `standards`: load this file.
