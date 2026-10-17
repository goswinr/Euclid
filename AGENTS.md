# AGENTS.md

Euclid is a 2D and 3D geometry library in F# for precision engineering and computational design.
It runs on .NET and, through Fable, on JavaScript and TypeScript.
It ships to NuGet, so its public API is versioned.

If a guideline or task is unclear, ask before you proceed.

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
- For otherwise invalid identifiers, use double-backtick names such as ``` ``some name`` ```.
- Use interpolated strings for messages: `$"... {value} ..."`.
- Keep diffs tight. Do not reformat unrelated code.

## Design rules

- **2D/3D symmetry.** Mirror names across dimensions: `Pt`/`Pnt`, `Vc`/`Vec`, `UnitVc`/`UnitVec`, `Line2D`/`Line3D`. Before you add a feature, find the counterpart in the other dimension and copy its style.
- **Keep the structs minimal.** Do not put behavior on the core struct types. Add it as extension members in the matching file under `Src/TypeExtensions/`.
- **Points and vectors are different types.** Vectors carry no location, so they get no `translate` or `rotateWithCenter` members.
- **No new dependencies.** The library has no third-party runtime dependencies. It only uses `System.Text.Json` on net472.
- **Preserve public signatures** unless you are explicitly extending the API.
- **Handle degenerate cases.** Provide tolerant variants for operations that can degenerate, such as zero length, colinearity or parallel inputs.

## Performance

- Inline small numeric functions and hot-path math.
- Avoid intermediate allocations. That includes temporary arrays, tuple deconstruction that creates closures, and throwaway `Pnt`/`Vec`/`Pt`/`Vc` values. Prefer `ResizeArray` reuse patterns that match the existing modules.
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

## Agent commands

Reusable workflows live in `.claude/commands/`:

- `checkFile`: verify that docstrings match the implementation.
- `checkTests`: add tests for a source file.
- `checkTransformMethods`: check that a type has the standard transform methods.
- `findAllocations`: remove intermediate allocations while keeping results bit-identical.
- `standards`: load this file.
