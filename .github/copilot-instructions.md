---
applyTo: "**"
---

# Project general coding standards

## General Rules for Agents
Please ask questions if any of these guidelines are unclear.
If you are unsure how to interpret a guideline in a specific situation, please ask for clarification before proceeding.
If you are unsure how to interpret a prompt or task, please ask for clarification before proceeding.

## Naming Conventions
- Use PascalCase for types, Discriminated Unions, instance members and modules
- Use camelCase for variables, functions, and static members
- Use double-backtick-escaped names like  ``some name with spaces`` for otherwise invalid identifiers

## Error Handling
- All error handling should use the `EuclidErrors` module from `EuclidErrors.fs`.
- When doing division, always check for zero denominators and throw an Exception via `EuclidErrors`.
 e.g. `failDivide` or `failTooSmall`
- Format exception messages consistently using the provided helper functions in `EuclidErrors.fs`.
- Format strings with interpolated strings `$"...{value} ...` for clarity.

## Whitespace
- F# is whitespace sensitive, just like python, the indentation with spaces defines the scope.
- Use 4 spaces for indentation, no tabs.

## Core Architecture
- Single F# library (`Euclid.fsproj`) targeting `net472` and `net6.0` with zero runtime dependencies.
- Geometry primitives are lightweight immutable structs: 2D (`Pt`, `Vc`, `UnitVc`) and 3D (`Pnt`, `Vec`, `UnitVec`).
- Rich functionality is added via extension modules placed in `Src/TypeExtensions/*` to keep core structs minimal for inlining and allocation reduction.
- Higher-order constructs: lines (`Line2D`, `Line3D`), planes (`PPlane`, `NPlane`), boxes (`BBox`, `Box`, `FreeBox`), triangles, polylines, matrices, quaternions, rotations, topology helpers, and offset algorithms.
- `Offset2D.fs` implements sophisticated constant and variable distance polyline offsets (miters, chamfers, U-turn handling). `Offset3D.fs` adapts these to 3D with local vertex normals.
- No global service layer; design is functional & modular: each file is an isolated domain unit.

## Key Design Conventions
- 2D vs 3D type naming is consistent: (`Pt`/`Pnt`, `Vc`/`Vec`, `UnitVc`/`UnitVec`). Maintain this symmetry in new APIs.
- Do not put behavioral members on struct types; add them as extension members in the corresponding file under `Src/TypeExtensions/`.
- Use inline functions for small numeric operations to enable optimizations.

- Robust numeric tolerance patterns: use `UtilEuclid.fs`, for checking near-zero, near-equal, and angle thresholds. eg `isTooSmall` or `isTooSmallSq` if the squared value is given. to save on `sqrt` computations.
- Angle thresholds use the `Cosine` or `Tangent` measured type in double backticks e.g. `Cosine.``175.0`` ` . When comparing a raw dot product, lift floats to the measure type with  with `UtilEuclid.withMeasure` or `!^` operator depending on context.

## Coordinate System

This library uses a **right-handed coordinate system** with the **Z-axis pointing up**.

## Performance Considerations
- Avoid allocating intermediate arrays; prefer `ResizeArray<_>` reuse patterns aligned with existing modules.
- Keep hot-path math inline; avoid unnecessary allocations (e.g., tuple deconstruction causing closures).

## Adding New Geometry
1. Add functional operations as extension members in a matching file under `Src/TypeExtensions`.
2. Mirror 2D/3D API naming where sensible.
3. Provide tolerant versions if operations can degenerate (colinearity, zero-length, parallel tests).

## Testing Workflow
- Tests are located in `Tests` folder using the `Euclid.Tests.fsproj` project.
- When adding test always try to cover special cases, like:
    - zero length vectors,
    - very short vectors,
    - parallel lines or vectors,
    - almost parallel lines or vectors,
    - empty collections
    - single element collections
    - duplicate elements in collections

- If Tests fail that should pass according to the docstring of a function, then try to fix the implementation of the function, not the test. And report the issue if you are unsure.

- Test Run in both .NET and Node.js environments. via Expecto and Fable.Mocha
.NET:
`dotnet run --project ./Tests/Euclid.Tests.fsproj`
Node.js:
`npm run test --prefix ./Tests`

## Build & Docs
- Build: `dotnet build` (multi-target). Package metadata is managed via project file & CHANGELOG automation.
- API docs generated with fsdocs (see `Docs/`). Keep public XML doc comments concise and technical.

## Error & Validation Strategy
- Prefer early validation and fail-fast with detailed context (indices, values, thresholds) matching existing message style.
- For performance-critical code, guard only essential invariants (e.g., zero-length, NaN, Infinity in DEBUG mode).

## Numeric Stability
- Avoid recomputing square roots when squared values suffice (pattern: `LengthSq` vs `Length`).
- For near parallel / colinear detection use squared magnitude thresholds (< 1e-18 etc.). Follow constants already in use to maintain consistency.

## Contribution Tips for AI
- Before adding a feature, search for an existing pattern in 2D or 3D counterpart and replicate style.
- Keep diffs tight; do not reformat unrelated code.
- Do not introduce external dependencies.
- Preserve existing public API signatures unless explicitly extending; versioning is tracked for NuGet.


## Examples
- See `UnitVec` and `UnitVc` files for unit length invariant enforcement.


## Documentation
- Use XML doc comments `///` for all public members with `<summary>`, `<param>`, and `<returns>` tags.
- Keep comments concise and focused on behavior, parameters, and return values.
- Put ASCII art diagrams in `<code>` sections to enforce formatting.
- Booleans are written in CAPITALIZED form in doc comments: TRUE, FALSE.


Keep responses concise, reference concrete files (e.g., `Offset2D.fs: line logic for UTurnBehavior`) and prefer implementing over suggesting when tools allow code edits.
