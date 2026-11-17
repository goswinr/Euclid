---
name: euclid
description: Write and review F# code that uses the Euclid NuGet package, a 2D and 3D geometry library for .NET and Fable (JavaScript, TypeScript). Use when code opens the Euclid namespace or uses its types, such as Pt, Pnt, Vc, Vec, UnitVc, UnitVec, Line2D, Line3D, XLine2D, XLine3D, PPlane, NPlane, Rect2D, Rect3D, BRect, BBox, Box, FreeBox, Polyline2D, Polyline3D, Offset2D, Offset3D, Matrix, RigidMatrix, Quaternion or Rotation2D. Explains which type to pick, the naming grammar, the argument order, mutability, and the failure, tolerance and equality contracts. Contains API tables with the signature of every public member, so look up names there instead of guessing, decompiling the assembly or searching the web.
license: MIT
---

# Euclid

Euclid is a 2D and 3D geometry library in F# for precision engineering and computational design.
It runs on .NET and, through Fable, on JavaScript and TypeScript, with the same API.
All types are in the `Euclid` namespace: `open Euclid`.

## Look up the exact API

Do not guess member names or argument orders.
The tables in `references/` list every public type and member with its full signature, for this version of the package:

| File | Contents |
|:--|:--|
| [ApiTable2D.md](references/ApiTable2D.md) | 2D types: `Pt`, `Vc`, `UnitVc`, `Rotation2D`, `Line2D`, `XLine2D`, `Rect2D`, `BRect`, `Tria2D`, `Points2D`, `Offset2D`, `Polyline2D`, `Topology2D`, `Similarity2D` |
| [ApiTable3D.md](references/ApiTable3D.md) | 3D types: `Pnt`, `Vec`, `UnitVec`, `Quaternion`, `PPlane`, `NPlane`, `Matrix`, `RigidMatrix`, `MatrixDecomposition`, `Line3D`, `XLine3D`, `Rect3D`, `Box`, `FreeBox`, `BBox`, `Tria3D`, `Points3D`, `Offset3D`, `Polyline3D`, `Topology3D` |
| [ApiTableGeneral.md](references/ApiTableGeneral.md) | The exceptions in `EuclidErrors`, the tolerances in `UtilEuclid`, the angle measures in `Cosine` and `Tangent`, and `Format` |

The 2D and 3D tables have thousands of rows. Search them instead of reading them whole:

- Every type starts with a heading like `## Polyline3D` or `## module Offset3D`.
  Read from there to the next `## ` heading to see all members of one type.
- Every member is one table row: `` | `name` | `signature` | ``.
  Instance members and properties start with a dot, like `.DistanceTo`. Static members carry their type name, like `Pnt.distance`.
- So search for `` `Pnt.distance` `` to find one static member, or for `` `Polyline3D.offset `` to find a family of related members.
- A static member is listed right below the instance member of the same name.
  The static members without an instance counterpart, mostly factories, follow the bold row **Other static members** near the end of a table.

For the full doc comment of a member, read the XML documentation `lib/net6.0/Euclid.xml` or the F# sources in `fable/Src/` of the restored package.
NuGet keeps it in the global packages folder, by default `~/.nuget/packages/euclid/<version>/`, on Windows `%UserProfile%\.nuget\packages\euclid\<version>\`.

## Pick the right type

| Purpose | 2D | 3D |
|:--|:--|:--|
| Location | `Pt` | `Pnt` |
| Direction with a length, no location | `Vc` | `Vec` |
| Direction with a length of exactly 1.0 | `UnitVc` | `UnitVec` |
| Rotation around the origin | `Rotation2D` | `Quaternion` |
| Finite line segment from `From` to `To` | `Line2D` | `Line3D` |
| Line intersections and closest points | `XLine2D` | `XLine3D` |
| Oriented rectangle: origin, X axis and Y axis | `Rect2D` | `Rect3D` |
| Axis-aligned bounds | `BRect` | `BBox` |
| Triangle functions | `Tria2D` | `Tria3D` |
| Functions on lists of points | `Points2D` | `Points3D` |
| Polyline, mutable | `Polyline2D` | `Polyline3D` |
| Offsets of flat coordinate buffers | `Offset2D` | `Offset3D` |
| Sorting and joining lines into chains or loops | `Topology2D` | `Topology3D` |

Only in 3D: `PPlane` (origin and X, Y and Z axes), `NPlane` (origin and normal), `Box` (oriented box: origin and three axes),
`FreeBox` (eight freely placed corner points, mutable), `Matrix` (4x4 transformation, also with scale, shear or projection),
`RigidMatrix` (rotation and translation only) and `MatrixDecomposition`.
Only in 2D: `Similarity2D`, which groups similar sets of points.

`p.AsPt` drops the Z coordinate of a `Pnt`, and `pt.WithZ z` turns a `Pt` into a `Pnt`.

## Coordinate system

Right-handed with Z up, as in Rhino, Blender, SketchUp and Revit, unlike Unity or Unreal.
In 2D, +X points right, +Y points up, and positive angles rotate counter-clockwise.
In 3D, +X points right, +Y points forward and +Z points up.

## Calling conventions

```fsharp
open Euclid

let a = Pnt(1.0, 2.0, 3.0)              // constructor
let b = Pnt.create 4.0 6.0 3.0          // curried factory
let d = a.DistanceTo b                  // instance member: PascalCase
let m = Pnt.midPnt a b                  // static member: camelCase and curried
let z = UnitVec.ZAxis                   // static constant value: PascalCase, like Pnt.Origin and PPlane.WorldXY
let moved =
    a
    |> Pnt.move (Vec(10.0, 0.0, 0.0))   // the geometry is the last argument,
    |> Pnt.rotateOnZDegrees 90.0        // so static members chain with |>
```

- Static members take the configuration first and the geometry last:
  `Rect3D.createFromPlane sizeX sizeY plane`, `Pnt.distanceToLine line pnt`, `Polyline2D.offset distance polyline`.
  Natural orders stay: `Line3D.create start finish`, `Pnt.lerp start finish factor`.
- A member with optional settings has a curried plain form that uses the defaults, and a tupled `…Custom` form with optional named arguments:
  `Polyline2D.offset 2.0 pl` and `Polyline2D.offsetCustom(pl, 2.0, loop = true)`.
- These stay tupled, because their argument order matters or is easy to mix up:
  `cross`, `multiply`, the `create…Unchecked` factories, and members with more than three arguments of the same type, like `Quaternion.create`.

## Names follow a grammar

Use it to guess a name, then confirm the name in the tables.

- 2D names say `Pt` and `Vc`, 3D names say `Pnt` and `Vec`: `BRect.MinPt` and `BBox.MinPnt`, `Line2D.isParallelToVc` and `Line3D.isParallelToVec`.
  Indexed point access says `Point` in both: `GetPoint`, `SetPoint`, `MovePoint`.
- `XY` and `XYZ` mean raw float coordinates: `Pnt.distanceToXYZ x y z pnt`, `Polyline2D.moveXY pointIndex x y pl`.
- Factories start with `create` and name their inputs after `From`:
  `Line2D.createFromPtAndVc pt vc`, `PPlane.createFrom3Pnts origin xPnt yPnt`. `createCentered…` takes a center point.
- A raw angle argument names its unit: `rotateDegrees`, `rotateRadians`, `Matrix.createRotationZDegrees`.
  A name that ends in `Around` takes a center point as its first argument: `Pt.rotateDegreesAround center degrees pt`.
- An instance member that relates its geometry to another input ends in `To`: `a.IsParallelTo b`, `p.DistanceTo q`.
  Its static form drops the `To` for a symmetric relation of two geometries of the same type, and a predicate then starts with `are`: `Vc.areParallel minCosine a b`, `Pnt.distance a b`.
  A static member whose other input has another type keeps `To` and names that type: `Line3D.isParallelToVec minTangent vec ln`.
  A direction between two points says `FromTo`: `Pt.direction2PiFromTo fromPt toPt`.
- `Sq` means squared, which saves a square root: `Pnt.distanceSq`, `.LengthSq`.
- `move` is the only translation verb. There is no `translate`.
- A point containment test says `ContainsPt` or `ContainsPnt`: `Rect2D.containsPt pt rect`, `BBox.containsPnt pnt box`. A plain `Contains` on `BRect` and `BBox` takes another box.
- On immutable types, `with…` returns a copy with one component replaced: `Pnt.withX x pnt`.
- `Ray` means a line extended infinitely in both directions, not a half-line: `XLine2D.tryIntersectRays`, `BBox.tryIntersectRay`.
  A function that intersects two rays says `Rays`, a function that takes one ray and another geometry says `Ray`.
  Parameters on a ray are 0.0 at its `From` point and 1.0 at its `To` point. Values below 0.0 or above 1.0 are valid results.

## Failure contract

The name of a member tells what it does when there is no result:

| Form | Behavior | Example |
|:--|:--|:--|
| `operation` | Returns the result, or throws if its preconditions fail | `Vec.unitize`, `PPlane.intersectLine` |
| `tryOperation` | Returns a `voption`: `ValueSome` or `ValueNone`, never an `option` | `Vec.tryUnitize`, `PPlane.tryIntersectLine` |
| `classify…` | Returns a discriminated union with a case for every relationship | `XLine2D.classifyIntersection` |
| `operationOrDefault` | Returns the given fallback instead of throwing | `Vec.unitizeOrDefault fallback v` |
| `operationUnchecked` | Skips the documented checks, for inputs that are already validated | `Vec.unitizeUnchecked`, `UnitVec.createUnchecked` |

Use `try…` or `classify…` when a missing result is an ordinary case, like parallel lines or a line that misses a plane.
Two finite lines only have these two forms, like `Line2D.tryIntersect` and `XLine2D.classifyIntersection`. Two rays also have the throwing `intersectRays`.
Handle degenerate input explicitly: zero-length lines and vectors (`.IsZeroLength`, `Vec.tryUnitize`), parallel and almost parallel lines, and empty or single-point lists.

The exceptions are in the module `EuclidErrors`: `EuclidException`, `EuclidDivByZeroException`, `EuclidUnitizingException`,
`EuclidTooSmallException`, `EuclidNanInfinityException`, `EuclidEmptySeqException` and a few more.
Each one derives directly from `System.Exception`, not from `EuclidException`, so `:? EuclidException` does not catch the others.

## Tolerances and equality

- The geometry types have `[<NoEquality; NoComparison>]`, so `a = b` and `compare a b` do not compile for them.
  Compare them with the static members `equalsExact` and `equalsWithin`, like `Pnt.equalsWithin 1e-9 a b`.
- `equalsExact a b` compares every field with `=`.
  `equalsWithin tol a b` lets every coordinate differ by up to `tol`, each one separately, not by the Euclidean distance.
  To compare by distance, use `Pnt.distanceSq a b < tol * tol`.
- Angle tolerances are units of measure, not degrees: `float<Cosine.cosine>` for vectors and `float<Tangent.tangent>` for lines.
  Pass one of the precomputed values, which are named by their angle in degrees:

  ```fsharp
  let vecsParallel = Vec.areParallel Cosine.``1.0`` vecA vecB      // within 1 degree, either orientation
  let linesParallel = Line3D.areParallel Tangent.``0.25`` lnA lnB  // within 0.25 degrees
  ```

  For other angles, lift a computed cosine or tangent to the measure with `UtilEuclid.withMeasure`, or with the `!^` operator after `open Euclid.UtilEuclid`.
- `UtilEuclid.isTooSmall` is TRUE below 1e-6. `UtilEuclid.zeroLengthTolerance` is 1e-12: shorter vectors cannot be unitized.

## Points, vectors and transformations

- `Pnt - Pnt` is a `Vec`, `Pnt + Vec` is a `Pnt`, `Vec + Vec` is a `Vec` and `Vec * float` scales. `a *** b` is the dot product of two vectors.
- Vectors have no location, so they have no `move`, `rotateAround` or `scaleOn`.
  Rotate them around the origin with `Vec.rotate quaternion v`, `Vec.rotateOnZDegrees degrees v` or `Vec.transformRigid rigidMatrix v`.
  A `Vec` has no transformation by a full `Matrix`. Transform points or lines instead.
- `.Unitized` and `Vec.unitize` throw for vectors shorter than 1e-12. Use `Vec.tryUnitize` when the length can be zero.
  `UnitVec.create x y z` unitizes its input.
- Combine transformations with `a.Then b` or `a |> Matrix.andThen b`.
  Both apply `a` first, on `Matrix`, `RigidMatrix`, `Quaternion` and `Rotation2D` alike.
- Avoid `***` for combining: `m1 *** m2` applies `m1` first, but for quaternions `q1 *** q2` is the Hamilton product and applies `q2` first.
  `***` is also right-associative in F#.

## Mutability

- Every type is immutable, except `Polyline2D`, `Polyline3D` and `FreeBox`.
- On these three, instance members change the object in place and return `unit`: `pl.Move v`, `pl.Reverse()`.
  Their static members return a changed copy: `Polyline2D.move v pl`.
- The indexed setters and movers, and `addPoint`, mutate as static members too: `Polyline2D.setPoint pointIndex pt pl`, `Polyline2D.movePoint pointIndex vc pl`.
  Setting or moving the first or last point of a closed polyline opens it. The `…PreservingClosure` variants change both points.
- `XYs` and `XYZs` expose the live flat coordinate buffer, and `createFromXYs` and `createFromXYZs` share the given buffer without copying it.
  Call `.Duplicate()` before you change an instance that other code holds.

## Platforms and debugging

- Fable compiles Euclid from source. Define `CHECKED_EUCLID` to compile in extra checks for NaN, unit lengths, index bounds and the inputs of `…Unchecked` factories.
  The .NET package on NuGet is built without these checks.
- `.AsString` gives a short readable text of a value, and `.AsFSharpCode` gives F# code that recreates it, for example for a failing test case.
