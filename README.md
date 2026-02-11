![Logo](https://raw.githubusercontent.com/goswinr/Euclid/main/Docs/img/logo128.png)

# Euclid

[![Euclid on nuget.org](https://img.shields.io/nuget/v/Euclid)](https://www.nuget.org/packages/Euclid/)
[![Build Status](https://github.com/goswinr/Euclid/actions/workflows/build.yml/badge.svg)](https://github.com/goswinr/Euclid/actions/workflows/build.yml)
[![Docs Build Status](https://github.com/goswinr/Euclid/actions/workflows/docs.yml/badge.svg)](https://github.com/goswinr/Euclid/actions/workflows/docs.yml)
[![Test Status](https://github.com/goswinr/Euclid/actions/workflows/test.yml/badge.svg)](https://github.com/goswinr/Euclid/actions/workflows/test.yml)
[![license](https://img.shields.io/github/license/goswinr/Euclid)](LICENSE.md)
![code size](https://img.shields.io/github/languages/code-size/goswinr/Euclid.svg)

A comprehensive 2D and 3D geometry library in F# without dependencies, designed for precision engineering and computational design.<br>
It runs on JavaScript too.


## Features

While most 3D geometry libraries just use a generic type ( often called Vec3 or similar) containing three floats for both points and vectors,<br>
Euclid makes a distinction to preserve semantic meaning and avoid accidental misuse.<br>
A point represents a position in space, while a vector represents a direction and magnitude.<br>
Even unitized vectors are a separate type. They are guaranteed to have a length of 1.0.<br>
This helps to skip checks for zero length vectors in many operations.


🎯 **Core Geometry Types**
- Points (`Pt`, `Pnt`), Vectors (`Vc`, `Vec`), Unit Vectors (`UnitVc`, `UnitVec`)
- Lines, Planes, Boxes, Bounding Boxes, Polylines
- Rotations, Quaternions, 4x4 and rigid orthonormal 4x3 matrices

✨ **Key Characteristics**
- **Zero dependencies:**  lightweight and self-contained
- **Double precision:**  designed for CAD/manufacturing accuracy
- **Immutable types:**  all primitive types are immutable for safety and ease of reasoning
- **Cross-platform:**  compiles to .NET, JavaScript, TypeScript, Rust, Python via Fable
- **Performance Focus:**  All small types are structs, functions are often inline and try to minimize the allocation.
- **Interoperability:**  Can be used with [Rhino3D](https://github.com/goswinr/Euclid.Rhino), Revit, or any .NET environment, and also in JavaScript/TypeScript projects via Fable.


## Coordinate System

This library uses a **right-handed coordinate system** with the **Z-axis pointing up**.

✅ **Same as in:** Rhino3D, Blender, SketchUp, Revit, AutoCAD<br>
❌ **Different from:** Unity, Unreal Engine, Maya

This choice aligns with industry-standard CAD and architectural software.

## Design Philosophy

### Points vs Vectors
- **Points** (`Pt`, `Pnt`): Positions in space
- **Vectors** (`Vc`, `Vec`): Directions and displacements

When a 4x4 transformation matrix is applied:
- **Points**: Undergo full transformation (rotation, scaling, translation)
- **Vectors**: Only rotate and scale (no translation)

This follows homogeneous coordinate conventions where vectors have w=0.

### Naming Conventions

**Core Types (2D and 3D):**

| Type | 2D | 3D |
|------|----|----|
| Point | `Pt` | `Pnt` |
| Vector | `Vc` | `Vec` |
| Unit Vector | `UnitVc` | `UnitVec` |
| Line | `Line2D` | `Line3D` |
| Polyline | `Polyline2D` | `Polyline3D` |
| Rectangle | `Rect2D` | `Rect3D` |
| Bounding Rect/Box | `BRect` | `BBox` |

**3D-only Types:**

| Type | Description |
|------|-------------|
| `PPlane` | Parametrized plane (origin + X/Y/Z axes) |
| `NPlane` | Normal plane (origin + normal vector) |
| `Box` | Oriented 3D box (origin + 3 axis vectors) |
| `Matrix` | 4x4 transformation matrix |
| `RigidMatrix` | 4x3 rigid transformation (rotation + translation only) |
| `Quaternion` | Unitized quaternion for 3D rotations |
| `Rotation2D` | 2D rotation stored as sine/cosine pair |

### Function Patterns
Functions are available in multiple forms:

```fsharp
// Static module function (lowercase, pipeable)
let normalized = Vec.unitize myVector
let moved = Pnt.translate myVec myPoint

// Instance method/property (uppercase)
let normalized = myVector.Unitized
let moved = myPoint.Transform myMatrix

// Pipeline style
myPoint
|> Pnt.translate (Vec(1, 0, 0))
|> Pnt.rotateZ 45.0
|> Pnt.transform myMatrix
```

### Custom Operators

```fsharp
a + b           // Point + Vector = Point, Vector + Vector = Vector
a - b           // Point - Point = Vector, Point - Vector = Point
a * 2.0         // Scale vector or point
a / 2.0         // Divide (with zero-check)
a *** b         // Dot product for vectors, matrix multiplication for matrices
-a              // Negate a vector
```

## API Documentation

**Full API Reference:** [goswinr.github.io/Euclid](https://goswinr.github.io/Euclid/reference/euclid.html)

## Platform Support

Thanks to [Fable](https://fable.io/), Euclid can
be used not only on .NET but also in JavaScript, TypeScript, Rust, and Python.

## Development

### Use of AI and LLMs
All core function are are written by hand to ensure performance and correctness.<br>
However, AI tools have been used for code review, typo and grammar checking in documentation<br>
and to generate not all but many of the tests.

### Prerequisites
- .NET SDK 10.0 or later (to have [scoped warnings](https://learn.microsoft.com/en-us/dotnet/fsharp/whats-new/fsharp-10#scoped-warning-suppression) available)
- Node.js (only needed to run the .NET tests in JavaScript and TypeScript via Fable.Mocha)

### Building from Source

```bash
git clone https://github.com/goswinr/Euclid.git
cd Euclid
dotnet build
```

## Testing

Tests run on both .NET and JavaScript with TypeScript build verification.

### .NET Testing

```bash
dotnet run --project ./Tests/Tests.fsproj
```

### JavaScript Testing

setup dependencies:

```bash
npm ci --prefix ./Tests
dotnet tool restore
```

then run tests:

```bash
npm run test --prefix ./Tests
```

The test suite ensures cross-platform compatibility and verifies TypeScript type definitions.

## Contributing

Contributions are welcome!


## Changelog
See [CHANGELOG.md](https://github.com/goswinr/Euclid/blob/main/CHANGELOG.md) for version history.

## Related Projects
[Euclid.Rhino](https://github.com/goswinr/Euclid.Rhino) - Rhino3D integration

## License
[MIT](https://github.com/goswinr/Euclid/blob/main/LICENSE.md)


## Installation

Add Euclid to your F# project via NuGet:

```bash
dotnet add package Euclid
```

Or in F# scripting:

```fsharp
#r "nuget: Euclid"
```

## Quick Start

```fsharp
open Euclid

// Create 3D points and vectors
let point1 = Pnt(1.0, 2.0, 3.0)
let point2 = Pnt(4.0, 5.0, 6.0)
let vector = Vec(1.0, 1.0, 0.0)

// Calculate distance
let distance = Pnt.distance point1 point2

// Create and use unit vectors
let unitVec = vector.Unitized  // returns a UnitVec

// Transform with 4x4 matrix
let matrix =
    Matrix.createShear(3.0, 0, 0, 0, 0, 0)
    *** // Combine transformations
    Matrix.createRotationZ 45

point1
|> Pnt.translate vector
|> Pnt.scale 3.0
|> Pnt.transform matrix
```

## Examples

### Points and Vectors (2D)

```fsharp
open Euclid

// 2D points and vectors
let a = Pt(1.0, 2.0)
let b = Pt(4.0, 6.0)

// Distance between points
let dist = Pt.distance a b              // 5.0

// Midpoint
let mid = Pt.midPt a b                  // Pt(2.5, 4.0)

// 2D vectors
let v = Vc(3.0, 4.0)
let len = v.Length                       // 5.0
let half = v.Half                        // Vc(1.5, 2.0)
let perp = v.Rotate90CCW                 // perpendicular vector

// Point + vector arithmetic
let moved = a + v                        // Pt(4.0, 6.0)
let diff = b - a                         // Vc(3.0, 4.0) : point - point = vector

// Unit vectors (guaranteed length 1.0)
let dir = v.Unitized                     // UnitVc(0.6, 0.8)
let angle = v.Direction360               // angle in degrees from X-axis

// Dot and cross product
let dot = v.Dot(Vc(1.0, 0.0))           // 3.0
let cross = v.Cross(Vc(1.0, 0.0))       // -4.0 (signed area in 2D)
```

### Points and Vectors (3D)

```fsharp
open Euclid

// 3D points and vectors
let p1 = Pnt(1.0, 2.0, 3.0)
let p2 = Pnt(4.0, 6.0, 3.0)

// Distance, midpoint, lerp
let dist = p1.DistanceTo p2             // 5.0
let mid  = Pnt.midPt p1 p2              // Pnt(2.5, 4.0, 3.0)
let lerp = Pnt.divPt(p1, p2, 0.25)     // 25% from p1 towards p2

// 3D vectors
let v1 = Vec(1.0, 0.0, 0.0)
let v2 = Vec(0.0, 1.0, 0.0)

// Cross product and dot product
let normal = v1.Cross v2                 // Vec(0, 0, 1)
let dot = v1.Dot v2                      // 0.0

// Angle between vectors
let angle = Vec.angle180 v1 v2           // 90.0 degrees

// Unit vectors
let dir = Vec(3.0, 4.0, 0.0).Unitized   // UnitVec(0.6, 0.8, 0.0)
let isPerp = dir.IsPerpendicularTo UnitVec.Zaxis  // true

// Convert between 2D and 3D
let pt2d = p1.AsPt                       // Pt(1.0, 2.0) - drops Z
let pt3d = Pt(1.0, 2.0).WithZ 5.0       // Pnt(1.0, 2.0, 5.0)
```

### Lines

```fsharp
open Euclid

// Create lines from points
let ln2d = Line2D(Pt(0, 0), Pt(10, 0))
let ln3d = Line3D(Pnt(0, 0, 0), Pnt(10, 5, 3))

// Or from coordinates
let ln = Line2D(0.0, 0.0, 10.0, 5.0)

// Line properties
let len    = ln2d.Length                 // 10.0
let midPt  = ln2d.Mid                   // Pt(5, 0)
let dir    = ln3d.Direction             // UnitVec of line direction
let tang   = ln3d.Tangent               // UnitVec (same as Direction)

// Evaluate at parameter (0.0 = start, 1.0 = end)
let quarterPt = ln2d.EvaluateAt 0.25    // Pt(2.5, 0)

// Extend, shrink, reverse
let longer  = ln2d.Extend(2.0, 3.0)     // extend 2 at start, 3 at end
let shorter = ln2d.Shrink(1.0, 1.0)     // shrink 1 from each end
let flipped = ln2d.Reversed              // swap start and end

// Closest point queries
let testPt = Pt(5.0, 3.0)
let closest = ln2d.ClosestPoint testPt   // Pt(5, 0) - clamped to segment
let param = ln2d.ClosestParameter testPt // 0.5

// Line-line relationships
let other = Line2D(Pt(5, -5), Pt(5, 5))
let isParallel = ln2d.IsParallelTo other       // false
let isPerp = ln2d.IsPerpendicularTo other      // true

// Intersection (returns option)
let hit = Line2D.tryIntersect ln2d other        // Some (Pt(5, 0))

// Offset a line
let offsetLn = Line2D.offset 2.0 ln2d   // offset by 2.0 to the left

// Move a line
let movedLn = ln2d.Move(Vc(0.0, 5.0))   // translate by vector
```

### Line Intersections (Discriminated Unions)

For detailed intersection analysis, the `XLine2D` and `XLine3D` modules return discriminated union types
that let you handle every case explicitly:

```fsharp
open Euclid

let lineA = Line2D(Pt(0, 0), Pt(10, 0))
let lineB = Line2D(Pt(5, -5), Pt(5, 5))

// Finite segment intersection - returns XPt discriminated union
match XLine2D.getIntersection(lineA, lineB) with
| XLine2D.XPt.Intersect pt       -> printfn $"Lines cross at {pt}"
| XLine2D.XPt.Apart              -> printfn "Segments don't reach each other"
| XLine2D.XPt.Parallel           -> printfn "Lines are parallel"
| XLine2D.XPt.TooShortA          -> printfn "Line A is too short"
| XLine2D.XPt.TooShortB          -> printfn "Line B is too short"
| XLine2D.XPt.TooShortBoth       -> printfn "Both lines are too short"

// Ray intersection (extends lines infinitely)
match XLine2D.getRayIntersection(lineA, lineB) with
| XLine2D.XRay.Intersect pt      -> printfn $"Rays meet at {pt}"
| XLine2D.XRay.Parallel          -> printfn "Rays are parallel"
| _                               -> ()

// 3D line relationships include skew detection
let ln3A = Line3D(Pnt(0, 0, 0), Pnt(10, 0, 0))
let ln3B = Line3D(Pnt(5, 0, 5), Pnt(5, 10, 5))

match XLine3D.getIntersection(ln3A, ln3B) with
| XLine3D.XPnt.Intersect pt      -> printfn $"Lines intersect at {pt}"
| XLine3D.XPnt.Skew (ptA, ptB, d) -> printfn $"Closest approach: {d} between {ptA} and {ptB}"
| XLine3D.XPnt.Apart             -> printfn "Segments don't reach each other"
| XLine3D.XPnt.Parallel          -> printfn "Lines are parallel"
| _                               -> ()
```

### Planes

```fsharp
open Euclid

// Normal plane (origin + normal direction)
let npl = NPlane(Pnt(0, 0, 5), UnitVec.Zaxis)

// Signed distance from plane (positive = same side as normal)
let d = npl.DistanceToPt (Pnt(3, 4, 8))         // 3.0
let d2 = npl.DistanceToPt (Pnt(3, 4, 2))        // -3.0

// Project a point onto the plane
let proj = npl.ClosestPoint (Pnt(3, 4, 8))       // Pnt(3, 4, 5)

// Angle between plane and vector (0 = parallel, 90 = perpendicular)
let ang = npl.Angle90ToVec (Vec(1, 0, 0))         // 0.0 (vector lies in plane)

// Parametrized plane (full coordinate frame with X, Y, Z axes)
let ppl = PPlane.createThreePoints
            (Pnt(0, 0, 0))    // origin
            (Pnt(10, 0, 0))   // point on X-axis
            (Pnt(0, 10, 0))   // point on Y-axis side
```

### Rectangles

```fsharp
open Euclid

// 2D rectangle from origin, X-direction vector, and Y-size
let rect = Rect2D.createFromXVectorAndWidth(Pt(0, 0), Vc(10, 0), 5.0)

// From direction and sizes
let dir = UnitVc.createFromDegrees 45.0
let rotRect = Rect2D.createFromDirectionAndSizes(Pt(0, 0), dir, 10.0, 5.0)

// Rectangle properties
let area = rect.Area
let cx = rect.SizeX
let cy = rect.SizeY
let center = rect.Center
let c0 = rect.Corner0                     // origin corner
let c2 = rect.Corner2                     // diagonally opposite corner

// Point evaluation (u, v parameters from 0 to 1)
let midPoint = rect.EvaluateAt(0.5, 0.5)  // center point
```

### Bounding Boxes

```fsharp
open Euclid

// 3D axis-aligned bounding box from points
let points = [Pnt(0, 0, 0); Pnt(10, 5, 3); Pnt(-2, 8, 1)]
let bbox = BBox.createFromSeq points

// BBox properties
let size = bbox.SizeX, bbox.SizeY, bbox.SizeZ   // 12.0, 8.0, 3.0
let minPt = bbox.MinPnt                           // Pnt(-2, 0, 0)
let maxPt = bbox.MaxPnt                           // Pnt(10, 8, 3)
let center = bbox.Center                          // Pnt(4, 4, 1.5)
let vol = bbox.Volume                             // 288.0

// Create from center and sizes
let box2 = BBox.createFromCenter(Pnt(0, 0, 0), 10.0, 10.0, 10.0)

// Expand bounding box
let bigger = bbox.Grown 1.0                // expand by 1.0 on all sides

// Check containment
let inside = bbox.Contains(Pnt(5, 4, 2))  // true

// Union of two bounding boxes
let combined = bbox.Union box2
```

### Polylines

```fsharp
open Euclid

// Create a 2D polyline
let pl2d = Polyline2D(ResizeArray [Pt(0, 0); Pt(10, 0); Pt(10, 5); Pt(0, 5)])

// Polyline properties
let len = pl2d.Length                      // total length
let count = pl2d.PointCount                // 4
let segs = pl2d.SegmentCount               // 3

// Access segments as Line2D
let firstSeg = pl2d.FirstSegment           // Line2D(Pt(0,0), Pt(10,0))
let seg = pl2d.GetSegment(1)               // Line2D(Pt(10,0), Pt(10,5))

// Modify (polylines are mutable)
pl2d.SetVertex 2 (Pt(12, 5))              // move a vertex
let copy = pl2d.Duplicate()                // deep copy

// 3D polyline
let pl3d = Polyline3D.create [Pnt(0,0,0); Pnt(10,0,0); Pnt(10,5,3)]
let totalLen = pl3d.Length
```

### Transformations with Matrices

```fsharp
open Euclid

let pt = Pnt(5.0, 0.0, 0.0)

// Translation
let m1 = Matrix.createTranslation(10.0, 0.0, 0.0)

// Rotation (angle in degrees)
let m2 = Matrix.createRotationZ 90.0             // 90 degrees around Z
let m3 = Matrix.createRotationX 45.0             // 45 degrees around X

// Rotation around arbitrary axis
let m4 = Matrix.createRotationAxis(UnitVec.Zaxis, 30.0)

// Rotation around axis at a specific center point
let m5 = Matrix.createRotationAxisCenter(Vec(0, 0, 1), Pnt(5, 5, 0), 45.0)

// Scale
let m6 = Matrix.createScale(2.0, 2.0, 1.0)      // scale X and Y by 2

// Shear
let m7 = Matrix.createShear(0.5, 0, 0, 0, 0, 0) // shear XY by 0.5

// Combine transformations with *** operator
let combined = m1 *** m2 *** m6

// Apply to points and vectors
let pt2 = pt.Transform combined                  // full transform (rotate, scale, translate)
let v = Vec(1, 0, 0)
let v2 = v.Transform combined                    // only rotate and scale (no translation)

// Functional pipeline style
let result =
    pt
    |> Pnt.translate (Vec(10, 0, 0))
    |> Pnt.rotateZ 90.0
    |> Pnt.scale 2.0
    |> Pnt.transform combined

// Matrix from plane to plane
let fromPlane = PPlane.createThreePoints (Pnt(0,0,0)) (Pnt(1,0,0)) (Pnt(0,1,0))
let toPlane   = PPlane.createThreePoints (Pnt(5,5,0)) (Pnt(6,5,0)) (Pnt(5,6,0))
let planeXform = Matrix.createPlaneToPlane(fromPlane, toPlane)

// Mirror about a plane
let mirror = Matrix.createMirror fromPlane
```

### Quaternion Rotations

```fsharp
open Euclid

// Create quaternion from axis and angle
let q1 = Quaternion.createFromDegree(Vec(0, 0, 1), 90.0)    // 90 degrees around Z
let q2 = Quaternion.createFromRadians(UnitVec.Xaxis, 1.5708) // Pi/2 around X

// Rotate from one direction to another
let q3 = Quaternion.createVecToVec(UnitVec.Xaxis, UnitVec.Yaxis) // X -> Y

// Quaternion properties
let angle = q1.AngleInDegrees                    // 90.0
let axis = q1.Axis                               // Vec close to (0, 0, 1)
let inv = q1.Inverse                             // reverse rotation

// Compose rotations by multiplication
let q4 = q1 * q2                                 // first q2, then q1

// Apply to points and vectors
let rotated = Pnt.rotateByQuaternion q1 (Pnt(1, 0, 0))   // Pnt(0, 1, 0)

// Convert quaternion to matrix
let mat = Matrix.createFromQuaternion q1
```

### Point Cloud Operations

```fsharp
open Euclid

let cloud = ResizeArray [Pnt(0,0,0); Pnt(1,0,0); Pnt(5,5,5); Pnt(10,10,10)]

// Find closest point to a test point
let nearest = Points3D.closestPoint(cloud, Pnt(1.1, 0.1, 0.0))  // Pnt(1,0,0)
let idx = Points3D.closestPointIdx(cloud, Pnt(1.1, 0.1, 0.0))   // 1

// Closest points between two sets
let setA = ResizeArray [Pnt(0,0,0); Pnt(10,10,10)]
let setB = ResizeArray [Pnt(1,0,0); Pnt(20,20,20)]
let (iA, iB) = Points3D.closestPointsIdx(setA, setB)            // (0, 0)

// Most distant point
let farthest = Points3D.mostDistantPoint(cloud, setB)

// Center of points
let center = Points3D.center cloud                // Pnt(4, 3.75, 3.75)

// Remove duplicate points within tolerance
let culled = Points3D.cullDuplicatePointsInSeq(cloud, 0.01)
```

### Error Handling

Euclid uses descriptive custom exceptions instead of silent failures:

```fsharp
open Euclid

// Unitizing a zero vector raises EuclidException
try
    let bad = Vec(0, 0, 0).Unitized      // fails: can't unitize zero vector
with :? EuclidException as e ->
    printfn $"Caught: {e.Message}"

// Division by near-zero raises EuclidDivByZeroException
// NaN or Infinity in inputs raises EuclidException

// Tolerances are conservative for precision engineering:
// - zeroLengthTolerance = 1e-12 (for divisions, unitizing)
// - isTooSmall = 1e-6 (general smallness check)
```

