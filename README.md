
# Euclid

[![Euclid on nuget.org](https://img.shields.io/nuget/v/Euclid)](https://www.nuget.org/packages/Euclid/)
[![Euclid on fuget.org](https://www.fuget.org/packages/Euclid/badge.svg)](https://www.fuget.org/packages/Euclid)
![code size](https://img.shields.io/github/languages/code-size/goswinr/Euclid.svg)
[![license](https://img.shields.io/github/license/goswinr/Euclid)](LICENSE)

![Logo](https://raw.githubusercontent.com/goswinr/Euclid/main/Doc/logo128.png)


Euclid is a 2D and 3D geometry library without dependencies.
It is mostly focused on creating and manipulating primitives such as
points, vectors, lines, planes, boxes, and bounding boxes, and polylines.
For transformations of those there are rotations, quaternions, 4x4 and rigid ortho normal 4x3 matrices.

Written in F# and designed for use with F#.
All primitive types are immutable and functions are curried where appropriate.
Many functions exist as both a lowercase static member and a uppercase methods or property.
e.g.: `Vec.unitized(v)` is the same as the `v.Unitized` property.
This library is intended to be used for design, construction and manufacturing too.
So it is using double precision floating point numbers for all values. (While most geometry libraries for games use single precision floats.)

This library can be compiled to JavaScript via [Fable](https://fable.io/) too.

See [Euclid.Rhino](https://github.com/goswinr/Euclid.Rhino) for converting form and to [Rhino3D](https://www.rhino3d.com/) geometry.

### Documentation

This library assumes a right handed coordinate system with the Z-axis pointing up.
Just like Rhino3D, SketchUp, Revit, AutoCAD and Blender have it. But [unlike](https://twitter.com/FreyaHolmer/status/1325556229410861056) Unity or Unreal Engine.

In this library a point is a position in space, a vector is a direction in space.
A 4x4 transformation matrix applied to a vector will only rotate and scale the vector but not translate it.
( You could think of this as a Homogeneous-coordinate-system where the last value is 0 ( mot 1), thus [disabling translation(https://www.youtube.com/watch?v=o-xwmTODTUI&t=216s)]. )


For the full API see [fuget.org](https://www.fuget.org/packages/Euclid) for now.

### License
[MIT](https://raw.githubusercontent.com/goswinr/Euclid/main/LICENSE.txt)

### Build
Just run `dotnet build` in the root directory.

### Test
To just run once: `dotnet run Tests\Euclid.Tests.fsproj`
To run and watch the tests use: `dotnet watch --project Tests\Euclid.Tests.fsproj run`

or:

```bash
cd Tests
```

For testing with .NET using Expecto run

```bash
dotnet run
```

for testing with Fable.Mocha run

```bash
npm test
```

### Release Notes

`0.9.0`
- add suffix '90' to angel functions in Plane module
- improve precision of Pi literals
- include NaN check in isTiny functions

`0.8.0`
- fix line splitting
- rename Rect.create functions

`0.7.0`
- refactor offset
- add Topology sorting

`0.6.1`
- remove reflection form error messages for nicer Fable output
- rename Rect.Rotate to Rect.RotateOrientation
- add Rect.grid
- fix Rect.subdivide

`0.5.0`
- allow equality checks to use zero tolerance.
- better docstring
- add more angle calculation functions
- rename distanceFromPoint to distanceToPt
- add rotateByQuarterCircle
- add ExtendRel functions for Lines
- use *** as operator for dot product, and matrix multiplication
- add splitting with gap to Lines and Rects

`0.4.1`
- fix bug in rect.Edge23
- add BBox.createFromIList

`0.4.0`
- add Line.withLengthFromMid
- rename isAngleLessThan functions
- add spherical interpolation (slerp) for vectors
- rename Length to Width on Boxes and Rects
- rename Width to Height2D on Rects
- rename Width to Depth on Boxes
- add EdgeMethods on Rects and Boxes

`0.3.0`
- renamed OrthoMatrix to RigidMatrix
- add DataMember Attributes for serialization
- ofXZY functions renamed to createFromMembersXYZ
- add DivideByInt static members for Array.average and Array.sum for functions
- fixed typos, improve documentation

`0.2.1`
- fix fable support

`0.2.0`
- fable support
- improved xml documentation

`0.1.0`
- first public release

### Fable build notes

Test is there is code using reflection via (or similar):
`dotnet fable  --verbose --lang typescript --outDir ./dist/TS --noReflection`
