

![Logo](https://raw.githubusercontent.com/goswinr/Euclid/main/Docs/img/logo128.png)

# Euclid


[![Euclid on nuget.org](https://img.shields.io/nuget/v/Euclid)](https://www.nuget.org/packages/Euclid/)
[![Build Status](https://github.com/goswinr/Euclid/actions/workflows/build.yml/badge.svg)](https://github.com/goswinr/Euclid/actions/workflows/build.yml)
[![Docs Build Status](https://github.com/goswinr/Euclid/actions/workflows/docs.yml/badge.svg)](https://github.com/goswinr/Euclid/actions/workflows/docs.yml)
[![Test Status](https://github.com/goswinr/Euclid/actions/workflows/test.yml/badge.svg)](https://github.com/goswinr/Euclid/actions/workflows/test.yml)
[![Hits](https://hits.seeyoufarm.com/api/count/incr/badge.svg?url=https%3A%2F%2Fgithub.com%2Fgoswinr%2FEuclid&count_bg=%2379C83D&title_bg=%23555555&icon=github.svg&icon_color=%23E7E7E7&title=hits&edge_flat=false)](https://hits.seeyoufarm.com)
[![license](https://img.shields.io/github/license/goswinr/Euclid)](LICENSE.md)
![code size](https://img.shields.io/github/languages/code-size/goswinr/Euclid.svg)


Euclid is a 2D and 3D geometry library for F# without dependencies.\
It is mostly focused on creating and manipulating primitives such as\
points, vectors, lines, planes, boxes, and bounding boxes, and polylines.\
For transformations of those there are rotations, quaternions, 4x4 and rigid ortho normal 4x3 matrices.

Written in F# and designed for use with F#, \
all primitive types are immutable and functions are curried where appropriate.\

This library is intended to be used for design, construction and manufacturing.\
So it is using double precision floating point numbers for all values. (While most geometry libraries for games use single precision floats.)

See [Euclid.Rhino](https://github.com/goswinr/Euclid.Rhino) for converting form and to [Rhino3D](https://www.rhino3d.com/) geometry.

This library can be compiled to JavaScript, Typescript, Rust or Python via [Fable](https://fable.io/).

## Where does Z point to?

This library assumes a right handed coordinate system with the Z-axis pointing up.\
Just like Rhino3D, Blender, SketchUp, Revit and AutoCAD have it. But [unlike](https://bsky.app/profile/freya.bsky.social/post/3lat5r6hlck25) Unity, Unreal or Maya.


## Design decisions

In this library a point is a position in space, a vector is a direction in space.\
A 4x4 transformation matrix applied to a vector will only rotate and scale the vector but not translate it.\
You could think of this as a Homogeneous-coordinate-system where the last value is 0 (not 1), thus [disabling translation](https://www.youtube.com/watch?v=o-xwmTODTUI&t=216s).

A 2D point is called `Pt` and a 3D point is called `Pnt`.\
A 2D vector is called `Vc` and a 3D vector is called `Vec`.\

This library has dedicated types for Unit vectors in 2D and 3D space.\
They are called `UnitVc` and `UnitVec` respectively.\
They are guaranteed to have a length of 1.0.

All types have respective modules with the same name for functions that operate on them.\
Many functions exist as both a lowercase static member and a uppercase methods or property.\
e.g.: `Vec.unitized(v)` is the same as the `v.Unitized` property.\

## Full API Documentation

[goswinr.github.io/Euclid](https://goswinr.github.io/Euclid)

## Changelog
see [CHANGELOG.md](https://github.com/goswinr/Euclid/blob/main/CHANGELOG.md)

## Build from source
Just run `dotnet build` in the root directory.


## Tests
All Tests run in both javascript and dotnet.
The typescript result is verified with the typescript compiler.
Go to the tests folder:

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

## License
[MIT](https://github.com/goswinr/Euclid/blob/main/LICENSE.md)

