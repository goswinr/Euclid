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

## Table of Contents

- [Features](#features)
- [Installation](#installation)
- [Quick Start](#quick-start)
- [Coordinate System](#coordinate-system)
- [Design Philosophy](#design-philosophy)
- [Usage Examples](#usage-examples)
- [API Documentation](#api-documentation)
- [Platform Support](#platform-support)
- [Development](#development)
- [Testing](#testing)
- [Contributing](#contributing)
- [License](#license)

## Features

🎯 **Core Geometry Types**
- Points (`Pt`, `Pnt`), Vectors (`Vc`, `Vec`), Unit Vectors (`UnitVc`, `UnitVec`)
- Lines, Planes, Boxes, Bounding Boxes, Polylines
- Rotations, Quaternions, 4x4 and rigid orthonormal 4x3 matrices

✨ **Key Advantages**
- **Zero dependencies** - lightweight and self-contained
- **Double precision** - engineered for CAD/manufacturing accuracy
- **Immutable types** - functional programming friendly
- **Type safety** - dedicated unit vector types prevent common errors
- **Cross-platform** - compiles to .NET, JavaScript, TypeScript, Rust, Python via Fable
- **Performant** - All small types are structs, functions are often inline and try to minimize the allocation of intermediate objects.

🔧 **Design & Manufacturing Focus**
- Optimized for design, construction, and manufacturing workflows
- Integrates seamlessly with [Rhino3D](https://github.com/goswinr/Euclid.Rhino)
- Right-handed coordinate system (Z-up) matching industry standards

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
    *** // Combine transfromations
    Matrix.createRotationZ 45

point1
|> Pnt.translate vector
|> Pnt.scale 3.0
|> Pnt.transform matrix

```

## Coordinate System

This library uses a **right-handed coordinate system** with the **Z-axis pointing up**.

✅ **Same as in:** Rhino3D, Blender, SketchUp, Revit, AutoCAD
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
| Type | 2D | 3D |
|------|----|----|
| Point | `Pt` | `Pnt` |
| Vector | `Vc` | `Vec` |
| Unit Vector | `UnitVc` | `UnitVec` |

### Function Patterns
Functions are available in multiple forms:
```fsharp
// Static module function (lowercase)
let normalized = Vec.unitized myVector

// Instance method/property (uppercase)
let normalized = myVector.Unitized
```

## API Documentation

📚 **Full API Reference:** [goswinr.github.io/Euclid](https://goswinr.github.io/Euclid/reference/euclid.html)

## Platform Support

Thanks to [Fable](https://fable.io/), Euclid can be compiled to multiple platforms:

- ✅ **.NET** (Primary target)
- ✅ **JavaScript** (Browser/Node.js)
- ✅ **TypeScript** (Type-safe JS)
- ✅ **Rust** (Systems programming)
- ✅ **Python** (Data science/ML)

## Development

### Building from Source
```bash
git clone https://github.com/goswinr/Euclid.git
cd Euclid
dotnet build
```

### Prerequisites
- .NET SDK 6.0 or later
- Node.js (for JavaScript testing)

## Testing

Tests run on both .NET and JavaScript platforms with TypeScript verification.

### .NET Testing
```bash
cd Tests
dotnet run
```

### JavaScript Testing
```bash
cd Tests
npm install
npm test
```

The test suite ensures cross-platform compatibility and verifies TypeScript type definitions.

## Contributing

Contributions are welcome!


## Changelog
📋 See [CHANGELOG.md](https://github.com/goswinr/Euclid/blob/main/CHANGELOG.md) for version history.

## Related Projects
🦏 [Euclid.Rhino](https://github.com/goswinr/Euclid.Rhino) - Rhino3D integration

## License
[MIT](https://github.com/goswinr/Euclid/blob/main/LICENSE.md)

