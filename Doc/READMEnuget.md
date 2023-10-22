

# Euclid

![code size](https://img.shields.io/github/languages/code-size/goswinr/Euclid.svg) 

![Logo](https://raw.githubusercontent.com/goswinr/Euclid/main/Doc/logo128.png)

Euclid is a 2D and 3D geometry library without dependencies. 
It is mostly focused on creating and manipulating primitives such as 
points, vectors, lines, planes, boxes, and bounding boxes, and polylines. 
For transformations of those there are rotations, quaternions, 4x4 and rigid ortho normal 4x3 matrices.

Written in F# and designed for use with F#.
All primitive types are immutable and functions are curried where appropriate.
This library is intended to be used for design, construction and manufacturing too. 
So it is using double precision floating point numbers for all values. (While most geometry libraries for games use single precision floats.)

This library can be used in JavaScript via [Fable](https://fable.io/) too.

See [Euclid.Rhino](https://github.com/goswinr/Euclid.Rhino) for converting form and to [Rhino3D](https://www.rhino3d.com/) geometry.

### Documentation

This library assumes a right handed coordinate system with the Z-axis pointing up.
Just like Rhino3D, SketchUp, Revit, AutoCAD and Blender have it. But [unlike](https://twitter.com/FreyaHolmer/status/1325556229410861056) Unity or Unreal Engine.

In this library a point is a position in space, a vector is a direction in space.
A 4x4 transformation matrix applied to a vector will only rotate and scale the vector but not translate it. ( = Homogeneous coordinates)

For the full API see [fuget.org](https://www.fuget.org/packages/Euclid) for now.

### License
[MIT](https://raw.githubusercontent.com/goswinr/Euclid/main/LICENSE.txt)

### Release Notes

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


