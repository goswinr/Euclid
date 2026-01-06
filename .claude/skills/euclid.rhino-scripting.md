# Euclid.Rhino Scripting Skill

## Overview
This skill provides the default boilerplate code and API reference for F# scripting in Rhino 8 using the Euclid.Rhino library. The library provides seamless conversion between Euclid geometry types and Rhino geometry types, plus drawing utilities.

## Default Script Setup

When creating a new F# script for Rhino using Euclid.Rhino, use this boilerplate:

```fsharp
#r "C:/Program Files/Rhino 8/System/RhinoCommon.dll"
#I @"D:\Git\_Euclid_\Euclid.Rhino\bin\Release\net7.0\"
#r "Euclid.dll"
#r "Euclid.Rhino.dll"
#r "nuget: Rhino.Scripting.FSharp"
#r "nuget: ResizeArrayT"
#r "nuget: Fesher"

open System
open ResizeArrayT
open Rhino.Scripting
open Rhino.Geometry
open Rhino.Scripting.FSharp
open Euclid
open Fesher

clearFeshLog()

type rs = RhinoScriptSyntax

rs.DisableRedraw()

// Your code here


```

## Key Components

- **RhinoCommon.dll**: Core Rhino geometry and API library
- **Euclid.dll**: Core Euclid geometry library
- **Euclid.Rhino.dll**: Rhino-specific extensions for Euclid
- **Rhino.Scripting.FSharp**: F# scripting utilities for Rhino
- **ResizeArrayT**: Typed resizable array utilities
- **Fesher**: F# scripting helper library

## Common Namespaces

| Namespace | Purpose |
|-----------|---------|
| `Rhino.Scripting` | RhinoScript-like API |
| `Rhino.Geometry` | Rhino geometry types |
| `Rhino.Scripting.FSharp` | F# extensions for Rhino |
| `Euclid` | Core Euclid geometry types |
| `EuclidRhino` | Rhino-specific utilities (Rs, State, ColorUtil, UtilLayer) |
| `Fesher` | F# scripting utilities |
| `ResizeArrayT` | Typed resizable arrays |

## Tips

- Use `rs.DisableRedraw()` before batch operations
- `clearFeshLog()` clears the Fesh editor log window
- The `rs` type alias provides convenient access to `RhinoScriptSyntax` methods

---

## EuclidRhino Module API Reference

### State Module
Holds current state like the active Rhino document.

| Member | Description |
|--------|-------------|
| `State.Doc` | The current active Rhino document (the file currently open) |

### Rs Module (Drawing Utilities)
Provides drawing functions for Rhino.

| Method | Description |
|--------|-------------|
| `Rs.AddLine(start: Point3d, end: Point3d)` | Adds a line curve. Returns Guid. |
| `Rs.AddLine(startX, startY, startZ, endX, endY, endZ)` | Adds a 3D line from coordinates. Returns Guid. |
| `Rs.AddLine2D(startX, startY, endX, endY)` | Adds a 2D line at z=0. Returns Guid. |
| `Rs.AddPolyline(points: Point3d seq)` | Adds a polyline curve. Requires at least 2 points. Returns Guid. |
| `Rs.CurveArrows(curveId: Guid, arrowStyle: int)` | Sets curve arrow display. Styles: 0=none, 1=start, 2=end, 3=both. |
| `Rs.DrawPlane(pl, ?axLength, ?suffixInDot, ?layer)` | Draws plane axes with labeled TextDots. Returns list of Guids. |
| `Rs.DrawVector(v: Vector3d)` | Draws a vector from World Origin with arrow. |
| `Rs.DrawVector(v: Vector3d, pt: Point3d)` | Draws a vector from a given point with arrow. |

### ColorUtil Module
Utilities for color manipulation.

| Method | Description |
|--------|-------------|
| `ColorUtil.fromHSL(h, s, l)` | Creates color from HSL values (0.0-1.0 range). |
| `ColorUtil.randomForRhino()` | Generates a random high-saturation color, excluding yellow, ideal for layer colors. Subsequent calls produce distinct hues. |

### UtilLayer Module
Layer management utilities.

| Method | Description |
|--------|-------------|
| `UtilLayer.getOrCreateLayer(name, colorFunc, lockedState, visibleState, collapseParents)` | Creates layer and parent layers if missing. |
| `UtilLayer.isAcceptableStringId(name, allowEmpty)` | Checks if string is valid for Rhino object names (no tabs, line returns, leading/trailing whitespace). |

---

## Euclid ↔ Rhino Geometry Conversions

The `Euclid.AutoOpenRhinoIntegration` module (auto-opened when using Euclid.Rhino) provides seamless conversion between Euclid and Rhino types.

### Points

#### Euclid.Pt (2D Point)
| Member | Description |
|--------|-------------|
| `pt.RhPt` | Convert to Rhino Point3d with Z=0.0 |
| `pt.RhPtZ(z)` | Convert to Rhino Point3d with given Z value |
| `pt.Draw()` | Draw in Rhino on current layer |
| `pt.Draw(layer)` | Draw on specified layer (creates if needed) |
| `pt.Draw(layer, name)` | Draw with layer and name |
| `pt.DrawDot(message)` | Draw as TextDot with message |
| `pt.DrawDot(message, layer)` | Draw as TextDot on specified layer |
| `Pt.ofRhPt(rhPoint3d)` | Convert from Rhino Point3d (ignores Z) |
| `Pt.toRhPt(pt)` | Static conversion to Rhino Point3d |
| `Pt.toRhPtZ(z, pt)` | Static conversion with Z value |
| `Pt.draw(pt)` | Static draw function |
| `Pt.drawDot(message, pt)` | Static TextDot draw function |

#### Euclid.Pnt (3D Point)
| Member | Description |
|--------|-------------|
| `pnt.RhPt` | Convert to Rhino Point3d |
| `pnt.Draw()` | Draw in Rhino on current layer |
| `pnt.Draw(layer)` | Draw on specified layer |
| `pnt.Draw(layer, name)` | Draw with layer and name |
| `pnt.DrawDot(message)` | Draw as TextDot |
| `pnt.DrawDot(message, layer)` | Draw as TextDot on layer |
| `Pnt.ofRhPt(rhPoint3d)` | Convert from Rhino Point3d |
| `Pnt.toRhPt(pnt)` | Static conversion to Rhino Point3d |
| `Pnt.draw(pnt)` | Static draw function |
| `Pnt.drawDot(message, pnt)` | Static TextDot draw function |

#### Rhino.Geometry.Point3d Extensions
| Member | Description |
|--------|-------------|
| `point3d.Pt` | Convert to Euclid 2D point (ignores Z) |
| `point3d.Pnt` | Convert to Euclid 3D point |
| `Point3d.toPt(point3d)` | Static conversion to Euclid Pt |
| `Point3d.toPnt(point3d)` | Static conversion to Euclid Pnt |

#### Rhino.Geometry.Point3f Extensions
| Member | Description |
|--------|-------------|
| `point3f.Pt` | Convert to Euclid 2D point (ignores Z) |
| `point3f.Pnt` | Convert to Euclid 3D point |

### Vectors

#### Euclid.Vc (2D Vector)
| Member | Description |
|--------|-------------|
| `vc.RhVec` | Convert to Rhino Vector3d with Z=0.0 |
| `vc.Draw(startPt, scale)` | Draw as line with arrow |
| `vc.Draw(startPt, layer)` | Draw on specified layer |
| `vc.Draw(startPt, scale, layer)` | Draw with scale and layer |
| `vc.Draw(startPt, scale, layer, name)` | Draw with all options |
| `Vc.ofRhVec(rhVector3d)` | Convert from Rhino Vector3d (ignores Z) |
| `Vc.toRhVec(vc)` | Static conversion to Rhino Vector3d |
| `Vc.draw(scale, startPt, vc)` | Static draw function |

#### Euclid.UnitVc (2D Unit Vector)
| Member | Description |
|--------|-------------|
| `unitVc.RhVec` | Convert to Rhino Vector3d with Z=0.0 |
| `unitVc.Draw(startPt, scale)` | Draw as line with arrow |
| `UnitVc.ofRhVec(rhVector3d)` | Unitize from Rhino Vector3d (ignores Z) |
| `UnitVc.toRhVec(unitVc)` | Static conversion to Rhino Vector3d |

#### Euclid.Vec (3D Vector)
| Member | Description |
|--------|-------------|
| `vec.RhVec` | Convert to Rhino Vector3d |
| `vec.Draw(startPnt, scale)` | Draw as line with arrow |
| `vec.Draw(startPnt, layer)` | Draw on specified layer |
| `vec.Draw(startPnt, scale, layer)` | Draw with scale and layer |
| `vec.Draw(startPnt, scale, layer, name)` | Draw with all options |
| `Vec.ofRhVec(rhVector3d)` | Convert from Rhino Vector3d |
| `Vec.toRhVec(vec)` | Static conversion to Rhino Vector3d |
| `Vec.draw(scale, startPt, vec)` | Static draw function |

#### Euclid.UnitVec (3D Unit Vector)
| Member | Description |
|--------|-------------|
| `unitVec.RhVec` | Convert to Rhino Vector3d |
| `unitVec.Draw(startPt, scale)` | Draw as line with arrow |
| `UnitVec.ofRhVec(rhVector3d)` | Unitize from Rhino Vector3d |
| `UnitVec.toRhVec(unitVec)` | Static conversion to Rhino Vector3d |

#### Rhino.Geometry.Vector3d Extensions
| Member | Description |
|--------|-------------|
| `vector3d.Vc` | Convert to Euclid 2D vector (ignores Z) |
| `vector3d.Vec` | Convert to Euclid 3D vector |
| `vector3d.UnitVc` | Convert to Euclid 2D unit vector (ignores Z) |
| `vector3d.UnitVec` | Convert to Euclid 3D unit vector |

#### Rhino.Geometry.Vector3f Extensions
| Member | Description |
|--------|-------------|
| `vector3f.Vc` | Convert to Euclid 2D vector (ignores Z) |
| `vector3f.Vec` | Convert to Euclid 3D vector |
| `vector3f.UnitVc` | Convert to Euclid 2D unit vector |
| `vector3f.UnitVec` | Convert to Euclid 3D unit vector |

### Lines

#### Euclid.Line2D
| Member | Description |
|--------|-------------|
| `line2d.RhLine` | Convert to Rhino Line with Z=0.0 |
| `line2d.Draw()` | Draw in Rhino on current layer |
| `line2d.Draw(layer)` | Draw on specified layer |
| `line2d.Draw(layer, name)` | Draw with layer and name |
| `Line2D.ofRhLine(rhLine)` | Convert from Rhino Line (ignores Z) |
| `Line2D.toRhLine(line2d)` | Static conversion to Rhino Line |
| `Line2D.draw(line2d)` | Static draw function |
| `Line2D.drawWithZ(z, line2d)` | Draw at specified Z level |

#### Euclid.Line3D
| Member | Description |
|--------|-------------|
| `line3d.RhLine` | Convert to Rhino Line |
| `line3d.Draw()` | Draw in Rhino on current layer |
| `line3d.Draw(layer)` | Draw on specified layer |
| `line3d.Draw(layer, name)` | Draw with layer and name |
| `Line3D.ofRhLine(rhLine)` | Convert from Rhino Line |
| `Line3D.toRhLine(line3d)` | Static conversion to Rhino Line |
| `Line3D.draw(line3d)` | Static draw function |

#### Rhino.Geometry.Line Extensions
| Member | Description |
|--------|-------------|
| `line.Line2D` | Convert to Euclid 2D line (ignores Z) |
| `line.Line3D` | Convert to Euclid 3D line |

### Polylines

#### Euclid.Polyline2D
| Member | Description |
|--------|-------------|
| `polyline2d.RhPolyline` | Convert to Rhino Polyline in XY plane |
| `polyline2d.RhPolylineCurve` | Convert to Rhino PolylineCurve |
| `polyline2d.RhPolylineZ(z)` | Convert with given Z value |
| `Polyline2D.ofRhPolyline(rhPolyline)` | Convert from Rhino Polyline (ignores Z) |
| `Polyline2D.toRhPolyline(polyline2d)` | Static conversion |
| `Polyline2D.toRhPolylineCurve(polyline2d)` | Static conversion to curve |
| `Polyline2D.toRhPolylineZ(z, polyline2d)` | Static conversion with Z |
| `Polyline2D.draw(polyline2d)` | Draw in XY plane |
| `Polyline2D.drawZ(z, polyline2d)` | Draw at specified Z level |

#### Euclid.Polyline3D
| Member | Description |
|--------|-------------|
| `polyline3d.RhPolyline` | Convert to Rhino Polyline |
| `polyline3d.RhPolylineCurve` | Convert to Rhino PolylineCurve |
| `Polyline3D.ofRhPolyline(rhPolyline)` | Convert from Rhino Polyline |
| `Polyline3D.toRhPolyline(polyline3d)` | Static conversion |
| `Polyline3D.toRhPolylineCurve(polyline3d)` | Static conversion to curve |
| `Polyline3D.draw(polyline3d)` | Draw in Rhino |

#### Rhino.Geometry.Polyline Extensions
| Member | Description |
|--------|-------------|
| `polyline.Polyline2D` | Convert to Euclid 2D polyline (ignores Z) |
| `polyline.Polyline3D` | Convert to Euclid 3D polyline |

### Loops

#### Euclid.Loop (Closed 2D Polyline)
| Member | Description |
|--------|-------------|
| `loop.RhPolyline` | Convert to closed Rhino Polyline in XY plane |
| `loop.RhPolylineZ(z)` | Convert with given Z value |
| `Loop.createOfRhPoly(minSegLen, snapTol, rhPolylineCurve)` | Create from Rhino PolylineCurve |
| `Loop.createOfRhGuid(minSegLen, snapTol, guid)` | Create from Guid of Rhino curve |
| `Loop.draw(loop)` | Draw in XY plane |
| `Loop.drawZ(z, loop)` | Draw at specified Z level |

### Rectangles

#### Euclid.Rect2D (2D Rectangle)
| Member | Description |
|--------|-------------|
| `rect2d.RhPolyline` | Convert to closed Rhino Polyline in XY plane |
| `rect2d.RhPolylineCurve` | Convert to closed PolylineCurve |
| `rect2d.RhSurface` | Convert to Rhino Surface |

#### Euclid.Rect3D (3D Rectangle)
| Member | Description |
|--------|-------------|
| `rect3d.RhPolyline` | Convert to closed Rhino Polyline |
| `rect3d.RhPolylineCurve` | Convert to closed PolylineCurve |
| `rect3d.RhSurface` | Convert to Rhino Surface |

### Bounding Boxes/Rectangles

#### Euclid.BRect (2D Bounding Rectangle)
| Member | Description |
|--------|-------------|
| `brect.RhPolyline` | Convert to closed Rhino Polyline |
| `BRect.drawPolyLine(brect)` | Draw as polyline on current layer |

#### Euclid.BBox (3D Bounding Box)
| Member | Description |
|--------|-------------|
| `bbox.RhBBox` | Convert to Rhino BoundingBox |
| `BBox.drawPolyLine(bbox)` | Draw as polyline with 10 vertices |
| `BBox.drawMesh(bbox)` | Draw as mesh on current layer |

#### Rhino.Geometry.BoundingBox Extensions
| Member | Description |
|--------|-------------|
| `boundingBox.BBox` | Convert to Euclid BBox (reorders if inverted) |
| `BoundingBox.toBBox(boundingBox)` | Static conversion |

### Boxes

#### Euclid.Box (Oriented 3D Box)
| Member | Description |
|--------|-------------|
| `box.RhBox` | Convert to Rhino Box |
| `Box.drawPolyLine(box)` | Draw as polyline with 10 vertices |
| `Box.drawMesh(box)` | Draw as mesh on current layer |

#### Rhino.Geometry.Box Extensions
| Member | Description |
|--------|-------------|
| `box.BoxEuclid` | Convert to Euclid Box |
| `Box.toBoxEuclid(box)` | Static conversion |

### Planes

#### Euclid.PPlane (Parametrized Plane)
| Member | Description |
|--------|-------------|
| `pplane.RhPlane` | Convert to Rhino Plane |
| `pplane.Draw()` | Draw axes with labeled TextDots |
| `pplane.Draw(axLength)` | Draw with specified axis length |
| `pplane.Draw(axLength, suffixInDot)` | Draw with suffix text on labels |
| `pplane.Draw(axLength, suffixInDot, layer)` | Draw on specified layer |
| `PPlane.ofRhPlane(rhPlane)` | Convert from Rhino Plane |
| `PPlane.toRhPlane(pplane)` | Static conversion to Rhino Plane |
| `PPlane.draw(axLength, pplane)` | Static draw function, returns 6 Guids (3 lines, 3 dots) |

#### Rhino.Geometry.Plane Extensions
| Member | Description |
|--------|-------------|
| `plane.PPlane` | Convert to Euclid PPlane |
| `Plane.toPPlane(plane)` | Static conversion |

---

## Usage Examples

### Converting Points
```fsharp
// Euclid to Rhino
let euclidPt = Pt(1.0, 2.0)
let rhinoPt = euclidPt.RhPt              // Point3d(1, 2, 0)
let rhinoPtZ = euclidPt.RhPtZ(5.0)       // Point3d(1, 2, 5)

// Rhino to Euclid
let rhinoPoint = Point3d(1.0, 2.0, 3.0)
let euclidPt2D = rhinoPoint.Pt           // Pt(1, 2) - Z ignored
let euclidPt3D = rhinoPoint.Pnt          // Pnt(1, 2, 3)
```

### Drawing Geometry
```fsharp
// Draw a 2D point
let pt = Pt(10.0, 20.0)
pt.Draw()                                 // On current layer
pt.Draw("MyLayer")                        // On specified layer
pt.DrawDot("Point A")                     // As TextDot with label

// Draw a 3D line
let line = Line3D.create(Pnt.Origin, Pnt(10.0, 10.0, 10.0))
line.Draw()                               // On current layer
line.Draw("Geometry::Lines")              // On nested layer

// Draw a vector with arrow
let vec = Vec(5.0, 5.0, 0.0)
vec.Draw(Pnt.Origin, 1.0)                 // From origin, scale 1.0
vec.Draw(Pnt.Origin, 2.0, "Vectors")      // With scale and layer

// Draw a plane with labeled axes
let plane = PPlane.WorldXY
plane.Draw(10.0)                          // 10 unit axis length
plane.Draw(5.0, "_A", "Planes")           // With suffix and layer
```

### Working with Polylines
```fsharp
// Create and draw a polyline
let pts = [Pt(0,0); Pt(10,0); Pt(10,10); Pt(0,10)]
let polyline = Polyline2D.create pts
polyline |> Polyline2D.draw               // In XY plane
polyline |> Polyline2D.drawZ 5.0          // At Z=5

// Convert from Rhino polyline
let rhPolyline : Rhino.Geometry.Polyline = ...
let euclidPoly = rhPolyline.Polyline2D    // Ignores Z
let euclidPoly3D = rhPolyline.Polyline3D  // Keeps Z
```

### Bounding Box Operations
```fsharp
// Get Rhino bounding box and convert
let rhBBox = someGeometry.GetBoundingBox(true)
let euclidBBox = rhBBox.BBox

// Draw bounding box
euclidBBox |> BBox.drawPolyLine           // As wireframe
euclidBBox |> BBox.drawMesh               // As solid mesh
```

### Layer Management
```fsharp
// Layers are auto-created when drawing with layer parameter
let line = Line3D.create(Pnt.Origin, Pnt(10, 10, 10))
line.Draw("Parent::Child::Grandchild")    // Creates nested layers

// Use ColorUtil for distinct layer colors
let color = ColorUtil.randomForRhino()    // High saturation, distinct hue
```
