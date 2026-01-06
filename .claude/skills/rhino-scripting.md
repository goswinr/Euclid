# Rhino.Scripting Agent Skill

This skill enables you to help users write scripts using the Rhino.Scripting library for Rhino3D CAD automation.

## Library Overview

**Rhino.Scripting** is an F# library that reimplements the RhinoScript API (originally from VBScript/Python) for use in F# and C#. It provides ~900 static methods on the `RhinoScriptSyntax` class for scripting the Rhino3D CAD application.

### Quick Setup

F# Script (.fsx) - Default template for Fesh editor in Rhino

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
//open Rhino //don't do this! see https://github.com/goswinr/Fesh.Rhino/issues/25
open Rhino.Scripting
open Rhino.Scripting.FSharp
open Euclid
open Euclid.UtilEuclid
open Fesher

type rs = RhinoScriptSyntax

let crv = rs.GetObject("Select a curve", rs.Filter.Curve)

...
```


### Key Properties

- `rs.Doc` - The current active Rhino document
- `rs.Ot` - Object table of the current document (shortcut for `Doc.Objects`)
- `rs.Sticky` - Dictionary to store state between scripting sessions
- `rs.Filter` - Object type enumeration for selection filters

## API Categories

### Geometry Creation

| Method | Description |
|--------|-------------|
| `AddPoint(x, y, z)` | Add a point object |
| `AddLine(start, end)` | Add a line between two points |
| `AddPolyline(points)` | Add a polyline through points |
| `AddCircle(plane, radius)` | Add a circle |
| `AddArc(plane, radius, angle)` | Add an arc |
| `AddArc3Pt(start, end, pointOnArc)` | Add a 3-point arc |
| `AddEllipse(plane, rx, ry)` | Add an ellipse |
| `AddRectangle(plane, width, height)` | Add a rectangle |
| `AddCurve(points, degree)` | Add a NURBS curve |
| `AddInterpCurve(points)` | Add interpolated curve through points |
| `AddSphere(center, radius)` | Add a sphere |
| `AddCylinder(plane, radius, height)` | Add a cylinder |
| `AddBox(corners)` | Add a box |
| `AddCone(plane, radius, height)` | Add a cone |
| `AddSrfPt(points)` | Add surface from corner points |
| `AddPlanarSrf(curves)` | Add planar surface from closed curves |
| `AddLoftSrf(curves)` | Add lofted surface through curves |
| `AddSweep1(rail, shapes)` | Add 1-rail sweep surface |
| `AddSweep2(rail1, rail2, shapes)` | Add 2-rail sweep surface |
| `AddRevSrf(curve, axis)` | Add surface of revolution |
| `AddMesh(vertices, faces)` | Add a mesh object |
| `AddText(text, plane, height)` | Add text object |
| `AddLeader(points, text)` | Add a leader annotation |
| `AddLinearDimension(...)` | Add linear dimension |

### Object Manipulation

| Method | Description |
|--------|-------------|
| `CopyObject(objectId)` | Copy an object |
| `CopyObjects(objectIds)` | Copy multiple objects |
| `DeleteObject(objectId)` | Delete an object |
| `DeleteObjects(objectIds)` | Delete multiple objects |
| `MoveObject(objectId, translation)` | Move an object |
| `MoveObjects(objectIds, translation)` | Move multiple objects |
| `RotateObject(objectId, center, angle)` | Rotate an object |
| `ScaleObject(objectId, origin, scale)` | Scale an object |
| `MirrorObject(objectId, start, end)` | Mirror an object |
| `TransformObject(objectId, xform)` | Transform with matrix |
| `OrientObject(objectId, ref, target)` | Orient object from reference to target |

### Object Properties

| Method | Description |
|--------|-------------|
| `ObjectName(objectId)` | Get object name |
| `ObjectName(objectId, name)` | Set object name |
| `ObjectColor(objectId)` | Get object color |
| `ObjectColor(objectId, color)` | Set object color |
| `ObjectLayer(objectId)` | Get object's layer name |
| `ObjectLayer(objectId, layer)` | Move object to layer |
| `ObjectMaterialIndex(objectId)` | Get material index |
| `ObjectType(objectId)` | Get object type |
| `IsPoint(objectId)` | Check if object is a point |
| `IsCurve(objectId)` | Check if object is a curve |
| `IsSurface(objectId)` | Check if object is a surface |
| `IsBrep(objectId)` | Check if object is a Brep |
| `IsMesh(objectId)` | Check if object is a mesh |

### Selection

| Method | Description |
|--------|-------------|
| `GetObject(message)` | Prompt user to select one object |
| `GetObjects(message)` | Prompt user to select multiple objects |
| `GetPoint(message)` | Prompt user to pick a point |
| `GetPoints(message)` | Prompt user to pick multiple points |
| `SelectObject(objectId)` | Select an object |
| `SelectObjects(objectIds)` | Select multiple objects |
| `UnselectObject(objectId)` | Unselect an object |
| `UnselectAllObjects()` | Unselect all objects |
| `SelectedObjects()` | Get all selected objects |
| `AllObjects()` | Get all objects in document |
| `ObjectsByLayer(layer)` | Get objects on a layer |
| `ObjectsByType(type)` | Get objects by type |
| `ObjectsByColor(color)` | Get objects by color |
| `ObjectsByGroup(group)` | Get objects in a group |
| `VisibleObjects()` | Get all visible objects |
| `HiddenObjects()` | Get all hidden objects |
| `LockedObjects()` | Get all locked objects |

### Curve Operations

| Method | Description |
|--------|-------------|
| `CurveLength(curveId)` | Get curve length |
| `CurveDomain(curveId)` | Get curve parameter domain |
| `CurveStartPoint(curveId)` | Get curve start point |
| `CurveEndPoint(curveId)` | Get curve end point |
| `CurveMidPoint(curveId)` | Get curve midpoint |
| `CurvePoints(curveId)` | Get curve control points |
| `CurveEditPoints(curveId)` | Get curve edit points |
| `CurveClosestPoint(curveId, point)` | Find closest point on curve |
| `EvaluateCurve(curveId, t)` | Evaluate curve at parameter |
| `CurveTangent(curveId, t)` | Get tangent at parameter |
| `CurveCurvature(curveId, t)` | Get curvature at parameter |
| `CurveNormal(curveId, t)` | Get normal at parameter |
| `DivideCurve(curveId, count)` | Divide curve by count |
| `DivideCurveLength(curveId, length)` | Divide curve by length |
| `CurveBooleanUnion(curves)` | Boolean union of curves |
| `CurveBooleanIntersection(c1, c2)` | Boolean intersection |
| `CurveBooleanDifference(c1, c2)` | Boolean difference |
| `OffsetCurve(curveId, direction, dist)` | Offset a curve |
| `ExtendCurve(curveId, type, side, obj)` | Extend curve |
| `TrimCurve(curveId, interval)` | Trim curve |
| `SplitCurve(curveId, params)` | Split curve at parameters |
| `JoinCurves(curves)` | Join curves |
| `ExplodeCurves(curves)` | Explode polycurves |
| `ReverseCurve(curveId)` | Reverse curve direction |
| `CloseCurve(curveId)` | Close an open curve |
| `IsCurveClosed(curveId)` | Check if curve is closed |
| `IsCurvePlanar(curveId)` | Check if curve is planar |
| `IsCurveLinear(curveId)` | Check if curve is linear |

### Surface Operations

| Method | Description |
|--------|-------------|
| `SurfaceArea(surfaceId)` | Get surface area |
| `SurfaceVolume(surfaceId)` | Get closed surface volume |
| `SurfaceDomain(surfaceId, dir)` | Get surface parameter domain |
| `SurfaceNormal(surfaceId, uv)` | Get surface normal at UV |
| `SurfaceClosestPoint(surfaceId, pt)` | Find closest point on surface |
| `EvaluateSurface(surfaceId, u, v)` | Evaluate surface at UV |
| `SurfaceCurvature(surfaceId, uv)` | Get curvature at UV |
| `SurfaceFrame(surfaceId, uv)` | Get frame at UV |
| `IsSurfaceClosed(surfaceId, dir)` | Check if closed in direction |
| `IsSurfacePlanar(surfaceId)` | Check if surface is planar |
| `SurfaceIsocurveDensity(surfaceId)` | Get/set isocurve density |
| `OffsetSurface(surfaceId, dist)` | Offset a surface |
| `ExtendSurface(surfaceId, edge, ext)` | Extend surface edge |
| `TrimSurface(surfaceId, curves)` | Trim surface with curves |
| `SplitBrep(brepId, cutter)` | Split Brep with cutter |
| `BooleanUnion(breps)` | Boolean union of Breps |
| `BooleanIntersection(breps)` | Boolean intersection |
| `BooleanDifference(input, cutter)` | Boolean difference |
| `CapPlanarHoles(brepId)` | Cap planar holes |
| `ExtractSurface(brepId, indices)` | Extract surfaces from Brep |
| `ExplodePolysurfaces(breps)` | Explode polysurfaces |
| `JoinSurfaces(surfaces)` | Join surfaces into Brep |

### Mesh Operations

| Method | Description |
|--------|-------------|
| `MeshArea(meshId)` | Get mesh area |
| `MeshVolume(meshId)` | Get mesh volume |
| `MeshVertices(meshId)` | Get mesh vertices |
| `MeshFaces(meshId)` | Get mesh faces |
| `MeshFaceVertices(meshId)` | Get face vertex indices |
| `MeshVertexNormals(meshId)` | Get vertex normals |
| `MeshFaceNormals(meshId)` | Get face normals |
| `MeshVertexColors(meshId)` | Get/set vertex colors |
| `MeshClosestPoint(meshId, point)` | Find closest point on mesh |
| `MeshBooleanUnion(meshes)` | Boolean union of meshes |
| `MeshBooleanIntersection(m1, m2)` | Boolean intersection |
| `MeshBooleanDifference(m1, m2)` | Boolean difference |
| `MeshOffset(meshId, distance)` | Offset mesh |
| `MeshQuadsToTriangles(meshId)` | Convert quads to triangles |
| `DisjointMeshCount(meshId)` | Count disjoint pieces |
| `SplitDisjointMesh(meshId)` | Split into disjoint meshes |

### Layers

| Method | Description |
|--------|-------------|
| `AddLayer(name)` | Add a new layer |
| `AddLayer(name, color)` | Add layer with color |
| `DeleteLayer(name)` | Delete a layer |
| `CurrentLayer()` | Get current layer name |
| `CurrentLayer(name)` | Set current layer |
| `LayerColor(layer)` | Get layer color |
| `LayerColor(layer, color)` | Set layer color |
| `LayerVisible(layer)` | Get layer visibility |
| `LayerVisible(layer, visible)` | Set layer visibility |
| `LayerLocked(layer)` | Get layer locked state |
| `LayerLocked(layer, locked)` | Set layer locked state |
| `LayerPrintColor(layer)` | Get layer print color |
| `LayerPrintWidth(layer)` | Get layer print width |
| `LayerLinetype(layer)` | Get/set layer linetype |
| `LayerMaterialIndex(layer)` | Get/set layer material |
| `ParentLayer(layer)` | Get parent layer |
| `LayerChildCount(layer)` | Get child layer count |
| `LayerChildren(layer)` | Get child layer names |
| `LayerNames()` | Get all layer names |
| `LayerCount()` | Get layer count |
| `IsLayer(name)` | Check if layer exists |
| `IsLayerEmpty(name)` | Check if layer has no objects |
| `IsLayerOn(name)` | Check if layer is on |
| `IsLayerVisible(name)` | Check if layer is visible |
| `IsLayerLocked(name)` | Check if layer is locked |
| `IsLayerCurrent(name)` | Check if layer is current |
| `PurgeLayer(name)` | Purge unused layer |
| `RenameLayer(oldName, newName)` | Rename a layer |

### Views and Display

| Method | Description |
|--------|-------------|
| `CurrentView()` | Get current view name |
| `CurrentView(name)` | Set current view |
| `ViewCamera(view)` | Get view camera location |
| `ViewCamera(view, point)` | Set view camera location |
| `ViewTarget(view)` | Get view target point |
| `ViewTarget(view, point)` | Set view target point |
| `ViewCameraUp(view)` | Get camera up direction |
| `ViewCameraLens(view)` | Get/set camera lens length |
| `ZoomExtents(view)` | Zoom to show all objects |
| `ZoomSelected(view)` | Zoom to selected objects |
| `ZoomBoundingBox(box)` | Zoom to bounding box |
| `Redraw()` | Redraw all views |
| `EnableRedraw(enable)` | Enable/disable redraw |
| `ViewDisplayMode(view)` | Get/set display mode |
| `IsViewMaximized(view)` | Check if view is maximized |
| `MaximizeRestoreView(view)` | Toggle maximize/restore |
| `ViewSize(view)` | Get view pixel dimensions |
| `ViewNames()` | Get all view names |
| `NamedViews()` | Get named views |
| `AddNamedView(name)` | Save current view |
| `RestoreNamedView(name)` | Restore named view |
| `DeleteNamedView(name)` | Delete named view |

### Transformations

| Method | Description |
|--------|-------------|
| `XformIdentity()` | Identity transform |
| `XformZero()` | Zero transform |
| `XformTranslation(vector)` | Translation transform |
| `XformScale(origin, scale)` | Uniform scale transform |
| `XformScale(plane, x, y, z)` | Non-uniform scale |
| `XformRotation(angle, axis, center)` | Rotation transform |
| `XformRotation2(angle, axis, point)` | Alternative rotation |
| `XformRotation3(start, end, center)` | Rotation from vectors |
| `XformRotation4(x0, y0, z0, x1, y1, z1)` | Full frame rotation |
| `XformMirror(point, normal)` | Mirror transform |
| `XformPlanarProjection(plane)` | Planar projection |
| `XformShear(plane, x, y, z)` | Shear transform |
| `XformMultiply(xform1, xform2)` | Multiply transforms |
| `XformInverse(xform)` | Inverse transform |

### Vectors and Points

| Method | Description |
|--------|-------------|
| `VectorCreate(from, to)` | Create vector from points |
| `VectorAdd(v1, v2)` | Add vectors |
| `VectorSubtract(v1, v2)` | Subtract vectors |
| `VectorScale(v, scale)` | Scale a vector |
| `VectorUnitize(v)` | Get unit vector |
| `VectorLength(v)` | Get vector length |
| `VectorReverse(v)` | Reverse vector direction |
| `VectorRotate(v, angle, axis)` | Rotate vector |
| `VectorDotProduct(v1, v2)` | Dot product |
| `VectorCrossProduct(v1, v2)` | Cross product |
| `VectorAngle(v1, v2)` | Angle between vectors |
| `PointAdd(p, v)` | Add vector to point |
| `PointSubtract(p1, p2)` | Subtract points (returns vector) |
| `PointTransform(p, xform)` | Transform a point |
| `PointDivide(p, divisor)` | Divide point coordinates |
| `PointScale(p, scale)` | Scale point coordinates |
| `PointCompare(p1, p2, tol)` | Compare points with tolerance |
| `Distance(p1, p2)` | Distance between points |

### User Data

| Method | Description |
|--------|-------------|
| `SetUserText(objectId, key, value)` | Set user text on object |
| `GetUserText(objectId, key)` | Get user text from object |
| `GetAllUserText(objectId)` | Get all user text key-value pairs |
| `DeleteUserText(objectId, key)` | Delete user text |
| `IsUserText(objectId)` | Check if object has user text |
| `SetDocumentUserText(key, value)` | Set document-level user text |
| `GetDocumentUserText(key)` | Get document-level user text |
| `DeleteDocumentUserText(key)` | Delete document user text |
| `GetAllDocumentUserText()` | Get all document user text |

### Groups

| Method | Description |
|--------|-------------|
| `AddGroup(name)` | Create a new group |
| `AddGroup()` | Create unnamed group |
| `DeleteGroup(name)` | Delete a group |
| `AddObjectToGroup(objectId, group)` | Add object to group |
| `AddObjectsToGroup(objectIds, group)` | Add objects to group |
| `RemoveObjectFromGroup(objectId, group)` | Remove object from group |
| `ObjectGroups(objectId)` | Get object's group names |
| `GroupNames()` | Get all group names |
| `GroupCount()` | Get number of groups |
| `IsGroup(name)` | Check if group exists |
| `IsGroupEmpty(name)` | Check if group is empty |
| `GroupObjects(name)` | Get objects in group |
| `RenameGroup(oldName, newName)` | Rename a group |

### Blocks

| Method | Description |
|--------|-------------|
| `AddBlock(objects, base, name)` | Create block definition |
| `InsertBlock(name, point)` | Insert block instance |
| `InsertBlock2(name, xform)` | Insert with transform |
| `DeleteBlock(name)` | Delete block definition |
| `BlockObjects(name)` | Get objects in block |
| `BlockCount()` | Get number of blocks |
| `BlockNames()` | Get all block names |
| `BlockPath(name)` | Get linked block file path |
| `IsBlock(name)` | Check if block exists |
| `IsBlockEmbedded(name)` | Check if block is embedded |
| `IsBlockReference(objectId)` | Check if object is block instance |
| `BlockInstanceXform(objectId)` | Get instance transform |
| `BlockInstanceName(objectId)` | Get instance's block name |
| `ExplodeBlockInstance(objectId)` | Explode block instance |
| `RenameBlock(oldName, newName)` | Rename block definition |

### Materials

| Method | Description |
|--------|-------------|
| `AddMaterial()` | Add default material |
| `AddMaterial(name)` | Add named material |
| `DeleteMaterial(index)` | Delete material by index |
| `MaterialColor(index)` | Get material diffuse color |
| `MaterialColor(index, color)` | Set material diffuse color |
| `MaterialName(index)` | Get material name |
| `MaterialName(index, name)` | Set material name |
| `MaterialTexture(index)` | Get material texture path |
| `MaterialTexture(index, path)` | Set material texture |
| `MaterialBump(index)` | Get/set bump texture |
| `MaterialReflectivity(index)` | Get/set reflectivity |
| `MaterialTransparency(index)` | Get/set transparency |
| `MaterialShine(index)` | Get/set shininess |
| `IsMaterial(index)` | Check if material exists |
| `MaterialCount()` | Get material count |
| `MatchMaterial(source, targets)` | Copy material to objects |

### Document Properties

| Method | Description |
|--------|-------------|
| `UnitSystem()` | Get document unit system |
| `UnitSystem(units)` | Set document unit system |
| `UnitAbsoluteTolerance()` | Get absolute tolerance |
| `UnitAngleTolerance()` | Get angle tolerance |
| `DocumentName()` | Get document file name |
| `DocumentPath()` | Get document file path |
| `DocumentModified()` | Check if document is modified |
| `Notes()` | Get document notes |
| `Notes(text)` | Set document notes |
| `ReadFileVersion()` | Get file version |

### User Interface

| Method | Description |
|--------|-------------|
| `MessageBox(message)` | Show message box |
| `MessageBox(message, buttons, title)` | Message box with options |
| `StringBox(message, default, title)` | Input string dialog |
| `ListBox(items, message, title)` | Single selection list |
| `MultiListBox(items, message, title)` | Multi-selection list |
| `ComboListBox(items, message, title)` | Dropdown selection |
| `CheckListBox(items, message, title)` | Checkbox list |
| `PropertyListBox(items, values)` | Property editor dialog |
| `RealBox(message, default, title)` | Input number dialog |
| `IntegerBox(message, default, title)` | Input integer dialog |
| `OpenFileName(title, filter)` | Open file dialog |
| `SaveFileName(title, filter)` | Save file dialog |
| `BrowseForFolder(title, folder)` | Browse folder dialog |

### Utility

| Method | Description |
|--------|-------------|
| `Command(command)` | Run Rhino command |
| `Command(command, echo)` | Run command with echo option |
| `Sleep(milliseconds)` | Pause execution |
| `Print(message)` | Print to command line |
| `Prompt(message)` | Set command prompt |
| `EscapeTest()` | Check if Escape was pressed |
| `Clamp(min, max, value)` | Clamp value to range |
| `FrangePython(start, stop, step)` | Float range (Python semantics) |
| `FxrangePython(start, stop, step)` | Lazy float range |

### Coercion Functions

Coercion functions convert flexible inputs to specific types:

| Method | Description |
|--------|-------------|
| `CoercePoint3d(...)` | Convert to Point3d |
| `CoercePoint2d(...)` | Convert to Point2d |
| `CoerceVector3d(...)` | Convert to Vector3d |
| `CoercePlane(...)` | Convert to Plane |
| `CoerceLine(...)` | Convert to Line |
| `CoerceGuid(...)` | Convert to Guid |
| `CoerceRhinoObject(guid)` | Get RhinoObject from Guid |
| `CoerceCurve(guid)` | Get Curve geometry |
| `CoerceSurface(guid)` | Get Surface geometry |
| `CoerceBrep(guid)` | Get Brep geometry |
| `CoerceMesh(guid)` | Get Mesh geometry |
| `CoerceGeometry(guid)` | Get any geometry |
| `CoerceLayer(name)` | Get Layer object |
| `CoerceColor(...)` | Convert to Color |
| `Coerce3dPoint(...)` | Legacy: convert to Point3d |
| `Coerce3dVector(...)` | Legacy: convert to Vector3d |

## Common Patterns

### Add Object with Layer

```fsharp
// Use the Add method with layer parameter
let guid = rs.Add(geometry, "LayerName", "ObjectName")

// Or with layer index for better performance
let layerIdx = rs.AddLayer("MyLayer")
let guid = rs.Add(geometry, layerIdx, "ObjectName")
```

### Iterate Objects

```fsharp
// Get all curves on a layer
let curves = rs.ObjectsByLayer("MyCurves")
for curveId in curves do
    let length = rs.CurveLength(curveId)
    printfn "Curve length: %f" length
```

### Transform Objects

```fsharp
// Create a rotation transform and apply it
let xform = rs.XformRotation(45.0, Vector3d.ZAxis, Point3d.Origin)
rs.TransformObject(objectId, xform)
```

### User Text

```fsharp
// Store metadata on objects
rs.SetUserText(objectId, "Material", "Steel")
rs.SetUserText(objectId, "Weight", "150.5")

// Retrieve later
let material = rs.GetUserText(objectId, "Material")
```

### Selection with Filters

```fsharp
// Use Filter for object selection
let curves = rs.GetObjects("Select curves", rs.Filter.Curve)
let surfaces = rs.GetObjects("Select surfaces", rs.Filter.Surface)
let points = rs.GetObjects("Select points", rs.Filter.Point)
```

## Thread Safety

All methods are thread-safe and automatically marshal to the Rhino UI thread when necessary. You can safely call methods from async contexts:

```fsharp
async {
    let! _ = Async.SwitchToThreadPool()
    // These calls automatically marshal to UI thread
    let pt = rs.AddPoint(0.0, 0.0, 0.0)
    rs.Redraw()
}
```

## Error Handling

The library uses custom exceptions:

- `RhinoScriptingException` - General scripting errors
- `RhinoUserInteractionException` - User cancelled operation (e.g., pressed Escape)

```fsharp
try
    let obj = rs.GetObject("Select an object")
    // ... work with obj
with
| :? RhinoScriptingException as ex ->
    printfn "Script error: %s" ex.Message
| :? RhinoUserInteractionException ->
    printfn "User cancelled"
```

## Escape Key Handling

Check for user interruption in long-running scripts:

```fsharp
for i in 0 .. 10000 do
    rs.EscapeTest() // Throws OperationCanceledException if Escape pressed
    // ... do work
```

## Best Practices

1. **Disable redraw for batch operations** - Use `rs.EnableRedraw(false)` before many operations, then `rs.EnableRedraw(true)` after

2. **Use layer indices** - When adding many objects to the same layer, get the layer index once and reuse it

3. **Check for cancellation** - Call `rs.EscapeTest()` in loops to allow user interruption

4. **Use coercion functions** - They handle multiple input formats and provide clear error messages

5. **Sticky dictionary** - Use `rs.Sticky` to persist state between script runs
