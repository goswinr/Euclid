---
description: Check that all standard transformation methods are present on a geometry type
allowed-tools: read_file, replace_string_in_file, multi_replace_string_in_file, semantic_search, grep_search, list_code_usages, file_search
arguments:
  - name: file
    description: The file to check for transformation methods (relative or absolute path)
    required: true
---

# Check Transformation Methods for $file

Audit `$file` to verify that all standard transformation methods are implemented consistently with the rest of the library.
If `$file` is `*`, check all geometry source files in `Src/` and `Src/TypeExtensions/`.

## 1. Transformation Methods Checklist

For each geometry type (points, vectors, lines, planes, boxes, etc.), verify the presence of these transformation methods where applicable:

### Matrix Transformations (4x4 Matrix) only for 3D Types
- [ ] `Transform(m:Matrix)` - instance member
- [ ] `transform (m:Matrix)` - static member with curried arguments

### RigidMatrix Transformations (rotation + translation only) only for 3D Types
- [ ] `TransformRigid(m:RigidMatrix)` - instance member
- [ ] `transformRigid (m:RigidMatrix)` - static member with curried arguments

### Rotation and Scale Naming Grammar
All rotate and scale members are curried and take the geometry last.
A raw angle argument always names its unit in the member name (`Degrees` or `Radians`).

| Static member | Instance member |
|---------------|-----------------|
| `rotate rotation geometry` | `Rotate rotation` |
| `rotateDegrees degrees geometry` | `RotateDegrees degrees` |
| `rotateRadians radians geometry` | `RotateRadians radians` |
| `rotateAround center rotation geometry` | `RotateAround center rotation` |
| `rotateAroundDegrees center degrees geometry` | `RotateAroundDegrees center degrees` |
| `rotateAroundRadians center radians geometry` | `RotateAroundRadians center radians` |
| `scale factor geometry` | `Scale factor` |
| `scaleOn center factor geometry` | `ScaleOn center factor` |

Vectors carry no location, so they get no `rotateAround` or `scaleOn` members.
Do not use the obsolete names `rotateBy`, `rotateWithCenter` or `rotateWithCenterBy`.

### Quaternion Rotations only for 3D Types
- [ ] `Rotate(q:Quaternion)` - instance member for rotation around origin
- [ ] `rotate (q:Quaternion)` - static member with curried arguments
- [ ] `RotateAround (cen:Pnt) (q:Quaternion)` - curried instance member for rotation around a point
- [ ] `rotateAround (cen:Pnt) (q:Quaternion)` - static member with curried arguments

### 2D Rotation for 2D Types
- [ ] `Rotate`, `RotateDegrees`, `RotateRadians` and their static counterparts - rotation around the origin
- [ ] `RotateAround`, `RotateAroundDegrees`, `RotateAroundRadians` and their static counterparts - rotation around a center (not for vectors)
- [ ] `Rotate90CW` / `Rotate90CCW` - quarter-turn rotations
- [ ] `RotateByQuarterCircle(n:int)` - rotation by multiples of 90°

### Rotation in the XY Plane for 3D Types
- [ ] `rotateOnZ (r:Rotation2D)` and `rotateOnZWithCenter (cen:Pnt) (r:Rotation2D)` - rotation around the world Z-axis

### Scale
- [ ] `Scale` / `scale` - scale from the origin
- [ ] `ScaleOn` / `scaleOn` - scale from a center point (not for vectors)

### Translation / Move Methods
- [ ] `Move(v:Vec)` or `Move(v:Vc)` - instance member to translate by a vector
- [ ] `move (v:Vec)` or `move (v:Vc)` - static member with curried arguments
- [ ] `translate (v:Vec)` - static member (alias for move)

### Axis-Aligned Translation
- [ ] `MoveX(distance:float)` - instance member to move along X-axis
- [ ] `MoveY(distance:float)` - instance member to move along Y-axis
- [ ] `MoveZ(distance:float)` - instance member to move along Z-axis (3D types only)
- [ ] `moveX (distance:float)` - static member
- [ ] `moveY (distance:float)` - static member
- [ ] `moveZ (distance:float)` - static member (3D types only)

## 2. Type-Specific Requirements

### For 3D Point Types (Pnt)
All transformation methods should be present including Matrix, RigidMatrix, Quaternion, and Move methods.

### For 3D Vector Types (Vec, UnitVec)
- Matrix and RigidMatrix transform only rotation/scale (no translation component)
- Quaternion rotation should preserve vector length for UnitVec
- Move methods may not apply (vectors are directions, not positions)

### For 2D Types (Pt, Vc, Line2D)
- Only 2D transformations apply
- Rotation2D instead of Quaternion
- No Z-axis methods

### For Lines (Line2D, Line3D)
All point-based transformations should apply to both endpoints.

### For Boxes (Box, BBox, FreeBox)
Transformation may require special handling depending on box type (axis-aligned vs oriented).

### For Planes (PPlane, NPlane)
Point and normal vector should be transformed consistently.

## 3. Consistency Checks

For each transformation method found:

- [ ] Instance member uses PascalCase (e.g., `Move`, `Transform`)
- [ ] Static member uses camelCase (e.g., `move`, `transform`)
- [ ] Static members have curried parameter order: `transformation -> geometry -> result`
- [ ] Docstrings clearly describe the transformation
- [ ] Implementation correctly applies transformation to all relevant components

## 4. Missing Methods Report

Generate a summary table showing:

| Type | Matrix | RigidMatrix | Quaternion | QuatAround | Rotate2D | Move | MoveX | MoveY | MoveZ |
|------|--------|-------------|------------|------------|----------|------|-------|-------|-------|
| Pnt  | ✓/✗    | ✓/✗         | ✓/✗        | ✓/✗            | N/A      | ✓/✗  | ✓/✗   | ✓/✗   | ✓/✗   |
| Vec  | ✓/✗    | ✓/✗         | ✓/✗        | N/A            | ✓/✗      | N/A  | N/A   | N/A   | N/A   |
| ...  | ...    | ...         | ...        | ...            | ...      | ...  | ...   | ...   | ...   |

## 5. Implementation Verification

For each existing method:

- Verify the transformation is mathematically correct
- Check that error handling follows project conventions (using `EuclidErrors`)
- Ensure tolerance checks are applied where needed
- Validate that unit-length invariants are preserved for UnitVec/UnitVc

## 6. Implement  Missing Methods

If methods are missing, provide implementation following the existing patterns:

```fsharp
// Example instance member pattern
member inline this.Transform(m:Matrix) =
    TypeName(m.TransformPnt(this.Start), m.TransformPnt(this.End))

// Example static member pattern
static member inline transform (m:Matrix) (obj:TypeName) =
    obj.Transform(m)
```

Reference existing implementations in:
- `Src/TypeExtensions/Pnt.fs` for point transformations
- `Src/TypeExtensions/Vec.fs` for vector transformations
- `Src/TypeExtensions/Line3D.fs` for line transformations
