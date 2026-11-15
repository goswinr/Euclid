# 3D offset test cases in Rhino

`offset3DCases.3dm` shows 31 cases of the 3D polyline offsets (`Polyline3D.offset…` and `Offset3D`) for checking them visually.
Each case has a numbered text dot below it.
The drawing was made with Euclid at commit `1e959f3`.

| File | Content |
|------|---------|
| `offset3DCases.3dm` | The drawing, a Rhino 8 file in millimeters. |
| `drawOffset3DCases.fsx` | The F# script that draws it. |

## Running the script

1. Build Euclid.Rhino against the local Euclid project, so that its output folder also contains the local `Euclid.dll`:
   `dotnet build D:/Git/_Euclid_/Euclid.Rhino/Euclid.Rhino.fsproj -c Release -f net8.0`
2. Run `drawOffset3DCases.fsx` in an F# script host inside Rhino 8, such as Fesh or Caddis.
   It loads `Euclid.dll` and `Euclid.Rhino.dll` from `D:/Git/_Euclid_/Euclid.Rhino/bin/Release/net8.0/`, and `Rhino.Scripting.FSharp` from NuGet.

The script deletes everything on the `Offset3D` layers before it draws, so it can run again.
At the end it zooms the Top and Perspective views to the drawing, and it prints the full message of every case that throws.
Rhino keeps a loaded DLL until it restarts, so restart Rhino after you rebuild Euclid.

## Layout

The 13 cases in rows 1 and 2 are the general shapes. The 18 cases in rows 3 to 5 are special cases.
All of them stand next to each other on the ground plane.

| Layer | Content |
|-------|---------|
| `Offset3D::Input` | The input polylines, in black. |
| `Offset3D::Vertices` | The vertices of the input polylines that have up to 30 points, to show collinear points. |
| `Offset3D::Offsets` | The results, one color per variant, named `<case number> <variant>`. The text dot says which color is which. |
| `Offset3D::Moves` | Lines from each input point to its offset point, for some 3D cases. |
| `Offset3D::RefNormal` | An arrow for the reference normal, for the cases that use one. The cases without an arrow use the average normal. |
| `Offset3D::Labels` | The text dots. When a variant throws, its dot says `THROWS` with the error message. |

## Cases

| # | Input | What to look for |
|---|-------|------------------|
| 1 | Half circle, 2° per point, closed by its diameter | The arc stays an arc, inside (+1) and outside (-1). |
| 2 | Circle of 200 points, tilted 30° to the reference normal | The in-plane offset stays in the plane of the circle. |
| 3 | Helix, 1° per point | Both offsets follow the helix. |
| 4 | Orthogonal route along X, Y, Z and X | Each corner offsets in its own plane, oriented by the reference normal. |
| 5 | Closed hexagon on the edges of a cube | The average normal orients the corners. |
| 6 | Coarse spiral | In-plane and perpendicular offsets combined. |
| 7 | Concave L | Constant and variable distances. |
| 8 | Collinear point between two corners in different planes | Constant and variable offsets are equal. The collinear point takes a direction between the two corner planes. |
| 9 | Square with a split edge, distances 1 1 2 | Proportional, Project and Skip on the split edge. |
| 10 | Arc, 2° per point, distance from 1 to 1.44 | A gentle taper: the arc stays smooth. |
| 11 | The same arc, distance from 1 to 2.76 | A steep taper: the result folds, see [Known problem](#known-problem). |
| 12 | Teardrop, distance from 1 to 1.6 | The tip and the arc with a slowly growing distance. |
| 13 | Non-planar quad | One perpendicular distance per point: 0, 1, 2 and 3. |
| 14 | Open, straight runs of collinear points at both ends | The runs stay parallel. The segment between the corner in XY and the corner in YZ twists by 15°, as in case 8. |
| 15 | Open, straight ends, distances 2 1 1 2 | The open ends are offset exactly. The collinear points lie on the line between the exact points. |
| 16 | Open, both ends on one line with a gap | Each end is offset square to its own segment. The ends are not joined. |
| 17 | Open, the end overlaps the start on one line | As in case 16. |
| 18 | As case 16, but not planar | As in case 16. |
| 19 | Closed, starting in the middle of an edge, distances 1 1 1 1 2 | The start point is handled like any other collinear point. Skip removes it, and the result stays closed. |
| 20 | Straight line without corners | The reference normal defines the plane, also when tilted by 45° (blue). |
| 21 | Straight line, distances 1 2 3 4 | The distance changes linearly between the two ends. Skip keeps only the ends. |
| 22 | Straight diagonal line in 3D | In-plane and perpendicular offsets. |
| 23 | Single segment | Works with a reference normal. Throws with the average normal, because two points span no area. |
| 24 | Vertical line, with the reference normal along it | Throws: the normal cannot define a plane. |
| 25 | Planar in XY, with the reference normal along X | Throws: the normal is perpendicular to the plane of every corner. |
| 26 | Spike that turns by 174° | Works, with a long miter. |
| 27 | Spike that turns by 176° | Throws: `failAtUTurnAbove` is 175° by default. |
| 28 | Straight back on itself, 180° | Throws at the U-turn. |
| 29 | Duplicate point | Throws at the zero-length segment. |
| 30 | Kink of 2.4°, distance 1 then 1.5 | Below 2.5° the kink counts as collinear. The result is smooth. |
| 31 | Kink of 2.6°, distance 1 then 1.5 | Above 2.5° the kink is a corner, and the result folds, see [Known problem](#known-problem). |

## Known problem

Cases 11 and 31 show a fold that is not fixed yet.

A point where the polyline turns by at least 2.5° (`considerCollinearBelow`) is a corner.
When the offset distance changes there, the corner is placed where the two offset lines intersect.
That point slides along the polyline by Δ / sin(turn), where Δ is the change in distance.
Just above 2.5° it slides by about 23 × Δ.
If it slides past a neighboring point, the offset folds back on itself.

- In case 31 the corner of the 2.6° kink lands at x = -1.05, behind the start point at x = 0.
- In case 11 the corners are forced by the accumulated turn on a finely sampled arc. They slide by about 1.15, roughly three segments.

The 2D variable offsets (`Offset2D.offsetVariable`) use the same rule.
