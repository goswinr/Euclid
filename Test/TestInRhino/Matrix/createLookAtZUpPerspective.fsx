#r "C:/Program Files/Rhino 8/System/RhinoCommon.dll"
#r "D:/Git/_Euclid_/Euclid/bin/Release/net6.0/Euclid.dll"
#r "nuget: Rhino.Scripting.FSharp"

open Rhino.Geometry
open Rhino.Scripting
open Rhino.Scripting.FSharp
open Euclid

type rs = RhinoScriptSyntax

// Rebuild Euclid before running this script when Matrix.fs has changed.
let rhinoPoint (p:Pnt) = Point3d(p.X, p.Y, p.Z)

let drawLine layer (line:Line3D) =
    rs.AddLine(rhinoPoint line.From, rhinoPoint line.To)
    |> rs.setLayer layer
    |> ignore

let drawPoint layer (p:Pnt) =
    rs.AddPoint(rhinoPoint p)
    |> rs.setLayer layer
    |> ignore

// A square based pyramid in world coordinates.
let base0 = Pnt(-4., -4., 0.)
let base1 = Pnt( 4., -4., 0.)
let base2 = Pnt( 4.,  4., 0.)
let base3 = Pnt(-4.,  4., 0.)
let apex  = Pnt( 0.,  0., 7.)

let pyramidLines =
    [|
        Line3D(base0, base1); Line3D(base1, base2)
        Line3D(base2, base3); Line3D(base3, base0)
        Line3D(base0, apex);  Line3D(base1, apex)
        Line3D(base2, apex);  Line3D(base3, apex)
        
        Line3D(base0, base0 *3.0)
        Line3D(base1, base1 *1.5)
    |]

let target = Pnt(1., -1., 2.)
let eye = Pnt(10., -16., 6.)
let cameraLine = Line3D(eye, target)

// createLookAtZUp places the camera at the origin and points it along local -Z.
let view = Matrix.createLookAtZUp(eye, target)
let perspective = Matrix.createPerspectiveAlongNegZ(5., 3., 1., 100.)
let worldToClip = view *** perspective

// Projected x/y are normalized screen coordinates.  Enlarge and move that result
// beside the world-space pyramid so both drawings are visible in the same Rhino view.
let screenScale = 16.
let screenOffset = Vec(17., 0., 0.)
let placeOnScreen (p:Pnt) =
    Pnt(p.X * screenScale, p.Y * screenScale, 0.) + screenOffset

let projectedLines =
    pyramidLines
    |> Array.map (Line3D.transform worldToClip)
    |> Array.map (fun line -> Line3D(placeOnScreen line.From, placeOnScreen line.To))

let screenCorners =
    [|
        Pnt(-1., -1., 0.); Pnt(1., -1., 0.)
        Pnt(1., 1., 0.);   Pnt(-1., 1., 0.)
    |]
    |> Array.map placeOnScreen

// World-space input and viewing direction.
pyramidLines |> Array.iter (drawLine "LookAtZUp::World pyramid")
drawLine "LookAtZUp::Camera direction" cameraLine
drawPoint "LookAtZUp::Camera direction" eye
drawPoint "LookAtZUp::Camera direction" target

// Perspective result after the Z-up look-at view.  The rectangle is the near-plane frame.
projectedLines |> Array.iter (drawLine "LookAtZUp::Perspective projection")
for i in 0 .. 3 do
    let next = (i + 1) % 4
    drawLine "LookAtZUp::Projection frame" (Line3D(screenCorners[i], screenCorners[next]))

printfn "view:\n%A" view
printfn "perspective:\n%A" perspective
printfn "worldToClip:\n%A" worldToClip
printfn "Drawn: world pyramid, camera line, and its projected pyramid."
