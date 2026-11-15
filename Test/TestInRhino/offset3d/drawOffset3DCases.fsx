// Draws test cases of the 3D offsets in Rhino, next to each other in rows, each labeled with a text dot.
// The drawing is saved in offset3DCases.3dm next to this script, see README.md.
// Running it again replaces the objects on the Offset3D layers.
// It uses the local build of Euclid.Rhino, which references the local Euclid project:
// dotnet build D:/Git/_Euclid_/Euclid.Rhino/Euclid.Rhino.fsproj -c Release -f net8.0
#r "C:/Program Files/Rhino 8/System/RhinoCommon.dll"
#I "D:/Git/_Euclid_/Euclid.Rhino/bin/Release/net8.0/"
#r "Euclid.dll"
#r "Euclid.Rhino.dll"
#r "nuget: Rhino.Scripting.FSharp"

open System
open System.Drawing
open Euclid
open Rhino.Scripting
open Rhino.Scripting.FSharp
type rs = RhinoScriptSyntax

let rad (deg:float) = deg * Math.PI / 180.0
let pl (pts: Pnt list) = Polyline3D.createFromPnts pts
let ds (xs: float list) = ResizeArray xs
let zeros n = ResizeArray (List.replicate n 0.0)

type Variant = { Label: string; Color: Color; Result: Result<Polyline3D, string> }
type Case = { Title: string; Input: Polyline3D; RefNormal: UnitVec voption; ShowMoves: bool; Variants: Variant list }

let red, blue, green, orange = Color.Crimson, Color.RoyalBlue, Color.ForestGreen, Color.DarkOrange
let v label color result = { Label = label; Color = color; Result = Ok result }
/// For variants that may throw, the message is shown in the label.
let tryV label color (f: unit -> Polyline3D) =
    let firstLine (m: string) = (m.Split '\n').[0].Trim()
    { Label = label; Color = color; Result = try Ok (f ()) with e -> Error (firstLine e.Message) }
let z = UnitVec.ZAxis

let cases = [
    let half = pl [ for i in 0 .. 90 -> let a = rad (2.0 * float i) in Pnt(10.0 * cos a, 10.0 * sin a, 0.0)
                    yield Pnt(10.0, 0.0, 0.0) ]
    { Title = "Half circle, 2° per point, + diameter\nin-plane +1 red, -1 blue"
      Input = half; RefNormal = ValueNone; ShowMoves = false
      Variants = [ v "+1" red (Polyline3D.offset 1.0 0.0 half); v "-1" blue (Polyline3D.offset -1.0 0.0 half) ] }

    let q = Quaternion.createFromDegreesUnitVec UnitVec.XAxis 30.0
    let circle = pl [ for i in 0 .. 200 -> let a = 2.0 * Math.PI * float (i % 200) / 200.0 in Pnt.rotate q (Pnt(10.0 * cos a, 10.0 * sin a, 0.0)) ]
    { Title = "Circle 200 pts, 30° to refNormal\nin-plane 1 red, perp 1 green"
      Input = circle; RefNormal = ValueSome z; ShowMoves = false
      Variants = [ v "in-plane" red (Polyline3D.offsetByNormal z 1.0 0.0 circle); v "perp" green (Polyline3D.offsetByNormal z 0.0 1.0 circle) ] }

    let helix = pl [ for i in 0 .. 360 -> let a = rad (float i) in Pnt(10.0 * cos a, 10.0 * sin a, 0.02 * float i) ]
    { Title = "Helix, 1° per point\nin-plane 1.5 red, perp 1.5 green"
      Input = helix; RefNormal = ValueSome z; ShowMoves = false
      Variants = [ v "in-plane" red (Polyline3D.offsetByNormal z 1.5 0.0 helix); v "perp" green (Polyline3D.offsetByNormal z 0.0 1.5 helix) ] }

    let route = pl [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 10.0, 0.0); Pnt(10.0, 10.0, 10.0); Pnt(20.0, 10.0, 10.0)]
    { Title = "Orthogonal route X Y Z X\nin-plane 2 red, perp 2 green"
      Input = route; RefNormal = ValueSome z; ShowMoves = true
      Variants = [ v "in-plane" red (Polyline3D.offsetByNormal z 2.0 0.0 route); v "perp" green (Polyline3D.offsetByNormal z 0.0 2.0 route) ] }

    let hexagon = pl [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 10.0, 0.0); Pnt(10.0, 10.0, 10.0); Pnt(0.0, 10.0, 10.0); Pnt(0.0, 0.0, 10.0); Pnt(0.0, 0.0, 0.0)]
    { Title = "Closed hexagon on cube edges\nin-plane 1.5 red, perp 1.5 green"
      Input = hexagon; RefNormal = ValueNone; ShowMoves = true
      Variants = [ v "in-plane" red (Polyline3D.offset 1.5 0.0 hexagon); v "perp" green (Polyline3D.offset 0.0 1.5 hexagon) ] }

    let spiral = pl [Pnt(10.0, 0.0, 0.0); Pnt(0.0, 10.0, 5.0); Pnt(-10.0, 0.0, 10.0); Pnt(0.0, -10.0, 15.0); Pnt(10.0, 0.0, 20.0)]
    { Title = "Coarse spiral\nin-plane 1.5 + perp 1 red"
      Input = spiral; RefNormal = ValueSome z; ShowMoves = true
      Variants = [ v "both" red (Polyline3D.offsetByNormal z 1.5 1.0 spiral) ] }

    let lShape = pl [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 4.0, 0.0); Pnt(4.0, 4.0, 0.0); Pnt(4.0, 10.0, 0.0); Pnt(0.0, 10.0, 0.0); Pnt(0.0, 0.0, 0.0)]
    { Title = "Concave L\nconstant 1 red, variable orange"
      Input = lShape; RefNormal = ValueNone; ShowMoves = false
      Variants = [ v "constant" red (Polyline3D.offset 1.0 0.0 lShape)
                   v "variable" orange (Polyline3D.offsetVariable (ds [0.5; 1.0; 1.5; 0.75; 1.25; 0.25]) (zeros 6) lShape) ] }

    let twisted = pl [Pnt(0.0, 10.0, 0.0); Pnt(0.0, 0.0, 0.0); Pnt(5.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 5.0, 10.0)]
    { Title = "Collinear pt, corners in 2 planes\nconstant red = variable orange"
      Input = twisted; RefNormal = ValueSome z; ShowMoves = true
      Variants = [ v "constant" red (Polyline3D.offsetByNormal z 1.0 1.0 twisted)
                   v "variable" orange (Polyline3D.offsetVariableByNormal z (ds [1.0; 1.0; 1.0; 1.0]) (ds [1.0; 1.0; 1.0; 1.0; 1.0]) twisted) ] }

    let mixed = pl [Pnt(0.0, 0.0, 0.0); Pnt(2.0, 0.0, 0.0); Pnt(5.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 10.0, 0.0); Pnt(0.0, 10.0, 0.0); Pnt(0.0, 0.0, 0.0)]
    let mixedWith b = Polyline3D.offsetVariableByNormalCustom(mixed, ds [1.0; 1.0; 2.0; 1.0; 1.0; 1.0], zeros 6, z, varDistParallelBehavior = b)
    { Title = "Split edge, distances 1 1 2\nProportional red, Project blue,\nSkip green"
      Input = mixed; RefNormal = ValueSome z; ShowMoves = false
      Variants = [ v "Proportional" red (mixedWith Offset3D.VarDistParallel.Proportional)
                   v "Project" blue (mixedWith Offset3D.VarDistParallel.Project)
                   v "Skip" green (mixedWith Offset3D.VarDistParallel.Skip) ] }

    let arc = pl [ for i in 0 .. 45 -> let a = rad (2.0 * float i) in Pnt(10.0 * cos a, 10.0 * sin a, 0.0) ]
    let arcWith step b = Polyline3D.offsetVariableByNormalCustom(arc, ds [ for i in 0 .. 44 -> 1.0 + step * float i ], zeros 46, z, varDistParallelBehavior = b)
    { Title = "Arc 2° per pt, distance 1 to 1.44\nProportional red, Project blue"
      Input = arc; RefNormal = ValueSome z; ShowMoves = false
      Variants = [ v "Proportional" red (arcWith 0.01 Offset3D.VarDistParallel.Proportional); v "Project" blue (arcWith 0.01 Offset3D.VarDistParallel.Project) ] }
    { Title = "Arc 2° per pt, distance 1 to 2.76\ntoo steep: folds. Proportional red,\nProject blue"
      Input = arc; RefNormal = ValueSome z; ShowMoves = false
      Variants = [ v "Proportional" red (arcWith 0.04 Offset3D.VarDistParallel.Proportional); v "Project" blue (arcWith 0.04 Offset3D.VarDistParallel.Project) ] }

    let tear = pl [ yield Pnt(20.0, 0.0, 0.0)
                    for i in 0 .. 120 -> let a = rad (60.0 + 2.0 * float i) in Pnt(10.0 * cos a, 10.0 * sin a, 0.0)
                    yield Pnt(20.0, 0.0, 0.0) ]
    let n = tear.SegmentCount
    { Title = "Teardrop, distance 1 to 1.6\nProportional red"
      Input = tear; RefNormal = ValueSome z; ShowMoves = false
      Variants = [ v "Proportional" red (Polyline3D.offsetVariableByNormalCustom(tear, ds [ for i in 0 .. n - 1 -> 1.0 + 0.005 * float i ], zeros n, z, varDistParallelBehavior = Offset3D.VarDistParallel.Proportional)) ] }

    let skewQuad = pl [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 3.0); Pnt(10.0, 10.0, 0.0); Pnt(0.0, 10.0, 3.0); Pnt(0.0, 0.0, 0.0)]
    { Title = "Non-planar quad\nin-plane 1, perp 0 1 2 3 red"
      Input = skewQuad; RefNormal = ValueSome z; ShowMoves = true
      Variants = [ v "variable" red (Polyline3D.offsetVariableByNormal z (ds [1.0; 1.0; 1.0; 1.0]) (ds [0.0; 1.0; 2.0; 3.0]) skewQuad) ] }
    ]

let pr, pj, sk = Offset3D.VarDistParallel.Proportional, Offset3D.VarDistParallel.Project, Offset3D.VarDistParallel.Skip
let varBy (n: UnitVec) b (ins: float list) (p: Polyline3D) =
    // one perpendicular distance per point, a closed polyline repeats its first point
    Polyline3D.offsetVariableByNormalCustom(p, ds ins, zeros (if p.IsClosed then p.SegmentCount else p.PointCount), n, varDistParallelBehavior = b)
let kinked deg = pl [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0 + 10.0 * cos (rad deg), 10.0 * sin (rad deg), 0.0)]
let spike deg = pl [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0 - 10.0 * cos (rad deg), 10.0 * sin (rad deg), 0.0)]

let specialCases = [
    let runs = pl [Pnt(0.0, 0.0, 0.0); Pnt(3.0, 0.0, 0.0); Pnt(6.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 8.0, 0.0); Pnt(10.0, 8.0, 4.0); Pnt(10.0, 8.0, 8.0)]
    { Title = "Open, straight runs\nat both ends\nin-plane 1.5 red, perp 1 green"
      Input = runs; RefNormal = ValueSome z; ShowMoves = true
      Variants = [ v "in-plane" red (Polyline3D.offsetByNormal z 1.5 0.0 runs); v "perp" green (Polyline3D.offsetByNormal z 0.0 1.0 runs) ] }

    let ends = pl [Pnt(0.0, 0.0, 0.0); Pnt(5.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 6.0, 0.0); Pnt(10.0, 12.0, 0.0)]
    let endsD = [2.0; 1.0; 1.0; 2.0]
    { Title = "Open, straight ends\ndistances 2 1 1 2\nProportional red,\nProject blue, Skip green"
      Input = ends; RefNormal = ValueSome z; ShowMoves = false
      Variants = [ v "Proportional" red (varBy z pr endsD ends); v "Project" blue (varBy z pj endsD ends); v "Skip" green (varBy z sk endsD ends) ] }

    let gap = pl [Pnt(4.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 8.0, 0.0); Pnt(0.0, 8.0, 0.0); Pnt(0.0, 0.0, 0.0); Pnt(2.0, 0.0, 0.0)]
    { Title = "Open, ends on one line, gap\naverage normal\n+1 red, -1 blue"
      Input = gap; RefNormal = ValueNone; ShowMoves = true
      Variants = [ tryV "+1" red (fun () -> Polyline3D.offset 1.0 0.0 gap); tryV "-1" blue (fun () -> Polyline3D.offset -1.0 0.0 gap) ] }

    let overlap = pl [Pnt(4.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 8.0, 0.0); Pnt(0.0, 8.0, 0.0); Pnt(0.0, 0.0, 0.0); Pnt(7.0, 0.0, 0.0)]
    { Title = "Open, end overlaps start\naverage normal\n+1 red, -1 blue"
      Input = overlap; RefNormal = ValueNone; ShowMoves = true
      Variants = [ tryV "+1" red (fun () -> Polyline3D.offset 1.0 0.0 overlap); tryV "-1" blue (fun () -> Polyline3D.offset -1.0 0.0 overlap) ] }

    let gap3D = pl [Pnt(4.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 8.0, 4.0); Pnt(0.0, 8.0, 4.0); Pnt(0.0, 0.0, 0.0); Pnt(2.0, 0.0, 0.0)]
    { Title = "Open, ends on one line,\nnon-planar\nin-plane 1 red, perp 1 green"
      Input = gap3D; RefNormal = ValueSome z; ShowMoves = true
      Variants = [ v "in-plane" red (Polyline3D.offsetByNormal z 1.0 0.0 gap3D); v "perp" green (Polyline3D.offsetByNormal z 0.0 1.0 gap3D) ] }

    let midStart = pl [Pnt(5.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 10.0, 0.0); Pnt(0.0, 10.0, 0.0); Pnt(0.0, 0.0, 0.0); Pnt(5.0, 0.0, 0.0)]
    let midD = [1.0; 1.0; 1.0; 1.0; 2.0]
    { Title = "Closed, starts mid-edge\ndistances 1 1 1 1 2\nProportional red,\nProject blue, Skip green"
      Input = midStart; RefNormal = ValueSome z; ShowMoves = false
      Variants = [ v "Proportional" red (varBy z pr midD midStart); v "Project" blue (varBy z pj midD midStart); v "Skip" green (varBy z sk midD midStart) ] }

    let line = pl [Pnt(0.0, 0.0, 0.0); Pnt(5.0, 0.0, 0.0); Pnt(12.0, 0.0, 0.0); Pnt(20.0, 0.0, 0.0)]
    let tilted = UnitVec.create 0.0 (-1.0) 1.0
    { Title = "Straight line, no corners\nin-plane 1 red, perp 1 green,\nin-plane 1 by 45° normal blue"
      Input = line; RefNormal = ValueSome z; ShowMoves = true
      Variants = [ v "in-plane" red (Polyline3D.offsetByNormal z 1.0 0.0 line); v "perp" green (Polyline3D.offsetByNormal z 0.0 1.0 line)
                   v "tilted normal" blue (Polyline3D.offsetByNormal tilted 1.0 0.0 line) ] }

    let line4 = pl [Pnt(0.0, 0.0, 0.0); Pnt(5.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(15.0, 0.0, 0.0); Pnt(20.0, 0.0, 0.0)]
    let lineD = [1.0; 2.0; 3.0; 4.0]
    { Title = "Straight line\ndistances 1 2 3 4\nProportional red,\nProject blue, Skip green"
      Input = line4; RefNormal = ValueSome z; ShowMoves = false
      Variants = [ v "Proportional" red (varBy z pr lineD line4); v "Project" blue (varBy z pj lineD line4); v "Skip" green (varBy z sk lineD line4) ] }

    let diag = pl [Pnt(0.0, 0.0, 0.0); Pnt(4.0, 3.0, 2.0); Pnt(8.0, 6.0, 4.0); Pnt(12.0, 9.0, 6.0)]
    { Title = "Straight 3D diagonal line\nin-plane 1 red, perp 1 green"
      Input = diag; RefNormal = ValueSome z; ShowMoves = true
      Variants = [ v "in-plane" red (Polyline3D.offsetByNormal z 1.0 0.0 diag); v "perp" green (Polyline3D.offsetByNormal z 0.0 1.0 diag) ] }

    let seg = pl [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 4.0, 3.0)]
    { Title = "Single segment\nby normal: in-plane 1\n+ perp 1 red\naverage normal: blue"
      Input = seg; RefNormal = ValueSome z; ShowMoves = true
      Variants = [ v "by normal" red (Polyline3D.offsetByNormal z 1.0 1.0 seg); tryV "average normal" blue (fun () -> Polyline3D.offset 1.0 1.0 seg) ] }

    let along = pl [Pnt(0.0, 0.0, 0.0); Pnt(0.0, 0.0, 5.0); Pnt(0.0, 0.0, 10.0)]
    { Title = "Vertical line,\nrefNormal along it"
      Input = along; RefNormal = ValueSome z; ShowMoves = false
      Variants = [ tryV "in-plane" red (fun () -> Polyline3D.offsetByNormal z 1.0 0.0 along) ] }

    let lFlat = pl [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(10.0, 8.0, 0.0); Pnt(16.0, 8.0, 0.0)]
    { Title = "Planar in XY, refNormal X\n(perpendicular to all corners)"
      Input = lFlat; RefNormal = ValueSome UnitVec.XAxis; ShowMoves = false
      Variants = [ tryV "in-plane" red (fun () -> Polyline3D.offsetByNormal UnitVec.XAxis 1.0 0.0 lFlat) ] }

    let s174 = spike 6.0
    { Title = "Spike, 174° turn\nin-plane +0.3 red, -0.3 blue"
      Input = s174; RefNormal = ValueSome z; ShowMoves = true
      Variants = [ tryV "+0.3" red (fun () -> Polyline3D.offsetByNormal z 0.3 0.0 s174); tryV "-0.3" blue (fun () -> Polyline3D.offsetByNormal z -0.3 0.0 s174) ] }

    let s176 = spike 4.0
    { Title = "Spike, 176° turn\n(fails above 175° by default)"
      Input = s176; RefNormal = ValueSome z; ShowMoves = false
      Variants = [ tryV "+0.3" red (fun () -> Polyline3D.offsetByNormal z 0.3 0.0 s176) ] }

    let back = pl [Pnt(0.0, 0.0, 0.0); Pnt(10.0, 0.0, 0.0); Pnt(4.0, 0.0, 0.0)]
    { Title = "Straight back on itself, 180°"
      Input = back; RefNormal = ValueSome z; ShowMoves = false
      Variants = [ tryV "+1" red (fun () -> Polyline3D.offsetByNormal z 1.0 0.0 back) ] }

    let dup = pl [Pnt(0.0, 0.0, 0.0); Pnt(8.0, 0.0, 0.0); Pnt(8.0, 0.0, 0.0); Pnt(14.0, 6.0, 0.0)]
    { Title = "Duplicate point"
      Input = dup; RefNormal = ValueSome z; ShowMoves = false
      Variants = [ tryV "+1" red (fun () -> Polyline3D.offsetByNormal z 1.0 0.0 dup) ] }

    let k24 = kinked 2.4
    { Title = "Kink 2.4°, distances 1 then 1.5\nbelow 2.5°: collinear\nProportional red, Project blue"
      Input = k24; RefNormal = ValueSome z; ShowMoves = true
      Variants = [ tryV "Proportional" red (fun () -> varBy z pr [1.0; 1.5] k24); tryV "Project" blue (fun () -> varBy z pj [1.0; 1.5] k24) ] }

    let k26 = kinked 2.6
    { Title = "Kink 2.6°, distances 1 then 1.5\nabove 2.5°: corner, the offset lines\nmeet behind the start: folds, red"
      Input = k26; RefNormal = ValueSome z; ShowMoves = true
      Variants = [ tryV "Proportional" red (fun () -> varBy z pr [1.0; 1.5] k26) ] }
    ]

// layers
let layer name (color:Color) =
    let full = "Offset3D::" + name
    if not (rs.IsLayer full) then rs.AddLayer(full, color) |> ignore
    full
let lInput  = layer "Input" Color.Black
let lOffset = layer "Offsets" Color.Gray
let lMoves  = layer "Moves" Color.DarkGray
let lNormal = layer "RefNormal" Color.SlateGray
let lLabels = layer "Labels" Color.Black
let lVerts  = layer "Vertices" Color.Black

// remove the objects of an earlier run, so the script can run again
for id in rs.AllObjects() do
    if (rs.ObjectLayer id).StartsWith "Offset3D" then rs.DeleteObject id |> ignore

rs.DisableRedraw()
let gap = 12.0       // between the cases of a row
let minCell = 30.0   // the minimum width of a case, so that the labels do not overlap
let perRow = 7
let rowPitch = 50.0  // between the rows, the labels are below each case
/// Wraps an error message into short lines for a text dot.
let wrap (width: int) (msg: string) =
    let lines = ResizeArray<string>()
    let mutable line = ""
    for w in msg.Split ' ' do
        if line.Length > 0 && line.Length + w.Length + 1 > width then
            lines.Add line
            line <- w
        else
            line <- if line.Length = 0 then w else line + " " + w
    if line.Length > 0 then lines.Add line
    String.Join("\n", Seq.truncate 4 lines) + (if lines.Count > 4 then " ..." else "")

let mutable x = 0.0
let mutable number = 0
let mutable firstRow = 0
for group in [cases; specialCases] do
    for k, c in List.indexed group do
        number <- number + 1
        if k % perRow = 0 then x <- 0.0
        let baseY = -rowPitch * float (firstRow + k / perRow)
        let oks = c.Variants |> List.choose (fun va -> match va.Result with Ok r -> Some (va, r) | Error _ -> None)
        let bb = oks |> List.fold (fun (b:BBox) (_, r) -> b.Union r.BoundingBox) c.Input.BoundingBox
        let cell = max bb.SizeX minCell
        let shift = Vec(x + (cell - bb.SizeX) * 0.5 - bb.MinX, baseY - bb.MinY, -bb.MinZ) // centered in its cell, next to each other along X, on the ground plane
        let place (p:Polyline3D) = Polyline3D.move shift p
        let input = place c.Input
        rs.AddPolyline input.RhPolyline |>! rs.setLayer lInput |> ignore
        if input.PointCount <= 30 then
            for i = 0 to input.PointCount - 1 do
                rs.AddPoint(input.GetPoint(i).RhPt) |>! rs.setLayer lVerts |> ignore
        for va, r in oks do
            let res = place r
            let id = rs.AddPolyline res.RhPolyline
            rs.setLayer lOffset id
            rs.ObjectColor(id, va.Color)
            rs.ObjectName(id, $"{number} {va.Label}")
            if c.ShowMoves && res.PointCount = input.PointCount then
                for i = 0 to input.PointCount - 1 do
                    let m = rs.AddLine(input.GetPoint(i).RhPt, res.GetPoint(i).RhPt)
                    rs.setLayer lMoves m
                    rs.ObjectColor(m, va.Color)
        let cx = x + cell * 0.5
        match c.RefNormal with
        | ValueSome n ->
            let o = Pnt(cx + bb.SizeX * 0.5 - 1.0, baseY - 2.0, 0.0)
            let a = rs.AddLine(o.RhPt, (o + n.AsVec * 4.0).RhPt)
            rs.setLayer lNormal a
            rs.CurveArrows(a, 2)
        | ValueNone -> ()
        let errors =
            [ for va in c.Variants do
                match va.Result with
                | Error m ->
                    // drop the 'Euclid.Module.function: ' prefix, the full message is printed at the end of the script
                    let i = m.IndexOf ": "
                    let short = if m.StartsWith "Euclid." && i > 0 then m.Substring(i + 2) else m
                    $"\n{va.Label} THROWS:\n{wrap 34 short}"
                | Ok _ -> () ]
        rs.AddTextDot($"{number}  {c.Title}" + String.concat "" errors, Pnt(cx, baseY - 8.0, 0.0).RhPt) |>! rs.setLayer lLabels |> ignore
        x <- x + cell + gap
    firstRow <- firstRow + (group.Length + perRow - 1) / perRow
rs.EnableRedraw()
// fit the Top and Perspective views to the drawing
let mutable box = Rhino.Geometry.BoundingBox.Empty
for id in rs.AllObjects() do
    if (rs.ObjectLayer id).StartsWith "Offset3D" then box.Union(rs.BoundingBox(id))
box.Inflate 3.0
for view in Rhino.RhinoDoc.ActiveDoc.Views do
    let vp = view.ActiveViewport
    if vp.Name = "Perspective" then
        vp.SetCameraLocations(box.Center, Rhino.Geometry.Point3d(box.Center.X - 60.0, box.Min.Y - 220.0, 160.0)) |> ignore
    if vp.Name = "Top" || vp.Name = "Perspective" then
        vp.ZoomBoundingBox box |> ignore
        view.Redraw()
printfn $"drew {number} cases in rows of {perRow}"
for c in specialCases do
    for va in c.Variants do
        match va.Result with
        | Error m -> printfn $"{c.Title.Split('\n').[0]} / {va.Label}: THROWS {m}"
        | Ok _ -> ()
