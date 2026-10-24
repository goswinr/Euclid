#r "C:/Program Files/Rhino 8/System/RhinoCommon.dll"

#r "D:/Git/_Euclid_/Euclid/bin/Release/net6.0/Euclid.dll"
#r "nuget: Rhino.Scripting.FSharp"
#r "nuget: ResizeArrayT"
#r "nuget: Fesher"

open System
open ResizeArrayT
open Rhino.Scripting
open Rhino.Scripting.FSharp
open Euclid
open Euclid
open Fesher


type rs = RhinoScriptSyntax
module R = ResizeArray

clearFeshLog()
let pnt(p:Rhino.Geometry.Point3d) = Pnt(p.X, p.Y, p.Z)
let rpt (p:Pnt) = Rhino.Geometry.Point3d(p.X, p.Y, p.Z)


let pts =
    rs.GetObject("Select Tria", rs.Filter.Surface)
    |> rs.SurfacePoints
    |> Polyline3D.createFromXYZMembers
    |> Polyline3D.removeDuplicatePoints 0.001
    |> _.AsPoints


// rs.AddPolyline [rpt pts[0]; rpt pts[1]; rpt pts[2]; rpt pts[0]] |> rs.setLayer "tria"
//
let prLn (ln:Line3D) (a:Pnt) (b:Pnt) (c:Pnt) =
    printfn $"let a = {a.AsFSharpCode}"
    printfn $"let b = {b.AsFSharpCode}"
    printfn $"let c = {c.AsFSharpCode}"
    printfn $"let ln = {ln.AsFSharpCode}"


let someToo lo (ln:Line3D) (a:Pnt) (b:Pnt) (c:Pnt) =
    if (Tria3D.intersectLine (ln, a, b, c)).IsNone then
        rs.SelectObject lo
        // edge1 = B - A,  edge2 = C - A
        rs.AddLine(rpt ln.From, rpt ln.To) |> rs.setLayer "Unexpected No Intersection"
        try rs.AddLine(rpt a, rpt b) |> rs.setLayer "Unexpected No Intersection" with _ -> ()
        try rs.AddLine(rpt c, rpt a) |> rs.setLayer "Unexpected No Intersection" with _ -> ()
        eprintfn "//Line does not intersect triangle:"
        prLn ln a b c
        printfn "Tria3D.intersectLine (ln, a, b, c) |> printfn \" %%A \" "

let noneToo lo (ln:Line3D) (a:Pnt) (b:Pnt) (c:Pnt) =
    if (Tria3D.intersectLine (ln, a, b, c)).IsSome then
        rs.SelectObject lo
        eprintfn "Line intersects triangle !!!!!"

let a = pts[0]
let b = pts[1]
let c = pts[2] // or pts[1] to test degenerate triangle !!
printfn $"let triaPt_a = {a.AsFSharpCode}"
printfn $"let triaPt_b = {b.AsFSharpCode}"
printfn $"let triaPt_c = {c.AsFSharpCode}"

for i, lo in rs.GetObjects("Select Lines", rs.Filter.Curve) |> Seq.indexed do
    if rs.IsLine(lo) then
        let s, e = rs.CurveStartPoint(lo), rs.CurveEndPoint(lo)
        let l = Line3D (pnt s, pnt e)
        
        let p = Tria3D.intersectLine (l, a, b, c)
        match p with
        | ValueSome p ->
            printfn $"let ln{i}_intersectiong = {l.AsFSharpCode}"
            let p = rpt p
            rs.AddPoint(p) |> rs.setLayer "Intersection Pt"
            rs.setLayer "Intersection" lo
            // Test peremutations of pts:
            someToo lo l b c a
            someToo lo l c a b
            someToo lo l a c b
            someToo lo l b a c
            someToo lo l c b a


        | ValueNone ->
            printfn $"let ln{i}_notIntersectiong = {l.AsFSharpCode}"
            lo |> rs.setLayer "No Intersection"
            // Test peremutations of pts:
            noneToo lo l b c a
            noneToo lo l c a b
            noneToo lo l a c b
            noneToo lo l b a c
            noneToo lo l c b a






