#r "System.Runtime.Serialization" // auto added
#r "D:/Git/_Euclid_/Euclid/bin/Debug/net472/Euclid.dll"
#r "C:/Program Files/Rhino 8/System/RhinoCommon.dll"
#r "nuget: Rhino.Scripting.FSharp"
#r "nuget: ResizeArrayT"
#r "nuget: Fesher"
open System
open ResizeArrayT
open Rhino
open Rhino.Scripting
open Rhino.Scripting.FSharp
open Euclid
open Fesher
type rs = RhinoScriptSyntax

clearFeshLog()

let pt3 (p:Pt) = Geometry.Point3d(p.X, p.Y, 0.)

let k = ref 0
let ind="    "
let ind0="        "

printfn $"{ind0}// Tests generated via {__SOURCE_DIRECTORY__}/{__SOURCE_FILE__}"
printfn $"{ind0}// and {rs.DocumentPath()}:\n\n"

for c in rs.GetObjects("crvs", printCount=false)  do 
    
    let pl =  c|> rs.CurvePoints |> ResizeArray.map Pt.createFromMembersXY |> Polyline2D
    
    Printfn.gray $"{ind0}let pli = {pl.AsFSharpCode}"
    
    if true  then // markup
        let cen, _ = Polyline2D.findLablePoint 0.2 pl 
        if pl.IsClockwise then   rs.AddTextDot("CW", pt3 cen) |> rs.setLayer "M::Clockwise"
        else  rs.AddTextDot("CCW", pt3 cen) |> rs.setLayer "M::Counter-Clockwise" 
        rs.AddTextDot("0", pt3 pl.Start)|> rs.setLayer "M::Idx"
        rs.AddTextDot("1", pt3 pl.SecondPoint)|> rs.setLayer "M::Idx" 
    
    for dist in [-1.1; 0.95] do 
           
        incr k
        Printfn.orange $"""{ind0}test "offset const dist {dist}-{k.Value}" {{""" 
        
        Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurnBehavior.Chamfer dist pl.Points 
        |> (fun ps -> Printfn.orange $"{ind0}{ind}let plExp = {Polyline2D(ps).AsFSharpCode}" ; ps)
        |> ResizeArray.map pt3
        |> rs.AddPolyline
        |> rs.setLayer $"O::Constant-dist<{dist}>"

        Printfn.orange $"{ind0}{ind}let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurnBehavior.Chamfer {dist} pli.Points |> Polyline2D"
        
        Printfn.orange $"""{ind0}{ind}"offset {k.Value}: dist {dist} on {pl.PointCount} points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)"""
        Printfn.blue $"{ind0}{ind}}}\n"





