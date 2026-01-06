#r "D:/Git/_Euclid_/Euclid/bin/Debug/net6.0/Euclid.dll"
#r "C:/Program Files/Rhino 8/System/RhinoCommon.dll"
#r "nuget: Rhino.Scripting.FSharp"
#r "nuget: ResizeArrayT"
#r "nuget: Fesher"
open System
open ResizeArrayT
open Rhino.Scripting
open Rhino.Scripting.FSharp
open Euclid
open Fesher
type rs = RhinoScriptSyntax



// test on file 
// D:\Git\_Euclid_\Euclid\Tests\TestInRhino\3D\offset3D.3dm.

clearFeshLog()

let pt3 (p:Pnt) = Rhino.Geometry.Point3d(p.X, p.Y, p.Z)

let k = ref 0
let ind="    "
let ind0="        "

printfn $"{ind0}// Tests generated via {__SOURCE_DIRECTORY__}/{__SOURCE_FILE__}"
printfn $"{ind0}// and {rs.DocumentPath()}:\n\n"




rs.DisableRedraw()
for c in rs.GetObjects("crvs", printCount=false)  do 
    
    let pl =  c|> rs.CurvePoints |> ResizeArray.map Pnt.createFromMembersXYZ |> Polyline3D.create
    
    Printfn.gray $"{ind0}let pli = {pl.AsFSharpCode}"
    
    //if true  then // markup
        //let cen, _ = Polyline2D.findLablePoint 0.2 pl 
        //if pl.IsClockwise then   rs.AddTextDot("CW", pt3 cen) |> rs.setLayer "M::Clockwise"
        //else  rs.AddTextDot("CCW", pt3 cen) |> rs.setLayer "M::Counter-Clockwise" 
        //rs.AddTextDot("0", pt3 pl.Start)|> rs.setLayer "M::Idx"
        //rs.AddTextDot("1", pt3 pl.SecondPoint)|> rs.setLayer "M::Idx" 
    
    for dist in [-1.1; 0.9] do 
           
        incr k
        Printfn.orange $"""{ind0}test "offset const dist {dist}-{k.Value}" {{""" 
        
        Polyline3D.offset(pl, dist, dist*0.5,  UnitVec.Zaxis ) 
        |> (fun ps -> Printfn.orange $"{ind0}{ind}let plExp = {ps.AsFSharpCode}" ; ps.Points)
        |> ResizeArray.map pt3
        |> rs.AddPolyline
        |> rs.setLayer $"O::Constant-dist<{dist}>"

        Printfn.orange $"{ind0}{ind}let plr = Offset2D.offset'' Cosine.``170.0`` Offset2D.UTurnBehavior.Chamfer {dist} pli.Points |> Polyline2D"
        
        Printfn.orange $"""{ind0}{ind}"offset {k.Value}: dist {dist} on {pl.PointCount} points" |> Expect.isTrue (Polyline2D.equals 0.0001 plr plExp)"""
        Printfn.blue $"{ind0}{ind}}}\n"





