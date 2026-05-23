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

let pr (vs:float seq) =  
    let sc = ";"
    $"(ResizeArray[| {vs |> Seq.map string |> String.concat sc} |]) "

for c in rs.GetObjects("crvs", printCount=false)  do 
    
    let pli =  c|> rs.CurvePoints |> ResizeArray.map Pt.createFromMembersXY |> Polyline2D
    
    Printfn.gray $"{ind0}let pli = {pli.AsFSharpCode}"
    
    if true  then // markup
        let cen, _ = Polyline2D.findLablePoint 0.2 pli 
        if pli.IsClockwise then   rs.AddTextDot("CW", pt3 cen) |> rs.setLayer "M::Clockwise"
        else  rs.AddTextDot("CCW", pt3 cen) |> rs.setLayer "M::Counter-Clockwise" 
        rs.AddTextDot("0", pt3 pli.Start)|> rs.setLayer "M::Idx"
        rs.AddTextDot("1", pt3 pli.SecondPoint)|> rs.setLayer "M::Idx" 
    
    for dist in [-1.2; 0.9] do 
        for vh in [ 
            Offset2D.VarDistParallelBehavior.Skip
            Offset2D.VarDistParallelBehavior.Project
            Offset2D.VarDistParallelBehavior.Proportional
            Offset2D.VarDistParallelBehavior.StepWithTwoPoints 
            ] do 
                incr k 
                
                // as if looped:  
                let offs = resizeArray{  
                        for i=0 to pli.Points.LastIndex do  // same as points
                            if i%2=0 then dist else dist * 0.5  } 
                Printfn.orange $"""{ind0}test "loop VarDistParallelBehavior.{vh}-{k.Value}-dist:{dist}" {{""" 
                Polyline2D.offset(pli, offs, loop=true, varDistParallelBehavior = vh, uTurnBehavior = Offset2D.UTurnBehavior.Chamfer)
                |> (fun ps -> Printfn.orange $"{ind0}{ind}let plExp = {ps.AsFSharpCode}" ; ps)
                |> _.Points
                |> ResizeArray.map pt3
                |> rs.AddPolyline
                |> rs.setLayer $"O-loop::{vh}::Var< {dist} >" 
                Printfn.orange $"{ind0}{ind}let plr = Polyline2D.offset(pli, {pr offs}, loop=true, varDistParallelBehavior = {vh},  uTurnBehavior = Offset2D.UTurnBehavior.Chamfer)" 
                Printfn.orange $"""{ind0}{ind}"loop offset {k.Value} VarDistParallelBehavior.{vh}: dist {dist} on {pli.PointCount} points" |> Expect.isTrue (Polyline2D.equals 1e-6 plr plExp)"""
                Printfn.orange $"{ind0}{ind}}}\n"


                // as if open:  
                let offs = resizeArray{  
                        for i=0 to pli.Points.LastIndex-1 do  //one less
                            if i%2=0 then dist else dist * 0.5  } 
                Printfn.orange $"""{ind0}test "open VarDistParallelBehavior.{vh}-{k.Value}-dist:{dist}" {{""" 
                Polyline2D.offset(pli, offs, varDistParallelBehavior = vh, uTurnBehavior = Offset2D.UTurnBehavior.Chamfer)
                |> (fun ps -> Printfn.orange $"{ind0}{ind}let plExp = {ps.AsFSharpCode}" ; ps)
                |> _.Points
                |> ResizeArray.map pt3
                |> rs.AddPolyline
                |> rs.setLayer $"O::{vh}::Var< {dist} >" 
                Printfn.orange $"{ind0}{ind}let plr = Polyline2D.offset(pli, {pr offs}, varDistParallelBehavior = {vh},  uTurnBehavior = Offset2D.UTurnBehavior.Chamfer)" 
                Printfn.orange $"""{ind0}{ind}"open offset {k.Value} VarDistParallelBehavior.{vh}: dist {dist} on {pli.PointCount} points" |> Expect.isTrue (Polyline2D.equals 1e-6 plr plExp)"""
                Printfn.orange $"{ind0}{ind}}}\n"
                
                
                