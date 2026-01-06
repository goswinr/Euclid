#r "C:/Program Files/Rhino 8/System/RhinoCommon.dll" 
#r "nuget:Rhino.Scripting.FSharp" 
#r "nuget:Euclid.Rhino"

open Euclid
open System
open Rhino.Scripting
open Rhino.Scripting.FSharp 

type rs = RhinoScriptSyntax

#load "D:\Git\_Euclid_\Euclid\Tests\GeneratedOffset3DTestData.fsx"


rs.DisableRedraw()

let mutable x = 0.0
for d in GeneratedOffset3DTestData.offset3DTestCases do 
    let bb = d.Output.BoundingBox.Union d.Input.BoundingBox
    x <- x - bb.MinX
    
    let c = bb.Center |> Pnt.moveX x |> Pnt.toRhPt
    
    rs.Ot.Add d.Input.RhPolylineCurve  |>!  rs.moveX x |> rs.setLayer "Input"
    rs.Ot.Add d.Output.RhPolylineCurve |>!  rs.moveX x |> rs.setLayer "Output"
    rs.AddTextDot($"d{d.InPlane}  p{d.Perp}", c) |> rs.setLayer "dist"
    rs.AddLine(c, c + d.RefNormal.RhVec * 2.0)|> rs.setLayer "ref normal"
    
    x <- x + bb.SizeX * 1.1 + 0.5
    
    
    
    
    