#r "C:/Program Files/Rhino 8/System/RhinoCommon.dll" 
#r "nuget: Rhino.Scripting.FSharp"  
#r "nuget: ResizeArray"

open System
open ResizeArray
open Rhino.Geometry
open Rhino.Scripting
open Rhino.Scripting.FSharp 

type rs = RhinoScriptSyntax


#r "D:/Git/_Euclid_/Euclid/bin/Release/net6.0/Euclid.dll"
open Euclid



let o = Pnt(0.0, 0.0, 0.0)
let x = Pnt(1.0, 0.0, 0.0)
let y = Pnt(0.0, 1.0, 0.0)
let r = Rect3D.createFrom3Points(o, x , y)
let l = Line3D(x |> Pnt.moveZ 0.1, y|> Pnt.moveZ -0.1)


let xx = Rect3D.intersectRay l r

printfn "Intersection: %A" xx

let rpt(p:Pnt)= Point3d(p.X, p.Y, p.Z)

r   
|> Rect3D.pointsLooped
|> Array.map rpt
|> rs.AddPolyline
|> ignore 

rs.AddLine(rpt l.From, rpt l.To) 
|> ignore 








