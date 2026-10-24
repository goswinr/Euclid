#r "C:/Program Files/Rhino 8/System/RhinoCommon.dll" 
#r "C:/Users/ROGO/.nuget/packages/rhino.scripting/0.14.0/lib/net8.0/Rhino.Scripting.dll"
#r "C:/Users/ROGO/.nuget/packages/rhino.scripting.fsharp/0.14.0/lib/net7.0/Rhino.Scripting.FSharp.dll"
#I "D:/Git/_Euclid_/Euclid.Rhino/bin/Debug/net8.0/"
#r "Euclid"
#r "Euclid.Rhino"

open System
open Euclid
open Rhino.Scripting
open Rhino.Scripting.FSharp


type rs = RhinoScriptSyntax


for g in rs.GetObjectsAndRemember "crvs" do
    let p = g|> rs.CoercePolyline |> Polyline2D.ofRhPolyline
    
    for d = -7 to -7 do 
        p
        //|> fun p -> printfn $"let input = {p.AsFSharpCode}" ; p
        |> Polyline2D.removeUTurnsDeeply Cosine.``170.0``
        //|> fun p -> printfn $"let expected = {p.AsFSharpCode}" ; p
        //|> Polyline2D.offset' (2.0 * float d)
        //|> fun p -> Polyline2D.offset(p,  2.0 * float d, uTurnBehavior = Offset2D.UTurn.Chamfer)
        |> Polyline2D.toRhPolylineCurve
        |> rs.Ot.AddCurve
        |> rs.setLayer $"3{d}"
        












