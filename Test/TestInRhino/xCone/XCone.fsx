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
open Euclid.AutoOpenLine2D

type rs = RhinoScriptSyntax
clearFeshLog()

let coneRadius = rs.GetObject("Select cone base circle", rs.Filter.Curve) |> rs.CircleRadius
let coneBaseZ = 0.0 //rs.GetPoint("Select cone base plane") |> fun pt -> pt.Z
let coneTipZ = rs.GetPoint("Select cone tip plane") |> fun pt -> pt.Z

let rpt (p:Pnt) = Rhino.Geometry.Point3d(p.X, p.Y, p.Z)

let xCone (ln:Line3D, g: System.Guid) =
    let result = XLine3D.intersectCone (ln, coneRadius, coneBaseZ, coneTipZ)
    match result with
    | XLine3D.XCone.NoIntersection ->
        rs.AddTextDot("No Intersection", rpt ln.From) |> rs.setLayer "No Intersection"
        g |> rs.setLayer "No Intersection"
    | XLine3D.XCone.Intersecting (t1, t2) ->
        let p1 = ln.EvaluateAt t1
        let p2 = ln.EvaluateAt t2
        rs.AddTextDot("Intersecting", rpt p1) |> rs.setLayer "Intersecting"
        rs.AddTextDot("Intersecting", rpt p2) |> rs.setLayer "Intersecting"
        g |> rs.setLayer "Intersecting"
    | XLine3D.XCone.IntersectingOne t ->
        let p = ln.EvaluateAt t
        rs.AddTextDot("IntersectingOne", rpt p) |> rs.setLayer "IntersectingOne"
        g |> rs.setLayer "IntersectingOne"
    | XLine3D.XCone.Touching t ->
        let p = ln.EvaluateAt t
        rs.AddTextDot("Touching", rpt p) |> rs.setLayer "Touching"
        g |> rs.setLayer "Touching"
    | XLine3D.XCone.ThroughTip t ->
        let p = ln.EvaluateAt t
        rs.AddTextDot("ThroughTip", rpt p) |> rs.setLayer "ThroughTip"
        g |> rs.setLayer "ThroughTip"
    | XLine3D.XCone.LineOnCone (tipParam, onConeTowardsPositiveT) ->
        let p = ln.EvaluateAt tipParam
        let label = sprintf "LineOnCone, tipParam=%.2f, onConeTowardsPositiveT=%b" tipParam onConeTowardsPositiveT
        rs.AddTextDot(label, rpt p) |> rs.setLayer "LineOnCone"
        g |> rs.setLayer "LineOnCone"


for c in rs.GetObjects("Select lines to intersect with cone", rs.Filter.Curve) do
    let st = rs.CurveStartPoint c |> fun pt -> Pnt(pt.X, pt.Y, pt.Z)
    let en = rs.CurveEndPoint c |> fun pt -> Pnt(pt.X, pt.Y, pt.Z)
    let ln = Line3D(st, en )
    xCone (ln, c) |> ignore