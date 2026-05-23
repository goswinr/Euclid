#r "C:/Program Files/Rhino 8/System/RhinoCommon.dll"
#r "System.Runtime.Serialization" // auto added
#r "D:/Git/_Euclid_/Euclid/bin/Debug/net472/Euclid.dll"
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
open Euclid.AutoOpenLine2D

type rs = RhinoScriptSyntax
clearFeshLog()


let pt3 (p:Pt) = Geometry.Point3d(p.X, p.Y, 0.)
let ln (l:Geometry.Line) = Line2D(l.FromX, l.FromY, l.ToX,  l.ToY)


let listMembers() =
        let t = typeof<Line2D>
        printfn "Members of Line2D:"
        t.GetMembers()
        |> Array.iter (fun m -> printfn "  %s: %s" m.Name (m.MemberType.ToString()))

listMembers()
let euclidAssembly = typeof<Line2D>.Assembly
printfn "Euclid Assembly Location: %s" euclidAssembly.Location
printfn "Euclid Assembly Full Version: %s" (euclidAssembly.FullName)

let rec go() =

        let a = rs.GetObject("Line 1", filter=rs.Filter.Curve)|> rs.CoerceLine |> ln
        let b = rs.GetObject("Line 2", filter=rs.Filter.Curve)|> rs.CoerceLine |> ln


        let pts = Line2D.divide 3 a

        printfn "%A" pts



        //match Line2D.tryIntersectOrOverlap a b with
        //|Some p ->  rs.AddTextDot("X", pt3 p)|> rs.setLayer "X"
        //|None -> rs.AddTextDot("n", pt3 (Pt.midPt a.Mid b.Mid))|> rs.setLayer "no X"

        //rs.AddTextDot("n", pt3 (Pt.midPt a.Mid b.Mid))|> rs.setLayer "no X"
        //rs.AddTextDot("1", pt3 a.Mid)|> rs.setLayer "1"
        //rs.AddTextDot("2", pt3 b.Mid)|> rs.setLayer "2"
        //rs.AddTextDot("1f", pt3 a.From)|> rs.setLayer "1f"
        //rs.AddTextDot("2f", pt3 b.From)|> rs.setLayer "2f"
        //rs.AddTextDot("1t", pt3 a.To)|> rs.setLayer "1f"
        //rs.AddTextDot("2t", pt3 b.To)|> rs.setLayer "2f"
        go()

go()

()