#r "C:/Program Files/Rhino 8/System/RhinoCommon.dll"
#r "D:/Git/_Euclid_/Euclid/bin/Release/net6.0/Euclid.dll"
#r "nuget: Rhino.Scripting.FSharp"

open System
open Rhino.Geometry
open Rhino.Scripting
open Rhino.Scripting.FSharp
open Euclid

type rs = RhinoScriptSyntax

let rhPt (p:Pnt) = Point3d(p.X, p.Y, p.Z)

let drawBox (box:BBox) =
    box.BottomPointsLooped
    |> Array.map rhPt
    |> rs.AddPolyline
    |> rs.setLayer "BBox::edges"
    |> ignore

    box.TopPointsLooped
    |> Array.map rhPt
    |> rs.AddPolyline
    |> rs.setLayer "BBox::edges"
    |> ignore

    let corners = box.Points
    for i = 0 to 3 do
        rs.AddLine(rhPt corners.[i], rhPt corners.[i + 4])
        |> rs.setLayer "BBox::edges"
        |> ignore

let drawIntersection index (plane:NPlane) (points:ResizeArray<Pnt>) =
    let planeNumber = index + 1
    let layer = $"BBox::intersection {planeNumber:D2}"

    match points.Count with
    | count when count >= 3 ->
        points
        |> Seq.map rhPt
        |> rs.AddPolylineClosed
        |> rs.setLayer layer
        |> ignore
    | 2 ->
        rs.AddLine(rhPt points.[0], rhPt points.[1])
        |> rs.setLayer layer
        |> ignore
    | 1 ->
        rs.AddPoint(rhPt points.[0])
        |> rs.setLayer layer
        |> ignore
    | _ ->
        printfn $"Plane {planeNumber} does not intersect the box."

    let origin = Pnt(plane.OriginX, plane.OriginY, plane.OriginZ)
    let normalEnd =
        Pnt(
            plane.OriginX + plane.NormalX * 1.5,
            plane.OriginY + plane.NormalY * 1.5,
            plane.OriginZ + plane.NormalZ * 1.5
        )

    rs.AddLine(rhPt origin, rhPt normalEnd)
    |> rs.setLayer "BBox::plane normals"
    |> ignore

    rs.AddTextDot($"{planeNumber}: {points.Count} pts", rhPt origin)
    |> rs.setLayer "BBox::plane labels"
    |> ignore

let random = Random(12345) // fixed seed so every run produces the same planes
let randomRange minValue maxValue =
    minValue + random.NextDouble() * (maxValue - minValue)

let rec randomDirection () =
    let direction =
        Vec(
            randomRange -1.0 1.0,
            randomRange -1.0 1.0,
            randomRange -1.0 1.0
        )
    if direction.LengthSq < 0.01 then randomDirection() else direction

let box = BBox.create(Pnt(-8.0, -5.0, -3.0), Pnt(8.0, 5.0, 3.0))
let planeCount = 4

rs.DisableRedraw()
try
    drawBox box

    for i = 0 to planeCount - 1 do
        // Keeping the plane origin inside the box guarantees an intersection.
        let origin =
            Pnt(
                randomRange (box.MinX * 0.4) (box.MaxX * 0.4),
                randomRange (box.MinY * 0.4) (box.MaxY * 0.4),
                randomRange (box.MinZ * 0.4) (box.MaxZ * 0.4)
            )
        let plane = NPlane.create(origin, randomDirection())
        let intersection = box.IntersectPlane plane

        drawIntersection i plane intersection
        let planeNumber = i + 1
        printfn $"Plane {planeNumber:D2}: {intersection.Count} intersection points"
finally
    rs.EnableRedraw(true)

printfn "Done."
