#r "C:/Program Files/Rhino 8/System/RhinoCommon.dll"
#I @"D:\Git\_Euclid_\Euclid.Rhino\bin\Release\net7.0\"
#r "Euclid.dll"
#r "Euclid.Rhino.dll"
#r "nuget: Rhino.Scripting.FSharp"
#r "nuget: ResizeArrayT"
#r "nuget: Fesher"
open System
open ResizeArrayT
open Rhino.Scripting
open Rhino.Scripting.FSharp
open Euclid
open Fesher
clearFeshLog()
type rs = RhinoScriptSyntax
rs.DisableRedraw()

open Euclid.Offset2D
open Euclid.EuclidErrors
open Euclid.UtilEuclid

let handleUTurn (pt:Pt, cosine:float, nPrev:UnitVc, nNext:UnitVc, dist:float, res:ResizeArray<Pt>, uTurnBehavior:UTurn, useUTurnBehaviorAbove:float<Cosine.cosine>) =
    match uTurnBehavior with
    | UTurn.Fail ->
        fail $"Offset2D.handleUTurn: pt {pt} makes a %.4f{toDegrees(acos cosine) } degree U-turn, max {toDegrees(acos (float useUTurnBehaviorAbove))} is allowed."

    | UTurn.Chamfer ->
        // (1.2) special case: sharp U-turn, add a chamfer, i.e., two points instead of one
        let chamferTangent = (nPrev - nNext).Unitized
        let chamferNormal = chamferTangent.Rotate90CCW
        let chamferNormalChecked = if chamferNormal *** nPrev >= 0.0 then chamferNormal else -chamferNormal
        let cos = UnitVc.dot (nPrev , chamferNormal)
        res.Add <| setOffCorner pt dist nPrev                chamferNormalChecked cos // the first chamfer point
        res.Add <| setOffCorner pt dist chamferNormalChecked nNext                cos // the second chamfer point

    | UTurn.UseThreshold  ->
        let midV = nPrev.Rotate90CW + nNext.Rotate90CCW //|> Vc.unitize// the offset vector with length of almost 2.0, so use *0.5 below:
        // make the offset point as if the acute angle matches the threshold angle, even if it is more acute
        // make sure the angle is not more acute than 179.9 degrees
        //let minCosine = unbox<float> Cosine.``179.9`` // -0.9999..
        //let clampedCos = max cosine minCosine // use max to get an angle bigger or equal to 179.9 degrees, so that tangent does not reach 0.0
        //let cosHalf = cos (0.5 * (acos (float useUTurnBehaviorAbove)))
        let cosHalf =  sqrt ((1.0 + float useUTurnBehaviorAbove) / 2.0)  // positive root
        res.Add <| pt - midV * (0.5 * dist / cosHalf)
        //printfn $"{midV} dist: {dist} sine: {sine} cosine: {cosine} useUTurnBehaviorAbove: {useUTurnBehaviorAbove}"
        


    | UTurn.Skip ->
        () // do nothing, skip the point

    | x -> // should never happen
        fail $"Offset2D.UTurnBehavior: enum value {x} not recognized."

let offsetWithDirections(pts:ResizeArray<Pt>, dirs: ResizeArray<UnitVc>, dist:float, uTurnBehavior:UTurn, useUTurnBehaviorAbove :float<Cosine.cosine>) : ResizeArray<Pt> = //[<OPT;DEF(Cosine.``179.0``)>]
    if pts.Count < 2 then
        fail $"Offset2D.offsetWithDirections: pts.Count must be at least 2 but is {pts.Count}."

    if pts.Count <> dirs.Count + 1  then
        fail $"Offset2D.offsetWithDirections: pts.Count must be one greater than normals.Count but they are {pts.Count} and {dirs.Count}."

    let res = ResizeArray<Pt>(pts.Count)
    let isOpen = Pt.distanceSq pts.First pts.Last > sqOpenTolerance
    let mutable fromIdx = 0
    let mutable nPrev = dirs.[dirs.Count-1]
    if isOpen then // this is needed for open polylines that have 180 degree opposing start and end segments
        fromIdx <- 1
        nPrev <- dirs.[0]
        res.Add <|  pts.[0] + nPrev * dist

    for i = fromIdx to pts.Count - 2 do // last point is skipped; it's dealt with at the end
        let nNext = dirs.[i] // the normal vector for the segment from pt to next point
        let pt = pts.[i]
        let cosine = UnitVc.dot (nPrev , nNext)
        if withMeasure cosine > useUTurnBehaviorAbove then // exclude U-turns
            // (1.1) the regular case:
            res.Add <| setOffCorner pt dist nPrev nNext cosine
        else
            handleUTurn (pt, cosine, nPrev, nNext, dist, res, uTurnBehavior, useUTurnBehaviorAbove)
        nPrev <- nNext

    // if the polyline is open, then we need to fix first and last point
    if isOpen then
        res.Add <| pts.Last + dirs.Last * dist
    else
        res.Add res.[0] // add the last point, which is the same as the first point
    res
 
type internal OPT = Runtime.InteropServices.OptionalAttribute
type internal DEF = Runtime.InteropServices.DefaultParameterValueAttribute
   
type Poly =
    static member offset(   polyLine:Polyline2D,
                            constantOffsetDistance: float,
                            [<OPT;DEF(false)>] loop:bool,
                            [<OPT;DEF(true)>] checkOrientation:bool,
                            [<OPT;DEF(Offset2D.UTurn.Fail)>] uTurnBehavior: Offset2D.UTurn,
                            [<OPT;DEF(Cosine.``175.0``)>] useUTurnBehaviorAbove: float<Cosine.cosine>
                            ) : Polyline2D =
        let pts = polyLine.Points
        if pts.Count < 2 then
            fail $"Polyline2D.offset: Polyline2D must have at least 2 points but has {pts.Count} points. {polyLine}"

        let constantOffsetDistance =
            if checkOrientation && polyLine.SignedArea < 0.0 then
                constantOffsetDistance * -1.0
            else
                constantOffsetDistance

        // check if looping desired and curve is open
        if loop && Pt.distanceSq pts.First pts.Last > Offset2D.sqOpenTolerance then
            let closedPts = pts
            pts.Add pts.First
            let normals = Offset2D.makeOffsetDirections closedPts
            let res  = offsetWithDirections(closedPts, normals, constantOffsetDistance,  uTurnBehavior, useUTurnBehaviorAbove)
            res.Pop() |> ignore // remove last point to open the polyline again
            Polyline2D.createDirectlyUnsafe res
        else
            let normals = Offset2D.makeOffsetDirections pts
            offsetWithDirections(pts , normals, constantOffsetDistance, uTurnBehavior, useUTurnBehaviorAbove)
            |> Polyline2D.createDirectlyUnsafe



let b = Pt(0, 10)
for i=0 to 25 do  
    let x = float i
    let x = 0.1 + x * 0.3
    let a = Pt(x, 0) 
    let c = Pt(-x, 0)
    let pl = Polyline2D.create [Pt.divPt(b, a, 9); b; Pt.divPt(b, c, 9) ]
    //pl.RhPolylineCurve |> rs.Ot.Add |> rs.setLayer $"pl"    
    
    //let o = Polyline2D.offset(pl, -0.22, uTurnBehavior=Offset2D.UTurn.UseThreshold, useUTurnBehaviorAbove=Cosine.``160.0``)
    //o.RhPolylineCurve |> rs.Ot.Add |> rs.setLayer $"o::{i}"
    
    //let ov = Polyline2D.offsetVar(pl, [| -0.2;  -0.3 |], uTurnBehavior=Offset2D.UTurn.UseThreshold, useUTurnBehaviorAbove=Cosine.``160.0``)
    //ov.RhPolylineCurve |> rs.Ot.Add |> rs.setLayer $"ov::{i}"
    
    let oc = Polyline2D.offsetVar(pl, [| -0.2;  -0.3 |], uTurnBehavior=Offset2D.UTurn.Chamfer, useUTurnBehaviorAbove=Cosine.``170.0``)
    oc.RhPolylineCurve |> rs.Ot.Add |> rs.setLayer $"ov::{i}"