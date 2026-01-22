namespace Euclid

open System
open UtilEuclid
open EuclidErrors
open Euclid.EuclidCollectionUtilities

/// The core algorithms for offsetting 2D polylines.
/// Normally you would not use this directly; prefer the Polyline2D or Points module.
module Offset2D=

    /// The squared tolerance for open polylines.
    let [<Literal>] sqOpenTolerance = 1e-12


    /// An Enum for describing what to do at U-turns (180 degree turns).
    /// It is not possible to offset U-turns correctly.
    /// This Enum describes the options:
    /// 1: Fail
    /// 2: Add a chamfer with two points.
    /// 3: Use the offset point for the threshold angle, by default this is 179 degrees.
    /// 4: Just skip the point.
    // [<RequireQualifiedAccess>]
    type UTurn = // needs to be an enum , so it can bve an optional parameter with default value

        /// Fail if at a U-turn of more than the tolerance (179 degrees?) is present in the polyline.
        | Fail = 1

        /// Add two points to create a chamfer at a U-turn above the tolerance.
        | Chamfer = 2

        /// This will create a very long offset at a U-turns.
        /// Makes the offset point as if the acute angel matches the threshold angle, even if it is more acute
        /// by default this is 179 degrees.
        /// This option guarantees the same number of points in the returned Polyline.
        /// (unless StepWithTwoPoints is used for var dist offsets)
        | UseThreshold = 3

        /// Just skip U-turn points.
        /// This will create a polyline with less points than the input that cuts across the U-turn.
        | Skip = 4


    /// An Enum for describing what to do with colinear segments with different offset distances.
    /// It is not possible to offset colinear segments if the distances are not the same.
    /// This Enum describes the options:
    /// 1: Fail
    /// 2: just skip the point.
    /// 3: add a point at closest distance on the oblique segment, non parallel offsets.
    /// 4: add a point perpendicular from here intersecting the oblique segment, non parallel offsets.
    /// 5: add a step with two points.
    // [<RequireQualifiedAccess>]
    type VarDistParallel = // needs to be an enum , so it can bve an optional parameter with default value

        /// Fail if colinear segments with different offset distances are found.
        | Fail = 1

        /// Just skip colinear points with different offset distances.
        /// This introduces non parallel offset segments.
        /// This reduces the total point count.
        | Skip = 2

        /// Add a point at the same proportional distance as the input segments.
        /// This keeps the total point count the same.
        /// This introduces non parallel offset segments.
        /// This keeps the total point count the same.
        | Proportional = 3

        /// Project the offset point onto the oblique segment.
        /// In shallow angles with strongly different distances the projected point may be outside of the input segments.
        /// This introduces non parallel offset segments.
        /// This keeps the total point count the same.
        /// This may create a self-intersection in the result if the projected point is outside of the segment.
        /// The Proportional method is usually better.
        | Project = 4

        /// Add a step with two points at the colinear segment.
        /// This keeps parallel offset segments.
        /// But introduces an extra point.
        | StepWithTwoPoints = 5



    /// Internal only.
    /// So that colinear points can be fixed after the main offsetting loop is done.
    type internal IndexToFixProportional = {
        /// the index of the colinear point in result
        idxRes:int
        /// index of colinear point in input.
        /// usually the same as idxRes, but a 180 uTurn with two points might have create a shifting of indices.
        idxOrig:int
    }

    [<NoComparison; NoEquality>]
    type internal IndexToProject = {
        /// the index of the colinear point in result
        idx:int
        /// the direction to project along
        dir: Vc
    }


    //          █████████                               █████                          █████
    //         ███░░░░░███                             ░░███                          ░░███
    //        ███     ░░░   ██████  ████████    █████  ███████    ██████   ████████   ███████
    //       ░███          ███░░███░░███░░███  ███░░  ░░░███░    ░░░░░███ ░░███░░███ ░░░███░
    //       ░███         ░███ ░███ ░███ ░███ ░░█████   ░███      ███████  ░███ ░███   ░███
    //       ░░███     ███░███ ░███ ░███ ░███  ░░░░███  ░███ ███ ███░░███  ░███ ░███   ░███ ███
    //        ░░█████████ ░░██████  ████ █████ ██████   ░░█████ ░░████████ ████ █████  ░░█████
    //         ░░░░░░░░░   ░░░░░░  ░░░░ ░░░░░ ░░░░░░     ░░░░░   ░░░░░░░░ ░░░░ ░░░░░    ░░░░░
    //
    //
    //
    //                    ██████     ██████                    █████
    //                   ███░░███   ███░░███                  ░░███
    //         ██████   ░███ ░░░   ░███ ░░░   █████   ██████  ███████
    //        ███░░███ ███████    ███████    ███░░   ███░░███░░░███░
    //       ░███ ░███░░░███░    ░░░███░    ░░█████ ░███████   ░███
    //       ░███ ░███  ░███       ░███      ░░░░███░███░░░    ░███ ███
    //       ░░██████   █████      █████     ██████ ░░██████   ░░█████
    //        ░░░░░░   ░░░░░      ░░░░░     ░░░░░░   ░░░░░░     ░░░░░
    //
    //
    //
    //            █████  ███           █████
    //           ░░███  ░░░           ░░███
    //         ███████  ████   █████  ███████    ██████   ████████    ██████   ██████
    //        ███░░███ ░░███  ███░░  ░░░███░    ░░░░░███ ░░███░░███  ███░░███ ███░░███
    //       ░███ ░███  ░███ ░░█████   ░███      ███████  ░███ ░███ ░███ ░░░ ░███████
    //       ░███ ░███  ░███  ░░░░███  ░███ ███ ███░░███  ░███ ░███ ░███  ███░███░░░
    //       ░░████████ █████ ██████   ░░█████ ░░████████ ████ █████░░██████ ░░██████
    //        ░░░░░░░░ ░░░░░ ░░░░░░     ░░░░░   ░░░░░░░░ ░░░░ ░░░░░  ░░░░░░   ░░░░░░



    /// The core offset Algorithm for constant distance offsets.
    /// Returns the offset point based on the previous and next normals,
    /// the distance, and the precomputed cosine (= dot product of nPrev * nNext).
    /// (pt + (nPrev + nNext) * (dist / (1.0 + cosine)))
    let inline setOffCorner (pt:Pt) (dist:float) (nPrev:UnitVc) (nNext:UnitVc) (cosine:float) :Pt =
        #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
            if cosine < -0.9999999 then
                fail $"Offset2D.setOffCorner: cosine is {cosine} , a 180 degree U-turn, which is not allowed here for dist {dist}, nPrev {nPrev}, nNext {nNext} at pt {pt}."
        #endif
            // based on https://www.angusj.com/clipper2/Docs/Trigonometry.htm
            pt + (nPrev + nNext) * (dist / (1.0 + cosine))


    /// Returns the normals of the segments of a polyline; each segment is rotated 90 degrees in counter-clockwise order.
    /// The count is one less than the input points.
    /// Fails on duplicate points.
    let makeOffsetDirections (pts:ResizeArray<Pt>) : ResizeArray<UnitVc> =
        if pts.Count < 2 then
            fail $"Offset2D.makeOffsetDirections: pts.Count {pts.Count} must be at least 2 for a polyline."
        let normals = ResizeArray<UnitVc>(pts.LastIndex) // the normals of the segments, one less than pts
        let mutable pp = pts.[0]
        for i = 1 to pts.LastIndex do
            let p = pts.[i]
            let v = p - pp // the vector from previous to current point
            let len = v.Length
            if len < 1e-6 then fail $"Offset2D.makeOffsetDirections: pts.[{i}] and pts.[{i-1}] are the same at {pp}"
            // TODO: or just skip duplicate points?
            let u = UnitVc.createUnchecked(v.X/len, v.Y/len) // the unit vector of the segment
            normals.Add u.Rotate90CCW
            pp <- p
        normals

    let internal handleUTurn (pt:Pt, cosine:float, nPrev:UnitVc, nNext:UnitVc, dist:float, res:ResizeArray<Pt>, uTurnBehavior:UTurn, useUTurnBehaviorAbove:float<Cosine.cosine>) =
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
            // make the offset point as if the acute angle matches the threshold angle, even if it is more acute
            let midV = nPrev.Rotate90CW + nNext.Rotate90CCW // the offset vector with length of almost 2.0, so use *0.5 below:
            let cosHalf =  sqrt ((1.0 + unbox<float> useUTurnBehaviorAbove) / 2.0)  // get the cosine of half the angle from the cosine of the full angle
            res.Add <| pt - midV * (0.5 * dist / cosHalf)  // hypotenuse = adjacent / cos(θ); use *0.5 because midV length is almost 2.0

        | UTurn.Skip ->
            () // do nothing, skip the point

        | x -> // should never happen
            fail $"Offset2D.UTurnBehavior: enum value {x} not recognized."

    /// The lines are described here by their normal vectors, not their tangent vectors.
    let inline private intersectFromNormals(fromA:Pt, nA:UnitVc, fromB:Pt, nB:UnitVc) : Pt =
        // using the normal vectors, first rotate 90 degrees to get the tangent vector
        let ax =  nA.Y // x and y swapped and one sign changed to rotate 90 degrees
        let ay = -nA.X
        let bx =  nB.Y
        let by = -nB.X
        let t = XLine2D.parameterA(fromA.X, fromA.Y, fromB.X, fromB.Y, ax, ay, bx, by)
        Pt(fromA.X + ax*t , fromA.Y + ay*t)

    let internal handleUTurnVarDist (pt:Pt, cosine:float, nPrev:UnitVc, nNext:UnitVc, dPrev:float, dNext:float, res:ResizeArray<Pt>, uTurnBehavior:UTurn, useUTurnBehaviorAbove:float<Cosine.cosine>) =
        // (2.3) special case: sharp U-turn with variable distances:
        match uTurnBehavior with
        | UTurn.Fail ->
            // for i,d in Seq.indexed dirs do printfn $"*dir[{i}]= {d.AsString}"
            fail $"Offset2D.offsetVariableWithDirections: pt {pt} makes a {toDegrees(acos cosine) } degree U-turn, max {toDegrees(acos (float useUTurnBehaviorAbove))} is allowed."

        | UTurn.Chamfer ->
            // (1.2) special case: sharp U-turn, add a chamfer (two points) with variable distance
            let chamferTangent = (nPrev - nNext).Unitized
            let chamferNormal = chamferTangent.Rotate90CCW
            let nMid = if chamferNormal *** nPrev >= 0.0 then chamferNormal else -chamferNormal // = chamferNormalChecked
            let dMid = (dPrev + dNext) * 0.5 // the mean distance
            let midOff = pt + nMid * dMid
            res.Add <| intersectFromNormals(pt + nPrev * dPrev, nPrev, midOff            , nMid) // the intersection of the two offset lines
            res.Add <| intersectFromNormals(midOff            , nMid , pt + nNext * dNext, nNext) // the intersection of the two offset lines

        | UTurn.UseThreshold  ->
            // make the offset point as if the acute angle matches the threshold angle, even if it is more acute
            let cosHalf =  sqrt ((1.0 + unbox<float> useUTurnBehaviorAbove) / 2.0)  // get the cosine of half the angle from the cosine of the full angle
            let rot = Rotation2D.createFromCosine cosHalf
            let nMid  = nPrev.Rotate90CW + nNext.Rotate90CCW |> Vc.unitize // the middle unit vector
            // create two normals rotated by the threshold angle instead of the real angle
            let nPrevThresh = nMid.RotateBy rot.Inverse
            let nNextThresh = nMid.RotateBy rot
            res.Add <| intersectFromNormals(pt + nPrev * dPrev, nPrevThresh, pt + nNext * dNext, nNextThresh) // the intersection of the two offset lines

        | UTurn.Skip ->
            () // do nothing, skip the point

        | x ->
            fail $"Offset2D.UTurnBehavior: enum value {x} not recognized."



    /// <summary> For closed or open polylines.</summary>
    /// <remarks> This function requires precomputed segment normals; the simpler 'Offset2D.offset' uses it internally too.</remarks>
    /// <param name="pts">The points of the Polyline2D to offset.</param>
    /// <param name="dirs">The normals of the Polyline2D to offset. One item less than pts. Must be created by counter clockwise rotation of each segment.</param>
    /// <param name="dist">The distance to offset the Polyline2D.</param>
    /// <param name="uTurnBehavior"> What to do at a 180 degree U-turn? Fail, Chamfer with two points, UseThreshold or Skip the point.</param>
    /// <param name="useUTurnBehaviorAbove"> The angle between normals after which, U-TurnBehavior is used.</param>
    /// <returns> A new ResizeArray of Points. Due to error correction at sharp U-turns the point count may not be the same as the input.</returns>
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





    //       █████   █████                      ███            █████     ████
    //      ░░███   ░░███                      ░░░            ░░███     ░░███
    //       ░███    ░███   ██████   ████████  ████   ██████   ░███████  ░███   ██████
    //       ░███    ░███  ░░░░░███ ░░███░░███░░███  ░░░░░███  ░███░░███ ░███  ███░░███
    //       ░░███   ███    ███████  ░███ ░░░  ░███   ███████  ░███ ░███ ░███ ░███████
    //        ░░░█████░    ███░░███  ░███      ░███  ███░░███  ░███ ░███ ░███ ░███░░░
    //          ░░███     ░░████████ █████     █████░░████████ ████████  █████░░██████
    //           ░░░       ░░░░░░░░ ░░░░░     ░░░░░  ░░░░░░░░ ░░░░░░░░  ░░░░░  ░░░░░░
    //
    //
    //
    //                      ██████     ██████                    █████
    //                     ███░░███   ███░░███                  ░░███
    //           ██████   ░███ ░░░   ░███ ░░░   █████   ██████  ███████
    //          ███░░███ ███████    ███████    ███░░   ███░░███░░░███░
    //         ░███ ░███░░░███░    ░░░███░    ░░█████ ░███████   ░███
    //         ░███ ░███  ░███       ░███      ░░░░███░███░░░    ░███ ███
    //         ░░██████   █████      █████     ██████ ░░██████   ░░█████
    //          ░░░░░░   ░░░░░      ░░░░░     ░░░░░░   ░░░░░░     ░░░░░
    //
    //
    //
    //              █████  ███           █████
    //             ░░███  ░░░           ░░███
    //           ███████  ████   █████  ███████    ██████   ████████    ██████   ██████
    //          ███░░███ ░░███  ███░░  ░░░███░    ░░░░░███ ░░███░░███  ███░░███ ███░░███
    //         ░███ ░███  ░███ ░░█████   ░███      ███████  ░███ ░███ ░███ ░░░ ░███████
    //         ░███ ░███  ░███  ░░░░███  ░███ ███ ███░░███  ░███ ░███ ░███  ███░███░░░
    //         ░░████████ █████ ██████   ░░█████ ░░████████ ████ █████░░██████ ░░██████
    //          ░░░░░░░░ ░░░░░ ░░░░░░     ░░░░░   ░░░░░░░░ ░░░░ ░░░░░  ░░░░░░   ░░░░░░



    /// Split list into chunks. Starts a new chunk if comparing the current item with the previous one returns true
    let internal chunkBy (split: 'T -> 'T -> bool) (res:ResizeArray<'T>) : ResizeArray<ResizeArray<'T>> =
        let chunks = ResizeArray<ResizeArray<'T>>()
        for i=0 to res.Count - 1 do
            let item = res.[i]
            if i=0 || split item res.[i-1] then // start a new chunk
                chunks.Add (ResizeArray<'T>())
            chunks.Last.Add item
        chunks

    /// If indices make a loop, merge the last chunk with the first chunk.
    let internal reLoop (lastIdx:int) (getIdx:'T-> int) (idxs:ResizeArray<ResizeArray<'T>>) =
        if idxs.Count > 1 then
            let firsts = idxs.First
            let lasts = idxs.Last
            if getIdx firsts.First = 0 && getIdx lasts.Last = lastIdx then
                lasts.AddRange firsts // add the first chunk to the last chunk
                idxs.First <- lasts    // replace the first chunk with the last one
                idxs.Pop()  |> ignore  // remove the last chunk


    /// Only used when VarDistParallelBehavior.Proportional is selected.
    let internal distributeProportionallyBadIdxs( res: ResizeArray<Pt>, colinearIdxs : ResizeArray<IndexToFixProportional>, origs: ResizeArray<Pt>) =
        let chunks = colinearIdxs |> chunkBy (fun thisIdx prevIdx -> thisIdx.idxRes <> prevIdx.idxRes + 1 ) // return true for split if not  consecutive indices
        reLoop (res.LastIndex) (fun (i: IndexToFixProportional) -> i.idxRes) chunks
        // for ch in chunks do
        //     printfn "new chunk:"
        //     for e in ch do eprintfn $"{e}"
        for i=0 to chunks.LastIndex do
            let chunk = chunks.[i]
            let offLn =
                let prevOkIdx = saveIdx (chunk.First.idxRes - 1) res.Count  // the previous point before the chunk
                let nextOkIdx = saveIdx (chunk.Last.idxRes  + 1) res.Count // the next point after the chunk
                Line2D(res[prevOkIdx], res[nextOkIdx])
            let origLn =
                let origPrevOkIdx = saveIdx (chunk.First.idxOrig - 1 ) origs.Count // .LastIndex //use origs last idx, not count, because origs is one item more,  the last and first point are already the same.
                let origNextOkIdx = saveIdx (chunk.Last.idxOrig  + 1 ) origs.Count // .LastIndex
                Line2D(origs[origPrevOkIdx], origs[origNextOkIdx])
            for j=0 to chunk.LastIndex do
                let bi = chunk.[j]
                let origPt = origs.[bi.idxOrig] // the original point to fix
                let t = origLn.RayClosestParameter(origPt) // the parameter on the original line
                res.[bi.idxRes] <- offLn.EvaluateAt t

    /// Only used when VarDistParallelBehavior.Project is selected.
    let internal projectBadIdxs(res: ResizeArray<Pt>, colinearIdxs : ResizeArray<IndexToProject>) =
       let chunks = colinearIdxs |> chunkBy (fun thisIdx prevIdx -> thisIdx.idx <> prevIdx.idx + 1 )  // return true for split if not consecutive indices
       reLoop res.LastIndex _.idx chunks
       for i=0 to chunks.LastIndex do
           let chunk = chunks.[i]
           let prev = res.GetLooped (chunk.First.idx - 1) // the previous point before the chunk
           let next = res.GetLooped (chunk.Last.idx + 1) // the next point after the chunk
           let ln = Line2D(prev,next)
           for j=0 to chunk.LastIndex do
                let itp = chunk.[j]
                let pt = res.[itp.idx] // the point to fix
                let pln = Line2D(pt, pt + itp.dir) // the line from the point in the direction of the normal
                // at shallow angles and strongly different distances, the projection may be outside of the segment
                // Should we allow that? or check for it and skip ?
                res.[itp.idx] <- XLine2D.tryIntersectRay(ln, pln).Value // Null Reference Exception should never happen here, because lines are not parallel



    (*
    // alternative offset point calculation for variable distances, all work correctly:

    let inline setOffCornerVar1 (pt:Pt) (distPrev:float) (distNext:float) (nPrev:UnitVc) (nNext:UnitVc) :Pt =
        let pAx = pt.X + nPrev.X * distPrev
        let pAy = pt.Y + nPrev.Y * distPrev
        let pBx = pt.X + nNext.X * distNext
        let pBy = pt.Y + nNext.Y * distNext
        let vAx = nPrev.Y // x and y swapped and one sign changed to rotate 90 degrees
        let vAy = -nPrev.X
        let vBx = nNext.Y
        let vBy = -nNext.X
        let t = XLine2D.parameterA( pAx, pAy, pBx, pBy, vAx, vAy, vBx, vBy)
        Pt( pAx + vAx * t , pAy + vAy * t)

    let inline setOffCornerVar2 (pt:Pt) (distPrev:float) (distNext:float) (nPrev:UnitVc) (nNext:UnitVc) (cosine:float) :Pt =
        // first get the offset point as if distances were the same
        let eqPt = pt + (nPrev + nNext) * (distNext / (1.0 + cosine))
        // second, find the offset point as if distPrev was 0.0.
        let vPrev = nPrev.Rotate90CW
        let cosN = vPrev *** nNext // never 0.0 here, because vectors are already checked to be not colinear
        // / the offset point as if distPrev was 0.0:
        let exPt = pt + vPrev * (distNext / cosN)
        // now interpolate between both points according to distPrev / distNext
        Pt.divPt(exPt, eqPt, distPrev / distNext )
    *)

    /// offset point calculation for variable distances:
    let inline internal setOffCornerVar3 (pt:Pt) (distPrev:float) (distNext:float) (nPrev:UnitVc) (nNext:UnitVc) (cosine:float) : Pt =
        let delta = distPrev - distNext
        let vNext = nNext.Rotate90CW // the unit vector along the next segment
        let cos2 = nPrev *** vNext // never 0.0 here, because vectors are already checked to be not colinear
        pt
            + vNext * (delta / cos2) // the offset for the delta in distances
            + (nPrev + nNext) * (distNext / (1.0 + cosine)) // the offset for the common distance



    let internal handleVarOffsetColinear (i:int, pt:Pt, nPrev:UnitVc, nNext:UnitVc, dPrev:float, dNext:float, res:ResizeArray<Pt>, idxsToFixProportional: ResizeArray<IndexToFixProportional>, projectIdxs: ResizeArray<IndexToProject>, varDistParallelBehavior: VarDistParallel) =
        match varDistParallelBehavior with
        | VarDistParallel.Fail ->
            fail $"Offset2D.offsetVariableWithDirections: pts.[{i}] and pts.[{i-1}] are colinear but have different distances {dPrev} and {dNext}."
        | VarDistParallel.Skip ->
            ()
        | VarDistParallel.Proportional ->
            idxsToFixProportional.Add  {idxRes = res.Count; idxOrig = i} // if there was a sharp U-turn with two points earlier, the the index into the originals now has an offset
            res.Add  pt // just add a dummy point , so we can edit it in place later
        | VarDistParallel.Project ->
            projectIdxs.Add {idx = res.Count;  dir = nPrev + nNext}
            res.Add  pt // just add the original point , so we can project it later
        | VarDistParallel.StepWithTwoPoints  ->
            // add two points, one for each distance
            res.Add <| pt + nPrev * dPrev
            res.Add <| pt + nNext * dNext
        | x ->
            fail $"Offset2D.VarDistParallelBehavior: enum value {x} not recognized."

    /// <summary> Offsetting each segment by its own distance. For closed or open polylines. Adds 2 chamfer points at U-turns and skips colinear points.</summary>
    /// <remarks> This function requires precomputed segment normals; the simpler 'Offset2D.offsetVariable' uses it internally too.</remarks>
    /// <param name="pts">The points of the Polyline to offset.</param>
    /// <param name="nDirs">The directions or normals of the Polyline segments to offset. One item less than pts. Must be created by counter clockwise rotation of each segment.</param>
    /// <param name="dists"> The distances to offset the Polyline. One item less than pts. Positive values will create inside offsets on counter-clockwise polylines.</param>
    /// <param name="varDistParallelBehavior"> What to do with colinear segments below 'useVarDistParallelBehaviorBelow' degrees when offset distances are different too.</param>
    /// <param name="uTurnBehavior"> What to do at a 180 degree U-turn? Fail, Chamfer with two points, UseThreshold or Skip the point.</param>
    /// <param name="useVarDistParallelBehaviorBelow"> The angle between normals below which points are considered colinear and VarDistParallelBehavior is applied if distances are not the same. </param>
    /// <param name="useUTurnBehaviorAbove"> The angle between normals after which uTurnBehavior is applied .</param>
    /// <returns> A new ResizeArray of Points. Due to error correction at sharp U-turns the point count may not be the same as the input.</returns>
    let offsetVariableWithDirections(pts:ResizeArray<Pt>, nDirs: ResizeArray<UnitVc>, dists:Collections.Generic.IList<float>, varDistParallelBehavior: VarDistParallel, uTurnBehavior:UTurn, useVarDistParallelBehaviorBelow :float<Cosine.cosine>, useUTurnBehaviorAbove :float<Cosine.cosine>) : ResizeArray<Pt> =
        if pts.Count < 2 then
            fail $"Offset2D.offsetVariableWithDirections:\n  point count must be at least 2, but is {pts.Count}."

        if pts.Count <> nDirs.Count + 1  then
            fail $"Offset2D.offsetVariableWithDirections:\n  point count must be 1 greater than normal directions count, but they are {pts.Count} and {nDirs.Count}."

        if pts.Count <> dists.Count + 1  then
            fail $"Offset2D.offsetVariableWithDirections:\n   point count must be 1 greater than offset distances count, but they are {pts.Count} and {dists.Count}."

        let res = ResizeArray<Pt>(pts.Count)
        let idxsToFixProportional = ResizeArray<IndexToFixProportional>() // only used if varDistParallelBehavior is Proportional
        let projectIdxs           = ResizeArray<IndexToProject>() // only used if varDistParallelBehavior is Project
        let mutable dPrev = dists.[dists.Count-1]
        let mutable nPrev = nDirs.[nDirs.Count-1]

        let isOpen = Pt.distanceSq pts.First pts.Last > sqOpenTolerance
        let mutable fromIdx = 0
        if isOpen then // this is needed for open polylines that have 180 degree opposing start and end segments , see check 2.01 below
            fromIdx <- 1
            nPrev <- nDirs.[0]
            dPrev <- dists.[0]
            res.Add <| pts.[0] + nPrev * dPrev

        for i = fromIdx to pts.Count - 2 do // last point is skipped; it is dealt with at the end
            let nNext = nDirs.[i]
            let dNext = dists.[i]
            let pt = pts.[i]
            let cosine = UnitVc.dot (nPrev , nNext)

            if abs (dPrev - dNext) < 1e-6 then
                // (1) both distances are the same, no line intersection needed
                if withMeasure cosine > useUTurnBehaviorAbove  then // exclude U-turns
                    // (1.1) the regular case for same distances:
                    res.Add <| setOffCorner pt dPrev nPrev nNext cosine
                else
                    // (1.2) special case: sharp U-turn, add a chamfer (two points)
                    handleUTurn (pt, cosine, nPrev, nNext, dPrev, res, uTurnBehavior, useUTurnBehaviorAbove)

            else
                // (2) distances are different, so we need to find the intersection of the two offset lines
                if withMeasure cosine > useUTurnBehaviorAbove || (i=0 && isOpen) then // exclude U-turns, but don't check the first point of an open polyline , ( check 2.01 )
                    if withMeasure cosine < useVarDistParallelBehaviorBelow then // check for colinear segments
                        // (2.1) the regular case for variable distances:
                        // res.Add <| setOffCornerVar pt dPrev dNext nPrev nNext  // or use alternative calculation method setOffCornerVar2 ?
                        // res.Add <| setOffCornerVar2 pt dPrev dNext nPrev nNext cos // or use alternative calculation method setOffCornerVar3 ?
                        res.Add <| setOffCornerVar3 pt dPrev dNext nPrev nNext cosine // the cheapest methods, all are correct
                    else
                        // (2.2) special case: colinear segments:
                        handleVarOffsetColinear (i, pt, nPrev, nNext, dPrev, dNext, res, idxsToFixProportional, projectIdxs, varDistParallelBehavior)

                else
                    // (2.3) special case: sharp U-turn with variable distances:
                    handleUTurnVarDist (pt, cosine, nPrev, nNext, dPrev, dNext, res, uTurnBehavior, useUTurnBehaviorAbove)

            nPrev <- nNext
            dPrev <- dNext

        // (3) if the polyline is open, then we need to fix first and last point
        if isOpen then
            res.Add <| pts.Last + nDirs.Last * dists.[dists.Count - 1]
        else
            res.Add res.[0] // add the last point, which is the same as the first point

        // (4) now if there were colinear segments with different distances, we need to fix those points
        // first add another bad index at the end.
        // then fix colinear points either proportionally or by projection
        if idxsToFixProportional.Count > 0 then
            // if the first index is to fix, then also add the last index to fix, to close the loop
            if idxsToFixProportional.First.idxRes = 0 then
                idxsToFixProportional.Add  {idxRes = res.LastIndex; idxOrig = pts.LastIndex}
            distributeProportionallyBadIdxs(res, idxsToFixProportional, pts)
        elif projectIdxs.Count > 0 then
            // if the first index is to fix, then also add the last index to fix, to close the loop
            if projectIdxs.First.idx = 0 then
                projectIdxs.Add  {idx = res.LastIndex; dir = projectIdxs.First.dir}
            projectBadIdxs(res, projectIdxs)
        res


    //    █████████                                  ███               █████      █████████   ███████████  █████
    //   ███░░░░░███                                ░░░               ░░███      ███░░░░░███ ░░███░░░░░███░░███
    //  ███     ░░░  █████ ████ ████████  ████████  ████   ██████   ███████     ░███    ░███  ░███    ░███ ░███
    // ░███         ░░███ ░███ ░░███░░███░░███░░███░░███  ███░░███ ███░░███     ░███████████  ░██████████  ░███
    // ░███          ░███ ░███  ░███ ░░░  ░███ ░░░  ░███ ░███████ ░███ ░███     ░███░░░░░███  ░███░░░░░░   ░███
    // ░░███     ███ ░███ ░███  ░███      ░███      ░███ ░███░░░  ░███ ░███     ░███    ░███  ░███         ░███
    //  ░░█████████  ░░████████ █████     █████     █████░░██████ ░░████████    █████   █████ █████        █████
    //   ░░░░░░░░░    ░░░░░░░░ ░░░░░     ░░░░░     ░░░░░  ░░░░░░   ░░░░░░░░    ░░░░░   ░░░░░ ░░░░░        ░░░░░





    /// <summary> A constant-distance offset algorithm for closed or open polylines.</summary>
    /// <param name="useUTurnBehaviorAbove"> The angle between normals after which the uTurnBehavior is applied.</param>
    /// <param name="uTurnBehavior"> What to do at a 180 degree U-turn? Fail, Chamfer with two points, UseThreshold or Skip the point.</param>
    /// <param name="pts">The points of the Polyline2D to offset.</param>
    /// <param name="dist">The distance to offset the Polyline2D. A positive distance will offset inwards on counter-clockwise curves.
    /// A negative distance will offset inwards on clockwise curves.</param>
    /// <returns> A new ResizeArray of Points. Due to error correction at sharp U-turns the point count may not be the same as the input. </returns>
    let offset'' (useUTurnBehaviorAbove :float<Cosine.cosine>) (uTurnBehavior:UTurn) (dist:float) (pts:ResizeArray<Pt>): ResizeArray<Pt> =
        if pts.Count < 2 then
            fail $"Offset2D.offset'': pts.Count must be at least 2 but is {pts.Count}."
        if abs dist < 1e-12 then
            pts.GetRange(0, pts.Count-1)// if distance is zero, just clone the polyline
        else
            offsetWithDirections(pts, makeOffsetDirections pts, dist, uTurnBehavior, useUTurnBehaviorAbove)


    /// <summary> A constant-distance offset algorithm for closed or open polylines.</summary>
    /// <param name="uTurnBehavior"> What to do at a 180 degree U-turn? Fail, Chamfer with two points, UseThreshold or Skip the point.
    /// This will only be applied for joints bigger than 175° degrees.</param>
    /// <param name="pts">The points of the Polyline2D to offset.</param>
    /// <param name="dist">The distance to offset the Polyline2D. A positive distance will offset inwards on counter-clockwise curves.
    /// A negative distance will offset inwards on clockwise curves.</param>
    /// <returns> A new ResizeArray of Points. Due to error correction at sharp U-turns the point count may not be the same as the input. </returns>
    let offset' (uTurnBehavior:UTurn) (dist:float) (pts:ResizeArray<Pt>): ResizeArray<Pt> =
        if pts.Count < 2 then
            fail $"Offset2D.offset': pts.Count must be at least 2 but is {pts.Count}."
        if abs dist < 1e-12 then
            pts.GetRange(0, pts.Count-1)// if distance is zero, just clone the polyline
        else
            offsetWithDirections(pts, makeOffsetDirections pts, dist, uTurnBehavior, Cosine.``175.0``)


    /// <summary> A constant-distance offset algorithm for closed or open polylines.
    /// Fails at corners or U-turns sharper than joints after 177.5° degrees.</summary>
    /// <param name="pts">The points of the Polyline2D to offset.</param>
    /// <param name="dist">The distance to offset the Polyline2D. A positive distance will offset inwards on counter-clockwise curves.
    /// A negative distance will offset inwards on clockwise curves.</param>
    /// <returns> A new ResizeArray of Points. The point count will be the same as the input. </returns>
    let offset (dist:float) (pts:ResizeArray<Pt>): ResizeArray<Pt> =
        if pts.Count < 2 then
            fail $"Offset2D.offset: pts.Count must be at least 2 but is {pts.Count}."
        if abs dist < 1e-12 then
            pts.GetRange(0, pts.Count-1)// if distance is zero, just clone the polyline
        else
            offsetWithDirections(pts, makeOffsetDirections pts, dist, UTurn.Fail, Cosine.``177.5``) // default chamfer after the given angle


    /// <summary> Offsetting each segment by its own distance. For closed or open polylines.
    /// The behaviour and the limits for colinear and 180 degree U-turns are configurable.</summary>
    /// <param name="useUTurnBehaviorAbove"> The angle between normals after which, instead of a normal miter, the joint is chamfered by adding an extra point.</param>
    /// <param name="useVarDistParallelBehaviorBelow"> The angle between normals below which points are considered colinear and VarDistParallelBehavior is applied if distances are not the same. </param>
    /// <param name="uTurnBehavior"> What to do at a 180 degree U-turn? Fail, Chamfer with two points, UseThreshold or Skip the point.</param>
    /// <param name="varDistParallelBehavior"> What to do with colinear segments below 'useVarDistParallelBehaviorBelow' degrees when offset distances are different too.</param>
    /// <param name="dists"> The distances to offset the Polyline. One item less than pts. Positive values will create inside offsets on counter-clockwise polylines.</param>
    /// <param name="pts">The points of the Polyline to offset.</param>
    /// <returns> A new ResizeArray of Points.
    /// Due to error correction at sharp U-turns or the picked behaviour for collinear segments
    /// the point count may not be the same as the input.</returns>
    let offsetVariable'' (useUTurnBehaviorAbove :float<Cosine.cosine>) (useVarDistParallelBehaviorBelow :float<Cosine.cosine>) (uTurnBehavior:UTurn) (varDistParallelBehavior: VarDistParallel) (dists:ResizeArray<float>) (pts:ResizeArray<Pt>)  =
        let nDirs = makeOffsetDirections pts
        offsetVariableWithDirections(pts, nDirs, dists, varDistParallelBehavior, uTurnBehavior, useVarDistParallelBehaviorBelow, useUTurnBehaviorAbove) // default chamfer after the given angle


    /// <summary> Offsetting each segment by its own distance.
    /// The behaviour for colinear and 180 degree U-turns is configurable.</summary>
    /// <param name="uTurnBehavior"> What to do at a 180 degree U-turn? Fail, Chamfer with two points, UseThreshold or Skip the point. will be applied for joints bigger than 175° degrees.</param>
    /// <param name="varDistParallelBehavior"> What to do with colinear segments within 2.5 degrees when offset distances are different too..</param>
    /// <param name="dists"> The distances to offset the Polyline. One item less than pts. Positive values will create inside offsets on counter-clockwise polylines.</param>
    /// <param name="pts">The points of the Polyline to offset.</param>
    /// <returns> A new ResizeArray of Points.
    /// Due to error correction at sharp U-turns or the picked behaviour for collinear segments
    /// the point count may not be the same as the input.</returns>
    let offsetVariable' (uTurnBehavior:UTurn)  (varDistParallelBehavior: VarDistParallel) (dists:ResizeArray<float>) (pts:ResizeArray<Pt>) =
        let nDirs = makeOffsetDirections pts
        offsetVariableWithDirections(pts, nDirs, dists, varDistParallelBehavior, uTurnBehavior, Cosine.``2.5``, Cosine.``175.0``)


    /// <summary> Offsetting each segment by its own distance. For closed or open polylines.
    /// Fails at U-turns above 175 degrees and at colinear segments within less than 2.5 degrees. </summary>
    /// <param name="dists"> The distances to offset the Polyline. One item less than pts.</param>
    /// <param name="pts">The points of the Polyline to offset. Positive values will create inside offsets on counter-clockwise polylines.</param>
    /// <returns> A new ResizeArray of Points. The point count is the same as the input.</returns>
    let offsetVariable (dists:ResizeArray<float>) (pts:ResizeArray<Pt>) =
        let nDirs = makeOffsetDirections pts
        offsetVariableWithDirections(pts, nDirs, dists, VarDistParallel.Fail, UTurn.Fail, Cosine.``2.5``, Cosine.``175.0``)

