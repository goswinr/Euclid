namespace Euclid

open System
open UtilEuclid
open EuclidErrors
open Euclid.EuclidCollectionUtilities

module R = ResizeArr

/// A module containing the core algorithms for offsetting 2D polylines.
/// Normally you would not use this directly; prefer the Polyline2D or Points2D module.
module Offset2D=

    /// The squared tolerance for open polylines.
    let [<Literal>] sqOpenTolerance = 1e-6 * 1e-6 // 1e-12


    /// An Enum for describing what to do at U-turns (180 degree turns).
    /// It is not possible to offset U-turns correctly.
    /// This Enum describes the options:
    /// 1: Fail
    /// 2: Add a chamfer with two points.
    /// 3: Use the offset point for the threshold angle, by default this is 179 degrees.
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


    /// An Enum for describing what to do with collinear segments with different offset distances.
    /// It is not possible to offset collinear segments if the distances are not the same.
    /// This Enum describes the options:
    /// 1: Fail
    /// 2: just skip the point.
    /// 3: add a point at closest distance on the oblique segment, non parallel offsets.
    /// 4: add a point perpendicular from here intersecting the oblique segment, non parallel offsets.
    /// 5: add a step with two points.
    // [<RequireQualifiedAccess>]
    type VarDistParallel = // needs to be an enum , so it can bve an optional parameter with default value

        /// Fail if collinear segments with different offset distances are found.
        | Fail = 1

        /// Just skip collinear points with different offset distances.
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

        /// Add a step with two points at the collinear segment.
        /// This keeps parallel offset segments.
        /// But introduces an extra point.
        | StepWithTwoPoints = 5



    /// Internal only.
    /// So that collinear points can be fixed after the main offsetting loop is done.
    type internal IndexToFixProportional = {
        /// the index of the collinear point in result
        idxRes:int
        /// index of collinear point in input.
        /// usually the same as idxRes, but a 180 uTurn with two points might have create a shifting of indices.
        idxOrig:int
    }

    [<NoComparison; NoEquality>]
    type internal IndexToProject = {
        /// the index of the collinear point in result
        idx:int
        /// x component of the direction to project along
        dirX: float
        /// y component of the direction to project along
        dirY: float
    }

    module private XY =

        let inline pointCount (xys: ResizeArray<float>) : int =
            (ResizeArr.len xys) / 2


        let inline checkEven methodName (xys: ResizeArray<float>) =
            if xys.Count % 2 <> 0 then
                failEven methodName xys

        let inline getX i (xys: ResizeArray<float>) : float =
            ResizeArr.getIdx (i * 2) xys


        let inline getY i (xys: ResizeArray<float>) : float =
            ResizeArr.getIdx (i * 2 + 1) xys


        let inline getPt i (xys: ResizeArray<float>) : Pt =
            Pt( ResizeArr.getIdx (i * 2    ) xys,
                ResizeArr.getIdx (i * 2 + 1) xys)

        let inline set i x y (xys: ResizeArray<float>) : unit =
            ResizeArr.setIdx (i * 2) x xys
            ResizeArr.setIdx (i * 2 + 1) y xys

        let inline add x y (xys: ResizeArray<float>) : unit =
            xys.Add x
            xys.Add y

        let inline sqDistFirstLast (xys: ResizeArray<float>) : float =
            let dx = xys.[0] - xys.SecondLast
            let dy = xys.[1] - xys.Last
            dx * dx + dy * dy

    /// A helper function to print interleaved ResizeArrays for debugging.
    let toStringXYs (xys: ResizeArray<float>) : string =
        if isNull xys then
            "ResizeArray<float> null"
        elif xys.Count = 0 then
            "Empty ResizeArray<float>"
        elif xys.Count = 1 then
            $"Invalid ResizeArray<float> with 1 value: {xys.[0]}"
        elif xys.Count = 2 then
            $"ResizeArray<float> Single Point: ({xys.[0]}, {xys.[1]})"
        else
            let sb = Text.StringBuilder()
            let cnt = xys.Count
            let ln (line:string) =
                sb.AppendLine(line) |> ignore
            if cnt % 2 <> 0 then
                ln $"An invalid ResizeArray<float> Polyline with {cnt} values, which is not even."
                for i = 0 to min (cnt - 1) 100 do
                    if i % 2 = 0 then
                        ln $"    x {xys.[i]}"
                    else
                        ln $"    y {xys.[i]}"
                if cnt > 100 then
                    ln $"    ... and {cnt - 100} more values."
            else
                // it's a valid Polyline
                let dist = sqrt (
                    (xys.[0] - xys.SecondLast) * (xys.[0] - xys.SecondLast) +
                    (xys.[1] - xys.Last      ) * (xys.[1] - xys.Last)
                    )

                if dist = 0.0 then
                    ln $"ResizeArray<float>:Closed polyline with {cnt / 2} points."
                elif dist < 1e-6 then
                    ln $"ResizeArray<float>:Almost closed polyline with {cnt / 2} points, distance between first and last point is {dist}."
                else
                    ln $"ResizeArray<float>:Open polyline with {cnt / 2} points, distance between first and last point is {dist}."
                let loopTil =  min cnt 100
                let mutable i = 0
                while i < loopTil do
                    let x = xys.[i]
                    let y = xys.[i + 1]
                    ln $"    {x}  {y}"
                    i <- i + 2
                if cnt > 100 then
                    ln $"    ... and {cnt - 100} more values."
                    ln $"    Last point: ({xys.SecondLast}, {xys.Last})"
            sb.ToString()

    let inline private dot (ax:float) (ay:float) (bx:float) (by:float) : float =
        ax * bx + ay * by

    let inline private chamferNormalBetween (nPrevX:float) (nPrevY:float) (nNextX:float) (nNextY:float) : struct (float * float * float) =
        let tx = nPrevX - nNextX
        let ty = nPrevY - nNextY
        let len = sqrt (tx * tx + ty * ty)
        let nx = -ty / len
        let ny =  tx / len
        let cos = dot nx ny nPrevX nPrevY
        if cos >= 0.0 then
            struct (nx, ny, cos)
        else
            struct (-nx, -ny, cos)



    /// Returns the unit tangents of the segments of an X and Y interleaved ResizeArray.
    /// The vector count is one less than the input point count. The result is interleaved x/y values.
    /// Fails on duplicate points.
    let makeUnitTangents (xys:ResizeArray<float>) : ResizeArray<float> =
        XY.checkEven "makeUnitTangents" xys
        let ptCount = XY.pointCount xys
        if ptCount < 2 then
            fail $"Offset2D.makeUnitTangents: point count {ptCount} must be at least 2 for a polyline."
        let uVecs = ResizeArray<float>((ptCount - 1) * 2) // the unit vectors of the segments, two floats per segment
        let mutable px = xys.[0]
        let mutable py = xys.[1]
        for i = 1 to ptCount - 1 do
            let x = XY.getX i xys
            let y = XY.getY i xys
            let vx = x - px
            let vy = y - py
            let len = sqrt (vx * vx + vy * vy)
            if len < 1e-6 then
                fail $"Offset2D.makeUnitTangents: point[{i}] and point[{i-1}] are the same at ({px}, {py})."
            // TODO: or just skip duplicate points?
            uVecs.Add (vx / len)
            uVecs.Add (vy / len)
            px <- x
            py <- y
        uVecs


    /// Returns the normals of the segments of an X and Y interleaved ResizeArray; each segment is rotated 90 degrees in counter-clockwise order.
    /// The vector count is one less than the input point count. The result is interleaved x/y values.
    /// Fails on duplicate points.
    let makeOffsetDirections (xys:ResizeArray<float>) : ResizeArray<float> =
        // printfn $"***Offset2D.makeOffsetDirections: for {xys.Count / 2} points.{toStringXYs xys}"
        XY.checkEven "makeOffsetDirections" xys
        let cnt = xys.Count
        if cnt < 4 then
            fail $"Offset2D.makeOffsetDirections: point count {cnt / 2} must be at least 2 for a polyline."
        let mutable px = xys.[0]
        let mutable py = xys.[1]
        let mutable i = 2
        let normals = ResizeArray<float>(cnt-2) // the normals of the segments, two floats per segment
        while i < cnt do
            let x = xys.[i]
            let y = xys.[i + 1]
            let vx = x - px
            let vy = y - py
            let len = sqrt (vx * vx + vy * vy)
            if len < 1e-6 then
                fail $"Offset2D.makeOffsetDirections: point[{i/2}] and point[{i/2-1}] are the same, at Pt({px}, {py}) in \n{toStringXYs xys}."
            normals.Add (vy / -len) // x = -y, y = x : =Rotate90CCW
            normals.Add (vx / len)
            px <- x
            py <- y
            i <- i + 2
        normals

    // #endregion
    // #region Remove U-Turns

    /// <summary>Removes Sharp U-Turns from an X and Y interleaved ResizeArray, like Polyline2D is using</summary>
    /// <param name="xys">The interleaved coordinates of the polyline.</param>
    /// <param name="ns">The precomputed interleaved unit tangents or unit normals. (either are fine)</param>
    /// <param name="minCos">The minimum cosine value for detecting U-turns.</param>
    /// <returns>If no U-turns are present, the original ResizeArray is returned.
    /// If U-turns are present, a new ResizeArray of coordinates is returned with the U-turns removed.</returns>
    let removeUTurns (xys:ResizeArray<float>, ns: ResizeArray<float>, minCos:float<Cosine.cosine>) : ResizeArray<float> =
        XY.checkEven "removeUTurns" xys
        XY.checkEven "removeUTurns normals" ns
        let ptCount = XY.pointCount xys
        let nCount = XY.pointCount ns
        if ptCount < 2 then
            fail $"Offset2D.removeUTurns: point count must be at least 2 but is {ptCount}."

        if ptCount <> nCount + 1  then
            fail $"Offset2D.removeUTurns: point count must be one greater than normals count but they are {ptCount} and {nCount}."

        let isClosed = XY.sqDistFirstLast xys < sqOpenTolerance

        // (1) first collect all the bad indices that have Uturns:
        let bad = Array.zeroCreate<bool> ptCount
        let mutable anyBad = false
        let mutable nPrevX = XY.getX (nCount - 1) ns
        let mutable nPrevY = XY.getY (nCount - 1) ns

        // (1.1) check the first and last point if it's a closed polyline
        if isClosed then
            let nNextX = XY.getX 0 ns
            let nNextY = XY.getY 0 ns
            let cosine = dot nPrevX nPrevY nNextX nNextY
            let isBad = withMeasure cosine <= minCos // cos is smaller than -0.99.. (but never less than -1.0)
            bad[0]            <- isBad
            bad[bad.Length-1] <- isBad
            anyBad <- isBad

        // (1.2) the main loop
        nPrevX <- XY.getX 0 ns
        nPrevY <- XY.getY 0 ns
        for i = 1 to bad.Length - 2 do
            let nNextX = XY.getX i ns // the normal vector for the segment from pt to next point
            let nNextY = XY.getY i ns
            let cosine = dot nPrevX nPrevY nNextX nNextY
            let isBad = withMeasure cosine <= minCos
            bad[i] <- isBad
            anyBad <- anyBad || isBad
            nPrevX <- nNextX
            nPrevY <- nNextY

        // (2) collect points and re-intersect where bad points are skipped
        if anyBad then
            let res = ResizeArray<float>(xys.Count)
            for i = 0 to bad.Length - 1 do
                if not bad[i] then
                    XY.add (XY.getX i xys) (XY.getY i xys) res
            // If the polyline was closed and its seam vertex (the shared start/end point) was a U-turn,
            // both bad[0] and bad[last] were skipped above, which would leave the result open.
            // Re-close it by repeating the new first point at the end so the closed property is preserved.
            if isClosed && bad[0] && res.Count >= 4 then
                XY.add res.[0] res.[1] res
            res
        else
            xys // no bad points, return original


    // #endregion
    // #region Constant Distance


    /// The core offset Algorithm for constant distance offsets.
    /// Returns the offset point based on the previous and next normals,
    /// the distance, and the precomputed cosine (= dot product of nPrev * nNext).
    /// (pt + (nPrev + nNext) * (dist / (1.0 + cosine)))
    let inline setOffCorner (res:ResizeArray<float>, x:float, y:float, dist:float, nPrevX:float, nPrevY:float, nNextX:float, nNextY:float, cosine:float) : unit =
        #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
            if cosine < -0.9999999 then
                fail $"Offset2D.setOffCorner: cosine is {cosine} , a 180 degree U-turn, which is not allowed here for dist {dist}, nPrev ({nPrevX}, {nPrevY}), nNext ({nNextX}, {nNextY}) at ({x}, {y})."
        #endif
            // based on https://www.angusj.com/clipper2/Docs/Trigonometry.htm
            let k = dist / (1.0 + cosine)
            let xx = x + (nPrevX + nNextX) * k
            let yy = y + (nPrevY + nNextY) * k
            res.Add xx
            res.Add yy


    let internal handleUTurn (x:float, y:float, cosine:float, nPrevX:float, nPrevY:float, nNextX:float, nNextY:float, dist:float, res:ResizeArray<float>, uTurnBehavior:UTurn, useUTurnBehaviorAbove:float<Cosine.cosine>) :unit =
        match uTurnBehavior with
        | UTurn.Fail ->
            fail $"Offset2D.handleUTurn: point ({x}, {y}) makes a %.4f{toDegrees(acos cosine) } degree U-turn, max {toDegrees(acos (float useUTurnBehaviorAbove))} is allowed."

        | UTurn.Chamfer ->
            // (1.2) special case: sharp U-turn, add a chamfer, i.e., two points instead of one
            let struct (nMidX, nMidY, cos) = chamferNormalBetween nPrevX nPrevY nNextX nNextY
            setOffCorner (res, x, y, dist, nPrevX, nPrevY, nMidX,  nMidY,  cos) // the first chamfer point
            setOffCorner (res, x, y, dist, nMidX,  nMidY,  nNextX, nNextY, cos) // the second chamfer point

        | UTurn.UseThreshold  ->
            // make the offset point as if the acute angle matches the threshold angle, even if it is more acute
            let midX = nPrevY - nNextY // nPrev.Rotate90CW + nNext.Rotate90CCW, length almost 2.0, so use *0.5 below
            let midY = nNextX - nPrevX
            let cosHalf =  sqrt ((1.0 + unbox<float> useUTurnBehaviorAbove) / 2.0)  // get the cosine of half the angle from the cosine of the full angle
            let xx = x - midX * (0.5 * dist / cosHalf) // hypotenuse = adjacent / cos(θ); use *0.5 because mid vector length is almost 2.0
            let yy = y - midY * (0.5 * dist / cosHalf)
            res.Add xx
            res.Add yy

        | x -> // should never happen
            fail $"Offset2D.UTurnBehavior: enum value {x} not recognized."


    /// The lines are described here by their normal vectors, not their tangent vectors.
    let inline private intersectFromNormals(res:ResizeArray<float>, fromAx:float, fromAy:float, nAx:float, nAy:float, fromBx:float, fromBy:float, nBx:float, nBy:float) : unit =
        // using the normal vectors, first rotate 90 degrees to get the tangent vector
        let ax =  nAy // x and y swapped and one sign changed to rotate 90 degrees
        let ay = -nAx
        let bx =  nBy
        let by = -nBx
        let t = XLineXY.parameterA(fromAx, fromAy, fromBx, fromBy, ax, ay, bx, by)
        let x = fromAx + ax*t
        let y = fromAy + ay*t
        res.Add x
        res.Add y


    let internal handleUTurnVarDist (x:float, y:float, cosine:float, nPrevX:float, nPrevY:float, nNextX:float, nNextY:float, dPrev:float, dNext:float, res:ResizeArray<float>, uTurnBehavior:UTurn, useUTurnBehaviorAbove:float<Cosine.cosine>) :unit  =
        // (2.3) special case: sharp U-turn with variable distances:
        match uTurnBehavior with
        | UTurn.Fail ->
            // for i,d in Seq.indexed dirs do printfn $"*dir[{i}]= {d.AsString}"
            fail $"Offset2D.offsetVariableWithDirections: point ({x}, {y}) makes a {toDegrees(acos cosine) } degree U-turn, max {toDegrees(acos (float useUTurnBehaviorAbove))} is allowed."

        | UTurn.Chamfer ->
            // (1.2) special case: sharp U-turn, add a chamfer (two points) with variable distance
            let struct (nMidX, nMidY, _) = chamferNormalBetween nPrevX nPrevY nNextX nNextY
            let dMid = (dPrev + dNext) * 0.5 // the mean distance
            let midX = x + nMidX * dMid
            let midY = y + nMidY * dMid
            let aX = x + nPrevX * dPrev
            let aY = y + nPrevY * dPrev
            let bX = x + nNextX * dNext
            let bY = y + nNextY * dNext
            intersectFromNormals(res, aX, aY, nPrevX, nPrevY, midX, midY, nMidX, nMidY) // the intersection of the two offset lines
            intersectFromNormals(res, midX, midY, nMidX, nMidY, bX, bY, nNextX, nNextY) // the intersection of the two offset lines


        | UTurn.UseThreshold  ->
            // make the offset point as if the acute angle matches the threshold angle, even if it is more acute
            let cosHalf =  sqrt ((1.0 + unbox<float> useUTurnBehaviorAbove) / 2.0)  // get the cosine of half the angle from the cosine of the full angle
            let sinHalf = sqrt (1.0 - cosHalf * cosHalf)
            let nMidX = nPrevY - nNextY // nPrev.Rotate90CW + nNext.Rotate90CCW
            let nMidY = nNextX - nPrevX
            let nMidLen = sqrt (nMidX * nMidX + nMidY * nMidY)
            let nMidX = nMidX / nMidLen
            let nMidY = nMidY / nMidLen
            // create two normals rotated by the threshold angle instead of the real angle
            let nPrevThreshX = nMidX * cosHalf + nMidY * sinHalf
            let nPrevThreshY = -nMidX * sinHalf + nMidY * cosHalf
            let nNextThreshX = nMidX * cosHalf - nMidY * sinHalf
            let nNextThreshY = nMidX * sinHalf + nMidY * cosHalf
            let aX = x + nPrevX * dPrev
            let aY = y + nPrevY * dPrev
            let bX = x + nNextX * dNext
            let bY = y + nNextY * dNext
            intersectFromNormals(res, aX, aY, nPrevThreshX, nPrevThreshY, bX, bY, nNextThreshX, nNextThreshY) // the intersection of the two offset lines

        | x ->
            fail $"Offset2D.UTurnBehavior: enum value {x} not recognized."



    /// <summary> For closed or open X and Y interleaved ResizeArrays of x and y.</summary>
    /// <remarks> This function requires precomputed segment normals; the simpler 'Offset2D.offset' uses it internally too.</remarks>
    /// <param name="xys">The interleaved coordinates of the Polyline2D to offset.</param>
    /// <param name="dirs">The interleaved normals of the Polyline2D to offset. One vector less than the point count. Must be created by counter clockwise rotation of each segment.</param>
    /// <param name="dist">The distance to offset the coordinates.</param>
    /// <param name="uTurnBehavior"> What to do at a 180 degree U-turn? Fail, Chamfer with two points, or UseThreshold.</param>
    /// <param name="useUTurnBehaviorAbove"> The angle between normals after which, U-TurnBehavior is used.</param>
    /// <returns> A new ResizeArray of interleaved coordinates. Due to error correction at sharp U-turns the point count may not be the same as the input.</returns>
    let offsetWithDirections(xys:ResizeArray<float>, dirs: ResizeArray<float>, dist:float, uTurnBehavior:UTurn, useUTurnBehaviorAbove :float<Cosine.cosine>) : ResizeArray<float> = //[<OPT;DEF(Cosine.``179.0``)>]
        XY.checkEven "offsetWithDirections" xys
        XY.checkEven "offsetWithDirections dirs" dirs
        let ptCount = XY.pointCount xys
        let dirCount = XY.pointCount dirs
        if ptCount < 2 then
            fail $"Offset2D.offsetWithDirections: point count must be at least 2 but is {ptCount}."

        if ptCount <> dirCount + 1  then
            fail $"Offset2D.offsetWithDirections: point count must be one greater than normals count but they are {ptCount} and {dirCount}."

        let res = ResizeArray<float>(xys.Count)
        let isOpen = XY.sqDistFirstLast xys > sqOpenTolerance
        let mutable fromIdx = 0
        let mutable nPrevX = XY.getX (dirCount - 1) dirs
        let mutable nPrevY = XY.getY (dirCount - 1) dirs
        if isOpen then // this is needed for open polylines that have 180 degree opposing start and end segments
            fromIdx <- 1
            nPrevX <- XY.getX 0 dirs
            nPrevY <- XY.getY 0 dirs
            XY.add (xys.[0] + nPrevX * dist) (xys.[1] + nPrevY * dist) res

        for i = fromIdx to ptCount - 2 do // last point is skipped; it's dealt with at the end
            let nNextX = XY.getX i dirs // the normal vector for the segment from pt to next point
            let nNextY = XY.getY i dirs
            let x = XY.getX i xys
            let y = XY.getY i xys
            let cosine = dot nPrevX nPrevY nNextX nNextY
            if withMeasure cosine > useUTurnBehaviorAbove then // exclude U-turns
                // (1.1) the regular case:
                setOffCorner (res, x, y, dist, nPrevX, nPrevY, nNextX, nNextY, cosine)
            else
                handleUTurn (x, y, cosine, nPrevX, nPrevY, nNextX, nNextY, dist, res, uTurnBehavior, useUTurnBehaviorAbove)
            nPrevX <- nNextX
            nPrevY <- nNextY

        // if the polyline is open, then we need to fix first and last point
        if isOpen then
            let nLastX = XY.getX (dirCount - 1) dirs
            let nLastY = XY.getY (dirCount - 1) dirs
            let li = (ptCount - 1) * 2
            XY.add (xys.[li] + nLastX * dist) (xys.[li + 1] + nLastY * dist) res
        else
            XY.add res.[0] res.[1] res // add the last point, which is the same as the first point
        res



    // #endregion
    // #region Variable Distance


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
    let internal distributeProportionallyBadIdxs( res: ResizeArray<float>, collinearIdxs : ResizeArray<IndexToFixProportional>, origs: ResizeArray<float>) =
        let chunks = collinearIdxs |> chunkBy (fun thisIdx prevIdx -> thisIdx.idxRes <> prevIdx.idxRes + 1 ) // return true for split if not  consecutive indices
        reLoop (XY.pointCount res - 1) (fun (i: IndexToFixProportional) -> i.idxRes) chunks
        for i=0 to chunks.LastIndex do
            let chunk = chunks.[i]
            let offLn =
                let prevOkIdx = safeIdx (chunk.First.idxRes - 1) (XY.pointCount res)  // the previous point before the chunk
                let nextOkIdx = safeIdx (chunk.Last.idxRes  + 1) (XY.pointCount res) // the next point after the chunk
                Line2D( XY.getX prevOkIdx res, XY.getY prevOkIdx res,
                        XY.getX nextOkIdx res, XY.getY nextOkIdx res)
            let origLn =
                let origPrevOkIdx = safeIdx (chunk.First.idxOrig - 1 ) (XY.pointCount origs)
                let origNextOkIdx = safeIdx (chunk.Last.idxOrig  + 1 ) (XY.pointCount origs)
                Line2D( XY.getX origPrevOkIdx origs, XY.getY origPrevOkIdx origs,
                        XY.getX origNextOkIdx origs, XY.getY origNextOkIdx origs)
            for j=0 to chunk.LastIndex do
                let bi = chunk.[j]
                let origPt = XY.getPt bi.idxOrig origs // the original point to fix
                let t = origLn.RayClosestParameter(origPt) // the parameter on the original line
                let fixedPt = offLn.EvaluateAt t
                XY.set bi.idxRes fixedPt.X fixedPt.Y res

    /// Only used when VarDistParallelBehavior.Project is selected.
    let internal projectBadIdxs(res: ResizeArray<float>, collinearIdxs : ResizeArray<IndexToProject>) =
       let chunks = collinearIdxs |> chunkBy (fun thisIdx prevIdx -> thisIdx.idx <> prevIdx.idx + 1 )  // return true for split if not consecutive indices
       reLoop (XY.pointCount res - 1) _.idx chunks
       for i=0 to chunks.LastIndex do
           let chunk = chunks.[i]
           let prev = XY.getPt (safeIdx (chunk.First.idx - 1) (XY.pointCount res)) res // the previous point before the chunk
           let next = XY.getPt (safeIdx (chunk.Last.idx + 1) (XY.pointCount res)) res // the next point after the chunk
           let ln = Line2D(prev,next)
           for j=0 to chunk.LastIndex do
                let itp = chunk.[j]
                let pt = XY.getPt itp.idx res // the point to fix
                let pln = Line2D(pt.X, pt.Y, pt.X + itp.dirX, pt.Y + itp.dirY) // the line from the point in the direction of the normal
                // at shallow angles and strongly different distances, the projection may be outside of the segment
                // Should we allow that? or check for it and skip ?
                let fixedPt = XLine2D.intersectRays(ln, pln) //  Exception should never happen here, because lines are not parallel
                XY.set itp.idx fixedPt.X fixedPt.Y res

    /// offset point calculation for variable distances:
    let inline internal setOffCornerVar3 (res: ResizeArray<float>, x:float, y:float, distPrev:float, distNext:float, nPrevX:float, nPrevY:float, nNextX:float, nNextY:float, cosine:float) : unit =
        let delta = distPrev - distNext
        let vNextX = nNextY // the unit vector along the next segment
        let vNextY = -nNextX
        let cos2 = dot nPrevX nPrevY vNextX vNextY // never 0.0 here, because vectors are already checked to be not collinear
        let common = distNext / (1.0 + cosine)
        let x = x + vNextX * (delta / cos2) + (nPrevX + nNextX) * common
        let y = y + vNextY * (delta / cos2) + (nPrevY + nNextY) * common
        res.Add x
        res.Add y

    let internal handleVarOffsetCollinear (i:int, x:float, y:float, nPrevX:float, nPrevY:float, nNextX:float, nNextY:float, dPrev:float, dNext:float, res:ResizeArray<float>, idxsToFixProportional: ResizeArray<IndexToFixProportional>, projectIdxs: ResizeArray<IndexToProject>, varDistParallelBehavior: VarDistParallel) =
        match varDistParallelBehavior with
        | VarDistParallel.Fail ->
            fail $"Offset2D.offsetVariableWithDirections: point[{i}] and point[{i-1}] are collinear but have different distances {dPrev} and {dNext}."
        | VarDistParallel.Skip ->
            ()
        | VarDistParallel.Proportional ->
            idxsToFixProportional.Add  {idxRes = XY.pointCount res; idxOrig = i} // if there was a sharp U-turn with two points earlier, the the index into the originals now has an offset
            XY.add x y res // just add a dummy point , so we can edit it in place later
        | VarDistParallel.Project ->
            projectIdxs.Add {idx = XY.pointCount res; dirX = nPrevX + nNextX; dirY = nPrevY + nNextY}
            XY.add x y res // just add the original point , so we can project it later
        | VarDistParallel.StepWithTwoPoints  ->
            // add two points, one for each distance
            XY.add (x + nPrevX * dPrev) (y + nPrevY * dPrev) res
            XY.add (x + nNextX * dNext) (y + nNextY * dNext) res
        | x ->
            fail $"Offset2D.VarDistParallelBehavior: enum value {x} not recognized."


    /// <summary> Offsetting each segment by its own distance. For closed or open polylines. Adds 2 chamfer points at U-turns and skips collinear points.</summary>
    /// <remarks> This function requires precomputed segment normals; the simpler 'Offset2D.offsetVariable' uses it internally too.</remarks>
    /// <param name="xys">The points of the Polyline to offset as a flat array of coordinates.</param>
    /// <param name="nDirs">The interleaved directions or normals of the Polyline segments to offset. One vector less than xys. Must be created by counter clockwise rotation of each segment.</param>
    /// <param name="dists"> The distances to offset the Polyline. One item less than xys. Positive values will create inside offsets on counter-clockwise polylines.</param>
    /// <param name="varDistParallelBehavior"> What to do with collinear segments below 'useVarDistParallelBehaviorBelow' degrees when offset distances are different too.</param>
    /// <param name="uTurnBehavior"> What to do at a 180 degree U-turn? Fail, Chamfer with two points, or UseThreshold.</param>
    /// <param name="useVarDistParallelBehaviorBelow"> The angle between normals below which points are considered collinear and VarDistParallelBehavior is applied if distances are not the same. </param>
    /// <param name="useUTurnBehaviorAbove"> The angle between normals after which uTurnBehavior is applied .</param>
    /// <returns> A new ResizeArray of Points as a flat array of coordinates. If UTurnBehavior.Chamfer is used the total point count may be more than the input.</returns>
    let offsetVariableWithDirections(xys : ResizeArray<float>,
                                     nDirs : ResizeArray<float>,
                                     dists : Collections.Generic.IList<float>,
                                     varDistParallelBehavior : VarDistParallel,
                                     uTurnBehavior : UTurn,
                                     useVarDistParallelBehaviorBelow : float<Cosine.cosine>,
                                     useUTurnBehaviorAbove : float<Cosine.cosine>
                                     ) : ResizeArray<float> =
        XY.checkEven "offsetVariableWithDirections" xys
        XY.checkEven "offsetVariableWithDirections nDirs" nDirs
        let ptCount = XY.pointCount xys
        let nDirCount = XY.pointCount nDirs
        if ptCount < 2 then
            fail $"Offset2D.offsetVariableWithDirections:\n point count must be at least 2, but is {ptCount}."

        if ptCount <> nDirCount + 1  then
            fail $"Offset2D.offsetVariableWithDirections:\n normal directions count must be {ptCount-1} for {ptCount} points, but is {nDirCount}."
        if ptCount <> dists.Count + 1  then
            fail $"Offset2D.offsetVariableWithDirections:\n distances count must be {ptCount-1} for {ptCount} points, but is {dists.Count}."

        let res = ResizeArray<float>(xys.Count)
        let idxsToFixProportional = ResizeArray<IndexToFixProportional>() // only used if varDistParallelBehavior is Proportional
        let projectIdxs           = ResizeArray<IndexToProject>() // only used if varDistParallelBehavior is Project
        let mutable dPrev = dists.[dists.Count-1]
        let mutable nPrevX = XY.getX (nDirCount - 1) nDirs
        let mutable nPrevY = XY.getY (nDirCount - 1) nDirs

        let isOpen = XY.sqDistFirstLast xys > sqOpenTolerance
        let mutable fromIdx = 0
        if isOpen then // this is needed for open polylines that have 180 degree opposing start and end segments , see check 2.01 below
            fromIdx <- 1
            nPrevX <- XY.getX 0 nDirs
            nPrevY <- XY.getY 0 nDirs
            dPrev <- dists.[0]
            XY.add (xys.[0] + nPrevX * dPrev) (xys.[1] + nPrevY * dPrev) res

        for i = fromIdx to ptCount - 2 do // last point is skipped; it is dealt with at the end
            let nNextX = XY.getX i nDirs
            let nNextY = XY.getY i nDirs
            let dNext = dists.[i]
            let x = XY.getX i xys
            let y = XY.getY i xys
            let cosine = dot nPrevX nPrevY nNextX nNextY

            if abs (dPrev - dNext) < 1e-6 then
                // (1) both distances are the same, no line intersection needed
                if withMeasure cosine > useUTurnBehaviorAbove  then // exclude U-turns
                    // (1.1) the regular case for same distances:
                    setOffCorner(res, x, y, dPrev, nPrevX, nPrevY, nNextX, nNextY, cosine)
                else
                    // (1.2) special case: sharp U-turn, add a chamfer (two points)
                    handleUTurn (x, y, cosine, nPrevX, nPrevY, nNextX, nNextY, dPrev, res, uTurnBehavior, useUTurnBehaviorAbove)

            else
                // (2) distances are different, so we need to find the intersection of the two offset lines
                if withMeasure cosine > useUTurnBehaviorAbove || (i=0 && isOpen) then // exclude U-turns, but don't check the first point of an open polyline , ( check 2.01 )
                    if withMeasure cosine < useVarDistParallelBehaviorBelow then // check for collinear segments
                        // (2.1) the regular case for variable distances:
                        setOffCornerVar3 (res, x, y, dPrev, dNext, nPrevX, nPrevY, nNextX, nNextY, cosine) // the cheapest method, all are correct
                    else
                        // (2.2) special case: collinear segments:
                        handleVarOffsetCollinear (i, x, y, nPrevX, nPrevY, nNextX, nNextY, dPrev, dNext, res, idxsToFixProportional, projectIdxs, varDistParallelBehavior)

                else
                    // (2.3) special case: sharp U-turn with variable distances:
                    handleUTurnVarDist (x, y, cosine, nPrevX, nPrevY, nNextX, nNextY, dPrev, dNext, res, uTurnBehavior, useUTurnBehaviorAbove)

            nPrevX <- nNextX
            nPrevY <- nNextY
            dPrev <- dNext

        // (3) if the polyline is open, then we need to fix first and last point
        if isOpen then
            let nLastX = XY.getX (nDirCount - 1) nDirs
            let nLastY = XY.getY (nDirCount - 1) nDirs
            let dLast = dists.[dists.Count - 1]
            let li = (ptCount - 1) * 2
            XY.add (xys.[li] + nLastX * dLast) (xys.[li + 1] + nLastY * dLast) res
        else
            XY.add res.[0] res.[1] res // add the last point, which is the same as the first point

        // (4) now if there were collinear segments with different distances, we need to fix those points
        // first add another bad index at the end.
        // then fix collinear points either proportionally or by projection
        if idxsToFixProportional.Count > 0 then
            // if the first index is to fix, then also add the last index to fix, to close the loop
            if idxsToFixProportional.First.idxRes = 0 then
                idxsToFixProportional.Add  {idxRes = XY.pointCount res - 1; idxOrig = ptCount - 1}
            distributeProportionallyBadIdxs(res, idxsToFixProportional, xys)
        elif projectIdxs.Count > 0 then
            // if the first index is to fix, then also add the last index to fix, to close the loop
            if projectIdxs.First.idx = 0 then
                projectIdxs.Add  {idx = XY.pointCount res - 1; dirX = projectIdxs.First.dirX; dirY = projectIdxs.First.dirY}
            projectBadIdxs(res, projectIdxs)
        res


    // #endregion
    // #region Curried API



    /// <summary> A constant-distance offset algorithm for closed or open polylines.</summary>
    /// <param name="useUTurnBehaviorAbove"> The angle between normals after which the uTurnBehavior is applied.</param>
    /// <param name="uTurnBehavior"> What to do at a 180 degree U-turn? Fail, Chamfer with two points, or UseThreshold.</param>
    /// <param name="xys">The X and Y interleaved ResizeArray of the Polyline2D to offset.</param>
    /// <param name="dist">The distance to offset the Polyline2D. A positive distance will offset inwards on counter-clockwise curves.
    /// A negative distance will offset inwards on clockwise curves.</param>
    /// <returns> A new ResizeArray of Points. Due to error correction at sharp U-turns the point count may not be the same as the input. </returns>
    let offset'' (useUTurnBehaviorAbove :float<Cosine.cosine>) (uTurnBehavior:UTurn) (dist:float) (xys:ResizeArray<float>) : ResizeArray<float> =
        if abs dist < 1e-12 then
            R.clone xys// if distance is zero, just clone the polyline
        else
            offsetWithDirections(xys, makeOffsetDirections xys, dist, uTurnBehavior, useUTurnBehaviorAbove)


    /// <summary> A constant-distance offset algorithm for closed or open polylines.</summary>
    /// <param name="uTurnBehavior"> What to do at a 180 degree U-turn? Fail, Chamfer with two points, or UseThreshold.
    /// This will only be applied for joints bigger than 175° degrees.</param>
    /// <param name="xys">The X and Y interleaved ResizeArray of the Polyline2D to offset.</param>
    /// <param name="dist">The distance to offset the Polyline2D. A positive distance will offset inwards on counter-clockwise curves.
    /// A negative distance will offset inwards on clockwise curves.</param>
    /// <returns> A new ResizeArray of Points. Due to error correction at sharp U-turns the point count may not be the same as the input. </returns>
    let offset' (uTurnBehavior:UTurn) (dist:float) (xys:ResizeArray<float>) : ResizeArray<float> =
        if abs dist < 1e-12 then
            R.clone xys// if distance is zero, just clone the polyline
        else
            offsetWithDirections(xys, makeOffsetDirections xys, dist, uTurnBehavior, Cosine.``175.0``)


    /// <summary> A constant-distance offset algorithm for closed or open polylines.
    /// Fails at corners or U-turns sharper than joints after 177.5° degrees.</summary>
    /// <param name="xys">The points of the Polyline2D to offset.</param>
    /// <param name="dist">The distance to offset the Polyline2D. A positive distance will offset inwards on counter-clockwise curves.
    /// A negative distance will offset inwards on clockwise curves.</param>
    /// <returns> A new ResizeArray of Points. The point count will be the same as the input. </returns>
    let offset (dist:float) (xys:ResizeArray<float>):ResizeArray<float> =
        if abs dist < 1e-12 then
            R.clone xys// if distance is zero, just clone the polyline
        else
            offsetWithDirections(xys, makeOffsetDirections xys, dist, UTurn.Fail, Cosine.``177.5``) // default chamfer after the given angle


    /// <summary> Offsetting each segment by its own distance. For closed or open polylines.
    /// The behaviour and the limits for collinear and 180 degree U-turns are configurable.</summary>
    /// <param name="useUTurnBehaviorAbove"> The angle between normals after which, instead of a normal miter, the joint is chamfered by adding an extra point.</param>
    /// <param name="useVarDistParallelBehaviorBelow"> The angle between normals below which points are considered collinear and VarDistParallelBehavior is applied if distances are not the same. </param>
    /// <param name="uTurnBehavior"> What to do at a 180 degree U-turn? Fail, Chamfer with two points, or UseThreshold.</param>
    /// <param name="varDistParallelBehavior"> What to do with collinear segments below 'useVarDistParallelBehaviorBelow' degrees when offset distances are different too.</param>
    /// <param name="dists"> The distances to offset the Polyline. One item less than xys. Positive values will create inside offsets on counter-clockwise polylines.</param>
    /// <param name="xys">The points of the Polyline to offset.</param>
    /// <returns> A new ResizeArray of Points.
    /// Due to error correction at sharp U-turns or the picked behaviour for collinear segments
    /// the point count may not be the same as the input.</returns>
    let offsetVariable'' (useUTurnBehaviorAbove :float<Cosine.cosine>) (useVarDistParallelBehaviorBelow :float<Cosine.cosine>) (uTurnBehavior:UTurn) (varDistParallelBehavior: VarDistParallel) (dists:ResizeArray<float>) (xys:ResizeArray<float>)  =
        let nDirs = makeOffsetDirections xys
        offsetVariableWithDirections(xys, nDirs, dists, varDistParallelBehavior, uTurnBehavior, useVarDistParallelBehaviorBelow, useUTurnBehaviorAbove) // default chamfer after the given angle


    /// <summary> Offsetting each segment by its own distance.
    /// The behaviour for collinear and 180 degree U-turns is configurable.</summary>
    /// <param name="uTurnBehavior"> What to do at a 180 degree U-turn? Fail, Chamfer with two points, or UseThreshold. Will be applied for joints bigger than 175° degrees.</param>
    /// <param name="varDistParallelBehavior"> What to do with collinear segments within 2.5 degrees when offset distances are different too.</param>
    /// <param name="dists"> The distances to offset the Polyline. One item less than xys. Positive values will create inside offsets on counter-clockwise polylines.</param>
    /// <param name="xys">The points of the Polyline to offset.</param>
    /// <returns> A new ResizeArray of Points.
    /// Due to error correction at sharp U-turns or the picked behaviour for collinear segments
    /// the point count may not be the same as the input.</returns>
    let offsetVariable' (uTurnBehavior:UTurn)  (varDistParallelBehavior: VarDistParallel) (dists:ResizeArray<float>) (xys:ResizeArray<float>) =
        let nDirs = makeOffsetDirections xys
        offsetVariableWithDirections(xys, nDirs, dists, varDistParallelBehavior, uTurnBehavior, Cosine.``2.5``, Cosine.``175.0``)


    /// <summary> Offsetting each segment by its own distance. For closed or open polylines.
    /// Fails at U-turns above 175 degrees and at collinear segments within less than 2.5 degrees. </summary>
    /// <param name="dists"> The distances to offset the Polyline. One item less than xys.</param>
    /// <param name="xys">The points of the Polyline to offset. Positive values will create inside offsets on counter-clockwise polylines.</param>
    /// <returns> A new ResizeArray of Points. The point count is the same as the input.</returns>
    let offsetVariable (dists:ResizeArray<float>) (xys:ResizeArray<float>) =
        let nDirs = makeOffsetDirections xys
        offsetVariableWithDirections(xys, nDirs, dists, VarDistParallel.Fail, UTurn.Fail, Cosine.``2.5``, Cosine.``175.0``)


