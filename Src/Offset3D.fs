namespace Euclid

open System
open UtilEuclid
open EuclidErrors
open Euclid.EuclidCollectionUtilities


/// A module containing the core algorithms for offsetting 3D polylines.
/// Normally you would not use this directly; prefer the Polyline3D module.
/// Each vertex has its own normal defined by the cross product of the incoming and outgoing segment tangents.
/// These vertex normals define a local plane for each joint; within that plane we build per-segment offset directions.
module Offset3D =

    /// The tolerance for considering points to be identical in the input polyline.
    /// The value is 1e-6
    let [<Literal>] internal openTolerance = 1e-6
    /// The squared tolerance for open polylines.
    let [<Literal>] internal openToleranceSq = 1e-12

    /// <summary>Formats an interleaved XYZ buffer for diagnostics, including null, empty and incomplete buffers.</summary>
    /// <param name="xyzs">Coordinates in the order x0, y0, z0, x1, y1, z1, ...</param>
    /// <returns>A description and up to 34 points; longer buffers also show the last point.</returns>
    let internal toStringXYZs (xyzs:ResizeArray<float>) : string =
        if isNull xyzs then
            "ResizeArray<float> null"
        elif xyzs.Count = 0 then
            "Empty ResizeArray<float>"
        elif xyzs.Count = 3 then
            $"ResizeArray<float> Single Point: ({xyzs.[0]}, {xyzs.[1]}, {xyzs.[2]})"
        else
            let sb = Text.StringBuilder()
            let cnt = xyzs.Count
            let ln (line:string) = sb.AppendLine(line) |> ignore
            if cnt % 3 <> 0 then
                ln $"An invalid ResizeArray<float> Polyline with {cnt} values, which is not divisible by 3."
                for i = 0 to min (cnt - 1) 101 do
                    let axis = if i % 3 = 0 then "x" elif i % 3 = 1 then "y" else "z"
                    ln $"    {axis} {xyzs.[i]}"
                if cnt > 102 then ln $"    ... and {cnt - 102} more values."
            else
                let dx = xyzs.[0] - xyzs.[cnt-3]
                let dy = xyzs.[1] - xyzs.[cnt-2]
                let dz = xyzs.[2] - xyzs.[cnt-1]
                let dist = sqrt(dx*dx + dy*dy + dz*dz)
                if dist = 0.0 then
                    ln $"ResizeArray<float>:Closed polyline with {cnt / 3} points."
                elif dist < openTolerance then
                    ln $"ResizeArray<float>:Almost closed polyline with {cnt / 3} points, distance between first and last point is {dist}."
                else
                    ln $"ResizeArray<float>:Open polyline with {cnt / 3} points, distance between first and last point is {dist}."
                let mutable i = 0
                while i < min cnt 102 do
                    ln $"    {xyzs.[i]}  {xyzs.[i+1]}  {xyzs.[i+2]}"
                    i <- i + 3
                if cnt > 102 then
                    ln $"    ... and {cnt - 102} more values."
                    ln $"    Last point: ({xyzs.[cnt-3]}, {xyzs.[cnt-2]}, {xyzs.[cnt-1]})"
            sb.ToString()

    /// We can only offset collinear segments if the distances are the same.
    /// What shall we do if they differ?
    /// Fail?
    /// or just skip the point?
    /// or add a point at closest distance on the oblique segment, non parallel offsets?
    /// or add a point perpendicular from here intersecting the oblique segment, non parallel offsets?
    // [<RequireQualifiedAccess>]
    type VarDistParallel = // needs to be an enum , so it can be an optional parameter with default value
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

        /// Project the offset point onto the oblique segment,
        /// where it crosses the plane through the input point that is perpendicular to the polyline.
        /// For a planar polyline this moves the point along its in-plane normal, like Offset2D.VarDistParallel.Project.
        /// In shallow angles with strongly different distances the projected point may be outside of the input segments.
        /// This introduces non parallel offset segments.
        /// This keeps the total point count the same.
        /// This may create a self-intersection in the result if the projected point is outside of the segment.
        /// The Proportional method is usually better.
        | Project = 4




    /// Holds the offset directions at a vertex.
    /// `prevInPlane` is the unit vector in the local plane pointing perpendicular to the previous segment,
    /// `nextInPlane` is the unit vector in the local plane pointing perpendicular to the next segment,
    /// `perpDir` is the unit vector perpendicular to the local plane made by the adjacent segments.
    [<NoComparison; NoEquality>]
    type OffsetDirection = {
        /// The in-plane unit direction perpendicular to the previous segment.
        prevInPlane: UnitVec
        /// The in-plane unit direction perpendicular to the next segment.
        nextInPlane: UnitVec
        /// The unit direction perpendicular to the plane of the adjacent segments.
        perpDir    : UnitVec
    }

    /// For a point where the segments before and after are collinear,
    /// this structure holds the index of the point, and the indices of the previous and next
    /// points that have valid offset directions.
    /// `prevOK` may be bigger than `nextOK` if the collinear point is at the start or end of a closed polyline.
    [<NoComparison; NoEquality>]
    type CollinearPoint = {
        /// The index of the collinear point.
        idx: int
        /// The index of the preceding point with a valid offset direction.
        prevOK: int
        /// The index of the following point with a valid offset direction.
        nextOK: int
    }

    module private XYZ =

        let inline pointCount (xyzs: ResizeArray<float>) : int =
            xyzs.Count / 3


        let inline checkMod3 methodName (xyzs: ResizeArray<float>) =
            if xyzs.Count % 3 <> 0 then
                failMod3 methodName xyzs

        let inline getX i (xyzs: ResizeArray<float>) : float =
            xyzs.[i * 3]

        let inline getY i (xyzs: ResizeArray<float>) : float =
            xyzs.[i * 3 + 1]

        let inline getZ i (xyzs: ResizeArray<float>) : float =
            xyzs.[i * 3 + 2]

        let inline set i x y z (xyzs: ResizeArray<float>) : unit =
            xyzs.[i * 3    ] <- x
            xyzs.[i * 3 + 1] <- y
            xyzs.[i * 3 + 2] <- z

        let inline add x y z (xyzs: ResizeArray<float>) : unit =
            xyzs.Add x
            xyzs.Add y
            xyzs.Add z

        let sqDistFirstLast (xyzs: ResizeArray<float>) : float =
            let n = xyzs.Count
            let dx = xyzs.[0] - xyzs.[n - 3]
            let dy = xyzs.[1] - xyzs.[n - 2]
            let dz = xyzs.[2] - xyzs.[n - 1]
            dx * dx + dy * dy + dz * dz

    let inline private rayClosestParameter(fromX:float, fromY:float, fromZ:float, toX:float, toY:float, toZ:float, pX:float, pY:float, pZ:float) : float =
        let vx = toX - fromX
        let vy = toY - fromY
        let vz = toZ - fromZ
        let lenSq = vx * vx + vy * vy + vz * vz
        if isTooSmallSq lenSq then
            fail $"Offset3D.rayClosestParameter: line from ({fromX}, {fromY}, {fromZ}) to ({toX}, {toY}, {toZ}) is too short."
        ((pX - fromX) * vx + (pY - fromY) * vy + (pZ - fromZ) * vz) / lenSq

    let inline private setLinePointAt (idx:int) (fromIdx:int) (toIdx:int) (t:float) (lineXYZs: ResizeArray<float>) (res: ResizeArray<float>) : unit =
        let fromX = XYZ.getX fromIdx lineXYZs
        let fromY = XYZ.getY fromIdx lineXYZs
        let fromZ = XYZ.getZ fromIdx lineXYZs
        let x = fromX + (XYZ.getX toIdx lineXYZs - fromX) * t
        let y = fromY + (XYZ.getY toIdx lineXYZs - fromY) * t
        let z = fromZ + (XYZ.getZ toIdx lineXYZs - fromZ) * t
        XYZ.set idx x y z res

    let private projectPointToLineInPlace (idx:int) (fromIdx:int) (toIdx:int) (pointXYZs: ResizeArray<float>) (lineXYZs: ResizeArray<float>) (res: ResizeArray<float>) : unit =
        let t =
            rayClosestParameter(
                XYZ.getX fromIdx lineXYZs,
                XYZ.getY fromIdx lineXYZs,
                XYZ.getZ fromIdx lineXYZs,
                XYZ.getX toIdx lineXYZs,
                XYZ.getY toIdx lineXYZs,
                XYZ.getZ toIdx lineXYZs,
                XYZ.getX idx pointXYZs,
                XYZ.getY idx pointXYZs,
                XYZ.getZ idx pointXYZs)
        setLinePointAt idx fromIdx toIdx t lineXYZs res

    /// Places point idx where the line between the points fromIdx and toIdx in res crosses
    /// the plane through point idx of pointXYZs that is perpendicular to the tangent (tx, ty, tz).
    let private intersectLineWithNormalPlaneInPlace (idx:int) (fromIdx:int) (toIdx:int) (tx:float) (ty:float) (tz:float) (pointXYZs: ResizeArray<float>) (res: ResizeArray<float>) : unit =
        let ax = XYZ.getX fromIdx res
        let ay = XYZ.getY fromIdx res
        let az = XYZ.getZ fromIdx res
        let denom = (XYZ.getX toIdx res - ax) * tx + (XYZ.getY toIdx res - ay) * ty + (XYZ.getZ toIdx res - az) * tz
        if isTooTiny (abs denom) then
            fail $"Offset3D.offsetVariableWithDirections: the line between the offset points[{fromIdx}] and [{toIdx}] is perpendicular to the polyline at point[{idx}], so VarDistParallel.Project cannot place it."
        let t = ((XYZ.getX idx pointXYZs - ax) * tx + (XYZ.getY idx pointXYZs - ay) * ty + (XYZ.getZ idx pointXYZs - az) * tz) / denom
        setLinePointAt idx fromIdx toIdx t res res

    /// Computes the unit vectors of each segment in the polyline defined by the X, Y and Z  interleaved ResizeArray xyzs (x0, y0, z0, x1, y1, z1, ...).
    /// Fails if any two consecutive points are identical.
    /// Returns an X, Y and Z interleaved ResizeArray of the segment unit vectors, with one vector less than the number of input points.
    let getSegmentUnitVectors(xyzs: ResizeArray<float>) : ResizeArray<float> =
        XYZ.checkMod3 "getSegmentUnitVectors" xyzs
        let ptCount = XYZ.pointCount xyzs
        if ptCount < 2 then
            fail $"Offset3D.getSegmentUnitVectors: point count {ptCount} must be at least 2 for a polyline."
        let uvs = ResizeArray<float>((ptCount - 1) * 3) // the unit vectors of the segments, one less than pts, three floats each
        let mutable px = xyzs.[0]
        let mutable py = xyzs.[1]
        let mutable pz = xyzs.[2]
        for i = 1 to ptCount - 1 do
            let x = XYZ.getX i xyzs
            let y = XYZ.getY i xyzs
            let z = XYZ.getZ i xyzs
            let vx = x - px
            let vy = y - py
            let vz = z - pz
            let len = sqrt (vx * vx + vy * vy + vz * vz)
            if len < openTolerance then
                fail $"Offset3D.getSegmentUnitVectors: point[{i}] and point[{i-1}] are the same at ({px}, {py}, {pz}) in \n{toStringXYZs xyzs}."
            let f = 1.0 / len
            uvs.Add (vx * f)
            uvs.Add (vy * f)
            uvs.Add (vz * f)
            px <- x
            py <- y
            pz <- z
        uvs


    /// <summary>Removes Sharp U-Turns from an X, Y and Z interleaved ResizeArray, like Polyline3D is using.</summary>
    /// <param name="xyzs">The interleaved coordinates of the polyline.</param>
    /// <param name="uvs">The precomputed X, Y and Z interleaved segment unit vectors (one per segment, as from getSegmentUnitVectors).</param>
    /// <param name="minCos">The minimum cosine value for detecting U-turns.</param>
    /// <returns>If no U-turns are present, the original ResizeArray is returned.
    /// If U-turns are present, a new ResizeArray of coordinates is returned with the U-turns removed.</returns>
    let removeUTurns (xyzs:ResizeArray<float>, uvs: ResizeArray<float>, minCos:float<Cosine.cosine>) : ResizeArray<float> =
        XYZ.checkMod3 "removeUTurns" xyzs
        XYZ.checkMod3 "removeUTurns segment vectors" uvs
        let ptCount = XYZ.pointCount xyzs
        let nCount = XYZ.pointCount uvs
        if ptCount < 2 then
            fail $"Offset3D.removeUTurns: point count must be at least 2 but is {ptCount}."
        if ptCount <> nCount + 1 then
            fail $"Offset3D.removeUTurns: point count must be one greater than segment vector count but they are {ptCount} and {nCount}."

        let isClosed = XYZ.sqDistFirstLast xyzs < openToleranceSq

        // the cosine between two segment unit vectors a and b (both already unitized, so this is their dot product)
        let inline cosineBetween a b =
            XYZ.getX a uvs * XYZ.getX b uvs +
            XYZ.getY a uvs * XYZ.getY b uvs +
            XYZ.getZ a uvs * XYZ.getZ b uvs

        // (1) first collect all the bad indices that have U-turns:
        let bad = Array.zeroCreate<bool> ptCount
        let mutable anyBad = false

        // (1.1) check the first and last point if it's a closed polyline
        if isClosed then
            let cosine = cosineBetween (nCount - 1) 0
            let isBad = withMeasure cosine <= minCos // cos is smaller than -0.99.. (but never less than -1.0)
            bad[0]            <- isBad
            bad[bad.Length-1] <- isBad
            anyBad <- isBad

        // (1.2) the main loop
        for i = 1 to bad.Length - 2 do
            let cosine = cosineBetween (i - 1) i // cosine between the segment ending at pt and the segment starting at pt
            let isBad = withMeasure cosine <= minCos
            bad[i] <- isBad
            anyBad <- anyBad || isBad

        // (2) collect points and skip bad ones
        if anyBad then
            let res = ResizeArray<float>(xyzs.Count)
            for i = 0 to bad.Length - 1 do
                if not bad[i] then
                    XYZ.add (XYZ.getX i xyzs) (XYZ.getY i xyzs) (XYZ.getZ i xyzs) res
            // If the polyline was closed and its seam vertex (the shared start/end point) was a U-turn,
            // both bad[0] and bad[last] were skipped above, which would leave the result open.
            // Re-close it by repeating the new first point at the end so the closed property is preserved.
            if isClosed && bad[0] && res.Count >= 6 then
                XYZ.add res.[0] res.[1] res.[2] res
            res
        else
            xyzs // no bad points, return original


    /// Returns a Unit vector.
    /// Only to be used when the input vectors are known to be perpendicular.
    let inline internal crossProduct (a:UnitVec, b:UnitVec) : UnitVec =
        #if DEBUG
            if abs(a***b) > 1e-9 then
                fail $"Offset3D.crossProduct: input UnitVec are not perpendicular: dot product is {a***b}."
        #endif
            UnitVec.createUnchecked (a.Y * b.Z - a.Z * b.Y, a.Z * b.X - a.X * b.Z, a.X * b.Y - a.Y * b.X)


    /// The offset direction at a point that is not a corner, derived from a given perpendicular direction.
    /// The perpendicular direction is made perpendicular to the tangent at the point,
    /// and the in-plane directions are perpendicular to it and to each segment.
    /// Fails if the given direction is parallel to the tangent.
    let private directionFromPerp (perp:UnitVec) (uPrev:UnitVec) (uNext:UnitVec) (ptIdx:int) : OffsetDirection =
        let tx = uPrev.X + uNext.X // the tangent, not unitized, its length is almost 2.0 because U-turns are excluded
        let ty = uPrev.Y + uNext.Y
        let tz = uPrev.Z + uNext.Z
        let tLenSq = tx * tx + ty * ty + tz * tz
        if isTooTinySq tLenSq then
            fail $"Offset3D.getOffsetDirections: the segments before and after pts.[{ptIdx}] are opposite: {uPrev} and {uNext}."
        let k = (perp.X * tx + perp.Y * ty + perp.Z * tz) / tLenSq
        let px = perp.X - tx * k
        let py = perp.Y - ty * k
        let pz = perp.Z - tz * k
        let pLen = sqrt (px * px + py * py + pz * pz)
        if isTooSmall pLen then
            fail $"Offset3D.getOffsetDirections: the reference normal {perp} is parallel to the polyline at pts.[{ptIdx}], so it cannot define the offset plane there."
        let p = UnitVec.createUnchecked(px / pLen, py / pLen, pz / pLen)
        let nPrev = UnitVec.cross(p, uPrev) |> Vec.unitize
        let nNext = UnitVec.cross(p, uNext) |> Vec.unitize
        { prevInPlane = nPrev; nextInPlane = nNext; perpDir = p }


    /// <summary>Computes the offset directions at each point of a polyline from its segment unit vectors.</summary>
    /// <remarks>
    /// At a corner the local plane contains both adjacent segments, and its normal is their cross product.
    /// Each normal is flipped to match the previous one, starting with refNormal, so that the inside stays on the same side along the polyline.
    /// A point is collinear if the cosine of the angle between its two segments is bigger than considerCollinearBelow.
    /// Collinear points get ValueNone. The offset functions place them on the line between the neighboring offset corners.
    /// The end points of an open polyline get the plane normal of the nearest corner, made perpendicular to their own segment.
    /// For a closed polyline the first and last points get the same direction, which is ValueNone if the seam is collinear.
    /// If no point is a corner, for example on a straight or a finely sampled curve, every point gets a direction derived from refNormal instead:
    /// the perpendicular direction is refNormal made perpendicular to the polyline, and the in-plane directions are perpendicular to it.
    /// </remarks>
    /// <param name="uvs">The X, Y and Z interleaved segment unit vectors, as from getSegmentUnitVectors.</param>
    /// <param name="refNormal">The reference normal that orients the first corner, or all points if there is no corner.</param>
    /// <param name="isOpen">TRUE for an open polyline, FALSE for a closed one, where the first and last point are the same.</param>
    /// <param name="considerCollinearBelow">Points where the segments turn by less than this angle are collinear.</param>
    /// <param name="failAtUTurnAbove">Fails at points where the segments turn by more than this angle.</param>
    /// <returns>One offset direction per point, ValueNone for collinear points.</returns>
    let getOffsetDirections(uvs: ResizeArray<float>, refNormal:UnitVec, isOpen:bool, considerCollinearBelow: float<Cosine.cosine>, failAtUTurnAbove :float<Cosine.cosine>) : ResizeArray<OffsetDirection voption> =
        let segCount = XYZ.pointCount uvs // number of segment unit vectors, three floats each
        let ptsCount = segCount + 1
        let dirs = ResizeArray<OffsetDirection voption>(ptsCount)
        let inline getUV i = UnitVec.createUnchecked(XYZ.getX i uvs, XYZ.getY i uvs, XYZ.getZ i uvs)

        // (1) setup
        let mutable fromIdx = 0
        let mutable uPrev = getUV (segCount - 1)
        if isOpen then // this is needed for open polylines that have 180 degree opposing start and end segments
            dirs.Add ValueNone // the start point is set at the end
            fromIdx <- 1
            uPrev <- getUV 0


        let mutable perp  = refNormal
        let mutable nNext = UnitVec.zAxis // dummy value
        let mutable nPrev = UnitVec.zAxis // dummy value
        let mutable firstCornerIdx = -1
        let mutable firstCornerPerp = refNormal
        let mutable firstCornerNPrev = UnitVec.zAxis // dummy value
        let mutable lastCornerIdx = -1

        //(2) compute the cross product of the segment unit vectors to get the vertex normals, and offset points
        for i = fromIdx to ptsCount - 2 do // last point is skipped; it's dealt with at the end
            let uNext = getUV i
            let dot   = UnitVec.dot uPrev uNext
            if withMeasure dot < failAtUTurnAbove then
                fail $"Offset3D.getOffsetDirections: {Cosine.inDegrees (withMeasure dot)} degree U-turn detected at pts.[{i}]. exceeding failAtUTurnAbove: {Cosine.inDegrees failAtUTurnAbove}."

            elif withMeasure dot > considerCollinearBelow then
                // collinear case: no offset direction defined yet, will be computed in later functions
                dirs.Add ValueNone

            else
                match UnitVec.cross(uPrev, uNext) |> Vec.tryUnitize with
                | ValueNone ->
                    // the segments are parallel, only possible with extreme thresholds
                    if dot > 0.0 then
                        dirs.Add ValueNone // collinear
                    else
                        fail $"Offset3D.getOffsetDirections: 180 degree U-turn detected at pts.[{i}]."
                | ValueSome cross ->
                    // normal case: offset the corner now
                    perp  <- cross |> UnitVec.matchOrientation perp // use previous perp to keep consistent relative orientation, not always the same refNormal, so it can adapt along the curve, eg. slowly rotate about the uNext axis
                    nNext <- crossProduct (perp,  uNext) // they are perpendicular so the result is already unitized
                    nPrev <- crossProduct (perp, uPrev) // they are perpendicular so the result is already unitized
                    dirs.Add (ValueSome { prevInPlane = nPrev; nextInPlane = nNext; perpDir = perp })
                    if firstCornerIdx = -1 then
                        firstCornerIdx   <- i
                        firstCornerPerp  <- perp
                        firstCornerNPrev <- nPrev
                    lastCornerIdx <- i

            uPrev <- uNext


        // (3) do last point and checks:
        if firstCornerIdx = -1 then
            // no corner, all points are collinear: derive every direction from refNormal
            dirs.Clear()
            let lastUnique = if isOpen then ptsCount - 1 else ptsCount - 2
            for i = 0 to lastUnique do
                let uP = if i > 0 then getUV (i - 1) elif isOpen then getUV 0 else getUV (segCount - 1)
                let uN = if i < segCount then getUV i else getUV (segCount - 1)
                dirs.Add (ValueSome (directionFromPerp refNormal uP uN i))
            if not isOpen then
                dirs.Add dirs.[0] // closed polyline, last point = first point

        elif isOpen then
            // the end points use the plane of the nearest corner, made perpendicular to their own segment
            dirs.[0] <-
                if firstCornerIdx = 1 then
                    ValueSome { prevInPlane = firstCornerNPrev; nextInPlane = firstCornerNPrev; perpDir = firstCornerPerp } // uses nPrev twice
                else
                    let u0 = getUV 0
                    ValueSome (directionFromPerp firstCornerPerp u0 u0 0)
            if lastCornerIdx = ptsCount - 2 then
                dirs.Add <| ValueSome { prevInPlane = nNext; nextInPlane = nNext; perpDir = perp } // uses nNext twice
            else
                let uLast = getUV (segCount - 1)
                dirs.Add <| ValueSome (directionFromPerp perp uLast uLast (ptsCount - 1))

        else
            // closed polyline, last point = first offset point
            dirs.Add dirs.[0] // this might be ValueNone if collinear

        dirs


    /// The core offset Algorithm for constant distance offsets.
    /// Writes the offset point into the result buffer based on the previous and next normals,
    /// the distance, and the precomputed cosine (= dot product of nPrev * nNext).
    /// res.Add ((nPrev + nNext) * (dist / (1.0 + cosine)) + perpDir * distPerp + pt)
    let inline setOffCorner (res:ResizeArray<float>, x:float, y:float, z:float, distInPlane:float, distPerp:float, nPrev:UnitVec, nNext:UnitVec, perpDir:UnitVec, cosine:float) : unit =
        #if DEBUG || CHECKED_EUCLID // CHECKED_EUCLID so checks can still be enabled when using with Fable release mode
            if cosine < -0.9999999 then
                fail $"Offset3D.setOffCorner: cosine is {cosine} , a 180 degree U-turn, which is not allowed here for distInPlane {distInPlane}, nPrev {nPrev}, nNext {nNext} at pt ({x}, {y}, {z})."
        #endif
            // based on https://www.angusj.com/clipper2/Docs/Trigonometry.htm
            let k = distInPlane / (1.0 + cosine)
            res.Add (x + (nPrev.X + nNext.X) * k + perpDir.X * distPerp)
            res.Add (y + (nPrev.Y + nNext.Y) * k + perpDir.Y * distPerp)
            res.Add (z + (nPrev.Z + nNext.Z) * k + perpDir.Z * distPerp)


    /// offset point calculation for variable distances:
    let inline setOffCornerVar (res:ResizeArray<float>, x:float, y:float, z:float, distInPlanePrev:float, distInPlaneNext:float, distPerp:float, nPrev:UnitVec, nNext:UnitVec, perpDir:UnitVec, cosine:float, vNext:UnitVec) : unit =
        let delta = distInPlanePrev - distInPlaneNext
        let cos2 = nPrev *** vNext // never 0.0 here, because vectors are already checked to be not collinear
        let a = delta / cos2
        let common = distInPlaneNext / (1.0 + cosine)
        res.Add (x + vNext.X * a + (nPrev.X + nNext.X) * common + perpDir.X * distPerp)
        res.Add (y + vNext.Y * a + (nPrev.Y + nNext.Y) * common + perpDir.Y * distPerp)
        res.Add (z + vNext.Z * a + (nPrev.Z + nNext.Z) * common + perpDir.Z * distPerp)



    /// For every marked point, finds the nearest points before and after it that are not marked.
    /// Works also for closed polylines where initial and end segments are collinear, the search wraps around the seam.
    let internal getNeighborsOfMarked (marked: bool[]) : ResizeArray<CollinearPoint> =
        let mutable prevIdx = -1
        let mutable nextIdx = -1
        let mutable j = -1
        let collinearPts = ResizeArray<CollinearPoint>()
        let count = marked.Length

        for i = 0 to count - 1 do
            if marked.[i] then
                // check if we need to find new line from prev and next, to project onto
                if nextIdx < i then
                    // (2.1.1) find the previous defined direction
                    j <-  i - 1
                    while j >= 0 do
                        if marked.[j] then
                            j <- j - 1 // continue search
                        else
                            prevIdx <- j
                            j <- -99 // found one, exit loop, use any value smaller than -1, if none is found the loop will exit at j = -1
                    // (2.1.2) check why the loop exited:
                    if j = -1 then
                        // we are at the start of a closed polyline ,nothing found yet, so need to search backwards from the end
                        j <- count - 1
                        while j >= 0 do
                            if marked.[j] then
                                j <- j - 1 // continue search
                            else
                                prevIdx <- j
                                j <- -999 // found one, exit loop, use any value smaller than -1, if none is found the loop will exit at j = -1

                        if j = -1 then // no point has a valid direction
                            fail $"Offset3D.getCollinearNeighbors: could not find a previous point with a valid offset direction for collinear point[{i}], none of the {count} points has one."


                    // (2.2.1) find the next defined direction
                    j <- i + 1
                    while j < count do
                        if marked.[j] then
                            j <- j + 1 // continue search
                        else
                            nextIdx <- j
                            j <- 999_999_999 // found one, exit loop, use any value bigger than count, if none is found the loop will exit at j = count
                    // (2.2.2) check why the loop exited:
                    if j = count then
                        // we are at the end of a closed polyline , nothing found yet, so we need to search forwards from the start
                        j <- 0
                        while j < count do
                            if marked.[j] then
                                j <- j + 1 // continue search
                            else
                                nextIdx <- j
                                j <- 1_000_000_000 //found one, exit loop,, use any value bigger than count, if none is found the loop will exit at j = count

                        if j = count then // no point has a valid direction
                            fail $"Offset3D.getCollinearNeighbors: could not find a next point with a valid offset direction for collinear point[{i}], none of the {count} points has one."

                // (2.3) add the collinear point with its prev and next OK indices even if prevIdx and nextIdx were found earlier
                collinearPts.Add { idx = i; prevOK = prevIdx; nextOK = nextIdx }

        collinearPts

    /// For all dirs that are ValueNone this will find the index of adjacent points that are OK.
    /// Works also for closed polylines where initial and end segments are collinear
    let getCollinearNeighbors(dirs: ResizeArray<OffsetDirection voption>) : ResizeArray<CollinearPoint> =
        let marked = Array.zeroCreate<bool> dirs.Count
        for i = 0 to dirs.LastIndex do
            marked.[i] <- dirs.[i].IsNone
        getNeighborsOfMarked marked

    /// <summary>Offsets a polyline by a constant distance.</summary>
    /// <param name="xyzs">The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.</param>
    /// <param name="dirs">The offset directions at each point of the polyline.</param>
    /// <param name="distInPlane">The distance to offset in the local plane of each vertex.</param>
    /// <param name="distPerpendicular">The distance to offset perpendicular to the local plane of each vertex.</param>
    /// <returns>The offset points as a ResizeArray of interleaved coordinates.</returns>
    let offsetWithDirections(xyzs:ResizeArray<float>, dirs: ResizeArray<OffsetDirection voption>, distInPlane:float, distPerpendicular:float) : ResizeArray<float> =
        XYZ.checkMod3 "offsetWithDirections" xyzs
        let ptCount = XYZ.pointCount xyzs
        if ptCount < 2 then
            fail $"Offset3D.offsetWithDirections: point count {ptCount} must be at least 2 for a polyline."

        if ptCount <> dirs.Count then
            fail $"Offset3D.offsetWithDirections: point count must equal offset directions count, but they are {ptCount} and {dirs.Count}."

        let res = ResizeArray<float>(xyzs.Count)
        let mutable needsSecondPass = false

        // (1) first pass: offset points that have valid directions
        for i = 0 to ptCount - 1 do
            let x = XYZ.getX i xyzs
            let y = XYZ.getY i xyzs
            let z = XYZ.getZ i xyzs
            match dirs.[i] with
            | ValueNone ->
                needsSecondPass <- true
                XYZ.add x y z res // for now just keep the input point, will be projected in second pass
            | ValueSome dir ->
                let cosine = dir.prevInPlane *** dir.nextInPlane
                setOffCorner(res, x, y, z, distInPlane, distPerpendicular, dir.prevInPlane, dir.nextInPlane, dir.perpDir, cosine)

        // (2) second pass: project collinear points
        if needsSecondPass then
            let collinearPoints = getCollinearNeighbors(dirs)
            for i = 0 to collinearPoints.LastIndex do
                let cP = collinearPoints.[i]
                projectPointToLineInPlace cP.idx cP.prevOK cP.nextOK xyzs res res

        res

    /// <summary>Offsets a polyline by variable distances.</summary>
    /// <remarks>Collinear points, where the segments turn by less than considerCollinearBelow, are placed on the line between the neighboring offset corners.
    /// Where the in-plane distances of their two segments are equal, they are placed at the closest point to the input point.
    /// Where these distances differ, varDistParallelBehavior decides.</remarks>
    /// <param name="xyzs">The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.</param>
    /// <param name="segmentDirs">The unit vectors of each segment in the polyline.</param>
    /// <param name="offDirs">The offset directions at each point of the polyline.</param>
    /// <param name="distsInPlane">The offset distances in the local plane, one per segment.
    /// Each segment is offset parallel to itself by its own distance.</param>
    /// <param name="distsPerpendicular">The offset distances perpendicular to the local plane, one per segment.
    /// Each point takes the distance of the segment that starts at it, the last point of an open polyline that of the last segment.
    /// So a segment is offset by its own distance at its start and by the next segment's distance at its end, and reversing the polyline changes the result.
    /// Collinear points take the perpendicular offset of the line between the neighboring offset corners instead.</param>
    /// <param name="isClosed">Whether the polyline is closed (TRUE) or open (FALSE).</param>
    /// <param name="varDistParallelBehavior">The behavior to use at collinear points where the two segments have different in-plane distances.</param>
    /// <param name="considerCollinearBelow">Points where the segments turn by less than this angle are collinear.
    /// Use the same value as for getOffsetDirections.</param>
    /// <returns>The offset points as a ResizeArray of interleaved coordinates.
    /// With VarDistParallel.Skip there may be fewer points than in the input.</returns>
    let offsetVariableWithDirections( xyzs:ResizeArray<float>,
                                    segmentDirs:ResizeArray<float>,
                                    offDirs: ResizeArray<OffsetDirection voption>,
                                    distsInPlane:Collections.Generic.IList<float>,
                                    distsPerpendicular:Collections.Generic.IList<float>,
                                    isClosed:bool,
                                    varDistParallelBehavior: VarDistParallel,
                                    considerCollinearBelow: float<Cosine.cosine>
                                    ) : ResizeArray<float> =
        XYZ.checkMod3 "offsetVariableWithDirections" xyzs
        XYZ.checkMod3 "offsetVariableWithDirections segment directions" segmentDirs
        let ptCount = XYZ.pointCount xyzs
        let segCount = XYZ.pointCount segmentDirs // number of segment unit vectors, three floats each
        let inline getSegUV i = UnitVec.createUnchecked(XYZ.getX i segmentDirs, XYZ.getY i segmentDirs, XYZ.getZ i segmentDirs)
        if ptCount < 2 then
            fail $"Offset3D.offsetVariableWithDirections: point count {ptCount} must be at least 2 for a polyline."

        if ptCount <> segCount + 1  then
            fail $"Offset3D.offsetVariableWithDirections:\n Segment directions count must be {ptCount-1} for {ptCount} points, but is {segCount}."

        if ptCount <> offDirs.Count then
            fail $"Offset3D.offsetVariableWithDirections:\n offset directions count must be {ptCount} for {ptCount} points, but is {offDirs.Count}."

        if ptCount <> distsInPlane.Count + 1  then
            fail $"Offset3D.offsetVariableWithDirections:\n in-plane distances count must be {ptCount-1} for {ptCount} points, but is {distsInPlane.Count}."

        if ptCount <> distsPerpendicular.Count + 1  then
            fail $"Offset3D.offsetVariableWithDirections:\n perpendicular distances count must be {ptCount-1} for {ptCount} points, but is {distsPerpendicular.Count}."


        let mutable res = ResizeArray<float>(xyzs.Count)
        let lastSeg = segCount - 1
        let needsFix = Array.zeroCreate<bool> ptCount // the collinear points that are placed in the second pass
        let mutable fixCount = 0

        // the end points of an open polyline take the distances of their own segment
        let inline nextDistAt i = distsInPlane.[min i lastSeg]
        let inline prevDistAt i = if i > 0 then distsInPlane.[i - 1] elif isClosed then distsInPlane.[lastSeg] else distsInPlane.[0]

        // the segments at a point turn by less than considerCollinearBelow, never TRUE at the end points of an open polyline
        let isCollinear i =
            if not isClosed && (i = 0 || i = ptCount - 1) then
                false
            else
                let a = if i > 0 then i - 1 else lastSeg
                let dot = XYZ.getX a segmentDirs * XYZ.getX i segmentDirs + XYZ.getY a segmentDirs * XYZ.getY i segmentDirs + XYZ.getZ a segmentDirs * XYZ.getZ i segmentDirs
                withMeasure dot > considerCollinearBelow

        // (1) first pass: offset points that have valid directions
        // the last point of a closed polyline is the first point, it is added below
        let lastPt = if isClosed then ptCount - 2 else ptCount - 1
        for i = 0 to lastPt do
            let x = XYZ.getX i xyzs
            let y = XYZ.getY i xyzs
            let z = XYZ.getZ i xyzs
            let seg = min i lastSeg // the segment starting at this point, or the last segment for the last point
            let nextDistInPlane = nextDistAt i
            let prevDistInPlane = prevDistAt i
            let distPerp = distsPerpendicular.[seg]
            let sameDist = abs (prevDistInPlane - nextDistInPlane) < 1e-6
            match offDirs.[i] with
            | ValueSome dir when sameDist ->
                // same distances, also at the end points of an open polyline, where prevInPlane and nextInPlane are the same
                let cosine = dir.prevInPlane *** dir.nextInPlane
                setOffCorner(res, x, y, z, nextDistInPlane, distPerp, dir.prevInPlane, dir.nextInPlane, dir.perpDir, cosine)
            | ValueSome dir when not (isCollinear i) ->
                let cosine = dir.prevInPlane *** dir.nextInPlane
                setOffCornerVar(res, x, y, z, prevDistInPlane, nextDistInPlane, distPerp, dir.prevInPlane, dir.nextInPlane, dir.perpDir, cosine, getSegUV seg)
            | _ ->
                // a collinear point without direction, or with different distances, so the corner would be far away or at infinity
                needsFix.[i] <- true
                fixCount <- fixCount + 1
                XYZ.add x y z res // for now just keep the input point, will be projected in second pass

        // (1.1) the last point of a closed polyline is the first point, a collinear one is placed in the second pass
        if isClosed then
            needsFix.[ptCount - 1] <- needsFix.[0]
            XYZ.add res.[0] res.[1] res.[2] res


        // (2) second pass: place the collinear points on the line between the neighboring offset corners
        if fixCount > 0 then
            if fixCount = lastPt + 1 then // only possible for a closed polyline
                fail $"Offset3D.offsetVariableWithDirections: the polyline has no corner to place its {fixCount} collinear points between."
            let skip = Array.zeroCreate<bool> ptCount // only used with VarDistParallel.Skip
            let collinearPoints = getNeighborsOfMarked needsFix
            for j = 0 to collinearPoints.LastIndex do
                let cP = collinearPoints.[j]
                let i = cP.idx
                if not (isClosed && i = ptCount - 1) then // the last point of a closed polyline is copied from the first one below
                    let prevDist = prevDistAt i
                    let nextDist = nextDistAt i
                    if abs (prevDist - nextDist) < 1e-6 then
                        // same distances, the line between the neighbors is the offset line
                        projectPointToLineInPlace i cP.prevOK cP.nextOK xyzs res res
                    else
                        match varDistParallelBehavior with
                        | VarDistParallel.Fail ->
                            fail $"Offset3D.offsetVariableWithDirections: point[{i}] is between collinear segments with the different in-plane distances {prevDist} and {nextDist}. Use another VarDistParallel behavior than Fail."

                        | VarDistParallel.Skip ->
                            skip.[i] <- true

                        | VarDistParallel.Proportional ->
                            // at the same parameter as on the line between the neighboring input points
                            let t =
                                rayClosestParameter(
                                    XYZ.getX cP.prevOK xyzs,
                                    XYZ.getY cP.prevOK xyzs,
                                    XYZ.getZ cP.prevOK xyzs,
                                    XYZ.getX cP.nextOK xyzs,
                                    XYZ.getY cP.nextOK xyzs,
                                    XYZ.getZ cP.nextOK xyzs,
                                    XYZ.getX i xyzs,
                                    XYZ.getY i xyzs,
                                    XYZ.getZ i xyzs)
                            setLinePointAt i cP.prevOK cP.nextOK t res res

                        | VarDistParallel.Project ->
                            // where the line between the neighboring offset points crosses the plane through the input point perpendicular to the tangent,
                            // so for planar polylines along the in-plane normal, like in Offset2D
                            let a = if i > 0 then i - 1 else lastSeg
                            let tx = XYZ.getX a segmentDirs + XYZ.getX i segmentDirs
                            let ty = XYZ.getY a segmentDirs + XYZ.getY i segmentDirs
                            let tz = XYZ.getZ a segmentDirs + XYZ.getZ i segmentDirs
                            intersectLineWithNormalPlaneInPlace i cP.prevOK cP.nextOK tx ty tz xyzs res

                        |_  ->
                            fail $"Offset3D.offsetVariableWithDirections: unknown VarDistParallel behavior {varDistParallelBehavior}."

            if isClosed then
                XYZ.set (ptCount - 1) res.[0] res.[1] res.[2] res

            if varDistParallelBehavior = VarDistParallel.Skip then
                if isClosed then
                    skip.[ptCount - 1] <- skip.[0]
                let newRes = ResizeArray<float>(res.Count)
                for i = 0 to ptCount - 1 do
                    if not skip.[i] then
                        XYZ.add (XYZ.getX i res) (XYZ.getY i res) (XYZ.getZ i res) newRes
                // if the seam point was skipped, close the polyline again with the new first point
                if isClosed && skip.[0] then
                    XYZ.add newRes.[0] newRes.[1] newRes.[2] newRes
                res <- newRes

        res

    /// Returns the average normal vector of the polyline defined by the X, Y and Z interleaved ResizeArray xyzs.
    /// It is calculated by summing up the cross products of all segments around the center point.
    /// Does not check for bad input, may be zero length if points are collinear.
    let internal averageNormal(xyzs: ResizeArray<float>) : Vec =
        let ptCount = XYZ.pointCount xyzs
        let mutable cx = 0.0
        let mutable cy = 0.0
        let mutable cz = 0.0
        for i = 0 to ptCount - 1 do
            cx <- cx + XYZ.getX i xyzs
            cy <- cy + XYZ.getY i xyzs
            cz <- cz + XYZ.getZ i xyzs
        let f = 1.0 / float ptCount
        let cX = cx * f
        let cY = cy * f
        let cZ = cz * f
        let mutable normal = Vec.Zero
        let li = ptCount - 1
        let mutable ax = XYZ.getX li xyzs - cX
        let mutable ay = XYZ.getY li xyzs - cY
        let mutable az = XYZ.getZ li xyzs - cZ
        for i = 0 to ptCount - 1 do
            let bx = XYZ.getX i xyzs - cX
            let by = XYZ.getY i xyzs - cY
            let bz = XYZ.getZ i xyzs - cZ
            normal <- Vec(normal.X + ay * bz - az * by,
                          normal.Y + az * bx - ax * bz,
                          normal.Z + ax * by - ay * bx)
            ax <- bx
            ay <- by
            az <- bz
        normal

    // #endregion
    // #region Curried API

    /// <summary> A constant-distance offset algorithm for closed or open polylines in 3D.
    /// Uses default angles: collinear below 2.5 degrees, fails at U-turns above 175 degrees.</summary>
    /// <param name="refNormal">The reference normal vector to use for computing the local planes.
    /// For a counter-clockwise polyline in the XY plane this is upwards. If it points down, the offset direction is flipped.</param>
    /// <param name="distInPlane">The offset distance in the local plane defined by two segments.
    /// A positive distance offsets to the inside of the polyline.
    /// A negative distance offsets to the outside.
    /// No matter how the polyline is oriented.</param>
    /// <param name="distPerpendicular">The offset distance perpendicular to the local plane.
    /// A positive distance offsets in the direction of the computed normal.
    /// For a counter-clockwise polyline in xy-plane this is Upwards.
    /// A negative distance offsets in the opposite direction.</param>
    /// <param name="xyzs"> The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.</param>
    /// <returns> A new ResizeArray of interleaved coordinates.</returns>
    let offsetByNormal (refNormal:UnitVec) (distInPlane: float) (distPerpendicular: float) (xyzs:ResizeArray<float>): ResizeArray<float> =
        let isOpen = XYZ.sqDistFirstLast xyzs > openToleranceSq
        let uvs = getSegmentUnitVectors xyzs
        let dirs = getOffsetDirections(uvs, refNormal, isOpen, Cosine.``2.5`` , Cosine.``175.0``)
        offsetWithDirections(xyzs, dirs, distInPlane, distPerpendicular)


    /// <summary> A constant-distance offset algorithm for closed or open polylines in 3D.
    /// Uses default angles: collinear below 2.5 degrees, fails at U-turns above 175 degrees.</summary>
    /// <param name="distInPlane">The offset distance in the local plane defined by two segments.
    /// A positive distance offsets to the inside of the polyline.
    /// A negative distance offsets to the outside.
    /// No matter how the polyline is oriented.</param>
    /// <param name="distPerpendicular">The offset distance perpendicular to the local plane.
    /// A positive distance offsets in the direction of the computed normal.
    /// For a counter-clockwise polyline in xy-plane this is Upwards.
    /// A negative distance offsets in the opposite direction.</param>
    /// <param name="xyzs"> The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.</param>
    /// <returns> A new ResizeArray of interleaved coordinates.</returns>
    let offset (distInPlane: float) (distPerpendicular: float) (xyzs: ResizeArray<float>) : ResizeArray<float> =
        let refNormal = averageNormal(xyzs) |> Vec.unitize
        offsetByNormal refNormal distInPlane distPerpendicular xyzs


    /// <summary> Offsetting each segment by its own distance. For closed or open polylines in 3D, with all settings given explicitly.
    /// Note that considerCollinearBelow comes before failAtUTurnAbove, both are cosine measures.</summary>
    /// <param name="xyzs"> The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.</param>
    /// <param name="distancesInPlane">The offset distances in the local plane defined by two segments.
    /// A positive distance offsets to the inside of the polyline.
    /// A negative distance offsets to the outside.
    /// No matter how the polyline is oriented.</param>
    /// <param name="distancesPerpendicular">The offset distances perpendicular to the local plane.
    /// A positive distance offsets in the direction of the computed normal.
    /// For a counter-clockwise polyline in xy-plane this is Upwards.
    /// A negative distance offsets in the opposite direction.</param>
    /// <param name="refNormal">The reference normal vector to use for computing the local planes.
    /// For a counter-clockwise polyline in the XY plane this is upwards. If it points down, the offset direction is flipped.</param>
    /// <param name="varDistParallelBehavior">The behavior to use at collinear points where the two segments have different in-plane distances.</param>
    /// <param name="considerCollinearBelow">The cosine of the angle below which segments are considered collinear, the other functions use 2.5 degrees.</param>
    /// <param name="failAtUTurnAbove">The cosine of the angle above which a U-turn is considered to fail, the other functions use 175 degrees.</param>
    /// <returns> A new ResizeArray of interleaved coordinates. If collinear segment handling is set to Skip the point count may differ from input.</returns>
    let offsetVariableByNormalCustom (xyzs: ResizeArray<float>,
                                      distancesInPlane: ResizeArray<float>,
                                      distancesPerpendicular: ResizeArray<float>,
                                      refNormal:UnitVec,
                                      varDistParallelBehavior: VarDistParallel,
                                      considerCollinearBelow: float<Cosine.cosine>,
                                      failAtUTurnAbove: float<Cosine.cosine>) : ResizeArray<float> =
        let isOpen = XYZ.sqDistFirstLast xyzs > openToleranceSq
        let uvs = getSegmentUnitVectors xyzs
        let dirs = getOffsetDirections(uvs, refNormal, isOpen, considerCollinearBelow, failAtUTurnAbove)
        offsetVariableWithDirections(xyzs, uvs, dirs, distancesInPlane, distancesPerpendicular, not isOpen, varDistParallelBehavior, considerCollinearBelow)


    /// <summary> Offsetting each segment by its own distance. For closed or open polylines in 3D.
    /// Fails if collinear segments with different offset distances are found (uses VarDistParallel.Fail).
    /// Uses default angles: collinear below 2.5 degrees, fails at U-turns above 175 degrees.</summary>
    /// <param name="refNormal">The reference normal vector to use for computing the local planes.
    /// For a counter-clockwise polyline in the XY plane this is upwards. If it points down, the offset direction is flipped.</param>
    /// <param name="distancesInPlane">The offset distances in the local plane defined by two segments.</param>
    /// <param name="distancesPerpendicular">The offset distances perpendicular to the local plane.</param>
    /// <param name="xyzs"> The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.</param>
    /// <returns> A new ResizeArray of interleaved coordinates.</returns>
    let offsetVariableByNormal (refNormal:UnitVec) (distancesInPlane: ResizeArray<float>) (distancesPerpendicular: ResizeArray<float>) (xyzs: ResizeArray<float>) : ResizeArray<float> =
        offsetVariableByNormalCustom(xyzs, distancesInPlane, distancesPerpendicular, refNormal, VarDistParallel.Fail, Cosine.``2.5``, Cosine.``175.0``)


    /// <summary> Offsetting each segment by its own distance. For closed or open polylines in 3D.
    /// Fails if collinear segments are found (uses VarDistParallel.Fail).
    /// Uses default angles: collinear below 2.5 degrees, fails at U-turns above 175 degrees.</summary>
    /// <param name="distancesInPlane">The offset distances in the local plane defined by two segments.
    /// A positive distance offsets to the inside of the polyline.
    /// A negative distance offsets to the outside.
    /// No matter how the polyline is oriented.</param>
    /// <param name="distancesPerpendicular">The offset distances perpendicular to the local plane.
    /// A positive distance offsets in the direction of the computed normal.
    /// For a counter-clockwise polyline in xy-plane this is Upwards.
    /// A negative distance offsets in the opposite direction.</param>
    /// <param name="xyzs"> The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.</param>
    /// <returns> A new ResizeArray of interleaved coordinates.</returns>
    let offsetVariable (distancesInPlane: ResizeArray<float>) (distancesPerpendicular: ResizeArray<float>) (xyzs: ResizeArray<float>) : ResizeArray<float> =
        let refNormal = averageNormal(xyzs) |> Vec.unitize
        offsetVariableByNormal refNormal distancesInPlane distancesPerpendicular xyzs

