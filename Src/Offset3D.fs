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
    let [<Literal>] openTolerance = 1e-6
    /// The squared tolerance for open polylines.
    let [<Literal>] sqOpenTolerance = 1e-12

    /// We can only offset colinear segments if the distances are the same.
    /// What shall we do if they differ?
    /// Fail?
    /// or just skip the point?
    /// or add a point at closest distance on the oblique segment, non parallel offsets?
    /// or add a point perpendicular from here intersecting the oblique segment, non parallel offsets?
    // [<RequireQualifiedAccess>]
    type VarDistParallel = // needs to be an enum , so it can be an optional parameter with default value
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




    /// Holds the offset directions at a vertex.
    /// `prevInPlane` is the unit vector in the local plane pointing perpendicular to the previous segment,
    /// `nextInPlane` is the unit vector in the local plane pointing perpendicular to the next segment,
    /// `perpDir` is the unit vector perpendicular to the local plane made by the adjacent segments.
    [<NoComparison; NoEquality>]
    type OffsetDirection = {
        prevInPlane: UnitVec
        nextInPlane: UnitVec
        perpDir    : UnitVec
    }

    /// For a point where the segments before and after are colinear,
    /// this structure holds the index of the point, and the indices of the previous and next
    /// points that have valid offset directions.
    /// `prevOK` may be bigger than `nextOK` if the colinear point is at the start or end of a closed polyline.
    [<NoComparison; NoEquality>]
    type ColinearPoint = {
        idx: int
        prevOK: int
        nextOK: int
    }

    module private XYZ =

        let inline pointCount (xyzs: ResizeArray<float>) : int =
            xyzs.Count / 3

        let failMod3 methodName (xyzs: ResizeArray<float>) =
            fail $"Offset3D.{methodName}: coordinate buffer must contain a multiple of 3 floats, but has {xyzs.Count} values."

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
                fail $"Offset3D.getSegmentUnitVectors: point[{i}] and point[{i-1}] are the same at ({px}, {py}, {pz})."
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

        let isClosed = XYZ.sqDistFirstLast xyzs < sqOpenTolerance

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


    /// For points between colinear segments, ValueNone is returned.
    /// For a open polyline, the first and last points will be ValueSome,
    /// For a closed polyline, the first and last points will be identical, and they may be ValueNone if colinear.
    /// Segments are considered colinear if the cosine of the angle between them is less than considerColinearBelow.
    /// Special case: If all points are in a line the first and last point will have directions derived from the refNormal. The inner points will be ValueNone.
    /// Fails if a U-turn exceeding failAtUTurnAbove is detected.
    let getOffsetDirections(uvs: ResizeArray<float>, refNormal:UnitVec, isOpen:bool, considerColinearBelow: float<Cosine.cosine>, failAtUTurnAbove :float<Cosine.cosine>) : ResizeArray<OffsetDirection voption> =
        let segCount = XYZ.pointCount uvs // number of segment unit vectors, three floats each
        let ptsCount = segCount + 1
        let dirs = ResizeArray<OffsetDirection voption>(ptsCount)
        let inline getUV i = UnitVec.createUnchecked(XYZ.getX i uvs, XYZ.getY i uvs, XYZ.getZ i uvs)

        // (1) setup
        let mutable firstDirPending = isOpen
        let mutable fromIdx = 0
        let mutable uPrev = getUV (segCount - 1)
        if isOpen then // this is needed for open polylines that have 180 degree opposing start and end segments
            dirs.Add ValueNone
            fromIdx <- 1
            uPrev <- getUV 0


        let mutable perp  = refNormal
        let mutable nNext = UnitVec.Zaxis // dummy value
        let mutable nPrev = UnitVec.Zaxis // dummy value

        //(2) compute the cross product of the segment unit vectors to get the vertex normals, and offset points
        for i = fromIdx to ptsCount - 2 do // last point is skipped; it's dealt with at the end
            let uNext = getUV i
            let dot   = UnitVec.dot(uPrev, uNext)
            if withMeasure dot < failAtUTurnAbove then
                fail $"Offset3D.getOffsetDirections: {Cosine.inDegrees (withMeasure dot)} degree U-turn detected at pts.[{i}]. exceeding failAtUTurnAbove: {Cosine.inDegrees failAtUTurnAbove}."

            elif withMeasure dot > considerColinearBelow then
                // colinear case: no offset direction defined yet, will be computed in later functions
                dirs.Add ValueNone

            else
                // normal case: offset the corner now
                perp  <- UnitVec.cross(uPrev, uNext) |> Vec.unitize |> UnitVec.matchOrientation perp // use previous perp to keep consistent relative orientation, not always the same refNormal, so it can adapt along the curve, eg. slowly rotate about the uNext axis
                nNext <- crossProduct (perp,  uNext) // they are perpendicular so the result is already unitized
                nPrev <- crossProduct (perp, uPrev) // they are perpendicular so the result is already unitized
                dirs.Add (ValueSome { prevInPlane = nPrev; nextInPlane = nNext; perpDir = perp })

                if firstDirPending  then
                    dirs.[0] <- ValueSome { prevInPlane = nPrev; nextInPlane = nPrev; perpDir = perp } // uses nPrev twice
                    firstDirPending <- false

            uPrev <- uNext


        // (3) do last point and checks:
        if firstDirPending then
            // handle case where all points are colinear for open polyline
            // set first point too
            let n = UnitVec.cross(refNormal, getUV 0) |> Vec.unitize
            let dir = ValueSome { prevInPlane = n; nextInPlane = n; perpDir = refNormal }
            for i = 0 to dirs.LastIndex do
                dirs.[i] <- dir
            dirs.Add dir // last point

        elif isOpen then
            dirs.Add <|  ValueSome { prevInPlane = nNext; nextInPlane = nNext; perpDir = perp } // uses nNext twice

        else
            // closed polyline, last point = first offset point
            dirs.Add dirs.[0] // this might be ValueNone if colinear

        dirs


    /// The core offset Algorithm for constant distance offsets.
    /// Writes the offset point into the result buffer based on the previous and next normals,
    /// the distance, and the precomputed cosine (= dot product of nPrev * nNext).
    /// res.Add ((nPrev + nNext) * (dist / (1.0 + cosine)) + perpDir * distPerp + pt)
    let inline setOffCorner (res:ResizeArray<float>, x:float, y:float, z:float, distInPlane:float, distPerp:float, nPrev:UnitVec, nNext:UnitVec, perpDir:UnitVec, cosine:float) : unit =
        #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
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
        let cos2 = nPrev *** vNext // never 0.0 here, because vectors are already checked to be not colinear
        let a = delta / cos2
        let common = distInPlaneNext / (1.0 + cosine)
        res.Add (x + vNext.X * a + (nPrev.X + nNext.X) * common + perpDir.X * distPerp)
        res.Add (y + vNext.Y * a + (nPrev.Y + nNext.Y) * common + perpDir.Y * distPerp)
        res.Add (z + vNext.Z * a + (nPrev.Z + nNext.Z) * common + perpDir.Z * distPerp)



    /// For all dirs that are ValueNone this will find the index of adjacent points that are OK.
    /// Works also for closed polylines where initial and end segments are colinear
    let getColinearNeighbors(dirs: ResizeArray<OffsetDirection voption>) : ResizeArray<ColinearPoint> =
        let mutable prevIdx = -1
        let mutable nextIdx = -1
        let mutable j = -1
        let colinearPts = ResizeArray<ColinearPoint>()

        for i = 0 to dirs.LastIndex do
            match dirs.[i] with
            | ValueSome _ -> ()
            | ValueNone ->
                // check if we need to find new line from prev and next, to project onto
                if nextIdx < i then
                    // (2.1.1) find the previous defined direction
                    j <-  i - 1
                    while j >= 0 do
                        match dirs.[j] with
                        | ValueSome _ ->
                            prevIdx <- j
                            j <- -99 // found one, exit loop, use any value smaller than -1, if none is found the loop will exit at j = -1
                        | ValueNone ->
                            j <- j - 1 // continue search
                    // (2.1.2) check why the loop exited:
                    if j = -1 then
                        // we are at the start of a closed polyline ,nothing found yet, so need to search backwards from the end
                        j <- dirs.LastIndex
                        while j >= 0 do
                            match dirs.[j] with
                            | ValueSome _ ->
                                prevIdx <- j
                                j <- -999 // found one, exit loop, use any value smaller than -1, if none is found the loop will exit at j = -1
                            | ValueNone ->
                                j <- j - 1 // continue search

                        if j = 0 then // actually should never happen if algorithm is correct
                            fail "Offset3D.offsetConstant: could not find previous valid offset direction for colinear point in closed polyline."


                    // (2.2.1) find the next defined direction
                    j <- i + 1
                    while j < dirs.Count do
                        match dirs.[j] with
                        | ValueSome _ ->
                            nextIdx <- j
                            j <- 999_999_999 // found one, exit loop, use any value bigger than dirs.Count, if none is found the loop will exit at j = dirs.Count
                        | ValueNone ->
                            j <- j + 1 // continue search
                    // (2.2.2) check why the loop exited:
                    if j = dirs.Count then
                        // we are at the end of a closed polyline , nothing found yet, so we need to search forwards from the start
                        j <- 0
                        while j < dirs.Count do
                            match dirs.[j] with
                            | ValueSome _ ->
                                nextIdx <- j
                                j <- 1_000_000_000 //found one, exit loop,, use any value bigger than dirs.Count, if none is found the loop will exit at j = dirs.Count
                            | ValueNone ->
                                j <- j + 1 // continue search

                        if j = dirs.Count then // actually should never happen if algorithm is correct
                            fail "Offset3D.offsetConstant: could not find next valid offset direction for colinear point in closed polyline."

                // (2.3) add the colinear point with its prev and next OK indices even if prevIdx and nextIdx were found earlier
                colinearPts.Add { idx = i; prevOK = prevIdx; nextOK = nextIdx }

        colinearPts

    /// <summary>Offsets a polyline by a constant distance.</summary>
    /// <param name="xyzs">The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.</param>
    /// <param name="dirs">The offset directions at each point of the polyline.</param>
    /// <param name="distInPlane">The distance to offset in the local plane of each vertex.</param>
    /// <param name="distPerpendicular">The distance to offset perpendicular to the local plane of each vertex.</param>
    /// <returns>The offset points as a ResizeArray of interleaved coordinates.</returns>
    let offsetConstantWithDirections(xyzs:ResizeArray<float>, dirs: ResizeArray<OffsetDirection voption>, distInPlane:float, distPerpendicular:float) : ResizeArray<float> =
        XYZ.checkMod3 "offsetConstantWithDirections" xyzs
        let ptCount = XYZ.pointCount xyzs
        if ptCount < 2 then
            fail $"Offset3D.offsetConstantWithDirections: point count {ptCount} must be at least 2 for a polyline."

        if ptCount <> dirs.Count then
            fail $"Offset3D.offsetConstantWithDirections: point count must equal offset directions count, but they are {ptCount} and {dirs.Count}."

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

        // (2) second pass: project colinear points
        if needsSecondPass then
            let colinearPoints = getColinearNeighbors(dirs)
            for i = 0 to colinearPoints.LastIndex do
                let cP = colinearPoints.[i]
                projectPointToLineInPlace cP.idx cP.prevOK cP.nextOK xyzs res res

        res

    /// <summary>Offsets a polyline by variable distances.</summary>
    /// <param name="xyzs">The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.</param>
    /// <param name="segmentDirs">The unit vectors of each segment in the polyline.</param>
    /// <param name="offDirs">The offset directions at each point of the polyline.</param>
    /// <param name="distsInPlane">The distance to offset in the local plane of each vertex.</param>
    /// <param name="distsPerpendicular">The distance to offset perpendicular to the local plane of each vertex.</param>
    /// <param name="isClosed">Whether the polyline is closed (true) or open (false).</param>
    /// <param name="varDistParallelBehavior">The behavior to use when colinear segments with different offset distances are found.</param>
    /// <returns>The offset points as a ResizeArray of interleaved coordinates.</returns>
    let offsetVariableWithDirections(   xyzs:ResizeArray<float>,
                                        segmentDirs:ResizeArray<float>,
                                        offDirs: ResizeArray<OffsetDirection voption>,
                                        distsInPlane:Collections.Generic.IList<float>,
                                        distsPerpendicular:Collections.Generic.IList<float>,
                                        isClosed:bool, varDistParallelBehavior: VarDistParallel
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

        if ptCount <> distsInPlane.Count + 1  then
            fail $"Offset3D.offsetVariableWithDirections:\n in-plane distances count must be {ptCount-1} for {ptCount} points, but is {distsInPlane.Count}."


        let mutable res = ResizeArray<float>(xyzs.Count)
        let mutable needsSecondPass = false
        let mutable prevDistInPlane = if isClosed then distsInPlane.[distsInPlane.Count - 1] else distsInPlane.[0]

        // (1) first pass: offset points that have valid directions
        for i = 0 to ptCount - 2 do
            let x = XYZ.getX i xyzs
            let y = XYZ.getY i xyzs
            let z = XYZ.getZ i xyzs
            let nextDistInPlane = distsInPlane.[i]
            let distPerp = distsPerpendicular.[i]
            match offDirs.[i] with
            | ValueNone ->
                needsSecondPass <- true
                XYZ.add x y z res // for now just keep the input point, will be projected in second pass
            | ValueSome dir ->
                let cosine = dir.prevInPlane *** dir.nextInPlane
                setOffCornerVar(res, x, y, z, prevDistInPlane, nextDistInPlane, distPerp, dir.prevInPlane, dir.nextInPlane, dir.perpDir, cosine, getSegUV i)
            prevDistInPlane <- nextDistInPlane


        // (1.1) handle last point extra because distsInPlane has one less entry than pts and nextDistInPlane depends on loop status
        let li = ptCount - 1
        let x = XYZ.getX li xyzs
        let y = XYZ.getY li xyzs
        let z = XYZ.getZ li xyzs
        let nextDistInPlane = if isClosed then distsInPlane.[0] else distsInPlane.[distsInPlane.Count - 1]
        let distPerp = distsPerpendicular.[distsPerpendicular.Count - 1]
        match offDirs.Last with
        | ValueNone ->
            needsSecondPass <- true
            XYZ.add x y z res // for now just keep the input point, will be projected in second pass
        | ValueSome dir ->
            let cosine = dir.prevInPlane *** dir.nextInPlane
            setOffCornerVar(res, x, y, z, prevDistInPlane, nextDistInPlane, distPerp, dir.prevInPlane, dir.nextInPlane, dir.perpDir, cosine, getSegUV (segCount - 1))


        // (2) second pass: project colinear points
        if needsSecondPass then
            match varDistParallelBehavior with
            | VarDistParallel.Fail ->
                fail "Offset3D.offsetVariableWithDirections: colinear segments with different offset distances found, cannot offset with VarDistParallel.Fail behavior."

            | VarDistParallel.Skip ->
                let newRes = ResizeArray<float>(xyzs.Count)
                for i = 0 to ptCount - 1 do
                    match offDirs.[i] with
                    | ValueSome _ ->
                        XYZ.add (XYZ.getX i res) (XYZ.getY i res) (XYZ.getZ i res) newRes
                    | ValueNone -> () // skip this point
                res <- newRes

            | VarDistParallel.Proportional ->
                // (2) or distribute with equal parameter spacing
                let colinearPoints = getColinearNeighbors(offDirs)
                for i = 0 to colinearPoints.LastIndex do
                    let cP = colinearPoints.[i]
                    let t =
                        rayClosestParameter(
                            XYZ.getX cP.prevOK xyzs,
                            XYZ.getY cP.prevOK xyzs,
                            XYZ.getZ cP.prevOK xyzs,
                            XYZ.getX cP.nextOK xyzs,
                            XYZ.getY cP.nextOK xyzs,
                            XYZ.getZ cP.nextOK xyzs,
                            XYZ.getX cP.idx xyzs,
                            XYZ.getY cP.idx xyzs,
                            XYZ.getZ cP.idx xyzs)
                    setLinePointAt cP.idx cP.prevOK cP.nextOK t res res

            | VarDistParallel.Project ->
                // just project the original point onto the line between the two adjacent offset points
                // this can lead to duplicate points in the result
                let colinearPoints = getColinearNeighbors(offDirs)
                for i = 0 to colinearPoints.LastIndex do
                    let cP = colinearPoints.[i]
                    projectPointToLineInPlace cP.idx cP.prevOK cP.nextOK xyzs res res

            |_  ->
                fail $"Offset3D.offsetVariableWithDirections: unknown VarDistParallel behavior {varDistParallelBehavior}."

        res

    /// Returns the average normal vector of the polyline defined by the X, Y and Z interleaved ResizeArray xyzs.
    /// It is calculated by summing up the cross products of all segments around the center point.
    /// Does not check for bad input, may be zero length if points are colinear.
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
    /// Uses default angles: colinear below 2.5 degrees, fails at U-turns above 175 degrees.</summary>
    /// <param name="refNormal">The reference normal vector to use for computing the local planes.
    /// For a counterclockwise polyline in xy-plane this is Upwards. If its down the offset direction is flipped.</param>
    /// <param name="distInPlane">The offset distance in the local plane defined by two segments.
    /// A positive distance offsets to the inside of the polyline.
    /// A negative distance offsets to the outside.
    /// No matter how the polyline is oriented.</param>
    /// <param name="distPerpendicular">The offset distance perpendicular to the local plane.
    /// A positive distance offsets in the direction of the computed normal.
    /// For a counterclockwise polyline in xy-plane this is Upwards.
    /// A negative distance offsets in the opposite direction.</param>
    /// <param name="xyzs"> The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.</param>
    /// <returns> A new ResizeArray of interleaved coordinates.</returns>
    let offset' (refNormal:UnitVec) (distInPlane: float) (distPerpendicular: float) (xyzs:ResizeArray<float>): ResizeArray<float> =
        let isOpen = XYZ.sqDistFirstLast xyzs > sqOpenTolerance
        let uvs = getSegmentUnitVectors xyzs
        let dirs = getOffsetDirections(uvs, refNormal, isOpen, Cosine.``2.5`` , Cosine.``175.0``)
        offsetConstantWithDirections(xyzs, dirs, distInPlane, distPerpendicular)


    /// <summary> A constant-distance offset algorithm for closed or open polylines in 3D.
    /// Uses default angles: colinear below 2.5 degrees, fails at U-turns above 175 degrees.</summary>
    /// <param name="distInPlane">The offset distance in the local plane defined by two segments.
    /// A positive distance offsets to the inside of the polyline.
    /// A negative distance offsets to the outside.
    /// No matter how the polyline is oriented.</param>
    /// <param name="distPerpendicular">The offset distance perpendicular to the local plane.
    /// A positive distance offsets in the direction of the computed normal.
    /// For a counterclockwise polyline in xy-plane this is Upwards.
    /// A negative distance offsets in the opposite direction.</param>
    /// <param name="xyzs"> The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.</param>
    /// <returns> A new ResizeArray of interleaved coordinates.</returns>
    let offset (distInPlane: float) (distPerpendicular: float) (xyzs: ResizeArray<float>) : ResizeArray<float> =
        let refNormal = averageNormal(xyzs) |> Vec.unitize
        offset' refNormal distInPlane distPerpendicular xyzs


    /// <summary> Offsetting each segment by its own distance. For closed or open polylines in 3D.
    /// Uses default angles: colinear below 2.5 degrees, fails at U-turns above 175 degrees.</summary>
    /// <param name="varDistParallelBehavior">The behavior to use when colinear segments with different offset distances are found.</param>
    /// <param name="refNormal">The reference normal vector to use for computing the local planes.
    /// For a counterclockwise polyline in xy-plane this is Upwards. If its down the offset direction is flipped.</param>
    /// <param name="distancesInPlane">The offset distances in the local plane defined by two segments.
    /// A positive distance offsets to the inside of the polyline.
    /// A negative distance offsets to the outside.
    /// No matter how the polyline is oriented.</param>
    /// <param name="distancesPerpendicular">The offset distances perpendicular to the local plane.
    /// A positive distance offsets in the direction of the computed normal.
    /// For a counterclockwise polyline in xy-plane this is Upwards.
    /// A negative distance offsets in the opposite direction.</param>
    /// <param name="xyzs"> The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.</param>
    /// <returns> A new ResizeArray of interleaved coordinates. If colinear segment handling is set to Skip the point count may differ from input.</returns>
    let offsetVariable' (varDistParallelBehavior: VarDistParallel) (refNormal:UnitVec) (distancesInPlane: ResizeArray<float>) (distancesPerpendicular: ResizeArray<float>) (xyzs: ResizeArray<float>) : ResizeArray<float> =
        let isOpen = XYZ.sqDistFirstLast xyzs > sqOpenTolerance
        let uvs = getSegmentUnitVectors xyzs
        let dirs = getOffsetDirections(uvs, refNormal, isOpen, Cosine.``2.5``, Cosine.``175.0``)
        offsetVariableWithDirections(xyzs, uvs, dirs, distancesInPlane, distancesPerpendicular, isOpen, varDistParallelBehavior)


    /// <summary> Offsetting each segment by its own distance. For closed or open polylines in 3D.
    /// Fails if colinear segments are found (uses VarDistParallel.Fail).
    /// Uses default angles: colinear below 2.5 degrees, fails at U-turns above 175 degrees.</summary>
    /// <param name="distancesInPlane">The offset distances in the local plane defined by two segments.
    /// A positive distance offsets to the inside of the polyline.
    /// A negative distance offsets to the outside.
    /// No matter how the polyline is oriented.</param>
    /// <param name="distancesPerpendicular">The offset distances perpendicular to the local plane.
    /// A positive distance offsets in the direction of the computed normal.
    /// For a counterclockwise polyline in xy-plane this is Upwards.
    /// A negative distance offsets in the opposite direction.</param>
    /// <param name="xyzs"> The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.</param>
    /// <returns> A new ResizeArray of interleaved coordinates.</returns>
    let offsetVariable (distancesInPlane: ResizeArray<float>) (distancesPerpendicular: ResizeArray<float>) (xyzs: ResizeArray<float>) : ResizeArray<float> =
        let refNormal = averageNormal(xyzs) |> Vec.unitize
        offsetVariable' VarDistParallel.Fail refNormal distancesInPlane distancesPerpendicular xyzs

