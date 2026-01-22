namespace Euclid

open System
open UtilEuclid
open EuclidErrors


/// 3D polyline offset utilities based on local vertex normals.
/// Each vertex has its own normal defined by the cross product of the incoming and outgoing segment tangents.
/// These vertex normals define a local plane for each joint; within that plane we build per-segment offset directions.
module Offset3D =

        /// The squared tolerance for open polylines.
    let [<Literal>] openTolerance = 1e-6
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
    type ColinearPnt = {
        idx: int
        prevOK: int
        nextOK: int
    }

    /// Computes the unit vectors of each segment in the polyline defined by pts.
    /// Fails if any two consecutive points are identical.
    /// Returns a ResizeArray of UnitVec, one less than the number of input points.
    let getSegmentUnitVectors(pts: ResizeArray<Pnt>) : ResizeArray<UnitVec> =
        if pts.Count < 2 then
            fail $"Offset3D.GetSegmentUnitVectors: pts.Count {pts.Count} must be at least 2 for a polyline."
        let uvs = ResizeArray<UnitVec>(pts.LastIndex) // the unit vectors of the segments, one less than pts
        let mutable prevPt = pts.[0]
        for i = 1 to pts.LastIndex do
            let p = pts.[i]
            let v = p - prevPt // the vector from previous to current point
            let len = v.Length
            if len < openTolerance then fail $"Offset3D.GetSegmentUnitVectors: pts.[{i}] and pts.[{i-1}] are the same at {prevPt}"
            let f = 1.0 / len
            uvs.Add (UnitVec.createUnchecked(v.X*f, v.Y*f, v.Z*f))
            prevPt <- p
        uvs


    /// Returns a Unit vector.
    /// Only to be used when the input vectors are known to be perpendicular.
    let inline internal crossProduct (a:UnitVec, b:UnitVec) =
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
    let getOffsetDirections(uvs: ResizeArray<UnitVec>, refNormal:UnitVec, isOpen:bool, considerColinearBelow: float<Cosine.cosine>, failAtUTurnAbove :float<Cosine.cosine>) : ResizeArray<OffsetDirection voption> =
        let ptsCount = uvs.Count + 1
        let dirs = ResizeArray<OffsetDirection voption>(ptsCount)

        // (1) setup
        let mutable firstDirPending = isOpen
        let mutable fromIdx = 0
        let mutable uPrev = uvs.Last
        if isOpen then // this is needed for open polylines that have 180 degree opposing start and end segments
            dirs.Add ValueNone
            fromIdx <- 1
            uPrev <- uvs.[0]


        let mutable perp  = refNormal
        let mutable nNext = UnitVec.Zaxis // dummy value
        let mutable nPrev = UnitVec.Zaxis // dummy value

        //(2) compute the cross product of the segment unit vectors to get the vertex normals, and offset points
        for i = fromIdx to ptsCount - 2 do // last point is skipped; it's dealt with at the end
            let uNext = uvs.[i]
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
            let n = UnitVec.cross(refNormal, uvs.[0]) |> Vec.unitize
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
    /// Returns the offset point based on the previous and next normals,
    /// the distance, and the precomputed cosine (= dot product of nPrev * nNext).
    /// (pt + (nPrev + nNext) * (dist / (1.0 + cosine)))
    let inline setOffCorner (pt:Pnt) (dist:float) (nPrev:UnitVec) (nNext:UnitVec) (cosine:float) :Pnt =
        #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
            if cosine < -0.9999999 then
                fail $"Offset3D.setOffCorner: cosine is {cosine} , a 180 degree U-turn, which is not allowed here for dist {dist}, nPrev {nPrev}, nNext {nNext} at pt {pt}."
        #endif
            // based on https://www.angusj.com/clipper2/Docs/Trigonometry.htm
            pt + (nPrev + nNext) * (dist / (1.0 + cosine))


    /// offset point calculation for variable distances:
    let inline setOffCornerVar (pt:Pnt) (distPrev:float) (distNext:float) (nPrev:UnitVec) (nNext:UnitVec) (cosine:float) (vNext:UnitVec) : Pnt =
        let delta = distPrev - distNext
        let cos2 = nPrev *** vNext // never 0.0 here, because vectors are already checked to be not colinear
        pt
            + vNext * (delta / cos2) // the offset for the delta in distances
            + (nPrev + nNext) * (distNext / (1.0 + cosine)) // the offset for the common distance



    /// For all dirs that are ValueNone this will find the index of adjacent points that are OK.
    /// Works also for closed polylines where initial and end segments are colinear
    let getColinearNeighbors(dirs: ResizeArray<OffsetDirection voption>) : ResizeArray<ColinearPnt> =
        let mutable prevIdx = -1
        let mutable nextIdx = -1
        let mutable j = -1
        let colinearPts = ResizeArray<ColinearPnt>()

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
    /// <param name="pts">The points of the polyline to offset.</param>
    /// <param name="dirs">The offset directions at each point of the polyline.</param>
    /// <param name="distInPlane">The distance to offset in the local plane of each vertex.</param>
    /// <param name="distPerpendicular">The distance to offset perpendicular to the local plane of each vertex.</param>
    /// <returns>The offset points as a ResizeArray of Pnt.</returns>
    let offsetConstantWithDirections(pts:ResizeArray<Pnt>, dirs: ResizeArray<OffsetDirection voption>, distInPlane:float, distPerpendicular:float) : ResizeArray<Pnt> =
        if pts.Count < 2 then
            fail $"Offset3D.offsetConstant: pts.Count {pts.Count} must be at least 2 for a polyline."

        let res = ResizeArray<Pnt>(pts.Count)
        let mutable needsSecondPass = false

        // (1) first pass: offset points that have valid directions
        for i = 0 to pts.LastIndex do
            let pt = pts.[i]
            match dirs.[i] with
            | ValueNone ->
                needsSecondPass <- true
                res.Add pt // for now just keep the input point, will be projected in second pass
            | ValueSome dir ->
                let cosine = dir.prevInPlane *** dir.nextInPlane
                let inPlanePt  = setOffCorner pt distInPlane dir.prevInPlane dir.nextInPlane cosine
                let offsetPt = inPlanePt + dir.perpDir * distPerpendicular // offset out of the plane by dist along the refNormal
                res.Add offsetPt

        // (2) second pass: project colinear points
        if needsSecondPass then
            let clPnts = getColinearNeighbors(dirs)
            for i = 0 to clPnts.LastIndex do
                let cP = clPnts.[i]
                let ln = Line3D(res.[cP.prevOK], res.[cP.nextOK])
                res.[cP.idx] <- Line3D.rayClosestPoint pts.[cP.idx] ln

        res

    /// <summary>Offsets a polyline by variable distances.</summary>
    /// <param name="pts">The points of the polyline to offset.</param>
    /// <param name="segmentDirs">The unit vectors of each segment in the polyline.</param>
    /// <param name="offDirs">The offset directions at each point of the polyline.</param>
    /// <param name="distsInPlane">The distance to offset in the local plane of each vertex.</param>
    /// <param name="distsPerpendicular">The distance to offset perpendicular to the local plane of each vertex.</param>
    /// <param name="isClosed">Whether the polyline is closed (true) or open (false).</param>
    /// <param name="varDistParallelBehavior">The behavior to use when colinear segments with different offset distances are found.</param>
    /// <returns>The offset points as a ResizeArray of Pnt.</returns>
    let offsetVariableWithDirections(pts:ResizeArray<Pnt>, segmentDirs:ResizeArray<UnitVec>, offDirs: ResizeArray<OffsetDirection voption>, distsInPlane:Collections.Generic.IList<float>, distsPerpendicular:Collections.Generic.IList<float>, isClosed:bool, varDistParallelBehavior: VarDistParallel) : ResizeArray<Pnt> =
        if pts.Count < 2 then
            fail $"Offset3D.offsetVariable: pts.Count {pts.Count} must be at least 2 for a polyline."

        if pts.Count <> segmentDirs.Count + 1  then
            fail $"Offset3D.offsetVariableWithDirections:\n   Point count must be 1 greater than normal directions count, but they are {pts.Count} and {segmentDirs.Count}."

        if pts.Count <> distsInPlane.Count + 1  then
            fail $"Offset3D.offsetVariableWithDirections:\n   Point count must be 1 greater than offset distances count, but they are {pts.Count} and {distsInPlane.Count}."

        let mutable res = ResizeArray<Pnt>(pts.Count)
        let mutable needsSecondPass = false
        let mutable prevDistInPlane = if isClosed then distsInPlane.[distsInPlane.Count - 1] else distsInPlane.[0]

        // (1) first pass: offset points that have valid directions
        for i = 0 to pts.LastIndex-1 do
            let pt = pts.[i]
            let nextDistInPlane = distsInPlane.[i]
            let distPerp = distsPerpendicular.[i]
            match offDirs.[i] with
            | ValueNone ->
                needsSecondPass <- true
                res.Add pt // for now just keep the input point, will be projected in second pass
            | ValueSome dir ->
                let cosine = dir.prevInPlane *** dir.nextInPlane
                let inPlanePt  = setOffCornerVar pt prevDistInPlane nextDistInPlane dir.prevInPlane dir.nextInPlane cosine segmentDirs.[i]
                let offsetPt = inPlanePt + dir.perpDir * distPerp // offset out of the plane by dist along the refNormal
                res.Add offsetPt


        // (1.1) handle last point extra because distsInPlane has one less entry than pts and nextDistInPlane depends on loop status
        let pt = pts.Last
        let nextDistInPlane = if isClosed then distsInPlane.[0] else distsInPlane.[distsInPlane.Count - 1]
        let distPerp = distsPerpendicular.[distsPerpendicular.Count - 1]
        match offDirs.Last with
        | ValueNone ->
            needsSecondPass <- true
            res.Add pt // for now just keep the input point, will be projected in second pass
        | ValueSome dir ->
            let cosine = dir.prevInPlane *** dir.nextInPlane
            let inPlanePt  = setOffCornerVar pt prevDistInPlane nextDistInPlane dir.prevInPlane dir.nextInPlane cosine segmentDirs.[segmentDirs.LastIndex]
            let offsetPt = inPlanePt + dir.perpDir * distPerp // offset out of the plane by dist along the refNormal
            res.Add offsetPt


        // (2) second pass: project colinear points
        if needsSecondPass then
            match varDistParallelBehavior with
            | VarDistParallel.Fail ->
                fail "Offset3D.offsetVariable: colinear segments with different offset distances found, cannot offset with VarDistParallel.Fail behavior."

            | VarDistParallel.Skip ->
                let newRes = ResizeArray<Pnt>(pts.Count)
                for i = 0 to pts.LastIndex do
                    match offDirs.[i] with
                    | ValueSome _ -> newRes.Add res.[i]
                    | ValueNone -> () // skip this point
                res <- newRes

            | VarDistParallel.Proportional ->
                // (2) or distribute with equal parameter spacing
                let clPnts = getColinearNeighbors(offDirs)
                for i = 0 to clPnts.LastIndex do
                    let cP = clPnts.[i]
                    let origLn = Line3D(pts.[cP.prevOK], pts.[cP.nextOK])
                    let t = origLn.RayClosestParameter pts.[cP.idx]
                    let ln = Line3D(res.[cP.prevOK], res.[cP.nextOK])
                    res.[cP.idx] <- ln.EvaluateAt(t)

            | VarDistParallel.Project ->
                // just project the original point onto the line between the two adjacent offset points
                // this can lead to duplicate points in the result
                let clPnts = getColinearNeighbors(offDirs)
                for i = 0 to clPnts.LastIndex do
                    let cP = clPnts.[i]
                    let ln = Line3D(res.[cP.prevOK], res.[cP.nextOK])
                    res.[cP.idx] <- Line3D.rayClosestPoint pts.[cP.idx] ln

            |_  ->
                fail $"Offset3D.offsetVariable: unknown VarDistParallel behavior {varDistParallelBehavior}."

        res

    /// Returns the average normal vector of the Polyline3D.
    /// It is calculated by summing up the cross products of all segments around the center point.
    /// Does not check for bad input, may be zero length if points are colinear.
    let internal averageNormal(pts: ResizeArray<Pnt>) : Vec =
        let mutable x = 0.0
        let mutable y = 0.0
        let mutable z = 0.0
        for i = 0 to pts.LastIndex do
            let p = pts.[i]
            x <- x + p.X
            y <- y + p.Y
            z <- z + p.Z
        let c = Pnt(x / float pts.Count, y / float pts.Count, z / float pts.Count)
        let mutable normal = Vec.Zero
        let mutable a = pts.[pts.LastIndex] - c
        for i = 0 to pts.LastIndex do
            let b = pts.[i] - c
            normal <- normal + Vec.cross(a, b)
            a <- b
        normal


    //     █████████                                  ███               █████      █████████   ███████████  █████
    //    ███░░░░░███                                ░░░               ░░███      ███░░░░░███ ░░███░░░░░███░░███
    //   ███     ░░░  █████ ████ ████████  ████████  ████   ██████   ███████     ░███    ░███  ░███    ░███ ░███
    //  ░███         ░░███ ░███ ░░███░░███░░███░░███░░███  ███░░███ ███░░███     ░███████████  ░██████████  ░███
    //  ░███          ░███ ░███  ░███ ░░░  ░███ ░░░  ░███ ░███████ ░███ ░███     ░███░░░░░███  ░███░░░░░░   ░███
    //  ░░███     ███ ░███ ░███  ░███      ░███      ░███ ░███░░░  ░███ ░███     ░███    ░███  ░███         ░███
    //   ░░█████████  ░░████████ █████     █████     █████░░██████ ░░████████    █████   █████ █████        █████
    //    ░░░░░░░░░    ░░░░░░░░ ░░░░░     ░░░░░     ░░░░░  ░░░░░░   ░░░░░░░░    ░░░░░   ░░░░░ ░░░░░        ░░░░░



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
    /// <param name="pts"> The points of the polyline to offset.</param>
    /// <returns> A new ResizeArray of Points.</returns>
    let offset' (refNormal:UnitVec) (distInPlane: float) (distPerpendicular: float) (pts:ResizeArray<Pnt>): ResizeArray<Pnt> =
        let isOpen = Pnt.distanceSq pts.First pts.Last > sqOpenTolerance
        let uvs = getSegmentUnitVectors pts
        let dirs = getOffsetDirections(uvs, refNormal, isOpen, Cosine.``2.5`` , Cosine.``175.0``)
        offsetConstantWithDirections(pts, dirs, distInPlane, distPerpendicular)


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
    /// <param name="pts"> The points of the polyline to offset.</param>
    /// <returns> A new ResizeArray of Points.</returns>
    let offset (distInPlane: float) (distPerpendicular: float) (pts: ResizeArray<Pnt>) : ResizeArray<Pnt> =
        let refNormal = averageNormal(pts) |> Vec.unitize
        offset' refNormal distInPlane distPerpendicular pts




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
    /// <param name="pts"> The points of the polyline to offset.</param>
    /// <returns> A new ResizeArray of Points. If colinear segment handling is set to Skip the point count may differ from input.</returns>
    let offsetVariable' (varDistParallelBehavior: VarDistParallel) (refNormal:UnitVec) (distancesInPlane: ResizeArray<float>) (distancesPerpendicular: ResizeArray<float>) (pts: ResizeArray<Pnt>) : ResizeArray<Pnt> =
        let isOpen = Pnt.distanceSq pts.First pts.Last > sqOpenTolerance
        let uvs = getSegmentUnitVectors pts
        let dirs = getOffsetDirections(uvs, refNormal, isOpen, Cosine.``2.5``, Cosine.``175.0``)
        offsetVariableWithDirections(pts, uvs, dirs, distancesInPlane, distancesPerpendicular, isOpen, varDistParallelBehavior)


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
    /// <param name="pts"> The points of the polyline to offset.</param>
    /// <returns> A new ResizeArray of Points.</returns>
    let offsetVariable (distancesInPlane: ResizeArray<float>) (distancesPerpendicular: ResizeArray<float>) (pts: ResizeArray<Pnt>) : ResizeArray<Pnt> =
        let refNormal = averageNormal(pts) |> Vec.unitize
        offsetVariable' VarDistParallel.Fail refNormal distancesInPlane distancesPerpendicular pts

