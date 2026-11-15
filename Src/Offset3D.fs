namespace Euclid

open System
open UtilEuclid
open EuclidErrors
open Euclid.EuclidCollectionUtilities


/// A module containing the core algorithms for offsetting 3D polylines.
/// For a simpler interface, prefer the offset functions in the Polyline3D module.
/// Each point gets its own offset direction: a local plane, with an in-plane direction perpendicular to each adjacent segment,
/// and the direction perpendicular to that plane.
/// At a corner the local plane contains both adjacent segments. A collinear point takes the plane interpolated between the neighboring corners.
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

    /// An Enum for describing what to do at collinear points where the two segments have different in-plane offset distances.
    /// Collinear segments can only be offset parallel to themselves if their distances are the same.
    /// A point is collinear here if it turns by less than considerCollinearBelow,
    /// and if the polyline also turns by less than that from the last point that was offset exactly.
    /// This Enum describes the options:
    /// 1: Fail
    /// 2: just skip the point.
    /// 3: add a point at the same proportional distance on the oblique segment, non parallel offsets.
    /// 4: add a point perpendicular from here intersecting the oblique segment, non parallel offsets.
    // [<RequireQualifiedAccess>]
    type VarDistParallel = // needs to be an enum , so it can be an optional parameter with default value
        // There is no StepWithTwoPoints like in Offset2D.VarDistParallel, on purpose:
        // a collinear 3D point only has the plane interpolated between its neighboring corners, so there is no consistent plane for the step.

        /// Fail if collinear segments with different offset distances are found.
        | Fail = 1

        /// Just skip collinear points with different offset distances.
        /// This introduces non parallel offset segments.
        /// This reduces the total point count.
        | Skip = 2

        /// Add a point at the same proportional distance as the input segments.
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


    /// The offset directions at a point of a polyline.
    /// At a corner, the local plane contains both adjacent segments.
    /// At a collinear point, it is interpolated between the planes of the neighboring corners.
    [<NoComparison; NoEquality>]
    type OffsetDirection = {
        /// The in-plane unit direction perpendicular to the previous segment.
        PreviousInPlane: UnitVec
        /// The in-plane unit direction perpendicular to the next segment.
        NextInPlane: UnitVec
        /// The unit direction perpendicular to the local plane.
        Perpendicular: UnitVec
    }

    /// For a marked point, the index of the point, and the indices of the nearest unmarked points before and after it.
    /// PreviousOK may be bigger than NextOK if the search wrapped around the seam of a closed polyline.
    [<NoComparison; NoEquality>]
    type internal MarkedPoint = {
        /// The index of the marked point.
        Index: int
        /// The index of the nearest unmarked point before it.
        PreviousOK: int
        /// The index of the nearest unmarked point after it.
        NextOK: int
    }

    module private XYZ =

        let inline pointCount (xyzs: ResizeArray<float>) : int =
            xyzs.Count / 3


        let inline checkMod3 methodName (xyzs: ResizeArray<float>) =
            if xyzs.Count % 3 <> 0 then
                failMod3 methodName xyzs

        let inline getX i (xyzs: ResizeArray<float>) : float =
            ResizeArr.getIdx (i * 3) xyzs

        let inline getY i (xyzs: ResizeArray<float>) : float =
            ResizeArr.getIdx (i * 3 + 1) xyzs

        let inline getZ i (xyzs: ResizeArray<float>) : float =
            ResizeArr.getIdx (i * 3 + 2) xyzs

        let inline set i x y z (xyzs: ResizeArray<float>) : unit =
            ResizeArr.setIdx (i * 3    ) x xyzs
            ResizeArr.setIdx (i * 3 + 1) y xyzs
            ResizeArr.setIdx (i * 3 + 2) z xyzs

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

        let inline sqDist a b (xyzs: ResizeArray<float>) : float =
            let dx = getX a xyzs - getX b xyzs
            let dy = getY a xyzs - getY b xyzs
            let dz = getZ a xyzs - getZ b xyzs
            dx * dx + dy * dy + dz * dz

        /// The dot product of the unit vectors a and b.
        let inline dot a b (uvs: ResizeArray<float>) : float =
            getX a uvs * getX b uvs + getY a uvs * getY b uvs + getZ a uvs * getZ b uvs

        let inline getUnitVec i (uvs: ResizeArray<float>) : UnitVec =
            UnitVec.createUnchecked(getX i uvs, getY i uvs, getZ i uvs)

    let inline private rayClosestParameter(fromX:float, fromY:float, fromZ:float, toX:float, toY:float, toZ:float, pX:float, pY:float, pZ:float) : float =
        let vx = toX - fromX
        let vy = toY - fromY
        let vz = toZ - fromZ
        let lenSq = vx * vx + vy * vy + vz * vz
        if isTooSmallSq lenSq then
            fail $"Offset3D.rayClosestParameter: line from ({fromX}, {fromY}, {fromZ}) to ({toX}, {toY}, {toZ}) is too short."
        ((pX - fromX) * vx + (pY - fromY) * vy + (pZ - fromZ) * vz) / lenSq

    /// The parameter of the closest point to point idx on the line between the points fromIdx and toIdx, all in xyzs.
    let inline private closestParameter (fromIdx:int) (toIdx:int) (idx:int) (xyzs: ResizeArray<float>) : float =
        rayClosestParameter(
            XYZ.getX fromIdx xyzs,
            XYZ.getY fromIdx xyzs,
            XYZ.getZ fromIdx xyzs,
            XYZ.getX toIdx xyzs,
            XYZ.getY toIdx xyzs,
            XYZ.getZ toIdx xyzs,
            XYZ.getX idx xyzs,
            XYZ.getY idx xyzs,
            XYZ.getZ idx xyzs)

    let inline private setLinePointAt (idx:int) (fromIdx:int) (toIdx:int) (t:float) (lineXYZs: ResizeArray<float>) (res: ResizeArray<float>) : unit =
        let fromX = XYZ.getX fromIdx lineXYZs
        let fromY = XYZ.getY fromIdx lineXYZs
        let fromZ = XYZ.getZ fromIdx lineXYZs
        let x = fromX + (XYZ.getX toIdx lineXYZs - fromX) * t
        let y = fromY + (XYZ.getY toIdx lineXYZs - fromY) * t
        let z = fromZ + (XYZ.getZ toIdx lineXYZs - fromZ) * t
        XYZ.set idx x y z res

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

    /// <summary>Computes the unit vectors of the segments of a polyline.</summary>
    /// <param name="xyzs">The X, Y and Z interleaved coordinates of the polyline (x0, y0, z0, x1, y1, z1, ...).</param>
    /// <returns>An X, Y and Z interleaved ResizeArray of the segment unit vectors, with one vector less than the number of points.
    /// Fails if two consecutive points are closer than 1e-6 or if there are less than two points.</returns>
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


    /// The offset direction at a point from a given perpendicular direction (px, py, pz), that does not need to be unitized
    /// or perpendicular to the polyline. It is made perpendicular to the tangent at the point,
    /// and the in-plane directions are perpendicular to it and to each segment.
    /// Returns ValueNone if the given direction is parallel to the tangent.
    /// Fails if the segments are opposite.
    let private tryDirectionFromPerpendicular (px:float) (py:float) (pz:float) (uPrev:UnitVec) (uNext:UnitVec) (ptIdx:int) : OffsetDirection voption =
        let tx = uPrev.X + uNext.X // the tangent, not unitized, its length is almost 2.0 because U-turns are excluded
        let ty = uPrev.Y + uNext.Y
        let tz = uPrev.Z + uNext.Z
        let tLenSq = tx * tx + ty * ty + tz * tz
        if isTooTinySq tLenSq then
            fail $"Offset3D.getOffsetDirections: the segments before and after pts.[{ptIdx}] are opposite: {uPrev} and {uNext}."
        let k = (px * tx + py * ty + pz * tz) / tLenSq
        let qx = px - tx * k
        let qy = py - ty * k
        let qz = pz - tz * k
        let len = sqrt (qx * qx + qy * qy + qz * qz)
        if isTooSmall len then
            ValueNone
        else
            let p = UnitVec.createUnchecked(qx / len, qy / len, qz / len)
            ValueSome { PreviousInPlane = UnitVec.cross(p, uPrev) |> Vec.unitize; NextInPlane = UnitVec.cross(p, uNext) |> Vec.unitize; Perpendicular = p }


    /// <summary>Computes the offset direction at each point of a polyline.</summary>
    /// <remarks>
    /// A point is a corner if it turns by at least the angle of considerCollinearBelow,
    /// or if the angle between the first segment after the previous corner and the segment after the point reaches it.
    /// So a finely sampled arc still gets a corner at least every few points. For an open polyline, the run before the first corner starts with the first segment.
    /// At a corner the local plane contains both adjacent segments, and its normal is their cross product.
    /// The corner normal that is most aligned with refNormal is flipped to match it, the first one if several are aligned equally within 1e-6.
    /// From there, each corner normal is flipped to match its neighbor corner, so that the inside stays on the same side along the polyline.
    /// If the two normals are within half a degree of perpendicular, it is flipped to match the in-plane direction of the neighbor corner on the segment towards it instead.
    /// So the result does not depend on rounding, and rotating the polyline rotates the result.
    /// Every other point takes the normal interpolated between the neighboring corners at its position, made perpendicular to the polyline.
    /// The points before the first and after the last corner of an open polyline, including its end points, take the normal of that corner.
    /// If no point is a corner, as on a straight polyline, every point takes refNormal made perpendicular to the polyline.
    /// The in-plane directions are perpendicular to the normal and to each adjacent segment.
    /// The end points of an open polyline only have one segment, so both of their in-plane directions are the same.
    /// For a closed polyline the last direction is the first.
    /// </remarks>
    /// <param name="xyzs">The X, Y and Z interleaved coordinates of the polyline. It is closed if the first and last point are within 1e-6.</param>
    /// <param name="uvs">The X, Y and Z interleaved segment unit vectors, as from getSegmentUnitVectors.</param>
    /// <param name="refNormal">The reference normal that orients the corners, or defines all directions if there is no corner.
    /// Fails if it is within half a degree of perpendicular to every corner normal, because then it cannot orient them.</param>
    /// <param name="considerCollinearBelow">Points where the segments turn by less than this angle are collinear, unless the polyline turned by more than this angle since the previous corner.</param>
    /// <param name="failAtUTurnAbove">Fails at points where the segments turn by more than this angle.
    /// There is no U-turn behavior like Offset2D.UTurn, on purpose: two almost opposite segments span no reliable plane in 3D, so a chamfer or threshold point has no defined direction. Remove U-turns first, for example with Polyline3D.removeUTurns.</param>
    /// <returns>One offset direction per point.</returns>
    let getOffsetDirections(xyzs: ResizeArray<float>, uvs: ResizeArray<float>, refNormal:UnitVec, considerCollinearBelow: float<Cosine.cosine>, failAtUTurnAbove :float<Cosine.cosine>) : ResizeArray<OffsetDirection> =
        XYZ.checkMod3 "getOffsetDirections" xyzs
        XYZ.checkMod3 "getOffsetDirections segment vectors" uvs
        let ptCount = XYZ.pointCount xyzs
        let segCount = XYZ.pointCount uvs
        if ptCount < 2 then
            fail $"Offset3D.getOffsetDirections: point count {ptCount} must be at least 2 for a polyline."
        if ptCount <> segCount + 1 then
            fail $"Offset3D.getOffsetDirections: point count must be one greater than segment vector count but they are {ptCount} and {segCount}."
        let isClosed = XYZ.sqDistFirstLast xyzs < openToleranceSq
        let count = if isClosed then ptCount - 1 else ptCount // the points with their own direction, the last point of a closed polyline is the first
        let lastSeg = segCount - 1
        // the segments before and after a point, the end points of an open polyline use their own segment twice
        let inline prevSeg i = if i > 0 then i - 1 elif isClosed then lastSeg else 0
        let inline nextSeg i = min i lastSeg

        // (1) check for U-turns at the points with two segments
        let firstInner = if isClosed then 0 else 1
        let lastInner = if isClosed then count - 1 else count - 2
        for i = firstInner to lastInner do
            let dot = XYZ.dot (prevSeg i) i uvs
            if withMeasure dot < failAtUTurnAbove then
                fail $"Offset3D.getOffsetDirections: {Cosine.inDegrees (withMeasure dot)} degree U-turn detected at pts.[{i}]. exceeding failAtUTurnAbove: {Cosine.inDegrees failAtUTurnAbove}."

        // (2) find the corners and their normals, not oriented yet
        let isCorner = Array.zeroCreate<bool> count
        let normals = Array.create count UnitVec.ZAxis // dummy values for the points that are not corners
        let inline isLocalCorner i = withMeasure (XYZ.dot (prevSeg i) i uvs) <= considerCollinearBelow
        // makes point i a corner, unless its segments are parallel, then their plane is unknown
        let tryMarkCorner i : bool =
            let a = XYZ.getUnitVec (prevSeg i) uvs
            let b = XYZ.getUnitVec i uvs
            let x = a.Y * b.Z - a.Z * b.Y
            let y = a.Z * b.X - a.X * b.Z
            let z = a.X * b.Y - a.Y * b.X
            let len = sqrt (x * x + y * y + z * z)
            if isTooSmall len then
                if a *** b < 0.0 then // only possible with extreme thresholds
                    fail $"Offset3D.getOffsetDirections: 180 degree U-turn detected at pts.[{i}]."
                false
            else
                isCorner.[i] <- true
                normals.[i] <- UnitVec.createUnchecked(x / len, y / len, z / len)
                true

        // the scan for corners starts at a corner, so that a closed polyline gives the same corners for any start point
        let mutable start = 0
        let mutable steps = count - 2 // for an open polyline, from point 1 to the second last point
        if isClosed then
            steps <- count - 1
            start <- -1
            let mutable i = 0
            while start = -1 && i < count do
                if isLocalCorner i && tryMarkCorner i then start <- i
                i <- i + 1
            if start = -1 then
                // no point turns by considerCollinearBelow, like on a finely sampled circle:
                // start at the first of the points that turn the most, so that rounding does not decide
                let mutable minDot = 2.0
                for j = 0 to count - 1 do
                    minDot <- min minDot (XYZ.dot (prevSeg j) j uvs)
                let mutable j = 0
                while start = -1 do
                    if isTooSmall (XYZ.dot (prevSeg j) j uvs - minDot) then start <- j
                    j <- j + 1
                if not (tryMarkCorner start) then
                    steps <- 0 // no point turns at all, all directions come from refNormal below
        let mutable runSeg = nextSeg start // the first segment after the previous corner
        for k = 1 to steps do
            let i = (start + k) % count
            if isLocalCorner i || withMeasure (XYZ.dot runSeg i uvs) <= considerCollinearBelow then
                if tryMarkCorner i then
                    runSeg <- i

        let dirs = ResizeArray<OffsetDirection>(ptCount)
        if not (Array.contains true isCorner) then
            // (3) no corner, like on a straight polyline: derive every direction from refNormal
            for i = 0 to count - 1 do
                match tryDirectionFromPerpendicular refNormal.X refNormal.Y refNormal.Z (XYZ.getUnitVec (prevSeg i) uvs) (XYZ.getUnitVec (nextSeg i) uvs) i with
                | ValueSome d -> dirs.Add d
                | ValueNone -> fail $"Offset3D.getOffsetDirections: the reference normal {refNormal} is parallel to the polyline at pts.[{i}], so it cannot define the offset plane there."
        else
            // (4) orient the corner normals, starting at the one most aligned with refNormal
            let perps = normals // oriented in place
            let mutable best = -1.0
            for i = 0 to count - 1 do
                if isCorner.[i] then
                    best <- max best (abs (normals.[i] *** refNormal))
            // the first of the corners that are equally aligned, so that rounding does not decide
            let mutable anchor = -1
            let mutable i = 0
            while anchor = -1 do
                if isCorner.[i] && isTooSmall (best - abs (normals.[i] *** refNormal)) then
                    anchor <- i
                i <- i + 1
            if withMeasure best < Cosine.``89.5`` then
                fail $"Offset3D.getOffsetDirections: the reference normal {refNormal} is perpendicular to the plane of every corner, so it cannot orient the offset. The best corner at pts.[{anchor}] has the normal {normals.[anchor]}."
            perps.[anchor] <- UnitVec.matchOrientation refNormal normals.[anchor]

            // Orients the corner normal at i from the oriented corner p before it.
            // If they are almost perpendicular, the side is given by the in-plane direction of p on the segment towards i.
            let orientFromPrevious (p:int) (i:int) : UnitVec =
                let n = normals.[i]
                let d = perps.[p] *** n
                if withMeasure (abs d) < Cosine.``89.5`` then
                    let nNextP = crossProduct(perps.[p], XYZ.getUnitVec (nextSeg p) uvs)
                    if nNextP *** n < 0.0 then -n else n
                elif d < 0.0 then -n else n

            // Orients the corner normal at i from the oriented corner q after it, with the same result as orientFromPrevious from i to q.
            let orientFromNext (q:int) (i:int) : UnitVec =
                let n = normals.[i]
                let d = perps.[q] *** n
                if withMeasure (abs d) < Cosine.``89.5`` then
                    let nPrevQ = crossProduct(perps.[q], XYZ.getUnitVec (prevSeg q) uvs)
                    if nPrevQ *** n > 0.0 then -n else n
                elif d < 0.0 then -n else n

            let mutable p = anchor
            if isClosed then
                for k = 1 to count - 1 do
                    let i = (anchor + k) % count
                    if isCorner.[i] then
                        perps.[i] <- orientFromPrevious p i
                        p <- i
            else
                for i = anchor + 1 to count - 1 do
                    if isCorner.[i] then
                        perps.[i] <- orientFromPrevious p i
                        p <- i
                p <- anchor
                for i = anchor - 1 downto 0 do
                    if isCorner.[i] then
                        perps.[i] <- orientFromNext p i
                        p <- i

            // (5) the nearest corners before and after each point, around the seam of a closed polyline
            let prevCorner = Array.create count -1
            let nextCorner = Array.create count -1
            let mutable c = -1
            if isClosed then
                let mutable i = count - 1
                while c = -1 do // there is a corner
                    if isCorner.[i] then c <- i
                    i <- i - 1
            for i = 0 to count - 1 do
                if isCorner.[i] then c <- i else prevCorner.[i] <- c
            c <- -1
            if isClosed then
                let mutable i = 0
                while c = -1 do
                    if isCorner.[i] then c <- i
                    i <- i + 1
            for i = count - 1 downto 0 do
                if isCorner.[i] then c <- i else nextCorner.[i] <- c

            // (6) the directions
            for i = 0 to count - 1 do
                let uPrev = XYZ.getUnitVec (prevSeg i) uvs
                let uNext = XYZ.getUnitVec (nextSeg i) uvs
                if isCorner.[i] then
                    let p = perps.[i]
                    dirs.Add { PreviousInPlane = crossProduct(p, uPrev); NextInPlane = crossProduct(p, uNext); Perpendicular = p }
                else
                    // interpolate the normals of the neighboring corners, or take the one of the only neighboring corner
                    let a = if prevCorner.[i] = -1 then nextCorner.[i] else prevCorner.[i]
                    let b = if nextCorner.[i] = -1 then prevCorner.[i] else nextCorner.[i]
                    let pa = perps.[a]
                    let pb = perps.[b]
                    let t =
                        if a = b || isTooSmallSq (XYZ.sqDist a b xyzs) then 0.5
                        else closestParameter a b i xyzs |> clamp01
                    let px = pa.X + (pb.X - pa.X) * t
                    let py = pa.Y + (pb.Y - pa.Y) * t
                    let pz = pa.Z + (pb.Z - pa.Z) * t
                    match tryDirectionFromPerpendicular px py pz uPrev uNext i with
                    | ValueSome d -> dirs.Add d
                    | ValueNone -> fail $"Offset3D.getOffsetDirections: the normals {pa} and {pb} of the corners at pts.[{a}] and pts.[{b}] cancel out or are parallel to the polyline at pts.[{i}] between them."

        if isClosed then
            dirs.Add dirs.[0] // the last point is the first point
        dirs


    /// <summary>The core offset algorithm. Adds the offset point to the result buffer.</summary>
    /// <remarks>Computes pt + (previousInPlane + nextInPlane) * (distanceInPlane / (1.0 + cosine)) + perpendicular * distancePerpendicular.
    /// At a 180 degree U-turn the cosine is -1.0 and the point is at infinity.</remarks>
    /// <param name="res">The buffer to add the X, Y and Z coordinates of the offset point to.</param>
    /// <param name="x">The X coordinate of the input point.</param>
    /// <param name="y">The Y coordinate of the input point.</param>
    /// <param name="z">The Z coordinate of the input point.</param>
    /// <param name="distanceInPlane">The offset distance in the local plane.</param>
    /// <param name="distancePerpendicular">The offset distance perpendicular to the local plane.</param>
    /// <param name="previousInPlane">The in-plane unit direction perpendicular to the previous segment.</param>
    /// <param name="nextInPlane">The in-plane unit direction perpendicular to the next segment.</param>
    /// <param name="perpendicular">The unit direction perpendicular to the local plane.</param>
    /// <param name="cosine">The precomputed dot product of previousInPlane and nextInPlane.</param>
    /// <returns>Unit, the point is added to res.</returns>
    let inline setOffCorner (res:ResizeArray<float>, x:float, y:float, z:float, distanceInPlane:float, distancePerpendicular:float, previousInPlane:UnitVec, nextInPlane:UnitVec, perpendicular:UnitVec, cosine:float) : unit =
        #if DEBUG || CHECKED_EUCLID // CHECKED_EUCLID so checks can still be enabled when using with Fable release mode
            if cosine < -0.9999999 then
                fail $"Offset3D.setOffCorner: cosine is {cosine} , a 180 degree U-turn, which is not allowed here for distanceInPlane {distanceInPlane}, previousInPlane {previousInPlane}, nextInPlane {nextInPlane} at pt ({x}, {y}, {z})."
        #endif
            // based on https://www.angusj.com/clipper2/Docs/Trigonometry.htm
            let k = distanceInPlane / (1.0 + cosine)
            res.Add (x + (previousInPlane.X + nextInPlane.X) * k + perpendicular.X * distancePerpendicular)
            res.Add (y + (previousInPlane.Y + nextInPlane.Y) * k + perpendicular.Y * distancePerpendicular)
            res.Add (z + (previousInPlane.Z + nextInPlane.Z) * k + perpendicular.Z * distancePerpendicular)


    /// The offset point for different in-plane distances: where the in-plane offset lines of the two segments meet.
    /// The corner of the offset by distanceInPlaneNext is moved along the next offset line until it is at distanceInPlanePrev from the previous segment.
    /// vNext is the unit vector of the next segment. The dot product of previousInPlane and vNext must not be zero.
    let inline internal setOffCornerVar (res:ResizeArray<float>, x:float, y:float, z:float, distanceInPlanePrev:float, distanceInPlaneNext:float, distancePerpendicular:float, previousInPlane:UnitVec, nextInPlane:UnitVec, perpendicular:UnitVec, cosine:float, vNext:UnitVec) : unit =
        let delta = distanceInPlanePrev - distanceInPlaneNext
        let cos2 = previousInPlane *** vNext
        let a = delta / cos2
        let common = distanceInPlaneNext / (1.0 + cosine)
        res.Add (x + vNext.X * a + (previousInPlane.X + nextInPlane.X) * common + perpendicular.X * distancePerpendicular)
        res.Add (y + vNext.Y * a + (previousInPlane.Y + nextInPlane.Y) * common + perpendicular.Y * distancePerpendicular)
        res.Add (z + vNext.Z * a + (previousInPlane.Z + nextInPlane.Z) * common + perpendicular.Z * distancePerpendicular)


    /// For every marked point, finds the nearest points before and after it that are not marked.
    /// Works also for closed polylines where initial and end segments are collinear, the search wraps around the seam.
    /// Fails if no point is unmarked.
    let internal getNeighborsOfMarked (marked: bool[]) : ResizeArray<MarkedPoint> =
        let mutable prevIdx = -1
        let mutable nextIdx = -1
        let mutable j = -1
        let markedPts = ResizeArray<MarkedPoint>()
        let count = marked.Length

        for i = 0 to count - 1 do
            if marked.[i] then
                // check if we need to find new line from prev and next, to project onto
                if nextIdx < i then
                    // (2.1.1) find the previous unmarked point
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

                        if j = -1 then // no point is unmarked
                            fail $"Offset3D.getNeighborsOfMarked: could not find an unmarked point before point[{i}], none of the {count} points is unmarked."


                    // (2.2.1) find the next unmarked point
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

                        if j = count then // no point is unmarked
                            fail $"Offset3D.getNeighborsOfMarked: could not find an unmarked point after point[{i}], none of the {count} points is unmarked."

                // (2.3) add the marked point with its previous and next unmarked indices even if they were found earlier
                markedPts.Add { Index = i; PreviousOK = prevIdx; NextOK = nextIdx }

        markedPts

    /// <summary>Offsets a polyline by a constant distance.</summary>
    /// <param name="xyzs">The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.
    /// It is closed if the first and last point are within 1e-6, then the last offset point is the first.</param>
    /// <param name="dirs">The offset directions at each point of the polyline, as from getOffsetDirections.</param>
    /// <param name="distanceInPlane">The distance to offset in the local plane of each point.</param>
    /// <param name="distancePerpendicular">The distance to offset perpendicular to the local plane of each point.</param>
    /// <returns>The offset points as a ResizeArray of interleaved coordinates.</returns>
    let offsetWithDirections(xyzs:ResizeArray<float>, dirs: ResizeArray<OffsetDirection>, distanceInPlane:float, distancePerpendicular:float) : ResizeArray<float> =
        XYZ.checkMod3 "offsetWithDirections" xyzs
        let ptCount = XYZ.pointCount xyzs
        if ptCount < 2 then
            fail $"Offset3D.offsetWithDirections: point count {ptCount} must be at least 2 for a polyline."
        if ptCount <> dirs.Count then
            fail $"Offset3D.offsetWithDirections: point count must equal offset directions count, but they are {ptCount} and {dirs.Count}."
        let isClosed = XYZ.sqDistFirstLast xyzs < openToleranceSq
        let res = ResizeArray<float>(xyzs.Count)
        let lastPt = if isClosed then ptCount - 2 else ptCount - 1
        for i = 0 to lastPt do
            let dir = ResizeArr.getIdx i dirs
            let cosine = dir.PreviousInPlane *** dir.NextInPlane
            setOffCorner(res, XYZ.getX i xyzs, XYZ.getY i xyzs, XYZ.getZ i xyzs, distanceInPlane, distancePerpendicular, dir.PreviousInPlane, dir.NextInPlane, dir.Perpendicular, cosine)
        if isClosed then
            XYZ.add res.[0] res.[1] res.[2] res // the last point is the first point
        res

    /// <summary>Offsets a polyline by variable distances.</summary>
    /// <remarks>Each point is offset in its local plane where the in-plane offset lines of its two segments meet,
    /// and then by its own perpendicular distance along the perpendicular direction of its local plane.
    /// If the in-plane distances of the two segments differ at a collinear point, the offset lines meet far away or not at all, and varDistParallelBehavior decides.
    /// A point is collinear here if it turns by less than considerCollinearBelow,
    /// and the polyline also turns by less than that, from the segment after the last point that was offset exactly to the segment after this point.
    /// So a finely sampled arc still gets an exact point at least every few points.
    /// Proportional and Project place the collinear points on the line between the in-plane offsets of the nearest exact points.</remarks>
    /// <param name="xyzs">The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.
    /// It is closed if the first and last point are within 1e-6, then the last offset point is the first.</param>
    /// <param name="uvs">The X, Y and Z interleaved segment unit vectors, as from getSegmentUnitVectors.</param>
    /// <param name="dirs">The offset directions at each point of the polyline, as from getOffsetDirections.</param>
    /// <param name="distancesInPlane">The offset distances in the local plane, one per segment.
    /// Each segment is offset parallel to itself by its own distance.</param>
    /// <param name="distancesPerpendicular">The offset distances perpendicular to the local plane, one per point.
    /// For a closed polyline one less, because its last point is its first point.</param>
    /// <param name="varDistParallelBehavior">The behavior to use at collinear points where the two segments have different in-plane distances.</param>
    /// <param name="considerCollinearBelow">Points where the segments turn by less than this angle are collinear,
    /// unless the polyline turned by more than this angle since the last point that was offset exactly.</param>
    /// <returns>The offset points as a ResizeArray of interleaved coordinates.
    /// With VarDistParallel.Skip there may be fewer points than in the input.</returns>
    let offsetVariableWithDirections(xyzs:ResizeArray<float>,
                                     uvs:ResizeArray<float>,
                                     dirs: ResizeArray<OffsetDirection>,
                                     distancesInPlane:Collections.Generic.IList<float>,
                                     distancesPerpendicular:Collections.Generic.IList<float>,
                                     varDistParallelBehavior: VarDistParallel,
                                     considerCollinearBelow: float<Cosine.cosine>
                                     ) : ResizeArray<float> =
        XYZ.checkMod3 "offsetVariableWithDirections" xyzs
        XYZ.checkMod3 "offsetVariableWithDirections segment vectors" uvs
        let ptCount = XYZ.pointCount xyzs
        let segCount = XYZ.pointCount uvs
        if ptCount < 2 then
            fail $"Offset3D.offsetVariableWithDirections: point count {ptCount} must be at least 2 for a polyline."

        if ptCount <> segCount + 1  then
            fail $"Offset3D.offsetVariableWithDirections:\n Segment directions count must be {ptCount-1} for {ptCount} points, but is {segCount}."

        if ptCount <> dirs.Count then
            fail $"Offset3D.offsetVariableWithDirections:\n offset directions count must be {ptCount} for {ptCount} points, but is {dirs.Count}."

        if ptCount <> distancesInPlane.Count + 1  then
            fail $"Offset3D.offsetVariableWithDirections:\n in-plane distances count must be {ptCount-1} for {ptCount} points, but is {distancesInPlane.Count}."

        let isClosed = XYZ.sqDistFirstLast xyzs < openToleranceSq
        if isClosed then
            if ptCount <> distancesPerpendicular.Count + 1 then
                fail $"Offset3D.offsetVariableWithDirections:\n perpendicular distances count must be {ptCount-1} for the {ptCount} points of a closed polyline, one per point except the last, which is the first, but is {distancesPerpendicular.Count}."
        elif ptCount <> distancesPerpendicular.Count then
            fail $"Offset3D.offsetVariableWithDirections:\n perpendicular distances count must be {ptCount} for the {ptCount} points of an open polyline, one per point, but is {distancesPerpendicular.Count}."

        let lastSeg = segCount - 1
        let count = if isClosed then ptCount - 1 else ptCount // the points to offset, the last point of a closed polyline is the first

        // the segments before and after a point, the end points of an open polyline use their own segment twice, and so their distances
        let inline prevSeg i = if i > 0 then i - 1 elif isClosed then lastSeg else 0
        let inline nextSeg i = min i lastSeg
        let inline prevDistAt i = IList.getIdx (prevSeg i) distancesInPlane
        let inline nextDistAt i = IList.getIdx (nextSeg i) distancesInPlane
        let inline isSameDist i = abs (prevDistAt i - nextDistAt i) < 1e-6
        // the in-plane offset lines of the two segments meet at a finite point
        let inline linesMeet i = not (isTooSmall (abs ((ResizeArr.getIdx i dirs).PreviousInPlane *** XYZ.getUnitVec (nextSeg i) uvs)))
        // the point is offset exactly no matter where the scan starts
        let isExact i =
            (not isClosed && (i = 0 || i = ptCount - 1))
            || isSameDist i
            || (withMeasure (XYZ.dot (prevSeg i) (nextSeg i) uvs) <= considerCollinearBelow && linesMeet i)

        // (1) find the collinear points with different distances, that need VarDistParallel.
        // The scan starts at a point that is offset exactly, so that a closed polyline gives the same points for any start point.
        let needsFix = Array.zeroCreate<bool> ptCount
        let mutable fixCount = 0
        let mutable start = 0
        if isClosed then
            start <- -1
            let mutable i = 0
            while start = -1 && i < count do
                if isExact i then start <- i
                i <- i + 1
            if start = -1 then
                // every point is collinear with different distances, like on a finely sampled circle:
                // start at the first of the points where the offset lines meet at the biggest angle, so that rounding does not decide
                let inline sineAt j = abs ((ResizeArr.getIdx j dirs).PreviousInPlane *** XYZ.getUnitVec (nextSeg j) uvs)
                let mutable best = 0.0
                for j = 0 to count - 1 do
                    best <- max best (sineAt j)
                if isTooSmall best then
                    fail $"Offset3D.offsetVariableWithDirections: no point of the closed polyline with {ptCount} points turns, so no point can be offset where the offset lines of different distances meet."
                let mutable j = 0
                while start = -1 do
                    if isTooSmall (best - sineAt j) then start <- j
                    j <- j + 1
        let steps = if isClosed then count - 1 else count - 2
        let mutable runSeg = nextSeg start // the first segment after the last point that is offset exactly
        for k = 1 to steps do
            let i = (start + k) % count
            if isExact i || (withMeasure (XYZ.dot runSeg (nextSeg i) uvs) <= considerCollinearBelow && linesMeet i) then
                runSeg <- nextSeg i
            else
                needsFix.[i] <- true
                fixCount <- fixCount + 1
        if isClosed then
            needsFix.[ptCount - 1] <- needsFix.[0]

        // (2) first pass: the in-plane offsets of the points that are offset exactly.
        // Their perpendicular offsets are added in the third pass, so that the second pass can place the collinear points in-plane.
        let mutable res = ResizeArray<float>(xyzs.Count)
        for i = 0 to count - 1 do
            let x = XYZ.getX i xyzs
            let y = XYZ.getY i xyzs
            let z = XYZ.getZ i xyzs
            if needsFix.[i] then
                XYZ.add x y z res // for now just keep the input point, it is placed in the second pass
            else
                let dir = ResizeArr.getIdx i dirs
                let prevDist = prevDistAt i
                let nextDist = nextDistAt i
                let cosine = dir.PreviousInPlane *** dir.NextInPlane
                if abs (prevDist - nextDist) < 1e-6 then
                    setOffCorner(res, x, y, z, nextDist, 0.0, dir.PreviousInPlane, dir.NextInPlane, dir.Perpendicular, cosine)
                else
                    setOffCornerVar(res, x, y, z, prevDist, nextDist, 0.0, dir.PreviousInPlane, dir.NextInPlane, dir.Perpendicular, cosine, XYZ.getUnitVec (nextSeg i) uvs)
        if isClosed then
            XYZ.add res.[0] res.[1] res.[2] res // the last point is the first point, a collinear one is placed in the second pass

        // (3) second pass: place the collinear points in-plane on the line between the neighboring exact points,
        // then offset them perpendicularly by their own distance
        let skip = if fixCount > 0 && varDistParallelBehavior = VarDistParallel.Skip then Array.zeroCreate<bool> ptCount else [||]
        if fixCount > 0 then
            let collinearPoints = getNeighborsOfMarked needsFix
            for j = 0 to collinearPoints.LastIndex do
                let cP = ResizeArr.getIdx j collinearPoints
                let i = cP.Index
                if not (isClosed && i = ptCount - 1) then // the last point of a closed polyline is copied from the first one below
                    match varDistParallelBehavior with
                    | VarDistParallel.Fail ->
                        fail $"Offset3D.offsetVariableWithDirections: point[{i}] is between collinear segments with the different in-plane distances {prevDistAt i} and {nextDistAt i}. Use another VarDistParallel behavior than Fail."

                    | VarDistParallel.Skip ->
                        skip.[i] <- true

                    | VarDistParallel.Proportional
                    | VarDistParallel.Project ->
                        if isTooSmallSq (XYZ.sqDist cP.PreviousOK cP.NextOK xyzs) then
                            fail $"Offset3D.offsetVariableWithDirections: point[{i}] is between collinear segments with the different in-plane distances {prevDistAt i} and {nextDistAt i}, but the only point that can be offset exactly is point[{cP.PreviousOK}], so VarDistParallel.{varDistParallelBehavior} has no line to place it on."
                        if varDistParallelBehavior = VarDistParallel.Proportional then
                            // at the same parameter as on the line between the neighboring input points
                            let t = closestParameter cP.PreviousOK cP.NextOK i xyzs
                            setLinePointAt i cP.PreviousOK cP.NextOK t res res
                        else
                            // where the line between the neighboring offset points crosses the plane through the input point perpendicular to the tangent,
                            // so for planar polylines along the in-plane normal, like in Offset2D
                            let a = prevSeg i
                            let tx = XYZ.getX a uvs + XYZ.getX i uvs
                            let ty = XYZ.getY a uvs + XYZ.getY i uvs
                            let tz = XYZ.getZ a uvs + XYZ.getZ i uvs
                            intersectLineWithNormalPlaneInPlace i cP.PreviousOK cP.NextOK tx ty tz xyzs res

                    |_  ->
                        fail $"Offset3D.offsetVariableWithDirections: unknown VarDistParallel behavior {varDistParallelBehavior}."

                    // (3.1) the perpendicular offset by the point's own distance, skipped points need none
                    let distPerp = IList.getIdx i distancesPerpendicular
                    if distPerp <> 0.0 && not (skip.Length > 0 && skip.[i]) then
                        let p = (ResizeArr.getIdx i dirs).Perpendicular
                        XYZ.set i (XYZ.getX i res + p.X * distPerp) (XYZ.getY i res + p.Y * distPerp) (XYZ.getZ i res + p.Z * distPerp) res

        // (4) third pass: the perpendicular offsets of the points that are offset exactly
        for i = 0 to count - 1 do
            if not needsFix.[i] then
                let p = (ResizeArr.getIdx i dirs).Perpendicular
                let distPerp = IList.getIdx i distancesPerpendicular
                XYZ.set i (XYZ.getX i res + p.X * distPerp) (XYZ.getY i res + p.Y * distPerp) (XYZ.getZ i res + p.Z * distPerp) res

        if isClosed then
            XYZ.set (ptCount - 1) res.[0] res.[1] res.[2] res

        if skip.Length > 0 then
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

    /// Sums up the cross products of all points around their center: Newell's method.
    /// The result is not unitized, its length is twice the area of the polygon projected onto that plane.
    /// Does not check for bad input, the result may be zero length, e.g. if all points are in one line.
    /// There must be at least one point.
    let internal averageNormal (xyzs: ResizeArray<float>) : Vec =
        let len = xyzs.Count
        // the center of all points
        let mutable ox = 0.0
        let mutable oy = 0.0
        let mutable oz = 0.0
        let mutable i = 0
        while i < len do
            ox <- ox + ResizeArr.getIdx i xyzs
            oy <- oy + ResizeArr.getIdx (i + 1) xyzs
            oz <- oz + ResizeArr.getIdx (i + 2) xyzs
            i <- i + 3
        let count = float (len / 3) // divide, just like Polyline3D.Center does, so the same center is used
        ox <- ox / count
        oy <- oy / count
        oz <- oz / count
        let mutable nx = 0.0
        let mutable ny = 0.0
        let mutable nz = 0.0
        let mutable ax = xyzs.[len - 3] - ox // the previous point, starting at the last one to close the loop
        let mutable ay = xyzs.[len - 2] - oy
        let mutable az = xyzs.[len - 1] - oz
        i <- 0
        while i < len do
            let bx = ResizeArr.getIdx i xyzs - ox
            let by = ResizeArr.getIdx (i + 1) xyzs - oy
            let bz = ResizeArr.getIdx (i + 2) xyzs - oz
            nx <- nx + ay * bz - az * by
            ny <- ny + az * bx - ax * bz
            nz <- nz + ax * by - ay * bx
            ax <- bx
            ay <- by
            az <- bz
            i <- i + 3
        Vec(nx, ny, nz)

    /// The unitized average normal of the polyline, for the offset functions that compute their reference normal.
    /// Fails with a clear message if there are less than two points or if the average normal is shorter than 1e-6,
    /// like Polyline3D.AverageNormal.
    let internal averageRefNormal (functionName:string) (byNormalName:string) (xyzs: ResizeArray<float>) : UnitVec =
        XYZ.checkMod3 functionName xyzs
        let ptCount = XYZ.pointCount xyzs
        if ptCount < 2 then
            fail $"Offset3D.{functionName}: point count {ptCount} must be at least 2 for a polyline."
        let n = averageNormal xyzs
        if isTooSmallSq n.LengthSq then
            fail $"Offset3D.{functionName}: Cannot compute the average normal, the polygon through the {ptCount} points has no area, for example because they are collinear. Use {byNormalName} with an explicit reference normal."
        n.Unitized

    // #endregion
    // #region Curried API

    /// <summary> A constant-distance offset algorithm for closed or open polylines in 3D, with all settings given explicitly.
    /// Note that considerCollinearBelow comes before failAtUTurnAbove, both are cosine measures.</summary>
    /// <param name="xyzs"> The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.</param>
    /// <param name="distanceInPlane">The offset distance in the local plane defined by two segments.
    /// A positive distance offsets to the inside of a polyline that turns counter-clockwise around refNormal,
    /// and to the outside of one that turns clockwise. A negative distance offsets the other way.</param>
    /// <param name="distancePerpendicular">The offset distance perpendicular to the local plane.
    /// A positive distance offsets to the side of refNormal, a negative distance to the opposite side.</param>
    /// <param name="refNormal">The reference normal vector to use for orienting the local planes.
    /// For a counter-clockwise polyline in the XY plane this is upwards. If it points down, the offset direction is flipped.</param>
    /// <param name="considerCollinearBelow">The cosine of the angle below which segments are considered collinear, the other functions use 2.5 degrees.
    /// A collinear point takes the plane interpolated between the neighboring corners.</param>
    /// <param name="failAtUTurnAbove">The cosine of the angle above which a U-turn is considered to fail, the other functions use 175 degrees.
    /// There is no U-turn behavior like Offset2D.UTurn, on purpose: two almost opposite segments span no reliable plane in 3D, so a chamfer or threshold point has no defined direction. Remove U-turns first, for example with Polyline3D.removeUTurns.</param>
    /// <returns> A new ResizeArray of interleaved coordinates.</returns>
    let offsetByNormalCustom (xyzs: ResizeArray<float>,
                              distanceInPlane: float,
                              distancePerpendicular: float,
                              refNormal: UnitVec,
                              considerCollinearBelow: float<Cosine.cosine>,
                              failAtUTurnAbove: float<Cosine.cosine>) : ResizeArray<float> =
        let uvs = getSegmentUnitVectors xyzs
        let dirs = getOffsetDirections(xyzs, uvs, refNormal, considerCollinearBelow, failAtUTurnAbove)
        offsetWithDirections(xyzs, dirs, distanceInPlane, distancePerpendicular)

    /// <summary> A constant-distance offset algorithm for closed or open polylines in 3D.
    /// Uses default angles: collinear below 2.5 degrees, fails at U-turns above 175 degrees.</summary>
    /// <param name="refNormal">The reference normal vector to use for orienting the local planes.
    /// For a counter-clockwise polyline in the XY plane this is upwards. If it points down, the offset direction is flipped.</param>
    /// <param name="distanceInPlane">The offset distance in the local plane defined by two segments.
    /// A positive distance offsets to the inside of a polyline that turns counter-clockwise around refNormal,
    /// and to the outside of one that turns clockwise. A negative distance offsets the other way.</param>
    /// <param name="distancePerpendicular">The offset distance perpendicular to the local plane.
    /// A positive distance offsets to the side of refNormal, a negative distance to the opposite side.</param>
    /// <param name="xyzs"> The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.</param>
    /// <returns> A new ResizeArray of interleaved coordinates.</returns>
    let offsetByNormal (refNormal:UnitVec) (distanceInPlane: float) (distancePerpendicular: float) (xyzs:ResizeArray<float>): ResizeArray<float> =
        offsetByNormalCustom(xyzs, distanceInPlane, distancePerpendicular, refNormal, Cosine.``2.5``, Cosine.``175.0``)

    /// <summary> A constant-distance offset algorithm for closed or open polylines in 3D, with all settings given explicitly.
    /// The reference normal is the average normal of the polyline.
    /// Note that considerCollinearBelow comes before failAtUTurnAbove, both are cosine measures.</summary>
    /// <param name="xyzs"> The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.</param>
    /// <param name="distanceInPlane">The offset distance in the local plane defined by two segments.
    /// A positive distance offsets to the inside of the polyline, a negative distance to the outside, no matter how the polyline is oriented.</param>
    /// <param name="distancePerpendicular">The offset distance perpendicular to the local plane.
    /// A positive distance offsets in the direction of the computed normal. For a counter-clockwise polyline in xy-plane this is Upwards.
    /// A negative distance offsets in the opposite direction.</param>
    /// <param name="considerCollinearBelow">The cosine of the angle below which segments are considered collinear, the other functions use 2.5 degrees.
    /// A collinear point takes the plane interpolated between the neighboring corners.</param>
    /// <param name="failAtUTurnAbove">The cosine of the angle above which a U-turn is considered to fail, the other functions use 175 degrees.
    /// There is no U-turn behavior like Offset2D.UTurn, on purpose: two almost opposite segments span no reliable plane in 3D, so a chamfer or threshold point has no defined direction. Remove U-turns first, for example with Polyline3D.removeUTurns.</param>
    /// <returns> A new ResizeArray of interleaved coordinates.</returns>
    let offsetCustom (xyzs: ResizeArray<float>,
                      distanceInPlane: float,
                      distancePerpendicular: float,
                      considerCollinearBelow: float<Cosine.cosine>,
                      failAtUTurnAbove: float<Cosine.cosine>) : ResizeArray<float> =
        let refNormal = averageRefNormal "offsetCustom" "offsetByNormalCustom" xyzs
        offsetByNormalCustom(xyzs, distanceInPlane, distancePerpendicular, refNormal, considerCollinearBelow, failAtUTurnAbove)

    /// <summary> A constant-distance offset algorithm for closed or open polylines in 3D.
    /// The reference normal is the average normal of the polyline.
    /// Uses default angles: collinear below 2.5 degrees, fails at U-turns above 175 degrees.</summary>
    /// <param name="distanceInPlane">The offset distance in the local plane defined by two segments.
    /// A positive distance offsets to the inside of the polyline.
    /// A negative distance offsets to the outside.
    /// No matter how the polyline is oriented.</param>
    /// <param name="distancePerpendicular">The offset distance perpendicular to the local plane.
    /// A positive distance offsets in the direction of the computed normal.
    /// For a counter-clockwise polyline in xy-plane this is Upwards.
    /// A negative distance offsets in the opposite direction.</param>
    /// <param name="xyzs"> The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.</param>
    /// <returns> A new ResizeArray of interleaved coordinates.</returns>
    let offset (distanceInPlane: float) (distancePerpendicular: float) (xyzs: ResizeArray<float>) : ResizeArray<float> =
        let refNormal = averageRefNormal "offset" "offsetByNormal" xyzs
        offsetByNormalCustom(xyzs, distanceInPlane, distancePerpendicular, refNormal, Cosine.``2.5``, Cosine.``175.0``)


    /// <summary> Offsetting each segment by its own distance. For closed or open polylines in 3D, with all settings given explicitly.
    /// Note that considerCollinearBelow comes before failAtUTurnAbove, both are cosine measures.</summary>
    /// <param name="xyzs"> The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.</param>
    /// <param name="distancesInPlane">The offset distances in the local plane defined by two segments, one per segment.
    /// A positive distance offsets to the inside of a polyline that turns counter-clockwise around refNormal,
    /// and to the outside of one that turns clockwise. A negative distance offsets the other way.</param>
    /// <param name="distancesPerpendicular">The offset distances perpendicular to the local plane, one per point.
    /// For a closed polyline one less, because its last point is its first point.
    /// A positive distance offsets to the side of refNormal, a negative distance to the opposite side.</param>
    /// <param name="refNormal">The reference normal vector to use for orienting the local planes.
    /// For a counter-clockwise polyline in the XY plane this is upwards. If it points down, the offset direction is flipped.</param>
    /// <param name="varDistParallelBehavior">The behavior to use at collinear points where the two segments have different in-plane distances.</param>
    /// <param name="considerCollinearBelow">The cosine of the angle below which segments are considered collinear, the other functions use 2.5 degrees.</param>
    /// <param name="failAtUTurnAbove">The cosine of the angle above which a U-turn is considered to fail, the other functions use 175 degrees.
    /// There is no U-turn behavior like Offset2D.UTurn, on purpose: two almost opposite segments span no reliable plane in 3D, so a chamfer or threshold point has no defined direction. Remove U-turns first, for example with Polyline3D.removeUTurns.</param>
    /// <returns> A new ResizeArray of interleaved coordinates. If collinear segment handling is set to Skip the point count may differ from input.</returns>
    let offsetVariableByNormalCustom (xyzs: ResizeArray<float>,
                                      distancesInPlane: Collections.Generic.IList<float>,
                                      distancesPerpendicular: Collections.Generic.IList<float>,
                                      refNormal:UnitVec,
                                      varDistParallelBehavior: VarDistParallel,
                                      considerCollinearBelow: float<Cosine.cosine>,
                                      failAtUTurnAbove: float<Cosine.cosine>) : ResizeArray<float> =
        let uvs = getSegmentUnitVectors xyzs
        let dirs = getOffsetDirections(xyzs, uvs, refNormal, considerCollinearBelow, failAtUTurnAbove)
        offsetVariableWithDirections(xyzs, uvs, dirs, distancesInPlane, distancesPerpendicular, varDistParallelBehavior, considerCollinearBelow)


    /// <summary> Offsetting each segment by its own distance. For closed or open polylines in 3D.
    /// Fails at collinear points where the two segments have different in-plane distances (uses VarDistParallel.Fail).
    /// Uses default angles: collinear below 2.5 degrees, fails at U-turns above 175 degrees.</summary>
    /// <param name="refNormal">The reference normal vector to use for orienting the local planes.
    /// For a counter-clockwise polyline in the XY plane this is upwards. If it points down, the offset direction is flipped.</param>
    /// <param name="distancesInPlane">The offset distances in the local plane defined by two segments, one per segment.
    /// A positive distance offsets to the inside of a polyline that turns counter-clockwise around refNormal.</param>
    /// <param name="distancesPerpendicular">The offset distances perpendicular to the local plane, one per point.
    /// For a closed polyline one less, because its last point is its first point.</param>
    /// <param name="xyzs"> The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.</param>
    /// <returns> A new ResizeArray of interleaved coordinates.</returns>
    let offsetVariableByNormal (refNormal:UnitVec) (distancesInPlane: Collections.Generic.IList<float>) (distancesPerpendicular: Collections.Generic.IList<float>) (xyzs: ResizeArray<float>) : ResizeArray<float> =
        offsetVariableByNormalCustom(xyzs, distancesInPlane, distancesPerpendicular, refNormal, VarDistParallel.Fail, Cosine.``2.5``, Cosine.``175.0``)

    /// <summary> Offsetting each segment by its own distance. For closed or open polylines in 3D, with all settings given explicitly.
    /// The reference normal is the average normal of the polyline.
    /// Note that considerCollinearBelow comes before failAtUTurnAbove, both are cosine measures.</summary>
    /// <param name="xyzs"> The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.</param>
    /// <param name="distancesInPlane">The offset distances in the local plane defined by two segments, one per segment.
    /// A positive distance offsets to the inside of the polyline, a negative distance to the outside, no matter how the polyline is oriented.</param>
    /// <param name="distancesPerpendicular">The offset distances perpendicular to the local plane, one per point.
    /// For a closed polyline one less, because its last point is its first point.
    /// A positive distance offsets in the direction of the computed normal. For a counter-clockwise polyline in xy-plane this is Upwards.</param>
    /// <param name="varDistParallelBehavior">The behavior to use at collinear points where the two segments have different in-plane distances.</param>
    /// <param name="considerCollinearBelow">The cosine of the angle below which segments are considered collinear, the other functions use 2.5 degrees.</param>
    /// <param name="failAtUTurnAbove">The cosine of the angle above which a U-turn is considered to fail, the other functions use 175 degrees.
    /// There is no U-turn behavior like Offset2D.UTurn, on purpose: two almost opposite segments span no reliable plane in 3D, so a chamfer or threshold point has no defined direction. Remove U-turns first, for example with Polyline3D.removeUTurns.</param>
    /// <returns> A new ResizeArray of interleaved coordinates. If collinear segment handling is set to Skip the point count may differ from input.</returns>
    let offsetVariableCustom (xyzs: ResizeArray<float>,
                              distancesInPlane: Collections.Generic.IList<float>,
                              distancesPerpendicular: Collections.Generic.IList<float>,
                              varDistParallelBehavior: VarDistParallel,
                              considerCollinearBelow: float<Cosine.cosine>,
                              failAtUTurnAbove: float<Cosine.cosine>) : ResizeArray<float> =
        let refNormal = averageRefNormal "offsetVariableCustom" "offsetVariableByNormalCustom" xyzs
        offsetVariableByNormalCustom(xyzs, distancesInPlane, distancesPerpendicular, refNormal, varDistParallelBehavior, considerCollinearBelow, failAtUTurnAbove)


    /// <summary> Offsetting each segment by its own distance. For closed or open polylines in 3D.
    /// The reference normal is the average normal of the polyline.
    /// Fails at collinear points where the two segments have different in-plane distances (uses VarDistParallel.Fail).
    /// Uses default angles: collinear below 2.5 degrees, fails at U-turns above 175 degrees.</summary>
    /// <param name="distancesInPlane">The offset distances in the local plane defined by two segments, one per segment.
    /// A positive distance offsets to the inside of the polyline.
    /// A negative distance offsets to the outside.
    /// No matter how the polyline is oriented.</param>
    /// <param name="distancesPerpendicular">The offset distances perpendicular to the local plane, one per point.
    /// For a closed polyline one less, because its last point is its first point.
    /// A positive distance offsets in the direction of the computed normal.
    /// For a counter-clockwise polyline in xy-plane this is Upwards.
    /// A negative distance offsets in the opposite direction.</param>
    /// <param name="xyzs"> The interleaved coordinates (x0, y0, z0, x1, y1, z1, ...) of the polyline to offset.</param>
    /// <returns> A new ResizeArray of interleaved coordinates.</returns>
    let offsetVariable (distancesInPlane: Collections.Generic.IList<float>) (distancesPerpendicular: Collections.Generic.IList<float>) (xyzs: ResizeArray<float>) : ResizeArray<float> =
        let refNormal = averageRefNormal "offsetVariable" "offsetVariableByNormal" xyzs
        offsetVariableByNormalCustom(xyzs, distancesInPlane, distancesPerpendicular, refNormal, VarDistParallel.Fail, Cosine.``2.5``, Cosine.``175.0``)
