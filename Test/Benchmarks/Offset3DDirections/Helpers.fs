namespace Euclid.Benchmarks

open System
open Euclid
open Euclid.UtilEuclid
open Euclid.EuclidErrors
open Euclid.EuclidCollectionUtilities

/// Copies of the internal helpers of Offset3D that do not index a ResizeArray, so that the variants can use them from this assembly.
module Shared =

    /// The squared tolerance for open polylines, like Offset3D.openToleranceSq.
    let [<Literal>] openToleranceSq = 1e-12

    /// For a marked point, the index of the point, and the indices of the nearest unmarked points before and after it.
    /// PreviousOK may be bigger than NextOK if the search wrapped around the seam of a closed polyline.
    [<NoComparison; NoEquality>]
    type MarkedPoint = {
        /// The index of the marked point.
        Index: int
        /// The index of the nearest unmarked point before it.
        PreviousOK: int
        /// The index of the nearest unmarked point after it.
        NextOK: int
    }

    /// Returns a Unit vector.
    /// Only to be used when the input vectors are known to be perpendicular.
    let inline crossProduct (a:UnitVec, b:UnitVec) : UnitVec =
        #if DEBUG
            if abs(a***b) > 1e-9 then
                fail $"Offset3D.crossProduct: input UnitVec are not perpendicular: dot product is {a***b}."
        #endif
            UnitVec.createUnchecked (a.Y * b.Z - a.Z * b.Y, a.Z * b.X - a.X * b.Z, a.X * b.Y - a.Y * b.X)

    /// The offset point for different in-plane distances: where the in-plane offset lines of the two segments meet.
    /// The corner of the offset by distanceInPlaneNext is moved along the next offset line until it is at distanceInPlanePrev from the previous segment.
    /// vNext is the unit vector of the next segment. The dot product of previousInPlane and vNext must not be zero.
    let inline setOffCornerVar (res:ResizeArray<float>, x:float, y:float, z:float, distanceInPlanePrev:float, distanceInPlaneNext:float, distancePerpendicular:float, previousInPlane:UnitVec, nextInPlane:UnitVec, perpendicular:UnitVec, cosine:float, vNext:UnitVec) : unit =
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
    let getNeighborsOfMarked (marked: bool[]) : ResizeArray<MarkedPoint> =
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


/// Copies of the private coordinate helpers of Offset3D before it used ResizeArr.getIdx.
/// Every read and write goes through the ResizeArray indexer, which Fable compiles to a bounds-checked call of a shared function.
module Checked =

    module XYZ =

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

    let inline rayClosestParameter(fromX:float, fromY:float, fromZ:float, toX:float, toY:float, toZ:float, pX:float, pY:float, pZ:float) : float =
        let vx = toX - fromX
        let vy = toY - fromY
        let vz = toZ - fromZ
        let lenSq = vx * vx + vy * vy + vz * vz
        if isTooSmallSq lenSq then
            fail $"Offset3D.rayClosestParameter: line from ({fromX}, {fromY}, {fromZ}) to ({toX}, {toY}, {toZ}) is too short."
        ((pX - fromX) * vx + (pY - fromY) * vy + (pZ - fromZ) * vz) / lenSq

    /// The parameter of the closest point to point idx on the line between the points fromIdx and toIdx, all in xyzs.
    let inline closestParameter (fromIdx:int) (toIdx:int) (idx:int) (xyzs: ResizeArray<float>) : float =
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

    let inline setLinePointAt (idx:int) (fromIdx:int) (toIdx:int) (t:float) (lineXYZs: ResizeArray<float>) (res: ResizeArray<float>) : unit =
        let fromX = XYZ.getX fromIdx lineXYZs
        let fromY = XYZ.getY fromIdx lineXYZs
        let fromZ = XYZ.getZ fromIdx lineXYZs
        let x = fromX + (XYZ.getX toIdx lineXYZs - fromX) * t
        let y = fromY + (XYZ.getY toIdx lineXYZs - fromY) * t
        let z = fromZ + (XYZ.getZ toIdx lineXYZs - fromZ) * t
        XYZ.set idx x y z res

    /// Places point idx where the line between the points fromIdx and toIdx in res crosses
    /// the plane through point idx of pointXYZs that is perpendicular to the tangent (tx, ty, tz).
    let intersectLineWithNormalPlaneInPlace (idx:int) (fromIdx:int) (toIdx:int) (tx:float) (ty:float) (tz:float) (pointXYZs: ResizeArray<float>) (res: ResizeArray<float>) : unit =
        let ax = XYZ.getX fromIdx res
        let ay = XYZ.getY fromIdx res
        let az = XYZ.getZ fromIdx res
        let denom = (XYZ.getX toIdx res - ax) * tx + (XYZ.getY toIdx res - ay) * ty + (XYZ.getZ toIdx res - az) * tz
        if isTooTiny (abs denom) then
            fail $"Offset3D.offsetVariableWithDirections: the line between the offset points[{fromIdx}] and [{toIdx}] is perpendicular to the polyline at point[{idx}], so VarDistParallel.Project cannot place it."
        let t = ((XYZ.getX idx pointXYZs - ax) * tx + (XYZ.getY idx pointXYZs - ay) * ty + (XYZ.getZ idx pointXYZs - az) * tz) / denom
        setLinePointAt idx fromIdx toIdx t res res


/// Copies of the private coordinate helpers of the shipped Offset3D.
/// Reads and writes go through ResizeArr.getIdx and ResizeArr.setIdx: plain array access in Fable Release, the ResizeArray indexer in .NET and in Fable Debug.
module Unchecked =

    module XYZ =

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

    let inline rayClosestParameter(fromX:float, fromY:float, fromZ:float, toX:float, toY:float, toZ:float, pX:float, pY:float, pZ:float) : float =
        let vx = toX - fromX
        let vy = toY - fromY
        let vz = toZ - fromZ
        let lenSq = vx * vx + vy * vy + vz * vz
        if isTooSmallSq lenSq then
            fail $"Offset3D.rayClosestParameter: line from ({fromX}, {fromY}, {fromZ}) to ({toX}, {toY}, {toZ}) is too short."
        ((pX - fromX) * vx + (pY - fromY) * vy + (pZ - fromZ) * vz) / lenSq

    /// The parameter of the closest point to point idx on the line between the points fromIdx and toIdx, all in xyzs.
    let inline closestParameter (fromIdx:int) (toIdx:int) (idx:int) (xyzs: ResizeArray<float>) : float =
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

    let inline setLinePointAt (idx:int) (fromIdx:int) (toIdx:int) (t:float) (lineXYZs: ResizeArray<float>) (res: ResizeArray<float>) : unit =
        let fromX = XYZ.getX fromIdx lineXYZs
        let fromY = XYZ.getY fromIdx lineXYZs
        let fromZ = XYZ.getZ fromIdx lineXYZs
        let x = fromX + (XYZ.getX toIdx lineXYZs - fromX) * t
        let y = fromY + (XYZ.getY toIdx lineXYZs - fromY) * t
        let z = fromZ + (XYZ.getZ toIdx lineXYZs - fromZ) * t
        XYZ.set idx x y z res

    /// Places point idx where the line between the points fromIdx and toIdx in res crosses
    /// the plane through point idx of pointXYZs that is perpendicular to the tangent (tx, ty, tz).
    let intersectLineWithNormalPlaneInPlace (idx:int) (fromIdx:int) (toIdx:int) (tx:float) (ty:float) (tz:float) (pointXYZs: ResizeArray<float>) (res: ResizeArray<float>) : unit =
        let ax = XYZ.getX fromIdx res
        let ay = XYZ.getY fromIdx res
        let az = XYZ.getZ fromIdx res
        let denom = (XYZ.getX toIdx res - ax) * tx + (XYZ.getY toIdx res - ay) * ty + (XYZ.getZ toIdx res - az) * tz
        if isTooTiny (abs denom) then
            fail $"Offset3D.offsetVariableWithDirections: the line between the offset points[{fromIdx}] and [{toIdx}] is perpendicular to the polyline at point[{idx}], so VarDistParallel.Project cannot place it."
        let t = ((XYZ.getX idx pointXYZs - ax) * tx + (XYZ.getY idx pointXYZs - ay) * ty + (XYZ.getZ idx pointXYZs - az) * tz) / denom
        setLinePointAt idx fromIdx toIdx t res res
