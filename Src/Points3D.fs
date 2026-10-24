namespace Euclid

open System
open UtilEuclid
open EuclidErrors
open System.Collections.Generic
open Euclid.EuclidCollectionUtilities


/// A type containing only static member functions for operating on multiple 3D points or set of 3D points.
/// Aka point-clouds
type Points3D   =


    /// Returns the closest 3D point index from a 3D point list to a given 3D point.
    static member closestPointIdx (pts:IList<Pnt>, pnt:Pnt) : int =
        if pts.Count = 0 then fail "Points3D.closestPoint: empty List of Points: pts"
        let mutable mi = -1
        let mutable mid = Double.MaxValue
        for i=0 to pts.Count - 1 do
            let p = pts.[i]
            let d = Pnt.sqDist p pnt
            if d < mid then
                mid <- d
                mi <- i
        mi

    /// Returns the closest point from a 3D point list to a given 3D point.
    static member closestPoint (pts:IList<Pnt>, pnt:Pnt) : Pnt=
        pts.[Points3D.closestPointIdx (pts, pnt)]

    /// Returns the closest of two 3D points to a given reference 3D point.
    /// If both points are equidistant the first point is returned.
    static member closestOfTwo (pnt1:Pnt) (pnt2:Pnt) (referencePoint:Pnt) : Pnt =
        let d1 = Pnt.sqDist pnt1 referencePoint
        let d2 = Pnt.sqDist pnt2 referencePoint
        if d1 <= d2 then
            pnt1
        else
            pnt2


    /// Returns the indices of the 3D points that are closest to each other.
    static member closestPointsIdx (xs:IList<Pnt>, ys:IList<Pnt>) : int * int =
        if xs.Count = 0 then fail "Points3D.closestPointsIdx: empty List of Points: xs"
        if ys.Count = 0 then fail "Points3D.closestPointsIdx: empty List of Points: ys"
        let mutable xi = -1
        let mutable yj = -1
        let mutable minD = Double.MaxValue
        for i=0 to xs.Count-1 do
            let pnt = xs.[i]
            for j=0 to ys.Count-1 do
                let d = Pnt.sqDist pnt ys.[j]
                if d < minD then
                    minD <- d
                    xi <- i
                    yj <- j
        xi, yj


    /// Given two lists of 3D points finds the pair that are closest to each other and returns their distance.
    static member minDistBetweenPointSets (xs:IList<Pnt>, ys:IList<Pnt>) : float =
        if xs.Count = 0 then fail "Points3D.minDistBetweenPointSets: empty List of Points: xs"
        if ys.Count = 0 then fail "Points3D.minDistBetweenPointSets: empty List of Points: ys"
        let (i, j) = Points3D.closestPointsIdx (xs, ys)
        Pnt.dist xs.[i]  ys.[j]



    /// Find the index of the 3D point that has the biggest distance to any 3D point from the other set.
    /// Basically the most lonely point in 'findPointFrom' list with respect to 'checkAgainst' list.
    /// Returns findPointFromIdx * checkAgainstIdx
    static member mostDistantPointIdx (findPointFrom:IList<Pnt>, checkAgainst:IList<Pnt>) : int*int=
        if findPointFrom.Count = 0 then fail "Points3D.mostDistantPoint: empty List of Points: findPointFrom"
        if checkAgainst.Count = 0 then fail "Points3D.mostDistantPoint: empty List of Points: checkAgainst"
        let mutable maxD = Double.MinValue
        let mutable findPointFromIdx = -1
        let mutable checkAgainstTempIdx = -1
        let mutable checkAgainstIdx = -1
        for i=0 to findPointFrom.Count-1 do
            let pnt = findPointFrom.[i]
            let mutable minD = Double.MaxValue
            for j=0 to checkAgainst.Count-1 do
                let d = Pnt.sqDist pnt checkAgainst.[j]
                if d < minD then
                    minD <- d
                    checkAgainstTempIdx <-j
            if minD > maxD then
                maxD <- minD
                findPointFromIdx <-i
                checkAgainstIdx <-checkAgainstTempIdx
        findPointFromIdx, checkAgainstIdx


    /// Find the 3D point that has the biggest distance to any 3D point from another set.
    static member mostDistantPoint (findPointFrom:IList<Pnt>, checkAgainst:IList<Pnt>) : Pnt =
        let i, _ = Points3D.mostDistantPointIdx (findPointFrom, checkAgainst)
        findPointFrom.[i]


    /// Finds the center, mean, or average point.
    static member center (pts: IList<Pnt>) : Pnt =
        if pts.Count = 0 then failDivide "Points3D.center: empty list of points" 0.0 pts
        let mutable x = 0.0
        let mutable y = 0.0
        let mutable z = 0.0
        for i = 0 to pts.Count-1 do
            let pnt = pts.[i]
            x <- x + pnt.X
            y <- y + pnt.Y
            z <- z + pnt.Z
        let n = float pts.Count
        Pnt(x / n, y / n, z / n)

    /// Finds the mean normal of many points.
    /// It finds the center point and then takes cross-products, iterating all points in pairs of two.
    /// The first three points define the orientation of the normal.
    /// So it considers the current order of points too.
    /// If the order is counter-clockwise in the World X-Y plane, then the normal is in world Z orientation.
    /// The sweep from the first to the second point (from the mean center) defines the orientation of the normal.
    static member normalOfPoints(pts: IList<Pnt>) : Vec =
        if pts.Count <= 2 then
            fail $"Points3D.normalOfPoints can't find normal of two or less points: {Format.iList pts}"

        if pts.Count = 3  then
            let p0 = pts.[0]
            let p1 = pts.[1]
            let p2 = pts.[2]
            // a = p0 - p1 and b = p2 - p1
            let ax = p0.X - p1.X
            let ay = p0.Y - p1.Y
            let az = p0.Z - p1.Z
            let bx = p2.X - p1.X
            let by = p2.Y - p1.Y
            let bz = p2.Z - p1.Z
            // cross product b x a
            let vx = by * az - bz * ay
            let vy = bz * ax - bx * az
            let vz = bx * ay - by * ax
            if isTooSmallSq (vx*vx + vy*vy + vz*vz) then
                fail $"Points3D.normalOfPoints: three points are in a line: {Format.iList pts}"
            Vec(vx, vy, vz)
        else
            let cen = pts |> Points3D.center
            let cx = cen.X
            let cy = cen.Y
            let cz = cen.Z
            let mutable vx = 0.0
            let mutable vy = 0.0
            let mutable vz = 0.0
            // the previous point, relative to the center
            let mutable tx = pts.[0].X - cx
            let mutable ty = pts.[0].Y - cy
            let mutable tz = pts.[0].Z - cz
            for i=1 to pts.Count-1 do
                let p = pts.[i]
                // this point, relative to the center
                let nx = p.X - cx
                let ny = p.Y - cy
                let nz = p.Z - cz
                // cross product t x n
                let x = ty * nz - tz * ny
                let y = tz * nx - tx * nz
                let z = tx * ny - ty * nx
                // match the orientation of the sum so far // TODO do this matching? first cross product defines orientation
                if vx * x + vy * y + vz * z < 0.0 then
                    vx <- vx - x
                    vy <- vy - y
                    vz <- vz - z
                else
                    vx <- vx + x
                    vy <- vy + y
                    vz <- vz + z
                tx <- nx
                ty <- ny
                tz <- nz
            if isTooSmallSq (vx*vx + vy*vy + vz*vz) then
                fail $"Points3D.normalOfPoints: points are in a line or sphere without clear normal: {Format.iList pts}"
            Vec(vx, vy, vz)

// #endregion


