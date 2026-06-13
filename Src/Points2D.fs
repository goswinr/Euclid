namespace Euclid

open System
open UtilEuclid
open EuclidErrors
open System.Collections.Generic
open Euclid.EuclidCollectionUtilities

/// A type containing only static member functions for operating on multiple 2D points or set of 2D points.
/// Aka point-clouds
type Points2D   =


    /// The sign is negative if the loop is clockwise.
    /// Last and first point should be the same.
    static member getSignedArea(ps:IList<Pt>) : float = //
        //https://helloacm.com/sign-area-of-irregular-polygon/
        let mutable area = 0.0
        let mutable t = ps.[0]
        for i=1 to ps.Count-1 do
            let n = ps.[i]
            let a = t.X - n.X
            let b = n.Y + t.Y
            area <- area + a*b
            t <- n
        area * 0.5


    /// Returns the closest 2D point index from a 2D point list to a given 2D point.
    static member closestPointIdx (pts:IList<Pt>, pt:Pt) : int =
        if pts.Count = 0 then fail "Points2D.closestPoint: empty List of Points: pts"
        let mutable mi = -1
        let mutable mid = Double.MaxValue
        for i=0 to pts.Count - 1 do
            let p = pts.[i]
            let d = Pt.sqDist p pt
            if d < mid then
                mid <- d
                mi <- i
        mi

    /// Returns the closest 2D point from a point list to a given 2D point.
    static member closestPoint (pts:IList<Pt>, pt:Pt) : Pt =
        pts.[Points2D.closestPointIdx (pts, pt)]


    /// Returns the closest of two 2D points to a given reference 2D point.
    /// If both points are equidistant the first point is returned.
    static member closestOfTwo (pt1:Pt) (pt2:Pt) (referencePoint:Pt) : Pt =
        let d1 = Pt.sqDist pt1 referencePoint
        let d2 = Pt.sqDist pt2 referencePoint
        if d1 <= d2 then
            pt1
        else
            pt2


    /// Returns the indices of the 2D points that are closest to each other.
    static member closestPointsIdx (xs:IList<Pt>, ys:IList<Pt>) : int*int =
        //TODO
        // (1)
        // the current quadratic runtime could be optimized by first sorting the points in x (or y) direction
        // then the search loop could start when x distance is smaller than minD, and exit when it is bigger
        // (2)
        // alternatively a spatial hash could be used to cluster nearby objects. the challenge would be to find the right cell size for each point
        // (3)
        // the bounding Rectangles of each set could be intersected. then expanded. then used to filter both lists.
        if xs.Count = 0 then fail "Points2D.closestPointsIdx: empty List of Points: xs"
        if ys.Count = 0 then fail "Points2D.closestPointsIdx: empty List of Points: ys"
        let mutable xi = -1
        let mutable yj = -1
        let mutable minD = Double.MaxValue
        for i=0 to xs.Count-1 do
            let pt = xs.[i]
            for j=0 to ys.Count-1 do
                let d = Pt.sqDist pt ys.[j]
                if d < minD then
                    minD <- d
                    xi <- i
                    yj <- j
        xi, yj



    /// Given two lists of 2D points finds the pair that are closest to each other and returns their distance.
    static member minDistBetweenPointSets (xs:IList<Pt>, ys:IList<Pt>) : float =
        if xs.Count = 0 then fail "Points2D.minDistBetweenPointSets: empty List of Points: xs"
        if ys.Count = 0 then fail "Points2D.minDistBetweenPointSets: empty List of Points: ys"
        let i, j = Points2D.closestPointsIdx (xs, ys)
        Pt.dist xs.[i]  ys.[j]



    /// Find the index of the 2D point that has the biggest distance to any 2D point from the other set.
    /// Basically the most lonely point in 'findPointFrom' list with respect to 'checkAgainst' list.
    /// Returns findPointFromIdx * checkAgainstIdx
    static member mostDistantPointIdx (findPointFrom:IList<Pt>, checkAgainst:IList<Pt>) : int*int=
        if findPointFrom.Count = 0 then fail "Points2D.mostDistantPoint: empty List of Points: findPointFrom"
        if checkAgainst.Count = 0 then fail "Points2D.mostDistantPoint: empty List of Points: checkAgainst"
        let mutable maxD = Double.MinValue
        let mutable findPointFromIdx = -1
        let mutable checkAgainstTempIdx = -1
        let mutable checkAgainstIdx = -1
        for i=0 to findPointFrom.Count-1 do
            let pt = findPointFrom.[i]
            let mutable minD = Double.MaxValue
            for j=0 to checkAgainst.Count-1 do
                let d = Pt.sqDist pt checkAgainst.[j]
                if d < minD then
                    minD <- d
                    checkAgainstTempIdx <-j
            if minD > maxD then
                maxD <- minD
                findPointFromIdx <-i
                checkAgainstIdx <-checkAgainstTempIdx
        findPointFromIdx, checkAgainstIdx


    /// Find the 2D point that has the biggest distance to any 2D point from another set.
    static member mostDistantPoint (findPointFrom:IList<Pt>, checkAgainst:IList<Pt>) : Pt =
        let i, _ = Points2D.mostDistantPointIdx (findPointFrom, checkAgainst)
        findPointFrom.[i]

    /// Similar to join polylines, this tries to find continuous sequences of 2D points.
    /// 'tolGap' is the maximum allowable gap between the start and the endpoint of two segments.
    /// Search starts from the segment with the most points.
    /// Both start and end point of each 2D point list are checked for adjacency.
    [<Obsolete("This method has very poor performance. It is recommended to use a spatial hash to cluster nearby points instead.")>] // TODO
    static member findContinuousPoints (ptss: ResizeArray<ResizeArray<Pt>>, tolGap:float) : ResizeArray<Pt> =
        let i = ptss |> ResizeArr.maxIndexBy ResizeArr.len
        let res = ptss.PopAt(i)
        let mutable loop = true
        while loop && ptss.Count > 0 do
            //first try to append to end
            let ende = res.[res.Count-1]
            let si = ptss |> ResizeArr.minIndexBy (fun ps -> Pt.sqDist ende ps.First)
            let ei = ptss |> ResizeArr.minIndexBy (fun ps -> Pt.sqDist ende ps.Last)
            let sd = Pt.dist ende ptss.[si].First
            let ed = Pt.dist ende ptss.[ei].Last
            if   sd < tolGap && sd < ed then  res.AddRange(              ptss.PopAt(si))
            elif ed < tolGap && ed < sd then  res.AddRange(ResizeArr.rev(ptss.PopAt(ei)) )
            else
                //search from start
                let start = res.[0]
                let si = ptss |> ResizeArr.minIndexBy (fun ps -> Pt.sqDist start ps.First)
                let ei = ptss |> ResizeArr.minIndexBy (fun ps -> Pt.sqDist start ps.Last)
                let sd = Pt.dist start ptss.[si].First
                let ed = Pt.dist start ptss.[ei].Last
                if   sd < tolGap && sd < ed then res.InsertRange(0, ResizeArr.rev(ptss.PopAt(si)) )
                elif ed < tolGap && ed < sd then res.InsertRange(0,               ptss.PopAt(ei))
                else
                    loop <- false
        res


