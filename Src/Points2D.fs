namespace Euclid

open System
open UtilEuclid
open EuclidErrors
open System.Collections.Generic

//  A static type containing functions for operating on multiple 2D points or set of 2D points. aka point-clouds
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
            let d = Pt.distanceSq p pt
            if d < mid then
                mid <- d
                mi <- i
        mi

    /// Returns the closest 2D point from a point list to a given 2D point.
    static member closestPoint (pts:IList<Pt>, pt:Pt) : Pt =
        pts.[Points2D.closestPointIdx (pts, pt)]


    /// Returns the closest of two 2D points to a given reference 2D point.
    /// If both points are equidistant the first point is returned.
    static member closestOfTwo (pt1:Pt) (pt2:Pt) (referencePoint:Pt) =
        let d1 = Pt.distanceSq pt1 referencePoint
        let d2 = Pt.distanceSq pt2 referencePoint
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
                let d = Pt.distanceSq pt ys.[j]
                if d < minD then
                    minD <- d
                    xi <- i
                    yj <- j
        xi, yj



    /// Given two lists of 2D points finds the pair that are closest to each other and returns their distance.
    static member minDistBetweenPointSets (xs:IList<Pt>, ys:IList<Pt>) : float =
        if xs.Count = 0 then fail "Points2D.minDistBetweenPointSets: empty List of Points: xs"
        if ys.Count = 0 then fail "Points2D.minDistBetweenPointSets: empty List of Points: ys"
        let (i, j) = Points2D.closestPointsIdx (xs, ys)
        Pt.distance xs.[i]  ys.[j]



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
                let d = Pt.distanceSq pt checkAgainst.[j]
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

    /// Culls 2D points if they are too close to previous or next item.
    /// Last and first 2D points stay the same.
    static member cullDuplicatePointsInSeq (pts:ResizeArray<Pt>, tolerance:float) : ResizeArray<Pt> =
        if pts.Count = 0 then fail "Points2D.cullDuplicatePointsInSeq: empty List of Points"
        if pts.Count = 1 then
            pts
        else
            let tolSq = tolerance*tolerance
            let res = ResizeArray(pts.Count)
            let mutable last = pts.[0]
            res.Add last
            let iLast = pts.Count-1
            for i = 1  to iLast do
                let pt = pts.[i]
                if Pt.distanceSq last pt > tolSq then
                    last <- pt
                    res.Add last
                elif i=iLast then // to ensure last point stays the same
                    res.RemoveAt(res.Count-1)
                    res.Add pt
            res




    /// Similar to join polylines, this tries to find continuous sequences of 2D points.
    /// 'tolGap' is the maximum allowable gap between the start and the endpoint of two segments.
    /// Search starts from the segment with the most points.
    /// Both start and end point of each 2D point list are checked for adjacency.
    static member findContinuousPoints (ptss: ResizeArray<ResizeArray<Pt>>, tolGap:float) : ResizeArray<Pt> =
        let i = ptss |> ResizeArr.maxIndexBy ResizeArr.length
        let res = ptss.Pop(i)
        let mutable loop = true
        while loop && ptss.Count > 0 do
            //first try to append to end
            let ende = res.[res.Count-1]
            let si = ptss |> ResizeArr.minIndexBy (fun ps -> Pt.distanceSq ende ps.First)
            let ei = ptss |> ResizeArr.minIndexBy (fun ps -> Pt.distanceSq ende ps.Last)
            let sd = Pt.distance ende ptss.[si].First
            let ed = Pt.distance ende ptss.[ei].Last
            if   sd < tolGap && sd < ed then  res.AddRange(                ptss.Pop(si))
            elif ed < tolGap && ed < sd then  res.AddRange(ResizeArr.rev(ptss.Pop(ei)) )
            else
                //search from start
                let start = res.[0]
                let si = ptss |> ResizeArr.minIndexBy (fun ps -> Pt.distanceSq start ps.First)
                let ei = ptss |> ResizeArr.minIndexBy (fun ps -> Pt.distanceSq start ps.Last)
                let sd = Pt.distance start ptss.[si].First
                let ed = Pt.distance start ptss.[ei].Last
                if   sd < tolGap && sd < ed then res.InsertRange(0, ResizeArr.rev(ptss.Pop(si)) )
                elif ed < tolGap && ed < sd then res.InsertRange(0,                 ptss.Pop(ei))
                else
                    loop <- false
        res



(* use Tria2D instead

    /// It finds the inner offset point in a corner (defined a point, a previous vector to this point and a next vector from this point)
    /// The offset from first and second segment are given separately and can vary (prevDist and nextDist).
    /// Use negative distance for outer offset.
    /// If Points are collinear by 0.25 degrees or less than 1-e6 units apart returns: ValueNone.
    /// Use negative distances to get outside offset.
    /// 'referenceOrient' is positive for counterclockwise loops otherwise negative.
    /// Returns the offset point, the shift direction for prev and next line.
    static member offsetInCornerEx2D(   thisPt:Pt,
                                        prevToThis:Vc,
                                        thisToNext:Vc,
                                        prevDist:float,
                                        nextDist:float,
                                        referenceOrient:float) : ValueOption<Pt*Vc*Vc> =
        if prevDist = 0. && nextDist = 0. then
            ValueSome (thisPt, Vc.Zero, Vc.Zero)
        else
            let ax = prevToThis.X
            let ay = prevToThis.Y
            let bx = thisToNext.X
            let by = thisToNext.Y
            let a = ax*ax + ay*ay // square length of A
            let c = bx*bx + by*by // square length of B
            if isTooSmallSq (c) then
                ValueNone
            elif isTooSmallSq (a) then
                ValueNone
            else
                let b = ax*bx + ay*by // dot product of both lines
                let ac = a*c // square of square length, never negative
                let bb = b*b // square of square dot product, never negative
                let discriminant = ac - bb // never negative, the dot product cannot be bigger than the two square length multiplied with each other
                let div = ac+bb // never negative
                let rel = discriminant/div
                if rel < float RelAngleDiscriminant.``0.25`` then //parallel
                    ValueNone
                else
                    let n = Vc.cross(prevToThis, thisToNext) |> UtilEuclid.matchSign referenceOrient
                    let prevShift = prevToThis.Rotate90CCW |> Vc.withLength (if n>0. then prevDist else -prevDist)
                    let nextShift = thisToNext.Rotate90CCW |> Vc.withLength (if n>0. then nextDist else -nextDist)
                    let offP = thisPt + prevShift
                    let offN = thisPt + nextShift
                    let vx = offN.X - offP.X
                    let vy = offN.Y - offP.Y
                    let e = bx*vx + by*vy
                    let d = ax*vx + ay*vy
                    let t = (c * d - b * e) / discriminant
                    let pt = offP + t * prevToThis
                    ValueSome (pt, prevShift, nextShift)

    /// It finds the inner offset point in a corner (defined by a Polyline from 3 points (prevPt, thisPt and nextPt)
    /// The offset from first and second segment are given separately and can vary (prevDist and nextDist).
    /// Use negative distance for outer offset.
    /// If Points are collinear by 0.25 degrees or less than 1-e6 units apart returns: ValueNone.
    /// Use negative distances to get outside offset.
    /// 'referenceOrient' is positive for counterclockwise loops otherwise negative.
    /// Returns the offset point, the shift direction for prev and next line.
    static member offsetInCornerEx2D(prevPt:Pt,
                                    thisPt:Pt,
                                    nextPt:Pt,
                                    prevDist:float,
                                    nextDist:float,
                                    referenceOrient:float) : ValueOption<Pt*Vc*Vc> =
        let prevV = thisPt - prevPt
        let nextV = nextPt - thisPt
        Points2D.offsetInCornerEx2D(thisPt, prevV, nextV, prevDist, nextDist, referenceOrient)


*)


