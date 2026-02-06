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
    static member closestPointIdx (pts:IList<Pnt>, pt:Pnt) : int =
        if pts.Count = 0 then fail "Points3D.closestPoint: empty List of Points: pts"
        let mutable mi = -1
        let mutable mid = Double.MaxValue
        for i=0 to pts.Count - 1 do
            let p = pts.[i]
            let d = Pnt.distanceSq p pt
            if d < mid then
                mid <- d
                mi <- i
        mi

    /// Returns the closest point from a 3D point list to a given 3D point.
    static member closestPoint (pts:IList<Pnt>, pt:Pnt) : Pnt=
        pts.[Points3D.closestPointIdx (pts, pt)]

    /// Returns the closest of two 3D points to a given reference 3D point.
    /// If both points are equidistant the first point is returned.
    static member closestOfTwo (pt1:Pnt) (pt2:Pnt) (referencePoint:Pnt) =
        let d1 = Pnt.distanceSq pt1 referencePoint
        let d2 = Pnt.distanceSq pt2 referencePoint
        if d1 <= d2 then
            pt1
        else
            pt2


    /// Returns the indices of the 3D points that are closest to each other.
    static member closestPointsIdx (xs:IList<Pnt>, ys:IList<Pnt>) =
        if xs.Count = 0 then fail "Points3D.closestPointsIdx: empty List of Points: xs"
        if ys.Count = 0 then fail "Points3D.closestPointsIdx: empty List of Points: ys"
        let mutable xi = -1
        let mutable yj = -1
        let mutable minD = Double.MaxValue
        for i=0 to xs.Count-1 do
            let pt = xs.[i]
            for j=0 to ys.Count-1 do
                let d = Pnt.distanceSq pt ys.[j]
                if d < minD then
                    minD <- d
                    xi <- i
                    yj <- j
        xi, yj


    /// Given two lists of 3D points finds the pair that are closest to each other and returns their distance.
    static member minDistBetweenPointSets (xs:IList<Pnt>, ys:IList<Pnt>) =
        if xs.Count = 0 then fail "Points3D.minDistBetweenPointSets: empty List of Points: xs"
        if ys.Count = 0 then fail "Points3D.minDistBetweenPointSets: empty List of Points: ys"
        let (i, j) = Points3D.closestPointsIdx (xs, ys)
        Pnt.distance xs.[i]  ys.[j]



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
            let pt = findPointFrom.[i]
            let mutable minD = Double.MaxValue
            for j=0 to checkAgainst.Count-1 do
                let d = Pnt.distanceSq pt checkAgainst.[j]
                if d < minD then
                    minD <- d
                    checkAgainstTempIdx <-j
            if minD > maxD then
                maxD <- minD
                findPointFromIdx <-i
                checkAgainstIdx <-checkAgainstTempIdx
        findPointFromIdx, checkAgainstIdx


    /// Find the 3D point that has the biggest distance to any 3D point from another set.
    static member mostDistantPoint (findPointFrom:IList<Pnt>, checkAgainst:IList<Pnt>) =
        let i, _ = Points3D.mostDistantPointIdx (findPointFrom, checkAgainst)
        findPointFrom.[i]


    /// Culls 3D points if they are too close to previous or next item.
    /// Last and first 3D points stay the same.
    static member cullDuplicatePointsInSeq (pts:ResizeArray<Pnt>, tolerance) :ResizeArray<Pnt> =
        if pts.Count = 0 then fail "Points3D.cullDuplicatePointsInSeq: empty List of Points"
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
                if Pnt.distanceSq last pt > tolSq then
                    last <- pt
                    res.Add last
                elif i=iLast then // to ensure last point stays the same
                    res.RemoveAt(res.Count-1)
                    res.Add pt
            res


    /// Similar to join polylines, this tries to find continuous sequences of 3D points.
    /// 'tolGap' is the maximum allowable gap between the start and the endpoint of two segments.
    /// Search starts from the segment with the most points.
    /// Both start and end point of each 3D point list are checked for adjacency.
    static member findContinuousPoints (ptss: ResizeArray<ResizeArray<Pnt>>, tolGap:float) =
        let i = ptss |> ResizeArr.maxIndexBy ResizeArr.length
        let res = ptss.Pop(i)
        let mutable loop = true
        while loop && ptss.Count > 0 do
            //first try to append to end
            let ende = res.[res.Count-1]
            let si = ptss |> ResizeArr.minIndexBy (fun ps -> Pnt.distanceSq ende ps.First)
            let ei = ptss |> ResizeArr.minIndexBy (fun ps -> Pnt.distanceSq ende ps.Last)
            let sd = Pnt.distance ende ptss.[si].First
            let ed = Pnt.distance ende ptss.[ei].Last
            if   sd < tolGap && sd < ed then  res.AddRange(                ptss.Pop(si))
            elif ed < tolGap && ed < sd then  res.AddRange(ResizeArr.rev(ptss.Pop(ei)) )
            else
                //search from start
                let start = res.[0]
                let si = ptss |> ResizeArr.minIndexBy (fun ps -> Pnt.distanceSq start ps.First)
                let ei = ptss |> ResizeArr.minIndexBy (fun ps -> Pnt.distanceSq start ps.Last)
                let sd = Pnt.distance start ptss.[si].First
                let ed = Pnt.distance start ptss.[ei].Last
                if   sd < tolGap && sd < ed then res.InsertRange(0, ResizeArr.rev(ptss.Pop(si)) )
                elif ed < tolGap && ed < sd then res.InsertRange(0,                 ptss.Pop(ei))
                else
                    loop <- false
        res

    /// Finds the center, mean, or average point.
    static member center (pts: IList<Pnt>) =
        let mutable sum = Pnt.Origin
        for i = 0 to pts.Count-1 do
            let pt = pts.[i]
            sum <- sum + pt
        sum / float pts.Count

    /// Finds the mean normal of many points.
    /// It finds the center point and then takes cross-products, iterating all points in pairs of two.
    /// The first three points define the orientation of the normal.
    /// So it considers the current order of points too.
    /// If the order is counterclockwise in the World X-Y plane, then the normal is in world Z orientation.
    /// The sweep from the first to the second point (from the mean center) defines the orientation of the normal.
    static member normalOfPoints(pts: IList<Pnt>) : Vec =
        if pts.Count <= 2 then
            fail $"Points3D.normalOfPoints can't find normal of two or less points: {Format.iList pts}"

        if pts.Count = 3  then
            let a = pts.[0] - pts.[1]
            let b = pts.[2] - pts.[1]
            let v= Vec.cross(b, a)
            if isTooSmallSq v.LengthSq  then
                fail $"Points3D.normalOfPoints: three points are in a line: {Format.iList pts}"
            v
        else
            let cen = pts |> Points3D.center
            let mutable v = Vec.Zero
            let mutable t = pts.[0]
            for i=1 to pts.Count-1 do
                let n = pts.[i]
                let x = Vec.cross(t-cen, n-cen)  |> Vec.matchOrientation v // TODO do this matching? first cross product defines orientation
                v <- v + x
                t <- n
            if isTooSmallSq v.LengthSq  then
                fail $"Points3D.normalOfPoints: points are in a line or sphere without clear normal: {Format.iList pts}"
            v



[<Obsolete("Use Points3D or Points2D static class instead")>]
type Points() =

    [<Obsolete("Use Tria3D.areaDouble instead")>]
    static member inline areaTriangleDoubleSq (a:Pnt, b:Pnt, c:Pnt) :float =
        Tria3D.areaDouble(a, b, c)

    [<Obsolete("Use Tria3D.area instead")>]
    static member inline areaTriangle (a:Pnt, b:Pnt, c:Pnt) :float =
        Tria3D.area(a, b, c)

    [<Obsolete("Use Tria3D.isLinearFast instead")>]
    static member inline areInLineFast (a:Pnt, b:Pnt, c:Pnt, [<OPT;DEF(0.001)>] maxSquareAreaParallelogram:float) =
        Tria3D.isLinearFast(a, b, c, maxSquareAreaParallelogram)

    [<Obsolete("Use Tria3D.isLinear instead")>]
    static member areInLine (a:Pnt, b:Pnt, c:Pnt, [<OPT;DEF(1e-6)>] distanceTolerance:float) =
        Tria3D.isLinear(a, b, c, distanceTolerance)

    [<Obsolete("Use Points2D.getSignedArea instead")>]
    static member getSignedArea(ps:ResizeArray<Pt>) =
        Points2D.getSignedArea(ps)

    [<Obsolete("Use Points3D.closestPointIdx instead")>]
    static member closestPointIdx (pts:ResizeArray<Pnt>, pt:Pnt) : int =
        Points3D.closestPointIdx(pts, pt)

    [<Obsolete("Use Points2D.closestPointIdx instead")>]
    static member closestPointIdx (pts:ResizeArray<Pt>, pt:Pt) : int =
        Points2D.closestPointIdx(pts, pt)

    [<Obsolete("Use Points3D.closestPoint instead")>]
    static member closestPoint (pts:ResizeArray<Pnt>, pt:Pnt) : Pnt=
        Points3D.closestPoint(pts, pt)

    [<Obsolete("Use Points2D.closestPoint instead")>]
    static member closestPoint (pts:ResizeArray<Pt>, pt:Pt) : Pt=
        Points2D.closestPoint(pts, pt)

    [<Obsolete("Use Points3D.closestPointsIdx instead")>]
    static member closestPointsIdx (xs:ResizeArray<Pnt>, ys:ResizeArray<Pnt>) =
        Points3D.closestPointsIdx(xs, ys)

    [<Obsolete("Use Points2D.closestPointsIdx instead")>]
    static member closestPointsIdx (xs:ResizeArray<Pt>, ys:ResizeArray<Pt>) =
        Points2D.closestPointsIdx(xs, ys)


    [<Obsolete("Use Tria3D.offsetVar instead")>]
    static member closestOfTwo (pt1:Pnt) (pt2:Pnt) (referencePoint:Pnt) =
        Points3D.closestOfTwo pt1 pt2 referencePoint


    [<Obsolete("Use Points3D.minDistBetweenPointSets instead")>]
    static member minDistBetweenPointSets (xs:ResizeArray<Pnt>, ys:ResizeArray<Pnt>) =
        Points3D.minDistBetweenPointSets(xs, ys)

    [<Obsolete("Use Points2D.minDistBetweenPointSets instead")>]
    static member minDistBetweenPointSets (xs:ResizeArray<Pt>, ys:ResizeArray<Pt>) =
        Points2D.minDistBetweenPointSets(xs, ys)

    [<Obsolete("Use Points3D.mostDistantPointIdx instead")>]
    static member mostDistantPointIdx (findPointFrom:ResizeArray<Pnt>, checkAgainst:ResizeArray<Pnt>) : int*int=
        Points3D.mostDistantPointIdx(findPointFrom, checkAgainst)

    [<Obsolete("Use Points2D.mostDistantPointIdx instead")>]
    static member mostDistantPointIdx (findPointFrom:ResizeArray<Pt>, checkAgainst:ResizeArray<Pt>) : int*int=
        Points2D.mostDistantPointIdx(findPointFrom, checkAgainst)

    [<Obsolete("Use Points3D.mostDistantPoint instead")>]
    static member mostDistantPoint (findPointFrom:ResizeArray<Pnt>, checkAgainst:ResizeArray<Pnt>) =
        Points3D.mostDistantPoint(findPointFrom, checkAgainst)

    [<Obsolete("Use Points2D.mostDistantPoint instead")>]
    static member mostDistantPoint (findPointFrom:ResizeArray<Pt>, checkAgainst:ResizeArray<Pt>) =
        Points2D.mostDistantPoint(findPointFrom, checkAgainst)

    [<Obsolete("Use Points3D.cullDuplicatePointsInSeq instead")>]
    static member cullDuplicatePointsInSeq (pts:ResizeArray<Pnt>, tolerance) =
        Points3D.cullDuplicatePointsInSeq(pts, tolerance)

    [<Obsolete("Use Points2D.cullDuplicatePointsInSeq instead")>]
    static member cullDuplicatePointsInSeq (pts:ResizeArray<Pt>, tolerance) =
        Points2D.cullDuplicatePointsInSeq(pts, tolerance)

    [<Obsolete("Use Points2D.findContinuousPoints instead")>]
    static member findContinuousPoints (ptss: ResizeArray<ResizeArray<Pt>>, tolGap:float) =
        Points2D.findContinuousPoints(ptss, tolGap)

    [<Obsolete("Use Points3D.findContinuousPoints instead")>]
    static member findContinuousPoints (ptss: ResizeArray<ResizeArray<Pnt>>, tolGap:float) =
        Points3D.findContinuousPoints(ptss, tolGap)


    [<Obsolete("Typo, use Points3D.findContinuousPoints instead")>]
    static member findContinuosPoints (ptss: ResizeArray<ResizeArray<Pnt>>, tolGap:float) =
        Points3D.findContinuousPoints(ptss, tolGap)

    [<Obsolete("Typo, use Points2D.findContinuousPoints instead")>]
    static member findContinuosPoints (ptss: ResizeArray<ResizeArray<Pt>>, tolGap:float) =
        Points2D.findContinuousPoints(ptss, tolGap)

    [<Obsolete("Use Points3D.center instead")>]
    static member center (pts: ResizeArray<Pnt>) =
        Points3D.center(pts)

    [<Obsolete("Use Points3D.normalOfPoints instead")>]
    static member normalOfPoints(pts: ResizeArray<Pnt>) : Vec =
        Points3D.normalOfPoints(pts)

    [<Obsolete("Use Points3D.normalOfPoints instead")>]
    static member normalOfPoints(pts:Pnt []) : Vec =
        Points3D.normalOfPoints(pts)

    // Tria2D


    [<Obsolete("Use Tria2D.offsetPtVar instead", error=true)>]
    static member offsetInCornerEx2D(_thisPt:Pt, _prevToThis:Vc, _thisToNext:Vc, _prevDist:float, _nextDist:float, _referenceOrient:float) : ValueOption<Pt*Vc*Vc> =
        fail "Points.offsetInCornerEx2D with referenceOrient is deprecated, referenceOrient is ignored, use Tria2D.offsetPtVar instead." |> unbox // unbox to make type checker happy


    [<Obsolete("Use Tria2D.offsetPtVar instead", error=true)>]
    static member offsetInCornerEx2D(_prevPt:Pt, _thisPt:Pt, _nextPt:Pt, _prevDist:float,_nextDist:float, _referenceOrient:float) : ValueOption<Pt*Vc*Vc> =
        fail "Points.offsetInCornerEx2D with referenceOrient is deprecated, referenceOrient is ignored, use Tria2D.offsetPtVar instead." |> unbox // unbox to make type checker happy


    // Tria3D


    [<Obsolete("Use Tria3D.offsetVar instead")>]
    static member offsetInCorner(thisPt:Pnt, prevToThis:Vec, thisToNext:Vec, prevDist:float, nextDist:float) : ValueOption<Pnt> =
        Tria3D.offsetVar(thisPt, thisPt+prevToThis, thisPt+thisToNext, prevDist, nextDist)

    [<Obsolete("Use Tria3D.offsetVar instead")>]
    static member offsetInCorner(prevPt:Pnt, thisPt:Pnt, nextPt:Pnt, prevDist:float, nextDist:float) : ValueOption<Pnt> =
        Tria3D.offsetVar(prevPt, thisPt, nextPt, prevDist, nextDist)

    [<Obsolete("Use Tria3D.offsetVar instead", error=true)>]
    static member offsetInCornerEx(_thisPt:Pnt, _prevToThis:Vec, _thisToNext:Vec, _prevDist:float, _nextDist:float, _referenceNormal:Vec) : ValueOption<Pnt*UnitVec*Vec*Vec> =
        fail "Points.offsetInCornerEx with referenceNormal is deprecated, referenceNormal is ignored, use Tria3D.offsetVar instead." |> unbox // unbox to make type checker happy


    [<Obsolete("Use Tria3D.offsetVar instead", error=true)>]
    static member offsetInCornerEx(_prevPt:Pnt, _thisPt:Pnt, _nextPt:Pnt, _prevDist:float, _nextDist:float, _referenceNormal:Vec) : ValueOption<Pnt*UnitVec*Vec*Vec> =
        fail "Points.offsetInCornerEx with referenceNormal is deprecated, referenceNormal is ignored, use Tria3D.offsetVar instead." |> unbox // unbox to make type checker happy



