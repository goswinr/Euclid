namespace Euclid

open System
open System.Threading.Tasks
open Euclid.LineIntersectionTypes
open UtilEuclid

/// Provides operations on 2D and 3D points.
[<AbstractClass; Sealed>]
[<RequireQualifiedAccess>]
type Points private () =
    // static class, use these attributes [<AbstractClass; Sealed>] to match C# static class
    // and make in visible in C# // https://stackoverflow.com/questions/13101995/defining-static-classes-in-f


    /// Returns the double square area of a triangle.
    /// This is the fastest way to get a comparison or sorting value for the area of a triangle.
    /// This is just the square length of the Cross Product vector.
    /// The length of a Cross Product vector is equal to the area of the parallelogram described by the two input vectors.
    static member inline areaTriangleDoubleSq (a:Pnt, b:Pnt, c:Pnt) :float =
        // 2 edges of the triangle as vectors
        let vX = b.X - a.X
        let vY = b.Y - a.Y
        let vZ = b.Z - a.Z
        let wX = c.X - a.X
        let wY = c.Y - a.Y
        let wZ = c.Z - a.Z
        // Cross Product of the two edges
        let x = vY * wZ - vZ * wY
        let y = vZ * wX - vX * wZ
        let z = vX * wY - vY * wX
        // the square length of the Cross Product is the square area of the parallelogram described by the two input vectors.
        x*x + y*y + z*z

    /// Returns the area of a triangle described by 3 points.
    static member inline areaTriangle (a:Pnt, b:Pnt, c:Pnt) :float =
        Points.areaTriangleDoubleSq(a, b, c) |> sqrt |>  ( * ) 0.5


    /// Checks if three points are in one line.
    /// This is a very fast check, but it is hard to find an appropriate tolerance. (Default is 0.001)
    /// This tolerance is the square area of the parallelogram described by two vectors created from the 3 points.
    /// So it also returns true if the points are equal or very close to each other.
    /// Returns false for NaN input values.
    /// See Points.areInLine function too.
    static member inline areInLineFast (a:Pnt, b:Pnt, c:Pnt, [<OPT;DEF(0.001)>] maxSquareAreaParallelogram:float) =
        let doubleSqArea = Points.areaTriangleDoubleSq(a, b, c)
        doubleSqArea < maxSquareAreaParallelogram


    /// Checks if three points are in one line. With a maximum deviation of the given distance tolerance. 1e-6 by default.
    /// Fails if any of the points are closer than the given distance tolerance to each other.
    static member areInLine (a:Pnt, b:Pnt, c:Pnt, [<OPT;DEF(1e-6)>] distanceTolerance:float) =

        let inline  distanceSqLineToPntInfinite(lnFrom:Pnt, lnTo:Pnt, p:Pnt,  lnSqLen:float) =
            // rewritten from  as Line3D.distanceLineToPntInfinite
            // http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
            let x = lnFrom.X - lnTo.X
            let y = lnFrom.Y - lnTo.Y
            let z = lnFrom.Z - lnTo.Z

            let u = lnFrom.X - p.X
            let v = lnFrom.Y - p.Y
            let w = lnFrom.Z - p.Z

            let dot = x*u + y*v + z*w

            let t = dot/lnSqLen
            let x' = lnFrom.X - x * t
            let y' = lnFrom.Y - y * t
            let z' = lnFrom.Z - z * t
            let u' = x' - p.X
            let v' = y' - p.Y
            let w' = z' - p.Z
            u'*u' + v'*v' + w'*w'

        let ab = b-a
        let bc = c-b
        let ca = a-c
        let abLenSq = ab.LengthSq
        let bcLenSq = bc.LengthSq
        let caLenSq = ca.LengthSq
        let distSq = distanceTolerance*distanceTolerance
        if not(distSq < abLenSq) then EuclidException.Raisef "Euclid.Points.areInLine failed on very short line %O to %O " a b
        if not(distSq < bcLenSq) then EuclidException.Raisef "Euclid.Points.areInLine failed on very short line %O to %O " b c
        if not(distSq < caLenSq) then EuclidException.Raisef "Euclid.Points.areInLine failed on very short line %O to %O " c a
        let dotA = ab *** ca
        let dotB = bc *** ab
        let dotC = ca *** bc
        // the corner with the biggest dot product is the one with the biggest angle,
        // so the one with the closest distance to the opposite line in this triangle
        if dotA > dotB && dotA > dotC then
            distanceSqLineToPntInfinite(b, c, a, bcLenSq) < distSq
        elif dotB > dotA && dotB > dotC then
            distanceSqLineToPntInfinite(c, a, b, caLenSq) < distSq
        else
            distanceSqLineToPntInfinite(a, b, c, abLenSq) < distSq



    /// The sign is negative if the loop is clockwise.
    /// Last and first point should be the same.
    static member getSignedArea(ps:ResizeArray<Pt>) = //
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


    /// Returns the closest 3D point index form a 3D point list to a given 3D point.
    static member closestPointIdx (pts:ResizeArray<Pnt>, pt:Pnt) : int =
        if pts.Count = 0 then EuclidException.Raisef "Euclid.Points.closestPoint<Pnt>: empty List of Points: pts"
        let mutable mi = -1
        let mutable mid = Double.MaxValue
        for i=0 to pts.Count - 1 do
            let p = pts.[i]
            let d = Pnt.distanceSq p pt
            if d < mid then
                mid <- d
                mi <- i
        mi

    /// Returns the closest 2D point index form a 2D point list to a given 2D point.
    static member closestPointIdx (pts:ResizeArray<Pt>, pt:Pt) : int =
        if pts.Count = 0 then EuclidException.Raisef "Euclid.Points.closestPoint<Pt>: empty List of Points: pts"
        let mutable mi = -1
        let mutable mid = Double.MaxValue
        for i=0 to pts.Count - 1 do
            let p = pts.[i]
            let d = Pt.distanceSq p pt
            if d < mid then
                mid <- d
                mi <- i
        mi

    /// Returns the closest point form a 3D point list to a given 3D point.
    static member closestPoint (pts:ResizeArray<Pnt>, pt:Pnt) : Pnt=
        pts.[Points.closestPointIdx (pts, pt)]

    /// Returns the closest 2D point form a point list to a given 2D point.
    static member closestPoint (pts:ResizeArray<Pt>, pt:Pt) : Pt=
        pts.[Points.closestPointIdx (pts, pt)]

    /// Returns the indices of the 3D points that are closest to each other.
    static member closestPointsIdx (xs:ResizeArray<Pnt>, ys:ResizeArray<Pnt>) =
        if xs.Count = 0 then EuclidException.Raisef "Euclid.Points.closestPointsIdx<Pnt>: empty List of Points: xs"
        if ys.Count = 0 then EuclidException.Raisef "Euclid.Points.closestPointsIdx<Pnt>: empty List of Points: ys"
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

    /// Returns the indices of the 2D points that are closest to each other.
    static member closestPointsIdx (xs:ResizeArray<Pt>, ys:ResizeArray<Pt>) =
        //TODO
        // (1)
        // the current quadratic runtime could be optimized by first sorting the points in x (or y) direction
        // then the search loop could star when x distance is smaller than minD, and exist when it is bigger
        // (2)
        // alternatively a spatial hash could be used to cluster nearby objects. the challenge would be to finde the right cell size for each point
        // (3)
        // the bounding Rectangles of each set could be intersected. then expanded. then used to filter both lists.
        if xs.Count = 0 then EuclidException.Raisef "Euclid.Points.closestPointsIdx<Pt>: empty List of Points: xs"
        if ys.Count = 0 then EuclidException.Raisef "Euclid.Points.closestPointsIdx<Pt>: empty List of Points: ys"
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


    /// Given two lists of 3D points finds the pair that are closest to each other and returns their distance.
    static member minDistBetweenPointSets (xs:ResizeArray<Pnt>, ys:ResizeArray<Pnt>) =
        if xs.Count = 0 then EuclidException.Raisef "Euclid.Points.minDistBetweenPointSets<Pnt>: empty List of Points: xs"
        if ys.Count = 0 then EuclidException.Raisef "Euclid.Points.minDistBetweenPointSets<Pnt>: empty List of Points: ys"
        let (i, j) = Points.closestPointsIdx (xs, ys)
        Pnt.distance xs.[i]  ys.[j]

    /// Given two lists of 2D points finds the pair that are closest to each other and returns their distance.
    static member minDistBetweenPointSets (xs:ResizeArray<Pt>, ys:ResizeArray<Pt>) =
        if xs.Count = 0 then EuclidException.Raisef "Euclid.Points.minDistBetweenPointSets<Pt>: empty List of Points: xs"
        if ys.Count = 0 then EuclidException.Raisef "Euclid.Points.minDistBetweenPointSets<Pt>: empty List of Points: ys"
        let (i, j) = Points.closestPointsIdx (xs, ys)
        Pt.distance xs.[i]  ys.[j]


    /// Find the index of the 3D point that has the biggest distance to any 3D point from the other set.
    /// Basically the most lonely point in 'findPointFrom' list with respect to 'checkAgainst' list.
    /// Returns findPointFromIdx * checkAgainstIdx
    static member mostDistantPointIdx (findPointFrom:ResizeArray<Pnt>, checkAgainst:ResizeArray<Pnt>) : int*int=
        if findPointFrom.Count = 0 then EuclidException.Raisef "Euclid.Points.mostDistantPoint<Pnt>: empty List of Points: findPointFrom"
        if checkAgainst.Count = 0 then EuclidException.Raisef "Euclid.Points.mostDistantPoint<Pnt>: empty List of Points: checkAgainst"
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

    /// Find the index of the 2D point that has the biggest distance to any 2D point from the other set.
    /// Basically the most lonely point in 'findPointFrom' list with respect to 'checkAgainst' list.
    /// Returns findPointFromIdx * checkAgainstIdx
    static member mostDistantPointIdx (findPointFrom:ResizeArray<Pt>, checkAgainst:ResizeArray<Pt>) : int*int=
        if findPointFrom.Count = 0 then EuclidException.Raisef "Euclid.Points.mostDistantPoint<Pt>: empty List of Points: findPointFrom"
        if checkAgainst.Count = 0 then EuclidException.Raisef "Euclid.Points.mostDistantPoint<Pt>: empty List of Points: checkAgainst"
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



    /// Find the 3D point that has the biggest distance to any 3D point from another set.
    static member mostDistantPoint (findPointFrom:ResizeArray<Pnt>, checkAgainst:ResizeArray<Pnt>) =
        let i, _ = Points.mostDistantPointIdx (findPointFrom, checkAgainst)
        findPointFrom.[i]

    /// Find the 2D point that has the biggest distance to any 2D point from another set.
    static member mostDistantPoint (findPointFrom:ResizeArray<Pt>, checkAgainst:ResizeArray<Pt>) =
        let i, _ = Points.mostDistantPointIdx (findPointFrom, checkAgainst)
        findPointFrom.[i]


    /// Culls 3D points if they are to close to previous or next item.
    /// Last and first 3D points stay the same.
    static member cullDuplicatePointsInSeq (pts:ResizeArray<Pnt>, tolerance) =
        if pts.Count = 0 then EuclidException.Raisef "Euclid.Points.cullDuplicatePointsInSeq<Pnt>: empty List of Points"
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

    /// Culls 2D points if they are to close to previous or next item.
    /// Last and first 2D points stay the same.
    static member cullDuplicatePointsInSeq (pts:ResizeArray<Pt>, tolerance) =
        if pts.Count = 0 then EuclidException.Raisef "Euclid.Points.cullDuplicatePointsInSeq<Pt>: empty List of Points"
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




    /// Similar to join polylines this tries to find continuos sequences of 2D points.
    /// 'tolGap' is the maximum allowable gap between the start and the endpoint of to segments.
    /// Search starts from the segment with the most points.
    /// Both start and end point of each 2D point list is checked for adjacency.
    static member findContinuosPoints (ptss: ResizeArray<ResizeArray<Pt>>, tolGap:float) =
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


    /// Similar to Join Polylines this tries to find continuos sequences of 3D points.
    /// 'tolGap' is the maximum allowable gap between the start and the endpoint of to segments.
    /// Search starts from the segment with the most points.
    /// Both start and end point of each 3D point list is checked for adjacency.
    static member findContinuosPoints (ptss: ResizeArray<ResizeArray<Pnt>>, tolGap:float) =
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

    /// Finds the center, mean or average point.
    static member center (pts: ResizeArray<Pnt>) =
        let mutable sum = Pnt.Origin
        for i = 0 to pts.Count-1 do
            let pt = pts.[i]
            sum <- sum + pt
        sum / float pts.Count

    /// Finds the mean normal of many points.
    /// It finds the center point and then takes cross-products iterating all points in pairs of two.
    /// The first three points define the orientation of the normal.
    /// So it considers the current order of points too.
    /// If the order is counterclockwise in the World X-Y plane then the normal is in world Z orientation.
    static member normalOfPoints(pts: ResizeArray<Pnt>) : Vec =
        if pts.Count <= 2 then
            EuclidException.Raisef "Euclid.Points.normalOfPoints can't find normal of two or less points %O" pts
        elif pts.Count = 3  then
            let a = pts.[0] - pts.[1]
            let b = pts.[2] - pts.[1]
            let v= Vec.cross(b, a)
            if isTooSmallSq v.LengthSq  then
                EuclidException.Raisef "Euclid.Points.normalOfPoints: three points are in a line %O" pts
            else
                v
        else
            let cen = pts |> Points.center
            let mutable v = Vec.Zero
            let mutable t = pts.[0]
            for i=1 to pts.Count-1 do
                let n = pts.[i]
                let a = t-cen
                let b = n-cen
                let x = Vec.cross(a, b)  |> Vec.matchOrientation v // TODO do this matching?
                v <- v + x
                t<-n
            if isTooSmallSq v.LengthSq  then
                EuclidException.Raisef "Euclid.Points.normalOfPoints: points are in a line or sphere without clear normal %O" pts
            else
                v

    /// Finds the mean normal of many points.
    /// It finds the center point and then takes cross-products iterating all points in pairs of two.
    /// The first three points define the orientation of the normal.
    /// So it considers the current order of points too.
    /// If the order is counterclockwise in the World X-Y plane then the normal is in world Z orientation.
    static member normalOfPoints(pts:Pnt []) : Vec =
        if pts.Length <= 2 then
            EuclidException.Raisef "Euclid.Points.normalOfPoints can't find normal of two or less points %O" pts
        elif pts.Length = 3  then
            let a = pts.[0] - pts.[1]
            let b = pts.[2] - pts.[1]
            let v= Vec.cross(b, a)
            if isTooSmallSq v.LengthSq  then
                EuclidException.Raisef "Euclid.Points.normalOfPoints: three points are in a line %O" pts
            else
                v
        else
            let mutable cen = Pnt.Origin
            for i=0 to pts.Length-1 do  cen <- cen + pts.[i]
            cen <- cen / float pts.Length
            let mutable v = Vec.Zero
            let mutable t = pts.[0]
            for i=1 to pts.Length-1 do
                let n = pts.[i]
                let x = Vec.cross(t-cen, n-cen)  |> Vec.matchOrientation v // TODO do this matching?
                v <- v + x
                t <- n
            if isTooSmallSq v.LengthSq  then
                EuclidException.Raisef "Euclid.Points.normalOfPoints: points are in a line or sphere without clear normal %O" pts
            else
                v




    /// It finds the inner offset point in a corner (defined a point, a previous vector to this point and a next vector from this point)
    /// The offset from first and second segment are given separately and can vary (prevDist and nextDist).
    /// Use negative distance for outer offset.
    /// If Points are collinear by 0.25 degrees or less than 1-e6 units apart returns: ValueNone.
    /// Use negative distances to get outside offset.
    static member offsetInCorner(thisPt:Pnt,
                                 prevToThis:Vec,
                                 thisToNext:Vec,
                                 prevDist:float,
                                 nextDist:float) : ValueOption<Pnt> =
        let ax = prevToThis.X
        let ay = prevToThis.Y
        let az = prevToThis.Z
        let bx = thisToNext.X
        let by = thisToNext.Y
        let bz = thisToNext.Z
        let a = ax*ax + ay*ay + az*az // square length of A
        let c = bx*bx + by*by + bz*bz // square length of B
        if isTooSmallSq c then
            ValueNone
        elif isTooSmallSq c  then
            ValueNone
        else
            let b = ax*bx + ay*by + az*bz // dot product of both lines
            let ac = a*c // square of square length, never negative
            let bb = b*b // square of square dot product, never negative
            let discriminant = ac - bb // never negative, the dot product cannot be bigger than the two square length multiplied with each other
            let div = ac+bb // never negative
            let rel = discriminant/div
            if rel < float RelAngleDiscriminant.``0.25`` then //parallel
                ValueNone
            else
                let n = Vec.cross(prevToThis, thisToNext)
                let offP = thisPt + (Vec.cross(n, prevToThis)  |> Vec.withLength prevDist)
                let offN = thisPt + (Vec.cross(n, thisToNext)  |> Vec.withLength nextDist)
                let vx = offN.X - offP.X
                let vy = offN.Y - offP.Y
                let vz = offN.Z - offP.Z
                let e = bx*vx + by*vy + bz*vz
                let d = ax*vx + ay*vy + az*vz
                let t = (c * d - b * e) / discriminant
                ValueSome <|  offP + t * prevToThis

    /// It finds the inner offset point in a corner (defined by a Polyline from 3 points (prevPt, thisPt and nextPt)
    /// The offset from first and second segment are given separately and can vary (prevDist and nextDist).
    /// Use negative distance for outer offset.
    /// If Points are collinear by 0.25 degrees or less than 1-e6 units apart returns: ValueNone.
    /// Use negative distances to get outside offset.
    static member offsetInCorner( prevPt:Pnt,
                                  thisPt:Pnt,
                                  nextPt:Pnt,
                                  prevDist:float,
                                  nextDist:float) : ValueOption<Pnt> =
        let prevV = thisPt - prevPt
        let nextV = nextPt - thisPt
        Points.offsetInCorner(thisPt, prevV, nextV, prevDist, nextDist)


    /// It finds the inner offset point in a corner (defined a point, a previous vector to this point and a next vector from this point)
    /// The offset from first and second segment are given separately and can vary (prevDist and nextDist).
    /// Use negative distance for outer offset.
    /// If Points are collinear by 0.25 degrees or less than 1-e6 units apart returns: ValueNone.
    /// Use negative distances to get outside offset.
    /// The 'referenceNormal' is' An approximate orientation Normal to help find the correct offset side, To be in Z Axis orientation for Counter-Clockwise loops in 2D.
    /// Returns the offset point, the unitized normal vector aligned with the referenceNormal, the shift direction for prev and next line.
    static member offsetInCornerEx( thisPt:Pnt,
                                    prevToThis:Vec,
                                    thisToNext:Vec,
                                    prevDist:float,
                                    nextDist:float,
                                    referenceNormal:Vec) : ValueOption<Pnt*UnitVec*Vec*Vec> =
        let ax = prevToThis.X
        let ay = prevToThis.Y
        let az = prevToThis.Z
        let bx = thisToNext.X
        let by = thisToNext.Y
        let bz = thisToNext.Z
        let a = ax*ax + ay*ay + az*az // square length of A
        let c = bx*bx + by*by + bz*bz // square length of B
        if isTooSmallSq (c) then
            ValueNone
        elif isTooSmallSq (a) then
            ValueNone
        else
            let b = ax*bx + ay*by + az*bz // dot product of both lines
            let ac = a*c // square of square length, never negative
            let bb = b*b // square of square dot product, never negative
            let discriminant = ac - bb // never negative, the dot product cannot be bigger than the two square length multiplied with each other
            let div = ac+bb // never negative
            let rel = discriminant/div
            if rel < float RelAngleDiscriminant.``0.25`` then //parallel
                ValueNone
            else
                let n = Vec.cross(prevToThis, thisToNext) |> Vec.matchOrientation referenceNormal
                let prevShift = Vec.cross(n, prevToThis)  |> Vec.withLength prevDist
                let nextShift = Vec.cross(n, thisToNext)  |> Vec.withLength nextDist
                let offP = thisPt + prevShift
                let offN = thisPt + nextShift
                let vx = offN.X - offP.X
                let vy = offN.Y - offP.Y
                let vz = offN.Z - offP.Z
                let e = bx*vx + by*vy + bz*vz
                let d = ax*vx + ay*vy + az*vz
                let t = (c * d - b * e) / discriminant
                let pt = offP + t * prevToThis
                ValueSome (pt, n.Unitized, prevShift, nextShift)

    /// It finds the inner offset point in a corner (defined by a Polyline from 3 points (prevPt, thisPt and nextPt)
    /// The offset from first and second segment are given separately and can vary (prevDist and nextDist).
    /// Use negative distance for outer offset.
    /// If Points are collinear by 0.25 degrees or less than 1-e6 units apart returns: ValueNone.
    /// Use negative distances to get outside offset.
    /// The 'referenceNormal' is' An approximate orientation Normal to help find the correct offset side, To be in Z Axis orientation for Counter-Clockwise loops in 2D.
    /// Returns the offset point, the unitized normal vector aligned with the referenceNormal, the shift direction for prev and next line.
    static member offsetInCornerEx( prevPt:Pnt,
                                    thisPt:Pnt,
                                    nextPt:Pnt,
                                    prevDist:float,
                                    nextDist:float,
                                    referenceNormal:Vec) : ValueOption<Pnt*UnitVec*Vec*Vec> =
        let prevV = thisPt - prevPt
        let nextV = nextPt - thisPt
        Points.offsetInCornerEx(thisPt, prevV, nextV, prevDist, nextDist, referenceNormal)

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
        Points.offsetInCornerEx2D(thisPt, prevV, nextV, prevDist, nextDist, referenceOrient)


    /// Returns the closer point of the two points to the reference given point.
    static member closestOfTwo (pt1:Pnt) (pt2:Pnt) (referencePoint:Pnt) =
        let d1 = Pnt.distanceSq pt1 referencePoint
        let d2 = Pnt.distanceSq pt2 referencePoint
        if d1 < d2 then
            pt1
        else
            pt2