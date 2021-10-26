namespace FsEx.Geo

open System


/// provides operations on 2D and 3D points
[<AbstractClass; Sealed>]
[<RequireQualifiedAccess>]
type Points private () = 

    // static class, use these attributes [<AbstractClass; Sealed>] to match C# static class
    // and make in visible in C# // https://stackoverflow.com/questions/13101995/defining-static-classes-in-f
    
    /// Checks if three points are in one line.
    /// Its does so by checking if the cross product of the vectors between the points is below the tolerance.    
    static member inline areInLine tol (a:Pnt) (b:Pnt) (c:Pnt) = 
         // TODO can be optimized by inlining floats.
        Vec.cross (b-a,c-a) |> Vec.isTiny tol
    
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


    /// Returns the closest 3D point index form a 3D point list  to a given 3D point
    static member closestPointIdx (pts:ResizeArray<Pnt>,pt:Pnt) : int = 
        if pts.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.closestPoint<Pnt>: empty List of Points: pts"
        let mutable mi = -1
        let mutable mid = Double.MaxValue
        for i=0 to pts.Count - 1 do
            let p = pts.[i]
            let d = Pnt.distanceSq p pt
            if d < mid then
                mid <- d
                mi <- i
        mi

    /// Returns the closest 2D point index form a 2D point list  to a given 2D point
    static member closestPointIdx (pts:ResizeArray<Pt>,pt:Pt) : int = 
        if pts.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.closestPoint<Pt>: empty List of Points: pts"
        let mutable mi = -1
        let mutable mid = Double.MaxValue
        for i=0 to pts.Count - 1 do
            let p = pts.[i]
            let d = Pt.distanceSq p pt
            if d < mid then
                mid <- d
                mi <- i
        mi
    
    /// Returns the closest point form a 3D point list to a given 3D point
    static member closestPoint (pts:ResizeArray<Pnt>, pt:Pnt) : Pnt= 
        pts.[Points.closestPointIdx (pts, pt)]

    /// Returns the closest 2D point form a point list to a given 2D point
    static member closestPoint (pts:ResizeArray<Pt>, pt:Pt) : Pt= 
        pts.[Points.closestPointIdx (pts, pt)]

    /// Returns the indices of the 3D points that are closest to each other
    static member closestPointsIdx (xs:ResizeArray<Pnt>, ys:ResizeArray<Pnt>) = 
        if xs.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.closestPointsIdx<Pnt>: empty List of Points: xs"
        if ys.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.closestPointsIdx<Pnt>: empty List of Points: ys"
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
        xi,yj   

    /// Returns the indices of the 2D points that are closest to each other
    static member closestPointsIdx (xs:ResizeArray<Pt>, ys:ResizeArray<Pt>) = 
        //TODO
        // (1)
        // the current quadratic runtime could be optimized by first sorting the points in x (or y) direction
        // then the search loop could star when x distance is smaller than minD, and exist when it is bigger
        // (2)
        // alternatively a spatial hash could be used to cluster nearby objects. the challenge would be to finde the right cell size for each point
        // (3)
        // the bounding Rectangles of each set could be intersected. then expanded. then used to filter both lists.
        if xs.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.closestPointsIdx<Pt>: empty List of Points: xs"
        if ys.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.closestPointsIdx<Pt>: empty List of Points: ys"
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
        xi,yj    


    /// Given two lists of 3D points finds the pair that are closest to each other and returns their distance.
    static member  minDistBetweenPointSets (xs:ResizeArray<Pnt>, ys:ResizeArray<Pnt>) = 
        if xs.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.minDistBetweenPointSets<Pnt>: empty List of Points: xs"
        if ys.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.minDistBetweenPointSets<Pnt>: empty List of Points: ys"
        let (i,j) = Points.closestPointsIdx (xs, ys)
        Pnt.distance xs.[i]  ys.[j]

    /// Given two lists of 2D points finds the pair that are closest to each other and returns their distance.
    static member  minDistBetweenPointSets (xs:ResizeArray<Pt>, ys:ResizeArray<Pt>) = 
        if xs.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.minDistBetweenPointSets<Pt>: empty List of Points: xs"
        if ys.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.minDistBetweenPointSets<Pt>: empty List of Points: ys"
        let (i,j) = Points.closestPointsIdx (xs, ys)
        Pt.distance xs.[i]  ys.[j]
    

    /// Find the index of the 3D point that has the biggest distance to any 3D point from the other set
    /// basically the most lonely point in 'findPointFrom' list with respect to 'checkAgainst' list
    /// Returns findPointFromIdx * checkAgainstIdx
    static member  mostDistantPointIdx (findPointFrom:ResizeArray<Pnt>, checkAgainst:ResizeArray<Pnt>) : int*int= 
        if findPointFrom.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.mostDistantPoint<Pnt>: empty List of Points: findPointFrom"
        if checkAgainst.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.mostDistantPoint<Pnt>: empty List of Points: checkAgainst"
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

    /// Find the index of the 2D point that has the biggest distance to any 2D point from the other set
    /// basically the most lonely point in 'findPointFrom' list with respect to 'checkAgainst' list
    /// Returns findPointFromIdx * checkAgainstIdx
    static member mostDistantPointIdx (findPointFrom:ResizeArray<Pt>, checkAgainst:ResizeArray<Pt>) : int*int= 
        if findPointFrom.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.mostDistantPoint<Pt>: empty List of Points: findPointFrom"
        if checkAgainst.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.mostDistantPoint<Pt>: empty List of Points: checkAgainst"
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
    


    /// Find the 3D point that has the biggest distance to any 3D point from another set
    static member  mostDistantPoint (findPointFrom:ResizeArray<Pnt>, checkAgainst:ResizeArray<Pnt>) = 
        let i,_ = Points.mostDistantPointIdx (findPointFrom, checkAgainst)
        findPointFrom.[i]       
        
    /// Find the 2D point that has the biggest distance to any 2D point from another set
    static member mostDistantPoint (findPointFrom:ResizeArray<Pt>, checkAgainst:ResizeArray<Pt>) = 
        let i,_ = Points.mostDistantPointIdx (findPointFrom, checkAgainst)
        findPointFrom.[i]
    
    
    /// Culls 3D points if they are to close to previous or next item
    /// Last and first 3D points stay the same
    static member cullDuplicatePointsInSeq (pts:ResizeArray<Pnt>, tolerance)  = 
        if pts.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.cullDuplicatePointsInSeq<Pnt>: empty List of Points"
        if pts.Count = 1 then
            pts
        else
            let tolSq = tolerance*tolerance
            let res  =  ResizeArray(pts.Count)
            let mutable last  = pts.[0]
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

    /// Culls 2D points if they are to close to previous or next item
    /// Last and first 2D points stay the same
    static member cullDuplicatePointsInSeq (pts:ResizeArray<Pt>, tolerance)  = 
        if pts.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.cullDuplicatePointsInSeq<Pt>: empty List of Points"
        if pts.Count = 1 then
            pts
        else
            let tolSq = tolerance*tolerance
            let res  =  ResizeArray(pts.Count)
            let mutable last  = pts.[0]
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
    /// Both start and end point of each 2D point list is checked for adjacency
    static member findContinuosPoints (ptss: ResizeArray<ResizeArray<Pt>>, tolGap:float)  = 
        let i =  ptss |> ResizeArray.maxIndexBy ResizeArray.length
        let res = ptss.Pop(i)
        let mutable loop = true
        while loop && ptss.Count > 0 do
            //first try to append to end
            let ende = res.[res.Count-1]
            let si = ptss |> ResizeArray.minIndexBy ( fun ps -> Pt.distanceSq ende ps.First )
            let ei = ptss |> ResizeArray.minIndexBy ( fun ps -> Pt.distanceSq ende ps.Last)
            let sd = Pt.distance ende ptss.[si].First
            let ed = Pt.distance ende ptss.[ei].Last
            if   sd < tolGap && sd < ed then  res.AddRange(                ptss.Pop(si))
            elif ed < tolGap && ed < sd then  res.AddRange(ResizeArray.rev(ptss.Pop(ei)) )
            else
                //search from start
                let start = res.[0]
                let si = ptss |> ResizeArray.minIndexBy ( fun ps -> Pt.distanceSq start ps.First )
                let ei = ptss |> ResizeArray.minIndexBy ( fun ps -> Pt.distanceSq start ps.Last)
                let sd = Pt.distance start ptss.[si].First
                let ed = Pt.distance start ptss.[ei].Last
                if   sd < tolGap && sd < ed then res.InsertRange(0, ResizeArray.rev(ptss.Pop(si)) )
                elif ed < tolGap && ed < sd then res.InsertRange(0,                 ptss.Pop(ei))
                else
                    loop <- false
        res

    /// Similar to Join Polylines this tries to find continuos sequences of 3D points.
    /// 'tolGap' is the maximum allowable gap between the start and the endpoint of to segments.
    /// Search starts from the segment with the most points.
    /// Both start and end point of each 3D point list is checked for adjacency
    static member findContinuosPoints (ptss: ResizeArray<ResizeArray<Pnt>>, tolGap:float)  = 
        let i =  ptss |> ResizeArray.maxIndexBy ResizeArray.length
        let res = ptss.Pop(i)
        let mutable loop = true
        while loop && ptss.Count > 0 do
            //first try to append to end
            let ende = res.[res.Count-1]
            let si = ptss |> ResizeArray.minIndexBy ( fun ps -> Pnt.distanceSq ende ps.First )
            let ei = ptss |> ResizeArray.minIndexBy ( fun ps -> Pnt.distanceSq ende ps.Last)
            let sd = Pnt.distance ende ptss.[si].First
            let ed = Pnt.distance ende ptss.[ei].Last
            if   sd < tolGap && sd < ed then  res.AddRange(                ptss.Pop(si))
            elif ed < tolGap && ed < sd then  res.AddRange(ResizeArray.rev(ptss.Pop(ei)) )
            else
                //search from start
                let start = res.[0]
                let si = ptss |> ResizeArray.minIndexBy ( fun ps -> Pnt.distanceSq start ps.First )
                let ei = ptss |> ResizeArray.minIndexBy ( fun ps -> Pnt.distanceSq start ps.Last)
                let sd = Pnt.distance start ptss.[si].First
                let ed = Pnt.distance start ptss.[ei].Last
                if   sd < tolGap && sd < ed then res.InsertRange(0, ResizeArray.rev(ptss.Pop(si)) )
                elif ed < tolGap && ed < sd then res.InsertRange(0,                 ptss.Pop(ei))
                else
                    loop <- false
        res
    
    /// Finds the center, mean or average point.
    static member center (pts: ResizeArray<Pnt>) = 
        let mutable sum = Pnt.Origin
        for pt in pts do  sum <- sum + pt
        sum / float pts.Count   

    /// Finds the mean normal of many points.
    /// It finds the center point and then takes cross-products iterating all points in pairs of two.
    /// The first two points define the orientation of the normal.
    /// So it considers the current order of points too, if counterclockwise in xy Plane the normal in z orientation.
    static member normalOfPoints(pts: ResizeArray<Pnt>) : UnitVec  = 
        if pts.Count <= 2 then
            FsExGeoException.Raise "FsEx.Geo.Points.NormalOfPoints can't find normal of two or less points %O" pts
        elif pts.Count = 3  then
            let a = pts.[0] - pts.[1]
            let b = pts.[2] - pts.[1]
            let v= Vec.cross(b, a)
            if v.IsTiny(1e-6) then FsExGeoException.Raise "FsEx.Geo.Points.NormalOfPoints: three points are in a line  %O" pts
            else
                v.Unitized
        else
            let cen = pts |> Points.center
            let mutable v = Vec.Zero
            for t, n in ResizeArray.thisNext pts do
                let a = t-cen
                let b = n-cen
                let x = Vec.cross(a, b)  |> Vec.matchOrientation v // TODO do this matching?
                v <- v + x
            if v.IsTiny(1e-3) then FsExGeoException.Raise "FsEx.Geo.Points.NormalOfPoints: points are in a line or sphere without clear normal  %O" pts
            else
                v.Unitized
    
    //
    /// Finds the inner offset point in a corner ( defined by a Polyline from 3 points ( prevPt, thisPt and nextPt)
    /// The offset from first and second segment are given separately and can vary (prevDist and nextDist).
    /// Use negative distance for outer offset
    /// The orientation parameter is an approximate orientation vector. It might flip the offset side if the dot product with the local normal is negative.
    /// Returns a Value tuple of :
    ///   - the first segment offset vector in actual length  ,
    ///   - second segment offset vector,
    ///   - the offset corner,
    ///   - and the unitized normal at the corner. Flipped if needed to match orientation of the orientation input vector (positive dot product)
    /// If Points are  collinear returns: Vec.Zero, Vec.Zero, Pnt.Origin, Vec.Zero
    /// To check if the return values are this sentinel or a real result look at the normal vector.
    static member findOffsetCorner( prevPt:Pnt,
                                    thisPt:Pnt,
                                    nextPt:Pnt,
                                    prevDist:float,
                                    nextDist:float,
                                    orientation:Vec) : struct(Vec* Vec * Pnt * Vec) = 
        let vp = prevPt - thisPt
        let vn = nextPt - thisPt
        if Vec.isAngleBelowQuatreDegree(vp, vn) then // TODO refine error criteria
            struct(Vec.Zero, Vec.Zero, Pnt.Origin, Vec.Zero)
        else
            let n  = Vec.cross(vp, vn) |> Vec.matchOrientation orientation
            let sp = Vec.cross(vp, n) |> Vec.setLength prevDist// the offset vectors
            let sn = Vec.cross(n, vn) |> Vec.setLength nextDist// the offset vectors
            let lp = Line3D(thisPt + sp , thisPt + sp + vp)  
            let ln = Line3D(thisPt + sn , thisPt + sn + vn)  
            let tp , tn = Line3D.intersectLineParametersInfinite lp ln //could also be solved with trigonometry functions ??            
            struct(sp, sn, lp.EvaluateAt(tp), n.Unitized*1.0 )  // return the unit vector as Vec ( because it might be Vec.Zero too)
