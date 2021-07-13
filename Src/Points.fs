namespace FsEx.Geo

open System


/// provides operations on 2D and 3D points
[<AbstractClass; Sealed>]
type Points private () = 

    // static class, use these attributes [<AbstractClass; Sealed>] to match C# static class
    // and make in visible in C# // https://stackoverflow.com/questions/13101995/defining-static-classes-in-f
    
    
    /// The sign is negative if the loop is clockwise.
    /// Last and first point should bethe same.
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

    /// Returns the closest 2D point form a Point list to a given 2D point
    static member closestPoint (pts:ResizeArray<Pt>, pt:Pt) : Pt= 
        pts.[Points.closestPointIdx (pts, pt)]

    /// Returns the indices of the 3D points that are closest to each other
    static member closestPointsIdx (xs:ResizeArray<Pnt>, ys:ResizeArray<Pnt>) = 
        if xs.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.closestPointsIdx<Pnt>: empty List of Points: xs"
        if ys.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.closestPointsIdx<Pnt>: empty List of Points: ys"
        let mutable xi = -1
        let mutable yj = -1
        let mutable mind = Double.MaxValue
        for i=0 to xs.Count-1 do
            let pt = xs.[i]
            for j=0 to ys.Count-1 do
                let d = Pnt.distanceSq pt ys.[j]
                if d < mind then
                    mind <- d
                    xi <- i
                    yj <- j
        xi,yj   

    /// Returns the indices of the 2D points that are closest to each other
    static member closestPointsIdx (xs:ResizeArray<Pt>, ys:ResizeArray<Pt>) = 
        //TODO
        // (1)
        // the current quadratic runtime could be optimised by first sorting the points in x (or y) direction
        // then the search loop could star when x distance is smaller than mind, and exist when it is bigger
        // (2)
        // alternatively a spatial hash could be used to cluster nearby objects. the challange would be to finde the right cell size for each point
        // (3)
        // the bounding Rectangles of each set could be intersected. then expanded. then used to filter both lists.
        if xs.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.closestPointsIdx<Pt>: empty List of Points: xs"
        if ys.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.closestPointsIdx<Pt>: empty List of Points: ys"
        let mutable xi = -1
        let mutable yj = -1
        let mutable mind = Double.MaxValue
        for i=0 to xs.Count-1 do
            let pt = xs.[i]
            for j=0 to ys.Count-1 do
                let d = Pt.distanceSq pt ys.[j]
                if d < mind then
                    mind <- d
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
    /// basicaly the most lonely point in 'findPointFrom' list with respect to 'checkAgainst' list
    /// Returns findPointFromIdx * checkAgainstIdx
    static member  mostDistantPointIdx (findPointFrom:ResizeArray<Pnt>, checkAgainst:ResizeArray<Pnt>) : int*int= 
        if findPointFrom.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.mostDistantPoint<Pnt>: empty List of Points: findPointFrom"
        if checkAgainst.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.mostDistantPoint<Pnt>: empty List of Points: checkAgainst"
        let mutable maxd = Double.MinValue
        let mutable findPointFromIdx = -1
        let mutable checkAgainstTempIdx = -1
        let mutable checkAgainstIdx = -1
        for i=0 to findPointFrom.Count-1 do
            let pt = findPointFrom.[i]
            let mutable mind = Double.MaxValue
            for j=0 to checkAgainst.Count-1 do
                let d = Pnt.distanceSq pt checkAgainst.[j]
                if d < mind then
                    mind <- d
                    checkAgainstTempIdx <-j
            if mind > maxd then
                maxd <- mind
                findPointFromIdx <-i
                checkAgainstIdx <-checkAgainstTempIdx
        findPointFromIdx, checkAgainstIdx

    /// Find the index of the 2D point that has the biggest distance to any 2D point from the other set
    /// basicaly the most lonely point in 'findPointFrom' list with respect to 'checkAgainst' list
    /// Returns findPointFromIdx * checkAgainstIdx
    static member mostDistantPointIdx (findPointFrom:ResizeArray<Pt>, checkAgainst:ResizeArray<Pt>) : int*int= 
        if findPointFrom.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.mostDistantPoint<Pt>: empty List of Points: findPointFrom"
        if checkAgainst.Count = 0 then FsExGeoException.Raise "FsEx.Geo.Points.mostDistantPoint<Pt>: empty List of Points: checkAgainst"
        let mutable maxd = Double.MinValue
        let mutable findPointFromIdx = -1
        let mutable checkAgainstTempIdx = -1
        let mutable checkAgainstIdx = -1
        for i=0 to findPointFrom.Count-1 do
            let pt = findPointFrom.[i]
            let mutable mind = Double.MaxValue
            for j=0 to checkAgainst.Count-1 do
                let d = Pt.distanceSq pt checkAgainst.[j]
                if d < mind then
                    mind <- d
                    checkAgainstTempIdx <-j
            if mind > maxd then
                maxd <- mind
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
   
    
    
    /// Similar to Join Polylines this tries to find continous sequences of 2D points.
    /// 'tolGap' is the maximun allowable gap between the start and the endpoint of to segments.
    /// Search starts from the segment with the most points.
    /// Both start and end point of each 2D point list is checked for adjacency
    static member findContinousPoints (ptss: ResizeArray<ResizeArray<Pt>>, tolGap:float)  = 
        let i =  ptss |> ResizeArray.maxIndBy ResizeArray.length
        let res = ptss.Pop(i)
        let mutable loop = true
        while loop && ptss.Count > 0 do
            //first try to append to end
            let ende = res.[res.Count-1]
            let si = ptss |> ResizeArray.minIndBy ( fun ps -> Pt.distanceSq ende ps.First )
            let ei = ptss |> ResizeArray.minIndBy ( fun ps -> Pt.distanceSq ende ps.Last)
            let sd = Pt.distance ende ptss.[si].First
            let ed = Pt.distance ende ptss.[ei].Last
            if   sd < tolGap && sd < ed then  res.AddRange(                ptss.Pop(si))
            elif ed < tolGap && ed < sd then  res.AddRange(ResizeArray.rev(ptss.Pop(ei)) )
            else
                //search from start
                let start = res.[0]
                let si = ptss |> ResizeArray.minIndBy ( fun ps -> Pt.distanceSq start ps.First )
                let ei = ptss |> ResizeArray.minIndBy ( fun ps -> Pt.distanceSq start ps.Last)
                let sd = Pt.distance start ptss.[si].First
                let ed = Pt.distance start ptss.[ei].Last
                if   sd < tolGap && sd < ed then res.InsertRange(0, ResizeArray.rev(ptss.Pop(si)) )
                elif ed < tolGap && ed < sd then res.InsertRange(0,                 ptss.Pop(ei))
                else
                    loop <- false
        res
       
    /// Similar to Join Polylines this tries to find continous sequences of 3D points.
    /// 'tolGap' is the maximun allowable gap between the start and the endpoint of to segments.
    /// Search starts from the segment with the most points.
    /// Both start and end point of each 3D point list is checked for adjacency
    static member findContinousPoints (ptss: ResizeArray<ResizeArray<Pnt>>, tolGap:float)  = 
        let i =  ptss |> ResizeArray.maxIndBy ResizeArray.length
        let res = ptss.Pop(i)
        let mutable loop = true
        while loop && ptss.Count > 0 do
            //first try to append to end
            let ende = res.[res.Count-1]
            let si = ptss |> ResizeArray.minIndBy ( fun ps -> Pnt.distanceSq ende ps.First )
            let ei = ptss |> ResizeArray.minIndBy ( fun ps -> Pnt.distanceSq ende ps.Last)
            let sd = Pnt.distance ende ptss.[si].First
            let ed = Pnt.distance ende ptss.[ei].Last
            if   sd < tolGap && sd < ed then  res.AddRange(                ptss.Pop(si))
            elif ed < tolGap && ed < sd then  res.AddRange(ResizeArray.rev(ptss.Pop(ei)) )
            else
                //search from start
                let start = res.[0]
                let si = ptss |> ResizeArray.minIndBy ( fun ps -> Pnt.distanceSq start ps.First )
                let ei = ptss |> ResizeArray.minIndBy ( fun ps -> Pnt.distanceSq start ps.Last)
                let sd = Pnt.distance start ptss.[si].First
                let ed = Pnt.distance start ptss.[ei].Last
                if   sd < tolGap && sd < ed then res.InsertRange(0, ResizeArray.rev(ptss.Pop(si)) )
                elif ed < tolGap && ed < sd then res.InsertRange(0,                 ptss.Pop(ei))
                else
                    loop <- false
        res
       

       // TODO add offset 3d functions
