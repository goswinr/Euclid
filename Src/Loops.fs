namespace FsEx.Geo

open System
    
    

/// A counter-clockwise,  closed series of points.
/// Checks for too short segments
/// Closes loop if not closed yet.
/// Makes it Counterclockwise.
/// Also check for self intersection.
/// Does NOT remove colinear points.
type Loop(points:ResizeArray<Pt>, minSegmentLength:float, snapThreshold:float) =
        
    let pts = 
        if points.Count<3 then FsExGeoException.Raise $"FsEx.Geo.Loop constructor: Input ResizeArray needs to have a least three points, not {points.Count} " 
            
        let ps= ResizeArray<Pt>(points.Count+1)
        // check gap sizes
        let minLenSq = minSegmentLength * minSegmentLength
        ps.Add points.[0]
        for i = 1 to points.Count-1 do
            let pt = points.[i]
            if Pt.distanceSq ps.Last pt > minLenSq then
                ps.Add pt
            else
                // set last to average
                ps.Last <- (ps.Last + pt) *0.5 
                Debug2D.drawDot $"short segm:{i-1}" pt 
                #if DEBUG
                eprintfn $"Loop constructor: Segment {i-1} shorter than {snapThreshold} was skiped, it was just {Pt.distance ps.Last pt} long."
                #endif
        // close
        if Pt.distanceSq ps.Last ps.First > minLenSq then
            ps.Add ps.First // add first to close loop
        else
            ps.Last <- ps.First // make sure they are exactly the same
        ps

    let segCount = pts.Count-1
        
    let segLastIdx = pts.Count-2      
    

    // get Area also Reverse to make it counter clockwise if needed
    let area =
        let sa = Points.getSignedArea pts
        if sa < 0. then  pts.Reverse() ;  -sa
        else sa 
        
    let mutable xmin, ymin = Double.MaxValue, Double.MaxValue // for overall bounding box
    let mutable xmax, ymax = Double.MinValue, Double.MinValue
        
    // loop again to precalculate vectors ,  unit vectors,   BBoxes,  and lengths
    let vcts, unitVcts, bboxes , lens=
        let vs   = Array.zeroCreate (segCount)
        let uvs  = Array.zeroCreate (segCount)
        let bs   = Array.zeroCreate (segCount)
        let lens = Array.zeroCreate (segCount)
        let mutable t = pts.[0]
        for ii = 1 to pts.Count-1 do // start at +1,  last = first
            let n = pts.[ii]
            let v = Vc.create(t, n)
            let l = v.Length
            let i = ii-1 // becaus loop starts at 1
            vs.[  i] <- v
            uvs.[ i] <- UnitVc.createUnchecked( v.X/l , v.Y/l) // no check for div by zero needed,  since minSpacing is already checked
            bs.[  i] <- BBox(t, n, snapThreshold)
            lens.[i] <- l
            // overall bounding box:
            xmin <- min xmin t.X
            ymin <- min ymin t.Y
            xmax <- max xmax t.X
            ymax <- max ymax t.Y
            // swap:
            t <- n
        vs, uvs,  bs , lens

    do
        // Test for 180 U-turns
        // angle 160 degrees, dot product of unit vectors: -0.93969
        // angle 170 degrees, dot product of unit vectors: -0.984808
        // angle 178 degrees, dot product of unit vectors: -0.999391
        // Check ther is no U-Turn between 170 and 180 degrees
        let mutable t = unitVcts.[0]
        for ii=1 to segLastIdx do
            let n = unitVcts.[ii]
            if t*n < -0.984808 then   
                Debug2D.drawDot $"+170° turn?" pts.[ii]
                FsExGeoException.Raise $"FsEx.Geo.Loop: Lines for Loop make a kink between 170 and 180 degrees."
            t <- n

        // check for self intersection, 
        if vcts.Length > 3 then // a triangle is cover by angle checks above
            // checking second last and last
            if Intersect.doIntersect( pts.[segLastIdx]  , unitVcts.[segLastIdx]  , lens.[segLastIdx]   , pts.[1], unitVcts.[1], lens.[1] , snapThreshold ) then   
                Debug2D.drawDot $"self X last with second" pts.[segLastIdx]
                Debug2D.drawPolyLine pts
                FsExGeoException.Raise $"FsEx.Geo.Loop: Loop has self intersection last with second"
            if Intersect.doIntersect( pts.[segLastIdx-1], unitVcts.[segLastIdx-1], lens.[segLastIdx-1] , pts.[0], unitVcts.[0], lens.[0] , snapThreshold ) then   
                Debug2D.drawDot $"self X second last with first" pts.[segLastIdx-1]
                Debug2D.drawPolyLine pts
                FsExGeoException.Raise $"FsEx.Geo.Loop: Loop has self intersection second last with first"
            // check each segment with all other segemnts
            for i = 1 to segLastIdx do // start at second ,  (last and first do intersect)
                // TODO quadratic O !  replace with sweep line algorithm ?
                let ap = pts.[i]
                let au = unitVcts.[i]
                let al = lens.[i]
                for j = i+2 to segLastIdx do
                    let bp = pts.[j]
                    let bu = unitVcts.[j]
                    let bl = lens.[j]
                    if Intersect.doIntersect(ap, au, al,  bp, bu, bl, snapThreshold ) then 
                        Debug2D.drawDot $"self X:{i}+{j}" bp
                        Debug2D.drawPolyLine pts
                        FsExGeoException.Raise $"FsEx.Geo.Loop: Loop of {points.Count} Points has self intersection."

    /// Without sign,  since loop is guaranteed to be Counter Clockwise
    member _.Area = area

    /// This list is one item shorter than Points
    member _.BBoxes = bboxes

    /// This list is one item shorter than Points
    member _.Vectors = vcts

    /// This list is one item shorter than Points
    member _.UnitVectors = unitVcts

    /// This list is one item shorter than Points
    member _.Lengths = lens

    /// This list is one item Longer than Vectors , BBoxes or Lengths
    /// Last Point equals first Point
    member _.Points = pts
        
    /// One more than Vector count 
    member val PointCount = pts.Count
        
    /// One less than Points count 
    member val VecCount = vcts.Length  
        
    /// The overall Bounding Box,  including snapThreshold
    member val BoundingBox = BBox(Pt(xmin, ymin) , Pt(xmax, ymax) , snapThreshold) 
        
    member _.MinSegmentLength = minSegmentLength
        
    member _.SnapThreshold = snapThreshold

    
open Intersect
    
[<RequireQualifiedAccess>]
type ContinueOn = A | B 
    
type Location = { 
    aIdx:int
    bIdx:int
    at:float // parameter on unitvector of A segment
    bt:float // parameter on unitvector of B segment
    dir: ContinueOn 
    }
    
// now develeped in Geeometry2D test:
    
//let booleanIntersection (loopA:Loop, loopB:Loop) :ResizeArray<Loop> = 
//    if loopA.SnapThreshold <> loopB.SnapThreshold then eprintfn $"Loop.union: loopA.SnapThreshold {loopA.SnapThreshold} <> loopB.SnapThreshold {loopB.SnapThreshold}"
//    if loopA.MinSegmentLength <> loopB.MinSegmentLength then eprintfn $"Loop.union: loopA.MinSegmentLength {loopA.MinSegmentLength} <> loopB.MinSegmentLength {loopB.MinSegmentLength}"
        
//    if not <| BBoxes.overlap loopA.BoundingBox loopB.BoundingBox then  
//        ResizeArray.empty 
//    else 
//        let aLen  = loopA.VecCount
//        let bLen  = loopB.VecCount
//        let aBox  = loopA.BBoxes
//        let bBox  = loopB.BBoxes
//        let aPts  = loopA.Points
//        let bPts  = loopB.Points
//        let aUnit = loopA.UnitVectors       
//        let bUnit = loopB.UnitVectors       
//        let aLens = loopA.Lengths       
//        let bLens = loopB.Lengths         
            
//        let locs = ResizeArray<Location>() 
            
//        /// first collect just the intersection points
//        for ai = 0 to aLen-1 do  
//            for bi = 0 to bLen-1 do // TODO quadratic!  replace with sweep line algorithm 
//                if BBoxes.overlap aBox.[ai] bBox.[bi] then 
//                    match getRelation(aPts.[ai], aUnit.[ai], aLens.[ai], bPts.[bi], bUnit.[bi], bLens.[bi], loopA.SnapThreshold) with 
//                    |NoIntersection ->  ()
//                    |Parallel       ->  ()  // more than threshold apart 
//                    |Colinear ->  failwith "implementation for Colinear overlap missing" //TODO
                            
//                    // parameters for unit vector,  might be out of bound by snapThreshold
//                    |BfromRight (at, bt)  ->  locs.Add {aIdx = ai;  bIdx = bi;  at=at; bt=bt ; dir = ContinueOn.B} 
//                    |BfromLeft  (at, bt)  ->  locs.Add {aIdx = ai;  bIdx = bi;  at=at; bt=bt ; dir = ContinueOn.A} // swaping a and b would yield boolean union !
            
//        /// second loop over points
//        if locs.IsEmpty then 
//            ResizeArray.empty 
//        elif locs.Count % 2 = 1 then  
//            failwith "implementation for odd location count missing"
//        else 
//            let resPts = ResizeArray<Pt>() 
                
//            let inline addPt(l:Location) =  
//                resPts.Add (aPts.[l.aIdx] +  aUnit.[l.aIdx] * l.at) // TODO add clamping ?
                
//            let inline addPts(si, ei, fromPts:ResizeArray<Pt>) =  
//                if si<=ei then  
//                    for i=si to ei do resPts.Add (fromPts.[i]) 
//                else 
//                    for i=si to fromPts.Count-2 do resPts.Add (fromPts.[i]) // -2 because start and end are the same
//                    for i=0  to ei              do resPts.Add (fromPts.[i]) 
                
                
//            locs |> ResizeArray.sortInPlaceBy(fun l -> l.aIdx, l.at) 
//            for t, n in Seq.thisNext locs do  
//                addPt t
//                match t.dir with
//                |ContinueOn.A ->  
//                    Debug2D.drawDot $"onA: {resPts.Count-1}" resPts.Last
//                    addPts(t.aIdx+1, n.aIdx, aPts) ; 
//                |ContinueOn.B ->  
//                    Debug2D.drawDot $"onB: {resPts.Count-1}" resPts.Last
//                    addPts(t.bIdx+1, n.bIdx, bPts) ; 
                
//            ResizeArray.singelton <| Loop(resPts,loopA.MinSegmentLength, loopA.SnapThreshold )  
                            
    
        
        
        
        
        
        
        
        
        
        
        
