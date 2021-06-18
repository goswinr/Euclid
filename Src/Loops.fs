namespace FsEx.Geo

open System    
open System .Collections.Generic   


/// Discriminated union for the three possible relations of a point to a closed polyline Loop
[<RequireQualifiedAccess>]
type PointLoopRel = 
    In | On | Out

    member r.IsInside =  r= PointLoopRel.In
    
    member r.IsOutside = r= PointLoopRel.Out 
        


/// A counter-clockwise,  closed series of points.
/// Checks for too short segments
/// Closes loop if not closed yet.
/// Makes it Counterclockwise.
/// Also check for self intersection.
/// Does NOT remove colinear points.
type Loop private ( pts:ResizeArray<Pt>
                  , unitVcts:UnitVc[]
                  , bboxes:BRect[] 
                  , lens:float[] 
                  , xys:float[] // TODO is this needed to be always precomputed ?
                  , area:float
                  , minSegmentLength:float
                  , snapThreshold:float
                  , box:BRect
                  ) =    

    /// Without sign,  since loop is guaranteed to be Counter Clockwise
    member _.Area = area

    /// This list is one item shorter than Points
    member _.BBoxes = bboxes
   
    /// This list is one item shorter than Points
    member _.UnitVectors = unitVcts

    /// This list is one item shorter than Points
    member _.Lengths = lens
    
    /// List of X and Y coordinates of Points.
    /// Even indices are X
    /// Odd  indices are Y
    /// Length is double of SegmentCount
    /// Last pair is not equal first pair.
    member _.XandYs = xys

    /// This list is one item Longer than Vectors , BBoxes or Lengths
    /// Last Point equals first Point
    member _.Points = pts
             
    /// One less than Points count 
    member val SegmentCount = unitVcts.Length  
        
    /// The overall Bounding Box. 
    /// Including snapThreshold expansion.
    member _.BoundingBox = box
        
    member _.MinSegmentLength = minSegmentLength
        
    member _.SnapThreshold = snapThreshold           

    /// Creates a deep copy
    member _.Clone() = Loop(pts.GetRange(0,pts.Count), Array.copy unitVcts, Array.copy bboxes, Array.copy lens, Array.copy xys, area, minSegmentLength, snapThreshold, box )

    /// Returns closest segment index        
    member lo.ClosestSegment (pt:Pt) :int=  
        let ps = lo.Points
        let us = lo.UnitVectors
        let ls = lo.Lengths
        let mutable dMin  = Double.MaxValue 
        let mutable iMin  = -1 
        for i = 0 to lo.SegmentCount-1 do  
            let t = ps.[i]
            let n = ps.[i+1]
            let u = us.[i]
            let l = ls.[i] 
            let d = pt.DistanceSqToLine(t, n, u, l)
            if d < dMin then  
                dMin  <- d 
                iMin  <- i
        iMin
        
     /// Returns closest and second closest segment index  
     /// They might both contain the closest point. ( in the corner where they meet) 
    member lo.ClosestSegments (pt:Pt) =  
        let ps = lo.Points
        let us = lo.UnitVectors
        let ls = lo.Lengths
        let mutable dMin  = Double.MaxValue 
        let mutable iFst  = 0
        let mutable iSnd  = 0 
        for i = 0 to lo.SegmentCount-1 do  
            let t = ps.[i]
            let n = ps.[i+1]
            let u = us.[i]
            let l = ls.[i] 
            let d = pt.DistanceSqToLine(t, n, u, l)
            if d <= dMin then  
                dMin  <- d 
                iSnd <- iFst
                iFst <- i 
        iFst, iSnd   
        
    /// Returns closest Point, 
    member lo.ClosestPoint (pt:Pt) :Pt= 
        let i = lo.ClosestSegment(pt) 
        let t = lo.Points.[i]
        let u = lo.UnitVectors.[i]
        let l = lo.Lengths.[i] 
        pt.ClosestPointOnLine(t, u, l) 
        
    /// Returns Relation between point and Loop: Inside, On or outside
    /// Tolerance for beeing on Loop is SnapThreshold    
    member lo.ContainsPoint(pt:Pt) =  
        // this implementation using the closest two segments is always correct.
        // faster implementations such as counting the crossings of a horizontal ray do fail if several loop segments are on the x axis and the test point too.
        if not <| lo.BoundingBox.Contains(pt) then  
            PointLoopRel.Out
        else
            let ps = lo.Points
            let us = lo.UnitVectors
            let ls = lo.Lengths
            let j, k  = lo.ClosestSegments(pt)     
            
            let pj = ps.[j]
            let uj = us.[j] 
            let lj = ls.[j]
            let dj = pt.DistanceToLine(pj, uj, lj) 
            if dj < snapThreshold then  
                PointLoopRel.On
            else 
                let pk = ps.[k]
                let uk = us.[k]
                let lk = ls.[k]
                let dk = pt.DistanceToLine(pk, uk, lk)  
                let i =  
                    if dj + lo.SnapThreshold < dk then // check if dj is closer than dk by more than the tolerance
                        j
                    else                        
                        // explicitly compare both offset of the two closest segments 
                        let uj90 = uj.RotatedCW * lo.SnapThreshold
                        let ddj = min (pt.DistanceSqToLine(pj+uj90 , uj, lj)) (pt.DistanceSqToLine(pj-uj90, uj, lj)) 
                        let uk90 = uk.RotatedCW * lo.SnapThreshold
                        let ddk = min (pt.DistanceSqToLine(pk+uk90 , uk, lk)) (pt.DistanceSqToLine(pk-uk90, uk, lk))                         
                        if ddj <= ddk then j else k                
                
                // once correct index is found check on which side the point is 
                if Vc.cross (lo.UnitVectors.[i] , pt-lo.Points.[i] ) < 0.0 then 
                    PointLoopRel.Out
                else                                                             
                    PointLoopRel.In           

    /// Creates a Loop from series of points.
    /// Checks for too short segments
    /// Closes loop if not closed yet.
    /// Makes it Counterclockwise.
    /// Also check for self intersection.
    /// Does NOT remove colinear points.
    static member create (minSegmentLength:float) (snapThreshold:float) (points:IList<Pt>)=
        let pts = 
            if minSegmentLength < 0.0 then FsExGeoException.Raise "FsEx.Geo.Loop constructor: minSegmentLength < 0.0:  %g" minSegmentLength
            if snapThreshold    < 0.0 then FsExGeoException.Raise "FsEx.Geo.Loop constructor: snapThreshold < 0.0:  %g" snapThreshold
            if points.Count<3 then FsExGeoException.Raise "FsEx.Geo.Loop constructor: Input ResizeArray needs to have a least three points, not  %d " points.Count
                    
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
                    Debug2D.drawDot (sprintf "short segm: %d" (i-1)) pt
                    #if DEBUG
                    eprintfn "Loop constructor: Segment %d shorter than %g was skiped, it was just %g long." (i-1) snapThreshold (Pt.distance ps.Last pt)
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
        let  unitVcts, bboxes , lens , xys=        
            let uvs  = Array.zeroCreate (segCount)
            let bs   = Array.zeroCreate (segCount)
            let lens = Array.zeroCreate (segCount)
            let xy   = Array.zeroCreate (segCount*2)
            let mutable xyi = 0 
            let mutable t = pts.[0]
            for ii = 1 to pts.Count-1 do // start at +1,  last = first
                let n = pts.[ii]
                let v = Vc.create(t, n)
                let l = v.Length
                let i = ii-1 // becaus loop starts at 1            
                uvs.[ i] <- UnitVc.createUnchecked( v.X/l , v.Y/l) // no check for div by zero needed,  since minSpacing is already checked
                bs.[  i] <- BRect.create(t, n, snapThreshold)
                lens.[i] <- l
                xy.[xyi  ] <- t.X
                xy.[xyi+1] <- t.Y
                xyi <- xyi + 2
        
                // overall bounding box:
                xmin <- min xmin t.X
                ymin <- min ymin t.Y
                xmax <- max xmax t.X
                ymax <- max ymax t.Y
                // swap:
                t <- n
            uvs,  bs , lens, xy
        
        
        // Test for 180 U-turns
        // angle 160 degrees, dot product of unit vectors: -0.93969
        // angle 170 degrees, dot product of unit vectors: -0.984808
        // angle 178 degrees, dot product of unit vectors: -0.999391
        // Check ther is no U-Turn between 170 and 180 degrees
        let mutable t = unitVcts.[0]
        for ii=1 to segLastIdx do
            let n = unitVcts.[ii]
            if t*n < -0.984808 then   
                Debug2D.drawDot "+170° turn?" pts.[ii]
                FsExGeoException.Raise "FsEx.Geo.Loop: Lines for Loop make a kink between 170 and 180 degrees."
            t <- n
                
        // Check for self intersection, 
        let inline selfIntersectionCheck(i,from,till)= 
            // TODO quadratic runtime !  replace with sweep line algorithm ?
            let ap  = pts.[i]
            let au  = unitVcts.[i]
            let al  = lens.[i]
            let abb = bboxes.[i]
            for j = from to till do
                let bbb = bboxes.[j]
                /// test on BRect overlap could be done here already instead of in doIntersectOrOverlapColinear
                let bp  = pts.[j]
                let bu  = unitVcts.[j]
                let bl  = lens.[j]
                if Intersect.doIntersectOrOverlapColinear(ap, au, al, abb,  bp, bu, bl, bbb, snapThreshold ) then 
                    Debug2D.drawDot (sprintf "self X:  %O +  %O"  i j) (Intersect.getXPointOrMid(ap, au, al,  bp, bu, bl, snapThreshold))
                    Debug2D.drawLine(ap,ap+au*al)
                    Debug2D.drawLine(bp,bp+bu*bl)
                    FsExGeoException.Raise "FsEx.Geo.Loop: Loop of  %O Points has self intersection." points.Count
        
        if unitVcts.Length > 3 then // a triangle is covered by angle checks above
            // checking second last and last
            selfIntersectionCheck(segLastIdx  , 1, 1)
            selfIntersectionCheck(segLastIdx-1, 0, 0)
            for i = 1 to segLastIdx do // start at second ,  (last and first do intersect)
                // TODO quadratic runtime !  replace with sweep line algorithm ?
                selfIntersectionCheck(i, i+2 , segLastIdx)   
        
        let box = BRect.createUnchecked(xmin-snapThreshold, ymin-snapThreshold, xmax+snapThreshold, ymax+snapThreshold) 

        Loop(pts, unitVcts, bboxes, lens, xys, area, minSegmentLength, snapThreshold, box)


        // see script files in Test Folder for almost working code of Loop Loop Intersection:
        // Loop Boolean Ops 14.fsx
