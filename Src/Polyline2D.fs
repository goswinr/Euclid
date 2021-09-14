namespace FsEx.Geo

open System
open Util

# nowarn "52" // copying of structs

/// A mutable 2D Polyline.
/// If the last point is the same as the first point, the Polyline2D is closed.
[<Struct; NoEquality; NoComparison>] // because its made up from floats
type Polyline2D =  
    /// The Origin Corner of the Box.
    val Points: ResizeArray<Pt>

    /// Internal constructor. Uses input List without copying it.
    internal new (points: ResizeArray<Pt>) = { Points = points }
        
    /// Nicely formatted string representation of the Box including its size.
    override pl.ToString() = 
        if pl.Points.Count = 0 then "An empty FsEx.Geo.Polyline2D."
        else sprintf"FsEx.Geo.Polyline2D with %d points from %s to %s" pl.Points.Count pl.Points.First.AsString pl.Points.Last.AsString      
    
    /// Creates a copy of the Polyline2D
    member inline p.Duplicate(): Polyline2D = 
        let ps = p.Points
        Polyline2D.createDirectlyUnsafe(ps.GetRange(0,ps.Count))

    /// Gets first Point of Polyline2D
    member p.Start = 
        if p.Points.Count < 2 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Polyline2D.Start failed on Polyline2D with less than 2 points %O" p
        p.Points.First
    
    /// Gets last or end Point of Polyline2D
    member p.End = 
        if p.Points.Count < 2 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Polyline2D.Start failed on Polyline2D with less than 2 points %O" p
        p.Points.Last

    /// Tests if Polyline2D is closed within 1e-16 units tolerance.
    member p.IsClosed = 
        if p.Points.Count < 2 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Polyline2D.IsClosed failed on Polyline2D with less than 2 points %O" p
        let v = p.Start  - p.End 
        v.LengthSq < 1e-32 //Util.zeroLengthTol**2

    /// Tests if Polyline2D is closed within given tolerance.
    member p.IsAlmostClosed(tolerance) = 
        if p.Points.Count < 2 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Polyline2D.IsAlmostClosed failed on Polyline2D with less than 2 points %O" p
        let v = p.Start  - p.End 
        v.LengthSq < tolerance*tolerance
        
    
    /// Reverse order of Polyline2D in place
    member p.ReverseInPlace() = 
        p.Points.Reverse()

    /// Return new Polyline2D in reversed Order
    member p.Reverse () = 
        let n = p.Duplicate()
        n.Points.Reverse()
        n

    /// Test if Polyline2D is CounterClockwise when projected in 2D. 
    /// Z values are ignored.
    /// The Polyline2D does not need to be actually closed.
    /// The signed area of the Polyline2D is calculated. 
    /// If it is positive the Polyline2D is CCW.
    member p.IsCounterClockwiseIn2D () = 
        //https://helloacm.com/sign-area-of-irregular-polygon/
        let ps = p.Points
        let mutable area = 0.0
        let mutable t = ps.Last // calculate from last to first too
        for i=0 to ps.Count-1 do
            let n = ps.[i]
            let a = t.X - n.X
            let b = n.Y + t.Y
            area <- area + a*b
            t <- n
        if   abs(area) < Util.zeroLengthTol then FsExGeoException.Raise "FsEx.Geo.Polyline2D.IsCounterClockwiseIn2D: Polyline2D as area 0.0: %O" p
        else area > 0.0 
        


    /// Returns the point at a given parameter on the Polyline2D.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at points.Count - 1.0 . 
    member pl.Evaluate(t:float) =
        let i = int t
        let p = t - float i
        if   i < -1 then 
            FsExGeoException.Raise "FsEx.Geo.Polyline2D.Evaluate: Parameter %f is less than 0.0" t
        elif i > pl.Points.Count then 
            FsExGeoException.Raise "FsEx.Geo.Polyline2D.Evaluate: Parameter %f is more than than point count(%d)." t pl.Points.Count 
        elif i =  -1 then 
            if p > 0.9999 then pl.Points.First
            else FsExGeoException.Raise "FsEx.Geo.Polyline2D.Evaluate: Parameter %f is less than 0.0" t
        elif i = pl.Points.Count then 
            if   p > 1e-4 then  FsExGeoException.Raise "FsEx.Geo.Polyline2D.Evaluate: Parameter %f is more than than point count(%d)." t pl.Points.Count 
            else pl.Points.Last
        // return point  if point is almost matching
        elif  p < zeroLengthTol then 
            pl.Points.[i]
        elif  p > 0.99999964 then // 0.99999964  is 6 steps smaller than 1.0: https://float.exposed/0x3f7ffffa
            pl.Points.[i+1]    
        else
            let t = pl.Points.[i]
            let v = pl.Points.[i+1] - t
            t + v * p

    /// Returns the Unitized Tangent at a given parameter on the Polyline2D.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at points.Count - 1.0 . 
    member pl.TangentAt(t:float) =
        let i = int t
        let p = t - float i
        if   i < -1 then 
            FsExGeoException.Raise "FsEx.Geo.Polyline2D.Evaluate: Parameter %f is less than 0.0" t
        elif i > pl.Points.Count then 
            FsExGeoException.Raise "FsEx.Geo.Polyline2D.Evaluate: Parameter %f is more than than point count(%d)." t pl.Points.Count 
        elif i =  -1 then 
            if p > 0.9999 then UnitVc.create(pl.Points.First,pl.Points.Second)
            else FsExGeoException.Raise "FsEx.Geo.Polyline2D.Evaluate: Parameter %f is less than 0.0" t
        elif i = pl.Points.Count then 
            if   p > 1e-4 then  FsExGeoException.Raise "FsEx.Geo.Polyline2D.Evaluate: Parameter %f is more than than point count(%d)." t pl.Points.Count 
            else UnitVc.create(pl.Points.SecondLast,pl.Points.Last)
        // return point  if point is almost matching
        else
            UnitVc.create(pl.Points.[i],pl.Points.[i+1])
        
        

    /// Returns the parameter on the Polyline2D that is the closet point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at points.Count - 1.0 . 
    member pl.ClosetParameter(pt:Pt) =
        // for very large Polyline2Ds, this is could be optimized by using search R-tree        
        let ps = pl.Points
        if ps.Count < 2 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Polyline2D.ClosestParameter failed on  Polyline2D with less than 2 points %O" pl
        // vectors of the segments 
        let vs = Array.zeroCreate (ps.Count-1)
        for i = 0 to vs.Length-1 do 
            let ti = ps[i]
            let ne = ps[i+1]
            vs[i] <- ne-ti
        
        // closest parameters  of the segments 
        let ts = Array.zeroCreate (ps.Count-1)
        for i = 0 to ts.Length-1 do 
            let p = ps[i]
            let v = vs[i]
            // finding ClosetParameter on line segment and clamp to 0.0 to 1.0
            let len = v.LengthSq
            ts[i] <- if len < 1e-9 then 0.0 else -((p-pt) * v) / len |> Util.clamp01 //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        
        // square distances per segment
        let ds = Array.zeroCreate (ps.Count-1)
        for i = 0 to ds.Length-1 do 
            let p = ps[i]
            let v = vs[i]
            let t = ts[i]
            ds[i] <- Pt.distanceSq pt (p + v*t)
        
        let i = Array.minIndex ds
        let t = ts.[i]
        float i + t
    
    /// Returns the point on the Polyline2D that is the closet point to the given point.
    member pl.ClosetPoint(pt:Pt) =
        let t = pl.ClosetParameter pt
        pl.Evaluate t

    /// Returns the Distance of the test point to the closest point on the Polyline2D.
    member pl.DistanceTo(pt:Pt) =
        let t = pl.ClosetParameter pt
        pl.Evaluate t  
        |> Pt.distance pt



    //-------------------------------------------------------------------
    //------------------------static members-----------------------------
    //-------------------------------------------------------------------


    /// Gets first Point of Polyline2D
    static member start (p:Polyline2D) = 
        if p.Points.Count < 2 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Polyline2D.Start failed on Polyline2D with less than 2 points %O" p
        p.Points.First
    
    /// Gets last or end Point of Polyline2D
    static member ende (p:Polyline2D) = 
        if p.Points.Count < 2 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Polyline2D.Start failed on Polyline2D with less than 2 points %O" p
        p.Points.Last
    
    /// Reverse order of Polyline2D in place
    static member inline reverseInPlace (p:Polyline2D) = p.ReverseInPlace()

    /// Returns new Polyline2D in reversed Order
    static member inline reverse (p:Polyline2D) = p.Reverse()       

    /// Returns the point at a given parameter on the Polyline2D.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at point count. 
    static member inline evaluate (pl:Polyline2D) t = pl.Evaluate t

    /// Returns the parameter on the Polyline2D that is the closet point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at point count. 
    static member inline closestParameter (pl:Polyline2D) (pt:Pt) = pl.ClosetParameter pt

    /// Returns the point on the Polyline2D that is the closet point to the given point.
    static member inline closestPoint (pl:Polyline2D) (pt:Pt) = pl.ClosetPoint pt

    /// Returns the Distance of the test point to the closest point on the Polyline2D.
    static member inline distanceTo (pl:Polyline2D) (pt:Pt) = pl.DistanceTo pt

    /// Create a new Polyline2D by copying over all points.
    static member create(points: seq<Pt>) = Polyline2D(ResizeArray(points))
    
    /// Create a new Polyline2D by using the provided ResizeArray directly.
    /// All later changes to the ResizeArray will be reflected in the Polyline2D.
    static member createDirectlyUnsafe (points: ResizeArray<Pt>) = Polyline2D( points )
    
    /// Create a new empty Polyline2D without any points. 
    /// But predefined capacity
    static member empty (capacity:int) = Polyline2D(ResizeArray(capacity))
