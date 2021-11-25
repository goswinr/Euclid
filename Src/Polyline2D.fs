namespace FsEx.Geo

open System
open Util

# nowarn "52" // copying of structs

/// A mutable 2D Polyline.
/// If the last point is the same as the first point, the Polyline2D is closed.
[<Struct; NoEquality; NoComparison>] // because its made up from floats
type Polyline2D =  
    
    /// Gets the internal list of all Points of Polyline3D.
    /// This is not a copy, so changes to the list will be reflected in the Polyline3D.
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

    /// Test if Polyline2D is CounterClockwise in top view. 
    /// Z values are ignored.
    /// The Polyline2D does not need to be actually closed.
    /// The signed area of the Polyline2D is calculated. 
    /// If it is positive the Polyline2D is CCW.
    member p.IsCounterClockwise = 
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
    member pl.EvaluateAt(t:float) =
        let i = int t
        let p = t - float i
        if   i < -1 then 
            FsExGeoException.Raise "FsEx.Geo.Polyline2D.EvaluateAt: Parameter %f is less than 0.0" t
        elif i > pl.Points.Count then 
            FsExGeoException.Raise "FsEx.Geo.Polyline2D.EvaluateAt: Parameter %f is more than than point count(%d)." t pl.Points.Count 
        elif i =  -1 then 
            if p > 0.9999 then pl.Points.First
            else FsExGeoException.Raise "FsEx.Geo.Polyline2D.EvaluateAt: Parameter %f is less than 0.0" t
        elif i = pl.Points.Count then 
            if   p > 1e-4 then  FsExGeoException.Raise "FsEx.Geo.Polyline2D.EvaluateAt: Parameter %f is more than than point count(%d)." t pl.Points.Count 
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
            FsExGeoException.Raise "FsEx.Geo.Polyline2D.EvaluateAt: Parameter %f is less than 0.0" t
        elif i > pl.Points.Count then 
            FsExGeoException.Raise "FsEx.Geo.Polyline2D.EvaluateAt: Parameter %f is more than than point count(%d)." t pl.Points.Count 
        elif i =  -1 then 
            if p > 0.9999 then UnitVc.create(pl.Points.First,pl.Points.Second)
            else FsExGeoException.Raise "FsEx.Geo.Polyline2D.EvaluateAt: Parameter %f is less than 0.0" t
        elif i = pl.Points.Count then 
            if   p > 1e-5 then  FsExGeoException.Raise "FsEx.Geo.Polyline2D.EvaluateAt: Parameter %f is more than than point count(%d)." t pl.Points.Count 
            else UnitVc.create(pl.Points.SecondLast,pl.Points.Last)
        // return point  if point is almost matching
        else
            UnitVc.create(pl.Points.[i],pl.Points.[i+1])
                

    /// Returns the parameter on the Polyline2D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at points.Count - 1.0 . 
    member pl.ClosestParameter(pt:Pt) =
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
            // finding ClosestParameter on line segment and clamp to 0.0 to 1.0
            let len = v.LengthSq
            ts[i] <- if len < 1e-9 then 0.0 else -((p-pt) * v) / len |> Util.clampBetweenZeroAndOne //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        
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
    
    /// Returns the point on the Polyline2D that is the closest point to the given point.
    member pl.ClosestPoint(pt:Pt) =
        let t = pl.ClosestParameter pt
        pl.EvaluateAt t

    /// Returns the Distance of the test point to the closest point on the Polyline2D.
    member pl.DistanceTo(pt:Pt) =
        let t = pl.ClosestParameter pt
        pl.EvaluateAt t  
        |> Pt.distance pt



    //-------------------------------------------------------------------
    //------------------------static members-----------------------------
    //-------------------------------------------------------------------

    /// Gets the internal list of all Points of Polyline3D.
    /// This is not a copy, so changes to the list will be reflected in the Polyline3D.
    static member inline pointsUnsafeInternal (p:Polyline2D) = p.Points

    /// Gets first Point of Polyline2D
    static member inline start (p:Polyline2D) = 
        if p.Points.Count < 2 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Polyline2D.Start failed on Polyline2D with less than 2 points %O" p
        p.Points.First
    
    /// Gets last or end Point of Polyline2D
    static member inline ende (p:Polyline2D) = 
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
    static member inline evaluateAt (pl:Polyline2D) t = pl.EvaluateAt t

    /// Apply a mapping function to each point in the 2D Polyline. Return new Polyline2D.
    static member map (mapping:Pt->Pt) (pl:Polyline2D) = pl.Points.ConvertAll (System.Converter mapping) |> Polyline2D.createDirectlyUnsafe

    /// Move a Polyline2D by a vector. (same as Polyline2D.move)
    static member inline translate (v:Vc) (pl:Polyline2D) = pl |> Polyline2D.map (Pt.addVc v)

    /// Move a Polyline2D by a vector. (same as Polyline2D.translate)
    static member inline move (v:Vc) (pl:Polyline2D) = Polyline2D.translate v pl

    /// Returns a Polyline2D moved by a given distance in X direction.
    static member inline moveX (distance:float) (pl:Polyline2D)  = pl |> Polyline2D.map (Pt.moveX distance)

    /// Returns a Polyline2D moved by a given distance in Y direction.
    static member inline moveY (distance:double) (pl:Polyline2D) = pl |> Polyline2D.map (Pt.moveY distance)

    /// Rotation a Polyline2D around Z-Axis.
    static member inline rotate (r:Rotation2D) (pl:Polyline2D) = pl |> Polyline2D.map (Pt.rotateBy r) 
    
    /// Rotation a Polyline2D round given Center point an a local Z Axis.
    static member inline rotateOn (cen:Pt) (r:Rotation2D) (pl:Polyline2D) = pl |> Polyline2D.map (Pt.rotateWithCenterBy cen r) 

    /// Returns the parameter on the Polyline2D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at point count. 
    static member inline closestParameter (pl:Polyline2D) (pt:Pt) = pl.ClosestParameter pt

    /// Returns the point on the Polyline2D that is the closest point to the given point.
    static member inline closestPoint (pl:Polyline2D) (pt:Pt) = pl.ClosestPoint pt

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

    /// Returns new Polyline2D from point at Parameter a to point at Parameter b.
    /// if 'a' is bigger 'b' then the new Polyline2D is in opposite direction.
    /// If a parameter is within 1e-5 of an integer value, the integer value is used as parameter.
    static member segment a b (pl:Polyline2D) = 
        let rev = a<b
        let u,v = if rev then b,a else a,b
        let np = Polyline2D.empty(int(v-u)+2)
        let nps = np.Points
        let ps  = pl.Points
        // first point
        let ui = int u
        let uf = u - float ui
        if uf < 0.9999 then 
            nps.Add(pl.EvaluateAt u)
        // inner points
        for i = int u + 1 to int v do 
            nps.Add(ps[i])
        // last point
        let vi = int v
        let vf = v - float vi
        if vf > 1e-4 then 
            nps.Add(pl.EvaluateAt v)
        // reverse if necessary
        if rev then 
            np.ReverseInPlace()
        np    
