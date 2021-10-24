namespace FsEx.Geo

open System
open Util

# nowarn "52" // copying of structs

/// OptionalAttribute for member parameters
type internal OPT = Runtime.InteropServices.OptionalAttribute

/// DefaultParameterValueAttribute for member parameters
type internal DEF =  Runtime.InteropServices.DefaultParameterValueAttribute


/// A mutable 3D Polyline.
/// If the last point is the same as the first point, the Polyline3D is closed.
[<Struct; NoEquality; NoComparison>] // because its made up from floats
type Polyline3D =  
    



    /// Gets the internal list of all Points of Polyline3D.
    /// This is not a copy, so changes to the list will be reflected in the Polyline3D.
    val Points: ResizeArray<Pnt>

    /// Internal constructor. Uses input List without copying it.
    internal new (points: ResizeArray<Pnt>) = { Points = points }

        
    /// Nicely formatted string representation of the Box including its size.
    override pl.ToString() = 
        if pl.Points.Count = 0 then "An empty FsEx.Geo.Polyline3D."
        else sprintf"FsEx.Geo.Polyline3D with %d points from %s to %s" pl.Points.Count pl.Points.First.AsString pl.Points.Last.AsString      
    
    /// Creates a copy of the Polyline3D
    member inline p.Duplicate(): Polyline3D = 
        let ps = p.Points
        Polyline3D.createDirectlyUnsafe(ps.GetRange(0,ps.Count))

    /// Gets first Point of Polyline3D
    member p.Start = 
        if p.Points.Count < 2 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Polyline3D.Start failed on Polyline3D with less than 2 points %O" p
        p.Points.First
    
    /// Gets last or end Point of Polyline3D
    member p.End = 
        if p.Points.Count < 2 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Polyline3D.Start failed on Polyline3D with less than 2 points %O" p
        p.Points.Last

    /// Tests if Polyline3D is closed within 1e-16 units tolerance.
    member p.IsClosed = 
        if p.Points.Count < 2 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Polyline3D.IsClosed failed on Polyline3D with less than 2 points %O" p
        let v = p.Start  - p.End 
        v.LengthSq < 1e-32 //Util.zeroLengthTol**2

    /// Tests if Polyline3D is closed within given tolerance.
    member p.IsAlmostClosed(tolerance) = 
        if p.Points.Count < 2 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Polyline3D.IsAlmostClosed failed on Polyline3D with less than 2 points %O" p
        let v = p.Start  - p.End 
        v.LengthSq < tolerance*tolerance
        
    
    /// Reverse order of Polyline3D in place
    member p.ReverseInPlace() = 
        p.Points.Reverse()

    /// Return new Polyline3D in reversed Order
    member p.Reverse () = 
        let n = p.Duplicate()
        n.Points.Reverse()
        n

    /// Test if Polyline3D is CounterClockwise when projected in 2D. 
    /// Z values are ignored.
    /// The Polyline3D does not need to be actually closed.
    /// The signed area of the Polyline3D is calculated. 
    /// If it is positive the Polyline3D is CCW.
    member p.IsCounterClockwiseIn2D = 
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
        if   abs(area) < Util.zeroLengthTol then FsExGeoException.Raise "FsEx.Geo.Polyline3D.IsCounterClockwiseIn2D: Polyline3D as area 0.0: %O" p
        else area > 0.0 
        


    /// Returns the point at a given parameter on the Polyline3D.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline3D starts at 0.0 and ends at points.Count - 1.0 . 
    /// If the parameter is within 1e-5 of an integer value, the integer value is used as parameter
    member pl.EvaluateAt(t:float) =
        let i = int t
        let p = t - float i
        if   i < -1 then 
            FsExGeoException.Raise "FsEx.Geo.Polyline3D.EvaluateAt: Parameter %f is less than 0.0" t
        elif i > pl.Points.Count then 
            FsExGeoException.Raise "FsEx.Geo.Polyline3D.EvaluateAt: Parameter %f is more than than point count(%d)." t pl.Points.Count 
        elif i =  -1 then 
            if p > 0.99999 then pl.Points.First
            else FsExGeoException.Raise "FsEx.Geo.Polyline3D.EvaluateAt: Parameter %f is less than 0.0" t
        elif i = pl.Points.Count then 
            if   p > 1e-5 then  FsExGeoException.Raise "FsEx.Geo.Polyline3D.EvaluateAt: Parameter %f is more than than point count(%d)." t pl.Points.Count 
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

    /// Returns the Unitized Tangent at a given parameter on the Polyline3D.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline3D starts at 0.0 and ends at points.Count - 1.0 . 
    member pl.TangentAt(t:float) =
        let i = int t
        let p = t - float i
        if   i < -1 then 
            FsExGeoException.Raise "FsEx.Geo.Polyline3D.EvaluateAt: Parameter %f is less than 0.0" t
        elif i > pl.Points.Count then 
            FsExGeoException.Raise "FsEx.Geo.Polyline3D.EvaluateAt: Parameter %f is more than than point count(%d)." t pl.Points.Count 
        elif i =  -1 then 
            if p > 0.9999 then UnitVec.create(pl.Points.First,pl.Points.Second)
            else FsExGeoException.Raise "FsEx.Geo.Polyline3D.EvaluateAt: Parameter %f is less than 0.0" t
        elif i = pl.Points.Count then 
            if   p > 1e-4 then  FsExGeoException.Raise "FsEx.Geo.Polyline3D.EvaluateAt: Parameter %f is more than than point count(%d)." t pl.Points.Count 
            else UnitVec.create(pl.Points.SecondLast,pl.Points.Last)
        // return point  if point is almost matching
        else
            UnitVec.create(pl.Points.[i],pl.Points.[i+1])
        
        

    /// Returns the parameter on the Polyline3D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline3D starts at 0.0 and ends at points.Count - 1.0 . 
    member pl.ClosestParameter(pt:Pnt) =
        // for very large polylines, this is could be optimized by using search R-tree        
        let ps = pl.Points
        if ps.Count < 2 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Polyline3D.ClosestParameter failed on  Polyline3D with less than 2 points %O" pl
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
            ts[i] <- if len < 1e-9 then 0.0 else -((p-pt) * v) / len |> Util.clamp01 //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        
        // square distances per segment
        let ds = Array.zeroCreate (ps.Count-1)
        for i = 0 to ds.Length-1 do 
            let p = ps[i]
            let v = vs[i]
            let t = ts[i]
            ds[i] <- Pnt.distanceSq pt (p + v*t)
        
        let i = Array.minIndex ds
        let t = ts.[i]
        float i + t
    
    /// Returns the point on the Polyline3D that is the closest point to the given point.
    member pl.ClosestPoint(pt:Pnt) =
        let t = pl.ClosestParameter pt
        pl.EvaluateAt t

    /// Returns the Distance of the test point to the closest point on the Polyline3D.
    member pl.DistanceTo(pt:Pnt) =
        let t = pl.ClosestParameter pt
        pl.EvaluateAt t  
        |> Pnt.distance pt


    //-------------------------------------------------------------------
    //------------------------static members-----------------------------
    //-------------------------------------------------------------------
    
    /// Gets the internal list of all Points of Polyline3D.
    /// This is not a copy, so changes to the list will be reflected in the Polyline3D.
    static member inline pointsUnsafeInternal (p:Polyline3D) = p.Points

    /// Gets first Point of Polyline3D
    static member inline start (p:Polyline3D) = 
        if p.Points.Count < 2 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Polyline3D.Start failed on Polyline3D with less than 2 points %O" p
        p.Points.First
    
    /// Gets last or end Point of Polyline3D
    static member inline ende (p:Polyline3D) = 
        if p.Points.Count < 2 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Polyline3D.Start failed on Polyline3D with less than 2 points %O" p
        p.Points.Last
    
    /// Reverse order of Polyline3D in place
    static member inline reverseInPlace (p:Polyline3D) = p.ReverseInPlace()

    /// Returns new Polyline3D in reversed Order
    static member inline reverse (p:Polyline3D) = p.Reverse()       

    /// Returns the point at a given parameter on the Polyline3D.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline3D starts at 0.0 and ends at point count. 
    static member inline evaluateAt (t:float) (pl:Polyline3D) = pl.EvaluateAt t


    /// Apply a mapping function to each point in the 3D Polyline. Return new Polyline3D.
    static member map (mapping:Pnt->Pnt) (pl:Polyline3D) = pl.Points.ConvertAll (System.Converter mapping) |> Polyline3D.createDirectlyUnsafe

    /// Move a Polyline3D by a vector. (same as Polyline3D.move)
    static member inline translate (v:Vec) (pl:Polyline3D) = pl |> Polyline3D.map (Pnt.addVec v)

    /// Move a Polyline3D by a vector. (same as Polyline3D.translate)
    static member inline move (v:Vec) (pl:Polyline3D) = Polyline3D.translate v pl

    /// Returns a Polyline3D moved by a given distance in X direction.
    static member inline moveX (distance:float) (pl:Polyline3D)  = pl |> Polyline3D.map (Pnt.moveX distance)

    /// Returns a Polyline3D moved by a given distance in Y direction.
    static member inline moveY (distance:double) (pl:Polyline3D) = pl |> Polyline3D.map (Pnt.moveY distance)
                
    /// Returns a Polyline3D moved by a given distance in Z direction.
    static member inline moveZ (distance:double) (pl:Polyline3D) = pl |> Polyline3D.map (Pnt.moveZ distance)

    /// Applies a 4x4 transformation matrix
    static member inline transform (m:Matrix) (pl:Polyline3D) = pl |> Polyline3D.map (Pnt.transform m)     
    
    /// Rotation a Polyline3D around Z-Axis.
    static member inline rotate (r:Rotation2D) (pl:Polyline3D) = pl |> Polyline3D.map (Pnt.rotateZBy r) 
    
    /// Rotation a Polyline3D round given Center point an a local Z Axis.
    static member inline rotateOn (cen:Pnt) (r:Rotation2D) (pl:Polyline3D) = pl |> Polyline3D.map (Pnt.rotateZonCenterBy cen r) 

    /// Returns the parameter on the Polyline3D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline3D starts at 0.0 and ends at point count. 
    static member inline closestParameter (pl:Polyline3D) (pt:Pnt) = pl.ClosestParameter pt

    /// Returns the point on the Polyline3D that is the closest point to the given point.
    static member inline closestPoint (pl:Polyline3D) (pt:Pnt) = pl.ClosestPoint pt

    /// Returns the Distance of the test point to the closest point on the Polyline3D.
    static member inline distanceTo (pl:Polyline3D) (pt:Pnt) = pl.DistanceTo pt

    /// Create a new Polyline3D by copying over all points.
    static member create(points: seq<Pnt>) = Polyline3D(ResizeArray(points))
    
    /// Create a new Polyline3D by using the provided ResizeArray directly.
    /// All later changes to the ResizeArray will be reflected in the Polyline3D.
    static member createDirectlyUnsafe (points: ResizeArray<Pnt>) = Polyline3D( points )
    
    /// Create a new empty Polyline3D without any points. 
    /// But predefined capacity
    static member empty (capacity:int) = Polyline3D(ResizeArray(capacity))
    
    /// Returns new Polyline3D from point at Parameter a to point at Parameter b.
    /// if 'a' is bigger 'b' then the new Polyline3D is in opposite direction.
    /// If a parameter is within 1e-5 of an integer value, the integer value is used as parameter.
    static member segment a b (pl:Polyline3D) = 
        let rev = a>b
        let u,v = if rev then b,a else a,b
        let np = Polyline3D.empty (int(v-u)+2)
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
    
    /// <summary>Offsets a Polyline in 3D space by finding the local offset in each corner.</summary>
    /// <param name="polyLine"> A 3D polyLine. Auto detects if given points are from a closed Polyline (first point = last point) and loops them.</param>
    /// <param name="offsetDistances">Offset distances can vary per segment, Positive distance is offset inwards, negative outwards.
    ///     Distances Sequence  must have exact count , be a singleton ( for repeating) or empty seq ( for ignoring)</param>
    /// <param name="normalDistances">Normal distances define a perpendicular offset at each corner.
    ///     Distances Sequence  must have exact count , be a singleton ( for repeating) or empty seq ( for ignoring)</param>
    /// <param name="loop">Consider last point and first point to be from a closed loop, even if they are not at the same location.</param>
    /// <returns>A list of points that has the same length as the input list.</returns>
    static member offset(   polyLine:Polyline3D,  // or IList so it can take a Point3dList class too
                            offsetDistances: float seq,
                            [<OPT;DEF(null:seq<float>)>] normalDistances: float seq,
                            [<OPT;DEF(false)>] loop:bool) :Polyline3D  = 
        let points = polyLine.Points
        let offDists0  = Array.ofSeq offsetDistances
        let normDists0 = Array.ofSeq (if isNull normalDistances then Seq.empty<float> else normalDistances)
        let pointk = points.Count
        let lastIndex = pointk - 1
        let lenDist = offDists0.Length
        let lenDistNorm = normDists0.Length
        if pointk < 2 then
            FsExGeoException.Raise "FsEx.Geo.Polyline.OffsetPoints needs at least two points but %O given" polyLine
        elif pointk = 2 then
            let offDist = 
                if   lenDist = 0 then 0.0
                elif lenDist = 1 then offDists0.[0]
                else FsExGeoException.Raise "FsEx.Geo.Polyline.OffsetPoints: offsetDistances has %d items but should have 1 or 0 for 2 given points %O" lenDist polyLine 
            let normDist = 
                if   lenDistNorm = 0 then 0.0
                elif lenDistNorm = 1 then normDists0.[0]
                else FsExGeoException.Raise "FsEx.Geo.Polyline.OffsetPoints: normalDistances has %d items but should have 1 or 0 for 2 given points %O" lenDistNorm polyLine
            let a, b = Pnt.offsetTwoPt(points.[0], points.[1] , offDist, normDist)
            let r = Polyline3D.empty 2
            r.Points.Add(a)
            r.Points.Add(b)
            r
        else // regular case more than 2 points
            let lastIsFirst = (points.[0] - points.Last).Length < 1e-6 //auto detect closed polyline points
            let distsNeeded = 
                if lastIsFirst then pointk - 1
                elif loop      then pointk
                else                pointk - 1
            let distsNeededNorm = 
                if lastIsFirst then pointk - 1
                elif loop      then pointk
                else                pointk   // not -1 !!
            let  offDists = 
                if   lenDist = 0 then             Array.create distsNeeded 0.0
                elif lenDist = 1 then             Array.create distsNeeded offDists0.[0]
                elif lenDist = distsNeeded then   offDists0
                else FsExGeoException.Raise "OffsetPoints: offsetDistances has %d items but should have %d (lastIsFirst=%b) (loop=%b)" lenDist distsNeeded lastIsFirst loop
            let normDists = 
                if   lenDistNorm = 0 then                 Array.create distsNeededNorm 0.0
                elif lenDistNorm = 1 then                 Array.create distsNeededNorm normDists0.[0]
                elif lenDistNorm = distsNeededNorm then   normDists0
                else FsExGeoException.Raise "FsEx.Geo.Polyline.OffsetPoints: normalDistances has %d items but should have %d (lastIsFirst=%b) (loop=%b)" lenDist distsNeededNorm lastIsFirst loop
            let refNormal = Points.normalOfPoints(points) //to have good starting direction, first kink might be in bad direction
            let Pts = ResizeArray<Pnt>(pointk)
            let Ns = ResizeArray<Vec>(pointk)
            for i, p, t, n in Seq.iPrevThisNext(points) do
                // first one:
                if i=0 then
                    if lastIsFirst then
                        let prev = points.GetNeg(-2) // because -1 is same as 0
                        let struct( _, _, pt, N) = Points.findOffsetCorner(prev, t, n, offDists.Last, offDists.[0], refNormal)
                        Pts.Add pt
                        Ns.Add N
                    else
                        let struct( _, sn, pt, N) = Points.findOffsetCorner(p, t, n, offDists.Last, offDists.[0], refNormal)
                        Ns.Add N
                        if loop then Pts.Add pt
                        else         Pts.Add (t + sn)
                // last one:
                elif i = lastIndex  then
                    if lastIsFirst then
                        let struct(_, _, pt, N) = Points.findOffsetCorner(p, t, points.[1], offDists.[i-1], offDists.[0], refNormal)
                        Pts.Add pt
                        Ns.Add N
                    elif loop then
                        let struct( _, _, pt, N) = Points.findOffsetCorner(p, t, n, offDists.[i-1], offDists.[i], refNormal)
                        Pts.Add pt
                        Ns.Add N
                    else
                        let struct( sp, _, _, N) = Points.findOffsetCorner(p, t, n, offDists.[i-1], offDists.[i-1], refNormal) // or any next off dist since only sp is used
                        Pts.Add (t + sp)
                        Ns.Add N
                else
                    let struct( _, _, pt, N ) = Points.findOffsetCorner(p, t, n, offDists.[i-1], offDists.[i], refNormal)
                    Pts.Add pt
                    Ns.Add N
            if lenDistNorm > 0 then
                for i=0 to  distsNeededNorm-1 do // ns might be shorter than pts if lastIsFirst= true
                    let n = Ns.[i]
                    if n <> Vec.Zero then
                        Pts.[i] <- Pts.[i] + n * normDists.[i]

            let rec searchBack i (ns:ResizeArray<Vec>) = 
                let ii = saveIdx (i) ns.Count
                let v = ns.[ii]
                if v <> Vec.Zero || i < -ns.Count then ii
                else searchBack (i-1) ns

            let rec  searchForward i (ns:ResizeArray<Vec>) = 
                let ii = saveIdx (i) ns.Count
                let v = ns.[ii]
                if v <> Vec.Zero || i > (2 * ns.Count) then ii
                else searchForward (i + 1) ns

            // fix collinear segments by nearest neighbors that are ok
            for i, n in Seq.indexed Ns do // ns might be shorter than pts if lastIsFirst= true
                if n = Vec.Zero then
                    let pi = searchBack (i-1) Ns
                    let ppt = Pts.[pi]
                    let pln = Line3D(points.[pi], points.[saveIdx (pi + 1) pointk])
                    let pclp = pln.ClosestPoint(ppt, limitToFiniteSegment=false)
                    let pv = ppt - pclp

                    let ni = searchForward (i + 1) Ns
                    let npt = Pts.[ni]
                    let nln = Line3D(points.[ni], points.[saveIdx (ni-1) pointk])
                    let nclp = nln.ClosestPoint(npt, limitToFiniteSegment=false)
                    let nv = npt - nclp
                    //print (pi,"prev i")
                    //print (i,"is collinear")
                    //print (ni,"next i")
                    if offDists.[pi] <> offDists.[saveIdx (ni-1) distsNeeded] then
                        FsExGeoException.Raise "FsEx.Geo.Polyline.OffsetPoints: can't fix collinear at index %d with index %d and %d because offset distances are mismatching: %f, %f" i pi ni offDists.[pi] offDists.[saveIdx (ni-1) pointk]
                    Pts.[i] <- points.[i] + (nv + pv)*0.5
            if lastIsFirst then Pts.[lastIndex] <- Pts.[0]
            Pts


    /// Offsets a Polyline in 3D space by finding th local offset in each corner.
    /// Positive distance is offset inwards, negative outwards.
    /// Normal distances define a perpendicular offset at each corner.
    /// Auto detects if given points are from a closed Polyline (first point = last point) and loops them
    /// Auto detects points from closed polylines and loops them
    static member OffsetPoints(     points:Point3d IList,
                                    offsetDistance: float,
                                    [<OPT;DEF(0.0)>]normalDistance: float ,
                                    [<OPT;DEF(false)>]loop:bool) :Point3d  Rarr  = 

        if normalDistance = 0.0 then Scripting.OffsetPoints(points,[offsetDistance],[]              , loop)
        else                         Scripting.OffsetPoints(points,[offsetDistance],[normalDistance], loop)



