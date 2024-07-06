namespace Euclid

open System
open UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open System.Collections.Generic // forIList

/// A mutable 2D Polyline.
/// If the last point is the same as the first point, the Polyline2D is closed.
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<DataContract>] // for using DataMember on fields
type Polyline2D =

    /// Gets the internal list of all Points of the Polyline2D.
    /// This is not a copy, so changes to the list will be reflected in the Polyline2D.
    [<DataMember>] val Points: ResizeArray<Pt>

    /// Internal constructor. Uses input List without copying it.
    internal new (points: ResizeArray<Pt>) = { Points = points }


    /// Nicely formatted string representation of the Box including its size.
    override pl.ToString() =
        if pl.Points.Count = 0 then
            "Euclid.Polyline2D with 0 points"
        else
            let f = pl.Points.First
            let l = pl.Points.Last
            sprintf"Euclid.Polyline2D with %d points from %s to %s" pl.Points.Count f.AsString l.AsString

    /// Creates a copy of the Polyline2D
    member inline p.Duplicate(): Polyline2D =
        let ps = p.Points
        Polyline2D.createDirectlyUnsafe(ps.GetRange(0, ps.Count))

    /// Gets first point of the Polyline2D
    member p.Start =
        if p.Points.Count < 2 then EuclidException.Raise "Euclid.Polyline2D.Start failed on Polyline2D with less than 2 points %O" p
        p.Points.First

    /// Gets last or end point of the Polyline2D
    member p.End =
        if p.Points.Count < 2 then EuclidException.Raise "Euclid.Polyline2D.Start failed on Polyline2D with less than 2 points %O" p
        p.Points.Last

    /// Gets the count of points in the Polyline2D
    member inline p.PointCount = p.Points.Count

        /// Gets the length of the Polyline2D
    member p.Length =
        let ps = p.Points
        if ps.Count < 2 then EuclidException.Raise "Euclid.Polyline3D.Length failed on Polyline2D with less than 2 points %O" p
        let mutable l = 0.0
        let mutable prev = ps.[0]
        for i = 1 to ps.Count-1 do
            let t = ps.[i]
            l <- l + Pt.distance prev t
            prev <- t
        l

    /// Gets bounding rectangle of the Polyline2D
    member p.BoundingRectangle = BRect.createFromIList p.Points

    /// Tests if Polyline2D start and end points are exactly the same.
    member inline p.IsClosed =
        if p.Points.Count < 2 then EuclidException.Raise "Euclid.Polyline3D.IsClosed failed on Polyline2D with less than 2 points %O" p
        let v = p.Start  - p.End
        v.IsZero

    /// Tests if Polyline2D is closed within given tolerance.
    member p.IsAlmostClosed(tolerance) =
        if p.Points.Count < 2 then EuclidException.Raise "Euclid.Polyline2D.IsAlmostClosed failed on Polyline2D with less than 2 points %O" p
        let v = p.Start  - p.End
        v.LengthSq < tolerance*tolerance

    /// Reverse order of the Polyline2D in place.
    member p.ReverseInPlace() =
        p.Points.Reverse()

    /// Returns new Polyline2D in reversed Order.
    member p.Reverse () =
        let n = p.Duplicate()
        n.Points.Reverse()
        n

    /// Close the Polyline2D if it is not already closed.
    /// If the ends are closer than the tolerance. The last point is set to equal the first point.
    /// Else the start point is added to the end of the Polyline2D.
    member p.CloseIfOpen(toleranceForAddingPoint) =
        if p.Points.Count < 2 then EuclidException.Raise "Euclid.Polyline2D.CloseIfOpen failed on Polyline2D with less than 2 points %O" p
        let v = p.Start  - p.End
        if v.LengthSq < toleranceForAddingPoint*toleranceForAddingPoint then
            p.Points.Last <- p.Start
        else
            p.Points.Add p.Start

    /// The signed area of the Polyline2D .
    /// If it is positive the Polyline2D is Counter Clockwise.
    /// Polyline does not need to be exactly closed. But then result might be wrong. Or without meaning.
    /// For self intersecting Polylines the result is also invalid.
    member p.SignedArea =
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
        area

    /// The area of the Polyline2D.
    /// Fails if Polyline is not exactly closed.
    /// For self intersecting Polylines the result is invalid.
    member p.Area =
        if not p.IsClosed then EuclidException.Raise "Euclid.Polyline2D.Area failed on Polyline2D that is not exactly closed %O" p
        abs(p.SignedArea)

    /// Test if Polyline2D is CounterClockwise.
    /// The Polyline2D does not need to be actually closed.
    /// The signed area of the Polyline2D is calculated.
    /// If it is positive the Polyline2D is Counter Clockwise.
    member p.IsCounterClockwise =
        let  area = p.SignedArea
        if   abs(area) < UtilEuclid.zeroLengthTolerance then EuclidException.Raise "Euclid.Polyline2D.IsCounterClockwiseIn2D: Polyline2D the area is zero: %O" p
        else area > 0.0



    /// Returns the point at a given parameter on the Polyline2D.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at points.Count - 1.0 .
    /// If the parameter is within 1e-5 of an integer value, the integer value is used as parameter.
    member pl.EvaluateAt(t:float) =
        let i = int t
        let p = t - float i
        if   i < -1 then
            EuclidException.Raise "Euclid.Polyline2D.EvaluateAt: Parameter %f is less than 0.0" t
        elif i > pl.Points.Count then
            EuclidException.Raise "Euclid.Polyline2D.EvaluateAt: Parameter %f is more than than point count(%d)." t pl.Points.Count
        elif i = -1 then
            if p > 0.99999 then pl.Points.First
            else EuclidException.Raise "Euclid.Polyline2D.EvaluateAt: Parameter %f is less than 0.0" t
        elif i = pl.Points.Count then
            if   p > 1e-5 then  EuclidException.Raise "Euclid.Polyline2D.EvaluateAt: Parameter %f is more than than point count(%d)." t pl.Points.Count
            else pl.Points.Last
        // return point  if point is almost matching
        elif isTooTiny (p) then
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
            EuclidException.Raise "Euclid.Polyline2D.EvaluateAt: Parameter %f is less than 0.0" t
        elif i > pl.Points.Count then
            EuclidException.Raise "Euclid.Polyline2D.EvaluateAt: Parameter %f is more than than point count(%d)." t pl.Points.Count
        elif i = -1 then
            if p > 0.9999 then UnitVc.create(pl.Points.First, pl.Points.Second)
            else EuclidException.Raise "Euclid.Polyline2D.EvaluateAt: Parameter %f is less than 0.0" t
        elif i = pl.Points.Count then
            if   p > 1e-4 then  EuclidException.Raise "Euclid.Polyline2D.EvaluateAt: Parameter %f is more than than point count(%d)." t pl.Points.Count
            else UnitVc.create(pl.Points.SecondLast, pl.Points.Last)
        // return point  if point is almost matching
        else
            UnitVc.create(pl.Points.[i], pl.Points.[i+1])



    /// Returns the parameter on the Polyline2D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at points.Count - 1.0 .
    member pl.ClosestParameter(pt:Pt) =
        // for very large polylines, this is could be optimized by using search R-tree
        let ps = pl.Points
        if ps.Count < 2 then EuclidException.Raise "Euclid.Polyline2D.ClosestParameter failed on  Polyline2D with less than 2 points %O" pl
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
            ts[i] <- if len < 1e-9 then 0.0 else -((p-pt) *** v) / len |> UtilEuclid.clampBetweenZeroAndOne //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html

        // square distances per segment
        let ds = Array.zeroCreate (ps.Count-1)
        for i = 0 to ds.Length-1 do
            let p = ps[i]
            let v = vs[i]
            let t = ts[i]
            ds[i] <- Pt.distanceSq pt (p + v*t)

        let i = Arr.minIndex ds
        let t = ts.[i]
        float i + t

    /// Returns the point on the Polyline2D that is the closest point to the given point.
    member pl.ClosestPoint(pt:Pt) =
        let t = pl.ClosestParameter pt
        pl.EvaluateAt t

    /// Returns the distance of the test point to the closest point on the Polyline2D.
    member pl.DistanceTo(pt:Pt) =
        let t = pl.ClosestParameter pt
        pl.EvaluateAt t
        |> Pt.distance pt


    //-------------------------------------------------------------------
    //------------------------static members-----------------------------
    //-------------------------------------------------------------------

    /// Gets the internal list of all Points of the Polyline2D.
    /// This is not a copy, so changes to the list will be reflected in the Polyline2D.
    static member inline pointsUnsafeInternal (p:Polyline2D) = p.Points

    /// Gets first point of the Polyline2D
    static member inline start (p:Polyline2D) =
        if p.Points.Count < 2 then EuclidException.Raise "Euclid.Polyline2D.Start failed on Polyline2D with less than 2 points %O" p
        p.Points.First

    /// Gets last or end point of the Polyline2D
    static member inline ende (p:Polyline2D) =
        if p.Points.Count < 2 then EuclidException.Raise "Euclid.Polyline2D.Start failed on Polyline2D with less than 2 points %O" p
        p.Points.Last

    /// Reverse order of the Polyline2D in place.
    static member inline reverseInPlace (p:Polyline2D) = p.ReverseInPlace()

    /// Returns new Polyline2D in reversed Order.
    static member inline reverse (p:Polyline2D) = p.Reverse()

    /// Returns the point at a given parameter on the Polyline2D.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at point count.
    static member inline evaluateAt (t:float) (pl:Polyline2D) = pl.EvaluateAt t

    /// Apply a mapping function to each point in the 2D Polyline2D. Returns new Polyline2D.
    static member map (mapping:Pt->Pt) (pl:Polyline2D) = pl.Points |> ResizeArr.map mapping |> Polyline2D.createDirectlyUnsafe

    /// Move a Polyline2D by a vector. (same as Polyline2D.move)
    static member inline translate (v:Vc) (pl:Polyline2D) = pl |> Polyline2D.map (Pt.addVc v)

    /// Move a Polyline2D by a vector. (same as Polyline2D.translate)
    static member inline move (v:Vc) (pl:Polyline2D) = Polyline2D.translate v pl

    /// Returns a Polyline2D moved by a given distance in X direction.
    static member inline moveX (distance:float) (pl:Polyline2D) = pl |> Polyline2D.map (Pt.moveX distance)

    /// Returns a Polyline2D moved by a given distance in Y direction.
    static member inline moveY (distance:double) (pl:Polyline2D) = pl |> Polyline2D.map (Pt.moveY distance)


    /// Rotation a Polyline2D around Z-Axis.
    static member inline rotate (r:Rotation2D) (pl:Polyline2D) = pl |> Polyline2D.map (Pt.rotateBy r)

    /// Rotation a Polyline2D round given Center point an a local Z-axis.
    static member inline rotateWithCenter (cen:Pt) (r:Rotation2D) (pl:Polyline2D) = pl |> Polyline2D.map (Pt.rotateWithCenterBy cen r)

    /// Returns the parameter on the Polyline2D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at point count.
    static member inline closestParameter (pl:Polyline2D) (pt:Pt) = pl.ClosestParameter pt

    /// Returns the point on the Polyline2D that is the closest point to the given point.
    static member inline closestPoint (pl:Polyline2D) (pt:Pt) = pl.ClosestPoint pt

    /// Returns the distance of the test point to the closest point on the Polyline2D.
    static member inline distanceTo (pl:Polyline2D) (pt:Pt) = pl.DistanceTo pt

    /// Create a new Polyline2D by copying over all points.
    static member create(points: seq<Pt>) = Polyline2D(ResizeArray(points))

    /// Create a new Polyline2D by using the provided ResizeArray directly.
    /// All later changes to the ResizeArray will be reflected in the Polyline2D.
    static member createDirectlyUnsafe (points: ResizeArray<Pt>) = Polyline2D(points)

    /// Create a new empty Polyline2D without any points.
    /// But predefined capacity.
    static member createEmpty (capacity:int) = Polyline2D(ResizeArray(capacity))

    /// Returns new Polyline2D from point at Parameter a to point at Parameter b.
    /// if 'a' is bigger 'b' then the new Polyline2D is in opposite direction.
    /// If a parameter is within 1e-5 of an integer value, the integer value is used as parameter.
    static member segment a b (pl:Polyline2D) =
        let rev = a>b
        let u, v = if rev then b, a else a, b
        let np = Polyline2D.createEmpty (int(v-u)+2)
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


    //--------------------------------------------------------------------------------
    //------------------------Offset------------------------------------
    //--------------------------------------------------------------------------------

    /// Returns the index of an outer corner and
    /// a cross product as normal vector corresponding to an Counter-Clockwise view on the loop.
    /// In 2D the cross product is the signed area of the parallelogram spanned by the two vectors. So just a scalar. Not a vector
    /// This is used to calculate the RefNormal vector for the offset function.
    /// The input vectors are the vectors for each segment of the polyline.
    /// From first and second point up to last and first point.
    /// The endIndexChange is to skip the last point if first and last are the same. ( then use -1)
    static member internal findOuterCornerAndRefNormal(pts:ResizeArray<Pt>, vs:Vc[], endIndexChange) : int*float =
        if pts.Count + endIndexChange <> vs.Length then EuclidException.Raise "Euclid.Polyline2D.findOuterCornerAndRefNormal pts (%d) and vs(%d) must have the same length." pts.Count vs.Length
        let us = Array.zeroCreate vs.Length
        // mark very short segments with 0, 0, 0:
        for i=0 to vs.Length-1 do
            let v = vs.[i]
            let l = v.Length
            if l > 1e-6 then
                let f = 1. / l
                us.[i] <- UnitVc.createUnchecked(v.X*f, v.Y*f)
            //else  us.[i] <- Vc.Zero //happens anyway by zeroCreate

        let ref = Points.getSignedArea(pts)
        let mutable posAngSum = 0.0
        let mutable negAngSum = 0.0
        let mutable posIdx = -1
        let mutable negIdx = -1
        let inline isNotZero (v:UnitVc) = v.X<>0. || v.Y<>0.

        // get angle sums
        let prevUIdx = us|> Array.findIndexBack isNotZero
        let mutable prevU = us.[prevUIdx]
        let len = us.Length
        let deg2 = Math.PI / 90. // 2 degree
        let rec loop(i) =
            if i < len then
                let thisU = us.[i]
                if isNotZero thisU then
                    let ang = UnitVc.anglePi thisU prevU
                    if ang < deg2 then // parallel point, loop on
                        loop(i+1)
                    else
                        let c = UnitVc.cross(prevU, thisU)
                        if c*ref>0.0 then
                            posAngSum <- posAngSum + ang
                            if posIdx= -1 then posIdx <- i
                        else
                            negAngSum <- negAngSum + ang
                            if negIdx= -1 then negIdx <- i
                        prevU <- thisU
                        loop(i+1)
                else // Duplicate point, loop on
                    loop(i+1)
        loop(0)

        if posAngSum > negAngSum then
            posIdx, ref
        else
            negIdx, -ref


    /// The inner core routine of Points.offset. This function considers input a closed polyline.
    /// The offset distances are given for each segment of the polyline. offDi:IList must have the same length as the polyline or be a singleton
    /// Start point and end point may not be equal, all arrays of same length.
    /// The 'referenceOrient' corresponds to the cross product of two segment of the polyline an an outside corner.
    /// If 'referenceOrient' is 0.0 then the algorithm tries to detect if the loop is clockwise or counterclockwise
    /// it also tries to find an outer corner on non convex polylines
    /// If 'referenceOrient' is bigger than 0.0 a counter-clock-wise loop is assumed and the detection part is skipped.
    /// If 'referenceOrient' is smaller than 0.0 a clock-wise loop is assumed and the detection part is skipped.
    /// The endIndexChange is to skip the last point if first and last are the same. ( then use -1)
    static member internal offsetCore(pts:ResizeArray<Pt>, offDi:IList<float>,  referenceOrient:float, fixColinearLooped:bool, allowObliqueOffsetOnColinearSegments, endIndexChange) : ResizeArray<Pt> =
        let offCount = offDi.Count
        if  offCount <> 1
        &&  offCount <> pts.Count + endIndexChange //case: the last point if first and last are the same.
        &&  offCount <> pts.Count - 1  then //case: open polyline and, no looping desired:
            EuclidException.Raise "Euclid.Polyline2D.offsetCore pts(%d) and offD(%d) must have the same length or offD must be of length 1." pts.Count offDi.Count

        let lenTolSq = 1e-12 //local squared length tolerance
        let lenPts = pts.Count + endIndexChange
        let lastIdx = lenPts - 1


        // to allow a single distance to be used everywhere
        let inline getOffDist i =
            if offCount = 1 then offDi[0]
            elif offCount = i then 0.0  //case: open polyline and, no looping desired:
            else offDi[i]

        // (1) collect array of vectors going from this to next
        let vs = Array.zeroCreate lenPts
        let mutable this = pts.[0]
        for i=1 to lastIdx do
            let next = pts.[i]
            vs.[i-1] <- next-this
            this <- next
        vs.[lastIdx] <- pts.[0]-pts[lastIdx]

        let refNorm = if referenceOrient = 0.0 then Polyline2D.findOuterCornerAndRefNormal(pts, vs, endIndexChange) |> snd else referenceOrient

        // (2) main loop
        let colinear: bool[] = Array.zeroCreate lenPts
        let res = pts.GetRange(0, lenPts) // copy
        //(2.1) find last valid vector
        let prevVIdx =
            vs
            |> Array.tryFindIndexBack (fun v -> v.LengthSq > lenTolSq)
            |> Option.defaultWith (fun _ -> EuclidException.Raise "Euclid.Polyline2D.offsetCore: invalid Polyline2D, all points are in the same location" )

        for i=prevVIdx+1 to lastIdx do
            colinear.[i]<-true
        let mutable prevV = vs.[prevVIdx]
        let mutable prevOff = getOffDist(prevVIdx)

        //(2.2) looping
        let rec loop(i) =
            if i <= lastIdx then
                let nextV = vs.[i]
                let ax = prevV.X
                let ay = prevV.Y
                let bx = nextV.X
                let by = nextV.Y
                let a = ax*ax + ay*ay // square length of prevV
                let c = bx*bx + by*by // square length of nextV
                if c < lenTolSq then
                    colinear.[i] <- true
                    loop(i+1)
                //elif a < lenTolSq then
                //    failwithf "To short segment not recognized. This should already be checked!"
                else
                    let b = ax*bx + ay*by  // dot product of both lines
                    let ac = a*c // square of square length, never negative
                    let bb = b*b // square of square dot product, never negative
                    let discriminant = ac - bb // never negative, the dot product cannot be bigger than the two square length multiplied with each other
                    let div = ac+bb // never negative
                    let rel = discriminant/div
                    if rel < float RelAngleDiscriminant.``0.25`` then //parallel
                        colinear.[i] <- true
                        loop(i+1)
                    else
                        /// Check with the orientation with the reference normal
                        /// In 2D the cross product vector is the signed area of the parallelogram spanned by the two vectors.
                        /// So just a scalar. Not actually a vector
                        let n = Vc.cross(prevV, nextV) |> matchSign refNorm

                        let thisOff = getOffDist(i)
                        let thisPt = pts.[i]
                        let prevShift = prevV.Rotate90CCW |> Vc.withLength (if n>0. then prevOff else -prevOff)
                        let nextShift = nextV.Rotate90CCW |> Vc.withLength (if n>0. then thisOff else -thisOff)
                        let offP = thisPt + prevShift
                        let offN = thisPt + nextShift
                        let vx = offN.X - offP.X
                        let vy = offN.Y - offP.Y
                        let e = bx*vx + by*vy
                        let d = ax*vx + ay*vy
                        let t = (c * d - b * e) / discriminant
                        res.[i] <- offP + t * prevV
                        prevOff <- thisOff
                        prevV <- nextV
                        loop(i+1)
        loop(0)

        // (3.5) correct colinear points by nearest neighbors that are ok in a loop
        if fixColinearLooped then
            let rec searchBack i =
                let ii = saveIdx (i) colinear.Length
                if not colinear.[ii] then ii
                elif i < -colinear.Length then EuclidException.Raise "Euclid.Polyline2D.offsetCore : all %d points for offset are colinear within 0.25 degree or identical. " pts.Count
                else searchBack (i - 1)

            let rec  searchForward i =
                let ii = saveIdx (i) colinear.Length
                if not colinear.[ii] then ii // no need to check for endless loop here because check is done in searchBack that is called first
                else searchForward (i + 1)

            for i = 0 to colinear.Length-1 do
                if colinear.[i]  then
                    let pi = searchBack    (i - 1)
                    let ni = searchForward (i + 1)
                    if pi = ni then //  does this ever happen ? it is either all colinear (caught in searchBack) or at least three points are not colinear ??
                        EuclidException.Raise "Euclid.Polyline2D.offsetCore : all %d points for offset are colinear within 0.25 degree or identical. " pts.Count
                    let ln = Line2D(res.[pi], res.[ni])
                    res.[i] <- ln.ClosestPointInfinite(pts.[i])
                    // TODO add safety check? could be omitted. offset is then averaged out.
                    if not allowObliqueOffsetOnColinearSegments && abs (getOffDist(pi) - getOffDist(ni)) > 1e-9 then
                        EuclidException.Raise "Euclid.Polyline2D.offsetCore: can't offset collinear segment at index %d with different offset distances from index %d and %d\r\n these distances are not the same: %f and %f" i pi ni (getOffDist(pi)) (getOffDist(ni))
        else
            let rec searchBack i =
                if i<0 then -1
                elif not colinear.[i] then i
                else searchBack (i - 1)

            let rec  searchForward i =
                if i = colinear.Length  then -1
                elif not colinear.[i] then i
                else searchForward (i + 1)

            for i = 0 to colinear.Length-1 do
                if colinear.[i]  then
                    let pi = searchBack    (i-1)
                    let ni = searchForward (i+1)
                    if pi = ni then //  does this ever happen ? it is either all colinear (caught in searchBack) or at least three points are not colinear ??
                        EuclidException.Raise "Euclid.Polyline2D.offsetCore : all %d points for offset are colinear within 0.25 degree or identical. " pts.Count
                    elif pi = -1 then
                        if ni = -1 then
                            EuclidException.Raise "Euclid.Polyline2D.offsetCore : all %d points for offset are colinear within 0.25 degree or identical. " pts.Count
                        else // colinear start, get frame
                            match Points.offsetInCornerEx2D (pts[ni-1], pts[ni], pts[ni+1],  getOffDist(ni-1),  getOffDist(ni), refNorm) with
                            |ValueNone -> EuclidException.Raise "Euclid.Polyline2D.offsetCore :offsetInCornerEx-1 failed unexpectedly."
                            |ValueSome (x, prevShift, _) -> res.[i] <- pts.[i] + prevShift
                    elif ni = -1 then  // colinear end, get frame
                        match Points.offsetInCornerEx2D (pts[pi-1], pts[pi], pts[pi+1],  getOffDist(pi-1),  getOffDist(pi), refNorm) with
                        |ValueNone -> EuclidException.Raise "Euclid.Polyline2D.offsetCore :offsetInCornerEx-1 failed unexpectedly."
                        |ValueSome (x, _, nextShift) -> res.[i] <- pts.[i] + nextShift
                    else
                        let ln = Line2D(res.[pi], res.[ni])
                        res.[i] <- ln.ClosestPointInfinite(pts.[i])
                        // TODO add safety check? could be omitted. offset is then averaged out.
                        if not allowObliqueOffsetOnColinearSegments && abs (getOffDist(pi) - getOffDist(ni)) > 1e-9 then
                            EuclidException.Raise "Euclid.Polyline2D.offsetCore: can't offset collinear segment at index %d with different offset distances from index %d and %d\r\n these distances are not the same: %f and %f" i pi ni (getOffDist(pi)) (getOffDist(ni))
        res


    /// <summary>Offsets a Polyline in 2D space by finding the local offset in each corner.
    /// Auto detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// Does not fail on colinear or duplicate points.</summary>
    /// <param name="polyLine"> A 2D Polyline. </param>
    /// <param name="offsetDistances">The parallel offset distances for each segment of the polyline.
    /// A positive distance offsets inwards in corners, a negative offset outwards.
    /// For open and closed polylines this list of distances must have one item less than number of points in the polyline.
    /// Except if the polyline is open and the loop parameter is set to true. Then points and distances list shall have the same count.
    /// A empty list for no offset or singleton for constant offset is allowed too.
    /// </param>
    /// <param name="loop">bool, Optional. Consider last point and first point to be from a closed loop, even if they are not at the same location.</param>
    /// <param name="referenceOrient">float, Optional.
    /// The 'referenceOrient' corresponds to the cross product of two segment of the polyline an an outside corner.
    /// If 'referenceOrient' is 0.0 or omitted then the algorithm tries to detect if the loop is clockwise or counterclockwise.
    /// it also tries to find an outer corner on non convex polylines.
    /// If 'referenceOrient' is bigger than 0.0 a counterclockwise loops is assumed and the detection part is skipped.
    /// If 'referenceOrient' is smaller than 0.0 a clockwise loops is assumed and the detection part is skipped.
    /// If a given referenceOrient does not correspond to the looping order the offset will be to the other side.</param>
    /// <param name="obliqueOffsets">bool, Optional. When two adjacent segments are colinear but have different offset distances there is no solution with a parallel offset.
    /// By default an exception is raised. Set this to true to create and averaged oblique offset instead of failing.</param>
    /// <returns>A new 2D polyline with the same amount of points.</returns>
    static member offset(   polyLine:Polyline2D,
                            offsetDistances: float IList,
                            [<OPT;DEF(false)>] loop:bool,
                            [<OPT;DEF(0.0)>]   referenceOrient:float,
                            [<OPT;DEF(false)>] obliqueOffsets:bool
                            ) : Polyline2D =
        let points = polyLine.Points
        // (1) Fail if polyline has less than one point
        if points.Count < 2 then
            EuclidException.Raise "Euclid.Polyline2D.offset needs at least two points but %O given." polyLine


        let checkDistanceCount len  =
            if isNull offsetDistances then Error("offsetDistances is null")
            elif offsetDistances.Count = len then Ok()
            elif offsetDistances.Count = 1   then Ok()
            else Error( $"offsetDistances has {offsetDistances.Count} items" )


        // (2) check if last and first point are the same and if so remove last point (one point needs to be removed from list)
        if Pt.distanceSq polyLine.Start polyLine.End < 1e-12 then // sqrt of 1e-6, auto detect closed polyline points  then
            match checkDistanceCount (points.Count - 1) with
            |Error k  -> EuclidException.Raise "Euclid.Polyline2D.offset: %s but should have 1 or %d for %d given points. \r\nIn closed Polyline2D with identical start and end points:\r\n%O" k (points.Count-1) points.Count polyLine |Ok _ -> ()
            // use endIndexChange = -1 to skip the last point, wil then be re added at the end.
            let res = Polyline2D.offsetCore(points, offsetDistances, referenceOrient, fixColinearLooped=true, allowObliqueOffsetOnColinearSegments=obliqueOffsets, endIndexChange = -1)
            res.Add(res.[0])  // set last equal first, it was skipped with 'endIndexChange = -1'
            Polyline2D.createDirectlyUnsafe res

        // (3) check if open but looping desired
        elif loop then
            match checkDistanceCount points.Count with
            |Error k  -> EuclidException.Raise "Euclid.Polyline2D.offset: %s but should have 1 or %d for %d given points. \r\nIn open Polyline2D with start and end apart and loop set to TRUE :\r\n%O" k points.Count points.Count polyLine  |Ok _ -> ()
            Polyline2D.offsetCore(points, offsetDistances, referenceOrient, fixColinearLooped=true, allowObliqueOffsetOnColinearSegments=obliqueOffsets, endIndexChange = 0)
            |> Polyline2D.createDirectlyUnsafe

        // (4) open polyline and, no looping desired: (one distance value will be added, in getOffDist in offsetCore, )
        else
            match checkDistanceCount (points.Count-1) with
            |Error k  -> EuclidException.Raise "Euclid.Polyline2D.offset: %s but should have 1 or %d for %d given points. \r\nIn open Polyline2D with start and end apart and loop set to FALSE :\r\n%O" k points.Count points.Count polyLine  |Ok _ -> ()
            let res = Polyline2D.offsetCore(points, offsetDistances, referenceOrient, fixColinearLooped=false, allowObliqueOffsetOnColinearSegments=obliqueOffsets, endIndexChange = 0)

            // (4.1) fix ends if not looped
            if not loop then
                let firstLn = Line2D(points.[0], points.[1])
                let firstV = res.[0]  - (firstLn.ClosestPointInfinite res.[0] )
                res.[0]  <- points.[0] + firstV
                let lastIdx = res.Count - 1
                let lastLn = Line2D(points.[lastIdx-1], points.[lastIdx])
                let lastV = res.[lastIdx]  - (lastLn.ClosestPointInfinite res.[lastIdx] )
                res.[lastIdx]  <- points.[lastIdx] + lastV

            Polyline2D.createDirectlyUnsafe res

    /// <summary>Offsets a Polyline in 2D space by finding the local offset in each corner.
    /// Auto detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// Does not fail on colinear or duplicate points.</summary>
    /// <param name="polyLine"> A 2D Polyline. </param>
    /// <param name="offsetDistance">The offset distance for all segments of the polyline.  A positive distance offsets inwards in corners, a negative offset outwards.</param>
    /// <param name="loop">Consider last point and first point to be from a closed loop, even if they are not at the same location.</param>
    /// <param name="referenceOrient">float, Optional.
    /// The 'referenceOrient' corresponds to the cross product of two segment of the polyline an an outside corner.
    /// If 'referenceOrient' is 0.0 or omitted then the algorithm tries to detect if the loop is clockwise or counterclockwise.
    /// it also tries to find an outer corner on non convex polylines.
    /// If 'referenceOrient' is bigger than 0.0 a counterclockwise loops is assumed and the detection part is skipped.
    /// If 'referenceOrient' is smaller than 0.0 a clockwise loops is assumed and the detection part is skipped.
    /// If a given referenceOrient does not correspond to the looping order the offset will be to the other side.</param>
    /// <returns>A new 2D polyline with the same amount of points.</returns>
    static member offset(   polyLine:Polyline2D,
                            offsetDistance: float,
                            [<OPT;DEF(false)>]loop:bool,
                            [<OPT;DEF(0.0)>] referenceOrient:float) : Polyline2D =
        Polyline2D.offset(polyLine, [|offsetDistance|], loop, referenceOrient, obliqueOffsets=false)
