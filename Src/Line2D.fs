namespace FsEx.Geo

open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>]
open FsEx.Geo.Util
open FsEx.Geo.LineIntersectionTypes

/// An immutable finite line in 2D. Represented by a 2D start and 2D end point.
[<Struct;NoEquality;NoComparison>]// because its made up from floats
[<IsReadOnly>]
type Line2D =
    /// Returns the X coordinate of the start point of the line.
    val FromX:float

    /// Returns the Y coordinate of the start point of the line.
    val FromY:float

    /// Returns the X coordinate of the end point of the line.
    val ToX  :float

    /// Returns the Y coordinate of the end point of the line.
    val ToY  :float

    //Create Line2D from 2D start point and 2D end point.
    new (a:Pt,b:Pt) = {FromX=a.X; FromY=a.Y; ToX=b.X; ToY=b.Y}

    //Create Line2D from 2D start point's x and y  and 2D end point's x and y .
    new (a,b,u,v) = {FromX=a; FromY=b;  ToX=u; ToY=v}

    /// Returns the length of the line.
    member inline ln.Length =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        sqrt(x*x + y*y)

    /// Returns the square length of the line.
    member inline ln.LengthSq =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        x*x + y*y

    /// Format 2D Line into string including type name, X and Y for start and end points , and Length.
    /// Using nice floating point number formatting .
    override ln.ToString() =
        sprintf "FsEx.Geo.Line2D from X=%s| Y=%s to X=%s| Y=%s Length %s"
            (Format.float ln.FromX)
            (Format.float ln.FromY)
            (Format.float ln.ToX)
            (Format.float ln.ToY)
            (Format.float ln.Length)

    /// Format 2D Line into string from X and Y for start and end points.
    /// Using nice floating point number formatting .
    /// But without full type name as in v.ToString()
    member ln.AsString =
        sprintf "X=%s| Y=%s to X=%s| Y=%s"
            (Format.float ln.FromX)
            (Format.float ln.FromY)
            (Format.float ln.ToX)
            (Format.float ln.ToY)


    /// The Start point of the 2D Line2D,
    member inline ln.From = Pt(ln.FromX,ln.FromY)

    /// The End point of the 2D Line2D,
    member inline ln.To   = Pt(ln.ToX,ln.ToY)

    /// Same as ln.Vector or ln.Tangent.
    /// The returned vector has the same length as the Line2D.
    member inline ln.Direction =
        Vc(ln.ToX-ln.FromX,ln.ToY-ln.FromY)

    /// Same as ln.Tangent or ln.Direction.
    /// The returned vector has the same length as the Line2D.
    member inline ln.Vector =
        Vc(ln.ToX-ln.FromX,ln.ToY-ln.FromY)

    /// Same as ln.Vector or ln.Direction.
    /// The returned vector has the same length as the Line2D.
    member inline ln.Tangent =
        Vc(ln.ToX-ln.FromX,ln.ToY-ln.FromY)

    /// Returns a unit-vector of the line Direction.
    member inline ln.UnitTangent =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let l = sqrt(x * x  + y * y)
        if l < zeroLengthTol then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.UnitTangent: x:%g and y:%g are too small for creating a unit-vector. Tolerance:%g" x y zeroLengthTol
        UnitVc.createUnchecked (x/l, y/l)

    /// Checks if 2D line is parallel to the world X axis.
    /// Deviation tolerance is 1e-6.
    /// Fails on lines shorter than 1e-6.
    member inline ln.IsXAligned =
        let x = abs (ln.ToX-ln.FromX)
        let y = abs (ln.ToY-ln.FromY)
        if x+y < 1e-6 then FsExGeoException.Raise "FsEx.Geo.Line2D.IsXAligned cannot not check very tiny Line. (tolerance 1e-6)  %O" ln
        else y < 1e-6 

    /// Checks if 2D line is parallel to the world Y axis.
    /// Deviation tolerance is 1e-6.
    /// Fails on lines shorter than 1e-6.
    member inline ln.IsYAligned =
        let x = abs (ln.ToX-ln.FromX)
        let y = abs (ln.ToY-ln.FromY)
        if x+y < 1e-6 then FsExGeoException.Raise "FsEx.Geo.Line2D.IsYAligned cannot not check very tiny Line. (tolerance 1e-6)  %O" ln
        else x < 1e-6

    /// Check if the line has same starting and ending point.
    member inline ln.IsZeroLength =
        ln.ToX = ln.FromX &&
        ln.ToY = ln.FromY

    /// Check if line is shorter than tolerance.
    member inline ln.IsTiny tol =
        ln.Length < tol

    /// Check if line is shorter than squared tolerance.
    member inline ln.IsTinySq tol =
        ln.LengthSq < tol

    /// Evaluate line at a given parameter ( parameters 0.0 to 1.0 are on the line ),
    member inline ln.EvaluateAt (p:float) =
        Pt  ( ln.FromX + (ln.ToX-ln.FromX)*p
            , ln.FromY + (ln.ToY-ln.FromY)*p)        

    /// Returns the length of the line segment from the start point to the given parameter.
    /// This length is negative if the parameter is negative.
    member inline ln.LengthTillParam (p:float) =
        let x = (ln.ToX - ln.FromX)*p
        let y = (ln.ToY - ln.FromY)*p
        let l = sqrt(x*x + y*y)
        if p> 0.0 then l else -l

    /// Returns the length of the line segment from the given parameter till the line End.
    /// This length is negative if the parameter is bigger than 1.0.
    member inline ln.LengthFromParam (t:float) =
        let p = 1.0-t
        let x = (ln.ToX - ln.FromX)*p
        let y = (ln.ToY - ln.FromY)*p
        let l = sqrt(x*x + y*y)
        if p> 0.0 then l else -l    
        

    /// Returns the midpoint of the line,
    member inline ln.Mid =
        let x = (ln.ToX + ln.FromX)*0.5
        let y = (ln.ToY + ln.FromY)*0.5
        Pt(x, y)

    /// Returns the Line2D reversed.
    member inline ln.Reversed =
        Line2D(ln.ToX, ln.ToY, ln.FromX, ln.FromY)

    /// Returns the lines Bounding Rectangle.
    member inline ln.BoundingRect =
        BRect.create ( ln.From, ln.To)

    /// Returns a Line2D from point at Parameter a to point at Parameter b.
    member inline ln.Segment(a, b) =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        Line2D( ln.FromX + x*a,
                ln.FromY + y*a,
                ln.FromX + x*b,
                ln.FromY + y*b)


    /// Extend 2D line by absolute amount at start and end.
    /// Fails on lines shorter than 1e-12.
    member inline ln.Extend (distAtStart:float, distAtEnd:float)  =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let l = sqrt(x*x + y*y )
        if l < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.Extend %O to short for finding point at a distance." ln
        Line2D( ln.FromX - x*distAtStart/l,
                ln.FromY - y*distAtStart/l,
                ln.ToX   + x*distAtEnd/l,
                ln.ToY   + y*distAtEnd/l)

    /// Extend 2D line by absolute amount at start.
    /// Fails on lines shorter than 1e-12.
    member inline ln.ExtendStart (distAtStart:float)  =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let l = sqrt(x*x + y*y )
        if l < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.ExtendStart %O to short for finding point at a distance." ln
        Line2D( ln.FromX - x*distAtStart/l,
                ln.FromY - y*distAtStart/l,
                ln.ToX   ,
                ln.ToY   )

    /// Extend 2D line by absolute amount at end.
    /// Fails on lines shorter than 1e-12.
    member inline ln.ExtendEnd  (distAtEnd:float)  =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let l = sqrt(x*x + y*y )
        if l < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.ExtendEnd %O to short for finding point at a distance." ln
        Line2D( ln.FromX ,
                ln.FromY ,
                ln.ToX   + x*distAtEnd/l,
                ln.ToY   + y*distAtEnd/l )
    
    /// Shrink 2D line by absolute amount at start and end.
    /// Fails on lines shorter than 1e-12.
    member inline ln.Shrink (distAtStart:float, distAtEnd:float)  =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let l = sqrt(x*x + y*y )
        if l < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.Shrink %O to short for finding point at a distance." ln
        Line2D( ln.FromX + x*distAtStart/l,
                ln.FromY + y*distAtStart/l,
                ln.ToX   - x*distAtEnd/l,
                ln.ToY   - y*distAtEnd/l)

    /// Shrink 2D line by absolute amount at start.
    /// Fails on lines shorter than 1e-12.
    member inline ln.ShrinkStart (distAtStart:float)  =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let l = sqrt(x*x + y*y )
        if l < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.ShrinkStart %O to short for finding point at a distance." ln
        Line2D( ln.FromX + x*distAtStart/l,
                ln.FromY + y*distAtStart/l,
                ln.ToX   ,
                ln.ToY   )

    /// Shrink 2D line by absolute amount at end.
    /// Fails on lines shorter than 1e-12.
    member inline ln.ShrinkEnd  (distAtEnd:float)  =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let l = sqrt(x*x + y*y )
        if l < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.ShrinkEnd %O to short for finding point at a distance." ln
        Line2D( ln.FromX ,
                ln.FromY ,
                ln.ToX   - x*distAtEnd/l,
                ln.ToY   - y*distAtEnd/l )



    /// Returns a Line2D moved by a vector.
    member inline ln.Move (v:Vc) =
        Line2D( ln.FromX+v.X,
                ln.FromY+v.Y,
                ln.ToX+v.X,
                ln.ToY+v.Y)

    /// Returns a Line2D moved by a given distance in X direction.
    member inline ln.MoveX (distance:float) =
        Line2D( ln.FromX+distance,
                ln.FromY,
                ln.ToX+distance,
                ln.ToY)

    /// Returns a Line2D moved by a given distance in Y direction.
    member inline ln.MoveY (distance:float) =
        Line2D( ln.FromX,
                ln.FromY+distance,
                ln.ToX,
                ln.ToY+distance)

    /// Assumes Line2D to be infinite!
    /// Returns the parameter at which a point is closest to the infinite line.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finite line.
    /// Fails on curves shorter than 1e-9 units. (ln.ClosestParameter does not)
    member ln.ClosestParameterInfinite (p:Pt) =
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let x = ln.FromX - ln.ToX
        let y = ln.FromY - ln.ToY
        let lenSq = x*x + y*y
        if lenSq < 1e-18 then // corresponds to a line Length of 1e-9
            FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.ClosestParameterInfinite failed on very short line %O %O" ln p
        let u = ln.FromX-p.X
        let v = ln.FromY-p.Y
        let dot = x*u + y*v
        dot / lenSq

    /// Return the parameter at which a point is closest to the (finite) line.
    /// The result is between 0.0 and 1.0.
    /// Does not fails on very short curves.
    member inline ln.ClosestParameter (p:Pt) =
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let x = ln.FromX - ln.ToX
        let y = ln.FromY - ln.ToY
        let u = ln.FromX-p.X
        let v = ln.FromY-p.Y
        let dot = x*u + y*v
        let lenSq = x*x + y*y
        if lenSq < 1e-18 then // corresponds to a line Length of 1e-9
            if dot < 0.0 then 0.0 else 1.0
        else
            dot / lenSq |> Util.clampBetweenZeroAndOne

    /// Assumes Line2D to be infinite!
    /// Returns closest point on infinite Line.
    /// Fails on curves shorter than 1e-9 units. (ln.ClosestPoint does not.)
    member ln.ClosestPointInfinite (p:Pt) =
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let x = ln.FromX - ln.ToX
        let y = ln.FromY - ln.ToY
        let lenSq = x*x + y*y
        if lenSq < 1e-18 then // corresponds to a line Length of 1e-9
            FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.ClosestPoint failed on very short line %O %O" ln p
        let u = ln.FromX-p.X
        let v = ln.FromY-p.Y
        let dot = x*u + y*v
        let t = dot/lenSq
        let x' = ln.FromX - x*t
        let y' = ln.FromY - y*t
        Pt(x', y')

    /// Returns closest point on (finite) Line.
    /// Does not fails on very short curves.
    member ln.ClosestPoint (p:Pt) =
        ln.EvaluateAt(ln.ClosestParameter(p))

    /// Assumes Line2D to be infinite!
    /// Returns square distance from point to infinite line.
    /// Fails on curves shorter than 1e-9 units. (ln.DistanceSqFromPoint does not.)
    member ln.DistanceSqFromPointInfinite(p:Pt) =
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let x = ln.FromX - ln.ToX
        let y = ln.FromY - ln.ToY
        let lenSq = x*x + y*y
        if lenSq < 1e-18 then // corresponds to a line Length of 1e-9
            FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.DistanceFromPointInfinite failed on very short line %O %O" ln p
        let u = ln.FromX - p.X
        let v = ln.FromY - p.Y
        let dot = x*u + y*v
        let t = dot/lenSq
        let x' = ln.FromX - x*t
        let y' = ln.FromY - y*t
        let u' = x' - p.X
        let v' = y' - p.Y
        u'*u' + v'*v'

    /// Assumes Line2D to be infinite!
    /// Returns distance from point to infinite line.
    /// Fails on curves shorter than 1e-9 units. (ln.DistanceFromPoint does not.)
    member inline ln.DistanceFromPointInfinite(p:Pt) =
        ln.DistanceSqFromPointInfinite(p) |> sqrt

    /// Returns square distance from point to finite line.
    member ln.DistanceSqFromPoint(p:Pt) =
        p
        |> ln.ClosestParameter
        |> ln.EvaluateAt
        |> Pt.distanceSq p

    /// Returns distance from point to (finite) line.
    member inline ln.DistanceFromPoint(p:Pt) =
        ln.DistanceSqFromPoint(p) |> sqrt


    /// Checks if the angle between the two 2D lines is less than 180 degrees.
    /// Calculates the dot product of two 2D lines.
    /// Then checks if it is positive.
    member inline ln.MatchesOrientation180  (l:Line2D) =
        let dot = (l.ToX-l.FromX)*(ln.ToX-ln.FromX) + (l.ToY-l.FromY)*(ln.ToY-ln.FromY)
        dot > 0.0

    /// Checks if the angle between the a 2D line and a 2D vector is less than 180 degrees.
    /// Calculates the dot product of both.
    /// Then checks if it is positive.
    member inline ln.MatchesOrientation180  (v:Vc) =
        let dot = v.X*(ln.ToX-ln.FromX) + v.Y*(ln.ToY-ln.FromY)
        dot > 0.0

    /// Checks if the angle between the a 2D line and a 2D unit-vector is less than 180 degrees.
    /// Calculates the dot product of both.
    /// Then checks if it is positive.
    member inline ln.MatchesOrientation180  (v:UnitVc) =
        let dot = v.X*(ln.ToX-ln.FromX) + v.Y*(ln.ToY-ln.FromY)
        dot > 0.0

    /// Checks if the angle between the two 2D lines is less than 90 degrees.
    /// Calculates the dot product of the unit vectors of the two 2D lines.
    /// Then checks if it is bigger than 0.707107 (cosine of 90 degrees).
    member inline ln.MatchesOrientation90  (l:Line2D) =
        let dot = ln.UnitTangent*l.UnitTangent
        dot > float Cosine.``45.0``



    /// Checks if two 2D lines are parallel.
    /// Ignores the line orientation.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minium cosine value.
    /// See FsEx.Geo.Cosine module.
    /// Fails on lines shorter than 1e-12.
    member inline ln.IsParallelTo( other:Line2D, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
        let a = ln.Vector
        let b = other.Vector
        let sa = a.LengthSq
        if sa < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Line2D.IsParallelTo: Line2D 'ln' is too short: %s. 'other':%s " a.AsString b.AsString
        let sb = b.LengthSq
        if sb < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Line2D.IsParallelTo: Line2D 'other' is too short: %s. 'ln':%s " b.AsString a.AsString
        let au = a * (1.0 / sqrt sa )
        let bu = b * (1.0 / sqrt sb )
        abs(bu*au) > float minCosine


    /// Checks if two 2D lines are parallel.
    /// Takes the line orientation into account too.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minium cosine value.
    /// See FsEx.Geo.Cosine module.
    /// Fails on lines shorter than 1e-12.
    member inline ln.IsParallelAndOrientedTo  (other:Line2D, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
        let a = ln.Vector
        let b = other.Vector
        let sa = a.LengthSq
        if sa < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Line2D.IsParallelAndOrientedTo: Line2D 'ln' is too short: %s. 'other':%s " a.AsString b.AsString
        let sb = b.LengthSq
        if sb < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Line2D.IsParallelAndOrientedTo: Line2D 'other' is too short: %s. 'ln':%s " b.AsString a.AsString
        let au = a * (1.0 / sqrt sa )
        let bu = b * (1.0 / sqrt sb )
        bu*au > float minCosine


    /// Checks if two 2D lines are perpendicular to each other.
    /// The default angle tolerance is 89.75 to 90.25 degrees.
    /// This tolerance can be customized by an optional minium cosine value.
    /// The default cosine is 0.0043633 ( = 89.75 deg )
    /// See FsEx.Geo.Cosine module.
    /// Fails on lines shorter than 1e-12.
    member inline ln.IsPerpendicularTo (other:Line2D, [<OPT;DEF(Cosine.``89.75``)>] maxCosine:float<Cosine.cosine> ) =
        let a = ln.Vector
        let b = other.Vector
        let sa = a.LengthSq
        if sa < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Line2D.IsPerpendicularTo: Line2D 'ln' is too short: %s. 'other':%s " a.AsString b.AsString
        let sb = b.LengthSq
        if sb < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Line2D.IsPerpendicularTo: Line2D 'other' is too short: %s. 'ln':%s " b.AsString a.AsString
        let au = a * (1.0 / sqrt sa )
        let bu = b * (1.0 / sqrt sb )
        let d = bu*au
        float -maxCosine < d && d  < float maxCosine // = cosine of 98.75 and 90.25 degrees

    /// Checks if two 2D lines are coincident within the distance tolerance. 1e-6 by default.
    /// This means that lines are parallel within the angle tolerance
    /// and the distance of second start to the first line is less than the distance tolerance.
    /// Also returns false on zero length lines (shorter than 1e-12).
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minium cosine value.
    /// See FsEx.Geo.Cosine module.
    member inline ln.IsCoincidentTo (other:Line2D,
                                    [<OPT;DEF(1e-6)>] distanceTolerance:float,
                                    [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine>) =
        let a = ln.Vector
        let b = other.Vector
        let sa = a.LengthSq
        if sa < 1e-24 then
            false
        else
            let sb = b.LengthSq
            if sb < 1e-24 then
                false
            else
                let au = a * (1.0 / sqrt sa )
                let bu = b * (1.0 / sqrt sb )
                abs(bu*au) > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:
                &&
                let pX = other.FromX
                let pY = other.FromY
                //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
                let x = a.X
                let y = a.Y
                let u = ln.FromX - pX
                let v = ln.FromY - pY
                let dot = x*u + y*v
                let t = dot/sa
                let x' = ln.FromX - x*t
                let y' = ln.FromY - y*t
                let u' = x' - pX
                let v' = y' - pY
                u'*u' + v'*v'  < distanceTolerance * distanceTolerance

    /// Check if a given point is on the right side of the infinite line.
    /// Also returns false if the point is on the line.
    member inline ln.IsPointOnRight(pt:Pt) =
        let lv = ln.Vector.Rotate90CW
        let pv = pt - ln.From
        lv*pv > 0.0 
        
    /// Check if a given point is on the left side of the infinite line.
    /// Also returns false if the point is on the line.
    member inline ln.IsPointOnLeft(pt:Pt) = 
        let lv = ln.Vector.Rotate90CCW
        let pv = pt - ln.From
        lv*pv > 0.0 

    //-------------------------------------------------------------------
    //------------------------static members-----------------------------
    //-------------------------------------------------------------------


    /// Checks if two 2D Lines are equal within tolerance.
    /// Identical Lines in opposite directions are not considered equal.
    static member inline equals tol (a:Line2D) (b:Line2D) =
        let tt = tol*tol
        Pt.distanceSq a.From b.From < tt &&
        Pt.distanceSq a.To b.To < tt

    /// Checks if two 2D lines are coincident within tolerance.
    /// This means that lines are parallel within 0.25 degrees
    /// and the distance of second start point to the first line is less than 1e-6.
    static member inline areCoincident (a:Line2D) (b:Line2D) = a.IsCoincidentTo(b)

    /// Creates a line starting at World Origin and going to along the given vector.
    static member inline createFromVec (v:Vc) = Line2D(0.,0.,v.X,v.Y)

    /// Creates a line starting at given point and going to along the given vector.
    static member inline createFromPtAndVc (p:Pt,v:Vc) =  Line2D(p.X, p.Y, p.X+v.X, p.Y+v.Y)

    /// Returns the Start point of the line. Same as Line2D.from.
    static member inline start (l:Line2D) = l.From

    /// Returns the Start point of the line. Same as Line2D.start.
    static member inline from (l:Line2D) = l.From

    /// Returns the Start point's X coordinate of the line.
    static member inline fromX (l:Line2D) = l.FromX

    /// Returns the Start point's Y coordinate of the line.
    static member inline fromY (l:Line2D) = l.FromY

    /// Returns the End point of the line. Same as Line2D.to'
    static member inline ende (l:Line2D) = l.To

    /// Returns the End point of the line. Same as Line2D.ende.
    static member inline to' (l:Line2D) = l.To

    /// Returns the End point's X coordinate of the line.
    static member inline toX (l:Line2D) = l.ToX

    /// Returns the End point's Y coordinate of the line.
    static member inline toY (l:Line2D) = l.ToY

    /// Set Line2D start point, returns a new line.
    static member inline setStart (pt:Pt) (ln:Line2D) =
        Line2D( pt.X, pt.Y, ln.ToX, ln.ToY)

    /// Set Line2D end point, returns a new line.
    static member inline setEnd (pt:Pt) (ln:Line2D) =
        Line2D( ln.FromX, ln.FromY, pt.X, pt.Y)

    /// Same as Line2D.vector or Line2D.tangent.
    /// The returned vector has the same length as the Line2D.
    static member inline direction (ln:Line2D) =
        Vc(ln.ToX-ln.FromX,ln.ToY-ln.FromY)

    /// Same as Line2D.tangent or Line2D.direction.
    /// The returned vector has the same length as the Line2D.
    static member inline vector (ln:Line2D) =
        Vc(ln.ToX-ln.FromX,ln.ToY-ln.FromY)

    /// Same as Line2D.vector or Line2D.direction.
    /// The returned vector has the same length as the Line2D.
    static member inline tangent (ln:Line2D) =
        Vc(ln.ToX-ln.FromX,ln.ToY-ln.FromY)

    /// Returns a unit-vector of the line Direction.
    static member inline unitTangent (ln:Line2D) = ln.UnitTangent

    /// Returns the length of the line.
    static member inline length (l:Line2D) = l.Length

    /// Returns the square length of the line.
    static member inline lengthSq (l:Line2D) = l.LengthSq

    /// Check if the line has same starting and ending point.
    static member inline isZeroLength (l:Line2D) = l.IsZeroLength

    /// Check if line is shorter than tolerance.
    static member inline isTiny tol (l:Line2D) = l.Length < tol

    /// Check if line is shorter than squared tolerance.
    static member inline isTinySq tol (l:Line2D) = l.LengthSq < tol

    /// Checks if 2D line is parallel to the world X axis.
    /// Tolerance is 1e-6.
    /// Fails on lines shorter than 1e-6.
    static member inline isXAligned (l:Line2D) = l.IsXAligned
    
    /// Checks if 2D line is parallel to the world Y axis.
    /// Tolerance is 1e-6.
    /// Fails on lines shorter than 1e-6.
    static member inline isYAligned (l:Line2D) = l.IsYAligned

    /// Evaluate line at a given parameter ( parameters 0.0 to 1.0 are on the line )
    static member inline evaluateAt t (ln:Line2D)  = ln.EvaluateAt t

    /// Get point at center of line.
    static member inline mid (ln:Line2D) = ln.Mid

    /// Reverse or flip the Line2D (same as Line2D.flip)
    static member inline reverse (ln:Line2D) = ln.Reversed

    /// Reverse or flip the Line2D (same as Line2D.reverse)
    static member inline flip (ln:Line2D) = ln.Reversed

    /// Returns new Line2D from point at Parameter a to point at Parameter b.
    static member inline segment a b (ln:Line2D) = ln.Segment (a, b)

    /// Move a Line2D by a vector. (same as Line2D.move)
    static member inline translate (v:Vc) (ln:Line2D) = ln.Move(v)

    /// Returns a Line2D moved by a given distance in X direction.
    static member inline moveX (distance:float) (ln:Line2D) = ln.MoveX(distance)

    /// Returns a Line2D moved by a given distance in Y direction.
    static member inline moveY (distance:double) (ln:Line2D) = ln.MoveY(distance)

    /// Move a Line2D by a vector. (same as Line2D.translate)
    static member inline move (v:Vc) (ln:Line2D) = ln.Move(v)

    /// Rotation a Line2D.
    static member inline rotate (r:Rotation2D) (l:Line2D) = Line2D(Pt.rotateBy r l.From, Pt.rotateBy r l.To)

    /// Rotation a Line2D around a given Center.
    static member inline rotateOn (cen:Pt) (r:Rotation2D) (l:Line2D) = Line2D(Pt.rotateWithCenterBy cen r l.From, Pt.rotateWithCenterBy cen r l.To)

    /// Ensure 2D Line has a positive dot product with given orientation Line.
    static member inline matchOrientation (orientationToMatch:Line2D) (l:Line2D) =
        if orientationToMatch.Vector * l.Vector < 0.0 then l.Reversed else l

    /// Checks if the angle between the two 2D lines is less than 180 degrees.
    /// Calculates the dot product of two 2D lines.
    /// Then checks if it is positive.
    static member inline matchesOrientation180  (l:Line2D) (ln:Line2D) = l.MatchesOrientation180 ln

    /// Checks if the angle between the two 2D lines is less than 90 degrees.
    /// Calculates the dot product of the unit vectors of the two 2D lines.
    /// Then checks if it is bigger than 0.707107 (cosine of 90 degrees).
    static member inline matchesOrientation90  (l:Line2D) (ln:Line2D) = l.MatchesOrientation90 ln

    /// Checks if two 2D lines are parallel. Ignoring orientation.
    /// Calculates the cross product of the two line vectors. (= the area of the parallelogram)
    /// And checks if it is smaller than 1e-9
    /// (NOTE: for very long lines a higher tolerance might be needed)
    static member inline  areParallel  (l:Line2D) (ln:Line2D) =   l.IsParallelTo ln

    /// Checks if two 2D lines are parallel and orientated the same way.
    /// Calculates the cross product of the two line vectors. (= the area of the parallelogram)
    /// And checks if it is smaller than 1e-9
    /// Then calculates the dot product and checks if it is positive.
    /// (NOTE: for very long lines a higher tolerance might be needed)
    static member inline areParallelAndMatchOrientation  (l:Line2D) (ln:Line2D) =  l.IsParallelAndOrientedTo ln

    /// Checks if two 2D lines are perpendicular.
    /// Calculates the dot product and checks if it is smaller than 1e-9.
    /// (NOTE: for very long lines a higher tolerance might be needed)
    static member inline arePerpendicular(l:Line2D) (ln:Line2D) =  l.IsPerpendicularTo ln

    /// Assumes Line2D to be infinite!
    /// Returns the parameter at which a point is closest to the infinite line.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finite line.
    static member inline closestParameterInfinite (p:Pt) (ln:Line2D)  = ln.ClosestParameterInfinite p


    /// Return the parameter at which a point is closest to the (finite) line.
    /// The result is between 0.0 and 1.0.
    static member inline closestParameter (p:Pt) (ln:Line2D)  = ln.ClosestParameter p

    /// Assumes Line2D to be infinite!
    /// Returns closest point on infinite Line.
    static member inline closestPointInfinite (p:Pt) (ln:Line2D)  = ln.ClosestPointInfinite p


    /// Returns closest point on (finite) Line.
    static member inline closestPoint (p:Pt) (ln:Line2D)  = ln.ClosestPoint p

    /// Assumes Line2D to be infinite!
    /// Returns the square distance from point to infinite line.
    static member inline distanceSqFromPointInfinite(p:Pt) (ln:Line2D)  = ln.DistanceSqFromPointInfinite p

    /// Assumes Line2D to be infinite!
    /// Returns distance from point to infinite line.
    static member inline distanceFromPointInfinite(p:Pt) (ln:Line2D)  = ln.DistanceFromPointInfinite p

    /// Returns the square distance from point to (finite) line.
    static member inline distanceSqFromPoint(p:Pt) (ln:Line2D)  = ln.DistanceSqFromPoint p

    /// Returns distance from point to (finite) line.
    static member inline distanceFromPoint(p:Pt) (ln:Line2D)  = ln.DistanceFromPoint p

    /// Get distance from start of line to point projected onto line, may be negative.
    static member inline lengthToPtOnLine (line:Line2D) pt =
        // TODO can be optimized by inlining floats.
        line.Tangent.Unitized * (pt-line.From)

    /// Extend 2D line by absolute amount at start and end.
    /// Fails on lines shorter than 1e-12.
    static member inline extend (distAtStart:float) (distAtEnd:float) (ln:Line2D) =
        ln.Extend(distAtStart, distAtEnd)

    /// Extend 2D line by absolute amount at start.
    /// Fails on lines shorter than 1e-12.
    static member inline extendStart (distAtStart:float)  (ln:Line2D) =
        ln.ExtendStart(distAtStart)

    /// Extend 2D line by absolute amount at end.
    /// Fails on lines shorter than 1e-12.
    static member inline extendEnd  (distAtEnd:float) (ln:Line2D) =
        ln.ExtendEnd(distAtEnd)

    /// Shrink 2D line by absolute amount at start and end.
    /// Fails on lines shorter than 1e-12.
    static member inline shrink (distAtStart:float) (distAtEnd:float) (ln:Line2D) =
        ln.Shrink(distAtStart, distAtEnd)

    /// Shrink 2D line by absolute amount at start.
    /// Fails on lines shorter than 1e-12.
    static member inline shrinkStart (distAtStart:float)  (ln:Line2D) =
        ln.ShrinkStart(distAtStart)

    /// Shrink 2D line by absolute amount at end.
    /// Fails on lines shorter than 1e-12.
    static member inline shrinkEnd  (distAtEnd:float) (ln:Line2D) =
        ln.ShrinkEnd(distAtEnd)


        

    /// Finds point at given distance from line start.
    /// Fails on lines shorter than 1e-12.
    static member inline pointAtDistance dist (ln:Line2D) =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let len = sqrt(x*x + y*y )
        if len < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.pointAtDistance %O to short for finding point at a distance." ln
        Pt(ln.FromX + x*dist/len,
            ln.FromY + y*dist/len)

    /// Returns new Line2D with given length, going out from start in direction of end.
    /// Fails on lines shorter than 1e-12.
    static member inline withLengthFromStart len (ln:Line2D) =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let l = sqrt(x*x + y*y )
        if l < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.withLengthFromStart %O to short for finding point at a distance." ln
        Line2D( ln.FromX ,
                ln.FromY ,
                ln.FromX + x*len/l ,
                ln.FromY + y*len/l )

    /// Returns new Line2D ending at current LineEnd with given length coming from direction of start.
    /// Fails on lines shorter than 1e-12.
    static member inline withLengthToEnd len (ln:Line2D) =
        let x = ln.FromX-ln.ToX
        let y = ln.FromY-ln.ToY
        let l = sqrt(x*x + y*y )
        if l < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.withLengthToEnd %O to short for finding point at a distance." ln
        Line2D( ln.ToX + x*len/l ,
                ln.ToY + y*len/l ,
                ln.ToX ,
                ln.ToY )


    /// Offset line in XY Plane to left side in line direction.
    /// Fails on lines shorter than 1e-12.
    static member offset amount (ln:Line2D) =
        let x = ln.ToX - ln.FromX
        let y = ln.ToY - ln.FromY
        let lenXY = sqrt (x*x + y*y)
        if lenXY  < 1e-12 then FsExGeoException.Raise "FsEx.Geo.Line2D.offset: Cannot offset vertical Line2D  (by %g) %O" amount ln
        let ox = -y*amount/lenXY // unitized, horizontal , perpendicular  vector
        let oy =  x*amount/lenXY  // unitized, horizontal , perpendicular  vector
        Line2D( ln.FromX+ox,
                ln.FromY+oy,
                ln.ToX+ox,
                ln.ToY+oy)

    /// Returns an array of points of length segment count + 1.
    /// Includes start and endpoint of line.
    static member divide (segments:int) (ln:Line2D) =
        match segments with
        | x when x < 1 -> FsExGeoException.Raise "FsEx.Geo.Line2D.divide failed for %d segments. Minimum one. for %O"  segments ln
        | 1 -> [|ln.From;  ln.To|]
        | k ->
            let x = ln.ToX - ln.FromX
            let y = ln.ToY - ln.FromY
            let sx = ln.FromX
            let sy = ln.FromY
            let kk = float k
            let r = Array.zeroCreate (k+1)
            r.[0] <- ln.From
            for i = 1 to k-1 do
                let t = float i / kk
                r.[i] <- Pt(sx + x*t, sy + y*t)
            r.[k] <- ln.To
            r


    /// Divides line into as many as segments as possible respecting the minimum segment length.
    /// Includes start and endpoint of line.
    static member divideMinLength (minSegmentLength:float) (ln:Line2D) =
        let len = ln.Length
        if len < minSegmentLength then
            FsExGeoException.Raise "FsEx.Geo.Line2D.divideMinLength minSegmentLength %g is bigger than line length %g for %O"  minSegmentLength len ln
        let k = int (len / minSegmentLength)
        Line2D.divide k ln


    /// Divides line into as few as segments as possible respecting the maximum segment length.
    /// Includes start and endpoint of line.
    static member divideMaxLength (maxSegmentLength:float) (ln:Line2D) =
        let len = ln.Length
        let k = int (len / maxSegmentLength) + 1
        Line2D.divide k ln


    //----------------------------------------------------------------------------------------------------------------
    //------------------------------Line Line Intersection : ----------------------------------------------------
    //----------------------------------------------------------------------------------------------------------------

    ///<summary> Intersects two infinite 2D lines.</summary>
    ///<param name="lnA"> The first line.</param>
    ///<param name="lnB"> The second line.</param>
    ///<param name="relAngleDiscriminant"> This is an optional tolerance for the internally calculated relative Angle Discriminant.
    /// The default value is '0.00000952' this corresponds to approx 0.25 degree. Below this angle the 'Parallel' or 'Coincident' union case is returned.
    /// Use the module FsEx.Geo.Util.RelAngleDiscriminant to set another tolerance here.</param>
    ///<param name="coincidentTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If parallel lines are closer than this the 'Coincident' union case is returned .</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than the 'TooShort' union case is returned .</param>
    ///<returns> An IntersectionParam Discriminated Union with the following cases:
    ///
    /// | TwoParam of twoParams : struct(float*float):
    /// The infinite lines are intersecting.
    /// They have each one point where they are intersecting each other.
    /// The tuple's order corresponds to the input order.
    ///
    /// | Parallel:
    /// The lines are parallel within 0.25 degrees.
    /// They have no points in common.
    ///
    /// | Coincident:
    /// The lines are coincident (or maybe even identical) .
    /// As infinite lines they have infinitely many points in common.
    /// They might still not have the same start and end points in their finit definition.
    ///
    /// | TooShort:
    /// One or both input lines is shorter than the given minimum Length tolerance. </returns>
    static member inline intersectionParamInfinite( lnA:Line2D ,
                                                    lnB:Line2D ,
                                                    [<OPT;DEF(RelAngleDiscriminant.``0.25``)>] relAngleDiscriminant:float<RelAngleDiscriminant.relAngDiscr>,
                                                    [<OPT;DEF(1e-6)>] coincidentTolerance:float,
                                                    [<OPT;DEF(1e-6)>] tooShortTolerance:float
                                                    ) : IntersectionParam =
        //https://stackoverflow.com/a/34604574/969070 but DP and DQ are in wrong order !
        let ax = lnA.FromX - lnA.ToX
        let ay = lnA.FromY - lnA.ToY
        let bx = lnB.FromX - lnB.ToX
        let by = lnB.FromY - lnB.ToY
        let a = ax*ax + ay*ay  // square length of A
        let c = bx*bx + by*by  // square length of B
        let shortSq = tooShortTolerance * tooShortTolerance
        if a < shortSq then  // vec A too short
            if c < shortSq then
                IntersectionParam.TooShortBoth
            else
                IntersectionParam.TooShortA
        elif c < shortSq then  // vec B too short
            IntersectionParam.TooShortB
        else
            let b = ax*bx + ay*by  // dot product of both lines
            let vx = lnB.FromX - lnA.FromX
            let vy = lnB.FromY - lnA.FromY
            let ac = a*c // square of square length  , never negative
            let bb = b*b // never negative
            let discriminant = ac - bb // never negative , the dot product cannot be bigger than the two square length multiplied with each other
            let div = ac+bb // never negative
            // getting the relation between the sum and the subtraction gives a good estimate of the angle between the lines
            // see module FsEx.Geo.Util.RelAngleDiscriminant
            let rel = discriminant/div
            if rel < float relAngleDiscriminant then //parallel
                let e = bx*vx + by*vy
                let t = e / c // c is already checked for being non zero. get closest parameter of lnA.From on lnB
                let p = lnB.EvaluateAt(t) //TODO could be inlined to optimize
                if Pt.distanceSq p lnA.From < coincidentTolerance*coincidentTolerance then
                    IntersectionParam.Coincident
                else
                    IntersectionParam.Parallel
            else
                let e = bx*vx + by*vy
                let d = ax*vx + ay*vy
                let t = (b * e - c * d) / discriminant
                let u = (a * e - b * d) / discriminant
                TwoParam (t,u)




    ///<summary> Gets the points at which two infinite 2D lines intersect. Or are closest to each other.</summary>
    ///<param name="lnA"> The first line.</param>
    ///<param name="lnB"> The second line.</param>
    ///<param name="relAngleDiscriminant"> This is an optional tolerance for the internally calculated relative Angle Discriminant.
    /// The default value is '0.00000952' this corresponds to approx 0.25 degree. Below this angle the 'Parallel' or 'Coincident' union case is returned.
    /// Use the module FsEx.Geo.Util.RelAngleDiscriminant to set another tolerance here.</param>
    ///<param name="coincidentTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If parallel lines are closer than this the 'Coincident' union case is returned .</param>
    ///<returns> An IntersectionPoints3D Discriminated Union with the following cases:
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than the 'TooShort' union case is returned .</param>
    ///
    /// | Point of xPoint : Pnt
    ///     The points of 2D intersection.
    ///
    /// | Parallel:
    ///     The lines are parallel, within the given tolerance.
    ///
    /// | Coincident:
    ///     The lines are coincident or maybe even identical.
    ///     As infinite lines they have infinitely many points in common.
    ///
    /// | TooShort:
    ///     One or both input lines is shorter than the given minimum Length tolerance.
    /// </returns>
    static member inline intersectionInfinite  (lnA:Line2D ,
                                                lnB:Line2D,
                                                [<OPT;DEF(RelAngleDiscriminant.``0.25``)>] relAngleDiscriminant:float<RelAngleDiscriminant.relAngDiscr>,
                                                [<OPT;DEF(1e-6)>] coincidentTolerance:float,
                                                [<OPT;DEF(1e-6)>] tooShortTolerance:float
                                                ) : IntersectionPoints2D =
        match Line2D.intersectionParamInfinite(lnA,lnB, relAngleDiscriminant , coincidentTolerance, tooShortTolerance) with
        |TwoParam (u,v)                       -> IntersectionPoints2D.Point (lnA.EvaluateAt u)
        |IntersectionParam.Parallel   -> IntersectionPoints2D.Parallel
        |IntersectionParam.Coincident -> IntersectionPoints2D.Coincident
        |IntersectionParam.TooShortA
        |IntersectionParam.TooShortB
        |IntersectionParam.TooShortBoth  ->  IntersectionPoints2D.TooShort

    ///<summary>Gets the single points where these two infinite 2D lines actually intersect each other.
    /// The returned point is on line A. </summary>
    ///<param name="lnA"> The first line.</param>
    ///<param name="lnB"> The second line.</param>
    ///<param name="relAngleDiscriminant"> This is an optional tolerance for the internally calculated relative Angle Discriminant.
    /// The default value is '0.00000952' this corresponds to approx 0.25 degree. Below this angle the 'Parallel' or 'Coincident' union case is returned.
    /// Use the module FsEx.Geo.Util.RelAngleDiscriminant to set another tolerance here.</param>
    ///<param name="coincidentTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If parallel lines are closer than this the 'Coincident' union case is returned .</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than the 'TooShort' union case is returned .</param>
    /// <returns> A single 3D point</returns>
    static member intersectionPointInfinite(lnA:Line2D ,
                                            lnB:Line2D ,
                                            [<OPT;DEF(RelAngleDiscriminant.``0.25``)>] relAngleDiscriminant:float<RelAngleDiscriminant.relAngDiscr>,
                                            [<OPT;DEF(1e-6)>] coincidentTolerance:float,
                                            [<OPT;DEF(1e-6)>] tooShortTolerance:float
                                            ) : Pt =
        match Line2D.intersectionInfinite(lnA , lnB, relAngleDiscriminant, coincidentTolerance, tooShortTolerance) with
        |IntersectionPoints2D.Point p     -> p
        |IntersectionPoints2D.Parallel    -> FsExGeoException.Raise "FsEx.Geo.Line2D.intersectionPointInfinite: Lines are parallel lnA: \r\n%O and lnB: \r\n%O" lnA lnB
        |IntersectionPoints2D.Coincident  -> FsExGeoException.Raise "FsEx.Geo.Line2D.intersectionPointInfinite: Lines are coincident lnA: \r\n%O and lnB: \r\n%O" lnA lnB
        |IntersectionPoints2D.TooShort    -> FsExGeoException.Raise "FsEx.Geo.Line2D.intersectionPointInfinite: Lines are tooShort lnA: \r\n%O and lnB: \r\n%O" lnA lnB


    //static member  distanceBetweenInfiniteLines(lnA , lnB) = // doesn't make sense for 2D lines



    /// <summary>Returns the intersection kind and the parameters at which two (finite) 2D Lines intersect or are closest to each other.
    /// The results are both between 0.0 and 1.0.
    /// For parallel and coincident lines it still returns the two end points that are closest to each other or a point in the middle of their overlap.
    /// First parameter is on lnA, second parameter is on lnB.
    /// The possible result cases are:
    ///
    /// | Intersecting : The finite lines are intersecting each other in one point inside both lines.
    /// | IntersectingEndsBoth:   The finite lines are intersecting each other at one of their end or start points point.
    /// | IntersectingEndsFirst:  The finite lines are intersecting. The first line is touching the second one with its end or start point.
    /// | IntersectingEndsSecond: The finite lines are intersecting. The second line is touching the first one with its end or start point.
    ///
    /// | Skew: (only applicable for 3D lines)
    ///
    /// | Apart: The finite lines are not intersecting .
    /// At least one of the parameters of closets points would be outside of the range 0.0 and 1.0.
    /// The returned parameters still indicate where the finite lines are closest to each other.
    ///
    ///------- Parallel and other special cases for finite lines: ---------------
    ///
    /// | Parallel : The finite lines are parallel. Within 0.25 degrees.
    /// The returned parameters are in the middle of their overlap,
    /// or the two end points that are closest to each other.
    ///
    /// | Overlapping
    /// The lines are coincident,  overlapping and parallel within 0.25 degrees.
    /// The returned parameters are at start and end of overlap.
    ///
    /// | CoincidentApart: The Lines are coincident, parallel within 0.25 degrees.  But ends are apart.
    /// The returned parameters still indicate where the lines are closest to each other.
    ///
    /// | Continuation : The Lines are coincident, parallel within 0.25 degrees.
    /// The ends are meeting in exactly one point. And Oriented the same way.
    /// The returned parameters indicate which ends these are.
    ///
    /// | ContinuationFlipped: The Lines are coincident, parallel within 0.25 degrees.
    /// The ends are meeting in exactly one point. But orientation is flipped.
    /// The returned parameters indicate which ends these are.
    ///
    /// | Identical: The Lines are identical , in orientation too with in 1e-6 tolerance.
    /// The returned parameters still indicate where the lines start and end.
    ///
    /// | IdenticalFlipped: The Lines are identical. But orientation is flipped.
    /// The returned parameters still indicate where the lines start and end.
    ///
    /// ------------------------------Error Cases: -----------------------------
    ///
    /// | TooShortA
    /// Input line A is shorter than the given minimum Length tolerance.
    /// The returned parameters are 0.5 for line A and the closets point to lineB from the middle of line A.
    ///
    /// | TooShortB
    /// Input line B is shorter than the given minimum Length tolerance.
    /// The returned parameters are the closets point to lineA from the middle of line B and 0.5 for line B.
    ///
    /// | TooShortBoth
    /// Both input lines are shorter than the given minimum Length tolerance.
    /// The returned parameters are 0.5 and 0.5 for both lines.</summary>
    ///<param name="lnA"> The first line.</param>
    ///<param name="lnB"> The second line.</param>
    ///<param name="relAngleDiscriminant"> This is an optional tolerance for the internally calculated relative Angle Discriminant.
    /// The default value is '0.00000952' this corresponds to approx 0.25 degree. Below this angle the 'Parallel' or 'Coincident' union case is returned.
    /// Use the module FsEx.Geo.Util.RelAngleDiscriminant to set another tolerance here.</param>
    ///<param name="coincidentTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If parallel lines are closer than this the 'Coincident' union case is returned .</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than the 'TooShort' union case is returned .</param>
    static member inline intersectionParam (lnA:Line2D ,
                                            lnB:Line2D,
                                            [<OPT;DEF(RelAngleDiscriminant.``0.25``)>] relAngleDiscriminant:float<RelAngleDiscriminant.relAngDiscr>,
                                            [<OPT;DEF(1e-6)>] coincidentTolerance:float ,
                                            [<OPT;DEF(1e-6)>] tooShortTolerance:float
                                            ) : IntersectionKind*float*float =
        match Line2D.intersectionParamInfinite(lnA,lnB, relAngleDiscriminant, coincidentTolerance, tooShortTolerance) with
        | IntersectionParam.TwoParam ( u , v ) ->
            /// numerical error tolerance check to also find an intersection that happens just after the line end:
            let ur = isZeroOneOrBetween u
            let vr = isZeroOneOrBetween v

            let inline fix01 zt1 x = match zt1 with Zero -> 0.0 |One -> 1.0 |Between |Outside -> x
            
            if   ur = Outside || vr = Outside then 
                // finite Lines are not intersecting, still find their closest Points:
                let pu = lnA.EvaluateAt  u
                let vt = Line2D.closestParameter pu lnB
                let pv = lnB.EvaluateAt vt
                let ut = Line2D.closestParameter pv lnA
                Apart, ut ,vt
            elif ur = Zero || ur = One then                 
                if vr = Zero || vr = One then                     
                    IntersectingEndsBoth,(fix01 ur u), (fix01 vr  v)
                else                          
                    IntersectingEndsFirst, (fix01 ur u), v
            elif vr = Zero || vr = One then
                IntersectingEndsSecond, u, (fix01 vr  v)
            else 
                Intersecting, u, v
            

        | IntersectionParam.Parallel ->

            let lv  =  lnA.Direction
            let llv = lnB.Direction
            //make a new line k that is oriented the same way:
            let flip = lv*llv < 0.0
            let k = if flip then lnB.Reversed else lnB
            let l0k0 = Vc.create(lnA.From,k.From)
            let l0k1 = Vc.create(lnA.From,k.To)
            let l1k0 = Vc.create(lnA.To  ,k.From)
            let l1k1 = Vc.create(lnA.To  ,k.To)
            // check if vectors between lines are in same orientation as line:
            let d00 = lv * l0k0 > 0.
            let d01 = lv * l0k1 > 0.
            let d11 = lv * l1k1 > 0.
            let d10 = lv * l1k0 > 0.

            // there are many valid parameters
            // Parameters are at the end or start of line lnA when possible
            // Full logic:
            //let u, v =
            //    if   not d00 && not d01 && not d10 && not d11 then
            //                                                        Printfn.gray "// lnA starts after k ends"
            //                                                        0.0, if flip then 0.0 else  1.0
            //    elif not d00 &&     d01 && not d10 && not d11 then
            //                                                        Printfn.gray "// k is overlapping lnA start"
            //                                                        0.0, lnB.ClosestParameter(lnA.From)
            //    elif     d00 &&     d01 && not d10 && not d11 then
            //                                                        Printfn.gray "// k is on both ends shorter than lnA  "
            //                                                        lnA.ClosestParameter(lnB.From), 0.0
            //    elif not d00 &&     d01 && not d10 &&     d11 then
            //                                                        Printfn.gray "// lnA is on both ends shorter than k  "
            //                                                        0.0  , lnB.ClosestParameter(lnA.From)
            //    elif     d00 &&     d01 && not d10 &&     d11 then
            //                                                        Printfn.gray "// k is overlapping lnA end "
            //                                                        1.0  , lnB.ClosestParameter(lnA.To)
            //    elif     d00 &&     d01 &&     d10 &&     d11 then
            //                                                        Printfn.gray "// k starts after lnA ends"
            //                                                        1.0, if flip then 1.0 else  0.0
            //    else failwith "Bad case in intersectLineParametersInfinite"
            //IntersectionKind.Parallel , u, v

            // Optimized logic:
            if d01 then
                if d10 then
                    IntersectionKind.Parallel ,1.0, if flip then 1.0 else  0.0   // k starts after lnA ends
                else
                    if d00 then
                        if d11 then IntersectionKind.Parallel ,1.0 , lnB.ClosestParameter(lnA.To)   // k is overlapping lnA end
                        else        IntersectionKind.Parallel ,lnA.ClosestParameter(lnB.From), 0.0  // k is on both ends shorter than lnA
                    else
                        IntersectionKind.Parallel ,0.0 , lnB.ClosestParameter(lnA.From) // k is overlapping lnA start // lnA is on both ends shorter than k
            else
                IntersectionKind.Parallel ,0.0 , if flip then 0.0 else  1.0  // lnA starts after k ends


        | IntersectionParam.Coincident ->
            // cases Overlapping | Continuation  | CoincidentApart | Identical
            let lv  =  lnA.Direction // Vec(ax,ay,az)
            let llv = lnB.Direction //Vec(bx,by,bz)
            let flip = lv*llv < 0.0
            let k = if flip then lnB.Reversed else lnB
            let l0k0 = Vc.create(lnA.From,k.From)
            let l0k1 = Vc.create(lnA.From,k.To)
            let l1k0 = Vc.create(lnA.To  ,k.From)
            let l1k1 = Vc.create(lnA.To  ,k.To)
            let coTolSq = coincidentTolerance*coincidentTolerance
            let z00 = l0k0.LengthSq < coTolSq
            let z01 = l0k1.LengthSq < coTolSq
            let z10 = l1k0.LengthSq < coTolSq
            let z11 = l1k1.LengthSq < coTolSq

            if z00 && z11 then
                if flip then IdenticalFlipped , 0.0 , 1.0
                else         Identical        , 0.0 , 0.0
            elif z10  then
                if flip then ContinuationFlipped , 1.0 , 1.0
                else         Continuation        , 1.0 , 0.0
            elif z01  then
                if flip then ContinuationFlipped , 0.0 , 0.0
                else         Continuation        , 0.0 , 1.0
            else
                // check if vectors between lines are in same orientation as line:
                let d00 = lv * l0k0 > 0.
                let d01 = lv * l0k1 > 0.
                let d10 = lv * l1k0 > 0.
                let d11 = lv * l1k1 > 0.
                // Full logic:
                //if   not d00 && not d01 && not d10 && not d11 then
                //                                                    Printfn.gray "// lnA starts after k ends"
                //                                                    CoincidentApart,0.0, if flip then 0.0 else  1.0
                //elif not d00 &&     d01 && not d10 && not d11 then
                //                                                    Printfn.gray "// k is overlapping lnA start"
                //                                                    Overlapping    ,0.0, if flip then 0.0 else  1.0
                //elif     d00 &&     d01 && not d10 && not d11 then
                //                                                    Printfn.gray "// k is on both ends shorter than lnA  "
                //                                                    Overlapping    ,lnA.ClosestParameter(lnB.From), 1.0
                //elif not d00 &&     d01 && not d10 &&     d11 then
                //                                                    Printfn.gray "// lnA is on both ends shorter than k  "
                //                                                    Overlapping    ,0.0  , lnB.ClosestParameter(lnA.To)
                //elif     d00 &&     d01 && not d10 &&     d11 then
                //                                                    Printfn.gray "// k is overlapping lnA end "
                //                                                    Overlapping    ,1.0  , if flip then 1.0 else  0.0
                //elif     d00 &&     d01 &&     d10 &&     d11 then
                //                                                    Printfn.gray "// k starts after lnA ends"
                //                                                    CoincidentApart,1.0, if flip then 1.0 else  0.0
                //else failwith "Bad case in intersectLineParametersInfinite"

                if d01 then
                    if d10 then
                        CoincidentApart, 1.0  , if flip then 1.0 else  0.0   // k starts after lnA ends
                    else
                        if d11 then
                            if  d00 then Overlapping  , 1.0  , if flip then 1.0 else  0.0  // k is overlapping lnA end
                            else         Overlapping  , 0.0  , lnB.ClosestParameter(lnA.To)   // lnA is on both ends shorter than k
                        else
                            if d00 then Overlapping , lnA.ClosestParameter(lnB.From), 1.0   // k is on both ends shorter than lnA
                            else        Overlapping , 0.0 , if flip then 0.0 else  1.0   // k is overlapping lnA start
                else
                    CoincidentApart, 0.0 , if flip then 0.0 else  1.0    // lnA starts after k ends

        |IntersectionParam.TooShortA     -> TooShortA    , 0.5 , lnB.ClosestParameter lnA.Mid
        |IntersectionParam.TooShortB     -> TooShortB    , lnA.ClosestParameter lnB.Mid, 0.5
        |IntersectionParam.TooShortBoth  -> TooShortBoth , 0.5, 0.5

    /// <summary>Returns the intersection kind and the points at which two (finite) 2D Lines are intersecting or closest to each other.
    /// The results are both between 0.0 and 1.0.
    /// For parallel and coincident lines it still returns the two end points that are closest to each other or a point in the middle of their overlap.
    /// First point is on lnA, second point is on lnB.
    /// The possible result cases are:
    ///
    /// | Intersecting : The finite lines are intersecting each other in one point.
    /// | IntersectingEndsBoth: The finite lines are intersecting each other at one of their end or start points point.
    /// | IntersectingEndsFirst: The finite lines are intersecting. The first line is touching the second one with its end or start point.
    /// | IntersectingEndsSecond:  The finite lines are intersecting. The second line is touching the first one with its end or start point.
    ///
    /// | Skew: (but only applicable for 3D lines)
    ///
    /// | Apart: The finite lines are not intersecting.
    /// At least one of the points of closets points would be outside of the range 0.0 and 1.0.
    /// The returned points still indicate where the finite lines are closest to each other.
    ///
    ///------- Parallel and other special cases for finite lines: ---------------
    ///
    /// | Parallel : The finite lines are parallel. Within 0.25 degrees.
    /// The returned points are in the middle of their overlap,
    /// or the two end points that are closest to each other.
    ///
    /// | Overlapping
    /// The lines are coincident,  overlapping and parallel within 0.25 degrees.
    /// The returned points are at start and end of overlap.
    ///
    /// | CoincidentApart: The Lines are coincident, parallel within 0.25 degrees.  But ends are apart.
    /// The returned points still indicate where the lines are closest to each other.
    ///
    /// | Continuation : The Lines are coincident, parallel within 0.25 degrees.
    /// The ends are meeting in exactly one point. And Oriented the same way.
    /// The returned points indicate which ends these are.
    ///
    /// | ContinuationFlipped: The Lines are coincident, parallel within 0.25 degrees.
    /// The ends are meeting in exactly one point. But orientation is flipped.
    /// The returned points indicate which ends these are.
    ///
    /// | Identical: The Lines are identical , in orientation too with in 1e-6 tolerance.
    /// The returned points still indicate where the lines start and end.
    ///
    /// | IdenticalFlipped: The Lines are identical. But orientation is flipped.
    /// The returned points still indicate where the lines start and end.
    ///
    /// ------------------------------Error Cases: -----------------------------
    ///
    /// | TooShortA
    /// Input line A is shorter than the given minimum Length tolerance.
    /// The returned parameters are 0.5 for line A and the closets point to lineB from the middle of line A.
    ///
    /// | TooShortB
    /// Input line B is shorter than the given minimum Length tolerance.
    /// The returned parameters are the closets point to lineA from the middle of line B and 0.5 for line B.
    ///
    /// | TooShortBoth
    /// Both input lines are shorter than the given minimum Length tolerance.
    /// The returned parameters are 0.5 and 0.5 for both lines.</summary>
    ///<param name="lnA"> The first line.</param>
    ///<param name="lnB"> The second line.</param>
    ///<param name="relAngleDiscriminant"> This is an optional tolerance for the internally calculated relative Angle Discriminant.
    /// The default value is '0.00000952' this corresponds to approx 0.25 degree. Below this angle the 'Parallel' or 'Coincident' union case is returned.
    /// Use the module FsEx.Geo.Util.RelAngleDiscriminant to set another tolerance here.</param>
    ///<param name="coincidentTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If parallel lines are closer than this the 'Coincident' union case is returned .</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than the 'TooShort' union case is returned .</param>
    static member inline intersection  (lnA:Line2D ,
                                        lnB:Line2D ,
                                        [<OPT;DEF(RelAngleDiscriminant.``0.25``)>] relAngleDiscriminant:float<RelAngleDiscriminant.relAngDiscr>,
                                        [<OPT;DEF(1e-6)>] coincidentTolerance:float,
                                        [<OPT;DEF(1e-6)>] tooShortTolerance:float
                                        ) : IntersectionKind*Pt*Pt =
        let k,u,v =  Line2D.intersectionParam(lnA,lnB, relAngleDiscriminant, coincidentTolerance, tooShortTolerance)
        match k with
        | Intersecting | IntersectingEndsBoth | IntersectingEndsFirst | IntersectingEndsSecond
        | Continuation | ContinuationFlipped ->
            let p = lnA.EvaluateAt u
            k,p,p // return same point twice ?
        | Skew | Apart  | IntersectionKind.Parallel
        | Overlapping | CoincidentApart | Identical | IdenticalFlipped
        | TooShortA | TooShortB | TooShortBoth -> // TODO or check if the zero length line is actually on the other line ?
            let a  = lnA.EvaluateAt u
            let b  = lnB.EvaluateAt v
            k,a,b


    /// Returns the single points where these two finite lines actually intersect each other. Or None.
    /// Unless the coincident lines ar continuing each other and just touching in one point.
    static member intersectionPoint (lnA:Line2D , lnB:Line2D) : option<Pt> =
        let k,u,v =  Line2D.intersectionParam(lnA,lnB)
        match k with
        | Intersecting | IntersectingEndsBoth | IntersectingEndsFirst | IntersectingEndsSecond
        | Continuation | ContinuationFlipped ->
            Some(lnA.EvaluateAt u)
        | Skew | Apart  | IntersectionKind.Parallel
        | Overlapping | CoincidentApart | Identical| IdenticalFlipped ->
            None
        | TooShortA | TooShortB -> // TODO or check if the short line or zero length line is still exactly on the other line ?
            None
        | TooShortBoth -> // TODO or return a point if two zero length lines are on the same point?
            None

    /// Returns the distance between two finite 2D lines.
    /// For parallel lines the distance is calculate form the actual finit elements. (like in the other cases.)
    /// So it is maybe bigger than the parallel offset.
    /// For Coincident and intersecting lines it always returns 0.0 even if there is actually a distance less than 1e-6.
    static member inline distanceBetweenLines(lnA,lnB) : float=
        let k,u,v =  Line2D.intersectionParam(lnA,lnB)
        match k with
        | Intersecting | IntersectingEndsBoth | IntersectingEndsFirst | IntersectingEndsSecond
        | Continuation | ContinuationFlipped | Overlapping   | Identical| IdenticalFlipped ->
            0.0
        | Skew | Apart  | IntersectionKind.Parallel | CoincidentApart
        | TooShortA | TooShortB | TooShortBoth ->
            let a  =  lnA.EvaluateAt u
            let b  = lnB.EvaluateAt v
            Pt.distance a b





