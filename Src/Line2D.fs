namespace FsEx.Geo

open FsEx.Geo.Util

/// A module for the result types of 2D and 3D Line-Line-intersections.
module LineIntersectionTypes = 

    /// For infinite 2D or 3D lines.
    /// The result line parameters from computing the intersection.
    [<Struct>]
    type IntersectionParamInfinite =  
        
        /// The infinite lines are intersecting (2D and 3D) or skew (3D only) 
        /// They have each one point where they are touching each other. ( Or are closest to each other. 3D only)
        /// Contains the parameters on the first and second line.
        | TwoParam of twoParams : struct(float*float)
        
        /// The lines are parallel within 0.25 degrees.
        /// They have no points in common
        | Parallel 
        
        /// The lines are coincident (or maybe even identical) .
        /// As infinite lines they have infinitely many points in common.
        /// They might still not have the same start and end points in their finit definition.
        | Coincident


    /// For infinite 2D lines.
    /// The result from computing the intersection of two infinite 2D lines.
    [<Struct>]
    [<NoEquality; NoComparison>]
    type IntersectionPointsInfinite2D =  

        /// The points of 2D intersection.
        | Point of xPoint:Pt
        
        /// The lines are parallel within 0.25 degrees.
        | Parallel

        /// The lines are coincident or maybe even identical.
        /// As infinite lines they have infinitely many points in common.
        /// They might still not have the same start and end points in their finit definition.
        | Coincident
    
    /// For infinite 3D lines.
    /// The result from computing the intersection of two infinite 2D lines.
    // [<Struct>]
    [<NoEquality; NoComparison>]
    type IntersectionPointsInfinite3D =  
        
        /// The lines are skew by mor than 1e-6. or the given tolerance
        /// Contains the points on the first and second 
        /// line where they are closest to each other.
        | TwoPoints of skewPoints : struct(Pnt*Pnt)

        /// The 3D lines are intersection in exactly one point.
        | OnePoint of xPoint : Pnt
        
        /// The lines are parallel within 0.25 degrees.
        | Parallel

        /// The lines are coincident or maybe even identical.
        /// As infinite lines they have infinitely many points in common.        
        | Coincident
    

    /// For finite 2D or 3D lines.
    /// An enumeration of all possible results from computing the intersection. 
    /// For finite lines there are much mor cases than for infinite lines
    /// General Cases:
    /// | Intersecting | IntersectingEndsBoth | IntersectingEndsFirst | IntersectingEndsSecond | Skew | Apart 
    /// Parallel Cases:
    /// | Parallel | Overlapping | CoincidentApart | Continuation | ContinuationFlipped| Identical| IdenticalFlipped
    type IntersectionKind =

        /// The finite lines are intersecting each other in one point.        
        | Intersecting 

        /// The finite lines are intersecting each other at one of their end or start points point.        
        | IntersectingEndsBoth 
        
        /// The finite lines are intersecting. The first line is touching the second one with its end or start point.        
        | IntersectingEndsFirst 

        /// The finite lines are intersecting. The second line is touching the first one with its end or start point.        
        | IntersectingEndsSecond

        /// The finite lines are skew to each other.
        /// Their closest points to each other are within the line.
        /// The returned parameters are between 0.0 and 1.0
        | Skew 

        /// The finite lines are not intersecting nor skew.
        /// At least one of the parameters of closets points would be outside of  the range 0.0 and 1.0.
        /// The returned parameters  still indicate where the finite lines are closest to each other.
        | Apart 
        
        //------- Parallel and other special cases for finite lines: ---------------

        /// The finite lines are parallel.
        /// Within 0.25 degrees.
        /// The returned parameters are in the middle of their overlap, 
        /// or in the middle of their distance apart.
        | Parallel 

        /// The lines are coincident,  overlapping and parallel within 0.25 degrees.
        /// The returned parameters are at start and end of overlap.
        | Overlapping 
        
        /// The Lines are coincident, parallel within 0.25 degrees. 
        /// But ends are apart. 
        /// The returned parameters still indicate where the lines are closest to each other.
        | CoincidentApart 

        /// The Lines are coincident, parallel within 0.25 degrees. 
        /// The ends are meeting in exactly one point.
        /// And Oriented the same way.
        /// The returned parameters indicate which ends these are.
        | Continuation 
        
        /// The Lines are coincident, parallel within 0.25 degrees.
        /// The ends are meeting in exactly one point.
        /// But orientation is flipped
        /// The returned parameters indicate which ends these are.
        | ContinuationFlipped

        /// The Lines are identical , in orientation too with in 1-e6 tolerance.
        /// The returned parameters still indicate where the lines start and end.
        | Identical

        /// The Lines are identical. But orientation is flipped
        /// The returned parameters still indicate where the lines start and end.
        | IdenticalFlipped
        
    /// Return true if the IntersectionKind is represented by one Point. 
    /// Not two like in skew or none like in parallel.
    let isIntersectionOnePoint k =
        match k with        
        | Intersecting | IntersectingEndsBoth | IntersectingEndsFirst 
        | IntersectingEndsSecond  | Continuation | ContinuationFlipped -> true
        | Skew | Apart | IntersectionKind.Parallel  
        | Overlapping  | CoincidentApart | Identical| IdenticalFlipped -> false
        
open LineIntersectionTypes        
open System.Runtime.InteropServices

/// An immutable finite line in 2D. Represented by a 2D start and 2D end point.
[<Struct;NoEquality;NoComparison>]// because its made up from floats
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

     /// Returns the Square length of the line.
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
    
    /// Same as ln.Vector or ln.Tangent
    /// The returned vector has the same length as the Line2D.
    member inline ln.Direction = 
        Vc(ln.ToX-ln.FromX,ln.ToY-ln.FromY)
    
    /// Same as ln.Tangent or ln.Direction
    /// The returned vector has the same length as the Line2D.
    member inline ln.Vector = 
        Vc(ln.ToX-ln.FromX,ln.ToY-ln.FromY)
    
    /// Same as ln.Vector or ln.Direction 
    /// The returned vector has the same length as the Line2D.
    member inline ln.Tangent = 
        Vc(ln.ToX-ln.FromX,ln.ToY-ln.FromY)
        
    /// Returns a unit vector of the line Direction
    member inline ln.UnitTangent = 
        UnitVc.create(ln.ToX-ln.FromX, ln.ToY-ln.FromY)   
    

    /// Check if the line has same starting and ending point.
    member inline ln.IsZeroLength = 
        ln.ToX = ln.FromX &&
        ln.ToY = ln.FromY 

    /// Check if line is shorter than tolerance.
    member inline ln.IsTiny tol = 
        ln.Length < tol
    
    /// Check if line is shorter than Squared tolerance.
    member inline ln.IsTinySq tol = 
        ln.LengthSq < tol
    
    /// Evaluate line at a given parameter ( parameters 0.0 to 1.0 are on the line ),
    member inline ln.EvaluateAt (p:float) = 
        let x = ln.FromX + (ln.ToX-ln.FromX)*p
        let y = ln.FromY + (ln.ToY-ln.FromY)*p        
        Pt(x,y)    
    
    /// Returns the midpoint of the line,
    member inline ln.Mid = 
        let x = (ln.ToX+ln.FromX)*0.5
        let y = (ln.ToY+ln.FromY)*0.5        
        Pt(x,y) 
    
    /// Returns the Line2D reversed.
    member inline ln.Reversed = 
        Line2D(ln.ToX,ln.ToY,ln.FromX,ln.FromY)  
    
    /// returns the lines Bounding Rectangle
    member inline ln.BoundingRect = 
        BRect.create ( ln.From, ln.To)

    /// Returns a Line2D from point at Parameter a to point at Parameter b
    member inline ln.Segment(a, b) = 
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        Line2D( ln.FromX + x*a,
                ln.FromY + y*a,
                ln.FromX + x*b,
                ln.FromY + y*b) 
    
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
    /// Fails on curves shorter than  1e-9 units. (ln.ClosestParameter does not)
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
    /// Fails on curves shorter than  1e-9 units. (ln.ClosestPoint does not.)
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
    /// Returns Square distance from point to infinite line.
    /// Fails on curves shorter than  1e-9 units. (ln.DistanceSqFromPoint does not.)
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
    /// Fails on curves shorter than  1e-9 units. (ln.DistanceFromPoint does not.)
    member inline ln.DistanceFromPointInfinite(p:Pt) =   
        ln.DistanceSqFromPointInfinite(p) |> sqrt   
    
    /// Returns Square distance from point to finite line.
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

    /// Checks if the angle between the a 2D line and a 2D unit vector is less than 180 degrees.
    /// Calculates the dot product of both. 
    /// Then checks if it is positive.
    member inline ln.MatchesOrientation180  (v:UnitVc) = 
        let dot = v.X*(ln.ToX-ln.FromX) + v.Y*(ln.ToY-ln.FromY) 
        dot > 0.0 

    /// Checks if the angle between the two 2D lines is less than 90 degrees.   
    /// Calculates the dot product of the unit vectors of the two 2D lines. 
    /// Then checks if it is bigger than 0.707107 (cosine of  90 degrees).
    member inline ln.MatchesOrientation90  (l:Line2D) = 
        let dot = ln.UnitTangent*l.UnitTangent
        dot > Cosine.``45.0``
                


    /// Checks if two 2D lines are parallel.
    /// Ignores the line orientation.
    /// The default angle tolerance is 0.25 degrees.  
    /// This tolerance can be customized by an optional minium cosine value.
    /// See FsEx.Geo.Cosine module.
    /// Fails on lines shorter than 1e-12.    
    member inline ln.IsParallelTo( other:Line2D, [<OPT;DEF(Cosine.``0.25``)>] minCosine ) = 
        let a = ln.Vector
        let b = other.Vector
        let sa = a.LengthSq
        if sa < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Line2D.IsParallelTo: Line2D 'ln' is too short: %s. 'other':%s " a.AsString b.AsString
        let sb = b.LengthSq
        if sb < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Line2D.IsParallelTo: Line2D 'other' is too short: %s. 'ln':%s " b.AsString a.AsString  
        let au = a * (1.0 / sqrt sa )
        let bu = b * (1.0 / sqrt sb )
        abs(bu*au) > minCosine // 0.999990480720734 = cosine of 0.25 degrees:            
        
        
    /// Checks if two 2D lines are parallel.
    /// Takes the line orientation into account too.
    /// The default angle tolerance is 0.25 degrees.  
    /// This tolerance can be customized by an optional minium cosine value.
    /// See FsEx.Geo.Cosine module.
    /// Fails on lines shorter than 1e-12.       
    member inline ln.IsParallelAndOrientedTo  (other:Line2D, [<OPT;DEF(Cosine.``0.25``)>] minCosine ) = 
        let a = ln.Vector
        let b = other.Vector
        let sa = a.LengthSq
        if sa < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Line2D.IsParallelAndOrientedTo: Line2D 'ln' is too short: %s. 'other':%s " a.AsString b.AsString
        let sb = b.LengthSq
        if sb < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Line2D.IsParallelAndOrientedTo: Line2D 'other' is too short: %s. 'ln':%s " b.AsString a.AsString 
        let au = a * (1.0 / sqrt sa )
        let bu = b * (1.0 / sqrt sb )
        bu*au >  minCosine // 0.999990480720734 = cosine of 0.25 degrees:    
        
    
    /// Checks if two 2D lines are perpendicular to each other.
    /// The default angle tolerance is 89.75 to  90.25 degrees.   
    /// This tolerance can be customized by an optional minium cosine value.
    /// The default cosine is 0.0043633 ( = 89.75 deg )
    /// See FsEx.Geo.Cosine module.
    /// Fails on lines shorter than 1e-12.  
    member inline ln.IsPerpendicularTo (other:Line2D, [<OPT;DEF(Cosine.``89.75``)>] maxCosine ) = 
        let a = ln.Vector
        let b = other.Vector
        let sa = a.LengthSq
        if sa < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Line2D.IsPerpendicularTo: Line2D 'ln' is too short: %s. 'other':%s " a.AsString b.AsString
        let sb = b.LengthSq
        if sb < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Line2D.IsPerpendicularTo: Line2D 'other' is too short: %s. 'ln':%s " b.AsString a.AsString 
        let au = a * (1.0 / sqrt sa )
        let bu = b * (1.0 / sqrt sb )
        let d = bu*au            
        -maxCosine < d && d  < maxCosine // = cosine of 98.75 and 90.25 degrees          
        
            
    //-------------------------------------------------------------------
    //------------------------static members-----------------------------
    //-------------------------------------------------------------------


    /// Checks if two 2D Lines are equal within tolerance.
    /// Identical Lines in opposite directions are not considered equal.
    static member equals tol (a:Line2D) (b:Line2D) =
        let tt = tol*tol
        Pt.distanceSq a.From b.From < tt &&
        Pt.distanceSq a.To b.To < tt

    /// Creates a line starting at World Origin and going to along the given Vector.
    static member inline createFromVec (v:Vc) = Line2D(0.,0.,v.X,v.Y)

    /// Creates a line starting at given Point and going to along the given Vector.
    static member inline createFromPtAndVc (p:Pt,v:Vc) =  Line2D(p.X, p.Y, p.X+v.X, p.Y+v.Y) 

    /// Returns the Start point of the line. Same as Line2D.from
    static member inline start (l:Line2D) = l.From
    
    /// Returns the Start point of the line. Same as Line2D.start
    static member inline from (l:Line2D) = l.From

    /// Returns the Start point's X coordinate of the line.
    static member inline fromX (l:Line2D) = l.FromX
    
    /// Returns the Start point's Y coordinate of the line.
    static member inline fromY (l:Line2D) = l.FromY
    
    /// Returns the End point of the line. Same as Line2D.to'
    static member inline ende (l:Line2D) = l.To
    
    /// Returns the End point of the line. Same as Line2D.ende
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

    /// Same as Line2D.vector or Line2D.tangent
    /// The returned vector has the same length as the Line2D.
    static member inline direction (ln:Line2D) = 
        Vc(ln.ToX-ln.FromX,ln.ToY-ln.FromY)
    
    /// Same as Line2D.tangent or Line2D.direction
    /// The returned vector has the same length as the Line2D.
    static member inline vector (ln:Line2D) = 
        Vc(ln.ToX-ln.FromX,ln.ToY-ln.FromY)
    
    /// Same as Line2D.vector or Line2D.direction 
    /// The returned vector has the same length as the Line2D.
    static member inline tangent (ln:Line2D) = 
        Vc(ln.ToX-ln.FromX,ln.ToY-ln.FromY)
        
    /// Returns a unit vector of the line Direction
    static member inline unitTangent (ln:Line2D) = 
        UnitVc.create(ln.ToX - ln.FromX, ln.ToY-ln.FromY)

    /// Returns the length of the line.
    static member inline length (l:Line2D) = l.Length

    /// Returns the Square length of the line.
    static member inline lengthSq (l:Line2D) = l.LengthSq  

    /// Check if the line has same starting and ending point.
    static member inline isZeroLength (l:Line2D) = l.IsZeroLength
    
    /// Check if line is shorter than tolerance.
    static member inline isTiny tol (l:Line2D) = l.Length < tol        
    
    /// Check if line is shorter than Squared tolerance.
    static member inline isTinySq tol (l:Line2D) = l.LengthSq < tol

    /// Evaluate line at a given parameter ( parameters 0.0 to 1.0 are on the line )
    static member inline evaluateAt t (ln:Line2D)  = ln.EvaluateAt t
    
    /// Get point at center of line
    static member inline mid (ln:Line2D) = ln.Mid

    /// Reverse or flip  the Line2D (same as Line2D.flip)
    static member inline reverse (ln:Line2D) = ln.Reversed

    /// Reverse or flip  the Line2D (same as Line2D.reverse)
    static member inline flip (ln:Line2D) = ln.Reversed
    
    /// Returns new Line2D from point at Parameter a to point at Parameter b
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

    /// Ensure 2D Line has a positive dot product with given orientation Line
    static member inline matchOrientation (orientationToMatch:Line2D) (l:Line2D) = 
        if orientationToMatch.Vector * l.Vector < 0.0 then l.Reversed else l  

    /// Checks if the angle between the two 2D lines is less than 180 degrees.
    /// Calculates the dot product of two 2D lines. 
    /// Then checks if it is positive.
    static member inline matchesOrientation180  (l:Line2D) (ln:Line2D) = l.MatchesOrientation180 ln       

    /// Checks if the angle between the two 2D lines is less than 90 degrees.   
    /// Calculates the dot product of the unit vectors of the two 2D lines. 
    /// Then checks if it is bigger than 0.707107 (cosine of  90 degrees).
    static member inline matchesOrientation90  (l:Line2D) (ln:Line2D) = l.MatchesOrientation90 ln
        
    /// Checks if two 2D lines are parallel. Ignoring orientation
    /// Calculates the cross product of the two line vectors. (= the area of the parallelogram)
    /// And checks if it is smaller than 1e-9
    /// (NOTE: for very long lines a higher tolerance might be needed)
    static member inline isParallelTo  (l:Line2D) (ln:Line2D) =   l.IsParallelTo ln

    /// Checks if two 2D lines are parallel and orientated the same way.
    /// Calculates the cross product of the two line vectors. (= the area of the parallelogram)
    /// And checks if it is smaller than 1e-9
    /// Then calculates the dot product and checks if it is positive.
    /// (NOTE: for very long lines a higher tolerance might be needed)
    static member inline isParallelAndOrientedTo  (l:Line2D) (ln:Line2D) =  l.IsParallelAndOrientedTo ln
        
    /// Checks if two 2D lines are perpendicular. 
    /// Calculates the dot product and checks if it is smaller than 1e-9.
    /// (NOTE: for very long lines a higher tolerance might be needed)
    static member inline isPerpendicularTo (l:Line2D) (ln:Line2D) =  l.IsPerpendicularTo ln

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
    /// Returns Square distance from point to infinite line.
    static member inline distanceSqFromPointInfinite(p:Pt) (ln:Line2D)  = ln.DistanceSqFromPointInfinite p
    
    /// Assumes Line2D to be infinite!
    /// Returns distance from point to infinite line.
    static member inline distanceFromPointInfinite(p:Pt) (ln:Line2D)  = ln.DistanceFromPointInfinite p   
        
    /// Returns Square distance from point to (finite) line.
    static member inline distanceSqFromPoint(p:Pt) (ln:Line2D)  = ln.DistanceSqFromPoint p
    
    /// Returns distance from point to (finite) line.
    static member inline distanceFromPoint(p:Pt) (ln:Line2D)  = ln.DistanceFromPoint p
    
    /// Get distance from start of line to point projected onto line, may be negative
    static member inline lengthToPtOnLine (line:Line2D) pt = 
        // TODO can be optimized by inlining floats.
        line.Tangent.Unitized * (pt-line.From)          

    /// Extend by absolute amount at start and end.
    static member inline extend (distAtStart:float) (distAtEnd:float) (ln:Line2D) = 
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY         
        let l = sqrt(x*x + y*y )
        if l < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.extend %O to short for finding point at a Distance" ln        
        Line2D( ln.FromX - x*distAtStart/l, 
                ln.FromY - y*distAtStart/l, 
                ln.ToX   + x*distAtEnd/l, 
                ln.ToY   + y*distAtEnd/l)
    
    /// Extend by absolute amount at start.
    static member inline extendStart (distAtStart:float)  (ln:Line2D) = 
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY         
        let l = sqrt(x*x + y*y )
        if l < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.extendStart %O to short for finding point at a Distance" ln        
        Line2D( ln.FromX - x*distAtStart/l, 
                ln.FromY - y*distAtStart/l,
                ln.ToX   , 
                ln.ToY   )
    
    /// Extend by absolute amount at end.
    static member inline extendEnd  (distAtEnd:float) (ln:Line2D) = 
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY         
        let l = sqrt(x*x + y*y )
        if l < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.extendEnd %O to short for finding point at a Distance" ln        
        Line2D( ln.FromX , 
                ln.FromY , 
                ln.ToX   + x*distAtEnd/l, 
                ln.ToY   + y*distAtEnd/l )  

    /// Finds point at given distance from line start
    static member inline pointAtDistance dist (ln:Line2D) = 
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY         
        let len = sqrt(x*x + y*y )
        if len < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.pointAtDistance %O to short for finding point at a Distance" ln        
        Pt(ln.FromX + x*dist/len, 
            ln.FromY + y*dist/len)
    
    /// Returns new Line2D with given length, going out from start in direction of end
    static member inline withLengthFromStart len (ln:Line2D) = 
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY        
        let l = sqrt(x*x + y*y )
        if l < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.withLengthFromStart %O to short for finding point at a Distance" ln        
        Line2D( ln.FromX , 
                ln.FromY , 
                ln.FromX + x*len/l , 
                ln.FromY + y*len/l ) 
    
    /// Returns new Line2D ending at current LineEnd with given length coming from direction of start.
    static member inline withLengthToEnd len (ln:Line2D) = 
        let x = ln.FromX-ln.ToX
        let y = ln.FromY-ln.ToY
        let l = sqrt(x*x + y*y )
        if l < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.withLengthToEnd %O to short for finding point at a Distance" ln
        Line2D( ln.ToX + x*len/l , 
                ln.ToY + y*len/l , 
                ln.ToX , 
                ln.ToY )
        

    /// Offset line in XY Plane to left side in line direction
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


    /// Assumes Lines to be infinite.
    /// Returns either the parameters at which two infinite 2D Lines intersect to each other.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finite line.    
    /// Or if the lines are parallel within approx 0.25 degrees then the Parallel Union Case. 
    /// Or if the parallel lines are closer than 1e-6 than Coincident Union Case.
    /// First parameter is on l, second parameter is on ll.
    static member inline intersectionParamInfinite(l:Line2D, ll:Line2D) : IntersectionParamInfinite =        
        //https://stackoverflow.com/a/34604574/969070 but DP and DQ are in wrong order !        
        let ax = l.FromX - l.ToX  
        let ay = l.FromY - l.ToY  
        let bx = ll.FromX - ll.ToX 
        let by = ll.FromY - ll.ToY 
        let vx = ll.FromX - l.FromX
        let vy = ll.FromY - l.FromY
        let a = ax*ax + ay*ay // square length
        let b = ax*bx + ay*by // dot product of lines
        let c = bx*bx + by*by // square length        
        let ac = a*c // square of square length  , never negative
        let bb = b*b // square of dot pf lines, never negative
        let discriminant = ac - bb
        let div = ac+bb // never negative
        if div < 2e-48 then // both or one  lines shorter than 1e-12 ! (2 * 1e-12 * 1e-12 * 1e-12 * 1e-12)
            TwoParam (0.5, 0.5) 
        else
            let e = bx*vx + by*vy  
            // getting the relation between the sum and the subtraction gives a good estimate of the angle between the lines
            // see file 'Calculate Angle constants for Line3D intersection.fsx' in Docs folder.
            // 1.5e-6 for 0.1  degree  
            // 1e-5   for 0.25 degree 
            // 1.5e-4 for 1.0  degree   
            let rel = discriminant/div
            if rel < 1e-5 then //parallel               
                let t = e / c //closest parameter of l.From on ll
                let p = ll.EvaluateAt(t) //TODO could be inlined to optimize 
                if Pt.distanceSq p l.From < 1e-12 then // square of 1e-6
                    IntersectionParamInfinite.Coincident
                else   
                    IntersectionParamInfinite.Parallel        
            else 
                let d = ax*vx + ay*vy
                let t = (b * e - c * d) / discriminant
                let u = (a * e - b * d) / discriminant
                TwoParam (t,u) 

    
    
    
    /// Assumes Lines to be infinite.    
    /// Returns the one Point where the two lines intersect.
    /// Else if the lines are parallel within approx 0.25 degrees then The Parallel Union Case.  
    /// Or if the parallel lines are closer than 1e-6 than Coincident Union Case.  
    static member inline intersectionInfinite  (l:Line2D , ll:Line2D) : IntersectionPointsInfinite2D =  
        match Line2D.intersectionParamInfinite(l,ll) with 
        |TwoParam (u,v)                       -> IntersectionPointsInfinite2D.Point (l.EvaluateAt u)
        |IntersectionParamInfinite.Parallel   -> IntersectionPointsInfinite2D.Parallel
        |IntersectionParamInfinite.Coincident -> IntersectionPointsInfinite2D.Coincident


    /// Assumes Lines to be infinite.    
    /// Returns the single points where these two infinite lines actually intersect each other.
    /// Raises an Exception if lines are parallel or coincident
    static member intersectionPointInfinite (l:Line2D , ll:Line2D) : Pt = 
        match Line2D.intersectionInfinite(l,ll) with 
        |IntersectionPointsInfinite2D.Point p  -> p
        |IntersectionPointsInfinite2D.Coincident  -> FsExGeoException.Raise "FsEx.Geo.Line2D.intersectionPointInfinite: Lines are coincident l: \r\n%O and ll: \r\n%O" l ll
        |IntersectionPointsInfinite2D.Parallel    -> FsExGeoException.Raise "FsEx.Geo.Line2D.intersectionPointInfinite: Lines are parallel l: \r\n%O and ll: \r\n%O" l ll
        
    /// Assumes Lines to be infinite.    
    /// Returns the distance between two infinite lines.
    /// Unless they are parallel and not coinciding the result is 0.0.   
    static member inline distanceBetweenInfiniteLines(l,ll) =
        match Line2D.intersectionParamInfinite(l,ll) with 
        |IntersectionParamInfinite.Coincident  
        |TwoParam _ -> 0.0
        |IntersectionParamInfinite.Parallel ->  l.DistanceFromPointInfinite ll.From

    /// Returns the intersection kind and the parameters at which two (finite) 2D Lines are intersect or are closest to each other.
    /// The results are both between 0.0 and 1.0.
    /// For parallel and coincident lines it still returns two parameters, in the middle of their overlap, or distance apart.
    /// First parameter is on l, second parameter is on ll.
    /// The possible result cases are:  
    /// 
    /// | Intersecting : The finite lines are intersecting each other in one point.
    /// | IntersectingEndsBoth: The finite lines are intersecting each other at one of their end or start points point. 
    /// | IntersectingEndsFirst: The finite lines are intersecting. The first line is touching the second one with its end or start point.       
    /// | IntersectingEndsSecond:  The finite lines are intersecting. The second line is touching the first one with its end or start point.        
    /// 
    /// | Skew: (only applicable for 3D lines)
    /// 
    /// | Apart: The finite lines are not intersecting .
    /// At least one of the parameters of closets points would be outside of  the range 0.0 and 1.0.
    /// The returned parameters  still indicate where the finite lines are closest to each other.
    /// 
    ///------- Parallel and other special cases for finite lines: ---------------
    /// 
    /// | Parallel : The finite lines are parallel. Within 0.25 degrees.
    /// The returned parameters are in the middle of their overlap, 
    /// or in the middle of their distance apart.
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
    /// | Identical: The Lines are identical , in orientation too with in 1-e6 tolerance.
    /// The returned parameters still indicate where the lines start and end.
    /// 
    /// | IdenticalFlipped: The Lines are identical. But orientation is flipped.
    /// The returned parameters still indicate where the lines start and end.        
    static member inline intersectionParam (l:Line2D , ll:Line2D) : IntersectionKind*float*float =         
        match Line2D.intersectionParamInfinite(l,ll) with 
        | IntersectionParamInfinite.TwoParam ( u0 , v0 ) -> 
            /// numerical error tolerance check to also find an intersection that happens just after the line end:
            let ur = isZeroOneOrBetween u0
            let vr = isZeroOneOrBetween v0
            let u =  match ur with Zero -> 0.0 |One -> 1.0 |Between |Outside -> u0
            let v =  match vr with Zero -> 0.0 |One -> 1.0 |Between |Outside -> v0             
            if ur=Zero || ur=One then 
                if vr=Zero || vr=One then 
                    IntersectingEndsBoth , u ,v
                else
                    IntersectingEndsFirst, u ,v
            elif vr=Zero || vr=One then 
                IntersectingEndsSecond, u,v
            elif  isBetweenZeroAndOne u && isBetweenZeroAndOne v then 
                Intersecting, u , v
            else 
                // finite Lines are not intersecting, still find their closest Points:
                let pu = l.EvaluateAt  u
                let vt = Line2D.closestParameter pu ll
                let pv = ll.EvaluateAt vt
                let ut = Line2D.closestParameter pv l 
                Apart, ut ,vt
        
        | IntersectionParamInfinite.Parallel ->
            
            let lv  =  l.Direction 
            let llv = ll.Direction            
            //make a new line k that is oriented the same way:
            let flip = lv*llv < 0.0 
            let k = if flip then ll.Reversed else ll
            let l0k0 = Vc.create(l.From,k.From)
            let l0k1 = Vc.create(l.From,k.To)
            let l1k0 = Vc.create(l.To  ,k.From)
            let l1k1 = Vc.create(l.To  ,k.To)
            // check if vectors between lines are in same orientation as line:
            let d00 = lv * l0k0 > 0.
            let d01 = lv * l0k1 > 0.
            let d11 = lv * l1k1 > 0.
            let d10 = lv * l1k0 > 0.  

            // there are many valid parameters
            // Parameters are at the end or start of line l when possible
            // Full logic:
            //let u, v = 
            //    if   not d00 && not d01 && not d10 && not d11 then  
            //                                                        Printfn.gray "// l starts after k ends"
            //                                                        0.0, if flip then 0.0 else  1.0 
            //    elif not d00 &&     d01 && not d10 && not d11 then  
            //                                                        Printfn.gray "// k is overlapping l start"
            //                                                        0.0, ll.ClosestParameter(l.From) 
            //    elif     d00 &&     d01 && not d10 && not d11 then  
            //                                                        Printfn.gray "// k is on both ends shorter than l  "
            //                                                        l.ClosestParameter(ll.From), 0.0                 
            //    elif not d00 &&     d01 && not d10 &&     d11 then  
            //                                                        Printfn.gray "// l is on both ends shorter than k  "
            //                                                        0.0  , ll.ClosestParameter(l.From)               
            //    elif     d00 &&     d01 && not d10 &&     d11 then  
            //                                                        Printfn.gray "// k is overlapping l end "
            //                                                        1.0  , ll.ClosestParameter(l.To)   
            //    elif     d00 &&     d01 &&     d10 &&     d11 then  
            //                                                        Printfn.gray "// k starts after l ends"
            //                                                        1.0, if flip then 1.0 else  0.0 
            //    else failwith "Bad case in intersectLineParametersInfinite"
            //IntersectionKind.Parallel , u, v 
            
            // Optimized logic: 
            if d01 then 
                if d10 then  
                    IntersectionKind.Parallel ,1.0, if flip then 1.0 else  0.0   // k starts after l ends 
                else 
                    if d00 then 
                        if d11 then IntersectionKind.Parallel ,1.0 , ll.ClosestParameter(l.To)   // k is overlapping l end  
                        else        IntersectionKind.Parallel ,l.ClosestParameter(ll.From), 0.0  // k is on both ends shorter than l 
                    else  
                        IntersectionKind.Parallel ,0.0 , ll.ClosestParameter(l.From) // k is overlapping l start // l is on both ends shorter than k  
            else  
                IntersectionKind.Parallel ,0.0 , if flip then 0.0 else  1.0  // l starts after k ends 
        
        
        | IntersectionParamInfinite.Coincident ->
            // cases Overlapping | Continuation  | CoincidentApart | Identical
            let lv  =  l.Direction // Vec(ax,ay,az)
            let llv = ll.Direction //Vec(bx,by,bz) 
            let flip = lv*llv < 0.0 
            let k = if flip then ll.Reversed else ll
            let l0k0 = Vc.create(l.From,k.From)
            let l0k1 = Vc.create(l.From,k.To)
            let l1k0 = Vc.create(l.To  ,k.From)
            let l1k1 = Vc.create(l.To  ,k.To)
            let z00 = l0k0.LengthSq < 1e-12 // the sqrt of 1e-6
            let z01 = l0k1.LengthSq < 1e-12 
            let z10 = l1k0.LengthSq < 1e-12 
            let z11 = l1k1.LengthSq < 1e-12 
            
            if z00 && z11 then 
                if flip then IdenticalFlipped , 0.0 , 0.0 
                else         Identical        , 0.0 , 1.0
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
                //                                                    Printfn.gray "// l starts after k ends"
                //                                                    CoincidentApart,0.0, if flip then 0.0 else  1.0 
                //elif not d00 &&     d01 && not d10 && not d11 then  
                //                                                    Printfn.gray "// k is overlapping l start"
                //                                                    Overlapping    ,0.0, if flip then 0.0 else  1.0 
                //elif     d00 &&     d01 && not d10 && not d11 then  
                //                                                    Printfn.gray "// k is on both ends shorter than l  "
                //                                                    Overlapping    ,l.ClosestParameter(ll.From), 1.0                 
                //elif not d00 &&     d01 && not d10 &&     d11 then  
                //                                                    Printfn.gray "// l is on both ends shorter than k  "
                //                                                    Overlapping    ,0.0  , ll.ClosestParameter(l.To)               
                //elif     d00 &&     d01 && not d10 &&     d11 then  
                //                                                    Printfn.gray "// k is overlapping l end "
                //                                                    Overlapping    ,1.0  , if flip then 1.0 else  0.0   
                //elif     d00 &&     d01 &&     d10 &&     d11 then  
                //                                                    Printfn.gray "// k starts after l ends"
                //                                                    CoincidentApart,1.0, if flip then 1.0 else  0.0 
                //else failwith "Bad case in intersectLineParametersInfinite"

                if d01 then  
                    if d10 then 
                        CoincidentApart, 1.0  , if flip then 1.0 else  0.0   // k starts after l ends
                    else  
                        if d11 then 
                            if  d00 then Overlapping  , 1.0  , if flip then 1.0 else  0.0  // k is overlapping l end 
                            else         Overlapping  , 0.0  , ll.ClosestParameter(l.To)   // l is on both ends shorter than k              
                        else
                            if d00 then Overlapping , l.ClosestParameter(ll.From), 1.0   // k is on both ends shorter than l               
                            else        Overlapping , 0.0 , if flip then 0.0 else  1.0   // k is overlapping l start 
                else 
                    CoincidentApart, 0.0 , if flip then 0.0 else  1.0    // l starts after k ends
                

    
    /// Returns the intersection kind and the points at which two (finite) 2D Lines are intersecting or closest to each other.
    /// The results are both between 0.0 and 1.0.
    /// For parallel and coincident lines it still returns two points, in the middle of their overlap, or distance apart.
    /// First point is on l, second point is on ll.
    /// The possible result cases are:  
    /// 
    /// | Intersecting : The finite lines are intersecting each other in one point.
    /// | IntersectingEndsBoth: The finite lines are intersecting each other at one of their end or start points point. 
    /// | IntersectingEndsFirst: The finite lines are intersecting. The first line is touching the second one with its end or start point.       
    /// | IntersectingEndsSecond:  The finite lines are intersecting. The second line is touching the first one with its end or start point.        
    /// 
    /// | Skew: (only applicable for 3D lines)
    /// 
    /// | Apart: The finite lines are not intersecting.
    /// At least one of the points of closets points would be outside of  the range 0.0 and 1.0.
    /// The returned points  still indicate where the finite lines are closest to each other.
    /// 
    ///------- Parallel and other special cases for finite lines: ---------------
    /// 
    /// | Parallel : The finite lines are parallel. Within 0.25 degrees.
    /// The returned points are in the middle of their overlap, 
    /// or in the middle of their distance apart.
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
    /// | Identical: The Lines are identical , in orientation too with in 1-e6 tolerance.
    /// The returned points still indicate where the lines start and end.
    /// 
    /// | IdenticalFlipped: The Lines are identical. But orientation is flipped.
    /// The returned points still indicate where the lines start and end.
    static member inline intersection  (l:Line2D , ll:Line2D) : IntersectionKind*Pt*Pt =  
        let k,u,v =  Line2D.intersectionParam(l,ll)
        match k with        
        | Intersecting | IntersectingEndsBoth | IntersectingEndsFirst | IntersectingEndsSecond 
        | Continuation | ContinuationFlipped -> 
            let p = l.EvaluateAt u
            k,p,p // return same point twice ?
        | Skew | Apart  | IntersectionKind.Parallel 
        | Overlapping | CoincidentApart | Identical| IdenticalFlipped -> 
            let a  =  l.EvaluateAt u 
            let b  = ll.EvaluateAt v 
            k,a,b            


    /// Returns the single points where these two finite lines actually intersect each other. Or None
    /// Unless the coincident lines ar continuing each other and just touching in one point.      
    static member intersectionPoint (l:Line2D , ll:Line2D) : option<Pt> = 
        let k,u,v =  Line2D.intersectionParam(l,ll)
        match k with        
        | Intersecting | IntersectingEndsBoth | IntersectingEndsFirst | IntersectingEndsSecond 
        | Continuation | ContinuationFlipped -> 
            Some(l.EvaluateAt u)
        | Skew | Apart  | IntersectionKind.Parallel 
        | Overlapping | CoincidentApart | Identical| IdenticalFlipped ->              
            // Raises an Exception if lines are apart, parallel or coincident. 
            //FsExGeoException.Raise "FsEx.Geo.Line2D.intersectionPoint: Lines are '%A' l: \r\n%O and ll: \r\n%O" k(l,ll)
            None

    /// Returns the distance between two finite 2D lines.
    /// For parallel lines the distance is calculate form the actual finit elements. (like in the other cases.) 
    /// So it is maybe bigger than the parallel offset.
    /// For Coincident and intersecting lines it always returns 0.0 even if there is actually a distance less than 1-e6.
    static member inline distanceBetweenLines(l,ll) : float=
        let k,u,v =  Line2D.intersectionParam(l,ll)
        match k with        
        | Intersecting | IntersectingEndsBoth | IntersectingEndsFirst | IntersectingEndsSecond 
        | Continuation | ContinuationFlipped | Overlapping   | Identical| IdenticalFlipped -> 
            0.0           
        | Skew | Apart  | IntersectionKind.Parallel | CoincidentApart ->
            let a  =  l.EvaluateAt u 
            let b  = ll.EvaluateAt v 
            Pt.distance a b
        




