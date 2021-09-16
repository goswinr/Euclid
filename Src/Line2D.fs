namespace FsEx.Geo

open System
open FsEx.Geo.Util

/// A finite line in 2D. Represented by a 2D start and 2D end point.
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
        sprintf "FsEx.Geo.Line2D from %s, %s to %s, %s Length %s" 
            (Format.float ln.FromX) 
            (Format.float ln.FromY)
            (Format.float ln.ToX)
            (Format.float ln.ToY)
            (Format.float ln.Length)

    /// Format 2D Line into string from X and Y for start and end points. 
    /// Using nice floating point number formatting .
    /// But without full type name as in v.ToString()
    member ln.AsString = 
        sprintf "%s, %s to %s, %s" 
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

    /// Returns a Line2D from point at Parameter a to point at Parameter b
    member inline ln.Segment(a, b) = 
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        Line2D( ln.FromX + x*a,
                ln.FromY + y*a,
                ln.FromX + x*b,
                ln.FromY + y*b) 
    
    /// Returns a Line2D moved by a vector.
    member inline ln.Translate (v:Vc) = 
        Line2D( ln.FromX+v.X,
                ln.FromY+v.Y,
                ln.ToX+v.X,
                ln.ToY+v.Y) 

    /// Returns a Line2D moved by a given distance in X direction.
    member inline ln.TranslateX (distance:float) = 
        Line2D( ln.FromX+distance,
                ln.FromY,
                ln.ToX+distance,
                ln.ToY)

    /// Returns a Line2D moved by a given distance in Y direction.
    member inline ln.TranslateY (distance:float) = 
        Line2D( ln.FromX,
                ln.FromY+distance,
                ln.ToX,
                ln.ToY+distance)

    /// Assumes Line2D to be infinite!
    /// Returns the parameter at which a point is closest to the infinit line.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finit line.
    member ln.ClosestParameterInfinite (p:Pt) = 
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY        
        let lenSq = x*x + y*y 
        if lenSq < 1e-18 then // corresponds to a line Length of 1e-9
            FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.ClosestParameterInfinite failed on very short line %O %O" ln p
        let u = ln.FromX-p.X
        let v = ln.FromY-p.Y        
        let dot = x*u + y*v 
        -dot / lenSq

    /// Return the parameter at which a point is closest to the (finite) line.
    /// The result is between 0.0 and 1.0.
    member inline ln.ClosestParameter (p:Pt) = 
        ln.ClosestParameterInfinite(p)
        |> Util.clamp01  
        
    /// Assumes Line2D to be infinite!
    /// Returns closest point on infinite Line.
    member ln.ClosestPointInfinit (p:Pt) = 
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY        
        let lenSq = x*x + y*y 
        if lenSq < 1e-18 then // corresponds to a line Length of 1e-9
            FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.ClosestPoint failed on very short line %O %O" ln p
        let u = ln.FromX-p.X
        let v = ln.FromY-p.Y        
        let dot = x*u + y*v 
        let t = -dot/lenSq        
        let x' = ln.FromX + x*t
        let y' = ln.FromY + y*t        
        Pt(x', y')

    /// Returns closest point on (finite) Line.
    member ln.ClosestPoint (p:Pt) = 
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY         
        let lenSq = x*x + y*y 
        if lenSq < 1e-18 then // corresponds to a line Length of 1e-9
            FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.ClosestPoint failed on very short line %O %O" ln p
        let u = ln.FromX-p.X
        let v = ln.FromY-p.Y        
        let dot = x*u + y*v 
        let t = -dot/lenSq
        if t<0.0 then
            Pt(ln.FromX, ln.FromY)
        elif t>1.0 then
            Pt(ln.ToX, ln.ToY)
        else
            let x' = ln.FromX + x*t
            let y' = ln.FromY + y*t            
            Pt(x', y')

    /// Assumes Line2D to be infinite!
    /// Returns Square distance from point to infinite line.
    member ln.DistanceSqFromPointInfinite(p:Pt) =  
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY         
        let lenSq = x*x + y*y 
        if lenSq < 1e-18 then // corresponds to a line Length of 1e-9
            FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.DistanceFromPointInfinite failed on very short line %O %O" ln p
        let u = ln.FromX-p.X
        let v = ln.FromY-p.Y        
        let dot = x*u + y*v 
        let t = -dot/lenSq    
        let x' = ln.FromX + x*t
        let y' = ln.FromY + y*t        
        let u' = x'-p.X
        let v' = y'-p.Y        
        u'*u' + v'*v' 
    /// Assumes Line2D to be infinite!
    /// Returns distance from point to infinite line.
    member inline ln.DistanceFromPointInfinite(p:Pt) =   
        ln.DistanceSqFromPointInfinite(p) |> sqrt   
    
    /// Returns Square distance from point to (finite) line.
    member ln.DistanceSqFromPoint(p:Pt) =  
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY         
        let lenSq = x*x + y*y 
        if lenSq < 1e-18 then // corresponds to a line Length of 1e-9
            FsExGeoDivByZeroException.Raise "FsEx.Geo.Line2D.DistanceSqFromPoint failed on very short line %O %O" ln p
        let u = ln.FromX-p.X
        let v = ln.FromY-p.Y
        let dot = x*u + y*v 
        let t = -dot/lenSq
        if t<0.0 then
            u*u + v*v 
        elif t>1.0 then
            let u = ln.ToX-p.X
            let v = ln.ToY-p.Y
            u*u + v*v 
        else
            let x' = ln.FromX + x*t
            let y' = ln.FromY + y*t
            let u = x'-p.X
            let v = y'-p.Y
            u*u + v*v 

    /// Returns distance from point to (finite) line.
    member inline ln.DistanceFromPoint(p:Pt) =  
        ln.DistanceSqFromPoint(p) |> sqrt    

    //-------------------------------------------------------------------
    //------------------------static members-----------------------------
    //-------------------------------------------------------------------

    /// Creates a line starting at World Origin and going to along the given Vector.
    static member inline createFromVec (v:Vc) = Line2D(0.,0.,v.X,v.Y)

    /// Creates a line starting at given Point and going to along the given Vector.
    static member inline createFromPntAndVec (p:Pt,v:Vc) =  Line2D(p.X, p.Y, p.X+v.X, p.Y+v.Y) 

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
    
    /// Translate a Line2D by a vector. (same as Line2D.move)
    static member inline translate (v:Vc) (ln:Line2D) = ln.Translate(v)

    /// Returns a Line2D moved by a given distance in X direction.
    static member inline translateX (distance:float) (ln:Line2D) = ln.TranslateX(distance)

    /// Returns a Line2D moved by a given distance in Y direction.
    static member inline translateY (distance:double) (ln:Line2D) = ln.TranslateY(distance)

    /// Translate a Line2D by a vector. (same as Line2D.translate)
    static member inline move (v:Vc) (ln:Line2D) = ln.Translate(v)

    /// Rotation a Line2D.
    static member inline rotate (r:Rotation2D) (l:Line2D) = Line2D(Pt.rotateBy r l.From, Pt.rotateBy r l.To) 
    
    /// Rotation a Line2D around a given Center.
    static member inline rotateOn (cen:Pt) (r:Rotation2D) (l:Line2D) = Line2D(Pt.rotateWithCenterBy cen r l.From, Pt.rotateWithCenterBy cen r l.To) 

    /// Assumes Line2D to be infinite!
    /// Returns the parameter at which a point is closest to the infinit line.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finit line.
    static member inline closestParameterInfinite (p:Pt) (ln:Line2D)  = ln.ClosestParameterInfinite p


    /// Return the parameter at which a point is closest to the (finite) line.
    /// The result is between 0.0 and 1.0.
    static member inline closestParameter (p:Pt) (ln:Line2D)  = ln.ClosestParameter p 

    /// Assumes Line2D to be infinite!
    /// Returns closest point on infinite Line.
    static member inline closestPointInfinit (p:Pt) (ln:Line2D)  = ln.ClosestPointInfinit p
        

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
    
    /// Calculates the dot product of two lines. 
    /// Then checks if it is positive to see if the Lines are oriented the same way.
    static member inline areInSameDirection (ll:Line2D) (l:Line2D) = 
        let dot = (l.ToX-l.FromX)*(ll.ToX-ll.FromX) + (l.ToY-l.FromY)*(ll.ToY-ll.FromY) 
        dot > 0.0 

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
    
    (*
    

        TODO !!!!!!!! adapt:

    /// Checks if two finit 2D lines do intersect.
    /// It first calculates the signed volume of the Parallelepiped define by three vectors from the four corners of the lines.
    /// Then it checks if it is smaller than given volumeTolerance fo Parallelepiped.
    /// Using the VolumeTolerance makes a positive result not only dependent on the distance of the two lines from each other but also on their lengths.
    /// This is a fast check for intersection.
    /// If you need an exact intersection test purely based on distance use Line2D.intersectLine
    static member inline doLinesIntersectTol volumeTolerance (l:Line2D) (ll:Line2D) =  
        let ux = l.ToX-l.FromX
        let uy = l.ToY-l.FromY
        let vx = ll.FromX-l.FromX
        let vy = ll.FromY-l.FromY
        let wx = ll.ToX-l.FromX
        let wy = ll.ToY-l.FromY
        let volume = ux*vy*wz + + vx*wy*uz + wx*uy*vz - wx*vy*uz - vx*uy*wz - ux*wy*vz // determinate of 3 vectors
        abs(volume) < volumeTolerance

     /// Checks if two finit 2D lines do intersect.
    /// It first calculates the signed volume of the Parallelepiped define by three vectors from the four corners of the lines.
    /// Then it checks if it is smaller than Util.zeroLengthTol.
    /// Using the volume of the Parallelepiped makes a positive result not only dependent on the distance of the two lines from each other but also on their lengths.
    /// This is a fast check for intersection.
    /// If you need an exact intersection test purely based on distance use Line2D.intersectLine
    static member inline doLinesIntersect (l:Line2D) (ll:Line2D) =  
        Line2D.doLinesIntersectTol Util.zeroLengthTol l ll

    /// Assumes Lines to be infinite!
    /// Returns the parameters at which two infinite 2D Lines are closest to each other.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finit line.    
    /// Fails on parallel Lines.
    /// First parameter is on l, second parameter is on ll.
    static member inline intersectLineParametersInfinite (l:Line2D) (ll:Line2D) =        
         // TODO can be optimized by inlining floats.
        let ba, cd, bd = l.From - l.To , ll.From - ll.To , l.From - ll.From
        //if ba.IsTiny 1e-12 then FsExGeoException.Raise "FsEx.Geo.Line2D.intersectLine: l too short:  %O other: %O" l ll
        //if cd.IsTiny 1e-12 then FsExGeoException.Raise "FsEx.Geo.Line2D.intersectLine: ll too short: %O other: %O" ll l 
        let x1, y1, z1 = - ba*ba ,   cd*ba ,   bd*ba 
        let x2, y2, z2 =   ba*cd , - cd*cd , - bd*cd
        let bAse = x2*y1 - x1*y2        
        if abs bAse < 1e-12 then FsExGeoException.Raise "FsEx.Geo.Line2D.intersectLine: Lines are parallel l: %O and ll: %O" l ll 
        else (y2*z1 - y1*z2)/bAse , -(x2*z1 - x1*z2)/bAse

    /// Returns the parameters at which two (finite) 2D Lines are closest to each other.
    /// The results are both between 0.0 and 1.0.
    /// Fails on parallel Lines.
    /// First parameter is on l, second parameter is on ll.
    static member inline intersectLineParameters (l:Line2D) (ll:Line2D) = 
        let a,b = Line2D.intersectLineParametersInfinite l ll
        clamp01 a, clamp01 b

    /// Assumes Lines to be infinite!    
    /// Returns the two points where these two infinite lines are closest to each other.
    /// Fails on parallel Lines.
    /// First point is on l, second point is on ll.
    static member inline intersectLinesInfinite (l:Line2D) (ll:Line2D) =        
        let a,b = Line2D.intersectLineParametersInfinite l ll
        l.EvaluateAt a , ll.EvaluateAt b 

        
    /// Returns the two points where the two (finit) lines  are closest to each other.
    /// Fails on parallel Lines.
    /// First point is on l, second point is on ll.
    static member inline intersectLines (l:Line2D) (ll:Line2D) =    
        let a,b = Line2D.intersectLineParameters l ll
        l.EvaluateAt a , ll.EvaluateAt b 

    /// Assumes Lines to be infinite!    
    /// Returns the singe points where these two infinite lines actually intersect each other.
    /// Fails if lines are parallel or skew by more than 1e-6 units 
    /// The returned point is exactly on ll.  
    static member intersectLinesInfiniteInOnePoint (l:Line2D) (ll:Line2D) : Pt = 
        let a,b = Line2D.intersectLinesInfinite l ll
        if Pt.distance a b > 1e-6 then FsExGeoException.Raise "FsEx.Geo.Line2D.intersectLinesInfiniteInOnePoint: Lines are not intersecting l: %O and ll: %O" l ll
        b
    
    /// Returns the single points where these two (finite) lines actually intersect each other.
    /// Fails if lines are parallel , skew, or apart by more than 1e-6 units 
    /// The returned point is exactly on ll.  
    static member intersectLinesInOnePoint (l:Line2D) (ll:Line2D) : Pt = 
        let a,b = Line2D.intersectLines l ll
        if Pt.distance a b > 1e-6 then FsExGeoException.Raise "FsEx.Geo.Line2D.intersectLinesInOnePoint: Lines are not intersecting l: %O and ll: %O" l ll
        b        
        
    /// Assumes Lines to be infinite!    
    /// Returns the distance between two infinite lines.
    /// Fails on parallel Lines.    
    static member inline distanceBetweenInfiniteLines l ll =
        // TODO make it not fail on Parallel lines !
        let a,b = Line2D.intersectLinesInfinite l ll
        Pt.distance a b
    
    /// Returns the distance between two (finite) 2D lines.
    /// Fails on parallel Lines. 
    static member inline distanceBetweenLines l ll=
        // TODO make it not fail on Parallel lines !
        let a,b = Line2D.intersectLines l ll
        Pt.distance a b 
    
    *)

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
            

    



