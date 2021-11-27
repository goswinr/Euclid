namespace FsEx.Geo

open FsEx.Geo.Util
open FsEx.Geo.LineIntersectionTypes  

/// An immutable finite line in 3D. Represented by a 3D start and 3D end point.
[<Struct;NoEquality;NoComparison>]// because its made up from floats
type Line3D =
    /// Returns the X coordinate of the start point of the line.
    val FromX:float

    /// Returns the Y coordinate of the start point of the line.
    val FromY:float

    /// Returns the Z coordinate of the start point of the line.
    val FromZ:float

    /// Returns the X coordinate of the end point of the line.
    val ToX  :float

    /// Returns the Y coordinate of the end point of the line.
    val ToY  :float

    /// Returns the Z coordinate of the end point of the line.
    val ToZ  :float
    
    //Create Line3D from 3D start point and 3D end point.
    new (a:Pnt,b:Pnt) = {FromX=a.X; FromY=a.Y; FromZ=a.Z; ToX=b.X; ToY=b.Y; ToZ=b.Z}
    
    //Create Line3D from 3D start point's x,y and z  and 3D end point's x,y and z.
    new (a,b,c,u,v,w) = {FromX=a; FromY=b; FromZ=c; ToX=u; ToY=v; ToZ=w}
    
    /// Returns the length of the line.
    member inline ln.Length = 
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        sqrt(x*x + y*y + z*z)

     /// Returns the Square length of the line.
    member inline ln.LengthSq = 
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        x*x + y*y + z*z
        
    /// Format 3D Line into string including type name, X,Y and Z for start and end points , and Length. 
    /// Using nice floating point number formatting .
    override ln.ToString() = 
        sprintf "FsEx.Geo.Line3D from X=%s| Y=%s| Z=%s to X=%s| Y=%s| Z=%s Length %s" 
            (Format.float ln.FromX) 
            (Format.float ln.FromY)
            (Format.float ln.FromZ)
            (Format.float ln.ToX)
            (Format.float ln.ToY)
            (Format.float ln.ToZ)
            (Format.float ln.Length)

    /// Format 3D Line into string from X,Y and Z for start and end points. 
    /// Using nice floating point number formatting .
    /// But without full type name as in v.ToString()
    member ln.AsString = 
        sprintf "%s, %s, %s to %s, %s, %s" 
            (Format.float ln.FromX) 
            (Format.float ln.FromY)
            (Format.float ln.FromZ)
            (Format.float ln.ToX)
            (Format.float ln.ToY)
            (Format.float ln.ToZ)


    /// The Start point of the 3D Line3D,
    member inline ln.From = Pnt(ln.FromX,ln.FromY,ln.FromZ)

    /// The End point of the 3D Line3D,
    member inline ln.To   = Pnt(ln.ToX,ln.ToY,ln.ToZ)
    
    /// Same as ln.Vector or ln.Tangent
    /// The returned vector has the same length as the Line3D.
    member inline ln.Direction = 
        Vec(ln.ToX-ln.FromX,ln.ToY-ln.FromY,ln.ToZ-ln.FromZ)
    
    /// Same as ln.Tangent or ln.Direction
    /// The returned vector has the same length as the Line3D.
    member inline ln.Vector = 
        Vec(ln.ToX-ln.FromX,ln.ToY-ln.FromY,ln.ToZ-ln.FromZ)
    
    /// Same as ln.Vector or ln.Direction 
    /// The returned vector has the same length as the Line3D.
    member inline ln.Tangent = 
        Vec(ln.ToX-ln.FromX,ln.ToY-ln.FromY,ln.ToZ-ln.FromZ)
        
    /// Returns a unit vector of the line Direction
    member inline ln.UnitTangent = 
        UnitVec.create(ln.ToX-ln.FromX,ln.ToY-ln.FromY,ln.ToZ-ln.FromZ) 

    /// Check if the line has same starting and ending point.
    member inline ln.IsZeroLength = 
        ln.ToX = ln.FromX &&
        ln.ToY = ln.FromY &&
        ln.ToZ = ln.FromZ    
    
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
        let z = ln.FromZ + (ln.ToZ-ln.FromZ)*p
        Pnt(x,y,z)    
    
    /// Returns the midpoint of the line,
    member inline ln.Mid = 
        let x = (ln.ToX+ln.FromX)*0.5
        let y = (ln.ToY+ln.FromY)*0.5
        let z = (ln.ToZ+ln.FromZ)*0.5
        Pnt(x,y,z) 
    
    /// returns the Line3D reversed.
    member inline ln.Reversed = 
        Line3D(ln.ToX,ln.ToY,ln.ToZ,ln.FromX,ln.FromY,ln.FromZ)  

    /// returns the lines Bounding Box
    member inline ln.BoundingBox = 
        BBox.create ( ln.From, ln.To)

    /// Returns a Line3D from point at Parameter a to point at Parameter b
    member inline ln.Segment(a, b) = 
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        Line3D( ln.FromX + x*a,
                ln.FromY + y*a,
                ln.FromZ + z*a,
                ln.FromX + x*b,
                ln.FromY + y*b,
                ln.FromZ + z*b) 
    
    /// Returns a Line3D moved by a vector.
    member inline ln.Move (v:Vec) = 
        Line3D( ln.FromX+v.X,
                ln.FromY+v.Y,
                ln.FromZ+v.Z,
                ln.ToX+v.X,
                ln.ToY+v.Y,
                ln.ToZ+v.Z)

    /// Returns a Line3D moved by a given distance in X direction.
    member inline ln.MoveX (distance:float) = 
        Line3D( ln.FromX+distance,
                ln.FromY,
                ln.FromZ,
                ln.ToX+distance,
                ln.ToY,
                ln.ToZ)
    /// Returns a Line3D moved by a given distance in Y direction.
    member inline ln.MoveY (distance:float) = 
        Line3D( ln.FromX,
                ln.FromY+distance,
                ln.FromZ,
                ln.ToX,
                ln.ToY+distance,
                ln.ToZ)
                
    /// Returns a Line3D moved by a given distance in Z direction.
    member inline ln.MoveZ (distance:float) = 
        Line3D( ln.FromX,
                ln.FromY,
                ln.FromZ+distance,
                ln.ToX,
                ln.ToY,
                ln.ToZ+distance) 

    /// Assumes Line3D to be infinite.
    /// Returns the parameter at which a point is closest to the infinite line.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finite line.
    /// Fails on curves shorter than  1e-9 units. (ln.ClosestParameter does not)
    member ln.ClosestParameterInfinite (p:Pnt) = 
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let x = ln.FromX - ln.ToX
        let y = ln.FromY - ln.ToY
        let z = ln.FromZ - ln.ToZ
        let lenSq = x*x + y*y + z*z
        if lenSq < 1e-18 then // corresponds to a line Length of 1e-9
            FsExGeoDivByZeroException.Raise "FsEx.Geo.Line3D.ClosestParameterInfinite failed on very short line %O %O" ln p
        let u = ln.FromX-p.X
        let v = ln.FromY-p.Y
        let w = ln.FromZ-p.Z
        let dot = x*u + y*v + z*w
        dot / lenSq

    /// Return the parameter at which a point is closest to the (finite) line.
    /// The result is between 0.0 and 1.0.
    /// Does not fails on very short curves.
    member inline ln.ClosestParameter (p:Pnt) = 
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let x = ln.FromX - ln.ToX
        let y = ln.FromY - ln.ToY
        let z = ln.FromZ - ln.ToZ
        let u = ln.FromX-p.X
        let v = ln.FromY-p.Y
        let w = ln.FromZ-p.Z
        let dot = x*u + y*v + z*w
        let lenSq = x*x + y*y + z*z
        if lenSq < 1e-18 then // corresponds to a line Length of 1e-9
            if dot < 0.0 then 0.0 else 1.0
        else
            dot / lenSq |> Util.clampBetweenZeroAndOne
        
        
        
    /// Assumes Line3D to be infinite.
    /// Returns closest point on infinite Line.
    /// Fails on curves shorter than  1e-9 units. (ln.ClosestPoint does not.)
    member ln.ClosestPointInfinite (p:Pnt) = 
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let x = ln.FromX - ln.ToX
        let y = ln.FromY - ln.ToY
        let z = ln.FromZ - ln.ToZ
        let lenSq = x*x + y*y + z*z
        if lenSq < 1e-18 then // corresponds to a line Length of 1e-9
            FsExGeoDivByZeroException.Raise "FsEx.Geo.Line3D.ClosestPoint failed on very short line %O %O" ln p
        let u = ln.FromX-p.X
        let v = ln.FromY-p.Y
        let w = ln.FromZ-p.Z
        let dot = x*u + y*v + z*w
        let t = dot/lenSq        
        let x' = ln.FromX - x*t
        let y' = ln.FromY - y*t
        let z' = ln.FromZ - z*t
        Pnt(x', y', z')

    /// Returns closest point on (finite) Line.
    /// Does not fails on very short curves. 
    member ln.ClosestPoint (p:Pnt) = 
        ln.EvaluateAt(ln.ClosestParameter(p))

    /// Assumes Line3D to be infinite.
    /// Returns Square distance from point to infinite line.
    /// Fails on curves shorter than  1e-9 units. (ln.DistanceSqFromPoint does not.)
    member ln.DistanceSqFromPointInfinite(p:Pnt) =  
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let x = ln.FromX - ln.ToX
        let y = ln.FromY - ln.ToY
        let z = ln.FromZ - ln.ToZ
        let lenSq = x*x + y*y + z*z
        if lenSq < 1e-18 then // corresponds to a line Length of 1e-9
            FsExGeoDivByZeroException.Raise "FsEx.Geo.Line3D.DistanceFromPointInfinite failed on very short line %O %O" ln p
        let u = ln.FromX - p.X
        let v = ln.FromY - p.Y
        let w = ln.FromZ - p.Z
        let dot = x*u + y*v + z*w
        let t = dot/lenSq    
        let x' = ln.FromX - x*t
        let y' = ln.FromY - y*t
        let z' = ln.FromZ - z*t
        let u' = x' - p.X
        let v' = y' - p.Y
        let w' = z' - p.Z
        u'*u' + v'*v' + w'*w'

    /// Assumes Line3D to be infinite.
    /// Returns distance from point to infinite line.
    /// Fails on curves shorter than  1e-9 units. (ln.DistanceFromPoint does not.)
    member inline ln.DistanceFromPointInfinite(p:Pnt) =   
        ln.DistanceSqFromPointInfinite(p) |> sqrt   
    
    /// Returns Square distance from point to finite line.
    member ln.DistanceSqFromPoint(p:Pnt) =  
        p
        |> ln.ClosestParameter
        |> ln.EvaluateAt
        |> Pnt.distanceSq p

    /// Returns distance from point to (finite) line.
    member inline ln.DistanceFromPoint(p:Pnt) =  
        ln.DistanceSqFromPoint(p) |> sqrt    

    /// Checks if the angle between the two 3D lines is less than 180 degrees.
    /// Calculates the dot product of two 3D lines. 
    /// Then checks if it is positive.
    member inline ln.MatchesOrientation180  (l:Line3D) = 
        let dot = (l.ToX-l.FromX)*(ln.ToX-ln.FromX) + (l.ToY-l.FromY)*(ln.ToY-ln.FromY) + (l.ToZ-l.FromZ)*(ln.ToZ-ln.FromZ) 
        dot > 0.0  

    /// Checks if the angle between the a 2D line and a 2D vector is less than 180 degrees.
    /// Calculates the dot product of both. 
    /// Then checks if it is positive.
    member inline ln.MatchesOrientation180  (v:Vec) = 
        let dot = v.X*(ln.ToX-ln.FromX) + v.Y*(ln.ToY-ln.FromY) + v.Z*(ln.ToZ-ln.FromZ)
        dot > 0.0 

    /// Checks if the angle between the a 2D line and a 2D unit vector is less than 180 degrees.
    /// Calculates the dot product of both. 
    /// Then checks if it is positive.
    member inline ln.MatchesOrientation180  (v:UnitVec) = 
        let dot = v.X*(ln.ToX-ln.FromX) + v.Y*(ln.ToY-ln.FromY) + v.Z*(ln.ToZ-ln.FromZ)
        dot > 0.0 

    /// Checks if the angle between the two 3D lines is less than 90 degrees.   
    /// Calculates the dot product of the unit vectors of the two 3D lines. 
    /// Then checks if it is bigger than 0.707107 (cosine of  90 degrees).
    member inline ln.MatchesOrientation90  (l:Line3D) = 
        let dot = ln.UnitTangent*l.UnitTangent
        dot > 0.707107
        
    /// Checks if two 3D lines are parallel.
    /// Ignores the line orientation.
    /// The default angle tolerance is 0.25 degrees.  
    /// This tolerance can be customized by an optional minium cosine value.
    /// See FsEx.Geo.Cosine module.
    /// Fails on lines shorter than 1e-12.    
    member inline ln.IsParallelTo( other:Line3D, [<OPT;DEF(Cosine.``0.25``)>] minCosine ) = 
        let a = ln.Vector
        let b = other.Vector
        let sa = a.LengthSq
        if sa < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Line3D.IsParallelTo: Line3D 'ln' is too short: %s. 'other':%s " a.AsString b.AsString
        let sb = b.LengthSq
        if sb < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Line3D.IsParallelTo: Line3D 'other' is too short: %s. 'ln':%s " b.AsString a.AsString  
        let au = a * (1.0 / sqrt sa )
        let bu = b * (1.0 / sqrt sb )
        abs(bu*au) > minCosine // 0.999990480720734 = cosine of 0.25 degrees:            
        
        
    /// Checks if two 3D lines are parallel.
    /// Takes the line orientation into account too.
    /// The default angle tolerance is 0.25 degrees.  
    /// This tolerance can be customized by an optional minium cosine value.
    /// See FsEx.Geo.Cosine module.
    /// Fails on lines shorter than 1e-12.       
    member inline ln.IsParallelAndOrientedTo  (other:Line3D, [<OPT;DEF(Cosine.``0.25``)>] minCosine ) = 
        let a = ln.Vector
        let b = other.Vector
        let sa = a.LengthSq
        if sa < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Line3D.IsParallelAndOrientedTo: Line3D 'ln' is too short: %s. 'other':%s " a.AsString b.AsString
        let sb = b.LengthSq
        if sb < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Line3D.IsParallelAndOrientedTo: Line3D 'other' is too short: %s. 'ln':%s " b.AsString a.AsString 
        let au = a * (1.0 / sqrt sa )
        let bu = b * (1.0 / sqrt sb )
        bu*au >  minCosine // 0.999990480720734 = cosine of 0.25 degrees:    
        
    
    /// Checks if two 3D lines are perpendicular to each other.
    /// The default angle tolerance is 89.75 to  90.25 degrees.   
    /// This tolerance can be customized by an optional minium cosine value.
    /// The default cosine is 0.0043633 ( = 89.75 deg )
    /// See FsEx.Geo.Cosine module.
    /// Fails on lines shorter than 1e-12.  
    member inline ln.IsPerpendicularTo (other:Line3D, [<OPT;DEF(Cosine.``89.75``)>] maxCosine ) = 
        let a = ln.Vector
        let b = other.Vector
        let sa = a.LengthSq
        if sa < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Line3D.IsPerpendicularTo: Line3D 'ln' is too short: %s. 'other':%s " a.AsString b.AsString
        let sb = b.LengthSq
        if sb < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Line3D.IsPerpendicularTo: Line3D 'other' is too short: %s. 'ln':%s " b.AsString a.AsString 
        let au = a * (1.0 / sqrt sa )
        let bu = b * (1.0 / sqrt sb )
        let d = bu*au            
        -maxCosine < d && d  < maxCosine // = cosine of 98.75 and 90.25 degrees  
            

    
    //-------------------------------------------------------------------
    //------------------------static members-----------------------------
    //-------------------------------------------------------------------

    
    /// Checks if two 3D Lines are equal within tolerance.
    /// Identical Lines in opposite directions are not considered equal.
    static member equals tol (a:Line3D) (b:Line3D) =
        let tt = tol*tol
        Pnt.distanceSq a.From b.From < tt &&
        Pnt.distanceSq a.To b.To < tt

    /// Creates a 2D line from 3D line. Ignoring Z value.
    static member inline toLine2D (ln:Line3D) = Line2D( ln.FromX,ln.FromY,ln.ToX,ln.ToY)

    /// Creates a 3D line from 2D line. Setting Z to 0.0
    static member inline ofLine2D (ln:Line2D) = Line3D( ln.FromX,ln.FromY,0.,ln.ToX,ln.ToY,0.)
    
    /// Creates a 3D line from 2D line. Setting Z to given value.
    static member inline ofLine2DwithZ (zLevel) (ln:Line2D) = Line3D( ln.FromX,ln.FromY,zLevel,ln.ToX,ln.ToY,zLevel)

    /// Creates a line starting at World Origin and going to along the given Vector.
    static member inline createFromVec (v:Vec) = Line3D(0.,0.,0.,v.X,v.Y,v.Z)

    /// Creates a line starting at given Point and going to along the given Vector.
    static member inline createFromPntAndVec (p:Pnt,v:Vec) =  Line3D(p.X, p.Y, p.Z, p.X+v.X, p.Y+v.Y, p.Z+v.Z) 

    /// Returns the Start point of the line. Same as Line3D.from
    static member inline start (l:Line3D) = l.From
    
    /// Returns the Start point of the line. Same as Line3D.start
    static member inline from (l:Line3D) = l.From

    /// Returns the Start point's X coordinate of the line.
    static member inline fromX (l:Line3D) = l.FromX
    
    /// Returns the Start point's Y coordinate of the line.
    static member inline fromY (l:Line3D) = l.FromY

    /// Returns the Start point's Z coordinate of the line.
    static member inline fromZ (l:Line3D) = l.FromZ
    
    /// Returns the End point of the line. Same as Line3D.to'
    static member inline ende (l:Line3D) = l.To
    
    /// Returns the End point of the line. Same as Line3D.ende
    static member inline to' (l:Line3D) = l.To

    /// Returns the End point's X coordinate of the line.
    static member inline toX (l:Line3D) = l.ToX

    /// Returns the End point's Y coordinate of the line.
    static member inline toY (l:Line3D) = l.ToY

    /// Returns the End point's Z coordinate of the line.
    static member inline toZ (l:Line3D) = l.ToZ

    /// Set Line3D start point, returns a new line. 
    static member inline setStart (pt:Pnt) (ln:Line3D) = 
        Line3D( pt.X, pt.Y, pt.Z, ln.ToX, ln.ToY, ln.ToZ )    
    
    /// Set Line3D end point, returns a new line.     
    static member inline setEnd (pt:Pnt) (ln:Line3D) = 
        Line3D( ln.FromX, ln.FromY, ln.FromZ, pt.X, pt.Y, pt.Z )  

    /// Same as Line3D.vector or Line3D.tangent
    /// The returned vector has the same length as the Line3D.
    static member inline direction (ln:Line3D) = 
        Vec(ln.ToX-ln.FromX,ln.ToY-ln.FromY,ln.ToZ-ln.FromZ)
    
    /// Same as Line3D.tangent or Line3D.direction
    /// The returned vector has the same length as the Line3D.
    static member inline vector (ln:Line3D) = 
        Vec(ln.ToX-ln.FromX,ln.ToY-ln.FromY,ln.ToZ-ln.FromZ)
    
    /// Same as Line3D.vector or Line3D.direction 
    /// The returned vector has the same length as the Line3D.
    static member inline tangent (ln:Line3D) = 
        Vec(ln.ToX-ln.FromX,ln.ToY-ln.FromY,ln.ToZ-ln.FromZ)
        
    /// Returns a unit vector of the line Direction
    static member inline unitTangent (ln:Line3D) = 
        UnitVec.create(ln.ToX-ln.FromX,ln.ToY-ln.FromY,ln.ToZ-ln.FromZ)

    /// Returns the length of the line.
    static member inline length (l:Line3D) = l.Length

    /// Returns the Square length of the line.
    static member inline lengthSq (l:Line3D) = l.LengthSq  

    /// Check if the line has same starting and ending point.
    static member inline isZeroLength (l:Line3D) = l.IsZeroLength
    
    /// Check if line is shorter than tolerance.
    static member inline isTiny tol (l:Line3D) = l.Length < tol        
    
    /// Check if line is shorter than Squared tolerance.
    static member inline isTinySq tol (l:Line3D) = l.LengthSq < tol

    /// Evaluate line at a given parameter ( parameters 0.0 to 1.0 are on the line )
    static member inline evaluateAt t (ln:Line3D)  = ln.EvaluateAt t
    
    /// Get point at center of line
    static member inline mid (ln:Line3D) = ln.Mid

    /// Reverse or flip  the 3D Line  (same as Line3D.flip)
    static member inline reverse (ln:Line3D) = ln.Reversed

    /// Reverse or flip  the 3D Line  (same as Line3D.reverse)
    static member inline flip (ln:Line3D) = ln.Reversed
    
    /// Returns new 3D Line  from point at Parameter a to point at Parameter b
    static member inline segment a b (ln:Line3D) = ln.Segment (a, b)
    
    /// Move a 3D Line  by a vector. (same as Line3D.move)
    static member inline translate (v:Vec) (ln:Line3D) = ln.Move(v)

    /// Returns a 3D Line  moved by a given distance in X direction.
    static member inline moveX (distance:float) (ln:Line3D) = ln.MoveX(distance)

    /// Returns a 3D Line  moved by a given distance in Y direction.
    static member inline moveY (distance:double) (ln:Line3D) = ln.MoveY(distance)
                
    /// Returns a 3D Line  moved by a given distance in Z direction.
    static member inline moveZ (distance:double) (ln:Line3D) = ln.MoveZ(distance)

    /// Move a 3D Line  by a vector. (same as Line3D.translate)
    static member inline move (v:Vec) (ln:Line3D) = ln.Move(v)

    /// Applies a 4x4 transformation matrix
    static member inline transform (m:Matrix) (l:Line3D) = Line3D(Pnt.transform m l.From, Pnt.transform m l.To)     
    
    /// Rotation a 3D Line around Z-Axis.
    static member inline rotate (r:Rotation2D) (l:Line3D) = Line3D(Pnt.rotateZBy r l.From, Pnt.rotateZBy r l.To) 
    
    /// Rotation a 3D Line  round given Center point an a local Z Axis.
    static member inline rotateOn (cen:Pnt) (r:Rotation2D) (l:Line3D) = Line3D(Pnt.rotateZonCenterBy cen r l.From, Pnt.rotateZonCenterBy cen r l.To) 

    /// Ensure 3D Line has a positive dot product with given orientation Line
    static member inline matchOrientation (orientationToMatch:Line3D) (l:Line3D) = 
        if orientationToMatch.Direction * l.Direction < 0.0 then l.Reversed else l    

    /// Checks if the angle between the two 3D lines is less than 180 degrees.
    /// Calculates the dot product of two 3D lines. 
    /// Then checks if it is positive.
    static member inline matchesOrientation180  (l:Line3D) (ln:Line3D) = l.MatchesOrientation180 ln       

    /// Checks if the angle between the two 3D lines is less than 90 degrees.   
    /// Calculates the dot product of the unit vectors of the two 3D lines. 
    /// Then checks if it is bigger than 0.707107 (cosine of  90 degrees).
    static member inline matchesOrientation90  (l:Line3D) (ln:Line3D) = l.MatchesOrientation90 ln
        
    /// Checks if two 3D lines are parallel. Ignoring orientation
    /// Calculates the cross product of the two line vectors. (= the area of the parallelogram)
    /// And checks if it is smaller than 1e-9
    /// (NOTE: for very long lines a higher tolerance might be needed)
    static member inline isParallelTo  (l:Line3D) (ln:Line3D) =   l.IsParallelTo ln

    /// Checks if two 3D lines are parallel and orientated the same way.
    /// Calculates the cross product of the two line vectors. (= the area of the parallelogram)
    /// And checks if it is smaller than 1e-9
    /// Then calculates the dot product and checks if it is positive.
    /// (NOTE: for very long lines a higher tolerance might be needed)
    static member inline isParallelAndOrientedTo  (l:Line3D) (ln:Line3D) =  l.IsParallelAndOrientedTo ln
        
    /// Checks if two 3D lines are perpendicular. 
    /// Calculates the dot product and checks if it is smaller than 1e-9.
    /// (NOTE: for very long lines a higher tolerance might be needed)
    static member inline isPerpendicularTo (l:Line3D) (ln:Line3D) =  l.IsPerpendicularTo ln 

    /// Assumes Line3D to be infinite.
    /// Returns the parameter at which a point is closest to the infinite line.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finite line.
    static member inline closestParameterInfinite (p:Pnt) (ln:Line3D)  = ln.ClosestParameterInfinite p

    /// Return the parameter at which a point is closest to the (finite) line.
    /// The result is between 0.0 and 1.0.
    static member inline closestParameter (p:Pnt) (ln:Line3D)  = ln.ClosestParameter p 

    /// Assumes Line3D to be infinite.
    /// Returns closest point on infinite Line.
    static member inline closestPointInfinite (p:Pnt) (ln:Line3D)  = ln.ClosestPointInfinite p        

    /// Returns closest point on (finite) Line.
    static member inline closestPoint (p:Pnt) (ln:Line3D)  = ln.ClosestPoint p    

    /// Assumes Line3D to be infinite.
    /// Returns Square distance from point to infinite line.
    static member inline distanceSqFromPointInfinite(p:Pnt) (ln:Line3D)  = ln.DistanceSqFromPointInfinite p
    
    /// Assumes Line3D to be infinite.
    /// Returns distance from point to infinite line.
    static member inline distanceFromPointInfinite(p:Pnt) (ln:Line3D)  = ln.DistanceFromPointInfinite p   
        
    /// Returns Square distance from point to (finite) line.
    static member inline distanceSqFromPoint(p:Pnt) (ln:Line3D)  = ln.DistanceSqFromPoint p
    
    /// Returns distance from point to (finite) line.
    static member inline distanceFromPoint(p:Pnt) (ln:Line3D)  = ln.DistanceFromPoint p
    
    /// Get distance from start of line to point projected onto line, may be negative
    static member inline lengthToPtOnLine (line:Line3D) pt = 
        // TODO can be optimized by inlining floats.
        line.Tangent.Unitized * (pt-line.From) 

    /// Extend by absolute amount at start and end.
    static member inline extend (distAtStart:float) (distAtEnd:float) (ln:Line3D) = 
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        let l = sqrt(x*x + y*y + z*z)
        if l < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line3D.extend %O to short for finding point at a Distance" ln        
        Line3D( ln.FromX - x*distAtStart/l, 
                ln.FromY - y*distAtStart/l, 
                ln.FromZ - z*distAtStart/l, 
                ln.ToX   + x*distAtEnd/l, 
                ln.ToY   + y*distAtEnd/l, 
                ln.ToZ   + z*distAtEnd/l )
    
    /// Extend by absolute amount at start.
    static member inline extendStart (distAtStart:float)  (ln:Line3D) = 
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        let l = sqrt(x*x + y*y + z*z)
        if l < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line3D.extendStart %O to short for finding point at a Distance" ln        
        Line3D( ln.FromX - x*distAtStart/l, 
                ln.FromY - y*distAtStart/l, 
                ln.FromZ - z*distAtStart/l, 
                ln.ToX   , 
                ln.ToY   , 
                ln.ToZ   )
    
    /// Extend by absolute amount at end.
    static member inline extendEnd  (distAtEnd:float) (ln:Line3D) = 
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        let l = sqrt(x*x + y*y + z*z)
        if l < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line3D.extendEnd %O to short for finding point at a Distance" ln        
        Line3D( ln.FromX , 
                ln.FromY , 
                ln.FromZ , 
                ln.ToX   + x*distAtEnd/l, 
                ln.ToY   + y*distAtEnd/l, 
                ln.ToZ   + z*distAtEnd/l )  

    /// Finds point at given distance from line start
    static member inline pointAtDistance dist (ln:Line3D) = 
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        let len = sqrt(x*x + y*y + z*z)
        if len < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line3D.pointAtDistance %O to short for finding point at a Distance" ln        
        Pnt(ln.FromX + x*dist/len, 
            ln.FromY + y*dist/len, 
            ln.FromZ + z*dist/len)
    
    /// Returns new Line3D with given length, going out from start in direction of end
    static member inline withLengthFromStart len (ln:Line3D) = 
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        let l = sqrt(x*x + y*y + z*z)
        if l < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line3D.withLengthFromStart %O to short for finding point at a Distance" ln        
        Line3D( ln.FromX , 
                ln.FromY , 
                ln.FromZ , 
                ln.FromX + x*len/l , 
                ln.FromY + y*len/l , 
                ln.FromZ + z*len/l ) 
    
    /// Returns new Line3D ending at current LineEnd with given length coming from direction of start.
    static member inline withLengthToEnd len (ln:Line3D) = 
        let x = ln.FromX-ln.ToX
        let y = ln.FromY-ln.ToY
        let z = ln.FromZ-ln.ToZ
        let l = sqrt(x*x + y*y + z*z)
        if l < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line3D.withLengthToEnd %O to short for finding point at a Distance" ln
        Line3D( ln.ToX + x*len/l , 
                ln.ToY + y*len/l , 
                ln.ToZ + z*len/l , 
                ln.ToX , 
                ln.ToY , 
                ln.ToZ )  

    
    /// Offset line in XY Plane to left side in line direction
    static member offset amount (ln:Line3D) = 
        let x = ln.ToX - ln.FromX
        let y = ln.ToY - ln.FromY
        let z = ln.ToZ - ln.FromZ
        let lenXY = sqrt (x*x + y*y) 
        if lenXY  < 1e-12 then FsExGeoException.Raise "FsEx.Geo.Line3D.offset: Cannot offset vertical Line3D  (by %g) %O" amount ln
        let ox = -y*amount/lenXY  // unitized, horizontal , perpendicular  vector
        let oy =  x*amount/lenXY  // unitized, horizontal , perpendicular  vector
        Line3D( ln.FromX+ox,
                ln.FromY+oy,
                ln.FromZ,
                ln.ToX+ox,
                ln.ToY+oy,
                ln.ToZ)    

    /// Returns an array of points of length segment count + 1.
    /// Includes start and endpoint of line.
    static member divide (segments:int) (ln:Line3D) = 
        match segments with
        | x when x < 1 -> FsExGeoException.Raise "FsEx.Geo.Line3D.divide failed for %d segments. Minimum one. for %O"  segments ln
        | 1 -> [|ln.From;  ln.To|]
        | k ->
            let x = ln.ToX - ln.FromX
            let y = ln.ToY - ln.FromY
            let z = ln.ToZ - ln.FromZ
            let sx = ln.FromX
            let sy = ln.FromY
            let sz = ln.FromZ
            let kk = float k
            let r = Array.zeroCreate (k+1)
            r.[0] <- ln.From
            for i = 1 to k-1 do
                let t = float i / kk
                r.[i] <- Pnt(sx + x*t, sy + y*t, sz + z*t)
            r.[k] <- ln.To
            r            
            

    /// Divides line into as many as segments as possible respecting the minimum segment length.
    /// Includes start and endpoint of line.
    static member divideMinLength (minSegmentLength:float) (ln:Line3D) = 
        let len = ln.Length
        if len < minSegmentLength then 
            FsExGeoException.Raise "FsEx.Geo.Line3D.divideMinLength minSegmentLength %g is bigger than line length %g for %O"  minSegmentLength len ln
        let k = int (len / minSegmentLength)
        Line3D.divide k ln


    /// Divides line into as few as segments as possible respecting the maximum segment length.
    /// Includes start and endpoint of line.
    static member divideMaxLength (maxSegmentLength:float) (ln:Line3D) = 
        let len = ln.Length
        let k = int (len / maxSegmentLength) + 1
        Line3D.divide k ln

type Line3D with            
    //----------------------------------------------------------------------------------------------------------------
    //------------------------------Line Line Intersection : ----------------------------------------------------
    //----------------------------------------------------------------------------------------------------------------


    /// Assumes Lines to be infinite.
    /// Returns either the parameters at which two infinite 3D Lines are closest to each other.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finite line.    
    /// Or if the lines are parallel within approx 0.25 degrees then The Parallel Union Case.
    /// First parameter is on l, second parameter is on ll.
    static member inline intersectionParamInfinite(l:Line3D) (ll:Line3D) : IntersectionParamInfinite =        
        //https://stackoverflow.com/a/34604574/969070 but DP and DQ are in wrong order !        
        let ax = l.FromX - l.ToX  
        let ay = l.FromY - l.ToY  
        let az = l.FromZ - l.ToZ
        let bx = ll.FromX - ll.ToX 
        let by = ll.FromY - ll.ToY 
        let bz = ll.FromZ - ll.ToZ
        let vx = ll.FromX - l.FromX
        let vy = ll.FromY - l.FromY
        let vz = ll.FromZ - l.FromZ
        let a = ax*ax + ay*ay + az*az // square length
        let b = ax*bx + ay*by + az*bz
        let c = bx*bx + by*by + bz*bz // square length        
        let ac = a*c // square of square length  , never negative
        let bb = b*b // never negative
        let discriminant = ac - bb
        let div = ac+bb // never negative
        if div < 2e-48 then // both lines shorter than 1e-12 ! (2 * 1e-12 * 1e-12 * 1e-12 * 1e-12)
            TwoParam (0.5, 0.5) 
        else
            let e = bx*vx + by*vy + bz*vz  
            // getting the relation between the sum and the subtraction gives a good estimate of the angle between the lines
            // see file 'Calculate Angle constants for Line3D intersection.fsx' in Docs folder.
            // 1.5e-6 for 0.1  degree  
            // 1e-5   for 0.25 degree 
            // 1.5e-4 for 1.0  degree   
            let rel = discriminant/div
            if rel < 1e-5 then //parallel               
                let t = e / c //closest parameter of l.From on ll
                let p = ll.EvaluateAt(t) //TODO could be inlined to optimize 
                if Pnt.distanceSq p l.From < 1e-12 then // square of 1e-6
                    IntersectionParamInfinite.Coincident
                else   
                    IntersectionParamInfinite.Parallel        
            else 
                let d = ax*vx + ay*vy + az*vz
                let t = (b * e - c * d) / discriminant
                let u = (a * e - b * d) / discriminant
                TwoParam (t,u) 
    
    

    /// Assumes Lines to be infinite.    
    /// Returns the one Point where the two lines intersect or are maximum the given tolerance apart.
    /// Or the two points where these two infinite lines are closest to each other.
    /// Else if the lines are parallel within approx 0.25 degrees then The Parallel Union Case.    
    static member inline intersectionInfiniteTol tolerance (l:Line3D) (ll:Line3D) : IntersectionPointsInfinite3D =  
        match Line3D.intersectionParamInfinite l ll with 
        |TwoParam (u,v) ->
            let a =  l.EvaluateAt u
            let b = ll.EvaluateAt v
            if Pnt.distanceSq a b > tolerance*tolerance then 
                IntersectionPointsInfinite3D.TwoPoints (a,b)
            else 
                IntersectionPointsInfinite3D.OnePoint (Pnt.midPt a b)
        |IntersectionParamInfinite.Parallel   ->  IntersectionPointsInfinite3D.Parallel
        |IntersectionParamInfinite.Coincident ->  IntersectionPointsInfinite3D.Coincident

    /// Assumes Lines to be infinite.    
    /// Returns the one Point where the two infinite lines intersect or are maximum the 1e-6 units apart.
    /// Or the two points where these two infinite lines are closest to each other.
    /// Else if the lines are parallel within approx 0.25 degrees then The Parallel Union Case.    
    static member inline intersectionInfinite  (l:Line3D) (ll:Line3D) : IntersectionPointsInfinite3D=  
        Line3D.intersectionInfiniteTol 1e-6 l ll            

    /// Assumes Lines to be infinite.    
    /// Returns the single points where these two infinite lines actually intersect each other.
    /// Fails if lines are parallel or skew by more than 1e-6 units 
    /// The returned point is in the middle between l and ll.  
    static member intersectionPointInfinite (l:Line3D) (ll:Line3D) : Pnt = 
        match Line3D.intersectionInfiniteTol 1e-6 l ll with 
        |IntersectionPointsInfinite3D.OnePoint p  -> p
        |IntersectionPointsInfinite3D.TwoPoints _ -> FsExGeoException.Raise "FsEx.Geo.Line3D.intersectionPointInfinite: Lines are skew l: \r\n%O and ll: \r\n%O" l ll
        |IntersectionPointsInfinite3D.Parallel    -> FsExGeoException.Raise "FsEx.Geo.Line3D.intersectionPointInfinite: Lines are parallel l: \r\n%O and ll: \r\n%O" l ll
        |IntersectionPointsInfinite3D.Coincident  -> FsExGeoException.Raise "FsEx.Geo.Line3D.intersectionPointInfinite: Lines are coincident l: \r\n%O and ll: \r\n%O" l ll

    /// Assumes Lines to be infinite.    
    /// Returns the distance between two infinite lines.
    /// Does NOT fail on parallel Lines. 
    /// Still returns their distance apart.   
    static member inline distanceBetweenInfiniteLines l ll =
        match Line3D.intersectionParamInfinite l ll with 
        |TwoParam (u,v) ->
            let a =  l.EvaluateAt u
            let b = ll.EvaluateAt v
            Pnt.distance a b 
        |IntersectionParamInfinite.Parallel -> 
            l.DistanceFromPointInfinite ll.From

    /// Returns the parameters at which two (finite) 3D Lines are closest to each other.
    /// The results are both between 0.0 and 1.0.
    /// For parallel lines it still returns two parameters, in the middle of their overlap, or distance apart.
    /// First parameter is on l, second parameter is on ll.
    static member inline intersectionParam (l:Line3D) (ll:Line3D) : IntersectionParam = 
        // possible results :  
        // | Intersecting | IntersectingEndsBoth  | IntersectingEndsFirst | IntersectingEndsSecond | (only 3D Skew) | Apart   
        // | Parallel  | Overlapping | Continuation  | CoincidentApart | Identical
        match Line3D.intersectionParamInfinite l ll with 
        | IntersectionParamInfinite.TwoParam ( u0 , v0 ) -> 
            /// numerical error tolerance check:
            let u = 
                match Util.isZeroOneOrBetween u0 with 
                |Zero -> 0.0
                |One -> 1.0
                |Between -> u0
                |Outside -> u0
            let v = 
                match Util.isZeroOneOrBetween v0 with 
                |Zero -> 0.0
                |One -> 1.0
                |Between -> v0
                |Outside -> v0
            if isBetweenZeroAndOne u && isBetweenZeroAndOne v then  
                IntersectionParam.Intersecting( u , v )
            else 
                // finite Lines are not intersection, still find their closest Points:
                let pu = l.EvaluateAt (clampBetweenZeroAndOne u)
                let vt = Line3D.closestParameter pu ll
                let pv = ll.EvaluateAt(vt)
                let ut = Line3D.closestParameter pv l 
                IntersectionParam.Apart (ut ,vt)
        
        | IntersectionParamInfinite.Parallel ->
            // Still return two parameters,  in the middle of their overlap, or distance apart.
            let lv  =  l.Direction // Vec(ax,ay,az)
            let llv = ll.Direction //Vec(bx,by,bz)            
            //make a new line k that is oriented the same way:
            let k = if lv*llv < 0.0 then ll.Reversed else ll
            let l0k0 = Vec.create(l.From,k.From)
            //let l0k1 = Vec.create(l.From,k.To)
            //let l1k0 = Vec.create(l.To  ,k.From)
            let l1k1 = Vec.create(l.To  ,k.To)
            // check if vectors between lines are in same orientation as line:
            let d00 = lv * l0k0 > 0.
            //let d01 = lv * l0k1 > 0.
            //let d10 = lv * l1k0 > 0.
            let d11 = lv * l1k1 > 0.
            
            // full logic:
            // if   not d00 && not d01 && not d10 && not d11 then let p = Pnt.midPt l.From k.To   in  l.ClosestParameter(p),ll.ClosestParameter(p) // l starts after k ends
            // elif not d00 &&     d01 && not d10 && not d11 then let p = Pnt.midPt l.From k.To   in  l.ClosestParameter(p),ll.ClosestParameter(p) // k is overlapping l start
            // elif     d00 &&     d01 && not d10 && not d11 then 0.5  , ll.ClosestParameter(l.Mid) // l is on both ends shorter than k                
            // elif not d00 &&     d01 && not d10 &&     d11 then l.ClosestParameter(ll.Mid), 0.5   // k is on both ends shorter than l                
            // elif     d00 &&     d01 && not d10 &&     d11 then let p = Pnt.midPt l.To   k.From in  l.ClosestParameter(p),ll.ClosestParameter(p) // k is overlapping l end 
            // elif     d00 &&     d01 &&     d10 &&     d11 then let p = Pnt.midPt l.To   k.From in  l.ClosestParameter(p),ll.ClosestParameter(p) // k starts after l ends
            // else
            //     failwith "Bad case in intersectLineParametersInfinite"

            // optimized logic  
            if       d00 && not d11 then IntersectionParam.Parallel (l.ClosestParameter ll.Mid, 0.5 ) // l is on both ends shorter than k                
            elif not d00 &&     d11 then IntersectionParam.Parallel (0.5  , ll.ClosestParameter l.Mid)  // k is on both ends shorter than l                
            else 
                let l0k1 = Vec.create(l.From,k.To)
                let d01 = lv * l0k1 > 0.
                let p = 
                    if d01 then  Pnt.midPt l.To   k.From
                    else         Pnt.midPt l.From k.To
                IntersectionParam.Parallel ( l.ClosestParameter p, ll.ClosestParameter p ) 

    /// Intersects two finite 3D lines.
    /// Returns the one Point where the two finite lines intersect or are maximum the given tolerance apart.
    /// Otherwise returns  two points.
    /// These points are always on the finite input line.
    /// The points are on the first and the second line respectively.    
    /// Otherwise it is where they are closest to each other.
    /// For parallel lines  within 0.25 degrees it still returns two parameters, in the middle of their overlap, or distance apart.
    /// First point is on l, second point is on ll .
    static member inline intersectionTol tolerance (l:Line3D) (ll:Line3D) : IntersectionPoints3D =  
        match Line3D.intersectionParam l ll with 
        | IntersectionParam.Intersecting( u , v ) -> 
            let a  =  l.EvaluateAt u 
            let b  = ll.EvaluateAt v 
            let d =  Pnt.distance a b
            if d < tolerance then 
                IntersectionPoints3D.OnePoint(a)
            else
                IntersectionPoints3D.TwoPointsSkew (a,b,d)
        | IntersectionParam.Apart( u , v ) -> 
            let a  =  l.EvaluateAt u 
            let b  = ll.EvaluateAt v 
            IntersectionPoints3D.TwoPointsApart (a,b)
        | IntersectionParam.Parallel( u , v ) -> 
            let a  =  l.EvaluateAt u 
            let b  = ll.EvaluateAt v 
            IntersectionPoints3D.Parallel (a,b)

    /// Intersects two finite 3D lines.
    /// Returns the one Point where the two finite lines intersect or are maximum 1e-6 apart.
    /// Otherwise returns  two points.
    /// These points are always on the finite input line.
    /// The points are on the first and the second line respectively.    
    /// Otherwise it is where they are closest to each other.
    /// For parallel lines  within 0.25 degrees it still returns two parameters, in the middle of their overlap, or distance apart.
    /// First point is on l, second point is on ll .
    static member inline intersection  (l:Line3D) (ll:Line3D) : IntersectionPoints3D =  
        match Line3D.intersectionParam l ll with 
        | IntersectionParam.Intersecting( u , v ) ->
            let a  =  l.EvaluateAt u 
            let b  = ll.EvaluateAt v 
            let d =  Pnt.distance a b
            if d < 1e-6 then 
                IntersectionPoints3D.OnePoint(a)
            else
                IntersectionPoints3D.TwoPointsSkew (a,b,d)
        | IntersectionParam.Apart( u , v ) -> 
            let a  =  l.EvaluateAt u 
            let b  = ll.EvaluateAt v 
            IntersectionPoints3D.TwoPointsApart (a,b)
        | IntersectionParam.Parallel( u , v ) -> 
            let a  =  l.EvaluateAt u 
            let b  = ll.EvaluateAt v 
            IntersectionPoints3D.Parallel (a,b)


    /// Returns the single points where these two finite lines actually intersect each other.
    /// Fails if lines are parallel or skew by more than 1e-6 units 
    /// The returned point is in the middle between l and ll.  
    static member intersectionInOnePoint (l:Line3D) (ll:Line3D) : Pnt = 
        match Line3D.intersection l ll with 
        |IntersectionPoints3D.OnePoint(a) -> a
        |IntersectionPoints3D.TwoPointsSkew (_,_,d) -> FsExGeoException.Raise "FsEx.Geo.Line3D.intersectionInOnePoint: Lines are skew by %g l: \r\n%O and ll: \r\n%O" d l ll
        |IntersectionPoints3D.TwoPointsApart _ -> FsExGeoException.Raise "FsEx.Geo.Line3D.intersectionInOnePoint: Lines are apart l: \r\n%O and ll: \r\n%O" l ll
        |IntersectionPoints3D.Parallel _       -> FsExGeoException.Raise "FsEx.Geo.Line3D.intersectionInOnePoint: Lines are parallel l: \r\n%O and ll: \r\n%O" l ll


    /// Returns the distance between two (finite) 3D lines.
    /// Fails on parallel Lines. 
    static member inline distanceBetweenLines l ll=
        match Line3D.intersectionParam l ll with 
        | IntersectionParam.Intersecting( u , v ) 
        | IntersectionParam.Apart( u , v ) 
        | IntersectionParam.Parallel( u , v ) -> 
            let a  =  l.EvaluateAt u 
            let b  = ll.EvaluateAt v 
            IntersectionPoints3D.Parallel (a,b)
    


    (*  not very useful because it's hard to find the correct tolerance:

    /// Fast check if two 3D lines are in the same 3D Plane.
    /// This can be used as a fast check to exclude intersection. Infinite lines in the same plane do intersect (unless parallel) 
    /// If you need an exact intersection test purely based on distance use Line3D.intersectLine
    /// It first calculates the signed volume of the Parallelepiped define by three vectors from the four corners of the lines.
    /// Then it checks if it is smaller than given volumeTolerance fo Parallelepiped.
    /// Using the VolumeTolerance makes a positive result not only dependent on the distance of the two 3D lines from each other but also on their lengths. 
    static member inline areInSamePlaneTol volumeTolerance (l:Line3D) (ll:Line3D) =  
        // Two non-parallel lines p1+Rv1 and p2+Rv2 intersect if and only if (v1×v2)⋅(p1−p2)=0
        // https://math.stackexchange.com/questions/697124/how-to-determine-if-two-lines-in-3d-intersect
        // vector of line l:
        let ax = l.ToX-l.FromX
        let ay = l.ToY-l.FromY
        let az = l.ToZ-l.FromZ
        //vector of line ll:
        let bx = ll.ToX-ll.FromX
        let by = ll.ToY-ll.FromY
        let bz = ll.ToZ-ll.FromZ
        // cross product:
        let cx = ay * bz - az * by   
        let cy = az * bx - ax * bz   
        let cz = ax * by - ay * bx 
        // vector from start of l to start of ll:
        let vx = l.FromX-ll.FromX
        let vy = l.FromY-ll.FromY
        let vz = l.FromZ-ll.FromZ
        let dot = cx*vx + cy*vy + cz*vz
        abs(dot) < volumeTolerance
    *)



