namespace FsEx.Geo

     
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>]
open FsEx.Geo.Util
open FsEx.Geo.LineIntersectionTypes  

/// An immutable finite line in 3D. Represented by a 3D start and 3D end point.
[<Struct;NoEquality;NoComparison>]// because its made up from floats
[<IsReadOnly>]
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
    member inline ln.MatchesOrientation180  (otherLn:Line3D) = 
        let dot = (otherLn.ToX-otherLn.FromX)*(ln.ToX-ln.FromX) + (otherLn.ToY-otherLn.FromY)*(ln.ToY-ln.FromY) + (otherLn.ToZ-otherLn.FromZ)*(ln.ToZ-ln.FromZ) 
        dot > 0.0  

    /// Checks if the angle between the a 3D line and a 3D vector is less than 180 degrees.
    /// Calculates the dot product of both. 
    /// Then checks if it is positive.
    member inline ln.MatchesOrientation180  (v:Vec) = 
        let dot = v.X*(ln.ToX-ln.FromX) + v.Y*(ln.ToY-ln.FromY) + v.Z*(ln.ToZ-ln.FromZ)
        dot > 0.0 

    /// Checks if the angle between the a 3D line and a 3D unit vector is less than 180 degrees.
    /// Calculates the dot product of both. 
    /// Then checks if it is positive.
    member inline ln.MatchesOrientation180  (v:UnitVec) = 
        let dot = v.X*(ln.ToX-ln.FromX) + v.Y*(ln.ToY-ln.FromY) + v.Z*(ln.ToZ-ln.FromZ)
        dot > 0.0 

    /// Checks if the angle between the two 3D lines is less than 90 degrees.   
    /// Calculates the dot product of the unit vectors of the two 3D lines. 
    /// Then checks if it is bigger than 0.707107 (cosine of  90 degrees).
    member inline ln.MatchesOrientation90  (otherLn:Line3D) = 
        let dot = ln.UnitTangent*otherLn.UnitTangent
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

    /// Applies or multiplies a 4x4 transformation matrix to a 3D line
    static member inline transform (m:Matrix) (l:Line3D) = Line3D(l.From*m, l.To*m)     
        
    /// Multiplies (or applies) an OrthoMatrix to a 3D line . 
    static member transformOrtho (m:OrthoMatrix) (l:Line3D)  = Line3D(l.From*m, l.To*m)  
    
    /// Multiplies (or applies) only the 3x3 rotation part of an OrthoMatrix to a 3D Unit Vector . 
    /// The resulting vector has the same length as the input.
    static member rotateOrtho (m:OrthoMatrix) (l:Line3D)  = Line3D(Pnt.rotateOrtho m l.From, Pnt.rotateOrtho m l.To)  

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
    /// Fails on lines shorter than 1e-12.
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
    /// Fails on lines shorter than 1e-12.
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
    /// Fails on lines shorter than 1e-12.
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

    /// Finds point at given distance from line start.
    /// Fails on lines shorter than 1e-12.
    static member inline pointAtDistance dist (ln:Line3D) = 
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        let len = sqrt(x*x + y*y + z*z)
        if len < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line3D.pointAtDistance %O to short for finding point at a Distance" ln        
        Pnt(ln.FromX + x*dist/len, 
            ln.FromY + y*dist/len, 
            ln.FromZ + z*dist/len)
    
    /// Returns new Line3D with given length, going out from start in direction of end.
    /// Fails on lines shorter than 1e-12.
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
    /// Fails on lines shorter than 1e-12.
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

    
    /// Offset line in XY Plane to left side in line direction.
    /// Fails on lines shorter than 1e-12.
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
        
    //----------------------------------------------------------------------------------------------------------------
    //------------------------------Line Line Intersection : ----------------------------------------------------
    //----------------------------------------------------------------------------------------------------------------


    ///<summary> Gets the parameters at which two infinite 3D lines intersect. Or are closest to each other.</summary>
    ///<param name="lnA"> The first line.</param>
    ///<param name="lnB"> The second line.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than the 'TooShort' union case is returned .</param>
    ///<param name="relAngleDiscriminant"> This is an optional tolerance for the internally calculated relative Angle Discriminant. 
    /// The default value is '0.00000952' this corresponds to approx 0.25 degree. Below this angle the 'Parallel' or 'Coincident' union case is returned. 
    /// Use the module FsEx.Geo.Util.RelAngleDiscriminant to set another tolerance here.</param>   
    ///<param name="coincidentTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If parallel lines are closer than this the 'Coincident' union case is returned .</param>    
    ///<returns> An IntersectionParam Discriminated Union with the following cases:  
    ///         
    /// | TwoParam of twoParams : struct(float*float):
    /// The infinite lines are intersecting  or skew.
    /// They have each one point where they are intersecting each other.  
    /// Or are closest to each other in skew case.
    /// The tuple's order corresponds to the input order.
    ///     
    /// | Parallel: 
    /// The lines are parallel within 0.25 degrees.
    /// They have no points in common
    ///     
    /// | Coincident:
    /// The lines are coincident (or maybe even identical) .
    /// As infinite lines they have infinitely many points in common.
    /// They might still not have the same start and end points in their finit definition.
    /// 
    /// | TooShort:
    /// One or both input lines is shorter than the given minimum Length tolerance. </returns>
    static member inline intersectionParamInfinite( lnA:Line3D , 
                                                    lnB:Line3D ,
                                                    [<OPT;DEF(1e-6)>] tooShortTolerance:float,
                                                    [<OPT;DEF(RelAngleDiscriminant.``0.25``)>] relAngleDiscriminant:float<RelAngleDiscriminant.relAngDiscr>,
                                                    [<OPT;DEF(1e-6)>] coincidentTolerance:float
                                                    ) : IntersectionParam =        
        //https://stackoverflow.com/a/34604574/969070 but DP and DQ are in wrong order !        
        let ax = lnA.FromX - lnA.ToX  
        let ay = lnA.FromY - lnA.ToY  
        let az = lnA.FromZ - lnA.ToZ
        let bx = lnB.FromX - lnB.ToX 
        let by = lnB.FromY - lnB.ToY 
        let bz = lnB.FromZ - lnB.ToZ
        let vx = lnB.FromX - lnA.FromX
        let vy = lnB.FromY - lnA.FromY
        let vz = lnB.FromZ - lnA.FromZ
        let a = ax*ax + ay*ay + az*az // square length of A
        let b = ax*bx + ay*by + az*bz // dot product of both lines
        let c = bx*bx + by*by + bz*bz // square length of B    
        let shortSq = tooShortTolerance * tooShortTolerance
        if a < shortSq then  // vec A too short            
            if c < shortSq then 
                IntersectionParam.TooShortBoth
            else
                IntersectionParam.TooShortA
        elif c < shortSq then  // vec B too short
            IntersectionParam.TooShortB
        else   
            let ac = a*c // square of square length  , never negative
            let bb = b*b // never negative
            let discriminant = ac - bb // never negative , the dot product cannot be bigger than the two square length multiplied with each other 
            let div = ac+bb // never negative                          
            // getting the relation between the sum and the subtraction gives a good estimate of the angle between the lines
            // see module FsEx.Geo.Util.RelAngleDiscriminant    
            let rel = discriminant/div
            if rel < float relAngleDiscriminant then //parallel               
                let e = bx*vx + by*vy + bz*vz  
                let t = e / c // c is already checked for being non zero. get closest parameter of lnA.From on lnB
                let p = lnB.EvaluateAt(t) //TODO could be inlined to optimize 
                if Pnt.distanceSq p lnA.From < coincidentTolerance*coincidentTolerance then 
                    IntersectionParam.Coincident
                else   
                    IntersectionParam.Parallel       
            else 
                let e = bx*vx + by*vy + bz*vz  
                let d = ax*vx + ay*vy + az*vz
                let t = (b * e - c * d) / discriminant
                let u = (a * e - b * d) / discriminant
                TwoParam (t,u)
    
    ///<summary> Gets the points at which two infinite 3D lines intersect. Or are closest to each other.</summary>
    ///<param name="lnA"> The first line.</param>
    ///<param name="lnB"> The second line.</param>
    ///<param name="skewTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If skew lines are closer than this distance they are considered intersecting in one point. The returned point is on line A.</param> 
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than the 'TooShort' union case is returned .</param>
    ///<param name="relAngleDiscriminant"> This is an optional tolerance for the internally calculated relative Angle Discriminant. 
    /// The default value is '0.00000952' this corresponds to approx 0.25 degree. Below this angle the 'Parallel' or 'Coincident' union case is returned. 
    /// Use the module FsEx.Geo.Util.RelAngleDiscriminant to set another tolerance here.</param>   
    ///<param name="coincidentTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If parallel lines are closer than this the 'Coincident' union case is returned .</param>   
    ///<returns> An IntersectionPoints3D Discriminated Union with the following cases:  
    /// | TwoPoints of skewPoints : struct(Pnt*Pnt)
    ///     The lines are skew by mor than 1e-6. or the given tolerance
    ///     Contains the points on the first and second 
    ///     line where they are closest to each other.
    /// 
    /// | OnePoint of xPoint : Pnt
    ///     The 3D lines are intersection in exactly one point. 
    ///     Or the points are closer than the skewTolerance.  1e-6 by default. The returned point is on line A.
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
    static member inline intersectionInfinite ( lnA:Line3D , 
                                                lnB:Line3D ,
                                                [<OPT;DEF(1e-6)>] skewTolerance:float,
                                                [<OPT;DEF(1e-6)>] tooShortTolerance:float,
                                                [<OPT;DEF(RelAngleDiscriminant.``0.25``)>] relAngleDiscriminant:float<RelAngleDiscriminant.relAngDiscr>,
                                                [<OPT;DEF(1e-6)>] coincidentTolerance:float
                                                ) : IntersectionPoints3D =  
        match Line3D.intersectionParamInfinite(lnA , lnB, tooShortTolerance, relAngleDiscriminant, coincidentTolerance) with 
        |TwoParam (u,v) ->
            let a =  lnA.EvaluateAt u
            let b = lnB.EvaluateAt v
            if Pnt.distanceSq a b > skewTolerance*skewTolerance then 
                IntersectionPoints3D.TwoPoints (a,b)
            else 
                IntersectionPoints3D.OnePoint a // or (Pnt.midPt a b) ?
        |IntersectionParam.Parallel   ->  IntersectionPoints3D.Parallel
        |IntersectionParam.Coincident ->  IntersectionPoints3D.Coincident
        |IntersectionParam.TooShortA
        |IntersectionParam.TooShortB
        |IntersectionParam.TooShortBoth  ->  IntersectionPoints3D.TooShort
        
    ///<summary>Gets the single points where these two infinite 3D lines actually intersect each other.
    /// The returned point is on line A. </summary>
    ///<param name="lnA"> The first line.</param>
    ///<param name="lnB"> The second line.</param>
    ///<param name="skewTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If skew lines are closer than this distance they are considered intersecting in one point. The returned point is on line A.</param> 
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than the 'TooShort' union case is returned .</param>
    ///<param name="relAngleDiscriminant"> This is an optional tolerance for the internally calculated relative Angle Discriminant. 
    /// The default value is '0.00000952' this corresponds to approx 0.25 degree. Below this angle the 'Parallel' or 'Coincident' union case is returned. 
    /// Use the module FsEx.Geo.Util.RelAngleDiscriminant to set another tolerance here.</param>   
    ///<param name="coincidentTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If parallel lines are closer than this the 'Coincident' union case is returned .</param> 
    /// <returns> A single 3D point</returns>  
    static member intersectionPointInfinite(lnA:Line3D , 
                                            lnB:Line3D ,
                                            [<OPT;DEF(1e-6)>] skewTolerance:float,
                                            [<OPT;DEF(1e-6)>] tooShortTolerance:float,
                                            [<OPT;DEF(RelAngleDiscriminant.``0.25``)>] relAngleDiscriminant:float<RelAngleDiscriminant.relAngDiscr>,
                                            [<OPT;DEF(1e-6)>] coincidentTolerance:float
                                            ) : Pnt = 
        match Line3D.intersectionInfinite(lnA , lnB, skewTolerance, tooShortTolerance, relAngleDiscriminant, coincidentTolerance) with 
        |IntersectionPoints3D.OnePoint p  -> p
        |IntersectionPoints3D.TwoPoints _ -> FsExGeoException.Raise "FsEx.Geo.Line3D.intersectionPointInfinite: Lines are skew lnA: \r\n%O and lnB: \r\n%O" lnA lnB
        |IntersectionPoints3D.Parallel    -> FsExGeoException.Raise "FsEx.Geo.Line3D.intersectionPointInfinite: Lines are parallel lnA: \r\n%O and lnB: \r\n%O" lnA lnB
        |IntersectionPoints3D.Coincident  -> FsExGeoException.Raise "FsEx.Geo.Line3D.intersectionPointInfinite: Lines are coincident lnA: \r\n%O and lnB: \r\n%O" lnA lnB
        |IntersectionPoints3D.TooShort    -> FsExGeoException.Raise "FsEx.Geo.Line3D.intersectionPointInfinite: Lines are tooShort lnA: \r\n%O and lnB: \r\n%O" lnA lnB

    /// Assumes Lines to be infinite.    
    /// Returns the distance between two infinite 3D lines. At their closest point.
    /// Fails if one or both lines are shorter than 1e-6.
    /// Unless the lines are skew or parallel this returns 0.0. 
    /// Uses the default tolerances from the Line3D.intersectionParamInfinite function 
    /// to detect parallel and coincident lines. 
    static member inline distanceBetweenInfiniteLines(lnA , lnB) =
        match Line3D.intersectionParamInfinite(lnA , lnB) with 
        |IntersectionParam.Coincident    ->  0.0        
        |IntersectionParam.TooShortA     ->  FsExGeoException.Raise "FsEx.Geo.Line3D.distanceBetweenInfiniteLines: Line A is tooShort lnA: \r\n%O and lnB: \r\n%O" lnA lnB
        |IntersectionParam.TooShortB     ->  FsExGeoException.Raise "FsEx.Geo.Line3D.distanceBetweenInfiniteLines: Line B is  tooShort lnA: \r\n%O and lnB: \r\n%O" lnA lnB
        |IntersectionParam.TooShortBoth  ->  FsExGeoException.Raise "FsEx.Geo.Line3D.distanceBetweenInfiniteLines: both Lines are tooShort lnA: \r\n%O and lnB: \r\n%O" lnA lnB
        |IntersectionParam.Parallel      -> lnA.DistanceFromPointInfinite lnB.From
        |TwoParam (u,v) -> // skew or intersecting
            let a =  lnA.EvaluateAt u
            let b = lnB.EvaluateAt v
            Pnt.distance a b 

    
    /// <summary>Returns the intersection kind and the parameters at which two (finite) 3D Lines are intersecting or closest to each other.
    /// The threshold for skew intersection can be given as an optional tolerance input. The default is 1e-6.
    /// If the two points ar within this distance one of the Intersecting Cases is returned. 
    /// (or Continuation Case if lines are colinear in one point)
    /// The results are both between 0.0 and 1.0.
    /// For parallel and coincident lines it still returns two parameters, in the middle of their overlap, or distance apart.
    /// First parameter is on lnA, second parameter is on lnB.
    /// The possible result cases are:  
    /// 
    /// | Intersecting : The finite lines are intersecting each other in one point.
    /// | IntersectingEndsBoth: The finite lines are intersecting each other at one of their end or start points point. 
    /// | IntersectingEndsFirst: The finite lines are intersecting. The first line is touching the second one with its end or start point.       
    /// | IntersectingEndsSecond:  The finite lines are intersecting. The second line is touching the first one with its end or start point.        
    /// 
    /// | Skew:  The finite lines are skew to each other.
    /// Their closest points to each other are within the line.
    /// The returned parameters are between 0.0 and 1.0
    /// 
    /// | Apart: The finite lines are not intersecting nor skew.
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
    /// The returned parameters are the closets point to lineA from the middle of line B and 0.5 for line B
    /// 
    /// | TooShortBoth     
    /// Both input lines are shorter than the given minimum Length tolerance.  
    /// The returned parameters are 0.5 and 0.5 for both lines. </summary>
    ///<param name="lnA"> The first line.</param>
    ///<param name="lnB"> The second line.</param>
    ///<param name="skewTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If skew lines are closer than this distance they are considered intersecting in one point. The returned point is on line A.</param> 
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than the 'TooShort' union case is returned .</param>
    ///<param name="relAngleDiscriminant"> This is an optional tolerance for the internally calculated relative Angle Discriminant. 
    /// The default value is '0.00000952' this corresponds to approx 0.25 degree. Below this angle the 'Parallel' or 'Coincident' union case is returned. 
    /// Use the module FsEx.Geo.Util.RelAngleDiscriminant to set another tolerance here.</param>   
    ///<param name="coincidentTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If parallel lines are closer than this the 'Coincident' union case is returned .</param>
    static member inline intersectionParam (lnA:Line3D , 
                                            lnB:Line3D,
                                            [<OPT;DEF(1e-6)>] skewTolerance:float,
                                            [<OPT;DEF(1e-6)>] tooShortTolerance:float,
                                            [<OPT;DEF(RelAngleDiscriminant.``0.25``)>] relAngleDiscriminant:float<RelAngleDiscriminant.relAngDiscr>,
                                            [<OPT;DEF(1e-6)>] coincidentTolerance:float
                                            ) :  IntersectionKind*float*float =         
        match Line3D.intersectionParamInfinite(lnA , lnB, tooShortTolerance, relAngleDiscriminant, coincidentTolerance) with 
        | IntersectionParam.TwoParam ( u0 , v0 ) -> 
            /// numerical error tolerance check to also find an intersection that happens just after the line end:
            let ur = isZeroOneOrBetween u0
            let vr = isZeroOneOrBetween v0
            let u =  match ur with Zero -> 0.0 |One -> 1.0 |Between |Outside -> u0
            let v =  match vr with Zero -> 0.0 |One -> 1.0 |Between |Outside -> v0 
            
            let a = lnA.EvaluateAt(u)
            let b = lnA.EvaluateAt(v)
            let d = Pnt.distanceSq a b
            if d < skewTolerance*skewTolerance then
                if ur=Zero || ur=One then 
                    if vr=Zero || vr=One then 
                        IntersectingEndsBoth , u ,v
                    else
                        IntersectingEndsFirst, u ,v
                elif vr=Zero || vr=One then 
                    IntersectingEndsSecond, u,v
                else
                    Intersecting, u , v
            elif isBetweenZeroAndOne u && isBetweenZeroAndOne v then 
                    Skew, u , v
            else
                // finite Lines are not intersection, still find their closest Points:
                let pu = lnA.EvaluateAt  u
                let vt = Line3D.closestParameter pu lnB
                let pv = lnB.EvaluateAt vt
                let ut = Line3D.closestParameter pv lnA 
                Apart,ut ,vt
        
        | IntersectionParam.Parallel ->
            
            let lv  =  lnA.Direction 
            let llv = lnB.Direction            
            //make a new line k that is oriented the same way:
            let flip = lv*llv < 0.0 
            let k = if flip then lnB.Reversed else lnB
            let l0k0 = Vec.create(lnA.From,k.From)
            let l0k1 = Vec.create(lnA.From,k.To)
            let l1k0 = Vec.create(lnA.To  ,k.From)
            let l1k1 = Vec.create(lnA.To  ,k.To)
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
                    IntersectionKind.Parallel, 1.0, if flip then 1.0 else  0.0   // k starts after lnA ends 
                else 
                    if d00 then 
                        if d11 then IntersectionKind.Parallel ,1.0 , lnB.ClosestParameter(lnA.To)   // k is overlapping lnA end  
                        else        IntersectionKind.Parallel ,lnA.ClosestParameter(lnB.From), 0.0  // k is on both ends shorter than lnA 
                    else  
                        IntersectionKind.Parallel, 0.0 , lnB.ClosestParameter(lnA.From) // k is overlapping lnA start // lnA is on both ends shorter than k  
            else  
                IntersectionKind.Parallel, 0.0 , if flip then 0.0 else  1.0  // lnA starts after k ends 
        
        
        | IntersectionParam.Coincident ->
            // cases Overlapping | Continuation  | CoincidentApart | Identical
            let lv  =  lnA.Direction // Vec(ax,ay,az)
            let llv = lnB.Direction //Vec(bx,by,bz) 
            let flip = lv*llv < 0.0 
            let k = if flip then lnB.Reversed else lnB
            let l0k0 = Vec.create(lnA.From,k.From)
            let l0k1 = Vec.create(lnA.From,k.To)
            let l1k0 = Vec.create(lnA.To  ,k.From)
            let l1k1 = Vec.create(lnA.To  ,k.To)
            let coTolSq = coincidentTolerance*coincidentTolerance
            let z00 = l0k0.LengthSq < coTolSq
            let z01 = l0k1.LengthSq < coTolSq
            let z10 = l1k0.LengthSq < coTolSq
            let z11 = l1k1.LengthSq < coTolSq
            
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

    /// <summary>Returns the intersection kind and the points at which two (finite) 3D Lines are intersecting or closest to each other.
    /// The threshold for skew intersection can be given as an optional tolerance input. The default is 1e-6.
    /// If the two points ar within this distance one of the Intersecting Cases is returned. 
    /// (or Continuation Case if lines are colinear in one point)
    /// one of the Intersecting Cases is returned. (or Continuation Case if lines are colinear in one point)
    /// The results are both between 0.0 and 1.0.
    /// For parallel and coincident lines it still returns two points, in the middle of their overlap, or distance apart.
    /// First point is on lnA, second point is on lnB.
    /// The possible result cases are:  
    /// 
    /// | Intersecting : The finite lines are intersecting each other in one point.
    /// | IntersectingEndsBoth: The finite lines are intersecting each other at one of their end or start points point. 
    /// | IntersectingEndsFirst: The finite lines are intersecting. The first line is touching the second one with its end or start point.       
    /// | IntersectingEndsSecond:  The finite lines are intersecting. The second line is touching the first one with its end or start point.        
    /// 
    /// | Skew:  The finite lines are skew to each other.
    /// Their closest points to each other are within the line.
    /// The returned points are between 0.0 and 1.0
    /// 
    /// | Apart: The finite lines are not intersecting nor skew.
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
    /// | Identical: The Lines are identical , in orientation too with in 1e-6 tolerance.
    /// The returned points still indicate where the lines start and end.
    /// 
    /// | IdenticalFlipped: The Lines are identical. But orientation is flipped.
    /// The returned points still indicate where the lines start and end. 
    /// /// ------------------------------Error Cases: -----------------------------
    /// 
    /// | TooShortA
    /// Input line A is shorter than the given minimum Length tolerance.
    /// The returned points are the middle of line A and the closets point it on line B.
    /// 
    /// | TooShortB
    /// Input line B is shorter than the given minimum Length tolerance.
    /// The returned points are the closets point to lineA from the middle of line B and the middle of line B.
    /// 
    /// | TooShortBoth     
    /// Both input lines are shorter than the given minimum Length tolerance.  
    /// The returned points are in the middle of both lines.</summary>
    ///<param name="lnA"> The first line.</param>
    ///<param name="lnB"> The second line.</param>
    ///<param name="skewTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If skew lines are closer than this distance they are considered intersecting in one point. The returned point is on line A.</param> 
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than the 'TooShort' union case is returned .</param>
    ///<param name="relAngleDiscriminant"> This is an optional tolerance for the internally calculated relative Angle Discriminant. 
    /// The default value is '0.00000952' this corresponds to approx 0.25 degree. Below this angle the 'Parallel' or 'Coincident' union case is returned. 
    /// Use the module FsEx.Geo.Util.RelAngleDiscriminant to set another tolerance here.</param>   
    ///<param name="coincidentTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If parallel lines are closer than this the 'Coincident' union case is returned .</param> 
    static member inline intersection  (lnA:Line3D , 
                                        lnB:Line3D,
                                        [<OPT;DEF(1e-6)>] skewTolerance:float,
                                        [<OPT;DEF(1e-6)>] tooShortTolerance:float,
                                        [<OPT;DEF(RelAngleDiscriminant.``0.25``)>] relAngleDiscriminant:float<RelAngleDiscriminant.relAngDiscr>,
                                        [<OPT;DEF(1e-6)>] coincidentTolerance:float
                                        ) : IntersectionKind*Pnt*Pnt =  
        let k,u,v =  Line3D.intersectionParam(lnA , lnB, skewTolerance, tooShortTolerance, relAngleDiscriminant, coincidentTolerance)
        match k with        
        | Intersecting | IntersectingEndsBoth | IntersectingEndsFirst | IntersectingEndsSecond 
        | Continuation | ContinuationFlipped -> 
            let p = lnA.EvaluateAt u
            k,p,p // return same point twice ?
        | Skew | Apart  | IntersectionKind.Parallel 
        | Overlapping | CoincidentApart | Identical| IdenticalFlipped 
        | TooShortA | TooShortB | TooShortBoth -> // TODO or check if the zero length line is actually on the other line ?
            let a  =  lnA.EvaluateAt u 
            let b  = lnB.EvaluateAt v 
            k,a,b            


    /// <summary>Returns the single points where these two finite lines actually intersect each other. Or None
    /// The threshold for skew intersection can be given as an optional tolerance input. 
    /// If the two points are within this distance a point is returned. 
    /// This might also be the  Continuation Case if lines are colinear in one point.</summary>
    ///<param name="lnA"> The first line.</param>
    ///<param name="lnB"> The second line.</param>
    ///<param name="skewTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If skew lines are closer than this distance they are considered intersecting in one point. The returned point is on line A.</param> 
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than the 'TooShort' union case is returned .</param>
    ///<param name="relAngleDiscriminant"> This is an optional tolerance for the internally calculated relative Angle Discriminant. 
    /// The default value is '0.00000952' this corresponds to approx 0.25 degree. Below this angle the 'Parallel' or 'Coincident' union case is returned. 
    /// Use the module FsEx.Geo.Util.RelAngleDiscriminant to set another tolerance here.</param>   
    ///<param name="coincidentTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If parallel lines are closer than this the 'Coincident' union case is returned .</param>
    static member intersectionPoint (lnA:Line3D , 
                                    lnB:Line3D,
                                    [<OPT;DEF(1e-6)>] skewTolerance:float,
                                    [<OPT;DEF(1e-6)>] tooShortTolerance:float,
                                    [<OPT;DEF(RelAngleDiscriminant.``0.25``)>] relAngleDiscriminant:float<RelAngleDiscriminant.relAngDiscr>,
                                    [<OPT;DEF(1e-6)>] coincidentTolerance:float
                                    ) :  option<Pnt> = 
        let k,u,v =  Line3D.intersectionParam(lnA , lnB, skewTolerance, tooShortTolerance, relAngleDiscriminant, coincidentTolerance)
        match k with        
        | Intersecting | IntersectingEndsBoth | IntersectingEndsFirst | IntersectingEndsSecond 
        | Continuation | ContinuationFlipped -> 
            Some(lnA.EvaluateAt u)
        | Skew | Apart  | IntersectionKind.Parallel 
        | Overlapping | CoincidentApart | Identical| IdenticalFlipped  ->
            None
        | TooShortA | TooShortB -> // TODO or check if the short line or zero length line is still exactly on the other line ?
            None
        | TooShortBoth -> // TODO or return a point if two zero length lines are on the same point?
            None

    /// Returns the distance between two finite 3D lines.
    /// For parallel lines the distance is calculate form the actual finit elements. (like in the other cases.) 
    /// So it is maybe bigger than the parallel offset.
    /// For Coincident and intersecting lines below a tolerance of 1e-16 this always returns 0.0.
    static member inline distanceBetweenLines(lnA , lnB) : float=
        let k,u,v =  Line3D.intersectionParam(lnA , lnB, 1e-16) // use lower skew tolerance here to not return 0.0 if it actually bigger
        match k with        
        | Intersecting | IntersectingEndsBoth | IntersectingEndsFirst | IntersectingEndsSecond 
        | Continuation | ContinuationFlipped | Overlapping   | Identical| IdenticalFlipped -> 
            0.0           
        | Skew | Apart  | IntersectionKind.Parallel | CoincidentApart 
        | TooShortA | TooShortB | TooShortBoth -> 
            let a  =  lnA.EvaluateAt u 
            let b  = lnB.EvaluateAt v 
            Pnt.distance a b
        

    (*  not very useful because it's hard to find the correct tolerance:

    /// Fast check if two 3D lines are in the same 3D Plane.
    /// This can be used as a fast check to exclude intersection. Infinite lines in the same plane do intersect (unless parallel) 
    /// If you need an exact intersection test purely based on distance use Line3D.intersectLine
    /// It first calculates the signed volume of the Parallelepiped define by three vectors from the four corners of the lines.
    /// Then it checks if it is smaller than given volumeTolerance fo Parallelepiped.
    /// Using the VolumeTolerance makes a positive result not only dependent on the distance of the two 3D lines from each other but also on their lengths. 
    static member inline areInSamePlaneTol volumeTolerance (lnA:Line3D , lnB:Line3D) =  
        // Two non-parallel lines p1+Rv1 and p2+Rv2 intersect if and only if (v1×v2)⋅(p1−p2)=0
        // https://math.stackexchange.com/questions/697124/how-to-determine-if-two-lines-in-3d-intersect
        // vector of line lnA:
        let ax = lnA.ToX-lnA.FromX
        let ay = lnA.ToY-lnA.FromY
        let az = lnA.ToZ-lnA.FromZ
        //vector of line lnB:
        let bx = lnB.ToX-lnB.FromX
        let by = lnB.ToY-lnB.FromY
        let bz = lnB.ToZ-lnB.FromZ
        // cross product:
        let cx = ay * bz - az * by   
        let cy = az * bx - ax * bz   
        let cz = ax * by - ay * bx 
        // vector from start of lnA to start of lnB:
        let vx = lnA.FromX-lnB.FromX
        let vy = lnA.FromY-lnB.FromY
        let vz = lnA.FromZ-lnB.FromZ
        let dot = cx*vx + cy*vy + cz*vz
        abs(dot) < volumeTolerance
    *)



