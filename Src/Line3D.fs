namespace FsEx.Geo

open System
open FsEx.Geo.Util

/// A finite line in 3D. Represented by a 3D start and 3D end point.
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

    /// Assumes Line3D to be infinite!
    /// Returns the parameter at which a point is closest to the infinit line.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finit line.
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
    member inline ln.ClosestParameter (p:Pnt) = 
        ln.ClosestParameterInfinite(p)
        |> Util.clamp01  
        
    /// Assumes Line3D to be infinite!
    /// Returns closest point on infinite Line.
    member ln.ClosestPointInfinit (p:Pnt) = 
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
    member ln.ClosestPoint (p:Pnt) = 
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let x = ln.FromX - ln.ToX
        let y = ln.FromY - ln.ToY
        let z = ln.FromZ - ln.ToZ
        let lenSq = x*x + y*y + z*z
        if lenSq < 1e-18 then // corresponds to a line Length of 1e-9
            FsExGeoDivByZeroException.Raise "FsEx.Geo.Line3D.ClosestPoint failed on very short line %O %O" ln p
        let u = ln.FromX - p.X
        let v = ln.FromY - p.Y
        let w = ln.FromZ - p.Z
        let dot = x*u + y*v + z*w
        let t = dot/lenSq
        if t<0.0 then
            Pnt(ln.FromX, ln.FromY, ln.FromZ)
        elif t>1.0 then
            Pnt(ln.ToX, ln.ToY, ln.ToZ)
        else
            let x' = ln.FromX - x*t
            let y' = ln.FromY - y*t
            let z' = ln.FromZ - z*t
            Pnt(x', y', z')

    /// Assumes Line3D to be infinite!
    /// Returns Square distance from point to infinite line.
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

    /// Assumes Line3D to be infinite!
    /// Returns distance from point to infinite line.
    member inline ln.DistanceFromPointInfinite(p:Pnt) =   
        ln.DistanceSqFromPointInfinite(p) |> sqrt   
    
    /// Returns Square distance from point to (finite) line.
    member ln.DistanceSqFromPoint(p:Pnt) =  
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let x = ln.FromX - ln.ToX
        let y = ln.FromY - ln.ToY
        let z = ln.FromZ - ln.ToZ
        let lenSq = x*x + y*y + z*z
        if lenSq < 1e-18 then // corresponds to a line Length of 1e-9
            FsExGeoDivByZeroException.Raise "FsEx.Geo.Line3D.DistanceSqFromPoint failed on very short line %O %O" ln p
        let u = ln.FromX-p.X
        let v = ln.FromY-p.Y
        let w = ln.FromZ-p.Z
        let dot = x*u + y*v + z*w
        let t = dot/lenSq
        if t<0.0 then
            u*u + v*v + w*w
        elif t>1.0 then
            let u = ln.ToX-p.X
            let v = ln.ToY-p.Y
            let w = ln.ToZ-p.Z
            u*u + v*v + w*w
        else
            let x' = ln.FromX - x*t
            let y' = ln.FromY - y*t
            let z' = ln.FromZ - z*t
            let u = x'-p.X
            let v = y'-p.Y
            let w = z'-p.Z
            u*u + v*v + w*w

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
        
    /// Checks if two 3D lines are parallel. Ignoring orientation
    /// Calculates the cross product of the two line vectors. (= the area of the parallelogram)
    /// And checks if it is smaller than 1e-9
    /// (NOTE: for very long lines a higher tolerance might be needed)
    member inline ln.IsParallelTo  (l:Line3D) =         
        // vector of line l:
        let ax = l.ToX-l.FromX
        let ay = l.ToY-l.FromY
        let az = l.ToZ-l.FromZ
        //vector of line ll:
        let bx = ln.ToX-ln.FromX
        let by = ln.ToY-ln.FromY
        let bz = ln.ToZ-ln.FromZ
        // cross product:
        let x = ay * bz - az * by   
        let y = az * bx - ax * bz   
        let z = ax * by - ay * bx 
        (x*x + y*y + z*z) < 3.162278e-05 // sqrt of 1e-9         

    /// Checks if a 3D lines and a 3D vector are parallel. Ignoring orientation
    /// Calculates the cross product of the two line vectors. (= the area of the parallelogram)
    /// And checks if it is smaller than 1e-9
    /// (NOTE: for very long lines a higher tolerance might be needed)
    member inline ln.IsParallelTo (v:Vec) =  
        let ax = v.X
        let ay = v.Y
        let az = v.Z
        let bx = ln.ToX-ln.FromX
        let by = ln.ToY-ln.FromY
        let bz = ln.ToZ-ln.FromZ
        // cross product:
        let x = ay * bz - az * by   
        let y = az * bx - ax * bz   
        let z = ax * by - ay * bx 
        (x*x + y*y + z*z) < 3.162278e-05 // sqrt of 1e-9   
        
    /// Checks if a 3D lines and a 3D unit vector are parallel. Ignoring orientation
    /// Calculates the cross product of the two line vectors. (= the area of the parallelogram)
    /// And checks if it is smaller than 1e-9
    /// (NOTE: for very long lines a higher tolerance might be needed)
    member inline ln.IsParallelTo (v:UnitVec) =         
        let ax = v.X
        let ay = v.Y
        let az = v.Z
        let bx = ln.ToX-ln.FromX
        let by = ln.ToY-ln.FromY
        let bz = ln.ToZ-ln.FromZ
        // cross product:
        let x = ay * bz - az * by   
        let y = az * bx - ax * bz   
        let z = ax * by - ay * bx 
        (x*x + y*y + z*z) < 3.162278e-05 // sqrt of 1e-9         

    /// Checks if two 3D lines are parallel and orientated the same way.
    /// Calculates the cross product of the two line vectors. (= the area of the parallelogram)
    /// And checks if it is smaller than 1e-9
    /// Then calculates the dot product and checks if it is positive.
    /// (NOTE: for very long lines a higher tolerance might be needed)
    member inline ln.IsParallelAndOrientedTo  (l:Line3D) =         
        // vector of line l:
        let ax = l.ToX-l.FromX
        let ay = l.ToY-l.FromY
        let az = l.ToZ-l.FromZ
        //vector of line ll:
        let bx = ln.ToX-ln.FromX
        let by = ln.ToY-ln.FromY
        let bz = ln.ToZ-ln.FromZ
        // cross product:
        let x = ay * bz - az * by   
        let y = az * bx - ax * bz   
        let z = ax * by - ay * bx 
        (x*x + y*y + z*z) < 3.162278e-05 // sqrt of 1e-9
        && 
        ax*bx+ay*by+az*bz > 0.0
    
    /// Checks if a 3D lines and a 3D vector are parallel and orientated the same way.
    /// Calculates the cross product of the two line vectors. (= the area of the parallelogram)
    /// And checks if it is smaller than 1e-9
    /// Then calculates the dot product and checks if it is positive.
    /// (NOTE: for very long lines a higher tolerance might be needed)
    member inline ln.IsParallelAndOrientedTo (v:Vec) =         
        let ax = v.X
        let ay = v.Y
        let az = v.Z
        let bx = ln.ToX-ln.FromX
        let by = ln.ToY-ln.FromY
        let bz = ln.ToZ-ln.FromZ
        // cross product:
        let x = ay * bz - az * by   
        let y = az * bx - ax * bz   
        let z = ax * by - ay * bx 
        (x*x + y*y + z*z) < 3.162278e-05 // sqrt of 1e-9 
        && 
        ax*bx+ay*by+az*bz > 0.0
    
    /// Checks if a 3D lines and a 3D unit vector are parallel and orientated the same way.
    /// Calculates the cross product of the two line vectors. (= the area of the parallelogram)
    /// And checks if it is smaller than 1e-9
    /// Then calculates the dot product and checks if it is positive.
    /// (NOTE: for very long lines a higher tolerance might be needed)
    member inline ln.IsParallelAndOrientedTo (v:UnitVec) =         
        let ax = v.X
        let ay = v.Y
        let az = v.Z
        let bx = ln.ToX-ln.FromX
        let by = ln.ToY-ln.FromY
        let bz = ln.ToZ-ln.FromZ
        // cross product:
        let x = ay * bz - az * by   
        let y = az * bx - ax * bz   
        let z = ax * by - ay * bx 
        (x*x + y*y + z*z) < 3.162278e-05 // sqrt of 1e-9 
        && 
        ax*bx+ay*by+az*bz > 0.0

    /// Checks if two 3D lines are perpendicular. 
    /// Calculates the dot product and checks if it is smaller than 1e-9.
    /// (NOTE: for very long lines a higher tolerance might be needed)
    member inline ln.IsPerpendicularTo (l:Line3D) =         
        let dot = (l.ToX-l.FromX)*(ln.ToX-ln.FromX) + (l.ToY-l.FromY)*(ln.ToY-ln.FromY)+ (l.ToZ-l.FromZ)*(ln.ToZ-ln.FromZ) 
        abs(dot) < 1e-9  

    /// Checks if a 3D lines and a 3D vector are perpendicular. 
    /// Calculates the dot product and checks if it is smaller than 1e-9.
    /// (NOTE: for very long lines a higher tolerance might be needed)
    member inline ln.IsPerpendicularTo (v:Vec) =         
        let dot = v.X*(ln.ToX-ln.FromX) + v.Y*(ln.ToY-ln.FromY) + v.Z*(ln.ToZ-ln.FromZ)
        abs(dot) < 1e-9  
    
    /// Checks if a 3D lines and a 3D unit vector are perpendicular. 
    /// Calculates the dot product and checks if it is smaller than 1e-9.
    /// (NOTE: for very long lines a higher tolerance might be needed)
    member inline ln.IsPerpendicularTo (v:UnitVec) =         
        let dot = v.X*(ln.ToX-ln.FromX) + v.Y*(ln.ToY-ln.FromY) + v.Z*(ln.ToZ-ln.FromZ)
        abs(dot) < 1e-9 

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

    /// Assumes Line3D to be infinite!
    /// Returns the parameter at which a point is closest to the infinit line.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finit line.
    static member inline closestParameterInfinite (p:Pnt) (ln:Line3D)  = ln.ClosestParameterInfinite p

    /// Return the parameter at which a point is closest to the (finite) line.
    /// The result is between 0.0 and 1.0.
    static member inline closestParameter (p:Pnt) (ln:Line3D)  = ln.ClosestParameter p 

    /// Assumes Line3D to be infinite!
    /// Returns closest point on infinite Line.
    static member inline closestPointInfinit (p:Pnt) (ln:Line3D)  = ln.ClosestPointInfinit p        

    /// Returns closest point on (finite) Line.
    static member inline closestPoint (p:Pnt) (ln:Line3D)  = ln.ClosestPoint p    

    /// Assumes Line3D to be infinite!
    /// Returns Square distance from point to infinite line.
    static member inline distanceSqFromPointInfinite(p:Pnt) (ln:Line3D)  = ln.DistanceSqFromPointInfinite p
    
    /// Assumes Line3D to be infinite!
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
   

    /// Fast check if two 3D lines are in the same 3D Plane.
    /// This can be used as a fast check for intersection.
    /// If you need an exact intersection test purely based on distance use Line3D.intersectLine
    /// It first calculates the signed volume of the Parallelepiped define by three vectors from the four corners of the lines.
    /// Then it checks if it is smaller than given volumeTolerance fo Parallelepiped.
    /// Using the VolumeTolerance makes a positive result not only dependent on the distance of the two 3D lines from each other but also on their lengths. 
    static member inline doLinesIntersectTol volumeTolerance (l:Line3D) (ll:Line3D) =  
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

    /// Checks if two finit 3D lines do intersect.
    /// It first calculates the signed volume of the Parallelepiped define by three vectors from the four corners of the lines.
    /// Then it checks if it is smaller than Util.zeroLengthTol.
    /// Using the volume of the Parallelepiped makes a positive result not only dependent on the distance of the two 3D lines from each other but also on their lengths.
    /// This is a fast check for intersection.
    /// If you need an exact intersection test purely based on distance use Line3D.intersectLine
    static member inline doLinesIntersect (l:Line3D) (ll:Line3D) =  
        Line3D.doLinesIntersectTol Util.zeroLengthTol l ll

    /// Assumes Lines to be infinite!
    /// Returns the parameters at which two infinite 3D Lines are closest to each other.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finit line.    
    /// Fails on parallel Lines.
    /// First parameter is on l, second parameter is on ll.
    static member inline intersectLineParametersInfinite (l:Line3D) (ll:Line3D) =        
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
        let a = ax*ax + ay*ay + az*az
        let b = ax*bx + ay*by + az*bz
        let c = bx*bx + by*by + bz*bz
        let d = ax*vx + ay*vy + az*vz
        let e = bx*vx + by*vy + bz*vz
        let det = a*c - b*b
        if abs det > 1e-12 then //not parallel
            let t = (b * e - c * d) / det
            let u = (a * e - b * d) / det
            t,u 
        else // parallel
            //TODO still return two parameters where their distance to each other is minimal(middle point of overlap)
            FsExGeoException.Raise "FsEx.Geo.Line3D.intersectLineParametersInfinite: Lines are parallel l: \r\n%O and ll: \r\n%O" l ll
        
    
    /// Assumes Lines to be infinite!    
    /// Returns the two points where these two infinite lines are closest to each other.
    /// Fails on parallel Lines.
    /// First point is on l, second point is on ll.
    static member inline intersectLinesInfinite (l:Line3D) (ll:Line3D) =        
        let a,b = Line3D.intersectLineParametersInfinite l ll
        l.EvaluateAt a , ll.EvaluateAt b 

    /// Assumes Lines to be infinite!    
    /// Returns the singe points where these two infinite lines actually intersect each other.
    /// Fails if lines are parallel or skew by more than 1e-6 units 
    /// The returned point is exactly on ll.  
    static member intersectLinesInfiniteInOnePoint (l:Line3D) (ll:Line3D) : Pnt = 
        let a,b = Line3D.intersectLinesInfinite l ll
        if Pnt.distance a b > 1e-6 then FsExGeoException.Raise "FsEx.Geo.Line3D.intersectLinesInfiniteInOnePoint: Lines are not intersecting l: \r\n%O and ll: \r\n%O" l ll
        b    
        
    /// Assumes Lines to be infinite!    
    /// Returns the distance between two infinite lines.
    /// Fails on parallel Lines.    
    static member inline distanceBetweenInfiniteLines l ll =
        // TODO make it not fail on Parallel lines !
        let a,b = Line3D.intersectLinesInfinite l ll
        Pnt.distance a b

    (*
        TODO !! fix sorting of points and test !
    
    /// Returns the parameters at which two (finite) 3D Lines are closest to each other.
    /// The results are both between 0.0 and 1.0.
    /// Fails on parallel Lines.
    /// First parameter is on l, second parameter is on ll.
    static member inline intersectLineParameters (l:Line3D) (ll:Line3D) = 
        //https://stackoverflow.com/a/34604574/969070 but DP and DQ in wrong order !        
        let ax = l.FromX - l.ToX  
        let ay = l.FromY - l.ToY  
        let az = l.FromZ - l.ToZ
        let bx = ll.FromX - ll.ToX 
        let by = ll.FromY - ll.ToY 
        let bz = ll.FromZ - ll.ToZ
        let vx = ll.FromX - l.FromX
        let vy = ll.FromY - l.FromY
        let vz = ll.FromZ - l.FromZ
        let a = ax*ax + ay*ay + az*az
        let b = ax*bx + ay*by + az*bz
        let c = bx*bx + by*by + bz*bz
        let d = ax*vx + ay*vy + az*vz
        let e = bx*vx + by*vy + bz*vz
        let det = a*c - b*b
        if abs det < 1e-12 then FsExGeoException.Raise "FsEx.Geo.Line3D.intersectLineParameters: Lines are parallel l: \r\n%O and ll: \r\n%O" l ll
        let t = (b * e - c * d) / det
        let u = (a * e - b * d) / det

        if isBetween01 t && isBetween01 u then 
            t,u
        else 
            let tt = 
                if a < 1e-18 then // corresponds to a line Length of 1e-9 
                    0.0
                else
                    // project the 2 other endpoints of the two lines onto the lines
                    let tLLfrom = 
                        //let u = l.FromX-ll.FromX
                        //let v = l.FromY-ll.FromY
                        //let w = l.FromZ-ll.FromZ
                        //let dot = x*u + y*v + z*w
                        //let tt  = dot / lenSq
                        -d / a // negate because vx in in wrong order : ll.FromX - l.FromX not l.FromX-ll.FromX

                    let tLLto = 
                        let u = l.FromX-ll.ToX
                        let v = l.FromY-ll.ToY
                        let w = l.FromZ-ll.ToZ
                        let dot = ax*u + ay*v + az*w
                        dot / a


        // project the 4 endpoints of the two lines onto the lines
        let tLLfrom = 
            //let u = l.FromX-ll.FromX
            //let v = l.FromY-ll.FromY
            //let w = l.FromZ-ll.FromZ
            //let dot = x*u + y*v + z*w
            //let tt  = dot / lenSq
            -d / a // negate because vx in in wrong order : ll.FromX - l.FromX not l.FromX-ll.FromX

        let tLLto = 
            let u = l.FromX-ll.ToX
            let v = l.FromY-ll.ToY
            let w = l.FromZ-ll.ToZ
            let dot = ax*u + ay*v + az*w
            dot / a

        let uLfrom = 
            //let u = ll.FromX-l.FromX
            //let v = ll.FromY-l.FromY
            //let w = ll.FromZ-l.FromZ
            //let dot = bx*u + by*v + bz*w
            //dot / c
            e / c

        let uLto =
            let u = ll.FromX-l.ToX
            let v = ll.FromY-l.ToY
            let w = ll.FromZ-l.ToZ
            let dot = bx*u + by*v + bz*w
            dot / c

            
        0.,0.    

    /// Returns the two points where the two (finit) lines  are closest to each other.
    /// Fails on parallel Lines.
    /// First point is on l, second point is on ll.
    static member inline intersectLines (l:Line3D) (ll:Line3D) =    
        let a,b = Line3D.intersectLineParameters l ll
        l.EvaluateAt a , ll.EvaluateAt b 
    
    /// Returns the single points where these two (finite) lines actually intersect each other.
    /// Fails if lines are parallel , skew, or apart by more than 1e-6 units 
    /// The returned point is exactly on ll.  
    static member intersectLinesInOnePoint (l:Line3D) (ll:Line3D) : Pnt = 
        let a,b = Line3D.intersectLines l ll
        if Pnt.distance a b > 1e-6 then FsExGeoException.Raise "FsEx.Geo.Line3D.intersectLinesInOnePoint: Lines are not intersecting l: \r\n%O and ll: \r\n%O" l ll
        b  

    /// Returns the distance between two (finite) 3D lines.
    /// Fails on parallel Lines. 
    static member inline distanceBetweenLines l ll=
        // TODO make it not fail on Parallel lines !
        let a,b = Line3D.intersectLines l ll
        Pnt.distance a b 
    *)



    
    

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
            

    



