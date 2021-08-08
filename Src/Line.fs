namespace FsEx.Geo

open System
open FsEx.Geo.Util

/// a finite line in 3D represneted by a 3D start and 3D end point.
type [<Struct>] Line =
    val From : Pnt
    val To   : Pnt
    
    //Create Line from 3D start point and 3D end point.
    new (a,b) = {From=a; To=b}
    
    //Create Line from 3D start point's x,y and z  and 3D end point's x,y and z.
    new (a,b,c,u,v,w) = {From=Pnt(a,b,c); To=Pnt(u,v,w)}
    
    /// Same as ln.Vector
    member inline ln.Tangent = ln.To-ln.From
    
    /// Same as ln.Tangent
    member inline ln.Vector = ln.To-ln.From
    
    member inline ln.IsZeroLength = ln.Tangent.IsZero
    
    member inline ln.IsTiny tol = ln.Tangent.IsTiny tol
    
    /// Evaluate line at a given parameter ( parameters 0.0 to 1.0 are on the line )
    member inline ln.At (p:float) = ln.From + p*(ln.To-ln.From)
    
    member inline ln.Mid = (ln.To-ln.From)* 0.5
    
    member inline ln.Length = ln.Tangent.Length
    
    member inline ln.Reversed = Line(ln.To, ln.From)
    
    /// return the parameter at which a point is closest.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the line
    member inline ln.ClosestPointParameter (pt:Pnt) = 
        let v = ln.Tangent
        let len = v.LengthSq
        if len < 1e-9 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line.ClosestPointParameter faild on very short line %A %A" ln pt
        -((ln.From-pt) * v) / len //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
    
    /// Distance to infinite line
    member inline ln.DistanceToPoint (pt:Pnt) = 
        let len = ln.Length
        if len < 1e-9 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line.DistanceToPoint failed on very short line %A %A" ln pt    
        (Vec.cross(pt-ln.From,pt-ln.To)).Length / len //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
    
    /// Returns closest point on infinite line, use Line.ClPtParam to check domain.
    member inline ln.ClosestPoint (p:Pnt) = ln.At (ln.ClosestPointParameter p) 
        
    /// Line from Point at Parameter a to Point at Parameter b
    member inline ln.Segment(a, b) = Line(ln.At a, ln.At b)  
    
    member inline ln.Translate (v:Vec) = Line(ln.From+v,ln.To+v) 
    
    override ln.ToString() = sprintf "FsEx.Geo.Line(%O to %O)" ln.From ln.To

    //-----------------------------------------------------------------------------------
    // STATIC MEMBERS

    static member inline createFromVec (v:Vec) = Line(Pnt(0.,0.,0.),Pnt(v.X,v.Y,v.Z))

    static member inline start (l:Line) = l.From

    static member inline ende (l:Line) = l.To
    
    static member inline length (l:Line) = l.Length

    static member inline reverse (l:Line) = l.Reversed

    /// Applies a 4x4 transformation matrix
    static member inline transform (m:Matrix) (l:Line) = Line(Pnt.transform m l.From, Pnt.transform m l.To)

    static member inline areInSameDirection (ll:Line) (l:Line) = l.Tangent*ll.Tangent > 0.0
    
    /// Evaluate line at a given parameter ( parameters 0.0 to 1.0 are on the line )
    static member inline at f (ln:Line)  = ln.At f

    /// Finds point at given distance from line start
    static member inline pointAtDistance dist (ln:Line) = 
        let l=ln.Length in 
        if l < 1e-6 then FsExGeoDivByZeroException.Raise " Line %O to short for finding point at a Distance" ln
        ln.At (dist/l)        

    /// Returns new Line from Point at Parameter a to Point at Parameter b
    static member inline segment a b (ln:Line) = ln.Segment (a, b)

    /// Returns new Line with given length, going out from start in direction of end
    static member inline withLengthFromStart len (ln:Line) = Line(ln.From, Pnt.distPt(ln.From,ln.To,len))
    
    /// Returns new Line terminationg at LineEnd with given length coming from direction of start.
    static member inline withLengthToEnd len (ln:Line) = Line(Pnt.distPt (ln.To,ln.From,len) ,ln.To)

    static member inline closestPointParameter pt (ln:Line)  = ln.ClosestPointParameter pt

    static member inline closestPoint pt (ln:Line)  = ln.ClosestPoint pt

    static member inline distanceToPoint pt (ln:Line) = ln.DistanceToPoint pt 

    /// Set Line start point 
    static member inline setStart pt (ln:Line) = Line (pt, ln.To)   
      
    /// Set Line end point    
    static member inline setEnd pt (ln:Line) = Line (ln.From, pt)

    /// Checks if the signed volume of the parallelepipeds define by three vectors from the four corners is smaller than Util.zeroLengthTol
    static member inline doLinesIntersect (l:Line) (ll:Line) =  
        abs (Vec.determinant (l.To-l.From,ll.From-l.From,ll.To-l.From)) < zeroLengthTol

    /// Returns the parameter on the two lines where these lines are closest to each other
    /// Fails on parallel Lines.
    static member inline intersectLine (l:Line) (ll:Line) =        
        let ba, cd, bd = l.From - l.To , ll.From - ll.To , l.From - ll.From
        if ba.IsTiny 1e-9 then FsExGeoException.Raise "FsEx.Geo.xLineLine: l too short: l: %O ll: %O" l ll
        if cd.IsTiny 1e-9 then FsExGeoException.Raise "FsEx.Geo.xLineLine: ll too short:l: %O ll: %O" l ll 
        let x1, y1, z1 = - ba*ba ,   cd*ba ,   bd*ba 
        let x2, y2, z2 =   ba*cd , - cd*cd , - bd*cd
        let bAse = x2*y1 - x1*y2        
        if abs bAse < 1e-9 then FsExGeoException.Raise "FsEx.Geo.intersectLine: Lines are parallel: l: %O ll: %O" l ll 
        else (y2*z1 - y1*z2)/bAse , -(x2*z1 - x1*z2)/bAse
        
    
    static member inline xLineLinePt (l:Line) (ll:Line)=
        let a,b = Line.intersectLine l ll
        l.At a , ll.At b
    
    static member inline are3PtsInLine tol (a:Pnt) (b:Pnt) (c:Pnt) = Vec.cross (b-a,c-a) |> Vec.isTiny tol
           
    static member inline lineLineDistance a b =
        let pa,pb = Line.intersectLine a b
        Pnt.distance ( a.At(pa |> clamp01)) (b.At(pb |> clamp01) )
    
    static member inline lineLineDistanceInfinite a b =
        let pa,pb = Line.intersectLine a b
        Pnt.distance (a.At pa) (b.At pb) 
    
    static member inline translate (v:Vec) (ln:Line) =  Line(ln.From+v,ln.To+v)
    
    /// Extend by absolute amount
    static member inline extend (da:float) (db:float) (ln:Line) = let t = ln.Tangent.Unitized in Line(ln.From - t*da , ln.To + t*db )

    /// Get distance from start of line to point projected onto line, may be negative
    static member inline lengthToPtOnLine (line:Line) pt = line.Tangent.Unitized * (pt-line.From)

    /// Intersects infinite line with cone that has it's Axis on ZAxis.
    /// coneRadius -> coneBaseZ -> coneTipZ ->  (ln:Line) -> Parameter*Parameter on the line
    static member inline xCone coneRadius coneBaseZ coneTipZ (ln:Line) =        
        let lam = coneRadius / ( coneBaseZ-coneTipZ )
        let lam = lam * lam
        let v = ln.Tangent
        let f2 = lam*v.Z*v.Z - v.X*v.X - v.Y*v.Y
        if abs f2 < zeroLengthTol then FsExGeoDivByZeroException.Raise "FsEx.Geo.Line.xCone failed for special case coneRadius:%g coneBaseZ:%g coneTipZ:%g %O " coneRadius coneBaseZ coneTipZ ln
        let f1 = 2.*lam*ln.From.Z*v.Z - 2.*lam*v.Z*coneTipZ - 2.*v.Y*ln.From.Y - 2.*ln.From.X*v.X
        let f0 = lam * ln.From.Z*ln.From.Z + lam*coneTipZ*coneTipZ - 2.*ln.From.Z*coneTipZ*lam - ln.From.Y*ln.From.Y - ln.From.X*ln.From.X
        let sqrtPart = sqrt(f1**2. - 4.*f2*f0)
        let div = 1. / (2. * f2)
        (-f1 + sqrtPart) * div ,
        (-f1 - sqrtPart) * div
 



    

