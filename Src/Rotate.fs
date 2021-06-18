namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open Util

#nowarn "44" // for hidden constructors via Obsolete Attribute

/// 2D Counter Clockwise Rotations in X,  Y or Z plane.
/// Internally stored as a sine and cosine value
/// For arbitrary rotations use Quat.
[<Struct; NoEquality; NoComparison>]
[<IsReadOnly>]
//[<IsByRefLike>]
type Rotate = 
    val sin : float
    val cos : float    

    /// Unsave internal constructor,  public only for inlining.
    [<Obsolete("Unsave internal constructor,  but must be public for inlining. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >] 
    new (sin, cos) = 
        #if DEBUG
        let sum = sin*sin + cos*cos in 
        if 0.99999999 > sum || sum > 1.00000001  then  FsExGeoException.Raise "FsEx.Geo.Rotate Constructor failed for sin:%g and cos:%g.  Because sin*sin + cos*cos needs to be 1.0." sin cos
        #endif
        {sin = sin; cos = cos}      
    
    override r.ToString() =  
        let deg = 
            let mutable w = r.sin
            if w < 0.0 then w <-0.0 // clamp,  to avoid error in asin
            if w > 1.0 then w <-1.0
            w  |> Math.Asin |> toDegrees |> Format.float
        sprintf "FsEx.Geo.Rotate of %s° degrees." deg
    
    ///Construct 2D Rotation from angle in Radians 
    static member createFromRadians alpha =  
        Rotate (sin alpha, cos alpha)
    
    /// Construct 2D Rotation from angle in Degree 
    static member createFromDegrees alpha = 
        let rad = toRadians alpha
        Rotate (sin rad, cos rad)     
        
    member inline r.InRadians = 
        let mutable w = r.sin
        if w < 0.0 then w <-0.0 // clamp,  to avoid error in asin
        if w > 1.0 then w <-1.0
        w  |> Math.Asin 

    member inline r.InDegrees = 
        r.InRadians|> toDegrees 

    member inline r.Inverse = Rotate (-r.sin, r.cos )

    member inline r.AddDegrees(deg:float) = 
        let a =  Math.Asin  r.sin + toRadians deg
        Rotate (sin a, cos a) 
    
    member inline r.AddRadians(rad:float) = 
        let a =  Math.Asin  r.sin + rad
        Rotate (sin a, cos a) 

    member inline r.Add(ro:Rotate) = 
        let a =  Math.Asin r.sin + Math.Asin ro.sin 
        Rotate (sin a, cos a) 
    
    /// Rotate the a 2D Point Counter Clockwise 
    member inline r.Rotate (p:Pt) = Pt (r.cos*p.X - r.sin*p.Y, r.sin*p.X + r.cos*p.Y)
    
    /// Rotate the a 2D Vector Counter Clockwise 
    member inline r.Rotate (v:Vc) = Vc (r.cos*v.X - r.sin*v.Y, r.sin*v.X + r.cos*v.Y)
    
    /// Rotate the a 2D UnitVector Counter Clockwise 
    member inline r.Rotate (v:UnitVc) = UnitVc (r.cos*v.X - r.sin*v.Y, r.sin*v.X + r.cos*v.Y)
    
    /// Rotate the Vector around X axis, from Y to Z Axis, Counter Clockwise looking from right.
    member inline r.RotateOnX (v:Vec) = Vec (v.X,  r.cos*v.Y - r.sin*v.Z, r.sin*v.Y + r.cos*v.Z)
    
    /// Rotate the Vector around Y axis, from Z to X Axis, Counter Clockwise looking from back.
    member inline r.RotateOnY (v:Vec) = Vec ( r.sin*v.Z + r.cos*v.X,  v.Y, r.cos*v.Z - r.sin*v.X) 
    
    /// Rotate the Vector around Z axis, from X to Y Axis, Counter Clockwise looking from top.
    member inline r.RotateOnZ (v:Vec) = Vec (r.cos*v.X - r.sin*v.Y, r.sin*v.X + r.cos*v.Y,  v.Z)
    
    /// Rotate the UnitVector around X axis, from Y to Z Axis, Counter Clockwise looking from right.
    member inline r.RotateOnX (v:UnitVec) = UnitVec (v.X,  r.cos*v.Y - r.sin*v.Z, r.sin*v.Y + r.cos*v.Z)
    
    /// Rotate the UnitVector around Y axis, from Z to X Axis, Counter Clockwise looking from back.
    member inline r.RotateOnY (v:UnitVec) = UnitVec ( r.sin*v.Z + r.cos*v.X,  v.Y, r.cos*v.Z - r.sin*v.X) 
    
    /// Rotate the UnitVector around Z axis, from X to Y Axis, Counter Clockwise looking from top.
    member inline r.RotateOnZ (v:UnitVec) = UnitVec (r.cos*v.X - r.sin*v.Y, r.sin*v.X + r.cos*v.Y,  v.Z) 
    
    /// Rotate the Point around X axis, from Y to Z Axis, Counter Clockwise looking from right.
    member inline r.RotateOnX (v:Pnt) = Pnt (v.X,  r.cos*v.Y - r.sin*v.Z, r.sin*v.Y + r.cos*v.Z)
    
    /// Rotate the Point around Y axis, from Z to X Axis, Counter Clockwise looking from back.
    member inline r.RotateOnY (v:Pnt) = Pnt ( r.sin*v.Z + r.cos*v.X,  v.Y, r.cos*v.Z - r.sin*v.X) 
    
    /// Rotate the Point around Z axis, from X to Y Axis, Counter Clockwise looking from top.
    member inline r.RotateOnZ (v:Pnt) = Pnt (r.cos*v.X - r.sin*v.Y, r.sin*v.X + r.cos*v.Y,  v.Z)
    
    /// Rotate the 2D Point around a center 2D Point. Counter Clockwise.
    member inline r.RotateWithCenter (cen:Pt,  pt:Pt) =  
        let x = pt.X - cen.X  
        let y = pt.Y - cen.Y         
        Pt (r.cos*x - r.sin*y + cen.X, r.sin*x + r.cos*y + cen.X) 

    /// Rotate the 3D Point around a center 3D Point and a X aligned axis, from Y to Z Axis, Counter Clockwise looking from right.
    member inline r.RotateOnXwithCenter (cen:Pnt,  pt:Pnt) =  
        let x = pt.X - cen.X 
        let y = pt.Y - cen.Y 
        let z = pt.Z - cen.Z
        Pnt (x + cen.X,  r.cos*y - r.sin*z + cen.Y, r.sin*y + r.cos*z + cen.Y) 

    /// Rotate the Point around a center Point and a Y aligned axis, from Z to X Axis, Counter Clockwise looking from back.
    member inline r.RotateOnYwithCenter  (cen:Pnt, pt:Pnt) =  
        let x = pt.X - cen.X 
        let y = pt.Y - cen.Y 
        let z = pt.Z - cen.Z
        Pnt ( r.sin*z + r.cos*x + cen.X, y + cen.X, r.cos*z - r.sin*x + cen.X) 
    
    /// Rotate the Point around a center Point and a Z aligned axis, from X to Y Axis, Counter Clockwise looking from top.
    member inline r.RotateOnZwithCenter  (cen:Pnt, pt:Pnt) =  
        let x = pt.X - cen.X  
        let y = pt.Y - cen.Y 
        let z = pt.Z - cen.Z
        Pnt (r.cos*x - r.sin*y + cen.X, r.sin*x + r.cos*y + cen.X, z + cen.X)
   
   
    /// Rotate 2D Point around a center point counter clockwise. Angle given in degrees.    
    static member inline ptWitCen  (cen:Pt)  angDegree (p:Pt)  = (Rotate.createFromDegrees angDegree).RotateWithCenter(cen,p) 
    
    /// Rotate 3D Point around a center point and Z alingned axis. Counter clockwise. Angle given in degrees.    
    static member inline pntWitCen  (cen:Pnt)  angDegree (p:Pnt)  = (Rotate.createFromDegrees angDegree).RotateOnZwithCenter(cen,p) 
   
    /// Rotate 2D Point counter clockwise. Angle given in degrees.    
    static member inline pt     angDegree (p:Pt)  = (Rotate.createFromDegrees angDegree).Rotate p    
    
    /// Rotate 2D Vector counter clockwise. Angle given in degrees.
    static member inline vc     angDegree (v:Vc)  = (Rotate.createFromDegrees angDegree).Rotate v    
    
    /// Rotate 2D UnitVector counter clockwise. Angle given in degrees.
    static member inline unitVc angDegree (v:UnitVc)  = (Rotate.createFromDegrees angDegree).Rotate v   
    
    /// Rotate 3D Point around Z axis. Counter clockwise. Angle given in degrees.    
    static member inline pnt     angDegree (p:Pnt)  = (Rotate.createFromDegrees angDegree).RotateOnZ p    
    
    /// Rotate 3D Vector around Z axis. Counter clockwise. Angle given in degrees.
    static member inline vec     angDegree (v:Vec)  = (Rotate.createFromDegrees angDegree).RotateOnZ v    
    
    /// Rotate 3D UnitVector around Z axis. Counter clockwise. Angle given in degrees.
    static member inline unitVec angDegree (v:UnitVec)  = (Rotate.createFromDegrees angDegree).RotateOnZ v 
