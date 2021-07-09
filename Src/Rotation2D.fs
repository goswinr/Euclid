namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open FsEx.Geo.Util

#nowarn "44" // for hidden constructors via Obsolete Attribute

/// 2D Counter Clockwise Rotations in X,  Y or Z plane.
/// Internally stored as a sine and cosine value
/// For arbitrary rotations use Quaternions or 4x4 Matrix. 
/// However this module has much better performance than the more general Matrix4x4 or a Quaternion
[<Struct; NoEquality; NoComparison>]
[<IsReadOnly>]
//[<IsByRefLike>]
type Rotation2D = 
    val sin : float
    val cos : float    

    /// Unsafe internal constructor,  public only for inlining.
    [<Obsolete("Unsafe internal constructor,  but must be public for inlining. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >] 
    new (sin, cos) = 
        #if DEBUG
        let sum = sin*sin + cos*cos in 
        if 0.99999999 > sum || sum > 1.00000001  then  FsExGeoException.Raise "FsEx.Geo.Rotate Constructor failed for sin:%g and cos:%g.  Because sin*sin + cos*cos needs to be 1.0." sin cos
        #endif
        {sin = sin; cos = cos}      
    
    override r.ToString() =  
        let deg =  r.sin  |> asinSafe |> toDegrees |> Format.float
        sprintf "FsEx.Geo.Rotation2D of %s° degrees." deg
    
    ///Construct 2D Rotation from angle in Radians 
    static member createFromRadians alpha =  
        Rotation2D (sin alpha, cos alpha)
    
    /// Construct 2D Rotation from angle in Degree 
    static member createFromDegrees alpha = 
        let rad = toRadians alpha
        Rotation2D (sin rad, cos rad)     
        
    member inline r.InRadians = 
        r.sin  |>  asinSafe 

    member inline r.InDegrees = 
        r.InRadians|> toDegrees 
    
    /// Returns the rotation in the opposite direction 
    member inline r.Inverse = Rotation2D (-r.sin, r.cos )

    /// Create a new Rotation that adds to the existing one 
    member inline r.AddDegrees(deg:float) = 
        let a =  asinSafe  r.sin + toRadians deg
        Rotation2D (sin a, cos a) 
    
    /// Create a new Rotation that adds to the existing one 
    member inline r.AddRadians(rad:float) = 
        let a =  asinSafe  r.sin + rad
        Rotation2D (sin a, cos a) 
    
    /// Create a new Rotation that adds to the existing one 
    member inline r.Add(ro:Rotation2D) = 
        let a =  asinSafe r.sin + asinSafe ro.sin 
        Rotation2D (sin a, cos a) 
    
   
