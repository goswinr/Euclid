namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open FsEx.Geo.Util

#nowarn "44" // for hidden constructors via Obsolete Attribute

/// 2D Counter Clockwise Rotation. It can be applied in world  X, Y or Z plane.
/// Internally stored just as a sine and cosine value
/// For arbitrary rotations use Quaternions or 4x4 Matrix. 
/// However this module has much better performance than the more general Matrix4x4 or a Quaternion
[<Struct; NoEquality; NoComparison>]
[<IsReadOnly>]
//[<IsByRefLike>]
type Rotation2D = 
    /// The sine component of this rotation
    val Sin : float
    
    /// The cosine component of this rotation
    val Cos : float    

    /// Unsafe internal constructor,  public only for inlining.
    [<Obsolete("Unsafe internal constructor,  but must be public for inlining. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >] 
    new (sin, cos) = 
        #if DEBUG
        let sum = sin*sin + cos*cos in 
        if 0.99999999 > sum || sum > 1.00000001  then  FsExGeoException.Raise "FsEx.Geo.Rotate Constructor failed for sin:%g and cos:%g.  Because sin*sin + cos*cos needs to be 1.0." sin cos
        #endif
        {Sin = sin; Cos = cos}      
    
    /// Format rotation into string showing angle in degrees as nicely formatted floating point number.
    override r.ToString() =  
        let deg =  r.Sin  |> asinSafe |> toDegrees |> Format.float
        sprintf "FsEx.Geo.Rotation2D of %s° degrees." deg
    
    ///Construct 2D Rotation from angle in Radians 
    static member createFromRadians rad =  
        Rotation2D (sin rad, cos rad)
    
    /// Construct 2D Rotation from angle in Degree 
    static member createFromDegrees deg = 
        let rad = toRadians deg
        Rotation2D (sin rad, cos rad)     
    
    /// Returns the angle represented by this Rotation in Radians
    member inline r.InRadians = 
        r.Sin  |>  asinSafe 
    
    /// Returns the angle represented by this Rotation in Degrees
    member inline r.InDegrees = 
        r.InRadians|> toDegrees 
    
    /// Returns the Rotation in the opposite direction 
    member inline r.Inverse = Rotation2D (-r.Sin, r.Cos )

    /// Create a new Rotation that adds Rotation to the existing one 
    member inline r.Add(ro:Rotation2D) = 
        //use 2x2 matrix multiplication logic for better performance:        
        Rotation2D  ( r.Sin*ro.Cos + r.Cos*ro.Sin 
                    , r.Cos*ro.Cos - r.Sin*ro.Sin)
    
    /// Create a new Rotation that adds and angle in Degrees to the existing one 
    member inline r.AddDegrees(deg:float) = 
        r.Add(Rotation2D.createFromDegrees deg)
    
    /// Create a new Rotation that adds and angle in Radians to the existing one 
    member inline r.AddRadians(rad:float) = 
        r.Add(Rotation2D.createFromRadians rad)

    
   
