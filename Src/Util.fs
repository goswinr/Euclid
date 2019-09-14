namespace FsEx.Geo



/// Exception in FsEx.Geo
type FsExGeoException (s:string) = 
   inherit System.Exception(s)

   static member inline Raise msg = raise (new FsExGeoException(msg))

/// Exception values that are too small to y a divisor
type FsExGeoDivByZeroException (s:string) = 
   inherit System.Exception(s)

   static member inline Raise msg = raise (new FsExGeoDivByZeroException(msg))


module Util =
    
    /// Tolerance for zero length: 1e-12 in divisions
    [<Literal>]
    let zeroLenghtTol = 1e-16 
    
    /// Converts Angels from Degrees to Radians
    let inline toRadians degrees =  0.0174532925199433 * degrees //  Math.PI / 180.

    /// Converts Angels from Radians to Degrees
    let inline toDegrees radians = 57.2957795130823 * radians  // 180. / Math.PI

    /// Math.PI * 2.0
    let twoPi = 6.28318530717959

    /// clamp value between -1.0 and +1.0
    let inline clamp11 (x:float)= 
        if   x < -1.0 then -1.0
        elif x >  1.0 then  1.0
        else                x
    
    /// clamp value between 0.0 and +1
    let inline clamp01 (x:float)= 
        if   x <  0.0 then  0.0
        elif x >  1.0 then  1.0
        else                x

(* module Units = 

    /// Degree (of Angle) 
    [<Measure>] type deg 
    
    /// Radians 
    [<Measure>] type rad 
    
    /// Converts Angels from Degrees to Radians
    let inline toRadians (degrees:float<deg>)  : float<rad> =  0.0174532925199433<rad/deg> * degrees 

    /// Converts Angels from Radians to Degrees
    let inline toDegrees (radians: float<rad>) :float<deg>  = 57.2957795130823<deg/rad> * radians 
    
    *)
    

