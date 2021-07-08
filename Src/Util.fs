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
    
    /// Tolerance for zero length: 1e-16 in divisions
    [<Literal>]
    let zeroLenghtTol = 1e-16 
    
    /// Converts Angels from Degrees to Radians
    let inline toRadians degrees =  0.0174532925199433 * degrees //  Math.PI / 180.

    /// Converts Angels from Radians to Degrees
    let inline toDegrees radians = 57.2957795130823 * radians  // 180. / Math.PI

    /// Math.PI * 2.0
    let twoPi = 6.28318530717959
    
    /// Math.PI * 0.5
    let halfPi = 1.5707963267949

    /// Clamp value between -1.0 and +1.0
    let inline clamp11 (x:float)= 
        if   x < -1.0 then -1.0
        elif x >  1.0 then  1.0
        else                x
    
    /// Clamp value between 0.0 and +1
    let inline clamp01 (x:float)= 
        if   x <  0.0 then  0.0
        elif x >  1.0 then  1.0
        else                x    

    /// Tests if a number is close to 1.0 by maximum 6 steps of float increment or decrement.
    /// So between 0.99999964 and 1.000000715.
    /// See https://float.exposed
    let inline isOne  x =  
        // 1.000000715 is 6 steps bigger  than 1.0: https://float.exposed/0x3f800006
        // 0.99999964  is 6 steps smaller than 1.0: https://float.exposed/0x3f7ffffa
        0.999999642372131347656 < x && x < 1.00000071525573730469 
    
    /// Tests if a number is close to -1.0 by maximum 6 steps of float increment or decrement.
    /// So between -0.99999964 and -1.000000715.
    /// See https://float.exposed
    let inline isMinusOne  x =
        -0.999999642372131347656 > x && x > -1.00000071525573730469 
        
    /// Tests if a number is close to 0.0 by 1e-7
    /// This is aproximatly the same tolerance that 6 incremets of a float are away from 1.0.
    /// See FsEx.Geo.Util.isOne function
    let inline isZero x = 
        -1e-7 < x && x < 1e-7 

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
    

