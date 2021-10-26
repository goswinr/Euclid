namespace FsEx.Geo
open System


/// Exception in FsEx.Geo
type FsExGeoException (s:string) = 
    inherit System.Exception(s)


   





    static member Raise msg =  Printf.kprintf (fun s -> raise (FsExGeoException(s))) msg


/// Exception for attempting to divide by a 0.0 or almost 0.0 value 
/// Almost 0.0 is defined by Util.zeroLengthTol (1e-16)
type FsExGeoDivByZeroException (s:string) = 
    inherit System.Exception(s)


   




        
    static member Raise msg = Printf.kprintf (fun s -> raise (FsExGeoDivByZeroException(s))) msg


/// Math Utility functions and values for use within FsEx.Geo
module Util =
    
    /// Tolerance for zero length: 1e-16 in divisions
    [<Literal>]
    let zeroLengthTol = 1e-16 

    /// Math.PI * 2.0
    [<Literal>]
    let twoPi = 6.28318530717959
    
    /// Math.PI * 0.5
    [<Literal>]
    let halfPi = 1.5707963267949
    
    /// Converts Angels from Degrees to Radians
    let inline toRadians degrees =  0.0174532925199433 * degrees //  Math.PI / 180.

    /// Converts Angels from Radians to Degrees
    let inline toDegrees radians = 57.2957795130823 * radians  // 180. / Math.PI

    /// Clamp value between -1.0 and +1.0
    let inline clamp11 (x:float)= 
        if   x < -1.0 then -1.0
        elif x >  1.0 then  1.0
        else                x
    
    /// Clamp value between 0.0 and +1.0
    let inline clamp01 (x:float)= 
        if   x <  0.0 then  0.0
        elif x >  1.0 then  1.0
        else                x    
    
    /// Check if value is between 0.0 and +1.0 inclusive.
    let inline isBetween01 (x:float) = 
        x >= 0.0 && x <= 1.0

    /// A safe arcsine (Inverse Sine) function.
    /// It clamps the input between -1 and 1
    let inline asinSafe a = a|> clamp11|> Math.Asin


    /// A safe arccosine (Inverse Cosine) function.
    /// It clamps the input between -1 and 1
    let inline acosSafe a = a|> clamp11|> Math.Acos

    /// Tests if a number is close to 1.0 by maximum 6 steps of float increment or decrement.
    /// So between 0.99999964 and 1.000000715.
    /// See https://float.exposed
    let inline isOne  x =  
        // 1.000000715 is 6 steps bigger  than 1.0: https://float.exposed/0x3f800006
        // 0.99999964  is 6 steps smaller than 1.0: https://float.exposed/0x3f7ffffa
        0.999999642372131347656 < x && x < 1.00000071525573730469 
        
    /// Tests if a the square root of a number is close to 1.0 by maximum 6 steps of float increment or decrement.
    /// So between 0.99999964 and 1.000000715.
    /// See https://float.exposed
    let inline isOneSq  x =  
        // 1.00000143 is 12 steps bigger  than 1.0: https://float.exposed/0x3f80000c
        // 0.99999928 is 12 steps smaller than 1.0: https://float.exposed/0x3f7ffff4
        0.999999284744262695313 < x && x < 1.00000143051147460938 
        
    /// Tests if a the square root of a number is NOT close to 1.0 by maximum 6 steps of float increment or decrement.
    /// So not between 0.99999964 and 1.000000715.
    /// See https://float.exposed
    let inline isNotOneSq  x =  
        // 1.00000143 is 12 steps bigger  than 1.0: https://float.exposed/0x3f80000c
        // 0.99999928 is 12 steps smaller than 1.0: https://float.exposed/0x3f7ffff4
        0.999999284744262695313 < x && x < 1.00000143051147460938            

    /// Tests if a number is NOT close to 1.0 by maximum 6 steps of float increment or decrement.
    /// So not between 0.99999964 and 1.000000715.
    /// See https://float.exposed
    let inline isNotOne  x =  
        // 1.000000715 is 6 steps bigger  than 1.0: https://float.exposed/0x3f800006
        // 0.99999964  is 6 steps smaller than 1.0: https://float.exposed/0x3f7ffffa
        0.999999642372131347656 > x || x > 1.00000071525573730469     
    
    /// Tests if a number is close to -1.0 by maximum 6 steps of float increment or decrement.
    /// So between -0.99999964 and -1.000000715.
    /// See https://float.exposed
    let inline isMinusOne  x =
        -0.999999642372131347656 > x && x > -1.00000071525573730469 
        
    /// Tests if a number is close to 0.0 by 1e-7
    /// This is approximately the same tolerance that 6 increments of a float are away from 1.0.
    /// See FsEx.Geo.Util.isOne function
    let inline isZero x = 
        -1e-7 < x && x < 1e-7 


    /// Tests if a number is NOT close to 0.0 by 1e-7
    /// This is approximately the same tolerance that 6 increments of a float are away from 1.0.
    /// See FsEx.Geo.Util.isOne function
    let inline isNotZero x = 
        x < -1e-7  ||  x > 1e-7 

    /// Any int will give a valid index for given collection size.
    /// Converts negative indices to positive ones and loops to start after last index is reached.
    /// Returns a valid index for a collection of 'length' items for any integer
    let inline saveIdx i length = 
        let t = i % length
        if t >= 0 then t
        else           t + length


(* 
    /// A standard Unset value. Use this value rather than Double.NaN when a bogus floating point value is required.
    /// This is equivalent to openNURBS ON_UNSET_VALUE
    [<Literal>]
    let UnsetValue = -1.23432101234321E+308


module Units = 

    /// Degree (of Angle) 
    [<Measure>] type deg 
    
    /// Radians 
    [<Measure>] type rad 
    
    /// Converts Angels from Degrees to Radians
    let inline toRadians (degrees:float<deg>)  : float<rad> =  0.0174532925199433<rad/deg> * degrees 

    /// Converts Angels from Radians to Degrees
    let inline toDegrees (radians: float<rad>) :float<deg>  = 57.2957795130823<deg/rad> * radians 
    
    *)
    

