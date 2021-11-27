namespace FsEx.Geo
open System


/// OptionalAttribute for member parameters
type internal OPT = Runtime.InteropServices.OptionalAttribute

/// DefaultParameterValueAttribute for member parameters
type internal DEF =  Runtime.InteropServices.DefaultParameterValueAttribute

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
    
    /// A type to represent the result of testing if a float is close to zero, close to one or in between.
    /// A number is close to 1.0 by maximum 6 steps of float increment or decrement.
    /// So between 0.99999964 and 1.000000715. Same step size for zero.
    /// This can be used for clamping the domain on a line.
    type ZeroToOne =
        |Zero
        |One
        |Between
        |Outside


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
    let inline clampBetweenMinusOneAndOne (x:float)= 
        if   x < -1.0 then -1.0
        elif x >  1.0 then  1.0
        else                x
    
    /// Clamp value between 0.0 and +1.0
    let inline clampBetweenZeroAndOne (x:float)= 
        if   x <  0.0 then  0.0
        elif x >  1.0 then  1.0
        else                x    

    /// A safe arcsine (Inverse Sine) function.
    /// It clamps the input between -1 and 1
    let inline asinSafe a = a|> clampBetweenMinusOneAndOne|> Math.Asin


    /// A safe arccosine (Inverse Cosine) function.
    /// It clamps the input between -1 and 1
    let inline acosSafe a = a|> clampBetweenMinusOneAndOne|> Math.Acos

    /// The float number that is 9 increments bigger than 1.0.
    /// This is approx 1.0 + 1e-6
    /// see https://float.exposed/0x3f800009
    [<Literal>]
    let ``1.0 + 1e-6`` = 1.00000107288360595703

    /// The float number that is 16 increments smaller than 1.0.
    /// This is approx 1.0 - 1e-6
    /// see https://float.exposed/0x3f7ffff0
    [<Literal>]
    let ``1.0 - 1e-6`` = 0.99999904632568359375

   /// The float number that is 9 increments smaller than -1.0.
    /// This is approx -1.0 + 1e-6
    /// see https://float.exposed/0xbf800009
    [<Literal>]
    let ``-1.0 + 1e-6`` = -1.00000107288360595703

    /// The float number that is 16 increments bigger than 1.0.
    /// This is approx 1.0 - 1e-6
    /// see https://float.exposed/0xbf7ffff0
    [<Literal>]
    let ``-1.0 - 1e-6`` = -0.99999904632568359375 

    /// Tests if a number is close to 1.0 by a 1e-6 tolerance.
    /// This is a float increment of 6 steps or decrement of 16 steps.   
    let inline isOne  x = 
        ``1.0 - 1e-6`` < x && x < ``1.0 + 1e-6`` 

    /// Tests if a number is NOT close to 1.0 by a 1e-6 tolerance.
    /// This is a float increment of 6 steps or decrement of 16 steps.   
    let inline isNotOne  x = 
        ``1.0 - 1e-6`` > x || x > ``1.0 + 1e-6``     
    
    /// Tests if a number is close to minus 1.0 by a 1e-6 tolerance.
    /// This is a float increment of 6 steps or decrement of 16 steps.   
    let inline isMinusOne  x = 
        ``-1.0 - 1e-6`` > x && x > ``-1.0 + 1e-6``     

    /// Tests if a number is close to 0.0 by 1e-6
    /// This is approximately the same tolerance that 6 increments of a float are away from 1.0.
    /// See FsEx.Geo.Util.isOne function
    let inline isZero x = 
        -1e-6 < x && x < 1e-6 

    /// Tests if a number is NOT close to 0.0 by 1e-6
    /// This is approximately the same tolerance that 6 increments of a float are away from 1.0.
    /// See FsEx.Geo.Util.isOne function
    let inline isNotZero x = 
        x < -1e-6  ||  x > 1e-6 
    
    /// Check if value is between 0.0 and +1.0 inclusive.
    let inline isBetweenZeroAndOne (x:float) = 
        x >= 0.0 && x <= 1.0

    /// Tests if a float is close to zero, close to one or in between.
    /// A number is close to 1.0 by maximum 6 steps of float increment or decrement.
    /// So between 0.99999964 and 1.000000715. Same step size for zero.
    let inline isZeroOneOrBetween  (x:float) = 
        if   isZero      x then Zero
        elif isOne       x then One
        elif isBetweenZeroAndOne x then Between
        else Outside
        
    /// Check if value is between 0.0 and +1.0 inclusive a tolerance of 1e-6 .
    let inline isBetweenZeroAndOneTolerant (x:float) = 
        -1e-6 < x && x < ``1.0 + 1e-6`` 

    /// Any int will give a valid index for given collection size.
    /// Converts negative indices to positive ones and loops to start after last index is reached.
    /// Returns a valid index for a collection of 'length' items for any integer
    let inline saveIdx i length = 
        let t = i % length
        if t >= 0 then t
        else           t + length

/// Precalculated cosine values for faster checking the angles of dot products of unit vectors.
[<RequireQualifiedAccess>]
module Cosine = 
    
    // for fsi: printfn "%.18f" (cos( 0.25 * (System.Math.PI / 180.)))

    /// The cosine of an angle of 0.01 degrees.
    /// This is exactly 0.999999984769129
    [<Literal>]
    let ``0.01`` = 0.999999984769129
    
    /// The cosine of an angle of 0.05 degrees.
    /// This is exactly 0.999999619228249
    [<Literal>]
    let ``0.05`` = 0.999999619228249
    
    /// The cosine of an angle of 0.1 degrees.
    /// This is exactly 0.999998476913288
    [<Literal>]
    let ``0.1`` = 0.999998476913288
    
    /// The cosine of an angle of 0.25 degrees.
    /// This is exactly 0.999990480720734
    [<Literal>]
    let ``0.25`` = 0.999990480720734
    
    /// The cosine of an angle of 0.5 degrees.
    /// This is exactly 0.999961923064171
    [<Literal>]
    let ``0.5`` = 0.999961923064171
    
    /// The cosine of an angle of 1.0 degrees.
    /// This is exactly 0.999847695156391
    [<Literal>]
    let ``1.0`` = 0.999847695156391
    
    /// The cosine of an angle of 3.0 degrees.
    /// This is exactly 0.998629534754574
    [<Literal>]
    let ``3.0`` = 0.998629534754574
    
    /// The cosine of an angle of 5.0 degrees.
    /// This is exactly 0.996194698091746
    [<Literal>]
    let ``5.0`` = 0.996194698091746
    
    /// The cosine of an angle of 10.0 degrees.
    /// This is exactly 0.984807753012208
    [<Literal>]
    let ``10.0`` = 0.984807753012208
    
    /// The cosine of an angle of 45.0 degrees.
    /// This is exactly 0.707106781186548
    [<Literal>]
    let ``45.0`` = 0.707106781186548
    
    /// The cosine of an angle of 60.0 degrees.
    /// This is exactly 0.5
    [<Literal>]
    let ``60.0`` = 0.5
    
    /// The cosine of an angle of 87.0 degrees.
    /// This is exactly 0.052335956242944
    [<Literal>]
    let ``87.0`` = 0.052335956242944
    
    /// The cosine of an angle of 89.0 degrees.
    /// This is exactly 0.0174524064372836
    [<Literal>]
    let ``89.0`` = 0.0174524064372836
    
    /// The cosine of an angle of 89.75 degrees.
    /// This is exactly 0.00436330928474658
    [<Literal>]
    let ``89.75`` = 0.00436330928474658
    
    /// The cosine of an angle of 89.9 degrees.
    /// This is exactly 0.00174532836589826
    [<Literal>]
    let ``89.9`` = 0.00174532836589826
    
    /// The cosine of an angle of 90.1 degrees.
    /// This is exactly -0.00174532836589814
    [<Literal>]
    let ``90.1`` = -0.00174532836589814
    
    /// The cosine of an angle of 90.25 degrees.
    /// This is exactly -0.00436330928474646
    [<Literal>]
    let ``90.25`` = -0.00436330928474646
    
    /// The cosine of an angle of 91.0 degrees.
    /// This is exactly -0.0174524064372835
    [<Literal>]
    let ``91.0`` = -0.0174524064372835
    
    /// The cosine of an angle of 93.0 degrees.
    /// This is exactly -0.0523359562429438
    [<Literal>]
    let ``93.0`` = -0.0523359562429438
    
    /// The cosine of an angle of 120.0 degrees.
    /// This is exactly -0.5
    [<Literal>]
    let ``120.0`` = -0.5
    
    /// The cosine of an angle of 135.0 degrees.
    /// This is exactly -0.707106781186547
    [<Literal>]
    let ``135.0`` = -0.707106781186547
    
    /// The cosine of an angle of 177.0 degrees.
    /// This is exactly -0.998629534754574
    [<Literal>]
    let ``177.0`` = -0.998629534754574
    
    /// The cosine of an angle of 179.0 degrees.
    /// This is exactly -0.999847695156391
    [<Literal>]
    let ``179.0`` = -0.999847695156391
    
    /// The cosine of an angle of 179.75 degrees.
    /// This is exactly -0.999990480720734
    [<Literal>]
    let ``179.75`` = -0.999990480720734
    
    /// The cosine of an angle of 179.9 degrees.
    /// This is exactly -0.999998476913288
    [<Literal>]
    let ``179.9`` = -0.999998476913288
    
    /// The cosine of an angle of 179.95 degrees.
    /// This is exactly -0.999999619228249
    [<Literal>]
    let ``179.95`` = -0.999999619228249


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
    

