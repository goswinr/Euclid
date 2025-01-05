// a workaround for Fable compiler to not fail on DataContract attribute
// see https://github.com/fable-compiler/Fable/issues/2253
#if FABLE_COMPILER
namespace System.Runtime.Serialization
type DataContract() = inherit System.Attribute() // just shadow the real attribute
type DataMember()   = inherit System.Attribute() // just shadow the real attribute
#endif

namespace Euclid

open System

/// OptionalAttribute for member parameters.
type internal OPT = Runtime.InteropServices.OptionalAttribute

/// DefaultParameterValueAttribute for member parameters.
type internal DEF = Runtime.InteropServices.DefaultParameterValueAttribute

/// Exception in Euclid.
type EuclidException (s:string) =
    inherit Exception(s)

    static member Raisef msg = Printf.kprintf (fun s -> raise (EuclidException(s))) msg
    static member Raise (txt:string) = raise (EuclidException(txt))

    /// This function is much smaller when it gets inlined compared to the Raise (Printf.kprintf) version
    static member Throw1 msg (v:'T) = raise (EuclidException(msg + ": " + v.ToString()))

/// Exception for attempting to divide by a 0.0 or almost 0.0 value.
/// Almost 0.0 is defined by UtilEuclid.zeroLengthTolerance as 1e-12.
type EuclidDivByZeroException (s:string) =
    inherit Exception(s)

    static member Raisef msg = Printf.kprintf (fun s -> raise (EuclidDivByZeroException(s))) msg

    static member Raise (txt:string) = raise (EuclidDivByZeroException(txt))

    /// This function is much smaller when it gets inlined compared to the Raise (Printf.kprintf) version
    static member Throw1 msg (v:'T) = raise (EuclidDivByZeroException(msg + ": " + v.ToString()))

/// Math Utility functions and values for use within Euclid.
module UtilEuclid =

    /// Test is a value is not null.
    let inline notNull x = match x with null -> false | _ -> true

    /// Tolerance for zero length: 1e-12 in divisions and unitizing of vectors.
    [<Literal>]
    let zeroLengthTolerance = 1e-12

    /// Squared Tolerance for zero length in divisions.: 1e-12 * 1e-12 = 1e-24
    [<Literal>]
    let private zeroLengthTolSquared = zeroLengthTolerance * zeroLengthTolerance

    /// Returns true for values smaller than 1e-6 and for NaN
    let inline isTooSmall x =
        // use 'not' to catch a NaN too ( a cross product of infinit long vectors can give a NaN length)
        not ( x > 1e-6 )

    /// Returns true for values smaller than 1e-12 (square of 1e-6)  and for NaN
    let inline isTooSmallSq x =
        // use 'not' to catch a NaN too ( a cross product of infinit long vectors can give a NaN length)
        not ( x > 1e-12 )

    /// Returns true for values smaller than 1e-12 and for NaN
    /// uses UtilEuclid.zeroLengthTolerance
    let inline isTooTiny x =
        // use 'not' to catch a NaN too ( a cross product of infinit long vectors can give a NaN length)
        not ( x > zeroLengthTolerance )

    /// Returns true for values smaller than 1e-24 (square of 1e-12) and for NaN
    /// uses UtilEuclid.zeroLengthTolSquared
    let inline isTooTinySq x =
        // use 'not' to catch a NaN too ( a cross product of infinit long vectors can give a NaN length)
        not ( x > zeroLengthTolSquared)

    /// Returns true for negative number and for NaN
    /// 0.0  and -0.0 is not negative.
    let inline isNegative x =
        not (x >= 0.0)


    /// Math.PI * 2.0
    /// This is equal to 360 degrees in radians.
    [<Literal>]
    let twoPi = 6.2831853071795862 //(Math.PI*2.0).ToString("R")

    /// Math.PI * 0.5
    /// This is equal to 90 degrees in radians.
    [<Literal>]
    let halfPi = 1.5707963267948966 // (Math.PI*0.5).ToString("R")

    /// Converts Angels from Degrees to Radians.
    /// By multiplying with 0.0174... (PI / 180.)
    let inline toRadians degrees = 0.017453292519943295 * degrees //  (Math.PI / 180.).ToString("R")

    /// Converts Angels from Radians to Degrees.
    /// By multiplying with 57.29... (180. / PI)
    let inline toDegrees radians = 57.295779513082323 * radians  // (180. / Math.PI).ToString("R")

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
    let inline asinSafe a = // TODO fail if 'a' is bigger than  1.01 or smaller than -1.01 ??
        a |> clampBetweenMinusOneAndOne |> Math.Asin

    /// A safe arccosine (Inverse Cosine) function.
    /// It clamps the input between -1 and 1
    let inline acosSafe a = // TODO fail if 'a' is bigger than  1.01 or smaller than -1.01 ??
        a |> clampBetweenMinusOneAndOne |> Math.Acos

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
    /// Also returns true for NaN.
    let inline isNotOne x = not (isOne x)


    /// Tests if a number is close to minus 1.0 by a 1e-6 tolerance.
    /// This is a float increment of 6 steps or decrement of 16 steps.
    let inline isMinusOne  x =
        ``-1.0 - 1e-6`` > x && x > ``-1.0 + 1e-6``

    /// Tests if a number is close to 0.0 by 1e-6
    /// This is approximately the same tolerance that 6 increments of a float are away from 1.0.
    /// See Euclid.UtilEuclid.isOne function.
    let inline isZero x =
        -1e-6 < x && x < 1e-6

    /// Tests if a number is NOT close to 0.0 by 1e-6
    /// This is approximately the same tolerance that 6 increments of a float are away from 1.0.
    /// See Euclid.UtilEuclid.isOne function.
    /// Also returns true for NaN.
    let inline isNotZero x = not (isZero x)

    /// Check if value is between 0.0 and +1.0 inclusive.
    let inline isBetweenZeroAndOne (x:float) =
        x >= 0.0 && x <= 1.0

    /// Check if value is between 0.0 and +1.0 inclusive a tolerance of 1e-6 .
    let inline isBetweenZeroAndOneTolerant (x:float) =
        -1e-6 < x && x < ``1.0 + 1e-6``


    /// Match the sign (+ or -) to a given number.
    let matchSign (signedValue:float) (numToMatch:float) =
        if   sign signedValue = sign numToMatch then numToMatch
        else -numToMatch

    /// Any int will give a valid index for given collection size.
    /// Converts negative indices to positive ones and loops to start after last index is reached.
    /// Returns a valid index for a collection of 'length' items for any integer.
    let inline saveIdx i length =
        let t = i % length
        if t >= 0 then t
        else           t + length

/// Precalculated cosine values for faster checking the angles of dot products of unit-vectors.
[<RequireQualifiedAccess>]
module Cosine =

    /// The Unit Of Measure for
    /// the precalculated cosine values.
    /// This UoM helps to avoid that degree or radians angle values are used in the parallel or orthogonality tests.
    [<Measure>]
    type cosine

    (*
    // to generate the below values use :


let print(degree:float) =
    let radians = degree * (System.Math.PI  / 180.)
    let v = cos(radians).ToString("R") // r for round trip
    let deg = degree.ToString("0.0####") // #0.0 ensure at least one trailing zero
    printfn $"""
    /// The cosine of an angle of {degree} degrees.
    /// This is exactly {v}
    [<Literal>]
    let ``{deg}`` = {v}<cosine>
    """
let steps =[
    0.01
    0.025
    0.05
    0.1
    0.25
    0.5
    1.0
    2.5
    5.0
    10.0
    15.0
    20.0
    30.0
    45.0
    ]
for s in steps            do print (s)
for s in steps|> List.rev |> Seq.skip 1 do print (90. - s)
for s in steps            do print (90. + s)
for s in steps|> List.rev |> Seq.skip 1 do print (180. - s)

    *)


    /// The cosine of an angle of 0.01 degrees.
    /// This is exactly 0.9999999847691291
    [<Literal>]
    let ``0.01`` = 0.9999999847691291<cosine>


    /// The cosine of an angle of 0.025 degrees.
    /// This is exactly 0.9999999048070578
    [<Literal>]
    let ``0.025`` = 0.9999999048070578<cosine>


    /// The cosine of an angle of 0.05 degrees.
    /// This is exactly 0.9999996192282494
    [<Literal>]
    let ``0.05`` = 0.9999996192282494<cosine>


    /// The cosine of an angle of 0.1 degrees.
    /// This is exactly 0.9999984769132877
    [<Literal>]
    let ``0.1`` = 0.9999984769132877<cosine>


    /// The cosine of an angle of 0.25 degrees.
    /// This is exactly 0.9999904807207345
    [<Literal>]
    let ``0.25`` = 0.9999904807207345<cosine>


    /// The cosine of an angle of 0.5 degrees.
    /// This is exactly 0.9999619230641713
    [<Literal>]
    let ``0.5`` = 0.9999619230641713<cosine>


    /// The cosine of an angle of 1 degrees.
    /// This is exactly 0.9998476951563913
    [<Literal>]
    let ``1.0`` = 0.9998476951563913<cosine>


    /// The cosine of an angle of 2.5 degrees.
    /// This is exactly 0.9990482215818578
    [<Literal>]
    let ``2.5`` = 0.9990482215818578<cosine>


    /// The cosine of an angle of 5 degrees.
    /// This is exactly 0.9961946980917455
    [<Literal>]
    let ``5.0`` = 0.9961946980917455<cosine>


    /// The cosine of an angle of 10 degrees.
    /// This is exactly 0.984807753012208
    [<Literal>]
    let ``10.0`` = 0.984807753012208<cosine>


    /// The cosine of an angle of 15 degrees.
    /// This is exactly 0.9659258262890683
    [<Literal>]
    let ``15.0`` = 0.9659258262890683<cosine>


    /// The cosine of an angle of 20 degrees.
    /// This is exactly 0.9396926207859084
    [<Literal>]
    let ``20.0`` = 0.9396926207859084<cosine>


    /// The cosine of an angle of 30 degrees.
    /// This is exactly 0.8660254037844387
    [<Literal>]
    let ``30.0`` = 0.8660254037844387<cosine>


    /// The cosine of an angle of 45 degrees.
    /// This is exactly 0.7071067811865476
    [<Literal>]
    let ``45.0`` = 0.7071067811865476<cosine>


    /// The cosine of an angle of 60 degrees.
    /// This is exactly 0.5000000000000001
    [<Literal>]
    let ``60.0`` = 0.5000000000000001<cosine>


    /// The cosine of an angle of 70 degrees.
    /// This is exactly 0.3420201433256688
    [<Literal>]
    let ``70.0`` = 0.3420201433256688<cosine>


    /// The cosine of an angle of 75 degrees.
    /// This is exactly 0.25881904510252074
    [<Literal>]
    let ``75.0`` = 0.25881904510252074<cosine>


    /// The cosine of an angle of 80 degrees.
    /// This is exactly 0.17364817766693041
    [<Literal>]
    let ``80.0`` = 0.17364817766693041<cosine>


    /// The cosine of an angle of 85 degrees.
    /// This is exactly 0.08715574274765814
    [<Literal>]
    let ``85.0`` = 0.08715574274765814<cosine>


    /// The cosine of an angle of 87.5 degrees.
    /// This is exactly 0.04361938736533601
    [<Literal>]
    let ``87.5`` = 0.04361938736533601<cosine>


    /// The cosine of an angle of 89 degrees.
    /// This is exactly 0.0174524064372836
    [<Literal>]
    let ``89.0`` = 0.0174524064372836<cosine>


    /// The cosine of an angle of 89.5 degrees.
    /// This is exactly 0.008726535498373897
    [<Literal>]
    let ``89.5`` = 0.008726535498373897<cosine>


    /// The cosine of an angle of 89.75 degrees.
    /// This is exactly 0.004363309284746582
    [<Literal>]
    let ``89.75`` = 0.004363309284746582<cosine>


    /// The cosine of an angle of 89.9 degrees.
    /// This is exactly 0.0017453283658982615
    [<Literal>]
    let ``89.9`` = 0.0017453283658982615<cosine>


    /// The cosine of an angle of 89.95 degrees.
    /// This is exactly 0.0008726645152351565
    [<Literal>]
    let ``89.95`` = 0.0008726645152351565<cosine>


    /// The cosine of an angle of 89.975 degrees.
    /// This is exactly 0.0004363322991533642
    [<Literal>]
    let ``89.975`` = 0.0004363322991533642<cosine>


    /// The cosine of an angle of 89.99 degrees.
    /// This is exactly 0.00017453292431338717
    [<Literal>]
    let ``89.99`` = 0.00017453292431338717<cosine>


    /// The cosine of an angle of 90.01 degrees.
    /// This is exactly -0.00017453292431348675
    [<Literal>]
    let ``90.01`` = -0.00017453292431348675<cosine>


    /// The cosine of an angle of 90.025 degrees.
    /// This is exactly -0.00043633229915346377
    [<Literal>]
    let ``90.025`` = -0.00043633229915346377<cosine>


    /// The cosine of an angle of 90.05 degrees.
    /// This is exactly -0.000872664515235034
    [<Literal>]
    let ``90.05`` = -0.000872664515235034<cosine>


    /// The cosine of an angle of 90.1 degrees.
    /// This is exactly -0.001745328365898139
    [<Literal>]
    let ``90.1`` = -0.001745328365898139<cosine>


    /// The cosine of an angle of 90.25 degrees.
    /// This is exactly -0.00436330928474646
    [<Literal>]
    let ``90.25`` = -0.00436330928474646<cosine>


    /// The cosine of an angle of 90.5 degrees.
    /// This is exactly -0.008726535498373997
    [<Literal>]
    let ``90.5`` = -0.008726535498373997<cosine>


    /// The cosine of an angle of 91 degrees.
    /// This is exactly -0.017452406437283477
    [<Literal>]
    let ``91.0`` = -0.017452406437283477<cosine>


    /// The cosine of an angle of 92.5 degrees.
    /// This is exactly -0.04361938736533589
    [<Literal>]
    let ``92.5`` = -0.04361938736533589<cosine>


    /// The cosine of an angle of 95 degrees.
    /// This is exactly -0.08715574274765824
    [<Literal>]
    let ``95.0`` = -0.08715574274765824<cosine>


    /// The cosine of an angle of 100 degrees.
    /// This is exactly -0.1736481776669303
    [<Literal>]
    let ``100.0`` = -0.1736481776669303<cosine>


    /// The cosine of an angle of 105 degrees.
    /// This is exactly -0.25881904510252085
    [<Literal>]
    let ``105.0`` = -0.25881904510252085<cosine>


    /// The cosine of an angle of 110 degrees.
    /// This is exactly -0.3420201433256687
    [<Literal>]
    let ``110.0`` = -0.3420201433256687<cosine>


    /// The cosine of an angle of 120 degrees.
    /// This is exactly -0.4999999999999998
    [<Literal>]
    let ``120.0`` = -0.4999999999999998<cosine>


    /// The cosine of an angle of 135 degrees.
    /// This is exactly -0.7071067811865475
    [<Literal>]
    let ``135.0`` = -0.7071067811865475<cosine>


    /// The cosine of an angle of 150 degrees.
    /// This is exactly -0.8660254037844387
    [<Literal>]
    let ``150.0`` = -0.8660254037844387<cosine>


    /// The cosine of an angle of 160 degrees.
    /// This is exactly -0.9396926207859083
    [<Literal>]
    let ``160.0`` = -0.9396926207859083<cosine>


    /// The cosine of an angle of 165 degrees.
    /// This is exactly -0.9659258262890682
    [<Literal>]
    let ``165.0`` = -0.9659258262890682<cosine>


    /// The cosine of an angle of 170 degrees.
    /// This is exactly -0.984807753012208
    [<Literal>]
    let ``170.0`` = -0.984807753012208<cosine>


    /// The cosine of an angle of 175 degrees.
    /// This is exactly -0.9961946980917455
    [<Literal>]
    let ``175.0`` = -0.9961946980917455<cosine>


    /// The cosine of an angle of 177.5 degrees.
    /// This is exactly -0.9990482215818578
    [<Literal>]
    let ``177.5`` = -0.9990482215818578<cosine>


    /// The cosine of an angle of 179 degrees.
    /// This is exactly -0.9998476951563913
    [<Literal>]
    let ``179.0`` = -0.9998476951563913<cosine>


    /// The cosine of an angle of 179.5 degrees.
    /// This is exactly -0.9999619230641713
    [<Literal>]
    let ``179.5`` = -0.9999619230641713<cosine>


    /// The cosine of an angle of 179.75 degrees.
    /// This is exactly -0.9999904807207345
    [<Literal>]
    let ``179.75`` = -0.9999904807207345<cosine>


    /// The cosine of an angle of 179.9 degrees.
    /// This is exactly -0.9999984769132877
    [<Literal>]
    let ``179.9`` = -0.9999984769132877<cosine>


    /// The cosine of an angle of 179.95 degrees.
    /// This is exactly -0.9999996192282494
    [<Literal>]
    let ``179.95`` = -0.9999996192282494<cosine>


    /// The cosine of an angle of 179.975 degrees.
    /// This is exactly -0.9999999048070578
    [<Literal>]
    let ``179.975`` = -0.9999999048070578<cosine>


    /// The cosine of an angle of 179.99 degrees.
    /// This is exactly -0.9999999847691291
    [<Literal>]
    let ``179.99`` = -0.9999999847691291<cosine>






/// Precalculated relative angle discriminant values for faster checking the angles in line line intersection.
[<RequireQualifiedAccess>]
module RelAngleDiscriminant =

    /// The Unit Of Measure for
    /// the precalculated relative angle discriminant values.
    /// This UoM helps to avoid that angle values are used directly in the line Intersection functions.
    [<Measure>]
    type relAngDiscr

    //the value from
    // let intersectLines (l:Line3D) (ll:Line3D) =
    //     //https://stackoverflow.com/a/34604574/969070 but DP and DQ are in wrong order !
    //     let ax = l.FromX - l.ToX
    //     let ay = l.FromY - l.ToY
    //     let az = l.FromZ - l.ToZ
    //     let bx = ll.FromX - ll.ToX
    //     let by = ll.FromY - ll.ToY
    //     let bz = ll.FromZ - ll.ToZ
    //     let vx = ll.FromX - l.FromX
    //     let vy = ll.FromY - l.FromY
    //     let vz = ll.FromZ - l.FromZ
    //     let a = ax*ax + ay*ay + az*az // square length
    //     let b = ax*bx + ay*by + az*bz
    //     let c = bx*bx + by*by + bz*bz // square length
    //     let d = ax*vx + ay*vy + az*vz
    //     let e = bx*vx + by*vy + bz*vz
    //     let ac = a*c
    //     let bb = b*b
    //     let discriminant = ac - bb
    //     // Getting this relation between the sum and the subtraction gives a very good estimate of the angle between the lines.
    //     let relAngleDiscriminant = discriminant/(ac+bb)     //
    //     if relAngleDiscriminant > 1.5e-6 then //not parallel //1e-5 for 0.25deg, //1.5e-6 for 0.1deg, //1.5e-4 for 1.0 deg
    //         let t = (b * e - c * d) / discriminant
    //         let u = (a * e - b * d) / discriminant
    //         Some (t, u)
    //     else
    //         None


    /// The discriminant for an angle of 0.01 degrees.
    /// This is exactly 0.00000001523087101891
    [<Literal>]
    let ``0.01`` = 0.00000001523087101891<relAngDiscr>

    /// The discriminant for an angle of 0.05 degrees.
    /// This is exactly 0.0000003807718230973
    [<Literal>]
    let ``0.05`` = 0.0000003807718230973<relAngDiscr>

    /// The discriminant for an angle of 0.1 degrees.
    /// This is exactly 0.00000152308787227638
    [<Literal>]
    let ``0.1`` = 0.00000152308787227638<relAngDiscr>

    /// The discriminant for an angle of 0.25 degrees.
    /// This is exactly 0.00000951932457379627
    [<Literal>]
    let ``0.25`` = 0.00000951932457379627<relAngDiscr>

    /// The discriminant for an angle of 0.5 degrees.
    /// This is exactly 0.0000380776607551941
    [<Literal>]
    let ``0.5`` = 0.0000380776607551941<relAngDiscr>

    /// The discriminant for an angle of 1.0 degrees.
    /// This is exactly 0.000152316441991406
    [<Literal>]
    let ``1.0`` = 0.000152316441991406<relAngDiscr>

    /// The discriminant for an angle of 3.0 degrees.
    /// This is exactly 0.00137140433203738
    [<Literal>]
    let ``3.0`` = 0.00137140433203738<relAngDiscr>

    /// The discriminant for an angle of 5.0 degrees.
    /// This is exactly 0.00381254201694106
    [<Literal>]
    let ``5.0`` = 0.00381254201694106<relAngDiscr>

    /// The discriminant for an angle of 10.0 degrees.
    /// This is exactly 0.0153076356505348
    [<Literal>]
    let ``10.0`` = 0.0153076356505348<relAngDiscr>

    /// The discriminant for an angle of 30.0 degrees.
    /// This is exactly 0.142857142857143
    [<Literal>]
    let ``30.0`` = 0.142857142857143<relAngDiscr>

    /// The discriminant for an angle of 45.0 degrees.
    /// This is exactly 0.333333333333333
    [<Literal>]
    let ``45.0`` = 0.333333333333333<relAngDiscr>

    /// The discriminant for an angle of 60.0 degrees.
    /// This is exactly 0.6
    [<Literal>]
    let ``60.0`` = 0.6<relAngDiscr>

    /// The discriminant for an angle of 87.0 degrees.
    /// This is exactly 0.994536859196742
    [<Literal>]
    let ``87.0`` = 0.994536859196742<relAngDiscr>

    /// The discriminant for an angle of 89.0 degrees.
    /// This is exactly 0.999391012508459
    [<Literal>]
    let ``89.0`` = 0.999391012508459<relAngDiscr>

    /// The discriminant for an angle of 89.75 degrees.
    /// This is exactly 0.999961923789084
    [<Literal>]
    let ``89.75`` = 0.999961923789084<relAngDiscr>

    /// The discriminant for an angle of 89.9 degrees.
    /// This is exactly 0.999993907676349
    [<Literal>]
    let ``89.9`` = 0.999993907676349<relAngDiscr>

    /// The discriminant for an angle of 89.95 degrees.
    /// This is exactly 0.999998476914448
    [<Literal>]
    let ``89.95`` = 0.999998476914448<relAngDiscr>

    /// The discriminant for an angle of 89.99 degrees.
    /// This is exactly 0.999999939076519
    [<Literal>]
    let ``89.99`` = 0.999999939076519<relAngDiscr>

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

    /// Converts Angels from Degrees to Radians.
    let inline toRadians (degrees:float<deg>)  : float<rad> = 0.0174532925199433<rad/deg> * degrees

    /// Converts Angels from Radians to Degrees.
    let inline toDegrees (radians: float<rad>) :float<deg> = 57.2957795130823<deg/rad> * radians

    *)


