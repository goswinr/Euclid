// A workaround for Fable compiler to not fail on DataContract and DataMember attributes
// see https://github.com/fable-compiler/Fable/issues/2253
#if FABLE_COMPILER
namespace System.Runtime.Serialization
type DataContract() = inherit System.Attribute() // just shadow the real attribute
type DataMember()   = inherit System.Attribute() // just shadow the real attribute
#endif

namespace Euclid

open System

/// Runtime.InteropServices.OptionalAttribute for member parameters.
type internal OPT =
    Runtime.InteropServices.OptionalAttribute

/// Runtime.InteropServices.DefaultParameterValueAttribute for member parameters.
type internal DEF =
    Runtime.InteropServices.DefaultParameterValueAttribute


/// A module for math , logic utility functions and default tolerance values for use within Euclid.
module UtilEuclid =

    /// Adds a Unit Of Measure to a float value.
    /// This is a compile time feature of F#. It's erased at runtime.
    let inline withMeasure (x:float) : float<'measure> =
        x |> LanguagePrimitives.FloatWithMeasure

    /// Adds a Unit Of Measure to a float value.
    /// This is a compile time feature of F#. It's erased at runtime.
    let inline ( !^ ) (x:float) : float<'measure> =
        x |> LanguagePrimitives.FloatWithMeasure

    /// Test if a value is not null.
    let inline notNull x =
        match x with null -> false | _ -> true

    /// Returns true if the float is NaN or Infinity.
    let inline isNanInfinity (x:float) =
        Double.IsNaN x || Double.IsInfinity x

    /// Squares a float value.
    let inline sq (x:float) =
        x * x


    /// Tolerance for axis alignment: 1e-9
    /// Used in Axis Alignment checks.
    [<Literal>]
    let axisAlignmentTolerance =
        1e-9

    /// Tolerance for zero length: 1e-12 in divisions and unitizing of vectors.
    [<Literal>]
    let zeroLengthTolerance =
        1e-12

    /// Squared Tolerance for zero length in divisions: 1e-12 * 1e-12 = 1e-24
    [<Literal>]
    let private zeroLengthTolSquared =
        zeroLengthTolerance * zeroLengthTolerance

    /// Returns true for values smaller than 1e-6 and for NaN.
    let inline isTooSmall x =
        // use 'not' to catch a NaN too ( a Cross Product of infinite long vectors can give a NaN length)
        not ( x > 1e-6 )

    /// Returns true for values smaller than 1e-12 (square of 1e-6)  and for NaN.
    let inline isTooSmallSq x =
        // use 'not' to catch a NaN too ( a Cross Product of infinite long vectors can give a NaN length)
        not ( x > 1e-12 )

    /// Returns true for values smaller than 1e-12 and for NaN.
    /// Uses UtilEuclid.zeroLengthTolerance
    let inline isTooTiny x =
        // use 'not' to catch a NaN too ( a Cross Product of infinite long vectors can give a NaN length)
        not ( x > zeroLengthTolerance )

    /// Returns 1 for values smaller than 1e-12 and for NaN.
    /// Otherwise returns 0
    /// Uses UtilEuclid.zeroLengthTolerance
    let inline countTooTinyOrNaN x =
        // Catches a NaN too  ( a Cross Product of infinite long vectors can give a NaN length)
        if x > zeroLengthTolerance then 0 else 1

    /// Returns true for values smaller than 1e-24 (square of 1e-12) and for NaN.
    /// Uses UtilEuclid.zeroLengthTolSquared
    let inline isTooTinySq x =
        // Catches a NaN too  ( a Cross Product of infinite long vectors can give a NaN length)
        not ( x > zeroLengthTolSquared)

    /// Returns 1 for values smaller than 1e-24 (square of 1e-12) and for NaN.
    /// Otherwise returns 0
    /// Uses UtilEuclid.zeroLengthTolSquared
    let inline countTooTinySqOrNaN x =
        // Catches a NaN too ( a Cross Product of infinite long vectors can give a NaN length)
        if x > zeroLengthTolSquared then 0 else 1

    /// Returns true for negative number and for NaN.
    /// 0.0  and -0.0 is not negative.
    let inline isNegative x =
        not (x >= 0.0)

    /// Math.PI * 2.0
    /// This is equal to 360 degrees in radians.
    [<Literal>]
    let twoPi =
        6.2831853071795862 //(Math.PI*2.0).ToString("R")

    /// Math.PI * 0.5
    /// This is equal to 90 degrees in radians.
    [<Literal>]
    let halfPi =
        1.5707963267948966 // (Math.PI*0.5).ToString("R")

    /// Converts Angles from Degrees to Radians.
    /// By multiplying with 0.0174... (PI / 180.)
    let inline toRadians degrees =
        0.017453292519943295 * degrees //  (Math.PI / 180.).ToString("R")

    /// Converts Angles from Radians to Degrees.
    /// By multiplying with 57.29... (180. / PI)
    let inline toDegrees radians =
        57.295779513082323 * radians  // (180. / Math.PI).ToString("R")

    /// Sorts two values and returns them as a tuple (min, max).
    let inline sort2 (a:'T) (b:'T) =
        if a <= b then a,b else b,a

    /// Clamp value between -1.0 and +1.0.
    /// Returns -1.0 for NaN
    let inline clampBetweenMinusOneAndOne (x:float)=
        if x > -1.0 then // to handle NaN too
            if x < 1.0 then
                x
            else
                1.0
        else // x <= -1.0 or NaN
            -1.0

    /// Clamp value between 0.0 and +1.0.
    /// Returns 0.0 for NaN
    let inline clampBetweenZeroAndOne (x:float)=
        if x > 0.0 then // to handle NaN too
            if x < 1.0 then
                x
            else
                1.0
        else // x <= 0.0 or NaN
            0.0


    /// A safe arcsine (Inverse Sine) function.
    /// It clamps the input between -1 and 1
    let inline asinSafe a = // TODO fail if 'a' is bigger than  1.01 or smaller than -1.01 ??
        a |> clampBetweenMinusOneAndOne |> Math.Asin

    /// A safe arccosine (Inverse Cosine) function.
    /// It clamps the input between -1 and 1
    let inline acosSafe a = // TODO fail if 'a' is bigger than  1.01 or smaller than -1.01 ??
        a |> clampBetweenMinusOneAndOne |> Math.Acos

    /// The float literal that is 1.0 + 1e-6
    /// This is literally 1.000001
    [<Literal>]
    let ``1.0 + 1e-6`` =
        1.000001

    /// The float literal that is 1.0 - 1e-6
    /// This is literally 0.999999
    [<Literal>]
    let ``1.0 - 1e-6`` =
        0.999999

   /// The float literal that is -1.0 + 1e-6
   /// This is literally -0.999999
    [<Literal>]
    let ``-1.0 + 1e-6`` =
        -0.999999

    /// The float literal that is -1.0 - 1e-6
    /// This is literally -1.000001
    [<Literal>]
    let ``-1.0 - 1e-6`` =
        -1.000001

    /// Tests if a number is close to 1.0 by a 1e-6 tolerance.
    /// This is a float increment of 6 steps or decrement of 16 steps.
    let inline isOne  x =
        ``1.0 - 1e-6`` < x && x < ``1.0 + 1e-6``

    /// Tests if a number is NOT close to 1.0 by a 1e-6 tolerance.
    /// This is a float increment of 6 steps or decrement of 16 steps.
    /// Also returns true for NaN.
    let inline isNotOne x =
        not (isOne x)


    /// Tests if a number is close to minus 1.0 by a 1e-6 tolerance.
    /// This is a float increment of 6 steps or decrement of 16 steps.
    let inline isMinusOne  x =
        ``-1.0 - 1e-6`` < x && x < ``-1.0 + 1e-6``

    /// Tests if a number is close to 0.0 by 1e-6
    /// This is approximately the same tolerance that 6 increments of a float are away from 1.0.
    /// See Euclid.UtilEuclid.isOne function.
    let inline isZero x =
        -1e-6 < x && x < 1e-6

    /// Tests if a number is NOT close to 0.0 by 1e-6
    /// This is approximately the same tolerance that 6 increments of a float are away from 1.0.
    /// See Euclid.UtilEuclid.isOne function.
    /// Also returns true for NaN.
    let inline isNotZero x =
        not (isZero x)

    /// Check if value is between 0.0 and +1.0 inclusive.
    let inline isBetweenZeroAndOne (x:float) =
        x >= 0.0 && x <= 1.0

    /// Check if value is between 0.0 and +1.0 inclusive a tolerance of 1e-6 .
    ///  -1e-6 < x < 1.0 + 1e-6
    let inline isBetweenZeroAndOneTolerantIncl (x:float) =
        -1e-6 < x && x < ``1.0 + 1e-6``

    /// Check if value is between 0.0 and +1.0 exclusive a tolerance of 1e-6 .
    /// 1e-6 < x < 1.0 - 1e-6
    let inline isBetweenZeroAndOneTolerantExcl (x:float) =
        1e-6 < x && x < ``1.0 - 1e-6``


    /// Match the sign (+ or -) to a given number.
    let matchSign (signedValue:float) (numToMatch:float) =
        if sign signedValue = sign numToMatch then
            numToMatch
        else
            -numToMatch


    /// Any positive or negative int will give a valid index for given collection size.
    /// Converts negative indices to positive ones and loops to start after last index is reached.
    /// Returns a valid index for a collection of 'length' items for any integer.
    let inline saveIdx i length =
        let t = i % length
        if t >= 0 then t
        else           t + length



/// A module of precalculated cosine values for faster checking the angles of dot products of unit-vectors.
[<RequireQualifiedAccess>]
module Cosine =

    /// The Unit Of Measure type for the precalculated cosine values.
    /// This UoM helps to avoid that degree or radians angle values are used in the parallel or orthogonality tests.
    [<Measure>]
    type cosine

    /// Converts a cosine value to degrees.
    let inDegrees (c:float<cosine>) : float =
        acos (unbox<float>  c) |> UtilEuclid.toDegrees

    (*
    // to generate the below values use :
    let print(degree:float) =
        let radians = degree * (System.Math.PI  / 180.)
        let v = cos(radians).ToString("R") // r for round trip
        let deg = sprintf "%0.3f" degree // #0.0 ensure at least one trailing zero
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
    print 0.0
    for s in steps            do print (s)
    for s in steps|> List.rev |> Seq.skip 1 do print (90. - s)
    for s in steps            do print (90. + s)
    for s in steps|> List.rev |> Seq.skip 1 do print (180. - s)
    print 180.0
    *)

    /// More than the cosine of an angle of 0.0 degrees.
    /// This is exactly 1.1
    /// A value that can never be reached from a cosine function.
    [<Literal>]
    let ``0.0+`` = 1.1<cosine>

    /// The cosine of an angle of 0.0 degrees.
    /// This is exactly 1.0
    [<Literal>]
    let ``0.0`` = 1.0<cosine>


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


    /// The cosine of an angle of 180.00 degrees.
    /// This is exactly -1.0
    [<Literal>]
    let ``180.0`` = -1.0<cosine>

    /// Less than the cosine of an angle of 180.00 degrees.
    /// This is exactly -1.1
    /// A value that can never be reached from a cosine function.
    [<Literal>]
    let ``180.0-`` = -1.1<cosine>



/// A module of precalculated tangent values for faster checking the angles between two vectors.
/// The Cross Product length (=determinant) divided by the Dot Product value gives the tangent of the angle between two vectors.
/// If one of vectors has a zero length, the tangent is NaN
[<RequireQualifiedAccess>]
module Tangent =

    /// The Unit Of Measure type for the precalculated tangent values.
    /// This UoM helps to avoid that degree or radians angle values are used in the parallel or orthogonality tests.
    /// The tangent is calculated by (determinant / dot-product) of two vectors.
    [<Measure>]
    type tangent

    /// Converts a tangent value to degrees.
    let inDegrees (t:float<tangent>) : float =
        atan (unbox<float> t) |> UtilEuclid.toDegrees

    (*
    // to generate the below values use :
    open System
    let print(degree:float) =
        let radians = degree * (Math.PI  / 180.)
        let v = tan radians
        let deg = sprintf "%0.3f" degree // #0.0 ensure at least one trailing zero
        let deg = deg.TrimEnd '0'
        let deg = if deg[deg.Length-1]='.' then deg+"0" else deg
        printfn $"""
        /// The inverse or arc tangent (atan) for an angle of {degree} degrees.
        /// This is exactly {v},  calculated by  (determinant / dot-product)
        [<Literal>]
        let ``{deg}`` = {v}<tangent>
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
    print 0.0
    for s in steps  do print (s)
    for s in steps|> List.rev |> Seq.skip 1 do print (90. - s)
    //for s in steps            do print (90. + s)
    //for s in steps|> List.rev |> Seq.skip 1 do print (180. - s)
    //print 180.0
    *)

    // The inverse or arc tangent (atan) for an angle of 0 degrees.
    // This is exactly 0,  calculated by  determinant / dot-product
    // [<Literal>]
    // let ``0.0`` = 0<tangent> // don't allow this value, if used in intersections of lines it would return infinity parameters


    /// The inverse or arc tangent (atan) for an angle of 0.01 degrees.
    /// This is exactly 0.0001745329269716253,  calculated by  determinant / dot-product
    [<Literal>]
    let ``0.01`` = 0.0001745329269716253<tangent>


    /// The inverse or arc tangent (atan) for an angle of 0.025 degrees.
    /// This is exactly 0.00043633234068908935,  calculated by  determinant / dot-product
    [<Literal>]
    let ``0.025`` = 0.00043633234068908935<tangent>


    /// The inverse or arc tangent (atan) for an angle of 0.05 degrees.
    /// This is exactly 0.0008726648475212713,  calculated by  determinant / dot-product
    [<Literal>]
    let ``0.05`` = 0.0008726648475212713<tangent>


    /// The inverse or arc tangent (atan) for an angle of 0.1 degrees.
    /// This is exactly 0.0017453310241888004,  calculated by  determinant / dot-product
    [<Literal>]
    let ``0.1`` = 0.0017453310241888004<tangent>


    /// The inverse or arc tangent (atan) for an angle of 0.25 degrees.
    /// This is exactly 0.004363350820701567,  calculated by  determinant / dot-product
    [<Literal>]
    let ``0.25`` = 0.004363350820701567<tangent>


    /// The inverse or arc tangent (atan) for an angle of 0.5 degrees.
    /// This is exactly 0.00872686779075879,  calculated by  determinant / dot-product
    [<Literal>]
    let ``0.5`` = 0.00872686779075879<tangent>


    /// The inverse or arc tangent (atan) for an angle of 1 degrees.
    /// This is exactly 0.017455064928217585,  calculated by  determinant / dot-product
    [<Literal>]
    let ``1.0`` = 0.017455064928217585<tangent>


    /// The inverse or arc tangent (atan) for an angle of 2.5 degrees.
    /// This is exactly 0.04366094290851206,  calculated by  determinant / dot-product
    [<Literal>]
    let ``2.5`` = 0.04366094290851206<tangent>


    /// The inverse or arc tangent (atan) for an angle of 5 degrees.
    /// This is exactly 0.08748866352592401,  calculated by  determinant / dot-product
    [<Literal>]
    let ``5.0`` = 0.08748866352592401<tangent>


    /// The inverse or arc tangent (atan) for an angle of 10 degrees.
    /// This is exactly 0.17632698070846498,  calculated by  determinant / dot-product
    [<Literal>]
    let ``10.0`` = 0.17632698070846498<tangent>


    /// The inverse or arc tangent (atan) for an angle of 15 degrees.
    /// This is exactly 0.2679491924311227,  calculated by  determinant / dot-product
    [<Literal>]
    let ``15.0`` = 0.2679491924311227<tangent>


    /// The inverse or arc tangent (atan) for an angle of 20 degrees.
    /// This is exactly 0.36397023426620234,  calculated by  determinant / dot-product
    [<Literal>]
    let ``20.0`` = 0.36397023426620234<tangent>


    /// The inverse or arc tangent (atan) for an angle of 30 degrees.
    /// This is exactly 0.5773502691896257,  calculated by  determinant / dot-product
    [<Literal>]
    let ``30.0`` = 0.5773502691896257<tangent>


    /// The inverse or arc tangent (atan) for an angle of 45 degrees.
    /// This is exactly 1.0,  calculated by  determinant / dot-product
    [<Literal>]
    let ``45.0`` = 1.0<tangent>


    /// The inverse or arc tangent (atan) for an angle of 60 degrees.
    /// This is exactly 1.7320508075688767,  calculated by  determinant / dot-product
    [<Literal>]
    let ``60.0`` = 1.7320508075688767<tangent>


    /// The inverse or arc tangent (atan) for an angle of 70 degrees.
    /// This is exactly 2.7474774194546216,  calculated by  determinant / dot-product
    [<Literal>]
    let ``70.0`` = 2.7474774194546216<tangent>


    /// The inverse or arc tangent (atan) for an angle of 75 degrees.
    /// This is exactly 3.7320508075688776,  calculated by  determinant / dot-product
    [<Literal>]
    let ``75.0`` = 3.7320508075688776<tangent>


    /// The inverse or arc tangent (atan) for an angle of 80 degrees.
    /// This is exactly 5.671281819617707,  calculated by  determinant / dot-product
    [<Literal>]
    let ``80.0`` = 5.671281819617707<tangent>


    /// The inverse or arc tangent (atan) for an angle of 85 degrees.
    /// This is exactly 11.430052302761348,  calculated by  determinant / dot-product
    [<Literal>]
    let ``85.0`` = 11.430052302761348<tangent>


    /// The inverse or arc tangent (atan) for an angle of 87.5 degrees.
    /// This is exactly 22.903765548431192,  calculated by  determinant / dot-product
    [<Literal>]
    let ``87.5`` = 22.903765548431192<tangent>


    /// The inverse or arc tangent (atan) for an angle of 89 degrees.
    /// This is exactly 57.289961630759144,  calculated by  determinant / dot-product
    [<Literal>]
    let ``89.0`` = 57.289961630759144<tangent>


    /// The inverse or arc tangent (atan) for an angle of 89.5 degrees.
    /// This is exactly 114.58865012931011,  calculated by  determinant / dot-product
    [<Literal>]
    let ``89.5`` = 114.58865012931011<tangent>


    /// The inverse or arc tangent (atan) for an angle of 89.75 degrees.
    /// This is exactly 229.1816636094393,  calculated by  determinant / dot-product
    [<Literal>]
    let ``89.75`` = 229.1816636094393<tangent>


    /// The inverse or arc tangent (atan) for an angle of 89.9 degrees.
    /// This is exactly 572.9572133543032,  calculated by  determinant / dot-product
    [<Literal>]
    let ``89.9`` = 572.9572133543032<tangent>


    /// The inverse or arc tangent (atan) for an angle of 89.95 degrees.
    /// This is exactly 1145.915299373414,  calculated by  determinant / dot-product
    [<Literal>]
    let ``89.95`` = 1145.915299373414<tangent>


    /// The inverse or arc tangent (atan) for an angle of 89.975 degrees.
    /// This is exactly 2291.8310350790075,  calculated by  determinant / dot-product
    [<Literal>]
    let ``89.975`` = 2291.8310350790075<tangent>


    /// The inverse or arc tangent (atan) for an angle of 89.99 degrees.
    /// This is exactly 5729.577893128937,  calculated by  determinant / dot-product
    [<Literal>]
    let ``89.99`` = 5729.577893128937<tangent>


    /// The inverse or arc tangent (atan) for an angle of 90.01 degrees.
    /// This is exactly -5729.577893125667,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``90.01`` = -5729.577893125667<tangent>


    /// The inverse or arc tangent (atan) for an angle of 90.025 degrees.
    /// This is exactly -2291.8310350784845,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``90.025`` = -2291.8310350784845<tangent>


    /// The inverse or arc tangent (atan) for an angle of 90.05 degrees.
    /// This is exactly -1145.9152993735747,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``90.05`` = -1145.9152993735747<tangent>


    /// The inverse or arc tangent (atan) for an angle of 90.1 degrees.
    /// This is exactly -572.9572133543435,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``90.1`` = -572.9572133543435<tangent>


    /// The inverse or arc tangent (atan) for an angle of 90.25 degrees.
    /// This is exactly -229.18166360944574,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``90.25`` = -229.18166360944574<tangent>


    /// The inverse or arc tangent (atan) for an angle of 90.5 degrees.
    /// This is exactly -114.5886501293088,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``90.5`` = -114.5886501293088<tangent>


    /// The inverse or arc tangent (atan) for an angle of 91 degrees.
    /// This is exactly -57.28996163075955,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``91.0`` = -57.28996163075955<tangent>


    /// The inverse or arc tangent (atan) for an angle of 92.5 degrees.
    /// This is exactly -22.903765548431256,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``92.5`` = -22.903765548431256<tangent>


    /// The inverse or arc tangent (atan) for an angle of 95 degrees.
    /// This is exactly -11.430052302761336,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``95.0`` = -11.430052302761336<tangent>


    /// The inverse or arc tangent (atan) for an angle of 100 degrees.
    /// This is exactly -5.671281819617711,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``100.0`` = -5.671281819617711<tangent>


    /// The inverse or arc tangent (atan) for an angle of 105 degrees.
    /// This is exactly -3.7320508075688763,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``105.0`` = -3.7320508075688763<tangent>


    /// The inverse or arc tangent (atan) for an angle of 110 degrees.
    /// This is exactly -2.7474774194546225,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``110.0`` = -2.7474774194546225<tangent>


    /// The inverse or arc tangent (atan) for an angle of 120 degrees.
    /// This is exactly -1.7320508075688783,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``120.0`` = -1.7320508075688783<tangent>


    /// The inverse or arc tangent (atan) for an angle of 135 degrees.
    /// This is exactly -1.0000000000000002,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``135.0`` = -1.0000000000000002<tangent>


    /// The inverse or arc tangent (atan) for an angle of 150 degrees.
    /// This is exactly -0.5773502691896257,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``150.0`` = -0.5773502691896257<tangent>


    /// The inverse or arc tangent (atan) for an angle of 160 degrees.
    /// This is exactly -0.36397023426620256,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``160.0`` = -0.36397023426620256<tangent>


    /// The inverse or arc tangent (atan) for an angle of 165 degrees.
    /// This is exactly -0.267949192431123,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``165.0`` = -0.267949192431123<tangent>


    /// The inverse or arc tangent (atan) for an angle of 170 degrees.
    /// This is exactly -0.1763269807084649,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``170.0`` = -0.1763269807084649<tangent>


    /// The inverse or arc tangent (atan) for an angle of 175 degrees.
    /// This is exactly -0.08748866352592402,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``175.0`` = -0.08748866352592402<tangent>


    /// The inverse or arc tangent (atan) for an angle of 177.5 degrees.
    /// This is exactly -0.043660942908512135,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``177.5`` = -0.043660942908512135<tangent>


    /// The inverse or arc tangent (atan) for an angle of 179 degrees.
    /// This is exactly -0.01745506492821751,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``179.0`` = -0.01745506492821751<tangent>


    /// The inverse or arc tangent (atan) for an angle of 179.5 degrees.
    /// This is exactly -0.008726867790758814,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``179.5`` = -0.008726867790758814<tangent>


    /// The inverse or arc tangent (atan) for an angle of 179.75 degrees.
    /// This is exactly -0.004363350820701418,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``179.75`` = -0.004363350820701418<tangent>


    /// The inverse or arc tangent (atan) for an angle of 179.9 degrees.
    /// This is exactly -0.0017453310241888143,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``179.9`` = -0.0017453310241888143<tangent>


    /// The inverse or arc tangent (atan) for an angle of 179.95 degrees.
    /// This is exactly -0.0008726648475213393,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``179.95`` = -0.0008726648475213393<tangent>


    /// The inverse or arc tangent (atan) for an angle of 179.975 degrees.
    /// This is exactly -0.0004363323406891847,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``179.975`` = -0.0004363323406891847<tangent>


    /// The inverse or arc tangent (atan) for an angle of 179.99 degrees.
    /// This is exactly -0.0001745329269717369,  calculated by  (determinant / dot-product)
    [<Literal>]
    let ``179.99`` = -0.0001745329269717369<tangent>





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


