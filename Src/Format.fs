namespace Euclid


/// For formatting floats with adaptive precision.
module Format =
    open System

    /// If the absolut value of a float is below this, display ~0.0
    /// The default is 1e-24
    /// This value can be set for example by hosting apps that have a build in absolute tolerance like Rhino3d.
    let mutable userZeroTolerance = 1e-24 //  default = Double.Epsilon = no rounding down

    module private Literals =

        /// string for RhinoMath.UnsetDouble -1.23432101234321e+308
        [<Literal>]
        let RhinoMathUnsetDouble = "RhinoMath.UnsetDouble" // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetValue.htm

        /// string for RhinoMath.UnsetSingle -1.234321e+38f
        [<Literal>]
        let RhinoMathUnsetSingle = "RhinoMath.UnsetSingle" // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetSingle.htm

        [<Literal>]
        let PositiveInfinity = "∞"

        [<Literal>]
        let NegativeInfinity = "-∞"

        [<Literal>]
        let NaN = "NaN"

        [<Literal>]
        let CloseToZeroPositive = "≈+0.0"

        [<Literal>]
        let CloseToZeroNegative = "≈-0.0"

        [<Literal>]
        let BelowUserZeroTolerance = "~0.0"

    /// Set this to change the printing of floats larger than 10'000
    let mutable thousandSeparator = '\'' // = just one quote '

    /// Assumes a string that represent a float or int with '.' as decimal separator and no other input formatting.
    let internal addThousandSeparators (s:string) =
        let b = Text.StringBuilder(s.Length + s.Length / 3 + 1)
        let inline add (c:char) = b.Append(c) |> ignore

        let inline doBeforeComma st en =
            for i=st to en-1 do // don't go to last one because it shall never get a separator
                let rest = en-i
                add s.[i]
                if rest % 3 = 0 then add thousandSeparator
            add s.[en] //add last (never with sep)

        let inline doAfterComma st en =
            add s.[st] //add fist (never with sep)
            for i=st+1 to en do // don't go to last one because it shall never get a separator
                let pos = i-st
                if pos % 3 = 0 then add thousandSeparator
                add s.[i]

        let start =
            if s.[0] = '-' then  add '-'; 1 // add minus if present and move start location
            else                          0

        match s.IndexOf('.') with
        | -1 ->
            match s.IndexOf("e") with //, StringComparison.OrdinalIgnoreCase) // not supported by Fable complier
            | -1 -> doBeforeComma start (s.Length-1)
            | e -> // if float is in scientific notation don't insert comas into it too:
                doBeforeComma start (s.Length-1)
                for ei = e to s.Length-1 do add s.[ei]
        | i ->
            if i>start then
                doBeforeComma start (i-1)
            add '.'
            if i < s.Length then
                match s.IndexOf("e") with //, StringComparison.OrdinalIgnoreCase) with // not supported by Fable complier
                | -1 -> doAfterComma (i+1) (s.Length-1)
                | e -> // if float is in scientific notation don't insert comas into it too:
                    doAfterComma (i+1) (e-1)
                    for ei = e to s.Length-1 do add s.[ei]

        b.ToString()

    let private invC = Globalization.CultureInfo.InvariantCulture

    let trim (n:string) =
        n.TrimEnd( [|'0'|] ).TrimEnd( [|'.'|] ) // remove trailing zeros and dot if no decimal digits left

    let trimThous (n:string) =
        n.TrimEnd( [|'0'; thousandSeparator |] ).TrimEnd( [|'.'|] ) // remove trailing zeros and dot if no decimal digits left

    /// Formatting with automatic precision.
    /// e.g.: 0 digits behind comma if above 1000
    /// if the value is smaller than 'userZeroTolerance' (1e-24)  '~0.0' will be shown.
    /// if the value is smaller than  (1e-7)  '≈+0.0' will be shown.
    let float (x:float) =
        if   Double.IsNaN x then Literals.NaN
        elif x = Double.NegativeInfinity then Literals.NegativeInfinity
        elif x = Double.PositiveInfinity then Literals.PositiveInfinity
        elif x = -1.23432101234321e+308  then Literals.RhinoMathUnsetDouble // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetValue.htm
        elif x = 0.0 then "0.0" // not "0" as in sprintf "%g"
        else
            let  a = abs x
            if   a <  userZeroTolerance then Literals.BelowUserZeroTolerance // do this check up here, this user value might be very high
            // elif a >= 10000.       then x.ToString("#")|> addThousandSeparators
            // elif a >= 1000.        then x.ToString("#")
            // elif a >= 100.         then x.ToString("0.#", invC)
            // elif a >= 10.          then x.ToString("0.0#", invC)
            // elif a >= 1.           then x.ToString("0.0##", invC)
            // elif a >= 0.1          then x.ToString("0.####", invC)
            // elif a >= 0.01         then x.ToString("0.#####", invC)
            // elif a >= 0.001        then x.ToString("0.######", invC)|> addThousandSeparators
            // elif a >= 0.000_1      then x.ToString("0.#######", invC)|> addThousandSeparators
            // elif a >= 0.000_01     then x.ToString("0.########", invC)|> addThousandSeparators
            // elif a >= 0.000_001    then x.ToString("0.#########", invC)|> addThousandSeparators
            // elif a >= 0.000_000_1  then x.ToString("0.##########", invC)|> addThousandSeparators
            //works better in Fable:  (but may have trailing zeros)
            elif a >= 10000.       then sprintf "%.0f"  x |> addThousandSeparators
            elif a >= 1000.        then sprintf "%.0f"  x
            elif a >= 100.         then sprintf "%.1f"  x |> trim
            elif a >= 10.          then sprintf "%.2f"  x |> trim
            elif a >= 1.           then sprintf "%.3f"  x |> trim
            elif a >= 0.1          then sprintf "%.4f"  x |> trim
            elif a >= 0.01         then sprintf "%.5f"  x |> trim
            elif a >= 0.001        then sprintf "%.6f"  x |> addThousandSeparators |> trimThous
            elif a >= 0.000_1      then sprintf "%.7f"  x |> addThousandSeparators |> trimThous
            elif a >= 0.000_01     then sprintf "%.8f"  x |> addThousandSeparators |> trimThous
            elif a >= 0.000_001    then sprintf "%.9f"  x |> addThousandSeparators |> trimThous
            elif a >= 0.000_000_1  then sprintf "%.10f" x |> addThousandSeparators |> trimThous

            elif x >= 0.0          then Literals.CloseToZeroPositive
            else                        Literals.CloseToZeroNegative

    /// Formatting with automatic precision.
    /// e.g.: 0 digits behind comma if above 1000
    /// if the value is smaller than veryCloseToZero (1e-24)  '~0.0' will be shown.
    /// if the value is smaller than  (1e-6)  '≈+0.0' will be shown.
    let single (x:float32) =
        if   Single.IsNaN x then Literals.NaN
        elif x = Single.NegativeInfinity then Literals.PositiveInfinity
        elif x = Single.PositiveInfinity then Literals.NegativeInfinity
        elif x = -1.234321e+38f          then Literals.RhinoMathUnsetSingle // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetSingle.htm
        elif x = 0.0f then "0.0" // not "0" as in sprintf "%g"
        else
            let  a = abs x
            if   a <  float32 userZeroTolerance then Literals.BelowUserZeroTolerance // do this check up here, this user value might be very high
            // elif a >= 10000.f   then x.ToString("#")|> addThousandSeparators
            // elif a >= 1000.f    then x.ToString("#")
            // elif a >= 100.f     then x.ToString("0.#", invC)
            // elif a >= 10.f      then x.ToString("0.0#", invC)
            // elif a >= 1.f       then x.ToString("0.0##", invC)
            // elif a >= 0.1f      then x.ToString("0.####", invC)
            // elif a >= 0.01f     then x.ToString("0.#####", invC)
            // elif a >= 0.001f    then x.ToString("0.######", invC)|> addThousandSeparators
            // elif a >= 0.0001f   then x.ToString("0.#######", invC)|> addThousandSeparators
            // elif a >= 0.00001f  then x.ToString("0.########", invC)|> addThousandSeparators
            // elif a >= 0.000001f then x.ToString("0.#########", invC)|> addThousandSeparators
            //works better in Fable: (but may have trailing zeros)
            elif a >= 10000.f       then sprintf "%.0f"  x |> addThousandSeparators
            elif a >= 1000.f        then sprintf "%.0f"  x
            elif a >= 100.f         then sprintf "%.1f"  x |> trim
            elif a >= 10.f          then sprintf "%.2f"  x |> trim
            elif a >= 1.f           then sprintf "%.3f"  x |> trim
            elif a >= 0.1f          then sprintf "%.4f"  x |> trim
            elif a >= 0.01f         then sprintf "%.5f"  x |> trim
            elif a >= 0.001f        then sprintf "%.6f"  x |> addThousandSeparators |> trimThous
            elif a >= 0.000_1f      then sprintf "%.7f"  x |> addThousandSeparators |> trimThous
            elif a >= 0.000_01f     then sprintf "%.8f"  x |> addThousandSeparators |> trimThous
            elif a >= 0.000_001f    then sprintf "%.9f"  x |> addThousandSeparators |> trimThous
            elif a >= 0.000_000_1f  then sprintf "%.10f" x |> addThousandSeparators |> trimThous
            elif x >= 0.0f      then Literals.CloseToZeroPositive
            else                     Literals.CloseToZeroNegative
