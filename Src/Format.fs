namespace Euclid
open System



/// A module with functions for formatting floats with adaptive precision.
module Format =


    module internal Literals =

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


    /// If the absolut value of a float is below this, display ~0.0
    /// The default is 1e-24
    /// This value can be set for example by hosting apps that have a built-in absolute tolerance like Rhino3d.
    let mutable userZeroTolerance = 1e-24 //  default = Double.Epsilon = no rounding down

    /// The global thousand separator character is used  by
    /// 'Euclid.Format.float' and 'Euclid.Format.single' if more than 3 digits are next to each other before and after the comma.
    /// If the separator is the NUL character '\000' no separator will be added.
    let mutable globalThousandSeparator = '\'' // = just one quote '


    /// The newline character for this platform.
    /// Just short for System.Environment.NewLine
    let nl = Environment.NewLine


    /// Insert thousand separators into a string representing a float or int.
    /// Before and after the decimal point.
    /// Assumes a string that represent a float or int
    /// with '.' as decimal separator and no other input formatting.
    let addThousandSeparators (thousandSeparator:char) (number:string) =

        let b = Text.StringBuilder(number.Length + number.Length / 3 + 1)
        let inline add (c:char) = b.Append(c) |> ignore

        let inline doBeforeComma st en =
            for i=st to en-1 do // don't go to last one because it shall never get a separator
                let rest = en-i
                add number.[i]
                if rest % 3 = 0 then add thousandSeparator
            add number.[en] //add last (never with sep)

        let inline doAfterComma st en =
            add number.[st] //add first (never with sep)
            for i=st+1 to en do // don't go to last one because it shall never get a separator
                let pos = i-st
                if pos % 3 = 0 then add thousandSeparator
                add number.[i]

        let start =
            if number.[0] = '-' then  add '-'; 1 // add minus if present and move start location
            else 0

        match number.IndexOf('.') with
        | -1 ->
            match max (number.IndexOf 'e') (number.IndexOf 'E') with //, StringComparison.OrdinalIgnoreCase) // not supported by Fable compiler
            |  -1  -> doBeforeComma start (number.Length-1)
            | eIdx -> // if float is in scientific notation don't insert comas into it too:
                doBeforeComma start (eIdx-1)
                for e = eIdx to number.Length-1 do
                    add number.[e]
        | periodIdx ->
            if periodIdx>start then
                doBeforeComma start (periodIdx-1)
            add '.'
            if periodIdx < number.Length then
                match max (number.IndexOf 'e') (number.IndexOf 'E') with //, StringComparison.OrdinalIgnoreCase) with // not supported by Fable compiler
                |  -1  -> doAfterComma (periodIdx+1) (number.Length-1)
                | eIdx -> // if float is in scientific notation don't insert comas into it too:
                    doAfterComma (periodIdx+1) (eIdx-1)
                    for e = eIdx to number.Length-1 do
                        add number.[e]

        b.ToString()


    [<RequireQualifiedAccess>]
    module ToMaxDigits =

            /// Remove trailing zeros but keep last zero if dot reached
            let trimZeros  (n:string) =
                let mutable l = n.Length
                let mutable i = n.Length - 1
                while i >= 0  do
                    let c = n.[i]
                    if   c = '0' then
                        l <- l-1 // trim 0
                    elif c = '.' then
                        l <- l+1 // to keep the last 0
                        i <- -2 // to break
                    else
                        i <- -2 // to break
                    i <- i-1
                n.Substring(0,l)

        #if FABLE_COMPILER
            open Fable.Core.JsInterop // for '?' operator
            let  d0 (x:float) : string  = x?toFixed(0)
            let  d1 (x:float) : string  = x?toFixed(1)  |> trimZeros
            let  d2 (x:float) : string  = x?toFixed(2)  |> trimZeros
            let  d3 (x:float) : string  = x?toFixed(3)  |> trimZeros
            let  d4 (x:float) : string  = x?toFixed(4)  |> trimZeros
            let  d5 (x:float) : string  = x?toFixed(5)  |> trimZeros
            let  d6 (x:float) : string  = x?toFixed(6)  |> trimZeros
            let  d7 (x:float) : string  = x?toFixed(7)  |> trimZeros
            let  d8 (x:float) : string  = x?toFixed(8)  |> trimZeros
            let  d9 (x:float) : string  = x?toFixed(9)  |> trimZeros
            let d10 (x:float) : string  = x?toFixed(10) |> trimZeros

        #else
            let private invC = Globalization.CultureInfo.InvariantCulture
            let  d0 (x:float) = x.ToString("#")
            let  d1 (x:float) = x.ToString("0.0", invC)
            let  d2 (x:float) = x.ToString("0.0#", invC)
            let  d3 (x:float) = x.ToString("0.0##", invC)
            let  d4 (x:float) = x.ToString("0.####", invC)
            let  d5 (x:float) = x.ToString("0.#####", invC)
            let  d6 (x:float) = x.ToString("0.######", invC)
            let  d7 (x:float) = x.ToString("0.#######", invC)
            let  d8 (x:float) = x.ToString("0.########", invC)
            let  d9 (x:float) = x.ToString("0.#########", invC)
            let d10 (x:float) = x.ToString("0.##########", invC)
        #endif


    /// Formatting double precision floating point numbers with automatic precision.
    /// e.g.: 0 digits behind comma if above 1000
    /// If the value is smaller than 'userZeroTolerance' (1e-24)  '~0.0' will be shown.
    /// If the value is smaller than  (1e-7)  '≈+0.0' will be shown.
    /// The thousand separator character is used if more than 3 digits are next to each other before and after the comma.
    /// If the separator is the NUL character '\000' no separator will be added.
    let floatWithSeparator (thousandSeparator:char) (x:float) =
        if Double.IsNaN x then Literals.NaN
        elif x = Double.NegativeInfinity then Literals.NegativeInfinity
        elif x = Double.PositiveInfinity then Literals.PositiveInfinity
        elif x = -1.23432101234321e+308  then Literals.RhinoMathUnsetDouble // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetValue.htm
        elif x = 0.0 then "0.0" // not "0" as in sprintf "%g"
        else
            let  a = abs x
            if   a <  userZeroTolerance then Literals.BelowUserZeroTolerance // do this check up here, this user value might be very high
            elif a >= 9999.5       then ToMaxDigits.d0  x |> addThousandSeparators thousandSeparator // 9999.5 rounds up to 10'000
            elif a >= 1000.        then ToMaxDigits.d0  x
            elif a >= 100.         then ToMaxDigits.d1  x
            elif a >= 10.          then ToMaxDigits.d2  x
            elif a >= 1.           then ToMaxDigits.d3  x
            elif a >= 0.1          then ToMaxDigits.d4  x
            elif a >= 0.01         then ToMaxDigits.d5  x
            elif a >= 0.001        then ToMaxDigits.d6  x |> addThousandSeparators thousandSeparator
            elif a >= 0.000_1      then ToMaxDigits.d7  x |> addThousandSeparators thousandSeparator
            elif a >= 0.000_01     then ToMaxDigits.d8  x |> addThousandSeparators thousandSeparator
            elif a >= 0.000_001    then ToMaxDigits.d9  x |> addThousandSeparators thousandSeparator
            elif a >= 0.000_000_1  then ToMaxDigits.d10 x |> addThousandSeparators thousandSeparator
            elif x >= 0.0          then Literals.CloseToZeroPositive
            else                        Literals.CloseToZeroNegative

    /// Formatting double precision floating point numbers with automatic precision.
    /// e.g.: 0 digits behind comma if above 1000
    /// If the value is smaller than 'userZeroTolerance' (1e-24)  '~0.0' will be shown.
    /// If the value is smaller than  (1e-7)  '≈+0.0' will be shown.
    /// The global thousand separator, a tick (') is used if more than 3 digits are next to each other before and after the comma.
    /// change it at 'Euclid.Format.globalThousandSeparator'
    let float (x:float) =
        floatWithSeparator globalThousandSeparator x


    let rarr(xs:ResizeArray<'T>) =
        if xs=null then "null-ResizeArray"
        elif xs.Count=0 then "empty ResizeArray"
        elif xs.Count=1 then $"ResizeArray with one item: [%O{xs.[0]}]"
        elif xs.Count=2 then $"ResizeArray with two items: [{nl}  %O{xs.[0]}{nl}  %O{xs.[1]}{nl}  ]"
        elif xs.Count=3 then $"ResizeArray with three items: [{nl}  %O{xs.[0]}{nl}  %O{xs.[1]}{nl}  %O{xs.[2]}{nl}  ]"
        elif xs.Count=4 then $"ResizeArray with four items: [{nl}  %O{xs.[0]}{nl}  %O{xs.[1]}{nl}  %O{xs.[2]}{nl}  %O{xs.[3]}{nl}  ]"
        else
            $"ResizeArray with {xs.Count} items: [{nl}  %O{xs.[0]}{nl}  %O{xs.[1]}{nl}  %O{xs.[2]}{nl}  %O{xs.[3]}{nl}  ...{nl}  %O{xs.[xs.Count-1]}{nl}  ]"


    open System.Collections.Generic
    let iList(xs:IList<'T>) =
        if xs=null then "null-IList<'T>"
        elif xs.Count=0 then "empty IList<'T>"
        elif xs.Count=1 then $"IList<'T> with one item: [%O{xs.[0]}]"
        elif xs.Count=2 then $"IList<'T> with two items: [{nl}  %O{xs.[0]}{nl}  %O{xs.[1]}{nl}  ]"
        elif xs.Count=3 then $"IList<'T> with three items: [{nl}  %O{xs.[0]}{nl}  %O{xs.[1]}{nl}  %O{xs.[2]}{nl}  ]"
        elif xs.Count=4 then $"IList<'T> with four items: [{nl}  %O{xs.[0]}{nl}  %O{xs.[1]}{nl}  %O{xs.[2]}{nl}  %O{xs.[3]}{nl}  ]"
        else
            $"IList<'T> with {xs.Count} items: [{nl}  %O{xs.[0]}{nl}  %O{xs.[1]}{nl}  %O{xs.[2]}{nl}  %O{xs.[3]}{nl}  ...{nl}  %O{xs.[xs.Count-1]}{nl}  ]"

