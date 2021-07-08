namespace FsEx.Geo
    

/// For formating floats with adaptive precision  
module Format = 
    open System

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
            let CloseToZeroPositive = "≈0.0"
    
            [<Literal>]
            let CloseToZeroNegative = "-≈0.0"
    
            [<Literal>]
            let VeryCloseToZero = "~0.0"
    
    /// Set this to change the printing of floats larger than 10'000
    let mutable thousandSeparator = '\'' // = just one quote '
    
    /// If the absolut value of a float is below this, display ±0.0
    /// default 1e-12
    /// This value can be set for exmaple by hosting apps that have a build in absolute tolerance like Rhino3d
    let mutable displayAsAlmostZeroBelow = 1e-12 // Double.Epsilon // default = Double.Epsilon = no rounding down

    let private addThousandSeparators (s:string) = 
        let b = Text.StringBuilder(s.Length + s.Length / 3 + 1)
        let inline add (c:char) = b.Append(c) |> ignore

        let inline doBeforeComma st en = 
            for i=st to en-1 do // dont go to last one becaus it shal never get a separator
                let rest = en-i
                add s.[i]
                if rest % 3 = 0 then add thousandSeparator
            add s.[en] //add last (never with sep)

        let inline doAfterComma st en = 
            add s.[st] //add fist (never with sep)
            for i=st+1 to en do // dont go to last one becaus it shal never get a separator
                let pos = i-st
                if pos % 3 = 0 then add thousandSeparator
                add s.[i]

        let start = 
            if s.[0] = '-' then  add '-'; 1 /// add minus if present and move start location
            else                          0

        match s.IndexOf('.') with
        | -1 -> doBeforeComma start (s.Length-1)
        | i ->
            if i>start then doBeforeComma start (i-1)
            add '.'
            if i < s.Length then doAfterComma (i+1) (s.Length-1)

        b.ToString()

    
    let private invC = Globalization.CultureInfo.InvariantCulture

    /// Formating with automatic precision
    /// e.g.: 0 digits behind comma if above 1000
    /// if there are more than 15 zeros behind the comma just '~0.0' will be displayed
    /// if the value is smaller than NiceStringSettings.roundToZeroBelow '0.0' will be shown.
    /// this is Double.Epsilon by default
    let float  (x:float) = 
        if   Double.IsNaN x then Literals.NaN
        elif x = Double.NegativeInfinity then Literals.NegativeInfinity
        elif x = Double.PositiveInfinity then Literals.PositiveInfinity
        elif x = -1.23432101234321e+308  then Literals.RhinoMathUnsetDouble // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetValue.htm
        elif x = 0.0 then "0.0" // not "0" as in sprintf "%g"
        else
            let  a = abs x
            if   a <  displayAsAlmostZeroBelow then Literals.VeryCloseToZero
            elif a >= 10000.     then x.ToString("#")|> addThousandSeparators
            elif a >= 1000.      then x.ToString("#")
            elif a >= 100.       then x.ToString("0.0" , invC)
            elif a >= 10.        then x.ToString("0.00" , invC)
            elif a >= 1.         then x.ToString("0.000" , invC)
            elif a >= 0.1        then x.ToString("0.0000" , invC)//|> addThousandSeparators
            elif a >= 0.01       then x.ToString("0.00000" , invC)//|> addThousandSeparators
            elif a >= 0.001      then x.ToString("0.000000" , invC)|> addThousandSeparators
            elif a >= 0.0001     then x.ToString("0.0000000" , invC)|> addThousandSeparators
            elif a >= 0.00001    then x.ToString("0.00000000" , invC)|> addThousandSeparators
            elif a >= 0.000001   then x.ToString("0.000000000" , invC)|> addThousandSeparators
            elif a >= 0.0000001  then x.ToString("0.0000000000" , invC)|> addThousandSeparators            
            elif x >= 0.0        then Literals.CloseToZeroPositive
            else                      Literals.CloseToZeroNegative

    /// Formating with automatic precision
    /// e.g.: 0 digits behind comma if above 1000
    let single (x:float32) = 
        if   Single.IsNaN x then Literals.NaN
        elif x = Single.NegativeInfinity then Literals.PositiveInfinity
        elif x = Single.PositiveInfinity then Literals.NegativeInfinity
        elif x = -1.234321e+38f          then Literals.RhinoMathUnsetSingle // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetSingle.htm
        elif x = 0.0f then "0.0" // not "0" as in sprintf "%g"
        else
            let  a = abs x
            if   a <  float32(displayAsAlmostZeroBelow) then Literals.VeryCloseToZero
            elif a >= 10000.f then x.ToString("#")|> addThousandSeparators
            elif a >= 1000.f   then x.ToString("#")
            elif a >= 100.f    then x.ToString("0.0" , invC)
            elif a >= 10.f     then x.ToString("0.00" , invC)
            elif a >= 1.f      then x.ToString("0.000" , invC)
            elif a >= 0.1f     then x.ToString("0.0000" , invC)//|> addThousandSeparators  
            elif a >= 0.01f    then x.ToString("0.00000" , invC)//|> addThousandSeparators  
            elif a >= 0.001f   then x.ToString("0.000000" , invC)|> addThousandSeparators  
            elif a >= 0.0001f  then x.ToString("0.0000000" , invC)|> addThousandSeparators  
            elif a >= 0.00001f then x.ToString("0.00000000" , invC)|> addThousandSeparators  
            elif x >= 0.0f     then Literals.CloseToZeroPositive
            else                    Literals.CloseToZeroNegative
