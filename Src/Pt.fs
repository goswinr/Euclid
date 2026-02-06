namespace Euclid

// Design notes:
// The structs types in this file only have the constructors, ToString override and operators defined in this file.
// For structs that need a checked and unchecked constructor (like unit-vectors) the main 'new' constructor is marked obsolete.
// A 'create' and 'createUnchecked' static member is provided instead.
// All other members are implemented as extension members. See files in folder members.
// This design however makes extension members inaccessible from C#. To fix this all types and all members could be put into one file.
// The types would have to be marked as recursive. This file would be very large and probably have bad editor performance.

// the attributes [<IsByRefLike; IsReadOnly>]  are for potential performance optimization.
// see https://www.bartoszsypytkowski.com/writing-high-performance-f-code/
// see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike

// The attributes [<DataContract>] is for using DataMember on fields for serialization.
// [<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>]
open Euclid.UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open EuclidErrors


/// <summary> Pt is an immutable 2D point. Made up from 2 floats: X and Y.</summary>
/// <remarks> 3D Points are called 'Pnt' </remarks>
[<Struct;NoEquality;NoComparison>]// because its made up from floats
[<IsReadOnly>] //[<IsByRefLike>]
[<DataContract>] // for using DataMember on fields
type Pt =

    /// <summary>The field holding the X part of this 2D point.</summary>
    [<DataMember>] val X : float

    /// <summary>The field holding the Y part of this 2D point.</summary>
    [<DataMember>] val Y : float

    /// <summary>Create a new 2D point from X and Y coordinates.</summary>
    /// <remarks>When compiled in DEBUG or with CHECK_EUCLID symbol defined, this constructor checks for
    /// NaN and Infinity values and raises an exception if any are found.
    /// This check is skipped in release mode for performance reasons.</remarks>
    new (x, y) =
        #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode // TODO : with this test all operations are 2.5 times slower
            if isNanInfinity x || isNanInfinity y then failNaN2 "Pt()" x y
        #endif
            {X=x; Y=y}


    /// Format 2D point into string including type name and nice floating point number formatting.
    override p.ToString() =
        let x = Format.float p.X
        let y = Format.float p.Y
        $"Euclid.Pt: X=%s{x}|Y=%s{y}"

    /// Format 2D point into string with nice floating point number formatting of X and Y
    /// But without full type name as in p.ToString()
    member p.AsString : string =
        let x = Format.float p.X
        let y = Format.float p.Y
        $"X=%s{x}|Y=%s{y}"

    /// Format 2D point into an F# code string that can be used to recreate the point.
    member p.AsFSharpCode : string =
        $"Pt({p.X}, {p.Y})"

    /// Subtract one 2D point from another.
    /// 'a-b' returns a new 2D vector from b to a.
    static member inline ( - ) (a:Pt, b:Pt) =
        Vc (a.X - b.X, a.Y - b.Y)

    /// Subtract a vector from a 2D point. Returns a new 2D point.
    static member inline ( - ) (a:Pt, b:Vc) =
        Pt (a.X - b.X, a.Y - b.Y)

    /// Subtract a unit-vector from a 2D point. Returns a new 2D point.
    static member inline ( - ) (a:Pt, b:UnitVc) =
        Pt (a.X - b.X, a.Y - b.Y)

    //static member inline ( + ) (v:UnitVc, p:Pt) = Pt (p.X + v.X, p.Y + v.Y)
    //static member inline ( + ) (v:Vc,     p:Pt) = Pt (p.X + v.X, p.Y + v.Y)

    /// Add a vector to a 2D point. Returns a new 2D point.
    static member inline ( + ) (p:Pt, v:Vc) =
        Pt (p.X + v.X, p.Y + v.Y)

    /// Add a unit-vector to a 2D point. Returns a new 2D point.
    static member inline ( + ) (p:Pt, v:UnitVc) =
        Pt (p.X + v.X, p.Y + v.Y)

    /// Add two 2D points together. Returns a new 2D point.
    static member inline ( + ) (a:Pt, b:Pt) =
        Pt (a.X + b.X, a.Y + b.Y) // required for Seq.average and Pnt.midPt

    /// Multiplies a 2D point with a scalar, also called scaling a point.
    /// Returns a new 2D point.
    static member inline ( * ) (a:Pt, f:float) =
        Pt (a.X * f, a.Y * f)

    /// Multiplies a scalar with a 2D point, also called scaling a point.
    /// Returns a new 2D point.
    static member inline ( * ) (f:float, a:Pt) =
        Pt (a.X * f, a.Y * f)

    /// Divides a 2D point by a scalar, also called dividing/scaling a point. Returns a new 2D point.
    static member inline ( / ) (p:Pt, f:float) =
        if isTooTiny (abs f) then failDivide "'/' operator" f p // don't compose error msg directly here to keep inlined code small.
        Pt (p.X / f, p.Y / f)

    //-----------------------------------------------------------------------------------------------------
    // These static members can't be extension methods to be useful for Array.sum and Array.average :
    //-----------------------------------------------------------------------------------------------------

    /// Divides the 2D point by an integer.
    /// (This member is needed by Array.average and similar functions)
    static member DivideByInt (pt:Pt, i:int) : Pt = // needed by 'Array.average'
        if i = 0 then failDivide "Pt.DivideByInt" 0.0 pt
        let d = float i
        Pt(pt.X/d, pt.Y/d)


    /// Same as Pt.Origin.
    static member inline Zero =
        Pt (0., 0. )  // needed by 'Array.sum' .

    /// Same as Pt.Zero.
    static member inline Origin =
        Pt (0., 0. )

(*
from:
https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike

netstandard2.0 does not support [<IsByRefLike>] nor  [<IsReadOnly>]

[<IsByRefLike>] is another attribute.
We are talking a lot about passing value types using memory location addresses instead of doing deep copies.
Marking struct using this attribute is basically saying "I always want to pass this value by reference".
This of course comes with severe limitations: it cannot be boxed (moved to managed heap) and
for this reason it can never be captured by closures, implement interfaces or be used as field in classes or other non-by-ref structs.

In terms of F# this basically means that this kind of structs are used mostly for code that
is executed right away within the function body, with no computation expressions or other indirections.
This usually qualifies them to hot paths in our code, where CPU intensive work is expected and allocations
are not welcome, like:

for .. in loops - in fact many moderns .NET structures have special variants of GetEnumerator
that doesn't allocate any memory and is implemented as by-ref struct.
F# also understands that pattern - in fact you can define custom
GetEnumerator(): MyEnumerator method for your collection, with MyEnumerator - which can even be a ref struct - having two methods:

Current: 'item and
MoveNext: unit -> bool, and F# will automatically understand how to use it in loops.
You can see an example implementation of it here -
https://github.com/Horusiath/fsharp.core.extensions/blob/62b102e84325e89b0a6c4065b973936c11adee55/src/FSharp.Core.Extensions/Vec.fs#L147
it's a part of implementation of persistent vector data type,
similar to FSharpX persistent vector, but it's 4.5 times faster and not allocating anything on heap when executed in loops.
*)
