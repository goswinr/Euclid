namespace Euclid

// Design notes:
// The structs types in this file only have the constructors, the ToString override and operators define in this file.
// For structs that need a checked and unchecked constructor (like unit-vectors) the main 'new' constructor is marked obsolete.
// A 'create' and 'createUnchecked' static member is provided instead.
// All other members are implemented as extension members. see files in folder 'members'.
// This design however makes extension members unaccessible from see C#. To fix this all types and all members could be put into one file.
// The types would have to be marked as recursive. This file would be very large and probably have bad editor performance.

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open Euclid.UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open EuclidErrors


/// An immutable 3D point. Made up from 3 floats: X, Y, and Z.
/// A 3D point represents a location in space, but not direction or an offset. (use Vec for that.)
/// (2D Points are called 'Pt' )
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
[<DataContract>] // for using DataMember on fields
type Pnt =

    /// Gets the X part of this 3D point.
    [<DataMember>] val X : float

    /// Gets the Y part of this 3D point.
    [<DataMember>] val Y : float

    /// Gets the Z part of this 3D point.
    [<DataMember>] val Z : float

    /// Create a new 3D point. Made up from 3 floats: X, Y, and Z.
    new (x, y, z) =
        #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode // with this test all Pnt operations are 2.5 times slower:
            if isNanInfinity x || isNanInfinity y || isNanInfinity z then failNaN3 "Pnt()" x y z
        #endif
            {X=x; Y=y; Z=z}

    /// Format 3D point into string including type name and nice floating point number formatting.
    override p.ToString() =
        let x = Format.float p.X
        let y = Format.float p.Y
        let z = Format.float p.Z
        $"Euclid.Pnt: X=%s{x}|Y=%s{y}|Z=%s{z}"


    /// Format 3D point into string with nice floating point number formatting of X, Y and Z
    /// But without full type name as in pt.ToString()
    member p.AsString =
        let x = Format.float p.X
        let y = Format.float p.Y
        let z = Format.float p.Z
        $"X=%s{x}|Y=%s{y}|Z=%s{z}"

    /// Format 3D point into an F# code string that can be used to recreate the point.
    member p.AsFSharpCode =
        $"Pnt({p.X}, {p.Y}, {p.Z})"

    /// Subtract one 3D point from another.
    /// 'a-b' returns a new 3D vector from b to a.
    static member inline ( - ) (a:Pnt, b:Pnt) =
        Vec (a.X - b.X, a.Y - b.Y, a.Z - b.Z)

    /// Subtract a unit-vector from a 3D point. Returns a new 3D point.
    static member inline ( - ) (p:Pnt, v:UnitVec) =
        Pnt (p.X - v.X, p.Y - v.Y, p.Z - v.Z)

    /// Subtract a vector from a 3D point. Returns a new 3D point.
    static member inline ( - ) (p:Pnt, v:Vec) =
        Pnt (p.X - v.X, p.Y - v.Y, p.Z - v.Z)

    /// Add two 3D points together. Returns a new 3D point.
    static member inline ( + ) (a:Pnt, b:Pnt) =
        Pnt (a.X + b.X, a.Y + b.Y, a.Z + b.Z) // required for Seq.average and Pnt.midPt

    /// Add a vector to a 3D point. Returns a new 3D point.
    static member inline ( + ) (p:Pnt, v:Vec) =
        Pnt (p.X + v.X, p.Y + v.Y, p.Z + v.Z)

    /// Add a unit-vector to a 3D point. Returns a new 3D point.
    static member inline ( + ) (p:Pnt, v:UnitVec) =
        Pnt (p.X + v.X, p.Y + v.Y, p.Z + v.Z)

    /// Multiplies a 3D point with a scalar, also called scaling a point. Returns a new 3D point.
    static member inline ( * ) (a:Pnt, f:float) =
        Pnt (a.X * f, a.Y * f, a.Z * f)

    /// Multiplies a scalar with a 3D point, also called scaling a point. Returns a new 3D point.
    static member inline ( * ) (f:float, a:Pnt) =
        Pnt (a.X * f, a.Y * f, a.Z * f)

    /// Divides a 3D point by a scalar. Returns a new 3D point.
    static member inline ( / ) (p:Pnt, f:float) =
        if abs f < UtilEuclid.zeroLengthTolerance then failDivide "'/' operator" f p // don't compose error msg directly here to keep inlined code small.
        //p * (1./f) // maybe faster but worse precision
        Pnt (p.X / f, p.Y / f, p.Z / f)

    //-----------------------------------------------------------------------------------------------------
    // These static members can't be extension methods to be useful for Array.sum and Array.average :
    //-----------------------------------------------------------------------------------------------------

    /// Same as Pnt.Origin.
    static member inline Zero =
        Pnt (0., 0., 0.)  // needed by 'Array.sum'

    /// Same as Pnt.Zero.
    static member inline Origin =
        Pnt (0. , 0., 0.)

    /// Divides the 3D point by an integer.
    /// (This member is needed by Array.average and similar functions)
    static member DivideByInt (pt:Pnt, i:int) = // needed by  'Array.average'
        if i=0 then failDivide "Pnt.DivideByInt" 0.0 pt
        let d = float i
        Pnt(pt.X/d, pt.Y/d, pt.Z/d)


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
