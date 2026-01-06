namespace Euclid

// Design notes:
// The struct types in this file only have the constructors, ToString override and operators defined in this file.
// For structs that need a checked and unchecked constructor (like unit-vectors) the main 'new' constructor is marked obsolete.
// A 'create' and 'createUnchecked' static member is provided instead.
// All other members are implemented as extension members. See files in folder members.
// This design, however, makes extension members inaccessible from C#. To fix this, all types and all members could be put into one file.
// The types would have to be marked as recursive. This file would be very large and probably have bad editor performance.
open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar




/// An immutable Parametrized Plane or Frame with unitized X, Y and Z Direction.
/// This struct is called 'PPlane'; the other plane 'NPlane' refers to an un-oriented plane consisting only of an Origin and a Normal.
/// Note: Never use the struct default constructor PPlane() as it will create an invalid zero length PPlane.
/// Use PPlane.create or PPlane.createUnchecked instead.
[<Struct; NoEquality; NoComparison>] // because it's made up from floats
[<IsReadOnly>]
[<DataContract>] // for using DataMember on fields
type PPlane =
    //[<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar

    /// The Origin 3D point of this PPlane.
    [<DataMember>] val Origin: Pnt

    /// The local X-axis of this PPlane.
    [<DataMember>] val Xaxis: UnitVec

    /// The local Y-axis of this PPlane.
    [<DataMember>] val Yaxis: UnitVec

    /// The local Z-axis of this PPlane.
    [<DataMember>] val Zaxis: UnitVec

    /// Unsafe internal constructor, doesn't check if the input is perpendicular, public only for inlining.
    [<Obsolete("Unsafe internal constructor, doesn't check if the input is perpendicular, but must be public for inlining. So marked Obsolete instead.") >]
    new (origin, axisX, axisY, axisZ) =
        {Origin=origin; Xaxis=axisX; Yaxis=axisY; Zaxis=axisZ}


    /// Unsafe internal constructor, doesn't check if the input is perpendicular.
    /// Requires correct input of unitized perpendicular vectors.
    static member inline createUnchecked (origin: Pnt, axisX: UnitVec, axisY: UnitVec, axisZ: UnitVec) =
        #nowarn "44"
        PPlane(origin, axisX, axisY, axisZ)
        #warnon "44" // re-enable warning for obsolete usage


    /// Format PPlane into string with nicely formatted floating point numbers.
    override pl.ToString() =
        let o = pl.Origin.AsString
        let x = pl.Xaxis.AsString
        let y = pl.Yaxis.AsString
        let z = pl.Zaxis.AsString
        $"Euclid.PPlane(%s{Format.nl}Origin=%s{o}%s{Format.nl}  X-axis=%s{x}%s{Format.nl}  Y-axis=%s{y}%s{Format.nl}  Z-axis=%s{z})"

    /// Format PPlane into string with nicely formatted floating point numbers.
    /// But without type name as in pl.ToString()
    member pl.AsString =
        let o = pl.Origin.AsString
        let x = pl.Xaxis.AsString
        let y = pl.Yaxis.AsString
        let z = pl.Zaxis.AsString
        $"%s{Format.nl}Origin=%s{o}%s{Format.nl}  X-axis=%s{x}%s{Format.nl}  Y-axis=%s{y}%s{Format.nl}  Z-axis=%s{z}"

    /// Format PPlane into an F# code string that can be used to recreate the plane.
    member pl.AsFSharpCode =
        $"PPlane.createUnchecked({pl.Origin.AsFSharpCode}, {pl.Xaxis.AsFSharpCode}, {pl.Yaxis.AsFSharpCode}, {pl.Zaxis.AsFSharpCode})"






    // see extension members in folder 'TypeExtensions/PPlane.fs'
