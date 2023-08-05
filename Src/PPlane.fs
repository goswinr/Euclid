namespace Euclid

// Design notes:
// The structs types in this file only have the constructors, ToString override and operators define in this file.
// For structs that need a checked and unchecked constructor ( like unit vectors) the main 'new' constructor is marked obsolete.
// A 'create' and 'createUnchecked' static member is provided instead.
// All other members are implemented as extension members. see files in folder members.
// This design however makes extension members unaccessible from see C#. To fix this all types and all members could be put into one file.
// the types would have to be marked as recursive. This file would be very large and probably have bad editor performance.
open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar


#nowarn "44" // for internal inline constructors

/// An immutable Parametrized Plane or Frame with X, Y and Z Direction.
/// This struct is called 'PPlane' because 'PPlane' refers to an un-oriented plane consisting only of a Origin and a Z-axis.
/// Note: Never use the struct default constructor PPlane() as it will create an invalid zero length PPlane.
/// Use PPlane.create or PPlane.createUnchecked instead.
[<Struct; NoEquality; NoComparison>] // because its made up from floats
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

    /// Unsafe internal constructor, doesn't check the input is perpendicular,  public only for inlining.
    [<Obsolete("Unsafe internal constructor, doesn't check the input is perpendicular, but must be public for inlining. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >]
    new (origin, axisX, axisY, axisZ) =
        {Origin=origin; Xaxis=axisX; Yaxis=axisY; Zaxis=axisZ}

    /// Format PPlane into string with nicely formatted floating point numbers.
    override pl.ToString() =
        sprintf "Euclid.PPlane(Origin:%s\r\nX-axis:%s\r\n Y-axis=%s\r\n Z-axis:%s)" pl.Origin.AsString pl.Xaxis.AsString pl.Yaxis.AsString pl.Zaxis.AsString

    /// For use as a faster internal constructor.
    /// Requires correct input of unitized perpendicular vectors.
    static member inline createUnchecked (origin: Pnt, axisX: UnitVec, axisY: UnitVec, axisZ: UnitVec) =
        new PPlane(origin, axisX, axisY, axisZ)


    
   
    // see extension members in folder 'members'
