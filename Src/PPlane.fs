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




/// A struct containing one 3D point and three 3D unit vectors, representing an immutable parametrized plane or frame
/// with unitized X, Y and Z Direction.
/// Internally it is stored as 12 floats (the Origin point coordinates and the three axis vector components),
/// just like the Box type. The Origin, Xaxis, Yaxis and Zaxis properties reconstruct the Pnt and UnitVec on demand.
/// This struct is called 'PPlane'; the other plane 'NPlane' refers to an un-oriented plane consisting only of an origin and a normal.
/// Note: Never use the struct default constructor PPlane() as it will create an invalid zero length PPlane.
/// Use PPlane.create or PPlane.createUnchecked instead.
[<Struct; NoEquality; NoComparison>] // because it's made up from floats
[<IsReadOnly>]
[<DataContract>] // for using DataMember on fields
type PPlane =
    //[<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar

    /// The X coordinate of the Origin 3D point of this PPlane.
    [<DataMember>] val public OriginX: float

    /// The Y coordinate of the Origin 3D point of this PPlane.
    [<DataMember>] val public OriginY: float

    /// The Z coordinate of the Origin 3D point of this PPlane.
    [<DataMember>] val public OriginZ: float

    /// The X component of the local X-axis unit vector of this PPlane.
    [<DataMember>] val public XaxisX: float

    /// The Y component of the local X-axis unit vector of this PPlane.
    [<DataMember>] val public XaxisY: float

    /// The Z component of the local X-axis unit vector of this PPlane.
    [<DataMember>] val public XaxisZ: float

    /// The X component of the local Y-axis unit vector of this PPlane.
    [<DataMember>] val public YaxisX: float

    /// The Y component of the local Y-axis unit vector of this PPlane.
    [<DataMember>] val public YaxisY: float

    /// The Z component of the local Y-axis unit vector of this PPlane.
    [<DataMember>] val public YaxisZ: float

    /// The X component of the local Z-axis unit vector of this PPlane.
    [<DataMember>] val public ZaxisX: float

    /// The Y component of the local Z-axis unit vector of this PPlane.
    [<DataMember>] val public ZaxisY: float

    /// The Z component of the local Z-axis unit vector of this PPlane.
    [<DataMember>] val public ZaxisZ: float

    /// Unsafe internal constructor, doesn't check if the input is perpendicular or unitized, public only for inlining.
    /// Creates a PPlane from Origin coordinates and X, Y and Z axis unit vector components.
    [<Obsolete("Unsafe internal constructor, doesn't check if the input is perpendicular, but must be public for inlining. So marked Obsolete instead.") >]
    new (originX:float, originY:float, originZ:float,
         xAxisX:float, xAxisY:float, xAxisZ:float,
         yAxisX:float, yAxisY:float, yAxisZ:float,
         zAxisX:float, zAxisY:float, zAxisZ:float) =
           {OriginX=originX; OriginY=originY; OriginZ=originZ
            XaxisX=xAxisX; XaxisY=xAxisY; XaxisZ=xAxisZ
            YaxisX=yAxisX; YaxisY=yAxisY; YaxisZ=yAxisZ
            ZaxisX=zAxisX; ZaxisY=zAxisY; ZaxisZ=zAxisZ}


    /// Unsafe internal constructor, doesn't check if the input is perpendicular or unitized.
    /// Requires correct input of unitized perpendicular vector components.
    static member inline createUnchecked(originX: float, originY: float, originZ: float,
                                            xAxisX: float, xAxisY: float, xAxisZ: float,
                                            yAxisX: float, yAxisY: float, yAxisZ: float,
                                            zAxisX: float, zAxisY: float, zAxisZ: float) : PPlane =
        #nowarn "44"
        PPlane(originX, originY, originZ, xAxisX, xAxisY, xAxisZ, yAxisX, yAxisY, yAxisZ, zAxisX, zAxisY, zAxisZ)
        #warnon "44" // re-enable warning for obsolete usage

    /// Unsafe internal constructor, doesn't check if the input is perpendicular.
    /// Requires correct input of unitized perpendicular vectors.
    static member inline createUncheckedVec (origin: Pnt, axisX: UnitVec, axisY: UnitVec, axisZ: UnitVec) : PPlane =
        PPlane.createUnchecked(origin.X, origin.Y, origin.Z,
                                  axisX.X, axisX.Y, axisX.Z,
                                  axisY.X, axisY.Y, axisY.Z,
                                  axisZ.X, axisZ.Y, axisZ.Z)


    /// Creates a 3D point from the Origin coordinates of this PPlane.
    member inline pl.Origin : Pnt =
        Pnt(pl.OriginX, pl.OriginY, pl.OriginZ)

    /// Creates the local X-axis unit vector of this PPlane.
    member inline pl.Xaxis : UnitVec =
        UnitVec.createUnchecked(pl.XaxisX, pl.XaxisY, pl.XaxisZ)

    /// Creates the local Y-axis unit vector of this PPlane.
    member inline pl.Yaxis : UnitVec =
        UnitVec.createUnchecked(pl.YaxisX, pl.YaxisY, pl.YaxisZ)

    /// Creates the local Z-axis unit vector of this PPlane.
    member inline pl.Zaxis : UnitVec =
        UnitVec.createUnchecked(pl.ZaxisX, pl.ZaxisY, pl.ZaxisZ)


    /// Format PPlane into string with nicely formatted floating point numbers.
    override pl.ToString() : string =
        let o = pl.Origin
        let x = pl.Xaxis
        let y = pl.Yaxis
        let z = pl.Zaxis
        $"Euclid.PPlane(%s{Format.nl}Origin=%s{o.AsString}%s{Format.nl}  X-axis=%s{x.AsString}%s{Format.nl}  Y-axis=%s{y.AsString}%s{Format.nl}  Z-axis=%s{z.AsString})"

    /// Format PPlane into string with nicely formatted floating point numbers.
    /// But without type name as in pl.ToString()
    member pl.AsString : string =
        let o = pl.Origin
        let x = pl.Xaxis
        let y = pl.Yaxis
        let z = pl.Zaxis
        $"%s{Format.nl}Origin=%s{o.AsString}%s{Format.nl}  X-axis=%s{x.AsString}%s{Format.nl}  Y-axis=%s{y.AsString}%s{Format.nl}  Z-axis=%s{z.AsString}"

    /// Format PPlane into string with nicely formatted floating point numbers.
    /// But without type name as in pl.ToString()
    static member inline asString (pl:PPlane) : string =
        pl.AsString

    /// Format PPlane into an F# code string that can be used to recreate the plane.
    member pl.AsFSharpCode : string =
        $"PPlane.createUnchecked({pl.OriginX}, {pl.OriginY}, {pl.OriginZ}, {pl.XaxisX}, {pl.XaxisY}, {pl.XaxisZ}, {pl.YaxisX}, {pl.YaxisY}, {pl.YaxisZ}, {pl.ZaxisX}, {pl.ZaxisY}, {pl.ZaxisZ})"






    // see extension members in folder 'TypeExtensions/PPlane.fs'

