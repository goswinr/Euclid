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
open Euclid.UtilEuclid
open EuclidErrors
#if !FABLE_COMPILER
open System.Text.Json.Serialization
#endif




/// A struct containing one 3D point and three 3D unit vectors, representing an immutable parametrized plane or frame
/// with unitized X, Y and Z Direction.
/// Internally it is stored as 12 floats (the Origin point coordinates and the three axis vector components),
/// just like the box type. The Origin, XAxis, YAxis and ZAxis properties reconstruct the Pnt and UnitVec on demand.
/// This struct is called 'PPlane'; the other plane 'NPlane' refers to an un-oriented plane consisting only of an origin and a normal.
/// Note: Never use the struct default constructor PPlane() as it will create an invalid zero length PPlane.
/// Use PPlane.create or PPlane.createUnchecked instead.
[<Struct; NoEquality; NoComparison>] // because it's made up from floats
[<IsReadOnly>]
[<DataContract>] // for using DataMember on fields, for Newtonsoft.Json
#if !FABLE_COMPILER
[<JsonConverter(typeof<PPlaneJsonConverter>)>]
#endif
type PPlane =


    /// The X coordinate of the Origin 3D point of this PPlane.
    [<DataMember>] val public OriginX: float

    /// The Y coordinate of the Origin 3D point of this PPlane.
    [<DataMember>] val public OriginY: float

    /// The Z coordinate of the Origin 3D point of this PPlane.
    [<DataMember>] val public OriginZ: float

    /// The X component of the local X-axis unit vector of this PPlane.
    [<DataMember>] val public XAxisX: float

    /// The Y component of the local X-axis unit vector of this PPlane.
    [<DataMember>] val public XAxisY: float

    /// The Z component of the local X-axis unit vector of this PPlane.
    [<DataMember>] val public XAxisZ: float

    /// The X component of the local Y-axis unit vector of this PPlane.
    [<DataMember>] val public YAxisX: float

    /// The Y component of the local Y-axis unit vector of this PPlane.
    [<DataMember>] val public YAxisY: float

    /// The Z component of the local Y-axis unit vector of this PPlane.
    [<DataMember>] val public YAxisZ: float

    /// The X component of the local Z-axis unit vector of this PPlane.
    [<DataMember>] val public ZAxisX: float

    /// The Y component of the local Z-axis unit vector of this PPlane.
    [<DataMember>] val public ZAxisY: float

    /// The Z component of the local Z-axis unit vector of this PPlane.
    [<DataMember>] val public ZAxisZ: float

    /// Unsafe internal constructor, doesn't check if the input is perpendicular or unitized, public only for inlining.
    /// Creates a PPlane from Origin coordinates and X-, Y-, and Z-axis unit vector components.
    [<Obsolete("Unsafe internal constructor, doesn't check if the input is perpendicular (unless compiled in DEBUG mode or with CHECKED_EUCLID), but must be public for inlining. So marked Obsolete instead.") >]
    new (originX:float, originY:float, originZ:float,
         xAxisX:float, xAxisY:float, xAxisZ:float,
         yAxisX:float, yAxisY:float, yAxisZ:float,
         zAxisX:float, zAxisY:float, zAxisZ:float) =
        #if DEBUG || CHECKED_EUCLID // CHECKED_EUCLID so checks can still be enabled when using with Fable release mode
            if isNanInfinity originX || isNanInfinity originY || isNanInfinity originZ then
                failNaN3 "PPlane() origin" originX originY originZ
            if isNotOne (xAxisX*xAxisX + xAxisY*xAxisY + xAxisZ*xAxisZ) then failNotOne3 "PPlane() Xaxis" xAxisX xAxisY xAxisZ
            if isNotOne (yAxisX*yAxisX + yAxisY*yAxisY + yAxisZ*yAxisZ) then failNotOne3 "PPlane() Yaxis" yAxisX yAxisY yAxisZ
            if isNotOne (zAxisX*zAxisX + zAxisY*zAxisY + zAxisZ*zAxisZ) then failNotOne3 "PPlane() Zaxis" zAxisX zAxisY zAxisZ
            let x = Vec(xAxisX, xAxisY, xAxisZ)
            let y = Vec(yAxisX, yAxisY, yAxisZ)
            let z = Vec(zAxisX, zAxisY, zAxisZ)
            // the axes are unitized, so the dot-product is the cosine of the angle between them:
            if isNotZero (xAxisX*yAxisX + xAxisY*yAxisY + xAxisZ*yAxisZ) then fail3 "PPlane(): X-axis and Y-axis are not perpendicular" x y z
            if isNotZero (xAxisX*zAxisX + xAxisY*zAxisY + xAxisZ*zAxisZ) then fail3 "PPlane(): X-axis and Z-axis are not perpendicular" x y z
            if isNotZero (yAxisX*zAxisX + yAxisY*zAxisY + yAxisZ*zAxisZ) then fail3 "PPlane(): Y-axis and Z-axis are not perpendicular" x y z
            // the cross product of two perpendicular unit vectors is a unit vector, so its dot-product with the Z-axis is one for a right-handed frame:
            let crossX = xAxisY*yAxisZ - xAxisZ*yAxisY
            let crossY = xAxisZ*yAxisX - xAxisX*yAxisZ
            let crossZ = xAxisX*yAxisY - xAxisY*yAxisX
            if isNotOne (crossX*zAxisX + crossY*zAxisY + crossZ*zAxisZ) then fail3 "PPlane(): X-, Y- and Z-axis do not form a right-handed frame" x y z
        #endif
            {OriginX=originX; OriginY=originY; OriginZ=originZ
             XAxisX=xAxisX; XAxisY=xAxisY; XAxisZ=xAxisZ
             YAxisX=yAxisX; YAxisY=yAxisY; YAxisZ=yAxisZ
             ZAxisX=zAxisX; ZAxisY=zAxisY; ZAxisZ=zAxisZ}


    /// Unsafe internal constructor, doesn't check if the input is perpendicular or unitized.
    /// Requires correct input of unitized perpendicular vector components.
    /// When compiled in DEBUG mode or with the CHECKED_EUCLID symbol defined the input is verified.
    static member inline createUnchecked(originX: float, originY: float, originZ: float,
                                         xAxisX: float, xAxisY: float, xAxisZ: float,
                                         yAxisX: float, yAxisY: float, yAxisZ: float,
                                         zAxisX: float, zAxisY: float, zAxisZ: float) : PPlane =
        #nowarn "44"
        PPlane(originX, originY, originZ, xAxisX, xAxisY, xAxisZ, yAxisX, yAxisY, yAxisZ, zAxisX, zAxisY, zAxisZ)
        #warnon "44" // re-enable warning for obsolete usage

    /// Unsafe internal constructor, doesn't check if the input is perpendicular.
    /// Requires correct input of unitized perpendicular vectors.
    static member inline createFromAxesUnchecked (origin: Pnt, xAxis: UnitVec, yAxis: UnitVec, zAxis: UnitVec) : PPlane =
        PPlane.createUnchecked(origin.X, origin.Y, origin.Z,
                                  xAxis.X, xAxis.Y, xAxis.Z,
                                  yAxis.X, yAxis.Y, yAxis.Z,
                                  zAxis.X, zAxis.Y, zAxis.Z)


    /// Creates a PPlane from three unit-length, mutually perpendicular, right-handed axes and an origin.
    /// Fails if the axes do not form a valid orthonormal frame.
    static member create (xAxis:UnitVec) (yAxis:UnitVec) (zAxis:UnitVec) (origin:Pnt) : PPlane =
        if isNotZero (xAxis *** yAxis) then fail $"PPlane.create failed. The X and Y axes must be mutually perpendicular."
        if isNotZero (xAxis *** zAxis) then fail $"PPlane.create failed. The X and Z axes must be mutually perpendicular."
        if isNotZero (yAxis *** zAxis) then fail $"PPlane.create failed. The Y and Z axes must be mutually perpendicular."
        if isNotOne (UnitVec.cross(xAxis, yAxis) *** zAxis) then fail $"PPlane.create failed. The axes must form a right-handed frame."
        PPlane.createUnchecked(
            origin.X, origin.Y, origin.Z,
            xAxis.X, xAxis.Y, xAxis.Z,
            yAxis.X, yAxis.Y, yAxis.Z,
            zAxis.X, zAxis.Y, zAxis.Z)


    /// Creates a 3D point from the Origin coordinates of this PPlane.
    member inline pl.Origin : Pnt =
        Pnt(pl.OriginX, pl.OriginY, pl.OriginZ)

    /// Creates the local X-axis unit vector of this PPlane.
    member inline pl.XAxis : UnitVec =
        UnitVec.createUnchecked(pl.XAxisX, pl.XAxisY, pl.XAxisZ)

    /// Creates the local Y-axis unit vector of this PPlane.
    member inline pl.YAxis : UnitVec =
        UnitVec.createUnchecked(pl.YAxisX, pl.YAxisY, pl.YAxisZ)

    /// Creates the local Z-axis unit vector of this PPlane.
    member inline pl.ZAxis : UnitVec =
        UnitVec.createUnchecked(pl.ZAxisX, pl.ZAxisY, pl.ZAxisZ)


    /// Format PPlane into string with nicely formatted floating point numbers.
    override pl.ToString() : string =
        let o = pl.Origin
        let x = pl.XAxis
        let y = pl.YAxis
        let z = pl.ZAxis
        $"Euclid.PPlane(%s{Format.nl}Origin=%s{o.AsString}%s{Format.nl}  X-axis=%s{x.AsString}%s{Format.nl}  Y-axis=%s{y.AsString}%s{Format.nl}  Z-axis=%s{z.AsString})"

    /// Format PPlane into string with nicely formatted floating point numbers.
    /// But without type name as in pl.ToString()
    member pl.AsString : string =
        let o = pl.Origin
        let x = pl.XAxis
        let y = pl.YAxis
        let z = pl.ZAxis
        $"%s{Format.nl}Origin=%s{o.AsString}%s{Format.nl}  X-axis=%s{x.AsString}%s{Format.nl}  Y-axis=%s{y.AsString}%s{Format.nl}  Z-axis=%s{z.AsString}"

    /// Format PPlane into string with nicely formatted floating point numbers.
    /// But without type name as in pl.ToString()
    static member inline asString (pl:PPlane) : string =
        pl.AsString

    /// Format PPlane into an F# code string that can be used to recreate the plane.
    member pl.AsFSharpCode : string =
        let f = Format.floatAsFSharpCode
        $"PPlane.createUnchecked({f pl.OriginX}, {f pl.OriginY}, {f pl.OriginZ}, {f pl.XAxisX}, {f pl.XAxisY}, {f pl.XAxisZ}, {f pl.YAxisX}, {f pl.YAxisY}, {f pl.YAxisZ}, {f pl.ZAxisX}, {f pl.ZAxisY}, {f pl.ZAxisZ})"

    /// Format PPlane into an F# code string that can be used to recreate the plane.
    static member inline asFSharpCode (pl:PPlane) : string =
        pl.AsFSharpCode


    // see extension members in folder 'TypeExtensions/PPlane.fs'

#if !FABLE_COMPILER
/// Serializes a PPlane as its origin and axis components with System.Text.Json.
and internal PPlaneJsonConverter() =
    inherit JsonConverter<PPlane>()

    override _.Write(writer, plane, options) =
        writer.WriteStartObject()
        JsonConvert.writeFloat writer options "OriginX" plane.OriginX
        JsonConvert.writeFloat writer options "OriginY" plane.OriginY
        JsonConvert.writeFloat writer options "OriginZ" plane.OriginZ
        JsonConvert.writeFloat writer options "XAxisX" plane.XAxisX
        JsonConvert.writeFloat writer options "XAxisY" plane.XAxisY
        JsonConvert.writeFloat writer options "XAxisZ" plane.XAxisZ
        JsonConvert.writeFloat writer options "YAxisX" plane.YAxisX
        JsonConvert.writeFloat writer options "YAxisY" plane.YAxisY
        JsonConvert.writeFloat writer options "YAxisZ" plane.YAxisZ
        JsonConvert.writeFloat writer options "ZAxisX" plane.ZAxisX
        JsonConvert.writeFloat writer options "ZAxisY" plane.ZAxisY
        JsonConvert.writeFloat writer options "ZAxisZ" plane.ZAxisZ
        writer.WriteEndObject()

    override _.Read(reader, _, options) =
        JsonConvert.readStartObject &reader "Euclid.PPlane"
        let originX = JsonConvert.readFloat &reader options "Euclid.PPlane" "OriginX"
        let originY = JsonConvert.readFloat &reader options "Euclid.PPlane" "OriginY"
        let originZ = JsonConvert.readFloat &reader options "Euclid.PPlane" "OriginZ"
        let xAxisX = JsonConvert.readFloat &reader options "Euclid.PPlane" "XAxisX"
        let xAxisY = JsonConvert.readFloat &reader options "Euclid.PPlane" "XAxisY"
        let xAxisZ = JsonConvert.readFloat &reader options "Euclid.PPlane" "XAxisZ"
        let yAxisX = JsonConvert.readFloat &reader options "Euclid.PPlane" "YAxisX"
        let yAxisY = JsonConvert.readFloat &reader options "Euclid.PPlane" "YAxisY"
        let yAxisZ = JsonConvert.readFloat &reader options "Euclid.PPlane" "YAxisZ"
        let zAxisX = JsonConvert.readFloat &reader options "Euclid.PPlane" "ZAxisX"
        let zAxisY = JsonConvert.readFloat &reader options "Euclid.PPlane" "ZAxisY"
        let zAxisZ = JsonConvert.readFloat &reader options "Euclid.PPlane" "ZAxisZ"
        JsonConvert.readEndObject &reader "Euclid.PPlane"
        let xAxis = UnitVec.createWithErrorMessage "X axis of PPlane" xAxisX xAxisY xAxisZ
        let yAxis = UnitVec.createWithErrorMessage "Y axis of PPlane" yAxisX yAxisY yAxisZ
        let zAxis = UnitVec.createWithErrorMessage "Z axis of PPlane" zAxisX zAxisY zAxisZ
        PPlane.create xAxis yAxis zAxis (Pnt(originX, originY, originZ))
#endif
