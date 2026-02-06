namespace Euclid

open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>]
open System.Runtime.Serialization // for serialization of struct fields only, but not properties, via [<DataMember>] attribute with Newtonsoft.Json or similar
open System

/// A struct containing 6 floats, representing an immutable finite line in 3D.
[<Struct;NoEquality;NoComparison>] // because it's made up from floats
[<IsReadOnly>]
[<DataContract>] // for using DataMember on fields
type Line3D =

    //[<DataMember>] // to serialize this struct field (but not properties) with Newtonsoft.Json and similar

    /// The field holding the X coordinate of the start point of the line.
    [<DataMember>] val FromX:float

    /// The field holding the Y coordinate of the start point of the line.
    [<DataMember>] val FromY:float

    /// The field holding the Z coordinate of the start point of the line.
    [<DataMember>] val FromZ:float

    /// The field holding the X coordinate of the end point of the line.
    [<DataMember>] val ToX  :float

    /// The field holding the Y coordinate of the end point of the line.
    [<DataMember>] val ToY  :float

    /// The field holding the Z coordinate of the end point of the line.
    [<DataMember>] val ToZ  :float

    /// Create Line3D from 3D start point and 3D end point.
    new (fromPt:Pnt, toPt:Pnt) =
        {FromX=fromPt.X; FromY=fromPt.Y; FromZ=fromPt.Z; ToX=toPt.X; ToY=toPt.Y; ToZ=toPt.Z}

    /// Create Line3D from 3D start point's x, y, and z and 3D end point's x, y, and z.
    new (fromX, fromY, fromZ, toX, toY, toZ) =
        {FromX=fromX; FromY=fromY; FromZ=fromZ; ToX=toX; ToY=toY; ToZ=toZ}

    /// The X component of the line Direction/Vector.
    member inline ln.VectorX : float =
        ln.ToX - ln.FromX

    /// The Y component of the line Direction/Vector.
    member inline ln.VectorY : float =
        ln.ToY - ln.FromY

    /// The Z component of the line Direction/Vector.
    member inline ln.VectorZ : float =
        ln.ToZ - ln.FromZ

    /// Returns the length of the line.
    member inline ln.Length : float =
        let x = ln.VectorX
        let y = ln.VectorY
        let z = ln.VectorZ
        sqrt(x*x + y*y + z*z)

    /// Returns the squared length of the line.
    member inline ln.LengthSq : float =
        let x = ln.VectorX
        let y = ln.VectorY
        let z = ln.VectorZ
        x*x + y*y + z*z

    /// Format 3D line into string including type name, X, Y and Z for start and end points, and Length.
    /// Using nice floating point number formatting.
    override ln.ToString() =
        let fromX = Format.float ln.FromX
        let fromY = Format.float ln.FromY
        let fromZ = Format.float ln.FromZ
        let toX = Format.float ln.ToX
        let toY = Format.float ln.ToY
        let toZ = Format.float ln.ToZ
        let length = Format.float ln.Length
        $"Euclid.Line3D from X=%s{fromX}|Y=%s{fromY}|Z=%s{fromZ} to X=%s{toX}|Y=%s{toY}|Z=%s{toZ} Length %s{length}"

    /// Format 3D line into string from X, Y and Z for start and end points.
    /// Using nice floating point number formatting.
    /// But without full type name as in ln.ToString()
    member ln.AsString : string =
        let fromX = Format.float ln.FromX
        let fromY = Format.float ln.FromY
        let fromZ = Format.float ln.FromZ
        let toX = Format.float ln.ToX
        let toY = Format.float ln.ToY
        let toZ = Format.float ln.ToZ
        $"%s{fromX}, %s{fromY}, %s{fromZ} to %s{toX}, %s{toY}, %s{toZ}"

    /// Format 3D line into an F# code string that can be used to recreate the line.
    member ln.AsFSharpCode : string =
        $"Line3D({ln.FromX}, {ln.FromY}, {ln.FromZ}, {ln.ToX}, {ln.ToY}, {ln.ToZ})"


    /// The start point of the Line3D.
    member inline ln.From : Pnt =
        Pnt(ln.FromX, ln.FromY, ln.FromZ)

    /// The end point of the Line3D.
    member inline ln.To : Pnt =
        Pnt(ln.ToX, ln.ToY, ln.ToZ)


    /// Same as ln.Vector or ln.Tangent.
    /// The returned vector has the same length as the Line3D.
    member inline ln.Direction : Vec =
        Vec(ln.VectorX, ln.VectorY, ln.VectorZ)

    /// Same as ln.Tangent or ln.Direction.
    /// The returned vector has the same length as the Line3D.
    member inline ln.Vector : Vec =
        Vec(ln.VectorX, ln.VectorY, ln.VectorZ)


    /// Same as ln.Vector or ln.Direction.
    /// The returned vector has the same length as the Line3D.
    member inline ln.Tangent : Vec =
        Vec(ln.VectorX, ln.VectorY, ln.VectorZ)

    /// Returns a unit-vector of the line Direction.
    member inline ln.UnitTangent : UnitVec =
        let x = ln.VectorX
        let y = ln.VectorY
        let z = ln.VectorZ
        let l = sqrt(x * x  + y * y + z * z)
        if UtilEuclid.isTooTiny l then
            EuclidErrors.failUnit3 "Line3D.UnitTangent" x y z
        let s = 1.0 / l
        UnitVec.createUnchecked (x*s, y*s, z*s)


    /// Multiplies (or applies) a Quaternion to a 3D line .
    /// The resulting line has the same length as the input.
    member inline l.Rotate (q:Quaternion) =
        // adapted from https://github.com/mrdoob/three.js/blob/dev/src/math/Vector3.js
        let u = l.FromX
        let v = l.FromY
        let w = l.FromZ
        let x = l.ToX
        let y = l.ToY
        let z = l.ToZ
        let qx = q.X
        let qy = q.Y
        let qz = q.Z
        let qw = q.W
        let tu = 2.0 * ( qy * w - qz * v)
        let tv = 2.0 * ( qz * u - qx * w)
        let tw = 2.0 * ( qx * v - qy * u)
        let tx = 2.0 * ( qy * z - qz * y)
        let ty = 2.0 * ( qz * x - qx * z)
        let tz = 2.0 * ( qx * y - qy * x)
        Line3D( u + qw * tu + qy * tw - qz * tv ,
                v + qw * tv + qz * tu - qx * tw ,
                w + qw * tw + qx * tv - qy * tu ,
                x + qw * tx + qy * tz - qz * ty ,
                y + qw * ty + qz * tx - qx * tz ,
                z + qw * tz + qx * ty - qy * tx )

    /// Multiplies (or applies) a Quaternion to a 3D line around a given center point.
    /// The resulting line has the same length as the input.
    member inline l.RotateWithCenter (cen:Pnt, q:Quaternion) =
        let cX = cen.X
        let cY = cen.Y
        let cZ = cen.Z
        let u = l.FromX - cX
        let v = l.FromY - cY
        let w = l.FromZ - cZ
        let x = l.ToX   - cX
        let y = l.ToY   - cY
        let z = l.ToZ   - cZ
        let qx = q.X
        let qy = q.Y
        let qz = q.Z
        let qw = q.W
        let tu = 2.0 * ( qy * w - qz * v)
        let tv = 2.0 * ( qz * u - qx * w)
        let tw = 2.0 * ( qx * v - qy * u)
        let tx = 2.0 * ( qy * z - qz * y)
        let ty = 2.0 * ( qz * x - qx * z)
        let tz = 2.0 * ( qx * y - qy * x)
        Line3D( u + qw * tu + qy * tw - qz * tv + cX ,
                v + qw * tv + qz * tu - qx * tw + cY,
                w + qw * tw + qx * tv - qy * tu + cZ ,
                x + qw * tx + qy * tz - qz * ty + cX ,
                y + qw * ty + qz * tx - qx * tz + cY ,
                z + qw * tz + qx * ty - qy * tx + cZ )


    /// Operator to Rotate by Quaternion around Origin.
    /// Multiplies (or applies) a Quaternion to a 3D line.
    [<Obsolete("Use ln.Rotate(q) or Line3D.rotate instead. Obsolete since 0.20.0")>]
    static member inline ( *** ) (ln:Line3D, q:Quaternion) =  ln.Rotate(q)