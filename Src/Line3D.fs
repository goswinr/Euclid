namespace Euclid

open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>]
open System.Runtime.Serialization // for serialization of struct fields only, but not properties, via [<DataMember>] attribute with Newtonsoft.Json or similar


/// An immutable finite line in 3D. Represented by a 3D start and 3D end point.
[<Struct;NoEquality;NoComparison>] // because it's made up from floats
[<IsReadOnly>]
[<DataContract>] // for using DataMember on fields
type Line3D =

    //[<DataMember>] // to serialize this struct field (but not properties) with Newtonsoft.Json and similar

    /// Returns the X coordinate of the start point of the line.
    [<DataMember>] val FromX:float

    /// Returns the Y coordinate of the start point of the line.
    [<DataMember>] val FromY:float

    /// Returns the Z coordinate of the start point of the line.
    [<DataMember>] val FromZ:float

    /// Returns the X coordinate of the end point of the line.
    [<DataMember>] val ToX  :float

    /// Returns the Y coordinate of the end point of the line.
    [<DataMember>] val ToY  :float

    /// Returns the Z coordinate of the end point of the line.
    [<DataMember>] val ToZ  :float

    /// Create Line3D from 3D start point and 3D end point.
    new (fromPt:Pnt, toPt:Pnt) =
        {FromX=fromPt.X; FromY=fromPt.Y; FromZ=fromPt.Z; ToX=toPt.X; ToY=toPt.Y; ToZ=toPt.Z}

    /// Create Line3D from 3D start point's x, y, and z and 3D end point's x, y, and z.
    new (fromX, fromY, fromZ, toX, toY, toZ) =
        {FromX=fromX; FromY=fromY; FromZ=fromZ; ToX=toX; ToY=toY; ToZ=toZ}

    /// Returns the length of the line.
    member inline ln.Length =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        sqrt(x*x + y*y + z*z)

     /// Returns the square length of the line.
    member inline ln.LengthSq =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        x*x + y*y + z*z

    /// Format 3D line into string including type name, X, Y and Z for start and end points, and Length.
    /// Using nice floating point number formatting .
    override ln.ToString() =
        sprintf "Euclid.Line3D from X=%s| Y=%s| Z=%s to X=%s| Y=%s| Z=%s Length %s"
            (Format.float ln.FromX)
            (Format.float ln.FromY)
            (Format.float ln.FromZ)
            (Format.float ln.ToX)
            (Format.float ln.ToY)
            (Format.float ln.ToZ)
            (Format.float ln.Length)

    /// Format 3D line into string from X, Y and Z for start and end points.
    /// Using nice floating point number formatting.
    /// But without full type name as in v.ToString()
    member ln.AsString =
        sprintf "%s, %s, %s to %s, %s, %s"
            (Format.float ln.FromX)
            (Format.float ln.FromY)
            (Format.float ln.FromZ)
            (Format.float ln.ToX)
            (Format.float ln.ToY)
            (Format.float ln.ToZ)

    /// The start point of the Line3D.
    member inline ln.From =
        Pnt(ln.FromX, ln.FromY, ln.FromZ)

    /// The end point of the Line3D.
    member inline ln.To =
        Pnt(ln.ToX, ln.ToY, ln.ToZ)

    /// Same as ln.Vector or ln.Tangent.
    /// The returned vector has the same length as the Line3D.
    member inline ln.Direction =
        Vec(ln.ToX-ln.FromX, ln.ToY-ln.FromY, ln.ToZ-ln.FromZ)

    /// Same as ln.Tangent or ln.Direction.
    /// The returned vector has the same length as the Line3D.
    member inline ln.Vector =
        Vec(ln.ToX-ln.FromX, ln.ToY-ln.FromY, ln.ToZ-ln.FromZ)

    /// Same as ln.Vector or ln.Direction.
    /// The returned vector has the same length as the Line3D.
    member inline ln.Tangent =
        Vec(ln.ToX-ln.FromX, ln.ToY-ln.FromY, ln.ToZ-ln.FromZ)

    /// Returns a unit-vector of the line Direction.
    member inline ln.UnitTangent =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        let l = sqrt(x * x  + y * y + z * z)
        if UtilEuclid.isTooTiny l then
            EuclidException.Raisef "Euclid.Line3D.UnitTangent: x:%g, y:%g and z:%g are too small for creating a unit-vector. Tolerance:%g" x y z UtilEuclid.zeroLengthTolerance
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


    /// Rotate by Quaternion around Origin.
    /// Multiplies (or applies) a Quaternion to a 3D line.
    static member inline ( *** ) (ln:Line3D, q:Quaternion) =  ln.Rotate(q)