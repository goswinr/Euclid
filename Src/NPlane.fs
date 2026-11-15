namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>]
open Euclid.UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open EuclidErrors
#if !FABLE_COMPILER
open System.Text.Json.Serialization
#endif


/// A struct containing a point and a unit normal, representing an
/// unparametrized plane defined by a point and a normal vector.
/// As opposed to the PPlane, this plane is not parametrized in X, Y, and Z directions.
/// Internally it is stored as 6 floats (the Origin point coordinates and the normal vector components),
/// just like the PPlane type. The Origin and Normal properties reconstruct the Pnt and UnitVec on demand.
/// Note: Never use the struct default constructor NPlane() as it will create an invalid zero plane.
/// Use NPlane.createFromNormal or NPlane.createUnchecked instead.
[<Struct;NoEquality;NoComparison>]// because it's made up from floats
[<IsReadOnly>]
[<DataContract>] // for using DataMember on fields, for Newtonsoft.Json
#if !FABLE_COMPILER
[<JsonConverter(typeof<NPlaneJsonConverter>)>]
#endif
type NPlane = // NPlane to avoid a name clash with Rhino Plane

    

    /// The X coordinate of the Origin 3D point of this NPlane.
    [<DataMember>] val public OriginX : float

    /// The Y coordinate of the Origin 3D point of this NPlane.
    [<DataMember>] val public OriginY : float

    /// The Z coordinate of the Origin 3D point of this NPlane.
    [<DataMember>] val public OriginZ : float

    /// The X component of the normal unit vector of this NPlane.
    [<DataMember>] val public NormalX : float

    /// The Y component of the normal unit vector of this NPlane.
    [<DataMember>] val public NormalY : float

    /// The Z component of the normal unit vector of this NPlane.
    [<DataMember>] val public NormalZ : float

    /// Create NPlane from a 3D origin point and normal unit vector.
    new (pnt:Pnt, n:UnitVec) =
        {OriginX = pnt.X; OriginY = pnt.Y; OriginZ = pnt.Z; NormalX = n.X; NormalY = n.Y; NormalZ = n.Z}

    /// Unsafe internal constructor, doesn't check if the normal is unitized, public only for inlining.
    /// Creates an NPlane from Origin coordinates and normal unit vector components.
    [<Obsolete("Unsafe internal constructor, doesn't check if the normal is unitized (unless compiled in DEBUG mode or with CHECKED_EUCLID), but must be public for inlining. So marked Obsolete instead.")>]
    new (originX:float, originY:float, originZ:float, normalX:float, normalY:float, normalZ:float) =
        #if DEBUG || CHECKED_EUCLID // CHECKED_EUCLID so checks can still be enabled when using with Fable release mode
            if isNanInfinity originX || isNanInfinity originY || isNanInfinity originZ then
                failNaN3 "NPlane() origin" originX originY originZ
            let lenSq = normalX*normalX + normalY*normalY + normalZ*normalZ
            if isNotOne lenSq then
                failNotOne3 "NPlane() normal" normalX normalY normalZ
        #endif
            {OriginX = originX; OriginY = originY; OriginZ = originZ; NormalX = normalX; NormalY = normalY; NormalZ = normalZ}

    /// Unsafe internal constructor, doesn't check if the normal is unitized.
    /// Requires correct input of unitized normal vector components.
    /// When compiled in DEBUG mode or with the CHECKED_EUCLID symbol defined the input is verified.
    static member inline createUnchecked(originX:float, originY:float, originZ:float, normalX:float, normalY:float, normalZ:float) : NPlane =
        #nowarn "44"
        NPlane(originX, originY, originZ, normalX, normalY, normalZ)
        #warnon "44" // re-enable warning for obsolete usage

    /// Unsafe internal constructor, doesn't check if the normal is unitized.
    /// Requires correct input of a unitized normal vector.
    static member inline createFromUnitNormalUnchecked (origin:Pnt, normal:UnitVec) : NPlane =
        NPlane.createUnchecked(origin.X, origin.Y, origin.Z, normal.X, normal.Y, normal.Z)

    /// Creates the Origin 3D point of this NPlane.
    member inline pl.Origin : Pnt =
        Pnt(pl.OriginX, pl.OriginY, pl.OriginZ)

    /// Gets the Planes origin.
    static member inline origin (a:NPlane) : Pnt =
        a.Origin

    /// Creates the unitized normal vector of this NPlane.
    member inline pl.Normal : UnitVec =
        UnitVec.createUnchecked(pl.NormalX, pl.NormalY, pl.NormalZ)

    /// Gets the Planes normal. A unitized vector.
    static member inline normal (p:NPlane) : UnitVec =
        p.Normal

    /// Format NPlane into string with nicely formatted floating point numbers.
    override pl.ToString() : string =
        let o = pl.Origin
        let n = pl.Normal
        $"Euclid.NPlane(Origin:%s{o.AsString}| Normal:%s{n.AsString})"

    /// Format NPlane into string with nicely formatted floating point numbers.
    /// But without type name as in pl.ToString()
    member pl.AsString : string =
        let o = pl.Origin
        let n = pl.Normal
        $"Origin:%s{o.AsString}| Normal:%s{n.AsString}"

    /// Format NPlane into string with nicely formatted floating point numbers.
    /// But without type name as in pl.ToString()
    static member inline asString (pl:NPlane) : string =
        pl.AsString

    /// Format NPlane into an F# code string that can be used to recreate the plane.
    member pl.AsFSharpCode : string =
        let f = Format.floatAsFSharpCode
        $"NPlane.createUnchecked({f pl.OriginX}, {f pl.OriginY}, {f pl.OriginZ}, {f pl.NormalX}, {f pl.NormalY}, {f pl.NormalZ})"

    /// Format NPlane into an F# code string that can be used to recreate the plane.
    static member inline asFSharpCode (pl:NPlane) : string =
        pl.AsFSharpCode

    /// Returns a new plane with the same Origin but flipped Normal.
    member inline pl.Flipped : NPlane =
        NPlane.createUnchecked(pl.OriginX, pl.OriginY, pl.OriginZ, -pl.NormalX, -pl.NormalY, -pl.NormalZ)

    /// Returns a new plane with the same Origin but flipped Normal.
    static member inline flipped (pl:NPlane) : NPlane =
        pl.Flipped

    /// Returns signed distance of point to plane, also indicating on which side it is.
    member inline pl.DistanceToXYZSigned (x:float, y:float, z:float) : float =
        XYZ.dot pl.NormalX pl.NormalY pl.NormalZ (x - pl.OriginX) (y - pl.OriginY) (z - pl.OriginZ)

    /// Returns signed distance of point to plane, also indicating on which side it is.
    static member inline distanceToXYZSigned (x:float) (y:float) (z:float) (pl:NPlane) : float =
        pl.DistanceToXYZSigned (x, y, z)

    /// Returns signed distance of point to plane, also indicating on which side it is.
    member inline pl.DistanceToPntSigned (pnt:Pnt) : float =
        pl.DistanceToXYZSigned (pnt.X, pnt.Y, pnt.Z)

    /// Returns signed distance of point to plane, also indicating on which side it is.
    static member inline distanceToPntSigned (pnt:Pnt) (pl:NPlane) : float =
        pl.DistanceToPntSigned pnt

    /// Returns absolute distance of point to plane.
    member inline pl.DistanceToXYZ (x:float, y:float, z:float) : float =
        abs (pl.DistanceToXYZSigned (x, y, z))

    /// Returns absolute distance of point to plane.
    static member inline distanceToXYZ (x:float) (y:float) (z:float) (pl:NPlane) : float =
        pl.DistanceToXYZ (x, y, z)

    /// Returns absolute distance of point to plane.
    member inline pl.DistanceToPnt (pnt:Pnt) : float =
        pl.DistanceToXYZ (pnt.X, pnt.Y, pnt.Z)

    /// Returns absolute distance of point to plane.
    static member inline distanceToPnt (pnt:Pnt) (pl:NPlane) : float =
        pl.DistanceToPnt pnt

    /// Returns the closest point on the plane from a test point.
    member inline pl.ClosestPoint (pnt:Pnt) : Pnt =
        let d = pl.DistanceToPntSigned pnt
        Pnt(pnt.X - pl.NormalX*d, pnt.Y - pl.NormalY*d, pnt.Z - pl.NormalZ*d)

    /// Returns the closest point on the plane from a test point.
    static member inline closestPoint (pnt:Pnt) (pl:NPlane) : Pnt =
        pl.ClosestPoint pnt

    /// First finds the closest point on the plane from a test point.
    /// Then returns a new plane with Origin at this point and the same Normal.
    member inline pl.PlaneAtClosestPnt (pnt:Pnt) : NPlane =
        let d = pl.DistanceToPntSigned pnt
        NPlane.createUnchecked(pnt.X - pl.NormalX*d, pnt.Y - pl.NormalY*d, pnt.Z - pl.NormalZ*d, pl.NormalX, pl.NormalY, pl.NormalZ)

    /// First finds the closest point on the plane from a test point.
    /// Then returns a new plane with Origin at this point and the same Normal.
    static member inline planeAtClosestPnt (pnt:Pnt) (pl:NPlane) : NPlane =
        pl.PlaneAtClosestPnt pnt

    /// Returns the angle to another plane in degrees, ignoring the normal's orientation.
    /// So 0.0 if the planes are parallel, and 90 degrees if the planes are perpendicular to each other.
    member inline this.Angle90ToPlane (pl:NPlane) : float =
        XYZ.angleHalfPiOfUnit this.NormalX this.NormalY this.NormalZ pl.NormalX pl.NormalY pl.NormalZ |> toDegrees

    /// Returns the angle to another plane in degrees, ignoring the normal's orientation.
    /// So 0.0 if the planes are parallel, and 90 degrees if the planes are perpendicular to each other.
    static member inline angle90ToPlane (pl:NPlane) (other:NPlane) : float =
        pl.Angle90ToPlane other

    /// Returns the angle to 3D vector in degrees, ignoring the plane's orientation.
    /// So 0.0 if the vector is parallel to the plane, and 90 degrees if the vector is perpendicular to the plane.
    member inline pl.Angle90ToVec (v:Vec) : float =
        let x = v.X
        let y = v.Y
        let z = v.Z
        let l = XYZ.length x y z
        if isTooTiny l then
            failTooSmall "NPlane.Angle90ToVec" v
        let f = 1.0 / l
        90.0 - (XYZ.angleHalfPiOfUnit (x*f) (y*f) (z*f) pl.NormalX pl.NormalY pl.NormalZ |> toDegrees)

    /// Returns the angle to 3D unit-vector in degrees, ignoring the plane's orientation.
    /// So 0.0 if the vector is parallel to the plane, and 90 degrees if the vector is perpendicular to the plane.
    member inline pl.Angle90ToVec (v:UnitVec) : float =
        90.0 - (XYZ.angleHalfPiOfUnit v.X v.Y v.Z pl.NormalX pl.NormalY pl.NormalZ |> toDegrees)

    /// Returns the angle to 3D vector in degrees, ignoring the plane's orientation.
    /// So 0.0 if the vector is parallel to the plane, and 90 degrees if the vector is perpendicular to the plane.
    static member inline angle90ToVec (v:Vec) (pl:NPlane) : float =
        pl.Angle90ToVec v

    /// Returns the angle to 3D unit-vector in degrees, ignoring the plane's orientation.
    /// So 0.0 if the vector is parallel to the plane, and 90 degrees if the vector is perpendicular to the plane.
    static member inline angle90ToUnitVec (v:UnitVec) (pl:NPlane) : float =
        pl.Angle90ToVec v

    /// Returns the angle to a Line3D in degrees, ignoring the normal's orientation.
    /// So 0.0 if the line is parallel to the plane, and 90 degrees if the line is perpendicular to the plane.
    member inline pl.Angle90ToLine (ln:Line3D) : float =
        let x = ln.VectorX
        let y = ln.VectorY
        let z = ln.VectorZ
        let l = XYZ.length x y z
        if isTooTiny l then
            failTooSmall "NPlane.Angle90ToLine" ln
        let f = 1.0 / l
        90.0 - (XYZ.angleHalfPiOfUnit (x*f) (y*f) (z*f) pl.NormalX pl.NormalY pl.NormalZ |> toDegrees)

    /// Returns the angle to a Line3D in degrees, ignoring the normal's orientation.
    /// So 0.0 if the line is parallel to the plane, and 90 degrees if the line is perpendicular to the plane.
    static member inline angle90ToLine (ln:Line3D) (pl:NPlane) : float =
        pl.Angle90ToLine ln

    /// Checks if two planes are coincident within the distance tolerance (1e-6 by default).
    /// This means that their normals are parallel within the angle tolerance
    /// and the distance of the second origin to the first plane is less than the distance tolerance.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minimum cosine value.
    /// See Euclid.Cosine module.
    member inline pl.IsCoincidentTo (other:NPlane,
                                    [<OPT;DEF(1e-6)>] distanceTolerance:float,
                                    [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine>) : bool  =
        // the normals are parallel:
        abs (XYZ.dot other.NormalX other.NormalY other.NormalZ pl.NormalX pl.NormalY pl.NormalZ) > float minCosine
        &&
        pl.DistanceToXYZ (other.OriginX, other.OriginY, other.OriginZ) < distanceTolerance

    /// <summary>Moves the origin of the plane by a vector. The normal stays the same.</summary>
    /// <param name="v">The vector to move by.</param>
    /// <returns>A new moved plane.</returns>
    member inline pl.Move (v:Vec) : NPlane =
        NPlane.createUnchecked(pl.OriginX + v.X, pl.OriginY + v.Y, pl.OriginZ + v.Z, pl.NormalX, pl.NormalY, pl.NormalZ)

    /// <summary>Moves the origin of the plane along the world X-axis. The normal stays the same.</summary>
    /// <param name="distance">The distance to move by.</param>
    /// <returns>A new moved plane.</returns>
    member inline pl.MoveX (distance:float) : NPlane =
        NPlane.createUnchecked(pl.OriginX + distance, pl.OriginY, pl.OriginZ, pl.NormalX, pl.NormalY, pl.NormalZ)

    /// <summary>Moves the origin of the plane along the world Y-axis. The normal stays the same.</summary>
    /// <param name="distance">The distance to move by.</param>
    /// <returns>A new moved plane.</returns>
    member inline pl.MoveY (distance:float) : NPlane =
        NPlane.createUnchecked(pl.OriginX, pl.OriginY + distance, pl.OriginZ, pl.NormalX, pl.NormalY, pl.NormalZ)

    /// <summary>Moves the origin of the plane along the world Z-axis. The normal stays the same.</summary>
    /// <param name="distance">The distance to move by.</param>
    /// <returns>A new moved plane.</returns>
    member inline pl.MoveZ (distance:float) : NPlane =
        NPlane.createUnchecked(pl.OriginX, pl.OriginY, pl.OriginZ + distance, pl.NormalX, pl.NormalY, pl.NormalZ)

    /// <summary>Scales the origin of the plane by a factor from the world origin. The normal stays the same.</summary>
    /// <param name="factor">The scale factor.</param>
    /// <returns>A new scaled plane.</returns>
    member inline pl.Scale (factor:float) : NPlane =
        NPlane.createUnchecked(pl.OriginX * factor, pl.OriginY * factor, pl.OriginZ * factor, pl.NormalX, pl.NormalY, pl.NormalZ)

    /// <summary>Scales the origin of the plane by a factor from a center point. The normal stays the same.</summary>
    /// <param name="cen">The center point of the scaling.</param>
    /// <param name="factor">The scale factor.</param>
    /// <returns>A new scaled plane.</returns>
    member inline pl.ScaleOn (cen:Pnt) (factor:float) : NPlane =
        NPlane.createUnchecked(
            cen.X + (pl.OriginX - cen.X) * factor,
            cen.Y + (pl.OriginY - cen.Y) * factor,
            cen.Z + (pl.OriginZ - cen.Z) * factor,
            pl.NormalX, pl.NormalY, pl.NormalZ)

    /// <summary>Rotates the plane by a Quaternion around the world origin.
    /// The origin and the normal are both rotated.</summary>
    /// <param name="q">The rotation to apply.</param>
    /// <returns>A new rotated plane.</returns>
    member pl.Rotate (q:Quaternion) : NPlane =
        let qx = q.X
        let qy = q.Y
        let qz = q.Z
        let qw = q.W
        let mutable x = pl.OriginX
        let mutable y = pl.OriginY
        let mutable z = pl.OriginZ
        let mutable tx = 2.0 * (qy * z - qz * y)
        let mutable ty = 2.0 * (qz * x - qx * z)
        let mutable tz = 2.0 * (qx * y - qy * x)
        let ox = x + qw * tx + qy * tz - qz * ty
        let oy = y + qw * ty + qz * tx - qx * tz
        let oz = z + qw * tz + qx * ty - qy * tx
        x <- pl.NormalX
        y <- pl.NormalY
        z <- pl.NormalZ
        tx <- 2.0 * (qy * z - qz * y)
        ty <- 2.0 * (qz * x - qx * z)
        tz <- 2.0 * (qx * y - qy * x)
        NPlane.createUnchecked(
            ox, oy, oz,
            x + qw * tx + qy * tz - qz * ty,
            y + qw * ty + qz * tx - qx * tz,
            z + qw * tz + qx * ty - qy * tx)

    /// <summary>Rotates the plane by a Quaternion around a given center point.
    /// The origin and the normal are both rotated.</summary>
    /// <param name="cen">The center point of the rotation.</param>
    /// <param name="q">The rotation to apply.</param>
    /// <returns>A new rotated plane.</returns>
    member pl.RotateAround (cen:Pnt) (q:Quaternion) : NPlane =
        let qx = q.X
        let qy = q.Y
        let qz = q.Z
        let qw = q.W
        let mutable x = pl.OriginX - cen.X
        let mutable y = pl.OriginY - cen.Y
        let mutable z = pl.OriginZ - cen.Z
        let mutable tx = 2.0 * (qy * z - qz * y)
        let mutable ty = 2.0 * (qz * x - qx * z)
        let mutable tz = 2.0 * (qx * y - qy * x)
        let ox = x + qw * tx + qy * tz - qz * ty + cen.X
        let oy = y + qw * ty + qz * tx - qx * tz + cen.Y
        let oz = z + qw * tz + qx * ty - qy * tx + cen.Z
        x <- pl.NormalX
        y <- pl.NormalY
        z <- pl.NormalZ
        tx <- 2.0 * (qy * z - qz * y)
        ty <- 2.0 * (qz * x - qx * z)
        tz <- 2.0 * (qx * y - qy * x)
        NPlane.createUnchecked(
            ox, oy, oz,
            x + qw * tx + qy * tz - qz * ty,
            y + qw * ty + qz * tx - qx * tz,
            z + qw * tz + qx * ty - qy * tx)

    /// <summary>Transforms the plane by a RigidMatrix.
    /// The origin is rotated and moved, the normal is only rotated.</summary>
    /// <param name="m">The rigid transformation to apply.</param>
    /// <returns>A new transformed plane.</returns>
    member pl.TransformRigid (m:RigidMatrix) : NPlane =
        let x = pl.OriginX
        let y = pl.OriginY
        let z = pl.OriginZ
        let nx = pl.NormalX
        let ny = pl.NormalY
        let nz = pl.NormalZ
        NPlane.createUnchecked(
            m.M11*x + m.M21*y + m.M31*z + m.X41,
            m.M12*x + m.M22*y + m.M32*z + m.Y42,
            m.M13*x + m.M23*y + m.M33*z + m.Z43,
            m.M11*nx + m.M21*ny + m.M31*nz,
            m.M12*nx + m.M22*ny + m.M32*nz,
            m.M13*nx + m.M23*ny + m.M33*nz)


    // #endregion
    // #region Static members

    /// Checks if two 3D planes are equal within tolerance.
    /// The tolerance applies to each coordinate of the origin and of the normal separately, not to the Euclidean distance between them.
    /// Use NPlane.equalsExact to check for an exact match.
    static member inline equalsWithin (tol:float) (a:NPlane) (b:NPlane)  : bool =
        abs (a.OriginX - b.OriginX) <= tol &&
        abs (a.OriginY - b.OriginY) <= tol &&
        abs (a.OriginZ - b.OriginZ) <= tol &&
        abs (a.NormalX - b.NormalX) <= tol &&
        abs (a.NormalY - b.NormalY) <= tol &&
        abs (a.NormalZ - b.NormalZ) <= tol

    /// Checks if two 3D planes are exactly equal.
    /// Compares each field with the = operator, so a NaN value is never equal.
    static member inline equalsExact (a:NPlane) (b:NPlane) : bool =
        a.OriginX = b.OriginX &&
        a.OriginY = b.OriginY &&
        a.OriginZ = b.OriginZ &&
        a.NormalX = b.NormalX &&
        a.NormalY = b.NormalY &&
        a.NormalZ = b.NormalZ

    /// <summary>Checks if two 3D planes are coincident within the given tolerances.
    /// This means that their normals are parallel within the angle tolerance
    /// and the distance of the second origin to the first plane is less than the distance tolerance.</summary>
    /// <param name="distanceTolerance">The maximum distance of the origin of plane b to plane a.</param>
    /// <param name="minCosine">The minimum cosine of the angle between the normals, ignoring their orientation. See the Euclid.Cosine module.</param>
    /// <param name="a">The first plane.</param>
    /// <param name="b">The second plane.</param>
    /// <returns>TRUE if the planes are coincident.</returns>
    static member inline isCoincidentTo (distanceTolerance:float) (minCosine:float<Cosine.cosine>) (a:NPlane) (b:NPlane) : bool =
        a.IsCoincidentTo(b, distanceTolerance, minCosine)

    /// Create Plane from a normal vector and an origin point. The normal vector gets unitized.
    static member createFromNormal (normal:Vec) (pnt:Pnt) : NPlane =
        let x = normal.X
        let y = normal.Y
        let z = normal.Z
        let l = XYZ.length x y z
        if isTooTiny l then failTooSmall "NPlane.createFromNormal" normal
        let li = 1.0 / l
        NPlane.createUnchecked(pnt.X, pnt.Y, pnt.Z, li*x, li*y, li*z)

    /// Create Plane from an already normalized normal vector and an origin point.
    static member inline createFromUnitNormal (normal:UnitVec) (pnt:Pnt) : NPlane =
        NPlane.createUnchecked(pnt.X, pnt.Y, pnt.Z, normal.X, normal.Y, normal.Z)

    /// Create Plane from 3 points.
    /// Point 'a' becomes the origin.
    /// Normal is calculated as cross product (c-b) × (a-b) following the right-hand rule.
    /// Fails if the three points are collinear.
    static member inline createFrom3Pnts (a:Pnt) (b:Pnt) (c:Pnt) : NPlane =
        let ux = c.X - b.X
        let uy = c.Y - b.Y
        let uz = c.Z - b.Z
        let vx = a.X - b.X
        let vy = a.Y - b.Y
        let vz = a.Z - b.Z
        let nx = uy * vz - uz * vy
        let ny = uz * vx - ux * vz
        let nz = ux * vy - uy * vx
        let lSq = XYZ.lengthSq nx ny nz
        if isTooSmallSq lSq then
            failCollinear "NPlane.createFrom3Pnts" a b c
        let f = 1.0 / sqrt lSq
        NPlane.createUnchecked(a.X, a.Y, a.Z, nx*f, ny*f, nz*f)

    /// Creates an NPlane from a parametrized PPlane.
    /// Uses the PPlane's origin and Z-axis (which becomes the normal).
    static member inline createFromPPlane (p:PPlane) : NPlane =
        NPlane.createUnchecked(p.OriginX, p.OriginY, p.OriginZ, p.ZAxisX, p.ZAxisY, p.ZAxisZ)

    /// Gets the Plane at world origin with normal in world Z direction.
    static member inline WorldXY : NPlane =
        NPlane.createUnchecked(0.0, 0.0, 0.0, 0.0, 0.0, 1.0)

    /// Returns the line of intersection between two planes.
    /// Or ValueNone if they are parallel or coincident.
    static member tryIntersect (a:NPlane) (b:NPlane) : Line3D voption =
        let vx = a.NormalY * b.NormalZ - a.NormalZ * b.NormalY
        let vy = a.NormalZ * b.NormalX - a.NormalX * b.NormalZ
        let vz = a.NormalX * b.NormalY - a.NormalY * b.NormalX
        if isTooSmallSq (XYZ.lengthSq vx vy vz) then
            ValueNone
        else
            let px = vy * a.NormalZ - vz * a.NormalY
            let py = vz * a.NormalX - vx * a.NormalZ
            let pz = vx * a.NormalY - vy * a.NormalX
            let nenner = XYZ.dot px py pz b.NormalX b.NormalY b.NormalZ
            let dot = XYZ.dot (b.OriginX - a.OriginX) (b.OriginY - a.OriginY) (b.OriginZ - a.OriginZ) b.NormalX b.NormalY b.NormalZ
            let t = dot / nenner
            let xpt = a.OriginX + px * t
            let ypt = a.OriginY + py * t
            let zpt = a.OriginZ + pz * t
            ValueSome (Line3D(xpt, ypt, zpt, xpt + vx, ypt + vy, zpt + vz))

    /// Returns the line of intersection between two planes.
    /// Fails if they are parallel or coincident.
    /// Use NPlane.tryIntersect to get ValueNone instead.
    static member intersect (a:NPlane) (b:NPlane) : Line3D =
        match NPlane.tryIntersect a b with
        | ValueSome ln -> ln
        | ValueNone -> fail $"NPlane.intersect: the planes are parallel or coincident:{nl}{a}{nl}{b}"


    /// Returns the intersection parameter of an infinite line / ray with the plane.
    /// Or ValueNone if they are parallel.
    static member tryIntersectRay (ln:Line3D) (pl:NPlane) : float voption =
        let nenner = XYZ.dot ln.VectorX ln.VectorY ln.VectorZ pl.NormalX pl.NormalY pl.NormalZ
        if isTooTiny(abs nenner) then
            ValueNone
        else
            let dot = XYZ.dot (pl.OriginX - ln.FromX) (pl.OriginY - ln.FromY) (pl.OriginZ - ln.FromZ) pl.NormalX pl.NormalY pl.NormalZ
            ValueSome (dot / nenner)

    /// Returns the intersection parameter of an infinite line / ray with the plane.
    /// Fails if they are parallel, or if the line has zero length.
    /// Use NPlane.tryIntersectRay to get ValueNone instead.
    static member intersectRay (ln:Line3D) (pl:NPlane) : float =
        match NPlane.tryIntersectRay ln pl with
        | ValueSome t -> t
        | ValueNone -> fail $"NPlane.intersectRay: the line is parallel to the plane or has zero length:{nl}{ln}{nl}{pl}"

    /// Returns intersection point of a finite line  with the Plane.
    /// Or ValueNone if they are parallel or the domain of intersection is outside 0.0 to 1.0
    /// Intersection just below 0.0 or just above 1.0 within tolerance of 1e-6 are clamped to 0.0 or 1.0
    static member tryIntersectLine (ln:Line3D) (pl:NPlane) : Pnt voption =
        match NPlane.tryIntersectRay ln pl with
        | ValueSome t ->
            if isBetweenZeroAndOneTolerantIncl t then
                let c = clamp01 t
                ValueSome (Pnt(ln.FromX + ln.VectorX*c, ln.FromY + ln.VectorY*c, ln.FromZ + ln.VectorZ*c))
            else
                ValueNone
        | ValueNone ->
            ValueNone

    /// Returns intersection point of a finite line with the Plane.
    /// Intersection just below 0.0 or just above 1.0 within tolerance of 1e-6 are clamped to 0.0 or 1.0
    /// Fails if they are parallel, if the line has zero length, or if the domain of intersection is outside 0.0 to 1.0.
    /// Use NPlane.tryIntersectLine to get ValueNone instead.
    static member intersectLine (ln:Line3D) (pl:NPlane) : Pnt =
        match NPlane.tryIntersectLine ln pl with
        | ValueSome pnt -> pnt
        | ValueNone -> fail $"NPlane.intersectLine: the line does not intersect the plane, it is parallel, too short or does not reach it:{nl}{ln}{nl}{pl}"

    /// Checks if a finite Line3D intersects with Plane in one point.
    /// Returns FALSE for parallel and coincident lines.
    /// Adds a tolerance of 1e-6 to the line's domain, so that intersection just below 0.0 or just above 1.0 is considered intersecting.
    static member inline doLinePlaneIntersect (ln:Line3D) (pl:NPlane) : bool =
        let nenner = XYZ.dot ln.VectorX ln.VectorY ln.VectorZ pl.NormalX pl.NormalY pl.NormalZ
        if isTooTiny (abs nenner) then
            false
        else
            let dot = XYZ.dot (pl.OriginX - ln.FromX) (pl.OriginY - ln.FromY) (pl.OriginZ - ln.FromZ) pl.NormalX pl.NormalY pl.NormalZ
            let t = dot / nenner
            isBetweenZeroAndOneTolerantIncl t

    /// Returns a new plane offset along the normal vector.
    static member inline offset (dist:float) (pl:NPlane) : NPlane =
        NPlane.createUnchecked(pl.OriginX + pl.NormalX*dist, pl.OriginY + pl.NormalY*dist, pl.OriginZ + pl.NormalZ*dist, pl.NormalX, pl.NormalY, pl.NormalZ)

    /// Offsets the plane by the given distance in the direction determined by a point.
    /// If the point is on the positive side of the plane (same direction as normal), offsets in the normal direction.
    /// If the point is on the negative side, offsets in the opposite direction.
    static member inline offsetInDirection (dirPnt:Pnt) (dist:float) (pl:NPlane) : NPlane =
        let dot = XYZ.dot pl.NormalX pl.NormalY pl.NormalZ (dirPnt.X - pl.OriginX) (dirPnt.Y - pl.OriginY) (dirPnt.Z - pl.OriginZ)
        if dot >= 0.0 then
            NPlane.createUnchecked(pl.OriginX + pl.NormalX*dist, pl.OriginY + pl.NormalY*dist, pl.OriginZ + pl.NormalZ*dist, pl.NormalX, pl.NormalY, pl.NormalZ)
        else
            NPlane.createUnchecked(pl.OriginX - pl.NormalX*dist, pl.OriginY - pl.NormalY*dist, pl.OriginZ - pl.NormalZ*dist, pl.NormalX, pl.NormalY, pl.NormalZ)

    /// Scales the plane's origin by a given factor from the world origin. The normal remains unchanged.
    static member inline scale (factor:float) (pl:NPlane) : NPlane =
        NPlane.createUnchecked(pl.OriginX * factor, pl.OriginY * factor, pl.OriginZ * factor, pl.NormalX, pl.NormalY, pl.NormalZ)

    /// Move plane origin by vector.
    static member inline move (translation:Vec) (pl:NPlane)  : NPlane =
        NPlane.createUnchecked(pl.OriginX + translation.X, pl.OriginY + translation.Y, pl.OriginZ + translation.Z, pl.NormalX, pl.NormalY, pl.NormalZ)

    /// <summary>Moves the origin of the plane along the world X-axis. The normal stays the same.</summary>
    /// <param name="distance">The distance to move by.</param>
    /// <param name="pl">The plane to move.</param>
    /// <returns>A new moved plane.</returns>
    static member inline moveX (distance:float) (pl:NPlane) : NPlane =
        pl.MoveX distance

    /// <summary>Moves the origin of the plane along the world Y-axis. The normal stays the same.</summary>
    /// <param name="distance">The distance to move by.</param>
    /// <param name="pl">The plane to move.</param>
    /// <returns>A new moved plane.</returns>
    static member inline moveY (distance:float) (pl:NPlane) : NPlane =
        pl.MoveY distance

    /// <summary>Moves the origin of the plane along the world Z-axis. The normal stays the same.</summary>
    /// <param name="distance">The distance to move by.</param>
    /// <param name="pl">The plane to move.</param>
    /// <returns>A new moved plane.</returns>
    static member inline moveZ (distance:float) (pl:NPlane) : NPlane =
        pl.MoveZ distance

    /// <summary>Scales the origin of the plane by a factor from a center point. The normal stays the same.</summary>
    /// <param name="cen">The center point of the scaling.</param>
    /// <param name="factor">The scale factor.</param>
    /// <param name="pl">The plane to scale.</param>
    /// <returns>A new scaled plane.</returns>
    static member inline scaleOn (cen:Pnt) (factor:float) (pl:NPlane) : NPlane =
        pl.ScaleOn cen factor

    /// <summary>Rotates the plane by a Quaternion around the world origin.
    /// The origin and the normal are both rotated.</summary>
    /// <param name="q">The rotation to apply.</param>
    /// <param name="pl">The plane to rotate.</param>
    /// <returns>A new rotated plane.</returns>
    static member inline rotate (q:Quaternion) (pl:NPlane) : NPlane =
        pl.Rotate q

    /// <summary>Rotates the plane by a Quaternion around a given center point.
    /// The origin and the normal are both rotated.</summary>
    /// <param name="cen">The center point of the rotation.</param>
    /// <param name="q">The rotation to apply.</param>
    /// <param name="pl">The plane to rotate.</param>
    /// <returns>A new rotated plane.</returns>
    static member inline rotateAround (cen:Pnt) (q:Quaternion) (pl:NPlane) : NPlane =
        pl.RotateAround cen q

    /// <summary>Transforms the plane by a RigidMatrix.
    /// The origin is rotated and moved, the normal is only rotated.</summary>
    /// <param name="m">The rigid transformation to apply.</param>
    /// <param name="pl">The plane to transform.</param>
    /// <returns>A new transformed plane.</returns>
    static member inline transformRigid (m:RigidMatrix) (pl:NPlane) : NPlane =
        pl.TransformRigid m

#if !FABLE_COMPILER
/// Serializes an NPlane as its origin and normal components with System.Text.Json.
and internal NPlaneJsonConverter() =
    inherit JsonConverter<NPlane>()

    override _.Write(writer, plane, options) =
        writer.WriteStartObject()
        JsonConvert.writeFloat writer options "OriginX" plane.OriginX
        JsonConvert.writeFloat writer options "OriginY" plane.OriginY
        JsonConvert.writeFloat writer options "OriginZ" plane.OriginZ
        JsonConvert.writeFloat writer options "NormalX" plane.NormalX
        JsonConvert.writeFloat writer options "NormalY" plane.NormalY
        JsonConvert.writeFloat writer options "NormalZ" plane.NormalZ
        writer.WriteEndObject()

    override _.Read(reader, _, options) =
        JsonConvert.readStartObject &reader "Euclid.NPlane"
        let originX = JsonConvert.readFloat &reader options "Euclid.NPlane" "OriginX"
        let originY = JsonConvert.readFloat &reader options "Euclid.NPlane" "OriginY"
        let originZ = JsonConvert.readFloat &reader options "Euclid.NPlane" "OriginZ"
        let normalX = JsonConvert.readFloat &reader options "Euclid.NPlane" "NormalX"
        let normalY = JsonConvert.readFloat &reader options "Euclid.NPlane" "NormalY"
        let normalZ = JsonConvert.readFloat &reader options "Euclid.NPlane" "NormalZ"
        JsonConvert.readEndObject &reader "Euclid.NPlane"
        NPlane.createFromNormal (Vec(normalX, normalY, normalZ)) (Pnt(originX, originY, originZ))
#endif
