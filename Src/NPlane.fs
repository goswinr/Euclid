namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>]
open Euclid.UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open EuclidErrors


/// A struct containing a point and a unit normal, representing an
/// unparametrized plane defined by a point and a normal vector.
/// As opposed to the PPlane, this plane is not parametrized in X, Y, and Z directions.
/// Internally it is stored as 6 floats (the Origin point coordinates and the normal vector components),
/// just like the PPlane type. The Origin and Normal properties reconstruct the Pnt and UnitVec on demand.
/// Note: Never use the struct default constructor NPlane() as it will create an invalid zero plane.
/// Use NPlane.create or NPlane.createUnchecked instead.
[<Struct;NoEquality;NoComparison>]// because it's made up from floats
[<IsReadOnly>]
[<DataContract>] // for using DataMember on fields
type NPlane = // NPlane to avoid a name clash with Rhino Plane

    //[<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar

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
    new (pt:Pnt, n:UnitVec) =
        {OriginX = pt.X; OriginY = pt.Y; OriginZ = pt.Z; NormalX = n.X; NormalY = n.Y; NormalZ = n.Z}

    /// Unsafe internal constructor, doesn't check if the normal is unitized, public only for inlining.
    /// Creates an NPlane from Origin coordinates and normal unit vector components.
    [<Obsolete("Unsafe internal constructor, doesn't check if the normal is unitized, but must be public for inlining. So marked Obsolete instead.")>]
    new (originX:float, originY:float, originZ:float, normalX:float, normalY:float, normalZ:float) =
        {OriginX = originX; OriginY = originY; OriginZ = originZ; NormalX = normalX; NormalY = normalY; NormalZ = normalZ}

    /// Unsafe internal constructor, doesn't check if the normal is unitized.
    /// Requires correct input of unitized normal vector components.
    static member inline createUnchecked(originX:float, originY:float, originZ:float, normalX:float, normalY:float, normalZ:float) : NPlane =
        #nowarn "44"
        NPlane(originX, originY, originZ, normalX, normalY, normalZ)
        #warnon "44" // re-enable warning for obsolete usage

    /// Unsafe internal constructor, doesn't check if the normal is unitized.
    /// Requires correct input of a unitized normal vector.
    static member inline createUncheckedVec (origin:Pnt, normal:UnitVec) : NPlane =
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
        $"NPlane.createUnchecked({pl.OriginX}, {pl.OriginY}, {pl.OriginZ}, {pl.NormalX}, {pl.NormalY}, {pl.NormalZ})"

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
    member inline pl.DistanceToPtSigned (pt:Pnt) : float =
        pl.DistanceToXYZSigned (pt.X, pt.Y, pt.Z)

    /// Returns signed distance of point to plane, also indicating on which side it is.
    static member inline distanceToPtSigned (pt:Pnt) (pl:NPlane) : float =
        pl.DistanceToPtSigned pt

    /// Returns absolute distance of point to plane.
    member inline pl.DistanceToXYZ (x:float, y:float, z:float) : float =
        abs (pl.DistanceToXYZSigned (x, y, z))

    /// Returns absolute distance of point to plane.
    static member inline distanceToXYZ (x:float) (y:float) (z:float) (pl:NPlane) : float =
        pl.DistanceToXYZ (x, y, z)

    /// Returns absolute distance of point to plane.
    member inline pl.DistanceToPt (pt:Pnt) : float =
        pl.DistanceToXYZ (pt.X, pt.Y, pt.Z)

    /// Returns absolute distance of point to plane.
    static member inline distanceToPt (pt:Pnt) (pl:NPlane) : float =
        pl.DistanceToPt pt

    /// Returns the closest point on the plane from a test point.
    member inline pl.ClosestPoint (pt:Pnt) : Pnt =
        let d = pl.DistanceToPtSigned pt
        Pnt(pt.X - pl.NormalX*d, pt.Y - pl.NormalY*d, pt.Z - pl.NormalZ*d)

    /// Returns the closest point on the plane from a test point.
    static member inline closestPoint (pt:Pnt) (pl:NPlane) : Pnt =
        pl.ClosestPoint pt

    /// First finds the closest point on the plane from a test point.
    /// Then returns a new plane with Origin at this point and the same Normal.
    member inline pl.PlaneAtClPt (pt:Pnt) : NPlane =
        let d = pl.DistanceToPtSigned pt
        NPlane.createUnchecked(pt.X - pl.NormalX*d, pt.Y - pl.NormalY*d, pt.Z - pl.NormalZ*d, pl.NormalX, pl.NormalY, pl.NormalZ)

    /// First finds the closest point on the plane from a test point.
    /// Then returns a new plane with Origin at this point and the same Normal.
    static member inline planeAtClPt (pt:Pnt) (pl:NPlane) : NPlane =
        pl.PlaneAtClPt pt

    /// Returns the angle to another plane in degrees, ignoring the normal's orientation.
    /// So 0.0 if the planes are parallel, and 90 degrees if the planes are perpendicular to each other.
    member inline this.Angle90ToPlane (pl:NPlane) : float =
        UnitVec.angle90 this.Normal pl.Normal

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
        let u = UnitVec.createUnchecked(x*f, y*f, z*f)
        90.0 - UnitVec.angle90 u pl.Normal

    /// Returns the angle to 3D unit-vector in degrees, ignoring the plane's orientation.
    /// So 0.0 if the vector is parallel to the plane, and 90 degrees if the vector is perpendicular to the plane.
    member inline pl.Angle90ToVec (v:UnitVec) : float =
        90.0 - UnitVec.angle90 v pl.Normal

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
        let u = UnitVec.createUnchecked(x*f, y*f, z*f)
        90.0 - UnitVec.angle90 u pl.Normal

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
        pl.Normal.IsParallelTo(other.Normal, minCosine)
        &&
        pl.DistanceToPt other.Origin < distanceTolerance


    // #endregion
    // #region Static members

    /// Checks if two 3D planes are equal within tolerance.
    /// The same tolerance is used for the origin and the tips of the normal.
    /// Use a tolerance of 0.0 to check for an exact match.
    static member inline equals (tol:float) (a:NPlane) (b:NPlane)  : bool =
        abs (a.OriginX - b.OriginX) <= tol &&
        abs (a.OriginY - b.OriginY) <= tol &&
        abs (a.OriginZ - b.OriginZ) <= tol &&
        abs (a.NormalX - b.NormalX) <= tol &&
        abs (a.NormalY - b.NormalY) <= tol &&
        abs (a.NormalZ - b.NormalZ) <= tol

    /// Checks if two 3D planes are coincident.
    /// This means that the normals are parallel within 0.25 degrees
    /// and the distance of second origin to the first plane is less than 1e-6 units tolerance.
    static member inline areCoincident (a:NPlane) (b:NPlane) : bool =
        a.IsCoincidentTo (b)

    /// Create Plane, normal vector gets unitized in constructor.
    static member create(pt:Pnt, normal:Vec) : NPlane =
        let x = normal.X
        let y = normal.Y
        let z = normal.Z
        let l = XYZ.length x y z
        if isTooTiny l then failTooSmall "NPlane.create" normal
        let li = 1.0 / l
        NPlane.createUnchecked(pt.X, pt.Y, pt.Z, li*x, li*y, li*z)

    /// Create Plane from already normalized input vector.
    static member inline create(pt:Pnt, normal:UnitVec) : NPlane =
        NPlane.createUnchecked(pt.X, pt.Y, pt.Z, normal.X, normal.Y, normal.Z)

    /// Create Plane from 3 points.
    /// Point 'a' becomes the origin.
    /// Normal is calculated as cross product (c-b) × (a-b) following the right-hand rule.
    /// Fails if the three points are colinear.
    static member inline createFrom3Points (a:Pnt) (b:Pnt) (c:Pnt) : NPlane =
        let ux = c.X - b.X
        let uy = c.Y - b.Y
        let uz = c.Z - b.Z
        let vx = a.X - b.X
        let vy = a.Y - b.Y
        let vz = a.Z - b.Z
        let nx = uy * vz - uz * vy
        let ny = uz * vx - ux * vz
        let nz = ux * vy - uy * vx
        let lSq = XYZ.sqLength nx ny nz
        if isTooSmallSq lSq then
            failColinear "NPlane.createFrom3Points" a b c
        let f = 1.0 / sqrt lSq
        NPlane.createUnchecked(a.X, a.Y, a.Z, nx*f, ny*f, nz*f)

    /// Creates an NPlane from a parametrized PPlane.
    /// Uses the PPlane's origin and Z-axis (which becomes the normal).
    static member inline createFromPPlane (p:PPlane) : NPlane =
        NPlane.createUnchecked(p.OriginX, p.OriginY, p.OriginZ, p.ZaxisX, p.ZaxisY, p.ZaxisZ)

    /// Gets the Plane at world origin with normal in world Z direction.
    static member inline xyPlane : NPlane =
        NPlane.createUnchecked(0.0, 0.0, 0.0, 0.0, 0.0, 1.0)

    /// Returns the angle to another Plane in Degree, ignoring the normal's orientation.
    /// So between 0 to 90 degrees.
    static member inline angleTo (a:NPlane) (b:NPlane) : float =
        a.Angle90ToPlane b

    /// Returns the line of intersection between two planes.
    /// Or None if they are parallel or coincident.
    static member intersect (a:NPlane) (b:NPlane) : Line3D option =
        let vx = a.NormalY * b.NormalZ - a.NormalZ * b.NormalY
        let vy = a.NormalZ * b.NormalX - a.NormalX * b.NormalZ
        let vz = a.NormalX * b.NormalY - a.NormalY * b.NormalX
        if isTooSmallSq (XYZ.sqLength vx vy vz) then
            None
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
            Some (Line3D(xpt, ypt, zpt, xpt + vx, ypt + vy, zpt + vz))

    /// Returns the parameter of intersection on a infinite line / ray with the Plane.
    /// Or None if they are parallel.
    static member intersectLineParameter (ln:Line3D) (pl:NPlane) : float option =
        let vx = ln.VectorX
        let vy = ln.VectorY
        let vz = ln.VectorZ
        let nenner = XYZ.dot vx vy vz pl.NormalX pl.NormalY pl.NormalZ
        if isTooSmall(abs nenner) then
            None
        else
            let dot = XYZ.dot (pl.OriginX - ln.FromX) (pl.OriginY - ln.FromY) (pl.OriginZ - ln.FromZ) pl.NormalX pl.NormalY pl.NormalZ
            Some (dot / nenner)

    /// Returns intersection point of a infinite line / ray with the Plane.
    /// Or None if they are parallel.
    static member intersectRay (ln:Line3D) (pl:NPlane) : Pnt option =
        let vx = ln.VectorX
        let vy = ln.VectorY
        let vz = ln.VectorZ
        let nenner = XYZ.dot vx vy vz pl.NormalX pl.NormalY pl.NormalZ
        if isTooSmall(abs nenner) then
            None
        else
            let dot = XYZ.dot (pl.OriginX - ln.FromX) (pl.OriginY - ln.FromY) (pl.OriginZ - ln.FromZ) pl.NormalX pl.NormalY pl.NormalZ
            let t = dot / nenner
            Some (Pnt(ln.FromX + vx*t, ln.FromY + vy*t, ln.FromZ + vz*t))

    /// Returns intersection point of a finite line  with the Plane.
    /// Or None if they are parallel or the domain of intersection is outside 0.0 to 1.0
    /// Intersection just below 0.0 or just above 1.0 within tolerance of 1e-6 are clamped to 0.0 or 1.0
    static member intersectLine (ln:Line3D) (pl:NPlane) : Pnt option =
        match NPlane.intersectLineParameter ln pl with
        | Some t ->
            if isBetweenZeroAndOneTolerantIncl t then
                let c = clampBetweenZeroAndOne t
                Some (Pnt(ln.FromX + ln.VectorX*c, ln.FromY + ln.VectorY*c, ln.FromZ + ln.VectorZ*c))
            else
                None
        | None ->
            None

    /// Checks if a finite Line3D intersects with Plane in one point.
    /// Returns false for parallel and coincident lines.
    static member inline doLinePlaneIntersect (ln:Line3D) (pl:NPlane) : bool =
        let nenner = XYZ.dot ln.VectorX ln.VectorY ln.VectorZ pl.NormalX pl.NormalY pl.NormalZ
        if isTooSmall (abs nenner) then
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
    static member inline offsetInDir (dirPt:Pnt) (dist:float) (pl:NPlane) : NPlane =
        let dot = XYZ.dot pl.NormalX pl.NormalY pl.NormalZ (dirPt.X - pl.OriginX) (dirPt.Y - pl.OriginY) (dirPt.Z - pl.OriginZ)
        if dot >= 0.0 then
            NPlane.createUnchecked(pl.OriginX + pl.NormalX*dist, pl.OriginY + pl.NormalY*dist, pl.OriginZ + pl.NormalZ*dist, pl.NormalX, pl.NormalY, pl.NormalZ)
        else
            NPlane.createUnchecked(pl.OriginX - pl.NormalX*dist, pl.OriginY - pl.NormalY*dist, pl.OriginZ - pl.NormalZ*dist, pl.NormalX, pl.NormalY, pl.NormalZ)

    /// Returns absolute distance of point to plane.
    static member inline distToPt (pt:Pnt) (pl:NPlane) : float =
        pl.DistanceToPt pt

    /// Returns signed distance of point to plane, also indicating on which side it is.
    static member inline distToPtSigned (pt:Pnt) (pl:NPlane) : float =
        pl.DistanceToPtSigned pt

    /// Scales the plane's origin by a given factor from the world origin. The normal remains unchanged.
    static member inline scale (factor:float) (pl:NPlane) : NPlane =
        NPlane.createUnchecked(pl.OriginX * factor, pl.OriginY * factor, pl.OriginZ * factor, pl.NormalX, pl.NormalY, pl.NormalZ)

    /// Move plane origin by vector.
    /// This is the same as NPlane.move.
    static member inline translate (translation:Vec) (pl:NPlane)  : NPlane =
        NPlane.createUnchecked(pl.OriginX + translation.X, pl.OriginY + translation.Y, pl.OriginZ + translation.Z, pl.NormalX, pl.NormalY, pl.NormalZ)

    /// Move plane origin by vector.
    /// This is same as NPlane.translate.
    static member inline move (translation:Vec) (pl:NPlane)  : NPlane =
        NPlane.createUnchecked(pl.OriginX + translation.X, pl.OriginY + translation.Y, pl.OriginZ + translation.Z, pl.NormalX, pl.NormalY, pl.NormalZ)

