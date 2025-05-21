namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>]
open Euclid.UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar


#nowarn "44" // for hidden constructors via Obsolete Attribute


/// An immutable un-parametrized plane defined by a point and a normal vector.
/// As opposed to the PPlane this plane is not parametrized in a X, Y and Z direction.
/// Note: Never use the struct default constructor NPlane() as it will create an invalid zero Plane.
/// Use NPlane.create or Plane.createUnchecked instead.
[<Struct;NoEquality;NoComparison>]// because its made up from floats
[<IsReadOnly>]
[<DataContract>] // for using DataMember on fields
type NPlane = // NPlane to avoid a name clash with Rhino Plane

    //[<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar

    /// The center point of the Plane.
    [<DataMember>] val Origin : Pnt

    /// The unitized normal of the Plane.
    [<DataMember>] val Normal : UnitVec

    new (pt:Pnt, n:UnitVec) = {Origin = pt; Normal = n}

    /// Format PPlane into string with nicely formatted floating point numbers.
    override pl.ToString() =
        sprintf "Euclid.NPlane(Origin:%s| Normal:%s)" pl.Origin.AsString pl.Normal.AsString

    /// Returns a new Plane with the same Origin but flipped Normal.
    member inline pl.Flipped  =
        NPlane(pl.Origin, -pl.Normal)

    /// Returns signed distance of point to plane, also indicating on which side it is.
    member inline pl.DistanceToPt pt =
        pl.Normal *** (pt-pl.Origin)

    /// Returns the closest point on the plane from a test point.
    member inline pl.ClosestPoint pt =
        pt - pl.Normal * (pl.DistanceToPt pt)

    /// First finds the closet point on plane from a test point.
    /// Then returns a new plane with Origin at this point and the same Normal.
    member inline pl.PlaneAtClPt pt =
        NPlane(pt - pl.Normal * (pl.DistanceToPt pt), pl.Normal)

    /// Returns the angle to another Plane in Degree, ignoring the normal's orientation.
    /// So 0.0 if the planes are parallel. And 90 degrees if the planes are perpendicular to ech other.
    member inline this.Angle90ToPlane (pl:NPlane) =
        UnitVec.angle90 this.Normal pl.Normal

    /// Returns the angle to 3D vector in Degree, ignoring the plane's orientation.
    /// So 0.0 if the vector is parallele to the Plane. And 90 degrees if the vector is perpendicular to the plane.
    member inline pl.Angle90ToVec (v:Vec) =
        90.0 - UnitVec.angle90 v.Unitized pl.Normal

    /// Returns the angle to 3D unit-vector in Degree, ignoring the plane's orientation.
    /// So 0.0 if the vector is parallele to the Plane. And 90 degrees if the vector is perpendicular to the plane.
    member inline pl.Angle90ToVec (v:UnitVec) =
        90.0 - UnitVec.angle90 v pl.Normal

    /// Returns the angle to a Line3D in Degree, ignoring the normal's orientation.
    /// So 0.0 if the line is parallele to the Plane. And 90 degrees if the line is perpendicular to the plane.
    member inline pl.Angle90ToLine (ln:Line3D) =
        90.0 - UnitVec.angle90 ln.UnitTangent pl.Normal

    /// Checks if two Planes are coincident within the distance tolerance. 1e-6 by default.
    /// This means that their Z-axes are parallel within the angle tolerance
    /// and the distance of second origin to the first plane is less than the distance tolerance.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minium cosine value.
    /// See Euclid.Cosine module.
    member inline pl.IsCoincidentTo (other:NPlane,
                                    [<OPT;DEF(1e-6)>] distanceTolerance:float,
                                    [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine>) : bool  =
        pl.Normal.IsParallelTo(other.Normal, minCosine)
        &&
        pl.DistanceToPt other.Origin < distanceTolerance

    //----------------------------------------------------------------------------------------------
    //--------------------------  Static Members  --------------------------------------------------
    //----------------------------------------------------------------------------------------------

    /// Checks if two 3D Planes are equal within tolerance.
    /// The same tolerance is used for the origin and the tips of the normal.
    /// Use a tolerance of 0.0 to check for an exact match.
    static member equals (tol:float) (a:NPlane) (b:NPlane) =
        abs (a.Origin.X - b.Origin.X) <= tol &&
        abs (a.Origin.Y - b.Origin.Y) <= tol &&
        abs (a.Origin.Z - b.Origin.Z) <= tol &&
        abs (a.Normal.X - b.Normal.X) <= tol &&
        abs (a.Normal.Y - b.Normal.Y) <= tol &&
        abs (a.Normal.Z - b.Normal.Z) <= tol


    /// Checks if two 3D Planes are coincident.
    /// This means that the Z-axes are parallel within 0.25 degrees
    /// and the distance of second origin to the first plane is less than 1e-6 units tolerance.
    static member inline areCoincident (a:NPlane) (b:NPlane) : bool =
        a.IsCoincidentTo (b)

    /// Create Plane, normal vector gets unitized in constructor.
    static member create(pt, normal:Vec) =
        let l = sqrt(normal.X*normal.X + normal.Y*normal.Y + normal.Z*normal.Z)
        if isTooTiny l then EuclidException.Raisef "Euclid.Plane.create: %O is too small for unitizing, tolerance:%g" normal zeroLengthTolerance
        let li = 1. / l
        NPlane(pt, UnitVec.createUnchecked(li*normal.X, li*normal.Y, li*normal.Z))

    /// Create Plane from already normalized input vector.
    static member inline create(pt, normal:UnitVec) =
        NPlane(pt, normal)

    /// Create Plane from 3 points.
    static member inline createFrom3Points (a:Pnt) (b:Pnt) (c:Pnt) =
        let n = Vec.cross (c-b, a-b)
        if isTooSmallSq n.LengthSq then EuclidException.Raisef "Euclid.Plane.createFrom3Points: the points %O, %O, %O are (almost) in one line or one Point, no Plane found." a b c
        NPlane(a, n.Unitized)

    /// Gets the Planes normal. A unitized vector.
    static member inline normal (p:NPlane) =
        p.Normal

    /// Gets the Planes origin.
    static member inline origin (a:NPlane) =
        a.Origin

    /// Gets the Plane at world origin with normal in world Z direction.
    static member inline xyPlane =
        NPlane(Pnt.Origin, UnitVec.Zaxis)

    /// Returns the angle to another Plane in Degree, ignoring the normal's orientation.
    /// So between 0 to 90 degrees.
    static member inline angleTo (a:NPlane) b =
        a.Angle90ToPlane b

    /// Returns the line of intersection between two planes.
    /// Or None if they are parallel.
    static member intersect  (a:NPlane) (b:NPlane) =
        let bn = b.Normal
        let an = a.Normal
        let ao = a.Origin
        let v = UnitVec.cross (an, bn)
        if isTooSmallSq v.LengthSq then
            //EuclidException.Raise "Euclid.Plane.intersect: Planes are parallel or coincident: %O, %O" a b
            None
        else
            let pa = Vec.cross(v, an)
            let nenner = pa *** bn
            let t = ((b.Origin - ao) *** bn) / nenner
            let xpt = ao + pa * t
            Some <| Line3D.createFromPntAndVec (xpt, v)

    /// Returns the parameter of intersection point of infinite Line3D with Plane.
    /// Or None if they are parallel.
    static member intersectLineParameter  (ln:Line3D) (pl:NPlane) =
        let n = pl.Normal
        let nenner = ln.Tangent *** n
        if isTooSmall(abs nenner) then
            // EuclidException.Raise "Euclid.Plane.intersectLineParameter: Line and Plane are parallel or line has zero length: %O, %O" ln pl
            None
        else
            Some <| ((pl.Origin - ln.From) *** pl.Normal) / nenner

    /// Returns intersection point of infinite Line3D with Plane.
    /// Or None if they are parallel.
    static member intersectLine (ln:Line3D) (pl:NPlane) =
        match NPlane.intersectLineParameter ln pl with
        | None   -> None
        | Some t -> Some <| ln.EvaluateAt t


    /// Checks if a finite Line3D intersects with Plane in one point.
    /// Returns false for parallel and coincident lines.
    static member inline doLinePlaneIntersect (ln:Line3D) (pl:NPlane) =
        let n = pl.Normal
        let nenner = ln.Tangent *** n
        let t = ((pl.Origin - ln.From) *** n)  / nenner // if nenner is 0.0 then 't' is Infinity
        0. <= t && t <= 1.

    /// Returns a new plane offset along the normal vector.
    static member inline offset dist (pl:NPlane) =
        NPlane(pl.Origin + pl.Normal*dist, pl.Normal)

    /// Offset Plane by amount in orientation towards DirPt:
    /// In direction of point -> distance -> Plane.
    static member inline offsetInDir dirPt dist (pl:NPlane) =
        if pl.Normal * (dirPt-pl.Origin) > 0. then NPlane(pl.Origin + pl.Normal*dist, pl.Normal)
        else                                       NPlane(pl.Origin - pl.Normal*dist, pl.Normal)

    /// Returns signed distance of point to plane, also indicating on which side it is.
    static member inline distToPt (pt:Pnt) (pl:NPlane) =
        pl.DistanceToPt pt


    /// Scales the plane's origin by a given factor from the world origin. The normal remains unchanged.
    static member inline scale (factor:float) (pl:NPlane) : NPlane =
        NPlane(pl.Origin * factor, pl.Normal)

    /// Move plane origin by vector.
    /// This is the same as NPlane.move.
    static member inline translate (translation:Vec) (pl:NPlane) =
        NPlane(pl.Origin + translation, pl.Normal)

    /// Move plane origin by vector.
    /// This is same as NPlane.translate.
    static member inline move (translation:Vec) (pl:NPlane) =
        NPlane(pl.Origin + translation, pl.Normal)

    (*
    static member fitFromConvexPts (pts:seq<Pnt>) =
        let cen = pts |> Seq.average
        let n = pts |> Seq.thisNextLoop |> Seq.map (fun (t, n) -> (t-cen)/*(n-cen)) |> Seq.sum |> Vec.norm
        Plane(cen, n)

    /// * NOT STABLE !! Different seq order different result ???
    static member fitFromPts (pts:seq<Pnt>) =
        let pts = Array.ofSeq pts
        let cen = pts |> Array.average
        let mutable Normal = Vec()
        for t, n in Array.thisNextLoop pts do
            let v = (t-cen)/*(n-cen)
            if Vec.dirMatch v Normal then Normal <- Normal + v
            else                     Normal <- Normal - v
        Plane.byNormal cen Normal

    static member arePtsPlanar tol (pts:seq<Pnt>)=
        let pl = pts|> Plane.fitFromPts
        pts|> Seq.exists (fun p -> pl.DistToPt p > tol) |> not

    static member arePtsInPlane tol (pl:Plane) (pts:seq<Pnt>)=
        pts|> Seq.exists (fun p -> pl.DistToPt p > tol) |> not
    *)

