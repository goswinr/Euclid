namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>]
open FsEx.Geo.Util


#nowarn "44" // for hidden constructors via Obsolete Attribute


/// An immutable plane defined by a point and a normal vector.
/// As opposed to the PPlane this plane is not parametrized in a X, Y and Z direction.
/// Note: Never use the struct default constructor Plane() as it will create an invalid zero Plane.
/// Use Plane.create or Plane.createUnchecked instead.
[<Struct;NoEquality;NoComparison>]// because its made up from floats
[<IsReadOnly>]
type Plane = // Normals are always unitized

    /// The center point of the Plane.
    val Origin : Pnt

    /// The unitized normal of the Plane.
    val Normal : UnitVec

    /// Unsafe internal constructor,  public only for inlining.
    [<Obsolete("Unsafe internal constructor,  but must be public for inlining. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >]
    new (pt,n) = {Origin = pt; Normal = n} // private unchecked constructor, supply unitized values

    /// Format PPlane into string with nicely formatted floating point numbers.
    override pl.ToString() = sprintf "FsEx.Geo.Plane(Origin:%s| Normal:%s)" pl.Origin.AsString pl.Normal.AsString

    /// Returns signed distance of point to plane, also indicating on which side it is.
    member inline pl.DistanceToPt pt = pl.Normal*(pt-pl.Origin)

    /// Returns the closest point on the plane from a test point.
    member inline pl.ClosestPoint pt = pt - pl.Normal*(pl.DistanceToPt pt)

    /// First finds the closet point on plane from a test point.
    /// Then returns a new plane with Origin at this point and the same Normal.
    member inline pl.PlaneAtClPt pt = Plane(pt - pl.Normal*(pl.DistanceToPt pt), pl.Normal)

    /// Returns the Angle to another Plane in Degree, ignoring the normal's orientation.
    /// So between 0 to 90 degrees.
    member inline this.AngleToPlane (pl:Plane) = UnitVec.angle90 this.Normal pl.Normal

    /// Returns the Angle to 3D vector in Degree, ignoring the plane's orientation.
    /// So between 0 to 90 degrees.
    member inline pl.AngleToVec (v:Vec) = UnitVec.angle90 v.Unitized pl.Normal

    /// Returns the Angle to 3D unit-vector in Degree, ignoring the plane's orientation.
    /// So between 0 to 90 degrees.
    member inline pl.AngleToVec (v:UnitVec) = UnitVec.angle90 v pl.Normal

    /// Returns the Angle to a Line3D in Degree, ignoring the normal's orientation.
    /// So between 0 to 90 degrees.
    member inline pl.AngleToLine (ln:Line3D) = UnitVec.angle90 ln.Tangent.Unitized pl.Normal

    /// Checks if two Planes are coincident within the distance tolerance. 1e-6 by default.
    /// This means that their Z-axes are parallel within the angle tolerance
    /// and the distance of second origin to the first plane is less than the distance tolerance.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minium cosine value.
    /// See FsEx.Geo.Cosine module.
    member inline pl.IsCoincidentTo (other:Plane,
                                    [<OPT;DEF(1e-6)>] distanceTolerance:float,
                                    [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine>) =
        pl.Normal.IsParallelTo(other.Normal,minCosine)
        &&
        pl.DistanceToPt other.Origin < distanceTolerance


    //----------------------------------------------------------------------------------------------
    //--------------------------  Static Members  --------------------------------------------------
    //----------------------------------------------------------------------------------------------

    /// Checks if two 3D Planes are equal within tolerance.
    static member equals tol (a:Plane) (b:Plane) =
        let tt = tol*tol
        Pnt.distanceSq a.Origin b.Origin < tt &&
        UnitVec.differenceSq a.Normal b.Normal < tt

    /// Checks if two 3D Parametrized Planes are coincident.
    /// This means that the Z-axes are parallel within 0.25 degrees
    /// and the distance of second origin to the first plane is less than 1e-6 units tolerance.
    static member inline areCoincident tol (a:Plane) (b:Plane) = a.IsCoincidentTo (b)

    /// Create Plane, normal vector gets unitized in constructor.
    static member create(pt,normal:Vec) =
        let l = sqrt(normal.X*normal.X+normal.Y*normal.Y+normal.Z*normal.Z)
        if l < zeroLengthTol then FsExGeoDivByZeroException.Raise "FsEx.Geo.Plane.create: %O is too small for unitizing, tolerance:%g" normal zeroLengthTol
        let li=1./l in
        Plane(pt,UnitVec.createUnchecked( li*normal.X , li*normal.Y ,li*normal.Z ))

    /// Create Plane from already normalized input vector.
    static member inline create(pt,normal:UnitVec) =
        Plane(pt,normal)

    /// Create Plane from 3 points.
    static member inline createFrom3Points (a:Pnt) (b:Pnt) (c:Pnt) =
        let n =  Vec.cross (c-b,a-b)
        if n.LengthSq < 1e-12 then FsExGeoException.Raise "FsEx.Geo.Plane.createFrom3Points: the points %O, %O, %O are (almost) in one Line3D, no Plane found." a b c
        Plane(a, n.Unitized)

    /// Gets the Planes normal. A unitized vector.
    static member inline normal (p:Plane) = p.Normal

    /// Gets the Planes origin.
    static member inline origin (a:Plane) = a.Origin

    /// Gets the Plane at world origin with normal in world Z direction.
    static member inline xyPlane = Plane(Pnt.Origin,UnitVec.Zaxis)

    /// Returns the Angle to another Plane in Degree, ignoring the normal's orientation.
    /// So between 0 to 90 degrees.
    static member inline angleTo (a:Plane) b = a.AngleToPlane b

    /// Returns the line of intersection between two planes.
    /// Fails if they are parallel.
    static member intersect  (a:Plane) (b:Plane) =
        let bn = b.Normal
        let an = a.Normal
        let ao = a.Origin
        let v = UnitVec.cross (an,bn)
        if v.LengthSq < 1e-18 then
            FsExGeoException.Raise "FsEx.Geo.Plane.intersect: Planes are parallel or coincident: %O, %O" a b
        let pa = Vec.cross(v, an)
        let nenner = pa * bn
        let t = ((b.Origin - ao ) * bn) / nenner
        let xpt = ao + pa * t
        Line3D.createFromPntAndVec (xpt, v)

    /// Returns the parameter of intersection point of infinite Line3D with Plane.
    /// Fails if they are parallel.
    static member intersectLineParameter  (ln:Line3D) (pl:Plane) =
        let n = pl.Normal
        let nenner = ln.Tangent * n
        if abs nenner < 1e-9 then
            FsExGeoException.Raise "FsEx.Geo.Plane.intersectLineParameter: Line and Plane are parallel or line has zero length: %O, %O" ln pl
        ((pl.Origin - ln.From) * pl.Normal) / nenner

    /// Returns intersection point of infinite Line3D with Plane.
    /// Fails if they are parallel.
    static member intersectLine (ln:Line3D) (pl:Plane) =
        ln.EvaluateAt <| Plane.intersectLineParameter  ln pl

    /// Checks if a finite Line3D intersects with Plane in one point.
    /// Returns false for parallel and coincident lines.
    static member inline doLinePlaneIntersect (ln:Line3D) (pl:Plane) =
        let n = pl.Normal
        let nenner = ln.Tangent * n
        let t = ((pl.Origin - ln.From) * n)  / nenner // if nenner is 0.0 then 't' is Infinity
        0. <= t && t <= 1.

    /// Returns a new plane offset along the normal vector.
    static member inline offset dist (pl:Plane) =
        Plane(pl.Origin + pl.Normal*dist , pl.Normal)

    /// Offset Plane by amount in orientation towards DirPt:
    /// In direction of point -> distance -> Plane.
    static member inline offsetInDir dirPt dist (pl:Plane) =
        if pl.Normal * (dirPt-pl.Origin) > 0. then Plane(pl.Origin + pl.Normal*dist , pl.Normal)
        else                                       Plane(pl.Origin - pl.Normal*dist , pl.Normal)

    /// Returns signed distance of point to plane, also indicating on which side it is.
    static member inline distToPt (pt:Pnt) (pl:Plane) =
        pl.DistanceToPt pt

    (*
    static member  fitFromConvexPts (pts:seq<Pnt>) =
        let cen = pts |> Seq.average
        let n = pts |> Seq.thisNextLoop |> Seq.map (fun (t,n) -> (t-cen)/*(n-cen)) |> Seq.sum |> Vec.norm
        Plane(cen,n)

    /// * NOT STABLE !! Different seq order different result ???
    static member  fitFromPts (pts:seq<Pnt>) =
        let pts = Array.ofSeq pts
        let cen = pts |> Array.average
        let mutable Normal = Vec()
        for t,n in Array.thisNextLoop pts do
            let v = (t-cen)/*(n-cen)
            if Vec.dirMatch v Normal then Normal <- Normal + v
            else                     Normal <- Normal - v
        Plane.byNormal cen Normal

    static member  arePtsPlanar tol (pts:seq<Pnt>)=
        let pl = pts|> Plane.fitFromPts
        pts|> Seq.exists (fun p -> pl.DistToPt p > tol) |> not

    static member  arePtsInPlane tol (pl:Plane) (pts:seq<Pnt>)=
        pts|> Seq.exists (fun p -> pl.DistToPt p > tol) |> not
    *)

