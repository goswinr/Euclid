namespace FsEx.Geo

open System
open FsEx.Geo.Util

#nowarn "44" // for hidden constructors via Obsolete Attribute


//TODO finish docstrings

/// A plane defined by a point and a normal vector
[<Struct;NoEquality;NoComparison>]// because its made up from floats
type Plane = // Normals are always unitized
    /// The center point of the Plane
    val Origin : Pnt
    /// The unitized normal of the Plane
    val Normal : UnitVec

    /// Unsafe internal constructor,  public only for inlining.
    [<Obsolete("Unsafe internal constructor,  but must be public for inlining. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >] 
    new (pt,n) = {Origin = pt; Normal = n} // private unchecked constructor, supply unitized values
    
    /// Format PPlane into string with nicely formatted floating point numbers.
    override pl.ToString() = sprintf "FsEx.Geo.Plane(Origin:%s| Normal:%s)" pl.Origin.AsString pl.Normal.AsString

    /// Create Plane, normal vector gets unitized in constructor
    static member create(pt,normal:Vec) = 
        let l = sqrt(normal.X*normal.X+normal.Y*normal.Y+normal.Z*normal.Z) 
        if l < zeroLengthTol then FsExGeoDivByZeroException.Raise "FsEx.Geo.Plane.create: %O is too small for unitizing, tolerance:%g" normal zeroLengthTol
        let li=1./l in 
        Plane(pt,UnitVec.createUnchecked( li*normal.X , li*normal.Y ,li*normal.Z ))

    /// normal vector gets unitized in constructor
    static member create(pt,normal:UnitVec) =        
        Plane(pt,normal)


    static member createFrom3Points (a:Pnt) (b:Pnt) (c:Pnt) =
        let n =  Vec.cross (c-b,a-b)
        if Vec.isTiny 1e-5 n then FsExGeoException.Raise "FsEx.Geo.Plane.createFrom3Points: the points %O, %O, %O are in one Line3D, no Plane found" a b c
        Plane(a, n.Unitized)

    /// Returns signed distance of point to plane, also indicating on which side it is.
    member inline pl.DistToPt pt = pl.Normal*(pt-pl.Origin) 

    /// Returns the closest point on the plane from a test point.
    member inline pl.ClPt pt = pt - pl.Normal*(pl.DistToPt pt) 

    /// Returns the closest point on the plane from a test point.
    member pl.PlaneAtClPt pt = Plane(pt - pl.Normal*(pl.DistToPt pt), pl.Normal)

    /// Returns the Angle to another Plane in Degree, ignoring orientation.
    member inline this.AngToPl (pl:Plane) = UnitVec.angle90 this.Normal pl.Normal 

    /// Returns the Angle to a Line3D in Degree, ignoring orientation.
    member inline pl.AngleToLine (ln:Line3D) = UnitVec.angle90 ln.Tangent.Unitized pl.Normal 
    

    //----------------------------------------------------------------------------------------------
    //--------------------------  Static Members  --------------------------------------------------
    //----------------------------------------------------------------------------------------------



    /// Checks if two 3D  Planes are equal within tolerance.
    static member equals tol (a:Plane) (b:Plane) =
        let tt = tol*tol
        Pnt.distanceSq a.Origin b.Origin < tt &&
        UnitVec.differenceSq a.Normal b.Normal < tt 


    static member  normal (p:Plane) = p.Normal
    
    static member  pt (a:Plane) = a.Origin
    
    static member  xyPlane = Plane(Pnt.Origin,UnitVec.ZAxis)
    
    static member  angle (a:Plane) b = a.AngToPl b

    /// Returns the parameter of intersection point of infinite Line3D with Plane, fails if they are parallel
    static member  intersectLineParameter  (ln:Line3D) (pl:Plane) = 
        let nenner = ln.Tangent * pl.Normal
        if abs nenner < 1e-6 then FsExGeoException.Raise "FsEx.Geo.intersectLineParameter: Lines and Plane are Parallel: %O, %O" ln pl
        (pl.Origin - ln.From) * pl.Normal / nenner
    
    /// Returns intersection point of infinite Line3D with Plane, fails if they are parallel
    static member intersectLine (ln:Line3D) (pl:Plane) = ln.EvaluateAt <| Plane.intersectLineParameter  ln pl
    
    /// checks if a finite Line3D intersects with Plane, fails if they are parallel
    static member  inline doLinePlaneIntersect (ln:Line3D) (pl:Plane) = 
        let t = Plane.intersectLineParameter ln pl
        0. <= t && t <= 1.
    
    static member  offset dist (pl:Plane) = Plane(pl.Origin + pl.Normal*dist , pl.Normal)        
    
    /// Offset Plane by amount  in orientation towards DirPt:
    /// in direction of point -> distance -> Plane
    static member  offsetInDir dirPt dist (pl:Plane) =
        if pl.Normal * (dirPt-pl.Origin) > 0. then Plane(pl.Origin + pl.Normal*dist , pl.Normal)
        else                              Plane(pl.Origin - pl.Normal*dist , pl.Normal)
    
    static member  distToPt (pt:Pnt) (pl:Plane) = pl.DistToPt pt
    
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


    

  
