namespace Euclid

open System
open Euclid.UtilEuclid
open EuclidErrors
open System.ComponentModel

/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type PPlane.
[<AutoOpen; EditorBrowsable(EditorBrowsableState.Never)>]
module AutoOpenPPlane =

    type PPlane with

        /// Returns signed distance of point to plane, also indicating on which side it is.
        member inline pl.DistanceToXYZSigned (x:float, y:float, z:float) : float =
            XYZ.dot pl.ZAxisX pl.ZAxisY pl.ZAxisZ (x - pl.OriginX) (y - pl.OriginY) (z - pl.OriginZ)

        /// Returns signed distance of point to plane, also indicating on which side it is.
        static member inline distanceToXYZSigned (x:float) (y:float) (z:float) (pl:PPlane) : float =
            pl.DistanceToXYZSigned (x, y, z)

        /// Returns signed distance of point to plane, also indicating on which side it is.
        member inline pl.DistanceToPntSigned (pnt:Pnt) : float =
            pl.DistanceToXYZSigned (pnt.X, pnt.Y, pnt.Z)

        /// Returns signed distance of point to plane, also indicating on which side it is.
        static member inline distanceToPntSigned pnt (pl:PPlane) : float =
            pl.DistanceToPntSigned pnt

        /// Returns absolute distance of point to plane.
        member inline pl.DistanceToXYZ (x:float, y:float, z:float) : float =
            abs (pl.DistanceToXYZSigned (x, y, z))

        /// Returns absolute distance of point to plane.
        static member inline distanceToXYZ (x:float) (y:float) (z:float) (pl:PPlane) : float =
            pl.DistanceToXYZ (x, y, z)

        /// Returns absolute distance of point to plane.
        member inline pl.DistanceToPnt (pnt:Pnt) : float =
            pl.DistanceToXYZ (pnt.X, pnt.Y, pnt.Z)

        /// Returns absolute distance of point to plane.
        static member inline distanceToPnt pnt (pl:PPlane) : float =
            pl.DistanceToPnt pnt



        /// Returns the X, Y and Z parameters of a point with regards to the plane.
        member inline pl.PointParameters (pnt:Pnt) : float * float * float =
            let vx = pnt.X - pl.OriginX
            let vy = pnt.Y - pl.OriginY
            let vz = pnt.Z - pl.OriginZ
            XYZ.dot pl.XAxisX pl.XAxisY pl.XAxisZ vx vy vz,
            XYZ.dot pl.YAxisX pl.YAxisY pl.YAxisZ vx vy vz,
            XYZ.dot pl.ZAxisX pl.ZAxisY pl.ZAxisZ vx vy vz

        /// Returns the X, Y and Z parameters of a point with regards to the plane.
        static member inline pointParameters pnt (pl:PPlane) : float * float * float =
            pl.PointParameters pnt

        /// First finds the closest point on plane from a test point.
        /// Then returns a new plane with Origin at this point and the same Axes vectors.
        member inline pl.PlaneAtClPnt (pnt:Pnt) : PPlane =
            let d = pl.DistanceToPntSigned pnt
            PPlane.createUnchecked(
                pnt.X - pl.ZAxisX*d, pnt.Y - pl.ZAxisY*d, pnt.Z - pl.ZAxisZ*d,
                pl.XAxisX, pl.XAxisY, pl.XAxisZ,
                pl.YAxisX, pl.YAxisY, pl.YAxisZ,
                pl.ZAxisX, pl.ZAxisY, pl.ZAxisZ)

        /// First finds the closest point on plane from a test point.
        /// Then returns a new plane with Origin at this point and the same Axes vectors.
        static member inline planeAtClPnt pnt (pl:PPlane) : PPlane =
            pl.PlaneAtClPnt pnt

        /// Returns the angle to another Plane in degrees, ignoring the normal's orientation.
        /// So 0.0 if the planes are parallel. And 90 degrees if the planes are perpendicular to each other.
        member inline this.Angle90ToPlane (pl:PPlane) : float =
            XYZ.angleHalfPiOfUnit this.ZAxisX this.ZAxisY this.ZAxisZ pl.ZAxisX pl.ZAxisY pl.ZAxisZ |> toDegrees

        /// Returns the angle to another Plane in degrees, ignoring the normal's orientation.
        /// So 0.0 if the planes are parallel. And 90 degrees if the planes are perpendicular to each other.
        static member inline angle90ToPlane (other:PPlane) (pl:PPlane) : float =
            pl.Angle90ToPlane other

        /// Returns the angle to 3D vector in degrees, ignoring the plane's orientation.
        /// So 0.0 if the vector is parallel to the Plane. And 90 degrees if the vector is perpendicular to the plane.
        member inline pl.Angle90ToVec (v:Vec) : float =
            let x = v.X
            let y = v.Y
            let z = v.Z
            let l = sqrt (x*x + y*y + z*z)
            if isTooTiny l then failUnit3 "PPlane.Angle90ToVec" x y z
            let f = 1. / l
            90.0 - (XYZ.angleHalfPiOfUnit (f*x) (f*y) (f*z) pl.ZAxisX pl.ZAxisY pl.ZAxisZ |> toDegrees)

        /// Returns the angle to 3D unit-vector in degrees, ignoring the plane's orientation.
        /// So 0.0 if the vector is parallel to the Plane. And 90 degrees if the vector is perpendicular to the plane.
        member inline pl.Angle90ToVec (v:UnitVec) : float =
            90.0 - (XYZ.angleHalfPiOfUnit v.X v.Y v.Z pl.ZAxisX pl.ZAxisY pl.ZAxisZ |> toDegrees)

        /// Returns the angle to 3D vector in degrees, ignoring the plane's orientation.
        /// So 0.0 if the vector is parallel to the Plane. And 90 degrees if the vector is perpendicular to the plane.
        static member inline angle90ToVec (v:Vec) (pl:PPlane) : float =
            pl.Angle90ToVec v

        /// Returns the angle to 3D unit-vector in degrees, ignoring the plane's orientation.
        /// So 0.0 if the vector is parallel to the Plane. And 90 degrees if the vector is perpendicular to the plane.
        static member inline angle90ToUnitVec (v:UnitVec) (pl:PPlane) : float =
            pl.Angle90ToVec v

        /// Returns the angle to a Line3D in degrees, ignoring the ZAxis's orientation.
        /// So 0.0 if the line is parallel to the Plane. And 90 degrees if the line is perpendicular to the plane.
        member inline pl.Angle90ToLine (ln:Line3D) : float =
            let x = ln.VectorX
            let y = ln.VectorY
            let z = ln.VectorZ
            let l = sqrt(x * x  + y * y + z * z)
            if isTooTiny l then
                failTooSmall "PPlane.Angle90ToLine" ln
            90.0 - (XYZ.angleHalfPiOfUnit (x/ l) (y/ l) (z/ l) pl.ZAxisX pl.ZAxisY pl.ZAxisZ |> toDegrees)

        /// Returns the angle to a Line3D in degrees, ignoring the ZAxis's orientation.
        /// So 0.0 if the line is parallel to the Plane. And 90 degrees if the line is perpendicular to the plane.
        static member inline angle90ToLine (ln:Line3D) (pl:PPlane) : float =
            pl.Angle90ToLine ln

        /// Evaluate at 3D parameter.
        member inline p.EvaluateAtXYZ (px:float, py:float, pz:float) : Pnt =
            Pnt(p.OriginX + p.XAxisX*px + p.YAxisX*py + p.ZAxisX*pz,
                p.OriginY + p.XAxisY*px + p.YAxisY*py + p.ZAxisY*pz,
                p.OriginZ + p.XAxisZ*px + p.YAxisZ*py + p.ZAxisZ*pz)

        /// Evaluate at 3D parameter.
        static member inline evaluateAtXYZ (px:float) (py:float) (pz:float) (pl:PPlane) : Pnt =
            pl.EvaluateAtXYZ(px, py, pz)

        /// Evaluate at 2D parameter (Z parameter = 0.0)
        member inline p.EvaluateAt (px:float, py:float) : Pnt =
            Pnt(p.OriginX + p.XAxisX*px + p.YAxisX*py,
                p.OriginY + p.XAxisY*px + p.YAxisY*py,
                p.OriginZ + p.XAxisZ*px + p.YAxisZ*py)

        /// Evaluate at 2D parameter (Z parameter = 0.0)
        static member inline evaluateAt (px:float) (py:float) (pl:PPlane) : Pnt =
            pl.EvaluateAt(px, py)

        /// Checks if two PPlanes are coincident within the distance tolerance. 1e-6 by default.
        /// This means that their Z-axes are parallel within the angle tolerance
        /// and the distance of second origin to the first plane is less than the distance tolerance.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        member inline pl.IsCoincidentTo (other:PPlane,
                                        [<OPT;DEF(1e-6)>] distanceTolerance:float,
                                        [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine>) : bool =
            // the Z-axes are parallel:
            abs (XYZ.dot other.ZAxisX other.ZAxisY other.ZAxisZ pl.ZAxisX pl.ZAxisY pl.ZAxisZ) > float minCosine
            &&
            pl.DistanceToXYZ (other.OriginX, other.OriginY, other.OriginZ) < distanceTolerance

        /// Checks if two PPlanes are coincident within the given tolerances.
        /// This means that their Z-axes are parallel within the angle tolerance
        /// and the distance of second origin to the first plane is less than the distance tolerance.
        /// Use a precomputed value from Euclid.Cosine module as tolerance.
        static member inline isCoincidentTo distanceTolerance minCosine (a:PPlane) (b:PPlane) : bool =
            a.IsCoincidentTo(b, distanceTolerance, minCosine)


        /// Returns the X and Y parameters of the closest point on the PPlane to a given point.
        member pl.ClosestParameters (p:Pnt) : float*float =
            let vx = p.X - pl.OriginX
            let vy = p.Y - pl.OriginY
            let vz = p.Z - pl.OriginZ
            let xParam = vx*pl.XAxisX + vy*pl.XAxisY + vz*pl.XAxisZ
            let yParam = vx*pl.YAxisX + vy*pl.YAxisY + vz*pl.YAxisZ
            xParam, yParam

        /// Returns the X and Y parameters of the closest point on the PPlane to a given point.
        static member inline closestParameters (p:Pnt) (pl:PPlane) : float*float =
            pl.ClosestParameters(p)


        /// Returns the closest point on the plane from a test point.
        member inline pl.ClosestPoint (pnt:Pnt) : Pnt =
            let d = pl.DistanceToPntSigned pnt
            Pnt(pnt.X - pl.ZAxisX*d, pnt.Y - pl.ZAxisY*d, pnt.Z - pl.ZAxisZ*d)

        /// Returns the closest point on the plane from a test point.
        static member inline closestPoint pnt (pl:PPlane) : Pnt =
            pl.ClosestPoint pnt

    // #endregion
    // #region Predefined Planes

        /// Checks if two Parametrized Planes are NOT equal within tolerance.
        /// The tolerance applies to each coordinate of the origin and of the X-axis and Y-axis separately, not to the Euclidean distance between them.
        static member inline notEqualsWithin (tol:float) (a:PPlane) (b:PPlane)  : bool =
            abs (a.OriginX - b.OriginX) > tol ||
            abs (a.OriginY - b.OriginY) > tol ||
            abs (a.OriginZ - b.OriginZ) > tol ||
            abs (a.XAxisX  - b.XAxisX ) > tol ||
            abs (a.XAxisY  - b.XAxisY ) > tol ||
            abs (a.XAxisZ  - b.XAxisZ ) > tol ||
            abs (a.YAxisX  - b.YAxisX ) > tol ||
            abs (a.YAxisY  - b.YAxisY ) > tol ||
            abs (a.YAxisZ  - b.YAxisZ ) > tol

        /// Returns the World Coordinate System Plane at World Origin.
        /// X-axis = World X-axis
        /// Y-axis = World Y-axis
        /// Z-axis = World Z-axis
        /// same as PPlane.WorldTop
        static member inline WorldXY : PPlane =
            PPlane.createUnchecked(0., 0., 0.,  1., 0., 0.,  0., 1., 0.,  0., 0., 1.)

        /// Returns the World Coordinate System Plane at World Origin.
        /// X-axis = World X-axis
        /// Y-axis = World Y-axis
        /// Z-axis = World Z-axis
        /// same as PPlane.WorldXY
        static member inline WorldTop : PPlane =
            PPlane.createUnchecked(0., 0., 0.,  1., 0., 0.,  0., 1., 0.,  0., 0., 1.)

        /// Returns the Coordinate System Plane of a Front view.
        /// X-axis = World X-axis
        /// Y-axis = World Z-axis
        /// Z-axis = minus World Y-axis
        static member inline WorldFront : PPlane =
            PPlane.createUnchecked(0., 0., 0.,  1., 0., 0.,  0., 0., 1.,  0., -1., 0.)

        /// Returns the Coordinate System Plane of a Right view.
        /// X-axis = World Y-axis
        /// Y-axis = World Z-axis
        /// Z-axis = World X-axis
        static member inline WorldRight : PPlane =
            PPlane.createUnchecked(0., 0., 0.,  0., 1., 0.,  0., 0., 1.,  1., 0., 0.)

        /// Returns the Coordinate System Plane of a Left view.
        /// X-axis = minus World Y-axis
        /// Y-axis = World Z-axis
        /// Z-axis = minus World X-axis
        static member inline WorldLeft : PPlane =
            PPlane.createUnchecked(0., 0., 0.,  0., -1., 0.,  0., 0., 1.,  -1., 0., 0.)

        /// Returns the Coordinate System Plane of a Back view.
        /// X-axis = minus World X-axis
        /// Y-axis = World Z-axis
        /// Z-axis = World Y-axis
        static member inline WorldBack : PPlane =
            PPlane.createUnchecked(0., 0., 0.,  -1., 0., 0.,  0., 0., 1.,  0., 1., 0.)

        /// Returns the Coordinate System Plane of a Bottom view.
        /// X-axis = World X-axis
        /// Y-axis = minus World Y-axis
        /// Z-axis = minus World Z-axis
        static member inline WorldBottom : PPlane =
            PPlane.createUnchecked(0., 0., 0.,  1., 0., 0.,  0., -1., 0.,  0., 0., -1.)

        /// WorldXY rotated 180 degrees round Z-axis.
        /// X-axis = minus World X-axis
        /// Y-axis = minus World Y-axis
        /// Z-axis = World Z-axis
        static member inline WorldMinusXMinusY : PPlane =
            PPlane.createUnchecked(0., 0., 0.,  -1., 0., 0.,  0., -1., 0.,  0., 0., 1.)

        /// WorldXY rotated 90 degrees round Z-axis counter-clockwise from top.
        /// X-axis = World Y-axis
        /// Y-axis = minus World X-axis
        /// Z-axis = World Z-axis
        static member inline WorldYMinusX : PPlane =
            PPlane.createUnchecked(0., 0., 0.,  0., 1., 0.,  -1., 0., 0.,  0., 0., 1.)

        /// WorldXY rotated 270 degrees round Z-axis counter-clockwise from top.
        /// X-axis = minus World Y-axis
        /// Y-axis = World X-axis
        /// Z-axis = World Z-axis
        static member inline WorldMinusYX : PPlane =
            PPlane.createUnchecked(0., 0., 0.,  0., -1., 0.,  1., 0., 0.,  0., 0., 1.)

        /// WorldXY rotated 180 degrees round X-axis, Z points down now.
        /// X-axis = World X-axis
        /// Y-axis = minus World Y-axis
        /// Z-axis = minus World Z-axis
        static member inline WorldXMinusY : PPlane =
            PPlane.createUnchecked(0., 0., 0.,  1., 0., 0.,  0., -1., 0.,  0., 0., -1.)


        // #endregion
        // #region create

        /// Builds Plane at first point, X-axis to second point,
        /// Y-axis to third point or at least in plane with third point.
        /// Fails if points are closer than 1e-6.
        static member createFrom3Pnts (origin:Pnt) (xPnt:Pnt) (yPnt:Pnt) : PPlane =
            let x = xPnt-origin
            let y = yPnt-origin
            let lx = x.Length
            let ly = y.Length
            if isTooSmall lx then
                failTooClose "PPlane.createFrom3Pnts xPnt" origin xPnt
            if isTooSmall ly then
                failTooClose "PPlane.createFrom3Pnts yPnt" origin yPnt
            let xf = 1./lx
            let yf = 1./ly
            let xu = UnitVec.createUnchecked(x.X*xf, x.Y*xf, x.Z*xf)
            let yu = UnitVec.createUnchecked(y.X*yf, y.Y*yf, y.Z*yf)
            if xu.IsParallelTo(yu, Cosine.``1.0``) then
                failCollinear "PPlane.createFrom3Pnts" origin xPnt yPnt
            let z = UnitVec.cross (xu, yu)
            let y' = Vec.cross (z, x)
            PPlane.createFromAxesUnchecked(origin, xu, y'.Unitized, z.Unitized)

        /// Creates a Parametrized Plane from a unit-vector representing the X-axis, a unit-vector as Y-axis hint and an origin point.
        /// The resulting PPlane will have the X-axis in direction of X vector.
        /// The X and Y vectors will define the plane and the side that Z will be on.
        /// The given Y vector does not need to be perpendicular to the X vector, just not parallel.
        /// Fails if the vectors are parallel within 1 degree.
        static member createFromUnitXAxisAndYAxis (xAxis:UnitVec) (yAxis:UnitVec) (origin:Pnt) : PPlane =
            if xAxis.IsParallelTo(yAxis, Cosine.``1.0``) then
                failCollinear "PPlane.createFromUnitXAxisAndYAxis" origin xAxis yAxis
            let z = UnitVec.cross (xAxis, yAxis)
            let y = Vec.cross (z, xAxis)
            PPlane.createFromAxesUnchecked(origin, xAxis, y.Unitized, z.Unitized)

        /// Creates a Parametrized Plane from a vector representing the X-axis, a vector as Y-axis hint and an origin point.
        /// The resulting PPlane will have the X-axis in direction of X vector.
        /// The X and Y vectors will define the plane and the side that Z will be on.
        /// The given Y vector does not need to be perpendicular to the X vector, just not parallel.
        /// Fails if the vectors are shorter than 1e-6.
        static member createFromXAxisAndYAxis (xAxis:Vec) (yAxis:Vec) (origin:Pnt) : PPlane =
            let lx = xAxis.Length
            let ly = yAxis.Length
            if isTooSmall lx then
                failTooSmall2 "PPlane.createFromXAxisAndYAxis xAxis" xAxis yAxis
            if isTooSmall ly then
                failTooSmall2 "PPlane.createFromXAxisAndYAxis yAxis" yAxis xAxis
            let xf = 1./lx
            let yf = 1./ly
            let xu = UnitVec.createUnchecked(xAxis.X*xf, xAxis.Y*xf, xAxis.Z*xf)
            let yu = UnitVec.createUnchecked(yAxis.X*yf, yAxis.Y*yf, yAxis.Z*yf)
            PPlane.createFromUnitXAxisAndYAxis xu yu origin

        /// Creates a Parametrized Plane from a unit-vector representing the normal (or Z-axis) and an origin point.
        /// The X-axis will be found by taking the Cross Product of the World Z-axis and the given normal (or Z-axis).
        /// This will make the X-axis horizontal.
        /// If this fails because they are coincident, the Cross Product of the World X-axis and the given normal (or Z-axis) will be used.
        static member createFromUnitNormal (normal:UnitVec) (origin:Pnt) : PPlane =
            // 0.5 degrees is a deliberately wider guard than the 0.25 degree parallel-tolerance used
            // elsewhere: it falls back early, before the Cross Product with the World Z-axis becomes
            // numerically unstable for near-vertical normals.
            if normal.IsParallelTo(UnitVec.zAxis, Cosine.``0.5``) then
                let y = Vec.cross (normal,Vec.xAxis)
                let x = Vec.cross (y, normal)
                PPlane.createFromAxesUnchecked(origin, x.Unitized, y.Unitized, normal)
            else
                let x = Vec.cross (Vec.zAxis, normal)
                let y = Vec.cross (normal, x)
                PPlane.createFromAxesUnchecked(origin, x.Unitized, y.Unitized, normal)

        /// Creates a Parametrized Plane from a vector representing the normal (or Z-axis) and an origin point.
        /// The X-axis will be found by taking the Cross Product of the World Z-axis and the given normal (or Z-axis).
        /// This will make the X-axis horizontal.
        /// If this fails because they are coincident, the Cross Product of the World X-axis and the given normal (or Z-axis) will be used.
        /// Fails if the vectors are shorter than 1e-6.
        static member createFromNormal (normal:Vec) (origin:Pnt) : PPlane =
            let len = normal.Length
            if isTooSmall len then
                failTooSmall "PPlane.createFromNormal" normal
            let f = 1./len
            let nu = UnitVec.createUnchecked(normal.X*f, normal.Y*f, normal.Z*f)
            PPlane.createFromUnitNormal nu origin

        /// Creates a Parametrized Plane from a unit-vector representing the Z-axis, a unit-vector as X-axis hint and an origin point.
        /// The given X vector does not need to be perpendicular to the normal vector, just not parallel.
        /// Fails if normal and xAxis are parallel within 1 degree.
        static member createFromUnitXAxisAndNormal (normal:UnitVec) (xAxis:UnitVec) (origin:Pnt) : PPlane =
            if normal.IsParallelTo(xAxis, Cosine.``1.0``) then
                failCollinear "PPlane.createFromUnitXAxisAndNormal" origin normal xAxis
            let y = UnitVec.cross (normal, xAxis)
            let x = Vec.cross (y, normal)
            PPlane.createFromAxesUnchecked(origin, x.Unitized, y.Unitized, normal)

        /// Creates a Parametrized Plane from a vector representing the Z-axis, a vector as X-axis hint and an origin point.
        /// The given X vector does not need to be perpendicular to the normal vector, just not parallel.
        /// Fails if the vectors are shorter than 1e-6 or normal and X are parallel.
        static member createFromXAxisAndNormal (normal:Vec) (xAxis:Vec) (origin:Pnt) : PPlane =
            let lx = xAxis.Length
            let ln = normal.Length
            if isTooSmall lx then
                failTooSmall2 "PPlane.createFromXAxisAndNormal xAxis" xAxis normal
            if isTooSmall ln then
                failTooSmall2 "PPlane.createFromXAxisAndNormal normal" normal xAxis
            let xf = 1./lx
            let nf = 1./ln
            let xu = UnitVec.createUnchecked(xAxis.X *xf,  xAxis.Y*xf,  xAxis.Z*xf)
            let nu = UnitVec.createUnchecked(normal.X*nf, normal.Y*nf, normal.Z*nf)
            PPlane.createFromUnitXAxisAndNormal nu xu origin



    // #endregion
    // #region Static members

        /// Checks if two Parametrized Planes are equal within tolerance.
        /// The tolerance applies to each coordinate of the origin and of the X-axis and Y-axis separately, not to the Euclidean distance between them.
        /// Use PPlane.equalsExact to check for an exact match.
        static member inline equalsWithin (tol:float) (a:PPlane) (b:PPlane)  : bool =
            abs (a.OriginX - b.OriginX) <= tol &&
            abs (a.OriginY - b.OriginY) <= tol &&
            abs (a.OriginZ - b.OriginZ) <= tol &&
            abs (a.XAxisX  - b.XAxisX ) <= tol &&
            abs (a.XAxisY  - b.XAxisY ) <= tol &&
            abs (a.XAxisZ  - b.XAxisZ ) <= tol &&
            abs (a.YAxisX  - b.YAxisX ) <= tol &&
            abs (a.YAxisY  - b.YAxisY ) <= tol &&
            abs (a.YAxisZ  - b.YAxisZ ) <= tol //&&
            //abs (a.ZAxisX - b.ZAxisX) <= tol &&
            //abs (a.ZAxisY - b.ZAxisY) <= tol &&
            //abs (a.ZAxisZ - b.ZAxisZ) <= tol

        /// Checks if two Parametrized Planes are exactly equal.
        /// Compares each field with the = operator, so a NaN value is never equal.
        static member inline equalsExact (a:PPlane) (b:PPlane) : bool =
            a.OriginX = b.OriginX &&
            a.OriginY = b.OriginY &&
            a.OriginZ = b.OriginZ &&
            a.XAxisX = b.XAxisX &&
            a.XAxisY = b.XAxisY &&
            a.XAxisZ = b.XAxisZ &&
            a.YAxisX = b.YAxisX &&
            a.YAxisY = b.YAxisY &&
            a.YAxisZ = b.YAxisZ

        /// Returns a new plane with given Origin. The axes stay the same.
        static member inline withOrigin (pnt:Pnt) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pnt.X, pnt.Y, pnt.Z, pl.XAxisX, pl.XAxisY, pl.XAxisZ, pl.YAxisX, pl.YAxisY, pl.YAxisZ, pl.ZAxisX, pl.ZAxisY, pl.ZAxisZ)

        /// Returns a new plane with given Origin X value changed. The axes stay the same.
        static member inline withOriginX (x:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(x, pl.OriginY, pl.OriginZ, pl.XAxisX, pl.XAxisY, pl.XAxisZ, pl.YAxisX, pl.YAxisY, pl.YAxisZ, pl.ZAxisX, pl.ZAxisY, pl.ZAxisZ)

        /// Returns a new plane with given Origin Y value changed. The axes stay the same.
        static member inline withOriginY (y:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX, y, pl.OriginZ, pl.XAxisX, pl.XAxisY, pl.XAxisZ, pl.YAxisX, pl.YAxisY, pl.YAxisZ, pl.ZAxisX, pl.ZAxisY, pl.ZAxisZ)

        /// Returns a new plane with given Origin Z value changed. The axes stay the same.
        static member inline withOriginZ (z:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX, pl.OriginY, z, pl.XAxisX, pl.XAxisY, pl.XAxisZ, pl.YAxisX, pl.YAxisY, pl.YAxisZ, pl.ZAxisX, pl.ZAxisY, pl.ZAxisZ)

        /// Returns a new plane with Origin translated by Vec.
        static member inline translateBy (v:Vec) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX + v.X, pl.OriginY + v.Y, pl.OriginZ + v.Z, pl.XAxisX, pl.XAxisY, pl.XAxisZ, pl.YAxisX, pl.YAxisY, pl.YAxisZ, pl.ZAxisX, pl.ZAxisY, pl.ZAxisZ)

        /// Returns a new plane with Origin translated in World X direction.
        static member inline translateByWorldX (x:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX + x, pl.OriginY, pl.OriginZ, pl.XAxisX, pl.XAxisY, pl.XAxisZ, pl.YAxisX, pl.YAxisY, pl.YAxisZ, pl.ZAxisX, pl.ZAxisY, pl.ZAxisZ)

        /// Returns a new plane with Origin translated in World Y direction.
        static member inline translateByWorldY (y:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX, pl.OriginY + y, pl.OriginZ, pl.XAxisX, pl.XAxisY, pl.XAxisZ, pl.YAxisX, pl.YAxisY, pl.YAxisZ, pl.ZAxisX, pl.ZAxisY, pl.ZAxisZ)

        /// Returns a new plane with Origin translated in World Z direction.
        static member inline translateByWorldZ (z:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX, pl.OriginY, pl.OriginZ + z, pl.XAxisX, pl.XAxisY, pl.XAxisZ, pl.YAxisX, pl.YAxisY, pl.YAxisZ, pl.ZAxisX, pl.ZAxisY, pl.ZAxisZ)

        /// Rotate about Z-axis of the Plane by an angle in degrees.
        /// Positive angles rotate counter-clockwise in top view (for the WorldXY plane).
        static member inline rotateZ (angDegree:float) (pl:PPlane) : PPlane =
            // The rotational 3x3 part of RigidMatrix.createRotationAxis (pl.ZAxis, angDegree),
            // which is also the 3x3 part of a rotation about the Z-axis at the origin of the plane.
            // Only this part is applied to the local axis directions.
            let angle = UtilEuclid.toRadians angDegree
            let c = cos angle
            let s = sin angle
            let t = 1.0 - c
            let x = pl.ZAxisX
            let y = pl.ZAxisY
            let z = pl.ZAxisZ
            let tx = t * x
            let ty = t * y
            let m11 = tx * x + c
            let m21 = tx * y - s * z
            let m31 = tx * z + s * y
            let m12 = tx * y + s * z
            let m22 = ty * y + c
            let m32 = ty * z - s * x
            let m13 = tx * z - s * y
            let m23 = ty * z + s * x
            let m33 = t  * z * z + c
            let xx = m11*pl.XAxisX + m21*pl.XAxisY + m31*pl.XAxisZ
            let xy = m12*pl.XAxisX + m22*pl.XAxisY + m32*pl.XAxisZ
            let xz = m13*pl.XAxisX + m23*pl.XAxisY + m33*pl.XAxisZ
            let yx = m11*pl.YAxisX + m21*pl.YAxisY + m31*pl.YAxisZ
            let yy = m12*pl.YAxisX + m22*pl.YAxisY + m32*pl.YAxisZ
            let yz = m13*pl.YAxisX + m23*pl.YAxisY + m33*pl.YAxisZ
            PPlane.createUnchecked(pl.OriginX, pl.OriginY, pl.OriginZ, xx, xy, xz, yx, yy, yz, pl.ZAxisX, pl.ZAxisY, pl.ZAxisZ)

        /// Move Plane along the local X-axis by the given distance.
        static member inline moveAlongLocalX (d:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX + pl.XAxisX*d, pl.OriginY + pl.XAxisY*d, pl.OriginZ + pl.XAxisZ*d, pl.XAxisX, pl.XAxisY, pl.XAxisZ, pl.YAxisX, pl.YAxisY, pl.YAxisZ, pl.ZAxisX, pl.ZAxisY, pl.ZAxisZ)

        /// Move Plane along the local Y-axis by the given distance.
        static member inline moveAlongLocalY (d:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX + pl.YAxisX*d, pl.OriginY + pl.YAxisY*d, pl.OriginZ + pl.YAxisZ*d, pl.XAxisX, pl.XAxisY, pl.XAxisZ, pl.YAxisX, pl.YAxisY, pl.YAxisZ, pl.ZAxisX, pl.ZAxisY, pl.ZAxisZ)

        /// Move Plane along the local Z-axis by the given distance.
        /// Same as PPlane.offset.
        static member inline moveAlongLocalZ (d:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX + pl.ZAxisX*d, pl.OriginY + pl.ZAxisY*d, pl.OriginZ + pl.ZAxisZ*d, pl.XAxisX, pl.XAxisY, pl.XAxisZ, pl.YAxisX, pl.YAxisY, pl.YAxisZ, pl.ZAxisX, pl.ZAxisY, pl.ZAxisZ)

        /// Move Plane along the local Z-axis by the given distance.
        /// Same as PPlane.moveAlongLocalZ.
        static member inline offset (d:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX + pl.ZAxisX*d, pl.OriginY + pl.ZAxisY*d, pl.OriginZ + pl.ZAxisZ*d, pl.XAxisX, pl.XAxisY, pl.XAxisZ, pl.YAxisX, pl.YAxisY, pl.YAxisZ, pl.ZAxisX, pl.ZAxisY, pl.ZAxisZ)

        /// Rotate the Plane 180 degrees on its Y-axis.
        /// Called flip because Z-axis points in the opposite direction.
        static member inline flipOnY (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX, pl.OriginY, pl.OriginZ, -pl.XAxisX, -pl.XAxisY, -pl.XAxisZ, pl.YAxisX, pl.YAxisY, pl.YAxisZ, -pl.ZAxisX, -pl.ZAxisY, -pl.ZAxisZ)

        /// Rotate the Plane 180 degrees on its X-axis.
        /// Called flip because Z-axis points in the opposite direction.
        static member inline flipOnX (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX, pl.OriginY, pl.OriginZ, pl.XAxisX, pl.XAxisY, pl.XAxisZ, -pl.YAxisX, -pl.YAxisY, -pl.YAxisZ, -pl.ZAxisX, -pl.ZAxisY, -pl.ZAxisZ)

        /// Rotate the Plane 180 degrees on its Z-axis.
        static member inline rotateOnZ180 (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX, pl.OriginY, pl.OriginZ, -pl.XAxisX, -pl.XAxisY, -pl.XAxisZ, -pl.YAxisX, -pl.YAxisY, -pl.YAxisZ, pl.ZAxisX, pl.ZAxisY, pl.ZAxisZ)

        /// Transforms the plane by the given RigidMatrix.
        /// The returned PPlane has orthogonal unit-vectors.
        static member transformRigid (m:RigidMatrix) (pl:PPlane) : PPlane =
            let ox = m.M11*pl.OriginX + m.M21*pl.OriginY + m.M31*pl.OriginZ + m.X41
            let oy = m.M12*pl.OriginX + m.M22*pl.OriginY + m.M32*pl.OriginZ + m.Y42
            let oz = m.M13*pl.OriginX + m.M23*pl.OriginY + m.M33*pl.OriginZ + m.Z43
            let xx = m.M11*pl.XAxisX + m.M21*pl.XAxisY + m.M31*pl.XAxisZ
            let xy = m.M12*pl.XAxisX + m.M22*pl.XAxisY + m.M32*pl.XAxisZ
            let xz = m.M13*pl.XAxisX + m.M23*pl.XAxisY + m.M33*pl.XAxisZ
            let yx = m.M11*pl.YAxisX + m.M21*pl.YAxisY + m.M31*pl.YAxisZ
            let yy = m.M12*pl.YAxisX + m.M22*pl.YAxisY + m.M32*pl.YAxisZ
            let yz = m.M13*pl.YAxisX + m.M23*pl.YAxisY + m.M33*pl.YAxisZ
            let zx = m.M11*pl.ZAxisX + m.M21*pl.ZAxisY + m.M31*pl.ZAxisZ
            let zy = m.M12*pl.ZAxisX + m.M22*pl.ZAxisY + m.M32*pl.ZAxisZ
            let zz = m.M13*pl.ZAxisX + m.M23*pl.ZAxisY + m.M33*pl.ZAxisZ
            PPlane.createUnchecked(ox, oy, oz, xx, xy, xz, yx, yy, yz, zx, zy, zz)

        /// Rotate Plane 180 degrees around Z-axis if the Y-axis orientation does not match World Y (pl.Yax.Y < 0.0)
        /// To ensure that Y is always positive. For example for showing Text.
        static member inline rotateZ180IfYNegative (pl:PPlane) : PPlane =
            if pl.YAxisY < 0.0 then PPlane.rotateOnZ180 pl else pl



        /// Returns the line of intersection between two planes.
        /// Returns ValueNone if they are parallel or coincident.
        static member tryIntersect (a:PPlane) (b:PPlane) : Line3D voption=
            // cross product of the planes' normals gives the direction of the line of intersection.
            let vx = a.ZAxisY * b.ZAxisZ - a.ZAxisZ * b.ZAxisY
            let vy = a.ZAxisZ * b.ZAxisX - a.ZAxisX * b.ZAxisZ
            let vz = a.ZAxisX * b.ZAxisY - a.ZAxisY * b.ZAxisX
            if isTooSmallSq (XYZ.lengthSq vx vy vz) then
                // EuclidException.Raise "Euclid.PPlane.intersect: Planes are parallel or coincident: %O, %O" a b
                ValueNone
            else
                // cross product of the direction of the line of intersection with plane A ZAxis
                let px = vy * a.ZAxisZ - vz * a.ZAxisY
                let py = vz * a.ZAxisX - vx * a.ZAxisZ
                let pz = vx * a.ZAxisY - vy * a.ZAxisX
                let nenner = XYZ.dot px py pz b.ZAxisX b.ZAxisY b.ZAxisZ
                let dot = XYZ.dot (b.OriginX - a.OriginX) (b.OriginY - a.OriginY) (b.OriginZ - a.OriginZ) b.ZAxisX b.ZAxisY b.ZAxisZ
                let t = dot / nenner
                let xpx = a.OriginX + px * t
                let xpy = a.OriginY + py * t
                let xpz = a.OriginZ + pz * t
                ValueSome (Line3D(xpx,      xpy,      xpz,
                                  xpx + vx, xpy + vy, xpz + vz)
                             )

        /// Returns the line of intersection between two planes.
        /// Fails if they are parallel or coincident.
        /// Use PPlane.tryIntersect to get ValueNone instead.
        static member intersect (a:PPlane) (b:PPlane) : Line3D =
            match PPlane.tryIntersect a b with
            | ValueSome ln -> ln
            | ValueNone -> fail $"PPlane.intersect: the planes are parallel or coincident:{nl}{a}{nl}{b}"

        /// Returns the parameter on the line.
        /// The parameter is the intersection point of the infinite ray with the PPlane.
        /// Returns ValueNone if they are parallel or coincident.
        static member tryIntersectRay (ln:Line3D) (pl:PPlane) : float voption =
            let nenner = XYZ.dot ln.VectorX ln.VectorY ln.VectorZ pl.ZAxisX pl.ZAxisY pl.ZAxisZ
            if isTooSmall (abs nenner) then
                ValueNone
            else
                let dot = XYZ.dot (pl.OriginX - ln.FromX) (pl.OriginY - ln.FromY) (pl.OriginZ - ln.FromZ) pl.ZAxisX pl.ZAxisY pl.ZAxisZ
                ValueSome (dot / nenner)

        /// Returns the parameter on the line.
        /// The parameter is the intersection point of the infinite ray with the PPlane.
        /// Fails if they are parallel or coincident, or if the line is too short.
        /// Use PPlane.tryIntersectRay to get ValueNone instead.
        static member intersectRay (ln:Line3D) (pl:PPlane) : float =
            match PPlane.tryIntersectRay ln pl with
            | ValueSome t -> t
            | ValueNone -> fail $"PPlane.intersectRay: the line is parallel to the plane or too short:{nl}{ln}{nl}{pl}"


        /// Returns the line parameter and the X and Y parameters on the Plane. as tuple (pLn, pPlX, pPlY).
        /// The parameters is the intersection point of the infinite ray with the PPlane.
        /// Returns ValueNone if they are parallel or coincident.
        static member tryIntersectRayAllParameters (ln:Line3D) (pl:PPlane) : voption<float*float*float> =
            let vx = ln.VectorX
            let vy = ln.VectorY
            let vz = ln.VectorZ
            let nenner = XYZ.dot vx vy vz pl.ZAxisX pl.ZAxisY pl.ZAxisZ
            if isTooSmall (abs nenner) then
                ValueNone
            else
                let dot = XYZ.dot (pl.OriginX - ln.FromX) (pl.OriginY - ln.FromY) (pl.OriginZ - ln.FromZ) pl.ZAxisX pl.ZAxisY pl.ZAxisZ
                let t = dot / nenner
                // The intersection point on the line:
                let wx = ln.FromX + vx * t - pl.OriginX
                let wy = ln.FromY + vy * t - pl.OriginY
                let wz = ln.FromZ + vz * t - pl.OriginZ
                // get closest parameters on the plane:
                let dotX = XYZ.dot pl.XAxisX pl.XAxisY pl.XAxisZ wx wy wz
                let dotY = XYZ.dot pl.YAxisX pl.YAxisY pl.YAxisZ wx wy wz
                ValueSome (t, dotX, dotY)

        /// Returns the line parameter and the X and Y parameters on the Plane. as tuple (pLn, pPlX, pPlY).
        /// The parameters is the intersection point of the infinite ray with the PPlane.
        /// Fails if they are parallel or coincident, or if the line is too short.
        /// Use PPlane.tryIntersectRayAllParameters to get ValueNone instead.
        static member intersectRayAllParameters (ln:Line3D) (pl:PPlane) : float*float*float =
            match PPlane.tryIntersectRayAllParameters ln pl with
            | ValueSome ps -> ps
            | ValueNone -> fail $"PPlane.intersectRayAllParameters: the line is parallel to the plane or too short:{nl}{ln}{nl}{pl}"

        /// Returns intersection point of a finite line with the Plane.
        /// Returns ValueNone if they are parallel or the domain of intersection is outside 0.0 to 1.0.
        /// Intersection just below 0.0 or just above 1.0 within tolerance of 1e-6 are clamped to 0.0 or 1.0.
        /// Returns ValueNone if the line is too short.
        static member tryIntersectLine (ln:Line3D) (pl:PPlane) : Pnt voption =
            match PPlane.tryIntersectRay ln pl with
            | ValueSome (t) ->
                if isBetweenZeroAndOneTolerantIncl t then
                    let c = clamp01 t
                    ValueSome (Pnt(ln.FromX + ln.VectorX * c, ln.FromY + ln.VectorY * c, ln.FromZ + ln.VectorZ * c))
                else
                    ValueNone
            | ValueNone ->
                ValueNone

        /// Returns intersection point of a finite line with the Plane.
        /// Intersection just below 0.0 or just above 1.0 within tolerance of 1e-6 are clamped to 0.0 or 1.0.
        /// Fails if they are parallel, if the line is too short, or if the domain of intersection is outside 0.0 to 1.0.
        /// Use PPlane.tryIntersectLine to get ValueNone instead.
        static member intersectLine (ln:Line3D) (pl:PPlane) : Pnt =
            match PPlane.tryIntersectLine ln pl with
            | ValueSome pnt -> pnt
            | ValueNone -> fail $"PPlane.intersectLine: the line does not intersect the plane, it is parallel, too short or does not reach it:{nl}{ln}{nl}{pl}"

        /// Checks if a finite Line3D intersects with Plane in one point.
        /// Returns FALSE for NaN values or (almost) parallel or coincident lines.
        static member inline doLinePlaneIntersect (ln:Line3D) (pl:PPlane) : bool =
            let nenner = XYZ.dot ln.VectorX ln.VectorY ln.VectorZ pl.ZAxisX pl.ZAxisY pl.ZAxisZ
            if isTooSmall (abs nenner) then
                false
            else
                let dot = XYZ.dot (pl.OriginX - ln.FromX) (pl.OriginY - ln.FromY) (pl.OriginZ - ln.FromZ) pl.ZAxisX pl.ZAxisY pl.ZAxisZ
                let t = dot / nenner // if nenner is 0.0 then 't' is Infinity
                0. <= t && t <= 1.

        /// Scales the PPlane's origin position by a given factor from the world origin (0,0,0).
        /// The axes directions remain unchanged.
        static member inline scale (factor:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX * factor, pl.OriginY * factor, pl.OriginZ * factor, pl.XAxisX, pl.XAxisY, pl.XAxisZ, pl.YAxisX, pl.YAxisY, pl.YAxisZ, pl.ZAxisX, pl.ZAxisY, pl.ZAxisZ)

        /// Move plane origin by vector in world space.By.
        static member inline move (translation:Vec) (pl:PPlane)  : PPlane =
            PPlane.createUnchecked(pl.OriginX + translation.X, pl.OriginY + translation.Y, pl.OriginZ + translation.Z, pl.XAxisX, pl.XAxisY, pl.XAxisZ, pl.YAxisX, pl.YAxisY, pl.YAxisZ, pl.ZAxisX, pl.ZAxisY, pl.ZAxisZ)
