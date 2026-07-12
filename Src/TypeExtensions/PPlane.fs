namespace Euclid

open System
open Euclid.UtilEuclid
open EuclidErrors

/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type PPlane.
[<AutoOpen>]
module AutoOpenPPlane =

    type PPlane with

        /// Returns signed distance of point to plane, also indicating on which side it is.
        member inline pl.DistanceToXYZSigned (x:float, y:float, z:float) : float =
            XYZ.dot pl.ZaxisX pl.ZaxisY pl.ZaxisZ (x - pl.OriginX) (y - pl.OriginY) (z - pl.OriginZ)

        /// Returns signed distance of point to plane, also indicating on which side it is.
        static member inline distanceToXYZSigned (x:float) (y:float) (z:float) (pl:PPlane) : float =
            pl.DistanceToXYZSigned (x, y, z)

        /// Returns signed distance of point to plane, also indicating on which side it is.
        member inline pl.DistanceToPntSigned (pt:Pnt) : float =
            pl.DistanceToXYZSigned (pt.X, pt.Y, pt.Z)

        /// Returns signed distance of point to plane, also indicating on which side it is.
        static member inline distanceToPntSigned pt (pl:PPlane) : float =
            pl.DistanceToPntSigned pt

        /// Returns absolute distance of point to plane.
        member inline pl.DistanceToXYZ (x:float, y:float, z:float) : float =
            abs (pl.DistanceToXYZSigned (x, y, z))

        /// Returns absolute distance of point to plane.
        static member inline distanceToXYZ (x:float) (y:float) (z:float) (pl:PPlane) : float =
            pl.DistanceToXYZ (x, y, z)

        /// Returns absolute distance of point to plane.
        member inline pl.DistanceToPnt (pt:Pnt) : float =
            pl.DistanceToXYZ (pt.X, pt.Y, pt.Z)

        /// Returns absolute distance of point to plane.
        static member inline distanceToPnt pt (pl:PPlane) : float =
            pl.DistanceToPnt pt



        /// Returns the X, Y and Z parameters of a point with regards to the plane.
        member inline pl.PointParameters (pt:Pnt) : float * float * float =
            let vx = pt.X - pl.OriginX
            let vy = pt.Y - pl.OriginY
            let vz = pt.Z - pl.OriginZ
            XYZ.dot pl.XaxisX pl.XaxisY pl.XaxisZ vx vy vz,
            XYZ.dot pl.YaxisX pl.YaxisY pl.YaxisZ vx vy vz,
            XYZ.dot pl.ZaxisX pl.ZaxisY pl.ZaxisZ vx vy vz

        /// Returns the X, Y and Z parameters of a point with regards to the plane.
        static member inline pointParameters pt (pl:PPlane) : float * float * float =
            pl.PointParameters pt

        /// First finds the closest point on plane from a test point.
        /// Then returns a new plane with Origin at this point and the same Axes vectors.
        member inline pl.PlaneAtClPnt (pt:Pnt) : PPlane =
            let d = pl.DistanceToPntSigned pt
            PPlane.createUnchecked(
                pt.X - pl.ZaxisX*d, pt.Y - pl.ZaxisY*d, pt.Z - pl.ZaxisZ*d,
                pl.XaxisX, pl.XaxisY, pl.XaxisZ,
                pl.YaxisX, pl.YaxisY, pl.YaxisZ,
                pl.ZaxisX, pl.ZaxisY, pl.ZaxisZ)

        /// First finds the closest point on plane from a test point.
        /// Then returns a new plane with Origin at this point and the same Axes vectors.
        static member inline planeAtClPnt pt (pl:PPlane) : PPlane =
            pl.PlaneAtClPnt pt

        /// Returns the angle to another Plane in degrees, ignoring the normal's orientation.
        /// So 0.0 if the planes are parallel. And 90 degrees if the planes are perpendicular to each other.
        member inline this.Angle90ToPlane (pl:PPlane) : float =
            UnitVec.angle90 this.Zaxis pl.Zaxis

        /// Returns the angle to another Plane in degrees, ignoring the normal's orientation.
        /// So 0.0 if the planes are parallel. And 90 degrees if the planes are perpendicular to each other.
        static member inline angle90ToPlane (other:PPlane) (pl:PPlane) : float =
            pl.Angle90ToPlane other

        /// Returns the angle to 3D vector in degrees, ignoring the plane's orientation.
        /// So 0.0 if the vector is parallel to the Plane. And 90 degrees if the vector is perpendicular to the plane.
        member inline pl.Angle90ToVec (v:Vec) : float =
            90.0 - UnitVec.angle90 v.Unitized pl.Zaxis

        /// Returns the angle to 3D unit-vector in degrees, ignoring the plane's orientation.
        /// So 0.0 if the vector is parallel to the Plane. And 90 degrees if the vector is perpendicular to the plane.
        member inline pl.Angle90ToVec (v:UnitVec) : float =
            90.0 - UnitVec.angle90 v pl.Zaxis

        /// Returns the angle to 3D vector in degrees, ignoring the plane's orientation.
        /// So 0.0 if the vector is parallel to the Plane. And 90 degrees if the vector is perpendicular to the plane.
        static member inline angle90ToVec (v:Vec) (pl:PPlane) : float =
            pl.Angle90ToVec v

        /// Returns the angle to 3D unit-vector in degrees, ignoring the plane's orientation.
        /// So 0.0 if the vector is parallel to the Plane. And 90 degrees if the vector is perpendicular to the plane.
        static member inline angle90ToUnitVec (v:UnitVec) (pl:PPlane) : float =
            pl.Angle90ToVec v

        /// Returns the angle to a Line3D in degrees, ignoring the Zaxis's orientation.
        /// So 0.0 if the line is parallel to the Plane. And 90 degrees if the line is perpendicular to the plane.
        member inline pl.Angle90ToLine (ln:Line3D) : float =
            let x = ln.VectorX
            let y = ln.VectorY
            let z = ln.VectorZ
            let l = sqrt(x * x  + y * y + z * z)
            if isTooTiny l then
                failTooSmall "PPlane.Angle90ToLine" ln
            let u = UnitVec.createUnchecked (x/ l, y/ l, z/ l)
            90.0 - UnitVec.angle90 u pl.Zaxis

        /// Returns the angle to a Line3D in degrees, ignoring the Zaxis's orientation.
        /// So 0.0 if the line is parallel to the Plane. And 90 degrees if the line is perpendicular to the plane.
        static member inline angle90ToLine (ln:Line3D) (pl:PPlane) : float =
            pl.Angle90ToLine ln

        /// Evaluate at 3D parameter.
        member inline p.EvaluateAtXYZ (px:float, py:float, pz:float) : Pnt =
            Pnt(p.OriginX + p.XaxisX*px + p.YaxisX*py + p.ZaxisX*pz,
                p.OriginY + p.XaxisY*px + p.YaxisY*py + p.ZaxisY*pz,
                p.OriginZ + p.XaxisZ*px + p.YaxisZ*py + p.ZaxisZ*pz)

        /// Evaluate at 3D parameter.
        static member inline evaluateAtXYZ (px:float, py:float, pz:float) (pl:PPlane) : Pnt =
            pl.EvaluateAtXYZ(px, py, pz)

        /// Evaluate at 2D parameter (Z parameter = 0.0)
        member inline p.EvaluateAt (px:float, py:float) : Pnt =
            Pnt(p.OriginX + p.XaxisX*px + p.YaxisX*py,
                p.OriginY + p.XaxisY*px + p.YaxisY*py,
                p.OriginZ + p.XaxisZ*px + p.YaxisZ*py)

        /// Evaluate at 2D parameter (Z parameter = 0.0)
        static member inline evaluateAt (px:float, py:float) (pl:PPlane) : Pnt =
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
            pl.Zaxis.IsParallelTo(other.Zaxis, minCosine)
            &&
            pl.DistanceToPnt other.Origin < distanceTolerance

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
            let xParam = vx*pl.XaxisX + vy*pl.XaxisY + vz*pl.XaxisZ
            let yParam = vx*pl.YaxisX + vy*pl.YaxisY + vz*pl.YaxisZ
            xParam, yParam

        /// Returns the X and Y parameters of the closest point on the PPlane to a given point.
        static member inline closestParameters (p:Pnt) (pl:PPlane) : float*float =
            pl.ClosestParameters(p)


        /// Returns the closest point on the plane from a test point.
        member inline pl.ClosestPoint (pt:Pnt) : Pnt =
            let d = pl.DistanceToPntSigned pt
            Pnt(pt.X - pl.ZaxisX*d, pt.Y - pl.ZaxisY*d, pt.Z - pl.ZaxisZ*d)

        /// Returns the closest point on the plane from a test point.
        static member inline closestPoint pt (pl:PPlane) : Pnt =
            pl.ClosestPoint pt

    // #endregion
    // #region Predefined Planes

        /// Checks if two Parametrized Planes are NOT equal within tolerance distance.
        /// For the tips of its unit vectors and its origin.
        /// Use a tolerance of 0.0 to check for an exact mismatch.
        static member inline notEquals (tol:float) (a:PPlane) (b:PPlane)  : bool =
            abs (a.OriginX - b.OriginX) > tol ||
            abs (a.OriginY - b.OriginY) > tol ||
            abs (a.OriginZ - b.OriginZ) > tol ||
            abs (a.XaxisX  - b.XaxisX ) > tol ||
            abs (a.XaxisY  - b.XaxisY ) > tol ||
            abs (a.XaxisZ  - b.XaxisZ ) > tol ||
            abs (a.YaxisX  - b.YaxisX ) > tol ||
            abs (a.YaxisY  - b.YaxisY ) > tol ||
            abs (a.YaxisZ  - b.YaxisZ ) > tol

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

        /// WorldXY rotated 90 degrees round Z-axis Counter-Clockwise from top.
        /// X-axis = World Y-axis
        /// Y-axis = minus World X-axis
        /// Z-axis = World Z-axis
        static member inline WorldYMinusX : PPlane =
            PPlane.createUnchecked(0., 0., 0.,  0., 1., 0.,  -1., 0., 0.,  0., 0., 1.)

        /// WorldXY rotated 270 degrees round Z-axis Counter-Clockwise from top.
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
        static member createThreePoints (origin:Pnt) (xPt:Pnt) (yPt:Pnt) : PPlane =
            let x = xPt-origin
            let y = yPt-origin
            let lx = x.Length
            let ly = y.Length
            if isTooSmall lx then
                failTooClose "PPlane.createThreePoints xPt" origin xPt
            if isTooSmall ly then
                failTooClose "PPlane.createThreePoints yPt" origin yPt
            let xf = 1./lx
            let yf = 1./ly
            let xu = UnitVec.createUnchecked(x.X*xf, x.Y*xf, x.Z*xf)
            let yu = UnitVec.createUnchecked(y.X*yf, y.Y*yf, y.Z*yf)
            if xu.IsParallelTo(yu, Cosine.``1.0``) then
                failCollinear "PPlane.createThreePoints" origin xPt yPt
            let z = UnitVec.cross (xu, yu)
            let y' = Vec.cross (z, x)
            PPlane.createUncheckedVec(origin, xu, y'.Unitized, z.Unitized)

        /// Creates a Parametrized Plane from a point and a unit-vector representing the X-axis and a Y-axis hint.
        /// The resulting PPlane will have the X-Axis in direction of X vector.
        /// The X and Y vectors will define the plane and the side that Z will be on.
        /// The given Y vector does not need to be perpendicular to the X vector, just not parallel.
        /// Fails if the vectors are shorter than 1e-6.
        static member createOriginXaxisYaxis (origin:Pnt, xAxis:UnitVec, yAxis:UnitVec) : PPlane =
            if xAxis.IsParallelTo(yAxis, Cosine.``1.0``) then
                failCollinear "PPlane.createOriginXaxisYaxis" origin xAxis yAxis
            let z = UnitVec.cross (xAxis, yAxis)
            let y = Vec.cross (z, xAxis)
            PPlane.createUncheckedVec(origin, xAxis, y.Unitized, z.Unitized)

        /// Creates a Parametrized Plane from a point and vector representing the X-axis and a Y-axis hint.
        /// The resulting PPlane will have the X-Axis in direction of X vector.
        /// The X and Y vectors will define the plane and the side that Z will be on.
        /// The given Y vector does not need to be perpendicular to the X vector, just not parallel.
        /// Fails if the vectors are shorter than 1e-6.
        static member createOriginXaxisYaxis (origin:Pnt, xAxis:Vec, yAxis:Vec) : PPlane =
            let lx = xAxis.Length
            let ly = yAxis.Length
            if isTooSmall lx then
                failTooSmall2 "PPlane.createOriginXaxisYaxis xAxis" xAxis yAxis
            if isTooSmall ly then
                failTooSmall2 "PPlane.createOriginXaxisYaxis yAxis" yAxis xAxis
            let xf = 1./lx
            let yf = 1./ly
            let xu = UnitVec.createUnchecked(xAxis.X*xf, xAxis.Y*xf, xAxis.Z*xf)
            let yu = UnitVec.createUnchecked(yAxis.X*yf, yAxis.Y*yf, yAxis.Z*yf)
            PPlane.createOriginXaxisYaxis (origin, xu, yu)

        /// Creates a Parametrized Plane from a point and unit-vector representing the normal (or Z-axis).
        /// The X-axis will be found by taking the Cross Product of the World Z-axis and the given normal (or Z-axis).
        /// This will make the X-axis horizontal.
        /// If this fails because they are coincident, the Cross Product of the World X-axis and the given normal (or Z-axis) will be used.
        static member createOriginNormal (origin:Pnt, normal:UnitVec) : PPlane =
            // 0.5 degrees is a deliberately wider guard than the 0.25 degree parallel-tolerance used
            // elsewhere: it falls back early, before the Cross Product with the World Z-axis becomes
            // numerically unstable for near-vertical normals.
            if normal.IsParallelTo(UnitVec.Zaxis, Cosine.``0.5``) then
                let y = Vec.cross (normal,Vec.Xaxis)
                let x = Vec.cross (y, normal)
                PPlane.createUncheckedVec(origin, x.Unitized, y.Unitized, normal)
            else
                let x = Vec.cross (Vec.Zaxis, normal)
                let y = Vec.cross (normal, x)
                PPlane.createUncheckedVec(origin, x.Unitized, y.Unitized, normal)

        /// Creates a Parametrized Plane from a point and vector representing the normal (or Z-axis).
        /// The X-axis will be found by taking the Cross Product of the World Z-axis and the given normal (or Z-axis).
        /// This will make the X-axis horizontal.
        /// If this fails because they are coincident, the Cross Product of the World X-axis and the given normal (or Z-axis) will be used.
        /// Fails if the vectors are shorter than 1e-6.
        static member createOriginNormal (origin:Pnt, normal:Vec) : PPlane =
            let len = normal.Length
            if isTooSmall len then
                failTooSmall "PPlane.createOriginNormal" normal
            let f = 1./len
            let nu = UnitVec.createUnchecked(normal.X*f, normal.Y*f, normal.Z*f)
            PPlane.createOriginNormal (origin, nu)

        /// Creates a Parametrized Plane from a point and unit-vector representing the Z-axis.
        /// The given X vector does not need to be perpendicular to the normal vector, just not parallel.
        /// Fails if the vectors are shorter than 1e-6 or normal and xAxis are parallel within 1 degree.
        static member createOriginNormalXaxis (origin:Pnt, normal:UnitVec, xAxis:UnitVec) : PPlane =
            if normal.IsParallelTo(xAxis, Cosine.``1.0``) then
                failCollinear "PPlane.createOriginNormalXaxis" origin normal xAxis
            let y = UnitVec.cross (normal, xAxis)
            let x = Vec.cross (y, normal)
            PPlane.createUncheckedVec(origin, x.Unitized, y.Unitized, normal)

        /// Creates a Parametrized Plane from a point and unit-vector representing the Z-axis.
        /// The given X vector does not need to be perpendicular to the normal vector, just not parallel.
        /// Fails if the vectors are shorter than 1e-6 or normal and X are parallel.
        static member createOriginNormalXaxis (origin:Pnt, normal:Vec, xAxis:Vec) : PPlane =
            let lx = xAxis.Length
            let ln = normal.Length
            if isTooSmall lx then
                failTooSmall2 "PPlane.createOriginNormalXaxis xAxis" xAxis normal
            if isTooSmall ln then
                failTooSmall2 "PPlane.createOriginNormalXaxis normal" normal xAxis
            let xf = 1./lx
            let nf = 1./ln
            let xu = UnitVec.createUnchecked(xAxis.X *xf,  xAxis.Y*xf,  xAxis.Z*xf)
            let nu = UnitVec.createUnchecked(normal.X*nf, normal.Y*nf, normal.Z*nf)
            PPlane.createOriginNormalXaxis (origin, nu, xu)



    // #endregion
    // #region Static members

        /// Checks if two Parametrized Planes are equal within tolerance distance.
        /// For the tips of its unit vectors and its origin.
        /// Use a tolerance of 0.0 to check for an exact match.
        static member inline equals (tol:float) (a:PPlane) (b:PPlane)  : bool =
            abs (a.OriginX - b.OriginX) <= tol &&
            abs (a.OriginY - b.OriginY) <= tol &&
            abs (a.OriginZ - b.OriginZ) <= tol &&
            abs (a.XaxisX  - b.XaxisX ) <= tol &&
            abs (a.XaxisY  - b.XaxisY ) <= tol &&
            abs (a.XaxisZ  - b.XaxisZ ) <= tol &&
            abs (a.YaxisX  - b.YaxisX ) <= tol &&
            abs (a.YaxisY  - b.YaxisY ) <= tol &&
            abs (a.YaxisZ  - b.YaxisZ ) <= tol //&&
            //abs (a.ZaxisX - b.ZaxisX) <= tol &&
            //abs (a.ZaxisY - b.ZaxisY) <= tol &&
            //abs (a.ZaxisZ - b.ZaxisZ) <= tol

        /// Returns a new plane with given Origin.
        static member inline setOrigin (pt:Pnt) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pt.X, pt.Y, pt.Z, pl.XaxisX, pl.XaxisY, pl.XaxisZ, pl.YaxisX, pl.YaxisY, pl.YaxisZ, pl.ZaxisX, pl.ZaxisY, pl.ZaxisZ)

        /// Returns a new plane with given Origin X value changed.
        static member inline setOriginX (x:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(x, pl.OriginY, pl.OriginZ, pl.XaxisX, pl.XaxisY, pl.XaxisZ, pl.YaxisX, pl.YaxisY, pl.YaxisZ, pl.ZaxisX, pl.ZaxisY, pl.ZaxisZ)

        /// Returns a new plane with given Origin Y value changed.
        static member inline setOriginY (y:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX, y, pl.OriginZ, pl.XaxisX, pl.XaxisY, pl.XaxisZ, pl.YaxisX, pl.YaxisY, pl.YaxisZ, pl.ZaxisX, pl.ZaxisY, pl.ZaxisZ)

        /// Returns a new plane with given Origin Z value changed.
        static member inline setOriginZ (z:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX, pl.OriginY, z, pl.XaxisX, pl.XaxisY, pl.XaxisZ, pl.YaxisX, pl.YaxisY, pl.YaxisZ, pl.ZaxisX, pl.ZaxisY, pl.ZaxisZ)

        /// Returns a new plane with Origin translated by Vec.
        static member inline translateBy (v:Vec) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX + v.X, pl.OriginY + v.Y, pl.OriginZ + v.Z, pl.XaxisX, pl.XaxisY, pl.XaxisZ, pl.YaxisX, pl.YaxisY, pl.YaxisZ, pl.ZaxisX, pl.ZaxisY, pl.ZaxisZ)

        /// Returns a new plane with Origin translated in World X direction.
        static member inline translateByWorldX (x:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX + x, pl.OriginY, pl.OriginZ, pl.XaxisX, pl.XaxisY, pl.XaxisZ, pl.YaxisX, pl.YaxisY, pl.YaxisZ, pl.ZaxisX, pl.ZaxisY, pl.ZaxisZ)

        /// Returns a new plane with Origin translated in World Y direction.
        static member inline translateByWorldY (y:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX, pl.OriginY + y, pl.OriginZ, pl.XaxisX, pl.XaxisY, pl.XaxisZ, pl.YaxisX, pl.YaxisY, pl.YaxisZ, pl.ZaxisX, pl.ZaxisY, pl.ZaxisZ)

        /// Returns a new plane with Origin translated in World Z direction.
        static member inline translateByWorldZ (z:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX, pl.OriginY, pl.OriginZ + z, pl.XaxisX, pl.XaxisY, pl.XaxisZ, pl.YaxisX, pl.YaxisY, pl.YaxisZ, pl.ZaxisX, pl.ZaxisY, pl.ZaxisZ)

        /// Rotate about Z-axis of the Plane by an angle in degrees.
        /// Counter-Clockwise in top view (for WorldXY Plane).
        static member inline rotateZ (angDegree:float) (pl:PPlane) : PPlane =
            let m = RigidMatrix.createRotationAxisCenter (pl.Zaxis, pl.Origin, angDegree)
            // Only the rotational 3x3 part of the matrix is applied to the local axis directions.
            let xx = m.M11*pl.XaxisX + m.M21*pl.XaxisY + m.M31*pl.XaxisZ
            let xy = m.M12*pl.XaxisX + m.M22*pl.XaxisY + m.M32*pl.XaxisZ
            let xz = m.M13*pl.XaxisX + m.M23*pl.XaxisY + m.M33*pl.XaxisZ
            let yx = m.M11*pl.YaxisX + m.M21*pl.YaxisY + m.M31*pl.YaxisZ
            let yy = m.M12*pl.YaxisX + m.M22*pl.YaxisY + m.M32*pl.YaxisZ
            let yz = m.M13*pl.YaxisX + m.M23*pl.YaxisY + m.M33*pl.YaxisZ
            PPlane.createUnchecked(pl.OriginX, pl.OriginY, pl.OriginZ, xx, xy, xz, yx, yy, yz, pl.ZaxisX, pl.ZaxisY, pl.ZaxisZ)

        /// Move Plane along the local X-axis by the given distance.
        static member inline translateLocalX (d:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX + pl.XaxisX*d, pl.OriginY + pl.XaxisY*d, pl.OriginZ + pl.XaxisZ*d, pl.XaxisX, pl.XaxisY, pl.XaxisZ, pl.YaxisX, pl.YaxisY, pl.YaxisZ, pl.ZaxisX, pl.ZaxisY, pl.ZaxisZ)

        /// Move Plane along the local Y-axis by the given distance.
        static member inline translateLocalY (d:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX + pl.YaxisX*d, pl.OriginY + pl.YaxisY*d, pl.OriginZ + pl.YaxisZ*d, pl.XaxisX, pl.XaxisY, pl.XaxisZ, pl.YaxisX, pl.YaxisY, pl.YaxisZ, pl.ZaxisX, pl.ZaxisY, pl.ZaxisZ)

        /// Move Plane along the local Z-axis by the given distance.
        /// Same as PPlane.offset.
        static member inline translateLocalZ (d:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX + pl.ZaxisX*d, pl.OriginY + pl.ZaxisY*d, pl.OriginZ + pl.ZaxisZ*d, pl.XaxisX, pl.XaxisY, pl.XaxisZ, pl.YaxisX, pl.YaxisY, pl.YaxisZ, pl.ZaxisX, pl.ZaxisY, pl.ZaxisZ)

        /// Move Plane along the local Z-axis by the given distance.
        /// Same as PPlane.translateLocalZ.
        static member inline offset (d:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX + pl.ZaxisX*d, pl.OriginY + pl.ZaxisY*d, pl.OriginZ + pl.ZaxisZ*d, pl.XaxisX, pl.XaxisY, pl.XaxisZ, pl.YaxisX, pl.YaxisY, pl.YaxisZ, pl.ZaxisX, pl.ZaxisY, pl.ZaxisZ)

        /// Rotate the Plane 180 degrees on its Y-axis.
        /// Called flip because Z-axis points in the opposite direction.
        static member inline flipOnY (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX, pl.OriginY, pl.OriginZ, -pl.XaxisX, -pl.XaxisY, -pl.XaxisZ, pl.YaxisX, pl.YaxisY, pl.YaxisZ, -pl.ZaxisX, -pl.ZaxisY, -pl.ZaxisZ)

        /// Rotate the Plane 180 degrees on its X-axis.
        /// Called flip because Z-axis points in the opposite direction.
        static member inline flipOnX (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX, pl.OriginY, pl.OriginZ, pl.XaxisX, pl.XaxisY, pl.XaxisZ, -pl.YaxisX, -pl.YaxisY, -pl.YaxisZ, -pl.ZaxisX, -pl.ZaxisY, -pl.ZaxisZ)

        /// Rotate the Plane 180 degrees on its Z-axis.
        static member inline rotateOnZ180 (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX, pl.OriginY, pl.OriginZ, -pl.XaxisX, -pl.XaxisY, -pl.XaxisZ, -pl.YaxisX, -pl.YaxisY, -pl.YaxisZ, pl.ZaxisX, pl.ZaxisY, pl.ZaxisZ)

        /// Transforms the plane by the given RigidMatrix.
        /// The returned PPlane has orthogonal unit-vectors.
        static member transformRigid (m:RigidMatrix) (pl:PPlane) : PPlane =
            let ox = m.M11*pl.OriginX + m.M21*pl.OriginY + m.M31*pl.OriginZ + m.X41
            let oy = m.M12*pl.OriginX + m.M22*pl.OriginY + m.M32*pl.OriginZ + m.Y42
            let oz = m.M13*pl.OriginX + m.M23*pl.OriginY + m.M33*pl.OriginZ + m.Z43
            let xx = m.M11*pl.XaxisX + m.M21*pl.XaxisY + m.M31*pl.XaxisZ
            let xy = m.M12*pl.XaxisX + m.M22*pl.XaxisY + m.M32*pl.XaxisZ
            let xz = m.M13*pl.XaxisX + m.M23*pl.XaxisY + m.M33*pl.XaxisZ
            let yx = m.M11*pl.YaxisX + m.M21*pl.YaxisY + m.M31*pl.YaxisZ
            let yy = m.M12*pl.YaxisX + m.M22*pl.YaxisY + m.M32*pl.YaxisZ
            let yz = m.M13*pl.YaxisX + m.M23*pl.YaxisY + m.M33*pl.YaxisZ
            let zx = m.M11*pl.ZaxisX + m.M21*pl.ZaxisY + m.M31*pl.ZaxisZ
            let zy = m.M12*pl.ZaxisX + m.M22*pl.ZaxisY + m.M32*pl.ZaxisZ
            let zz = m.M13*pl.ZaxisX + m.M23*pl.ZaxisY + m.M33*pl.ZaxisZ
            PPlane.createUnchecked(ox, oy, oz, xx, xy, xz, yx, yy, yz, zx, zy, zz)

        /// Rotate Plane 180 degrees around Z-axis if the Y-axis orientation does not match World Y (pl.Yax.Y < 0.0)
        /// To ensure that Y is always positive. For example for showing Text.
        static member inline rotateZ180IfYNegative (pl:PPlane) : PPlane =
            if pl.YaxisY < 0.0 then PPlane.rotateOnZ180 pl else pl



        /// Returns the line of intersection between two planes.
        /// Returns None if they are parallel or coincident.
        static member intersect (a:PPlane) (b:PPlane) : Line3D voption=
            // cross product of the planes' normals gives the direction of the line of intersection.
            let vx = a.ZaxisY * b.ZaxisZ - a.ZaxisZ * b.ZaxisY
            let vy = a.ZaxisZ * b.ZaxisX - a.ZaxisX * b.ZaxisZ
            let vz = a.ZaxisX * b.ZaxisY - a.ZaxisY * b.ZaxisX
            if isTooSmallSq (XYZ.sqLength vx vy vz) then
                // EuclidException.Raise "Euclid.PPlane.intersect: Planes are parallel or coincident: %O, %O" a b
                ValueNone
            else
                // cross product of the direction of the line of intersection with plane A ZAxis
                let px = vy * a.ZaxisZ - vz * a.ZaxisY
                let py = vz * a.ZaxisX - vx * a.ZaxisZ
                let pz = vx * a.ZaxisY - vy * a.ZaxisX
                let nenner = XYZ.dot px py pz b.ZaxisX b.ZaxisY b.ZaxisZ
                let dot = XYZ.dot (b.OriginX - a.OriginX) (b.OriginY - a.OriginY) (b.OriginZ - a.OriginZ) b.ZaxisX b.ZaxisY b.ZaxisZ
                let t = dot / nenner
                let xpx = a.OriginX + px * t
                let xpy = a.OriginY + py * t
                let xpz = a.OriginZ + pz * t
                ValueSome (Line3D(xpx,      xpy,      xpz,
                                  xpx + vx, xpy + vy, xpz + vz)
                             )

        /// Returns the parameter on the line.
        /// The parameter is the intersection point of the infinite ray with the PPlane.
        /// Returns None if they are parallel or coincident.
        static member intersectRay  (ln:Line3D) (pl:PPlane) : float voption =
            let nenner = XYZ.dot ln.VectorX ln.VectorY ln.VectorZ pl.ZaxisX pl.ZaxisY pl.ZaxisZ
            if isTooSmall (abs nenner) then
                ValueNone
            else
                let dot = XYZ.dot (pl.OriginX - ln.FromX) (pl.OriginY - ln.FromY) (pl.OriginZ - ln.FromZ) pl.ZaxisX pl.ZaxisY pl.ZaxisZ
                ValueSome (dot / nenner)


        /// Returns the line parameter and the X and Y parameters on the Plane. as tuple (pLn, pPlX, pPlY).
        /// The parameters is the intersection point of the infinite ray with the PPlane.
        /// Returns None if they are parallel or coincident.
        static member intersectRayAllParameters  (ln:Line3D) (pl:PPlane) : voption<float*float*float> =
            let vx = ln.VectorX
            let vy = ln.VectorY
            let vz = ln.VectorZ
            let nenner = XYZ.dot vx vy vz pl.ZaxisX pl.ZaxisY pl.ZaxisZ
            if isTooSmall (abs nenner) then
                ValueNone
            else
                let dot = XYZ.dot (pl.OriginX - ln.FromX) (pl.OriginY - ln.FromY) (pl.OriginZ - ln.FromZ) pl.ZaxisX pl.ZaxisY pl.ZaxisZ
                let t = dot / nenner
                // The intersection point on the line:
                let wx = ln.FromX + vx * t - pl.OriginX
                let wy = ln.FromY + vy * t - pl.OriginY
                let wz = ln.FromZ + vz * t - pl.OriginZ
                // get closest parameters on the plane:
                let dotX = XYZ.dot pl.XaxisX pl.XaxisY pl.XaxisZ wx wy wz
                let dotY = XYZ.dot pl.YaxisX pl.YaxisY pl.YaxisZ wx wy wz
                ValueSome (t, dotX, dotY)

        /// Returns intersection point of a finite line with the Plane.
        /// Returns None if they are parallel or the domain of intersection is outside 0.0 to 1.0.
        /// Intersection just below 0.0 or just above 1.0 within tolerance of 1e-6 are clamped to 0.0 or 1.0.
        /// Returns None if the line is too short.
        static member intersectLine (ln:Line3D) (pl:PPlane) : Pnt voption =
            match PPlane.intersectRay ln pl with
            | ValueSome (t) ->
                if isBetweenZeroAndOneTolerantIncl t then
                    let c = clamp01 t
                    ValueSome (Pnt(ln.FromX + ln.VectorX * c, ln.FromY + ln.VectorY * c, ln.FromZ + ln.VectorZ * c))
                else
                    ValueNone
            | ValueNone ->
                ValueNone

        /// Checks if a finite Line3D intersects with Plane in one point.
        /// Returns false for NaN values or (almost) parallel or coincident lines.
        static member inline doLinePlaneIntersect (ln:Line3D) (pl:PPlane) : bool =
            let nenner = XYZ.dot ln.VectorX ln.VectorY ln.VectorZ pl.ZaxisX pl.ZaxisY pl.ZaxisZ
            if isTooSmall (abs nenner) then
                false
            else
                let dot = XYZ.dot (pl.OriginX - ln.FromX) (pl.OriginY - ln.FromY) (pl.OriginZ - ln.FromZ) pl.ZaxisX pl.ZaxisY pl.ZaxisZ
                let t = dot / nenner // if nenner is 0.0 then 't' is Infinity
                0. <= t && t <= 1.

        /// Scales the PPlane's origin position by a given factor from the world origin (0,0,0).
        /// The axes directions remain unchanged.
        static member inline scale (factor:float) (pl:PPlane) : PPlane =
            PPlane.createUnchecked(pl.OriginX * factor, pl.OriginY * factor, pl.OriginZ * factor, pl.XaxisX, pl.XaxisY, pl.XaxisZ, pl.YaxisX, pl.YaxisY, pl.YaxisZ, pl.ZaxisX, pl.ZaxisY, pl.ZaxisZ)

        /// Move plane origin by vector. Same as PPlane.move.
        static member inline translate (translation:Vec) (pl:PPlane)  : PPlane =
            PPlane.createUnchecked(pl.OriginX + translation.X, pl.OriginY + translation.Y, pl.OriginZ + translation.Z, pl.XaxisX, pl.XaxisY, pl.XaxisZ, pl.YaxisX, pl.YaxisY, pl.YaxisZ, pl.ZaxisX, pl.ZaxisY, pl.ZaxisZ)

        /// Move plane origin by vector. Same as PPlane.translate.
        static member inline move (translation:Vec) (pl:PPlane)  : PPlane =
            PPlane.createUnchecked(pl.OriginX + translation.X, pl.OriginY + translation.Y, pl.OriginZ + translation.Z, pl.XaxisX, pl.XaxisY, pl.XaxisZ, pl.YaxisX, pl.YaxisY, pl.YaxisZ, pl.ZaxisX, pl.ZaxisY, pl.ZaxisZ)

        [<Obsolete("Use PPlane.isCoincidentTo instead. Obsolete since 0.21.0")>]
        static member inline areCoincident tol (a:PPlane) (b:PPlane) : bool =
            PPlane.isCoincidentTo tol Cosine.``0.25`` a b


        [<Obsolete("rename to PPlane.transformRigid for clarity")>]
        static member transform (m:RigidMatrix) (pl:PPlane) : PPlane =
            PPlane.transformRigid m pl

