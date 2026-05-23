namespace Euclid

open System
open UtilEuclid
open EuclidErrors


/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type Pnt.
[<AutoOpen>]
module AutoOpenPnt =

    type Pnt with

        /// Returns a boolean indicating whether X, Y or Z is NaN or Infinity.
        member inline p.IsInValid : bool =
            isNanInfinity p.X || isNanInfinity p.Y || isNanInfinity p.Z

        /// Returns a boolean indicating whether X, Y or Z is NaN or Infinity.
        static member inline isInValid (pt:Pnt) : bool =
            pt.IsInValid

        /// Returns a boolean indicating whether X, Y and Z are valid (not NaN or Infinity).
        member inline p.IsValid : bool =
            not p.IsInValid

        /// Returns a boolean indicating whether X, Y and Z are valid (not NaN or Infinity).
        static member inline isValid (pt:Pnt) : bool =
            pt.IsValid

        /// Returns the 3D point as 3D vector.
        member inline p.AsVec : Vec =
            Vec(p.X, p.Y, p.Z)

        /// Returns the 3D point as 3D vector.
        static member inline asVec (pt:Pnt) : Vec =
            pt.AsVec

        /// Returns the 3D point as 2D point.
        member inline p.AsPt : Pt =
            Pt(p.X, p.Y)

        /// Returns the 3D point as 2D point.
        static member inline asPt (pt:Pnt) : Pt =
            pt.AsPt

        /// Returns a boolean indicating whether X, Y and Z are exactly 0.0.
        member inline pt.IsOrigin : bool =
            pt.X = 0.0 && pt.Y = 0.0 && pt.Z= 0.0

        /// Returns a boolean indicating whether X, Y and Z are exactly 0.0.
        static member inline isOrigin (pt:Pnt) : bool =
            pt.IsOrigin

        /// Returns a boolean indicating if any of X, Y and Z is not exactly 0.0.
        member inline p.IsNotOrigin : bool =
            p.X <> 0.0 || p.Y <> 0.0 || p.Z <> 0.0

        /// Returns a boolean indicating if any of X, Y and Z is not exactly 0.0.
        static member inline isNotOrigin (pt:Pnt) : bool =
            pt.IsNotOrigin

        /// Returns a boolean indicating whether the absolute value of X, Y and Z is each less than the given tolerance.
        member inline pt.IsAlmostOrigin tol : bool =
            abs pt.X < tol && abs pt.Y < tol && abs pt.Z < tol

        /// Returns a boolean indicating whether the absolute value of X, Y and Z is each less than the given tolerance.
        static member inline isAlmostOrigin tol (pt:Pnt) : bool =
            pt.IsAlmostOrigin tol

        /// Returns new 3D point with new X coordinate, Y and Z stay the same.
        member inline pt.WithX x : Pnt =
            Pnt (x, pt.Y, pt.Z)

        /// Returns new 3D point with new X coordinate, Y and Z stay the same.
        static member inline withX x (pt:Pnt) : Pnt =
            Pnt(x, pt.Y, pt.Z)

        /// Returns a new 3D point with new Y coordinate, X and Z stay the same.
        member inline pt.WithY y : Pnt =
            Pnt (pt.X, y, pt.Z)

        /// Returns a new 3D point with new Y coordinate, X and Z stay the same.
        static member inline withY y (pt:Pnt) : Pnt =
            Pnt(pt.X, y, pt.Z)

        /// Returns a new 3D point with new Z coordinate, X and Y stay the same.
        member inline pt.WithZ z : Pnt =
            Pnt (pt.X, pt.Y, z)

        /// Returns a new 3D point with new Z coordinate, X and Y stay the same.
        static member inline withZ z (pt:Pnt) : Pnt =
            Pnt(pt.X, pt.Y, z)

        /// Returns the distance between two 3D points.
        member inline p.DistanceTo (b:Pnt) : float =
            let x = p.X-b.X
            let y = p.Y-b.Y
            let z = p.Z-b.Z
            sqrt(x*x + y*y + z*z)

        /// Returns the distance between two 3D points.
        static member inline distanceTo (b:Pnt) (p:Pnt) : float =
            p.DistanceTo b

        /// Returns the distance between two 3D points.
        static member inline dist (b:Pnt) (p:Pnt) : float =
            p.DistanceTo b

        /// Returns the squared distance between two 3D points.
        /// This operation is slightly faster than the distance function, and sufficient for many algorithms like finding closest points.
        member inline p.SqDistanceTo (b:Pnt) : float =
            let x = p.X-b.X
            let y = p.Y-b.Y
            let z = p.Z-b.Z
            x*x + y*y + z*z

        /// Returns the squared distance between two 3D points.
        /// This operation is slightly faster than the distance function, and sufficient for many algorithms like finding closest points.
        /// Same as Pnt.sqDist
        static member inline sqDistanceTo (b:Pnt) (p:Pnt) : float =
            p.SqDistanceTo b

        /// Returns the squared distance between two 3D points.
        /// This operation is slightly faster than the distance function, and sufficient for many algorithms like finding closest points.
        /// Same as Pnt.sqDistanceTo.
        static member inline sqDist (b:Pnt) (p:Pnt) : float =
            p.SqDistanceTo b

        /// Returns the distance from Origin (0, 0, 0)
        member inline pt.DistanceFromOrigin : float =
            sqrt (pt.X*pt.X + pt.Y*pt.Y + pt.Z*pt.Z)

        /// Returns the distance from Origin (0, 0, 0)
        static member inline distanceFromOrigin (pt:Pnt) : float =
            pt.DistanceFromOrigin

        /// Returns the squared distance from Origin (0, 0, 0)
        member inline pt.SqDistanceFromOrigin : float =
            pt.X*pt.X + pt.Y*pt.Y + pt.Z*pt.Z

        /// Returns the squared distance from Origin (0, 0, 0)
        static member inline sqDistanceFromOrigin (pt:Pnt) : float =
            pt.SqDistanceFromOrigin

        /// Returns the projected distance from Origin (0, 0, 0). Ignoring the Z component.
        member inline pt.DistanceInXYFromOrigin : float =
            sqrt (pt.X*pt.X + pt.Y*pt.Y)

        /// Returns the projected distance from Origin (0, 0, 0). Ignoring the Z component.
        static member inline distanceInXYFromOrigin (pt:Pnt) : float =
            pt.DistanceInXYFromOrigin

        /// Returns the projected squared distance from Origin (0, 0, 0). Ignoring the Z component.
        member inline pt.DistanceInXYFromOriginSquare : float =
            pt.X*pt.X + pt.Y*pt.Y

        /// Returns the projected squared distance from Origin (0, 0, 0). Ignoring the Z component.
        static member inline distanceInXYFromOriginSquare (pt:Pnt) : float =
            pt.DistanceInXYFromOriginSquare

        /// Returns new 3D point with given distance from Origin by scaling it up or down.
        member inline pt.WithDistanceFromOrigin (l:float) : Pnt =
            let d = pt.DistanceFromOrigin
            if isTooTiny d then failTooSmall "Pnt.WithDistanceFromOrigin" pt
            pt * (l/d)

        /// Returns new 3D point with given distance from Origin by scaling it up or down.
        static member inline withDistanceFromOrigin (l:float) (pt:Pnt) : Pnt =
            pt.WithDistanceFromOrigin l

        /// Returns the Diamond Angle from this point to another point projected in X-Y plane.
        /// The diamond angle is always positive and in the range of 0.0 to 4.0 (for 360 Degrees)
        /// 0.0 = Xaxis, going Counter-Clockwise. Ignoring Z component.
        /// This is the fastest angle computation since it does not use Math.Cos or Math.Sin.
        /// It is useful for radial sorting.
        member inline p.DirectionDiamondInXYTo(o:Pnt) : float =
            // https://stackoverflow.com/a/14675998/969070
            let x = o.X - p.X
            let y = o.Y - p.Y
            if isTooTiny (abs x + abs y) then failTooSmall "Pnt.DirectionDiamondInXYTo" p
            if y >= 0.0 then
                if x >= 0.0 then
                    y/(x + y)
                else
                    1.0 - x/(-x + y)
            else
                if x < 0.0 then
                    2.0 - y/(-x - y)
                else
                    3.0 + x/(x - y)

        /// Returns the Diamond Angle from this point to another point projected in X-Y plane.
        /// The diamond angle is always positive and in the range of 0.0 to 4.0 (for 360 Degrees)
        /// 0.0 = Xaxis, going Counter-Clockwise. Ignoring Z component.
        /// This is the fastest angle computation since it does not use Math.Cos or Math.Sin.
        /// It is useful for radial sorting.
        static member inline directionDiamondInXYTo (o:Pnt, p:Pnt) : float = // not curried because argument order is important
            p.DirectionDiamondInXYTo(o)

        /// Returns the angle in Radians from this point to another point projected in X-Y plane.
        /// 0.0 = Xaxis, going Counter-Clockwise till two Pi.
        member inline p.Angle2PiInXYTo(o:Pnt) : float =
            // https://stackoverflow.com/a/14675998/969070
            let x = o.X-p.X
            let y = o.Y-p.Y
            if isTooTiny (abs x + abs y) then failTooSmall "Pnt.Angle2PiInXYTo" p
            let a = Math.Atan2(y, x)
            if a < 0. then  a + UtilEuclid.twoPi
            else            a

        /// Returns the angle in Radians from this point to another point projected in X-Y plane.
        /// 0.0 = Xaxis, going Counter-Clockwise till two Pi.
        static member inline angle2PiInXYTo (o:Pnt, p:Pnt) : float = // not curried because argument order is important
            p.Angle2PiInXYTo(o)

        /// Returns the angle in Degrees from this point to another point projected in X-Y plane.
        /// 0.0 = Xaxis, going Counter-Clockwise till 360.
        member inline p.Angle360InXYTo(o:Pnt) : float =
            p.Angle2PiInXYTo o |> toDegrees

        /// Returns the angle in Degrees from this point to another point projected in X-Y plane.
        /// 0.0 = Xaxis, going Counter-Clockwise till 360.
        static member inline angle360InXYTo (o:Pnt, p:Pnt) : float = // not curried because argument order is important
            p.Angle360InXYTo(o)

        /// Returns the closest point on a finite line segment to test point.
        /// The line segment is defined by start point 'fromPt' and end point 'toPt'.
        /// Fails if fromPt and toPt are coincident or too close together.
        member inline testPt.ClosestPointOnLine(ln:Line3D) : Pnt =
            XLine3D.clPtLn(ln.FromX, ln.FromY, ln.FromZ, ln.VectorX, ln.VectorY, ln.VectorZ,  testPt.X, testPt.Y, testPt.Z)

        /// Returns the closest point on a finite line segment to test point.
        /// The line segment is defined by start point 'fromPt' and end point 'toPt'.
        /// Fails if fromPt and toPt are coincident or too close together.
        static member inline closestPointOnLine (ln:Line3D) (testPt:Pnt) : Pnt =
            testPt.ClosestPointOnLine(ln)

        /// Returns the squared distance between point and finite line segment.
        /// The line segment is defined by start point 'fromPt', unit direction 'uv', and length 'len'.
        member inline testPt.SqDistanceToLine(ln:Line3D) : float =
            XLine3D.sqDistLnPt(ln.FromX, ln.FromY, ln.FromZ, ln.VectorX, ln.VectorY, ln.VectorZ, testPt.X, testPt.Y, testPt.Z)

        /// Returns the squared distance between point and finite line segment.
        /// The line segment is defined by start point 'fromPt', unit direction 'uv', and length 'len'.
        static member inline sqDistanceToLine (ln:Line3D) (testPt:Pnt) : float =
            testPt.SqDistanceToLine(ln)

        /// Returns the distance between point and finite line segment.
        /// The line segment is defined by start point 'fromPt', unit direction 'uv', and length 'len'.
        member inline testPt.DistanceToLine(ln:Line3D) : float =
            testPt.SqDistanceToLine ln |> sqrt

        /// Returns the distance between point and finite line segment.
        /// The line segment is defined by start point 'fromPt', unit direction 'uv', and length 'len'.
        static member inline distanceToLine (ln:Line3D) (testPt:Pnt) : float =
            testPt.DistanceToLine(ln)

        /// Multiplies (or applies) a Matrix to a 3D point (with an implicit 1 in the 4th dimension,
        /// so that it also works correctly for projections.)
        member inline p.Transform (m:Matrix) : Pnt =
            p *** m // operator * is defined in Matrix.fs

        /// Multiplies (or applies) a Matrix to a 3D point (with an implicit 1 in the 4th dimension,
        /// so that it also works correctly for projections.)
        static member inline transform (m:Matrix) (p:Pnt) : Pnt =
            p.Transform m

        /// Multiplies (or applies) a RigidMatrix to a 3D point.
        member inline p.TransformRigid (m:RigidMatrix) : Pnt =
            p *** m // operator * is defined in RigidMatrix.fs

        /// Multiplies (or applies) a RigidMatrix to a 3D point.
        static member inline transformRigid (m:RigidMatrix) (p:Pnt) : Pnt =
            p.TransformRigid m

        /// Multiplies (or applies) only the 3x3 rotation part of a RigidMatrix to a 3D point.
        member inline p.TransformRigidRotateOnly (m:RigidMatrix) : Pnt =
            let x = p.X
            let y = p.Y
            let z = p.Z
            Pnt ( m.M11*x + m.M21*y + m.M31*z
                , m.M12*x + m.M22*y + m.M32*z
                , m.M13*x + m.M23*y + m.M33*z
                )

        /// Multiplies (or applies) only the 3x3 rotation part of a RigidMatrix to a 3D point.
        static member transformRigidRotateOnly (m:RigidMatrix) (p:Pnt) : Pnt =
            p.TransformRigidRotateOnly m

    // #endregion
    // #region Static members



        /// Checks if two 3D points are equal within tolerance.
        /// Use a tolerance of 0.0 to check for an exact match.
        static member inline equals (tol:float) (a:Pnt) (b:Pnt) : bool =
            abs (a.X-b.X) <= tol &&
            abs (a.Y-b.Y) <= tol &&
            abs (a.Z-b.Z) <= tol

        /// Check if two 3D points are not equal within a given tolerance.
        /// Use a tolerance of 0.0 to check if the two points are not exactly equal.
        static member notEquals (tol:float) (a:Pnt) (b:Pnt) : bool =
            abs (a.X-b.X) > tol ||
            abs (a.Y-b.Y) > tol ||
            abs (a.Z-b.Z) > tol

        /// Accepts any type that has a X, Y and Z (UPPERCASE) member that can be converted to a float.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline createFromMembersXYZ pt : Pnt =
            let x = ( ^T : (member X : _) pt)
            let y = ( ^T : (member Y : _) pt)
            let z = ( ^T : (member Z : _) pt)
            try
                Pnt(float x, float y, float z)
            with e ->
                fail2 "Pnt.createFromMembersXYZ" pt e |> unbox // unbox to make type checker happy

        /// Accepts any type that has a x, y and z (lowercase) member that can be converted to a float.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline createFromMembersxyz pt : Pnt =
            let x = ( ^T : (member x : _) pt)
            let y = ( ^T : (member y : _) pt)
            let z = ( ^T : (member z : _) pt)
            try
                Pnt(float x, float y, float z)
            with e ->
                fail2 "Pnt.createFromMembersxyz" pt e |> unbox // unbox to make type checker happy

        /// Create 3D point from 2D point. Using 0.0 for Z
        static member inline createFromPt (p:Pt) : Pnt =
            Pnt (p.X, p.Y, 0.0)

        /// Create 3D point from 3D vector.
        static member inline createFromVec (v:Vec) : Pnt =
            Pnt (v.X, v.Y, v.Z)

        /// Create 3D point from 3D unit-vector.
        static member inline createFromUnitVec (v:UnitVec) : Pnt =
            Pnt (v.X, v.Y, v.Z)

        // /// Create 3D point from X, Y and Z components.
        // static member inline create (x:float) (y:float) (z:float) : Pnt = Pnt(x, y, z)

        /// Returns a 3D point from Z level and 2D point.
        static member inline createFromPtWithZ (z:float) (p:Pt) : Pnt =
            Pnt (p.X, p.Y, z)

        /// Project point to World X-Y plane.
        /// Use make2D to convert to 2D point instance.
        static member inline projectToXYPlane (pt:Pnt) : Pnt =
            Pnt(pt.X, pt.Y, 0.0)

        /// Gets the X value of 3D point.
        static member inline getX (pt:Pnt) : float =
            pt.X

        /// Gets the Y value of 3D point.
        static member inline getY (pt:Pnt) : float =
            pt.Y

        /// Gets the Z value of 3D point.
        static member inline getZ (pt:Pnt) : float =
            pt.Z

        /// Adds two 3D points and return new 3D point.
        static member inline add (a:Pnt) (b:Pnt) : Pnt =
            a + b

        /// Add a 3D point to a 3D vector and return new 3D point.
        static member inline addVec (v:Vec) (a:Pnt) : Pnt =
            a + v

        /// Returns the midpoint of two 3D points.
        static member inline midPt (a:Pnt) (b:Pnt) : Pnt =
            (a+b) * 0.5

        /// Scale a 3D point by a scalar and return new 3D point.
        static member inline scale (f:float) (pt:Pnt) : Pnt =
            pt*f

        /// Move point 3D by vector. Same as Pnt.move.
        static member inline translate (shift:Vec) (pt:Pnt) : Pnt =
            pt + shift

        /// Move point 3D by vector. Same as Pnt.translate.
        static member inline move (shift:Vec) (pt:Pnt) : Pnt =
            pt + shift

        /// Add float to X component of a 3D point and return new 3D point.
        static member inline moveX (x:float) (pt:Pnt) : Pnt =
            Pnt (pt.X+x, pt.Y, pt.Z)

        /// Add float to Y component of a 3D point and return new 3D point.
        static member inline moveY (y:float) (pt:Pnt) : Pnt =
            Pnt (pt.X, pt.Y+y, pt.Z)

        /// Add float to Z component of a 3D point and return new 3D point.
        static member inline moveZ (z:float) (pt:Pnt) : Pnt =
            Pnt (pt.X, pt.Y, pt.Z+z)

        /// Returns the horizontal distance between two 3D points(ignoring their Z Value)
        static member inline distanceInXY (a:Pnt) (b:Pnt) : float =
            let x = a.X-b.X
            let y = a.Y-b.Y
            sqrt(x*x + y*y)

        /// Returns angle between three 3D Points in Radians. Range 0.0 to Pi.
        static member inline anglePiPts (ptPrev:Pnt, ptThis:Pnt, ptNext:Pnt) : float =
            Vec.anglePi (ptPrev-ptThis) (ptNext-ptThis)

        /// Returns angle between three 3D Points in Degrees. Range 0.0 to 180
        static member inline angle180Pts (ptPrev:Pnt, ptThis:Pnt, ptNext:Pnt) : float =
            Pnt.anglePiPts(ptPrev, ptThis, ptNext) |> toDegrees

        /// Returns a (not unitized) bisector vector in the middle direction from ptThis.
        /// Code : (ptPrev-ptThis).Unitized  + (ptNext-ptThis).Unitized
        /// ptPrev * ptThis * ptNext ->   bisector vector
        static member inline bisector (ptPrev:Pnt, ptThis:Pnt, ptNext:Pnt) : Vec =
            (ptPrev-ptThis).Unitized  + (ptNext-ptThis).Unitized

        /// For three Points describing a plane return a normal.
        /// If the returned vector has length zero then the points are in one line.
        static member normalOf3Pts (a:Pnt, b:Pnt, c:Pnt) : Vec =
            Vec.cross (a-b,c-b)

        /// Returns a point that is at a given distance from a 3D point in the direction of another point.
        static member inline distPt (fromPt:Pnt, dirPt:Pnt, distance:float) : Pnt =
            let x = dirPt.X - fromPt.X
            let y = dirPt.Y - fromPt.Y
            let z = dirPt.Z - fromPt.Z
            let len = sqrt(x*x + y*y + z*z)
            if isTooTiny len then failTooClose "Pnt.distPt" fromPt dirPt
            let fac = distance / len
            Pnt(fromPt.X + x*fac,
                fromPt.Y + y*fac,
                fromPt.Z + z*fac)

        /// Linearly interpolates between two 3D points.
        /// e.g. rel=0.5 will return the middle point, rel=1.0 the endPoint,
        /// rel=3.0 a point three times the distance beyond the end point.
        /// Same as Pnt.lerp.
        static member inline divPt (fromPt:Pnt, toPt:Pnt, rel:float) : Pnt = // not curried because argument order is important and would be confusing
            Pnt(fromPt.X + (toPt.X-fromPt.X)*rel,
                fromPt.Y + (toPt.Y-fromPt.Y)*rel,
                fromPt.Z + (toPt.Z-fromPt.Z)*rel)

        /// Linearly interpolates between two 3D points.
        /// e.g. rel=0.5 will return the middle point, rel=1.0 the endPoint,
        /// rel=3.0 a point three times the distance beyond the end point.
        /// Same as Pnt.divPt.
        static member inline lerp (fromPt:Pnt, toPt:Pnt, rel:float) : Pnt = // not curried because argument order is important and would be confusing
            Pnt.divPt(fromPt, toPt, rel)

        /// Returns a point that is at a given Z level,
        /// going from a point in the direction of another point.
        static member extendToZLevel  (fromPt:Pnt,toPt:Pnt, z:float) : Pnt =
            let v = toPt - fromPt
            if fromPt.Z < toPt.Z && z < fromPt.Z  then fail $"Pnt.extendToZLevel: cannot be reached for fromPt:{fromPt} toPt:{toPt} z:{z}"
            if fromPt.Z > toPt.Z && z > fromPt.Z  then fail $"Pnt.extendToZLevel: cannot be reached for fromPt:{fromPt} toPt:{toPt} z:{z}"
            let dot = abs (v *** Vec.Zaxis)
            if dot < 0.0001 then fail $"Pnt.extendToZLevel: cannot be reached for fromPt:{fromPt} toPt:{toPt} because they are both at the same level. target z:{z}"
            let diffZ = abs (fromPt.Z - z)
            let fac = diffZ / dot
            fromPt + v * fac

        /// Snaps to a point if it is within the snapDistance.
        /// otherwise returns the original point.
        static member inline snapIfClose (snapDistance:float) (snapTo:Pnt) (pt:Pnt) : Pnt =
            let v = snapTo-pt
            if v.Length < snapDistance then snapTo else pt

        /// Snaps the points coordinate to the given precision.
        /// e.g. snap 0.1 Pnt(0.123, 0.456, 0) -> Pnt(0.1, 0.5, 0)
        /// e.g. snap 10  Pnt(3    , 19   , 0) -> Pnt(0  , 20 , 0)
        /// does: (Math.Round (x/precision)) * precision
        static member inline snap (precision:float) (pt:Pnt) : Pnt =
            if isTooTiny (precision) then fail $"Pnt.snap: precision too small or negative:{precision}"
            Pnt( (Math.Round (pt.X/precision)) * precision,
                 (Math.Round (pt.Y/precision)) * precision,
                 (Math.Round (pt.Z/precision)) * precision)

        /// Every 3D line has a normal vector in the X-Y plane.
        /// Rotated Counter-Clockwise in top view.
        /// The result is unitized.
        /// If line is vertical then Xaxis is returned.
        /// see also : Vec.perpendicularVecInXY.
        static member normalOfTwoPointsInXY (fromPt:Pnt, toPt:Pnt) : Vec =
            let x = toPt.Y - fromPt.Y
            let y = fromPt.X - toPt.X  // this is the same as: Vec.cross v Vec.Zaxis
            let len = sqrt(x*x + y*y)
            if isTooTiny len then Vec.Xaxis
            else Vec(x/len, y/len, 0.0)


        // #endregion
        // #region Rotate

        /// Rotate by Quaternion around Origin
        static member inline rotate (q:Quaternion) (pt:Pnt) : Pnt =
            pt *** q  // operator * is defined in Quaternion.fs

        /// Rotate by Quaternion around given center point.
        static member inline rotateWithCenter (cen:Pnt) (q:Quaternion) (pt:Pnt) : Pnt =
            // adapted from https://github.com/mrdoob/three.js/blob/dev/src/math/Vector3.js
            let x = pt.X - cen.X
            let y = pt.Y - cen.Y
            let z = pt.Z - cen.Z
            let qx = q.X
            let qy = q.Y
            let qz = q.Z
            let qw = q.W
            // calculate quat * vector
            let ix =  qw * x + qy * z - qz * y
            let iy =  qw * y + qz * x - qx * z
            let iz =  qw * z + qx * y - qy * x
            let iw = -qx * x - qy * y - qz * z
            // calculate result * inverse quat
            Pnt(  ix * qw + iw * - qx + iy * - qz - iz * - qy  + cen.X
                , iy * qw + iw * - qy + iz * - qx - ix * - qz  + cen.Y
                , iz * qw + iw * - qz + ix * - qy - iy * - qx  + cen.Z
                )


        /// Rotate the 3D point around X-axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member rotateOnX (r:Rotation2D) (p:Pnt) : Pnt =
            Pnt (p.X, r.Cos*p.Y - r.Sin*p.Z, r.Sin*p.Y + r.Cos*p.Z)

        /// Rotate the 3D point around Y-axis, from Z to X-axis, Counter Clockwise looking from back.
        static member rotateOnY (r:Rotation2D) (p:Pnt) : Pnt =
            Pnt (r.Sin*p.Z + r.Cos*p.X, p.Y, r.Cos*p.Z - r.Sin*p.X)

        /// Rotate the 3D point around Z-axis, from X to Y-axis, Counter Clockwise looking from top.
        static member rotateOnZ (r:Rotation2D) (p:Pnt) : Pnt =
            Pnt (r.Cos*p.X - r.Sin*p.Y, r.Sin*p.X + r.Cos*p.Y, p.Z)

        /// Rotate the 3D point around a center 3D point and a X aligned axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member rotateOnXWithCenter (cen:Pnt) (r:Rotation2D) (pt:Pnt) : Pnt =
            let x = pt.X - cen.X
            let y = pt.Y - cen.Y
            let z = pt.Z - cen.Z
            Pnt  (  x                 + cen.X,
                    r.Cos*y - r.Sin*z + cen.Y,
                    r.Sin*y + r.Cos*z + cen.Z)

        /// Rotate the 3D point around a center point and a Y aligned axis, from Z to X-axis, Counter Clockwise looking from back.
        static member rotateOnYWithCenter (cen:Pnt) (r:Rotation2D) (pt:Pnt) : Pnt =
            let x = pt.X - cen.X
            let y = pt.Y - cen.Y
            let z = pt.Z - cen.Z
            Pnt (   r.Sin*z + r.Cos*x + cen.X,
                    y                 + cen.Y,
                    r.Cos*z - r.Sin*x + cen.Z)

        /// Rotate the 3D point around a center point and a Z aligned axis, from X to Y-axis, Counter Clockwise looking from top.
        static member rotateOnZWithCenter (cen:Pnt) (r:Rotation2D) (pt:Pnt) : Pnt =
            let x = pt.X - cen.X
            let y = pt.Y - cen.Y
            let z = pt.Z - cen.Z
            Pnt (   r.Cos*x - r.Sin*y + cen.X,
                    r.Sin*x + r.Cos*y + cen.Y,
                    z                 + cen.Z)

        /// Rotate the 3D point in Degrees around X-axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member inline rotateOnXDeg (angDegree) (pt:Pnt) : Pnt =
            Pnt.rotateOnX (Rotation2D.createFromDegrees angDegree) pt

        /// Rotate the 3D point in Degrees around Y-axis, from Z to X-axis, Counter Clockwise looking from back.
        static member inline rotateOnYDeg (angDegree) (pt:Pnt) : Pnt =
            Pnt.rotateOnY (Rotation2D.createFromDegrees angDegree) pt

        /// Rotate the 3D point in Degrees around Z-axis, from X to Y-axis, Counter Clockwise looking from top.
        static member inline rotateOnZDeg (angDegree) (pt:Pnt) : Pnt =
            Pnt.rotateOnZ (Rotation2D.createFromDegrees angDegree) pt


        /// Rotate the 3D point in Degrees around center point and a X aligned axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member inline rotateOnXWithCenterDeg (cen:Pnt) (angDegree) (pt:Pnt) : Pnt =
            Pnt.rotateOnXWithCenter cen (Rotation2D.createFromDegrees angDegree) pt

        /// Rotate the 3D point in Degrees around center point and a Y aligned axis, from Z to X-axis, Counter Clockwise looking from back.
        static member inline rotateOnYWithCenterDeg (cen:Pnt) (angDegree) (pt:Pnt) : Pnt =
            Pnt.rotateOnYWithCenter cen (Rotation2D.createFromDegrees angDegree) pt

        /// Rotate the 3D point in Degrees around center point and a Z aligned axis, from X to Y-axis, Counter Clockwise looking from top.
        static member inline rotateOnZWithCenterDeg (cen:Pnt) (angDegree) (pt:Pnt) : Pnt =
            Pnt.rotateOnZWithCenter cen (Rotation2D.createFromDegrees angDegree) pt



        /// Returns angle in Degrees at mid point (thisPt).
        static member angleInCorner (prevPt:Pnt, thisPt:Pnt, nextPt:Pnt) : float =
            let a = prevPt-thisPt
            let b = nextPt-thisPt
            Vec.angle180 a b

        /// <summary>
        /// Projects a test point onto an endless line defined by an origin point and a unit direction vector.
        /// </summary>
        /// <param name="fromPt">The origin point of the endless line.</param>
        /// <param name="uv">The unit direction vector describing the orientation of the endless line.</param>
        /// <param name="testPt">The point to be projected onto the line.</param>
        /// <returns>The parameter (scalar value) along the unit vector at which the projection of testPt falls on the line.</returns>
        static member inline projectedParameter (fromPt:Pnt, uv:UnitVec, testPt:Pnt) : float =
            let dir = testPt - fromPt
            Vec.dot (dir, uv)

        /// <summary>
        /// Projects a test point onto an endless line defined by an origin point and a direction vector.
        /// </summary>
        /// <param name="fromPt">The origin point of the endless line.</param>
        /// <param name="v">The direction vector of the endless line. Does not need to be unitized.</param>
        /// <param name="testPt">The point to be projected onto the line.</param>
        /// <returns>The parameter (scaling factor for the direction vector) at which the projection of testPt falls on the line.</returns>
        static member inline projectedParameter (fromPt:Pnt, v:Vec, testPt:Pnt) : float =
            let dir = testPt - fromPt
            let lenSq = v.LengthSq
            if isTooTinySq(lenSq) then failTooSmall "Pnt.projectedParameter" v
            Vec.dot (v, dir) / lenSq

        /// <summary>
        /// Projects a test point onto an endless line defined by two points.
        /// </summary>
        /// <param name="fromPt">The start point defining the endless line.</param>
        /// <param name="toPt">The end point defining the direction of the endless line.</param>
        /// <param name="testPt">The point to be projected onto the line.</param>
        /// <returns>The parameter along the line direction (fromPt to toPt) at which the projection of testPt falls.
        /// A value of 0.0 corresponds to fromPt, 1.0 corresponds to toPt.</returns>
        static member inline projectedParameter (fromPt:Pnt, toPt:Pnt, testPt:Pnt) : float =
            let dir  = testPt - fromPt
            let v    = toPt   - fromPt
            let lenSq = v.LengthSq
            if isTooTinySq(lenSq) then failTooClose "Pnt.projectedParameter" fromPt toPt
            Vec.dot (v, dir) / lenSq

        /// Returns the closer point of the two points to the reference given point.
        /// When both points are equally close, the first point is returned.
        static member closestOfTwo (pt1:Pnt) (pt2:Pnt) (referencePoint:Pnt) : Pnt =
            let d1 = Pnt.sqDistanceTo pt1 referencePoint
            let d2 = Pnt.sqDistanceTo pt2 referencePoint
            if d1 <= d2 then
                pt1
            else
                pt2

        // #endregion
        // #region Obsolete

        [<Obsolete("Use SqDistanceTo instead.")>]
        member inline p.DistanceToSquare (b:Pnt) : float =
            p.SqDistanceTo b

        [<Obsolete("Use SqDistanceFromOrigin instead.")>]
        member inline pt.DistanceFromOriginSquare : float =
            pt.SqDistanceFromOrigin

        [<Obsolete("Use sqDistanceFromOrigin instead.")>]
        static member inline distanceFromOriginSquare (pt:Pnt) : float =
            pt.SqDistanceFromOrigin


        [<Obsolete("Use .distanceTo or .dist instead.")>]
        static member inline distance (a:Pnt) (b:Pnt) : float =
            Pnt.distanceTo a b

        [<Obsolete("Use .sqDistanceTo or .sqDist instead.")>]
        static member inline distanceSq (a:Pnt) (b:Pnt) : float =
            Pnt.sqDistanceTo a b

        [<Obsolete("Use .withDistanceFromOrigin instead.")>]
        static member inline setDistanceFromOrigin f (pt:Pnt) : Pnt =
            Pnt.withDistanceFromOrigin f pt

        [<Obsolete("Use .distanceInXY instead.")>]
        static member inline distanceToXY (a:Pnt) (b:Pnt) : float =
            Pnt.distanceInXY a b

        [<Obsolete("Use Line2D.offset instead")>]
        static member offsetTwoPt(  fromPt:Pnt, toPt:Pnt, distHor:float, distNormal:float) : Pnt*Pnt=
            let v = toPt - fromPt
            let normHor =
                Vec.cross (v , Vec.Zaxis)
                |> Vec.unitizeOrDefault UnitVec.Xaxis

            let normFree =
                Vec.cross (v , normHor)
                |> Vec.unitizeOrDefault UnitVec.Zaxis

            let shift = distHor * normHor + distNormal * normFree
            fromPt +  shift, toPt + shift

        [<Obsolete("Use Pnt.rotateOnXDeg instead.")>]
        static member inline rotateX (angDegree) (pt:Pnt) : Pnt =
            Pnt.rotateOnXDeg angDegree pt

        [<Obsolete("Use Pnt.rotateOnYDeg instead.")>]
        static member inline rotateY (angDegree) (pt:Pnt) : Pnt =
            Pnt.rotateOnYDeg angDegree pt

        [<Obsolete("Use Pnt.rotateOnZDeg instead.")>]
        static member inline rotateZ (angDegree) (pt:Pnt) : Pnt =
            Pnt.rotateOnZDeg angDegree pt

        [<Obsolete("Use Pnt.rotateOnX instead.")>]
        static member inline rotateXBy (r:Rotation2D) (pt:Pnt) : Pnt =
            Pnt.rotateOnX r pt

        [<Obsolete("Use Pnt.rotateOnY instead.")>]
        static member inline rotateYBy (r:Rotation2D) (pt:Pnt) : Pnt =
            Pnt.rotateOnY r pt

        [<Obsolete("Use Pnt.rotateOnZ instead.")>]
        static member inline rotateZBy (r:Rotation2D) (pt:Pnt) : Pnt =
            Pnt.rotateOnZ r pt

        [<Obsolete("Use Pnt.rotateOnXWithCenterDeg instead.")>]
        static member inline rotateXWithCenter (cen:Pnt) (angDegree) (pt:Pnt) : Pnt =
            Pnt.rotateOnXWithCenterDeg cen angDegree pt

        [<Obsolete("Use Pnt.rotateOnYWithCenterDeg instead.")>]
        static member inline rotateYWithCenter (cen:Pnt) (angDegree) (pt:Pnt) : Pnt =
            Pnt.rotateOnYWithCenterDeg cen angDegree pt

        [<Obsolete("Use Pnt.rotateOnZWithCenterDeg instead.")>]
        static member inline rotateZWithCenter (cen:Pnt) (angDegree) (pt:Pnt) : Pnt =
            Pnt.rotateOnZWithCenterDeg cen angDegree pt

        [<Obsolete("Use Pnt.rotateOnXWithCenter instead.")>]
        static member inline rotateXWithCenterBy (cen:Pnt) (r:Rotation2D) (pt:Pnt) : Pnt =
            Pnt.rotateOnXWithCenter cen r pt

        [<Obsolete("Use Pnt.rotateOnYWithCenter instead.")>]
        static member inline rotateYWithCenterBy (cen:Pnt) (r:Rotation2D) (pt:Pnt) : Pnt =
            Pnt.rotateOnYWithCenter cen r pt

        [<Obsolete("Use Pnt.rotateOnZWithCenter instead.")>]
        static member inline rotateZWithCenterBy (cen:Pnt) (r:Rotation2D) (pt:Pnt) : Pnt =
            Pnt.rotateOnZWithCenter cen r pt

        [<Obsolete("Use Pnt.rotate instead.")>]
        static member inline rotateByQuaternion (q:Quaternion) (pt:Pnt) : Pnt =
            Pnt.rotate q pt

        [<Obsolete("Use Pnt.rotateWithCenter instead.")>]
        static member inline rotateWithCenterByQuat (cen:Pnt) (q:Quaternion) (pt:Pnt) : Pnt =
            Pnt.rotateWithCenter cen q pt

        [<Obsolete("Use the constructor Pnt(x, y, z) instead.")>]
        static member inline create (x:float, y:float, z:float) : Pnt =
            Pnt(x, y, z)

        [<Obsolete("Use Pnt.distanceInXY instead.")>]
        static member inline distanceXY (a:Pnt) (b:Pnt) : float =
            Pnt.distanceInXY a b

        [<Obsolete("Use Pnt.rotateOnXWithCenterDeg instead.")>]
        static member inline rotateXwithCenter (cen:Pnt) (angDegree) (pt:Pnt) : Pnt =
            Pnt.rotateOnXWithCenterDeg cen angDegree pt

        [<Obsolete("Use Pnt.rotateOnYWithCenterDeg instead.")>]
        static member inline rotateYwithCenter (cen:Pnt) (angDegree) (pt:Pnt) : Pnt =
            Pnt.rotateOnYWithCenterDeg cen angDegree pt

        [<Obsolete("Use Pnt.rotateOnZWithCenterDeg instead.")>]
        static member inline rotateZwithCenter (cen:Pnt) (angDegree) (pt:Pnt) : Pnt =
            Pnt.rotateOnZWithCenterDeg cen angDegree pt

        [<Obsolete("Use Pnt.rotateOnXWithCenter instead.")>]
        static member inline rotateXwithCenterBy (cen:Pnt) (r:Rotation2D) (pt:Pnt) : Pnt =
            Pnt.rotateOnXWithCenter cen r pt

        [<Obsolete("Use Pnt.rotateOnYWithCenter instead.")>]
        static member inline rotateYwithCenterBy (cen:Pnt) (r:Rotation2D) (pt:Pnt) : Pnt =
            Pnt.rotateOnYWithCenter cen r pt

        [<Obsolete("Use Pnt.rotateOnZWithCenter instead.")>]
        static member inline rotateZwithCenterBy (cen:Pnt) (r:Rotation2D) (pt:Pnt) : Pnt =
            Pnt.rotateOnZWithCenter cen r pt