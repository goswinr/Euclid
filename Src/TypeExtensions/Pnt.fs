namespace Euclid

open System
open UtilEuclid
open EuclidErrors
open System.ComponentModel


/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type Pnt.
[<AutoOpen; EditorBrowsable(EditorBrowsableState.Never)>]
module AutoOpenPnt =

    type Pnt with

        /// Returns a boolean indicating whether X, Y or Z is NaN or Infinity.
        member inline p.IsInvalid : bool =
            isNanInfinity p.X || isNanInfinity p.Y || isNanInfinity p.Z

        /// Same as pnt.IsInvalid.
        static member inline isInvalid (pnt:Pnt) : bool =
            pnt.IsInvalid

        /// Returns a boolean indicating whether X, Y and Z are valid (not NaN or Infinity).
        member inline p.IsValid : bool =
            not p.IsInvalid

        /// Returns a boolean indicating whether X, Y and Z are valid (not NaN or Infinity).
        static member inline isValid (pnt:Pnt) : bool =
            pnt.IsValid

        /// Returns the 3D point as 3D vector.
        member inline p.AsVec : Vec =
            Vec(p.X, p.Y, p.Z)

        /// Returns the 3D point as 3D vector.
        static member inline asVec (pnt:Pnt) : Vec =
            pnt.AsVec

        /// Returns the 3D point as 2D point.
        member inline p.AsPt : Pt =
            Pt(p.X, p.Y)

        /// Returns the 3D point as 2D point.
        static member inline asPt (pnt:Pnt) : Pt =
            pnt.AsPt

        /// Returns the 3D point as 2D vector, discarding the Z value.
        member inline p.AsVc : Vc =
            Vc(p.X, p.Y)

        /// Returns the 3D point as 2D vector, discarding the Z value.
        static member inline asVc (pnt:Pnt) : Vc =
            pnt.AsVc

        /// Returns a boolean indicating whether X, Y and Z are exactly 0.0.
        member inline pnt.IsOrigin : bool =
            pnt.X = 0.0 && pnt.Y = 0.0 && pnt.Z = 0.0

        /// Returns a boolean indicating whether X, Y and Z are exactly 0.0.
        static member inline isOrigin (pnt:Pnt) : bool =
            pnt.IsOrigin

        /// Returns a boolean indicating if any of X, Y and Z is not exactly 0.0.
        member inline p.IsNotOrigin : bool =
            p.X <> 0.0 || p.Y <> 0.0 || p.Z <> 0.0

        /// Returns a boolean indicating if any of X, Y and Z is not exactly 0.0.
        static member inline isNotOrigin (pnt:Pnt) : bool =
            pnt.IsNotOrigin

        /// Returns a boolean indicating whether the absolute value of X, Y and Z is each less than the given tolerance.
        member inline pnt.IsAlmostOrigin tol : bool =
            abs pnt.X < tol && abs pnt.Y < tol && abs pnt.Z < tol

        /// Returns a boolean indicating whether the absolute value of X, Y and Z is each less than the given tolerance.
        static member inline isAlmostOrigin tol (pnt:Pnt) : bool =
            pnt.IsAlmostOrigin tol

        /// Returns new 3D point with new X coordinate, Y and Z stay the same.
        member inline pnt.WithX x : Pnt =
            Pnt (x, pnt.Y, pnt.Z)

        /// Returns new 3D point with new X coordinate, Y and Z stay the same.
        static member inline withX x (pnt:Pnt) : Pnt =
            Pnt(x, pnt.Y, pnt.Z)

        /// Returns a new 3D point with new Y coordinate, X and Z stay the same.
        member inline pnt.WithY y : Pnt =
            Pnt (pnt.X, y, pnt.Z)

        /// Returns a new 3D point with new Y coordinate, X and Z stay the same.
        static member inline withY y (pnt:Pnt) : Pnt =
            Pnt(pnt.X, y, pnt.Z)

        /// Returns a new 3D point with new Z coordinate, X and Y stay the same.
        member inline pnt.WithZ z : Pnt =
            Pnt (pnt.X, pnt.Y, z)

        /// Returns a new 3D point with new Z coordinate, X and Y stay the same.
        static member inline withZ z (pnt:Pnt) : Pnt =
            Pnt(pnt.X, pnt.Y, z)


        /// Returns the distance between two 3D points.
        member inline p.DistanceTo (b:Pnt) : float =
            let x = p.X-b.X
            let y = p.Y-b.Y
            let z = p.Z-b.Z
            sqrt(x*x + y*y + z*z)

        /// Returns the distance between two 3D points.
        static member inline distanceTo (b:Pnt) (p:Pnt) : float =
            p.DistanceTo b

        /// Returns the distance between a 3D point and given x, y, z coordinates.
        member inline p.DistanceToXYZ (x:float, y:float, z:float) : float =
            let dx = p.X - x
            let dy = p.Y - y
            let dz = p.Z - z
            sqrt(dx*dx + dy*dy + dz*dz)

        /// Returns the distance between a 3D point and given x, y, z coordinates.
        static member inline distanceToXYZ (x:float) (y:float) (z:float) (p:Pnt) : float =
            p.DistanceToXYZ (x, y, z)

        /// Returns the squared distance between two 3D points.
        /// This operation is slightly faster than the distance function, and sufficient for many algorithms like finding closest points.
        member inline p.DistanceToSq (b:Pnt) : float =
            let x = p.X-b.X
            let y = p.Y-b.Y
            let z = p.Z-b.Z
            x*x + y*y + z*z

        /// Returns the squared distance between two 3D points.
        /// This operation is slightly faster than the distance function, and sufficient for many algorithms like finding closest points.
        static member inline distanceToSq (b:Pnt) (p:Pnt) : float =
            p.DistanceToSq b


        /// Returns the squared distance between a 3D point and given x, y, z coordinates.
        member inline p.DistanceToXYZSq (x:float, y:float, z:float) : float =
            let dx = p.X - x
            let dy = p.Y - y
            let dz = p.Z - z
            dx*dx + dy*dy + dz*dz

        /// Returns the squared distance between a 3D point and given x, y, z coordinates.
        static member inline distanceToXYZSq (x:float) (y:float) (z:float) (p:Pnt) : float =
            p.DistanceToXYZSq (x, y, z)





        /// Returns the distance from Origin (0, 0, 0)
        member inline pnt.DistanceFromOrigin : float =
            sqrt (pnt.X*pnt.X + pnt.Y*pnt.Y + pnt.Z*pnt.Z)

        /// Returns the distance from Origin (0, 0, 0)
        static member inline distanceFromOrigin (pnt:Pnt) : float =
            pnt.DistanceFromOrigin

        /// Returns the squared distance from Origin (0, 0, 0)
        member inline pnt.DistanceFromOriginSq : float =
            pnt.X*pnt.X + pnt.Y*pnt.Y + pnt.Z*pnt.Z

        /// Returns the squared distance from Origin (0, 0, 0)
        static member inline distanceFromOriginSq (pnt:Pnt) : float =
            pnt.DistanceFromOriginSq

        /// Returns the projected distance from Origin (0, 0, 0). Ignoring the Z component.
        member inline pnt.DistanceInXYFromOrigin : float =
            sqrt (pnt.X*pnt.X + pnt.Y*pnt.Y)

        /// Returns the projected distance from Origin (0, 0, 0). Ignoring the Z component.
        static member inline distanceInXYFromOrigin (pnt:Pnt) : float =
            pnt.DistanceInXYFromOrigin

        /// Returns the projected squared distance from Origin (0, 0, 0). Ignoring the Z component.
        member inline pnt.DistanceInXYFromOriginSq : float =
            pnt.X*pnt.X + pnt.Y*pnt.Y

        /// Same as pnt.DistanceInXYFromOriginSq.
        static member inline distanceInXYFromOriginSq (pnt:Pnt) : float =
            pnt.DistanceInXYFromOriginSq

        /// Returns new 3D point with given distance from Origin by scaling it up or down.
        member inline pnt.WithDistanceFromOrigin (l:float) : Pnt =
            let d = pnt.DistanceFromOrigin
            if isTooTiny d then failTooSmall "Pnt.WithDistanceFromOrigin" pnt
            pnt * (l/d)

        /// Returns new 3D point with given distance from Origin by scaling it up or down.
        static member inline withDistanceFromOrigin (l:float) (pnt:Pnt) : Pnt =
            pnt.WithDistanceFromOrigin l

        /// Returns the Diamond Angle from this point to another point projected in X-Y plane.
        /// The diamond angle is always positive and in the range of 0.0 to 4.0 (for 360 degrees)
        /// 0.0 = XAxis, going counter-clockwise. Ignoring Z component.
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

        /// Returns the Diamond Angle from 'fromPnt' to 'toPnt' projected in X-Y plane.
        /// The diamond angle is always positive and in the range of 0.0 to 4.0 (for 360 degrees)
        /// 0.0 = XAxis, going counter-clockwise. Ignoring Z component.
        /// This is the fastest angle computation since it does not use Math.Cos or Math.Sin.
        /// It is useful for radial sorting.
        static member inline directionDiamondInXYTo (fromPnt:Pnt) (toPnt:Pnt) : float =
            fromPnt.DirectionDiamondInXYTo(toPnt)

        /// Returns the angle in radians from this point to another point projected in X-Y plane.
        /// 0.0 = XAxis, going counter-clockwise till two Pi.
        member inline p.Angle2PiInXYTo(o:Pnt) : float =
            // https://stackoverflow.com/a/14675998/969070
            let x = o.X-p.X
            let y = o.Y-p.Y
            if isTooTiny (abs x + abs y) then failTooSmall "Pnt.Angle2PiInXYTo" p
            let a = Math.Atan2(y, x)
            if a < 0. then  a + UtilEuclid.twoPi
            else            a

        /// Returns the angle in radians from 'fromPnt' to 'toPnt' projected in X-Y plane.
        /// 0.0 = XAxis, going counter-clockwise till two Pi.
        static member inline angle2PiInXYTo (fromPnt:Pnt) (toPnt:Pnt) : float =
            fromPnt.Angle2PiInXYTo(toPnt)

        /// Returns the angle in degrees from this point to another point projected in X-Y plane.
        /// 0.0 = XAxis, going counter-clockwise till 360.
        member inline p.Angle360InXYTo(o:Pnt) : float =
            p.Angle2PiInXYTo o |> toDegrees

        /// Returns the angle in degrees from 'fromPnt' to 'toPnt' projected in X-Y plane.
        /// 0.0 = XAxis, going counter-clockwise till 360.
        static member inline angle360InXYTo (fromPnt:Pnt) (toPnt:Pnt) : float =
            fromPnt.Angle360InXYTo(toPnt)

        /// Returns the closest point on a finite line segment to test point.
        /// The line segment is given as a Line3D `ln`.
        /// Fails if the line is degenerate (zero-length).
        member inline testPnt.ClosestPointOnLine(ln:Line3D) : Pnt =
            XLineXYZ.clPtLn(ln.FromX, ln.FromY, ln.FromZ, ln.VectorX, ln.VectorY, ln.VectorZ,  testPnt.X, testPnt.Y, testPnt.Z)

        /// Returns the closest point on a finite line segment to test point.
        /// The line segment is given as a Line3D `ln`.
        /// Fails if the line is degenerate (zero-length).
        static member inline closestPointOnLine (ln:Line3D) (testPnt:Pnt) : Pnt =
            testPnt.ClosestPointOnLine(ln)

        /// Returns the squared distance between point and finite line segment.
        /// The line segment is given as a Line3D `ln`.
        member inline testPnt.DistanceToLineSq(ln:Line3D) : float =
            XLineXYZ.sqDistLnPt(ln.FromX, ln.FromY, ln.FromZ, ln.VectorX, ln.VectorY, ln.VectorZ, testPnt.X, testPnt.Y, testPnt.Z)

        /// Returns the squared distance between point and finite line segment.
        /// The line segment is given as a Line3D `ln`.
        static member inline distanceToLineSq (ln:Line3D) (testPnt:Pnt) : float =
            testPnt.DistanceToLineSq(ln)

        /// Returns the distance between point and finite line segment.
        /// The line segment is given as a Line3D `ln`.
        member inline testPnt.DistanceToLine(ln:Line3D) : float =
            testPnt.DistanceToLineSq ln |> sqrt

        /// Returns the distance between point and finite line segment.
        /// The line segment is given as a Line3D `ln`.
        static member inline distanceToLine (ln:Line3D) (testPnt:Pnt) : float =
            testPnt.DistanceToLine(ln)

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
        /// The tolerance applies to the X, Y and Z coordinates separately, not to the Euclidean distance between the points.
        /// Use Pnt.equalsExact to check for an exact match.
        static member inline equalsWithin (tol:float) (a:Pnt) (b:Pnt) : bool =
            abs (a.X-b.X) <= tol &&
            abs (a.Y-b.Y) <= tol &&
            abs (a.Z-b.Z) <= tol

        /// Checks if two 3D points are exactly equal.
        /// Compares each field with the = operator, so a NaN value is never equal.
        static member inline equalsExact (a:Pnt) (b:Pnt) : bool =
            a.X = b.X &&
            a.Y = b.Y &&
            a.Z = b.Z

        /// Check if two 3D points are not equal within a given tolerance.
        /// The tolerance applies to the X, Y and Z coordinates separately, not to the Euclidean distance between the points.
        static member notEqualsWithin (tol:float) (a:Pnt) (b:Pnt) : bool =
            abs (a.X-b.X) > tol ||
            abs (a.Y-b.Y) > tol ||
            abs (a.Z-b.Z) > tol

        /// Accepts any type that has an X, Y and Z (UPPERCASE) member that can be converted to a float.
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

        /// Create 3D point from X, Y and Z components.
        static member inline create (x:float) (y:float) (z:float) : Pnt =
            Pnt(x, y, z)

        /// Returns a 3D point from Z level and 2D point.
        static member inline createFromPtWithZ (z:float) (p:Pt) : Pnt =
            Pnt (p.X, p.Y, z)

        /// Project point to World X-Y plane.
        /// Use Pnt.asPt to convert to a 2D point instance.
        static member inline projectToXYPlane (pnt:Pnt) : Pnt =
            Pnt(pnt.X, pnt.Y, 0.0)

        /// Gets the X value of 3D point.
        static member inline getX (pnt:Pnt) : float =
            pnt.X

        /// Gets the Y value of 3D point.
        static member inline getY (pnt:Pnt) : float =
            pnt.Y

        /// Gets the Z value of 3D point.
        static member inline getZ (pnt:Pnt) : float =
            pnt.Z

        /// Adds two 3D points and return new 3D point.
        static member inline add (a:Pnt) (b:Pnt) : Pnt =
            a + b

        /// Returns the midpoint of two 3D points.
        static member inline midPnt (a:Pnt) (b:Pnt) : Pnt =
            Pnt((a.X + b.X) * 0.5, (a.Y + b.Y) * 0.5, (a.Z + b.Z) * 0.5)

        /// Scale a 3D point by a scalar and return new 3D point.
        static member inline scale (f:float) (pnt:Pnt) : Pnt =
            pnt*f

        /// <summary>Scales a 3D point by a factor around a given center point.</summary>
        /// <param name="cen">The center point of the scaling.</param>
        /// <param name="factor">The scale factor.</param>
        /// <param name="pnt">The 3D point to scale.</param>
        /// <returns>The scaled 3D point.</returns>
        static member inline scaleOn (cen:Pnt) (factor:float) (pnt:Pnt) : Pnt =
            Pnt(cen.X + (pnt.X - cen.X) * factor,
                cen.Y + (pnt.Y - cen.Y) * factor,
                cen.Z + (pnt.Z - cen.Z) * factor)

        /// Move point 3D by vector.
        static member inline move (shift:Vec) (pnt:Pnt) : Pnt =
            pnt + shift

        /// Add float to X component of a 3D point and return new 3D point.
        static member inline moveX (x:float) (pnt:Pnt) : Pnt =
            Pnt (pnt.X+x, pnt.Y, pnt.Z)

        /// Add float to Y component of a 3D point and return new 3D point.
        static member inline moveY (y:float) (pnt:Pnt) : Pnt =
            Pnt (pnt.X, pnt.Y+y, pnt.Z)

        /// Add float to Z component of a 3D point and return new 3D point.
        static member inline moveZ (z:float) (pnt:Pnt) : Pnt =
            Pnt (pnt.X, pnt.Y, pnt.Z+z)

        /// Returns the horizontal distance between two 3D points(ignoring their Z Value)
        static member inline distanceInXY (a:Pnt) (b:Pnt) : float =
            let x = a.X-b.X
            let y = a.Y-b.Y
            sqrt(x*x + y*y)

        /// Returns angle between three 3D Points in radians. Range 0.0 to Pi.
        static member inline anglePiPnts (pntPrev:Pnt) (pntThis:Pnt) (pntNext:Pnt) : float =
            anglePiOfVecs "Pnt.anglePiPnts"
                (pntPrev.X - pntThis.X) (pntPrev.Y - pntThis.Y) (pntPrev.Z - pntThis.Z)
                (pntNext.X - pntThis.X) (pntNext.Y - pntThis.Y) (pntNext.Z - pntThis.Z)

        /// Returns angle between three 3D Points in degrees. Range 0.0 to 180
        static member inline angle180Pnts (pntPrev:Pnt) (pntThis:Pnt) (pntNext:Pnt) : float =
            Pnt.anglePiPnts pntPrev pntThis pntNext |> toDegrees

        /// Returns a (not unitized) bisector vector in the middle direction from pntThis.
        /// Code : (pntPrev-pntThis).Unitized  + (pntNext-pntThis).Unitized
        /// pntPrev * pntThis * pntNext ->   bisector vector
        static member inline bisector (pntPrev:Pnt) (pntThis:Pnt) (pntNext:Pnt) : Vec =
            let ax = pntPrev.X - pntThis.X
            let ay = pntPrev.Y - pntThis.Y
            let az = pntPrev.Z - pntThis.Z
            let bx = pntNext.X - pntThis.X
            let by = pntNext.Y - pntThis.Y
            let bz = pntNext.Z - pntThis.Z
            let la = sqrt (ax*ax + ay*ay + az*az)
            if isTooTiny la then failUnit3 "Pnt.bisector: pntPrev to pntThis" ax ay az
            let lb = sqrt (bx*bx + by*by + bz*bz)
            if isTooTiny lb then failUnit3 "Pnt.bisector: pntNext to pntThis" bx by bz
            let fa = 1. / la
            let fb = 1. / lb
            Vec(fa*ax + fb*bx, fa*ay + fb*by, fa*az + fb*bz)

        /// For three Points describing a plane return a normal.
        /// The points 'a', 'b' and 'c' in counter-clockwise order return a normal pointing towards the
        /// viewer (e.g. +Z for points in the XY plane), matching NPlane.createFrom3Pnts and
        /// Points3D.normalOfPoints. The result is not unitized.
        /// If the returned vector has length zero then the points are in one line.
        static member normalOf3Pnts (a:Pnt) (b:Pnt) (c:Pnt) : Vec =
            // cross product of (b - a) and (c - a)
            let vx = b.X - a.X
            let vy = b.Y - a.Y
            let vz = b.Z - a.Z
            let wx = c.X - a.X
            let wy = c.Y - a.Y
            let wz = c.Z - a.Z
            Vec(vy * wz - vz * wy,
                vz * wx - vx * wz,
                vx * wy - vy * wx)

        /// Returns a point that is at a given distance from a 3D point in the direction of another point.
        static member inline pointAtDistance (fromPnt:Pnt) (dirPnt:Pnt) (distance:float) : Pnt =
            let x = dirPnt.X - fromPnt.X
            let y = dirPnt.Y - fromPnt.Y
            let z = dirPnt.Z - fromPnt.Z
            let len = sqrt(x*x + y*y + z*z)
            if isTooTiny len then failTooClose "Pnt.pointAtDistance" fromPnt dirPnt
            let fac = distance / len
            Pnt(fromPnt.X + x*fac,
                fromPnt.Y + y*fac,
                fromPnt.Z + z*fac)

        /// Linearly interpolates between two 3D points.
        /// e.g. rel=0.5 will return the middle point, rel=1.0 the endPoint,
        /// rel=3.0 a point three times the distance beyond the end point.
        /// Same as Pnt.lerp.
        static member inline divPnt (fromPnt:Pnt) (toPnt:Pnt) (rel:float) : Pnt =
            Pnt(fromPnt.X + (toPnt.X-fromPnt.X)*rel,
                fromPnt.Y + (toPnt.Y-fromPnt.Y)*rel,
                fromPnt.Z + (toPnt.Z-fromPnt.Z)*rel)

        /// Linearly interpolates between two 3D points.
        /// e.g. rel=0.5 will return the middle point, rel=1.0 the endPoint,
        /// rel=3.0 a point three times the distance beyond the end point.
        /// Same as Pnt.divPnt.
        static member inline lerp (fromPnt:Pnt) (toPnt:Pnt) (rel:float) : Pnt =
            Pnt.divPnt fromPnt toPnt rel

        /// Returns a point that is at a given Z level,
        /// going from a point in the direction of another point.
        static member extendToZLevel (fromPnt:Pnt) (toPnt:Pnt) (z:float) : Pnt =
            let vx = toPnt.X - fromPnt.X
            let vy = toPnt.Y - fromPnt.Y
            let vz = toPnt.Z - fromPnt.Z
            if fromPnt.Z < toPnt.Z && z < fromPnt.Z  then fail $"Pnt.extendToZLevel: cannot be reached for fromPnt:{fromPnt} toPnt:{toPnt} z:{z}"
            if fromPnt.Z > toPnt.Z && z > fromPnt.Z  then fail $"Pnt.extendToZLevel: cannot be reached for fromPnt:{fromPnt} toPnt:{toPnt} z:{z}"
            let dot = abs vz // the dot product with the Z-axis
            if dot < 0.0001 then fail $"Pnt.extendToZLevel: cannot be reached for fromPnt:{fromPnt} toPnt:{toPnt} because they are both at the same level. target z:{z}"
            let diffZ = abs (fromPnt.Z - z)
            let fac = diffZ / dot
            Pnt(fromPnt.X + vx * fac, fromPnt.Y + vy * fac, fromPnt.Z + vz * fac)

        /// Snaps to a point if it is within the snapDistance.
        /// otherwise returns the original point.
        static member inline snapIfClose (snapDistance:float) (snapTo:Pnt) (pnt:Pnt) : Pnt =
            let v = snapTo-pnt
            if v.Length < snapDistance then snapTo else pnt

        /// Snaps the points coordinate to the given precision.
        /// e.g. snap 0.1 Pnt(0.123, 0.456, 0) -> Pnt(0.1, 0.5, 0)
        /// e.g. snap 10  Pnt(3    , 19   , 0) -> Pnt(0  , 20 , 0)
        /// does: (Math.Round (x/precision)) * precision
        static member inline snap (precision:float) (pnt:Pnt) : Pnt =
            if isTooTiny (precision) then fail $"Pnt.snap: precision too small or negative:{precision}"
            Pnt( (Math.Round (pnt.X/precision)) * precision,
                 (Math.Round (pnt.Y/precision)) * precision,
                 (Math.Round (pnt.Z/precision)) * precision)

        /// Every 3D line has a normal vector in the X-Y plane.
        /// Rotated counter-clockwise in top view.
        /// The result is unitized.
        /// If line is vertical then XAxis is returned.
        /// see also : Vec.perpendicularVecInXY.
        static member normalOfTwoPointsInXY (fromPnt:Pnt) (toPnt:Pnt) : Vec =
            let x = toPnt.Y - fromPnt.Y
            let y = fromPnt.X - toPnt.X  // this is the same as: Vec.cross v Vec.ZAxis
            let len = sqrt(x*x + y*y)
            if isTooTiny len then Vec.XAxis
            else Vec(x/len, y/len, 0.0)


        // #endregion
        // #region Rotate

        /// <summary>Rotate the 3D point by a Quaternion around the World Origin.</summary>
        /// <param name="q">The rotation to apply.</param>
        /// <returns>The rotated 3D point.</returns>
        member inline pnt.Rotate (q:Quaternion) : Pnt =
            pnt *** q  // operator * is defined in Quaternion.fs

        /// Rotate by Quaternion around Origin
        static member inline rotate (q:Quaternion) (pnt:Pnt) : Pnt =
            pnt *** q  // operator * is defined in Quaternion.fs

        /// <summary>Rotate a 3D point by a Quaternion around a given center point.</summary>
        /// <param name="cen">The center point of the rotation.</param>
        /// <param name="q">The rotation to apply.</param>
        /// <param name="pnt">The 3D point to rotate.</param>
        /// <returns>The rotated 3D point.</returns>
        static member inline rotateAround (cen:Pnt) (q:Quaternion) (pnt:Pnt) : Pnt =
            // adapted from https://github.com/mrdoob/three.js/blob/dev/src/math/Vector3.js
            let x = pnt.X - cen.X
            let y = pnt.Y - cen.Y
            let z = pnt.Z - cen.Z
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

        /// <summary>Rotate the 3D point by a Quaternion around a given center point.</summary>
        /// <param name="cen">The center point of the rotation.</param>
        /// <param name="q">The rotation to apply.</param>
        /// <returns>The rotated 3D point.</returns>
        member inline pnt.RotateAround (cen:Pnt) (q:Quaternion) : Pnt =
            Pnt.rotateAround cen q pnt


        /// Rotate the 3D point around X-axis, from Y to Z-axis, counter-clockwise looking from right.
        static member rotateOnX (r:Rotation2D) (p:Pnt) : Pnt =
            Pnt (p.X, r.Cos*p.Y - r.Sin*p.Z, r.Sin*p.Y + r.Cos*p.Z)

        /// Rotate the 3D point around Y-axis, from Z to X-axis, counter-clockwise looking from back.
        static member rotateOnY (r:Rotation2D) (p:Pnt) : Pnt =
            Pnt (r.Sin*p.Z + r.Cos*p.X, p.Y, r.Cos*p.Z - r.Sin*p.X)

        /// Rotate the 3D point around Z-axis, from X to Y-axis, counter-clockwise looking from top.
        static member rotateOnZ (r:Rotation2D) (p:Pnt) : Pnt =
            Pnt (r.Cos*p.X - r.Sin*p.Y, r.Sin*p.X + r.Cos*p.Y, p.Z)

        /// Rotate the 3D point around a center 3D point and an X-aligned axis, from Y to Z-axis, counter-clockwise looking from right.
        static member rotateOnXAround (cen:Pnt) (r:Rotation2D) (pnt:Pnt) : Pnt =
            let x = pnt.X - cen.X
            let y = pnt.Y - cen.Y
            let z = pnt.Z - cen.Z
            Pnt  (  x                 + cen.X,
                    r.Cos*y - r.Sin*z + cen.Y,
                    r.Sin*y + r.Cos*z + cen.Z)

        /// Rotate the 3D point around a center point and a Y aligned axis, from Z to X-axis, counter-clockwise looking from back.
        static member rotateOnYAround (cen:Pnt) (r:Rotation2D) (pnt:Pnt) : Pnt =
            let x = pnt.X - cen.X
            let y = pnt.Y - cen.Y
            let z = pnt.Z - cen.Z
            Pnt (   r.Sin*z + r.Cos*x + cen.X,
                    y                 + cen.Y,
                    r.Cos*z - r.Sin*x + cen.Z)

        /// Rotate the 3D point around a center point and a Z aligned axis, from X to Y-axis, counter-clockwise looking from top.
        static member rotateOnZAround (cen:Pnt) (r:Rotation2D) (pnt:Pnt) : Pnt =
            let x = pnt.X - cen.X
            let y = pnt.Y - cen.Y
            let z = pnt.Z - cen.Z
            Pnt (   r.Cos*x - r.Sin*y + cen.X,
                    r.Sin*x + r.Cos*y + cen.Y,
                    z                 + cen.Z)

        /// Rotate the 3D point in degrees around X-axis, from Y to Z-axis, counter-clockwise looking from right.
        static member inline rotateOnXDegrees (angDegree) (pnt:Pnt) : Pnt =
            Pnt.rotateOnX (Rotation2D.createFromDegrees angDegree) pnt

        /// Rotate the 3D point in degrees around Y-axis, from Z to X-axis, counter-clockwise looking from back.
        static member inline rotateOnYDegrees (angDegree) (pnt:Pnt) : Pnt =
            Pnt.rotateOnY (Rotation2D.createFromDegrees angDegree) pnt

        /// Rotate the 3D point in degrees around Z-axis, from X to Y-axis, counter-clockwise looking from top.
        static member inline rotateOnZDegrees (angDegree) (pnt:Pnt) : Pnt =
            Pnt.rotateOnZ (Rotation2D.createFromDegrees angDegree) pnt


        /// Rotate the 3D point in degrees around center point and an X-aligned axis, from Y to Z-axis, counter-clockwise looking from right.
        static member inline rotateOnXDegreesAround (cen:Pnt) (angDegree) (pnt:Pnt) : Pnt =
            Pnt.rotateOnXAround cen (Rotation2D.createFromDegrees angDegree) pnt

        /// Rotate the 3D point in degrees around center point and a Y aligned axis, from Z to X-axis, counter-clockwise looking from back.
        static member inline rotateOnYDegreesAround (cen:Pnt) (angDegree) (pnt:Pnt) : Pnt =
            Pnt.rotateOnYAround cen (Rotation2D.createFromDegrees angDegree) pnt

        /// Rotate the 3D point in degrees around center point and a Z aligned axis, from X to Y-axis, counter-clockwise looking from top.
        static member inline rotateOnZDegreesAround (cen:Pnt) (angDegree) (pnt:Pnt) : Pnt =
            Pnt.rotateOnZAround cen (Rotation2D.createFromDegrees angDegree) pnt

        /// 90-degree rotation counter-clockwise around the Z-axis through the World Origin.
        member inline pnt.Rotate2Dby90CCW : Pnt =
            Pnt(-pnt.Y, pnt.X, pnt.Z)

        /// 90-degree rotation counter-clockwise around the Z-axis through the World Origin.
        static member inline rotate2Dby90CCW (pnt:Pnt) : Pnt =
            Pnt(-pnt.Y, pnt.X, pnt.Z)

        /// 90-degree rotation clockwise around the Z-axis through the World Origin.
        member inline pnt.Rotate2Dby90CW : Pnt =
            Pnt(pnt.Y, -pnt.X, pnt.Z)

        /// 90-degree rotation clockwise around the Z-axis through the World Origin.
        static member inline rotate2Dby90CW (pnt:Pnt) : Pnt =
            Pnt(pnt.Y, -pnt.X, pnt.Z)

        /// Rotates the 3D point around the Z-axis through the World Origin by a given number of quarter-circles (i.e. multiples of 90
        /// degrees or Pi/2 radians). A positive number rotates counter-clockwise, a
        /// negative number rotates clockwise. The Z coordinate is preserved.
        member inline pnt.RotateByQuarterCircle(numberOfQuarters:int) : Pnt =
            Pnt.rotateByQuarterCircle numberOfQuarters pnt

        /// Rotates a 3D point around the Z-axis through the World Origin by a given number of quarter-circles (i.e. multiples of 90
        /// degrees or Pi/2 radians). A positive number rotates counter-clockwise, a
        /// negative number rotates clockwise. The Z coordinate is preserved.
        static member rotateByQuarterCircle (numberOfQuarters:int) (pnt:Pnt) : Pnt =
            let mutable nQuad = numberOfQuarters % 4
            if nQuad < 0 then nQuad <- nQuad + 4
            match nQuad with
            | 0 -> pnt
            | 1 -> Pnt(-pnt.Y, pnt.X, pnt.Z)
            | 2 -> Pnt(-pnt.X, -pnt.Y, pnt.Z)
            | 3 -> Pnt(pnt.Y, -pnt.X, pnt.Z)
            | _ -> pnt // should never happen because of the modulo % 4 operation



        /// <summary>
        /// Projects a test point onto an endless line defined by an origin point and a unit direction vector.
        /// </summary>
        /// <param name="fromPnt">The origin point of the endless line.</param>
        /// <param name="uv">The unit direction vector describing the orientation of the endless line.</param>
        /// <param name="testPnt">The point to be projected onto the line.</param>
        /// <returns>The parameter (scalar value) along the unit vector at which the projection of testPnt falls on the line.</returns>
        static member inline projectedParameterOnUnitVec (fromPnt:Pnt) (uv:UnitVec) (testPnt:Pnt) : float =
            (testPnt.X - fromPnt.X) * uv.X + (testPnt.Y - fromPnt.Y) * uv.Y + (testPnt.Z - fromPnt.Z) * uv.Z

        /// <summary>
        /// Projects a test point onto an endless line defined by an origin point and a direction vector.
        /// </summary>
        /// <param name="fromPnt">The origin point of the endless line.</param>
        /// <param name="v">The direction vector of the endless line. Does not need to be unitized.</param>
        /// <param name="testPnt">The point to be projected onto the line.</param>
        /// <returns>The parameter (scaling factor for the direction vector) at which the projection of testPnt falls on the line.</returns>
        static member inline projectedParameterOnVec (fromPnt:Pnt) (v:Vec) (testPnt:Pnt) : float =
            let lenSq = v.LengthSq
            if isTooTinySq(lenSq) then failTooSmall "Pnt.projectedParameterOnVec" v
            (v.X * (testPnt.X - fromPnt.X) + v.Y * (testPnt.Y - fromPnt.Y) + v.Z * (testPnt.Z - fromPnt.Z)) / lenSq

        /// <summary>
        /// Projects a test point onto an endless line defined by two points.
        /// </summary>
        /// <param name="fromPnt">The start point defining the endless line.</param>
        /// <param name="toPnt">The end point defining the direction of the endless line.</param>
        /// <param name="testPnt">The point to be projected onto the line.</param>
        /// <returns>The parameter along the line direction (fromPnt to toPnt) at which the projection of testPnt falls.
        /// A value of 0.0 corresponds to fromPnt, 1.0 corresponds to toPnt.</returns>
        static member inline projectedParameterBetweenPnts (fromPnt:Pnt) (toPnt:Pnt) (testPnt:Pnt) : float =
            let vx = toPnt.X - fromPnt.X
            let vy = toPnt.Y - fromPnt.Y
            let vz = toPnt.Z - fromPnt.Z
            let lenSq = vx*vx + vy*vy + vz*vz
            if isTooTinySq(lenSq) then failTooClose "Pnt.projectedParameterBetweenPnts" fromPnt toPnt
            (vx * (testPnt.X - fromPnt.X) + vy * (testPnt.Y - fromPnt.Y) + vz * (testPnt.Z - fromPnt.Z)) / lenSq

        /// Returns the closer point of the two points to the reference given point.
        /// When both points are equally close, the first point is returned.
        static member closestOfTwo (pnt1:Pnt) (pnt2:Pnt) (referencePoint:Pnt) : Pnt =
            let d1 = Pnt.distanceToSq pnt1 referencePoint
            let d2 = Pnt.distanceToSq pnt2 referencePoint
            if d1 <= d2 then
                pnt1
            else
                pnt2

        // #endregion
