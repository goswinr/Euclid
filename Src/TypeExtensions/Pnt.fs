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
        member inline p.IsInValid =
            isNanInfinity p.X || isNanInfinity p.Y || isNanInfinity p.Z

        /// Returns a boolean indicating whether X, Y and Z are valid (not NaN or Infinity).
        member inline p.IsValid : bool =
            not p.IsInValid

        /// Returns the 3D point as 3D vector.
        member inline p.AsVec =
            Vec(p.X, p.Y, p.Z)

        /// Returns the 3D point as 2D point.
        member inline p.AsPt =
            Pt(p.X, p.Y)

        /// Returns a boolean indicating whether X, Y and Z are exactly 0.0.
        member inline pt.IsOrigin =
            pt.X = 0.0 && pt.Y = 0.0 && pt.Z= 0.0

        /// Returns a boolean indicating if any of X, Y and Z is not exactly 0.0.
        member inline p.IsNotOrigin =
            p.X <> 0.0 || p.Y <> 0.0 || p.Z <> 0.0

        /// Returns a boolean indicating whether the absolute value of X, Y and Z is each less than the given tolerance.
        member inline pt.IsAlmostOrigin tol =
            abs pt.X < tol && abs pt.Y < tol && abs pt.Z < tol

        /// Returns new 3D point with new X coordinate, Y and Z stay the same.
        member inline pt.WithX x =
            Pnt (x, pt.Y, pt.Z)

        /// Returns a new 3D point with new Y coordinate, X and Z stay the same.
        member inline pt.WithY y =
            Pnt (pt.X, y, pt.Z)

        /// Returns a new 3D point with new Z coordinate, X and Y stay the same.
        member inline pt.WithZ z =
            Pnt (pt.X, pt.Y, z)

        /// Returns the distance between two 3D points.
        member inline p.DistanceTo (b:Pnt) =
            let x = p.X-b.X
            let y = p.Y-b.Y
            let z = p.Z-b.Z
            sqrt(x*x + y*y + z*z)

        [<Obsolete("Use SqDistanceTo instead.")>]
        member inline p.DistanceToSquare (b:Pnt) =
            let x = p.X-b.X
            let y = p.Y-b.Y
            let z = p.Z-b.Z
            x*x + y*y + z*z

        /// Returns the squared distance between two 3D points.
        /// This operation is slightly faster than the distance function, and sufficient for many algorithms like finding closest points.
        member inline p.SqDistanceTo (b:Pnt) =
            let x = p.X-b.X
            let y = p.Y-b.Y
            let z = p.Z-b.Z
            x*x + y*y + z*z

        /// Returns the distance from Origin (0, 0, 0)
        member inline pt.DistanceFromOrigin =
            sqrt (pt.X*pt.X + pt.Y*pt.Y + pt.Z*pt.Z)

        [<Obsolete("Use SqDistanceFromOrigin instead.")>]
        member inline pt.DistanceFromOriginSquare =
            pt.X*pt.X + pt.Y*pt.Y + pt.Z*pt.Z

        /// Returns the squared distance from Origin (0, 0, 0)
        member inline pt.SqDistanceFromOrigin =
            pt.X*pt.X + pt.Y*pt.Y + pt.Z*pt.Z

        /// Returns the projected distance from Origin (0, 0, 0). Ignoring the Z component.
        member inline pt.DistanceInXYFromOrigin =
            sqrt (pt.X*pt.X + pt.Y*pt.Y)

        /// Returns the projected squared distance from Origin (0, 0, 0). Ignoring the Z component.
        member inline pt.DistanceInXYFromOriginSquare =
            pt.X*pt.X + pt.Y*pt.Y

        /// Returns new 3D point with given distance from Origin by scaling it up or down.
        member inline pt.WithDistanceFromOrigin (l:float) =
            let d = pt.DistanceFromOrigin
            if isTooTiny d then failTooSmall "Pnt.WithDistanceFromOrigin" pt
            pt * (l/d)

        /// Returns the Diamond Angle from this point to another point projected in X-Y plane.
        /// The diamond angle is always positive and in the range of 0.0 to 4.0 (for 360 Degrees)
        /// 0.0 = Xaxis, going Counter-Clockwise. Ignoring Z component.
        /// This is the fastest angle computation since it does not use Math.Cos or Math.Sin.
        /// It is useful for radial sorting.
        member inline p.DirectionDiamondInXYTo(o:Pnt) =
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

        /// Returns the angle in Radians from this point to another point projected in X-Y plane.
        /// 0.0 = Xaxis, going Counter-Clockwise till two Pi.
        member inline p.Angle2PiInXYTo(o:Pnt) =
            // https://stackoverflow.com/a/14675998/969070
            let x = o.X-p.X
            let y = o.Y-p.Y
            if isTooTiny (abs x + abs y) then failTooSmall "Pnt.Angle2PiInXYTo" p
            let a = Math.Atan2(y, x)
            if a < 0. then  a + UtilEuclid.twoPi
            else            a

        /// Returns the angle in Degrees from this point to another point projected in X-Y plane.
        /// 0.0 = Xaxis, going Counter-Clockwise till 360.
        member inline p.Angle360InXYTo(o:Pnt) =
            p.Angle2PiInXYTo o |> toDegrees

        /// Get closest point on finite line to test point.
        member inline testPt.ClosestPointOnLine(fromPt:Pnt, toPt:Pnt) =
            let dir = testPt - fromPt
            let v   = toPt   - fromPt
            let lenSq = v.LengthSq
            if isTooTinySq lenSq then failTooClose "Pnt.ClosestPointOnLine" fromPt toPt
            let dot = Vec.dot (v, dir) / lenSq
            if   dot <= 0.0 then  fromPt
            elif dot >= 1.0 then  toPt
            else                 fromPt+dot*v

        /// Get closest point on finite line to test point.
        member inline testPt.ClosestPointOnLine(fromPt:Pnt, uv:UnitVec, len:float) =
            let dir = testPt-fromPt
            let dot = Vec.dot (uv, dir)
            if   dot <= 0.0 then  fromPt
            elif dot >= len then (fromPt+len*uv)
            else                 fromPt+dot*uv

        /// Returns the squared distance between point and finite line segment defined by
        /// start point, direction and length.
        member inline testPt.SqDistanceToLine(fromPt:Pnt, uv:UnitVec, len:float) =
            let dir = testPt-fromPt
            let dot = Vec.dot (uv, dir)
            if   dot <= 0.0 then testPt.SqDistanceTo  fromPt
            elif dot >= len then testPt.SqDistanceTo (fromPt+len*uv)
            else                 testPt.SqDistanceTo (fromPt+dot*uv)

        [<Obsolete("Use SqDistanceToLine instead.")>]
        member inline testPt.DistanceToLineSquare(fromPt:Pnt, uv:UnitVec, len:float) =
            testPt.SqDistanceToLine(fromPt, uv, len)

        /// Returns the squared distance between point and finite line segment defined by
        /// start point, end point, direction and length.
        /// The last two parameters help speed up calculations.
        member inline testPt.SqDistanceToLine(fromPt:Pnt, toPt:Pnt, uv:UnitVec, len:float) =
            let dir = testPt-fromPt
            let dot = Vec.dot (uv, dir)
            if   dot <= 0.0 then testPt.SqDistanceTo fromPt
            elif dot >= len then testPt.SqDistanceTo toPt
            else                 testPt.SqDistanceTo (fromPt+dot*uv)

        [<Obsolete("Use SqDistanceToLine instead.")>]
        member inline testPt.DistanceToLineSquare(fromPt:Pnt, toPt:Pnt, uv:UnitVec, len:float) =
            testPt.SqDistanceToLine(fromPt, toPt, uv, len)

        /// Returns the distance between point and finite line segment defined by
        /// start point, direction and length.
        member inline testPt.DistanceToLine(fromPt:Pnt, uv:UnitVec, len:float) =
            let dir = testPt-fromPt
            let dot = Vec.dot (uv, dir)
            if   dot <= 0.0 then testPt.DistanceTo  fromPt
            elif dot >= len then testPt.DistanceTo (fromPt+len*uv)
            else                 testPt.DistanceTo (fromPt+dot*uv)

        /// Returns the distance between point and finite line segment defined by start and end.
        member inline testPt.DistanceToLine(fromPt:Pnt, toPt:Pnt) =
            let dir = testPt - fromPt
            let v   = toPt   - fromPt
            let lenSq = v.LengthSq
            if isTooTinySq(lenSq) then failTooClose "Pnt.DistanceToLine" fromPt toPt
            let dot = Vec.dot (v, dir) / v.LengthSq
            if   dot <= 0.0 then testPt.DistanceTo   fromPt
            elif dot >= 1.0 then testPt.DistanceTo   toPt
            else                 testPt.DistanceTo   (fromPt + v * dot)

        /// Multiplies (or applies) a Matrix to a 3D point (with an implicit 1 in the 4th dimension,
        /// so that it also works correctly for projections.)
        member inline p.Transform (m:Matrix) =
            p *** m // operator * is defined in Matrix.fs

        /// Multiplies (or applies) a RigidMatrix to a 3D point.
        member inline p.TransformRigid (m:RigidMatrix) =
            p *** m // operator * is defined in RigidMatrix.fs

        /// Multiplies (or applies) only the 3x3 rotation part of a RigidMatrix to a 3D point.
        member inline p.TransformRigidRotateOnly (m:RigidMatrix) =
            let x = p.X
            let y = p.Y
            let z = p.Z
            Pnt ( m.M11*x + m.M21*y + m.M31*z
                , m.M12*x + m.M22*y + m.M32*z
                , m.M13*x + m.M23*y + m.M33*z
                )


        // ----------------------------------------------------------------------------------
        //            █████               █████     ███
        //           ░░███               ░░███     ░░░
        //    █████  ███████    ██████   ███████   ████   ██████
        //   ███░░  ░░░███░    ░░░░░███ ░░░███░   ░░███  ███░░███
        //  ░░█████   ░███      ███████   ░███     ░███ ░███ ░░░
        //   ░░░░███  ░███ ███ ███░░███   ░███ ███ ░███ ░███  ███
        //   ██████   ░░█████ ░░████████  ░░█████  █████░░██████
        //  ░░░░░░     ░░░░░   ░░░░░░░░    ░░░░░  ░░░░░  ░░░░░░
        //
        //                                             █████
        //                                            ░░███
        //    █████████████    ██████  █████████████   ░███████   ██████  ████████   █████
        //   ░░███░░███░░███  ███░░███░░███░░███░░███  ░███░░███ ███░░███░░███░░███ ███░░
        //    ░███ ░███ ░███ ░███████  ░███ ░███ ░███  ░███ ░███░███████  ░███ ░░░ ░░█████
        //    ░███ ░███ ░███ ░███░░░   ░███ ░███ ░███  ░███ ░███░███░░░   ░███      ░░░░███
        //    █████░███ █████░░██████  █████░███ █████ ████████ ░░██████  █████     ██████
        //   ░░░░░ ░░░ ░░░░░  ░░░░░░  ░░░░░ ░░░ ░░░░░ ░░░░░░░░   ░░░░░░  ░░░░░     ░░░░░░
        // ------------------------------------------------------------------------------------



        /// Checks if two 3D points are equal within tolerance.
        /// Use a tolerance of 0.0 to check for an exact match.
        static member inline equals (tol:float) (a:Pnt) (b:Pnt) =
            abs (a.X-b.X) <= tol &&
            abs (a.Y-b.Y) <= tol &&
            abs (a.Z-b.Z) <= tol

        /// Check if two 3D points are not equal within a given tolerance.
        /// Use a tolerance of 0.0 to check if the two points are not exactly equal.
        static member notEquals (tol:float) (a:Pnt) (b:Pnt) =
            abs (a.X-b.X) > tol ||
            abs (a.Y-b.Y) > tol ||
            abs (a.Z-b.Z) > tol


        /// Accepts any type that has a X, Y and Z (UPPERCASE) member that can be converted to a float.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline createFromMembersXYZ pt =
            let x = ( ^T : (member X : _) pt)
            let y = ( ^T : (member Y : _) pt)
            let z = ( ^T : (member Z : _) pt)
            try
                Pnt(float x, float y, float z)
            with e ->
                fail2 "Pnt.createFromMembersXYZ" pt e |> unbox // unbox to make type checker happy

        /// Accepts any type that has a x, y and z (lowercase) member that can be converted to a float.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline createFromMembersxyz pt =
            let x = ( ^T : (member x : _) pt)
            let y = ( ^T : (member y : _) pt)
            let z = ( ^T : (member z : _) pt)
            try
                Pnt(float x, float y, float z)
            with e ->
                fail2 "Pnt.createFromMembersxyz" pt e |> unbox // unbox to make type checker happy

        /// Create 3D point from 2D point. Using 0.0 for Z
        static member inline createFromPt (p:Pt) = Pnt (p.X, p.Y, 0.0)

        /// Create 3D point from 3D vector.
        static member inline createFromVec (v:Vec) = Pnt (v.X, v.Y, v.Z)

        /// Create 3D point from 3D unit-vector.
        static member inline createFromUnitVec (v:UnitVec) = Pnt (v.X, v.Y, v.Z)

        /// Create 3D point from X, Y and Z components.
        static member inline create (x:float, y:float, z:float) = Pnt(x, y, z)

        /// Returns a 3D point from Z level and 2D point.
        static member inline createFromPtWithZ (z:float) (p:Pt) = Pnt (p.X, p.Y, z)

        /// Project point to World X-Y plane.
        /// Use make2D to convert to 2D point instance.
        static member inline projectToXYPlane (pt:Pnt) = Pnt(pt.X, pt.Y, 0.0)

        /// Sets the X value and return new 3D point.
        static member inline withX x (pt:Pnt) = Pnt(x, pt.Y, pt.Z)

        /// Sets the Y value and return new 3D point.
        static member inline withY y (pt:Pnt) = Pnt(pt.X, y, pt.Z)

        /// Sets the Z value and return new 3D point.
        static member inline withZ z (pt:Pnt) = Pnt(pt.X, pt.Y, z)

        /// Gets the X value of 3D point.
        static member inline getX (pt:Pnt) = pt.X

        /// Gets the Y value of 3D point.
        static member inline getY (pt:Pnt) = pt.Y

        /// Gets the Z value of 3D point.
        static member inline getZ (pt:Pnt) = pt.Z

        /// Adds two 3D points and return new 3D point.
        static member inline add (a:Pnt) (b:Pnt) = a + b

        /// Add a 3D point to a 3D vector and return new 3D point.
        static member inline addVec (v:Vec) (a:Pnt) = a + v

        /// Returns the midpoint of two 3D points.
        static member inline midPt (a:Pnt) (b:Pnt) = (a+b) * 0.5

        /// Scale a 3D point by a scalar and return new 3D point.
        static member inline scale (f:float) (pt:Pnt) = pt*f

        /// Move point 3D by vector. Same as Pnt.move.
        static member inline translate (shift:Vec) (pt:Pnt) =
            pt + shift

        /// Move point 3D by vector. Same as Pnt.translate.
        static member inline move (shift:Vec) (pt:Pnt) =
            pt + shift

        /// Add float to X component of a 3D point and return new 3D point.
        static member inline moveX (x:float) (pt:Pnt) = Pnt (pt.X+x, pt.Y, pt.Z)

        /// Add float to Y component of a 3D point and return new 3D point.
        static member inline moveY (y:float) (pt:Pnt) = Pnt (pt.X, pt.Y+y, pt.Z)

        /// Add float to Z component of a 3D point and return new 3D point.
        static member inline moveZ (z:float) (pt:Pnt) = Pnt (pt.X, pt.Y, pt.Z+z)

        /// Returns the distance between two 3D points.
        static member inline distance (a:Pnt) (b:Pnt) =
            let x = a.X-b.X
            let y = a.Y-b.Y
            let z = a.Z-b.Z
            sqrt(x*x + y*y + z*z)

        /// Returns the horizontal distance between two 3D points(ignoring their Z Value)
        static member inline distanceXY (a:Pnt) (b:Pnt) =
            let x = a.X-b.X
            let y = a.Y-b.Y
            sqrt(x*x + y*y)

        /// Returns the squared distance between two 3D points.
        /// This operation is slightly faster than the Pnt.distance function, and sufficient for many algorithms like finding closest points.
        static member inline distanceSq (a:Pnt) (b:Pnt) =
            let x = a.X-b.X
            let y = a.Y-b.Y
            let z = a.Z-b.Z
            x*x + y*y + z*z

        /// Returns the distance from World Origin.
        static member inline distanceFromOrigin (pt:Pnt) = pt.DistanceFromOrigin

        [<Obsolete("Use sqDistanceFromOrigin instead.")>]
        static member inline distanceFromOriginSquare (pt:Pnt) = pt.SqDistanceFromOrigin

        /// Returns the square distance from World Origin.
        static member inline sqDistanceFromOrigin (pt:Pnt) = pt.SqDistanceFromOrigin

        /// Returns a new 3D point at a given distance from World Origin by scaling the input.
        static member inline setDistanceFromOrigin f (pt:Pnt) = pt.WithDistanceFromOrigin f

        /// Returns angle between three 3D Points in Radians. Range 0.0 to Pi.
        static member inline anglePiPts (ptPrev:Pnt, ptThis:Pnt, ptNext:Pnt) =
            Vec.anglePi (ptPrev-ptThis) (ptNext-ptThis)

        /// Returns angle between three 3D Points in Degrees. Range 0.0 to 180
        static member inline angle180Pts (ptPrev:Pnt, ptThis:Pnt, ptNext:Pnt) =
            Pnt.anglePiPts (ptPrev, ptThis, ptNext) |> toDegrees

        /// Returns a (not unitized) bisector vector in the middle direction from ptThis.
        /// Code : (ptPrev-ptThis).Unitized  + (ptNext-ptThis).Unitized
        /// ptPrev * ptThis * ptNext ->   bisector vector
        static member inline bisector (ptPrev:Pnt, ptThis:Pnt, ptNext:Pnt) =
            (ptPrev-ptThis).Unitized  + (ptNext-ptThis).Unitized

        /// For three Points describing a plane return a normal.
        /// If the returned vector has length zero then the points are in one line.
        static member normalOf3Pts (a:Pnt, b:Pnt, c:Pnt) = Vec.cross (a-b, c-b)

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
        static member inline divPt(fromPt:Pnt, toPt:Pnt, rel:float) : Pnt =
            Pnt(fromPt.X + (toPt.X-fromPt.X)*rel,
                fromPt.Y + (toPt.Y-fromPt.Y)*rel,
                fromPt.Z + (toPt.Z-fromPt.Z)*rel)


        /// Linearly interpolates between two 3D points.
        /// e.g. rel=0.5 will return the middle point, rel=1.0 the endPoint,
        /// rel=3.0 a point three times the distance beyond the end point.
        /// Same as Pnt.divPt.
        static member inline lerp(fromPt:Pnt, toPt:Pnt, rel:float) : Pnt =
            Pnt.divPt(fromPt, toPt, rel)


        /// Returns a point that is at a given Z level,
        /// going from a point in the direction of another point.
        static member inline extendToZLevel (fromPt:Pnt, toPt:Pnt, z:float) =
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
        static member inline snapIfClose (snapDistance) (snapTo:Pnt) (pt:Pnt) =
            let v = snapTo-pt
            if v.Length < snapDistance then snapTo else pt

        /// Snaps the points coordinate to the given precision.
        /// e.g. snap 0.1 Pnt(0.123, 0.456, 0) -> Pnt(0.1, 0.5, 0)
        /// e.g. snap 10  Pnt(3    , 19   , 0) -> Pnt(0  , 20 , 0)
        /// does: (Math.Round (x/precision)) * precision
        static member inline snap (precision) (pt:Pnt) =
            if isTooTiny (precision) then fail $"Pnt.snap: precision too small or negative:{precision}"
            Pnt( (Math.Round (pt.X/precision)) * precision,
                 (Math.Round (pt.Y/precision)) * precision,
                 (Math.Round (pt.Z/precision)) * precision)

        /// Every line has a normal vector in X-Y plane.
        /// Rotated Counter-Clockwise in top view.
        /// The result is unitized.
        /// If line is vertical then Xaxis is returned.
        /// see also : Vec.perpendicularVecInXY.
        static member normalOfTwoPointsInXY(fromPt:Pnt, toPt:Pnt) =
            let x = toPt.Y - fromPt.Y
            let y = fromPt.X - toPt.X  // this is the same as: Vec.cross v Vec.Zaxis
            let len = sqrt(x*x + y*y)
            if isTooTiny len then Vec.Xaxis
            else Vec(x/len, y/len, 0.0)


        [<Obsolete("Use Line2D.offset instead")>]
        static member offsetTwoPt(  fromPt:Pnt,
                                    toPt:Pnt,
                                    distHor:float,
                                    distNormal:float) : Pnt*Pnt=
            let v = toPt - fromPt
            let normHor =
                Vec.cross(v, Vec.Zaxis)
                |> Vec.unitizeOrDefault UnitVec.Xaxis

            let normFree =
                Vec.cross(v, normHor)
                |> Vec.unitizeOrDefault UnitVec.Zaxis

            let shift = distHor * normHor + distNormal * normFree
            fromPt +  shift, toPt + shift


        /// Multiplies (or applies) a Matrix to a 3D point (with an implicit 1 in the 4th dimension,
        /// so that it also works correctly for projections.)
        static member inline transform (m:Matrix) (p:Pnt) =
            p.Transform m

        /// Multiplies (or applies) a RigidMatrix to a 3D point.
        static member inline transformRigid (m:RigidMatrix) (p:Pnt) =
            p.TransformRigid m

        // --------------- Rotate 2D and 3D: ----------------------------

        /// Multiplies (or applies) only the 3x3 rotation part of a RigidMatrix to a 3D point.
        static member transformRigidRotateOnly (m:RigidMatrix) (p:Pnt) =
            p.TransformRigidRotateOnly m

        /// Rotate the 3D point around X-axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member rotateXBy (r:Rotation2D) (p:Pnt) = Pnt (p.X, r.Cos*p.Y - r.Sin*p.Z, r.Sin*p.Y + r.Cos*p.Z)

        /// Rotate the 3D point around Y-axis, from Z to X-axis, Counter Clockwise looking from back.
        static member rotateYBy (r:Rotation2D) (p:Pnt) = Pnt (r.Sin*p.Z + r.Cos*p.X, p.Y, r.Cos*p.Z - r.Sin*p.X)

        /// Rotate the 3D point around Z-axis, from X to Y-axis, Counter Clockwise looking from top.
        static member rotateZBy (r:Rotation2D) (p:Pnt) = Pnt (r.Cos*p.X - r.Sin*p.Y, r.Sin*p.X + r.Cos*p.Y, p.Z)

        /// Rotate the 3D point around a center 3D point and a X aligned axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member rotateXwithCenterBy (cen:Pnt) (r:Rotation2D) (pt:Pnt) =
            let x = pt.X - cen.X
            let y = pt.Y - cen.Y
            let z = pt.Z - cen.Z
            Pnt  (  x                 + cen.X,
                    r.Cos*y - r.Sin*z + cen.Y,
                    r.Sin*y + r.Cos*z + cen.Z)

        /// Rotate the 3D point around a center point and a Y aligned axis, from Z to X-axis, Counter Clockwise looking from back.
        static member rotateYwithCenterBy (cen:Pnt) (r:Rotation2D) (pt:Pnt) =
            let x = pt.X - cen.X
            let y = pt.Y - cen.Y
            let z = pt.Z - cen.Z
            Pnt (   r.Sin*z + r.Cos*x + cen.X,
                    y                 + cen.Y,
                    r.Cos*z - r.Sin*x + cen.Z)

        /// Rotate the 3D point around a center point and a Z aligned axis, from X to Y-axis, Counter Clockwise looking from top.
        static member rotateZwithCenterBy (cen:Pnt) (r:Rotation2D) (pt:Pnt) =
            let x = pt.X - cen.X
            let y = pt.Y - cen.Y
            let z = pt.Z - cen.Z
            Pnt (   r.Cos*x - r.Sin*y + cen.X,
                    r.Sin*x + r.Cos*y + cen.Y,
                    z                 + cen.Z)

        /// Rotate the 3D point in Degrees around X-axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member inline rotateX (angDegree) (pt:Pnt) =
            Pnt.rotateXBy (Rotation2D.createFromDegrees angDegree) pt

        /// Rotate the 3D point in Degrees around Y-axis, from Z to X-axis, Counter Clockwise looking from back.
        static member inline rotateY (angDegree) (pt:Pnt) =
            Pnt.rotateYBy (Rotation2D.createFromDegrees angDegree) pt

        /// Rotate the 3D point in Degrees around Z-axis, from X to Y-axis, Counter Clockwise looking from top.
        static member inline rotateZ (angDegree) (pt:Pnt) =
            Pnt.rotateZBy (Rotation2D.createFromDegrees angDegree) pt

        /// Rotate the 3D point in Degrees around center point and a X aligned axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member inline rotateXwithCenter (cen:Pnt) (angDegree) (pt:Pnt) =
            Pnt.rotateXwithCenterBy cen (Rotation2D.createFromDegrees angDegree) pt

        /// Rotate the 3D point in Degrees around center point and a Y aligned axis, from Z to X-axis, Counter Clockwise looking from back.
        static member inline rotateYwithCenter (cen:Pnt) (angDegree) (pt:Pnt) =
            Pnt.rotateYwithCenterBy cen (Rotation2D.createFromDegrees angDegree) pt

        /// Rotate the 3D point in Degrees around center point and a Z aligned axis, from X to Y-axis, Counter Clockwise looking from top.
        static member inline rotateZwithCenter (cen:Pnt) (angDegree) (pt:Pnt) =
            Pnt.rotateZwithCenterBy cen (Rotation2D.createFromDegrees angDegree) pt

        /// Rotate by Quaternion around Origin
        static member inline rotateByQuaternion (q:Quaternion) (pt:Pnt) =
            pt *** q  // operator * is defined in Quaternion.fs

        /// Rotate by Quaternion around given Center point.
        static member inline rotateWithCenterByQuat (cen:Pnt) (q:Quaternion) (pt:Pnt) =
            // adapted from https://github.com/mrdoob/three.js/blob/dev/src/math/Vector3.js
            let x = pt.X-cen.X
            let y = pt.Y-cen.Y
            let z = pt.Z-cen.Z
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

        /// Returns angle in Degrees at mid point (thisPt).
        static member angleInCorner(prevPt:Pnt, thisPt:Pnt, nextPt:Pnt) =
            let a = prevPt-thisPt
            let b = nextPt-thisPt
            Vec.angle180 a b

        /// 'fromPt' point and uv unit-vector describe an endless line.
        /// testPt gets projected on to this line.
        /// Returns the parameter (or scaling for unit-vector) on this line of the projection.
        static member inline projectedParameter (fromPt:Pnt, uv:UnitVec, testPt:Pnt) =
            let dir = testPt-fromPt
            Vec.dot (dir, uv)

        /// 'fromPt' point and 'v' vector describe an endless 3D line.
        /// 'testPt' gets projected onto this line.
        /// Returns the parameter (or scaling for vector) on this line of the projection.
        static member inline projectedParameter (fromPt:Pnt, v:Vec, testPt:Pnt) =
            let dir = testPt-fromPt
            let lenSq = v.LengthSq
            if isTooTinySq(lenSq) then failTooSmall "Pnt.projectedParameter" v
            Vec.dot (v, dir) / lenSq

        /// 'fromPt' point and 'toPt' point describe an endless 3D line.
        /// 'testPt' gets projected onto this line.
        /// Returns the parameter (or scaling for vector) on this line of the projection.
        static member inline projectedParameter (fromPt:Pnt, toPt:Pnt, testPt:Pnt) =
            let dir = testPt - fromPt
            let v   = toPt   - fromPt
            let lenSq = v.LengthSq
            if isTooTinySq(lenSq) then failTooClose "Pnt.projectedParameter" fromPt toPt
            Vec.dot (v, dir) / lenSq



        /// Returns the closer point of the two points to the reference given point.
        /// When both points are equally close, the first point is returned.
        static member closestOfTwo (pt1:Pnt) (pt2:Pnt) (referencePoint:Pnt) =
            let d1 = Pnt.distanceSq pt1 referencePoint
            let d2 = Pnt.distanceSq pt2 referencePoint
            if d1 <= d2 then
                pt1
            else
                pt2