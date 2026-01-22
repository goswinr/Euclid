namespace Euclid

open System
open EuclidErrors


/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type Pt.
[<AutoOpen>]
module AutoOpenPt =
    open UtilEuclid

    type Pt with

        /// Returns a boolean indicating whether X or Y is NaN or Infinity.
        member inline p.IsInValid =
            isNanInfinity p.X || isNanInfinity p.Y

        /// Returns a boolean indicating whether X and Y are both valid (not NaN or Infinity).
        member inline p.IsValid : bool =
            not p.IsInValid

        /// Returns the 2D point as 2D vector.
        member inline p.AsVc =
            Vc(p.X, p.Y)

        /// Returns the 2D point as 3D vector. Using 0.0 for Z.
        member inline p.AsVec =
            Vec(p.X, p.Y, 0.0)

        /// Returns the 2D point as 3D point. Using 0.0 for Z.
        /// Use the member pt.WithZ to set Z to a different value.
        member inline p.AsPnt =
            Pnt(p.X, p.Y, 0.0)

        /// Returns a boolean indicating whether X and Y are exactly 0.0.
        member inline pt.IsOrigin =
            pt.X = 0.0 && pt.Y = 0.0

        /// Returns a boolean indicating if any of X and Y is not exactly 0.0.
        member inline p.IsNotOrigin =
            p.X <> 0.0 || p.Y <> 0.0

        /// Returns a boolean indicating whether the absolute value of X and Y is each less than the given tolerance.
        member inline pt.IsAlmostOrigin tol =
            abs pt.X < tol && abs pt.Y < tol

        /// Returns new 2D point with new X coordinate, Y stays the same.
        member inline pt.WithX x =
            Pt (x, pt.Y)

        /// Returns new 2D point with new Y coordinate, X stays the same.
        member inline pt.WithY y =
            Pt (pt.X, y)

        /// Returns new 3D point with Z coordinate, X and Y stay the same.
        /// If you want Z to be 0.0 you can use pt.AsPnt too.
        member inline pt.WithZ z =
            Pnt (pt.X, pt.Y, z)

        /// Returns the distance between two 2D points.
        member inline p.DistanceTo (b:Pt) =
            let x = p.X-b.X
            let y = p.Y-b.Y
            sqrt(x*x + y*y)


        /// Returns the squared distance between two 2D points.
        /// This operation is slightly faster than the distance function, and sufficient for many algorithms like finding closest points.
        member inline p.SqDistanceTo (b:Pt) =
            let x = p.X-b.X
            let y = p.Y-b.Y
            x*x + y*y

        /// Returns the distance from Origin (0, 0)
        member inline pt.DistanceFromOrigin =
            sqrt (pt.X*pt.X + pt.Y*pt.Y)

        /// Returns the squared distance from Origin (0, 0)
        member inline pt.SqDistanceFromOrigin =
            pt.X*pt.X + pt.Y*pt.Y


        /// Returns new 2D point with given distance from Origin by scaling it up or down.
        member inline pt.WithDistanceFromOrigin (l:float) =
            let d = pt.DistanceFromOrigin
            if isTooTiny d then failTooSmall "Pt.WithDistanceFromOrigin" pt
            pt * (l/d)


        /// Returns the Diamond Angle from this point to another point.
        /// Calculates the proportion of X to Y component.
        /// It is always positive and in the range of 0.0 to 4.0 (for 360 Degrees)
        /// 0.0 = Xaxis, going Counter-Clockwise.
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        /// Fails if the two points are coincident or too close together.
        member inline p.DirectionDiamondTo(o:Pt) =
            // https://stackoverflow.com/a/14675998/969070
            let x = o.X-p.X
            let y = o.Y-p.Y
            if isTooTiny (abs(x) + abs(y)) then failTooSmall "Pt.DirectionDiamondTo" p
            if y >= 0.0 then
                if x >= 0.0 then
                    y/(x+y)
                else
                    1.0 - x/(-x+y)
            else
                if x < 0.0 then
                    2.0 - y/(-x-y)
                else
                    3.0 + x/(x-y)


        /// Returns the angle in Radians from this point to another point.
        /// 0.0 = Xaxis, going Counter-Clockwise till two Pi.
        /// Fails if the two points are coincident or too close together.
        member inline p.Angle2PiTo(o:Pt) =
            // https://stackoverflow.com/a/14675998/969070
            let x = o.X-p.X
            let y = o.Y-p.Y
            if isTooTiny (abs(x) + abs(y)) then failTooSmall "Pt.Angle2PiTo" p
            let a = Math.Atan2(y, x)
            if a < 0. then  a + UtilEuclid.twoPi
            else            a

        /// Returns the angle in Degrees from this point to another point.
        /// 0.0 = Xaxis, going Counter-Clockwise till 360.
        /// Fails if the two points are coincident or too close together.
        member inline p.Angle360To(o:Pt) =
            p.Angle2PiTo o |> toDegrees

        /// Returns the closest point on a finite line segment to test point.
        /// The line segment is defined by start point 'fromPt' and end point 'toPt'.
        /// Fails if fromPt and toPt are coincident or too close together.
        member inline testPt.ClosestPointOnLine(ln:Line2D) =
            XLine2D.clPtLn(ln.FromX, ln.FromY, ln.VectorX, ln.VectorY, testPt.X, testPt.Y)

        /// Returns the squared distance between point and finite line segment.
        /// The line segment is defined by start point 'fromPt', unit direction 'uv', and length 'len'.
        member inline testPt.SqDistanceToLine(ln:Line2D) =
            XLine2D.sqDistLnPt(ln.FromX, ln.FromY, ln.VectorX, ln.VectorY, testPt.X, testPt.Y)


        /// Returns the distance between point and finite line segment.
        /// The line segment is defined by start point 'fromPt', unit direction 'uv', and length 'len'.
        member inline testPt.DistanceToLine(ln:Line2D) =
            testPt.SqDistanceToLine ln |> sqrt



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
        // --------------------------------------------------------------------------------

        /// Checks if two 2D points are equal within tolerance.
        /// Use a tolerance of 0.0 to check for an exact match.
        static member inline equals (tol:float) (a:Pt) (b:Pt) =
            abs (a.X-b.X) <= tol &&
            abs (a.Y-b.Y) <= tol

        /// Check if two 2D points are not equal within a given tolerance.
        /// Use a tolerance of 0.0 to check if the two points are not exactly equal.
        static member notEquals (tol:float) (a:Pt) (b:Pt) =
            abs (a.X-b.X) > tol ||
            abs (a.Y-b.Y) > tol

        /// Accepts any type that has X and Y (UPPERCASE) members that can be converted to float values.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline createFromMembersXY pt =
            let x = ( ^T : (member X: _) pt)
            let y = ( ^T : (member Y: _) pt)
            try
                Pt(float x, float y)
            with e ->
                fail2 "Pt.createFromMembersXY" pt e |> unbox // unbox to make type checker happy


        /// Accepts any type that has x and y (lowercase) members that can be converted to float values.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline createFromMembersxy pt =
            let x = ( ^T : (member x: _) pt)
            let y = ( ^T : (member y: _) pt)
            try
                Pt(float x, float y)
            with e ->
                fail2 "Pt.createFromMembersxy" pt e |> unbox // unbox to make type checker happy


        /// Create 2D point from 3D point. Ignoring Z component.
        static member inline createFromPnt (p:Pnt) =
            Pt (p.X, p.Y)

        /// Create 2D point from 2D vector.
        static member inline createFromVc (v:Vc) =
            Pt (v.X, v.Y)

        /// Create 2D point from 2D unit-vector.
        static member inline createFromUnitVc (v:UnitVc) =
            Pt (v.X, v.Y)

        /// Create a 2D point from X and Y components.
        static member inline create (x:float, y:float) =
            Pt(x, y)

        /// Sets the X value and returns new 2D point.
        static member inline withX x (pt:Pt) =
            Pt(x, pt.Y)

        /// Sets the Y value and returns new 2D point.
        static member inline withY y (pt:Pt) =
            Pt(pt.X, y)

        /// Gets the X value of 2D point.
        static member inline getX (pt:Pt) =
            pt.X

        /// Gets the Y value of 2D point.
        static member inline getY (pt:Pt) =
            pt.Y

        /// Adds two 2D points. Returns a new 2D point.
        static member inline add (a:Pt) (b:Pt) =
            a + b

        /// Add a 2D point to a 2D vector. Returns a new 2D point.
        static member inline addVc (v:Vc) (a:Pt) =
            a + v

        /// Returns the midpoint of two 2D points.
        static member inline midPt (a:Pt) (b:Pt) =
            (a+b) * 0.5

        /// Scales a 2D point by a factor. Returns a new 2D point.
        static member inline scale (f:float) (pt:Pt) =
            pt*f

        /// Move a 2D point by a vector. Same as Pt.move.
        static member inline translate (shift:Vc) (pt:Pt) =
            pt + shift

        /// Move a 2D point by a vector. Same as Pt.translate.
        static member inline move (shift:Vc) (pt:Pt) =
            pt + shift

        /// Add a float to X component of a 2D point. Returns a new 2D point.
        static member inline moveX (x:float) (pt:Pt) =
            Pt (pt.X+x, pt.Y)

        /// Add a float to Y component of a 2D point. Returns a new 2D point.
        static member inline moveY (y:float) (pt:Pt) =
            Pt (pt.X, pt.Y+y)

        /// Returns the distance between two 2D points.
        static member inline distance (a:Pt) (b:Pt) =
            let vx = a.X-b.X
            let vy = a.Y-b.Y
            sqrt(vx*vx + vy*vy)

        /// Returns the squared distance between two 2D points.
        /// This operation is slightly faster than the distance function, and sufficient for many algorithms like finding closest points.
        static member inline distanceSq (a:Pt) (b:Pt) =
            let vx = a.X-b.X
            let vy = a.Y-b.Y
            vx*vx + vy*vy

        /// Returns the distance from World Origin.
        static member inline distanceFromOrigin (pt:Pt) = pt.DistanceFromOrigin


        /// Returns the square distance from World Origin.
        static member inline sqDistanceFromOrigin (pt:Pt) = pt.SqDistanceFromOrigin


        /// Returns a new 2D point at a given distance from World Origin by scaling the input.
        static member inline setDistanceFromOrigin f (pt:Pt) = pt.WithDistanceFromOrigin f

        /// Returns angle between three 2D Points in Radians. Range 0.0 to Pi.
        static member inline anglePiPts (ptPrev:Pt, ptThis:Pt, ptNext:Pt) =
            Vc.anglePi (ptPrev-ptThis) (ptNext-ptThis)

        /// Returns angle between three 2D Points in Degrees. Range 0.0 to 180
        static member inline angle180Pts (ptPrev:Pt, ptThis:Pt, ptNext:Pt) =
            Pt.anglePiPts (ptPrev, ptThis, ptNext) |> toDegrees

        /// Returns a (not unitized) bisector vector in the middle direction from ptThis.
        /// Code : (ptPrev-ptThis).Unitized  + (ptNext-ptThis).Unitized
        /// ptPrev * ptThis * ptNext ->   bisector vector
        static member inline bisector (ptPrev:Pt, ptThis:Pt, ptNext:Pt) =
           (ptPrev-ptThis).Unitized  + (ptNext-ptThis).Unitized

        /// Rotate a 2D point counter-clockwise by a 2D rotation (that has cos and sin precomputed)
        static member inline rotateBy (r:Rotation2D) (p:Pt) : Pt =
            Pt(r.Cos*p.X - r.Sin*p.Y,
                r.Sin*p.X + r.Cos*p.Y)

        /// Rotate the 2D point in Degrees. Counter Clockwise.
        /// For better Performance precompute the Rotate2D struct and use its member to rotate.
        /// see Vc.rotateBy.
        static member inline rotate (angDegree) (vec:Pt) : Pt =
            Pt.rotateBy (Rotation2D.createFromDegrees angDegree) vec

        /// Rotate the 2D point around a center 2D point. Counter Clockwise.
        /// By a 2D Rotation (that has cos and sin precomputed)
        static member inline rotateWithCenterBy (cen:Pt) (r:Rotation2D) (pt:Pt) =
            let x = pt.X - cen.X
            let y = pt.Y - cen.Y
            Pt (r.Cos*x - r.Sin*y + cen.X,
                r.Sin*x + r.Cos*y + cen.Y)

        /// Rotate 2D point around a center point Counter-Clockwise. Angle given in Degrees.
        static member inline rotateWithCenter (cen:Pt)  angDegree (pt:Pt) =
            Pt.rotateWithCenterBy cen (Rotation2D.createFromDegrees angDegree) pt


        /// Returns a point that is at a given distance from a 2D point in the direction of another point.
        static member inline distPt (fromPt:Pt, dirPt:Pt, distance:float) : Pt =
            let x = dirPt.X - fromPt.X
            let y = dirPt.Y - fromPt.Y
            let len = sqrt(x*x + y*y)
            if isTooTiny len then failTooClose "Pt.distPt" fromPt dirPt
            let fac = distance / len
            Pt(fromPt.X + x*fac,
               fromPt.Y + y*fac)

        /// Linearly interpolates between two 2D points.
        /// e.g. rel=0.5 will return the middle point, rel=1.0 the endPoint,
        /// rel=3.0 a point three times the distance beyond the end point.
        /// Same as Pt.lerp.
        static member inline divPt(fromPt:Pt, toPt:Pt, rel:float) : Pt =
            Pt(fromPt.X + (toPt.X-fromPt.X)*rel,
                fromPt.Y + (toPt.Y-fromPt.Y)*rel)


        /// Linearly interpolates between two 2D points.
        /// e.g. rel=0.5 will return the middle point, rel=1.0 the endPoint,
        /// rel=3.0 a point three times the distance beyond the end point.
        /// Same as Pt.divPt.
        static member inline lerp(fromPt:Pt, toPt:Pt, rel:float) : Pt =
            Pt.divPt(fromPt, toPt, rel)

        /// Snaps to a point if it is within the snapDistance.
        /// otherwise returns the original point.
        static member inline snapIfClose (snapDistance) (snapTo:Pt) (pt:Pt) =
            let v = snapTo-pt
            if v.Length < snapDistance then snapTo else pt

        /// Snaps the point's coordinates to the given precision.
        /// e.g. snap 0.1 Pt(0.123, 0.456) -> Pt(0.1, 0.5)
        /// e.g. snap 10  Pt(3    , 19)    -> Pt(0  , 20)
        /// does: (Math.Round (x/precision)) * precision
        static member inline snap (precision:float) (pt:Pt) =
            if isTooTiny (precision) then fail $"Pt.snap: precision too small or negative:{precision}"
            Pt( (Math.Round (pt.X/precision)) * precision,
                (Math.Round (pt.Y/precision)) * precision)

        /// Returns angle in Degrees at mid point (thisPt).
        static member angleInCorner(prevPt:Pt, thisPt:Pt, nextPt:Pt) =
            let a = prevPt-thisPt
            let b = nextPt-thisPt
            Vc.angle180 a b

        /// 'fromPt' Pt and UnitVc describe an endless 2D line.
        /// testPt gets projected on to this line.
        /// Returns the parameter (or scaling for unit-vector) on this line of the projection.
        static member inline projectedParameter (fromPt:Pt, uv:UnitVc, testPt:Pt) =
            let dir = testPt-fromPt
            Vc.dot (dir, uv)


        /// 'fromPt' and 'vec' vector describe an endless 2D line.
        /// 'testPt' gets projected onto this line.
        /// Returns the parameter (or scaling for vector) on this line of the projection.
        static member inline projectedParameter (fromPt:Pt, vec:Vc, testPt:Pt) =
            let dir = testPt-fromPt
            let lenSq = vec.LengthSq
            if isTooTinySq(lenSq) then failTooSmall "Pt.projectedParameter" vec
            Vc.dot (vec, dir) / lenSq


        /// 'fromPt' and 'toPt' describe an endless 2D line.
        /// 'testPt' gets projected on to this line.
        /// Returns the parameter (or scaling for vector) on this line of the projection.
        static member inline projectedParameter (fromPt:Pt, toPt:Pt, testPt:Pt) =
            let dir = testPt - fromPt
            let v   = toPt   - fromPt
            let lenSq = v.LengthSq
            if isTooTinySq(lenSq) then failTooClose "Pt.projectedParameter" fromPt toPt
            Vc.dot (v, dir) / lenSq


        /// Returns the closer point of the two points to the reference given point.
        /// When both points are equally close, the first point is returned.
        static member closestOfTwo (pt1:Pt) (pt2:Pt) (referencePoint:Pt) =
            let d1 = Pt.distanceSq pt1 referencePoint
            let d2 = Pt.distanceSq pt2 referencePoint
            if d1 <= d2 then
                pt1
            else
                pt2



        [<Obsolete("Use .sqDistanceFromOrigin instead.")>]
        static member inline distanceFromOriginSquare (pt:Pt) = pt.SqDistanceFromOrigin


        [<Obsolete("Use .SqDistanceTo instead.")>]
        member inline p.DistanceToSquare (b:Pt) =
            let x = p.X-b.X
            let y = p.Y-b.Y
            x*x + y*y


        [<Obsolete("Use .SqDistanceFromOrigin instead.")>]
        member inline pt.DistanceFromOriginSquare =
            pt.X*pt.X + pt.Y*pt.Y

        [<Obsolete("Use .SqDistanceToLine instead.")>]
        member inline pt.DistanceToLineSquare (ln:Line2D)=
            pt.SqDistanceToLine(ln)