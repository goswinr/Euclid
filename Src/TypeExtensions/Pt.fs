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
        member inline p.IsInValid : bool =
            isNanInfinity p.X || isNanInfinity p.Y

        /// Returns a boolean indicating whether X or Y is NaN or Infinity.
        static member inline isInValid (pt:Pt) : bool =
            pt.IsInValid

        /// Returns a boolean indicating whether X and Y are both valid (not NaN or Infinity).
        member inline p.IsValid : bool =
            not p.IsInValid

        /// Returns a boolean indicating whether X and Y are both valid (not NaN or Infinity).
        static member inline isValid (pt:Pt) : bool =
            pt.IsValid

        /// Returns the 2D point as 2D vector.
        member inline p.AsVc : Vc =
            Vc(p.X, p.Y)

        /// Returns the 2D point as 2D vector.
        static member inline asVc (pt:Pt) : Vc =
            pt.AsVc

        /// Returns the 2D point as 3D vector. Using 0.0 for Z.
        member inline p.AsVec : Vec =
            Vec(p.X, p.Y, 0.0)

        /// Returns the 2D point as 3D vector. Using 0.0 for Z.
        static member inline asVec (pt:Pt) : Vec =
            pt.AsVec

        /// Returns the 2D point as 3D point. Using 0.0 for Z.
        /// Use the member pt.WithZ to set Z to a different value.
        member inline p.AsPnt : Pnt =
            Pnt(p.X, p.Y, 0.0)

        /// Returns the 2D point as 3D point. Using 0.0 for Z.
        /// Use the member pt.WithZ to set Z to a different value.
        static member inline asPnt (pt:Pt) : Pnt =
            pt.AsPnt

        /// Returns a boolean indicating whether X and Y are exactly 0.0.
        member inline pt.IsOrigin : bool =
            pt.X = 0.0 && pt.Y = 0.0

        /// Returns a boolean indicating whether X and Y are exactly 0.0.
        static member inline isOrigin (pt:Pt) : bool =
            pt.IsOrigin

        /// Returns a boolean indicating if any of X and Y is not exactly 0.0.
        member inline p.IsNotOrigin : bool =
            p.X <> 0.0 || p.Y <> 0.0

        /// Returns a boolean indicating if any of X and Y is not exactly 0.0.
        static member inline isNotOrigin (pt:Pt) : bool =
            pt.IsNotOrigin

        /// Returns a boolean indicating whether the absolute value of X and Y is each less than the given tolerance.
        member inline pt.IsAlmostOrigin tol : bool =
            abs pt.X < tol && abs pt.Y < tol

        /// Returns a boolean indicating whether the absolute value of X and Y is each less than the given tolerance.
        static member inline isAlmostOrigin tol (pt:Pt) : bool =
            pt.IsAlmostOrigin tol

        /// Returns new 2D point with new X coordinate, Y stays the same.
        member inline pt.WithX x : Pt =
            Pt (x, pt.Y)

        /// Returns new 2D point with new X coordinate, Y stays the same.
        static member inline withX x (pt:Pt) : Pt =
            Pt(x, pt.Y)

        /// Returns new 2D point with new Y coordinate, X stays the same.
        member inline pt.WithY y : Pt =
            Pt (pt.X, y)

        /// Returns new 2D point with new Y coordinate, X stays the same.
        static member inline withY y (pt:Pt) : Pt =
            Pt(pt.X, y)

        /// Returns new 3D point with Z coordinate, X and Y stay the same.
        /// If you want Z to be 0.0 you can use pt.AsPnt too.
        member inline pt.WithZ z : Pnt =
            Pnt (pt.X, pt.Y, z)

        /// Returns new 3D point with Z coordinate, X and Y stay the same.
        /// If you want Z to be 0.0 you can use pt.AsPnt too.
        static member inline withZ z (pt:Pt) : Pnt =
            pt.WithZ z

        /// Returns the distance between two 2D points.
        member inline p.DistanceTo (b:Pt) : float =
            let x = p.X-b.X
            let y = p.Y-b.Y
            sqrt(x*x + y*y)

        /// Returns the distance between two 2D points.
        /// Same as Pt.dist
        static member inline distanceTo (b:Pt) (p:Pt) : float =
            p.DistanceTo b

        /// Returns the distance between two 2D points.
        /// Same as Pt.distanceTo.
        static member inline dist (b:Pt) (p:Pt) : float =
            b.DistanceTo p

        /// Returns the squared distance between two 2D points.
        /// This operation is slightly faster than the distance function, and sufficient for many algorithms like finding closest points.
        member inline p.SqDistanceTo (b:Pt) : float =
            let x = p.X-b.X
            let y = p.Y-b.Y
            x*x + y*y

        /// Returns the squared distance between two 2D points.
        /// This operation is slightly faster than the distance function, and sufficient for many algorithms like finding closest points.
        /// Same as Pt.sqDist.
        static member inline sqDistanceTo (b:Pt) (p:Pt) : float =
            p.SqDistanceTo b

        /// Returns the squared distance between two 2D points.
        /// This operation is slightly faster than the distance function, and sufficient for many algorithms like finding closest points.
        /// Same as Pt.sqDistanceTo.
        static member inline sqDist (b:Pt) (p:Pt) : float =
            b.SqDistanceTo p

        /// Returns the distance from Origin (0, 0)
        member inline pt.DistanceFromOrigin : float =
            sqrt (pt.X*pt.X + pt.Y*pt.Y)

        /// Returns the distance from Origin (0, 0)
        static member inline distanceFromOrigin (pt:Pt) : float =
            pt.DistanceFromOrigin

        /// Returns the squared distance from Origin (0, 0)
        member inline pt.SqDistanceFromOrigin : float =
            pt.X*pt.X + pt.Y*pt.Y

        /// Returns the squared distance from Origin (0, 0)
        static member inline sqDistanceFromOrigin (pt:Pt) : float =
            pt.SqDistanceFromOrigin

        /// Returns new 2D point with given distance from Origin by scaling it up or down.
        member inline pt.WithDistanceFromOrigin (l:float) : Pt =
            let d = pt.DistanceFromOrigin
            if isTooTiny d then failTooSmall "Pt.WithDistanceFromOrigin" pt
            pt * (l/d)

        /// Returns new 2D point with given distance from Origin by scaling it up or down.
        static member inline withDistanceFromOrigin (l:float) (pt:Pt) : Pt =
            pt.WithDistanceFromOrigin l

        /// Rotate a 2D point Counter Clockwise by a 2D Rotation (that has cos and sin precomputed)
        member inline pt.RotateBy (r:Rotation2D) : Pt =
            Pt(r.Cos*pt.X - r.Sin*pt.Y,
                r.Sin*pt.X + r.Cos*pt.Y)

        /// Rotate a 2D point Counter Clockwise by a 2D Rotation (that has cos and sin precomputed)
        static member inline rotateBy (r:Rotation2D) (p:Pt) : Pt =
            Pt(r.Cos*p.X - r.Sin*p.Y,
                r.Sin*p.X + r.Cos*p.Y)

        /// <summary>Rotate the 2D point in Degrees. Counter Clockwise.</summary>
        /// <remarks>For better performance precompute the Rotation2D struct and rotate with this.RotateBy(rotation2D).</remarks>
        member inline pt.Rotate angDegree : Pt =
            pt.RotateBy (Rotation2D.createFromDegrees angDegree)

        /// <summary>Rotate the 2D point in Degrees. Counter Clockwise.</summary>
        /// <remarks>For better performance precompute the Rotation2D struct and rotate with this.RotateBy(rotation2D).</remarks>
        static member inline rotate angDegree (vec:Pt) : Pt =
            Pt.rotateBy (Rotation2D.createFromDegrees angDegree) vec

        /// <summary>Rotate the 2D point in Radians. Counter Clockwise.</summary>
        /// <remarks>For better performance precompute the Rotation2D struct and rotate with this.RotateBy(rotation2D).</remarks>
        member inline pt.RotateRadians angRadians  : Pt =
            pt.RotateBy (Rotation2D.createFromRadians angRadians)

        /// <summary>Rotate the 2D point in Radians. Counter Clockwise.</summary>
        /// <remarks>For better performance precompute the Rotation2D struct and rotate with this.RotateBy(rotation2D).</remarks>
        static member inline rotateRadians angRadians (pt:Pt) : Pt =
            pt.RotateRadians angRadians

        /// 90 Degree rotation Counter-Clockwise.
        member inline pt.Rotate90CCW : Pt =
            Pt( -pt.Y, pt.X)

        /// 90 Degree rotation Counter-Clockwise.
        static member inline rotate90CCW (pt:Pt) : Pt =
            pt.Rotate90CCW

        /// 90 Degree rotation clockwise.
        member inline pt.Rotate90CW : Pt =
            Pt(pt.Y, -pt.X)

        /// 90 Degree rotation clockwise.
        static member inline rotate90CW (pt:Pt) : Pt =
            pt.Rotate90CW

        /// Rotates the 2D point by a given number of quarter-circles (i.e. multiples of 90
        /// degrees or Pi/2 radians). A positive number rotates counter-clockwise, a
        /// negative number rotates clockwise. The length of the vector is preserved.
        member inline pt.RotateByQuarterCircle(numberOfQuarters:int) : Pt =
            Pt.rotateByQuarterCircle numberOfQuarters pt

        /// Rotates a point by a given number of quarter-circles (i.e. multiples of 90
        /// degrees or Pi/2 radians). A positive number rotates counter-clockwise, a
        /// negative number rotates clockwise. The length of the vector is preserved.
        static member rotateByQuarterCircle (numberOfQuarters:int) (v:Pt) : Pt=
            let mutable nQuad = numberOfQuarters % 4
            if nQuad < 0 then nQuad <- nQuad + 4
            match nQuad with
            | 0 -> v
            | 1 -> Pt(-v.Y, v.X)
            | 2 -> Pt(-v.X, -v.Y)
            | 3 -> Pt(v.Y, -v.X)
            | _ -> v // should never happen

        /// Returns the Diamond Angle from this point to another point.
        /// Calculates the proportion of X to Y component.
        /// It is always positive and in the range of 0.0 to 4.0 (for 360 Degrees)
        /// 0.0 = Xaxis, going Counter-Clockwise.
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        /// Fails if the two points are coincident or too close together.
        member inline p.DirectionDiamondTo(o:Pt) : float =
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

        /// Returns the Diamond Angle from this point to another point.
        /// Calculates the proportion of X to Y component.
        /// It is always positive and in the range of 0.0 to 4.0 (for 360 Degrees)
        /// 0.0 = Xaxis, going Counter-Clockwise.
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        /// Fails if the two points are coincident or too close together.
        static member inline directionDiamondTo (o:Pt) (p:Pt) : float =
            p.DirectionDiamondTo(o)

        /// Returns the angle in Radians from this point to another point.
        /// 0.0 = Xaxis, going Counter-Clockwise till two Pi.
        /// Fails if the two points are coincident or too close together.
        member inline p.Angle2PiTo(o:Pt) : float =
            // https://stackoverflow.com/a/14675998/969070
            let x = o.X-p.X
            let y = o.Y-p.Y
            if isTooTiny (abs(x) + abs(y)) then failTooSmall "Pt.Angle2PiTo" p
            let a = Math.Atan2(y, x)
            if a < 0. then  a + UtilEuclid.twoPi
            else            a

        /// Returns the angle in Radians from 'fromPt' to 'toPt'.
        /// 0.0 = Xaxis, going Counter-Clockwise till two Pi.
        /// Fails if the two points are coincident or too close together.
        static member inline angle2PiTo (fromPt:Pt, toPt:Pt) : float = // not curried because argument order is important
            fromPt.Angle2PiTo(toPt)

        /// Returns the angle in Degrees from this point to another point.
        /// 0.0 = Xaxis, going Counter-Clockwise till 360.
        /// Fails if the two points are coincident or too close together.
        member inline p.Angle360To(o:Pt) : float =
            p.Angle2PiTo o |> toDegrees

        /// Returns the angle in Degrees from 'fromPt' to 'toPt'.
        /// 0.0 = Xaxis, going Counter-Clockwise till 360.
        /// Fails if the two points are coincident or too close together.
        static member inline angle360To (fromPt:Pt, toPt:Pt) : float =  // not curried because argument order is important
            fromPt.Angle360To(toPt)

        /// Returns the closest point on a finite line segment to test point.
        /// The line segment is given as a Line2D `ln`.
        /// Fails if the line is degenerate (zero-length).
        member inline testPt.ClosestPointOnLine(ln:Line2D) : Pt =
            XLine2D.clPtLn(ln.FromX, ln.FromY, ln.VectorX, ln.VectorY, testPt.X, testPt.Y)

        /// Returns the closest point on a finite line segment to test point.
        /// The line segment is given as a Line2D `ln`.
        /// Fails if the line is degenerate (zero-length).
        static member inline closestPointOnLine (ln:Line2D) (testPt:Pt) : Pt =
            testPt.ClosestPointOnLine(ln)

        /// Returns the squared distance between point and finite line segment.
        /// The line segment is given as a Line2D `ln`.
        member inline testPt.SqDistanceToLine(ln:Line2D) : float =
            XLine2D.sqDistLnPt(ln.FromX, ln.FromY, ln.VectorX, ln.VectorY, testPt.X, testPt.Y)

        /// Returns the squared distance between point and finite line segment.
        /// The line segment is given as a Line2D `ln`.
        static member inline sqDistanceToLine (ln:Line2D) (testPt:Pt) : float =
            testPt.SqDistanceToLine(ln)

        /// Returns the distance between point and finite line segment.
        /// The line segment is given as a Line2D `ln`.
        member inline testPt.DistanceToLine(ln:Line2D) : float =
            testPt.SqDistanceToLine ln |> sqrt

        /// Returns the distance between point and finite line segment.
        /// The line segment is given as a Line2D `ln`.
        static member inline distanceToLine (ln:Line2D) (testPt:Pt) : float =
            testPt.DistanceToLine(ln)



        // #endregion
        // #region Static members



        /// Checks if two 2D points are equal within tolerance.
        /// Use a tolerance of 0.0 to check for an exact match.
        static member inline equals (tol:float) (a:Pt) (b:Pt) : bool =
            abs (a.X-b.X) <= tol &&
            abs (a.Y-b.Y) <= tol

        /// Check if two 2D points are not equal within a given tolerance.
        /// Use a tolerance of 0.0 to check if the two points are not exactly equal.
        static member notEquals (tol:float) (a:Pt) (b:Pt) : bool =
            abs (a.X-b.X) > tol ||
            abs (a.Y-b.Y) > tol

        /// Accepts any type that has X and Y (UPPERCASE) members that can be converted to float values.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline createFromMembersXY pt : Pt =
            let x = ( ^T : (member X: _) pt)
            let y = ( ^T : (member Y: _) pt)
            try
                Pt(float x, float y)
            with e ->
                fail2 "Pt.createFromMembersXY" pt e |> unbox // unbox to make type checker happy

        /// Accepts any type that has x and y (lowercase) members that can be converted to float values.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline createFromMembersxy pt : Pt =
            let x = ( ^T : (member x: _) pt)
            let y = ( ^T : (member y: _) pt)
            try
                Pt(float x, float y)
            with e ->
                fail2 "Pt.createFromMembersxy" pt e |> unbox // unbox to make type checker happy


        // /// Create a 2D point from X and Y components.
        // static member inline create (x:float) (y:float) : Pt =
        //     Pt(x, y)

        /// Create 2D point from 3D point. Ignoring Z component.
        static member inline createFromPnt (p:Pnt) : Pt =
            Pt (p.X, p.Y)

        /// Create 2D point from 2D vector.
        static member inline createFromVc (v:Vc) : Pt =
            Pt (v.X, v.Y)

        /// Create 2D point from 2D unit-vector.
        static member inline createFromUnitVc (v:UnitVc) : Pt =
            Pt (v.X, v.Y)

        /// Gets the X value of 2D point.
        static member inline getX (pt:Pt) : float =
            pt.X

        /// Gets the Y value of 2D point.
        static member inline getY (pt:Pt) : float =
            pt.Y

        /// Adds two 2D points. Returns a new 2D point.
        static member inline add (a:Pt) (b:Pt) : Pt =
            a + b

        /// Add a 2D point to a 2D vector. Returns a new 2D point.
        static member inline addVc (v:Vc) (a:Pt) : Pt =
            a + v

        /// Returns the midpoint of two 2D points.
        static member inline midPt (a:Pt) (b:Pt) : Pt =
            (a+b) * 0.5

        /// Scales a 2D point by a factor. Returns a new 2D point.
        static member inline scale (f:float) (pt:Pt) : Pt =
            pt*f

        /// Move a 2D point by a vector. Same as Pt.move.
        static member inline translate (shift:Vc) (pt:Pt) : Pt =
            pt + shift

        /// Move a 2D point by a vector. Same as Pt.translate.
        static member inline move (shift:Vc) (pt:Pt) : Pt =
            pt + shift

        /// Add a float to X component of a 2D point. Returns a new 2D point.
        static member inline moveX (x:float) (pt:Pt) : Pt =
            Pt (pt.X+x, pt.Y)

        /// Add a float to Y component of a 2D point. Returns a new 2D point.
        static member inline moveY (y:float) (pt:Pt) : Pt =
            Pt (pt.X, pt.Y+y)

        /// Returns angle between three 2D Points in Radians. Range 0.0 to Pi.
        static member inline anglePiPts (ptPrev:Pt, ptThis:Pt, ptNext:Pt) : float =
            Vc.anglePi (ptPrev-ptThis) (ptNext-ptThis)

        /// Returns angle between three 2D Points in Degrees. Range 0.0 to 180
        static member inline angle180Pts (ptPrev:Pt, ptThis:Pt, ptNext:Pt) : float =
            Pt.anglePiPts(ptPrev, ptThis, ptNext) |> toDegrees

        /// Returns a (not unitized) bisector vector in the middle direction from ptThis.
        /// Code : (ptPrev-ptThis).Unitized  + (ptNext-ptThis).Unitized
        /// ptPrev * ptThis * ptNext ->   bisector vector
        static member inline bisector (ptPrev:Pt, ptThis:Pt, ptNext:Pt) : Vc =
           (ptPrev-ptThis).Unitized  + (ptNext-ptThis).Unitized

        /// Rotate the 2D point around a center 2D point. Counter Clockwise.
        /// By a 2D Rotation (that has cos and sin precomputed)
        static member inline rotateWithCenterBy (cen:Pt) (r:Rotation2D) (pt:Pt) : Pt =
            let x = pt.X - cen.X
            let y = pt.Y - cen.Y
            Pt (r.Cos*x - r.Sin*y + cen.X,
                r.Sin*x + r.Cos*y + cen.Y)

        /// Rotate 2D point around a center point Counter-Clockwise. Angle given in Degrees.
        static member inline rotateWithCenter (cen:Pt) (angDegree:float) (pt:Pt) : Pt =
            Pt.rotateWithCenterBy cen (Rotation2D.createFromDegrees angDegree) pt

        /// Returns a point that is at a given distance from a 2D point in the direction of another point.
        static member inline distPt ( fromPt:Pt, dirPt:Pt, distance:float) : Pt = // not curried because argument order is important and would be confusing
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
        static member inline divPt (fromPt:Pt, toPt:Pt, rel:float) : Pt = // not curried because argument order is important and would be confusing
            Pt( fromPt.X + (toPt.X-fromPt.X)*rel,
                fromPt.Y + (toPt.Y-fromPt.Y)*rel)

        /// Linearly interpolates between two 2D points.
        /// e.g. rel=0.5 will return the middle point, rel=1.0 the endPoint,
        /// rel=3.0 a point three times the distance beyond the end point.
        /// Same as Pt.divPt.
        static member inline lerp (fromPt:Pt, toPt:Pt, rel:float) : Pt = // not curried because argument order is important and would be confusing
            Pt.divPt(fromPt, toPt, rel)

        /// Snaps to a point if it is within the snapDistance.
        /// otherwise returns the original point.
        static member inline snapIfClose (snapDistance:float) (snapTo:Pt) (pt:Pt) : Pt =
            let v = snapTo-pt
            if v.Length < snapDistance then snapTo else pt

        /// Snaps the point's coordinates to the given precision.
        /// e.g. snap 0.1 Pt(0.123, 0.456) -> Pt(0.1, 0.5)
        /// e.g. snap 10  Pt(3    , 19)    -> Pt(0  , 20)
        /// does: (Math.Round (x/precision)) * precision
        static member inline snap (precision:float) (pt:Pt) : Pt =
            if isTooTiny (precision) then fail $"Pt.snap: precision too small or negative:{precision}"
            Pt( (Math.Round (pt.X/precision)) * precision,
                (Math.Round (pt.Y/precision)) * precision)

        /// Returns angle in Degrees at mid point (thisPt).
        static member angleInCorner (prevPt:Pt, thisPt:Pt, nextPt:Pt) : float =
            let a = prevPt-thisPt
            let b = nextPt-thisPt
            Vc.angle180 a b

        /// <summary>
        /// Projects a test point onto an endless line defined by an origin point and a unit direction vector.
        /// </summary>
        /// <param name="fromPt">The origin point of the endless line.</param>
        /// <param name="uv">The unit direction vector describing the orientation of the endless line.</param>
        /// <param name="testPt">The point to be projected onto the line.</param>
        /// <returns>The parameter (scalar value) along the unit vector at which the projection of testPt falls on the line.</returns>
        static member inline projectedParameter (fromPt:Pt, uv:UnitVc, testPt:Pt) : float =
            let dir = testPt - fromPt
            Vc.dot (dir, uv)

        /// <summary>
        /// Projects a test point onto an endless line defined by an origin point and a direction vector.
        /// </summary>
        /// <param name="fromPt">The origin point of the endless line.</param>
        /// <param name="v">The direction vector of the endless line. Does not need to be unitized.</param>
        /// <param name="testPt">The point to be projected onto the line.</param>
        /// <returns>The parameter (scaling factor for the direction vector) at which the projection of testPt falls on the line.</returns>
        static member inline projectedParameter (fromPt:Pt, v:Vc, testPt:Pt) : float =
            let dir = testPt - fromPt
            let lenSq = v.LengthSq
            if isTooTinySq(lenSq) then failTooSmall "Pt.projectedParameter" v
            Vc.dot (v, dir) / lenSq

        /// <summary>
        /// Projects a test point onto an endless line defined by two points.
        /// </summary>
        /// <param name="fromPt">The start point defining the endless line.</param>
        /// <param name="toPt">The end point defining the direction of the endless line.</param>
        /// <param name="testPt">The point to be projected onto the line.</param>
        /// <returns>The parameter along the line direction (fromPt to toPt) at which the projection of testPt falls.
        /// A value of 0.0 corresponds to fromPt, 1.0 corresponds to toPt.</returns>
        static member inline projectedParameter (fromPt:Pt, toPt:Pt, testPt:Pt) : float =
            let dir  = testPt - fromPt
            let v    = toPt   - fromPt
            let lenSq = v.LengthSq
            if isTooTinySq(lenSq) then failTooClose "Pt.projectedParameter" fromPt toPt
            Vc.dot (v, dir) / lenSq

        /// Returns the closer point of the two points to the reference given point.
        /// When both points are equally close, the first point is returned.
        static member closestOfTwo (pt1:Pt) (pt2:Pt) (referencePoint:Pt) : Pt =
            let d1 = Pt.sqDistanceTo pt1 referencePoint
            let d2 = Pt.sqDistanceTo pt2 referencePoint
            if d1 <= d2 then
                pt1
            else
                pt2

        // #endregion
        // #region Obsolete

        [<Obsolete("Use .SqDistanceTo instead.")>]
        member inline p.DistanceToSquare (b:Pt) : float =
            let x = p.X-b.X
            let y = p.Y-b.Y
            x*x + y*y

        [<Obsolete("Use .SqDistanceFromOrigin instead.")>]
        member inline pt.DistanceFromOriginSquare : float =
            pt.X*pt.X + pt.Y*pt.Y

        [<Obsolete("Use .SqDistanceToLine instead.")>]
        member inline pt.DistanceToLineSquare (ln:Line2D) : float =
            pt.SqDistanceToLine(ln)

        [<Obsolete("Use .sqDistanceFromOrigin instead.")>]
        static member inline distanceFromOriginSquare (pt:Pt) : float =
            pt.SqDistanceFromOrigin

        [<Obsolete("Use .distanceTo or .dist instead.")>]
        static member inline distance (a:Pt) (b:Pt) : float =
            let vx = a.X-b.X
            let vy = a.Y-b.Y
            sqrt(vx*vx + vy*vy)

        [<Obsolete("Use .sqDistanceTo or .sqDist instead.")>]
        static member inline distanceSq (a:Pt) (b:Pt) : float =
            let vx = a.X-b.X
            let vy = a.Y-b.Y
            vx*vx + vy*vy

        [<Obsolete("Use .withDistanceFromOrigin instead.")>]
        static member inline setDistanceFromOrigin f (pt:Pt) : Pt =
            pt.WithDistanceFromOrigin f

        [<Obsolete("Use the constructor Pt(x, y) instead.")>]
        static member inline create (x:float, y:float) : Pt =
            Pt(x, y)

