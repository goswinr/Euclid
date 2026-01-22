namespace Euclid

open System
open EuclidErrors


/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type UnitVc.
[<AutoOpen>]
module AutoOpenUnitVc =
    open UtilEuclid

    /// Returns distance between the tips of two 2D vectors.
    let inline internal vecDist2(ax:float, ay:float, bx:float, by:float) =
        let x = bx-ax
        let y = by-ay
        sqrt(x*x+y*y)

    type UnitVc with

        /// Convert 2D unit-vector to 2D point.
        member inline v.AsPt =
            Pt(v.X, v.Y)

        /// Convert 2D unit-vector to 2D vector
        member inline v.AsVc =
            Vc(v.X, v.Y)

        /// Convert 2D unit-vector to 3D vector using 0.0 as Z value.
        /// If you want a different Z value use the member v.WithZ(z)
        member inline v.AsVec =
            Vec(v.X, v.Y, 0.0)

        /// Convert 2D unit-vector to 3D unit-vector using 0.0 as Z value.
        /// If you want a different Z value use the member v.WithZ(z)
        member inline v.AsUnitVec =
            UnitVec.createUnchecked(v.X, v.Y, 0.0)

        /// Convert 2D unit-vector to 3D point using 0.0 as Z value.
        member inline v.AsPnt =
            Pnt(v.X, v.Y, 0.0)

        /// Returns a new 2D vector with new X coordinate, Y stays the same.
        /// Note: The result is a regular Vc, not a UnitVc anymore.
        member inline v.WithX x =
            Vc (x, v.Y)

        /// Returns a new 2D vector with new Y coordinate, X stays the same.
        /// Note: The result is a regular Vc, not a UnitVc anymore.
        member inline v.WithY y =
            Vc (v.X, y)

        /// Returns new 3D vector with Z coordinate, X and Y stay the same.
        /// If you want Z to be 0.0 you can use v.AsVec or v.AsUnitVec too.
        member inline v.WithZ z =
            Vec (v.X, v.Y, z)

        /// The 2D Cross Product of a 2D unit-vector with a 2D vector.
        /// It is just a scalar equal to the signed area of the parallelogram spanned by the input vectors.
        /// If the rotation from 'a' to 'b' is Counter-Clockwise the result is positive.
        member inline a.Cross (b:Vc) =
            a.X*b.Y - a.Y*b.X

        /// The 2D Cross Product of two 2D unit-vectors.
        /// It is also known as the Determinant, or the sine of the angle between the two vectors.
        /// It is just a scalar equal to the signed area of the parallelogram spanned by the input vectors.
        /// If the rotation from 'a' to 'b' is Counter-Clockwise the result is positive.
        member inline a.Cross (b:UnitVc) =
            a.X*b.Y - a.Y*b.X

        /// Dot product, or scalar product of two 2D unit-vectors.
        /// Returns a float. This float is the Cosine of the angle between the two 2D vectors.
        member inline a.Dot ( b:UnitVc) =
            a.X * b.X + a.Y * b.Y

        /// Dot product, or scalar product of a 2D unit-vector with a 2D vector.
        /// Returns a float. This float is the projected length of the 2D vector on the direction of the unit-vector.
        member inline a.Dot ( b:Vc) =
            a.X * b.X + a.Y * b.Y

        /// Dot product, or scalar product of two 2D unit-vectors.
        /// This float of unit-vectors is the Cosine of the angle between the two vectors.
        /// Returns a float with a F# unit of Measure Euclid.Cosine.cosine.
        /// This is useful for comparing the angle to precomputed values in the Euclid.Cosine module.
        member inline a.DotCosine (b:UnitVc) : float<Cosine.cosine> =
            a.X * b.X + a.Y * b.Y  |> LanguagePrimitives.FloatWithMeasure


        /// Rotate the a 2D unit-vector Counter Clockwise by a 2D Rotation (that has cos and sin precomputed)
        member inline v.RotateBy (r:Rotation2D) =
            UnitVc.createUnchecked (
                r.Cos*v.X - r.Sin*v.Y,
                r.Sin*v.X + r.Cos*v.Y)

        /// Rotate the 2D unit-vector in Degrees. Counter Clockwise.
        /// For better Performance recompute the Rotate2D struct and use its member to rotate. see UnitVc.RotateBy.
        member inline v.Rotate (angDegree)  =
            v.RotateBy (Rotation2D.createFromDegrees angDegree)

        /// Rotate the 2D unit-vector in Degrees. Counter Clockwise.
        /// For better Performance recompute the Rotate2D struct and use its member to rotate. see UnitVc.RotateBy.
        member inline v.RotateRadians (angRadians)  =
            v.RotateBy (Rotation2D.createFromRadians angRadians)

        /// 90 Degree rotation Counter-Clockwise.
        member inline v.Rotate90CCW =
            UnitVc.createUnchecked( -v.Y, v.X )

        /// 90 Degree rotation clockwise.
        member inline v.Rotate90CW =
            UnitVc.createUnchecked(  v.Y, -v.X )

        /// Rotates the 2D unit-vector by a given number of quarter-circles (i.e. multiples of 90
        /// degrees or Pi/2 radians). A positive number rotates counter-clockwise, a
        /// negative number rotates clockwise. The length of the vector is preserved.
        member inline v.RotateByQuarterCircle(numberOfQuarters:int) =
            UnitVc.rotateByQuarterCircle numberOfQuarters v


        /// Rotates a vector by a given number of quarter-circles (i.e. multiples of 90
        /// degrees or Pi/2 radians). A positive number rotates counter-clockwise, a
        /// negative number rotates clockwise. The length of the vector is preserved.
        static member rotateByQuarterCircle (numberOfQuarters:int) (v:UnitVc)=
            let mutable nQuad = numberOfQuarters % 4
            if nQuad < 0 then nQuad <- nQuad + 4
            match nQuad with
            | 0 -> v
            | 1 -> UnitVc.createUnchecked(-v.Y, v.X)
            | 2 -> UnitVc.createUnchecked(-v.X, -v.Y)
            | 3 -> UnitVc.createUnchecked(v.Y, -v.X)
            | _ -> fail "never happens"  |> unbox // unbox to make type checker happy


        /// The diamond angle.
        /// Calculates the proportion of X to Y component.
        /// It is always positive and in the range of 0.0 to 4.0 (for 360 Degrees)
        /// 0.0 = Xaxis, going Counter-Clockwise.
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        member inline v.DirectionDiamond =
            // https://stackoverflow.com/a/14675998/969070
            if v.Y >= 0.0 then
                if v.X >= 0.0 then
                    v.Y/(v.X+v.Y)
                else
                    1.0 - v.X/(-v.X+v.Y)
            else
                if v.X < 0.0 then
                    2.0 - v.Y/(-v.X-v.Y)
                else
                    3.0 + v.X/(v.X-v.Y)

        /// Returns the angle in Radians from X-axis,
        /// Going Counter-Clockwise till two Pi.
        member inline v.Direction2Pi =
            // https://stackoverflow.com/a/14675998/969070
            let a = Math.Atan2(v.Y, v.X)
            if a < 0. then
                a + UtilEuclid.twoPi
            else
                a

        /// Returns the angle in Radians from X-axis,
        /// Ignores orientation.
        /// Range 0.0 to Pi.
        member inline v.DirectionPi =
            // https://stackoverflow.com/a/14675998/969070
            let a = Math.Atan2(v.Y, v.X)
            if a < 0. then
                a + Math.PI
            else
                a

        /// Returns the angle in Degrees from X-axis.
        /// Going Counter-Clockwise till 360.
        member inline v.Direction360 =
            v.Direction2Pi |> toDegrees

        /// Returns the angle in Degrees from X-axis,
        /// Ignores orientation.
        /// Range 0.0 to 180.
        member inline v.Direction180 =
            v.DirectionPi |> toDegrees

        /// Returns positive angle for rotating Counter-Clockwise from this vector to vector 'b' .
        /// In Diamond Angle. Using only proportion of X to Y components.
        /// Range of 0.0 to 4.0 (for 360 Degrees)
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        member inline v.AngleDiamondTo (b:UnitVc) =
            let r = b.DirectionDiamond - v.DirectionDiamond
            if r >= 0. then  r
            else r + 4.0

        /// Checks if the angle between the two 2D unit-vectors is less than 90 degrees.
        /// Calculates the dot product of two 2D unit-vectors.
        /// Then checks if it is bigger than 1e-12.
        member inline v.MatchesOrientation (other:UnitVc) =
            v *** other > 1e-12

        /// Checks if the angle between this 2D unit-vector and a 2D vector is less than 90 degrees.
        /// Calculates the dot product of a 2D vector and a unit-vector.
        /// Then checks if it is bigger than 1e-12.
        member inline v.MatchesOrientation (other:Vc) =
            if isTooTinySq(other.LengthSq) then failTooSmall2 "UnitVc.MatchesOrientation" other v
            v *** other > 1e-12


        /// Checks if the angle between the two 2D unit-vectors is more than 90 degrees.
        /// Calculates the dot product of two 2D unit-vectors.
        /// Then checks if it is smaller than minus 1e-12.
        member inline v.IsOppositeOrientation (other:UnitVc) =
            v *** other < -1e-12

        /// Checks if the angle between this 2D unit-vector and a 2D vector is more than 90 degrees.
        /// Calculates the dot product of a 2D vector and a unit-vector.
        /// Then checks if it is smaller than minus 1e-12.
        member inline v.IsOppositeOrientation (other:Vc) =
            if isTooTinySq(other.LengthSq) then failTooSmall2 "UnitVc.IsOppositeOrientation" other v
            v *** other < -1e-12

        /// Checks if 2D unit-vector is parallel to the world X axis. Ignoring orientation.
        /// The absolute deviation tolerance along Y axis is 1e-9.
        member inline v.IsXAligned =
            let y = abs (v.Y)
            y < 1e-9

        /// Checks if 2D unit-vector is parallel to the world Y axis. Ignoring orientation.
        /// The absolute deviation tolerance along X axis is 1e-9.
        member inline v.IsYAligned =
            let x = abs (v.X)
            x < 1e-9

        /// Checks if two 2D unit-vectors are parallel.
        /// Ignores the line orientation.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        member inline this.IsParallelTo(other:UnitVc, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
            abs(other *** this) > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:

        /// Checks if this 2D unit-vectors and a 2D vector are parallel.
        /// Ignores the line orientation.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        member inline this.IsParallelTo(other:Vc, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
            let ol = other.LengthSq
            if isTooTinySq(ol) then failTooSmall2 "UnitVc.IsParallelTo" other this
            let ou = other * (1.0 / sqrt ol)
            abs(ou***this) > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:



        /// Checks if two 2D unit-vectors are parallel.
        /// Takes the line orientation into account too.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        member inline this.IsParallelAndOrientedTo (other:UnitVc, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
            other *** this > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:

        /// Checks if this 2D unit-vectors and a 2D vector are parallel.
        /// Takes the line orientation into account too.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        member inline this.IsParallelAndOrientedTo (other:Vc, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
            let ol = other.LengthSq
            if isTooTinySq(ol) then failTooSmall2 "UnitVc.IsParallelAndOrientedTo" other this
            let ou = other * (1.0 / sqrt ol)
            ou *** this > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:


        /// Checks if two 2D unit-vectors are perpendicular to each other.
        /// The default angle tolerance is 89.75 to 90.25 degrees.
        /// This tolerance can be customized by an optional maximum cosine value.
        /// The default cosine is 0.0043633 ( = 89.75 deg)
        /// See Euclid.Cosine module.
        member inline this.IsPerpendicularTo (other:UnitVc, [<OPT;DEF(Cosine.``89.75``)>] maxCosine:float<Cosine.cosine> ) =
            let d = other *** this
            !^(abs d) < maxCosine

        /// Checks if this 2D unit-vectors and a 2D vector are perpendicular to each other.
        /// The default angle tolerance is 89.75 to 90.25 degrees.
        /// This tolerance can be customized by an optional maximum cosine value.
        /// The default cosine is 0.0043633 ( = 89.75 deg)
        /// See Euclid.Cosine module.
        member inline this.IsPerpendicularTo (other:Vc, [<OPT;DEF(Cosine.``89.75``)>] maxCosine:float<Cosine.cosine> ) =
            let ol = other.LengthSq
            if isTooTinySq(ol) then failTooSmall2 "UnitVc.IsPerpendicularTo" other this
            let ou = other * (1.0 / sqrt ol)
            let d = ou *** this
            !^(abs d) < maxCosine


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




        /// Checks if two 2D unit-vectors are equal within tolerance.
        /// Identical unit-vectors in opposite directions are not considered equal.
        /// Use a tolerance of 0.0 to check for an exact match.
        static member inline equals (tol:float) (a:UnitVc) (b:UnitVc) =
            abs (a.X-b.X) <= tol &&
            abs (a.Y-b.Y) <= tol

        /// Check if two 2d unit vectors are not equal within a given tolerance.
        /// Use a tolerance of 0.0 to check if the two unit vectors are not exactly equal.
        static member notEquals (tol:float) (a:UnitVc) (b:UnitVc) =
            abs (a.X-b.X) > tol ||
            abs (a.Y-b.Y) > tol


        /// Create 2D unit-vector from start and endpoint. Does the unitizing too.
        static member inline create (fromPt:Pt, toPt:Pt) =
            let x = toPt.X - fromPt.X
            let y = toPt.Y - fromPt.Y
            let l = sqrt(x * x  + y * y)
            if isTooTiny l then failTooClose "UnitVc.create" fromPt toPt
            UnitVc.createUnchecked(x/l, y/l)

        /// Returns the World X-axis with length one: UnitVc(1, 0)
        static member inline Xaxis =
            UnitVc.createUnchecked (1.0, 0.0)

        /// Returns the World Y-axis with length one: UnitVc(0, 1)
        static member inline Yaxis =
            UnitVc.createUnchecked (0.0, 1.0)

        /// Returns the distance between the tips of two 2D unit-vectors.
        static member inline difference (a:UnitVc) (b:UnitVc) =
            let x = b.X-a.X
            let y = b.Y-a.Y
            sqrt(x*x+y*y)

        /// Returns the squared distance between the tips of two 2D unit-vectors.
        /// This operation is slightly faster than Vc.difference and sufficient for many algorithms like finding closest points.
        static member inline differenceSq (a:UnitVc) (b:UnitVc) =
            let x = b.X-a.X
            let y = b.Y-a.Y
            x*x+y*y

        /// Accepts any type that has a X and Y (UPPERCASE) member that can be converted to a float.
        /// Does the unitizing too.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline createFromMembersXY vec =
            let x = ( ^T : (member X : _) vec)
            let y = ( ^T : (member Y : _) vec)
            try
                UnitVc.create(float x, float y)
            with e ->
                fail2 "UnitVc.createFromMembersXY" vec e |> unbox // unbox to make type checker happy


        /// Accepts any type that has a x and y (lowercase) member that can be converted to a float.
        /// Does the unitizing too.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline createFromMembersxy vec =
            let x = ( ^T : (member x : _) vec)
            let y = ( ^T : (member y : _) vec)
            try
                UnitVc.create(float x, float y)
            with e ->
                fail2 "UnitVc.createFromMembersxy" vec e |> unbox // unbox to make type checker happy


        /// Create 2D unit-vector from 2D point. Does the unitizing too.
        static member inline createFromPt (pt:Pt) =
            let l = sqrt (pt.X*pt.X + pt.Y*pt.Y)
            if isTooTiny l then
                failTooClose "UnitVc.createFromPt" pt "Origin"
            UnitVc.createUnchecked(pt.X / l, pt.Y / l)

        /// Create 2D unit-vector from 2D vector. Does the unitizing too.
        static member inline createFromVec (v:Vc) =
            let l = sqrt (v.X*v.X + v.Y*v.Y)
            if isTooTiny l then
                failTooSmall "UnitVc.createFromVec" v
            UnitVc.createUnchecked(v.X / l, v.Y / l)


        /// Convert 2D unit-vector to 2D point.
        static member inline asPt(v:UnitVc) =
            Pt(v.X, v.Y)

        /// Convert 2D unit-vector to 2D vector using 0.0 as Z value.
        /// If you want a different Z value use the member w.WithZ(z)
        static member inline asVec(v:UnitVc) =
            Vec(v.X, v.Y, 0.0)

        /// Convert 2D unit-vector to 2D unit-vector using 0.0 as Z value.
        static member inline asUnitVec(v:UnitVc) =
            UnitVec.createUnchecked(v.X, v.Y, 0.0)

        /// Convert 2D unit-vector to 2D point using 0.0 as Z value.
        static member inline asPnt(v:UnitVc) =
            Pnt(v.X, v.Y, 0.0)

        //static member inline cross (a:UnitVc, b:UnitVc) // moved to UnitVc.fs

        /// The 2D Cross Product.
        /// It is also known as the Determinant, Wedge Product or Outer Product.
        /// It is just a scalar equal to the signed area of the parallelogram spanned by the input vectors.
        /// If the rotation from 'a' to 'b' is Counter-Clockwise the result is positive.
        static member inline cross (a:UnitVc, b:Vc)  : float =
            a.X*b.Y - a.Y*b.X

        /// The 2D Cross Product.
        /// It is also known as the Determinant, Wedge Product or Outer Product.
        /// It is just a scalar equal to the signed area of the parallelogram spanned by the input vectors.
        /// If the rotation from 'a' to 'b' is Counter-Clockwise the result is positive.
        static member inline cross (a:Vc, b:UnitVc)  : float =
            a.X*b.Y - a.Y*b.X

        //static member inline dot (a:UnitVc, b:UnitVc)  //moved to Vc type declaration

        /// Dot product, or scalar product of a 2D unit-vector with a 2D vector.
        /// Returns a float.
        /// This float is the projected length of the 2D vector on the direction of the unit-vector.
        static member inline dot (a:UnitVc, b:Vc)  : float =
            a.X * b.X + a.Y * b.Y

        /// Dot product, or scalar product of a 2D vector with a 2D unit-vector.
        /// Returns a float.
        /// This float is the projected length of the 2D vector on the direction of the unit-vector.
        static member inline dot (a:Vc, b:UnitVc)  : float =
            a.X * b.X + a.Y * b.Y

        /// Tangent of the angle between two 2D unit-vectors.
        /// Calculated as Cross Product divided by Dot Product.
        /// Result may be Infinity if the angle is 90 or 270 degrees.
        static member inline tangent (a:UnitVc, b:UnitVc) =
            let dot = a.X * b.X + a.Y * b.Y
            let det = a.X * b.Y - a.Y * b.X
            det / dot

        /// Sine of the angle between two 2D unit-vectors.
        /// This is the same as the 2D Cross Product or Determinant of the two unit-vectors.
        static member inline sine (a:UnitVc, b:UnitVc) =
            a.X * b.Y - a.Y * b.X


        /// Dot product, or scalar product of two 2D unit-vector.
        /// This float of unit-vectors is the Cosine of the angle between the two vectors.
        /// Returns a float with a unit of Measure Euclid.Cosine.cosine.
        /// This is useful for comparing the angle to precomputed values in the Euclid.Cosine module.
        static member inline dotCosine (a:UnitVc) ( b:UnitVc) : float<Cosine.cosine> =
            a.X * b.X + a.Y * b.Y  |> LanguagePrimitives.FloatWithMeasure

        /// Gets the X part of this 2D unit-vector.
        static member inline getX (v:UnitVc) =
            v.X

        /// Gets the Y part of this 2D unit-vector.
        static member inline getY (v:UnitVc) =
            v.Y

        /// Returns a new 2D vector with new X value, Y stays the same.
        static member inline withX x (v:UnitVc) =
            v.WithX x

        /// Returns a new 2D vector with new Y value, X stays the same.
        static member inline withY y (v:UnitVc) =
            v.WithY y

        /// Returns new 3D vector with Z value, X and Y stay the same.
        /// If you want Z to be 0.0 you can use v.AsVec or v.AsUnitVec too.
        static member inline withZ z (v:UnitVc) =
            v.WithZ z

        /// Add two 2D unit-vectors together. Returns a new (non-unitized) 2D vector.
        static member inline add (a:UnitVc) (b:UnitVc) =
            b + a

        /// Multiplies a 2D unit-vector with a scalar, also called scaling a vector.
        /// Same as UnitVc.withLength. Returns a new (non-unitized) 2D vector.
        static member inline scale (scale:float) (v:UnitVc) =
            Vc (v.X * scale, v.Y * scale)

        /// Multiplies a 2D unit-vector with a scalar, also called scaling a vector.
        /// Same as UnitVc.withLength. Returns a new (non-unitized) 2D vector.
        static member inline withLength (length:float) (v:UnitVc) =
            Vc (v.X * length, v.Y * length)

        /// Add to the X part of this 2D unit-vectors together. Returns a new (non-unitized) 2D vector.
        static member inline moveX x (v:UnitVc) =
            Vc (v.X+x, v.Y)

        /// Add to the Y part of this 2D unit-vectors together. Returns a new (non-unitized) 2D vector.
        static member inline moveY y (v:UnitVc) =
            Vc (v.X, v.Y+y)

        /// Negate or inverse a 2D unit-vectors. Returns a new 2D unit-vector.
        /// Same as UnitVc.reverse.
        static member inline flip (v:UnitVc) =
            -v

        /// Negate or inverse a 2D unit-vectors. Returns a new 2D unit-vector.
        /// Same as UnitVc.flip.
        static member inline reverse (v:UnitVc) =
            -v

        /// Returns angle between two 2D unit-vectors in Radians.
        /// Takes vector orientation into account.
        /// Ignores order of input vectors. anglePi(a, b) = anglePi(b, a)
        /// Range 0.0 to Pi( = 180 Degree)
        static member inline anglePi (a:UnitVc) (b:UnitVc) =
            // The "straight forward" method of acos(u.v) has large precision
            // issues when the dot product is near +/-1.  This is due to the
            // steep slope of the acos function as we approach +/- 1.  Slight
            // precision errors in the dot product calculation cause large
            // variation in the output value.
            // To avoid this we use an alternative method which finds the
            // angle bisector by (u-v)/2
            // Because u and v and unit-vectors, (u-v)/2 forms a right angle
            // with the angle bisector.  The hypotenuse is 1, therefore
            // 2*asin(|u-v|/2) gives us the angle between u and v.
            // The largest possible value of |u-v| occurs with perpendicular
            // vectors and is sqrt(2)/2 which is well away from extreme slope
            // at +/-1. (See Windows OS Bug 01706299 for details) (form WPF reference source code)
            let dot = a *** b
            if -0.98 < dot && dot < 0.98 then // threshold for switching 0.98 ?
                acos dot
            else
                if dot < 0. then Math.PI - 2.0 * asin(vecDist2(-a.X, -a.Y, b.X, b.Y) * 0.5)
                else                       2.0 * asin(vecDist2( a.X,  a.Y, b.X, b.Y) * 0.5)
            //let mutable r = b.Direction2Pi  - a.Direction2Pi  // this alternative calculation is about 10x slower for unit-vectors
            //if r < 0.      then  r <- r + UtilEuclid.twoPi
            //if r > Math.PI then  r <- UtilEuclid.twoPi - r
            //r


        /// Returns positive angle between two 2D unit-vectors in Radians.
        /// Ignores orientation.
        /// Ignores order of input vectors. angleHalfPi(a, b) = angleHalfPi(b, a) = angleHalfPi(-b, a) = angleHalfPi(-b, -a)
        /// Range 0.0 to Pi/2 ( = 90 Degree)
        static member inline angleHalfPi (a:UnitVc) (b:UnitVc) =
            let dot = a *** b
            let dotAbs = abs dot // constrains teh result to range 0-90 degrees
            if dotAbs < 0.98 then
                acos dotAbs
            else
                if dot < 0. then 2.0 * asin(vecDist2(-a.X, -a.Y, b.X, b.Y) * 0.5)
                else             2.0 * asin(vecDist2( a.X,  a.Y, b.X, b.Y) * 0.5)
            //let mutable r = b.Direction2Pi  - a.Direction2Pi // this alternative calculation is about 10x slower for unit-vectors
            //if r < 0.      then  r <- r + UtilEuclid.twoPi
            //if r > Math.PI then  r <- UtilEuclid.twoPi - r
            //if r > halfPi  then  r <- Math.PI - r
            //r


        /// Returns positive angle for rotating Counter-Clockwise from vector 'a' to vector 'b'.
        /// In Radians.
        /// Range: 0.0 to 2 Pi ( = 360 Degrees)
        static member inline angle2Pi (a:UnitVc, b:UnitVc) =
            let r = b.Direction2Pi  - a.Direction2Pi
            if r >= 0. then  r
            else r + UtilEuclid.twoPi

        /// Returns positive angle between two 2D unit-vectors in Degrees,
        /// Ignores vector orientation.
        /// Ignores order of input vectors. angle90(a, b) = angle90(b, a) = angle90(-b, a) = angle90(-b, -a)
        /// Range: 0 to 90 Degrees.
        static member inline angle90 (a:UnitVc) (b:UnitVc) =
            UnitVc.angleHalfPi a b |>  toDegrees


        /// Returns positive angle between two 2D unit-vectors in Degrees.
        /// Takes vector orientation into account.
        /// Ignores order of input vectors. angle180(a, b) = angle180(b, a)
        /// Range 0 to 180 Degrees.
        static member inline angle180 (a:UnitVc) (b:UnitVc) =
            UnitVc.anglePi a b |>  toDegrees


        /// Returns positive angle for rotating Counter-Clockwise from vector 'a' to vector 'b'.
        /// In Degrees.
        /// Range: 0 to 360 Degrees
        static member inline angle360 (a:UnitVc, b:UnitVc) =
            UnitVc.angle2Pi (a, b) |> toDegrees


        /// Returns positive angle for rotating Counter-Clockwise from vector 'a' to vector 'b'.
        /// In Diamond Angle. Using only proportion of X to Y components.
        /// Range of 0.0 to 4.0 (for 360 Degrees)
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        static member inline angleDiamond (a:UnitVc, b:UnitVc) =
            a.AngleDiamondTo(b)


        /// The diamond angle.
        /// Calculates the proportion of X to Y component.
        /// It is always positive and in the range of 0.0 to 4.0 (for 360 Degrees)
        /// 0.0 = Xaxis, going Counter-Clockwise.
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        static member inline directionDiamond(a:UnitVc) =
            a.DirectionDiamond

        /// Returns positive angle of unit-vector. Counter-Clockwise from X-axis.
        /// In Radians.
        /// Range: 0.0 to 2 Pi ( = 360 Degrees)
        static member inline direction2Pi (v:UnitVc) =
            v.Direction2Pi

        /// Returns positive angle of unit-vector. Counter-Clockwise from X-axis.
        /// In Degrees.
        /// Range: 0 to 360 Degrees
        static member inline direction360 (v:UnitVc) =
            v.Direction360

        /// Checks if the angle between two unit-vectors is smaller than a threshold angle specified as a precomputed cosine value.
        /// Ignores vector orientation. So the angle is between 0 to 90 degrees ignoring their orientation.
        /// Use the Euclid.Cosine module to get some precomputed cosine values.
        /// Use UnitVc.isAngleBelow for considering vector orientation.
        static member inline isParallelWithin (cosineValue: float<Cosine.cosine>) (a:UnitVc) (b:UnitVc) =
            abs(a *** b) > float cosineValue


        /// Checks if the angle between two unit-vectors is bigger than a threshold angle specified as a precomputed cosine value.
        /// Ignores vector orientation. So the angle is between 0 to 90 degrees ignoring their orientation.
        /// Use the Euclid.Cosine module to get some precomputed cosine values.
        /// Use UnitVc.isAngleAbove for considering vector orientation.
        static member inline isNotParallelWithin (cosineValue: float<Cosine.cosine>) (a:UnitVc) (b:UnitVc) =
            abs(a *** b) < float cosineValue


        /// Checks if the angle between two unit-vectors is smaller than a threshold angle specified as a precomputed cosine value.
        /// Considers the vector orientation too. So the angle is between 0 to 180 degrees.
        /// Use the Euclid.Cosine module to get some precomputed cosine values.
        /// Use UnitVc.isParallelWithin to ignore vector orientation.
        static member inline isAngleBelow (cosineValue: float<Cosine.cosine>) (a:UnitVc) (b:UnitVc) =
            a *** b > float cosineValue


        /// Checks if the angle between two unit-vectors is bigger than a threshold angle specified as a precomputed cosine value.
        /// Considers the vector orientation too. So the angle is between 0 to 180 degrees.
        /// Use the Euclid.Cosine module to get some precomputed cosine values.
        /// Use UnitVc.isNotParallelWithin to ignore vector orientation.
        static member inline isAngleAbove (cosineValue: float<Cosine.cosine>) (a:UnitVc) (b:UnitVc) =
            a *** b < float cosineValue


        /// Returns positive or negative slope of a 2D unit-vector in Radians.
        /// This is the angle from the X-axis in the 2D plane (or its reverse).
        /// Range -1.57 to +1.57 Radians.
        /// This is just asin(v.Y).
        static member inline slopeRadians (v:UnitVc) =
            let r = Math.Atan2(v.Y, v.X)
            if   r > halfPi  then  r - Math.PI
            elif r < -halfPi then  r + Math.PI
            else r

        /// Returns positive or negative slope of a 2D unit-vector in Degrees.
        /// This is the angle from the X-axis in the 2D plane (or its reverse).
        /// Range -90 to +90 Degrees.
        /// This is just asin(v.Y).
        static member inline slopeDegree (v:UnitVc) =
            UnitVc.slopeRadians v |> toDegrees

        /// Returns positive or negative slope of a 2D unit-vector in Percent.
        /// This is the angle from the X-axis in the 2D plane (or its reverse).
        /// 100% = 45 Degrees.
        /// Returns positive (or negative) infinity if line is vertical.
        static member inline slopePercent (v:UnitVc) =
            100.0 * v.Y / abs(v.X)

        /// Returns positive or negative angle of a vector in Radians from the X-axis.
        /// Range -3.14 to +3.14 Radians.
        /// This is just atan2(v.Y, v.X).
        static member inline angleToXPi (v:UnitVc) =
            Math.Atan2(v.Y, v.X)

        /// Returns positive or negative angle of a vector in Degrees from the X-axis.
        /// Range -180 to +180 Degrees.
        /// This is just atan2(v.Y, v.X) to degrees.
        static member inline angleToX180 (v:UnitVc) =
            UnitVc.angleToXPi v |> toDegrees

        /// Ensure that the 2D unit-vector has a positive dot product with given 2D orientation unit-vector.
        static member inline matchOrientation (orientationToMatch:UnitVc) (vecToFlip:UnitVc) =
            if orientationToMatch *** vecToFlip < 0.0 then -vecToFlip else vecToFlip

        /// Ensure that the 2D unit-vector has a positive dot product with given 2D orientation vector.
        static member inline matchVcOrientation (orientationToMatch:Vc) (vecToFlip:UnitVc) =
            if orientationToMatch *** vecToFlip < 0.0 then -vecToFlip else vecToFlip

        /// Checks if the angle between the two 2D unit-vectors is less than 90 degrees.
        /// Calculates the dot product of two 2D unit-vectors.
        /// Then checks if it is bigger than 1e-12.
        static member inline matchesOrientation (other:UnitVc) (v:UnitVc) =
            v.MatchesOrientation other

        /// Checks if the angle between the two 2D unit-vectors is more than 90 degrees.
        /// Calculates the dot product of two 2D unit-vectors.
        /// Then checks if it is smaller than -1e-12.
        static member inline isOppositeOrientation (other:UnitVc) (v:UnitVc) =
            v.IsOppositeOrientation other


        /// Checks if Angle between two vectors is Below 0.25 Degree.
        /// Ignores vector orientation.
        /// Same as isAngleBelowQuatreDegree.
        static member inline areParallel (other:UnitVc) (v:UnitVc) =
            v.IsParallelTo other

        /// Checks if Angle between two vectors is Below 0.25 Degree.
        /// Takes vector orientation into account.
        static member inline areParallelAndMatchOrientation (other:UnitVc) (v:UnitVc) =
            v.IsParallelAndOrientedTo other

        /// Checks if Angle between two vectors is between 89.75 and 90.25 Degree.
        /// Ignores vector orientation.
        static member inline arePerpendicular(other:UnitVc) (v:UnitVc) =
            v.IsPerpendicularTo other

        /// Rotate the a 2D unit-vector Counter Clockwise by a 2D Rotation (that has cos and sin precomputed)
        static member inline rotateBy (r:Rotation2D) (v:UnitVc) =
            UnitVc.createUnchecked (
                r.Cos*v.X - r.Sin*v.Y,
                r.Sin*v.X + r.Cos*v.Y)

        /// Rotate the 2D unit-vector in Degrees. Counter Clockwise.
        /// For better Performance recompute the Rotate2D struct and use its member to rotate. see UnitVc.rotateBy.
        static member inline rotate (angDegree) (vec:UnitVc) =
            UnitVc.rotateBy (Rotation2D.createFromDegrees angDegree) vec

        /// 90 Degree rotation Counter-Clockwise.
        static member inline rotate90CCW (v:UnitVc) =
            UnitVc.createUnchecked( -v.Y, v.X )

        /// 90 Degree rotation clockwise.
        static member inline rotate90CW (v:UnitVc) =
            UnitVc.createUnchecked(  v.Y, -v.X )

        /// Linearly interpolates between two vectors.
        /// e.g. rel=0.5 will return the middle vector, rel=1.0 the end vector,
        /// rel=1.5 a vector half the distance beyond the end vector.
        static member lerp (start:UnitVc, ende:UnitVc, rel:float) : Vc =
            start + rel * (ende - start)

        /// Spherically interpolates between start and end by amount rel (0.0 to 1.0).
        /// The difference between this and linear interpolation (aka, "lerp") is that the vectors are treated as directions rather than points in space.
        /// The direction of the returned vector is interpolated by the angle and its magnitude is interpolated between the magnitudes of start and end.
        /// Interpolation continues before and after the range of 0.0 and 1.0
        static member slerp (start:UnitVc, ende:UnitVc, rel:float) :UnitVc =
            // https://en.wikipedia.org/wiki/Slerp
            // implementation tested in Rhino!
            let dot = start *** ende
            if dot > float Cosine.``0.05`` then  // vectors are in the same direction interpolate length only
                start
            elif dot < float Cosine.``179.95`` then
                fail2 "UnitVc.slerp vectors are 180 deg opposite." start ende  |> unbox // unbox to make type checker happy
            else
                let ang = acos(dot) // the angle between the two vectors
                let p = ende - start*dot  // a vector perpendicular to start and in the same plane with ende.
                let perp = UnitVc.create(p.X, p.Y)
                let theta = ang*rel // the angle part we want for the result
                let theta360 = (theta+UtilEuclid.twoPi) % UtilEuclid.twoPi // make sure it is in the range 0.0 to 2 Pi (360 degrees)
                let cosine = cos (theta360)
                let sine   = sqrt(1.0 - cosine*cosine)
                if theta360 < Math.PI then  // in the range 0 to 180 degrees, only applicable if rel is beyond 0.0 or 1.0
                    let v = start * cosine + perp * sine
                    UnitVc.createUnchecked(v.X, v.Y)
                else
                    let v = start * cosine - perp * sine
                    UnitVc.createUnchecked(v.X, v.Y)

        /// Checks if 2D unit-vector is parallel to the world X axis. Ignoring orientation.
        /// The absolute deviation tolerance along Y axis is 1e-9.
        static member inline isXAligned (v:UnitVc) =
            v.IsXAligned

        /// Checks if 2D unit-vector is parallel to the world Y axis. Ignoring orientation.
        /// The absolute deviation tolerance along X axis is 1e-9.
        static member inline isYAligned (v:UnitVc) =
            v.IsYAligned

        [<Obsolete("Use Euclid.XLine2D module instead.")>]
        static member intersection( ptA:Pt, ptB:Pt, vA:UnitVc, vB:UnitVc ) : Option<float*float> =
            Some (XLine2D.parameters( ptA.X, ptA.Y, vA.X, vA.Y, ptB.X, ptB.Y, vB.X, vB.Y ))

        [<Obsolete("Use UnitVc.isParallelWithin instead.")>]
        static member inline isAngle90Below (cosineValue: float<Cosine.cosine>) (a:UnitVc) (b:UnitVc) =
            UnitVc.isParallelWithin cosineValue a b

        [<Obsolete("Use UnitVc.isNotParallelWithin instead.")>]
        static member inline isAngle90Above (cosineValue: float<Cosine.cosine>) (a:UnitVc) (b:UnitVc) =
            UnitVc.isNotParallelWithin cosineValue a b

        [<Obsolete("Use UnitVc.isAngleBelow instead.")>]
        static member inline isAngle180Below (cosineValue: float<Cosine.cosine>) (a:UnitVc) (b:UnitVc) =
            UnitVc.isAngleBelow cosineValue a b

        [<Obsolete("Use UnitVc.isAngleAbove instead.")>]
        static member inline isAngle180Above (cosineValue: float<Cosine.cosine>) (a:UnitVc) (b:UnitVc) =
            UnitVc.isAngleAbove cosineValue a b
