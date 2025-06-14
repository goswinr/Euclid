namespace Euclid
open System
open UtilEuclid

#nowarn "44" // to skip Obsolete warnings (members just needs to be public for inlining, but should be hidden)

/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type Vc.
[<AutoOpen>]
module AutoOpenVc =

    type Vc with

        /// Convert 2D vector to 2D point.
        member inline v.AsPt =
            Pt(v.X, v.Y)

        /// Convert 2D vector to 3D vector using 0.0 as Z value.
        /// If you want a different Z value use the member v.WithZ(z)
        member inline v.AsVec =
            Vec(v.X, v.Y, 0.0)

        /// Convert 2D vector to 3D point using 0.0 as Z value.
        member inline v.AsPnt =
            Pnt(v.X, v.Y, 0.0)

        /// Returns a boolean indicating whether X and Y are exactly 0.0.
        member inline v.IsZero =
            v.X = 0.0 && v.Y = 0.0

        /// Returns a boolean indicating if any of X and Y is not exactly 0.0.
        member inline v.IsNotZero =
            v.X <> 0.0 || v.Y <> 0.0

        /// Check if the 2D vector is shorter than the tolerance.
        /// Also checks if any component is a NaN.
        member inline v.IsTiny tol =
            not (v.Length > tol)

        /// Check if the 2D vectors square length is shorter than the squared tolerance.
        /// Also checks if any component is a NaN.
        member inline v.IsTinySq tol =
            not (v.LengthSq > tol)

        //member inline v.Length moved to Vc type declaration
        //member inline v.LengthSq moved to Vc type declaration

        /// Returns a new 2D vector with new X coordinate, Y stays the same.
        member inline v.WithX x =
            Vc (x, v.Y)

        /// Returns a new 2D vector with new Y coordinate, X stays the same.
        member inline v.WithY y =
            Vc (v.X, y)

        /// Returns new 3D vector with Z coordinate, X and Y stay the same.
        /// If you want Z to be 0.0 you can use v.AsVec too.
        member inline v.WithZ z =
            Vec (v.X, v.Y, z)

        /// Returns a new 2D vector with half the length.
        member inline v.Half =
            Vc (v.X*0.5, v.Y*0.5)

        /// A separate function to compose the error message that does not get inlined.
        [<Obsolete("Not actually obsolete but just hidden. (Needs to be public for inlining of the functions using it.)")>]
        member v.FailedWithLength(desiredLength) = EuclidDivByZeroException.Raisef "Euclid.Vc.WithLength %g : %O is too small for unitizing, Tolerance:%g" desiredLength v zeroLengthTolerance

        /// Returns a new 2D vector scaled to the desired length.
        /// Same as Vc.withLength.
        member inline v.WithLength (desiredLength:float) =
            let l = v.Length
            if isTooTiny l then v.FailedWithLength(desiredLength) // don't compose error msg directly here to keep inlined code small.
            v * (desiredLength / l)

        /// A separate function to compose the error message that does not get inlined.
        [<Obsolete("Not actually obsolete but just hidden. (Needs to be public for inlining of the functions using it.)")>]
        member v.FailedUnitized() = EuclidDivByZeroException.Raisef "Euclid.Vc.Unitized %O is too small for unitizing, Tolerance:%g" v zeroLengthTolerance

        /// Returns the 2D vector unitized.
        /// Fails with EuclidDivByZeroException if the length of the vector is
        /// too small (1e-16) to unitize.
        member inline v.Unitized =
            let l = sqrt(v.X * v.X  + v.Y * v.Y)
            if isTooTiny l then v.FailedUnitized() // don't compose error msg directly here to keep inlined code small.
            UnitVc.createUnchecked(v.X/l, v.Y/l)

        /// Test if the 2D vector is a unit-vector.
        /// Tests if square length is within 6 float steps of 1.0
        /// So between 0.99999964 and 1.000000715.
        member inline v.IsUnit =
            UtilEuclid.isOne v.LengthSq

        /// The 2D Cross Product of two 2D vectors.
        /// It is just a scalar equal to the signed square area of the parallelogram spanned by the input vectors.
        /// If the rotation from 'a' to 'b' is Counter-Clockwise the result is positive.
        member inline a.Cross (b:Vc) =
            a.X*b.Y - a.Y*b.X

        /// The 2D Cross Product of a 2D vector with a 2D unit-vector.
        /// It is just a scalar equal to the signed square area of the parallelogram spanned by the input vectors.
        /// If the rotation from 'a' to 'b' is Counter-Clockwise the result is positive.
        member inline a.Cross (b:UnitVc) =
            a.X*b.Y - a.Y*b.X

        /// Dot product, or scalar product of two 2D vectors.
        /// Returns a float.
        member inline a.Dot ( b:Vc) =
            a.X * b.X + a.Y * b.Y

        /// Dot product, or scalar product of a 2D vector with a 2D unit-vector.
        /// Returns a float.
        /// This float is the projected length of the 2D vector on the direction of the unit-vector.
        member inline a.Dot ( b:UnitVc) =
            a.X * b.X + a.Y * b.Y


        /// Rotate the a 2D vector Counter Clockwise by a 2D Rotation (that has cos and sin precomputed)
        member inline v.RotateBy (r:Rotation2D) =
            Vc(r.Cos*v.X - r.Sin*v.Y,
                r.Sin*v.X + r.Cos*v.Y)

        /// Rotate the 2D vector in Degrees. Counter Clockwise.
        /// For better Performance precomputed the Rotate2D struct and use its member to rotate. see Vc.RotateBy.
        member inline v.Rotate (angDegree) =
            v.RotateBy (Rotation2D.createFromDegrees angDegree)

        /// 90 Degree rotation Counter-Clockwise.
        member inline v.Rotate90CCW =
            Vc( -v.Y, v.X)

        /// 90 Degree rotation clockwise.
        member inline v.Rotate90CW =
            Vc(v.Y, -v.X)

        /// A separate function to compose the error message that does not get inlined.
        [<Obsolete("Not actually obsolete but just hidden. (Needs to be public for inlining of the functions using it.)")>]
        member v.FailedDirectionDiamond() = EuclidDivByZeroException.Raisef "Euclid.Vc.DirectionDiamond: input vector is zero length: %O" v

        /// The diamond angle.
        /// Calculates the proportion of X to Y component.
        /// It is always positive and in the range of 0.0 to 4.0 (for 360 Degrees)
        /// 0.0 = Xaxis, going Counter-Clockwise.
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        member inline v.DirectionDiamond =
            // https://stackoverflow.com/a/14675998/969070
            //#if DEBUG
            if isTooTiny (abs v.X + abs v.Y) then v.FailedDirectionDiamond() // might return NaN without this check  // don't compose error msg directly here to keep inlined code small.
            //#endif
            if v.Y >= 0.0 then
                if v.X >= 0.0 then
                    v.Y / (v.X + v.Y)
                else
                    1.0 - v.X / (-v.X + v.Y)
            else
                if v.X < 0.0 then
                    2.0 - v.Y / (-v.X - v.Y)
                else
                    3.0 + v.X / (v.X - v.Y)


        /// A separate function to compose the error message that does not get inlined.
        [<Obsolete("Not actually obsolete but just hidden. (Needs to be public for inlining of the functions using it.)")>]
        member v.FailedDirection2Pi() = EuclidDivByZeroException.Raisef "Euclid.Vc.Direction2Pi: input vector is zero length: %O" v

        /// Returns the angle in Radians from X-axis.
        /// Going Counter-Clockwise till two Pi.
        member inline v.Direction2Pi =
            // https://stackoverflow.com/a/14675998/969070
            //#if DEBUG
            if isTooTiny (abs v.X + abs v.Y) then v.FailedDirection2Pi() // don't compose error msg directly here to keep inlined code small.
            //#endif
            let a = Math.Atan2(v.Y, v.X)
            if a < 0. then
                a + UtilEuclid.twoPi
            else
                a

        /// A separate function to compose the error message that does not get inlined.
        [<Obsolete("Not actually obsolete but just hidden. (Needs to be public for inlining of the functions using it.)")>]
        member v.FailedDirectionPi() = EuclidDivByZeroException.Raisef "Euclid.Vc.DirectionPi: input vector is zero length: %O" v

        /// Returns the angle in Radians from X-axis.
        /// Going Counter-Clockwise till two Pi.
        /// Ignores orientation.
        /// Range 0.0 to Pi.
        member inline v.DirectionPi =
            // https://stackoverflow.com/a/14675998/969070
            //#if DEBUG // TODO : with this test all  operations are 2.5 times slower
            if isTooTinySq(v.LengthSq)  then v.FailedDirectionPi() // don't compose error msg directly here to keep inlined code small.
            //#endif
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
        member inline v.AngleDiamondTo (b:Vc) =
            let r = b.DirectionDiamond - v.DirectionDiamond
            if r >= 0. then  r
            else r + 4.0


        /// Checks if the angle between the two 2D vectors is less than 180 degrees.
        /// Calculates the dot product of two 2D vectors.
        /// Then checks if it is bigger than 1e-12.
        /// Fails if any of the two vectors is shorter than zeroLengthTolerance  (1e-12).
        member inline v.MatchesOrientation (other:Vc) =
            if isTooTinySq(v.LengthSq    ) then EuclidException.Raisef "Euclid.Vc.MatchesOrientation: Vc 'this' is too short: %s. 'other':%s " v.AsString other.AsString
            if isTooTinySq(other.LengthSq) then EuclidException.Raisef "Euclid.Vc.MatchesOrientation: Vc 'other' is too short: %s. 'this':%s " other.AsString v.AsString
            v *** other > 1e-12

        /// Checks if the angle between this 2D vectors and a 2D unit-vector is less than 180 degrees.
        /// Calculates the dot product of a 2D vector and a unit-vectors.
        /// Then checks if it is bigger than 1e-12.
        /// Fails if the vector is shorter than zeroLengthTolerance  (1e-12).
        member inline v.MatchesOrientation (other:UnitVc) =
            if isTooTinySq(v.LengthSq) then EuclidException.Raisef "Euclid.Vc.MatchesOrientation: Vc 'this' is too short: %s. 'other':%s " v.AsString other.AsString
            v *** other > 1e-12

        /// Checks if the angle between the two 2D vectors is more than 180 degrees.
        /// Calculates the dot product of two 2D vectors.
        /// Then checks if it is smaller than -1e-12.
        /// Fails if any of the two vectors is zero length .
        member inline v.IsOppositeOrientation (other:Vc) =
            if isTooTinySq(v.LengthSq    ) then EuclidException.Raisef "Euclid.Vc.IsOppositeOrientation: Vc 'this'  is too short: %s. 'other':%s " v.AsString other.AsString
            if isTooTinySq(other.LengthSq) then EuclidException.Raisef "Euclid.Vc.IsOppositeOrientation: Vc 'other' is too short: %s. 'this':%s " other.AsString v.AsString
            v *** other < -1e-12

        /// Checks if the angle between this 2D vectors and a 2D unit-vector is more than 180 degrees.
        /// Calculates the dot product of a 2D vector and a unit-vectors.
        /// Then checks if it is smaller than minus 1e-12.
        /// Fails if the vector is shorter than zeroLengthTolerance  (1e-12).
        member inline v.IsOppositeOrientation (other:UnitVc) =
            if isTooTinySq(v.LengthSq) then EuclidException.Raisef "Euclid.Vc.IsOppositeOrientation: Vc 'this' is too short: %s. 'other':%s " v.AsString other.AsString
            v *** other < -1e-12


        /// Checks if 2D vector is parallel to the world X axis. Ignoring orientation.
        /// The absolute deviation tolerance along Y axis is 1e-9.
        /// Fails on vectors shorter than 1e-6.
        member inline v.IsXAligned =
            let x = abs (v.X)
            let y = abs (v.Y)
            if isTooSmall (x+y) then EuclidException.Raisef "Euclid.Vc.IsXAligned cannot not check very tiny vector. (tolerance 1e-6) %O" v
            else y < 1e-9

        /// Checks if 2D vector is parallel to the world Y axis. Ignoring orientation.
        /// The absolute deviation tolerance along X axis is 1e-9.
        /// Fails on vectors shorter than 1e-6.
        member inline v.IsYAligned =
            let x = abs (v.X)
            let y = abs (v.Y)
            if isTooSmall (x+y)then EuclidException.Raisef "Euclid.Vc.IsYAligned cannot not check very tiny vector. (tolerance 1e-6) %O" v
            else x < 1e-9

        /// Checks if two 2D vectors are parallel.
        /// Ignores the line orientation.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        member inline this.IsParallelTo(other:Vc, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
            let sa = this.LengthSq
            if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Vc.IsParallelTo: Vc 'this' is too short: %s. 'other':%s " this.AsString other.AsString
            let sb = other.LengthSq
            if isTooTinySq(sb) then EuclidException.Raisef "Euclid.Vc.IsParallelTo: Vc 'other' is too short: %s. 'this':%s " other.AsString this.AsString
            let au = this * (1.0 / sqrt sa)
            let bu = other * (1.0 / sqrt sb)
            abs(bu *** au) > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:

        /// Checks if this 2D vectors and a 2D unit-vector are parallel.
        /// Ignores the line orientation.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        member inline this.IsParallelTo(other:UnitVc, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
            let sa = this.LengthSq
            if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Vc.IsParallelTo: Vc 'this' is too short: %s. 'other':%s " this.AsString other.AsString
            let au = this * (1.0 / sqrt sa)
            abs(other *** au) > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:


        /// Checks if two 2D vectors are parallel.
        /// Takes the line orientation into account too.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        member inline this.IsParallelAndOrientedTo (other:Vc, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
            let sa = this.LengthSq
            if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Vc.IsParallelAndOrientedTo: Vc 'this' is too short: %s. 'other':%s " this.AsString other.AsString
            let sb = other.LengthSq
            if isTooTinySq(sb) then EuclidException.Raisef "Euclid.Vc.IsParallelAndOrientedTo: Vc 'other' is too short: %s. 'this':%s " other.AsString this.AsString
            let au = this  * (1.0 / sqrt sa)
            let bu = other * (1.0 / sqrt sb)
            bu *** au > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:

        /// Checks this 2D vectors and a 2D unit-vector are parallel.
        /// Takes the line orientation into account too.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        member inline this.IsParallelAndOrientedTo (other:UnitVc, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
            let sa = this.LengthSq
            if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Vc.IsParallelAndOrientedTo: Vc 'this' is too short: %s. 'other':%s " this.AsString other.AsString
            let au = this * (1.0 / sqrt sa)
            other *** au > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:



        /// Checks if two 2D vectors are perpendicular to each other.
        /// The default angle tolerance is 89.75 to 90.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// The default cosine is 0.0043633 ( = 89.75 deg)
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        member inline this.IsPerpendicularTo (other:Vc, [<OPT;DEF(Cosine.``89.75``)>] maxCosine:float<Cosine.cosine> ) =
            let sa = this.LengthSq
            if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Vc.IsPerpendicularTo: Vc 'this' is too short: %s. 'other':%s " this.AsString other.AsString
            let sb = other.LengthSq
            if isTooTinySq(sb) then EuclidException.Raisef "Euclid.Vc.IsPerpendicularTo: Vc 'other' is too short: %s. 'this':%s " other.AsString this.AsString
            let au = this  * (1.0 / sqrt sa)
            let bu = other * (1.0 / sqrt sb)
            let d = bu *** au
            float -maxCosine < d && d  < float maxCosine // = cosine of 98.75 and 90.25 degrees

        /// Checks if this 2D vectors and a 2D unit-vector are perpendicular to each other.
        /// The default angle tolerance is 89.75 to 90.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// The default cosine is 0.0043633 ( = 89.75 deg)
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        member inline this.IsPerpendicularTo (other:UnitVc, [<OPT;DEF(Cosine.``89.75``)>] maxCosine:float<Cosine.cosine> ) =
            let sa = this.LengthSq
            if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Vc.IsPerpendicularTo: Vc 'this' is too short: %s. 'other':%s " this.AsString other.AsString
            let au = this * (1.0 / sqrt sa)
            let d = other *** au
            float -maxCosine < d && d  < float maxCosine // = cosine of 98.75 and 90.25 degrees



        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------

        /// Returns the World X-axis with length one: Vc(1, 0)
        static member inline Xaxis =
            Vc(1, 0)

        /// Returns the World Y-axis with length one: Vc(0, 1)
        static member inline Yaxis =
            Vc(0, 1)

        /// Checks if two 2D vectors are equal within tolerance.
        /// Identical vectors in opposite directions are not considered equal.
        /// Use a tolerance of 0.0 to check for an exact match.
        static member inline equals (tol:float) (a:Vc) (b:Vc) =
            abs (a.X-b.X) <= tol &&
            abs (a.Y-b.Y) <= tol


        /// Check if two 2D vectors  are not equal within a given tolerance.
        /// Use a tolerance of 0.0 to check if the two vectors are not exactly equal.
        static member notEquals (tol:float) (a:Vc) (b:Vc) =
            abs (a.X-b.X) > tol ||
            abs (a.Y-b.Y) > tol

        /// Returns the distance between the tips of two 2D vectors.
        static member inline difference (a:Vc) (b:Vc) =
            let v = a-b
            sqrt(v.X*v.X + v.Y*v.Y)

        /// Returns the squared distance between the tips of two 2D vectors.
        /// This operation is slightly faster than Vc.difference and sufficient for many algorithms like finding closest vectors.
        static member inline differenceSq (a:Vc) (b:Vc) =
            let v = a-b
            v.X*v.X + v.Y*v.Y

        /// A separate function to compose the error message that does not get inlined.
        [<Obsolete("Not actually obsolete but just hidden. (Needs to be public for inlining of the functions using it.)")>]
        static member failedCreateFromMembersXY(v:'T,e:exn) = EuclidException.Raise $"Euclid.Vc.createFromMembersXY: {v} could not be converted to a Euclid.Vc:{Format.nl}{e}"
        /// Accepts any type that has a X and Y (UPPERCASE) member that can be converted to a float.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline createFromMembersXY vec =
            let x = ( ^T : (member X : _) vec)
            let y = ( ^T : (member Y : _) vec)
            try Vc(float x, float y)
            with e -> Vc.failedCreateFromMembersXY(vec,e)

        /// A separate function to compose the error message that does not get inlined.
        [<Obsolete("Not actually obsolete but just hidden. (Needs to be public for inlining of the functions using it.)")>]
        static member failedCreateFromMembersxy(v:'T,e:exn) = EuclidException.Raise $"Euclid.Vc.createFromMembersxy: {v} could not be converted to a Euclid.Vc:{Format.nl}{e}"
        /// Accepts any type that has a x and y (lowercase) member that can be converted to a float.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline createFromMembersxy vec =
            let x = ( ^T : (member x : _) vec)
            let y = ( ^T : (member y : _) vec)
            try Vc(float x, float y)
            with e -> Vc.failedCreateFromMembersxy(vec,e)


         /// Create 2D vector from 2D point.
        static member inline createFromPt (pt:Pnt) =
            Vc(pt.X, pt.Y)

        /// Create 2D vector from 2D unit-vector.
        static member inline createFromUnitVc (v:UnitVc) =
            Vc(v.X, v.Y)

        /// Convert 2D vector to 2D point.
        static member inline asPt(v:Vc) =
            Pt(v.X, v.Y)

        /// Convert 2D vector to 3D point. Using 0.0 as Z value.
        static member inline asPnt(v:Vc) =
            Pnt(v.X, v.Y, 0.0)

        /// Convert 2D vector to 3D vector. Using 0.0 as Z value.
        static member inline asVec(v:Vc) =
            Vec(v.X, v.Y, 0.0)

        /// The 2D Cross Product.
        /// It is also known as the Determinant, Wedge Product or Outer Product.
        /// It is just a scalar equal to the signed square area of the parallelogram spanned by the input vectors.
        /// If the rotation from 'a' to 'b' is Counter-Clockwise the result is positive.
        static member inline cross (a:Vc, b:Vc) =
            a.X * b.Y - a.Y * b.X

        /// The 2D Cross Product.
        /// It is also known as the Determinant, Wedge Product or Outer Product.
        /// It is just a scalar equal to the signed square area of the parallelogram spanned by the input vectors.
        /// If the rotation from 'a' to 'b' is Counter-Clockwise the result is positive.
        static member inline cross (a:UnitVc, b:Vc) =
            a.X * b.Y - a.Y * b.X

        /// The 2D Cross Product.
        /// It is just a scalar equal to the signed square area of the parallelogram spanned by the input vectors.
        /// If the rotation from 'a' to 'b' is Counter-Clockwise the result is positive.
        static member inline cross (a:Vc, b:UnitVc) =
            a.X * b.Y - a.Y * b.X

        /// Dot product, or scalar product of two 2D vectors.
        /// Returns a float.
        static member inline dot (a:Vc, b:Vc) =
            a.X * b.X + a.Y * b.Y

        /// Dot product, or scalar product of a 2D unit-vector with a 2D vector.
        /// Returns a float.
        /// This float is the projected length of the 2D vector on the direction of the unit-vector.
        static member inline dot (a:Vc, b:UnitVc) =
            a.X * b.X + a.Y * b.Y

        /// Dot product, or scalar product of a 2D vector with a 2D unit-vector.
        /// Returns a float.
        /// This float is the projected length of the 2D vector on the direction of the unit-vector.
        static member inline dot (a:UnitVc, b:Vc) =
            a.X * b.X + a.Y * b.Y

        /// Gets the X part of this 2D vector.
        static member inline getX (v:Vc) = v.X

        /// Gets the Y part of this 2D vector.
        static member inline getY (v:Vc) = v.Y

        /// Returns a new 2D vector with new X value, Y stays the same.
        static member inline withX x (v:Vc) = v.WithX x

        /// Returns a new 2D vector with new Y value, X stays the same.
        static member inline withY y (v:Vc) = v.WithY y

        /// Add two 2D vectors together. Returns a new 2D vector.
        static member inline add (a:Vc) (b:Vc) = b + a

        /// Multiplies a 2D vector with a scalar, also called scaling a vector.
        /// Same as Vc.withLength. Returns a new 2D vector.
        static member inline scale (f:float) (v:Vc) =
            Vc (v.X * f, v.Y * f)

        /// Returns a new 2D vector scaled to the desired length.
        /// Same as vc.WithLength. Returns a new 2D vector.
        static member inline withLength(desiredLength:float) (v:Vc) =
            v.WithLength desiredLength

        /// Add to the X part of this 2D vectors together. Returns a new 2D vector.
        static member inline moveX x (v:Vc) =
            Vc (v.X+x, v.Y)

        /// Add to the Y part of this 2D vectors together. Returns a new 2D vector.
        static member inline moveY y (v:Vc) =
            Vc (v.X, v.Y+y)

        /// Check if the 2D vector is shorter than the tolerance.
        /// Also checks if any component is a NaN.
        static member inline isTiny tol (v:Vc) =
            not (v.Length > tol)

        /// Check if the 2D vectors square length is shorter than the squared tolerance.
        /// Also checks if any component is a NaN.
        static member inline isTinySq tol (v:Vc) =
            not (v.LengthSq > tol)

        /// Returns the length of the 2D vector.
        static member inline length (v:Vc) =
            v.Length

        /// Returns the squared length of the 2D vector.
        /// The square length is faster to calculate and often good enough for use cases such as sorting vectors by length.
        static member inline lengthSq (v:Vc) =
            v.LengthSq

        /// Returns a new 2D vector from X and Y parts.
        static member inline create (x:float, y:float) =
            Vc(x, y)

        /// Returns a new 2D vector from start and end point.
        static member inline create (start:Pt, ende:Pt) =
            ende-start

        /// Negate or inverse a 2D vectors. Returns a new 2D vector.
        /// Same as Vc.flip.
        static member inline reverse (v:Vc) =
            -v

        /// Negate or inverse a 2D vectors. Returns a new 2D vector.
        /// Same as Vc.reverse.
        static member inline flip (v:Vc) =
            -v

        /// Returns 2D vector unitized, fails on zero length vectors.
        static member inline unitize (v:Vc) =
            v.Unitized

        /// Unitize 2D vector, if input vector is shorter than 1e-6 the default unit-vector is returned.
        static member inline unitizeOrDefault (defaultUnitVector:UnitVc) (v:Vc) =
            let l = v.LengthSq
            if l < 1e-12  then  // = sqrt (1e-06)
                defaultUnitVector
            else
                let f = 1.0 / sqrt(l)
                UnitVc.createUnchecked(v.X*f, v.Y*f)

        /// Returns angle between two 2D vectors in Radians.
        /// Takes vector orientation into account.
        /// Range 0.0 to Pi ( = 180 Degree)
        static member inline anglePi (a:Vc) (b:Vc) =
            UnitVc.anglePi a.Unitized b.Unitized
            //    let mutable r = b.Direction2Pi  - a.Direction2Pi     // this does perform slightly worse than using unitizing and acos for anglePi
            //    if r < 0.      then  r <- r + UtilEuclid.twoPi
            //    if r > Math.PI then  r <- UtilEuclid.twoPi - r
            //    r

        /// Returns positive angle between two 2D vectors in Radians. Ignores orientation.
        /// Range 0.0 to Pi/2 ( = 90 Degree)
        static member inline angleHalfPi (a:Vc) (b:Vc) =
            UnitVc.angleHalfPi a.Unitized b.Unitized
            //    let mutable r = b.Direction2Pi  - a.Direction2Pi   // this does perform slightly worse than using unitizing and acos for anglePi
            //    if r < 0.      then  r <- r + UtilEuclid.twoPi
            //    if r > Math.PI then  r <- UtilEuclid.twoPi - r
            //    if r > halfPi  then  r <- Math.PI - r
            //    r


        /// Returns positive angle for rotating Counter-Clockwise from vector 'a' to vector 'b' .
        /// In Radians.
        /// Range: 0.0 to 2 Pi ( = 360 Degrees)
        static member inline angle2Pi (a:Vc, b:Vc) =
            let r = b.Direction2Pi  - a.Direction2Pi
            if r >= 0. then  r
            else r + UtilEuclid.twoPi

        /// Returns positive angle between two 2D vectors in Degrees,
        /// Ignores vector orientation.
        /// Range: 0 to 90 Degrees.
        static member inline angle90 (a:Vc) (b:Vc) =
            Vc.angleHalfPi a b |>  toDegrees

        /// Returns positive angle between two 2D vectors in Degrees.
        /// Takes vector orientation into account.
        /// Range 0 to 180 Degrees.
        static member inline angle180 (a:Vc) (b:Vc) =
            Vc.anglePi a b |>  toDegrees


        /// Returns positive angle for rotating Counter-Clockwise from vector 'a' to vector 'b' .
        /// In Degree.
        /// Range: 0 to 360 Degrees
        static member inline angle360 (a:Vc, b:Vc) =
            Vc.angle2Pi (a, b) |> toDegrees

        /// Returns positive angle for rotating Counter-Clockwise from vector 'a' to vector 'b' .
        /// In Diamond Angle. Using only proportion of X to Y components.
        /// Range of 0.0 to 4.0 (for 360 Degrees)
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        static member inline angleDiamond (a:Vc, b:Vc) =
            let r = b.DirectionDiamond - a.DirectionDiamond
            if r >= 0. then  r
            else r + 4.0

        /// The diamond angle.
        /// Calculates the proportion of X to Y component.
        /// It is always positive and in the range of 0.0 to 4.0 (for 360 Degrees)
        /// 0.0 = Xaxis, going Counter-Clockwise.
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        static member inline directionDiamond(a:Vc) =
            // https://stackoverflow.com/a/14675998/969070
            if a.Y >= 0.0 then
                if a.X >= 0.0 then
                    a.Y/(a.X+a.Y)
                else
                    1.0 - a.X/(-a.X+a.Y)
            else
                if a.X < 0.0 then
                    2.0 - a.Y/(-a.X-a.Y)
                else
                    3.0 + a.X/(a.X-a.Y)

        /// Returns positive angle of vector. Counter-Clockwise from X-axis.
        /// In Radians.
        /// Range: 0.0 to 2 Pi ( = 360 Degrees)
        static member inline direction2Pi (v:Vc) =
            v.Direction2Pi

        /// Returns positive angle of vector. Counter-Clockwise from X-axis.
        /// In Degree.
        /// Range: 0 to 360 Degrees
        static member inline direction360 (v:Vc) =
            v.Direction360


        /// Checks if Angle between two vectors is less than given Cosine.
        /// Ignores vector orientation. The angle between two vectors can be 0 to 90 degrees ignoring their direction.
        /// Use the Euclid.Cosine module to get some precomputed cosine values
        /// Fails on zero length vectors, tolerance 1e-12.
        static member inline isAngle90Below (cosineValue: float<Cosine.cosine>) (a:Vc) (b:Vc) =
            let sa = a.LengthSq
            if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Vc.isAngle90Below: Vc a is too short: %s. Vc b:%s " a.AsString b.AsString
            let sb = b.LengthSq
            if isTooTinySq(sb) then EuclidException.Raisef "Euclid.Vc.isAngle90Below: Vc b is too short: %s. Vc a:%s " b.AsString a.AsString
            let au = a * (1.0 / sqrt sa)
            let bu = b * (1.0 / sqrt sb)
            abs(bu *** au) > float cosineValue

        /// Checks if Angle between two vectors is more than given Cosine.
        /// Ignores vector orientation. The angle between two vectors can be 0 to 90 degrees ignoring their direction.
        /// Use the Euclid.Cosine module to get some precomputed cosine values.
        /// Fails on zero length vectors, tolerance 1e-12.
        static member inline isAngle90Above(cosineValue: float<Cosine.cosine>) (a:Vc) (b:Vc) =
            let sa = a.LengthSq
            if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Vc.isAngle90Above: Vc a is too short: %s. Vc b:%s " a.AsString b.AsString
            let sb = b.LengthSq
            if isTooTinySq(sb) then EuclidException.Raisef "Euclid.Vc.isAngle90Above: Vc b is too short: %s. Vc a:%s " b.AsString a.AsString
            let au = a * (1.0 / sqrt sa)
            let bu = b * (1.0 / sqrt sb)
            abs(bu *** au) < float cosineValue


        /// Checks if Angle between two vectors is less than given Cosine.
        /// Does not ignores vector orientation.The angle between two vectors can be 0 to 180 degrees.
        /// Use the Euclid.Cosine module to get some precomputed cosine values
        /// Fails on zero length vectors, tolerance 1e-12.
        static member inline isAngle180Below (cosineValue: float<Cosine.cosine>) (a:Vc) (b:Vc) =
            let sa = a.LengthSq
            if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Vc.isAngle180Below: Vc a is too short: %s. Vc b:%s " a.AsString b.AsString
            let sb = b.LengthSq
            if isTooTinySq(sb) then EuclidException.Raisef "Euclid.Vc.isAngle180Below: Vc b is too short: %s. Vc a:%s " b.AsString a.AsString
            let au = a * (1.0 / sqrt sa)
            let bu = b * (1.0 / sqrt sb)
            bu *** au > float cosineValue

        /// Checks if Angle between two vectors is more than given Cosine.
        /// Does not ignores vector orientation.The angle between two vectors can be 0 to 180 degrees.
        /// Use the Euclid.Cosine module to get some precomputed cosine values.
        /// Fails on zero length vectors, tolerance 1e-12.
        static member inline isAngle180Above(cosineValue: float<Cosine.cosine>) (a:Vc) (b:Vc) =
            let sa = a.LengthSq
            if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Vc.isAngle180Above: Vc a is too short: %s. Vc b:%s " a.AsString b.AsString
            let sb = b.LengthSq
            if isTooTinySq(sb) then EuclidException.Raisef "Euclid.Vc.isAngle180Above: Vc b is too short: %s. Vc a:%s " b.AsString a.AsString
            let au = a * (1.0 / sqrt sa)
            let bu = b * (1.0 / sqrt sb)
            bu *** au < float cosineValue


        /// Returns positive or negative slope of a vector in Radians.
        /// This is the same as the positive or negative angle to the X-axis (or its reverse).
        /// Range -1.57 to +1.57 Radians.
        /// This is just Math.Atan2(v.Y, v.X).
        static member inline slopeRadians (v:Vc) =
            if isTooTinySq(v.LengthSq) then EuclidException.Raisef "Euclid.Vc.slopeRadians: Vc is too short: %s" v.AsString
            let r = Math.Atan2(v.Y, v.X)
            if   r > halfPi  then  r - Math.PI
            elif r < -halfPi then  r + Math.PI
            else r

        /// Returns positive or negative slope of a vector in Degrees.
        /// This is the same as the positive or negative angle to the X-axis(or its reverse).
        /// Range -90 to +90 Degrees.
        /// This is just Math.Atan2(v.Y, v.X).
        static member inline slopeDegrees (v:Vc) =
            Vc.slopeRadians v |> toDegrees


        /// Returns positive or negative slope of a vector in Percent to the X-axis(or its reverse).
        /// 100% = 45 Degrees.
        /// Returns positive (or negative) infinity if line is vertical or input has length zero.
        static member inline slopePercent (v:Vc) =
            100.0 * v.Y / abs(v.X)

        /// Returns positive or negative angle of a vector in Radians from the X-axis.
        /// Range -3.14 to +3.14 Radians.
        /// This is just atan2(v.Y, v.X).
        static member inline angleToXPi (vc:Vc) =
            if isTooTinySq(vc.LengthSq) then EuclidException.Raisef "Euclid.Vc.angleToXPi: Vc is too short: %s" vc.AsString
            Math.Atan2(vc.Y, vc.X)


        /// Returns positive or negative angle of a vector in Degrees from the X-axis.
        /// Range -180 to +180 Degrees.
        /// This is just atan2(v.Y, v.X) to degrees.
        static member inline angleToX180 (v:Vc) =
            Vc.angleToXPi v |> toDegrees

        /// Returns a  bisector vector in the middle direction.
        /// Code : a.Unitized + b.Unitized
        static member inline bisector (a:Vc) (b:Vc) = a.Unitized + b.Unitized

        /// Ensure that the 2D  vector has a positive dot product with given 2D orientation vector.
        static member inline matchOrientation (orientationToMatch:Vc) (vecToFlip:Vc) =
            if orientationToMatch *** vecToFlip < 0.0 then -vecToFlip else vecToFlip

        /// Ensure that the 2D vector has a positive dot product with given 2D orientation unit-vector.
        static member inline matchUnitVcOrientation (orientationToMatch:UnitVc) (vecToFlip:Vc) =
            if orientationToMatch *** vecToFlip < 0.0 then -vecToFlip else vecToFlip

        /// Checks if the angle between the two 2D vectors is less than 180 degrees.
        /// Calculates the dot product of two 2D vectors.
        /// Then checks if it is bigger than 1e-12.
        /// If any of the two vectors is zero length returns false.
        static member inline matchesOrientation (v:Vc) (other:Vc) =
            v.MatchesOrientation other


        /// Checks if the angle between the two 2D vectors is more than 180 degrees.
        /// Calculates the dot product of two 2D vectors.
        /// Then checks if it is smaller than minus 1e-12.
        /// If any of the two vectors is zero length returns false.
        static member inline isOppositeOrientation (v:Vc) (other:Vc) =
            v.IsOppositeOrientation other

        /// Checks if Angle between two vectors is Below 0.25 Degree.
        /// Ignores vector orientation.
        /// Fails on zero length vectors, tolerance 1e-12.
        static member inline areParallel (other:Vc) (v:Vc) =
            v.IsParallelTo other

        /// Checks if Angle between two vectors is between 98.75 and 90.25 Degree.
        /// Ignores vector orientation.
        /// Fails on zero length vectors, tolerance 1e-12.
        static member inline areParallelAndMatchOrientation (other:Vc) (v:Vc) =
            v.IsParallelAndOrientedTo other

        /// Checks if Angle between two vectors is between 98.75 and 90.25 Degree.
        /// Ignores vector orientation.
        /// Fails on zero length vectors, tolerance 1e-12.
        static member inline arePerpendicular(other:Vc) (v:Vc) =
            v.IsPerpendicularTo(other)


        /// Rotate the a 2D vector Counter Clockwise by a 2D Rotation (that has cos and sin precomputed)
        static member inline rotateBy (r:Rotation2D) (v:Vc) =
            Vc(r.Cos*v.X - r.Sin*v.Y,
               r.Sin*v.X + r.Cos*v.Y)

        /// Rotate the 2D vector in Degrees. Counter Clockwise.
        /// For better Performance precomputed the Rotate2D struct and use its member to rotate. see Vc.rotateBy.
        static member inline rotate (angDegree) (vec:Vc) =
            Vc.rotateBy (Rotation2D.createFromDegrees angDegree) vec


        /// 90 Degree rotation Counter-Clockwise.
        static member inline rotate90CCW (v:Vc) =
            Vc( -v.Y, v.X)

        /// 90 Degree rotation clockwise.
        static member inline rotate90CW (v:Vc) =
            Vc( v.Y, -v.X)


        /// Rotates a vector by a given number of quarter-circles (i.e. multiples of 90
        /// degrees or Pi/2 radians). A positive number rotates counter-clockwise, a
        /// negative number rotates clockwise. The length of the vector is preserved.
        static member rotateByQuarterCircle (numberOfQuarters) (v:Vc)=
            let mutable nQuad = numberOfQuarters % 4
            if nQuad < 0 then nQuad <- nQuad + 4
            match nQuad with
            | 0 -> v
            | 1 -> Vc(-v.Y, v.X)
            | 2 -> Vc(-v.X, -v.Y)
            | 3 -> Vc(v.Y, -v.X)
            | _ -> Vc.Xaxis // should never happen


        /// Linearly interpolates between two vectors.
        /// e.g. rel=0.5 will return the middle vector, rel=1.0 the end vector,
        /// rel=1.5 a vector half the distance beyond the end vector.
        static member lerp (start:Vc, ende:Vc, rel:float) =
            start + rel * (ende - start)

        /// Spherically interpolates between start and end by amount rel (0.0 to 1.0).
        /// The difference between this and linear interpolation (aka, "lerp") is that the vectors are treated as directions rather than points in space.
        /// The direction of the returned vector is interpolated by the angle and its magnitude is interpolated between the magnitudes of start and end.
        /// Interpolation continues before and after the range of 0.0 and 0.1
        static member slerp (start:Vc, ende:Vc, rel:float) =
            // https://en.wikipedia.org/wiki/Slerp
            // implementation tested in Rhino!
            let sLen = start.Length
            let eLen = ende.Length
            if isTooTiny sLen then EuclidDivByZeroException.Throw1 "Euclid.Vc.slerp: Can't interpolate from zero length vector:" start
            if isTooTiny eLen then EuclidDivByZeroException.Throw1 "Euclid.Vc.slerp: Can't interpolate to zero length vector:" ende
            let fs = 1.0 / sLen
            let fe = 1.0 / eLen
            let su  = start * fs //unitized start vector
            let eu  = ende  * fe //unitized end   vector
            let dot = su *** eu
            if dot > float Cosine.``0.05`` then  // vectors are in the same direction interpolate length only
                Vc.lerp(start, ende, rel)
            elif dot < float Cosine.``179.95`` then
                EuclidDivByZeroException.Throw1 "Euclid.Vc.slerp: Can't interpolate vectors in opposite directions:" ende
            else
                let ang = acos(dot) // the angel between the two vectors
                let perp = eu - su*dot |> Vc.unitize // a vector perpendicular to start and in the same plane with ende.
                let theta = ang*rel // the angle part we want for the result
                let theta360 = (theta+UtilEuclid.twoPi) % UtilEuclid.twoPi // make sure it is i the range 0.0 to 2 Pi (360 degrees)
                let cosine = cos (theta360)
                let sine   = sqrt(1.0 - cosine*cosine)
                let res =  //unitized result vector
                    if theta360 < Math.PI then  // in the range 0 to 180 degrees, only applicable if rel is beyond 0.0 or 0.1
                        su * cosine + perp * sine
                    else
                        su * cosine - perp * sine
                let lenRel = sLen + rel * (eLen-sLen)
                if lenRel < 0.0 then
                    Vc.Zero // otherwise the vector would get flipped and grow again , only applicable if rel is beyond 0.0 or 0.1
                else
                    res * abs lenRel

        // Checks if 2D vector is parallel to the world X axis. Ignoring orientation.
        /// Tolerance is 1e-6.
        /// Fails on vectors shorter than 1e-6.
        static member inline isXAligned (v:Vc) =
            v.IsXAligned

        /// Checks if 2D vector is parallel to the world Y axis. Ignoring orientation.
        /// Tolerance is 1e-6.
        /// Fails on vectors shorter than 1e-6.
        static member inline isYAligned (v:Vc) =
            v.IsYAligned


        ///<summary> Intersects two infinite 2D lines.
        /// The lines are defined by a start point and a vector.</summary>
        ///<param name="ptA"> The start point of the first line.</param>
        ///<param name="ptB"> The start point of the second line.</param>
        ///<param name="vA" > The vector of the first line.</param>
        ///<param name="vB" > The vector of the second line.</param>
        ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
        ///  If one or both vectors are shorter than this ValueNone is returned.</param>
        ///<param name="relAngleDiscriminant"> This is an optional tolerance for the internally calculated relative Angle Discriminant.
        /// The default value corresponds to approx 0.25 degree. Below this angle vectors are considered parallel.
        /// See module Euclid.UtilEuclid.RelAngleDiscriminant</param>
        ///<returns> For (almost) zero length or (almost) parallel vectors: ValueNone
        /// Else ValueSome with a tuple of the parameters at which the two infinite 2D Lines intersect to each other.
        /// The tuple's order corresponds to the input order.</returns>
        static member intersection( ptA:Pt,
                                    ptB:Pt,
                                    vA:Vc,
                                    vB:Vc,
                                    [<OPT;DEF(1e-6)>] tooShortTolerance:float,
                                    [<OPT;DEF(RelAngleDiscriminant.``0.25``)>] relAngleDiscriminant:float<RelAngleDiscriminant.relAngDiscr>
                                    ) : ValueOption<float*float> =
            //https://stackoverflow.com/a/34604574/969070 but DP and DQ are in wrong order !
            let ax = -vA.X
            let ay = -vA.Y
            let bx = -vB.X
            let by = -vB.Y
            let vx = ptB.X - ptA.X
            let vy = ptB.Y - ptA.Y
            let a = ax*ax + ay*ay // square length of A
            let b = ax*bx + ay*by // dot product of both lines
            let c = bx*bx + by*by // square length of B
            if a < tooShortTolerance * tooShortTolerance then  // vec A too short
                ValueNone
            elif c < tooShortTolerance * tooShortTolerance then  // vec B too short
                ValueNone
            else
                let ac = a * c // square of square length, never negative
                let bb = b * b // never negative
                let discriminant = ac - bb // never negative, the dot product cannot be bigger than the two square length multiplied with each other
                let div          = ac + bb // never negative
                // getting the relation between the sum and the subtraction gives a good estimate of the angle between the lines
                // see module Euclid.UtilEuclid.RelAngleDiscriminant
                let rel = discriminant / div
                if rel < float relAngleDiscriminant then //parallel
                    ValueNone
                else
                    let e = bx*vx + by*vy
                    let d = ax*vx + ay*vy
                    let t = (b * e - c * d) / discriminant
                    let u = (a * e - b * d) / discriminant
                    ValueSome (t, u)
