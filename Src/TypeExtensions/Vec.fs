namespace Euclid
open System

#nowarn "44" // to skip Obsolete warnings (members just needs to be public for inlining, but should be hidden)

/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type Vec.
[<AutoOpen>]
module AutoOpenVec =
    open UtilEuclid

    type Vec with

        /// Convert 3D vector to 3D point.
        member inline v.AsPnt =
            Pnt(v.X, v.Y, v.Z)

        /// Convert 3D vector to 2D vector, discarding the Z value.
        member inline v.AsVc =
            Vc(v.X, v.Y)

        /// Returns a boolean indicating wether X, Y and Z are all exactly 0.0.
        member inline v.IsZero =
            v.X = 0.0 && v.Y = 0.0 && v.Z = 0.0

        /// Returns a boolean indicating if any of X, Y and Z is not exactly 0.0.
        member inline v.IsNotZero =
            not v.IsZero

        /// Check if the 3D vector is shorter than the tolerance.
        /// Also checks if any component is a NaN.
        member inline v.IsTiny tol =
            not (v.Length > tol)

        /// Check if the 3D vectors square length is shorter than the squared tolerance.
        /// Also checks if any component is a NaN.
        member inline v.IsTinySq tol =
            not (v.LengthSq > tol)


        /// Returns the length of the 3D vector projected into World X-Y plane.
        member inline v.LengthInXY =
            sqrt (v.X*v.X + v.Y*v.Y)

        /// Returns the squared length of the 3D vector projected into World X-Y plane.
        /// The square length is faster to calculate and often good enough for use cases such as sorting vectors by length.
        member inline v.LengthSqInXY =
            v.X*v.X + v.Y*v.Y

        /// Returns  a new 3D vector with new X coordinate, Y and Z stay the same.
        member inline v.WithX x =
            Vec (x, v.Y, v.Z)

        /// Returns a new 3D vector with new y coordinate, X and Z stay the same.
        member inline v.WithY y =
            Vec (v.X, y, v.Z)

        /// Returns a new 3D vector with new z coordinate, X and Y stay the same.
        member inline v.WithZ z =
            Vec (v.X, v.Y, z)

        /// Returns a new 3D vector with half the length.
        member inline v.Half =
            Vec (v.X*0.5, v.Y*0.5, v.Z*0.5)

        /// A separate function to compose the error message that does not get inlined.
        [<Obsolete("Not actually obsolete but just hidden. (Needs to be public for inlining of the functions using it.)")>]
        member v.FailedWithLength(desiredLength) = EuclidDivByZeroException.Raisef "Euclid.Vec.WithLength %g : %O is too small for unitizing, Tolerance:%g" desiredLength v zeroLengthTolerance
        /// Returns a new 3D vector scaled to the desired length.
        /// Same as Vec.withLength.
        member inline v.WithLength (desiredLength:float) =
            let l = v.Length
            if isTooTiny l then v.FailedWithLength desiredLength // don't compose error msg directly here to keep inlined code small.
            v * (desiredLength / l)

        /// A separate function to compose the error message that does not get inlined.
        [<Obsolete("Not actually obsolete but just hidden. (Needs to be public for inlining of the functions using it.)")>]
        member v.FailedUnitized() = EuclidDivByZeroException.Raisef "Euclid.Vec.Unitized %O is too small for unitizing, Tolerance:%g" v zeroLengthTolerance
        /// Returns the 3D vector unitized.
        /// Fails with EuclidDivByZeroException if the length of the vector is
        /// too small (1e-16) to unitize.
        member inline v.Unitized =
            let l = v.Length
            if isTooTiny l then v.FailedUnitized() // don't compose error msg directly here to keep inlined code small.
            let li = 1. / l
            UnitVec.createUnchecked(li*v.X, li*v.Y, li*v.Z)

        // Returns the 3D vector unitized.
        // If the length of the vector is 0.0 an invalid unit-vector is returned.
        // UnitVec(0, 0, 0)
        //member inline v.UnitizedUnchecked =
        //    let li = 1. / sqrt(v.X*v.X + v.Y*v.Y + v.Z*v.Z)
        //    UnitVec.createUnchecked(li*v.X, li*v.Y, li*v.Z)

        /// Test if the 3D vector is a unit-vector.
        /// Test if the vectors square length is within 6 float steps of 1.0
        /// So between 0.99999964 and 1.000000715.
        member inline v.IsUnit =
            UtilEuclid.isOne v.LengthSq

        /// Returns a perpendicular horizontal vector. Rotated counterclockwise.
        /// Or Vec.Zero if input is vertical.
        /// Just does Vec(-v.Y, v.X, 0.0)
        member inline v.PerpendicularInXY =
            Vec(-v.Y, v.X, 0)

        /// 90 Degree rotation Counter-Clockwise around Z-axis.
        member inline v.RotateOnZ90CCW =
            Vec( -v.Y, v.X, v.Z)

        /// 90 Degree rotation clockwise around Z-axis.
        member inline v.RotateOnZ90CW =
            Vec(v.Y, -v.X, v.Z)

        /// A separate function to compose the error message that does not get inlined.
        [<Obsolete("Not actually obsolete but just hidden. (Needs to be public for inlining of the functions using it.)")>]
        member v.FailedDirectionDiamondInXY() = EuclidDivByZeroException.Raisef "Euclid.Vec.DirectionDiamondInXY: input vector is vertical or zero length:%O" v
        /// The diamond angle.
        /// Calculates the proportion of X to Y component.
        /// It is always positive and in the range of 0.0 to 4.0 (for 360 Degrees)
        /// 0.0 = Xaxis, going Counter-Clockwise.
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.DirectionDiamondInXY =
            // https://stackoverflow.com/a/14675998/969070
            if isTooTiny (abs v.X + abs v.Y) then v.FailedDirectionDiamondInXY()
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

        /// A separate function to compose the error message that does not get inlined.
        [<Obsolete("Not actually obsolete but just hidden. (Needs to be public for inlining of the functions using it.)")>]
        member v.FailedDirection2PiInXY() = EuclidDivByZeroException.Raisef "Euclid.Vec.Direction2PiInXY: input vector is zero length or vertical: %O" v
        /// Returns the angle in Radians from X-axis,
        /// Going Counter-Clockwise till two Pi.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.Direction2PiInXY =
            // https://stackoverflow.com/a/14675998/969070
            if isTooTiny (abs v.X + abs v.Y) then v.FailedDirection2PiInXY()
            let a = Math.Atan2(v.Y, v.X)
            if a < 0. then
                a + UtilEuclid.twoPi
            else
                a

        /// A separate function to compose the error message that does not get inlined.
        [<Obsolete("Not actually obsolete but just hidden. (Needs to be public for inlining of the functions using it.)")>]
        member v.FailedDirectionPiInXY() = EuclidDivByZeroException.Raisef "Euclid.Vec.DirectionPiInXY: input vector is zero length or vertical: %O" v
        /// Returns the angle in Radians from X-axis,
        /// Ignores orientation.
        /// Range 0.0 to Pi.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.DirectionPiInXY =
            // https://stackoverflow.com/a/14675998/969070
            if isTooTiny (abs v.X + abs v.Y) then v.FailedDirectionPiInXY()
            let a = Math.Atan2(v.Y, v.X)
            if a < 0. then
                a + Math.PI
            else
                a

        /// Returns the angle in Degrees from X-axis.
        /// Going Counter-Clockwise till 360.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.Direction360InXY =
            v.Direction2PiInXY |> toDegrees

        /// Returns the angle in Degrees from X-axis,
        /// Ignores orientation.
        /// Range 0.0 to 180.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.Direction180InXY =
            v.DirectionPiInXY |> toDegrees

        /// Returns positive angle for rotating Counter-Clockwise from this vector to vector 'b' .
        /// In Diamond Angle. Using only proportion of X to Y components.
        /// Range of 0.0 to 4.0 (for 360 Degrees)
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.AngleDiamondInXYTo (b:Vec) =
            let r = b.DirectionDiamondInXY - v.DirectionDiamondInXY
            if r >= 0. then  r
            else r + 4.0

        /// Checks if the angle between the two 3D vectors is less than 180 degrees.
        /// Calculates the dot product of two 3D vectors.
        /// Then checks if it is bigger than 1e-12.
        /// Fails if any of the two vectors is shorter than zeroLengthTolerance  (1e-12).
        member inline v.MatchesOrientation (other:Vec) =
            if isTooTinySq(v.LengthSq    ) then EuclidException.Raisef "Euclid.Vec.MatchesOrientation: Vec 'this' is too short: %s. 'other':%s " v.AsString other.AsString
            if isTooTinySq(other.LengthSq) then EuclidException.Raisef "Euclid.Vec.MatchesOrientation: Vec 'other' is too short: %s. 'this':%s " other.AsString v.AsString
            v *** other > 1e-12

        /// Checks if the angle between this 3D vectors and a 3D unit-vector is less than 180 degrees.
        /// Calculates the dot product of two 3D vectors.
        /// Then checks if it is bigger than 1e-12.
        /// Fails if the vector is shorter than zeroLengthTolerance  (1e-12).
        member inline v.MatchesOrientation (other:UnitVec) =
            if isTooTinySq(v.LengthSq) then EuclidException.Raisef "Euclid.Vec.MatchesOrientation: Vec 'this' is too short: %s. 'other':%s " v.AsString other.AsString
            v *** other > 1e-12


        /// Checks if the angle between the two 3D vectors is more than 180 degrees.
        /// Calculates the dot product of two 3D vectors.
        /// Then checks if it is smaller than minus 1e-12.
        /// Fails if any of the two vectors is shorter than zeroLengthTolerance  (1e-12).
        member inline v.IsOppositeOrientation (other:Vec) =
            if isTooTinySq(v.LengthSq    ) then EuclidException.Raisef "Euclid.Vec.IsOppositeOrientation: Vec 'this' is too short: %s. 'other':%s " v.AsString other.AsString
            if isTooTinySq(other.LengthSq) then EuclidException.Raisef "Euclid.Vec.IsOppositeOrientation: Vec 'other' is too short: %s. 'this':%s " other.AsString v.AsString
            v *** other < -1e-12

        /// Checks if the angle between this 3D vectors and a 3D unit-vector is more than 180 degrees.
        /// Calculates the dot product of a 3D vector and a unit-vectors.
        /// Then checks if it is smaller than -1e-12.
        /// Fails if the vector is shorter than zeroLengthTolerance  (1e-12).
        member inline v.IsOppositeOrientation (other:UnitVec) =
            if isTooTinySq(v.LengthSq) then EuclidException.Raisef "Euclid.Vec.IsOppositeOrientation: Vec 'this' is too short: %s. 'other':%s " v.AsString other.AsString
            v *** other < -1e-12


        /// Checks if 3D vector is parallel to the world X axis. Ignoring orientation.
        /// The absolute deviation tolerance along Y and Z axis is 1e-9.
        /// Fails on vectors shorter than 1e-6.
        member inline v.IsXAligned =
            let x = abs (v.X)
            let y = abs (v.Y)
            let z = abs (v.Z)
            if isTooSmall (x+y+z) then EuclidException.Throw1 "Euclid.Vec.IsXAligned cannot not check very tiny vector. (tolerance 1e-6)" v
            else y < 1e-9 && z < 1e-9

        /// Checks if 3D vector is parallel to the world Y axis. Ignoring orientation.
        /// The absolute deviation tolerance along X and Z axis is 1e-9.
        /// Fails on vectors shorter than 1e-6.
        member inline v.IsYAligned =
            let x = abs (v.X)
            let y = abs (v.Y)
            let z = abs (v.Z)
            if isTooSmall (x+y+z) then EuclidException.Raisef "Euclid.Vec.IsYAligned cannot not check very tiny vector. (tolerance 1e-6) %O" v
            else x < 1e-9 && z < 1e-9

        /// Checks if 3D vector is parallel to the world Z axis. Ignoring orientation.
        /// The absolute deviation tolerance along X and Y axis is 1e-9.
        /// Fails on vectors shorter than 1e-6.
        /// Same as v.IsVertical
        member inline v.IsZAligned =
            let x = abs (v.X)
            let y = abs (v.Y)
            let z = abs (v.Z)
            if isTooSmall (x+y+z) then EuclidException.Raisef "Euclid.Vec.IsZAligned cannot not check very tiny vector. (tolerance 1e-6) %O" v
            else x < 1e-9 && y < 1e-9

        /// Checks if 3D vector is parallel to the world Z axis. Ignoring orientation.
        /// The absolute deviation tolerance along X and Y axis is 1e-9.
        /// Fails on vectors shorter than 1e-6.
        /// Same as v.IsZAligned
        member inline v.IsVertical =
            let x = abs (v.X)
            let y = abs (v.Y)
            let z = abs (v.Z)
            if isTooSmall (x+y+z) then EuclidException.Raisef "Euclid.Vec.IsVertical cannot not check very tiny vector. (tolerance 1e-6) %O" v
            else x < 1e-9 && y < 1e-9

        /// Checks if 3D vector is horizontal (Z component is almost zero).
        /// The absolute deviation tolerance along Z axis is 1e-9.
        /// Fails on vectors shorter than 1e-6.
        member inline v.IsHorizontal =
            let x = abs (v.X)
            let y = abs (v.Y)
            let z = abs (v.Z)
            if isTooSmall (x+y+z) then EuclidException.Raisef "Euclid.Vec.IsHorizontal cannot not check very tiny vector. (tolerance 1e-6) %O" v
            else z < 1e-9

        /// Checks if two 3D vectors are parallel.
        /// Ignores the line orientation.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minium cosine value.
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        member inline this.IsParallelTo(other:Vec, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
            let sa = this.LengthSq
            if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Vec.IsParallelTo: Vec 'this' is too short: %s. 'other':%s " this.AsString other.AsString
            let sb = other.LengthSq
            if isTooTinySq(sb) then EuclidException.Raisef "Euclid.Vec.IsParallelTo: Vec 'other' is too short: %s. 'this':%s " other.AsString this.AsString
            let au = this  * (1.0 / sqrt sa)
            let bu = other * (1.0 / sqrt sb)
            abs(bu *** au) > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:

        /// Checks if this 3D vectors and a 3D unit-vector are parallel.
        /// Ignores the line orientation.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minium cosine value.
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        member inline this.IsParallelTo(other:UnitVec, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
            let sa = this.LengthSq
            if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Vec.IsParallelTo: Vec 'this' is too short: %s. 'other':%s " this.AsString other.AsString
            let au = this * (1.0 / sqrt sa)
            abs(other *** au) > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:




        /// Checks if two 3D vectors are parallel.
        /// Takes the line orientation into account too.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minium cosine value.
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        member inline this.IsParallelAndOrientedTo (other:Vec, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
            let sa = this.LengthSq
            if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Vec.IsParallelAndOrientedTo: Vec 'this' is too short: %s. 'other':%s " this.AsString other.AsString
            let sb = other.LengthSq
            if isTooTinySq(sb) then EuclidException.Raisef "Euclid.Vec.IsParallelAndOrientedTo: Vec 'other' is too short: %s. 'this':%s " other.AsString this.AsString
            let au = this  * (1.0 / sqrt sa)
            let bu = other * (1.0 / sqrt sb)
            bu *** au > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:

        /// Checks if this 3D vectors and a 3D unit-vector are parallel.
        /// Takes the line orientation into account too.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minium cosine value.
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        member inline this.IsParallelAndOrientedTo (other:UnitVec, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
            let sa = this.LengthSq
            if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Vec.IsParallelAndOrientedTo: Vec 'this' is too short: %s. 'other':%s " this.AsString other.AsString
            let au = this * (1.0 / sqrt sa)
            other *** au > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:


        /// Checks if two 3D vectors are perpendicular to each other.
        /// The default angle tolerance is 89.75 to 90.25 degrees.
        /// This tolerance can be customized by an optional minium cosine value.
        /// The default cosine is 0.0043633 ( = 89.75 deg)
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        member inline this.IsPerpendicularTo (other:Vec, [<OPT;DEF(Cosine.``89.75``)>] maxCosine:float<Cosine.cosine> ) =
            let sa = this.LengthSq
            if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Vec.IsPerpendicularTo: Vec 'this' is too short: %s. 'other':%s " this.AsString other.AsString
            let sb = other.LengthSq
            if isTooTinySq(sb) then EuclidException.Raisef "Euclid.Vec.IsPerpendicularTo: Vec 'other' is too short: %s. 'this':%s " other.AsString this.AsString
            let au = this  * (1.0 / sqrt sa)
            let bu = other * (1.0 / sqrt sb)
            let d = bu *** au
            float -maxCosine < d && d  < float maxCosine // = cosine of 98.75 and 90.25 degrees

        /// Checks if this 3D vectors and a 3D unit-vector are perpendicular to each other.
        /// The default angle tolerance is 89.75 to 90.25 degrees.
        /// This tolerance can be customized by an optional minium cosine value.
        /// The default cosine is 0.0043633 ( = 89.75 deg)
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        member inline this.IsPerpendicularTo (other:UnitVec, [<OPT;DEF(Cosine.``89.75``)>] maxCosine:float<Cosine.cosine> ) =
            let sa = this.LengthSq
            if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Vec.IsPerpendicularTo: Vec 'this' is too short: %s. 'other':%s " this.AsString other.AsString
            let au = this * (1.0 / sqrt sa)
            let d = other *** au
            float -maxCosine < d && d  < float maxCosine // = cosine of 98.75 and 90.25 degrees


        /// Multiplies a Matrix with a 3D vector.
        /// Since a 3D vector represents a direction or an offset in space, but not a location,
        /// all translations are ignored. (Homogeneous Vector)
        member inline v.Transform (m:Matrix) =
            v *** m // operator * is defined in Matrix.fs

        /// Multiplies (or applies) a RigidMatrix to a 3D vector.
        /// Since a 3D vector represents a direction or an offset in space, but not a location,
        /// all translations are ignored. (Homogeneous Vector)
        member inline v.TransformRigid (m:RigidMatrix) =
            v *** m // operator * is defined in RigidMatrix.fs


        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------

        /// Returns the World X-axis with length one: Vec(1, 0, 0)
        static member inline Xaxis =
            Vec(1, 0, 0)

        /// Returns the World Y-axis with length one: Vec(0, 1, 0)
        static member inline Yaxis =
            Vec(0, 1, 0)

        /// Returns the World Z-axis with length one: Vec(0, 0, 1)
        static member inline Zaxis =
            Vec(0, 0, 1)

        /// Checks if two 3D vectors are equal within tolerance.
        /// Identical vectors in opposite directions are not considered equal.
        /// Use a tolerance of 0.0 to check for an exact match.
        static member inline equals (tol:float) (a:Vec) (b:Vec) =
            abs (a.X-b.X) <= tol &&
            abs (a.Y-b.Y) <= tol &&
            abs (a.Z-b.Z) <= tol


        /// Check if two 3D vectors  are not equal within a given tolerance.
        /// Use a tolerance of 0.0 to check if the two vectors are not exactly equal.
        static member notEquals (tol:float) (a:Vec) (b:Vec) =
            abs (a.X-b.X) > tol ||
            abs (a.Y-b.Y) > tol ||
            abs (a.Z-b.Z) > tol

        /// Returns the distance between the tips of two 3D vectors.
        static member inline difference (a:Vec) (b:Vec) =
            let x = a.X - b.X
            let y = a.Y - b.Y
            let z = a.Z - b.Z
            sqrt (x*x + y*y + z*z)

        /// Returns the squared distance between the tips of two 3D vectors.
        /// This operation is slightly faster than Vec.difference and sufficient for many algorithms like finding closest vectors.
        static member inline differenceSq (a:Vec) (b:Vec) =
            let x = a.X - b.X
            let y = a.Y - b.Y
            let z = a.Z - b.Z
            x*x + y*y + z*z

        /// A separate function to compose the error message that does not get inlined.
        [<Obsolete("Not actually obsolete but just hidden. (Needs to be public for inlining of the functions using it.)")>]
        static member failedCreateFromMembersXYZ(v:'T,e:exn) = EuclidException.Raisef "Euclid.Vec.createFromMembersXYZ: %A could not be converted to a Euclid.Vec:\r\n%A" v e
        /// Accepts any type that has a X, Y and Z (UPPERCASE) member that can be converted to a float.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline createFromMembersXYZ vec =
            let x = ( ^T : (member X : _) vec)
            let y = ( ^T : (member Y : _) vec)
            let z = ( ^T : (member Z : _) vec)
            try Vec(float x, float y, float z)
            with e -> Vec.failedCreateFromMembersXYZ (vec, e)

        /// A separate function to compose the error message that does not get inlined.
        [<Obsolete("Not actually obsolete but just hidden. (Needs to be public for inlining of the functions using it.)")>]
        static member failedCreateFromMembersxyz(v:'T,e:exn) = EuclidException.Raisef "Euclid.Vec.createFromMembersxyz: %A could not be converted to a Euclid.Vec:\r\n%A" v e
        /// Accepts any type that has a x, y and z (lowercase) member that can be converted to a float.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline createFromMembersxyz vec =
            let x = ( ^T : (member x : _) vec)
            let y = ( ^T : (member y : _) vec)
            let z = ( ^T : (member z : _) vec)
            try Vec(float x, float y, float z)
            with e -> Vec.failedCreateFromMembersxyz (vec, e)

        /// Create 3D vector from 3D point.
        static member inline createFromPnt (pt:Pnt) =
            Vec(pt.X, pt.Y, pt.Z)

        /// Create 3D vector from 3D unit-vector.
        static member inline createFromUnitVec (v:UnitVec) = Vec(v.X, v.Y, v.Z)

        /// Convert 3D vector to 2D point by ignoring Z value.
        static member inline asPt(v:Vec) =
            Pt(v.X, v.Y)

        /// Convert 3D vector to 2D vector by ignoring Z value.
        static member inline asVc(v:Vec) =
            Vc(v.X, v.Y)

        /// Convert 3D vector to 3D point.
        static member inline asPnt(v:Vec) =
            Pnt(v.X, v.Y, v.Z)


        /// Cross product, of a 3D unit-vectors an a 3D vector.
        /// The resulting vector is perpendicular to both input vectors.
        /// Its length is the area of the parallelogram spanned by the input vectors.
        /// Its direction follows th right-hand rule.
        /// A x B = |A| * |B| * sin(angle)
        static member inline cross (a:UnitVec, b:Vec) =
            Vec (a.Y * b.Z - a.Z * b.Y, a.Z * b.X - a.X * b.Z, a.X * b.Y - a.Y * b.X)

        /// Cross product, of a 3D vector and a 3D unit-vectors.
        /// The resulting vector is perpendicular to both input vectors.
        /// Its length is the area of the parallelogram spanned by the input vectors.
        /// Its direction follows th right-hand rule.
        /// A x B = |A| * |B| * sin(angle)
        static member inline cross (a:Vec, b:UnitVec) =
            Vec (a.Y * b.Z - a.Z * b.Y, a.Z * b.X - a.X * b.Z, a.X * b.Y - a.Y * b.X)


        //static member inline dot (a:Vec, b:Vec)   //moved to Vec type declaration

        /// Dot product, or scalar product of a 3D unit-vector with a 3D vector.
        /// Returns a float. This float is the projected length of the 3D vector on the direction of the unit-vector.
        static member inline dot (a:UnitVec, b:Vec) =
            a.X * b.X + a.Y * b.Y + a.Z * b.Z

        /// Dot product, or scalar product of a 3D vector with a 3D unit-vector.
        /// Returns a float. This float is the projected length of the 3D vector on the direction of the unit-vector.
        static member inline dot (a:Vec, b:UnitVec) =
            a.X * b.X + a.Y * b.Y + a.Z * b.Z

        /// Gets the X part of this 3D vector.
        static member inline getX (v:Vec) =
            v.X

        /// Gets the Y part of this 3D vector.
        static member inline getY (v:Vec) =
            v.Y

        /// Gets the Z part of this 3D vector.
        static member inline getZ (v:Vec) =
            v.Z

        /// Returns new 3D vector with new X value, Y and Z stay the same.
        static member inline withX  x (v:Vec) =
            v.WithX x

        /// Returns new 3D vector with new Y value, X and Z stay the same.
        static member inline withY  y (v:Vec) =
            v.WithY y

        /// Returns new 3D vector with new z value, X and Y stay the same.
        static member inline withZ z (v:Vec) =
            v.WithZ z

        /// Add two 3D vectors together. Returns a new 3D vector.
        static member inline add (a:Vec) (b:Vec) =
            b + a

        /// Multiplies a 3D vector with a scalar, also called scaling a vector.
        /// Returns a new 3D vector.
        static member inline scale (f:float) (v:Vec) =
            Vec (v.X * f, v.Y * f, v.Z * f)

        /// Returns a new 3D vector scaled to the desired length.
        /// Same as vec.WithLength. Returns a new 3D vector.
        static member inline withLength(desiredLength:float) (v:Vec) =
            v.WithLength desiredLength

        /// Add to the X part of this 3D vectors together. Returns a new 3D vector.
        static member inline moveX x (v:Vec) =
            Vec (v.X+x, v.Y, v.Z)

        /// Add to the Y part of this 3D vectors together. Returns a new 3D vector.
        static member inline moveY y (v:Vec) =
            Vec (v.X, v.Y+y, v.Z)

        /// Add to the Z part of this 3D vectors together. Returns a new 3D vector.
        static member inline moveZ z (v:Vec) =
            Vec (v.X, v.Y, v.Z+z)

        /// Check if the 3D vector is shorter than the tolerance.
        /// Also checks if any component is a NaN.
        static member inline isTiny tol (v:Vec) =
            not (v.Length > tol)

        /// Check if the 3D vectors square length is shorter than the squared tolerance.
        /// Also checks if any component is a NaN.
        static member inline isTinySq tol (v:Vec) =
            not (v.LengthSq > tol)

        /// Returns the length of the 3D vector.
        static member inline length (v:Vec) =
            v.Length

        /// Returns the squared length of the 3D vector.
        /// The square length is faster to calculate and often good enough for use cases such as sorting vectors by length.
        static member inline lengthSq (v:Vec) =
            v.LengthSq

        /// Returns a new 3D vector from X, Y and Z parts.
        static member inline create (x:float, y:float, z:float) =
            Vec(x, y, z)

        /// Returns a new 3D vector from start and end point.
        static member inline create (start:Pnt, ende:Pnt) =
            ende-start

        /// Returns a 3D vector from z value and 2D vector.
        static member inline createFromVcWithZ (z:float) (v:Vc) =
            Vec (v.X, v.Y, z)

        /// Project vector to World X-Y plane.
        /// Use Vc.ofVec to convert to a 2D vector.
        static member inline projectToXYPlane (v:Vec) =
            Vec(v.X, v.Y, 0.0)

        /// Negate or inverse a 3D vectors. Returns a new 3D vector.
        /// Same as Vec.flip.
        static member inline reverse (v:Vec) =
            -v

        /// Negate or inverse a 3D vectors. Returns a new 3D vector.
        /// Same as Vec.reverse.
        static member inline flip (v:Vec) =
            -v

        /// Flips the vector if Z part is smaller than 0.0
        static member inline flipToPointUp (v:Vec) =
            if v.Z < 0.0 then -v else v

        /// Returns 3D vector unitized, fails on zero length vectors.
        static member inline unitize (v:Vec) =
            v.Unitized

        /// Unitize 3D vector, if input vector is shorter than 1e-6 the default unit-vector is returned.
        static member inline unitizeOrDefault (defaultUnitVector:UnitVec) (v:Vec) =
            let l = v.LengthSq
            if l < 1e-12  then  // = sqrt (1e-06)
                defaultUnitVector
            else
                let f = 1.0 / sqrt(l)
                UnitVec.createUnchecked(v.X*f, v.Y*f, v.Z*f)

        /// Returns three vector's Determinant.
        /// This is also the signed volume of the Parallelepipeds define by these three vectors.
        /// Also called scalar triple product, mixed product, Box product, or in German: Spatprodukt.
        /// It is defined as the dot product of one of the vectors with the cross product of the other two.
        static member inline determinant (u:Vec, v:Vec, w:Vec) =
            u.X*v.Y*w.Z + v.X*w.Y*u.Z + w.X*u.Y*v.Z - w.X*v.Y*u.Z - v.X*u.Y*w.Z - u.X*w.Y*v.Z

        /// Returns positive angle between two 3D vectors in Radians.
        /// Takes vector orientation into account,
        /// Range 0.0 to Pi( = 0 to 180 Degree).
        static member inline anglePi (a:Vec) (b:Vec) =
            UnitVec.anglePi a.Unitized b.Unitized

        /// Returns positive angle between two 3D vectors in Degrees.
        /// Takes vector orientation into account,
        /// Range 0 to 180 Degrees.
        static member inline angle180 (a:Vec) (b:Vec) =
            UnitVec.angle180 a.Unitized b.Unitized

        /// Returns positive angle between two 3D vectors in Radians.
        /// Ignores vector orientation,
        /// Range: 0.0 to Pi/2 ( = 0 to 90 Degrees)
        static member inline angleHalfPi (a:Vec) (b:Vec) =
            UnitVec.angleHalfPi a.Unitized b.Unitized

        /// Returns positive angle between two 3D vectors in Degrees.
        /// Ignores vector orientation,
        /// Range: 0 to 90 Degrees.
        static member inline angle90 (a:Vec) (b:Vec) =
            UnitVec.angle90 a.Unitized b.Unitized

        /// Returns positive angle from vector 'a' to vector 'b' projected in X-Y plane.
        /// In Radians.
        /// Considering Counter-Clockwise rotation round the World Zaxis.
        /// Range: 0.0 to 2 Pi ( = 0 to 360 Degrees)
        static member inline angle2PiInXY (a:Vec, b:Vec) =
            let r = b.Direction2PiInXY  - a.Direction2PiInXY
            if r >= 0. then  r
            else r + UtilEuclid.twoPi

        /// Returns positive angle of two 3D vector projected in X-Y plane.
        /// In Degrees.
        /// Considering positive rotation round the World Z-axis.
        /// Range: 0 to 360 Degrees.
        static member inline angle360InXY (a:Vec, b:Vec) =
            Vec.angle2PiInXY (a, b) |> toDegrees

        /// Returns positive angle for rotating Counter-Clockwise from vector 'a' to vector 'b' .
        /// In Diamond Angle. Using only proportion of X to Y components.
        /// Range of 0.0 to 4.0 (for 360 Degrees)
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline angleDiamondInXY (a:Vec, b:Vec) =
            a.AngleDiamondInXYTo(b)

        /// The diamond angle.
        /// Returns positive angle of 3D vector in World X-Y plane.
        /// Calculates the proportion of X to Y component.
        /// It is always positive and in the range of 0.0 to 4.0 (for 360 Degrees)
        /// 0.0 = Xaxis, going Counter-Clockwise.
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline directionDiamondInXY(v:Vec) =
            v.DirectionDiamondInXY

        /// Returns positive angle of 3D vector in World X-Y plane. Counter-Clockwise from X-axis.
        /// In Radians.
        /// Range: 0.0 to 2 Pi ( = 0 to 360 Degrees)
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline direction2PiInXY (v:Vec) =
            v.Direction2PiInXY

        /// Returns positive angle of 3D vector in World X-Y plane. Counter-Clockwise from X-axis.
        /// In Degree.
        /// Range: 0.0 to 2 Pi ( = 0 to 360 Degrees)
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline direction360InXY (v:Vec) =
            v.Direction360InXY

        /// Returns a (not unitized) bisector vector in the middle direction.
        /// Code : a.Unitized + b.Unitized.
        static member inline bisector (a:Vec) (b:Vec) =
            a.Unitized + b.Unitized

        /// Ensure that the 3D  vector has a positive dot product with given 3D orientation vector.
        static member inline matchOrientation (orientationToMatch:Vec) (vecToFlip:Vec) =
            if orientationToMatch *** vecToFlip < 0.0 then -vecToFlip else vecToFlip

        /// Ensure that the 3D vector has a positive dot product with given 3D orientation unit-vector.
        static member inline matchUnitVecOrientation (orientationToMatch:UnitVec) (vecToFlip:Vec) =
            if orientationToMatch *** vecToFlip < 0.0 then -vecToFlip else vecToFlip


        /// Checks if the angle between the two 3D vectors is less than 180 degrees.
        /// Calculates the dot product of two 3D vectors.
        /// Then checks if it is bigger than 1e-12.
        /// If any of the two vectors is zero length returns false.
        static member inline matchesOrientation (v:Vec) (other:Vec) =
            v.MatchesOrientation other


        /// Checks if the angle between the two 3D vectors is more than 180 degrees.
        /// Calculates the dot product of two 3D vectors.
        /// Then checks if it is smaller than minus 1e-12.
        /// If any of the two vectors is zero length returns false.
        static member inline isOppositeOrientation (v:Vec) (other:Vec) =
            v.IsOppositeOrientation other


        /// Checks if Angle between two vectors is Below 0.25 Degree.
        /// Ignores vector orientation.
        /// Fails on zero length vectors, tolerance 1e-12.
        static member inline areParallel (other:Vec) (v:Vec) =
            v.IsParallelTo other


        /// Checks if Angle between two vectors is between 98.75 and 90.25 Degree.
        /// Ignores vector orientation.
        /// Fails on zero length vectors, tolerance 1e-12.
        static member inline areParallelAndMatchOrientation (other:Vec) (v:Vec) =
            v.IsParallelAndOrientedTo other

        /// Checks if Angle between two vectors is between 98.75 and 90.25 Degree.
        /// Ignores vector orientation.
        /// Fails on zero length vectors, tolerance 1e-12.
        static member inline arePerpendicular (other:Vec) (v:Vec) =
            v.IsPerpendicularTo other


        // Rotate2D:

        /// 90 Degree rotation Counter-Clockwise around Z-axis.
        static member inline rotateOnZ90CCW(v:Vec) =
            Vec( -v.Y, v.X, v.Z)

        /// 90 Degree rotation clockwise around Z-axis.
        static member inline rotateOnZ90CW(v:Vec) =
            Vec(  v.Y, -v.X, v.Z)

        /// Rotate the 3D vector around X-axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member rotateXBy (r:Rotation2D) (v:Vec) =
            Vec (v.X, r.Cos*v.Y - r.Sin*v.Z, r.Sin*v.Y + r.Cos*v.Z)

        /// Rotate the 3D vector around Y-axis, from Z to X-axis, Counter Clockwise looking from back.
        static member rotateYBy (r:Rotation2D) (v:Vec) =
            Vec (r.Sin*v.Z + r.Cos*v.X, v.Y, r.Cos*v.Z - r.Sin*v.X)

        /// Rotate the 3D vector around Z-axis, from X to Y-axis, Counter Clockwise looking from top.
        static member rotateZBy (r:Rotation2D) (v:Vec) =
            Vec (r.Cos*v.X - r.Sin*v.Y, r.Sin*v.X + r.Cos*v.Y, v.Z)

        /// Rotate the 3D vector in Degrees around X-axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member inline rotateX (angDegree) (v:Vec) =
            Vec.rotateXBy (Rotation2D.createFromDegrees angDegree) v

        /// Rotate the 3D vector in Degrees around Y-axis, from Z to X-axis, Counter Clockwise looking from back.
        static member inline rotateY (angDegree) (v:Vec) =
            Vec.rotateYBy (Rotation2D.createFromDegrees angDegree) v

        /// Rotate the 3D vector in Degrees around Z-axis, from X to Y-axis, Counter Clockwise looking from top.
        static member inline rotateZ (angDegree) (v:Vec) =
            Vec.rotateZBy (Rotation2D.createFromDegrees angDegree) v

        /// Rotate by Quaternion.
        static member inline rotateByQuaternion (q:Quaternion) (v:Vec) =
            v *** q  // operator * is defined in Quaternion.fs

        /// Linearly interpolates between two vectors.
        /// e.g. rel=0.5 will return the middle vector, rel=1.0 the end vector,
        /// rel=1.5 a vector half the distance beyond the end vector.
        static member lerp (start:Vec, ende:Vec, rel:float) =
            start + rel * (ende - start)

        /// Spherically interpolates between start and end by amount rel (0.0 to 1.0).
        /// The difference between this and linear interpolation (aka, "lerp") is that the vectors are treated as directions rather than points in space.
        /// The direction of the returned vector is interpolated by the angle and its magnitude is interpolated between the magnitudes of start and end.
        /// Interpolation continues before and after the range of 0.0 and 0.1
        static member slerp (start:Vec, ende:Vec, rel:float) =
            // https://en.wikipedia.org/wiki/Slerp
            // implementation tested in Rhino!
            let sLen = start.Length
            let eLen = ende.Length
            if isTooTiny sLen then EuclidDivByZeroException.Throw1 "Euclid.Vec.slerp: Can't interpolate from zero length vector:" start
            if isTooTiny eLen then EuclidDivByZeroException.Throw1 "Euclid.Vec.slerp: Can't interpolate to zero length vector:" ende
            let fs = 1.0 / sLen
            let fe = 1.0 / eLen
            let su  = start * fs //unitized start vector
            let eu  = ende  * fe //unitized end   vector
            let dot = su *** eu
            if dot > float Cosine.``0.05`` then  // vectors are in the same direction interpolate linear only
                Vec.lerp(start, ende, rel)
            elif dot < float Cosine.``179.95`` then
                EuclidDivByZeroException.Throw1 "Euclid.Vec.slerp: Can't interpolate vectors in opposite directions:" ende
            else
                let ang = acos(dot) // the angel between the two vectors
                let perp = eu - su*dot |> Vec.unitize // a vector perpendicular to start and in the same plane with ende.
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
                    Vec.Zero // otherwise the vector would get flipped and grow again , only applicable if rel is beyond 0.0 or 0.1
                else
                    res * abs lenRel


        /// Returns the vector length projected into X Y Plane.
        /// sqrt(v.X * v.X  + v.Y * v.Y)
        static member inline lengthInXY(v:Vec) =
            sqrt(v.X * v.X  + v.Y * v.Y)

        /// Checks if 3D vector is parallel to the world X axis. Ignoring orientation.
        /// Tolerance is 1e-6.
        /// Fails on vectors shorter than 1e-6.
        static member inline isXAligned (v:Vec) =
            v.IsXAligned

        /// Checks if 3D vector is parallel to the world Y axis. Ignoring orientation.
        /// Tolerance is 1e-6.
        /// Fails on vectors shorter than 1e-6.
        static member inline isYAligned (v:Vec) =
            v.IsYAligned

        /// Checks if 3D vector is parallel to the world Z axis. Ignoring orientation.
        /// Tolerance is 1e-6.
        /// Fails on vectors shorter than 1e-6.
        /// Same as ln.IsVertical
        static member inline isZAligned (v:Vec) =
            v.IsZAligned

        /// Checks if 3D vector is parallel to the world Z axis. Ignoring orientation.
        /// Tolerance is 1e-6.
        /// Fails on vectors shorter than 1e-6.
        /// Same as ln.IsZAligned
        static member inline isVertical (v:Vec) =
            v.IsVertical

        /// Checks if line is horizontal (Z component is almost zero).
        /// Tolerance is 1e-6.
        /// Fails on lines shorter than 1e-6.
        static member inline isHorizontal (v:Vec) =
            v.IsHorizontal

        /// Returns positive or negative slope of a vector in Radians.
        /// In relation to X-Y plane.
        /// Range -1.57 to +1.57 Radians.
        static member inline slopeRadians (v:Vec) =
            let len2D = sqrt(v.X*v.X + v.Y*v.Y)
            Math.Atan2(v.Z, len2D)

        /// Returns positive or negative slope of a vector in Degrees.
        /// In relation to X-Y plane.
        /// Range -90 to +90 Degrees.
        static member inline slopeDegrees (v:Vec) =
            Vec.slopeRadians v |> toDegrees

        /// Returns positive or negative slope of a vector in Percent.
        /// In relation to X-Y plane.
        /// 100% = 45 Degrees.
        /// Returns positive (or negative) Infinity if line is vertical or input has length zero.
        static member inline slopePercent (v:Vec) =
            //if isTooTiny (abs(v.Z)) then EuclidDivByZeroException.Raise "Euclid.Vec.slopePercent: Can't get Slope from vertical vector %O" v
            let len2D = sqrt(v.X*v.X + v.Y*v.Y)
            100.0 * v.Z / len2D

        /// Reverse vector if Z part is smaller than 0.0
        static member inline orientUp (v:Vec) =
            if v.Z < 0.0 then -v else v

        /// Reverse vector if Z part is bigger than 0.0
        static member inline orientDown (v:Vec) =
            if v.Z < 0.0 then v else -v

        /// Returns a perpendicular horizontal vector. Rotated counterclockwise.
        /// Just does Vec(-v.Y, v.X, 0.0)
        /// On vertical input vector resulting vector if of zero length.
        static member inline perpendicularInXY (v:Vec) =
            Vec(-v.Y, v.X, 0.0)

        /// Returns a vector that is perpendicular to the given vector and in the same vertical Plane.
        /// Projected into the X-Y plane input and output vectors are parallel and of same orientation.
        /// Not of same length, not unitized.
        /// On vertical input vector resulting vector if of zero length.
        static member inline perpendicularInVerticalPlane (v:Vec) =
            let hor = Vec(v.Y, -v.X, 0.0)
            let r = Vec.cross (v, hor)
            if v.Z < 0.0 then -r else r

        /// Multiplies a Matrix with a 3D vector
        /// Since a 3D vector represents a direction or an offset in space, but not a location,
        /// the implicit the 4th dimension is 0.0 so that all translations are ignored. (Homogeneous Vector)
        static member inline transform (m:Matrix) (v:Vec) =
            v.Transform(m)

        /// Multiplies (or applies) a RigidMatrix to a 3D vector.
        /// Since a 3D vector represents a direction or an offset in space, but not a location,
        /// all translations are ignored. (Homogeneous Vector)
        static member inline transformRigid (m:RigidMatrix) (v:Vec) =
            v.TransformRigid(m)


        /// Checks if Angle between two vectors is less than given Cosine.
        /// Ignores vector orientation. The angle between two vectors can be 0 to 90 degrees ignoring their direction.
        /// Use the Euclid.Cosine module to get some precomputed cosine values
        /// Fails on zero length vectors, tolerance 1e-12.
        static member inline isAngle90Below (cosineValue: float<Cosine.cosine>) (a:Vec) (b:Vec) =
            let sa = a.LengthSq
            if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Vec.isAngle90Below: Vec a is too short: %s. Vec b:%s " a.AsString b.AsString
            let sb = b.LengthSq
            if isTooTinySq(sb) then EuclidException.Raisef "Euclid.Vec.isAngle90Below: Vec b is too short: %s. Vec a:%s " b.AsString a.AsString
            let au = a * (1.0 / sqrt sa)
            let bu = b * (1.0 / sqrt sb)
            abs(bu *** au) > float cosineValue

        /// Checks if Angle between two vectors is more than given Cosine.
        /// Ignores vector orientation. The angle between two vectors can be 0 to 90 degrees ignoring their direction.
        /// Use the Euclid.Cosine module to get some precomputed cosine values.
        /// Fails on zero length vectors, tolerance 1e-12.
        static member inline isAngle90Above(cosineValue: float<Cosine.cosine>) (a:Vec) (b:Vec) =
            let sa = a.LengthSq
            if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Vec.isAngle90Above: Vec a is too short: %s. Vec b:%s " a.AsString b.AsString
            let sb = b.LengthSq
            if isTooTinySq(sb) then EuclidException.Raisef "Euclid.Vec.isAngle90Above: Vec b is too short: %s. Vec a:%s " b.AsString a.AsString
            let au = a * (1.0 / sqrt sa)
            let bu = b * (1.0 / sqrt sb)
            abs(bu *** au) < float cosineValue


        /// Checks if Angle between two vectors is less than given Cosine.
        /// Does not ignores vector orientation.The angle between two vectors can be 0 to 180 degrees.
        /// Use the Euclid.Cosine module to get some precomputed cosine values
        /// Fails on zero length vectors, tolerance 1e-12.
        static member inline isAngle180Below (cosineValue: float<Cosine.cosine>) (a:Vec) (b:Vec) =
            let sa = a.LengthSq
            if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Vec.isAngle180Below: Vec a is too short: %s. Vec b:%s " a.AsString b.AsString
            let sb = b.LengthSq
            if isTooTinySq(sb) then EuclidException.Raisef "Euclid.Vec.isAngle180Below: Vec b is too short: %s. Vec a:%s " b.AsString a.AsString
            let au = a * (1.0 / sqrt sa)
            let bu = b * (1.0 / sqrt sb)
            bu *** au > float cosineValue

        /// Checks if Angle between two vectors is more than given Cosine.
        /// Does not ignores vector orientation.The angle between two vectors can be 0 to 180 degrees.
        /// Use the Euclid.Cosine module to get some precomputed cosine values.
        /// Fails on zero length vectors, tolerance 1e-12.
        static member inline isAngle180Above(cosineValue: float<Cosine.cosine>) (a:Vec) (b:Vec) =
            let sa = a.LengthSq
            if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Vec.isAngle180Above: Vec a is too short: %s. Vec b:%s " a.AsString b.AsString
            let sb = b.LengthSq
            if isTooTinySq(sb) then EuclidException.Raisef "Euclid.Vec.isAngle180Above: Vec b is too short: %s. Vec a:%s " b.AsString a.AsString
            let au = a * (1.0 / sqrt sa)
            let bu = b * (1.0 / sqrt sb)
            bu *** au < float cosineValue


        ///<summary> Intersects two infinite 3D lines.
        /// The lines are defined by a start point and a vector.
        /// 'ValueNone' is returned, if the angle between the vectors is less than 0.25 degrees
        /// or any of them is shorter than 1e-6. These tolerances can be adjusted with optional parameters. </summary>
        ///<param name="ptA"> The start point of the first line.</param>
        ///<param name="ptB"> The start point of the second line.</param>
        ///<param name="vA" > The vector of the first line.</param>
        ///<param name="vB" > The vector of the second line.</param>
        ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
        ///  If one or both vectors are shorter than this ValueNone is returned.</param>
        ///<param name="relAngleDiscriminant"> This is an optional tolerance for the internally calculated relative Angle Discriminant.
        /// The default value corresponds to approx 0.25 degree. Below this angle vectors are considered parallel.
        /// Use the module Euclid.UtilEuclid.RelAngleDiscriminant to set another tolerance here.</param>
        ///<returns> For (almost) zero length or (almost) parallel vectors: ValueNone
        /// Else ValueSome with a tuple of the parameters at which the two infinite 2D lines intersect to each other.
        /// The tuple's order corresponds to the input order.</returns>
        static member inline intersection(  ptA:Pnt,
                                            ptB:Pnt,
                                            vA:Vec,
                                            vB:Vec,
                                            [<OPT;DEF(1e-6)>] tooShortTolerance:float,
                                            [<OPT;DEF(RelAngleDiscriminant.``0.25``)>] relAngleDiscriminant:float<RelAngleDiscriminant.relAngDiscr>
                                            ) : ValueOption<float*float> =
            //https://stackoverflow.com/a/34604574/969070 but DP and DQ are in wrong order !
            let ax = vA.X
            let ay = vA.Y
            let az = vA.Z
            let bx = vB.X
            let by = vB.Y
            let bz = vB.Z
            let a = ax*ax + ay*ay + az*az // square length of A
            let c = bx*bx + by*by + bz*bz // square length of B
            if a < tooShortTolerance * tooShortTolerance then  // vec A too short
                ValueNone
            elif c < tooShortTolerance * tooShortTolerance then  // vec B too short
                ValueNone
            else
                let b = ax*bx + ay*by + az*bz // dot product of both lines
                let ac = a*c // square of square length, never negative
                let bb = b*b // square of square dot product, never negative
                let discriminant = ac - bb // never negative, the dot product cannot be bigger than the two square length multiplied with each other
                let div = ac+bb // never negative
                // getting the relation between the sum and the subtraction gives a good estimate of the angle between the lines
                // see module Euclid.UtilEuclid.RelAngleDiscriminant
                let rel = discriminant/div
                if rel < float relAngleDiscriminant then //parallel
                    ValueNone
                else
                    let vx = ptB.X - ptA.X
                    let vy = ptB.Y - ptA.Y
                    let vz = ptB.Z - ptA.Z
                    let e = bx*vx + by*vy + bz*vz
                    let d = ax*vx + ay*vy + az*vz
                    let t = (c * d - b * e) / discriminant
                    let u = (b * d - a * e) / discriminant
                    ValueSome (t, u)

