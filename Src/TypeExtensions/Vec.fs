namespace Euclid
open System
open UtilEuclid
open EuclidErrors

/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type Vec.
[<AutoOpen>]
module AutoOpenVec =

    type Vec with

        /// Convert 3D vector to 3D point.
        member inline v.AsPnt : Pnt =
            Pnt(v.X, v.Y, v.Z)

        /// Convert 3D vector to 3D point.
        static member inline asPnt(v:Vec) : Pnt =
            Pnt(v.X, v.Y, v.Z)

        /// Convert 3D vector to 2D vector, discarding the Z value.
        member inline v.AsVc : Vc =
            Vc(v.X, v.Y)

        /// Convert 3D vector to 2D vector, discarding the Z value.
        static member inline asVc(v:Vec) : Vc =
            Vc(v.X, v.Y)

        /// Returns a boolean indicating whether X, Y and Z are all exactly 0.0.
        member inline v.IsZero : bool =
            v.X = 0.0 && v.Y = 0.0 && v.Z = 0.0

        /// Returns a boolean indicating whether X, Y and Z are all exactly 0.0.
        static member inline isZero (v:Vec) : bool =
            v.IsZero

        /// Returns a boolean indicating if any of X, Y and Z is not exactly 0.0.
        member inline v.IsNotZero : bool =
            not v.IsZero

        /// Returns a boolean indicating if any of X, Y and Z is not exactly 0.0.
        static member inline isNotZero (v:Vec) : bool =
            v.IsNotZero

        /// Check if the 3D vector is shorter than the tolerance.
        /// Also checks if any component is a NaN.
        member inline v.IsTiny tol : bool =
            not (v.Length > tol)

        /// Check if the 3D vector is shorter than the tolerance.
        /// Also checks if any component is a NaN.
        static member inline isTiny tol (v:Vec) : bool =
            not (v.Length > tol)

        /// Check if the 3D vectors square length is shorter than the squared tolerance.
        /// Also checks if any component is a NaN.
        member inline v.IsTinySq tol : bool =
            not (v.LengthSq > tol)

        /// Check if the 3D vectors square length is shorter than the squared tolerance.
        /// Also checks if any component is a NaN.
        static member inline isTinySq tol (v:Vec) : bool =
            not (v.LengthSq > tol)

        /// Returns the length of the 3D vector projected into World X-Y plane.
        member inline v.LengthInXY : float =
            sqrt (v.X*v.X + v.Y*v.Y)

        /// Returns the length of the 3D vector projected into World X-Y plane.
        static member inline lengthInXY(v:Vec) : float =
            sqrt(v.X * v.X  + v.Y * v.Y)

        /// Returns the squared length of the 3D vector projected into World X-Y plane.
        /// The square length is faster to calculate and often good enough for use cases such as sorting vectors by length.
        member inline v.LengthSqInXY : float =
            v.X*v.X + v.Y*v.Y

        /// Returns the squared length of the 3D vector projected into World X-Y plane.
        /// The square length is faster to calculate and often good enough for use cases such as sorting vectors by length.
        static member inline lengthSqInXY (v:Vec) : float =
            v.LengthSqInXY

        /// Returns a new 3D vector with new X coordinate, Y and Z stay the same.
        member inline v.WithX x : Vec =
            Vec (x, v.Y, v.Z)

        /// Returns a new 3D vector with new X coordinate, Y and Z stay the same.
        /// Same as Vec.setX.
        static member inline withX  x (v:Vec) : Vec =
            v.WithX x

        /// Returns a new 3D vector with new y coordinate, X and Z stay the same.
        member inline v.WithY y : Vec =
            Vec (v.X, y, v.Z)

        /// Returns a new 3D vector with new y coordinate, X and Z stay the same.
        /// Same as Vec.setY.
        static member inline withY  y (v:Vec) : Vec =
            v.WithY y

        /// Returns a new 3D vector with new z coordinate, X and Y stay the same.
        member inline v.WithZ z : Vec =
            Vec (v.X, v.Y, z)

        /// Returns a new 3D vector with new z coordinate, X and Y stay the same.
        /// Same as Vec.setZ.
        static member inline withZ z (v:Vec) : Vec =
            v.WithZ z

        /// Returns a new 3D vector with half the length.
        member inline v.Half : Vec =
            Vec (v.X*0.5, v.Y*0.5, v.Z*0.5)

        /// Returns the half-length vector.
        static member inline half (v:Vec) : Vec =
            v.Half

        /// Cross product, of a 3D vector and a 3D unit-vector.
        /// The resulting vector is perpendicular to both input vectors.
        /// The length of this resulting vector is the area of the parallelogram spanned by the input vectors.
        /// Its direction follows the right-hand rule.
        /// A x B = |A| * |B| * sin(angle)
        member inline a.Cross (b:UnitVec) : Vec =
            Vec (a.Y * b.Z - a.Z * b.Y, a.Z * b.X - a.X * b.Z, a.X * b.Y - a.Y * b.X)

        /// Cross product of two 3D vectors.
        /// The resulting vector is perpendicular to both input vectors.
        /// The length of this resulting vector is the area of the parallelogram spanned by the input vectors.
        /// Its direction follows the right-hand rule.
        /// A x B = |A| * |B| * sin(angle)
        member inline a.Cross (b:Vec) : Vec =
            Vec (a.Y * b.Z - a.Z * b.Y, a.Z * b.X - a.X * b.Z, a.X * b.Y - a.Y * b.X)

        /// Cross product, of a 3D vector and a 3D unit-vector.
        /// The resulting vector is perpendicular to both input vectors.
        /// The length of this resulting vector is the area of the parallelogram spanned by the input vectors.
        /// Its direction follows the right-hand rule.
        /// A x B = |A| * |B| * sin(angle)
        static member inline cross (a:Vec, b:UnitVec)  : Vec =
            Vec (a.Y * b.Z - a.Z * b.Y, a.Z * b.X - a.X * b.Z, a.X * b.Y - a.Y * b.X)

        /// Cross product, of a 3D unit-vector and a 3D vector.
        /// The resulting vector is perpendicular to both input vectors.
        /// The length of this resulting vector is the area of the parallelogram spanned by the input vectors.
        /// Its direction follows the right-hand rule.
        /// A x B = |A| * |B| * sin(angle)
        static member inline cross (a:UnitVec, b:Vec)  : Vec =
            Vec (a.Y * b.Z - a.Z * b.Y, a.Z * b.X - a.X * b.Z, a.X * b.Y - a.Y * b.X)

        /// Dot product, or scalar product of two 3D vectors.
        /// Returns a float.
        member inline a.Dot (b:Vec) : float =
            a.X * b.X + a.Y * b.Y + a.Z * b.Z

        /// Dot product, or scalar product of a 3D vector with a 3D unit-vector.
        /// Returns a float.
        /// This float is the projected length of the 3D vector on the direction of the unit-vector.
        member inline a.Dot (b:UnitVec) : float =
            a.X * b.X + a.Y * b.Y + a.Z * b.Z

        /// Dot product, or scalar product of a 3D vector with a 3D unit-vector.
        /// Returns a float.
        /// This float is the projected length of the 3D vector on the direction of the unit-vector.
        static member inline dot (a:Vec, b:UnitVec)  : float =
            a.X * b.X + a.Y * b.Y + a.Z * b.Z

        /// Dot product, or scalar product of a 3D unit-vector with a 3D vector.
        /// Returns a float.
        /// This float is the projected length of the 3D vector on the direction of the unit-vector.
        static member inline dot (a:UnitVec, b:Vec)  : float =
            a.X * b.X + a.Y * b.Y + a.Z * b.Z

        /// Returns a new 3D vector scaled to the desired length.
        /// Same as Vec.withLength.
        member inline v.WithLength (desiredLength:float) : Vec =
            let l = v.Length
            if isTooTiny l then failTooSmall "Vec.WithLength" v
            v * (desiredLength / l)

        /// Returns a new 3D vector scaled to the desired length.
        /// Same as Vec.withLength.
        static member inline withLength(desiredLength:float) (v:Vec) : Vec =
            v.WithLength desiredLength

        /// Returns the 3D vector unitized.
        /// Fails with EuclidDivByZeroException if the length of the vector is
        /// less than 1e-12 (UtilEuclid.zeroLengthTolerance).
        member inline v.Unitized : UnitVec =
            let x = v.X
            let y = v.Y
            let z = v.Z
            let l = sqrt (x*x + y*y + z*z)
            if isTooTiny l then failUnit3 "Vec.Unitized" x y z
            let f = 1. / l
            UnitVec.createUnchecked(f*x, f*y, f*z)

        /// Returns the 3D vector unitized.
        /// Fails with EuclidDivByZeroException if the length of the vector is
        /// less than 1e-12 (UtilEuclid.zeroLengthTolerance).
        static member inline unitize (v:Vec) : UnitVec =
            let x = v.X
            let y = v.Y
            let z = v.Z
            let l = sqrt (x*x + y*y + z*z)
            if isTooTiny l then failUnit3 "Vec.unitize" x y z
            let f = 1. / l
            UnitVec.createUnchecked(f*x, f*y, f*z)


        // Returns the 3D vector unitized.
        // If the length of the vector is 0.0 an invalid unit-vector is returned.
        // UnitVec(0, 0, 0)
        //member inline v.UnitizedUnchecked =
        //    let li = 1. / sqrt(v.X*v.X + v.Y*v.Y + v.Z*v.Z)
        //    UnitVec.createUnchecked(li*v.X, li*v.Y, li*v.Z)

        /// Test if the 3D vector is a unit-vector.
        /// Tests if the squared length is between 0.999999 and 1.000001 (1e-6 tolerance).
        member inline v.IsUnit : bool =
            UtilEuclid.isOne v.LengthSq

        /// Test if the 3D vector is a unit-vector.
        /// Tests if the squared length is between 0.999999 and 1.000001 (1e-6 tolerance).
        static member inline isUnit (v:Vec) : bool =
            v.IsUnit

        /// Returns a perpendicular horizontal vector. Rotated counterclockwise.
        /// Or Vec.Zero if input is vertical.
        /// Just does Vec(-v.Y, v.X, 0.0)
        member inline v.PerpendicularInXY : Vec =
            Vec(-v.Y, v.X, 0)

        /// Returns a perpendicular horizontal vector. Rotated counterclockwise.
        /// Just does Vec(-v.Y, v.X, 0.0)
        /// On vertical input vector resulting vector is of zero length.
        static member inline perpendicularInXY (v:Vec) : Vec =
            Vec(-v.Y, v.X, 0.0)

        /// 90-degree rotation Counter-Clockwise around Z-axis.
        member inline v.RotateOnZ90CCW : Vec =
            Vec( -v.Y, v.X, v.Z)

        /// 90-degree rotation Counter-Clockwise around Z-axis.
        static member inline rotateOnZ90CCW(v:Vec) : Vec =
            Vec( -v.Y, v.X, v.Z)

        /// 90-degree rotation clockwise around Z-axis.
        member inline v.RotateOnZ90CW : Vec =
            Vec(v.Y, -v.X, v.Z)

        /// 90-degree rotation clockwise around Z-axis.
        static member inline rotateOnZ90CW(v:Vec) : Vec =
            Vec(  v.Y, -v.X, v.Z)

        /// Rotates the 3D vector around the Z-axis by a given number of quarter-circles (i.e. multiples of 90
        /// degrees or Pi/2 radians). A positive number rotates counter-clockwise, a
        /// negative number rotates clockwise. The length of the vector is preserved.
        member inline v.RotateByQuarterCircle(numberOfQuarters:int) : Vec =
            Vec.rotateByQuarterCircle numberOfQuarters v

        /// Rotates a 3D vector around the Z-axis by a given number of quarter-circles (i.e. multiples of 90
        /// degrees or Pi/2 radians). A positive number rotates counter-clockwise, a
        /// negative number rotates clockwise. The length of the vector is preserved.
        static member rotateByQuarterCircle (numberOfQuarters:int) (v:Vec) : Vec =
            let mutable nQuad = numberOfQuarters % 4
            if nQuad < 0 then nQuad <- nQuad + 4
            match nQuad with
            | 0 -> v
            | 1 -> Vec(-v.Y, v.X, v.Z)
            | 2 -> Vec(-v.X, -v.Y, v.Z)
            | 3 -> Vec(v.Y, -v.X, v.Z)
            | _ -> v // should never happen because of the modulo % 4 operation

        /// The diamond angle.
        /// Calculates the proportion of X to Y component.
        /// It is always positive and in the range of 0.0 to 4.0 (for 360 degrees)
        /// 0.0 = Xaxis, going Counter-Clockwise.
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.DirectionDiamondInXY : float =
            // https://stackoverflow.com/a/14675998/969070
            if isTooTiny (abs v.X + abs v.Y) then failVertical "Vec.DirectionDiamondInXY" v
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

        /// The diamond angle.
        /// Calculates the proportion of X to Y component.
        /// It is always positive and in the range of 0.0 to 4.0 (for 360 degrees)
        /// 0.0 = Xaxis, going Counter-Clockwise.
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline directionDiamondInXY(v:Vec) : float =
            v.DirectionDiamondInXY

        /// Returns the angle in radians from X-axis,
        /// Going Counter-Clockwise till two Pi.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.Direction2PiInXY : float =
            // https://stackoverflow.com/a/14675998/969070
            if isTooTiny (abs v.X + abs v.Y) then failVertical "Vec.Direction2PiInXY" v
            let a = Math.Atan2(v.Y, v.X)
            if a < 0. then
                a + UtilEuclid.twoPi
            else
                a

        /// Returns the angle in radians from X-axis,
        /// Going Counter-Clockwise till two Pi.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline direction2PiInXY (v:Vec) : float =
            v.Direction2PiInXY

        /// Returns the angle in radians from X-axis,
        /// Ignores orientation.
        /// Range 0.0 to Pi.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.DirectionPiInXY : float =
            // https://stackoverflow.com/a/14675998/969070
            if isTooTiny (abs v.X + abs v.Y) then failVertical "Vec.DirectionPiInXY" v
            let a = Math.Atan2(v.Y, v.X)
            if a < 0. then
                a + Math.PI
            else
                a

        /// Returns the angle in radians from X-axis,
        /// Ignores orientation.
        /// Range 0.0 to Pi.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline directionPiInXY (v:Vec) : float =
            v.DirectionPiInXY

        /// Returns the angle in degrees from X-axis.
        /// Going Counter-Clockwise till 360.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.Direction360InXY : float =
            v.Direction2PiInXY |> toDegrees

        /// Returns the angle in degrees from X-axis.
        /// Going Counter-Clockwise till 360.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline direction360InXY (v:Vec) : float =
            v.Direction360InXY

        /// Returns the angle in degrees from X-axis,
        /// Ignores orientation.
        /// Range 0.0 to 180.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.Direction180InXY : float =
            v.DirectionPiInXY |> toDegrees

        /// Returns the angle in degrees from X-axis,
        /// Ignores orientation.
        /// Range 0.0 to 180.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline direction180InXY (v:Vec) : float =
            v.Direction180InXY

        /// Returns positive angle for rotating Counter-Clockwise from this vector to vector 'b' .
        /// In Diamond Angle. Using only proportion of X to Y components.
        /// Range of 0.0 to 4.0 (for 360 degrees)
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.AngleDiamondInXYTo (b:Vec) : float =
            let r = b.DirectionDiamondInXY - v.DirectionDiamondInXY
            if r >= 0. then  r
            else r + 4.0

        /// Returns positive angle for rotating Counter-Clockwise from vector 'a' to vector 'b' .
        /// In Diamond Angle. Using only proportion of X to Y components.
        /// Range of 0.0 to 4.0 (for 360 degrees)
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline angleDiamondInXY (a:Vec, b:Vec) : float =
            a.AngleDiamondInXYTo(b)

        /// Checks if the angle between the two 3D vectors is less than 90 degrees.
        /// Calculates the dot product of two 3D vectors.
        /// Then checks if it is bigger than 1e-12.
        /// Fails if any of the two vectors is shorter than zeroLengthTolerance (1e-12).
        member inline v.MatchesOrientation (other:Vec) : bool =
            if isTooTinySq(v.LengthSq    ) then failTooSmall2 "Vec.MatchesOrientation" v other
            if isTooTinySq(other.LengthSq) then failTooSmall2 "Vec.MatchesOrientation" other v
            v *** other > 1e-12

        /// Checks if the angle between this 3D vectors and a 3D unit-vector is less than 90 degrees.
        /// Calculates the dot product of two 3D vectors.
        /// Then checks if it is bigger than 1e-12.
        /// Fails if the vector is shorter than zeroLengthTolerance (1e-12).
        member inline v.MatchesOrientation (other:UnitVec) : bool =
            if isTooTinySq(v.LengthSq) then failTooSmall2 "Vec.MatchesOrientation" v other
            v *** other > 1e-12

        /// Checks if the angle between the two 3D vectors is less than 90 degrees.
        /// Calculates the dot product of two 3D vectors.
        /// Then checks if it is bigger than 1e-12.
        /// Fails if any of the two vectors is shorter than zeroLengthTolerance (1e-12).
        static member inline matchesOrientation (v:Vec) (other:Vec) : bool =
            v.MatchesOrientation other

        /// Checks if the angle between the two 3D vectors is more than 90 degrees.
        /// Calculates the dot product of two 3D vectors.
        /// Then checks if it is smaller than minus 1e-12.
        /// Fails if any of the two vectors is shorter than zeroLengthTolerance (1e-12).
        member inline v.IsOppositeOrientation (other:Vec) : bool =
            if isTooTinySq(v.LengthSq    ) then failTooSmall2 "Vec.IsOppositeOrientation" v other
            if isTooTinySq(other.LengthSq) then failTooSmall2 "Vec.IsOppositeOrientation" other v
            v *** other < -1e-12

        /// Checks if the angle between this 3D vectors and a 3D unit-vector is more than 90 degrees.
        /// Calculates the dot product of a 3D vector and a unit-vectors.
        /// Then checks if it is smaller than -1e-12.
        /// Fails if the vector is shorter than zeroLengthTolerance (1e-12).
        member inline v.IsOppositeOrientation (other:UnitVec) : bool =
            if isTooTinySq(v.LengthSq) then failTooSmall2 "Vec.IsOppositeOrientation" v other
            v *** other < -1e-12

        /// Checks if the angle between the two 3D vectors is more than 90 degrees.
        /// Calculates the dot product of two 3D vectors.
        /// Then checks if it is smaller than minus 1e-12.
        /// Fails if any of the two vectors is shorter than zeroLengthTolerance (1e-12).
        static member inline isOppositeOrientation (v:Vec) (other:Vec) : bool =
            v.IsOppositeOrientation other

        /// Checks if 3D vector is parallel to the world X axis. Ignoring orientation.
        /// The absolute deviation tolerance along Y and Z axis is 1e-9 (axisAlignmentTolerance).
        /// Fails on vectors shorter than 1e-6.
        member inline v.IsXAligned : bool =
            let x = abs (v.X)
            let y = abs (v.Y)
            let z = abs (v.Z)
            if isTooSmall (x+y+z) then
                failTooSmall "Vec.IsXAligned" v
            y < axisAlignmentTolerance && z < axisAlignmentTolerance

        /// Checks if 3D vector is parallel to the world X axis. Ignoring orientation.
        /// The absolute deviation tolerance along Y and Z axis is 1e-9 (axisAlignmentTolerance).
        /// Fails on vectors shorter than 1e-6.
        static member inline isXAligned (v:Vec) : bool =
            v.IsXAligned

        /// Checks if 3D vector is parallel to the world Y axis. Ignoring orientation.
        /// The absolute deviation tolerance along X and Z axis is 1e-9 (axisAlignmentTolerance).
        /// Fails on vectors shorter than 1e-6.
        member inline v.IsYAligned : bool =
            let x = abs (v.X)
            let y = abs (v.Y)
            let z = abs (v.Z)
            if isTooSmall (x+y+z) then
                failTooSmall "Vec.IsYAligned" v
            x < axisAlignmentTolerance && z < axisAlignmentTolerance

        /// Checks if 3D vector is parallel to the world Y axis. Ignoring orientation.
        /// The absolute deviation tolerance along X and Z axis is 1e-9 (axisAlignmentTolerance).
        /// Fails on vectors shorter than 1e-6.
        static member inline isYAligned (v:Vec) : bool =
            v.IsYAligned

        /// Checks if 3D vector is parallel to the world Z axis. Ignoring orientation.
        /// The absolute deviation tolerance along X and Y axis is 1e-9 (axisAlignmentTolerance).
        /// Fails on vectors shorter than 1e-6.
        /// Same as v.IsVertical
        member inline v.IsZAligned : bool =
            let x = abs (v.X)
            let y = abs (v.Y)
            let z = abs (v.Z)
            if isTooSmall (x+y+z) then
                failTooSmall "Vec.IsZAligned" v
            x < axisAlignmentTolerance && y < axisAlignmentTolerance

        /// Checks if 3D vector is parallel to the world Z axis. Ignoring orientation.
        /// The absolute deviation tolerance along X and Y axis is 1e-9 (axisAlignmentTolerance).
        /// Fails on vectors shorter than 1e-6.
        /// Same as v.IsVertical
        static member inline isZAligned (v:Vec) : bool =
            v.IsZAligned

        /// Checks if 3D vector is parallel to the world Z axis. Ignoring orientation.
        /// The absolute deviation tolerance along X and Y axis is 1e-9 (axisAlignmentTolerance).
        /// Fails on vectors shorter than 1e-6.
        /// Same as v.IsZAligned
        member inline v.IsVertical : bool =
            let x = abs (v.X)
            let y = abs (v.Y)
            let z = abs (v.Z)
            if isTooSmall (x+y+z) then
                failTooSmall "Vec.IsVertical" v
            x < axisAlignmentTolerance && y < axisAlignmentTolerance

        /// Checks if 3D vector is parallel to the world Z axis. Ignoring orientation.
        /// The absolute deviation tolerance along X and Y axis is 1e-9 (axisAlignmentTolerance).
        /// Fails on vectors shorter than 1e-6.
        /// Same as v.IsZAligned
        static member inline isVertical (v:Vec) : bool =
            v.IsVertical

        /// Checks if 3D vector is horizontal (Z component is almost zero).
        /// The absolute deviation tolerance along Z axis is 1e-9 (axisAlignmentTolerance).
        /// Fails on vectors shorter than 1e-6.
        member inline v.IsHorizontal : bool =
            let x = abs (v.X)
            let y = abs (v.Y)
            let z = abs (v.Z)
            if isTooSmall (x+y+z) then
                failTooSmall "Vec.IsHorizontal" v
            z < axisAlignmentTolerance

        /// Checks if 3D vector is horizontal (Z component is almost zero).
        /// The absolute deviation tolerance along Z axis is 1e-9 (axisAlignmentTolerance).
        /// Fails on vectors shorter than 1e-6.
        static member inline isHorizontal (v:Vec) : bool =
            v.IsHorizontal

        /// Checks if two 3D vectors are parallel.
        /// Ignores the line orientation.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        member inline this.IsParallelTo(other:Vec, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) : bool =
            let sa = this.LengthSq
            if isTooTinySq(sa) then failTooSmall2 "Vec.IsParallelTo" this other
            let sb = other.LengthSq
            if isTooTinySq(sb) then failTooSmall2 "Vec.IsParallelTo" other this
            let au = this  * (1.0 / sqrt sa)
            let bu = other * (1.0 / sqrt sb)
            abs(bu *** au) > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:

        /// Checks if this 3D vectors and a 3D unit-vector are parallel.
        /// Ignores the line orientation.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        member inline this.IsParallelTo(other:UnitVec, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) : bool =
            let sa = this.LengthSq
            if isTooTinySq(sa) then failTooSmall2 "Vec.IsParallelTo" this other
            let au = this * (1.0 / sqrt sa)
            abs(other *** au) > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:

        /// Checks if two 3D vectors are parallel.
        /// Ignores the line orientation.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        static member inline isParallelTo (minCosine:float<Cosine.cosine>) (a:Vec) (b:Vec) : bool =
            a.IsParallelTo(b, minCosine)

        /// Checks if this 3D vectors and a 3D unit-vector are parallel.
        /// Ignores the line orientation.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        static member inline isParallelToUnitVec (minCosine:float<Cosine.cosine>) (b:UnitVec) (a:Vec): bool =
            a.IsParallelTo(b, minCosine)

        /// Checks if two 3D vectors are parallel.
        /// Takes the line orientation into account too.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        member inline this.IsParallelAndOrientedTo (other:Vec, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) : bool =
            let sa = this.LengthSq
            if isTooTinySq(sa) then failTooSmall2 "Vec.IsParallelAndOrientedTo" this other
            let sb = other.LengthSq
            if isTooTinySq(sb) then failTooSmall2 "Vec.IsParallelAndOrientedTo" other this
            let au = this  * (1.0 / sqrt sa)
            let bu = other * (1.0 / sqrt sb)
            bu *** au > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:

        /// Checks if this 3D vectors and a 3D unit-vector are parallel.
        /// Takes the line orientation into account too.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        member inline this.IsParallelAndOrientedTo (other:UnitVec, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) : bool =
            let sa = this.LengthSq
            if isTooTinySq(sa) then failTooSmall2 "Vec.IsParallelAndOrientedTo" this other
            let au = this * (1.0 / sqrt sa)
            other *** au > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:

        /// Checks if two 3D vectors are parallel.
        /// Takes the line orientation into account too.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        static member inline isParallelAndOrientedTo (minCosine:float<Cosine.cosine>) (a:Vec) (b:Vec) : bool =
            a.IsParallelAndOrientedTo(b, minCosine)

        /// Checks if this 3D vectors and a 3D unit-vector are parallel.
        /// Takes the line orientation into account too.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        static member inline isParallelAndOrientedToUnitVec (minCosine:float<Cosine.cosine>) (b:UnitVec) (a:Vec) : bool =
            a.IsParallelAndOrientedTo(b, minCosine)

        /// Checks if two 3D vectors are perpendicular to each other.
        /// The default angle tolerance is 89.75 to 90.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// The default cosine is 0.0043633 ( = 89.75 deg)
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        member inline this.IsPerpendicularTo (other:Vec, [<OPT;DEF(Cosine.``89.75``)>] maxCosine:float<Cosine.cosine> ) : bool =
            let sa = this.LengthSq
            if isTooTinySq(sa) then failTooSmall2 "Vec.IsPerpendicularTo" this other
            let sb = other.LengthSq
            if isTooTinySq(sb) then failTooSmall2 "Vec.IsPerpendicularTo" other this
            let au = this  * (1.0 / sqrt sa)
            let bu = other * (1.0 / sqrt sb)
            let d = bu *** au
            !^(abs d) < maxCosine

        /// Checks if this 3D vectors and a 3D unit-vector are perpendicular to each other.
        /// The default angle tolerance is 89.75 to 90.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// The default cosine is 0.0043633 ( = 89.75 deg)
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        member inline this.IsPerpendicularTo (other:UnitVec, [<OPT;DEF(Cosine.``89.75``)>] maxCosine:float<Cosine.cosine> ) : bool =
            let sa = this.LengthSq
            if isTooTinySq(sa) then failTooSmall2 "Vec.IsPerpendicularTo" this other
            let au = this * (1.0 / sqrt sa)
            let d = other *** au
            !^(abs d) < maxCosine

        /// Checks if two 3D vectors are perpendicular to each other.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        static member inline isPerpendicularTo (maxCosine:float<Cosine.cosine>) (a:Vec) (b:Vec) : bool =
            a.IsPerpendicularTo(b, maxCosine)

        /// Checks if this 3D vectors and a 3D unit-vector are perpendicular to each other.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        /// Fails on vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
        static member inline isPerpendicularToUnitVec (maxCosine:float<Cosine.cosine>) (b:UnitVec) (a:Vec) : bool =
            a.IsPerpendicularTo(b, maxCosine)

        /// Multiplies (or applies) a RigidMatrix to a 3D vector.
        /// Since a 3D vector represents a direction or translation in space, but not a location,
        /// all translations are ignored. (Homogeneous Vector)
        member inline v.TransformRigid (m:RigidMatrix) : Vec =
            v *** m // operator *** is defined in RigidMatrix.fs

        /// Multiplies (or applies) a RigidMatrix to a 3D vector.
        /// Since a 3D vector represents a direction or translation in space, but not a location,
        /// all translations are ignored. (Homogeneous Vector)
        static member inline transformRigid (m:RigidMatrix) (v:Vec) : Vec =
            v.TransformRigid(m)



    // #endregion
    // #region Static members


        /// Returns the World X-axis with length one: Vec(1, 0, 0)
        static member inline Xaxis : Vec =
            Vec(1, 0, 0)

        /// Returns the World Y-axis with length one: Vec(0, 1, 0)
        static member inline Yaxis : Vec =
            Vec(0, 1, 0)

        /// Returns the World Z-axis with length one: Vec(0, 0, 1)
        static member inline Zaxis : Vec =
            Vec(0, 0, 1)

        /// Checks if two 3D vectors are equal within tolerance.
        /// Identical vectors in opposite directions are not considered equal.
        /// Use a tolerance of 0.0 to check for an exact match.
        static member inline equals (tol:float) (a:Vec) (b:Vec) : bool =
            abs (a.X-b.X) <= tol &&
            abs (a.Y-b.Y) <= tol &&
            abs (a.Z-b.Z) <= tol

        /// Check if two 3D vectors  are not equal within a given tolerance.
        /// Use a tolerance of 0.0 to check if the two vectors are not exactly equal.
        static member notEquals (tol:float) (a:Vec) (b:Vec) : bool =
            abs (a.X-b.X) > tol ||
            abs (a.Y-b.Y) > tol ||
            abs (a.Z-b.Z) > tol

        /// Returns the distance between the tips of two 3D vectors.
        static member inline difference (a:Vec) (b:Vec) : float =
            let x = a.X - b.X
            let y = a.Y - b.Y
            let z = a.Z - b.Z
            sqrt (x*x + y*y + z*z)

        /// Returns the squared distance between the tips of two 3D vectors.
        /// This operation is slightly faster than Vec.difference and sufficient for many algorithms like finding closest vectors.
        static member inline differenceSq (a:Vec) (b:Vec) : float =
            let x = a.X - b.X
            let y = a.Y - b.Y
            let z = a.Z - b.Z
            x*x + y*y + z*z

        /// Accepts any type that has a X, Y and Z (UPPERCASE) member that can be converted to a float.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline createFromMembersXYZ vec : Vec =
            let x = ( ^T : (member X : _) vec)
            let y = ( ^T : (member Y : _) vec)
            let z = ( ^T : (member Z : _) vec)
            try
                Vec(float x, float y, float z)
            with e ->
                fail2 "Vec.createFromMembersXYZ" vec e |> unbox // unbox to make type checker happy

        /// Accepts any type that has a x, y and z (lowercase) member that can be converted to a float.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline createFromMembersxyz vec : Vec =
            let x = ( ^T : (member x : _) vec)
            let y = ( ^T : (member y : _) vec)
            let z = ( ^T : (member z : _) vec)
            try
                Vec(float x, float y, float z)
            with e ->
                fail2 "Vec.createFromMembersxyz" vec e |> unbox // unbox to make type checker happy

        /// Create 3D vector from 3D point.
        static member inline createFromPnt (pt:Pnt) : Vec =
            Vec(pt.X, pt.Y, pt.Z)

        /// Create 3D vector from 3D unit-vector.
        static member inline createFromUnitVec (v:UnitVec) : Vec =
            Vec(v.X, v.Y, v.Z)

        /// Convert 3D vector to 2D point by ignoring Z value.
        static member inline asPt(v:Vec) : Pt =
            Pt(v.X, v.Y)

        /// Gets the X part of this 3D vector.
        static member inline getX (v:Vec) : float =
           v.X

        /// Gets the Y part of this 3D vector.
        static member inline getY (v:Vec) : float =
            v.Y

        /// Gets the Z part of this 3D vector.
        static member inline getZ (v:Vec) : float =
            v.Z

        /// Returns a new 3D vector with new X coordinate, Y and Z stay the same.
        /// Same as Vec.withX.
        static member inline setX (x:float) (v:Vec) : Vec =
            Vec(x, v.Y, v.Z)

        /// Returns a new 3D vector with new Y coordinate, X and Z stay the same.
        /// Same as Vec.withY.
        static member inline setY (y:float) (v:Vec) : Vec =
            Vec(v.X, y, v.Z)

        /// Returns a new 3D vector with new Z coordinate, X and Y stay the same.
        /// Same as Vec.withZ.
        static member inline setZ (z:float) (v:Vec) : Vec =
            Vec(v.X, v.Y, z)

        /// Add two 3D vectors together. Returns a new 3D vector.
        static member inline add (a:Vec) (b:Vec) : Vec =
            Vec (a.X + b.X, a.Y + b.Y, a.Z + b.Z)

        /// Multiplies a 3D vector with a scalar, also called scaling a vector.
        /// Returns a new 3D vector scaled by the provided factor.
        static member inline scale (f:float) (v:Vec) : Vec =
            Vec (v.X * f, v.Y * f, v.Z * f)

        /// Adds the given delta to the X component and returns a new 3D vector.
        static member inline addX x (v:Vec) : Vec =
            Vec (v.X+x, v.Y, v.Z)

        /// Adds the given delta to the Y component and returns a new 3D vector.
        static member inline addY y (v:Vec) : Vec =
            Vec (v.X, v.Y+y, v.Z)

        /// Adds the given delta to the Z component and returns a new 3D vector.
        static member inline addZ z (v:Vec) : Vec =
            Vec (v.X, v.Y, v.Z+z)

        /// Returns the length of the 3D vector.
        static member inline length (v:Vec) : float =
            v.Length

        /// Returns the squared length of the 3D vector.
        /// The square length is faster to calculate and often good enough for use cases such as sorting vectors by length.
        static member inline lengthSq (v:Vec) : float =
            v.LengthSq

        /// Returns a new 3D vector from X, Y and Z parts.
        static member inline create (x:float, y:float, z:float) : Vec =
            Vec(x, y, z)

        /// Returns a new 3D vector from start and end point.
        static member inline create (start:Pnt, ende:Pnt) : Vec =
            ende-start

        /// Returns a 3D vector from z value and 2D vector.
        static member inline createFromVcWithZ (z:float) (v:Vc) : Vec =
            Vec (v.X, v.Y, z)

        /// Project vector to World X-Y plane.
        /// Use Vec.asVc to convert to a 2D vector.
        static member inline projectToXYPlane (v:Vec) : Vec =
            Vec(v.X, v.Y, 0.0)

        /// Negate or inverse a 3D vectors. Returns a new 3D vector.
        /// Same as Vec.flip.
        static member inline reverse (v:Vec) : Vec =
            -v

        /// Negate or inverse a 3D vectors. Returns a new 3D vector.
        /// Same as Vec.reverse.
        static member inline flip (v:Vec) : Vec =
            -v

        /// Flips the vector if Z part is smaller than 0.0
        static member inline flipToPointUp (v:Vec) : Vec =
            if v.Z < 0.0 then -v else v

        /// Returns the 3D vector unitized, fails on zero length vectors.
        /// But as Vec type not as UnitVec
        static member inline unitizeAsVec (v:Vec) : Vec =
            let x = v.X
            let y = v.Y
            let z = v.Z
            let l = sqrt (x*x + y*y + z*z)
            if isTooTiny l then failUnit3 "Vec.unitizeAsVec" x y z
            let f = 1. / l
            Vec(f*x, f*y, f*z)

        /// Returns 3D vector unitized, the error message is used in the Exception if the vector is too short.
        static member inline unitizeWithErrMsg (errMsg:string) (v:Vec) : UnitVec =
            let x = v.X
            let y = v.Y
            let z = v.Z
            let l = sqrt (x*x + y*y + z*z)
            if isTooTiny l then
                failTooSmall $"Vec.unitizeWithErrMsg: {errMsg}" v
            let f = 1. / l
            UnitVec.createUnchecked(x*f, y*f, z*f)

        /// Returns 3D vector unitized.
        /// If vector has zero length this will return an INFINITY unit vector
        static member inline unitizeUnchecked (v:Vec) : UnitVec =
            let x = v.X
            let y = v.Y
            let z = v.Z
            let f = 1. / sqrt (x*x + y*y + z*z)
            UnitVec.createUnchecked(f*x, f*y, f*z)

        /// Unitize 3D vector, if input vector is shorter than 1e-6 the default unit-vector is returned.
        static member inline unitizeOrDefault (defaultUnitVector:UnitVec) (v:Vec) : UnitVec =
            let l = v.LengthSq
            if l < 1e-12  then  // = (1e-6)^2
                defaultUnitVector
            else
                let f = 1.0 / sqrt(l)
                UnitVec.createUnchecked(v.X*f, v.Y*f, v.Z*f)

        /// Returns three vector's Determinant.
        /// This is also the signed volume of the Parallelepipeds define by these three vectors.
        /// Also called scalar triple product, mixed product, Box product, or in German: Spatprodukt.
        /// It is defined as the dot product of one of the vectors with the Cross Product of the other two.
        static member inline determinant3 (u:Vec, v:Vec, w:Vec) : float =
            u.X*v.Y*w.Z + v.X*w.Y*u.Z + w.X*u.Y*v.Z - w.X*v.Y*u.Z - v.X*u.Y*w.Z - u.X*w.Y*v.Z

        /// Returns positive angle between two 3D vectors in radians.
        /// Takes vector orientation into account,
        /// Range 0.0 to Pi( = 0 to 180 degrees).
        static member inline anglePi (a:Vec) (b:Vec) : float =
            UnitVec.anglePi a.Unitized b.Unitized

        /// Returns positive angle between two 3D vectors in degrees.
        /// Takes vector orientation into account,
        /// Range 0 to 180 degrees.
        static member inline angle180 (a:Vec) (b:Vec) : float =
            UnitVec.angle180 a.Unitized b.Unitized

        /// Returns positive angle between two 3D vectors in radians.
        /// Ignores vector orientation,
        /// Range: 0.0 to Pi/2 ( = 0 to 90 degrees)
        static member inline angleHalfPi (a:Vec) (b:Vec) : float =
            UnitVec.angleHalfPi a.Unitized b.Unitized

        /// Returns positive angle between two 3D vectors in degrees.
        /// Ignores vector orientation,
        /// Range: 0 to 90 degrees.
        static member inline angle90 (a:Vec) (b:Vec) : float =
            UnitVec.angle90 a.Unitized b.Unitized

        /// Returns positive angle from vector 'a' to vector 'b' projected in X-Y plane.
        /// In radians.
        /// Considering Counter-Clockwise rotation round the World Zaxis.
        /// Range: 0.0 to 2 Pi ( = 0 to 360 degrees)
        static member inline angle2PiInXY (a:Vec, b:Vec) : float =
            let r = b.Direction2PiInXY  - a.Direction2PiInXY
            if r >= 0. then  r
            else r + UtilEuclid.twoPi

        /// Returns positive angle of two 3D vector projected in X-Y plane.
        /// In degrees.
        /// Considering positive rotation round the World Z-axis.
        /// Range: 0 to 360 degrees.
        static member inline angle360InXY (a:Vec, b:Vec) : float =
            Vec.angle2PiInXY (a, b) |> toDegrees

        /// Returns a (not unitized) bisector vector in the middle direction.
        /// Code : a.Unitized + b.Unitized.
        static member inline bisector (a:Vec) (b:Vec) : Vec =
            a.Unitized + b.Unitized

        /// Ensure that the 3D  vector has a positive dot product with given 3D orientation vector.
        static member inline matchOrientation (orientationToMatch:Vec) (vecToFlip:Vec) : Vec =
            if orientationToMatch *** vecToFlip < 0.0 then -vecToFlip else vecToFlip

        /// Ensure that the 3D vector has a positive dot product with given 3D orientation unit-vector.
        static member inline matchUnitVecOrientation (orientationToMatch:UnitVec) (vecToFlip:Vec) : Vec =
            if orientationToMatch *** vecToFlip < 0.0 then -vecToFlip else vecToFlip


        // #endregion
        // #region Rotate

        /// Rotate by Quaternion around Origin
        static member inline rotate (q:Quaternion) (pt:Vec) : Vec =
            pt *** q  // operator * is defined in Quaternion.fs

        // Note: there is intentionally no Vec.rotateWithCenter: a vector is only a direction with
        // magnitude, it has no location, so rotating it around a center point is not a valid operation.
        // See the "Points vs Vectors" section in README.md.

        /// Rotate the 3D vector around X-axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member rotateOnX (r:Rotation2D) (v:Vec) : Vec =
            Vec (v.X, r.Cos*v.Y - r.Sin*v.Z, r.Sin*v.Y + r.Cos*v.Z)

        /// Rotate the 3D vector around Y-axis, from Z to X-axis, Counter Clockwise looking from back.
        static member rotateOnY (r:Rotation2D) (v:Vec) : Vec =
            Vec (r.Sin*v.Z + r.Cos*v.X, v.Y, r.Cos*v.Z - r.Sin*v.X)

        /// Rotate the 3D vector around Z-axis, from X to Y-axis, Counter Clockwise looking from top.
        static member rotateOnZ (r:Rotation2D) (v:Vec) : Vec =
            Vec (r.Cos*v.X - r.Sin*v.Y, r.Sin*v.X + r.Cos*v.Y, v.Z)

        /// Rotate the 3D vector in degrees around X-axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member inline rotateOnXDeg (angDegree) (v:Vec) : Vec =
            Vec.rotateOnX (Rotation2D.createFromDegrees angDegree) v

        /// Rotate the 3D vector in degrees around Y-axis, from Z to X-axis, Counter Clockwise looking from back.
        static member inline rotateOnYDeg (angDegree) (v:Vec) : Vec =
            Vec.rotateOnY (Rotation2D.createFromDegrees angDegree) v

        /// Rotate the 3D vector in degrees around Z-axis, from X to Y-axis, Counter Clockwise looking from top.
        static member inline rotateOnZDeg (angDegree) (v:Vec) : Vec =
            Vec.rotateOnZ (Rotation2D.createFromDegrees angDegree) v


        /// Linearly interpolates between two vectors.
        /// e.g. rel=0.5 will return the middle vector, rel=1.0 the end vector,
        /// rel=1.5 a vector half the distance beyond the end vector.
        static member lerp (start:Vec, ende:Vec, rel:float) : Vec = // not curried because argument order is important and would be confusing
            Vec(
                start.X + rel * (ende.X - start.X),
                start.Y + rel * (ende.Y - start.Y),
                start.Z + rel * (ende.Z - start.Z)
            )

        /// Spherically interpolates between start and end by amount rel (0.0 to 1.0).
        /// The difference between this and linear interpolation (aka, "lerp") is that the vectors are treated as directions rather than points in space.
        /// The direction of the returned vector is interpolated by the angle and its magnitude is interpolated between the magnitudes of start and end.
        /// Interpolation continues before and after the range of 0.0 and 1.0.
        static member slerp (start:Vec, ende:Vec, rel:float) : Vec = // not curried because argument order is important and would be confusing
            // https://en.wikipedia.org/wiki/Slerp
            // implementation tested in Rhino!
            let sLen = start.Length
            let eLen = ende.Length
            if isTooTiny sLen then failTooSmall "Vec.slerp" start
            if isTooTiny eLen then failTooSmall "Vec.slerp" ende
            let fs = 1.0 / sLen
            let fe = 1.0 / eLen
            let su  = start * fs //unitized start vector
            let eu  = ende  * fe //unitized end   vector
            let dot = su *** eu
            if dot > float Cosine.``0.05`` then  // vectors are in the same direction interpolate linear only
                Vec.lerp (start, ende, rel)
            elif dot < float Cosine.``179.95`` then
                fail2 "Vec.slerp vectors are 180 deg opposite." start ende |> unbox // unbox to make type checker happy
            else
                let ang = acos(dot) // the angle between the two vectors
                let perp = eu - su * dot |> Vec.unitize // a vector perpendicular to start and in the same plane with ende.
                let theta = ang*rel // the angle part we want for the result
                let theta360 = (theta + UtilEuclid.twoPi) % UtilEuclid.twoPi // make sure it is in the range 0.0 to 2 Pi (360 degrees)
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

        /// Returns positive or negative slope of a 3D vector in radians.
        /// This is the elevation angle from the World X-Y plane (not from the X-axis).
        /// Range -1.57 to +1.57 radians.
        static member inline slopeRadians (v:Vec) : float =
            let len2D = sqrt(v.X*v.X + v.Y*v.Y)
            Math.Atan2(v.Z, len2D)

        /// Returns positive or negative slope of a 3D vector in degrees.
        /// This is the elevation angle from the World X-Y plane (not from the X-axis).
        /// Range -90 to +90 degrees.
        static member inline slopeDegrees (v:Vec) : float =
            Vec.slopeRadians v |> toDegrees

        /// Returns positive or negative slope of a vector in Percent.
        /// In relation to X-Y plane.
        /// 100% = 45 degrees.
        /// Returns positive (or negative) Infinity if the vector is vertical.
        /// Returns NaN if the vector is zero-length.
        static member inline slopePercent (v:Vec) : float =
            //if isTooTiny (abs(v.Z)) then EuclidDivByZeroException.Raise "Euclid.Vec.slopePercent: Can't get Slope from vertical vector %O" v
            let len2D = sqrt(v.X*v.X + v.Y*v.Y)
            100.0 * v.Z / len2D

        /// Reverse vector if Z part is smaller than 0.0
        static member inline orientUp (v:Vec) : Vec =
            if v.Z < 0.0 then -v else v

        /// Reverse vector if Z part is bigger than 0.0
        static member inline orientDown (v:Vec) : Vec =
            if v.Z < 0.0 then v else -v

        /// Returns a vector that is perpendicular to the given vector and in the same vertical Plane.
        /// Projected into the X-Y plane input and output vectors are parallel and of same orientation.
        /// Not of same length, not unitized.
        /// On vertical input vector resulting vector is of zero length.
        static member inline perpendicularInVerticalPlane (v:Vec) : Vec =
            let hor = Vec(v.Y, -v.X, 0.0)
            let r = Vec.cross (v, hor)
            if v.Z < 0.0 then -r else r

        /// Checks if the angle between two vectors is smaller than a threshold angle specified as a precomputed cosine value.
        /// Ignores vector orientation. So the angle is between 0 to 90 degrees ignoring their orientation.
        /// Use the Euclid.Cosine module to get some precomputed cosine values
        /// Fails on tiny vectors shorter than 1e-12.
        /// Use Vec.isAngleBelow for considering vector orientation.
        static member inline isParallelWithin (cosineValue: float<Cosine.cosine>) (a:Vec) (b:Vec) : bool =
            let sa = a.LengthSq
            if isTooTinySq(sa) then failTooSmall2 "Vec.isParallelWithin" a b
            let sb = b.LengthSq
            if isTooTinySq(sb) then failTooSmall2 "Vec.isParallelWithin" b a
            let au = a * (1.0 / sqrt sa)
            let bu = b * (1.0 / sqrt sb)
            abs(bu *** au) > float cosineValue

        /// Checks if the angle between two vectors is bigger than a threshold angle specified as a precomputed cosine value.
        /// Ignores vector orientation. So the angle is between 0 to 90 degrees ignoring their orientation.
        /// Use the Euclid.Cosine module to get some precomputed cosine values
        /// Fails on tiny vectors shorter than 1e-12.
        /// Use Vec.isAngleAbove for considering vector orientation.
        static member inline isNotParallelWithin (cosineValue: float<Cosine.cosine>) (a:Vec) (b:Vec) : bool =
            let sa = a.LengthSq
            if isTooTinySq(sa) then failTooSmall2 "Vec.isNotParallelWithin" a b
            let sb = b.LengthSq
            if isTooTinySq(sb) then failTooSmall2 "Vec.isNotParallelWithin" b a
            let au = a * (1.0 / sqrt sa)
            let bu = b * (1.0 / sqrt sb)
            abs(bu *** au) < float cosineValue

        /// Checks if the angle between two vectors is smaller than a threshold angle specified as a precomputed cosine value.
        /// Considers the vector orientation too. So the angle is between 0 to 180 degrees.
        /// Use the Euclid.Cosine module to get some precomputed cosine values
        /// Fails on tiny vectors shorter than 1e-12.
        /// Use Vec.isParallelWithin to ignore vector orientation.
        static member inline isAngleBelow (cosineValue: float<Cosine.cosine>) (a:Vec) (b:Vec) : bool =
            let sa = a.LengthSq
            if isTooTinySq(sa) then failTooSmall2 "Vec.isAngleBelow" a b
            let sb = b.LengthSq
            if isTooTinySq(sb) then failTooSmall2 "Vec.isAngleBelow" b a
            let au = a * (1.0 / sqrt sa)
            let bu = b * (1.0 / sqrt sb)
            bu *** au > float cosineValue

        /// Checks if the angle between two vectors is bigger than a threshold angle specified as a precomputed cosine value.
        /// Considers the vector orientation too. So the angle is between 0 to 180 degrees.
        /// Use the Euclid.Cosine module to get some precomputed cosine values
        /// Fails on tiny vectors shorter than 1e-12.
        /// Use Vec.isNotParallelWithin to ignore vector orientation.
        static member inline isAngleAbove(cosineValue: float<Cosine.cosine>) (a:Vec) (b:Vec) : bool =
            let sa = a.LengthSq
            if isTooTinySq(sa) then failTooSmall2 "Vec.isAngleAbove" a b
            let sb = b.LengthSq
            if isTooTinySq(sb) then failTooSmall2 "Vec.isAngleAbove" b a
            let au = a * (1.0 / sqrt sa)
            let bu = b * (1.0 / sqrt sb)
            bu *** au < float cosineValue



        // #endregion
        // #region Obsolete

        [<Obsolete("Use Vec.isParallelWithin instead.")>]
        static member inline isAngle90Below (cosineValue: float<Cosine.cosine>) (a:Vec) (b:Vec) : bool =
            Vec.isParallelWithin cosineValue a b

        [<Obsolete("Use Vec.isNotParallelWithin instead.")>]
        static member inline isAngle90Above(cosineValue: float<Cosine.cosine>) (a:Vec) (b:Vec) : bool =
            Vec.isNotParallelWithin cosineValue a b

        [<Obsolete("Use Vec.isAngleBelow instead.")>]
        static member inline isAngle180Below (cosineValue: float<Cosine.cosine>) (a:Vec) (b:Vec) : bool =
            Vec.isAngleBelow cosineValue a b

        [<Obsolete("Use Vec.isAngleAbove instead.")>]
        static member inline isAngle180Above(cosineValue: float<Cosine.cosine>) (a:Vec) (b:Vec) : bool =
            Vec.isAngleAbove cosineValue a b

        [<Obsolete("Use Euclid.XLine3D module instead.")>]
        static member inline intersection (ptA:Pnt,ptB:Pnt,vA:Vec,vB:Vec) : ValueOption<float*float> =
            ValueSome (XLineXYZ.parameters(ptA.X, ptA.Y, ptA.Z, ptB.X, ptB.Y, ptB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z))

        [<Obsolete("Use Vec.isParallelTo instead. Obsolete since 0.21.0")>]
        static member inline areParallel (other:Vec) (v:Vec) : bool =
            Vec.isParallelTo Cosine.``0.25`` v other

        [<Obsolete("Use Vec.isParallelAndOrientedTo instead. Obsolete since 0.21.0")>]
        static member inline areParallelAndMatchOrientation (other:Vec) (v:Vec) : bool =
            Vec.isParallelAndOrientedTo Cosine.``0.25`` v other

        [<Obsolete("Use Vec.isPerpendicularTo instead. Obsolete since 0.21.0")>]
        static member inline arePerpendicular (other:Vec) (v:Vec) : bool =
            Vec.isPerpendicularTo Cosine.``89.75`` v other

        [<Obsolete("Use Vec.rotate instead.")>]
        static member inline rotateByQuaternion (q:Quaternion) (v:Vec) : Vec =
            Vec.rotate q v

        #nowarn "44" //obsolete

        /// Multiplies a Matrix with a 3D vector.
        /// Since a 3D vector represents a direction or translation in space, but not a location,
        /// all translations are ignored. (Homogeneous Vector)
        [<Obsolete("If the matrix is projecting this transformation will not be correct. Use RigidMatrix or with a Pnt instead.")>]
        member inline v.Transform (m:Matrix) : Vec =
            v *** m // operator *** is defined in Matrix.fs

        /// Multiplies a Matrix with a 3D vector.
        /// Since a 3D vector represents a direction or translation in space, but not a location,
        /// all translations are ignored. (Homogeneous Vector)
        [<Obsolete("If the matrix is projecting this transformation will not be correct. Use RigidMatrix or with a Pnt instead.")>]
        static member inline transform (m:Matrix) (v:Vec) : Vec =
            v.Transform(m)

        [<Obsolete("Use Vec.determinant3 instead.")>]
        static member inline determinant (u:Vec, v:Vec, w:Vec) : float =
            Vec.determinant3 (u, v, w)

        [<Obsolete("Use Vec.rotateOnX instead.")>]
        static member inline rotateXBy (r:Rotation2D) (v:Vec) : Vec =
            Vec.rotateOnX r v

        [<Obsolete("Use Vec.rotateOnY instead.")>]
        static member inline rotateYBy (r:Rotation2D) (v:Vec) : Vec =
            Vec.rotateOnY r v

        [<Obsolete("Use Vec.rotateOnZ instead.")>]
        static member inline rotateZBy (r:Rotation2D) (v:Vec) : Vec =
            Vec.rotateOnZ r v

        [<Obsolete("Use Vec.rotateOnXDeg instead.")>]
        static member inline rotateX (angDegree) (v:Vec) : Vec =
            Vec.rotateOnXDeg angDegree v

        [<Obsolete("Use Vec.rotateOnYDeg instead.")>]
        static member inline rotateY (angDegree) (v:Vec) : Vec =
            Vec.rotateOnYDeg angDegree v

        [<Obsolete("Use Vec.rotateOnZDeg instead.")>]
        static member inline rotateZ (angDegree) (v:Vec) : Vec =
            Vec.rotateOnZDeg angDegree v

        /// Obsolete, use Vec.addX instead. A vector has no location; this is a vector addition.
        [<Obsolete("Use Vec.addX instead.")>]
        static member inline moveX x (v:Vec) : Vec =
            Vec.addX x v

        /// Obsolete, use Vec.addY instead. A vector has no location; this is a vector addition.
        [<Obsolete("Use Vec.addY instead.")>]
        static member inline moveY y (v:Vec) : Vec =
            Vec.addY y v

        /// Obsolete, use Vec.addZ instead. A vector has no location; this is a vector addition.
        [<Obsolete("Use Vec.addZ instead.")>]
        static member inline moveZ z (v:Vec) : Vec =
            Vec.addZ z v



