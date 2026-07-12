namespace Euclid

open System
open EuclidErrors


/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type UnitVec.
[<AutoOpen>]
module AutoOpenUnitVec =
    open UtilEuclid

    /// Returns distance between the tips of two 3D unit-vectors.
    let inline private vecDist3(ax:float, ay:float, az:float, bx:float, by:float, bz:float) =
        let x = bx-ax
        let y = by-ay
        let z = bz-az
        sqrt(x*x + y*y + z*z)


    type UnitVec with

        /// Convert 3D unit-vector to 3D vector.
        member inline v.AsVec : Vec =
            Vec(v.X, v.Y, v.Z)

        /// Convert 3D unit-vector to 3D vector.
        static member inline asVec(v:UnitVec) : Vec =
            Vec(v.X, v.Y, v.Z)

        /// Convert 3D unit-vector to 3D point.
        member inline v.AsPnt : Pnt =
            Pnt(v.X, v.Y, v.Z)

        /// Convert 3D unit-vector to 3D point.
        static member inline asPnt(v:UnitVec) : Pnt =
            Pnt(v.X, v.Y, v.Z)

        /// Convert 3D unit-vector to 2D vector, discarding the Z value.
        member inline v.AsVc : Vc =
            Vc(v.X, v.Y)

        /// Convert 3D unit-vector to 2D vector, discarding the Z value.
        static member inline asVc(v:UnitVec) : Vc =
            Vc(v.X, v.Y)

        /// Convert 3D unit-vector to 2D point, discarding the Z value.
        member inline v.AsPt : Pt =
            Pt(v.X, v.Y)

        /// Convert 3D unit-vector to 2D unit-vector by ignoring Z value and unitizing again.
        /// Fails if the projection into the X-Y plane is too short.
        member inline v.AsUnitVc : UnitVc =
            UnitVc.create(v.X, v.Y)

        /// Returns the length of the 3D vector projected into World X-Y plane.
        member inline v.LengthInXY : float =
            sqrt (v.X*v.X + v.Y*v.Y)

        /// Returns the length of the 3D vector projected into World X-Y plane.
        static member inline lengthInXY(v:UnitVec) : float =
            sqrt (v.X*v.X + v.Y*v.Y)

        /// Returns the squared length of the 3D vector projected into World X-Y plane.
        /// The square length is faster to calculate and often good enough for use cases such as sorting vectors by length.
        member inline v.LengthSqInXY : float =
            v.X*v.X + v.Y*v.Y

        /// Returns the squared length of the 3D vector projected into World X-Y plane.
        /// The square length is faster to calculate and often good enough for use cases such as sorting vectors by length.
        static member inline lengthSqInXY (v:UnitVec) : float =
            v.LengthSqInXY

        /// Returns a new 3D vector with new X coordinate, Y and Z stay the same.
        member inline v.WithX x : Vec =
            Vec (x, v.Y, v.Z)

        /// Returns a new 3D vector with new X coordinate, Y and Z stay the same.
        static member inline withX  x (v:UnitVec) : Vec =
            v.WithX x

        /// Returns a new 3D vector with new y coordinate, X and Z stay the same.
        member inline v.WithY y : Vec =
            Vec (v.X, y, v.Z)

        /// Returns a new 3D vector with new y coordinate, X and Z stay the same.
        static member inline withY  y (v:UnitVec) : Vec =
            v.WithY y

        /// Returns a new 3D vector with new z coordinate, X and Y stay the same.
        member inline v.WithZ z : Vec =
            Vec (v.X, v.Y, z)

        /// Returns a new 3D vector with new z coordinate, X and Y stay the same.
        static member inline withZ z (v:UnitVec) : Vec =
            v.WithZ z

        /// Returns a perpendicular horizontal vector. Rotated counter-clockwise.
        /// Or Vec.Zero if input is vertical.
        /// Just does Vec(-v.Y, v.X, 0.0)
        member inline v.PerpendicularInXY : Vec =
            Vec(-v.Y, v.X, 0)

        /// Returns a perpendicular horizontal vector. Rotated counter-clockwise.
        /// Or Vec.Zero if input is vertical.
        /// Just does Vec(-v.Y, v.X, 0.0)
        static member inline perpendicularInXY (v:UnitVec) :Vec =
            Vec(-v.Y, v.X, 0.0)

        /// 90-degree rotation counter-clockwise around Z-axis.
        member inline v.RotateOnZ90CCW : UnitVec =
            UnitVec.createUnchecked( -v.Y, v.X, v.Z)

        /// 90-degree rotation counter-clockwise around Z-axis.
        static member inline rotateOnZ90CCW(v:UnitVec) : UnitVec =
            UnitVec.createUnchecked( -v.Y, v.X, v.Z)

        /// 90-degree rotation clockwise around Z-axis.
        member inline v.RotateOnZ90CW : UnitVec =
            UnitVec.createUnchecked(  v.Y, -v.X, v.Z)

        /// 90-degree rotation clockwise around Z-axis.
        static member inline rotateOnZ90CW(v:UnitVec) : UnitVec =
            UnitVec.createUnchecked(  v.Y, -v.X, v.Z)

        /// Rotates the 3D unit-vector around the Z-axis by a given number of quarter-circles (i.e. multiples of 90
        /// degrees or Pi/2 radians). A positive number rotates counter-clockwise, a
        /// negative number rotates clockwise. The length of the vector is preserved.
        member inline v.RotateByQuarterCircle(numberOfQuarters:int) : UnitVec =
            UnitVec.rotateByQuarterCircle numberOfQuarters v

        /// Rotates a 3D unit-vector around the Z-axis by a given number of quarter-circles (i.e. multiples of 90
        /// degrees or Pi/2 radians). A positive number rotates counter-clockwise, a
        /// negative number rotates clockwise. The length of the vector is preserved.
        static member rotateByQuarterCircle (numberOfQuarters:int) (v:UnitVec) : UnitVec =
            let mutable nQuad = numberOfQuarters % 4
            if nQuad < 0 then nQuad <- nQuad + 4
            match nQuad with
            | 0 -> v
            | 1 -> UnitVec.createUnchecked(-v.Y, v.X, v.Z)
            | 2 -> UnitVec.createUnchecked(-v.X, -v.Y, v.Z)
            | 3 -> UnitVec.createUnchecked(v.Y, -v.X, v.Z)
            | _ -> v // should never happen because of the modulo % 4 operation

        /// Cross product, of two 3D unit-vectors.
        /// The resulting vector is perpendicular to both input vectors.
        /// The length of this resulting vector is the area of the parallelogram spanned by the input vectors,
        /// or the sine of the angle between the two unit-vectors.
        /// Its direction follows the right-hand rule.
        /// A x B = |A| * |B| * sin(angle)
        member inline a.Cross (b:UnitVec) : Vec =
            Vec(a.Y * b.Z - a.Z * b.Y,
                a.Z * b.X - a.X * b.Z,
                a.X * b.Y - a.Y * b.X)

        /// Cross product, of a 3D unit-vector and a 3D vector.
        /// The resulting vector is perpendicular to both input vectors.
        /// The length of this resulting vector is the area of the parallelogram spanned by the input vectors.
        /// Its direction follows the right-hand rule.
        /// A x B = |A| * |B| * sin(angle)
        member inline a.Cross (b:Vec) : Vec =
            Vec(a.Y * b.Z - a.Z * b.Y,
                a.Z * b.X - a.X * b.Z,
                a.X * b.Y - a.Y * b.X)

        /// Cross product, of a 3D unit-vector and a 3D vector.
        /// The resulting vector is perpendicular to both input vectors.
        /// The length of this resulting vector is the area of the parallelogram spanned by the input vectors.
        /// Its direction follows the right-hand rule.
        /// A x B = |A| * |B| * sin(angle)
        static member inline cross (a:UnitVec, b:Vec)  : Vec =
            Vec(a.Y * b.Z - a.Z * b.Y,
                a.Z * b.X - a.X * b.Z,
                a.X * b.Y - a.Y * b.X)

        /// Cross product, of a 3D vector and a 3D unit-vector.
        /// The resulting vector is perpendicular to both input vectors.
        /// The length of this resulting vector is the area of the parallelogram spanned by the input vectors.
        /// Its direction follows the right-hand rule.
        /// A x B = |A| * |B| * sin(angle)
        static member inline cross (a:Vec, b:UnitVec)  : Vec =
            Vec(a.Y * b.Z - a.Z * b.Y,
                a.Z * b.X - a.X * b.Z,
                a.X * b.Y - a.Y * b.X)

        /// Dot product, or scalar product of two 3D unit-vectors.
        /// Returns a float.
        /// This float of unit-vectors is the Cosine of the angle between the two vectors.
        member inline a.Dot (b:UnitVec) : float =
            a.X * b.X + a.Y * b.Y + a.Z * b.Z

        /// Dot product, or scalar product of a 3D unit-vector with a 3D vector.
        /// Returns a float.
        /// This float is the projected length of the 3D vector on the direction of the unit-vector.
        member inline a.Dot (b:Vec) : float =
            a.X * b.X + a.Y * b.Y + a.Z * b.Z

        /// Dot product, or scalar product of a 3D unit-vector with a 3D vector.
        /// Returns a float.
        /// This float is the projected length of the 3D vector on the direction of the unit-vector.
        static member inline dot (a:UnitVec, b:Vec)  : float =
            a.X * b.X + a.Y * b.Y + a.Z * b.Z

        /// Dot product, or scalar product of a 3D vector with a 3D unit-vector.
        /// Returns a float.
        /// This float is the projected length of the 3D vector on the direction of the unit-vector.
        static member inline dot (a:Vec, b:UnitVec)  : float =
            a.X * b.X + a.Y * b.Y + a.Z * b.Z

        /// Dot product, or scalar product of two 3D unit-vector.
        /// This float of unit-vectors is the Cosine of the angle between the two vectors.
        /// Returns a float with a unit of Measure Euclid.Cosine.cosine.
        /// This is useful for comparing the angle to precomputed values in the Euclid.Cosine module.
        member inline a.DotCosine (b:UnitVec) : float<Cosine.cosine> =
            a.X * b.X + a.Y * b.Y + a.Z * b.Z  |> LanguagePrimitives.FloatWithMeasure

        /// Dot product, or scalar product of two 3D unit-vector.
        /// This float of unit-vectors is the Cosine of the angle between the two vectors.
        /// Returns a float with a unit of Measure Euclid.Cosine.cosine.
        /// This is useful for comparing the angle to precomputed values in the Euclid.Cosine module.
        static member inline dotCosine (a:UnitVec) (b:UnitVec) :float<Cosine.cosine> =
            a.X * b.X + a.Y * b.Y  + a.Z * b.Z
            |> LanguagePrimitives.FloatWithMeasure

        /// The diamond angle is always positive and in the range of 0.0 to 4.0 (for 360 degrees)
        /// 0.0 = Xaxis, going counter-clockwise. Ignoring Z component.
        /// This is the fastest angle computation since it does not use Math.Cos or Math.Sin.
        /// It is useful for radial sorting.
        /// For X-Y plane. Considers only the X and Y components of the vector.
        member inline v.DirectionDiamondInXY : float =
            // https://stackoverflow.com/a/14675998/969070
            if isTooTiny (abs v.X + abs v.Y) then failVertical "UnitVec.DirectionDiamondInXY" v // don't compose error msg directly here to keep inlined code small.
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

        /// The diamond angle is always positive and in the range of 0.0 to 4.0 (for 360 degrees)
        /// 0.0 = Xaxis, going counter-clockwise. Ignoring Z component.
        /// This is the fastest angle computation since it does not use Math.Cos or Math.Sin.
        /// It is useful for radial sorting.
        /// For X-Y plane. Considers only the X and Y components of the vector.
        static member inline directionDiamondInXY(v:UnitVec) : float =
            v.DirectionDiamondInXY

        /// Returns the angle in radians from X-axis,
        /// Going counter-clockwise till two Pi.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        /// Fails if vector is vertical.
        member inline v.Direction2PiInXY : float =
            // https://stackoverflow.com/a/14675998/969070
            if isTooTiny (abs v.X + abs v.Y) then failVertical "UnitVec.Direction2PiInXY" v // don't compose error msg directly here to keep inlined code small.
            let a = Math.Atan2(v.Y, v.X)
            if a < 0. then
                a + UtilEuclid.twoPi
            else
                a

        /// Returns the angle in radians from X-axis,
        /// Going counter-clockwise till two Pi.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        /// Fails if vector is vertical.
        static member inline direction2PiInXY (v:UnitVec) : float =
            v.Direction2PiInXY

        /// Returns the angle in radians from X-axis in World X-Y plane,
        /// Ignores orientation.
        /// Range 0.0 to Pi.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        /// Fails if vector is vertical.
        member inline v.DirectionPiInXY : float =
            // https://stackoverflow.com/a/14675998/969070
            if isTooTiny (abs v.X + abs v.Y) then failVertical "UnitVec.DirectionPiInXY" v // don't compose error msg directly here to keep inlined code small.
            let a = Math.Atan2(v.Y, v.X)
            if a < 0. then
                a + Math.PI
            else
                a

        /// Returns the angle in radians from X-axis in World X-Y plane,
        /// Ignores orientation.
        /// Range 0.0 to Pi.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        /// Fails if vector is vertical.
        static member inline directionPiInXY (v:UnitVec) : float =
            v.DirectionPiInXY

        /// Returns the angle in degrees from X-axis in World X-Y plane.
        /// Going counter-clockwise till 360.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.Direction360InXY : float =
            v.Direction2PiInXY |> toDegrees

        /// Returns the angle in degrees from X-axis in World X-Y plane.
        /// Going counter-clockwise till 360.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline direction360InXY (v:UnitVec) : float =
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
        static member inline direction180InXY (v:UnitVec) : float =
            v.Direction180InXY

        /// Returns positive angle for rotating counter-clockwise from this vector to vector 'b' .
        /// In Diamond Angle. Using only proportion of X to Y components.
        /// Range of 0.0 to 4.0 (for 360 degrees)
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.AngleDiamondInXYTo (b:UnitVec) : float =
            let r = b.DirectionDiamondInXY - v.DirectionDiamondInXY
            if r >= 0. then  r
            else r + 4.0

        /// Returns positive angle for rotating counter-clockwise from vector 'a' to vector 'b' .
        /// In Diamond Angle. Using only proportion of X to Y components.
        /// Range of 0.0 to 4.0 (for 360 degrees)
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline angleDiamondInXY (a:UnitVec, b:UnitVec) : float =
            a.AngleDiamondInXYTo(b)

        /// Checks if the angle between the two 3D unit-vectors is less than 90 degrees.
        /// Calculates the dot product of two 3D unit-vectors.
        /// Then checks if it is bigger than 1e-12.
        member inline v.MatchesOrientation (other:UnitVec) : bool =
            v *** other > 1e-12

        /// Checks if the angle between the this 3D unit vector and a 3D vector is less than 90 degrees.
        /// Calculates the dot product of a 3D vector and a unit-vectors.
        /// Then checks if it is bigger than 1e-12.
        member inline v.MatchesOrientation (other:Vec) : bool =
            if isTooTinySq(other.LengthSq) then failTooSmall "UnitVec.MatchesOrientation" other
            v *** other > 1e-12

        /// Checks if the angle between the two 3D unit-vectors is less than 90 degrees.
        /// Calculates the dot product of two 3D unit-vectors.
        /// Then checks if it is bigger than 1e-12.
        static member inline matchesOrientation (other:UnitVec) (v:UnitVec) : bool =
            v.MatchesOrientation other

        /// Checks if the angle between the two 3D unit-vectors is more than 90 degrees.
        /// Calculates the dot product of two 3D unit-vectors.
        /// Then checks if it is smaller than minus 1e-12.
        member inline v.IsOppositeOrientation (other:UnitVec) : bool =
            v *** other < -1e-12

        /// Checks if the angle between this 3D unit-vector and a 3D vector is more than 90 degrees.
        /// Calculates the dot product of a 3D vector and a unit-vector.
        /// Then checks if it is smaller than minus 1e-12.
        member inline v.IsOppositeOrientation (other:Vec) : bool =
            if isTooTinySq(other.LengthSq) then failTooSmall "UnitVec.IsOppositeOrientation" other
            v *** other < -1e-12

        /// Checks if the angle between the two 3D unit-vectors is more than 90 degrees.
        /// Calculates the dot product of two 3D unit-vectors.
        /// Then checks if it is smaller than minus 1e-12.
        static member inline isOppositeOrientation (other:UnitVec) (v:UnitVec) : bool =
            v.IsOppositeOrientation other

        /// Checks if 3D unit-vector is parallel to the world X-axis. Ignoring orientation.
        /// The absolute deviation tolerance along the Y and Z axes is 1e-9 (axisAlignmentTolerance).
        member inline v.IsXAligned : bool =
            let y = abs (v.Y)
            let z = abs (v.Z)
            y < axisAlignmentTolerance && z < axisAlignmentTolerance

        /// Checks if 3D unit-vector is parallel to the world X-axis. Ignoring orientation.
        /// The absolute deviation tolerance along the Y and Z axes is 1e-9 (axisAlignmentTolerance).
        static member inline isXAligned (v:UnitVec) : bool =
            v.IsXAligned

        /// Checks if 3D unit-vector is parallel to the world Y-axis. Ignoring orientation.
        /// The absolute deviation tolerance along the X and Z axes is 1e-9 (axisAlignmentTolerance).
        member inline v.IsYAligned : bool =
            let x = abs (v.X)
            let z = abs (v.Z)
            x < axisAlignmentTolerance && z < axisAlignmentTolerance

        /// Checks if 3D unit-vector is parallel to the world Y-axis. Ignoring orientation.
        /// The absolute deviation tolerance along the X and Z axes is 1e-9 (axisAlignmentTolerance).
        static member inline isYAligned (v:UnitVec) : bool =
            v.IsYAligned

        /// Checks if 3D unit-vector is parallel to the world Z-axis. Ignoring orientation.
        /// The absolute deviation tolerance along the X and Y axes is 1e-9 (axisAlignmentTolerance).
        /// Same as v.IsVertical
        member inline v.IsZAligned : bool =
            let x = abs (v.X)
            let y = abs (v.Y)
            x < axisAlignmentTolerance && y < axisAlignmentTolerance

        /// Checks if 3D unit-vector is parallel to the world Z-axis. Ignoring orientation.
        /// The absolute deviation tolerance along the X and Y axes is 1e-9 (axisAlignmentTolerance).
        /// Same as v.IsVertical
        static member inline isZAligned (v:UnitVec) : bool =
            v.IsZAligned

        /// Checks if 3D unit-vector is parallel to the world Z-axis. Ignoring orientation.
        /// The absolute deviation tolerance along the X and Y axes is 1e-9 (axisAlignmentTolerance).
        /// Same as v.IsZAligned
        member inline v.IsVertical : bool =
            let x = abs (v.X)
            let y = abs (v.Y)
            x < axisAlignmentTolerance && y < axisAlignmentTolerance

        /// Checks if 3D unit-vector is parallel to the world Z-axis. Ignoring orientation.
        /// The absolute deviation tolerance along the X and Y axes is 1e-9 (axisAlignmentTolerance).
        /// Same as v.IsZAligned
        static member inline isVertical (v:UnitVec) : bool =
            v.IsVertical

        /// Checks if 3D unit-vector is horizontal (Z component is almost zero).
        /// The absolute deviation tolerance along the Z-axis is 1e-9 (axisAlignmentTolerance).
        /// Fails on vectors shorter than 1e-6.
        member inline v.IsHorizontal : bool =
            let z = abs (v.Z)
            z < axisAlignmentTolerance

        /// Checks if 3D vector is horizontal (Z component is almost zero).
        /// The absolute deviation tolerance along the Z-axis is 1e-9 (axisAlignmentTolerance).
        /// Fails on vectors shorter than 1e-6.
        static member inline isHorizontal (v:UnitVec) : bool =
            v.IsHorizontal

        /// Checks if two 3D unit-vectors are parallel.
        /// Ignores the line orientation.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        member inline this.IsParallelTo(other:UnitVec, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) : bool =
            abs(other *** this) > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:

        /// Checks if this 3D unit vector and a 3D vector are parallel.
        /// Ignores the line orientation.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        member inline this.IsParallelTo(other:Vec, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) : bool =
            let sb = other.LengthSq
            if isTooTinySq(sb) then failTooSmall "UnitVec.IsParallelTo" other
            let ou = other * (1.0 / sqrt sb)
            abs(ou *** this) > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:

        /// Checks if two 3D unit-vectors are parallel. Ignoring orientation.
        /// Use a precomputed value from Euclid.Cosine module as tolerance.
        static member inline isParallelTo minCosine (a:UnitVec) (b:UnitVec) : bool =
            a.IsParallelTo(b, minCosine)

        /// Checks if two 3D unit-vectors are parallel.
        /// Takes the line orientation into account too.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        member inline this.IsParallelAndOrientedTo (other:UnitVec, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) : bool =
            other *** this > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:

        /// Checks if this 3D unit vector and a 3D vector are parallel.
        /// Takes the line orientation into account too.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// See Euclid.Cosine module.
        member inline this.IsParallelAndOrientedTo (other:Vec, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) : bool =
            let sb = other.LengthSq
            if isTooTinySq(sb) then failTooSmall "UnitVec.IsParallelAndOrientedTo" other
            let ou = other * (1.0 / sqrt sb)
            ou *** this > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:

        /// Checks if two 3D unit-vectors are parallel and orientated the same way.
        /// Use a precomputed value from Euclid.Cosine module as tolerance.
        static member inline isParallelAndOrientedTo minCosine (a:UnitVec) (b:UnitVec) : bool =
            a.IsParallelAndOrientedTo(b, minCosine)

        /// Checks if two 3D unit-vectors are perpendicular to each other.
        /// The default angle tolerance is 89.75 to 90.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// The default cosine is 0.0043633 ( = 89.75 deg)
        /// See Euclid.Cosine module.
        member inline this.IsPerpendicularTo (other:UnitVec, [<OPT;DEF(Cosine.``89.75``)>] maxCosine:float<Cosine.cosine> ) : bool =
            let d = other *** this
            float -maxCosine < d && d  < float maxCosine // = cosine of 89.75 and 90.25 degrees

        /// Checks if this 3D unit vector and a 3D vector are perpendicular to each other.
        /// The default angle tolerance is 89.75 to 90.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// The default cosine is 0.0043633 ( = 89.75 deg)
        /// See Euclid.Cosine module.
        member inline this.IsPerpendicularTo (other:Vec, [<OPT;DEF(Cosine.``89.75``)>] maxCosine:float<Cosine.cosine> ) : bool =
            let sb = other.LengthSq
            if isTooTinySq(sb) then failTooSmall "UnitVec.IsPerpendicularTo" other
            let ou = other * (1.0 / sqrt sb)
            let d = ou *** this
            float -maxCosine < d && d  < float maxCosine // = cosine of 89.75 and 90.25 degrees

        /// Checks if two 3D unit-vectors are perpendicular to each other.
        /// The default angle tolerance is 89.75 to 90.25 degrees.
        /// This tolerance can be customized by an optional minimum cosine value.
        /// The default cosine is 0.0043633 ( = 89.75 deg)
        /// See Euclid.Cosine module.
        static member inline isPerpendicularTo maxCosine (a:UnitVec) (b:UnitVec) : bool =
            a.IsPerpendicularTo(b, maxCosine)



        /// Multiplies (or applies) a RigidMatrix to a 3D unit-vector.
        /// Since a 3D vector represents a direction or translation in space, but not a location,
        /// all translations are ignored. (Homogeneous Vector)
        member inline v.TransformRigid (m:RigidMatrix) : UnitVec =
            v *** m // operator * is defined in RigidMatrix.fs


        /// Multiplies (or applies) a RigidMatrix to a 3D unit-vector.
        /// Since a 3D vector represents a direction or translation in space, but not a location,
        /// all translations are ignored. (Homogeneous Vector)
        /// The resulting vector is  unitized too.
        static member inline transformRigid (m:RigidMatrix) (v:UnitVec) : UnitVec =
            v.TransformRigid(m)


        // #endregion
        // #region Static members

        /// Create 3D unit-vector from start and endpoint. Does the unitizing too.
        static member inline create (fromPt:Pnt, toPt:Pnt) : UnitVec =
            let x = toPt.X - fromPt.X
            let y = toPt.Y - fromPt.Y
            let z = toPt.Z - fromPt.Z
            let l = sqrt(x * x  + y * y + z * z)
            if isTooTiny l then failTooClose "UnitVec.create" fromPt toPt
            let f = 1.0/l
            UnitVec.createUnchecked(x*f, y*f, z*f)

        /// Returns the World X-axis with length one: UnitVec(1, 0, 0)
        static member inline Xaxis : UnitVec =
            UnitVec.createUnchecked (1.0, 0.0, 0.0)

        /// Returns the World Y-axis with length one: UnitVec(0, 1, 0)
        static member inline Yaxis : UnitVec =
            UnitVec.createUnchecked (0.0, 1.0, 0.0)

        /// Returns the World Z-axis with length one: UnitVec(0, 0, 1)
        static member inline Zaxis : UnitVec =
            UnitVec.createUnchecked (0.0, 0.0, 1.0)

        /// Checks if two 3D unit-vectors are equal within tolerance.
        /// Identical unit-vectors in opposite directions are not considered equal.
        /// Use a tolerance of 0.0 to check for an exact match.
        static member inline equals (tol:float) (a:UnitVec) (b:UnitVec) : bool =
            abs (a.X-b.X) <= tol &&
            abs (a.Y-b.Y) <= tol &&
            abs (a.Z-b.Z) <= tol

        /// Check if two 3D unit-vectors  are not equal within a given tolerance.
        /// Use a tolerance of 0.0 to check if the two unit-vectors  are not exactly equal.
        static member notEquals (tol:float) (a:UnitVec) (b:UnitVec) : bool =
            abs (a.X-b.X) > tol ||
            abs (a.Y-b.Y) > tol ||
            abs (a.Z-b.Z) > tol

        /// Returns the distance between the tips of two 3D unit-vectors.
        static member inline difference (a:UnitVec) (b:UnitVec) : float =
            let x = b.X - a.X
            let y = b.Y - a.Y
            let z = b.Z - a.Z
            sqrt(x*x + y*y + z*z)

        /// Returns the squared distance between the tips of two 3D unit-vectors.
        /// This operation is slightly faster than Vec.difference and sufficient for many algorithms like finding closest points.
        static member inline differenceSq (a:UnitVec) (b:UnitVec) : float =
            let x = b.X - a.X
            let y = b.Y - a.Y
            let z = b.Z - a.Z
            x*x + y*y + z*z

        /// Accepts any type that has an X, Y and Z (UPPERCASE) member that can be converted to a float.
        /// Does the unitizing too.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline createFromMembersXYZ vec : UnitVec =
            let x = ( ^T : (member X : _) vec)
            let y = ( ^T : (member Y : _) vec)
            let z = ( ^T : (member Z : _) vec)
            try
                UnitVec.create(float x, float y, float z)
            with e ->
                fail2 "UnitVec.createFromMembersXYZ" vec e |> unbox // unbox to make type checker happy

        /// Accepts any type that has a x, y and z (lowercase) member that can be converted to a float.
        /// Does the unitizing too.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline createFromMembersxyz vec : UnitVec =
            let x = ( ^T : (member x : _) vec)
            let y = ( ^T : (member y : _) vec)
            let z = ( ^T : (member z : _) vec)
            try
                UnitVec.create(float x, float y, float z)
            with e ->
                fail2 "UnitVec.createFromMembersxyz" vec e  |> unbox // unbox to make type checker happy

        /// Create 3D unit-vector from 3D point. Does the unitizing too.
        static member inline createFromPnt (pt:Pnt) : UnitVec =
            let l = sqrt (pt.X*pt.X + pt.Y*pt.Y + pt.Z*pt.Z)
            if isTooTiny(l) then failTooSmall "UnitVec.createFromPnt" pt
            let li = 1. / l
            UnitVec.createUnchecked(li*pt.X, li*pt.Y, li*pt.Z)

        /// Create 3D unit-vector from 3D vector. Does the unitizing too.
        static member inline createFromVec (v:Vec) : UnitVec =
            let l = sqrt (v.X*v.X + v.Y*v.Y + v.Z*v.Z)
            if isTooTiny(l) then failTooSmall "UnitVec.createFromVec" v
            let li = 1. / l
            UnitVec.createUnchecked(li*v.X, li*v.Y, li*v.Z)

        /// Convert 3D unit-vector to 2D point by ignoring Z value.
        static member inline asPt(v:UnitVec) : Pt =
            Pt(v.X, v.Y)

        /// Convert 3D unit-vector to 2D unit-vector by ignoring Z value and unitizing again.
        static member inline asUnitVc(v:UnitVec) : UnitVc =
            UnitVc.create(v.X, v.Y)

        //static member inline cross (a:UnitVec, b:UnitVec)  moved to Vec type declaration

        //static member inline dot (a:UnitVec, b:UnitVec)  //moved to UnitVec type declaration

        /// Gets the X part of this 3D unit-vector.
        static member inline getX (v:UnitVec) : float =
            v.X

        /// Gets the Y part of this 3D unit-vector.
        static member inline getY (v:UnitVec) : float =
            v.Y

        /// Gets the Z part of this 3D unit-vector.
        static member inline getZ (v:UnitVec) : float =
            v.Z

        /// Add two 3D unit-vectors together. Returns a new (non-unitized) 3D vector.
        static member inline add (a:UnitVec) (b:UnitVec) : Vec =
            b + a

        /// Multiplies a 3D unit-vector with a scalar, also called scaling a vector.
        /// Same as UnitVec.withLength. Returns a new (non-unitized) 3D vector.
        static member inline scale (f:float) (v:UnitVec) : Vec =
            Vec (v.X * f, v.Y * f, v.Z * f)

        /// Multiplies a 3D unit-vector with a scalar, also called scaling a vector.
        /// Same as UnitVec.scale. Returns a new (non-unitized) 3D vector.
        static member inline withLength(f:float) (v:UnitVec) : Vec =
            Vec (v.X * f, v.Y * f, v.Z * f)

        /// Adds the given delta to the X component. Returns a new (non-unitized) 3D vector.
        static member inline addX x (v:UnitVec) : Vec =
            Vec (v.X+x, v.Y, v.Z)

        /// Adds the given delta to the Y component. Returns a new (non-unitized) 3D vector.
        static member inline addY y (v:UnitVec) : Vec =
            Vec (v.X, v.Y+y, v.Z)

        /// Adds the given delta to the Z component. Returns a new (non-unitized) 3D vector.
        static member inline addZ z (v:UnitVec) : Vec =
            Vec (v.X, v.Y, v.Z+z)

        /// Project vector to World X-Y plane.
        /// Use UnitVec.asVc to convert to a 2D vector instance.
        static member inline projectToXYPlane (v:UnitVec) : Vec =
            Vec(v.X, v.Y, 0.0)

        /// Negate or inverse a 3D unit-vector. Returns a new 3D unit-vector.
        /// Same as UnitVec.flip.
        static member inline reverse (v:UnitVec) : UnitVec =
            -v

        /// Negate or inverse a 3D unit-vector. Returns a new 3D unit-vector.
        /// Same as UnitVec.reverse.
        static member inline flip (v:UnitVec) : UnitVec =
            -v

        /// Flips the vector if Z part is smaller than 0.0
        static member inline flipToPointUp (v:UnitVec) : UnitVec =
            if v.Z < 0.0 then -v else v

        /// Returns three vectors Determinant.
        /// This is also the signed volume of the Parallelepipeds define by these three vectors.
        /// Also called scalar triple product, mixed product, box product, or in german: Spatprodukt.
        /// It is defined as the dot product of one of the vectors with the Cross Product of the other two.
        static member inline determinant3 (u:UnitVec, v:UnitVec, w:UnitVec) : float =
            u.X*v.Y*w.Z + v.X*w.Y*u.Z + w.X*u.Y*v.Z - w.X*v.Y*u.Z - v.X*u.Y*w.Z - u.X*w.Y*v.Z

        /// Returns angle between two 3D unit-vectors in radians.
        /// Takes vector orientation into account.
        /// Range 0.0 to Pi( = 0 to 180 degrees)
        static member inline anglePi (a:UnitVec) (b:UnitVec) : float =
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
            // at +/-1. (See Windows OS Bug 01706299 for details) (from WPF reference source code)
            let dot = a *** b
            if -0.98 < dot && dot < 0.98 then // threshold for switching 0.98 ?
                acos dot
            else
                if dot < 0. then Math.PI - 2.0 * asin(vecDist3(-a.X, -a.Y, -a.Z, b.X, b.Y, b.Z) * 0.5)
                else                       2.0 * asin(vecDist3(a.X ,  a.Y,  a.Z, b.X, b.Y, b.Z) * 0.5)

        /// Returns positive angle between two 3D unit-vectors in radians.
        /// Ignores orientation.
        /// Range 0.0 to Pi/2 ( = 0 to 90 degrees)
        static member inline angleHalfPi (a:UnitVec) (b:UnitVec) : float =
            let dot = a *** b
            let dotAbs = abs dot
            if dotAbs < 0.98 then
                acos dotAbs
            else
                if dot < 0. then 2.0 * asin(vecDist3(-a.X, -a.Y, -a.Z, b.X, b.Y, b.Z) * 0.5)
                else             2.0 * asin(vecDist3(a.X ,  a.Y,  a.Z, b.X, b.Y, b.Z) * 0.5)

        /// Returns positive angle from vector 'a' to vector 'b' projected in X-Y plane.
        /// In radians.
        /// Considering counter-clockwise rotation round the World Z-axis.
        /// Range: 0.0 to 2 Pi ( = 0 to 360 degrees)
        static member inline angle2PiInXY (a:UnitVec, b:UnitVec) : float = // not curried because argument order is important
            let r = b.Direction2PiInXY  - a.Direction2PiInXY
            if r >= 0. then  r
            else r + UtilEuclid.twoPi

        /// Returns positive angle between two 3D unit-vectors in degrees,
        /// Ignores vector orientation.
        /// Range: 0 to 90 degrees.
        static member inline angle90 (a:UnitVec) (b:UnitVec) : float =
            UnitVec.angleHalfPi a b |>  toDegrees

        /// Returns positive angle between two 3D unit-vectors in degrees.
        /// Takes vector orientation into account.
        /// Range 0 to 180 degrees.
        static member inline angle180 (a:UnitVec) (b:UnitVec) : float =
            UnitVec.anglePi a b |>  toDegrees

        /// Returns positive angle of two 3D unit-vector projected in X-Y plane in degrees.
        /// Considering positive rotation round the World Z-axis.
        /// Range:  0 to 360 degrees.
        static member inline angle360InXY (a:UnitVec, b:UnitVec) : float = // not curried because argument order is important
            UnitVec.angle2PiInXY (a, b) |> toDegrees

        /// Ensure that the 3D unit-vector has a positive dot product with given 3D orientation unit-vector.
        static member inline matchOrientation (orientationToMatch:UnitVec) (vecToFlip:UnitVec) : UnitVec =
            if orientationToMatch *** vecToFlip < 0.0 then -vecToFlip else vecToFlip

        /// Ensure that the 3D unit-vector has a positive dot product with given 3D orientation vector.
        static member inline matchVecOrientation (orientationToMatch:Vec) (vecToFlip:UnitVec) : UnitVec =
            if orientationToMatch *** vecToFlip < 0.0 then -vecToFlip else vecToFlip

        // #endregion
        // #region Rotate

        /// Rotate by Quaternion around Origin
        static member inline rotate (q:Quaternion) (pt:UnitVec) : UnitVec =
            pt *** q  // operator * is defined in Quaternion.fs

        // Note: there is intentionally no UnitVec.rotateWithCenter: a vector is only a direction with
        // magnitude, it has no location, so rotating it around a center point is not a valid operation
        // (and the translation would also break the unit-length invariant).
        // See the "Points vs Vectors" section in README.md.

        /// Rotate the 3D unit-vector around X-axis, from Y to Z-axis, counter-clockwise looking from right.
        static member inline rotateOnX (r:Rotation2D) (v:UnitVec) : UnitVec =
            UnitVec.createUnchecked (v.X, r.Cos*v.Y - r.Sin*v.Z, r.Sin*v.Y + r.Cos*v.Z)

        /// Rotate the 3D unit-vector around Y-axis, from Z to X-axis, counter-clockwise looking from back.
        static member inline rotateOnY (r:Rotation2D) (v:UnitVec) : UnitVec =
            UnitVec.createUnchecked (r.Sin*v.Z + r.Cos*v.X, v.Y, r.Cos*v.Z - r.Sin*v.X)

        /// Rotate the 3D unit-vector around Z-axis, from X to Y-axis, counter-clockwise looking from top.
        static member inline rotateOnZ (r:Rotation2D) (v:UnitVec) : UnitVec =
            UnitVec.createUnchecked (r.Cos*v.X - r.Sin*v.Y, r.Sin*v.X + r.Cos*v.Y, v.Z)

        /// Rotate the 3D unit-vector in degrees around X-axis, from Y to Z-axis, counter-clockwise looking from right.
        static member inline rotateOnXDeg (angDegree) (v:UnitVec) : UnitVec =
            UnitVec.rotateOnX (Rotation2D.createFromDegrees angDegree) v

        /// Rotate the 3D unit-vector in degrees around Y-axis, from Z to X-axis, counter-clockwise looking from back.
        static member inline rotateOnYDeg (angDegree) (v:UnitVec) : UnitVec =
            UnitVec.rotateOnY (Rotation2D.createFromDegrees angDegree) v

        /// Rotate the 3D unit-vector in degrees around Z-axis, from X to Y-axis, counter-clockwise looking from top.
        static member inline rotateOnZDeg (angDegree) (v:UnitVec) : UnitVec =
            UnitVec.rotateOnZ (Rotation2D.createFromDegrees angDegree) v

        /// Returns positive or negative slope of a 3D unit-vector in radians.
        /// This is the elevation angle from the World X-Y plane (not from the X-axis).
        /// Range -1.57 to +1.57 radians.
        static member inline slopeRadians (v:UnitVec) : float =
            v.Z |> asinSafe

        /// Returns positive or negative slope of a 3D unit-vector in degrees.
        /// This is the elevation angle from the World X-Y plane (not from the X-axis).
        /// Range -90 to +90 degrees.
        static member inline slopeDegrees (v:UnitVec) : float =
            UnitVec.slopeRadians v |> toDegrees

        /// Returns positive or negative slope of a 3D unit-vector in Percent.
        /// This is the elevation angle from the World X-Y plane (not from the X-axis).
        /// 100% = 45 degrees.
        /// Returns positive (or negative) infinity if line is vertical.
        static member inline slopePercent (v:UnitVec) : float =
            //if isTooTiny (abs(v.Z)) then EuclidDivByZeroException.Raise "Euclid.UnitVec.slopePercent: Can't get Slope from vertical unit-vector %O" v
            let len2D = sqrt(v.X*v.X + v.Y*v.Y)
            100.0 * v.Z / len2D

        /// Reverse vector if Z part is smaller than 0.0
        static member inline orientUp (v:UnitVec) : UnitVec =
            if v.Z < 0.0 then -v else v

        /// Reverse vector if Z part is bigger than 0.0
        static member inline orientDown (v:UnitVec) : UnitVec =
            if v.Z < 0.0 then v else -v

        /// Returns a vector that is perpendicular to the given vector and in the same vertical Plane.
        /// Projected into the X-Y plane input and output vectors are parallel and of same orientation.
        /// Not of same length, not unitized.
        /// On vertical input vector resulting vector is of zero length.
        static member inline perpendicularInVerticalPlane (v:UnitVec) :Vec =
            let hor = Vec(v.Y, -v.X, 0.0)
            let r = UnitVec.cross (v , hor)
            if v.Z < 0.0 then -r else r

        /// Checks if the angle between two unit-vectors is smaller than a threshold angle specified as a precomputed cosine value.
        /// Ignores vector orientation. So the angle is between 0 to 90 degrees ignoring their orientation.
        /// Use the Euclid.Cosine module to get some precomputed cosine values.
        /// Use UnitVec.isAngleBelow for considering vector orientation.
        static member inline isParallelWithin (cosineValue: float<Cosine.cosine>) (a:UnitVec) (b:UnitVec) : bool =
            abs(a *** b) > float cosineValue

        /// Checks if the angle between two unit-vectors is bigger than a threshold angle specified as a precomputed cosine value.
        /// Ignores vector orientation. So the angle is between 0 to 90 degrees ignoring their orientation.
        /// Use the Euclid.Cosine module to get some precomputed cosine values.
        /// Use UnitVec.isAngleAbove for considering vector orientation.
        static member inline isNotParallelWithin (cosineValue: float<Cosine.cosine>) (a:UnitVec) (b:UnitVec) : bool =
            abs(a *** b) < float cosineValue

        /// Checks if the angle between two unit-vectors is smaller than a threshold angle specified as a precomputed cosine value.
        /// Considers the vector orientation too. So the angle is between 0 to 180 degrees.
        /// Use the Euclid.Cosine module to get some precomputed cosine values.
        /// Use UnitVec.isParallelWithin to ignore vector orientation.
        static member inline isAngleBelow (cosineValue: float<Cosine.cosine>) (a:UnitVec) (b:UnitVec) : bool =
            a *** b > float cosineValue

        /// Checks if the angle between two unit-vectors is bigger than a threshold angle specified as a precomputed cosine value.
        /// Considers the vector orientation too. So the angle is between 0 to 180 degrees.
        /// Use the Euclid.Cosine module to get some precomputed cosine values.
        /// Use UnitVec.isNotParallelWithin to ignore vector orientation.
        static member inline isAngleAbove (cosineValue: float<Cosine.cosine>) (a:UnitVec) (b:UnitVec) : bool =
            a *** b < float cosineValue

        /// Linearly interpolates between two vectors.
        /// e.g. rel=0.5 will return the middle vector, rel=1.0 the end vector,
        /// rel=1.5 a vector half the distance beyond the end vector.
        static member lerp (start:UnitVec, endValue:UnitVec, rel:float) : Vec =
            Vec(
                start.X + rel*(endValue.X - start.X),
                start.Y + rel*(endValue.Y - start.Y),
                start.Z + rel*(endValue.Z - start.Z) )

        /// Spherically interpolates between start and end by amount rel (0.0 to 1.0).
        /// The difference between this and linear interpolation (aka, "lerp") is that the vectors are treated as directions rather than points in space.
        /// The direction of the returned vector is interpolated by the angle and its magnitude is interpolated between the magnitudes of start and end.
        /// Interpolation continues before and after the range of 0.0 and 1.0.
        static member slerp (start:UnitVec, endValue:UnitVec, rel:float) :UnitVec =
            // https://en.wikipedia.org/wiki/Slerp
            // implementation tested in Rhino!
            let dot = start *** endValue
            if dot > float Cosine.``0.05`` then  // vectors are in the same direction interpolate length only
                start
            elif dot < float Cosine.``179.95`` then
                fail2 "UnitVec.slerp vectors are 180 deg opposite." start endValue |> unbox // unbox to make type checker happy
            else
                let ang = acos(dot) // the angle between the two vectors
                let p = endValue - start*dot  // a vector perpendicular to start and in the same plane with endValue.
                let perp = UnitVec.create(p.X, p.Y, p.Z)
                let theta = ang*rel // the angle part we want for the result
                let theta360 = (theta+UtilEuclid.twoPi) % UtilEuclid.twoPi // make sure it is in the range 0.0 to 2 Pi (360 degrees)
                let cosine = cos (theta360)
                let sine   = sqrt(1.0 - cosine*cosine)
                let vx = start * cosine
                let vy = perp * sine
                if theta360 < Math.PI then  // in the range 0 to 180 degrees, only applicable if rel is beyond 0.0 or 0.1
                    let v = vx + vy
                    UnitVec.createUnchecked (v.X, v.Y, v.Z)
                else
                    let v = vx - vy
                    UnitVec.createUnchecked (v.X, v.Y, v.Z)

        // #endregion
        // #region Obsolete

        [<Obsolete("Use Euclid.XLine3D module instead.")>]
        static member inline intersection (ptA:Pnt) (ptB:Pnt) (vA:UnitVec) (vB:UnitVec) : Option<float*float> =
            Some (XLineXYZ.parameters(ptA.X, ptA.Y, ptA.Z, ptB.X, ptB.Y, ptB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z))

        [<Obsolete("Use UnitVec.isParallelWithin instead.")>]
        static member inline isAngle90Below (cosineValue: float<Cosine.cosine>) (a:UnitVec) (b:UnitVec) : bool =
            UnitVec.isParallelWithin cosineValue a b

        [<Obsolete("Use UnitVec.isNotParallelWithin instead.")>]
        static member inline isAngle90Above (cosineValue: float<Cosine.cosine>) (a:UnitVec) (b:UnitVec) : bool =
            UnitVec.isNotParallelWithin cosineValue a b

        [<Obsolete("Use UnitVec.isAngleBelow instead.")>]
        static member inline isAngle180Below (cosineValue: float<Cosine.cosine>) (a:UnitVec) (b:UnitVec) : bool =
            UnitVec.isAngleBelow cosineValue a b

        [<Obsolete("Use UnitVec.isAngleAbove instead.")>]
        static member inline isAngle180Above (cosineValue: float<Cosine.cosine>) (a:UnitVec) (b:UnitVec) : bool =
            UnitVec.isAngleAbove cosineValue a b

        [<Obsolete("Use UnitVec.isParallelTo instead. Obsolete since 0.21.0")>]
        static member inline areParallel (other:UnitVec) (v:UnitVec) : bool =
            UnitVec.isParallelTo Cosine.``0.25`` v other

        [<Obsolete("Use UnitVec.isParallelAndOrientedTo instead. Obsolete since 0.21.0")>]
        static member inline areParallelAndMatchOrientation (other:UnitVec) (v:UnitVec) : bool =
            UnitVec.isParallelAndOrientedTo Cosine.``0.25`` v other

        [<Obsolete("Use UnitVec.isPerpendicularTo instead. Obsolete since 0.21.0")>]
        static member inline arePerpendicular(other:UnitVec) (v:UnitVec) : bool =
            UnitVec.isPerpendicularTo Cosine.``89.75`` v other

        #nowarn "44" //obsolete

        /// Multiplies a Matrix with a 3D vector.
        /// Since a 3D vector represents a direction or translation in space, but not a location,
        /// the implicit the 4th dimension is 0.0 so that all translations are ignored. (Homogeneous Vector)
        /// The resulting vector is not unitized.
        [<Obsolete("If the matrix is projecting this transformation will not be correct. Use RigidMatrix or with a Pnt instead.")>]
        member inline v.Transform (m:Matrix) : Vec =
            v *** m // operator * is defined in Matrix.fs

        /// Multiplies a Matrix with a 3D vector.
        /// Since a 3D vector represents a direction or translation in space, but not a location,
        /// the implicit the 4th dimension is 0.0 so that all translations are ignored. (Homogeneous Vector)
        /// The resulting vector is not unitized.
        [<Obsolete("If the matrix is projecting this transformation will not be correct. Use RigidMatrix or with a Pnt instead.")>]
        static member inline transform (m:Matrix) (v:UnitVec) : Vec =
            v.Transform(m)

        /// Rotate by Quaternion.
        [<Obsolete("Use UnitVec.rotate instead.")>]
        static member inline rotateByQuaternion (q:Quaternion) (v:UnitVec) : UnitVec =
            v *** q  // operator * is defined in Quaternion.fs

        /// Obsolete, use UnitVec.addX instead. A vector has no location; this is a vector addition.
        [<Obsolete("Use UnitVec.addX instead.")>]
        static member inline moveX x (v:UnitVec) : Vec =
            UnitVec.addX x v

        /// Obsolete, use UnitVec.addY instead. A vector has no location; this is a vector addition.
        [<Obsolete("Use UnitVec.addY instead.")>]
        static member inline moveY y (v:UnitVec) : Vec =
            UnitVec.addY y v

        /// Obsolete, use UnitVec.addZ instead. A vector has no location; this is a vector addition.
        [<Obsolete("Use UnitVec.addZ instead.")>]
        static member inline moveZ z (v:UnitVec) : Vec =
            UnitVec.addZ z v



