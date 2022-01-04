namespace FsEx.Geo

open System

/// When FsEx.Geo is opened this module will be auto-opened.
/// It only contains extension members for type UnitVc.
[<AutoOpen>]
module AutoOpenUnitVc =
    open Util

    /// Returns distance between the tips of two 2D vectors.
    let inline internal vecDist2(ax:float,ay:float,bx:float,by:float) =
        let x = bx-ax
        let y = by-ay
        sqrt(x*x+y*y)

    type UnitVc with

        /// Returns new 2D vector with new X coordinate, Y stays the same.
        member inline v.WithX x = Vc (x ,v.Y)

        /// Returns new 2D vector with new Y coordinate, X stays the same.
        member inline v.WithY y = Vc (v.X, y)

        /// Returns new 3D vector with Z coordinate, X and Y stay the same.
        /// If you want Z to be 0.0 you can use v.AsVec or v.AsUnitVec too.
        member inline v.WithZ z = Vec (v.X, v.Y, z)

        /// 2D cross product.
        /// Its Just a scalar equal to the signed area of the parallelogram spanned by the input vectors.
        member inline a.Cross (b:Vc)     = a.X*b.Y - a.Y*b.X

        /// 2D cross product.
        /// Its Just a scalar equal to the signed area of the parallelogram spanned by the input vectors.
        member inline a.Cross (b:UnitVc) = a.X*b.Y - a.Y*b.X

        /// 90 Degree rotation counter clockwise.
        member inline v.Rotate90CCW = UnitVc.createUnchecked( -v.Y,   v.X  )

        /// 90 Degree rotation clockwise.
        member inline v.Rotate90CW  = UnitVc.createUnchecked(  v.Y,  -v.X  )

        /// The diamond angle.
        /// Calculates the proportion of X to Y component.
        /// It is always positive and in the range of 0.0 to 4.0 ( for 360 Degrees)
        /// 0.0 = Xaxis,  going Counter clockwise.
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        member inline v.DirectionDiamond =
            // https://stackoverflow.com/a/14675998/969070
            #if DEBUG
            if abs(v.X) < zeroLengthTol && abs(v.Y) < zeroLengthTol then
                FsExGeoDivByZeroException.Raise "FsEx.Geo.UnitVc.DirectionDiamondInXY: input vector is zero length:%O" v
            #endif
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

        /// Returns the Angle in Radians from Xaxis,
        /// Going Counter clockwise till two Pi.
        member inline v.Direction2Pi =
            // https://stackoverflow.com/a/14675998/969070
            #if DEBUG
            if abs(v.X) < zeroLengthTol && abs(v.Y) < zeroLengthTol then
                FsExGeoDivByZeroException.Raise "FsEx.Geo.UnitVc.Direction2Pi: input vector is zero length: %O" v
            #endif
            let a = Math.Atan2(v.Y, v.X)
            if a < 0. then
                a + Util.twoPi
            else
                a

        /// Returns the Angle in Radians from Xaxis,
        /// Ignores orientation.
        /// Range 0.0 to Pi.
        member inline v.DirectionPi =
            // https://stackoverflow.com/a/14675998/969070
            #if DEBUG
            if abs(v.X) < zeroLengthTol && abs(v.Y) < zeroLengthTol then
                FsExGeoDivByZeroException.Raise "FsEx.Geo.UnitVc.DirectionPi: input vector is zero length: %O" v
            #endif
            let a = Math.Atan2(v.Y, v.X)
            if a < 0. then
                a + Math.PI
            else
                a

        /// Returns the Angle in Degrees from Xaxis.
        /// Going Counter clockwise till 360.
        member inline v.Direction360 =
            v.Direction2Pi |> toDegrees

        /// Returns the Angle in Radians from Xaxis,
        /// Ignores orientation.
        /// Range 0.0 to 180.
        member inline v.Direction180 =
            v.DirectionPi |> toDegrees

        /// Returns positive angle for rotating counter clockwise from this vector to vector 'b' .
        /// In Diamond Angle. Using only proportion of X to Y components.
        /// Range of 0.0 to 4.0 ( for 360 Degrees)
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        member inline v.AngleDiamondTo (b:UnitVc)   =
            let r = b.DirectionDiamond - v.DirectionDiamond
            if r >= 0. then  r
            else r + 4.0

        /// Convert 2D unit-vector to 2D point.
        member inline v.AsPt         = Pt( v.X, v.Y)

        /// Convert 2D unit-vector to 3D vector using 0.0 as Z value.
        /// If you want a different Z value use the member v.WithZ(z)
        member inline v.AsVec        = Vec(v.X, v.Y, 0.0)

        /// Convert 2D unit-vector to 3D unit-vector using 0.0 as Z value.
        /// If you want a different Z value use the member v.WithZ(z)
        member inline v.AsUnitVec    = UnitVec.createUnchecked(v.X, v.Y, 0.0)

        /// Convert 2D unit-vector to 3D point using 0.0 as Z value.
        member inline v.AsPnt        = Pnt(v.X, v.Y, 0.0)

        /// Checks if the angle between the two 2D unit vectors is less than 180 degrees.
        /// Calculates the dot product of two 2D unit vectors.
        /// Then checks if it is positive.
        member inline v.MatchesOrientation180  (other:UnitVc) =
            v * other > 0.0

        /// Checks if the angle between the two 2D unit vectors is less than 90 degrees.
        /// Calculates the dot product of the two 2D unit vectors .
        /// Then checks if it is bigger than 0.707107 (cosine of 90 degrees).
        member inline v.MatchesOrientation90  (other:UnitVc) =
            v* other > 0.707107

        /// Checks if two 3D unit vectors are parallel.
        /// Ignores the line orientation.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minium cosine value.
        /// See FsEx.Geo.Cosine module.
        member inline a.IsParallelTo( b:UnitVc, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
            abs(b*a) > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:


        /// Checks if two 3D unit vectors are parallel.
        /// Takes the line orientation into account too.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minium cosine value.
        /// See FsEx.Geo.Cosine module.
        member inline a.IsParallelAndOrientedTo  (b:UnitVc, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
            b*a > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:


        /// Checks if two 3D unit vectors are perpendicular to each other.
        /// The default angle tolerance is 89.75 to 90.25 degrees.
        /// This tolerance can be customized by an optional minium cosine value.
        /// The default cosine is 0.0043633 ( = 89.75 deg )
        /// See FsEx.Geo.Cosine module.
        member inline a.IsPerpendicularTo (b:UnitVc, [<OPT;DEF(Cosine.``89.75``)>] maxCosine:float<Cosine.cosine> ) =
            let d = b*a
            float -maxCosine < d && d  < float maxCosine // = cosine of 98.75 and 90.25 degrees

        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------

        /// Create 2D unit-vector from start and endpoint. Does the unitizing too.
        static member inline create (fromPnt:Pt, toPnt:Pt) =
            let x = toPnt.X - fromPnt.X
            let y = toPnt.Y - fromPnt.Y
            UnitVc.create( x , y  )

        /// Returns the World X-axis with length one: UnitVc(1,0)
        static member inline Xaxis  = UnitVc.createUnchecked (1.0 , 0.0)

        /// Returns the World Y-axis with length one: UnitVc(0,1)
        static member inline Yaxis  = UnitVc.createUnchecked (0.0 , 1.0)

        /// Returns the distance between the tips of two 2D unit vectors.
        static member inline difference (a:UnitVc) (b:UnitVc) = let v = a-b in sqrt(v.X*v.X + v.Y*v.Y )

        /// Returns the squared distance between the tips of two 2D unit vectors.
        /// This operation is slightly faster than Vec.difference and sufficient for many algorithms like finding closest points.
        static member inline differenceSq (a:UnitVc) (b:UnitVc) = let v = a-b in  v.X*v.X + v.Y*v.Y

        // These members cannot be implemented since
        // Array.sum and Array.average of UnitVc would return a 'Vc' and not a 'UnitVc'
        // static member Zero = UnitVc ( 0. , 0.)  // needed by 'Array.sum'
        // static member inline DivideByInt (v:UnitVc, i:int) = v / float i  // needed by  'Array.average'

        /// Accepts any type that has a X and Y (UPPERCASE) member that can be converted to a float.
        /// Does the unitizing too.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline ofXY vec  =
            let x = ( ^T : (member X : _) vec)
            let y = ( ^T : (member Y : _) vec)
            try UnitVc.create(float x, float y)
            with e -> FsExGeoException.Raise "FsEx.Geo.UnitVc.ofXY: %A could not be converted to a FsEx.Geo.UnitVc:\r\n%A" vec e

        /// Accepts any type that has a x and y (lowercase) member that can be converted to a float.
        /// Does the unitizing too.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline ofxy vec  =
            let x = ( ^T : (member x : _) vec)
            let y = ( ^T : (member y : _) vec)
            try UnitVc.create(float x, float y)
            with e -> FsExGeoException.Raise "FsEx.Geo.UnitVc.ofxy: %A could not be converted to a FsEx.Geo.UnitVc:\r\n%A" vec e

        /// Create 2D unit-vector from 2D point. Does the unitizing too.
        static member inline ofPt  (pt:Pt) =
            let l = sqrt (pt.X*pt.X + pt.Y*pt.Y )
            if l <  zeroLengthTol then FsExGeoDivByZeroException.Raise "FsEx.Geo.UnitVc.ofPt failed on too short %O" pt
            UnitVc.createUnchecked( pt.X / l , pt.Y / l )

        /// Create 2D unit-vector from 2D vector. Does the unitizing too.
        static member inline ofVec  (v:Vc) =
            let l = sqrt (v.X*v.X + v.Y*v.Y )
            if l <  zeroLengthTol then FsExGeoDivByZeroException.Raise "FsEx.Geo.UnitVc.ofVc failed on too short %O" v
            UnitVc.createUnchecked( v.X / l , v.Y / l )


        /// Convert 2D unit-vector to 2D point.
        static member inline asPt(v:UnitVc)  = Pt( v.X, v.Y)

        /// Convert 2D unit-vector to 3D vector using 0.0 as Z value.
        /// If you want a different Z value use the member w.WithZ(z)
        static member inline asVec(v:UnitVc) = Vec(v.X, v.Y, 0.0)

        /// Convert 2D unit-vector to 3D unit-vector using 0.0 as Z value.
        static member inline asUnitVec(v:UnitVc) = UnitVec.createUnchecked(v.X, v.Y, 0.0)

        /// Convert 2D unit-vector to 3D point using 0.0 as Z value.
        static member inline asPnt(v:UnitVc) = Pnt(v.X, v.Y, 0.0)

        /// 2D cross product.
        /// Its Just a scalar equal to the area of the parallelogram spanned by the input vectors.
        static member inline cross (a:UnitVc, b:UnitVc)      = a.X*b.Y - a.Y*b.X

        /// 2D cross product.
        /// Its Just a scalar equal to the area of the parallelogram spanned by the input vectors.
        static member inline cross (a:UnitVc, b:Vc)  = a.X*b.Y - a.Y*b.X

        /// 2D cross product.
        /// Its Just a scalar equal to the area of the parallelogram spanned by the input vectors.
        static member inline cross (a:Vc, b:UnitVc)  = a.X*b.Y - a.Y*b.X

        /// Dot product, or scalar product of two 2D unit vectors.
        /// Returns a float. This float is the Cosine of the angle between the two 2D unit vectors.
        static member inline dot  (a:UnitVc, b:UnitVc  ) = a.X * b.X + a.Y * b.Y

        /// Dot product, or scalar product of a 2D unit-vector with a 2D vector.
        /// Returns a float. This float is the projected length of the 2D vector on the direction of the unit-vector.
        static member inline dot  (a:UnitVc, b:Vc  ) = a.X * b.X + a.Y * b.Y

        /// Dot product, or scalar product of a 2D vector with a 2D unit-vector.
        /// Returns a float. This float is the projected length of the 2D vector on the direction of the unit-vector.
        static member inline dot  (a:Vc, b:UnitVc  ) = a.X * b.X + a.Y * b.Y

        /// Gets the X part of this 2D unit-vector.
        static member inline getX     (v:UnitVc) = v.X

        /// Gets the Y part of this 2D unit-vector.
        static member inline getY     (v:UnitVc) = v.Y

        /// Returns new 2D vector with new X value, Y stays the same.
        static member inline setX     x (v:UnitVc) = v.WithX x

        /// Returns new 2D vector with new Y value, X stays the same.
        static member inline setY     y (v:UnitVc) = v.WithY y

        /// Returns new 3D vector with Z value, X and Y stay the same.
        /// If you want Z to be 0.0 you can use v.AsVec or v.AsUnitVec too.
        static member inline setZ     z (v:UnitVc) = v.WithZ z

        /// Add two 2D unit vectors together. Returns a new (non-unitized) 2D vector.
        static member inline add      (a:UnitVc) (b:UnitVc) = b + a

        /// Multiplies a 2D unit-vector with a scalar, also called scaling a vector.
        /// Same as UnitVc.setLength. Returns a new (non-unitized) 2D vector.
        static member inline scale      (scale:float) (v:UnitVc) = Vc (v.X * scale , v.Y * scale )

        /// Multiplies a 2D unit-vector with a scalar, also called scaling a vector.
        /// Same as UnitVc.setLength. Returns a new (non-unitized) 2D vector.
        static member inline setLength  (length:float) (v:UnitVc) = Vc (v.X * length , v.Y * length )

        /// Add to the X part of this 2D unit vectors together. Returns a new (non-unitized) 2D vector.
        static member inline moveX     x (v:UnitVc) = Vc (v.X+x, v.Y)

        /// Add to the Y part of this 2D unit vectors together. Returns a new (non-unitized) 2D vector.
        static member inline moveY     y (v:UnitVc) = Vc (v.X,   v.Y+y)

        /// Negate or inverse a 2D unit vectors. Returns a new 2D unit-vector.
        /// Same as UnitVc.reverse.
        static member inline flip  (v:UnitVc) = -v

        /// Negate or inverse a 2D unit vectors. Returns a new 2D unit-vector.
        /// Same as UnitVc.flip.
        static member inline reverse  (v:UnitVc) = -v

        /// Returns angle between two 2D unit vectors in Radians.
        /// Takes vector orientation into account.
        /// Ignores order of input vectors. anglePi(a,b) = anglePi(b,a)
        /// Range 0.0 to Pi( = 0 to 180 Degree)
        static member inline anglePi (a:UnitVc) (b:UnitVc) =
            // The "straight forward" method of acos(u.v) has large precision
            // issues when the dot product is near +/-1.  This is due to the
            // steep slope of the acos function as we approach +/- 1.  Slight
            // precision errors in the dot product calculation cause large
            // variation in the output value.
            // To avoid this we use an alternative method which finds the
            // angle bisector by (u-v)/2
            // Because u and v and unit vectors, (u-v)/2 forms a right angle
            // with the angle bisector.  The hypotenuse is 1, therefore
            // 2*asin(|u-v|/2) gives us the angle between u and v.
            // The largest possible value of |u-v| occurs with perpendicular
            // vectors and is sqrt(2)/2 which is well away from extreme slope
            // at +/-1. (See Windows OS Bug 01706299 for details) (form WPF reference source code)
            let dot = a * b
            if -0.98 < dot && dot < 0.98 then // threshold for switching 0.98 ?
                acos dot
            else
                if dot < 0. then Math.PI - 2.0 * asin(vecDist2(-a.X,-a.Y,b.X,b.Y) * 0.5)
                else                       2.0 * asin(vecDist2( a.X, a.Y,b.X,b.Y) * 0.5)
            //let mutable r = b.Direction2Pi  - a.Direction2Pi  // this alternative calculation is about 10x slower for unit vectors
            //if r < 0.      then  r <- r + Util.twoPi
            //if r > Math.PI then  r <- Util.twoPi - r
            //r


        /// Returns positive angle between two 2D unit vectors in Radians.
        /// Ignores orientation.
        /// Ignores order of input vectors. angleHalfPi(a,b) = angleHalfPi(b,a) = angleHalfPi(-b,a) = angleHalfPi(-b,-a)
        /// Range 0.0 to Pi/2 ( = 0 to 90 Degree)
        static member inline angleHalfPi (a:UnitVc) (b:UnitVc) =
            let dot =  a * b
            let dotAbs = abs dot
            if dotAbs < 0.98 then
                acos dotAbs
            else
                if dot < 0. then 2.0 * asin(vecDist2(-a.X,-a.Y,b.X,b.Y) * 0.5)
                else             2.0 * asin(vecDist2( a.X, a.Y,b.X,b.Y) * 0.5)
            //let mutable r = b.Direction2Pi  - a.Direction2Pi // this alternative calculation is about 10x slower for unit vectors
            //if r < 0.      then  r <- r + Util.twoPi
            //if r > Math.PI then  r <- Util.twoPi - r
            //if r > halfPi  then  r <- Math.PI - r
            //r


        /// Returns positive angle for rotating counter clockwise from vector 'a' to vector 'b' .
        /// In Radians.
        /// Range: 0.0 to 2 Pi ( = 0 to 360 Degrees)
        static member inline angle2Pi (a:UnitVc , b:UnitVc)   =
            let r = b.Direction2Pi  - a.Direction2Pi
            if r >= 0. then  r
            else r + Util.twoPi

        /// Returns positive angle between two 2D unit vectors in Degrees,
        /// Ignores vector orientation.
        /// Ignores order of input vectors. angle90(a,b) = angle90(b,a) = angle90(-b,a) = angle90(-b,-a)
        /// Range: 0 to 90 Degrees.
        static member inline angle90 (a:UnitVc) (b:UnitVc) =
            UnitVc.angleHalfPi a b |>  toDegrees


        /// Returns positive angle between two 2D unit vectors in Degrees.
        /// Takes vector orientation into account.
        /// Ignores order of input vectors. angle180(a,b) = angle180(b,a)
        /// Range 0 to 180 Degrees.
        static member inline angle180 (a:UnitVc) (b:UnitVc) =
            UnitVc.anglePi a b |>  toDegrees


        /// Returns positive angle for rotating counter clockwise from vector 'a' to vector 'b' .
        /// In Degree.
        /// Range: 0.0 to 2 Pi ( = 0 to 360 Degrees)
        static member inline angle360 (a:UnitVc, b:UnitVc)  =
            UnitVc.angle2Pi (a,b) |> toDegrees


        /// Returns positive angle for rotating counter clockwise from vector 'a' to vector 'b' .
        /// In Diamond Angle. Using only proportion of X to Y components.
        /// Range of 0.0 to 4.0 ( for 360 Degrees)
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        static member inline angleDiamond (a:UnitVc , b:UnitVc)   = a.AngleDiamondTo(b)


        /// The diamond angle.
        /// Calculates the proportion of X to Y component.
        /// It is always positive and in the range of 0.0 to 4.0 ( for 360 Degrees)
        /// 0.0 = Xaxis,  going Counter clockwise.
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        static member inline directionDiamond(a:UnitVc) = a.DirectionDiamond

        /// Returns positive angle of unit-vector. Counter clockwise from X-axis.
        /// In Radians.
        /// Range: 0.0 to 2 Pi ( = 0 to 360 Degrees)
        static member inline direction2Pi (v:UnitVc)   = v.Direction2Pi

        /// Returns positive angle of unit-vector. Counter clockwise from X-axis.
        /// In Degree.
        /// Range: 0.0 to 2 Pi ( = 0 to 360 Degrees)
        static member inline direction360 (v:UnitVc)  = v.Direction360

        /// Ensure vector has a positive dot product with given orientation vector.
        static member inline matchOrientation (orientationToMatch:UnitVc) (v:UnitVc) =
            if orientationToMatch * v < 0.0 then -v else v

        /// Checks if the angle between the two 2D unit vectors is less than 180 degrees.
        /// Calculates the dot product of two 2D unit vectors.
        /// Then checks if it is positive.
        static member inline matchesOrientation180  (v:UnitVc) (other:UnitVc) =
            v.MatchesOrientation180(other)

        /// Checks if the angle between the two 2D unit vectors is less than 90 degrees.
        /// Calculates the dot product of the two 2D unit vectors unitized.
        /// Then checks if it is bigger than 0.707107 (cosine of 90 degrees).
        static member inline matchesOrientation90  (v:UnitVc)  (other:UnitVc) =
            v.MatchesOrientation90(other)


        /// Checks if Angle between two vectors is Below 0.25 Degree.
        /// Ignores vector orientation.
        /// Same as isAngleBelowQuatreDegree.
        static member inline isParallelTo (other:UnitVc) (v:UnitVc) =   v.IsParallelTo other


        /// Checks if Angle between two vectors is between 98.75 and 90.25 Degree.
        /// Ignores vector orientation.
        static member inline isParallelAndOrientedTo (other:UnitVc) (v:UnitVc) = v.IsParallelAndOrientedTo other

        /// Checks if Angle between two vectors is between 98.75 and 90.25 Degree.
        /// Ignores vector orientation.
        static member inline isPerpendicularTo (other:UnitVc) (v:UnitVc) =  v.IsPerpendicularTo other


        /// Rotate the a 2D UnitVector Counter Clockwise by a 2D Rotation (that has cos and sin precomputed)
        static member inline rotateBy (r:Rotation2D) (v:UnitVc) =
            UnitVc.createUnchecked (
                r.Cos*v.X - r.Sin*v.Y,
                r.Sin*v.X + r.Cos*v.Y)

        /// Rotate the 2D UnitVector in Degrees. Counter Clockwise.
        /// For better Performance recompute the Rotate2D struct and use its member to rotate. see UnitVc.rotateBy.
        static member inline rotate (angDegree) (vec:UnitVc) =
            UnitVc.rotateBy (Rotation2D.createFromDegrees angDegree) vec

        /// 90 Degree rotation counter clockwise.
        static member inline rotate90CCW (v:UnitVc) = UnitVc.createUnchecked( -v.Y,   v.X  )

        /// 90 Degree rotation clockwise.
        static member inline rotate90CW (v:UnitVc) = UnitVc.createUnchecked(  v.Y,  -v.X  )

        /// Checks if Angle between two vectors is Below one Degree.
        /// Ignores vector orientation.
        /// Use Vec.isParallelTo for custom tolerance.
        static member isAngleBelow1Degree(a:UnitVc, b:UnitVc) =
            abs(b*a) > float Cosine.``1.0``


        /// Checks if Angle between two vectors is Below 0.25 Degree.
        /// Ignores vector orientation.
        /// Use Vec.isParallelTo for custom tolerance.
        static member isAngleBelowQuatreDegree(a:UnitVc, b:UnitVc) =
            abs(b*a) > float Cosine.``0.25``


        /// Checks if Angle between two vectors is Below 5 Degrees.
        /// Ignores vector orientation.
        /// Use Vec.isParallelTo for custom tolerance.
        static member isAngleBelow5Degree(a:UnitVc, b:UnitVc) =
            abs(b*a) > float Cosine.``5.0``
