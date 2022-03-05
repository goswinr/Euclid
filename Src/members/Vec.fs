namespace FsEx.Geo
open System

/// When FsEx.Geo is opened this module will be auto-opened.
/// It only contains extension members for type Vec.
[<AutoOpen>]
module AutoOpenVec =
    open Util

    type Vec with

        /// Returns a boolean indicating wether X,Y and Z are all exactly 0.0.
        member inline v.IsZero =  v.X = 0.0 && v.Y = 0.0 && v.Z = 0.0

        /// Returns a boolean indicating if any of X,Y and Z is not exactly 0.0.
        member inline v.IsNotZero =  v.X <> 0.0 || v.Y <> 0.0 || v.Z <> 0.0

        /// Check if the 3D vector is shorter than the tolerance.
        member inline v.IsTiny tol =
            v.Length < tol

        /// Check if the 3D vector is shorter than the squared tolerance.
        member inline v.IsTinySq tol =
            v.LengthSq < tol

        //member inline v.Length moved to Vec type declaration
        //member inline v.LengthSq moved to Vec type declaration

        /// Returns the length of the 3D vector projected into World X-Y plane.
        member inline v.LengthInXY =  sqrt (v.X*v.X + v.Y*v.Y)

        /// Returns the squared length of the 3D vector projected into World X-Y plane.
        /// The square length is faster to calculate and often good enough for use cases such as sorting vectors by length.
        member inline v.LengthSqInXY = v.X*v.X + v.Y*v.Y

        /// Returns  a new 3D vector with new X coordinate, Y and Z stay the same.
        member inline v.WithX x = Vec (x ,v.Y, v.Z)

        /// Returns a new 3D vector with new y coordinate, X and Z stay the same.
        member inline v.WithY y = Vec (v.X, y, v.Z)

        /// Returns a new 3D vector with new z coordinate, X and Y stay the same.
        member inline v.WithZ z = Vec (v.X ,v.Y, z)

        /// Returns a new 3D vector with half the length.
        member inline v.Half = Vec (v.X*0.5 ,v.Y*0.5, v.Z*0.5)

        /// Returns a new 3D vector scaled to the desired length.
        /// Same as Vec.setLength.
        member inline v.WithLength (desiredLength:float) =
            let l = v.Length
            if l < zeroLengthTol then
                FsExGeoDivByZeroException.Raise "FsEx.Geo.Vec.WithLength %g : %O is too small for unitizing, Tolerance:%g" desiredLength v zeroLengthTol
            v * (desiredLength / l)

        /// Returns the 3D vector unitized.
        /// Fails with FsExGeoDivByZeroException if the length of the vector is
        /// too small (1e-16) to unitize.
        member inline v.Unitized =
            let l = v.Length
            if l < zeroLengthTol then
                FsExGeoDivByZeroException.Raise "FsEx.Geo.Vec.Unitized: %O is too small for unitizing, Tolerance:%g" v zeroLengthTol
            let li = 1. / l
            UnitVec.createUnchecked( li*v.X , li*v.Y ,li*v.Z )

        // Returns the 3D vector unitized.
        // If the length of the vector is 0.0 an invalid unit-vector is returned.
        // UnitVec(0,0,0)
        //member inline v.UnitizedUnchecked =
        //    let li = 1. / sqrt(v.X*v.X + v.Y*v.Y + v.Z*v.Z)
        //    UnitVec.createUnchecked( li*v.X , li*v.Y ,li*v.Z )

        /// Test if the 3D vector is a unit-vector.
        /// Test if the vectors square length is within 6 float steps of 1.0
        /// So between 0.99999964 and 1.000000715.
        member inline v.IsUnit   =
            Util.isOne v.LengthSq

        /// Returns a perpendicular horizontal vector. Rotated counterclockwise.
        /// Or Vec.Zero if input is vertical.
        /// Just does Vec(-v.Y, v.X, 0.0)
        member inline v.PerpendicularInXY = Vec(-v.Y, v.X, 0)

        /// 90 Degree rotation counter clockwise around Z-axis.
        member inline v.RotateOnZ90CCW = Vec( -v.Y,   v.X ,   v.Z)

        /// 90 Degree rotation clockwise around Z-axis.
        member inline v.RotateOnZ90CW  = Vec(  v.Y,  -v.X,   v.Z  )


        /// The diamond angle.
        /// Calculates the proportion of X to Y component.
        /// It is always positive and in the range of 0.0 to 4.0 ( for 360 Degrees)
        /// 0.0 = Xaxis,  going Counter clockwise.
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.DirectionDiamondInXY =
            // https://stackoverflow.com/a/14675998/969070
            #if DEBUG
            if abs(v.X) < zeroLengthTol && abs(v.Y) < zeroLengthTol then
                FsExGeoDivByZeroException.Raise "FsEx.Geo.Vec.DirectionDiamondInXY: input vector is vertical or zero length:%O" v
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
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.Direction2PiInXY =
            // https://stackoverflow.com/a/14675998/969070
            #if DEBUG
            if abs(v.X) < zeroLengthTol && abs(v.Y) < zeroLengthTol then // TODO : with this test all  operations are 2.5 times slower
                FsExGeoDivByZeroException.Raise "FsEx.Geo.Vec.Direction2PiInXY: input vector is zero length: %O" v
            #endif
            let a = Math.Atan2(v.Y, v.X)
            if a < 0. then
                a + Util.twoPi
            else
                a

        /// Returns the Angle in Radians from Xaxis,
        /// Ignores orientation.
        /// Range 0.0 to Pi.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.DirectionPiInXY =
            // https://stackoverflow.com/a/14675998/969070
            #if DEBUG
            if abs(v.X) < zeroLengthTol && abs(v.Y) < zeroLengthTol then // TODO : with this test all  operations are 2.5 times slower
                FsExGeoDivByZeroException.Raise "FsEx.Geo.Vec.DirectionPiInXY: input vector is zero length: %O" v
            #endif
            let a = Math.Atan2(v.Y, v.X)
            if a < 0. then
                a + Math.PI
            else
                a

        /// Returns the Angle in Degrees from Xaxis.
        /// Going Counter clockwise till 360.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.Direction360InXY =
            v.Direction2PiInXY |> toDegrees

        /// Returns the Angle in Radians from Xaxis,
        /// Ignores orientation.
        /// Range 0.0 to 180.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.Direction180InXY =
            v.DirectionPiInXY |> toDegrees

        /// Returns positive angle for rotating counter clockwise from this vector to vector 'b' .
        /// In Diamond Angle. Using only proportion of X to Y components.
        /// Range of 0.0 to 4.0 ( for 360 Degrees)
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.AngleDiamondInXYTo (b:Vec)   =
            let r = b.DirectionDiamondInXY - v.DirectionDiamondInXY
            if r >= 0. then  r
            else r + 4.0

        /// Convert 3D vector to 3D point.
        member inline v.AsPnt = Pnt(v.X, v.Y, v.Z)

        /// Convert 3D vector to 2D vector, discarding the Z value.
        member inline v.AsVc  = Vc(v.X, v.Y)


        /// Checks if the angle between the two 3D vectors is less than 180 degrees.
        /// Calculates the dot product of two 3D vectors.
        /// Then checks if it is positive.
        member inline v.MatchesOrientation180  (other:Vec) =
            v * other > 0.0

        /// Checks if the angle between the two 3D vectors is less than 90 degrees.
        /// Calculates the dot product of the two 3D vectors unitized.
        /// Then checks if it is bigger than 0.707107 (cosine of 90 degrees).
        member inline v.MatchesOrientation90  (other:Vec) =
            v.Unitized * other.Unitized > 0.707107



        /// Checks if two 3D vectors are parallel.
        /// Ignores the line orientation.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minium cosine value.
        /// See FsEx.Geo.Cosine module.
        /// Fails on vectors shorter than 1e-12.
        member inline a.IsParallelTo( b:Vec, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
            let sa = a.LengthSq
            if sa < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Vec.IsParallelTo: Vec 'ln' is too short: %s. 'other':%s " a.AsString b.AsString
            let sb = b.LengthSq
            if sb < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Vec.IsParallelTo: Vec 'other' is too short: %s. 'ln':%s " b.AsString a.AsString
            let au = a * (1.0 / sqrt sa )
            let bu = b * (1.0 / sqrt sb )
            abs(bu*au) > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:


        /// Checks if two 3D vectors are parallel.
        /// Takes the line orientation into account too.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minium cosine value.
        /// See FsEx.Geo.Cosine module.
        /// Fails on vectors shorter than 1e-12.
        member inline a.IsParallelAndOrientedTo  (b:Vec, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
            let sa = a.LengthSq
            if sa < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Vec.IsParallelAndOrientedTo: Vec 'ln' is too short: %s. 'other':%s " a.AsString b.AsString
            let sb = b.LengthSq
            if sb < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Vec.IsParallelAndOrientedTo: Vec 'other' is too short: %s. 'ln':%s " b.AsString a.AsString
            let au = a * (1.0 / sqrt sa )
            let bu = b * (1.0 / sqrt sb )
            bu*au > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:


        /// Checks if two 3D vectors are perpendicular to each other.
        /// The default angle tolerance is 89.75 to 90.25 degrees.
        /// This tolerance can be customized by an optional minium cosine value.
        /// The default cosine is 0.0043633 ( = 89.75 deg )
        /// See FsEx.Geo.Cosine module.
        /// Fails on vectors shorter than 1e-12.
        member inline a.IsPerpendicularTo (b:Vec, [<OPT;DEF(Cosine.``89.75``)>] maxCosine:float<Cosine.cosine> ) =
            let sa = a.LengthSq
            if sa < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Vec.IsPerpendicularTo: Vec 'ln' is too short: %s. 'other':%s " a.AsString b.AsString
            let sb = b.LengthSq
            if sb < 1e-24 then FsExGeoException.Raise "FsEx.Geo.Vec.IsPerpendicularTo: Vec 'other' is too short: %s. 'ln':%s " b.AsString a.AsString
            let au = a * (1.0 / sqrt sa )
            let bu = b * (1.0 / sqrt sb )
            let d = bu*au
            float -maxCosine < d && d  < float maxCosine // = cosine of 98.75 and 90.25 degrees

        /// Multiplies a Matrix with a 3D vector (with an implicit 1 in the 4th dimension,
        /// So that it also works correctly for projections.)
        member inline v.Transform (m:Matrix) =
            v*m // operator * is defined in Matrix.fs

        /// Multiplies (or applies) an OrthoMatrix to a 3D vector .
        member inline v.TransformOrtho (m:OrthoMatrix)  =
            v*m // operator * is defined in OrthoMatrix.fs

        /// Multiplies (or applies) only the 3x3 rotation part of an OrthoMatrix to a 3D vector .
        /// The resulting vector has the same length as the input.
        member inline v.RotateOrtho (m:OrthoMatrix) =
            let x = v.X
            let y = v.Y
            let z = v.Z
            Vec ( m.M11*x + m.M21*y + m.M31*z
                , m.M12*x + m.M22*y + m.M32*z
                , m.M13*x + m.M23*y + m.M33*z
                )

        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------

        /// Returns the World X-axis with length one: Vec(1,0,0)
        static member inline Xaxis  = Vec(1,0,0)

        /// Returns the World Y-axis with length one: Vec(0,1,0)
        static member inline Yaxis  = Vec(0,1,0)

        /// Returns the World Z-axis with length one: Vec(0,0,1)
        static member inline Zaxis  = Vec(0,0,1)

        /// Returns a zero length vector: Vec(0,0,0)
        static member inline Zero   = Vec(0,0,0)  // this member is needed by Seq.sum, so that it doesn't fail on empty seq.

        /// Returns the distance between the tips of two 3D vectors.
        static member inline difference (a:Vec) (b:Vec) = let v = a-b in sqrt(v.X*v.X + v.Y*v.Y + v.Z*v.Z)

        /// Returns the squared distance between the tips of two 3D vectors.
        /// This operation is slightly faster than Vec.difference and sufficient for many algorithms like finding closest points.
        static member inline differenceSq (a:Vec) (b:Vec) = let v = a-b in  v.X*v.X + v.Y*v.Y + v.Z*v.Z

        /// Divides the vector by an integer.
        /// (This member is needed by Array.average and similar functions)
        static member inline DivideByInt (v:Vec, i:int) = // needed by 'Array.average'
            if i<>0 then v / float i
            else FsExGeoDivByZeroException.Raise "FsEx.Geo.Vec.DivideByInt is zero %O " v

                /// Accepts any type that has a X, Y and Z (UPPERCASE) member that can be converted to a float.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline ofXYZ vec  =
            let x = ( ^T : (member X : _) vec)
            let y = ( ^T : (member Y : _) vec)
            let z = ( ^T : (member Z : _) vec)
            try Vec(float x, float y, float z)
            with e -> FsExGeoException.Raise "FsEx.Geo.Vec.ofXYZ: %A could not be converted to a FsEx.Geo.Vec:\r\n%A" vec e

        /// Accepts any type that has a x, y and z (lowercase) member that can be converted to a float.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline ofxyz vec  =
            let x = ( ^T : (member x : _) vec)
            let y = ( ^T : (member y : _) vec)
            let z = ( ^T : (member z : _) vec)
            try Vec(float x, float y, float z)
            with e -> FsExGeoException.Raise "FsEx.Geo.Vec.ofxyz: %A could not be converted to a FsEx.Geo.Vec:\r\n%A" vec e

        /// Create 3D vector from 3D point.
        static member inline ofPnt  (pt:Pnt) =  Vec( pt.X , pt.Y , pt.Z )

        /// Create 3D vector from 3D unit-vector.
        static member inline ofUnitVec (v:UnitVec) =  Vec(v.X, v.Y, v.Z)

        /// Convert 3D vector to 2D point by ignoring Z value.
        static member inline asPt(v:Vec)  = Pt( v.X, v.Y)

        /// Convert 3D vector to 2D vector by ignoring Z value.
        static member inline asVc(v:Vec) = Vc(v.X, v.Y)

        /// Convert 3D vector to 3D point.
        static member inline asPnt(v:Vec) = Pnt(v.X, v.Y, v.Z)


        //static member inline cross (a:Vec, b:Vec)  //moved to Vec type declaration

        /// Cross product, of a 3D unit vectors an a 3D vector.
        /// The resulting vector is perpendicular to both input vectors.
        /// Its length is the area of the parallelogram spanned by the input vectors.
        /// Its direction follows th right-hand rule.
        /// A x B = |A| * |B| * sin(angle)
        static member inline cross (a:UnitVec, b:Vec)  = Vec (a.Y * b.Z - a.Z * b.Y ,  a.Z * b.X - a.X * b.Z ,  a.X * b.Y - a.Y * b.X )

        /// Cross product, of a 3D vector and a 3D unit vectors.
        /// The resulting vector is perpendicular to both input vectors.
        /// Its length is the area of the parallelogram spanned by the input vectors.
        /// Its direction follows th right-hand rule.
        /// A x B = |A| * |B| * sin(angle)
        static member inline cross (a:Vec, b:UnitVec)  = Vec (a.Y * b.Z - a.Z * b.Y ,  a.Z * b.X - a.X * b.Z ,  a.X * b.Y - a.Y * b.X )


        //static member inline dot  (a:Vec, b:Vec)   //moved to Vec type declaration

        /// Dot product, or scalar product of a 3D unit-vector with a 3D vector.
        /// Returns a float. This float is the projected length of the 3D vector on the direction of the unit-vector.
        static member inline dot  (a:UnitVec, b:Vec ) = a.X * b.X + a.Y * b.Y + a.Z * b.Z

        /// Dot product, or scalar product of a 3D vector with a 3D unit-vector.
        /// Returns a float. This float is the projected length of the 3D vector on the direction of the unit-vector.
        static member inline dot  (a:Vec, b:UnitVec) = a.X * b.X + a.Y * b.Y + a.Z * b.Z

        /// Gets the X part of this 3D vector.
        static member inline getX  (v:Vec) = v.X

        /// Gets the Y part of this 3D vector.
        static member inline getY (v:Vec) = v.Y

        /// Gets the Z part of this 3D vector.
        static member inline getZ  (v:Vec) = v.Z

        /// Returns new 3D vector with new X value, Y and Z stay the same.
        static member inline setX  x (v:Vec) = v.WithX x

        /// Returns new 3D vector with new Y value, X and Z stay the same.
        static member inline setY  y (v:Vec) = v.WithY y

        /// Returns new 3D vector with new z value, X and Y stay the same.
        static member inline setZ z (v:Vec) = v.WithZ z

        /// Add two 3D vectors together. Returns a new 3D vector.
        static member inline add  (a:Vec) (b:Vec) = b + a

        /// Multiplies a 3D vector with a scalar, also called scaling a vector.
        /// Returns a new 3D vector.
        static member inline scale (f:float) (v:Vec) = Vec (v.X * f , v.Y * f , v.Z * f)

        /// Returns a new 3D vector scaled to the desired length.
        /// Same as vec.WithLength. Returns a new 3D vector.
        static member inline setLength(desiredLength:float) (v:Vec) =
            let l = v.Length
            if l < zeroLengthTol then FsExGeoDivByZeroException.Raise "FsEx.Geo.Vec.setLength %g : %O is too small for unitizing, Tolerance:%g" desiredLength v zeroLengthTol
            v * (desiredLength / l)

        /// Add to the X part of this 3D vectors together. Returns a new 3D vector.
        static member inline moveX x (v:Vec) = Vec (v.X+x, v.Y,   v.Z)

        /// Add to the Y part of this 3D vectors together. Returns a new 3D vector.
        static member inline moveY y (v:Vec) = Vec (v.X,   v.Y+y, v.Z)

        /// Add to the Z part of this 3D vectors together. Returns a new 3D vector.
        static member inline moveZ z (v:Vec) = Vec (v.X,   v.Y,   v.Z+z)

        /// Check if the 3D vector is shorter than the tolerance.
        static member inline isTiny tol (v:Vec) =
            v.Length < tol

        /// Check if the 3D vector is shorter than the squared tolerance.
        static member inline isTinySq tol (v:Vec) =
            v.LengthSq < tol

        /// Returns the length of the 3D vector.
        static member inline length  (v:Vec) = v.Length

        /// Returns the squared length of the 3D vector.
        /// The square length is faster to calculate and often good enough for use cases such as sorting vectors by length.
        static member inline lengthSq (v:Vec) = v.LengthSq

        /// Returns a new 3D vector from X,Y and Z parts.
        static member inline create (x:float, y:float, z:float) =  Vec( x , y , z )

        /// Returns a new 3D vector from start and end point.
        static member inline create (start:Pnt,ende:Pnt) = ende-start

        /// Returns a 3D vector from z value and 2D vector.
        static member inline ofVcWithZ  (z:float)  (v:Vc)  = Vec (v.X, v.Y, z)

        /// Project vector to World X-Y plane.
        /// Use Vc.ofVec to convert to 2D vector instance.
        static member inline projectToXYPlane (v:Vec) = Vec(v.X,v.Y, 0.0)

        /// Negate or inverse a 3D vectors. Returns a new 3D vector.
        /// Same as Vec.flip.
        static member inline reverse  (v:Vec) = -v

        /// Negate or inverse a 3D vectors. Returns a new 3D vector.
        /// Same as Vec.reverse.
        static member inline flip  (v:Vec) = -v

        /// Flips the vector if Z part is smaller than 0.0
        static member inline flipToPointUp (v:Vec) = if v.Z < 0.0 then -v else v

        /// Returns 3D vector unitized, fails on zero length vectors.
        static member inline unitize (v:Vec) =  v.Unitized

        /// Unitize 3D vector, if input vector is shorter than 1e-6 the default unit-vector is returned.
        static member inline unitizeOrDefault (defaultUnitVector:UnitVec) (v:Vec) =
            let l = v.LengthSq
            if l < 1e-12  then  // = sqrt (1e-06)
                defaultUnitVector
            else
                let f = 1.0 / sqrt(l)
                UnitVec.createUnchecked(v.X*f , v.Y*f , v.Z*f)

        /// Returns three vector's Determinant.
        /// This is also the signed volume of the Parallelepipeds define by these three vectors.
        /// Also called scalar triple product, mixed product, Box product, or in German: Spatprodukt.
        /// It is defined as the dot product of one of the vectors with the cross product of the other two.
        static member inline determinant (u:Vec, v:Vec, w:Vec) = u.X*v.Y*w.Z + v.X*w.Y*u.Z + w.X*u.Y*v.Z - w.X*v.Y*u.Z - v.X*u.Y*w.Z - u.X*w.Y*v.Z

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
        /// Considering counter clockwise rotation round the World Zaxis.
        /// Range: 0.0 to 2 Pi ( = 0 to 360 Degrees)
        static member inline angle2PiInXY (a:Vec, b:Vec)   =
            let r = b.Direction2PiInXY  - a.Direction2PiInXY
            if r >= 0. then  r
            else r + Util.twoPi

        /// Returns positive angle of two 3D vector projected in X-Y plane.
        /// In Degrees.
        /// Considering positive rotation round the World Z-axis.
        /// Range: 0 to 360 Degrees.
        static member inline angle360InXY (a:Vec, b:Vec)   =
            Vec.angle2PiInXY (a, b) |> toDegrees

        /// Returns positive angle for rotating counter clockwise from vector 'a' to vector 'b' .
        /// In Diamond Angle. Using only proportion of X to Y components.
        /// Range of 0.0 to 4.0 ( for 360 Degrees)
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline angleDiamondInXY (a:Vec , b:Vec)   = a.AngleDiamondInXYTo(b)

        /// The diamond angle.
        /// Returns positive angle of 3D vector in World X-Y plane.
        /// Calculates the proportion of X to Y component.
        /// It is always positive and in the range of 0.0 to 4.0 ( for 360 Degrees)
        /// 0.0 = Xaxis,  going Counter clockwise.
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions.
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline directionDiamondInXY(v:Vec) = v.DirectionDiamondInXY

        /// Returns positive angle of 3D vector in World X-Y plane. Counter clockwise from X-axis.
        /// In Radians.
        /// Range: 0.0 to 2 Pi ( = 0 to 360 Degrees)
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline direction2PiInXY (v:Vec)   = v.Direction2PiInXY

        /// Returns positive angle of 3D vector in World X-Y plane. Counter clockwise from X-axis.
        /// In Degree.
        /// Range: 0.0 to 2 Pi ( = 0 to 360 Degrees)
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline direction360InXY (v:Vec)  = v.Direction360InXY

        /// Returns a (not unitized) bisector vector in the middle direction.
        /// Code : a.Unitized + b.Unitized.
        static member inline bisector (a:Vec) (b:Vec) = a.Unitized + b.Unitized


        /// Ensure vector has a positive dot product with given orientation vector.
        static member inline matchOrientation (orientationToMatch:Vec) (v:Vec) =
            if orientationToMatch * v < 0.0 then -v else v


        /// Checks if the angle between the two 3D vectors is less than 180 degrees.
        /// Calculates the dot product of two 3D vectors.
        /// Then checks if it is positive.
        static member inline matchesOrientation180 (other:Vec) (v:Vec) = v.MatchesOrientation180 other

        /// Checks if the angle between the two 3D vectors is less than 90 degrees.
        /// Calculates the dot product of the two 3D vectors unitized.
        /// Then checks if it is bigger than 0.707107 (cosine of 90 degrees).
        static member inline matchesOrientation90 (other:Vec) (v:Vec) = v.MatchesOrientation90 other

        /// Checks if Angle between two vectors is Below 0.25 Degree.
        /// Ignores vector orientation.
        /// Fails on zero length vectors, tolerance 1e-12.
        /// Same as isAngleBelowQuatreDegree.
        static member inline  areParallel (other:Vec) (v:Vec) =   v.IsParallelTo other


        /// Checks if Angle between two vectors is between 98.75 and 90.25 Degree.
        /// Ignores vector orientation.
        /// Fails on zero length vectors, tolerance 1e-12.
        static member inline areParallelAndMatchOrientation (other:Vec) (v:Vec) = v.IsParallelAndOrientedTo other

        /// Checks if Angle between two vectors is between 98.75 and 90.25 Degree.
        /// Ignores vector orientation.
        /// Fails on zero length vectors, tolerance 1e-12.
        static member inline arePerpendicular(other:Vec) (v:Vec) =  v.IsPerpendicularTo other


        // Rotate2D:

        /// 90 Degree rotation counter clockwise around Z-axis.
        static member inline rotateOnZ90CCW(v:Vec) = Vec( -v.Y,   v.X ,   v.Z)

        /// 90 Degree rotation clockwise around Z-axis.
        static member inline rotateOnZ90CW(v:Vec)  = Vec(  v.Y,  -v.X,   v.Z  )

        /// Rotate the 3D vector around X-axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member rotateXBy (r:Rotation2D) (v:Vec) = Vec (v.X,  r.Cos*v.Y - r.Sin*v.Z, r.Sin*v.Y + r.Cos*v.Z)

        /// Rotate the 3D vector around Y-axis, from Z to X-axis, Counter Clockwise looking from back.
        static member rotateYBy (r:Rotation2D) (v:Vec) = Vec ( r.Sin*v.Z + r.Cos*v.X,  v.Y, r.Cos*v.Z - r.Sin*v.X)

        /// Rotate the 3D vector around Z-axis, from X to Y-axis, Counter Clockwise looking from top.
        static member rotateZBy (r:Rotation2D) (v:Vec) = Vec (r.Cos*v.X - r.Sin*v.Y, r.Sin*v.X + r.Cos*v.Y,  v.Z)

        /// Rotate the 3D vector in Degrees around X-axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member inline rotateX (angDegree) (v:Vec) =
            Vec.rotateXBy (Rotation2D.createFromDegrees angDegree) v

        /// Rotate the 3D vector in Degrees around Y-axis, from Z to X-axis, Counter Clockwise looking from back.
        static member inline rotateY (angDegree) (v:Vec) =
            Vec.rotateYBy  (Rotation2D.createFromDegrees angDegree) v

        /// Rotate the 3D vector in Degrees around Z-axis, from X to Y-axis, Counter Clockwise looking from top.
        static member inline rotateZ (angDegree) (v:Vec) =
            Vec.rotateZBy  (Rotation2D.createFromDegrees angDegree) v

        /// Rotate by Quaternion.
        static member inline rotateByQuaternion  (q:Quaternion) (v:Vec) =
            v*q  // operator * is defined in Quaternion.fs

        /// Returns the vector length projected into X Y Plane.
        /// sqrt( v.X * v.X  + v.Y * v.Y)
        static member inline lengthInXY(v:Vec) = sqrt(v.X * v.X  + v.Y * v.Y)

        /// Checks if a vector is vertical by doing:
        /// abs(v.X) + abs(v.Y) < zeroLengthTol.
        /// Fails on tiny (shorter than zeroLengthTol) vectors.
        static member inline isVertical (v:Vec) =
            if v.LengthSq < 1e-16 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Vec.isVertical cannot not check very tiny vector for verticality %O" v
            abs(v.X) + abs(v.Y) < zeroLengthTol

        /// Checks if a vector is horizontal by doing:
        /// abs(v.Z) < zeroLengthTol.
        /// Fails on tiny (shorter than zeroLengthTol) vectors.
        static member inline isHorizontal (v:Vec) =
            if v.LengthSq < 1e-16 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Vec.isHorizontal Cannot not check very tiny vector for horizontality %O" v
            abs(v.Z) < zeroLengthTol

        /// Returns positive or negative slope of a vector in Radians.
        /// In relation to X-Y plane.
        static member inline slopeRadians (v:Vec) =
            let f = Vec(v.X, v.Y, 0.0)
            if v.Z >= 0.0 then  Vec.angleHalfPi v f
            else              -(Vec.angleHalfPi v f)

        /// Returns positive or negative slope of a vector in Degrees.
        /// In relation to X-Y plane.
        static member inline slopeDegrees (v:Vec) =
            let f = Vec(v.X, v.Y, 0.0)
            if v.Z >= 0.0 then  Vec.angle90 v f
            else              -(Vec.angle90 v f)

        /// Returns positive or negative slope of a vector in Percent.
        /// In relation to X-Y plane.
        /// 100% = 45 Degrees.
        static member inline slopePercent (v:Vec) =
            if abs(v.Z) < zeroLengthTol then FsExGeoDivByZeroException.Raise "FsEx.Geo.Vec.slopePercent: Can't get Slope from vertical vector %O" v
            let f = Vec(v.X, v.Y, 0.0)
            100.0 * (v.Z/f.Length)


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

        /// Multiplies a Matrix with a 3D vector (with an implicit 1 in the 4th dimension,
        /// So that it also works correctly for projections.)
        static member inline transform (m:Matrix) (v:Vec) =
            v.Transform(m)

        /// Multiplies (or applies) an OrthoMatrix to a 3D vector .
        static member inline  transformOrtho (m:OrthoMatrix) (v:Vec) =
            v.TransformOrtho(m)

        /// Multiplies (or applies) only the 3x3 rotation part of an OrthoMatrix to a 3D vector .
        /// The resulting vector has the same length as the input.
        static member inline rotateOrtho (m:OrthoMatrix) (v:Vec) =
            v.RotateOrtho(m)

        /// Checks if Angle between two vectors is less than one Degree.
        /// Ignores vector orientation.
        /// Fails on zero length vectors, tolerance 1e-12.
        static member inline isAngleLessThan1Degree(a:Vec, b:Vec) =
            let sa = a.LengthSq
            if sa < 1e-24 then   FsExGeoException.Raise "FsEx.Geo.Vec.isAngleLessThan1Degree: Vec a is too short: %s. Vec b:%s " a.AsString b.AsString
            let sb = b.LengthSq
            if sb < 1e-24 then   FsExGeoException.Raise "FsEx.Geo.Vec.isAngleLessThan1Degree: Vec b is too short: %s. Vec a:%s " b.AsString a.AsString
            let au = a * (1.0 / sqrt sa )
            let bu = b * (1.0 / sqrt sb )
            abs(bu*au) > float Cosine.``1.0``


        /// Checks if Angle between two vectors is less than 0.25 Degree.
        /// Ignores vector orientation.
        /// Fails on zero length vectors, tolerance 1e-12.
        /// Same as Vec. areParallel.
        static member inline isAngleLessThanQuatreDegree(a:Vec, b:Vec) =
            let sa = a.LengthSq
            if sa < 1e-24 then   FsExGeoException.Raise "FsEx.Geo.Vec.isAngleLessThanQuatreDegree: Vec a is too short: %s. Vec b:%s " a.AsString b.AsString
            let sb = b.LengthSq
            if sb < 1e-24 then   FsExGeoException.Raise "FsEx.Geo.Vec.isAngleLessThanQuatreDegree: Vec b is too short: %s. Vec a:%s " b.AsString a.AsString
            let au = a * (1.0 / sqrt sa )
            let bu = b * (1.0 / sqrt sb )
            abs(bu*au) > float Cosine.``0.25``


        /// Checks if Angle between two vectors is less than 5 Degrees.
        /// Ignores vector orientation.
        /// Fails on zero length vectors, tolerance 1e-12.
        static member inline isAngleLessThan5Degree(a:Vec, b:Vec) =
            let sa = a.LengthSq
            if sa < 1e-24 then   FsExGeoException.Raise "FsEx.Geo.Vec.isAngleLessThanQuatreDegree: Vec a is too short: %s. Vec b:%s " a.AsString b.AsString
            let sb = b.LengthSq
            if sb < 1e-24 then   FsExGeoException.Raise "FsEx.Geo.Vec.isAngleLessThanQuatreDegree: Vec b is too short: %s. Vec a:%s " b.AsString a.AsString
            let au = a * (1.0 / sqrt sa )
            let bu = b * (1.0 / sqrt sb )
            abs(bu*au) > float Cosine.``5.0``


        

        ///<summary> Intersects two infinite 3D lines.
        /// The lines are defined by a start point and a vector.
        /// 'ValueNone' is returned, if the angle between the vectors is less than 0.25 degrees
        /// or any of them is shorter than 1e-6. These tolerances can be adjusted with optional parameters. </summary>
        ///<param name="ptA"> The start point of the first line.</param>
        ///<param name="ptB"> The start point of the second line.</param>
        ///<param name="vA" > The vector of the first line.</param>
        ///<param name="vB" > The vector of the second line.</param>
        ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
        ///  If one or both vectors are shorter than this ValueNone is returned .</param>
        ///<param name="relAngleDiscriminant"> This is an optional tolerance for the internally calculated relative Angle Discriminant.
        /// The default value corresponds to approx 0.25 degree. Below this angle vectors are considered parallel.
        /// Use the module FsEx.Geo.Util.RelAngleDiscriminant to set another tolerance here.</param>
        ///<returns> For (almost) zero length or (almost) parallel vectors: ValueNone
        /// Else ValueSome with a tuple of the parameters at which the two infinite 2D lines intersect to each other.
        /// The tuple's order corresponds to the input order.</returns>
        static member inline intersection( ptA:Pnt,
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
                let ac = a*c // square of square length , never negative
                let bb = b*b // square of square dot product, never negative
                let discriminant = ac - bb // never negative , the dot product cannot be bigger than the two square length multiplied with each other
                let div = ac+bb // never negative
                // getting the relation between the sum and the subtraction gives a good estimate of the angle between the lines
                // see module FsEx.Geo.Util.RelAngleDiscriminant
                let rel = discriminant/div
                if rel < float relAngleDiscriminant then //parallel
                    ValueNone
                else
                    let vx = ptB.X - ptA.X
                    let vy = ptB.Y - ptA.Y
                    let vz = ptB.Z - ptA.Z
                    let e = bx*vx + by*vy + bz*vz
                    let d = ax*vx + ay*vy + az*vz
                    let t = (c * d - b * e ) / discriminant
                    let u = (b * d - a * e ) / discriminant
                    ValueSome (t,u)

        ///<summary> Similar and aligned to to Vec.intersection this function checks if two 3D vectors intersect.
        /// 'false' is returned, if the angle between the vectors is less than 0.25 degrees
        /// or any of them is shorter than 1e-6. These tolerances can be adjusted with optional parameters.
        /// The function Vec.areParallel does the same thing but with a more precises way of calculating parallel lines </summary>
        ///<param name="vA" > The vector of the first line.</param>
        ///<param name="vB" > The vector of the second line.</param>
        ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
        ///  If one or both vectors are shorter than this ValueNone is returned .</param>
        ///<param name="relAngleDiscriminant"> This is an optional tolerance for the internally calculated relative Angle Discriminant.
        /// The default value corresponds to approx 0.25 degree. Below this angle vectors are considered parallel.
        /// Use the module FsEx.Geo.Util.RelAngleDiscriminant to set another tolerance here.</param>
        ///<returns> For (almost) zero length or (almost) parallel vectors: 'false' else 'true'.</returns>
        static member inline doIntersect (  vA:Vec,
                                            vB:Vec,
                                            [<OPT;DEF(1e-6)>] tooShortTolerance:float,
                                            [<OPT;DEF(RelAngleDiscriminant.``0.25``)>] relAngleDiscriminant:float<RelAngleDiscriminant.relAngDiscr>
                                            ) : bool =
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
                false
            elif c < tooShortTolerance * tooShortTolerance then  // vec B too short
                false
            else
                let b = ax*bx + ay*by + az*bz // dot product of both lines
                let ac = a*c // square of square length  , never negative
                let bb = b*b // never negative
                let discriminant = ac - bb // never negative , the dot product cannot be bigger than the two square length multiplied with each other
                let div = ac+bb // never negative
                // getting the relation between the sum and the subtraction gives a good estimate of the angle between the lines
                // see module FsEx.Geo.Util.RelAngleDiscriminant
                let rel = discriminant/div
                rel > float relAngleDiscriminant