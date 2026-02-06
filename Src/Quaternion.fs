namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open Euclid.UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open EuclidErrors

/// A struct containing 4 floats, representing an immutable unitized Quaternion, for arbitrary 3D rotations.
/// This implementation guarantees the Quaternion to be always unitized.
/// Note: Never use the struct default constructor Quaternion() as it will create an invalid zero length Quaternion.
/// Use Quaternion.create or Quaternion.createUnchecked instead.
[<Struct; NoEquality; NoComparison>]
[<IsReadOnly>]
//[<IsByRefLike>]
[<DataContract>] // for using DataMember on fields
type Quaternion =
    //  https://github.com/mrdoob/three.js/blob/dev/src/math/Quaternion.js

    //[<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar

    /// The field holding the X component of this Quaternion.
    [<DataMember>] val X:float

    /// The field holding the Y component of this Quaternion.
    [<DataMember>] val Y:float

    /// The field holding the Z component of this Quaternion.
    [<DataMember>] val Z:float

    /// The field holding the W component of this Quaternion.
    [<DataMember>] val W:float

    /// Unsafe internal constructor, public only for inlining.
    [<Obsolete("This is not Obsolete, but an unsafe internal constructor. The input is not verified, so it might create invalid geometry. It is exposed as a public member so that it can be inlined. So marked Obsolete instead.") >]
    new (x, y, z, w) =
        #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
            let l = x*x  + y*y + z*z + w*w
            if isNotOne l then failQuat w x y z
        #endif
            {X=x; Y=y; Z=z; W=w}

    /// This unsafe constructor does NOT do any unitizing.
    static member inline createUnchecked (x, y, z, w) : Quaternion =
        #nowarn "44"
        Quaternion(x, y, z, w)
        #warnon "44" // re-enable warning for obsolete usage

    /// Format Quaternion into string also showing angle in Degree as nicely formatted floating point number.
    override q.ToString() =
        let ang = q.W |> acosSafe |> ( * ) 2.0 |>  toDegrees
        $"Euclid.Quaternion(X={Format.float q.X}|Y={Format.float q.Y}|Z={Format.float q.Z}, W={Format.float q.W}| angle: {Format.float ang}°)"

    /// Format Quaternion into string also showing angle in Degree as nicely formatted floating point number.
    /// Using nice floating point number formatting.
    /// But without full type name as in q.ToString()
    member q.AsString : string =
        let ang = q.W |> acosSafe |> ( * ) 2.0 |>  toDegrees
        $"X={Format.float q.X}|Y={Format.float q.Y}|Z={Format.float q.Z}, angle: {Format.float ang}°"

    /// Format Quaternion into an F# code string that can be used to recreate the quaternion.
    member q.AsFSharpCode : string =
        $"Quaternion.create({q.X}, {q.Y}, {q.Z}, {q.W})"


    /// Returns a new Quaternion for the inverse rotation.
    /// Same as q.Inverse.
    member inline q.Conjugate =
        Quaternion.createUnchecked(-q.X, -q.Y, -q.Z, q.W)

    /// Returns a new Quaternion for the inverse rotation.
    /// Same as q.Conjugate.
    member inline q.Inverse =
        Quaternion.createUnchecked(-q.X, -q.Y, -q.Z, q.W)

    /// This method should normally not be needed as Euclid Quaternions are always unitized.
    /// However, use this to correct numerical drift after many quaternion multiplications.
    /// This is the same as calling Quaternion.create with the current components.
    /// Returns a renormalized copy of this Quaternion with unit length.
    member inline q.Normalize() =
        Quaternion.create(q.X, q.Y, q.Z, q.W)

    [<Obsolete("The Magnitude is always one. This function only exist for testing.")>]
    member q.Magnitude =
        sqrt (q.X*q.X + q.Y*q.Y + q.Z*q.Z + q.W*q.W)

    /// Returns the angle in Radians.
    member inline q.AngleInRadians =
        q.W |> acosSafe |> ( * ) 2.0

    /// Returns the angle in Degree.
    member inline q.AngleInDegrees =
        q.AngleInRadians |>  toDegrees

    /// Returns the rotation axis of this Quaternion as a (non-unit) vector.
    /// This is just q.X, q.Y and q.Z, which equals axis * sin(angle/2).
    /// For the identity quaternion (no rotation), this returns a zero-length vector Vec(0,0,0).
    /// The length of this vector is sin(angle/2), ranging from 0 (no rotation) to 1 (180° rotation).
    member inline q.Axis =
        Vec(q.X, q.Y, q.Z)

    /// Get a new Quaternion that rotates around the same axis but with a different angle. In Radians.
    /// Fails for identity or near-identity quaternions where the rotation axis is not defined (angle ≈ 0).
    member q.setAngleInRadians (angleInRadians) =
        let length = sqrt(q.X*q.X + q.Y*q.Y + q.Z*q.Z)
        if isTooTiny(length) then
            fail $"Quaternion.setAngleInRadians failed. The length of the axis is too short: {q.Axis}"
        let sc = 1. / length // inverse for unitizing vector:
        let angHalf = angleInRadians * 0.5
        let sa = sc * sin angHalf
        Quaternion.createUnchecked( q.X * sa,
                     q.Y * sa,
                     q.Z * sa,
                     cos angHalf )

    /// Get a new Quaternion that rotates around the same axis but with a different angle. In Degree.
    /// Fails for identity or near-identity quaternions where the rotation axis is not defined (angle ≈ 0).
    member inline q.setAngleInDegrees angleInDegrees =
        q.setAngleInRadians (toRadians angleInDegrees)

    (* TODO the interpolation follows a Cone. is that correct ?

    /// Does a spherical linear interpolation between quaternions.
    /// 'rel' represents the amount of rotation between this quaternion (where rel is 0)
    /// and the other (where rel is 1).
    /// The parameter 'rel' is NOT clamped.
    member q.Slerp (other:Quaternion, rel) =
        if   rel = 0.0 then q
        elif rel = 1.0 then other
        else
            let x = q.X
            let y = q.Y
            let z = q.Z
            let w = q.W
            let x2 = other.X
            let y2 = other.Y
            let z2 = other.Z
            let w2 = other.W
            let cosHalfTheta = w * w2 + x * x2 + y * y2 + z * z2
            let sqrSinHalfTheta = 1.0 - cosHalfTheta * cosHalfTheta
            if sqrSinHalfTheta <= 1e-18 then
                let s = 1.0 - rel
                Quaternion (s * x + rel * x2, s * y + rel * y2, s * z + rel * z2,s * w + rel * w2)
            else
                let sinHalfTheta = sqrt sqrSinHalfTheta
                let halfTheta = atan2 sinHalfTheta cosHalfTheta
                let ratioA = sin ((1.0 - rel) * halfTheta) / sinHalfTheta
                let ratioB = sin (rel * halfTheta) / sinHalfTheta
                Quaternion (
                    x * ratioA + x2 * ratioB,
                    y * ratioA + y2 * ratioB,
                    z * ratioA + z2 * ratioB,
                    w * ratioA + w2 * ratioB
                    )
    *)


    // --------------------------------------------------------------------
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


    /// Checks if two Quaternion are equal within tolerance (component-wise comparison).
    /// Use a tolerance of 0.0 to check for an exact match.
    /// Note: A Quaternion with every component negated represents the same rotation but will be considered unequal by this function.
    /// For rotational equivalence, consider using Quaternion.equalsRotation function.
    static member inline equals (tolerance:float) (a:Quaternion) (b:Quaternion)  : bool =
        abs(a.X-b.X) <= tolerance &&
        abs(a.Y-b.Y) <= tolerance &&
        abs(a.Z-b.Z) <= tolerance &&
        abs(a.W-b.W) <= tolerance

    /// Checks if two Quaternion represent an equal rotation.
    /// Use a tolerance of 0.0 to check for an exact match.
    /// Note: A Quaternion with every component negated represents the same rotation and will be considered equal by this function.
    /// For component-wise equality, consider using Quaternion.equals function.
    static member inline equalsRotation (tolerance:float) (a:Quaternion) (b:Quaternion) =
        Quaternion.equals tolerance a b ||
        Quaternion.equals tolerance a (Quaternion.createUnchecked(-b.X, -b.Y, -b.Z, -b.W))


    /// Multiply two Quaternions. It's like adding one rotation to the other.
    /// Note: Repeated multiplications may accumulate floating-point errors that violate the unit-length invariant.
    /// Use q.Normalize() to renormalize if combining many rotations.
    static member multiply (l:Quaternion, r:Quaternion) =
        Quaternion.createUnchecked(
            l.W * r.X + l.X * r.W + l.Y * r.Z - l.Z * r.Y,
            l.W * r.Y + l.Y * r.W + l.Z * r.X - l.X * r.Z,
            l.W * r.Z + l.Z * r.W + l.X * r.Y - l.Y * r.X,
            l.W * r.W - l.X * r.X - l.Y * r.Y - l.Z * r.Z)

    /// Multiply two Quaternions. It's like adding one rotation to the other.
    static member inline ( *** ) (l:Quaternion, r:Quaternion) =
        Quaternion.multiply(l, r)

    /// This constructor does unitizing too.
    static member create (x, y, z, w) : Quaternion =
        let l = sqrt(x*x  + y*y + z*z + w*w)
        if isTooTiny (abs l) then
            fail $"Quaternion create failed for x:{x}, y:{y}, z:{z}, w:{w}. The length needs to be bigger than zero."
        let sc = 1./l
        Quaternion.createUnchecked(x*sc, y*sc, z*sc, w*sc)


    /// The identity quaternion that does not do any rotation.
    /// This is Quaternion(x=0, y=0, z=0, w=1)
    static member inline identity =
        Quaternion.createUnchecked(0, 0, 0, 1)


    /// The created rotation is Clockwise looking in the direction of the vector.
    /// The vector may be of any length but zero.
    static member createFromRadians (axis:Vec, angleInRadians) =
        let length = sqrt(axis.X*axis.X + axis.Y*axis.Y + axis.Z*axis.Z)
        if isTooTiny(length) then
            fail $"Quaternion.createFromRadians failed too short axis: %O{axis} and rotation: %g{toDegrees angleInRadians}° Degrees."
        let angHalf = angleInRadians * 0.5
        let sa = sin angHalf
        let sc = 1. / length // inverse for unitizing vector:
        Quaternion.createUnchecked(axis.X * sc * sa, axis.Y * sc * sa, axis.Z * sc * sa, cos angHalf)

    /// The created rotation is Clockwise looking in the direction of the unit-vector.
    static member inline createFromRadians (axis:UnitVec, angleInRadians) =
        let angHalf = angleInRadians * 0.5
        let sa = sin angHalf
        Quaternion.createUnchecked(axis.X  * sa, axis.Y  * sa, axis.Z  * sa, cos angHalf)

    /// The created rotation is Clockwise looking in the direction of the vector (of any length but zero).
    static member inline createFromDegree (axis:Vec, angleInDegrees) =
        Quaternion.createFromRadians (axis, toRadians angleInDegrees)

    /// The created rotation is Clockwise looking in the direction of the unit-vector.
    static member inline createFromDegree (axis:UnitVec, angleInDegrees) =
        Quaternion.createFromRadians (axis, toRadians angleInDegrees)

    /// Creates a rotation from one vector's direction to another vector's direction.
    /// If the tips of the two unit vectors are closer than 1e-12 (squared: 1e-24) then an identity Quaternion is returned.
    /// If the tips of the two vectors are almost exactly opposite (sum length too small),
    /// there is no valid unique 180 degree rotation that can be found, so an exception is raised.
    static member inline createVecToVec(vecFrom:UnitVec, vecTo:UnitVec) =
        let v = vecFrom - vecTo
        if v.LengthSq < 1e-24 then // the vectors are almost the same (threshold: (1e-12)^2)
            Quaternion.identity
        else
            let v = vecFrom + vecTo
            if isTooSmallSq v.LengthSq then // the vectors are almost exactly opposite
                fail $"Quaternion.createVecToVec failed to find a rotation axis for (almost) colinear unit-vectors in opposite directions: %O{vecFrom} and %O{vecTo}"

            // cross vectors(vFrom, vTo); // inlined to avoid cyclic dependency
            Quaternion.create ( vecFrom.Y * vecTo.Z - vecFrom.Z * vecTo.Y
                              , vecFrom.Z * vecTo.X - vecFrom.X * vecTo.Z
                              , vecFrom.X * vecTo.Y - vecFrom.Y * vecTo.X
                              , vecFrom *** vecTo  + 1.0
                              )


    /// Creates a rotation from one vector's direction to another vector's direction.
    /// If the tips of the two vectors (unitized) are closer than 1e-12 (squared: 1e-24) then an identity Quaternion is returned.
    /// If the tips of the two vectors (unitized) are almost exactly opposite (sum length too small),
    /// there is no valid unique 180 degree rotation that can be found, so an exception is raised.
    static member createVecToVec(vecFrom:Vec, vecTo:Vec) =
        let fu =
            let x = vecFrom.X
            let y = vecFrom.Y
            let z = vecFrom.Z
            let length = sqrt(x*x + y*y + z*z)
            if isTooTiny(length) then
                fail $"Quaternion.createVecToVec failed. The vector is too short: vecFrom: %O{vecFrom}"
            let sc = 1. / length // inverse for unitizing vector:
            UnitVec.createUnchecked(x*sc, y*sc, z*sc)
        let tu =
            let x = vecTo.X
            let y = vecTo.Y
            let z = vecTo.Z
            let length = sqrt(x*x + y*y + z*z)
            if isTooTiny(length) then
                fail $"Quaternion.createVecToVec failed. The vector is too short: vecTo: %O{vecTo}"
            let sc = 1. / length // inverse for unitizing vector:
            UnitVec.createUnchecked(x*sc, y*sc, z*sc)

        let v = fu - tu
        if v.LengthSq < 1e-24 then // the vectors are almost the same
            Quaternion.identity
        else
            let v = fu + tu
            if isTooSmallSq v.LengthSq then // the vectors are almost exactly opposite
                fail $"Quaternion.createVecToVec failed to find a rotation axis for (almost) colinear  (or NaN) vectors in opposite directions: %O{vecFrom} and %O{vecTo}"

            // cross vectors( vFrom, vTo); // inlined to avoid cyclic dependency
            Quaternion.create ( fu.Y * tu.Z - fu.Z * tu.Y
                              , fu.Z * tu.X - fu.X * tu.Z
                              , fu.X * tu.Y - fu.Y * tu.X
                              , fu *** tu  + 1.0
                              )

    /// Angles are given in Degrees,
    /// The order in which to apply rotations is X-Y-Z,
    /// which means that the object will first be rotated around its X-axis,
    /// then its Y-axis and finally its Z-axis.
    /// This uses intrinsic Tait-Bryan angles.
    /// This means that rotations are performed with respect to the local coordinate system.
    /// That is, for order X-Y-Z, the rotation is first around the local-X-axis (which is the same as the World-X-axis),
    /// then around local-Y (which may now be different from the World Y-axis),
    /// then local-Z (which may be different from the World Z-axis)
    static member createFromEulerXYZ(degreesX, degreesY, degreesZ) =
        let c1 = cos(toRadians degreesX * 0.5)
        let c2 = cos(toRadians degreesY * 0.5)
        let c3 = cos(toRadians degreesZ * 0.5)
        let s1 = sin(toRadians degreesX * 0.5)
        let s2 = sin(toRadians degreesY * 0.5)
        let s3 = sin(toRadians degreesZ * 0.5)
        Quaternion.createUnchecked(  s1 * c2 * c3 + c1 * s2 * s3
                                  ,  c1 * s2 * c3 - s1 * c2 * s3
                                  ,  c1 * c2 * s3 + s1 * s2 * c3
                                  ,  c1 * c2 * c3 - s1 * s2 * s3
                                  )

    /// Angles are given in Degrees,
    /// The order in which to apply rotations is Y-X-Z,
    /// which means that the object will first be rotated around its Y-axis,
    /// then its X-axis and finally its Z-axis.
    /// This uses intrinsic Tait-Bryan angles.
    /// This means that rotations are performed with respect to the local coordinate system.
    /// That is, for order Y-X-Z, the rotation is first around the local-Y-axis (which is the same as the World-Y-axis),
    /// then around local-X (which may now be different from the World X-axis),
    /// then local-Z (which may be different from the World Z-axis)
    static member createFromEulerYXZ(degreesY, degreesX, degreesZ) =
        let c1 = cos(toRadians degreesX * 0.5)
        let c2 = cos(toRadians degreesY * 0.5)
        let c3 = cos(toRadians degreesZ * 0.5)
        let s1 = sin(toRadians degreesX * 0.5)
        let s2 = sin(toRadians degreesY * 0.5)
        let s3 = sin(toRadians degreesZ * 0.5)
        Quaternion.createUnchecked(  s1 * c2 * c3 + c1 * s2 * s3
                                  ,  c1 * s2 * c3 - s1 * c2 * s3
                                  ,  c1 * c2 * s3 - s1 * s2 * c3
                                  ,  c1 * c2 * c3 + s1 * s2 * s3
                                  )


    /// Angles are given in Degrees,
    /// The order in which to apply rotations is Z-X-Y,
    /// which means that the object will first be rotated around its Z-axis,
    /// then its X-axis finally its Y-axis.
    /// This uses intrinsic Tait-Bryan angles.
    /// This means that rotations are performed with respect to the local coordinate system.
    /// That is, for order Z-X-Y, the rotation is first around the local-Z-axis (which is the same as the World-Z-axis),
    /// then around local-X (which may now be different from the World X-axis),
    /// then local-Y (which may be different from the World Y-axis)
    static member createFromEulerZXY(degreesZ, degreesX, degreesY) =
        let c1 = cos(toRadians degreesX * 0.5)
        let c2 = cos(toRadians degreesY * 0.5)
        let c3 = cos(toRadians degreesZ * 0.5)
        let s1 = sin(toRadians degreesX * 0.5)
        let s2 = sin(toRadians degreesY * 0.5)
        let s3 = sin(toRadians degreesZ * 0.5)
        Quaternion.createUnchecked(  s1 * c2 * c3 - c1 * s2 * s3
                                  ,  c1 * s2 * c3 + s1 * c2 * s3
                                  ,  c1 * c2 * s3 + s1 * s2 * c3
                                  ,  c1 * c2 * c3 - s1 * s2 * s3)


    /// Angles are given in Degrees,
    /// The order in which to apply rotations is Z-Y-X,
    /// which means that the object will first be rotated around its Z-axis,
    /// then its Y-axis and finally its X-axis.
    /// This uses intrinsic Tait-Bryan angles.
    /// This means that rotations are performed with respect to the local coordinate system.
    /// That is, for order Z-Y-X, the rotation is first around the local Z-axis (which is the same as the World Z-axis),
    /// then around local-Y (which may now be different from the World Y-axis),
    /// then local-X (which may be different from the World X-axis)
    static member createFromEulerZYX(degreesZ, degreesY, degreesX) =
        let c1 = cos(toRadians degreesX * 0.5)
        let c2 = cos(toRadians degreesY * 0.5)
        let c3 = cos(toRadians degreesZ * 0.5)
        let s1 = sin(toRadians degreesX * 0.5)
        let s2 = sin(toRadians degreesY * 0.5)
        let s3 = sin(toRadians degreesZ * 0.5)
        Quaternion.createUnchecked(  s1 * c2 * c3 - c1 * s2 * s3
                                  ,  c1 * s2 * c3 + s1 * c2 * s3
                                  ,  c1 * c2 * s3 - s1 * s2 * c3
                                  ,  c1 * c2 * c3 + s1 * s2 * s3)

    /// Angles are given in Degrees,
    /// The order in which to apply rotations is Y-Z-X,
    /// which means that the object will first be rotated around its Y-axis,
    /// then its Z-axis and finally its X-axis.
    /// This uses intrinsic Tait-Bryan angles.
    /// This means that rotations are performed with respect to the local coordinate system.
    /// That is, for order Y-Z-X, the rotation is first around the local-Y-axis (which is the same as the World-Y-axis),
    /// then around local-Z (which may now be different from the World Z-axis),
    /// then local-X (which may be different from the World X-axis)
    static member createFromEulerYZX(degreesY, degreesZ, degreesX) =
        let c1 = cos(toRadians degreesX * 0.5)
        let c2 = cos(toRadians degreesY * 0.5)
        let c3 = cos(toRadians degreesZ * 0.5)
        let s1 = sin(toRadians degreesX * 0.5)
        let s2 = sin(toRadians degreesY * 0.5)
        let s3 = sin(toRadians degreesZ * 0.5)
        Quaternion.createUnchecked(  s1 * c2 * c3 + c1 * s2 * s3
                                ,  c1 * s2 * c3 + s1 * c2 * s3
                                ,  c1 * c2 * s3 - s1 * s2 * c3
                                ,  c1 * c2 * c3 - s1 * s2 * s3)

    /// Angles are given in Degrees,
    /// The order in which to apply rotations is X-Z-Y,
    /// which means that the object will first be rotated around its X-axis,
    /// then its Z-axis and finally its Y-axis.
    /// This uses intrinsic Tait-Bryan angles.
    /// This means that rotations are performed with respect to the local coordinate system.
    /// That is, for order X-Z-Y, the rotation is first around the local-X-axis (which is the same as the World-X-axis),
    /// then around local-Z (which may now be different from the World Z-axis),
    /// then local-Y (which may be different from the World Y-axis)
    static member createFromEulerXZY(degreesX, degreesZ, degreesY) =
        let c1 = cos(toRadians degreesX * 0.5)
        let c2 = cos(toRadians degreesY * 0.5)
        let c3 = cos(toRadians degreesZ * 0.5)
        let s1 = sin(toRadians degreesX * 0.5)
        let s2 = sin(toRadians degreesY * 0.5)
        let s3 = sin(toRadians degreesZ * 0.5)
        Quaternion.createUnchecked(  s1 * c2 * c3 - c1 * s2 * s3
                                  ,  c1 * s2 * c3 - s1 * c2 * s3
                                  ,  c1 * c2 * s3 + s1 * s2 * c3
                                  ,  c1 * c2 * c3 + s1 * s2 * s3)



    /// The quaternion expresses a relationship between two coordinate frames, A and B say.
    /// Returns the EulerAngles in Degrees: Alpha, Beta, Gamma.
    /// This relationship, if expressed using Euler angles, is as follows:
    /// 1) Rotate Frame A about its Z-axis by angle Gamma;
    /// 2) Rotate the resulting frame about its (new) Y-axis by angle Beta;
    /// 3) Rotate the resulting frame about its (new) X-axis by angle Alpha, to arrive at frame B.
    /// Returns the angles in Degrees as triple. For rotating first around the axis Z then local Y and finally local X.
    /// Note: This conversion may encounter gimbal lock issues when Beta is near ±90°.
    /// see Quaternion.createFromEulerZYX(z, y, x)
    static member toEulerAnglesZYX(q:Quaternion) : float*float*float =
        // from https://github.com/mathnet/mathnet-spatial/blob/master/src/Spatial/Euclidean/Quaternion.cs#L491
        toDegrees <| Math.Atan2( 2.0 * (q.W*q.Z + q.X*q.Y),  q.W*q.W + q.X*q.X - q.Y*q.Y - q.Z*q.Z)
        ,
        toDegrees <|  asinSafe (2.0 * (q.W*q.Y - q.X*q.Z))
        ,
        toDegrees  <| Math.Atan2(2.0 * (q.W*q.X + q.Y*q.Z),  q.W*q.W + q.Z*q.Z - q.X*q.X - q.Y*q.Y)


    // ----------------------------------------------
    // operators for Quaternion multiplication:
    // ----------------------------------------------


    /// Rotate by Quaternion around Origin.
    /// Multiplies (or applies) a Quaternion to a 3D vector.
    static member inline ( *** ) ( v:Vec, q:Quaternion) =
        // adapted from https://github.com/mrdoob/three.js/blob/dev/src/math/Vector3.js
        let x = v.X
        let y = v.Y
        let z = v.Z
        let qx = q.X
        let qy = q.Y
        let qz = q.Z
        let qw = q.W
        // t = 2 * cross(q.xyz, v);
        let tx = 2.0 * (qy * z - qz * y)
        let ty = 2.0 * (qz * x - qx * z)
        let tz = 2.0 * (qx * y - qy * x)
        // v + q.w * t + cross(q.xyz, t);
        Vec    ( x + qw * tx + qy * tz - qz * ty,
                 y + qw * ty + qz * tx - qx * tz,
                 z + qw * tz + qx * ty - qy * tx)

    /// Rotate by Quaternion around Origin.
    /// Multiplies (or applies) a Quaternion to a 3D unit-vector.
    static member inline ( *** ) (v:UnitVec, q:Quaternion) =
        // adapted from https://github.com/mrdoob/three.js/blob/dev/src/math/Vector3.js
        let x = v.X
        let y = v.Y
        let z = v.Z
        let qx = q.X
        let qy = q.Y
        let qz = q.Z
        let qw = q.W
        // t = 2 * cross( q.xyz, v);
        let tx = 2.0 * ( qy * z - qz * y)
        let ty = 2.0 * ( qz * x - qx * z)
        let tz = 2.0 * ( qx * y - qy * x)
        // v + q.w * t + cross( q.xyz, t);
        UnitVec.createUnchecked( x + qw * tx + qy * tz - qz * ty,
                                 y + qw * ty + qz * tx - qx * tz,
                                 z + qw * tz + qx * ty - qy * tx)


    /// Rotate by Quaternion around Origin.
    /// Multiplies (or applies) a Quaternion to a 3D point.
    static member inline ( *** ) ( p:Pnt, q:Quaternion) =
        // adapted from https://github.com/mrdoob/three.js/blob/dev/src/math/Vector3.js
        let x = p.X
        let y = p.Y
        let z = p.Z
        let qx = q.X
        let qy = q.Y
        let qz = q.Z
        let qw = q.W
        // t = 2 * cross( q.xyz, v);
        let tx = 2.0 * ( qy * z - qz * y)
        let ty = 2.0 * ( qz * x - qx * z)
        let tz = 2.0 * ( qx * y - qy * x)
        // v + q.w * t + cross( q.xyz, t);
        Pnt( x + qw * tx + qy * tz - qz * ty,
             y + qw * ty + qz * tx - qx * tz,
             z + qw * tz + qx * ty - qy * tx)


    //static member inline ( *** ) ( p:Line3D, q:Quaternion) = // defined in Line3D.fs

    //static member inline slerp (start:Quaternion, ende:Quaternion, rel) = start.Slerp(ende, rel)
