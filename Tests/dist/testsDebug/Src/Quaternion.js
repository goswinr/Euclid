
import { float } from "./Format.js";
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { fail, failQuat } from "./EuclidErrors.js";
import { Vec_$ctor_Z7AD9E565 } from "./Vec.js";
import { interpolate } from "../fable_modules/fable-library-js.5.0.0/String.js";
import { UnitVec_$ctor_Z7AD9E565 } from "./UnitVec.js";

/**
 * A struct containing 4 floats, representing an immutable unitized Quaternion, for arbitrary 3D rotations.
 * This implementation guarantees the Quaternion to be always unitized.
 * Note: Never use the struct default constructor Quaternion() as it will create an invalid zero length Quaternion.
 * Use Quaternion.create or Quaternion.createUnchecked instead.
 */
export class Quaternion extends Record {
    constructor(X, Y, Z, W) {
        super();
        this.X = X;
        this.Y = Y;
        this.Z = Z;
        this.W = W;
    }
    /**
     * Format Quaternion into string also showing angle in Degree as nicely formatted floating point number.
     */
    toString() {
        let d, x_1;
        const q = this;
        const ang = 57.29577951308232 * (2 * ((d = ((x_1 = q.W, (x_1 > -1) ? ((x_1 < 1) ? x_1 : 1) : -1)), Math.acos(d))));
        return `Euclid.Quaternion(X=${float(q.X)}|Y=${float(q.Y)}|Z=${float(q.Z)}, W=${float(q.W)}| angle: ${float(ang)}°)`;
    }
}

export function Quaternion_$reflection() {
    return class_type("Euclid.Quaternion", undefined, Quaternion, class_type("System.ValueType"));
}

/**
 * Unsafe internal constructor, public only for inlining.
 */
export function Quaternion_$ctor_77D16AC0(x, y, z, w) {
    let x_2;
    const l = (((x * x) + (y * y)) + (z * z)) + (w * w);
    if (!((x_2 = l, (0.999999 < x_2) && (x_2 < 1.000001)))) {
        failQuat(w, x, y, z);
    }
    return new Quaternion(x, y, z, w);
}

/**
 * Format Quaternion into string also showing angle in Degree as nicely formatted floating point number.
 * Using nice floating point number formatting.
 * But without full type name as in q.ToString()
 */
export function Quaternion__get_AsString(q) {
    let d, x_1;
    const ang = 57.29577951308232 * (2 * ((d = ((x_1 = q.W, (x_1 > -1) ? ((x_1 < 1) ? x_1 : 1) : -1)), Math.acos(d))));
    return `X=${float(q.X)}|Y=${float(q.Y)}|Z=${float(q.Z)}, angle: ${float(ang)}°`;
}

/**
 * Format Quaternion into an F# code string that can be used to recreate the quaternion.
 */
export function Quaternion__get_AsFSharpCode(q) {
    return `Quaternion.create(${q.X}, ${q.Y}, ${q.Z}, ${q.W})`;
}

export function Quaternion__get_Magnitude(q) {
    return Math.sqrt((((q.X * q.X) + (q.Y * q.Y)) + (q.Z * q.Z)) + (q.W * q.W));
}

/**
 * Get a new Quaternion that rotates around the same axis but with a different angle. In Radians.
 * Fails for identity or near-identity quaternions where the rotation axis is not defined (angle ≈ 0).
 */
export function Quaternion__setAngleInRadians_5E38073B(q, angleInRadians) {
    let q_1;
    const length = Math.sqrt(((q.X * q.X) + (q.Y * q.Y)) + (q.Z * q.Z));
    if (!(length > 1E-12)) {
        fail(`Quaternion.setAngleInRadians failed. The length of the axis is too short: ${(q_1 = q, Vec_$ctor_Z7AD9E565(q_1.X, q_1.Y, q_1.Z))}`);
    }
    const sc = 1 / length;
    const angHalf = angleInRadians * 0.5;
    const sa = sc * Math.sin(angHalf);
    return Quaternion_$ctor_77D16AC0(q.X * sa, q.Y * sa, q.Z * sa, Math.cos(angHalf));
}

/**
 * Multiply two Quaternions. It's like adding one rotation to the other.
 * Note: Repeated multiplications may accumulate floating-point errors that violate the unit-length invariant.
 * Use q.Normalize() to renormalize if combining many rotations.
 */
export function Quaternion_multiply_400F31E0(l, r) {
    return Quaternion_$ctor_77D16AC0((((l.W * r.X) + (l.X * r.W)) + (l.Y * r.Z)) - (l.Z * r.Y), (((l.W * r.Y) + (l.Y * r.W)) + (l.Z * r.X)) - (l.X * r.Z), (((l.W * r.Z) + (l.Z * r.W)) + (l.X * r.Y)) - (l.Y * r.X), (((l.W * r.W) - (l.X * r.X)) - (l.Y * r.Y)) - (l.Z * r.Z));
}

/**
 * This constructor does unitizing too.
 */
export function Quaternion_create_77D16AC0(x, y, z, w) {
    const l = Math.sqrt((((x * x) + (y * y)) + (z * z)) + (w * w));
    if (!(Math.abs(l) > 1E-12)) {
        fail(`Quaternion create failed for x:${x}, y:${y}, z:${z}, w:${w}. The length needs to be bigger than zero.`);
    }
    const sc = 1 / l;
    return Quaternion_$ctor_77D16AC0(x * sc, y * sc, z * sc, w * sc);
}

/**
 * The created rotation is Clockwise looking in the direction of the vector.
 * The vector may be of any length but zero.
 */
export function Quaternion_createFromRadians_Z3D1F83EE(axis, angleInRadians) {
    const length = Math.sqrt(((axis.X * axis.X) + (axis.Y * axis.Y)) + (axis.Z * axis.Z));
    if (!(length > 1E-12)) {
        fail(`Quaternion.createFromRadians failed too short axis: ${interpolate("%O%P()", [axis])} and rotation: ${interpolate("%g%P()", [57.29577951308232 * angleInRadians])}° Degrees.`);
    }
    const angHalf = angleInRadians * 0.5;
    const sa = Math.sin(angHalf);
    const sc = 1 / length;
    return Quaternion_$ctor_77D16AC0((axis.X * sc) * sa, (axis.Y * sc) * sa, (axis.Z * sc) * sa, Math.cos(angHalf));
}

/**
 * Creates a rotation from one vector's direction to another vector's direction.
 * If the tips of the two vectors (unitized) are closer than 1e-12 (squared: 1e-24) then an identity Quaternion is returned.
 * If the tips of the two vectors (unitized) are almost exactly opposite (sum length too small),
 * there is no valid unique 180 degree rotation that can be found, so an exception is raised.
 */
export function Quaternion_createVecToVec_5A694120(vecFrom, vecTo) {
    let v_1, v_3, a_2, b_2;
    let fu;
    const x = vecFrom.X;
    const y = vecFrom.Y;
    const z = vecFrom.Z;
    const length = Math.sqrt(((x * x) + (y * y)) + (z * z));
    if (!(length > 1E-12)) {
        fail(`Quaternion.createVecToVec failed. The vector is too short: vecFrom: ${interpolate("%O%P()", [vecFrom])}`);
    }
    const sc = 1 / length;
    fu = UnitVec_$ctor_Z7AD9E565(x * sc, y * sc, z * sc);
    let tu;
    const x_3 = vecTo.X;
    const y_2 = vecTo.Y;
    const z_2 = vecTo.Z;
    const length_1 = Math.sqrt(((x_3 * x_3) + (y_2 * y_2)) + (z_2 * z_2));
    if (!(length_1 > 1E-12)) {
        fail(`Quaternion.createVecToVec failed. The vector is too short: vecTo: ${interpolate("%O%P()", [vecTo])}`);
    }
    const sc_1 = 1 / length_1;
    tu = UnitVec_$ctor_Z7AD9E565(x_3 * sc_1, y_2 * sc_1, z_2 * sc_1);
    let v;
    const a = fu;
    const b = tu;
    v = Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
    if (((v_1 = v, ((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z))) < 1E-24) {
        return Quaternion_$ctor_77D16AC0(0, 0, 0, 1);
    }
    else {
        let v_2;
        const a_1 = fu;
        const b_1 = tu;
        v_2 = Vec_$ctor_Z7AD9E565(a_1.X + b_1.X, a_1.Y + b_1.Y, a_1.Z + b_1.Z);
        if (!(((v_3 = v_2, ((v_3.X * v_3.X) + (v_3.Y * v_3.Y)) + (v_3.Z * v_3.Z))) > 1E-12)) {
            fail(`Quaternion.createVecToVec failed to find a rotation axis for (almost) colinear  (or NaN) vectors in opposite directions: ${interpolate("%O%P()", [vecFrom])} and ${interpolate("%O%P()", [vecTo])}`);
        }
        return Quaternion_create_77D16AC0((fu.Y * tu.Z) - (fu.Z * tu.Y), (fu.Z * tu.X) - (fu.X * tu.Z), (fu.X * tu.Y) - (fu.Y * tu.X), ((a_2 = fu, (b_2 = tu, ((a_2.X * b_2.X) + (a_2.Y * b_2.Y)) + (a_2.Z * b_2.Z)))) + 1);
    }
}

/**
 * Angles are given in Degrees,
 * The order in which to apply rotations is X-Y-Z,
 * which means that the object will first be rotated around its X-axis,
 * then its Y-axis and finally its Z-axis.
 * This uses intrinsic Tait-Bryan angles.
 * This means that rotations are performed with respect to the local coordinate system.
 * That is, for order X-Y-Z, the rotation is first around the local-X-axis (which is the same as the World-X-axis),
 * then around local-Y (which may now be different from the World Y-axis),
 * then local-Z (which may be different from the World Z-axis)
 */
export function Quaternion_createFromEulerXYZ_Z7AD9E565(degreesX, degreesY, degreesZ) {
    const c1 = Math.cos((0.017453292519943295 * degreesX) * 0.5);
    const c2 = Math.cos((0.017453292519943295 * degreesY) * 0.5);
    const c3 = Math.cos((0.017453292519943295 * degreesZ) * 0.5);
    const s1 = Math.sin((0.017453292519943295 * degreesX) * 0.5);
    const s2 = Math.sin((0.017453292519943295 * degreesY) * 0.5);
    const s3 = Math.sin((0.017453292519943295 * degreesZ) * 0.5);
    return Quaternion_$ctor_77D16AC0(((s1 * c2) * c3) + ((c1 * s2) * s3), ((c1 * s2) * c3) - ((s1 * c2) * s3), ((c1 * c2) * s3) + ((s1 * s2) * c3), ((c1 * c2) * c3) - ((s1 * s2) * s3));
}

/**
 * Angles are given in Degrees,
 * The order in which to apply rotations is Y-X-Z,
 * which means that the object will first be rotated around its Y-axis,
 * then its X-axis and finally its Z-axis.
 * This uses intrinsic Tait-Bryan angles.
 * This means that rotations are performed with respect to the local coordinate system.
 * That is, for order Y-X-Z, the rotation is first around the local-Y-axis (which is the same as the World-Y-axis),
 * then around local-X (which may now be different from the World X-axis),
 * then local-Z (which may be different from the World Z-axis)
 */
export function Quaternion_createFromEulerYXZ_Z7AD9E565(degreesY, degreesX, degreesZ) {
    const c1 = Math.cos((0.017453292519943295 * degreesX) * 0.5);
    const c2 = Math.cos((0.017453292519943295 * degreesY) * 0.5);
    const c3 = Math.cos((0.017453292519943295 * degreesZ) * 0.5);
    const s1 = Math.sin((0.017453292519943295 * degreesX) * 0.5);
    const s2 = Math.sin((0.017453292519943295 * degreesY) * 0.5);
    const s3 = Math.sin((0.017453292519943295 * degreesZ) * 0.5);
    return Quaternion_$ctor_77D16AC0(((s1 * c2) * c3) + ((c1 * s2) * s3), ((c1 * s2) * c3) - ((s1 * c2) * s3), ((c1 * c2) * s3) - ((s1 * s2) * c3), ((c1 * c2) * c3) + ((s1 * s2) * s3));
}

/**
 * Angles are given in Degrees,
 * The order in which to apply rotations is Z-X-Y,
 * which means that the object will first be rotated around its Z-axis,
 * then its X-axis finally its Y-axis.
 * This uses intrinsic Tait-Bryan angles.
 * This means that rotations are performed with respect to the local coordinate system.
 * That is, for order Z-X-Y, the rotation is first around the local-Z-axis (which is the same as the World-Z-axis),
 * then around local-X (which may now be different from the World X-axis),
 * then local-Y (which may be different from the World Y-axis)
 */
export function Quaternion_createFromEulerZXY_Z7AD9E565(degreesZ, degreesX, degreesY) {
    const c1 = Math.cos((0.017453292519943295 * degreesX) * 0.5);
    const c2 = Math.cos((0.017453292519943295 * degreesY) * 0.5);
    const c3 = Math.cos((0.017453292519943295 * degreesZ) * 0.5);
    const s1 = Math.sin((0.017453292519943295 * degreesX) * 0.5);
    const s2 = Math.sin((0.017453292519943295 * degreesY) * 0.5);
    const s3 = Math.sin((0.017453292519943295 * degreesZ) * 0.5);
    return Quaternion_$ctor_77D16AC0(((s1 * c2) * c3) - ((c1 * s2) * s3), ((c1 * s2) * c3) + ((s1 * c2) * s3), ((c1 * c2) * s3) + ((s1 * s2) * c3), ((c1 * c2) * c3) - ((s1 * s2) * s3));
}

/**
 * Angles are given in Degrees,
 * The order in which to apply rotations is Z-Y-X,
 * which means that the object will first be rotated around its Z-axis,
 * then its Y-axis and finally its X-axis.
 * This uses intrinsic Tait-Bryan angles.
 * This means that rotations are performed with respect to the local coordinate system.
 * That is, for order Z-Y-X, the rotation is first around the local Z-axis (which is the same as the World Z-axis),
 * then around local-Y (which may now be different from the World Y-axis),
 * then local-X (which may be different from the World X-axis)
 */
export function Quaternion_createFromEulerZYX_Z7AD9E565(degreesZ, degreesY, degreesX) {
    const c1 = Math.cos((0.017453292519943295 * degreesX) * 0.5);
    const c2 = Math.cos((0.017453292519943295 * degreesY) * 0.5);
    const c3 = Math.cos((0.017453292519943295 * degreesZ) * 0.5);
    const s1 = Math.sin((0.017453292519943295 * degreesX) * 0.5);
    const s2 = Math.sin((0.017453292519943295 * degreesY) * 0.5);
    const s3 = Math.sin((0.017453292519943295 * degreesZ) * 0.5);
    return Quaternion_$ctor_77D16AC0(((s1 * c2) * c3) - ((c1 * s2) * s3), ((c1 * s2) * c3) + ((s1 * c2) * s3), ((c1 * c2) * s3) - ((s1 * s2) * c3), ((c1 * c2) * c3) + ((s1 * s2) * s3));
}

/**
 * Angles are given in Degrees,
 * The order in which to apply rotations is Y-Z-X,
 * which means that the object will first be rotated around its Y-axis,
 * then its Z-axis and finally its X-axis.
 * This uses intrinsic Tait-Bryan angles.
 * This means that rotations are performed with respect to the local coordinate system.
 * That is, for order Y-Z-X, the rotation is first around the local-Y-axis (which is the same as the World-Y-axis),
 * then around local-Z (which may now be different from the World Z-axis),
 * then local-X (which may be different from the World X-axis)
 */
export function Quaternion_createFromEulerYZX_Z7AD9E565(degreesY, degreesZ, degreesX) {
    const c1 = Math.cos((0.017453292519943295 * degreesX) * 0.5);
    const c2 = Math.cos((0.017453292519943295 * degreesY) * 0.5);
    const c3 = Math.cos((0.017453292519943295 * degreesZ) * 0.5);
    const s1 = Math.sin((0.017453292519943295 * degreesX) * 0.5);
    const s2 = Math.sin((0.017453292519943295 * degreesY) * 0.5);
    const s3 = Math.sin((0.017453292519943295 * degreesZ) * 0.5);
    return Quaternion_$ctor_77D16AC0(((s1 * c2) * c3) + ((c1 * s2) * s3), ((c1 * s2) * c3) + ((s1 * c2) * s3), ((c1 * c2) * s3) - ((s1 * s2) * c3), ((c1 * c2) * c3) - ((s1 * s2) * s3));
}

/**
 * Angles are given in Degrees,
 * The order in which to apply rotations is X-Z-Y,
 * which means that the object will first be rotated around its X-axis,
 * then its Z-axis and finally its Y-axis.
 * This uses intrinsic Tait-Bryan angles.
 * This means that rotations are performed with respect to the local coordinate system.
 * That is, for order X-Z-Y, the rotation is first around the local-X-axis (which is the same as the World-X-axis),
 * then around local-Z (which may now be different from the World Z-axis),
 * then local-Y (which may be different from the World Y-axis)
 */
export function Quaternion_createFromEulerXZY_Z7AD9E565(degreesX, degreesZ, degreesY) {
    const c1 = Math.cos((0.017453292519943295 * degreesX) * 0.5);
    const c2 = Math.cos((0.017453292519943295 * degreesY) * 0.5);
    const c3 = Math.cos((0.017453292519943295 * degreesZ) * 0.5);
    const s1 = Math.sin((0.017453292519943295 * degreesX) * 0.5);
    const s2 = Math.sin((0.017453292519943295 * degreesY) * 0.5);
    const s3 = Math.sin((0.017453292519943295 * degreesZ) * 0.5);
    return Quaternion_$ctor_77D16AC0(((s1 * c2) * c3) - ((c1 * s2) * s3), ((c1 * s2) * c3) - ((s1 * c2) * s3), ((c1 * c2) * s3) + ((s1 * s2) * c3), ((c1 * c2) * c3) + ((s1 * s2) * s3));
}

/**
 * The quaternion expresses a relationship between two coordinate frames, A and B say.
 * Returns the EulerAngles in Degrees: Alpha, Beta, Gamma.
 * This relationship, if expressed using Euler angles, is as follows:
 * 1) Rotate Frame A about its Z-axis by angle Gamma;
 * 2) Rotate the resulting frame about its (new) Y-axis by angle Beta;
 * 3) Rotate the resulting frame about its (new) X-axis by angle Alpha, to arrive at frame B.
 * Returns the angles in Degrees as triple. For rotating first around the axis Z then local Y and finally local X.
 * Note: This conversion may encounter gimbal lock issues when Beta is near ±90°.
 * see Quaternion.createFromEulerZYX(z, y, x)
 */
export function Quaternion_toEulerAnglesZYX_Z2A007687(q) {
    let d, x_1;
    return [57.29577951308232 * Math.atan2(2 * ((q.W * q.Z) + (q.X * q.Y)), (((q.W * q.W) + (q.X * q.X)) - (q.Y * q.Y)) - (q.Z * q.Z)), 57.29577951308232 * ((d = ((x_1 = (2 * ((q.W * q.Y) - (q.X * q.Z))), (x_1 > -1) ? ((x_1 < 1) ? x_1 : 1) : -1)), Math.asin(d))), 57.29577951308232 * Math.atan2(2 * ((q.W * q.X) + (q.Y * q.Z)), (((q.W * q.W) + (q.Z * q.Z)) - (q.X * q.X)) - (q.Y * q.Y))];
}

