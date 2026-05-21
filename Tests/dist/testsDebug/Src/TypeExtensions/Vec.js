
import { Vec_$ctor_Z7AD9E565 } from "../Vec.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_1 } from "../Vec.js";
import { failUnit3, fail2, failTooSmall } from "../EuclidErrors.js";
import { UnitVec_$ctor_Z7AD9E565 } from "../UnitVec.js";

/**
 * Check if two 3D vectors  are not equal within a given tolerance.
 * Use a tolerance of 0.0 to check if the two vectors are not exactly equal.
 */
export function Euclid_Vec__Vec_notEquals_Static(tol, a, b) {
    if ((Math.abs(a.X - b.X) > tol) ? true : (Math.abs(a.Y - b.Y) > tol)) {
        return true;
    }
    else {
        return Math.abs(a.Z - b.Z) > tol;
    }
}

/**
 * Rotate the 3D vector around X-axis, from Y to Z-axis, Counter Clockwise looking from right.
 */
export function Euclid_Vec__Vec_rotateXBy_Static(r, v) {
    return Vec_$ctor_Z7AD9E565(v.X, (r.Cos * v.Y) - (r.Sin * v.Z), (r.Sin * v.Y) + (r.Cos * v.Z));
}

/**
 * Rotate the 3D vector around Y-axis, from Z to X-axis, Counter Clockwise looking from back.
 */
export function Euclid_Vec__Vec_rotateYBy_Static(r, v) {
    return Vec_$ctor_Z7AD9E565((r.Sin * v.Z) + (r.Cos * v.X), v.Y, (r.Cos * v.Z) - (r.Sin * v.X));
}

/**
 * Rotate the 3D vector around Z-axis, from X to Y-axis, Counter Clockwise looking from top.
 */
export function Euclid_Vec__Vec_rotateZBy_Static(r, v) {
    return Vec_$ctor_Z7AD9E565((r.Cos * v.X) - (r.Sin * v.Y), (r.Sin * v.X) + (r.Cos * v.Y), v.Z);
}

/**
 * Rotates a 3D vector around the Z-axis by a given number of quarter-circles (i.e. multiples of 90
 * degrees or Pi/2 radians). A positive number rotates counter-clockwise, a
 * negative number rotates clockwise. The length of the vector is preserved.
 */
export function Euclid_Vec__Vec_rotateByQuarterCircle_Static(numberOfQuarters, v) {
    let nQuad = numberOfQuarters % 4;
    if (nQuad < 0) {
        nQuad = ((nQuad + 4) | 0);
    }
    switch (nQuad) {
        case 0:
            return v;
        case 1:
            return Vec_$ctor_Z7AD9E565(-v.Y, v.X, v.Z);
        case 2:
            return Vec_$ctor_Z7AD9E565(-v.X, -v.Y, v.Z);
        case 3:
            return Vec_$ctor_Z7AD9E565(v.Y, -v.X, v.Z);
        default:
            return Vec_$ctor_Z7AD9E565(1, 0, 0);
    }
}

/**
 * Linearly interpolates between two vectors.
 * e.g. rel=0.5 will return the middle vector, rel=1.0 the end vector,
 * rel=1.5 a vector half the distance beyond the end vector.
 */
export function Euclid_Vec__Vec_lerp_Static_Z6569DE5(start, ende, rel) {
    const a_2 = start;
    let b_1;
    const f = rel;
    let a_1;
    const a = ende;
    const b = start;
    a_1 = Vec_$ctor_Z7AD9E565_1(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
    b_1 = Vec_$ctor_Z7AD9E565_1(a_1.X * f, a_1.Y * f, a_1.Z * f);
    return Vec_$ctor_Z7AD9E565_1(a_2.X + b_1.X, a_2.Y + b_1.Y, a_2.Z + b_1.Z);
}

/**
 * Spherically interpolates between start and end by amount rel (0.0 to 1.0).
 * The difference between this and linear interpolation (aka, "lerp") is that the vectors are treated as directions rather than points in space.
 * The direction of the returned vector is interpolated by the angle and its magnitude is interpolated between the magnitudes of start and end.
 * Interpolation continues before and after the range of 0.0 and 1.0.
 */
export function Euclid_Vec__Vec_slerp_Static_Z6569DE5(start, ende, rel) {
    let sLen;
    const v = start;
    sLen = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    let eLen;
    const v_1 = ende;
    eLen = Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
    if (!(sLen > 1E-12)) {
        failTooSmall("Vec.slerp", start);
    }
    if (!(eLen > 1E-12)) {
        failTooSmall("Vec.slerp", ende);
    }
    const fs = 1 / sLen;
    const fe = 1 / eLen;
    let su;
    const a = start;
    const f = fs;
    su = Vec_$ctor_Z7AD9E565_1(a.X * f, a.Y * f, a.Z * f);
    let eu;
    const a_1 = ende;
    const f_1 = fe;
    eu = Vec_$ctor_Z7AD9E565_1(a_1.X * f_1, a_1.Y * f_1, a_1.Z * f_1);
    let dot;
    const a_2 = su;
    const b = eu;
    dot = (((a_2.X * b.X) + (a_2.Y * b.Y)) + (a_2.Z * b.Z));
    if (dot > 0.9999996192282494) {
        return Euclid_Vec__Vec_lerp_Static_Z6569DE5(start, ende, rel);
    }
    else if (dot < -0.9999996192282494) {
        return fail2("Vec.slerp vectors are 180 deg opposite.", start, ende);
    }
    else {
        const ang = Math.acos(dot);
        let perp;
        let v_3;
        const a_4 = eu;
        let b_1;
        const a_3 = su;
        const f_2 = dot;
        b_1 = Vec_$ctor_Z7AD9E565_1(a_3.X * f_2, a_3.Y * f_2, a_3.Z * f_2);
        v_3 = Vec_$ctor_Z7AD9E565_1(a_4.X - b_1.X, a_4.Y - b_1.Y, a_4.Z - b_1.Z);
        const x_2 = v_3.X;
        const y = v_3.Y;
        const z = v_3.Z;
        const l = Math.sqrt(((x_2 * x_2) + (y * y)) + (z * z));
        if (!(l > 1E-12)) {
            failUnit3("Vec.unitize", x_2, y, z);
        }
        const f_3 = 1 / l;
        perp = UnitVec_$ctor_Z7AD9E565(f_3 * x_2, f_3 * y, f_3 * z);
        const theta = ang * rel;
        const theta360 = (theta + 6.283185307179586) % 6.283185307179586;
        const cosine = Math.cos(theta360);
        const sine = Math.sqrt(1 - (cosine * cosine));
        let res;
        if (theta360 < 3.141592653589793) {
            let a_7;
            const a_5 = su;
            const f_4 = cosine;
            a_7 = Vec_$ctor_Z7AD9E565_1(a_5.X * f_4, a_5.Y * f_4, a_5.Z * f_4);
            let b_2;
            const a_6 = perp;
            const f_5 = sine;
            b_2 = Vec_$ctor_Z7AD9E565_1(a_6.X * f_5, a_6.Y * f_5, a_6.Z * f_5);
            res = Vec_$ctor_Z7AD9E565_1(a_7.X + b_2.X, a_7.Y + b_2.Y, a_7.Z + b_2.Z);
        }
        else {
            let a_10;
            const a_8 = su;
            const f_6 = cosine;
            a_10 = Vec_$ctor_Z7AD9E565_1(a_8.X * f_6, a_8.Y * f_6, a_8.Z * f_6);
            let b_3;
            const a_9 = perp;
            const f_7 = sine;
            b_3 = Vec_$ctor_Z7AD9E565_1(a_9.X * f_7, a_9.Y * f_7, a_9.Z * f_7);
            res = Vec_$ctor_Z7AD9E565_1(a_10.X - b_3.X, a_10.Y - b_3.Y, a_10.Z - b_3.Z);
        }
        const lenRel = sLen + (rel * (eLen - sLen));
        if (lenRel < 0) {
            return Vec_$ctor_Z7AD9E565_1(0, 0, 0);
        }
        else {
            const a_11 = res;
            const f_8 = Math.abs(lenRel);
            return Vec_$ctor_Z7AD9E565_1(a_11.X * f_8, a_11.Y * f_8, a_11.Z * f_8);
        }
    }
}

