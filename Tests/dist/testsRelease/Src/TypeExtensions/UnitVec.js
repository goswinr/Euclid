
import { UnitVec_$ctor_Z7AD9E565 } from "../UnitVec.js";
import { Vec_$ctor_Z7AD9E565 } from "../Vec.js";
import { fail2 } from "../EuclidErrors.js";
import { failUnit3 } from "../EuclidErrors.js";

/**
 * Check if two 3D unit-vectors  are not equal within a given tolerance.
 * Use a tolerance of 0.0 to check if the two unit-vectors  are not exactly equal.
 */
export function Euclid_UnitVec__UnitVec_notEquals_Static(tol, a, b) {
    if ((Math.abs(a.X - b.X) > tol) ? true : (Math.abs(a.Y - b.Y) > tol)) {
        return true;
    }
    else {
        return Math.abs(a.Z - b.Z) > tol;
    }
}

/**
 * Rotates a 3D unit-vector around the Z-axis by a given number of quarter-circles (i.e. multiples of 90
 * degrees or Pi/2 radians). A positive number rotates counter-clockwise, a
 * negative number rotates clockwise. The length of the vector is preserved.
 */
export function Euclid_UnitVec__UnitVec_rotateByQuarterCircle_Static(numberOfQuarters, v) {
    let nQuad = numberOfQuarters % 4;
    if (nQuad < 0) {
        nQuad = ((nQuad + 4) | 0);
    }
    switch (nQuad) {
        case 0:
            return v;
        case 1:
            return UnitVec_$ctor_Z7AD9E565(-v.Y, v.X, v.Z);
        case 2:
            return UnitVec_$ctor_Z7AD9E565(-v.X, -v.Y, v.Z);
        case 3:
            return UnitVec_$ctor_Z7AD9E565(v.Y, -v.X, v.Z);
        default:
            return v;
    }
}

/**
 * Linearly interpolates between two vectors.
 * e.g. rel=0.5 will return the middle vector, rel=1.0 the end vector,
 * rel=1.5 a vector half the distance beyond the end vector.
 */
export function Euclid_UnitVec__UnitVec_lerp_Static_Z679E3AE5(start, ende, rel) {
    const a_2 = start;
    let b_1;
    const f = rel;
    let a_1;
    const a = ende;
    const b = start;
    a_1 = Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
    b_1 = Vec_$ctor_Z7AD9E565(a_1.X * f, a_1.Y * f, a_1.Z * f);
    return Vec_$ctor_Z7AD9E565(a_2.X + b_1.X, a_2.Y + b_1.Y, a_2.Z + b_1.Z);
}

/**
 * Spherically interpolates between start and end by amount rel (0.0 to 1.0).
 * The difference between this and linear interpolation (aka, "lerp") is that the vectors are treated as directions rather than points in space.
 * The direction of the returned vector is interpolated by the angle and its magnitude is interpolated between the magnitudes of start and end.
 * Interpolation continues before and after the range of 0.0 and 1.0.
 */
export function Euclid_UnitVec__UnitVec_slerp_Static_Z679E3AE5(start, ende, rel) {
    let dot;
    const a = start;
    const b = ende;
    dot = (((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z));
    if (dot > 0.9999996192282494) {
        return start;
    }
    else if (dot < -0.9999996192282494) {
        return fail2("UnitVec.slerp vectors are 180 deg opposite.", start, ende);
    }
    else {
        const ang = Math.acos(dot);
        let p;
        const a_2 = ende;
        let b_1;
        const a_1 = start;
        const f = dot;
        b_1 = Vec_$ctor_Z7AD9E565(a_1.X * f, a_1.Y * f, a_1.Z * f);
        p = Vec_$ctor_Z7AD9E565(a_2.X - b_1.X, a_2.Y - b_1.Y, a_2.Z - b_1.Z);
        let perp;
        const x = p.X;
        const y = p.Y;
        const z = p.Z;
        const l = Math.sqrt(((x * x) + (y * y)) + (z * z));
        if (!(l > 1E-12)) {
            failUnit3("UnitVec.create", x, y, z);
        }
        const li = 1 / l;
        perp = UnitVec_$ctor_Z7AD9E565(li * x, li * y, li * z);
        const theta360 = ((ang * rel) + 6.283185307179586) % 6.283185307179586;
        const cosine = Math.cos(theta360);
        const sine = Math.sqrt(1 - (cosine * cosine));
        let vx;
        const a_3 = start;
        const f_1 = cosine;
        vx = Vec_$ctor_Z7AD9E565(a_3.X * f_1, a_3.Y * f_1, a_3.Z * f_1);
        let vy;
        const a_4 = perp;
        const f_2 = sine;
        vy = Vec_$ctor_Z7AD9E565(a_4.X * f_2, a_4.Y * f_2, a_4.Z * f_2);
        if (theta360 < 3.141592653589793) {
            let v;
            const a_5 = vx;
            const b_2 = vy;
            v = Vec_$ctor_Z7AD9E565(a_5.X + b_2.X, a_5.Y + b_2.Y, a_5.Z + b_2.Z);
            return UnitVec_$ctor_Z7AD9E565(v.X, v.Y, v.Z);
        }
        else {
            let v_1;
            const a_6 = vx;
            const b_3 = vy;
            v_1 = Vec_$ctor_Z7AD9E565(a_6.X - b_3.X, a_6.Y - b_3.Y, a_6.Z - b_3.Z);
            return UnitVec_$ctor_Z7AD9E565(v_1.X, v_1.Y, v_1.Z);
        }
    }
}

