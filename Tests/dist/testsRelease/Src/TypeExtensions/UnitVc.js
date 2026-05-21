
import { UnitVc_$ctor_7B00E9A0 } from "../UnitVc.js";
import { fail2, fail } from "../EuclidErrors.js";
import { Vc_$ctor_7B00E9A0 } from "../Vc.js";
import { failUnit2 } from "../EuclidErrors.js";

/**
 * Rotates a vector by a given number of quarter-circles (i.e. multiples of 90
 * degrees or Pi/2 radians). A positive number rotates counter-clockwise, a
 * negative number rotates clockwise. The length of the vector is preserved.
 */
export function Euclid_UnitVc__UnitVc_rotateByQuarterCircle_Static(numberOfQuarters, v) {
    let nQuad = numberOfQuarters % 4;
    if (nQuad < 0) {
        nQuad = ((nQuad + 4) | 0);
    }
    switch (nQuad) {
        case 0:
            return v;
        case 1:
            return UnitVc_$ctor_7B00E9A0(-v.Y, v.X);
        case 2:
            return UnitVc_$ctor_7B00E9A0(-v.X, -v.Y);
        case 3:
            return UnitVc_$ctor_7B00E9A0(v.Y, -v.X);
        default:
            return fail("never happens");
    }
}

/**
 * Check if two 2d unit vectors are not equal within a given tolerance.
 * Use a tolerance of 0.0 to check if the two unit vectors are not exactly equal.
 */
export function Euclid_UnitVc__UnitVc_notEquals_Static(tol, a, b) {
    if (Math.abs(a.X - b.X) > tol) {
        return true;
    }
    else {
        return Math.abs(a.Y - b.Y) > tol;
    }
}

/**
 * Linearly interpolates between two vectors.
 * e.g. rel=0.5 will return the middle vector, rel=1.0 the end vector,
 * rel=1.5 a vector half the distance beyond the end vector.
 */
export function Euclid_UnitVc__UnitVc_lerp_Static_3D21F0FB(start, ende, rel) {
    const a_1 = start;
    let b_1;
    const f = rel;
    let v;
    const a = ende;
    const b = start;
    v = Vc_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y);
    b_1 = Vc_$ctor_7B00E9A0(v.X * f, v.Y * f);
    return Vc_$ctor_7B00E9A0(a_1.X + b_1.X, a_1.Y + b_1.Y);
}

/**
 * Spherically interpolates between start and end by amount rel (0.0 to 1.0).
 * The difference between this and linear interpolation (aka, "lerp") is that the vectors are treated as directions rather than points in space.
 * The direction of the returned vector is interpolated by the angle and its magnitude is interpolated between the magnitudes of start and end.
 * Interpolation continues before and after the range of 0.0 and 1.0
 */
export function Euclid_UnitVc__UnitVc_slerp_Static_3D21F0FB(start, ende, rel) {
    let dot;
    const a = start;
    const b = ende;
    dot = ((a.X * b.X) + (a.Y * b.Y));
    if (dot > 0.9999996192282494) {
        return start;
    }
    else if (dot < -0.9999996192282494) {
        return fail2("UnitVc.slerp vectors are 180 deg opposite.", start, ende);
    }
    else {
        const ang = Math.acos(dot);
        let p;
        const a_2 = ende;
        let b_1;
        const a_1 = start;
        const f = dot;
        b_1 = Vc_$ctor_7B00E9A0(a_1.X * f, a_1.Y * f);
        p = Vc_$ctor_7B00E9A0(a_2.X - b_1.X, a_2.Y - b_1.Y);
        let perp;
        const x = p.X;
        const y = p.Y;
        const l = Math.sqrt((x * x) + (y * y));
        if (!(l > 1E-12)) {
            failUnit2("UnitVc.create", x, y);
        }
        perp = UnitVc_$ctor_7B00E9A0(x / l, y / l);
        const theta360 = ((ang * rel) + 6.283185307179586) % 6.283185307179586;
        const cosine = Math.cos(theta360);
        const sine = Math.sqrt(1 - (cosine * cosine));
        if (theta360 < 3.141592653589793) {
            let v;
            let a_5;
            const a_3 = start;
            const f_1 = cosine;
            a_5 = Vc_$ctor_7B00E9A0(a_3.X * f_1, a_3.Y * f_1);
            let b_2;
            const a_4 = perp;
            const f_2 = sine;
            b_2 = Vc_$ctor_7B00E9A0(a_4.X * f_2, a_4.Y * f_2);
            v = Vc_$ctor_7B00E9A0(a_5.X + b_2.X, a_5.Y + b_2.Y);
            return UnitVc_$ctor_7B00E9A0(v.X, v.Y);
        }
        else {
            let v_1;
            let a_8;
            const a_6 = start;
            const f_3 = cosine;
            a_8 = Vc_$ctor_7B00E9A0(a_6.X * f_3, a_6.Y * f_3);
            let b_3;
            const a_7 = perp;
            const f_4 = sine;
            b_3 = Vc_$ctor_7B00E9A0(a_7.X * f_4, a_7.Y * f_4);
            v_1 = Vc_$ctor_7B00E9A0(a_8.X - b_3.X, a_8.Y - b_3.Y);
            return UnitVc_$ctor_7B00E9A0(v_1.X, v_1.Y);
        }
    }
}

export function Euclid_UnitVc__UnitVc_intersection_Static_54248720(ptA, ptB, vA, vB) {
    let vAx, vAy, vBx, vBy, det, dx, dy;
    return (vAx = ptB.X, (vAy = ptB.Y, (vBx = vB.X, (vBy = vB.Y, (det = ((vAx * vBy) - (vAy * vBx)), (dx = (vA.X - ptA.X), (dy = (vA.Y - ptA.Y), [((dx * vBy) - (dy * vBx)) / det, ((dx * vAy) - (dy * vAx)) / det])))))));
}

