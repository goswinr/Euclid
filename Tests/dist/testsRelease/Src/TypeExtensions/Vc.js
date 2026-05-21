
import { Vc_$ctor_7B00E9A0 } from "../Vc.js";
import { Vc_$ctor_7B00E9A0 as Vc_$ctor_7B00E9A0_1 } from "../Vc.js";
import { failUnit2, fail2, failTooSmall2 } from "../EuclidErrors.js";
import { UnitVc_$ctor_7B00E9A0 } from "../UnitVc.js";

/**
 * Check if two 2D vectors  are not equal within a given tolerance.
 * Use a tolerance of 0.0 to check if the two vectors are not exactly equal.
 */
export function Euclid_Vc__Vc_notEquals_Static(tol, a, b) {
    if (Math.abs(a.X - b.X) > tol) {
        return true;
    }
    else {
        return Math.abs(a.Y - b.Y) > tol;
    }
}

/**
 * Rotates a vector by a given number of quarter-circles (i.e. multiples of 90
 * degrees or Pi/2 radians). A positive number rotates counter-clockwise, a
 * negative number rotates clockwise. The length of the vector is preserved.
 */
export function Euclid_Vc__Vc_rotateByQuarterCircle_Static(numberOfQuarters, v) {
    let nQuad = numberOfQuarters % 4;
    if (nQuad < 0) {
        nQuad = ((nQuad + 4) | 0);
    }
    switch (nQuad) {
        case 0:
            return v;
        case 1:
            return Vc_$ctor_7B00E9A0(-v.Y, v.X);
        case 2:
            return Vc_$ctor_7B00E9A0(-v.X, -v.Y);
        case 3:
            return Vc_$ctor_7B00E9A0(v.Y, -v.X);
        default:
            return v;
    }
}

/**
 * Linearly interpolates between two vectors.
 * e.g. rel=0.5 will return the middle vector, rel=1.0 the end vector,
 * rel=1.5 a vector half the distance beyond the end vector.
 */
export function Euclid_Vc__Vc_lerp_Static_645D98BB(start, ende, rel) {
    const a_1 = start;
    let b_1;
    const f = rel;
    let v;
    const a = ende;
    const b = start;
    v = Vc_$ctor_7B00E9A0_1(a.X - b.X, a.Y - b.Y);
    b_1 = Vc_$ctor_7B00E9A0_1(v.X * f, v.Y * f);
    return Vc_$ctor_7B00E9A0_1(a_1.X + b_1.X, a_1.Y + b_1.Y);
}

/**
 * Spherically interpolates between start and end by the amount rel.
 * The difference between this and linear interpolation (aka, "lerp") is that the vectors are treated as directions rather than points in space.
 * The direction of the returned vector is interpolated by the angle and its magnitude is interpolated between the magnitudes of start and end.
 * Interpolation continues before and after the range of 0.0 and 1.0.
 */
export function Euclid_Vc__Vc_slerp_Static_645D98BB(start, ende, rel) {
    let sLen;
    const v = start;
    const x = v.X;
    const y = v.Y;
    sLen = Math.sqrt((x * x) + (y * y));
    let eLen;
    const v_1 = ende;
    const x_1 = v_1.X;
    const y_1 = v_1.Y;
    eLen = Math.sqrt((x_1 * x_1) + (y_1 * y_1));
    if (!(sLen > 1E-12)) {
        failTooSmall2("Vc.slerp", start, ende);
    }
    if (!(eLen > 1E-12)) {
        failTooSmall2("Vc.slerp", ende, start);
    }
    const fe = 1 / eLen;
    let su;
    const v_2 = start;
    const f = 1 / sLen;
    su = Vc_$ctor_7B00E9A0_1(v_2.X * f, v_2.Y * f);
    let eu;
    const v_3 = ende;
    const f_1 = fe;
    eu = Vc_$ctor_7B00E9A0_1(v_3.X * f_1, v_3.Y * f_1);
    let dot;
    const a = su;
    const b = eu;
    dot = ((a.X * b.X) + (a.Y * b.Y));
    if (dot > 0.9999996192282494) {
        return Euclid_Vc__Vc_lerp_Static_645D98BB(start, ende, rel);
    }
    else if (dot < -0.9999996192282494) {
        return fail2("Vc.slerp vectors are 180 deg opposite.", start, ende);
    }
    else {
        const ang = Math.acos(dot);
        let perp;
        let v_6;
        const a_1 = eu;
        let b_1;
        const v_4 = su;
        const f_2 = dot;
        b_1 = Vc_$ctor_7B00E9A0_1(v_4.X * f_2, v_4.Y * f_2);
        v_6 = Vc_$ctor_7B00E9A0_1(a_1.X - b_1.X, a_1.Y - b_1.Y);
        const x_4 = v_6.X;
        const y_2 = v_6.Y;
        const l = Math.sqrt((x_4 * x_4) + (y_2 * y_2));
        if (!(l > 1E-12)) {
            failUnit2("Vc.unitize", x_4, y_2);
        }
        perp = UnitVc_$ctor_7B00E9A0(x_4 / l, y_2 / l);
        const theta360 = ((ang * rel) + 6.283185307179586) % 6.283185307179586;
        const cosine = Math.cos(theta360);
        const sine = Math.sqrt(1 - (cosine * cosine));
        let res;
        if (theta360 < 3.141592653589793) {
            let a_3;
            const v_7 = su;
            const f_3 = cosine;
            a_3 = Vc_$ctor_7B00E9A0_1(v_7.X * f_3, v_7.Y * f_3);
            let b_2;
            const a_2 = perp;
            const f_4 = sine;
            b_2 = Vc_$ctor_7B00E9A0_1(a_2.X * f_4, a_2.Y * f_4);
            res = Vc_$ctor_7B00E9A0_1(a_3.X + b_2.X, a_3.Y + b_2.Y);
        }
        else {
            let a_5;
            const v_8 = su;
            const f_5 = cosine;
            a_5 = Vc_$ctor_7B00E9A0_1(v_8.X * f_5, v_8.Y * f_5);
            let b_3;
            const a_4 = perp;
            const f_6 = sine;
            b_3 = Vc_$ctor_7B00E9A0_1(a_4.X * f_6, a_4.Y * f_6);
            res = Vc_$ctor_7B00E9A0_1(a_5.X - b_3.X, a_5.Y - b_3.Y);
        }
        const lenRel = sLen + (rel * (eLen - sLen));
        if (lenRel < 0) {
            return Vc_$ctor_7B00E9A0_1(0, 0);
        }
        else {
            const v_9 = res;
            const f_7 = Math.abs(lenRel);
            return Vc_$ctor_7B00E9A0_1(v_9.X * f_7, v_9.Y * f_7);
        }
    }
}

/**
 * Returns the intersection parameters for two infinite lines.
 * Always returns Some since XLine2D.parameters handles parallel lines by returning infinity values.
 */
export function Euclid_Vc__Vc_intersection_Static_2BAF7E0(ptA, ptB, vA, vB) {
    let vAx, vAy, vBx, vBy, det, dx, dy;
    return (vAx = ptB.X, (vAy = ptB.Y, (vBx = vB.X, (vBy = vB.Y, (det = ((vAx * vBy) - (vAy * vBx)), (dx = (vA.X - ptA.X), (dy = (vA.Y - ptA.Y), [((dx * vBy) - (dy * vBx)) / det, ((dx * vAy) - (dy * vAx)) / det])))))));
}

