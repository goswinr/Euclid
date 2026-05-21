
import { Vec_$ctor_Z7AD9E565 } from "../Vec.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_1 } from "../Vec.js";
import { UnitVec_$ctor_Z7AD9E565 } from "../UnitVec.js";
import { Pnt_$ctor_Z7AD9E565 } from "../Pnt.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_1 } from "../Pnt.js";
import { failUnit3 } from "../EuclidErrors.js";

/**
 * Check if two 3D points are not equal within a given tolerance.
 * Use a tolerance of 0.0 to check if the two points are not exactly equal.
 */
export function Euclid_Pnt__Pnt_notEquals_Static(tol, a, b) {
    if ((Math.abs(a.X - b.X) > tol) ? true : (Math.abs(a.Y - b.Y) > tol)) {
        return true;
    }
    else {
        return Math.abs(a.Z - b.Z) > tol;
    }
}

/**
 * For three Points describing a plane return a normal.
 * If the returned vector has length zero then the points are in one line.
 */
export function Euclid_Pnt__Pnt_normalOf3Pts_Static_6180BC13(a, b, c) {
    let a_3;
    const a_1 = a;
    const b_1 = b;
    a_3 = Vec_$ctor_Z7AD9E565(a_1.X - b_1.X, a_1.Y - b_1.Y, a_1.Z - b_1.Z);
    let b_3;
    const a_2 = c;
    const b_2 = b;
    b_3 = Vec_$ctor_Z7AD9E565(a_2.X - b_2.X, a_2.Y - b_2.Y, a_2.Z - b_2.Z);
    return Vec_$ctor_Z7AD9E565((a_3.Y * b_3.Z) - (a_3.Z * b_3.Y), (a_3.Z * b_3.X) - (a_3.X * b_3.Z), (a_3.X * b_3.Y) - (a_3.Y * b_3.X));
}

/**
 * Every line has a normal vector in X-Y plane.
 * Rotated Counter-Clockwise in top view.
 * The result is unitized.
 * If line is vertical then Xaxis is returned.
 * see also : Vec.perpendicularVecInXY.
 */
export function Euclid_Pnt__Pnt_normalOfTwoPointsInXY_Static_5A6659A0(fromPt, toPt) {
    const x = toPt.Y - fromPt.Y;
    const y = fromPt.X - toPt.X;
    const len = Math.sqrt((x * x) + (y * y));
    if (!(len > 1E-12)) {
        return Vec_$ctor_Z7AD9E565_1(1, 0, 0);
    }
    else {
        return Vec_$ctor_Z7AD9E565_1(x / len, y / len, 0);
    }
}

export function Euclid_Pnt__Pnt_offsetTwoPt_Static_4402DAC0(fromPt, toPt, distHor, distNormal) {
    let p, v_7, p_1, v_8;
    let v;
    const a = toPt;
    const b = fromPt;
    v = Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
    let normHor;
    let v_2;
    const a_1 = v;
    const b_1 = Vec_$ctor_Z7AD9E565_1(0, 0, 1);
    v_2 = Vec_$ctor_Z7AD9E565((a_1.Y * b_1.Z) - (a_1.Z * b_1.Y), (a_1.Z * b_1.X) - (a_1.X * b_1.Z), (a_1.X * b_1.Y) - (a_1.Y * b_1.X));
    let l;
    const v_3 = v_2;
    l = (((v_3.X * v_3.X) + (v_3.Y * v_3.Y)) + (v_3.Z * v_3.Z));
    if (l < 1E-12) {
        normHor = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
    }
    else {
        const f = 1 / Math.sqrt(l);
        normHor = UnitVec_$ctor_Z7AD9E565(v_2.X * f, v_2.Y * f, v_2.Z * f);
    }
    let normFree;
    let v_5;
    const a_2 = v;
    const b_2 = normHor;
    v_5 = Vec_$ctor_Z7AD9E565_1((a_2.Y * b_2.Z) - (a_2.Z * b_2.Y), (a_2.Z * b_2.X) - (a_2.X * b_2.Z), (a_2.X * b_2.Y) - (a_2.Y * b_2.X));
    let l_1;
    const v_6 = v_5;
    l_1 = (((v_6.X * v_6.X) + (v_6.Y * v_6.Y)) + (v_6.Z * v_6.Z));
    if (l_1 < 1E-12) {
        normFree = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    }
    else {
        const f_1 = 1 / Math.sqrt(l_1);
        normFree = UnitVec_$ctor_Z7AD9E565(v_5.X * f_1, v_5.Y * f_1, v_5.Z * f_1);
    }
    let shift;
    let a_5;
    const f_2 = distHor;
    const a_3 = normHor;
    a_5 = Vec_$ctor_Z7AD9E565(a_3.X * f_2, a_3.Y * f_2, a_3.Z * f_2);
    let b_3;
    const f_3 = distNormal;
    const a_4 = normFree;
    b_3 = Vec_$ctor_Z7AD9E565(a_4.X * f_3, a_4.Y * f_3, a_4.Z * f_3);
    shift = Vec_$ctor_Z7AD9E565(a_5.X + b_3.X, a_5.Y + b_3.Y, a_5.Z + b_3.Z);
    return [(p = fromPt, (v_7 = shift, Pnt_$ctor_Z7AD9E565(p.X + v_7.X, p.Y + v_7.Y, p.Z + v_7.Z))), (p_1 = toPt, (v_8 = shift, Pnt_$ctor_Z7AD9E565(p_1.X + v_8.X, p_1.Y + v_8.Y, p_1.Z + v_8.Z)))];
}

/**
 * Multiplies (or applies) only the 3x3 rotation part of a RigidMatrix to a 3D point.
 */
export function Euclid_Pnt__Pnt_transformRigidRotateOnly_Static(m, p) {
    const p_1 = p;
    const m_1 = m;
    const x = p_1.X;
    const y = p_1.Y;
    const z = p_1.Z;
    return Pnt_$ctor_Z7AD9E565_1(((m_1.M11 * x) + (m_1.M21 * y)) + (m_1.M31 * z), ((m_1.M12 * x) + (m_1.M22 * y)) + (m_1.M32 * z), ((m_1.M13 * x) + (m_1.M23 * y)) + (m_1.M33 * z));
}

/**
 * Rotate the 3D point around X-axis, from Y to Z-axis, Counter Clockwise looking from right.
 */
export function Euclid_Pnt__Pnt_rotateXBy_Static(r, p) {
    return Pnt_$ctor_Z7AD9E565_1(p.X, (r.Cos * p.Y) - (r.Sin * p.Z), (r.Sin * p.Y) + (r.Cos * p.Z));
}

/**
 * Rotate the 3D point around Y-axis, from Z to X-axis, Counter Clockwise looking from back.
 */
export function Euclid_Pnt__Pnt_rotateYBy_Static(r, p) {
    return Pnt_$ctor_Z7AD9E565_1((r.Sin * p.Z) + (r.Cos * p.X), p.Y, (r.Cos * p.Z) - (r.Sin * p.X));
}

/**
 * Rotate the 3D point around Z-axis, from X to Y-axis, Counter Clockwise looking from top.
 */
export function Euclid_Pnt__Pnt_rotateZBy_Static(r, p) {
    return Pnt_$ctor_Z7AD9E565_1((r.Cos * p.X) - (r.Sin * p.Y), (r.Sin * p.X) + (r.Cos * p.Y), p.Z);
}

/**
 * Rotate the 3D point around a center 3D point and a X aligned axis, from Y to Z-axis, Counter Clockwise looking from right.
 */
export function Euclid_Pnt__Pnt_rotateXwithCenterBy_Static(cen, r, pt) {
    const y = pt.Y - cen.Y;
    const z = pt.Z - cen.Z;
    return Pnt_$ctor_Z7AD9E565_1((pt.X - cen.X) + cen.X, ((r.Cos * y) - (r.Sin * z)) + cen.Y, ((r.Sin * y) + (r.Cos * z)) + cen.Z);
}

/**
 * Rotate the 3D point around a center point and a Y aligned axis, from Z to X-axis, Counter Clockwise looking from back.
 */
export function Euclid_Pnt__Pnt_rotateYwithCenterBy_Static(cen, r, pt) {
    const x = pt.X - cen.X;
    const z = pt.Z - cen.Z;
    return Pnt_$ctor_Z7AD9E565_1(((r.Sin * z) + (r.Cos * x)) + cen.X, (pt.Y - cen.Y) + cen.Y, ((r.Cos * z) - (r.Sin * x)) + cen.Z);
}

/**
 * Rotate the 3D point around a center point and a Z aligned axis, from X to Y-axis, Counter Clockwise looking from top.
 */
export function Euclid_Pnt__Pnt_rotateZwithCenterBy_Static(cen, r, pt) {
    const x = pt.X - cen.X;
    const y = pt.Y - cen.Y;
    return Pnt_$ctor_Z7AD9E565_1(((r.Cos * x) - (r.Sin * y)) + cen.X, ((r.Sin * x) + (r.Cos * y)) + cen.Y, (pt.Z - cen.Z) + cen.Z);
}

/**
 * Returns angle in Degrees at mid point (thisPt).
 */
export function Euclid_Pnt__Pnt_angleInCorner_Static_6180BC13(prevPt, thisPt, nextPt) {
    let a_6, v, a, b, x, y, z, l, f, b_6, v_1, x_3, y_2, z_2, l_1, f_1, dot, a_7, b_7, x_6, y_4, z_4, x_7, y_5, z_5;
    let b_3;
    const a_2 = nextPt;
    const b_1 = thisPt;
    b_3 = Vec_$ctor_Z7AD9E565(a_2.X - b_1.X, a_2.Y - b_1.Y, a_2.Z - b_1.Z);
    return 57.29577951308232 * ((a_6 = ((v = ((a = prevPt, (b = thisPt, Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z)))), (x = v.X, (y = v.Y, (z = v.Z, (l = Math.sqrt(((x * x) + (y * y)) + (z * z)), (!(l > 1E-12) ? failUnit3("Vec.Unitized", x, y, z) : undefined, (f = (1 / l), UnitVec_$ctor_Z7AD9E565(f * x, f * y, f * z))))))))), (b_6 = ((v_1 = b_3, (x_3 = v_1.X, (y_2 = v_1.Y, (z_2 = v_1.Z, (l_1 = Math.sqrt(((x_3 * x_3) + (y_2 * y_2)) + (z_2 * z_2)), (!(l_1 > 1E-12) ? failUnit3("Vec.Unitized", x_3, y_2, z_2) : undefined, (f_1 = (1 / l_1), UnitVec_$ctor_Z7AD9E565(f_1 * x_3, f_1 * y_2, f_1 * z_2))))))))), (dot = ((a_7 = a_6, (b_7 = b_6, ((a_7.X * b_7.X) + (a_7.Y * b_7.Y)) + (a_7.Z * b_7.Z)))), ((-0.98 < dot) && (dot < 0.98)) ? Math.acos(dot) : ((dot < 0) ? (3.141592653589793 - (2 * Math.asin(((x_6 = (b_6.X - -a_6.X), (y_4 = (b_6.Y - -a_6.Y), (z_4 = (b_6.Z - -a_6.Z), Math.sqrt(((x_6 * x_6) + (y_4 * y_4)) + (z_4 * z_4)))))) * 0.5))) : (2 * Math.asin(((x_7 = (b_6.X - a_6.X), (y_5 = (b_6.Y - a_6.Y), (z_5 = (b_6.Z - a_6.Z), Math.sqrt(((x_7 * x_7) + (y_5 * y_5)) + (z_5 * z_5)))))) * 0.5)))))));
}

/**
 * Returns the closer point of the two points to the reference given point.
 * When both points are equally close, the first point is returned.
 */
export function Euclid_Pnt__Pnt_closestOfTwo_Static(pt1, pt2, referencePoint) {
    let a_1, b_1, x, y, z, a_3, b_3, x_1, y_1, z_1;
    if (((a_1 = pt1, (b_1 = referencePoint, (x = (a_1.X - b_1.X), (y = (a_1.Y - b_1.Y), (z = (a_1.Z - b_1.Z), ((x * x) + (y * y)) + (z * z))))))) <= ((a_3 = pt2, (b_3 = referencePoint, (x_1 = (a_3.X - b_3.X), (y_1 = (a_3.Y - b_3.Y), (z_1 = (a_3.Z - b_3.Z), ((x_1 * x_1) + (y_1 * y_1)) + (z_1 * z_1)))))))) {
        return pt1;
    }
    else {
        return pt2;
    }
}

