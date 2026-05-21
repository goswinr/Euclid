
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { Vec_$ctor_Z7AD9E565 } from "./Vec.js";
import { failColinear } from "./EuclidErrors.js";
import { failUnit3 } from "./EuclidErrors.js";
import { UnitVec_$ctor_Z7AD9E565 } from "./UnitVec.js";
import { Pnt_$ctor_Z7AD9E565 } from "./Pnt.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_1 } from "./Vec.js";
import { Euclid_Line3D__Line3D_intersectCone_Static_1D0AE3BB } from "./TypeExtensions/Line3D.js";

/**
 * A type containing only static member functions for operations on 2D Triangles.
 */
export class Tria3D {
    constructor() {
    }
}

export function Tria3D_$reflection() {
    return class_type("Euclid.Tria3D", undefined, Tria3D);
}

/**
 * Checks if three points are in one line.
 * By finding the biggest angle in the triangle.
 * And then measuring the distance from this point to the line defined by the other two points.
 */
export function Tria3D_isLinear_Z305FC6B8(a, b, c, distanceTolerance = 1E-06) {
    let tupledArg, lnFrom, lnTo, p, lnSqLen, x, y, z, u, v_3, w, dot, t, x$0027, y$0027, z$0027, u$0027, v$0027, w$0027, tupledArg_1, lnFrom_1, lnTo_1, p_1, lnSqLen_1, x_1, y_1, z_1, u_1, v_4, w_1, dot_1, t_1, x$0027_1, y$0027_1, z$0027_1, u$0027_1, v$0027_1, w$0027_1, tupledArg_2, lnFrom_2, lnTo_2, p_2, lnSqLen_2, x_2, y_2, z_2, u_2, v_5, w_2, dot_2, t_2, x$0027_2, y$0027_2, z$0027_2, u$0027_2, v$0027_2, w$0027_2;
    let ab;
    const a_1 = b;
    const b_1 = a;
    ab = Vec_$ctor_Z7AD9E565(a_1.X - b_1.X, a_1.Y - b_1.Y, a_1.Z - b_1.Z);
    let bc;
    const a_2 = c;
    const b_2 = b;
    bc = Vec_$ctor_Z7AD9E565(a_2.X - b_2.X, a_2.Y - b_2.Y, a_2.Z - b_2.Z);
    let ca;
    const a_3 = a;
    const b_3 = c;
    ca = Vec_$ctor_Z7AD9E565(a_3.X - b_3.X, a_3.Y - b_3.Y, a_3.Z - b_3.Z);
    let abLenSq;
    const v = ab;
    abLenSq = (((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    let bcLenSq;
    const v_1 = bc;
    bcLenSq = (((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
    let caLenSq;
    const v_2 = ca;
    caLenSq = (((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z));
    const distSq = distanceTolerance * distanceTolerance;
    if (((abLenSq < distSq) ? true : (bcLenSq < distSq)) ? true : (caLenSq < distSq)) {
        return true;
    }
    else {
        let dotA;
        const a_4 = ab;
        const b_4 = ca;
        dotA = (((a_4.X * b_4.X) + (a_4.Y * b_4.Y)) + (a_4.Z * b_4.Z));
        let dotB;
        const a_5 = bc;
        const b_5 = ab;
        dotB = (((a_5.X * b_5.X) + (a_5.Y * b_5.Y)) + (a_5.Z * b_5.Z));
        let dotC;
        const a_6 = ca;
        const b_6 = bc;
        dotC = (((a_6.X * b_6.X) + (a_6.Y * b_6.Y)) + (a_6.Z * b_6.Z));
        if ((dotA > dotB) && (dotA > dotC)) {
            return ((tupledArg = [b, c, a, bcLenSq], (lnFrom = tupledArg[0], (lnTo = tupledArg[1], (p = tupledArg[2], (lnSqLen = tupledArg[3], (x = (lnFrom.X - lnTo.X), (y = (lnFrom.Y - lnTo.Y), (z = (lnFrom.Z - lnTo.Z), (u = (lnFrom.X - p.X), (v_3 = (lnFrom.Y - p.Y), (w = (lnFrom.Z - p.Z), (dot = (((x * u) + (y * v_3)) + (z * w)), (t = (dot / lnSqLen), (x$0027 = (lnFrom.X - (x * t)), (y$0027 = (lnFrom.Y - (y * t)), (z$0027 = (lnFrom.Z - (z * t)), (u$0027 = (x$0027 - p.X), (v$0027 = (y$0027 - p.Y), (w$0027 = (z$0027 - p.Z), ((u$0027 * u$0027) + (v$0027 * v$0027)) + (w$0027 * w$0027))))))))))))))))))))) < distSq;
        }
        else if ((dotB > dotA) && (dotB > dotC)) {
            return ((tupledArg_1 = [c, a, b, caLenSq], (lnFrom_1 = tupledArg_1[0], (lnTo_1 = tupledArg_1[1], (p_1 = tupledArg_1[2], (lnSqLen_1 = tupledArg_1[3], (x_1 = (lnFrom_1.X - lnTo_1.X), (y_1 = (lnFrom_1.Y - lnTo_1.Y), (z_1 = (lnFrom_1.Z - lnTo_1.Z), (u_1 = (lnFrom_1.X - p_1.X), (v_4 = (lnFrom_1.Y - p_1.Y), (w_1 = (lnFrom_1.Z - p_1.Z), (dot_1 = (((x_1 * u_1) + (y_1 * v_4)) + (z_1 * w_1)), (t_1 = (dot_1 / lnSqLen_1), (x$0027_1 = (lnFrom_1.X - (x_1 * t_1)), (y$0027_1 = (lnFrom_1.Y - (y_1 * t_1)), (z$0027_1 = (lnFrom_1.Z - (z_1 * t_1)), (u$0027_1 = (x$0027_1 - p_1.X), (v$0027_1 = (y$0027_1 - p_1.Y), (w$0027_1 = (z$0027_1 - p_1.Z), ((u$0027_1 * u$0027_1) + (v$0027_1 * v$0027_1)) + (w$0027_1 * w$0027_1))))))))))))))))))))) < distSq;
        }
        else {
            return ((tupledArg_2 = [a, b, c, abLenSq], (lnFrom_2 = tupledArg_2[0], (lnTo_2 = tupledArg_2[1], (p_2 = tupledArg_2[2], (lnSqLen_2 = tupledArg_2[3], (x_2 = (lnFrom_2.X - lnTo_2.X), (y_2 = (lnFrom_2.Y - lnTo_2.Y), (z_2 = (lnFrom_2.Z - lnTo_2.Z), (u_2 = (lnFrom_2.X - p_2.X), (v_5 = (lnFrom_2.Y - p_2.Y), (w_2 = (lnFrom_2.Z - p_2.Z), (dot_2 = (((x_2 * u_2) + (y_2 * v_5)) + (z_2 * w_2)), (t_2 = (dot_2 / lnSqLen_2), (x$0027_2 = (lnFrom_2.X - (x_2 * t_2)), (y$0027_2 = (lnFrom_2.Y - (y_2 * t_2)), (z$0027_2 = (lnFrom_2.Z - (z_2 * t_2)), (u$0027_2 = (x$0027_2 - p_2.X), (v$0027_2 = (y$0027_2 - p_2.Y), (w$0027_2 = (z$0027_2 - p_2.Z), ((u$0027_2 * u$0027_2) + (v$0027_2 * v$0027_2)) + (w$0027_2 * w$0027_2))))))))))))))))))))) < distSq;
        }
    }
}

/**
 * Offsets one point by a given distance.
 * If the points 'prev', 'this' and 'next' are in counter-clockwise order, the offset is inwards.
 * Otherwise it is outwards.
 * A negative offset distance inverts the direction.
 */
export function Tria3D_offsetPnt_Z305FC6B8(pntToOffset, prev, next, dist) {
    let vPrev;
    const a = pntToOffset;
    const b = prev;
    vPrev = Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
    let vNext;
    const a_1 = next;
    const b_1 = pntToOffset;
    vNext = Vec_$ctor_Z7AD9E565(a_1.X - b_1.X, a_1.Y - b_1.Y, a_1.Z - b_1.Z);
    let perp;
    const a_2 = vPrev;
    const b_2 = vNext;
    perp = Vec_$ctor_Z7AD9E565((a_2.Y * b_2.Z) - (a_2.Z * b_2.Y), (a_2.Z * b_2.X) - (a_2.X * b_2.Z), (a_2.X * b_2.Y) - (a_2.Y * b_2.X));
    let lenSq;
    const v = perp;
    lenSq = (((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    if (!(lenSq > 1E-24)) {
        failColinear("Tria3D.offsetPnt", pntToOffset, prev, next);
    }
    let nPrev;
    let v_2;
    const a_3 = perp;
    const b_3 = vPrev;
    v_2 = Vec_$ctor_Z7AD9E565((a_3.Y * b_3.Z) - (a_3.Z * b_3.Y), (a_3.Z * b_3.X) - (a_3.X * b_3.Z), (a_3.X * b_3.Y) - (a_3.Y * b_3.X));
    const x_1 = v_2.X;
    const y = v_2.Y;
    const z = v_2.Z;
    const l = Math.sqrt(((x_1 * x_1) + (y * y)) + (z * z));
    if (!(l > 1E-12)) {
        failUnit3("Vec.unitize", x_1, y, z);
    }
    const f = 1 / l;
    nPrev = UnitVec_$ctor_Z7AD9E565(f * x_1, f * y, f * z);
    let nNext;
    let v_4;
    const a_4 = perp;
    const b_4 = vNext;
    v_4 = Vec_$ctor_Z7AD9E565((a_4.Y * b_4.Z) - (a_4.Z * b_4.Y), (a_4.Z * b_4.X) - (a_4.X * b_4.Z), (a_4.X * b_4.Y) - (a_4.Y * b_4.X));
    const x_4 = v_4.X;
    const y_2 = v_4.Y;
    const z_2 = v_4.Z;
    const l_1 = Math.sqrt(((x_4 * x_4) + (y_2 * y_2)) + (z_2 * z_2));
    if (!(l_1 > 1E-12)) {
        failUnit3("Vec.unitize", x_4, y_2, z_2);
    }
    const f_1 = 1 / l_1;
    nNext = UnitVec_$ctor_Z7AD9E565(f_1 * x_4, f_1 * y_2, f_1 * z_2);
    let cosine;
    const a_5 = nPrev;
    const b_5 = nNext;
    cosine = (((a_5.X * b_5.X) + (a_5.Y * b_5.Y)) + (a_5.Z * b_5.Z));
    if (cosine < -0.9998476951563913) {
        let dirPrev;
        const v_7 = vPrev;
        let l_2;
        const v_8 = v_7;
        l_2 = Math.sqrt(((v_8.X * v_8.X) + (v_8.Y * v_8.Y)) + (v_8.Z * v_8.Z));
        if (!(l_2 > 1E-12)) {
            failUnit3("Vec.WithLength", v_7.X, v_7.Y, v_7.Z);
        }
        const a_6 = v_7;
        const f_2 = 28.645 / l_2;
        dirPrev = Vec_$ctor_Z7AD9E565(a_6.X * f_2, a_6.Y * f_2, a_6.Z * f_2);
        let dirNext;
        const v_11 = vNext;
        let l_3;
        const v_12 = v_11;
        l_3 = Math.sqrt(((v_12.X * v_12.X) + (v_12.Y * v_12.Y)) + (v_12.Z * v_12.Z));
        if (!(l_3 > 1E-12)) {
            failUnit3("Vec.WithLength", v_11.X, v_11.Y, v_11.Z);
        }
        const a_7 = v_11;
        const f_3 = -28.645 / l_3;
        dirNext = Vec_$ctor_Z7AD9E565(a_7.X * f_3, a_7.Y * f_3, a_7.Z * f_3);
        let p_1;
        const p = pntToOffset;
        const v_13 = dirPrev;
        p_1 = Pnt_$ctor_Z7AD9E565(p.X + v_13.X, p.Y + v_13.Y, p.Z + v_13.Z);
        const v_14 = dirNext;
        return Pnt_$ctor_Z7AD9E565(p_1.X + v_14.X, p_1.Y + v_14.Y, p_1.Z + v_14.Z);
    }
    else {
        const cosine_1 = cosine;
        const p_2 = pntToOffset;
        let v_15;
        let a_9;
        const a_8 = nPrev;
        const b_6 = nNext;
        a_9 = Vec_$ctor_Z7AD9E565(a_8.X + b_6.X, a_8.Y + b_6.Y, a_8.Z + b_6.Z);
        const f_4 = dist / (1 + cosine_1);
        v_15 = Vec_$ctor_Z7AD9E565(a_9.X * f_4, a_9.Y * f_4, a_9.Z * f_4);
        return Pnt_$ctor_Z7AD9E565(p_2.X + v_15.X, p_2.Y + v_15.Y, p_2.Z + v_15.Z);
    }
}

/**
 * Offsets all points by a given distance.
 * The offset is inwards for positive distance and outwards for negative distance.
 */
export function Tria3D_offset_Z305FC6B8(a, b, c, dist) {
    let v, pt_1, nPrev_1, nNext_1, perp_2, cosine_1, dirPrev, v_9, a_11, b_11, l_3, v_10, a_12, f_3, dirNext, v_12, a_13, b_12, l_4, v_13, a_14, f_4, p_1, p, v_14, v_15, p_2, v_16, a_16, a_15, b_13, f_5, pt_4, nPrev_4, nNext_4, perp_4, cosine_4, dirPrev_1, v_19, a_17, b_14, l_5, v_20, a_18, f_6, dirNext_1, v_22, a_19, b_15, l_6, v_23, a_20, f_7, p_4, p_3, v_24, v_25, p_5, v_26, a_22, a_21, b_16, f_8, pt_7, nPrev_7, nNext_7, perp_6, cosine_7, dirPrev_2, v_29, a_23, b_17, l_7, v_30, a_24, f_9, dirNext_2, v_32, a_25, b_18, l_8, v_33, a_26, f_10, p_7, p_6, v_34, v_35, p_8, v_36, a_28, a_27, b_19, f_11;
    let vAB;
    const a_1 = b;
    const b_1 = a;
    vAB = Vec_$ctor_Z7AD9E565(a_1.X - b_1.X, a_1.Y - b_1.Y, a_1.Z - b_1.Z);
    let vBC;
    const a_2 = c;
    const b_2 = b;
    vBC = Vec_$ctor_Z7AD9E565(a_2.X - b_2.X, a_2.Y - b_2.Y, a_2.Z - b_2.Z);
    let vCA;
    const a_3 = a;
    const b_3 = c;
    vCA = Vec_$ctor_Z7AD9E565(a_3.X - b_3.X, a_3.Y - b_3.Y, a_3.Z - b_3.Z);
    let perp;
    const a_4 = vAB;
    const b_4 = vBC;
    perp = Vec_$ctor_Z7AD9E565((a_4.Y * b_4.Z) - (a_4.Z * b_4.Y), (a_4.Z * b_4.X) - (a_4.X * b_4.Z), (a_4.X * b_4.Y) - (a_4.Y * b_4.X));
    if (!(((v = perp, ((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z))) > 1E-12)) {
        failColinear("Tria3D.offset", a, b, c);
    }
    let na;
    let v_2;
    const a_5 = perp;
    const b_5 = vAB;
    v_2 = Vec_$ctor_Z7AD9E565((a_5.Y * b_5.Z) - (a_5.Z * b_5.Y), (a_5.Z * b_5.X) - (a_5.X * b_5.Z), (a_5.X * b_5.Y) - (a_5.Y * b_5.X));
    const x_1 = v_2.X;
    const y = v_2.Y;
    const z = v_2.Z;
    const l = Math.sqrt(((x_1 * x_1) + (y * y)) + (z * z));
    if (!(l > 1E-12)) {
        failUnit3("Vec.unitize", x_1, y, z);
    }
    const f = 1 / l;
    na = UnitVec_$ctor_Z7AD9E565(f * x_1, f * y, f * z);
    let nb;
    let v_4;
    const a_6 = perp;
    const b_6 = vBC;
    v_4 = Vec_$ctor_Z7AD9E565((a_6.Y * b_6.Z) - (a_6.Z * b_6.Y), (a_6.Z * b_6.X) - (a_6.X * b_6.Z), (a_6.X * b_6.Y) - (a_6.Y * b_6.X));
    const x_4 = v_4.X;
    const y_2 = v_4.Y;
    const z_2 = v_4.Z;
    const l_1 = Math.sqrt(((x_4 * x_4) + (y_2 * y_2)) + (z_2 * z_2));
    if (!(l_1 > 1E-12)) {
        failUnit3("Vec.unitize", x_4, y_2, z_2);
    }
    const f_1 = 1 / l_1;
    nb = UnitVec_$ctor_Z7AD9E565(f_1 * x_4, f_1 * y_2, f_1 * z_2);
    let nc;
    let v_6;
    const a_7 = perp;
    const b_7 = vCA;
    v_6 = Vec_$ctor_Z7AD9E565((a_7.Y * b_7.Z) - (a_7.Z * b_7.Y), (a_7.Z * b_7.X) - (a_7.X * b_7.Z), (a_7.X * b_7.Y) - (a_7.Y * b_7.X));
    const x_7 = v_6.X;
    const y_4 = v_6.Y;
    const z_4 = v_6.Z;
    const l_2 = Math.sqrt(((x_7 * x_7) + (y_4 * y_4)) + (z_4 * z_4));
    if (!(l_2 > 1E-12)) {
        failUnit3("Vec.unitize", x_7, y_4, z_4);
    }
    const f_2 = 1 / l_2;
    nc = UnitVec_$ctor_Z7AD9E565(f_2 * x_7, f_2 * y_4, f_2 * z_4);
    let cosineA;
    const a_8 = na;
    const b_8 = nc;
    cosineA = (((a_8.X * b_8.X) + (a_8.Y * b_8.Y)) + (a_8.Z * b_8.Z));
    let cosineB;
    const a_9 = na;
    const b_9 = nb;
    cosineB = (((a_9.X * b_9.X) + (a_9.Y * b_9.Y)) + (a_9.Z * b_9.Z));
    let cosineC;
    const a_10 = nb;
    const b_10 = nc;
    cosineC = (((a_10.X * b_10.X) + (a_10.Y * b_10.Y)) + (a_10.Z * b_10.Z));
    return [(pt_1 = a, (nPrev_1 = na, (nNext_1 = nc, (perp_2 = perp, (cosine_1 = cosineA, (cosine_1 < -0.9998476951563913) ? ((dirPrev = ((v_9 = ((a_11 = nPrev_1, (b_11 = perp_2, Vec_$ctor_Z7AD9E565_1((a_11.Y * b_11.Z) - (a_11.Z * b_11.Y), (a_11.Z * b_11.X) - (a_11.X * b_11.Z), (a_11.X * b_11.Y) - (a_11.Y * b_11.X))))), (l_3 = ((v_10 = v_9, Math.sqrt(((v_10.X * v_10.X) + (v_10.Y * v_10.Y)) + (v_10.Z * v_10.Z)))), (!(l_3 > 1E-12) ? failUnit3("Vec.WithLength", v_9.X, v_9.Y, v_9.Z) : undefined, (a_12 = v_9, (f_3 = (28.645 / l_3), Vec_$ctor_Z7AD9E565(a_12.X * f_3, a_12.Y * f_3, a_12.Z * f_3))))))), (dirNext = ((v_12 = ((a_13 = perp_2, (b_12 = nNext_1, Vec_$ctor_Z7AD9E565_1((a_13.Y * b_12.Z) - (a_13.Z * b_12.Y), (a_13.Z * b_12.X) - (a_13.X * b_12.Z), (a_13.X * b_12.Y) - (a_13.Y * b_12.X))))), (l_4 = ((v_13 = v_12, Math.sqrt(((v_13.X * v_13.X) + (v_13.Y * v_13.Y)) + (v_13.Z * v_13.Z)))), (!(l_4 > 1E-12) ? failUnit3("Vec.WithLength", v_12.X, v_12.Y, v_12.Z) : undefined, (a_14 = v_12, (f_4 = (28.645 / l_4), Vec_$ctor_Z7AD9E565(a_14.X * f_4, a_14.Y * f_4, a_14.Z * f_4))))))), (p_1 = ((p = pt_1, (v_14 = dirPrev, Pnt_$ctor_Z7AD9E565(p.X + v_14.X, p.Y + v_14.Y, p.Z + v_14.Z)))), (v_15 = dirNext, Pnt_$ctor_Z7AD9E565(p_1.X + v_15.X, p_1.Y + v_15.Y, p_1.Z + v_15.Z)))))) : ((p_2 = pt_1, (v_16 = ((a_16 = ((a_15 = nPrev_1, (b_13 = nNext_1, Vec_$ctor_Z7AD9E565(a_15.X + b_13.X, a_15.Y + b_13.Y, a_15.Z + b_13.Z)))), (f_5 = (dist / (1 + cosine_1)), Vec_$ctor_Z7AD9E565(a_16.X * f_5, a_16.Y * f_5, a_16.Z * f_5)))), Pnt_$ctor_Z7AD9E565(p_2.X + v_16.X, p_2.Y + v_16.Y, p_2.Z + v_16.Z))))))))), (pt_4 = b, (nPrev_4 = na, (nNext_4 = nb, (perp_4 = perp, (cosine_4 = cosineB, (cosine_4 < -0.9998476951563913) ? ((dirPrev_1 = ((v_19 = ((a_17 = nPrev_4, (b_14 = perp_4, Vec_$ctor_Z7AD9E565_1((a_17.Y * b_14.Z) - (a_17.Z * b_14.Y), (a_17.Z * b_14.X) - (a_17.X * b_14.Z), (a_17.X * b_14.Y) - (a_17.Y * b_14.X))))), (l_5 = ((v_20 = v_19, Math.sqrt(((v_20.X * v_20.X) + (v_20.Y * v_20.Y)) + (v_20.Z * v_20.Z)))), (!(l_5 > 1E-12) ? failUnit3("Vec.WithLength", v_19.X, v_19.Y, v_19.Z) : undefined, (a_18 = v_19, (f_6 = (28.645 / l_5), Vec_$ctor_Z7AD9E565(a_18.X * f_6, a_18.Y * f_6, a_18.Z * f_6))))))), (dirNext_1 = ((v_22 = ((a_19 = perp_4, (b_15 = nNext_4, Vec_$ctor_Z7AD9E565_1((a_19.Y * b_15.Z) - (a_19.Z * b_15.Y), (a_19.Z * b_15.X) - (a_19.X * b_15.Z), (a_19.X * b_15.Y) - (a_19.Y * b_15.X))))), (l_6 = ((v_23 = v_22, Math.sqrt(((v_23.X * v_23.X) + (v_23.Y * v_23.Y)) + (v_23.Z * v_23.Z)))), (!(l_6 > 1E-12) ? failUnit3("Vec.WithLength", v_22.X, v_22.Y, v_22.Z) : undefined, (a_20 = v_22, (f_7 = (28.645 / l_6), Vec_$ctor_Z7AD9E565(a_20.X * f_7, a_20.Y * f_7, a_20.Z * f_7))))))), (p_4 = ((p_3 = pt_4, (v_24 = dirPrev_1, Pnt_$ctor_Z7AD9E565(p_3.X + v_24.X, p_3.Y + v_24.Y, p_3.Z + v_24.Z)))), (v_25 = dirNext_1, Pnt_$ctor_Z7AD9E565(p_4.X + v_25.X, p_4.Y + v_25.Y, p_4.Z + v_25.Z)))))) : ((p_5 = pt_4, (v_26 = ((a_22 = ((a_21 = nPrev_4, (b_16 = nNext_4, Vec_$ctor_Z7AD9E565(a_21.X + b_16.X, a_21.Y + b_16.Y, a_21.Z + b_16.Z)))), (f_8 = (dist / (1 + cosine_4)), Vec_$ctor_Z7AD9E565(a_22.X * f_8, a_22.Y * f_8, a_22.Z * f_8)))), Pnt_$ctor_Z7AD9E565(p_5.X + v_26.X, p_5.Y + v_26.Y, p_5.Z + v_26.Z))))))))), (pt_7 = c, (nPrev_7 = nb, (nNext_7 = nc, (perp_6 = perp, (cosine_7 = cosineC, (cosine_7 < -0.9998476951563913) ? ((dirPrev_2 = ((v_29 = ((a_23 = nPrev_7, (b_17 = perp_6, Vec_$ctor_Z7AD9E565_1((a_23.Y * b_17.Z) - (a_23.Z * b_17.Y), (a_23.Z * b_17.X) - (a_23.X * b_17.Z), (a_23.X * b_17.Y) - (a_23.Y * b_17.X))))), (l_7 = ((v_30 = v_29, Math.sqrt(((v_30.X * v_30.X) + (v_30.Y * v_30.Y)) + (v_30.Z * v_30.Z)))), (!(l_7 > 1E-12) ? failUnit3("Vec.WithLength", v_29.X, v_29.Y, v_29.Z) : undefined, (a_24 = v_29, (f_9 = (28.645 / l_7), Vec_$ctor_Z7AD9E565(a_24.X * f_9, a_24.Y * f_9, a_24.Z * f_9))))))), (dirNext_2 = ((v_32 = ((a_25 = perp_6, (b_18 = nNext_7, Vec_$ctor_Z7AD9E565_1((a_25.Y * b_18.Z) - (a_25.Z * b_18.Y), (a_25.Z * b_18.X) - (a_25.X * b_18.Z), (a_25.X * b_18.Y) - (a_25.Y * b_18.X))))), (l_8 = ((v_33 = v_32, Math.sqrt(((v_33.X * v_33.X) + (v_33.Y * v_33.Y)) + (v_33.Z * v_33.Z)))), (!(l_8 > 1E-12) ? failUnit3("Vec.WithLength", v_32.X, v_32.Y, v_32.Z) : undefined, (a_26 = v_32, (f_10 = (28.645 / l_8), Vec_$ctor_Z7AD9E565(a_26.X * f_10, a_26.Y * f_10, a_26.Z * f_10))))))), (p_7 = ((p_6 = pt_7, (v_34 = dirPrev_2, Pnt_$ctor_Z7AD9E565(p_6.X + v_34.X, p_6.Y + v_34.Y, p_6.Z + v_34.Z)))), (v_35 = dirNext_2, Pnt_$ctor_Z7AD9E565(p_7.X + v_35.X, p_7.Y + v_35.Y, p_7.Z + v_35.Z)))))) : ((p_8 = pt_7, (v_36 = ((a_28 = ((a_27 = nPrev_7, (b_19 = nNext_7, Vec_$ctor_Z7AD9E565(a_27.X + b_19.X, a_27.Y + b_19.Y, a_27.Z + b_19.Z)))), (f_11 = (dist / (1 + cosine_7)), Vec_$ctor_Z7AD9E565(a_28.X * f_11, a_28.Y * f_11, a_28.Z * f_11)))), Pnt_$ctor_Z7AD9E565(p_8.X + v_36.X, p_8.Y + v_36.Y, p_8.Z + v_36.Z)))))))))];
}

/**
 * Finds the offset point based on
 * the previous and next unit normals (= offset direction),
 * and their offset distances.
 */
export function Tria3D_offsetVar_Z62609A8D(ptToOffset, prev, next, distPrev, distNext) {
    let vPrev;
    const a = ptToOffset;
    const b = prev;
    vPrev = Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
    let vNext;
    const a_1 = next;
    const b_1 = ptToOffset;
    vNext = Vec_$ctor_Z7AD9E565(a_1.X - b_1.X, a_1.Y - b_1.Y, a_1.Z - b_1.Z);
    let perp;
    const a_2 = vPrev;
    const b_2 = vNext;
    perp = Vec_$ctor_Z7AD9E565((a_2.Y * b_2.Z) - (a_2.Z * b_2.Y), (a_2.Z * b_2.X) - (a_2.X * b_2.Z), (a_2.X * b_2.Y) - (a_2.Y * b_2.X));
    let lenPerpSq;
    const v = perp;
    lenPerpSq = (((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    if (!(lenPerpSq > 1E-24)) {
        return undefined;
    }
    else {
        let vPrevU;
        const v_2 = vPrev;
        const x_1 = v_2.X;
        const y = v_2.Y;
        const z = v_2.Z;
        const l = Math.sqrt(((x_1 * x_1) + (y * y)) + (z * z));
        if (!(l > 1E-12)) {
            failUnit3("Vec.unitize", x_1, y, z);
        }
        const f = 1 / l;
        vPrevU = UnitVec_$ctor_Z7AD9E565(f * x_1, f * y, f * z);
        let vNextU;
        const v_4 = vNext;
        const x_4 = v_4.X;
        const y_2 = v_4.Y;
        const z_2 = v_4.Z;
        const l_1 = Math.sqrt(((x_4 * x_4) + (y_2 * y_2)) + (z_2 * z_2));
        if (!(l_1 > 1E-12)) {
            failUnit3("Vec.unitize", x_4, y_2, z_2);
        }
        const f_1 = 1 / l_1;
        vNextU = UnitVec_$ctor_Z7AD9E565(f_1 * x_4, f_1 * y_2, f_1 * z_2);
        let perpU;
        const a_3 = perp;
        const f_2 = 1 / Math.sqrt(lenPerpSq);
        perpU = Vec_$ctor_Z7AD9E565(a_3.X * f_2, a_3.Y * f_2, a_3.Z * f_2);
        let nPrevU;
        const a_4 = perpU;
        const b_3 = vPrevU;
        nPrevU = Vec_$ctor_Z7AD9E565_1((a_4.Y * b_3.Z) - (a_4.Z * b_3.Y), (a_4.Z * b_3.X) - (a_4.X * b_3.Z), (a_4.X * b_3.Y) - (a_4.Y * b_3.X));
        let nNextU;
        const a_5 = perpU;
        const b_4 = vNextU;
        nNextU = Vec_$ctor_Z7AD9E565_1((a_5.Y * b_4.Z) - (a_5.Z * b_4.Y), (a_5.Z * b_4.X) - (a_5.X * b_4.Z), (a_5.X * b_4.Y) - (a_5.Y * b_4.X));
        const ax = vPrevU.X;
        const ay = vPrevU.Y;
        const az = vPrevU.Z;
        const bx = vNextU.X;
        const by = vNextU.Y;
        const bz = vNextU.Z;
        let lnAFrom;
        const p = ptToOffset;
        let v_5;
        const a_6 = nPrevU;
        const f_3 = distPrev;
        v_5 = Vec_$ctor_Z7AD9E565(a_6.X * f_3, a_6.Y * f_3, a_6.Z * f_3);
        lnAFrom = Pnt_$ctor_Z7AD9E565(p.X + v_5.X, p.Y + v_5.Y, p.Z + v_5.Z);
        let lnBFrom;
        const p_1 = ptToOffset;
        let v_6;
        const a_7 = nNextU;
        const f_4 = distNext;
        v_6 = Vec_$ctor_Z7AD9E565(a_7.X * f_4, a_7.Y * f_4, a_7.Z * f_4);
        lnBFrom = Pnt_$ctor_Z7AD9E565(p_1.X + v_6.X, p_1.Y + v_6.Y, p_1.Z + v_6.Z);
        const b_5 = ((ax * bx) + (ay * by)) + (az * bz);
        const vx = lnBFrom.X - lnAFrom.X;
        const vy = lnBFrom.Y - lnAFrom.Y;
        const vz = lnBFrom.Z - lnAFrom.Z;
        const bb = b_5 * b_5;
        const discriminant = 1 - bb;
        const e = ((bx * vx) + (by * vy)) + (bz * vz);
        const d = ((ax * vx) + (ay * vy)) + (az * vz);
        const tPrev = ((b_5 * e) - d) / discriminant;
        let pt;
        const p_2 = lnAFrom;
        let v_7;
        const a_8 = nPrevU;
        const f_5 = tPrev;
        v_7 = Vec_$ctor_Z7AD9E565(a_8.X * f_5, a_8.Y * f_5, a_8.Z * f_5);
        pt = Pnt_$ctor_Z7AD9E565(p_2.X + v_7.X, p_2.Y + v_7.Y, p_2.Z + v_7.Z);
        return pt;
    }
}

/**
 * Calculates the intersection of a finite line with a triangle.
 * Returns Some(Pnt) or None if no intersection was found,
 * or if the input line has near zero length,
 * or if the input triangle has near zero area.
 * This algorithm still returns an intersection even if line and triangle are almost parallel.
 * Since it is using the triple product, it is hard to find an appropriate tolerance for
 * considering lines and triangles parallel based on the volume of the tetrahedron between them.
 */
export function Tria3D_intersectLine_Z60F2B6CD(line, p1, p2, p3) {
    let a_35, a_36, b_36, b_35, x_6, p, v_1, a_37, f;
    let q1;
    const ln = line;
    q1 = Pnt_$ctor_Z7AD9E565(ln.FromX, ln.FromY, ln.FromZ);
    let q2;
    const ln_1 = line;
    q2 = Pnt_$ctor_Z7AD9E565(ln_1.ToX, ln_1.ToY, ln_1.ToZ);
    let s1;
    let x;
    const tupledArg = [q1, p1, p2, p3];
    const a = tupledArg[0];
    const b = tupledArg[1];
    const c = tupledArg[2];
    const d = tupledArg[3];
    let a_1;
    let a_4;
    const a_2 = b;
    const b_2 = a;
    a_4 = Vec_$ctor_Z7AD9E565(a_2.X - b_2.X, a_2.Y - b_2.Y, a_2.Z - b_2.Z);
    let b_4;
    const a_3 = c;
    const b_3 = a;
    b_4 = Vec_$ctor_Z7AD9E565(a_3.X - b_3.X, a_3.Y - b_3.Y, a_3.Z - b_3.Z);
    a_1 = Vec_$ctor_Z7AD9E565((a_4.Y * b_4.Z) - (a_4.Z * b_4.Y), (a_4.Z * b_4.X) - (a_4.X * b_4.Z), (a_4.X * b_4.Y) - (a_4.Y * b_4.X));
    let b_1;
    const a_5 = d;
    const b_5 = a;
    b_1 = Vec_$ctor_Z7AD9E565(a_5.X - b_5.X, a_5.Y - b_5.Y, a_5.Z - b_5.Z);
    x = (((a_1.X * b_1.X) + (a_1.Y * b_1.Y)) + (a_1.Z * b_1.Z));
    s1 = ((x === 0) ? 0 : ((x > 0) ? 1 : -1));
    let s2;
    let x_1;
    const tupledArg_1 = [q2, p1, p2, p3];
    const a_6 = tupledArg_1[0];
    const b_6 = tupledArg_1[1];
    const c_1 = tupledArg_1[2];
    const d_1 = tupledArg_1[3];
    let a_7;
    let a_10;
    const a_8 = b_6;
    const b_8 = a_6;
    a_10 = Vec_$ctor_Z7AD9E565(a_8.X - b_8.X, a_8.Y - b_8.Y, a_8.Z - b_8.Z);
    let b_10;
    const a_9 = c_1;
    const b_9 = a_6;
    b_10 = Vec_$ctor_Z7AD9E565(a_9.X - b_9.X, a_9.Y - b_9.Y, a_9.Z - b_9.Z);
    a_7 = Vec_$ctor_Z7AD9E565((a_10.Y * b_10.Z) - (a_10.Z * b_10.Y), (a_10.Z * b_10.X) - (a_10.X * b_10.Z), (a_10.X * b_10.Y) - (a_10.Y * b_10.X));
    let b_7;
    const a_11 = d_1;
    const b_11 = a_6;
    b_7 = Vec_$ctor_Z7AD9E565(a_11.X - b_11.X, a_11.Y - b_11.Y, a_11.Z - b_11.Z);
    x_1 = (((a_7.X * b_7.X) + (a_7.Y * b_7.Y)) + (a_7.Z * b_7.Z));
    s2 = ((x_1 === 0) ? 0 : ((x_1 > 0) ? 1 : -1));
    if (s1 === s2) {
        return undefined;
    }
    else {
        let s3;
        let x_2;
        const tupledArg_2 = [q1, q2, p1, p2];
        const a_12 = tupledArg_2[0];
        const b_12 = tupledArg_2[1];
        const c_2 = tupledArg_2[2];
        const d_2 = tupledArg_2[3];
        let a_13;
        let a_16;
        const a_14 = b_12;
        const b_14 = a_12;
        a_16 = Vec_$ctor_Z7AD9E565(a_14.X - b_14.X, a_14.Y - b_14.Y, a_14.Z - b_14.Z);
        let b_16;
        const a_15 = c_2;
        const b_15 = a_12;
        b_16 = Vec_$ctor_Z7AD9E565(a_15.X - b_15.X, a_15.Y - b_15.Y, a_15.Z - b_15.Z);
        a_13 = Vec_$ctor_Z7AD9E565((a_16.Y * b_16.Z) - (a_16.Z * b_16.Y), (a_16.Z * b_16.X) - (a_16.X * b_16.Z), (a_16.X * b_16.Y) - (a_16.Y * b_16.X));
        let b_13;
        const a_17 = d_2;
        const b_17 = a_12;
        b_13 = Vec_$ctor_Z7AD9E565(a_17.X - b_17.X, a_17.Y - b_17.Y, a_17.Z - b_17.Z);
        x_2 = (((a_13.X * b_13.X) + (a_13.Y * b_13.Y)) + (a_13.Z * b_13.Z));
        s3 = ((x_2 === 0) ? 0 : ((x_2 > 0) ? 1 : -1));
        let s4;
        let x_3;
        const tupledArg_3 = [q1, q2, p2, p3];
        const a_18 = tupledArg_3[0];
        const b_18 = tupledArg_3[1];
        const c_3 = tupledArg_3[2];
        const d_3 = tupledArg_3[3];
        let a_19;
        let a_22;
        const a_20 = b_18;
        const b_20 = a_18;
        a_22 = Vec_$ctor_Z7AD9E565(a_20.X - b_20.X, a_20.Y - b_20.Y, a_20.Z - b_20.Z);
        let b_22;
        const a_21 = c_3;
        const b_21 = a_18;
        b_22 = Vec_$ctor_Z7AD9E565(a_21.X - b_21.X, a_21.Y - b_21.Y, a_21.Z - b_21.Z);
        a_19 = Vec_$ctor_Z7AD9E565((a_22.Y * b_22.Z) - (a_22.Z * b_22.Y), (a_22.Z * b_22.X) - (a_22.X * b_22.Z), (a_22.X * b_22.Y) - (a_22.Y * b_22.X));
        let b_19;
        const a_23 = d_3;
        const b_23 = a_18;
        b_19 = Vec_$ctor_Z7AD9E565(a_23.X - b_23.X, a_23.Y - b_23.Y, a_23.Z - b_23.Z);
        x_3 = (((a_19.X * b_19.X) + (a_19.Y * b_19.Y)) + (a_19.Z * b_19.Z));
        s4 = ((x_3 === 0) ? 0 : ((x_3 > 0) ? 1 : -1));
        let s5;
        let x_4;
        const tupledArg_4 = [q1, q2, p3, p1];
        const a_24 = tupledArg_4[0];
        const b_24 = tupledArg_4[1];
        const c_4 = tupledArg_4[2];
        const d_4 = tupledArg_4[3];
        let a_25;
        let a_28;
        const a_26 = b_24;
        const b_26 = a_24;
        a_28 = Vec_$ctor_Z7AD9E565(a_26.X - b_26.X, a_26.Y - b_26.Y, a_26.Z - b_26.Z);
        let b_28;
        const a_27 = c_4;
        const b_27 = a_24;
        b_28 = Vec_$ctor_Z7AD9E565(a_27.X - b_27.X, a_27.Y - b_27.Y, a_27.Z - b_27.Z);
        a_25 = Vec_$ctor_Z7AD9E565((a_28.Y * b_28.Z) - (a_28.Z * b_28.Y), (a_28.Z * b_28.X) - (a_28.X * b_28.Z), (a_28.X * b_28.Y) - (a_28.Y * b_28.X));
        let b_25;
        const a_29 = d_4;
        const b_29 = a_24;
        b_25 = Vec_$ctor_Z7AD9E565(a_29.X - b_29.X, a_29.Y - b_29.Y, a_29.Z - b_29.Z);
        x_4 = (((a_25.X * b_25.X) + (a_25.Y * b_25.Y)) + (a_25.Z * b_25.Z));
        s5 = ((x_4 === 0) ? 0 : ((x_4 > 0) ? 1 : -1));
        if ((s3 === s4) && (s4 === s5)) {
            let n;
            let a_32;
            const a_30 = p2;
            const b_30 = p1;
            a_32 = Vec_$ctor_Z7AD9E565(a_30.X - b_30.X, a_30.Y - b_30.Y, a_30.Z - b_30.Z);
            let b_32;
            const a_31 = p3;
            const b_31 = p1;
            b_32 = Vec_$ctor_Z7AD9E565(a_31.X - b_31.X, a_31.Y - b_31.Y, a_31.Z - b_31.Z);
            n = Vec_$ctor_Z7AD9E565((a_32.Y * b_32.Z) - (a_32.Z * b_32.Y), (a_32.Z * b_32.X) - (a_32.X * b_32.Z), (a_32.X * b_32.Y) - (a_32.Y * b_32.X));
            let v;
            const a_33 = q2;
            const b_33 = q1;
            v = Vec_$ctor_Z7AD9E565(a_33.X - b_33.X, a_33.Y - b_33.Y, a_33.Z - b_33.Z);
            let div;
            const a_34 = v;
            const b_34 = n;
            div = (((a_34.X * b_34.X) + (a_34.Y * b_34.Y)) + (a_34.Z * b_34.Z));
            if (!(Math.abs(div) > 1E-24)) {
                return undefined;
            }
            else {
                const t = ((a_35 = ((a_36 = p1, (b_36 = q1, Vec_$ctor_Z7AD9E565(a_36.X - b_36.X, a_36.Y - b_36.Y, a_36.Z - b_36.Z)))), (b_35 = n, ((a_35.X * b_35.X) + (a_35.Y * b_35.Y)) + (a_35.Z * b_35.Z)))) / div;
                if ((x_6 = t, (x_6 >= 0) && (x_6 <= 1))) {
                    return (p = q1, (v_1 = ((a_37 = v, (f = t, Vec_$ctor_Z7AD9E565(a_37.X * f, a_37.Y * f, a_37.Z * f)))), Pnt_$ctor_Z7AD9E565(p.X + v_1.X, p.Y + v_1.Y, p.Z + v_1.Z)));
                }
                else {
                    return undefined;
                }
            }
        }
        else {
            return undefined;
        }
    }
}

export function Intersect_lineTriangle(line, p1, p2, p3) {
    return Tria3D_intersectLine_Z60F2B6CD(line, p1, p2, p3);
}

export function Intersect_lineCone(ln, coneRadius, coneBaseZ, coneTipZ) {
    return Euclid_Line3D__Line3D_intersectCone_Static_1D0AE3BB(ln, coneRadius, coneBaseZ, coneTipZ);
}

