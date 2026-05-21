
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { Vc_$ctor_7B00E9A0 } from "./Vc.js";
import { failUnit2 } from "./EuclidErrors.js";
import { UnitVc_$ctor_7B00E9A0 } from "./UnitVc.js";
import { Pt_$ctor_7B00E9A0 } from "./Pt.js";

/**
 * A type containing only static member functions for operations on 2D Triangles.
 */
export class Tria2D {
    constructor() {
    }
}

export function Tria2D_$reflection() {
    return class_type("Euclid.Tria2D", undefined, Tria2D);
}

/**
 * Checks if three points are in one line.
 * By finding the biggest angle in the triangle.
 * And then measuring the distance from this point to the line defined by the other two points.
 */
export function Tria2D_isLinear_365C4B06(a, b, c, distanceTolerance = 1E-06) {
    let tupledArg, lnFrom, lnTo, p, x_3, y_3, t, u$0027, v$0027, tupledArg_1, lnFrom_1, lnTo_1, p_1, x_4, y_4, t_1, u$0027_1, v$0027_1, tupledArg_2, lnFrom_2, lnTo_2, p_2, x_5, y_5, t_2, u$0027_2, v$0027_2;
    let ab;
    const a_1 = b;
    const b_1 = a;
    ab = Vc_$ctor_7B00E9A0(a_1.X - b_1.X, a_1.Y - b_1.Y);
    let bc;
    const a_2 = c;
    const b_2 = b;
    bc = Vc_$ctor_7B00E9A0(a_2.X - b_2.X, a_2.Y - b_2.Y);
    let ca;
    const a_3 = a;
    const b_3 = c;
    ca = Vc_$ctor_7B00E9A0(a_3.X - b_3.X, a_3.Y - b_3.Y);
    let abLenSq;
    const v = ab;
    const x = v.X;
    const y = v.Y;
    abLenSq = ((x * x) + (y * y));
    let bcLenSq;
    const v_1 = bc;
    const x_1 = v_1.X;
    const y_1 = v_1.Y;
    bcLenSq = ((x_1 * x_1) + (y_1 * y_1));
    let caLenSq;
    const v_2 = ca;
    const x_2 = v_2.X;
    const y_2 = v_2.Y;
    caLenSq = ((x_2 * x_2) + (y_2 * y_2));
    const distSq = distanceTolerance * distanceTolerance;
    if (((abLenSq < distSq) ? true : (bcLenSq < distSq)) ? true : (caLenSq < distSq)) {
        return true;
    }
    else {
        let dotA;
        const a_4 = ab;
        const b_4 = ca;
        dotA = ((a_4.X * b_4.X) + (a_4.Y * b_4.Y));
        let dotB;
        const a_5 = bc;
        const b_5 = ab;
        dotB = ((a_5.X * b_5.X) + (a_5.Y * b_5.Y));
        let dotC;
        const a_6 = ca;
        const b_6 = bc;
        dotC = ((a_6.X * b_6.X) + (a_6.Y * b_6.Y));
        if ((dotA > dotB) && (dotA > dotC)) {
            return ((tupledArg = [b, c, a, bcLenSq], (lnFrom = tupledArg[0], (lnTo = tupledArg[1], (p = tupledArg[2], (x_3 = (lnFrom.X - lnTo.X), (y_3 = (lnFrom.Y - lnTo.Y), (t = (((x_3 * (lnFrom.X - p.X)) + (y_3 * (lnFrom.Y - p.Y))) / tupledArg[3]), (u$0027 = ((lnFrom.X - (x_3 * t)) - p.X), (v$0027 = ((lnFrom.Y - (y_3 * t)) - p.Y), (u$0027 * u$0027) + (v$0027 * v$0027))))))))))) < distSq;
        }
        else if ((dotB > dotA) && (dotB > dotC)) {
            return ((tupledArg_1 = [c, a, b, caLenSq], (lnFrom_1 = tupledArg_1[0], (lnTo_1 = tupledArg_1[1], (p_1 = tupledArg_1[2], (x_4 = (lnFrom_1.X - lnTo_1.X), (y_4 = (lnFrom_1.Y - lnTo_1.Y), (t_1 = (((x_4 * (lnFrom_1.X - p_1.X)) + (y_4 * (lnFrom_1.Y - p_1.Y))) / tupledArg_1[3]), (u$0027_1 = ((lnFrom_1.X - (x_4 * t_1)) - p_1.X), (v$0027_1 = ((lnFrom_1.Y - (y_4 * t_1)) - p_1.Y), (u$0027_1 * u$0027_1) + (v$0027_1 * v$0027_1))))))))))) < distSq;
        }
        else {
            return ((tupledArg_2 = [a, b, c, abLenSq], (lnFrom_2 = tupledArg_2[0], (lnTo_2 = tupledArg_2[1], (p_2 = tupledArg_2[2], (x_5 = (lnFrom_2.X - lnTo_2.X), (y_5 = (lnFrom_2.Y - lnTo_2.Y), (t_2 = (((x_5 * (lnFrom_2.X - p_2.X)) + (y_5 * (lnFrom_2.Y - p_2.Y))) / tupledArg_2[3]), (u$0027_2 = ((lnFrom_2.X - (x_5 * t_2)) - p_2.X), (v$0027_2 = ((lnFrom_2.Y - (y_5 * t_2)) - p_2.Y), (u$0027_2 * u$0027_2) + (v$0027_2 * v$0027_2))))))))))) < distSq;
        }
    }
}

/**
 * Offsets one point by a given distance.
 * If the points 'prev', 'this' and 'next' are in counter-clockwise order, the offset is inwards.
 * Otherwise it is outwards.
 * A negative offset distance inverts the direction.
 */
export function Tria2D_offsetPt_365C4B06(ptToOffset, prev, next, dist) {
    let vPrev;
    let v_1;
    const a = ptToOffset;
    const b = prev;
    v_1 = Vc_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y);
    const x = v_1.X;
    const y = v_1.Y;
    const l = Math.sqrt((x * x) + (y * y));
    if (!(l > 1E-12)) {
        failUnit2("Vc.unitize", x, y);
    }
    vPrev = UnitVc_$ctor_7B00E9A0(x / l, y / l);
    let vNext;
    let v_3;
    const a_1 = next;
    const b_1 = ptToOffset;
    v_3 = Vc_$ctor_7B00E9A0(a_1.X - b_1.X, a_1.Y - b_1.Y);
    const x_3 = v_3.X;
    const y_2 = v_3.Y;
    const l_1 = Math.sqrt((x_3 * x_3) + (y_2 * y_2));
    if (!(l_1 > 1E-12)) {
        failUnit2("Vc.unitize", x_3, y_2);
    }
    vNext = UnitVc_$ctor_7B00E9A0(x_3 / l_1, y_2 / l_1);
    let nPrev;
    const v_5 = vPrev;
    nPrev = UnitVc_$ctor_7B00E9A0(-v_5.Y, v_5.X);
    let nNext;
    const v_7 = vNext;
    nNext = UnitVc_$ctor_7B00E9A0(-v_7.Y, v_7.X);
    let cosine;
    const a_2 = nPrev;
    const b_2 = nNext;
    cosine = ((a_2.X * b_2.X) + (a_2.Y * b_2.Y));
    if (cosine < -0.9998476951563913) {
        const p = ptToOffset;
        let v_10;
        let v_9;
        const a_3 = vPrev;
        const b_3 = vNext;
        v_9 = Vc_$ctor_7B00E9A0(a_3.X - b_3.X, a_3.Y - b_3.Y);
        v_10 = Vc_$ctor_7B00E9A0(v_9.X * 28.645, v_9.Y * 28.645);
        return Pt_$ctor_7B00E9A0(p.X + v_10.X, p.Y + v_10.Y);
    }
    else {
        const p_1 = ptToOffset;
        let v_12;
        let v_11;
        const a_4 = nPrev;
        const b_4 = nNext;
        v_11 = Vc_$ctor_7B00E9A0(a_4.X + b_4.X, a_4.Y + b_4.Y);
        const f_1 = dist / (1 + cosine);
        v_12 = Vc_$ctor_7B00E9A0(v_11.X * f_1, v_11.Y * f_1);
        return Pt_$ctor_7B00E9A0(p_1.X + v_12.X, p_1.Y + v_12.Y);
    }
}

/**
 * Offsets all points by a given distance.
 * If the points are in counter-clockwise order, the offset is inwards.
 * Otherwise it is outwards.
 * A negative offset distance inverts the direction.
 */
export function Tria2D_offset_365C4B06(a, b, c, dist) {
    let pt_1, nPrev_1, nNext_1, cosine_1, a_4, b_4, p, v_16, v_15, a_7, v_13, b_7, v_14, p_1, v_18, v_17, a_8, b_8, f_1, pt_4, nPrev_4, nNext_4, cosine_4, p_2, v_23, v_22, a_9, v_20, b_9, v_21, p_3, v_25, v_24, a_10, b_10, f_3, pt_7, nPrev_7, nNext_7, cosine_7, p_4, v_30, v_29, a_11, v_27, b_11, v_28, p_5, v_32, v_31, a_12, b_12, f_5;
    let na;
    let v_3;
    let v_1;
    const a_1 = b;
    const b_1 = a;
    v_1 = Vc_$ctor_7B00E9A0(a_1.X - b_1.X, a_1.Y - b_1.Y);
    const x = v_1.X;
    const y = v_1.Y;
    const l = Math.sqrt((x * x) + (y * y));
    if (!(l > 1E-12)) {
        failUnit2("Vc.unitize", x, y);
    }
    v_3 = UnitVc_$ctor_7B00E9A0(x / l, y / l);
    na = UnitVc_$ctor_7B00E9A0(-v_3.Y, v_3.X);
    let nb;
    let v_7;
    let v_5;
    const a_2 = c;
    const b_2 = b;
    v_5 = Vc_$ctor_7B00E9A0(a_2.X - b_2.X, a_2.Y - b_2.Y);
    const x_4 = v_5.X;
    const y_3 = v_5.Y;
    const l_1 = Math.sqrt((x_4 * x_4) + (y_3 * y_3));
    if (!(l_1 > 1E-12)) {
        failUnit2("Vc.unitize", x_4, y_3);
    }
    v_7 = UnitVc_$ctor_7B00E9A0(x_4 / l_1, y_3 / l_1);
    nb = UnitVc_$ctor_7B00E9A0(-v_7.Y, v_7.X);
    let nc;
    let v_11;
    let v_9;
    const a_3 = a;
    const b_3 = c;
    v_9 = Vc_$ctor_7B00E9A0(a_3.X - b_3.X, a_3.Y - b_3.Y);
    const x_8 = v_9.X;
    const y_6 = v_9.Y;
    const l_2 = Math.sqrt((x_8 * x_8) + (y_6 * y_6));
    if (!(l_2 > 1E-12)) {
        failUnit2("Vc.unitize", x_8, y_6);
    }
    v_11 = UnitVc_$ctor_7B00E9A0(x_8 / l_2, y_6 / l_2);
    nc = UnitVc_$ctor_7B00E9A0(-v_11.Y, v_11.X);
    let cosineB;
    const a_5 = na;
    const b_5 = nb;
    cosineB = ((a_5.X * b_5.X) + (a_5.Y * b_5.Y));
    let cosineC;
    const a_6 = nb;
    const b_6 = nc;
    cosineC = ((a_6.X * b_6.X) + (a_6.Y * b_6.Y));
    return [(pt_1 = a, (nPrev_1 = na, (nNext_1 = nc, (cosine_1 = ((a_4 = na, (b_4 = nc, (a_4.X * b_4.X) + (a_4.Y * b_4.Y)))), (cosine_1 < -0.9998476951563913) ? ((p = pt_1, (v_16 = ((v_15 = ((a_7 = ((v_13 = nPrev_1, UnitVc_$ctor_7B00E9A0(-v_13.Y, v_13.X))), (b_7 = ((v_14 = nNext_1, UnitVc_$ctor_7B00E9A0(v_14.Y, -v_14.X))), Vc_$ctor_7B00E9A0(a_7.X + b_7.X, a_7.Y + b_7.Y)))), Vc_$ctor_7B00E9A0(v_15.X * 28.645, v_15.Y * 28.645))), Pt_$ctor_7B00E9A0(p.X + v_16.X, p.Y + v_16.Y)))) : ((p_1 = pt_1, (v_18 = ((v_17 = ((a_8 = nPrev_1, (b_8 = nNext_1, Vc_$ctor_7B00E9A0(a_8.X + b_8.X, a_8.Y + b_8.Y)))), (f_1 = (dist / (1 + cosine_1)), Vc_$ctor_7B00E9A0(v_17.X * f_1, v_17.Y * f_1)))), Pt_$ctor_7B00E9A0(p_1.X + v_18.X, p_1.Y + v_18.Y)))))))), (pt_4 = b, (nPrev_4 = na, (nNext_4 = nb, (cosine_4 = cosineB, (cosine_4 < -0.9998476951563913) ? ((p_2 = pt_4, (v_23 = ((v_22 = ((a_9 = ((v_20 = nPrev_4, UnitVc_$ctor_7B00E9A0(-v_20.Y, v_20.X))), (b_9 = ((v_21 = nNext_4, UnitVc_$ctor_7B00E9A0(v_21.Y, -v_21.X))), Vc_$ctor_7B00E9A0(a_9.X + b_9.X, a_9.Y + b_9.Y)))), Vc_$ctor_7B00E9A0(v_22.X * 28.645, v_22.Y * 28.645))), Pt_$ctor_7B00E9A0(p_2.X + v_23.X, p_2.Y + v_23.Y)))) : ((p_3 = pt_4, (v_25 = ((v_24 = ((a_10 = nPrev_4, (b_10 = nNext_4, Vc_$ctor_7B00E9A0(a_10.X + b_10.X, a_10.Y + b_10.Y)))), (f_3 = (dist / (1 + cosine_4)), Vc_$ctor_7B00E9A0(v_24.X * f_3, v_24.Y * f_3)))), Pt_$ctor_7B00E9A0(p_3.X + v_25.X, p_3.Y + v_25.Y)))))))), (pt_7 = c, (nPrev_7 = nb, (nNext_7 = nc, (cosine_7 = cosineC, (cosine_7 < -0.9998476951563913) ? ((p_4 = pt_7, (v_30 = ((v_29 = ((a_11 = ((v_27 = nPrev_7, UnitVc_$ctor_7B00E9A0(-v_27.Y, v_27.X))), (b_11 = ((v_28 = nNext_7, UnitVc_$ctor_7B00E9A0(v_28.Y, -v_28.X))), Vc_$ctor_7B00E9A0(a_11.X + b_11.X, a_11.Y + b_11.Y)))), Vc_$ctor_7B00E9A0(v_29.X * 28.645, v_29.Y * 28.645))), Pt_$ctor_7B00E9A0(p_4.X + v_30.X, p_4.Y + v_30.Y)))) : ((p_5 = pt_7, (v_32 = ((v_31 = ((a_12 = nPrev_7, (b_12 = nNext_7, Vc_$ctor_7B00E9A0(a_12.X + b_12.X, a_12.Y + b_12.Y)))), (f_5 = (dist / (1 + cosine_7)), Vc_$ctor_7B00E9A0(v_31.X * f_5, v_31.Y * f_5)))), Pt_$ctor_7B00E9A0(p_5.X + v_32.X, p_5.Y + v_32.Y))))))))];
}

/**
 * Finds the offset point based on
 * the previous and next unit normals (= offset direction),
 * and their offset distances.
 * This is the core function for offsetting variable. it is used by Tria2D.offsetVar(..).
 */
export function Tria2D_offsetPtVarByNormals_3ED64C5D(ptToOffset, prevN, nextN, prevDist, nextDist) {
    let p_2, v_4, a_5, f_2, a_4, a_3, b_1, b_2, a_6, b_3;
    let prevPt;
    const p = ptToOffset;
    let v;
    const a = prevN;
    const f = prevDist;
    v = Vc_$ctor_7B00E9A0(a.X * f, a.Y * f);
    prevPt = Pt_$ctor_7B00E9A0(p.X + v.X, p.Y + v.Y);
    let nextPt;
    const p_1 = ptToOffset;
    let v_1;
    const a_1 = nextN;
    const f_1 = nextDist;
    v_1 = Vc_$ctor_7B00E9A0(a_1.X * f_1, a_1.Y * f_1);
    nextPt = Pt_$ctor_7B00E9A0(p_1.X + v_1.X, p_1.Y + v_1.Y);
    let prevDir;
    const v_2 = prevN;
    prevDir = UnitVc_$ctor_7B00E9A0(-v_2.Y, v_2.X);
    let nextDir;
    const v_3 = nextN;
    nextDir = UnitVc_$ctor_7B00E9A0(-v_3.Y, v_3.X);
    let aXb;
    const a_2 = prevDir;
    const b = nextDir;
    aXb = ((a_2.X * b.Y) - (a_2.Y * b.X));
    if (Math.abs(aXb) > 1E-12) {
        return (p_2 = prevPt, (v_4 = ((a_5 = prevDir, (f_2 = (((a_4 = ((a_3 = nextPt, (b_1 = prevPt, Vc_$ctor_7B00E9A0(a_3.X - b_1.X, a_3.Y - b_1.Y)))), (b_2 = nextDir, (a_4.X * b_2.Y) - (a_4.Y * b_2.X)))) / aXb), Vc_$ctor_7B00E9A0(a_5.X * f_2, a_5.Y * f_2)))), Pt_$ctor_7B00E9A0(p_2.X + v_4.X, p_2.Y + v_4.Y)));
    }
    else if ((Math.abs(prevDist - nextDist) < 1E-12) && (((a_6 = prevDir, (b_3 = nextDir, (a_6.X * b_3.X) + (a_6.Y * b_3.Y)))) > 0)) {
        return prevPt;
    }
    else {
        return undefined;
    }
}

/**
 * Finds the offset point based on
 * the previous and next points,
 * and their offset distances.
 * A positive offset distance will be to the left side of the line from 'prev' to 'ptToOffset'.
 * A negative offset distance inverts the direction.
 * This function use Tria2D.offsetVarByNormals(..) internally.
 */
export function Tria2D_offsetPtVar_5FDDACFD(ptToOffset, prev, next, distPrev, distNext) {
    let v_5, v_7;
    let vPrev;
    let v_1;
    const a = ptToOffset;
    const b = prev;
    v_1 = Vc_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y);
    const x = v_1.X;
    const y = v_1.Y;
    const l = Math.sqrt((x * x) + (y * y));
    if (!(l > 1E-12)) {
        failUnit2("Vc.unitize", x, y);
    }
    vPrev = UnitVc_$ctor_7B00E9A0(x / l, y / l);
    let vNext;
    let v_3;
    const a_1 = next;
    const b_1 = ptToOffset;
    v_3 = Vc_$ctor_7B00E9A0(a_1.X - b_1.X, a_1.Y - b_1.Y);
    const x_3 = v_3.X;
    const y_2 = v_3.Y;
    const l_1 = Math.sqrt((x_3 * x_3) + (y_2 * y_2));
    if (!(l_1 > 1E-12)) {
        failUnit2("Vc.unitize", x_3, y_2);
    }
    vNext = UnitVc_$ctor_7B00E9A0(x_3 / l_1, y_2 / l_1);
    return Tria2D_offsetPtVarByNormals_3ED64C5D(ptToOffset, (v_5 = vPrev, UnitVc_$ctor_7B00E9A0(-v_5.Y, v_5.X)), (v_7 = vNext, UnitVc_$ctor_7B00E9A0(-v_7.Y, v_7.X)), distPrev, distNext);
}

