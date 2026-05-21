
import { Pt_$ctor_7B00E9A0 } from "../Pt.js";
import { Vc_$ctor_7B00E9A0 } from "../Vc.js";
import { failTooSmall } from "../EuclidErrors.js";
import { UnitVc_$ctor_7B00E9A0 } from "../UnitVc.js";

/**
 * Rotates a point by a given number of quarter-circles (i.e. multiples of 90
 * degrees or Pi/2 radians). A positive number rotates counter-clockwise, a
 * negative number rotates clockwise. The length of the vector is preserved.
 */
export function Euclid_Pt__Pt_rotateByQuarterCircle_Static(numberOfQuarters, v) {
    let nQuad = numberOfQuarters % 4;
    if (nQuad < 0) {
        nQuad = ((nQuad + 4) | 0);
    }
    switch (nQuad) {
        case 0:
            return v;
        case 1:
            return Pt_$ctor_7B00E9A0(-v.Y, v.X);
        case 2:
            return Pt_$ctor_7B00E9A0(-v.X, -v.Y);
        case 3:
            return Pt_$ctor_7B00E9A0(v.Y, -v.X);
        default:
            return v;
    }
}

/**
 * Check if two 2D points are not equal within a given tolerance.
 * Use a tolerance of 0.0 to check if the two points are not exactly equal.
 */
export function Euclid_Pt__Pt_notEquals_Static(tol, a, b) {
    if (Math.abs(a.X - b.X) > tol) {
        return true;
    }
    else {
        return Math.abs(a.Y - b.Y) > tol;
    }
}

/**
 * Returns angle in Degrees at mid point (thisPt).
 */
export function Euclid_Pt__Pt_angleInCorner_Static_50BD389D(prevPt, thisPt, nextPt) {
    let a_6, v, x, y, l, b_6, v_1, x_3, y_2, l_1, dot, a_7, b_7, x_6, y_4, x_7, y_5;
    let a_1;
    const a = prevPt;
    const b = thisPt;
    a_1 = Vc_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y);
    let b_2;
    const a_2 = nextPt;
    const b_1 = thisPt;
    b_2 = Vc_$ctor_7B00E9A0(a_2.X - b_1.X, a_2.Y - b_1.Y);
    return 57.29577951308232 * ((a_6 = ((v = a_1, (x = v.X, (y = v.Y, (l = Math.sqrt((x * x) + (y * y)), (!(l > 1E-12) ? failTooSmall("Vc.Unitized", v) : undefined, UnitVc_$ctor_7B00E9A0(x / l, y / l))))))), (b_6 = ((v_1 = b_2, (x_3 = v_1.X, (y_2 = v_1.Y, (l_1 = Math.sqrt((x_3 * x_3) + (y_2 * y_2)), (!(l_1 > 1E-12) ? failTooSmall("Vc.Unitized", v_1) : undefined, UnitVc_$ctor_7B00E9A0(x_3 / l_1, y_2 / l_1))))))), (dot = ((a_7 = a_6, (b_7 = b_6, (a_7.X * b_7.X) + (a_7.Y * b_7.Y)))), ((-0.98 < dot) && (dot < 0.98)) ? Math.acos(dot) : ((dot < 0) ? (3.141592653589793 - (2 * Math.asin(((x_6 = (b_6.X - -a_6.X), (y_4 = (b_6.Y - -a_6.Y), Math.sqrt((x_6 * x_6) + (y_4 * y_4))))) * 0.5))) : (2 * Math.asin(((x_7 = (b_6.X - a_6.X), (y_5 = (b_6.Y - a_6.Y), Math.sqrt((x_7 * x_7) + (y_5 * y_5))))) * 0.5)))))));
}

/**
 * Returns the closer point of the two points to the reference given point.
 * When both points are equally close, the first point is returned.
 */
export function Euclid_Pt__Pt_closestOfTwo_Static(pt1, pt2, referencePoint) {
    let d1;
    const a_1 = pt1;
    const b_1 = referencePoint;
    const vx = a_1.X - b_1.X;
    const vy = a_1.Y - b_1.Y;
    d1 = ((vx * vx) + (vy * vy));
    let d2;
    const a_3 = pt2;
    const b_3 = referencePoint;
    const vx_1 = a_3.X - b_3.X;
    const vy_1 = a_3.Y - b_3.Y;
    d2 = ((vx_1 * vx_1) + (vy_1 * vy_1));
    if (d1 <= d2) {
        return pt1;
    }
    else {
        return pt2;
    }
}

