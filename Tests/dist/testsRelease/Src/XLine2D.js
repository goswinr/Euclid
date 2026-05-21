
import { Union } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type, union_type, float64_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { Pt_$ctor_7B00E9A0, Pt_$reflection } from "./Pt.js";
import { min } from "../fable_modules/fable-library-js.5.0.0/Double.js";

/**
 * Describes the possible intersection parameters of two rays (rays are 2D lines extended infinitely in both directions).
 * Returns parameters on both lines if they intersect.
 */
export class XLine2DModule_XRayParam extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Intersect", "Parallel", "TooShortA", "TooShortB", "TooShortBoth"];
    }
}

export function XLine2DModule_XRayParam_$reflection() {
    return union_type("Euclid.XLine2DModule.XRayParam", [], XLine2DModule_XRayParam, () => [[["twoParams", float64_type], ["Item2", float64_type]], [], [], [], []]);
}

/**
 * Describes the possible intersection point of two rays (rays are 2D lines extended infinitely in both directions).
 * Returns the intersection point if they intersect.
 */
export class XLine2DModule_XRay extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Intersect", "Parallel", "TooShortA", "TooShortB", "TooShortBoth"];
    }
}

export function XLine2DModule_XRay_$reflection() {
    return union_type("Euclid.XLine2DModule.XRay", [], XLine2DModule_XRay, () => [[["xPt", Pt_$reflection()]], [], [], [], []]);
}

/**
 * Describes the intersection parameters of two 2D lines.
 * Returns the parameters in the range 0.0 to 1.0 on both lines if they intersect.
 */
export class XLine2DModule_XParam extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Intersect", "Apart", "Parallel", "TooShortA", "TooShortB", "TooShortBoth"];
    }
}

export function XLine2DModule_XParam_$reflection() {
    return union_type("Euclid.XLine2DModule.XParam", [], XLine2DModule_XParam, () => [[["twoParams", float64_type], ["Item2", float64_type]], [], [], [], [], []]);
}

/**
 * Describes the possible relationships of two finite 2D lines.
 * Returns the intersection point if they intersect.
 */
export class XLine2DModule_XPt extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Intersect", "Apart", "Parallel", "TooShortA", "TooShortB", "TooShortBoth"];
    }
}

export function XLine2DModule_XPt_$reflection() {
    return union_type("Euclid.XLine2DModule.XPt", [], XLine2DModule_XPt, () => [[["xPt", Pt_$reflection()]], [], [], [], [], []]);
}

/**
 * Describes the possible cases of the closest parameters between finite 2D lines.
 */
export class XLine2DModule_ClParams extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Intersect", "Parallel", "Apart", "TooShortA", "TooShortB", "TooShortBoth"];
    }
}

export function XLine2DModule_ClParams_$reflection() {
    return union_type("Euclid.XLine2DModule.ClParams", [], XLine2DModule_ClParams, () => [[["paramA", float64_type], ["paramB", float64_type]], [["paramA", float64_type], ["paramB", float64_type]], [["paramA", float64_type], ["paramB", float64_type], ["squareDist", float64_type]], [], [], []]);
}

/**
 * Describes the possible cases of the closest points between finite 2D lines.
 */
export class XLine2DModule_ClPts extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Intersect", "Parallel", "Apart", "TooShortA", "TooShortB", "TooShortBoth"];
    }
}

export function XLine2DModule_ClPts_$reflection() {
    return union_type("Euclid.XLine2DModule.ClPts", [], XLine2DModule_ClPts, () => [[["pt", Pt_$reflection()]], [["ptA", Pt_$reflection()], ["ptB", Pt_$reflection()]], [["ptA", Pt_$reflection()], ["ptB", Pt_$reflection()], ["squareDist", float64_type]], [], [], []]);
}

/**
 * Describes the possible cases of two finite 2D lines touching at their ends.
 * Does not check if they are parallel or intersecting.
 */
export class XLine2DModule_XEnds extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["NotTouching", "StartA_StartB", "EndA_EndB", "EndA_StartB", "StartA_EndB", "Identical", "IdenticalFlipped"];
    }
}

export function XLine2DModule_XEnds_$reflection() {
    return union_type("Euclid.XLine2DModule.XEnds", [], XLine2DModule_XEnds, () => [[], [], [], [], [], [], []]);
}

/**
 * A type containing only static member functions for computing 2D line intersections.
 * Some functions return Discriminated Unions from the XLine2D module.
 */
export class XLine2D {
    constructor() {
    }
}

export function XLine2D_$reflection() {
    return class_type("Euclid.XLine2D", undefined, XLine2D);
}

/**
 * Checks if 2D lines are not only parallel and coincident but are also overlapping or at least touching at their ends.
 */
export function XLine2D_doOverlap_199764BB(pAx, pAy, pBx, pBy, vAx, vAy, vBx, vBy, tolerance = 1E-06) {
    let x_2, x_3;
    let t;
    const vAx_1 = vAx;
    const vAy_1 = vAy;
    t = (((vAx_1 * (pBx - pAx)) + (vAy_1 * (pBy - pAy))) / ((vAx_1 * vAx_1) + (vAy_1 * vAy_1)));
    const vx = (pAx + (vAx * t)) - pBx;
    const vy = (pAy + (vAy * t)) - pBy;
    const sqTolerance = tolerance * tolerance;
    if (((vx * vx) + (vy * vy)) > sqTolerance) {
        return false;
    }
    else {
        const bex = pBx + vBx;
        const bey = pBy + vBy;
        let u_2;
        const vAx_2 = vAx;
        const vAy_2 = vAy;
        u_2 = (((vAx_2 * (bex - pAx)) + (vAy_2 * (bey - pAy))) / ((vAx_2 * vAx_2) + (vAy_2 * vAy_2)));
        const vx2 = (pAx + (vAx * u_2)) - bex;
        const vy2 = (pAy + (vAy * u_2)) - bey;
        if (((vx2 * vx2) + (vy2 * vy2)) > sqTolerance) {
            return false;
        }
        else if ((x_2 = t, (-1E-06 < x_2) && (x_2 < 1.000001))) {
            return true;
        }
        else if ((x_3 = u_2, (-1E-06 < x_3) && (x_3 < 1.000001))) {
            return true;
        }
        else if ((t < 0) && (u_2 > 1)) {
            return true;
        }
        else if (u_2 < 0) {
            return t > 1;
        }
        else {
            return false;
        }
    }
}

/**
 * Tries to get intersection parameters of two rays (rays are 2D lines extended infinitely).
 */
export function XLine2D_getRayIntersectionParam_40607834(pAx, pAy, pBx, pBy, vAx, vAy, vBx, vBy, tangent = 0.004363350820701567, tooShortTolerance = 1E-06) {
    if ((Math.abs(vAx) + Math.abs(vAy)) < tooShortTolerance) {
        if ((Math.abs(vBx) + Math.abs(vBy)) < tooShortTolerance) {
            return new XLine2DModule_XRayParam(4, []);
        }
        else {
            return new XLine2DModule_XRayParam(2, []);
        }
    }
    else if ((Math.abs(vBx) + Math.abs(vBy)) < tooShortTolerance) {
        return new XLine2DModule_XRayParam(3, []);
    }
    else {
        const det = (vAx * vBy) - (vAy * vBx);
        const tan = det / ((vAx * vBx) + (vAy * vBy));
        if (Math.abs(tan) > tangent) {
            const dx = pBx - pAx;
            const dy = pBy - pAy;
            const tupledArg = [((dx * vBy) - (dy * vBx)) / det, ((dx * vAy) - (dy * vAx)) / det];
            return new XLine2DModule_XRayParam(0, [tupledArg[0], tupledArg[1]]);
        }
        else {
            return new XLine2DModule_XRayParam(1, []);
        }
    }
}

/**
 * Tries to get intersection point of two rays (rays are 2D lines extended infinitely).
 */
export function XLine2D_getRayIntersection_40607834(pAx, pAy, pBx, pBy, vAx, vAy, vBx, vBy, tangent = 0.004363350820701567, tooShortTolerance = 1E-06) {
    if ((Math.abs(vAx) + Math.abs(vAy)) < tooShortTolerance) {
        if ((Math.abs(vBx) + Math.abs(vBy)) < tooShortTolerance) {
            return new XLine2DModule_XRay(4, []);
        }
        else {
            return new XLine2DModule_XRay(2, []);
        }
    }
    else if ((Math.abs(vBx) + Math.abs(vBy)) < tooShortTolerance) {
        return new XLine2DModule_XRay(3, []);
    }
    else {
        const det = (vAx * vBy) - (vAy * vBx);
        const tan = det / ((vAx * vBx) + (vAy * vBy));
        if (Math.abs(tan) > tangent) {
            const t = (((pBx - pAx) * vBy) - ((pBy - pAy) * vBx)) / det;
            return new XLine2DModule_XRay(0, [Pt_$ctor_7B00E9A0(pAx + (t * vAx), pAy + (t * vAy))]);
        }
        else {
            return new XLine2DModule_XRay(1, []);
        }
    }
}

/**
 * Tries to get intersection parameters of two finite 2D-lines.
 */
export function XLine2D_getIntersectionParam_40607834(pAx, pAy, pBx, pBy, vAx, vAy, vBx, vBy, tangent = 0.004363350820701567, tooShortTolerance = 1E-06) {
    if ((Math.abs(vAx) + Math.abs(vAy)) < tooShortTolerance) {
        if ((Math.abs(vBx) + Math.abs(vBy)) < tooShortTolerance) {
            return new XLine2DModule_XParam(5, []);
        }
        else {
            return new XLine2DModule_XParam(3, []);
        }
    }
    else if ((Math.abs(vBx) + Math.abs(vBy)) < tooShortTolerance) {
        return new XLine2DModule_XParam(4, []);
    }
    else {
        const det = (vAx * vBy) - (vAy * vBx);
        const tan = det / ((vAx * vBx) + (vAy * vBy));
        if (Math.abs(tan) > tangent) {
            const dx = pBx - pAx;
            const dy = pBy - pAy;
            const t = ((dx * vBy) - (dy * vBx)) / det;
            const u = ((dx * vAy) - (dy * vAx)) / det;
            if ((((t > -1E-06) && (t < 1.000001)) && (u > -1E-06)) && (u < 1.000001)) {
                return new XLine2DModule_XParam(0, [t, u]);
            }
            else {
                return new XLine2DModule_XParam(1, []);
            }
        }
        else {
            return new XLine2DModule_XParam(2, []);
        }
    }
}

/**
 * Tries to get intersection point of two finite 2D lines.
 */
export function XLine2D_getIntersection_40607834(pAx, pAy, pBx, pBy, vAx, vAy, vBx, vBy, tangent = 0.004363350820701567, tooShortTolerance = 1E-06) {
    if ((Math.abs(vAx) + Math.abs(vAy)) < tooShortTolerance) {
        if ((Math.abs(vBx) + Math.abs(vBy)) < tooShortTolerance) {
            return new XLine2DModule_XPt(5, []);
        }
        else {
            return new XLine2DModule_XPt(3, []);
        }
    }
    else if ((Math.abs(vBx) + Math.abs(vBy)) < tooShortTolerance) {
        return new XLine2DModule_XPt(4, []);
    }
    else {
        const det = (vAx * vBy) - (vAy * vBx);
        const tan = det / ((vAx * vBx) + (vAy * vBy));
        if (Math.abs(tan) > tangent) {
            const dx = pBx - pAx;
            const dy = pBy - pAy;
            const t = ((dx * vBy) - (dy * vBx)) / det;
            const u = ((dx * vAy) - (dy * vAx)) / det;
            if ((((t > -1E-06) && (t < 1.000001)) && (u > -1E-06)) && (u < 1.000001)) {
                return new XLine2DModule_XPt(0, [Pt_$ctor_7B00E9A0(pAx + (t * vAx), pAy + (t * vAy))]);
            }
            else {
                return new XLine2DModule_XPt(1, []);
            }
        }
        else {
            return new XLine2DModule_XPt(2, []);
        }
    }
}

/**
 * Gets the parameters on the finite lines where the lines are closest to each other.
 * For parallel or coincident lines, this returns the midpoint of the interval spanned by the projected endpoints
 * (which coincides with the middle of any overlapping segment when one exists).
 */
export function XLine2D_getClosestParameters_40607834(pAx, pAy, pBx, pBy, vAx, vAy, vBx, vBy, tangent = 0.004363350820701567, tooShortTolerance = 1E-06) {
    let x_8, vAx_4, vAy_4, x_12, vAx_6, vAy_6, x_13, x_14, x_15, x_16, x_21, vAx_9, vAy_9, x_25, vAx_11, vAy_11;
    if ((Math.abs(vAx) + Math.abs(vAy)) < tooShortTolerance) {
        if ((Math.abs(vBx) + Math.abs(vBy)) < tooShortTolerance) {
            return new XLine2DModule_ClParams(5, []);
        }
        else {
            return new XLine2DModule_ClParams(3, []);
        }
    }
    else if ((Math.abs(vBx) + Math.abs(vBy)) < tooShortTolerance) {
        return new XLine2DModule_ClParams(4, []);
    }
    else {
        const det = (vAx * vBy) - (vAy * vBx);
        const tan = det / ((vAx * vBx) + (vAy * vBy));
        if (Math.abs(tan) > tangent) {
            const dx = pBx - pAx;
            const dy = pBy - pAy;
            const t = ((dx * vBy) - (dy * vBx)) / det;
            const u = ((dx * vAy) - (dy * vAx)) / det;
            if ((((t >= -1E-06) && (t <= 1.000001)) && (u >= -1E-06)) && (u <= 1.000001)) {
                return new XLine2DModule_ClParams(0, [t, u]);
            }
            else {
                let tupledArg;
                const pAx_1 = pAx;
                const pAy_1 = pAy;
                const pBx_1 = pBx;
                const pBy_1 = pBy;
                const vAx_1 = vAx;
                const vAy_1 = vAy;
                const vBx_1 = vBx;
                const vBy_1 = vBy;
                let patternInput;
                const pAx_2 = pAx_1;
                const pAy_2 = pAy_1;
                const pBx_2 = pBx_1;
                const pBy_2 = pBy_1;
                const vAx_2 = vAx_1;
                const vAy_2 = vAy_1;
                const vBx_2 = vBx_1;
                const vBy_2 = vBy_1;
                let uAs;
                let x_6;
                const vAx_3 = vAx_2;
                const vAy_3 = vAy_2;
                x_6 = (((vAx_3 * (pBx_2 - pAx_2)) + (vAy_3 * (pBy_2 - pAy_2))) / ((vAx_3 * vAx_3) + (vAy_3 * vAy_3)));
                uAs = ((x_6 > 0) ? ((x_6 < 1) ? x_6 : 1) : 0);
                let uAe;
                let x_10;
                const vAx_5 = vAx_2;
                const vAy_5 = vAy_2;
                x_10 = (((vAx_5 * ((pBx_2 + vBx_2) - pAx_2)) + (vAy_5 * ((pBy_2 + vBy_2) - pAy_2))) / ((vAx_5 * vAx_5) + (vAy_5 * vAy_5)));
                uAe = ((x_10 > 0) ? ((x_10 < 1) ? x_10 : 1) : 0);
                patternInput = [uAs, (x_8 = ((vAx_4 = vBx_2, (vAy_4 = vBy_2, ((vAx_4 * ((pAx_2 + (uAs * vAx_2)) - pBx_2)) + (vAy_4 * ((pAy_2 + (uAs * vAy_2)) - pBy_2))) / ((vAx_4 * vAx_4) + (vAy_4 * vAy_4))))), (x_8 > 0) ? ((x_8 < 1) ? x_8 : 1) : 0), uAe, (x_12 = ((vAx_6 = vBx_2, (vAy_6 = vBy_2, ((vAx_6 * ((pAx_2 + (uAe * vAx_2)) - pBx_2)) + (vAy_6 * ((pAy_2 + (uAe * vAy_2)) - pBy_2))) / ((vAx_6 * vAx_6) + (vAy_6 * vAy_6))))), (x_12 > 0) ? ((x_12 < 1) ? x_12 : 1) : 0)];
                const uBs_1 = patternInput[1];
                const uBe_1 = patternInput[3];
                const uAs_1 = patternInput[0];
                const uAe_1 = patternInput[2];
                const distSqStart = ((x_13 = ((pAx_1 + (uAs_1 * vAx_1)) - (pBx_1 + (uBs_1 * vBx_1))), x_13 * x_13)) + ((x_14 = ((pAy_1 + (uAs_1 * vAy_1)) - (pBy_1 + (uBs_1 * vBy_1))), x_14 * x_14));
                const distSqEnd = ((x_15 = ((pAx_1 + (uAe_1 * vAx_1)) - (pBx_1 + (uBe_1 * vBx_1))), x_15 * x_15)) + ((x_16 = ((pAy_1 + (uAe_1 * vAy_1)) - (pBy_1 + (uBe_1 * vBy_1))), x_16 * x_16));
                tupledArg = ((distSqStart < distSqEnd) ? [uAs_1, uBs_1, distSqStart] : [uAe_1, uBe_1, distSqEnd]);
                return new XLine2DModule_ClParams(2, [tupledArg[0], tupledArg[1], tupledArg[2]]);
            }
        }
        else {
            let patternInput_1;
            const pAx_7 = pAx;
            const pAy_7 = pAy;
            const pBx_3 = pBx;
            const pBy_3 = pBy;
            const vAx_7 = vAx;
            const vAy_7 = vAy;
            const vBx_3 = vBx;
            const vBy_3 = vBy;
            let uAs_2;
            let x_19;
            const vAx_8 = vAx_7;
            const vAy_8 = vAy_7;
            x_19 = (((vAx_8 * (pBx_3 - pAx_7)) + (vAy_8 * (pBy_3 - pAy_7))) / ((vAx_8 * vAx_8) + (vAy_8 * vAy_8)));
            uAs_2 = ((x_19 > 0) ? ((x_19 < 1) ? x_19 : 1) : 0);
            let uAe_2;
            let x_23;
            const vAx_10 = vAx_7;
            const vAy_10 = vAy_7;
            x_23 = (((vAx_10 * ((pBx_3 + vBx_3) - pAx_7)) + (vAy_10 * ((pBy_3 + vBy_3) - pAy_7))) / ((vAx_10 * vAx_10) + (vAy_10 * vAy_10)));
            uAe_2 = ((x_23 > 0) ? ((x_23 < 1) ? x_23 : 1) : 0);
            patternInput_1 = [uAs_2, (x_21 = ((vAx_9 = vBx_3, (vAy_9 = vBy_3, ((vAx_9 * ((pAx_7 + (uAs_2 * vAx_7)) - pBx_3)) + (vAy_9 * ((pAy_7 + (uAs_2 * vAy_7)) - pBy_3))) / ((vAx_9 * vAx_9) + (vAy_9 * vAy_9))))), (x_21 > 0) ? ((x_21 < 1) ? x_21 : 1) : 0), uAe_2, (x_25 = ((vAx_11 = vBx_3, (vAy_11 = vBy_3, ((vAx_11 * ((pAx_7 + (uAe_2 * vAx_7)) - pBx_3)) + (vAy_11 * ((pAy_7 + (uAe_2 * vAy_7)) - pBy_3))) / ((vAx_11 * vAx_11) + (vAy_11 * vAy_11))))), (x_25 > 0) ? ((x_25 < 1) ? x_25 : 1) : 0)];
            return new XLine2DModule_ClParams(1, [(patternInput_1[0] + patternInput_1[2]) * 0.5, (patternInput_1[1] + patternInput_1[3]) * 0.5]);
        }
    }
}

/**
 * Gets the point or points on the finite lines where the lines are closest to each other.
 * For parallel or coincident lines, this returns the midpoint of the interval spanned by the projected endpoints
 * (matching the middle of the overlapping segment when one exists).
 */
export function XLine2D_getClosestPoints_40607834(pAx, pAy, pBx, pBy, vAx, vAy, vBx, vBy, tangent = 0.004363350820701567, tooShortTolerance = 1E-06) {
    let x_8, vAx_4, vAy_4, x_12, vAx_6, vAy_6, x_13, x_14, x_15, x_16, x_21, vAx_9, vAy_9, x_25, vAx_11, vAy_11;
    if ((Math.abs(vAx) + Math.abs(vAy)) < tooShortTolerance) {
        if ((Math.abs(vBx) + Math.abs(vBy)) < tooShortTolerance) {
            return new XLine2DModule_ClPts(5, []);
        }
        else {
            return new XLine2DModule_ClPts(3, []);
        }
    }
    else if ((Math.abs(vBx) + Math.abs(vBy)) < tooShortTolerance) {
        return new XLine2DModule_ClPts(4, []);
    }
    else {
        const det = (vAx * vBy) - (vAy * vBx);
        const tan = det / ((vAx * vBx) + (vAy * vBy));
        if (Math.abs(tan) > tangent) {
            const dx = pBx - pAx;
            const dy = pBy - pAy;
            const t = ((dx * vBy) - (dy * vBx)) / det;
            const u = ((dx * vAy) - (dy * vAx)) / det;
            if ((((t > -1E-06) && (t < 1.000001)) && (u > -1E-06)) && (u < 1.000001)) {
                return new XLine2DModule_ClPts(0, [Pt_$ctor_7B00E9A0(pAx + (t * vAx), pAy + (t * vAy))]);
            }
            else {
                let tupledArg;
                const pAx_1 = pAx;
                const pAy_1 = pAy;
                const pBx_1 = pBx;
                const pBy_1 = pBy;
                const vAx_1 = vAx;
                const vAy_1 = vAy;
                const vBx_1 = vBx;
                const vBy_1 = vBy;
                let patternInput;
                const pAx_2 = pAx_1;
                const pAy_2 = pAy_1;
                const pBx_2 = pBx_1;
                const pBy_2 = pBy_1;
                const vAx_2 = vAx_1;
                const vAy_2 = vAy_1;
                const vBx_2 = vBx_1;
                const vBy_2 = vBy_1;
                let uAs;
                let x_6;
                const vAx_3 = vAx_2;
                const vAy_3 = vAy_2;
                x_6 = (((vAx_3 * (pBx_2 - pAx_2)) + (vAy_3 * (pBy_2 - pAy_2))) / ((vAx_3 * vAx_3) + (vAy_3 * vAy_3)));
                uAs = ((x_6 > 0) ? ((x_6 < 1) ? x_6 : 1) : 0);
                let uAe;
                let x_10;
                const vAx_5 = vAx_2;
                const vAy_5 = vAy_2;
                x_10 = (((vAx_5 * ((pBx_2 + vBx_2) - pAx_2)) + (vAy_5 * ((pBy_2 + vBy_2) - pAy_2))) / ((vAx_5 * vAx_5) + (vAy_5 * vAy_5)));
                uAe = ((x_10 > 0) ? ((x_10 < 1) ? x_10 : 1) : 0);
                patternInput = [uAs, (x_8 = ((vAx_4 = vBx_2, (vAy_4 = vBy_2, ((vAx_4 * ((pAx_2 + (uAs * vAx_2)) - pBx_2)) + (vAy_4 * ((pAy_2 + (uAs * vAy_2)) - pBy_2))) / ((vAx_4 * vAx_4) + (vAy_4 * vAy_4))))), (x_8 > 0) ? ((x_8 < 1) ? x_8 : 1) : 0), uAe, (x_12 = ((vAx_6 = vBx_2, (vAy_6 = vBy_2, ((vAx_6 * ((pAx_2 + (uAe * vAx_2)) - pBx_2)) + (vAy_6 * ((pAy_2 + (uAe * vAy_2)) - pBy_2))) / ((vAx_6 * vAx_6) + (vAy_6 * vAy_6))))), (x_12 > 0) ? ((x_12 < 1) ? x_12 : 1) : 0)];
                const uBs_1 = patternInput[1];
                const uBe_1 = patternInput[3];
                const uAs_1 = patternInput[0];
                const uAe_1 = patternInput[2];
                const clAsX = pAx_1 + (uAs_1 * vAx_1);
                const clAsY = pAy_1 + (uAs_1 * vAy_1);
                const clBsX = pBx_1 + (uBs_1 * vBx_1);
                const clBsY = pBy_1 + (uBs_1 * vBy_1);
                const distSqStart = ((x_13 = (clAsX - clBsX), x_13 * x_13)) + ((x_14 = (clAsY - clBsY), x_14 * x_14));
                const clAeX = pAx_1 + (uAe_1 * vAx_1);
                const clAeY = pAy_1 + (uAe_1 * vAy_1);
                const clBeX = pBx_1 + (uBe_1 * vBx_1);
                const clBeY = pBy_1 + (uBe_1 * vBy_1);
                const distSqEnd = ((x_15 = (clAeX - clBeX), x_15 * x_15)) + ((x_16 = (clAeY - clBeY), x_16 * x_16));
                tupledArg = ((distSqStart < distSqEnd) ? [Pt_$ctor_7B00E9A0(clAsX, clAsY), Pt_$ctor_7B00E9A0(clBsX, clBsY), distSqStart] : [Pt_$ctor_7B00E9A0(clAeX, clAeY), Pt_$ctor_7B00E9A0(clBeX, clBeY), distSqEnd]);
                return new XLine2DModule_ClPts(2, [tupledArg[0], tupledArg[1], tupledArg[2]]);
            }
        }
        else {
            let patternInput_1;
            const pAx_7 = pAx;
            const pAy_7 = pAy;
            const pBx_3 = pBx;
            const pBy_3 = pBy;
            const vAx_7 = vAx;
            const vAy_7 = vAy;
            const vBx_3 = vBx;
            const vBy_3 = vBy;
            let uAs_2;
            let x_19;
            const vAx_8 = vAx_7;
            const vAy_8 = vAy_7;
            x_19 = (((vAx_8 * (pBx_3 - pAx_7)) + (vAy_8 * (pBy_3 - pAy_7))) / ((vAx_8 * vAx_8) + (vAy_8 * vAy_8)));
            uAs_2 = ((x_19 > 0) ? ((x_19 < 1) ? x_19 : 1) : 0);
            let uAe_2;
            let x_23;
            const vAx_10 = vAx_7;
            const vAy_10 = vAy_7;
            x_23 = (((vAx_10 * ((pBx_3 + vBx_3) - pAx_7)) + (vAy_10 * ((pBy_3 + vBy_3) - pAy_7))) / ((vAx_10 * vAx_10) + (vAy_10 * vAy_10)));
            uAe_2 = ((x_23 > 0) ? ((x_23 < 1) ? x_23 : 1) : 0);
            patternInput_1 = [uAs_2, (x_21 = ((vAx_9 = vBx_3, (vAy_9 = vBy_3, ((vAx_9 * ((pAx_7 + (uAs_2 * vAx_7)) - pBx_3)) + (vAy_9 * ((pAy_7 + (uAs_2 * vAy_7)) - pBy_3))) / ((vAx_9 * vAx_9) + (vAy_9 * vAy_9))))), (x_21 > 0) ? ((x_21 < 1) ? x_21 : 1) : 0), uAe_2, (x_25 = ((vAx_11 = vBx_3, (vAy_11 = vBy_3, ((vAx_11 * ((pAx_7 + (uAe_2 * vAx_7)) - pBx_3)) + (vAy_11 * ((pAy_7 + (uAe_2 * vAy_7)) - pBy_3))) / ((vAx_11 * vAx_11) + (vAy_11 * vAy_11))))), (x_25 > 0) ? ((x_25 < 1) ? x_25 : 1) : 0)];
            const tA = (patternInput_1[0] + patternInput_1[2]) * 0.5;
            const tB = (patternInput_1[1] + patternInput_1[3]) * 0.5;
            return new XLine2DModule_ClPts(1, [Pt_$ctor_7B00E9A0(pAx + (tA * vAx), pAy + (tA * vAy)), Pt_$ctor_7B00E9A0(pBx + (tB * vBx), pBy + (tB * vBy))]);
        }
    }
}

/**
 * Gets the square distance between two finite lines. Works on Parallel lines too.
 */
export function XLine2D_getSqDistance_Z6A6F0C80(pAx, pAy, pBx, pBy, vAx, vAy, vBx, vBy) {
    let x_4, vAx_4, vAy_4, x_8, vAx_6, vAy_6, x_9, x_10, x_11, x_12;
    const det = (vAx * vBy) - (vAy * vBx);
    const dx = pBx - pAx;
    const dy = pBy - pAy;
    const t = ((dx * vBy) - (dy * vBx)) / det;
    const u = ((dx * vAy) - (dy * vAx)) / det;
    if ((((t >= 0) && (t <= 1)) && (u >= 0)) && (u <= 1)) {
        return 0;
    }
    else {
        const pAx_1 = pAx;
        const pAy_1 = pAy;
        const pBx_1 = pBx;
        const pBy_1 = pBy;
        const vAx_1 = vAx;
        const vAy_1 = vAy;
        const vBx_1 = vBx;
        const vBy_1 = vBy;
        let patternInput;
        const pAx_2 = pAx_1;
        const pAy_2 = pAy_1;
        const pBx_2 = pBx_1;
        const pBy_2 = pBy_1;
        const vAx_2 = vAx_1;
        const vAy_2 = vAy_1;
        const vBx_2 = vBx_1;
        const vBy_2 = vBy_1;
        let uAs;
        let x_2;
        const vAx_3 = vAx_2;
        const vAy_3 = vAy_2;
        x_2 = (((vAx_3 * (pBx_2 - pAx_2)) + (vAy_3 * (pBy_2 - pAy_2))) / ((vAx_3 * vAx_3) + (vAy_3 * vAy_3)));
        uAs = ((x_2 > 0) ? ((x_2 < 1) ? x_2 : 1) : 0);
        let uAe;
        let x_6;
        const vAx_5 = vAx_2;
        const vAy_5 = vAy_2;
        x_6 = (((vAx_5 * ((pBx_2 + vBx_2) - pAx_2)) + (vAy_5 * ((pBy_2 + vBy_2) - pAy_2))) / ((vAx_5 * vAx_5) + (vAy_5 * vAy_5)));
        uAe = ((x_6 > 0) ? ((x_6 < 1) ? x_6 : 1) : 0);
        patternInput = [uAs, (x_4 = ((vAx_4 = vBx_2, (vAy_4 = vBy_2, ((vAx_4 * ((pAx_2 + (uAs * vAx_2)) - pBx_2)) + (vAy_4 * ((pAy_2 + (uAs * vAy_2)) - pBy_2))) / ((vAx_4 * vAx_4) + (vAy_4 * vAy_4))))), (x_4 > 0) ? ((x_4 < 1) ? x_4 : 1) : 0), uAe, (x_8 = ((vAx_6 = vBx_2, (vAy_6 = vBy_2, ((vAx_6 * ((pAx_2 + (uAe * vAx_2)) - pBx_2)) + (vAy_6 * ((pAy_2 + (uAe * vAy_2)) - pBy_2))) / ((vAx_6 * vAx_6) + (vAy_6 * vAy_6))))), (x_8 > 0) ? ((x_8 < 1) ? x_8 : 1) : 0)];
        const uBs_1 = patternInput[1];
        const uBe_1 = patternInput[3];
        const uAs_1 = patternInput[0];
        const uAe_1 = patternInput[2];
        return min(((x_9 = ((pAx_1 + (uAs_1 * vAx_1)) - (pBx_1 + (uBs_1 * vBx_1))), x_9 * x_9)) + ((x_10 = ((pAy_1 + (uAs_1 * vAy_1)) - (pBy_1 + (uBs_1 * vBy_1))), x_10 * x_10)), ((x_11 = ((pAx_1 + (uAe_1 * vAx_1)) - (pBx_1 + (uBe_1 * vBx_1))), x_11 * x_11)) + ((x_12 = ((pAy_1 + (uAe_1 * vAy_1)) - (pBy_1 + (uBe_1 * vBy_1))), x_12 * x_12)));
    }
}

/**
 * Checks if the two finite 2D lines are touching each other at exactly one of their end points
 * within the given tolerance.
 */
export function XLine2D_getEndsTouching_Z44565CE5(a, b, tolerance = 1E-06) {
    let x, x_1, x_2, x_3, x_4, x_5, x_6, x_7, x_8, x_9, x_10, x_11;
    const sqTolerance = tolerance * tolerance;
    if ((((x = (a.ToX - b.FromX), x * x)) + ((x_1 = (a.ToY - b.FromY), x_1 * x_1))) < sqTolerance) {
        if ((((x_2 = (a.FromX - b.ToX), x_2 * x_2)) + ((x_3 = (a.FromY - b.ToY), x_3 * x_3))) < sqTolerance) {
            return new XLine2DModule_XEnds(6, []);
        }
        else {
            return new XLine2DModule_XEnds(3, []);
        }
    }
    else if ((((x_4 = (a.FromX - b.ToX), x_4 * x_4)) + ((x_5 = (a.FromY - b.ToY), x_5 * x_5))) < sqTolerance) {
        return new XLine2DModule_XEnds(4, []);
    }
    else if ((((x_6 = (a.FromX - b.FromX), x_6 * x_6)) + ((x_7 = (a.FromY - b.FromY), x_7 * x_7))) < sqTolerance) {
        if ((((x_8 = (a.ToX - b.ToX), x_8 * x_8)) + ((x_9 = (a.ToY - b.ToY), x_9 * x_9))) < sqTolerance) {
            return new XLine2DModule_XEnds(5, []);
        }
        else {
            return new XLine2DModule_XEnds(1, []);
        }
    }
    else if ((((x_10 = (a.ToX - b.ToX), x_10 * x_10)) + ((x_11 = (a.ToY - b.ToY), x_11 * x_11))) < sqTolerance) {
        return new XLine2DModule_XEnds(2, []);
    }
    else {
        return new XLine2DModule_XEnds(0, []);
    }
}

