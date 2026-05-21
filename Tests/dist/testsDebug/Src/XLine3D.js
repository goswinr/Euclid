
import { Union } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type, union_type, float64_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { Pnt_$ctor_Z7AD9E565, Pnt_$reflection } from "./Pnt.js";
import { min } from "../fable_modules/fable-library-js.5.0.0/Double.js";
import { fail } from "./EuclidErrors.js";
import { Vec_$ctor_Z7AD9E565 } from "./Vec.js";

/**
 * The result of a line-cone intersection test.
 * This is the return type of the function Intersection.lineCone.
 */
export class XLine3DModule_XCone extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["NoIntersection", "Tangential", "Touching", "Intersecting"];
    }
}

export function XLine3DModule_XCone_$reflection() {
    return union_type("Euclid.XLine3DModule.XCone", [], XLine3DModule_XCone, () => [[], [], [["Item", float64_type]], [["Item1", float64_type], ["Item2", float64_type]]]);
}

/**
 * Describes the possible relationships of two rays (rays are 3D lines extended infinitely in both directions).
 * Returns parameters on both lines if they intersect or are skew.
 */
export class XLine3DModule_XRayParam extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["SkewOrX", "Parallel", "TooShortA", "TooShortB", "TooShortBoth"];
    }
}

export function XLine3DModule_XRayParam_$reflection() {
    return union_type("Euclid.XLine3DModule.XRayParam", [], XLine3DModule_XRayParam, () => [[["twoParams", float64_type], ["Item2", float64_type]], [], [], [], []]);
}

/**
 * Describes the possible relationships of two rays (rays are 3D lines extended infinitely in both directions).
 * Returns the intersection or closest point if they intersect or are skew.
 */
export class XLine3DModule_XRay extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Intersect", "Skew", "Parallel", "TooShortA", "TooShortB", "TooShortBoth"];
    }
}

export function XLine3DModule_XRay_$reflection() {
    return union_type("Euclid.XLine3DModule.XRay", [], XLine3DModule_XRay, () => [[["xPt", Pnt_$reflection()]], [["closestPtA", Pnt_$reflection()], ["closestPtB", Pnt_$reflection()], ["squareDist", float64_type]], [], [], [], []]);
}

/**
 * Describes the possible relationships of two finite 3D lines.
 * Returns parameters on both lines if they intersect or are skew.
 */
export class XLine3DModule_XParam extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Intersect", "Skew", "Apart", "Parallel", "TooShortA", "TooShortB", "TooShortBoth"];
    }
}

export function XLine3DModule_XParam_$reflection() {
    return union_type("Euclid.XLine3DModule.XParam", [], XLine3DModule_XParam, () => [[["twoParams", float64_type], ["Item2", float64_type]], [["paramA", float64_type], ["paramB", float64_type], ["squareDist", float64_type]], [], [], [], [], []]);
}

/**
 * Describes the possible relationships of two finite 3D lines.
 * Returns the intersection or closest point if they intersect or are skew.
 */
export class XLine3DModule_XPnt extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Intersect", "Skew", "Apart", "Parallel", "TooShortA", "TooShortB", "TooShortBoth"];
    }
}

export function XLine3DModule_XPnt_$reflection() {
    return union_type("Euclid.XLine3DModule.XPnt", [], XLine3DModule_XPnt, () => [[["xPnt", Pnt_$reflection()]], [["closestPntA", Pnt_$reflection()], ["closestPntB", Pnt_$reflection()], ["squareDist", float64_type]], [], [], [], [], []]);
}

/**
 * Describes the possible cases of the closest parameters between finite 3D lines.
 */
export class XLine3DModule_ClParams extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Intersect", "Skew", "Parallel", "Apart", "TooShortA", "TooShortB", "TooShortBoth"];
    }
}

export function XLine3DModule_ClParams_$reflection() {
    return union_type("Euclid.XLine3DModule.ClParams", [], XLine3DModule_ClParams, () => [[["paramA", float64_type], ["paramB", float64_type]], [["paramA", float64_type], ["paramB", float64_type], ["squareDist", float64_type]], [["paramA", float64_type], ["paramB", float64_type]], [["paramA", float64_type], ["paramB", float64_type], ["squareDist", float64_type]], [], [], []]);
}

/**
 * Describes the possible cases of the closest points between finite 3D lines.
 */
export class XLine3DModule_ClPts extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Intersect", "Skew", "Parallel", "Apart", "TooShortA", "TooShortB", "TooShortBoth"];
    }
}

export function XLine3DModule_ClPts_$reflection() {
    return union_type("Euclid.XLine3DModule.ClPts", [], XLine3DModule_ClPts, () => [[["pt", Pnt_$reflection()]], [["ptA", Pnt_$reflection()], ["ptB", Pnt_$reflection()], ["squareDist", float64_type]], [["ptA", Pnt_$reflection()], ["ptB", Pnt_$reflection()]], [["ptA", Pnt_$reflection()], ["ptB", Pnt_$reflection()], ["squareDist", float64_type]], [], [], []]);
}

/**
 * Describes the possible cases of two finite 3D lines touching at their ends.
 * Does not check if they are parallel or intersecting.
 */
export class XLine3DModule_XEnds extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["NotTouching", "StartA_StartB", "EndA_EndB", "EndA_StartB", "StartA_EndB", "Identical", "IdenticalFlipped"];
    }
}

export function XLine3DModule_XEnds_$reflection() {
    return union_type("Euclid.XLine3DModule.XEnds", [], XLine3DModule_XEnds, () => [[], [], [], [], [], [], []]);
}

/**
 * A type containing only static member functions for computing 3D line intersections and closest approaches.
 * Some functions return Discriminated Unions from the XLine3D module.
 */
export class XLine3D {
    constructor() {
    }
}

export function XLine3D_$reflection() {
    return class_type("Euclid.XLine3D", undefined, XLine3D);
}

/**
 * Checks if 3D lines are not only parallel and coincident but are also overlapping or at least touching at their ends.
 */
export function XLine3D_doOverlap_6B19E37B(pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz, tolerance = 1E-06) {
    let x_2, x_3;
    let t;
    const vAx_1 = vAx;
    const vAy_1 = vAy;
    const vAz_1 = vAz;
    const u = pBx - pAx;
    const v = pBy - pAy;
    const w = pBz - pAz;
    const dotV = ((vAx_1 * u) + (vAy_1 * v)) + (vAz_1 * w);
    const lenSq = ((vAx_1 * vAx_1) + (vAy_1 * vAy_1)) + (vAz_1 * vAz_1);
    t = (dotV / lenSq);
    const clPtX = pAx + (vAx * t);
    const clPtY = pAy + (vAy * t);
    const clPtZ = pAz + (vAz * t);
    const vx = clPtX - pBx;
    const vy = clPtY - pBy;
    const vz = clPtZ - pBz;
    const dist = ((vx * vx) + (vy * vy)) + (vz * vz);
    const sqTolerance = tolerance * tolerance;
    if (dist > sqTolerance) {
        return false;
    }
    else {
        const bex = pBx + vBx;
        const bey = pBy + vBy;
        const bez = pBz + vBz;
        let u_2;
        const vAx_2 = vAx;
        const vAy_2 = vAy;
        const vAz_2 = vAz;
        const u_1 = bex - pAx;
        const v_1 = bey - pAy;
        const w_1 = bez - pAz;
        const dotV_1 = ((vAx_2 * u_1) + (vAy_2 * v_1)) + (vAz_2 * w_1);
        const lenSq_1 = ((vAx_2 * vAx_2) + (vAy_2 * vAy_2)) + (vAz_2 * vAz_2);
        u_2 = (dotV_1 / lenSq_1);
        const cluPtx = pAx + (vAx * u_2);
        const cluPty = pAy + (vAy * u_2);
        const cluPtz = pAz + (vAz * u_2);
        const vx2 = cluPtx - bex;
        const vy2 = cluPty - bey;
        const vz2 = cluPtz - bez;
        const dist2 = ((vx2 * vx2) + (vy2 * vy2)) + (vz2 * vz2);
        if (dist2 > sqTolerance) {
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
 * Tries to get an actual intersection point of two rays (3D lines treated as infinite).
 */
export function XLine3D_tryIntersectRay_ZA268E31(pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz, maxSkewDistance = 1E-06, tangent = 0.004363350820701567, tooShortTolerance = 1E-06) {
    if (((Math.abs(vAx) + Math.abs(vAy)) + Math.abs(vAz)) < tooShortTolerance) {
        return undefined;
    }
    else if (((Math.abs(vBx) + Math.abs(vBy)) + Math.abs(vBz)) < tooShortTolerance) {
        return undefined;
    }
    else {
        const crossX = (vAy * vBz) - (vAz * vBy);
        const crossY = (vAz * vBx) - (vAx * vBz);
        const crossZ = (vAx * vBy) - (vAy * vBx);
        const crossMag = Math.sqrt(((crossX * crossX) + (crossY * crossY)) + (crossZ * crossZ));
        const dot = ((vAx * vBx) + (vAy * vBy)) + (vAz * vBz);
        const tan = crossMag / dot;
        if (Math.abs(tan) > tangent) {
            const crossMagSq = crossMag * crossMag;
            const dx = pBx - pAx;
            const dy = pBy - pAy;
            const dz = pBz - pAz;
            const numerX_t = (dy * vBz) - (dz * vBy);
            const numerY_t = (dz * vBx) - (dx * vBz);
            const numerZ_t = (dx * vBy) - (dy * vBx);
            const numerator_t = ((numerX_t * crossX) + (numerY_t * crossY)) + (numerZ_t * crossZ);
            const t = numerator_t / crossMagSq;
            const ptAx = pAx + (t * vAx);
            const ptAy = pAy + (t * vAy);
            const ptAz = pAz + (t * vAz);
            let d;
            const pAx_1 = pBx;
            const pAy_1 = pBy;
            const pAz_1 = pBz;
            const vAx_1 = vBx;
            const vAy_1 = vBy;
            const vAz_1 = vBz;
            const x_3 = ptAx;
            const y_2 = ptAy;
            const z_2 = ptAz;
            let t_1;
            const vAx_2 = vAx_1;
            const vAy_2 = vAy_1;
            const vAz_2 = vAz_1;
            const u = x_3 - pAx_1;
            const v = y_2 - pAy_1;
            const w = z_2 - pAz_1;
            const dotV = ((vAx_2 * u) + (vAy_2 * v)) + (vAz_2 * w);
            const lenSq = ((vAx_2 * vAx_2) + (vAy_2 * vAy_2)) + (vAz_2 * vAz_2);
            t_1 = (dotV / lenSq);
            const clPtX = pAx_1 + (vAx_1 * t_1);
            const clPtY = pAy_1 + (vAy_1 * t_1);
            const clPtZ = pAz_1 + (vAz_1 * t_1);
            const vx = clPtX - x_3;
            const vy = clPtY - y_2;
            const vz = clPtZ - z_2;
            d = (((vx * vx) + (vy * vy)) + (vz * vz));
            if (d < (maxSkewDistance * maxSkewDistance)) {
                return Pnt_$ctor_Z7AD9E565(ptAx, ptAy, ptAz);
            }
            else {
                return undefined;
            }
        }
        else {
            return undefined;
        }
    }
}

/**
 * Tries to get an actual intersection point of two finite 3D lines.
 */
export function XLine3D_tryIntersect_ZA268E31(pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz, maxSkewDistance = 1E-06, tangent = 0.004363350820701567, tooShortTolerance = 1E-06) {
    if (((Math.abs(vAx) + Math.abs(vAy)) + Math.abs(vAz)) < tooShortTolerance) {
        return undefined;
    }
    else if (((Math.abs(vBx) + Math.abs(vBy)) + Math.abs(vBz)) < tooShortTolerance) {
        return undefined;
    }
    else {
        const crossX = (vAy * vBz) - (vAz * vBy);
        const crossY = (vAz * vBx) - (vAx * vBz);
        const crossZ = (vAx * vBy) - (vAy * vBx);
        const crossMag = Math.sqrt(((crossX * crossX) + (crossY * crossY)) + (crossZ * crossZ));
        const dot = ((vAx * vBx) + (vAy * vBy)) + (vAz * vBz);
        const tan = crossMag / dot;
        if (Math.abs(tan) > tangent) {
            const crossMagSq = crossMag * crossMag;
            const dx = pBx - pAx;
            const dy = pBy - pAy;
            const dz = pBz - pAz;
            const numerX_t = (dy * vBz) - (dz * vBy);
            const numerY_t = (dz * vBx) - (dx * vBz);
            const numerZ_t = (dx * vBy) - (dy * vBx);
            const numerator_t = ((numerX_t * crossX) + (numerY_t * crossY)) + (numerZ_t * crossZ);
            const t = numerator_t / crossMagSq;
            if ((t > -1E-06) && (t < (1 + 1E-06))) {
                const numerX_u = (dy * vAz) - (dz * vAy);
                const numerY_u = (dz * vAx) - (dx * vAz);
                const numerZ_u = (dx * vAy) - (dy * vAx);
                const numerator_u = ((numerX_u * crossX) + (numerY_u * crossY)) + (numerZ_u * crossZ);
                const u = numerator_u / crossMagSq;
                if ((u > -1E-06) && (u < (1 + 1E-06))) {
                    const ptAx = pAx + (t * vAx);
                    const ptAy = pAy + (t * vAy);
                    const ptAz = pAz + (t * vAz);
                    const ptBx = pBx + (u * vBx);
                    const ptBy = pBy + (u * vBy);
                    const ptBz = pBz + (u * vBz);
                    const dx2 = ptAx - ptBx;
                    const dy2 = ptAy - ptBy;
                    const dz2 = ptAz - ptBz;
                    const sqDist = ((dx2 * dx2) + (dy2 * dy2)) + (dz2 * dz2);
                    if (sqDist < (maxSkewDistance * maxSkewDistance)) {
                        return Pnt_$ctor_Z7AD9E565(ptAx, ptAy, ptAz);
                    }
                    else {
                        return undefined;
                    }
                }
                else {
                    return undefined;
                }
            }
            else {
                return undefined;
            }
        }
        else {
            return undefined;
        }
    }
}

/**
 * Tries to get closest approach parameters of two rays (rays are 3D lines extended infinitely).
 */
export function XLine3D_getRayClosestParam_Z1F58440C(pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz, tangent = 0.004363350820701567, tooShortTolerance = 1E-06) {
    if (((Math.abs(vAx) + Math.abs(vAy)) + Math.abs(vAz)) < tooShortTolerance) {
        if (((Math.abs(vBx) + Math.abs(vBy)) + Math.abs(vBz)) < tooShortTolerance) {
            return new XLine3DModule_XRayParam(4, []);
        }
        else {
            return new XLine3DModule_XRayParam(2, []);
        }
    }
    else if (((Math.abs(vBx) + Math.abs(vBy)) + Math.abs(vBz)) < tooShortTolerance) {
        return new XLine3DModule_XRayParam(3, []);
    }
    else {
        const crossX = (vAy * vBz) - (vAz * vBy);
        const crossY = (vAz * vBx) - (vAx * vBz);
        const crossZ = (vAx * vBy) - (vAy * vBx);
        const crossMag = Math.sqrt(((crossX * crossX) + (crossY * crossY)) + (crossZ * crossZ));
        const dot = ((vAx * vBx) + (vAy * vBy)) + (vAz * vBz);
        const tan = crossMag / dot;
        if (Math.abs(tan) > tangent) {
            const crossMagSq = crossMag * crossMag;
            const dx = pBx - pAx;
            const dy = pBy - pAy;
            const dz = pBz - pAz;
            const numerX_t = (dy * vBz) - (dz * vBy);
            const numerY_t = (dz * vBx) - (dx * vBz);
            const numerZ_t = (dx * vBy) - (dy * vBx);
            const numerator_t = ((numerX_t * crossX) + (numerY_t * crossY)) + (numerZ_t * crossZ);
            const t = numerator_t / crossMagSq;
            const numerX_u = (dy * vAz) - (dz * vAy);
            const numerY_u = (dz * vAx) - (dx * vAz);
            const numerZ_u = (dx * vAy) - (dy * vAx);
            const numerator_u = ((numerX_u * crossX) + (numerY_u * crossY)) + (numerZ_u * crossZ);
            const u = numerator_u / crossMagSq;
            const tupledArg = [t, u];
            return new XLine3DModule_XRayParam(0, [tupledArg[0], tupledArg[1]]);
        }
        else {
            return new XLine3DModule_XRayParam(1, []);
        }
    }
}

/**
 * Tries to get closest approach point of two rays (rays are 3D lines extended infinitely).
 */
export function XLine3D_getRayIntersection_ZA268E31(pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz, maxSkewDistance = 1E-06, tangent = 0.004363350820701567, tooShortTolerance = 1E-06) {
    if (((Math.abs(vAx) + Math.abs(vAy)) + Math.abs(vAz)) < tooShortTolerance) {
        if (((Math.abs(vBx) + Math.abs(vBy)) + Math.abs(vBz)) < tooShortTolerance) {
            return new XLine3DModule_XRay(5, []);
        }
        else {
            return new XLine3DModule_XRay(3, []);
        }
    }
    else if (((Math.abs(vBx) + Math.abs(vBy)) + Math.abs(vBz)) < tooShortTolerance) {
        return new XLine3DModule_XRay(4, []);
    }
    else {
        const crossX = (vAy * vBz) - (vAz * vBy);
        const crossY = (vAz * vBx) - (vAx * vBz);
        const crossZ = (vAx * vBy) - (vAy * vBx);
        const crossMag = Math.sqrt(((crossX * crossX) + (crossY * crossY)) + (crossZ * crossZ));
        const dot = ((vAx * vBx) + (vAy * vBy)) + (vAz * vBz);
        const tan = crossMag / dot;
        if (Math.abs(tan) > tangent) {
            const crossMagSq = crossMag * crossMag;
            const dx = pBx - pAx;
            const dy = pBy - pAy;
            const dz = pBz - pAz;
            const numerX_t = (dy * vBz) - (dz * vBy);
            const numerY_t = (dz * vBx) - (dx * vBz);
            const numerZ_t = (dx * vBy) - (dy * vBx);
            const numerator_t = ((numerX_t * crossX) + (numerY_t * crossY)) + (numerZ_t * crossZ);
            const t = numerator_t / crossMagSq;
            const numerX_u = (dy * vAz) - (dz * vAy);
            const numerY_u = (dz * vAx) - (dx * vAz);
            const numerZ_u = (dx * vAy) - (dy * vAx);
            const numerator_u = ((numerX_u * crossX) + (numerY_u * crossY)) + (numerZ_u * crossZ);
            const u = numerator_u / crossMagSq;
            const ptAx = pAx + (t * vAx);
            const ptAy = pAy + (t * vAy);
            const ptAz = pAz + (t * vAz);
            const ptBx = pBx + (u * vBx);
            const ptBy = pBy + (u * vBy);
            const ptBz = pBz + (u * vBz);
            const dx2 = ptAx - ptBx;
            const dy2 = ptAy - ptBy;
            const dz2 = ptAz - ptBz;
            const sqDist = ((dx2 * dx2) + (dy2 * dy2)) + (dz2 * dz2);
            if (sqDist < (maxSkewDistance * maxSkewDistance)) {
                const ptA = Pnt_$ctor_Z7AD9E565(ptAx, ptAy, ptAz);
                return new XLine3DModule_XRay(0, [ptA]);
            }
            else {
                const ptA_1 = Pnt_$ctor_Z7AD9E565(ptAx, ptAy, ptAz);
                const ptB = Pnt_$ctor_Z7AD9E565(ptBx, ptBy, ptBz);
                return new XLine3DModule_XRay(1, [ptA_1, ptB, sqDist]);
            }
        }
        else {
            return new XLine3DModule_XRay(2, []);
        }
    }
}

/**
 * Tries to get intersection or closest approach parameters of two finite 3D-lines.
 */
export function XLine3D_getIntersectionParam_ZA268E31(pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz, maxSkewDistance = 1E-06, tangent = 0.004363350820701567, tooShortTolerance = 1E-06) {
    if (((Math.abs(vAx) + Math.abs(vAy)) + Math.abs(vAz)) < tooShortTolerance) {
        if (((Math.abs(vBx) + Math.abs(vBy)) + Math.abs(vBz)) < tooShortTolerance) {
            return new XLine3DModule_XParam(6, []);
        }
        else {
            return new XLine3DModule_XParam(4, []);
        }
    }
    else if (((Math.abs(vBx) + Math.abs(vBy)) + Math.abs(vBz)) < tooShortTolerance) {
        return new XLine3DModule_XParam(5, []);
    }
    else {
        const crossX = (vAy * vBz) - (vAz * vBy);
        const crossY = (vAz * vBx) - (vAx * vBz);
        const crossZ = (vAx * vBy) - (vAy * vBx);
        const crossMag = Math.sqrt(((crossX * crossX) + (crossY * crossY)) + (crossZ * crossZ));
        const dot = ((vAx * vBx) + (vAy * vBy)) + (vAz * vBz);
        const tan = crossMag / dot;
        if (Math.abs(tan) > tangent) {
            const crossMagSq = crossMag * crossMag;
            const dx = pBx - pAx;
            const dy = pBy - pAy;
            const dz = pBz - pAz;
            const numerX_t = (dy * vBz) - (dz * vBy);
            const numerY_t = (dz * vBx) - (dx * vBz);
            const numerZ_t = (dx * vBy) - (dy * vBx);
            const numerator_t = ((numerX_t * crossX) + (numerY_t * crossY)) + (numerZ_t * crossZ);
            const t = numerator_t / crossMagSq;
            const numerX_u = (dy * vAz) - (dz * vAy);
            const numerY_u = (dz * vAx) - (dx * vAz);
            const numerZ_u = (dx * vAy) - (dy * vAx);
            const numerator_u = ((numerX_u * crossX) + (numerY_u * crossY)) + (numerZ_u * crossZ);
            const u = numerator_u / crossMagSq;
            if ((((t > -1E-06) && (t < 1.000001)) && (u > -1E-06)) && (u < 1.000001)) {
                const ptAx = pAx + (t * vAx);
                const ptAy = pAy + (t * vAy);
                const ptAz = pAz + (t * vAz);
                const ptBx = pBx + (u * vBx);
                const ptBy = pBy + (u * vBy);
                const ptBz = pBz + (u * vBz);
                const dx2 = ptAx - ptBx;
                const dy2 = ptAy - ptBy;
                const dz2 = ptAz - ptBz;
                const sqDist = ((dx2 * dx2) + (dy2 * dy2)) + (dz2 * dz2);
                if (sqDist < (maxSkewDistance * maxSkewDistance)) {
                    return new XLine3DModule_XParam(0, [t, u]);
                }
                else {
                    return new XLine3DModule_XParam(1, [t, u, sqDist]);
                }
            }
            else {
                return new XLine3DModule_XParam(2, []);
            }
        }
        else {
            return new XLine3DModule_XParam(3, []);
        }
    }
}

/**
 * Tries to get intersection or closest approach point of two finite 3D lines.
 */
export function XLine3D_getIntersection_ZA268E31(pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz, maxSkewDistance = 1E-06, tangent = 0.004363350820701567, tooShortTolerance = 1E-06) {
    if (((Math.abs(vAx) + Math.abs(vAy)) + Math.abs(vAz)) < tooShortTolerance) {
        if (((Math.abs(vBx) + Math.abs(vBy)) + Math.abs(vBz)) < tooShortTolerance) {
            return new XLine3DModule_XPnt(6, []);
        }
        else {
            return new XLine3DModule_XPnt(4, []);
        }
    }
    else if (((Math.abs(vBx) + Math.abs(vBy)) + Math.abs(vBz)) < tooShortTolerance) {
        return new XLine3DModule_XPnt(5, []);
    }
    else {
        const crossX = (vAy * vBz) - (vAz * vBy);
        const crossY = (vAz * vBx) - (vAx * vBz);
        const crossZ = (vAx * vBy) - (vAy * vBx);
        const crossMag = Math.sqrt(((crossX * crossX) + (crossY * crossY)) + (crossZ * crossZ));
        const dot = ((vAx * vBx) + (vAy * vBy)) + (vAz * vBz);
        const tan = crossMag / dot;
        if (Math.abs(tan) > tangent) {
            const crossMagSq = crossMag * crossMag;
            const dx = pBx - pAx;
            const dy = pBy - pAy;
            const dz = pBz - pAz;
            const numerX_t = (dy * vBz) - (dz * vBy);
            const numerY_t = (dz * vBx) - (dx * vBz);
            const numerZ_t = (dx * vBy) - (dy * vBx);
            const numerator_t = ((numerX_t * crossX) + (numerY_t * crossY)) + (numerZ_t * crossZ);
            const t = numerator_t / crossMagSq;
            const numerX_u = (dy * vAz) - (dz * vAy);
            const numerY_u = (dz * vAx) - (dx * vAz);
            const numerZ_u = (dx * vAy) - (dy * vAx);
            const numerator_u = ((numerX_u * crossX) + (numerY_u * crossY)) + (numerZ_u * crossZ);
            const u = numerator_u / crossMagSq;
            if ((((t > -1E-06) && (t < 1.000001)) && (u > -1E-06)) && (u < 1.000001)) {
                const ptAx = pAx + (t * vAx);
                const ptAy = pAy + (t * vAy);
                const ptAz = pAz + (t * vAz);
                const ptBx = pBx + (u * vBx);
                const ptBy = pBy + (u * vBy);
                const ptBz = pBz + (u * vBz);
                const dx2 = ptAx - ptBx;
                const dy2 = ptAy - ptBy;
                const dz2 = ptAz - ptBz;
                const sqDist = ((dx2 * dx2) + (dy2 * dy2)) + (dz2 * dz2);
                if (sqDist < (maxSkewDistance * maxSkewDistance)) {
                    return new XLine3DModule_XPnt(0, [Pnt_$ctor_Z7AD9E565(ptAx, ptAy, ptAz)]);
                }
                else {
                    return new XLine3DModule_XPnt(1, [Pnt_$ctor_Z7AD9E565(ptAx, ptAy, ptAz), Pnt_$ctor_Z7AD9E565(ptBx, ptBy, ptBz), sqDist]);
                }
            }
            else {
                return new XLine3DModule_XPnt(2, []);
            }
        }
        else {
            return new XLine3DModule_XPnt(3, []);
        }
    }
}

/**
 * Gets the parameters on the finite lines where the lines are closest to each other.
 * For parallel lines, this will return the parameter at the middle point of any overlapping segment.
 * For skew lines, this returns the closest approach parameters.
 */
export function XLine3D_getClosestParameters_Z1F58440C(pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz, tangent = 0.004363350820701567, tooShortTolerance = 1E-06) {
    let x_13, x_14, x_15, x_16, x_17, x_18;
    if (((Math.abs(vAx) + Math.abs(vAy)) + Math.abs(vAz)) < tooShortTolerance) {
        if (((Math.abs(vBx) + Math.abs(vBy)) + Math.abs(vBz)) < tooShortTolerance) {
            return new XLine3DModule_ClParams(6, []);
        }
        else {
            return new XLine3DModule_ClParams(4, []);
        }
    }
    else if (((Math.abs(vBx) + Math.abs(vBy)) + Math.abs(vBz)) < tooShortTolerance) {
        return new XLine3DModule_ClParams(5, []);
    }
    else {
        const crossX = (vAy * vBz) - (vAz * vBy);
        const crossY = (vAz * vBx) - (vAx * vBz);
        const crossZ = (vAx * vBy) - (vAy * vBx);
        const crossMag = Math.sqrt(((crossX * crossX) + (crossY * crossY)) + (crossZ * crossZ));
        const dot = ((vAx * vBx) + (vAy * vBy)) + (vAz * vBz);
        const tan = crossMag / dot;
        if (Math.abs(tan) > tangent) {
            const crossMagSq = crossMag * crossMag;
            const dx = pBx - pAx;
            const dy = pBy - pAy;
            const dz = pBz - pAz;
            const numerX_t = (dy * vBz) - (dz * vBy);
            const numerY_t = (dz * vBx) - (dx * vBz);
            const numerZ_t = (dx * vBy) - (dy * vBx);
            const numerator_t = ((numerX_t * crossX) + (numerY_t * crossY)) + (numerZ_t * crossZ);
            const t = numerator_t / crossMagSq;
            const numerX_u = (dy * vAz) - (dz * vAy);
            const numerY_u = (dz * vAx) - (dx * vAz);
            const numerZ_u = (dx * vAy) - (dy * vAx);
            const numerator_u = ((numerX_u * crossX) + (numerY_u * crossY)) + (numerZ_u * crossZ);
            const u = numerator_u / crossMagSq;
            if ((((t >= -1E-06) && (t <= 1.000001)) && (u >= -1E-06)) && (u <= 1.000001)) {
                const ptAx = pAx + (t * vAx);
                const ptAy = pAy + (t * vAy);
                const ptAz = pAz + (t * vAz);
                const ptBx = pBx + (u * vBx);
                const ptBy = pBy + (u * vBy);
                const ptBz = pBz + (u * vBz);
                const dx2 = ptAx - ptBx;
                const dy2 = ptAy - ptBy;
                const dz2 = ptAz - ptBz;
                const sqDist = ((dx2 * dx2) + (dy2 * dy2)) + (dz2 * dz2);
                if (sqDist < 1E-12) {
                    return new XLine3DModule_ClParams(0, [t, u]);
                }
                else {
                    return new XLine3DModule_ClParams(1, [t, u, sqDist]);
                }
            }
            else {
                let tupledArg;
                const pAx_1 = pAx;
                const pAy_1 = pAy;
                const pAz_1 = pAz;
                const pBx_1 = pBx;
                const pBy_1 = pBy;
                const pBz_1 = pBz;
                const vAx_1 = vAx;
                const vAy_1 = vAy;
                const vAz_1 = vAz;
                const vBx_1 = vBx;
                const vBy_1 = vBy;
                const vBz_1 = vBz;
                let patternInput;
                const pAx_2 = pAx_1;
                const pAy_2 = pAy_1;
                const pAz_2 = pAz_1;
                const pBx_2 = pBx_1;
                const pBy_2 = pBy_1;
                const pBz_2 = pBz_1;
                const vAx_2 = vAx_1;
                const vAy_2 = vAy_1;
                const vAz_2 = vAz_1;
                const vBx_2 = vBx_1;
                const vBy_2 = vBy_1;
                const vBz_2 = vBz_1;
                let uAs;
                let x_6;
                const vAx_3 = vAx_2;
                const vAy_3 = vAy_2;
                const vAz_3 = vAz_2;
                const u_1 = pBx_2 - pAx_2;
                const v = pBy_2 - pAy_2;
                const w = pBz_2 - pAz_2;
                const dotV = ((vAx_3 * u_1) + (vAy_3 * v)) + (vAz_3 * w);
                const lenSq = ((vAx_3 * vAx_3) + (vAy_3 * vAy_3)) + (vAz_3 * vAz_3);
                x_6 = (dotV / lenSq);
                uAs = ((x_6 > 0) ? ((x_6 < 1) ? x_6 : 1) : 0);
                const x_1_1 = pAx_2 + (uAs * vAx_2);
                const y_4 = pAy_2 + (uAs * vAy_2);
                const z_4 = pAz_2 + (uAs * vAz_2);
                let uBs;
                let x_8;
                const vAx_4 = vBx_2;
                const vAy_4 = vBy_2;
                const vAz_4 = vBz_2;
                const u_2 = x_1_1 - pBx_2;
                const v_1 = y_4 - pBy_2;
                const w_1 = z_4 - pBz_2;
                const dotV_1 = ((vAx_4 * u_2) + (vAy_4 * v_1)) + (vAz_4 * w_1);
                const lenSq_1 = ((vAx_4 * vAx_4) + (vAy_4 * vAy_4)) + (vAz_4 * vAz_4);
                x_8 = (dotV_1 / lenSq_1);
                uBs = ((x_8 > 0) ? ((x_8 < 1) ? x_8 : 1) : 0);
                const eBx = pBx_2 + vBx_2;
                const eBy = pBy_2 + vBy_2;
                const eBz = pBz_2 + vBz_2;
                let uAe;
                let x_10;
                const vAx_5 = vAx_2;
                const vAy_5 = vAy_2;
                const vAz_5 = vAz_2;
                const u_3 = eBx - pAx_2;
                const v_2 = eBy - pAy_2;
                const w_2 = eBz - pAz_2;
                const dotV_2 = ((vAx_5 * u_3) + (vAy_5 * v_2)) + (vAz_5 * w_2);
                const lenSq_2 = ((vAx_5 * vAx_5) + (vAy_5 * vAy_5)) + (vAz_5 * vAz_5);
                x_10 = (dotV_2 / lenSq_2);
                uAe = ((x_10 > 0) ? ((x_10 < 1) ? x_10 : 1) : 0);
                const xE = pAx_2 + (uAe * vAx_2);
                const yE = pAy_2 + (uAe * vAy_2);
                const zE = pAz_2 + (uAe * vAz_2);
                let uBe;
                let x_12;
                const vAx_6 = vBx_2;
                const vAy_6 = vBy_2;
                const vAz_6 = vBz_2;
                const u_4 = xE - pBx_2;
                const v_3 = yE - pBy_2;
                const w_3 = zE - pBz_2;
                const dotV_3 = ((vAx_6 * u_4) + (vAy_6 * v_3)) + (vAz_6 * w_3);
                const lenSq_3 = ((vAx_6 * vAx_6) + (vAy_6 * vAy_6)) + (vAz_6 * vAz_6);
                x_12 = (dotV_3 / lenSq_3);
                uBe = ((x_12 > 0) ? ((x_12 < 1) ? x_12 : 1) : 0);
                patternInput = [uAs, uBs, uAe, uBe];
                const uBs_1 = patternInput[1];
                const uBe_1 = patternInput[3];
                const uAs_1 = patternInput[0];
                const uAe_1 = patternInput[2];
                const clAsX = pAx_1 + (uAs_1 * vAx_1);
                const clAsY = pAy_1 + (uAs_1 * vAy_1);
                const clAsZ = pAz_1 + (uAs_1 * vAz_1);
                const clBsX = pBx_1 + (uBs_1 * vBx_1);
                const clBsY = pBy_1 + (uBs_1 * vBy_1);
                const clBsZ = pBz_1 + (uBs_1 * vBz_1);
                const distSqStart = (((x_13 = (clAsX - clBsX), x_13 * x_13)) + ((x_14 = (clAsY - clBsY), x_14 * x_14))) + ((x_15 = (clAsZ - clBsZ), x_15 * x_15));
                const clAeX = pAx_1 + (uAe_1 * vAx_1);
                const clAeY = pAy_1 + (uAe_1 * vAy_1);
                const clAeZ = pAz_1 + (uAe_1 * vAz_1);
                const clBeX = pBx_1 + (uBe_1 * vBx_1);
                const clBeY = pBy_1 + (uBe_1 * vBy_1);
                const clBeZ = pBz_1 + (uBe_1 * vBz_1);
                const distSqEnd = (((x_16 = (clAeX - clBeX), x_16 * x_16)) + ((x_17 = (clAeY - clBeY), x_17 * x_17))) + ((x_18 = (clAeZ - clBeZ), x_18 * x_18));
                tupledArg = ((distSqStart < distSqEnd) ? [uAs_1, uBs_1, distSqStart] : [uAe_1, uBe_1, distSqEnd]);
                return new XLine3DModule_ClParams(3, [tupledArg[0], tupledArg[1], tupledArg[2]]);
            }
        }
        else {
            let patternInput_1;
            const pAx_7 = pAx;
            const pAy_7 = pAy;
            const pAz_7 = pAz;
            const pBx_3 = pBx;
            const pBy_3 = pBy;
            const pBz_3 = pBz;
            const vAx_7 = vAx;
            const vAy_7 = vAy;
            const vAz_7 = vAz;
            const vBx_3 = vBx;
            const vBy_3 = vBy;
            const vBz_3 = vBz;
            let uAs_2;
            let x_21;
            const vAx_8 = vAx_7;
            const vAy_8 = vAy_7;
            const vAz_8 = vAz_7;
            const u_5 = pBx_3 - pAx_7;
            const v_4 = pBy_3 - pAy_7;
            const w_4 = pBz_3 - pAz_7;
            const dotV_4 = ((vAx_8 * u_5) + (vAy_8 * v_4)) + (vAz_8 * w_4);
            const lenSq_4 = ((vAx_8 * vAx_8) + (vAy_8 * vAy_8)) + (vAz_8 * vAz_8);
            x_21 = (dotV_4 / lenSq_4);
            uAs_2 = ((x_21 > 0) ? ((x_21 < 1) ? x_21 : 1) : 0);
            const x_1_2 = pAx_7 + (uAs_2 * vAx_7);
            const y_9 = pAy_7 + (uAs_2 * vAy_7);
            const z_9 = pAz_7 + (uAs_2 * vAz_7);
            let uBs_2;
            let x_23;
            const vAx_9 = vBx_3;
            const vAy_9 = vBy_3;
            const vAz_9 = vBz_3;
            const u_6 = x_1_2 - pBx_3;
            const v_5 = y_9 - pBy_3;
            const w_5 = z_9 - pBz_3;
            const dotV_5 = ((vAx_9 * u_6) + (vAy_9 * v_5)) + (vAz_9 * w_5);
            const lenSq_5 = ((vAx_9 * vAx_9) + (vAy_9 * vAy_9)) + (vAz_9 * vAz_9);
            x_23 = (dotV_5 / lenSq_5);
            uBs_2 = ((x_23 > 0) ? ((x_23 < 1) ? x_23 : 1) : 0);
            const eBx_1 = pBx_3 + vBx_3;
            const eBy_1 = pBy_3 + vBy_3;
            const eBz_1 = pBz_3 + vBz_3;
            let uAe_2;
            let x_25;
            const vAx_10 = vAx_7;
            const vAy_10 = vAy_7;
            const vAz_10 = vAz_7;
            const u_7 = eBx_1 - pAx_7;
            const v_6 = eBy_1 - pAy_7;
            const w_6 = eBz_1 - pAz_7;
            const dotV_6 = ((vAx_10 * u_7) + (vAy_10 * v_6)) + (vAz_10 * w_6);
            const lenSq_6 = ((vAx_10 * vAx_10) + (vAy_10 * vAy_10)) + (vAz_10 * vAz_10);
            x_25 = (dotV_6 / lenSq_6);
            uAe_2 = ((x_25 > 0) ? ((x_25 < 1) ? x_25 : 1) : 0);
            const xE_1 = pAx_7 + (uAe_2 * vAx_7);
            const yE_1 = pAy_7 + (uAe_2 * vAy_7);
            const zE_1 = pAz_7 + (uAe_2 * vAz_7);
            let uBe_2;
            let x_27;
            const vAx_11 = vBx_3;
            const vAy_11 = vBy_3;
            const vAz_11 = vBz_3;
            const u_8 = xE_1 - pBx_3;
            const v_7 = yE_1 - pBy_3;
            const w_7 = zE_1 - pBz_3;
            const dotV_7 = ((vAx_11 * u_8) + (vAy_11 * v_7)) + (vAz_11 * w_7);
            const lenSq_7 = ((vAx_11 * vAx_11) + (vAy_11 * vAy_11)) + (vAz_11 * vAz_11);
            x_27 = (dotV_7 / lenSq_7);
            uBe_2 = ((x_27 > 0) ? ((x_27 < 1) ? x_27 : 1) : 0);
            patternInput_1 = [uAs_2, uBs_2, uAe_2, uBe_2];
            const uBs_3 = patternInput_1[1];
            const uBe_3 = patternInput_1[3];
            const uAs_3 = patternInput_1[0];
            const uAe_3 = patternInput_1[2];
            return new XLine3DModule_ClParams(2, [(uAs_3 + uAe_3) * 0.5, (uBs_3 + uBe_3) * 0.5]);
        }
    }
}

/**
 * Gets the point or points on the finite lines where the lines are closest to each other.
 * For parallel lines, this will return the middle point of any overlapping segment.
 * For skew lines, this returns the two closest approach points.
 */
export function XLine3D_getClosestPoints_Z1F58440C(pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz, tangent = 0.004363350820701567, tooShortTolerance = 1E-06) {
    let x_13, x_14, x_15, x_16, x_17, x_18;
    if (((Math.abs(vAx) + Math.abs(vAy)) + Math.abs(vAz)) < tooShortTolerance) {
        if (((Math.abs(vBx) + Math.abs(vBy)) + Math.abs(vBz)) < tooShortTolerance) {
            return new XLine3DModule_ClPts(6, []);
        }
        else {
            return new XLine3DModule_ClPts(4, []);
        }
    }
    else if (((Math.abs(vBx) + Math.abs(vBy)) + Math.abs(vBz)) < tooShortTolerance) {
        return new XLine3DModule_ClPts(5, []);
    }
    else {
        const crossX = (vAy * vBz) - (vAz * vBy);
        const crossY = (vAz * vBx) - (vAx * vBz);
        const crossZ = (vAx * vBy) - (vAy * vBx);
        const crossMag = Math.sqrt(((crossX * crossX) + (crossY * crossY)) + (crossZ * crossZ));
        const dot = ((vAx * vBx) + (vAy * vBy)) + (vAz * vBz);
        const tan = crossMag / dot;
        if (Math.abs(tan) > tangent) {
            const crossMagSq = crossMag * crossMag;
            const dx = pBx - pAx;
            const dy = pBy - pAy;
            const dz = pBz - pAz;
            const numerX_t = (dy * vBz) - (dz * vBy);
            const numerY_t = (dz * vBx) - (dx * vBz);
            const numerZ_t = (dx * vBy) - (dy * vBx);
            const numerator_t = ((numerX_t * crossX) + (numerY_t * crossY)) + (numerZ_t * crossZ);
            const t = numerator_t / crossMagSq;
            const numerX_u = (dy * vAz) - (dz * vAy);
            const numerY_u = (dz * vAx) - (dx * vAz);
            const numerZ_u = (dx * vAy) - (dy * vAx);
            const numerator_u = ((numerX_u * crossX) + (numerY_u * crossY)) + (numerZ_u * crossZ);
            const u = numerator_u / crossMagSq;
            if ((((t >= 0) && (t <= 1)) && (u >= 0)) && (u <= 1)) {
                const ptAx = pAx + (t * vAx);
                const ptAy = pAy + (t * vAy);
                const ptAz = pAz + (t * vAz);
                const ptBx = pBx + (u * vBx);
                const ptBy = pBy + (u * vBy);
                const ptBz = pBz + (u * vBz);
                const dx2 = ptAx - ptBx;
                const dy2 = ptAy - ptBy;
                const dz2 = ptAz - ptBz;
                const sqDist = ((dx2 * dx2) + (dy2 * dy2)) + (dz2 * dz2);
                if (sqDist < 1E-12) {
                    return new XLine3DModule_ClPts(0, [Pnt_$ctor_Z7AD9E565(ptAx, ptAy, ptAz)]);
                }
                else {
                    return new XLine3DModule_ClPts(1, [Pnt_$ctor_Z7AD9E565(ptAx, ptAy, ptAz), Pnt_$ctor_Z7AD9E565(ptBx, ptBy, ptBz), sqDist]);
                }
            }
            else {
                let tupledArg;
                const pAx_1 = pAx;
                const pAy_1 = pAy;
                const pAz_1 = pAz;
                const pBx_1 = pBx;
                const pBy_1 = pBy;
                const pBz_1 = pBz;
                const vAx_1 = vAx;
                const vAy_1 = vAy;
                const vAz_1 = vAz;
                const vBx_1 = vBx;
                const vBy_1 = vBy;
                const vBz_1 = vBz;
                let patternInput;
                const pAx_2 = pAx_1;
                const pAy_2 = pAy_1;
                const pAz_2 = pAz_1;
                const pBx_2 = pBx_1;
                const pBy_2 = pBy_1;
                const pBz_2 = pBz_1;
                const vAx_2 = vAx_1;
                const vAy_2 = vAy_1;
                const vAz_2 = vAz_1;
                const vBx_2 = vBx_1;
                const vBy_2 = vBy_1;
                const vBz_2 = vBz_1;
                let uAs;
                let x_6;
                const vAx_3 = vAx_2;
                const vAy_3 = vAy_2;
                const vAz_3 = vAz_2;
                const u_1 = pBx_2 - pAx_2;
                const v = pBy_2 - pAy_2;
                const w = pBz_2 - pAz_2;
                const dotV = ((vAx_3 * u_1) + (vAy_3 * v)) + (vAz_3 * w);
                const lenSq = ((vAx_3 * vAx_3) + (vAy_3 * vAy_3)) + (vAz_3 * vAz_3);
                x_6 = (dotV / lenSq);
                uAs = ((x_6 > 0) ? ((x_6 < 1) ? x_6 : 1) : 0);
                const x_1_1 = pAx_2 + (uAs * vAx_2);
                const y_4 = pAy_2 + (uAs * vAy_2);
                const z_4 = pAz_2 + (uAs * vAz_2);
                let uBs;
                let x_8;
                const vAx_4 = vBx_2;
                const vAy_4 = vBy_2;
                const vAz_4 = vBz_2;
                const u_2 = x_1_1 - pBx_2;
                const v_1 = y_4 - pBy_2;
                const w_1 = z_4 - pBz_2;
                const dotV_1 = ((vAx_4 * u_2) + (vAy_4 * v_1)) + (vAz_4 * w_1);
                const lenSq_1 = ((vAx_4 * vAx_4) + (vAy_4 * vAy_4)) + (vAz_4 * vAz_4);
                x_8 = (dotV_1 / lenSq_1);
                uBs = ((x_8 > 0) ? ((x_8 < 1) ? x_8 : 1) : 0);
                const eBx = pBx_2 + vBx_2;
                const eBy = pBy_2 + vBy_2;
                const eBz = pBz_2 + vBz_2;
                let uAe;
                let x_10;
                const vAx_5 = vAx_2;
                const vAy_5 = vAy_2;
                const vAz_5 = vAz_2;
                const u_3 = eBx - pAx_2;
                const v_2 = eBy - pAy_2;
                const w_2 = eBz - pAz_2;
                const dotV_2 = ((vAx_5 * u_3) + (vAy_5 * v_2)) + (vAz_5 * w_2);
                const lenSq_2 = ((vAx_5 * vAx_5) + (vAy_5 * vAy_5)) + (vAz_5 * vAz_5);
                x_10 = (dotV_2 / lenSq_2);
                uAe = ((x_10 > 0) ? ((x_10 < 1) ? x_10 : 1) : 0);
                const xE = pAx_2 + (uAe * vAx_2);
                const yE = pAy_2 + (uAe * vAy_2);
                const zE = pAz_2 + (uAe * vAz_2);
                let uBe;
                let x_12;
                const vAx_6 = vBx_2;
                const vAy_6 = vBy_2;
                const vAz_6 = vBz_2;
                const u_4 = xE - pBx_2;
                const v_3 = yE - pBy_2;
                const w_3 = zE - pBz_2;
                const dotV_3 = ((vAx_6 * u_4) + (vAy_6 * v_3)) + (vAz_6 * w_3);
                const lenSq_3 = ((vAx_6 * vAx_6) + (vAy_6 * vAy_6)) + (vAz_6 * vAz_6);
                x_12 = (dotV_3 / lenSq_3);
                uBe = ((x_12 > 0) ? ((x_12 < 1) ? x_12 : 1) : 0);
                patternInput = [uAs, uBs, uAe, uBe];
                const uBs_1 = patternInput[1];
                const uBe_1 = patternInput[3];
                const uAs_1 = patternInput[0];
                const uAe_1 = patternInput[2];
                const clAsX = pAx_1 + (uAs_1 * vAx_1);
                const clAsY = pAy_1 + (uAs_1 * vAy_1);
                const clAsZ = pAz_1 + (uAs_1 * vAz_1);
                const clBsX = pBx_1 + (uBs_1 * vBx_1);
                const clBsY = pBy_1 + (uBs_1 * vBy_1);
                const clBsZ = pBz_1 + (uBs_1 * vBz_1);
                const distSqStart = (((x_13 = (clAsX - clBsX), x_13 * x_13)) + ((x_14 = (clAsY - clBsY), x_14 * x_14))) + ((x_15 = (clAsZ - clBsZ), x_15 * x_15));
                const clAeX = pAx_1 + (uAe_1 * vAx_1);
                const clAeY = pAy_1 + (uAe_1 * vAy_1);
                const clAeZ = pAz_1 + (uAe_1 * vAz_1);
                const clBeX = pBx_1 + (uBe_1 * vBx_1);
                const clBeY = pBy_1 + (uBe_1 * vBy_1);
                const clBeZ = pBz_1 + (uBe_1 * vBz_1);
                const distSqEnd = (((x_16 = (clAeX - clBeX), x_16 * x_16)) + ((x_17 = (clAeY - clBeY), x_17 * x_17))) + ((x_18 = (clAeZ - clBeZ), x_18 * x_18));
                tupledArg = ((distSqStart < distSqEnd) ? [Pnt_$ctor_Z7AD9E565(clAsX, clAsY, clAsZ), Pnt_$ctor_Z7AD9E565(clBsX, clBsY, clBsZ), distSqStart] : [Pnt_$ctor_Z7AD9E565(clAeX, clAeY, clAeZ), Pnt_$ctor_Z7AD9E565(clBeX, clBeY, clBeZ), distSqEnd]);
                return new XLine3DModule_ClPts(3, [tupledArg[0], tupledArg[1], tupledArg[2]]);
            }
        }
        else {
            let patternInput_1;
            const pAx_7 = pAx;
            const pAy_7 = pAy;
            const pAz_7 = pAz;
            const pBx_3 = pBx;
            const pBy_3 = pBy;
            const pBz_3 = pBz;
            const vAx_7 = vAx;
            const vAy_7 = vAy;
            const vAz_7 = vAz;
            const vBx_3 = vBx;
            const vBy_3 = vBy;
            const vBz_3 = vBz;
            let uAs_2;
            let x_21;
            const vAx_8 = vAx_7;
            const vAy_8 = vAy_7;
            const vAz_8 = vAz_7;
            const u_5 = pBx_3 - pAx_7;
            const v_4 = pBy_3 - pAy_7;
            const w_4 = pBz_3 - pAz_7;
            const dotV_4 = ((vAx_8 * u_5) + (vAy_8 * v_4)) + (vAz_8 * w_4);
            const lenSq_4 = ((vAx_8 * vAx_8) + (vAy_8 * vAy_8)) + (vAz_8 * vAz_8);
            x_21 = (dotV_4 / lenSq_4);
            uAs_2 = ((x_21 > 0) ? ((x_21 < 1) ? x_21 : 1) : 0);
            const x_1_2 = pAx_7 + (uAs_2 * vAx_7);
            const y_9 = pAy_7 + (uAs_2 * vAy_7);
            const z_9 = pAz_7 + (uAs_2 * vAz_7);
            let uBs_2;
            let x_23;
            const vAx_9 = vBx_3;
            const vAy_9 = vBy_3;
            const vAz_9 = vBz_3;
            const u_6 = x_1_2 - pBx_3;
            const v_5 = y_9 - pBy_3;
            const w_5 = z_9 - pBz_3;
            const dotV_5 = ((vAx_9 * u_6) + (vAy_9 * v_5)) + (vAz_9 * w_5);
            const lenSq_5 = ((vAx_9 * vAx_9) + (vAy_9 * vAy_9)) + (vAz_9 * vAz_9);
            x_23 = (dotV_5 / lenSq_5);
            uBs_2 = ((x_23 > 0) ? ((x_23 < 1) ? x_23 : 1) : 0);
            const eBx_1 = pBx_3 + vBx_3;
            const eBy_1 = pBy_3 + vBy_3;
            const eBz_1 = pBz_3 + vBz_3;
            let uAe_2;
            let x_25;
            const vAx_10 = vAx_7;
            const vAy_10 = vAy_7;
            const vAz_10 = vAz_7;
            const u_7 = eBx_1 - pAx_7;
            const v_6 = eBy_1 - pAy_7;
            const w_6 = eBz_1 - pAz_7;
            const dotV_6 = ((vAx_10 * u_7) + (vAy_10 * v_6)) + (vAz_10 * w_6);
            const lenSq_6 = ((vAx_10 * vAx_10) + (vAy_10 * vAy_10)) + (vAz_10 * vAz_10);
            x_25 = (dotV_6 / lenSq_6);
            uAe_2 = ((x_25 > 0) ? ((x_25 < 1) ? x_25 : 1) : 0);
            const xE_1 = pAx_7 + (uAe_2 * vAx_7);
            const yE_1 = pAy_7 + (uAe_2 * vAy_7);
            const zE_1 = pAz_7 + (uAe_2 * vAz_7);
            let uBe_2;
            let x_27;
            const vAx_11 = vBx_3;
            const vAy_11 = vBy_3;
            const vAz_11 = vBz_3;
            const u_8 = xE_1 - pBx_3;
            const v_7 = yE_1 - pBy_3;
            const w_7 = zE_1 - pBz_3;
            const dotV_7 = ((vAx_11 * u_8) + (vAy_11 * v_7)) + (vAz_11 * w_7);
            const lenSq_7 = ((vAx_11 * vAx_11) + (vAy_11 * vAy_11)) + (vAz_11 * vAz_11);
            x_27 = (dotV_7 / lenSq_7);
            uBe_2 = ((x_27 > 0) ? ((x_27 < 1) ? x_27 : 1) : 0);
            patternInput_1 = [uAs_2, uBs_2, uAe_2, uBe_2];
            const uBs_3 = patternInput_1[1];
            const uBe_3 = patternInput_1[3];
            const uAs_3 = patternInput_1[0];
            const uAe_3 = patternInput_1[2];
            const tA = (uAs_3 + uAe_3) * 0.5;
            const tB = (uBs_3 + uBe_3) * 0.5;
            const a = Pnt_$ctor_Z7AD9E565(pAx + (tA * vAx), pAy + (tA * vAy), pAz + (tA * vAz));
            const b = Pnt_$ctor_Z7AD9E565(pBx + (tB * vBx), pBy + (tB * vBy), pBz + (tB * vBz));
            return new XLine3DModule_ClPts(2, [a, b]);
        }
    }
}

/**
 * Gets the squared distance between two finite lines. Works on parallel and skew lines too.
 */
export function XLine3D_getSqDistance_Z15A9A3C0(pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz) {
    let x_9, x_10, x_11, x_12, x_13, x_14;
    const crossX = (vAy * vBz) - (vAz * vBy);
    const crossY = (vAz * vBx) - (vAx * vBz);
    const crossZ = (vAx * vBy) - (vAy * vBx);
    const crossMagSq = ((crossX * crossX) + (crossY * crossY)) + (crossZ * crossZ);
    const dx = pBx - pAx;
    const dy = pBy - pAy;
    const dz = pBz - pAz;
    const numerX_t = (dy * vBz) - (dz * vBy);
    const numerY_t = (dz * vBx) - (dx * vBz);
    const numerZ_t = (dx * vBy) - (dy * vBx);
    const numerator_t = ((numerX_t * crossX) + (numerY_t * crossY)) + (numerZ_t * crossZ);
    const t = numerator_t / crossMagSq;
    const numerX_u = (dy * vAz) - (dz * vAy);
    const numerY_u = (dz * vAx) - (dx * vAz);
    const numerZ_u = (dx * vAy) - (dy * vAx);
    const numerator_u = ((numerX_u * crossX) + (numerY_u * crossY)) + (numerZ_u * crossZ);
    const u = numerator_u / crossMagSq;
    if ((((t >= 0) && (t <= 1)) && (u >= 0)) && (u <= 1)) {
        const ptAx = pAx + (t * vAx);
        const ptAy = pAy + (t * vAy);
        const ptAz = pAz + (t * vAz);
        const ptBx = pBx + (u * vBx);
        const ptBy = pBy + (u * vBy);
        const ptBz = pBz + (u * vBz);
        const dx2 = ptAx - ptBx;
        const dy2 = ptAy - ptBy;
        const dz2 = ptAz - ptBz;
        return ((dx2 * dx2) + (dy2 * dy2)) + (dz2 * dz2);
    }
    else {
        const pAx_1 = pAx;
        const pAy_1 = pAy;
        const pAz_1 = pAz;
        const pBx_1 = pBx;
        const pBy_1 = pBy;
        const pBz_1 = pBz;
        const vAx_1 = vAx;
        const vAy_1 = vAy;
        const vAz_1 = vAz;
        const vBx_1 = vBx;
        const vBy_1 = vBy;
        const vBz_1 = vBz;
        let patternInput;
        const pAx_2 = pAx_1;
        const pAy_2 = pAy_1;
        const pAz_2 = pAz_1;
        const pBx_2 = pBx_1;
        const pBy_2 = pBy_1;
        const pBz_2 = pBz_1;
        const vAx_2 = vAx_1;
        const vAy_2 = vAy_1;
        const vAz_2 = vAz_1;
        const vBx_2 = vBx_1;
        const vBy_2 = vBy_1;
        const vBz_2 = vBz_1;
        let uAs;
        let x_2;
        const vAx_3 = vAx_2;
        const vAy_3 = vAy_2;
        const vAz_3 = vAz_2;
        const u_1 = pBx_2 - pAx_2;
        const v = pBy_2 - pAy_2;
        const w = pBz_2 - pAz_2;
        const dotV = ((vAx_3 * u_1) + (vAy_3 * v)) + (vAz_3 * w);
        const lenSq = ((vAx_3 * vAx_3) + (vAy_3 * vAy_3)) + (vAz_3 * vAz_3);
        x_2 = (dotV / lenSq);
        uAs = ((x_2 > 0) ? ((x_2 < 1) ? x_2 : 1) : 0);
        const x_1_1 = pAx_2 + (uAs * vAx_2);
        const y_1 = pAy_2 + (uAs * vAy_2);
        const z_1 = pAz_2 + (uAs * vAz_2);
        let uBs;
        let x_4;
        const vAx_4 = vBx_2;
        const vAy_4 = vBy_2;
        const vAz_4 = vBz_2;
        const u_2 = x_1_1 - pBx_2;
        const v_1 = y_1 - pBy_2;
        const w_1 = z_1 - pBz_2;
        const dotV_1 = ((vAx_4 * u_2) + (vAy_4 * v_1)) + (vAz_4 * w_1);
        const lenSq_1 = ((vAx_4 * vAx_4) + (vAy_4 * vAy_4)) + (vAz_4 * vAz_4);
        x_4 = (dotV_1 / lenSq_1);
        uBs = ((x_4 > 0) ? ((x_4 < 1) ? x_4 : 1) : 0);
        const eBx = pBx_2 + vBx_2;
        const eBy = pBy_2 + vBy_2;
        const eBz = pBz_2 + vBz_2;
        let uAe;
        let x_6;
        const vAx_5 = vAx_2;
        const vAy_5 = vAy_2;
        const vAz_5 = vAz_2;
        const u_3 = eBx - pAx_2;
        const v_2 = eBy - pAy_2;
        const w_2 = eBz - pAz_2;
        const dotV_2 = ((vAx_5 * u_3) + (vAy_5 * v_2)) + (vAz_5 * w_2);
        const lenSq_2 = ((vAx_5 * vAx_5) + (vAy_5 * vAy_5)) + (vAz_5 * vAz_5);
        x_6 = (dotV_2 / lenSq_2);
        uAe = ((x_6 > 0) ? ((x_6 < 1) ? x_6 : 1) : 0);
        const xE = pAx_2 + (uAe * vAx_2);
        const yE = pAy_2 + (uAe * vAy_2);
        const zE = pAz_2 + (uAe * vAz_2);
        let uBe;
        let x_8;
        const vAx_6 = vBx_2;
        const vAy_6 = vBy_2;
        const vAz_6 = vBz_2;
        const u_4 = xE - pBx_2;
        const v_3 = yE - pBy_2;
        const w_3 = zE - pBz_2;
        const dotV_3 = ((vAx_6 * u_4) + (vAy_6 * v_3)) + (vAz_6 * w_3);
        const lenSq_3 = ((vAx_6 * vAx_6) + (vAy_6 * vAy_6)) + (vAz_6 * vAz_6);
        x_8 = (dotV_3 / lenSq_3);
        uBe = ((x_8 > 0) ? ((x_8 < 1) ? x_8 : 1) : 0);
        patternInput = [uAs, uBs, uAe, uBe];
        const uBs_1 = patternInput[1];
        const uBe_1 = patternInput[3];
        const uAs_1 = patternInput[0];
        const uAe_1 = patternInput[2];
        const clAsX = pAx_1 + (uAs_1 * vAx_1);
        const clAsY = pAy_1 + (uAs_1 * vAy_1);
        const clAsZ = pAz_1 + (uAs_1 * vAz_1);
        const clBsX = pBx_1 + (uBs_1 * vBx_1);
        const clBsY = pBy_1 + (uBs_1 * vBy_1);
        const clBsZ = pBz_1 + (uBs_1 * vBz_1);
        const distSqStart = (((x_9 = (clAsX - clBsX), x_9 * x_9)) + ((x_10 = (clAsY - clBsY), x_10 * x_10))) + ((x_11 = (clAsZ - clBsZ), x_11 * x_11));
        const clAeX = pAx_1 + (uAe_1 * vAx_1);
        const clAeY = pAy_1 + (uAe_1 * vAy_1);
        const clAeZ = pAz_1 + (uAe_1 * vAz_1);
        const clBeX = pBx_1 + (uBe_1 * vBx_1);
        const clBeY = pBy_1 + (uBe_1 * vBy_1);
        const clBeZ = pBz_1 + (uBe_1 * vBz_1);
        const distSqEnd = (((x_12 = (clAeX - clBeX), x_12 * x_12)) + ((x_13 = (clAeY - clBeY), x_13 * x_13))) + ((x_14 = (clAeZ - clBeZ), x_14 * x_14));
        return min(distSqStart, distSqEnd);
    }
}

/**
 * Checks if the two finite 3D lines are touching each other at exactly one of their end points
 * within the given tolerance.
 * This will return a separate case (5 or 6) if the lines are touching on both points.
 */
export function XLine3D_getEndsTouching_Z4454D4C5(a, b, tolerance = 1E-06) {
    let x, x_1, x_2, x_3, x_4, x_5, x_6, x_7, x_8, x_9, x_10, x_11, x_12, x_13, x_14, x_15, x_16, x_17;
    const sqTolerance = tolerance * tolerance;
    if (((((x = (a.ToX - b.FromX), x * x)) + ((x_1 = (a.ToY - b.FromY), x_1 * x_1))) + ((x_2 = (a.ToZ - b.FromZ), x_2 * x_2))) < sqTolerance) {
        if (((((x_3 = (a.FromX - b.ToX), x_3 * x_3)) + ((x_4 = (a.FromY - b.ToY), x_4 * x_4))) + ((x_5 = (a.FromZ - b.ToZ), x_5 * x_5))) < sqTolerance) {
            return new XLine3DModule_XEnds(6, []);
        }
        else {
            return new XLine3DModule_XEnds(3, []);
        }
    }
    else if (((((x_6 = (a.FromX - b.ToX), x_6 * x_6)) + ((x_7 = (a.FromY - b.ToY), x_7 * x_7))) + ((x_8 = (a.FromZ - b.ToZ), x_8 * x_8))) < sqTolerance) {
        return new XLine3DModule_XEnds(4, []);
    }
    else if (((((x_9 = (a.FromX - b.FromX), x_9 * x_9)) + ((x_10 = (a.FromY - b.FromY), x_10 * x_10))) + ((x_11 = (a.FromZ - b.FromZ), x_11 * x_11))) < sqTolerance) {
        if (((((x_12 = (a.ToX - b.ToX), x_12 * x_12)) + ((x_13 = (a.ToY - b.ToY), x_13 * x_13))) + ((x_14 = (a.ToZ - b.ToZ), x_14 * x_14))) < sqTolerance) {
            return new XLine3DModule_XEnds(5, []);
        }
        else {
            return new XLine3DModule_XEnds(1, []);
        }
    }
    else if (((((x_15 = (a.ToX - b.ToX), x_15 * x_15)) + ((x_16 = (a.ToY - b.ToY), x_16 * x_16))) + ((x_17 = (a.ToZ - b.ToZ), x_17 * x_17))) < sqTolerance) {
        return new XLine3DModule_XEnds(2, []);
    }
    else {
        return new XLine3DModule_XEnds(0, []);
    }
}

/**
 * Intersects a ray with an infinite double cone that has its axis on the Z-axis.
 */
export function XLine3D_intersectCone_1D0AE3BB(ray, coneRadius, coneBaseZ, coneTipZ) {
    let ln_1, ln_2, ln_3;
    const h = coneBaseZ - coneTipZ;
    if (!(Math.abs(h) > 1E-12)) {
        fail(`XLine3D.intersectCone: cone has zero height: coneRadius: ${coneRadius}, coneBaseZ: ${coneBaseZ}, coneTipZ: ${coneTipZ}`);
    }
    const lam = coneRadius / h;
    const lam_1 = lam * lam;
    let v;
    const ln = ray;
    v = Vec_$ctor_Z7AD9E565((ln_1 = ln, ln_1.ToX - ln_1.FromX), (ln_2 = ln, ln_2.ToY - ln_2.FromY), (ln_3 = ln, ln_3.ToZ - ln_3.FromZ));
    const f2 = (((lam_1 * v.Z) * v.Z) - (v.X * v.X)) - (v.Y * v.Y);
    if (!(Math.abs(f2) > 1E-12)) {
        return new XLine3DModule_XCone(1, []);
    }
    else {
        const f1 = (((((2 * lam_1) * ray.FromZ) * v.Z) - (((2 * lam_1) * v.Z) * coneTipZ)) - ((2 * v.Y) * ray.FromY)) - ((2 * ray.FromX) * v.X);
        const f0 = (((((lam_1 * ray.FromZ) * ray.FromZ) + ((lam_1 * coneTipZ) * coneTipZ)) - (((2 * ray.FromZ) * coneTipZ) * lam_1)) - (ray.FromY * ray.FromY)) - (ray.FromX * ray.FromX);
        const part = Math.pow(f1, 2) - ((4 * f2) * f0);
        if (part < 0) {
            return new XLine3DModule_XCone(0, []);
        }
        else {
            const sqrtPart = Math.sqrt(part);
            const div = 1 / (2 * f2);
            const u = (-f1 + sqrtPart) * div;
            const v_1 = (-f1 - sqrtPart) * div;
            if (!(Math.abs(u - v_1) > 1E-12)) {
                return new XLine3DModule_XCone(2, [(u + v_1) * 0.5]);
            }
            else {
                return new XLine3DModule_XCone(3, [u, v_1]);
            }
        }
    }
}

