
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
    t = ((((vAx_1 * (pBx - pAx)) + (vAy_1 * (pBy - pAy))) + (vAz_1 * (pBz - pAz))) / (((vAx_1 * vAx_1) + (vAy_1 * vAy_1)) + (vAz_1 * vAz_1)));
    const vx = (pAx + (vAx * t)) - pBx;
    const vy = (pAy + (vAy * t)) - pBy;
    const vz = (pAz + (vAz * t)) - pBz;
    const sqTolerance = tolerance * tolerance;
    if ((((vx * vx) + (vy * vy)) + (vz * vz)) > sqTolerance) {
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
        u_2 = ((((vAx_2 * (bex - pAx)) + (vAy_2 * (bey - pAy))) + (vAz_2 * (bez - pAz))) / (((vAx_2 * vAx_2) + (vAy_2 * vAy_2)) + (vAz_2 * vAz_2)));
        const vx2 = (pAx + (vAx * u_2)) - bex;
        const vy2 = (pAy + (vAy * u_2)) - bey;
        const vz2 = (pAz + (vAz * u_2)) - bez;
        if ((((vx2 * vx2) + (vy2 * vy2)) + (vz2 * vz2)) > sqTolerance) {
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
    let pAx_1, pAy_1, pAz_1, vAx_1, vAy_1, vAz_1, x_3, y_2, z_2, t_1, vAx_2, vAy_2, vAz_2, vx, vy, vz;
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
        const tan = crossMag / (((vAx * vBx) + (vAy * vBy)) + (vAz * vBz));
        if (Math.abs(tan) > tangent) {
            const dx = pBx - pAx;
            const dy = pBy - pAy;
            const dz = pBz - pAz;
            const t = (((((dy * vBz) - (dz * vBy)) * crossX) + (((dz * vBx) - (dx * vBz)) * crossY)) + (((dx * vBy) - (dy * vBx)) * crossZ)) / (crossMag * crossMag);
            const ptAx = pAx + (t * vAx);
            const ptAy = pAy + (t * vAy);
            const ptAz = pAz + (t * vAz);
            if (((pAx_1 = pBx, (pAy_1 = pBy, (pAz_1 = pBz, (vAx_1 = vBx, (vAy_1 = vBy, (vAz_1 = vBz, (x_3 = ptAx, (y_2 = ptAy, (z_2 = ptAz, (t_1 = ((vAx_2 = vAx_1, (vAy_2 = vAy_1, (vAz_2 = vAz_1, (((vAx_2 * (x_3 - pAx_1)) + (vAy_2 * (y_2 - pAy_1))) + (vAz_2 * (z_2 - pAz_1))) / (((vAx_2 * vAx_2) + (vAy_2 * vAy_2)) + (vAz_2 * vAz_2)))))), (vx = ((pAx_1 + (vAx_1 * t_1)) - x_3), (vy = ((pAy_1 + (vAy_1 * t_1)) - y_2), (vz = ((pAz_1 + (vAz_1 * t_1)) - z_2), ((vx * vx) + (vy * vy)) + (vz * vz))))))))))))))) < (maxSkewDistance * maxSkewDistance)) {
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
        const tan = crossMag / (((vAx * vBx) + (vAy * vBy)) + (vAz * vBz));
        if (Math.abs(tan) > tangent) {
            const crossMagSq = crossMag * crossMag;
            const dx = pBx - pAx;
            const dy = pBy - pAy;
            const dz = pBz - pAz;
            const t = (((((dy * vBz) - (dz * vBy)) * crossX) + (((dz * vBx) - (dx * vBz)) * crossY)) + (((dx * vBy) - (dy * vBx)) * crossZ)) / crossMagSq;
            if ((t > -1E-06) && (t < (1 + 1E-06))) {
                const u = (((((dy * vAz) - (dz * vAy)) * crossX) + (((dz * vAx) - (dx * vAz)) * crossY)) + (((dx * vAy) - (dy * vAx)) * crossZ)) / crossMagSq;
                if ((u > -1E-06) && (u < (1 + 1E-06))) {
                    const ptAx = pAx + (t * vAx);
                    const ptAy = pAy + (t * vAy);
                    const ptAz = pAz + (t * vAz);
                    const dx2 = ptAx - (pBx + (u * vBx));
                    const dy2 = ptAy - (pBy + (u * vBy));
                    const dz2 = ptAz - (pBz + (u * vBz));
                    if ((((dx2 * dx2) + (dy2 * dy2)) + (dz2 * dz2)) < (maxSkewDistance * maxSkewDistance)) {
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
        const tan = crossMag / (((vAx * vBx) + (vAy * vBy)) + (vAz * vBz));
        if (Math.abs(tan) > tangent) {
            const crossMagSq = crossMag * crossMag;
            const dx = pBx - pAx;
            const dy = pBy - pAy;
            const dz = pBz - pAz;
            const tupledArg = [(((((dy * vBz) - (dz * vBy)) * crossX) + (((dz * vBx) - (dx * vBz)) * crossY)) + (((dx * vBy) - (dy * vBx)) * crossZ)) / crossMagSq, (((((dy * vAz) - (dz * vAy)) * crossX) + (((dz * vAx) - (dx * vAz)) * crossY)) + (((dx * vAy) - (dy * vAx)) * crossZ)) / crossMagSq];
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
        const tan = crossMag / (((vAx * vBx) + (vAy * vBy)) + (vAz * vBz));
        if (Math.abs(tan) > tangent) {
            const crossMagSq = crossMag * crossMag;
            const dx = pBx - pAx;
            const dy = pBy - pAy;
            const dz = pBz - pAz;
            const t = (((((dy * vBz) - (dz * vBy)) * crossX) + (((dz * vBx) - (dx * vBz)) * crossY)) + (((dx * vBy) - (dy * vBx)) * crossZ)) / crossMagSq;
            const u = (((((dy * vAz) - (dz * vAy)) * crossX) + (((dz * vAx) - (dx * vAz)) * crossY)) + (((dx * vAy) - (dy * vAx)) * crossZ)) / crossMagSq;
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
                return new XLine3DModule_XRay(0, [Pnt_$ctor_Z7AD9E565(ptAx, ptAy, ptAz)]);
            }
            else {
                return new XLine3DModule_XRay(1, [Pnt_$ctor_Z7AD9E565(ptAx, ptAy, ptAz), Pnt_$ctor_Z7AD9E565(ptBx, ptBy, ptBz), sqDist]);
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
        const tan = crossMag / (((vAx * vBx) + (vAy * vBy)) + (vAz * vBz));
        if (Math.abs(tan) > tangent) {
            const crossMagSq = crossMag * crossMag;
            const dx = pBx - pAx;
            const dy = pBy - pAy;
            const dz = pBz - pAz;
            const t = (((((dy * vBz) - (dz * vBy)) * crossX) + (((dz * vBx) - (dx * vBz)) * crossY)) + (((dx * vBy) - (dy * vBx)) * crossZ)) / crossMagSq;
            const u = (((((dy * vAz) - (dz * vAy)) * crossX) + (((dz * vAx) - (dx * vAz)) * crossY)) + (((dx * vAy) - (dy * vAx)) * crossZ)) / crossMagSq;
            if ((((t > -1E-06) && (t < 1.000001)) && (u > -1E-06)) && (u < 1.000001)) {
                const dx2 = (pAx + (t * vAx)) - (pBx + (u * vBx));
                const dy2 = (pAy + (t * vAy)) - (pBy + (u * vBy));
                const dz2 = (pAz + (t * vAz)) - (pBz + (u * vBz));
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
        const tan = crossMag / (((vAx * vBx) + (vAy * vBy)) + (vAz * vBz));
        if (Math.abs(tan) > tangent) {
            const crossMagSq = crossMag * crossMag;
            const dx = pBx - pAx;
            const dy = pBy - pAy;
            const dz = pBz - pAz;
            const t = (((((dy * vBz) - (dz * vBy)) * crossX) + (((dz * vBx) - (dx * vBz)) * crossY)) + (((dx * vBy) - (dy * vBx)) * crossZ)) / crossMagSq;
            const u = (((((dy * vAz) - (dz * vAy)) * crossX) + (((dz * vAx) - (dx * vAz)) * crossY)) + (((dx * vAy) - (dy * vAx)) * crossZ)) / crossMagSq;
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
    let x_8, vAx_4, vAy_4, vAz_4, x_12, vAx_6, vAy_6, vAz_6, x_13, x_14, x_15, x_16, x_17, x_18, x_23, vAx_9, vAy_9, vAz_9, x_27, vAx_11, vAy_11, vAz_11;
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
        const tan = crossMag / (((vAx * vBx) + (vAy * vBy)) + (vAz * vBz));
        if (Math.abs(tan) > tangent) {
            const crossMagSq = crossMag * crossMag;
            const dx = pBx - pAx;
            const dy = pBy - pAy;
            const dz = pBz - pAz;
            const t = (((((dy * vBz) - (dz * vBy)) * crossX) + (((dz * vBx) - (dx * vBz)) * crossY)) + (((dx * vBy) - (dy * vBx)) * crossZ)) / crossMagSq;
            const u = (((((dy * vAz) - (dz * vAy)) * crossX) + (((dz * vAx) - (dx * vAz)) * crossY)) + (((dx * vAy) - (dy * vAx)) * crossZ)) / crossMagSq;
            if ((((t >= -1E-06) && (t <= 1.000001)) && (u >= -1E-06)) && (u <= 1.000001)) {
                const dx2 = (pAx + (t * vAx)) - (pBx + (u * vBx));
                const dy2 = (pAy + (t * vAy)) - (pBy + (u * vBy));
                const dz2 = (pAz + (t * vAz)) - (pBz + (u * vBz));
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
                x_6 = ((((vAx_3 * (pBx_2 - pAx_2)) + (vAy_3 * (pBy_2 - pAy_2))) + (vAz_3 * (pBz_2 - pAz_2))) / (((vAx_3 * vAx_3) + (vAy_3 * vAy_3)) + (vAz_3 * vAz_3)));
                uAs = ((x_6 > 0) ? ((x_6 < 1) ? x_6 : 1) : 0);
                let uAe;
                let x_10;
                const vAx_5 = vAx_2;
                const vAy_5 = vAy_2;
                const vAz_5 = vAz_2;
                x_10 = ((((vAx_5 * ((pBx_2 + vBx_2) - pAx_2)) + (vAy_5 * ((pBy_2 + vBy_2) - pAy_2))) + (vAz_5 * ((pBz_2 + vBz_2) - pAz_2))) / (((vAx_5 * vAx_5) + (vAy_5 * vAy_5)) + (vAz_5 * vAz_5)));
                uAe = ((x_10 > 0) ? ((x_10 < 1) ? x_10 : 1) : 0);
                patternInput = [uAs, (x_8 = ((vAx_4 = vBx_2, (vAy_4 = vBy_2, (vAz_4 = vBz_2, (((vAx_4 * ((pAx_2 + (uAs * vAx_2)) - pBx_2)) + (vAy_4 * ((pAy_2 + (uAs * vAy_2)) - pBy_2))) + (vAz_4 * ((pAz_2 + (uAs * vAz_2)) - pBz_2))) / (((vAx_4 * vAx_4) + (vAy_4 * vAy_4)) + (vAz_4 * vAz_4)))))), (x_8 > 0) ? ((x_8 < 1) ? x_8 : 1) : 0), uAe, (x_12 = ((vAx_6 = vBx_2, (vAy_6 = vBy_2, (vAz_6 = vBz_2, (((vAx_6 * ((pAx_2 + (uAe * vAx_2)) - pBx_2)) + (vAy_6 * ((pAy_2 + (uAe * vAy_2)) - pBy_2))) + (vAz_6 * ((pAz_2 + (uAe * vAz_2)) - pBz_2))) / (((vAx_6 * vAx_6) + (vAy_6 * vAy_6)) + (vAz_6 * vAz_6)))))), (x_12 > 0) ? ((x_12 < 1) ? x_12 : 1) : 0)];
                const uBs_1 = patternInput[1];
                const uBe_1 = patternInput[3];
                const uAs_1 = patternInput[0];
                const uAe_1 = patternInput[2];
                const distSqStart = (((x_13 = ((pAx_1 + (uAs_1 * vAx_1)) - (pBx_1 + (uBs_1 * vBx_1))), x_13 * x_13)) + ((x_14 = ((pAy_1 + (uAs_1 * vAy_1)) - (pBy_1 + (uBs_1 * vBy_1))), x_14 * x_14))) + ((x_15 = ((pAz_1 + (uAs_1 * vAz_1)) - (pBz_1 + (uBs_1 * vBz_1))), x_15 * x_15));
                const distSqEnd = (((x_16 = ((pAx_1 + (uAe_1 * vAx_1)) - (pBx_1 + (uBe_1 * vBx_1))), x_16 * x_16)) + ((x_17 = ((pAy_1 + (uAe_1 * vAy_1)) - (pBy_1 + (uBe_1 * vBy_1))), x_17 * x_17))) + ((x_18 = ((pAz_1 + (uAe_1 * vAz_1)) - (pBz_1 + (uBe_1 * vBz_1))), x_18 * x_18));
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
            x_21 = ((((vAx_8 * (pBx_3 - pAx_7)) + (vAy_8 * (pBy_3 - pAy_7))) + (vAz_8 * (pBz_3 - pAz_7))) / (((vAx_8 * vAx_8) + (vAy_8 * vAy_8)) + (vAz_8 * vAz_8)));
            uAs_2 = ((x_21 > 0) ? ((x_21 < 1) ? x_21 : 1) : 0);
            let uAe_2;
            let x_25;
            const vAx_10 = vAx_7;
            const vAy_10 = vAy_7;
            const vAz_10 = vAz_7;
            x_25 = ((((vAx_10 * ((pBx_3 + vBx_3) - pAx_7)) + (vAy_10 * ((pBy_3 + vBy_3) - pAy_7))) + (vAz_10 * ((pBz_3 + vBz_3) - pAz_7))) / (((vAx_10 * vAx_10) + (vAy_10 * vAy_10)) + (vAz_10 * vAz_10)));
            uAe_2 = ((x_25 > 0) ? ((x_25 < 1) ? x_25 : 1) : 0);
            patternInput_1 = [uAs_2, (x_23 = ((vAx_9 = vBx_3, (vAy_9 = vBy_3, (vAz_9 = vBz_3, (((vAx_9 * ((pAx_7 + (uAs_2 * vAx_7)) - pBx_3)) + (vAy_9 * ((pAy_7 + (uAs_2 * vAy_7)) - pBy_3))) + (vAz_9 * ((pAz_7 + (uAs_2 * vAz_7)) - pBz_3))) / (((vAx_9 * vAx_9) + (vAy_9 * vAy_9)) + (vAz_9 * vAz_9)))))), (x_23 > 0) ? ((x_23 < 1) ? x_23 : 1) : 0), uAe_2, (x_27 = ((vAx_11 = vBx_3, (vAy_11 = vBy_3, (vAz_11 = vBz_3, (((vAx_11 * ((pAx_7 + (uAe_2 * vAx_7)) - pBx_3)) + (vAy_11 * ((pAy_7 + (uAe_2 * vAy_7)) - pBy_3))) + (vAz_11 * ((pAz_7 + (uAe_2 * vAz_7)) - pBz_3))) / (((vAx_11 * vAx_11) + (vAy_11 * vAy_11)) + (vAz_11 * vAz_11)))))), (x_27 > 0) ? ((x_27 < 1) ? x_27 : 1) : 0)];
            return new XLine3DModule_ClParams(2, [(patternInput_1[0] + patternInput_1[2]) * 0.5, (patternInput_1[1] + patternInput_1[3]) * 0.5]);
        }
    }
}

/**
 * Gets the point or points on the finite lines where the lines are closest to each other.
 * For parallel lines, this will return the middle point of any overlapping segment.
 * For skew lines, this returns the two closest approach points.
 */
export function XLine3D_getClosestPoints_Z1F58440C(pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz, tangent = 0.004363350820701567, tooShortTolerance = 1E-06) {
    let x_8, vAx_4, vAy_4, vAz_4, x_12, vAx_6, vAy_6, vAz_6, x_13, x_14, x_15, x_16, x_17, x_18, x_23, vAx_9, vAy_9, vAz_9, x_27, vAx_11, vAy_11, vAz_11;
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
        const tan = crossMag / (((vAx * vBx) + (vAy * vBy)) + (vAz * vBz));
        if (Math.abs(tan) > tangent) {
            const crossMagSq = crossMag * crossMag;
            const dx = pBx - pAx;
            const dy = pBy - pAy;
            const dz = pBz - pAz;
            const t = (((((dy * vBz) - (dz * vBy)) * crossX) + (((dz * vBx) - (dx * vBz)) * crossY)) + (((dx * vBy) - (dy * vBx)) * crossZ)) / crossMagSq;
            const u = (((((dy * vAz) - (dz * vAy)) * crossX) + (((dz * vAx) - (dx * vAz)) * crossY)) + (((dx * vAy) - (dy * vAx)) * crossZ)) / crossMagSq;
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
                x_6 = ((((vAx_3 * (pBx_2 - pAx_2)) + (vAy_3 * (pBy_2 - pAy_2))) + (vAz_3 * (pBz_2 - pAz_2))) / (((vAx_3 * vAx_3) + (vAy_3 * vAy_3)) + (vAz_3 * vAz_3)));
                uAs = ((x_6 > 0) ? ((x_6 < 1) ? x_6 : 1) : 0);
                let uAe;
                let x_10;
                const vAx_5 = vAx_2;
                const vAy_5 = vAy_2;
                const vAz_5 = vAz_2;
                x_10 = ((((vAx_5 * ((pBx_2 + vBx_2) - pAx_2)) + (vAy_5 * ((pBy_2 + vBy_2) - pAy_2))) + (vAz_5 * ((pBz_2 + vBz_2) - pAz_2))) / (((vAx_5 * vAx_5) + (vAy_5 * vAy_5)) + (vAz_5 * vAz_5)));
                uAe = ((x_10 > 0) ? ((x_10 < 1) ? x_10 : 1) : 0);
                patternInput = [uAs, (x_8 = ((vAx_4 = vBx_2, (vAy_4 = vBy_2, (vAz_4 = vBz_2, (((vAx_4 * ((pAx_2 + (uAs * vAx_2)) - pBx_2)) + (vAy_4 * ((pAy_2 + (uAs * vAy_2)) - pBy_2))) + (vAz_4 * ((pAz_2 + (uAs * vAz_2)) - pBz_2))) / (((vAx_4 * vAx_4) + (vAy_4 * vAy_4)) + (vAz_4 * vAz_4)))))), (x_8 > 0) ? ((x_8 < 1) ? x_8 : 1) : 0), uAe, (x_12 = ((vAx_6 = vBx_2, (vAy_6 = vBy_2, (vAz_6 = vBz_2, (((vAx_6 * ((pAx_2 + (uAe * vAx_2)) - pBx_2)) + (vAy_6 * ((pAy_2 + (uAe * vAy_2)) - pBy_2))) + (vAz_6 * ((pAz_2 + (uAe * vAz_2)) - pBz_2))) / (((vAx_6 * vAx_6) + (vAy_6 * vAy_6)) + (vAz_6 * vAz_6)))))), (x_12 > 0) ? ((x_12 < 1) ? x_12 : 1) : 0)];
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
            x_21 = ((((vAx_8 * (pBx_3 - pAx_7)) + (vAy_8 * (pBy_3 - pAy_7))) + (vAz_8 * (pBz_3 - pAz_7))) / (((vAx_8 * vAx_8) + (vAy_8 * vAy_8)) + (vAz_8 * vAz_8)));
            uAs_2 = ((x_21 > 0) ? ((x_21 < 1) ? x_21 : 1) : 0);
            let uAe_2;
            let x_25;
            const vAx_10 = vAx_7;
            const vAy_10 = vAy_7;
            const vAz_10 = vAz_7;
            x_25 = ((((vAx_10 * ((pBx_3 + vBx_3) - pAx_7)) + (vAy_10 * ((pBy_3 + vBy_3) - pAy_7))) + (vAz_10 * ((pBz_3 + vBz_3) - pAz_7))) / (((vAx_10 * vAx_10) + (vAy_10 * vAy_10)) + (vAz_10 * vAz_10)));
            uAe_2 = ((x_25 > 0) ? ((x_25 < 1) ? x_25 : 1) : 0);
            patternInput_1 = [uAs_2, (x_23 = ((vAx_9 = vBx_3, (vAy_9 = vBy_3, (vAz_9 = vBz_3, (((vAx_9 * ((pAx_7 + (uAs_2 * vAx_7)) - pBx_3)) + (vAy_9 * ((pAy_7 + (uAs_2 * vAy_7)) - pBy_3))) + (vAz_9 * ((pAz_7 + (uAs_2 * vAz_7)) - pBz_3))) / (((vAx_9 * vAx_9) + (vAy_9 * vAy_9)) + (vAz_9 * vAz_9)))))), (x_23 > 0) ? ((x_23 < 1) ? x_23 : 1) : 0), uAe_2, (x_27 = ((vAx_11 = vBx_3, (vAy_11 = vBy_3, (vAz_11 = vBz_3, (((vAx_11 * ((pAx_7 + (uAe_2 * vAx_7)) - pBx_3)) + (vAy_11 * ((pAy_7 + (uAe_2 * vAy_7)) - pBy_3))) + (vAz_11 * ((pAz_7 + (uAe_2 * vAz_7)) - pBz_3))) / (((vAx_11 * vAx_11) + (vAy_11 * vAy_11)) + (vAz_11 * vAz_11)))))), (x_27 > 0) ? ((x_27 < 1) ? x_27 : 1) : 0)];
            const tA = (patternInput_1[0] + patternInput_1[2]) * 0.5;
            const tB = (patternInput_1[1] + patternInput_1[3]) * 0.5;
            return new XLine3DModule_ClPts(2, [Pnt_$ctor_Z7AD9E565(pAx + (tA * vAx), pAy + (tA * vAy), pAz + (tA * vAz)), Pnt_$ctor_Z7AD9E565(pBx + (tB * vBx), pBy + (tB * vBy), pBz + (tB * vBz))]);
        }
    }
}

/**
 * Gets the squared distance between two finite lines. Works on parallel and skew lines too.
 */
export function XLine3D_getSqDistance_Z15A9A3C0(pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz) {
    let x_4, vAx_4, vAy_4, vAz_4, x_8, vAx_6, vAy_6, vAz_6, x_9, x_10, x_11, x_12, x_13, x_14;
    const crossX = (vAy * vBz) - (vAz * vBy);
    const crossY = (vAz * vBx) - (vAx * vBz);
    const crossZ = (vAx * vBy) - (vAy * vBx);
    const crossMagSq = ((crossX * crossX) + (crossY * crossY)) + (crossZ * crossZ);
    const dx = pBx - pAx;
    const dy = pBy - pAy;
    const dz = pBz - pAz;
    const t = (((((dy * vBz) - (dz * vBy)) * crossX) + (((dz * vBx) - (dx * vBz)) * crossY)) + (((dx * vBy) - (dy * vBx)) * crossZ)) / crossMagSq;
    const u = (((((dy * vAz) - (dz * vAy)) * crossX) + (((dz * vAx) - (dx * vAz)) * crossY)) + (((dx * vAy) - (dy * vAx)) * crossZ)) / crossMagSq;
    if ((((t >= 0) && (t <= 1)) && (u >= 0)) && (u <= 1)) {
        const dx2 = (pAx + (t * vAx)) - (pBx + (u * vBx));
        const dy2 = (pAy + (t * vAy)) - (pBy + (u * vBy));
        const dz2 = (pAz + (t * vAz)) - (pBz + (u * vBz));
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
        x_2 = ((((vAx_3 * (pBx_2 - pAx_2)) + (vAy_3 * (pBy_2 - pAy_2))) + (vAz_3 * (pBz_2 - pAz_2))) / (((vAx_3 * vAx_3) + (vAy_3 * vAy_3)) + (vAz_3 * vAz_3)));
        uAs = ((x_2 > 0) ? ((x_2 < 1) ? x_2 : 1) : 0);
        let uAe;
        let x_6;
        const vAx_5 = vAx_2;
        const vAy_5 = vAy_2;
        const vAz_5 = vAz_2;
        x_6 = ((((vAx_5 * ((pBx_2 + vBx_2) - pAx_2)) + (vAy_5 * ((pBy_2 + vBy_2) - pAy_2))) + (vAz_5 * ((pBz_2 + vBz_2) - pAz_2))) / (((vAx_5 * vAx_5) + (vAy_5 * vAy_5)) + (vAz_5 * vAz_5)));
        uAe = ((x_6 > 0) ? ((x_6 < 1) ? x_6 : 1) : 0);
        patternInput = [uAs, (x_4 = ((vAx_4 = vBx_2, (vAy_4 = vBy_2, (vAz_4 = vBz_2, (((vAx_4 * ((pAx_2 + (uAs * vAx_2)) - pBx_2)) + (vAy_4 * ((pAy_2 + (uAs * vAy_2)) - pBy_2))) + (vAz_4 * ((pAz_2 + (uAs * vAz_2)) - pBz_2))) / (((vAx_4 * vAx_4) + (vAy_4 * vAy_4)) + (vAz_4 * vAz_4)))))), (x_4 > 0) ? ((x_4 < 1) ? x_4 : 1) : 0), uAe, (x_8 = ((vAx_6 = vBx_2, (vAy_6 = vBy_2, (vAz_6 = vBz_2, (((vAx_6 * ((pAx_2 + (uAe * vAx_2)) - pBx_2)) + (vAy_6 * ((pAy_2 + (uAe * vAy_2)) - pBy_2))) + (vAz_6 * ((pAz_2 + (uAe * vAz_2)) - pBz_2))) / (((vAx_6 * vAx_6) + (vAy_6 * vAy_6)) + (vAz_6 * vAz_6)))))), (x_8 > 0) ? ((x_8 < 1) ? x_8 : 1) : 0)];
        const uBs_1 = patternInput[1];
        const uBe_1 = patternInput[3];
        const uAs_1 = patternInput[0];
        const uAe_1 = patternInput[2];
        return min((((x_9 = ((pAx_1 + (uAs_1 * vAx_1)) - (pBx_1 + (uBs_1 * vBx_1))), x_9 * x_9)) + ((x_10 = ((pAy_1 + (uAs_1 * vAy_1)) - (pBy_1 + (uBs_1 * vBy_1))), x_10 * x_10))) + ((x_11 = ((pAz_1 + (uAs_1 * vAz_1)) - (pBz_1 + (uBs_1 * vBz_1))), x_11 * x_11)), (((x_12 = ((pAx_1 + (uAe_1 * vAx_1)) - (pBx_1 + (uBe_1 * vBx_1))), x_12 * x_12)) + ((x_13 = ((pAy_1 + (uAe_1 * vAy_1)) - (pBy_1 + (uBe_1 * vBy_1))), x_13 * x_13))) + ((x_14 = ((pAz_1 + (uAe_1 * vAz_1)) - (pBz_1 + (uBe_1 * vBz_1))), x_14 * x_14)));
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

