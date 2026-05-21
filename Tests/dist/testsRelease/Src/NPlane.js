
import { Pnt_$ctor_Z7AD9E565, Pnt__get_AsString } from "./Pnt.js";
import { UnitVec_$ctor_Z7AD9E565, UnitVec__get_AsString } from "./UnitVec.js";
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { concat } from "../fable_modules/fable-library-js.5.0.0/String.js";
import { failTooSmall } from "./EuclidErrors.js";
import { Vec_$ctor_Z7AD9E565 } from "./Vec.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_1 } from "./Vec.js";
import { Line3D_$ctor_76A78260 } from "./Line3D.js";

/**
 * A struct containing a Pnt and a UnitVec, representing an
 * unparametrized plane defined by a point and a normal vector.
 * As opposed to the PPlane, this plane is not parametrized in X, Y, and Z directions.
 * Note: Never use the struct default constructor NPlane() as it will create an invalid zero plane.
 * Use NPlane.create or NPlane.createUnchecked instead.
 */
export class NPlane extends Record {
    constructor(Origin, Normal) {
        super();
        this.Origin = Origin;
        this.Normal = Normal;
    }
    /**
     * Format NPlane into string with nicely formatted floating point numbers.
     */
    toString() {
        const pl = this;
        return `Euclid.NPlane(Origin:${Pnt__get_AsString(pl.Origin)}| Normal:${UnitVec__get_AsString(pl.Normal)})`;
    }
}

export function NPlane_$reflection() {
    return class_type("Euclid.NPlane", undefined, NPlane, class_type("System.ValueType"));
}

export function NPlane_$ctor_Z2DDF2344(pt, n) {
    return new NPlane(pt, n);
}

/**
 * Format NPlane into string with nicely formatted floating point numbers.
 * But without type name as in pl.ToString()
 */
export function NPlane__get_AsString(pl) {
    return concat("Origin:", Pnt__get_AsString(pl.Origin), "| Normal:", ...UnitVec__get_AsString(pl.Normal));
}

/**
 * Format NPlane into an F# code string that can be used to recreate the plane.
 */
export function NPlane__get_AsFSharpCode(pl) {
    return `NPlane(Pnt(${pl.Origin.X}, ${pl.Origin.Y}, ${pl.Origin.Z}), UnitVec.create(${pl.Normal.X}, ${pl.Normal.Y}, ${pl.Normal.Z}))`;
}

/**
 * Checks if two 3D planes are equal within tolerance.
 * The same tolerance is used for the origin and the tips of the normal.
 * Use a tolerance of 0.0 to check for an exact match.
 */
export function NPlane_equals(tol, a, b) {
    if (((((Math.abs(a.Origin.X - b.Origin.X) <= tol) && (Math.abs(a.Origin.Y - b.Origin.Y) <= tol)) && (Math.abs(a.Origin.Z - b.Origin.Z) <= tol)) && (Math.abs(a.Normal.X - b.Normal.X) <= tol)) && (Math.abs(a.Normal.Y - b.Normal.Y) <= tol)) {
        return Math.abs(a.Normal.Z - b.Normal.Z) <= tol;
    }
    else {
        return false;
    }
}

/**
 * Create Plane, normal vector gets unitized in constructor.
 */
export function NPlane_create_5A66521A(pt, normal) {
    let l;
    const v = normal;
    l = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    if (!(l > 1E-12)) {
        failTooSmall("NPlane.create", normal);
    }
    const li = 1 / l;
    return NPlane_$ctor_Z2DDF2344(pt, UnitVec_$ctor_Z7AD9E565(li * normal.X, li * normal.Y, li * normal.Z));
}

/**
 * Returns the line of intersection between two planes.
 * Or None if they are parallel or coincident.
 */
export function NPlane_intersect(a, b) {
    let v_1, p_1, p, v_2, a_6, f, a_4, a_5, b_5, b_4, v_3;
    const bn = b.Normal;
    const an = a.Normal;
    const ao = a.Origin;
    let v;
    const a_1 = an;
    const b_1 = bn;
    v = Vec_$ctor_Z7AD9E565((a_1.Y * b_1.Z) - (a_1.Z * b_1.Y), (a_1.Z * b_1.X) - (a_1.X * b_1.Z), (a_1.X * b_1.Y) - (a_1.Y * b_1.X));
    if (!(((v_1 = v, ((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z))) > 1E-12)) {
        return undefined;
    }
    else {
        let pa;
        const a_2 = v;
        const b_2 = an;
        pa = Vec_$ctor_Z7AD9E565_1((a_2.Y * b_2.Z) - (a_2.Z * b_2.Y), (a_2.Z * b_2.X) - (a_2.X * b_2.Z), (a_2.X * b_2.Y) - (a_2.Y * b_2.X));
        let nenner;
        const a_3 = pa;
        const b_3 = bn;
        nenner = (((a_3.X * b_3.X) + (a_3.Y * b_3.Y)) + (a_3.Z * b_3.Z));
        return (p_1 = ((p = ao, (v_2 = ((a_6 = pa, (f = (((a_4 = ((a_5 = b.Origin, (b_5 = ao, Vec_$ctor_Z7AD9E565(a_5.X - b_5.X, a_5.Y - b_5.Y, a_5.Z - b_5.Z)))), (b_4 = bn, ((a_4.X * b_4.X) + (a_4.Y * b_4.Y)) + (a_4.Z * b_4.Z)))) / nenner), Vec_$ctor_Z7AD9E565(a_6.X * f, a_6.Y * f, a_6.Z * f)))), Pnt_$ctor_Z7AD9E565(p.X + v_2.X, p.Y + v_2.Y, p.Z + v_2.Z)))), (v_3 = v, Line3D_$ctor_76A78260(p_1.X, p_1.Y, p_1.Z, p_1.X + v_3.X, p_1.Y + v_3.Y, p_1.Z + v_3.Z)));
    }
}

/**
 * Returns the parameter of intersection on a infinite line / ray with the Plane.
 * Or None if they are parallel.
 */
export function NPlane_intersectLineParameter(ln, pl) {
    let ln_2, ln_3, ln_4, a_1, a_2, b_2, ln_5, b_1;
    const n = pl.Normal;
    let nenner;
    let a;
    const ln_1 = ln;
    a = Vec_$ctor_Z7AD9E565((ln_2 = ln_1, ln_2.ToX - ln_2.FromX), (ln_3 = ln_1, ln_3.ToY - ln_3.FromY), (ln_4 = ln_1, ln_4.ToZ - ln_4.FromZ));
    const b = n;
    nenner = (((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z));
    if (!(Math.abs(nenner) > 1E-06)) {
        return undefined;
    }
    else {
        return ((a_1 = ((a_2 = pl.Origin, (b_2 = ((ln_5 = ln, Pnt_$ctor_Z7AD9E565(ln_5.FromX, ln_5.FromY, ln_5.FromZ))), Vec_$ctor_Z7AD9E565(a_2.X - b_2.X, a_2.Y - b_2.Y, a_2.Z - b_2.Z)))), (b_1 = n, ((a_1.X * b_1.X) + (a_1.Y * b_1.Y)) + (a_1.Z * b_1.Z)))) / nenner;
    }
}

/**
 * Returns intersection point of a infinite line / ray with the Plane.
 * Or None if they are parallel.
 */
export function NPlane_intersectRay(ln, pl) {
    let ln_2, ln_3, ln_4, a_1, a_2, b_2, ln_5, b_1, p, ln_6, v_1, a_3, f;
    const n = pl.Normal;
    let v;
    const ln_1 = ln;
    v = Vec_$ctor_Z7AD9E565((ln_2 = ln_1, ln_2.ToX - ln_2.FromX), (ln_3 = ln_1, ln_3.ToY - ln_3.FromY), (ln_4 = ln_1, ln_4.ToZ - ln_4.FromZ));
    let nenner;
    const a = v;
    const b = n;
    nenner = (((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z));
    const t = ((a_1 = ((a_2 = pl.Origin, (b_2 = ((ln_5 = ln, Pnt_$ctor_Z7AD9E565(ln_5.FromX, ln_5.FromY, ln_5.FromZ))), Vec_$ctor_Z7AD9E565(a_2.X - b_2.X, a_2.Y - b_2.Y, a_2.Z - b_2.Z)))), (b_1 = n, ((a_1.X * b_1.X) + (a_1.Y * b_1.Y)) + (a_1.Z * b_1.Z)))) / nenner;
    if (!(Math.abs(nenner) > 1E-06)) {
        return undefined;
    }
    else {
        return (p = ((ln_6 = ln, Pnt_$ctor_Z7AD9E565(ln_6.FromX, ln_6.FromY, ln_6.FromZ))), (v_1 = ((a_3 = v, (f = t, Vec_$ctor_Z7AD9E565(a_3.X * f, a_3.Y * f, a_3.Z * f)))), Pnt_$ctor_Z7AD9E565(p.X + v_1.X, p.Y + v_1.Y, p.Z + v_1.Z)));
    }
}

/**
 * Returns intersection point of a finite line  with the Plane.
 * Or None if they are parallel or the domain of intersection is outside 0.0 to 1.0
 * Intersection just below 0.0 or just above 1.0 within tolerance of 1e-6 are clamped to 0.0 or 1.0
 */
export function NPlane_intersectLine(ln, pl) {
    let ln_2, ln_3, ln_4, a_1, a_2, b_2, ln_5, b_1, x, p, ln_6, v_1, a_3, f, x_1;
    const n = pl.Normal;
    let v;
    const ln_1 = ln;
    v = Vec_$ctor_Z7AD9E565((ln_2 = ln_1, ln_2.ToX - ln_2.FromX), (ln_3 = ln_1, ln_3.ToY - ln_3.FromY), (ln_4 = ln_1, ln_4.ToZ - ln_4.FromZ));
    let nenner;
    const a = v;
    const b = n;
    nenner = (((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z));
    const t = ((a_1 = ((a_2 = pl.Origin, (b_2 = ((ln_5 = ln, Pnt_$ctor_Z7AD9E565(ln_5.FromX, ln_5.FromY, ln_5.FromZ))), Vec_$ctor_Z7AD9E565(a_2.X - b_2.X, a_2.Y - b_2.Y, a_2.Z - b_2.Z)))), (b_1 = n, ((a_1.X * b_1.X) + (a_1.Y * b_1.Y)) + (a_1.Z * b_1.Z)))) / nenner;
    if ((x = t, (-1E-06 < x) && (x < 1.000001))) {
        return (p = ((ln_6 = ln, Pnt_$ctor_Z7AD9E565(ln_6.FromX, ln_6.FromY, ln_6.FromZ))), (v_1 = ((a_3 = v, (f = ((x_1 = t, (x_1 > 0) ? ((x_1 < 1) ? x_1 : 1) : 0)), Vec_$ctor_Z7AD9E565(a_3.X * f, a_3.Y * f, a_3.Z * f)))), Pnt_$ctor_Z7AD9E565(p.X + v_1.X, p.Y + v_1.Y, p.Z + v_1.Z)));
    }
    else {
        return undefined;
    }
}

