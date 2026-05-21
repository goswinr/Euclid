
import { Pnt__get_AsFSharpCode, Pnt__get_AsString } from "./Pnt.js";
import { UnitVec__get_AsFSharpCode, UnitVec__get_AsString } from "./UnitVec.js";
import { nl } from "./Format.js";
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";

/**
 * A struct containing one 3D point and three 3D unit vectors, representing an immutable parametrized plane or frame
 * with unitized X, Y and Z Direction.
 * This struct is called 'PPlane'; the other plane 'NPlane' refers to an un-oriented plane consisting only of an origin and a normal.
 * Note: Never use the struct default constructor PPlane() as it will create an invalid zero length PPlane.
 * Use PPlane.create or PPlane.createUnchecked instead.
 */
export class PPlane extends Record {
    constructor(Origin, Xaxis, Yaxis, Zaxis) {
        super();
        this.Origin = Origin;
        this.Xaxis = Xaxis;
        this.Yaxis = Yaxis;
        this.Zaxis = Zaxis;
    }
    /**
     * Format PPlane into string with nicely formatted floating point numbers.
     */
    toString() {
        const pl = this;
        const o = Pnt__get_AsString(pl.Origin);
        const x = UnitVec__get_AsString(pl.Xaxis);
        const y = UnitVec__get_AsString(pl.Yaxis);
        const z = UnitVec__get_AsString(pl.Zaxis);
        return `Euclid.PPlane(${nl}Origin=${o}${nl}  X-axis=${x}${nl}  Y-axis=${y}${nl}  Z-axis=${z})`;
    }
}

export function PPlane_$reflection() {
    return class_type("Euclid.PPlane", undefined, PPlane, class_type("System.ValueType"));
}

/**
 * Unsafe internal constructor, doesn't check if the input is perpendicular, public only for inlining.
 */
export function PPlane_$ctor_3CB4665C(origin, axisX, axisY, axisZ) {
    return new PPlane(origin, axisX, axisY, axisZ);
}

/**
 * Format PPlane into string with nicely formatted floating point numbers.
 * But without type name as in pl.ToString()
 */
export function PPlane__get_AsString(pl) {
    const o = Pnt__get_AsString(pl.Origin);
    const x = UnitVec__get_AsString(pl.Xaxis);
    const y = UnitVec__get_AsString(pl.Yaxis);
    const z = UnitVec__get_AsString(pl.Zaxis);
    return `${nl}Origin=${o}${nl}  X-axis=${x}${nl}  Y-axis=${y}${nl}  Z-axis=${z}`;
}

/**
 * Format PPlane into an F# code string that can be used to recreate the plane.
 */
export function PPlane__get_AsFSharpCode(pl) {
    return `PPlane.createUnchecked(${Pnt__get_AsFSharpCode(pl.Origin)}, ${UnitVec__get_AsFSharpCode(pl.Xaxis)}, ${UnitVec__get_AsFSharpCode(pl.Yaxis)}, ${UnitVec__get_AsFSharpCode(pl.Zaxis)})`;
}

