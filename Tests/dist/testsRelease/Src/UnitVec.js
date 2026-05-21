
import { float } from "./Format.js";
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";

/**
 * A struct containing 3 floats, representing an 3D unitized vector.
 * All instances of this type are guaranteed to be always unitized.
 * A 3D vector represents a direction or translation in space, but not a location.
 * A 4x4 transformation matrix applied to a vector will only rotate and scale the vector but not translate it.
 * (2D unit-vectors are called 'UnitVc' )
 * Use UnitVec.create or UnitVec.createUnchecked to create instances.
 * Note: Never use the struct default constructor UnitVec() as it will create an invalid zero length vector.
 * Use UnitVec.create or UnitVec.createUnchecked instead.
 */
export class UnitVec extends Record {
    constructor(X, Y, Z) {
        super();
        this.X = X;
        this.Y = Y;
        this.Z = Z;
    }
    /**
     * Format 3D unit-vector into string including type name and nice floating point number formatting.
     */
    toString() {
        const p = this;
        return `Euclid.UnitVec: X=${float(p.X)}|Y=${float(p.Y)}|Z=${float(p.Z)}`;
    }
}

export function UnitVec_$reflection() {
    return class_type("Euclid.UnitVec", undefined, UnitVec, class_type("System.ValueType"));
}

/**
 * Unsafe internal constructor, doesn't check or unitize the input, public only for inlining.
 */
export function UnitVec_$ctor_Z7AD9E565(x, y, z) {
    return new UnitVec(x, y, z);
}

/**
 * Format 3D unit-vector into string with nice floating point number formatting of X, Y and Z
 * But without full type name as in v.ToString()
 */
export function UnitVec__get_AsString(v) {
    return `X=${float(v.X)}|Y=${float(v.Y)}|Z=${float(v.Z)}`;
}

/**
 * Format 3D unit-vector into an F# code string that can be used to recreate the unit-vector.
 */
export function UnitVec__get_AsFSharpCode(v) {
    return `UnitVec.create(${v.X}, ${v.Y}, ${v.Z})`;
}

