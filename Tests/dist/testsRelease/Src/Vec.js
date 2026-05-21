
import { float } from "./Format.js";
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { failDivide } from "./EuclidErrors.js";

/**
 * An immutable 3D vector of any length. Made up from 3 floats: X, Y, and Z.
 * A 3D vector represents a direction or translation in space, but not a location.
 * A 4x4 transformation matrix applied to a vector will only rotate and scale the vector but not translate it.
 * (3D unit-vectors of length 1.0 are called 'UnitVec' )
 * (2D vectors are called 'Vc' )
 */
export class Vec extends Record {
    constructor(X, Y, Z) {
        super();
        this.X = X;
        this.Y = Y;
        this.Z = Z;
    }
    /**
     * Format 3D vector into string including type name and nice floating point number formatting of X, Y, Z and length.
     */
    toString() {
        const v = this;
        return `Euclid.Vec: X=${float(v.X)}|Y=${float(v.Y)}|Z=${float(v.Z)}|length: ${float(Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z)))}`;
    }
}

export function Vec_$reflection() {
    return class_type("Euclid.Vec", undefined, Vec, class_type("System.ValueType"));
}

/**
 * Create a new 3D vector from X, Y, and Z coordinates.
 */
export function Vec_$ctor_Z7AD9E565(x, y, z) {
    return new Vec(x, y, z);
}

/**
 * Format 3D vector into string with nice floating point number formatting of X, Y and Z.
 * But without full type name or length as in v.ToString()
 */
export function Vec__get_AsString(v) {
    return `X=${float(v.X)}|Y=${float(v.Y)}|Z=${float(v.Z)}`;
}

/**
 * Format 3D vector into an F# code string that can be used to recreate the vector.
 */
export function Vec__get_AsFSharpCode(v) {
    return `Vec(${v.X}, ${v.Y}, ${v.Z})`;
}

/**
 * Divides the vector by an integer.
 * (This member is needed by Array.average and similar functions)
 */
export function Vec_DivideByInt_3165DD75(v, i) {
    if (i === 0) {
        failDivide("Vec.DivideByInt", 0, v);
    }
    const d = i;
    return Vec_$ctor_Z7AD9E565(v.X / d, v.Y / d, v.Z / d);
}

