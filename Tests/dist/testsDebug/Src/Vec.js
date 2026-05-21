
import { float } from "./Format.js";
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { isInfinity } from "../fable_modules/fable-library-js.5.0.0/Double.js";
import { failDivide, failNaN3 } from "./EuclidErrors.js";

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
        const x = float(v.X);
        const y = float(v.Y);
        const z = float(v.Z);
        const len = float(Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z)));
        return `Euclid.Vec: X=${x}|Y=${y}|Z=${z}|length: ${len}`;
    }
}

export function Vec_$reflection() {
    return class_type("Euclid.Vec", undefined, Vec, class_type("System.ValueType"));
}

/**
 * Create a new 3D vector from X, Y, and Z coordinates.
 */
export function Vec_$ctor_Z7AD9E565(x, y, z) {
    let x_1, x_2, x_3;
    if ((((x_1 = x, Number.isNaN(x_1) ? true : isInfinity(x_1))) ? true : ((x_2 = y, Number.isNaN(x_2) ? true : isInfinity(x_2)))) ? true : ((x_3 = z, Number.isNaN(x_3) ? true : isInfinity(x_3)))) {
        failNaN3("Vec()", x, y, z);
    }
    return new Vec(x, y, z);
}

/**
 * Format 3D vector into string with nice floating point number formatting of X, Y and Z.
 * But without full type name or length as in v.ToString()
 */
export function Vec__get_AsString(v) {
    const x = float(v.X);
    const y = float(v.Y);
    const z = float(v.Z);
    return `X=${x}|Y=${y}|Z=${z}`;
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

