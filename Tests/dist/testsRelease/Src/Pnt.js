
import { float } from "./Format.js";
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { failDivide } from "./EuclidErrors.js";

/**
 * A struct containing 3 floats, representing an immutable 3D point. X, Y, and Z.
 * A 3D point represents a location in space, but not direction. (use Vec for that.)
 * (2D Points are called 'Pt' )
 */
export class Pnt extends Record {
    constructor(X, Y, Z) {
        super();
        this.X = X;
        this.Y = Y;
        this.Z = Z;
    }
    /**
     * Format 3D point into string including type name and nice floating point number formatting.
     */
    toString() {
        const p = this;
        return `Euclid.Pnt: X=${float(p.X)}|Y=${float(p.Y)}|Z=${float(p.Z)}`;
    }
}

export function Pnt_$reflection() {
    return class_type("Euclid.Pnt", undefined, Pnt, class_type("System.ValueType"));
}

/**
 * Create a new 3D point form X, Y, and Z coordinates.
 */
export function Pnt_$ctor_Z7AD9E565(x, y, z) {
    return new Pnt(x, y, z);
}

/**
 * Format 3D point into string with nice floating point number formatting of X, Y and Z
 * But without full type name as in pt.ToString()
 */
export function Pnt__get_AsString(p) {
    return `X=${float(p.X)}|Y=${float(p.Y)}|Z=${float(p.Z)}`;
}

/**
 * Format 3D point into an F# code string that can be used to recreate the point.
 */
export function Pnt__get_AsFSharpCode(p) {
    return `Pnt(${p.X}, ${p.Y}, ${p.Z})`;
}

/**
 * Divides the 3D point by an integer.
 * (This member is needed by Array.average and similar functions)
 */
export function Pnt_DivideByInt_316ACE4F(pt, i) {
    if (i === 0) {
        failDivide("Pnt.DivideByInt", 0, pt);
    }
    const d = i;
    return Pnt_$ctor_Z7AD9E565(pt.X / d, pt.Y / d, pt.Z / d);
}

