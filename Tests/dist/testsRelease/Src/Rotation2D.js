
import { concat } from "../fable_modules/fable-library-js.5.0.0/String.js";
import { float } from "./Format.js";
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { failUnit2 } from "./EuclidErrors.js";

/**
 * A struct containing 2 floats, representing a 2D Counter Clockwise rotation.
 * It can be applied in World X, Y or Z plane.
 * Internally stored just as a Sine and Cosine value.
 * For arbitrary rotations use Quaternions or 4x4 Matrix.
 * However this module has much better performance than the more general Matrix4x4 or a Quaternion.
 * Note: Never use the struct default constructor Rotation2D() as it will create an invalid zero Rotation2D.
 * Use Rotation2D.createFromRadians, Rotation2D.createFromDegrees, or Rotation2D.createUnchecked instead.
 */
export class Rotation2D extends Record {
    constructor(Sin, Cos) {
        super();
        this.Sin = Sin;
        this.Cos = Cos;
    }
    /**
     * Format rotation into string showing angle in Degrees as nicely formatted floating point number.
     */
    toString() {
        const r = this;
        return concat("Euclid.Rotation2D of ", float(57.29577951308232 * Math.atan2(r.Sin, r.Cos)), ..."° Degrees.");
    }
}

export function Rotation2D_$reflection() {
    return class_type("Euclid.Rotation2D", undefined, Rotation2D, class_type("System.ValueType"));
}

/**
 * Unsafe internal constructor, public only for inlining.
 */
export function Rotation2D_$ctor_7B00E9A0(sin, cos) {
    return new Rotation2D(sin, cos);
}

/**
 * Format rotation into string showing angle in Degrees as nicely formatted floating point number.
 * But without type name as in r.ToString()
 */
export function Rotation2D__get_AsString(r) {
    return concat(float(57.29577951308232 * Math.atan2(r.Sin, r.Cos)), ..."° Degrees.");
}

/**
 * Format Rotation2D into an F# code string that can be used to recreate the rotation.
 */
export function Rotation2D__get_AsFSharpCode(r) {
    return `Rotation2D.createUnchecked(${r.Sin}, ${r.Cos})`;
}

/**
 * Construct a 2D Rotation from two unit vectors.
 * The rotation represents the counter-clockwise angle from vector 'a' to vector 'b'.
 * Uses the dot product for cosine and cross product for sine.
 */
export function Rotation2D_createFromVectors_67D9FFC0(a, b) {
    let a_2, b_2, a_1, b_1;
    return Rotation2D_$ctor_7B00E9A0((a_2 = a, (b_2 = b, (a_2.X * b_2.Y) - (a_2.Y * b_2.X))), (a_1 = a, (b_1 = b, (a_1.X * b_1.X) + (a_1.Y * b_1.Y))));
}

/**
 * Construct a 2D Rotation from two 2D vectors.
 * The rotation represents the counter-clockwise angle from vector 'a' to vector 'b'.
 * Normalizes both vectors internally and uses dot product for cosine and cross product for sine.
 * Fails if either vector has near-zero length.
 */
export function Rotation2D_createFromVectors_Z53905080(a, b) {
    let la;
    const v = a;
    const x = v.X;
    const y = v.Y;
    la = Math.sqrt((x * x) + (y * y));
    let lb;
    const v_1 = b;
    const x_1 = v_1.X;
    const y_1 = v_1.Y;
    lb = Math.sqrt((x_1 * x_1) + (y_1 * y_1));
    if (!(la > 1E-12) ? true : !(lb > 1E-12)) {
        failUnit2("Rotation2D.createFromVectors", la, lb);
    }
    const ax = a.X / la;
    const ay = a.Y / la;
    const bx = b.X / lb;
    const by = b.Y / lb;
    return Rotation2D_$ctor_7B00E9A0((ax * by) - (ay * bx), (ax * bx) + (ay * by));
}

