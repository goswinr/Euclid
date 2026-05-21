
import { item, map2, chunkBySize, maxBy, map } from "../fable_modules/fable-library-js.5.0.0/Array.js";
import { join, concat, printf, toText } from "../fable_modules/fable-library-js.5.0.0/String.js";
import { comparePrimitives } from "../fable_modules/fable-library-js.5.0.0/Util.js";
import { nl } from "./Format.js";
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { Vec_$ctor_Z7AD9E565 } from "./Vec.js";
import { failDivide, fail } from "./EuclidErrors.js";
import { UnitVec_$ctor_Z7AD9E565 } from "./UnitVec.js";
import { isPositiveInfinity } from "../fable_modules/fable-library-js.5.0.0/Double.js";

/**
 * A struct containing 16 floats, representing an immutable 4x4 transformation matrix.
 * The matrix is represented in the following column-vector syntax form:
 * <code>
 * M11 M21 M31 X41
 * M12 M22 M32 Y42
 * M13 M23 M33 Z43
 * M14 M24 M34 M44
 * </code>
 * Where X41, Y42 and Z43 refer to the translation part of the matrix.
 * Note: Never use the struct default constructor Matrix() as it will create an invalid zero Matrix.
 * Use Matrix.create or Matrix.createUnchecked instead.
 */
export class Matrix extends Record {
    constructor(M11, M21, M31, X41, M12, M22, M32, Y42, M13, M23, M33, Z43, M14, M24, M34, M44) {
        super();
        this.M11 = M11;
        this.M21 = M21;
        this.M31 = M31;
        this.X41 = X41;
        this.M12 = M12;
        this.M22 = M22;
        this.M32 = M32;
        this.Y42 = Y42;
        this.M13 = M13;
        this.M23 = M23;
        this.M33 = M33;
        this.Z43 = Z43;
        this.M14 = M14;
        this.M24 = M24;
        this.M34 = M34;
        this.M44 = M44;
    }
    /**
     * Nicely formats the Matrix to a Grid of 4x4 including field names.
     * Using the following column-vector syntax form:
     * <code>
     * M11 M21 M31 X41
     * M12 M22 M32 Y42
     * M13 M23 M33 Z43
     * M14 M24 M34 M44
     * </code>
     * Where X41, Y42 and Z43 refer to the translation part of the matrix.
     */
    toString() {
        let clo;
        const m = this;
        const names = ["M11", "M21", "M31", "X41", "M12", "M22", "M32", "Y42", "M13", "M23", "M33", "Z43", "M14", "M24", "M34", "M44"];
        let ts;
        const array = Matrix__get_ToArrayByRows(m);
        ts = map((clo = toText(printf("%0.3f")), clo), array);
        const most = maxBy((s) => (s.length | 0), ts, {
            Compare: (x, y) => (comparePrimitives(x, y) | 0),
        });
        return concat("Column-Vector Transformation Matrix:", ...nl) + join("\n", map((es) => (" " + join(" | ", es)), chunkBySize(4, map2((n, v) => (((n + ": ") + (Array((most.length - v.length) + 1).join(" "))) + v), names, ts))));
    }
}

export function Matrix_$reflection() {
    return class_type("Euclid.Matrix", undefined, Matrix, class_type("System.ValueType"));
}

/**
 * Create a 4x4 Transformation Matrix.
 * This Constructor takes arguments in row-major order.
 * So (M11, M21, M31, X41, M12, ...)
 * The matrix is represented in the following column-vector syntax form:
 * <code>
 * M11 M21 M31 X41
 * M12 M22 M32 Y42
 * M13 M23 M33 Z43
 * M14 M24 M34 M44
 * </code>
 * Where X41, Y42 and Z43 refer to the translation part of the matrix.
 */
export function Matrix_$ctor_Z61E40B00(m11, m21, m31, x41, m12, m22, m32, y42, m13, m23, m33, z43, m14, m24, m34, m44) {
    return new Matrix(m11, m21, m31, x41, m12, m22, m32, y42, m13, m23, m33, z43, m14, m24, m34, m44);
}

/**
 * Returns the 16 elements column-major order:
 * <code>[| M11 M12 M13 M14 M21 M22 M23 M24 M31 M32 M33 M34 X41 Y42 Z43 M44 |]</code>
 * Where X41, Y42 and Z43 refer to the translation part of the matrix.
 */
export function Matrix__get_ToArrayByColumns(m) {
    return new Float64Array([m.M11, m.M12, m.M13, m.M14, m.M21, m.M22, m.M23, m.M24, m.M31, m.M32, m.M33, m.M34, m.X41, m.Y42, m.Z43, m.M44]);
}

/**
 * Returns the 16 elements in row-major order:
 * <code>[| M11 M21 M31 X41 M12 M22 M32 Y42 M13 M23 M33 Z43 M14 M24 M34 M44 |]</code>
 * Where X41, Y42 and Z43 refer to the translation part of the matrix.
 */
export function Matrix__get_ToArrayByRows(m) {
    return new Float64Array([m.M11, m.M21, m.M31, m.X41, m.M12, m.M22, m.M32, m.Y42, m.M13, m.M23, m.M33, m.Z43, m.M14, m.M24, m.M34, m.M44]);
}

/**
 * Nicely formats the Matrix to a Grid of 4x4 (without field names)
 * the following column-vector syntax form:
 * <code>
 * M11 M21 M31 X41
 * M12 M22 M32 Y42
 * M13 M23 M33 Z43
 * M14 M24 M34 M44
 * </code>
 * Where X41, Y42 and Z43 refer to the translation part of the matrix.
 */
export function Matrix__get_AsString(m) {
    let clo;
    let ts;
    const array = Matrix__get_ToArrayByRows(m);
    ts = map((clo = toText(printf("%0.3f")), clo), array);
    const most = maxBy((s) => (s.length | 0), ts, {
        Compare: (x, y) => (comparePrimitives(x, y) | 0),
    });
    return concat("4x4 Column-Vector Transformation Matrix:", ...nl) + join("\n", map((es) => (" " + join(" | ", es)), chunkBySize(4, map((x_1) => ((Array((most.length - x_1.length) + 1).join(" ")) + x_1), ts))));
}

/**
 * Format Matrix into an F# code string that can be used to recreate the matrix.
 * The output matches the constructor's row-major parameter order.
 */
export function Matrix__get_AsFSharpCode(m) {
    return `Matrix(${m.M11}, ${m.M21}, ${m.M31}, ${m.X41}, ${m.M12}, ${m.M22}, ${m.M32}, ${m.Y42}, ${m.M13}, ${m.M23}, ${m.M33}, ${m.Z43}, ${m.M14}, ${m.M24}, ${m.M34}, ${m.M44})`;
}

/**
 * Returns the first column vector. M11, M12 and M13.
 */
export function Matrix__get_ColumnVector1(m) {
    return Vec_$ctor_Z7AD9E565(m.M11, m.M12, m.M13);
}

/**
 * Returns the second column vector. M21, M22 and M23.
 */
export function Matrix__get_ColumnVector2(m) {
    return Vec_$ctor_Z7AD9E565(m.M21, m.M22, m.M23);
}

/**
 * Returns the third column vector. M31, M32 and M33.
 */
export function Matrix__get_ColumnVector3(m) {
    return Vec_$ctor_Z7AD9E565(m.M31, m.M32, m.M33);
}

/**
 * Returns the translation or fourth column vector. X41, Y42 and Z43.
 */
export function Matrix__get_Translation(m) {
    return Vec_$ctor_Z7AD9E565(m.X41, m.Y42, m.Z43);
}

/**
 * The determinant of the Matrix.
 * The Determinant describes the signed volume that a unit cube will have after the matrix was applied.
 */
export function Matrix__get_Determinant(m) {
    const m00 = m.M11;
    const m01 = m.M12;
    const m02 = m.M13;
    const m03 = m.M14;
    const m10 = m.M21;
    const m11 = m.M22;
    const m12 = m.M23;
    const m13 = m.M24;
    const m20 = m.M31;
    const m21 = m.M32;
    const m22 = m.M33;
    const m23 = m.M34;
    const x30 = m.X41;
    const y31 = m.Y42;
    const z32 = m.Z43;
    const m33 = m.M44;
    const m03m12 = m03 * m12;
    const m02m11 = m02 * m11;
    const m03m10 = m03 * m10;
    const m03m11 = m03 * m11;
    const m01m10 = m01 * m10;
    const m02m10 = m02 * m10;
    const m21x30 = m21 * x30;
    const m23x30 = m23 * x30;
    const m22y31 = m22 * y31;
    const m20z32 = m20 * z32;
    const m23z32 = m23 * z32;
    const m21m33 = m21 * m33;
    const m02m13 = m02 * m13;
    const m01m12 = m01 * m12;
    const m00m13 = m00 * m13;
    const m01m13 = m01 * m13;
    const m00m11 = m00 * m11;
    const m00m12 = m00 * m12;
    const m22x30 = m22 * x30;
    const m20y31 = m20 * y31;
    const m23y31 = m23 * y31;
    const m21z32 = m21 * z32;
    const m20m33 = m20 * m33;
    const m22m33 = m22 * m33;
    return (((((((((((((((((((((((m03m12 * m21x30) - (m02m13 * m21x30)) - (m03m11 * m22x30)) + (m01m13 * m22x30)) + (m02m11 * m23x30)) - (m01m12 * m23x30)) - (m03m12 * m20y31)) + (m02m13 * m20y31)) + (m03m10 * m22y31)) - (m00m13 * m22y31)) - (m02m10 * m23y31)) + (m00m12 * m23y31)) + (m03m11 * m20z32)) - (m01m13 * m20z32)) - (m03m10 * m21z32)) + (m00m13 * m21z32)) + (m01m10 * m23z32)) - (m00m11 * m23z32)) - (m02m11 * m20m33)) + (m01m12 * m20m33)) + (m02m10 * m21m33)) - (m00m12 * m21m33)) - (m01m10 * m22m33)) + (m00m11 * m22m33);
}

/**
 * Inverts the matrix.
 * If the determinant is zero the Matrix cannot be inverted.
 * An Exception is raised.
 */
export function Matrix__get_Inverse(m) {
    const m00 = m.M11;
    const m01 = m.M12;
    const m02 = m.M13;
    const m03 = m.M14;
    const m10 = m.M21;
    const m11 = m.M22;
    const m12 = m.M23;
    const m13 = m.M24;
    const m20 = m.M31;
    const m21 = m.M32;
    const m22 = m.M33;
    const m23 = m.M34;
    const x30 = m.X41;
    const y31 = m.Y42;
    const z32 = m.Z43;
    const m33 = m.M44;
    const m03_m12 = m03 * m12;
    const m02_m13 = m02 * m13;
    const m03_m11 = m03 * m11;
    const m01_m12 = m01 * m12;
    const m01_m13 = m01 * m13;
    const m02_m11 = m02 * m11;
    const m03_m10 = m03 * m10;
    const m02_m10 = m02 * m10;
    const m00_m13 = m00 * m13;
    const m00_m12 = m00 * m12;
    const m00_m11 = m00 * m11;
    const m01_m10 = m01 * m10;
    const t03 = (((((m03_m12 * m21) - (m02_m13 * m21)) - (m03_m11 * m22)) + (m01_m13 * m22)) + (m02_m11 * m23)) - (m01_m12 * m23);
    const t13 = (((((m02_m13 * m20) - (m03_m12 * m20)) + (m03_m10 * m22)) - (m00_m13 * m22)) - (m02_m10 * m23)) + (m00_m12 * m23);
    const t23 = (((((m03_m11 * m20) - (m01_m13 * m20)) - (m03_m10 * m21)) + (m00_m13 * m21)) + (m01_m10 * m23)) - (m00_m11 * m23);
    const t33 = (((((m01_m12 * m20) - (m02_m11 * m20)) + (m02_m10 * m21)) - (m00_m12 * m21)) - (m01_m10 * m22)) + (m00_m11 * m22);
    const determinant = (((t03 * x30) + (t13 * y31)) + (t23 * z32)) + (t33 * m33);
    if (Math.abs(determinant) < 1E-16) {
        fail(`Matrix has a zero or almost zero Determinant. It is smaller than 1e-16. It cannot be inverted:${nl}${m}`);
    }
    const detInv = 1 / determinant;
    const m20_y31 = m20 * y31;
    const m20_z32 = m20 * z32;
    const m20_m33 = m20 * m33;
    const m21_x30 = m21 * x30;
    const m21_z32 = m21 * z32;
    const m21_m33 = m21 * m33;
    const m22_x30 = m22 * x30;
    const m22_y31 = m22 * y31;
    const m22_m33 = m22 * m33;
    const m23_x30 = m23 * x30;
    const m23_y31 = m23 * y31;
    const m23_z32 = m23 * z32;
    return Matrix_$ctor_Z61E40B00(((((((m12 * m23_y31) - (m13 * m22_y31)) + (m13 * m21_z32)) - (m11 * m23_z32)) - (m12 * m21_m33)) + (m11 * m22_m33)) * detInv, ((((((m13 * m22_x30) - (m12 * m23_x30)) - (m13 * m20_z32)) + (m10 * m23_z32)) + (m12 * m20_m33)) - (m10 * m22_m33)) * detInv, ((((((m11 * m23_x30) - (m13 * m21_x30)) + (m13 * m20_y31)) - (m10 * m23_y31)) - (m11 * m20_m33)) + (m10 * m21_m33)) * detInv, ((((((m12 * m21_x30) - (m11 * m22_x30)) - (m12 * m20_y31)) + (m10 * m22_y31)) + (m11 * m20_z32)) - (m10 * m21_z32)) * detInv, ((((((m03 * m22_y31) - (m02 * m23_y31)) - (m03 * m21_z32)) + (m01 * m23_z32)) + (m02 * m21_m33)) - (m01 * m22_m33)) * detInv, ((((((m02 * m23_x30) - (m03 * m22_x30)) + (m03 * m20_z32)) - (m00 * m23_z32)) - (m02 * m20_m33)) + (m00 * m22_m33)) * detInv, ((((((m03 * m21_x30) - (m01 * m23_x30)) - (m03 * m20_y31)) + (m00 * m23_y31)) + (m01 * m20_m33)) - (m00 * m21_m33)) * detInv, ((((((m01 * m22_x30) - (m02 * m21_x30)) + (m02 * m20_y31)) - (m00 * m22_y31)) - (m01 * m20_z32)) + (m00 * m21_z32)) * detInv, ((((((m02_m13 * y31) - (m03_m12 * y31)) + (m03_m11 * z32)) - (m01_m13 * z32)) - (m02_m11 * m33)) + (m01_m12 * m33)) * detInv, ((((((m03_m12 * x30) - (m02_m13 * x30)) - (m03_m10 * z32)) + (m00_m13 * z32)) + (m02_m10 * m33)) - (m00_m12 * m33)) * detInv, ((((((m01_m13 * x30) - (m03_m11 * x30)) + (m03_m10 * y31)) - (m00_m13 * y31)) - (m01_m10 * m33)) + (m00_m11 * m33)) * detInv, ((((((m02_m11 * x30) - (m01_m12 * x30)) - (m02_m10 * y31)) + (m00_m12 * y31)) + (m01_m10 * z32)) - (m00_m11 * z32)) * detInv, t03 * detInv, t13 * detInv, t23 * detInv, t33 * detInv);
}

/**
 * Checks if the Matrix is an identity matrix in the form of:
 * <code>
 * 1  0  0  0
 * 0  1  0  0
 * 0  0  1  0
 * 0  0  0  1
 * </code>
 * Using an approximate tolerance of 1e-6.
 */
export function Matrix__get_IsIdentity(m) {
    let x, x_1, x_2, x_3, x_4, x_5, x_6, x_7, x_8, x_9, x_10, x_11, x_12, x_13, x_14;
    if ((((((((((((((((x = m.M11, (0.999999 < x) && (x < 1.000001))) && ((x_1 = m.M21, (-1E-06 < x_1) && (x_1 < 1E-06)))) && ((x_2 = m.M31, (-1E-06 < x_2) && (x_2 < 1E-06)))) && ((x_3 = m.X41, (-1E-06 < x_3) && (x_3 < 1E-06)))) && ((x_4 = m.M12, (-1E-06 < x_4) && (x_4 < 1E-06)))) && ((x_5 = m.M22, (0.999999 < x_5) && (x_5 < 1.000001)))) && ((x_6 = m.M32, (-1E-06 < x_6) && (x_6 < 1E-06)))) && ((x_7 = m.Y42, (-1E-06 < x_7) && (x_7 < 1E-06)))) && ((x_8 = m.M13, (-1E-06 < x_8) && (x_8 < 1E-06)))) && ((x_9 = m.M23, (-1E-06 < x_9) && (x_9 < 1E-06)))) && ((x_10 = m.M33, (0.999999 < x_10) && (x_10 < 1.000001)))) && ((x_11 = m.Z43, (-1E-06 < x_11) && (x_11 < 1E-06)))) && ((x_12 = m.M14, (-1E-06 < x_12) && (x_12 < 1E-06)))) && ((x_13 = m.M24, (-1E-06 < x_13) && (x_13 < 1E-06)))) && ((x_14 = m.M34, (-1E-06 < x_14) && (x_14 < 1E-06)))) {
        const x_15 = m.M44;
        if (0.999999 < x_15) {
            return x_15 < 1.000001;
        }
        else {
            return false;
        }
    }
    else {
        return false;
    }
}

/**
 * Checks if the Matrix is an affine transformation.
 * That means it does not do any projection.
 * The fields m.M14, m.M24 and m.M34 must be 0.0 and m.M44 must be 1.0 or very close to it.
 * Using an approximate tolerance of 1e-6.
 */
export function Matrix__get_IsAffine(m) {
    let x, x_1, x_2;
    if ((((x = m.M14, (-1E-06 < x) && (x < 1E-06))) && ((x_1 = m.M24, (-1E-06 < x_1) && (x_1 < 1E-06)))) && ((x_2 = m.M34, (-1E-06 < x_2) && (x_2 < 1E-06)))) {
        const x_3 = m.M44;
        if (0.999999 < x_3) {
            return x_3 < 1.000001;
        }
        else {
            return false;
        }
    }
    else {
        return false;
    }
}

/**
 * Checks if the Matrix is a projection transformation.
 * That means it does perform projection (perspective divide).
 * Returns true if at least one of the fields m.M14, m.M24, m.M34 is not 0.0 or m.M44 is not 1.0.
 * Using an approximate tolerance of 1e-6.
 */
export function Matrix__get_IsProjecting(m) {
    let x_1, x_3, x_5, x_7;
    if ((!((x_1 = m.M14, (-1E-06 < x_1) && (x_1 < 1E-06))) ? true : !((x_3 = m.M24, (-1E-06 < x_3) && (x_3 < 1E-06)))) ? true : !((x_5 = m.M34, (-1E-06 < x_5) && (x_5 < 1E-06)))) {
        return true;
    }
    else {
        return !((x_7 = m.M44, (0.999999 < x_7) && (x_7 < 1.000001)));
    }
}

/**
 * Returns if the Matrix is orthogonal.
 * It might also be mirroring or scaling, but not shearing or projecting.
 * It must be affine and the dot products of the three column vectors must be zero.
 */
export function Matrix__get_IsOrthogonal(m) {
    let x_1, a, b, x_2, a_1, b_1;
    if (Matrix__get_IsAffine(m)) {
        const x = Vec_$ctor_Z7AD9E565(m.M11, m.M12, m.M13);
        const y = Vec_$ctor_Z7AD9E565(m.M21, m.M22, m.M23);
        const z = Vec_$ctor_Z7AD9E565(m.M31, m.M32, m.M33);
        if (((x_1 = ((a = x, (b = y, ((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z)))), (-1E-06 < x_1) && (x_1 < 1E-06))) && ((x_2 = ((a_1 = x, (b_1 = z, ((a_1.X * b_1.X) + (a_1.Y * b_1.Y)) + (a_1.Z * b_1.Z)))), (-1E-06 < x_2) && (x_2 < 1E-06)))) {
            let x_3;
            const a_2 = y;
            const b_2 = z;
            x_3 = (((a_2.X * b_2.X) + (a_2.Y * b_2.Y)) + (a_2.Z * b_2.Z));
            if (-1E-06 < x_3) {
                return x_3 < 1E-06;
            }
            else {
                return false;
            }
        }
        else {
            return false;
        }
    }
    else {
        return false;
    }
}

/**
 * Returns if the Matrix is mirroring or reflection.
 * It might also be rotating, translating or scaling, but not projecting.
 * It must be affine and the determinate of the 3x3 part must be negative.
 * Same as m.IsReflecting.
 */
export function Matrix__get_IsMirroring(m) {
    let a, a_1, b_1, b;
    if (Matrix__get_IsAffine(m)) {
        const x = Vec_$ctor_Z7AD9E565(m.M11, m.M12, m.M13);
        const y = Vec_$ctor_Z7AD9E565(m.M21, m.M22, m.M23);
        const z = Vec_$ctor_Z7AD9E565(m.M31, m.M32, m.M33);
        return ((a = ((a_1 = x, (b_1 = y, Vec_$ctor_Z7AD9E565((a_1.Y * b_1.Z) - (a_1.Z * b_1.Y), (a_1.Z * b_1.X) - (a_1.X * b_1.Z), (a_1.X * b_1.Y) - (a_1.Y * b_1.X))))), (b = z, ((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z)))) < 0;
    }
    else {
        return false;
    }
}

/**
 * Returns if the Matrix is mirroring or reflection.
 * It might also be rotating, translating or scaling, but not projecting.
 * It must be affine and the determinate of the 3x3 part must be negative.
 * Same as m.IsMirroring.
 */
export function Matrix__get_IsReflecting(m) {
    return Matrix__get_IsMirroring(m);
}

/**
 * Returns if the Matrix is scaling.
 * It might also be rotating, translating or reflecting, but not projecting.
 * It must be affine and at least one of the 3 column vectors must have a squared length other than 1.0.
 */
export function Matrix__get_IsScaling(m) {
    let x_1, v, x_3, v_1, x_5, v_2;
    if (Matrix__get_IsAffine(m)) {
        if (!((x_1 = ((v = Vec_$ctor_Z7AD9E565(m.M11, m.M12, m.M13), ((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z))), (0.999999 < x_1) && (x_1 < 1.000001))) ? true : !((x_3 = ((v_1 = Vec_$ctor_Z7AD9E565(m.M21, m.M22, m.M23), ((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z))), (0.999999 < x_3) && (x_3 < 1.000001)))) {
            return true;
        }
        else {
            return !((x_5 = ((v_2 = Vec_$ctor_Z7AD9E565(m.M31, m.M32, m.M33), ((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z))), (0.999999 < x_5) && (x_5 < 1.000001)));
        }
    }
    else {
        return false;
    }
}

/**
 * Returns true if the Matrix is translating.
 * It might also be rotating, scaling and reflecting, but not projecting.
 */
export function Matrix__get_IsTranslating(m) {
    let x_1, x_3, x_5;
    if (Matrix__get_IsAffine(m)) {
        if (!((x_1 = m.X41, (-1E-06 < x_1) && (x_1 < 1E-06))) ? true : !((x_3 = m.Y42, (-1E-06 < x_3) && (x_3 < 1E-06)))) {
            return true;
        }
        else {
            return !((x_5 = m.Z43, (-1E-06 < x_5) && (x_5 < 1E-06)));
        }
    }
    else {
        return false;
    }
}

/**
 * Returns true if the Matrix is only translating.
 * It might not be rotating, scaling, reflecting, or projecting.
 */
export function Matrix__get_IsOnlyTranslating(m) {
    let x, x_1, x_2, x_3, x_4, x_5, x_6, x_7, x_8, x_9, x_10, x_11;
    if (((((((((((((x = m.M11, (0.999999 < x) && (x < 1.000001))) && ((x_1 = m.M21, (-1E-06 < x_1) && (x_1 < 1E-06)))) && ((x_2 = m.M31, (-1E-06 < x_2) && (x_2 < 1E-06)))) && ((x_3 = m.M12, (-1E-06 < x_3) && (x_3 < 1E-06)))) && ((x_4 = m.M22, (0.999999 < x_4) && (x_4 < 1.000001)))) && ((x_5 = m.M32, (-1E-06 < x_5) && (x_5 < 1E-06)))) && ((x_6 = m.M13, (-1E-06 < x_6) && (x_6 < 1E-06)))) && ((x_7 = m.M23, (-1E-06 < x_7) && (x_7 < 1E-06)))) && ((x_8 = m.M33, (0.999999 < x_8) && (x_8 < 1.000001)))) && ((x_9 = m.M14, (-1E-06 < x_9) && (x_9 < 1E-06)))) && ((x_10 = m.M24, (-1E-06 < x_10) && (x_10 < 1E-06)))) && ((x_11 = m.M34, (-1E-06 < x_11) && (x_11 < 1E-06)))) {
        const x_12 = m.M44;
        if (0.999999 < x_12) {
            return x_12 < 1.000001;
        }
        else {
            return false;
        }
    }
    else {
        return false;
    }
}

/**
 * Checks if two matrices are equal within tolerance.
 * By comparing the fields M11 to M44 each with the given tolerance.
 * Use a tolerance of 0.0 to check for an exact match.
 */
export function Matrix_equals(tol, a, b) {
    if (((((((((((((((Math.abs(a.M11 - b.M11) <= tol) && (Math.abs(a.M12 - b.M12) <= tol)) && (Math.abs(a.M13 - b.M13) <= tol)) && (Math.abs(a.M14 - b.M14) <= tol)) && (Math.abs(a.M21 - b.M21) <= tol)) && (Math.abs(a.M22 - b.M22) <= tol)) && (Math.abs(a.M23 - b.M23) <= tol)) && (Math.abs(a.M24 - b.M24) <= tol)) && (Math.abs(a.M31 - b.M31) <= tol)) && (Math.abs(a.M32 - b.M32) <= tol)) && (Math.abs(a.M33 - b.M33) <= tol)) && (Math.abs(a.M34 - b.M34) <= tol)) && (Math.abs(a.X41 - b.X41) <= tol)) && (Math.abs(a.Y42 - b.Y42) <= tol)) && (Math.abs(a.Z43 - b.Z43) <= tol)) {
        return Math.abs(a.M44 - b.M44) <= tol;
    }
    else {
        return false;
    }
}

/**
 * Multiplies matrixA with matrixB.
 * The resulting transformation will first do matrixA and then matrixB.
 */
export function Matrix_multiply_Z11D053C0(matrixA, matrixB) {
    const a11 = matrixA.M11;
    const a12 = matrixA.M12;
    const a13 = matrixA.M13;
    const a14 = matrixA.M14;
    const a21 = matrixA.M21;
    const a22 = matrixA.M22;
    const a23 = matrixA.M23;
    const a24 = matrixA.M24;
    const a31 = matrixA.M31;
    const a32 = matrixA.M32;
    const a33 = matrixA.M33;
    const a34 = matrixA.M34;
    const a41 = matrixA.X41;
    const a42 = matrixA.Y42;
    const a43 = matrixA.Z43;
    const a44 = matrixA.M44;
    const b11 = matrixB.M11;
    const b12 = matrixB.M12;
    const b13 = matrixB.M13;
    const b14 = matrixB.M14;
    const b21 = matrixB.M21;
    const b22 = matrixB.M22;
    const b23 = matrixB.M23;
    const b24 = matrixB.M24;
    const b31 = matrixB.M31;
    const b32 = matrixB.M32;
    const b33 = matrixB.M33;
    const b34 = matrixB.M34;
    const b41 = matrixB.X41;
    const b42 = matrixB.Y42;
    const b43 = matrixB.Z43;
    const b44 = matrixB.M44;
    return Matrix_$ctor_Z61E40B00((((a11 * b11) + (a12 * b21)) + (a13 * b31)) + (a14 * b41), (((a21 * b11) + (a22 * b21)) + (a23 * b31)) + (a24 * b41), (((a31 * b11) + (a32 * b21)) + (a33 * b31)) + (a34 * b41), (((a41 * b11) + (a42 * b21)) + (a43 * b31)) + (a44 * b41), (((a11 * b12) + (a12 * b22)) + (a13 * b32)) + (a14 * b42), (((a21 * b12) + (a22 * b22)) + (a23 * b32)) + (a24 * b42), (((a31 * b12) + (a32 * b22)) + (a33 * b32)) + (a34 * b42), (((a41 * b12) + (a42 * b22)) + (a43 * b32)) + (a44 * b42), (((a11 * b13) + (a12 * b23)) + (a13 * b33)) + (a14 * b43), (((a21 * b13) + (a22 * b23)) + (a23 * b33)) + (a24 * b43), (((a31 * b13) + (a32 * b23)) + (a33 * b33)) + (a34 * b43), (((a41 * b13) + (a42 * b23)) + (a43 * b33)) + (a44 * b43), (((a11 * b14) + (a12 * b24)) + (a13 * b34)) + (a14 * b44), (((a21 * b14) + (a22 * b24)) + (a23 * b34)) + (a24 * b44), (((a31 * b14) + (a32 * b24)) + (a33 * b34)) + (a34 * b44), (((a41 * b14) + (a42 * b24)) + (a43 * b34)) + (a44 * b44));
}

/**
 * Transposes the Matrix by swapping rows and columns.
 * For transformation matrices, this swaps the effect of row-major and column-major conventions.
 * For orthogonal rotation matrices, the transpose equals the inverse.
 */
export function Matrix_transpose_3CAE9522(m) {
    return Matrix_$ctor_Z61E40B00(m.M11, m.M12, m.M13, m.M14, m.M21, m.M22, m.M23, m.M24, m.M31, m.M32, m.M33, m.M34, m.X41, m.Y42, m.Z43, m.M44);
}

/**
 * Returns the identity matrix:
 * <code>
 * 1  0  0  0
 * 0  1  0  0
 * 0  0  1  0
 * 0  0  0  1
 * </code>
 */
export function Matrix_get_identity() {
    return Matrix_$ctor_Z61E40B00(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1);
}

/**
 * Creates a translation matrix.
 * The resulting matrix will be:
 * <code>
 * 1  0  0  x
 * 0  1  0  y
 * 0  0  1  z
 * 0  0  0  1
 * </code>
 */
export function Matrix_createTranslation_Z7AD9E565(x, y, z) {
    return Matrix_$ctor_Z61E40B00(1, 0, 0, x, 0, 1, 0, y, 0, 0, 1, z, 0, 0, 0, 1);
}

/**
 * Creates a translation matrix.
 * The resulting matrix will be:
 * <code>
 * 1  0  0  v.X
 * 0  1  0  v.Y
 * 0  0  1  v.Z
 * 0  0  0  1
 * </code>
 */
export function Matrix_createTranslation_Z394EC5F7(v) {
    return Matrix_$ctor_Z61E40B00(1, 0, 0, v.X, 0, 1, 0, v.Y, 0, 0, 1, v.Z, 0, 0, 0, 1);
}

/**
 * Creates a rotation transformation matrix around the X-axis
 * by angle in Degrees (not Radians).
 * A positive rotation will be from Y towards Z-axis,
 * so counter-clockwise the X-axis vector is pointing towards the observer.
 * The resulting matrix will be:
 * <code>
 * 1 0      0        0
 * 0 cos(θ) -sin(θ)  0
 * 0 sin(θ) cos(θ)   0
 * 0 0      0        1
 * </code>
 */
export function Matrix_createRotationX_5E38073B(angleDegrees) {
    const angle = 0.017453292519943295 * angleDegrees;
    const c = Math.cos(angle);
    const s = Math.sin(angle);
    return Matrix_$ctor_Z61E40B00(1, 0, 0, 0, 0, c, -s, 0, 0, s, c, 0, 0, 0, 0, 1);
}

/**
 * Creates a rotation transformation matrix around the Y-axis
 * by angle in Degrees (not Radians).
 * A positive rotation will be from Z towards X-axis,
 * so counter-clockwise the Y-axis vector is pointing towards the observer.
 * The resulting matrix will be:
 * <code>
 * cos(θ)  0 sin(θ) 0
 * 0       1 0      0
 * -sin(θ) 0 cos(θ) 0
 * 0       0 0      1
 * </code>
 */
export function Matrix_createRotationY_5E38073B(angleDegrees) {
    const angle = 0.017453292519943295 * angleDegrees;
    const c = Math.cos(angle);
    const s = Math.sin(angle);
    return Matrix_$ctor_Z61E40B00(c, 0, s, 0, 0, 1, 0, 0, -s, 0, c, 0, 0, 0, 0, 1);
}

/**
 * Creates a rotation transformation matrix around the Z-axis
 * by angle in Degrees (not Radians).
 * A positive rotation will be from X toward Y-axis,
 * so counter-clockwise the Z-axis vector is pointing towards the observer.
 * The resulting matrix will be:
 * <code>
 * cos(θ) -sin(θ) 0 0
 * sin(θ) cos(θ)  0 0
 * 0      0       1 0
 * 0      0       0 1
 * </code>
 */
export function Matrix_createRotationZ_5E38073B(angleDegrees) {
    const angle = 0.017453292519943295 * angleDegrees;
    const c = Math.cos(angle);
    const s = Math.sin(angle);
    return Matrix_$ctor_Z61E40B00(c, -s, 0, 0, s, c, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1);
}

/**
 * Creates a rotation around an axis transformation matrix.
 * A positive angle rotates counter-clockwise when the axis vector is pointing towards the observer (right-hand rule).
 */
export function Matrix_createRotationAxis_73D64DB4(axis, angleDegrees) {
    const angle = 0.017453292519943295 * angleDegrees;
    const c = Math.cos(angle);
    const s = Math.sin(angle);
    const t = 1 - c;
    const x = axis.X;
    const y = axis.Y;
    const z = axis.Z;
    const tx = t * x;
    const ty = t * y;
    return Matrix_$ctor_Z61E40B00((tx * x) + c, (tx * y) - (s * z), (tx * z) + (s * y), 0, (tx * y) + (s * z), (ty * y) + c, (ty * z) - (s * x), 0, (tx * z) - (s * y), (ty * z) + (s * x), ((t * z) * z) + c, 0, 0, 0, 0, 1);
}

/**
 * Creates a rotation around an axis transformation matrix.
 * A positive angle rotates counter-clockwise when the axis vector is pointing towards the observer (right-hand rule).
 */
export function Matrix_createRotationAxis_Z3D1F83EE(axis, angleDegrees) {
    const len = Math.sqrt(((axis.X * axis.X) + (axis.Y * axis.Y)) + (axis.Z * axis.Z));
    if (!(len > 1E-12)) {
        fail(`Matrix.createRotationAxis failed on too short axis: ${axis} and rotation: ${angleDegrees}° Degrees.`);
    }
    const sc = 1 / len;
    const x_1 = axis.X * sc;
    const y = axis.Y * sc;
    const z = axis.Z * sc;
    const angle = 0.017453292519943295 * angleDegrees;
    const c = Math.cos(angle);
    const s = Math.sin(angle);
    const t = 1 - c;
    const tx = t * x_1;
    const ty = t * y;
    return Matrix_$ctor_Z61E40B00((tx * x_1) + c, (tx * y) - (s * z), (tx * z) + (s * y), 0, (tx * y) + (s * z), (ty * y) + c, (ty * z) - (s * x_1), 0, (tx * z) - (s * y), (ty * z) + (s * x_1), ((t * z) * z) + c, 0, 0, 0, 0, 1);
}

/**
 * Creates a rotation matrix around an axis at a given center point.
 * A positive angle rotates counter-clockwise when the axis vector is pointing towards the observer (right-hand rule).
 */
export function Matrix_createRotationAxisCenter_Z655651F(axis, cen, angleDegrees) {
    return Matrix_multiply_Z11D053C0(Matrix_createTranslation_Z7AD9E565(-cen.X, -cen.Y, -cen.Z), Matrix_multiply_Z11D053C0(Matrix_createRotationAxis_Z3D1F83EE(axis, angleDegrees), Matrix_createTranslation_Z7AD9E565(cen.X, cen.Y, cen.Z)));
}

/**
 * Creates a rotation matrix around an axis at a given center point.
 * A positive angle rotates counter-clockwise when the axis vector is pointing towards the observer (right-hand rule).
 */
export function Matrix_createRotationAxisCenter_976E587(axis, cen, angleDegrees) {
    return Matrix_multiply_Z11D053C0(Matrix_createTranslation_Z7AD9E565(-cen.X, -cen.Y, -cen.Z), Matrix_multiply_Z11D053C0(Matrix_createRotationAxis_73D64DB4(axis, angleDegrees), Matrix_createTranslation_Z7AD9E565(cen.X, cen.Y, cen.Z)));
}

/**
 * Creates a scale transformation matrix.
 * The resulting matrix will be:
 * <code>
 * x, 0, 0, 0,
 * 0, y, 0, 0,
 * 0, 0, z, 0,
 * 0, 0, 0, 1
 * </code>
 */
export function Matrix_createScale_Z7AD9E565(x, y, z) {
    return Matrix_$ctor_Z61E40B00(x, 0, 0, 0, 0, y, 0, 0, 0, 0, z, 0, 0, 0, 0, 1);
}

/**
 * Creates a rotation from one unit-vectors direction to another unit-vectors direction.
 * If the tips of the two unitized vectors have a distance less than 1e-12 the identity matrix is returned.
 * If the tips of the two vectors are almost exactly opposite,
 * that is if tips of the two vectors are less than 1e-12 apart when summed,
 * there is no valid unique 180 degree rotation that can be found, so an exception is raised.
 */
export function Matrix_createVecToVec_6319FE20(vecFrom, vecTo) {
    let v, a, b, v_2, a_1, b_1;
    if (!(((v = ((a = vecFrom, (b = vecTo, Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z)))), ((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z))) > 1E-24)) {
        return Matrix_get_identity();
    }
    else {
        if (!(((v_2 = ((a_1 = vecFrom, (b_1 = vecTo, Vec_$ctor_Z7AD9E565(a_1.X + b_1.X, a_1.Y + b_1.Y, a_1.Z + b_1.Z)))), ((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z))) > 1E-24)) {
            fail(`Matrix.createVecToVec failed to find a rotation axis for (almost) colinear unit-vectors in opposite directions: ${vecFrom} and ${vecTo}`);
        }
        let axis0;
        const a_2 = vecFrom;
        const b_2 = vecTo;
        axis0 = Vec_$ctor_Z7AD9E565((a_2.Y * b_2.Z) - (a_2.Z * b_2.Y), (a_2.Z * b_2.X) - (a_2.X * b_2.Z), (a_2.X * b_2.Y) - (a_2.Y * b_2.X));
        let len;
        const v_3 = axis0;
        len = Math.sqrt(((v_3.X * v_3.X) + (v_3.Y * v_3.Y)) + (v_3.Z * v_3.Z));
        let axis;
        const v_4 = axis0;
        const f = len;
        if (!(Math.abs(f) > 1E-12)) {
            failDivide("\'/\' operator", f, v_4);
        }
        axis = Vec_$ctor_Z7AD9E565(v_4.X / f, v_4.Y / f, v_4.Z / f);
        const x_3 = axis.X;
        const y = axis.Y;
        const z = axis.Z;
        let c;
        const a_3 = vecFrom;
        const b_3 = vecTo;
        c = (((a_3.X * b_3.X) + (a_3.Y * b_3.Y)) + (a_3.Z * b_3.Z));
        const s = len;
        const t = 1 - c;
        const tx = t * x_3;
        const ty = t * y;
        return Matrix_$ctor_Z61E40B00((tx * x_3) + c, (tx * y) - (s * z), (tx * z) + (s * y), 0, (tx * y) + (s * z), (ty * y) + c, (ty * z) - (s * x_3), 0, (tx * z) - (s * y), (ty * z) + (s * x_3), ((t * z) * z) + c, 0, 0, 0, 0, 1);
    }
}

/**
 * Creates a rotation from one vectors direction to another vectors direction.
 * Does NOT do any scaling.
 * Ignores the length of the input vectors and unitizes them first.
 * If the tips of the two unitized vectors have a distance less than 1e-12 the identity matrix is returned.
 * If the tips of the two vectors are almost exactly opposite,
 * that is if tips of the two vectors are less than 1e-12 apart when summed,
 * there is no valid unique 180 degree rotation that can be found, so an exception is raised.
 * Fails if either vector is too short (length less than 1e-6).
 */
export function Matrix_createVecToVec_5A694120(vecFrom, vecTo) {
    let v, a, b, v_2, a_1, b_1;
    let fu;
    const x = vecFrom.X;
    const y = vecFrom.Y;
    const z = vecFrom.Z;
    const length = Math.sqrt(((x * x) + (y * y)) + (z * z));
    if (!(length > 1E-06)) {
        fail(`Matrix.createVecToVec failed. The vector is too short: vecFrom: ${vecFrom}`);
    }
    const sc = 1 / length;
    fu = UnitVec_$ctor_Z7AD9E565(x * sc, y * sc, z * sc);
    let tu;
    const x_3 = vecTo.X;
    const y_2 = vecTo.Y;
    const z_2 = vecTo.Z;
    const length_1 = Math.sqrt(((x_3 * x_3) + (y_2 * y_2)) + (z_2 * z_2));
    if (!(length_1 > 1E-06)) {
        fail(`Matrix.createVecToVec failed. The vector is too short: vecTo: ${vecTo}`);
    }
    const sc_1 = 1 / length_1;
    tu = UnitVec_$ctor_Z7AD9E565(x_3 * sc_1, y_2 * sc_1, z_2 * sc_1);
    if (!(((v = ((a = fu, (b = tu, Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z)))), ((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z))) > 1E-24)) {
        return Matrix_get_identity();
    }
    else {
        if (!(((v_2 = ((a_1 = fu, (b_1 = tu, Vec_$ctor_Z7AD9E565(a_1.X + b_1.X, a_1.Y + b_1.Y, a_1.Z + b_1.Z)))), ((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z))) > 1E-24)) {
            fail(`Matrix.createVecToVec failed to find a rotation axis for (almost) colinear (or NaN) vectors in opposite directions: ${vecFrom} and ${vecTo}`);
        }
        let axis0;
        const a_2 = fu;
        const b_2 = tu;
        axis0 = Vec_$ctor_Z7AD9E565((a_2.Y * b_2.Z) - (a_2.Z * b_2.Y), (a_2.Z * b_2.X) - (a_2.X * b_2.Z), (a_2.X * b_2.Y) - (a_2.Y * b_2.X));
        let len;
        const v_3 = axis0;
        len = Math.sqrt(((v_3.X * v_3.X) + (v_3.Y * v_3.Y)) + (v_3.Z * v_3.Z));
        let axis;
        const v_4 = axis0;
        const f = len;
        if (!(Math.abs(f) > 1E-12)) {
            failDivide("\'/\' operator", f, v_4);
        }
        axis = Vec_$ctor_Z7AD9E565(v_4.X / f, v_4.Y / f, v_4.Z / f);
        const x_9 = axis.X;
        const y_4 = axis.Y;
        const z_4 = axis.Z;
        let c;
        const a_3 = fu;
        const b_3 = tu;
        c = (((a_3.X * b_3.X) + (a_3.Y * b_3.Y)) + (a_3.Z * b_3.Z));
        const s = len;
        const t = 1 - c;
        const tx = t * x_9;
        const ty = t * y_4;
        return Matrix_$ctor_Z61E40B00((tx * x_9) + c, (tx * y_4) - (s * z_4), (tx * z_4) + (s * y_4), 0, (tx * y_4) + (s * z_4), (ty * y_4) + c, (ty * z_4) - (s * x_9), 0, (tx * z_4) - (s * y_4), (ty * z_4) + (s * x_9), ((t * z_4) * z_4) + c, 0, 0, 0, 0, 1);
    }
}

/**
 * Creates a shear transformation matrix.
 * The resulting matrix will be:
 * <code>
 * 1,   yx,  zx,  0,
 * xy,   1,  zy,  0,
 * xz,  yz,   1,  0,
 * 0,    0,   0,  1
 * </code>
 */
export function Matrix_createShear_76A78260(xy, xz, yx, yz, zx, zy) {
    return Matrix_$ctor_Z61E40B00(1, yx, zx, 0, xy, 1, zy, 0, xz, yz, 1, 0, 0, 0, 0, 1);
}

/**
 * Creates a Matrix to transform from World plane or Coordinate System to given Plane.
 * Also called Change of Basis.
 */
export function Matrix_createToPlane_2F29039F(p) {
    return Matrix_$ctor_Z61E40B00(p.Xaxis.X, p.Yaxis.X, p.Zaxis.X, p.Origin.X, p.Xaxis.Y, p.Yaxis.Y, p.Zaxis.Y, p.Origin.Y, p.Xaxis.Z, p.Yaxis.Z, p.Zaxis.Z, p.Origin.Z, 0, 0, 0, 1);
}

/**
 * Creates a Matrix to transform from one Plane or Coordinate System to another Plane.
 */
export function Matrix_createPlaneToPlane_3B6074E0(fromPlane, toPlane) {
    return Matrix_multiply_Z11D053C0(Matrix__get_Inverse(Matrix_createToPlane_2F29039F(fromPlane)), Matrix_createToPlane_2F29039F(toPlane));
}

/**
 * Creates a Matrix to mirror on a Plane.
 */
export function Matrix_createMirror_2F29039F(p) {
    const toPlane = Matrix_createToPlane_2F29039F(p);
    return Matrix_multiply_Z11D053C0(Matrix__get_Inverse(toPlane), Matrix_multiply_Z11D053C0(Matrix_createScale_Z7AD9E565(1, 1, -1), toPlane));
}

/**
 * Create Matrix from Quaternion.
 */
export function Matrix_createFromQuaternion_Z2A007687(quaternion) {
    const x = quaternion.X;
    const y = quaternion.Y;
    const z = quaternion.Z;
    const w = quaternion.W;
    const x2 = x + x;
    const y2 = y + y;
    const z2 = z + z;
    const xx = x * x2;
    const xy = x * y2;
    const xz = x * z2;
    const yy = y * y2;
    const yz = y * z2;
    const zz = z * z2;
    const wx = w * x2;
    const wy = w * y2;
    const wz = w * z2;
    return Matrix_$ctor_Z61E40B00(1 - (yy + zz), xy - wz, xz + wy, 0, xy + wz, 1 - (xx + zz), yz - wx, 0, xz - wy, yz + wx, 1 - (xx + yy), 0, 0, 0, 0, 1);
}

/**
 * Creates a matrix from array of 16 elements in Column Major order:
 * <code>[| M11 M12 M13 M14 M21 M22 M23 M24 M31 M32 M33 M34 X41 Y42 Z43 M44 |]</code>
 * Where X41, Y42 and Z43 refer to the translation part of the matrix.
 */
export function Matrix_createFromColumMajorArray_52AF8430(xs) {
    if (xs.length !== 16) {
        fail(`Matrix.createFromColumMajorArray expects an array of 16 items but got ${xs.length}`);
    }
    return Matrix_$ctor_Z61E40B00(item(0, xs), item(4, xs), item(8, xs), item(12, xs), item(1, xs), item(5, xs), item(9, xs), item(13, xs), item(2, xs), item(6, xs), item(10, xs), item(14, xs), item(3, xs), item(7, xs), item(11, xs), item(15, xs));
}

/**
 * Creates a matrix from array of 16 elements in Row Major order:
 * <code>[| M11 M21 M31 X41 M12 M22 M32 Y42 M13 M23 M33 Z43 M14 M24 M34 M44 |]</code>
 * Where X41, Y42 and Z43 refer to the translation part of the matrix.
 */
export function Matrix_createFromRowMajorArray_52AF8430(xs) {
    if (xs.length !== 16) {
        fail(`Matrix.createFromRowMajorArray expects an array of 16 items but got ${xs.length}`);
    }
    return Matrix_$ctor_Z61E40B00(item(0, xs), item(1, xs), item(2, xs), item(3, xs), item(4, xs), item(5, xs), item(6, xs), item(7, xs), item(8, xs), item(9, xs), item(10, xs), item(11, xs), item(12, xs), item(13, xs), item(14, xs), item(15, xs));
}

/**
 * Add a vector translation to an existing matrix.
 */
export function Matrix_addTranslation(v, m) {
    return Matrix_$ctor_Z61E40B00(m.M11, m.M21, m.M31, m.X41 + v.X, m.M12, m.M22, m.M32, m.Y42 + v.Y, m.M13, m.M23, m.M33, m.Z43 + v.Z, m.M14, m.M24, m.M34, m.M44);
}

/**
 * Add a X, Y and Z translation to an existing matrix.
 */
export function Matrix_addTranslationXYZ(x, y, z, m) {
    return Matrix_$ctor_Z61E40B00(m.M11, m.M21, m.M31, m.X41 + x, m.M12, m.M22, m.M32, m.Y42 + y, m.M13, m.M23, m.M33, m.Z43 + z, m.M14, m.M24, m.M34, m.M44);
}

/**
 * Creates a perspective projection matrix from the given view volume dimensions.
 */
export function Matrix_createPerspective_77D16AC0(width, height, nearPlaneDistance, farPlaneDistance) {
    if (nearPlaneDistance <= 0) {
        fail(`Matrix.createPerspective: nearPlaneDistance must be greater than 0.0 but got ${nearPlaneDistance}`);
    }
    if (farPlaneDistance <= 0) {
        fail(`Matrix.createPerspective: farPlaneDistance must be greater than 0.0 but got ${farPlaneDistance}`);
    }
    if (nearPlaneDistance >= farPlaneDistance) {
        fail(`Matrix.createPerspective: nearPlaneDistance (${nearPlaneDistance}) must be less than farPlaneDistance (${farPlaneDistance})`);
    }
    const negFarRange = isPositiveInfinity(farPlaneDistance) ? -1 : (farPlaneDistance / (nearPlaneDistance - farPlaneDistance));
    return Matrix_$ctor_Z61E40B00((2 * nearPlaneDistance) / width, 0, 0, 0, 0, (2 * nearPlaneDistance) / height, 0, 0, 0, 0, negFarRange, nearPlaneDistance * negFarRange, 0, 0, -1, 0);
}

