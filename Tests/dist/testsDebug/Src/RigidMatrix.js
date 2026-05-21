
import { map2, chunkBySize, maxBy, map } from "../fable_modules/fable-library-js.5.0.0/Array.js";
import { join, concat, printf, toText } from "../fable_modules/fable-library-js.5.0.0/String.js";
import { comparePrimitives } from "../fable_modules/fable-library-js.5.0.0/Util.js";
import { nl } from "./Format.js";
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { Vec_$ctor_Z7AD9E565 } from "./Vec.js";
import { Matrix__get_IsProjecting, Matrix__get_Determinant, Matrix_$ctor_Z61E40B00 } from "./Matrix.js";
import { failDivide, fail } from "./EuclidErrors.js";
import { UnitVec_$ctor_Z7AD9E565 } from "./UnitVec.js";

/**
 * A struct containing 12 floats, representing an immutable 4x3 rigid transformation matrix.
 * This matrix guarantees to NOT scale, shear, flip, mirror, reflect or project.
 * Angles are preserved. Lengths are preserved. Area is preserved. Volume is preserved.
 * Transformations with 4x3 RigidMatrices are faster than with the more general 4x4 Matrices.
 * A rigid matrix is made from a rotational 3x3 part whose columns and rows are orthogonal 3D unit-vectors.
 * The last 1x3 column is the translation part of the matrix.
 * It can be thought of as a 4x4 Orthonormal matrix where the last row would always be 0, 0, 0, 1.
 * Thus it is not stored in the RigidMatrix 4x3 struct.
 * The matrix is represented in the following column-vector syntax form:
 * <code>
 * M11 M21 M31 X41
 * M12 M22 M32 Y42
 * M13 M23 M33 Z43
 * </code>
 * Where X41, Y42 and Z43 refer to the translation part of the RigidMatrix.
 * The Determinant of this matrix is always 1.0.
 * Note: Never use the struct default constructor RigidMatrix() as it will create an invalid zero RigidMatrix.
 * Use RigidMatrix.create instead.
 */
export class RigidMatrix extends Record {
    constructor(M11, M21, M31, X41, M12, M22, M32, Y42, M13, M23, M33, Z43) {
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
    }
    /**
     * Nicely formats the Matrix to a Grid of 4x3 including field names.
     * Using the following column-vector syntax form:
     * <code>
     * M11 M21 M31 X41
     * M12 M22 M32 Y42
     * M13 M23 M33 Z43
     * </code>
     * Where X41, Y42 and Z43 refer to the translation part of the matrix.
     */
    toString() {
        let clo;
        const m = this;
        const names = ["M11", "M21", "M31", "X41", "M12", "M22", "M32", "Y42", "M13", "M23", "M33", "Z43"];
        let ts;
        const array = RigidMatrix__get_ToArrayByRows(m);
        ts = map((clo = toText(printf("%0.3f")), clo), array);
        const most = maxBy((s) => (s.length | 0), ts, {
            Compare: (x, y) => (comparePrimitives(x, y) | 0),
        });
        return concat("Column-Vector Rigid Transformation Matrix:", ...nl) + join("\n", map((es) => (" " + join(" | ", es)), chunkBySize(4, map2((n, v) => (((n + ": ") + (Array((most.length - v.length) + 1).join(" "))) + v), names, ts))));
    }
}

export function RigidMatrix_$reflection() {
    return class_type("Euclid.RigidMatrix", undefined, RigidMatrix, class_type("System.ValueType"));
}

/**
 * Creates an immutable 4x3 Transformation Matrix. For only rotation and translation in 3D space.
 * This Constructor takes arguments in row-major order.
 * The matrix is represented in the following column-vector syntax form:
 * <code>
 * M11 M21 M31 X41
 * M12 M22 M32 Y42
 * M13 M23 M33 Z43
 * </code>
 * Where X41, Y42 and Z43 refer to the translation part of the RigidMatrix.
 */
export function RigidMatrix_$ctor_Z15A9A3C0(m11, m21, m31, x41, m12, m22, m32, y42, m13, m23, m33, z43) {
    return new RigidMatrix(m11, m21, m31, x41, m12, m22, m32, y42, m13, m23, m33, z43);
}

/**
 * Returns the 12 elements column-major order:
 * <code>[| M11 M12 M13 M21 M22 M23 M31 M32 M33 X41 Y42 Z43 |]</code>
 * Where X41, Y42 and Z43 refer to the translation part of the RigidMatrix.
 */
export function RigidMatrix__get_ToArrayByColumns(m) {
    return new Float64Array([m.M11, m.M12, m.M13, m.M21, m.M22, m.M23, m.M31, m.M32, m.M33, m.X41, m.Y42, m.Z43]);
}

/**
 * Returns the 12 elements in row-major order:
 * <code>[| M11 M21 M31 X41 M12 M22 M32 Y42 M13 M23 M33 Z43 |]</code>
 * Where X41, Y42 and Z43 refer to the translation part of the RigidMatrix.
 */
export function RigidMatrix__get_ToArrayByRows(m) {
    return new Float64Array([m.M11, m.M21, m.M31, m.X41, m.M12, m.M22, m.M32, m.Y42, m.M13, m.M23, m.M33, m.Z43]);
}

/**
 * Nicely formats the Matrix to a Grid of 4x3.
 */
export function RigidMatrix__get_AsString(m) {
    let clo;
    let ts;
    const array = RigidMatrix__get_ToArrayByRows(m);
    ts = map((clo = toText(printf("%0.3f")), clo), array);
    const most = maxBy((s) => (s.length | 0), ts, {
        Compare: (x, y) => (comparePrimitives(x, y) | 0),
    });
    return concat("4x3 Column-Vector Rigid Transformation Matrix:", ...nl) + join("\n", map((es) => (" " + join(" | ", es)), chunkBySize(4, map((x_1) => ((Array((most.length - x_1.length) + 1).join(" ")) + x_1), ts))));
}

/**
 * Formats a RigidMatrix into an F# code string that can be used to recreate the matrix.
 */
export function RigidMatrix__get_AsFSharpCode(m) {
    return `RigidMatrix.create(${m.M11}, ${m.M21}, ${m.M31}, ${m.X41}, ${m.M12}, ${m.M22}, ${m.M32}, ${m.Y42}, ${m.M13}, ${m.M23}, ${m.M33}, ${m.Z43})`;
}

/**
 * Returns the first column vector. M11, M12 and M13.
 */
export function RigidMatrix__get_ColumnVector1(m) {
    return Vec_$ctor_Z7AD9E565(m.M11, m.M12, m.M13);
}

/**
 * Returns the second column vector. M21, M22 and M23.
 */
export function RigidMatrix__get_ColumnVector2(m) {
    return Vec_$ctor_Z7AD9E565(m.M21, m.M22, m.M23);
}

/**
 * Returns the third column vector. M31, M32 and M33.
 */
export function RigidMatrix__get_ColumnVector3(m) {
    return Vec_$ctor_Z7AD9E565(m.M31, m.M32, m.M33);
}

/**
 * Returns the translation or fourth column vector. X41, Y42 and Z43.
 */
export function RigidMatrix__get_Translation(m) {
    return Vec_$ctor_Z7AD9E565(m.X41, m.Y42, m.Z43);
}

/**
 * Converts the 3x4 RigidMatrix to a general 4x4 Matrix.
 */
export function RigidMatrix__get_Matrix(m) {
    return Matrix_$ctor_Z61E40B00(m.M11, m.M21, m.M31, m.X41, m.M12, m.M22, m.M32, m.Y42, m.M13, m.M23, m.M33, m.Z43, 0, 0, 0, 1);
}

/**
 * The Determinant of a Rigid Matrix is always 1.0.
 * The Determinant describes the volume that a unit cube will have after the matrix was applied.
 * This method only exists for testing.
 */
export function RigidMatrix__get_Determinant(m) {
    const m_1 = RigidMatrix__get_Matrix(m);
    return Matrix__get_Determinant(m_1);
}

/**
 * Inverts the RigidMatrix.
 * Rigid matrices always have determinant 1.0 so they can always be inverted.
 */
export function RigidMatrix__get_Inverse(m) {
    const n11 = m.M11;
    const n21 = m.M21;
    const n31 = m.M31;
    const x41 = m.X41;
    const n12 = m.M12;
    const n22 = m.M22;
    const n32 = m.M32;
    const y42 = m.Y42;
    const n13 = m.M13;
    const n23 = m.M23;
    const n33 = m.M33;
    const z43 = m.Z43;
    const n33y42 = n33 * y42;
    const n32z43 = n32 * z43;
    const n13n22 = n13 * n22;
    const n12n23 = n12 * n23;
    const n31z43 = n31 * z43;
    const n31y42 = n31 * y42;
    const n13n21 = n13 * n21;
    const n12n21 = n12 * n21;
    const n33x41 = n33 * x41;
    const n11n23 = n11 * n23;
    const n11n22 = n11 * n22;
    const n32x41 = n32 * x41;
    return RigidMatrix_$ctor_Z15A9A3C0((n22 * n33) - (n23 * n32), (n23 * n31) - (n21 * n33), (n21 * n32) - (n22 * n31), (((((n23 * n32x41) - (n22 * n33x41)) - (n23 * n31y42)) + (n21 * n33y42)) + (n22 * n31z43)) - (n21 * n32z43), (n13 * n32) - (n12 * n33), (n11 * n33) - (n13 * n31), (n12 * n31) - (n11 * n32), (((((n12 * n33x41) - (n13 * n32x41)) + (n13 * n31y42)) - (n11 * n33y42)) - (n12 * n31z43)) + (n11 * n32z43), n12n23 - n13n22, n13n21 - n11n23, n11n22 - n12n21, (((((n13n22 * x41) - (n12n23 * x41)) - (n13n21 * y42)) + (n11n23 * y42)) + (n12n21 * z43)) - (n11n22 * z43));
}

/**
 * Checks if the Matrix is an Identity Matrix in the form of:
 * <code>
 * 1  0  0  0
 * 0  1  0  0
 * 0  0  1  0
 * </code>
 * Using an approximate tolerance of 1e-6.
 */
export function RigidMatrix__get_IsIdentity(m) {
    let x, x_1, x_2, x_3, x_4, x_5, x_6, x_7, x_8, x_9, x_10;
    if ((((((((((((x = m.M11, (0.999999 < x) && (x < 1.000001))) && ((x_1 = m.M21, (-1E-06 < x_1) && (x_1 < 1E-06)))) && ((x_2 = m.M31, (-1E-06 < x_2) && (x_2 < 1E-06)))) && ((x_3 = m.X41, (-1E-06 < x_3) && (x_3 < 1E-06)))) && ((x_4 = m.M12, (-1E-06 < x_4) && (x_4 < 1E-06)))) && ((x_5 = m.M22, (0.999999 < x_5) && (x_5 < 1.000001)))) && ((x_6 = m.M32, (-1E-06 < x_6) && (x_6 < 1E-06)))) && ((x_7 = m.Y42, (-1E-06 < x_7) && (x_7 < 1E-06)))) && ((x_8 = m.M13, (-1E-06 < x_8) && (x_8 < 1E-06)))) && ((x_9 = m.M23, (-1E-06 < x_9) && (x_9 < 1E-06)))) && ((x_10 = m.M33, (0.999999 < x_10) && (x_10 < 1.000001)))) {
        const x_11 = m.Z43;
        if (-1E-06 < x_11) {
            return x_11 < 1E-06;
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
 * Checks if two Matrices are equal within tolerance.
 * By comparing the fields M11 to Z43 each with the given tolerance.
 * Use a tolerance of 0.0 to check for an exact match.
 */
export function RigidMatrix_equals(tol, a, b) {
    if (((((((((((Math.abs(a.M11 - b.M11) <= tol) && (Math.abs(a.M12 - b.M12) <= tol)) && (Math.abs(a.M13 - b.M13) <= tol)) && (Math.abs(a.M21 - b.M21) <= tol)) && (Math.abs(a.M22 - b.M22) <= tol)) && (Math.abs(a.M23 - b.M23) <= tol)) && (Math.abs(a.M31 - b.M31) <= tol)) && (Math.abs(a.M32 - b.M32) <= tol)) && (Math.abs(a.M33 - b.M33) <= tol)) && (Math.abs(a.X41 - b.X41) <= tol)) && (Math.abs(a.Y42 - b.Y42) <= tol)) {
        return Math.abs(a.Z43 - b.Z43) <= tol;
    }
    else {
        return false;
    }
}

/**
 * Multiplies matrixA with matrixB.
 * The resulting transformation will first do matrixA and then matrixB.
 */
export function RigidMatrix_multiply_Z31732520(matrixA, matrixB) {
    const a11 = matrixA.M11;
    const a12 = matrixA.M12;
    const a13 = matrixA.M13;
    const a21 = matrixA.M21;
    const a22 = matrixA.M22;
    const a23 = matrixA.M23;
    const a31 = matrixA.M31;
    const a32 = matrixA.M32;
    const a33 = matrixA.M33;
    const a41 = matrixA.X41;
    const a42 = matrixA.Y42;
    const a43 = matrixA.Z43;
    const b11 = matrixB.M11;
    const b12 = matrixB.M12;
    const b13 = matrixB.M13;
    const b21 = matrixB.M21;
    const b22 = matrixB.M22;
    const b23 = matrixB.M23;
    const b31 = matrixB.M31;
    const b32 = matrixB.M32;
    const b33 = matrixB.M33;
    return RigidMatrix_$ctor_Z15A9A3C0(((a11 * b11) + (a12 * b21)) + (a13 * b31), ((a21 * b11) + (a22 * b21)) + (a23 * b31), ((a31 * b11) + (a32 * b21)) + (a33 * b31), (((a41 * b11) + (a42 * b21)) + (a43 * b31)) + matrixB.X41, ((a11 * b12) + (a12 * b22)) + (a13 * b32), ((a21 * b12) + (a22 * b22)) + (a23 * b32), ((a31 * b12) + (a32 * b22)) + (a33 * b32), (((a41 * b12) + (a42 * b22)) + (a43 * b32)) + matrixB.Y42, ((a11 * b13) + (a12 * b23)) + (a13 * b33), ((a21 * b13) + (a22 * b23)) + (a23 * b33), ((a31 * b13) + (a32 * b23)) + (a33 * b33), (((a41 * b13) + (a42 * b23)) + (a43 * b33)) + matrixB.Z43);
}

/**
 * Returns the Identity RigidMatrix:
 * <code>
 * 1  0  0  0
 * 0  1  0  0
 * 0  0  1  0
 * </code>
 */
export function RigidMatrix_get_identity() {
    return RigidMatrix_$ctor_Z15A9A3C0(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0);
}

/**
 * Creates a translation RigidMatrix.
 * The resulting matrix will be:
 * <code>
 * 1  0  0  x
 * 0  1  0  y
 * 0  0  1  z
 * </code>
 */
export function RigidMatrix_createTranslation_Z7AD9E565(x, y, z) {
    return RigidMatrix_$ctor_Z15A9A3C0(1, 0, 0, x, 0, 1, 0, y, 0, 0, 1, z);
}

/**
 * Creates a translation RigidMatrix.
 * The resulting RigidMatrix will be:
 * <code>
 * 1  0  0  v.X
 * 0  1  0  v.Y
 * 0  0  1  v.Z
 * </code>
 */
export function RigidMatrix_createTranslation_Z394EC5F7(v) {
    return RigidMatrix_$ctor_Z15A9A3C0(1, 0, 0, v.X, 0, 1, 0, v.Y, 0, 0, 1, v.Z);
}

/**
 * Creates a translation RigidMatrix.
 * The resulting RigidMatrix will be:
 * <code>
 * 1  0  0  x
 * 0  1  0  0
 * 0  0  1  0
 * </code>
 */
export function RigidMatrix_createTranslationX_5E38073B(x) {
    return RigidMatrix_$ctor_Z15A9A3C0(1, 0, 0, x, 0, 1, 0, 0, 0, 0, 1, 0);
}

/**
 * Creates a translation RigidMatrix.
 * The resulting RigidMatrix will be:
 * <code>
 * 1  0  0  0
 * 0  1  0  y
 * 0  0  1  0
 * </code>
 */
export function RigidMatrix_createTranslationY_5E38073B(y) {
    return RigidMatrix_$ctor_Z15A9A3C0(1, 0, 0, 0, 0, 1, 0, y, 0, 0, 1, 0);
}

/**
 * Creates a translation RigidMatrix.
 * The resulting RigidMatrix will be:
 * <code>
 * 1  0  0  0
 * 0  1  0  0
 * 0  0  1  z
 * </code>
 */
export function RigidMatrix_createTranslationZ_5E38073B(z) {
    return RigidMatrix_$ctor_Z15A9A3C0(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, z);
}

/**
 * Creates a rotation around the X-axis RigidMatrix
 * by angle in Degrees (not Radians).
 * A positive rotation will be from Y towards Z-axis,
 * so counter-clockwise when the X-axis vector is pointing towards the observer. (right-hand rule)
 * The resulting RigidMatrix will be:
 * <code>
 * 1 0      0        0
 * 0 cos(θ) -sin(θ)  0
 * 0 sin(θ) cos(θ)   0
 * </code>
 */
export function RigidMatrix_createRotationX_5E38073B(angleDegrees) {
    const angle = 0.017453292519943295 * angleDegrees;
    const c = Math.cos(angle);
    const s = Math.sin(angle);
    return RigidMatrix_$ctor_Z15A9A3C0(1, 0, 0, 0, 0, c, -s, 0, 0, s, c, 0);
}

/**
 * Creates a rotation around the Y-axis RigidMatrix
 * by angle in Degrees (not Radians).
 * A positive rotation will be from Z towards X-axis,
 * so counter-clockwise when the Y-axis vector is pointing towards the observer. (right-hand rule)
 * The resulting RigidMatrix will be:
 * <code>
 * cos(θ)  0 sin(θ) 0
 * 0       1 0      0
 * -sin(θ) 0 cos(θ) 0
 * </code>
 */
export function RigidMatrix_createRotationY_5E38073B(angleDegrees) {
    const angle = 0.017453292519943295 * angleDegrees;
    const c = Math.cos(angle);
    const s = Math.sin(angle);
    return RigidMatrix_$ctor_Z15A9A3C0(c, 0, s, 0, 0, 1, 0, 0, -s, 0, c, 0);
}

/**
 * Creates a rotation around the Z-axis RigidMatrix
 * by angle in Degrees (not Radians).
 * A positive rotation will be from X towards Y-axis,
 * so counter-clockwise when the Z-axis vector is pointing towards the observer. (right-hand rule)
 * The resulting RigidMatrix will be:
 * <code>
 * cos(θ) -sin(θ) 0 0
 * sin(θ) cos(θ)  0 0
 * 0      0       1 0
 * </code>
 */
export function RigidMatrix_createRotationZ_5E38073B(angleDegrees) {
    const angle = 0.017453292519943295 * angleDegrees;
    const c = Math.cos(angle);
    const s = Math.sin(angle);
    return RigidMatrix_$ctor_Z15A9A3C0(c, -s, 0, 0, s, c, 0, 0, 0, 0, 1, 0);
}

/**
 * Creates a rotation around an Axis RigidMatrix.
 * A positive angle rotates counter-clockwise when the axis vector is pointing towards the observer (right-hand rule).
 */
export function RigidMatrix_createRotationAxis_73D64DB4(axis, angleDegrees) {
    const angle = 0.017453292519943295 * angleDegrees;
    const c = Math.cos(angle);
    const s = Math.sin(angle);
    const t = 1 - c;
    const x = axis.X;
    const y = axis.Y;
    const z = axis.Z;
    const tx = t * x;
    const ty = t * y;
    return RigidMatrix_$ctor_Z15A9A3C0((tx * x) + c, (tx * y) - (s * z), (tx * z) + (s * y), 0, (tx * y) + (s * z), (ty * y) + c, (ty * z) - (s * x), 0, (tx * z) - (s * y), (ty * z) + (s * x), ((t * z) * z) + c, 0);
}

/**
 * Creates a rotation around an Axis RigidMatrix.
 * A positive angle rotates counter-clockwise when the axis vector is pointing towards the observer (right-hand rule).
 */
export function RigidMatrix_createRotationAxis_Z3D1F83EE(axis, angleDegrees) {
    const len = Math.sqrt(((axis.X * axis.X) + (axis.Y * axis.Y)) + (axis.Z * axis.Z));
    if (!(len > 1E-12)) {
        fail(`RigidMatrix.createRotationAxis failed on too short axis: ${axis} and rotation: ${angleDegrees}° Degrees.`);
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
    return RigidMatrix_$ctor_Z15A9A3C0((tx * x_1) + c, (tx * y) - (s * z), (tx * z) + (s * y), 0, (tx * y) + (s * z), (ty * y) + c, (ty * z) - (s * x_1), 0, (tx * z) - (s * y), (ty * z) + (s * x_1), ((t * z) * z) + c, 0);
}

/**
 * Creates a rotation matrix around an Axis at a given center point.
 * A positive angle rotates counter-clockwise when the axis vector is pointing towards the observer (right-hand rule).
 */
export function RigidMatrix_createRotationAxisCenter_Z655651F(axis, cen, angleDegrees) {
    return RigidMatrix_multiply_Z31732520(RigidMatrix_createTranslation_Z7AD9E565(-cen.X, -cen.Y, -cen.Z), RigidMatrix_multiply_Z31732520(RigidMatrix_createRotationAxis_Z3D1F83EE(axis, angleDegrees), RigidMatrix_createTranslation_Z7AD9E565(cen.X, cen.Y, cen.Z)));
}

/**
 * Creates a rotation matrix around an Axis at a given center point.
 * A positive angle rotates counter-clockwise when the axis vector is pointing towards the observer (right-hand rule).
 */
export function RigidMatrix_createRotationAxisCenter_976E587(axis, cen, angleDegrees) {
    return RigidMatrix_multiply_Z31732520(RigidMatrix_createTranslation_Z7AD9E565(-cen.X, -cen.Y, -cen.Z), RigidMatrix_multiply_Z31732520(RigidMatrix_createRotationAxis_73D64DB4(axis, angleDegrees), RigidMatrix_createTranslation_Z7AD9E565(cen.X, cen.Y, cen.Z)));
}

/**
 * Creates a rotation from one unit-vector's direction to another unit-vector's direction.
 * If the tips of the two unitized vectors have a distance less than 1e-12 the identity matrix is returned.
 * If the tips of the two vectors are almost exactly opposite,
 * that is if tips of the two vectors are less than 1e-12 apart when summed,
 * there is no valid unique 180 degree rotation that can be found, so an exception is raised.
 */
export function RigidMatrix_createVecToVec_6319FE20(vecFrom, vecTo) {
    let v, v_2;
    let vt;
    const a = vecFrom;
    const b = vecTo;
    vt = Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
    if (!(((v = vt, ((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z))) > 1E-24)) {
        return RigidMatrix_get_identity();
    }
    else {
        let v_1;
        const a_1 = vecFrom;
        const b_1 = vecTo;
        v_1 = Vec_$ctor_Z7AD9E565(a_1.X + b_1.X, a_1.Y + b_1.Y, a_1.Z + b_1.Z);
        if (!(((v_2 = v_1, ((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z))) > 1E-24)) {
            fail(`RigidMatrix.createVecToVec failed to find a rotation axis for (almost) colinear unit-vectors in opposite directions: ${vecFrom} and ${vecTo}`);
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
        return RigidMatrix_$ctor_Z15A9A3C0((tx * x_3) + c, (tx * y) - (s * z), (tx * z) + (s * y), 0, (tx * y) + (s * z), (ty * y) + c, (ty * z) - (s * x_3), 0, (tx * z) - (s * y), (ty * z) + (s * x_3), ((t * z) * z) + c, 0);
    }
}

/**
 * Creates a rotation from one vector's direction to another vector's direction.
 * If the tips of the two unitized vectors have a distance less than 1e-12 the identity matrix is returned.
 * If the tips of the two vectors are almost exactly opposite,
 * that is if tips of the two vectors are less than 1e-12 apart when summed,
 * there is no valid unique 180 degree rotation that can be found, so an exception is raised.
 * Fails if either vector is too short (length less than 1e-6).
 */
export function RigidMatrix_createVecToVec_5A694120(vecFrom, vecTo) {
    let v, v_2;
    let fu;
    const x = vecFrom.X;
    const y = vecFrom.Y;
    const z = vecFrom.Z;
    const length = Math.sqrt(((x * x) + (y * y)) + (z * z));
    if (!(length > 1E-06)) {
        fail(`RigidMatrix.createVecToVec failed. The vector is too short: vecFrom: ${vecFrom}`);
    }
    const sc = 1 / length;
    fu = UnitVec_$ctor_Z7AD9E565(x * sc, y * sc, z * sc);
    let tu;
    const x_3 = vecTo.X;
    const y_2 = vecTo.Y;
    const z_2 = vecTo.Z;
    const length_1 = Math.sqrt(((x_3 * x_3) + (y_2 * y_2)) + (z_2 * z_2));
    if (!(length_1 > 1E-06)) {
        fail(`RigidMatrix.createVecToVec failed. The vector is too short: vecTo: ${vecTo}`);
    }
    const sc_1 = 1 / length_1;
    tu = UnitVec_$ctor_Z7AD9E565(x_3 * sc_1, y_2 * sc_1, z_2 * sc_1);
    let vt;
    const a = fu;
    const b = tu;
    vt = Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
    if (!(((v = vt, ((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z))) > 1E-24)) {
        return RigidMatrix_get_identity();
    }
    else {
        let v_1;
        const a_1 = fu;
        const b_1 = tu;
        v_1 = Vec_$ctor_Z7AD9E565(a_1.X + b_1.X, a_1.Y + b_1.Y, a_1.Z + b_1.Z);
        if (!(((v_2 = v_1, ((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z))) > 1E-24)) {
            fail(`RigidMatrix.createVecToVec failed to find a rotation axis for (almost) colinear vectors in opposite directions: ${vecFrom} and ${vecTo}`);
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
        return RigidMatrix_$ctor_Z15A9A3C0((tx * x_9) + c, (tx * y_4) - (s * z_4), (tx * z_4) + (s * y_4), 0, (tx * y_4) + (s * z_4), (ty * y_4) + c, (ty * z_4) - (s * x_9), 0, (tx * z_4) - (s * y_4), (ty * z_4) + (s * x_9), ((t * z_4) * z_4) + c, 0);
    }
}

/**
 * Creates a RigidMatrix to transform from World plane or Coordinate System to given Plane.
 * Also called Change of Basis.
 */
export function RigidMatrix_createToPlane_2F29039F(p) {
    return RigidMatrix_$ctor_Z15A9A3C0(p.Xaxis.X, p.Yaxis.X, p.Zaxis.X, p.Origin.X, p.Xaxis.Y, p.Yaxis.Y, p.Zaxis.Y, p.Origin.Y, p.Xaxis.Z, p.Yaxis.Z, p.Zaxis.Z, p.Origin.Z);
}

/**
 * Creates a RigidMatrix to transform from one Plane or Coordinate System to another Plane.
 */
export function RigidMatrix_createPlaneToPlane_3B6074E0(fromPlane, toPlane) {
    const f = RigidMatrix__get_Inverse(RigidMatrix_createToPlane_2F29039F(fromPlane));
    const t = RigidMatrix_createToPlane_2F29039F(toPlane);
    return RigidMatrix_multiply_Z31732520(f, t);
}

/**
 * Tries to create a 3x4 RigidMatrix from a general 4x4 matrix.
 * Returns None if the input matrix does scale, shear, flip, mirror, reflect or project.
 * However, translation is allowed.
 */
export function RigidMatrix_tryCreateFromMatrix_3CAE9522(m) {
    let x_1, v, x_2, v_1, x_3, v_2, x_4, a, b, x_5, a_1, b_1, x_6, a_2, b_2, x_7, a_3, a_4, b_4, b_3;
    if (Matrix__get_IsProjecting(m)) {
        return undefined;
    }
    else {
        const x = Vec_$ctor_Z7AD9E565(m.M11, m.M12, m.M13);
        const y = Vec_$ctor_Z7AD9E565(m.M21, m.M22, m.M23);
        const z = Vec_$ctor_Z7AD9E565(m.M31, m.M32, m.M33);
        if ((((((((x_1 = ((v = x, ((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z))), (0.999999 < x_1) && (x_1 < 1.000001))) && ((x_2 = ((v_1 = y, ((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z))), (0.999999 < x_2) && (x_2 < 1.000001)))) && ((x_3 = ((v_2 = z, ((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z))), (0.999999 < x_3) && (x_3 < 1.000001)))) && ((x_4 = ((a = x, (b = y, ((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z)))), (-1E-06 < x_4) && (x_4 < 1E-06)))) && ((x_5 = ((a_1 = x, (b_1 = z, ((a_1.X * b_1.X) + (a_1.Y * b_1.Y)) + (a_1.Z * b_1.Z)))), (-1E-06 < x_5) && (x_5 < 1E-06)))) && ((x_6 = ((a_2 = y, (b_2 = z, ((a_2.X * b_2.X) + (a_2.Y * b_2.Y)) + (a_2.Z * b_2.Z)))), (-1E-06 < x_6) && (x_6 < 1E-06)))) && ((x_7 = ((a_3 = ((a_4 = x, (b_4 = y, Vec_$ctor_Z7AD9E565((a_4.Y * b_4.Z) - (a_4.Z * b_4.Y), (a_4.Z * b_4.X) - (a_4.X * b_4.Z), (a_4.X * b_4.Y) - (a_4.Y * b_4.X))))), (b_3 = z, ((a_3.X * b_3.X) + (a_3.Y * b_3.Y)) + (a_3.Z * b_3.Z)))), (0.999999 < x_7) && (x_7 < 1.000001)))) {
            return RigidMatrix_$ctor_Z15A9A3C0(m.M11, m.M21, m.M31, m.X41, m.M12, m.M22, m.M32, m.Y42, m.M13, m.M23, m.M33, m.Z43);
        }
        else {
            return undefined;
        }
    }
}

/**
 * Creates an immutable 4x3 Transformation Matrix.
 * Checks the input values to ensure they form a valid RigidMatrix.
 * This Constructor takes arguments in row-major order:
 * <code>M11 M21 M31 X41 M12 M22 M32 Y42 M13 M23 M33 Z43</code>
 * Where X41, Y42 and Z43 refer to the translation part of the RigidMatrix.
 */
export function RigidMatrix_create_Z15A9A3C0(m11, m21, m31, x41, m12, m22, m32, y42, m13, m23, m33, z43) {
    let x_1, v, x_2, v_1, x_3, v_2, x_4, a, b, x_5, a_1, b_1, x_6, a_2, b_2, x_7, a_3, a_4, b_4, b_3;
    const z = Vec_$ctor_Z7AD9E565(m31, m32, m33);
    const y = Vec_$ctor_Z7AD9E565(m21, m22, m23);
    const x = Vec_$ctor_Z7AD9E565(m11, m12, m13);
    if ((((((((x_1 = ((v = x, ((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z))), (0.999999 < x_1) && (x_1 < 1.000001))) && ((x_2 = ((v_1 = y, ((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z))), (0.999999 < x_2) && (x_2 < 1.000001)))) && ((x_3 = ((v_2 = z, ((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z))), (0.999999 < x_3) && (x_3 < 1.000001)))) && ((x_4 = ((a = x, (b = y, ((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z)))), (-1E-06 < x_4) && (x_4 < 1E-06)))) && ((x_5 = ((a_1 = x, (b_1 = z, ((a_1.X * b_1.X) + (a_1.Y * b_1.Y)) + (a_1.Z * b_1.Z)))), (-1E-06 < x_5) && (x_5 < 1E-06)))) && ((x_6 = ((a_2 = y, (b_2 = z, ((a_2.X * b_2.X) + (a_2.Y * b_2.Y)) + (a_2.Z * b_2.Z)))), (-1E-06 < x_6) && (x_6 < 1E-06)))) && ((x_7 = ((a_3 = ((a_4 = x, (b_4 = y, Vec_$ctor_Z7AD9E565((a_4.Y * b_4.Z) - (a_4.Z * b_4.Y), (a_4.Z * b_4.X) - (a_4.X * b_4.Z), (a_4.X * b_4.Y) - (a_4.Y * b_4.X))))), (b_3 = z, ((a_3.X * b_3.X) + (a_3.Y * b_3.Y)) + (a_3.Z * b_3.Z)))), (0.999999 < x_7) && (x_7 < 1.000001)))) {
        return RigidMatrix_$ctor_Z15A9A3C0(m11, m21, m31, x41, m12, m22, m32, y42, m13, m23, m33, z43);
    }
    else {
        return fail(`RigidMatrix.create failed. The input values do scale, shear, flip, mirror, reflect or project: ${m11}, ${m21}, ${m31}, ${x41}, ${m12}, ${m22}, ${m32}, ${y42}, ${m13}, ${m23}, ${m33}, ${z43}`);
    }
}

/**
 * Tries to create a 3x4 RigidMatrix from a general 4x4 matrix.
 * Fails if the input matrix does scale, shear, flip, mirror, reflect or project.
 * However, translation is allowed.
 */
export function RigidMatrix_createFromMatrix_3CAE9522(m) {
    const matchValue = RigidMatrix_tryCreateFromMatrix_3CAE9522(m);
    if (matchValue == null) {
        return fail(`RigidMatrix.createFromMatrix failed. The input matrix does scale, shear, flip, mirror, reflect or project: ${m}`);
    }
    else {
        const m_1 = matchValue;
        return m_1;
    }
}

/**
 * Converts the 3x4 RigidMatrix to a general 4x4 Matrix.
 */
export function RigidMatrix_toMatrix_Z625426AD(m) {
    return Matrix_$ctor_Z61E40B00(m.M11, m.M21, m.M31, m.X41, m.M12, m.M22, m.M32, m.Y42, m.M13, m.M23, m.M33, m.Z43, 0, 0, 0, 1);
}

/**
 * Create a RigidMatrix from a Quaternion.
 */
export function RigidMatrix_createFromQuaternion_Z2A007687(quaternion) {
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
    return RigidMatrix_$ctor_Z15A9A3C0(1 - (yy + zz), xy - wz, xz + wy, 0, xy + wz, 1 - (xx + zz), yz - wx, 0, xz - wy, yz + wx, 1 - (xx + yy), 0);
}

/**
 * Removes the translation part by setting X41, Y42 and Z43 to 0.0.
 */
export function RigidMatrix_removeTranslation_Z625426AD(m) {
    return RigidMatrix_$ctor_Z15A9A3C0(m.M11, m.M21, m.M31, 0, m.M12, m.M22, m.M32, 0, m.M13, m.M23, m.M33, 0);
}

/**
 * Adds a vector translation to an existing RigidMatrix.
 */
export function RigidMatrix_addTranslation(v, m) {
    return RigidMatrix_$ctor_Z15A9A3C0(m.M11, m.M21, m.M31, m.X41 + v.X, m.M12, m.M22, m.M32, m.Y42 + v.Y, m.M13, m.M23, m.M33, m.Z43 + v.Z);
}

/**
 * Adds an X, Y and Z translation to an existing RigidMatrix.
 */
export function RigidMatrix_addTranslationXYZ(x, y, z, m) {
    return RigidMatrix_$ctor_Z15A9A3C0(m.M11, m.M21, m.M31, m.X41 + x, m.M12, m.M22, m.M32, m.Y42 + y, m.M13, m.M23, m.M33, m.Z43 + z);
}

