
import { RigidMatrix_tryCreateFromMatrix_3CAE9522 } from "../RigidMatrix.js";
import { Quaternion_$ctor_77D16AC0 } from "../Quaternion.js";

/**
 * If the matrix is affine and has no shear or scaling, it returns the rotation defined in its 3x3 part.
 */
export function Euclid_Quaternion__Quaternion_tryCreateFromMatrix_Static_3CAE9522(matrix) {
    const matchValue = RigidMatrix_tryCreateFromMatrix_3CAE9522(matrix);
    if (matchValue != null) {
        const m = matchValue;
        const m11 = m.M11;
        const m12 = m.M21;
        const m13 = m.M31;
        const m21 = m.M12;
        const m22 = m.M22;
        const m23 = m.M32;
        const m31 = m.M13;
        const m32 = m.M23;
        const m33 = m.M33;
        const trace = (m11 + m22) + m33;
        if (trace > 0) {
            const s = 0.5 / Math.sqrt(trace + 1);
            return Quaternion_$ctor_77D16AC0((m32 - m23) * s, (m13 - m31) * s, (m21 - m12) * s, 0.25 / s);
        }
        else if ((m11 > m22) && (m11 > m33)) {
            const s_1 = 2 * Math.sqrt(((1 + m11) - m22) - m33);
            return Quaternion_$ctor_77D16AC0(0.25 * s_1, (m12 + m21) / s_1, (m13 + m31) / s_1, (m32 - m23) / s_1);
        }
        else if (m22 > m33) {
            const s_2 = 2 * Math.sqrt(((1 + m22) - m11) - m33);
            return Quaternion_$ctor_77D16AC0((m12 + m21) / s_2, 0.25 * s_2, (m23 + m32) / s_2, (m13 - m31) / s_2);
        }
        else {
            const s_3 = 2 * Math.sqrt(((1 + m33) - m11) - m22);
            return Quaternion_$ctor_77D16AC0((m13 + m31) / s_3, (m23 + m32) / s_3, 0.25 * s_3, (m21 - m12) / s_3);
        }
    }
    else {
        return undefined;
    }
}

/**
 * Returns the rotation defined in a RigidMatrix's 3x3 part.
 */
export function Euclid_Quaternion__Quaternion_createFromRigidMatrix_Static_Z625426AD(orthoMatrix) {
    const m11 = orthoMatrix.M11;
    const m12 = orthoMatrix.M21;
    const m13 = orthoMatrix.M31;
    const m21 = orthoMatrix.M12;
    const m22 = orthoMatrix.M22;
    const m23 = orthoMatrix.M32;
    const m31 = orthoMatrix.M13;
    const m32 = orthoMatrix.M23;
    const m33 = orthoMatrix.M33;
    const trace = (m11 + m22) + m33;
    if (trace > 0) {
        const s = 0.5 / Math.sqrt(trace + 1);
        return Quaternion_$ctor_77D16AC0((m32 - m23) * s, (m13 - m31) * s, (m21 - m12) * s, 0.25 / s);
    }
    else if ((m11 > m22) && (m11 > m33)) {
        const s_1 = 2 * Math.sqrt(((1 + m11) - m22) - m33);
        return Quaternion_$ctor_77D16AC0(0.25 * s_1, (m12 + m21) / s_1, (m13 + m31) / s_1, (m32 - m23) / s_1);
    }
    else if (m22 > m33) {
        const s_2 = 2 * Math.sqrt(((1 + m22) - m11) - m33);
        return Quaternion_$ctor_77D16AC0((m12 + m21) / s_2, 0.25 * s_2, (m23 + m32) / s_2, (m13 - m31) / s_2);
    }
    else {
        const s_3 = 2 * Math.sqrt(((1 + m33) - m11) - m22);
        return Quaternion_$ctor_77D16AC0((m13 + m31) / s_3, (m23 + m32) / s_3, 0.25 * s_3, (m21 - m12) / s_3);
    }
}

