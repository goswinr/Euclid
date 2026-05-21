
import { AccuracyModule_low, Test_TestCaseBuilder__For_Z371464DD, Test_TestCaseBuilder__Combine_3A59D1F3, AccuracyModule_veryHigh, Expect_isNone, Expect_throws, AccuracyModule_medium, AccuracyModule_high, Expect_floatClose, Test_TestCaseBuilder__Zero, Expect_isTrue, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList, Expect_isFalse } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Pnt_$ctor_Z7AD9E565, Pnt__get_AsString } from "./Src/Pnt.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { RigidMatrix__get_Translation, RigidMatrix__get_ToArrayByRows, RigidMatrix__get_ToArrayByColumns, RigidMatrix_addTranslationXYZ, RigidMatrix_createTranslationZ_5E38073B, RigidMatrix_createTranslationY_5E38073B, RigidMatrix_createTranslationX_5E38073B, RigidMatrix_create_Z15A9A3C0, RigidMatrix_createRotationAxisCenter_976E587, RigidMatrix_createRotationAxis_73D64DB4, RigidMatrix_tryCreateFromMatrix_3CAE9522, RigidMatrix_createVecToVec_5A694120, RigidMatrix__get_ColumnVector3, RigidMatrix__get_ColumnVector2, RigidMatrix__get_ColumnVector1, RigidMatrix_createPlaneToPlane_3B6074E0, RigidMatrix_createToPlane_2F29039F, RigidMatrix_createFromMatrix_3CAE9522, RigidMatrix__get_Matrix, RigidMatrix_equals, RigidMatrix_removeTranslation_Z625426AD, RigidMatrix__get_IsIdentity, RigidMatrix_createVecToVec_6319FE20, RigidMatrix_createRotationAxisCenter_Z655651F, RigidMatrix_createRotationAxis_Z3D1F83EE, RigidMatrix_createRotationZ_5E38073B, RigidMatrix_createRotationY_5E38073B, RigidMatrix_createRotationX_5E38073B, RigidMatrix_createFromQuaternion_Z2A007687, RigidMatrix__get_Inverse, RigidMatrix_get_identity, RigidMatrix_multiply_Z31732520, RigidMatrix_createTranslation_Z394EC5F7, RigidMatrix_addTranslation } from "./Src/RigidMatrix.js";
import { Vec_$ctor_Z7AD9E565 } from "./Src/Vec.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_1 } from "./Src/Pnt.js";
import { Quaternion_$ctor_77D16AC0, Quaternion_createFromRadians_Z3D1F83EE } from "./Src/Quaternion.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_1 } from "./Src/Vec.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_2 } from "./Src/Vec.js";
import { RigidMatrix_multiply_Z31732520 as RigidMatrix_multiply_Z31732520_1 } from "./Src/RigidMatrix.js";
import { UnitVec_$ctor_Z7AD9E565 } from "./Src/UnitVec.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_2 } from "./Src/Pnt.js";
import { failUnit3 } from "./Src/EuclidErrors.js";
import { PPlane_$ctor_3CB4665C } from "./Src/PPlane.js";
import { Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_58CBC093 } from "./Src/TypeExtensions/PPlane.js";
import { Matrix_$ctor_Z61E40B00, Matrix_createPerspective_77D16AC0, Matrix_createScale_Z7AD9E565 } from "./Src/Matrix.js";
import { rangeDouble } from "./fable_modules/fable-library-js.5.0.0/Range.js";
import { contains, ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { failUnit3 as failUnit3_1 } from "./Src/EuclidErrors.js";
import { item, equalsWith } from "./fable_modules/fable-library-js.5.0.0/Array.js";
import { int32ToString, Exception, structuralHash, assertEqual } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { equals, class_type, decimal_type, string_type, bool_type, int32_type, array_type, float64_type } from "./fable_modules/fable-library-js.5.0.0/Reflection.js";
import { seqToString } from "./fable_modules/fable-library-js.5.0.0/Types.js";
import { printf, toText } from "./fable_modules/fable-library-js.5.0.0/String.js";

export function notEqual(a, b, msg) {
    let a_2, b_2, x, y, z;
    const same = ((a_2 = a, (b_2 = b, (x = (a_2.X - b_2.X), (y = (a_2.Y - b_2.Y), (z = (a_2.Z - b_2.Z), Math.sqrt(((x * x) + (y * y)) + (z * z)))))))) < 1E-12;
    if (same) {
        Expect_isFalse(same)(`${msg} expected: 
${Pnt__get_AsString(b)}, got: 
${Pnt__get_AsString(a)}`);
    }
    else {
        Expect_isFalse(same)(msg);
    }
}

export const tests = Test_testList("RigidMatrix transformations", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix translate ", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let a_3, b_4, x_1, y_1, z_1, a_6, b_8, x_3, y_3, z_3, a_9, b_12, x_5, y_5, z_5, a_12, b_16, x_7, y_7, z_7;
        const m_1 = RigidMatrix_addTranslation(Vec_$ctor_Z7AD9E565(4, 1, -1), RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(2, 3, 4)));
        const a = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        let b;
        const v_1 = a;
        const m_2 = m_1;
        const x = v_1.X;
        const y = v_1.Y;
        const z = v_1.Z;
        b = Pnt_$ctor_Z7AD9E565_1((((m_2.M11 * x) + (m_2.M21 * y)) + (m_2.M31 * z)) + m_2.X41, (((m_2.M12 * x) + (m_2.M22 * y)) + (m_2.M32 * z)) + m_2.Y42, (((m_2.M13 * x) + (m_2.M23 * y)) + (m_2.M33 * z)) + m_2.Z43);
        const a_1 = b;
        const b_2 = Pnt_$ctor_Z7AD9E565(7, 6, 6);
        const same = ((a_3 = a_1, (b_4 = b_2, (x_1 = (a_3.X - b_4.X), (y_1 = (a_3.Y - b_4.Y), (z_1 = (a_3.Z - b_4.Z), Math.sqrt(((x_1 * x_1) + (y_1 * y_1)) + (z_1 * z_1)))))))) < 1E-12;
        if (!same) {
            Expect_isTrue(same)(`${"translation ok"} expected: 
${Pnt__get_AsString(b_2)}, got: 
${Pnt__get_AsString(a_1)}`);
        }
        else {
            Expect_isTrue(same)("translation ok");
        }
        let c;
        const v_2 = a;
        const m_6 = m_1;
        const x_2 = v_2.X;
        const y_2 = v_2.Y;
        const z_2 = v_2.Z;
        c = Pnt_$ctor_Z7AD9E565_1((((m_6.M11 * x_2) + (m_6.M21 * y_2)) + (m_6.M31 * z_2)) + m_6.X41, (((m_6.M12 * x_2) + (m_6.M22 * y_2)) + (m_6.M32 * z_2)) + m_6.Y42, (((m_6.M13 * x_2) + (m_6.M23 * y_2)) + (m_6.M33 * z_2)) + m_6.Z43);
        const a_4 = c;
        const b_6 = Pnt_$ctor_Z7AD9E565(7, 6, 6);
        const same_1 = ((a_6 = a_4, (b_8 = b_6, (x_3 = (a_6.X - b_8.X), (y_3 = (a_6.Y - b_8.Y), (z_3 = (a_6.Z - b_8.Z), Math.sqrt(((x_3 * x_3) + (y_3 * y_3)) + (z_3 * z_3)))))))) < 1E-12;
        if (!same_1) {
            Expect_isTrue(same_1)(`${"transform ok"} expected: 
${Pnt__get_AsString(b_6)}, got: 
${Pnt__get_AsString(a_4)}`);
        }
        else {
            Expect_isTrue(same_1)("transform ok");
        }
        const m1 = RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(2, 3, 4));
        const m2 = RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(4, 1, -1));
        const m3 = RigidMatrix_multiply_Z31732520(m1, m2);
        let d;
        const v_3 = a;
        const m_7 = m3;
        const x_4 = v_3.X;
        const y_4 = v_3.Y;
        const z_4 = v_3.Z;
        d = Pnt_$ctor_Z7AD9E565_1((((m_7.M11 * x_4) + (m_7.M21 * y_4)) + (m_7.M31 * z_4)) + m_7.X41, (((m_7.M12 * x_4) + (m_7.M22 * y_4)) + (m_7.M32 * z_4)) + m_7.Y42, (((m_7.M13 * x_4) + (m_7.M23 * y_4)) + (m_7.M33 * z_4)) + m_7.Z43);
        const a_7 = d;
        const b_10 = Pnt_$ctor_Z7AD9E565(7, 6, 6);
        const same_2 = ((a_9 = a_7, (b_12 = b_10, (x_5 = (a_9.X - b_12.X), (y_5 = (a_9.Y - b_12.Y), (z_5 = (a_9.Z - b_12.Z), Math.sqrt(((x_5 * x_5) + (y_5 * y_5)) + (z_5 * z_5)))))))) < 1E-12;
        if (!same_2) {
            Expect_isTrue(same_2)(`${"multiply ok"} expected: 
${Pnt__get_AsString(b_10)}, got: 
${Pnt__get_AsString(a_7)}`);
        }
        else {
            Expect_isTrue(same_2)("multiply ok");
        }
        let e;
        const v_4 = a;
        const m_11 = m3;
        const x_6 = v_4.X;
        const y_6 = v_4.Y;
        const z_6 = v_4.Z;
        e = Pnt_$ctor_Z7AD9E565_1((((m_11.M11 * x_6) + (m_11.M21 * y_6)) + (m_11.M31 * z_6)) + m_11.X41, (((m_11.M12 * x_6) + (m_11.M22 * y_6)) + (m_11.M32 * z_6)) + m_11.Y42, (((m_11.M13 * x_6) + (m_11.M23 * y_6)) + (m_11.M33 * z_6)) + m_11.Z43);
        const a_10 = e;
        const b_14 = Pnt_$ctor_Z7AD9E565(7, 6, 6);
        const same_3 = ((a_12 = a_10, (b_16 = b_14, (x_7 = (a_12.X - b_16.X), (y_7 = (a_12.Y - b_16.Y), (z_7 = (a_12.Z - b_16.Z), Math.sqrt(((x_7 * x_7) + (y_7 * y_7)) + (z_7 * z_7)))))))) < 1E-12;
        if (!same_3) {
            Expect_isTrue(same_3)(`${"transform multiply ok"} expected: 
${Pnt__get_AsString(b_14)}, got: 
${Pnt__get_AsString(a_10)}`);
        }
        else {
            Expect_isTrue(same_3)("transform multiply ok");
        }
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix inverse id ", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let a_16, b_19, x_10, y_10, z_10;
        const m_12 = RigidMatrix_get_identity();
        const inv = RigidMatrix__get_Inverse(m_12);
        const a_13 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        let ax;
        const v_5 = a_13;
        const m_13 = m_12;
        const x_8 = v_5.X;
        const y_8 = v_5.Y;
        const z_8 = v_5.Z;
        ax = Pnt_$ctor_Z7AD9E565_1((((m_13.M11 * x_8) + (m_13.M21 * y_8)) + (m_13.M31 * z_8)) + m_13.X41, (((m_13.M12 * x_8) + (m_13.M22 * y_8)) + (m_13.M32 * z_8)) + m_13.Y42, (((m_13.M13 * x_8) + (m_13.M23 * y_8)) + (m_13.M33 * z_8)) + m_13.Z43);
        let a0;
        const v_6 = ax;
        const m_14 = inv;
        const x_9 = v_6.X;
        const y_9 = v_6.Y;
        const z_9 = v_6.Z;
        a0 = Pnt_$ctor_Z7AD9E565_1((((m_14.M11 * x_9) + (m_14.M21 * y_9)) + (m_14.M31 * z_9)) + m_14.X41, (((m_14.M12 * x_9) + (m_14.M22 * y_9)) + (m_14.M32 * z_9)) + m_14.Y42, (((m_14.M13 * x_9) + (m_14.M23 * y_9)) + (m_14.M33 * z_9)) + m_14.Z43);
        const a_14 = a0;
        const b_17 = a_13;
        const same_4 = ((a_16 = a_14, (b_19 = b_17, (x_10 = (a_16.X - b_19.X), (y_10 = (a_16.Y - b_19.Y), (z_10 = (a_16.Z - b_19.Z), Math.sqrt(((x_10 * x_10) + (y_10 * y_10)) + (z_10 * z_10)))))))) < 1E-12;
        if (!same_4) {
            Expect_isTrue(same_4)(`${"inverse identity ok"} expected: 
${Pnt__get_AsString(b_17)}, got: 
${Pnt__get_AsString(a_14)}`);
        }
        else {
            Expect_isTrue(same_4)("inverse identity ok");
        }
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix inverse transform rot ***", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let a_17, a_21, b_22, x_13, y_13, z_13;
        const t = RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(-2, -3, 4));
        const q = Quaternion_createFromRadians_Z3D1F83EE((a_17 = Vec_$ctor_Z7AD9E565_1(0, 0, 1), Vec_$ctor_Z7AD9E565_2(a_17.X * 9, a_17.Y * 9, a_17.Z * 9)), 0.017453292519943295 * 90);
        const r = RigidMatrix_createFromQuaternion_Z2A007687(q);
        const m_15 = RigidMatrix_multiply_Z31732520_1(t, r);
        const inv_1 = RigidMatrix__get_Inverse(m_15);
        const a_18 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        let r_1;
        let v_8;
        const v_7 = a_18;
        const m_19 = m_15;
        const x_11 = v_7.X;
        const y_11 = v_7.Y;
        const z_11 = v_7.Z;
        v_8 = Pnt_$ctor_Z7AD9E565_1((((m_19.M11 * x_11) + (m_19.M21 * y_11)) + (m_19.M31 * z_11)) + m_19.X41, (((m_19.M12 * x_11) + (m_19.M22 * y_11)) + (m_19.M32 * z_11)) + m_19.Y42, (((m_19.M13 * x_11) + (m_19.M23 * y_11)) + (m_19.M33 * z_11)) + m_19.Z43);
        const m_23 = inv_1;
        const x_12 = v_8.X;
        const y_12 = v_8.Y;
        const z_12 = v_8.Z;
        r_1 = Pnt_$ctor_Z7AD9E565_1((((m_23.M11 * x_12) + (m_23.M21 * y_12)) + (m_23.M31 * z_12)) + m_23.X41, (((m_23.M12 * x_12) + (m_23.M22 * y_12)) + (m_23.M32 * z_12)) + m_23.Y42, (((m_23.M13 * x_12) + (m_23.M23 * y_12)) + (m_23.M33 * z_12)) + m_23.Z43);
        const a_19 = a_18;
        const b_20 = r_1;
        const same_5 = ((a_21 = a_19, (b_22 = b_20, (x_13 = (a_21.X - b_22.X), (y_13 = (a_21.Y - b_22.Y), (z_13 = (a_21.Z - b_22.Z), Math.sqrt(((x_13 * x_13) + (y_13 * y_13)) + (z_13 * z_13)))))))) < 1E-12;
        if (!same_5) {
            Expect_isTrue(same_5)(`${"inverse transformRigid ok"} expected: 
${Pnt__get_AsString(b_20)}, got: 
${Pnt__get_AsString(a_19)}`);
        }
        else {
            Expect_isTrue(same_5)("inverse transformRigid ok");
        }
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix inverse transform rot inv ***", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let a_22, a_26, b_25, x_16, y_16, z_16;
        const t_1 = RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(-2, -3, 4));
        const q_1 = Quaternion_createFromRadians_Z3D1F83EE((a_22 = Vec_$ctor_Z7AD9E565_1(0, 0, 1), Vec_$ctor_Z7AD9E565_2(a_22.X * 9, a_22.Y * 9, a_22.Z * 9)), 0.017453292519943295 * 90);
        const r_2 = RigidMatrix_createFromQuaternion_Z2A007687(q_1);
        const m_24 = RigidMatrix_multiply_Z31732520_1(r_2, t_1);
        const inv_2 = RigidMatrix__get_Inverse(m_24);
        const a_23 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        let r_3;
        let v_10;
        const v_9 = a_23;
        const m_28 = m_24;
        const x_14 = v_9.X;
        const y_14 = v_9.Y;
        const z_14 = v_9.Z;
        v_10 = Pnt_$ctor_Z7AD9E565_1((((m_28.M11 * x_14) + (m_28.M21 * y_14)) + (m_28.M31 * z_14)) + m_28.X41, (((m_28.M12 * x_14) + (m_28.M22 * y_14)) + (m_28.M32 * z_14)) + m_28.Y42, (((m_28.M13 * x_14) + (m_28.M23 * y_14)) + (m_28.M33 * z_14)) + m_28.Z43);
        const m_32 = inv_2;
        const x_15 = v_10.X;
        const y_15 = v_10.Y;
        const z_15 = v_10.Z;
        r_3 = Pnt_$ctor_Z7AD9E565_1((((m_32.M11 * x_15) + (m_32.M21 * y_15)) + (m_32.M31 * z_15)) + m_32.X41, (((m_32.M12 * x_15) + (m_32.M22 * y_15)) + (m_32.M32 * z_15)) + m_32.Y42, (((m_32.M13 * x_15) + (m_32.M23 * y_15)) + (m_32.M33 * z_15)) + m_32.Z43);
        const a_24 = a_23;
        const b_23 = r_3;
        const same_6 = ((a_26 = a_24, (b_25 = b_23, (x_16 = (a_26.X - b_25.X), (y_16 = (a_26.Y - b_25.Y), (z_16 = (a_26.Z - b_25.Z), Math.sqrt(((x_16 * x_16) + (y_16 * y_16)) + (z_16 * z_16)))))))) < 1E-12;
        if (!same_6) {
            Expect_isTrue(same_6)(`${"inverse transformRigid ok"} expected: 
${Pnt__get_AsString(b_23)}, got: 
${Pnt__get_AsString(a_24)}`);
        }
        else {
            Expect_isTrue(same_6)("inverse transformRigid ok");
        }
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix inverse transformRigid  ", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let a_30, b_28, x_19, y_19, z_19;
        const m_33 = RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(-2, -3, 4));
        const inv_3 = RigidMatrix__get_Inverse(m_33);
        const a_27 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        let ax_1;
        const v_11 = a_27;
        const m_34 = m_33;
        const x_17 = v_11.X;
        const y_17 = v_11.Y;
        const z_17 = v_11.Z;
        ax_1 = Pnt_$ctor_Z7AD9E565_1((((m_34.M11 * x_17) + (m_34.M21 * y_17)) + (m_34.M31 * z_17)) + m_34.X41, (((m_34.M12 * x_17) + (m_34.M22 * y_17)) + (m_34.M32 * z_17)) + m_34.Y42, (((m_34.M13 * x_17) + (m_34.M23 * y_17)) + (m_34.M33 * z_17)) + m_34.Z43);
        let a0_1;
        const v_12 = ax_1;
        const m_35 = inv_3;
        const x_18 = v_12.X;
        const y_18 = v_12.Y;
        const z_18 = v_12.Z;
        a0_1 = Pnt_$ctor_Z7AD9E565_1((((m_35.M11 * x_18) + (m_35.M21 * y_18)) + (m_35.M31 * z_18)) + m_35.X41, (((m_35.M12 * x_18) + (m_35.M22 * y_18)) + (m_35.M32 * z_18)) + m_35.Y42, (((m_35.M13 * x_18) + (m_35.M23 * y_18)) + (m_35.M33 * z_18)) + m_35.Z43);
        const a_28 = a0_1;
        const b_26 = a_27;
        const same_7 = ((a_30 = a_28, (b_28 = b_26, (x_19 = (a_30.X - b_28.X), (y_19 = (a_30.Y - b_28.Y), (z_19 = (a_30.Z - b_28.Z), Math.sqrt(((x_19 * x_19) + (y_19 * y_19)) + (z_19 * z_19)))))))) < 1E-12;
        if (!same_7) {
            Expect_isTrue(same_7)(`${"inverse *** ok"} expected: 
${Pnt__get_AsString(b_26)}, got: 
${Pnt__get_AsString(a_28)}`);
        }
        else {
            Expect_isTrue(same_7)("inverse *** ok");
        }
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix inverse rotate ", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let a_31, a_35, b_31, x_22, y_22, z_22;
        const q_2 = Quaternion_createFromRadians_Z3D1F83EE((a_31 = Vec_$ctor_Z7AD9E565_1(0, 0, 1), Vec_$ctor_Z7AD9E565_2(a_31.X * 9, a_31.Y * 9, a_31.Z * 9)), 0.017453292519943295 * 90);
        const m_36 = RigidMatrix_createFromQuaternion_Z2A007687(q_2);
        const a_32 = Pnt_$ctor_Z7AD9E565(9, 0, 3);
        const inv_4 = RigidMatrix__get_Inverse(m_36);
        let c_1;
        let v_14;
        const v_13 = a_32;
        const m_40 = m_36;
        const x_20 = v_13.X;
        const y_20 = v_13.Y;
        const z_20 = v_13.Z;
        v_14 = Pnt_$ctor_Z7AD9E565_1((((m_40.M11 * x_20) + (m_40.M21 * y_20)) + (m_40.M31 * z_20)) + m_40.X41, (((m_40.M12 * x_20) + (m_40.M22 * y_20)) + (m_40.M32 * z_20)) + m_40.Y42, (((m_40.M13 * x_20) + (m_40.M23 * y_20)) + (m_40.M33 * z_20)) + m_40.Z43);
        const m_44 = inv_4;
        const x_21 = v_14.X;
        const y_21 = v_14.Y;
        const z_21 = v_14.Z;
        c_1 = Pnt_$ctor_Z7AD9E565_1((((m_44.M11 * x_21) + (m_44.M21 * y_21)) + (m_44.M31 * z_21)) + m_44.X41, (((m_44.M12 * x_21) + (m_44.M22 * y_21)) + (m_44.M32 * z_21)) + m_44.Y42, (((m_44.M13 * x_21) + (m_44.M23 * y_21)) + (m_44.M33 * z_21)) + m_44.Z43);
        const a_33 = a_32;
        const b_29 = c_1;
        const same_8 = ((a_35 = a_33, (b_31 = b_29, (x_22 = (a_35.X - b_31.X), (y_22 = (a_35.Y - b_31.Y), (z_22 = (a_35.Z - b_31.Z), Math.sqrt(((x_22 * x_22) + (y_22 * y_22)) + (z_22 * z_22)))))))) < 1E-12;
        if (!same_8) {
            Expect_isTrue(same_8)(`${"rotateByQuaternion inverse"} expected: 
${Pnt__get_AsString(b_29)}, got: 
${Pnt__get_AsString(a_33)}`);
        }
        else {
            Expect_isTrue(same_8)("rotateByQuaternion inverse");
        }
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix rotation X axis", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        let a_38, b_35, x_24, y_24, z_24;
        const m_45 = RigidMatrix_createRotationX_5E38073B(90);
        const p_24 = Pnt_$ctor_Z7AD9E565(0, 1, 0);
        let result;
        const v_15 = p_24;
        const m_46 = m_45;
        const x_23 = v_15.X;
        const y_23 = v_15.Y;
        const z_23 = v_15.Z;
        result = Pnt_$ctor_Z7AD9E565_1((((m_46.M11 * x_23) + (m_46.M21 * y_23)) + (m_46.M31 * z_23)) + m_46.X41, (((m_46.M12 * x_23) + (m_46.M22 * y_23)) + (m_46.M32 * z_23)) + m_46.Y42, (((m_46.M13 * x_23) + (m_46.M23 * y_23)) + (m_46.M33 * z_23)) + m_46.Z43);
        const a_36 = result;
        const b_33 = Pnt_$ctor_Z7AD9E565(0, 0, 1);
        const same_9 = ((a_38 = a_36, (b_35 = b_33, (x_24 = (a_38.X - b_35.X), (y_24 = (a_38.Y - b_35.Y), (z_24 = (a_38.Z - b_35.Z), Math.sqrt(((x_24 * x_24) + (y_24 * y_24)) + (z_24 * z_24)))))))) < 1E-12;
        if (!same_9) {
            Expect_isTrue(same_9)(`${"90° rotation around X-axis"} expected: 
${Pnt__get_AsString(b_33)}, got: 
${Pnt__get_AsString(a_36)}`);
        }
        else {
            Expect_isTrue(same_9)("90° rotation around X-axis");
        }
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix rotation Y axis", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let a_41, b_39, x_26, y_26, z_26;
        const m_47 = RigidMatrix_createRotationY_5E38073B(90);
        const p_25 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        let result_1;
        const v_16 = p_25;
        const m_48 = m_47;
        const x_25 = v_16.X;
        const y_25 = v_16.Y;
        const z_25 = v_16.Z;
        result_1 = Pnt_$ctor_Z7AD9E565_1((((m_48.M11 * x_25) + (m_48.M21 * y_25)) + (m_48.M31 * z_25)) + m_48.X41, (((m_48.M12 * x_25) + (m_48.M22 * y_25)) + (m_48.M32 * z_25)) + m_48.Y42, (((m_48.M13 * x_25) + (m_48.M23 * y_25)) + (m_48.M33 * z_25)) + m_48.Z43);
        const a_39 = result_1;
        const b_37 = Pnt_$ctor_Z7AD9E565(0, 0, -1);
        const same_10 = ((a_41 = a_39, (b_39 = b_37, (x_26 = (a_41.X - b_39.X), (y_26 = (a_41.Y - b_39.Y), (z_26 = (a_41.Z - b_39.Z), Math.sqrt(((x_26 * x_26) + (y_26 * y_26)) + (z_26 * z_26)))))))) < 1E-12;
        if (!same_10) {
            Expect_isTrue(same_10)(`${"90° rotation around Y-axis"} expected: 
${Pnt__get_AsString(b_37)}, got: 
${Pnt__get_AsString(a_39)}`);
        }
        else {
            Expect_isTrue(same_10)("90° rotation around Y-axis");
        }
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix rotation Z axis", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let a_44, b_43, x_28, y_28, z_28;
        const m_49 = RigidMatrix_createRotationZ_5E38073B(90);
        const p_26 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        let result_2;
        const v_17 = p_26;
        const m_50 = m_49;
        const x_27 = v_17.X;
        const y_27 = v_17.Y;
        const z_27 = v_17.Z;
        result_2 = Pnt_$ctor_Z7AD9E565_1((((m_50.M11 * x_27) + (m_50.M21 * y_27)) + (m_50.M31 * z_27)) + m_50.X41, (((m_50.M12 * x_27) + (m_50.M22 * y_27)) + (m_50.M32 * z_27)) + m_50.Y42, (((m_50.M13 * x_27) + (m_50.M23 * y_27)) + (m_50.M33 * z_27)) + m_50.Z43);
        const a_42 = result_2;
        const b_41 = Pnt_$ctor_Z7AD9E565(0, 1, 0);
        const same_11 = ((a_44 = a_42, (b_43 = b_41, (x_28 = (a_44.X - b_43.X), (y_28 = (a_44.Y - b_43.Y), (z_28 = (a_44.Z - b_43.Z), Math.sqrt(((x_28 * x_28) + (y_28 * y_28)) + (z_28 * z_28)))))))) < 1E-12;
        if (!same_11) {
            Expect_isTrue(same_11)(`${"90° rotation around Z-axis"} expected: 
${Pnt__get_AsString(b_41)}, got: 
${Pnt__get_AsString(a_42)}`);
        }
        else {
            Expect_isTrue(same_11)("90° rotation around Z-axis");
        }
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix rotation arbitrary axis", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        let a_47, b_46, x_31, y_31, z_31;
        const axis_3 = Vec_$ctor_Z7AD9E565(1, 1, 1);
        const m_51 = RigidMatrix_createRotationAxis_Z3D1F83EE(axis_3, 120);
        const p_27 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        let result_3;
        const v_18 = p_27;
        const m_52 = m_51;
        const x_29 = v_18.X;
        const y_29 = v_18.Y;
        const z_29 = v_18.Z;
        result_3 = Pnt_$ctor_Z7AD9E565_1((((m_52.M11 * x_29) + (m_52.M21 * y_29)) + (m_52.M31 * z_29)) + m_52.X41, (((m_52.M12 * x_29) + (m_52.M22 * y_29)) + (m_52.M32 * z_29)) + m_52.Y42, (((m_52.M13 * x_29) + (m_52.M23 * y_29)) + (m_52.M33 * z_29)) + m_52.Z43);
        let resultBack;
        const v_19 = result_3;
        const m_53 = RigidMatrix_multiply_Z31732520_1(m_51, m_51);
        const x_30 = v_19.X;
        const y_30 = v_19.Y;
        const z_30 = v_19.Z;
        resultBack = Pnt_$ctor_Z7AD9E565_1((((m_53.M11 * x_30) + (m_53.M21 * y_30)) + (m_53.M31 * z_30)) + m_53.X41, (((m_53.M12 * x_30) + (m_53.M22 * y_30)) + (m_53.M32 * z_30)) + m_53.Y42, (((m_53.M13 * x_30) + (m_53.M23 * y_30)) + (m_53.M33 * z_30)) + m_53.Z43);
        const a_45 = resultBack;
        const b_44 = p_27;
        const same_12 = ((a_47 = a_45, (b_46 = b_44, (x_31 = (a_47.X - b_46.X), (y_31 = (a_47.Y - b_46.Y), (z_31 = (a_47.Z - b_46.Z), Math.sqrt(((x_31 * x_31) + (y_31 * y_31)) + (z_31 * z_31)))))))) < 1E-12;
        if (!same_12) {
            Expect_isTrue(same_12)(`${"120° rotation around (1,1,1) three times returns to start"} expected: 
${Pnt__get_AsString(b_44)}, got: 
${Pnt__get_AsString(a_45)}`);
        }
        else {
            Expect_isTrue(same_12)("120° rotation around (1,1,1) three times returns to start");
        }
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix rotation with center point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let a_50, b_50, x_33, y_33, z_33;
        const axis_4 = Vec_$ctor_Z7AD9E565_1(0, 0, 1);
        const center = Pnt_$ctor_Z7AD9E565(1, 1, 0);
        const m_54 = RigidMatrix_createRotationAxisCenter_Z655651F(axis_4, center, 90);
        const p_28 = Pnt_$ctor_Z7AD9E565(2, 1, 0);
        let result_4;
        const v_20 = p_28;
        const m_55 = m_54;
        const x_32 = v_20.X;
        const y_32 = v_20.Y;
        const z_32 = v_20.Z;
        result_4 = Pnt_$ctor_Z7AD9E565_1((((m_55.M11 * x_32) + (m_55.M21 * y_32)) + (m_55.M31 * z_32)) + m_55.X41, (((m_55.M12 * x_32) + (m_55.M22 * y_32)) + (m_55.M32 * z_32)) + m_55.Y42, (((m_55.M13 * x_32) + (m_55.M23 * y_32)) + (m_55.M33 * z_32)) + m_55.Z43);
        const a_48 = result_4;
        const b_48 = Pnt_$ctor_Z7AD9E565(1, 2, 0);
        const same_13 = ((a_50 = a_48, (b_50 = b_48, (x_33 = (a_50.X - b_50.X), (y_33 = (a_50.Y - b_50.Y), (z_33 = (a_50.Z - b_50.Z), Math.sqrt(((x_33 * x_33) + (y_33 * y_33)) + (z_33 * z_33)))))))) < 1E-12;
        if (!same_13) {
            Expect_isTrue(same_13)(`${"90° rotation around Z at center (1,1,0)"} expected: 
${Pnt__get_AsString(b_48)}, got: 
${Pnt__get_AsString(a_48)}`);
        }
        else {
            Expect_isTrue(same_13)("90° rotation around Z at center (1,1,0)");
        }
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix vec to vec rotation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        let a_54, b_54, x_38, y_38, z_38;
        const fromVec = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
        const toVec = UnitVec_$ctor_Z7AD9E565(0, 1, 0);
        const m_56 = RigidMatrix_createVecToVec_6319FE20(fromVec, toVec);
        let result_5;
        const v_21 = fromVec;
        const m_57 = m_56;
        const x_36 = v_21.X;
        const y_36 = v_21.Y;
        const z_36 = v_21.Z;
        result_5 = UnitVec_$ctor_Z7AD9E565(((m_57.M11 * x_36) + (m_57.M21 * y_36)) + (m_57.M31 * z_36), ((m_57.M12 * x_36) + (m_57.M22 * y_36)) + (m_57.M32 * z_36), ((m_57.M13 * x_36) + (m_57.M23 * y_36)) + (m_57.M33 * z_36));
        let a_52;
        const v_22 = result_5;
        a_52 = Pnt_$ctor_Z7AD9E565_2(v_22.X, v_22.Y, v_22.Z);
        let b_52;
        const v_23 = toVec;
        b_52 = Pnt_$ctor_Z7AD9E565_2(v_23.X, v_23.Y, v_23.Z);
        const same_14 = ((a_54 = a_52, (b_54 = b_52, (x_38 = (a_54.X - b_54.X), (y_38 = (a_54.Y - b_54.Y), (z_38 = (a_54.Z - b_54.Z), Math.sqrt(((x_38 * x_38) + (y_38 * y_38)) + (z_38 * z_38)))))))) < 1E-12;
        if (!same_14) {
            Expect_isTrue(same_14)(`${"rotation from X to Y axis"} expected: 
${Pnt__get_AsString(b_52)}, got: 
${Pnt__get_AsString(a_52)}`);
        }
        else {
            Expect_isTrue(same_14)("rotation from X to Y axis");
        }
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix vec to vec same direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        let vec;
        const x_39 = 1;
        const y_39 = 2;
        const z_39 = 3;
        const l = Math.sqrt(((x_39 * x_39) + (y_39 * y_39)) + (z_39 * z_39));
        if (!(l > 1E-12)) {
            failUnit3("UnitVec.create", x_39, y_39, z_39);
        }
        const li = 1 / l;
        vec = UnitVec_$ctor_Z7AD9E565(li * x_39, li * y_39, li * z_39);
        const m_58 = RigidMatrix_createVecToVec_6319FE20(vec, vec);
        Expect_isTrue(RigidMatrix__get_IsIdentity(m_58))("same direction should give identity");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix from quaternion identity", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        const q_3 = Quaternion_$ctor_77D16AC0(0, 0, 0, 1);
        const m_59 = RigidMatrix_createFromQuaternion_Z2A007687(q_3);
        Expect_isTrue(RigidMatrix__get_IsIdentity(m_59))("quaternion identity creates matrix identity");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix composition order", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let a_57, b_58, x_45, y_44, z_44, a_60, b_62, x_46, y_45, z_45;
        const t1 = RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(1, 0, 0));
        const r1 = RigidMatrix_createRotationZ_5E38073B(90);
        const p_29 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const m1_1 = RigidMatrix_multiply_Z31732520_1(t1, r1);
        let result1;
        const v_24 = p_29;
        const m_60 = m1_1;
        const x_43 = v_24.X;
        const y_42 = v_24.Y;
        const z_42 = v_24.Z;
        result1 = Pnt_$ctor_Z7AD9E565_1((((m_60.M11 * x_43) + (m_60.M21 * y_42)) + (m_60.M31 * z_42)) + m_60.X41, (((m_60.M12 * x_43) + (m_60.M22 * y_42)) + (m_60.M32 * z_42)) + m_60.Y42, (((m_60.M13 * x_43) + (m_60.M23 * y_42)) + (m_60.M33 * z_42)) + m_60.Z43);
        const m2_1 = RigidMatrix_multiply_Z31732520_1(r1, t1);
        let result2;
        const v_25 = p_29;
        const m_61 = m2_1;
        const x_44 = v_25.X;
        const y_43 = v_25.Y;
        const z_43 = v_25.Z;
        result2 = Pnt_$ctor_Z7AD9E565_1((((m_61.M11 * x_44) + (m_61.M21 * y_43)) + (m_61.M31 * z_43)) + m_61.X41, (((m_61.M12 * x_44) + (m_61.M22 * y_43)) + (m_61.M32 * z_43)) + m_61.Y42, (((m_61.M13 * x_44) + (m_61.M23 * y_43)) + (m_61.M33 * z_43)) + m_61.Z43);
        notEqual(result1, result2, "composition order matters");
        const a_55 = result1;
        const b_56 = Pnt_$ctor_Z7AD9E565(0, 2, 0);
        const same_15 = ((a_57 = a_55, (b_58 = b_56, (x_45 = (a_57.X - b_58.X), (y_44 = (a_57.Y - b_58.Y), (z_44 = (a_57.Z - b_58.Z), Math.sqrt(((x_45 * x_45) + (y_44 * y_44)) + (z_44 * z_44)))))))) < 1E-12;
        if (!same_15) {
            Expect_isTrue(same_15)(`${"translate then rotate"} expected: 
${Pnt__get_AsString(b_56)}, got: 
${Pnt__get_AsString(a_55)}`);
        }
        else {
            Expect_isTrue(same_15)("translate then rotate");
        }
        const a_58 = result2;
        const b_60 = Pnt_$ctor_Z7AD9E565(1, 1, 0);
        const same_16 = ((a_60 = a_58, (b_62 = b_60, (x_46 = (a_60.X - b_62.X), (y_45 = (a_60.Y - b_62.Y), (z_45 = (a_60.Z - b_62.Z), Math.sqrt(((x_46 * x_46) + (y_45 * y_45)) + (z_45 * z_45)))))))) < 1E-12;
        if (!same_16) {
            Expect_isTrue(same_16)(`${"rotate then translate"} expected: 
${Pnt__get_AsString(b_60)}, got: 
${Pnt__get_AsString(a_58)}`);
        }
        else {
            Expect_isTrue(same_16)("rotate then translate");
        }
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix vector transformation ignores translation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        let a_64, b_66, x_48, y_47, z_47;
        const m_62 = RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(10, 20, 30));
        const v_26 = Vec_$ctor_Z7AD9E565(1, 2, 3);
        let result_6;
        const v_27 = v_26;
        const m_63 = m_62;
        const x_47 = v_27.X;
        const y_46 = v_27.Y;
        const z_46 = v_27.Z;
        result_6 = Vec_$ctor_Z7AD9E565_2(((m_63.M11 * x_47) + (m_63.M21 * y_46)) + (m_63.M31 * z_46), ((m_63.M12 * x_47) + (m_63.M22 * y_46)) + (m_63.M32 * z_46), ((m_63.M13 * x_47) + (m_63.M23 * y_46)) + (m_63.M33 * z_46));
        let a_62;
        const v_28 = result_6;
        a_62 = Pnt_$ctor_Z7AD9E565_2(v_28.X, v_28.Y, v_28.Z);
        let b_64;
        const v_29 = v_26;
        b_64 = Pnt_$ctor_Z7AD9E565_2(v_29.X, v_29.Y, v_29.Z);
        const same_17 = ((a_64 = a_62, (b_66 = b_64, (x_48 = (a_64.X - b_66.X), (y_47 = (a_64.Y - b_66.Y), (z_47 = (a_64.Z - b_66.Z), Math.sqrt(((x_48 * x_48) + (y_47 * y_47)) + (z_47 * z_47)))))))) < 1E-12;
        if (!same_17) {
            Expect_isTrue(same_17)(`${"vector transformation ignores translation"} expected: 
${Pnt__get_AsString(b_64)}, got: 
${Pnt__get_AsString(a_62)}`);
        }
        else {
            Expect_isTrue(same_17)("vector transformation ignores translation");
        }
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix unit vector transformation preserves length", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let v_31, v_32;
        const m_64 = RigidMatrix_createRotationAxis_Z3D1F83EE(Vec_$ctor_Z7AD9E565(1, 1, 1), 45);
        const uv = Vec_$ctor_Z7AD9E565_1(1, 2, 3);
        let result_7;
        const v_30 = uv;
        const m_65 = m_64;
        const x_50 = v_30.X;
        const y_49 = v_30.Y;
        const z_49 = v_30.Z;
        result_7 = Vec_$ctor_Z7AD9E565_2(((m_65.M11 * x_50) + (m_65.M21 * y_49)) + (m_65.M31 * z_49), ((m_65.M12 * x_50) + (m_65.M22 * y_49)) + (m_65.M32 * z_49), ((m_65.M13 * x_50) + (m_65.M23 * y_49)) + (m_65.M33 * z_49));
        Expect_floatClose(AccuracyModule_high, (v_31 = result_7, Math.sqrt(((v_31.X * v_31.X) + (v_31.Y * v_31.Y)) + (v_31.Z * v_31.Z))), (v_32 = uv, Math.sqrt(((v_32.X * v_32.X) + (v_32.Y * v_32.Y)) + (v_32.Z * v_32.Z))), "unit vector length preserved");
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix remove translation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        const m_66 = RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(5, 6, 7));
        const m2_2 = RigidMatrix_removeTranslation_Z625426AD(m_66);
        Expect_isTrue(RigidMatrix__get_IsIdentity(m2_2))("remove translation creates identity");
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix add translation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        const m1_2 = RigidMatrix_createRotationZ_5E38073B(45);
        const m2_3 = RigidMatrix_addTranslation(Vec_$ctor_Z7AD9E565(1, 2, 3), m1_2);
        const expected_1 = RigidMatrix_multiply_Z31732520_1(m1_2, RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(1, 2, 3)));
        Expect_isTrue(RigidMatrix_equals(1E-12, m2_3, expected_1))("add translation equivalent to multiplication");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix to/from matrix conversion", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        const rm = RigidMatrix_multiply_Z31732520_1(RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(1, 2, 3)), RigidMatrix_createRotationZ_5E38073B(30));
        const matrix = RigidMatrix__get_Matrix(rm);
        const rmBack = RigidMatrix_createFromMatrix_3CAE9522(matrix);
        Expect_isTrue(RigidMatrix_equals(1E-12, rm, rmBack))("round trip conversion preserves matrix");
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix plane transformations", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        const plane = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1));
        const m_68 = RigidMatrix_createToPlane_2F29039F(plane);
        Expect_isTrue(RigidMatrix__get_IsIdentity(m_68))("to world XY plane is identity");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix plane to plane", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        let a_69, b_72, x_60, y_59, z_59;
        const fromPlane = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1));
        const toPlane = Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_58CBC093(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1), UnitVec_$ctor_Z7AD9E565(1, 0, 0));
        const m_69 = RigidMatrix_createPlaneToPlane_3B6074E0(fromPlane, toPlane);
        const p_30 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        let result_8;
        const v_34 = p_30;
        const m_70 = m_69;
        const x_59 = v_34.X;
        const y_58 = v_34.Y;
        const z_58 = v_34.Z;
        result_8 = Pnt_$ctor_Z7AD9E565_1((((m_70.M11 * x_59) + (m_70.M21 * y_58)) + (m_70.M31 * z_58)) + m_70.X41, (((m_70.M12 * x_59) + (m_70.M22 * y_58)) + (m_70.M32 * z_58)) + m_70.Y42, (((m_70.M13 * x_59) + (m_70.M23 * y_58)) + (m_70.M33 * z_58)) + m_70.Z43);
        const a_67 = result_8;
        const b_70 = Pnt_$ctor_Z7AD9E565(0, 0, 1);
        const same_18 = ((a_69 = a_67, (b_72 = b_70, (x_60 = (a_69.X - b_72.X), (y_59 = (a_69.Y - b_72.Y), (z_59 = (a_69.Z - b_72.Z), Math.sqrt(((x_60 * x_60) + (y_59 * y_59)) + (z_59 * z_59)))))))) < 1E-12;
        if (!same_18) {
            Expect_isTrue(same_18)(`${"transform from XY to YZ plane"} expected: 
${Pnt__get_AsString(b_70)}, got: 
${Pnt__get_AsString(a_67)}`);
        }
        else {
            Expect_isTrue(same_18)("transform from XY to YZ plane");
        }
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix column vectors are orthonormal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        let v_35, v_36, v_37, a_70, b_73, a_71, b_74, a_72, b_75;
        const m_71 = RigidMatrix_createRotationAxis_Z3D1F83EE(Vec_$ctor_Z7AD9E565(1, 2, 3), 42);
        const c1 = RigidMatrix__get_ColumnVector1(m_71);
        const c2 = RigidMatrix__get_ColumnVector2(m_71);
        const c3 = RigidMatrix__get_ColumnVector3(m_71);
        Expect_floatClose(AccuracyModule_high, (v_35 = c1, Math.sqrt(((v_35.X * v_35.X) + (v_35.Y * v_35.Y)) + (v_35.Z * v_35.Z))), 1, "column 1 is unit length");
        Expect_floatClose(AccuracyModule_high, (v_36 = c2, Math.sqrt(((v_36.X * v_36.X) + (v_36.Y * v_36.Y)) + (v_36.Z * v_36.Z))), 1, "column 2 is unit length");
        Expect_floatClose(AccuracyModule_high, (v_37 = c3, Math.sqrt(((v_37.X * v_37.X) + (v_37.Y * v_37.Y)) + (v_37.Z * v_37.Z))), 1, "column 3 is unit length");
        Expect_floatClose(AccuracyModule_high, (a_70 = c1, (b_73 = c2, ((a_70.X * b_73.X) + (a_70.Y * b_73.Y)) + (a_70.Z * b_73.Z))), 0, "columns 1&2 orthogonal");
        Expect_floatClose(AccuracyModule_high, (a_71 = c1, (b_74 = c3, ((a_71.X * b_74.X) + (a_71.Y * b_74.Y)) + (a_71.Z * b_74.Z))), 0, "columns 1&3 orthogonal");
        Expect_floatClose(AccuracyModule_high, (a_72 = c2, (b_75 = c3, ((a_72.X * b_75.X) + (a_72.Y * b_75.Y)) + (a_72.Z * b_75.Z))), 0, "columns 2&3 orthogonal");
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix equals with tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        const m1_3 = RigidMatrix_createRotationZ_5E38073B(45);
        const m2_4 = RigidMatrix_createRotationZ_5E38073B(45.0000001);
        const m3_1 = RigidMatrix_createRotationZ_5E38073B(46);
        Expect_isTrue(RigidMatrix_equals(1E-06, m1_3, m2_4))("matrices equal within tolerance");
        Expect_isFalse(RigidMatrix_equals(1E-06, m1_3, m3_1))("matrices not equal outside tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix zero rotation angle", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        const m1_4 = RigidMatrix_createRotationX_5E38073B(0);
        const m2_5 = RigidMatrix_createRotationY_5E38073B(0);
        const m3_2 = RigidMatrix_createRotationZ_5E38073B(0);
        Expect_isTrue(RigidMatrix__get_IsIdentity(m1_4))("zero X rotation is identity");
        Expect_isTrue(RigidMatrix__get_IsIdentity(m2_5))("zero Y rotation is identity");
        Expect_isTrue(RigidMatrix__get_IsIdentity(m3_2))("zero Z rotation is identity");
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix 360 degree rotation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        let a_78, b_80, x_62, y_61, z_61, a_82, b_83, x_64, y_63, z_63, a_86, b_86, x_66, y_65, z_65;
        const p_31 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        const m1_5 = RigidMatrix_createRotationX_5E38073B(360);
        const m2_6 = RigidMatrix_createRotationY_5E38073B(360);
        const m3_3 = RigidMatrix_createRotationZ_5E38073B(360);
        let a_76;
        const v_38 = p_31;
        const m_72 = m1_5;
        const x_61 = v_38.X;
        const y_60 = v_38.Y;
        const z_60 = v_38.Z;
        a_76 = Pnt_$ctor_Z7AD9E565_1((((m_72.M11 * x_61) + (m_72.M21 * y_60)) + (m_72.M31 * z_60)) + m_72.X41, (((m_72.M12 * x_61) + (m_72.M22 * y_60)) + (m_72.M32 * z_60)) + m_72.Y42, (((m_72.M13 * x_61) + (m_72.M23 * y_60)) + (m_72.M33 * z_60)) + m_72.Z43);
        const b_78 = p_31;
        const same_19 = ((a_78 = a_76, (b_80 = b_78, (x_62 = (a_78.X - b_80.X), (y_61 = (a_78.Y - b_80.Y), (z_61 = (a_78.Z - b_80.Z), Math.sqrt(((x_62 * x_62) + (y_61 * y_61)) + (z_61 * z_61)))))))) < 1E-12;
        if (!same_19) {
            Expect_isTrue(same_19)(`${"360° X rotation returns to start"} expected: 
${Pnt__get_AsString(b_78)}, got: 
${Pnt__get_AsString(a_76)}`);
        }
        else {
            Expect_isTrue(same_19)("360° X rotation returns to start");
        }
        let a_80;
        const v_39 = p_31;
        const m_73 = m2_6;
        const x_63 = v_39.X;
        const y_62 = v_39.Y;
        const z_62 = v_39.Z;
        a_80 = Pnt_$ctor_Z7AD9E565_1((((m_73.M11 * x_63) + (m_73.M21 * y_62)) + (m_73.M31 * z_62)) + m_73.X41, (((m_73.M12 * x_63) + (m_73.M22 * y_62)) + (m_73.M32 * z_62)) + m_73.Y42, (((m_73.M13 * x_63) + (m_73.M23 * y_62)) + (m_73.M33 * z_62)) + m_73.Z43);
        const b_81 = p_31;
        const same_20 = ((a_82 = a_80, (b_83 = b_81, (x_64 = (a_82.X - b_83.X), (y_63 = (a_82.Y - b_83.Y), (z_63 = (a_82.Z - b_83.Z), Math.sqrt(((x_64 * x_64) + (y_63 * y_63)) + (z_63 * z_63)))))))) < 1E-12;
        if (!same_20) {
            Expect_isTrue(same_20)(`${"360° Y rotation returns to start"} expected: 
${Pnt__get_AsString(b_81)}, got: 
${Pnt__get_AsString(a_80)}`);
        }
        else {
            Expect_isTrue(same_20)("360° Y rotation returns to start");
        }
        let a_84;
        const v_40 = p_31;
        const m_74 = m3_3;
        const x_65 = v_40.X;
        const y_64 = v_40.Y;
        const z_64 = v_40.Z;
        a_84 = Pnt_$ctor_Z7AD9E565_1((((m_74.M11 * x_65) + (m_74.M21 * y_64)) + (m_74.M31 * z_64)) + m_74.X41, (((m_74.M12 * x_65) + (m_74.M22 * y_64)) + (m_74.M32 * z_64)) + m_74.Y42, (((m_74.M13 * x_65) + (m_74.M23 * y_64)) + (m_74.M33 * z_64)) + m_74.Z43);
        const b_84 = p_31;
        const same_21 = ((a_86 = a_84, (b_86 = b_84, (x_66 = (a_86.X - b_86.X), (y_65 = (a_86.Y - b_86.Y), (z_65 = (a_86.Z - b_86.Z), Math.sqrt(((x_66 * x_66) + (y_65 * y_65)) + (z_65 * z_65)))))))) < 1E-12;
        if (!same_21) {
            Expect_isTrue(same_21)(`${"360° Z rotation returns to start"} expected: 
${Pnt__get_AsString(b_84)}, got: 
${Pnt__get_AsString(a_84)}`);
        }
        else {
            Expect_isTrue(same_21)("360° Z rotation returns to start");
        }
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix very small rotation angles", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        let a_88, b_88, x_68, y_67, z_67;
        const smallAngle = 1E-10;
        const p_32 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const m_75 = RigidMatrix_createRotationZ_5E38073B(smallAngle);
        let result_9;
        const v_41 = p_32;
        const m_76 = m_75;
        const x_67 = v_41.X;
        const y_66 = v_41.Y;
        const z_66 = v_41.Z;
        result_9 = Pnt_$ctor_Z7AD9E565_1((((m_76.M11 * x_67) + (m_76.M21 * y_66)) + (m_76.M31 * z_66)) + m_76.X41, (((m_76.M12 * x_67) + (m_76.M22 * y_66)) + (m_76.M32 * z_66)) + m_76.Y42, (((m_76.M13 * x_67) + (m_76.M23 * y_66)) + (m_76.M33 * z_66)) + m_76.Z43);
        Expect_floatClose(AccuracyModule_medium, (a_88 = p_32, (b_88 = result_9, (x_68 = (a_88.X - b_88.X), (y_67 = (a_88.Y - b_88.Y), (z_67 = (a_88.Z - b_88.Z), Math.sqrt(((x_68 * x_68) + (y_67 * y_67)) + (z_67 * z_67))))))), 0, "very small rotation preserves approximately same position");
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix large rotation angles", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        let a_91, b_91, x_70, y_69, z_69;
        const largeAngle = 7200;
        const p_33 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const m_77 = RigidMatrix_createRotationZ_5E38073B(largeAngle);
        let result_10;
        const v_42 = p_33;
        const m_78 = m_77;
        const x_69 = v_42.X;
        const y_68 = v_42.Y;
        const z_68 = v_42.Z;
        result_10 = Pnt_$ctor_Z7AD9E565_1((((m_78.M11 * x_69) + (m_78.M21 * y_68)) + (m_78.M31 * z_68)) + m_78.X41, (((m_78.M12 * x_69) + (m_78.M22 * y_68)) + (m_78.M32 * z_68)) + m_78.Y42, (((m_78.M13 * x_69) + (m_78.M23 * y_68)) + (m_78.M33 * z_68)) + m_78.Z43);
        const a_89 = result_10;
        const b_89 = p_33;
        const same_22 = ((a_91 = a_89, (b_91 = b_89, (x_70 = (a_91.X - b_91.X), (y_69 = (a_91.Y - b_91.Y), (z_69 = (a_91.Z - b_91.Z), Math.sqrt(((x_70 * x_70) + (y_69 * y_69)) + (z_69 * z_69)))))))) < 1E-12;
        if (!same_22) {
            Expect_isTrue(same_22)(`${"large angle rotation equivalent to modulo 360"} expected: 
${Pnt__get_AsString(b_89)}, got: 
${Pnt__get_AsString(a_89)}`);
        }
        else {
            Expect_isTrue(same_22)("large angle rotation equivalent to modulo 360");
        }
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix negative rotation angles", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        let a_94, b_95, x_73, y_72, z_72, a_97, b_99, x_74, y_73, z_73;
        const p_34 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const m1_6 = RigidMatrix_createRotationZ_5E38073B(90);
        const m2_7 = RigidMatrix_createRotationZ_5E38073B(-90);
        let result1_1;
        const v_43 = p_34;
        const m_79 = m1_6;
        const x_71 = v_43.X;
        const y_70 = v_43.Y;
        const z_70 = v_43.Z;
        result1_1 = Pnt_$ctor_Z7AD9E565_1((((m_79.M11 * x_71) + (m_79.M21 * y_70)) + (m_79.M31 * z_70)) + m_79.X41, (((m_79.M12 * x_71) + (m_79.M22 * y_70)) + (m_79.M32 * z_70)) + m_79.Y42, (((m_79.M13 * x_71) + (m_79.M23 * y_70)) + (m_79.M33 * z_70)) + m_79.Z43);
        let result2_1;
        const v_44 = p_34;
        const m_80 = m2_7;
        const x_72 = v_44.X;
        const y_71 = v_44.Y;
        const z_71 = v_44.Z;
        result2_1 = Pnt_$ctor_Z7AD9E565_1((((m_80.M11 * x_72) + (m_80.M21 * y_71)) + (m_80.M31 * z_71)) + m_80.X41, (((m_80.M12 * x_72) + (m_80.M22 * y_71)) + (m_80.M32 * z_71)) + m_80.Y42, (((m_80.M13 * x_72) + (m_80.M23 * y_71)) + (m_80.M33 * z_71)) + m_80.Z43);
        const a_92 = result1_1;
        const b_93 = Pnt_$ctor_Z7AD9E565(0, 1, 0);
        const same_23 = ((a_94 = a_92, (b_95 = b_93, (x_73 = (a_94.X - b_95.X), (y_72 = (a_94.Y - b_95.Y), (z_72 = (a_94.Z - b_95.Z), Math.sqrt(((x_73 * x_73) + (y_72 * y_72)) + (z_72 * z_72)))))))) < 1E-12;
        if (!same_23) {
            Expect_isTrue(same_23)(`${"negative rotation is opposite direction"} expected: 
${Pnt__get_AsString(b_93)}, got: 
${Pnt__get_AsString(a_92)}`);
        }
        else {
            Expect_isTrue(same_23)("negative rotation is opposite direction");
        }
        const a_95 = result2_1;
        const b_97 = Pnt_$ctor_Z7AD9E565(0, -1, 0);
        const same_24 = ((a_97 = a_95, (b_99 = b_97, (x_74 = (a_97.X - b_99.X), (y_73 = (a_97.Y - b_99.Y), (z_73 = (a_97.Z - b_99.Z), Math.sqrt(((x_74 * x_74) + (y_73 * y_73)) + (z_73 * z_73)))))))) < 1E-12;
        if (!same_24) {
            Expect_isTrue(same_24)(`${"negative 90° Z rotation"} expected: 
${Pnt__get_AsString(b_97)}, got: 
${Pnt__get_AsString(a_95)}`);
        }
        else {
            Expect_isTrue(same_24)("negative 90° Z rotation");
        }
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix rotation axis too short throws exception", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        const tinyAxis = Vec_$ctor_Z7AD9E565(1E-15, 1E-15, 1E-15);
        Expect_throws(() => {
            RigidMatrix_createRotationAxis_Z3D1F83EE(tinyAxis, 45);
        }, "Should throw for tiny axis");
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix vec to vec opposite vectors throws exception", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        const vec1 = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
        let vec2;
        const v_45 = vec1;
        vec2 = UnitVec_$ctor_Z7AD9E565(-v_45.X, -v_45.Y, -v_45.Z);
        Expect_throws(() => {
            RigidMatrix_createVecToVec_6319FE20(vec1, vec2);
        }, "Should throw for opposite vectors");
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})(), (() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix vec to vec with zero vectors throws exception", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        const zeroVec = Vec_$ctor_Z7AD9E565(0, 0, 0);
        const normalVec = Vec_$ctor_Z7AD9E565(1, 0, 0);
        Expect_throws(() => {
            RigidMatrix_createVecToVec_5A694120(zeroVec, normalVec);
        }, "Should throw for zero from vector");
        Expect_throws(() => {
            RigidMatrix_createVecToVec_5A694120(normalVec, zeroVec);
        }, "Should throw for zero to vector");
        Test_TestCaseBuilder__Zero(builder$0040_31);
    }));
})(), (() => {
    const builder$0040_32 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix createFromMatrix with scaling matrix fails", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_32, Test_TestCaseBuilder__Delay_1505(builder$0040_32, () => {
        const scalingMatrix = Matrix_createScale_Z7AD9E565(2, 2, 2);
        const result_11 = RigidMatrix_tryCreateFromMatrix_3CAE9522(scalingMatrix);
        Expect_isNone(result_11, "scaling matrix should return None");
        Expect_throws(() => {
            RigidMatrix_createFromMatrix_3CAE9522(scalingMatrix);
        }, "Should throw for scaling matrix");
        Test_TestCaseBuilder__Zero(builder$0040_32);
    }));
})(), (() => {
    const builder$0040_33 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix createFromMatrix with projection matrix fails", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_33, Test_TestCaseBuilder__Delay_1505(builder$0040_33, () => {
        const projectionMatrix = Matrix_createPerspective_77D16AC0(90, 1, 0.1, 100);
        const result_12 = RigidMatrix_tryCreateFromMatrix_3CAE9522(projectionMatrix);
        Expect_isNone(result_12, "projection matrix should return None");
        Test_TestCaseBuilder__Zero(builder$0040_33);
    }));
})(), (() => {
    const builder$0040_34 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix createFromMatrix with shear matrix fails", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_34, Test_TestCaseBuilder__Delay_1505(builder$0040_34, () => {
        const shearMatrix = Matrix_$ctor_Z61E40B00(1, 0.5, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1);
        const result_13 = RigidMatrix_tryCreateFromMatrix_3CAE9522(shearMatrix);
        Expect_isNone(result_13, "shear matrix should return None");
        Test_TestCaseBuilder__Zero(builder$0040_34);
    }));
})(), (() => {
    const builder$0040_35 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix very small translation values", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_35, Test_TestCaseBuilder__Delay_1505(builder$0040_35, () => {
        const tinyTranslation = Vec_$ctor_Z7AD9E565(1E-15, 1E-15, 1E-15);
        const m_81 = RigidMatrix_createTranslation_Z394EC5F7(tinyTranslation);
        const p_35 = Pnt_$ctor_Z7AD9E565(1, 1, 1);
        let result_14;
        const v_46 = p_35;
        const m_82 = m_81;
        const x_77 = v_46.X;
        const y_76 = v_46.Y;
        const z_76 = v_46.Z;
        result_14 = Pnt_$ctor_Z7AD9E565_1((((m_82.M11 * x_77) + (m_82.M21 * y_76)) + (m_82.M31 * z_76)) + m_82.X41, (((m_82.M12 * x_77) + (m_82.M22 * y_76)) + (m_82.M32 * z_76)) + m_82.Y42, (((m_82.M13 * x_77) + (m_82.M23 * y_76)) + (m_82.M33 * z_76)) + m_82.Z43);
        Expect_floatClose(AccuracyModule_veryHigh, result_14.X - p_35.X, 1E-15, "tiny translation preserved");
        Test_TestCaseBuilder__Zero(builder$0040_35);
    }));
})(), (() => {
    const builder$0040_36 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix numerical precision with multiple operations", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_36, Test_TestCaseBuilder__Delay_1505(builder$0040_36, () => {
        const p_36 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        let current = p_36;
        const rotation = RigidMatrix_createRotationZ_5E38073B(1);
        Test_TestCaseBuilder__Combine_3A59D1F3(builder$0040_36, Test_TestCaseBuilder__For_Z371464DD(builder$0040_36, rangeDouble(1, 1, 360), (_arg) => {
            let v_47, m_83, x_78, y_77, z_77;
            const _ = _arg | 0;
            current = ((v_47 = current, (m_83 = rotation, (x_78 = v_47.X, (y_77 = v_47.Y, (z_77 = v_47.Z, Pnt_$ctor_Z7AD9E565_1((((m_83.M11 * x_78) + (m_83.M21 * y_77)) + (m_83.M31 * z_77)) + m_83.X41, (((m_83.M12 * x_78) + (m_83.M22 * y_77)) + (m_83.M32 * z_77)) + m_83.Y42, (((m_83.M13 * x_78) + (m_83.M23 * y_77)) + (m_83.M33 * z_77)) + m_83.Z43)))))));
            Test_TestCaseBuilder__Zero(builder$0040_36);
        }), Test_TestCaseBuilder__Delay_1505(builder$0040_36, () => {
            let a_99, b_101, x_79, y_78, z_78;
            Expect_floatClose(AccuracyModule_low, (a_99 = p_36, (b_101 = current, (x_79 = (a_99.X - b_101.X), (y_78 = (a_99.Y - b_101.Y), (z_78 = (a_99.Z - b_101.Z), Math.sqrt(((x_79 * x_79) + (y_78 * y_78)) + (z_78 * z_78))))))), 0, "360 small rotations return approximately to start");
            Test_TestCaseBuilder__Zero(builder$0040_36);
        }));
    }));
})(), (() => {
    const builder$0040_37 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix inverse numerical stability", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_37, Test_TestCaseBuilder__Delay_1505(builder$0040_37, () => {
        const matrices = ofArray([RigidMatrix_createRotationAxis_Z3D1F83EE(Vec_$ctor_Z7AD9E565(1, 1, 1), 179.9), RigidMatrix_createRotationX_5E38073B(0.001), RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(1E-10, 1E-10, 1E-10))]);
        Test_TestCaseBuilder__For_Z371464DD(builder$0040_37, matrices, (_arg_1) => {
            const m_84 = _arg_1;
            const inv_5 = RigidMatrix__get_Inverse(m_84);
            const identity = RigidMatrix_multiply_Z31732520_1(m_84, inv_5);
            Expect_isTrue(RigidMatrix__get_IsIdentity(identity))("inverse is numerically stable");
            Test_TestCaseBuilder__Zero(builder$0040_37);
        });
    }));
})(), (() => {
    const builder$0040_38 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix addTranslation with zero vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_38, Test_TestCaseBuilder__Delay_1505(builder$0040_38, () => {
        const m1_7 = RigidMatrix_createRotationZ_5E38073B(45);
        const m2_8 = RigidMatrix_addTranslation(Vec_$ctor_Z7AD9E565_2(0, 0, 0), m1_7);
        Expect_isTrue(RigidMatrix_equals(1E-15, m1_7, m2_8))("adding zero translation doesn\'t change matrix");
        Test_TestCaseBuilder__Zero(builder$0040_38);
    }));
})(), (() => {
    const builder$0040_39 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix removeTranslation from identity", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_39, Test_TestCaseBuilder__Delay_1505(builder$0040_39, () => {
        const m1_8 = RigidMatrix_get_identity();
        const m2_9 = RigidMatrix_removeTranslation_Z625426AD(m1_8);
        Expect_isTrue(RigidMatrix__get_IsIdentity(m2_9))("removing translation from identity gives identity");
        Test_TestCaseBuilder__Zero(builder$0040_39);
    }));
})(), (() => {
    const builder$0040_40 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix createRotationAxisCenter with origin center", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_40, Test_TestCaseBuilder__Delay_1505(builder$0040_40, () => {
        const axis_5 = Vec_$ctor_Z7AD9E565(0, 0, 1);
        const center_1 = Pnt_$ctor_Z7AD9E565_1(0, 0, 0);
        const angle = 90;
        const m1_9 = RigidMatrix_createRotationAxisCenter_Z655651F(axis_5, center_1, angle);
        const m2_10 = RigidMatrix_createRotationZ_5E38073B(angle);
        Expect_isTrue(RigidMatrix_equals(1E-15, m1_9, m2_10))("rotation around origin same as simple rotation");
        Test_TestCaseBuilder__Zero(builder$0040_40);
    }));
})(), (() => {
    const builder$0040_41 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix equals with zero tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_41, Test_TestCaseBuilder__Delay_1505(builder$0040_41, () => {
        const m1_10 = RigidMatrix_createRotationZ_5E38073B(45);
        const m2_11 = RigidMatrix_createRotationZ_5E38073B(45);
        const m3_4 = RigidMatrix_createRotationZ_5E38073B(45.0000001);
        Expect_isTrue(RigidMatrix_equals(0, m1_10, m2_11))("identical matrices equal with zero tolerance");
        Expect_isFalse(RigidMatrix_equals(0, m1_10, m3_4))("slightly different matrices not equal with zero tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_41);
    }));
})(), (() => {
    const builder$0040_42 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix column vectors form right-handed system", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_42, Test_TestCaseBuilder__Delay_1505(builder$0040_42, () => {
        const m_86 = RigidMatrix_createRotationAxis_Z3D1F83EE(Vec_$ctor_Z7AD9E565(1, 2, 3), 42);
        const c1_1 = RigidMatrix__get_ColumnVector1(m_86);
        const c2_1 = RigidMatrix__get_ColumnVector2(m_86);
        const c3_1 = RigidMatrix__get_ColumnVector3(m_86);
        let cross;
        const a_104 = c1_1;
        const b_106 = c2_1;
        cross = Vec_$ctor_Z7AD9E565_2((a_104.Y * b_106.Z) - (a_104.Z * b_106.Y), (a_104.Z * b_106.X) - (a_104.X * b_106.Z), (a_104.X * b_106.Y) - (a_104.Y * b_106.X));
        let dotWithC3;
        const a_105 = cross;
        const b_107 = c3_1;
        dotWithC3 = (((a_105.X * b_107.X) + (a_105.Y * b_107.Y)) + (a_105.Z * b_107.Z));
        Expect_floatClose(AccuracyModule_high, dotWithC3, 1, "cross product of first two columns equals third (right-handed)");
        Test_TestCaseBuilder__Zero(builder$0040_42);
    }));
})(), (() => {
    const builder$0040_43 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix transformation preserves distances", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_43, Test_TestCaseBuilder__Delay_1505(builder$0040_43, () => {
        const m_87 = RigidMatrix_multiply_Z31732520_1(RigidMatrix_createRotationAxis_Z3D1F83EE(Vec_$ctor_Z7AD9E565(1, 1, 1), 67), RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(5, -3, 2)));
        const p1 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        const p2 = Pnt_$ctor_Z7AD9E565(4, 5, 6);
        let originalDistance;
        const a_107 = p1;
        const b_109 = p2;
        const x_80 = a_107.X - b_109.X;
        const y_79 = a_107.Y - b_109.Y;
        const z_79 = a_107.Z - b_109.Z;
        originalDistance = Math.sqrt(((x_80 * x_80) + (y_79 * y_79)) + (z_79 * z_79));
        let tp1;
        const v_49 = p1;
        const m_88 = m_87;
        const x_81 = v_49.X;
        const y_80 = v_49.Y;
        const z_80 = v_49.Z;
        tp1 = Pnt_$ctor_Z7AD9E565_1((((m_88.M11 * x_81) + (m_88.M21 * y_80)) + (m_88.M31 * z_80)) + m_88.X41, (((m_88.M12 * x_81) + (m_88.M22 * y_80)) + (m_88.M32 * z_80)) + m_88.Y42, (((m_88.M13 * x_81) + (m_88.M23 * y_80)) + (m_88.M33 * z_80)) + m_88.Z43);
        let tp2;
        const v_50 = p2;
        const m_89 = m_87;
        const x_82 = v_50.X;
        const y_81 = v_50.Y;
        const z_81 = v_50.Z;
        tp2 = Pnt_$ctor_Z7AD9E565_1((((m_89.M11 * x_82) + (m_89.M21 * y_81)) + (m_89.M31 * z_81)) + m_89.X41, (((m_89.M12 * x_82) + (m_89.M22 * y_81)) + (m_89.M32 * z_81)) + m_89.Y42, (((m_89.M13 * x_82) + (m_89.M23 * y_81)) + (m_89.M33 * z_81)) + m_89.Z43);
        let transformedDistance;
        const a_109 = tp1;
        const b_111 = tp2;
        const x_83 = a_109.X - b_111.X;
        const y_82 = a_109.Y - b_111.Y;
        const z_82 = a_109.Z - b_111.Z;
        transformedDistance = Math.sqrt(((x_83 * x_83) + (y_82 * y_82)) + (z_82 * z_82));
        Expect_floatClose(AccuracyModule_high, transformedDistance, originalDistance, "rigid transformation preserves distances");
        Test_TestCaseBuilder__Zero(builder$0040_43);
    }));
})(), (() => {
    const builder$0040_44 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix transformation preserves angles", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_44, Test_TestCaseBuilder__Delay_1505(builder$0040_44, () => {
        let a_113, v_51, x_84, y_83, z_83, l_1, f_3, b_115, v_52, x_87, y_85, z_85, l_2, f_4, dot, a_114, b_116, x_90, y_87, z_87, x_91, y_88, z_88, a_118, v_55, x_94, y_91, z_91, l_3, f_5, b_120, v_56, x_97, y_93, z_93, l_4, f_6, dot_1, a_119, b_121, x_100, y_95, z_95, x_101, y_96, z_96;
        const m_90 = RigidMatrix_multiply_Z31732520_1(RigidMatrix_createRotationY_5E38073B(30), RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(1, 2, 3)));
        const v1 = Vec_$ctor_Z7AD9E565(1, 0, 0);
        const v2 = Vec_$ctor_Z7AD9E565(0, 1, 0);
        const originalAngle = 57.29577951308232 * ((a_113 = ((v_51 = v1, (x_84 = v_51.X, (y_83 = v_51.Y, (z_83 = v_51.Z, (l_1 = Math.sqrt(((x_84 * x_84) + (y_83 * y_83)) + (z_83 * z_83)), (!(l_1 > 1E-12) ? failUnit3_1("Vec.Unitized", x_84, y_83, z_83) : undefined, (f_3 = (1 / l_1), UnitVec_$ctor_Z7AD9E565(f_3 * x_84, f_3 * y_83, f_3 * z_83))))))))), (b_115 = ((v_52 = v2, (x_87 = v_52.X, (y_85 = v_52.Y, (z_85 = v_52.Z, (l_2 = Math.sqrt(((x_87 * x_87) + (y_85 * y_85)) + (z_85 * z_85)), (!(l_2 > 1E-12) ? failUnit3_1("Vec.Unitized", x_87, y_85, z_85) : undefined, (f_4 = (1 / l_2), UnitVec_$ctor_Z7AD9E565(f_4 * x_87, f_4 * y_85, f_4 * z_85))))))))), (dot = ((a_114 = a_113, (b_116 = b_115, ((a_114.X * b_116.X) + (a_114.Y * b_116.Y)) + (a_114.Z * b_116.Z)))), ((-0.98 < dot) && (dot < 0.98)) ? Math.acos(dot) : ((dot < 0) ? (3.141592653589793 - (2 * Math.asin(((x_90 = (b_115.X - -a_113.X), (y_87 = (b_115.Y - -a_113.Y), (z_87 = (b_115.Z - -a_113.Z), Math.sqrt(((x_90 * x_90) + (y_87 * y_87)) + (z_87 * z_87)))))) * 0.5))) : (2 * Math.asin(((x_91 = (b_115.X - a_113.X), (y_88 = (b_115.Y - a_113.Y), (z_88 = (b_115.Z - a_113.Z), Math.sqrt(((x_91 * x_91) + (y_88 * y_88)) + (z_88 * z_88)))))) * 0.5)))))));
        let tv1;
        const v_53 = v1;
        const m_91 = m_90;
        const x_92 = v_53.X;
        const y_89 = v_53.Y;
        const z_89 = v_53.Z;
        tv1 = Vec_$ctor_Z7AD9E565_2(((m_91.M11 * x_92) + (m_91.M21 * y_89)) + (m_91.M31 * z_89), ((m_91.M12 * x_92) + (m_91.M22 * y_89)) + (m_91.M32 * z_89), ((m_91.M13 * x_92) + (m_91.M23 * y_89)) + (m_91.M33 * z_89));
        let tv2;
        const v_54 = v2;
        const m_92 = m_90;
        const x_93 = v_54.X;
        const y_90 = v_54.Y;
        const z_90 = v_54.Z;
        tv2 = Vec_$ctor_Z7AD9E565_2(((m_92.M11 * x_93) + (m_92.M21 * y_90)) + (m_92.M31 * z_90), ((m_92.M12 * x_93) + (m_92.M22 * y_90)) + (m_92.M32 * z_90), ((m_92.M13 * x_93) + (m_92.M23 * y_90)) + (m_92.M33 * z_90));
        const transformedAngle = 57.29577951308232 * ((a_118 = ((v_55 = tv1, (x_94 = v_55.X, (y_91 = v_55.Y, (z_91 = v_55.Z, (l_3 = Math.sqrt(((x_94 * x_94) + (y_91 * y_91)) + (z_91 * z_91)), (!(l_3 > 1E-12) ? failUnit3_1("Vec.Unitized", x_94, y_91, z_91) : undefined, (f_5 = (1 / l_3), UnitVec_$ctor_Z7AD9E565(f_5 * x_94, f_5 * y_91, f_5 * z_91))))))))), (b_120 = ((v_56 = tv2, (x_97 = v_56.X, (y_93 = v_56.Y, (z_93 = v_56.Z, (l_4 = Math.sqrt(((x_97 * x_97) + (y_93 * y_93)) + (z_93 * z_93)), (!(l_4 > 1E-12) ? failUnit3_1("Vec.Unitized", x_97, y_93, z_93) : undefined, (f_6 = (1 / l_4), UnitVec_$ctor_Z7AD9E565(f_6 * x_97, f_6 * y_93, f_6 * z_93))))))))), (dot_1 = ((a_119 = a_118, (b_121 = b_120, ((a_119.X * b_121.X) + (a_119.Y * b_121.Y)) + (a_119.Z * b_121.Z)))), ((-0.98 < dot_1) && (dot_1 < 0.98)) ? Math.acos(dot_1) : ((dot_1 < 0) ? (3.141592653589793 - (2 * Math.asin(((x_100 = (b_120.X - -a_118.X), (y_95 = (b_120.Y - -a_118.Y), (z_95 = (b_120.Z - -a_118.Z), Math.sqrt(((x_100 * x_100) + (y_95 * y_95)) + (z_95 * z_95)))))) * 0.5))) : (2 * Math.asin(((x_101 = (b_120.X - a_118.X), (y_96 = (b_120.Y - a_118.Y), (z_96 = (b_120.Z - a_118.Z), Math.sqrt(((x_101 * x_101) + (y_96 * y_96)) + (z_96 * z_96)))))) * 0.5)))))));
        Expect_floatClose(AccuracyModule_high, transformedAngle, originalAngle, "rigid transformation preserves angles");
        Test_TestCaseBuilder__Zero(builder$0040_44);
    }));
})(), (() => {
    const builder$0040_45 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix createRotationAxis with non-unit Vec", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_45, Test_TestCaseBuilder__Delay_1505(builder$0040_45, () => {
        let a_122, b_124, x_107, y_101, z_101;
        const axis_6 = Vec_$ctor_Z7AD9E565(2, 4, 6);
        const m_93 = RigidMatrix_createRotationAxis_Z3D1F83EE(axis_6, 90);
        const p_37 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        let result_15;
        const v_57 = p_37;
        const m_94 = m_93;
        const x_102 = v_57.X;
        const y_97 = v_57.Y;
        const z_97 = v_57.Z;
        result_15 = Pnt_$ctor_Z7AD9E565_1((((m_94.M11 * x_102) + (m_94.M21 * y_97)) + (m_94.M31 * z_97)) + m_94.X41, (((m_94.M12 * x_102) + (m_94.M22 * y_97)) + (m_94.M32 * z_97)) + m_94.Y42, (((m_94.M13 * x_102) + (m_94.M23 * y_97)) + (m_94.M33 * z_97)) + m_94.Z43);
        let unitAxis;
        const x_103 = 2;
        const y_98 = 4;
        const z_98 = 6;
        const l_5 = Math.sqrt(((x_103 * x_103) + (y_98 * y_98)) + (z_98 * z_98));
        if (!(l_5 > 1E-12)) {
            failUnit3("UnitVec.create", x_103, y_98, z_98);
        }
        const li_1 = 1 / l_5;
        unitAxis = UnitVec_$ctor_Z7AD9E565(li_1 * x_103, li_1 * y_98, li_1 * z_98);
        const mUnit = RigidMatrix_createRotationAxis_73D64DB4(unitAxis, 90);
        let resultUnit;
        const v_58 = p_37;
        const m_95 = mUnit;
        const x_106 = v_58.X;
        const y_100 = v_58.Y;
        const z_100 = v_58.Z;
        resultUnit = Pnt_$ctor_Z7AD9E565_1((((m_95.M11 * x_106) + (m_95.M21 * y_100)) + (m_95.M31 * z_100)) + m_95.X41, (((m_95.M12 * x_106) + (m_95.M22 * y_100)) + (m_95.M32 * z_100)) + m_95.Y42, (((m_95.M13 * x_106) + (m_95.M23 * y_100)) + (m_95.M33 * z_100)) + m_95.Z43);
        const a_120 = result_15;
        const b_122 = resultUnit;
        const same_25 = ((a_122 = a_120, (b_124 = b_122, (x_107 = (a_122.X - b_124.X), (y_101 = (a_122.Y - b_124.Y), (z_101 = (a_122.Z - b_124.Z), Math.sqrt(((x_107 * x_107) + (y_101 * y_101)) + (z_101 * z_101)))))))) < 1E-12;
        if (!same_25) {
            Expect_isTrue(same_25)(`${"non-unit axis gives same result as unitized"} expected: 
${Pnt__get_AsString(b_122)}, got: 
${Pnt__get_AsString(a_120)}`);
        }
        else {
            Expect_isTrue(same_25)("non-unit axis gives same result as unitized");
        }
        Test_TestCaseBuilder__Zero(builder$0040_45);
    }));
})(), (() => {
    const builder$0040_46 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix createRotationAxisCenter with non-unit Vec", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_46, Test_TestCaseBuilder__Delay_1505(builder$0040_46, () => {
        const axis_7 = Vec_$ctor_Z7AD9E565(1, 2, 1);
        const center_2 = Pnt_$ctor_Z7AD9E565(2, 3, 4);
        const m_96 = RigidMatrix_createRotationAxisCenter_Z655651F(axis_7, center_2, 45);
        let unitAxis_1;
        const x_108 = 1;
        const y_102 = 2;
        const z_102 = 1;
        const l_6 = Math.sqrt(((x_108 * x_108) + (y_102 * y_102)) + (z_102 * z_102));
        if (!(l_6 > 1E-12)) {
            failUnit3("UnitVec.create", x_108, y_102, z_102);
        }
        const li_2 = 1 / l_6;
        unitAxis_1 = UnitVec_$ctor_Z7AD9E565(li_2 * x_108, li_2 * y_102, li_2 * z_102);
        const mUnit_1 = RigidMatrix_createRotationAxisCenter_976E587(unitAxis_1, center_2, 45);
        Expect_isTrue(RigidMatrix_equals(1E-12, m_96, mUnit_1))("non-unit axis center rotation same as unitized");
        Test_TestCaseBuilder__Zero(builder$0040_46);
    }));
})(), (() => {
    const builder$0040_47 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix createVecToVec with non-unit Vec", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_47, Test_TestCaseBuilder__Delay_1505(builder$0040_47, () => {
        let a_127, b_129, x_112, y_105, z_105;
        const fromVec_1 = Vec_$ctor_Z7AD9E565(3, 0, 0);
        const toVec_1 = Vec_$ctor_Z7AD9E565(0, 5, 0);
        const m_97 = RigidMatrix_createVecToVec_5A694120(fromVec_1, toVec_1);
        let result_16;
        const v_59 = fromVec_1;
        const m_98 = m_97;
        const x_111 = v_59.X;
        const y_104 = v_59.Y;
        const z_104 = v_59.Z;
        result_16 = Vec_$ctor_Z7AD9E565_2(((m_98.M11 * x_111) + (m_98.M21 * y_104)) + (m_98.M31 * z_104), ((m_98.M12 * x_111) + (m_98.M22 * y_104)) + (m_98.M32 * z_104), ((m_98.M13 * x_111) + (m_98.M23 * y_104)) + (m_98.M33 * z_104));
        const expected_12 = Vec_$ctor_Z7AD9E565(0, 3, 0);
        let a_125;
        const v_60 = result_16;
        a_125 = Pnt_$ctor_Z7AD9E565_2(v_60.X, v_60.Y, v_60.Z);
        let b_127;
        const v_61 = expected_12;
        b_127 = Pnt_$ctor_Z7AD9E565_2(v_61.X, v_61.Y, v_61.Z);
        const same_26 = ((a_127 = a_125, (b_129 = b_127, (x_112 = (a_127.X - b_129.X), (y_105 = (a_127.Y - b_129.Y), (z_105 = (a_127.Z - b_129.Z), Math.sqrt(((x_112 * x_112) + (y_105 * y_105)) + (z_105 * z_105)))))))) < 1E-12;
        if (!same_26) {
            Expect_isTrue(same_26)(`${"non-unit vec to vec rotation preserves length"} expected: 
${Pnt__get_AsString(b_127)}, got: 
${Pnt__get_AsString(a_125)}`);
        }
        else {
            Expect_isTrue(same_26)("non-unit vec to vec rotation preserves length");
        }
        Test_TestCaseBuilder__Zero(builder$0040_47);
    }));
})(), (() => {
    const builder$0040_48 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix create with reflection matrix fails", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_48, Test_TestCaseBuilder__Delay_1505(builder$0040_48, () => {
        const reflectX = -1;
        Expect_throws(() => {
            RigidMatrix_create_Z15A9A3C0(reflectX, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0);
        }, "Should throw for reflection matrix");
        Test_TestCaseBuilder__Zero(builder$0040_48);
    }));
})(), (() => {
    const builder$0040_49 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix tryCreateFromMatrix with reflection fails", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_49, Test_TestCaseBuilder__Delay_1505(builder$0040_49, () => {
        const reflectionMatrix = Matrix_$ctor_Z61E40B00(-1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1);
        const result_17 = RigidMatrix_tryCreateFromMatrix_3CAE9522(reflectionMatrix);
        Expect_isNone(result_17, "reflection matrix should return None");
        Test_TestCaseBuilder__Zero(builder$0040_49);
    }));
})(), (() => {
    const builder$0040_50 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix createVecToVec with nearly identical vectors", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_50, Test_TestCaseBuilder__Delay_1505(builder$0040_50, () => {
        const vec1_1 = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
        let vec2_1;
        const l_7 = Math.sqrt(((1 * 1) + (1E-13 * 1E-13)) + (0 * 0));
        if (!(l_7 > 1E-12)) {
            failUnit3("UnitVec.create", 1, 1E-13, 0);
        }
        const li_3 = 1 / l_7;
        vec2_1 = UnitVec_$ctor_Z7AD9E565(li_3 * 1, li_3 * 1E-13, li_3 * 0);
        const m_99 = RigidMatrix_createVecToVec_6319FE20(vec1_1, vec2_1);
        Expect_isTrue(RigidMatrix__get_IsIdentity(m_99))("nearly identical vectors give identity");
        Test_TestCaseBuilder__Zero(builder$0040_50);
    }));
})(), (() => {
    const builder$0040_51 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix createVecToVec Vec overload with nearly identical vectors", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_51, Test_TestCaseBuilder__Delay_1505(builder$0040_51, () => {
        const vec1_2 = Vec_$ctor_Z7AD9E565(5, 0, 0);
        const vec2_2 = Vec_$ctor_Z7AD9E565(5, 1E-13, 0);
        const m_100 = RigidMatrix_createVecToVec_5A694120(vec1_2, vec2_2);
        Expect_isTrue(RigidMatrix__get_IsIdentity(m_100))("nearly identical non-unit vectors give identity");
        Test_TestCaseBuilder__Zero(builder$0040_51);
    }));
})(), (() => {
    const builder$0040_52 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix createTranslationX", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_52, Test_TestCaseBuilder__Delay_1505(builder$0040_52, () => {
        let a_130, b_133, x_118, y_110, z_110;
        const m_101 = RigidMatrix_createTranslationX_5E38073B(5);
        const p_38 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        let result_18;
        const v_62 = p_38;
        const m_102 = m_101;
        const x_117 = v_62.X;
        const y_109 = v_62.Y;
        const z_109 = v_62.Z;
        result_18 = Pnt_$ctor_Z7AD9E565_1((((m_102.M11 * x_117) + (m_102.M21 * y_109)) + (m_102.M31 * z_109)) + m_102.X41, (((m_102.M12 * x_117) + (m_102.M22 * y_109)) + (m_102.M32 * z_109)) + m_102.Y42, (((m_102.M13 * x_117) + (m_102.M23 * y_109)) + (m_102.M33 * z_109)) + m_102.Z43);
        const a_128 = result_18;
        const b_131 = Pnt_$ctor_Z7AD9E565(6, 2, 3);
        const same_27 = ((a_130 = a_128, (b_133 = b_131, (x_118 = (a_130.X - b_133.X), (y_110 = (a_130.Y - b_133.Y), (z_110 = (a_130.Z - b_133.Z), Math.sqrt(((x_118 * x_118) + (y_110 * y_110)) + (z_110 * z_110)))))))) < 1E-12;
        if (!same_27) {
            Expect_isTrue(same_27)(`${"X translation only"} expected: 
${Pnt__get_AsString(b_131)}, got: 
${Pnt__get_AsString(a_128)}`);
        }
        else {
            Expect_isTrue(same_27)("X translation only");
        }
        Test_TestCaseBuilder__Zero(builder$0040_52);
    }));
})(), (() => {
    const builder$0040_53 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix createTranslationY", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_53, Test_TestCaseBuilder__Delay_1505(builder$0040_53, () => {
        let a_133, b_137, x_120, y_112, z_112;
        const m_103 = RigidMatrix_createTranslationY_5E38073B(7);
        const p_39 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        let result_19;
        const v_63 = p_39;
        const m_104 = m_103;
        const x_119 = v_63.X;
        const y_111 = v_63.Y;
        const z_111 = v_63.Z;
        result_19 = Pnt_$ctor_Z7AD9E565_1((((m_104.M11 * x_119) + (m_104.M21 * y_111)) + (m_104.M31 * z_111)) + m_104.X41, (((m_104.M12 * x_119) + (m_104.M22 * y_111)) + (m_104.M32 * z_111)) + m_104.Y42, (((m_104.M13 * x_119) + (m_104.M23 * y_111)) + (m_104.M33 * z_111)) + m_104.Z43);
        const a_131 = result_19;
        const b_135 = Pnt_$ctor_Z7AD9E565(1, 9, 3);
        const same_28 = ((a_133 = a_131, (b_137 = b_135, (x_120 = (a_133.X - b_137.X), (y_112 = (a_133.Y - b_137.Y), (z_112 = (a_133.Z - b_137.Z), Math.sqrt(((x_120 * x_120) + (y_112 * y_112)) + (z_112 * z_112)))))))) < 1E-12;
        if (!same_28) {
            Expect_isTrue(same_28)(`${"Y translation only"} expected: 
${Pnt__get_AsString(b_135)}, got: 
${Pnt__get_AsString(a_131)}`);
        }
        else {
            Expect_isTrue(same_28)("Y translation only");
        }
        Test_TestCaseBuilder__Zero(builder$0040_53);
    }));
})(), (() => {
    const builder$0040_54 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix createTranslationZ", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_54, Test_TestCaseBuilder__Delay_1505(builder$0040_54, () => {
        let a_136, b_141, x_122, y_114, z_114;
        const m_105 = RigidMatrix_createTranslationZ_5E38073B(-2);
        const p_40 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        let result_20;
        const v_64 = p_40;
        const m_106 = m_105;
        const x_121 = v_64.X;
        const y_113 = v_64.Y;
        const z_113 = v_64.Z;
        result_20 = Pnt_$ctor_Z7AD9E565_1((((m_106.M11 * x_121) + (m_106.M21 * y_113)) + (m_106.M31 * z_113)) + m_106.X41, (((m_106.M12 * x_121) + (m_106.M22 * y_113)) + (m_106.M32 * z_113)) + m_106.Y42, (((m_106.M13 * x_121) + (m_106.M23 * y_113)) + (m_106.M33 * z_113)) + m_106.Z43);
        const a_134 = result_20;
        const b_139 = Pnt_$ctor_Z7AD9E565(1, 2, 1);
        const same_29 = ((a_136 = a_134, (b_141 = b_139, (x_122 = (a_136.X - b_141.X), (y_114 = (a_136.Y - b_141.Y), (z_114 = (a_136.Z - b_141.Z), Math.sqrt(((x_122 * x_122) + (y_114 * y_114)) + (z_114 * z_114)))))))) < 1E-12;
        if (!same_29) {
            Expect_isTrue(same_29)(`${"Z translation only"} expected: 
${Pnt__get_AsString(b_139)}, got: 
${Pnt__get_AsString(a_134)}`);
        }
        else {
            Expect_isTrue(same_29)("Z translation only");
        }
        Test_TestCaseBuilder__Zero(builder$0040_54);
    }));
})(), (() => {
    const builder$0040_55 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix addTranslationXYZ", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_55, Test_TestCaseBuilder__Delay_1505(builder$0040_55, () => {
        const m1_11 = RigidMatrix_createRotationZ_5E38073B(45);
        const m2_12 = RigidMatrix_addTranslationXYZ(1, 2, 3, m1_11);
        const expected_13 = RigidMatrix_addTranslation(Vec_$ctor_Z7AD9E565(1, 2, 3), m1_11);
        Expect_isTrue(RigidMatrix_equals(1E-15, m2_12, expected_13))("addTranslationXYZ same as addTranslation Vec");
        Test_TestCaseBuilder__Zero(builder$0040_55);
    }));
})(), (() => {
    const builder$0040_56 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix ToArrayByColumns ordering", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_56, Test_TestCaseBuilder__Delay_1505(builder$0040_56, () => {
        const m_109 = RigidMatrix_get_identity();
        const arr = RigidMatrix__get_ToArrayByColumns(m_109);
        const expected_14 = new Float64Array([1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0]);
        const actual_10 = arr;
        const expected_15 = expected_14;
        if (equalsWith((x_124, y_116) => (x_124 === y_116), actual_10, expected_15) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_10, expected_15, "column-major ordering correct");
        }
        else {
            let valueType;
            let copyOfStruct = actual_10;
            valueType = array_type(float64_type);
            const primitiveTypes = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg;
            if (contains(valueType, primitiveTypes, {
                Equals: equals,
                GetHashCode: (x_125) => (structuralHash(x_125) | 0),
            })) {
                const arg_112 = seqToString(expected_15);
                const arg_1_11 = seqToString(actual_10);
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_112)(arg_1_11)("column-major ordering correct");
            }
            else {
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_15)(actual_10)("column-major ordering correct");
            }
            throw new Exception(errorMsg);
        }
        Test_TestCaseBuilder__Zero(builder$0040_56);
    }));
})(), (() => {
    const builder$0040_57 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix ToArrayByRows ordering", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_57, Test_TestCaseBuilder__Delay_1505(builder$0040_57, () => {
        const m_110 = RigidMatrix_multiply_Z31732520_1(RigidMatrix_createRotationZ_5E38073B(90), RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(1, 2, 3)));
        const arr_1 = RigidMatrix__get_ToArrayByRows(m_110);
        const actual_12 = arr_1.length | 0;
        if ((actual_12 === 12) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_12, 12, "row-major ordering length correct");
        }
        else {
            let valueType_1;
            let copyOfStruct_1 = actual_12;
            valueType_1 = int32_type;
            const primitiveTypes_1 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_1;
            if (contains(valueType_1, primitiveTypes_1, {
                Equals: equals,
                GetHashCode: (x_126) => (structuralHash(x_126) | 0),
            })) {
                const arg_115 = int32ToString(12);
                const arg_1_12 = int32ToString(actual_12);
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_115)(arg_1_12)("row-major ordering length correct");
            }
            else {
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(12)(actual_12)("row-major ordering length correct");
            }
            throw new Exception(errorMsg_1);
        }
        const mBack = RigidMatrix_create_Z15A9A3C0(item(0, arr_1), item(1, arr_1), item(2, arr_1), item(3, arr_1), item(4, arr_1), item(5, arr_1), item(6, arr_1), item(7, arr_1), item(8, arr_1), item(9, arr_1), item(10, arr_1), item(11, arr_1));
        Expect_isTrue(RigidMatrix_equals(1E-15, m_110, mBack))("reconstructed matrix equals original");
        Test_TestCaseBuilder__Zero(builder$0040_57);
    }));
})(), (() => {
    const builder$0040_58 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix Translation property", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_58, Test_TestCaseBuilder__Delay_1505(builder$0040_58, () => {
        let a_142, b_147, x_127, y_119, z_116;
        const trans = Vec_$ctor_Z7AD9E565(10, 20, 30);
        const m_111 = RigidMatrix_createTranslation_Z394EC5F7(trans);
        const result_21 = RigidMatrix__get_Translation(m_111);
        let a_140;
        const v_66 = result_21;
        a_140 = Pnt_$ctor_Z7AD9E565_2(v_66.X, v_66.Y, v_66.Z);
        let b_145;
        const v_67 = trans;
        b_145 = Pnt_$ctor_Z7AD9E565_2(v_67.X, v_67.Y, v_67.Z);
        const same_30 = ((a_142 = a_140, (b_147 = b_145, (x_127 = (a_142.X - b_147.X), (y_119 = (a_142.Y - b_147.Y), (z_116 = (a_142.Z - b_147.Z), Math.sqrt(((x_127 * x_127) + (y_119 * y_119)) + (z_116 * z_116)))))))) < 1E-12;
        if (!same_30) {
            Expect_isTrue(same_30)(`${"Translation property returns correct vector"} expected: 
${Pnt__get_AsString(b_145)}, got: 
${Pnt__get_AsString(a_140)}`);
        }
        else {
            Expect_isTrue(same_30)("Translation property returns correct vector");
        }
        Test_TestCaseBuilder__Zero(builder$0040_58);
    }));
})(), (() => {
    const builder$0040_59 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix UnitVec transformation preserves unit length explicitly", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_59, Test_TestCaseBuilder__Delay_1505(builder$0040_59, () => {
        let copyOfStruct_2, v_69, v_70;
        const m_112 = RigidMatrix_multiply_Z31732520_1(RigidMatrix_createRotationAxis_Z3D1F83EE(Vec_$ctor_Z7AD9E565(1, 1, 1), 67), RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(5, -3, 2)));
        let uv_1;
        const x_128 = 2;
        const y_120 = 3;
        const z_117 = 4;
        const l_8 = Math.sqrt(((x_128 * x_128) + (y_120 * y_120)) + (z_117 * z_117));
        if (!(l_8 > 1E-12)) {
            failUnit3("UnitVec.create", x_128, y_120, z_117);
        }
        const li_4 = 1 / l_8;
        uv_1 = UnitVec_$ctor_Z7AD9E565(li_4 * x_128, li_4 * y_120, li_4 * z_117);
        let result_22;
        const v_68 = uv_1;
        const m_113 = m_112;
        const x_131 = v_68.X;
        const y_122 = v_68.Y;
        const z_119 = v_68.Z;
        result_22 = UnitVec_$ctor_Z7AD9E565(((m_113.M11 * x_131) + (m_113.M21 * y_122)) + (m_113.M31 * z_119), ((m_113.M12 * x_131) + (m_113.M22 * y_122)) + (m_113.M32 * z_119), ((m_113.M13 * x_131) + (m_113.M23 * y_122)) + (m_113.M33 * z_119));
        Expect_floatClose(AccuracyModule_high, (copyOfStruct_2 = ((v_69 = result_22, Vec_$ctor_Z7AD9E565_1(v_69.X, v_69.Y, v_69.Z))), (v_70 = copyOfStruct_2, Math.sqrt(((v_70.X * v_70.X) + (v_70.Y * v_70.Y)) + (v_70.Z * v_70.Z)))), 1, "UnitVec transformation returns UnitVec");
        Test_TestCaseBuilder__Zero(builder$0040_59);
    }));
})(), (() => {
    const builder$0040_60 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix large translation values", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_60, Test_TestCaseBuilder__Delay_1505(builder$0040_60, () => {
        let a_145, b_151, x_134, y_125, z_122;
        const largeTrans = Vec_$ctor_Z7AD9E565(10000000000, -10000000000, 10000000000);
        const m_114 = RigidMatrix_createTranslation_Z394EC5F7(largeTrans);
        const p_41 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        let result_23;
        const v_71 = p_41;
        const m_115 = m_114;
        const x_133 = v_71.X;
        const y_124 = v_71.Y;
        const z_121 = v_71.Z;
        result_23 = Pnt_$ctor_Z7AD9E565_1((((m_115.M11 * x_133) + (m_115.M21 * y_124)) + (m_115.M31 * z_121)) + m_115.X41, (((m_115.M12 * x_133) + (m_115.M22 * y_124)) + (m_115.M32 * z_121)) + m_115.Y42, (((m_115.M13 * x_133) + (m_115.M23 * y_124)) + (m_115.M33 * z_121)) + m_115.Z43);
        const a_143 = result_23;
        const b_149 = Pnt_$ctor_Z7AD9E565(10000000000 + 1, -10000000000 + 2, 10000000000 + 3);
        const same_31 = ((a_145 = a_143, (b_151 = b_149, (x_134 = (a_145.X - b_151.X), (y_125 = (a_145.Y - b_151.Y), (z_122 = (a_145.Z - b_151.Z), Math.sqrt(((x_134 * x_134) + (y_125 * y_125)) + (z_122 * z_122)))))))) < 1E-12;
        if (!same_31) {
            Expect_isTrue(same_31)(`${"large translation works"} expected: 
${Pnt__get_AsString(b_149)}, got: 
${Pnt__get_AsString(a_143)}`);
        }
        else {
            Expect_isTrue(same_31)("large translation works");
        }
        Test_TestCaseBuilder__Zero(builder$0040_60);
    }));
})(), (() => {
    const builder$0040_61 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix multiple matrix compositions", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_61, Test_TestCaseBuilder__Delay_1505(builder$0040_61, () => {
        let a_148, b_154, x_137, y_128, z_125;
        const m1_12 = RigidMatrix_createRotationX_5E38073B(30);
        const m2_13 = RigidMatrix_createRotationY_5E38073B(45);
        const m3_5 = RigidMatrix_createRotationZ_5E38073B(60);
        const m4 = RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(1, 2, 3));
        const combined = RigidMatrix_multiply_Z31732520_1(m1_12, RigidMatrix_multiply_Z31732520_1(m2_13, RigidMatrix_multiply_Z31732520_1(m3_5, m4)));
        const p_42 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        let result_24;
        const v_72 = p_42;
        const m_116 = combined;
        const x_135 = v_72.X;
        const y_126 = v_72.Y;
        const z_123 = v_72.Z;
        result_24 = Pnt_$ctor_Z7AD9E565_1((((m_116.M11 * x_135) + (m_116.M21 * y_126)) + (m_116.M31 * z_123)) + m_116.X41, (((m_116.M12 * x_135) + (m_116.M22 * y_126)) + (m_116.M32 * z_123)) + m_116.Y42, (((m_116.M13 * x_135) + (m_116.M23 * y_126)) + (m_116.M33 * z_123)) + m_116.Z43);
        let expected_19;
        const v_73 = p_42;
        const m_117 = RigidMatrix_multiply_Z31732520_1(m1_12, RigidMatrix_multiply_Z31732520_1(m2_13, RigidMatrix_multiply_Z31732520_1(m3_5, m4)));
        const x_136 = v_73.X;
        const y_127 = v_73.Y;
        const z_124 = v_73.Z;
        expected_19 = Pnt_$ctor_Z7AD9E565_1((((m_117.M11 * x_136) + (m_117.M21 * y_127)) + (m_117.M31 * z_124)) + m_117.X41, (((m_117.M12 * x_136) + (m_117.M22 * y_127)) + (m_117.M32 * z_124)) + m_117.Y42, (((m_117.M13 * x_136) + (m_117.M23 * y_127)) + (m_117.M33 * z_124)) + m_117.Z43);
        const a_146 = result_24;
        const b_152 = expected_19;
        const same_32 = ((a_148 = a_146, (b_154 = b_152, (x_137 = (a_148.X - b_154.X), (y_128 = (a_148.Y - b_154.Y), (z_125 = (a_148.Z - b_154.Z), Math.sqrt(((x_137 * x_137) + (y_128 * y_128)) + (z_125 * z_125)))))))) < 1E-12;
        if (!same_32) {
            Expect_isTrue(same_32)(`${"multiple compositions work correctly"} expected: 
${Pnt__get_AsString(b_152)}, got: 
${Pnt__get_AsString(a_146)}`);
        }
        else {
            Expect_isTrue(same_32)("multiple compositions work correctly");
        }
        Test_TestCaseBuilder__Zero(builder$0040_61);
    }));
})(), (() => {
    const builder$0040_62 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix column vectors extraction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_62, Test_TestCaseBuilder__Delay_1505(builder$0040_62, () => {
        let a_152, b_158, x_138, y_129, z_126;
        const m_118 = RigidMatrix_createRotationZ_5E38073B(45);
        const _c1 = RigidMatrix__get_ColumnVector1(m_118);
        const _c2 = RigidMatrix__get_ColumnVector2(m_118);
        const c3_2 = RigidMatrix__get_ColumnVector3(m_118);
        let a_150;
        const v_74 = c3_2;
        a_150 = Pnt_$ctor_Z7AD9E565_2(v_74.X, v_74.Y, v_74.Z);
        let b_156;
        const v_75 = Vec_$ctor_Z7AD9E565_1(0, 0, 1);
        b_156 = Pnt_$ctor_Z7AD9E565_2(v_75.X, v_75.Y, v_75.Z);
        const same_33 = ((a_152 = a_150, (b_158 = b_156, (x_138 = (a_152.X - b_158.X), (y_129 = (a_152.Y - b_158.Y), (z_126 = (a_152.Z - b_158.Z), Math.sqrt(((x_138 * x_138) + (y_129 * y_129)) + (z_126 * z_126)))))))) < 1E-12;
        if (!same_33) {
            Expect_isTrue(same_33)(`${"third column is Z-axis for Z rotation"} expected: 
${Pnt__get_AsString(b_156)}, got: 
${Pnt__get_AsString(a_150)}`);
        }
        else {
            Expect_isTrue(same_33)("third column is Z-axis for Z rotation");
        }
        Test_TestCaseBuilder__Zero(builder$0040_62);
    }));
})(), (() => {
    const builder$0040_63 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix createVecToVec 90 degree rotation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_63, Test_TestCaseBuilder__Delay_1505(builder$0040_63, () => {
        let a_156, b_162, x_143, y_134, z_131;
        const from = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
        const to_ = UnitVec_$ctor_Z7AD9E565(0, 1, 0);
        const m_119 = RigidMatrix_createVecToVec_6319FE20(from, to_);
        let result_25;
        const v_76 = from;
        const m_120 = m_119;
        const x_141 = v_76.X;
        const y_132 = v_76.Y;
        const z_129 = v_76.Z;
        result_25 = UnitVec_$ctor_Z7AD9E565(((m_120.M11 * x_141) + (m_120.M21 * y_132)) + (m_120.M31 * z_129), ((m_120.M12 * x_141) + (m_120.M22 * y_132)) + (m_120.M32 * z_129), ((m_120.M13 * x_141) + (m_120.M23 * y_132)) + (m_120.M33 * z_129));
        let a_154;
        const v_77 = result_25;
        a_154 = Pnt_$ctor_Z7AD9E565_2(v_77.X, v_77.Y, v_77.Z);
        let b_160;
        const v_78 = to_;
        b_160 = Pnt_$ctor_Z7AD9E565_2(v_78.X, v_78.Y, v_78.Z);
        const same_34 = ((a_156 = a_154, (b_162 = b_160, (x_143 = (a_156.X - b_162.X), (y_134 = (a_156.Y - b_162.Y), (z_131 = (a_156.Z - b_162.Z), Math.sqrt(((x_143 * x_143) + (y_134 * y_134)) + (z_131 * z_131)))))))) < 1E-12;
        if (!same_34) {
            Expect_isTrue(same_34)(`${"90° rotation X to Y"} expected: 
${Pnt__get_AsString(b_160)}, got: 
${Pnt__get_AsString(a_154)}`);
        }
        else {
            Expect_isTrue(same_34)("90° rotation X to Y");
        }
        Test_TestCaseBuilder__Zero(builder$0040_63);
    }));
})(), (() => {
    const builder$0040_64 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix createVecToVec 180 degree rotation alternative", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_64, Test_TestCaseBuilder__Delay_1505(builder$0040_64, () => {
        let a_158, v_80, b_164, v_81, x_152, y_141, z_138;
        let from_1;
        const x_144 = 1;
        const y_135 = 0;
        const z_132 = 0;
        const l_9 = Math.sqrt(((x_144 * x_144) + (y_135 * y_135)) + (z_132 * z_132));
        if (!(l_9 > 1E-12)) {
            failUnit3("UnitVec.create", x_144, y_135, z_132);
        }
        const li_5 = 1 / l_9;
        from_1 = UnitVec_$ctor_Z7AD9E565(li_5 * x_144, li_5 * y_135, li_5 * z_132);
        let to__1;
        const z_134 = 0;
        const l_10 = Math.sqrt(((-0.99999 * -0.99999) + (1E-05 * 1E-05)) + (z_134 * z_134));
        if (!(l_10 > 1E-12)) {
            failUnit3("UnitVec.create", -0.99999, 1E-05, z_134);
        }
        const li_6 = 1 / l_10;
        to__1 = UnitVec_$ctor_Z7AD9E565(li_6 * -0.99999, li_6 * 1E-05, li_6 * z_134);
        const m_121 = RigidMatrix_createVecToVec_6319FE20(from_1, to__1);
        let result_26;
        const v_79 = from_1;
        const m_122 = m_121;
        const x_150 = v_79.X;
        const y_139 = v_79.Y;
        const z_136 = v_79.Z;
        result_26 = UnitVec_$ctor_Z7AD9E565(((m_122.M11 * x_150) + (m_122.M21 * y_139)) + (m_122.M31 * z_136), ((m_122.M12 * x_150) + (m_122.M22 * y_139)) + (m_122.M32 * z_136), ((m_122.M13 * x_150) + (m_122.M23 * y_139)) + (m_122.M33 * z_136));
        Expect_floatClose(AccuracyModule_low, (a_158 = ((v_80 = result_26, Pnt_$ctor_Z7AD9E565_2(v_80.X, v_80.Y, v_80.Z))), (b_164 = ((v_81 = to__1, Pnt_$ctor_Z7AD9E565_2(v_81.X, v_81.Y, v_81.Z))), (x_152 = (a_158.X - b_164.X), (y_141 = (a_158.Y - b_164.Y), (z_138 = (a_158.Z - b_164.Z), Math.sqrt(((x_152 * x_152) + (y_141 * y_141)) + (z_138 * z_138))))))), 0, "near-180° rotation works");
        Test_TestCaseBuilder__Zero(builder$0040_64);
    }));
})(), (() => {
    const builder$0040_65 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix createVecToVec with perpendicular vectors", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_65, Test_TestCaseBuilder__Delay_1505(builder$0040_65, () => {
        let a_162, b_168, x_157, y_146, z_143;
        const from_2 = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
        const to__2 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
        const m_123 = RigidMatrix_createVecToVec_6319FE20(from_2, to__2);
        let result_27;
        const v_82 = from_2;
        const m_124 = m_123;
        const x_155 = v_82.X;
        const y_144 = v_82.Y;
        const z_141 = v_82.Z;
        result_27 = UnitVec_$ctor_Z7AD9E565(((m_124.M11 * x_155) + (m_124.M21 * y_144)) + (m_124.M31 * z_141), ((m_124.M12 * x_155) + (m_124.M22 * y_144)) + (m_124.M32 * z_141), ((m_124.M13 * x_155) + (m_124.M23 * y_144)) + (m_124.M33 * z_141));
        let a_160;
        const v_83 = result_27;
        a_160 = Pnt_$ctor_Z7AD9E565_2(v_83.X, v_83.Y, v_83.Z);
        let b_166;
        const v_84 = to__2;
        b_166 = Pnt_$ctor_Z7AD9E565_2(v_84.X, v_84.Y, v_84.Z);
        const same_35 = ((a_162 = a_160, (b_168 = b_166, (x_157 = (a_162.X - b_168.X), (y_146 = (a_162.Y - b_168.Y), (z_143 = (a_162.Z - b_168.Z), Math.sqrt(((x_157 * x_157) + (y_146 * y_146)) + (z_143 * z_143)))))))) < 1E-12;
        if (!same_35) {
            Expect_isTrue(same_35)(`${"perpendicular rotation X to Z"} expected: 
${Pnt__get_AsString(b_166)}, got: 
${Pnt__get_AsString(a_160)}`);
        }
        else {
            Expect_isTrue(same_35)("perpendicular rotation X to Z");
        }
        Test_TestCaseBuilder__Zero(builder$0040_65);
    }));
})(), (() => {
    const builder$0040_66 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix createVecToVec with arbitrary unit vectors", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_66, Test_TestCaseBuilder__Delay_1505(builder$0040_66, () => {
        let a_166, b_172, x_166, y_153, z_150;
        let from_3;
        const x_158 = 1;
        const y_147 = 2;
        const z_144 = 3;
        const l_11 = Math.sqrt(((x_158 * x_158) + (y_147 * y_147)) + (z_144 * z_144));
        if (!(l_11 > 1E-12)) {
            failUnit3("UnitVec.create", x_158, y_147, z_144);
        }
        const li_7 = 1 / l_11;
        from_3 = UnitVec_$ctor_Z7AD9E565(li_7 * x_158, li_7 * y_147, li_7 * z_144);
        let to__3;
        const x_161 = 3;
        const y_149 = 1;
        const z_146 = 2;
        const l_12 = Math.sqrt(((x_161 * x_161) + (y_149 * y_149)) + (z_146 * z_146));
        if (!(l_12 > 1E-12)) {
            failUnit3("UnitVec.create", x_161, y_149, z_146);
        }
        const li_8 = 1 / l_12;
        to__3 = UnitVec_$ctor_Z7AD9E565(li_8 * x_161, li_8 * y_149, li_8 * z_146);
        const m_125 = RigidMatrix_createVecToVec_6319FE20(from_3, to__3);
        let result_28;
        const v_85 = from_3;
        const m_126 = m_125;
        const x_164 = v_85.X;
        const y_151 = v_85.Y;
        const z_148 = v_85.Z;
        result_28 = UnitVec_$ctor_Z7AD9E565(((m_126.M11 * x_164) + (m_126.M21 * y_151)) + (m_126.M31 * z_148), ((m_126.M12 * x_164) + (m_126.M22 * y_151)) + (m_126.M32 * z_148), ((m_126.M13 * x_164) + (m_126.M23 * y_151)) + (m_126.M33 * z_148));
        let a_164;
        const v_86 = result_28;
        a_164 = Pnt_$ctor_Z7AD9E565_2(v_86.X, v_86.Y, v_86.Z);
        let b_170;
        const v_87 = to__3;
        b_170 = Pnt_$ctor_Z7AD9E565_2(v_87.X, v_87.Y, v_87.Z);
        const same_36 = ((a_166 = a_164, (b_172 = b_170, (x_166 = (a_166.X - b_172.X), (y_153 = (a_166.Y - b_172.Y), (z_150 = (a_166.Z - b_172.Z), Math.sqrt(((x_166 * x_166) + (y_153 * y_153)) + (z_150 * z_150)))))))) < 1E-12;
        if (!same_36) {
            Expect_isTrue(same_36)(`${"arbitrary unit vector rotation"} expected: 
${Pnt__get_AsString(b_170)}, got: 
${Pnt__get_AsString(a_164)}`);
        }
        else {
            Expect_isTrue(same_36)("arbitrary unit vector rotation");
        }
        Test_TestCaseBuilder__Zero(builder$0040_66);
    }));
})(), (() => {
    const builder$0040_67 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix createVecToVec Vec overload preserves orthogonality", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_67, Test_TestCaseBuilder__Delay_1505(builder$0040_67, () => {
        let a_167, b_173;
        const from_4 = Vec_$ctor_Z7AD9E565(2, 0, 0);
        const to__4 = Vec_$ctor_Z7AD9E565(0, 3, 0);
        const m_127 = RigidMatrix_createVecToVec_5A694120(from_4, to__4);
        const perp = Vec_$ctor_Z7AD9E565(0, 0, 1);
        let rotatedPerp;
        const v_88 = perp;
        const m_128 = m_127;
        const x_167 = v_88.X;
        const y_154 = v_88.Y;
        const z_151 = v_88.Z;
        rotatedPerp = Vec_$ctor_Z7AD9E565_2(((m_128.M11 * x_167) + (m_128.M21 * y_154)) + (m_128.M31 * z_151), ((m_128.M12 * x_167) + (m_128.M22 * y_154)) + (m_128.M32 * z_151), ((m_128.M13 * x_167) + (m_128.M23 * y_154)) + (m_128.M33 * z_151));
        Expect_floatClose(AccuracyModule_high, (a_167 = from_4, (b_173 = rotatedPerp, ((a_167.X * b_173.X) + (a_167.Y * b_173.Y)) + (a_167.Z * b_173.Z))), 0, "perpendicular vector stays perpendicular");
        Test_TestCaseBuilder__Zero(builder$0040_67);
    }));
})(), (() => {
    const builder$0040_68 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix createVecToVec small angle rotation numerical stability", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_68, Test_TestCaseBuilder__Delay_1505(builder$0040_68, () => {
        let a_169, v_90, b_175, v_91, x_174, y_160, z_157;
        const from_5 = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
        let to__5;
        const l_13 = Math.sqrt(((0.99999999 * 0.99999999) + (0.0001 * 0.0001)) + (0 * 0));
        if (!(l_13 > 1E-12)) {
            failUnit3("UnitVec.create", 0.99999999, 0.0001, 0);
        }
        const li_9 = 1 / l_13;
        to__5 = UnitVec_$ctor_Z7AD9E565(li_9 * 0.99999999, li_9 * 0.0001, li_9 * 0);
        const m_129 = RigidMatrix_createVecToVec_6319FE20(from_5, to__5);
        let result_29;
        const v_89 = from_5;
        const m_130 = m_129;
        const x_172 = v_89.X;
        const y_158 = v_89.Y;
        const z_155 = v_89.Z;
        result_29 = UnitVec_$ctor_Z7AD9E565(((m_130.M11 * x_172) + (m_130.M21 * y_158)) + (m_130.M31 * z_155), ((m_130.M12 * x_172) + (m_130.M22 * y_158)) + (m_130.M32 * z_155), ((m_130.M13 * x_172) + (m_130.M23 * y_158)) + (m_130.M33 * z_155));
        Expect_floatClose(AccuracyModule_medium, (a_169 = ((v_90 = result_29, Pnt_$ctor_Z7AD9E565_2(v_90.X, v_90.Y, v_90.Z))), (b_175 = ((v_91 = to__5, Pnt_$ctor_Z7AD9E565_2(v_91.X, v_91.Y, v_91.Z))), (x_174 = (a_169.X - b_175.X), (y_160 = (a_169.Y - b_175.Y), (z_157 = (a_169.Z - b_175.Z), Math.sqrt(((x_174 * x_174) + (y_160 * y_160)) + (z_157 * z_157))))))), 0, "small angle rotation numerically stable");
        Test_TestCaseBuilder__Zero(builder$0040_68);
    }));
})(), (() => {
    const builder$0040_69 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix createVecToVec with different length vectors", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_69, Test_TestCaseBuilder__Delay_1505(builder$0040_69, () => {
        let v_93, a_173, b_179, x_182, y_166, z_163;
        const from_6 = Vec_$ctor_Z7AD9E565(5, 0, 0);
        const to__6 = Vec_$ctor_Z7AD9E565(0, 0, 3);
        const m_131 = RigidMatrix_createVecToVec_5A694120(from_6, to__6);
        let result_30;
        const v_92 = from_6;
        const m_132 = m_131;
        const x_175 = v_92.X;
        const y_161 = v_92.Y;
        const z_158 = v_92.Z;
        result_30 = Vec_$ctor_Z7AD9E565_2(((m_132.M11 * x_175) + (m_132.M21 * y_161)) + (m_132.M31 * z_158), ((m_132.M12 * x_175) + (m_132.M22 * y_161)) + (m_132.M32 * z_158), ((m_132.M13 * x_175) + (m_132.M23 * y_161)) + (m_132.M33 * z_158));
        Expect_floatClose(AccuracyModule_high, (v_93 = result_30, Math.sqrt(((v_93.X * v_93.X) + (v_93.Y * v_93.Y)) + (v_93.Z * v_93.Z))), 5, "rotation preserves from vector length");
        let resultDir;
        const v_94 = result_30;
        const x_176 = v_94.X;
        const y_162 = v_94.Y;
        const z_159 = v_94.Z;
        const l_14 = Math.sqrt(((x_176 * x_176) + (y_162 * y_162)) + (z_159 * z_159));
        if (!(l_14 > 1E-12)) {
            failUnit3_1("Vec.Unitized", x_176, y_162, z_159);
        }
        const f_7 = 1 / l_14;
        resultDir = UnitVec_$ctor_Z7AD9E565(f_7 * x_176, f_7 * y_162, f_7 * z_159);
        let toDir;
        const v_95 = to__6;
        const x_179 = v_95.X;
        const y_164 = v_95.Y;
        const z_161 = v_95.Z;
        const l_15 = Math.sqrt(((x_179 * x_179) + (y_164 * y_164)) + (z_161 * z_161));
        if (!(l_15 > 1E-12)) {
            failUnit3_1("Vec.Unitized", x_179, y_164, z_161);
        }
        const f_8 = 1 / l_15;
        toDir = UnitVec_$ctor_Z7AD9E565(f_8 * x_179, f_8 * y_164, f_8 * z_161);
        let a_171;
        const v_96 = resultDir;
        a_171 = Pnt_$ctor_Z7AD9E565_2(v_96.X, v_96.Y, v_96.Z);
        let b_177;
        const v_97 = toDir;
        b_177 = Pnt_$ctor_Z7AD9E565_2(v_97.X, v_97.Y, v_97.Z);
        const same_37 = ((a_173 = a_171, (b_179 = b_177, (x_182 = (a_173.X - b_179.X), (y_166 = (a_173.Y - b_179.Y), (z_163 = (a_173.Z - b_179.Z), Math.sqrt(((x_182 * x_182) + (y_166 * y_166)) + (z_163 * z_163)))))))) < 1E-12;
        if (!same_37) {
            Expect_isTrue(same_37)(`${"rotation aligns direction"} expected: 
${Pnt__get_AsString(b_177)}, got: 
${Pnt__get_AsString(a_171)}`);
        }
        else {
            Expect_isTrue(same_37)("rotation aligns direction");
        }
        Test_TestCaseBuilder__Zero(builder$0040_69);
    }));
})(), (() => {
    const builder$0040_70 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix createVecToVec is transpose of reverse rotation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_70, Test_TestCaseBuilder__Delay_1505(builder$0040_70, () => {
        let from_7;
        const x_183 = 1;
        const y_167 = 1;
        const z_164 = 0;
        const l_16 = Math.sqrt(((x_183 * x_183) + (y_167 * y_167)) + (z_164 * z_164));
        if (!(l_16 > 1E-12)) {
            failUnit3("UnitVec.create", x_183, y_167, z_164);
        }
        const li_10 = 1 / l_16;
        from_7 = UnitVec_$ctor_Z7AD9E565(li_10 * x_183, li_10 * y_167, li_10 * z_164);
        let to__7;
        const x_186 = 0;
        const y_169 = 1;
        const z_166 = 1;
        const l_17 = Math.sqrt(((x_186 * x_186) + (y_169 * y_169)) + (z_166 * z_166));
        if (!(l_17 > 1E-12)) {
            failUnit3("UnitVec.create", x_186, y_169, z_166);
        }
        const li_11 = 1 / l_17;
        to__7 = UnitVec_$ctor_Z7AD9E565(li_11 * x_186, li_11 * y_169, li_11 * z_166);
        const m1_13 = RigidMatrix_createVecToVec_6319FE20(from_7, to__7);
        const m2_14 = RigidMatrix_createVecToVec_6319FE20(to__7, from_7);
        const composed = RigidMatrix_multiply_Z31732520_1(m1_13, m2_14);
        Expect_isTrue(RigidMatrix__get_IsIdentity(composed))("forward then reverse gives identity");
        Test_TestCaseBuilder__Zero(builder$0040_70);
    }));
})(), (() => {
    const builder$0040_71 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RigidMatrix createVecToVec maintains handedness", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_71, Test_TestCaseBuilder__Delay_1505(builder$0040_71, () => {
        let a_174, b_180;
        const from_8 = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
        const to__8 = UnitVec_$ctor_Z7AD9E565(0, 1, 0);
        const m_133 = RigidMatrix_createVecToVec_6319FE20(from_8, to__8);
        let zTransformed;
        const v_98 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
        const m_134 = m_133;
        const x_191 = v_98.X;
        const y_173 = v_98.Y;
        const z_170 = v_98.Z;
        zTransformed = UnitVec_$ctor_Z7AD9E565(((m_134.M11 * x_191) + (m_134.M21 * y_173)) + (m_134.M31 * z_170), ((m_134.M12 * x_191) + (m_134.M22 * y_173)) + (m_134.M32 * z_170), ((m_134.M13 * x_191) + (m_134.M23 * y_173)) + (m_134.M33 * z_170));
        Expect_floatClose(AccuracyModule_high, (a_174 = zTransformed, (b_180 = UnitVec_$ctor_Z7AD9E565(0, 0, 1), ((a_174.X * b_180.X) + (a_174.Y * b_180.Y)) + (a_174.Z * b_180.Z))), 1, "maintains right-handed coordinate system");
        Test_TestCaseBuilder__Zero(builder$0040_71);
    }));
})()]));

