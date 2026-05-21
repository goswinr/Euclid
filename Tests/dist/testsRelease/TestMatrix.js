
import { AccuracyModule_medium, AccuracyModule_high, AccuracyModule_low, Expect_floatClose, Expect_throws, Expect_isFalse, Test_TestCaseBuilder__Zero, Expect_isTrue, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Matrix_createPlaneToPlane_3B6074E0, Matrix_createMirror_2F29039F, Matrix__get_IsProjecting, Matrix_createPerspective_77D16AC0, Matrix_createVecToVec_5A694120, Matrix_createFromColumMajorArray_52AF8430, Matrix_createFromRowMajorArray_52AF8430, Matrix__get_ToArrayByColumns, Matrix__get_ToArrayByRows, Matrix_$ctor_Z61E40B00, Matrix_transpose_3CAE9522, Matrix_equals, Matrix__get_ColumnVector3, Matrix__get_ColumnVector2, Matrix__get_ColumnVector1, Matrix__get_IsReflecting, Matrix__get_IsMirroring, Matrix__get_IsOnlyTranslating, Matrix__get_IsTranslating, Matrix__get_IsScaling, Matrix__get_IsOrthogonal, Matrix__get_IsAffine, Matrix__get_IsIdentity, Matrix_createVecToVec_6319FE20, Matrix__get_Determinant, Matrix_createShear_76A78260, Matrix_createRotationAxisCenter_Z655651F, Matrix_createRotationAxis_Z3D1F83EE, Matrix_createRotationZ_5E38073B, Matrix_createRotationY_5E38073B, Matrix_createRotationX_5E38073B, Matrix_createScale_Z7AD9E565, Matrix_createFromQuaternion_Z2A007687, Matrix__get_Inverse, Matrix_get_identity, Matrix_multiply_Z11D053C0, Matrix_createTranslation_Z394EC5F7, Matrix_addTranslation } from "./Src/Matrix.js";
import { Vec_$ctor_Z7AD9E565 } from "./Src/Vec.js";
import { Pnt__get_AsString, Pnt_$ctor_Z7AD9E565 } from "./Src/Pnt.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_1 } from "./Src/Pnt.js";
import { Quaternion_createFromRadians_Z3D1F83EE } from "./Src/Quaternion.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_1 } from "./Src/Vec.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_2 } from "./Src/Vec.js";
import { Matrix_multiply_Z11D053C0 as Matrix_multiply_Z11D053C0_1 } from "./Src/Matrix.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_2 } from "./Src/Pnt.js";
import { UnitVec_$ctor_Z7AD9E565 } from "./Src/UnitVec.js";
import { failUnit3 } from "./Src/EuclidErrors.js";
import { failUnit3 as failUnit3_1 } from "./Src/EuclidErrors.js";
import { initialize } from "./fable_modules/fable-library-js.5.0.0/Array.js";
import { PPlane_$ctor_3CB4665C } from "./Src/PPlane.js";
import { ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";

export const tests = Test_testList("Matrix transformations", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix translate ", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let a_3, b_4, x_1, y_1, z_1, a_6, b_8, x_3, y_3, z_3, a_9, b_12, x_5, y_5, z_5, a_12, b_16, x_7, y_7, z_7;
        const m_1 = Matrix_addTranslation(Vec_$ctor_Z7AD9E565(4, 1, -1), Matrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(2, 3, 4)));
        const a = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        let a_1;
        const p = a;
        const m_2 = m_1;
        const x = p.X;
        const y = p.Y;
        const z = p.Z;
        const sc = 1 / ((((m_2.M14 * x) + (m_2.M24 * y)) + (m_2.M34 * z)) + m_2.M44);
        a_1 = Pnt_$ctor_Z7AD9E565_1(((((m_2.M11 * x) + (m_2.M21 * y)) + (m_2.M31 * z)) + m_2.X41) * sc, ((((m_2.M12 * x) + (m_2.M22 * y)) + (m_2.M32 * z)) + m_2.Y42) * sc, ((((m_2.M13 * x) + (m_2.M23 * y)) + (m_2.M33 * z)) + m_2.Z43) * sc);
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
        let a_4;
        const p_4 = a;
        const m_6 = m_1;
        const x_2 = p_4.X;
        const y_2 = p_4.Y;
        const z_2 = p_4.Z;
        const sc_1 = 1 / ((((m_6.M14 * x_2) + (m_6.M24 * y_2)) + (m_6.M34 * z_2)) + m_6.M44);
        a_4 = Pnt_$ctor_Z7AD9E565_1(((((m_6.M11 * x_2) + (m_6.M21 * y_2)) + (m_6.M31 * z_2)) + m_6.X41) * sc_1, ((((m_6.M12 * x_2) + (m_6.M22 * y_2)) + (m_6.M32 * z_2)) + m_6.Y42) * sc_1, ((((m_6.M13 * x_2) + (m_6.M23 * y_2)) + (m_6.M33 * z_2)) + m_6.Z43) * sc_1);
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
        const m3 = Matrix_multiply_Z11D053C0(Matrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(2, 3, 4)), Matrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(4, 1, -1)));
        let a_7;
        const p_5 = a;
        const m_7 = m3;
        const x_4 = p_5.X;
        const y_4 = p_5.Y;
        const z_4 = p_5.Z;
        const sc_2 = 1 / ((((m_7.M14 * x_4) + (m_7.M24 * y_4)) + (m_7.M34 * z_4)) + m_7.M44);
        a_7 = Pnt_$ctor_Z7AD9E565_1(((((m_7.M11 * x_4) + (m_7.M21 * y_4)) + (m_7.M31 * z_4)) + m_7.X41) * sc_2, ((((m_7.M12 * x_4) + (m_7.M22 * y_4)) + (m_7.M32 * z_4)) + m_7.Y42) * sc_2, ((((m_7.M13 * x_4) + (m_7.M23 * y_4)) + (m_7.M33 * z_4)) + m_7.Z43) * sc_2);
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
        let a_10;
        const p_9 = a;
        const m_11 = m3;
        const x_6 = p_9.X;
        const y_6 = p_9.Y;
        const z_6 = p_9.Z;
        const sc_3 = 1 / ((((m_11.M14 * x_6) + (m_11.M24 * y_6)) + (m_11.M34 * z_6)) + m_11.M44);
        a_10 = Pnt_$ctor_Z7AD9E565_1(((((m_11.M11 * x_6) + (m_11.M21 * y_6)) + (m_11.M31 * z_6)) + m_11.X41) * sc_3, ((((m_11.M12 * x_6) + (m_11.M22 * y_6)) + (m_11.M32 * z_6)) + m_11.Y42) * sc_3, ((((m_11.M13 * x_6) + (m_11.M23 * y_6)) + (m_11.M33 * z_6)) + m_11.Z43) * sc_3);
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
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix inverse id ", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let a_16, b_19, x_10, y_10, z_10;
        const m_12 = Matrix_get_identity();
        const inv = Matrix__get_Inverse(m_12);
        const a_13 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        let a_14;
        let p_11;
        const p_10 = a_13;
        const m_13 = m_12;
        const x_8 = p_10.X;
        const y_8 = p_10.Y;
        const z_8 = p_10.Z;
        const sc_4 = 1 / ((((m_13.M14 * x_8) + (m_13.M24 * y_8)) + (m_13.M34 * z_8)) + m_13.M44);
        p_11 = Pnt_$ctor_Z7AD9E565_1(((((m_13.M11 * x_8) + (m_13.M21 * y_8)) + (m_13.M31 * z_8)) + m_13.X41) * sc_4, ((((m_13.M12 * x_8) + (m_13.M22 * y_8)) + (m_13.M32 * z_8)) + m_13.Y42) * sc_4, ((((m_13.M13 * x_8) + (m_13.M23 * y_8)) + (m_13.M33 * z_8)) + m_13.Z43) * sc_4);
        const m_14 = inv;
        const x_9 = p_11.X;
        const y_9 = p_11.Y;
        const z_9 = p_11.Z;
        const sc_5 = 1 / ((((m_14.M14 * x_9) + (m_14.M24 * y_9)) + (m_14.M34 * z_9)) + m_14.M44);
        a_14 = Pnt_$ctor_Z7AD9E565_1(((((m_14.M11 * x_9) + (m_14.M21 * y_9)) + (m_14.M31 * z_9)) + m_14.X41) * sc_5, ((((m_14.M12 * x_9) + (m_14.M22 * y_9)) + (m_14.M32 * z_9)) + m_14.Y42) * sc_5, ((((m_14.M13 * x_9) + (m_14.M23 * y_9)) + (m_14.M33 * z_9)) + m_14.Z43) * sc_5);
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
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix inverse transform ***", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let a_20, b_22, x_13, y_13, z_13;
        const m_15 = Matrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(-2, -3, 4));
        const inv_1 = Matrix__get_Inverse(m_15);
        const a_17 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        const a_18 = a_17;
        let b_20;
        let p_19;
        const p_15 = a_17;
        const m_19 = m_15;
        const x_11 = p_15.X;
        const y_11 = p_15.Y;
        const z_11 = p_15.Z;
        const sc_6 = 1 / ((((m_19.M14 * x_11) + (m_19.M24 * y_11)) + (m_19.M34 * z_11)) + m_19.M44);
        p_19 = Pnt_$ctor_Z7AD9E565_1(((((m_19.M11 * x_11) + (m_19.M21 * y_11)) + (m_19.M31 * z_11)) + m_19.X41) * sc_6, ((((m_19.M12 * x_11) + (m_19.M22 * y_11)) + (m_19.M32 * z_11)) + m_19.Y42) * sc_6, ((((m_19.M13 * x_11) + (m_19.M23 * y_11)) + (m_19.M33 * z_11)) + m_19.Z43) * sc_6);
        const m_23 = inv_1;
        const x_12 = p_19.X;
        const y_12 = p_19.Y;
        const z_12 = p_19.Z;
        const sc_7 = 1 / ((((m_23.M14 * x_12) + (m_23.M24 * y_12)) + (m_23.M34 * z_12)) + m_23.M44);
        b_20 = Pnt_$ctor_Z7AD9E565_1(((((m_23.M11 * x_12) + (m_23.M21 * y_12)) + (m_23.M31 * z_12)) + m_23.X41) * sc_7, ((((m_23.M12 * x_12) + (m_23.M22 * y_12)) + (m_23.M32 * z_12)) + m_23.Y42) * sc_7, ((((m_23.M13 * x_12) + (m_23.M23 * y_12)) + (m_23.M33 * z_12)) + m_23.Z43) * sc_7);
        const same_5 = ((a_20 = a_18, (b_22 = b_20, (x_13 = (a_20.X - b_22.X), (y_13 = (a_20.Y - b_22.Y), (z_13 = (a_20.Z - b_22.Z), Math.sqrt(((x_13 * x_13) + (y_13 * y_13)) + (z_13 * z_13)))))))) < 1E-12;
        if (!same_5) {
            Expect_isTrue(same_5)(`${"inverse transform ok"} expected: 
${Pnt__get_AsString(b_20)}, got: 
${Pnt__get_AsString(a_18)}`);
        }
        else {
            Expect_isTrue(same_5)("inverse transform ok");
        }
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix inverse transform  ", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let a_24, b_25, x_16, y_16, z_16;
        const m_24 = Matrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(-2, -3, 4));
        const inv_2 = Matrix__get_Inverse(m_24);
        const a_21 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        let a_22;
        let p_21;
        const p_20 = a_21;
        const m_25 = m_24;
        const x_14 = p_20.X;
        const y_14 = p_20.Y;
        const z_14 = p_20.Z;
        const sc_8 = 1 / ((((m_25.M14 * x_14) + (m_25.M24 * y_14)) + (m_25.M34 * z_14)) + m_25.M44);
        p_21 = Pnt_$ctor_Z7AD9E565_1(((((m_25.M11 * x_14) + (m_25.M21 * y_14)) + (m_25.M31 * z_14)) + m_25.X41) * sc_8, ((((m_25.M12 * x_14) + (m_25.M22 * y_14)) + (m_25.M32 * z_14)) + m_25.Y42) * sc_8, ((((m_25.M13 * x_14) + (m_25.M23 * y_14)) + (m_25.M33 * z_14)) + m_25.Z43) * sc_8);
        const m_26 = inv_2;
        const x_15 = p_21.X;
        const y_15 = p_21.Y;
        const z_15 = p_21.Z;
        const sc_9 = 1 / ((((m_26.M14 * x_15) + (m_26.M24 * y_15)) + (m_26.M34 * z_15)) + m_26.M44);
        a_22 = Pnt_$ctor_Z7AD9E565_1(((((m_26.M11 * x_15) + (m_26.M21 * y_15)) + (m_26.M31 * z_15)) + m_26.X41) * sc_9, ((((m_26.M12 * x_15) + (m_26.M22 * y_15)) + (m_26.M32 * z_15)) + m_26.Y42) * sc_9, ((((m_26.M13 * x_15) + (m_26.M23 * y_15)) + (m_26.M33 * z_15)) + m_26.Z43) * sc_9);
        const b_23 = a_21;
        const same_6 = ((a_24 = a_22, (b_25 = b_23, (x_16 = (a_24.X - b_25.X), (y_16 = (a_24.Y - b_25.Y), (z_16 = (a_24.Z - b_25.Z), Math.sqrt(((x_16 * x_16) + (y_16 * y_16)) + (z_16 * z_16)))))))) < 1E-12;
        if (!same_6) {
            Expect_isTrue(same_6)(`${"inverse *** ok"} expected: 
${Pnt__get_AsString(b_23)}, got: 
${Pnt__get_AsString(a_22)}`);
        }
        else {
            Expect_isTrue(same_6)("inverse *** ok");
        }
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix inverse rotate ", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let a_25, a_29, b_28, x_19, y_19, z_19;
        const m_27 = Matrix_createFromQuaternion_Z2A007687(Quaternion_createFromRadians_Z3D1F83EE((a_25 = Vec_$ctor_Z7AD9E565_1(0, 0, 1), Vec_$ctor_Z7AD9E565_2(a_25.X * 9, a_25.Y * 9, a_25.Z * 9)), 0.017453292519943295 * 90));
        const a_26 = Pnt_$ctor_Z7AD9E565(9, 0, 3);
        const a_27 = a_26;
        let b_26;
        let p_29;
        const p_25 = a_26;
        const m_31 = m_27;
        const x_17 = p_25.X;
        const y_17 = p_25.Y;
        const z_17 = p_25.Z;
        const sc_10 = 1 / ((((m_31.M14 * x_17) + (m_31.M24 * y_17)) + (m_31.M34 * z_17)) + m_31.M44);
        p_29 = Pnt_$ctor_Z7AD9E565_1(((((m_31.M11 * x_17) + (m_31.M21 * y_17)) + (m_31.M31 * z_17)) + m_31.X41) * sc_10, ((((m_31.M12 * x_17) + (m_31.M22 * y_17)) + (m_31.M32 * z_17)) + m_31.Y42) * sc_10, ((((m_31.M13 * x_17) + (m_31.M23 * y_17)) + (m_31.M33 * z_17)) + m_31.Z43) * sc_10);
        const m_35 = Matrix__get_Inverse(m_27);
        const x_18 = p_29.X;
        const y_18 = p_29.Y;
        const z_18 = p_29.Z;
        const sc_11 = 1 / ((((m_35.M14 * x_18) + (m_35.M24 * y_18)) + (m_35.M34 * z_18)) + m_35.M44);
        b_26 = Pnt_$ctor_Z7AD9E565_1(((((m_35.M11 * x_18) + (m_35.M21 * y_18)) + (m_35.M31 * z_18)) + m_35.X41) * sc_11, ((((m_35.M12 * x_18) + (m_35.M22 * y_18)) + (m_35.M32 * z_18)) + m_35.Y42) * sc_11, ((((m_35.M13 * x_18) + (m_35.M23 * y_18)) + (m_35.M33 * z_18)) + m_35.Z43) * sc_11);
        const same_7 = ((a_29 = a_27, (b_28 = b_26, (x_19 = (a_29.X - b_28.X), (y_19 = (a_29.Y - b_28.Y), (z_19 = (a_29.Z - b_28.Z), Math.sqrt(((x_19 * x_19) + (y_19 * y_19)) + (z_19 * z_19)))))))) < 1E-12;
        if (!same_7) {
            Expect_isTrue(same_7)(`${"rotateByQuaternion inverse"} expected: 
${Pnt__get_AsString(b_26)}, got: 
${Pnt__get_AsString(a_27)}`);
        }
        else {
            Expect_isTrue(same_7)("rotateByQuaternion inverse");
        }
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix inverse transform rot ***", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let a_30, a_34, b_31, x_22, y_22, z_22;
        const m_36 = Matrix_multiply_Z11D053C0_1(Matrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(-2, -3, 4)), Matrix_createFromQuaternion_Z2A007687(Quaternion_createFromRadians_Z3D1F83EE((a_30 = Vec_$ctor_Z7AD9E565_1(0, 0, 1), Vec_$ctor_Z7AD9E565_2(a_30.X * 9, a_30.Y * 9, a_30.Z * 9)), 0.017453292519943295 * 90)));
        const inv_4 = Matrix__get_Inverse(m_36);
        const a_31 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        const a_32 = a_31;
        let b_29;
        let p_37;
        const p_33 = a_31;
        const m_40 = m_36;
        const x_20 = p_33.X;
        const y_20 = p_33.Y;
        const z_20 = p_33.Z;
        const sc_12 = 1 / ((((m_40.M14 * x_20) + (m_40.M24 * y_20)) + (m_40.M34 * z_20)) + m_40.M44);
        p_37 = Pnt_$ctor_Z7AD9E565_1(((((m_40.M11 * x_20) + (m_40.M21 * y_20)) + (m_40.M31 * z_20)) + m_40.X41) * sc_12, ((((m_40.M12 * x_20) + (m_40.M22 * y_20)) + (m_40.M32 * z_20)) + m_40.Y42) * sc_12, ((((m_40.M13 * x_20) + (m_40.M23 * y_20)) + (m_40.M33 * z_20)) + m_40.Z43) * sc_12);
        const m_44 = inv_4;
        const x_21 = p_37.X;
        const y_21 = p_37.Y;
        const z_21 = p_37.Z;
        const sc_13 = 1 / ((((m_44.M14 * x_21) + (m_44.M24 * y_21)) + (m_44.M34 * z_21)) + m_44.M44);
        b_29 = Pnt_$ctor_Z7AD9E565_1(((((m_44.M11 * x_21) + (m_44.M21 * y_21)) + (m_44.M31 * z_21)) + m_44.X41) * sc_13, ((((m_44.M12 * x_21) + (m_44.M22 * y_21)) + (m_44.M32 * z_21)) + m_44.Y42) * sc_13, ((((m_44.M13 * x_21) + (m_44.M23 * y_21)) + (m_44.M33 * z_21)) + m_44.Z43) * sc_13);
        const same_8 = ((a_34 = a_32, (b_31 = b_29, (x_22 = (a_34.X - b_31.X), (y_22 = (a_34.Y - b_31.Y), (z_22 = (a_34.Z - b_31.Z), Math.sqrt(((x_22 * x_22) + (y_22 * y_22)) + (z_22 * z_22)))))))) < 1E-12;
        if (!same_8) {
            Expect_isTrue(same_8)(`${"inverse transformRigid ok"} expected: 
${Pnt__get_AsString(b_29)}, got: 
${Pnt__get_AsString(a_32)}`);
        }
        else {
            Expect_isTrue(same_8)("inverse transformRigid ok");
        }
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix inverse transform rot inv ***", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        let a_35, a_39, b_34, x_25, y_25, z_25;
        const t_1 = Matrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(-2, -3, 4));
        const m_45 = Matrix_multiply_Z11D053C0_1(Matrix_createFromQuaternion_Z2A007687(Quaternion_createFromRadians_Z3D1F83EE((a_35 = Vec_$ctor_Z7AD9E565_1(0, 0, 1), Vec_$ctor_Z7AD9E565_2(a_35.X * 9, a_35.Y * 9, a_35.Z * 9)), 0.017453292519943295 * 90)), t_1);
        const inv_5 = Matrix__get_Inverse(m_45);
        const a_36 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        const a_37 = a_36;
        let b_32;
        let p_45;
        const p_41 = a_36;
        const m_49 = m_45;
        const x_23 = p_41.X;
        const y_23 = p_41.Y;
        const z_23 = p_41.Z;
        const sc_14 = 1 / ((((m_49.M14 * x_23) + (m_49.M24 * y_23)) + (m_49.M34 * z_23)) + m_49.M44);
        p_45 = Pnt_$ctor_Z7AD9E565_1(((((m_49.M11 * x_23) + (m_49.M21 * y_23)) + (m_49.M31 * z_23)) + m_49.X41) * sc_14, ((((m_49.M12 * x_23) + (m_49.M22 * y_23)) + (m_49.M32 * z_23)) + m_49.Y42) * sc_14, ((((m_49.M13 * x_23) + (m_49.M23 * y_23)) + (m_49.M33 * z_23)) + m_49.Z43) * sc_14);
        const m_53 = inv_5;
        const x_24 = p_45.X;
        const y_24 = p_45.Y;
        const z_24 = p_45.Z;
        const sc_15 = 1 / ((((m_53.M14 * x_24) + (m_53.M24 * y_24)) + (m_53.M34 * z_24)) + m_53.M44);
        b_32 = Pnt_$ctor_Z7AD9E565_1(((((m_53.M11 * x_24) + (m_53.M21 * y_24)) + (m_53.M31 * z_24)) + m_53.X41) * sc_15, ((((m_53.M12 * x_24) + (m_53.M22 * y_24)) + (m_53.M32 * z_24)) + m_53.Y42) * sc_15, ((((m_53.M13 * x_24) + (m_53.M23 * y_24)) + (m_53.M33 * z_24)) + m_53.Z43) * sc_15);
        const same_9 = ((a_39 = a_37, (b_34 = b_32, (x_25 = (a_39.X - b_34.X), (y_25 = (a_39.Y - b_34.Y), (z_25 = (a_39.Z - b_34.Z), Math.sqrt(((x_25 * x_25) + (y_25 * y_25)) + (z_25 * z_25)))))))) < 1E-12;
        if (!same_9) {
            Expect_isTrue(same_9)(`${"inverse transformRigid ok"} expected: 
${Pnt__get_AsString(b_32)}, got: 
${Pnt__get_AsString(a_37)}`);
        }
        else {
            Expect_isTrue(same_9)("inverse transformRigid ok");
        }
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix scaling uniform", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let a_43, b_39, x_27, y_27, z_27, a_47, b_43, x_29, y_29, z_29;
        const m_54 = Matrix_createScale_Z7AD9E565(2, 2, 2);
        let a_41;
        const p_46 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        const m_55 = m_54;
        const x_26 = p_46.X;
        const y_26 = p_46.Y;
        const z_26 = p_46.Z;
        const sc_16 = 1 / ((((m_55.M14 * x_26) + (m_55.M24 * y_26)) + (m_55.M34 * z_26)) + m_55.M44);
        a_41 = Pnt_$ctor_Z7AD9E565_1(((((m_55.M11 * x_26) + (m_55.M21 * y_26)) + (m_55.M31 * z_26)) + m_55.X41) * sc_16, ((((m_55.M12 * x_26) + (m_55.M22 * y_26)) + (m_55.M32 * z_26)) + m_55.Y42) * sc_16, ((((m_55.M13 * x_26) + (m_55.M23 * y_26)) + (m_55.M33 * z_26)) + m_55.Z43) * sc_16);
        const b_37 = Pnt_$ctor_Z7AD9E565(2, 4, 6);
        const same_10 = ((a_43 = a_41, (b_39 = b_37, (x_27 = (a_43.X - b_39.X), (y_27 = (a_43.Y - b_39.Y), (z_27 = (a_43.Z - b_39.Z), Math.sqrt(((x_27 * x_27) + (y_27 * y_27)) + (z_27 * z_27)))))))) < 1E-12;
        if (!same_10) {
            Expect_isTrue(same_10)(`${"uniform scaling ok"} expected: 
${Pnt__get_AsString(b_37)}, got: 
${Pnt__get_AsString(a_41)}`);
        }
        else {
            Expect_isTrue(same_10)("uniform scaling ok");
        }
        let a_45;
        let v_3;
        const v_2 = Vec_$ctor_Z7AD9E565(1, 1, 1);
        const m_56 = m_54;
        const x_28 = v_2.X;
        const y_28 = v_2.Y;
        const z_28 = v_2.Z;
        v_3 = Vec_$ctor_Z7AD9E565_2(((m_56.M11 * x_28) + (m_56.M21 * y_28)) + (m_56.M31 * z_28), ((m_56.M12 * x_28) + (m_56.M22 * y_28)) + (m_56.M32 * z_28), ((m_56.M13 * x_28) + (m_56.M23 * y_28)) + (m_56.M33 * z_28));
        a_45 = Pnt_$ctor_Z7AD9E565_2(v_3.X, v_3.Y, v_3.Z);
        let b_41;
        const v_4 = Vec_$ctor_Z7AD9E565(2, 2, 2);
        b_41 = Pnt_$ctor_Z7AD9E565_2(v_4.X, v_4.Y, v_4.Z);
        const same_11 = ((a_47 = a_45, (b_43 = b_41, (x_29 = (a_47.X - b_43.X), (y_29 = (a_47.Y - b_43.Y), (z_29 = (a_47.Z - b_43.Z), Math.sqrt(((x_29 * x_29) + (y_29 * y_29)) + (z_29 * z_29)))))))) < 1E-12;
        if (!same_11) {
            Expect_isTrue(same_11)(`${"uniform scaling vector ok"} expected: 
${Pnt__get_AsString(b_41)}, got: 
${Pnt__get_AsString(a_45)}`);
        }
        else {
            Expect_isTrue(same_11)("uniform scaling vector ok");
        }
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix scaling non-uniform", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let a_51, b_48, x_31, y_31, z_31, a_54, b_51, x_33, y_33, z_33;
        const m_57 = Matrix_createScale_Z7AD9E565(2, 3, 0.5);
        const a_48 = Pnt_$ctor_Z7AD9E565(1, 2, 4);
        let b_44;
        const p_47 = a_48;
        const m_58 = m_57;
        const x_30 = p_47.X;
        const y_30 = p_47.Y;
        const z_30 = p_47.Z;
        const sc_17 = 1 / ((((m_58.M14 * x_30) + (m_58.M24 * y_30)) + (m_58.M34 * z_30)) + m_58.M44);
        b_44 = Pnt_$ctor_Z7AD9E565_1(((((m_58.M11 * x_30) + (m_58.M21 * y_30)) + (m_58.M31 * z_30)) + m_58.X41) * sc_17, ((((m_58.M12 * x_30) + (m_58.M22 * y_30)) + (m_58.M32 * z_30)) + m_58.Y42) * sc_17, ((((m_58.M13 * x_30) + (m_58.M23 * y_30)) + (m_58.M33 * z_30)) + m_58.Z43) * sc_17);
        const a_49 = b_44;
        const b_46 = Pnt_$ctor_Z7AD9E565(2, 6, 2);
        const same_12 = ((a_51 = a_49, (b_48 = b_46, (x_31 = (a_51.X - b_48.X), (y_31 = (a_51.Y - b_48.Y), (z_31 = (a_51.Z - b_48.Z), Math.sqrt(((x_31 * x_31) + (y_31 * y_31)) + (z_31 * z_31)))))))) < 1E-12;
        if (!same_12) {
            Expect_isTrue(same_12)(`${"non-uniform scaling ok"} expected: 
${Pnt__get_AsString(b_46)}, got: 
${Pnt__get_AsString(a_49)}`);
        }
        else {
            Expect_isTrue(same_12)("non-uniform scaling ok");
        }
        let a_52;
        const p_48 = b_44;
        const m_59 = Matrix__get_Inverse(m_57);
        const x_32 = p_48.X;
        const y_32 = p_48.Y;
        const z_32 = p_48.Z;
        const sc_18 = 1 / ((((m_59.M14 * x_32) + (m_59.M24 * y_32)) + (m_59.M34 * z_32)) + m_59.M44);
        a_52 = Pnt_$ctor_Z7AD9E565_1(((((m_59.M11 * x_32) + (m_59.M21 * y_32)) + (m_59.M31 * z_32)) + m_59.X41) * sc_18, ((((m_59.M12 * x_32) + (m_59.M22 * y_32)) + (m_59.M32 * z_32)) + m_59.Y42) * sc_18, ((((m_59.M13 * x_32) + (m_59.M23 * y_32)) + (m_59.M33 * z_32)) + m_59.Z43) * sc_18);
        const b_49 = a_48;
        const same_13 = ((a_54 = a_52, (b_51 = b_49, (x_33 = (a_54.X - b_51.X), (y_33 = (a_54.Y - b_51.Y), (z_33 = (a_54.Z - b_51.Z), Math.sqrt(((x_33 * x_33) + (y_33 * y_33)) + (z_33 * z_33)))))))) < 1E-12;
        if (!same_13) {
            Expect_isTrue(same_13)(`${"scaling inverse ok"} expected: 
${Pnt__get_AsString(b_49)}, got: 
${Pnt__get_AsString(a_52)}`);
        }
        else {
            Expect_isTrue(same_13)("scaling inverse ok");
        }
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix rotation X axis", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        let a_58, b_56, x_35, y_35, z_35, a_61, b_59, x_37, y_37, z_37;
        const m_60 = Matrix_createRotationX_5E38073B(90);
        const a_55 = Pnt_$ctor_Z7AD9E565(1, 1, 0);
        let b_52;
        const p_49 = a_55;
        const m_61 = m_60;
        const x_34 = p_49.X;
        const y_34 = p_49.Y;
        const z_34 = p_49.Z;
        const sc_19 = 1 / ((((m_61.M14 * x_34) + (m_61.M24 * y_34)) + (m_61.M34 * z_34)) + m_61.M44);
        b_52 = Pnt_$ctor_Z7AD9E565_1(((((m_61.M11 * x_34) + (m_61.M21 * y_34)) + (m_61.M31 * z_34)) + m_61.X41) * sc_19, ((((m_61.M12 * x_34) + (m_61.M22 * y_34)) + (m_61.M32 * z_34)) + m_61.Y42) * sc_19, ((((m_61.M13 * x_34) + (m_61.M23 * y_34)) + (m_61.M33 * z_34)) + m_61.Z43) * sc_19);
        const a_56 = b_52;
        const b_54 = Pnt_$ctor_Z7AD9E565(1, 0, 1);
        const same_14 = ((a_58 = a_56, (b_56 = b_54, (x_35 = (a_58.X - b_56.X), (y_35 = (a_58.Y - b_56.Y), (z_35 = (a_58.Z - b_56.Z), Math.sqrt(((x_35 * x_35) + (y_35 * y_35)) + (z_35 * z_35)))))))) < 1E-12;
        if (!same_14) {
            Expect_isTrue(same_14)(`${"rotation X 90 degrees"} expected: 
${Pnt__get_AsString(b_54)}, got: 
${Pnt__get_AsString(a_56)}`);
        }
        else {
            Expect_isTrue(same_14)("rotation X 90 degrees");
        }
        let a_59;
        const p_50 = b_52;
        const m_62 = Matrix__get_Inverse(m_60);
        const x_36 = p_50.X;
        const y_36 = p_50.Y;
        const z_36 = p_50.Z;
        const sc_20 = 1 / ((((m_62.M14 * x_36) + (m_62.M24 * y_36)) + (m_62.M34 * z_36)) + m_62.M44);
        a_59 = Pnt_$ctor_Z7AD9E565_1(((((m_62.M11 * x_36) + (m_62.M21 * y_36)) + (m_62.M31 * z_36)) + m_62.X41) * sc_20, ((((m_62.M12 * x_36) + (m_62.M22 * y_36)) + (m_62.M32 * z_36)) + m_62.Y42) * sc_20, ((((m_62.M13 * x_36) + (m_62.M23 * y_36)) + (m_62.M33 * z_36)) + m_62.Z43) * sc_20);
        const b_57 = a_55;
        const same_15 = ((a_61 = a_59, (b_59 = b_57, (x_37 = (a_61.X - b_59.X), (y_37 = (a_61.Y - b_59.Y), (z_37 = (a_61.Z - b_59.Z), Math.sqrt(((x_37 * x_37) + (y_37 * y_37)) + (z_37 * z_37)))))))) < 1E-12;
        if (!same_15) {
            Expect_isTrue(same_15)(`${"rotation X inverse"} expected: 
${Pnt__get_AsString(b_57)}, got: 
${Pnt__get_AsString(a_59)}`);
        }
        else {
            Expect_isTrue(same_15)("rotation X inverse");
        }
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix rotation Y axis", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let a_65, b_64, x_39, y_39, z_39, a_68, b_67, x_41, y_41, z_41;
        const m_63 = Matrix_createRotationY_5E38073B(90);
        const a_62 = Pnt_$ctor_Z7AD9E565(1, 0, 1);
        let b_60;
        const p_51 = a_62;
        const m_64 = m_63;
        const x_38 = p_51.X;
        const y_38 = p_51.Y;
        const z_38 = p_51.Z;
        const sc_21 = 1 / ((((m_64.M14 * x_38) + (m_64.M24 * y_38)) + (m_64.M34 * z_38)) + m_64.M44);
        b_60 = Pnt_$ctor_Z7AD9E565_1(((((m_64.M11 * x_38) + (m_64.M21 * y_38)) + (m_64.M31 * z_38)) + m_64.X41) * sc_21, ((((m_64.M12 * x_38) + (m_64.M22 * y_38)) + (m_64.M32 * z_38)) + m_64.Y42) * sc_21, ((((m_64.M13 * x_38) + (m_64.M23 * y_38)) + (m_64.M33 * z_38)) + m_64.Z43) * sc_21);
        const a_63 = b_60;
        const b_62 = Pnt_$ctor_Z7AD9E565(1, 0, -1);
        const same_16 = ((a_65 = a_63, (b_64 = b_62, (x_39 = (a_65.X - b_64.X), (y_39 = (a_65.Y - b_64.Y), (z_39 = (a_65.Z - b_64.Z), Math.sqrt(((x_39 * x_39) + (y_39 * y_39)) + (z_39 * z_39)))))))) < 1E-12;
        if (!same_16) {
            Expect_isTrue(same_16)(`${"rotation Y 90 degrees"} expected: 
${Pnt__get_AsString(b_62)}, got: 
${Pnt__get_AsString(a_63)}`);
        }
        else {
            Expect_isTrue(same_16)("rotation Y 90 degrees");
        }
        let a_66;
        const p_52 = b_60;
        const m_65 = Matrix__get_Inverse(m_63);
        const x_40 = p_52.X;
        const y_40 = p_52.Y;
        const z_40 = p_52.Z;
        const sc_22 = 1 / ((((m_65.M14 * x_40) + (m_65.M24 * y_40)) + (m_65.M34 * z_40)) + m_65.M44);
        a_66 = Pnt_$ctor_Z7AD9E565_1(((((m_65.M11 * x_40) + (m_65.M21 * y_40)) + (m_65.M31 * z_40)) + m_65.X41) * sc_22, ((((m_65.M12 * x_40) + (m_65.M22 * y_40)) + (m_65.M32 * z_40)) + m_65.Y42) * sc_22, ((((m_65.M13 * x_40) + (m_65.M23 * y_40)) + (m_65.M33 * z_40)) + m_65.Z43) * sc_22);
        const b_65 = a_62;
        const same_17 = ((a_68 = a_66, (b_67 = b_65, (x_41 = (a_68.X - b_67.X), (y_41 = (a_68.Y - b_67.Y), (z_41 = (a_68.Z - b_67.Z), Math.sqrt(((x_41 * x_41) + (y_41 * y_41)) + (z_41 * z_41)))))))) < 1E-12;
        if (!same_17) {
            Expect_isTrue(same_17)(`${"rotation Y inverse"} expected: 
${Pnt__get_AsString(b_65)}, got: 
${Pnt__get_AsString(a_66)}`);
        }
        else {
            Expect_isTrue(same_17)("rotation Y inverse");
        }
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix rotation Z axis", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        let a_72, b_72, x_43, y_43, z_43, a_75, b_75, x_45, y_45, z_45;
        const m_66 = Matrix_createRotationZ_5E38073B(90);
        const a_69 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        let b_68;
        const p_53 = a_69;
        const m_67 = m_66;
        const x_42 = p_53.X;
        const y_42 = p_53.Y;
        const z_42 = p_53.Z;
        const sc_23 = 1 / ((((m_67.M14 * x_42) + (m_67.M24 * y_42)) + (m_67.M34 * z_42)) + m_67.M44);
        b_68 = Pnt_$ctor_Z7AD9E565_1(((((m_67.M11 * x_42) + (m_67.M21 * y_42)) + (m_67.M31 * z_42)) + m_67.X41) * sc_23, ((((m_67.M12 * x_42) + (m_67.M22 * y_42)) + (m_67.M32 * z_42)) + m_67.Y42) * sc_23, ((((m_67.M13 * x_42) + (m_67.M23 * y_42)) + (m_67.M33 * z_42)) + m_67.Z43) * sc_23);
        const a_70 = b_68;
        const b_70 = Pnt_$ctor_Z7AD9E565(0, 1, 0);
        const same_18 = ((a_72 = a_70, (b_72 = b_70, (x_43 = (a_72.X - b_72.X), (y_43 = (a_72.Y - b_72.Y), (z_43 = (a_72.Z - b_72.Z), Math.sqrt(((x_43 * x_43) + (y_43 * y_43)) + (z_43 * z_43)))))))) < 1E-12;
        if (!same_18) {
            Expect_isTrue(same_18)(`${"rotation Z 90 degrees"} expected: 
${Pnt__get_AsString(b_70)}, got: 
${Pnt__get_AsString(a_70)}`);
        }
        else {
            Expect_isTrue(same_18)("rotation Z 90 degrees");
        }
        let a_73;
        const p_54 = b_68;
        const m_68 = Matrix__get_Inverse(m_66);
        const x_44 = p_54.X;
        const y_44 = p_54.Y;
        const z_44 = p_54.Z;
        const sc_24 = 1 / ((((m_68.M14 * x_44) + (m_68.M24 * y_44)) + (m_68.M34 * z_44)) + m_68.M44);
        a_73 = Pnt_$ctor_Z7AD9E565_1(((((m_68.M11 * x_44) + (m_68.M21 * y_44)) + (m_68.M31 * z_44)) + m_68.X41) * sc_24, ((((m_68.M12 * x_44) + (m_68.M22 * y_44)) + (m_68.M32 * z_44)) + m_68.Y42) * sc_24, ((((m_68.M13 * x_44) + (m_68.M23 * y_44)) + (m_68.M33 * z_44)) + m_68.Z43) * sc_24);
        const b_73 = a_69;
        const same_19 = ((a_75 = a_73, (b_75 = b_73, (x_45 = (a_75.X - b_75.X), (y_45 = (a_75.Y - b_75.Y), (z_45 = (a_75.Z - b_75.Z), Math.sqrt(((x_45 * x_45) + (y_45 * y_45)) + (z_45 * z_45)))))))) < 1E-12;
        if (!same_19) {
            Expect_isTrue(same_19)(`${"rotation Z inverse"} expected: 
${Pnt__get_AsString(b_73)}, got: 
${Pnt__get_AsString(a_73)}`);
        }
        else {
            Expect_isTrue(same_19)("rotation Z inverse");
        }
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix rotation arbitrary axis", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        let a_79, b_79, x_49, y_49, z_49;
        const m_69 = Matrix_createRotationAxis_Z3D1F83EE(Vec_$ctor_Z7AD9E565(1, 1, 1), 120);
        const a_76 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        let a_77;
        let p_57;
        let p_56;
        const p_55 = a_76;
        const m_70 = m_69;
        const x_46 = p_55.X;
        const y_46 = p_55.Y;
        const z_46 = p_55.Z;
        const sc_25 = 1 / ((((m_70.M14 * x_46) + (m_70.M24 * y_46)) + (m_70.M34 * z_46)) + m_70.M44);
        p_56 = Pnt_$ctor_Z7AD9E565_1(((((m_70.M11 * x_46) + (m_70.M21 * y_46)) + (m_70.M31 * z_46)) + m_70.X41) * sc_25, ((((m_70.M12 * x_46) + (m_70.M22 * y_46)) + (m_70.M32 * z_46)) + m_70.Y42) * sc_25, ((((m_70.M13 * x_46) + (m_70.M23 * y_46)) + (m_70.M33 * z_46)) + m_70.Z43) * sc_25);
        const m_71 = m_69;
        const x_47 = p_56.X;
        const y_47 = p_56.Y;
        const z_47 = p_56.Z;
        const sc_26 = 1 / ((((m_71.M14 * x_47) + (m_71.M24 * y_47)) + (m_71.M34 * z_47)) + m_71.M44);
        p_57 = Pnt_$ctor_Z7AD9E565_1(((((m_71.M11 * x_47) + (m_71.M21 * y_47)) + (m_71.M31 * z_47)) + m_71.X41) * sc_26, ((((m_71.M12 * x_47) + (m_71.M22 * y_47)) + (m_71.M32 * z_47)) + m_71.Y42) * sc_26, ((((m_71.M13 * x_47) + (m_71.M23 * y_47)) + (m_71.M33 * z_47)) + m_71.Z43) * sc_26);
        const m_72 = m_69;
        const x_48 = p_57.X;
        const y_48 = p_57.Y;
        const z_48 = p_57.Z;
        const sc_27 = 1 / ((((m_72.M14 * x_48) + (m_72.M24 * y_48)) + (m_72.M34 * z_48)) + m_72.M44);
        a_77 = Pnt_$ctor_Z7AD9E565_1(((((m_72.M11 * x_48) + (m_72.M21 * y_48)) + (m_72.M31 * z_48)) + m_72.X41) * sc_27, ((((m_72.M12 * x_48) + (m_72.M22 * y_48)) + (m_72.M32 * z_48)) + m_72.Y42) * sc_27, ((((m_72.M13 * x_48) + (m_72.M23 * y_48)) + (m_72.M33 * z_48)) + m_72.Z43) * sc_27);
        const b_77 = a_76;
        const same_20 = ((a_79 = a_77, (b_79 = b_77, (x_49 = (a_79.X - b_79.X), (y_49 = (a_79.Y - b_79.Y), (z_49 = (a_79.Z - b_79.Z), Math.sqrt(((x_49 * x_49) + (y_49 * y_49)) + (z_49 * z_49)))))))) < 1E-12;
        if (!same_20) {
            Expect_isTrue(same_20)(`${"rotation 120 degrees x3 = identity"} expected: 
${Pnt__get_AsString(b_77)}, got: 
${Pnt__get_AsString(a_77)}`);
        }
        else {
            Expect_isTrue(same_20)("rotation 120 degrees x3 = identity");
        }
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix rotation with center", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        let a_83, b_84, x_51, y_51, z_51;
        const m_73 = Matrix_createRotationAxisCenter_Z655651F(Vec_$ctor_Z7AD9E565_1(0, 0, 1), Pnt_$ctor_Z7AD9E565(1, 1, 0), 90);
        let a_81;
        const p_58 = Pnt_$ctor_Z7AD9E565(2, 1, 0);
        const m_74 = m_73;
        const x_50 = p_58.X;
        const y_50 = p_58.Y;
        const z_50 = p_58.Z;
        const sc_28 = 1 / ((((m_74.M14 * x_50) + (m_74.M24 * y_50)) + (m_74.M34 * z_50)) + m_74.M44);
        a_81 = Pnt_$ctor_Z7AD9E565_1(((((m_74.M11 * x_50) + (m_74.M21 * y_50)) + (m_74.M31 * z_50)) + m_74.X41) * sc_28, ((((m_74.M12 * x_50) + (m_74.M22 * y_50)) + (m_74.M32 * z_50)) + m_74.Y42) * sc_28, ((((m_74.M13 * x_50) + (m_74.M23 * y_50)) + (m_74.M33 * z_50)) + m_74.Z43) * sc_28);
        const b_82 = Pnt_$ctor_Z7AD9E565(1, 2, 0);
        const same_21 = ((a_83 = a_81, (b_84 = b_82, (x_51 = (a_83.X - b_84.X), (y_51 = (a_83.Y - b_84.Y), (z_51 = (a_83.Z - b_84.Z), Math.sqrt(((x_51 * x_51) + (y_51 * y_51)) + (z_51 * z_51)))))))) < 1E-12;
        if (!same_21) {
            Expect_isTrue(same_21)(`${"rotation around center"} expected: 
${Pnt__get_AsString(b_82)}, got: 
${Pnt__get_AsString(a_81)}`);
        }
        else {
            Expect_isTrue(same_21)("rotation around center");
        }
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix shear transformation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let a_87, b_89, x_53, y_53, z_53;
        const m_75 = Matrix_createShear_76A78260(0.5, 3, 0, 0, 0, 0);
        let a_85;
        const p_59 = Pnt_$ctor_Z7AD9E565(2, 0, 0);
        const m_76 = m_75;
        const x_52 = p_59.X;
        const y_52 = p_59.Y;
        const z_52 = p_59.Z;
        const sc_29 = 1 / ((((m_76.M14 * x_52) + (m_76.M24 * y_52)) + (m_76.M34 * z_52)) + m_76.M44);
        a_85 = Pnt_$ctor_Z7AD9E565_1(((((m_76.M11 * x_52) + (m_76.M21 * y_52)) + (m_76.M31 * z_52)) + m_76.X41) * sc_29, ((((m_76.M12 * x_52) + (m_76.M22 * y_52)) + (m_76.M32 * z_52)) + m_76.Y42) * sc_29, ((((m_76.M13 * x_52) + (m_76.M23 * y_52)) + (m_76.M33 * z_52)) + m_76.Z43) * sc_29);
        const b_87 = Pnt_$ctor_Z7AD9E565(2, 1, 6);
        const same_22 = ((a_87 = a_85, (b_89 = b_87, (x_53 = (a_87.X - b_89.X), (y_53 = (a_87.Y - b_89.Y), (z_53 = (a_87.Z - b_89.Z), Math.sqrt(((x_53 * x_53) + (y_53 * y_53)) + (z_53 * z_53)))))))) < 1E-12;
        if (!same_22) {
            Expect_isTrue(same_22)(`${"shear XY"} expected: 
${Pnt__get_AsString(b_87)}, got: 
${Pnt__get_AsString(a_85)}`);
        }
        else {
            Expect_isTrue(same_22)("shear XY");
        }
        const det = Matrix__get_Determinant(m_75);
        Expect_isTrue(Math.abs(det - 1) < 1E-12)("shear determinant is 1");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix combined transformations TRS", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        let a_91, b_94, x_55, y_55, z_55;
        const m_77 = Matrix_multiply_Z11D053C0_1(Matrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(1, 2, 3)), Matrix_multiply_Z11D053C0_1(Matrix_createRotationZ_5E38073B(90), Matrix_createScale_Z7AD9E565(2, 2, 2)));
        let a_89;
        const p_60 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const m_78 = m_77;
        const x_54 = p_60.X;
        const y_54 = p_60.Y;
        const z_54 = p_60.Z;
        const sc_30 = 1 / ((((m_78.M14 * x_54) + (m_78.M24 * y_54)) + (m_78.M34 * z_54)) + m_78.M44);
        a_89 = Pnt_$ctor_Z7AD9E565_1(((((m_78.M11 * x_54) + (m_78.M21 * y_54)) + (m_78.M31 * z_54)) + m_78.X41) * sc_30, ((((m_78.M12 * x_54) + (m_78.M22 * y_54)) + (m_78.M32 * z_54)) + m_78.Y42) * sc_30, ((((m_78.M13 * x_54) + (m_78.M23 * y_54)) + (m_78.M33 * z_54)) + m_78.Z43) * sc_30);
        const b_92 = Pnt_$ctor_Z7AD9E565(-4, 4, 6);
        const same_23 = ((a_91 = a_89, (b_94 = b_92, (x_55 = (a_91.X - b_94.X), (y_55 = (a_91.Y - b_94.Y), (z_55 = (a_91.Z - b_94.Z), Math.sqrt(((x_55 * x_55) + (y_55 * y_55)) + (z_55 * z_55)))))))) < 1E-12;
        if (!same_23) {
            Expect_isTrue(same_23)(`${"TRS transformation"} expected: 
${Pnt__get_AsString(b_92)}, got: 
${Pnt__get_AsString(a_89)}`);
        }
        else {
            Expect_isTrue(same_23)("TRS transformation");
        }
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix combined transformations TRS on Vec", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let a_96, b_99, x_57, y_57, z_57;
        const m_79 = Matrix_multiply_Z11D053C0_1(Matrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(1, 2, 3)), Matrix_multiply_Z11D053C0_1(Matrix_createRotationZ_5E38073B(90), Matrix_createScale_Z7AD9E565(2, 2, 2)));
        let a_94;
        let v_6;
        const v_5 = Vec_$ctor_Z7AD9E565(1, 0, 0);
        const m_80 = m_79;
        const x_56 = v_5.X;
        const y_56 = v_5.Y;
        const z_56 = v_5.Z;
        v_6 = Vec_$ctor_Z7AD9E565_2(((m_80.M11 * x_56) + (m_80.M21 * y_56)) + (m_80.M31 * z_56), ((m_80.M12 * x_56) + (m_80.M22 * y_56)) + (m_80.M32 * z_56), ((m_80.M13 * x_56) + (m_80.M23 * y_56)) + (m_80.M33 * z_56));
        a_94 = Pnt_$ctor_Z7AD9E565_2(v_6.X, v_6.Y, v_6.Z);
        const b_97 = Pnt_$ctor_Z7AD9E565(0, 2, 0);
        const same_24 = ((a_96 = a_94, (b_99 = b_97, (x_57 = (a_96.X - b_99.X), (y_57 = (a_96.Y - b_99.Y), (z_57 = (a_96.Z - b_99.Z), Math.sqrt(((x_57 * x_57) + (y_57 * y_57)) + (z_57 * z_57)))))))) < 1E-12;
        if (!same_24) {
            Expect_isTrue(same_24)(`${"TRS transformation"} expected: 
${Pnt__get_AsString(b_97)}, got: 
${Pnt__get_AsString(a_94)}`);
        }
        else {
            Expect_isTrue(same_24)("TRS transformation");
        }
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix vec to vec rotation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        let a_100, b_103, x_61, y_61, z_61;
        const v1 = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
        const v2 = UnitVec_$ctor_Z7AD9E565(0, 1, 0);
        let a_98;
        let v_8;
        const v_7 = v1;
        const m_82 = Matrix_createVecToVec_6319FE20(v1, v2);
        const x_60 = v_7.X;
        const y_60 = v_7.Y;
        const z_60 = v_7.Z;
        v_8 = Vec_$ctor_Z7AD9E565_2(((m_82.M11 * x_60) + (m_82.M21 * y_60)) + (m_82.M31 * z_60), ((m_82.M12 * x_60) + (m_82.M22 * y_60)) + (m_82.M32 * z_60), ((m_82.M13 * x_60) + (m_82.M23 * y_60)) + (m_82.M33 * z_60));
        a_98 = Pnt_$ctor_Z7AD9E565_2(v_8.X, v_8.Y, v_8.Z);
        let b_101;
        const v_9 = Vec_$ctor_Z7AD9E565(v2.X, v2.Y, v2.Z);
        b_101 = Pnt_$ctor_Z7AD9E565_2(v_9.X, v_9.Y, v_9.Z);
        const same_25 = ((a_100 = a_98, (b_103 = b_101, (x_61 = (a_100.X - b_103.X), (y_61 = (a_100.Y - b_103.Y), (z_61 = (a_100.Z - b_103.Z), Math.sqrt(((x_61 * x_61) + (y_61 * y_61)) + (z_61 * z_61)))))))) < 1E-12;
        if (!same_25) {
            Expect_isTrue(same_25)(`${"vec to vec rotation"} expected: 
${Pnt__get_AsString(b_101)}, got: 
${Pnt__get_AsString(a_98)}`);
        }
        else {
            Expect_isTrue(same_25)("vec to vec rotation");
        }
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix vec to vec same direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        let v_10;
        const x_62 = 1;
        const y_62 = 1;
        const z_62 = 1;
        const l = Math.sqrt(((x_62 * x_62) + (y_62 * y_62)) + (z_62 * z_62));
        if (!(l > 1E-12)) {
            failUnit3("UnitVec.create", x_62, y_62, z_62);
        }
        const li = 1 / l;
        v_10 = UnitVec_$ctor_Z7AD9E565(li * x_62, li * y_62, li * z_62);
        Expect_isTrue(Matrix__get_IsIdentity(Matrix_createVecToVec_6319FE20(v_10, v_10)))("same vector gives identity");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix properties identity", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        const m_84 = Matrix_get_identity();
        Expect_isTrue(Matrix__get_IsIdentity(m_84))("identity IsIdentity");
        Expect_isTrue(Matrix__get_IsAffine(m_84))("identity IsAffine");
        Expect_isTrue(Matrix__get_IsOrthogonal(m_84))("identity IsOrthogonal");
        Expect_isFalse(Matrix__get_IsScaling(m_84))("identity not IsScaling");
        Expect_isFalse(Matrix__get_IsTranslating(m_84))("identity not IsTranslating");
        Expect_isTrue(Math.abs(Matrix__get_Determinant(m_84) - 1) < 1E-12)("identity determinant is 1");
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix properties translation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        const m_85 = Matrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(1, 2, 3));
        Expect_isFalse(Matrix__get_IsIdentity(m_85))("translation not IsIdentity");
        Expect_isTrue(Matrix__get_IsAffine(m_85))("translation IsAffine");
        Expect_isTrue(Matrix__get_IsTranslating(m_85))("translation IsTranslating");
        Expect_isTrue(Matrix__get_IsOnlyTranslating(m_85))("translation IsOnlyTranslating");
        Expect_isFalse(Matrix__get_IsScaling(m_85))("translation not IsScaling");
        Expect_isTrue(Math.abs(Matrix__get_Determinant(m_85) - 1) < 1E-12)("translation determinant is 1");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix properties scaling", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        const m_86 = Matrix_createScale_Z7AD9E565(2, 3, 4);
        Expect_isFalse(Matrix__get_IsIdentity(m_86))("scaling not IsIdentity");
        Expect_isTrue(Matrix__get_IsAffine(m_86))("scaling IsAffine");
        Expect_isTrue(Matrix__get_IsScaling(m_86))("scaling IsScaling");
        Expect_isFalse(Matrix__get_IsTranslating(m_86))("scaling not IsTranslating");
        Expect_isTrue(Math.abs(Matrix__get_Determinant(m_86) - 24) < 1E-12)("scaling determinant is 24");
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix properties rotation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        const m_87 = Matrix_createRotationZ_5E38073B(45);
        Expect_isFalse(Matrix__get_IsIdentity(m_87))("rotation not IsIdentity");
        Expect_isTrue(Matrix__get_IsAffine(m_87))("rotation IsAffine");
        Expect_isTrue(Matrix__get_IsOrthogonal(m_87))("rotation IsOrthogonal");
        Expect_isFalse(Matrix__get_IsScaling(m_87))("rotation not IsScaling");
        Expect_isTrue(Math.abs(Matrix__get_Determinant(m_87) - 1) < 1E-12)("rotation determinant is 1");
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix properties mirroring", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        const m_88 = Matrix_createScale_Z7AD9E565(-1, 1, 1);
        Expect_isTrue(Matrix__get_IsAffine(m_88))("mirroring IsAffine");
        Expect_isTrue(Matrix__get_IsMirroring(m_88))("mirroring IsMirroring");
        Expect_isTrue(Matrix__get_IsReflecting(m_88))("mirroring IsReflecting");
        Expect_isTrue(Math.abs(Matrix__get_Determinant(m_88) + 1) < 1E-12)("mirroring determinant is -1");
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix from quaternion", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        let a_104, b_108, x_66, y_65, z_65;
        const m_89 = Matrix_createFromQuaternion_Z2A007687(Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565_1(0, 0, 1), 0.017453292519943295 * 90));
        let a_102;
        const p_61 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const m_90 = m_89;
        const x_65 = p_61.X;
        const y_64 = p_61.Y;
        const z_64 = p_61.Z;
        const sc_31 = 1 / ((((m_90.M14 * x_65) + (m_90.M24 * y_64)) + (m_90.M34 * z_64)) + m_90.M44);
        a_102 = Pnt_$ctor_Z7AD9E565_1(((((m_90.M11 * x_65) + (m_90.M21 * y_64)) + (m_90.M31 * z_64)) + m_90.X41) * sc_31, ((((m_90.M12 * x_65) + (m_90.M22 * y_64)) + (m_90.M32 * z_64)) + m_90.Y42) * sc_31, ((((m_90.M13 * x_65) + (m_90.M23 * y_64)) + (m_90.M33 * z_64)) + m_90.Z43) * sc_31);
        const b_106 = Pnt_$ctor_Z7AD9E565(0, 1, 0);
        const same_26 = ((a_104 = a_102, (b_108 = b_106, (x_66 = (a_104.X - b_108.X), (y_65 = (a_104.Y - b_108.Y), (z_65 = (a_104.Z - b_108.Z), Math.sqrt(((x_66 * x_66) + (y_65 * y_65)) + (z_65 * z_65)))))))) < 1E-12;
        if (!same_26) {
            Expect_isTrue(same_26)(`${"quaternion to matrix rotation"} expected: 
${Pnt__get_AsString(b_106)}, got: 
${Pnt__get_AsString(a_102)}`);
        }
        else {
            Expect_isTrue(same_26)("quaternion to matrix rotation");
        }
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix column vectors", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        let a_108, b_112, x_67, y_66, z_66, a_112, b_116, x_68, y_67, z_67, a_116, b_120, x_69, y_68, z_68;
        const m_91 = Matrix_createRotationZ_5E38073B(90);
        const col1 = Matrix__get_ColumnVector1(m_91);
        const col2 = Matrix__get_ColumnVector2(m_91);
        const col3 = Matrix__get_ColumnVector3(m_91);
        let a_106;
        const v_11 = col1;
        a_106 = Pnt_$ctor_Z7AD9E565_2(v_11.X, v_11.Y, v_11.Z);
        const b_110 = Pnt_$ctor_Z7AD9E565(0, 1, 0);
        const same_27 = ((a_108 = a_106, (b_112 = b_110, (x_67 = (a_108.X - b_112.X), (y_66 = (a_108.Y - b_112.Y), (z_66 = (a_108.Z - b_112.Z), Math.sqrt(((x_67 * x_67) + (y_66 * y_66)) + (z_66 * z_66)))))))) < 1E-12;
        if (!same_27) {
            Expect_isTrue(same_27)(`${"column 1 after Z rotation"} expected: 
${Pnt__get_AsString(b_110)}, got: 
${Pnt__get_AsString(a_106)}`);
        }
        else {
            Expect_isTrue(same_27)("column 1 after Z rotation");
        }
        let a_110;
        const v_12 = col2;
        a_110 = Pnt_$ctor_Z7AD9E565_2(v_12.X, v_12.Y, v_12.Z);
        const b_114 = Pnt_$ctor_Z7AD9E565(-1, 0, 0);
        const same_28 = ((a_112 = a_110, (b_116 = b_114, (x_68 = (a_112.X - b_116.X), (y_67 = (a_112.Y - b_116.Y), (z_67 = (a_112.Z - b_116.Z), Math.sqrt(((x_68 * x_68) + (y_67 * y_67)) + (z_67 * z_67)))))))) < 1E-12;
        if (!same_28) {
            Expect_isTrue(same_28)(`${"column 2 after Z rotation"} expected: 
${Pnt__get_AsString(b_114)}, got: 
${Pnt__get_AsString(a_110)}`);
        }
        else {
            Expect_isTrue(same_28)("column 2 after Z rotation");
        }
        let a_114;
        const v_13 = col3;
        a_114 = Pnt_$ctor_Z7AD9E565_2(v_13.X, v_13.Y, v_13.Z);
        const b_118 = Pnt_$ctor_Z7AD9E565(0, 0, 1);
        const same_29 = ((a_116 = a_114, (b_120 = b_118, (x_69 = (a_116.X - b_120.X), (y_68 = (a_116.Y - b_120.Y), (z_68 = (a_116.Z - b_120.Z), Math.sqrt(((x_69 * x_69) + (y_68 * y_68)) + (z_68 * z_68)))))))) < 1E-12;
        if (!same_29) {
            Expect_isTrue(same_29)(`${"column 3 after Z rotation"} expected: 
${Pnt__get_AsString(b_118)}, got: 
${Pnt__get_AsString(a_114)}`);
        }
        else {
            Expect_isTrue(same_29)("column 3 after Z rotation");
        }
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix transpose", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        Expect_isTrue(Matrix_equals(1E-12, Matrix_transpose_3CAE9522(Matrix_$ctor_Z61E40B00(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)), Matrix_$ctor_Z61E40B00(1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15, 4, 8, 12, 16)))("matrix transpose");
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix multiply associativity", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        const a_118 = Matrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(1, 0, 0));
        const b_122 = Matrix_createRotationZ_5E38073B(90);
        const c_7 = Matrix_createScale_Z7AD9E565(2, 2, 2);
        Expect_isTrue(Matrix_equals(1E-12, Matrix_multiply_Z11D053C0_1(Matrix_multiply_Z11D053C0_1(a_118, b_122), c_7), Matrix_multiply_Z11D053C0_1(a_118, Matrix_multiply_Z11D053C0_1(b_122, c_7))))("matrix multiplication associativity");
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix determinant scaling", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        const det_1 = Matrix__get_Determinant(Matrix_createScale_Z7AD9E565(2, 3, 4));
        Expect_isTrue(Math.abs(det_1 - 24) < 1E-12)("scaling determinant");
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix arrays conversion", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        const m_94 = Matrix_$ctor_Z61E40B00(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
        const rowArray = Matrix__get_ToArrayByRows(m_94);
        const colArray = Matrix__get_ToArrayByColumns(m_94);
        const mFromRows = Matrix_createFromRowMajorArray_52AF8430(rowArray);
        const mFromCols = Matrix_createFromColumMajorArray_52AF8430(colArray);
        Expect_isTrue(Matrix_equals(1E-12, m_94, mFromRows))("matrix from row array");
        Expect_isTrue(Matrix_equals(1E-12, m_94, mFromCols))("matrix from column array");
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix add translation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        let a_125, b_130, x_71, y_70, z_70;
        const m_95 = Matrix_createRotationZ_5E38073B(90);
        const m2_1 = Matrix_addTranslation(Vec_$ctor_Z7AD9E565(1, 2, 3), m_95);
        let a_123;
        const p_62 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const m_97 = m2_1;
        const x_70 = p_62.X;
        const y_69 = p_62.Y;
        const z_69 = p_62.Z;
        const sc_32 = 1 / ((((m_97.M14 * x_70) + (m_97.M24 * y_69)) + (m_97.M34 * z_69)) + m_97.M44);
        a_123 = Pnt_$ctor_Z7AD9E565_1(((((m_97.M11 * x_70) + (m_97.M21 * y_69)) + (m_97.M31 * z_69)) + m_97.X41) * sc_32, ((((m_97.M12 * x_70) + (m_97.M22 * y_69)) + (m_97.M32 * z_69)) + m_97.Y42) * sc_32, ((((m_97.M13 * x_70) + (m_97.M23 * y_69)) + (m_97.M33 * z_69)) + m_97.Z43) * sc_32);
        const b_128 = Pnt_$ctor_Z7AD9E565(1, 3, 3);
        const same_30 = ((a_125 = a_123, (b_130 = b_128, (x_71 = (a_125.X - b_130.X), (y_70 = (a_125.Y - b_130.Y), (z_70 = (a_125.Z - b_130.Z), Math.sqrt(((x_71 * x_71) + (y_70 * y_70)) + (z_70 * z_70)))))))) < 1E-12;
        if (!same_30) {
            Expect_isTrue(same_30)(`${"add translation to rotation"} expected: 
${Pnt__get_AsString(b_128)}, got: 
${Pnt__get_AsString(a_123)}`);
        }
        else {
            Expect_isTrue(same_30)("add translation to rotation");
        }
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})(), (() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix very small transformation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        let a_129, b_135, x_73, y_72, z_72;
        const m_98 = Matrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(1E-10, 1E-10, 1E-10));
        let a_127;
        const p_63 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const m_99 = m_98;
        const x_72 = p_63.X;
        const y_71 = p_63.Y;
        const z_71 = p_63.Z;
        const sc_33 = 1 / ((((m_99.M14 * x_72) + (m_99.M24 * y_71)) + (m_99.M34 * z_71)) + m_99.M44);
        a_127 = Pnt_$ctor_Z7AD9E565_1(((((m_99.M11 * x_72) + (m_99.M21 * y_71)) + (m_99.M31 * z_71)) + m_99.X41) * sc_33, ((((m_99.M12 * x_72) + (m_99.M22 * y_71)) + (m_99.M32 * z_71)) + m_99.Y42) * sc_33, ((((m_99.M13 * x_72) + (m_99.M23 * y_71)) + (m_99.M33 * z_71)) + m_99.Z43) * sc_33);
        const b_133 = Pnt_$ctor_Z7AD9E565(1E-10, 1E-10, 1E-10);
        const same_31 = ((a_129 = a_127, (b_135 = b_133, (x_73 = (a_129.X - b_135.X), (y_72 = (a_129.Y - b_135.Y), (z_72 = (a_129.Z - b_135.Z), Math.sqrt(((x_73 * x_73) + (y_72 * y_72)) + (z_72 * z_72)))))))) < 1E-12;
        if (!same_31) {
            Expect_isTrue(same_31)(`${"very small translation"} expected: 
${Pnt__get_AsString(b_133)}, got: 
${Pnt__get_AsString(a_127)}`);
        }
        else {
            Expect_isTrue(same_31)("very small translation");
        }
        Test_TestCaseBuilder__Zero(builder$0040_31);
    }));
})(), (() => {
    const builder$0040_32 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix vec to vec opposite vectors should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_32, Test_TestCaseBuilder__Delay_1505(builder$0040_32, () => {
        const v1_1 = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
        let v2_1;
        const v_15 = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
        v2_1 = UnitVec_$ctor_Z7AD9E565(-v_15.X, -v_15.Y, -v_15.Z);
        Expect_throws(() => {
            Matrix_createVecToVec_6319FE20(v1_1, v2_1);
        }, "opposite vectors should throw");
        Test_TestCaseBuilder__Zero(builder$0040_32);
    }));
})(), (() => {
    const builder$0040_33 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix vec to vec nearly same vectors", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_33, Test_TestCaseBuilder__Delay_1505(builder$0040_33, () => {
        let l_1, li_1, l_2, li_2;
        Expect_isTrue(Matrix__get_IsIdentity(Matrix_createVecToVec_6319FE20((l_1 = Math.sqrt(((1 * 1) + (0 * 0)) + (0 * 0)), (!(l_1 > 1E-12) ? failUnit3("UnitVec.create", 1, 0, 0) : undefined, (li_1 = (1 / l_1), UnitVec_$ctor_Z7AD9E565(li_1 * 1, li_1 * 0, li_1 * 0)))), (l_2 = Math.sqrt(((1 * 1) + (1E-13 * 1E-13)) + (0 * 0)), (!(l_2 > 1E-12) ? failUnit3("UnitVec.create", 1, 1E-13, 0) : undefined, (li_2 = (1 / l_2), UnitVec_$ctor_Z7AD9E565(li_2 * 1, li_2 * 1E-13, li_2 * 0)))))))("nearly same vectors should give identity");
        Test_TestCaseBuilder__Zero(builder$0040_33);
    }));
})(), (() => {
    const builder$0040_34 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix vec to vec with zero-length Vec should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_34, Test_TestCaseBuilder__Delay_1505(builder$0040_34, () => {
        const v1_3 = Vec_$ctor_Z7AD9E565(0, 0, 0);
        const v2_3 = Vec_$ctor_Z7AD9E565(1, 0, 0);
        Expect_throws(() => {
            Matrix_createVecToVec_5A694120(v1_3, v2_3);
        }, "zero-length from vector should throw");
        Test_TestCaseBuilder__Zero(builder$0040_34);
    }));
})(), (() => {
    const builder$0040_35 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix vec to vec with zero-length target Vec should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_35, Test_TestCaseBuilder__Delay_1505(builder$0040_35, () => {
        const v1_4 = Vec_$ctor_Z7AD9E565(1, 0, 0);
        const v2_4 = Vec_$ctor_Z7AD9E565(0, 0, 0);
        Expect_throws(() => {
            Matrix_createVecToVec_5A694120(v1_4, v2_4);
        }, "zero-length to vector should throw");
        Test_TestCaseBuilder__Zero(builder$0040_35);
    }));
})(), (() => {
    const builder$0040_36 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix createVecToVec 90 degree rotation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_36, Test_TestCaseBuilder__Delay_1505(builder$0040_36, () => {
        let a_133, b_139, x_86, y_83, z_83;
        const from = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
        const to_ = UnitVec_$ctor_Z7AD9E565(0, 1, 0);
        let a_131;
        let v_17;
        const v_16 = from;
        const m_102 = Matrix_createVecToVec_6319FE20(from, to_);
        const x_85 = v_16.X;
        const y_82 = v_16.Y;
        const z_82 = v_16.Z;
        v_17 = Vec_$ctor_Z7AD9E565_2(((m_102.M11 * x_85) + (m_102.M21 * y_82)) + (m_102.M31 * z_82), ((m_102.M12 * x_85) + (m_102.M22 * y_82)) + (m_102.M32 * z_82), ((m_102.M13 * x_85) + (m_102.M23 * y_82)) + (m_102.M33 * z_82));
        a_131 = Pnt_$ctor_Z7AD9E565_2(v_17.X, v_17.Y, v_17.Z);
        let b_137;
        const v_18 = to_;
        b_137 = Pnt_$ctor_Z7AD9E565_2(v_18.X, v_18.Y, v_18.Z);
        const same_32 = ((a_133 = a_131, (b_139 = b_137, (x_86 = (a_133.X - b_139.X), (y_83 = (a_133.Y - b_139.Y), (z_83 = (a_133.Z - b_139.Z), Math.sqrt(((x_86 * x_86) + (y_83 * y_83)) + (z_83 * z_83)))))))) < 1E-12;
        if (!same_32) {
            Expect_isTrue(same_32)(`${"90° rotation X to Y"} expected: 
${Pnt__get_AsString(b_137)}, got: 
${Pnt__get_AsString(a_131)}`);
        }
        else {
            Expect_isTrue(same_32)("90° rotation X to Y");
        }
        Test_TestCaseBuilder__Zero(builder$0040_36);
    }));
})(), (() => {
    const builder$0040_37 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix createVecToVec 180 degree rotation alternative", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_37, Test_TestCaseBuilder__Delay_1505(builder$0040_37, () => {
        let a_135, v_20, v_19, m_104, x_93, y_88, z_88, b_141, v_21, x_94, y_89, z_89;
        let from_1;
        const x_87 = 1;
        const y_84 = 0;
        const z_84 = 0;
        const l_3 = Math.sqrt(((x_87 * x_87) + (y_84 * y_84)) + (z_84 * z_84));
        if (!(l_3 > 1E-12)) {
            failUnit3("UnitVec.create", x_87, y_84, z_84);
        }
        const li_3 = 1 / l_3;
        from_1 = UnitVec_$ctor_Z7AD9E565(li_3 * x_87, li_3 * y_84, li_3 * z_84);
        let to__1;
        const z_86 = 0;
        const l_4 = Math.sqrt(((-0.99999 * -0.99999) + (1E-05 * 1E-05)) + (z_86 * z_86));
        if (!(l_4 > 1E-12)) {
            failUnit3("UnitVec.create", -0.99999, 1E-05, z_86);
        }
        const li_4 = 1 / l_4;
        to__1 = UnitVec_$ctor_Z7AD9E565(li_4 * -0.99999, li_4 * 1E-05, li_4 * z_86);
        Expect_floatClose(AccuracyModule_low, (a_135 = ((v_20 = ((v_19 = from_1, (m_104 = Matrix_createVecToVec_6319FE20(from_1, to__1), (x_93 = v_19.X, (y_88 = v_19.Y, (z_88 = v_19.Z, Vec_$ctor_Z7AD9E565_2(((m_104.M11 * x_93) + (m_104.M21 * y_88)) + (m_104.M31 * z_88), ((m_104.M12 * x_93) + (m_104.M22 * y_88)) + (m_104.M32 * z_88), ((m_104.M13 * x_93) + (m_104.M23 * y_88)) + (m_104.M33 * z_88)))))))), Pnt_$ctor_Z7AD9E565_2(v_20.X, v_20.Y, v_20.Z))), (b_141 = ((v_21 = to__1, Pnt_$ctor_Z7AD9E565_2(v_21.X, v_21.Y, v_21.Z))), (x_94 = (a_135.X - b_141.X), (y_89 = (a_135.Y - b_141.Y), (z_89 = (a_135.Z - b_141.Z), Math.sqrt(((x_94 * x_94) + (y_89 * y_89)) + (z_89 * z_89))))))), 0, "near-180° rotation works");
        Test_TestCaseBuilder__Zero(builder$0040_37);
    }));
})(), (() => {
    const builder$0040_38 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix createVecToVec with perpendicular vectors", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_38, Test_TestCaseBuilder__Delay_1505(builder$0040_38, () => {
        let a_139, b_145, x_98, y_93, z_93;
        const from_2 = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
        const to__2 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
        let a_137;
        let v_23;
        const v_22 = from_2;
        const m_106 = Matrix_createVecToVec_6319FE20(from_2, to__2);
        const x_97 = v_22.X;
        const y_92 = v_22.Y;
        const z_92 = v_22.Z;
        v_23 = Vec_$ctor_Z7AD9E565_2(((m_106.M11 * x_97) + (m_106.M21 * y_92)) + (m_106.M31 * z_92), ((m_106.M12 * x_97) + (m_106.M22 * y_92)) + (m_106.M32 * z_92), ((m_106.M13 * x_97) + (m_106.M23 * y_92)) + (m_106.M33 * z_92));
        a_137 = Pnt_$ctor_Z7AD9E565_2(v_23.X, v_23.Y, v_23.Z);
        let b_143;
        const v_24 = to__2;
        b_143 = Pnt_$ctor_Z7AD9E565_2(v_24.X, v_24.Y, v_24.Z);
        const same_33 = ((a_139 = a_137, (b_145 = b_143, (x_98 = (a_139.X - b_145.X), (y_93 = (a_139.Y - b_145.Y), (z_93 = (a_139.Z - b_145.Z), Math.sqrt(((x_98 * x_98) + (y_93 * y_93)) + (z_93 * z_93)))))))) < 1E-12;
        if (!same_33) {
            Expect_isTrue(same_33)(`${"perpendicular rotation X to Z"} expected: 
${Pnt__get_AsString(b_143)}, got: 
${Pnt__get_AsString(a_137)}`);
        }
        else {
            Expect_isTrue(same_33)("perpendicular rotation X to Z");
        }
        Test_TestCaseBuilder__Zero(builder$0040_38);
    }));
})(), (() => {
    const builder$0040_39 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix createVecToVec with arbitrary unit vectors", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_39, Test_TestCaseBuilder__Delay_1505(builder$0040_39, () => {
        let a_143, b_149, x_106, y_99, z_99;
        let from_3;
        const x_99 = 1;
        const y_94 = 2;
        const z_94 = 3;
        const l_5 = Math.sqrt(((x_99 * x_99) + (y_94 * y_94)) + (z_94 * z_94));
        if (!(l_5 > 1E-12)) {
            failUnit3("UnitVec.create", x_99, y_94, z_94);
        }
        const li_5 = 1 / l_5;
        from_3 = UnitVec_$ctor_Z7AD9E565(li_5 * x_99, li_5 * y_94, li_5 * z_94);
        let to__3;
        const x_102 = 3;
        const y_96 = 1;
        const z_96 = 2;
        const l_6 = Math.sqrt(((x_102 * x_102) + (y_96 * y_96)) + (z_96 * z_96));
        if (!(l_6 > 1E-12)) {
            failUnit3("UnitVec.create", x_102, y_96, z_96);
        }
        const li_6 = 1 / l_6;
        to__3 = UnitVec_$ctor_Z7AD9E565(li_6 * x_102, li_6 * y_96, li_6 * z_96);
        let a_141;
        let v_26;
        const v_25 = from_3;
        const m_108 = Matrix_createVecToVec_6319FE20(from_3, to__3);
        const x_105 = v_25.X;
        const y_98 = v_25.Y;
        const z_98 = v_25.Z;
        v_26 = Vec_$ctor_Z7AD9E565_2(((m_108.M11 * x_105) + (m_108.M21 * y_98)) + (m_108.M31 * z_98), ((m_108.M12 * x_105) + (m_108.M22 * y_98)) + (m_108.M32 * z_98), ((m_108.M13 * x_105) + (m_108.M23 * y_98)) + (m_108.M33 * z_98));
        a_141 = Pnt_$ctor_Z7AD9E565_2(v_26.X, v_26.Y, v_26.Z);
        let b_147;
        const v_27 = to__3;
        b_147 = Pnt_$ctor_Z7AD9E565_2(v_27.X, v_27.Y, v_27.Z);
        const same_34 = ((a_143 = a_141, (b_149 = b_147, (x_106 = (a_143.X - b_149.X), (y_99 = (a_143.Y - b_149.Y), (z_99 = (a_143.Z - b_149.Z), Math.sqrt(((x_106 * x_106) + (y_99 * y_99)) + (z_99 * z_99)))))))) < 1E-12;
        if (!same_34) {
            Expect_isTrue(same_34)(`${"arbitrary unit vector rotation"} expected: 
${Pnt__get_AsString(b_147)}, got: 
${Pnt__get_AsString(a_141)}`);
        }
        else {
            Expect_isTrue(same_34)("arbitrary unit vector rotation");
        }
        Test_TestCaseBuilder__Zero(builder$0040_39);
    }));
})(), (() => {
    const builder$0040_40 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix createVecToVec Vec overload preserves orthogonality", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_40, Test_TestCaseBuilder__Delay_1505(builder$0040_40, () => {
        let a_144, b_150, v_28, m_110, x_107, y_100, z_100;
        const from_4 = Vec_$ctor_Z7AD9E565(2, 0, 0);
        const m_109 = Matrix_createVecToVec_5A694120(from_4, Vec_$ctor_Z7AD9E565(0, 3, 0));
        Expect_floatClose(AccuracyModule_high, (a_144 = from_4, (b_150 = ((v_28 = Vec_$ctor_Z7AD9E565(0, 0, 1), (m_110 = m_109, (x_107 = v_28.X, (y_100 = v_28.Y, (z_100 = v_28.Z, Vec_$ctor_Z7AD9E565_2(((m_110.M11 * x_107) + (m_110.M21 * y_100)) + (m_110.M31 * z_100), ((m_110.M12 * x_107) + (m_110.M22 * y_100)) + (m_110.M32 * z_100), ((m_110.M13 * x_107) + (m_110.M23 * y_100)) + (m_110.M33 * z_100)))))))), ((a_144.X * b_150.X) + (a_144.Y * b_150.Y)) + (a_144.Z * b_150.Z))), 0, "perpendicular vector stays perpendicular");
        Test_TestCaseBuilder__Zero(builder$0040_40);
    }));
})(), (() => {
    const builder$0040_41 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix createVecToVec small angle rotation numerical stability", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_41, Test_TestCaseBuilder__Delay_1505(builder$0040_41, () => {
        let a_146, v_30, v_29, m_112, x_112, y_104, z_104, b_152, v_31, x_113, y_105, z_105;
        const from_5 = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
        let to__5;
        const l_7 = Math.sqrt(((0.99999999 * 0.99999999) + (0.0001 * 0.0001)) + (0 * 0));
        if (!(l_7 > 1E-12)) {
            failUnit3("UnitVec.create", 0.99999999, 0.0001, 0);
        }
        const li_7 = 1 / l_7;
        to__5 = UnitVec_$ctor_Z7AD9E565(li_7 * 0.99999999, li_7 * 0.0001, li_7 * 0);
        Expect_floatClose(AccuracyModule_medium, (a_146 = ((v_30 = ((v_29 = from_5, (m_112 = Matrix_createVecToVec_6319FE20(from_5, to__5), (x_112 = v_29.X, (y_104 = v_29.Y, (z_104 = v_29.Z, Vec_$ctor_Z7AD9E565_2(((m_112.M11 * x_112) + (m_112.M21 * y_104)) + (m_112.M31 * z_104), ((m_112.M12 * x_112) + (m_112.M22 * y_104)) + (m_112.M32 * z_104), ((m_112.M13 * x_112) + (m_112.M23 * y_104)) + (m_112.M33 * z_104)))))))), Pnt_$ctor_Z7AD9E565_2(v_30.X, v_30.Y, v_30.Z))), (b_152 = ((v_31 = to__5, Pnt_$ctor_Z7AD9E565_2(v_31.X, v_31.Y, v_31.Z))), (x_113 = (a_146.X - b_152.X), (y_105 = (a_146.Y - b_152.Y), (z_105 = (a_146.Z - b_152.Z), Math.sqrt(((x_113 * x_113) + (y_105 * y_105)) + (z_105 * z_105))))))), 0, "small angle rotation numerically stable");
        Test_TestCaseBuilder__Zero(builder$0040_41);
    }));
})(), (() => {
    const builder$0040_42 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix createVecToVec with different length vectors", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_42, Test_TestCaseBuilder__Delay_1505(builder$0040_42, () => {
        let v_33, a_150, b_156, x_121, y_111, z_111;
        const from_6 = Vec_$ctor_Z7AD9E565(5, 0, 0);
        const to__6 = Vec_$ctor_Z7AD9E565(0, 0, 3);
        let result_6;
        const v_32 = from_6;
        const m_114 = Matrix_createVecToVec_5A694120(from_6, to__6);
        const x_114 = v_32.X;
        const y_106 = v_32.Y;
        const z_106 = v_32.Z;
        result_6 = Vec_$ctor_Z7AD9E565_2(((m_114.M11 * x_114) + (m_114.M21 * y_106)) + (m_114.M31 * z_106), ((m_114.M12 * x_114) + (m_114.M22 * y_106)) + (m_114.M32 * z_106), ((m_114.M13 * x_114) + (m_114.M23 * y_106)) + (m_114.M33 * z_106));
        Expect_floatClose(AccuracyModule_high, (v_33 = result_6, Math.sqrt(((v_33.X * v_33.X) + (v_33.Y * v_33.Y)) + (v_33.Z * v_33.Z))), 5, "rotation preserves from vector length");
        let resultDir;
        const v_34 = result_6;
        const x_115 = v_34.X;
        const y_107 = v_34.Y;
        const z_107 = v_34.Z;
        const l_8 = Math.sqrt(((x_115 * x_115) + (y_107 * y_107)) + (z_107 * z_107));
        if (!(l_8 > 1E-12)) {
            failUnit3_1("Vec.Unitized", x_115, y_107, z_107);
        }
        const f_6 = 1 / l_8;
        resultDir = UnitVec_$ctor_Z7AD9E565(f_6 * x_115, f_6 * y_107, f_6 * z_107);
        let toDir;
        const v_35 = to__6;
        const x_118 = v_35.X;
        const y_109 = v_35.Y;
        const z_109 = v_35.Z;
        const l_9 = Math.sqrt(((x_118 * x_118) + (y_109 * y_109)) + (z_109 * z_109));
        if (!(l_9 > 1E-12)) {
            failUnit3_1("Vec.Unitized", x_118, y_109, z_109);
        }
        const f_7 = 1 / l_9;
        toDir = UnitVec_$ctor_Z7AD9E565(f_7 * x_118, f_7 * y_109, f_7 * z_109);
        let a_148;
        const v_36 = resultDir;
        a_148 = Pnt_$ctor_Z7AD9E565_2(v_36.X, v_36.Y, v_36.Z);
        let b_154;
        const v_37 = toDir;
        b_154 = Pnt_$ctor_Z7AD9E565_2(v_37.X, v_37.Y, v_37.Z);
        const same_35 = ((a_150 = a_148, (b_156 = b_154, (x_121 = (a_150.X - b_156.X), (y_111 = (a_150.Y - b_156.Y), (z_111 = (a_150.Z - b_156.Z), Math.sqrt(((x_121 * x_121) + (y_111 * y_111)) + (z_111 * z_111)))))))) < 1E-12;
        if (!same_35) {
            Expect_isTrue(same_35)(`${"rotation aligns direction"} expected: 
${Pnt__get_AsString(b_154)}, got: 
${Pnt__get_AsString(a_148)}`);
        }
        else {
            Expect_isTrue(same_35)("rotation aligns direction");
        }
        Test_TestCaseBuilder__Zero(builder$0040_42);
    }));
})(), (() => {
    const builder$0040_43 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix createVecToVec is inverse of reverse rotation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_43, Test_TestCaseBuilder__Delay_1505(builder$0040_43, () => {
        let from_7;
        const x_122 = 1;
        const y_112 = 1;
        const z_112 = 0;
        const l_10 = Math.sqrt(((x_122 * x_122) + (y_112 * y_112)) + (z_112 * z_112));
        if (!(l_10 > 1E-12)) {
            failUnit3("UnitVec.create", x_122, y_112, z_112);
        }
        const li_8 = 1 / l_10;
        from_7 = UnitVec_$ctor_Z7AD9E565(li_8 * x_122, li_8 * y_112, li_8 * z_112);
        let to__7;
        const x_125 = 0;
        const y_114 = 1;
        const z_114 = 1;
        const l_11 = Math.sqrt(((x_125 * x_125) + (y_114 * y_114)) + (z_114 * z_114));
        if (!(l_11 > 1E-12)) {
            failUnit3("UnitVec.create", x_125, y_114, z_114);
        }
        const li_9 = 1 / l_11;
        to__7 = UnitVec_$ctor_Z7AD9E565(li_9 * x_125, li_9 * y_114, li_9 * z_114);
        Expect_isTrue(Matrix__get_IsIdentity(Matrix_multiply_Z11D053C0_1(Matrix_createVecToVec_6319FE20(from_7, to__7), Matrix_createVecToVec_6319FE20(to__7, from_7))))("forward then reverse gives identity");
        Test_TestCaseBuilder__Zero(builder$0040_43);
    }));
})(), (() => {
    const builder$0040_44 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix createVecToVec maintains handedness", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_44, Test_TestCaseBuilder__Delay_1505(builder$0040_44, () => {
        let a_151, v_38, m_116, x_130, y_118, z_118, b_157;
        const m_115 = Matrix_createVecToVec_6319FE20(UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0));
        Expect_floatClose(AccuracyModule_high, (a_151 = ((v_38 = UnitVec_$ctor_Z7AD9E565(0, 0, 1), (m_116 = m_115, (x_130 = v_38.X, (y_118 = v_38.Y, (z_118 = v_38.Z, Vec_$ctor_Z7AD9E565_2(((m_116.M11 * x_130) + (m_116.M21 * y_118)) + (m_116.M31 * z_118), ((m_116.M12 * x_130) + (m_116.M22 * y_118)) + (m_116.M32 * z_118), ((m_116.M13 * x_130) + (m_116.M23 * y_118)) + (m_116.M33 * z_118)))))))), (b_157 = UnitVec_$ctor_Z7AD9E565(0, 0, 1), ((a_151.X * b_157.X) + (a_151.Y * b_157.Y)) + (a_151.Z * b_157.Z))), 1, "maintains right-handed coordinate system");
        Test_TestCaseBuilder__Zero(builder$0040_44);
    }));
})(), (() => {
    const builder$0040_45 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix inverse of zero determinant should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_45, Test_TestCaseBuilder__Delay_1505(builder$0040_45, () => {
        const m_117 = Matrix_$ctor_Z61E40B00(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        Expect_throws(() => {
            Matrix__get_Inverse(m_117);
        }, "zero determinant should throw");
        Test_TestCaseBuilder__Zero(builder$0040_45);
    }));
})(), (() => {
    const builder$0040_46 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix inverse of nearly singular should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_46, Test_TestCaseBuilder__Delay_1505(builder$0040_46, () => {
        const m_118 = Matrix_$ctor_Z61E40B00(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1E-17, 0, 0, 0, 0, 1);
        Expect_throws(() => {
            Matrix__get_Inverse(m_118);
        }, "nearly zero determinant should throw");
        Test_TestCaseBuilder__Zero(builder$0040_46);
    }));
})(), (() => {
    const builder$0040_47 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix rotation axis zero-length should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_47, Test_TestCaseBuilder__Delay_1505(builder$0040_47, () => {
        const axis_6 = Vec_$ctor_Z7AD9E565(0, 0, 0);
        Expect_throws(() => {
            Matrix_createRotationAxis_Z3D1F83EE(axis_6, 45);
        }, "zero-length axis should throw");
        Test_TestCaseBuilder__Zero(builder$0040_47);
    }));
})(), (() => {
    const builder$0040_48 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix rotation axis very short should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_48, Test_TestCaseBuilder__Delay_1505(builder$0040_48, () => {
        const axis_7 = Vec_$ctor_Z7AD9E565(1E-13, 0, 0);
        Expect_throws(() => {
            Matrix_createRotationAxis_Z3D1F83EE(axis_7, 45);
        }, "very short axis should throw");
        Test_TestCaseBuilder__Zero(builder$0040_48);
    }));
})(), (() => {
    const builder$0040_49 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix perspective projection IsProjecting", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_49, Test_TestCaseBuilder__Delay_1505(builder$0040_49, () => {
        const m_119 = Matrix_createPerspective_77D16AC0(800, 600, 0.1, 100);
        Expect_isTrue(Matrix__get_IsProjecting(m_119))("perspective matrix should be projecting");
        Expect_isFalse(Matrix__get_IsAffine(m_119))("perspective matrix should not be affine");
        Test_TestCaseBuilder__Zero(builder$0040_49);
    }));
})(), (() => {
    const builder$0040_50 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix perspective negative near plane should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_50, Test_TestCaseBuilder__Delay_1505(builder$0040_50, () => {
        Expect_throws(() => {
            Matrix_createPerspective_77D16AC0(800, 600, -0.1, 100);
        }, "negative near plane should throw");
        Test_TestCaseBuilder__Zero(builder$0040_50);
    }));
})(), (() => {
    const builder$0040_51 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix perspective zero near plane should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_51, Test_TestCaseBuilder__Delay_1505(builder$0040_51, () => {
        Expect_throws(() => {
            Matrix_createPerspective_77D16AC0(800, 600, 0, 100);
        }, "zero near plane should throw");
        Test_TestCaseBuilder__Zero(builder$0040_51);
    }));
})(), (() => {
    const builder$0040_52 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix perspective negative far plane should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_52, Test_TestCaseBuilder__Delay_1505(builder$0040_52, () => {
        Expect_throws(() => {
            Matrix_createPerspective_77D16AC0(800, 600, 0.1, -100);
        }, "negative far plane should throw");
        Test_TestCaseBuilder__Zero(builder$0040_52);
    }));
})(), (() => {
    const builder$0040_53 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix perspective near >= far should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_53, Test_TestCaseBuilder__Delay_1505(builder$0040_53, () => {
        Expect_throws(() => {
            Matrix_createPerspective_77D16AC0(800, 600, 100, 50);
        }, "near >= far should throw");
        Test_TestCaseBuilder__Zero(builder$0040_53);
    }));
})(), (() => {
    const builder$0040_54 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix perspective near == far should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_54, Test_TestCaseBuilder__Delay_1505(builder$0040_54, () => {
        Expect_throws(() => {
            Matrix_createPerspective_77D16AC0(800, 600, 100, 100);
        }, "near == far should throw");
        Test_TestCaseBuilder__Zero(builder$0040_54);
    }));
})(), (() => {
    const builder$0040_55 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix perspective valid parameters", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_55, Test_TestCaseBuilder__Delay_1505(builder$0040_55, () => {
        let a_154, p_64, m_121, x_133, y_121, z_121, sc_34, b_160, x_134, y_122, z_122;
        const m_120 = Matrix_createPerspective_77D16AC0(800, 600, 0.1, 100);
        Expect_isTrue(Matrix__get_IsProjecting(m_120))("valid perspective should not throw");
        Expect_isTrue(((a_154 = ((p_64 = Pnt_$ctor_Z7AD9E565(1, 1, 10), (m_121 = m_120, (x_133 = p_64.X, (y_121 = p_64.Y, (z_121 = p_64.Z, (sc_34 = (1 / ((((m_121.M14 * x_133) + (m_121.M24 * y_121)) + (m_121.M34 * z_121)) + m_121.M44)), Pnt_$ctor_Z7AD9E565_1(((((m_121.M11 * x_133) + (m_121.M21 * y_121)) + (m_121.M31 * z_121)) + m_121.X41) * sc_34, ((((m_121.M12 * x_133) + (m_121.M22 * y_121)) + (m_121.M32 * z_121)) + m_121.Y42) * sc_34, ((((m_121.M13 * x_133) + (m_121.M23 * y_121)) + (m_121.M33 * z_121)) + m_121.Z43) * sc_34)))))))), (b_160 = Pnt_$ctor_Z7AD9E565_1(0, 0, 0), (x_134 = (a_154.X - b_160.X), (y_122 = (a_154.Y - b_160.Y), (z_122 = (a_154.Z - b_160.Z), Math.sqrt(((x_134 * x_134) + (y_122 * y_122)) + (z_122 * z_122)))))))) > 0)("perspective transformation should work");
        Test_TestCaseBuilder__Zero(builder$0040_55);
    }));
})(), (() => {
    const builder$0040_56 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix from row array wrong length should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_56, Test_TestCaseBuilder__Delay_1505(builder$0040_56, () => {
        const wrongArray = new Float64Array([1, 2, 3]);
        Expect_throws(() => {
            Matrix_createFromRowMajorArray_52AF8430(wrongArray);
        }, "wrong array length should throw");
        Test_TestCaseBuilder__Zero(builder$0040_56);
    }));
})(), (() => {
    const builder$0040_57 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix from column array wrong length should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_57, Test_TestCaseBuilder__Delay_1505(builder$0040_57, () => {
        const wrongArray_1 = new Float64Array([1, 2, 3, 4, 5]);
        Expect_throws(() => {
            Matrix_createFromColumMajorArray_52AF8430(wrongArray_1);
        }, "wrong array length should throw");
        Test_TestCaseBuilder__Zero(builder$0040_57);
    }));
})(), (() => {
    const builder$0040_58 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix from row array too many elements should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_58, Test_TestCaseBuilder__Delay_1505(builder$0040_58, () => {
        const wrongArray_2 = initialize(20, (value_14) => value_14, Float64Array);
        Expect_throws(() => {
            Matrix_createFromRowMajorArray_52AF8430(wrongArray_2);
        }, "too many array elements should throw");
        Test_TestCaseBuilder__Zero(builder$0040_58);
    }));
})(), (() => {
    const builder$0040_59 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix mirror on XY plane", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_59, Test_TestCaseBuilder__Delay_1505(builder$0040_59, () => {
        let a_158, b_165, x_139, y_127, z_127;
        const m_122 = Matrix_createMirror_2F29039F(PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1)));
        let a_156;
        const p_65 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        const m_123 = m_122;
        const x_138 = p_65.X;
        const y_126 = p_65.Y;
        const z_126 = p_65.Z;
        const sc_35 = 1 / ((((m_123.M14 * x_138) + (m_123.M24 * y_126)) + (m_123.M34 * z_126)) + m_123.M44);
        a_156 = Pnt_$ctor_Z7AD9E565_1(((((m_123.M11 * x_138) + (m_123.M21 * y_126)) + (m_123.M31 * z_126)) + m_123.X41) * sc_35, ((((m_123.M12 * x_138) + (m_123.M22 * y_126)) + (m_123.M32 * z_126)) + m_123.Y42) * sc_35, ((((m_123.M13 * x_138) + (m_123.M23 * y_126)) + (m_123.M33 * z_126)) + m_123.Z43) * sc_35);
        const b_163 = Pnt_$ctor_Z7AD9E565(1, 2, -3);
        const same_36 = ((a_158 = a_156, (b_165 = b_163, (x_139 = (a_158.X - b_165.X), (y_127 = (a_158.Y - b_165.Y), (z_127 = (a_158.Z - b_165.Z), Math.sqrt(((x_139 * x_139) + (y_127 * y_127)) + (z_127 * z_127)))))))) < 1E-12;
        if (!same_36) {
            Expect_isTrue(same_36)(`${"mirror on XY plane"} expected: 
${Pnt__get_AsString(b_163)}, got: 
${Pnt__get_AsString(a_156)}`);
        }
        else {
            Expect_isTrue(same_36)("mirror on XY plane");
        }
        Expect_isTrue(Math.abs(Matrix__get_Determinant(m_122) + 1) < 1E-12)("mirror determinant is -1");
        Expect_isTrue(Matrix__get_IsMirroring(m_122))("mirror IsMirroring");
        Test_TestCaseBuilder__Zero(builder$0040_59);
    }));
})(), (() => {
    const builder$0040_60 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix mirror on Right plane", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_60, Test_TestCaseBuilder__Delay_1505(builder$0040_60, () => {
        let a_162, b_170, x_144, y_132, z_132;
        const m_124 = Matrix_createMirror_2F29039F(PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1), UnitVec_$ctor_Z7AD9E565(1, 0, 0)));
        let a_160;
        const p_66 = Pnt_$ctor_Z7AD9E565(3, 2, 1);
        const m_125 = m_124;
        const x_143 = p_66.X;
        const y_131 = p_66.Y;
        const z_131 = p_66.Z;
        const sc_36 = 1 / ((((m_125.M14 * x_143) + (m_125.M24 * y_131)) + (m_125.M34 * z_131)) + m_125.M44);
        a_160 = Pnt_$ctor_Z7AD9E565_1(((((m_125.M11 * x_143) + (m_125.M21 * y_131)) + (m_125.M31 * z_131)) + m_125.X41) * sc_36, ((((m_125.M12 * x_143) + (m_125.M22 * y_131)) + (m_125.M32 * z_131)) + m_125.Y42) * sc_36, ((((m_125.M13 * x_143) + (m_125.M23 * y_131)) + (m_125.M33 * z_131)) + m_125.Z43) * sc_36);
        const b_168 = Pnt_$ctor_Z7AD9E565(-3, 2, 1);
        const same_37 = ((a_162 = a_160, (b_170 = b_168, (x_144 = (a_162.X - b_170.X), (y_132 = (a_162.Y - b_170.Y), (z_132 = (a_162.Z - b_170.Z), Math.sqrt(((x_144 * x_144) + (y_132 * y_132)) + (z_132 * z_132)))))))) < 1E-12;
        if (!same_37) {
            Expect_isTrue(same_37)(`${"mirror on Right plane flips X"} expected: 
${Pnt__get_AsString(b_168)}, got: 
${Pnt__get_AsString(a_160)}`);
        }
        else {
            Expect_isTrue(same_37)("mirror on Right plane flips X");
        }
        Test_TestCaseBuilder__Zero(builder$0040_60);
    }));
})(), (() => {
    const builder$0040_61 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix plane to plane transformation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_61, Test_TestCaseBuilder__Delay_1505(builder$0040_61, () => {
        let a_166, b_175, x_152, y_140, z_140;
        const m_126 = Matrix_createPlaneToPlane_3B6074E0(PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1)), PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565(1, 2, 3), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1)));
        let a_164;
        const p_67 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const m_127 = m_126;
        const x_151 = p_67.X;
        const y_139 = p_67.Y;
        const z_139 = p_67.Z;
        const sc_37 = 1 / ((((m_127.M14 * x_151) + (m_127.M24 * y_139)) + (m_127.M34 * z_139)) + m_127.M44);
        a_164 = Pnt_$ctor_Z7AD9E565_1(((((m_127.M11 * x_151) + (m_127.M21 * y_139)) + (m_127.M31 * z_139)) + m_127.X41) * sc_37, ((((m_127.M12 * x_151) + (m_127.M22 * y_139)) + (m_127.M32 * z_139)) + m_127.Y42) * sc_37, ((((m_127.M13 * x_151) + (m_127.M23 * y_139)) + (m_127.M33 * z_139)) + m_127.Z43) * sc_37);
        const b_173 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        const same_38 = ((a_166 = a_164, (b_175 = b_173, (x_152 = (a_166.X - b_175.X), (y_140 = (a_166.Y - b_175.Y), (z_140 = (a_166.Z - b_175.Z), Math.sqrt(((x_152 * x_152) + (y_140 * y_140)) + (z_140 * z_140)))))))) < 1E-12;
        if (!same_38) {
            Expect_isTrue(same_38)(`${"plane to plane origin"} expected: 
${Pnt__get_AsString(b_173)}, got: 
${Pnt__get_AsString(a_164)}`);
        }
        else {
            Expect_isTrue(same_38)("plane to plane origin");
        }
        Test_TestCaseBuilder__Zero(builder$0040_61);
    }));
})(), (() => {
    const builder$0040_62 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix plane to plane with rotation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_62, Test_TestCaseBuilder__Delay_1505(builder$0040_62, () => {
        let v_39, a_170, b_180, x_161, y_149, z_149;
        const m_128 = Matrix_createPlaneToPlane_3B6074E0(PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1)), PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), (v_39 = UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(-v_39.X, -v_39.Y, -v_39.Z)), UnitVec_$ctor_Z7AD9E565(0, 0, 1)));
        let a_168;
        const p_68 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const m_129 = m_128;
        const x_160 = p_68.X;
        const y_148 = p_68.Y;
        const z_148 = p_68.Z;
        const sc_38 = 1 / ((((m_129.M14 * x_160) + (m_129.M24 * y_148)) + (m_129.M34 * z_148)) + m_129.M44);
        a_168 = Pnt_$ctor_Z7AD9E565_1(((((m_129.M11 * x_160) + (m_129.M21 * y_148)) + (m_129.M31 * z_148)) + m_129.X41) * sc_38, ((((m_129.M12 * x_160) + (m_129.M22 * y_148)) + (m_129.M32 * z_148)) + m_129.Y42) * sc_38, ((((m_129.M13 * x_160) + (m_129.M23 * y_148)) + (m_129.M33 * z_148)) + m_129.Z43) * sc_38);
        const b_178 = Pnt_$ctor_Z7AD9E565(0, 1, 0);
        const same_39 = ((a_170 = a_168, (b_180 = b_178, (x_161 = (a_170.X - b_180.X), (y_149 = (a_170.Y - b_180.Y), (z_149 = (a_170.Z - b_180.Z), Math.sqrt(((x_161 * x_161) + (y_149 * y_149)) + (z_149 * z_149)))))))) < 1E-12;
        if (!same_39) {
            Expect_isTrue(same_39)(`${"plane to plane rotation"} expected: 
${Pnt__get_AsString(b_178)}, got: 
${Pnt__get_AsString(a_168)}`);
        }
        else {
            Expect_isTrue(same_39)("plane to plane rotation");
        }
        Test_TestCaseBuilder__Zero(builder$0040_62);
    }));
})(), (() => {
    const builder$0040_63 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix shear multiple components", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_63, Test_TestCaseBuilder__Delay_1505(builder$0040_63, () => {
        let a_174, b_184, x_163, y_151, z_151;
        const m_130 = Matrix_createShear_76A78260(0.5, 0.3, 0.2, 0.4, 0.1, 0.6);
        let a_172;
        const p_69 = Pnt_$ctor_Z7AD9E565(1, 1, 1);
        const m_131 = m_130;
        const x_162 = p_69.X;
        const y_150 = p_69.Y;
        const z_150 = p_69.Z;
        const sc_39 = 1 / ((((m_131.M14 * x_162) + (m_131.M24 * y_150)) + (m_131.M34 * z_150)) + m_131.M44);
        a_172 = Pnt_$ctor_Z7AD9E565_1(((((m_131.M11 * x_162) + (m_131.M21 * y_150)) + (m_131.M31 * z_150)) + m_131.X41) * sc_39, ((((m_131.M12 * x_162) + (m_131.M22 * y_150)) + (m_131.M32 * z_150)) + m_131.Y42) * sc_39, ((((m_131.M13 * x_162) + (m_131.M23 * y_150)) + (m_131.M33 * z_150)) + m_131.Z43) * sc_39);
        const b_182 = Pnt_$ctor_Z7AD9E565((1 + 0.2) + 0.1, (1 + 0.5) + 0.6, (1 + 0.3) + 0.4);
        const same_40 = ((a_174 = a_172, (b_184 = b_182, (x_163 = (a_174.X - b_184.X), (y_151 = (a_174.Y - b_184.Y), (z_151 = (a_174.Z - b_184.Z), Math.sqrt(((x_163 * x_163) + (y_151 * y_151)) + (z_151 * z_151)))))))) < 1E-12;
        if (!same_40) {
            Expect_isTrue(same_40)(`${"multiple shear components"} expected: 
${Pnt__get_AsString(b_182)}, got: 
${Pnt__get_AsString(a_172)}`);
        }
        else {
            Expect_isTrue(same_40)("multiple shear components");
        }
        Test_TestCaseBuilder__Zero(builder$0040_63);
    }));
})(), (() => {
    const builder$0040_64 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix shear docstring verification xy=3", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_64, Test_TestCaseBuilder__Delay_1505(builder$0040_64, () => {
        let a_178, b_188, x_165, y_153, z_153;
        const m_132 = Matrix_createShear_76A78260(3, 0, 0, 0, 0, 0);
        let a_176;
        const p_70 = Pnt_$ctor_Z7AD9E565(2, 1, 3);
        const m_133 = m_132;
        const x_164 = p_70.X;
        const y_152 = p_70.Y;
        const z_152 = p_70.Z;
        const sc_40 = 1 / ((((m_133.M14 * x_164) + (m_133.M24 * y_152)) + (m_133.M34 * z_152)) + m_133.M44);
        a_176 = Pnt_$ctor_Z7AD9E565_1(((((m_133.M11 * x_164) + (m_133.M21 * y_152)) + (m_133.M31 * z_152)) + m_133.X41) * sc_40, ((((m_133.M12 * x_164) + (m_133.M22 * y_152)) + (m_133.M32 * z_152)) + m_133.Y42) * sc_40, ((((m_133.M13 * x_164) + (m_133.M23 * y_152)) + (m_133.M33 * z_152)) + m_133.Z43) * sc_40);
        const b_186 = Pnt_$ctor_Z7AD9E565(2, 7, 3);
        const same_41 = ((a_178 = a_176, (b_188 = b_186, (x_165 = (a_178.X - b_188.X), (y_153 = (a_178.Y - b_188.Y), (z_153 = (a_178.Z - b_188.Z), Math.sqrt(((x_165 * x_165) + (y_153 * y_153)) + (z_153 * z_153)))))))) < 1E-12;
        if (!same_41) {
            Expect_isTrue(same_41)(`${"shear docstring verification xy=3"} expected: 
${Pnt__get_AsString(b_186)}, got: 
${Pnt__get_AsString(a_176)}`);
        }
        else {
            Expect_isTrue(same_41)("shear docstring verification xy=3");
        }
        Test_TestCaseBuilder__Zero(builder$0040_64);
    }));
})(), (() => {
    const builder$0040_65 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Matrix shear docstring verification zx=0.5", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_65, Test_TestCaseBuilder__Delay_1505(builder$0040_65, () => {
        let a_182, b_192, x_167, y_155, z_155;
        const m_134 = Matrix_createShear_76A78260(0, 0, 0, 0, 0.5, 0);
        const a_179 = Pnt_$ctor_Z7AD9E565(2, 1, 3);
        let a_180;
        const p_71 = a_179;
        const m_135 = m_134;
        const x_166 = p_71.X;
        const y_154 = p_71.Y;
        const z_154 = p_71.Z;
        const sc_41 = 1 / ((((m_135.M14 * x_166) + (m_135.M24 * y_154)) + (m_135.M34 * z_154)) + m_135.M44);
        a_180 = Pnt_$ctor_Z7AD9E565_1(((((m_135.M11 * x_166) + (m_135.M21 * y_154)) + (m_135.M31 * z_154)) + m_135.X41) * sc_41, ((((m_135.M12 * x_166) + (m_135.M22 * y_154)) + (m_135.M32 * z_154)) + m_135.Y42) * sc_41, ((((m_135.M13 * x_166) + (m_135.M23 * y_154)) + (m_135.M33 * z_154)) + m_135.Z43) * sc_41);
        const b_190 = Pnt_$ctor_Z7AD9E565(a_179.X + (0.5 * a_179.Z), a_179.Y, a_179.Z);
        const same_42 = ((a_182 = a_180, (b_192 = b_190, (x_167 = (a_182.X - b_192.X), (y_155 = (a_182.Y - b_192.Y), (z_155 = (a_182.Z - b_192.Z), Math.sqrt(((x_167 * x_167) + (y_155 * y_155)) + (z_155 * z_155)))))))) < 1E-12;
        if (!same_42) {
            Expect_isTrue(same_42)(`${"shear docstring verification zx=0.5"} expected: 
${Pnt__get_AsString(b_190)}, got: 
${Pnt__get_AsString(a_180)}`);
        }
        else {
            Expect_isTrue(same_42)("shear docstring verification zx=0.5");
        }
        Test_TestCaseBuilder__Zero(builder$0040_65);
    }));
})()]));

