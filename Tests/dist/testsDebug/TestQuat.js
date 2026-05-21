
import { AccuracyModule_low, Test_TestCaseBuilder__For_Z371464DD, Test_TestCaseBuilder__Combine_3A59D1F3, Expect_isFalse, Expect_throws, AccuracyModule_medium, AccuracyModule_high, Expect_floatClose, Test_TestCaseBuilder__Zero, Expect_isTrue, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Quaternion_create_77D16AC0, Quaternion__setAngleInRadians_5E38073B, Quaternion_multiply_400F31E0, Quaternion_$ctor_77D16AC0, Quaternion_createFromRadians_Z3D1F83EE } from "./Src/Quaternion.js";
import { Vec_$ctor_Z7AD9E565 } from "./Src/Vec.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_1 } from "./Src/Vec.js";
import { Pnt__get_AsString, Pnt_$ctor_Z7AD9E565 } from "./Src/Pnt.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_1 } from "./Src/Pnt.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_2 } from "./Src/Vec.js";
import { UnitVec_$ctor_Z7AD9E565 } from "./Src/UnitVec.js";
import { fail } from "./Src/EuclidErrors.js";
import { interpolate } from "./fable_modules/fable-library-js.5.0.0/String.js";
import { Quaternion_createVecToVec_5A694120, Quaternion__setAngleInRadians_5E38073B as Quaternion__setAngleInRadians_5E38073B_1, Quaternion_createFromRadians_Z3D1F83EE as Quaternion_createFromRadians_Z3D1F83EE_1, Quaternion_create_77D16AC0 as Quaternion_create_77D16AC0_1, Quaternion_toEulerAnglesZYX_Z2A007687, Quaternion_createFromEulerZYX_Z7AD9E565, Quaternion_createFromEulerXYZ_Z7AD9E565 } from "./Src/Quaternion.js";
import { failTooSmall } from "./Src/EuclidErrors.js";
import { rangeDouble } from "./fable_modules/fable-library-js.5.0.0/Range.js";
import { ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";

export const tests = Test_testList("Quaternion transformations", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion 90 z", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let a, a_4, b_3, x_1, y_1, z_1, a_7, b_6, x_3, y_3, z_3;
        const q = Quaternion_createFromRadians_Z3D1F83EE((a = Vec_$ctor_Z7AD9E565(0, 0, 1), Vec_$ctor_Z7AD9E565_1(a.X * 9, a.Y * 9, a.Z * 9)), 0.017453292519943295 * 90);
        const a_1 = Pnt_$ctor_Z7AD9E565(9, 0, 3);
        let b;
        const p = a_1;
        const q_1 = q;
        const x = p.X;
        const y = p.Y;
        const z = p.Z;
        const qx = q_1.X;
        const qy = q_1.Y;
        const qz = q_1.Z;
        const qw = q_1.W;
        const tx = 2 * ((qy * z) - (qz * y));
        const ty = 2 * ((qz * x) - (qx * z));
        const tz = 2 * ((qx * y) - (qy * x));
        b = Pnt_$ctor_Z7AD9E565_1(((x + (qw * tx)) + (qy * tz)) - (qz * ty), ((y + (qw * ty)) + (qz * tx)) - (qx * tz), ((z + (qw * tz)) + (qx * ty)) - (qy * tx));
        const expected = Pnt_$ctor_Z7AD9E565(0, 9, 3);
        const a_2 = b;
        const b_1 = expected;
        const same = ((a_4 = a_2, (b_3 = b_1, (x_1 = (a_4.X - b_3.X), (y_1 = (a_4.Y - b_3.Y), (z_1 = (a_4.Z - b_3.Z), Math.sqrt(((x_1 * x_1) + (y_1 * y_1)) + (z_1 * z_1)))))))) < 1E-12;
        if (!same) {
            Expect_isTrue(same)(`${"*** z "} expected: 
${Pnt__get_AsString(b_1)}, got: 
${Pnt__get_AsString(a_2)}`);
        }
        else {
            Expect_isTrue(same)("*** z ");
        }
        let c;
        const p_1 = a_1;
        const q_4 = q;
        const x_2 = p_1.X;
        const y_2 = p_1.Y;
        const z_2 = p_1.Z;
        const qx_1 = q_4.X;
        const qy_1 = q_4.Y;
        const qz_1 = q_4.Z;
        const qw_1 = q_4.W;
        const tx_1 = 2 * ((qy_1 * z_2) - (qz_1 * y_2));
        const ty_1 = 2 * ((qz_1 * x_2) - (qx_1 * z_2));
        const tz_1 = 2 * ((qx_1 * y_2) - (qy_1 * x_2));
        c = Pnt_$ctor_Z7AD9E565_1(((x_2 + (qw_1 * tx_1)) + (qy_1 * tz_1)) - (qz_1 * ty_1), ((y_2 + (qw_1 * ty_1)) + (qz_1 * tx_1)) - (qx_1 * tz_1), ((z_2 + (qw_1 * tz_1)) + (qx_1 * ty_1)) - (qy_1 * tx_1));
        const a_5 = c;
        const b_4 = expected;
        const same_1 = ((a_7 = a_5, (b_6 = b_4, (x_3 = (a_7.X - b_6.X), (y_3 = (a_7.Y - b_6.Y), (z_3 = (a_7.Z - b_6.Z), Math.sqrt(((x_3 * x_3) + (y_3 * y_3)) + (z_3 * z_3)))))))) < 1E-12;
        if (!same_1) {
            Expect_isTrue(same_1)(`${"rotateByQuaternion z"} expected: 
${Pnt__get_AsString(b_4)}, got: 
${Pnt__get_AsString(a_5)}`);
        }
        else {
            Expect_isTrue(same_1)("rotateByQuaternion z");
        }
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion 90 x", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let a_8, a_12, b_10, x_5, y_5, z_5, a_15, b_13, x_7, y_7, z_7;
        const q_5 = Quaternion_createFromRadians_Z3D1F83EE((a_8 = Vec_$ctor_Z7AD9E565(1, 0, 0), Vec_$ctor_Z7AD9E565_1(a_8.X * 9, a_8.Y * 9, a_8.Z * 9)), 0.017453292519943295 * 90);
        const a_9 = Pnt_$ctor_Z7AD9E565(0, 9, 0);
        let b_7;
        const p_2 = a_9;
        const q_6 = q_5;
        const x_4 = p_2.X;
        const y_4 = p_2.Y;
        const z_4 = p_2.Z;
        const qx_2 = q_6.X;
        const qy_2 = q_6.Y;
        const qz_2 = q_6.Z;
        const qw_2 = q_6.W;
        const tx_2 = 2 * ((qy_2 * z_4) - (qz_2 * y_4));
        const ty_2 = 2 * ((qz_2 * x_4) - (qx_2 * z_4));
        const tz_2 = 2 * ((qx_2 * y_4) - (qy_2 * x_4));
        b_7 = Pnt_$ctor_Z7AD9E565_1(((x_4 + (qw_2 * tx_2)) + (qy_2 * tz_2)) - (qz_2 * ty_2), ((y_4 + (qw_2 * ty_2)) + (qz_2 * tx_2)) - (qx_2 * tz_2), ((z_4 + (qw_2 * tz_2)) + (qx_2 * ty_2)) - (qy_2 * tx_2));
        const expected_1 = Pnt_$ctor_Z7AD9E565(0, 0, 9);
        const a_10 = b_7;
        const b_8 = expected_1;
        const same_2 = ((a_12 = a_10, (b_10 = b_8, (x_5 = (a_12.X - b_10.X), (y_5 = (a_12.Y - b_10.Y), (z_5 = (a_12.Z - b_10.Z), Math.sqrt(((x_5 * x_5) + (y_5 * y_5)) + (z_5 * z_5)))))))) < 1E-12;
        if (!same_2) {
            Expect_isTrue(same_2)(`${"*** x"} expected: 
${Pnt__get_AsString(b_8)}, got: 
${Pnt__get_AsString(a_10)}`);
        }
        else {
            Expect_isTrue(same_2)("*** x");
        }
        let c_1;
        const p_3 = a_9;
        const q_9 = q_5;
        const x_6 = p_3.X;
        const y_6 = p_3.Y;
        const z_6 = p_3.Z;
        const qx_3 = q_9.X;
        const qy_3 = q_9.Y;
        const qz_3 = q_9.Z;
        const qw_3 = q_9.W;
        const tx_3 = 2 * ((qy_3 * z_6) - (qz_3 * y_6));
        const ty_3 = 2 * ((qz_3 * x_6) - (qx_3 * z_6));
        const tz_3 = 2 * ((qx_3 * y_6) - (qy_3 * x_6));
        c_1 = Pnt_$ctor_Z7AD9E565_1(((x_6 + (qw_3 * tx_3)) + (qy_3 * tz_3)) - (qz_3 * ty_3), ((y_6 + (qw_3 * ty_3)) + (qz_3 * tx_3)) - (qx_3 * tz_3), ((z_6 + (qw_3 * tz_3)) + (qx_3 * ty_3)) - (qy_3 * tx_3));
        const a_13 = c_1;
        const b_11 = expected_1;
        const same_3 = ((a_15 = a_13, (b_13 = b_11, (x_7 = (a_15.X - b_13.X), (y_7 = (a_15.Y - b_13.Y), (z_7 = (a_15.Z - b_13.Z), Math.sqrt(((x_7 * x_7) + (y_7 * y_7)) + (z_7 * z_7)))))))) < 1E-12;
        if (!same_3) {
            Expect_isTrue(same_3)(`${"rotateByQuaternion x"} expected: 
${Pnt__get_AsString(b_11)}, got: 
${Pnt__get_AsString(a_13)}`);
        }
        else {
            Expect_isTrue(same_3)("rotateByQuaternion x");
        }
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion 90 y", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let a_19, b_17, x_9, y_9, z_9, a_22, b_20, x_11, y_11, z_11;
        const q_10 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 1, 0), 0.017453292519943295 * 90);
        const a_16 = Pnt_$ctor_Z7AD9E565(9, 0, 0);
        let b_14;
        const p_4 = a_16;
        const q_11 = q_10;
        const x_8 = p_4.X;
        const y_8 = p_4.Y;
        const z_8 = p_4.Z;
        const qx_4 = q_11.X;
        const qy_4 = q_11.Y;
        const qz_4 = q_11.Z;
        const qw_4 = q_11.W;
        const tx_4 = 2 * ((qy_4 * z_8) - (qz_4 * y_8));
        const ty_4 = 2 * ((qz_4 * x_8) - (qx_4 * z_8));
        const tz_4 = 2 * ((qx_4 * y_8) - (qy_4 * x_8));
        b_14 = Pnt_$ctor_Z7AD9E565_1(((x_8 + (qw_4 * tx_4)) + (qy_4 * tz_4)) - (qz_4 * ty_4), ((y_8 + (qw_4 * ty_4)) + (qz_4 * tx_4)) - (qx_4 * tz_4), ((z_8 + (qw_4 * tz_4)) + (qx_4 * ty_4)) - (qy_4 * tx_4));
        const expected_2 = Pnt_$ctor_Z7AD9E565(0, 0, -9);
        const a_17 = b_14;
        const b_15 = expected_2;
        const same_4 = ((a_19 = a_17, (b_17 = b_15, (x_9 = (a_19.X - b_17.X), (y_9 = (a_19.Y - b_17.Y), (z_9 = (a_19.Z - b_17.Z), Math.sqrt(((x_9 * x_9) + (y_9 * y_9)) + (z_9 * z_9)))))))) < 1E-12;
        if (!same_4) {
            Expect_isTrue(same_4)(`${"*** y"} expected: 
${Pnt__get_AsString(b_15)}, got: 
${Pnt__get_AsString(a_17)}`);
        }
        else {
            Expect_isTrue(same_4)("*** y");
        }
        let c_2;
        const p_5 = a_16;
        const q_14 = q_10;
        const x_10 = p_5.X;
        const y_10 = p_5.Y;
        const z_10 = p_5.Z;
        const qx_5 = q_14.X;
        const qy_5 = q_14.Y;
        const qz_5 = q_14.Z;
        const qw_5 = q_14.W;
        const tx_5 = 2 * ((qy_5 * z_10) - (qz_5 * y_10));
        const ty_5 = 2 * ((qz_5 * x_10) - (qx_5 * z_10));
        const tz_5 = 2 * ((qx_5 * y_10) - (qy_5 * x_10));
        c_2 = Pnt_$ctor_Z7AD9E565_1(((x_10 + (qw_5 * tx_5)) + (qy_5 * tz_5)) - (qz_5 * ty_5), ((y_10 + (qw_5 * ty_5)) + (qz_5 * tx_5)) - (qx_5 * tz_5), ((z_10 + (qw_5 * tz_5)) + (qx_5 * ty_5)) - (qy_5 * tx_5));
        const a_20 = c_2;
        const b_18 = expected_2;
        const same_5 = ((a_22 = a_20, (b_20 = b_18, (x_11 = (a_22.X - b_20.X), (y_11 = (a_22.Y - b_20.Y), (z_11 = (a_22.Z - b_20.Z), Math.sqrt(((x_11 * x_11) + (y_11 * y_11)) + (z_11 * z_11)))))))) < 1E-12;
        if (!same_5) {
            Expect_isTrue(same_5)(`${"rotateByQuaternion y"} expected: 
${Pnt__get_AsString(b_18)}, got: 
${Pnt__get_AsString(a_20)}`);
        }
        else {
            Expect_isTrue(same_5)("rotateByQuaternion y");
        }
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion 90 inverse", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let a_23, a_27, b_24, x_15, y_15, z_15, a_30, b_27, x_18, y_18, z_18;
        const q_15 = Quaternion_createFromRadians_Z3D1F83EE((a_23 = Vec_$ctor_Z7AD9E565(0, 0, 1), Vec_$ctor_Z7AD9E565_1(a_23.X * 9, a_23.Y * 9, a_23.Z * 9)), 0.017453292519943295 * 90);
        const a_24 = Pnt_$ctor_Z7AD9E565(9, 0, 3);
        let b_21;
        const p_6 = a_24;
        const q_16 = q_15;
        const x_12 = p_6.X;
        const y_12 = p_6.Y;
        const z_12 = p_6.Z;
        const qx_6 = q_16.X;
        const qy_6 = q_16.Y;
        const qz_6 = q_16.Z;
        const qw_6 = q_16.W;
        const tx_6 = 2 * ((qy_6 * z_12) - (qz_6 * y_12));
        const ty_6 = 2 * ((qz_6 * x_12) - (qx_6 * z_12));
        const tz_6 = 2 * ((qx_6 * y_12) - (qy_6 * x_12));
        b_21 = Pnt_$ctor_Z7AD9E565_1(((x_12 + (qw_6 * tx_6)) + (qy_6 * tz_6)) - (qz_6 * ty_6), ((y_12 + (qw_6 * ty_6)) + (qz_6 * tx_6)) - (qx_6 * tz_6), ((z_12 + (qw_6 * tz_6)) + (qx_6 * ty_6)) - (qy_6 * tx_6));
        let iq;
        const q_17 = q_15;
        iq = Quaternion_$ctor_77D16AC0(-q_17.X, -q_17.Y, -q_17.Z, q_17.W);
        let c_3;
        const p_7 = b_21;
        const q_18 = iq;
        const x_14 = p_7.X;
        const y_14 = p_7.Y;
        const z_14 = p_7.Z;
        const qx_7 = q_18.X;
        const qy_7 = q_18.Y;
        const qz_7 = q_18.Z;
        const qw_7 = q_18.W;
        const tx_7 = 2 * ((qy_7 * z_14) - (qz_7 * y_14));
        const ty_7 = 2 * ((qz_7 * x_14) - (qx_7 * z_14));
        const tz_7 = 2 * ((qx_7 * y_14) - (qy_7 * x_14));
        c_3 = Pnt_$ctor_Z7AD9E565_1(((x_14 + (qw_7 * tx_7)) + (qy_7 * tz_7)) - (qz_7 * ty_7), ((y_14 + (qw_7 * ty_7)) + (qz_7 * tx_7)) - (qx_7 * tz_7), ((z_14 + (qw_7 * tz_7)) + (qx_7 * ty_7)) - (qy_7 * tx_7));
        const a_25 = a_24;
        const b_22 = c_3;
        const same_6 = ((a_27 = a_25, (b_24 = b_22, (x_15 = (a_27.X - b_24.X), (y_15 = (a_27.Y - b_24.Y), (z_15 = (a_27.Z - b_24.Z), Math.sqrt(((x_15 * x_15) + (y_15 * y_15)) + (z_15 * z_15)))))))) < 1E-12;
        if (!same_6) {
            Expect_isTrue(same_6)(`${"*** inverse"} expected: 
${Pnt__get_AsString(b_22)}, got: 
${Pnt__get_AsString(a_25)}`);
        }
        else {
            Expect_isTrue(same_6)("*** inverse");
        }
        let d;
        let p_9;
        const p_8 = a_24;
        const q_21 = q_15;
        const x_16 = p_8.X;
        const y_16 = p_8.Y;
        const z_16 = p_8.Z;
        const qx_8 = q_21.X;
        const qy_8 = q_21.Y;
        const qz_8 = q_21.Z;
        const qw_8 = q_21.W;
        const tx_8 = 2 * ((qy_8 * z_16) - (qz_8 * y_16));
        const ty_8 = 2 * ((qz_8 * x_16) - (qx_8 * z_16));
        const tz_8 = 2 * ((qx_8 * y_16) - (qy_8 * x_16));
        p_9 = Pnt_$ctor_Z7AD9E565_1(((x_16 + (qw_8 * tx_8)) + (qy_8 * tz_8)) - (qz_8 * ty_8), ((y_16 + (qw_8 * ty_8)) + (qz_8 * tx_8)) - (qx_8 * tz_8), ((z_16 + (qw_8 * tz_8)) + (qx_8 * ty_8)) - (qy_8 * tx_8));
        const q_24 = iq;
        const x_17 = p_9.X;
        const y_17 = p_9.Y;
        const z_17 = p_9.Z;
        const qx_9 = q_24.X;
        const qy_9 = q_24.Y;
        const qz_9 = q_24.Z;
        const qw_9 = q_24.W;
        const tx_9 = 2 * ((qy_9 * z_17) - (qz_9 * y_17));
        const ty_9 = 2 * ((qz_9 * x_17) - (qx_9 * z_17));
        const tz_9 = 2 * ((qx_9 * y_17) - (qy_9 * x_17));
        d = Pnt_$ctor_Z7AD9E565_1(((x_17 + (qw_9 * tx_9)) + (qy_9 * tz_9)) - (qz_9 * ty_9), ((y_17 + (qw_9 * ty_9)) + (qz_9 * tx_9)) - (qx_9 * tz_9), ((z_17 + (qw_9 * tz_9)) + (qx_9 * ty_9)) - (qy_9 * tx_9));
        const a_28 = a_24;
        const b_25 = d;
        const same_7 = ((a_30 = a_28, (b_27 = b_25, (x_18 = (a_30.X - b_27.X), (y_18 = (a_30.Y - b_27.Y), (z_18 = (a_30.Z - b_27.Z), Math.sqrt(((x_18 * x_18) + (y_18 * y_18)) + (z_18 * z_18)))))))) < 1E-12;
        if (!same_7) {
            Expect_isTrue(same_7)(`${"rotateByQuaternion inverse"} expected: 
${Pnt__get_AsString(b_25)}, got: 
${Pnt__get_AsString(a_28)}`);
        }
        else {
            Expect_isTrue(same_7)("rotateByQuaternion inverse");
        }
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion 180 degrees", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let a_34, b_31, x_20, y_20, z_20;
        const q_25 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 180);
        const a_31 = Pnt_$ctor_Z7AD9E565(5, 3, 7);
        let b_28;
        const p_10 = a_31;
        const q_26 = q_25;
        const x_19 = p_10.X;
        const y_19 = p_10.Y;
        const z_19 = p_10.Z;
        const qx_10 = q_26.X;
        const qy_10 = q_26.Y;
        const qz_10 = q_26.Z;
        const qw_10 = q_26.W;
        const tx_10 = 2 * ((qy_10 * z_19) - (qz_10 * y_19));
        const ty_10 = 2 * ((qz_10 * x_19) - (qx_10 * z_19));
        const tz_10 = 2 * ((qx_10 * y_19) - (qy_10 * x_19));
        b_28 = Pnt_$ctor_Z7AD9E565_1(((x_19 + (qw_10 * tx_10)) + (qy_10 * tz_10)) - (qz_10 * ty_10), ((y_19 + (qw_10 * ty_10)) + (qz_10 * tx_10)) - (qx_10 * tz_10), ((z_19 + (qw_10 * tz_10)) + (qx_10 * ty_10)) - (qy_10 * tx_10));
        const expected_3 = Pnt_$ctor_Z7AD9E565(-5, -3, 7);
        const a_32 = b_28;
        const b_29 = expected_3;
        const same_8 = ((a_34 = a_32, (b_31 = b_29, (x_20 = (a_34.X - b_31.X), (y_20 = (a_34.Y - b_31.Y), (z_20 = (a_34.Z - b_31.Z), Math.sqrt(((x_20 * x_20) + (y_20 * y_20)) + (z_20 * z_20)))))))) < 1E-12;
        if (!same_8) {
            Expect_isTrue(same_8)(`${"180° rotation z"} expected: 
${Pnt__get_AsString(b_29)}, got: 
${Pnt__get_AsString(a_32)}`);
        }
        else {
            Expect_isTrue(same_8)("180° rotation z");
        }
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion 45 degrees", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let a_38, b_35, x_22, y_22, z_22;
        const q_27 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 45);
        const a_35 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        let b_32;
        const p_11 = a_35;
        const q_28 = q_27;
        const x_21 = p_11.X;
        const y_21 = p_11.Y;
        const z_21 = p_11.Z;
        const qx_11 = q_28.X;
        const qy_11 = q_28.Y;
        const qz_11 = q_28.Z;
        const qw_11 = q_28.W;
        const tx_11 = 2 * ((qy_11 * z_21) - (qz_11 * y_21));
        const ty_11 = 2 * ((qz_11 * x_21) - (qx_11 * z_21));
        const tz_11 = 2 * ((qx_11 * y_21) - (qy_11 * x_21));
        b_32 = Pnt_$ctor_Z7AD9E565_1(((x_21 + (qw_11 * tx_11)) + (qy_11 * tz_11)) - (qz_11 * ty_11), ((y_21 + (qw_11 * ty_11)) + (qz_11 * tx_11)) - (qx_11 * tz_11), ((z_21 + (qw_11 * tz_11)) + (qx_11 * ty_11)) - (qy_11 * tx_11));
        const sqrt2_2 = Math.sqrt(2) / 2;
        const expected_4 = Pnt_$ctor_Z7AD9E565(sqrt2_2, sqrt2_2, 0);
        const a_36 = b_32;
        const b_33 = expected_4;
        const same_9 = ((a_38 = a_36, (b_35 = b_33, (x_22 = (a_38.X - b_35.X), (y_22 = (a_38.Y - b_35.Y), (z_22 = (a_38.Z - b_35.Z), Math.sqrt(((x_22 * x_22) + (y_22 * y_22)) + (z_22 * z_22)))))))) < 1E-12;
        if (!same_9) {
            Expect_isTrue(same_9)(`${"45° rotation z"} expected: 
${Pnt__get_AsString(b_33)}, got: 
${Pnt__get_AsString(a_36)}`);
        }
        else {
            Expect_isTrue(same_9)("45° rotation z");
        }
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion arbitrary axis", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        let a_42, b_39, x_24, y_24, z_24;
        const axis_6 = Vec_$ctor_Z7AD9E565_2(1, 1, 1);
        const q_29 = Quaternion_createFromRadians_Z3D1F83EE(axis_6, 0.017453292519943295 * 120);
        const a_39 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        let b_36;
        const p_12 = a_39;
        const q_30 = q_29;
        const x_23 = p_12.X;
        const y_23 = p_12.Y;
        const z_23 = p_12.Z;
        const qx_12 = q_30.X;
        const qy_12 = q_30.Y;
        const qz_12 = q_30.Z;
        const qw_12 = q_30.W;
        const tx_12 = 2 * ((qy_12 * z_23) - (qz_12 * y_23));
        const ty_12 = 2 * ((qz_12 * x_23) - (qx_12 * z_23));
        const tz_12 = 2 * ((qx_12 * y_23) - (qy_12 * x_23));
        b_36 = Pnt_$ctor_Z7AD9E565_1(((x_23 + (qw_12 * tx_12)) + (qy_12 * tz_12)) - (qz_12 * ty_12), ((y_23 + (qw_12 * ty_12)) + (qz_12 * tx_12)) - (qx_12 * tz_12), ((z_23 + (qw_12 * tz_12)) + (qx_12 * ty_12)) - (qy_12 * tx_12));
        const expected_5 = Pnt_$ctor_Z7AD9E565(0, 1, 0);
        const a_40 = b_36;
        const b_37 = expected_5;
        const same_10 = ((a_42 = a_40, (b_39 = b_37, (x_24 = (a_42.X - b_39.X), (y_24 = (a_42.Y - b_39.Y), (z_24 = (a_42.Z - b_39.Z), Math.sqrt(((x_24 * x_24) + (y_24 * y_24)) + (z_24 * z_24)))))))) < 1E-12;
        if (!same_10) {
            Expect_isTrue(same_10)(`${"120° around (1,1,1)"} expected: 
${Pnt__get_AsString(b_37)}, got: 
${Pnt__get_AsString(a_40)}`);
        }
        else {
            Expect_isTrue(same_10)("120° around (1,1,1)");
        }
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion multiplication composition", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let a_46, b_42, x_27, y_27, z_27;
        const q1 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 45);
        const q2 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 45);
        const qCombined = Quaternion_multiply_400F31E0(q1, q2);
        const qDirect = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 90);
        const a_43 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        let result1;
        const p_13 = a_43;
        const q_31 = qCombined;
        const x_25 = p_13.X;
        const y_25 = p_13.Y;
        const z_25 = p_13.Z;
        const qx_13 = q_31.X;
        const qy_13 = q_31.Y;
        const qz_13 = q_31.Z;
        const qw_13 = q_31.W;
        const tx_13 = 2 * ((qy_13 * z_25) - (qz_13 * y_25));
        const ty_13 = 2 * ((qz_13 * x_25) - (qx_13 * z_25));
        const tz_13 = 2 * ((qx_13 * y_25) - (qy_13 * x_25));
        result1 = Pnt_$ctor_Z7AD9E565_1(((x_25 + (qw_13 * tx_13)) + (qy_13 * tz_13)) - (qz_13 * ty_13), ((y_25 + (qw_13 * ty_13)) + (qz_13 * tx_13)) - (qx_13 * tz_13), ((z_25 + (qw_13 * tz_13)) + (qx_13 * ty_13)) - (qy_13 * tx_13));
        let result2;
        const p_14 = a_43;
        const q_32 = qDirect;
        const x_26 = p_14.X;
        const y_26 = p_14.Y;
        const z_26 = p_14.Z;
        const qx_14 = q_32.X;
        const qy_14 = q_32.Y;
        const qz_14 = q_32.Z;
        const qw_14 = q_32.W;
        const tx_14 = 2 * ((qy_14 * z_26) - (qz_14 * y_26));
        const ty_14 = 2 * ((qz_14 * x_26) - (qx_14 * z_26));
        const tz_14 = 2 * ((qx_14 * y_26) - (qy_14 * x_26));
        result2 = Pnt_$ctor_Z7AD9E565_1(((x_26 + (qw_14 * tx_14)) + (qy_14 * tz_14)) - (qz_14 * ty_14), ((y_26 + (qw_14 * ty_14)) + (qz_14 * tx_14)) - (qx_14 * tz_14), ((z_26 + (qw_14 * tz_14)) + (qx_14 * ty_14)) - (qy_14 * tx_14));
        const a_44 = result1;
        const b_40 = result2;
        const same_11 = ((a_46 = a_44, (b_42 = b_40, (x_27 = (a_46.X - b_42.X), (y_27 = (a_46.Y - b_42.Y), (z_27 = (a_46.Z - b_42.Z), Math.sqrt(((x_27 * x_27) + (y_27 * y_27)) + (z_27 * z_27)))))))) < 1E-12;
        if (!same_11) {
            Expect_isTrue(same_11)(`${"quaternion multiplication"} expected: 
${Pnt__get_AsString(b_40)}, got: 
${Pnt__get_AsString(a_44)}`);
        }
        else {
            Expect_isTrue(same_11)("quaternion multiplication");
        }
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion identity", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let a_50, b_46, x_30, y_30, z_30;
        const q_33 = Quaternion_$ctor_77D16AC0(0, 0, 0, 1);
        const a_47 = Pnt_$ctor_Z7AD9E565(5, 3, 7);
        let b_43;
        const p_15 = a_47;
        const q_34 = q_33;
        const x_29 = p_15.X;
        const y_29 = p_15.Y;
        const z_29 = p_15.Z;
        const qx_15 = q_34.X;
        const qy_15 = q_34.Y;
        const qz_15 = q_34.Z;
        const qw_15 = q_34.W;
        const tx_15 = 2 * ((qy_15 * z_29) - (qz_15 * y_29));
        const ty_15 = 2 * ((qz_15 * x_29) - (qx_15 * z_29));
        const tz_15 = 2 * ((qx_15 * y_29) - (qy_15 * x_29));
        b_43 = Pnt_$ctor_Z7AD9E565_1(((x_29 + (qw_15 * tx_15)) + (qy_15 * tz_15)) - (qz_15 * ty_15), ((y_29 + (qw_15 * ty_15)) + (qz_15 * tx_15)) - (qx_15 * tz_15), ((z_29 + (qw_15 * tz_15)) + (qx_15 * ty_15)) - (qy_15 * tx_15));
        const a_48 = a_47;
        const b_44 = b_43;
        const same_12 = ((a_50 = a_48, (b_46 = b_44, (x_30 = (a_50.X - b_46.X), (y_30 = (a_50.Y - b_46.Y), (z_30 = (a_50.Z - b_46.Z), Math.sqrt(((x_30 * x_30) + (y_30 * y_30)) + (z_30 * z_30)))))))) < 1E-12;
        if (!same_12) {
            Expect_isTrue(same_12)(`${"identity quaternion"} expected: 
${Pnt__get_AsString(b_44)}, got: 
${Pnt__get_AsString(a_48)}`);
        }
        else {
            Expect_isTrue(same_12)("identity quaternion");
        }
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion angle properties", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        let d_1, x_32, d_2, x_35;
        const q_35 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(1, 0, 0), 0.017453292519943295 * 60);
        Expect_floatClose(AccuracyModule_high, 57.29577951308232 * (2 * ((d_1 = ((x_32 = q_35.W, (x_32 > -1) ? ((x_32 < 1) ? x_32 : 1) : -1)), Math.acos(d_1)))), 60, "angle in degrees");
        Expect_floatClose(AccuracyModule_high, 2 * ((d_2 = ((x_35 = q_35.W, (x_35 > -1) ? ((x_35 < 1) ? x_35 : 1) : -1)), Math.acos(d_2))), 3.141592653589793 / 3, "angle in radians");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion set angle", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let a_58, b_49, x_38, y_34, z_32;
        const q_39 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 90);
        const q2_1 = Quaternion__setAngleInRadians_5E38073B(q_39, 0.017453292519943295 * 45);
        const a_55 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        let result;
        const p_16 = a_55;
        const q_41 = q2_1;
        const x_37 = p_16.X;
        const y_33 = p_16.Y;
        const z_31 = p_16.Z;
        const qx_16 = q_41.X;
        const qy_16 = q_41.Y;
        const qz_16 = q_41.Z;
        const qw_16 = q_41.W;
        const tx_16 = 2 * ((qy_16 * z_31) - (qz_16 * y_33));
        const ty_16 = 2 * ((qz_16 * x_37) - (qx_16 * z_31));
        const tz_16 = 2 * ((qx_16 * y_33) - (qy_16 * x_37));
        result = Pnt_$ctor_Z7AD9E565_1(((x_37 + (qw_16 * tx_16)) + (qy_16 * tz_16)) - (qz_16 * ty_16), ((y_33 + (qw_16 * ty_16)) + (qz_16 * tx_16)) - (qx_16 * tz_16), ((z_31 + (qw_16 * tz_16)) + (qx_16 * ty_16)) - (qy_16 * tx_16));
        const sqrt2_2_1 = Math.sqrt(2) / 2;
        const expected_6 = Pnt_$ctor_Z7AD9E565(sqrt2_2_1, sqrt2_2_1, 0);
        const a_56 = result;
        const b_47 = expected_6;
        const same_13 = ((a_58 = a_56, (b_49 = b_47, (x_38 = (a_58.X - b_49.X), (y_34 = (a_58.Y - b_49.Y), (z_32 = (a_58.Z - b_49.Z), Math.sqrt(((x_38 * x_38) + (y_34 * y_34)) + (z_32 * z_32)))))))) < 1E-12;
        if (!same_13) {
            Expect_isTrue(same_13)(`${"set angle to 45°"} expected: 
${Pnt__get_AsString(b_47)}, got: 
${Pnt__get_AsString(a_56)}`);
        }
        else {
            Expect_isTrue(same_13)("set angle to 45°");
        }
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion vector to vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        let v_1, v_2, a_61, b_52;
        const from = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
        const toPt = UnitVec_$ctor_Z7AD9E565(0, 1, 0);
        let q_42;
        const vecFrom = from;
        const vecTo = toPt;
        let v;
        const a_59 = vecFrom;
        const b_50 = vecTo;
        v = Vec_$ctor_Z7AD9E565_1(a_59.X - b_50.X, a_59.Y - b_50.Y, a_59.Z - b_50.Z);
        if (((v_1 = v, ((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z))) < 1E-24) {
            q_42 = Quaternion_$ctor_77D16AC0(0, 0, 0, 1);
        }
        else {
            let v_1_1;
            const a_60 = vecFrom;
            const b_51 = vecTo;
            v_1_1 = Vec_$ctor_Z7AD9E565_1(a_60.X + b_51.X, a_60.Y + b_51.Y, a_60.Z + b_51.Z);
            if (!(((v_2 = v_1_1, ((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z))) > 1E-12)) {
                fail(`Quaternion.createVecToVec failed to find a rotation axis for (almost) colinear unit-vectors in opposite directions: ${interpolate("%O%P()", [vecFrom])} and ${interpolate("%O%P()", [vecTo])}`);
            }
            q_42 = Quaternion_create_77D16AC0((vecFrom.Y * vecTo.Z) - (vecFrom.Z * vecTo.Y), (vecFrom.Z * vecTo.X) - (vecFrom.X * vecTo.Z), (vecFrom.X * vecTo.Y) - (vecFrom.Y * vecTo.X), ((a_61 = vecFrom, (b_52 = vecTo, ((a_61.X * b_52.X) + (a_61.Y * b_52.Y)) + (a_61.Z * b_52.Z)))) + 1);
        }
        let result_1;
        const v_3 = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
        const q_43 = q_42;
        const x_43 = v_3.X;
        const y_38 = v_3.Y;
        const z_36 = v_3.Z;
        const qx_17 = q_43.X;
        const qy_17 = q_43.Y;
        const qz_17 = q_43.Z;
        const qw_17 = q_43.W;
        const tx_17 = 2 * ((qy_17 * z_36) - (qz_17 * y_38));
        const ty_17 = 2 * ((qz_17 * x_43) - (qx_17 * z_36));
        const tz_17 = 2 * ((qx_17 * y_38) - (qy_17 * x_43));
        result_1 = UnitVec_$ctor_Z7AD9E565(((x_43 + (qw_17 * tx_17)) + (qy_17 * tz_17)) - (qz_17 * ty_17), ((y_38 + (qw_17 * ty_17)) + (qz_17 * tx_17)) - (qx_17 * tz_17), ((z_36 + (qw_17 * tz_17)) + (qx_17 * ty_17)) - (qy_17 * tx_17));
        const expected_7 = UnitVec_$ctor_Z7AD9E565(0, 1, 0);
        Expect_floatClose(AccuracyModule_high, result_1.X, expected_7.X, "vec to vec X");
        Expect_floatClose(AccuracyModule_high, result_1.Y, expected_7.Y, "vec to vec Y");
        Expect_floatClose(AccuracyModule_high, result_1.Z, expected_7.Z, "vec to vec Z");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion Euler XYZ", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        const q_44 = Quaternion_createFromEulerXYZ_Z7AD9E565(30, 45, 60);
        Expect_floatClose(AccuracyModule_high, (((q_44.X * q_44.X) + (q_44.Y * q_44.Y)) + (q_44.Z * q_44.Z)) + (q_44.W * q_44.W), 1, "quaternion is unit length");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion Euler roundtrip ZYX", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        const originalZ = 30;
        const originalY = 45;
        const originalX = 60;
        const q_45 = Quaternion_createFromEulerZYX_Z7AD9E565(originalZ, originalY, originalX);
        const patternInput_1 = Quaternion_toEulerAnglesZYX_Z2A007687(q_45);
        const z_40 = patternInput_1[0];
        const y_42 = patternInput_1[1];
        const x_47 = patternInput_1[2];
        Expect_floatClose(AccuracyModule_medium, z_40, originalZ, "roundtrip Z");
        Expect_floatClose(AccuracyModule_medium, y_42, originalY, "roundtrip Y");
        Expect_floatClose(AccuracyModule_medium, x_47, originalX, "roundtrip X");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion conjugate equals inverse", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        const q_46 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565_2(1, 2, 3), 0.017453292519943295 * 47);
        let conj;
        const q_47 = q_46;
        conj = Quaternion_$ctor_77D16AC0(-q_47.X, -q_47.Y, -q_47.Z, q_47.W);
        let inv;
        const q_48 = q_46;
        inv = Quaternion_$ctor_77D16AC0(-q_48.X, -q_48.Y, -q_48.Z, q_48.W);
        Expect_floatClose(AccuracyModule_high, conj.X, inv.X, "conjugate X = inverse X");
        Expect_floatClose(AccuracyModule_high, conj.Y, inv.Y, "conjugate Y = inverse Y");
        Expect_floatClose(AccuracyModule_high, conj.Z, inv.Z, "conjugate Z = inverse Z");
        Expect_floatClose(AccuracyModule_high, conj.W, inv.W, "conjugate W = inverse W");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion multiple rotations", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        let a_65, b_55, x_51, y_46, z_44;
        const q1_1 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(1, 0, 0), 0.017453292519943295 * 90);
        const q2_2 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 1, 0), 0.017453292519943295 * 90);
        const q3 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 90);
        const a_62 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        let result_2;
        const p_17 = a_62;
        const q_49 = Quaternion_multiply_400F31E0(q1_1, Quaternion_multiply_400F31E0(q2_2, q3));
        const x_50 = p_17.X;
        const y_45 = p_17.Y;
        const z_43 = p_17.Z;
        const qx_18 = q_49.X;
        const qy_18 = q_49.Y;
        const qz_18 = q_49.Z;
        const qw_18 = q_49.W;
        const tx_18 = 2 * ((qy_18 * z_43) - (qz_18 * y_45));
        const ty_18 = 2 * ((qz_18 * x_50) - (qx_18 * z_43));
        const tz_18 = 2 * ((qx_18 * y_45) - (qy_18 * x_50));
        result_2 = Pnt_$ctor_Z7AD9E565_1(((x_50 + (qw_18 * tx_18)) + (qy_18 * tz_18)) - (qz_18 * ty_18), ((y_45 + (qw_18 * ty_18)) + (qz_18 * tx_18)) - (qx_18 * tz_18), ((z_43 + (qw_18 * tz_18)) + (qx_18 * ty_18)) - (qy_18 * tx_18));
        const expected_8 = Pnt_$ctor_Z7AD9E565(0, 0, 1);
        const a_63 = result_2;
        const b_53 = expected_8;
        const same_14 = ((a_65 = a_63, (b_55 = b_53, (x_51 = (a_65.X - b_55.X), (y_46 = (a_65.Y - b_55.Y), (z_44 = (a_65.Z - b_55.Z), Math.sqrt(((x_51 * x_51) + (y_46 * y_46)) + (z_44 * z_44)))))))) < 1E-12;
        if (!same_14) {
            Expect_isTrue(same_14)(`${"multiple rotations"} expected: 
${Pnt__get_AsString(b_53)}, got: 
${Pnt__get_AsString(a_63)}`);
        }
        else {
            Expect_isTrue(same_14)("multiple rotations");
        }
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion multiple rotations2", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let a_69, b_58, x_53, y_48, z_46;
        const q1_2 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(1, 0, 0), 0.017453292519943295 * 90);
        const q2_3 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 1, 0), 0.017453292519943295 * 90);
        const q3_1 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 90);
        const a_66 = Pnt_$ctor_Z7AD9E565(0, 1, 0);
        let result_3;
        const p_18 = a_66;
        const q_50 = Quaternion_multiply_400F31E0(q1_2, Quaternion_multiply_400F31E0(q2_3, q3_1));
        const x_52 = p_18.X;
        const y_47 = p_18.Y;
        const z_45 = p_18.Z;
        const qx_19 = q_50.X;
        const qy_19 = q_50.Y;
        const qz_19 = q_50.Z;
        const qw_19 = q_50.W;
        const tx_19 = 2 * ((qy_19 * z_45) - (qz_19 * y_47));
        const ty_19 = 2 * ((qz_19 * x_52) - (qx_19 * z_45));
        const tz_19 = 2 * ((qx_19 * y_47) - (qy_19 * x_52));
        result_3 = Pnt_$ctor_Z7AD9E565_1(((x_52 + (qw_19 * tx_19)) + (qy_19 * tz_19)) - (qz_19 * ty_19), ((y_47 + (qw_19 * ty_19)) + (qz_19 * tx_19)) - (qx_19 * tz_19), ((z_45 + (qw_19 * tz_19)) + (qx_19 * ty_19)) - (qy_19 * tx_19));
        const expected_9 = Pnt_$ctor_Z7AD9E565(0, -1, 0);
        const a_67 = result_3;
        const b_56 = expected_9;
        const same_15 = ((a_69 = a_67, (b_58 = b_56, (x_53 = (a_69.X - b_58.X), (y_48 = (a_69.Y - b_58.Y), (z_46 = (a_69.Z - b_58.Z), Math.sqrt(((x_53 * x_53) + (y_48 * y_48)) + (z_46 * z_46)))))))) < 1E-12;
        if (!same_15) {
            Expect_isTrue(same_15)(`${"multiple rotations"} expected: 
${Pnt__get_AsString(b_56)}, got: 
${Pnt__get_AsString(a_67)}`);
        }
        else {
            Expect_isTrue(same_15)("multiple rotations");
        }
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion small angles", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        const q_51 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 0.001);
        const a_70 = Pnt_$ctor_Z7AD9E565(1000, 0, 0);
        let b_59;
        const p_19 = a_70;
        const q_52 = q_51;
        const x_54 = p_19.X;
        const y_49 = p_19.Y;
        const z_47 = p_19.Z;
        const qx_20 = q_52.X;
        const qy_20 = q_52.Y;
        const qz_20 = q_52.Z;
        const qw_20 = q_52.W;
        const tx_20 = 2 * ((qy_20 * z_47) - (qz_20 * y_49));
        const ty_20 = 2 * ((qz_20 * x_54) - (qx_20 * z_47));
        const tz_20 = 2 * ((qx_20 * y_49) - (qy_20 * x_54));
        b_59 = Pnt_$ctor_Z7AD9E565_1(((x_54 + (qw_20 * tx_20)) + (qy_20 * tz_20)) - (qz_20 * ty_20), ((y_49 + (qw_20 * ty_20)) + (qz_20 * tx_20)) - (qx_20 * tz_20), ((z_47 + (qw_20 * tz_20)) + (qx_20 * ty_20)) - (qy_20 * tx_20));
        Expect_floatClose(AccuracyModule_medium, b_59.X, a_70.X, "small angle X unchanged");
        Expect_isTrue(Math.abs(b_59.Y) < 1)("small angle Y is small");
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion opposite vectors error", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        const from_1 = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
        let toPt_1;
        const v_4 = Vec_$ctor_Z7AD9E565_2(-1, 0, 0);
        const l_5 = Math.sqrt(((v_4.X * v_4.X) + (v_4.Y * v_4.Y)) + (v_4.Z * v_4.Z));
        if (!(l_5 > 1E-12)) {
            failTooSmall("UnitVec.createFromVec", v_4);
        }
        const li = 1 / l_5;
        toPt_1 = UnitVec_$ctor_Z7AD9E565(li * v_4.X, li * v_4.Y, li * v_4.Z);
        Expect_throws(() => {
            let vecFrom_1, vecTo_1, v_5, a_71, b_60, v_6, v_1_2, a_72, b_61, v_7, a_73, b_62;
            (vecFrom_1 = from_1, (vecTo_1 = toPt_1, (v_5 = ((a_71 = vecFrom_1, (b_60 = vecTo_1, Vec_$ctor_Z7AD9E565_1(a_71.X - b_60.X, a_71.Y - b_60.Y, a_71.Z - b_60.Z)))), (((v_6 = v_5, ((v_6.X * v_6.X) + (v_6.Y * v_6.Y)) + (v_6.Z * v_6.Z))) < 1E-24) ? Quaternion_$ctor_77D16AC0(0, 0, 0, 1) : ((v_1_2 = ((a_72 = vecFrom_1, (b_61 = vecTo_1, Vec_$ctor_Z7AD9E565_1(a_72.X + b_61.X, a_72.Y + b_61.Y, a_72.Z + b_61.Z)))), (!(((v_7 = v_1_2, ((v_7.X * v_7.X) + (v_7.Y * v_7.Y)) + (v_7.Z * v_7.Z))) > 1E-12) ? fail(`Quaternion.createVecToVec failed to find a rotation axis for (almost) colinear unit-vectors in opposite directions: ${interpolate("%O%P()", [vecFrom_1])} and ${interpolate("%O%P()", [vecTo_1])}`) : undefined, Quaternion_create_77D16AC0((vecFrom_1.Y * vecTo_1.Z) - (vecFrom_1.Z * vecTo_1.Y), (vecFrom_1.Z * vecTo_1.X) - (vecFrom_1.X * vecTo_1.Z), (vecFrom_1.X * vecTo_1.Y) - (vecFrom_1.Y * vecTo_1.X), ((a_73 = vecFrom_1, (b_62 = vecTo_1, ((a_73.X * b_62.X) + (a_73.Y * b_62.Y)) + (a_73.Z * b_62.Z)))) + 1)))))));
        }, "opposite vectors should throw");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion create with zero-length should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        Expect_throws(() => {
            Quaternion_create_77D16AC0_1(0, 0, 0, 0);
        }, "zero-length quaternion should fail");
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion create with very small values should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        Expect_throws(() => {
            Quaternion_create_77D16AC0_1(1E-20, 1E-20, 1E-20, 1E-20);
        }, "very small quaternion should fail");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion createFromRadians with zero-length axis should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        const zeroAxis = Vec_$ctor_Z7AD9E565_2(0, 0, 0);
        Expect_throws(() => {
            Quaternion_createFromRadians_Z3D1F83EE_1(zeroAxis, 90);
        }, "zero-length axis should fail");
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion createFromRadians with very short axis should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        const tinyAxis = Vec_$ctor_Z7AD9E565_2(1E-20, 1E-20, 1E-20);
        Expect_throws(() => {
            Quaternion_createFromRadians_Z3D1F83EE_1(tinyAxis, 90);
        }, "very short axis should fail");
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion createFromDegree with very short axis should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        const tinyAxis_1 = Vec_$ctor_Z7AD9E565_2(1E-15, 0, 0);
        Expect_throws(() => {
            Quaternion_createFromRadians_Z3D1F83EE(tinyAxis_1, 0.017453292519943295 * 45);
        }, "very short axis should fail");
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion setAngle on identity should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        const q_53 = Quaternion_$ctor_77D16AC0(0, 0, 0, 1);
        Expect_throws(() => {
            Quaternion__setAngleInRadians_5E38073B_1(q_53, 1);
        }, "setAngle on identity should fail");
        Expect_throws(() => {
            Quaternion__setAngleInRadians_5E38073B(q_53, 0.017453292519943295 * 45);
        }, "setAngle on identity should fail");
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion setAngle on near-identity should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        const q_55 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 1E-10);
        Expect_throws(() => {
            Quaternion__setAngleInRadians_5E38073B_1(q_55, 1);
        }, "setAngle on near-identity should fail");
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion Axis on identity returns zero vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        const q_56 = Quaternion_$ctor_77D16AC0(0, 0, 0, 1);
        let axis_23;
        const q_57 = q_56;
        axis_23 = Vec_$ctor_Z7AD9E565_1(q_57.X, q_57.Y, q_57.Z);
        Expect_floatClose(AccuracyModule_high, axis_23.X, 0, "identity axis X is 0");
        Expect_floatClose(AccuracyModule_high, axis_23.Y, 0, "identity axis Y is 0");
        Expect_floatClose(AccuracyModule_high, axis_23.Z, 0, "identity axis Z is 0");
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion Axis length equals sin(angle/2)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        const q_58 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 60);
        let axis_25;
        const q_59 = q_58;
        axis_25 = Vec_$ctor_Z7AD9E565_1(q_59.X, q_59.Y, q_59.Z);
        const expectedLength = Math.sin(3.141592653589793 / 6);
        const actualLength = Math.sqrt(((axis_25.X * axis_25.X) + (axis_25.Y * axis_25.Y)) + (axis_25.Z * axis_25.Z));
        Expect_floatClose(AccuracyModule_high, actualLength, expectedLength, "axis length is sin(angle/2)");
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion equals with double-coverage (q vs -q)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        let a_75, b_64, a_79, b_67, x_65, y_58, z_56;
        const q_60 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 45);
        const qNeg = Quaternion_$ctor_77D16AC0(-q_60.X, -q_60.Y, -q_60.Z, -q_60.W);
        Expect_isFalse((a_75 = q_60, (b_64 = qNeg, (((Math.abs(a_75.X - b_64.X) <= 1E-10) && (Math.abs(a_75.Y - b_64.Y) <= 1E-10)) && (Math.abs(a_75.Z - b_64.Z) <= 1E-10)) && (Math.abs(a_75.W - b_64.W) <= 1E-10))))("q and -q should be unequal (component-wise)");
        const a_76 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        let result1_1;
        const p_20 = a_76;
        const q_61 = q_60;
        const x_63 = p_20.X;
        const y_56 = p_20.Y;
        const z_54 = p_20.Z;
        const qx_21 = q_61.X;
        const qy_21 = q_61.Y;
        const qz_21 = q_61.Z;
        const qw_21 = q_61.W;
        const tx_21 = 2 * ((qy_21 * z_54) - (qz_21 * y_56));
        const ty_21 = 2 * ((qz_21 * x_63) - (qx_21 * z_54));
        const tz_21 = 2 * ((qx_21 * y_56) - (qy_21 * x_63));
        result1_1 = Pnt_$ctor_Z7AD9E565_1(((x_63 + (qw_21 * tx_21)) + (qy_21 * tz_21)) - (qz_21 * ty_21), ((y_56 + (qw_21 * ty_21)) + (qz_21 * tx_21)) - (qx_21 * tz_21), ((z_54 + (qw_21 * tz_21)) + (qx_21 * ty_21)) - (qy_21 * tx_21));
        let result2_1;
        const p_21 = a_76;
        const q_62 = qNeg;
        const x_64 = p_21.X;
        const y_57 = p_21.Y;
        const z_55 = p_21.Z;
        const qx_22 = q_62.X;
        const qy_22 = q_62.Y;
        const qz_22 = q_62.Z;
        const qw_22 = q_62.W;
        const tx_22 = 2 * ((qy_22 * z_55) - (qz_22 * y_57));
        const ty_22 = 2 * ((qz_22 * x_64) - (qx_22 * z_55));
        const tz_22 = 2 * ((qx_22 * y_57) - (qy_22 * x_64));
        result2_1 = Pnt_$ctor_Z7AD9E565_1(((x_64 + (qw_22 * tx_22)) + (qy_22 * tz_22)) - (qz_22 * ty_22), ((y_57 + (qw_22 * ty_22)) + (qz_22 * tx_22)) - (qx_22 * tz_22), ((z_55 + (qw_22 * tz_22)) + (qx_22 * ty_22)) - (qy_22 * tx_22));
        const a_77 = result1_1;
        const b_65 = result2_1;
        const same_16 = ((a_79 = a_77, (b_67 = b_65, (x_65 = (a_79.X - b_67.X), (y_58 = (a_79.Y - b_67.Y), (z_56 = (a_79.Z - b_67.Z), Math.sqrt(((x_65 * x_65) + (y_58 * y_58)) + (z_56 * z_56)))))))) < 1E-12;
        if (!same_16) {
            Expect_isTrue(same_16)(`${"q and -q produce same rotation"} expected: 
${Pnt__get_AsString(b_65)}, got: 
${Pnt__get_AsString(a_77)}`);
        }
        else {
            Expect_isTrue(same_16)("q and -q produce same rotation");
        }
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion equalsRotation with same quaternion", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        let a_81, b_69, tolerance_4, a_82, b_70, tolerance_5, a_83, b_71;
        const q1_3 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 45);
        const q2_4 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 45);
        Expect_isTrue((a_81 = q1_3, (b_69 = q2_4, ((tolerance_4 = 1E-10, (a_82 = a_81, (b_70 = b_69, (((Math.abs(a_82.X - b_70.X) <= tolerance_4) && (Math.abs(a_82.Y - b_70.Y) <= tolerance_4)) && (Math.abs(a_82.Z - b_70.Z) <= tolerance_4)) && (Math.abs(a_82.W - b_70.W) <= tolerance_4))))) ? true : ((tolerance_5 = 1E-10, (a_83 = a_81, (b_71 = Quaternion_$ctor_77D16AC0(-b_69.X, -b_69.Y, -b_69.Z, -b_69.W), (((Math.abs(a_83.X - b_71.X) <= tolerance_5) && (Math.abs(a_83.Y - b_71.Y) <= tolerance_5)) && (Math.abs(a_83.Z - b_71.Z) <= tolerance_5)) && (Math.abs(a_83.W - b_71.W) <= tolerance_5))))))))("same quaternions should be rotationally equal");
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion equalsRotation with negated quaternion (q vs -q)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        let a_85, b_73, tolerance_8, a_86, b_74, tolerance_9, a_87, b_75, a_89, b_77;
        const q_63 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 45);
        const qNeg_1 = Quaternion_$ctor_77D16AC0(-q_63.X, -q_63.Y, -q_63.Z, -q_63.W);
        Expect_isTrue((a_85 = q_63, (b_73 = qNeg_1, ((tolerance_8 = 1E-10, (a_86 = a_85, (b_74 = b_73, (((Math.abs(a_86.X - b_74.X) <= tolerance_8) && (Math.abs(a_86.Y - b_74.Y) <= tolerance_8)) && (Math.abs(a_86.Z - b_74.Z) <= tolerance_8)) && (Math.abs(a_86.W - b_74.W) <= tolerance_8))))) ? true : ((tolerance_9 = 1E-10, (a_87 = a_85, (b_75 = Quaternion_$ctor_77D16AC0(-b_73.X, -b_73.Y, -b_73.Z, -b_73.W), (((Math.abs(a_87.X - b_75.X) <= tolerance_9) && (Math.abs(a_87.Y - b_75.Y) <= tolerance_9)) && (Math.abs(a_87.Z - b_75.Z) <= tolerance_9)) && (Math.abs(a_87.W - b_75.W) <= tolerance_9))))))))("q and -q should be rotationally equal");
        Expect_isFalse((a_89 = q_63, (b_77 = qNeg_1, (((Math.abs(a_89.X - b_77.X) <= 1E-10) && (Math.abs(a_89.Y - b_77.Y) <= 1E-10)) && (Math.abs(a_89.Z - b_77.Z) <= 1E-10)) && (Math.abs(a_89.W - b_77.W) <= 1E-10))))("q and -q should be component-wise unequal");
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})(), (() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion equalsRotation with different rotations", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        let a_91, b_79, tolerance_14, a_92, b_80, tolerance_15, a_93, b_81;
        const q1_4 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 45);
        const q2_5 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 90);
        Expect_isFalse((a_91 = q1_4, (b_79 = q2_5, ((tolerance_14 = 1E-10, (a_92 = a_91, (b_80 = b_79, (((Math.abs(a_92.X - b_80.X) <= tolerance_14) && (Math.abs(a_92.Y - b_80.Y) <= tolerance_14)) && (Math.abs(a_92.Z - b_80.Z) <= tolerance_14)) && (Math.abs(a_92.W - b_80.W) <= tolerance_14))))) ? true : ((tolerance_15 = 1E-10, (a_93 = a_91, (b_81 = Quaternion_$ctor_77D16AC0(-b_79.X, -b_79.Y, -b_79.Z, -b_79.W), (((Math.abs(a_93.X - b_81.X) <= tolerance_15) && (Math.abs(a_93.Y - b_81.Y) <= tolerance_15)) && (Math.abs(a_93.Z - b_81.Z) <= tolerance_15)) && (Math.abs(a_93.W - b_81.W) <= tolerance_15))))))))("different rotations should not be equal");
        Test_TestCaseBuilder__Zero(builder$0040_31);
    }));
})(), (() => {
    const builder$0040_32 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion equalsRotation with different axes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_32, Test_TestCaseBuilder__Delay_1505(builder$0040_32, () => {
        let a_95, b_83, tolerance_18, a_96, b_84, tolerance_19, a_97, b_85;
        const q1_5 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 45);
        const q2_6 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(1, 0, 0), 0.017453292519943295 * 45);
        Expect_isFalse((a_95 = q1_5, (b_83 = q2_6, ((tolerance_18 = 1E-10, (a_96 = a_95, (b_84 = b_83, (((Math.abs(a_96.X - b_84.X) <= tolerance_18) && (Math.abs(a_96.Y - b_84.Y) <= tolerance_18)) && (Math.abs(a_96.Z - b_84.Z) <= tolerance_18)) && (Math.abs(a_96.W - b_84.W) <= tolerance_18))))) ? true : ((tolerance_19 = 1E-10, (a_97 = a_95, (b_85 = Quaternion_$ctor_77D16AC0(-b_83.X, -b_83.Y, -b_83.Z, -b_83.W), (((Math.abs(a_97.X - b_85.X) <= tolerance_19) && (Math.abs(a_97.Y - b_85.Y) <= tolerance_19)) && (Math.abs(a_97.Z - b_85.Z) <= tolerance_19)) && (Math.abs(a_97.W - b_85.W) <= tolerance_19))))))))("same angle different axis should not be equal");
        Test_TestCaseBuilder__Zero(builder$0040_32);
    }));
})(), (() => {
    const builder$0040_33 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion equalsRotation with tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_33, Test_TestCaseBuilder__Delay_1505(builder$0040_33, () => {
        let a_99, b_87, tolerance_22, a_100, b_88, tolerance_23, a_101, b_89, a_103, b_91, tolerance_26, a_104, b_92, tolerance_27, a_105, b_93;
        const q1_6 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 45);
        const q2_7 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 45.001);
        Expect_isTrue((a_99 = q1_6, (b_87 = q2_7, ((tolerance_22 = 0.01, (a_100 = a_99, (b_88 = b_87, (((Math.abs(a_100.X - b_88.X) <= tolerance_22) && (Math.abs(a_100.Y - b_88.Y) <= tolerance_22)) && (Math.abs(a_100.Z - b_88.Z) <= tolerance_22)) && (Math.abs(a_100.W - b_88.W) <= tolerance_22))))) ? true : ((tolerance_23 = 0.01, (a_101 = a_99, (b_89 = Quaternion_$ctor_77D16AC0(-b_87.X, -b_87.Y, -b_87.Z, -b_87.W), (((Math.abs(a_101.X - b_89.X) <= tolerance_23) && (Math.abs(a_101.Y - b_89.Y) <= tolerance_23)) && (Math.abs(a_101.Z - b_89.Z) <= tolerance_23)) && (Math.abs(a_101.W - b_89.W) <= tolerance_23))))))))("similar quaternions within tolerance");
        Expect_isFalse((a_103 = q1_6, (b_91 = q2_7, ((tolerance_26 = 1E-10, (a_104 = a_103, (b_92 = b_91, (((Math.abs(a_104.X - b_92.X) <= tolerance_26) && (Math.abs(a_104.Y - b_92.Y) <= tolerance_26)) && (Math.abs(a_104.Z - b_92.Z) <= tolerance_26)) && (Math.abs(a_104.W - b_92.W) <= tolerance_26))))) ? true : ((tolerance_27 = 1E-10, (a_105 = a_103, (b_93 = Quaternion_$ctor_77D16AC0(-b_91.X, -b_91.Y, -b_91.Z, -b_91.W), (((Math.abs(a_105.X - b_93.X) <= tolerance_27) && (Math.abs(a_105.Y - b_93.Y) <= tolerance_27)) && (Math.abs(a_105.Z - b_93.Z) <= tolerance_27)) && (Math.abs(a_105.W - b_93.W) <= tolerance_27))))))))("similar quaternions outside tight tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_33);
    }));
})(), (() => {
    const builder$0040_34 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion equalsRotation with negated and tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_34, Test_TestCaseBuilder__Delay_1505(builder$0040_34, () => {
        let a_107, b_95, tolerance_30, a_108, b_96, tolerance_31, a_109, b_97;
        const q1_7 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 45);
        const q2_8 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 45.001);
        const q2Neg = Quaternion_$ctor_77D16AC0(-q2_8.X, -q2_8.Y, -q2_8.Z, -q2_8.W);
        Expect_isTrue((a_107 = q1_7, (b_95 = q2Neg, ((tolerance_30 = 0.01, (a_108 = a_107, (b_96 = b_95, (((Math.abs(a_108.X - b_96.X) <= tolerance_30) && (Math.abs(a_108.Y - b_96.Y) <= tolerance_30)) && (Math.abs(a_108.Z - b_96.Z) <= tolerance_30)) && (Math.abs(a_108.W - b_96.W) <= tolerance_30))))) ? true : ((tolerance_31 = 0.01, (a_109 = a_107, (b_97 = Quaternion_$ctor_77D16AC0(-b_95.X, -b_95.Y, -b_95.Z, -b_95.W), (((Math.abs(a_109.X - b_97.X) <= tolerance_31) && (Math.abs(a_109.Y - b_97.Y) <= tolerance_31)) && (Math.abs(a_109.Z - b_97.Z) <= tolerance_31)) && (Math.abs(a_109.W - b_97.W) <= tolerance_31))))))))("negated similar quaternions within tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_34);
    }));
})(), (() => {
    const builder$0040_35 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion equalsRotation with identity", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_35, Test_TestCaseBuilder__Delay_1505(builder$0040_35, () => {
        let a_111, b_99, tolerance_34, a_112, b_100, tolerance_35, a_113, b_101;
        const q1_8 = Quaternion_$ctor_77D16AC0(0, 0, 0, 1);
        const q2_9 = Quaternion_$ctor_77D16AC0(0, 0, 0, 1);
        Expect_isTrue((a_111 = q1_8, (b_99 = q2_9, ((tolerance_34 = 1E-10, (a_112 = a_111, (b_100 = b_99, (((Math.abs(a_112.X - b_100.X) <= tolerance_34) && (Math.abs(a_112.Y - b_100.Y) <= tolerance_34)) && (Math.abs(a_112.Z - b_100.Z) <= tolerance_34)) && (Math.abs(a_112.W - b_100.W) <= tolerance_34))))) ? true : ((tolerance_35 = 1E-10, (a_113 = a_111, (b_101 = Quaternion_$ctor_77D16AC0(-b_99.X, -b_99.Y, -b_99.Z, -b_99.W), (((Math.abs(a_113.X - b_101.X) <= tolerance_35) && (Math.abs(a_113.Y - b_101.Y) <= tolerance_35)) && (Math.abs(a_113.Z - b_101.Z) <= tolerance_35)) && (Math.abs(a_113.W - b_101.W) <= tolerance_35))))))))("identity quaternions should be equal");
        Test_TestCaseBuilder__Zero(builder$0040_35);
    }));
})(), (() => {
    const builder$0040_36 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion equalsRotation with zero tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_36, Test_TestCaseBuilder__Delay_1505(builder$0040_36, () => {
        let a_115, b_103, tolerance_38, a_116, b_104, tolerance_39, a_117, b_105;
        const q_64 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 45);
        const qNeg_2 = Quaternion_$ctor_77D16AC0(-q_64.X, -q_64.Y, -q_64.Z, -q_64.W);
        Expect_isTrue((a_115 = q_64, (b_103 = qNeg_2, ((tolerance_38 = 0, (a_116 = a_115, (b_104 = b_103, (((Math.abs(a_116.X - b_104.X) <= tolerance_38) && (Math.abs(a_116.Y - b_104.Y) <= tolerance_38)) && (Math.abs(a_116.Z - b_104.Z) <= tolerance_38)) && (Math.abs(a_116.W - b_104.W) <= tolerance_38))))) ? true : ((tolerance_39 = 0, (a_117 = a_115, (b_105 = Quaternion_$ctor_77D16AC0(-b_103.X, -b_103.Y, -b_103.Z, -b_103.W), (((Math.abs(a_117.X - b_105.X) <= tolerance_39) && (Math.abs(a_117.Y - b_105.Y) <= tolerance_39)) && (Math.abs(a_117.Z - b_105.Z) <= tolerance_39)) && (Math.abs(a_117.W - b_105.W) <= tolerance_39))))))))("q and -q equal with zero tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_36);
    }));
})(), (() => {
    const builder$0040_37 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion equalsRotation consistency with rotation result", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_37, Test_TestCaseBuilder__Delay_1505(builder$0040_37, () => {
        let a_119, b_107, tolerance_42, a_120, b_108, tolerance_43, a_121, b_109, a_124, b_112, x_84, y_77, z_75;
        const q1_9 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565_2(1, 1, 1), 0.017453292519943295 * 60);
        const q2_10 = Quaternion_$ctor_77D16AC0(-q1_9.X, -q1_9.Y, -q1_9.Z, -q1_9.W);
        Expect_isTrue((a_119 = q1_9, (b_107 = q2_10, ((tolerance_42 = 1E-10, (a_120 = a_119, (b_108 = b_107, (((Math.abs(a_120.X - b_108.X) <= tolerance_42) && (Math.abs(a_120.Y - b_108.Y) <= tolerance_42)) && (Math.abs(a_120.Z - b_108.Z) <= tolerance_42)) && (Math.abs(a_120.W - b_108.W) <= tolerance_42))))) ? true : ((tolerance_43 = 1E-10, (a_121 = a_119, (b_109 = Quaternion_$ctor_77D16AC0(-b_107.X, -b_107.Y, -b_107.Z, -b_107.W), (((Math.abs(a_121.X - b_109.X) <= tolerance_43) && (Math.abs(a_121.Y - b_109.Y) <= tolerance_43)) && (Math.abs(a_121.Z - b_109.Z) <= tolerance_43)) && (Math.abs(a_121.W - b_109.W) <= tolerance_43))))))))("q and -q are rotationally equal");
        const testPt = Pnt_$ctor_Z7AD9E565(5, 3, 7);
        let result1_2;
        const p_22 = testPt;
        const q_65 = q1_9;
        const x_82 = p_22.X;
        const y_75 = p_22.Y;
        const z_73 = p_22.Z;
        const qx_23 = q_65.X;
        const qy_23 = q_65.Y;
        const qz_23 = q_65.Z;
        const qw_23 = q_65.W;
        const tx_23 = 2 * ((qy_23 * z_73) - (qz_23 * y_75));
        const ty_23 = 2 * ((qz_23 * x_82) - (qx_23 * z_73));
        const tz_23 = 2 * ((qx_23 * y_75) - (qy_23 * x_82));
        result1_2 = Pnt_$ctor_Z7AD9E565_1(((x_82 + (qw_23 * tx_23)) + (qy_23 * tz_23)) - (qz_23 * ty_23), ((y_75 + (qw_23 * ty_23)) + (qz_23 * tx_23)) - (qx_23 * tz_23), ((z_73 + (qw_23 * tz_23)) + (qx_23 * ty_23)) - (qy_23 * tx_23));
        let result2_2;
        const p_23 = testPt;
        const q_66 = q2_10;
        const x_83 = p_23.X;
        const y_76 = p_23.Y;
        const z_74 = p_23.Z;
        const qx_24 = q_66.X;
        const qy_24 = q_66.Y;
        const qz_24 = q_66.Z;
        const qw_24 = q_66.W;
        const tx_24 = 2 * ((qy_24 * z_74) - (qz_24 * y_76));
        const ty_24 = 2 * ((qz_24 * x_83) - (qx_24 * z_74));
        const tz_24 = 2 * ((qx_24 * y_76) - (qy_24 * x_83));
        result2_2 = Pnt_$ctor_Z7AD9E565_1(((x_83 + (qw_24 * tx_24)) + (qy_24 * tz_24)) - (qz_24 * ty_24), ((y_76 + (qw_24 * ty_24)) + (qz_24 * tx_24)) - (qx_24 * tz_24), ((z_74 + (qw_24 * tz_24)) + (qx_24 * ty_24)) - (qy_24 * tx_24));
        const a_122 = result1_2;
        const b_110 = result2_2;
        const same_17 = ((a_124 = a_122, (b_112 = b_110, (x_84 = (a_124.X - b_112.X), (y_77 = (a_124.Y - b_112.Y), (z_75 = (a_124.Z - b_112.Z), Math.sqrt(((x_84 * x_84) + (y_77 * y_77)) + (z_75 * z_75)))))))) < 1E-12;
        if (!same_17) {
            Expect_isTrue(same_17)(`${"rotationally equal quaternions produce same result"} expected: 
${Pnt__get_AsString(b_110)}, got: 
${Pnt__get_AsString(a_122)}`);
        }
        else {
            Expect_isTrue(same_17)("rotationally equal quaternions produce same result");
        }
        Test_TestCaseBuilder__Zero(builder$0040_37);
    }));
})(), (() => {
    const builder$0040_38 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion equalsRotation with 180 degree rotation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_38, Test_TestCaseBuilder__Delay_1505(builder$0040_38, () => {
        let a_126, b_114, tolerance_46, a_127, b_115, tolerance_47, a_128, b_116;
        const q1_10 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 180);
        const q1Neg = Quaternion_$ctor_77D16AC0(-q1_10.X, -q1_10.Y, -q1_10.Z, -q1_10.W);
        Expect_isTrue((a_126 = q1_10, (b_114 = q1Neg, ((tolerance_46 = 1E-10, (a_127 = a_126, (b_115 = b_114, (((Math.abs(a_127.X - b_115.X) <= tolerance_46) && (Math.abs(a_127.Y - b_115.Y) <= tolerance_46)) && (Math.abs(a_127.Z - b_115.Z) <= tolerance_46)) && (Math.abs(a_127.W - b_115.W) <= tolerance_46))))) ? true : ((tolerance_47 = 1E-10, (a_128 = a_126, (b_116 = Quaternion_$ctor_77D16AC0(-b_114.X, -b_114.Y, -b_114.Z, -b_114.W), (((Math.abs(a_128.X - b_116.X) <= tolerance_47) && (Math.abs(a_128.Y - b_116.Y) <= tolerance_47)) && (Math.abs(a_128.Z - b_116.Z) <= tolerance_47)) && (Math.abs(a_128.W - b_116.W) <= tolerance_47))))))))("180° rotation q and -q are equal");
        Test_TestCaseBuilder__Zero(builder$0040_38);
    }));
})(), (() => {
    const builder$0040_39 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion createVecToVec with nearly identical vectors returns identity", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_39, Test_TestCaseBuilder__Delay_1505(builder$0040_39, () => {
        let v_10, v_11, a_131, b_119, d_3, x_93;
        const from_2 = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
        let toPt_2;
        const v_8 = Vec_$ctor_Z7AD9E565_2(1, 1E-13, 0);
        const l_6 = Math.sqrt(((v_8.X * v_8.X) + (v_8.Y * v_8.Y)) + (v_8.Z * v_8.Z));
        if (!(l_6 > 1E-12)) {
            failTooSmall("UnitVec.createFromVec", v_8);
        }
        const li_1 = 1 / l_6;
        toPt_2 = UnitVec_$ctor_Z7AD9E565(li_1 * v_8.X, li_1 * v_8.Y, li_1 * v_8.Z);
        let q_67;
        const vecFrom_2 = from_2;
        const vecTo_2 = toPt_2;
        let v_9;
        const a_129 = vecFrom_2;
        const b_117 = vecTo_2;
        v_9 = Vec_$ctor_Z7AD9E565_1(a_129.X - b_117.X, a_129.Y - b_117.Y, a_129.Z - b_117.Z);
        if (((v_10 = v_9, ((v_10.X * v_10.X) + (v_10.Y * v_10.Y)) + (v_10.Z * v_10.Z))) < 1E-24) {
            q_67 = Quaternion_$ctor_77D16AC0(0, 0, 0, 1);
        }
        else {
            let v_1_3;
            const a_130 = vecFrom_2;
            const b_118 = vecTo_2;
            v_1_3 = Vec_$ctor_Z7AD9E565_1(a_130.X + b_118.X, a_130.Y + b_118.Y, a_130.Z + b_118.Z);
            if (!(((v_11 = v_1_3, ((v_11.X * v_11.X) + (v_11.Y * v_11.Y)) + (v_11.Z * v_11.Z))) > 1E-12)) {
                fail(`Quaternion.createVecToVec failed to find a rotation axis for (almost) colinear unit-vectors in opposite directions: ${interpolate("%O%P()", [vecFrom_2])} and ${interpolate("%O%P()", [vecTo_2])}`);
            }
            q_67 = Quaternion_create_77D16AC0((vecFrom_2.Y * vecTo_2.Z) - (vecFrom_2.Z * vecTo_2.Y), (vecFrom_2.Z * vecTo_2.X) - (vecFrom_2.X * vecTo_2.Z), (vecFrom_2.X * vecTo_2.Y) - (vecFrom_2.Y * vecTo_2.X), ((a_131 = vecFrom_2, (b_119 = vecTo_2, ((a_131.X * b_119.X) + (a_131.Y * b_119.Y)) + (a_131.Z * b_119.Z)))) + 1);
        }
        Expect_floatClose(AccuracyModule_medium, 57.29577951308232 * (2 * ((d_3 = ((x_93 = q_67.W, (x_93 > -1) ? ((x_93 < 1) ? x_93 : 1) : -1)), Math.acos(d_3)))), 0, "nearly identical vectors produce near-identity");
        Test_TestCaseBuilder__Zero(builder$0040_39);
    }));
})(), (() => {
    const builder$0040_40 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion createVecToVec with exactly identical vectors returns identity", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_40, Test_TestCaseBuilder__Delay_1505(builder$0040_40, () => {
        let v_13, v_14, a_136, b_122;
        const from_3 = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
        const toPt_3 = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
        let q_70;
        const vecFrom_3 = from_3;
        const vecTo_3 = toPt_3;
        let v_12;
        const a_134 = vecFrom_3;
        const b_120 = vecTo_3;
        v_12 = Vec_$ctor_Z7AD9E565_1(a_134.X - b_120.X, a_134.Y - b_120.Y, a_134.Z - b_120.Z);
        if (((v_13 = v_12, ((v_13.X * v_13.X) + (v_13.Y * v_13.Y)) + (v_13.Z * v_13.Z))) < 1E-24) {
            q_70 = Quaternion_$ctor_77D16AC0(0, 0, 0, 1);
        }
        else {
            let v_1_4;
            const a_135 = vecFrom_3;
            const b_121 = vecTo_3;
            v_1_4 = Vec_$ctor_Z7AD9E565_1(a_135.X + b_121.X, a_135.Y + b_121.Y, a_135.Z + b_121.Z);
            if (!(((v_14 = v_1_4, ((v_14.X * v_14.X) + (v_14.Y * v_14.Y)) + (v_14.Z * v_14.Z))) > 1E-12)) {
                fail(`Quaternion.createVecToVec failed to find a rotation axis for (almost) colinear unit-vectors in opposite directions: ${interpolate("%O%P()", [vecFrom_3])} and ${interpolate("%O%P()", [vecTo_3])}`);
            }
            q_70 = Quaternion_create_77D16AC0((vecFrom_3.Y * vecTo_3.Z) - (vecFrom_3.Z * vecTo_3.Y), (vecFrom_3.Z * vecTo_3.X) - (vecFrom_3.X * vecTo_3.Z), (vecFrom_3.X * vecTo_3.Y) - (vecFrom_3.Y * vecTo_3.X), ((a_136 = vecFrom_3, (b_122 = vecTo_3, ((a_136.X * b_122.X) + (a_136.Y * b_122.Y)) + (a_136.Z * b_122.Z)))) + 1);
        }
        Expect_floatClose(AccuracyModule_high, q_70.X, 0, "identical vectors X");
        Expect_floatClose(AccuracyModule_high, q_70.Y, 0, "identical vectors Y");
        Expect_floatClose(AccuracyModule_high, q_70.Z, 0, "identical vectors Z");
        Expect_floatClose(AccuracyModule_high, q_70.W, 1, "identical vectors W");
        Test_TestCaseBuilder__Zero(builder$0040_40);
    }));
})(), (() => {
    const builder$0040_41 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion createVecToVec (Vec overload) with zero-length from should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_41, Test_TestCaseBuilder__Delay_1505(builder$0040_41, () => {
        const zeroVec = Vec_$ctor_Z7AD9E565_2(0, 0, 0);
        const target = Vec_$ctor_Z7AD9E565_2(1, 0, 0);
        Expect_throws(() => {
            Quaternion_createVecToVec_5A694120(zeroVec, target);
        }, "zero-length vecFrom should fail");
        Test_TestCaseBuilder__Zero(builder$0040_41);
    }));
})(), (() => {
    const builder$0040_42 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion createVecToVec (Vec overload) with zero-length to should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_42, Test_TestCaseBuilder__Delay_1505(builder$0040_42, () => {
        const source = Vec_$ctor_Z7AD9E565_2(1, 0, 0);
        const zeroVec_1 = Vec_$ctor_Z7AD9E565_2(0, 0, 0);
        Expect_throws(() => {
            Quaternion_createVecToVec_5A694120(source, zeroVec_1);
        }, "zero-length vecTo should fail");
        Test_TestCaseBuilder__Zero(builder$0040_42);
    }));
})(), (() => {
    const builder$0040_43 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion createVecToVec (Vec overload) with very short vectors should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_43, Test_TestCaseBuilder__Delay_1505(builder$0040_43, () => {
        const tinyVec = Vec_$ctor_Z7AD9E565_2(1E-20, 0, 0);
        const target_1 = Vec_$ctor_Z7AD9E565_2(1, 0, 0);
        Expect_throws(() => {
            Quaternion_createVecToVec_5A694120(tinyVec, target_1);
        }, "very short vector should fail");
        Test_TestCaseBuilder__Zero(builder$0040_43);
    }));
})(), (() => {
    const builder$0040_44 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion numerical drift from repeated multiplications", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_44, Test_TestCaseBuilder__Delay_1505(builder$0040_44, () => {
        const q_71 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 1);
        let result_4 = Quaternion_$ctor_77D16AC0(0, 0, 0, 1);
        Test_TestCaseBuilder__Combine_3A59D1F3(builder$0040_44, Test_TestCaseBuilder__For_Z371464DD(builder$0040_44, rangeDouble(1, 1, 360), (_arg) => {
            result_4 = Quaternion_multiply_400F31E0(result_4, q_71);
            Test_TestCaseBuilder__Zero(builder$0040_44);
        }), Test_TestCaseBuilder__Delay_1505(builder$0040_44, () => {
            const magnitude = Math.sqrt((((result_4.X * result_4.X) + (result_4.Y * result_4.Y)) + (result_4.Z * result_4.Z)) + (result_4.W * result_4.W));
            Expect_floatClose(AccuracyModule_medium, magnitude, 1, "quaternion should remain unit length after many multiplications");
            const testPt_1 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
            let rotated;
            const p_24 = testPt_1;
            const q_72 = result_4;
            const x_100 = p_24.X;
            const y_88 = p_24.Y;
            const z_85 = p_24.Z;
            const qx_25 = q_72.X;
            const qy_25 = q_72.Y;
            const qz_25 = q_72.Z;
            const qw_25 = q_72.W;
            const tx_25 = 2 * ((qy_25 * z_85) - (qz_25 * y_88));
            const ty_25 = 2 * ((qz_25 * x_100) - (qx_25 * z_85));
            const tz_25 = 2 * ((qx_25 * y_88) - (qy_25 * x_100));
            rotated = Pnt_$ctor_Z7AD9E565_1(((x_100 + (qw_25 * tx_25)) + (qy_25 * tz_25)) - (qz_25 * ty_25), ((y_88 + (qw_25 * ty_25)) + (qz_25 * tx_25)) - (qx_25 * tz_25), ((z_85 + (qw_25 * tz_25)) + (qx_25 * ty_25)) - (qy_25 * tx_25));
            Expect_floatClose(AccuracyModule_low, rotated.X, testPt_1.X, "full circle X");
            Expect_floatClose(AccuracyModule_low, rotated.Y, testPt_1.Y, "full circle Y");
            Test_TestCaseBuilder__Zero(builder$0040_44);
        }));
    }));
})(), (() => {
    const builder$0040_45 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion toEulerAnglesZYX gimbal lock near 90 degrees", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_45, Test_TestCaseBuilder__Delay_1505(builder$0040_45, () => {
        const q_73 = Quaternion_createFromEulerZYX_Z7AD9E565(30, 89.9, 60);
        const patternInput_2 = Quaternion_toEulerAnglesZYX_Z2A007687(q_73);
        const y_89 = patternInput_2[1];
        const _z = patternInput_2[0];
        const _x = patternInput_2[2];
        Expect_floatClose(AccuracyModule_low, y_89, 89.9, "near gimbal lock Y");
        Test_TestCaseBuilder__Zero(builder$0040_45);
    }));
})(), (() => {
    const builder$0040_46 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion toEulerAnglesZYX at exact 90 degrees", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_46, Test_TestCaseBuilder__Delay_1505(builder$0040_46, () => {
        const q_74 = Quaternion_createFromEulerZYX_Z7AD9E565(30, 90, 60);
        const patternInput_3 = Quaternion_toEulerAnglesZYX_Z2A007687(q_74);
        const y_90 = patternInput_3[1];
        const _z_1 = patternInput_3[0];
        const _x_1 = patternInput_3[2];
        Expect_floatClose(AccuracyModule_medium, y_90, 90, "gimbal lock Y = 90");
        Test_TestCaseBuilder__Zero(builder$0040_46);
    }));
})(), (() => {
    const builder$0040_47 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion equals with different tolerances", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_47, Test_TestCaseBuilder__Delay_1505(builder$0040_47, () => {
        let a_138, b_124, a_140, b_126;
        const q1_11 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 45);
        const q2_11 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 45.001);
        Expect_isTrue((a_138 = q1_11, (b_124 = q2_11, (((Math.abs(a_138.X - b_124.X) <= 0.01) && (Math.abs(a_138.Y - b_124.Y) <= 0.01)) && (Math.abs(a_138.Z - b_124.Z) <= 0.01)) && (Math.abs(a_138.W - b_124.W) <= 0.01))))("similar quaternions within loose tolerance");
        Expect_isFalse((a_140 = q1_11, (b_126 = q2_11, (((Math.abs(a_140.X - b_126.X) <= 1E-10) && (Math.abs(a_140.Y - b_126.Y) <= 1E-10)) && (Math.abs(a_140.Z - b_126.Z) <= 1E-10)) && (Math.abs(a_140.W - b_126.W) <= 1E-10))))("similar quaternions outside tight tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_47);
    }));
})(), (() => {
    const builder$0040_48 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion zero angle rotation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_48, Test_TestCaseBuilder__Delay_1505(builder$0040_48, () => {
        let a_144, b_130, x_102, y_92, z_87;
        const q_75 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 0);
        const a_141 = Pnt_$ctor_Z7AD9E565(5, 3, 7);
        let b_127;
        const p_25 = a_141;
        const q_76 = q_75;
        const x_101 = p_25.X;
        const y_91 = p_25.Y;
        const z_86 = p_25.Z;
        const qx_26 = q_76.X;
        const qy_26 = q_76.Y;
        const qz_26 = q_76.Z;
        const qw_26 = q_76.W;
        const tx_26 = 2 * ((qy_26 * z_86) - (qz_26 * y_91));
        const ty_26 = 2 * ((qz_26 * x_101) - (qx_26 * z_86));
        const tz_26 = 2 * ((qx_26 * y_91) - (qy_26 * x_101));
        b_127 = Pnt_$ctor_Z7AD9E565_1(((x_101 + (qw_26 * tx_26)) + (qy_26 * tz_26)) - (qz_26 * ty_26), ((y_91 + (qw_26 * ty_26)) + (qz_26 * tx_26)) - (qx_26 * tz_26), ((z_86 + (qw_26 * tz_26)) + (qx_26 * ty_26)) - (qy_26 * tx_26));
        const a_142 = a_141;
        const b_128 = b_127;
        const same_18 = ((a_144 = a_142, (b_130 = b_128, (x_102 = (a_144.X - b_130.X), (y_92 = (a_144.Y - b_130.Y), (z_87 = (a_144.Z - b_130.Z), Math.sqrt(((x_102 * x_102) + (y_92 * y_92)) + (z_87 * z_87)))))))) < 1E-12;
        if (!same_18) {
            Expect_isTrue(same_18)(`${"zero angle rotation"} expected: 
${Pnt__get_AsString(b_128)}, got: 
${Pnt__get_AsString(a_142)}`);
        }
        else {
            Expect_isTrue(same_18)("zero angle rotation");
        }
        Test_TestCaseBuilder__Zero(builder$0040_48);
    }));
})(), (() => {
    const builder$0040_49 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion 360 degree rotation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_49, Test_TestCaseBuilder__Delay_1505(builder$0040_49, () => {
        let a_148, b_134, x_104, y_94, z_89;
        const q_77 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 360);
        const a_145 = Pnt_$ctor_Z7AD9E565(5, 3, 7);
        let b_131;
        const p_26 = a_145;
        const q_78 = q_77;
        const x_103 = p_26.X;
        const y_93 = p_26.Y;
        const z_88 = p_26.Z;
        const qx_27 = q_78.X;
        const qy_27 = q_78.Y;
        const qz_27 = q_78.Z;
        const qw_27 = q_78.W;
        const tx_27 = 2 * ((qy_27 * z_88) - (qz_27 * y_93));
        const ty_27 = 2 * ((qz_27 * x_103) - (qx_27 * z_88));
        const tz_27 = 2 * ((qx_27 * y_93) - (qy_27 * x_103));
        b_131 = Pnt_$ctor_Z7AD9E565_1(((x_103 + (qw_27 * tx_27)) + (qy_27 * tz_27)) - (qz_27 * ty_27), ((y_93 + (qw_27 * ty_27)) + (qz_27 * tx_27)) - (qx_27 * tz_27), ((z_88 + (qw_27 * tz_27)) + (qx_27 * ty_27)) - (qy_27 * tx_27));
        const a_146 = a_145;
        const b_132 = b_131;
        const same_19 = ((a_148 = a_146, (b_134 = b_132, (x_104 = (a_148.X - b_134.X), (y_94 = (a_148.Y - b_134.Y), (z_89 = (a_148.Z - b_134.Z), Math.sqrt(((x_104 * x_104) + (y_94 * y_94)) + (z_89 * z_89)))))))) < 1E-12;
        if (!same_19) {
            Expect_isTrue(same_19)(`${"360° rotation returns to original"} expected: 
${Pnt__get_AsString(b_132)}, got: 
${Pnt__get_AsString(a_146)}`);
        }
        else {
            Expect_isTrue(same_19)("360° rotation returns to original");
        }
        Test_TestCaseBuilder__Zero(builder$0040_49);
    }));
})(), (() => {
    const builder$0040_50 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion very large angle", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_50, Test_TestCaseBuilder__Delay_1505(builder$0040_50, () => {
        let a_152, b_138, x_106, y_96, z_91;
        const q_79 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 720);
        const a_149 = Pnt_$ctor_Z7AD9E565(5, 3, 7);
        let b_135;
        const p_27 = a_149;
        const q_80 = q_79;
        const x_105 = p_27.X;
        const y_95 = p_27.Y;
        const z_90 = p_27.Z;
        const qx_28 = q_80.X;
        const qy_28 = q_80.Y;
        const qz_28 = q_80.Z;
        const qw_28 = q_80.W;
        const tx_28 = 2 * ((qy_28 * z_90) - (qz_28 * y_95));
        const ty_28 = 2 * ((qz_28 * x_105) - (qx_28 * z_90));
        const tz_28 = 2 * ((qx_28 * y_95) - (qy_28 * x_105));
        b_135 = Pnt_$ctor_Z7AD9E565_1(((x_105 + (qw_28 * tx_28)) + (qy_28 * tz_28)) - (qz_28 * ty_28), ((y_95 + (qw_28 * ty_28)) + (qz_28 * tx_28)) - (qx_28 * tz_28), ((z_90 + (qw_28 * tz_28)) + (qx_28 * ty_28)) - (qy_28 * tx_28));
        const a_150 = a_149;
        const b_136 = b_135;
        const same_20 = ((a_152 = a_150, (b_138 = b_136, (x_106 = (a_152.X - b_138.X), (y_96 = (a_152.Y - b_138.Y), (z_91 = (a_152.Z - b_138.Z), Math.sqrt(((x_106 * x_106) + (y_96 * y_96)) + (z_91 * z_91)))))))) < 1E-12;
        if (!same_20) {
            Expect_isTrue(same_20)(`${"720° rotation returns to original"} expected: 
${Pnt__get_AsString(b_136)}, got: 
${Pnt__get_AsString(a_150)}`);
        }
        else {
            Expect_isTrue(same_20)("720° rotation returns to original");
        }
        Test_TestCaseBuilder__Zero(builder$0040_50);
    }));
})(), (() => {
    const builder$0040_51 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion negative angle", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_51, Test_TestCaseBuilder__Delay_1505(builder$0040_51, () => {
        let a_156, b_141, x_109, y_99, z_94;
        const q1_12 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * -90);
        const q2_12 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 270);
        const a_153 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        let result1_3;
        const p_28 = a_153;
        const q_81 = q1_12;
        const x_107 = p_28.X;
        const y_97 = p_28.Y;
        const z_92 = p_28.Z;
        const qx_29 = q_81.X;
        const qy_29 = q_81.Y;
        const qz_29 = q_81.Z;
        const qw_29 = q_81.W;
        const tx_29 = 2 * ((qy_29 * z_92) - (qz_29 * y_97));
        const ty_29 = 2 * ((qz_29 * x_107) - (qx_29 * z_92));
        const tz_29 = 2 * ((qx_29 * y_97) - (qy_29 * x_107));
        result1_3 = Pnt_$ctor_Z7AD9E565_1(((x_107 + (qw_29 * tx_29)) + (qy_29 * tz_29)) - (qz_29 * ty_29), ((y_97 + (qw_29 * ty_29)) + (qz_29 * tx_29)) - (qx_29 * tz_29), ((z_92 + (qw_29 * tz_29)) + (qx_29 * ty_29)) - (qy_29 * tx_29));
        let result2_3;
        const p_29 = a_153;
        const q_82 = q2_12;
        const x_108 = p_29.X;
        const y_98 = p_29.Y;
        const z_93 = p_29.Z;
        const qx_30 = q_82.X;
        const qy_30 = q_82.Y;
        const qz_30 = q_82.Z;
        const qw_30 = q_82.W;
        const tx_30 = 2 * ((qy_30 * z_93) - (qz_30 * y_98));
        const ty_30 = 2 * ((qz_30 * x_108) - (qx_30 * z_93));
        const tz_30 = 2 * ((qx_30 * y_98) - (qy_30 * x_108));
        result2_3 = Pnt_$ctor_Z7AD9E565_1(((x_108 + (qw_30 * tx_30)) + (qy_30 * tz_30)) - (qz_30 * ty_30), ((y_98 + (qw_30 * ty_30)) + (qz_30 * tx_30)) - (qx_30 * tz_30), ((z_93 + (qw_30 * tz_30)) + (qx_30 * ty_30)) - (qy_30 * tx_30));
        const a_154 = result1_3;
        const b_139 = result2_3;
        const same_21 = ((a_156 = a_154, (b_141 = b_139, (x_109 = (a_156.X - b_141.X), (y_99 = (a_156.Y - b_141.Y), (z_94 = (a_156.Z - b_141.Z), Math.sqrt(((x_109 * x_109) + (y_99 * y_99)) + (z_94 * z_94)))))))) < 1E-12;
        if (!same_21) {
            Expect_isTrue(same_21)(`${"-90° equals 270°"} expected: 
${Pnt__get_AsString(b_139)}, got: 
${Pnt__get_AsString(a_154)}`);
        }
        else {
            Expect_isTrue(same_21)("-90° equals 270°");
        }
        Test_TestCaseBuilder__Zero(builder$0040_51);
    }));
})(), (() => {
    const builder$0040_52 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion Normalize restores unit length", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_52, Test_TestCaseBuilder__Delay_1505(builder$0040_52, () => {
        const q1_13 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 45);
        const scale = 1.001;
        const qBad = Quaternion_create_77D16AC0_1(q1_13.X * scale, q1_13.Y * scale, q1_13.Z * scale, q1_13.W * scale);
        let qNormalized;
        const q_83 = qBad;
        qNormalized = Quaternion_create_77D16AC0(q_83.X, q_83.Y, q_83.Z, q_83.W);
        const magnitude_1 = Math.sqrt((((qNormalized.X * qNormalized.X) + (qNormalized.Y * qNormalized.Y)) + (qNormalized.Z * qNormalized.Z)) + (qNormalized.W * qNormalized.W));
        Expect_floatClose(AccuracyModule_high, magnitude_1, 1, "normalized quaternion has unit length");
        Test_TestCaseBuilder__Zero(builder$0040_52);
    }));
})(), (() => {
    const builder$0040_53 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion Normalize preserves rotation direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_53, Test_TestCaseBuilder__Delay_1505(builder$0040_53, () => {
        let a_160, b_144, x_112, y_102, z_97;
        const q_84 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 60);
        let qNormalized_1;
        const q_85 = q_84;
        qNormalized_1 = Quaternion_create_77D16AC0(q_85.X, q_85.Y, q_85.Z, q_85.W);
        const a_157 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        let result1_4;
        const p_30 = a_157;
        const q_86 = q_84;
        const x_110 = p_30.X;
        const y_100 = p_30.Y;
        const z_95 = p_30.Z;
        const qx_31 = q_86.X;
        const qy_31 = q_86.Y;
        const qz_31 = q_86.Z;
        const qw_31 = q_86.W;
        const tx_31 = 2 * ((qy_31 * z_95) - (qz_31 * y_100));
        const ty_31 = 2 * ((qz_31 * x_110) - (qx_31 * z_95));
        const tz_31 = 2 * ((qx_31 * y_100) - (qy_31 * x_110));
        result1_4 = Pnt_$ctor_Z7AD9E565_1(((x_110 + (qw_31 * tx_31)) + (qy_31 * tz_31)) - (qz_31 * ty_31), ((y_100 + (qw_31 * ty_31)) + (qz_31 * tx_31)) - (qx_31 * tz_31), ((z_95 + (qw_31 * tz_31)) + (qx_31 * ty_31)) - (qy_31 * tx_31));
        let result2_4;
        const p_31 = a_157;
        const q_87 = qNormalized_1;
        const x_111 = p_31.X;
        const y_101 = p_31.Y;
        const z_96 = p_31.Z;
        const qx_32 = q_87.X;
        const qy_32 = q_87.Y;
        const qz_32 = q_87.Z;
        const qw_32 = q_87.W;
        const tx_32 = 2 * ((qy_32 * z_96) - (qz_32 * y_101));
        const ty_32 = 2 * ((qz_32 * x_111) - (qx_32 * z_96));
        const tz_32 = 2 * ((qx_32 * y_101) - (qy_32 * x_111));
        result2_4 = Pnt_$ctor_Z7AD9E565_1(((x_111 + (qw_32 * tx_32)) + (qy_32 * tz_32)) - (qz_32 * ty_32), ((y_101 + (qw_32 * ty_32)) + (qz_32 * tx_32)) - (qx_32 * tz_32), ((z_96 + (qw_32 * tz_32)) + (qx_32 * ty_32)) - (qy_32 * tx_32));
        const a_158 = result1_4;
        const b_142 = result2_4;
        const same_22 = ((a_160 = a_158, (b_144 = b_142, (x_112 = (a_160.X - b_144.X), (y_102 = (a_160.Y - b_144.Y), (z_97 = (a_160.Z - b_144.Z), Math.sqrt(((x_112 * x_112) + (y_102 * y_102)) + (z_97 * z_97)))))))) < 1E-12;
        if (!same_22) {
            Expect_isTrue(same_22)(`${"Normalize preserves rotation"} expected: 
${Pnt__get_AsString(b_142)}, got: 
${Pnt__get_AsString(a_158)}`);
        }
        else {
            Expect_isTrue(same_22)("Normalize preserves rotation");
        }
        Test_TestCaseBuilder__Zero(builder$0040_53);
    }));
})(), (() => {
    const builder$0040_54 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion Normalize after many multiplications", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_54, Test_TestCaseBuilder__Delay_1505(builder$0040_54, () => {
        const q_88 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 1);
        let result_5 = Quaternion_$ctor_77D16AC0(0, 0, 0, 1);
        Test_TestCaseBuilder__Combine_3A59D1F3(builder$0040_54, Test_TestCaseBuilder__For_Z371464DD(builder$0040_54, rangeDouble(1, 1, 1000), (_arg_1) => {
            result_5 = Quaternion_multiply_400F31E0(result_5, q_88);
            Test_TestCaseBuilder__Zero(builder$0040_54);
        }), Test_TestCaseBuilder__Delay_1505(builder$0040_54, () => {
            const magnitudeBefore = Math.sqrt((((result_5.X * result_5.X) + (result_5.Y * result_5.Y)) + (result_5.Z * result_5.Z)) + (result_5.W * result_5.W));
            let normalized;
            const q_89 = result_5;
            normalized = Quaternion_create_77D16AC0(q_89.X, q_89.Y, q_89.Z, q_89.W);
            const magnitudeAfter = Math.sqrt((((normalized.X * normalized.X) + (normalized.Y * normalized.Y)) + (normalized.Z * normalized.Z)) + (normalized.W * normalized.W));
            Expect_floatClose(AccuracyModule_high, magnitudeAfter, 1, "normalized magnitude is exactly 1.0");
            Expect_isTrue(Math.abs(magnitudeAfter - 1) <= Math.abs(magnitudeBefore - 1))("normalization improves unit length");
            Test_TestCaseBuilder__Zero(builder$0040_54);
        }));
    }));
})(), (() => {
    const builder$0040_55 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion Normalize on already normalized quaternion", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_55, Test_TestCaseBuilder__Delay_1505(builder$0040_55, () => {
        const q_90 = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565(0, 0, 1), 0.017453292519943295 * 45);
        let qNormalized_2;
        const q_91 = q_90;
        qNormalized_2 = Quaternion_create_77D16AC0(q_91.X, q_91.Y, q_91.Z, q_91.W);
        Expect_floatClose(AccuracyModule_high, qNormalized_2.X, q_90.X, "normalized X matches original");
        Expect_floatClose(AccuracyModule_high, qNormalized_2.Y, q_90.Y, "normalized Y matches original");
        Expect_floatClose(AccuracyModule_high, qNormalized_2.Z, q_90.Z, "normalized Z matches original");
        Expect_floatClose(AccuracyModule_high, qNormalized_2.W, q_90.W, "normalized W matches original");
        Test_TestCaseBuilder__Zero(builder$0040_55);
    }));
})(), (() => {
    const builder$0040_56 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Quaternion Normalize with zero values should fail", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_56, Test_TestCaseBuilder__Delay_1505(builder$0040_56, () => {
        Expect_throws(() => {
            Quaternion_create_77D16AC0_1(0, 0, 0, 0);
        }, "create zero quaternion should fail");
        Expect_throws(() => {
            Quaternion_create_77D16AC0_1(1E-20, 1E-20, 1E-20, 1E-20);
        }, "create tiny quaternion should fail");
        Test_TestCaseBuilder__Zero(builder$0040_56);
    }));
})()]));

