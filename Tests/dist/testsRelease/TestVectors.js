
import { Expect_throws, Expect_isTrue, Test_TestCaseBuilder__Zero, Expect_floatClose, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList, AccuracyModule_medium, AccuracyModule_veryHigh } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Pt_$ctor_7B00E9A0 } from "./Src/Pt.js";
import { Vc_$ctor_7B00E9A0 } from "./Src/Vc.js";
import { Pt_$ctor_7B00E9A0 as Pt_$ctor_7B00E9A0_1 } from "./Src/Pt.js";
import { Vc_$ctor_7B00E9A0 as Vc_$ctor_7B00E9A0_1 } from "./Src/Vc.js";
import { ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { failUnit3, failUnit2, failDivide } from "./Src/EuclidErrors.js";
import { UnitVc_$ctor_7B00E9A0 } from "./Src/UnitVc.js";
import { Pnt_$ctor_Z7AD9E565 } from "./Src/Pnt.js";
import { Vec_$ctor_Z7AD9E565 } from "./Src/Vec.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_1 } from "./Src/Pnt.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_1 } from "./Src/Vec.js";
import { UnitVec_$ctor_Z7AD9E565 } from "./Src/UnitVec.js";

export const tol = AccuracyModule_veryHigh;

export const tolMed = AccuracyModule_medium;

export const tests = Test_testList("Basic Vector and Point Types", ofArray([Test_testList("Pt (2D Point)", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create 2D point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const pt = Pt_$ctor_7B00E9A0(1, 2);
        Expect_floatClose(tol, pt.X, 1, "X is 1.0");
        Expect_floatClose(tol, pt.Y, 2, "Y is 2.0");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("distance between points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let a_1, b_1, vx, vy;
        Expect_floatClose(tol, (a_1 = Pt_$ctor_7B00E9A0(0, 0), (b_1 = Pt_$ctor_7B00E9A0(3, 4), (vx = (a_1.X - b_1.X), (vy = (a_1.Y - b_1.Y), Math.sqrt((vx * vx) + (vy * vy)))))), 5, "distance is 5.0");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("distanceSq between points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let a_3, b_3, vx_1, vy_1;
        Expect_floatClose(tol, (a_3 = Pt_$ctor_7B00E9A0(0, 0), (b_3 = Pt_$ctor_7B00E9A0(3, 4), (vx_1 = (a_3.X - b_3.X), (vy_1 = (a_3.Y - b_3.Y), (vx_1 * vx_1) + (vy_1 * vy_1))))), 25, "distanceSq is 25.0");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("point addition with vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let result;
        const p = Pt_$ctor_7B00E9A0(1, 2);
        const v = Vc_$ctor_7B00E9A0(3, 4);
        result = Pt_$ctor_7B00E9A0_1(p.X + v.X, p.Y + v.Y);
        Expect_floatClose(tol, result.X, 4, "result X is 4.0");
        Expect_floatClose(tol, result.Y, 6, "result Y is 6.0");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("point subtraction yields vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let vc_1;
        const a_4 = Pt_$ctor_7B00E9A0(5, 7);
        const b_4 = Pt_$ctor_7B00E9A0(2, 3);
        vc_1 = Vc_$ctor_7B00E9A0_1(a_4.X - b_4.X, a_4.Y - b_4.Y);
        Expect_floatClose(tol, vc_1.X, 3, "vector X is 3.0");
        Expect_floatClose(tol, vc_1.Y, 4, "vector Y is 4.0");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})()])), Test_testList("Vc (2D Vector)", ofArray([(() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create 2D vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const vc_2 = Vc_$ctor_7B00E9A0(3, 4);
        Expect_floatClose(tol, vc_2.X, 3, "X is 3.0");
        Expect_floatClose(tol, vc_2.Y, 4, "Y is 4.0");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("vector length", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        let v_1, x, y;
        Expect_floatClose(tol, (v_1 = Vc_$ctor_7B00E9A0(3, 4), (x = v_1.X, (y = v_1.Y, Math.sqrt((x * x) + (y * y))))), 5, "length is 5.0");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("vector lengthSq", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let v_2, x_1, y_1;
        Expect_floatClose(tol, (v_2 = Vc_$ctor_7B00E9A0(3, 4), (x_1 = v_2.X, (y_1 = v_2.Y, (x_1 * x_1) + (y_1 * y_1)))), 25, "lengthSq is 25.0");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("vector addition", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let result_1;
        const a_5 = Vc_$ctor_7B00E9A0(1, 2);
        const b_5 = Vc_$ctor_7B00E9A0(3, 4);
        result_1 = Vc_$ctor_7B00E9A0_1(a_5.X + b_5.X, a_5.Y + b_5.Y);
        Expect_floatClose(tol, result_1.X, 4, "result X is 4.0");
        Expect_floatClose(tol, result_1.Y, 6, "result Y is 6.0");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("vector subtraction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        let result_2;
        const a_6 = Vc_$ctor_7B00E9A0(5, 7);
        const b_6 = Vc_$ctor_7B00E9A0(2, 3);
        result_2 = Vc_$ctor_7B00E9A0_1(a_6.X - b_6.X, a_6.Y - b_6.Y);
        Expect_floatClose(tol, result_2.X, 3, "result X is 3.0");
        Expect_floatClose(tol, result_2.Y, 4, "result Y is 4.0");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("vector scalar multiplication", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let result_3;
        const v_3 = Vc_$ctor_7B00E9A0(2, 3);
        result_3 = Vc_$ctor_7B00E9A0_1(v_3.X * 2, v_3.Y * 2);
        Expect_floatClose(tol, result_3.X, 4, "result X is 4.0");
        Expect_floatClose(tol, result_3.Y, 6, "result Y is 6.0");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("vector scalar division", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        let result_4;
        const v_4 = Vc_$ctor_7B00E9A0(6, 8);
        if (!(Math.abs(2) > 1E-12)) {
            failDivide("\'/\' operator", 2, v_4);
        }
        result_4 = Vc_$ctor_7B00E9A0_1(v_4.X / 2, v_4.Y / 2);
        Expect_floatClose(tol, result_4.X, 3, "result X is 3.0");
        Expect_floatClose(tol, result_4.Y, 4, "result Y is 4.0");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})()])), Test_testList("UnitVc (2D Unit Vector)", ofArray([(() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create unit vector from components", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        let uvc;
        const l = Math.sqrt((3 * 3) + (4 * 4));
        if (!(l > 1E-12)) {
            failUnit2("UnitVc.create", 3, 4);
        }
        uvc = UnitVc_$ctor_7B00E9A0(3 / l, 4 / l);
        Expect_isTrue(Math.abs(((uvc.X * uvc.X) + (uvc.Y * uvc.Y)) - 1) < 1E-09)("X and Y are normalized");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("unit vector X and Y components", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        let uvc_1;
        const l_1 = Math.sqrt((3 * 3) + (4 * 4));
        if (!(l_1 > 1E-12)) {
            failUnit2("UnitVc.create", 3, 4);
        }
        uvc_1 = UnitVc_$ctor_7B00E9A0(3 / l_1, 4 / l_1);
        Expect_floatClose(tolMed, uvc_1.X, 0.6, "X is 0.6");
        Expect_floatClose(tolMed, uvc_1.Y, 0.8, "Y is 0.8");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create unit vector from zero throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        Expect_throws(() => {
            let l_2;
            (l_2 = Math.sqrt((0 * 0) + (0 * 0)), (!(l_2 > 1E-12) ? failUnit2("UnitVc.create", 0, 0) : undefined, UnitVc_$ctor_7B00E9A0(0 / l_2, 0 / l_2)));
        }, "throws on zero vector");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create unit vector from very small throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        Expect_throws(() => {
            let l_3;
            (l_3 = Math.sqrt((1E-15 * 1E-15) + (0 * 0)), (!(l_3 > 1E-12) ? failUnit2("UnitVc.create", 1E-15, 0) : undefined, UnitVc_$ctor_7B00E9A0(1E-15 / l_3, 0 / l_3)));
        }, "throws on very small vector");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("unit vector from normalized Vc", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        const vc_7 = Vc_$ctor_7B00E9A0(3, 4);
        let uvc_2;
        const x_15 = vc_7.X;
        const y_10 = vc_7.Y;
        const l_4 = Math.sqrt((x_15 * x_15) + (y_10 * y_10));
        if (!(l_4 > 1E-12)) {
            failUnit2("UnitVc.create", x_15, y_10);
        }
        uvc_2 = UnitVc_$ctor_7B00E9A0(x_15 / l_4, y_10 / l_4);
        let direct;
        const v_6 = vc_7;
        let f_4;
        const v_5 = vc_7;
        const x_18 = v_5.X;
        const y_12 = v_5.Y;
        f_4 = Math.sqrt((x_18 * x_18) + (y_12 * y_12));
        if (!(Math.abs(f_4) > 1E-12)) {
            failDivide("\'/\' operator", f_4, v_6);
        }
        direct = Vc_$ctor_7B00E9A0_1(v_6.X / f_4, v_6.Y / f_4);
        Expect_floatClose(tol, uvc_2.X, direct.X, "X matches direct normalization");
        Expect_floatClose(tol, uvc_2.Y, direct.Y, "Y matches direct normalization");
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})()])), Test_testList("Pnt (3D Point)", ofArray([(() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create 3D point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        const pnt = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        Expect_floatClose(tol, pnt.X, 1, "X is 1.0");
        Expect_floatClose(tol, pnt.Y, 2, "Y is 2.0");
        Expect_floatClose(tol, pnt.Z, 3, "Z is 3.0");
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("distance between 3D points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        let a_8, b_8, x_20, y_13, z;
        Expect_floatClose(tol, (a_8 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_8 = Pnt_$ctor_Z7AD9E565(1, 2, 2), (x_20 = (a_8.X - b_8.X), (y_13 = (a_8.Y - b_8.Y), (z = (a_8.Z - b_8.Z), Math.sqrt(((x_20 * x_20) + (y_13 * y_13)) + (z * z))))))), 3, "distance is 3.0");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("distanceSq between 3D points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        let a_10, b_10, x_21, y_14, z_1;
        Expect_floatClose(tol, (a_10 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_10 = Pnt_$ctor_Z7AD9E565(1, 2, 2), (x_21 = (a_10.X - b_10.X), (y_14 = (a_10.Y - b_10.Y), (z_1 = (a_10.Z - b_10.Z), ((x_21 * x_21) + (y_14 * y_14)) + (z_1 * z_1)))))), 9, "distanceSq is 9.0");
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("point addition with 3D vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        let result_5;
        const p_1 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        const v_7 = Vec_$ctor_Z7AD9E565(4, 5, 6);
        result_5 = Pnt_$ctor_Z7AD9E565_1(p_1.X + v_7.X, p_1.Y + v_7.Y, p_1.Z + v_7.Z);
        Expect_floatClose(tol, result_5.X, 5, "result X is 5.0");
        Expect_floatClose(tol, result_5.Y, 7, "result Y is 7.0");
        Expect_floatClose(tol, result_5.Z, 9, "result Z is 9.0");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("point subtraction yields 3D vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        let vec_1;
        const a_11 = Pnt_$ctor_Z7AD9E565(5, 7, 9);
        const b_11 = Pnt_$ctor_Z7AD9E565(2, 3, 4);
        vec_1 = Vec_$ctor_Z7AD9E565_1(a_11.X - b_11.X, a_11.Y - b_11.Y, a_11.Z - b_11.Z);
        Expect_floatClose(tol, vec_1.X, 3, "vector X is 3.0");
        Expect_floatClose(tol, vec_1.Y, 4, "vector Y is 4.0");
        Expect_floatClose(tol, vec_1.Z, 5, "vector Z is 5.0");
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})()])), Test_testList("Vec (3D Vector)", ofArray([(() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create 3D vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        const vec_2 = Vec_$ctor_Z7AD9E565(3, 4, 0);
        Expect_floatClose(tol, vec_2.X, 3, "X is 3.0");
        Expect_floatClose(tol, vec_2.Y, 4, "Y is 4.0");
        Expect_floatClose(tol, vec_2.Z, 0, "Z is 0.0");
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("3D vector length", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        let v_8;
        Expect_floatClose(tol, (v_8 = Vec_$ctor_Z7AD9E565(2, 3, 6), Math.sqrt(((v_8.X * v_8.X) + (v_8.Y * v_8.Y)) + (v_8.Z * v_8.Z))), 7, "length is 7.0");
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("3D vector lengthSq", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        let v_9;
        Expect_floatClose(tol, (v_9 = Vec_$ctor_Z7AD9E565(2, 3, 6), ((v_9.X * v_9.X) + (v_9.Y * v_9.Y)) + (v_9.Z * v_9.Z)), 49, "lengthSq is 49.0");
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("3D vector addition", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        let result_6;
        const a_12 = Vec_$ctor_Z7AD9E565(1, 2, 3);
        const b_12 = Vec_$ctor_Z7AD9E565(4, 5, 6);
        result_6 = Vec_$ctor_Z7AD9E565_1(a_12.X + b_12.X, a_12.Y + b_12.Y, a_12.Z + b_12.Z);
        Expect_floatClose(tol, result_6.X, 5, "result X is 5.0");
        Expect_floatClose(tol, result_6.Y, 7, "result Y is 7.0");
        Expect_floatClose(tol, result_6.Z, 9, "result Z is 9.0");
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("3D vector subtraction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        let result_7;
        const a_13 = Vec_$ctor_Z7AD9E565(5, 7, 9);
        const b_13 = Vec_$ctor_Z7AD9E565(2, 3, 4);
        result_7 = Vec_$ctor_Z7AD9E565_1(a_13.X - b_13.X, a_13.Y - b_13.Y, a_13.Z - b_13.Z);
        Expect_floatClose(tol, result_7.X, 3, "result X is 3.0");
        Expect_floatClose(tol, result_7.Y, 4, "result Y is 4.0");
        Expect_floatClose(tol, result_7.Z, 5, "result Z is 5.0");
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("3D vector scalar multiplication", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        let result_8;
        const a_14 = Vec_$ctor_Z7AD9E565(2, 3, 4);
        result_8 = Vec_$ctor_Z7AD9E565_1(a_14.X * 2, a_14.Y * 2, a_14.Z * 2);
        Expect_floatClose(tol, result_8.X, 4, "result X is 4.0");
        Expect_floatClose(tol, result_8.Y, 6, "result Y is 6.0");
        Expect_floatClose(tol, result_8.Z, 8, "result Z is 8.0");
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("3D vector scalar division", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        let result_9;
        const v_10 = Vec_$ctor_Z7AD9E565(6, 8, 10);
        if (!(Math.abs(2) > 1E-12)) {
            failDivide("\'/\' operator", 2, v_10);
        }
        result_9 = Vec_$ctor_Z7AD9E565_1(v_10.X / 2, v_10.Y / 2, v_10.Z / 2);
        Expect_floatClose(tol, result_9.X, 3, "result X is 3.0");
        Expect_floatClose(tol, result_9.Y, 4, "result Y is 4.0");
        Expect_floatClose(tol, result_9.Z, 5, "result Z is 5.0");
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("3D vector cross product", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        let cross;
        const a_15 = Vec_$ctor_Z7AD9E565(1, 0, 0);
        const b_14 = Vec_$ctor_Z7AD9E565(0, 1, 0);
        cross = Vec_$ctor_Z7AD9E565_1((a_15.Y * b_14.Z) - (a_15.Z * b_14.Y), (a_15.Z * b_14.X) - (a_15.X * b_14.Z), (a_15.X * b_14.Y) - (a_15.Y * b_14.X));
        Expect_floatClose(tol, cross.X, 0, "cross X is 0.0");
        Expect_floatClose(tol, cross.Y, 0, "cross Y is 0.0");
        Expect_floatClose(tol, cross.Z, 1, "cross Z is 1.0");
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("3D vector dot product", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        let a_16, b_15;
        Expect_floatClose(tol, (a_16 = Vec_$ctor_Z7AD9E565(1, 2, 3), (b_15 = Vec_$ctor_Z7AD9E565(4, 5, 6), ((a_16.X * b_15.X) + (a_16.Y * b_15.Y)) + (a_16.Z * b_15.Z))), 32, "dot is 32.0");
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})()])), Test_testList("UnitVec (3D Unit Vector)", ofArray([(() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create 3D unit vector from components", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        let uvec;
        const l_5 = Math.sqrt(((3 * 3) + (4 * 4)) + (0 * 0));
        if (!(l_5 > 1E-12)) {
            failUnit3("UnitVec.create", 3, 4, 0);
        }
        const li = 1 / l_5;
        uvec = UnitVec_$ctor_Z7AD9E565(li * 3, li * 4, li * 0);
        Expect_isTrue(Math.abs((((uvec.X * uvec.X) + (uvec.Y * uvec.Y)) + (uvec.Z * uvec.Z)) - 1) < 1E-09)("X Y Z are normalized");
        Test_TestCaseBuilder__Zero(builder$0040_31);
    }));
})(), (() => {
    const builder$0040_32 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("3D unit vector X Y Z components", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_32, Test_TestCaseBuilder__Delay_1505(builder$0040_32, () => {
        let uvec_1;
        const l_6 = Math.sqrt(((2 * 2) + (3 * 3)) + (6 * 6));
        if (!(l_6 > 1E-12)) {
            failUnit3("UnitVec.create", 2, 3, 6);
        }
        const li_1 = 1 / l_6;
        uvec_1 = UnitVec_$ctor_Z7AD9E565(li_1 * 2, li_1 * 3, li_1 * 6);
        const len_2 = Math.sqrt((4 + 9) + 36);
        Expect_floatClose(tolMed, uvec_1.X, 2 / len_2, "X is 2/7");
        Expect_floatClose(tolMed, uvec_1.Y, 3 / len_2, "Y is 3/7");
        Expect_floatClose(tolMed, uvec_1.Z, 6 / len_2, "Z is 6/7");
        Test_TestCaseBuilder__Zero(builder$0040_32);
    }));
})(), (() => {
    const builder$0040_33 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create 3D unit vector from zero throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_33, Test_TestCaseBuilder__Delay_1505(builder$0040_33, () => {
        Expect_throws(() => {
            let l_7, li_2;
            (l_7 = Math.sqrt(((0 * 0) + (0 * 0)) + (0 * 0)), (!(l_7 > 1E-12) ? failUnit3("UnitVec.create", 0, 0, 0) : undefined, (li_2 = (1 / l_7), UnitVec_$ctor_Z7AD9E565(li_2 * 0, li_2 * 0, li_2 * 0))));
        }, "throws on zero vector");
        Test_TestCaseBuilder__Zero(builder$0040_33);
    }));
})(), (() => {
    const builder$0040_34 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create 3D unit vector from very small throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_34, Test_TestCaseBuilder__Delay_1505(builder$0040_34, () => {
        Expect_throws(() => {
            let l_8, li_3;
            (l_8 = Math.sqrt(((1E-15 * 1E-15) + (0 * 0)) + (0 * 0)), (!(l_8 > 1E-12) ? failUnit3("UnitVec.create", 1E-15, 0, 0) : undefined, (li_3 = (1 / l_8), UnitVec_$ctor_Z7AD9E565(li_3 * 1E-15, li_3 * 0, li_3 * 0))));
        }, "throws on very small vector");
        Test_TestCaseBuilder__Zero(builder$0040_34);
    }));
})(), (() => {
    const builder$0040_35 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("3D unit vector from normalized Vec", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_35, Test_TestCaseBuilder__Delay_1505(builder$0040_35, () => {
        const vec_7 = Vec_$ctor_Z7AD9E565(3, 4, 0);
        let uvec_2;
        const x_35 = vec_7.X;
        const y_23 = vec_7.Y;
        const z_10 = vec_7.Z;
        const l_9 = Math.sqrt(((x_35 * x_35) + (y_23 * y_23)) + (z_10 * z_10));
        if (!(l_9 > 1E-12)) {
            failUnit3("UnitVec.create", x_35, y_23, z_10);
        }
        const li_4 = 1 / l_9;
        uvec_2 = UnitVec_$ctor_Z7AD9E565(li_4 * x_35, li_4 * y_23, li_4 * z_10);
        let direct_1;
        const v_12 = vec_7;
        let f_9;
        const v_11 = vec_7;
        f_9 = Math.sqrt(((v_11.X * v_11.X) + (v_11.Y * v_11.Y)) + (v_11.Z * v_11.Z));
        if (!(Math.abs(f_9) > 1E-12)) {
            failDivide("\'/\' operator", f_9, v_12);
        }
        direct_1 = Vec_$ctor_Z7AD9E565_1(v_12.X / f_9, v_12.Y / f_9, v_12.Z / f_9);
        Expect_floatClose(tol, uvec_2.X, direct_1.X, "X matches direct normalization");
        Expect_floatClose(tol, uvec_2.Y, direct_1.Y, "Y matches direct normalization");
        Expect_floatClose(tol, uvec_2.Z, direct_1.Z, "Z matches direct normalization");
        Test_TestCaseBuilder__Zero(builder$0040_35);
    }));
})(), (() => {
    const builder$0040_36 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("3D unit vector dot product", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_36, Test_TestCaseBuilder__Delay_1505(builder$0040_36, () => {
        let a_17, l_10, li_5, b_16, l_11, li_6;
        Expect_floatClose(tolMed, (a_17 = ((l_10 = Math.sqrt(((1 * 1) + (0 * 0)) + (0 * 0)), (!(l_10 > 1E-12) ? failUnit3("UnitVec.create", 1, 0, 0) : undefined, (li_5 = (1 / l_10), UnitVec_$ctor_Z7AD9E565(li_5 * 1, li_5 * 0, li_5 * 0))))), (b_16 = ((l_11 = Math.sqrt(((1 * 1) + (1 * 1)) + (0 * 0)), (!(l_11 > 1E-12) ? failUnit3("UnitVec.create", 1, 1, 0) : undefined, (li_6 = (1 / l_11), UnitVec_$ctor_Z7AD9E565(li_6 * 1, li_6 * 1, li_6 * 0))))), ((a_17.X * b_16.X) + (a_17.Y * b_16.Y)) + (a_17.Z * b_16.Z))), 1 / Math.sqrt(2), "dot is 1/sqrt(2)");
        Test_TestCaseBuilder__Zero(builder$0040_36);
    }));
})(), (() => {
    const builder$0040_37 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("3D unit vector cross product", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_37, Test_TestCaseBuilder__Delay_1505(builder$0040_37, () => {
        let cross_1;
        let a_18;
        const l_12 = Math.sqrt(((1 * 1) + (0 * 0)) + (0 * 0));
        if (!(l_12 > 1E-12)) {
            failUnit3("UnitVec.create", 1, 0, 0);
        }
        const li_7 = 1 / l_12;
        a_18 = UnitVec_$ctor_Z7AD9E565(li_7 * 1, li_7 * 0, li_7 * 0);
        let b_17;
        const l_13 = Math.sqrt(((0 * 0) + (1 * 1)) + (0 * 0));
        if (!(l_13 > 1E-12)) {
            failUnit3("UnitVec.create", 0, 1, 0);
        }
        const li_8 = 1 / l_13;
        b_17 = UnitVec_$ctor_Z7AD9E565(li_8 * 0, li_8 * 1, li_8 * 0);
        cross_1 = Vec_$ctor_Z7AD9E565_1((a_18.Y * b_17.Z) - (a_18.Z * b_17.Y), (a_18.Z * b_17.X) - (a_18.X * b_17.Z), (a_18.X * b_17.Y) - (a_18.Y * b_17.X));
        Expect_floatClose(tol, cross_1.X, 0, "cross X is 0.0");
        Expect_floatClose(tol, cross_1.Y, 0, "cross Y is 0.0");
        Expect_floatClose(tol, cross_1.Z, 1, "cross Z is 1.0");
        Test_TestCaseBuilder__Zero(builder$0040_37);
    }));
})()])), Test_testList("Edge Cases", ofArray([(() => {
    const builder$0040_38 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("2D zero vector has zero length", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_38, Test_TestCaseBuilder__Delay_1505(builder$0040_38, () => {
        let v_13, x_51, y_33;
        Expect_floatClose(tol, (v_13 = Vc_$ctor_7B00E9A0(0, 0), (x_51 = v_13.X, (y_33 = v_13.Y, Math.sqrt((x_51 * x_51) + (y_33 * y_33))))), 0, "length is 0.0");
        Test_TestCaseBuilder__Zero(builder$0040_38);
    }));
})(), (() => {
    const builder$0040_39 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("3D zero vector has zero length", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_39, Test_TestCaseBuilder__Delay_1505(builder$0040_39, () => {
        let v_14;
        Expect_floatClose(tol, (v_14 = Vec_$ctor_Z7AD9E565(0, 0, 0), Math.sqrt(((v_14.X * v_14.X) + (v_14.Y * v_14.Y)) + (v_14.Z * v_14.Z))), 0, "length is 0.0");
        Test_TestCaseBuilder__Zero(builder$0040_39);
    }));
})(), (() => {
    const builder$0040_40 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("2D very small vector has small length", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_40, Test_TestCaseBuilder__Delay_1505(builder$0040_40, () => {
        let v_15, x_52, y_34;
        Expect_isTrue(((v_15 = Vc_$ctor_7B00E9A0(1E-10, 1E-10), (x_52 = v_15.X, (y_34 = v_15.Y, Math.sqrt((x_52 * x_52) + (y_34 * y_34)))))) < 1E-09)("length is very small");
        Test_TestCaseBuilder__Zero(builder$0040_40);
    }));
})(), (() => {
    const builder$0040_41 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("3D very small vector has small length", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_41, Test_TestCaseBuilder__Delay_1505(builder$0040_41, () => {
        let v_16;
        Expect_isTrue(((v_16 = Vec_$ctor_Z7AD9E565(1E-10, 1E-10, 1E-10), Math.sqrt(((v_16.X * v_16.X) + (v_16.Y * v_16.Y)) + (v_16.Z * v_16.Z)))) < 1E-09)("length is very small");
        Test_TestCaseBuilder__Zero(builder$0040_41);
    }));
})(), (() => {
    const builder$0040_42 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("2D point equality", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_42, Test_TestCaseBuilder__Delay_1505(builder$0040_42, () => {
        let a_20, b_19, vx_2, vy_2;
        Expect_floatClose(tol, (a_20 = Pt_$ctor_7B00E9A0(1, 2), (b_19 = Pt_$ctor_7B00E9A0(1, 2), (vx_2 = (a_20.X - b_19.X), (vy_2 = (a_20.Y - b_19.Y), Math.sqrt((vx_2 * vx_2) + (vy_2 * vy_2)))))), 0, "identical points have zero distance");
        Test_TestCaseBuilder__Zero(builder$0040_42);
    }));
})(), (() => {
    const builder$0040_43 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("3D point equality", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_43, Test_TestCaseBuilder__Delay_1505(builder$0040_43, () => {
        let a_22, b_21, x_53, y_35, z_20;
        Expect_floatClose(tol, (a_22 = Pnt_$ctor_Z7AD9E565(1, 2, 3), (b_21 = Pnt_$ctor_Z7AD9E565(1, 2, 3), (x_53 = (a_22.X - b_21.X), (y_35 = (a_22.Y - b_21.Y), (z_20 = (a_22.Z - b_21.Z), Math.sqrt(((x_53 * x_53) + (y_35 * y_35)) + (z_20 * z_20))))))), 0, "identical points have zero distance");
        Test_TestCaseBuilder__Zero(builder$0040_43);
    }));
})(), (() => {
    const builder$0040_44 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("2D negative vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_44, Test_TestCaseBuilder__Delay_1505(builder$0040_44, () => {
        let neg;
        const v_17 = Vc_$ctor_7B00E9A0(3, 4);
        neg = Vc_$ctor_7B00E9A0_1(v_17.X * -1, v_17.Y * -1);
        Expect_floatClose(tol, neg.X, -3, "negative X is -3.0");
        Expect_floatClose(tol, neg.Y, -4, "negative Y is -4.0");
        Test_TestCaseBuilder__Zero(builder$0040_44);
    }));
})(), (() => {
    const builder$0040_45 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("3D negative vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_45, Test_TestCaseBuilder__Delay_1505(builder$0040_45, () => {
        let neg_1;
        const a_23 = Vec_$ctor_Z7AD9E565(3, 4, 5);
        neg_1 = Vec_$ctor_Z7AD9E565_1(a_23.X * -1, a_23.Y * -1, a_23.Z * -1);
        Expect_floatClose(tol, neg_1.X, -3, "negative X is -3.0");
        Expect_floatClose(tol, neg_1.Y, -4, "negative Y is -4.0");
        Expect_floatClose(tol, neg_1.Z, -5, "negative Z is -5.0");
        Test_TestCaseBuilder__Zero(builder$0040_45);
    }));
})()]))]));

