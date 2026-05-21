
import { Expect_isFalse, Expect_isTrue, Test_TestCaseBuilder__Zero, Expect_floatClose, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList, AccuracyModule_veryHigh } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Pt_$ctor_7B00E9A0 } from "./Src/Pt.js";
import { Tria2D_offsetPtVarByNormals_3ED64C5D, Tria2D_isLinear_365C4B06, Tria2D_offsetPtVar_5FDDACFD, Tria2D_offsetPt_365C4B06, Tria2D_offset_365C4B06 } from "./Src/Tria2D.js";
import { sign, Exception } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { printf, toText } from "./fable_modules/fable-library-js.5.0.0/String.js";
import { Vc_$ctor_7B00E9A0 } from "./Src/Vc.js";
import { failUnit2 } from "./Src/EuclidErrors.js";
import { UnitVc_$ctor_7B00E9A0 } from "./Src/UnitVc.js";
import { Pt_$ctor_7B00E9A0 as Pt_$ctor_7B00E9A0_1 } from "./Src/Pt.js";
import { ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";

export const tol = AccuracyModule_veryHigh;

export const tests = Test_testList("Tria2D ", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("det", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let a_1, b_1, c_1, a_2, b_2, c_2, a_3, b_3, c_3, a_4, b_4, c_4, a_5, b_5, c_5;
        const a = Pt_$ctor_7B00E9A0(0, 0);
        const b = Pt_$ctor_7B00E9A0(1, 0);
        const c = Pt_$ctor_7B00E9A0(0, 1);
        const d = Pt_$ctor_7B00E9A0(1, 1);
        Expect_floatClose(tol, (a_1 = a, (b_1 = a, (c_1 = a, ((b_1.X - a_1.X) * (c_1.Y - a_1.Y)) - ((b_1.Y - a_1.Y) * (c_1.X - a_1.X))))), 0, "det is 0");
        Expect_floatClose(tol, (a_2 = a, (b_2 = b, (c_2 = a, ((b_2.X - a_2.X) * (c_2.Y - a_2.Y)) - ((b_2.Y - a_2.Y) * (c_2.X - a_2.X))))), 0, "det is 0a");
        Expect_floatClose(tol, (a_3 = a, (b_3 = b, (c_3 = c, ((b_3.X - a_3.X) * (c_3.Y - a_3.Y)) - ((b_3.Y - a_3.Y) * (c_3.X - a_3.X))))), 1, "det is 1.0");
        Expect_floatClose(tol, (a_4 = a, (b_4 = b, (c_4 = d, ((b_4.X - a_4.X) * (c_4.Y - a_4.Y)) - ((b_4.Y - a_4.Y) * (c_4.X - a_4.X))))), 1, "det is 1.0b");
        Expect_floatClose(tol, (a_5 = c, (b_5 = b, (c_5 = a, ((b_5.X - a_5.X) * (c_5.Y - a_5.Y)) - ((b_5.Y - a_5.Y) * (c_5.X - a_5.X))))), -1, "det negative orientation");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset distance 0 returns original points (CCW right triangle)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let a_8, b_8, vx, vy, a_10, b_10, vx_1, vy_1, a_12, b_12, vx_2, vy_2;
        const a_6 = Pt_$ctor_7B00E9A0(0, 0);
        const b_6 = Pt_$ctor_7B00E9A0(1, 0);
        const c_6 = Pt_$ctor_7B00E9A0(1, 1);
        const patternInput = Tria2D_offset_365C4B06(a_6, b_6, c_6, 0);
        Expect_isTrue(((a_8 = patternInput[0], (b_8 = a_6, (vx = (a_8.X - b_8.X), (vy = (a_8.Y - b_8.Y), Math.sqrt((vx * vx) + (vy * vy))))))) < 1E-09)("oa==a");
        Expect_isTrue(((a_10 = patternInput[1], (b_10 = b_6, (vx_1 = (a_10.X - b_10.X), (vy_1 = (a_10.Y - b_10.Y), Math.sqrt((vx_1 * vx_1) + (vy_1 * vy_1))))))) < 1E-09)("ob==b");
        Expect_isTrue(((a_12 = patternInput[2], (b_12 = c_6, (vx_2 = (a_12.X - b_12.X), (vy_2 = (a_12.Y - b_12.Y), Math.sqrt((vx_2 * vx_2) + (vy_2 * vy_2))))))) < 1E-09)("oc==c");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset distance 0 returns original points (CW orientation)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let a_15, b_15, vx_3, vy_3, a_17, b_17, vx_4, vy_4, a_19, b_19, vx_5, vy_5;
        const a_13 = Pt_$ctor_7B00E9A0(1, 1);
        const b_13 = Pt_$ctor_7B00E9A0(1, 0);
        const c_7 = Pt_$ctor_7B00E9A0(0, 0);
        const patternInput_1 = Tria2D_offset_365C4B06(a_13, b_13, c_7, 0);
        Expect_isTrue(((a_15 = patternInput_1[0], (b_15 = a_13, (vx_3 = (a_15.X - b_15.X), (vy_3 = (a_15.Y - b_15.Y), Math.sqrt((vx_3 * vx_3) + (vy_3 * vy_3))))))) < 1E-09)("oa==a (cw)");
        Expect_isTrue(((a_17 = patternInput_1[1], (b_17 = b_13, (vx_4 = (a_17.X - b_17.X), (vy_4 = (a_17.Y - b_17.Y), Math.sqrt((vx_4 * vx_4) + (vy_4 * vy_4))))))) < 1E-09)("ob==b (cw)");
        Expect_isTrue(((a_19 = patternInput_1[2], (b_19 = c_7, (vx_5 = (a_19.X - b_19.X), (vy_5 = (a_19.Y - b_19.Y), Math.sqrt((vx_5 * vx_5) + (vy_5 * vy_5))))))) < 1E-09)("oc==c (cw)");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetPt distance 0 returns same point (90-degree corner)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let a_21, b_21, vx_6, vy_6;
        const prev = Pt_$ctor_7B00E9A0(0, 0);
        const pt = Pt_$ctor_7B00E9A0(1, 0);
        Expect_isTrue(((a_21 = Tria2D_offsetPt_365C4B06(pt, prev, Pt_$ctor_7B00E9A0(1, 1), 0), (b_21 = pt, (vx_6 = (a_21.X - b_21.X), (vy_6 = (a_21.Y - b_21.Y), Math.sqrt((vx_6 * vx_6) + (vy_6 * vy_6))))))) < 1E-09)("offsetPt(0) == pt");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetVarByNormals with zero distances returns original point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let a_23, b_23, vx_7, vy_7;
        const prev_1 = Pt_$ctor_7B00E9A0(0, 0);
        const pt_1 = Pt_$ctor_7B00E9A0(1, 0);
        const matchValue = Tria2D_offsetPtVar_5FDDACFD(pt_1, prev_1, Pt_$ctor_7B00E9A0(1, 1), 0, 0);
        if (matchValue == null) {
            throw new Exception("expected point for zero distances");
            Test_TestCaseBuilder__Zero(builder$0040_4);
        }
        else {
            Expect_isTrue(((a_23 = matchValue, (b_23 = pt_1, (vx_7 = (a_23.X - b_23.X), (vy_7 = (a_23.Y - b_23.Y), Math.sqrt((vx_7 * vx_7) + (vy_7 * vy_7))))))) < 1E-09)("offsetVarByNormals(0,0)==pt");
            Test_TestCaseBuilder__Zero(builder$0040_4);
        }
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("area, areaDouble, areaSigned", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let a_26, b_26, c_10, a_29, b_29, c_13, a_31, b_31, c_15, a_33, b_33, c_17;
        const a_24 = Pt_$ctor_7B00E9A0(0, 0);
        const b_24 = Pt_$ctor_7B00E9A0(4, 0);
        const c_8 = Pt_$ctor_7B00E9A0(0, 3);
        Expect_floatClose(tol, Math.abs((a_26 = a_24, (b_26 = b_24, (c_10 = c_8, ((b_26.X - a_26.X) * (c_10.Y - a_26.Y)) - ((b_26.Y - a_26.Y) * (c_10.X - a_26.X)))))), 12, "areaDouble = 12");
        Expect_floatClose(tol, Math.abs((a_29 = a_24, (b_29 = b_24, (c_13 = c_8, ((b_29.X - a_29.X) * (c_13.Y - a_29.Y)) - ((b_29.Y - a_29.Y) * (c_13.X - a_29.X)))))) * 0.5, 6, "area = 6");
        Expect_floatClose(tol, ((a_31 = a_24, (b_31 = b_24, (c_15 = c_8, ((b_31.X - a_31.X) * (c_15.Y - a_31.Y)) - ((b_31.Y - a_31.Y) * (c_15.X - a_31.X)))))) * 0.5, 6, "areaSigned = +6");
        Expect_floatClose(tol, ((a_33 = a_24, (b_33 = c_8, (c_17 = b_24, ((b_33.X - a_33.X) * (c_17.Y - a_33.Y)) - ((b_33.Y - a_33.Y) * (c_17.X - a_33.X)))))) * 0.5, -6, "areaSigned = -6 (reversed)");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("isLinearFast collinear and non-collinear", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        let a_37, b_37, c_21, a_40, b_40, c_24, a_43, b_43, c_27;
        const a_34 = Pt_$ctor_7B00E9A0(0, 0);
        const b_34 = Pt_$ctor_7B00E9A0(1, 1);
        const c_18 = Pt_$ctor_7B00E9A0(2, 2);
        Expect_isTrue(Math.abs((a_37 = a_34, (b_37 = b_34, (c_21 = c_18, ((b_37.X - a_37.X) * (c_21.Y - a_37.Y)) - ((b_37.Y - a_37.Y) * (c_21.X - a_37.X)))))) < 1E-06)("collinear should be linear (fast)");
        const a$0027 = Pt_$ctor_7B00E9A0(0, 0);
        const b$0027 = Pt_$ctor_7B00E9A0(1, 0);
        const c$0027 = Pt_$ctor_7B00E9A0(0, 1);
        Expect_isFalse(Math.abs((a_40 = a$0027, (b_40 = b$0027, (c_24 = c$0027, ((b_40.X - a_40.X) * (c_24.Y - a_40.Y)) - ((b_40.Y - a_40.Y) * (c_24.X - a_40.X)))))) < 0.1)("non-collinear should not be linear for small tol (fast)");
        Expect_isTrue(Math.abs((a_43 = a$0027, (b_43 = b$0027, (c_27 = c$0027, ((b_43.X - a_43.X) * (c_27.Y - a_43.Y)) - ((b_43.Y - a_43.Y) * (c_27.X - a_43.X)))))) < 2)("non-collinear can be considered linear for huge tol (fast)");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("isLinear robust, including duplicate points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        Expect_isFalse(Tria2D_isLinear_365C4B06(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(0, 1), 1E-06))("right triangle should be non-linear");
        Expect_isTrue(Tria2D_isLinear_365C4B06(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(2, 2), 1E-06))("diagonal points should be linear");
        Expect_isTrue(Tria2D_isLinear_365C4B06(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), 1E-06))("duplicate points treated as linear");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetPt at 90-degree corner (equal distances)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let a_46, b_46, vx_8, vy_8;
        const prev_2 = Pt_$ctor_7B00E9A0(0, 0);
        const o_2 = Tria2D_offsetPt_365C4B06(Pt_$ctor_7B00E9A0(1, 0), prev_2, Pt_$ctor_7B00E9A0(1, 1), 0.25);
        const expected_9 = Pt_$ctor_7B00E9A0(1 - 0.25, 0.25);
        Expect_isTrue(((a_46 = o_2, (b_46 = expected_9, (vx_8 = (a_46.X - b_46.X), (vy_8 = (a_46.Y - b_46.Y), Math.sqrt((vx_8 * vx_8) + (vy_8 * vy_8))))))) < 1E-09)(toText(printf("offsetPt expected %A, got %A"))(expected_9)(o_2));
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetPt 180-degree U-turn guard", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const prev_3 = Pt_$ctor_7B00E9A0(0, 0);
        const o_3 = Tria2D_offsetPt_365C4B06(Pt_$ctor_7B00E9A0(1, 0), prev_3, Pt_$ctor_7B00E9A0(0, 0), 1);
        Expect_isTrue(o_3.X > 10)(toText(printf("expected large X due to U-turn, got %A"))(o_3));
        Expect_floatClose(tol, o_3.Y, 0, "Y ~ 0 at U-turn");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetVarByNormals equal distances matches offsetPt at 90-degree corner", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let v_3, v_1, a_47, b_47, x, y, l, v_7, v_5, a_48, b_48, x_4, y_3, l_1, a_50, b_50, vx_9, vy_9;
        const prev_4 = Pt_$ctor_7B00E9A0(0, 0);
        const pt_4 = Pt_$ctor_7B00E9A0(1, 0);
        const next_4 = Pt_$ctor_7B00E9A0(1, 1);
        const matchValue_1 = Tria2D_offsetPtVarByNormals_3ED64C5D(pt_4, (v_3 = ((v_1 = ((a_47 = pt_4, (b_47 = prev_4, Vc_$ctor_7B00E9A0(a_47.X - b_47.X, a_47.Y - b_47.Y)))), (x = v_1.X, (y = v_1.Y, (l = Math.sqrt((x * x) + (y * y)), (!(l > 1E-12) ? failUnit2("Vc.unitize", x, y) : undefined, UnitVc_$ctor_7B00E9A0(x / l, y / l))))))), UnitVc_$ctor_7B00E9A0(-v_3.Y, v_3.X)), (v_7 = ((v_5 = ((a_48 = next_4, (b_48 = pt_4, Vc_$ctor_7B00E9A0(a_48.X - b_48.X, a_48.Y - b_48.Y)))), (x_4 = v_5.X, (y_3 = v_5.Y, (l_1 = Math.sqrt((x_4 * x_4) + (y_3 * y_3)), (!(l_1 > 1E-12) ? failUnit2("Vc.unitize", x_4, y_3) : undefined, UnitVc_$ctor_7B00E9A0(x_4 / l_1, y_3 / l_1))))))), UnitVc_$ctor_7B00E9A0(-v_7.Y, v_7.X)), 0.37, 0.37);
        if (matchValue_1 == null) {
            throw new Exception("offsetVarByNormals returned None unexpectedly");
            Test_TestCaseBuilder__Zero(builder$0040_10);
        }
        else {
            const o_4 = matchValue_1;
            const expected_11 = Pt_$ctor_7B00E9A0(1 - 0.37, 0.37);
            Expect_isTrue(((a_50 = o_4, (b_50 = expected_11, (vx_9 = (a_50.X - b_50.X), (vy_9 = (a_50.Y - b_50.Y), Math.sqrt((vx_9 * vx_9) + (vy_9 * vy_9))))))) < 1E-09)(toText(printf("offsetVarByNormals(expected=%A, got=%A)"))(expected_11)(o_4));
            Test_TestCaseBuilder__Zero(builder$0040_10);
        }
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetVarByNormals with different distances (90-degree corner)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        let v_11, v_9, a_51, b_51, x_8, y_6, l_2, v_15, v_13, a_52, b_52, x_12, y_9, l_3, a_54, b_54, vx_10, vy_10;
        const prev_5 = Pt_$ctor_7B00E9A0(0, 0);
        const pt_5 = Pt_$ctor_7B00E9A0(1, 0);
        const next_5 = Pt_$ctor_7B00E9A0(1, 1);
        const matchValue_2 = Tria2D_offsetPtVarByNormals_3ED64C5D(pt_5, (v_11 = ((v_9 = ((a_51 = pt_5, (b_51 = prev_5, Vc_$ctor_7B00E9A0(a_51.X - b_51.X, a_51.Y - b_51.Y)))), (x_8 = v_9.X, (y_6 = v_9.Y, (l_2 = Math.sqrt((x_8 * x_8) + (y_6 * y_6)), (!(l_2 > 1E-12) ? failUnit2("Vc.unitize", x_8, y_6) : undefined, UnitVc_$ctor_7B00E9A0(x_8 / l_2, y_6 / l_2))))))), UnitVc_$ctor_7B00E9A0(-v_11.Y, v_11.X)), (v_15 = ((v_13 = ((a_52 = next_5, (b_52 = pt_5, Vc_$ctor_7B00E9A0(a_52.X - b_52.X, a_52.Y - b_52.Y)))), (x_12 = v_13.X, (y_9 = v_13.Y, (l_3 = Math.sqrt((x_12 * x_12) + (y_9 * y_9)), (!(l_3 > 1E-12) ? failUnit2("Vc.unitize", x_12, y_9) : undefined, UnitVc_$ctor_7B00E9A0(x_12 / l_3, y_9 / l_3))))))), UnitVc_$ctor_7B00E9A0(-v_15.Y, v_15.X)), 0.2, 0.7);
        if (matchValue_2 == null) {
            throw new Exception("offsetVarByNormals returned None unexpectedly");
            Test_TestCaseBuilder__Zero(builder$0040_11);
        }
        else {
            const o_5 = matchValue_2;
            const expected_12 = Pt_$ctor_7B00E9A0(1 - 0.7, 0.2);
            Expect_isTrue(((a_54 = o_5, (b_54 = expected_12, (vx_10 = (a_54.X - b_54.X), (vy_10 = (a_54.Y - b_54.Y), Math.sqrt((vx_10 * vx_10) + (vy_10 * vy_10))))))) < 1E-09)(toText(printf("offsetVarByNormals(expected=%A, got=%A)"))(expected_12)(o_5));
            Test_TestCaseBuilder__Zero(builder$0040_11);
        }
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetPtVar with different distances (90-degree corner)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        let a_56, b_56, vx_11, vy_11;
        const prev_6 = Pt_$ctor_7B00E9A0(0, 0);
        const matchValue_3 = Tria2D_offsetPtVar_5FDDACFD(Pt_$ctor_7B00E9A0(1, 0), prev_6, Pt_$ctor_7B00E9A0(1, 1), 0.2, 0.7);
        if (matchValue_3 == null) {
            throw new Exception("offsetVarByNormals returned None unexpectedly");
            Test_TestCaseBuilder__Zero(builder$0040_12);
        }
        else {
            const o_6 = matchValue_3;
            const expected_13 = Pt_$ctor_7B00E9A0(1 - 0.7, 0.2);
            Expect_isTrue(((a_56 = o_6, (b_56 = expected_13, (vx_11 = (a_56.X - b_56.X), (vy_11 = (a_56.Y - b_56.Y), Math.sqrt((vx_11 * vx_11) + (vy_11 * vy_11))))))) < 1E-09)(toText(printf("offsetVarByNormals(expected=%A, got=%A)"))(expected_13)(o_6));
            Test_TestCaseBuilder__Zero(builder$0040_12);
        }
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetPtVar with different distances (90-degree corner), rev order", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        let a_58, b_58, vx_12, vy_12;
        const prev_7 = Pt_$ctor_7B00E9A0(1, 1);
        const matchValue_4 = Tria2D_offsetPtVar_5FDDACFD(Pt_$ctor_7B00E9A0(1, 0), prev_7, Pt_$ctor_7B00E9A0(0, 0), 0.2, 0.7);
        if (matchValue_4 == null) {
            throw new Exception("offsetVarByNormals returned None unexpectedly");
            Test_TestCaseBuilder__Zero(builder$0040_13);
        }
        else {
            const o_7 = matchValue_4;
            const expected_14 = Pt_$ctor_7B00E9A0(1 + 0.2, -0.7);
            Expect_isTrue(((a_58 = o_7, (b_58 = expected_14, (vx_12 = (a_58.X - b_58.X), (vy_12 = (a_58.Y - b_58.Y), Math.sqrt((vx_12 * vx_12) + (vy_12 * vy_12))))))) < 1E-09)(toText(printf("offsetVarByNormals(expected=%A, got=%A)"))(expected_14)(o_7));
            Test_TestCaseBuilder__Zero(builder$0040_13);
        }
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset equals per-vertex offsetVarByNormals (right triangle)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let a_64, b_64, vx_13, vy_13, a_66, b_66, vx_14, vy_14, a_68, b_68, vx_15, vy_15;
        const a_59 = Pt_$ctor_7B00E9A0(0, 0);
        const b_59 = Pt_$ctor_7B00E9A0(1, 0);
        const c_29 = Pt_$ctor_7B00E9A0(1, 1);
        let na;
        let v_19;
        let v_17;
        const a_60 = b_59;
        const b_60 = a_59;
        v_17 = Vc_$ctor_7B00E9A0(a_60.X - b_60.X, a_60.Y - b_60.Y);
        const x_16 = v_17.X;
        const y_12 = v_17.Y;
        const l_4 = Math.sqrt((x_16 * x_16) + (y_12 * y_12));
        if (!(l_4 > 1E-12)) {
            failUnit2("Vc.unitize", x_16, y_12);
        }
        v_19 = UnitVc_$ctor_7B00E9A0(x_16 / l_4, y_12 / l_4);
        na = UnitVc_$ctor_7B00E9A0(-v_19.Y, v_19.X);
        let nb;
        let v_23;
        let v_21;
        const a_61 = c_29;
        const b_61 = b_59;
        v_21 = Vc_$ctor_7B00E9A0(a_61.X - b_61.X, a_61.Y - b_61.Y);
        const x_20 = v_21.X;
        const y_15 = v_21.Y;
        const l_5 = Math.sqrt((x_20 * x_20) + (y_15 * y_15));
        if (!(l_5 > 1E-12)) {
            failUnit2("Vc.unitize", x_20, y_15);
        }
        v_23 = UnitVc_$ctor_7B00E9A0(x_20 / l_5, y_15 / l_5);
        nb = UnitVc_$ctor_7B00E9A0(-v_23.Y, v_23.X);
        let nc;
        let v_27;
        let v_25;
        const a_62 = a_59;
        const b_62 = c_29;
        v_25 = Vc_$ctor_7B00E9A0(a_62.X - b_62.X, a_62.Y - b_62.Y);
        const x_24 = v_25.X;
        const y_18 = v_25.Y;
        const l_6 = Math.sqrt((x_24 * x_24) + (y_18 * y_18));
        if (!(l_6 > 1E-12)) {
            failUnit2("Vc.unitize", x_24, y_18);
        }
        v_27 = UnitVc_$ctor_7B00E9A0(x_24 / l_6, y_18 / l_6);
        nc = UnitVc_$ctor_7B00E9A0(-v_27.Y, v_27.X);
        let ea;
        const matchValue_5 = Tria2D_offsetPtVarByNormals_3ED64C5D(a_59, na, nc, 0.37, 0.37);
        if (matchValue_5 != null) {
            ea = matchValue_5;
        }
        else {
            throw new Exception("expected ea");
        }
        let eb;
        const matchValue_6 = Tria2D_offsetPtVarByNormals_3ED64C5D(b_59, na, nb, 0.37, 0.37);
        if (matchValue_6 != null) {
            eb = matchValue_6;
        }
        else {
            throw new Exception("expected eb");
        }
        let ec;
        const matchValue_7 = Tria2D_offsetPtVarByNormals_3ED64C5D(c_29, nb, nc, 0.37, 0.37);
        if (matchValue_7 != null) {
            ec = matchValue_7;
        }
        else {
            throw new Exception("expected ec");
        }
        const patternInput_2 = Tria2D_offset_365C4B06(a_59, b_59, c_29, 0.37);
        Expect_isTrue(((a_64 = patternInput_2[0], (b_64 = ea, (vx_13 = (a_64.X - b_64.X), (vy_13 = (a_64.Y - b_64.Y), Math.sqrt((vx_13 * vx_13) + (vy_13 * vy_13))))))) < 1E-09)("oa matches");
        Expect_isTrue(((a_66 = patternInput_2[1], (b_66 = eb, (vx_14 = (a_66.X - b_66.X), (vy_14 = (a_66.Y - b_66.Y), Math.sqrt((vx_14 * vx_14) + (vy_14 * vy_14))))))) < 1E-09)("ob matches");
        Expect_isTrue(((a_68 = patternInput_2[2], (b_68 = ec, (vx_15 = (a_68.X - b_68.X), (vy_15 = (a_68.Y - b_68.Y), Math.sqrt((vx_15 * vx_15) + (vy_15 * vy_15))))))) < 1E-09)("oc matches");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset reversed orientation and negative distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        let a_74, b_74, vx_16, vy_16, a_76, b_76, vx_17, vy_17, a_78, b_78, vx_18, vy_18;
        const a_69 = Pt_$ctor_7B00E9A0(1, 1);
        const b_69 = Pt_$ctor_7B00E9A0(1, 0);
        const c_30 = Pt_$ctor_7B00E9A0(0, 0);
        let na_1;
        let v_31;
        let v_29;
        const a_70 = b_69;
        const b_70 = a_69;
        v_29 = Vc_$ctor_7B00E9A0(a_70.X - b_70.X, a_70.Y - b_70.Y);
        const x_28 = v_29.X;
        const y_21 = v_29.Y;
        const l_7 = Math.sqrt((x_28 * x_28) + (y_21 * y_21));
        if (!(l_7 > 1E-12)) {
            failUnit2("Vc.unitize", x_28, y_21);
        }
        v_31 = UnitVc_$ctor_7B00E9A0(x_28 / l_7, y_21 / l_7);
        na_1 = UnitVc_$ctor_7B00E9A0(-v_31.Y, v_31.X);
        let nb_1;
        let v_35;
        let v_33;
        const a_71 = c_30;
        const b_71 = b_69;
        v_33 = Vc_$ctor_7B00E9A0(a_71.X - b_71.X, a_71.Y - b_71.Y);
        const x_32 = v_33.X;
        const y_24 = v_33.Y;
        const l_8 = Math.sqrt((x_32 * x_32) + (y_24 * y_24));
        if (!(l_8 > 1E-12)) {
            failUnit2("Vc.unitize", x_32, y_24);
        }
        v_35 = UnitVc_$ctor_7B00E9A0(x_32 / l_8, y_24 / l_8);
        nb_1 = UnitVc_$ctor_7B00E9A0(-v_35.Y, v_35.X);
        let nc_1;
        let v_39;
        let v_37;
        const a_72 = a_69;
        const b_72 = c_30;
        v_37 = Vc_$ctor_7B00E9A0(a_72.X - b_72.X, a_72.Y - b_72.Y);
        const x_36 = v_37.X;
        const y_27 = v_37.Y;
        const l_9 = Math.sqrt((x_36 * x_36) + (y_27 * y_27));
        if (!(l_9 > 1E-12)) {
            failUnit2("Vc.unitize", x_36, y_27);
        }
        v_39 = UnitVc_$ctor_7B00E9A0(x_36 / l_9, y_27 / l_9);
        nc_1 = UnitVc_$ctor_7B00E9A0(-v_39.Y, v_39.X);
        let ea_1;
        const matchValue_8 = Tria2D_offsetPtVarByNormals_3ED64C5D(a_69, na_1, nc_1, -0.2, -0.2);
        if (matchValue_8 != null) {
            ea_1 = matchValue_8;
        }
        else {
            throw new Exception("expected ea");
        }
        let eb_1;
        const matchValue_9 = Tria2D_offsetPtVarByNormals_3ED64C5D(b_69, na_1, nb_1, -0.2, -0.2);
        if (matchValue_9 != null) {
            eb_1 = matchValue_9;
        }
        else {
            throw new Exception("expected eb");
        }
        let ec_1;
        const matchValue_10 = Tria2D_offsetPtVarByNormals_3ED64C5D(c_30, nb_1, nc_1, -0.2, -0.2);
        if (matchValue_10 != null) {
            ec_1 = matchValue_10;
        }
        else {
            throw new Exception("expected ec");
        }
        const patternInput_3 = Tria2D_offset_365C4B06(a_69, b_69, c_30, -0.2);
        Expect_isTrue(((a_74 = patternInput_3[0], (b_74 = ea_1, (vx_16 = (a_74.X - b_74.X), (vy_16 = (a_74.Y - b_74.Y), Math.sqrt((vx_16 * vx_16) + (vy_16 * vy_16))))))) < 1E-09)("oa matches (rev/neg)");
        Expect_isTrue(((a_76 = patternInput_3[1], (b_76 = eb_1, (vx_17 = (a_76.X - b_76.X), (vy_17 = (a_76.Y - b_76.Y), Math.sqrt((vx_17 * vx_17) + (vy_17 * vy_17))))))) < 1E-09)("ob matches (rev/neg)");
        Expect_isTrue(((a_78 = patternInput_3[2], (b_78 = ec_1, (vx_18 = (a_78.X - b_78.X), (vy_18 = (a_78.Y - b_78.Y), Math.sqrt((vx_18 * vx_18) + (vy_18 * vy_18))))))) < 1E-09)("oc matches (rev/neg)");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset positive then negative returns to original (CCW right triangle)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let a_81, b_81, vx_19, vy_19, a_83, b_83, vx_20, vy_20, a_85, b_85, vx_21, vy_21;
        const a_79 = Pt_$ctor_7B00E9A0(0, 0);
        const b_79 = Pt_$ctor_7B00E9A0(1, 0);
        const c_31 = Pt_$ctor_7B00E9A0(1, 1);
        const patternInput_4 = Tria2D_offset_365C4B06(a_79, b_79, c_31, 0.25);
        const patternInput_5 = Tria2D_offset_365C4B06(patternInput_4[0], patternInput_4[1], patternInput_4[2], -0.25);
        Expect_isTrue(((a_81 = patternInput_5[0], (b_81 = a_79, (vx_19 = (a_81.X - b_81.X), (vy_19 = (a_81.Y - b_81.Y), Math.sqrt((vx_19 * vx_19) + (vy_19 * vy_19))))))) < 1E-09)("a returns to original after +d then -d");
        Expect_isTrue(((a_83 = patternInput_5[1], (b_83 = b_79, (vx_20 = (a_83.X - b_83.X), (vy_20 = (a_83.Y - b_83.Y), Math.sqrt((vx_20 * vx_20) + (vy_20 * vy_20))))))) < 1E-09)("b returns to original after +d then -d");
        Expect_isTrue(((a_85 = patternInput_5[2], (b_85 = c_31, (vx_21 = (a_85.X - b_85.X), (vy_21 = (a_85.Y - b_85.Y), Math.sqrt((vx_21 * vx_21) + (vy_21 * vy_21))))))) < 1E-09)("c returns to original after +d then -d");
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset negative then positive returns to original (CCW right triangle)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        let a_88, b_88, vx_22, vy_22, a_90, b_90, vx_23, vy_23, a_92, b_92, vx_24, vy_24;
        const a_86 = Pt_$ctor_7B00E9A0(0, 0);
        const b_86 = Pt_$ctor_7B00E9A0(1, 0);
        const c_32 = Pt_$ctor_7B00E9A0(1, 1);
        const patternInput_6 = Tria2D_offset_365C4B06(a_86, b_86, c_32, -0.25);
        const patternInput_7 = Tria2D_offset_365C4B06(patternInput_6[0], patternInput_6[1], patternInput_6[2], 0.25);
        Expect_isTrue(((a_88 = patternInput_7[0], (b_88 = a_86, (vx_22 = (a_88.X - b_88.X), (vy_22 = (a_88.Y - b_88.Y), Math.sqrt((vx_22 * vx_22) + (vy_22 * vy_22))))))) < 1E-09)("a returns to original after -d then +d");
        Expect_isTrue(((a_90 = patternInput_7[1], (b_90 = b_86, (vx_23 = (a_90.X - b_90.X), (vy_23 = (a_90.Y - b_90.Y), Math.sqrt((vx_23 * vx_23) + (vy_23 * vy_23))))))) < 1E-09)("b returns to original after -d then +d");
        Expect_isTrue(((a_92 = patternInput_7[2], (b_92 = c_32, (vx_24 = (a_92.X - b_92.X), (vy_24 = (a_92.Y - b_92.Y), Math.sqrt((vx_24 * vx_24) + (vy_24 * vy_24))))))) < 1E-09)("c returns to original after -d then +d");
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset positive then negative returns to original (CW triangle)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        let a_95, b_95, vx_25, vy_25, a_97, b_97, vx_26, vy_26, a_99, b_99, vx_27, vy_27;
        const a_93 = Pt_$ctor_7B00E9A0(1, 1);
        const b_93 = Pt_$ctor_7B00E9A0(1, 0);
        const c_33 = Pt_$ctor_7B00E9A0(0, 0);
        const patternInput_8 = Tria2D_offset_365C4B06(a_93, b_93, c_33, 0.11);
        const patternInput_9 = Tria2D_offset_365C4B06(patternInput_8[0], patternInput_8[1], patternInput_8[2], -0.11);
        Expect_isTrue(((a_95 = patternInput_9[0], (b_95 = a_93, (vx_25 = (a_95.X - b_95.X), (vy_25 = (a_95.Y - b_95.Y), Math.sqrt((vx_25 * vx_25) + (vy_25 * vy_25))))))) < 1E-09)("a returns to original after +d then -d (CW)");
        Expect_isTrue(((a_97 = patternInput_9[1], (b_97 = b_93, (vx_26 = (a_97.X - b_97.X), (vy_26 = (a_97.Y - b_97.Y), Math.sqrt((vx_26 * vx_26) + (vy_26 * vy_26))))))) < 1E-09)("b returns to original after +d then -d (CW)");
        Expect_isTrue(((a_99 = patternInput_9[2], (b_99 = c_33, (vx_27 = (a_99.X - b_99.X), (vy_27 = (a_99.Y - b_99.Y), Math.sqrt((vx_27 * vx_27) + (vy_27 * vy_27))))))) < 1E-09)("c returns to original after +d then -d (CW)");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset negative then positive returns to original (CW triangle)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        let a_102, b_102, vx_28, vy_28, a_104, b_104, vx_29, vy_29, a_106, b_106, vx_30, vy_30;
        const a_100 = Pt_$ctor_7B00E9A0(1, 1);
        const b_100 = Pt_$ctor_7B00E9A0(1, 0);
        const c_34 = Pt_$ctor_7B00E9A0(0, 0);
        const patternInput_10 = Tria2D_offset_365C4B06(a_100, b_100, c_34, -0.12);
        const patternInput_11 = Tria2D_offset_365C4B06(patternInput_10[0], patternInput_10[1], patternInput_10[2], 0.12);
        Expect_isTrue(((a_102 = patternInput_11[0], (b_102 = a_100, (vx_28 = (a_102.X - b_102.X), (vy_28 = (a_102.Y - b_102.Y), Math.sqrt((vx_28 * vx_28) + (vy_28 * vy_28))))))) < 1E-09)("\'a\' returns to original after -d then +d (CW)");
        Expect_isTrue(((a_104 = patternInput_11[1], (b_104 = b_100, (vx_29 = (a_104.X - b_104.X), (vy_29 = (a_104.Y - b_104.Y), Math.sqrt((vx_29 * vx_29) + (vy_29 * vy_29))))))) < 1E-09)("\'b\' returns to original after -d then +d (CW)");
        Expect_isTrue(((a_106 = patternInput_11[2], (b_106 = c_34, (vx_30 = (a_106.X - b_106.X), (vy_30 = (a_106.Y - b_106.Y), Math.sqrt((vx_30 * vx_30) + (vy_30 * vy_30))))))) < 1E-09)("\'c\' returns to original after -d then +d (CW)");
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset positive then negative returns to original (equilateral triangle)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        let a_109, b_109, vx_31, vy_31, a_111, b_111, vx_32, vy_32, a_113, b_113, vx_33, vy_33;
        const sqrt3 = Math.sqrt(3);
        const a_107 = Pt_$ctor_7B00E9A0(0, 0);
        const b_107 = Pt_$ctor_7B00E9A0(2, 0);
        const c_35 = Pt_$ctor_7B00E9A0(1, sqrt3);
        const patternInput_12 = Tria2D_offset_365C4B06(a_107, b_107, c_35, 0.111);
        const patternInput_13 = Tria2D_offset_365C4B06(patternInput_12[0], patternInput_12[1], patternInput_12[2], -0.111);
        Expect_isTrue(((a_109 = patternInput_13[0], (b_109 = a_107, (vx_31 = (a_109.X - b_109.X), (vy_31 = (a_109.Y - b_109.Y), Math.sqrt((vx_31 * vx_31) + (vy_31 * vy_31))))))) < 1E-09)("a returns to original after +d then -d (equilateral)");
        Expect_isTrue(((a_111 = patternInput_13[1], (b_111 = b_107, (vx_32 = (a_111.X - b_111.X), (vy_32 = (a_111.Y - b_111.Y), Math.sqrt((vx_32 * vx_32) + (vy_32 * vy_32))))))) < 1E-09)("b returns to original after +d then -d (equilateral)");
        Expect_isTrue(((a_113 = patternInput_13[2], (b_113 = c_35, (vx_33 = (a_113.X - b_113.X), (vy_33 = (a_113.Y - b_113.Y), Math.sqrt((vx_33 * vx_33) + (vy_33 * vy_33))))))) < 1E-09)("c returns to original after +d then -d (equilateral)");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetPtVar positive then negative returns to original (equal distances)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        let p_6, v_40, a_114, b_114, p_7, v_41, a_115, b_115, a_117, b_117, vx_34, vy_34;
        const prev_8 = Pt_$ctor_7B00E9A0(0, 0);
        const pt_8 = Pt_$ctor_7B00E9A0(1, 0);
        const next_8 = Pt_$ctor_7B00E9A0(1, 1);
        const matchValue_11 = Tria2D_offsetPtVar_5FDDACFD(pt_8, prev_8, next_8, 0.25, 0.25);
        if (matchValue_11 == null) {
            throw new Exception("first offsetPtVar returned None");
            Test_TestCaseBuilder__Zero(builder$0040_21);
        }
        else {
            const o1 = matchValue_11;
            const matchValue_12 = Tria2D_offsetPtVar_5FDDACFD(o1, (p_6 = prev_8, (v_40 = ((a_114 = o1, (b_114 = pt_8, Vc_$ctor_7B00E9A0(a_114.X - b_114.X, a_114.Y - b_114.Y)))), Pt_$ctor_7B00E9A0_1(p_6.X + v_40.X, p_6.Y + v_40.Y))), (p_7 = next_8, (v_41 = ((a_115 = o1, (b_115 = pt_8, Vc_$ctor_7B00E9A0(a_115.X - b_115.X, a_115.Y - b_115.Y)))), Pt_$ctor_7B00E9A0_1(p_7.X + v_41.X, p_7.Y + v_41.Y))), -0.25, -0.25);
            if (matchValue_12 == null) {
                throw new Exception("second offsetPtVar returned None");
                Test_TestCaseBuilder__Zero(builder$0040_21);
            }
            else {
                Expect_isTrue(((a_117 = matchValue_12, (b_117 = pt_8, (vx_34 = (a_117.X - b_117.X), (vy_34 = (a_117.Y - b_117.Y), Math.sqrt((vx_34 * vx_34) + (vy_34 * vy_34))))))) < 1E-09)("offsetPtVar point returns to original after +d then -d (equal)");
                Test_TestCaseBuilder__Zero(builder$0040_21);
            }
        }
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetPtVar negative then positive returns to original (equal distances)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        let p_8, v_42, a_118, b_118, p_9, v_43, a_119, b_119, a_121, b_121, vx_35, vy_35;
        const prev_9 = Pt_$ctor_7B00E9A0(0, 0);
        const pt_9 = Pt_$ctor_7B00E9A0(1, 0);
        const next_9 = Pt_$ctor_7B00E9A0(1, 1);
        const matchValue_13 = Tria2D_offsetPtVar_5FDDACFD(pt_9, prev_9, next_9, -0.25, -0.25);
        if (matchValue_13 == null) {
            throw new Exception("first offsetPtVar returned None");
            Test_TestCaseBuilder__Zero(builder$0040_22);
        }
        else {
            const o1_1 = matchValue_13;
            const matchValue_14 = Tria2D_offsetPtVar_5FDDACFD(o1_1, (p_8 = prev_9, (v_42 = ((a_118 = o1_1, (b_118 = pt_9, Vc_$ctor_7B00E9A0(a_118.X - b_118.X, a_118.Y - b_118.Y)))), Pt_$ctor_7B00E9A0_1(p_8.X + v_42.X, p_8.Y + v_42.Y))), (p_9 = next_9, (v_43 = ((a_119 = o1_1, (b_119 = pt_9, Vc_$ctor_7B00E9A0(a_119.X - b_119.X, a_119.Y - b_119.Y)))), Pt_$ctor_7B00E9A0_1(p_9.X + v_43.X, p_9.Y + v_43.Y))), 0.25, 0.25);
            if (matchValue_14 == null) {
                throw new Exception("second offsetPtVar returned None");
                Test_TestCaseBuilder__Zero(builder$0040_22);
            }
            else {
                Expect_isTrue(((a_121 = matchValue_14, (b_121 = pt_9, (vx_35 = (a_121.X - b_121.X), (vy_35 = (a_121.Y - b_121.Y), Math.sqrt((vx_35 * vx_35) + (vy_35 * vy_35))))))) < 1E-09)("offsetPtVar point returns to original after -d then +d (equal)");
                Test_TestCaseBuilder__Zero(builder$0040_22);
            }
        }
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetPtVar positive then negative returns to original (different distances)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        let p_10, v_44, a_122, b_122, p_11, v_45, a_123, b_123, a_125, b_125, vx_36, vy_36;
        const prev_10 = Pt_$ctor_7B00E9A0(0, 0);
        const pt_10 = Pt_$ctor_7B00E9A0(1, 0);
        const next_10 = Pt_$ctor_7B00E9A0(1, 1);
        const matchValue_15 = Tria2D_offsetPtVar_5FDDACFD(pt_10, prev_10, next_10, 0.2, 0.3);
        if (matchValue_15 == null) {
            throw new Exception("first offsetPtVar returned None");
            Test_TestCaseBuilder__Zero(builder$0040_23);
        }
        else {
            const o1_2 = matchValue_15;
            const matchValue_16 = Tria2D_offsetPtVar_5FDDACFD(o1_2, (p_10 = prev_10, (v_44 = ((a_122 = o1_2, (b_122 = pt_10, Vc_$ctor_7B00E9A0(a_122.X - b_122.X, a_122.Y - b_122.Y)))), Pt_$ctor_7B00E9A0_1(p_10.X + v_44.X, p_10.Y + v_44.Y))), (p_11 = next_10, (v_45 = ((a_123 = o1_2, (b_123 = pt_10, Vc_$ctor_7B00E9A0(a_123.X - b_123.X, a_123.Y - b_123.Y)))), Pt_$ctor_7B00E9A0_1(p_11.X + v_45.X, p_11.Y + v_45.Y))), -0.2, -0.3);
            if (matchValue_16 == null) {
                throw new Exception("second offsetPtVar returned None");
                Test_TestCaseBuilder__Zero(builder$0040_23);
            }
            else {
                Expect_isTrue(((a_125 = matchValue_16, (b_125 = pt_10, (vx_36 = (a_125.X - b_125.X), (vy_36 = (a_125.Y - b_125.Y), Math.sqrt((vx_36 * vx_36) + (vy_36 * vy_36))))))) < 1E-09)("offsetPtVar point returns to original after +d then -d (different)");
                Test_TestCaseBuilder__Zero(builder$0040_23);
            }
        }
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetPtVar negative then positive returns to original (different distances)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        let p_12, v_46, a_126, b_126, p_13, v_47, a_127, b_127, a_129, b_129, vx_37, vy_37;
        const prev_11 = Pt_$ctor_7B00E9A0(0, 0);
        const pt_11 = Pt_$ctor_7B00E9A0(1, 0);
        const next_11 = Pt_$ctor_7B00E9A0(1, 1);
        const matchValue_17 = Tria2D_offsetPtVar_5FDDACFD(pt_11, prev_11, next_11, -0.15, -0.35);
        if (matchValue_17 == null) {
            throw new Exception("first offsetPtVar returned None");
            Test_TestCaseBuilder__Zero(builder$0040_24);
        }
        else {
            const o1_3 = matchValue_17;
            const matchValue_18 = Tria2D_offsetPtVar_5FDDACFD(o1_3, (p_12 = prev_11, (v_46 = ((a_126 = o1_3, (b_126 = pt_11, Vc_$ctor_7B00E9A0(a_126.X - b_126.X, a_126.Y - b_126.Y)))), Pt_$ctor_7B00E9A0_1(p_12.X + v_46.X, p_12.Y + v_46.Y))), (p_13 = next_11, (v_47 = ((a_127 = o1_3, (b_127 = pt_11, Vc_$ctor_7B00E9A0(a_127.X - b_127.X, a_127.Y - b_127.Y)))), Pt_$ctor_7B00E9A0_1(p_13.X + v_47.X, p_13.Y + v_47.Y))), 0.15, 0.35);
            if (matchValue_18 == null) {
                throw new Exception("second offsetPtVar returned None");
                Test_TestCaseBuilder__Zero(builder$0040_24);
            }
            else {
                Expect_isTrue(((a_129 = matchValue_18, (b_129 = pt_11, (vx_37 = (a_129.X - b_129.X), (vy_37 = (a_129.Y - b_129.Y), Math.sqrt((vx_37 * vx_37) + (vy_37 * vy_37))))))) < 1E-09)("offsetPtVar point returns to original after -d then +d (different)");
                Test_TestCaseBuilder__Zero(builder$0040_24);
            }
        }
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset inward CCW triangle reduces area", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        let a_133, b_133, c_39, a_136, b_136, c_42;
        const sqrt3_1 = Math.sqrt(3);
        const a_130 = Pt_$ctor_7B00E9A0(0, 0);
        const b_130 = Pt_$ctor_7B00E9A0(2, 0);
        const c_36 = Pt_$ctor_7B00E9A0(1, sqrt3_1);
        const originalArea = Math.abs((a_133 = a_130, (b_133 = b_130, (c_39 = c_36, ((b_133.X - a_133.X) * (c_39.Y - a_133.Y)) - ((b_133.Y - a_133.Y) * (c_39.X - a_133.X)))))) * 0.5;
        const patternInput_14 = Tria2D_offset_365C4B06(a_130, b_130, c_36, 0.1);
        Expect_isTrue((Math.abs((a_136 = patternInput_14[0], (b_136 = patternInput_14[1], (c_42 = patternInput_14[2], ((b_136.X - a_136.X) * (c_42.Y - a_136.Y)) - ((b_136.Y - a_136.Y) * (c_42.X - a_136.X)))))) * 0.5) < originalArea)("inward offset reduces area");
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset outward CCW triangle increases area", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        let a_140, b_140, c_46, a_143, b_143, c_49;
        const sqrt3_2 = Math.sqrt(3);
        const a_137 = Pt_$ctor_7B00E9A0(0, 0);
        const b_137 = Pt_$ctor_7B00E9A0(2, 0);
        const c_43 = Pt_$ctor_7B00E9A0(1, sqrt3_2);
        const originalArea_1 = Math.abs((a_140 = a_137, (b_140 = b_137, (c_46 = c_43, ((b_140.X - a_140.X) * (c_46.Y - a_140.Y)) - ((b_140.Y - a_140.Y) * (c_46.X - a_140.X)))))) * 0.5;
        const patternInput_15 = Tria2D_offset_365C4B06(a_137, b_137, c_43, -0.1);
        Expect_isTrue((Math.abs((a_143 = patternInput_15[0], (b_143 = patternInput_15[1], (c_49 = patternInput_15[2], ((b_143.X - a_143.X) * (c_49.Y - a_143.Y)) - ((b_143.Y - a_143.Y) * (c_49.X - a_143.X)))))) * 0.5) > originalArea_1)("outward offset increases area");
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset inward CW triangle increases area (offset goes outward)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        let a_147, b_147, c_53, a_150, b_150, c_56;
        const a_144 = Pt_$ctor_7B00E9A0(1, 1);
        const b_144 = Pt_$ctor_7B00E9A0(1, 0);
        const c_50 = Pt_$ctor_7B00E9A0(0, 0);
        const originalArea_2 = Math.abs((a_147 = a_144, (b_147 = b_144, (c_53 = c_50, ((b_147.X - a_147.X) * (c_53.Y - a_147.Y)) - ((b_147.Y - a_147.Y) * (c_53.X - a_147.X)))))) * 0.5;
        const patternInput_16 = Tria2D_offset_365C4B06(a_144, b_144, c_50, 0.1);
        Expect_isTrue((Math.abs((a_150 = patternInput_16[0], (b_150 = patternInput_16[1], (c_56 = patternInput_16[2], ((b_150.X - a_150.X) * (c_56.Y - a_150.Y)) - ((b_150.Y - a_150.Y) * (c_56.X - a_150.X)))))) * 0.5) > originalArea_2)("positive offset on CW triangle increases area");
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset equilateral triangle with known geometry", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        const sqrt3_3 = Math.sqrt(3);
        const a_151 = Pt_$ctor_7B00E9A0(-1, -sqrt3_3 / 3);
        const b_151 = Pt_$ctor_7B00E9A0(1, -sqrt3_3 / 3);
        const c_57 = Pt_$ctor_7B00E9A0(0, (2 * sqrt3_3) / 3);
        const patternInput_17 = Tria2D_offset_365C4B06(a_151, b_151, c_57, 0.1);
        const centroid = Pt_$ctor_7B00E9A0(((a_151.X + b_151.X) + c_57.X) / 3, ((a_151.Y + b_151.Y) + c_57.Y) / 3);
        let distA;
        const a_153 = a_151;
        const b_153 = centroid;
        const vx_38 = a_153.X - b_153.X;
        const vy_38 = a_153.Y - b_153.Y;
        distA = Math.sqrt((vx_38 * vx_38) + (vy_38 * vy_38));
        let distOa;
        const a_155 = patternInput_17[0];
        const b_155 = centroid;
        const vx_39 = a_155.X - b_155.X;
        const vy_39 = a_155.Y - b_155.Y;
        distOa = Math.sqrt((vx_39 * vx_39) + (vy_39 * vy_39));
        let distB;
        const a_157 = b_151;
        const b_157 = centroid;
        const vx_40 = a_157.X - b_157.X;
        const vy_40 = a_157.Y - b_157.Y;
        distB = Math.sqrt((vx_40 * vx_40) + (vy_40 * vy_40));
        let distOb;
        const a_159 = patternInput_17[1];
        const b_159 = centroid;
        const vx_41 = a_159.X - b_159.X;
        const vy_41 = a_159.Y - b_159.Y;
        distOb = Math.sqrt((vx_41 * vx_41) + (vy_41 * vy_41));
        let distC;
        const a_161 = c_57;
        const b_161 = centroid;
        const vx_42 = a_161.X - b_161.X;
        const vy_42 = a_161.Y - b_161.Y;
        distC = Math.sqrt((vx_42 * vx_42) + (vy_42 * vy_42));
        let distOc;
        const a_163 = patternInput_17[2];
        const b_163 = centroid;
        const vx_43 = a_163.X - b_163.X;
        const vy_43 = a_163.Y - b_163.Y;
        distOc = Math.sqrt((vx_43 * vx_43) + (vy_43 * vy_43));
        Expect_isTrue(distOa < distA)("oa is closer to centroid than a");
        Expect_isTrue(distOb < distB)("ob is closer to centroid than b");
        Expect_isTrue(distOc < distC)("oc is closer to centroid than c");
        Expect_floatClose(tol, distOa, distOb, "offset triangle is equilateral (oa-ob)");
        Expect_floatClose(tol, distOb, distOc, "offset triangle is equilateral (ob-oc)");
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset preserves orientation CCW", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        const a_164 = Pt_$ctor_7B00E9A0(0, 0);
        const b_164 = Pt_$ctor_7B00E9A0(2, 0);
        const c_58 = Pt_$ctor_7B00E9A0(1, 1);
        let originalDet;
        const a_165 = a_164;
        const b_165 = b_164;
        const c_59 = c_58;
        originalDet = (((b_165.X - a_165.X) * (c_59.Y - a_165.Y)) - ((b_165.Y - a_165.Y) * (c_59.X - a_165.X)));
        const patternInput_18 = Tria2D_offset_365C4B06(a_164, b_164, c_58, 0.1);
        let offsetDet;
        const a_166 = patternInput_18[0];
        const b_166 = patternInput_18[1];
        const c_60 = patternInput_18[2];
        offsetDet = (((b_166.X - a_166.X) * (c_60.Y - a_166.Y)) - ((b_166.Y - a_166.Y) * (c_60.X - a_166.X)));
        Expect_isTrue(sign(originalDet) === sign(offsetDet))("CCW orientation preserved after offset");
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset preserves orientation CW", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        const a_167 = Pt_$ctor_7B00E9A0(1, 1);
        const b_167 = Pt_$ctor_7B00E9A0(2, 0);
        const c_61 = Pt_$ctor_7B00E9A0(0, 0);
        let originalDet_1;
        const a_168 = a_167;
        const b_168 = b_167;
        const c_62 = c_61;
        originalDet_1 = (((b_168.X - a_168.X) * (c_62.Y - a_168.Y)) - ((b_168.Y - a_168.Y) * (c_62.X - a_168.X)));
        const patternInput_19 = Tria2D_offset_365C4B06(a_167, b_167, c_61, 0.1);
        let offsetDet_1;
        const a_169 = patternInput_19[0];
        const b_169 = patternInput_19[1];
        const c_63 = patternInput_19[2];
        offsetDet_1 = (((b_169.X - a_169.X) * (c_63.Y - a_169.Y)) - ((b_169.Y - a_169.Y) * (c_63.X - a_169.X)));
        Expect_isTrue(sign(originalDet_1) === sign(offsetDet_1))("CW orientation preserved after offset");
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})()]));

