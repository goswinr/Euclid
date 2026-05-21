
import { Test_TestCaseBuilder__TryWith_Z570AC55B, Expect_isFalse, Expect_isTrue, Test_TestCaseBuilder__Zero, Expect_floatClose, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList, AccuracyModule_veryHigh } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Pnt_$ctor_Z7AD9E565 } from "./Src/Pnt.js";
import { Vec_$ctor_Z7AD9E565 } from "./Src/Vec.js";
import { Tria3D_offsetVar_Z62609A8D, Tria3D_offsetPnt_Z305FC6B8, Tria3D_offset_Z305FC6B8, Tria3D_isLinear_Z305FC6B8 } from "./Src/Tria3D.js";
import { Exception } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { isInfinity } from "./fable_modules/fable-library-js.5.0.0/Double.js";
import { printf, toText } from "./fable_modules/fable-library-js.5.0.0/String.js";
import { EuclidException } from "./Src/EuclidErrors.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_1 } from "./Src/Pnt.js";
import { failUnit3 } from "./Src/EuclidErrors.js";
import { UnitVec_$ctor_Z7AD9E565 } from "./Src/UnitVec.js";
import { ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";

export const tol = AccuracyModule_veryHigh;

export const tests = Test_testList("Tria3D ", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areaDouble and area", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let a_1, v, a_2, b_2, w, a_3, b_3, v_3, a_4, b_4, a_6, v_4, a_7, b_7, w_1, a_8, b_8, v_6, a_9, b_9, a_10, v_7, a_11, b_11, w_2, a_12, b_12, v_9, a_13, b_13, a_15, v_10, a_16, b_16, w_3, a_17, b_17, v_12, a_18, b_18;
        const a = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b = Pnt_$ctor_Z7AD9E565(4, 0, 0);
        const c = Pnt_$ctor_Z7AD9E565(0, 3, 0);
        Expect_floatClose(tol, (a_1 = a, (v = ((a_2 = b, (b_2 = a_1, Vec_$ctor_Z7AD9E565(a_2.X - b_2.X, a_2.Y - b_2.Y, a_2.Z - b_2.Z)))), (w = ((a_3 = c, (b_3 = a_1, Vec_$ctor_Z7AD9E565(a_3.X - b_3.X, a_3.Y - b_3.Y, a_3.Z - b_3.Z)))), (v_3 = ((a_4 = v, (b_4 = w, Vec_$ctor_Z7AD9E565((a_4.Y * b_4.Z) - (a_4.Z * b_4.Y), (a_4.Z * b_4.X) - (a_4.X * b_4.Z), (a_4.X * b_4.Y) - (a_4.Y * b_4.X))))), Math.sqrt(((v_3.X * v_3.X) + (v_3.Y * v_3.Y)) + (v_3.Z * v_3.Z)))))), 12, "areaDouble = 12");
        Expect_floatClose(tol, ((a_6 = a, (v_4 = ((a_7 = b, (b_7 = a_6, Vec_$ctor_Z7AD9E565(a_7.X - b_7.X, a_7.Y - b_7.Y, a_7.Z - b_7.Z)))), (w_1 = ((a_8 = c, (b_8 = a_6, Vec_$ctor_Z7AD9E565(a_8.X - b_8.X, a_8.Y - b_8.Y, a_8.Z - b_8.Z)))), (v_6 = ((a_9 = v_4, (b_9 = w_1, Vec_$ctor_Z7AD9E565((a_9.Y * b_9.Z) - (a_9.Z * b_9.Y), (a_9.Z * b_9.X) - (a_9.X * b_9.Z), (a_9.X * b_9.Y) - (a_9.Y * b_9.X))))), Math.sqrt(((v_6.X * v_6.X) + (v_6.Y * v_6.Y)) + (v_6.Z * v_6.Z))))))) * 0.5, 6, "area = 6");
        const a2 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b2 = Pnt_$ctor_Z7AD9E565(3, 0, 0);
        const c2 = Pnt_$ctor_Z7AD9E565(0, 0, 4);
        Expect_floatClose(tol, (a_10 = a2, (v_7 = ((a_11 = b2, (b_11 = a_10, Vec_$ctor_Z7AD9E565(a_11.X - b_11.X, a_11.Y - b_11.Y, a_11.Z - b_11.Z)))), (w_2 = ((a_12 = c2, (b_12 = a_10, Vec_$ctor_Z7AD9E565(a_12.X - b_12.X, a_12.Y - b_12.Y, a_12.Z - b_12.Z)))), (v_9 = ((a_13 = v_7, (b_13 = w_2, Vec_$ctor_Z7AD9E565((a_13.Y * b_13.Z) - (a_13.Z * b_13.Y), (a_13.Z * b_13.X) - (a_13.X * b_13.Z), (a_13.X * b_13.Y) - (a_13.Y * b_13.X))))), Math.sqrt(((v_9.X * v_9.X) + (v_9.Y * v_9.Y)) + (v_9.Z * v_9.Z)))))), 12, "areaDouble = 12 (XZ)");
        Expect_floatClose(tol, ((a_15 = a2, (v_10 = ((a_16 = b2, (b_16 = a_15, Vec_$ctor_Z7AD9E565(a_16.X - b_16.X, a_16.Y - b_16.Y, a_16.Z - b_16.Z)))), (w_3 = ((a_17 = c2, (b_17 = a_15, Vec_$ctor_Z7AD9E565(a_17.X - b_17.X, a_17.Y - b_17.Y, a_17.Z - b_17.Z)))), (v_12 = ((a_18 = v_10, (b_18 = w_3, Vec_$ctor_Z7AD9E565((a_18.Y * b_18.Z) - (a_18.Z * b_18.Y), (a_18.Z * b_18.X) - (a_18.X * b_18.Z), (a_18.X * b_18.Y) - (a_18.Y * b_18.X))))), Math.sqrt(((v_12.X * v_12.X) + (v_12.Y * v_12.Y)) + (v_12.Z * v_12.Z))))))) * 0.5, 6, "area = 6 (XZ)");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("area of degenerate triangles", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let a_21, v_13, a_22, b_22, w_4, a_23, b_23, v_15, a_24, b_24, a_26, v_16, a_27, b_27, w_5, a_28, b_28, v_18, a_29, b_29, a_31, v_19, a_32, b_32, w_6, a_33, b_33, v_21, a_34, b_34;
        const a_19 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_19 = Pnt_$ctor_Z7AD9E565(1, 1, 1);
        const c_7 = Pnt_$ctor_Z7AD9E565(2, 2, 2);
        Expect_floatClose(tol, ((a_21 = a_19, (v_13 = ((a_22 = b_19, (b_22 = a_21, Vec_$ctor_Z7AD9E565(a_22.X - b_22.X, a_22.Y - b_22.Y, a_22.Z - b_22.Z)))), (w_4 = ((a_23 = c_7, (b_23 = a_21, Vec_$ctor_Z7AD9E565(a_23.X - b_23.X, a_23.Y - b_23.Y, a_23.Z - b_23.Z)))), (v_15 = ((a_24 = v_13, (b_24 = w_4, Vec_$ctor_Z7AD9E565((a_24.Y * b_24.Z) - (a_24.Z * b_24.Y), (a_24.Z * b_24.X) - (a_24.X * b_24.Z), (a_24.X * b_24.Y) - (a_24.Y * b_24.X))))), Math.sqrt(((v_15.X * v_15.X) + (v_15.Y * v_15.Y)) + (v_15.Z * v_15.Z))))))) * 0.5, 0, "collinear points have zero area");
        Expect_floatClose(tol, ((a_26 = a_19, (v_16 = ((a_27 = a_19, (b_27 = a_26, Vec_$ctor_Z7AD9E565(a_27.X - b_27.X, a_27.Y - b_27.Y, a_27.Z - b_27.Z)))), (w_5 = ((a_28 = a_19, (b_28 = a_26, Vec_$ctor_Z7AD9E565(a_28.X - b_28.X, a_28.Y - b_28.Y, a_28.Z - b_28.Z)))), (v_18 = ((a_29 = v_16, (b_29 = w_5, Vec_$ctor_Z7AD9E565((a_29.Y * b_29.Z) - (a_29.Z * b_29.Y), (a_29.Z * b_29.X) - (a_29.X * b_29.Z), (a_29.X * b_29.Y) - (a_29.Y * b_29.X))))), Math.sqrt(((v_18.X * v_18.X) + (v_18.Y * v_18.Y)) + (v_18.Z * v_18.Z))))))) * 0.5, 0, "duplicate points have zero area");
        Expect_floatClose(tol, ((a_31 = a_19, (v_19 = ((a_32 = a_19, (b_32 = a_31, Vec_$ctor_Z7AD9E565(a_32.X - b_32.X, a_32.Y - b_32.Y, a_32.Z - b_32.Z)))), (w_6 = ((a_33 = b_19, (b_33 = a_31, Vec_$ctor_Z7AD9E565(a_33.X - b_33.X, a_33.Y - b_33.Y, a_33.Z - b_33.Z)))), (v_21 = ((a_34 = v_19, (b_34 = w_6, Vec_$ctor_Z7AD9E565((a_34.Y * b_34.Z) - (a_34.Z * b_34.Y), (a_34.Z * b_34.X) - (a_34.X * b_34.Z), (a_34.X * b_34.Y) - (a_34.Y * b_34.X))))), Math.sqrt(((v_21.X * v_21.X) + (v_21.Y * v_21.Y)) + (v_21.Z * v_21.Z))))))) * 0.5, 0, "two duplicate points have zero area");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("isLinearFast collinear and non-collinear", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let doubleArea, a_37, v_22, a_38, b_38, w_7, a_39, b_39, v_24, a_40, b_40, doubleArea_1, a_42, v_25, a_43, b_43, w_8, a_44, b_44, v_27, a_45, b_45, doubleArea_2, a_47, v_28, a_48, b_48, w_9, a_49, b_49, v_30, a_50, b_50;
        const a_35 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_35 = Pnt_$ctor_Z7AD9E565(1, 1, 1);
        const c_14 = Pnt_$ctor_Z7AD9E565(2, 2, 2);
        Expect_isTrue((doubleArea = ((a_37 = a_35, (v_22 = ((a_38 = b_35, (b_38 = a_37, Vec_$ctor_Z7AD9E565(a_38.X - b_38.X, a_38.Y - b_38.Y, a_38.Z - b_38.Z)))), (w_7 = ((a_39 = c_14, (b_39 = a_37, Vec_$ctor_Z7AD9E565(a_39.X - b_39.X, a_39.Y - b_39.Y, a_39.Z - b_39.Z)))), (v_24 = ((a_40 = v_22, (b_40 = w_7, Vec_$ctor_Z7AD9E565((a_40.Y * b_40.Z) - (a_40.Z * b_40.Y), (a_40.Z * b_40.X) - (a_40.X * b_40.Z), (a_40.X * b_40.Y) - (a_40.Y * b_40.X))))), Math.sqrt(((v_24.X * v_24.X) + (v_24.Y * v_24.Y)) + (v_24.Z * v_24.Z))))))), doubleArea < 1E-06))("collinear should be linear (fast)");
        const a$0027 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b$0027 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const c$0027 = Pnt_$ctor_Z7AD9E565(0, 1, 0);
        Expect_isFalse((doubleArea_1 = ((a_42 = a$0027, (v_25 = ((a_43 = b$0027, (b_43 = a_42, Vec_$ctor_Z7AD9E565(a_43.X - b_43.X, a_43.Y - b_43.Y, a_43.Z - b_43.Z)))), (w_8 = ((a_44 = c$0027, (b_44 = a_42, Vec_$ctor_Z7AD9E565(a_44.X - b_44.X, a_44.Y - b_44.Y, a_44.Z - b_44.Z)))), (v_27 = ((a_45 = v_25, (b_45 = w_8, Vec_$ctor_Z7AD9E565((a_45.Y * b_45.Z) - (a_45.Z * b_45.Y), (a_45.Z * b_45.X) - (a_45.X * b_45.Z), (a_45.X * b_45.Y) - (a_45.Y * b_45.X))))), Math.sqrt(((v_27.X * v_27.X) + (v_27.Y * v_27.Y)) + (v_27.Z * v_27.Z))))))), doubleArea_1 < 0.1))("non-collinear should not be linear for small tol (fast)");
        Expect_isTrue((doubleArea_2 = ((a_47 = a$0027, (v_28 = ((a_48 = b$0027, (b_48 = a_47, Vec_$ctor_Z7AD9E565(a_48.X - b_48.X, a_48.Y - b_48.Y, a_48.Z - b_48.Z)))), (w_9 = ((a_49 = c$0027, (b_49 = a_47, Vec_$ctor_Z7AD9E565(a_49.X - b_49.X, a_49.Y - b_49.Y, a_49.Z - b_49.Z)))), (v_30 = ((a_50 = v_28, (b_50 = w_9, Vec_$ctor_Z7AD9E565((a_50.Y * b_50.Z) - (a_50.Z * b_50.Y), (a_50.Z * b_50.X) - (a_50.X * b_50.Z), (a_50.X * b_50.Y) - (a_50.Y * b_50.X))))), Math.sqrt(((v_30.X * v_30.X) + (v_30.Y * v_30.Y)) + (v_30.Z * v_30.Z))))))), doubleArea_2 < 2))("non-collinear can be considered linear for huge tol (fast)");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("isLinear robust, including duplicate points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        const a_51 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_51 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const c_21 = Pnt_$ctor_Z7AD9E565(0, 1, 0);
        Expect_isFalse(Tria3D_isLinear_Z305FC6B8(a_51, b_51, c_21, 1E-06))("right triangle should be non-linear");
        const a2_1 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b2_1 = Pnt_$ctor_Z7AD9E565(1, 1, 1);
        const c2_1 = Pnt_$ctor_Z7AD9E565(2, 2, 2);
        Expect_isTrue(Tria3D_isLinear_Z305FC6B8(a2_1, b2_1, c2_1, 1E-06))("diagonal points should be linear");
        const a3 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b3 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const c3 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        Expect_isTrue(Tria3D_isLinear_Z305FC6B8(a3, b3, c3, 1E-06))("duplicate points treated as linear");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset distance 0 returns original points (CCW triangle in XY plane)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let a_54, b_54, x, y, z, a_56, b_56, x_1, y_1, z_1, a_58, b_58, x_2, y_2, z_2;
        const a_52 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_52 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const c_22 = Pnt_$ctor_Z7AD9E565(1, 1, 0);
        const patternInput = Tria3D_offset_Z305FC6B8(a_52, b_52, c_22, 0);
        const oc = patternInput[2];
        const ob = patternInput[1];
        const oa = patternInput[0];
        Expect_isTrue(((a_54 = oa, (b_54 = a_52, (x = (a_54.X - b_54.X), (y = (a_54.Y - b_54.Y), (z = (a_54.Z - b_54.Z), Math.sqrt(((x * x) + (y * y)) + (z * z)))))))) < 1E-09)("oa==a");
        Expect_isTrue(((a_56 = ob, (b_56 = b_52, (x_1 = (a_56.X - b_56.X), (y_1 = (a_56.Y - b_56.Y), (z_1 = (a_56.Z - b_56.Z), Math.sqrt(((x_1 * x_1) + (y_1 * y_1)) + (z_1 * z_1)))))))) < 1E-09)("ob==b");
        Expect_isTrue(((a_58 = oc, (b_58 = c_22, (x_2 = (a_58.X - b_58.X), (y_2 = (a_58.Y - b_58.Y), (z_2 = (a_58.Z - b_58.Z), Math.sqrt(((x_2 * x_2) + (y_2 * y_2)) + (z_2 * z_2)))))))) < 1E-09)("oc==c");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset distance 0 returns original points (triangle in XZ plane)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let a_61, b_61, x_3, y_3, z_3, a_63, b_63, x_4, y_4, z_4, a_65, b_65, x_5, y_5, z_5;
        const a_59 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_59 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const c_23 = Pnt_$ctor_Z7AD9E565(1, 0, 1);
        const patternInput_1 = Tria3D_offset_Z305FC6B8(a_59, b_59, c_23, 0);
        const oc_1 = patternInput_1[2];
        const ob_1 = patternInput_1[1];
        const oa_1 = patternInput_1[0];
        Expect_isTrue(((a_61 = oa_1, (b_61 = a_59, (x_3 = (a_61.X - b_61.X), (y_3 = (a_61.Y - b_61.Y), (z_3 = (a_61.Z - b_61.Z), Math.sqrt(((x_3 * x_3) + (y_3 * y_3)) + (z_3 * z_3)))))))) < 1E-09)("oa==a (XZ)");
        Expect_isTrue(((a_63 = ob_1, (b_63 = b_59, (x_4 = (a_63.X - b_63.X), (y_4 = (a_63.Y - b_63.Y), (z_4 = (a_63.Z - b_63.Z), Math.sqrt(((x_4 * x_4) + (y_4 * y_4)) + (z_4 * z_4)))))))) < 1E-09)("ob==b (XZ)");
        Expect_isTrue(((a_65 = oc_1, (b_65 = c_23, (x_5 = (a_65.X - b_65.X), (y_5 = (a_65.Y - b_65.Y), (z_5 = (a_65.Z - b_65.Z), Math.sqrt(((x_5 * x_5) + (y_5 * y_5)) + (z_5 * z_5)))))))) < 1E-09)("oc==c (XZ)");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset distance 0 returns original points (triangle in YZ plane)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        let a_68, b_68, x_6, y_6, z_6, a_70, b_70, x_7, y_7, z_7, a_72, b_72, x_8, y_8, z_8;
        const a_66 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_66 = Pnt_$ctor_Z7AD9E565(0, 1, 0);
        const c_24 = Pnt_$ctor_Z7AD9E565(0, 1, 1);
        const patternInput_2 = Tria3D_offset_Z305FC6B8(a_66, b_66, c_24, 0);
        const oc_2 = patternInput_2[2];
        const ob_2 = patternInput_2[1];
        const oa_2 = patternInput_2[0];
        Expect_isTrue(((a_68 = oa_2, (b_68 = a_66, (x_6 = (a_68.X - b_68.X), (y_6 = (a_68.Y - b_68.Y), (z_6 = (a_68.Z - b_68.Z), Math.sqrt(((x_6 * x_6) + (y_6 * y_6)) + (z_6 * z_6)))))))) < 1E-09)("oa==a (YZ)");
        Expect_isTrue(((a_70 = ob_2, (b_70 = b_66, (x_7 = (a_70.X - b_70.X), (y_7 = (a_70.Y - b_70.Y), (z_7 = (a_70.Z - b_70.Z), Math.sqrt(((x_7 * x_7) + (y_7 * y_7)) + (z_7 * z_7)))))))) < 1E-09)("ob==b (YZ)");
        Expect_isTrue(((a_72 = oc_2, (b_72 = c_24, (x_8 = (a_72.X - b_72.X), (y_8 = (a_72.Y - b_72.Y), (z_8 = (a_72.Z - b_72.Z), Math.sqrt(((x_8 * x_8) + (y_8 * y_8)) + (z_8 * z_8)))))))) < 1E-09)("oc==c (YZ)");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetPnt distance 0 returns same point (90-degree corner in XY)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let a_74, b_74, x_9, y_9, z_9;
        const prev = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const pt = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const next = Pnt_$ctor_Z7AD9E565(1, 1, 0);
        const o = Tria3D_offsetPnt_Z305FC6B8(pt, prev, next, 0);
        Expect_isTrue(((a_74 = o, (b_74 = pt, (x_9 = (a_74.X - b_74.X), (y_9 = (a_74.Y - b_74.Y), (z_9 = (a_74.Z - b_74.Z), Math.sqrt(((x_9 * x_9) + (y_9 * y_9)) + (z_9 * z_9)))))))) < 1E-09)("offsetPnt(0) == pt");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetPnt distance 0 returns same point (90-degree corner in XZ)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let a_76, b_76, x_10, y_10, z_10;
        const prev_1 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const pt_1 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const next_1 = Pnt_$ctor_Z7AD9E565(1, 0, 1);
        const o_1 = Tria3D_offsetPnt_Z305FC6B8(pt_1, prev_1, next_1, 0);
        Expect_isTrue(((a_76 = o_1, (b_76 = pt_1, (x_10 = (a_76.X - b_76.X), (y_10 = (a_76.Y - b_76.Y), (z_10 = (a_76.Z - b_76.Z), Math.sqrt(((x_10 * x_10) + (y_10 * y_10)) + (z_10 * z_10)))))))) < 1E-09)("offsetPnt(0) == pt (XZ)");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetVar with zero distances returns original point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        let a_78, b_78, x_11, y_11, z_11;
        const prev_2 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const pt_2 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const next_2 = Pnt_$ctor_Z7AD9E565(1, 1, 0);
        const matchValue = Tria3D_offsetVar_Z62609A8D(pt_2, prev_2, next_2, 0, 0);
        if (matchValue == null) {
            throw new Exception("expected point for zero distances");
            Test_TestCaseBuilder__Zero(builder$0040_9);
        }
        else {
            const o_2 = matchValue;
            Expect_isTrue(((a_78 = o_2, (b_78 = pt_2, (x_11 = (a_78.X - b_78.X), (y_11 = (a_78.Y - b_78.Y), (z_11 = (a_78.Z - b_78.Z), Math.sqrt(((x_11 * x_11) + (y_11 * y_11)) + (z_11 * z_11)))))))) < 1E-09)("offsetVar(0,0)==pt");
            Test_TestCaseBuilder__Zero(builder$0040_9);
        }
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset positive then negative returns to original (CCW triangle in XY)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let a_81, b_81, x_12, y_12, z_12, a_83, b_83, x_13, y_13, z_13, a_85, b_85, x_14, y_14, z_14;
        const a_79 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_79 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const c_25 = Pnt_$ctor_Z7AD9E565(1, 1, 0);
        const d = 0.25;
        const patternInput_3 = Tria3D_offset_Z305FC6B8(a_79, b_79, c_25, d);
        const oc1 = patternInput_3[2];
        const ob1 = patternInput_3[1];
        const oa1 = patternInput_3[0];
        const patternInput_4 = Tria3D_offset_Z305FC6B8(oa1, ob1, oc1, -d);
        const oc2 = patternInput_4[2];
        const ob2 = patternInput_4[1];
        const oa2 = patternInput_4[0];
        Expect_isTrue(((a_81 = oa2, (b_81 = a_79, (x_12 = (a_81.X - b_81.X), (y_12 = (a_81.Y - b_81.Y), (z_12 = (a_81.Z - b_81.Z), Math.sqrt(((x_12 * x_12) + (y_12 * y_12)) + (z_12 * z_12)))))))) < 1E-09)("a returns to original after +d then -d");
        Expect_isTrue(((a_83 = ob2, (b_83 = b_79, (x_13 = (a_83.X - b_83.X), (y_13 = (a_83.Y - b_83.Y), (z_13 = (a_83.Z - b_83.Z), Math.sqrt(((x_13 * x_13) + (y_13 * y_13)) + (z_13 * z_13)))))))) < 1E-09)("b returns to original after +d then -d");
        Expect_isTrue(((a_85 = oc2, (b_85 = c_25, (x_14 = (a_85.X - b_85.X), (y_14 = (a_85.Y - b_85.Y), (z_14 = (a_85.Z - b_85.Z), Math.sqrt(((x_14 * x_14) + (y_14 * y_14)) + (z_14 * z_14)))))))) < 1E-09)("c returns to original after +d then -d");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset negative then positive returns to original (CCW triangle in XY)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        let a_88, b_88, x_15, y_15, z_15, a_90, b_90, x_16, y_16, z_16, a_92, b_92, x_17, y_17, z_17;
        const a_86 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_86 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const c_26 = Pnt_$ctor_Z7AD9E565(1, 1, 0);
        const d_1 = 0.25;
        const patternInput_5 = Tria3D_offset_Z305FC6B8(a_86, b_86, c_26, -d_1);
        const oc1_1 = patternInput_5[2];
        const ob1_1 = patternInput_5[1];
        const oa1_1 = patternInput_5[0];
        const patternInput_6 = Tria3D_offset_Z305FC6B8(oa1_1, ob1_1, oc1_1, d_1);
        const oc2_1 = patternInput_6[2];
        const ob2_1 = patternInput_6[1];
        const oa2_1 = patternInput_6[0];
        Expect_isTrue(((a_88 = oa2_1, (b_88 = a_86, (x_15 = (a_88.X - b_88.X), (y_15 = (a_88.Y - b_88.Y), (z_15 = (a_88.Z - b_88.Z), Math.sqrt(((x_15 * x_15) + (y_15 * y_15)) + (z_15 * z_15)))))))) < 1E-09)("a returns to original after -d then +d");
        Expect_isTrue(((a_90 = ob2_1, (b_90 = b_86, (x_16 = (a_90.X - b_90.X), (y_16 = (a_90.Y - b_90.Y), (z_16 = (a_90.Z - b_90.Z), Math.sqrt(((x_16 * x_16) + (y_16 * y_16)) + (z_16 * z_16)))))))) < 1E-09)("b returns to original after -d then +d");
        Expect_isTrue(((a_92 = oc2_1, (b_92 = c_26, (x_17 = (a_92.X - b_92.X), (y_17 = (a_92.Y - b_92.Y), (z_17 = (a_92.Z - b_92.Z), Math.sqrt(((x_17 * x_17) + (y_17 * y_17)) + (z_17 * z_17)))))))) < 1E-09)("c returns to original after -d then +d");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset positive then negative returns to original (triangle in XZ plane)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        let a_95, b_95, x_18, y_18, z_18, a_97, b_97, x_19, y_19, z_19, a_99, b_99, x_20, y_20, z_20;
        const a_93 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_93 = Pnt_$ctor_Z7AD9E565(2, 0, 0);
        const c_27 = Pnt_$ctor_Z7AD9E565(1, 0, 2);
        const d_2 = 0.3;
        const patternInput_7 = Tria3D_offset_Z305FC6B8(a_93, b_93, c_27, d_2);
        const oc1_2 = patternInput_7[2];
        const ob1_2 = patternInput_7[1];
        const oa1_2 = patternInput_7[0];
        const patternInput_8 = Tria3D_offset_Z305FC6B8(oa1_2, ob1_2, oc1_2, -d_2);
        const oc2_2 = patternInput_8[2];
        const ob2_2 = patternInput_8[1];
        const oa2_2 = patternInput_8[0];
        Expect_isTrue(((a_95 = oa2_2, (b_95 = a_93, (x_18 = (a_95.X - b_95.X), (y_18 = (a_95.Y - b_95.Y), (z_18 = (a_95.Z - b_95.Z), Math.sqrt(((x_18 * x_18) + (y_18 * y_18)) + (z_18 * z_18)))))))) < 1E-09)("a returns to original after +d then -d (XZ)");
        Expect_isTrue(((a_97 = ob2_2, (b_97 = b_93, (x_19 = (a_97.X - b_97.X), (y_19 = (a_97.Y - b_97.Y), (z_19 = (a_97.Z - b_97.Z), Math.sqrt(((x_19 * x_19) + (y_19 * y_19)) + (z_19 * z_19)))))))) < 1E-09)("b returns to original after +d then -d (XZ)");
        Expect_isTrue(((a_99 = oc2_2, (b_99 = c_27, (x_20 = (a_99.X - b_99.X), (y_20 = (a_99.Y - b_99.Y), (z_20 = (a_99.Z - b_99.Z), Math.sqrt(((x_20 * x_20) + (y_20 * y_20)) + (z_20 * z_20)))))))) < 1E-09)("c returns to original after +d then -d (XZ)");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset negative then positive returns to original (triangle in YZ plane)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        let a_102, b_102, x_21, y_21, z_21, a_104, b_104, x_22, y_22, z_22, a_106, b_106, x_23, y_23, z_23;
        const a_100 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_100 = Pnt_$ctor_Z7AD9E565(0, 2, 0);
        const c_28 = Pnt_$ctor_Z7AD9E565(0, 1, 2);
        const d_3 = 0.2;
        const patternInput_9 = Tria3D_offset_Z305FC6B8(a_100, b_100, c_28, -d_3);
        const oc1_3 = patternInput_9[2];
        const ob1_3 = patternInput_9[1];
        const oa1_3 = patternInput_9[0];
        const patternInput_10 = Tria3D_offset_Z305FC6B8(oa1_3, ob1_3, oc1_3, d_3);
        const oc2_3 = patternInput_10[2];
        const ob2_3 = patternInput_10[1];
        const oa2_3 = patternInput_10[0];
        Expect_isTrue(((a_102 = oa2_3, (b_102 = a_100, (x_21 = (a_102.X - b_102.X), (y_21 = (a_102.Y - b_102.Y), (z_21 = (a_102.Z - b_102.Z), Math.sqrt(((x_21 * x_21) + (y_21 * y_21)) + (z_21 * z_21)))))))) < 1E-09)("a returns to original after -d then +d (YZ)");
        Expect_isTrue(((a_104 = ob2_3, (b_104 = b_100, (x_22 = (a_104.X - b_104.X), (y_22 = (a_104.Y - b_104.Y), (z_22 = (a_104.Z - b_104.Z), Math.sqrt(((x_22 * x_22) + (y_22 * y_22)) + (z_22 * z_22)))))))) < 1E-09)("b returns to original after -d then +d (YZ)");
        Expect_isTrue(((a_106 = oc2_3, (b_106 = c_28, (x_23 = (a_106.X - b_106.X), (y_23 = (a_106.Y - b_106.Y), (z_23 = (a_106.Z - b_106.Z), Math.sqrt(((x_23 * x_23) + (y_23 * y_23)) + (z_23 * z_23)))))))) < 1E-09)("c returns to original after -d then +d (YZ)");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset positive then negative returns to original (arbitrary orientation)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let a_109, b_109, x_24, y_24, z_24, a_111, b_111, x_25, y_25, z_25, a_113, b_113, x_26, y_26, z_26;
        const a_107 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        const b_107 = Pnt_$ctor_Z7AD9E565(4, 1, 2);
        const c_29 = Pnt_$ctor_Z7AD9E565(2, 5, 1);
        const d_4 = 0.15;
        const patternInput_11 = Tria3D_offset_Z305FC6B8(a_107, b_107, c_29, d_4);
        const oc1_4 = patternInput_11[2];
        const ob1_4 = patternInput_11[1];
        const oa1_4 = patternInput_11[0];
        const patternInput_12 = Tria3D_offset_Z305FC6B8(oa1_4, ob1_4, oc1_4, -d_4);
        const oc2_4 = patternInput_12[2];
        const ob2_4 = patternInput_12[1];
        const oa2_4 = patternInput_12[0];
        Expect_isTrue(((a_109 = oa2_4, (b_109 = a_107, (x_24 = (a_109.X - b_109.X), (y_24 = (a_109.Y - b_109.Y), (z_24 = (a_109.Z - b_109.Z), Math.sqrt(((x_24 * x_24) + (y_24 * y_24)) + (z_24 * z_24)))))))) < 1E-09)("a returns to original after +d then -d (arbitrary)");
        Expect_isTrue(((a_111 = ob2_4, (b_111 = b_107, (x_25 = (a_111.X - b_111.X), (y_25 = (a_111.Y - b_111.Y), (z_25 = (a_111.Z - b_111.Z), Math.sqrt(((x_25 * x_25) + (y_25 * y_25)) + (z_25 * z_25)))))))) < 1E-09)("b returns to original after +d then -d (arbitrary)");
        Expect_isTrue(((a_113 = oc2_4, (b_113 = c_29, (x_26 = (a_113.X - b_113.X), (y_26 = (a_113.Y - b_113.Y), (z_26 = (a_113.Z - b_113.Z), Math.sqrt(((x_26 * x_26) + (y_26 * y_26)) + (z_26 * z_26)))))))) < 1E-09)("c returns to original after +d then -d (arbitrary)");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset with larger distances maintains roundtrip property", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        let a_116, b_116, x_27, y_27, z_27, a_118, b_118, x_28, y_28, z_28, a_120, b_120, x_29, y_29, z_29;
        const a_114 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_114 = Pnt_$ctor_Z7AD9E565(3, 0, 0);
        const c_30 = Pnt_$ctor_Z7AD9E565(1.5, 3, 0);
        const d_5 = 0.5;
        const patternInput_13 = Tria3D_offset_Z305FC6B8(a_114, b_114, c_30, d_5);
        const oc1_5 = patternInput_13[2];
        const ob1_5 = patternInput_13[1];
        const oa1_5 = patternInput_13[0];
        const patternInput_14 = Tria3D_offset_Z305FC6B8(oa1_5, ob1_5, oc1_5, -d_5);
        const oc2_5 = patternInput_14[2];
        const ob2_5 = patternInput_14[1];
        const oa2_5 = patternInput_14[0];
        Expect_isTrue(((a_116 = oa2_5, (b_116 = a_114, (x_27 = (a_116.X - b_116.X), (y_27 = (a_116.Y - b_116.Y), (z_27 = (a_116.Z - b_116.Z), Math.sqrt(((x_27 * x_27) + (y_27 * y_27)) + (z_27 * z_27)))))))) < 1E-09)("a returns to original after +d then -d (medium offset)");
        Expect_isTrue(((a_118 = ob2_5, (b_118 = b_114, (x_28 = (a_118.X - b_118.X), (y_28 = (a_118.Y - b_118.Y), (z_28 = (a_118.Z - b_118.Z), Math.sqrt(((x_28 * x_28) + (y_28 * y_28)) + (z_28 * z_28)))))))) < 1E-09)("b returns to original after +d then -d (medium offset)");
        Expect_isTrue(((a_120 = oc2_5, (b_120 = c_30, (x_29 = (a_120.X - b_120.X), (y_29 = (a_120.Y - b_120.Y), (z_29 = (a_120.Z - b_120.Z), Math.sqrt(((x_29 * x_29) + (y_29 * y_29)) + (z_29 * z_29)))))))) < 1E-09)("c returns to original after +d then -d (medium offset)");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetVar with different distances (90-degree corner in XY)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        const prev_3 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const pt_3 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const next_3 = Pnt_$ctor_Z7AD9E565(1, 1, 0);
        const dPrev = 0.2;
        const dNext = 0.7;
        const matchValue_1 = Tria3D_offsetVar_Z62609A8D(pt_3, prev_3, next_3, dPrev, dNext);
        if (matchValue_1 == null) {
            throw new Exception("offsetVar returned None unexpectedly");
            Test_TestCaseBuilder__Zero(builder$0040_16);
        }
        else {
            const o_3 = matchValue_1;
            Expect_isTrue(!((isInfinity(o_3.X) ? true : isInfinity(o_3.Y)) ? true : isInfinity(o_3.Z)))(toText(printf("offsetVar with different distances result: %A"))(o_3));
            Test_TestCaseBuilder__Zero(builder$0040_16);
        }
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetVar handles collinear input gracefully", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        const prev_4 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const pt_4 = Pnt_$ctor_Z7AD9E565(1, 1, 1);
        const next_4 = Pnt_$ctor_Z7AD9E565(2, 2, 2);
        const matchValue_2 = Tria3D_offsetVar_Z62609A8D(pt_4, prev_4, next_4, 0.5, 0.5);
        if (matchValue_2 == null) {
            Expect_isTrue(true)("offsetVar correctly returns None for collinear points");
            Test_TestCaseBuilder__Zero(builder$0040_17);
        }
        else {
            throw new Exception("expected None for collinear points");
            Test_TestCaseBuilder__Zero(builder$0040_17);
        }
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetPnt handles U-turn gracefully (in XY plane)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        const prev_5 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const pt_5 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const next_5 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        Test_TestCaseBuilder__TryWith_Z570AC55B(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
            const _o = Tria3D_offsetPnt_Z305FC6B8(pt_5, prev_5, next_5, 1);
            throw new Exception("Expected exception for collinear/duplicate points");
            Test_TestCaseBuilder__Zero(builder$0040_18);
        }), (_arg) => {
            if (_arg instanceof EuclidException) {
                Expect_isTrue(true)("offsetPnt correctly throws exception for collinear points");
                Test_TestCaseBuilder__Zero(builder$0040_18);
            }
            else {
                throw new Exception("Expected EuclidException");
                Test_TestCaseBuilder__Zero(builder$0040_18);
            }
        });
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("area calculation matches expected for known triangles", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        let a_123, v_31, a_124, b_124, w_10, a_125, b_125, v_33, a_126, b_126, a_128, v_34, a_129, b_129, w_11, a_130, b_130, v_36, a_131, b_131;
        const a_121 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_121 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const c_31 = Pnt_$ctor_Z7AD9E565(0, 1, 0);
        Expect_floatClose(tol, ((a_123 = a_121, (v_31 = ((a_124 = b_121, (b_124 = a_123, Vec_$ctor_Z7AD9E565(a_124.X - b_124.X, a_124.Y - b_124.Y, a_124.Z - b_124.Z)))), (w_10 = ((a_125 = c_31, (b_125 = a_123, Vec_$ctor_Z7AD9E565(a_125.X - b_125.X, a_125.Y - b_125.Y, a_125.Z - b_125.Z)))), (v_33 = ((a_126 = v_31, (b_126 = w_10, Vec_$ctor_Z7AD9E565((a_126.Y * b_126.Z) - (a_126.Z * b_126.Y), (a_126.Z * b_126.X) - (a_126.X * b_126.Z), (a_126.X * b_126.Y) - (a_126.Y * b_126.X))))), Math.sqrt(((v_33.X * v_33.X) + (v_33.Y * v_33.Y)) + (v_33.Z * v_33.Z))))))) * 0.5, 0.5, "unit right triangle area = 0.5");
        const sqrt3 = Math.sqrt(3);
        const a2_2 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b2_2 = Pnt_$ctor_Z7AD9E565(2, 0, 0);
        const c2_2 = Pnt_$ctor_Z7AD9E565(1, sqrt3, 0);
        Expect_floatClose(tol, ((a_128 = a2_2, (v_34 = ((a_129 = b2_2, (b_129 = a_128, Vec_$ctor_Z7AD9E565(a_129.X - b_129.X, a_129.Y - b_129.Y, a_129.Z - b_129.Z)))), (w_11 = ((a_130 = c2_2, (b_130 = a_128, Vec_$ctor_Z7AD9E565(a_130.X - b_130.X, a_130.Y - b_130.Y, a_130.Z - b_130.Z)))), (v_36 = ((a_131 = v_34, (b_131 = w_11, Vec_$ctor_Z7AD9E565((a_131.Y * b_131.Z) - (a_131.Z * b_131.Y), (a_131.Z * b_131.X) - (a_131.X * b_131.Z), (a_131.X * b_131.Y) - (a_131.Y * b_131.X))))), Math.sqrt(((v_36.X * v_36.X) + (v_36.Y * v_36.Y)) + (v_36.Z * v_36.Z))))))) * 0.5, sqrt3, "equilateral triangle area = sqrt(3)");
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetVar positive then negative returns to original (equal distances)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        let a_134, b_134, x_30, y_30, z_30;
        const prev_6 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const pt_6 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const next_6 = Pnt_$ctor_Z7AD9E565(1, 1, 0);
        const d_6 = 0.25;
        const matchValue_3 = Tria3D_offsetVar_Z62609A8D(pt_6, prev_6, next_6, d_6, d_6);
        if (matchValue_3 == null) {
            throw new Exception("first offsetVar returned None");
            Test_TestCaseBuilder__Zero(builder$0040_20);
        }
        else {
            const o1 = matchValue_3;
            let displacement;
            const a_132 = o1;
            const b_132 = pt_6;
            displacement = Vec_$ctor_Z7AD9E565(a_132.X - b_132.X, a_132.Y - b_132.Y, a_132.Z - b_132.Z);
            let newPrev;
            const p = prev_6;
            const v_37 = displacement;
            newPrev = Pnt_$ctor_Z7AD9E565_1(p.X + v_37.X, p.Y + v_37.Y, p.Z + v_37.Z);
            let newNext;
            const p_1 = next_6;
            const v_38 = displacement;
            newNext = Pnt_$ctor_Z7AD9E565_1(p_1.X + v_38.X, p_1.Y + v_38.Y, p_1.Z + v_38.Z);
            const matchValue_4 = Tria3D_offsetVar_Z62609A8D(o1, newPrev, newNext, -d_6, -d_6);
            if (matchValue_4 == null) {
                throw new Exception("second offsetVar returned None");
                Test_TestCaseBuilder__Zero(builder$0040_20);
            }
            else {
                const o2 = matchValue_4;
                Expect_isTrue(((a_134 = o2, (b_134 = pt_6, (x_30 = (a_134.X - b_134.X), (y_30 = (a_134.Y - b_134.Y), (z_30 = (a_134.Z - b_134.Z), Math.sqrt(((x_30 * x_30) + (y_30 * y_30)) + (z_30 * z_30)))))))) < 1E-09)("offsetVar point returns to original after +d then -d (equal)");
                Test_TestCaseBuilder__Zero(builder$0040_20);
            }
        }
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetVar negative then positive returns to original (equal distances)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        let a_137, b_137, x_31, y_31, z_31;
        const prev_7 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const pt_7 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const next_7 = Pnt_$ctor_Z7AD9E565(1, 1, 0);
        const d_7 = 0.25;
        const matchValue_5 = Tria3D_offsetVar_Z62609A8D(pt_7, prev_7, next_7, -d_7, -d_7);
        if (matchValue_5 == null) {
            throw new Exception("first offsetVar returned None");
            Test_TestCaseBuilder__Zero(builder$0040_21);
        }
        else {
            const o1_1 = matchValue_5;
            let displacement_1;
            const a_135 = o1_1;
            const b_135 = pt_7;
            displacement_1 = Vec_$ctor_Z7AD9E565(a_135.X - b_135.X, a_135.Y - b_135.Y, a_135.Z - b_135.Z);
            let newPrev_1;
            const p_2 = prev_7;
            const v_39 = displacement_1;
            newPrev_1 = Pnt_$ctor_Z7AD9E565_1(p_2.X + v_39.X, p_2.Y + v_39.Y, p_2.Z + v_39.Z);
            let newNext_1;
            const p_3 = next_7;
            const v_40 = displacement_1;
            newNext_1 = Pnt_$ctor_Z7AD9E565_1(p_3.X + v_40.X, p_3.Y + v_40.Y, p_3.Z + v_40.Z);
            const matchValue_6 = Tria3D_offsetVar_Z62609A8D(o1_1, newPrev_1, newNext_1, d_7, d_7);
            if (matchValue_6 == null) {
                throw new Exception("second offsetVar returned None");
                Test_TestCaseBuilder__Zero(builder$0040_21);
            }
            else {
                const o2_1 = matchValue_6;
                Expect_isTrue(((a_137 = o2_1, (b_137 = pt_7, (x_31 = (a_137.X - b_137.X), (y_31 = (a_137.Y - b_137.Y), (z_31 = (a_137.Z - b_137.Z), Math.sqrt(((x_31 * x_31) + (y_31 * y_31)) + (z_31 * z_31)))))))) < 1E-09)("offsetVar point returns to original after -d then +d (equal)");
                Test_TestCaseBuilder__Zero(builder$0040_21);
            }
        }
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetVar positive then negative returns to original (different distances)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        let a_140, b_140, x_32, y_32, z_32;
        const prev_8 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const pt_8 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const next_8 = Pnt_$ctor_Z7AD9E565(1, 1, 0);
        const dPrev_1 = 0.2;
        const dNext_1 = 0.3;
        const matchValue_7 = Tria3D_offsetVar_Z62609A8D(pt_8, prev_8, next_8, dPrev_1, dNext_1);
        if (matchValue_7 == null) {
            throw new Exception("first offsetVar returned None");
            Test_TestCaseBuilder__Zero(builder$0040_22);
        }
        else {
            const o1_2 = matchValue_7;
            let displacement_2;
            const a_138 = o1_2;
            const b_138 = pt_8;
            displacement_2 = Vec_$ctor_Z7AD9E565(a_138.X - b_138.X, a_138.Y - b_138.Y, a_138.Z - b_138.Z);
            let newPrev_2;
            const p_4 = prev_8;
            const v_41 = displacement_2;
            newPrev_2 = Pnt_$ctor_Z7AD9E565_1(p_4.X + v_41.X, p_4.Y + v_41.Y, p_4.Z + v_41.Z);
            let newNext_2;
            const p_5 = next_8;
            const v_42 = displacement_2;
            newNext_2 = Pnt_$ctor_Z7AD9E565_1(p_5.X + v_42.X, p_5.Y + v_42.Y, p_5.Z + v_42.Z);
            const matchValue_8 = Tria3D_offsetVar_Z62609A8D(o1_2, newPrev_2, newNext_2, -dPrev_1, -dNext_1);
            if (matchValue_8 == null) {
                throw new Exception("second offsetVar returned None");
                Test_TestCaseBuilder__Zero(builder$0040_22);
            }
            else {
                const o2_2 = matchValue_8;
                Expect_isTrue(((a_140 = o2_2, (b_140 = pt_8, (x_32 = (a_140.X - b_140.X), (y_32 = (a_140.Y - b_140.Y), (z_32 = (a_140.Z - b_140.Z), Math.sqrt(((x_32 * x_32) + (y_32 * y_32)) + (z_32 * z_32)))))))) < 1E-09)("offsetVar point returns to original after +d then -d (different)");
                Test_TestCaseBuilder__Zero(builder$0040_22);
            }
        }
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetVar negative then positive returns to original (different distances in XZ)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        let a_143, b_143, x_33, y_33, z_33;
        const prev_9 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const pt_9 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const next_9 = Pnt_$ctor_Z7AD9E565(1, 0, 1);
        const dPrev_2 = 0.15;
        const dNext_2 = 0.35;
        const matchValue_9 = Tria3D_offsetVar_Z62609A8D(pt_9, prev_9, next_9, -dPrev_2, -dNext_2);
        if (matchValue_9 == null) {
            throw new Exception("first offsetVar returned None");
            Test_TestCaseBuilder__Zero(builder$0040_23);
        }
        else {
            const o1_3 = matchValue_9;
            let displacement_3;
            const a_141 = o1_3;
            const b_141 = pt_9;
            displacement_3 = Vec_$ctor_Z7AD9E565(a_141.X - b_141.X, a_141.Y - b_141.Y, a_141.Z - b_141.Z);
            let newPrev_3;
            const p_6 = prev_9;
            const v_43 = displacement_3;
            newPrev_3 = Pnt_$ctor_Z7AD9E565_1(p_6.X + v_43.X, p_6.Y + v_43.Y, p_6.Z + v_43.Z);
            let newNext_3;
            const p_7 = next_9;
            const v_44 = displacement_3;
            newNext_3 = Pnt_$ctor_Z7AD9E565_1(p_7.X + v_44.X, p_7.Y + v_44.Y, p_7.Z + v_44.Z);
            const matchValue_10 = Tria3D_offsetVar_Z62609A8D(o1_3, newPrev_3, newNext_3, dPrev_2, dNext_2);
            if (matchValue_10 == null) {
                throw new Exception("second offsetVar returned None");
                Test_TestCaseBuilder__Zero(builder$0040_23);
            }
            else {
                const o2_3 = matchValue_10;
                Expect_isTrue(((a_143 = o2_3, (b_143 = pt_9, (x_33 = (a_143.X - b_143.X), (y_33 = (a_143.Y - b_143.Y), (z_33 = (a_143.Z - b_143.Z), Math.sqrt(((x_33 * x_33) + (y_33 * y_33)) + (z_33 * z_33)))))))) < 1E-09)("offsetVar point returns to original after -d then +d (different XZ)");
                Test_TestCaseBuilder__Zero(builder$0040_23);
            }
        }
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset inward reduces area (equilateral triangle in XY plane)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        let a_146, v_45, a_147, b_147, w_12, a_148, b_148, v_47, a_149, b_149, a_151, v_48, a_152, b_152, w_13, a_153, b_153, v_50, a_154, b_154;
        const sqrt3_1 = Math.sqrt(3);
        const a_144 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_144 = Pnt_$ctor_Z7AD9E565(2, 0, 0);
        const c_36 = Pnt_$ctor_Z7AD9E565(1, sqrt3_1, 0);
        const originalArea = ((a_146 = a_144, (v_45 = ((a_147 = b_144, (b_147 = a_146, Vec_$ctor_Z7AD9E565(a_147.X - b_147.X, a_147.Y - b_147.Y, a_147.Z - b_147.Z)))), (w_12 = ((a_148 = c_36, (b_148 = a_146, Vec_$ctor_Z7AD9E565(a_148.X - b_148.X, a_148.Y - b_148.Y, a_148.Z - b_148.Z)))), (v_47 = ((a_149 = v_45, (b_149 = w_12, Vec_$ctor_Z7AD9E565((a_149.Y * b_149.Z) - (a_149.Z * b_149.Y), (a_149.Z * b_149.X) - (a_149.X * b_149.Z), (a_149.X * b_149.Y) - (a_149.Y * b_149.X))))), Math.sqrt(((v_47.X * v_47.X) + (v_47.Y * v_47.Y)) + (v_47.Z * v_47.Z))))))) * 0.5;
        const d_8 = 0.1;
        const patternInput_15 = Tria3D_offset_Z305FC6B8(a_144, b_144, c_36, d_8);
        const oc_3 = patternInput_15[2];
        const ob_3 = patternInput_15[1];
        const oa_3 = patternInput_15[0];
        const offsetArea = ((a_151 = oa_3, (v_48 = ((a_152 = ob_3, (b_152 = a_151, Vec_$ctor_Z7AD9E565(a_152.X - b_152.X, a_152.Y - b_152.Y, a_152.Z - b_152.Z)))), (w_13 = ((a_153 = oc_3, (b_153 = a_151, Vec_$ctor_Z7AD9E565(a_153.X - b_153.X, a_153.Y - b_153.Y, a_153.Z - b_153.Z)))), (v_50 = ((a_154 = v_48, (b_154 = w_13, Vec_$ctor_Z7AD9E565((a_154.Y * b_154.Z) - (a_154.Z * b_154.Y), (a_154.Z * b_154.X) - (a_154.X * b_154.Z), (a_154.X * b_154.Y) - (a_154.Y * b_154.X))))), Math.sqrt(((v_50.X * v_50.X) + (v_50.Y * v_50.Y)) + (v_50.Z * v_50.Z))))))) * 0.5;
        Expect_isTrue(offsetArea < originalArea)("inward offset reduces area");
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset outward increases area (equilateral triangle in XY plane)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        let a_157, v_51, a_158, b_158, w_14, a_159, b_159, v_53, a_160, b_160, a_162, v_54, a_163, b_163, w_15, a_164, b_164, v_56, a_165, b_165;
        const sqrt3_2 = Math.sqrt(3);
        const a_155 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_155 = Pnt_$ctor_Z7AD9E565(2, 0, 0);
        const c_41 = Pnt_$ctor_Z7AD9E565(1, sqrt3_2, 0);
        const originalArea_1 = ((a_157 = a_155, (v_51 = ((a_158 = b_155, (b_158 = a_157, Vec_$ctor_Z7AD9E565(a_158.X - b_158.X, a_158.Y - b_158.Y, a_158.Z - b_158.Z)))), (w_14 = ((a_159 = c_41, (b_159 = a_157, Vec_$ctor_Z7AD9E565(a_159.X - b_159.X, a_159.Y - b_159.Y, a_159.Z - b_159.Z)))), (v_53 = ((a_160 = v_51, (b_160 = w_14, Vec_$ctor_Z7AD9E565((a_160.Y * b_160.Z) - (a_160.Z * b_160.Y), (a_160.Z * b_160.X) - (a_160.X * b_160.Z), (a_160.X * b_160.Y) - (a_160.Y * b_160.X))))), Math.sqrt(((v_53.X * v_53.X) + (v_53.Y * v_53.Y)) + (v_53.Z * v_53.Z))))))) * 0.5;
        const d_9 = -0.1;
        const patternInput_16 = Tria3D_offset_Z305FC6B8(a_155, b_155, c_41, d_9);
        const oc_4 = patternInput_16[2];
        const ob_4 = patternInput_16[1];
        const oa_4 = patternInput_16[0];
        const offsetArea_1 = ((a_162 = oa_4, (v_54 = ((a_163 = ob_4, (b_163 = a_162, Vec_$ctor_Z7AD9E565(a_163.X - b_163.X, a_163.Y - b_163.Y, a_163.Z - b_163.Z)))), (w_15 = ((a_164 = oc_4, (b_164 = a_162, Vec_$ctor_Z7AD9E565(a_164.X - b_164.X, a_164.Y - b_164.Y, a_164.Z - b_164.Z)))), (v_56 = ((a_165 = v_54, (b_165 = w_15, Vec_$ctor_Z7AD9E565((a_165.Y * b_165.Z) - (a_165.Z * b_165.Y), (a_165.Z * b_165.X) - (a_165.X * b_165.Z), (a_165.X * b_165.Y) - (a_165.Y * b_165.X))))), Math.sqrt(((v_56.X * v_56.X) + (v_56.Y * v_56.Y)) + (v_56.Z * v_56.Z))))))) * 0.5;
        Expect_isTrue(offsetArea_1 > originalArea_1)("outward offset increases area");
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset inward reduces area (triangle in XZ plane)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        let a_168, v_57, a_169, b_169, w_16, a_170, b_170, v_59, a_171, b_171, a_173, v_60, a_174, b_174, w_17, a_175, b_175, v_62, a_176, b_176;
        const a_166 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_166 = Pnt_$ctor_Z7AD9E565(2, 0, 0);
        const c_46 = Pnt_$ctor_Z7AD9E565(1, 0, 2);
        const originalArea_2 = ((a_168 = a_166, (v_57 = ((a_169 = b_166, (b_169 = a_168, Vec_$ctor_Z7AD9E565(a_169.X - b_169.X, a_169.Y - b_169.Y, a_169.Z - b_169.Z)))), (w_16 = ((a_170 = c_46, (b_170 = a_168, Vec_$ctor_Z7AD9E565(a_170.X - b_170.X, a_170.Y - b_170.Y, a_170.Z - b_170.Z)))), (v_59 = ((a_171 = v_57, (b_171 = w_16, Vec_$ctor_Z7AD9E565((a_171.Y * b_171.Z) - (a_171.Z * b_171.Y), (a_171.Z * b_171.X) - (a_171.X * b_171.Z), (a_171.X * b_171.Y) - (a_171.Y * b_171.X))))), Math.sqrt(((v_59.X * v_59.X) + (v_59.Y * v_59.Y)) + (v_59.Z * v_59.Z))))))) * 0.5;
        const d_10 = 0.1;
        const patternInput_17 = Tria3D_offset_Z305FC6B8(a_166, b_166, c_46, d_10);
        const oc_5 = patternInput_17[2];
        const ob_5 = patternInput_17[1];
        const oa_5 = patternInput_17[0];
        const offsetArea_2 = ((a_173 = oa_5, (v_60 = ((a_174 = ob_5, (b_174 = a_173, Vec_$ctor_Z7AD9E565(a_174.X - b_174.X, a_174.Y - b_174.Y, a_174.Z - b_174.Z)))), (w_17 = ((a_175 = oc_5, (b_175 = a_173, Vec_$ctor_Z7AD9E565(a_175.X - b_175.X, a_175.Y - b_175.Y, a_175.Z - b_175.Z)))), (v_62 = ((a_176 = v_60, (b_176 = w_17, Vec_$ctor_Z7AD9E565((a_176.Y * b_176.Z) - (a_176.Z * b_176.Y), (a_176.Z * b_176.X) - (a_176.X * b_176.Z), (a_176.X * b_176.Y) - (a_176.Y * b_176.X))))), Math.sqrt(((v_62.X * v_62.X) + (v_62.Y * v_62.Y)) + (v_62.Z * v_62.Z))))))) * 0.5;
        Expect_isTrue(offsetArea_2 < originalArea_2)("inward offset reduces area (XZ)");
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset outward increases area (triangle in YZ plane)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        let a_179, v_63, a_180, b_180, w_18, a_181, b_181, v_65, a_182, b_182, a_184, v_66, a_185, b_185, w_19, a_186, b_186, v_68, a_187, b_187, a_189, v_69, a_190, b_190, w_20, a_191, b_191, v_71, a_192, b_192, a_194, v_72, a_195, b_195, w_21, a_196, b_196, v_74, a_197, b_197;
        const a_177 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_177 = Pnt_$ctor_Z7AD9E565(0, 2, 0);
        const c_51 = Pnt_$ctor_Z7AD9E565(0, 1, 2);
        const originalArea_3 = ((a_179 = a_177, (v_63 = ((a_180 = b_177, (b_180 = a_179, Vec_$ctor_Z7AD9E565(a_180.X - b_180.X, a_180.Y - b_180.Y, a_180.Z - b_180.Z)))), (w_18 = ((a_181 = c_51, (b_181 = a_179, Vec_$ctor_Z7AD9E565(a_181.X - b_181.X, a_181.Y - b_181.Y, a_181.Z - b_181.Z)))), (v_65 = ((a_182 = v_63, (b_182 = w_18, Vec_$ctor_Z7AD9E565((a_182.Y * b_182.Z) - (a_182.Z * b_182.Y), (a_182.Z * b_182.X) - (a_182.X * b_182.Z), (a_182.X * b_182.Y) - (a_182.Y * b_182.X))))), Math.sqrt(((v_65.X * v_65.X) + (v_65.Y * v_65.Y)) + (v_65.Z * v_65.Z))))))) * 0.5;
        const d_11 = -0.1;
        const patternInput_18 = Tria3D_offset_Z305FC6B8(a_177, b_177, c_51, d_11);
        const oc_6 = patternInput_18[2];
        const ob_6 = patternInput_18[1];
        const oa_6 = patternInput_18[0];
        const offsetArea_3 = ((a_184 = oa_6, (v_66 = ((a_185 = ob_6, (b_185 = a_184, Vec_$ctor_Z7AD9E565(a_185.X - b_185.X, a_185.Y - b_185.Y, a_185.Z - b_185.Z)))), (w_19 = ((a_186 = oc_6, (b_186 = a_184, Vec_$ctor_Z7AD9E565(a_186.X - b_186.X, a_186.Y - b_186.Y, a_186.Z - b_186.Z)))), (v_68 = ((a_187 = v_66, (b_187 = w_19, Vec_$ctor_Z7AD9E565((a_187.Y * b_187.Z) - (a_187.Z * b_187.Y), (a_187.Z * b_187.X) - (a_187.X * b_187.Z), (a_187.X * b_187.Y) - (a_187.Y * b_187.X))))), Math.sqrt(((v_68.X * v_68.X) + (v_68.Y * v_68.Y)) + (v_68.Z * v_68.Z))))))) * 0.5;
        Expect_isTrue(offsetArea_3 > originalArea_3)("outward offset increases area (YZ)");
        const originalArea_4 = ((a_189 = a_177, (v_69 = ((a_190 = c_51, (b_190 = a_189, Vec_$ctor_Z7AD9E565(a_190.X - b_190.X, a_190.Y - b_190.Y, a_190.Z - b_190.Z)))), (w_20 = ((a_191 = b_177, (b_191 = a_189, Vec_$ctor_Z7AD9E565(a_191.X - b_191.X, a_191.Y - b_191.Y, a_191.Z - b_191.Z)))), (v_71 = ((a_192 = v_69, (b_192 = w_20, Vec_$ctor_Z7AD9E565((a_192.Y * b_192.Z) - (a_192.Z * b_192.Y), (a_192.Z * b_192.X) - (a_192.X * b_192.Z), (a_192.X * b_192.Y) - (a_192.Y * b_192.X))))), Math.sqrt(((v_71.X * v_71.X) + (v_71.Y * v_71.Y)) + (v_71.Z * v_71.Z))))))) * 0.5;
        const d_12 = -0.1;
        const patternInput_19 = Tria3D_offset_Z305FC6B8(a_177, c_51, b_177, d_12);
        const oc_7 = patternInput_19[2];
        const ob_7 = patternInput_19[1];
        const oa_7 = patternInput_19[0];
        const offsetArea_4 = ((a_194 = oa_7, (v_72 = ((a_195 = ob_7, (b_195 = a_194, Vec_$ctor_Z7AD9E565(a_195.X - b_195.X, a_195.Y - b_195.Y, a_195.Z - b_195.Z)))), (w_21 = ((a_196 = oc_7, (b_196 = a_194, Vec_$ctor_Z7AD9E565(a_196.X - b_196.X, a_196.Y - b_196.Y, a_196.Z - b_196.Z)))), (v_74 = ((a_197 = v_72, (b_197 = w_21, Vec_$ctor_Z7AD9E565((a_197.Y * b_197.Z) - (a_197.Z * b_197.Y), (a_197.Z * b_197.X) - (a_197.X * b_197.Z), (a_197.X * b_197.Y) - (a_197.Y * b_197.X))))), Math.sqrt(((v_74.X * v_74.X) + (v_74.Y * v_74.Y)) + (v_74.Z * v_74.Z))))))) * 0.5;
        Expect_isTrue(offsetArea_4 > originalArea_4)("outward offset increases area (YZ)");
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset equilateral triangle preserves symmetry", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        const sqrt3_3 = Math.sqrt(3);
        const a_198 = Pnt_$ctor_Z7AD9E565(-1, -sqrt3_3 / 3, 0);
        const b_198 = Pnt_$ctor_Z7AD9E565(1, -sqrt3_3 / 3, 0);
        const c_60 = Pnt_$ctor_Z7AD9E565(0, (2 * sqrt3_3) / 3, 0);
        const d_13 = 0.1;
        const patternInput_20 = Tria3D_offset_Z305FC6B8(a_198, b_198, c_60, d_13);
        const oc_8 = patternInput_20[2];
        const ob_8 = patternInput_20[1];
        const oa_8 = patternInput_20[0];
        const centroid = Pnt_$ctor_Z7AD9E565(((a_198.X + b_198.X) + c_60.X) / 3, ((a_198.Y + b_198.Y) + c_60.Y) / 3, ((a_198.Z + b_198.Z) + c_60.Z) / 3);
        let distA;
        const a_200 = a_198;
        const b_200 = centroid;
        const x_34 = a_200.X - b_200.X;
        const y_34 = a_200.Y - b_200.Y;
        const z_34 = a_200.Z - b_200.Z;
        distA = Math.sqrt(((x_34 * x_34) + (y_34 * y_34)) + (z_34 * z_34));
        let distOa;
        const a_202 = oa_8;
        const b_202 = centroid;
        const x_35 = a_202.X - b_202.X;
        const y_35 = a_202.Y - b_202.Y;
        const z_35 = a_202.Z - b_202.Z;
        distOa = Math.sqrt(((x_35 * x_35) + (y_35 * y_35)) + (z_35 * z_35));
        let distB;
        const a_204 = b_198;
        const b_204 = centroid;
        const x_36 = a_204.X - b_204.X;
        const y_36 = a_204.Y - b_204.Y;
        const z_36 = a_204.Z - b_204.Z;
        distB = Math.sqrt(((x_36 * x_36) + (y_36 * y_36)) + (z_36 * z_36));
        let distOb;
        const a_206 = ob_8;
        const b_206 = centroid;
        const x_37 = a_206.X - b_206.X;
        const y_37 = a_206.Y - b_206.Y;
        const z_37 = a_206.Z - b_206.Z;
        distOb = Math.sqrt(((x_37 * x_37) + (y_37 * y_37)) + (z_37 * z_37));
        let distC;
        const a_208 = c_60;
        const b_208 = centroid;
        const x_38 = a_208.X - b_208.X;
        const y_38 = a_208.Y - b_208.Y;
        const z_38 = a_208.Z - b_208.Z;
        distC = Math.sqrt(((x_38 * x_38) + (y_38 * y_38)) + (z_38 * z_38));
        let distOc;
        const a_210 = oc_8;
        const b_210 = centroid;
        const x_39 = a_210.X - b_210.X;
        const y_39 = a_210.Y - b_210.Y;
        const z_39 = a_210.Z - b_210.Z;
        distOc = Math.sqrt(((x_39 * x_39) + (y_39 * y_39)) + (z_39 * z_39));
        Expect_isTrue(distOa < distA)("oa is closer to centroid than a");
        Expect_isTrue(distOb < distB)("ob is closer to centroid than b");
        Expect_isTrue(distOc < distC)("oc is closer to centroid than c");
        Expect_floatClose(tol, distOa, distOb, "offset triangle is equilateral (oa-ob)");
        Expect_floatClose(tol, distOb, distOc, "offset triangle is equilateral (ob-oc)");
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset stays in same plane (XY)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        const a_211 = Pnt_$ctor_Z7AD9E565(0, 0, 5);
        const b_211 = Pnt_$ctor_Z7AD9E565(2, 0, 5);
        const c_61 = Pnt_$ctor_Z7AD9E565(1, 2, 5);
        const d_14 = 0.2;
        const patternInput_21 = Tria3D_offset_Z305FC6B8(a_211, b_211, c_61, d_14);
        const oc_9 = patternInput_21[2];
        const ob_9 = patternInput_21[1];
        const oa_9 = patternInput_21[0];
        Expect_floatClose(tol, oa_9.Z, 5, "oa stays in Z=5 plane");
        Expect_floatClose(tol, ob_9.Z, 5, "ob stays in Z=5 plane");
        Expect_floatClose(tol, oc_9.Z, 5, "oc stays in Z=5 plane");
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset stays in same plane (arbitrary tilted)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        let a_216, a_217, b_217, b_216, a_218, a_219, b_219, b_218, a_220, a_221, b_221, b_220;
        const a_212 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_212 = Pnt_$ctor_Z7AD9E565(1, 0, 1);
        const c_62 = Pnt_$ctor_Z7AD9E565(0, 1, 1);
        const d_15 = 0.1;
        const patternInput_22 = Tria3D_offset_Z305FC6B8(a_212, b_212, c_62, d_15);
        const oc_10 = patternInput_22[2];
        const ob_10 = patternInput_22[1];
        const oa_10 = patternInput_22[0];
        let v1;
        const a_213 = b_212;
        const b_213 = a_212;
        v1 = Vec_$ctor_Z7AD9E565(a_213.X - b_213.X, a_213.Y - b_213.Y, a_213.Z - b_213.Z);
        let v2;
        const a_214 = c_62;
        const b_214 = a_212;
        v2 = Vec_$ctor_Z7AD9E565(a_214.X - b_214.X, a_214.Y - b_214.Y, a_214.Z - b_214.Z);
        let normal;
        let v_76;
        const a_215 = v1;
        const b_215 = v2;
        v_76 = Vec_$ctor_Z7AD9E565((a_215.Y * b_215.Z) - (a_215.Z * b_215.Y), (a_215.Z * b_215.X) - (a_215.X * b_215.Z), (a_215.X * b_215.Y) - (a_215.Y * b_215.X));
        const x_40 = v_76.X;
        const y_40 = v_76.Y;
        const z_40 = v_76.Z;
        const l = Math.sqrt(((x_40 * x_40) + (y_40 * y_40)) + (z_40 * z_40));
        if (!(l > 1E-12)) {
            failUnit3("Vec.unitize", x_40, y_40, z_40);
        }
        const f = 1 / l;
        normal = UnitVec_$ctor_Z7AD9E565(f * x_40, f * y_40, f * z_40);
        const distOa_1 = Math.abs((a_216 = ((a_217 = oa_10, (b_217 = a_212, Vec_$ctor_Z7AD9E565(a_217.X - b_217.X, a_217.Y - b_217.Y, a_217.Z - b_217.Z)))), (b_216 = normal, ((a_216.X * b_216.X) + (a_216.Y * b_216.Y)) + (a_216.Z * b_216.Z))));
        const distOb_1 = Math.abs((a_218 = ((a_219 = ob_10, (b_219 = a_212, Vec_$ctor_Z7AD9E565(a_219.X - b_219.X, a_219.Y - b_219.Y, a_219.Z - b_219.Z)))), (b_218 = normal, ((a_218.X * b_218.X) + (a_218.Y * b_218.Y)) + (a_218.Z * b_218.Z))));
        const distOc_1 = Math.abs((a_220 = ((a_221 = oc_10, (b_221 = a_212, Vec_$ctor_Z7AD9E565(a_221.X - b_221.X, a_221.Y - b_221.Y, a_221.Z - b_221.Z)))), (b_220 = normal, ((a_220.X * b_220.X) + (a_220.Y * b_220.Y)) + (a_220.Z * b_220.Z))));
        Expect_floatClose(tol, distOa_1, 0, "oa stays in original plane");
        Expect_floatClose(tol, distOb_1, 0, "ob stays in original plane");
        Expect_floatClose(tol, distOc_1, 0, "oc stays in original plane");
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})(), (() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset throws for collinear points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        const a_222 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_222 = Pnt_$ctor_Z7AD9E565(1, 1, 1);
        const c_63 = Pnt_$ctor_Z7AD9E565(2, 2, 2);
        Test_TestCaseBuilder__TryWith_Z570AC55B(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
            Tria3D_offset_Z305FC6B8(a_222, b_222, c_63, 0.1);
            throw new Exception("Expected exception for collinear points");
            Test_TestCaseBuilder__Zero(builder$0040_31);
        }), (_arg_1) => {
            if (_arg_1 instanceof EuclidException) {
                Expect_isTrue(true)("offset correctly throws exception for collinear points");
                Test_TestCaseBuilder__Zero(builder$0040_31);
            }
            else {
                throw new Exception("Expected EuclidException");
                Test_TestCaseBuilder__Zero(builder$0040_31);
            }
        });
    }));
})()]));

