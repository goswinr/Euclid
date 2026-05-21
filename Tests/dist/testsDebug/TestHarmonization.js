
import { Test_TestCaseBuilder__For_Z371464DD, Expect_isTrue, Test_TestCaseBuilder__Zero, AccuracyModule_high, Expect_floatClose, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Pnt_$ctor_Z7AD9E565 } from "./Src/Pnt.js";
import { ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { Vec_$ctor_Z7AD9E565 } from "./Src/Vec.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_1 } from "./Src/Vec.js";
import { failUnit3 } from "./Src/EuclidErrors.js";
import { UnitVec_$ctor_Z7AD9E565 } from "./Src/UnitVec.js";
import { Vc_$ctor_7B00E9A0 } from "./Src/Vc.js";
import { Euclid_Vc__Vc_rotateByQuarterCircle_Static } from "./Src/TypeExtensions/Vc.js";
import { failUnit3 as failUnit3_1, failUnit2 } from "./Src/EuclidErrors.js";
import { UnitVc_$ctor_7B00E9A0 } from "./Src/UnitVc.js";
import { Euclid_UnitVc__UnitVc_rotateByQuarterCircle_Static } from "./Src/TypeExtensions/UnitVc.js";
import { Euclid_Vec__Vec_rotateByQuarterCircle_Static } from "./Src/TypeExtensions/Vec.js";
import { Euclid_UnitVec__UnitVec_rotateByQuarterCircle_Static } from "./Src/TypeExtensions/UnitVec.js";
import { Euclid_Vc__Vc_rotateByQuarterCircle_Static as Euclid_Vc__Vc_rotateByQuarterCircle_Static_1 } from "./Src/TypeExtensions/Vc.js";
import { Euclid_Vec__Vec_rotateByQuarterCircle_Static as Euclid_Vec__Vec_rotateByQuarterCircle_Static_1 } from "./Src/TypeExtensions/Vec.js";

export const tests = Test_testList("Harmonization Tests", ofArray([Test_testList("Pnt Squared Distance Methods", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Pnt.SqDistanceTo should match DistanceTo squared", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const p1 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        const p2 = Pnt_$ctor_Z7AD9E565(4, 6, 8);
        let sqDist;
        const p = p1;
        const b = p2;
        const x = p.X - b.X;
        const y = p.Y - b.Y;
        const z = p.Z - b.Z;
        sqDist = (((x * x) + (y * y)) + (z * z));
        let dist;
        const p_1 = p1;
        const b_1 = p2;
        const x_1 = p_1.X - b_1.X;
        const y_1 = p_1.Y - b_1.Y;
        const z_1 = p_1.Z - b_1.Z;
        dist = Math.sqrt(((x_1 * x_1) + (y_1 * y_1)) + (z_1 * z_1));
        Expect_floatClose(AccuracyModule_high, sqDist, dist * dist, "SqDistanceTo should equal DistanceTo squared");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Pnt.SqDistanceFromOrigin should match DistanceFromOrigin squared", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const p_2 = Pnt_$ctor_Z7AD9E565(3, 4, 0);
        let sqDist_1;
        const pt = p_2;
        sqDist_1 = (((pt.X * pt.X) + (pt.Y * pt.Y)) + (pt.Z * pt.Z));
        let dist_1;
        const pt_1 = p_2;
        dist_1 = Math.sqrt(((pt_1.X * pt_1.X) + (pt_1.Y * pt_1.Y)) + (pt_1.Z * pt_1.Z));
        Expect_floatClose(AccuracyModule_high, sqDist_1, dist_1 * dist_1, "SqDistanceFromOrigin should equal 25.0");
        Expect_floatClose(AccuracyModule_high, sqDist_1, 25, "Should be 25.0");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Pnt.SqDistanceTo on identical points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let p_4, b_2, x_2, y_2, z_2;
        const p_3 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        Expect_floatClose(AccuracyModule_high, (p_4 = p_3, (b_2 = p_3, (x_2 = (p_4.X - b_2.X), (y_2 = (p_4.Y - b_2.Y), (z_2 = (p_4.Z - b_2.Z), ((x_2 * x_2) + (y_2 * y_2)) + (z_2 * z_2)))))), 0, "Distance to self should be 0");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Pnt.SqDistanceTo on very close points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        const p1_1 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        const p2_1 = Pnt_$ctor_Z7AD9E565(1 + 1E-10, 2, 3);
        let sqDist_2;
        const p_5 = p1_1;
        const b_3 = p2_1;
        const x_3 = p_5.X - b_3.X;
        const y_3 = p_5.Y - b_3.Y;
        const z_3 = p_5.Z - b_3.Z;
        sqDist_2 = (((x_3 * x_3) + (y_3 * y_3)) + (z_3 * z_3));
        Expect_isTrue(sqDist_2 < 1E-15)("Very close points should have tiny squared distance");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})()])), Test_testList("Vec.Half", ofArray([(() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Vec.Half should return half length vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const v = Vec_$ctor_Z7AD9E565(4, 6, 8);
        let half;
        const v_1 = v;
        half = Vec_$ctor_Z7AD9E565_1(v_1.X * 0.5, v_1.Y * 0.5, v_1.Z * 0.5);
        Expect_floatClose(AccuracyModule_high, half.X, 2, "X should be halved");
        Expect_floatClose(AccuracyModule_high, half.Y, 3, "Y should be halved");
        Expect_floatClose(AccuracyModule_high, half.Z, 4, "Z should be halved");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Vec.Half preserves direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let a, b_4;
        const v_2 = Vec_$ctor_Z7AD9E565(1, 2, 3);
        let half_1;
        const v_3 = v_2;
        half_1 = Vec_$ctor_Z7AD9E565_1(v_3.X * 0.5, v_3.Y * 0.5, v_3.Z * 0.5);
        let unitV;
        const v_4 = v_2;
        const x_4 = v_4.X;
        const y_4 = v_4.Y;
        const z_4 = v_4.Z;
        const l = Math.sqrt(((x_4 * x_4) + (y_4 * y_4)) + (z_4 * z_4));
        if (!(l > 1E-12)) {
            failUnit3("Vec.Unitized", x_4, y_4, z_4);
        }
        const f = 1 / l;
        unitV = UnitVec_$ctor_Z7AD9E565(f * x_4, f * y_4, f * z_4);
        let unitHalf;
        const v_5 = half_1;
        const x_7 = v_5.X;
        const y_6 = v_5.Y;
        const z_6 = v_5.Z;
        const l_1 = Math.sqrt(((x_7 * x_7) + (y_6 * y_6)) + (z_6 * z_6));
        if (!(l_1 > 1E-12)) {
            failUnit3("Vec.Unitized", x_7, y_6, z_6);
        }
        const f_1 = 1 / l_1;
        unitHalf = UnitVec_$ctor_Z7AD9E565(f_1 * x_7, f_1 * y_6, f_1 * z_6);
        Expect_floatClose(AccuracyModule_high, (a = unitV, (b_4 = unitHalf, ((a.X * b_4.X) + (a.Y * b_4.Y)) + (a.Z * b_4.Z))), 1, "Direction should be preserved");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})()])), Test_testList("rotateByQuarterCircle 2D", ofArray([(() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Vc.rotateByQuarterCircle 0 quarters", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const v_6 = Vc_$ctor_7B00E9A0(1, 0);
        const rotated = Euclid_Vc__Vc_rotateByQuarterCircle_Static(0, v_6);
        Expect_floatClose(AccuracyModule_high, rotated.X, 1, "X unchanged");
        Expect_floatClose(AccuracyModule_high, rotated.Y, 0, "Y unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Vc.rotateByQuarterCircle 1 quarter CCW", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        const v_8 = Vc_$ctor_7B00E9A0(1, 0);
        const rotated_1 = Euclid_Vc__Vc_rotateByQuarterCircle_Static(1, v_8);
        Expect_floatClose(AccuracyModule_high, rotated_1.X, 0, "Should rotate to Y axis");
        Expect_floatClose(AccuracyModule_high, rotated_1.Y, 1, "Should rotate to Y axis");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Vc.rotateByQuarterCircle 2 quarters", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        const v_10 = Vc_$ctor_7B00E9A0(1, 0);
        const rotated_2 = Euclid_Vc__Vc_rotateByQuarterCircle_Static(2, v_10);
        Expect_floatClose(AccuracyModule_high, rotated_2.X, -1, "Should reverse X");
        Expect_floatClose(AccuracyModule_high, rotated_2.Y, 0, "Y should be 0");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Vc.rotateByQuarterCircle 3 quarters CCW (= 1 CW)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const v_12 = Vc_$ctor_7B00E9A0(1, 0);
        const rotated_3 = Euclid_Vc__Vc_rotateByQuarterCircle_Static(3, v_12);
        Expect_floatClose(AccuracyModule_high, rotated_3.X, 0, "Should rotate to -Y axis");
        Expect_floatClose(AccuracyModule_high, rotated_3.Y, -1, "Should rotate to -Y axis");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Vc.rotateByQuarterCircle -1 quarter (CW)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        const v_14 = Vc_$ctor_7B00E9A0(1, 0);
        const rotated_4 = Euclid_Vc__Vc_rotateByQuarterCircle_Static(-1, v_14);
        Expect_floatClose(AccuracyModule_high, rotated_4.X, 0, "Should rotate CW to -Y");
        Expect_floatClose(AccuracyModule_high, rotated_4.Y, -1, "Should rotate CW to -Y");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Vc.rotateByQuarterCircle 4 quarters = full circle", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        const v_16 = Vc_$ctor_7B00E9A0(3, 4);
        const rotated_5 = Euclid_Vc__Vc_rotateByQuarterCircle_Static(4, v_16);
        Expect_floatClose(AccuracyModule_high, rotated_5.X, v_16.X, "Full rotation returns to start");
        Expect_floatClose(AccuracyModule_high, rotated_5.Y, v_16.Y, "Full rotation returns to start");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Vc.rotateByQuarterCircle preserves length", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        let v_21, x_11, y_9;
        const v_18 = Vc_$ctor_7B00E9A0(3, 4);
        let len;
        const v_19 = v_18;
        const x_10 = v_19.X;
        const y_8 = v_19.Y;
        len = Math.sqrt((x_10 * x_10) + (y_8 * y_8));
        const rotated_6 = Euclid_Vc__Vc_rotateByQuarterCircle_Static(1, v_18);
        Expect_floatClose(AccuracyModule_high, (v_21 = rotated_6, (x_11 = v_21.X, (y_9 = v_21.Y, Math.sqrt((x_11 * x_11) + (y_9 * y_9))))), len, "Length should be preserved");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("UnitVc.rotateByQuarterCircle preserves unit length", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        let v_22;
        const l_2 = Math.sqrt((1 * 1) + (1 * 1));
        if (!(l_2 > 1E-12)) {
            failUnit2("UnitVc.create", 1, 1);
        }
        v_22 = UnitVc_$ctor_7B00E9A0(1 / l_2, 1 / l_2);
        const rotated_7 = Euclid_UnitVc__UnitVc_rotateByQuarterCircle_Static(1, v_22);
        Expect_floatClose(AccuracyModule_high, (rotated_7.X * rotated_7.X) + (rotated_7.Y * rotated_7.Y), 1, "Should remain unit vector");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})()])), Test_testList("rotateByQuarterCircle 3D", ofArray([(() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Vec.rotateByQuarterCircle 0 quarters", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        const v_24 = Vec_$ctor_Z7AD9E565(1, 0, 5);
        const rotated_8 = Euclid_Vec__Vec_rotateByQuarterCircle_Static(0, v_24);
        Expect_floatClose(AccuracyModule_high, rotated_8.X, 1, "X unchanged");
        Expect_floatClose(AccuracyModule_high, rotated_8.Y, 0, "Y unchanged");
        Expect_floatClose(AccuracyModule_high, rotated_8.Z, 5, "Z unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Vec.rotateByQuarterCircle 1 quarter around Z-axis", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        const v_26 = Vec_$ctor_Z7AD9E565(1, 0, 5);
        const rotated_9 = Euclid_Vec__Vec_rotateByQuarterCircle_Static(1, v_26);
        Expect_floatClose(AccuracyModule_high, rotated_9.X, 0, "Should rotate in XY plane");
        Expect_floatClose(AccuracyModule_high, rotated_9.Y, 1, "Should rotate in XY plane");
        Expect_floatClose(AccuracyModule_high, rotated_9.Z, 5, "Z should be preserved");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Vec.rotateByQuarterCircle preserves Z component", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        const v_28 = Vec_$ctor_Z7AD9E565(3, 4, 7);
        const rotated_10 = Euclid_Vec__Vec_rotateByQuarterCircle_Static(2, v_28);
        Expect_floatClose(AccuracyModule_high, rotated_10.Z, v_28.Z, "Z should be preserved");
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Vec.rotateByQuarterCircle preserves length", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        const v_30 = Vec_$ctor_Z7AD9E565(3, 4, 5);
        let len_1;
        const v_31 = v_30;
        len_1 = Math.sqrt(((v_31.X * v_31.X) + (v_31.Y * v_31.Y)) + (v_31.Z * v_31.Z));
        Test_TestCaseBuilder__For_Z371464DD(builder$0040_17, [-2, -1, 0, 1, 2, 3, 4, 5], (_arg) => {
            let v_33;
            const quarters = _arg | 0;
            const rotated_11 = Euclid_Vec__Vec_rotateByQuarterCircle_Static(quarters, v_30);
            Expect_floatClose(AccuracyModule_high, (v_33 = rotated_11, Math.sqrt(((v_33.X * v_33.X) + (v_33.Y * v_33.Y)) + (v_33.Z * v_33.Z))), len_1, `Length preserved for ${quarters} quarters`);
            Test_TestCaseBuilder__Zero(builder$0040_17);
        });
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("UnitVec.rotateByQuarterCircle preserves unit length", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        let v_34;
        const l_3 = Math.sqrt(((1 * 1) + (1 * 1)) + (0 * 0));
        if (!(l_3 > 1E-12)) {
            failUnit3_1("UnitVec.create", 1, 1, 0);
        }
        const li = 1 / l_3;
        v_34 = UnitVec_$ctor_Z7AD9E565(li * 1, li * 1, li * 0);
        const rotated_12 = Euclid_UnitVec__UnitVec_rotateByQuarterCircle_Static(1, v_34);
        Expect_floatClose(AccuracyModule_high, ((rotated_12.X * rotated_12.X) + (rotated_12.Y * rotated_12.Y)) + (rotated_12.Z * rotated_12.Z), 1, "Should remain unit vector");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Vec.rotateByQuarterCircle on vertical vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        const v_36 = Vec_$ctor_Z7AD9E565(0, 0, 5);
        const rotated_13 = Euclid_Vec__Vec_rotateByQuarterCircle_Static(1, v_36);
        Expect_floatClose(AccuracyModule_high, rotated_13.X, 0, "Vertical stays vertical in X");
        Expect_floatClose(AccuracyModule_high, rotated_13.Y, 0, "Vertical stays vertical in Y");
        Expect_floatClose(AccuracyModule_high, rotated_13.Z, 5, "Z unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})()])), Test_testList("Static vs Member consistency", ofArray([(() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Vc.rotateByQuarterCircle static matches member", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        const v_38 = Vc_$ctor_7B00E9A0(3, 4);
        const staticResult = Euclid_Vc__Vc_rotateByQuarterCircle_Static_1(2, v_38);
        const memberResult = Euclid_Vc__Vc_rotateByQuarterCircle_Static(2, v_38);
        Expect_floatClose(AccuracyModule_high, staticResult.X, memberResult.X, "Static and member should match");
        Expect_floatClose(AccuracyModule_high, staticResult.Y, memberResult.Y, "Static and member should match");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Vec.rotateByQuarterCircle static matches member", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        const v_41 = Vec_$ctor_Z7AD9E565(3, 4, 5);
        const staticResult_1 = Euclid_Vec__Vec_rotateByQuarterCircle_Static_1(2, v_41);
        const memberResult_1 = Euclid_Vec__Vec_rotateByQuarterCircle_Static(2, v_41);
        Expect_floatClose(AccuracyModule_high, staticResult_1.X, memberResult_1.X, "Static and member should match");
        Expect_floatClose(AccuracyModule_high, staticResult_1.Y, memberResult_1.Y, "Static and member should match");
        Expect_floatClose(AccuracyModule_high, staticResult_1.Z, memberResult_1.Z, "Static and member should match");
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})()])), Test_testList("Edge cases", ofArray([(() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("rotateByQuarterCircle with large numbers", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        const v_44 = Vc_$ctor_7B00E9A0(1, 0);
        const rotated_14 = Euclid_Vc__Vc_rotateByQuarterCircle_Static(1000, v_44);
        Expect_floatClose(AccuracyModule_high, rotated_14.X, 1, "Large number should wrap");
        Expect_floatClose(AccuracyModule_high, rotated_14.Y, 0, "Large number should wrap");
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("rotateByQuarterCircle with negative large numbers", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        const v_46 = Vc_$ctor_7B00E9A0(1, 0);
        const rotated_15 = Euclid_Vc__Vc_rotateByQuarterCircle_Static(-1001, v_46);
        Expect_floatClose(AccuracyModule_high, rotated_15.X, 0, "Should rotate correctly");
        Expect_floatClose(AccuracyModule_high, rotated_15.Y, -1, "Should rotate correctly");
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})()]))]));

