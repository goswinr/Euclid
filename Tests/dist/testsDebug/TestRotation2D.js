
import { Test_TestCaseBuilder__For_Z371464DD, Test_TestCaseBuilder__Combine_3A59D1F3, Expect_stringContains, Expect_isFalse, Expect_throws, Test_TestCaseBuilder__Zero, Expect_isTrue, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Rotation2D_$ctor_7B00E9A0 } from "./Src/Rotation2D.js";
import { ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { UnitVc_$ctor_7B00E9A0 } from "./Src/UnitVc.js";
import { Rotation2D__get_AsFSharpCode, Rotation2D__get_AsString, Rotation2D_createFromVectors_Z53905080, Rotation2D_createFromVectors_67D9FFC0 } from "./Src/Rotation2D.js";
import { Vc_$ctor_7B00E9A0 } from "./Src/Vc.js";
import { failUnit2 } from "./Src/EuclidErrors.js";
import { toString } from "./fable_modules/fable-library-js.5.0.0/Types.js";
import { rangeDouble } from "./fable_modules/fable-library-js.5.0.0/Range.js";
import { Pt_$ctor_7B00E9A0 } from "./Src/Pt.js";
import { Pt_$ctor_7B00E9A0 as Pt_$ctor_7B00E9A0_1 } from "./Src/Pt.js";
import { Exception } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { Vc_$ctor_7B00E9A0 as Vc_$ctor_7B00E9A0_1 } from "./Src/Vc.js";
import { Vc_$ctor_7B00E9A0 as Vc_$ctor_7B00E9A0_2 } from "./Src/Vc.js";

export const tests = Test_testList("Rotation2D", ofArray([Test_testList("Creation and Properties", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromDegrees 0", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let r;
        const rad = 0.017453292519943295 * 0;
        r = Rotation2D_$ctor_7B00E9A0(Math.sin(rad), Math.cos(rad));
        Expect_isTrue(Math.abs(r.Sin - 0) < 1E-09)("Sin should be 0");
        Expect_isTrue(Math.abs(r.Cos - 1) < 1E-09)("Cos should be 1");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromDegrees 90", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let r_1;
        const rad_1 = 0.017453292519943295 * 90;
        r_1 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_1), Math.cos(rad_1));
        Expect_isTrue(Math.abs(r_1.Sin - 1) < 1E-09)("Sin should be 1");
        Expect_isTrue(Math.abs(r_1.Cos - 0) < 1E-09)("Cos should be 0");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromDegrees 180", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let r_2;
        const rad_2 = 0.017453292519943295 * 180;
        r_2 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_2), Math.cos(rad_2));
        Expect_isTrue(Math.abs(r_2.Sin - 0) < 1E-09)("Sin should be 0");
        Expect_isTrue(Math.abs(r_2.Cos - -1) < 1E-09)("Cos should be -1");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromDegrees -90", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let r_3;
        const rad_3 = 0.017453292519943295 * -90;
        r_3 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_3), Math.cos(rad_3));
        Expect_isTrue(Math.abs(r_3.Sin - -1) < 1E-09)("Sin should be -1");
        Expect_isTrue(Math.abs(r_3.Cos - 0) < 1E-09)("Cos should be 0");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromRadians π/2", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let r_4;
        const rad_4 = 3.141592653589793 / 2;
        r_4 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_4), Math.cos(rad_4));
        Expect_isTrue(Math.abs(r_4.Sin - 1) < 1E-09)("Sin should be 1");
        Expect_isTrue(Math.abs(r_4.Cos - 0) < 1E-09)("Cos should be 0");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromRadians π", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const r_5 = Rotation2D_$ctor_7B00E9A0(Math.sin(3.141592653589793), Math.cos(3.141592653589793));
        Expect_isTrue(Math.abs(r_5.Sin - 0) < 1E-09)("Sin should be 0");
        Expect_isTrue(Math.abs(r_5.Cos - -1) < 1E-09)("Cos should be -1");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})()])), Test_testList("InRadians and InDegrees full range", ofArray([(() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("InDegrees 45°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        let a_12, r_8;
        let r_6;
        const rad_6 = 0.017453292519943295 * 45;
        r_6 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_6), Math.cos(rad_6));
        Expect_isTrue((a_12 = (57.29577951308232 * ((r_8 = r_6, Math.atan2(r_8.Sin, r_8.Cos)))), Math.abs(a_12 - 45) < 1E-09))("Should return 45°");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("InDegrees 90°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let a_13, r_11;
        let r_9;
        const rad_7 = 0.017453292519943295 * 90;
        r_9 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_7), Math.cos(rad_7));
        Expect_isTrue((a_13 = (57.29577951308232 * ((r_11 = r_9, Math.atan2(r_11.Sin, r_11.Cos)))), Math.abs(a_13 - 90) < 1E-09))("Should return 90°");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("InDegrees 135°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let a_14, r_14;
        let r_12;
        const rad_8 = 0.017453292519943295 * 135;
        r_12 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_8), Math.cos(rad_8));
        Expect_isTrue((a_14 = (57.29577951308232 * ((r_14 = r_12, Math.atan2(r_14.Sin, r_14.Cos)))), Math.abs(a_14 - 135) < 1E-09))("Should return 135° not 45°");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("InDegrees 180°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        let r_17;
        let r_15;
        const rad_9 = 0.017453292519943295 * 180;
        r_15 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_9), Math.cos(rad_9));
        const deg_8 = Math.abs(57.29577951308232 * ((r_17 = r_15, Math.atan2(r_17.Sin, r_17.Cos))));
        Expect_isTrue(Math.abs(deg_8 - 180) < 1E-09)("Should return ±180°");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("InDegrees 270°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let a_16, r_20;
        let r_18;
        const rad_10 = 0.017453292519943295 * 270;
        r_18 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_10), Math.cos(rad_10));
        Expect_isTrue((a_16 = (57.29577951308232 * ((r_20 = r_18, Math.atan2(r_20.Sin, r_20.Cos)))), Math.abs(a_16 - -90) < 1E-09))("Should return -90° (equivalent to 270°)");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("InDegrees -90°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        let a_17, r_23;
        let r_21;
        const rad_11 = 0.017453292519943295 * -90;
        r_21 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_11), Math.cos(rad_11));
        Expect_isTrue((a_17 = (57.29577951308232 * ((r_23 = r_21, Math.atan2(r_23.Sin, r_23.Cos)))), Math.abs(a_17 - -90) < 1E-09))("Should return -90°");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("InDegrees -135°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        let a_18, r_26;
        let r_24;
        const rad_12 = 0.017453292519943295 * -135;
        r_24 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_12), Math.cos(rad_12));
        Expect_isTrue((a_18 = (57.29577951308232 * ((r_26 = r_24, Math.atan2(r_26.Sin, r_26.Cos)))), Math.abs(a_18 - -135) < 1E-09))("Should return -135°");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("InRadians π/4", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        let a_19, r_28, b_19;
        let r_27;
        const rad_13 = 3.141592653589793 / 4;
        r_27 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_13), Math.cos(rad_13));
        Expect_isTrue((a_19 = ((r_28 = r_27, Math.atan2(r_28.Sin, r_28.Cos))), (b_19 = (3.141592653589793 / 4), Math.abs(a_19 - b_19) < 1E-09)))("Should return π/4");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("InRadians 3π/4", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let a_20, r_30, b_20;
        let r_29;
        const rad_14 = (3 * 3.141592653589793) / 4;
        r_29 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_14), Math.cos(rad_14));
        Expect_isTrue((a_20 = ((r_30 = r_29, Math.atan2(r_30.Sin, r_30.Cos))), (b_20 = ((3 * 3.141592653589793) / 4), Math.abs(a_20 - b_20) < 1E-09)))("Should return 3π/4");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})()])), Test_testList("Inverse", ofArray([(() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Inverse of 45°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        let a_21, r_34;
        let r_31;
        const rad_15 = 0.017453292519943295 * 45;
        r_31 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_15), Math.cos(rad_15));
        let inv;
        const r_32 = r_31;
        inv = Rotation2D_$ctor_7B00E9A0(-r_32.Sin, r_32.Cos);
        Expect_isTrue((a_21 = (57.29577951308232 * ((r_34 = inv, Math.atan2(r_34.Sin, r_34.Cos)))), Math.abs(a_21 - -45) < 1E-09))("Inverse should be -45°");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Inverse of -90°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let a_22, r_38;
        let r_35;
        const rad_16 = 0.017453292519943295 * -90;
        r_35 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_16), Math.cos(rad_16));
        let inv_1;
        const r_36 = r_35;
        inv_1 = Rotation2D_$ctor_7B00E9A0(-r_36.Sin, r_36.Cos);
        Expect_isTrue((a_22 = (57.29577951308232 * ((r_38 = inv_1, Math.atan2(r_38.Sin, r_38.Cos)))), Math.abs(a_22 - 90) < 1E-09))("Inverse should be 90°");
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Inverse of 135°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        let a_23, r_42;
        let r_39;
        const rad_17 = 0.017453292519943295 * 135;
        r_39 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_17), Math.cos(rad_17));
        let inv_2;
        const r_40 = r_39;
        inv_2 = Rotation2D_$ctor_7B00E9A0(-r_40.Sin, r_40.Cos);
        Expect_isTrue((a_23 = (57.29577951308232 * ((r_42 = inv_2, Math.atan2(r_42.Sin, r_42.Cos)))), Math.abs(a_23 - -135) < 1E-09))("Inverse should be -135°");
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Double inverse returns original", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        let r_43;
        const rad_18 = 0.017453292519943295 * 67;
        r_43 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_18), Math.cos(rad_18));
        let inv_3;
        let copyOfStruct;
        const r_44 = r_43;
        copyOfStruct = Rotation2D_$ctor_7B00E9A0(-r_44.Sin, r_44.Cos);
        const r_45 = copyOfStruct;
        inv_3 = Rotation2D_$ctor_7B00E9A0(-r_45.Sin, r_45.Cos);
        Expect_isTrue((Math.abs(r_43.Sin - inv_3.Sin) < 1E-09) && (Math.abs(r_43.Cos - inv_3.Cos) < 1E-09))("Double inverse should equal original");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})()])), Test_testList("Add operations", ofArray([(() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Add 45° + 45° = 90°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        let a_26, r_48;
        let r1;
        const rad_19 = 0.017453292519943295 * 45;
        r1 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_19), Math.cos(rad_19));
        let r2;
        const rad_20 = 0.017453292519943295 * 45;
        r2 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_20), Math.cos(rad_20));
        let result;
        const r_46 = r1;
        const ro = r2;
        result = Rotation2D_$ctor_7B00E9A0((r_46.Sin * ro.Cos) + (r_46.Cos * ro.Sin), (r_46.Cos * ro.Cos) - (r_46.Sin * ro.Sin));
        Expect_isTrue((a_26 = (57.29577951308232 * ((r_48 = result, Math.atan2(r_48.Sin, r_48.Cos)))), Math.abs(a_26 - 90) < 1E-09))("45° + 45° should be 90°");
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Add 90° + 90° = 180°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        let r_51;
        let r1_1;
        const rad_21 = 0.017453292519943295 * 90;
        r1_1 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_21), Math.cos(rad_21));
        let r2_1;
        const rad_22 = 0.017453292519943295 * 90;
        r2_1 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_22), Math.cos(rad_22));
        let result_1;
        const r_49 = r1_1;
        const ro_1 = r2_1;
        result_1 = Rotation2D_$ctor_7B00E9A0((r_49.Sin * ro_1.Cos) + (r_49.Cos * ro_1.Sin), (r_49.Cos * ro_1.Cos) - (r_49.Sin * ro_1.Sin));
        const deg_20 = Math.abs(57.29577951308232 * ((r_51 = result_1, Math.atan2(r_51.Sin, r_51.Cos))));
        Expect_isTrue(Math.abs(deg_20 - 180) < 1E-09)("90° + 90° should be ±180°");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Add 135° + 90° = -135°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        let a_28, r_54;
        let r1_2;
        const rad_23 = 0.017453292519943295 * 135;
        r1_2 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_23), Math.cos(rad_23));
        let r2_2;
        const rad_24 = 0.017453292519943295 * 90;
        r2_2 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_24), Math.cos(rad_24));
        let result_2;
        const r_52 = r1_2;
        const ro_2 = r2_2;
        result_2 = Rotation2D_$ctor_7B00E9A0((r_52.Sin * ro_2.Cos) + (r_52.Cos * ro_2.Sin), (r_52.Cos * ro_2.Cos) - (r_52.Sin * ro_2.Sin));
        Expect_isTrue((a_28 = (57.29577951308232 * ((r_54 = result_2, Math.atan2(r_54.Sin, r_54.Cos)))), Math.abs(a_28 - -135) < 1E-09))("135° + 90° wraps to -135°");
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Add negative angles", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        let a_29, r_57;
        let r1_3;
        const rad_25 = 0.017453292519943295 * -45;
        r1_3 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_25), Math.cos(rad_25));
        let r2_3;
        const rad_26 = 0.017453292519943295 * -45;
        r2_3 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_26), Math.cos(rad_26));
        let result_3;
        const r_55 = r1_3;
        const ro_3 = r2_3;
        result_3 = Rotation2D_$ctor_7B00E9A0((r_55.Sin * ro_3.Cos) + (r_55.Cos * ro_3.Sin), (r_55.Cos * ro_3.Cos) - (r_55.Sin * ro_3.Sin));
        Expect_isTrue((a_29 = (57.29577951308232 * ((r_57 = result_3, Math.atan2(r_57.Sin, r_57.Cos)))), Math.abs(a_29 - -90) < 1E-09))("-45° + -45° should be -90°");
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("AddDegrees 30° + 60° = 90°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        let a_30, r_62;
        let r_58;
        const rad_27 = 0.017453292519943295 * 30;
        r_58 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_27), Math.cos(rad_27));
        let result_4;
        const r_60 = r_58;
        let ro_4;
        const rad_28 = 0.017453292519943295 * 60;
        ro_4 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_28), Math.cos(rad_28));
        result_4 = Rotation2D_$ctor_7B00E9A0((r_60.Sin * ro_4.Cos) + (r_60.Cos * ro_4.Sin), (r_60.Cos * ro_4.Cos) - (r_60.Sin * ro_4.Sin));
        Expect_isTrue((a_30 = (57.29577951308232 * ((r_62 = result_4, Math.atan2(r_62.Sin, r_62.Cos)))), Math.abs(a_30 - 90) < 1E-09))("30° + 60° should be 90°");
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("AddRadians π/4 + π/4 = π/2", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        let a_31, r_66, b_31;
        let r_63;
        const rad_29 = 3.141592653589793 / 4;
        r_63 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_29), Math.cos(rad_29));
        let result_5;
        const r_65 = r_63;
        let ro_5;
        const rad_31 = 3.141592653589793 / 4;
        ro_5 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_31), Math.cos(rad_31));
        result_5 = Rotation2D_$ctor_7B00E9A0((r_65.Sin * ro_5.Cos) + (r_65.Cos * ro_5.Sin), (r_65.Cos * ro_5.Cos) - (r_65.Sin * ro_5.Sin));
        Expect_isTrue((a_31 = ((r_66 = result_5, Math.atan2(r_66.Sin, r_66.Cos))), (b_31 = (3.141592653589793 / 2), Math.abs(a_31 - b_31) < 1E-09)))("π/4 + π/4 should be π/2");
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})()])), Test_testList("createFromVectors with UnitVc", ofArray([(() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("From Xaxis to Yaxis = 90°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        let a_33, r_69;
        const a_32 = UnitVc_$ctor_7B00E9A0(1, 0);
        const b_32 = UnitVc_$ctor_7B00E9A0(0, 1);
        const r_67 = Rotation2D_createFromVectors_67D9FFC0(a_32, b_32);
        Expect_isTrue((a_33 = (57.29577951308232 * ((r_69 = r_67, Math.atan2(r_69.Sin, r_69.Cos)))), Math.abs(a_33 - 90) < 1E-09))("Xaxis to Yaxis should be 90°");
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("From Yaxis to Xaxis = -90°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        let a_35, r_72;
        const a_34 = UnitVc_$ctor_7B00E9A0(0, 1);
        const b_34 = UnitVc_$ctor_7B00E9A0(1, 0);
        const r_70 = Rotation2D_createFromVectors_67D9FFC0(a_34, b_34);
        Expect_isTrue((a_35 = (57.29577951308232 * ((r_72 = r_70, Math.atan2(r_72.Sin, r_72.Cos)))), Math.abs(a_35 - -90) < 1E-09))("Yaxis to Xaxis should be -90°");
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("From Xaxis to -Xaxis = 180°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        let r_75;
        const a_36 = UnitVc_$ctor_7B00E9A0(1, 0);
        let b_36;
        const v_1 = Vc_$ctor_7B00E9A0(-1, 0);
        const x_5 = v_1.X;
        const y_5 = v_1.Y;
        const l = Math.sqrt((x_5 * x_5) + (y_5 * y_5));
        if (!(l > 1E-12)) {
            failUnit2("Vc.unitize", x_5, y_5);
        }
        b_36 = UnitVc_$ctor_7B00E9A0(x_5 / l, y_5 / l);
        const r_73 = Rotation2D_createFromVectors_67D9FFC0(a_36, b_36);
        const deg_28 = Math.abs(57.29577951308232 * ((r_75 = r_73, Math.atan2(r_75.Sin, r_75.Cos))));
        Expect_isTrue(Math.abs(deg_28 - 180) < 1E-09)("Xaxis to -Xaxis should be ±180°");
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Same vector = 0°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        let a_39, r_78;
        const a_38 = UnitVc_$ctor_7B00E9A0(1, 0);
        const b_38 = UnitVc_$ctor_7B00E9A0(1, 0);
        const r_76 = Rotation2D_createFromVectors_67D9FFC0(a_38, b_38);
        Expect_isTrue((a_39 = (57.29577951308232 * ((r_78 = r_76, Math.atan2(r_78.Sin, r_78.Cos)))), Math.abs(a_39 - 0) < 1E-09))("Same vector should be 0°");
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("45° rotation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        let a_41, r_81;
        const a_40 = UnitVc_$ctor_7B00E9A0(1, 0);
        let b_40;
        const v_3 = Vc_$ctor_7B00E9A0(1, 1);
        const x_11 = v_3.X;
        const y_10 = v_3.Y;
        const l_1 = Math.sqrt((x_11 * x_11) + (y_10 * y_10));
        if (!(l_1 > 1E-12)) {
            failUnit2("Vc.unitize", x_11, y_10);
        }
        b_40 = UnitVc_$ctor_7B00E9A0(x_11 / l_1, y_10 / l_1);
        const r_79 = Rotation2D_createFromVectors_67D9FFC0(a_40, b_40);
        Expect_isTrue((a_41 = (57.29577951308232 * ((r_81 = r_79, Math.atan2(r_81.Sin, r_81.Cos)))), Math.abs(a_41 - 45) < 1E-09))("Should be 45°");
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("135° rotation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        let a_43, r_84;
        const a_42 = UnitVc_$ctor_7B00E9A0(1, 0);
        let b_42;
        const v_5 = Vc_$ctor_7B00E9A0(-1, 1);
        const x_15 = v_5.X;
        const y_13 = v_5.Y;
        const l_2 = Math.sqrt((x_15 * x_15) + (y_13 * y_13));
        if (!(l_2 > 1E-12)) {
            failUnit2("Vc.unitize", x_15, y_13);
        }
        b_42 = UnitVc_$ctor_7B00E9A0(x_15 / l_2, y_13 / l_2);
        const r_82 = Rotation2D_createFromVectors_67D9FFC0(a_42, b_42);
        Expect_isTrue((a_43 = (57.29577951308232 * ((r_84 = r_82, Math.atan2(r_84.Sin, r_84.Cos)))), Math.abs(a_43 - 135) < 1E-09))("Should be 135°");
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})()])), Test_testList("createFromVectors with Vc", ofArray([(() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("From (1,0) to (0,1) = 90°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        let a_45, r_87;
        const a_44 = Vc_$ctor_7B00E9A0(1, 0);
        const b_44 = Vc_$ctor_7B00E9A0(0, 1);
        const r_85 = Rotation2D_createFromVectors_Z53905080(a_44, b_44);
        Expect_isTrue((a_45 = (57.29577951308232 * ((r_87 = r_85, Math.atan2(r_87.Sin, r_87.Cos)))), Math.abs(a_45 - 90) < 1E-09))("(1,0) to (0,1) should be 90°");
        Test_TestCaseBuilder__Zero(builder$0040_31);
    }));
})(), (() => {
    const builder$0040_32 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("From (2,0) to (0,3) = 90°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_32, Test_TestCaseBuilder__Delay_1505(builder$0040_32, () => {
        let a_47, r_90;
        const a_46 = Vc_$ctor_7B00E9A0(2, 0);
        const b_46 = Vc_$ctor_7B00E9A0(0, 3);
        const r_88 = Rotation2D_createFromVectors_Z53905080(a_46, b_46);
        Expect_isTrue((a_47 = (57.29577951308232 * ((r_90 = r_88, Math.atan2(r_90.Sin, r_90.Cos)))), Math.abs(a_47 - 90) < 1E-09))("Magnitude doesn\'t affect angle");
        Test_TestCaseBuilder__Zero(builder$0040_32);
    }));
})(), (() => {
    const builder$0040_33 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("From (1,1) to (-1,1) = 90°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_33, Test_TestCaseBuilder__Delay_1505(builder$0040_33, () => {
        let a_49, r_93;
        const a_48 = Vc_$ctor_7B00E9A0(1, 1);
        const b_48 = Vc_$ctor_7B00E9A0(-1, 1);
        const r_91 = Rotation2D_createFromVectors_Z53905080(a_48, b_48);
        Expect_isTrue((a_49 = (57.29577951308232 * ((r_93 = r_91, Math.atan2(r_93.Sin, r_93.Cos)))), Math.abs(a_49 - 90) < 1E-09))("(1,1) to (-1,1) should be 90°");
        Test_TestCaseBuilder__Zero(builder$0040_33);
    }));
})(), (() => {
    const builder$0040_34 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Zero-length vector a throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_34, Test_TestCaseBuilder__Delay_1505(builder$0040_34, () => {
        const a_50 = Vc_$ctor_7B00E9A0(0, 0);
        const b_50 = Vc_$ctor_7B00E9A0(1, 0);
        Expect_throws(() => {
            Rotation2D_createFromVectors_Z53905080(a_50, b_50);
        }, "Should throw for zero-length vector a");
        Test_TestCaseBuilder__Zero(builder$0040_34);
    }));
})(), (() => {
    const builder$0040_35 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Zero-length vector b throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_35, Test_TestCaseBuilder__Delay_1505(builder$0040_35, () => {
        const a_51 = Vc_$ctor_7B00E9A0(1, 0);
        const b_51 = Vc_$ctor_7B00E9A0(0, 0);
        Expect_throws(() => {
            Rotation2D_createFromVectors_Z53905080(a_51, b_51);
        }, "Should throw for zero-length vector b");
        Test_TestCaseBuilder__Zero(builder$0040_35);
    }));
})(), (() => {
    const builder$0040_36 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Very small vector a throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_36, Test_TestCaseBuilder__Delay_1505(builder$0040_36, () => {
        const a_52 = Vc_$ctor_7B00E9A0(1E-20, 0);
        const b_52 = Vc_$ctor_7B00E9A0(1, 0);
        Expect_throws(() => {
            Rotation2D_createFromVectors_Z53905080(a_52, b_52);
        }, "Should throw for very small vector a");
        Test_TestCaseBuilder__Zero(builder$0040_36);
    }));
})(), (() => {
    const builder$0040_37 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Very small vector b throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_37, Test_TestCaseBuilder__Delay_1505(builder$0040_37, () => {
        const a_53 = Vc_$ctor_7B00E9A0(1, 0);
        const b_53 = Vc_$ctor_7B00E9A0(0, 1E-20);
        Expect_throws(() => {
            Rotation2D_createFromVectors_Z53905080(a_53, b_53);
        }, "Should throw for very small vector b");
        Test_TestCaseBuilder__Zero(builder$0040_37);
    }));
})()])), Test_testList("equals method", ofArray([(() => {
    const builder$0040_38 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Exact equality with tolerance 0.0", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_38, Test_TestCaseBuilder__Delay_1505(builder$0040_38, () => {
        let a_55, b_55;
        let r1_4;
        const rad_32 = 0.017453292519943295 * 45;
        r1_4 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_32), Math.cos(rad_32));
        let r2_4;
        const rad_33 = 0.017453292519943295 * 45;
        r2_4 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_33), Math.cos(rad_33));
        Expect_isTrue((a_55 = r1_4, (b_55 = r2_4, (Math.abs(a_55.Sin - b_55.Sin) <= 0) && (Math.abs(a_55.Cos - b_55.Cos) <= 0))))("Same rotations should be equal with 0.0 tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_38);
    }));
})(), (() => {
    const builder$0040_39 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Within tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_39, Test_TestCaseBuilder__Delay_1505(builder$0040_39, () => {
        let a_57, b_57;
        let r1_5;
        const rad_34 = 0.017453292519943295 * 45;
        r1_5 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_34), Math.cos(rad_34));
        let r2_5;
        const rad_35 = 0.017453292519943295 * 45.001;
        r2_5 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_35), Math.cos(rad_35));
        Expect_isTrue((a_57 = r1_5, (b_57 = r2_5, (Math.abs(a_57.Sin - b_57.Sin) <= 0.001) && (Math.abs(a_57.Cos - b_57.Cos) <= 0.001))))("Should be equal within tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_39);
    }));
})(), (() => {
    const builder$0040_40 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Outside tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_40, Test_TestCaseBuilder__Delay_1505(builder$0040_40, () => {
        let a_59, b_59;
        let r1_6;
        const rad_36 = 0.017453292519943295 * 45;
        r1_6 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_36), Math.cos(rad_36));
        let r2_6;
        const rad_37 = 0.017453292519943295 * 46;
        r2_6 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_37), Math.cos(rad_37));
        Expect_isFalse((a_59 = r1_6, (b_59 = r2_6, (Math.abs(a_59.Sin - b_59.Sin) <= 0.001) && (Math.abs(a_59.Cos - b_59.Cos) <= 0.001))))("Should not be equal outside tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_40);
    }));
})(), (() => {
    const builder$0040_41 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Different rotations not equal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_41, Test_TestCaseBuilder__Delay_1505(builder$0040_41, () => {
        let a_61, b_61;
        let r1_7;
        const rad_38 = 0.017453292519943295 * 45;
        r1_7 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_38), Math.cos(rad_38));
        let r2_7;
        const rad_39 = 0.017453292519943295 * 90;
        r2_7 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_39), Math.cos(rad_39));
        Expect_isFalse((a_61 = r1_7, (b_61 = r2_7, (Math.abs(a_61.Sin - b_61.Sin) <= 0) && (Math.abs(a_61.Cos - b_61.Cos) <= 0))))("Different rotations should not be equal");
        Test_TestCaseBuilder__Zero(builder$0040_41);
    }));
})()])), Test_testList("String representations", ofArray([(() => {
    const builder$0040_42 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ToString contains degrees", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_42, Test_TestCaseBuilder__Delay_1505(builder$0040_42, () => {
        let r_94;
        const rad_40 = 0.017453292519943295 * 45;
        r_94 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_40), Math.cos(rad_40));
        const str = toString(r_94);
        Expect_stringContains(str, "45", "ToString should contain angle value");
        Test_TestCaseBuilder__Zero(builder$0040_42);
    }));
})(), (() => {
    const builder$0040_43 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("AsString contains degrees", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_43, Test_TestCaseBuilder__Delay_1505(builder$0040_43, () => {
        let r_95;
        const rad_41 = 0.017453292519943295 * 90;
        r_95 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_41), Math.cos(rad_41));
        const str_1 = Rotation2D__get_AsString(r_95);
        Expect_stringContains(str_1, "90", "AsString should contain angle value");
        Test_TestCaseBuilder__Zero(builder$0040_43);
    }));
})(), (() => {
    const builder$0040_44 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("AsFSharpCode is valid", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_44, Test_TestCaseBuilder__Delay_1505(builder$0040_44, () => {
        let r_96;
        const rad_42 = 0.017453292519943295 * 30;
        r_96 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_42), Math.cos(rad_42));
        const code = Rotation2D__get_AsFSharpCode(r_96);
        Expect_stringContains(code, "Rotation2D", "AsFSharpCode should contain type name");
        Test_TestCaseBuilder__Zero(builder$0040_44);
    }));
})()])), Test_testList("Edge cases", ofArray([(() => {
    const builder$0040_45 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("360° equals 0°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_45, Test_TestCaseBuilder__Delay_1505(builder$0040_45, () => {
        let a_63, b_63;
        let r360;
        const rad_43 = 0.017453292519943295 * 360;
        r360 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_43), Math.cos(rad_43));
        let r0;
        const rad_44 = 0.017453292519943295 * 0;
        r0 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_44), Math.cos(rad_44));
        Expect_isTrue((a_63 = r360, (b_63 = r0, (Math.abs(a_63.Sin - b_63.Sin) <= 1E-09) && (Math.abs(a_63.Cos - b_63.Cos) <= 1E-09))))("360° should equal 0°");
        Test_TestCaseBuilder__Zero(builder$0040_45);
    }));
})(), (() => {
    const builder$0040_46 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("720° equals 0°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_46, Test_TestCaseBuilder__Delay_1505(builder$0040_46, () => {
        let a_65, b_65;
        let r720;
        const rad_45 = 0.017453292519943295 * 720;
        r720 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_45), Math.cos(rad_45));
        let r0_1;
        const rad_46 = 0.017453292519943295 * 0;
        r0_1 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_46), Math.cos(rad_46));
        Expect_isTrue((a_65 = r720, (b_65 = r0_1, (Math.abs(a_65.Sin - b_65.Sin) <= 1E-09) && (Math.abs(a_65.Cos - b_65.Cos) <= 1E-09))))("720° should equal 0°");
        Test_TestCaseBuilder__Zero(builder$0040_46);
    }));
})(), (() => {
    const builder$0040_47 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("-360° equals 0°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_47, Test_TestCaseBuilder__Delay_1505(builder$0040_47, () => {
        let a_67, b_67;
        let rNeg360;
        const rad_47 = 0.017453292519943295 * -360;
        rNeg360 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_47), Math.cos(rad_47));
        let r0_2;
        const rad_48 = 0.017453292519943295 * 0;
        r0_2 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_48), Math.cos(rad_48));
        Expect_isTrue((a_67 = rNeg360, (b_67 = r0_2, (Math.abs(a_67.Sin - b_67.Sin) <= 1E-09) && (Math.abs(a_67.Cos - b_67.Cos) <= 1E-09))))("-360° should equal 0°");
        Test_TestCaseBuilder__Zero(builder$0040_47);
    }));
})(), (() => {
    const builder$0040_48 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Add with wrapping positive", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_48, Test_TestCaseBuilder__Delay_1505(builder$0040_48, () => {
        let a_68, r_99;
        let r1_8;
        const rad_49 = 0.017453292519943295 * 200;
        r1_8 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_49), Math.cos(rad_49));
        let r2_8;
        const rad_50 = 0.017453292519943295 * 200;
        r2_8 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_50), Math.cos(rad_50));
        let result_6;
        const r_97 = r1_8;
        const ro_6 = r2_8;
        result_6 = Rotation2D_$ctor_7B00E9A0((r_97.Sin * ro_6.Cos) + (r_97.Cos * ro_6.Sin), (r_97.Cos * ro_6.Cos) - (r_97.Sin * ro_6.Sin));
        Expect_isTrue((a_68 = (57.29577951308232 * ((r_99 = result_6, Math.atan2(r_99.Sin, r_99.Cos)))), Math.abs(a_68 - 40) < 1E-09))("200° + 200° should wrap to 40°");
        Test_TestCaseBuilder__Zero(builder$0040_48);
    }));
})(), (() => {
    const builder$0040_49 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Add with wrapping negative", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_49, Test_TestCaseBuilder__Delay_1505(builder$0040_49, () => {
        let a_69, r_102;
        let r1_9;
        const rad_51 = 0.017453292519943295 * -200;
        r1_9 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_51), Math.cos(rad_51));
        let r2_9;
        const rad_52 = 0.017453292519943295 * -200;
        r2_9 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_52), Math.cos(rad_52));
        let result_7;
        const r_100 = r1_9;
        const ro_7 = r2_9;
        result_7 = Rotation2D_$ctor_7B00E9A0((r_100.Sin * ro_7.Cos) + (r_100.Cos * ro_7.Sin), (r_100.Cos * ro_7.Cos) - (r_100.Sin * ro_7.Sin));
        Expect_isTrue((a_69 = (57.29577951308232 * ((r_102 = result_7, Math.atan2(r_102.Sin, r_102.Cos)))), Math.abs(a_69 - -40) < 1E-09))("-200° + -200° should wrap to -40°");
        Test_TestCaseBuilder__Zero(builder$0040_49);
    }));
})()])), Test_testList("Add rotations beyond 360° (multiple full rotations)", ofArray([(() => {
    const builder$0040_50 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("90° + 90° + 90° + 90° = 0° (full rotation counterclockwise)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_50, Test_TestCaseBuilder__Delay_1505(builder$0040_50, () => {
        let a_70, r_111;
        let r90;
        const rad_53 = 0.017453292519943295 * 90;
        r90 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_53), Math.cos(rad_53));
        let result_8;
        let copyOfStruct_6;
        let copyOfStruct_5;
        let copyOfStruct_4;
        let copyOfStruct_3;
        let copyOfStruct_2;
        let copyOfStruct_1;
        const r_103 = r90;
        const ro_8 = r90;
        copyOfStruct_1 = Rotation2D_$ctor_7B00E9A0((r_103.Sin * ro_8.Cos) + (r_103.Cos * ro_8.Sin), (r_103.Cos * ro_8.Cos) - (r_103.Sin * ro_8.Sin));
        const r_104 = copyOfStruct_1;
        const ro_9 = r90;
        copyOfStruct_2 = Rotation2D_$ctor_7B00E9A0((r_104.Sin * ro_9.Cos) + (r_104.Cos * ro_9.Sin), (r_104.Cos * ro_9.Cos) - (r_104.Sin * ro_9.Sin));
        const r_105 = copyOfStruct_2;
        const ro_10 = r90;
        copyOfStruct_3 = Rotation2D_$ctor_7B00E9A0((r_105.Sin * ro_10.Cos) + (r_105.Cos * ro_10.Sin), (r_105.Cos * ro_10.Cos) - (r_105.Sin * ro_10.Sin));
        const r_106 = copyOfStruct_3;
        const ro_11 = r90;
        copyOfStruct_4 = Rotation2D_$ctor_7B00E9A0((r_106.Sin * ro_11.Cos) + (r_106.Cos * ro_11.Sin), (r_106.Cos * ro_11.Cos) - (r_106.Sin * ro_11.Sin));
        const r_107 = copyOfStruct_4;
        const ro_12 = r90;
        copyOfStruct_5 = Rotation2D_$ctor_7B00E9A0((r_107.Sin * ro_12.Cos) + (r_107.Cos * ro_12.Sin), (r_107.Cos * ro_12.Cos) - (r_107.Sin * ro_12.Sin));
        const r_108 = copyOfStruct_5;
        const ro_13 = r90;
        copyOfStruct_6 = Rotation2D_$ctor_7B00E9A0((r_108.Sin * ro_13.Cos) + (r_108.Cos * ro_13.Sin), (r_108.Cos * ro_13.Cos) - (r_108.Sin * ro_13.Sin));
        const r_109 = copyOfStruct_6;
        const ro_14 = r90;
        result_8 = Rotation2D_$ctor_7B00E9A0((r_109.Sin * ro_14.Cos) + (r_109.Cos * ro_14.Sin), (r_109.Cos * ro_14.Cos) - (r_109.Sin * ro_14.Sin));
        Expect_isTrue((a_70 = (57.29577951308232 * ((r_111 = result_8, Math.atan2(r_111.Sin, r_111.Cos)))), Math.abs(a_70 - 0) < 1E-09))("Eight 90° rotations should return to 0°");
        Test_TestCaseBuilder__Zero(builder$0040_50);
    }));
})(), (() => {
    const builder$0040_51 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("45° added eight times = 0° (full rotation counterclockwise)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_51, Test_TestCaseBuilder__Delay_1505(builder$0040_51, () => {
        let a_71, r_120;
        let r45;
        const rad_54 = 0.017453292519943295 * 45;
        r45 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_54), Math.cos(rad_54));
        let result_9;
        let copyOfStruct_12;
        let copyOfStruct_11;
        let copyOfStruct_10;
        let copyOfStruct_9;
        let copyOfStruct_8;
        let copyOfStruct_7;
        const r_112 = r45;
        const ro_15 = r45;
        copyOfStruct_7 = Rotation2D_$ctor_7B00E9A0((r_112.Sin * ro_15.Cos) + (r_112.Cos * ro_15.Sin), (r_112.Cos * ro_15.Cos) - (r_112.Sin * ro_15.Sin));
        const r_113 = copyOfStruct_7;
        const ro_16 = r45;
        copyOfStruct_8 = Rotation2D_$ctor_7B00E9A0((r_113.Sin * ro_16.Cos) + (r_113.Cos * ro_16.Sin), (r_113.Cos * ro_16.Cos) - (r_113.Sin * ro_16.Sin));
        const r_114 = copyOfStruct_8;
        const ro_17 = r45;
        copyOfStruct_9 = Rotation2D_$ctor_7B00E9A0((r_114.Sin * ro_17.Cos) + (r_114.Cos * ro_17.Sin), (r_114.Cos * ro_17.Cos) - (r_114.Sin * ro_17.Sin));
        const r_115 = copyOfStruct_9;
        const ro_18 = r45;
        copyOfStruct_10 = Rotation2D_$ctor_7B00E9A0((r_115.Sin * ro_18.Cos) + (r_115.Cos * ro_18.Sin), (r_115.Cos * ro_18.Cos) - (r_115.Sin * ro_18.Sin));
        const r_116 = copyOfStruct_10;
        const ro_19 = r45;
        copyOfStruct_11 = Rotation2D_$ctor_7B00E9A0((r_116.Sin * ro_19.Cos) + (r_116.Cos * ro_19.Sin), (r_116.Cos * ro_19.Cos) - (r_116.Sin * ro_19.Sin));
        const r_117 = copyOfStruct_11;
        const ro_20 = r45;
        copyOfStruct_12 = Rotation2D_$ctor_7B00E9A0((r_117.Sin * ro_20.Cos) + (r_117.Cos * ro_20.Sin), (r_117.Cos * ro_20.Cos) - (r_117.Sin * ro_20.Sin));
        const r_118 = copyOfStruct_12;
        const ro_21 = r45;
        result_9 = Rotation2D_$ctor_7B00E9A0((r_118.Sin * ro_21.Cos) + (r_118.Cos * ro_21.Sin), (r_118.Cos * ro_21.Cos) - (r_118.Sin * ro_21.Sin));
        Expect_isTrue((a_71 = (57.29577951308232 * ((r_120 = result_9, Math.atan2(r_120.Sin, r_120.Cos)))), Math.abs(a_71 - 0) < 1E-09))("Eight 45° rotations (360°) should return to 0°");
        Test_TestCaseBuilder__Zero(builder$0040_51);
    }));
})(), (() => {
    const builder$0040_52 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("-90° + -90° + -90° + -90° = 0° (full rotation clockwise)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_52, Test_TestCaseBuilder__Delay_1505(builder$0040_52, () => {
        let a_72, r_125;
        let rNeg90;
        const rad_55 = 0.017453292519943295 * -90;
        rNeg90 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_55), Math.cos(rad_55));
        let result_10;
        let copyOfStruct_14;
        let copyOfStruct_13;
        const r_121 = rNeg90;
        const ro_22 = rNeg90;
        copyOfStruct_13 = Rotation2D_$ctor_7B00E9A0((r_121.Sin * ro_22.Cos) + (r_121.Cos * ro_22.Sin), (r_121.Cos * ro_22.Cos) - (r_121.Sin * ro_22.Sin));
        const r_122 = copyOfStruct_13;
        const ro_23 = rNeg90;
        copyOfStruct_14 = Rotation2D_$ctor_7B00E9A0((r_122.Sin * ro_23.Cos) + (r_122.Cos * ro_23.Sin), (r_122.Cos * ro_23.Cos) - (r_122.Sin * ro_23.Sin));
        const r_123 = copyOfStruct_14;
        const ro_24 = rNeg90;
        result_10 = Rotation2D_$ctor_7B00E9A0((r_123.Sin * ro_24.Cos) + (r_123.Cos * ro_24.Sin), (r_123.Cos * ro_24.Cos) - (r_123.Sin * ro_24.Sin));
        Expect_isTrue((a_72 = (57.29577951308232 * ((r_125 = result_10, Math.atan2(r_125.Sin, r_125.Cos)))), Math.abs(a_72 - 0) < 1E-09))("Four -90° rotations should return to 0°");
        Test_TestCaseBuilder__Zero(builder$0040_52);
    }));
})(), (() => {
    const builder$0040_53 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("100° + 100° + 100° + 100° = 40° (wraps counterclockwise)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_53, Test_TestCaseBuilder__Delay_1505(builder$0040_53, () => {
        let a_73, r_130;
        let r100;
        const rad_56 = 0.017453292519943295 * 100;
        r100 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_56), Math.cos(rad_56));
        let result_11;
        let copyOfStruct_16;
        let copyOfStruct_15;
        const r_126 = r100;
        const ro_25 = r100;
        copyOfStruct_15 = Rotation2D_$ctor_7B00E9A0((r_126.Sin * ro_25.Cos) + (r_126.Cos * ro_25.Sin), (r_126.Cos * ro_25.Cos) - (r_126.Sin * ro_25.Sin));
        const r_127 = copyOfStruct_15;
        const ro_26 = r100;
        copyOfStruct_16 = Rotation2D_$ctor_7B00E9A0((r_127.Sin * ro_26.Cos) + (r_127.Cos * ro_26.Sin), (r_127.Cos * ro_26.Cos) - (r_127.Sin * ro_26.Sin));
        const r_128 = copyOfStruct_16;
        const ro_27 = r100;
        result_11 = Rotation2D_$ctor_7B00E9A0((r_128.Sin * ro_27.Cos) + (r_128.Cos * ro_27.Sin), (r_128.Cos * ro_27.Cos) - (r_128.Sin * ro_27.Sin));
        Expect_isTrue((a_73 = (57.29577951308232 * ((r_130 = result_11, Math.atan2(r_130.Sin, r_130.Cos)))), Math.abs(a_73 - 40) < 1E-09))("Four 100° rotations (400°) should wrap to 40°");
        Test_TestCaseBuilder__Zero(builder$0040_53);
    }));
})(), (() => {
    const builder$0040_54 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Adding 720° worth of rotations: 180° + 180° + 180° + 180° = 0°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_54, Test_TestCaseBuilder__Delay_1505(builder$0040_54, () => {
        let a_74, r_135;
        let r180;
        const rad_57 = 0.017453292519943295 * 180;
        r180 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_57), Math.cos(rad_57));
        let result_12;
        let copyOfStruct_18;
        let copyOfStruct_17;
        const r_131 = r180;
        const ro_28 = r180;
        copyOfStruct_17 = Rotation2D_$ctor_7B00E9A0((r_131.Sin * ro_28.Cos) + (r_131.Cos * ro_28.Sin), (r_131.Cos * ro_28.Cos) - (r_131.Sin * ro_28.Sin));
        const r_132 = copyOfStruct_17;
        const ro_29 = r180;
        copyOfStruct_18 = Rotation2D_$ctor_7B00E9A0((r_132.Sin * ro_29.Cos) + (r_132.Cos * ro_29.Sin), (r_132.Cos * ro_29.Cos) - (r_132.Sin * ro_29.Sin));
        const r_133 = copyOfStruct_18;
        const ro_30 = r180;
        result_12 = Rotation2D_$ctor_7B00E9A0((r_133.Sin * ro_30.Cos) + (r_133.Cos * ro_30.Sin), (r_133.Cos * ro_30.Cos) - (r_133.Sin * ro_30.Sin));
        Expect_isTrue((a_74 = Math.abs(57.29577951308232 * ((r_135 = result_12, Math.atan2(r_135.Sin, r_135.Cos)))), Math.abs(a_74 - 0) < 1E-09))("Four 180° rotations (720°) should return to 0°");
        Test_TestCaseBuilder__Zero(builder$0040_54);
    }));
})(), (() => {
    const builder$0040_55 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Mixing positive and negative: 270° + -90° = 180°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_55, Test_TestCaseBuilder__Delay_1505(builder$0040_55, () => {
        let r_138;
        let r270;
        const rad_58 = 0.017453292519943295 * 270;
        r270 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_58), Math.cos(rad_58));
        let rNeg90_1;
        const rad_59 = 0.017453292519943295 * -90;
        rNeg90_1 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_59), Math.cos(rad_59));
        let result_13;
        const r_136 = r270;
        const ro_31 = rNeg90_1;
        result_13 = Rotation2D_$ctor_7B00E9A0((r_136.Sin * ro_31.Cos) + (r_136.Cos * ro_31.Sin), (r_136.Cos * ro_31.Cos) - (r_136.Sin * ro_31.Sin));
        const deg_57 = Math.abs(57.29577951308232 * ((r_138 = result_13, Math.atan2(r_138.Sin, r_138.Cos))));
        Expect_isTrue(Math.abs(deg_57 - 180) < 1E-09)("270° + -90° should be ±180°");
        Test_TestCaseBuilder__Zero(builder$0040_55);
    }));
})(), (() => {
    const builder$0040_56 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Large positive accumulation: 250° + 250° = 140°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_56, Test_TestCaseBuilder__Delay_1505(builder$0040_56, () => {
        let a_76, r_141;
        let r250;
        const rad_60 = 0.017453292519943295 * 250;
        r250 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_60), Math.cos(rad_60));
        let result_14;
        const r_139 = r250;
        const ro_32 = r250;
        result_14 = Rotation2D_$ctor_7B00E9A0((r_139.Sin * ro_32.Cos) + (r_139.Cos * ro_32.Sin), (r_139.Cos * ro_32.Cos) - (r_139.Sin * ro_32.Sin));
        Expect_isTrue((a_76 = (57.29577951308232 * ((r_141 = result_14, Math.atan2(r_141.Sin, r_141.Cos)))), Math.abs(a_76 - 140) < 1E-09))("500° should wrap to 140° (500 - 360 )");
        Test_TestCaseBuilder__Zero(builder$0040_56);
    }));
})(), (() => {
    const builder$0040_57 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Large negative accumulation: -250° + -250° = -140°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_57, Test_TestCaseBuilder__Delay_1505(builder$0040_57, () => {
        let a_77, r_144;
        let rNeg250;
        const rad_61 = 0.017453292519943295 * -250;
        rNeg250 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_61), Math.cos(rad_61));
        let result_15;
        const r_142 = rNeg250;
        const ro_33 = rNeg250;
        result_15 = Rotation2D_$ctor_7B00E9A0((r_142.Sin * ro_33.Cos) + (r_142.Cos * ro_33.Sin), (r_142.Cos * ro_33.Cos) - (r_142.Sin * ro_33.Sin));
        Expect_isTrue((a_77 = (57.29577951308232 * ((r_144 = result_15, Math.atan2(r_144.Sin, r_144.Cos)))), Math.abs(a_77 - -140) < 1E-09))("-500° should wrap to -140° (-500 + 360 )");
        Test_TestCaseBuilder__Zero(builder$0040_57);
    }));
})(), (() => {
    const builder$0040_58 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("135° added three times = 45° (wraps once)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_58, Test_TestCaseBuilder__Delay_1505(builder$0040_58, () => {
        let a_78, r_148;
        let r135;
        const rad_62 = 0.017453292519943295 * 135;
        r135 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_62), Math.cos(rad_62));
        let result_16;
        let copyOfStruct_19;
        const r_145 = r135;
        const ro_34 = r135;
        copyOfStruct_19 = Rotation2D_$ctor_7B00E9A0((r_145.Sin * ro_34.Cos) + (r_145.Cos * ro_34.Sin), (r_145.Cos * ro_34.Cos) - (r_145.Sin * ro_34.Sin));
        const r_146 = copyOfStruct_19;
        const ro_35 = r135;
        result_16 = Rotation2D_$ctor_7B00E9A0((r_146.Sin * ro_35.Cos) + (r_146.Cos * ro_35.Sin), (r_146.Cos * ro_35.Cos) - (r_146.Sin * ro_35.Sin));
        Expect_isTrue((a_78 = (57.29577951308232 * ((r_148 = result_16, Math.atan2(r_148.Sin, r_148.Cos)))), Math.abs(a_78 - 45) < 1E-09))("Three 135° rotations (405°) should wrap to 45°");
        Test_TestCaseBuilder__Zero(builder$0040_58);
    }));
})(), (() => {
    const builder$0040_59 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("-135° added three times = -45° (wraps once clockwise)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_59, Test_TestCaseBuilder__Delay_1505(builder$0040_59, () => {
        let a_79, r_152;
        let rNeg135;
        const rad_63 = 0.017453292519943295 * -135;
        rNeg135 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_63), Math.cos(rad_63));
        let result_17;
        let copyOfStruct_20;
        const r_149 = rNeg135;
        const ro_36 = rNeg135;
        copyOfStruct_20 = Rotation2D_$ctor_7B00E9A0((r_149.Sin * ro_36.Cos) + (r_149.Cos * ro_36.Sin), (r_149.Cos * ro_36.Cos) - (r_149.Sin * ro_36.Sin));
        const r_150 = copyOfStruct_20;
        const ro_37 = rNeg135;
        result_17 = Rotation2D_$ctor_7B00E9A0((r_150.Sin * ro_37.Cos) + (r_150.Cos * ro_37.Sin), (r_150.Cos * ro_37.Cos) - (r_150.Sin * ro_37.Sin));
        Expect_isTrue((a_79 = (57.29577951308232 * ((r_152 = result_17, Math.atan2(r_152.Sin, r_152.Cos)))), Math.abs(a_79 - -45) < 1E-09))("Three -135° rotations (-405°) should wrap to -45°");
        Test_TestCaseBuilder__Zero(builder$0040_59);
    }));
})(), (() => {
    const builder$0040_60 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Very large accumulation: 1000° equivalent to 280°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_60, Test_TestCaseBuilder__Delay_1505(builder$0040_60, () => {
        let a_81, b_81;
        let r1000;
        const rad_64 = 0.017453292519943295 * 1000;
        r1000 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_64), Math.cos(rad_64));
        let r280;
        const rad_65 = 0.017453292519943295 * 280;
        r280 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_65), Math.cos(rad_65));
        Expect_isTrue((a_81 = r1000, (b_81 = r280, (Math.abs(a_81.Sin - b_81.Sin) <= 1E-09) && (Math.abs(a_81.Cos - b_81.Cos) <= 1E-09))))("1000° (2 full rotations + 280°) should equal 280°");
        Test_TestCaseBuilder__Zero(builder$0040_60);
    }));
})(), (() => {
    const builder$0040_61 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Very large negative accumulation: -1000° equivalent to -280°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_61, Test_TestCaseBuilder__Delay_1505(builder$0040_61, () => {
        let a_83, b_83;
        let rNeg1000;
        const rad_66 = 0.017453292519943295 * -1000;
        rNeg1000 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_66), Math.cos(rad_66));
        let rNeg280;
        const rad_67 = 0.017453292519943295 * -280;
        rNeg280 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_67), Math.cos(rad_67));
        Expect_isTrue((a_83 = rNeg1000, (b_83 = rNeg280, (Math.abs(a_83.Sin - b_83.Sin) <= 1E-09) && (Math.abs(a_83.Cos - b_83.Cos) <= 1E-09))))("-1000° should equal -280°");
        Test_TestCaseBuilder__Zero(builder$0040_61);
    }));
})(), (() => {
    const builder$0040_62 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Accumulated rotation maintains precision over many adds", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_62, Test_TestCaseBuilder__Delay_1505(builder$0040_62, () => {
        let r36;
        const rad_68 = 0.017453292519943295 * 36;
        r36 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_68), Math.cos(rad_68));
        let result_18 = r36;
        Test_TestCaseBuilder__Combine_3A59D1F3(builder$0040_62, Test_TestCaseBuilder__For_Z371464DD(builder$0040_62, rangeDouble(1, 1, 9), (_arg) => {
            let r_153, ro_38;
            const _i = _arg | 0;
            result_18 = ((r_153 = result_18, (ro_38 = r36, Rotation2D_$ctor_7B00E9A0((r_153.Sin * ro_38.Cos) + (r_153.Cos * ro_38.Sin), (r_153.Cos * ro_38.Cos) - (r_153.Sin * ro_38.Sin)))));
            Test_TestCaseBuilder__Zero(builder$0040_62);
        }), Test_TestCaseBuilder__Delay_1505(builder$0040_62, () => {
            let a_84, r_155;
            Expect_isTrue((a_84 = (57.29577951308232 * ((r_155 = result_18, Math.atan2(r_155.Sin, r_155.Cos)))), Math.abs(a_84 - 0) < 1E-09))("Ten 36° rotations should accumulate to 360° = 0°");
            Test_TestCaseBuilder__Zero(builder$0040_62);
        }));
    }));
})(), (() => {
    const builder$0040_63 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("AddDegrees with accumulation beyond 360°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_63, Test_TestCaseBuilder__Delay_1505(builder$0040_63, () => {
        let a_85, r_164;
        let r_156;
        const rad_69 = 0.017453292519943295 * 100;
        r_156 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_69), Math.cos(rad_69));
        let result_19;
        let copyOfStruct_22;
        let copyOfStruct_21;
        const r_158 = r_156;
        let ro_39;
        const rad_70 = 0.017453292519943295 * 100;
        ro_39 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_70), Math.cos(rad_70));
        copyOfStruct_21 = Rotation2D_$ctor_7B00E9A0((r_158.Sin * ro_39.Cos) + (r_158.Cos * ro_39.Sin), (r_158.Cos * ro_39.Cos) - (r_158.Sin * ro_39.Sin));
        const r_160 = copyOfStruct_21;
        let ro_40;
        const rad_71 = 0.017453292519943295 * 100;
        ro_40 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_71), Math.cos(rad_71));
        copyOfStruct_22 = Rotation2D_$ctor_7B00E9A0((r_160.Sin * ro_40.Cos) + (r_160.Cos * ro_40.Sin), (r_160.Cos * ro_40.Cos) - (r_160.Sin * ro_40.Sin));
        const r_162 = copyOfStruct_22;
        let ro_41;
        const rad_72 = 0.017453292519943295 * 100;
        ro_41 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_72), Math.cos(rad_72));
        result_19 = Rotation2D_$ctor_7B00E9A0((r_162.Sin * ro_41.Cos) + (r_162.Cos * ro_41.Sin), (r_162.Cos * ro_41.Cos) - (r_162.Sin * ro_41.Sin));
        Expect_isTrue((a_85 = (57.29577951308232 * ((r_164 = result_19, Math.atan2(r_164.Sin, r_164.Cos)))), Math.abs(a_85 - 40) < 1E-09))("100° + 300° via AddDegrees should be 40°");
        Test_TestCaseBuilder__Zero(builder$0040_63);
    }));
})(), (() => {
    const builder$0040_64 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("AddRadians with accumulation beyond 2π", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_64, Test_TestCaseBuilder__Delay_1505(builder$0040_64, () => {
        let a_86, r_172;
        let r_165;
        const rad_73 = 3.141592653589793 / 2;
        r_165 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_73), Math.cos(rad_73));
        let result_20;
        let copyOfStruct_24;
        let copyOfStruct_23;
        const r_167 = r_165;
        let ro_42;
        const rad_75 = 3.141592653589793 / 2;
        ro_42 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_75), Math.cos(rad_75));
        copyOfStruct_23 = Rotation2D_$ctor_7B00E9A0((r_167.Sin * ro_42.Cos) + (r_167.Cos * ro_42.Sin), (r_167.Cos * ro_42.Cos) - (r_167.Sin * ro_42.Sin));
        const r_169 = copyOfStruct_23;
        let ro_43;
        const rad_77 = 3.141592653589793 / 2;
        ro_43 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_77), Math.cos(rad_77));
        copyOfStruct_24 = Rotation2D_$ctor_7B00E9A0((r_169.Sin * ro_43.Cos) + (r_169.Cos * ro_43.Sin), (r_169.Cos * ro_43.Cos) - (r_169.Sin * ro_43.Sin));
        const r_171 = copyOfStruct_24;
        let ro_44;
        const rad_79 = 3.141592653589793 / 2;
        ro_44 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_79), Math.cos(rad_79));
        result_20 = Rotation2D_$ctor_7B00E9A0((r_171.Sin * ro_44.Cos) + (r_171.Cos * ro_44.Sin), (r_171.Cos * ro_44.Cos) - (r_171.Sin * ro_44.Sin));
        Expect_isTrue((a_86 = ((r_172 = result_20, Math.atan2(r_172.Sin, r_172.Cos))), Math.abs(a_86 - 0) < 1E-09))("Four π/2 rotations via AddRadians should be 0");
        Test_TestCaseBuilder__Zero(builder$0040_64);
    }));
})()])), Test_testList("Half rotation - point transformation tests", ofArray([(() => {
    const builder$0040_65 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Half of 0° transforms point correctly", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_65, Test_TestCaseBuilder__Delay_1505(builder$0040_65, () => {
        let a_88, b_88, vx, vy;
        const testPt = Pt_$ctor_7B00E9A0(3, 4);
        let r_173;
        const rad_80 = 0.017453292519943295 * 0;
        r_173 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_80), Math.cos(rad_80));
        let half;
        const r_174 = r_173;
        const halfCos = Math.sqrt((1 + r_174.Cos) / 2);
        const halfSin = Math.sqrt((1 - r_174.Cos) / 2);
        const halfSinSigned = (r_174.Sin < 0) ? -halfSin : halfSin;
        half = Rotation2D_$ctor_7B00E9A0(halfSinSigned, halfCos);
        let fullResult;
        const r_176 = r_173;
        const p_1 = testPt;
        fullResult = Pt_$ctor_7B00E9A0_1((r_176.Cos * p_1.X) - (r_176.Sin * p_1.Y), (r_176.Sin * p_1.X) + (r_176.Cos * p_1.Y));
        let halfTwiceResult;
        const r_180 = half;
        let p_5;
        const r_178 = half;
        const p_3 = testPt;
        p_5 = Pt_$ctor_7B00E9A0_1((r_178.Cos * p_3.X) - (r_178.Sin * p_3.Y), (r_178.Sin * p_3.X) + (r_178.Cos * p_3.Y));
        halfTwiceResult = Pt_$ctor_7B00E9A0_1((r_180.Cos * p_5.X) - (r_180.Sin * p_5.Y), (r_180.Sin * p_5.X) + (r_180.Cos * p_5.Y));
        Expect_isTrue(((a_88 = fullResult, (b_88 = halfTwiceResult, (vx = (a_88.X - b_88.X), (vy = (a_88.Y - b_88.Y), Math.sqrt((vx * vx) + (vy * vy))))))) < 1E-09)("Rotating by half twice should equal full rotation for 0°");
        Test_TestCaseBuilder__Zero(builder$0040_65);
    }));
})(), (() => {
    const builder$0040_66 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Half of 30° transforms point correctly", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_66, Test_TestCaseBuilder__Delay_1505(builder$0040_66, () => {
        let a_90, b_90, vx_1, vy_1;
        const testPt_1 = Pt_$ctor_7B00E9A0(3, 4);
        let r_181;
        const rad_81 = 0.017453292519943295 * 30;
        r_181 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_81), Math.cos(rad_81));
        let half_1;
        const r_182 = r_181;
        const halfCos_1 = Math.sqrt((1 + r_182.Cos) / 2);
        const halfSin_1 = Math.sqrt((1 - r_182.Cos) / 2);
        const halfSinSigned_1 = (r_182.Sin < 0) ? -halfSin_1 : halfSin_1;
        half_1 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_1, halfCos_1);
        let fullResult_1;
        const r_184 = r_181;
        const p_7 = testPt_1;
        fullResult_1 = Pt_$ctor_7B00E9A0_1((r_184.Cos * p_7.X) - (r_184.Sin * p_7.Y), (r_184.Sin * p_7.X) + (r_184.Cos * p_7.Y));
        let halfTwiceResult_1;
        const r_188 = half_1;
        let p_11;
        const r_186 = half_1;
        const p_9 = testPt_1;
        p_11 = Pt_$ctor_7B00E9A0_1((r_186.Cos * p_9.X) - (r_186.Sin * p_9.Y), (r_186.Sin * p_9.X) + (r_186.Cos * p_9.Y));
        halfTwiceResult_1 = Pt_$ctor_7B00E9A0_1((r_188.Cos * p_11.X) - (r_188.Sin * p_11.Y), (r_188.Sin * p_11.X) + (r_188.Cos * p_11.Y));
        Expect_isTrue(((a_90 = fullResult_1, (b_90 = halfTwiceResult_1, (vx_1 = (a_90.X - b_90.X), (vy_1 = (a_90.Y - b_90.Y), Math.sqrt((vx_1 * vx_1) + (vy_1 * vy_1))))))) < 1E-09)("Rotating by half twice should equal full rotation for 30°");
        Test_TestCaseBuilder__Zero(builder$0040_66);
    }));
})(), (() => {
    const builder$0040_67 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Half of 120° transforms point correctly", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_67, Test_TestCaseBuilder__Delay_1505(builder$0040_67, () => {
        let a_92, b_92, vx_2, vy_2;
        const testPt_2 = Pt_$ctor_7B00E9A0(3, 4);
        let r_189;
        const rad_82 = 0.017453292519943295 * 120;
        r_189 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_82), Math.cos(rad_82));
        let half_2;
        const r_190 = r_189;
        const halfCos_2 = Math.sqrt((1 + r_190.Cos) / 2);
        const halfSin_2 = Math.sqrt((1 - r_190.Cos) / 2);
        const halfSinSigned_2 = (r_190.Sin < 0) ? -halfSin_2 : halfSin_2;
        half_2 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_2, halfCos_2);
        let fullResult_2;
        const r_192 = r_189;
        const p_13 = testPt_2;
        fullResult_2 = Pt_$ctor_7B00E9A0_1((r_192.Cos * p_13.X) - (r_192.Sin * p_13.Y), (r_192.Sin * p_13.X) + (r_192.Cos * p_13.Y));
        let halfTwiceResult_2;
        const r_196 = half_2;
        let p_17;
        const r_194 = half_2;
        const p_15 = testPt_2;
        p_17 = Pt_$ctor_7B00E9A0_1((r_194.Cos * p_15.X) - (r_194.Sin * p_15.Y), (r_194.Sin * p_15.X) + (r_194.Cos * p_15.Y));
        halfTwiceResult_2 = Pt_$ctor_7B00E9A0_1((r_196.Cos * p_17.X) - (r_196.Sin * p_17.Y), (r_196.Sin * p_17.X) + (r_196.Cos * p_17.Y));
        Expect_isTrue(((a_92 = fullResult_2, (b_92 = halfTwiceResult_2, (vx_2 = (a_92.X - b_92.X), (vy_2 = (a_92.Y - b_92.Y), Math.sqrt((vx_2 * vx_2) + (vy_2 * vy_2))))))) < 1E-09)("Rotating by half twice should equal full rotation for 120°");
        Test_TestCaseBuilder__Zero(builder$0040_67);
    }));
})(), (() => {
    const builder$0040_68 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Half of -60° transforms point correctly", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_68, Test_TestCaseBuilder__Delay_1505(builder$0040_68, () => {
        let a_94, b_94, vx_3, vy_3;
        const testPt_3 = Pt_$ctor_7B00E9A0(3, 4);
        let r_197;
        const rad_83 = 0.017453292519943295 * -60;
        r_197 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_83), Math.cos(rad_83));
        let half_3;
        const r_198 = r_197;
        const halfCos_3 = Math.sqrt((1 + r_198.Cos) / 2);
        const halfSin_3 = Math.sqrt((1 - r_198.Cos) / 2);
        const halfSinSigned_3 = (r_198.Sin < 0) ? -halfSin_3 : halfSin_3;
        half_3 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_3, halfCos_3);
        let fullResult_3;
        const r_200 = r_197;
        const p_19 = testPt_3;
        fullResult_3 = Pt_$ctor_7B00E9A0_1((r_200.Cos * p_19.X) - (r_200.Sin * p_19.Y), (r_200.Sin * p_19.X) + (r_200.Cos * p_19.Y));
        let halfTwiceResult_3;
        const r_204 = half_3;
        let p_23;
        const r_202 = half_3;
        const p_21 = testPt_3;
        p_23 = Pt_$ctor_7B00E9A0_1((r_202.Cos * p_21.X) - (r_202.Sin * p_21.Y), (r_202.Sin * p_21.X) + (r_202.Cos * p_21.Y));
        halfTwiceResult_3 = Pt_$ctor_7B00E9A0_1((r_204.Cos * p_23.X) - (r_204.Sin * p_23.Y), (r_204.Sin * p_23.X) + (r_204.Cos * p_23.Y));
        Expect_isTrue(((a_94 = fullResult_3, (b_94 = halfTwiceResult_3, (vx_3 = (a_94.X - b_94.X), (vy_3 = (a_94.Y - b_94.Y), Math.sqrt((vx_3 * vx_3) + (vy_3 * vy_3))))))) < 1E-09)("Rotating by half twice should equal full rotation for -60°");
        Test_TestCaseBuilder__Zero(builder$0040_68);
    }));
})(), (() => {
    const builder$0040_69 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Half of -15° transforms point correctly", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_69, Test_TestCaseBuilder__Delay_1505(builder$0040_69, () => {
        let a_96, b_96, vx_4, vy_4;
        const testPt_4 = Pt_$ctor_7B00E9A0(3, 4);
        let r_205;
        const rad_84 = 0.017453292519943295 * -15;
        r_205 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_84), Math.cos(rad_84));
        let half_4;
        const r_206 = r_205;
        const halfCos_4 = Math.sqrt((1 + r_206.Cos) / 2);
        const halfSin_4 = Math.sqrt((1 - r_206.Cos) / 2);
        const halfSinSigned_4 = (r_206.Sin < 0) ? -halfSin_4 : halfSin_4;
        half_4 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_4, halfCos_4);
        let fullResult_4;
        const r_208 = r_205;
        const p_25 = testPt_4;
        fullResult_4 = Pt_$ctor_7B00E9A0_1((r_208.Cos * p_25.X) - (r_208.Sin * p_25.Y), (r_208.Sin * p_25.X) + (r_208.Cos * p_25.Y));
        let halfTwiceResult_4;
        const r_212 = half_4;
        let p_29;
        const r_210 = half_4;
        const p_27 = testPt_4;
        p_29 = Pt_$ctor_7B00E9A0_1((r_210.Cos * p_27.X) - (r_210.Sin * p_27.Y), (r_210.Sin * p_27.X) + (r_210.Cos * p_27.Y));
        halfTwiceResult_4 = Pt_$ctor_7B00E9A0_1((r_212.Cos * p_29.X) - (r_212.Sin * p_29.Y), (r_212.Sin * p_29.X) + (r_212.Cos * p_29.Y));
        Expect_isTrue(((a_96 = fullResult_4, (b_96 = halfTwiceResult_4, (vx_4 = (a_96.X - b_96.X), (vy_4 = (a_96.Y - b_96.Y), Math.sqrt((vx_4 * vx_4) + (vy_4 * vy_4))))))) < 1E-09)("Rotating by half twice should equal full rotation for -15°");
        Test_TestCaseBuilder__Zero(builder$0040_69);
    }));
})(), (() => {
    const builder$0040_70 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Half of -0.1° transforms point correctly", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_70, Test_TestCaseBuilder__Delay_1505(builder$0040_70, () => {
        let a_98, b_98, vx_5, vy_5;
        const testPt_5 = Pt_$ctor_7B00E9A0(3, 4);
        let r_213;
        const rad_85 = 0.017453292519943295 * -0.1;
        r_213 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_85), Math.cos(rad_85));
        let half_5;
        const r_214 = r_213;
        const halfCos_5 = Math.sqrt((1 + r_214.Cos) / 2);
        const halfSin_5 = Math.sqrt((1 - r_214.Cos) / 2);
        const halfSinSigned_5 = (r_214.Sin < 0) ? -halfSin_5 : halfSin_5;
        half_5 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_5, halfCos_5);
        let fullResult_5;
        const r_216 = r_213;
        const p_31 = testPt_5;
        fullResult_5 = Pt_$ctor_7B00E9A0_1((r_216.Cos * p_31.X) - (r_216.Sin * p_31.Y), (r_216.Sin * p_31.X) + (r_216.Cos * p_31.Y));
        let halfTwiceResult_5;
        const r_220 = half_5;
        let p_35;
        const r_218 = half_5;
        const p_33 = testPt_5;
        p_35 = Pt_$ctor_7B00E9A0_1((r_218.Cos * p_33.X) - (r_218.Sin * p_33.Y), (r_218.Sin * p_33.X) + (r_218.Cos * p_33.Y));
        halfTwiceResult_5 = Pt_$ctor_7B00E9A0_1((r_220.Cos * p_35.X) - (r_220.Sin * p_35.Y), (r_220.Sin * p_35.X) + (r_220.Cos * p_35.Y));
        Expect_isTrue(((a_98 = fullResult_5, (b_98 = halfTwiceResult_5, (vx_5 = (a_98.X - b_98.X), (vy_5 = (a_98.Y - b_98.Y), Math.sqrt((vx_5 * vx_5) + (vy_5 * vy_5))))))) < 1E-09)("Rotating by half twice should equal full rotation for -0.1°");
        Test_TestCaseBuilder__Zero(builder$0040_70);
    }));
})(), (() => {
    const builder$0040_71 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Half of 89° transforms point correctly", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_71, Test_TestCaseBuilder__Delay_1505(builder$0040_71, () => {
        let a_100, b_100, vx_6, vy_6;
        const testPt_6 = Pt_$ctor_7B00E9A0(3, 4);
        let r_221;
        const rad_86 = 0.017453292519943295 * 89;
        r_221 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_86), Math.cos(rad_86));
        let half_6;
        const r_222 = r_221;
        const halfCos_6 = Math.sqrt((1 + r_222.Cos) / 2);
        const halfSin_6 = Math.sqrt((1 - r_222.Cos) / 2);
        const halfSinSigned_6 = (r_222.Sin < 0) ? -halfSin_6 : halfSin_6;
        half_6 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_6, halfCos_6);
        let fullResult_6;
        const r_224 = r_221;
        const p_37 = testPt_6;
        fullResult_6 = Pt_$ctor_7B00E9A0_1((r_224.Cos * p_37.X) - (r_224.Sin * p_37.Y), (r_224.Sin * p_37.X) + (r_224.Cos * p_37.Y));
        let halfTwiceResult_6;
        const r_228 = half_6;
        let p_41;
        const r_226 = half_6;
        const p_39 = testPt_6;
        p_41 = Pt_$ctor_7B00E9A0_1((r_226.Cos * p_39.X) - (r_226.Sin * p_39.Y), (r_226.Sin * p_39.X) + (r_226.Cos * p_39.Y));
        halfTwiceResult_6 = Pt_$ctor_7B00E9A0_1((r_228.Cos * p_41.X) - (r_228.Sin * p_41.Y), (r_228.Sin * p_41.X) + (r_228.Cos * p_41.Y));
        Expect_isTrue(((a_100 = fullResult_6, (b_100 = halfTwiceResult_6, (vx_6 = (a_100.X - b_100.X), (vy_6 = (a_100.Y - b_100.Y), Math.sqrt((vx_6 * vx_6) + (vy_6 * vy_6))))))) < 1E-09)("Rotating by half twice should equal full rotation for 89°");
        Test_TestCaseBuilder__Zero(builder$0040_71);
    }));
})(), (() => {
    const builder$0040_72 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Half of 91° transforms point correctly", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_72, Test_TestCaseBuilder__Delay_1505(builder$0040_72, () => {
        let a_102, b_102, vx_7, vy_7;
        const testPt_7 = Pt_$ctor_7B00E9A0(3, 4);
        let r_229;
        const rad_87 = 0.017453292519943295 * 91;
        r_229 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_87), Math.cos(rad_87));
        let half_7;
        const r_230 = r_229;
        const halfCos_7 = Math.sqrt((1 + r_230.Cos) / 2);
        const halfSin_7 = Math.sqrt((1 - r_230.Cos) / 2);
        const halfSinSigned_7 = (r_230.Sin < 0) ? -halfSin_7 : halfSin_7;
        half_7 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_7, halfCos_7);
        let fullResult_7;
        const r_232 = r_229;
        const p_43 = testPt_7;
        fullResult_7 = Pt_$ctor_7B00E9A0_1((r_232.Cos * p_43.X) - (r_232.Sin * p_43.Y), (r_232.Sin * p_43.X) + (r_232.Cos * p_43.Y));
        let halfTwiceResult_7;
        const r_236 = half_7;
        let p_47;
        const r_234 = half_7;
        const p_45 = testPt_7;
        p_47 = Pt_$ctor_7B00E9A0_1((r_234.Cos * p_45.X) - (r_234.Sin * p_45.Y), (r_234.Sin * p_45.X) + (r_234.Cos * p_45.Y));
        halfTwiceResult_7 = Pt_$ctor_7B00E9A0_1((r_236.Cos * p_47.X) - (r_236.Sin * p_47.Y), (r_236.Sin * p_47.X) + (r_236.Cos * p_47.Y));
        Expect_isTrue(((a_102 = fullResult_7, (b_102 = halfTwiceResult_7, (vx_7 = (a_102.X - b_102.X), (vy_7 = (a_102.Y - b_102.Y), Math.sqrt((vx_7 * vx_7) + (vy_7 * vy_7))))))) < 1E-09)("Rotating by half twice should equal full rotation for 91°");
        Test_TestCaseBuilder__Zero(builder$0040_72);
    }));
})(), (() => {
    const builder$0040_73 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Half of 179° transforms point correctly", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_73, Test_TestCaseBuilder__Delay_1505(builder$0040_73, () => {
        let a_104, b_104, vx_8, vy_8;
        const testPt_8 = Pt_$ctor_7B00E9A0(3, 4);
        let r_237;
        const rad_88 = 0.017453292519943295 * 179;
        r_237 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_88), Math.cos(rad_88));
        let half_8;
        const r_238 = r_237;
        const halfCos_8 = Math.sqrt((1 + r_238.Cos) / 2);
        const halfSin_8 = Math.sqrt((1 - r_238.Cos) / 2);
        const halfSinSigned_8 = (r_238.Sin < 0) ? -halfSin_8 : halfSin_8;
        half_8 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_8, halfCos_8);
        let fullResult_8;
        const r_240 = r_237;
        const p_49 = testPt_8;
        fullResult_8 = Pt_$ctor_7B00E9A0_1((r_240.Cos * p_49.X) - (r_240.Sin * p_49.Y), (r_240.Sin * p_49.X) + (r_240.Cos * p_49.Y));
        let halfTwiceResult_8;
        const r_244 = half_8;
        let p_53;
        const r_242 = half_8;
        const p_51 = testPt_8;
        p_53 = Pt_$ctor_7B00E9A0_1((r_242.Cos * p_51.X) - (r_242.Sin * p_51.Y), (r_242.Sin * p_51.X) + (r_242.Cos * p_51.Y));
        halfTwiceResult_8 = Pt_$ctor_7B00E9A0_1((r_244.Cos * p_53.X) - (r_244.Sin * p_53.Y), (r_244.Sin * p_53.X) + (r_244.Cos * p_53.Y));
        Expect_isTrue(((a_104 = fullResult_8, (b_104 = halfTwiceResult_8, (vx_8 = (a_104.X - b_104.X), (vy_8 = (a_104.Y - b_104.Y), Math.sqrt((vx_8 * vx_8) + (vy_8 * vy_8))))))) < 1E-09)("Rotating by half twice should equal full rotation for 179°");
        Test_TestCaseBuilder__Zero(builder$0040_73);
    }));
})(), (() => {
    const builder$0040_74 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Half of -179° transforms point correctly", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_74, Test_TestCaseBuilder__Delay_1505(builder$0040_74, () => {
        let a_106, b_106, vx_9, vy_9;
        const testPt_9 = Pt_$ctor_7B00E9A0(3, 4);
        let r_245;
        const rad_89 = 0.017453292519943295 * -179;
        r_245 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_89), Math.cos(rad_89));
        let half_9;
        const r_246 = r_245;
        const halfCos_9 = Math.sqrt((1 + r_246.Cos) / 2);
        const halfSin_9 = Math.sqrt((1 - r_246.Cos) / 2);
        const halfSinSigned_9 = (r_246.Sin < 0) ? -halfSin_9 : halfSin_9;
        half_9 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_9, halfCos_9);
        let fullResult_9;
        const r_248 = r_245;
        const p_55 = testPt_9;
        fullResult_9 = Pt_$ctor_7B00E9A0_1((r_248.Cos * p_55.X) - (r_248.Sin * p_55.Y), (r_248.Sin * p_55.X) + (r_248.Cos * p_55.Y));
        let halfTwiceResult_9;
        const r_252 = half_9;
        let p_59;
        const r_250 = half_9;
        const p_57 = testPt_9;
        p_59 = Pt_$ctor_7B00E9A0_1((r_250.Cos * p_57.X) - (r_250.Sin * p_57.Y), (r_250.Sin * p_57.X) + (r_250.Cos * p_57.Y));
        halfTwiceResult_9 = Pt_$ctor_7B00E9A0_1((r_252.Cos * p_59.X) - (r_252.Sin * p_59.Y), (r_252.Sin * p_59.X) + (r_252.Cos * p_59.Y));
        Expect_isTrue(((a_106 = fullResult_9, (b_106 = halfTwiceResult_9, (vx_9 = (a_106.X - b_106.X), (vy_9 = (a_106.Y - b_106.Y), Math.sqrt((vx_9 * vx_9) + (vy_9 * vy_9))))))) < 1E-09)("Rotating by half twice should equal full rotation for -179°");
        Test_TestCaseBuilder__Zero(builder$0040_74);
    }));
})(), (() => {
    const builder$0040_75 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Half of π/2 rad transforms point correctly", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_75, Test_TestCaseBuilder__Delay_1505(builder$0040_75, () => {
        let a_108, b_108, vx_10, vy_10;
        const testPt_10 = Pt_$ctor_7B00E9A0(3, 4);
        let r_253;
        const rad_90 = 3.141592653589793 / 2;
        r_253 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_90), Math.cos(rad_90));
        let half_10;
        const r_254 = r_253;
        const halfCos_10 = Math.sqrt((1 + r_254.Cos) / 2);
        const halfSin_10 = Math.sqrt((1 - r_254.Cos) / 2);
        const halfSinSigned_10 = (r_254.Sin < 0) ? -halfSin_10 : halfSin_10;
        half_10 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_10, halfCos_10);
        let fullResult_10;
        const r_256 = r_253;
        const p_61 = testPt_10;
        fullResult_10 = Pt_$ctor_7B00E9A0_1((r_256.Cos * p_61.X) - (r_256.Sin * p_61.Y), (r_256.Sin * p_61.X) + (r_256.Cos * p_61.Y));
        let halfTwiceResult_10;
        const r_260 = half_10;
        let p_65;
        const r_258 = half_10;
        const p_63 = testPt_10;
        p_65 = Pt_$ctor_7B00E9A0_1((r_258.Cos * p_63.X) - (r_258.Sin * p_63.Y), (r_258.Sin * p_63.X) + (r_258.Cos * p_63.Y));
        halfTwiceResult_10 = Pt_$ctor_7B00E9A0_1((r_260.Cos * p_65.X) - (r_260.Sin * p_65.Y), (r_260.Sin * p_65.X) + (r_260.Cos * p_65.Y));
        Expect_isTrue(((a_108 = fullResult_10, (b_108 = halfTwiceResult_10, (vx_10 = (a_108.X - b_108.X), (vy_10 = (a_108.Y - b_108.Y), Math.sqrt((vx_10 * vx_10) + (vy_10 * vy_10))))))) < 1E-09)("Rotating by half twice should equal full rotation for π/2");
        Test_TestCaseBuilder__Zero(builder$0040_75);
    }));
})(), (() => {
    const builder$0040_76 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Half of π rad transforms point correctly", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_76, Test_TestCaseBuilder__Delay_1505(builder$0040_76, () => {
        let a_110, b_110, vx_11, vy_11;
        const testPt_11 = Pt_$ctor_7B00E9A0(3, 4);
        const r_261 = Rotation2D_$ctor_7B00E9A0(Math.sin(3.141592653589793), Math.cos(3.141592653589793));
        let half_11;
        const r_262 = r_261;
        const halfCos_11 = Math.sqrt((1 + r_262.Cos) / 2);
        const halfSin_11 = Math.sqrt((1 - r_262.Cos) / 2);
        const halfSinSigned_11 = (r_262.Sin < 0) ? -halfSin_11 : halfSin_11;
        half_11 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_11, halfCos_11);
        let fullResult_11;
        const r_264 = r_261;
        const p_67 = testPt_11;
        fullResult_11 = Pt_$ctor_7B00E9A0_1((r_264.Cos * p_67.X) - (r_264.Sin * p_67.Y), (r_264.Sin * p_67.X) + (r_264.Cos * p_67.Y));
        let halfTwiceResult_11;
        const r_268 = half_11;
        let p_71;
        const r_266 = half_11;
        const p_69 = testPt_11;
        p_71 = Pt_$ctor_7B00E9A0_1((r_266.Cos * p_69.X) - (r_266.Sin * p_69.Y), (r_266.Sin * p_69.X) + (r_266.Cos * p_69.Y));
        halfTwiceResult_11 = Pt_$ctor_7B00E9A0_1((r_268.Cos * p_71.X) - (r_268.Sin * p_71.Y), (r_268.Sin * p_71.X) + (r_268.Cos * p_71.Y));
        Expect_isTrue(((a_110 = fullResult_11, (b_110 = halfTwiceResult_11, (vx_11 = (a_110.X - b_110.X), (vy_11 = (a_110.Y - b_110.Y), Math.sqrt((vx_11 * vx_11) + (vy_11 * vy_11))))))) < 1E-09)("Rotating by half twice should equal full rotation for π");
        Test_TestCaseBuilder__Zero(builder$0040_76);
    }));
})(), (() => {
    const builder$0040_77 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Half of -π/2 rad transforms point correctly", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_77, Test_TestCaseBuilder__Delay_1505(builder$0040_77, () => {
        let a_112, b_112, vx_12, vy_12;
        const testPt_12 = Pt_$ctor_7B00E9A0(3, 4);
        let r_269;
        const rad_92 = -3.141592653589793 / 2;
        r_269 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_92), Math.cos(rad_92));
        let half_12;
        const r_270 = r_269;
        const halfCos_12 = Math.sqrt((1 + r_270.Cos) / 2);
        const halfSin_12 = Math.sqrt((1 - r_270.Cos) / 2);
        const halfSinSigned_12 = (r_270.Sin < 0) ? -halfSin_12 : halfSin_12;
        half_12 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_12, halfCos_12);
        let fullResult_12;
        const r_272 = r_269;
        const p_73 = testPt_12;
        fullResult_12 = Pt_$ctor_7B00E9A0_1((r_272.Cos * p_73.X) - (r_272.Sin * p_73.Y), (r_272.Sin * p_73.X) + (r_272.Cos * p_73.Y));
        let halfTwiceResult_12;
        const r_276 = half_12;
        let p_77;
        const r_274 = half_12;
        const p_75 = testPt_12;
        p_77 = Pt_$ctor_7B00E9A0_1((r_274.Cos * p_75.X) - (r_274.Sin * p_75.Y), (r_274.Sin * p_75.X) + (r_274.Cos * p_75.Y));
        halfTwiceResult_12 = Pt_$ctor_7B00E9A0_1((r_276.Cos * p_77.X) - (r_276.Sin * p_77.Y), (r_276.Sin * p_77.X) + (r_276.Cos * p_77.Y));
        Expect_isTrue(((a_112 = fullResult_12, (b_112 = halfTwiceResult_12, (vx_12 = (a_112.X - b_112.X), (vy_12 = (a_112.Y - b_112.Y), Math.sqrt((vx_12 * vx_12) + (vy_12 * vy_12))))))) < 1E-09)("Rotating by half twice should equal full rotation for -π/2");
        Test_TestCaseBuilder__Zero(builder$0040_77);
    }));
})(), (() => {
    const builder$0040_78 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Half rotation twice equals full rotation for all angles 0° to 360° in 10° steps", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_78, Test_TestCaseBuilder__Delay_1505(builder$0040_78, () => {
        const testPts = [Pt_$ctor_7B00E9A0(3, 4), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(-2, 5), Pt_$ctor_7B00E9A0(7, -3)];
        const angles = new Float64Array([0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, -10, -20, -30, -40, -50, -60, -70, -80, -90, -100, -110, -120, -130, -140, -150, -160, -170, -180]);
        Test_TestCaseBuilder__For_Z371464DD(builder$0040_78, angles, (_arg_1) => {
            const deg_84 = _arg_1;
            Test_TestCaseBuilder__For_Z371464DD(builder$0040_78, testPts, (_arg_2) => {
                let a_114, b_114, vx_13, vy_13;
                const testPt_13 = _arg_2;
                let r_277;
                const rad_93 = 0.017453292519943295 * deg_84;
                r_277 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_93), Math.cos(rad_93));
                let half_13;
                const r_278 = r_277;
                const halfCos_13 = Math.sqrt((1 + r_278.Cos) / 2);
                const halfSin_13 = Math.sqrt((1 - r_278.Cos) / 2);
                const halfSinSigned_13 = (r_278.Sin < 0) ? -halfSin_13 : halfSin_13;
                half_13 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_13, halfCos_13);
                let fullResult_13;
                const r_280 = r_277;
                const p_79 = testPt_13;
                fullResult_13 = Pt_$ctor_7B00E9A0_1((r_280.Cos * p_79.X) - (r_280.Sin * p_79.Y), (r_280.Sin * p_79.X) + (r_280.Cos * p_79.Y));
                let halfTwiceResult_13;
                const r_284 = half_13;
                let p_83;
                const r_282 = half_13;
                const p_81 = testPt_13;
                p_83 = Pt_$ctor_7B00E9A0_1((r_282.Cos * p_81.X) - (r_282.Sin * p_81.Y), (r_282.Sin * p_81.X) + (r_282.Cos * p_81.Y));
                halfTwiceResult_13 = Pt_$ctor_7B00E9A0_1((r_284.Cos * p_83.X) - (r_284.Sin * p_83.Y), (r_284.Sin * p_83.X) + (r_284.Cos * p_83.Y));
                if (!(((a_114 = fullResult_13, (b_114 = halfTwiceResult_13, (vx_13 = (a_114.X - b_114.X), (vy_13 = (a_114.Y - b_114.Y), Math.sqrt((vx_13 * vx_13) + (vy_13 * vy_13))))))) < 1E-09)) {
                    throw new Exception(`Half rotation twice should equal full rotation for ${deg_84}° at point ${testPt_13}`);
                    Test_TestCaseBuilder__Zero(builder$0040_78);
                }
                else {
                    Test_TestCaseBuilder__Zero(builder$0040_78);
                }
            });
        });
    }));
})(), (() => {
    const builder$0040_79 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Half rotation on vector: half twice equals full for 90°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_79, Test_TestCaseBuilder__Delay_1505(builder$0040_79, () => {
        let v_13, a_115, b_115, x_18, y_15;
        const testVc = Vc_$ctor_7B00E9A0(3, 4);
        let r_285;
        const rad_94 = 0.017453292519943295 * 90;
        r_285 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_94), Math.cos(rad_94));
        let half_14;
        const r_286 = r_285;
        const halfCos_14 = Math.sqrt((1 + r_286.Cos) / 2);
        const halfSin_14 = Math.sqrt((1 - r_286.Cos) / 2);
        const halfSinSigned_14 = (r_286.Sin < 0) ? -halfSin_14 : halfSin_14;
        half_14 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_14, halfCos_14);
        let fullResult_14;
        const r_288 = r_285;
        const v_7 = testVc;
        fullResult_14 = Vc_$ctor_7B00E9A0_1((r_288.Cos * v_7.X) - (r_288.Sin * v_7.Y), (r_288.Sin * v_7.X) + (r_288.Cos * v_7.Y));
        let halfTwiceResult_14;
        const r_292 = half_14;
        let v_11;
        const r_290 = half_14;
        const v_9 = testVc;
        v_11 = Vc_$ctor_7B00E9A0_1((r_290.Cos * v_9.X) - (r_290.Sin * v_9.Y), (r_290.Sin * v_9.X) + (r_290.Cos * v_9.Y));
        halfTwiceResult_14 = Vc_$ctor_7B00E9A0_1((r_292.Cos * v_11.X) - (r_292.Sin * v_11.Y), (r_292.Sin * v_11.X) + (r_292.Cos * v_11.Y));
        Expect_isTrue(((v_13 = ((a_115 = fullResult_14, (b_115 = halfTwiceResult_14, Vc_$ctor_7B00E9A0_2(a_115.X - b_115.X, a_115.Y - b_115.Y)))), (x_18 = v_13.X, (y_15 = v_13.Y, Math.sqrt((x_18 * x_18) + (y_15 * y_15)))))) < 1E-09)("Vector: half twice should equal full for 90°");
        Test_TestCaseBuilder__Zero(builder$0040_79);
    }));
})(), (() => {
    const builder$0040_80 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Half rotation on vector: half twice equals full for 180°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_80, Test_TestCaseBuilder__Delay_1505(builder$0040_80, () => {
        let v_21, a_117, b_117, x_19, y_16;
        const testVc_1 = Vc_$ctor_7B00E9A0(3, 4);
        let r_293;
        const rad_95 = 0.017453292519943295 * 180;
        r_293 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_95), Math.cos(rad_95));
        let half_15;
        const r_294 = r_293;
        const halfCos_15 = Math.sqrt((1 + r_294.Cos) / 2);
        const halfSin_15 = Math.sqrt((1 - r_294.Cos) / 2);
        const halfSinSigned_15 = (r_294.Sin < 0) ? -halfSin_15 : halfSin_15;
        half_15 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_15, halfCos_15);
        let fullResult_15;
        const r_296 = r_293;
        const v_15 = testVc_1;
        fullResult_15 = Vc_$ctor_7B00E9A0_1((r_296.Cos * v_15.X) - (r_296.Sin * v_15.Y), (r_296.Sin * v_15.X) + (r_296.Cos * v_15.Y));
        let halfTwiceResult_15;
        const r_300 = half_15;
        let v_19;
        const r_298 = half_15;
        const v_17 = testVc_1;
        v_19 = Vc_$ctor_7B00E9A0_1((r_298.Cos * v_17.X) - (r_298.Sin * v_17.Y), (r_298.Sin * v_17.X) + (r_298.Cos * v_17.Y));
        halfTwiceResult_15 = Vc_$ctor_7B00E9A0_1((r_300.Cos * v_19.X) - (r_300.Sin * v_19.Y), (r_300.Sin * v_19.X) + (r_300.Cos * v_19.Y));
        Expect_isTrue(((v_21 = ((a_117 = fullResult_15, (b_117 = halfTwiceResult_15, Vc_$ctor_7B00E9A0_2(a_117.X - b_117.X, a_117.Y - b_117.Y)))), (x_19 = v_21.X, (y_16 = v_21.Y, Math.sqrt((x_19 * x_19) + (y_16 * y_16)))))) < 1E-09)("Vector: half twice should equal full for 180°");
        Test_TestCaseBuilder__Zero(builder$0040_80);
    }));
})(), (() => {
    const builder$0040_81 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Half rotation on vector: half twice equals full for -135°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_81, Test_TestCaseBuilder__Delay_1505(builder$0040_81, () => {
        let v_29, a_119, b_119, x_20, y_17;
        const testVc_2 = Vc_$ctor_7B00E9A0(3, 4);
        let r_301;
        const rad_96 = 0.017453292519943295 * -135;
        r_301 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_96), Math.cos(rad_96));
        let half_16;
        const r_302 = r_301;
        const halfCos_16 = Math.sqrt((1 + r_302.Cos) / 2);
        const halfSin_16 = Math.sqrt((1 - r_302.Cos) / 2);
        const halfSinSigned_16 = (r_302.Sin < 0) ? -halfSin_16 : halfSin_16;
        half_16 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_16, halfCos_16);
        let fullResult_16;
        const r_304 = r_301;
        const v_23 = testVc_2;
        fullResult_16 = Vc_$ctor_7B00E9A0_1((r_304.Cos * v_23.X) - (r_304.Sin * v_23.Y), (r_304.Sin * v_23.X) + (r_304.Cos * v_23.Y));
        let halfTwiceResult_16;
        const r_308 = half_16;
        let v_27;
        const r_306 = half_16;
        const v_25 = testVc_2;
        v_27 = Vc_$ctor_7B00E9A0_1((r_306.Cos * v_25.X) - (r_306.Sin * v_25.Y), (r_306.Sin * v_25.X) + (r_306.Cos * v_25.Y));
        halfTwiceResult_16 = Vc_$ctor_7B00E9A0_1((r_308.Cos * v_27.X) - (r_308.Sin * v_27.Y), (r_308.Sin * v_27.X) + (r_308.Cos * v_27.Y));
        Expect_isTrue(((v_29 = ((a_119 = fullResult_16, (b_119 = halfTwiceResult_16, Vc_$ctor_7B00E9A0_2(a_119.X - b_119.X, a_119.Y - b_119.Y)))), (x_20 = v_29.X, (y_17 = v_29.Y, Math.sqrt((x_20 * x_20) + (y_17 * y_17)))))) < 1E-09)("Vector: half twice should equal full for -135°");
        Test_TestCaseBuilder__Zero(builder$0040_81);
    }));
})(), (() => {
    const builder$0040_82 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Applying Half twice gives quarter rotation - verified by point transformation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_82, Test_TestCaseBuilder__Delay_1505(builder$0040_82, () => {
        let a_122, b_122, vx_14, vy_14;
        const testPt_14 = Pt_$ctor_7B00E9A0(1, 0);
        let r_309;
        const rad_97 = 0.017453292519943295 * 180;
        r_309 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_97), Math.cos(rad_97));
        let half_17;
        const r_310 = r_309;
        const halfCos_17 = Math.sqrt((1 + r_310.Cos) / 2);
        const halfSin_17 = Math.sqrt((1 - r_310.Cos) / 2);
        const halfSinSigned_17 = (r_310.Sin < 0) ? -halfSin_17 : halfSin_17;
        half_17 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_17, halfCos_17);
        let quarter;
        const r_311 = half_17;
        const halfCos_18 = Math.sqrt((1 + r_311.Cos) / 2);
        const halfSin_18 = Math.sqrt((1 - r_311.Cos) / 2);
        const halfSinSigned_18 = (r_311.Sin < 0) ? -halfSin_18 : halfSin_18;
        quarter = Rotation2D_$ctor_7B00E9A0(halfSinSigned_18, halfCos_18);
        let expectedPt;
        let r_313;
        const rad_98 = 0.017453292519943295 * 45;
        r_313 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_98), Math.cos(rad_98));
        const p_85 = testPt_14;
        expectedPt = Pt_$ctor_7B00E9A0_1((r_313.Cos * p_85.X) - (r_313.Sin * p_85.Y), (r_313.Sin * p_85.X) + (r_313.Cos * p_85.Y));
        let actualPt;
        const r_315 = quarter;
        const p_87 = testPt_14;
        actualPt = Pt_$ctor_7B00E9A0_1((r_315.Cos * p_87.X) - (r_315.Sin * p_87.Y), (r_315.Sin * p_87.X) + (r_315.Cos * p_87.Y));
        Expect_isTrue(((a_122 = expectedPt, (b_122 = actualPt, (vx_14 = (a_122.X - b_122.X), (vy_14 = (a_122.Y - b_122.Y), Math.sqrt((vx_14 * vx_14) + (vy_14 * vy_14))))))) < 1E-09)("Half of Half of 180° should rotate point by 45°");
        Test_TestCaseBuilder__Zero(builder$0040_82);
    }));
})(), (() => {
    const builder$0040_83 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Applying Half three times - verified by point transformation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_83, Test_TestCaseBuilder__Delay_1505(builder$0040_83, () => {
        let a_124, b_124, vx_15, vy_15;
        const testPt_15 = Pt_$ctor_7B00E9A0(1, 0);
        let r_316;
        const rad_99 = 0.017453292519943295 * 160;
        r_316 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_99), Math.cos(rad_99));
        let half1;
        const r_317 = r_316;
        const halfCos_19 = Math.sqrt((1 + r_317.Cos) / 2);
        const halfSin_19 = Math.sqrt((1 - r_317.Cos) / 2);
        const halfSinSigned_19 = (r_317.Sin < 0) ? -halfSin_19 : halfSin_19;
        half1 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_19, halfCos_19);
        let half2;
        const r_318 = half1;
        const halfCos_20 = Math.sqrt((1 + r_318.Cos) / 2);
        const halfSin_20 = Math.sqrt((1 - r_318.Cos) / 2);
        const halfSinSigned_20 = (r_318.Sin < 0) ? -halfSin_20 : halfSin_20;
        half2 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_20, halfCos_20);
        let half3;
        const r_319 = half2;
        const halfCos_21 = Math.sqrt((1 + r_319.Cos) / 2);
        const halfSin_21 = Math.sqrt((1 - r_319.Cos) / 2);
        const halfSinSigned_21 = (r_319.Sin < 0) ? -halfSin_21 : halfSin_21;
        half3 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_21, halfCos_21);
        let expectedPt_1;
        let r_321;
        const rad_100 = 0.017453292519943295 * 20;
        r_321 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_100), Math.cos(rad_100));
        const p_89 = testPt_15;
        expectedPt_1 = Pt_$ctor_7B00E9A0_1((r_321.Cos * p_89.X) - (r_321.Sin * p_89.Y), (r_321.Sin * p_89.X) + (r_321.Cos * p_89.Y));
        let actualPt_1;
        const r_323 = half3;
        const p_91 = testPt_15;
        actualPt_1 = Pt_$ctor_7B00E9A0_1((r_323.Cos * p_91.X) - (r_323.Sin * p_91.Y), (r_323.Sin * p_91.X) + (r_323.Cos * p_91.Y));
        Expect_isTrue(((a_124 = expectedPt_1, (b_124 = actualPt_1, (vx_15 = (a_124.X - b_124.X), (vy_15 = (a_124.Y - b_124.Y), Math.sqrt((vx_15 * vx_15) + (vy_15 * vy_15))))))) < 1E-09)("160° / 2 / 2 / 2 = 20° - verified by point position");
        Test_TestCaseBuilder__Zero(builder$0040_83);
    }));
})(), (() => {
    const builder$0040_84 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Half rotation works correctly for points at various positions for 45°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_84, Test_TestCaseBuilder__Delay_1505(builder$0040_84, () => {
        const testPts_1 = [Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(-1, 0), Pt_$ctor_7B00E9A0(0, -1), Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(-1, 1), Pt_$ctor_7B00E9A0(-1, -1), Pt_$ctor_7B00E9A0(1, -1), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(0, 10), Pt_$ctor_7B00E9A0(5, 5)];
        let r_324;
        const rad_101 = 0.017453292519943295 * 45;
        r_324 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_101), Math.cos(rad_101));
        let half_18;
        const r_325 = r_324;
        const halfCos_22 = Math.sqrt((1 + r_325.Cos) / 2);
        const halfSin_22 = Math.sqrt((1 - r_325.Cos) / 2);
        const halfSinSigned_22 = (r_325.Sin < 0) ? -halfSin_22 : halfSin_22;
        half_18 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_22, halfCos_22);
        Test_TestCaseBuilder__For_Z371464DD(builder$0040_84, testPts_1, (_arg_3) => {
            let a_126, b_126, vx_16, vy_16;
            const testPt_16 = _arg_3;
            let fullResult_17;
            const r_327 = r_324;
            const p_93 = testPt_16;
            fullResult_17 = Pt_$ctor_7B00E9A0_1((r_327.Cos * p_93.X) - (r_327.Sin * p_93.Y), (r_327.Sin * p_93.X) + (r_327.Cos * p_93.Y));
            let halfTwiceResult_17;
            const r_331 = half_18;
            let p_97;
            const r_329 = half_18;
            const p_95 = testPt_16;
            p_97 = Pt_$ctor_7B00E9A0_1((r_329.Cos * p_95.X) - (r_329.Sin * p_95.Y), (r_329.Sin * p_95.X) + (r_329.Cos * p_95.Y));
            halfTwiceResult_17 = Pt_$ctor_7B00E9A0_1((r_331.Cos * p_97.X) - (r_331.Sin * p_97.Y), (r_331.Sin * p_97.X) + (r_331.Cos * p_97.Y));
            if (!(((a_126 = fullResult_17, (b_126 = halfTwiceResult_17, (vx_16 = (a_126.X - b_126.X), (vy_16 = (a_126.Y - b_126.Y), Math.sqrt((vx_16 * vx_16) + (vy_16 * vy_16))))))) < 1E-09)) {
                throw new Exception(`Half rotation twice should equal full rotation for 45° at point ${testPt_16}`);
                Test_TestCaseBuilder__Zero(builder$0040_84);
            }
            else {
                Test_TestCaseBuilder__Zero(builder$0040_84);
            }
        });
    }));
})(), (() => {
    const builder$0040_85 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Half rotation works correctly for points at various positions for -120°", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_85, Test_TestCaseBuilder__Delay_1505(builder$0040_85, () => {
        const testPts_2 = [Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(-1, 0), Pt_$ctor_7B00E9A0(0, -1), Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(-1, 1), Pt_$ctor_7B00E9A0(-1, -1), Pt_$ctor_7B00E9A0(1, -1), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(0, 10), Pt_$ctor_7B00E9A0(5, 5)];
        let r_332;
        const rad_102 = 0.017453292519943295 * -120;
        r_332 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_102), Math.cos(rad_102));
        let half_19;
        const r_333 = r_332;
        const halfCos_23 = Math.sqrt((1 + r_333.Cos) / 2);
        const halfSin_23 = Math.sqrt((1 - r_333.Cos) / 2);
        const halfSinSigned_23 = (r_333.Sin < 0) ? -halfSin_23 : halfSin_23;
        half_19 = Rotation2D_$ctor_7B00E9A0(halfSinSigned_23, halfCos_23);
        Test_TestCaseBuilder__For_Z371464DD(builder$0040_85, testPts_2, (_arg_4) => {
            let a_128, b_128, vx_17, vy_17;
            const testPt_17 = _arg_4;
            let fullResult_18;
            const r_335 = r_332;
            const p_99 = testPt_17;
            fullResult_18 = Pt_$ctor_7B00E9A0_1((r_335.Cos * p_99.X) - (r_335.Sin * p_99.Y), (r_335.Sin * p_99.X) + (r_335.Cos * p_99.Y));
            let halfTwiceResult_18;
            const r_339 = half_19;
            let p_103;
            const r_337 = half_19;
            const p_101 = testPt_17;
            p_103 = Pt_$ctor_7B00E9A0_1((r_337.Cos * p_101.X) - (r_337.Sin * p_101.Y), (r_337.Sin * p_101.X) + (r_337.Cos * p_101.Y));
            halfTwiceResult_18 = Pt_$ctor_7B00E9A0_1((r_339.Cos * p_103.X) - (r_339.Sin * p_103.Y), (r_339.Sin * p_103.X) + (r_339.Cos * p_103.Y));
            if (!(((a_128 = fullResult_18, (b_128 = halfTwiceResult_18, (vx_17 = (a_128.X - b_128.X), (vy_17 = (a_128.Y - b_128.Y), Math.sqrt((vx_17 * vx_17) + (vy_17 * vy_17))))))) < 1E-09)) {
                throw new Exception(`Half rotation twice should equal full rotation for -120° at point ${testPt_17}`);
                Test_TestCaseBuilder__Zero(builder$0040_85);
            }
            else {
                Test_TestCaseBuilder__Zero(builder$0040_85);
            }
        });
    }));
})()]))]));

