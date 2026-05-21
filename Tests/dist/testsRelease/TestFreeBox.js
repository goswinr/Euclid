
import { Expect_throws, Test_TestCaseBuilder__Zero, Expect_isTrue, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Pnt_$ctor_Z7AD9E565 } from "./Src/Pnt.js";
import { FreeBox__RotateWithCenter_4928E16A, FreeBox__Rotate_Z2A007687, FreeBox__TransformRigid_Z625426AD, FreeBox__Transform_3CAE9522, FreeBox__MoveZ_5E38073B, FreeBox__MoveY_5E38073B, FreeBox__MoveX_5E38073B, FreeBox__Move_Z394EC5F7, FreeBox__ScaleOn, FreeBox__Scale_5E38073B, FreeBox__get_Pt7, FreeBox__get_Pt5, FreeBox__get_Pt3, FreeBox__get_Pt2, FreeBox__get_Pt1, FreeBox__GetPt_Z524259A4, FreeBox_createFromFour2DPointsArgs_Z282B0FA0, FreeBox__get_Pt4, FreeBox_createFromFour2DPoints, FreeBox_createFromBox_Z394E92B4, FreeBox__get_SizeZ, FreeBox__get_SizeY, FreeBox__get_SizeX, FreeBox__get_Zaxis, FreeBox__get_Yaxis, FreeBox__get_Xaxis, FreeBox__get_Origin, FreeBox__get_Pt6, FreeBox__get_Pt0, FreeBox__get_Points, FreeBox_createFromEightPoints_Z6C25B48 } from "./Src/FreeBox.js";
import { int32ToString, structuralHash, Exception, assertEqual } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { singleton, ofArray, contains } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { equals, class_type, decimal_type, string_type, float64_type, bool_type, int32_type } from "./fable_modules/fable-library-js.5.0.0/Reflection.js";
import { printf, toText } from "./fable_modules/fable-library-js.5.0.0/String.js";
import { item } from "./fable_modules/fable-library-js.5.0.0/Array.js";
import { Vec_$ctor_Z7AD9E565 } from "./Src/Vec.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_1 } from "./Src/Vec.js";
import { Box_$ctor_5706A3BA } from "./Src/Box.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_1 } from "./Src/Pnt.js";
import { Pt_$ctor_7B00E9A0 } from "./Src/Pt.js";
import { FreeBox__Scale_5E38073B as FreeBox__Scale_5E38073B_1, FreeBox__RotateWithCenter_4928E16A as FreeBox__RotateWithCenter_4928E16A_1, FreeBox__Rotate_Z2A007687 as FreeBox__Rotate_Z2A007687_1, FreeBox__TransformRigid_Z625426AD as FreeBox__TransformRigid_Z625426AD_1, FreeBox__Transform_3CAE9522 as FreeBox__Transform_3CAE9522_1, FreeBox__MoveZ_5E38073B as FreeBox__MoveZ_5E38073B_1, FreeBox__MoveY_5E38073B as FreeBox__MoveY_5E38073B_1, FreeBox__MoveX_5E38073B as FreeBox__MoveX_5E38073B_1, FreeBox__Move_Z394EC5F7 as FreeBox__Move_Z394EC5F7_1 } from "./Src/FreeBox.js";
import { Matrix_createTranslation_Z394EC5F7, Matrix_get_identity } from "./Src/Matrix.js";
import { RigidMatrix_createTranslation_Z394EC5F7 } from "./Src/RigidMatrix.js";
import { Quaternion_$ctor_77D16AC0 } from "./Src/Quaternion.js";
import { UnitVec_$ctor_Z7AD9E565 } from "./Src/UnitVec.js";

export const tests = Test_testList("FreeBox", ofArray([Test_testList("Constructor and Basic Properties", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromEightPoints with 8 points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let copyOfStruct, arg, arg_1, a_2, b_2, x_1, y_1, z, a_4, b_4, x_2, y_2, z_1;
        const pts = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 5, 0), Pnt_$ctor_Z7AD9E565(0, 5, 0), Pnt_$ctor_Z7AD9E565(0, 0, 3), Pnt_$ctor_Z7AD9E565(10, 0, 3), Pnt_$ctor_Z7AD9E565(10, 5, 3), Pnt_$ctor_Z7AD9E565(0, 5, 3)];
        const box = FreeBox_createFromEightPoints_Z6C25B48(pts);
        const actual = FreeBox__get_Points(box).length | 0;
        if ((actual === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual, 8, "Should have 8 points");
        }
        else {
            throw new Exception(contains((copyOfStruct = actual, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x) => (structuralHash(x) | 0),
            }) ? ((arg = int32ToString(8), (arg_1 = int32ToString(actual), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("Should have 8 points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual)("Should have 8 points"));
        }
        Expect_isTrue(((a_2 = FreeBox__get_Pt0(box), (b_2 = item(0, pts), (x_1 = (a_2.X - b_2.X), (y_1 = (a_2.Y - b_2.Y), (z = (a_2.Z - b_2.Z), Math.sqrt(((x_1 * x_1) + (y_1 * y_1)) + (z * z)))))))) < 1E-09)("Pt0 should match input");
        Expect_isTrue(((a_4 = FreeBox__get_Pt6(box), (b_4 = item(6, pts), (x_2 = (a_4.X - b_4.X), (y_2 = (a_4.Y - b_4.Y), (z_1 = (a_4.Z - b_4.Z), Math.sqrt(((x_2 * x_2) + (y_2 * y_2)) + (z_1 * z_1)))))))) < 1E-09)("Pt6 should match input");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromEightPoints rejects wrong number of points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const pts_1 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 5, 0)];
        Expect_throws(() => {
            FreeBox_createFromEightPoints_Z6C25B48(pts_1);
        }, "Should throw with wrong number of points");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Origin is first point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let a_6, b_6, x_3, y_3, z_2;
        const pts_2 = [Pnt_$ctor_Z7AD9E565(1, 2, 3), Pnt_$ctor_Z7AD9E565(11, 2, 3), Pnt_$ctor_Z7AD9E565(11, 7, 3), Pnt_$ctor_Z7AD9E565(1, 7, 3), Pnt_$ctor_Z7AD9E565(1, 2, 6), Pnt_$ctor_Z7AD9E565(11, 2, 6), Pnt_$ctor_Z7AD9E565(11, 7, 6), Pnt_$ctor_Z7AD9E565(1, 7, 6)];
        Expect_isTrue(((a_6 = FreeBox__get_Origin(FreeBox_createFromEightPoints_Z6C25B48(pts_2)), (b_6 = item(0, pts_2), (x_3 = (a_6.X - b_6.X), (y_3 = (a_6.Y - b_6.Y), (z_2 = (a_6.Z - b_6.Z), Math.sqrt(((x_3 * x_3) + (y_3 * y_3)) + (z_2 * z_2)))))))) < 1E-09)("Origin should be first point");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Xaxis is vector from Pt0 to Pt1", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let v_1, a_7, b_7;
        const box_2 = FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 5, 0), Pnt_$ctor_Z7AD9E565(0, 5, 0), Pnt_$ctor_Z7AD9E565(0, 0, 3), Pnt_$ctor_Z7AD9E565(10, 0, 3), Pnt_$ctor_Z7AD9E565(10, 5, 3), Pnt_$ctor_Z7AD9E565(0, 5, 3)]);
        const expected_1 = Vec_$ctor_Z7AD9E565(10, 0, 0);
        Expect_isTrue(((v_1 = ((a_7 = FreeBox__get_Xaxis(box_2), (b_7 = expected_1, Vec_$ctor_Z7AD9E565_1(a_7.X - b_7.X, a_7.Y - b_7.Y, a_7.Z - b_7.Z)))), Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z)))) < 1E-09)("Xaxis should be Pt1 - Pt0");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Yaxis is vector from Pt0 to Pt3", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let v_3, a_8, b_8;
        const box_3 = FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 5, 0), Pnt_$ctor_Z7AD9E565(0, 5, 0), Pnt_$ctor_Z7AD9E565(0, 0, 3), Pnt_$ctor_Z7AD9E565(10, 0, 3), Pnt_$ctor_Z7AD9E565(10, 5, 3), Pnt_$ctor_Z7AD9E565(0, 5, 3)]);
        const expected_2 = Vec_$ctor_Z7AD9E565(0, 5, 0);
        Expect_isTrue(((v_3 = ((a_8 = FreeBox__get_Yaxis(box_3), (b_8 = expected_2, Vec_$ctor_Z7AD9E565_1(a_8.X - b_8.X, a_8.Y - b_8.Y, a_8.Z - b_8.Z)))), Math.sqrt(((v_3.X * v_3.X) + (v_3.Y * v_3.Y)) + (v_3.Z * v_3.Z)))) < 1E-09)("Yaxis should be Pt3 - Pt0");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Zaxis is vector from Pt0 to Pt4", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let v_5, a_9, b_9;
        const box_4 = FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 5, 0), Pnt_$ctor_Z7AD9E565(0, 5, 0), Pnt_$ctor_Z7AD9E565(0, 0, 3), Pnt_$ctor_Z7AD9E565(10, 0, 3), Pnt_$ctor_Z7AD9E565(10, 5, 3), Pnt_$ctor_Z7AD9E565(0, 5, 3)]);
        const expected_3 = Vec_$ctor_Z7AD9E565(0, 0, 3);
        Expect_isTrue(((v_5 = ((a_9 = FreeBox__get_Zaxis(box_4), (b_9 = expected_3, Vec_$ctor_Z7AD9E565_1(a_9.X - b_9.X, a_9.Y - b_9.Y, a_9.Z - b_9.Z)))), Math.sqrt(((v_5.X * v_5.X) + (v_5.Y * v_5.Y)) + (v_5.Z * v_5.Z)))) < 1E-09)("Zaxis should be Pt4 - Pt0");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("SizeX, SizeY, SizeZ", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        let copyOfStruct_1, arg_6, arg_1_1, copyOfStruct_2, arg_7, arg_1_2, copyOfStruct_3, arg_8, arg_1_3;
        const box_5 = FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 5, 0), Pnt_$ctor_Z7AD9E565(0, 5, 0), Pnt_$ctor_Z7AD9E565(0, 0, 3), Pnt_$ctor_Z7AD9E565(10, 0, 3), Pnt_$ctor_Z7AD9E565(10, 5, 3), Pnt_$ctor_Z7AD9E565(0, 5, 3)]);
        const actual_1 = FreeBox__get_SizeX(box_5);
        if ((actual_1 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, 10, "SizeX should be 10");
        }
        else {
            throw new Exception(contains((copyOfStruct_1 = actual_1, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_4) => (structuralHash(x_4) | 0),
            }) ? ((arg_6 = (10).toString(), (arg_1_1 = actual_1.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("SizeX should be 10")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_1)("SizeX should be 10"));
        }
        const actual_2 = FreeBox__get_SizeY(box_5);
        if ((actual_2 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_2, 5, "SizeY should be 5");
        }
        else {
            throw new Exception(contains((copyOfStruct_2 = actual_2, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_5) => (structuralHash(x_5) | 0),
            }) ? ((arg_7 = (5).toString(), (arg_1_2 = actual_2.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("SizeY should be 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_2)("SizeY should be 5"));
        }
        const actual_3 = FreeBox__get_SizeZ(box_5);
        if ((actual_3 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, 3, "SizeZ should be 3");
        }
        else {
            throw new Exception(contains((copyOfStruct_3 = actual_3, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_6) => (structuralHash(x_6) | 0),
            }) ? ((arg_8 = (3).toString(), (arg_1_3 = actual_3.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("SizeZ should be 3")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_3)("SizeZ should be 3"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})()])), Test_testList("Creation from Box", singleton((() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromBox", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let a_11, b_12, x_7, y_7, z_3, a_13, b_15, b_13, p_2, p_1, p, v_6, v_7, v_8, x_8, y_8, z_4, copyOfStruct_4, arg_9, arg_1_4, copyOfStruct_5, arg_10, arg_1_5, copyOfStruct_6, arg_11, arg_1_6;
        const box_6 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        const freeBox = FreeBox_createFromBox_Z394E92B4(box_6);
        Expect_isTrue(((a_11 = FreeBox__get_Pt0(freeBox), (b_12 = box_6.Origin, (x_7 = (a_11.X - b_12.X), (y_7 = (a_11.Y - b_12.Y), (z_3 = (a_11.Z - b_12.Z), Math.sqrt(((x_7 * x_7) + (y_7 * y_7)) + (z_3 * z_3)))))))) < 1E-09)("Pt0 should match");
        Expect_isTrue(((a_13 = FreeBox__get_Pt6(freeBox), (b_15 = ((b_13 = box_6, (p_2 = ((p_1 = ((p = b_13.Origin, (v_6 = b_13.Xaxis, Pnt_$ctor_Z7AD9E565_1(p.X + v_6.X, p.Y + v_6.Y, p.Z + v_6.Z)))), (v_7 = b_13.Yaxis, Pnt_$ctor_Z7AD9E565_1(p_1.X + v_7.X, p_1.Y + v_7.Y, p_1.Z + v_7.Z)))), (v_8 = b_13.Zaxis, Pnt_$ctor_Z7AD9E565_1(p_2.X + v_8.X, p_2.Y + v_8.Y, p_2.Z + v_8.Z))))), (x_8 = (a_13.X - b_15.X), (y_8 = (a_13.Y - b_15.Y), (z_4 = (a_13.Z - b_15.Z), Math.sqrt(((x_8 * x_8) + (y_8 * y_8)) + (z_4 * z_4)))))))) < 1E-09)("Pt6 should match");
        const actual_4 = FreeBox__get_SizeX(freeBox);
        let expected_7;
        const v_9 = box_6.Xaxis;
        expected_7 = Math.sqrt(((v_9.X * v_9.X) + (v_9.Y * v_9.Y)) + (v_9.Z * v_9.Z));
        if ((actual_4 === expected_7) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_4, expected_7, "SizeX should match");
        }
        else {
            throw new Exception(contains((copyOfStruct_4 = actual_4, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_9) => (structuralHash(x_9) | 0),
            }) ? ((arg_9 = expected_7.toString(), (arg_1_4 = actual_4.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("SizeX should match")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_7)(actual_4)("SizeX should match"));
        }
        const actual_5 = FreeBox__get_SizeY(freeBox);
        let expected_8;
        const v_10 = box_6.Yaxis;
        expected_8 = Math.sqrt(((v_10.X * v_10.X) + (v_10.Y * v_10.Y)) + (v_10.Z * v_10.Z));
        if ((actual_5 === expected_8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_5, expected_8, "SizeY should match");
        }
        else {
            throw new Exception(contains((copyOfStruct_5 = actual_5, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_10) => (structuralHash(x_10) | 0),
            }) ? ((arg_10 = expected_8.toString(), (arg_1_5 = actual_5.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("SizeY should match")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_8)(actual_5)("SizeY should match"));
        }
        const actual_6 = FreeBox__get_SizeZ(freeBox);
        let expected_9;
        const v_11 = box_6.Zaxis;
        expected_9 = Math.sqrt(((v_11.X * v_11.X) + (v_11.Y * v_11.Y)) + (v_11.Z * v_11.Z));
        if ((actual_6 === expected_9) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_6, expected_9, "SizeZ should match");
        }
        else {
            throw new Exception(contains((copyOfStruct_6 = actual_6, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_11) => (structuralHash(x_11) | 0),
            }) ? ((arg_11 = expected_9.toString(), (arg_1_6 = actual_6.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_11)(arg_1_6)("SizeZ should match")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_9)(actual_6)("SizeZ should match"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})())), Test_testList("Creation from 2D Points", ofArray([(() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromFour2DPoints with valid points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let a_15, b_20, x_12, y_12, z_5, a_17, b_22, x_13, y_13, z_6, copyOfStruct_7, arg_12, arg_1_7;
        const box_7 = FreeBox_createFromFour2DPoints(2, 8, [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 5), Pt_$ctor_7B00E9A0(0, 5)]);
        Expect_isTrue(((a_15 = FreeBox__get_Pt0(box_7), (b_20 = Pnt_$ctor_Z7AD9E565(0, 0, 2), (x_12 = (a_15.X - b_20.X), (y_12 = (a_15.Y - b_20.Y), (z_5 = (a_15.Z - b_20.Z), Math.sqrt(((x_12 * x_12) + (y_12 * y_12)) + (z_5 * z_5)))))))) < 1E-09)("Pt0 should be at zMin");
        Expect_isTrue(((a_17 = FreeBox__get_Pt4(box_7), (b_22 = Pnt_$ctor_Z7AD9E565(0, 0, 8), (x_13 = (a_17.X - b_22.X), (y_13 = (a_17.Y - b_22.Y), (z_6 = (a_17.Z - b_22.Z), Math.sqrt(((x_13 * x_13) + (y_13 * y_13)) + (z_6 * z_6)))))))) < 1E-09)("Pt4 should be at zMax");
        const actual_7 = FreeBox__get_SizeZ(box_7);
        if ((actual_7 === 6) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, 6, "SizeZ should be zMax - zMin");
        }
        else {
            throw new Exception(contains((copyOfStruct_7 = actual_7, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_14) => (structuralHash(x_14) | 0),
            }) ? ((arg_12 = (6).toString(), (arg_1_7 = actual_7.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_12)(arg_1_7)("SizeZ should be zMax - zMin")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(6)(actual_7)("SizeZ should be zMax - zMin"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromFour2DPoints rejects wrong number of points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const pts_9 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 5)];
        Expect_throws(() => {
            FreeBox_createFromFour2DPoints(2, 8, pts_9);
        }, "Should throw with wrong number of points");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromFour2DPointsArgs", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let a_20, b_25, x_15, y_15, z_7, a_22, b_27, x_16, y_16, z_8;
        const box_8 = FreeBox_createFromFour2DPointsArgs_Z282B0FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 5), Pt_$ctor_7B00E9A0(0, 5), 2, 8);
        Expect_isTrue(((a_20 = FreeBox__get_Pt0(box_8), (b_25 = Pnt_$ctor_Z7AD9E565(0, 0, 2), (x_15 = (a_20.X - b_25.X), (y_15 = (a_20.Y - b_25.Y), (z_7 = (a_20.Z - b_25.Z), Math.sqrt(((x_15 * x_15) + (y_15 * y_15)) + (z_7 * z_7)))))))) < 1E-09)("Pt0 should be at zMin");
        Expect_isTrue(((a_22 = FreeBox__get_Pt4(box_8), (b_27 = Pnt_$ctor_Z7AD9E565(0, 0, 8), (x_16 = (a_22.X - b_27.X), (y_16 = (a_22.Y - b_27.Y), (z_8 = (a_22.Z - b_27.Z), Math.sqrt(((x_16 * x_16) + (y_16 * y_16)) + (z_8 * z_8)))))))) < 1E-09)("Pt4 should be at zMax");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})()])), Test_testList("Point Access", ofArray([(() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("GetPt with valid index", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        let a_24, b_29, x_17, y_17, z_9, a_26, b_31, x_18, y_18, z_10;
        const pts_11 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 5, 0), Pnt_$ctor_Z7AD9E565(0, 5, 0), Pnt_$ctor_Z7AD9E565(0, 0, 3), Pnt_$ctor_Z7AD9E565(10, 0, 3), Pnt_$ctor_Z7AD9E565(10, 5, 3), Pnt_$ctor_Z7AD9E565(0, 5, 3)];
        const box_9 = FreeBox_createFromEightPoints_Z6C25B48(pts_11);
        Expect_isTrue(((a_24 = FreeBox__GetPt_Z524259A4(box_9, 0), (b_29 = item(0, pts_11), (x_17 = (a_24.X - b_29.X), (y_17 = (a_24.Y - b_29.Y), (z_9 = (a_24.Z - b_29.Z), Math.sqrt(((x_17 * x_17) + (y_17 * y_17)) + (z_9 * z_9)))))))) < 1E-09)("GetPt 0 should return Pt0");
        Expect_isTrue(((a_26 = FreeBox__GetPt_Z524259A4(box_9, 7), (b_31 = item(7, pts_11), (x_18 = (a_26.X - b_31.X), (y_18 = (a_26.Y - b_31.Y), (z_10 = (a_26.Z - b_31.Z), Math.sqrt(((x_18 * x_18) + (y_18 * y_18)) + (z_10 * z_10)))))))) < 1E-09)("GetPt 7 should return Pt7");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("GetPt with invalid index throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        const box_10 = FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 5, 0), Pnt_$ctor_Z7AD9E565(0, 5, 0), Pnt_$ctor_Z7AD9E565(0, 0, 3), Pnt_$ctor_Z7AD9E565(10, 0, 3), Pnt_$ctor_Z7AD9E565(10, 5, 3), Pnt_$ctor_Z7AD9E565(0, 5, 3)]);
        Expect_throws(() => {
            FreeBox__GetPt_Z524259A4(box_10, 8);
        }, "Should throw for index 8");
        Expect_throws(() => {
            FreeBox__GetPt_Z524259A4(box_10, -1);
        }, "Should throw for negative index");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("All Pt properties work", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        let a_28, b_33, x_19, y_19, z_11, a_30, b_35, x_20, y_20, z_12, a_32, b_37, x_21, y_21, z_13, a_34, b_39, x_22, y_22, z_14, a_36, b_41, x_23, y_23, z_15, a_38, b_43, x_24, y_24, z_16, a_40, b_45, x_25, y_25, z_17, a_42, b_47, x_26, y_26, z_18;
        const pts_13 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 5, 0), Pnt_$ctor_Z7AD9E565(0, 5, 0), Pnt_$ctor_Z7AD9E565(0, 0, 3), Pnt_$ctor_Z7AD9E565(10, 0, 3), Pnt_$ctor_Z7AD9E565(10, 5, 3), Pnt_$ctor_Z7AD9E565(0, 5, 3)];
        const box_11 = FreeBox_createFromEightPoints_Z6C25B48(pts_13);
        Expect_isTrue(((a_28 = FreeBox__get_Pt0(box_11), (b_33 = item(0, pts_13), (x_19 = (a_28.X - b_33.X), (y_19 = (a_28.Y - b_33.Y), (z_11 = (a_28.Z - b_33.Z), Math.sqrt(((x_19 * x_19) + (y_19 * y_19)) + (z_11 * z_11)))))))) < 1E-09)("Pt0 should match");
        Expect_isTrue(((a_30 = FreeBox__get_Pt1(box_11), (b_35 = item(1, pts_13), (x_20 = (a_30.X - b_35.X), (y_20 = (a_30.Y - b_35.Y), (z_12 = (a_30.Z - b_35.Z), Math.sqrt(((x_20 * x_20) + (y_20 * y_20)) + (z_12 * z_12)))))))) < 1E-09)("Pt1 should match");
        Expect_isTrue(((a_32 = FreeBox__get_Pt2(box_11), (b_37 = item(2, pts_13), (x_21 = (a_32.X - b_37.X), (y_21 = (a_32.Y - b_37.Y), (z_13 = (a_32.Z - b_37.Z), Math.sqrt(((x_21 * x_21) + (y_21 * y_21)) + (z_13 * z_13)))))))) < 1E-09)("Pt2 should match");
        Expect_isTrue(((a_34 = FreeBox__get_Pt3(box_11), (b_39 = item(3, pts_13), (x_22 = (a_34.X - b_39.X), (y_22 = (a_34.Y - b_39.Y), (z_14 = (a_34.Z - b_39.Z), Math.sqrt(((x_22 * x_22) + (y_22 * y_22)) + (z_14 * z_14)))))))) < 1E-09)("Pt3 should match");
        Expect_isTrue(((a_36 = FreeBox__get_Pt4(box_11), (b_41 = item(4, pts_13), (x_23 = (a_36.X - b_41.X), (y_23 = (a_36.Y - b_41.Y), (z_15 = (a_36.Z - b_41.Z), Math.sqrt(((x_23 * x_23) + (y_23 * y_23)) + (z_15 * z_15)))))))) < 1E-09)("Pt4 should match");
        Expect_isTrue(((a_38 = FreeBox__get_Pt5(box_11), (b_43 = item(5, pts_13), (x_24 = (a_38.X - b_43.X), (y_24 = (a_38.Y - b_43.Y), (z_16 = (a_38.Z - b_43.Z), Math.sqrt(((x_24 * x_24) + (y_24 * y_24)) + (z_16 * z_16)))))))) < 1E-09)("Pt5 should match");
        Expect_isTrue(((a_40 = FreeBox__get_Pt6(box_11), (b_45 = item(6, pts_13), (x_25 = (a_40.X - b_45.X), (y_25 = (a_40.Y - b_45.Y), (z_17 = (a_40.Z - b_45.Z), Math.sqrt(((x_25 * x_25) + (y_25 * y_25)) + (z_17 * z_17)))))))) < 1E-09)("Pt6 should match");
        Expect_isTrue(((a_42 = FreeBox__get_Pt7(box_11), (b_47 = item(7, pts_13), (x_26 = (a_42.X - b_47.X), (y_26 = (a_42.Y - b_47.Y), (z_18 = (a_42.Z - b_47.Z), Math.sqrt(((x_26 * x_26) + (y_26 * y_26)) + (z_18 * z_18)))))))) < 1E-09)("Pt7 should match");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})()])), Test_testList("Transformation Methods", ofArray([(() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Scale from world origin", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let a_44, b_49, x_27, y_27, z_19, a_46, b_51, x_28, y_28, z_20;
        const scaled = FreeBox__Scale_5E38073B(FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(1, 2, 3), Pnt_$ctor_Z7AD9E565(11, 2, 3), Pnt_$ctor_Z7AD9E565(11, 7, 3), Pnt_$ctor_Z7AD9E565(1, 7, 3), Pnt_$ctor_Z7AD9E565(1, 2, 6), Pnt_$ctor_Z7AD9E565(11, 2, 6), Pnt_$ctor_Z7AD9E565(11, 7, 6), Pnt_$ctor_Z7AD9E565(1, 7, 6)]), 2);
        Expect_isTrue(((a_44 = FreeBox__get_Pt0(scaled), (b_49 = Pnt_$ctor_Z7AD9E565(2, 4, 6), (x_27 = (a_44.X - b_49.X), (y_27 = (a_44.Y - b_49.Y), (z_19 = (a_44.Z - b_49.Z), Math.sqrt(((x_27 * x_27) + (y_27 * y_27)) + (z_19 * z_19)))))))) < 1E-09)("Pt0 should be scaled");
        Expect_isTrue(((a_46 = FreeBox__get_Pt6(scaled), (b_51 = Pnt_$ctor_Z7AD9E565(22, 14, 12), (x_28 = (a_46.X - b_51.X), (y_28 = (a_46.Y - b_51.Y), (z_20 = (a_46.Z - b_51.Z), Math.sqrt(((x_28 * x_28) + (y_28 * y_28)) + (z_20 * z_20)))))))) < 1E-09)("Pt6 should be scaled");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ScaleOn center point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        let a_48, b_53, x_29, y_29, z_21, a_50, b_55, x_30, y_30, z_22;
        const scaled_1 = FreeBox__ScaleOn(FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 10), Pnt_$ctor_Z7AD9E565(10, 0, 10), Pnt_$ctor_Z7AD9E565(10, 10, 10), Pnt_$ctor_Z7AD9E565(0, 10, 10)]), Pnt_$ctor_Z7AD9E565(5, 5, 5), 2);
        Expect_isTrue(((a_48 = FreeBox__get_Pt0(scaled_1), (b_53 = Pnt_$ctor_Z7AD9E565(-5, -5, -5), (x_29 = (a_48.X - b_53.X), (y_29 = (a_48.Y - b_53.Y), (z_21 = (a_48.Z - b_53.Z), Math.sqrt(((x_29 * x_29) + (y_29 * y_29)) + (z_21 * z_21)))))))) < 1E-09)("Pt0 should be scaled around center");
        Expect_isTrue(((a_50 = FreeBox__get_Pt6(scaled_1), (b_55 = Pnt_$ctor_Z7AD9E565(15, 15, 15), (x_30 = (a_50.X - b_55.X), (y_30 = (a_50.Y - b_55.Y), (z_22 = (a_50.Z - b_55.Z), Math.sqrt(((x_30 * x_30) + (y_30 * y_30)) + (z_22 * z_22)))))))) < 1E-09)("Pt6 should be scaled around center");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Scale with factor 0.5 shrinks box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let a_52, b_57, x_31, y_31, z_23, a_54, b_59, x_32, y_32, z_24;
        const scaled_2 = FreeBox__Scale_5E38073B(FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 10), Pnt_$ctor_Z7AD9E565(10, 0, 10), Pnt_$ctor_Z7AD9E565(10, 10, 10), Pnt_$ctor_Z7AD9E565(0, 10, 10)]), 0.5);
        Expect_isTrue(((a_52 = FreeBox__get_Pt0(scaled_2), (b_57 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (x_31 = (a_52.X - b_57.X), (y_31 = (a_52.Y - b_57.Y), (z_23 = (a_52.Z - b_57.Z), Math.sqrt(((x_31 * x_31) + (y_31 * y_31)) + (z_23 * z_23)))))))) < 1E-09)("Pt0 should remain at origin");
        Expect_isTrue(((a_54 = FreeBox__get_Pt6(scaled_2), (b_59 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (x_32 = (a_54.X - b_59.X), (y_32 = (a_54.Y - b_59.Y), (z_24 = (a_54.Z - b_59.Z), Math.sqrt(((x_32 * x_32) + (y_32 * y_32)) + (z_24 * z_24)))))))) < 1E-09)("Pt6 should be halved");
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Move instance method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        let a_56, b_61, x_33, y_33, z_25, a_58, b_63, x_34, y_34, z_26;
        const moved = FreeBox__Move_Z394EC5F7(FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 10), Pnt_$ctor_Z7AD9E565(10, 0, 10), Pnt_$ctor_Z7AD9E565(10, 10, 10), Pnt_$ctor_Z7AD9E565(0, 10, 10)]), Vec_$ctor_Z7AD9E565(5, 3, 2));
        Expect_isTrue(((a_56 = FreeBox__get_Pt0(moved), (b_61 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_33 = (a_56.X - b_61.X), (y_33 = (a_56.Y - b_61.Y), (z_25 = (a_56.Z - b_61.Z), Math.sqrt(((x_33 * x_33) + (y_33 * y_33)) + (z_25 * z_25)))))))) < 1E-09)("Pt0 should be moved");
        Expect_isTrue(((a_58 = FreeBox__get_Pt6(moved), (b_63 = Pnt_$ctor_Z7AD9E565(15, 13, 12), (x_34 = (a_58.X - b_63.X), (y_34 = (a_58.Y - b_63.Y), (z_26 = (a_58.Z - b_63.Z), Math.sqrt(((x_34 * x_34) + (y_34 * y_34)) + (z_26 * z_26)))))))) < 1E-09)("Pt6 should be moved");
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("MoveX instance method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        let a_60, b_65, x_35, y_35, z_27;
        Expect_isTrue(((a_60 = FreeBox__get_Pt0(FreeBox__MoveX_5E38073B(FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 10), Pnt_$ctor_Z7AD9E565(10, 0, 10), Pnt_$ctor_Z7AD9E565(10, 10, 10), Pnt_$ctor_Z7AD9E565(0, 10, 10)]), 5)), (b_65 = Pnt_$ctor_Z7AD9E565(5, 0, 0), (x_35 = (a_60.X - b_65.X), (y_35 = (a_60.Y - b_65.Y), (z_27 = (a_60.Z - b_65.Z), Math.sqrt(((x_35 * x_35) + (y_35 * y_35)) + (z_27 * z_27)))))))) < 1E-09)("Pt0 should be moved in X");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("MoveY instance method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        let a_62, b_67, x_36, y_36, z_28;
        Expect_isTrue(((a_62 = FreeBox__get_Pt0(FreeBox__MoveY_5E38073B(FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 10), Pnt_$ctor_Z7AD9E565(10, 0, 10), Pnt_$ctor_Z7AD9E565(10, 10, 10), Pnt_$ctor_Z7AD9E565(0, 10, 10)]), 3)), (b_67 = Pnt_$ctor_Z7AD9E565(0, 3, 0), (x_36 = (a_62.X - b_67.X), (y_36 = (a_62.Y - b_67.Y), (z_28 = (a_62.Z - b_67.Z), Math.sqrt(((x_36 * x_36) + (y_36 * y_36)) + (z_28 * z_28)))))))) < 1E-09)("Pt0 should be moved in Y");
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("MoveZ instance method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        let a_64, b_69, x_37, y_37, z_29;
        Expect_isTrue(((a_64 = FreeBox__get_Pt0(FreeBox__MoveZ_5E38073B(FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 10), Pnt_$ctor_Z7AD9E565(10, 0, 10), Pnt_$ctor_Z7AD9E565(10, 10, 10), Pnt_$ctor_Z7AD9E565(0, 10, 10)]), 2)), (b_69 = Pnt_$ctor_Z7AD9E565(0, 0, 2), (x_37 = (a_64.X - b_69.X), (y_37 = (a_64.Y - b_69.Y), (z_29 = (a_64.Z - b_69.Z), Math.sqrt(((x_37 * x_37) + (y_37 * y_37)) + (z_29 * z_29)))))))) < 1E-09)("Pt0 should be moved in Z");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("move static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        let a_66, b_73, x_38, y_38, z_30;
        Expect_isTrue(((a_66 = FreeBox__get_Pt0(FreeBox__Move_Z394EC5F7_1(FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 10), Pnt_$ctor_Z7AD9E565(10, 0, 10), Pnt_$ctor_Z7AD9E565(10, 10, 10), Pnt_$ctor_Z7AD9E565(0, 10, 10)]), Vec_$ctor_Z7AD9E565(5, 3, 2))), (b_73 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_38 = (a_66.X - b_73.X), (y_38 = (a_66.Y - b_73.Y), (z_30 = (a_66.Z - b_73.Z), Math.sqrt(((x_38 * x_38) + (y_38 * y_38)) + (z_30 * z_30)))))))) < 1E-09)("Pt0 should be moved");
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("translate static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        let a_68, b_77, x_39, y_39, z_31;
        Expect_isTrue(((a_68 = FreeBox__get_Pt0(FreeBox__Move_Z394EC5F7_1(FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 10), Pnt_$ctor_Z7AD9E565(10, 0, 10), Pnt_$ctor_Z7AD9E565(10, 10, 10), Pnt_$ctor_Z7AD9E565(0, 10, 10)]), Vec_$ctor_Z7AD9E565(5, 3, 2))), (b_77 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_39 = (a_68.X - b_77.X), (y_39 = (a_68.Y - b_77.Y), (z_31 = (a_68.Z - b_77.Z), Math.sqrt(((x_39 * x_39) + (y_39 * y_39)) + (z_31 * z_31)))))))) < 1E-09)("Pt0 should be translated");
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("moveX, moveY, moveZ static methods", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        let a_70, b_85, x_40, y_40, z_32, a_72, b_87, x_41, y_41, z_33, a_74, b_89, x_42, y_42, z_34;
        const box_21 = FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 10), Pnt_$ctor_Z7AD9E565(10, 0, 10), Pnt_$ctor_Z7AD9E565(10, 10, 10), Pnt_$ctor_Z7AD9E565(0, 10, 10)]);
        const movedX = FreeBox__MoveX_5E38073B_1(box_21, 5);
        const movedY = FreeBox__MoveY_5E38073B_1(box_21, 3);
        const movedZ = FreeBox__MoveZ_5E38073B_1(box_21, 2);
        Expect_isTrue(((a_70 = FreeBox__get_Pt0(movedX), (b_85 = Pnt_$ctor_Z7AD9E565(5, 0, 0), (x_40 = (a_70.X - b_85.X), (y_40 = (a_70.Y - b_85.Y), (z_32 = (a_70.Z - b_85.Z), Math.sqrt(((x_40 * x_40) + (y_40 * y_40)) + (z_32 * z_32)))))))) < 1E-09)("moveX should work");
        Expect_isTrue(((a_72 = FreeBox__get_Pt0(movedY), (b_87 = Pnt_$ctor_Z7AD9E565(0, 3, 0), (x_41 = (a_72.X - b_87.X), (y_41 = (a_72.Y - b_87.Y), (z_33 = (a_72.Z - b_87.Z), Math.sqrt(((x_41 * x_41) + (y_41 * y_41)) + (z_33 * z_33)))))))) < 1E-09)("moveY should work");
        Expect_isTrue(((a_74 = FreeBox__get_Pt0(movedZ), (b_89 = Pnt_$ctor_Z7AD9E565(0, 0, 2), (x_42 = (a_74.X - b_89.X), (y_42 = (a_74.Y - b_89.Y), (z_34 = (a_74.Z - b_89.Z), Math.sqrt(((x_42 * x_42) + (y_42 * y_42)) + (z_34 * z_34)))))))) < 1E-09)("moveZ should work");
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Transform with identity matrix", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        let a_76, b_91, x_43, y_43, z_35;
        const box_22 = FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(1, 2, 3), Pnt_$ctor_Z7AD9E565(11, 2, 3), Pnt_$ctor_Z7AD9E565(11, 7, 3), Pnt_$ctor_Z7AD9E565(1, 7, 3), Pnt_$ctor_Z7AD9E565(1, 2, 6), Pnt_$ctor_Z7AD9E565(11, 2, 6), Pnt_$ctor_Z7AD9E565(11, 7, 6), Pnt_$ctor_Z7AD9E565(1, 7, 6)]);
        Expect_isTrue(((a_76 = FreeBox__get_Pt0(FreeBox__Transform_3CAE9522(box_22, Matrix_get_identity())), (b_91 = FreeBox__get_Pt0(box_22), (x_43 = (a_76.X - b_91.X), (y_43 = (a_76.Y - b_91.Y), (z_35 = (a_76.Z - b_91.Z), Math.sqrt(((x_43 * x_43) + (y_43 * y_43)) + (z_35 * z_35)))))))) < 1E-09)("Pt0 should be unchanged with identity");
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Transform with translation matrix", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        let a_78, b_93, x_44, y_44, z_36;
        Expect_isTrue(((a_78 = FreeBox__get_Pt0(FreeBox__Transform_3CAE9522(FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 10), Pnt_$ctor_Z7AD9E565(10, 0, 10), Pnt_$ctor_Z7AD9E565(10, 10, 10), Pnt_$ctor_Z7AD9E565(0, 10, 10)]), Matrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(5, 3, 2)))), (b_93 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_44 = (a_78.X - b_93.X), (y_44 = (a_78.Y - b_93.Y), (z_36 = (a_78.Z - b_93.Z), Math.sqrt(((x_44 * x_44) + (y_44 * y_44)) + (z_36 * z_36)))))))) < 1E-09)("Pt0 should be translated");
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("transform static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        let a_80, b_97, x_45, y_45, z_37;
        Expect_isTrue(((a_80 = FreeBox__get_Pt0(FreeBox__Transform_3CAE9522_1(FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 10), Pnt_$ctor_Z7AD9E565(10, 0, 10), Pnt_$ctor_Z7AD9E565(10, 10, 10), Pnt_$ctor_Z7AD9E565(0, 10, 10)]), Matrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(5, 3, 2)))), (b_97 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_45 = (a_80.X - b_97.X), (y_45 = (a_80.Y - b_97.Y), (z_37 = (a_80.Z - b_97.Z), Math.sqrt(((x_45 * x_45) + (y_45 * y_45)) + (z_37 * z_37)))))))) < 1E-09)("Pt0 should be translated");
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("TransformRigid instance method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        let a_82, b_99, x_46, y_46, z_38;
        Expect_isTrue(((a_82 = FreeBox__get_Pt0(FreeBox__TransformRigid_Z625426AD(FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 10), Pnt_$ctor_Z7AD9E565(10, 0, 10), Pnt_$ctor_Z7AD9E565(10, 10, 10), Pnt_$ctor_Z7AD9E565(0, 10, 10)]), RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(5, 3, 2)))), (b_99 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_46 = (a_82.X - b_99.X), (y_46 = (a_82.Y - b_99.Y), (z_38 = (a_82.Z - b_99.Z), Math.sqrt(((x_46 * x_46) + (y_46 * y_46)) + (z_38 * z_38)))))))) < 1E-09)("Pt0 should be translated");
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("transformRigid static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        let a_84, b_103, x_47, y_47, z_39;
        Expect_isTrue(((a_84 = FreeBox__get_Pt0(FreeBox__TransformRigid_Z625426AD_1(FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 10), Pnt_$ctor_Z7AD9E565(10, 0, 10), Pnt_$ctor_Z7AD9E565(10, 10, 10), Pnt_$ctor_Z7AD9E565(0, 10, 10)]), RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(5, 3, 2)))), (b_103 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_47 = (a_84.X - b_103.X), (y_47 = (a_84.Y - b_103.Y), (z_39 = (a_84.Z - b_103.Z), Math.sqrt(((x_47 * x_47) + (y_47 * y_47)) + (z_39 * z_39)))))))) < 1E-09)("Pt0 should be translated");
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rotate with identity quaternion", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        let a_86, b_105, x_49, y_49, z_41;
        const box_27 = FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(1, 2, 3), Pnt_$ctor_Z7AD9E565(11, 2, 3), Pnt_$ctor_Z7AD9E565(11, 7, 3), Pnt_$ctor_Z7AD9E565(1, 7, 3), Pnt_$ctor_Z7AD9E565(1, 2, 6), Pnt_$ctor_Z7AD9E565(11, 2, 6), Pnt_$ctor_Z7AD9E565(11, 7, 6), Pnt_$ctor_Z7AD9E565(1, 7, 6)]);
        Expect_isTrue(((a_86 = FreeBox__get_Pt0(FreeBox__Rotate_Z2A007687(box_27, Quaternion_$ctor_77D16AC0(0, 0, 0, 1))), (b_105 = FreeBox__get_Pt0(box_27), (x_49 = (a_86.X - b_105.X), (y_49 = (a_86.Y - b_105.Y), (z_41 = (a_86.Z - b_105.Z), Math.sqrt(((x_49 * x_49) + (y_49 * y_49)) + (z_41 * z_41)))))))) < 1E-09)("Pt0 should be unchanged with identity quaternion");
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rotate 90 degrees around Z axis", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        let a_88, axis_1, angHalf, sa, b_107, x_52, y_52, z_44;
        Expect_isTrue(((a_88 = FreeBox__get_Pt0(FreeBox__Rotate_Z2A007687(FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(2, 0, 0), Pnt_$ctor_Z7AD9E565(2, 1, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0), Pnt_$ctor_Z7AD9E565(1, 0, 1), Pnt_$ctor_Z7AD9E565(2, 0, 1), Pnt_$ctor_Z7AD9E565(2, 1, 1), Pnt_$ctor_Z7AD9E565(1, 1, 1)]), (axis_1 = UnitVec_$ctor_Z7AD9E565(0, 0, 1), (angHalf = ((0.017453292519943295 * 90) * 0.5), (sa = Math.sin(angHalf), Quaternion_$ctor_77D16AC0(axis_1.X * sa, axis_1.Y * sa, axis_1.Z * sa, Math.cos(angHalf))))))), (b_107 = Pnt_$ctor_Z7AD9E565(0, 1, 0), (x_52 = (a_88.X - b_107.X), (y_52 = (a_88.Y - b_107.Y), (z_44 = (a_88.Z - b_107.Z), Math.sqrt(((x_52 * x_52) + (y_52 * y_52)) + (z_44 * z_44)))))))) < 1E-09)("Pt0 should be rotated 90 degrees");
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})(), (() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("rotate static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        let a_90, axis_3, angHalf_1, sa_1, b_111, x_55, y_55, z_47;
        Expect_isTrue(((a_90 = FreeBox__get_Pt0(FreeBox__Rotate_Z2A007687_1(FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(2, 0, 0), Pnt_$ctor_Z7AD9E565(2, 1, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0), Pnt_$ctor_Z7AD9E565(1, 0, 1), Pnt_$ctor_Z7AD9E565(2, 0, 1), Pnt_$ctor_Z7AD9E565(2, 1, 1), Pnt_$ctor_Z7AD9E565(1, 1, 1)]), (axis_3 = UnitVec_$ctor_Z7AD9E565(0, 0, 1), (angHalf_1 = ((0.017453292519943295 * 90) * 0.5), (sa_1 = Math.sin(angHalf_1), Quaternion_$ctor_77D16AC0(axis_3.X * sa_1, axis_3.Y * sa_1, axis_3.Z * sa_1, Math.cos(angHalf_1))))))), (b_111 = Pnt_$ctor_Z7AD9E565(0, 1, 0), (x_55 = (a_90.X - b_111.X), (y_55 = (a_90.Y - b_111.Y), (z_47 = (a_90.Z - b_111.Z), Math.sqrt(((x_55 * x_55) + (y_55 * y_55)) + (z_47 * z_47)))))))) < 1E-09)("Pt0 should be rotated 90 degrees");
        Test_TestCaseBuilder__Zero(builder$0040_31);
    }));
})(), (() => {
    const builder$0040_32 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RotateWithCenter keeps center point fixed", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_32, Test_TestCaseBuilder__Delay_1505(builder$0040_32, () => {
        let a_92, axis_5, angHalf_2, sa_2, b_113, x_58, y_58, z_50;
        Expect_isTrue(((a_92 = FreeBox__get_Pt0(FreeBox__RotateWithCenter_4928E16A(FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(2, 0, 0), Pnt_$ctor_Z7AD9E565(2, 2, 0), Pnt_$ctor_Z7AD9E565(0, 2, 0), Pnt_$ctor_Z7AD9E565(0, 0, 2), Pnt_$ctor_Z7AD9E565(2, 0, 2), Pnt_$ctor_Z7AD9E565(2, 2, 2), Pnt_$ctor_Z7AD9E565(0, 2, 2)]), Pnt_$ctor_Z7AD9E565(1, 1, 1), (axis_5 = UnitVec_$ctor_Z7AD9E565(0, 0, 1), (angHalf_2 = ((0.017453292519943295 * 90) * 0.5), (sa_2 = Math.sin(angHalf_2), Quaternion_$ctor_77D16AC0(axis_5.X * sa_2, axis_5.Y * sa_2, axis_5.Z * sa_2, Math.cos(angHalf_2))))))), (b_113 = Pnt_$ctor_Z7AD9E565(2, 0, 0), (x_58 = (a_92.X - b_113.X), (y_58 = (a_92.Y - b_113.Y), (z_50 = (a_92.Z - b_113.Z), Math.sqrt(((x_58 * x_58) + (y_58 * y_58)) + (z_50 * z_50)))))))) < 1E-09)("Pt0 should be rotated around center");
        Test_TestCaseBuilder__Zero(builder$0040_32);
    }));
})(), (() => {
    const builder$0040_33 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("rotateWithCenter static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_33, Test_TestCaseBuilder__Delay_1505(builder$0040_33, () => {
        let a_94, axis_7, angHalf_3, sa_3, b_117, x_61, y_61, z_53;
        Expect_isTrue(((a_94 = FreeBox__get_Pt0(FreeBox__RotateWithCenter_4928E16A_1(FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(2, 0, 0), Pnt_$ctor_Z7AD9E565(2, 2, 0), Pnt_$ctor_Z7AD9E565(0, 2, 0), Pnt_$ctor_Z7AD9E565(0, 0, 2), Pnt_$ctor_Z7AD9E565(2, 0, 2), Pnt_$ctor_Z7AD9E565(2, 2, 2), Pnt_$ctor_Z7AD9E565(0, 2, 2)]), Pnt_$ctor_Z7AD9E565(1, 1, 1), (axis_7 = UnitVec_$ctor_Z7AD9E565(0, 0, 1), (angHalf_3 = ((0.017453292519943295 * 90) * 0.5), (sa_3 = Math.sin(angHalf_3), Quaternion_$ctor_77D16AC0(axis_7.X * sa_3, axis_7.Y * sa_3, axis_7.Z * sa_3, Math.cos(angHalf_3))))))), (b_117 = Pnt_$ctor_Z7AD9E565(2, 0, 0), (x_61 = (a_94.X - b_117.X), (y_61 = (a_94.Y - b_117.Y), (z_53 = (a_94.Z - b_117.Z), Math.sqrt(((x_61 * x_61) + (y_61 * y_61)) + (z_53 * z_53)))))))) < 1E-09)("Pt0 should be rotated around center");
        Test_TestCaseBuilder__Zero(builder$0040_33);
    }));
})(), (() => {
    const builder$0040_34 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("scale static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_34, Test_TestCaseBuilder__Delay_1505(builder$0040_34, () => {
        let a_96, b_121, x_62, y_62, z_54;
        Expect_isTrue(((a_96 = FreeBox__get_Pt0(FreeBox__Scale_5E38073B_1(FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(1, 2, 3), Pnt_$ctor_Z7AD9E565(11, 2, 3), Pnt_$ctor_Z7AD9E565(11, 7, 3), Pnt_$ctor_Z7AD9E565(1, 7, 3), Pnt_$ctor_Z7AD9E565(1, 2, 6), Pnt_$ctor_Z7AD9E565(11, 2, 6), Pnt_$ctor_Z7AD9E565(11, 7, 6), Pnt_$ctor_Z7AD9E565(1, 7, 6)]), 2)), (b_121 = Pnt_$ctor_Z7AD9E565(2, 4, 6), (x_62 = (a_96.X - b_121.X), (y_62 = (a_96.Y - b_121.Y), (z_54 = (a_96.Z - b_121.Z), Math.sqrt(((x_62 * x_62) + (y_62 * y_62)) + (z_54 * z_54)))))))) < 1E-09)("Pt0 should be scaled");
        Test_TestCaseBuilder__Zero(builder$0040_34);
    }));
})()])), Test_testList("Edge Cases", ofArray([(() => {
    const builder$0040_35 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Box with zero volume (all points coincident)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_35, Test_TestCaseBuilder__Delay_1505(builder$0040_35, () => {
        let copyOfStruct_8, arg_13, arg_1_8, copyOfStruct_9, arg_14, arg_1_9, copyOfStruct_10, arg_15, arg_1_10;
        const box_33 = FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(5, 5, 5), Pnt_$ctor_Z7AD9E565(5, 5, 5), Pnt_$ctor_Z7AD9E565(5, 5, 5), Pnt_$ctor_Z7AD9E565(5, 5, 5), Pnt_$ctor_Z7AD9E565(5, 5, 5), Pnt_$ctor_Z7AD9E565(5, 5, 5), Pnt_$ctor_Z7AD9E565(5, 5, 5), Pnt_$ctor_Z7AD9E565(5, 5, 5)]);
        const actual_8 = FreeBox__get_SizeX(box_33);
        if ((actual_8 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_8, 0, "SizeX should be 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_8 = actual_8, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_63) => (structuralHash(x_63) | 0),
            }) ? ((arg_13 = (0).toString(), (arg_1_8 = actual_8.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_13)(arg_1_8)("SizeX should be 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_8)("SizeX should be 0"));
        }
        const actual_9 = FreeBox__get_SizeY(box_33);
        if ((actual_9 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_9, 0, "SizeY should be 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_9 = actual_9, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_64) => (structuralHash(x_64) | 0),
            }) ? ((arg_14 = (0).toString(), (arg_1_9 = actual_9.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_14)(arg_1_9)("SizeY should be 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_9)("SizeY should be 0"));
        }
        const actual_10 = FreeBox__get_SizeZ(box_33);
        if ((actual_10 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_10, 0, "SizeZ should be 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_10 = actual_10, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_65) => (structuralHash(x_65) | 0),
            }) ? ((arg_15 = (0).toString(), (arg_1_10 = actual_10.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_15)(arg_1_10)("SizeZ should be 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_10)("SizeZ should be 0"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_35);
    }));
})(), (() => {
    const builder$0040_36 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Box with one dimension (line)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_36, Test_TestCaseBuilder__Delay_1505(builder$0040_36, () => {
        let copyOfStruct_11, arg_16, arg_1_11, copyOfStruct_12, arg_17, arg_1_12, copyOfStruct_13, arg_18, arg_1_13;
        const box_34 = FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 5, 5), Pnt_$ctor_Z7AD9E565(10, 5, 5), Pnt_$ctor_Z7AD9E565(10, 5, 5), Pnt_$ctor_Z7AD9E565(0, 5, 5), Pnt_$ctor_Z7AD9E565(0, 5, 5), Pnt_$ctor_Z7AD9E565(10, 5, 5), Pnt_$ctor_Z7AD9E565(10, 5, 5), Pnt_$ctor_Z7AD9E565(0, 5, 5)]);
        const actual_11 = FreeBox__get_SizeX(box_34);
        if ((actual_11 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_11, 10, "SizeX should be 10");
        }
        else {
            throw new Exception(contains((copyOfStruct_11 = actual_11, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_66) => (structuralHash(x_66) | 0),
            }) ? ((arg_16 = (10).toString(), (arg_1_11 = actual_11.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_16)(arg_1_11)("SizeX should be 10")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_11)("SizeX should be 10"));
        }
        const actual_12 = FreeBox__get_SizeY(box_34);
        if ((actual_12 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_12, 0, "SizeY should be 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_12 = actual_12, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_67) => (structuralHash(x_67) | 0),
            }) ? ((arg_17 = (0).toString(), (arg_1_12 = actual_12.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_17)(arg_1_12)("SizeY should be 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_12)("SizeY should be 0"));
        }
        const actual_13 = FreeBox__get_SizeZ(box_34);
        if ((actual_13 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_13, 0, "SizeZ should be 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_13 = actual_13, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_68) => (structuralHash(x_68) | 0),
            }) ? ((arg_18 = (0).toString(), (arg_1_13 = actual_13.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_18)(arg_1_13)("SizeZ should be 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_13)("SizeZ should be 0"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_36);
    }));
})(), (() => {
    const builder$0040_37 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Box with two dimensions (flat)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_37, Test_TestCaseBuilder__Delay_1505(builder$0040_37, () => {
        let copyOfStruct_14, arg_19, arg_1_14, copyOfStruct_15, arg_20, arg_1_15, copyOfStruct_16, arg_21, arg_1_16;
        const box_35 = FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 5), Pnt_$ctor_Z7AD9E565(10, 0, 5), Pnt_$ctor_Z7AD9E565(10, 10, 5), Pnt_$ctor_Z7AD9E565(0, 10, 5), Pnt_$ctor_Z7AD9E565(0, 0, 5), Pnt_$ctor_Z7AD9E565(10, 0, 5), Pnt_$ctor_Z7AD9E565(10, 10, 5), Pnt_$ctor_Z7AD9E565(0, 10, 5)]);
        const actual_14 = FreeBox__get_SizeX(box_35);
        if ((actual_14 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_14, 10, "SizeX should be 10");
        }
        else {
            throw new Exception(contains((copyOfStruct_14 = actual_14, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_69) => (structuralHash(x_69) | 0),
            }) ? ((arg_19 = (10).toString(), (arg_1_14 = actual_14.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_19)(arg_1_14)("SizeX should be 10")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_14)("SizeX should be 10"));
        }
        const actual_15 = FreeBox__get_SizeY(box_35);
        if ((actual_15 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_15, 10, "SizeY should be 10");
        }
        else {
            throw new Exception(contains((copyOfStruct_15 = actual_15, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_70) => (structuralHash(x_70) | 0),
            }) ? ((arg_20 = (10).toString(), (arg_1_15 = actual_15.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_20)(arg_1_15)("SizeY should be 10")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_15)("SizeY should be 10"));
        }
        const actual_16 = FreeBox__get_SizeZ(box_35);
        if ((actual_16 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_16, 0, "SizeZ should be 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_16 = actual_16, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_71) => (structuralHash(x_71) | 0),
            }) ? ((arg_21 = (0).toString(), (arg_1_16 = actual_16.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_21)(arg_1_16)("SizeZ should be 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_16)("SizeZ should be 0"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_37);
    }));
})(), (() => {
    const builder$0040_38 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Non-axis-aligned box (rotated)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_38, Test_TestCaseBuilder__Delay_1505(builder$0040_38, () => {
        const box_36 = FreeBox_createFromEightPoints_Z6C25B48([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 20, 0), Pnt_$ctor_Z7AD9E565(-10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 5), Pnt_$ctor_Z7AD9E565(10, 10, 5), Pnt_$ctor_Z7AD9E565(0, 20, 5), Pnt_$ctor_Z7AD9E565(-10, 10, 5)]);
        Expect_isTrue(FreeBox__get_SizeX(box_36) > 0)("Should have non-zero SizeX");
        Expect_isTrue(FreeBox__get_SizeY(box_36) > 0)("Should have non-zero SizeY");
        Expect_isTrue(FreeBox__get_SizeZ(box_36) > 0)("Should have non-zero SizeZ");
        Test_TestCaseBuilder__Zero(builder$0040_38);
    }));
})()]))]));

