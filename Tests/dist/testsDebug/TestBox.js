
import { Expect_isFalse, Expect_throws, Test_TestCaseBuilder__Zero, Expect_isTrue, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Pnt_$ctor_Z7AD9E565 } from "./Src/Pnt.js";
import { Vec_$ctor_Z7AD9E565 } from "./Src/Vec.js";
import { Box__IntersectRay_4CC2E360 as Box__IntersectRay_4CC2E360_1, Box__Contains_Z394ECE4D as Box__Contains_Z394ECE4D_1, Box_$ctor_5706A3BA } from "./Src/Box.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_1 } from "./Src/Vec.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_1 } from "./Src/Pnt.js";
import { int32ToString, Exception, structuralHash, assertEqual } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { equals, class_type, decimal_type, string_type, bool_type, int32_type, float64_type } from "./fable_modules/fable-library-js.5.0.0/Reflection.js";
import { singleton, contains, ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { printf, toText } from "./fable_modules/fable-library-js.5.0.0/String.js";
import { Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_6181AF53 } from "./Src/TypeExtensions/PPlane.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_2 } from "./Src/Vec.js";
import { BBox_$ctor_76A78260 } from "./Src/BBox.js";
import { Rect2D_createFromDirectionAndSizes_2A9F2837 } from "./Src/Rect2D.js";
import { Pt_$ctor_7B00E9A0 } from "./Src/Pt.js";
import { UnitVc_$ctor_7B00E9A0 } from "./Src/UnitVc.js";
import { Box__IntersectRay_4CC2E360, Box_createFromPlaneAndPoints, Box__get_AreaOfSmallestFace, Box__get_AreaOfBiggestFace, Box_notEquals, Box_equals, Box__get_TopFace, Box__get_BottomFace, Box__get_Faces, Box__get_Edges, Box__get_Points, Box__get_BBox, Box__Contains_Z394ECE4D, Box_expandRelXYZ, Box_expandRel, Box_expandXYZ, Box_expand, Box_translateLocalZ, Box_translateLocalY, Box_translateLocalX, Box_createFromDirsAndPoints, Box_createFromRect2D } from "./Src/Box.js";
import { Rect3D_createFromVectors_6181AF53 } from "./Src/Rect3D.js";
import { failUnit3 } from "./Src/EuclidErrors.js";
import { failTooSmall } from "./Src/EuclidErrors.js";
import { UnitVec_$ctor_Z7AD9E565 } from "./Src/UnitVec.js";
import { min, max } from "./fable_modules/fable-library-js.5.0.0/Double.js";
import { toString } from "./fable_modules/fable-library-js.5.0.0/Types.js";
import { Matrix_createTranslation_Z394EC5F7, Matrix_get_identity } from "./Src/Matrix.js";
import { RigidMatrix_createTranslation_Z394EC5F7 } from "./Src/RigidMatrix.js";
import { Quaternion_$ctor_77D16AC0 } from "./Src/Quaternion.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_2 } from "./Src/Pnt.js";
import { PPlane_$ctor_3CB4665C } from "./Src/PPlane.js";
import { Line3D_$ctor_5A6659A0 } from "./Src/Line3D.js";
import { Line3D_$ctor_5A6659A0 as Line3D_$ctor_5A6659A0_1 } from "./Src/Line3D.js";
import { value as value_6 } from "./fable_modules/fable-library-js.5.0.0/Option.js";

export const tests = Test_testList("Box", ofArray([Test_testList("Constructor and Basic Properties", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createUnchecked from origin and axes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let a_2, b_2, x, y, z, v_1, a_3, b_3, v_3, a_5, b_5, v_5, a_7, b_7;
        const origin = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const xAxis = Vec_$ctor_Z7AD9E565(10, 0, 0);
        const yAxis = Vec_$ctor_Z7AD9E565(0, 5, 0);
        const zAxis = Vec_$ctor_Z7AD9E565(0, 0, 3);
        const box = Box_$ctor_5706A3BA(origin, xAxis, yAxis, zAxis);
        Expect_isTrue(((a_2 = box.Origin, (b_2 = origin, (x = (a_2.X - b_2.X), (y = (a_2.Y - b_2.Y), (z = (a_2.Z - b_2.Z), Math.sqrt(((x * x) + (y * y)) + (z * z)))))))) < 1E-09)("Origin should match");
        Expect_isTrue(((v_1 = ((a_3 = box.Xaxis, (b_3 = xAxis, Vec_$ctor_Z7AD9E565_1(a_3.X - b_3.X, a_3.Y - b_3.Y, a_3.Z - b_3.Z)))), Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z)))) < 1E-09)("Xaxis should match");
        Expect_isTrue(((v_3 = ((a_5 = box.Yaxis, (b_5 = yAxis, Vec_$ctor_Z7AD9E565_1(a_5.X - b_5.X, a_5.Y - b_5.Y, a_5.Z - b_5.Z)))), Math.sqrt(((v_3.X * v_3.X) + (v_3.Y * v_3.Y)) + (v_3.Z * v_3.Z)))) < 1E-09)("Yaxis should match");
        Expect_isTrue(((v_5 = ((a_7 = box.Zaxis, (b_7 = zAxis, Vec_$ctor_Z7AD9E565_1(a_7.X - b_7.X, a_7.Y - b_7.Y, a_7.Z - b_7.Z)))), Math.sqrt(((v_5.X * v_5.X) + (v_5.Y * v_5.Y)) + (v_5.Z * v_5.Z)))) < 1E-09)("Zaxis should match");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("SizeX, SizeY, SizeZ", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const box_1 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let actual;
        const v_6 = box_1.Xaxis;
        actual = Math.sqrt(((v_6.X * v_6.X) + (v_6.Y * v_6.Y)) + (v_6.Z * v_6.Z));
        if ((actual === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual, 10, "SizeX should be 10");
        }
        else {
            let valueType;
            let copyOfStruct = actual;
            valueType = float64_type;
            const primitiveTypes = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg;
            if (contains(valueType, primitiveTypes, {
                Equals: equals,
                GetHashCode: (x_1) => (structuralHash(x_1) | 0),
            })) {
                const arg = (10).toString();
                const arg_1 = actual.toString();
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("SizeX should be 10");
            }
            else {
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual)("SizeX should be 10");
            }
            throw new Exception(errorMsg);
        }
        let actual_1;
        const v_7 = box_1.Yaxis;
        actual_1 = Math.sqrt(((v_7.X * v_7.X) + (v_7.Y * v_7.Y)) + (v_7.Z * v_7.Z));
        if ((actual_1 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, 5, "SizeY should be 5");
        }
        else {
            let valueType_1;
            let copyOfStruct_1 = actual_1;
            valueType_1 = float64_type;
            const primitiveTypes_1 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_1;
            if (contains(valueType_1, primitiveTypes_1, {
                Equals: equals,
                GetHashCode: (x_2) => (structuralHash(x_2) | 0),
            })) {
                const arg_6 = (5).toString();
                const arg_1_1 = actual_1.toString();
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("SizeY should be 5");
            }
            else {
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_1)("SizeY should be 5");
            }
            throw new Exception(errorMsg_1);
        }
        let actual_2;
        const v_8 = box_1.Zaxis;
        actual_2 = Math.sqrt(((v_8.X * v_8.X) + (v_8.Y * v_8.Y)) + (v_8.Z * v_8.Z));
        if ((actual_2 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_2, 3, "SizeZ should be 3");
        }
        else {
            let valueType_2;
            let copyOfStruct_2 = actual_2;
            valueType_2 = float64_type;
            const primitiveTypes_2 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_2;
            if (contains(valueType_2, primitiveTypes_2, {
                Equals: equals,
                GetHashCode: (x_3) => (structuralHash(x_3) | 0),
            })) {
                const arg_7 = (3).toString();
                const arg_1_2 = actual_2.toString();
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("SizeZ should be 3");
            }
            else {
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_2)("SizeZ should be 3");
            }
            throw new Exception(errorMsg_2);
        }
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("SizeXSq, SizeYSq, SizeZSq", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const box_2 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let actual_3;
        const v_9 = box_2.Xaxis;
        actual_3 = (((v_9.X * v_9.X) + (v_9.Y * v_9.Y)) + (v_9.Z * v_9.Z));
        if ((actual_3 === 100) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, 100, "SizeXSq should be 100");
        }
        else {
            let valueType_3;
            let copyOfStruct_3 = actual_3;
            valueType_3 = float64_type;
            const primitiveTypes_3 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_3;
            if (contains(valueType_3, primitiveTypes_3, {
                Equals: equals,
                GetHashCode: (x_4) => (structuralHash(x_4) | 0),
            })) {
                const arg_8 = (100).toString();
                const arg_1_3 = actual_3.toString();
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("SizeXSq should be 100");
            }
            else {
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(100)(actual_3)("SizeXSq should be 100");
            }
            throw new Exception(errorMsg_3);
        }
        let actual_4;
        const v_10 = box_2.Yaxis;
        actual_4 = (((v_10.X * v_10.X) + (v_10.Y * v_10.Y)) + (v_10.Z * v_10.Z));
        if ((actual_4 === 25) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_4, 25, "SizeYSq should be 25");
        }
        else {
            let valueType_4;
            let copyOfStruct_4 = actual_4;
            valueType_4 = float64_type;
            const primitiveTypes_4 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_4;
            if (contains(valueType_4, primitiveTypes_4, {
                Equals: equals,
                GetHashCode: (x_5) => (structuralHash(x_5) | 0),
            })) {
                const arg_9 = (25).toString();
                const arg_1_4 = actual_4.toString();
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("SizeYSq should be 25");
            }
            else {
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(25)(actual_4)("SizeYSq should be 25");
            }
            throw new Exception(errorMsg_4);
        }
        let actual_5;
        const v_11 = box_2.Zaxis;
        actual_5 = (((v_11.X * v_11.X) + (v_11.Y * v_11.Y)) + (v_11.Z * v_11.Z));
        if ((actual_5 === 9) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_5, 9, "SizeZSq should be 9");
        }
        else {
            let valueType_5;
            let copyOfStruct_5 = actual_5;
            valueType_5 = float64_type;
            const primitiveTypes_5 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_5;
            if (contains(valueType_5, primitiveTypes_5, {
                Equals: equals,
                GetHashCode: (x_6) => (structuralHash(x_6) | 0),
            })) {
                const arg_10 = (9).toString();
                const arg_1_5 = actual_5.toString();
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("SizeZSq should be 9");
            }
            else {
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(9)(actual_5)("SizeZSq should be 9");
            }
            throw new Exception(errorMsg_5);
        }
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("FarCorner", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let a_10, b_15, p_2, p_1, p, v_12, v_13, v_14, b_17, x_7, y_7, z_1;
        const box_3 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(1, 2, 3), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        const expected_6 = Pnt_$ctor_Z7AD9E565(11, 7, 6);
        Expect_isTrue(((a_10 = ((b_15 = box_3, (p_2 = ((p_1 = ((p = b_15.Origin, (v_12 = b_15.Xaxis, Pnt_$ctor_Z7AD9E565_1(p.X + v_12.X, p.Y + v_12.Y, p.Z + v_12.Z)))), (v_13 = b_15.Yaxis, Pnt_$ctor_Z7AD9E565_1(p_1.X + v_13.X, p_1.Y + v_13.Y, p_1.Z + v_13.Z)))), (v_14 = b_15.Zaxis, Pnt_$ctor_Z7AD9E565_1(p_2.X + v_14.X, p_2.Y + v_14.Y, p_2.Z + v_14.Z))))), (b_17 = expected_6, (x_7 = (a_10.X - b_17.X), (y_7 = (a_10.Y - b_17.Y), (z_1 = (a_10.Z - b_17.Z), Math.sqrt(((x_7 * x_7) + (y_7 * y_7)) + (z_1 * z_1)))))))) < 1E-09)("FarCorner should be sum of origin and all axes");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Diagonal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let v_16, a_13, b_18, a_12, a_11, b_19, b_20, b_21;
        const box_4 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        const expected_7 = Vec_$ctor_Z7AD9E565(10, 5, 3);
        Expect_isTrue(((v_16 = ((a_13 = ((b_18 = box_4, (a_12 = ((a_11 = b_18.Xaxis, (b_19 = b_18.Yaxis, Vec_$ctor_Z7AD9E565_1(a_11.X + b_19.X, a_11.Y + b_19.Y, a_11.Z + b_19.Z)))), (b_20 = b_18.Zaxis, Vec_$ctor_Z7AD9E565_1(a_12.X + b_20.X, a_12.Y + b_20.Y, a_12.Z + b_20.Z))))), (b_21 = expected_7, Vec_$ctor_Z7AD9E565_1(a_13.X - b_21.X, a_13.Y - b_21.Y, a_13.Z - b_21.Z)))), Math.sqrt(((v_16.X * v_16.X) + (v_16.Y * v_16.Y)) + (v_16.Z * v_16.Z)))) < 1E-09)("Diagonal should be sum of all axes");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Center", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let a_19, b_23, p_5, p_4, p_3, v_17, a_15, v_18, a_16, v_19, a_17, b_25, x_8, y_8, z_2;
        const box_5 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        const expected_8 = Pnt_$ctor_Z7AD9E565(5, 5, 5);
        Expect_isTrue(((a_19 = ((b_23 = box_5, (p_5 = ((p_4 = ((p_3 = b_23.Origin, (v_17 = ((a_15 = b_23.Xaxis, Vec_$ctor_Z7AD9E565_1(a_15.X * 0.5, a_15.Y * 0.5, a_15.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_3.X + v_17.X, p_3.Y + v_17.Y, p_3.Z + v_17.Z)))), (v_18 = ((a_16 = b_23.Yaxis, Vec_$ctor_Z7AD9E565_1(a_16.X * 0.5, a_16.Y * 0.5, a_16.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_4.X + v_18.X, p_4.Y + v_18.Y, p_4.Z + v_18.Z)))), (v_19 = ((a_17 = b_23.Zaxis, Vec_$ctor_Z7AD9E565_1(a_17.X * 0.5, a_17.Y * 0.5, a_17.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_5.X + v_19.X, p_5.Y + v_19.Y, p_5.Z + v_19.Z))))), (b_25 = expected_8, (x_8 = (a_19.X - b_25.X), (y_8 = (a_19.Y - b_25.Y), (z_2 = (a_19.Z - b_25.Z), Math.sqrt(((x_8 * x_8) + (y_8 * y_8)) + (z_2 * z_2)))))))) < 1E-09)("Center should be at (5, 5, 5)");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Volume", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        let v_20, v_21, v_22;
        const box_6 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let actual_6;
        const b_26 = box_6;
        actual_6 = ((((v_20 = b_26.Xaxis, Math.sqrt(((v_20.X * v_20.X) + (v_20.Y * v_20.Y)) + (v_20.Z * v_20.Z)))) * ((v_21 = b_26.Yaxis, Math.sqrt(((v_21.X * v_21.X) + (v_21.Y * v_21.Y)) + (v_21.Z * v_21.Z))))) * ((v_22 = b_26.Zaxis, Math.sqrt(((v_22.X * v_22.X) + (v_22.Y * v_22.Y)) + (v_22.Z * v_22.Z)))));
        if ((actual_6 === 150) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_6, 150, "Volume should be 150");
        }
        else {
            let valueType_6;
            let copyOfStruct_6 = actual_6;
            valueType_6 = float64_type;
            const primitiveTypes_6 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_6;
            if (contains(valueType_6, primitiveTypes_6, {
                Equals: equals,
                GetHashCode: (x_9) => (structuralHash(x_9) | 0),
            })) {
                const arg_11 = (150).toString();
                const arg_1_6 = actual_6.toString();
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_11)(arg_1_6)("Volume should be 150");
            }
            else {
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(150)(actual_6)("Volume should be 150");
            }
            throw new Exception(errorMsg_6);
        }
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})()])), Test_testList("Creation Methods", ofArray([(() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromPlane", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let a_20, a_21, a_22;
        const pl = Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_6181AF53(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565_2(1, 0, 0), Vec_$ctor_Z7AD9E565_2(0, 1, 0));
        let box_7;
        const pl_2 = pl;
        box_7 = Box_$ctor_5706A3BA(pl_2.Origin, (a_20 = pl_2.Xaxis, Vec_$ctor_Z7AD9E565_1(a_20.X * 10, a_20.Y * 10, a_20.Z * 10)), (a_21 = pl_2.Yaxis, Vec_$ctor_Z7AD9E565_1(a_21.X * 5, a_21.Y * 5, a_21.Z * 5)), (a_22 = pl_2.Zaxis, Vec_$ctor_Z7AD9E565_1(a_22.X * 3, a_22.Y * 3, a_22.Z * 3)));
        let actual_7;
        const v_23 = box_7.Xaxis;
        actual_7 = Math.sqrt(((v_23.X * v_23.X) + (v_23.Y * v_23.Y)) + (v_23.Z * v_23.Z));
        if ((actual_7 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, 10, "SizeX should be 10");
        }
        else {
            let valueType_7;
            let copyOfStruct_7 = actual_7;
            valueType_7 = float64_type;
            const primitiveTypes_7 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_7;
            if (contains(valueType_7, primitiveTypes_7, {
                Equals: equals,
                GetHashCode: (x_12) => (structuralHash(x_12) | 0),
            })) {
                const arg_12 = (10).toString();
                const arg_1_7 = actual_7.toString();
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_12)(arg_1_7)("SizeX should be 10");
            }
            else {
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_7)("SizeX should be 10");
            }
            throw new Exception(errorMsg_7);
        }
        let actual_8;
        const v_24 = box_7.Yaxis;
        actual_8 = Math.sqrt(((v_24.X * v_24.X) + (v_24.Y * v_24.Y)) + (v_24.Z * v_24.Z));
        if ((actual_8 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_8, 5, "SizeY should be 5");
        }
        else {
            let valueType_8;
            let copyOfStruct_8 = actual_8;
            valueType_8 = float64_type;
            const primitiveTypes_8 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_8;
            if (contains(valueType_8, primitiveTypes_8, {
                Equals: equals,
                GetHashCode: (x_13) => (structuralHash(x_13) | 0),
            })) {
                const arg_13 = (5).toString();
                const arg_1_8 = actual_8.toString();
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_13)(arg_1_8)("SizeY should be 5");
            }
            else {
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_8)("SizeY should be 5");
            }
            throw new Exception(errorMsg_8);
        }
        let actual_9;
        const v_25 = box_7.Zaxis;
        actual_9 = Math.sqrt(((v_25.X * v_25.X) + (v_25.Y * v_25.Y)) + (v_25.Z * v_25.Z));
        if ((actual_9 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_9, 3, "SizeZ should be 3");
        }
        else {
            let valueType_9;
            let copyOfStruct_9 = actual_9;
            valueType_9 = float64_type;
            const primitiveTypes_9 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_9;
            if (contains(valueType_9, primitiveTypes_9, {
                Equals: equals,
                GetHashCode: (x_14) => (structuralHash(x_14) | 0),
            })) {
                const arg_14 = (3).toString();
                const arg_1_9 = actual_9.toString();
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_14)(arg_1_9)("SizeZ should be 3");
            }
            else {
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_9)("SizeZ should be 3");
            }
            throw new Exception(errorMsg_9);
        }
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromBoundingBox", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let b_32, a_24, f_6, b_33, a_25, f_7, b_34, a_26, f_8, b_35, a_28, b_41, b_39, x_18, y_18, z_5;
        let bbox;
        const a_23 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_30 = Pnt_$ctor_Z7AD9E565(10, 5, 3);
        let minX = a_23.X;
        let maxX;
        if (b_30.X > minX) {
            maxX = b_30.X;
        }
        else {
            minX = b_30.X;
            maxX = a_23.X;
        }
        let minY = a_23.Y;
        let maxY;
        if (b_30.Y > minY) {
            maxY = b_30.Y;
        }
        else {
            minY = b_30.Y;
            maxY = a_23.Y;
        }
        let minZ = a_23.Z;
        let maxZ;
        if (b_30.Z > minZ) {
            maxZ = b_30.Z;
        }
        else {
            minZ = b_30.Z;
            maxZ = a_23.Z;
        }
        bbox = BBox_$ctor_76A78260(minX, minY, minZ, maxX, maxY, maxZ);
        let box_8;
        const b_31 = bbox;
        box_8 = Box_$ctor_5706A3BA((b_32 = b_31, Pnt_$ctor_Z7AD9E565_1(b_32.MinX, b_32.MinY, b_32.MinZ)), (a_24 = Vec_$ctor_Z7AD9E565_2(1, 0, 0), (f_6 = ((b_33 = b_31, b_33.MaxX - b_33.MinX)), Vec_$ctor_Z7AD9E565_1(a_24.X * f_6, a_24.Y * f_6, a_24.Z * f_6))), (a_25 = Vec_$ctor_Z7AD9E565_2(0, 1, 0), (f_7 = ((b_34 = b_31, b_34.MaxY - b_34.MinY)), Vec_$ctor_Z7AD9E565_1(a_25.X * f_7, a_25.Y * f_7, a_25.Z * f_7))), (a_26 = Vec_$ctor_Z7AD9E565_2(0, 0, 1), (f_8 = ((b_35 = b_31, b_35.MaxZ - b_35.MinZ)), Vec_$ctor_Z7AD9E565_1(a_26.X * f_8, a_26.Y * f_8, a_26.Z * f_8))));
        let actual_10;
        const v_26 = box_8.Xaxis;
        actual_10 = Math.sqrt(((v_26.X * v_26.X) + (v_26.Y * v_26.Y)) + (v_26.Z * v_26.Z));
        if ((actual_10 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_10, 10, "SizeX should be 10");
        }
        else {
            let valueType_10;
            let copyOfStruct_10 = actual_10;
            valueType_10 = float64_type;
            const primitiveTypes_10 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_10;
            if (contains(valueType_10, primitiveTypes_10, {
                Equals: equals,
                GetHashCode: (x_15) => (structuralHash(x_15) | 0),
            })) {
                const arg_15 = (10).toString();
                const arg_1_10 = actual_10.toString();
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_15)(arg_1_10)("SizeX should be 10");
            }
            else {
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_10)("SizeX should be 10");
            }
            throw new Exception(errorMsg_10);
        }
        let actual_11;
        const v_27 = box_8.Yaxis;
        actual_11 = Math.sqrt(((v_27.X * v_27.X) + (v_27.Y * v_27.Y)) + (v_27.Z * v_27.Z));
        if ((actual_11 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_11, 5, "SizeY should be 5");
        }
        else {
            let valueType_11;
            let copyOfStruct_11 = actual_11;
            valueType_11 = float64_type;
            const primitiveTypes_11 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_11;
            if (contains(valueType_11, primitiveTypes_11, {
                Equals: equals,
                GetHashCode: (x_16) => (structuralHash(x_16) | 0),
            })) {
                const arg_16 = (5).toString();
                const arg_1_11 = actual_11.toString();
                errorMsg_11 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_16)(arg_1_11)("SizeY should be 5");
            }
            else {
                errorMsg_11 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_11)("SizeY should be 5");
            }
            throw new Exception(errorMsg_11);
        }
        let actual_12;
        const v_28 = box_8.Zaxis;
        actual_12 = Math.sqrt(((v_28.X * v_28.X) + (v_28.Y * v_28.Y)) + (v_28.Z * v_28.Z));
        if ((actual_12 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_12, 3, "SizeZ should be 3");
        }
        else {
            let valueType_12;
            let copyOfStruct_12 = actual_12;
            valueType_12 = float64_type;
            const primitiveTypes_12 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_12;
            if (contains(valueType_12, primitiveTypes_12, {
                Equals: equals,
                GetHashCode: (x_17) => (structuralHash(x_17) | 0),
            })) {
                const arg_17 = (3).toString();
                const arg_1_12 = actual_12.toString();
                errorMsg_12 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_17)(arg_1_12)("SizeZ should be 3");
            }
            else {
                errorMsg_12 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_12)("SizeZ should be 3");
            }
            throw new Exception(errorMsg_12);
        }
        Expect_isTrue(((a_28 = box_8.Origin, (b_41 = ((b_39 = bbox, Pnt_$ctor_Z7AD9E565_1(b_39.MinX, b_39.MinY, b_39.MinZ))), (x_18 = (a_28.X - b_41.X), (y_18 = (a_28.Y - b_41.Y), (z_5 = (a_28.Z - b_41.Z), Math.sqrt(((x_18 * x_18) + (y_18 * y_18)) + (z_5 * z_5)))))))) < 1E-09)("Origin should be at BBox min point");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromRect2D", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const rect = Rect2D_createFromDirectionAndSizes_2A9F2837(Pt_$ctor_7B00E9A0(0, 0), UnitVc_$ctor_7B00E9A0(1, 0), 10, 5);
        const box_9 = Box_createFromRect2D(2, 8, rect);
        let actual_13;
        const v_29 = box_9.Xaxis;
        actual_13 = Math.sqrt(((v_29.X * v_29.X) + (v_29.Y * v_29.Y)) + (v_29.Z * v_29.Z));
        if ((actual_13 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_13, 10, "SizeX should be 10");
        }
        else {
            let valueType_13;
            let copyOfStruct_13 = actual_13;
            valueType_13 = float64_type;
            const primitiveTypes_13 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_13;
            if (contains(valueType_13, primitiveTypes_13, {
                Equals: equals,
                GetHashCode: (x_20) => (structuralHash(x_20) | 0),
            })) {
                const arg_18 = (10).toString();
                const arg_1_13 = actual_13.toString();
                errorMsg_13 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_18)(arg_1_13)("SizeX should be 10");
            }
            else {
                errorMsg_13 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_13)("SizeX should be 10");
            }
            throw new Exception(errorMsg_13);
        }
        let actual_14;
        const v_30 = box_9.Yaxis;
        actual_14 = Math.sqrt(((v_30.X * v_30.X) + (v_30.Y * v_30.Y)) + (v_30.Z * v_30.Z));
        if ((actual_14 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_14, 5, "SizeY should be 5");
        }
        else {
            let valueType_14;
            let copyOfStruct_14 = actual_14;
            valueType_14 = float64_type;
            const primitiveTypes_14 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_14;
            if (contains(valueType_14, primitiveTypes_14, {
                Equals: equals,
                GetHashCode: (x_21) => (structuralHash(x_21) | 0),
            })) {
                const arg_19 = (5).toString();
                const arg_1_14 = actual_14.toString();
                errorMsg_14 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_19)(arg_1_14)("SizeY should be 5");
            }
            else {
                errorMsg_14 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_14)("SizeY should be 5");
            }
            throw new Exception(errorMsg_14);
        }
        let actual_15;
        const v_31 = box_9.Zaxis;
        actual_15 = Math.sqrt(((v_31.X * v_31.X) + (v_31.Y * v_31.Y)) + (v_31.Z * v_31.Z));
        if ((actual_15 === 6) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_15, 6, "SizeZ should be 6");
        }
        else {
            let valueType_15;
            let copyOfStruct_15 = actual_15;
            valueType_15 = float64_type;
            const primitiveTypes_15 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_15;
            if (contains(valueType_15, primitiveTypes_15, {
                Equals: equals,
                GetHashCode: (x_22) => (structuralHash(x_22) | 0),
            })) {
                const arg_20 = (6).toString();
                const arg_1_15 = actual_15.toString();
                errorMsg_15 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_20)(arg_1_15)("SizeZ should be 6");
            }
            else {
                errorMsg_15 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(6)(actual_15)("SizeZ should be 6");
            }
            throw new Exception(errorMsg_15);
        }
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromRect3D", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let a_29, a_30, p_6, v_34, v_32, l, v_33, a_32, f_11, v_35, desiredLength_1, l_1, v_36, a_33, f_12;
        const rect_1 = Rect3D_createFromVectors_6181AF53(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), (a_29 = Vec_$ctor_Z7AD9E565_2(1, 0, 0), Vec_$ctor_Z7AD9E565_1(a_29.X * 10, a_29.Y * 10, a_29.Z * 10)), (a_30 = Vec_$ctor_Z7AD9E565_2(0, 1, 0), Vec_$ctor_Z7AD9E565_1(a_30.X * 5, a_30.Y * 5, a_30.Z * 5)));
        let box_10;
        const r_2 = rect_1;
        let z_6;
        const a_31 = r_2.Xaxis;
        const b_45 = r_2.Yaxis;
        z_6 = Vec_$ctor_Z7AD9E565_1((a_31.Y * b_45.Z) - (a_31.Z * b_45.Y), (a_31.Z * b_45.X) - (a_31.X * b_45.Z), (a_31.X * b_45.Y) - (a_31.Y * b_45.X));
        box_10 = Box_$ctor_5706A3BA((p_6 = r_2.Origin, (v_34 = ((v_32 = z_6, (l = ((v_33 = v_32, Math.sqrt(((v_33.X * v_33.X) + (v_33.Y * v_33.Y)) + (v_33.Z * v_33.Z)))), (!(l > 1E-12) ? failUnit3("Vec.WithLength", v_32.X, v_32.Y, v_32.Z) : undefined, (a_32 = v_32, (f_11 = (2 / l), Vec_$ctor_Z7AD9E565_1(a_32.X * f_11, a_32.Y * f_11, a_32.Z * f_11))))))), Pnt_$ctor_Z7AD9E565_1(p_6.X + v_34.X, p_6.Y + v_34.Y, p_6.Z + v_34.Z))), r_2.Xaxis, r_2.Yaxis, (v_35 = z_6, (desiredLength_1 = (8 - 2), (l_1 = ((v_36 = v_35, Math.sqrt(((v_36.X * v_36.X) + (v_36.Y * v_36.Y)) + (v_36.Z * v_36.Z)))), (!(l_1 > 1E-12) ? failUnit3("Vec.WithLength", v_35.X, v_35.Y, v_35.Z) : undefined, (a_33 = v_35, (f_12 = (desiredLength_1 / l_1), Vec_$ctor_Z7AD9E565_1(a_33.X * f_12, a_33.Y * f_12, a_33.Z * f_12))))))));
        let actual_16;
        const v_37 = box_10.Xaxis;
        actual_16 = Math.sqrt(((v_37.X * v_37.X) + (v_37.Y * v_37.Y)) + (v_37.Z * v_37.Z));
        if ((actual_16 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_16, 10, "SizeX should be 10");
        }
        else {
            let valueType_16;
            let copyOfStruct_16 = actual_16;
            valueType_16 = float64_type;
            const primitiveTypes_16 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_16;
            if (contains(valueType_16, primitiveTypes_16, {
                Equals: equals,
                GetHashCode: (x_25) => (structuralHash(x_25) | 0),
            })) {
                const arg_21 = (10).toString();
                const arg_1_16 = actual_16.toString();
                errorMsg_16 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_21)(arg_1_16)("SizeX should be 10");
            }
            else {
                errorMsg_16 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_16)("SizeX should be 10");
            }
            throw new Exception(errorMsg_16);
        }
        let actual_17;
        const v_38 = box_10.Yaxis;
        actual_17 = Math.sqrt(((v_38.X * v_38.X) + (v_38.Y * v_38.Y)) + (v_38.Z * v_38.Z));
        if ((actual_17 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_17, 5, "SizeY should be 5");
        }
        else {
            let valueType_17;
            let copyOfStruct_17 = actual_17;
            valueType_17 = float64_type;
            const primitiveTypes_17 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_17;
            if (contains(valueType_17, primitiveTypes_17, {
                Equals: equals,
                GetHashCode: (x_26) => (structuralHash(x_26) | 0),
            })) {
                const arg_22 = (5).toString();
                const arg_1_17 = actual_17.toString();
                errorMsg_17 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_22)(arg_1_17)("SizeY should be 5");
            }
            else {
                errorMsg_17 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_17)("SizeY should be 5");
            }
            throw new Exception(errorMsg_17);
        }
        let actual_18;
        const v_39 = box_10.Zaxis;
        actual_18 = Math.sqrt(((v_39.X * v_39.X) + (v_39.Y * v_39.Y)) + (v_39.Z * v_39.Z));
        if ((actual_18 === 6) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_18, 6, "SizeZ should be 6");
        }
        else {
            let valueType_18;
            let copyOfStruct_18 = actual_18;
            valueType_18 = float64_type;
            const primitiveTypes_18 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_18;
            if (contains(valueType_18, primitiveTypes_18, {
                Equals: equals,
                GetHashCode: (x_27) => (structuralHash(x_27) | 0),
            })) {
                const arg_23 = (6).toString();
                const arg_1_18 = actual_18.toString();
                errorMsg_18 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_23)(arg_1_18)("SizeZ should be 6");
            }
            else {
                errorMsg_18 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(6)(actual_18)("SizeZ should be 6");
            }
            throw new Exception(errorMsg_18);
        }
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromDirsAndPoints with multiple points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        let v_40;
        const pts = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(0, 5, 0), Pnt_$ctor_Z7AD9E565(0, 0, 3)];
        const box_11 = Box_createFromDirsAndPoints(Vec_$ctor_Z7AD9E565_2(1, 0, 0), Vec_$ctor_Z7AD9E565_2(0, 1, 0), pts);
        Expect_isTrue(((v_40 = box_11.Xaxis, Math.sqrt(((v_40.X * v_40.X) + (v_40.Y * v_40.Y)) + (v_40.Z * v_40.Z)))) >= 0)("Should create valid box");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromDirsAndPoints rejects too few points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        const pts_2 = [Pnt_$ctor_Z7AD9E565(0, 0, 0)];
        Expect_throws(() => {
            Box_createFromDirsAndPoints(Vec_$ctor_Z7AD9E565_2(1, 0, 0), Vec_$ctor_Z7AD9E565_2(0, 1, 0), pts_2);
        }, "Should throw with just 1 point");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromDirsAndPoints rejects zero-length dirX", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        const pts_4 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)];
        Expect_throws(() => {
            Box_createFromDirsAndPoints(Vec_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565_2(0, 1, 0), pts_4);
        }, "Should throw with zero dirX");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromDirsAndPoints rejects zero-length dirY", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        const pts_6 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)];
        Expect_throws(() => {
            Box_createFromDirsAndPoints(Vec_$ctor_Z7AD9E565_2(1, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 0, 0), pts_6);
        }, "Should throw with zero dirY");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})()])), Test_testList("Unit Axis Methods", ofArray([(() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("XaxisUnit for standard box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        let v_43, a_35, v_41, b_50;
        const box_12 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let unitX;
        const r_3 = box_12;
        const a_34 = r_3.Xaxis;
        const x_28 = a_34.X;
        const y_26 = a_34.Y;
        const z_7 = a_34.Z;
        const sqLen = ((x_28 * x_28) + (y_26 * y_26)) + (z_7 * z_7);
        if (!(sqLen > 1E-24)) {
            failTooSmall("Box.XaxisUnit Xaxis", r_3);
        }
        const f_13 = 1 / Math.sqrt(sqLen);
        unitX = UnitVec_$ctor_Z7AD9E565(x_28 * f_13, y_26 * f_13, z_7 * f_13);
        Expect_isTrue(((v_43 = ((a_35 = ((v_41 = unitX, Vec_$ctor_Z7AD9E565_2(v_41.X, v_41.Y, v_41.Z))), (b_50 = Vec_$ctor_Z7AD9E565(1, 0, 0), Vec_$ctor_Z7AD9E565_1(a_35.X - b_50.X, a_35.Y - b_50.Y, a_35.Z - b_50.Z)))), Math.sqrt(((v_43.X * v_43.X) + (v_43.Y * v_43.Y)) + (v_43.Z * v_43.Z)))) < 1E-09)("XaxisUnit should be (1, 0, 0)");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("YaxisUnit for standard box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let v_46, a_38, v_44, b_52;
        const box_13 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let unitY;
        const r_4 = box_13;
        const a_37 = r_4.Yaxis;
        const x_31 = a_37.X;
        const y_28 = a_37.Y;
        const z_9 = a_37.Z;
        const sqLen_1 = ((x_31 * x_31) + (y_28 * y_28)) + (z_9 * z_9);
        if (!(sqLen_1 > 1E-24)) {
            failTooSmall("Box.YaxisUnit Yaxis", r_4);
        }
        const f_14 = 1 / Math.sqrt(sqLen_1);
        unitY = UnitVec_$ctor_Z7AD9E565(x_31 * f_14, y_28 * f_14, z_9 * f_14);
        Expect_isTrue(((v_46 = ((a_38 = ((v_44 = unitY, Vec_$ctor_Z7AD9E565_2(v_44.X, v_44.Y, v_44.Z))), (b_52 = Vec_$ctor_Z7AD9E565(0, 1, 0), Vec_$ctor_Z7AD9E565_1(a_38.X - b_52.X, a_38.Y - b_52.Y, a_38.Z - b_52.Z)))), Math.sqrt(((v_46.X * v_46.X) + (v_46.Y * v_46.Y)) + (v_46.Z * v_46.Z)))) < 1E-09)("YaxisUnit should be (0, 1, 0)");
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ZaxisUnit for standard box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        let v_49, a_41, v_47, b_54;
        const box_14 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let unitZ;
        const r_5 = box_14;
        const a_40 = r_5.Zaxis;
        const x_34 = a_40.X;
        const y_30 = a_40.Y;
        const z_11 = a_40.Z;
        const sqLen_2 = ((x_34 * x_34) + (y_30 * y_30)) + (z_11 * z_11);
        if (!(sqLen_2 > 1E-24)) {
            failTooSmall("Box.ZaxisUnit Zaxis", r_5);
        }
        const f_15 = 1 / Math.sqrt(sqLen_2);
        unitZ = UnitVec_$ctor_Z7AD9E565(x_34 * f_15, y_30 * f_15, z_11 * f_15);
        Expect_isTrue(((v_49 = ((a_41 = ((v_47 = unitZ, Vec_$ctor_Z7AD9E565_2(v_47.X, v_47.Y, v_47.Z))), (b_54 = Vec_$ctor_Z7AD9E565(0, 0, 1), Vec_$ctor_Z7AD9E565_1(a_41.X - b_54.X, a_41.Y - b_54.Y, a_41.Z - b_54.Z)))), Math.sqrt(((v_49.X * v_49.X) + (v_49.Y * v_49.Y)) + (v_49.Z * v_49.Z)))) < 1E-09)("ZaxisUnit should be (0, 0, 1)");
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("XaxisUnit throws for zero-length axis", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        const box_15 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        Expect_throws(() => {
            let r_6, a_43, x_37, y_32, z_13, sqLen_3, f_16;
            (r_6 = box_15, (a_43 = r_6.Xaxis, (x_37 = a_43.X, (y_32 = a_43.Y, (z_13 = a_43.Z, (sqLen_3 = (((x_37 * x_37) + (y_32 * y_32)) + (z_13 * z_13)), (!(sqLen_3 > 1E-24) ? failTooSmall("Box.XaxisUnit Xaxis", r_6) : undefined, (f_16 = (1 / Math.sqrt(sqLen_3)), UnitVec_$ctor_Z7AD9E565(x_37 * f_16, y_32 * f_16, z_13 * f_16)))))))));
        }, "Should throw for zero-length Xaxis");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})()])), Test_testList("Evaluation", ofArray([(() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("EvaluateAt origin (0, 0, 0)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        let a_48, b_58, x_40, y_34, z_15;
        const box_16 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(1, 2, 3), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let pt;
        const b_56 = box_16;
        let p_9;
        let p_8;
        const p_7 = b_56.Origin;
        let v_50;
        const a_44 = b_56.Xaxis;
        v_50 = Vec_$ctor_Z7AD9E565_1(a_44.X * 0, a_44.Y * 0, a_44.Z * 0);
        p_8 = Pnt_$ctor_Z7AD9E565_1(p_7.X + v_50.X, p_7.Y + v_50.Y, p_7.Z + v_50.Z);
        let v_51;
        const a_45 = b_56.Yaxis;
        v_51 = Vec_$ctor_Z7AD9E565_1(a_45.X * 0, a_45.Y * 0, a_45.Z * 0);
        p_9 = Pnt_$ctor_Z7AD9E565_1(p_8.X + v_51.X, p_8.Y + v_51.Y, p_8.Z + v_51.Z);
        let v_52;
        const a_46 = b_56.Zaxis;
        v_52 = Vec_$ctor_Z7AD9E565_1(a_46.X * 0, a_46.Y * 0, a_46.Z * 0);
        pt = Pnt_$ctor_Z7AD9E565_1(p_9.X + v_52.X, p_9.Y + v_52.Y, p_9.Z + v_52.Z);
        Expect_isTrue(((a_48 = pt, (b_58 = box_16.Origin, (x_40 = (a_48.X - b_58.X), (y_34 = (a_48.Y - b_58.Y), (z_15 = (a_48.Z - b_58.Z), Math.sqrt(((x_40 * x_40) + (y_34 * y_34)) + (z_15 * z_15)))))))) < 1E-09)("Should return Origin at (0, 0, 0)");
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("EvaluateAt far corner (1, 1, 1)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        let a_53, b_62, b_60, p_15, p_14, p_13, v_56, v_57, v_58, x_41, y_35, z_16;
        const box_17 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(1, 2, 3), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let pt_1;
        const b_59 = box_17;
        let p_12;
        let p_11;
        const p_10 = b_59.Origin;
        let v_53;
        const a_49 = b_59.Xaxis;
        v_53 = Vec_$ctor_Z7AD9E565_1(a_49.X * 1, a_49.Y * 1, a_49.Z * 1);
        p_11 = Pnt_$ctor_Z7AD9E565_1(p_10.X + v_53.X, p_10.Y + v_53.Y, p_10.Z + v_53.Z);
        let v_54;
        const a_50 = b_59.Yaxis;
        v_54 = Vec_$ctor_Z7AD9E565_1(a_50.X * 1, a_50.Y * 1, a_50.Z * 1);
        p_12 = Pnt_$ctor_Z7AD9E565_1(p_11.X + v_54.X, p_11.Y + v_54.Y, p_11.Z + v_54.Z);
        let v_55;
        const a_51 = b_59.Zaxis;
        v_55 = Vec_$ctor_Z7AD9E565_1(a_51.X * 1, a_51.Y * 1, a_51.Z * 1);
        pt_1 = Pnt_$ctor_Z7AD9E565_1(p_12.X + v_55.X, p_12.Y + v_55.Y, p_12.Z + v_55.Z);
        Expect_isTrue(((a_53 = pt_1, (b_62 = ((b_60 = box_17, (p_15 = ((p_14 = ((p_13 = b_60.Origin, (v_56 = b_60.Xaxis, Pnt_$ctor_Z7AD9E565_1(p_13.X + v_56.X, p_13.Y + v_56.Y, p_13.Z + v_56.Z)))), (v_57 = b_60.Yaxis, Pnt_$ctor_Z7AD9E565_1(p_14.X + v_57.X, p_14.Y + v_57.Y, p_14.Z + v_57.Z)))), (v_58 = b_60.Zaxis, Pnt_$ctor_Z7AD9E565_1(p_15.X + v_58.X, p_15.Y + v_58.Y, p_15.Z + v_58.Z))))), (x_41 = (a_53.X - b_62.X), (y_35 = (a_53.Y - b_62.Y), (z_16 = (a_53.Z - b_62.Z), Math.sqrt(((x_41 * x_41) + (y_35 * y_35)) + (z_16 * z_16)))))))) < 1E-09)("Should return FarCorner at (1, 1, 1)");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("EvaluateAt center (0.5, 0.5, 0.5)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        let a_58, b_65, x_42, y_36, z_17;
        const box_18 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        let pt_2;
        const b_63 = box_18;
        let p_18;
        let p_17;
        const p_16 = b_63.Origin;
        let v_59;
        const a_54 = b_63.Xaxis;
        v_59 = Vec_$ctor_Z7AD9E565_1(a_54.X * 0.5, a_54.Y * 0.5, a_54.Z * 0.5);
        p_17 = Pnt_$ctor_Z7AD9E565_1(p_16.X + v_59.X, p_16.Y + v_59.Y, p_16.Z + v_59.Z);
        let v_60;
        const a_55 = b_63.Yaxis;
        v_60 = Vec_$ctor_Z7AD9E565_1(a_55.X * 0.5, a_55.Y * 0.5, a_55.Z * 0.5);
        p_18 = Pnt_$ctor_Z7AD9E565_1(p_17.X + v_60.X, p_17.Y + v_60.Y, p_17.Z + v_60.Z);
        let v_61;
        const a_56 = b_63.Zaxis;
        v_61 = Vec_$ctor_Z7AD9E565_1(a_56.X * 0.5, a_56.Y * 0.5, a_56.Z * 0.5);
        pt_2 = Pnt_$ctor_Z7AD9E565_1(p_18.X + v_61.X, p_18.Y + v_61.Y, p_18.Z + v_61.Z);
        Expect_isTrue(((a_58 = pt_2, (b_65 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (x_42 = (a_58.X - b_65.X), (y_36 = (a_58.Y - b_65.Y), (z_17 = (a_58.Z - b_65.Z), Math.sqrt(((x_42 * x_42) + (y_36 * y_36)) + (z_17 * z_17)))))))) < 1E-09)("Should return center at (0.5, 0.5, 0.5)");
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})()])), Test_testList("Size Methods", ofArray([(() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("LongestEdge", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        const box_19 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let actual_19;
        const b_66 = box_19;
        let x_43;
        const v_62 = b_66.Xaxis;
        x_43 = (((v_62.X * v_62.X) + (v_62.Y * v_62.Y)) + (v_62.Z * v_62.Z));
        let y_37;
        const v_63 = b_66.Yaxis;
        y_37 = (((v_63.X * v_63.X) + (v_63.Y * v_63.Y)) + (v_63.Z * v_63.Z));
        let z_18;
        const v_64 = b_66.Zaxis;
        z_18 = (((v_64.X * v_64.X) + (v_64.Y * v_64.Y)) + (v_64.Z * v_64.Z));
        actual_19 = Math.sqrt(max(max(x_43, y_37), z_18));
        if ((actual_19 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_19, 10, "Longest edge should be 10");
        }
        else {
            let valueType_19;
            let copyOfStruct_19 = actual_19;
            valueType_19 = float64_type;
            const primitiveTypes_19 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_19;
            if (contains(valueType_19, primitiveTypes_19, {
                Equals: equals,
                GetHashCode: (x_44) => (structuralHash(x_44) | 0),
            })) {
                const arg_24 = (10).toString();
                const arg_1_19 = actual_19.toString();
                errorMsg_19 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_24)(arg_1_19)("Longest edge should be 10");
            }
            else {
                errorMsg_19 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_19)("Longest edge should be 10");
            }
            throw new Exception(errorMsg_19);
        }
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ShortestEdge", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        const box_20 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let actual_20;
        const b_67 = box_20;
        let x_45;
        const v_65 = b_67.Xaxis;
        x_45 = (((v_65.X * v_65.X) + (v_65.Y * v_65.Y)) + (v_65.Z * v_65.Z));
        let y_39;
        const v_66 = b_67.Yaxis;
        y_39 = (((v_66.X * v_66.X) + (v_66.Y * v_66.Y)) + (v_66.Z * v_66.Z));
        let z_19;
        const v_67 = b_67.Zaxis;
        z_19 = (((v_67.X * v_67.X) + (v_67.Y * v_67.Y)) + (v_67.Z * v_67.Z));
        actual_20 = Math.sqrt(min(min(x_45, y_39), z_19));
        if ((actual_20 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_20, 3, "Shortest edge should be 3");
        }
        else {
            let valueType_20;
            let copyOfStruct_20 = actual_20;
            valueType_20 = float64_type;
            const primitiveTypes_20 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_20;
            if (contains(valueType_20, primitiveTypes_20, {
                Equals: equals,
                GetHashCode: (x_46) => (structuralHash(x_46) | 0),
            })) {
                const arg_25 = (3).toString();
                const arg_1_20 = actual_20.toString();
                errorMsg_20 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_25)(arg_1_20)("Shortest edge should be 3");
            }
            else {
                errorMsg_20 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_20)("Shortest edge should be 3");
            }
            throw new Exception(errorMsg_20);
        }
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("LongestEdgeSq", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        const box_21 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let actual_21;
        const b_68 = box_21;
        let x_47;
        const v_68 = b_68.Xaxis;
        x_47 = (((v_68.X * v_68.X) + (v_68.Y * v_68.Y)) + (v_68.Z * v_68.Z));
        let y_41;
        const v_69 = b_68.Yaxis;
        y_41 = (((v_69.X * v_69.X) + (v_69.Y * v_69.Y)) + (v_69.Z * v_69.Z));
        let z_20;
        const v_70 = b_68.Zaxis;
        z_20 = (((v_70.X * v_70.X) + (v_70.Y * v_70.Y)) + (v_70.Z * v_70.Z));
        actual_21 = max(max(x_47, y_41), z_20);
        if ((actual_21 === 100) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_21, 100, "Longest edge squared should be 100");
        }
        else {
            let valueType_21;
            let copyOfStruct_21 = actual_21;
            valueType_21 = float64_type;
            const primitiveTypes_21 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_21;
            if (contains(valueType_21, primitiveTypes_21, {
                Equals: equals,
                GetHashCode: (x_48) => (structuralHash(x_48) | 0),
            })) {
                const arg_26 = (100).toString();
                const arg_1_21 = actual_21.toString();
                errorMsg_21 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_26)(arg_1_21)("Longest edge squared should be 100");
            }
            else {
                errorMsg_21 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(100)(actual_21)("Longest edge squared should be 100");
            }
            throw new Exception(errorMsg_21);
        }
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ShortestEdgeSq", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        const box_22 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let actual_22;
        const b_69 = box_22;
        let x_49;
        const v_71 = b_69.Xaxis;
        x_49 = (((v_71.X * v_71.X) + (v_71.Y * v_71.Y)) + (v_71.Z * v_71.Z));
        let y_43;
        const v_72 = b_69.Yaxis;
        y_43 = (((v_72.X * v_72.X) + (v_72.Y * v_72.Y)) + (v_72.Z * v_72.Z));
        let z_21;
        const v_73 = b_69.Zaxis;
        z_21 = (((v_73.X * v_73.X) + (v_73.Y * v_73.Y)) + (v_73.Z * v_73.Z));
        actual_22 = min(min(x_49, y_43), z_21);
        if ((actual_22 === 9) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_22, 9, "Shortest edge squared should be 9");
        }
        else {
            let valueType_22;
            let copyOfStruct_22 = actual_22;
            valueType_22 = float64_type;
            const primitiveTypes_22 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_22;
            if (contains(valueType_22, primitiveTypes_22, {
                Equals: equals,
                GetHashCode: (x_50) => (structuralHash(x_50) | 0),
            })) {
                const arg_27 = (9).toString();
                const arg_1_22 = actual_22.toString();
                errorMsg_22 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_27)(arg_1_22)("Shortest edge squared should be 9");
            }
            else {
                errorMsg_22 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(9)(actual_22)("Shortest edge squared should be 9");
            }
            throw new Exception(errorMsg_22);
        }
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})()])), Test_testList("Validation Methods", ofArray([(() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsZero for tiny box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        let b_70, v_74, v_75, v_76;
        const box_23 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(1E-13, 0, 0), Vec_$ctor_Z7AD9E565(0, 1E-13, 0), Vec_$ctor_Z7AD9E565(0, 0, 1E-13));
        Expect_isTrue((b_70 = box_23, (!(((v_74 = b_70.Xaxis, ((v_74.X * v_74.X) + (v_74.Y * v_74.Y)) + (v_74.Z * v_74.Z))) > 1E-24) && !(((v_75 = b_70.Yaxis, ((v_75.X * v_75.X) + (v_75.Y * v_75.Y)) + (v_75.Z * v_75.Z))) > 1E-24)) && !(((v_76 = b_70.Zaxis, ((v_76.X * v_76.X) + (v_76.Y * v_76.Y)) + (v_76.Z * v_76.Z))) > 1E-24)))("Tiny box should be zero");
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsPoint is same as IsZero", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        let v_77, v_78, v_79, v_80, v_81, v_82;
        const box_24 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(1E-13, 0, 0), Vec_$ctor_Z7AD9E565(0, 1E-13, 0), Vec_$ctor_Z7AD9E565(0, 0, 1E-13));
        let actual_23;
        const b_71 = box_24;
        actual_23 = ((!(((v_77 = b_71.Xaxis, ((v_77.X * v_77.X) + (v_77.Y * v_77.Y)) + (v_77.Z * v_77.Z))) > 1E-24) && !(((v_78 = b_71.Yaxis, ((v_78.X * v_78.X) + (v_78.Y * v_78.Y)) + (v_78.Z * v_78.Z))) > 1E-24)) && !(((v_79 = b_71.Zaxis, ((v_79.X * v_79.X) + (v_79.Y * v_79.Y)) + (v_79.Z * v_79.Z))) > 1E-24));
        let expected_26;
        const b_72 = box_24;
        expected_26 = ((!(((v_80 = b_72.Xaxis, ((v_80.X * v_80.X) + (v_80.Y * v_80.Y)) + (v_80.Z * v_80.Z))) > 1E-24) && !(((v_81 = b_72.Yaxis, ((v_81.X * v_81.X) + (v_81.Y * v_81.Y)) + (v_81.Z * v_81.Z))) > 1E-24)) && !(((v_82 = b_72.Zaxis, ((v_82.X * v_82.X) + (v_82.Y * v_82.Y)) + (v_82.Z * v_82.Z))) > 1E-24));
        if ((actual_23 === expected_26) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_23, expected_26, "IsPoint should equal IsZero");
        }
        else {
            let valueType_23;
            let copyOfStruct_23 = actual_23;
            valueType_23 = bool_type;
            const primitiveTypes_23 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_23;
            if (contains(valueType_23, primitiveTypes_23, {
                Equals: equals,
                GetHashCode: (x_60) => (structuralHash(x_60) | 0),
            })) {
                const arg_28 = toString(expected_26);
                const arg_1_23 = toString(actual_23);
                errorMsg_23 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_28)(arg_1_23)("IsPoint should equal IsZero");
            }
            else {
                errorMsg_23 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_26)(actual_23)("IsPoint should equal IsZero");
            }
            throw new Exception(errorMsg_23);
        }
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsLine for box with one dimension", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        let b_74, v_83, v_84, v_85;
        const box_25 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 1E-13, 0), Vec_$ctor_Z7AD9E565(0, 0, 1E-13));
        Expect_isTrue(((b_74 = box_25, (((((v_83 = b_74.Xaxis, ((v_83.X * v_83.X) + (v_83.Y * v_83.Y)) + (v_83.Z * v_83.Z))) > 1E-24) ? 0 : 1) + ((((v_84 = b_74.Yaxis, ((v_84.X * v_84.X) + (v_84.Y * v_84.Y)) + (v_84.Z * v_84.Z))) > 1E-24) ? 0 : 1)) + ((((v_85 = b_74.Zaxis, ((v_85.X * v_85.X) + (v_85.Y * v_85.Y)) + (v_85.Z * v_85.Z))) > 1E-24) ? 0 : 1))) === 2)("Box with one dimension should be a line");
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsFlat for box with two dimensions", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        let b_76, v_86, v_87, v_88;
        const box_26 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 1E-13));
        Expect_isTrue(((b_76 = box_26, (((((v_86 = b_76.Xaxis, ((v_86.X * v_86.X) + (v_86.Y * v_86.Y)) + (v_86.Z * v_86.Z))) > 1E-24) ? 0 : 1) + ((((v_87 = b_76.Yaxis, ((v_87.X * v_87.X) + (v_87.Y * v_87.Y)) + (v_87.Z * v_87.Z))) > 1E-24) ? 0 : 1)) + ((((v_88 = b_76.Zaxis, ((v_88.X * v_88.X) + (v_88.Y * v_88.Y)) + (v_88.Z * v_88.Z))) > 1E-24) ? 0 : 1))) === 1)("Box with two dimensions should be flat");
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsValid for normal box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        let b_78, v_89, v_90, v_91;
        const box_27 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        Expect_isTrue(((b_78 = box_27, (((((v_89 = b_78.Xaxis, ((v_89.X * v_89.X) + (v_89.Y * v_89.Y)) + (v_89.Z * v_89.Z))) > 1E-24) ? 0 : 1) + ((((v_90 = b_78.Yaxis, ((v_90.X * v_90.X) + (v_90.Y * v_90.Y)) + (v_90.Z * v_90.Z))) > 1E-24) ? 0 : 1)) + ((((v_91 = b_78.Zaxis, ((v_91.X * v_91.X) + (v_91.Y * v_91.Y)) + (v_91.Z * v_91.Z))) > 1E-24) ? 0 : 1))) === 0)("Normal box should be valid");
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})(), (() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("HasVolume for normal box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        let b_80, v_92, v_93, v_94;
        const box_28 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        Expect_isTrue(((b_80 = box_28, (((((v_92 = b_80.Xaxis, ((v_92.X * v_92.X) + (v_92.Y * v_92.Y)) + (v_92.Z * v_92.Z))) > 1E-24) ? 0 : 1) + ((((v_93 = b_80.Yaxis, ((v_93.X * v_93.X) + (v_93.Y * v_93.Y)) + (v_93.Z * v_93.Z))) > 1E-24) ? 0 : 1)) + ((((v_94 = b_80.Zaxis, ((v_94.X * v_94.X) + (v_94.Y * v_94.Y)) + (v_94.Z * v_94.Z))) > 1E-24) ? 0 : 1))) === 0)("Normal box should have volume");
        Test_TestCaseBuilder__Zero(builder$0040_31);
    }));
})(), (() => {
    const builder$0040_32 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("CountZeroSides for normal box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_32, Test_TestCaseBuilder__Delay_1505(builder$0040_32, () => {
        let v_95, v_96, v_97;
        const box_29 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let actual_24;
        const b_81 = box_29;
        actual_24 = ((((((v_95 = b_81.Xaxis, ((v_95.X * v_95.X) + (v_95.Y * v_95.Y)) + (v_95.Z * v_95.Z))) > 1E-24) ? 0 : 1) + ((((v_96 = b_81.Yaxis, ((v_96.X * v_96.X) + (v_96.Y * v_96.Y)) + (v_96.Z * v_96.Z))) > 1E-24) ? 0 : 1)) + ((((v_97 = b_81.Zaxis, ((v_97.X * v_97.X) + (v_97.Y * v_97.Y)) + (v_97.Z * v_97.Z))) > 1E-24) ? 0 : 1));
        if ((actual_24 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_24, 0, "Normal box should have 0 zero sides");
        }
        else {
            let valueType_24;
            let copyOfStruct_24 = actual_24;
            valueType_24 = int32_type;
            const primitiveTypes_24 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_24;
            if (contains(valueType_24, primitiveTypes_24, {
                Equals: equals,
                GetHashCode: (x_76) => (structuralHash(x_76) | 0),
            })) {
                const arg_29 = int32ToString(0);
                const arg_1_24 = int32ToString(actual_24);
                errorMsg_24 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_29)(arg_1_24)("Normal box should have 0 zero sides");
            }
            else {
                errorMsg_24 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_24)("Normal box should have 0 zero sides");
            }
            throw new Exception(errorMsg_24);
        }
        Test_TestCaseBuilder__Zero(builder$0040_32);
    }));
})(), (() => {
    const builder$0040_33 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("CountZeroSides for flat box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_33, Test_TestCaseBuilder__Delay_1505(builder$0040_33, () => {
        let v_98, v_99, v_100;
        const box_30 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 1E-13));
        let actual_25;
        const b_82 = box_30;
        actual_25 = ((((((v_98 = b_82.Xaxis, ((v_98.X * v_98.X) + (v_98.Y * v_98.Y)) + (v_98.Z * v_98.Z))) > 1E-24) ? 0 : 1) + ((((v_99 = b_82.Yaxis, ((v_99.X * v_99.X) + (v_99.Y * v_99.Y)) + (v_99.Z * v_99.Z))) > 1E-24) ? 0 : 1)) + ((((v_100 = b_82.Zaxis, ((v_100.X * v_100.X) + (v_100.Y * v_100.Y)) + (v_100.Z * v_100.Z))) > 1E-24) ? 0 : 1));
        if ((actual_25 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_25, 1, "Flat box should have 1 zero side");
        }
        else {
            let valueType_25;
            let copyOfStruct_25 = actual_25;
            valueType_25 = int32_type;
            const primitiveTypes_25 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_25;
            if (contains(valueType_25, primitiveTypes_25, {
                Equals: equals,
                GetHashCode: (x_80) => (structuralHash(x_80) | 0),
            })) {
                const arg_30 = int32ToString(1);
                const arg_1_25 = int32ToString(actual_25);
                errorMsg_25 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_30)(arg_1_25)("Flat box should have 1 zero side");
            }
            else {
                errorMsg_25 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_25)("Flat box should have 1 zero side");
            }
            throw new Exception(errorMsg_25);
        }
        Test_TestCaseBuilder__Zero(builder$0040_33);
    }));
})(), (() => {
    const builder$0040_34 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("CountZeroSides for line box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_34, Test_TestCaseBuilder__Delay_1505(builder$0040_34, () => {
        let v_101, v_102, v_103;
        const box_31 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 1E-13, 0), Vec_$ctor_Z7AD9E565(0, 0, 1E-13));
        let actual_26;
        const b_83 = box_31;
        actual_26 = ((((((v_101 = b_83.Xaxis, ((v_101.X * v_101.X) + (v_101.Y * v_101.Y)) + (v_101.Z * v_101.Z))) > 1E-24) ? 0 : 1) + ((((v_102 = b_83.Yaxis, ((v_102.X * v_102.X) + (v_102.Y * v_102.Y)) + (v_102.Z * v_102.Z))) > 1E-24) ? 0 : 1)) + ((((v_103 = b_83.Zaxis, ((v_103.X * v_103.X) + (v_103.Y * v_103.Y)) + (v_103.Z * v_103.Z))) > 1E-24) ? 0 : 1));
        if ((actual_26 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_26, 2, "Line box should have 2 zero sides");
        }
        else {
            let valueType_26;
            let copyOfStruct_26 = actual_26;
            valueType_26 = int32_type;
            const primitiveTypes_26 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_26;
            if (contains(valueType_26, primitiveTypes_26, {
                Equals: equals,
                GetHashCode: (x_84) => (structuralHash(x_84) | 0),
            })) {
                const arg_31 = int32ToString(2);
                const arg_1_26 = int32ToString(actual_26);
                errorMsg_26 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_31)(arg_1_26)("Line box should have 2 zero sides");
            }
            else {
                errorMsg_26 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_26)("Line box should have 2 zero sides");
            }
            throw new Exception(errorMsg_26);
        }
        Test_TestCaseBuilder__Zero(builder$0040_34);
    }));
})(), (() => {
    const builder$0040_35 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("CountZeroSides for point box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_35, Test_TestCaseBuilder__Delay_1505(builder$0040_35, () => {
        let v_104, v_105, v_106;
        const box_32 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(1E-13, 0, 0), Vec_$ctor_Z7AD9E565(0, 1E-13, 0), Vec_$ctor_Z7AD9E565(0, 0, 1E-13));
        let actual_27;
        const b_84 = box_32;
        actual_27 = ((((((v_104 = b_84.Xaxis, ((v_104.X * v_104.X) + (v_104.Y * v_104.Y)) + (v_104.Z * v_104.Z))) > 1E-24) ? 0 : 1) + ((((v_105 = b_84.Yaxis, ((v_105.X * v_105.X) + (v_105.Y * v_105.Y)) + (v_105.Z * v_105.Z))) > 1E-24) ? 0 : 1)) + ((((v_106 = b_84.Zaxis, ((v_106.X * v_106.X) + (v_106.Y * v_106.Y)) + (v_106.Z * v_106.Z))) > 1E-24) ? 0 : 1));
        if ((actual_27 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_27, 3, "Point box should have 3 zero sides");
        }
        else {
            let valueType_27;
            let copyOfStruct_27 = actual_27;
            valueType_27 = int32_type;
            const primitiveTypes_27 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_27;
            if (contains(valueType_27, primitiveTypes_27, {
                Equals: equals,
                GetHashCode: (x_88) => (structuralHash(x_88) | 0),
            })) {
                const arg_32 = int32ToString(3);
                const arg_1_27 = int32ToString(actual_27);
                errorMsg_27 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_32)(arg_1_27)("Point box should have 3 zero sides");
            }
            else {
                errorMsg_27 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_27)("Point box should have 3 zero sides");
            }
            throw new Exception(errorMsg_27);
        }
        Test_TestCaseBuilder__Zero(builder$0040_35);
    }));
})()])), Test_testList("Transformation Methods", ofArray([(() => {
    const builder$0040_36 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("move by vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_36, Test_TestCaseBuilder__Delay_1505(builder$0040_36, () => {
        let p_19, v_109, a_60, b_88, x_89, y_50, z_22, v_111, a_61, b_89;
        const box_33 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let moved;
        const b_86 = box_33;
        moved = Box_$ctor_5706A3BA((p_19 = b_86.Origin, (v_109 = Vec_$ctor_Z7AD9E565(5, 3, 2), Pnt_$ctor_Z7AD9E565_1(p_19.X + v_109.X, p_19.Y + v_109.Y, p_19.Z + v_109.Z))), b_86.Xaxis, b_86.Yaxis, b_86.Zaxis);
        Expect_isTrue(((a_60 = moved.Origin, (b_88 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_89 = (a_60.X - b_88.X), (y_50 = (a_60.Y - b_88.Y), (z_22 = (a_60.Z - b_88.Z), Math.sqrt(((x_89 * x_89) + (y_50 * y_50)) + (z_22 * z_22)))))))) < 1E-09)("Origin should be moved");
        Expect_isTrue(((v_111 = ((a_61 = moved.Xaxis, (b_89 = box_33.Xaxis, Vec_$ctor_Z7AD9E565_1(a_61.X - b_89.X, a_61.Y - b_89.Y, a_61.Z - b_89.Z)))), Math.sqrt(((v_111.X * v_111.X) + (v_111.Y * v_111.Y)) + (v_111.Z * v_111.Z)))) < 1E-09)("Xaxis should be unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_36);
    }));
})(), (() => {
    const builder$0040_37 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Move instance method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_37, Test_TestCaseBuilder__Delay_1505(builder$0040_37, () => {
        let p_20, v_113, a_64, b_93, x_90, y_51, z_23, v_115, a_65, b_94;
        const box_34 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let moved_1;
        const b_91 = box_34;
        moved_1 = Box_$ctor_5706A3BA((p_20 = b_91.Origin, (v_113 = Vec_$ctor_Z7AD9E565(5, 3, 2), Pnt_$ctor_Z7AD9E565_1(p_20.X + v_113.X, p_20.Y + v_113.Y, p_20.Z + v_113.Z))), b_91.Xaxis, b_91.Yaxis, b_91.Zaxis);
        Expect_isTrue(((a_64 = moved_1.Origin, (b_93 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_90 = (a_64.X - b_93.X), (y_51 = (a_64.Y - b_93.Y), (z_23 = (a_64.Z - b_93.Z), Math.sqrt(((x_90 * x_90) + (y_51 * y_51)) + (z_23 * z_23)))))))) < 1E-09)("Origin should be moved");
        Expect_isTrue(((v_115 = ((a_65 = moved_1.Xaxis, (b_94 = box_34.Xaxis, Vec_$ctor_Z7AD9E565_1(a_65.X - b_94.X, a_65.Y - b_94.Y, a_65.Z - b_94.Z)))), Math.sqrt(((v_115.X * v_115.X) + (v_115.Y * v_115.Y)) + (v_115.Z * v_115.Z)))) < 1E-09)("Xaxis should be unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_37);
    }));
})(), (() => {
    const builder$0040_38 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("MoveX instance method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_38, Test_TestCaseBuilder__Delay_1505(builder$0040_38, () => {
        let a_68, b_98, x_91, y_52, z_24;
        const box_35 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let moved_2;
        const b_96 = box_35;
        moved_2 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(b_96.Origin.X + 5, b_96.Origin.Y, b_96.Origin.Z), b_96.Xaxis, b_96.Yaxis, b_96.Zaxis);
        Expect_isTrue(((a_68 = moved_2.Origin, (b_98 = Pnt_$ctor_Z7AD9E565(5, 0, 0), (x_91 = (a_68.X - b_98.X), (y_52 = (a_68.Y - b_98.Y), (z_24 = (a_68.Z - b_98.Z), Math.sqrt(((x_91 * x_91) + (y_52 * y_52)) + (z_24 * z_24)))))))) < 1E-09)("Origin should be moved in X");
        Test_TestCaseBuilder__Zero(builder$0040_38);
    }));
})(), (() => {
    const builder$0040_39 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("MoveY instance method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_39, Test_TestCaseBuilder__Delay_1505(builder$0040_39, () => {
        let a_70, b_101, x_92, y_53, z_25;
        const box_36 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let moved_3;
        const b_99 = box_36;
        moved_3 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(b_99.Origin.X, b_99.Origin.Y + 3, b_99.Origin.Z), b_99.Xaxis, b_99.Yaxis, b_99.Zaxis);
        Expect_isTrue(((a_70 = moved_3.Origin, (b_101 = Pnt_$ctor_Z7AD9E565(0, 3, 0), (x_92 = (a_70.X - b_101.X), (y_53 = (a_70.Y - b_101.Y), (z_25 = (a_70.Z - b_101.Z), Math.sqrt(((x_92 * x_92) + (y_53 * y_53)) + (z_25 * z_25)))))))) < 1E-09)("Origin should be moved in Y");
        Test_TestCaseBuilder__Zero(builder$0040_39);
    }));
})(), (() => {
    const builder$0040_40 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("MoveZ instance method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_40, Test_TestCaseBuilder__Delay_1505(builder$0040_40, () => {
        let a_72, b_104, x_93, y_54, z_26;
        const box_37 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let moved_4;
        const b_102 = box_37;
        moved_4 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(b_102.Origin.X, b_102.Origin.Y, b_102.Origin.Z + 2), b_102.Xaxis, b_102.Yaxis, b_102.Zaxis);
        Expect_isTrue(((a_72 = moved_4.Origin, (b_104 = Pnt_$ctor_Z7AD9E565(0, 0, 2), (x_93 = (a_72.X - b_104.X), (y_54 = (a_72.Y - b_104.Y), (z_26 = (a_72.Z - b_104.Z), Math.sqrt(((x_93 * x_93) + (y_54 * y_54)) + (z_26 * z_26)))))))) < 1E-09)("Origin should be moved in Z");
        Test_TestCaseBuilder__Zero(builder$0040_40);
    }));
})(), (() => {
    const builder$0040_41 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("moveX static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_41, Test_TestCaseBuilder__Delay_1505(builder$0040_41, () => {
        let a_74, b_108, x_94, y_55, z_27;
        const box_38 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let moved_5;
        const b_106 = box_38;
        moved_5 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(b_106.Origin.X + 5, b_106.Origin.Y, b_106.Origin.Z), b_106.Xaxis, b_106.Yaxis, b_106.Zaxis);
        Expect_isTrue(((a_74 = moved_5.Origin, (b_108 = Pnt_$ctor_Z7AD9E565(5, 0, 0), (x_94 = (a_74.X - b_108.X), (y_55 = (a_74.Y - b_108.Y), (z_27 = (a_74.Z - b_108.Z), Math.sqrt(((x_94 * x_94) + (y_55 * y_55)) + (z_27 * z_27)))))))) < 1E-09)("Origin should be moved in X");
        Test_TestCaseBuilder__Zero(builder$0040_41);
    }));
})(), (() => {
    const builder$0040_42 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("moveY static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_42, Test_TestCaseBuilder__Delay_1505(builder$0040_42, () => {
        let a_76, b_112, x_95, y_56, z_28;
        const box_39 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let moved_6;
        const b_110 = box_39;
        moved_6 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(b_110.Origin.X, b_110.Origin.Y + 3, b_110.Origin.Z), b_110.Xaxis, b_110.Yaxis, b_110.Zaxis);
        Expect_isTrue(((a_76 = moved_6.Origin, (b_112 = Pnt_$ctor_Z7AD9E565(0, 3, 0), (x_95 = (a_76.X - b_112.X), (y_56 = (a_76.Y - b_112.Y), (z_28 = (a_76.Z - b_112.Z), Math.sqrt(((x_95 * x_95) + (y_56 * y_56)) + (z_28 * z_28)))))))) < 1E-09)("Origin should be moved in Y");
        Test_TestCaseBuilder__Zero(builder$0040_42);
    }));
})(), (() => {
    const builder$0040_43 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("moveZ static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_43, Test_TestCaseBuilder__Delay_1505(builder$0040_43, () => {
        let a_78, b_116, x_96, y_57, z_29;
        const box_40 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let moved_7;
        const b_114 = box_40;
        moved_7 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(b_114.Origin.X, b_114.Origin.Y, b_114.Origin.Z + 2), b_114.Xaxis, b_114.Yaxis, b_114.Zaxis);
        Expect_isTrue(((a_78 = moved_7.Origin, (b_116 = Pnt_$ctor_Z7AD9E565(0, 0, 2), (x_96 = (a_78.X - b_116.X), (y_57 = (a_78.Y - b_116.Y), (z_29 = (a_78.Z - b_116.Z), Math.sqrt(((x_96 * x_96) + (y_57 * y_57)) + (z_29 * z_29)))))))) < 1E-09)("Origin should be moved in Z");
        Test_TestCaseBuilder__Zero(builder$0040_43);
    }));
})(), (() => {
    const builder$0040_44 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("translate static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_44, Test_TestCaseBuilder__Delay_1505(builder$0040_44, () => {
        let p_21, v_118, a_80, b_120, x_97, y_58, z_30;
        const box_41 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let moved_8;
        const b_118 = box_41;
        moved_8 = Box_$ctor_5706A3BA((p_21 = b_118.Origin, (v_118 = Vec_$ctor_Z7AD9E565(5, 3, 2), Pnt_$ctor_Z7AD9E565_1(p_21.X + v_118.X, p_21.Y + v_118.Y, p_21.Z + v_118.Z))), b_118.Xaxis, b_118.Yaxis, b_118.Zaxis);
        Expect_isTrue(((a_80 = moved_8.Origin, (b_120 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_97 = (a_80.X - b_120.X), (y_58 = (a_80.Y - b_120.Y), (z_30 = (a_80.Z - b_120.Z), Math.sqrt(((x_97 * x_97) + (y_58 * y_58)) + (z_30 * z_30)))))))) < 1E-09)("Origin should be translated");
        Test_TestCaseBuilder__Zero(builder$0040_44);
    }));
})(), (() => {
    const builder$0040_45 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Transform with identity matrix", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_45, Test_TestCaseBuilder__Delay_1505(builder$0040_45, () => {
        let a_82, b_123, x_103, y_64, z_36, v_130, a_83, b_124;
        const box_42 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(1, 2, 3), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let transformed;
        const b_121 = box_42;
        const m = Matrix_get_identity();
        let o;
        const p_22 = b_121.Origin;
        const m_1 = m;
        const x_98 = p_22.X;
        const y_59 = p_22.Y;
        const z_31 = p_22.Z;
        const x$0027 = (((m_1.M11 * x_98) + (m_1.M21 * y_59)) + (m_1.M31 * z_31)) + m_1.X41;
        const y$0027 = (((m_1.M12 * x_98) + (m_1.M22 * y_59)) + (m_1.M32 * z_31)) + m_1.Y42;
        const z$0027 = (((m_1.M13 * x_98) + (m_1.M23 * y_59)) + (m_1.M33 * z_31)) + m_1.Z43;
        const w$0027 = (((m_1.M14 * x_98) + (m_1.M24 * y_59)) + (m_1.M34 * z_31)) + m_1.M44;
        const sc = 1 / w$0027;
        o = Pnt_$ctor_Z7AD9E565_1(x$0027 * sc, y$0027 * sc, z$0027 * sc);
        let x_99;
        const v_122 = b_121.Xaxis;
        const m_4 = m;
        const x_100 = v_122.X;
        const y_60 = v_122.Y;
        const z_32 = v_122.Z;
        const x$0027_1 = ((m_4.M11 * x_100) + (m_4.M21 * y_60)) + (m_4.M31 * z_32);
        const y$0027_1 = ((m_4.M12 * x_100) + (m_4.M22 * y_60)) + (m_4.M32 * z_32);
        const z$0027_1 = ((m_4.M13 * x_100) + (m_4.M23 * y_60)) + (m_4.M33 * z_32);
        x_99 = Vec_$ctor_Z7AD9E565_1(x$0027_1, y$0027_1, z$0027_1);
        let y_61;
        const v_125 = b_121.Yaxis;
        const m_7 = m;
        const x_101 = v_125.X;
        const y_62 = v_125.Y;
        const z_33 = v_125.Z;
        const x$0027_2 = ((m_7.M11 * x_101) + (m_7.M21 * y_62)) + (m_7.M31 * z_33);
        const y$0027_2 = ((m_7.M12 * x_101) + (m_7.M22 * y_62)) + (m_7.M32 * z_33);
        const z$0027_2 = ((m_7.M13 * x_101) + (m_7.M23 * y_62)) + (m_7.M33 * z_33);
        y_61 = Vec_$ctor_Z7AD9E565_1(x$0027_2, y$0027_2, z$0027_2);
        let z_34;
        const v_128 = b_121.Zaxis;
        const m_10 = m;
        const x_102 = v_128.X;
        const y_63 = v_128.Y;
        const z_35 = v_128.Z;
        const x$0027_3 = ((m_10.M11 * x_102) + (m_10.M21 * y_63)) + (m_10.M31 * z_35);
        const y$0027_3 = ((m_10.M12 * x_102) + (m_10.M22 * y_63)) + (m_10.M32 * z_35);
        const z$0027_3 = ((m_10.M13 * x_102) + (m_10.M23 * y_63)) + (m_10.M33 * z_35);
        z_34 = Vec_$ctor_Z7AD9E565_1(x$0027_3, y$0027_3, z$0027_3);
        transformed = Box_$ctor_5706A3BA(o, x_99, y_61, z_34);
        Expect_isTrue(((a_82 = transformed.Origin, (b_123 = box_42.Origin, (x_103 = (a_82.X - b_123.X), (y_64 = (a_82.Y - b_123.Y), (z_36 = (a_82.Z - b_123.Z), Math.sqrt(((x_103 * x_103) + (y_64 * y_64)) + (z_36 * z_36)))))))) < 1E-09)("Origin should be unchanged with identity");
        Expect_isTrue(((v_130 = ((a_83 = transformed.Xaxis, (b_124 = box_42.Xaxis, Vec_$ctor_Z7AD9E565_1(a_83.X - b_124.X, a_83.Y - b_124.Y, a_83.Z - b_124.Z)))), Math.sqrt(((v_130.X * v_130.X) + (v_130.Y * v_130.Y)) + (v_130.Z * v_130.Z)))) < 1E-09)("Xaxis should be unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_45);
    }));
})(), (() => {
    const builder$0040_46 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Transform with translation matrix", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_46, Test_TestCaseBuilder__Delay_1505(builder$0040_46, () => {
        let a_86, b_128, x_109, y_70, z_42;
        const box_43 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        const m_11 = Matrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(5, 3, 2));
        let transformed_1;
        const b_126 = box_43;
        const m_12 = m_11;
        let o_1;
        const p_23 = b_126.Origin;
        const m_13 = m_12;
        const x_104 = p_23.X;
        const y_65 = p_23.Y;
        const z_37 = p_23.Z;
        const x$0027_4 = (((m_13.M11 * x_104) + (m_13.M21 * y_65)) + (m_13.M31 * z_37)) + m_13.X41;
        const y$0027_4 = (((m_13.M12 * x_104) + (m_13.M22 * y_65)) + (m_13.M32 * z_37)) + m_13.Y42;
        const z$0027_4 = (((m_13.M13 * x_104) + (m_13.M23 * y_65)) + (m_13.M33 * z_37)) + m_13.Z43;
        const w$0027_1 = (((m_13.M14 * x_104) + (m_13.M24 * y_65)) + (m_13.M34 * z_37)) + m_13.M44;
        const sc_1 = 1 / w$0027_1;
        o_1 = Pnt_$ctor_Z7AD9E565_1(x$0027_4 * sc_1, y$0027_4 * sc_1, z$0027_4 * sc_1);
        let x_105;
        const v_134 = b_126.Xaxis;
        const m_16 = m_12;
        const x_106 = v_134.X;
        const y_66 = v_134.Y;
        const z_38 = v_134.Z;
        const x$0027_5 = ((m_16.M11 * x_106) + (m_16.M21 * y_66)) + (m_16.M31 * z_38);
        const y$0027_5 = ((m_16.M12 * x_106) + (m_16.M22 * y_66)) + (m_16.M32 * z_38);
        const z$0027_5 = ((m_16.M13 * x_106) + (m_16.M23 * y_66)) + (m_16.M33 * z_38);
        x_105 = Vec_$ctor_Z7AD9E565_1(x$0027_5, y$0027_5, z$0027_5);
        let y_67;
        const v_137 = b_126.Yaxis;
        const m_19 = m_12;
        const x_107 = v_137.X;
        const y_68 = v_137.Y;
        const z_39 = v_137.Z;
        const x$0027_6 = ((m_19.M11 * x_107) + (m_19.M21 * y_68)) + (m_19.M31 * z_39);
        const y$0027_6 = ((m_19.M12 * x_107) + (m_19.M22 * y_68)) + (m_19.M32 * z_39);
        const z$0027_6 = ((m_19.M13 * x_107) + (m_19.M23 * y_68)) + (m_19.M33 * z_39);
        y_67 = Vec_$ctor_Z7AD9E565_1(x$0027_6, y$0027_6, z$0027_6);
        let z_40;
        const v_140 = b_126.Zaxis;
        const m_22 = m_12;
        const x_108 = v_140.X;
        const y_69 = v_140.Y;
        const z_41 = v_140.Z;
        const x$0027_7 = ((m_22.M11 * x_108) + (m_22.M21 * y_69)) + (m_22.M31 * z_41);
        const y$0027_7 = ((m_22.M12 * x_108) + (m_22.M22 * y_69)) + (m_22.M32 * z_41);
        const z$0027_7 = ((m_22.M13 * x_108) + (m_22.M23 * y_69)) + (m_22.M33 * z_41);
        z_40 = Vec_$ctor_Z7AD9E565_1(x$0027_7, y$0027_7, z$0027_7);
        transformed_1 = Box_$ctor_5706A3BA(o_1, x_105, y_67, z_40);
        Expect_isTrue(((a_86 = transformed_1.Origin, (b_128 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_109 = (a_86.X - b_128.X), (y_70 = (a_86.Y - b_128.Y), (z_42 = (a_86.Z - b_128.Z), Math.sqrt(((x_109 * x_109) + (y_70 * y_70)) + (z_42 * z_42)))))))) < 1E-09)("Origin should be translated");
        Test_TestCaseBuilder__Zero(builder$0040_46);
    }));
})(), (() => {
    const builder$0040_47 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("transform static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_47, Test_TestCaseBuilder__Delay_1505(builder$0040_47, () => {
        let a_88, b_132, x_115, y_76, z_48;
        const box_44 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        const m_23 = Matrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(5, 3, 2));
        let transformed_2;
        const m_25 = m_23;
        const b_130 = box_44;
        let o_2;
        const p_24 = b_130.Origin;
        const m_26 = m_25;
        const x_110 = p_24.X;
        const y_71 = p_24.Y;
        const z_43 = p_24.Z;
        const x$0027_8 = (((m_26.M11 * x_110) + (m_26.M21 * y_71)) + (m_26.M31 * z_43)) + m_26.X41;
        const y$0027_8 = (((m_26.M12 * x_110) + (m_26.M22 * y_71)) + (m_26.M32 * z_43)) + m_26.Y42;
        const z$0027_8 = (((m_26.M13 * x_110) + (m_26.M23 * y_71)) + (m_26.M33 * z_43)) + m_26.Z43;
        const w$0027_2 = (((m_26.M14 * x_110) + (m_26.M24 * y_71)) + (m_26.M34 * z_43)) + m_26.M44;
        const sc_2 = 1 / w$0027_2;
        o_2 = Pnt_$ctor_Z7AD9E565_1(x$0027_8 * sc_2, y$0027_8 * sc_2, z$0027_8 * sc_2);
        let x_111;
        const v_144 = b_130.Xaxis;
        const m_29 = m_25;
        const x_112 = v_144.X;
        const y_72 = v_144.Y;
        const z_44 = v_144.Z;
        const x$0027_9 = ((m_29.M11 * x_112) + (m_29.M21 * y_72)) + (m_29.M31 * z_44);
        const y$0027_9 = ((m_29.M12 * x_112) + (m_29.M22 * y_72)) + (m_29.M32 * z_44);
        const z$0027_9 = ((m_29.M13 * x_112) + (m_29.M23 * y_72)) + (m_29.M33 * z_44);
        x_111 = Vec_$ctor_Z7AD9E565_1(x$0027_9, y$0027_9, z$0027_9);
        let y_73;
        const v_147 = b_130.Yaxis;
        const m_32 = m_25;
        const x_113 = v_147.X;
        const y_74 = v_147.Y;
        const z_45 = v_147.Z;
        const x$0027_10 = ((m_32.M11 * x_113) + (m_32.M21 * y_74)) + (m_32.M31 * z_45);
        const y$0027_10 = ((m_32.M12 * x_113) + (m_32.M22 * y_74)) + (m_32.M32 * z_45);
        const z$0027_10 = ((m_32.M13 * x_113) + (m_32.M23 * y_74)) + (m_32.M33 * z_45);
        y_73 = Vec_$ctor_Z7AD9E565_1(x$0027_10, y$0027_10, z$0027_10);
        let z_46;
        const v_150 = b_130.Zaxis;
        const m_35 = m_25;
        const x_114 = v_150.X;
        const y_75 = v_150.Y;
        const z_47 = v_150.Z;
        const x$0027_11 = ((m_35.M11 * x_114) + (m_35.M21 * y_75)) + (m_35.M31 * z_47);
        const y$0027_11 = ((m_35.M12 * x_114) + (m_35.M22 * y_75)) + (m_35.M32 * z_47);
        const z$0027_11 = ((m_35.M13 * x_114) + (m_35.M23 * y_75)) + (m_35.M33 * z_47);
        z_46 = Vec_$ctor_Z7AD9E565_1(x$0027_11, y$0027_11, z$0027_11);
        transformed_2 = Box_$ctor_5706A3BA(o_2, x_111, y_73, z_46);
        Expect_isTrue(((a_88 = transformed_2.Origin, (b_132 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_115 = (a_88.X - b_132.X), (y_76 = (a_88.Y - b_132.Y), (z_48 = (a_88.Z - b_132.Z), Math.sqrt(((x_115 * x_115) + (y_76 * y_76)) + (z_48 * z_48)))))))) < 1E-09)("Origin should be translated");
        Test_TestCaseBuilder__Zero(builder$0040_47);
    }));
})(), (() => {
    const builder$0040_48 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("TransformRigid instance method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_48, Test_TestCaseBuilder__Delay_1505(builder$0040_48, () => {
        let a_90, b_135, x_121, y_82, z_54;
        const box_45 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        const m_36 = RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(5, 3, 2));
        let transformed_3;
        const b_133 = box_45;
        const m_37 = m_36;
        let o_3;
        const v_151 = b_133.Origin;
        const m_38 = m_37;
        const x_116 = v_151.X;
        const y_77 = v_151.Y;
        const z_49 = v_151.Z;
        o_3 = Pnt_$ctor_Z7AD9E565_1((((m_38.M11 * x_116) + (m_38.M21 * y_77)) + (m_38.M31 * z_49)) + m_38.X41, (((m_38.M12 * x_116) + (m_38.M22 * y_77)) + (m_38.M32 * z_49)) + m_38.Y42, (((m_38.M13 * x_116) + (m_38.M23 * y_77)) + (m_38.M33 * z_49)) + m_38.Z43);
        let x_117;
        const v_155 = b_133.Xaxis;
        const m_41 = m_37;
        const x_118 = v_155.X;
        const y_78 = v_155.Y;
        const z_50 = v_155.Z;
        x_117 = Vec_$ctor_Z7AD9E565_1(((m_41.M11 * x_118) + (m_41.M21 * y_78)) + (m_41.M31 * z_50), ((m_41.M12 * x_118) + (m_41.M22 * y_78)) + (m_41.M32 * z_50), ((m_41.M13 * x_118) + (m_41.M23 * y_78)) + (m_41.M33 * z_50));
        let y_79;
        const v_158 = b_133.Yaxis;
        const m_44 = m_37;
        const x_119 = v_158.X;
        const y_80 = v_158.Y;
        const z_51 = v_158.Z;
        y_79 = Vec_$ctor_Z7AD9E565_1(((m_44.M11 * x_119) + (m_44.M21 * y_80)) + (m_44.M31 * z_51), ((m_44.M12 * x_119) + (m_44.M22 * y_80)) + (m_44.M32 * z_51), ((m_44.M13 * x_119) + (m_44.M23 * y_80)) + (m_44.M33 * z_51));
        let z_52;
        const v_161 = b_133.Zaxis;
        const m_47 = m_37;
        const x_120 = v_161.X;
        const y_81 = v_161.Y;
        const z_53 = v_161.Z;
        z_52 = Vec_$ctor_Z7AD9E565_1(((m_47.M11 * x_120) + (m_47.M21 * y_81)) + (m_47.M31 * z_53), ((m_47.M12 * x_120) + (m_47.M22 * y_81)) + (m_47.M32 * z_53), ((m_47.M13 * x_120) + (m_47.M23 * y_81)) + (m_47.M33 * z_53));
        transformed_3 = Box_$ctor_5706A3BA(o_3, x_117, y_79, z_52);
        Expect_isTrue(((a_90 = transformed_3.Origin, (b_135 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_121 = (a_90.X - b_135.X), (y_82 = (a_90.Y - b_135.Y), (z_54 = (a_90.Z - b_135.Z), Math.sqrt(((x_121 * x_121) + (y_82 * y_82)) + (z_54 * z_54)))))))) < 1E-09)("Origin should be translated");
        Test_TestCaseBuilder__Zero(builder$0040_48);
    }));
})(), (() => {
    const builder$0040_49 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("transformRigid static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_49, Test_TestCaseBuilder__Delay_1505(builder$0040_49, () => {
        let a_92, b_139, x_127, y_88, z_60;
        const box_46 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        const m_48 = RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(5, 3, 2));
        let transformed_4;
        const m_50 = m_48;
        const b_137 = box_46;
        let o_4;
        const v_162 = b_137.Origin;
        const m_53 = m_50;
        const x_122 = v_162.X;
        const y_83 = v_162.Y;
        const z_55 = v_162.Z;
        o_4 = Pnt_$ctor_Z7AD9E565_1((((m_53.M11 * x_122) + (m_53.M21 * y_83)) + (m_53.M31 * z_55)) + m_53.X41, (((m_53.M12 * x_122) + (m_53.M22 * y_83)) + (m_53.M32 * z_55)) + m_53.Y42, (((m_53.M13 * x_122) + (m_53.M23 * y_83)) + (m_53.M33 * z_55)) + m_53.Z43);
        let x_123;
        const v_166 = b_137.Xaxis;
        const m_56 = m_50;
        const x_124 = v_166.X;
        const y_84 = v_166.Y;
        const z_56 = v_166.Z;
        x_123 = Vec_$ctor_Z7AD9E565_1(((m_56.M11 * x_124) + (m_56.M21 * y_84)) + (m_56.M31 * z_56), ((m_56.M12 * x_124) + (m_56.M22 * y_84)) + (m_56.M32 * z_56), ((m_56.M13 * x_124) + (m_56.M23 * y_84)) + (m_56.M33 * z_56));
        let y_85;
        const v_169 = b_137.Yaxis;
        const m_59 = m_50;
        const x_125 = v_169.X;
        const y_86 = v_169.Y;
        const z_57 = v_169.Z;
        y_85 = Vec_$ctor_Z7AD9E565_1(((m_59.M11 * x_125) + (m_59.M21 * y_86)) + (m_59.M31 * z_57), ((m_59.M12 * x_125) + (m_59.M22 * y_86)) + (m_59.M32 * z_57), ((m_59.M13 * x_125) + (m_59.M23 * y_86)) + (m_59.M33 * z_57));
        let z_58;
        const v_172 = b_137.Zaxis;
        const m_62 = m_50;
        const x_126 = v_172.X;
        const y_87 = v_172.Y;
        const z_59 = v_172.Z;
        z_58 = Vec_$ctor_Z7AD9E565_1(((m_62.M11 * x_126) + (m_62.M21 * y_87)) + (m_62.M31 * z_59), ((m_62.M12 * x_126) + (m_62.M22 * y_87)) + (m_62.M32 * z_59), ((m_62.M13 * x_126) + (m_62.M23 * y_87)) + (m_62.M33 * z_59));
        transformed_4 = Box_$ctor_5706A3BA(o_4, x_123, y_85, z_58);
        Expect_isTrue(((a_92 = transformed_4.Origin, (b_139 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_127 = (a_92.X - b_139.X), (y_88 = (a_92.Y - b_139.Y), (z_60 = (a_92.Z - b_139.Z), Math.sqrt(((x_127 * x_127) + (y_88 * y_88)) + (z_60 * z_60)))))))) < 1E-09)("Origin should be translated");
        Test_TestCaseBuilder__Zero(builder$0040_49);
    }));
})(), (() => {
    const builder$0040_50 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rotate with identity quaternion", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_50, Test_TestCaseBuilder__Delay_1505(builder$0040_50, () => {
        let a_94, b_142, x_134, y_95, z_67, v_177, a_95, b_143;
        const box_47 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(1, 2, 3), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let rotated;
        const b_140 = box_47;
        const q = Quaternion_$ctor_77D16AC0(0, 0, 0, 1);
        let o_5;
        const p_28 = b_140.Origin;
        const q_1 = q;
        const x_129 = p_28.X;
        const y_90 = p_28.Y;
        const z_62 = p_28.Z;
        const qx = q_1.X;
        const qy = q_1.Y;
        const qz = q_1.Z;
        const qw = q_1.W;
        const tx = 2 * ((qy * z_62) - (qz * y_90));
        const ty = 2 * ((qz * x_129) - (qx * z_62));
        const tz = 2 * ((qx * y_90) - (qy * x_129));
        o_5 = Pnt_$ctor_Z7AD9E565_1(((x_129 + (qw * tx)) + (qy * tz)) - (qz * ty), ((y_90 + (qw * ty)) + (qz * tx)) - (qx * tz), ((z_62 + (qw * tz)) + (qx * ty)) - (qy * tx));
        let x_130;
        const v_173 = b_140.Xaxis;
        const q_2 = q;
        const x_131 = v_173.X;
        const y_91 = v_173.Y;
        const z_63 = v_173.Z;
        const qx_1 = q_2.X;
        const qy_1 = q_2.Y;
        const qz_1 = q_2.Z;
        const qw_1 = q_2.W;
        const tx_1 = 2 * ((qy_1 * z_63) - (qz_1 * y_91));
        const ty_1 = 2 * ((qz_1 * x_131) - (qx_1 * z_63));
        const tz_1 = 2 * ((qx_1 * y_91) - (qy_1 * x_131));
        x_130 = Vec_$ctor_Z7AD9E565_1(((x_131 + (qw_1 * tx_1)) + (qy_1 * tz_1)) - (qz_1 * ty_1), ((y_91 + (qw_1 * ty_1)) + (qz_1 * tx_1)) - (qx_1 * tz_1), ((z_63 + (qw_1 * tz_1)) + (qx_1 * ty_1)) - (qy_1 * tx_1));
        let y_92;
        const v_174 = b_140.Yaxis;
        const q_3 = q;
        const x_132 = v_174.X;
        const y_93 = v_174.Y;
        const z_64 = v_174.Z;
        const qx_2 = q_3.X;
        const qy_2 = q_3.Y;
        const qz_2 = q_3.Z;
        const qw_2 = q_3.W;
        const tx_2 = 2 * ((qy_2 * z_64) - (qz_2 * y_93));
        const ty_2 = 2 * ((qz_2 * x_132) - (qx_2 * z_64));
        const tz_2 = 2 * ((qx_2 * y_93) - (qy_2 * x_132));
        y_92 = Vec_$ctor_Z7AD9E565_1(((x_132 + (qw_2 * tx_2)) + (qy_2 * tz_2)) - (qz_2 * ty_2), ((y_93 + (qw_2 * ty_2)) + (qz_2 * tx_2)) - (qx_2 * tz_2), ((z_64 + (qw_2 * tz_2)) + (qx_2 * ty_2)) - (qy_2 * tx_2));
        let z_65;
        const v_175 = b_140.Zaxis;
        const q_4 = q;
        const x_133 = v_175.X;
        const y_94 = v_175.Y;
        const z_66 = v_175.Z;
        const qx_3 = q_4.X;
        const qy_3 = q_4.Y;
        const qz_3 = q_4.Z;
        const qw_3 = q_4.W;
        const tx_3 = 2 * ((qy_3 * z_66) - (qz_3 * y_94));
        const ty_3 = 2 * ((qz_3 * x_133) - (qx_3 * z_66));
        const tz_3 = 2 * ((qx_3 * y_94) - (qy_3 * x_133));
        z_65 = Vec_$ctor_Z7AD9E565_1(((x_133 + (qw_3 * tx_3)) + (qy_3 * tz_3)) - (qz_3 * ty_3), ((y_94 + (qw_3 * ty_3)) + (qz_3 * tx_3)) - (qx_3 * tz_3), ((z_66 + (qw_3 * tz_3)) + (qx_3 * ty_3)) - (qy_3 * tx_3));
        rotated = Box_$ctor_5706A3BA(o_5, x_130, y_92, z_65);
        Expect_isTrue(((a_94 = rotated.Origin, (b_142 = box_47.Origin, (x_134 = (a_94.X - b_142.X), (y_95 = (a_94.Y - b_142.Y), (z_67 = (a_94.Z - b_142.Z), Math.sqrt(((x_134 * x_134) + (y_95 * y_95)) + (z_67 * z_67)))))))) < 1E-09)("Origin should be unchanged with identity quaternion");
        Expect_isTrue(((v_177 = ((a_95 = rotated.Xaxis, (b_143 = box_47.Xaxis, Vec_$ctor_Z7AD9E565_1(a_95.X - b_143.X, a_95.Y - b_143.Y, a_95.Z - b_143.Z)))), Math.sqrt(((v_177.X * v_177.X) + (v_177.Y * v_177.Y)) + (v_177.Z * v_177.Z)))) < 1E-09)("Xaxis should be unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_50);
    }));
})(), (() => {
    const builder$0040_51 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rotate 90 degrees around Z axis", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_51, Test_TestCaseBuilder__Delay_1505(builder$0040_51, () => {
        let a_98, b_147, x_142, y_103, z_75;
        const box_48 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(1, 0, 0), Vec_$ctor_Z7AD9E565(1, 0, 0), Vec_$ctor_Z7AD9E565(0, 1, 0), Vec_$ctor_Z7AD9E565(0, 0, 1));
        let q_5;
        const axis_1 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
        const angHalf = (0.017453292519943295 * 90) * 0.5;
        const sa = Math.sin(angHalf);
        q_5 = Quaternion_$ctor_77D16AC0(axis_1.X * sa, axis_1.Y * sa, axis_1.Z * sa, Math.cos(angHalf));
        let rotated_1;
        const b_145 = box_48;
        const q_6 = q_5;
        let o_6;
        const p_29 = b_145.Origin;
        const q_7 = q_6;
        const x_137 = p_29.X;
        const y_98 = p_29.Y;
        const z_70 = p_29.Z;
        const qx_4 = q_7.X;
        const qy_4 = q_7.Y;
        const qz_4 = q_7.Z;
        const qw_4 = q_7.W;
        const tx_4 = 2 * ((qy_4 * z_70) - (qz_4 * y_98));
        const ty_4 = 2 * ((qz_4 * x_137) - (qx_4 * z_70));
        const tz_4 = 2 * ((qx_4 * y_98) - (qy_4 * x_137));
        o_6 = Pnt_$ctor_Z7AD9E565_1(((x_137 + (qw_4 * tx_4)) + (qy_4 * tz_4)) - (qz_4 * ty_4), ((y_98 + (qw_4 * ty_4)) + (qz_4 * tx_4)) - (qx_4 * tz_4), ((z_70 + (qw_4 * tz_4)) + (qx_4 * ty_4)) - (qy_4 * tx_4));
        let x_138;
        const v_178 = b_145.Xaxis;
        const q_8 = q_6;
        const x_139 = v_178.X;
        const y_99 = v_178.Y;
        const z_71 = v_178.Z;
        const qx_5 = q_8.X;
        const qy_5 = q_8.Y;
        const qz_5 = q_8.Z;
        const qw_5 = q_8.W;
        const tx_5 = 2 * ((qy_5 * z_71) - (qz_5 * y_99));
        const ty_5 = 2 * ((qz_5 * x_139) - (qx_5 * z_71));
        const tz_5 = 2 * ((qx_5 * y_99) - (qy_5 * x_139));
        x_138 = Vec_$ctor_Z7AD9E565_1(((x_139 + (qw_5 * tx_5)) + (qy_5 * tz_5)) - (qz_5 * ty_5), ((y_99 + (qw_5 * ty_5)) + (qz_5 * tx_5)) - (qx_5 * tz_5), ((z_71 + (qw_5 * tz_5)) + (qx_5 * ty_5)) - (qy_5 * tx_5));
        let y_100;
        const v_179 = b_145.Yaxis;
        const q_9 = q_6;
        const x_140 = v_179.X;
        const y_101 = v_179.Y;
        const z_72 = v_179.Z;
        const qx_6 = q_9.X;
        const qy_6 = q_9.Y;
        const qz_6 = q_9.Z;
        const qw_6 = q_9.W;
        const tx_6 = 2 * ((qy_6 * z_72) - (qz_6 * y_101));
        const ty_6 = 2 * ((qz_6 * x_140) - (qx_6 * z_72));
        const tz_6 = 2 * ((qx_6 * y_101) - (qy_6 * x_140));
        y_100 = Vec_$ctor_Z7AD9E565_1(((x_140 + (qw_6 * tx_6)) + (qy_6 * tz_6)) - (qz_6 * ty_6), ((y_101 + (qw_6 * ty_6)) + (qz_6 * tx_6)) - (qx_6 * tz_6), ((z_72 + (qw_6 * tz_6)) + (qx_6 * ty_6)) - (qy_6 * tx_6));
        let z_73;
        const v_180 = b_145.Zaxis;
        const q_10 = q_6;
        const x_141 = v_180.X;
        const y_102 = v_180.Y;
        const z_74 = v_180.Z;
        const qx_7 = q_10.X;
        const qy_7 = q_10.Y;
        const qz_7 = q_10.Z;
        const qw_7 = q_10.W;
        const tx_7 = 2 * ((qy_7 * z_74) - (qz_7 * y_102));
        const ty_7 = 2 * ((qz_7 * x_141) - (qx_7 * z_74));
        const tz_7 = 2 * ((qx_7 * y_102) - (qy_7 * x_141));
        z_73 = Vec_$ctor_Z7AD9E565_1(((x_141 + (qw_7 * tx_7)) + (qy_7 * tz_7)) - (qz_7 * ty_7), ((y_102 + (qw_7 * ty_7)) + (qz_7 * tx_7)) - (qx_7 * tz_7), ((z_74 + (qw_7 * tz_7)) + (qx_7 * ty_7)) - (qy_7 * tx_7));
        rotated_1 = Box_$ctor_5706A3BA(o_6, x_138, y_100, z_73);
        Expect_isTrue(((a_98 = rotated_1.Origin, (b_147 = Pnt_$ctor_Z7AD9E565(0, 1, 0), (x_142 = (a_98.X - b_147.X), (y_103 = (a_98.Y - b_147.Y), (z_75 = (a_98.Z - b_147.Z), Math.sqrt(((x_142 * x_142) + (y_103 * y_103)) + (z_75 * z_75)))))))) < 1E-09)("Origin should be rotated 90 degrees");
        Test_TestCaseBuilder__Zero(builder$0040_51);
    }));
})(), (() => {
    const builder$0040_52 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("rotate static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_52, Test_TestCaseBuilder__Delay_1505(builder$0040_52, () => {
        let a_100, b_151, x_150, y_111, z_83;
        const box_49 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(1, 0, 0), Vec_$ctor_Z7AD9E565(1, 0, 0), Vec_$ctor_Z7AD9E565(0, 1, 0), Vec_$ctor_Z7AD9E565(0, 0, 1));
        let q_11;
        const axis_3 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
        const angHalf_1 = (0.017453292519943295 * 90) * 0.5;
        const sa_1 = Math.sin(angHalf_1);
        q_11 = Quaternion_$ctor_77D16AC0(axis_3.X * sa_1, axis_3.Y * sa_1, axis_3.Z * sa_1, Math.cos(angHalf_1));
        let rotated_2;
        const q_13 = q_11;
        const b_149 = box_49;
        let o_7;
        const p_30 = b_149.Origin;
        const q_14 = q_13;
        const x_145 = p_30.X;
        const y_106 = p_30.Y;
        const z_78 = p_30.Z;
        const qx_8 = q_14.X;
        const qy_8 = q_14.Y;
        const qz_8 = q_14.Z;
        const qw_8 = q_14.W;
        const tx_8 = 2 * ((qy_8 * z_78) - (qz_8 * y_106));
        const ty_8 = 2 * ((qz_8 * x_145) - (qx_8 * z_78));
        const tz_8 = 2 * ((qx_8 * y_106) - (qy_8 * x_145));
        o_7 = Pnt_$ctor_Z7AD9E565_1(((x_145 + (qw_8 * tx_8)) + (qy_8 * tz_8)) - (qz_8 * ty_8), ((y_106 + (qw_8 * ty_8)) + (qz_8 * tx_8)) - (qx_8 * tz_8), ((z_78 + (qw_8 * tz_8)) + (qx_8 * ty_8)) - (qy_8 * tx_8));
        let x_146;
        const v_181 = b_149.Xaxis;
        const q_15 = q_13;
        const x_147 = v_181.X;
        const y_107 = v_181.Y;
        const z_79 = v_181.Z;
        const qx_9 = q_15.X;
        const qy_9 = q_15.Y;
        const qz_9 = q_15.Z;
        const qw_9 = q_15.W;
        const tx_9 = 2 * ((qy_9 * z_79) - (qz_9 * y_107));
        const ty_9 = 2 * ((qz_9 * x_147) - (qx_9 * z_79));
        const tz_9 = 2 * ((qx_9 * y_107) - (qy_9 * x_147));
        x_146 = Vec_$ctor_Z7AD9E565_1(((x_147 + (qw_9 * tx_9)) + (qy_9 * tz_9)) - (qz_9 * ty_9), ((y_107 + (qw_9 * ty_9)) + (qz_9 * tx_9)) - (qx_9 * tz_9), ((z_79 + (qw_9 * tz_9)) + (qx_9 * ty_9)) - (qy_9 * tx_9));
        let y_108;
        const v_182 = b_149.Yaxis;
        const q_16 = q_13;
        const x_148 = v_182.X;
        const y_109 = v_182.Y;
        const z_80 = v_182.Z;
        const qx_10 = q_16.X;
        const qy_10 = q_16.Y;
        const qz_10 = q_16.Z;
        const qw_10 = q_16.W;
        const tx_10 = 2 * ((qy_10 * z_80) - (qz_10 * y_109));
        const ty_10 = 2 * ((qz_10 * x_148) - (qx_10 * z_80));
        const tz_10 = 2 * ((qx_10 * y_109) - (qy_10 * x_148));
        y_108 = Vec_$ctor_Z7AD9E565_1(((x_148 + (qw_10 * tx_10)) + (qy_10 * tz_10)) - (qz_10 * ty_10), ((y_109 + (qw_10 * ty_10)) + (qz_10 * tx_10)) - (qx_10 * tz_10), ((z_80 + (qw_10 * tz_10)) + (qx_10 * ty_10)) - (qy_10 * tx_10));
        let z_81;
        const v_183 = b_149.Zaxis;
        const q_17 = q_13;
        const x_149 = v_183.X;
        const y_110 = v_183.Y;
        const z_82 = v_183.Z;
        const qx_11 = q_17.X;
        const qy_11 = q_17.Y;
        const qz_11 = q_17.Z;
        const qw_11 = q_17.W;
        const tx_11 = 2 * ((qy_11 * z_82) - (qz_11 * y_110));
        const ty_11 = 2 * ((qz_11 * x_149) - (qx_11 * z_82));
        const tz_11 = 2 * ((qx_11 * y_110) - (qy_11 * x_149));
        z_81 = Vec_$ctor_Z7AD9E565_1(((x_149 + (qw_11 * tx_11)) + (qy_11 * tz_11)) - (qz_11 * ty_11), ((y_110 + (qw_11 * ty_11)) + (qz_11 * tx_11)) - (qx_11 * tz_11), ((z_82 + (qw_11 * tz_11)) + (qx_11 * ty_11)) - (qy_11 * tx_11));
        rotated_2 = Box_$ctor_5706A3BA(o_7, x_146, y_108, z_81);
        Expect_isTrue(((a_100 = rotated_2.Origin, (b_151 = Pnt_$ctor_Z7AD9E565(0, 1, 0), (x_150 = (a_100.X - b_151.X), (y_111 = (a_100.Y - b_151.Y), (z_83 = (a_100.Z - b_151.Z), Math.sqrt(((x_150 * x_150) + (y_111 * y_111)) + (z_83 * z_83)))))))) < 1E-09)("Origin should be rotated 90 degrees");
        Test_TestCaseBuilder__Zero(builder$0040_52);
    }));
})(), (() => {
    const builder$0040_53 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RotateWithCenter keeps center point fixed", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_53, Test_TestCaseBuilder__Delay_1505(builder$0040_53, () => {
        let a_108, b_154, p_36, p_35, p_34, v_190, a_104, v_191, a_105, v_192, a_106, b_156, x_158, y_119, z_91;
        const box_50 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(2, 0, 0), Vec_$ctor_Z7AD9E565(0, 2, 0), Vec_$ctor_Z7AD9E565(0, 0, 2));
        let center;
        const b_152 = box_50;
        let p_33;
        let p_32;
        const p_31 = b_152.Origin;
        let v_184;
        const a_101 = b_152.Xaxis;
        v_184 = Vec_$ctor_Z7AD9E565_1(a_101.X * 0.5, a_101.Y * 0.5, a_101.Z * 0.5);
        p_32 = Pnt_$ctor_Z7AD9E565_1(p_31.X + v_184.X, p_31.Y + v_184.Y, p_31.Z + v_184.Z);
        let v_185;
        const a_102 = b_152.Yaxis;
        v_185 = Vec_$ctor_Z7AD9E565_1(a_102.X * 0.5, a_102.Y * 0.5, a_102.Z * 0.5);
        p_33 = Pnt_$ctor_Z7AD9E565_1(p_32.X + v_185.X, p_32.Y + v_185.Y, p_32.Z + v_185.Z);
        let v_186;
        const a_103 = b_152.Zaxis;
        v_186 = Vec_$ctor_Z7AD9E565_1(a_103.X * 0.5, a_103.Y * 0.5, a_103.Z * 0.5);
        center = Pnt_$ctor_Z7AD9E565_1(p_33.X + v_186.X, p_33.Y + v_186.Y, p_33.Z + v_186.Z);
        let q_18;
        const axis_5 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
        const angHalf_2 = (0.017453292519943295 * 90) * 0.5;
        const sa_2 = Math.sin(angHalf_2);
        q_18 = Quaternion_$ctor_77D16AC0(axis_5.X * sa_2, axis_5.Y * sa_2, axis_5.Z * sa_2, Math.cos(angHalf_2));
        let rotated_3;
        const b_153 = box_50;
        const q_19 = q_18;
        let o_8;
        const cen_2 = center;
        const q_20 = q_19;
        const pt_4 = b_153.Origin;
        const x_153 = pt_4.X - cen_2.X;
        const y_114 = pt_4.Y - cen_2.Y;
        const z_86 = pt_4.Z - cen_2.Z;
        const qx_12 = q_20.X;
        const qy_12 = q_20.Y;
        const qz_12 = q_20.Z;
        const qw_12 = q_20.W;
        const ix = ((qw_12 * x_153) + (qy_12 * z_86)) - (qz_12 * y_114);
        const iy = ((qw_12 * y_114) + (qz_12 * x_153)) - (qx_12 * z_86);
        const iz = ((qw_12 * z_86) + (qx_12 * y_114)) - (qy_12 * x_153);
        const iw = ((-qx_12 * x_153) - (qy_12 * y_114)) - (qz_12 * z_86);
        o_8 = Pnt_$ctor_Z7AD9E565_2(((((ix * qw_12) + (iw * -qx_12)) + (iy * -qz_12)) - (iz * -qy_12)) + cen_2.X, ((((iy * qw_12) + (iw * -qy_12)) + (iz * -qx_12)) - (ix * -qz_12)) + cen_2.Y, ((((iz * qw_12) + (iw * -qz_12)) + (ix * -qy_12)) - (iy * -qx_12)) + cen_2.Z);
        let x_154;
        const v_187 = b_153.Xaxis;
        const q_21 = q_19;
        const x_155 = v_187.X;
        const y_115 = v_187.Y;
        const z_87 = v_187.Z;
        const qx_13 = q_21.X;
        const qy_13 = q_21.Y;
        const qz_13 = q_21.Z;
        const qw_13 = q_21.W;
        const tx_12 = 2 * ((qy_13 * z_87) - (qz_13 * y_115));
        const ty_12 = 2 * ((qz_13 * x_155) - (qx_13 * z_87));
        const tz_12 = 2 * ((qx_13 * y_115) - (qy_13 * x_155));
        x_154 = Vec_$ctor_Z7AD9E565_1(((x_155 + (qw_13 * tx_12)) + (qy_13 * tz_12)) - (qz_13 * ty_12), ((y_115 + (qw_13 * ty_12)) + (qz_13 * tx_12)) - (qx_13 * tz_12), ((z_87 + (qw_13 * tz_12)) + (qx_13 * ty_12)) - (qy_13 * tx_12));
        let y_116;
        const v_188 = b_153.Yaxis;
        const q_22 = q_19;
        const x_156 = v_188.X;
        const y_117 = v_188.Y;
        const z_88 = v_188.Z;
        const qx_14 = q_22.X;
        const qy_14 = q_22.Y;
        const qz_14 = q_22.Z;
        const qw_14 = q_22.W;
        const tx_13 = 2 * ((qy_14 * z_88) - (qz_14 * y_117));
        const ty_13 = 2 * ((qz_14 * x_156) - (qx_14 * z_88));
        const tz_13 = 2 * ((qx_14 * y_117) - (qy_14 * x_156));
        y_116 = Vec_$ctor_Z7AD9E565_1(((x_156 + (qw_14 * tx_13)) + (qy_14 * tz_13)) - (qz_14 * ty_13), ((y_117 + (qw_14 * ty_13)) + (qz_14 * tx_13)) - (qx_14 * tz_13), ((z_88 + (qw_14 * tz_13)) + (qx_14 * ty_13)) - (qy_14 * tx_13));
        let z_89;
        const v_189 = b_153.Zaxis;
        const q_23 = q_19;
        const x_157 = v_189.X;
        const y_118 = v_189.Y;
        const z_90 = v_189.Z;
        const qx_15 = q_23.X;
        const qy_15 = q_23.Y;
        const qz_15 = q_23.Z;
        const qw_15 = q_23.W;
        const tx_14 = 2 * ((qy_15 * z_90) - (qz_15 * y_118));
        const ty_14 = 2 * ((qz_15 * x_157) - (qx_15 * z_90));
        const tz_14 = 2 * ((qx_15 * y_118) - (qy_15 * x_157));
        z_89 = Vec_$ctor_Z7AD9E565_1(((x_157 + (qw_15 * tx_14)) + (qy_15 * tz_14)) - (qz_15 * ty_14), ((y_118 + (qw_15 * ty_14)) + (qz_15 * tx_14)) - (qx_15 * tz_14), ((z_90 + (qw_15 * tz_14)) + (qx_15 * ty_14)) - (qy_15 * tx_14));
        rotated_3 = Box_$ctor_5706A3BA(o_8, x_154, y_116, z_89);
        Expect_isTrue(((a_108 = ((b_154 = rotated_3, (p_36 = ((p_35 = ((p_34 = b_154.Origin, (v_190 = ((a_104 = b_154.Xaxis, Vec_$ctor_Z7AD9E565_1(a_104.X * 0.5, a_104.Y * 0.5, a_104.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_34.X + v_190.X, p_34.Y + v_190.Y, p_34.Z + v_190.Z)))), (v_191 = ((a_105 = b_154.Yaxis, Vec_$ctor_Z7AD9E565_1(a_105.X * 0.5, a_105.Y * 0.5, a_105.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_35.X + v_191.X, p_35.Y + v_191.Y, p_35.Z + v_191.Z)))), (v_192 = ((a_106 = b_154.Zaxis, Vec_$ctor_Z7AD9E565_1(a_106.X * 0.5, a_106.Y * 0.5, a_106.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_36.X + v_192.X, p_36.Y + v_192.Y, p_36.Z + v_192.Z))))), (b_156 = center, (x_158 = (a_108.X - b_156.X), (y_119 = (a_108.Y - b_156.Y), (z_91 = (a_108.Z - b_156.Z), Math.sqrt(((x_158 * x_158) + (y_119 * y_119)) + (z_91 * z_91)))))))) < 1E-09)("Center should remain fixed");
        Test_TestCaseBuilder__Zero(builder$0040_53);
    }));
})(), (() => {
    const builder$0040_54 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("rotateWithCenter static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_54, Test_TestCaseBuilder__Delay_1505(builder$0040_54, () => {
        let a_116, b_160, p_42, p_41, p_40, v_199, a_112, v_200, a_113, v_201, a_114, b_162, x_166, y_127, z_99;
        const box_51 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(2, 0, 0), Vec_$ctor_Z7AD9E565(0, 2, 0), Vec_$ctor_Z7AD9E565(0, 0, 2));
        let center_1;
        const b_157 = box_51;
        let p_39;
        let p_38;
        const p_37 = b_157.Origin;
        let v_193;
        const a_109 = b_157.Xaxis;
        v_193 = Vec_$ctor_Z7AD9E565_1(a_109.X * 0.5, a_109.Y * 0.5, a_109.Z * 0.5);
        p_38 = Pnt_$ctor_Z7AD9E565_1(p_37.X + v_193.X, p_37.Y + v_193.Y, p_37.Z + v_193.Z);
        let v_194;
        const a_110 = b_157.Yaxis;
        v_194 = Vec_$ctor_Z7AD9E565_1(a_110.X * 0.5, a_110.Y * 0.5, a_110.Z * 0.5);
        p_39 = Pnt_$ctor_Z7AD9E565_1(p_38.X + v_194.X, p_38.Y + v_194.Y, p_38.Z + v_194.Z);
        let v_195;
        const a_111 = b_157.Zaxis;
        v_195 = Vec_$ctor_Z7AD9E565_1(a_111.X * 0.5, a_111.Y * 0.5, a_111.Z * 0.5);
        center_1 = Pnt_$ctor_Z7AD9E565_1(p_39.X + v_195.X, p_39.Y + v_195.Y, p_39.Z + v_195.Z);
        let q_24;
        const axis_7 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
        const angHalf_3 = (0.017453292519943295 * 90) * 0.5;
        const sa_3 = Math.sin(angHalf_3);
        q_24 = Quaternion_$ctor_77D16AC0(axis_7.X * sa_3, axis_7.Y * sa_3, axis_7.Z * sa_3, Math.cos(angHalf_3));
        let rotated_4;
        const q_26 = q_24;
        const b_159 = box_51;
        let o_9;
        const cen_5 = center_1;
        const q_27 = q_26;
        const pt_6 = b_159.Origin;
        const x_161 = pt_6.X - cen_5.X;
        const y_122 = pt_6.Y - cen_5.Y;
        const z_94 = pt_6.Z - cen_5.Z;
        const qx_16 = q_27.X;
        const qy_16 = q_27.Y;
        const qz_16 = q_27.Z;
        const qw_16 = q_27.W;
        const ix_1 = ((qw_16 * x_161) + (qy_16 * z_94)) - (qz_16 * y_122);
        const iy_1 = ((qw_16 * y_122) + (qz_16 * x_161)) - (qx_16 * z_94);
        const iz_1 = ((qw_16 * z_94) + (qx_16 * y_122)) - (qy_16 * x_161);
        const iw_1 = ((-qx_16 * x_161) - (qy_16 * y_122)) - (qz_16 * z_94);
        o_9 = Pnt_$ctor_Z7AD9E565_2(((((ix_1 * qw_16) + (iw_1 * -qx_16)) + (iy_1 * -qz_16)) - (iz_1 * -qy_16)) + cen_5.X, ((((iy_1 * qw_16) + (iw_1 * -qy_16)) + (iz_1 * -qx_16)) - (ix_1 * -qz_16)) + cen_5.Y, ((((iz_1 * qw_16) + (iw_1 * -qz_16)) + (ix_1 * -qy_16)) - (iy_1 * -qx_16)) + cen_5.Z);
        let x_162;
        const v_196 = b_159.Xaxis;
        const q_28 = q_26;
        const x_163 = v_196.X;
        const y_123 = v_196.Y;
        const z_95 = v_196.Z;
        const qx_17 = q_28.X;
        const qy_17 = q_28.Y;
        const qz_17 = q_28.Z;
        const qw_17 = q_28.W;
        const tx_15 = 2 * ((qy_17 * z_95) - (qz_17 * y_123));
        const ty_15 = 2 * ((qz_17 * x_163) - (qx_17 * z_95));
        const tz_15 = 2 * ((qx_17 * y_123) - (qy_17 * x_163));
        x_162 = Vec_$ctor_Z7AD9E565_1(((x_163 + (qw_17 * tx_15)) + (qy_17 * tz_15)) - (qz_17 * ty_15), ((y_123 + (qw_17 * ty_15)) + (qz_17 * tx_15)) - (qx_17 * tz_15), ((z_95 + (qw_17 * tz_15)) + (qx_17 * ty_15)) - (qy_17 * tx_15));
        let y_124;
        const v_197 = b_159.Yaxis;
        const q_29 = q_26;
        const x_164 = v_197.X;
        const y_125 = v_197.Y;
        const z_96 = v_197.Z;
        const qx_18 = q_29.X;
        const qy_18 = q_29.Y;
        const qz_18 = q_29.Z;
        const qw_18 = q_29.W;
        const tx_16 = 2 * ((qy_18 * z_96) - (qz_18 * y_125));
        const ty_16 = 2 * ((qz_18 * x_164) - (qx_18 * z_96));
        const tz_16 = 2 * ((qx_18 * y_125) - (qy_18 * x_164));
        y_124 = Vec_$ctor_Z7AD9E565_1(((x_164 + (qw_18 * tx_16)) + (qy_18 * tz_16)) - (qz_18 * ty_16), ((y_125 + (qw_18 * ty_16)) + (qz_18 * tx_16)) - (qx_18 * tz_16), ((z_96 + (qw_18 * tz_16)) + (qx_18 * ty_16)) - (qy_18 * tx_16));
        let z_97;
        const v_198 = b_159.Zaxis;
        const q_30 = q_26;
        const x_165 = v_198.X;
        const y_126 = v_198.Y;
        const z_98 = v_198.Z;
        const qx_19 = q_30.X;
        const qy_19 = q_30.Y;
        const qz_19 = q_30.Z;
        const qw_19 = q_30.W;
        const tx_17 = 2 * ((qy_19 * z_98) - (qz_19 * y_126));
        const ty_17 = 2 * ((qz_19 * x_165) - (qx_19 * z_98));
        const tz_17 = 2 * ((qx_19 * y_126) - (qy_19 * x_165));
        z_97 = Vec_$ctor_Z7AD9E565_1(((x_165 + (qw_19 * tx_17)) + (qy_19 * tz_17)) - (qz_19 * ty_17), ((y_126 + (qw_19 * ty_17)) + (qz_19 * tx_17)) - (qx_19 * tz_17), ((z_98 + (qw_19 * tz_17)) + (qx_19 * ty_17)) - (qy_19 * tx_17));
        rotated_4 = Box_$ctor_5706A3BA(o_9, x_162, y_124, z_97);
        Expect_isTrue(((a_116 = ((b_160 = rotated_4, (p_42 = ((p_41 = ((p_40 = b_160.Origin, (v_199 = ((a_112 = b_160.Xaxis, Vec_$ctor_Z7AD9E565_1(a_112.X * 0.5, a_112.Y * 0.5, a_112.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_40.X + v_199.X, p_40.Y + v_199.Y, p_40.Z + v_199.Z)))), (v_200 = ((a_113 = b_160.Yaxis, Vec_$ctor_Z7AD9E565_1(a_113.X * 0.5, a_113.Y * 0.5, a_113.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_41.X + v_200.X, p_41.Y + v_200.Y, p_41.Z + v_200.Z)))), (v_201 = ((a_114 = b_160.Zaxis, Vec_$ctor_Z7AD9E565_1(a_114.X * 0.5, a_114.Y * 0.5, a_114.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_42.X + v_201.X, p_42.Y + v_201.Y, p_42.Z + v_201.Z))))), (b_162 = center_1, (x_166 = (a_116.X - b_162.X), (y_127 = (a_116.Y - b_162.Y), (z_99 = (a_116.Z - b_162.Z), Math.sqrt(((x_166 * x_166) + (y_127 * y_127)) + (z_99 * z_99)))))))) < 1E-09)("Center should remain fixed");
        Test_TestCaseBuilder__Zero(builder$0040_54);
    }));
})(), (() => {
    const builder$0040_55 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("translateLocalX", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_55, Test_TestCaseBuilder__Delay_1505(builder$0040_55, () => {
        let a_118, b_165, x_167, y_128, z_100;
        const box_52 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        const moved_9 = Box_translateLocalX(5, box_52);
        Expect_isTrue(((a_118 = moved_9.Origin, (b_165 = Pnt_$ctor_Z7AD9E565(5, 0, 0), (x_167 = (a_118.X - b_165.X), (y_128 = (a_118.Y - b_165.Y), (z_100 = (a_118.Z - b_165.Z), Math.sqrt(((x_167 * x_167) + (y_128 * y_128)) + (z_100 * z_100)))))))) < 1E-09)("Should move along X-axis");
        Test_TestCaseBuilder__Zero(builder$0040_55);
    }));
})(), (() => {
    const builder$0040_56 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("translateLocalY", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_56, Test_TestCaseBuilder__Delay_1505(builder$0040_56, () => {
        let a_120, b_168, x_168, y_129, z_101;
        const box_53 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        const moved_10 = Box_translateLocalY(3, box_53);
        Expect_isTrue(((a_120 = moved_10.Origin, (b_168 = Pnt_$ctor_Z7AD9E565(0, 3, 0), (x_168 = (a_120.X - b_168.X), (y_129 = (a_120.Y - b_168.Y), (z_101 = (a_120.Z - b_168.Z), Math.sqrt(((x_168 * x_168) + (y_129 * y_129)) + (z_101 * z_101)))))))) < 1E-09)("Should move along Y-axis");
        Test_TestCaseBuilder__Zero(builder$0040_56);
    }));
})(), (() => {
    const builder$0040_57 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("translateLocalZ", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_57, Test_TestCaseBuilder__Delay_1505(builder$0040_57, () => {
        let a_122, b_171, x_169, y_130, z_102;
        const box_54 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        const moved_11 = Box_translateLocalZ(2, box_54);
        Expect_isTrue(((a_122 = moved_11.Origin, (b_171 = Pnt_$ctor_Z7AD9E565(0, 0, 2), (x_169 = (a_122.X - b_171.X), (y_130 = (a_122.Y - b_171.Y), (z_102 = (a_122.Z - b_171.Z), Math.sqrt(((x_169 * x_169) + (y_130 * y_130)) + (z_102 * z_102)))))))) < 1E-09)("Should move along Z-axis");
        Test_TestCaseBuilder__Zero(builder$0040_57);
    }));
})(), (() => {
    const builder$0040_58 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Scale from world origin", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_58, Test_TestCaseBuilder__Delay_1505(builder$0040_58, () => {
        let a_123, a_124, a_125, a_126, a_128, b_174, x_170, y_131, z_103;
        const box_55 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(1, 2, 3), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let scaled;
        const b_172 = box_55;
        scaled = Box_$ctor_5706A3BA((a_123 = b_172.Origin, Pnt_$ctor_Z7AD9E565_1(a_123.X * 2, a_123.Y * 2, a_123.Z * 2)), (a_124 = b_172.Xaxis, Vec_$ctor_Z7AD9E565_1(a_124.X * 2, a_124.Y * 2, a_124.Z * 2)), (a_125 = b_172.Yaxis, Vec_$ctor_Z7AD9E565_1(a_125.X * 2, a_125.Y * 2, a_125.Z * 2)), (a_126 = b_172.Zaxis, Vec_$ctor_Z7AD9E565_1(a_126.X * 2, a_126.Y * 2, a_126.Z * 2)));
        Expect_isTrue(((a_128 = scaled.Origin, (b_174 = Pnt_$ctor_Z7AD9E565(2, 4, 6), (x_170 = (a_128.X - b_174.X), (y_131 = (a_128.Y - b_174.Y), (z_103 = (a_128.Z - b_174.Z), Math.sqrt(((x_170 * x_170) + (y_131 * y_131)) + (z_103 * z_103)))))))) < 1E-09)("Origin should be scaled");
        let actual_28;
        const v_202 = scaled.Xaxis;
        actual_28 = Math.sqrt(((v_202.X * v_202.X) + (v_202.Y * v_202.Y)) + (v_202.Z * v_202.Z));
        if ((actual_28 === 20) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_28, 20, "SizeX should be scaled");
        }
        else {
            let valueType_28;
            let copyOfStruct_28 = actual_28;
            valueType_28 = float64_type;
            const primitiveTypes_28 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_28;
            if (contains(valueType_28, primitiveTypes_28, {
                Equals: equals,
                GetHashCode: (x_171) => (structuralHash(x_171) | 0),
            })) {
                const arg_58 = (20).toString();
                const arg_1_53 = actual_28.toString();
                errorMsg_28 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_58)(arg_1_53)("SizeX should be scaled");
            }
            else {
                errorMsg_28 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(20)(actual_28)("SizeX should be scaled");
            }
            throw new Exception(errorMsg_28);
        }
        let actual_29;
        const v_203 = scaled.Yaxis;
        actual_29 = Math.sqrt(((v_203.X * v_203.X) + (v_203.Y * v_203.Y)) + (v_203.Z * v_203.Z));
        if ((actual_29 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_29, 10, "SizeY should be scaled");
        }
        else {
            let valueType_29;
            let copyOfStruct_29 = actual_29;
            valueType_29 = float64_type;
            const primitiveTypes_29 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_29;
            if (contains(valueType_29, primitiveTypes_29, {
                Equals: equals,
                GetHashCode: (x_172) => (structuralHash(x_172) | 0),
            })) {
                const arg_59 = (10).toString();
                const arg_1_54 = actual_29.toString();
                errorMsg_29 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_59)(arg_1_54)("SizeY should be scaled");
            }
            else {
                errorMsg_29 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_29)("SizeY should be scaled");
            }
            throw new Exception(errorMsg_29);
        }
        let actual_30;
        const v_204 = scaled.Zaxis;
        actual_30 = Math.sqrt(((v_204.X * v_204.X) + (v_204.Y * v_204.Y)) + (v_204.Z * v_204.Z));
        if ((actual_30 === 6) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_30, 6, "SizeZ should be scaled");
        }
        else {
            let valueType_30;
            let copyOfStruct_30 = actual_30;
            valueType_30 = float64_type;
            const primitiveTypes_30 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_30;
            if (contains(valueType_30, primitiveTypes_30, {
                Equals: equals,
                GetHashCode: (x_173) => (structuralHash(x_173) | 0),
            })) {
                const arg_60 = (6).toString();
                const arg_1_55 = actual_30.toString();
                errorMsg_30 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_60)(arg_1_55)("SizeZ should be scaled");
            }
            else {
                errorMsg_30 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(6)(actual_30)("SizeZ should be scaled");
            }
            throw new Exception(errorMsg_30);
        }
        Test_TestCaseBuilder__Zero(builder$0040_58);
    }));
})(), (() => {
    const builder$0040_59 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ScaleOn center point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_59, Test_TestCaseBuilder__Delay_1505(builder$0040_59, () => {
        let a_129, f_42, a_130, f_43, a_131, f_44, a_139, b_178, p_45, p_44, p_43, v_205, a_132, v_206, a_133, v_207, a_134, b_181, b_179, p_48, p_47, p_46, v_208, a_135, v_209, a_136, v_210, a_137, x_174, y_135, z_104;
        const box_56 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        let scaled_1;
        const l_2 = box_56;
        const cen_7 = Pnt_$ctor_Z7AD9E565(5, 5, 5);
        const cx = cen_7.X;
        const cy = cen_7.Y;
        const cz = cen_7.Z;
        const o_10 = l_2.Origin;
        scaled_1 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(cx + ((o_10.X - cx) * 2), cy + ((o_10.Y - cy) * 2), cz + ((o_10.Z - cz) * 2)), (a_129 = l_2.Xaxis, (f_42 = 2, Vec_$ctor_Z7AD9E565_1(a_129.X * f_42, a_129.Y * f_42, a_129.Z * f_42))), (a_130 = l_2.Yaxis, (f_43 = 2, Vec_$ctor_Z7AD9E565_1(a_130.X * f_43, a_130.Y * f_43, a_130.Z * f_43))), (a_131 = l_2.Zaxis, (f_44 = 2, Vec_$ctor_Z7AD9E565_1(a_131.X * f_44, a_131.Y * f_44, a_131.Z * f_44))));
        Expect_isTrue(((a_139 = ((b_178 = scaled_1, (p_45 = ((p_44 = ((p_43 = b_178.Origin, (v_205 = ((a_132 = b_178.Xaxis, Vec_$ctor_Z7AD9E565_1(a_132.X * 0.5, a_132.Y * 0.5, a_132.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_43.X + v_205.X, p_43.Y + v_205.Y, p_43.Z + v_205.Z)))), (v_206 = ((a_133 = b_178.Yaxis, Vec_$ctor_Z7AD9E565_1(a_133.X * 0.5, a_133.Y * 0.5, a_133.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_44.X + v_206.X, p_44.Y + v_206.Y, p_44.Z + v_206.Z)))), (v_207 = ((a_134 = b_178.Zaxis, Vec_$ctor_Z7AD9E565_1(a_134.X * 0.5, a_134.Y * 0.5, a_134.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_45.X + v_207.X, p_45.Y + v_207.Y, p_45.Z + v_207.Z))))), (b_181 = ((b_179 = box_56, (p_48 = ((p_47 = ((p_46 = b_179.Origin, (v_208 = ((a_135 = b_179.Xaxis, Vec_$ctor_Z7AD9E565_1(a_135.X * 0.5, a_135.Y * 0.5, a_135.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_46.X + v_208.X, p_46.Y + v_208.Y, p_46.Z + v_208.Z)))), (v_209 = ((a_136 = b_179.Yaxis, Vec_$ctor_Z7AD9E565_1(a_136.X * 0.5, a_136.Y * 0.5, a_136.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_47.X + v_209.X, p_47.Y + v_209.Y, p_47.Z + v_209.Z)))), (v_210 = ((a_137 = b_179.Zaxis, Vec_$ctor_Z7AD9E565_1(a_137.X * 0.5, a_137.Y * 0.5, a_137.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_48.X + v_210.X, p_48.Y + v_210.Y, p_48.Z + v_210.Z))))), (x_174 = (a_139.X - b_181.X), (y_135 = (a_139.Y - b_181.Y), (z_104 = (a_139.Z - b_181.Z), Math.sqrt(((x_174 * x_174) + (y_135 * y_135)) + (z_104 * z_104)))))))) < 1E-09)("Center should remain at same position");
        let actual_31;
        const v_211 = scaled_1.Xaxis;
        actual_31 = Math.sqrt(((v_211.X * v_211.X) + (v_211.Y * v_211.Y)) + (v_211.Z * v_211.Z));
        if ((actual_31 === 20) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_31, 20, "SizeX should be scaled");
        }
        else {
            let valueType_31;
            let copyOfStruct_31 = actual_31;
            valueType_31 = float64_type;
            const primitiveTypes_31 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_31;
            if (contains(valueType_31, primitiveTypes_31, {
                Equals: equals,
                GetHashCode: (x_175) => (structuralHash(x_175) | 0),
            })) {
                const arg_61 = (20).toString();
                const arg_1_56 = actual_31.toString();
                errorMsg_31 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_61)(arg_1_56)("SizeX should be scaled");
            }
            else {
                errorMsg_31 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(20)(actual_31)("SizeX should be scaled");
            }
            throw new Exception(errorMsg_31);
        }
        Test_TestCaseBuilder__Zero(builder$0040_59);
    }));
})(), (() => {
    const builder$0040_60 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("scale static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_60, Test_TestCaseBuilder__Delay_1505(builder$0040_60, () => {
        let a_140, f_51, a_141, f_52, a_142, f_53, a_143, f_54;
        const box_57 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(1, 2, 3), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let scaled_2;
        const b_184 = box_57;
        scaled_2 = Box_$ctor_5706A3BA((a_140 = b_184.Origin, (f_51 = 2, Pnt_$ctor_Z7AD9E565_1(a_140.X * f_51, a_140.Y * f_51, a_140.Z * f_51))), (a_141 = b_184.Xaxis, (f_52 = 2, Vec_$ctor_Z7AD9E565_1(a_141.X * f_52, a_141.Y * f_52, a_141.Z * f_52))), (a_142 = b_184.Yaxis, (f_53 = 2, Vec_$ctor_Z7AD9E565_1(a_142.X * f_53, a_142.Y * f_53, a_142.Z * f_53))), (a_143 = b_184.Zaxis, (f_54 = 2, Vec_$ctor_Z7AD9E565_1(a_143.X * f_54, a_143.Y * f_54, a_143.Z * f_54))));
        let actual_32;
        const v_212 = scaled_2.Xaxis;
        actual_32 = Math.sqrt(((v_212.X * v_212.X) + (v_212.Y * v_212.Y)) + (v_212.Z * v_212.Z));
        if ((actual_32 === 20) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_32, 20, "SizeX should be scaled");
        }
        else {
            let valueType_32;
            let copyOfStruct_32 = actual_32;
            valueType_32 = float64_type;
            const primitiveTypes_32 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_32;
            if (contains(valueType_32, primitiveTypes_32, {
                Equals: equals,
                GetHashCode: (x_176) => (structuralHash(x_176) | 0),
            })) {
                const arg_62 = (20).toString();
                const arg_1_57 = actual_32.toString();
                errorMsg_32 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_62)(arg_1_57)("SizeX should be scaled");
            }
            else {
                errorMsg_32 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(20)(actual_32)("SizeX should be scaled");
            }
            throw new Exception(errorMsg_32);
        }
        Test_TestCaseBuilder__Zero(builder$0040_60);
    }));
})()])), Test_testList("Expansion Methods", ofArray([(() => {
    const builder$0040_61 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("expand by positive distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_61, Test_TestCaseBuilder__Delay_1505(builder$0040_61, () => {
        let v_213, v_214, v_215, v_216, v_217, v_218;
        const box_58 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        const expanded = Box_expand(1, box_58);
        Expect_isTrue(((v_213 = box_58.Xaxis, Math.sqrt(((v_213.X * v_213.X) + (v_213.Y * v_213.Y)) + (v_213.Z * v_213.Z)))) < ((v_214 = expanded.Xaxis, Math.sqrt(((v_214.X * v_214.X) + (v_214.Y * v_214.Y)) + (v_214.Z * v_214.Z)))))("Should expand in X");
        Expect_isTrue(((v_215 = box_58.Yaxis, Math.sqrt(((v_215.X * v_215.X) + (v_215.Y * v_215.Y)) + (v_215.Z * v_215.Z)))) < ((v_216 = expanded.Yaxis, Math.sqrt(((v_216.X * v_216.X) + (v_216.Y * v_216.Y)) + (v_216.Z * v_216.Z)))))("Should expand in Y");
        Expect_isTrue(((v_217 = box_58.Zaxis, Math.sqrt(((v_217.X * v_217.X) + (v_217.Y * v_217.Y)) + (v_217.Z * v_217.Z)))) < ((v_218 = expanded.Zaxis, Math.sqrt(((v_218.X * v_218.X) + (v_218.Y * v_218.Y)) + (v_218.Z * v_218.Z)))))("Should expand in Z");
        Test_TestCaseBuilder__Zero(builder$0040_61);
    }));
})(), (() => {
    const builder$0040_62 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("expand throws on underflow", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_62, Test_TestCaseBuilder__Delay_1505(builder$0040_62, () => {
        const box_59 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(2, 0, 0), Vec_$ctor_Z7AD9E565(0, 2, 0), Vec_$ctor_Z7AD9E565(0, 0, 2));
        Expect_throws(() => {
            Box_expand(-2, box_59);
        }, "Should throw when shrinking causes underflow");
        Test_TestCaseBuilder__Zero(builder$0040_62);
    }));
})(), (() => {
    const builder$0040_63 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("expandXYZ with different distances", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_63, Test_TestCaseBuilder__Delay_1505(builder$0040_63, () => {
        let v_219, v_220, v_221, v_222, v_223, v_224;
        const box_60 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        const expanded_1 = Box_expandXYZ(1, 2, 3, box_60);
        Expect_isTrue(((v_219 = box_60.Xaxis, Math.sqrt(((v_219.X * v_219.X) + (v_219.Y * v_219.Y)) + (v_219.Z * v_219.Z)))) < ((v_220 = expanded_1.Xaxis, Math.sqrt(((v_220.X * v_220.X) + (v_220.Y * v_220.Y)) + (v_220.Z * v_220.Z)))))("Should expand in X");
        Expect_isTrue(((v_221 = box_60.Yaxis, Math.sqrt(((v_221.X * v_221.X) + (v_221.Y * v_221.Y)) + (v_221.Z * v_221.Z)))) < ((v_222 = expanded_1.Yaxis, Math.sqrt(((v_222.X * v_222.X) + (v_222.Y * v_222.Y)) + (v_222.Z * v_222.Z)))))("Should expand in Y");
        Expect_isTrue(((v_223 = box_60.Zaxis, Math.sqrt(((v_223.X * v_223.X) + (v_223.Y * v_223.Y)) + (v_223.Z * v_223.Z)))) < ((v_224 = expanded_1.Zaxis, Math.sqrt(((v_224.X * v_224.X) + (v_224.Y * v_224.Y)) + (v_224.Z * v_224.Z)))))("Should expand in Z");
        Test_TestCaseBuilder__Zero(builder$0040_63);
    }));
})(), (() => {
    const builder$0040_64 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("expandRel with factor 1.5", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_64, Test_TestCaseBuilder__Delay_1505(builder$0040_64, () => {
        let a_151, b_202, p_51, p_50, p_49, v_225, a_144, v_226, a_145, v_227, a_146, b_205, b_203, p_54, p_53, p_52, v_228, a_147, v_229, a_148, v_230, a_149, x_177, y_138, z_105;
        const box_61 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        const expanded_2 = Box_expandRel(1.5, box_61);
        Expect_isTrue(((a_151 = ((b_202 = expanded_2, (p_51 = ((p_50 = ((p_49 = b_202.Origin, (v_225 = ((a_144 = b_202.Xaxis, Vec_$ctor_Z7AD9E565_1(a_144.X * 0.5, a_144.Y * 0.5, a_144.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_49.X + v_225.X, p_49.Y + v_225.Y, p_49.Z + v_225.Z)))), (v_226 = ((a_145 = b_202.Yaxis, Vec_$ctor_Z7AD9E565_1(a_145.X * 0.5, a_145.Y * 0.5, a_145.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_50.X + v_226.X, p_50.Y + v_226.Y, p_50.Z + v_226.Z)))), (v_227 = ((a_146 = b_202.Zaxis, Vec_$ctor_Z7AD9E565_1(a_146.X * 0.5, a_146.Y * 0.5, a_146.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_51.X + v_227.X, p_51.Y + v_227.Y, p_51.Z + v_227.Z))))), (b_205 = ((b_203 = box_61, (p_54 = ((p_53 = ((p_52 = b_203.Origin, (v_228 = ((a_147 = b_203.Xaxis, Vec_$ctor_Z7AD9E565_1(a_147.X * 0.5, a_147.Y * 0.5, a_147.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_52.X + v_228.X, p_52.Y + v_228.Y, p_52.Z + v_228.Z)))), (v_229 = ((a_148 = b_203.Yaxis, Vec_$ctor_Z7AD9E565_1(a_148.X * 0.5, a_148.Y * 0.5, a_148.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_53.X + v_229.X, p_53.Y + v_229.Y, p_53.Z + v_229.Z)))), (v_230 = ((a_149 = b_203.Zaxis, Vec_$ctor_Z7AD9E565_1(a_149.X * 0.5, a_149.Y * 0.5, a_149.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_54.X + v_230.X, p_54.Y + v_230.Y, p_54.Z + v_230.Z))))), (x_177 = (a_151.X - b_205.X), (y_138 = (a_151.Y - b_205.Y), (z_105 = (a_151.Z - b_205.Z), Math.sqrt(((x_177 * x_177) + (y_138 * y_138)) + (z_105 * z_105)))))))) < 1E-09)("Center should remain the same");
        let actual_33;
        const v_231 = expanded_2.Xaxis;
        actual_33 = Math.sqrt(((v_231.X * v_231.X) + (v_231.Y * v_231.Y)) + (v_231.Z * v_231.Z));
        if ((actual_33 === 15) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_33, 15, "SizeX should be 15");
        }
        else {
            let valueType_33;
            let copyOfStruct_33 = actual_33;
            valueType_33 = float64_type;
            const primitiveTypes_33 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_33;
            if (contains(valueType_33, primitiveTypes_33, {
                Equals: equals,
                GetHashCode: (x_178) => (structuralHash(x_178) | 0),
            })) {
                const arg_63 = (15).toString();
                const arg_1_58 = actual_33.toString();
                errorMsg_33 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_63)(arg_1_58)("SizeX should be 15");
            }
            else {
                errorMsg_33 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(15)(actual_33)("SizeX should be 15");
            }
            throw new Exception(errorMsg_33);
        }
        let actual_34;
        const v_232 = expanded_2.Yaxis;
        actual_34 = Math.sqrt(((v_232.X * v_232.X) + (v_232.Y * v_232.Y)) + (v_232.Z * v_232.Z));
        if ((actual_34 === 15) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_34, 15, "SizeY should be 15");
        }
        else {
            let valueType_34;
            let copyOfStruct_34 = actual_34;
            valueType_34 = float64_type;
            const primitiveTypes_34 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_34;
            if (contains(valueType_34, primitiveTypes_34, {
                Equals: equals,
                GetHashCode: (x_179) => (structuralHash(x_179) | 0),
            })) {
                const arg_64 = (15).toString();
                const arg_1_59 = actual_34.toString();
                errorMsg_34 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_64)(arg_1_59)("SizeY should be 15");
            }
            else {
                errorMsg_34 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(15)(actual_34)("SizeY should be 15");
            }
            throw new Exception(errorMsg_34);
        }
        let actual_35;
        const v_233 = expanded_2.Zaxis;
        actual_35 = Math.sqrt(((v_233.X * v_233.X) + (v_233.Y * v_233.Y)) + (v_233.Z * v_233.Z));
        if ((actual_35 === 15) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_35, 15, "SizeZ should be 15");
        }
        else {
            let valueType_35;
            let copyOfStruct_35 = actual_35;
            valueType_35 = float64_type;
            const primitiveTypes_35 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_35;
            if (contains(valueType_35, primitiveTypes_35, {
                Equals: equals,
                GetHashCode: (x_180) => (structuralHash(x_180) | 0),
            })) {
                const arg_65 = (15).toString();
                const arg_1_60 = actual_35.toString();
                errorMsg_35 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_65)(arg_1_60)("SizeZ should be 15");
            }
            else {
                errorMsg_35 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(15)(actual_35)("SizeZ should be 15");
            }
            throw new Exception(errorMsg_35);
        }
        Test_TestCaseBuilder__Zero(builder$0040_64);
    }));
})(), (() => {
    const builder$0040_65 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("expandRel rejects negative factor", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_65, Test_TestCaseBuilder__Delay_1505(builder$0040_65, () => {
        const box_62 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        Expect_throws(() => {
            Box_expandRel(-0.5, box_62);
        }, "Should throw on negative factor");
        Test_TestCaseBuilder__Zero(builder$0040_65);
    }));
})(), (() => {
    const builder$0040_66 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("expandRelXYZ with different factors", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_66, Test_TestCaseBuilder__Delay_1505(builder$0040_66, () => {
        let a_159, b_211, p_57, p_56, p_55, v_234, a_152, v_235, a_153, v_236, a_154, b_214, b_212, p_60, p_59, p_58, v_237, a_155, v_238, a_156, v_239, a_157, x_181, y_142, z_106;
        const box_63 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        const expanded_3 = Box_expandRelXYZ(1.5, 0.5, 2, box_63);
        Expect_isTrue(((a_159 = ((b_211 = expanded_3, (p_57 = ((p_56 = ((p_55 = b_211.Origin, (v_234 = ((a_152 = b_211.Xaxis, Vec_$ctor_Z7AD9E565_1(a_152.X * 0.5, a_152.Y * 0.5, a_152.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_55.X + v_234.X, p_55.Y + v_234.Y, p_55.Z + v_234.Z)))), (v_235 = ((a_153 = b_211.Yaxis, Vec_$ctor_Z7AD9E565_1(a_153.X * 0.5, a_153.Y * 0.5, a_153.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_56.X + v_235.X, p_56.Y + v_235.Y, p_56.Z + v_235.Z)))), (v_236 = ((a_154 = b_211.Zaxis, Vec_$ctor_Z7AD9E565_1(a_154.X * 0.5, a_154.Y * 0.5, a_154.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_57.X + v_236.X, p_57.Y + v_236.Y, p_57.Z + v_236.Z))))), (b_214 = ((b_212 = box_63, (p_60 = ((p_59 = ((p_58 = b_212.Origin, (v_237 = ((a_155 = b_212.Xaxis, Vec_$ctor_Z7AD9E565_1(a_155.X * 0.5, a_155.Y * 0.5, a_155.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_58.X + v_237.X, p_58.Y + v_237.Y, p_58.Z + v_237.Z)))), (v_238 = ((a_156 = b_212.Yaxis, Vec_$ctor_Z7AD9E565_1(a_156.X * 0.5, a_156.Y * 0.5, a_156.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_59.X + v_238.X, p_59.Y + v_238.Y, p_59.Z + v_238.Z)))), (v_239 = ((a_157 = b_212.Zaxis, Vec_$ctor_Z7AD9E565_1(a_157.X * 0.5, a_157.Y * 0.5, a_157.Z * 0.5))), Pnt_$ctor_Z7AD9E565_1(p_60.X + v_239.X, p_60.Y + v_239.Y, p_60.Z + v_239.Z))))), (x_181 = (a_159.X - b_214.X), (y_142 = (a_159.Y - b_214.Y), (z_106 = (a_159.Z - b_214.Z), Math.sqrt(((x_181 * x_181) + (y_142 * y_142)) + (z_106 * z_106)))))))) < 1E-09)("Center should remain the same");
        let actual_36;
        const v_240 = expanded_3.Xaxis;
        actual_36 = Math.sqrt(((v_240.X * v_240.X) + (v_240.Y * v_240.Y)) + (v_240.Z * v_240.Z));
        if ((actual_36 === 15) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_36, 15, "SizeX should be 15");
        }
        else {
            let valueType_36;
            let copyOfStruct_36 = actual_36;
            valueType_36 = float64_type;
            const primitiveTypes_36 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_36;
            if (contains(valueType_36, primitiveTypes_36, {
                Equals: equals,
                GetHashCode: (x_182) => (structuralHash(x_182) | 0),
            })) {
                const arg_66 = (15).toString();
                const arg_1_61 = actual_36.toString();
                errorMsg_36 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_66)(arg_1_61)("SizeX should be 15");
            }
            else {
                errorMsg_36 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(15)(actual_36)("SizeX should be 15");
            }
            throw new Exception(errorMsg_36);
        }
        let actual_37;
        const v_241 = expanded_3.Yaxis;
        actual_37 = Math.sqrt(((v_241.X * v_241.X) + (v_241.Y * v_241.Y)) + (v_241.Z * v_241.Z));
        if ((actual_37 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_37, 5, "SizeY should be 5");
        }
        else {
            let valueType_37;
            let copyOfStruct_37 = actual_37;
            valueType_37 = float64_type;
            const primitiveTypes_37 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_37;
            if (contains(valueType_37, primitiveTypes_37, {
                Equals: equals,
                GetHashCode: (x_183) => (structuralHash(x_183) | 0),
            })) {
                const arg_67 = (5).toString();
                const arg_1_62 = actual_37.toString();
                errorMsg_37 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_67)(arg_1_62)("SizeY should be 5");
            }
            else {
                errorMsg_37 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_37)("SizeY should be 5");
            }
            throw new Exception(errorMsg_37);
        }
        let actual_38;
        const v_242 = expanded_3.Zaxis;
        actual_38 = Math.sqrt(((v_242.X * v_242.X) + (v_242.Y * v_242.Y)) + (v_242.Z * v_242.Z));
        if ((actual_38 === 20) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_38, 20, "SizeZ should be 20");
        }
        else {
            let valueType_38;
            let copyOfStruct_38 = actual_38;
            valueType_38 = float64_type;
            const primitiveTypes_38 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_38;
            if (contains(valueType_38, primitiveTypes_38, {
                Equals: equals,
                GetHashCode: (x_184) => (structuralHash(x_184) | 0),
            })) {
                const arg_68 = (20).toString();
                const arg_1_63 = actual_38.toString();
                errorMsg_38 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_68)(arg_1_63)("SizeZ should be 20");
            }
            else {
                errorMsg_38 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(20)(actual_38)("SizeZ should be 20");
            }
            throw new Exception(errorMsg_38);
        }
        Test_TestCaseBuilder__Zero(builder$0040_66);
    }));
})()])), Test_testList("Containment", ofArray([(() => {
    const builder$0040_67 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Contains point inside", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_67, Test_TestCaseBuilder__Delay_1505(builder$0040_67, () => {
        const box_64 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        const pt_7 = Pnt_$ctor_Z7AD9E565(5, 5, 5);
        Expect_isTrue(Box__Contains_Z394ECE4D(box_64, pt_7))("Box should contain point inside");
        Test_TestCaseBuilder__Zero(builder$0040_67);
    }));
})(), (() => {
    const builder$0040_68 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Contains point on boundary", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_68, Test_TestCaseBuilder__Delay_1505(builder$0040_68, () => {
        const box_65 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        const pt_8 = Pnt_$ctor_Z7AD9E565(10, 5, 5);
        Expect_isTrue(Box__Contains_Z394ECE4D(box_65, pt_8))("Box should contain point on boundary");
        Test_TestCaseBuilder__Zero(builder$0040_68);
    }));
})(), (() => {
    const builder$0040_69 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Contains point outside", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_69, Test_TestCaseBuilder__Delay_1505(builder$0040_69, () => {
        const box_66 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        const pt_9 = Pnt_$ctor_Z7AD9E565(11, 5, 5);
        Expect_isFalse(Box__Contains_Z394ECE4D(box_66, pt_9))("Box should not contain point outside");
        Test_TestCaseBuilder__Zero(builder$0040_69);
    }));
})(), (() => {
    const builder$0040_70 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("contains static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_70, Test_TestCaseBuilder__Delay_1505(builder$0040_70, () => {
        const box_67 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        const pt_10 = Pnt_$ctor_Z7AD9E565(5, 5, 5);
        Expect_isTrue(Box__Contains_Z394ECE4D_1(box_67, pt_10))("Static method should work");
        Test_TestCaseBuilder__Zero(builder$0040_70);
    }));
})()])), Test_testList("BBox Conversion", ofArray([(() => {
    const builder$0040_71 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("BBox for axis-aligned box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_71, Test_TestCaseBuilder__Delay_1505(builder$0040_71, () => {
        const box_68 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(1, 2, 3), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        const bbox_1 = Box__get_BBox(box_68);
        const actual_39 = bbox_1.MinX;
        if ((actual_39 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_39, 1, "BBox MinX should be 1");
        }
        else {
            let valueType_39;
            let copyOfStruct_39 = actual_39;
            valueType_39 = float64_type;
            const primitiveTypes_39 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_39;
            if (contains(valueType_39, primitiveTypes_39, {
                Equals: equals,
                GetHashCode: (x_185) => (structuralHash(x_185) | 0),
            })) {
                const arg_69 = (1).toString();
                const arg_1_64 = actual_39.toString();
                errorMsg_39 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_69)(arg_1_64)("BBox MinX should be 1");
            }
            else {
                errorMsg_39 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_39)("BBox MinX should be 1");
            }
            throw new Exception(errorMsg_39);
        }
        const actual_40 = bbox_1.MinY;
        if ((actual_40 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_40, 2, "BBox MinY should be 2");
        }
        else {
            let valueType_40;
            let copyOfStruct_40 = actual_40;
            valueType_40 = float64_type;
            const primitiveTypes_40 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_40;
            if (contains(valueType_40, primitiveTypes_40, {
                Equals: equals,
                GetHashCode: (x_186) => (structuralHash(x_186) | 0),
            })) {
                const arg_70 = (2).toString();
                const arg_1_65 = actual_40.toString();
                errorMsg_40 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_70)(arg_1_65)("BBox MinY should be 2");
            }
            else {
                errorMsg_40 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_40)("BBox MinY should be 2");
            }
            throw new Exception(errorMsg_40);
        }
        const actual_41 = bbox_1.MinZ;
        if ((actual_41 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_41, 3, "BBox MinZ should be 3");
        }
        else {
            let valueType_41;
            let copyOfStruct_41 = actual_41;
            valueType_41 = float64_type;
            const primitiveTypes_41 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_41;
            if (contains(valueType_41, primitiveTypes_41, {
                Equals: equals,
                GetHashCode: (x_187) => (structuralHash(x_187) | 0),
            })) {
                const arg_71 = (3).toString();
                const arg_1_66 = actual_41.toString();
                errorMsg_41 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_71)(arg_1_66)("BBox MinZ should be 3");
            }
            else {
                errorMsg_41 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_41)("BBox MinZ should be 3");
            }
            throw new Exception(errorMsg_41);
        }
        const actual_42 = bbox_1.MaxX;
        if ((actual_42 === 11) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_42, 11, "BBox MaxX should be 11");
        }
        else {
            let valueType_42;
            let copyOfStruct_42 = actual_42;
            valueType_42 = float64_type;
            const primitiveTypes_42 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_42;
            if (contains(valueType_42, primitiveTypes_42, {
                Equals: equals,
                GetHashCode: (x_188) => (structuralHash(x_188) | 0),
            })) {
                const arg_72 = (11).toString();
                const arg_1_67 = actual_42.toString();
                errorMsg_42 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_72)(arg_1_67)("BBox MaxX should be 11");
            }
            else {
                errorMsg_42 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(11)(actual_42)("BBox MaxX should be 11");
            }
            throw new Exception(errorMsg_42);
        }
        const actual_43 = bbox_1.MaxY;
        if ((actual_43 === 7) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_43, 7, "BBox MaxY should be 7");
        }
        else {
            let valueType_43;
            let copyOfStruct_43 = actual_43;
            valueType_43 = float64_type;
            const primitiveTypes_43 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_43;
            if (contains(valueType_43, primitiveTypes_43, {
                Equals: equals,
                GetHashCode: (x_189) => (structuralHash(x_189) | 0),
            })) {
                const arg_73 = (7).toString();
                const arg_1_68 = actual_43.toString();
                errorMsg_43 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_73)(arg_1_68)("BBox MaxY should be 7");
            }
            else {
                errorMsg_43 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(7)(actual_43)("BBox MaxY should be 7");
            }
            throw new Exception(errorMsg_43);
        }
        const actual_44 = bbox_1.MaxZ;
        if ((actual_44 === 6) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_44, 6, "BBox MaxZ should be 6");
        }
        else {
            let valueType_44;
            let copyOfStruct_44 = actual_44;
            valueType_44 = float64_type;
            const primitiveTypes_44 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_44;
            if (contains(valueType_44, primitiveTypes_44, {
                Equals: equals,
                GetHashCode: (x_190) => (structuralHash(x_190) | 0),
            })) {
                const arg_74 = (6).toString();
                const arg_1_69 = actual_44.toString();
                errorMsg_44 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_74)(arg_1_69)("BBox MaxZ should be 6");
            }
            else {
                errorMsg_44 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(6)(actual_44)("BBox MaxZ should be 6");
            }
            throw new Exception(errorMsg_44);
        }
        Test_TestCaseBuilder__Zero(builder$0040_71);
    }));
})(), (() => {
    const builder$0040_72 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("BBox for rotated box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_72, Test_TestCaseBuilder__Delay_1505(builder$0040_72, () => {
        const box_69 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 10, 0), Vec_$ctor_Z7AD9E565(-10, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        const bbox_2 = Box__get_BBox(box_69);
        Expect_isTrue(bbox_2.MinX <= 0)("BBox should contain origin");
        Expect_isTrue(bbox_2.MaxX >= 0)("BBox should contain all corners");
        Test_TestCaseBuilder__Zero(builder$0040_72);
    }));
})()])), Test_testList("Plane Conversion", singleton((() => {
    const builder$0040_73 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Plane from box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_73, Test_TestCaseBuilder__Delay_1505(builder$0040_73, () => {
        let a_161, b_222, x_201, y_159, z_114;
        const box_70 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let plane;
        const b_220 = box_70;
        let x_191;
        const v_243 = b_220.Xaxis;
        const x_192 = v_243.X;
        const y_152 = v_243.Y;
        const z_107 = v_243.Z;
        const l_3 = Math.sqrt(((x_192 * x_192) + (y_152 * y_152)) + (z_107 * z_107));
        if (!(l_3 > 1E-12)) {
            failUnit3("Vec.Unitized", x_192, y_152, z_107);
        }
        const f_67 = 1 / l_3;
        x_191 = UnitVec_$ctor_Z7AD9E565(f_67 * x_192, f_67 * y_152, f_67 * z_107);
        let y_154;
        const v_244 = b_220.Yaxis;
        const x_195 = v_244.X;
        const y_155 = v_244.Y;
        const z_109 = v_244.Z;
        const l_4 = Math.sqrt(((x_195 * x_195) + (y_155 * y_155)) + (z_109 * z_109));
        if (!(l_4 > 1E-12)) {
            failUnit3("Vec.Unitized", x_195, y_155, z_109);
        }
        const f_68 = 1 / l_4;
        y_154 = UnitVec_$ctor_Z7AD9E565(f_68 * x_195, f_68 * y_155, f_68 * z_109);
        let z_111;
        const v_245 = b_220.Zaxis;
        const x_198 = v_245.X;
        const y_157 = v_245.Y;
        const z_112 = v_245.Z;
        const l_5 = Math.sqrt(((x_198 * x_198) + (y_157 * y_157)) + (z_112 * z_112));
        if (!(l_5 > 1E-12)) {
            failUnit3("Vec.Unitized", x_198, y_157, z_112);
        }
        const f_69 = 1 / l_5;
        z_111 = UnitVec_$ctor_Z7AD9E565(f_69 * x_198, f_69 * y_157, f_69 * z_112);
        plane = PPlane_$ctor_3CB4665C(b_220.Origin, x_191, y_154, z_111);
        Expect_isTrue(((a_161 = plane.Origin, (b_222 = box_70.Origin, (x_201 = (a_161.X - b_222.X), (y_159 = (a_161.Y - b_222.Y), (z_114 = (a_161.Z - b_222.Z), Math.sqrt(((x_201 * x_201) + (y_159 * y_159)) + (z_114 * z_114)))))))) < 1E-09)("Plane origin should match box origin");
        Test_TestCaseBuilder__Zero(builder$0040_73);
    }));
})())), Test_testList("Points and Edges", ofArray([(() => {
    const builder$0040_74 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Points array has 8 points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_74, Test_TestCaseBuilder__Delay_1505(builder$0040_74, () => {
        const box_71 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        const pts_8 = Box__get_Points(box_71);
        const actual_45 = pts_8.length | 0;
        if ((actual_45 === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_45, 8, "Should have 8 points");
        }
        else {
            let valueType_45;
            let copyOfStruct_45 = actual_45;
            valueType_45 = int32_type;
            const primitiveTypes_45 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_45;
            if (contains(valueType_45, primitiveTypes_45, {
                Equals: equals,
                GetHashCode: (x_202) => (structuralHash(x_202) | 0),
            })) {
                const arg_75 = int32ToString(8);
                const arg_1_70 = int32ToString(actual_45);
                errorMsg_45 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_75)(arg_1_70)("Should have 8 points");
            }
            else {
                errorMsg_45 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual_45)("Should have 8 points");
            }
            throw new Exception(errorMsg_45);
        }
        Test_TestCaseBuilder__Zero(builder$0040_74);
    }));
})(), (() => {
    const builder$0040_75 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Pt0 is Origin", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_75, Test_TestCaseBuilder__Delay_1505(builder$0040_75, () => {
        let a_163, b_225, x_203, y_161, z_115;
        const box_72 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(1, 2, 3), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        Expect_isTrue(((a_163 = box_72.Origin, (b_225 = box_72.Origin, (x_203 = (a_163.X - b_225.X), (y_161 = (a_163.Y - b_225.Y), (z_115 = (a_163.Z - b_225.Z), Math.sqrt(((x_203 * x_203) + (y_161 * y_161)) + (z_115 * z_115)))))))) < 1E-09)("Pt0 should equal Origin");
        Test_TestCaseBuilder__Zero(builder$0040_75);
    }));
})(), (() => {
    const builder$0040_76 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Pt6 is FarCorner", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_76, Test_TestCaseBuilder__Delay_1505(builder$0040_76, () => {
        let a_165, b_226, p_65, p_64, p_63, v_246, v_247, v_248, b_229, b_227, p_68, p_67, p_66, v_249, v_250, v_251, x_204, y_162, z_116;
        const box_73 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(1, 2, 3), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        Expect_isTrue(((a_165 = ((b_226 = box_73, (p_65 = ((p_64 = ((p_63 = b_226.Origin, (v_246 = b_226.Xaxis, Pnt_$ctor_Z7AD9E565_1(p_63.X + v_246.X, p_63.Y + v_246.Y, p_63.Z + v_246.Z)))), (v_247 = b_226.Yaxis, Pnt_$ctor_Z7AD9E565_1(p_64.X + v_247.X, p_64.Y + v_247.Y, p_64.Z + v_247.Z)))), (v_248 = b_226.Zaxis, Pnt_$ctor_Z7AD9E565_1(p_65.X + v_248.X, p_65.Y + v_248.Y, p_65.Z + v_248.Z))))), (b_229 = ((b_227 = box_73, (p_68 = ((p_67 = ((p_66 = b_227.Origin, (v_249 = b_227.Xaxis, Pnt_$ctor_Z7AD9E565_1(p_66.X + v_249.X, p_66.Y + v_249.Y, p_66.Z + v_249.Z)))), (v_250 = b_227.Yaxis, Pnt_$ctor_Z7AD9E565_1(p_67.X + v_250.X, p_67.Y + v_250.Y, p_67.Z + v_250.Z)))), (v_251 = b_227.Zaxis, Pnt_$ctor_Z7AD9E565_1(p_68.X + v_251.X, p_68.Y + v_251.Y, p_68.Z + v_251.Z))))), (x_204 = (a_165.X - b_229.X), (y_162 = (a_165.Y - b_229.Y), (z_116 = (a_165.Z - b_229.Z), Math.sqrt(((x_204 * x_204) + (y_162 * y_162)) + (z_116 * z_116)))))))) < 1E-09)("Pt6 should equal FarCorner");
        Test_TestCaseBuilder__Zero(builder$0040_76);
    }));
})(), (() => {
    const builder$0040_77 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Edges array has 12 edges", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_77, Test_TestCaseBuilder__Delay_1505(builder$0040_77, () => {
        const box_74 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        const edges = Box__get_Edges(box_74);
        const actual_46 = edges.length | 0;
        if ((actual_46 === 12) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_46, 12, "Should have 12 edges");
        }
        else {
            let valueType_46;
            let copyOfStruct_46 = actual_46;
            valueType_46 = int32_type;
            const primitiveTypes_46 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_46;
            if (contains(valueType_46, primitiveTypes_46, {
                Equals: equals,
                GetHashCode: (x_205) => (structuralHash(x_205) | 0),
            })) {
                const arg_76 = int32ToString(12);
                const arg_1_71 = int32ToString(actual_46);
                errorMsg_46 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_76)(arg_1_71)("Should have 12 edges");
            }
            else {
                errorMsg_46 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(12)(actual_46)("Should have 12 edges");
            }
            throw new Exception(errorMsg_46);
        }
        Test_TestCaseBuilder__Zero(builder$0040_77);
    }));
})(), (() => {
    const builder$0040_78 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Edge0 is parallel to Xaxis", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_78, Test_TestCaseBuilder__Delay_1505(builder$0040_78, () => {
        let p_69, v_252, a_167, ln, b_233, x_206, y_164, z_117;
        const box_75 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        let edge;
        const b_230 = box_75;
        const st = b_230.Origin;
        edge = Line3D_$ctor_5A6659A0(st, (p_69 = st, (v_252 = b_230.Xaxis, Pnt_$ctor_Z7AD9E565_1(p_69.X + v_252.X, p_69.Y + v_252.Y, p_69.Z + v_252.Z))));
        Expect_isTrue(((a_167 = ((ln = edge, Pnt_$ctor_Z7AD9E565_1(ln.FromX, ln.FromY, ln.FromZ))), (b_233 = box_75.Origin, (x_206 = (a_167.X - b_233.X), (y_164 = (a_167.Y - b_233.Y), (z_117 = (a_167.Z - b_233.Z), Math.sqrt(((x_206 * x_206) + (y_164 * y_164)) + (z_117 * z_117)))))))) < 1E-09)("Edge0 should start at Pt0");
        Test_TestCaseBuilder__Zero(builder$0040_78);
    }));
})(), (() => {
    const builder$0040_79 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Faces array has 6 faces", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_79, Test_TestCaseBuilder__Delay_1505(builder$0040_79, () => {
        const box_76 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        const faces = Box__get_Faces(box_76);
        const actual_47 = faces.length | 0;
        if ((actual_47 === 6) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_47, 6, "Should have 6 faces");
        }
        else {
            let valueType_47;
            let copyOfStruct_47 = actual_47;
            valueType_47 = int32_type;
            const primitiveTypes_47 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_47;
            if (contains(valueType_47, primitiveTypes_47, {
                Equals: equals,
                GetHashCode: (x_207) => (structuralHash(x_207) | 0),
            })) {
                const arg_77 = int32ToString(6);
                const arg_1_72 = int32ToString(actual_47);
                errorMsg_47 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_77)(arg_1_72)("Should have 6 faces");
            }
            else {
                errorMsg_47 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(6)(actual_47)("Should have 6 faces");
            }
            throw new Exception(errorMsg_47);
        }
        Test_TestCaseBuilder__Zero(builder$0040_79);
    }));
})(), (() => {
    const builder$0040_80 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("BottomFace has correct origin", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_80, Test_TestCaseBuilder__Delay_1505(builder$0040_80, () => {
        let a_169, b_235, x_208, y_166, z_118;
        const box_77 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(1, 2, 3), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        const bottom = Box__get_BottomFace(box_77);
        Expect_isTrue(((a_169 = bottom.Origin, (b_235 = box_77.Origin, (x_208 = (a_169.X - b_235.X), (y_166 = (a_169.Y - b_235.Y), (z_118 = (a_169.Z - b_235.Z), Math.sqrt(((x_208 * x_208) + (y_166 * y_166)) + (z_118 * z_118)))))))) < 1E-09)("Bottom face should start at origin");
        Test_TestCaseBuilder__Zero(builder$0040_80);
    }));
})(), (() => {
    const builder$0040_81 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("TopFace has correct origin", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_81, Test_TestCaseBuilder__Delay_1505(builder$0040_81, () => {
        let a_171, b_237, p_70, v_253, x_209, y_167, z_119;
        const box_78 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(1, 2, 3), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        const top = Box__get_TopFace(box_78);
        Expect_isTrue(((a_171 = top.Origin, (b_237 = ((p_70 = box_78.Origin, (v_253 = box_78.Zaxis, Pnt_$ctor_Z7AD9E565_1(p_70.X + v_253.X, p_70.Y + v_253.Y, p_70.Z + v_253.Z)))), (x_209 = (a_171.X - b_237.X), (y_167 = (a_171.Y - b_237.Y), (z_119 = (a_171.Z - b_237.Z), Math.sqrt(((x_209 * x_209) + (y_167 * y_167)) + (z_119 * z_119)))))))) < 1E-09)("Top face should be offset by Zaxis");
        Test_TestCaseBuilder__Zero(builder$0040_81);
    }));
})()])), Test_testList("Equality Methods", ofArray([(() => {
    const builder$0040_82 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("equals with exact match", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_82, Test_TestCaseBuilder__Delay_1505(builder$0040_82, () => {
        const a_172 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        const b_238 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        Expect_isTrue(Box_equals(0, a_172, b_238))("Exact boxes should be equal");
        Test_TestCaseBuilder__Zero(builder$0040_82);
    }));
})(), (() => {
    const builder$0040_83 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("equals with tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_83, Test_TestCaseBuilder__Delay_1505(builder$0040_83, () => {
        const a_174 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        const b_240 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0.001, 0.001, 0.001), Vec_$ctor_Z7AD9E565(10.001, 0, 0), Vec_$ctor_Z7AD9E565(0, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        Expect_isTrue(Box_equals(0.01, a_174, b_240))("Boxes should be equal within tolerance");
        Expect_isFalse(Box_equals(0.0001, a_174, b_240))("Boxes should not be equal with small tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_83);
    }));
})(), (() => {
    const builder$0040_84 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("notEquals", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_84, Test_TestCaseBuilder__Delay_1505(builder$0040_84, () => {
        const a_177 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        const b_243 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(1, 1, 1), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 10, 0), Vec_$ctor_Z7AD9E565(0, 0, 10));
        Expect_isTrue(Box_notEquals(0.5, a_177, b_243))("Different boxes should not be equal");
        Test_TestCaseBuilder__Zero(builder$0040_84);
    }));
})()])), Test_testList("Face Area Methods", ofArray([(() => {
    const builder$0040_85 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("AreaOfBiggestFace", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_85, Test_TestCaseBuilder__Delay_1505(builder$0040_85, () => {
        const box_79 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        const area = Box__get_AreaOfBiggestFace(box_79);
        const actual_48 = area;
        if ((actual_48 === 50) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_48, 50, "Biggest face should be X*Y = 50");
        }
        else {
            let valueType_48;
            let copyOfStruct_48 = actual_48;
            valueType_48 = float64_type;
            const primitiveTypes_48 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_48;
            if (contains(valueType_48, primitiveTypes_48, {
                Equals: equals,
                GetHashCode: (x_210) => (structuralHash(x_210) | 0),
            })) {
                const arg_78 = (50).toString();
                const arg_1_73 = actual_48.toString();
                errorMsg_48 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_78)(arg_1_73)("Biggest face should be X*Y = 50");
            }
            else {
                errorMsg_48 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(50)(actual_48)("Biggest face should be X*Y = 50");
            }
            throw new Exception(errorMsg_48);
        }
        Test_TestCaseBuilder__Zero(builder$0040_85);
    }));
})(), (() => {
    const builder$0040_86 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("AreaOfSmallestFace", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_86, Test_TestCaseBuilder__Delay_1505(builder$0040_86, () => {
        const box_80 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565(10, 0, 0), Vec_$ctor_Z7AD9E565(0, 5, 0), Vec_$ctor_Z7AD9E565(0, 0, 3));
        const area_1 = Box__get_AreaOfSmallestFace(box_80);
        const actual_49 = area_1;
        if ((actual_49 === 15) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_49, 15, "Smallest face should be Y*Z = 15");
        }
        else {
            let valueType_49;
            let copyOfStruct_49 = actual_49;
            valueType_49 = float64_type;
            const primitiveTypes_49 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_49;
            if (contains(valueType_49, primitiveTypes_49, {
                Equals: equals,
                GetHashCode: (x_211) => (structuralHash(x_211) | 0),
            })) {
                const arg_79 = (15).toString();
                const arg_1_74 = actual_49.toString();
                errorMsg_49 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_79)(arg_1_74)("Smallest face should be Y*Z = 15");
            }
            else {
                errorMsg_49 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(15)(actual_49)("Smallest face should be Y*Z = 15");
            }
            throw new Exception(errorMsg_49);
        }
        Test_TestCaseBuilder__Zero(builder$0040_86);
    }));
})()])), Test_testList("Box.createFromPlaneAndPoints", ofArray([(() => {
    const builder$0040_87 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("all positive projections", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_87, Test_TestCaseBuilder__Delay_1505(builder$0040_87, () => {
        let a_182, v_254, a_183, v_255, a_184, v_256;
        const pl_3 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1));
        const pts_9 = [Pnt_$ctor_Z7AD9E565(1, 1, 1), Pnt_$ctor_Z7AD9E565(2, 1, 1), Pnt_$ctor_Z7AD9E565(2, 2, 1), Pnt_$ctor_Z7AD9E565(1, 2, 1), Pnt_$ctor_Z7AD9E565(1, 1, 2), Pnt_$ctor_Z7AD9E565(2, 1, 2), Pnt_$ctor_Z7AD9E565(2, 2, 2), Pnt_$ctor_Z7AD9E565(1, 2, 2)];
        const box_81 = Box_createFromPlaneAndPoints(pl_3, pts_9);
        Expect_isTrue(Math.abs(box_81.Origin.X - 1) < 1E-09)("positive - origin X");
        Expect_isTrue(Math.abs(box_81.Origin.Y - 1) < 1E-09)("positive - origin Y");
        Expect_isTrue(Math.abs(box_81.Origin.Z - 1) < 1E-09)("positive - origin Z");
        Expect_isTrue((a_182 = ((v_254 = box_81.Xaxis, Math.sqrt(((v_254.X * v_254.X) + (v_254.Y * v_254.Y)) + (v_254.Z * v_254.Z)))), Math.abs(a_182 - 1) < 1E-09))("positive - SizeX");
        Expect_isTrue((a_183 = ((v_255 = box_81.Yaxis, Math.sqrt(((v_255.X * v_255.X) + (v_255.Y * v_255.Y)) + (v_255.Z * v_255.Z)))), Math.abs(a_183 - 1) < 1E-09))("positive - SizeY");
        Expect_isTrue((a_184 = ((v_256 = box_81.Zaxis, Math.sqrt(((v_256.X * v_256.X) + (v_256.Y * v_256.Y)) + (v_256.Z * v_256.Z)))), Math.abs(a_184 - 1) < 1E-09))("positive - SizeZ");
        Test_TestCaseBuilder__Zero(builder$0040_87);
    }));
})(), (() => {
    const builder$0040_88 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("all negative projections", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_88, Test_TestCaseBuilder__Delay_1505(builder$0040_88, () => {
        let a_188, v_257, a_189, v_258, a_190, v_259;
        const pl_5 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1));
        const pts_11 = [Pnt_$ctor_Z7AD9E565(-2, -2, -2), Pnt_$ctor_Z7AD9E565(-1, -2, -2), Pnt_$ctor_Z7AD9E565(-1, -1, -2), Pnt_$ctor_Z7AD9E565(-2, -1, -2), Pnt_$ctor_Z7AD9E565(-2, -2, -1), Pnt_$ctor_Z7AD9E565(-1, -2, -1), Pnt_$ctor_Z7AD9E565(-1, -1, -1), Pnt_$ctor_Z7AD9E565(-2, -1, -1)];
        const box_82 = Box_createFromPlaneAndPoints(pl_5, pts_11);
        Expect_isTrue(Math.abs(box_82.Origin.X - -2) < 1E-09)("negative - origin X");
        Expect_isTrue(Math.abs(box_82.Origin.Y - -2) < 1E-09)("negative - origin Y");
        Expect_isTrue(Math.abs(box_82.Origin.Z - -2) < 1E-09)("negative - origin Z");
        Expect_isTrue((a_188 = ((v_257 = box_82.Xaxis, Math.sqrt(((v_257.X * v_257.X) + (v_257.Y * v_257.Y)) + (v_257.Z * v_257.Z)))), Math.abs(a_188 - 1) < 1E-09))("negative - SizeX");
        Expect_isTrue((a_189 = ((v_258 = box_82.Yaxis, Math.sqrt(((v_258.X * v_258.X) + (v_258.Y * v_258.Y)) + (v_258.Z * v_258.Z)))), Math.abs(a_189 - 1) < 1E-09))("negative - SizeY");
        Expect_isTrue((a_190 = ((v_259 = box_82.Zaxis, Math.sqrt(((v_259.X * v_259.X) + (v_259.Y * v_259.Y)) + (v_259.Z * v_259.Z)))), Math.abs(a_190 - 1) < 1E-09))("negative - SizeZ");
        Test_TestCaseBuilder__Zero(builder$0040_88);
    }));
})(), (() => {
    const builder$0040_89 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("mixed projections", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_89, Test_TestCaseBuilder__Delay_1505(builder$0040_89, () => {
        let a_194, v_260, a_195, v_261, a_196, v_262;
        const pl_7 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1));
        const pts_13 = [Pnt_$ctor_Z7AD9E565(-1, -1, -1), Pnt_$ctor_Z7AD9E565(2, -1, -1), Pnt_$ctor_Z7AD9E565(2, 1, -1), Pnt_$ctor_Z7AD9E565(-1, 1, -1), Pnt_$ctor_Z7AD9E565(-1, -1, 1), Pnt_$ctor_Z7AD9E565(2, -1, 1), Pnt_$ctor_Z7AD9E565(2, 1, 1), Pnt_$ctor_Z7AD9E565(-1, 1, 1)];
        const box_83 = Box_createFromPlaneAndPoints(pl_7, pts_13);
        Expect_isTrue(Math.abs(box_83.Origin.X - -1) < 1E-09)("mixed - origin X");
        Expect_isTrue(Math.abs(box_83.Origin.Y - -1) < 1E-09)("mixed - origin Y");
        Expect_isTrue(Math.abs(box_83.Origin.Z - -1) < 1E-09)("mixed - origin Z");
        Expect_isTrue((a_194 = ((v_260 = box_83.Xaxis, Math.sqrt(((v_260.X * v_260.X) + (v_260.Y * v_260.Y)) + (v_260.Z * v_260.Z)))), Math.abs(a_194 - 3) < 1E-09))("mixed - SizeX");
        Expect_isTrue((a_195 = ((v_261 = box_83.Yaxis, Math.sqrt(((v_261.X * v_261.X) + (v_261.Y * v_261.Y)) + (v_261.Z * v_261.Z)))), Math.abs(a_195 - 2) < 1E-09))("mixed - SizeY");
        Expect_isTrue((a_196 = ((v_262 = box_83.Zaxis, Math.sqrt(((v_262.X * v_262.X) + (v_262.Y * v_262.Y)) + (v_262.Z * v_262.Z)))), Math.abs(a_196 - 2) < 1E-09))("mixed - SizeZ");
        Test_TestCaseBuilder__Zero(builder$0040_89);
    }));
})(), (() => {
    const builder$0040_90 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset plane", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_90, Test_TestCaseBuilder__Delay_1505(builder$0040_90, () => {
        let a_200, v_263, a_201, v_264, a_202, v_265;
        const pl_9 = Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_6181AF53(Pnt_$ctor_Z7AD9E565(10, 10, 10), Vec_$ctor_Z7AD9E565_2(1, 0, 0), Vec_$ctor_Z7AD9E565_2(0, 1, 0));
        const pts_15 = [Pnt_$ctor_Z7AD9E565(11, 11, 11), Pnt_$ctor_Z7AD9E565(13, 11, 11), Pnt_$ctor_Z7AD9E565(13, 12, 11), Pnt_$ctor_Z7AD9E565(11, 12, 11), Pnt_$ctor_Z7AD9E565(11, 11, 12), Pnt_$ctor_Z7AD9E565(13, 11, 12), Pnt_$ctor_Z7AD9E565(13, 12, 12), Pnt_$ctor_Z7AD9E565(11, 12, 12)];
        const box_84 = Box_createFromPlaneAndPoints(pl_9, pts_15);
        Expect_isTrue(Math.abs(box_84.Origin.X - 11) < 1E-09)("offset - origin X");
        Expect_isTrue(Math.abs(box_84.Origin.Y - 11) < 1E-09)("offset - origin Y");
        Expect_isTrue(Math.abs(box_84.Origin.Z - 11) < 1E-09)("offset - origin Z");
        Expect_isTrue((a_200 = ((v_263 = box_84.Xaxis, Math.sqrt(((v_263.X * v_263.X) + (v_263.Y * v_263.Y)) + (v_263.Z * v_263.Z)))), Math.abs(a_200 - 2) < 1E-09))("offset - SizeX");
        Expect_isTrue((a_201 = ((v_264 = box_84.Yaxis, Math.sqrt(((v_264.X * v_264.X) + (v_264.Y * v_264.Y)) + (v_264.Z * v_264.Z)))), Math.abs(a_201 - 1) < 1E-09))("offset - SizeY");
        Expect_isTrue((a_202 = ((v_265 = box_84.Zaxis, Math.sqrt(((v_265.X * v_265.X) + (v_265.Y * v_265.Y)) + (v_265.Z * v_265.Z)))), Math.abs(a_202 - 1) < 1E-09))("offset - SizeZ");
        Test_TestCaseBuilder__Zero(builder$0040_90);
    }));
})()])), Test_testList("IntersectRay", ofArray([(() => {
    const builder$0040_91 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ray through center of axis-aligned box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_91, Test_TestCaseBuilder__Delay_1505(builder$0040_91, () => {
        const box_85 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(2, 0, 0), Vec_$ctor_Z7AD9E565(0, 2, 0), Vec_$ctor_Z7AD9E565(0, 0, 2));
        const ray = Line3D_$ctor_5A6659A0_1(Pnt_$ctor_Z7AD9E565(-5, 1, 1), Pnt_$ctor_Z7AD9E565(5, 1, 1));
        const result = Box__IntersectRay_4CC2E360(box_85, ray);
        Expect_isTrue(result != null)("Should intersect");
        const patternInput = value_6(result);
        const tExit = patternInput[1];
        const tEntry = patternInput[0];
        Expect_isTrue(Math.abs(tEntry - 0.5) < 1E-09)(`Entry parameter should be 0.5, got ${tEntry}`);
        Expect_isTrue(Math.abs(tExit - 0.7) < 1E-09)(`Exit parameter should be 0.7, got ${tExit}`);
        Test_TestCaseBuilder__Zero(builder$0040_91);
    }));
})(), (() => {
    const builder$0040_92 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ray missing box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_92, Test_TestCaseBuilder__Delay_1505(builder$0040_92, () => {
        const box_86 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(2, 0, 0), Vec_$ctor_Z7AD9E565(0, 2, 0), Vec_$ctor_Z7AD9E565(0, 0, 2));
        const ray_1 = Line3D_$ctor_5A6659A0_1(Pnt_$ctor_Z7AD9E565(-5, 10, 10), Pnt_$ctor_Z7AD9E565(5, 10, 10));
        const result_1 = Box__IntersectRay_4CC2E360(box_86, ray_1);
        Expect_isTrue(result_1 == null)("Should not intersect");
        Test_TestCaseBuilder__Zero(builder$0040_92);
    }));
})(), (() => {
    const builder$0040_93 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ray starting inside box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_93, Test_TestCaseBuilder__Delay_1505(builder$0040_93, () => {
        const box_87 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(2, 0, 0), Vec_$ctor_Z7AD9E565(0, 2, 0), Vec_$ctor_Z7AD9E565(0, 0, 2));
        const ray_2 = Line3D_$ctor_5A6659A0_1(Pnt_$ctor_Z7AD9E565(1, 1, 1), Pnt_$ctor_Z7AD9E565(3, 1, 1));
        const result_2 = Box__IntersectRay_4CC2E360(box_87, ray_2);
        Expect_isTrue(result_2 != null)("Should intersect");
        const patternInput_1 = value_6(result_2);
        const tExit_1 = patternInput_1[1];
        const tEntry_1 = patternInput_1[0];
        Expect_isTrue(tEntry_1 < 0)(`Entry should be negative (behind ray origin), got ${tEntry_1}`);
        Expect_isTrue(tExit_1 > 0)(`Exit should be positive, got ${tExit_1}`);
        Test_TestCaseBuilder__Zero(builder$0040_93);
    }));
})(), (() => {
    const builder$0040_94 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ray parallel to box face but outside", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_94, Test_TestCaseBuilder__Delay_1505(builder$0040_94, () => {
        const box_88 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(2, 0, 0), Vec_$ctor_Z7AD9E565(0, 2, 0), Vec_$ctor_Z7AD9E565(0, 0, 2));
        const ray_3 = Line3D_$ctor_5A6659A0_1(Pnt_$ctor_Z7AD9E565(-1, 5, 1), Pnt_$ctor_Z7AD9E565(3, 5, 1));
        const result_3 = Box__IntersectRay_4CC2E360(box_88, ray_3);
        Expect_isTrue(result_3 == null)("Should not intersect (ray parallel but outside)");
        Test_TestCaseBuilder__Zero(builder$0040_94);
    }));
})(), (() => {
    const builder$0040_95 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ray parallel to box face inside slab", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_95, Test_TestCaseBuilder__Delay_1505(builder$0040_95, () => {
        const box_89 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(2, 0, 0), Vec_$ctor_Z7AD9E565(0, 2, 0), Vec_$ctor_Z7AD9E565(0, 0, 2));
        const ray_4 = Line3D_$ctor_5A6659A0_1(Pnt_$ctor_Z7AD9E565(-5, 1, 1), Pnt_$ctor_Z7AD9E565(5, 1, 1));
        const result_4 = Box__IntersectRay_4CC2E360(box_89, ray_4);
        Expect_isTrue(result_4 != null)("Should intersect");
        Test_TestCaseBuilder__Zero(builder$0040_95);
    }));
})(), (() => {
    const builder$0040_96 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("rotated box ray intersection", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_96, Test_TestCaseBuilder__Delay_1505(builder$0040_96, () => {
        const sqrt2over2 = Math.sqrt(2) / 2;
        let xAxis_113;
        const a_205 = Vec_$ctor_Z7AD9E565(sqrt2over2, sqrt2over2, 0);
        xAxis_113 = Vec_$ctor_Z7AD9E565_1(a_205.X * 2, a_205.Y * 2, a_205.Z * 2);
        let yAxis_113;
        const a_206 = Vec_$ctor_Z7AD9E565(-sqrt2over2, sqrt2over2, 0);
        yAxis_113 = Vec_$ctor_Z7AD9E565_1(a_206.X * 2, a_206.Y * 2, a_206.Z * 2);
        const zAxis_113 = Vec_$ctor_Z7AD9E565(0, 0, 2);
        const box_90 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), xAxis_113, yAxis_113, zAxis_113);
        const ray_5 = Line3D_$ctor_5A6659A0_1(Pnt_$ctor_Z7AD9E565(-5, 0, 1), Pnt_$ctor_Z7AD9E565(5, 0, 1));
        const result_5 = Box__IntersectRay_4CC2E360(box_90, ray_5);
        Expect_isTrue(result_5 != null)("Should intersect rotated box");
        Test_TestCaseBuilder__Zero(builder$0040_96);
    }));
})(), (() => {
    const builder$0040_97 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ray too short (zero length)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_97, Test_TestCaseBuilder__Delay_1505(builder$0040_97, () => {
        const box_91 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(2, 0, 0), Vec_$ctor_Z7AD9E565(0, 2, 0), Vec_$ctor_Z7AD9E565(0, 0, 2));
        const ray_6 = Line3D_$ctor_5A6659A0_1(Pnt_$ctor_Z7AD9E565(1, 1, 1), Pnt_$ctor_Z7AD9E565(1, 1, 1));
        const result_6 = Box__IntersectRay_4CC2E360(box_91, ray_6);
        Expect_isTrue(result_6 == null)("Should return None for zero-length ray");
        Test_TestCaseBuilder__Zero(builder$0040_97);
    }));
})(), (() => {
    const builder$0040_98 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ray grazing box corner", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_98, Test_TestCaseBuilder__Delay_1505(builder$0040_98, () => {
        const box_92 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(2, 0, 0), Vec_$ctor_Z7AD9E565(0, 2, 0), Vec_$ctor_Z7AD9E565(0, 0, 2));
        const ray_7 = Line3D_$ctor_5A6659A0_1(Pnt_$ctor_Z7AD9E565(2, 2, 0), Pnt_$ctor_Z7AD9E565(2, 2, 4));
        const result_7 = Box__IntersectRay_4CC2E360(box_92, ray_7);
        Expect_isTrue(result_7 != null)("Should intersect at corner edge");
        Test_TestCaseBuilder__Zero(builder$0040_98);
    }));
})(), (() => {
    const builder$0040_99 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ray along box edge", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_99, Test_TestCaseBuilder__Delay_1505(builder$0040_99, () => {
        const box_93 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(2, 0, 0), Vec_$ctor_Z7AD9E565(0, 2, 0), Vec_$ctor_Z7AD9E565(0, 0, 2));
        const ray_8 = Line3D_$ctor_5A6659A0_1(Pnt_$ctor_Z7AD9E565(0, 0, -1), Pnt_$ctor_Z7AD9E565(0, 0, 3));
        const result_8 = Box__IntersectRay_4CC2E360(box_93, ray_8);
        Expect_isTrue(result_8 != null)("Should intersect along edge");
        Test_TestCaseBuilder__Zero(builder$0040_99);
    }));
})(), (() => {
    const builder$0040_100 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static intersectRay function", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_100, Test_TestCaseBuilder__Delay_1505(builder$0040_100, () => {
        const box_94 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(2, 0, 0), Vec_$ctor_Z7AD9E565(0, 2, 0), Vec_$ctor_Z7AD9E565(0, 0, 2));
        const ray_9 = Line3D_$ctor_5A6659A0_1(Pnt_$ctor_Z7AD9E565(-5, 1, 1), Pnt_$ctor_Z7AD9E565(5, 1, 1));
        const result_9 = Box__IntersectRay_4CC2E360_1(box_94, ray_9);
        Expect_isTrue(result_9 != null)("Static function should also work");
        Test_TestCaseBuilder__Zero(builder$0040_100);
    }));
})(), (() => {
    const builder$0040_101 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("intersection points can be computed from parameters", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_101, Test_TestCaseBuilder__Delay_1505(builder$0040_101, () => {
        let ln_3, ln_4, ln_5, ln_8, ln_9, ln_10;
        const box_97 = Box_$ctor_5706A3BA(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(4, 0, 0), Vec_$ctor_Z7AD9E565(0, 4, 0), Vec_$ctor_Z7AD9E565(0, 0, 4));
        const ray_12 = Line3D_$ctor_5A6659A0_1(Pnt_$ctor_Z7AD9E565(-2, 2, 2), Pnt_$ctor_Z7AD9E565(6, 2, 2));
        const result_10 = Box__IntersectRay_4CC2E360(box_97, ray_12);
        Expect_isTrue(result_10 != null)("Should intersect");
        const patternInput_2 = value_6(result_10);
        const tExit_2 = patternInput_2[1];
        const tEntry_2 = patternInput_2[0];
        let entryPt;
        let p_71;
        const ln_1 = ray_12;
        p_71 = Pnt_$ctor_Z7AD9E565_1(ln_1.FromX, ln_1.FromY, ln_1.FromZ);
        let v_266;
        let a_207;
        const ln_2 = ray_12;
        a_207 = Vec_$ctor_Z7AD9E565_1((ln_3 = ln_2, ln_3.ToX - ln_3.FromX), (ln_4 = ln_2, ln_4.ToY - ln_4.FromY), (ln_5 = ln_2, ln_5.ToZ - ln_5.FromZ));
        const f_72 = tEntry_2;
        v_266 = Vec_$ctor_Z7AD9E565_1(a_207.X * f_72, a_207.Y * f_72, a_207.Z * f_72);
        entryPt = Pnt_$ctor_Z7AD9E565_1(p_71.X + v_266.X, p_71.Y + v_266.Y, p_71.Z + v_266.Z);
        let exitPt;
        let p_72;
        const ln_6 = ray_12;
        p_72 = Pnt_$ctor_Z7AD9E565_1(ln_6.FromX, ln_6.FromY, ln_6.FromZ);
        let v_267;
        let a_208;
        const ln_7 = ray_12;
        a_208 = Vec_$ctor_Z7AD9E565_1((ln_8 = ln_7, ln_8.ToX - ln_8.FromX), (ln_9 = ln_7, ln_9.ToY - ln_9.FromY), (ln_10 = ln_7, ln_10.ToZ - ln_10.FromZ));
        const f_73 = tExit_2;
        v_267 = Vec_$ctor_Z7AD9E565_1(a_208.X * f_73, a_208.Y * f_73, a_208.Z * f_73);
        exitPt = Pnt_$ctor_Z7AD9E565_1(p_72.X + v_267.X, p_72.Y + v_267.Y, p_72.Z + v_267.Z);
        Expect_isTrue(Math.abs(entryPt.X - 0) < 1E-09)(`Entry X should be 0, got ${entryPt.X}`);
        Expect_isTrue(Math.abs(exitPt.X - 4) < 1E-09)(`Exit X should be 4, got ${exitPt.X}`);
        Test_TestCaseBuilder__Zero(builder$0040_101);
    }));
})()]))]));

