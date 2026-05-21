
import { Pnt_$ctor_Z7AD9E565 } from "./Src/Pnt.js";
import { Vec_$ctor_Z7AD9E565 } from "./Src/Vec.js";
import { Rect3D_move, Rect3D_fitToPoints, Rect3D_createFrom3Points_6180BC13, Rect3D_grid_638F50CE, Rect3D_createFromVectors_6181AF53 } from "./Src/Rect3D.js";
import { Test_TestCaseBuilder__Zero, Expect_isTrue, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Exception, int32ToString, structuralHash, assertEqual } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { equals, class_type, decimal_type, string_type, float64_type, bool_type, int32_type } from "./fable_modules/fable-library-js.5.0.0/Reflection.js";
import { contains, ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { printf, toText } from "./fable_modules/fable-library-js.5.0.0/String.js";
import { item } from "./fable_modules/fable-library-js.5.0.0/Array.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_1 } from "./Src/Pnt.js";
import { Line3D_$ctor_5A6659A0 } from "./Src/Line3D.js";
import { Euclid_Line3D__Line3D_SqDistanceRayPoint_Z394ECE4D } from "./Src/TypeExtensions/Line3D.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_1 } from "./Src/Vec.js";
import { Rect3D_$ctor_6181AF53 } from "./Src/Rect3D.js";
import { Matrix_createTranslation_Z394EC5F7, Matrix_get_identity } from "./Src/Matrix.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_2 } from "./Src/Vec.js";
import { RigidMatrix_createTranslation_Z394EC5F7 } from "./Src/RigidMatrix.js";
import { Quaternion_$ctor_77D16AC0 } from "./Src/Quaternion.js";
import { UnitVec_$ctor_Z7AD9E565 } from "./Src/UnitVec.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_2 } from "./Src/Pnt.js";

export const o = Pnt_$ctor_Z7AD9E565(0, 0, 0);

export const x = Vec_$ctor_Z7AD9E565(1, 0, 0);

export const y = Vec_$ctor_Z7AD9E565(0, 1, 0);

export const rect = Rect3D_createFromVectors_6181AF53(o, x, y);

export const tests = Test_testList("Rect3D", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rect3D.grid", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let a_2, b_2, x_3, y_3, z, a_4, b_4, x_4, y_4, z_1, a_6, b_6, x_7, y_7, z_2, a_8, b_8, x_8, y_8, z_3, a_10, b_10, x_9, y_9, z_4, a_12, b_12, x_10, y_10, z_5;
        const grid = Rect3D_grid_638F50CE(rect, 2, 2);
        const actual_1 = grid.length | 0;
        if ((actual_1 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, 2, "2-Rect3D.grid: array outer length");
        }
        else {
            let valueType;
            let copyOfStruct = actual_1;
            valueType = int32_type;
            const primitiveTypes = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg;
            if (contains(valueType, primitiveTypes, {
                Equals: equals,
                GetHashCode: (x_1) => (structuralHash(x_1) | 0),
            })) {
                const arg = int32ToString(2);
                const arg_1 = int32ToString(actual_1);
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("2-Rect3D.grid: array outer length");
            }
            else {
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_1)("2-Rect3D.grid: array outer length");
            }
            throw new Exception(errorMsg);
        }
        const actual_3 = item(0, grid).length | 0;
        if ((actual_3 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, 2, "2-Rect3D.grid: array inner length");
        }
        else {
            let valueType_1;
            let copyOfStruct_1 = actual_3;
            valueType_1 = int32_type;
            const primitiveTypes_1 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_1;
            if (contains(valueType_1, primitiveTypes_1, {
                Equals: equals,
                GetHashCode: (x_2) => (structuralHash(x_2) | 0),
            })) {
                const arg_6 = int32ToString(2);
                const arg_1_1 = int32ToString(actual_3);
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("2-Rect3D.grid: array inner length");
            }
            else {
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_3)("2-Rect3D.grid: array inner length");
            }
            throw new Exception(errorMsg_1);
        }
        Expect_isTrue(((a_2 = item(1, item(1, grid)), (b_2 = Pnt_$ctor_Z7AD9E565_1(1, 1, 0), (x_3 = (a_2.X - b_2.X), (y_3 = (a_2.Y - b_2.Y), (z = (a_2.Z - b_2.Z), Math.sqrt(((x_3 * x_3) + (y_3 * y_3)) + (z * z)))))))) < 1E-09)("2-Corner1,1");
        Expect_isTrue(((a_4 = item(0, item(0, grid)), (b_4 = Pnt_$ctor_Z7AD9E565_1(0, 0, 0), (x_4 = (a_4.X - b_4.X), (y_4 = (a_4.Y - b_4.Y), (z_1 = (a_4.Z - b_4.Z), Math.sqrt(((x_4 * x_4) + (y_4 * y_4)) + (z_1 * z_1)))))))) < 1E-09)("2-Corner0,0");
        const grid_1 = Rect3D_grid_638F50CE(rect, 3, 3);
        const actual_5 = grid_1.length | 0;
        if ((actual_5 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_5, 3, "Rect3D.grid: array outer length");
        }
        else {
            let valueType_2;
            let copyOfStruct_2 = actual_5;
            valueType_2 = int32_type;
            const primitiveTypes_2 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_2;
            if (contains(valueType_2, primitiveTypes_2, {
                Equals: equals,
                GetHashCode: (x_5) => (structuralHash(x_5) | 0),
            })) {
                const arg_7 = int32ToString(3);
                const arg_1_2 = int32ToString(actual_5);
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("Rect3D.grid: array outer length");
            }
            else {
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_5)("Rect3D.grid: array outer length");
            }
            throw new Exception(errorMsg_2);
        }
        const actual_7 = item(0, grid_1).length | 0;
        if ((actual_7 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, 3, "Rect3D.grid: array inner length");
        }
        else {
            let valueType_3;
            let copyOfStruct_3 = actual_7;
            valueType_3 = int32_type;
            const primitiveTypes_3 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_3;
            if (contains(valueType_3, primitiveTypes_3, {
                Equals: equals,
                GetHashCode: (x_6) => (structuralHash(x_6) | 0),
            })) {
                const arg_8 = int32ToString(3);
                const arg_1_3 = int32ToString(actual_7);
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("Rect3D.grid: array inner length");
            }
            else {
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_7)("Rect3D.grid: array inner length");
            }
            throw new Exception(errorMsg_3);
        }
        Expect_isTrue(((a_6 = item(0, item(0, grid_1)), (b_6 = Pnt_$ctor_Z7AD9E565_1(0, 0, 0), (x_7 = (a_6.X - b_6.X), (y_7 = (a_6.Y - b_6.Y), (z_2 = (a_6.Z - b_6.Z), Math.sqrt(((x_7 * x_7) + (y_7 * y_7)) + (z_2 * z_2)))))))) < 1E-09)("3-Corner0,0");
        Expect_isTrue(((a_8 = item(1, item(1, grid_1)), (b_8 = Pnt_$ctor_Z7AD9E565_1(0.5, 0.5, 0), (x_8 = (a_8.X - b_8.X), (y_8 = (a_8.Y - b_8.Y), (z_3 = (a_8.Z - b_8.Z), Math.sqrt(((x_8 * x_8) + (y_8 * y_8)) + (z_3 * z_3)))))))) < 1E-09)("3-Corner1,1");
        Expect_isTrue(((a_10 = item(2, item(2, grid_1)), (b_10 = Pnt_$ctor_Z7AD9E565_1(1, 1, 0), (x_9 = (a_10.X - b_10.X), (y_9 = (a_10.Y - b_10.Y), (z_4 = (a_10.Z - b_10.Z), Math.sqrt(((x_9 * x_9) + (y_9 * y_9)) + (z_4 * z_4)))))))) < 1E-09)("3-Corner2,2");
        Expect_isTrue(((a_12 = item(2, item(0, grid_1)), (b_12 = Pnt_$ctor_Z7AD9E565_1(0, 1, 0), (x_10 = (a_12.X - b_12.X), (y_10 = (a_12.Y - b_12.Y), (z_5 = (a_12.Z - b_12.Z), Math.sqrt(((x_10 * x_10) + (y_10 * y_10)) + (z_5 * z_5)))))))) < 1E-09)("3-Corner0,0");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rect3D.SizeX", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let a_13, v, b_13, ln_1, x_12, ln_2, y_12, ln_3, z_6, ln_4, a_14, v_1;
        const o_1 = Pnt_$ctor_Z7AD9E565_1(1, 2, 3);
        const x_11 = Pnt_$ctor_Z7AD9E565_1(5, 7, 8);
        const y_11 = Pnt_$ctor_Z7AD9E565_1(-8, 5, 4);
        const l = Line3D_$ctor_5A6659A0(o_1, x_11);
        let d;
        const value = Euclid_Line3D__Line3D_SqDistanceRayPoint_Z394ECE4D(l, y_11);
        d = Math.sqrt(value);
        const r = Rect3D_createFrom3Points_6180BC13(o_1, x_11, y_11);
        Expect_isTrue((a_13 = ((v = r.Xaxis, Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z)))), (b_13 = ((ln_1 = l, (x_12 = ((ln_2 = ln_1, ln_2.ToX - ln_2.FromX)), (y_12 = ((ln_3 = ln_1, ln_3.ToY - ln_3.FromY)), (z_6 = ((ln_4 = ln_1, ln_4.ToZ - ln_4.FromZ)), Math.sqrt(((x_12 * x_12) + (y_12 * y_12)) + (z_6 * z_6))))))), Math.abs(a_13 - b_13) < 1E-09)))("Rect3D.SizeX");
        Expect_isTrue((a_14 = ((v_1 = r.Yaxis, Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z)))), Math.abs(a_14 - d) < 1E-09))("Rect3D.SizeY");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rect3D.fitToPoints - all positive projections", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let a_17, v_2, a_18, v_3;
        const refRect = Rect3D_createFromVectors_6181AF53(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(1, 0, 0), Vec_$ctor_Z7AD9E565(0, 1, 0));
        const pts = [Pnt_$ctor_Z7AD9E565_1(1, 1, 0), Pnt_$ctor_Z7AD9E565_1(2, 1, 0), Pnt_$ctor_Z7AD9E565_1(2, 2, 0), Pnt_$ctor_Z7AD9E565_1(1, 2, 0)];
        const fitted = Rect3D_fitToPoints(pts, refRect);
        Expect_isTrue(Math.abs(fitted.Origin.X - 1) < 1E-09)("fitToPoints positive - origin X");
        Expect_isTrue(Math.abs(fitted.Origin.Y - 1) < 1E-09)("fitToPoints positive - origin Y");
        Expect_isTrue((a_17 = ((v_2 = fitted.Xaxis, Math.sqrt(((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z)))), Math.abs(a_17 - 1) < 1E-09))("fitToPoints positive - SizeX");
        Expect_isTrue((a_18 = ((v_3 = fitted.Yaxis, Math.sqrt(((v_3.X * v_3.X) + (v_3.Y * v_3.Y)) + (v_3.Z * v_3.Z)))), Math.abs(a_18 - 1) < 1E-09))("fitToPoints positive - SizeY");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rect3D.fitToPoints - all negative projections", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let a_21, v_4, a_22, v_5;
        const refRect_2 = Rect3D_createFromVectors_6181AF53(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(1, 0, 0), Vec_$ctor_Z7AD9E565(0, 1, 0));
        const pts_2 = [Pnt_$ctor_Z7AD9E565_1(-2, -2, 0), Pnt_$ctor_Z7AD9E565_1(-1, -2, 0), Pnt_$ctor_Z7AD9E565_1(-1, -1, 0), Pnt_$ctor_Z7AD9E565_1(-2, -1, 0)];
        const fitted_1 = Rect3D_fitToPoints(pts_2, refRect_2);
        Expect_isTrue(Math.abs(fitted_1.Origin.X - -2) < 1E-09)("fitToPoints negative - origin X");
        Expect_isTrue(Math.abs(fitted_1.Origin.Y - -2) < 1E-09)("fitToPoints negative - origin Y");
        Expect_isTrue((a_21 = ((v_4 = fitted_1.Xaxis, Math.sqrt(((v_4.X * v_4.X) + (v_4.Y * v_4.Y)) + (v_4.Z * v_4.Z)))), Math.abs(a_21 - 1) < 1E-09))("fitToPoints negative - SizeX");
        Expect_isTrue((a_22 = ((v_5 = fitted_1.Yaxis, Math.sqrt(((v_5.X * v_5.X) + (v_5.Y * v_5.Y)) + (v_5.Z * v_5.Z)))), Math.abs(a_22 - 1) < 1E-09))("fitToPoints negative - SizeY");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rect3D.fitToPoints - mixed projections", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let a_25, v_6, a_26, v_7;
        const refRect_4 = Rect3D_createFromVectors_6181AF53(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(1, 0, 0), Vec_$ctor_Z7AD9E565(0, 1, 0));
        const pts_4 = [Pnt_$ctor_Z7AD9E565_1(-1, -1, 0), Pnt_$ctor_Z7AD9E565_1(2, -1, 0), Pnt_$ctor_Z7AD9E565_1(2, 1, 0), Pnt_$ctor_Z7AD9E565_1(-1, 1, 0)];
        const fitted_2 = Rect3D_fitToPoints(pts_4, refRect_4);
        Expect_isTrue(Math.abs(fitted_2.Origin.X - -1) < 1E-09)("fitToPoints mixed - origin X");
        Expect_isTrue(Math.abs(fitted_2.Origin.Y - -1) < 1E-09)("fitToPoints mixed - origin Y");
        Expect_isTrue((a_25 = ((v_6 = fitted_2.Xaxis, Math.sqrt(((v_6.X * v_6.X) + (v_6.Y * v_6.Y)) + (v_6.Z * v_6.Z)))), Math.abs(a_25 - 3) < 1E-09))("fitToPoints mixed - SizeX");
        Expect_isTrue((a_26 = ((v_7 = fitted_2.Yaxis, Math.sqrt(((v_7.X * v_7.X) + (v_7.Y * v_7.Y)) + (v_7.Z * v_7.Z)))), Math.abs(a_26 - 2) < 1E-09))("fitToPoints mixed - SizeY");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rect3D.fitToPoints - offset reference rectangle", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let a_29, v_8, a_30, v_9;
        const refRect_6 = Rect3D_createFromVectors_6181AF53(Pnt_$ctor_Z7AD9E565_1(10, 10, 0), Vec_$ctor_Z7AD9E565(1, 0, 0), Vec_$ctor_Z7AD9E565(0, 1, 0));
        const pts_6 = [Pnt_$ctor_Z7AD9E565_1(11, 11, 0), Pnt_$ctor_Z7AD9E565_1(13, 11, 0), Pnt_$ctor_Z7AD9E565_1(13, 12, 0), Pnt_$ctor_Z7AD9E565_1(11, 12, 0)];
        const fitted_3 = Rect3D_fitToPoints(pts_6, refRect_6);
        Expect_isTrue(Math.abs(fitted_3.Origin.X - 11) < 1E-09)("fitToPoints offset - origin X");
        Expect_isTrue(Math.abs(fitted_3.Origin.Y - 11) < 1E-09)("fitToPoints offset - origin Y");
        Expect_isTrue((a_29 = ((v_8 = fitted_3.Xaxis, Math.sqrt(((v_8.X * v_8.X) + (v_8.Y * v_8.Y)) + (v_8.Z * v_8.Z)))), Math.abs(a_29 - 2) < 1E-09))("fitToPoints offset - SizeX");
        Expect_isTrue((a_30 = ((v_9 = fitted_3.Yaxis, Math.sqrt(((v_9.X * v_9.X) + (v_9.Y * v_9.Y)) + (v_9.Z * v_9.Z)))), Math.abs(a_30 - 1) < 1E-09))("fitToPoints offset - SizeY");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), Test_testList("Transformation Methods", ofArray([(() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Move instance method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        let p_1, v_11, a_32, b_32, x_14, y_14, z_7;
        const r_11 = Rect3D_createFromVectors_6181AF53(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565_1(10, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 5, 0));
        let moved;
        const r_12 = r_11;
        moved = Rect3D_$ctor_6181AF53((p_1 = r_12.Origin, (v_11 = Vec_$ctor_Z7AD9E565_1(5, 3, 2), Pnt_$ctor_Z7AD9E565(p_1.X + v_11.X, p_1.Y + v_11.Y, p_1.Z + v_11.Z))), r_12.Xaxis, r_12.Yaxis);
        Expect_isTrue(((a_32 = moved.Origin, (b_32 = Pnt_$ctor_Z7AD9E565_1(5, 3, 2), (x_14 = (a_32.X - b_32.X), (y_14 = (a_32.Y - b_32.Y), (z_7 = (a_32.Z - b_32.Z), Math.sqrt(((x_14 * x_14) + (y_14 * y_14)) + (z_7 * z_7)))))))) < 1E-09)("Move - origin");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("MoveX instance method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let a_34, b_34, x_16, y_16, z_8;
        const r_13 = Rect3D_createFromVectors_6181AF53(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565_1(10, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 5, 0));
        let moved_1;
        const r_14 = r_13;
        moved_1 = Rect3D_$ctor_6181AF53(Pnt_$ctor_Z7AD9E565(r_14.Origin.X + 5, r_14.Origin.Y, r_14.Origin.Z), r_14.Xaxis, r_14.Yaxis);
        Expect_isTrue(((a_34 = moved_1.Origin, (b_34 = Pnt_$ctor_Z7AD9E565_1(5, 0, 0), (x_16 = (a_34.X - b_34.X), (y_16 = (a_34.Y - b_34.Y), (z_8 = (a_34.Z - b_34.Z), Math.sqrt(((x_16 * x_16) + (y_16 * y_16)) + (z_8 * z_8)))))))) < 1E-09)("MoveX - origin");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("MoveY instance method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let a_36, b_36, x_18, y_18, z_9;
        const r_15 = Rect3D_createFromVectors_6181AF53(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565_1(10, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 5, 0));
        let moved_2;
        const r_16 = r_15;
        moved_2 = Rect3D_$ctor_6181AF53(Pnt_$ctor_Z7AD9E565(r_16.Origin.X, r_16.Origin.Y + 3, r_16.Origin.Z), r_16.Xaxis, r_16.Yaxis);
        Expect_isTrue(((a_36 = moved_2.Origin, (b_36 = Pnt_$ctor_Z7AD9E565_1(0, 3, 0), (x_18 = (a_36.X - b_36.X), (y_18 = (a_36.Y - b_36.Y), (z_9 = (a_36.Z - b_36.Z), Math.sqrt(((x_18 * x_18) + (y_18 * y_18)) + (z_9 * z_9)))))))) < 1E-09)("MoveY - origin");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("MoveZ instance method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        let a_38, b_38, x_20, y_20, z_10;
        const r_17 = Rect3D_createFromVectors_6181AF53(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565_1(10, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 5, 0));
        let moved_3;
        const r_18 = r_17;
        moved_3 = Rect3D_$ctor_6181AF53(Pnt_$ctor_Z7AD9E565(r_18.Origin.X, r_18.Origin.Y, r_18.Origin.Z + 2), r_18.Xaxis, r_18.Yaxis);
        Expect_isTrue(((a_38 = moved_3.Origin, (b_38 = Pnt_$ctor_Z7AD9E565_1(0, 0, 2), (x_20 = (a_38.X - b_38.X), (y_20 = (a_38.Y - b_38.Y), (z_10 = (a_38.Z - b_38.Z), Math.sqrt(((x_20 * x_20) + (y_20 * y_20)) + (z_10 * z_10)))))))) < 1E-09)("MoveZ - origin");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("move static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let a_40, b_40, x_21, y_21, z_11;
        const r_19 = Rect3D_createFromVectors_6181AF53(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565_1(10, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 5, 0));
        const moved_4 = Rect3D_move(Vec_$ctor_Z7AD9E565_1(5, 3, 2), r_19);
        Expect_isTrue(((a_40 = moved_4.Origin, (b_40 = Pnt_$ctor_Z7AD9E565_1(5, 3, 2), (x_21 = (a_40.X - b_40.X), (y_21 = (a_40.Y - b_40.Y), (z_11 = (a_40.Z - b_40.Z), Math.sqrt(((x_21 * x_21) + (y_21 * y_21)) + (z_11 * z_11)))))))) < 1E-09)("move - origin");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("moveX, moveY, moveZ static methods", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        let a_42, b_42, x_25, y_25, z_12, a_44, b_44, x_26, y_26, z_13, a_46, b_46, x_27, y_27, z_14;
        const r_21 = Rect3D_createFromVectors_6181AF53(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565_1(10, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 5, 0));
        let movedX;
        const r_23 = r_21;
        movedX = Rect3D_$ctor_6181AF53(Pnt_$ctor_Z7AD9E565(r_23.Origin.X + 5, r_23.Origin.Y, r_23.Origin.Z), r_23.Xaxis, r_23.Yaxis);
        let movedY;
        const r_25 = r_21;
        movedY = Rect3D_$ctor_6181AF53(Pnt_$ctor_Z7AD9E565(r_25.Origin.X, r_25.Origin.Y + 3, r_25.Origin.Z), r_25.Xaxis, r_25.Yaxis);
        let movedZ;
        const r_27 = r_21;
        movedZ = Rect3D_$ctor_6181AF53(Pnt_$ctor_Z7AD9E565(r_27.Origin.X, r_27.Origin.Y, r_27.Origin.Z + 2), r_27.Xaxis, r_27.Yaxis);
        Expect_isTrue(((a_42 = movedX.Origin, (b_42 = Pnt_$ctor_Z7AD9E565_1(5, 0, 0), (x_25 = (a_42.X - b_42.X), (y_25 = (a_42.Y - b_42.Y), (z_12 = (a_42.Z - b_42.Z), Math.sqrt(((x_25 * x_25) + (y_25 * y_25)) + (z_12 * z_12)))))))) < 1E-09)("moveX");
        Expect_isTrue(((a_44 = movedY.Origin, (b_44 = Pnt_$ctor_Z7AD9E565_1(0, 3, 0), (x_26 = (a_44.X - b_44.X), (y_26 = (a_44.Y - b_44.Y), (z_13 = (a_44.Z - b_44.Z), Math.sqrt(((x_26 * x_26) + (y_26 * y_26)) + (z_13 * z_13)))))))) < 1E-09)("moveY");
        Expect_isTrue(((a_46 = movedZ.Origin, (b_46 = Pnt_$ctor_Z7AD9E565_1(0, 0, 2), (x_27 = (a_46.X - b_46.X), (y_27 = (a_46.Y - b_46.Y), (z_14 = (a_46.Z - b_46.Z), Math.sqrt(((x_27 * x_27) + (y_27 * y_27)) + (z_14 * z_14)))))))) < 1E-09)("moveZ");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Transform with identity matrix", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        let a_48, b_48, x_33, y_33, z_18;
        const r_28 = Rect3D_createFromVectors_6181AF53(Pnt_$ctor_Z7AD9E565_1(1, 2, 3), Vec_$ctor_Z7AD9E565_1(10, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 5, 0));
        let transformed;
        const r_29 = r_28;
        const m = Matrix_get_identity();
        let o_2;
        const p_2 = r_29.Origin;
        const m_1 = m;
        const x_28 = p_2.X;
        const y_28 = p_2.Y;
        const z_15 = p_2.Z;
        const x$0027 = (((m_1.M11 * x_28) + (m_1.M21 * y_28)) + (m_1.M31 * z_15)) + m_1.X41;
        const y$0027 = (((m_1.M12 * x_28) + (m_1.M22 * y_28)) + (m_1.M32 * z_15)) + m_1.Y42;
        const z$0027 = (((m_1.M13 * x_28) + (m_1.M23 * y_28)) + (m_1.M33 * z_15)) + m_1.Z43;
        const w$0027 = (((m_1.M14 * x_28) + (m_1.M24 * y_28)) + (m_1.M34 * z_15)) + m_1.M44;
        const sc = 1 / w$0027;
        o_2 = Pnt_$ctor_Z7AD9E565(x$0027 * sc, y$0027 * sc, z$0027 * sc);
        let x_29;
        const v_16 = r_29.Xaxis;
        const m_4 = m;
        const x_30 = v_16.X;
        const y_29 = v_16.Y;
        const z_16 = v_16.Z;
        const x$0027_1 = ((m_4.M11 * x_30) + (m_4.M21 * y_29)) + (m_4.M31 * z_16);
        const y$0027_1 = ((m_4.M12 * x_30) + (m_4.M22 * y_29)) + (m_4.M32 * z_16);
        const z$0027_1 = ((m_4.M13 * x_30) + (m_4.M23 * y_29)) + (m_4.M33 * z_16);
        x_29 = Vec_$ctor_Z7AD9E565_2(x$0027_1, y$0027_1, z$0027_1);
        let y_30;
        const v_19 = r_29.Yaxis;
        const m_7 = m;
        const x_31 = v_19.X;
        const y_31 = v_19.Y;
        const z_17 = v_19.Z;
        const x$0027_2 = ((m_7.M11 * x_31) + (m_7.M21 * y_31)) + (m_7.M31 * z_17);
        const y$0027_2 = ((m_7.M12 * x_31) + (m_7.M22 * y_31)) + (m_7.M32 * z_17);
        const z$0027_2 = ((m_7.M13 * x_31) + (m_7.M23 * y_31)) + (m_7.M33 * z_17);
        y_30 = Vec_$ctor_Z7AD9E565_2(x$0027_2, y$0027_2, z$0027_2);
        transformed = Rect3D_$ctor_6181AF53(o_2, x_29, y_30);
        Expect_isTrue(((a_48 = transformed.Origin, (b_48 = r_28.Origin, (x_33 = (a_48.X - b_48.X), (y_33 = (a_48.Y - b_48.Y), (z_18 = (a_48.Z - b_48.Z), Math.sqrt(((x_33 * x_33) + (y_33 * y_33)) + (z_18 * z_18)))))))) < 1E-09)("Transform identity - origin");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Transform with translation matrix", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        let a_50, b_50, x_39, y_39, z_22;
        const r_30 = Rect3D_createFromVectors_6181AF53(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565_1(10, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 5, 0));
        const m_8 = Matrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565_1(5, 3, 2));
        let transformed_1;
        const r_31 = r_30;
        const m_9 = m_8;
        let o_3;
        const p_3 = r_31.Origin;
        const m_10 = m_9;
        const x_34 = p_3.X;
        const y_34 = p_3.Y;
        const z_19 = p_3.Z;
        const x$0027_3 = (((m_10.M11 * x_34) + (m_10.M21 * y_34)) + (m_10.M31 * z_19)) + m_10.X41;
        const y$0027_3 = (((m_10.M12 * x_34) + (m_10.M22 * y_34)) + (m_10.M32 * z_19)) + m_10.Y42;
        const z$0027_3 = (((m_10.M13 * x_34) + (m_10.M23 * y_34)) + (m_10.M33 * z_19)) + m_10.Z43;
        const w$0027_1 = (((m_10.M14 * x_34) + (m_10.M24 * y_34)) + (m_10.M34 * z_19)) + m_10.M44;
        const sc_1 = 1 / w$0027_1;
        o_3 = Pnt_$ctor_Z7AD9E565(x$0027_3 * sc_1, y$0027_3 * sc_1, z$0027_3 * sc_1);
        let x_35;
        const v_23 = r_31.Xaxis;
        const m_13 = m_9;
        const x_36 = v_23.X;
        const y_35 = v_23.Y;
        const z_20 = v_23.Z;
        const x$0027_4 = ((m_13.M11 * x_36) + (m_13.M21 * y_35)) + (m_13.M31 * z_20);
        const y$0027_4 = ((m_13.M12 * x_36) + (m_13.M22 * y_35)) + (m_13.M32 * z_20);
        const z$0027_4 = ((m_13.M13 * x_36) + (m_13.M23 * y_35)) + (m_13.M33 * z_20);
        x_35 = Vec_$ctor_Z7AD9E565_2(x$0027_4, y$0027_4, z$0027_4);
        let y_36;
        const v_26 = r_31.Yaxis;
        const m_16 = m_9;
        const x_37 = v_26.X;
        const y_37 = v_26.Y;
        const z_21 = v_26.Z;
        const x$0027_5 = ((m_16.M11 * x_37) + (m_16.M21 * y_37)) + (m_16.M31 * z_21);
        const y$0027_5 = ((m_16.M12 * x_37) + (m_16.M22 * y_37)) + (m_16.M32 * z_21);
        const z$0027_5 = ((m_16.M13 * x_37) + (m_16.M23 * y_37)) + (m_16.M33 * z_21);
        y_36 = Vec_$ctor_Z7AD9E565_2(x$0027_5, y$0027_5, z$0027_5);
        transformed_1 = Rect3D_$ctor_6181AF53(o_3, x_35, y_36);
        Expect_isTrue(((a_50 = transformed_1.Origin, (b_50 = Pnt_$ctor_Z7AD9E565_1(5, 3, 2), (x_39 = (a_50.X - b_50.X), (y_39 = (a_50.Y - b_50.Y), (z_22 = (a_50.Z - b_50.Z), Math.sqrt(((x_39 * x_39) + (y_39 * y_39)) + (z_22 * z_22)))))))) < 1E-09)("Transform translation - origin");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("transform static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let a_52, b_52, x_45, y_45, z_26;
        const r_32 = Rect3D_createFromVectors_6181AF53(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565_1(10, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 5, 0));
        const m_17 = Matrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565_1(5, 3, 2));
        let transformed_2;
        const m_19 = m_17;
        const r_34 = r_32;
        let o_4;
        const p_4 = r_34.Origin;
        const m_20 = m_19;
        const x_40 = p_4.X;
        const y_40 = p_4.Y;
        const z_23 = p_4.Z;
        const x$0027_6 = (((m_20.M11 * x_40) + (m_20.M21 * y_40)) + (m_20.M31 * z_23)) + m_20.X41;
        const y$0027_6 = (((m_20.M12 * x_40) + (m_20.M22 * y_40)) + (m_20.M32 * z_23)) + m_20.Y42;
        const z$0027_6 = (((m_20.M13 * x_40) + (m_20.M23 * y_40)) + (m_20.M33 * z_23)) + m_20.Z43;
        const w$0027_2 = (((m_20.M14 * x_40) + (m_20.M24 * y_40)) + (m_20.M34 * z_23)) + m_20.M44;
        const sc_2 = 1 / w$0027_2;
        o_4 = Pnt_$ctor_Z7AD9E565(x$0027_6 * sc_2, y$0027_6 * sc_2, z$0027_6 * sc_2);
        let x_41;
        const v_30 = r_34.Xaxis;
        const m_23 = m_19;
        const x_42 = v_30.X;
        const y_41 = v_30.Y;
        const z_24 = v_30.Z;
        const x$0027_7 = ((m_23.M11 * x_42) + (m_23.M21 * y_41)) + (m_23.M31 * z_24);
        const y$0027_7 = ((m_23.M12 * x_42) + (m_23.M22 * y_41)) + (m_23.M32 * z_24);
        const z$0027_7 = ((m_23.M13 * x_42) + (m_23.M23 * y_41)) + (m_23.M33 * z_24);
        x_41 = Vec_$ctor_Z7AD9E565_2(x$0027_7, y$0027_7, z$0027_7);
        let y_42;
        const v_33 = r_34.Yaxis;
        const m_26 = m_19;
        const x_43 = v_33.X;
        const y_43 = v_33.Y;
        const z_25 = v_33.Z;
        const x$0027_8 = ((m_26.M11 * x_43) + (m_26.M21 * y_43)) + (m_26.M31 * z_25);
        const y$0027_8 = ((m_26.M12 * x_43) + (m_26.M22 * y_43)) + (m_26.M32 * z_25);
        const z$0027_8 = ((m_26.M13 * x_43) + (m_26.M23 * y_43)) + (m_26.M33 * z_25);
        y_42 = Vec_$ctor_Z7AD9E565_2(x$0027_8, y$0027_8, z$0027_8);
        transformed_2 = Rect3D_$ctor_6181AF53(o_4, x_41, y_42);
        Expect_isTrue(((a_52 = transformed_2.Origin, (b_52 = Pnt_$ctor_Z7AD9E565_1(5, 3, 2), (x_45 = (a_52.X - b_52.X), (y_45 = (a_52.Y - b_52.Y), (z_26 = (a_52.Z - b_52.Z), Math.sqrt(((x_45 * x_45) + (y_45 * y_45)) + (z_26 * z_26)))))))) < 1E-09)("transform static - origin");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("TransformRigid instance method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        let a_54, b_54, x_51, y_51, z_30;
        const r_35 = Rect3D_createFromVectors_6181AF53(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565_1(10, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 5, 0));
        const m_27 = RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565_1(5, 3, 2));
        let transformed_3;
        const r_36 = r_35;
        const m_28 = m_27;
        let o_5;
        const v_34 = r_36.Origin;
        const m_29 = m_28;
        const x_46 = v_34.X;
        const y_46 = v_34.Y;
        const z_27 = v_34.Z;
        o_5 = Pnt_$ctor_Z7AD9E565((((m_29.M11 * x_46) + (m_29.M21 * y_46)) + (m_29.M31 * z_27)) + m_29.X41, (((m_29.M12 * x_46) + (m_29.M22 * y_46)) + (m_29.M32 * z_27)) + m_29.Y42, (((m_29.M13 * x_46) + (m_29.M23 * y_46)) + (m_29.M33 * z_27)) + m_29.Z43);
        let x_47;
        const v_38 = r_36.Xaxis;
        const m_32 = m_28;
        const x_48 = v_38.X;
        const y_47 = v_38.Y;
        const z_28 = v_38.Z;
        x_47 = Vec_$ctor_Z7AD9E565_2(((m_32.M11 * x_48) + (m_32.M21 * y_47)) + (m_32.M31 * z_28), ((m_32.M12 * x_48) + (m_32.M22 * y_47)) + (m_32.M32 * z_28), ((m_32.M13 * x_48) + (m_32.M23 * y_47)) + (m_32.M33 * z_28));
        let y_48;
        const v_41 = r_36.Yaxis;
        const m_35 = m_28;
        const x_49 = v_41.X;
        const y_49 = v_41.Y;
        const z_29 = v_41.Z;
        y_48 = Vec_$ctor_Z7AD9E565_2(((m_35.M11 * x_49) + (m_35.M21 * y_49)) + (m_35.M31 * z_29), ((m_35.M12 * x_49) + (m_35.M22 * y_49)) + (m_35.M32 * z_29), ((m_35.M13 * x_49) + (m_35.M23 * y_49)) + (m_35.M33 * z_29));
        transformed_3 = Rect3D_$ctor_6181AF53(o_5, x_47, y_48);
        Expect_isTrue(((a_54 = transformed_3.Origin, (b_54 = Pnt_$ctor_Z7AD9E565_1(5, 3, 2), (x_51 = (a_54.X - b_54.X), (y_51 = (a_54.Y - b_54.Y), (z_30 = (a_54.Z - b_54.Z), Math.sqrt(((x_51 * x_51) + (y_51 * y_51)) + (z_30 * z_30)))))))) < 1E-09)("TransformRigid - origin");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("transformRigid static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let a_56, b_56, x_57, y_57, z_34;
        const r_37 = Rect3D_createFromVectors_6181AF53(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565_1(10, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 5, 0));
        const m_36 = RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565_1(5, 3, 2));
        let transformed_4;
        const m_38 = m_36;
        const r_39 = r_37;
        let o_6;
        const v_42 = r_39.Origin;
        const m_41 = m_38;
        const x_52 = v_42.X;
        const y_52 = v_42.Y;
        const z_31 = v_42.Z;
        o_6 = Pnt_$ctor_Z7AD9E565((((m_41.M11 * x_52) + (m_41.M21 * y_52)) + (m_41.M31 * z_31)) + m_41.X41, (((m_41.M12 * x_52) + (m_41.M22 * y_52)) + (m_41.M32 * z_31)) + m_41.Y42, (((m_41.M13 * x_52) + (m_41.M23 * y_52)) + (m_41.M33 * z_31)) + m_41.Z43);
        let x_53;
        const v_46 = r_39.Xaxis;
        const m_44 = m_38;
        const x_54 = v_46.X;
        const y_53 = v_46.Y;
        const z_32 = v_46.Z;
        x_53 = Vec_$ctor_Z7AD9E565_2(((m_44.M11 * x_54) + (m_44.M21 * y_53)) + (m_44.M31 * z_32), ((m_44.M12 * x_54) + (m_44.M22 * y_53)) + (m_44.M32 * z_32), ((m_44.M13 * x_54) + (m_44.M23 * y_53)) + (m_44.M33 * z_32));
        let y_54;
        const v_49 = r_39.Yaxis;
        const m_47 = m_38;
        const x_55 = v_49.X;
        const y_55 = v_49.Y;
        const z_33 = v_49.Z;
        y_54 = Vec_$ctor_Z7AD9E565_2(((m_47.M11 * x_55) + (m_47.M21 * y_55)) + (m_47.M31 * z_33), ((m_47.M12 * x_55) + (m_47.M22 * y_55)) + (m_47.M32 * z_33), ((m_47.M13 * x_55) + (m_47.M23 * y_55)) + (m_47.M33 * z_33));
        transformed_4 = Rect3D_$ctor_6181AF53(o_6, x_53, y_54);
        Expect_isTrue(((a_56 = transformed_4.Origin, (b_56 = Pnt_$ctor_Z7AD9E565_1(5, 3, 2), (x_57 = (a_56.X - b_56.X), (y_57 = (a_56.Y - b_56.Y), (z_34 = (a_56.Z - b_56.Z), Math.sqrt(((x_57 * x_57) + (y_57 * y_57)) + (z_34 * z_34)))))))) < 1E-09)("transformRigid static - origin");
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rotate with identity quaternion", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        let a_58, b_58, x_64, y_64, z_39;
        const r_40 = Rect3D_createFromVectors_6181AF53(Pnt_$ctor_Z7AD9E565_1(1, 2, 3), Vec_$ctor_Z7AD9E565_1(10, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 5, 0));
        let rotated;
        const r_41 = r_40;
        const q = Quaternion_$ctor_77D16AC0(0, 0, 0, 1);
        let o_7;
        const p_8 = r_41.Origin;
        const q_1 = q;
        const x_59 = p_8.X;
        const y_59 = p_8.Y;
        const z_36 = p_8.Z;
        const qx = q_1.X;
        const qy = q_1.Y;
        const qz = q_1.Z;
        const qw = q_1.W;
        const tx = 2 * ((qy * z_36) - (qz * y_59));
        const ty = 2 * ((qz * x_59) - (qx * z_36));
        const tz = 2 * ((qx * y_59) - (qy * x_59));
        o_7 = Pnt_$ctor_Z7AD9E565(((x_59 + (qw * tx)) + (qy * tz)) - (qz * ty), ((y_59 + (qw * ty)) + (qz * tx)) - (qx * tz), ((z_36 + (qw * tz)) + (qx * ty)) - (qy * tx));
        let x_60;
        const v_50 = r_41.Xaxis;
        const q_2 = q;
        const x_61 = v_50.X;
        const y_60 = v_50.Y;
        const z_37 = v_50.Z;
        const qx_1 = q_2.X;
        const qy_1 = q_2.Y;
        const qz_1 = q_2.Z;
        const qw_1 = q_2.W;
        const tx_1 = 2 * ((qy_1 * z_37) - (qz_1 * y_60));
        const ty_1 = 2 * ((qz_1 * x_61) - (qx_1 * z_37));
        const tz_1 = 2 * ((qx_1 * y_60) - (qy_1 * x_61));
        x_60 = Vec_$ctor_Z7AD9E565_2(((x_61 + (qw_1 * tx_1)) + (qy_1 * tz_1)) - (qz_1 * ty_1), ((y_60 + (qw_1 * ty_1)) + (qz_1 * tx_1)) - (qx_1 * tz_1), ((z_37 + (qw_1 * tz_1)) + (qx_1 * ty_1)) - (qy_1 * tx_1));
        let y_61;
        const v_51 = r_41.Yaxis;
        const q_3 = q;
        const x_62 = v_51.X;
        const y_62 = v_51.Y;
        const z_38 = v_51.Z;
        const qx_2 = q_3.X;
        const qy_2 = q_3.Y;
        const qz_2 = q_3.Z;
        const qw_2 = q_3.W;
        const tx_2 = 2 * ((qy_2 * z_38) - (qz_2 * y_62));
        const ty_2 = 2 * ((qz_2 * x_62) - (qx_2 * z_38));
        const tz_2 = 2 * ((qx_2 * y_62) - (qy_2 * x_62));
        y_61 = Vec_$ctor_Z7AD9E565_2(((x_62 + (qw_2 * tx_2)) + (qy_2 * tz_2)) - (qz_2 * ty_2), ((y_62 + (qw_2 * ty_2)) + (qz_2 * tx_2)) - (qx_2 * tz_2), ((z_38 + (qw_2 * tz_2)) + (qx_2 * ty_2)) - (qy_2 * tx_2));
        rotated = Rect3D_$ctor_6181AF53(o_7, x_60, y_61);
        Expect_isTrue(((a_58 = rotated.Origin, (b_58 = r_40.Origin, (x_64 = (a_58.X - b_58.X), (y_64 = (a_58.Y - b_58.Y), (z_39 = (a_58.Z - b_58.Z), Math.sqrt(((x_64 * x_64) + (y_64 * y_64)) + (z_39 * z_39)))))))) < 1E-09)("Rotate identity - origin");
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rotate 90 degrees around Z axis", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        let a_60, b_60, x_72, y_72, z_45;
        const r_42 = Rect3D_createFromVectors_6181AF53(Pnt_$ctor_Z7AD9E565_1(1, 0, 0), Vec_$ctor_Z7AD9E565_1(1, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 1, 0));
        let q_4;
        const axis_1 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
        const angHalf = (0.017453292519943295 * 90) * 0.5;
        const sa = Math.sin(angHalf);
        q_4 = Quaternion_$ctor_77D16AC0(axis_1.X * sa, axis_1.Y * sa, axis_1.Z * sa, Math.cos(angHalf));
        let rotated_1;
        const r_43 = r_42;
        const q_5 = q_4;
        let o_8;
        const p_9 = r_43.Origin;
        const q_6 = q_5;
        const x_67 = p_9.X;
        const y_67 = p_9.Y;
        const z_42 = p_9.Z;
        const qx_3 = q_6.X;
        const qy_3 = q_6.Y;
        const qz_3 = q_6.Z;
        const qw_3 = q_6.W;
        const tx_3 = 2 * ((qy_3 * z_42) - (qz_3 * y_67));
        const ty_3 = 2 * ((qz_3 * x_67) - (qx_3 * z_42));
        const tz_3 = 2 * ((qx_3 * y_67) - (qy_3 * x_67));
        o_8 = Pnt_$ctor_Z7AD9E565(((x_67 + (qw_3 * tx_3)) + (qy_3 * tz_3)) - (qz_3 * ty_3), ((y_67 + (qw_3 * ty_3)) + (qz_3 * tx_3)) - (qx_3 * tz_3), ((z_42 + (qw_3 * tz_3)) + (qx_3 * ty_3)) - (qy_3 * tx_3));
        let x_68;
        const v_52 = r_43.Xaxis;
        const q_7 = q_5;
        const x_69 = v_52.X;
        const y_68 = v_52.Y;
        const z_43 = v_52.Z;
        const qx_4 = q_7.X;
        const qy_4 = q_7.Y;
        const qz_4 = q_7.Z;
        const qw_4 = q_7.W;
        const tx_4 = 2 * ((qy_4 * z_43) - (qz_4 * y_68));
        const ty_4 = 2 * ((qz_4 * x_69) - (qx_4 * z_43));
        const tz_4 = 2 * ((qx_4 * y_68) - (qy_4 * x_69));
        x_68 = Vec_$ctor_Z7AD9E565_2(((x_69 + (qw_4 * tx_4)) + (qy_4 * tz_4)) - (qz_4 * ty_4), ((y_68 + (qw_4 * ty_4)) + (qz_4 * tx_4)) - (qx_4 * tz_4), ((z_43 + (qw_4 * tz_4)) + (qx_4 * ty_4)) - (qy_4 * tx_4));
        let y_69;
        const v_53 = r_43.Yaxis;
        const q_8 = q_5;
        const x_70 = v_53.X;
        const y_70 = v_53.Y;
        const z_44 = v_53.Z;
        const qx_5 = q_8.X;
        const qy_5 = q_8.Y;
        const qz_5 = q_8.Z;
        const qw_5 = q_8.W;
        const tx_5 = 2 * ((qy_5 * z_44) - (qz_5 * y_70));
        const ty_5 = 2 * ((qz_5 * x_70) - (qx_5 * z_44));
        const tz_5 = 2 * ((qx_5 * y_70) - (qy_5 * x_70));
        y_69 = Vec_$ctor_Z7AD9E565_2(((x_70 + (qw_5 * tx_5)) + (qy_5 * tz_5)) - (qz_5 * ty_5), ((y_70 + (qw_5 * ty_5)) + (qz_5 * tx_5)) - (qx_5 * tz_5), ((z_44 + (qw_5 * tz_5)) + (qx_5 * ty_5)) - (qy_5 * tx_5));
        rotated_1 = Rect3D_$ctor_6181AF53(o_8, x_68, y_69);
        Expect_isTrue(((a_60 = rotated_1.Origin, (b_60 = Pnt_$ctor_Z7AD9E565_1(0, 1, 0), (x_72 = (a_60.X - b_60.X), (y_72 = (a_60.Y - b_60.Y), (z_45 = (a_60.Z - b_60.Z), Math.sqrt(((x_72 * x_72) + (y_72 * y_72)) + (z_45 * z_45)))))))) < 1E-09)("Rotate 90 - origin");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("rotate static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        let a_62, b_62, x_80, y_80, z_51;
        const r_44 = Rect3D_createFromVectors_6181AF53(Pnt_$ctor_Z7AD9E565_1(1, 0, 0), Vec_$ctor_Z7AD9E565_1(1, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 1, 0));
        let q_9;
        const axis_3 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
        const angHalf_1 = (0.017453292519943295 * 90) * 0.5;
        const sa_1 = Math.sin(angHalf_1);
        q_9 = Quaternion_$ctor_77D16AC0(axis_3.X * sa_1, axis_3.Y * sa_1, axis_3.Z * sa_1, Math.cos(angHalf_1));
        let rotated_2;
        const q_11 = q_9;
        const r_46 = r_44;
        let o_9;
        const p_10 = r_46.Origin;
        const q_12 = q_11;
        const x_75 = p_10.X;
        const y_75 = p_10.Y;
        const z_48 = p_10.Z;
        const qx_6 = q_12.X;
        const qy_6 = q_12.Y;
        const qz_6 = q_12.Z;
        const qw_6 = q_12.W;
        const tx_6 = 2 * ((qy_6 * z_48) - (qz_6 * y_75));
        const ty_6 = 2 * ((qz_6 * x_75) - (qx_6 * z_48));
        const tz_6 = 2 * ((qx_6 * y_75) - (qy_6 * x_75));
        o_9 = Pnt_$ctor_Z7AD9E565(((x_75 + (qw_6 * tx_6)) + (qy_6 * tz_6)) - (qz_6 * ty_6), ((y_75 + (qw_6 * ty_6)) + (qz_6 * tx_6)) - (qx_6 * tz_6), ((z_48 + (qw_6 * tz_6)) + (qx_6 * ty_6)) - (qy_6 * tx_6));
        let x_76;
        const v_54 = r_46.Xaxis;
        const q_13 = q_11;
        const x_77 = v_54.X;
        const y_76 = v_54.Y;
        const z_49 = v_54.Z;
        const qx_7 = q_13.X;
        const qy_7 = q_13.Y;
        const qz_7 = q_13.Z;
        const qw_7 = q_13.W;
        const tx_7 = 2 * ((qy_7 * z_49) - (qz_7 * y_76));
        const ty_7 = 2 * ((qz_7 * x_77) - (qx_7 * z_49));
        const tz_7 = 2 * ((qx_7 * y_76) - (qy_7 * x_77));
        x_76 = Vec_$ctor_Z7AD9E565_2(((x_77 + (qw_7 * tx_7)) + (qy_7 * tz_7)) - (qz_7 * ty_7), ((y_76 + (qw_7 * ty_7)) + (qz_7 * tx_7)) - (qx_7 * tz_7), ((z_49 + (qw_7 * tz_7)) + (qx_7 * ty_7)) - (qy_7 * tx_7));
        let y_77;
        const v_55 = r_46.Yaxis;
        const q_14 = q_11;
        const x_78 = v_55.X;
        const y_78 = v_55.Y;
        const z_50 = v_55.Z;
        const qx_8 = q_14.X;
        const qy_8 = q_14.Y;
        const qz_8 = q_14.Z;
        const qw_8 = q_14.W;
        const tx_8 = 2 * ((qy_8 * z_50) - (qz_8 * y_78));
        const ty_8 = 2 * ((qz_8 * x_78) - (qx_8 * z_50));
        const tz_8 = 2 * ((qx_8 * y_78) - (qy_8 * x_78));
        y_77 = Vec_$ctor_Z7AD9E565_2(((x_78 + (qw_8 * tx_8)) + (qy_8 * tz_8)) - (qz_8 * ty_8), ((y_78 + (qw_8 * ty_8)) + (qz_8 * tx_8)) - (qx_8 * tz_8), ((z_50 + (qw_8 * tz_8)) + (qx_8 * ty_8)) - (qy_8 * tx_8));
        rotated_2 = Rect3D_$ctor_6181AF53(o_9, x_76, y_77);
        Expect_isTrue(((a_62 = rotated_2.Origin, (b_62 = Pnt_$ctor_Z7AD9E565_1(0, 1, 0), (x_80 = (a_62.X - b_62.X), (y_80 = (a_62.Y - b_62.Y), (z_51 = (a_62.Z - b_62.Z), Math.sqrt(((x_80 * x_80) + (y_80 * y_80)) + (z_51 * z_51)))))))) < 1E-09)("rotate static - origin");
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RotateWithCenter keeps center fixed", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        let a_68, r_50, p_14, p_13, v_60, a_65, v_61, a_66, b_64, x_88, y_88, z_57;
        const r_47 = Rect3D_createFromVectors_6181AF53(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565_1(2, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 2, 0));
        let center;
        const r_48 = r_47;
        let p_12;
        const p_11 = r_48.Origin;
        let v_56;
        const a_63 = r_48.Xaxis;
        v_56 = Vec_$ctor_Z7AD9E565_2(a_63.X * 0.5, a_63.Y * 0.5, a_63.Z * 0.5);
        p_12 = Pnt_$ctor_Z7AD9E565(p_11.X + v_56.X, p_11.Y + v_56.Y, p_11.Z + v_56.Z);
        let v_57;
        const a_64 = r_48.Yaxis;
        v_57 = Vec_$ctor_Z7AD9E565_2(a_64.X * 0.5, a_64.Y * 0.5, a_64.Z * 0.5);
        center = Pnt_$ctor_Z7AD9E565(p_12.X + v_57.X, p_12.Y + v_57.Y, p_12.Z + v_57.Z);
        let q_15;
        const axis_5 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
        const angHalf_2 = (0.017453292519943295 * 90) * 0.5;
        const sa_2 = Math.sin(angHalf_2);
        q_15 = Quaternion_$ctor_77D16AC0(axis_5.X * sa_2, axis_5.Y * sa_2, axis_5.Z * sa_2, Math.cos(angHalf_2));
        let rotated_3;
        const r_49 = r_47;
        const q_16 = q_15;
        let o_10;
        const cen_2 = center;
        const q_17 = q_16;
        const pt_1 = r_49.Origin;
        const x_83 = pt_1.X - cen_2.X;
        const y_83 = pt_1.Y - cen_2.Y;
        const z_54 = pt_1.Z - cen_2.Z;
        const qx_9 = q_17.X;
        const qy_9 = q_17.Y;
        const qz_9 = q_17.Z;
        const qw_9 = q_17.W;
        const ix = ((qw_9 * x_83) + (qy_9 * z_54)) - (qz_9 * y_83);
        const iy = ((qw_9 * y_83) + (qz_9 * x_83)) - (qx_9 * z_54);
        const iz = ((qw_9 * z_54) + (qx_9 * y_83)) - (qy_9 * x_83);
        const iw = ((-qx_9 * x_83) - (qy_9 * y_83)) - (qz_9 * z_54);
        o_10 = Pnt_$ctor_Z7AD9E565_2(((((ix * qw_9) + (iw * -qx_9)) + (iy * -qz_9)) - (iz * -qy_9)) + cen_2.X, ((((iy * qw_9) + (iw * -qy_9)) + (iz * -qx_9)) - (ix * -qz_9)) + cen_2.Y, ((((iz * qw_9) + (iw * -qz_9)) + (ix * -qy_9)) - (iy * -qx_9)) + cen_2.Z);
        let x_84;
        const v_58 = r_49.Xaxis;
        const q_18 = q_16;
        const x_85 = v_58.X;
        const y_84 = v_58.Y;
        const z_55 = v_58.Z;
        const qx_10 = q_18.X;
        const qy_10 = q_18.Y;
        const qz_10 = q_18.Z;
        const qw_10 = q_18.W;
        const tx_9 = 2 * ((qy_10 * z_55) - (qz_10 * y_84));
        const ty_9 = 2 * ((qz_10 * x_85) - (qx_10 * z_55));
        const tz_9 = 2 * ((qx_10 * y_84) - (qy_10 * x_85));
        x_84 = Vec_$ctor_Z7AD9E565_2(((x_85 + (qw_10 * tx_9)) + (qy_10 * tz_9)) - (qz_10 * ty_9), ((y_84 + (qw_10 * ty_9)) + (qz_10 * tx_9)) - (qx_10 * tz_9), ((z_55 + (qw_10 * tz_9)) + (qx_10 * ty_9)) - (qy_10 * tx_9));
        let y_85;
        const v_59 = r_49.Yaxis;
        const q_19 = q_16;
        const x_86 = v_59.X;
        const y_86 = v_59.Y;
        const z_56 = v_59.Z;
        const qx_11 = q_19.X;
        const qy_11 = q_19.Y;
        const qz_11 = q_19.Z;
        const qw_11 = q_19.W;
        const tx_10 = 2 * ((qy_11 * z_56) - (qz_11 * y_86));
        const ty_10 = 2 * ((qz_11 * x_86) - (qx_11 * z_56));
        const tz_10 = 2 * ((qx_11 * y_86) - (qy_11 * x_86));
        y_85 = Vec_$ctor_Z7AD9E565_2(((x_86 + (qw_11 * tx_10)) + (qy_11 * tz_10)) - (qz_11 * ty_10), ((y_86 + (qw_11 * ty_10)) + (qz_11 * tx_10)) - (qx_11 * tz_10), ((z_56 + (qw_11 * tz_10)) + (qx_11 * ty_10)) - (qy_11 * tx_10));
        rotated_3 = Rect3D_$ctor_6181AF53(o_10, x_84, y_85);
        Expect_isTrue(((a_68 = ((r_50 = rotated_3, (p_14 = ((p_13 = r_50.Origin, (v_60 = ((a_65 = r_50.Xaxis, Vec_$ctor_Z7AD9E565_2(a_65.X * 0.5, a_65.Y * 0.5, a_65.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p_13.X + v_60.X, p_13.Y + v_60.Y, p_13.Z + v_60.Z)))), (v_61 = ((a_66 = r_50.Yaxis, Vec_$ctor_Z7AD9E565_2(a_66.X * 0.5, a_66.Y * 0.5, a_66.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p_14.X + v_61.X, p_14.Y + v_61.Y, p_14.Z + v_61.Z))))), (b_64 = center, (x_88 = (a_68.X - b_64.X), (y_88 = (a_68.Y - b_64.Y), (z_57 = (a_68.Z - b_64.Z), Math.sqrt(((x_88 * x_88) + (y_88 * y_88)) + (z_57 * z_57)))))))) < 1E-09)("RotateWithCenter - center");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("rotateWithCenter static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        let a_74, r_55, p_18, p_17, v_66, a_71, v_67, a_72, b_66, x_96, y_96, z_63;
        const r_51 = Rect3D_createFromVectors_6181AF53(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565_1(2, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 2, 0));
        let center_1;
        const r_52 = r_51;
        let p_16;
        const p_15 = r_52.Origin;
        let v_62;
        const a_69 = r_52.Xaxis;
        v_62 = Vec_$ctor_Z7AD9E565_2(a_69.X * 0.5, a_69.Y * 0.5, a_69.Z * 0.5);
        p_16 = Pnt_$ctor_Z7AD9E565(p_15.X + v_62.X, p_15.Y + v_62.Y, p_15.Z + v_62.Z);
        let v_63;
        const a_70 = r_52.Yaxis;
        v_63 = Vec_$ctor_Z7AD9E565_2(a_70.X * 0.5, a_70.Y * 0.5, a_70.Z * 0.5);
        center_1 = Pnt_$ctor_Z7AD9E565(p_16.X + v_63.X, p_16.Y + v_63.Y, p_16.Z + v_63.Z);
        let q_20;
        const axis_7 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
        const angHalf_3 = (0.017453292519943295 * 90) * 0.5;
        const sa_3 = Math.sin(angHalf_3);
        q_20 = Quaternion_$ctor_77D16AC0(axis_7.X * sa_3, axis_7.Y * sa_3, axis_7.Z * sa_3, Math.cos(angHalf_3));
        let rotated_4;
        const q_22 = q_20;
        const r_54 = r_51;
        let o_11;
        const cen_5 = center_1;
        const q_23 = q_22;
        const pt_3 = r_54.Origin;
        const x_91 = pt_3.X - cen_5.X;
        const y_91 = pt_3.Y - cen_5.Y;
        const z_60 = pt_3.Z - cen_5.Z;
        const qx_12 = q_23.X;
        const qy_12 = q_23.Y;
        const qz_12 = q_23.Z;
        const qw_12 = q_23.W;
        const ix_1 = ((qw_12 * x_91) + (qy_12 * z_60)) - (qz_12 * y_91);
        const iy_1 = ((qw_12 * y_91) + (qz_12 * x_91)) - (qx_12 * z_60);
        const iz_1 = ((qw_12 * z_60) + (qx_12 * y_91)) - (qy_12 * x_91);
        const iw_1 = ((-qx_12 * x_91) - (qy_12 * y_91)) - (qz_12 * z_60);
        o_11 = Pnt_$ctor_Z7AD9E565_2(((((ix_1 * qw_12) + (iw_1 * -qx_12)) + (iy_1 * -qz_12)) - (iz_1 * -qy_12)) + cen_5.X, ((((iy_1 * qw_12) + (iw_1 * -qy_12)) + (iz_1 * -qx_12)) - (ix_1 * -qz_12)) + cen_5.Y, ((((iz_1 * qw_12) + (iw_1 * -qz_12)) + (ix_1 * -qy_12)) - (iy_1 * -qx_12)) + cen_5.Z);
        let x_92;
        const v_64 = r_54.Xaxis;
        const q_24 = q_22;
        const x_93 = v_64.X;
        const y_92 = v_64.Y;
        const z_61 = v_64.Z;
        const qx_13 = q_24.X;
        const qy_13 = q_24.Y;
        const qz_13 = q_24.Z;
        const qw_13 = q_24.W;
        const tx_11 = 2 * ((qy_13 * z_61) - (qz_13 * y_92));
        const ty_11 = 2 * ((qz_13 * x_93) - (qx_13 * z_61));
        const tz_11 = 2 * ((qx_13 * y_92) - (qy_13 * x_93));
        x_92 = Vec_$ctor_Z7AD9E565_2(((x_93 + (qw_13 * tx_11)) + (qy_13 * tz_11)) - (qz_13 * ty_11), ((y_92 + (qw_13 * ty_11)) + (qz_13 * tx_11)) - (qx_13 * tz_11), ((z_61 + (qw_13 * tz_11)) + (qx_13 * ty_11)) - (qy_13 * tx_11));
        let y_93;
        const v_65 = r_54.Yaxis;
        const q_25 = q_22;
        const x_94 = v_65.X;
        const y_94 = v_65.Y;
        const z_62 = v_65.Z;
        const qx_14 = q_25.X;
        const qy_14 = q_25.Y;
        const qz_14 = q_25.Z;
        const qw_14 = q_25.W;
        const tx_12 = 2 * ((qy_14 * z_62) - (qz_14 * y_94));
        const ty_12 = 2 * ((qz_14 * x_94) - (qx_14 * z_62));
        const tz_12 = 2 * ((qx_14 * y_94) - (qy_14 * x_94));
        y_93 = Vec_$ctor_Z7AD9E565_2(((x_94 + (qw_14 * tx_12)) + (qy_14 * tz_12)) - (qz_14 * ty_12), ((y_94 + (qw_14 * ty_12)) + (qz_14 * tx_12)) - (qx_14 * tz_12), ((z_62 + (qw_14 * tz_12)) + (qx_14 * ty_12)) - (qy_14 * tx_12));
        rotated_4 = Rect3D_$ctor_6181AF53(o_11, x_92, y_93);
        Expect_isTrue(((a_74 = ((r_55 = rotated_4, (p_18 = ((p_17 = r_55.Origin, (v_66 = ((a_71 = r_55.Xaxis, Vec_$ctor_Z7AD9E565_2(a_71.X * 0.5, a_71.Y * 0.5, a_71.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p_17.X + v_66.X, p_17.Y + v_66.Y, p_17.Z + v_66.Z)))), (v_67 = ((a_72 = r_55.Yaxis, Vec_$ctor_Z7AD9E565_2(a_72.X * 0.5, a_72.Y * 0.5, a_72.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p_18.X + v_67.X, p_18.Y + v_67.Y, p_18.Z + v_67.Z))))), (b_66 = center_1, (x_96 = (a_74.X - b_66.X), (y_96 = (a_74.Y - b_66.Y), (z_63 = (a_74.Z - b_66.Z), Math.sqrt(((x_96 * x_96) + (y_96 * y_96)) + (z_63 * z_63)))))))) < 1E-09)("rotateWithCenter static - center");
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})()]))]));

