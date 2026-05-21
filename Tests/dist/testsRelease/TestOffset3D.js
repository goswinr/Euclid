
import { Expect_isFalse, Expect_floatClose, Expect_throws, Expect_isTrue, Test_testCase, Test_testList, AccuracyModule_veryHigh } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { StringBuilder__AppendLine_Z721C83C5, StringBuilder_$ctor } from "./fable_modules/fable-library-js.5.0.0/System.Text.js";
import { printf, toText, concat } from "./fable_modules/fable-library-js.5.0.0/String.js";
import { Polyline3D_offset_1FF60F20, Polyline3D__get_Points, Polyline3D__get_PointCount, Polyline3D_offsetWithRef_39D084EF, Polyline3D_$ctor_516DFD0A, Polyline3D__get_AsFSharpCode } from "./Src/Polyline3D.js";
import { Operators_Lock } from "./fable_modules/fable-library-js.5.0.0/FSharp.Core.js";
import { toString } from "./fable_modules/fable-library-js.5.0.0/Types.js";
import { Pnt_$ctor_Z7AD9E565 } from "./Src/Pnt.js";
import { UnitVec_$ctor_Z7AD9E565 } from "./Src/UnitVec.js";
import { int32ToString, structuralHash, Exception, assertEqual } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { sum, ofArray, contains } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { equals, class_type, decimal_type, string_type, float64_type, bool_type, int32_type } from "./fable_modules/fable-library-js.5.0.0/Reflection.js";
import { item } from "./fable_modules/fable-library-js.5.0.0/Array.js";
import { map, singleton, collect, delay, toList } from "./fable_modules/fable-library-js.5.0.0/Seq.js";
import { rangeDouble } from "./fable_modules/fable-library-js.5.0.0/Range.js";
import { failUnit3 } from "./Src/EuclidErrors.js";

export const tol = AccuracyModule_veryHigh;

export const saveTestData = false;

export const testDataFilePath = ".NET only";

export const testDataCollector = [];

/**
 * Records a test case with input polyline, parameters, and output polyline
 */
export function recordTestCase(testName, input, inPlane, perp, refNormal, output) {
    if (saveTestData) {
        const sb = StringBuilder_$ctor();
        StringBuilder__AppendLine_Z721C83C5(sb, concat("    { Name = \"", testName, ..."\""));
        StringBuilder__AppendLine_Z721C83C5(sb, concat("      Input = ", ...Polyline3D__get_AsFSharpCode(input)));
        StringBuilder__AppendLine_Z721C83C5(sb, `      RefNormal = UnitVec.create(${refNormal.X}, ${refNormal.Y}, ${refNormal.Z})`);
        StringBuilder__AppendLine_Z721C83C5(sb, `      InPlane = ${inPlane}`);
        StringBuilder__AppendLine_Z721C83C5(sb, `      Perp = ${perp}`);
        StringBuilder__AppendLine_Z721C83C5(sb, concat("      Output = ", Polyline3D__get_AsFSharpCode(output), ..." }"));
        Operators_Lock(testDataCollector, () => {
            void (testDataCollector.push(toString(sb)));
        });
    }
}

/**
 * Writes all collected test data to the F# file
 */
export function writeTestDataFile() {
}

export const tests = Test_testList("Offset3D ", ofArray([Test_testCase("Polyline3D.offset open L-shape in XY plane with Z-up normal", () => {
    let copyOfStruct, arg, arg_1, a_2, b_2, x_2, y_2, z_1, a_4, b_4, x_3, y_3, z_2, a_6, b_6, x_4, y_4, z_3;
    const pl = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0)]);
    const refNormal = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const result = Polyline3D_offsetWithRef_39D084EF(pl, 1, 0, refNormal, false);
    recordTestCase("open L-shape in XY plane", pl, 1, 0, refNormal, result);
    const actual = Polyline3D__get_PointCount(result) | 0;
    if ((actual === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual, 3, "Should have 3 points");
    }
    else {
        throw new Exception(contains((copyOfStruct = actual, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_1) => (structuralHash(x_1) | 0),
        }) ? ((arg = int32ToString(3), (arg_1 = int32ToString(actual), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("Should have 3 points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual)("Should have 3 points"));
    }
    Expect_isTrue(((a_2 = item(0, Polyline3D__get_Points(result)), (b_2 = Pnt_$ctor_Z7AD9E565(0, 1, 0), (x_2 = (a_2.X - b_2.X), (y_2 = (a_2.Y - b_2.Y), (z_1 = (a_2.Z - b_2.Z), Math.sqrt(((x_2 * x_2) + (y_2 * y_2)) + (z_1 * z_1)))))))) < 1E-06)(`Start point should be Pnt(0, 1, 0), got ${item(0, Polyline3D__get_Points(result))}`);
    Expect_isTrue(((a_4 = item(1, Polyline3D__get_Points(result)), (b_4 = Pnt_$ctor_Z7AD9E565(9, 1, 0), (x_3 = (a_4.X - b_4.X), (y_3 = (a_4.Y - b_4.Y), (z_2 = (a_4.Z - b_4.Z), Math.sqrt(((x_3 * x_3) + (y_3 * y_3)) + (z_2 * z_2)))))))) < 1E-06)(`Corner should be Pnt(9, 1, 0), got ${item(1, Polyline3D__get_Points(result))}`);
    Expect_isTrue(((a_6 = item(2, Polyline3D__get_Points(result)), (b_6 = Pnt_$ctor_Z7AD9E565(9, 10, 0), (x_4 = (a_6.X - b_6.X), (y_4 = (a_6.Y - b_6.Y), (z_3 = (a_6.Z - b_6.Z), Math.sqrt(((x_4 * x_4) + (y_4 * y_4)) + (z_3 * z_3)))))))) < 1E-06)(`End point should be Pnt(9, 10, 0), got ${item(2, Polyline3D__get_Points(result))}`);
}), Test_testCase("Polyline3D.offset open L-shape with perpendicular offset", () => {
    let copyOfStruct_1, arg_6, arg_1_1, a_8, b_8, x_7, y_7, z_5, a_10, b_10, x_8, y_8, z_6, a_12, b_12, x_9, y_9, z_7;
    const pl_1 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0)]);
    const refNormal_1 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const result_1 = Polyline3D_offsetWithRef_39D084EF(pl_1, 0, 2, refNormal_1, false);
    recordTestCase("open L-shape with perpendicular offset", pl_1, 0, 2, refNormal_1, result_1);
    const actual_1 = Polyline3D__get_PointCount(result_1) | 0;
    if ((actual_1 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_1, 3, "Should have 3 points");
    }
    else {
        throw new Exception(contains((copyOfStruct_1 = actual_1, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_6) => (structuralHash(x_6) | 0),
        }) ? ((arg_6 = int32ToString(3), (arg_1_1 = int32ToString(actual_1), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("Should have 3 points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_1)("Should have 3 points"));
    }
    Expect_isTrue(((a_8 = item(0, Polyline3D__get_Points(result_1)), (b_8 = Pnt_$ctor_Z7AD9E565(0, 0, 2), (x_7 = (a_8.X - b_8.X), (y_7 = (a_8.Y - b_8.Y), (z_5 = (a_8.Z - b_8.Z), Math.sqrt(((x_7 * x_7) + (y_7 * y_7)) + (z_5 * z_5)))))))) < 1E-06)(`Start point should be at Z=2, got ${item(0, Polyline3D__get_Points(result_1))}`);
    Expect_isTrue(((a_10 = item(1, Polyline3D__get_Points(result_1)), (b_10 = Pnt_$ctor_Z7AD9E565(10, 0, 2), (x_8 = (a_10.X - b_10.X), (y_8 = (a_10.Y - b_10.Y), (z_6 = (a_10.Z - b_10.Z), Math.sqrt(((x_8 * x_8) + (y_8 * y_8)) + (z_6 * z_6)))))))) < 1E-06)(`Corner should be at Z=2, got ${item(1, Polyline3D__get_Points(result_1))}`);
    Expect_isTrue(((a_12 = item(2, Polyline3D__get_Points(result_1)), (b_12 = Pnt_$ctor_Z7AD9E565(10, 10, 2), (x_9 = (a_12.X - b_12.X), (y_9 = (a_12.Y - b_12.Y), (z_7 = (a_12.Z - b_12.Z), Math.sqrt(((x_9 * x_9) + (y_9 * y_9)) + (z_7 * z_7)))))))) < 1E-06)(`End point should be at Z=2, got ${item(2, Polyline3D__get_Points(result_1))}`);
}), Test_testCase("Polyline3D.offset open straight line (colinear)", () => {
    let copyOfStruct_2, arg_7, arg_1_2, a_14, b_14, x_12, y_12, z_9, a_16, b_16, x_13, y_13, z_10, a_18, b_18, x_14, y_14, z_11;
    const pl_2 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(5, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)]);
    const refNormal_2 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const result_2 = Polyline3D_offsetWithRef_39D084EF(pl_2, 1, 0, refNormal_2, false);
    recordTestCase("open straight line (colinear)", pl_2, 1, 0, refNormal_2, result_2);
    const actual_2 = Polyline3D__get_PointCount(result_2) | 0;
    if ((actual_2 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_2, 3, "Should have 3 points");
    }
    else {
        throw new Exception(contains((copyOfStruct_2 = actual_2, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_11) => (structuralHash(x_11) | 0),
        }) ? ((arg_7 = int32ToString(3), (arg_1_2 = int32ToString(actual_2), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("Should have 3 points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_2)("Should have 3 points"));
    }
    Expect_isTrue(((a_14 = item(0, Polyline3D__get_Points(result_2)), (b_14 = Pnt_$ctor_Z7AD9E565(0, 1, 0), (x_12 = (a_14.X - b_14.X), (y_12 = (a_14.Y - b_14.Y), (z_9 = (a_14.Z - b_14.Z), Math.sqrt(((x_12 * x_12) + (y_12 * y_12)) + (z_9 * z_9)))))))) < 1E-06)(`Start should be at Y=1, got ${item(0, Polyline3D__get_Points(result_2))}`);
    Expect_isTrue(((a_16 = item(1, Polyline3D__get_Points(result_2)), (b_16 = Pnt_$ctor_Z7AD9E565(5, 1, 0), (x_13 = (a_16.X - b_16.X), (y_13 = (a_16.Y - b_16.Y), (z_10 = (a_16.Z - b_16.Z), Math.sqrt(((x_13 * x_13) + (y_13 * y_13)) + (z_10 * z_10)))))))) < 1E-06)(`Middle should be at Y=1, got ${item(1, Polyline3D__get_Points(result_2))}`);
    Expect_isTrue(((a_18 = item(2, Polyline3D__get_Points(result_2)), (b_18 = Pnt_$ctor_Z7AD9E565(10, 1, 0), (x_14 = (a_18.X - b_18.X), (y_14 = (a_18.Y - b_18.Y), (z_11 = (a_18.Z - b_18.Z), Math.sqrt(((x_14 * x_14) + (y_14 * y_14)) + (z_11 * z_11)))))))) < 1E-06)(`End should be at Y=1, got ${item(2, Polyline3D__get_Points(result_2))}`);
}), Test_testCase("Polyline3D.offset closed triangle", () => {
    let copyOfStruct_3, arg_8, arg_1_3, a_20, b_20, x_17, y_17, z_13;
    const pl_3 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(5, 8.66, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)]);
    const refNormal_3 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const result_3 = Polyline3D_offsetWithRef_39D084EF(pl_3, 1, 0, refNormal_3, false);
    recordTestCase("closed triangle", pl_3, 1, 0, refNormal_3, result_3);
    const actual_3 = Polyline3D__get_PointCount(result_3) | 0;
    if ((actual_3 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_3, 4, "Should have 4 points (closed)");
    }
    else {
        throw new Exception(contains((copyOfStruct_3 = actual_3, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_16) => (structuralHash(x_16) | 0),
        }) ? ((arg_8 = int32ToString(4), (arg_1_3 = int32ToString(actual_3), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("Should have 4 points (closed)")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_3)("Should have 4 points (closed)"));
    }
    Expect_isTrue(((a_20 = item(0, Polyline3D__get_Points(result_3)), (b_20 = item(3, Polyline3D__get_Points(result_3)), (x_17 = (a_20.X - b_20.X), (y_17 = (a_20.Y - b_20.Y), (z_13 = (a_20.Z - b_20.Z), Math.sqrt(((x_17 * x_17) + (y_17 * y_17)) + (z_13 * z_13)))))))) < 1E-06)(`Should remain closed, start=${item(0, Polyline3D__get_Points(result_3))}, end=${item(3, Polyline3D__get_Points(result_3))}`);
}), Test_testCase("Polyline3D.offset closed square inward", () => {
    let copyOfStruct_4, arg_9, arg_1_4, a_22, b_22, x_20, y_20, z_15, a_24, b_24, x_21, y_21, z_16, a_26, b_26, x_22, y_22, z_17, a_28, b_28, x_23, y_23, z_18;
    const pl_4 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)]);
    const refNormal_4 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const result_4 = Polyline3D_offsetWithRef_39D084EF(pl_4, 1, 0, refNormal_4, false);
    recordTestCase("closed square inward", pl_4, 1, 0, refNormal_4, result_4);
    const actual_4 = Polyline3D__get_PointCount(result_4) | 0;
    if ((actual_4 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_4, 5, "Should have 5 points (closed)");
    }
    else {
        throw new Exception(contains((copyOfStruct_4 = actual_4, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_19) => (structuralHash(x_19) | 0),
        }) ? ((arg_9 = int32ToString(5), (arg_1_4 = int32ToString(actual_4), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("Should have 5 points (closed)")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_4)("Should have 5 points (closed)"));
    }
    Expect_isTrue(((a_22 = item(0, Polyline3D__get_Points(result_4)), (b_22 = Pnt_$ctor_Z7AD9E565(1, 1, 0), (x_20 = (a_22.X - b_22.X), (y_20 = (a_22.Y - b_22.Y), (z_15 = (a_22.Z - b_22.Z), Math.sqrt(((x_20 * x_20) + (y_20 * y_20)) + (z_15 * z_15)))))))) < 1E-06)(`Corner 0 should be at (1,1,0), got ${item(0, Polyline3D__get_Points(result_4))}`);
    Expect_isTrue(((a_24 = item(1, Polyline3D__get_Points(result_4)), (b_24 = Pnt_$ctor_Z7AD9E565(9, 1, 0), (x_21 = (a_24.X - b_24.X), (y_21 = (a_24.Y - b_24.Y), (z_16 = (a_24.Z - b_24.Z), Math.sqrt(((x_21 * x_21) + (y_21 * y_21)) + (z_16 * z_16)))))))) < 1E-06)(`Corner 1 should be at (9,1,0), got ${item(1, Polyline3D__get_Points(result_4))}`);
    Expect_isTrue(((a_26 = item(2, Polyline3D__get_Points(result_4)), (b_26 = Pnt_$ctor_Z7AD9E565(9, 9, 0), (x_22 = (a_26.X - b_26.X), (y_22 = (a_26.Y - b_26.Y), (z_17 = (a_26.Z - b_26.Z), Math.sqrt(((x_22 * x_22) + (y_22 * y_22)) + (z_17 * z_17)))))))) < 1E-06)(`Corner 2 should be at (9,9,0), got ${item(2, Polyline3D__get_Points(result_4))}`);
    Expect_isTrue(((a_28 = item(3, Polyline3D__get_Points(result_4)), (b_28 = Pnt_$ctor_Z7AD9E565(1, 9, 0), (x_23 = (a_28.X - b_28.X), (y_23 = (a_28.Y - b_28.Y), (z_18 = (a_28.Z - b_28.Z), Math.sqrt(((x_23 * x_23) + (y_23 * y_23)) + (z_18 * z_18)))))))) < 1E-06)(`Corner 3 should be at (1,9,0), got ${item(3, Polyline3D__get_Points(result_4))}`);
}), Test_testCase("Polyline3D.offset closed square outward", () => {
    let copyOfStruct_5, arg_10, arg_1_5, a_30, b_30, x_26, y_26, z_20, a_32, b_32, x_27, y_27, z_21, a_34, b_34, x_28, y_28, z_22, a_36, b_36, x_29, y_29, z_23;
    const pl_5 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)]);
    const refNormal_5 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const result_5 = Polyline3D_offsetWithRef_39D084EF(pl_5, -1, 0, refNormal_5, false);
    recordTestCase("closed square outward", pl_5, -1, 0, refNormal_5, result_5);
    const actual_5 = Polyline3D__get_PointCount(result_5) | 0;
    if ((actual_5 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_5, 5, "Should have 5 points (closed)");
    }
    else {
        throw new Exception(contains((copyOfStruct_5 = actual_5, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_25) => (structuralHash(x_25) | 0),
        }) ? ((arg_10 = int32ToString(5), (arg_1_5 = int32ToString(actual_5), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("Should have 5 points (closed)")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_5)("Should have 5 points (closed)"));
    }
    Expect_isTrue(((a_30 = item(0, Polyline3D__get_Points(result_5)), (b_30 = Pnt_$ctor_Z7AD9E565(-1, -1, 0), (x_26 = (a_30.X - b_30.X), (y_26 = (a_30.Y - b_30.Y), (z_20 = (a_30.Z - b_30.Z), Math.sqrt(((x_26 * x_26) + (y_26 * y_26)) + (z_20 * z_20)))))))) < 1E-06)(`Corner 0 should be at (-1,-1,0), got ${item(0, Polyline3D__get_Points(result_5))}`);
    Expect_isTrue(((a_32 = item(1, Polyline3D__get_Points(result_5)), (b_32 = Pnt_$ctor_Z7AD9E565(11, -1, 0), (x_27 = (a_32.X - b_32.X), (y_27 = (a_32.Y - b_32.Y), (z_21 = (a_32.Z - b_32.Z), Math.sqrt(((x_27 * x_27) + (y_27 * y_27)) + (z_21 * z_21)))))))) < 1E-06)(`Corner 1 should be at (11,-1,0), got ${item(1, Polyline3D__get_Points(result_5))}`);
    Expect_isTrue(((a_34 = item(2, Polyline3D__get_Points(result_5)), (b_34 = Pnt_$ctor_Z7AD9E565(11, 11, 0), (x_28 = (a_34.X - b_34.X), (y_28 = (a_34.Y - b_34.Y), (z_22 = (a_34.Z - b_34.Z), Math.sqrt(((x_28 * x_28) + (y_28 * y_28)) + (z_22 * z_22)))))))) < 1E-06)(`Corner 2 should be at (11,11,0), got ${item(2, Polyline3D__get_Points(result_5))}`);
    Expect_isTrue(((a_36 = item(3, Polyline3D__get_Points(result_5)), (b_36 = Pnt_$ctor_Z7AD9E565(-1, 11, 0), (x_29 = (a_36.X - b_36.X), (y_29 = (a_36.Y - b_36.Y), (z_23 = (a_36.Z - b_36.Z), Math.sqrt(((x_29 * x_29) + (y_29 * y_29)) + (z_23 * z_23)))))))) < 1E-06)(`Corner 3 should be at (-1,11,0), got ${item(3, Polyline3D__get_Points(result_5))}`);
}), Test_testCase("Polyline3D.offset with loop=true on open polyline", () => {
    let copyOfStruct_6, arg_11, arg_1_6;
    const pl_6 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0)]);
    const refNormal_6 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const result_6 = Polyline3D_offsetWithRef_39D084EF(pl_6, 1, 0, refNormal_6, true);
    recordTestCase("open L-shape with loop=true", pl_6, 1, 0, refNormal_6, result_6);
    const actual_6 = Polyline3D__get_PointCount(result_6) | 0;
    if ((actual_6 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_6, 3, "Should still have 3 points (loop closes and opens)");
    }
    else {
        throw new Exception(contains((copyOfStruct_6 = actual_6, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_31) => (structuralHash(x_31) | 0),
        }) ? ((arg_11 = int32ToString(3), (arg_1_6 = int32ToString(actual_6), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_11)(arg_1_6)("Should still have 3 points (loop closes and opens)")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_6)("Should still have 3 points (loop closes and opens)"));
    }
}), Test_testCase("Polyline3D.offset 3D polyline not in XY plane", () => {
    let copyOfStruct_7, arg_12, arg_1_7;
    const pl_7 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 10)]);
    const refNormal_7 = UnitVec_$ctor_Z7AD9E565(0, 1, 0);
    const result_7 = Polyline3D_offsetWithRef_39D084EF(pl_7, 1, 0, refNormal_7, false);
    recordTestCase("L-shape in XZ plane", pl_7, 1, 0, refNormal_7, result_7);
    const actual_7 = Polyline3D__get_PointCount(result_7) | 0;
    if ((actual_7 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_7, 3, "Should have 3 points");
    }
    else {
        throw new Exception(contains((copyOfStruct_7 = actual_7, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_33) => (structuralHash(x_33) | 0),
        }) ? ((arg_12 = int32ToString(3), (arg_1_7 = int32ToString(actual_7), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_12)(arg_1_7)("Should have 3 points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_7)("Should have 3 points"));
    }
}), Test_testCase("Polyline3D.offset combined in-plane and perpendicular", () => {
    let copyOfStruct_8, arg_13, arg_1_8, a_38, b_38, x_36, y_36, z_27, a_40, b_40, x_37, y_37, z_28;
    const pl_8 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)]);
    const refNormal_8 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const result_8 = Polyline3D_offsetWithRef_39D084EF(pl_8, 2, 3, refNormal_8, false);
    recordTestCase("combined in-plane and perpendicular", pl_8, 2, 3, refNormal_8, result_8);
    const actual_8 = Polyline3D__get_PointCount(result_8) | 0;
    if ((actual_8 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_8, 2, "Should have 2 points");
    }
    else {
        throw new Exception(contains((copyOfStruct_8 = actual_8, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_35) => (structuralHash(x_35) | 0),
        }) ? ((arg_13 = int32ToString(2), (arg_1_8 = int32ToString(actual_8), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_13)(arg_1_8)("Should have 2 points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_8)("Should have 2 points"));
    }
    Expect_isTrue(((a_38 = item(0, Polyline3D__get_Points(result_8)), (b_38 = Pnt_$ctor_Z7AD9E565(0, 2, 3), (x_36 = (a_38.X - b_38.X), (y_36 = (a_38.Y - b_38.Y), (z_27 = (a_38.Z - b_38.Z), Math.sqrt(((x_36 * x_36) + (y_36 * y_36)) + (z_27 * z_27)))))))) < 1E-06)(`Start should be at (0,2,3), got ${item(0, Polyline3D__get_Points(result_8))}`);
    Expect_isTrue(((a_40 = item(1, Polyline3D__get_Points(result_8)), (b_40 = Pnt_$ctor_Z7AD9E565(10, 2, 3), (x_37 = (a_40.X - b_40.X), (y_37 = (a_40.Y - b_40.Y), (z_28 = (a_40.Z - b_40.Z), Math.sqrt(((x_37 * x_37) + (y_37 * y_37)) + (z_28 * z_28)))))))) < 1E-06)(`End should be at (10,2,3), got ${item(1, Polyline3D__get_Points(result_8))}`);
}), Test_testCase("Polyline3D.offset zero distances returns original shape", () => {
    let copyOfStruct_9, arg_14, arg_1_9, a_42, b_42, x_40, y_40, z_30, a_44, b_44, x_41, y_41, z_31, a_46, b_46, x_42, y_42, z_32;
    const pts_9 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0)];
    const pl_9 = Polyline3D_$ctor_516DFD0A(pts_9);
    const refNormal_9 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const result_9 = Polyline3D_offsetWithRef_39D084EF(pl_9, 0, 0, refNormal_9, false);
    recordTestCase("zero distances returns original", pl_9, 0, 0, refNormal_9, result_9);
    const actual_9 = Polyline3D__get_PointCount(result_9) | 0;
    if ((actual_9 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_9, 3, "Should have 3 points");
    }
    else {
        throw new Exception(contains((copyOfStruct_9 = actual_9, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_39) => (structuralHash(x_39) | 0),
        }) ? ((arg_14 = int32ToString(3), (arg_1_9 = int32ToString(actual_9), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_14)(arg_1_9)("Should have 3 points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_9)("Should have 3 points"));
    }
    Expect_isTrue(((a_42 = item(0, Polyline3D__get_Points(result_9)), (b_42 = item(0, pts_9), (x_40 = (a_42.X - b_42.X), (y_40 = (a_42.Y - b_42.Y), (z_30 = (a_42.Z - b_42.Z), Math.sqrt(((x_40 * x_40) + (y_40 * y_40)) + (z_30 * z_30)))))))) < 1E-06)("Point 0 should be unchanged");
    Expect_isTrue(((a_44 = item(1, Polyline3D__get_Points(result_9)), (b_44 = item(1, pts_9), (x_41 = (a_44.X - b_44.X), (y_41 = (a_44.Y - b_44.Y), (z_31 = (a_44.Z - b_44.Z), Math.sqrt(((x_41 * x_41) + (y_41 * y_41)) + (z_31 * z_31)))))))) < 1E-06)("Point 1 should be unchanged");
    Expect_isTrue(((a_46 = item(2, Polyline3D__get_Points(result_9)), (b_46 = item(2, pts_9), (x_42 = (a_46.X - b_46.X), (y_42 = (a_46.Y - b_46.Y), (z_32 = (a_46.Z - b_46.Z), Math.sqrt(((x_42 * x_42) + (y_42 * y_42)) + (z_32 * z_32)))))))) < 1E-06)("Point 2 should be unchanged");
}), Test_testCase("Polyline3D.offset minimum 2 points", () => {
    let copyOfStruct_10, arg_15, arg_1_10;
    const pl_10 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)]);
    const refNormal_10 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const result_10 = Polyline3D_offsetWithRef_39D084EF(pl_10, 1, 0, refNormal_10, false);
    recordTestCase("minimum 2 points", pl_10, 1, 0, refNormal_10, result_10);
    const actual_10 = Polyline3D__get_PointCount(result_10) | 0;
    if ((actual_10 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_10, 2, "Should have 2 points");
    }
    else {
        throw new Exception(contains((copyOfStruct_10 = actual_10, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_44) => (structuralHash(x_44) | 0),
        }) ? ((arg_15 = int32ToString(2), (arg_1_10 = int32ToString(actual_10), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_15)(arg_1_10)("Should have 2 points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_10)("Should have 2 points"));
    }
}), Test_testCase("Polyline3D.offset fails with less than 2 points", () => {
    const pl_11 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0)]);
    const refNormal_11 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    Expect_throws(() => {
        Polyline3D_offsetWithRef_39D084EF(pl_11, 1, 0, refNormal_11, false);
    }, "Should fail with less than 2 points");
}), Test_testCase("Polyline3D.offset negative perpendicular offset", () => {
    let a_48, b_48, x_47, y_47, z_36, a_50, b_50, x_48, y_48, z_37;
    const pl_12 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)]);
    const refNormal_12 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const result_11 = Polyline3D_offsetWithRef_39D084EF(pl_12, 0, -2, refNormal_12, false);
    recordTestCase("negative perpendicular offset", pl_12, 0, -2, refNormal_12, result_11);
    Expect_isTrue(((a_48 = item(0, Polyline3D__get_Points(result_11)), (b_48 = Pnt_$ctor_Z7AD9E565(0, 0, -2), (x_47 = (a_48.X - b_48.X), (y_47 = (a_48.Y - b_48.Y), (z_36 = (a_48.Z - b_48.Z), Math.sqrt(((x_47 * x_47) + (y_47 * y_47)) + (z_36 * z_36)))))))) < 1E-06)(`Start should be at Z=-2, got ${item(0, Polyline3D__get_Points(result_11))}`);
    Expect_isTrue(((a_50 = item(1, Polyline3D__get_Points(result_11)), (b_50 = Pnt_$ctor_Z7AD9E565(10, 0, -2), (x_48 = (a_50.X - b_50.X), (y_48 = (a_50.Y - b_50.Y), (z_37 = (a_50.Z - b_50.Z), Math.sqrt(((x_48 * x_48) + (y_48 * y_48)) + (z_37 * z_37)))))))) < 1E-06)(`End should be at Z=-2, got ${item(1, Polyline3D__get_Points(result_11))}`);
}), Test_testCase("Polyline3D.offset positive then negative returns to original (closed square)", () => {
    let copyOfStruct_11, arg_16, arg_1_11;
    const pts_13 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)];
    const pl_13 = Polyline3D_$ctor_516DFD0A(pts_13);
    const refNormal_13 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const offset2 = Polyline3D_offsetWithRef_39D084EF(Polyline3D_offsetWithRef_39D084EF(pl_13, 1, 0, refNormal_13, false), -1, 0, refNormal_13, false);
    const actual_11 = Polyline3D__get_PointCount(offset2) | 0;
    const expected_11 = pts_13.length | 0;
    if ((actual_11 === expected_11) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_11, expected_11, "same count after roundtrip");
    }
    else {
        throw new Exception(contains((copyOfStruct_11 = actual_11, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_50) => (structuralHash(x_50) | 0),
        }) ? ((arg_16 = int32ToString(expected_11), (arg_1_11 = int32ToString(actual_11), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_16)(arg_1_11)("same count after roundtrip")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_11)(actual_11)("same count after roundtrip"));
    }
    for (let i = 0; i <= (pts_13.length - 1); i++) {
        let a_52, b_52, x_51, y_51, z_39;
        Expect_isTrue(((a_52 = item(i, Polyline3D__get_Points(offset2)), (b_52 = item(i, pts_13), (x_51 = (a_52.X - b_52.X), (y_51 = (a_52.Y - b_52.Y), (z_39 = (a_52.Z - b_52.Z), Math.sqrt(((x_51 * x_51) + (y_51 * y_51)) + (z_39 * z_39)))))))) < 1E-06)(`pt[${i}] returns to original after +d then -d`);
    }
}), Test_testCase("Polyline3D.offset negative then positive returns to original (closed square)", () => {
    let copyOfStruct_12, arg_17, arg_1_12;
    const pts_14 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)];
    const pl_14 = Polyline3D_$ctor_516DFD0A(pts_14);
    const refNormal_14 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const offset2_1 = Polyline3D_offsetWithRef_39D084EF(Polyline3D_offsetWithRef_39D084EF(pl_14, -1.5, 0, refNormal_14, false), 1.5, 0, refNormal_14, false);
    const actual_12 = Polyline3D__get_PointCount(offset2_1) | 0;
    const expected_12 = pts_14.length | 0;
    if ((actual_12 === expected_12) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_12, expected_12, "same count after roundtrip");
    }
    else {
        throw new Exception(contains((copyOfStruct_12 = actual_12, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_53) => (structuralHash(x_53) | 0),
        }) ? ((arg_17 = int32ToString(expected_12), (arg_1_12 = int32ToString(actual_12), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_17)(arg_1_12)("same count after roundtrip")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_12)(actual_12)("same count after roundtrip"));
    }
    for (let i_1 = 0; i_1 <= (pts_14.length - 1); i_1++) {
        let a_54, b_54, x_54, y_54, z_41;
        Expect_isTrue(((a_54 = item(i_1, Polyline3D__get_Points(offset2_1)), (b_54 = item(i_1, pts_14), (x_54 = (a_54.X - b_54.X), (y_54 = (a_54.Y - b_54.Y), (z_41 = (a_54.Z - b_54.Z), Math.sqrt(((x_54 * x_54) + (y_54 * y_54)) + (z_41 * z_41)))))))) < 1E-06)(`pt[${i_1}] returns to original after -d then +d`);
    }
}), Test_testCase("Polyline3D.offset positive then negative returns to original (open L-shape)", () => {
    let copyOfStruct_13, arg_18, arg_1_13;
    const pts_15 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0)];
    const pl_15 = Polyline3D_$ctor_516DFD0A(pts_15);
    const refNormal_15 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const offset2_2 = Polyline3D_offsetWithRef_39D084EF(Polyline3D_offsetWithRef_39D084EF(pl_15, 1, 0, refNormal_15, false), -1, 0, refNormal_15, false);
    const actual_13 = Polyline3D__get_PointCount(offset2_2) | 0;
    const expected_13 = pts_15.length | 0;
    if ((actual_13 === expected_13) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_13, expected_13, "same count after roundtrip");
    }
    else {
        throw new Exception(contains((copyOfStruct_13 = actual_13, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_56) => (structuralHash(x_56) | 0),
        }) ? ((arg_18 = int32ToString(expected_13), (arg_1_13 = int32ToString(actual_13), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_18)(arg_1_13)("same count after roundtrip")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_13)(actual_13)("same count after roundtrip"));
    }
    for (let i_2 = 0; i_2 <= (pts_15.length - 1); i_2++) {
        let a_56, b_56, x_57, y_57, z_43;
        Expect_isTrue(((a_56 = item(i_2, Polyline3D__get_Points(offset2_2)), (b_56 = item(i_2, pts_15), (x_57 = (a_56.X - b_56.X), (y_57 = (a_56.Y - b_56.Y), (z_43 = (a_56.Z - b_56.Z), Math.sqrt(((x_57 * x_57) + (y_57 * y_57)) + (z_43 * z_43)))))))) < 1E-06)(`pt[${i_2}] returns to original after +d then -d (open L-shape)`);
    }
}), Test_testCase("Polyline3D.offset positive then negative returns to original (triangle)", () => {
    let copyOfStruct_14, arg_19, arg_1_14;
    const pts_16 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(5, 8.66, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)];
    const pl_16 = Polyline3D_$ctor_516DFD0A(pts_16);
    const refNormal_16 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const offset2_3 = Polyline3D_offsetWithRef_39D084EF(Polyline3D_offsetWithRef_39D084EF(pl_16, 0.5, 0, refNormal_16, false), -0.5, 0, refNormal_16, false);
    const actual_14 = Polyline3D__get_PointCount(offset2_3) | 0;
    const expected_14 = pts_16.length | 0;
    if ((actual_14 === expected_14) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_14, expected_14, "same count after roundtrip (triangle)");
    }
    else {
        throw new Exception(contains((copyOfStruct_14 = actual_14, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_59) => (structuralHash(x_59) | 0),
        }) ? ((arg_19 = int32ToString(expected_14), (arg_1_14 = int32ToString(actual_14), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_19)(arg_1_14)("same count after roundtrip (triangle)")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_14)(actual_14)("same count after roundtrip (triangle)"));
    }
    for (let i_3 = 0; i_3 <= (pts_16.length - 1); i_3++) {
        let a_58, b_58, x_60, y_60, z_45;
        Expect_isTrue(((a_58 = item(i_3, Polyline3D__get_Points(offset2_3)), (b_58 = item(i_3, pts_16), (x_60 = (a_58.X - b_58.X), (y_60 = (a_58.Y - b_58.Y), (z_45 = (a_58.Z - b_58.Z), Math.sqrt(((x_60 * x_60) + (y_60 * y_60)) + (z_45 * z_45)))))))) < 1E-06)(`pt[${i_3}] returns to original (triangle)`);
    }
}), Test_testCase("Polyline3D.offset roundtrip with perpendicular offset", () => {
    let copyOfStruct_15, arg_20, arg_1_15;
    const pts_17 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)];
    const pl_17 = Polyline3D_$ctor_516DFD0A(pts_17);
    const refNormal_17 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const offset2_4 = Polyline3D_offsetWithRef_39D084EF(Polyline3D_offsetWithRef_39D084EF(pl_17, 0, 2, refNormal_17, false), 0, -2, refNormal_17, false);
    const actual_15 = Polyline3D__get_PointCount(offset2_4) | 0;
    const expected_15 = pts_17.length | 0;
    if ((actual_15 === expected_15) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_15, expected_15, "same count after roundtrip");
    }
    else {
        throw new Exception(contains((copyOfStruct_15 = actual_15, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_62) => (structuralHash(x_62) | 0),
        }) ? ((arg_20 = int32ToString(expected_15), (arg_1_15 = int32ToString(actual_15), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_20)(arg_1_15)("same count after roundtrip")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_15)(actual_15)("same count after roundtrip"));
    }
    for (let i_4 = 0; i_4 <= (pts_17.length - 1); i_4++) {
        let a_60, b_60, x_63, y_63, z_47;
        Expect_isTrue(((a_60 = item(i_4, Polyline3D__get_Points(offset2_4)), (b_60 = item(i_4, pts_17), (x_63 = (a_60.X - b_60.X), (y_63 = (a_60.Y - b_60.Y), (z_47 = (a_60.Z - b_60.Z), Math.sqrt(((x_63 * x_63) + (y_63 * y_63)) + (z_47 * z_47)))))))) < 1E-06)(`pt[${i_4}] returns to original after perpendicular roundtrip`);
    }
}), Test_testCase("Polyline3D.offset roundtrip with combined offsets", () => {
    let copyOfStruct_16, arg_21, arg_1_16;
    const pts_18 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)];
    const pl_18 = Polyline3D_$ctor_516DFD0A(pts_18);
    const refNormal_18 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const offset2_5 = Polyline3D_offsetWithRef_39D084EF(Polyline3D_offsetWithRef_39D084EF(pl_18, 1, 2, refNormal_18, false), -1, -2, refNormal_18, false);
    const actual_16 = Polyline3D__get_PointCount(offset2_5) | 0;
    const expected_16 = pts_18.length | 0;
    if ((actual_16 === expected_16) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_16, expected_16, "same count after combined roundtrip");
    }
    else {
        throw new Exception(contains((copyOfStruct_16 = actual_16, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_65) => (structuralHash(x_65) | 0),
        }) ? ((arg_21 = int32ToString(expected_16), (arg_1_16 = int32ToString(actual_16), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_21)(arg_1_16)("same count after combined roundtrip")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_16)(actual_16)("same count after combined roundtrip"));
    }
    for (let i_5 = 0; i_5 <= (pts_18.length - 1); i_5++) {
        let a_62, b_62, x_66, y_66, z_49;
        Expect_isTrue(((a_62 = item(i_5, Polyline3D__get_Points(offset2_5)), (b_62 = item(i_5, pts_18), (x_66 = (a_62.X - b_62.X), (y_66 = (a_62.Y - b_62.Y), (z_49 = (a_62.Z - b_62.Z), Math.sqrt(((x_66 * x_66) + (y_66 * y_66)) + (z_49 * z_49)))))))) < 1E-06)(`pt[${i_5}] returns to original after combined roundtrip`);
    }
}), Test_testCase("Polyline3D.offset pentagon inward", () => {
    let copyOfStruct_17, arg_22, arg_1_17;
    const pts_19 = Array.from(toList(delay(() => collect((i_6) => {
        const angle = (((i_6 * 2) * 3.141592653589793) / 5) - (3.141592653589793 / 2);
        return singleton(Pnt_$ctor_Z7AD9E565(10 * Math.cos(angle), 10 * Math.sin(angle), 0));
    }, rangeDouble(0, 1, 4)))));
    void (pts_19.push(item(0, pts_19)));
    const pl_19 = Polyline3D_$ctor_516DFD0A(pts_19);
    const refNormal_19 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const offset2_6 = Polyline3D_offsetWithRef_39D084EF(Polyline3D_offsetWithRef_39D084EF(pl_19, 1, 0, refNormal_19, false), -1, 0, refNormal_19, false);
    const actual_17 = Polyline3D__get_PointCount(offset2_6) | 0;
    const expected_17 = pts_19.length | 0;
    if ((actual_17 === expected_17) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_17, expected_17, "same count after roundtrip (pentagon)");
    }
    else {
        throw new Exception(contains((copyOfStruct_17 = actual_17, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_68) => (structuralHash(x_68) | 0),
        }) ? ((arg_22 = int32ToString(expected_17), (arg_1_17 = int32ToString(actual_17), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_22)(arg_1_17)("same count after roundtrip (pentagon)")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_17)(actual_17)("same count after roundtrip (pentagon)"));
    }
    for (let i_7 = 0; i_7 <= (pts_19.length - 1); i_7++) {
        let a_64, b_64, x_69, y_69, z_51;
        Expect_isTrue(((a_64 = item(i_7, Polyline3D__get_Points(offset2_6)), (b_64 = item(i_7, pts_19), (x_69 = (a_64.X - b_64.X), (y_69 = (a_64.Y - b_64.Y), (z_51 = (a_64.Z - b_64.Z), Math.sqrt(((x_69 * x_69) + (y_69 * y_69)) + (z_51 * z_51)))))))) < 1E-06)(`pt[${i_7}] returns to original (pentagon)`);
    }
}), Test_testCase("Polyline3D.offset hexagon roundtrip", () => {
    let copyOfStruct_18, arg_23, arg_1_18;
    const pts_20 = Array.from(toList(delay(() => collect((i_8) => {
        const angle_1 = (i_8 * 3.141592653589793) / 3;
        return singleton(Pnt_$ctor_Z7AD9E565(10 * Math.cos(angle_1), 10 * Math.sin(angle_1), 0));
    }, rangeDouble(0, 1, 5)))));
    void (pts_20.push(item(0, pts_20)));
    const pl_20 = Polyline3D_$ctor_516DFD0A(pts_20);
    const refNormal_20 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const offset1_7 = Polyline3D_offsetWithRef_39D084EF(pl_20, 1.5, 0, refNormal_20, false);
    recordTestCase("hexagon", pl_20, 1.5, 0, refNormal_20, offset1_7);
    const offset2_7 = Polyline3D_offsetWithRef_39D084EF(offset1_7, -1.5, 0, refNormal_20, false);
    const actual_18 = Polyline3D__get_PointCount(offset2_7) | 0;
    const expected_18 = pts_20.length | 0;
    if ((actual_18 === expected_18) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_18, expected_18, "same count after roundtrip (hexagon)");
    }
    else {
        throw new Exception(contains((copyOfStruct_18 = actual_18, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_71) => (structuralHash(x_71) | 0),
        }) ? ((arg_23 = int32ToString(expected_18), (arg_1_18 = int32ToString(actual_18), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_23)(arg_1_18)("same count after roundtrip (hexagon)")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_18)(actual_18)("same count after roundtrip (hexagon)"));
    }
    for (let i_9 = 0; i_9 <= (pts_20.length - 1); i_9++) {
        let a_66, b_66, x_72, y_72, z_53;
        Expect_isTrue(((a_66 = item(i_9, Polyline3D__get_Points(offset2_7)), (b_66 = item(i_9, pts_20), (x_72 = (a_66.X - b_66.X), (y_72 = (a_66.Y - b_66.Y), (z_53 = (a_66.Z - b_66.Z), Math.sqrt(((x_72 * x_72) + (y_72 * y_72)) + (z_53 * z_53)))))))) < 1E-06)(`pt[${i_9}] returns to original (hexagon)`);
    }
}), Test_testCase("Polyline3D.offset open zigzag roundtrip", () => {
    let copyOfStruct_19, arg_24, arg_1_19;
    const pts_21 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(20, 0, 0), Pnt_$ctor_Z7AD9E565(30, 10, 0), Pnt_$ctor_Z7AD9E565(40, 0, 0)];
    const pl_21 = Polyline3D_$ctor_516DFD0A(pts_21);
    const refNormal_21 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const offset2_8 = Polyline3D_offsetWithRef_39D084EF(Polyline3D_offsetWithRef_39D084EF(pl_21, 1, 0, refNormal_21, false), -1, 0, refNormal_21, false);
    const actual_19 = Polyline3D__get_PointCount(offset2_8) | 0;
    const expected_19 = pts_21.length | 0;
    if ((actual_19 === expected_19) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_19, expected_19, "same count after roundtrip (zigzag)");
    }
    else {
        throw new Exception(contains((copyOfStruct_19 = actual_19, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_74) => (structuralHash(x_74) | 0),
        }) ? ((arg_24 = int32ToString(expected_19), (arg_1_19 = int32ToString(actual_19), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_24)(arg_1_19)("same count after roundtrip (zigzag)")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_19)(actual_19)("same count after roundtrip (zigzag)"));
    }
    for (let i_10 = 0; i_10 <= (pts_21.length - 1); i_10++) {
        let a_68, b_68, x_75, y_75, z_55;
        Expect_isTrue(((a_68 = item(i_10, Polyline3D__get_Points(offset2_8)), (b_68 = item(i_10, pts_21), (x_75 = (a_68.X - b_68.X), (y_75 = (a_68.Y - b_68.Y), (z_55 = (a_68.Z - b_68.Z), Math.sqrt(((x_75 * x_75) + (y_75 * y_75)) + (z_55 * z_55)))))))) < 1E-06)(`pt[${i_10}] returns to original (zigzag)`);
    }
}), Test_testCase("Polyline3D.offset open staircase pattern", () => {
    let copyOfStruct_20, arg_25, arg_1_20;
    const pts_22 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(20, 10, 0), Pnt_$ctor_Z7AD9E565(20, 20, 0)];
    const pl_22 = Polyline3D_$ctor_516DFD0A(pts_22);
    const refNormal_22 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const offset2_9 = Polyline3D_offsetWithRef_39D084EF(Polyline3D_offsetWithRef_39D084EF(pl_22, 1, 0, refNormal_22, false), -1, 0, refNormal_22, false);
    const actual_20 = Polyline3D__get_PointCount(offset2_9) | 0;
    const expected_20 = pts_22.length | 0;
    if ((actual_20 === expected_20) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_20, expected_20, "same count after roundtrip (staircase)");
    }
    else {
        throw new Exception(contains((copyOfStruct_20 = actual_20, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_77) => (structuralHash(x_77) | 0),
        }) ? ((arg_25 = int32ToString(expected_20), (arg_1_20 = int32ToString(actual_20), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_25)(arg_1_20)("same count after roundtrip (staircase)")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_20)(actual_20)("same count after roundtrip (staircase)"));
    }
    for (let i_11 = 0; i_11 <= (pts_22.length - 1); i_11++) {
        let a_70, b_70, x_78, y_78, z_57;
        Expect_isTrue(((a_70 = item(i_11, Polyline3D__get_Points(offset2_9)), (b_70 = item(i_11, pts_22), (x_78 = (a_70.X - b_70.X), (y_78 = (a_70.Y - b_70.Y), (z_57 = (a_70.Z - b_70.Z), Math.sqrt(((x_78 * x_78) + (y_78 * y_78)) + (z_57 * z_57)))))))) < 1E-06)(`pt[${i_11}] returns to original (staircase)`);
    }
}), Test_testCase("Polyline3D.offset with colinear points in middle", () => {
    let copyOfStruct_21, arg_26, arg_1_21;
    const pts_23 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(5, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)];
    const pl_23 = Polyline3D_$ctor_516DFD0A(pts_23);
    const refNormal_23 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const offset1_10 = Polyline3D_offsetWithRef_39D084EF(pl_23, 1, 0, refNormal_23, false);
    recordTestCase("square with colinear point", pl_23, 1, 0, refNormal_23, offset1_10);
    const offset2_10 = Polyline3D_offsetWithRef_39D084EF(offset1_10, -1, 0, refNormal_23, false);
    const actual_21 = Polyline3D__get_PointCount(offset2_10) | 0;
    const expected_21 = pts_23.length | 0;
    if ((actual_21 === expected_21) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_21, expected_21, "same count after roundtrip (colinear)");
    }
    else {
        throw new Exception(contains((copyOfStruct_21 = actual_21, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_80) => (structuralHash(x_80) | 0),
        }) ? ((arg_26 = int32ToString(expected_21), (arg_1_21 = int32ToString(actual_21), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_26)(arg_1_21)("same count after roundtrip (colinear)")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_21)(actual_21)("same count after roundtrip (colinear)"));
    }
    for (let i_12 = 0; i_12 <= (pts_23.length - 1); i_12++) {
        let a_72, b_72, x_81, y_81, z_59;
        Expect_isTrue(((a_72 = item(i_12, Polyline3D__get_Points(offset2_10)), (b_72 = item(i_12, pts_23), (x_81 = (a_72.X - b_72.X), (y_81 = (a_72.Y - b_72.Y), (z_59 = (a_72.Z - b_72.Z), Math.sqrt(((x_81 * x_81) + (y_81 * y_81)) + (z_59 * z_59)))))))) < 1E-06)(`pt[${i_12}] returns to original (colinear)`);
    }
}), Test_testCase("Polyline3D.offset open line with multiple colinear points", () => {
    let copyOfStruct_22, arg_27, arg_1_22;
    const pts_24 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(5, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(15, 0, 0), Pnt_$ctor_Z7AD9E565(20, 0, 0)];
    const pl_24 = Polyline3D_$ctor_516DFD0A(pts_24);
    const refNormal_24 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const result_12 = Polyline3D_offsetWithRef_39D084EF(pl_24, 2, 0, refNormal_24, false);
    recordTestCase("open line with multiple colinear points", pl_24, 2, 0, refNormal_24, result_12);
    const actual_22 = Polyline3D__get_PointCount(result_12) | 0;
    if ((actual_22 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_22, 5, "Should have 5 points");
    }
    else {
        throw new Exception(contains((copyOfStruct_22 = actual_22, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_83) => (structuralHash(x_83) | 0),
        }) ? ((arg_27 = int32ToString(5), (arg_1_22 = int32ToString(actual_22), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_27)(arg_1_22)("Should have 5 points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_22)("Should have 5 points"));
    }
    for (let i_13 = 0; i_13 <= (Polyline3D__get_PointCount(result_12) - 1); i_13++) {
        let a_74, b_74, x_84, y_84, z_61;
        Expect_isTrue(((a_74 = item(i_13, Polyline3D__get_Points(result_12)), (b_74 = Pnt_$ctor_Z7AD9E565(item(i_13, pts_24).X, 2, 0), (x_84 = (a_74.X - b_74.X), (y_84 = (a_74.Y - b_74.Y), (z_61 = (a_74.Z - b_74.Z), Math.sqrt(((x_84 * x_84) + (y_84 * y_84)) + (z_61 * z_61)))))))) < 1E-06)(`pt[${i_13}] should be at Y=2`);
    }
}), Test_testCase("Polyline3D.offset in XZ plane", () => {
    let copyOfStruct_24, arg_28, arg_1_23;
    const pts_25 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 10), Pnt_$ctor_Z7AD9E565(0, 0, 10), Pnt_$ctor_Z7AD9E565(0, 0, 0)];
    const pl_25 = Polyline3D_$ctor_516DFD0A(pts_25);
    const refNormal_25 = UnitVec_$ctor_Z7AD9E565(0, 1, 0);
    const offset1_11 = Polyline3D_offsetWithRef_39D084EF(pl_25, 1, 0, refNormal_25, false);
    recordTestCase("XZ plane square", pl_25, 1, 0, refNormal_25, offset1_11);
    const offset2_11 = Polyline3D_offsetWithRef_39D084EF(offset1_11, -1, 0, refNormal_25, false);
    const actual_23 = Polyline3D__get_PointCount(offset2_11) | 0;
    const expected_23 = pts_25.length | 0;
    if ((actual_23 === expected_23) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_23, expected_23, "same count after roundtrip (XZ plane)");
    }
    else {
        throw new Exception(contains((copyOfStruct_24 = actual_23, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_86) => (structuralHash(x_86) | 0),
        }) ? ((arg_28 = int32ToString(expected_23), (arg_1_23 = int32ToString(actual_23), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_28)(arg_1_23)("same count after roundtrip (XZ plane)")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_23)(actual_23)("same count after roundtrip (XZ plane)"));
    }
    for (let i_14 = 0; i_14 <= (pts_25.length - 1); i_14++) {
        let a_76, b_76, x_87, y_87, z_63;
        Expect_isTrue(((a_76 = item(i_14, Polyline3D__get_Points(offset2_11)), (b_76 = item(i_14, pts_25), (x_87 = (a_76.X - b_76.X), (y_87 = (a_76.Y - b_76.Y), (z_63 = (a_76.Z - b_76.Z), Math.sqrt(((x_87 * x_87) + (y_87 * y_87)) + (z_63 * z_63)))))))) < 1E-06)(`pt[${i_14}] returns to original (XZ plane)`);
    }
}), Test_testCase("Polyline3D.offset in YZ plane", () => {
    let copyOfStruct_25, arg_29, arg_1_24;
    const pts_26 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 10), Pnt_$ctor_Z7AD9E565(0, 0, 10), Pnt_$ctor_Z7AD9E565(0, 0, 0)];
    const pl_26 = Polyline3D_$ctor_516DFD0A(pts_26);
    const refNormal_26 = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
    const offset1_12 = Polyline3D_offsetWithRef_39D084EF(pl_26, 1, 0, refNormal_26, false);
    recordTestCase("YZ plane square", pl_26, 1, 0, refNormal_26, offset1_12);
    const offset2_12 = Polyline3D_offsetWithRef_39D084EF(offset1_12, -1, 0, refNormal_26, false);
    const actual_24 = Polyline3D__get_PointCount(offset2_12) | 0;
    const expected_24 = pts_26.length | 0;
    if ((actual_24 === expected_24) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_24, expected_24, "same count after roundtrip (YZ plane)");
    }
    else {
        throw new Exception(contains((copyOfStruct_25 = actual_24, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_89) => (structuralHash(x_89) | 0),
        }) ? ((arg_29 = int32ToString(expected_24), (arg_1_24 = int32ToString(actual_24), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_29)(arg_1_24)("same count after roundtrip (YZ plane)")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_24)(actual_24)("same count after roundtrip (YZ plane)"));
    }
    for (let i_15 = 0; i_15 <= (pts_26.length - 1); i_15++) {
        let a_78, b_78, x_90, y_90, z_65;
        Expect_isTrue(((a_78 = item(i_15, Polyline3D__get_Points(offset2_12)), (b_78 = item(i_15, pts_26), (x_90 = (a_78.X - b_78.X), (y_90 = (a_78.Y - b_78.Y), (z_65 = (a_78.Z - b_78.Z), Math.sqrt(((x_90 * x_90) + (y_90 * y_90)) + (z_65 * z_65)))))))) < 1E-06)(`pt[${i_15}] returns to original (YZ plane)`);
    }
}), Test_testCase("Polyline3D.offset tilted plane (45 deg)", () => {
    let copyOfStruct_26, arg_30, arg_1_25;
    const pts_27 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 10), Pnt_$ctor_Z7AD9E565(5, 8.66, 5), Pnt_$ctor_Z7AD9E565(0, 0, 0)];
    const pl_27 = Polyline3D_$ctor_516DFD0A(pts_27);
    let refNormal_27;
    const l = Math.sqrt(((1 * 1) + (0 * 0)) + (-1 * -1));
    if (!(l > 1E-12)) {
        failUnit3("UnitVec.create", 1, 0, -1);
    }
    const li = 1 / l;
    refNormal_27 = UnitVec_$ctor_Z7AD9E565(li * 1, li * 0, li * -1);
    const offset1_13 = Polyline3D_offsetWithRef_39D084EF(pl_27, 0.5, 0, refNormal_27, false);
    recordTestCase("tilted plane triangle", pl_27, 0.5, 0, refNormal_27, offset1_13);
    const offset2_13 = Polyline3D_offsetWithRef_39D084EF(offset1_13, -0.5, 0, refNormal_27, false);
    const actual_25 = Polyline3D__get_PointCount(offset2_13) | 0;
    const expected_25 = pts_27.length | 0;
    if ((actual_25 === expected_25) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_25, expected_25, "same count after roundtrip (tilted plane)");
    }
    else {
        throw new Exception(contains((copyOfStruct_26 = actual_25, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_94) => (structuralHash(x_94) | 0),
        }) ? ((arg_30 = int32ToString(expected_25), (arg_1_25 = int32ToString(actual_25), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_30)(arg_1_25)("same count after roundtrip (tilted plane)")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_25)(actual_25)("same count after roundtrip (tilted plane)"));
    }
    for (let i_16 = 0; i_16 <= (pts_27.length - 1); i_16++) {
        let a_80, b_80, x_95, y_94, z_68;
        Expect_isTrue(((a_80 = item(i_16, Polyline3D__get_Points(offset2_13)), (b_80 = item(i_16, pts_27), (x_95 = (a_80.X - b_80.X), (y_94 = (a_80.Y - b_80.Y), (z_68 = (a_80.Z - b_80.Z), Math.sqrt(((x_95 * x_95) + (y_94 * y_94)) + (z_68 * z_68)))))))) < 1E-06)(`pt[${i_16}] returns to original (tilted plane)`);
    }
}), Test_testCase("Polyline3D.offset 3D spiral staircase produces valid result", () => {
    let copyOfStruct_27, arg_31, arg_1_26;
    const pts_28 = [Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(0, 10, 5), Pnt_$ctor_Z7AD9E565(-10, 0, 10), Pnt_$ctor_Z7AD9E565(0, -10, 15), Pnt_$ctor_Z7AD9E565(10, 0, 20)];
    const pl_28 = Polyline3D_$ctor_516DFD0A(pts_28);
    const refNormal_28 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const result_13 = Polyline3D_offsetWithRef_39D084EF(pl_28, 1, 0, refNormal_28, false);
    recordTestCase("3D spiral staircase", pl_28, 1, 0, refNormal_28, result_13);
    const actual_26 = Polyline3D__get_PointCount(result_13) | 0;
    const expected_26 = pts_28.length | 0;
    if ((actual_26 === expected_26) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_26, expected_26, "Should have same point count");
    }
    else {
        throw new Exception(contains((copyOfStruct_27 = actual_26, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_97) => (structuralHash(x_97) | 0),
        }) ? ((arg_31 = int32ToString(expected_26), (arg_1_26 = int32ToString(actual_26), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_31)(arg_1_26)("Should have same point count")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_26)(actual_26)("Should have same point count"));
    }
    Expect_isTrue(sum(toList(delay(() => map((i_17) => {
        const a_82 = item(i_17, Polyline3D__get_Points(result_13));
        const b_82 = item(i_17, pts_28);
        const x_98 = a_82.X - b_82.X;
        const y_97 = a_82.Y - b_82.Y;
        const z_70 = a_82.Z - b_82.Z;
        return Math.sqrt(((x_98 * x_98) + (y_97 * y_97)) + (z_70 * z_70));
    }, rangeDouble(0, 1, pts_28.length - 1)))), {
        GetZero: () => 0,
        Add: (x_99, y_98) => (x_99 + y_98),
    }) > 0.1)("Offset should produce different points");
}), Test_testCase("Polyline3D.offset closed rectangle specific values", () => {
    let copyOfStruct_28, arg_32, arg_1_27, a_84, b_84, x_102, y_101, z_72, a_86, b_86, x_103, y_102, z_73, a_88, b_88, x_104, y_103, z_74, a_90, b_90, x_105, y_104, z_75;
    const pl_29 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(20, 0, 0), Pnt_$ctor_Z7AD9E565(20, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)]);
    const refNormal_29 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const result_14 = Polyline3D_offsetWithRef_39D084EF(pl_29, 2, 0, refNormal_29, false);
    recordTestCase("closed rectangle 20x10", pl_29, 2, 0, refNormal_29, result_14);
    const actual_27 = Polyline3D__get_PointCount(result_14) | 0;
    if ((actual_27 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_27, 5, "Should have 5 points");
    }
    else {
        throw new Exception(contains((copyOfStruct_28 = actual_27, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_101) => (structuralHash(x_101) | 0),
        }) ? ((arg_32 = int32ToString(5), (arg_1_27 = int32ToString(actual_27), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_32)(arg_1_27)("Should have 5 points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_27)("Should have 5 points"));
    }
    Expect_isTrue(((a_84 = item(0, Polyline3D__get_Points(result_14)), (b_84 = Pnt_$ctor_Z7AD9E565(2, 2, 0), (x_102 = (a_84.X - b_84.X), (y_101 = (a_84.Y - b_84.Y), (z_72 = (a_84.Z - b_84.Z), Math.sqrt(((x_102 * x_102) + (y_101 * y_101)) + (z_72 * z_72)))))))) < 1E-06)(`Corner 0 should be at (2,2,0), got ${item(0, Polyline3D__get_Points(result_14))}`);
    Expect_isTrue(((a_86 = item(1, Polyline3D__get_Points(result_14)), (b_86 = Pnt_$ctor_Z7AD9E565(18, 2, 0), (x_103 = (a_86.X - b_86.X), (y_102 = (a_86.Y - b_86.Y), (z_73 = (a_86.Z - b_86.Z), Math.sqrt(((x_103 * x_103) + (y_102 * y_102)) + (z_73 * z_73)))))))) < 1E-06)(`Corner 1 should be at (18,2,0), got ${item(1, Polyline3D__get_Points(result_14))}`);
    Expect_isTrue(((a_88 = item(2, Polyline3D__get_Points(result_14)), (b_88 = Pnt_$ctor_Z7AD9E565(18, 8, 0), (x_104 = (a_88.X - b_88.X), (y_103 = (a_88.Y - b_88.Y), (z_74 = (a_88.Z - b_88.Z), Math.sqrt(((x_104 * x_104) + (y_103 * y_103)) + (z_74 * z_74)))))))) < 1E-06)(`Corner 2 should be at (18,8,0), got ${item(2, Polyline3D__get_Points(result_14))}`);
    Expect_isTrue(((a_90 = item(3, Polyline3D__get_Points(result_14)), (b_90 = Pnt_$ctor_Z7AD9E565(2, 8, 0), (x_105 = (a_90.X - b_90.X), (y_104 = (a_90.Y - b_90.Y), (z_75 = (a_90.Z - b_90.Z), Math.sqrt(((x_105 * x_105) + (y_104 * y_104)) + (z_75 * z_75)))))))) < 1E-06)(`Corner 3 should be at (2,8,0), got ${item(3, Polyline3D__get_Points(result_14))}`);
}), Test_testCase("Polyline3D.offset open 3-point line distance check", () => {
    const pts_30 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0)];
    const pl_30 = Polyline3D_$ctor_516DFD0A(pts_30);
    const refNormal_30 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const result_15 = Polyline3D_offsetWithRef_39D084EF(pl_30, 3, 0, refNormal_30, false);
    recordTestCase("open 3-point line distance check", pl_30, 3, 0, refNormal_30, result_15);
    let dist0;
    const a_92 = item(0, Polyline3D__get_Points(result_15));
    const b_92 = item(0, pts_30);
    const x_107 = a_92.X - b_92.X;
    const y_106 = a_92.Y - b_92.Y;
    const z_77 = a_92.Z - b_92.Z;
    dist0 = Math.sqrt(((x_107 * x_107) + (y_106 * y_106)) + (z_77 * z_77));
    let dist2;
    const a_94 = item(2, Polyline3D__get_Points(result_15));
    const b_94 = item(2, pts_30);
    const x_108 = a_94.X - b_94.X;
    const y_107 = a_94.Y - b_94.Y;
    const z_78 = a_94.Z - b_94.Z;
    dist2 = Math.sqrt(((x_108 * x_108) + (y_107 * y_107)) + (z_78 * z_78));
    Expect_floatClose(tol, dist0, 3, `First point should be ${3} away, got ${dist0}`);
    Expect_floatClose(tol, dist2, 3, `Last point should be ${3} away, got ${dist2}`);
}), Test_testCase("Polyline3D.offset with loop flag on open L returns closed behavior", () => {
    let copyOfStruct_29, arg_33, arg_1_28, copyOfStruct_30, arg_34, arg_1_29, a_96, b_96, x_112, y_111, z_80;
    const pl_31 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0)]);
    const refNormal_31 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const resultLoop = Polyline3D_offsetWithRef_39D084EF(pl_31, 1, 0, refNormal_31, true);
    recordTestCase("open L with loop=true", pl_31, 1, 0, refNormal_31, resultLoop);
    const resultOpen = Polyline3D_offsetWithRef_39D084EF(pl_31, 1, 0, refNormal_31, false);
    recordTestCase("open L with loop=false", pl_31, 1, 0, refNormal_31, resultOpen);
    const actual_28 = Polyline3D__get_PointCount(resultLoop) | 0;
    if ((actual_28 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_28, 3, "Loop result should have 3 points");
    }
    else {
        throw new Exception(contains((copyOfStruct_29 = actual_28, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_110) => (structuralHash(x_110) | 0),
        }) ? ((arg_33 = int32ToString(3), (arg_1_28 = int32ToString(actual_28), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_33)(arg_1_28)("Loop result should have 3 points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_28)("Loop result should have 3 points"));
    }
    const actual_29 = Polyline3D__get_PointCount(resultOpen) | 0;
    if ((actual_29 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_29, 3, "Open result should have 3 points");
    }
    else {
        throw new Exception(contains((copyOfStruct_30 = actual_29, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_111) => (structuralHash(x_111) | 0),
        }) ? ((arg_34 = int32ToString(3), (arg_1_29 = int32ToString(actual_29), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_34)(arg_1_29)("Open result should have 3 points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_29)("Open result should have 3 points"));
    }
    Expect_isFalse(((a_96 = item(0, Polyline3D__get_Points(resultLoop)), (b_96 = item(0, Polyline3D__get_Points(resultOpen)), (x_112 = (a_96.X - b_96.X), (y_111 = (a_96.Y - b_96.Y), (z_80 = (a_96.Z - b_96.Z), Math.sqrt(((x_112 * x_112) + (y_111 * y_111)) + (z_80 * z_80)))))))) < 1E-06)("Start points should differ between loop and open");
}), Test_testCase("Polyline3D.offset large offset value", () => {
    let copyOfStruct_31, arg_35, arg_1_30;
    const pts_32 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(100, 0, 0), Pnt_$ctor_Z7AD9E565(100, 100, 0), Pnt_$ctor_Z7AD9E565(0, 100, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)];
    const pl_32 = Polyline3D_$ctor_516DFD0A(pts_32);
    const refNormal_32 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const offset2_14 = Polyline3D_offsetWithRef_39D084EF(Polyline3D_offsetWithRef_39D084EF(pl_32, 10, 0, refNormal_32, false), -10, 0, refNormal_32, false);
    const actual_30 = Polyline3D__get_PointCount(offset2_14) | 0;
    const expected_30 = pts_32.length | 0;
    if ((actual_30 === expected_30) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_30, expected_30, "same count after roundtrip (large offset)");
    }
    else {
        throw new Exception(contains((copyOfStruct_31 = actual_30, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_114) => (structuralHash(x_114) | 0),
        }) ? ((arg_35 = int32ToString(expected_30), (arg_1_30 = int32ToString(actual_30), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_35)(arg_1_30)("same count after roundtrip (large offset)")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_30)(actual_30)("same count after roundtrip (large offset)"));
    }
    for (let i_18 = 0; i_18 <= (pts_32.length - 1); i_18++) {
        let a_98, b_98, x_115, y_114, z_82;
        Expect_isTrue(((a_98 = item(i_18, Polyline3D__get_Points(offset2_14)), (b_98 = item(i_18, pts_32), (x_115 = (a_98.X - b_98.X), (y_114 = (a_98.Y - b_98.Y), (z_82 = (a_98.Z - b_98.Z), Math.sqrt(((x_115 * x_115) + (y_114 * y_114)) + (z_82 * z_82)))))))) < 1E-06)(`pt[${i_18}] returns to original (large offset)`);
    }
}), Test_testCase("Polyline3D.offset very small offset value", () => {
    let copyOfStruct_32, arg_36, arg_1_31;
    const pts_33 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)];
    const pl_33 = Polyline3D_$ctor_516DFD0A(pts_33);
    const refNormal_33 = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    const offset2_15 = Polyline3D_offsetWithRef_39D084EF(Polyline3D_offsetWithRef_39D084EF(pl_33, 0.001, 0, refNormal_33, false), -0.001, 0, refNormal_33, false);
    const actual_31 = Polyline3D__get_PointCount(offset2_15) | 0;
    const expected_31 = pts_33.length | 0;
    if ((actual_31 === expected_31) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
        assertEqual(actual_31, expected_31, "same count after roundtrip (small offset)");
    }
    else {
        throw new Exception(contains((copyOfStruct_32 = actual_31, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
            Equals: equals,
            GetHashCode: (x_117) => (structuralHash(x_117) | 0),
        }) ? ((arg_36 = int32ToString(expected_31), (arg_1_31 = int32ToString(actual_31), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_36)(arg_1_31)("same count after roundtrip (small offset)")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_31)(actual_31)("same count after roundtrip (small offset)"));
    }
    for (let i_19 = 0; i_19 <= (pts_33.length - 1); i_19++) {
        let a_100, b_100, x_118, y_117, z_84;
        Expect_isTrue(((a_100 = item(i_19, Polyline3D__get_Points(offset2_15)), (b_100 = item(i_19, pts_33), (x_118 = (a_100.X - b_100.X), (y_117 = (a_100.Y - b_100.Y), (z_84 = (a_100.Z - b_100.Z), Math.sqrt(((x_118 * x_118) + (y_117 * y_117)) + (z_84 * z_84)))))))) < 1E-06)(`pt[${i_19}] returns to original (small offset)`);
    }
}), Test_testCase("Polyline3D.offset positive inPlane offsets inward for CCW square", () => {
    let a_102, b_102, x_119, y_118, z_85, a_104, b_104, x_120, y_119, z_86, a_106, b_106, x_121, y_120, z_87, a_108, b_108, x_122, y_121, z_88;
    const result_16 = Polyline3D_offset_1FF60F20(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)]), 1, 0, false);
    Expect_isTrue(((a_102 = item(0, Polyline3D__get_Points(result_16)), (b_102 = Pnt_$ctor_Z7AD9E565(1, 1, 0), (x_119 = (a_102.X - b_102.X), (y_118 = (a_102.Y - b_102.Y), (z_85 = (a_102.Z - b_102.Z), Math.sqrt(((x_119 * x_119) + (y_118 * y_118)) + (z_85 * z_85)))))))) < 1E-06)(`CCW: Corner 0 should be inward at (1,1,0), got ${item(0, Polyline3D__get_Points(result_16))}`);
    Expect_isTrue(((a_104 = item(1, Polyline3D__get_Points(result_16)), (b_104 = Pnt_$ctor_Z7AD9E565(9, 1, 0), (x_120 = (a_104.X - b_104.X), (y_119 = (a_104.Y - b_104.Y), (z_86 = (a_104.Z - b_104.Z), Math.sqrt(((x_120 * x_120) + (y_119 * y_119)) + (z_86 * z_86)))))))) < 1E-06)(`CCW: Corner 1 should be inward at (9,1,0), got ${item(1, Polyline3D__get_Points(result_16))}`);
    Expect_isTrue(((a_106 = item(2, Polyline3D__get_Points(result_16)), (b_106 = Pnt_$ctor_Z7AD9E565(9, 9, 0), (x_121 = (a_106.X - b_106.X), (y_120 = (a_106.Y - b_106.Y), (z_87 = (a_106.Z - b_106.Z), Math.sqrt(((x_121 * x_121) + (y_120 * y_120)) + (z_87 * z_87)))))))) < 1E-06)(`CCW: Corner 2 should be inward at (9,9,0), got ${item(2, Polyline3D__get_Points(result_16))}`);
    Expect_isTrue(((a_108 = item(3, Polyline3D__get_Points(result_16)), (b_108 = Pnt_$ctor_Z7AD9E565(1, 9, 0), (x_122 = (a_108.X - b_108.X), (y_121 = (a_108.Y - b_108.Y), (z_88 = (a_108.Z - b_108.Z), Math.sqrt(((x_122 * x_122) + (y_121 * y_121)) + (z_88 * z_88)))))))) < 1E-06)(`CCW: Corner 3 should be inward at (1,9,0), got ${item(3, Polyline3D__get_Points(result_16))}`);
}), Test_testCase("Polyline3D.offset positive inPlane offsets inward for CW square", () => {
    let a_110, b_110, x_123, y_122, z_89, a_112, b_112, x_124, y_123, z_90, a_114, b_114, x_125, y_124, z_91, a_116, b_116, x_126, y_125, z_92;
    const result_17 = Polyline3D_offset_1FF60F20(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)]), 1, 0, false);
    Expect_isTrue(((a_110 = item(0, Polyline3D__get_Points(result_17)), (b_110 = Pnt_$ctor_Z7AD9E565(1, 1, 0), (x_123 = (a_110.X - b_110.X), (y_122 = (a_110.Y - b_110.Y), (z_89 = (a_110.Z - b_110.Z), Math.sqrt(((x_123 * x_123) + (y_122 * y_122)) + (z_89 * z_89)))))))) < 1E-06)(`CW: Corner 0 should be inward at (1,1,0), got ${item(0, Polyline3D__get_Points(result_17))}`);
    Expect_isTrue(((a_112 = item(1, Polyline3D__get_Points(result_17)), (b_112 = Pnt_$ctor_Z7AD9E565(1, 9, 0), (x_124 = (a_112.X - b_112.X), (y_123 = (a_112.Y - b_112.Y), (z_90 = (a_112.Z - b_112.Z), Math.sqrt(((x_124 * x_124) + (y_123 * y_123)) + (z_90 * z_90)))))))) < 1E-06)(`CW: Corner 1 should be inward at (1,9,0), got ${item(1, Polyline3D__get_Points(result_17))}`);
    Expect_isTrue(((a_114 = item(2, Polyline3D__get_Points(result_17)), (b_114 = Pnt_$ctor_Z7AD9E565(9, 9, 0), (x_125 = (a_114.X - b_114.X), (y_124 = (a_114.Y - b_114.Y), (z_91 = (a_114.Z - b_114.Z), Math.sqrt(((x_125 * x_125) + (y_124 * y_124)) + (z_91 * z_91)))))))) < 1E-06)(`CW: Corner 2 should be inward at (9,9,0), got ${item(2, Polyline3D__get_Points(result_17))}`);
    Expect_isTrue(((a_116 = item(3, Polyline3D__get_Points(result_17)), (b_116 = Pnt_$ctor_Z7AD9E565(9, 1, 0), (x_126 = (a_116.X - b_116.X), (y_125 = (a_116.Y - b_116.Y), (z_92 = (a_116.Z - b_116.Z), Math.sqrt(((x_126 * x_126) + (y_125 * y_125)) + (z_92 * z_92)))))))) < 1E-06)(`CW: Corner 3 should be inward at (9,1,0), got ${item(3, Polyline3D__get_Points(result_17))}`);
}), Test_testCase("Polyline3D.offset negative inPlane offsets outward for CCW square", () => {
    let a_118, b_118, x_127, y_126, z_93, a_120, b_120, x_128, y_127, z_94, a_122, b_122, x_129, y_128, z_95, a_124, b_124, x_130, y_129, z_96;
    const result_18 = Polyline3D_offset_1FF60F20(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)]), -1, 0, false);
    Expect_isTrue(((a_118 = item(0, Polyline3D__get_Points(result_18)), (b_118 = Pnt_$ctor_Z7AD9E565(-1, -1, 0), (x_127 = (a_118.X - b_118.X), (y_126 = (a_118.Y - b_118.Y), (z_93 = (a_118.Z - b_118.Z), Math.sqrt(((x_127 * x_127) + (y_126 * y_126)) + (z_93 * z_93)))))))) < 1E-06)(`CCW: Corner 0 should be outward at (-1,-1,0), got ${item(0, Polyline3D__get_Points(result_18))}`);
    Expect_isTrue(((a_120 = item(1, Polyline3D__get_Points(result_18)), (b_120 = Pnt_$ctor_Z7AD9E565(11, -1, 0), (x_128 = (a_120.X - b_120.X), (y_127 = (a_120.Y - b_120.Y), (z_94 = (a_120.Z - b_120.Z), Math.sqrt(((x_128 * x_128) + (y_127 * y_127)) + (z_94 * z_94)))))))) < 1E-06)(`CCW: Corner 1 should be outward at (11,-1,0), got ${item(1, Polyline3D__get_Points(result_18))}`);
    Expect_isTrue(((a_122 = item(2, Polyline3D__get_Points(result_18)), (b_122 = Pnt_$ctor_Z7AD9E565(11, 11, 0), (x_129 = (a_122.X - b_122.X), (y_128 = (a_122.Y - b_122.Y), (z_95 = (a_122.Z - b_122.Z), Math.sqrt(((x_129 * x_129) + (y_128 * y_128)) + (z_95 * z_95)))))))) < 1E-06)(`CCW: Corner 2 should be outward at (11,11,0), got ${item(2, Polyline3D__get_Points(result_18))}`);
    Expect_isTrue(((a_124 = item(3, Polyline3D__get_Points(result_18)), (b_124 = Pnt_$ctor_Z7AD9E565(-1, 11, 0), (x_130 = (a_124.X - b_124.X), (y_129 = (a_124.Y - b_124.Y), (z_96 = (a_124.Z - b_124.Z), Math.sqrt(((x_130 * x_130) + (y_129 * y_129)) + (z_96 * z_96)))))))) < 1E-06)(`CCW: Corner 3 should be outward at (-1,11,0), got ${item(3, Polyline3D__get_Points(result_18))}`);
}), Test_testCase("Polyline3D.offset negative inPlane offsets outward for CW square", () => {
    let a_126, b_126, x_131, y_130, z_97, a_128, b_128, x_132, y_131, z_98, a_130, b_130, x_133, y_132, z_99, a_132, b_132, x_134, y_133, z_100;
    const result_19 = Polyline3D_offset_1FF60F20(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)]), -1, 0, false);
    Expect_isTrue(((a_126 = item(0, Polyline3D__get_Points(result_19)), (b_126 = Pnt_$ctor_Z7AD9E565(-1, -1, 0), (x_131 = (a_126.X - b_126.X), (y_130 = (a_126.Y - b_126.Y), (z_97 = (a_126.Z - b_126.Z), Math.sqrt(((x_131 * x_131) + (y_130 * y_130)) + (z_97 * z_97)))))))) < 1E-06)(`CW: Corner 0 should be outward at (-1,-1,0), got ${item(0, Polyline3D__get_Points(result_19))}`);
    Expect_isTrue(((a_128 = item(1, Polyline3D__get_Points(result_19)), (b_128 = Pnt_$ctor_Z7AD9E565(-1, 11, 0), (x_132 = (a_128.X - b_128.X), (y_131 = (a_128.Y - b_128.Y), (z_98 = (a_128.Z - b_128.Z), Math.sqrt(((x_132 * x_132) + (y_131 * y_131)) + (z_98 * z_98)))))))) < 1E-06)(`CW: Corner 1 should be outward at (-1,11,0), got ${item(1, Polyline3D__get_Points(result_19))}`);
    Expect_isTrue(((a_130 = item(2, Polyline3D__get_Points(result_19)), (b_130 = Pnt_$ctor_Z7AD9E565(11, 11, 0), (x_133 = (a_130.X - b_130.X), (y_132 = (a_130.Y - b_130.Y), (z_99 = (a_130.Z - b_130.Z), Math.sqrt(((x_133 * x_133) + (y_132 * y_132)) + (z_99 * z_99)))))))) < 1E-06)(`CW: Corner 2 should be outward at (11,11,0), got ${item(2, Polyline3D__get_Points(result_19))}`);
    Expect_isTrue(((a_132 = item(3, Polyline3D__get_Points(result_19)), (b_132 = Pnt_$ctor_Z7AD9E565(11, -1, 0), (x_134 = (a_132.X - b_132.X), (y_133 = (a_132.Y - b_132.Y), (z_100 = (a_132.Z - b_132.Z), Math.sqrt(((x_134 * x_134) + (y_133 * y_133)) + (z_100 * z_100)))))))) < 1E-06)(`CW: Corner 3 should be outward at (11,-1,0), got ${item(3, Polyline3D__get_Points(result_19))}`);
}), Test_testCase("Polyline3D.offset positive perp offsets upward for CCW square in XY plane", () => {
    const result_20 = Polyline3D_offset_1FF60F20(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)]), 0, 2, false);
    for (let i_20 = 0; i_20 <= 3; i_20++) {
        Expect_isTrue(Math.abs(item(i_20, Polyline3D__get_Points(result_20)).Z - 2) < 1E-06)(`CCW: Point ${i_20} should be at Z=2, got Z=${item(i_20, Polyline3D__get_Points(result_20)).Z}`);
    }
}), Test_testCase("Polyline3D.offset positive perp offsets downward for CW square in XY plane", () => {
    const result_21 = Polyline3D_offset_1FF60F20(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)]), 0, 2, false);
    for (let i_21 = 0; i_21 <= 3; i_21++) {
        Expect_isTrue(Math.abs(item(i_21, Polyline3D__get_Points(result_21)).Z - -2) < 1E-06)(`CW: Point ${i_21} should be at Z=-2, got Z=${item(i_21, Polyline3D__get_Points(result_21)).Z}`);
    }
}), Test_testCase("Polyline3D.offset negative perp offsets downward for CCW square in XY plane", () => {
    const result_22 = Polyline3D_offset_1FF60F20(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)]), 0, -2, false);
    for (let i_22 = 0; i_22 <= 3; i_22++) {
        Expect_isTrue(Math.abs(item(i_22, Polyline3D__get_Points(result_22)).Z - -2) < 1E-06)(`CCW: Point ${i_22} should be at Z=-2, got Z=${item(i_22, Polyline3D__get_Points(result_22)).Z}`);
    }
}), Test_testCase("Polyline3D.offset negative perp offsets upward for CW square in XY plane", () => {
    const result_23 = Polyline3D_offset_1FF60F20(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)]), 0, -2, false);
    for (let i_23 = 0; i_23 <= 3; i_23++) {
        Expect_isTrue(Math.abs(item(i_23, Polyline3D__get_Points(result_23)).Z - 2) < 1E-06)(`CW: Point ${i_23} should be at Z=2, got Z=${item(i_23, Polyline3D__get_Points(result_23)).Z}`);
    }
}), Test_testCase("Write test data file (when saveTestData=true)", () => {
    writeTestDataFile();
    Expect_isTrue(true)("Test data file written if saveTestData was enabled");
})]));

