
import { Expect_isFalse, Expect_throws, Expect_isTrue, Test_TestCaseBuilder__Zero, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Pnt_$ctor_Z7AD9E565 } from "./Src/Pnt.js";
import { BBox__get_AsString, BBox_$ctor_76A78260 } from "./Src/BBox.js";
import { int32ToString, disposeSafe, getEnumerator, structuralHash, Exception, assertEqual } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { ofArray, contains } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { equals, class_type, decimal_type, string_type, bool_type, int32_type, float64_type } from "./fable_modules/fable-library-js.5.0.0/Reflection.js";
import { printf, toText } from "./fable_modules/fable-library-js.5.0.0/String.js";
import { Pnt__get_AsString, Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_1 } from "./Src/Pnt.js";
import { Vec_$ctor_Z7AD9E565 } from "./Src/Vec.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_1 } from "./Src/Vec.js";
import { Operators_IsNull } from "./fable_modules/fable-library-js.5.0.0/FSharp.Core.js";
import { fail, failEmptySeq, failNull } from "./Src/EuclidErrors.js";
import { isEmpty } from "./fable_modules/fable-library-js.5.0.0/Seq.js";
import { max, min } from "./fable_modules/fable-library-js.5.0.0/Double.js";
import { count } from "./fable_modules/fable-library-js.5.0.0/CollectionUtil.js";
import { item } from "./fable_modules/fable-library-js.5.0.0/Array.js";
import { Line3D_$ctor_5A6659A0 } from "./Src/Line3D.js";
import { Pt_$ctor_7B00E9A0 } from "./Src/Pt.js";
import { BRect_$ctor_77D16AC0 } from "./Src/BRect.js";
import { BBox_notEquals, BBox_equals, BBox__get_Edges, BBox__get_TopPointsLooped, BBox__get_TopPoints, BBox__get_BottomPointsLooped, BBox__get_BottomPoints, BBox__get_Points, BBox__IsTouching_668B4595, BBox_moveZ, BBox_moveY, BBox_moveX, BBox_translate, BBox_move, BBox_expandRelXYZ, BBox_expandRel } from "./Src/BBox.js";
import { Line3D_$ctor_5A6659A0 as Line3D_$ctor_5A6659A0_1 } from "./Src/Line3D.js";
import { toString } from "./fable_modules/fable-library-js.5.0.0/Types.js";

export const tests = Test_testList("BBox", ofArray([Test_testList("Constructor and Basic Properties", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create from two points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let copyOfStruct, arg, arg_1, copyOfStruct_1, arg_6, arg_1_1, copyOfStruct_2, arg_7, arg_1_2, copyOfStruct_3, arg_8, arg_1_3, copyOfStruct_4, arg_9, arg_1_4, copyOfStruct_5, arg_10, arg_1_5;
        let box;
        const a = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b = Pnt_$ctor_Z7AD9E565(10, 5, 3);
        let minX = a.X;
        let maxX;
        if (b.X > minX) {
            maxX = b.X;
        }
        else {
            minX = b.X;
            maxX = a.X;
        }
        let minY = a.Y;
        let maxY;
        if (b.Y > minY) {
            maxY = b.Y;
        }
        else {
            minY = b.Y;
            maxY = a.Y;
        }
        let minZ = a.Z;
        let maxZ;
        if (b.Z > minZ) {
            maxZ = b.Z;
        }
        else {
            minZ = b.Z;
            maxZ = a.Z;
        }
        box = BBox_$ctor_76A78260(minX, minY, minZ, maxX, maxY, maxZ);
        const actual = box.MinX;
        if ((actual === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual, 0, "MinX should be 0");
        }
        else {
            throw new Exception(contains((copyOfStruct = actual, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x) => (structuralHash(x) | 0),
            }) ? ((arg = (0).toString(), (arg_1 = actual.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("MinX should be 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual)("MinX should be 0"));
        }
        const actual_1 = box.MinY;
        if ((actual_1 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, 0, "MinY should be 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_1 = actual_1, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_1) => (structuralHash(x_1) | 0),
            }) ? ((arg_6 = (0).toString(), (arg_1_1 = actual_1.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("MinY should be 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_1)("MinY should be 0"));
        }
        const actual_2 = box.MinZ;
        if ((actual_2 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_2, 0, "MinZ should be 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_2 = actual_2, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_2) => (structuralHash(x_2) | 0),
            }) ? ((arg_7 = (0).toString(), (arg_1_2 = actual_2.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("MinZ should be 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_2)("MinZ should be 0"));
        }
        const actual_3 = box.MaxX;
        if ((actual_3 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, 10, "MaxX should be 10");
        }
        else {
            throw new Exception(contains((copyOfStruct_3 = actual_3, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_3) => (structuralHash(x_3) | 0),
            }) ? ((arg_8 = (10).toString(), (arg_1_3 = actual_3.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("MaxX should be 10")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_3)("MaxX should be 10"));
        }
        const actual_4 = box.MaxY;
        if ((actual_4 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_4, 5, "MaxY should be 5");
        }
        else {
            throw new Exception(contains((copyOfStruct_4 = actual_4, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_4) => (structuralHash(x_4) | 0),
            }) ? ((arg_9 = (5).toString(), (arg_1_4 = actual_4.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("MaxY should be 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_4)("MaxY should be 5"));
        }
        const actual_5 = box.MaxZ;
        if ((actual_5 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_5, 3, "MaxZ should be 3");
        }
        else {
            throw new Exception(contains((copyOfStruct_5 = actual_5, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_5) => (structuralHash(x_5) | 0),
            }) ? ((arg_10 = (3).toString(), (arg_1_5 = actual_5.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("MaxZ should be 3")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_5)("MaxZ should be 3"));
        }
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create from two points with swapped coordinates", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let copyOfStruct_6, arg_11, arg_1_6, copyOfStruct_7, arg_12, arg_1_7, copyOfStruct_8, arg_13, arg_1_8, copyOfStruct_9, arg_14, arg_1_9, copyOfStruct_10, arg_15, arg_1_10, copyOfStruct_11, arg_16, arg_1_11;
        let box_1;
        const a_1 = Pnt_$ctor_Z7AD9E565(10, 5, 3);
        const b_1 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        let minX_2 = a_1.X;
        let maxX_2;
        if (b_1.X > minX_2) {
            maxX_2 = b_1.X;
        }
        else {
            minX_2 = b_1.X;
            maxX_2 = a_1.X;
        }
        let minY_2 = a_1.Y;
        let maxY_2;
        if (b_1.Y > minY_2) {
            maxY_2 = b_1.Y;
        }
        else {
            minY_2 = b_1.Y;
            maxY_2 = a_1.Y;
        }
        let minZ_2 = a_1.Z;
        let maxZ_2;
        if (b_1.Z > minZ_2) {
            maxZ_2 = b_1.Z;
        }
        else {
            minZ_2 = b_1.Z;
            maxZ_2 = a_1.Z;
        }
        box_1 = BBox_$ctor_76A78260(minX_2, minY_2, minZ_2, maxX_2, maxY_2, maxZ_2);
        const actual_6 = box_1.MinX;
        if ((actual_6 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_6, 0, "MinX should be 0 after sorting");
        }
        else {
            throw new Exception(contains((copyOfStruct_6 = actual_6, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_6) => (structuralHash(x_6) | 0),
            }) ? ((arg_11 = (0).toString(), (arg_1_6 = actual_6.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_11)(arg_1_6)("MinX should be 0 after sorting")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_6)("MinX should be 0 after sorting"));
        }
        const actual_7 = box_1.MinY;
        if ((actual_7 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, 0, "MinY should be 0 after sorting");
        }
        else {
            throw new Exception(contains((copyOfStruct_7 = actual_7, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_7) => (structuralHash(x_7) | 0),
            }) ? ((arg_12 = (0).toString(), (arg_1_7 = actual_7.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_12)(arg_1_7)("MinY should be 0 after sorting")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_7)("MinY should be 0 after sorting"));
        }
        const actual_8 = box_1.MinZ;
        if ((actual_8 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_8, 0, "MinZ should be 0 after sorting");
        }
        else {
            throw new Exception(contains((copyOfStruct_8 = actual_8, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_8) => (structuralHash(x_8) | 0),
            }) ? ((arg_13 = (0).toString(), (arg_1_8 = actual_8.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_13)(arg_1_8)("MinZ should be 0 after sorting")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_8)("MinZ should be 0 after sorting"));
        }
        const actual_9 = box_1.MaxX;
        if ((actual_9 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_9, 10, "MaxX should be 10 after sorting");
        }
        else {
            throw new Exception(contains((copyOfStruct_9 = actual_9, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_9) => (structuralHash(x_9) | 0),
            }) ? ((arg_14 = (10).toString(), (arg_1_9 = actual_9.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_14)(arg_1_9)("MaxX should be 10 after sorting")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_9)("MaxX should be 10 after sorting"));
        }
        const actual_10 = box_1.MaxY;
        if ((actual_10 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_10, 5, "MaxY should be 5 after sorting");
        }
        else {
            throw new Exception(contains((copyOfStruct_10 = actual_10, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_10) => (structuralHash(x_10) | 0),
            }) ? ((arg_15 = (5).toString(), (arg_1_10 = actual_10.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_15)(arg_1_10)("MaxY should be 5 after sorting")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_10)("MaxY should be 5 after sorting"));
        }
        const actual_11 = box_1.MaxZ;
        if ((actual_11 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_11, 3, "MaxZ should be 3 after sorting");
        }
        else {
            throw new Exception(contains((copyOfStruct_11 = actual_11, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_11) => (structuralHash(x_11) | 0),
            }) ? ((arg_16 = (3).toString(), (arg_1_11 = actual_11.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_16)(arg_1_11)("MaxZ should be 3 after sorting")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_11)("MaxZ should be 3 after sorting"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create from two points with mixed ordering", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let copyOfStruct_12, arg_17, arg_1_12, copyOfStruct_13, arg_18, arg_1_13, copyOfStruct_14, arg_19, arg_1_14, copyOfStruct_15, arg_20, arg_1_15, copyOfStruct_16, arg_21, arg_1_16, copyOfStruct_17, arg_22, arg_1_17;
        let box_2;
        const a_2 = Pnt_$ctor_Z7AD9E565(10, 0, 3);
        const b_2 = Pnt_$ctor_Z7AD9E565(0, 5, 0);
        let minX_4 = a_2.X;
        let maxX_4;
        if (b_2.X > minX_4) {
            maxX_4 = b_2.X;
        }
        else {
            minX_4 = b_2.X;
            maxX_4 = a_2.X;
        }
        let minY_4 = a_2.Y;
        let maxY_4;
        if (b_2.Y > minY_4) {
            maxY_4 = b_2.Y;
        }
        else {
            minY_4 = b_2.Y;
            maxY_4 = a_2.Y;
        }
        let minZ_4 = a_2.Z;
        let maxZ_4;
        if (b_2.Z > minZ_4) {
            maxZ_4 = b_2.Z;
        }
        else {
            minZ_4 = b_2.Z;
            maxZ_4 = a_2.Z;
        }
        box_2 = BBox_$ctor_76A78260(minX_4, minY_4, minZ_4, maxX_4, maxY_4, maxZ_4);
        const actual_12 = box_2.MinX;
        if ((actual_12 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_12, 0, "MinX should be 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_12 = actual_12, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_12) => (structuralHash(x_12) | 0),
            }) ? ((arg_17 = (0).toString(), (arg_1_12 = actual_12.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_17)(arg_1_12)("MinX should be 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_12)("MinX should be 0"));
        }
        const actual_13 = box_2.MinY;
        if ((actual_13 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_13, 0, "MinY should be 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_13 = actual_13, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_13) => (structuralHash(x_13) | 0),
            }) ? ((arg_18 = (0).toString(), (arg_1_13 = actual_13.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_18)(arg_1_13)("MinY should be 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_13)("MinY should be 0"));
        }
        const actual_14 = box_2.MinZ;
        if ((actual_14 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_14, 0, "MinZ should be 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_14 = actual_14, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_14) => (structuralHash(x_14) | 0),
            }) ? ((arg_19 = (0).toString(), (arg_1_14 = actual_14.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_19)(arg_1_14)("MinZ should be 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_14)("MinZ should be 0"));
        }
        const actual_15 = box_2.MaxX;
        if ((actual_15 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_15, 10, "MaxX should be 10");
        }
        else {
            throw new Exception(contains((copyOfStruct_15 = actual_15, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_15) => (structuralHash(x_15) | 0),
            }) ? ((arg_20 = (10).toString(), (arg_1_15 = actual_15.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_20)(arg_1_15)("MaxX should be 10")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_15)("MaxX should be 10"));
        }
        const actual_16 = box_2.MaxY;
        if ((actual_16 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_16, 5, "MaxY should be 5");
        }
        else {
            throw new Exception(contains((copyOfStruct_16 = actual_16, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_16) => (structuralHash(x_16) | 0),
            }) ? ((arg_21 = (5).toString(), (arg_1_16 = actual_16.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_21)(arg_1_16)("MaxY should be 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_16)("MaxY should be 5"));
        }
        const actual_17 = box_2.MaxZ;
        if ((actual_17 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_17, 3, "MaxZ should be 3");
        }
        else {
            throw new Exception(contains((copyOfStruct_17 = actual_17, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_17) => (structuralHash(x_17) | 0),
            }) ? ((arg_22 = (3).toString(), (arg_1_17 = actual_17.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_22)(arg_1_17)("MaxZ should be 3")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_17)("MaxZ should be 3"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("MinPnt and MaxPnt properties", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let a_5, b_4, b_6, x_18, y_18, z, a_7, b_7, b_9, x_19, y_19, z_1;
        let box_3;
        const a_3 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        const b_3 = Pnt_$ctor_Z7AD9E565(5, 7, 9);
        let minX_6 = a_3.X;
        let maxX_6;
        if (b_3.X > minX_6) {
            maxX_6 = b_3.X;
        }
        else {
            minX_6 = b_3.X;
            maxX_6 = a_3.X;
        }
        let minY_6 = a_3.Y;
        let maxY_6;
        if (b_3.Y > minY_6) {
            maxY_6 = b_3.Y;
        }
        else {
            minY_6 = b_3.Y;
            maxY_6 = a_3.Y;
        }
        let minZ_6 = a_3.Z;
        let maxZ_6;
        if (b_3.Z > minZ_6) {
            maxZ_6 = b_3.Z;
        }
        else {
            minZ_6 = b_3.Z;
            maxZ_6 = a_3.Z;
        }
        box_3 = BBox_$ctor_76A78260(minX_6, minY_6, minZ_6, maxX_6, maxY_6, maxZ_6);
        Expect_isTrue(((a_5 = ((b_4 = box_3, Pnt_$ctor_Z7AD9E565_1(b_4.MinX, b_4.MinY, b_4.MinZ))), (b_6 = Pnt_$ctor_Z7AD9E565(1, 2, 3), (x_18 = (a_5.X - b_6.X), (y_18 = (a_5.Y - b_6.Y), (z = (a_5.Z - b_6.Z), Math.sqrt(((x_18 * x_18) + (y_18 * y_18)) + (z * z)))))))) < 1E-09)("MinPnt should be (1, 2, 3)");
        Expect_isTrue(((a_7 = ((b_7 = box_3, Pnt_$ctor_Z7AD9E565_1(b_7.MaxX, b_7.MaxY, b_7.MaxZ))), (b_9 = Pnt_$ctor_Z7AD9E565(5, 7, 9), (x_19 = (a_7.X - b_9.X), (y_19 = (a_7.Y - b_9.Y), (z_1 = (a_7.Z - b_9.Z), Math.sqrt(((x_19 * x_19) + (y_19 * y_19)) + (z_1 * z_1)))))))) < 1E-09)("MaxPnt should be (5, 7, 9)");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("SizeX, SizeY, SizeZ", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let copyOfStruct_18, arg_23, arg_1_18, copyOfStruct_19, arg_24, arg_1_19, copyOfStruct_20, arg_25, arg_1_20;
        let box_4;
        const a_8 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        const b_10 = Pnt_$ctor_Z7AD9E565(5, 7, 9);
        let minX_8 = a_8.X;
        let maxX_8;
        if (b_10.X > minX_8) {
            maxX_8 = b_10.X;
        }
        else {
            minX_8 = b_10.X;
            maxX_8 = a_8.X;
        }
        let minY_8 = a_8.Y;
        let maxY_8;
        if (b_10.Y > minY_8) {
            maxY_8 = b_10.Y;
        }
        else {
            minY_8 = b_10.Y;
            maxY_8 = a_8.Y;
        }
        let minZ_8 = a_8.Z;
        let maxZ_8;
        if (b_10.Z > minZ_8) {
            maxZ_8 = b_10.Z;
        }
        else {
            minZ_8 = b_10.Z;
            maxZ_8 = a_8.Z;
        }
        box_4 = BBox_$ctor_76A78260(minX_8, minY_8, minZ_8, maxX_8, maxY_8, maxZ_8);
        let actual_18;
        const b_11 = box_4;
        actual_18 = (b_11.MaxX - b_11.MinX);
        if ((actual_18 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_18, 4, "SizeX should be 4");
        }
        else {
            throw new Exception(contains((copyOfStruct_18 = actual_18, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_20) => (structuralHash(x_20) | 0),
            }) ? ((arg_23 = (4).toString(), (arg_1_18 = actual_18.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_23)(arg_1_18)("SizeX should be 4")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_18)("SizeX should be 4"));
        }
        let actual_19;
        const b_12 = box_4;
        actual_19 = (b_12.MaxY - b_12.MinY);
        if ((actual_19 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_19, 5, "SizeY should be 5");
        }
        else {
            throw new Exception(contains((copyOfStruct_19 = actual_19, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_21) => (structuralHash(x_21) | 0),
            }) ? ((arg_24 = (5).toString(), (arg_1_19 = actual_19.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_24)(arg_1_19)("SizeY should be 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_19)("SizeY should be 5"));
        }
        let actual_20;
        const b_13 = box_4;
        actual_20 = (b_13.MaxZ - b_13.MinZ);
        if ((actual_20 === 6) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_20, 6, "SizeZ should be 6");
        }
        else {
            throw new Exception(contains((copyOfStruct_20 = actual_20, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_22) => (structuralHash(x_22) | 0),
            }) ? ((arg_25 = (6).toString(), (arg_1_20 = actual_20.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_25)(arg_1_20)("SizeZ should be 6")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(6)(actual_20)("SizeZ should be 6"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Center", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let a_11, b_15, a_9, b_14, minX_10, maxX_10, minY_10, maxY_10, minZ_10, maxZ_10, b_17, x_23, y_23, z_2;
        Expect_isTrue(((a_11 = ((b_15 = ((a_9 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_14 = Pnt_$ctor_Z7AD9E565(10, 6, 4), (minX_10 = a_9.X, (maxX_10 = ((b_14.X > minX_10) ? b_14.X : ((minX_10 = b_14.X, a_9.X))), (minY_10 = a_9.Y, (maxY_10 = ((b_14.Y > minY_10) ? b_14.Y : ((minY_10 = b_14.Y, a_9.Y))), (minZ_10 = a_9.Z, (maxZ_10 = ((b_14.Z > minZ_10) ? b_14.Z : ((minZ_10 = b_14.Z, a_9.Z))), BBox_$ctor_76A78260(minX_10, minY_10, minZ_10, maxX_10, maxY_10, maxZ_10)))))))))), Pnt_$ctor_Z7AD9E565_1((b_15.MaxX + b_15.MinX) * 0.5, (b_15.MaxY + b_15.MinY) * 0.5, (b_15.MaxZ + b_15.MinZ) * 0.5))), (b_17 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_23 = (a_11.X - b_17.X), (y_23 = (a_11.Y - b_17.Y), (z_2 = (a_11.Z - b_17.Z), Math.sqrt(((x_23 * x_23) + (y_23 * y_23)) + (z_2 * z_2)))))))) < 1E-09)("Center should be (5, 3, 2)");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Diagonal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        let v_1, a_13, b_19, a_12, b_18, minX_12, maxX_12, minY_12, maxY_12, minZ_12, maxZ_12, b_20;
        Expect_isTrue(((v_1 = ((a_13 = ((b_19 = ((a_12 = Pnt_$ctor_Z7AD9E565(1, 2, 3), (b_18 = Pnt_$ctor_Z7AD9E565(5, 7, 9), (minX_12 = a_12.X, (maxX_12 = ((b_18.X > minX_12) ? b_18.X : ((minX_12 = b_18.X, a_12.X))), (minY_12 = a_12.Y, (maxY_12 = ((b_18.Y > minY_12) ? b_18.Y : ((minY_12 = b_18.Y, a_12.Y))), (minZ_12 = a_12.Z, (maxZ_12 = ((b_18.Z > minZ_12) ? b_18.Z : ((minZ_12 = b_18.Z, a_12.Z))), BBox_$ctor_76A78260(minX_12, minY_12, minZ_12, maxX_12, maxY_12, maxZ_12)))))))))), Vec_$ctor_Z7AD9E565(b_19.MaxX - b_19.MinX, b_19.MaxY - b_19.MinY, b_19.MaxZ - b_19.MinZ))), (b_20 = Vec_$ctor_Z7AD9E565_1(4, 5, 6), Vec_$ctor_Z7AD9E565(a_13.X - b_20.X, a_13.Y - b_20.Y, a_13.Z - b_20.Z)))), Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z)))) < 1E-09)("Diagonal should be (4, 5, 6)");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Volume", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let b_24, b_25, b_26, copyOfStruct_21, arg_26, arg_1_21;
        let actual_21;
        let b_23;
        const a_15 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_22 = Pnt_$ctor_Z7AD9E565(10, 5, 2);
        let minX_14 = a_15.X;
        let maxX_14;
        if (b_22.X > minX_14) {
            maxX_14 = b_22.X;
        }
        else {
            minX_14 = b_22.X;
            maxX_14 = a_15.X;
        }
        let minY_14 = a_15.Y;
        let maxY_14;
        if (b_22.Y > minY_14) {
            maxY_14 = b_22.Y;
        }
        else {
            minY_14 = b_22.Y;
            maxY_14 = a_15.Y;
        }
        let minZ_14 = a_15.Z;
        let maxZ_14;
        if (b_22.Z > minZ_14) {
            maxZ_14 = b_22.Z;
        }
        else {
            minZ_14 = b_22.Z;
            maxZ_14 = a_15.Z;
        }
        b_23 = BBox_$ctor_76A78260(minX_14, minY_14, minZ_14, maxX_14, maxY_14, maxZ_14);
        actual_21 = ((((b_24 = b_23, b_24.MaxX - b_24.MinX)) * ((b_25 = b_23, b_25.MaxY - b_25.MinY))) * ((b_26 = b_23, b_26.MaxZ - b_26.MinZ)));
        if ((actual_21 === 100) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_21, 100, "Volume should be 100");
        }
        else {
            throw new Exception(contains((copyOfStruct_21 = actual_21, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_24) => (structuralHash(x_24) | 0),
            }) ? ((arg_26 = (100).toString(), (arg_1_21 = actual_21.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_26)(arg_1_21)("Volume should be 100")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(100)(actual_21)("Volume should be 100"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Volume static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let b_29, b_30, b_31, copyOfStruct_22, arg_27, arg_1_22;
        let actual_22;
        let b_28;
        const a_16 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_27 = Pnt_$ctor_Z7AD9E565(10, 5, 2);
        let minX_16 = a_16.X;
        let maxX_16;
        if (b_27.X > minX_16) {
            maxX_16 = b_27.X;
        }
        else {
            minX_16 = b_27.X;
            maxX_16 = a_16.X;
        }
        let minY_16 = a_16.Y;
        let maxY_16;
        if (b_27.Y > minY_16) {
            maxY_16 = b_27.Y;
        }
        else {
            minY_16 = b_27.Y;
            maxY_16 = a_16.Y;
        }
        let minZ_16 = a_16.Z;
        let maxZ_16;
        if (b_27.Z > minZ_16) {
            maxZ_16 = b_27.Z;
        }
        else {
            minZ_16 = b_27.Z;
            maxZ_16 = a_16.Z;
        }
        b_28 = BBox_$ctor_76A78260(minX_16, minY_16, minZ_16, maxX_16, maxY_16, maxZ_16);
        actual_22 = ((((b_29 = b_28, b_29.MaxX - b_29.MinX)) * ((b_30 = b_28, b_30.MaxY - b_30.MinY))) * ((b_31 = b_28, b_31.MaxZ - b_31.MinZ)));
        if ((actual_22 === 100) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_22, 100, "Volume should be 100");
        }
        else {
            throw new Exception(contains((copyOfStruct_22 = actual_22, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_25) => (structuralHash(x_25) | 0),
            }) ? ((arg_27 = (100).toString(), (arg_1_22 = actual_22.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_27)(arg_1_22)("Volume should be 100")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(100)(actual_22)("Volume should be 100"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})()])), Test_testList("Creation Methods", ofArray([(() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromSeq with valid points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        let copyOfStruct_23, arg_28, arg_1_23, copyOfStruct_24, arg_29, arg_1_24, copyOfStruct_25, arg_30, arg_1_25, copyOfStruct_26, arg_31, arg_1_26, copyOfStruct_27, arg_32, arg_1_27, copyOfStruct_28, arg_33, arg_1_28;
        let box_9;
        const ps = [Pnt_$ctor_Z7AD9E565(1, 2, 3), Pnt_$ctor_Z7AD9E565(5, 3, 1), Pnt_$ctor_Z7AD9E565(2, 7, 5), Pnt_$ctor_Z7AD9E565(8, 1, 2)];
        if (Operators_IsNull(ps)) {
            failNull("BBox.createFromSeq", "seq<Pnt>");
        }
        if (isEmpty(ps)) {
            failEmptySeq("BBox.createFromSeq", "seq<Pnt>");
        }
        let minX_18 = 1.7976931348623157E+308;
        let minY_18 = 1.7976931348623157E+308;
        let minZ_18 = 1.7976931348623157E+308;
        let maxX_18 = -1.7976931348623157E+308;
        let maxY_18 = -1.7976931348623157E+308;
        let maxZ_18 = -1.7976931348623157E+308;
        const enumerator = getEnumerator(ps);
        try {
            while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
                const p = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
                minX_18 = min(minX_18, p.X);
                minY_18 = min(minY_18, p.Y);
                minZ_18 = min(minZ_18, p.Z);
                maxX_18 = max(maxX_18, p.X);
                maxY_18 = max(maxY_18, p.Y);
                maxZ_18 = max(maxZ_18, p.Z);
            }
        }
        finally {
            disposeSafe(enumerator);
        }
        box_9 = BBox_$ctor_76A78260(minX_18, minY_18, minZ_18, maxX_18, maxY_18, maxZ_18);
        const actual_23 = box_9.MinX;
        if ((actual_23 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_23, 1, "MinX should be 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_23 = actual_23, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_26) => (structuralHash(x_26) | 0),
            }) ? ((arg_28 = (1).toString(), (arg_1_23 = actual_23.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_28)(arg_1_23)("MinX should be 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_23)("MinX should be 1"));
        }
        const actual_24 = box_9.MinY;
        if ((actual_24 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_24, 1, "MinY should be 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_24 = actual_24, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_27) => (structuralHash(x_27) | 0),
            }) ? ((arg_29 = (1).toString(), (arg_1_24 = actual_24.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_29)(arg_1_24)("MinY should be 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_24)("MinY should be 1"));
        }
        const actual_25 = box_9.MinZ;
        if ((actual_25 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_25, 1, "MinZ should be 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_25 = actual_25, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_28) => (structuralHash(x_28) | 0),
            }) ? ((arg_30 = (1).toString(), (arg_1_25 = actual_25.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_30)(arg_1_25)("MinZ should be 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_25)("MinZ should be 1"));
        }
        const actual_26 = box_9.MaxX;
        if ((actual_26 === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_26, 8, "MaxX should be 8");
        }
        else {
            throw new Exception(contains((copyOfStruct_26 = actual_26, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_29) => (structuralHash(x_29) | 0),
            }) ? ((arg_31 = (8).toString(), (arg_1_26 = actual_26.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_31)(arg_1_26)("MaxX should be 8")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual_26)("MaxX should be 8"));
        }
        const actual_27 = box_9.MaxY;
        if ((actual_27 === 7) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_27, 7, "MaxY should be 7");
        }
        else {
            throw new Exception(contains((copyOfStruct_27 = actual_27, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_30) => (structuralHash(x_30) | 0),
            }) ? ((arg_32 = (7).toString(), (arg_1_27 = actual_27.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_32)(arg_1_27)("MaxY should be 7")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(7)(actual_27)("MaxY should be 7"));
        }
        const actual_28 = box_9.MaxZ;
        if ((actual_28 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_28, 5, "MaxZ should be 5");
        }
        else {
            throw new Exception(contains((copyOfStruct_28 = actual_28, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_31) => (structuralHash(x_31) | 0),
            }) ? ((arg_33 = (5).toString(), (arg_1_28 = actual_28.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_33)(arg_1_28)("MaxZ should be 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_28)("MaxZ should be 5"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromSeq with single point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let copyOfStruct_29, arg_34, arg_1_29, copyOfStruct_30, arg_35, arg_1_30, copyOfStruct_31, arg_36, arg_1_31, copyOfStruct_32, arg_37, arg_1_32, copyOfStruct_33, arg_38, arg_1_33, copyOfStruct_34, arg_39, arg_1_34;
        let box_10;
        const ps_1 = [Pnt_$ctor_Z7AD9E565(5, 3, 2)];
        if (Operators_IsNull(ps_1)) {
            failNull("BBox.createFromSeq", "seq<Pnt>");
        }
        if (isEmpty(ps_1)) {
            failEmptySeq("BBox.createFromSeq", "seq<Pnt>");
        }
        let minX_20 = 1.7976931348623157E+308;
        let minY_20 = 1.7976931348623157E+308;
        let minZ_20 = 1.7976931348623157E+308;
        let maxX_20 = -1.7976931348623157E+308;
        let maxY_20 = -1.7976931348623157E+308;
        let maxZ_20 = -1.7976931348623157E+308;
        const enumerator_1 = getEnumerator(ps_1);
        try {
            while (enumerator_1["System.Collections.IEnumerator.MoveNext"]()) {
                const p_1 = enumerator_1["System.Collections.Generic.IEnumerator`1.get_Current"]();
                minX_20 = min(minX_20, p_1.X);
                minY_20 = min(minY_20, p_1.Y);
                minZ_20 = min(minZ_20, p_1.Z);
                maxX_20 = max(maxX_20, p_1.X);
                maxY_20 = max(maxY_20, p_1.Y);
                maxZ_20 = max(maxZ_20, p_1.Z);
            }
        }
        finally {
            disposeSafe(enumerator_1);
        }
        box_10 = BBox_$ctor_76A78260(minX_20, minY_20, minZ_20, maxX_20, maxY_20, maxZ_20);
        const actual_29 = box_10.MinX;
        if ((actual_29 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_29, 5, "MinX should be 5");
        }
        else {
            throw new Exception(contains((copyOfStruct_29 = actual_29, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_32) => (structuralHash(x_32) | 0),
            }) ? ((arg_34 = (5).toString(), (arg_1_29 = actual_29.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_34)(arg_1_29)("MinX should be 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_29)("MinX should be 5"));
        }
        const actual_30 = box_10.MinY;
        if ((actual_30 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_30, 3, "MinY should be 3");
        }
        else {
            throw new Exception(contains((copyOfStruct_30 = actual_30, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_33) => (structuralHash(x_33) | 0),
            }) ? ((arg_35 = (3).toString(), (arg_1_30 = actual_30.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_35)(arg_1_30)("MinY should be 3")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_30)("MinY should be 3"));
        }
        const actual_31 = box_10.MinZ;
        if ((actual_31 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_31, 2, "MinZ should be 2");
        }
        else {
            throw new Exception(contains((copyOfStruct_31 = actual_31, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_34) => (structuralHash(x_34) | 0),
            }) ? ((arg_36 = (2).toString(), (arg_1_31 = actual_31.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_36)(arg_1_31)("MinZ should be 2")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_31)("MinZ should be 2"));
        }
        const actual_32 = box_10.MaxX;
        if ((actual_32 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_32, 5, "MaxX should be 5");
        }
        else {
            throw new Exception(contains((copyOfStruct_32 = actual_32, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_35) => (structuralHash(x_35) | 0),
            }) ? ((arg_37 = (5).toString(), (arg_1_32 = actual_32.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_37)(arg_1_32)("MaxX should be 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_32)("MaxX should be 5"));
        }
        const actual_33 = box_10.MaxY;
        if ((actual_33 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_33, 3, "MaxY should be 3");
        }
        else {
            throw new Exception(contains((copyOfStruct_33 = actual_33, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_36) => (structuralHash(x_36) | 0),
            }) ? ((arg_38 = (3).toString(), (arg_1_33 = actual_33.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_38)(arg_1_33)("MaxY should be 3")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_33)("MaxY should be 3"));
        }
        const actual_34 = box_10.MaxZ;
        if ((actual_34 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_34, 2, "MaxZ should be 2");
        }
        else {
            throw new Exception(contains((copyOfStruct_34 = actual_34, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_37) => (structuralHash(x_37) | 0),
            }) ? ((arg_39 = (2).toString(), (arg_1_34 = actual_34.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_39)(arg_1_34)("MaxZ should be 2")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_34)("MaxZ should be 2"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromSeq throws on empty sequence", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        Expect_throws(() => {
            let ps_2, minX_22, minY_22, minZ_22, maxX_22, maxY_22, maxZ_22, enumerator_2;
            (ps_2 = [], (Operators_IsNull(ps_2) ? failNull("BBox.createFromSeq", "seq<Pnt>") : undefined, (isEmpty(ps_2) ? failEmptySeq("BBox.createFromSeq", "seq<Pnt>") : undefined, (minX_22 = 1.7976931348623157E+308, (minY_22 = 1.7976931348623157E+308, (minZ_22 = 1.7976931348623157E+308, (maxX_22 = -1.7976931348623157E+308, (maxY_22 = -1.7976931348623157E+308, (maxZ_22 = -1.7976931348623157E+308, ((enumerator_2 = getEnumerator(ps_2), (() => {
                try {
                    while (enumerator_2["System.Collections.IEnumerator.MoveNext"]()) {
                        const p_2 = enumerator_2["System.Collections.Generic.IEnumerator`1.get_Current"]();
                        minX_22 = min(minX_22, p_2.X);
                        minY_22 = min(minY_22, p_2.Y);
                        minZ_22 = min(minZ_22, p_2.Z);
                        maxX_22 = max(maxX_22, p_2.X);
                        maxY_22 = max(maxY_22, p_2.Y);
                        maxZ_22 = max(maxZ_22, p_2.Z);
                    }
                }
                finally {
                    disposeSafe(enumerator_2);
                }
            })()), BBox_$ctor_76A78260(minX_22, minY_22, minZ_22, maxX_22, maxY_22, maxZ_22)))))))))));
        }, "Should throw on empty sequence");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromIList with valid points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        let copyOfStruct_35, arg_40, arg_1_35, copyOfStruct_36, arg_41, arg_1_36, copyOfStruct_37, arg_42, arg_1_37;
        let box_11;
        const ps_3 = [Pnt_$ctor_Z7AD9E565(1, 2, 3), Pnt_$ctor_Z7AD9E565(5, 3, 1), Pnt_$ctor_Z7AD9E565(2, 7, 5)];
        if (Operators_IsNull(ps_3)) {
            failNull("BBox.createFromIList", "IList<Pnt>");
        }
        if (count(ps_3) === 0) {
            failEmptySeq("BBox.createFromIList", "IList<Pnt>");
        }
        let minX_24 = 1.7976931348623157E+308;
        let minY_24 = 1.7976931348623157E+308;
        let minZ_24 = 1.7976931348623157E+308;
        let maxX_24 = -1.7976931348623157E+308;
        let maxY_24 = -1.7976931348623157E+308;
        let maxZ_24 = -1.7976931348623157E+308;
        for (let i = 0; i <= (count(ps_3) - 1); i++) {
            const p_3 = item(i, ps_3);
            minX_24 = min(minX_24, p_3.X);
            minY_24 = min(minY_24, p_3.Y);
            minZ_24 = min(minZ_24, p_3.Z);
            maxX_24 = max(maxX_24, p_3.X);
            maxY_24 = max(maxY_24, p_3.Y);
            maxZ_24 = max(maxZ_24, p_3.Z);
        }
        box_11 = BBox_$ctor_76A78260(minX_24, minY_24, minZ_24, maxX_24, maxY_24, maxZ_24);
        const actual_35 = box_11.MinX;
        if ((actual_35 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_35, 1, "MinX should be 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_35 = actual_35, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_38) => (structuralHash(x_38) | 0),
            }) ? ((arg_40 = (1).toString(), (arg_1_35 = actual_35.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_40)(arg_1_35)("MinX should be 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_35)("MinX should be 1"));
        }
        const actual_36 = box_11.MaxY;
        if ((actual_36 === 7) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_36, 7, "MaxY should be 7");
        }
        else {
            throw new Exception(contains((copyOfStruct_36 = actual_36, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_39) => (structuralHash(x_39) | 0),
            }) ? ((arg_41 = (7).toString(), (arg_1_36 = actual_36.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_41)(arg_1_36)("MaxY should be 7")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(7)(actual_36)("MaxY should be 7"));
        }
        const actual_37 = box_11.MaxZ;
        if ((actual_37 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_37, 5, "MaxZ should be 5");
        }
        else {
            throw new Exception(contains((copyOfStruct_37 = actual_37, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_40) => (structuralHash(x_40) | 0),
            }) ? ((arg_42 = (5).toString(), (arg_1_37 = actual_37.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_42)(arg_1_37)("MaxZ should be 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_37)("MaxZ should be 5"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromIList throws on empty list", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        Expect_throws(() => {
            let ps_4, minX_26, minY_26, minZ_26, maxX_26, maxY_26, maxZ_26;
            (ps_4 = [], (Operators_IsNull(ps_4) ? failNull("BBox.createFromIList", "IList<Pnt>") : undefined, ((count(ps_4) === 0) ? failEmptySeq("BBox.createFromIList", "IList<Pnt>") : undefined, (minX_26 = 1.7976931348623157E+308, (minY_26 = 1.7976931348623157E+308, (minZ_26 = 1.7976931348623157E+308, (maxX_26 = -1.7976931348623157E+308, (maxY_26 = -1.7976931348623157E+308, (maxZ_26 = -1.7976931348623157E+308, ((() => {
                for (let i_1 = 0; i_1 <= (count(ps_4) - 1); i_1++) {
                    const p_4 = item(i_1, ps_4);
                    minX_26 = min(minX_26, p_4.X);
                    minY_26 = min(minY_26, p_4.Y);
                    minZ_26 = min(minZ_26, p_4.Z);
                    maxX_26 = max(maxX_26, p_4.X);
                    maxY_26 = max(maxY_26, p_4.Y);
                    maxZ_26 = max(maxZ_26, p_4.Z);
                }
            })(), BBox_$ctor_76A78260(minX_26, minY_26, minZ_26, maxX_26, maxY_26, maxZ_26)))))))))));
        }, "Should throw on empty list");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromCenter with valid size", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let copyOfStruct_38, arg_43, arg_1_38, copyOfStruct_39, arg_44, arg_1_39, copyOfStruct_40, arg_45, arg_1_40, copyOfStruct_41, arg_46, arg_1_41, copyOfStruct_42, arg_47, arg_1_42, copyOfStruct_43, arg_48, arg_1_43;
        let box_12;
        const center = Pnt_$ctor_Z7AD9E565(5, 5, 5);
        if (!(10 >= 0)) {
            fail(`BBox.createFromCenter sizeX is negative: ${10}, sizeY is: ${6}, sizeZ is: ${4}, center: ${Pnt__get_AsString(center)}`);
        }
        if (!(6 >= 0)) {
            fail(`BBox.createFromCenter sizeY is negative: ${6}, sizeX is: ${10}, sizeZ is: ${4}, center: ${Pnt__get_AsString(center)}`);
        }
        if (!(4 >= 0)) {
            fail(`BBox.createFromCenter sizeZ is negative: ${4}, sizeX is: ${10}, sizeY is: ${6}, center: ${Pnt__get_AsString(center)}`);
        }
        box_12 = BBox_$ctor_76A78260(center.X - (10 * 0.5), center.Y - (6 * 0.5), center.Z - (4 * 0.5), center.X + (10 * 0.5), center.Y + (6 * 0.5), center.Z + (4 * 0.5));
        const actual_38 = box_12.MinX;
        if ((actual_38 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_38, 0, "MinX should be 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_38 = actual_38, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_44) => (structuralHash(x_44) | 0),
            }) ? ((arg_43 = (0).toString(), (arg_1_38 = actual_38.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_43)(arg_1_38)("MinX should be 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_38)("MinX should be 0"));
        }
        const actual_39 = box_12.MinY;
        if ((actual_39 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_39, 2, "MinY should be 2");
        }
        else {
            throw new Exception(contains((copyOfStruct_39 = actual_39, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_45) => (structuralHash(x_45) | 0),
            }) ? ((arg_44 = (2).toString(), (arg_1_39 = actual_39.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_44)(arg_1_39)("MinY should be 2")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_39)("MinY should be 2"));
        }
        const actual_40 = box_12.MinZ;
        if ((actual_40 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_40, 3, "MinZ should be 3");
        }
        else {
            throw new Exception(contains((copyOfStruct_40 = actual_40, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_46) => (structuralHash(x_46) | 0),
            }) ? ((arg_45 = (3).toString(), (arg_1_40 = actual_40.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_45)(arg_1_40)("MinZ should be 3")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_40)("MinZ should be 3"));
        }
        const actual_41 = box_12.MaxX;
        if ((actual_41 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_41, 10, "MaxX should be 10");
        }
        else {
            throw new Exception(contains((copyOfStruct_41 = actual_41, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_47) => (structuralHash(x_47) | 0),
            }) ? ((arg_46 = (10).toString(), (arg_1_41 = actual_41.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_46)(arg_1_41)("MaxX should be 10")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_41)("MaxX should be 10"));
        }
        const actual_42 = box_12.MaxY;
        if ((actual_42 === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_42, 8, "MaxY should be 8");
        }
        else {
            throw new Exception(contains((copyOfStruct_42 = actual_42, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_48) => (structuralHash(x_48) | 0),
            }) ? ((arg_47 = (8).toString(), (arg_1_42 = actual_42.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_47)(arg_1_42)("MaxY should be 8")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual_42)("MaxY should be 8"));
        }
        const actual_43 = box_12.MaxZ;
        if ((actual_43 === 7) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_43, 7, "MaxZ should be 7");
        }
        else {
            throw new Exception(contains((copyOfStruct_43 = actual_43, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_49) => (structuralHash(x_49) | 0),
            }) ? ((arg_48 = (7).toString(), (arg_1_43 = actual_43.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_48)(arg_1_43)("MaxZ should be 7")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(7)(actual_43)("MaxZ should be 7"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromCenter rejects negative sizeX", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        Expect_throws(() => {
            let center_1;
            (center_1 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (!(-10 >= 0) ? fail(`BBox.createFromCenter sizeX is negative: ${-10}, sizeY is: ${6}, sizeZ is: ${4}, center: ${Pnt__get_AsString(center_1)}`) : undefined, (!(6 >= 0) ? fail(`BBox.createFromCenter sizeY is negative: ${6}, sizeX is: ${-10}, sizeZ is: ${4}, center: ${Pnt__get_AsString(center_1)}`) : undefined, (!(4 >= 0) ? fail(`BBox.createFromCenter sizeZ is negative: ${4}, sizeX is: ${-10}, sizeY is: ${6}, center: ${Pnt__get_AsString(center_1)}`) : undefined, BBox_$ctor_76A78260(center_1.X - (-10 * 0.5), center_1.Y - (6 * 0.5), center_1.Z - (4 * 0.5), center_1.X + (-10 * 0.5), center_1.Y + (6 * 0.5), center_1.Z + (4 * 0.5))))));
        }, "Should throw on negative sizeX");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromCenter rejects negative sizeY", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        Expect_throws(() => {
            let center_2;
            (center_2 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (!(10 >= 0) ? fail(`BBox.createFromCenter sizeX is negative: ${10}, sizeY is: ${-6}, sizeZ is: ${4}, center: ${Pnt__get_AsString(center_2)}`) : undefined, (!(-6 >= 0) ? fail(`BBox.createFromCenter sizeY is negative: ${-6}, sizeX is: ${10}, sizeZ is: ${4}, center: ${Pnt__get_AsString(center_2)}`) : undefined, (!(4 >= 0) ? fail(`BBox.createFromCenter sizeZ is negative: ${4}, sizeX is: ${10}, sizeY is: ${-6}, center: ${Pnt__get_AsString(center_2)}`) : undefined, BBox_$ctor_76A78260(center_2.X - (10 * 0.5), center_2.Y - (-6 * 0.5), center_2.Z - (4 * 0.5), center_2.X + (10 * 0.5), center_2.Y + (-6 * 0.5), center_2.Z + (4 * 0.5))))));
        }, "Should throw on negative sizeY");
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromCenter rejects negative sizeZ", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        Expect_throws(() => {
            let center_3;
            (center_3 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (!(10 >= 0) ? fail(`BBox.createFromCenter sizeX is negative: ${10}, sizeY is: ${6}, sizeZ is: ${-4}, center: ${Pnt__get_AsString(center_3)}`) : undefined, (!(6 >= 0) ? fail(`BBox.createFromCenter sizeY is negative: ${6}, sizeX is: ${10}, sizeZ is: ${-4}, center: ${Pnt__get_AsString(center_3)}`) : undefined, (!(-4 >= 0) ? fail(`BBox.createFromCenter sizeZ is negative: ${-4}, sizeX is: ${10}, sizeY is: ${6}, center: ${Pnt__get_AsString(center_3)}`) : undefined, BBox_$ctor_76A78260(center_3.X - (10 * 0.5), center_3.Y - (6 * 0.5), center_3.Z - (-4 * 0.5), center_3.X + (10 * 0.5), center_3.Y + (6 * 0.5), center_3.Z + (-4 * 0.5))))));
        }, "Should throw on negative sizeZ");
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromLine", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        let copyOfStruct_44, arg_49, arg_1_44, copyOfStruct_45, arg_50, arg_1_45, copyOfStruct_46, arg_51, arg_1_46, copyOfStruct_47, arg_52, arg_1_47, copyOfStruct_48, arg_53, arg_1_48, copyOfStruct_49, arg_54, arg_1_49;
        let box_13;
        const l = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(1, 2, 3), Pnt_$ctor_Z7AD9E565(5, 7, 9));
        const minX_36 = min(l.FromX, l.ToX);
        const maxX_36 = max(l.FromX, l.ToX);
        const minY_36 = min(l.FromY, l.ToY);
        const maxY_36 = max(l.FromY, l.ToY);
        box_13 = BBox_$ctor_76A78260(minX_36, minY_36, min(l.FromZ, l.ToZ), maxX_36, maxY_36, max(l.FromZ, l.ToZ));
        const actual_44 = box_13.MinX;
        if ((actual_44 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_44, 1, "MinX should be 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_44 = actual_44, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_59) => (structuralHash(x_59) | 0),
            }) ? ((arg_49 = (1).toString(), (arg_1_44 = actual_44.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_49)(arg_1_44)("MinX should be 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_44)("MinX should be 1"));
        }
        const actual_45 = box_13.MinY;
        if ((actual_45 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_45, 2, "MinY should be 2");
        }
        else {
            throw new Exception(contains((copyOfStruct_45 = actual_45, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_60) => (structuralHash(x_60) | 0),
            }) ? ((arg_50 = (2).toString(), (arg_1_45 = actual_45.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_50)(arg_1_45)("MinY should be 2")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_45)("MinY should be 2"));
        }
        const actual_46 = box_13.MinZ;
        if ((actual_46 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_46, 3, "MinZ should be 3");
        }
        else {
            throw new Exception(contains((copyOfStruct_46 = actual_46, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_61) => (structuralHash(x_61) | 0),
            }) ? ((arg_51 = (3).toString(), (arg_1_46 = actual_46.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_51)(arg_1_46)("MinZ should be 3")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_46)("MinZ should be 3"));
        }
        const actual_47 = box_13.MaxX;
        if ((actual_47 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_47, 5, "MaxX should be 5");
        }
        else {
            throw new Exception(contains((copyOfStruct_47 = actual_47, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_62) => (structuralHash(x_62) | 0),
            }) ? ((arg_52 = (5).toString(), (arg_1_47 = actual_47.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_52)(arg_1_47)("MaxX should be 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_47)("MaxX should be 5"));
        }
        const actual_48 = box_13.MaxY;
        if ((actual_48 === 7) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_48, 7, "MaxY should be 7");
        }
        else {
            throw new Exception(contains((copyOfStruct_48 = actual_48, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_63) => (structuralHash(x_63) | 0),
            }) ? ((arg_53 = (7).toString(), (arg_1_48 = actual_48.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_53)(arg_1_48)("MaxY should be 7")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(7)(actual_48)("MaxY should be 7"));
        }
        const actual_49 = box_13.MaxZ;
        if ((actual_49 === 9) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_49, 9, "MaxZ should be 9");
        }
        else {
            throw new Exception(contains((copyOfStruct_49 = actual_49, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_64) => (structuralHash(x_64) | 0),
            }) ? ((arg_54 = (9).toString(), (arg_1_49 = actual_49.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_54)(arg_1_49)("MaxZ should be 9")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(9)(actual_49)("MaxZ should be 9"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromBRect", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        let copyOfStruct_50, arg_55, arg_1_50, copyOfStruct_51, arg_56, arg_1_51, copyOfStruct_52, arg_57, arg_1_52, copyOfStruct_53, arg_58, arg_1_53, copyOfStruct_54, arg_59, arg_1_54, copyOfStruct_55, arg_60, arg_1_55;
        let box_14;
        let r_1;
        const a_17 = Pt_$ctor_7B00E9A0(1, 2);
        const b_32 = Pt_$ctor_7B00E9A0(5, 7);
        let minX_38 = a_17.X;
        let maxX_38;
        if (b_32.X > minX_38) {
            maxX_38 = b_32.X;
        }
        else {
            minX_38 = b_32.X;
            maxX_38 = a_17.X;
        }
        let minY_38 = a_17.Y;
        let maxY_38;
        if (b_32.Y > minY_38) {
            maxY_38 = b_32.Y;
        }
        else {
            minY_38 = b_32.Y;
            maxY_38 = a_17.Y;
        }
        r_1 = BRect_$ctor_77D16AC0(minX_38, minY_38, maxX_38, maxY_38);
        if (3 > 9) {
            fail(`BBox.createFromBRect: minZ > maxZ: ${3} > ${9}`);
        }
        box_14 = BBox_$ctor_76A78260(r_1.MinX, r_1.MinY, 3, r_1.MaxX, r_1.MaxY, 9);
        const actual_50 = box_14.MinX;
        if ((actual_50 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_50, 1, "MinX should be 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_50 = actual_50, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_65) => (structuralHash(x_65) | 0),
            }) ? ((arg_55 = (1).toString(), (arg_1_50 = actual_50.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_55)(arg_1_50)("MinX should be 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_50)("MinX should be 1"));
        }
        const actual_51 = box_14.MinY;
        if ((actual_51 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_51, 2, "MinY should be 2");
        }
        else {
            throw new Exception(contains((copyOfStruct_51 = actual_51, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_66) => (structuralHash(x_66) | 0),
            }) ? ((arg_56 = (2).toString(), (arg_1_51 = actual_51.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_56)(arg_1_51)("MinY should be 2")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_51)("MinY should be 2"));
        }
        const actual_52 = box_14.MinZ;
        if ((actual_52 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_52, 3, "MinZ should be 3");
        }
        else {
            throw new Exception(contains((copyOfStruct_52 = actual_52, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_67) => (structuralHash(x_67) | 0),
            }) ? ((arg_57 = (3).toString(), (arg_1_52 = actual_52.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_57)(arg_1_52)("MinZ should be 3")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_52)("MinZ should be 3"));
        }
        const actual_53 = box_14.MaxX;
        if ((actual_53 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_53, 5, "MaxX should be 5");
        }
        else {
            throw new Exception(contains((copyOfStruct_53 = actual_53, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_68) => (structuralHash(x_68) | 0),
            }) ? ((arg_58 = (5).toString(), (arg_1_53 = actual_53.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_58)(arg_1_53)("MaxX should be 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_53)("MaxX should be 5"));
        }
        const actual_54 = box_14.MaxY;
        if ((actual_54 === 7) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_54, 7, "MaxY should be 7");
        }
        else {
            throw new Exception(contains((copyOfStruct_54 = actual_54, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_69) => (structuralHash(x_69) | 0),
            }) ? ((arg_59 = (7).toString(), (arg_1_54 = actual_54.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_59)(arg_1_54)("MaxY should be 7")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(7)(actual_54)("MaxY should be 7"));
        }
        const actual_55 = box_14.MaxZ;
        if ((actual_55 === 9) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_55, 9, "MaxZ should be 9");
        }
        else {
            throw new Exception(contains((copyOfStruct_55 = actual_55, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_70) => (structuralHash(x_70) | 0),
            }) ? ((arg_60 = (9).toString(), (arg_1_55 = actual_55.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_60)(arg_1_55)("MaxZ should be 9")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(9)(actual_55)("MaxZ should be 9"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromBRect rejects invalid Z bounds", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        let rect_1;
        const a_18 = Pt_$ctor_7B00E9A0(1, 2);
        const b_33 = Pt_$ctor_7B00E9A0(5, 7);
        let minX_41 = a_18.X;
        let maxX_41;
        if (b_33.X > minX_41) {
            maxX_41 = b_33.X;
        }
        else {
            minX_41 = b_33.X;
            maxX_41 = a_18.X;
        }
        let minY_41 = a_18.Y;
        let maxY_41;
        if (b_33.Y > minY_41) {
            maxY_41 = b_33.Y;
        }
        else {
            minY_41 = b_33.Y;
            maxY_41 = a_18.Y;
        }
        rect_1 = BRect_$ctor_77D16AC0(minX_41, minY_41, maxX_41, maxY_41);
        Expect_throws(() => {
            let r_3;
            (r_3 = rect_1, ((9 > 3) ? fail(`BBox.createFromBRect: minZ > maxZ: ${9} > ${3}`) : undefined, BBox_$ctor_76A78260(r_3.MinX, r_3.MinY, 9, r_3.MaxX, r_3.MaxY, 3)));
        }, "Should throw when minZ > maxZ");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})()])), Test_testList("Expansion Methods", ofArray([(() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Expand by positive distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        let copyOfStruct_56, arg_61, arg_1_56, copyOfStruct_57, arg_62, arg_1_57, copyOfStruct_58, arg_63, arg_1_58, copyOfStruct_59, arg_64, arg_1_59, copyOfStruct_60, arg_65, arg_1_60, copyOfStruct_61, arg_66, arg_1_61;
        let expanded;
        let b_35;
        const a_19 = Pnt_$ctor_Z7AD9E565(2, 3, 4);
        const b_34 = Pnt_$ctor_Z7AD9E565(8, 9, 10);
        let minX_44 = a_19.X;
        let maxX_44;
        if (b_34.X > minX_44) {
            maxX_44 = b_34.X;
        }
        else {
            minX_44 = b_34.X;
            maxX_44 = a_19.X;
        }
        let minY_44 = a_19.Y;
        let maxY_44;
        if (b_34.Y > minY_44) {
            maxY_44 = b_34.Y;
        }
        else {
            minY_44 = b_34.Y;
            maxY_44 = a_19.Y;
        }
        let minZ_44 = a_19.Z;
        let maxZ_44;
        if (b_34.Z > minZ_44) {
            maxZ_44 = b_34.Z;
        }
        else {
            minZ_44 = b_34.Z;
            maxZ_44 = a_19.Z;
        }
        b_35 = BBox_$ctor_76A78260(minX_44, minY_44, minZ_44, maxX_44, maxY_44, maxZ_44);
        const n = BBox_$ctor_76A78260(b_35.MinX - 1, b_35.MinY - 1, b_35.MinZ - 1, b_35.MaxX + 1, b_35.MaxY + 1, b_35.MaxZ + 1);
        if ((1 < 0) && (((n.MinX > n.MaxX) ? true : (n.MinY > n.MaxY)) ? true : (n.MinZ > n.MaxZ))) {
            fail(`BBox.Expand(dist): Negative distance ${1} causes an underflow, on ${BBox__get_AsString(b_35)}`);
        }
        expanded = n;
        const actual_56 = expanded.MinX;
        if ((actual_56 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_56, 1, "MinX should be 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_56 = actual_56, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_71) => (structuralHash(x_71) | 0),
            }) ? ((arg_61 = (1).toString(), (arg_1_56 = actual_56.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_61)(arg_1_56)("MinX should be 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_56)("MinX should be 1"));
        }
        const actual_57 = expanded.MinY;
        if ((actual_57 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_57, 2, "MinY should be 2");
        }
        else {
            throw new Exception(contains((copyOfStruct_57 = actual_57, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_72) => (structuralHash(x_72) | 0),
            }) ? ((arg_62 = (2).toString(), (arg_1_57 = actual_57.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_62)(arg_1_57)("MinY should be 2")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_57)("MinY should be 2"));
        }
        const actual_58 = expanded.MinZ;
        if ((actual_58 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_58, 3, "MinZ should be 3");
        }
        else {
            throw new Exception(contains((copyOfStruct_58 = actual_58, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_73) => (structuralHash(x_73) | 0),
            }) ? ((arg_63 = (3).toString(), (arg_1_58 = actual_58.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_63)(arg_1_58)("MinZ should be 3")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_58)("MinZ should be 3"));
        }
        const actual_59 = expanded.MaxX;
        if ((actual_59 === 9) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_59, 9, "MaxX should be 9");
        }
        else {
            throw new Exception(contains((copyOfStruct_59 = actual_59, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_74) => (structuralHash(x_74) | 0),
            }) ? ((arg_64 = (9).toString(), (arg_1_59 = actual_59.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_64)(arg_1_59)("MaxX should be 9")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(9)(actual_59)("MaxX should be 9"));
        }
        const actual_60 = expanded.MaxY;
        if ((actual_60 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_60, 10, "MaxY should be 10");
        }
        else {
            throw new Exception(contains((copyOfStruct_60 = actual_60, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_75) => (structuralHash(x_75) | 0),
            }) ? ((arg_65 = (10).toString(), (arg_1_60 = actual_60.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_65)(arg_1_60)("MaxY should be 10")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_60)("MaxY should be 10"));
        }
        const actual_61 = expanded.MaxZ;
        if ((actual_61 === 11) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_61, 11, "MaxZ should be 11");
        }
        else {
            throw new Exception(contains((copyOfStruct_61 = actual_61, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_76) => (structuralHash(x_76) | 0),
            }) ? ((arg_66 = (11).toString(), (arg_1_61 = actual_61.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_66)(arg_1_61)("MaxZ should be 11")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(11)(actual_61)("MaxZ should be 11"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Expand by negative distance (shrink)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        let copyOfStruct_62, arg_67, arg_1_62, copyOfStruct_63, arg_68, arg_1_63;
        let shrunk;
        let b_37;
        const a_20 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_36 = Pnt_$ctor_Z7AD9E565(10, 10, 10);
        let minX_47 = a_20.X;
        let maxX_47;
        if (b_36.X > minX_47) {
            maxX_47 = b_36.X;
        }
        else {
            minX_47 = b_36.X;
            maxX_47 = a_20.X;
        }
        let minY_47 = a_20.Y;
        let maxY_47;
        if (b_36.Y > minY_47) {
            maxY_47 = b_36.Y;
        }
        else {
            minY_47 = b_36.Y;
            maxY_47 = a_20.Y;
        }
        let minZ_47 = a_20.Z;
        let maxZ_47;
        if (b_36.Z > minZ_47) {
            maxZ_47 = b_36.Z;
        }
        else {
            minZ_47 = b_36.Z;
            maxZ_47 = a_20.Z;
        }
        b_37 = BBox_$ctor_76A78260(minX_47, minY_47, minZ_47, maxX_47, maxY_47, maxZ_47);
        const n_1 = BBox_$ctor_76A78260(b_37.MinX - -2, b_37.MinY - -2, b_37.MinZ - -2, b_37.MaxX + -2, b_37.MaxY + -2, b_37.MaxZ + -2);
        if ((-2 < 0) && (((n_1.MinX > n_1.MaxX) ? true : (n_1.MinY > n_1.MaxY)) ? true : (n_1.MinZ > n_1.MaxZ))) {
            fail(`BBox.Expand(dist): Negative distance ${-2} causes an underflow, on ${BBox__get_AsString(b_37)}`);
        }
        shrunk = n_1;
        const actual_62 = shrunk.MinX;
        if ((actual_62 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_62, 2, "MinX should be 2");
        }
        else {
            throw new Exception(contains((copyOfStruct_62 = actual_62, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_77) => (structuralHash(x_77) | 0),
            }) ? ((arg_67 = (2).toString(), (arg_1_62 = actual_62.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_67)(arg_1_62)("MinX should be 2")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_62)("MinX should be 2"));
        }
        const actual_63 = shrunk.MaxX;
        if ((actual_63 === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_63, 8, "MaxX should be 8");
        }
        else {
            throw new Exception(contains((copyOfStruct_63 = actual_63, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_78) => (structuralHash(x_78) | 0),
            }) ? ((arg_68 = (8).toString(), (arg_1_63 = actual_63.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_68)(arg_1_63)("MaxX should be 8")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual_63)("MaxX should be 8"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Expand throws on underflow", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        let box_17;
        const a_21 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_38 = Pnt_$ctor_Z7AD9E565(2, 2, 2);
        let minX_50 = a_21.X;
        let maxX_50;
        if (b_38.X > minX_50) {
            maxX_50 = b_38.X;
        }
        else {
            minX_50 = b_38.X;
            maxX_50 = a_21.X;
        }
        let minY_50 = a_21.Y;
        let maxY_50;
        if (b_38.Y > minY_50) {
            maxY_50 = b_38.Y;
        }
        else {
            minY_50 = b_38.Y;
            maxY_50 = a_21.Y;
        }
        let minZ_50 = a_21.Z;
        let maxZ_50;
        if (b_38.Z > minZ_50) {
            maxZ_50 = b_38.Z;
        }
        else {
            minZ_50 = b_38.Z;
            maxZ_50 = a_21.Z;
        }
        box_17 = BBox_$ctor_76A78260(minX_50, minY_50, minZ_50, maxX_50, maxY_50, maxZ_50);
        Expect_throws(() => {
            let b_39, n_2;
            (b_39 = box_17, (n_2 = BBox_$ctor_76A78260(b_39.MinX - -2, b_39.MinY - -2, b_39.MinZ - -2, b_39.MaxX + -2, b_39.MaxY + -2, b_39.MaxZ + -2), (((-2 < 0) && (((n_2.MinX > n_2.MaxX) ? true : (n_2.MinY > n_2.MaxY)) ? true : (n_2.MinZ > n_2.MaxZ))) ? fail(`BBox.Expand(dist): Negative distance ${-2} causes an underflow, on ${BBox__get_AsString(b_39)}`) : undefined, n_2)));
        }, "Should throw when shrinking causes underflow");
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Expand with XYZ distances", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        let copyOfStruct_64, arg_69, arg_1_64, copyOfStruct_65, arg_70, arg_1_65, copyOfStruct_66, arg_71, arg_1_66, copyOfStruct_67, arg_72, arg_1_67, copyOfStruct_68, arg_73, arg_1_68, copyOfStruct_69, arg_74, arg_1_69;
        let expanded_1;
        let b_41;
        const a_22 = Pnt_$ctor_Z7AD9E565(2, 3, 4);
        const b_40 = Pnt_$ctor_Z7AD9E565(8, 9, 10);
        let minX_53 = a_22.X;
        let maxX_53;
        if (b_40.X > minX_53) {
            maxX_53 = b_40.X;
        }
        else {
            minX_53 = b_40.X;
            maxX_53 = a_22.X;
        }
        let minY_53 = a_22.Y;
        let maxY_53;
        if (b_40.Y > minY_53) {
            maxY_53 = b_40.Y;
        }
        else {
            minY_53 = b_40.Y;
            maxY_53 = a_22.Y;
        }
        let minZ_53 = a_22.Z;
        let maxZ_53;
        if (b_40.Z > minZ_53) {
            maxZ_53 = b_40.Z;
        }
        else {
            minZ_53 = b_40.Z;
            maxZ_53 = a_22.Z;
        }
        b_41 = BBox_$ctor_76A78260(minX_53, minY_53, minZ_53, maxX_53, maxY_53, maxZ_53);
        const n_3 = BBox_$ctor_76A78260(b_41.MinX - 1, b_41.MinY - 2, b_41.MinZ - 3, b_41.MaxX + 1, b_41.MaxY + 2, b_41.MaxZ + 3);
        if (((n_3.MinX > n_3.MaxX) ? true : (n_3.MinY > n_3.MaxY)) ? true : (n_3.MinZ > n_3.MaxZ)) {
            fail(`BBox.Expand(x, y, z): Negative distance(s) X: ${1} Y: ${2} and Z: ${3} cause an underflow, on ${BBox__get_AsString(b_41)}`);
        }
        expanded_1 = n_3;
        const actual_64 = expanded_1.MinX;
        if ((actual_64 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_64, 1, "MinX should be 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_64 = actual_64, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_79) => (structuralHash(x_79) | 0),
            }) ? ((arg_69 = (1).toString(), (arg_1_64 = actual_64.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_69)(arg_1_64)("MinX should be 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_64)("MinX should be 1"));
        }
        const actual_65 = expanded_1.MinY;
        if ((actual_65 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_65, 1, "MinY should be 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_65 = actual_65, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_80) => (structuralHash(x_80) | 0),
            }) ? ((arg_70 = (1).toString(), (arg_1_65 = actual_65.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_70)(arg_1_65)("MinY should be 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_65)("MinY should be 1"));
        }
        const actual_66 = expanded_1.MinZ;
        if ((actual_66 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_66, 1, "MinZ should be 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_66 = actual_66, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_81) => (structuralHash(x_81) | 0),
            }) ? ((arg_71 = (1).toString(), (arg_1_66 = actual_66.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_71)(arg_1_66)("MinZ should be 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_66)("MinZ should be 1"));
        }
        const actual_67 = expanded_1.MaxX;
        if ((actual_67 === 9) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_67, 9, "MaxX should be 9");
        }
        else {
            throw new Exception(contains((copyOfStruct_67 = actual_67, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_82) => (structuralHash(x_82) | 0),
            }) ? ((arg_72 = (9).toString(), (arg_1_67 = actual_67.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_72)(arg_1_67)("MaxX should be 9")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(9)(actual_67)("MaxX should be 9"));
        }
        const actual_68 = expanded_1.MaxY;
        if ((actual_68 === 11) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_68, 11, "MaxY should be 11");
        }
        else {
            throw new Exception(contains((copyOfStruct_68 = actual_68, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_83) => (structuralHash(x_83) | 0),
            }) ? ((arg_73 = (11).toString(), (arg_1_68 = actual_68.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_73)(arg_1_68)("MaxY should be 11")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(11)(actual_68)("MaxY should be 11"));
        }
        const actual_69 = expanded_1.MaxZ;
        if ((actual_69 === 13) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_69, 13, "MaxZ should be 13");
        }
        else {
            throw new Exception(contains((copyOfStruct_69 = actual_69, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_84) => (structuralHash(x_84) | 0),
            }) ? ((arg_74 = (13).toString(), (arg_1_69 = actual_69.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_74)(arg_1_69)("MaxZ should be 13")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(13)(actual_69)("MaxZ should be 13"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ExpandSafe with positive distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        let copyOfStruct_70, arg_75, arg_1_70, copyOfStruct_71, arg_76, arg_1_71;
        let expanded_2;
        let b_44;
        const a_23 = Pnt_$ctor_Z7AD9E565(2, 3, 4);
        const b_42 = Pnt_$ctor_Z7AD9E565(8, 9, 10);
        let minX_56 = a_23.X;
        let maxX_56;
        if (b_42.X > minX_56) {
            maxX_56 = b_42.X;
        }
        else {
            minX_56 = b_42.X;
            maxX_56 = a_23.X;
        }
        let minY_56 = a_23.Y;
        let maxY_56;
        if (b_42.Y > minY_56) {
            maxY_56 = b_42.Y;
        }
        else {
            minY_56 = b_42.Y;
            maxY_56 = a_23.Y;
        }
        let minZ_56 = a_23.Z;
        let maxZ_56;
        if (b_42.Z > minZ_56) {
            maxZ_56 = b_42.Z;
        }
        else {
            minZ_56 = b_42.Z;
            maxZ_56 = a_23.Z;
        }
        b_44 = BBox_$ctor_76A78260(minX_56, minY_56, minZ_56, maxX_56, maxY_56, maxZ_56);
        let minXCh = b_44.MinX - 1;
        let maxXCh = b_44.MaxX + 1;
        if (minXCh > maxXCh) {
            const mid = b_44.MinX + ((b_44.MaxX - b_44.MinX) * 0.5);
            minXCh = mid;
            maxXCh = mid;
        }
        let minYCh = b_44.MinY - 1;
        let maxYCh = b_44.MaxY + 1;
        if (minYCh > maxYCh) {
            const mid_1 = b_44.MinY + ((b_44.MaxY - b_44.MinY) * 0.5);
            minYCh = mid_1;
            maxYCh = mid_1;
        }
        let minZCh = b_44.MinZ - 1;
        let maxZCh = b_44.MaxZ + 1;
        if (minZCh > maxZCh) {
            const mid_2 = b_44.MinZ + ((b_44.MaxZ - b_44.MinZ) * 0.5);
            minZCh = mid_2;
            maxZCh = mid_2;
        }
        expanded_2 = BBox_$ctor_76A78260(minXCh, minYCh, minZCh, maxXCh, maxYCh, maxZCh);
        const actual_70 = expanded_2.MinX;
        if ((actual_70 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_70, 1, "MinX should be 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_70 = actual_70, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_85) => (structuralHash(x_85) | 0),
            }) ? ((arg_75 = (1).toString(), (arg_1_70 = actual_70.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_75)(arg_1_70)("MinX should be 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_70)("MinX should be 1"));
        }
        const actual_71 = expanded_2.MaxX;
        if ((actual_71 === 9) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_71, 9, "MaxX should be 9");
        }
        else {
            throw new Exception(contains((copyOfStruct_71 = actual_71, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_86) => (structuralHash(x_86) | 0),
            }) ? ((arg_76 = (9).toString(), (arg_1_71 = actual_71.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_76)(arg_1_71)("MaxX should be 9")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(9)(actual_71)("MaxX should be 9"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ExpandSafe with negative distance causing underflow", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        let copyOfStruct_72, arg_77, arg_1_72, copyOfStruct_73, arg_78, arg_1_73, copyOfStruct_74, arg_79, arg_1_74, copyOfStruct_75, arg_80, arg_1_75;
        let shrunk_1;
        let b_47;
        const a_24 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_45 = Pnt_$ctor_Z7AD9E565(2, 2, 2);
        let minX_59 = a_24.X;
        let maxX_59;
        if (b_45.X > minX_59) {
            maxX_59 = b_45.X;
        }
        else {
            minX_59 = b_45.X;
            maxX_59 = a_24.X;
        }
        let minY_59 = a_24.Y;
        let maxY_59;
        if (b_45.Y > minY_59) {
            maxY_59 = b_45.Y;
        }
        else {
            minY_59 = b_45.Y;
            maxY_59 = a_24.Y;
        }
        let minZ_59 = a_24.Z;
        let maxZ_59;
        if (b_45.Z > minZ_59) {
            maxZ_59 = b_45.Z;
        }
        else {
            minZ_59 = b_45.Z;
            maxZ_59 = a_24.Z;
        }
        b_47 = BBox_$ctor_76A78260(minX_59, minY_59, minZ_59, maxX_59, maxY_59, maxZ_59);
        let minXCh_1 = b_47.MinX - -5;
        let maxXCh_1 = b_47.MaxX + -5;
        if (minXCh_1 > maxXCh_1) {
            const mid_3 = b_47.MinX + ((b_47.MaxX - b_47.MinX) * 0.5);
            minXCh_1 = mid_3;
            maxXCh_1 = mid_3;
        }
        let minYCh_1 = b_47.MinY - -5;
        let maxYCh_1 = b_47.MaxY + -5;
        if (minYCh_1 > maxYCh_1) {
            const mid_1_1 = b_47.MinY + ((b_47.MaxY - b_47.MinY) * 0.5);
            minYCh_1 = mid_1_1;
            maxYCh_1 = mid_1_1;
        }
        let minZCh_1 = b_47.MinZ - -5;
        let maxZCh_1 = b_47.MaxZ + -5;
        if (minZCh_1 > maxZCh_1) {
            const mid_2_1 = b_47.MinZ + ((b_47.MaxZ - b_47.MinZ) * 0.5);
            minZCh_1 = mid_2_1;
            maxZCh_1 = mid_2_1;
        }
        shrunk_1 = BBox_$ctor_76A78260(minXCh_1, minYCh_1, minZCh_1, maxXCh_1, maxYCh_1, maxZCh_1);
        const actual_72 = shrunk_1.MinX;
        if ((actual_72 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_72, 1, "MinX should be midpoint 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_72 = actual_72, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_87) => (structuralHash(x_87) | 0),
            }) ? ((arg_77 = (1).toString(), (arg_1_72 = actual_72.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_77)(arg_1_72)("MinX should be midpoint 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_72)("MinX should be midpoint 1"));
        }
        const actual_73 = shrunk_1.MaxX;
        if ((actual_73 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_73, 1, "MaxX should be midpoint 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_73 = actual_73, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_88) => (structuralHash(x_88) | 0),
            }) ? ((arg_78 = (1).toString(), (arg_1_73 = actual_73.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_78)(arg_1_73)("MaxX should be midpoint 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_73)("MaxX should be midpoint 1"));
        }
        const actual_74 = shrunk_1.MinY;
        if ((actual_74 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_74, 1, "MinY should be midpoint 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_74 = actual_74, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_89) => (structuralHash(x_89) | 0),
            }) ? ((arg_79 = (1).toString(), (arg_1_74 = actual_74.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_79)(arg_1_74)("MinY should be midpoint 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_74)("MinY should be midpoint 1"));
        }
        const actual_75 = shrunk_1.MaxY;
        if ((actual_75 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_75, 1, "MaxY should be midpoint 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_75 = actual_75, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_90) => (structuralHash(x_90) | 0),
            }) ? ((arg_80 = (1).toString(), (arg_1_75 = actual_75.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_80)(arg_1_75)("MaxY should be midpoint 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_75)("MaxY should be midpoint 1"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ExpandSafe with mixed underflow", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        let copyOfStruct_76, arg_81, arg_1_76, copyOfStruct_77, arg_82, arg_1_77, copyOfStruct_78, arg_83, arg_1_78, copyOfStruct_79, arg_84, arg_1_79, copyOfStruct_80, arg_85, arg_1_80, copyOfStruct_81, arg_86, arg_1_81;
        let shrunk_2;
        let b_49;
        const a_25 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_48 = Pnt_$ctor_Z7AD9E565(2, 10, 4);
        let minX_62 = a_25.X;
        let maxX_62;
        if (b_48.X > minX_62) {
            maxX_62 = b_48.X;
        }
        else {
            minX_62 = b_48.X;
            maxX_62 = a_25.X;
        }
        let minY_62 = a_25.Y;
        let maxY_62;
        if (b_48.Y > minY_62) {
            maxY_62 = b_48.Y;
        }
        else {
            minY_62 = b_48.Y;
            maxY_62 = a_25.Y;
        }
        let minZ_62 = a_25.Z;
        let maxZ_62;
        if (b_48.Z > minZ_62) {
            maxZ_62 = b_48.Z;
        }
        else {
            minZ_62 = b_48.Z;
            maxZ_62 = a_25.Z;
        }
        b_49 = BBox_$ctor_76A78260(minX_62, minY_62, minZ_62, maxX_62, maxY_62, maxZ_62);
        let minXCh_2 = b_49.MinX - -3;
        let maxXCh_2 = b_49.MaxX + -3;
        if (minXCh_2 > maxXCh_2) {
            const mid_4 = b_49.MinX + ((b_49.MaxX - b_49.MinX) * 0.5);
            minXCh_2 = mid_4;
            maxXCh_2 = mid_4;
        }
        let minYCh_2 = b_49.MinY - -1;
        let maxYCh_2 = b_49.MaxY + -1;
        if (minYCh_2 > maxYCh_2) {
            const mid_1_2 = b_49.MinY + ((b_49.MaxY - b_49.MinY) * 0.5);
            minYCh_2 = mid_1_2;
            maxYCh_2 = mid_1_2;
        }
        let minZCh_2 = b_49.MinZ - -5;
        let maxZCh_2 = b_49.MaxZ + -5;
        if (minZCh_2 > maxZCh_2) {
            const mid_2_2 = b_49.MinZ + ((b_49.MaxZ - b_49.MinZ) * 0.5);
            minZCh_2 = mid_2_2;
            maxZCh_2 = mid_2_2;
        }
        shrunk_2 = BBox_$ctor_76A78260(minXCh_2, minYCh_2, minZCh_2, maxXCh_2, maxYCh_2, maxZCh_2);
        const actual_76 = shrunk_2.MinX;
        if ((actual_76 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_76, 1, "MinX should collapse to midpoint");
        }
        else {
            throw new Exception(contains((copyOfStruct_76 = actual_76, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_91) => (structuralHash(x_91) | 0),
            }) ? ((arg_81 = (1).toString(), (arg_1_76 = actual_76.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_81)(arg_1_76)("MinX should collapse to midpoint")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_76)("MinX should collapse to midpoint"));
        }
        const actual_77 = shrunk_2.MaxX;
        if ((actual_77 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_77, 1, "MaxX should collapse to midpoint");
        }
        else {
            throw new Exception(contains((copyOfStruct_77 = actual_77, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_92) => (structuralHash(x_92) | 0),
            }) ? ((arg_82 = (1).toString(), (arg_1_77 = actual_77.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_82)(arg_1_77)("MaxX should collapse to midpoint")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_77)("MaxX should collapse to midpoint"));
        }
        const actual_78 = shrunk_2.MinY;
        if ((actual_78 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_78, 1, "MinY should be 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_78 = actual_78, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_93) => (structuralHash(x_93) | 0),
            }) ? ((arg_83 = (1).toString(), (arg_1_78 = actual_78.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_83)(arg_1_78)("MinY should be 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_78)("MinY should be 1"));
        }
        const actual_79 = shrunk_2.MaxY;
        if ((actual_79 === 9) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_79, 9, "MaxY should be 9");
        }
        else {
            throw new Exception(contains((copyOfStruct_79 = actual_79, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_94) => (structuralHash(x_94) | 0),
            }) ? ((arg_84 = (9).toString(), (arg_1_79 = actual_79.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_84)(arg_1_79)("MaxY should be 9")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(9)(actual_79)("MaxY should be 9"));
        }
        const actual_80 = shrunk_2.MinZ;
        if ((actual_80 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_80, 2, "MinZ should collapse to midpoint");
        }
        else {
            throw new Exception(contains((copyOfStruct_80 = actual_80, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_95) => (structuralHash(x_95) | 0),
            }) ? ((arg_85 = (2).toString(), (arg_1_80 = actual_80.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_85)(arg_1_80)("MinZ should collapse to midpoint")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_80)("MinZ should collapse to midpoint"));
        }
        const actual_81 = shrunk_2.MaxZ;
        if ((actual_81 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_81, 2, "MaxZ should collapse to midpoint");
        }
        else {
            throw new Exception(contains((copyOfStruct_81 = actual_81, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_96) => (structuralHash(x_96) | 0),
            }) ? ((arg_86 = (2).toString(), (arg_1_81 = actual_81.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_86)(arg_1_81)("MaxZ should collapse to midpoint")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_81)("MaxZ should collapse to midpoint"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ExpandXaxis with positive distances", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        let copyOfStruct_82, arg_87, arg_1_82, copyOfStruct_83, arg_88, arg_1_83, copyOfStruct_84, arg_89, arg_1_84, copyOfStruct_85, arg_90, arg_1_85;
        let expanded_3;
        let b_51;
        const a_26 = Pnt_$ctor_Z7AD9E565(2, 3, 4);
        const b_50 = Pnt_$ctor_Z7AD9E565(8, 9, 10);
        let minX_65 = a_26.X;
        let maxX_65;
        if (b_50.X > minX_65) {
            maxX_65 = b_50.X;
        }
        else {
            minX_65 = b_50.X;
            maxX_65 = a_26.X;
        }
        let minY_65 = a_26.Y;
        let maxY_65;
        if (b_50.Y > minY_65) {
            maxY_65 = b_50.Y;
        }
        else {
            minY_65 = b_50.Y;
            maxY_65 = a_26.Y;
        }
        let minZ_65 = a_26.Z;
        let maxZ_65;
        if (b_50.Z > minZ_65) {
            maxZ_65 = b_50.Z;
        }
        else {
            minZ_65 = b_50.Z;
            maxZ_65 = a_26.Z;
        }
        b_51 = BBox_$ctor_76A78260(minX_65, minY_65, minZ_65, maxX_65, maxY_65, maxZ_65);
        const n_4 = BBox_$ctor_76A78260(b_51.MinX - 1, b_51.MinY, b_51.MinZ, b_51.MaxX + 2, b_51.MaxY, b_51.MaxZ);
        if (n_4.MinX > n_4.MaxX) {
            fail(`BBox.ExpandXaxis: Negative distances for start(${1}) and end (${2}) cause an underflow, on ${BBox__get_AsString(b_51)}`);
        }
        expanded_3 = n_4;
        const actual_82 = expanded_3.MinX;
        if ((actual_82 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_82, 1, "MinX should be 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_82 = actual_82, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_97) => (structuralHash(x_97) | 0),
            }) ? ((arg_87 = (1).toString(), (arg_1_82 = actual_82.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_87)(arg_1_82)("MinX should be 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_82)("MinX should be 1"));
        }
        const actual_83 = expanded_3.MaxX;
        if ((actual_83 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_83, 10, "MaxX should be 10");
        }
        else {
            throw new Exception(contains((copyOfStruct_83 = actual_83, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_98) => (structuralHash(x_98) | 0),
            }) ? ((arg_88 = (10).toString(), (arg_1_83 = actual_83.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_88)(arg_1_83)("MaxX should be 10")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_83)("MaxX should be 10"));
        }
        const actual_84 = expanded_3.MinY;
        if ((actual_84 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_84, 3, "MinY should be unchanged");
        }
        else {
            throw new Exception(contains((copyOfStruct_84 = actual_84, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_99) => (structuralHash(x_99) | 0),
            }) ? ((arg_89 = (3).toString(), (arg_1_84 = actual_84.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_89)(arg_1_84)("MinY should be unchanged")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_84)("MinY should be unchanged"));
        }
        const actual_85 = expanded_3.MaxY;
        if ((actual_85 === 9) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_85, 9, "MaxY should be unchanged");
        }
        else {
            throw new Exception(contains((copyOfStruct_85 = actual_85, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_100) => (structuralHash(x_100) | 0),
            }) ? ((arg_90 = (9).toString(), (arg_1_85 = actual_85.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_90)(arg_1_85)("MaxY should be unchanged")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(9)(actual_85)("MaxY should be unchanged"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ExpandYaxis with positive distances", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        let copyOfStruct_86, arg_91, arg_1_86, copyOfStruct_87, arg_92, arg_1_87, copyOfStruct_88, arg_93, arg_1_88, copyOfStruct_89, arg_94, arg_1_89;
        let expanded_4;
        let b_53;
        const a_27 = Pnt_$ctor_Z7AD9E565(2, 3, 4);
        const b_52 = Pnt_$ctor_Z7AD9E565(8, 9, 10);
        let minX_68 = a_27.X;
        let maxX_68;
        if (b_52.X > minX_68) {
            maxX_68 = b_52.X;
        }
        else {
            minX_68 = b_52.X;
            maxX_68 = a_27.X;
        }
        let minY_68 = a_27.Y;
        let maxY_68;
        if (b_52.Y > minY_68) {
            maxY_68 = b_52.Y;
        }
        else {
            minY_68 = b_52.Y;
            maxY_68 = a_27.Y;
        }
        let minZ_68 = a_27.Z;
        let maxZ_68;
        if (b_52.Z > minZ_68) {
            maxZ_68 = b_52.Z;
        }
        else {
            minZ_68 = b_52.Z;
            maxZ_68 = a_27.Z;
        }
        b_53 = BBox_$ctor_76A78260(minX_68, minY_68, minZ_68, maxX_68, maxY_68, maxZ_68);
        const n_5 = BBox_$ctor_76A78260(b_53.MinX, b_53.MinY - 1, b_53.MinZ, b_53.MaxX, b_53.MaxY + 2, b_53.MaxZ);
        if (n_5.MinY > n_5.MaxY) {
            fail(`BBox.ExpandYaxis: Negative distances for start(${1}) and end(${2}) cause an underflow, on ${BBox__get_AsString(b_53)}`);
        }
        expanded_4 = n_5;
        const actual_86 = expanded_4.MinY;
        if ((actual_86 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_86, 2, "MinY should be 2");
        }
        else {
            throw new Exception(contains((copyOfStruct_86 = actual_86, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_101) => (structuralHash(x_101) | 0),
            }) ? ((arg_91 = (2).toString(), (arg_1_86 = actual_86.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_91)(arg_1_86)("MinY should be 2")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_86)("MinY should be 2"));
        }
        const actual_87 = expanded_4.MaxY;
        if ((actual_87 === 11) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_87, 11, "MaxY should be 11");
        }
        else {
            throw new Exception(contains((copyOfStruct_87 = actual_87, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_102) => (structuralHash(x_102) | 0),
            }) ? ((arg_92 = (11).toString(), (arg_1_87 = actual_87.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_92)(arg_1_87)("MaxY should be 11")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(11)(actual_87)("MaxY should be 11"));
        }
        const actual_88 = expanded_4.MinX;
        if ((actual_88 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_88, 2, "MinX should be unchanged");
        }
        else {
            throw new Exception(contains((copyOfStruct_88 = actual_88, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_103) => (structuralHash(x_103) | 0),
            }) ? ((arg_93 = (2).toString(), (arg_1_88 = actual_88.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_93)(arg_1_88)("MinX should be unchanged")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_88)("MinX should be unchanged"));
        }
        const actual_89 = expanded_4.MaxX;
        if ((actual_89 === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_89, 8, "MaxX should be unchanged");
        }
        else {
            throw new Exception(contains((copyOfStruct_89 = actual_89, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_104) => (structuralHash(x_104) | 0),
            }) ? ((arg_94 = (8).toString(), (arg_1_89 = actual_89.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_94)(arg_1_89)("MaxX should be unchanged")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual_89)("MaxX should be unchanged"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ExpandZaxis with positive distances", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        let copyOfStruct_90, arg_95, arg_1_90, copyOfStruct_91, arg_96, arg_1_91, copyOfStruct_92, arg_97, arg_1_92;
        let expanded_5;
        let b_55;
        const a_28 = Pnt_$ctor_Z7AD9E565(2, 3, 4);
        const b_54 = Pnt_$ctor_Z7AD9E565(8, 9, 10);
        let minX_71 = a_28.X;
        let maxX_71;
        if (b_54.X > minX_71) {
            maxX_71 = b_54.X;
        }
        else {
            minX_71 = b_54.X;
            maxX_71 = a_28.X;
        }
        let minY_71 = a_28.Y;
        let maxY_71;
        if (b_54.Y > minY_71) {
            maxY_71 = b_54.Y;
        }
        else {
            minY_71 = b_54.Y;
            maxY_71 = a_28.Y;
        }
        let minZ_71 = a_28.Z;
        let maxZ_71;
        if (b_54.Z > minZ_71) {
            maxZ_71 = b_54.Z;
        }
        else {
            minZ_71 = b_54.Z;
            maxZ_71 = a_28.Z;
        }
        b_55 = BBox_$ctor_76A78260(minX_71, minY_71, minZ_71, maxX_71, maxY_71, maxZ_71);
        const n_6 = BBox_$ctor_76A78260(b_55.MinX, b_55.MinY, b_55.MinZ - 1, b_55.MaxX, b_55.MaxY, b_55.MaxZ + 2);
        if (n_6.MinZ > n_6.MaxZ) {
            fail(`BBox.ExpandZaxis: Negative distances for start(${1}) and end(${2}) cause an underflow, on ${BBox__get_AsString(b_55)}`);
        }
        expanded_5 = n_6;
        const actual_90 = expanded_5.MinZ;
        if ((actual_90 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_90, 3, "MinZ should be 3");
        }
        else {
            throw new Exception(contains((copyOfStruct_90 = actual_90, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_105) => (structuralHash(x_105) | 0),
            }) ? ((arg_95 = (3).toString(), (arg_1_90 = actual_90.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_95)(arg_1_90)("MinZ should be 3")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_90)("MinZ should be 3"));
        }
        const actual_91 = expanded_5.MaxZ;
        if ((actual_91 === 12) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_91, 12, "MaxZ should be 12");
        }
        else {
            throw new Exception(contains((copyOfStruct_91 = actual_91, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_106) => (structuralHash(x_106) | 0),
            }) ? ((arg_96 = (12).toString(), (arg_1_91 = actual_91.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_96)(arg_1_91)("MaxZ should be 12")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(12)(actual_91)("MaxZ should be 12"));
        }
        const actual_92 = expanded_5.MinX;
        if ((actual_92 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_92, 2, "MinX should be unchanged");
        }
        else {
            throw new Exception(contains((copyOfStruct_92 = actual_92, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_107) => (structuralHash(x_107) | 0),
            }) ? ((arg_97 = (2).toString(), (arg_1_92 = actual_92.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_97)(arg_1_92)("MinX should be unchanged")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_92)("MinX should be unchanged"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})(), (() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("expandRel with factor 1.5", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        let a_29, b_56, minX_74, maxX_74, minY_74, maxY_74, minZ_74, maxZ_74, a_31, b_58, b_60, x_108, y_96, z_3, copyOfStruct_93, arg_98, arg_1_93, copyOfStruct_94, arg_99, arg_1_94, copyOfStruct_95, arg_100, arg_1_95;
        const expanded_6 = BBox_expandRel(1.5, (a_29 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_56 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_74 = a_29.X, (maxX_74 = ((b_56.X > minX_74) ? b_56.X : ((minX_74 = b_56.X, a_29.X))), (minY_74 = a_29.Y, (maxY_74 = ((b_56.Y > minY_74) ? b_56.Y : ((minY_74 = b_56.Y, a_29.Y))), (minZ_74 = a_29.Z, (maxZ_74 = ((b_56.Z > minZ_74) ? b_56.Z : ((minZ_74 = b_56.Z, a_29.Z))), BBox_$ctor_76A78260(minX_74, minY_74, minZ_74, maxX_74, maxY_74, maxZ_74))))))))));
        Expect_isTrue(((a_31 = ((b_58 = expanded_6, Pnt_$ctor_Z7AD9E565_1((b_58.MaxX + b_58.MinX) * 0.5, (b_58.MaxY + b_58.MinY) * 0.5, (b_58.MaxZ + b_58.MinZ) * 0.5))), (b_60 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (x_108 = (a_31.X - b_60.X), (y_96 = (a_31.Y - b_60.Y), (z_3 = (a_31.Z - b_60.Z), Math.sqrt(((x_108 * x_108) + (y_96 * y_96)) + (z_3 * z_3)))))))) < 1E-09)("Center should remain at (5, 5, 5)");
        let actual_93;
        const b_61 = expanded_6;
        actual_93 = (b_61.MaxX - b_61.MinX);
        if ((actual_93 === 15) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_93, 15, "SizeX should be 15");
        }
        else {
            throw new Exception(contains((copyOfStruct_93 = actual_93, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_109) => (structuralHash(x_109) | 0),
            }) ? ((arg_98 = (15).toString(), (arg_1_93 = actual_93.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_98)(arg_1_93)("SizeX should be 15")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(15)(actual_93)("SizeX should be 15"));
        }
        let actual_94;
        const b_62 = expanded_6;
        actual_94 = (b_62.MaxY - b_62.MinY);
        if ((actual_94 === 15) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_94, 15, "SizeY should be 15");
        }
        else {
            throw new Exception(contains((copyOfStruct_94 = actual_94, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_110) => (structuralHash(x_110) | 0),
            }) ? ((arg_99 = (15).toString(), (arg_1_94 = actual_94.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_99)(arg_1_94)("SizeY should be 15")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(15)(actual_94)("SizeY should be 15"));
        }
        let actual_95;
        const b_63 = expanded_6;
        actual_95 = (b_63.MaxZ - b_63.MinZ);
        if ((actual_95 === 15) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_95, 15, "SizeZ should be 15");
        }
        else {
            throw new Exception(contains((copyOfStruct_95 = actual_95, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_111) => (structuralHash(x_111) | 0),
            }) ? ((arg_100 = (15).toString(), (arg_1_95 = actual_95.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_100)(arg_1_95)("SizeZ should be 15")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(15)(actual_95)("SizeZ should be 15"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_31);
    }));
})(), (() => {
    const builder$0040_32 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("expandRel with factor 0.5 (shrink)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_32, Test_TestCaseBuilder__Delay_1505(builder$0040_32, () => {
        let a_32, b_64, minX_76, maxX_76, minY_76, maxY_76, minZ_76, maxZ_76, a_34, b_66, b_68, x_112, y_100, z_4, copyOfStruct_96, arg_101, arg_1_96;
        const shrunk_3 = BBox_expandRel(0.5, (a_32 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_64 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_76 = a_32.X, (maxX_76 = ((b_64.X > minX_76) ? b_64.X : ((minX_76 = b_64.X, a_32.X))), (minY_76 = a_32.Y, (maxY_76 = ((b_64.Y > minY_76) ? b_64.Y : ((minY_76 = b_64.Y, a_32.Y))), (minZ_76 = a_32.Z, (maxZ_76 = ((b_64.Z > minZ_76) ? b_64.Z : ((minZ_76 = b_64.Z, a_32.Z))), BBox_$ctor_76A78260(minX_76, minY_76, minZ_76, maxX_76, maxY_76, maxZ_76))))))))));
        Expect_isTrue(((a_34 = ((b_66 = shrunk_3, Pnt_$ctor_Z7AD9E565_1((b_66.MaxX + b_66.MinX) * 0.5, (b_66.MaxY + b_66.MinY) * 0.5, (b_66.MaxZ + b_66.MinZ) * 0.5))), (b_68 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (x_112 = (a_34.X - b_68.X), (y_100 = (a_34.Y - b_68.Y), (z_4 = (a_34.Z - b_68.Z), Math.sqrt(((x_112 * x_112) + (y_100 * y_100)) + (z_4 * z_4)))))))) < 1E-09)("Center should remain at (5, 5, 5)");
        let actual_96;
        const b_69 = shrunk_3;
        actual_96 = (b_69.MaxX - b_69.MinX);
        if ((actual_96 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_96, 5, "SizeX should be 5");
        }
        else {
            throw new Exception(contains((copyOfStruct_96 = actual_96, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_113) => (structuralHash(x_113) | 0),
            }) ? ((arg_101 = (5).toString(), (arg_1_96 = actual_96.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_101)(arg_1_96)("SizeX should be 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_96)("SizeX should be 5"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_32);
    }));
})(), (() => {
    const builder$0040_33 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("expandRel rejects negative factor", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_33, Test_TestCaseBuilder__Delay_1505(builder$0040_33, () => {
        let box_27;
        const a_35 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_70 = Pnt_$ctor_Z7AD9E565(10, 10, 10);
        let minX_78 = a_35.X;
        let maxX_78;
        if (b_70.X > minX_78) {
            maxX_78 = b_70.X;
        }
        else {
            minX_78 = b_70.X;
            maxX_78 = a_35.X;
        }
        let minY_78 = a_35.Y;
        let maxY_78;
        if (b_70.Y > minY_78) {
            maxY_78 = b_70.Y;
        }
        else {
            minY_78 = b_70.Y;
            maxY_78 = a_35.Y;
        }
        let minZ_78 = a_35.Z;
        let maxZ_78;
        if (b_70.Z > minZ_78) {
            maxZ_78 = b_70.Z;
        }
        else {
            minZ_78 = b_70.Z;
            maxZ_78 = a_35.Z;
        }
        box_27 = BBox_$ctor_76A78260(minX_78, minY_78, minZ_78, maxX_78, maxY_78, maxZ_78);
        Expect_throws(() => {
            BBox_expandRel(-0.5, box_27);
        }, "Should throw on negative factor");
        Test_TestCaseBuilder__Zero(builder$0040_33);
    }));
})(), (() => {
    const builder$0040_34 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("expandRelXYZ with different factors", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_34, Test_TestCaseBuilder__Delay_1505(builder$0040_34, () => {
        let a_36, b_72, minX_80, maxX_80, minY_80, maxY_80, minZ_80, maxZ_80, a_38, b_74, b_76, x_114, y_102, z_5, copyOfStruct_97, arg_102, arg_1_97, copyOfStruct_98, arg_103, arg_1_98, copyOfStruct_99, arg_104, arg_1_99;
        const expanded_7 = BBox_expandRelXYZ(1.5, 0.5, 2, (a_36 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_72 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_80 = a_36.X, (maxX_80 = ((b_72.X > minX_80) ? b_72.X : ((minX_80 = b_72.X, a_36.X))), (minY_80 = a_36.Y, (maxY_80 = ((b_72.Y > minY_80) ? b_72.Y : ((minY_80 = b_72.Y, a_36.Y))), (minZ_80 = a_36.Z, (maxZ_80 = ((b_72.Z > minZ_80) ? b_72.Z : ((minZ_80 = b_72.Z, a_36.Z))), BBox_$ctor_76A78260(minX_80, minY_80, minZ_80, maxX_80, maxY_80, maxZ_80))))))))));
        Expect_isTrue(((a_38 = ((b_74 = expanded_7, Pnt_$ctor_Z7AD9E565_1((b_74.MaxX + b_74.MinX) * 0.5, (b_74.MaxY + b_74.MinY) * 0.5, (b_74.MaxZ + b_74.MinZ) * 0.5))), (b_76 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (x_114 = (a_38.X - b_76.X), (y_102 = (a_38.Y - b_76.Y), (z_5 = (a_38.Z - b_76.Z), Math.sqrt(((x_114 * x_114) + (y_102 * y_102)) + (z_5 * z_5)))))))) < 1E-09)("Center should remain at (5, 5, 5)");
        let actual_97;
        const b_77 = expanded_7;
        actual_97 = (b_77.MaxX - b_77.MinX);
        if ((actual_97 === 15) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_97, 15, "SizeX should be 15");
        }
        else {
            throw new Exception(contains((copyOfStruct_97 = actual_97, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_115) => (structuralHash(x_115) | 0),
            }) ? ((arg_102 = (15).toString(), (arg_1_97 = actual_97.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_102)(arg_1_97)("SizeX should be 15")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(15)(actual_97)("SizeX should be 15"));
        }
        let actual_98;
        const b_78 = expanded_7;
        actual_98 = (b_78.MaxY - b_78.MinY);
        if ((actual_98 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_98, 5, "SizeY should be 5");
        }
        else {
            throw new Exception(contains((copyOfStruct_98 = actual_98, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_116) => (structuralHash(x_116) | 0),
            }) ? ((arg_103 = (5).toString(), (arg_1_98 = actual_98.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_103)(arg_1_98)("SizeY should be 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_98)("SizeY should be 5"));
        }
        let actual_99;
        const b_79 = expanded_7;
        actual_99 = (b_79.MaxZ - b_79.MinZ);
        if ((actual_99 === 20) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_99, 20, "SizeZ should be 20");
        }
        else {
            throw new Exception(contains((copyOfStruct_99 = actual_99, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_117) => (structuralHash(x_117) | 0),
            }) ? ((arg_104 = (20).toString(), (arg_1_99 = actual_99.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_104)(arg_1_99)("SizeZ should be 20")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(20)(actual_99)("SizeZ should be 20"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_34);
    }));
})()])), Test_testList("Transformation Methods", ofArray([(() => {
    const builder$0040_35 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("move by vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_35, Test_TestCaseBuilder__Delay_1505(builder$0040_35, () => {
        let copyOfStruct_100, arg_105, arg_1_100, copyOfStruct_101, arg_106, arg_1_101, copyOfStruct_102, arg_107, arg_1_102, copyOfStruct_103, arg_108, arg_1_103, copyOfStruct_104, arg_109, arg_1_104, copyOfStruct_105, arg_110, arg_1_105;
        let box_29;
        const a_39 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_80 = Pnt_$ctor_Z7AD9E565(10, 5, 3);
        let minX_82 = a_39.X;
        let maxX_82;
        if (b_80.X > minX_82) {
            maxX_82 = b_80.X;
        }
        else {
            minX_82 = b_80.X;
            maxX_82 = a_39.X;
        }
        let minY_82 = a_39.Y;
        let maxY_82;
        if (b_80.Y > minY_82) {
            maxY_82 = b_80.Y;
        }
        else {
            minY_82 = b_80.Y;
            maxY_82 = a_39.Y;
        }
        let minZ_82 = a_39.Z;
        let maxZ_82;
        if (b_80.Z > minZ_82) {
            maxZ_82 = b_80.Z;
        }
        else {
            minZ_82 = b_80.Z;
            maxZ_82 = a_39.Z;
        }
        box_29 = BBox_$ctor_76A78260(minX_82, minY_82, minZ_82, maxX_82, maxY_82, maxZ_82);
        const moved = BBox_move(Vec_$ctor_Z7AD9E565_1(5, 3, 2), box_29);
        const actual_100 = moved.MinX;
        if ((actual_100 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_100, 5, "MinX should be 5");
        }
        else {
            throw new Exception(contains((copyOfStruct_100 = actual_100, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_118) => (structuralHash(x_118) | 0),
            }) ? ((arg_105 = (5).toString(), (arg_1_100 = actual_100.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_105)(arg_1_100)("MinX should be 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_100)("MinX should be 5"));
        }
        const actual_101 = moved.MinY;
        if ((actual_101 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_101, 3, "MinY should be 3");
        }
        else {
            throw new Exception(contains((copyOfStruct_101 = actual_101, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_119) => (structuralHash(x_119) | 0),
            }) ? ((arg_106 = (3).toString(), (arg_1_101 = actual_101.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_106)(arg_1_101)("MinY should be 3")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_101)("MinY should be 3"));
        }
        const actual_102 = moved.MinZ;
        if ((actual_102 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_102, 2, "MinZ should be 2");
        }
        else {
            throw new Exception(contains((copyOfStruct_102 = actual_102, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_120) => (structuralHash(x_120) | 0),
            }) ? ((arg_107 = (2).toString(), (arg_1_102 = actual_102.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_107)(arg_1_102)("MinZ should be 2")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_102)("MinZ should be 2"));
        }
        const actual_103 = moved.MaxX;
        if ((actual_103 === 15) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_103, 15, "MaxX should be 15");
        }
        else {
            throw new Exception(contains((copyOfStruct_103 = actual_103, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_121) => (structuralHash(x_121) | 0),
            }) ? ((arg_108 = (15).toString(), (arg_1_103 = actual_103.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_108)(arg_1_103)("MaxX should be 15")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(15)(actual_103)("MaxX should be 15"));
        }
        const actual_104 = moved.MaxY;
        if ((actual_104 === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_104, 8, "MaxY should be 8");
        }
        else {
            throw new Exception(contains((copyOfStruct_104 = actual_104, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_122) => (structuralHash(x_122) | 0),
            }) ? ((arg_109 = (8).toString(), (arg_1_104 = actual_104.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_109)(arg_1_104)("MaxY should be 8")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual_104)("MaxY should be 8"));
        }
        const actual_105 = moved.MaxZ;
        if ((actual_105 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_105, 5, "MaxZ should be 5");
        }
        else {
            throw new Exception(contains((copyOfStruct_105 = actual_105, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_123) => (structuralHash(x_123) | 0),
            }) ? ((arg_110 = (5).toString(), (arg_1_105 = actual_105.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_110)(arg_1_105)("MaxZ should be 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_105)("MaxZ should be 5"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_35);
    }));
})(), (() => {
    const builder$0040_36 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("translate by vector (alias for move)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_36, Test_TestCaseBuilder__Delay_1505(builder$0040_36, () => {
        let copyOfStruct_106, arg_111, arg_1_106, copyOfStruct_107, arg_112, arg_1_107;
        let box_30;
        const a_40 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_82 = Pnt_$ctor_Z7AD9E565(10, 5, 3);
        let minX_84 = a_40.X;
        let maxX_84;
        if (b_82.X > minX_84) {
            maxX_84 = b_82.X;
        }
        else {
            minX_84 = b_82.X;
            maxX_84 = a_40.X;
        }
        let minY_84 = a_40.Y;
        let maxY_84;
        if (b_82.Y > minY_84) {
            maxY_84 = b_82.Y;
        }
        else {
            minY_84 = b_82.Y;
            maxY_84 = a_40.Y;
        }
        let minZ_84 = a_40.Z;
        let maxZ_84;
        if (b_82.Z > minZ_84) {
            maxZ_84 = b_82.Z;
        }
        else {
            minZ_84 = b_82.Z;
            maxZ_84 = a_40.Z;
        }
        box_30 = BBox_$ctor_76A78260(minX_84, minY_84, minZ_84, maxX_84, maxY_84, maxZ_84);
        const moved_1 = BBox_translate(Vec_$ctor_Z7AD9E565_1(5, 3, 2), box_30);
        const actual_106 = moved_1.MinX;
        if ((actual_106 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_106, 5, "MinX should be 5");
        }
        else {
            throw new Exception(contains((copyOfStruct_106 = actual_106, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_124) => (structuralHash(x_124) | 0),
            }) ? ((arg_111 = (5).toString(), (arg_1_106 = actual_106.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_111)(arg_1_106)("MinX should be 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_106)("MinX should be 5"));
        }
        const actual_107 = moved_1.MaxX;
        if ((actual_107 === 15) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_107, 15, "MaxX should be 15");
        }
        else {
            throw new Exception(contains((copyOfStruct_107 = actual_107, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_125) => (structuralHash(x_125) | 0),
            }) ? ((arg_112 = (15).toString(), (arg_1_107 = actual_107.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_112)(arg_1_107)("MaxX should be 15")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(15)(actual_107)("MaxX should be 15"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_36);
    }));
})(), (() => {
    const builder$0040_37 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("moveX", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_37, Test_TestCaseBuilder__Delay_1505(builder$0040_37, () => {
        let a_41, b_84, minX_86, maxX_86, minY_86, maxY_86, minZ_86, maxZ_86, copyOfStruct_108, arg_113, arg_1_108, copyOfStruct_109, arg_114, arg_1_109, copyOfStruct_110, arg_115, arg_1_110, copyOfStruct_111, arg_116, arg_1_111;
        const moved_2 = BBox_moveX(5, (a_41 = Pnt_$ctor_Z7AD9E565(2, 3, 4), (b_84 = Pnt_$ctor_Z7AD9E565(8, 9, 10), (minX_86 = a_41.X, (maxX_86 = ((b_84.X > minX_86) ? b_84.X : ((minX_86 = b_84.X, a_41.X))), (minY_86 = a_41.Y, (maxY_86 = ((b_84.Y > minY_86) ? b_84.Y : ((minY_86 = b_84.Y, a_41.Y))), (minZ_86 = a_41.Z, (maxZ_86 = ((b_84.Z > minZ_86) ? b_84.Z : ((minZ_86 = b_84.Z, a_41.Z))), BBox_$ctor_76A78260(minX_86, minY_86, minZ_86, maxX_86, maxY_86, maxZ_86))))))))));
        const actual_108 = moved_2.MinX;
        if ((actual_108 === 7) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_108, 7, "MinX should be 7");
        }
        else {
            throw new Exception(contains((copyOfStruct_108 = actual_108, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_126) => (structuralHash(x_126) | 0),
            }) ? ((arg_113 = (7).toString(), (arg_1_108 = actual_108.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_113)(arg_1_108)("MinX should be 7")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(7)(actual_108)("MinX should be 7"));
        }
        const actual_109 = moved_2.MaxX;
        if ((actual_109 === 13) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_109, 13, "MaxX should be 13");
        }
        else {
            throw new Exception(contains((copyOfStruct_109 = actual_109, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_127) => (structuralHash(x_127) | 0),
            }) ? ((arg_114 = (13).toString(), (arg_1_109 = actual_109.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_114)(arg_1_109)("MaxX should be 13")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(13)(actual_109)("MaxX should be 13"));
        }
        const actual_110 = moved_2.MinY;
        if ((actual_110 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_110, 3, "MinY should be unchanged");
        }
        else {
            throw new Exception(contains((copyOfStruct_110 = actual_110, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_128) => (structuralHash(x_128) | 0),
            }) ? ((arg_115 = (3).toString(), (arg_1_110 = actual_110.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_115)(arg_1_110)("MinY should be unchanged")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_110)("MinY should be unchanged"));
        }
        const actual_111 = moved_2.MinZ;
        if ((actual_111 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_111, 4, "MinZ should be unchanged");
        }
        else {
            throw new Exception(contains((copyOfStruct_111 = actual_111, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_129) => (structuralHash(x_129) | 0),
            }) ? ((arg_116 = (4).toString(), (arg_1_111 = actual_111.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_116)(arg_1_111)("MinZ should be unchanged")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_111)("MinZ should be unchanged"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_37);
    }));
})(), (() => {
    const builder$0040_38 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("moveY", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_38, Test_TestCaseBuilder__Delay_1505(builder$0040_38, () => {
        let a_42, b_86, minX_88, maxX_88, minY_88, maxY_88, minZ_88, maxZ_88, copyOfStruct_112, arg_117, arg_1_112, copyOfStruct_113, arg_118, arg_1_113, copyOfStruct_114, arg_119, arg_1_114;
        const moved_3 = BBox_moveY(5, (a_42 = Pnt_$ctor_Z7AD9E565(2, 3, 4), (b_86 = Pnt_$ctor_Z7AD9E565(8, 9, 10), (minX_88 = a_42.X, (maxX_88 = ((b_86.X > minX_88) ? b_86.X : ((minX_88 = b_86.X, a_42.X))), (minY_88 = a_42.Y, (maxY_88 = ((b_86.Y > minY_88) ? b_86.Y : ((minY_88 = b_86.Y, a_42.Y))), (minZ_88 = a_42.Z, (maxZ_88 = ((b_86.Z > minZ_88) ? b_86.Z : ((minZ_88 = b_86.Z, a_42.Z))), BBox_$ctor_76A78260(minX_88, minY_88, minZ_88, maxX_88, maxY_88, maxZ_88))))))))));
        const actual_112 = moved_3.MinY;
        if ((actual_112 === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_112, 8, "MinY should be 8");
        }
        else {
            throw new Exception(contains((copyOfStruct_112 = actual_112, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_130) => (structuralHash(x_130) | 0),
            }) ? ((arg_117 = (8).toString(), (arg_1_112 = actual_112.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_117)(arg_1_112)("MinY should be 8")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual_112)("MinY should be 8"));
        }
        const actual_113 = moved_3.MaxY;
        if ((actual_113 === 14) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_113, 14, "MaxY should be 14");
        }
        else {
            throw new Exception(contains((copyOfStruct_113 = actual_113, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_131) => (structuralHash(x_131) | 0),
            }) ? ((arg_118 = (14).toString(), (arg_1_113 = actual_113.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_118)(arg_1_113)("MaxY should be 14")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(14)(actual_113)("MaxY should be 14"));
        }
        const actual_114 = moved_3.MinX;
        if ((actual_114 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_114, 2, "MinX should be unchanged");
        }
        else {
            throw new Exception(contains((copyOfStruct_114 = actual_114, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_132) => (structuralHash(x_132) | 0),
            }) ? ((arg_119 = (2).toString(), (arg_1_114 = actual_114.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_119)(arg_1_114)("MinX should be unchanged")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_114)("MinX should be unchanged"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_38);
    }));
})(), (() => {
    const builder$0040_39 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("moveZ", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_39, Test_TestCaseBuilder__Delay_1505(builder$0040_39, () => {
        let a_43, b_88, minX_90, maxX_90, minY_90, maxY_90, minZ_90, maxZ_90, copyOfStruct_115, arg_120, arg_1_115, copyOfStruct_116, arg_121, arg_1_116, copyOfStruct_117, arg_122, arg_1_117;
        const moved_4 = BBox_moveZ(5, (a_43 = Pnt_$ctor_Z7AD9E565(2, 3, 4), (b_88 = Pnt_$ctor_Z7AD9E565(8, 9, 10), (minX_90 = a_43.X, (maxX_90 = ((b_88.X > minX_90) ? b_88.X : ((minX_90 = b_88.X, a_43.X))), (minY_90 = a_43.Y, (maxY_90 = ((b_88.Y > minY_90) ? b_88.Y : ((minY_90 = b_88.Y, a_43.Y))), (minZ_90 = a_43.Z, (maxZ_90 = ((b_88.Z > minZ_90) ? b_88.Z : ((minZ_90 = b_88.Z, a_43.Z))), BBox_$ctor_76A78260(minX_90, minY_90, minZ_90, maxX_90, maxY_90, maxZ_90))))))))));
        const actual_115 = moved_4.MinZ;
        if ((actual_115 === 9) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_115, 9, "MinZ should be 9");
        }
        else {
            throw new Exception(contains((copyOfStruct_115 = actual_115, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_133) => (structuralHash(x_133) | 0),
            }) ? ((arg_120 = (9).toString(), (arg_1_115 = actual_115.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_120)(arg_1_115)("MinZ should be 9")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(9)(actual_115)("MinZ should be 9"));
        }
        const actual_116 = moved_4.MaxZ;
        if ((actual_116 === 15) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_116, 15, "MaxZ should be 15");
        }
        else {
            throw new Exception(contains((copyOfStruct_116 = actual_116, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_134) => (structuralHash(x_134) | 0),
            }) ? ((arg_121 = (15).toString(), (arg_1_116 = actual_116.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_121)(arg_1_116)("MaxZ should be 15")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(15)(actual_116)("MaxZ should be 15"));
        }
        const actual_117 = moved_4.MinX;
        if ((actual_117 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_117, 2, "MinX should be unchanged");
        }
        else {
            throw new Exception(contains((copyOfStruct_117 = actual_117, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_135) => (structuralHash(x_135) | 0),
            }) ? ((arg_122 = (2).toString(), (arg_1_117 = actual_117.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_122)(arg_1_117)("MinX should be unchanged")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_117)("MinX should be unchanged"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_39);
    }));
})(), (() => {
    const builder$0040_40 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("scale from world origin", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_40, Test_TestCaseBuilder__Delay_1505(builder$0040_40, () => {
        let copyOfStruct_118, arg_123, arg_1_118, copyOfStruct_119, arg_124, arg_1_119, copyOfStruct_120, arg_125, arg_1_120, copyOfStruct_121, arg_126, arg_1_121, copyOfStruct_122, arg_127, arg_1_122, copyOfStruct_123, arg_128, arg_1_123;
        let scaled;
        let b_92;
        const a_44 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        const b_90 = Pnt_$ctor_Z7AD9E565(5, 7, 9);
        let minX_92 = a_44.X;
        let maxX_92;
        if (b_90.X > minX_92) {
            maxX_92 = b_90.X;
        }
        else {
            minX_92 = b_90.X;
            maxX_92 = a_44.X;
        }
        let minY_92 = a_44.Y;
        let maxY_92;
        if (b_90.Y > minY_92) {
            maxY_92 = b_90.Y;
        }
        else {
            minY_92 = b_90.Y;
            maxY_92 = a_44.Y;
        }
        let minZ_92 = a_44.Z;
        let maxZ_92;
        if (b_90.Z > minZ_92) {
            maxZ_92 = b_90.Z;
        }
        else {
            minZ_92 = b_90.Z;
            maxZ_92 = a_44.Z;
        }
        b_92 = BBox_$ctor_76A78260(minX_92, minY_92, minZ_92, maxX_92, maxY_92, maxZ_92);
        if (2 < 0) {
            fail(`BBox.scale: a negative factor ${2} is not allowed for scaling the 3D bounding box ${BBox__get_AsString(b_92)}`);
        }
        scaled = BBox_$ctor_76A78260(b_92.MinX * 2, b_92.MinY * 2, b_92.MinZ * 2, b_92.MaxX * 2, b_92.MaxY * 2, b_92.MaxZ * 2);
        const actual_118 = scaled.MinX;
        if ((actual_118 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_118, 2, "MinX should be 2");
        }
        else {
            throw new Exception(contains((copyOfStruct_118 = actual_118, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_136) => (structuralHash(x_136) | 0),
            }) ? ((arg_123 = (2).toString(), (arg_1_118 = actual_118.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_123)(arg_1_118)("MinX should be 2")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_118)("MinX should be 2"));
        }
        const actual_119 = scaled.MinY;
        if ((actual_119 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_119, 4, "MinY should be 4");
        }
        else {
            throw new Exception(contains((copyOfStruct_119 = actual_119, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_137) => (structuralHash(x_137) | 0),
            }) ? ((arg_124 = (4).toString(), (arg_1_119 = actual_119.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_124)(arg_1_119)("MinY should be 4")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_119)("MinY should be 4"));
        }
        const actual_120 = scaled.MinZ;
        if ((actual_120 === 6) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_120, 6, "MinZ should be 6");
        }
        else {
            throw new Exception(contains((copyOfStruct_120 = actual_120, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_138) => (structuralHash(x_138) | 0),
            }) ? ((arg_125 = (6).toString(), (arg_1_120 = actual_120.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_125)(arg_1_120)("MinZ should be 6")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(6)(actual_120)("MinZ should be 6"));
        }
        const actual_121 = scaled.MaxX;
        if ((actual_121 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_121, 10, "MaxX should be 10");
        }
        else {
            throw new Exception(contains((copyOfStruct_121 = actual_121, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_139) => (structuralHash(x_139) | 0),
            }) ? ((arg_126 = (10).toString(), (arg_1_121 = actual_121.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_126)(arg_1_121)("MaxX should be 10")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_121)("MaxX should be 10"));
        }
        const actual_122 = scaled.MaxY;
        if ((actual_122 === 14) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_122, 14, "MaxY should be 14");
        }
        else {
            throw new Exception(contains((copyOfStruct_122 = actual_122, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_140) => (structuralHash(x_140) | 0),
            }) ? ((arg_127 = (14).toString(), (arg_1_122 = actual_122.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_127)(arg_1_122)("MaxY should be 14")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(14)(actual_122)("MaxY should be 14"));
        }
        const actual_123 = scaled.MaxZ;
        if ((actual_123 === 18) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_123, 18, "MaxZ should be 18");
        }
        else {
            throw new Exception(contains((copyOfStruct_123 = actual_123, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_141) => (structuralHash(x_141) | 0),
            }) ? ((arg_128 = (18).toString(), (arg_1_123 = actual_123.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_128)(arg_1_123)("MaxZ should be 18")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(18)(actual_123)("MaxZ should be 18"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_40);
    }));
})()])), Test_testList("Overlap and Intersection", ofArray([(() => {
    const builder$0040_41 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("OverlapsWith - overlapping boxes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_41, Test_TestCaseBuilder__Delay_1505(builder$0040_41, () => {
        let b_96, a_48, b_97, a_49;
        let a_46;
        const a_45 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_93 = Pnt_$ctor_Z7AD9E565(5, 5, 5);
        let minX_95 = a_45.X;
        let maxX_95;
        if (b_93.X > minX_95) {
            maxX_95 = b_93.X;
        }
        else {
            minX_95 = b_93.X;
            maxX_95 = a_45.X;
        }
        let minY_95 = a_45.Y;
        let maxY_95;
        if (b_93.Y > minY_95) {
            maxY_95 = b_93.Y;
        }
        else {
            minY_95 = b_93.Y;
            maxY_95 = a_45.Y;
        }
        let minZ_95 = a_45.Z;
        let maxZ_95;
        if (b_93.Z > minZ_95) {
            maxZ_95 = b_93.Z;
        }
        else {
            minZ_95 = b_93.Z;
            maxZ_95 = a_45.Z;
        }
        a_46 = BBox_$ctor_76A78260(minX_95, minY_95, minZ_95, maxX_95, maxY_95, maxZ_95);
        let b_95;
        const a_47 = Pnt_$ctor_Z7AD9E565(3, 3, 3);
        const b_94 = Pnt_$ctor_Z7AD9E565(8, 8, 8);
        let minX_97 = a_47.X;
        let maxX_97;
        if (b_94.X > minX_97) {
            maxX_97 = b_94.X;
        }
        else {
            minX_97 = b_94.X;
            maxX_97 = a_47.X;
        }
        let minY_97 = a_47.Y;
        let maxY_97;
        if (b_94.Y > minY_97) {
            maxY_97 = b_94.Y;
        }
        else {
            minY_97 = b_94.Y;
            maxY_97 = a_47.Y;
        }
        let minZ_97 = a_47.Z;
        let maxZ_97;
        if (b_94.Z > minZ_97) {
            maxZ_97 = b_94.Z;
        }
        else {
            minZ_97 = b_94.Z;
            maxZ_97 = a_47.Z;
        }
        b_95 = BBox_$ctor_76A78260(minX_97, minY_97, minZ_97, maxX_97, maxY_97, maxZ_97);
        Expect_isTrue((b_96 = a_46, (a_48 = b_95, !((((((b_96.MinX > a_48.MaxX) ? true : (a_48.MinX > b_96.MaxX)) ? true : (a_48.MinY > b_96.MaxY)) ? true : (b_96.MinY > a_48.MaxY)) ? true : (a_48.MinZ > b_96.MaxZ)) ? true : (b_96.MinZ > a_48.MaxZ)))))("Boxes should overlap");
        Expect_isTrue((b_97 = b_95, (a_49 = a_46, !((((((b_97.MinX > a_49.MaxX) ? true : (a_49.MinX > b_97.MaxX)) ? true : (a_49.MinY > b_97.MaxY)) ? true : (b_97.MinY > a_49.MaxY)) ? true : (a_49.MinZ > b_97.MaxZ)) ? true : (b_97.MinZ > a_49.MaxZ)))))("Overlap should be symmetric");
        Test_TestCaseBuilder__Zero(builder$0040_41);
    }));
})(), (() => {
    const builder$0040_42 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("OverlapsWith - touching boxes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_42, Test_TestCaseBuilder__Delay_1505(builder$0040_42, () => {
        let b_101, a_50, b_98, minX_99, maxX_99, minY_99, maxY_99, minZ_99, maxZ_99, a_53, a_52, b_99, minX_101, maxX_101, minY_101, maxY_101, minZ_101, maxZ_101;
        Expect_isTrue((b_101 = ((a_50 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_98 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (minX_99 = a_50.X, (maxX_99 = ((b_98.X > minX_99) ? b_98.X : ((minX_99 = b_98.X, a_50.X))), (minY_99 = a_50.Y, (maxY_99 = ((b_98.Y > minY_99) ? b_98.Y : ((minY_99 = b_98.Y, a_50.Y))), (minZ_99 = a_50.Z, (maxZ_99 = ((b_98.Z > minZ_99) ? b_98.Z : ((minZ_99 = b_98.Z, a_50.Z))), BBox_$ctor_76A78260(minX_99, minY_99, minZ_99, maxX_99, maxY_99, maxZ_99)))))))))), (a_53 = ((a_52 = Pnt_$ctor_Z7AD9E565(5, 0, 0), (b_99 = Pnt_$ctor_Z7AD9E565(10, 5, 5), (minX_101 = a_52.X, (maxX_101 = ((b_99.X > minX_101) ? b_99.X : ((minX_101 = b_99.X, a_52.X))), (minY_101 = a_52.Y, (maxY_101 = ((b_99.Y > minY_101) ? b_99.Y : ((minY_101 = b_99.Y, a_52.Y))), (minZ_101 = a_52.Z, (maxZ_101 = ((b_99.Z > minZ_101) ? b_99.Z : ((minZ_101 = b_99.Z, a_52.Z))), BBox_$ctor_76A78260(minX_101, minY_101, minZ_101, maxX_101, maxY_101, maxZ_101)))))))))), !((((((b_101.MinX > a_53.MaxX) ? true : (a_53.MinX > b_101.MaxX)) ? true : (a_53.MinY > b_101.MaxY)) ? true : (b_101.MinY > a_53.MaxY)) ? true : (a_53.MinZ > b_101.MaxZ)) ? true : (b_101.MinZ > a_53.MaxZ)))))("Touching boxes should overlap");
        Test_TestCaseBuilder__Zero(builder$0040_42);
    }));
})(), (() => {
    const builder$0040_43 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("OverlapsWith - separated boxes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_43, Test_TestCaseBuilder__Delay_1505(builder$0040_43, () => {
        let b_105, a_54, b_102, minX_103, maxX_103, minY_103, maxY_103, minZ_103, maxZ_103, a_57, a_56, b_103, minX_105, maxX_105, minY_105, maxY_105, minZ_105, maxZ_105;
        Expect_isFalse((b_105 = ((a_54 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_102 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (minX_103 = a_54.X, (maxX_103 = ((b_102.X > minX_103) ? b_102.X : ((minX_103 = b_102.X, a_54.X))), (minY_103 = a_54.Y, (maxY_103 = ((b_102.Y > minY_103) ? b_102.Y : ((minY_103 = b_102.Y, a_54.Y))), (minZ_103 = a_54.Z, (maxZ_103 = ((b_102.Z > minZ_103) ? b_102.Z : ((minZ_103 = b_102.Z, a_54.Z))), BBox_$ctor_76A78260(minX_103, minY_103, minZ_103, maxX_103, maxY_103, maxZ_103)))))))))), (a_57 = ((a_56 = Pnt_$ctor_Z7AD9E565(6, 6, 6), (b_103 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_105 = a_56.X, (maxX_105 = ((b_103.X > minX_105) ? b_103.X : ((minX_105 = b_103.X, a_56.X))), (minY_105 = a_56.Y, (maxY_105 = ((b_103.Y > minY_105) ? b_103.Y : ((minY_105 = b_103.Y, a_56.Y))), (minZ_105 = a_56.Z, (maxZ_105 = ((b_103.Z > minZ_105) ? b_103.Z : ((minZ_105 = b_103.Z, a_56.Z))), BBox_$ctor_76A78260(minX_105, minY_105, minZ_105, maxX_105, maxY_105, maxZ_105)))))))))), !((((((b_105.MinX > a_57.MaxX) ? true : (a_57.MinX > b_105.MaxX)) ? true : (a_57.MinY > b_105.MaxY)) ? true : (b_105.MinY > a_57.MaxY)) ? true : (a_57.MinZ > b_105.MaxZ)) ? true : (b_105.MinZ > a_57.MaxZ)))))("Separated boxes should not overlap");
        Test_TestCaseBuilder__Zero(builder$0040_43);
    }));
})(), (() => {
    const builder$0040_44 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("OverlapsWith - one inside the other", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_44, Test_TestCaseBuilder__Delay_1505(builder$0040_44, () => {
        let b_109, a_61, b_110, a_62;
        let a_59;
        const a_58 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_106 = Pnt_$ctor_Z7AD9E565(10, 10, 10);
        let minX_107 = a_58.X;
        let maxX_107;
        if (b_106.X > minX_107) {
            maxX_107 = b_106.X;
        }
        else {
            minX_107 = b_106.X;
            maxX_107 = a_58.X;
        }
        let minY_107 = a_58.Y;
        let maxY_107;
        if (b_106.Y > minY_107) {
            maxY_107 = b_106.Y;
        }
        else {
            minY_107 = b_106.Y;
            maxY_107 = a_58.Y;
        }
        let minZ_107 = a_58.Z;
        let maxZ_107;
        if (b_106.Z > minZ_107) {
            maxZ_107 = b_106.Z;
        }
        else {
            minZ_107 = b_106.Z;
            maxZ_107 = a_58.Z;
        }
        a_59 = BBox_$ctor_76A78260(minX_107, minY_107, minZ_107, maxX_107, maxY_107, maxZ_107);
        let b_108;
        const a_60 = Pnt_$ctor_Z7AD9E565(2, 2, 2);
        const b_107 = Pnt_$ctor_Z7AD9E565(8, 8, 8);
        let minX_109 = a_60.X;
        let maxX_109;
        if (b_107.X > minX_109) {
            maxX_109 = b_107.X;
        }
        else {
            minX_109 = b_107.X;
            maxX_109 = a_60.X;
        }
        let minY_109 = a_60.Y;
        let maxY_109;
        if (b_107.Y > minY_109) {
            maxY_109 = b_107.Y;
        }
        else {
            minY_109 = b_107.Y;
            maxY_109 = a_60.Y;
        }
        let minZ_109 = a_60.Z;
        let maxZ_109;
        if (b_107.Z > minZ_109) {
            maxZ_109 = b_107.Z;
        }
        else {
            minZ_109 = b_107.Z;
            maxZ_109 = a_60.Z;
        }
        b_108 = BBox_$ctor_76A78260(minX_109, minY_109, minZ_109, maxX_109, maxY_109, maxZ_109);
        Expect_isTrue((b_109 = a_59, (a_61 = b_108, !((((((b_109.MinX > a_61.MaxX) ? true : (a_61.MinX > b_109.MaxX)) ? true : (a_61.MinY > b_109.MaxY)) ? true : (b_109.MinY > a_61.MaxY)) ? true : (a_61.MinZ > b_109.MaxZ)) ? true : (b_109.MinZ > a_61.MaxZ)))))("Inner box should overlap");
        Expect_isTrue((b_110 = b_108, (a_62 = a_59, !((((((b_110.MinX > a_62.MaxX) ? true : (a_62.MinX > b_110.MaxX)) ? true : (a_62.MinY > b_110.MaxY)) ? true : (b_110.MinY > a_62.MaxY)) ? true : (a_62.MinZ > b_110.MaxZ)) ? true : (b_110.MinZ > a_62.MaxZ)))))("Overlap should be symmetric");
        Test_TestCaseBuilder__Zero(builder$0040_44);
    }));
})(), (() => {
    const builder$0040_45 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("OverlapsWith with tolerance - barely overlapping", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_45, Test_TestCaseBuilder__Delay_1505(builder$0040_45, () => {
        let b_114, a_66, b_115, a_67;
        let a_64;
        const a_63 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_111 = Pnt_$ctor_Z7AD9E565(5, 5, 5);
        let minX_111 = a_63.X;
        let maxX_111;
        if (b_111.X > minX_111) {
            maxX_111 = b_111.X;
        }
        else {
            minX_111 = b_111.X;
            maxX_111 = a_63.X;
        }
        let minY_111 = a_63.Y;
        let maxY_111;
        if (b_111.Y > minY_111) {
            maxY_111 = b_111.Y;
        }
        else {
            minY_111 = b_111.Y;
            maxY_111 = a_63.Y;
        }
        let minZ_111 = a_63.Z;
        let maxZ_111;
        if (b_111.Z > minZ_111) {
            maxZ_111 = b_111.Z;
        }
        else {
            minZ_111 = b_111.Z;
            maxZ_111 = a_63.Z;
        }
        a_64 = BBox_$ctor_76A78260(minX_111, minY_111, minZ_111, maxX_111, maxY_111, maxZ_111);
        let b_113;
        const a_65 = Pnt_$ctor_Z7AD9E565(5.5, 0, 0);
        const b_112 = Pnt_$ctor_Z7AD9E565(10, 5, 5);
        let minX_113 = a_65.X;
        let maxX_113;
        if (b_112.X > minX_113) {
            maxX_113 = b_112.X;
        }
        else {
            minX_113 = b_112.X;
            maxX_113 = a_65.X;
        }
        let minY_113 = a_65.Y;
        let maxY_113;
        if (b_112.Y > minY_113) {
            maxY_113 = b_112.Y;
        }
        else {
            minY_113 = b_112.Y;
            maxY_113 = a_65.Y;
        }
        let minZ_113 = a_65.Z;
        let maxZ_113;
        if (b_112.Z > minZ_113) {
            maxZ_113 = b_112.Z;
        }
        else {
            minZ_113 = b_112.Z;
            maxZ_113 = a_65.Z;
        }
        b_113 = BBox_$ctor_76A78260(minX_113, minY_113, minZ_113, maxX_113, maxY_113, maxZ_113);
        Expect_isFalse((b_114 = a_64, (a_66 = b_113, !((((((b_114.MinX > (a_66.MaxX - 0.1)) ? true : (a_66.MinX > (b_114.MaxX - 0.1))) ? true : (a_66.MinY > (b_114.MaxY - 0.1))) ? true : (b_114.MinY > (a_66.MaxY - 0.1))) ? true : (a_66.MinZ > (b_114.MaxZ - 0.1))) ? true : (b_114.MinZ > (a_66.MaxZ - 0.1))))))("Should not overlap with small tolerance");
        Expect_isTrue((b_115 = a_64, (a_67 = b_113, !((((((b_115.MinX > (a_67.MaxX - -0.6)) ? true : (a_67.MinX > (b_115.MaxX - -0.6))) ? true : (a_67.MinY > (b_115.MaxY - -0.6))) ? true : (b_115.MinY > (a_67.MaxY - -0.6))) ? true : (a_67.MinZ > (b_115.MaxZ - -0.6))) ? true : (b_115.MinZ > (a_67.MaxZ - -0.6))))))("Should overlap with negative tolerance (expands boxes)");
        Test_TestCaseBuilder__Zero(builder$0040_45);
    }));
})(), (() => {
    const builder$0040_46 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doOverlap static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_46, Test_TestCaseBuilder__Delay_1505(builder$0040_46, () => {
        let b_121, a_70, b_117, minX_117, maxX_117, minY_117, maxY_117, minZ_117, maxZ_117, a_73, a_68, b_116, minX_115, maxX_115, minY_115, maxY_115, minZ_115, maxZ_115;
        Expect_isTrue((b_121 = ((a_70 = Pnt_$ctor_Z7AD9E565(3, 3, 3), (b_117 = Pnt_$ctor_Z7AD9E565(8, 8, 8), (minX_117 = a_70.X, (maxX_117 = ((b_117.X > minX_117) ? b_117.X : ((minX_117 = b_117.X, a_70.X))), (minY_117 = a_70.Y, (maxY_117 = ((b_117.Y > minY_117) ? b_117.Y : ((minY_117 = b_117.Y, a_70.Y))), (minZ_117 = a_70.Z, (maxZ_117 = ((b_117.Z > minZ_117) ? b_117.Z : ((minZ_117 = b_117.Z, a_70.Z))), BBox_$ctor_76A78260(minX_117, minY_117, minZ_117, maxX_117, maxY_117, maxZ_117)))))))))), (a_73 = ((a_68 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_116 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (minX_115 = a_68.X, (maxX_115 = ((b_116.X > minX_115) ? b_116.X : ((minX_115 = b_116.X, a_68.X))), (minY_115 = a_68.Y, (maxY_115 = ((b_116.Y > minY_115) ? b_116.Y : ((minY_115 = b_116.Y, a_68.Y))), (minZ_115 = a_68.Z, (maxZ_115 = ((b_116.Z > minZ_115) ? b_116.Z : ((minZ_115 = b_116.Z, a_68.Z))), BBox_$ctor_76A78260(minX_115, minY_115, minZ_115, maxX_115, maxY_115, maxZ_115)))))))))), !((((((b_121.MinX > a_73.MaxX) ? true : (a_73.MinX > b_121.MaxX)) ? true : (a_73.MinY > b_121.MaxY)) ? true : (b_121.MinY > a_73.MaxY)) ? true : (a_73.MinZ > b_121.MaxZ)) ? true : (b_121.MinZ > a_73.MaxZ)))))("Static method should work");
        Test_TestCaseBuilder__Zero(builder$0040_46);
    }));
})(), (() => {
    const builder$0040_47 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersection - overlapping boxes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_47, Test_TestCaseBuilder__Delay_1505(builder$0040_47, () => {
        let copyOfStruct_124, arg_129, arg_1_124, copyOfStruct_125, arg_130, arg_1_125, copyOfStruct_126, arg_131, arg_1_126, copyOfStruct_127, arg_132, arg_1_127, copyOfStruct_128, arg_133, arg_1_128, copyOfStruct_129, arg_134, arg_1_129;
        let matchValue;
        let b_125;
        const a_74 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_122 = Pnt_$ctor_Z7AD9E565(5, 5, 5);
        let minX_119 = a_74.X;
        let maxX_119;
        if (b_122.X > minX_119) {
            maxX_119 = b_122.X;
        }
        else {
            minX_119 = b_122.X;
            maxX_119 = a_74.X;
        }
        let minY_119 = a_74.Y;
        let maxY_119;
        if (b_122.Y > minY_119) {
            maxY_119 = b_122.Y;
        }
        else {
            minY_119 = b_122.Y;
            maxY_119 = a_74.Y;
        }
        let minZ_119 = a_74.Z;
        let maxZ_119;
        if (b_122.Z > minZ_119) {
            maxZ_119 = b_122.Z;
        }
        else {
            minZ_119 = b_122.Z;
            maxZ_119 = a_74.Z;
        }
        b_125 = BBox_$ctor_76A78260(minX_119, minY_119, minZ_119, maxX_119, maxY_119, maxZ_119);
        let a_77;
        const a_76 = Pnt_$ctor_Z7AD9E565(3, 3, 3);
        const b_123 = Pnt_$ctor_Z7AD9E565(8, 8, 8);
        let minX_121 = a_76.X;
        let maxX_121;
        if (b_123.X > minX_121) {
            maxX_121 = b_123.X;
        }
        else {
            minX_121 = b_123.X;
            maxX_121 = a_76.X;
        }
        let minY_121 = a_76.Y;
        let maxY_121;
        if (b_123.Y > minY_121) {
            maxY_121 = b_123.Y;
        }
        else {
            minY_121 = b_123.Y;
            maxY_121 = a_76.Y;
        }
        let minZ_121 = a_76.Z;
        let maxZ_121;
        if (b_123.Z > minZ_121) {
            maxZ_121 = b_123.Z;
        }
        else {
            minZ_121 = b_123.Z;
            maxZ_121 = a_76.Z;
        }
        a_77 = BBox_$ctor_76A78260(minX_121, minY_121, minZ_121, maxX_121, maxY_121, maxZ_121);
        const minX_123 = max(a_77.MinX, b_125.MinX);
        const minY_123 = max(a_77.MinY, b_125.MinY);
        const minZ_123 = max(a_77.MinZ, b_125.MinZ);
        const maxX_123 = min(a_77.MaxX, b_125.MaxX);
        const maxY_123 = min(a_77.MaxY, b_125.MaxY);
        const maxZ_123 = min(a_77.MaxZ, b_125.MaxZ);
        matchValue = ((((minX_123 <= maxX_123) && (minY_123 <= maxY_123)) && (minZ_123 <= maxZ_123)) ? BBox_$ctor_76A78260(minX_123, minY_123, minZ_123, maxX_123, maxY_123, maxZ_123) : undefined);
        if (matchValue == null) {
            throw new Exception("Should have intersection");
            Test_TestCaseBuilder__Zero(builder$0040_47);
        }
        else {
            const intersect = matchValue;
            const actual_124 = intersect.MinX;
            if ((actual_124 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_124, 3, "Intersection MinX should be 3");
            }
            else {
                throw new Exception(contains((copyOfStruct_124 = actual_124, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                    Equals: equals,
                    GetHashCode: (x_142) => (structuralHash(x_142) | 0),
                }) ? ((arg_129 = (3).toString(), (arg_1_124 = actual_124.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_129)(arg_1_124)("Intersection MinX should be 3")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_124)("Intersection MinX should be 3"));
            }
            const actual_125 = intersect.MinY;
            if ((actual_125 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_125, 3, "Intersection MinY should be 3");
            }
            else {
                throw new Exception(contains((copyOfStruct_125 = actual_125, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                    Equals: equals,
                    GetHashCode: (x_143) => (structuralHash(x_143) | 0),
                }) ? ((arg_130 = (3).toString(), (arg_1_125 = actual_125.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_130)(arg_1_125)("Intersection MinY should be 3")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_125)("Intersection MinY should be 3"));
            }
            const actual_126 = intersect.MinZ;
            if ((actual_126 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_126, 3, "Intersection MinZ should be 3");
            }
            else {
                throw new Exception(contains((copyOfStruct_126 = actual_126, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                    Equals: equals,
                    GetHashCode: (x_144) => (structuralHash(x_144) | 0),
                }) ? ((arg_131 = (3).toString(), (arg_1_126 = actual_126.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_131)(arg_1_126)("Intersection MinZ should be 3")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_126)("Intersection MinZ should be 3"));
            }
            const actual_127 = intersect.MaxX;
            if ((actual_127 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_127, 5, "Intersection MaxX should be 5");
            }
            else {
                throw new Exception(contains((copyOfStruct_127 = actual_127, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                    Equals: equals,
                    GetHashCode: (x_145) => (structuralHash(x_145) | 0),
                }) ? ((arg_132 = (5).toString(), (arg_1_127 = actual_127.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_132)(arg_1_127)("Intersection MaxX should be 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_127)("Intersection MaxX should be 5"));
            }
            const actual_128 = intersect.MaxY;
            if ((actual_128 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_128, 5, "Intersection MaxY should be 5");
            }
            else {
                throw new Exception(contains((copyOfStruct_128 = actual_128, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                    Equals: equals,
                    GetHashCode: (x_146) => (structuralHash(x_146) | 0),
                }) ? ((arg_133 = (5).toString(), (arg_1_128 = actual_128.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_133)(arg_1_128)("Intersection MaxY should be 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_128)("Intersection MaxY should be 5"));
            }
            const actual_129 = intersect.MaxZ;
            if ((actual_129 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_129, 5, "Intersection MaxZ should be 5");
            }
            else {
                throw new Exception(contains((copyOfStruct_129 = actual_129, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                    Equals: equals,
                    GetHashCode: (x_147) => (structuralHash(x_147) | 0),
                }) ? ((arg_134 = (5).toString(), (arg_1_129 = actual_129.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_134)(arg_1_129)("Intersection MaxZ should be 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_129)("Intersection MaxZ should be 5"));
            }
            Test_TestCaseBuilder__Zero(builder$0040_47);
        }
    }));
})(), (() => {
    const builder$0040_48 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersection - touching boxes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_48, Test_TestCaseBuilder__Delay_1505(builder$0040_48, () => {
        let b_131;
        let matchValue_1;
        let b_129;
        const a_78 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_126 = Pnt_$ctor_Z7AD9E565(5, 5, 5);
        let minX_125 = a_78.X;
        let maxX_125;
        if (b_126.X > minX_125) {
            maxX_125 = b_126.X;
        }
        else {
            minX_125 = b_126.X;
            maxX_125 = a_78.X;
        }
        let minY_125 = a_78.Y;
        let maxY_125;
        if (b_126.Y > minY_125) {
            maxY_125 = b_126.Y;
        }
        else {
            minY_125 = b_126.Y;
            maxY_125 = a_78.Y;
        }
        let minZ_125 = a_78.Z;
        let maxZ_125;
        if (b_126.Z > minZ_125) {
            maxZ_125 = b_126.Z;
        }
        else {
            minZ_125 = b_126.Z;
            maxZ_125 = a_78.Z;
        }
        b_129 = BBox_$ctor_76A78260(minX_125, minY_125, minZ_125, maxX_125, maxY_125, maxZ_125);
        let a_81;
        const a_80 = Pnt_$ctor_Z7AD9E565(5, 0, 0);
        const b_127 = Pnt_$ctor_Z7AD9E565(10, 5, 5);
        let minX_127 = a_80.X;
        let maxX_127;
        if (b_127.X > minX_127) {
            maxX_127 = b_127.X;
        }
        else {
            minX_127 = b_127.X;
            maxX_127 = a_80.X;
        }
        let minY_127 = a_80.Y;
        let maxY_127;
        if (b_127.Y > minY_127) {
            maxY_127 = b_127.Y;
        }
        else {
            minY_127 = b_127.Y;
            maxY_127 = a_80.Y;
        }
        let minZ_127 = a_80.Z;
        let maxZ_127;
        if (b_127.Z > minZ_127) {
            maxZ_127 = b_127.Z;
        }
        else {
            minZ_127 = b_127.Z;
            maxZ_127 = a_80.Z;
        }
        a_81 = BBox_$ctor_76A78260(minX_127, minY_127, minZ_127, maxX_127, maxY_127, maxZ_127);
        const minX_129 = max(a_81.MinX, b_129.MinX);
        const minY_129 = max(a_81.MinY, b_129.MinY);
        const minZ_129 = max(a_81.MinZ, b_129.MinZ);
        const maxX_129 = min(a_81.MaxX, b_129.MaxX);
        const maxY_129 = min(a_81.MaxY, b_129.MaxY);
        const maxZ_129 = min(a_81.MaxZ, b_129.MaxZ);
        matchValue_1 = ((((minX_129 <= maxX_129) && (minY_129 <= maxY_129)) && (minZ_129 <= maxZ_129)) ? BBox_$ctor_76A78260(minX_129, minY_129, minZ_129, maxX_129, maxY_129, maxZ_129) : undefined);
        if (matchValue_1 == null) {
            throw new Exception("Should have intersection (touching)");
            Test_TestCaseBuilder__Zero(builder$0040_48);
        }
        else {
            Expect_isTrue(((b_131 = matchValue_1, ((((b_131.MaxX - b_131.MinX) > 1E-12) ? 0 : 1) + (((b_131.MaxY - b_131.MinY) > 1E-12) ? 0 : 1)) + (((b_131.MaxZ - b_131.MinZ) > 1E-12) ? 0 : 1))) === 1)("Intersection should be flat");
            Test_TestCaseBuilder__Zero(builder$0040_48);
        }
    }));
})(), (() => {
    const builder$0040_49 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersection - separated boxes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_49, Test_TestCaseBuilder__Delay_1505(builder$0040_49, () => {
        let matchValue_2;
        let b_135;
        const a_82 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_132 = Pnt_$ctor_Z7AD9E565(5, 5, 5);
        let minX_131 = a_82.X;
        let maxX_131;
        if (b_132.X > minX_131) {
            maxX_131 = b_132.X;
        }
        else {
            minX_131 = b_132.X;
            maxX_131 = a_82.X;
        }
        let minY_131 = a_82.Y;
        let maxY_131;
        if (b_132.Y > minY_131) {
            maxY_131 = b_132.Y;
        }
        else {
            minY_131 = b_132.Y;
            maxY_131 = a_82.Y;
        }
        let minZ_131 = a_82.Z;
        let maxZ_131;
        if (b_132.Z > minZ_131) {
            maxZ_131 = b_132.Z;
        }
        else {
            minZ_131 = b_132.Z;
            maxZ_131 = a_82.Z;
        }
        b_135 = BBox_$ctor_76A78260(minX_131, minY_131, minZ_131, maxX_131, maxY_131, maxZ_131);
        let a_85;
        const a_84 = Pnt_$ctor_Z7AD9E565(6, 6, 6);
        const b_133 = Pnt_$ctor_Z7AD9E565(10, 10, 10);
        let minX_133 = a_84.X;
        let maxX_133;
        if (b_133.X > minX_133) {
            maxX_133 = b_133.X;
        }
        else {
            minX_133 = b_133.X;
            maxX_133 = a_84.X;
        }
        let minY_133 = a_84.Y;
        let maxY_133;
        if (b_133.Y > minY_133) {
            maxY_133 = b_133.Y;
        }
        else {
            minY_133 = b_133.Y;
            maxY_133 = a_84.Y;
        }
        let minZ_133 = a_84.Z;
        let maxZ_133;
        if (b_133.Z > minZ_133) {
            maxZ_133 = b_133.Z;
        }
        else {
            minZ_133 = b_133.Z;
            maxZ_133 = a_84.Z;
        }
        a_85 = BBox_$ctor_76A78260(minX_133, minY_133, minZ_133, maxX_133, maxY_133, maxZ_133);
        const minX_135 = max(a_85.MinX, b_135.MinX);
        const minY_135 = max(a_85.MinY, b_135.MinY);
        const minZ_135 = max(a_85.MinZ, b_135.MinZ);
        const maxX_135 = min(a_85.MaxX, b_135.MaxX);
        const maxY_135 = min(a_85.MaxY, b_135.MaxY);
        const maxZ_135 = min(a_85.MaxZ, b_135.MaxZ);
        matchValue_2 = ((((minX_135 <= maxX_135) && (minY_135 <= maxY_135)) && (minZ_135 <= maxZ_135)) ? BBox_$ctor_76A78260(minX_135, minY_135, minZ_135, maxX_135, maxY_135, maxZ_135) : undefined);
        if (matchValue_2 == null) {
            Test_TestCaseBuilder__Zero(builder$0040_49);
        }
        else {
            throw new Exception("Should not have intersection");
            Test_TestCaseBuilder__Zero(builder$0040_49);
        }
    }));
})(), (() => {
    const builder$0040_50 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("intersection static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_50, Test_TestCaseBuilder__Delay_1505(builder$0040_50, () => {
        let copyOfStruct_130, arg_135, arg_1_130;
        let matchValue_3;
        let b_141;
        const a_86 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_136 = Pnt_$ctor_Z7AD9E565(5, 5, 5);
        let minX_137 = a_86.X;
        let maxX_137;
        if (b_136.X > minX_137) {
            maxX_137 = b_136.X;
        }
        else {
            minX_137 = b_136.X;
            maxX_137 = a_86.X;
        }
        let minY_137 = a_86.Y;
        let maxY_137;
        if (b_136.Y > minY_137) {
            maxY_137 = b_136.Y;
        }
        else {
            minY_137 = b_136.Y;
            maxY_137 = a_86.Y;
        }
        let minZ_137 = a_86.Z;
        let maxZ_137;
        if (b_136.Z > minZ_137) {
            maxZ_137 = b_136.Z;
        }
        else {
            minZ_137 = b_136.Z;
            maxZ_137 = a_86.Z;
        }
        b_141 = BBox_$ctor_76A78260(minX_137, minY_137, minZ_137, maxX_137, maxY_137, maxZ_137);
        let a_91;
        const a_88 = Pnt_$ctor_Z7AD9E565(3, 3, 3);
        const b_137 = Pnt_$ctor_Z7AD9E565(8, 8, 8);
        let minX_139 = a_88.X;
        let maxX_139;
        if (b_137.X > minX_139) {
            maxX_139 = b_137.X;
        }
        else {
            minX_139 = b_137.X;
            maxX_139 = a_88.X;
        }
        let minY_139 = a_88.Y;
        let maxY_139;
        if (b_137.Y > minY_139) {
            maxY_139 = b_137.Y;
        }
        else {
            minY_139 = b_137.Y;
            maxY_139 = a_88.Y;
        }
        let minZ_139 = a_88.Z;
        let maxZ_139;
        if (b_137.Z > minZ_139) {
            maxZ_139 = b_137.Z;
        }
        else {
            minZ_139 = b_137.Z;
            maxZ_139 = a_88.Z;
        }
        a_91 = BBox_$ctor_76A78260(minX_139, minY_139, minZ_139, maxX_139, maxY_139, maxZ_139);
        const minX_141 = max(a_91.MinX, b_141.MinX);
        const minY_141 = max(a_91.MinY, b_141.MinY);
        const minZ_141 = max(a_91.MinZ, b_141.MinZ);
        const maxX_141 = min(a_91.MaxX, b_141.MaxX);
        const maxY_141 = min(a_91.MaxY, b_141.MaxY);
        const maxZ_141 = min(a_91.MaxZ, b_141.MaxZ);
        matchValue_3 = ((((minX_141 <= maxX_141) && (minY_141 <= maxY_141)) && (minZ_141 <= maxZ_141)) ? BBox_$ctor_76A78260(minX_141, minY_141, minZ_141, maxX_141, maxY_141, maxZ_141) : undefined);
        if (matchValue_3 == null) {
            throw new Exception("Should have intersection");
            Test_TestCaseBuilder__Zero(builder$0040_50);
        }
        else {
            const actual_130 = matchValue_3.MinX;
            if ((actual_130 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_130, 3, "Intersection MinX should be 3");
            }
            else {
                throw new Exception(contains((copyOfStruct_130 = actual_130, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                    Equals: equals,
                    GetHashCode: (x_151) => (structuralHash(x_151) | 0),
                }) ? ((arg_135 = (3).toString(), (arg_1_130 = actual_130.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_135)(arg_1_130)("Intersection MinX should be 3")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_130)("Intersection MinX should be 3"));
            }
            Test_TestCaseBuilder__Zero(builder$0040_50);
        }
    }));
})(), (() => {
    const builder$0040_51 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching - boxes touching on face", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_51, Test_TestCaseBuilder__Delay_1505(builder$0040_51, () => {
        let a_94, b_143, minX_145, maxX_145, minY_145, maxY_145, minZ_145, maxZ_145, a_92, b_142, minX_143, maxX_143, minY_143, maxY_143, minZ_143, maxZ_143;
        Expect_isTrue(BBox__IsTouching_668B4595((a_92 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_142 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (minX_143 = a_92.X, (maxX_143 = ((b_142.X > minX_143) ? b_142.X : ((minX_143 = b_142.X, a_92.X))), (minY_143 = a_92.Y, (maxY_143 = ((b_142.Y > minY_143) ? b_142.Y : ((minY_143 = b_142.Y, a_92.Y))), (minZ_143 = a_92.Z, (maxZ_143 = ((b_142.Z > minZ_143) ? b_142.Z : ((minZ_143 = b_142.Z, a_92.Z))), BBox_$ctor_76A78260(minX_143, minY_143, minZ_143, maxX_143, maxY_143, maxZ_143))))))))), (a_94 = Pnt_$ctor_Z7AD9E565(5, 0, 0), (b_143 = Pnt_$ctor_Z7AD9E565(10, 5, 5), (minX_145 = a_94.X, (maxX_145 = ((b_143.X > minX_145) ? b_143.X : ((minX_145 = b_143.X, a_94.X))), (minY_145 = a_94.Y, (maxY_145 = ((b_143.Y > minY_145) ? b_143.Y : ((minY_145 = b_143.Y, a_94.Y))), (minZ_145 = a_94.Z, (maxZ_145 = ((b_143.Z > minZ_145) ? b_143.Z : ((minZ_145 = b_143.Z, a_94.Z))), BBox_$ctor_76A78260(minX_145, minY_145, minZ_145, maxX_145, maxY_145, maxZ_145))))))))), 0.01))("Boxes should be touching");
        Test_TestCaseBuilder__Zero(builder$0040_51);
    }));
})(), (() => {
    const builder$0040_52 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching - boxes overlapping (not just touching)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_52, Test_TestCaseBuilder__Delay_1505(builder$0040_52, () => {
        let a_97, b_146, minX_149, maxX_149, minY_149, maxY_149, minZ_149, maxZ_149, a_95, b_145, minX_147, maxX_147, minY_147, maxY_147, minZ_147, maxZ_147;
        Expect_isFalse(BBox__IsTouching_668B4595((a_95 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_145 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (minX_147 = a_95.X, (maxX_147 = ((b_145.X > minX_147) ? b_145.X : ((minX_147 = b_145.X, a_95.X))), (minY_147 = a_95.Y, (maxY_147 = ((b_145.Y > minY_147) ? b_145.Y : ((minY_147 = b_145.Y, a_95.Y))), (minZ_147 = a_95.Z, (maxZ_147 = ((b_145.Z > minZ_147) ? b_145.Z : ((minZ_147 = b_145.Z, a_95.Z))), BBox_$ctor_76A78260(minX_147, minY_147, minZ_147, maxX_147, maxY_147, maxZ_147))))))))), (a_97 = Pnt_$ctor_Z7AD9E565(3, 3, 3), (b_146 = Pnt_$ctor_Z7AD9E565(8, 8, 8), (minX_149 = a_97.X, (maxX_149 = ((b_146.X > minX_149) ? b_146.X : ((minX_149 = b_146.X, a_97.X))), (minY_149 = a_97.Y, (maxY_149 = ((b_146.Y > minY_149) ? b_146.Y : ((minY_149 = b_146.Y, a_97.Y))), (minZ_149 = a_97.Z, (maxZ_149 = ((b_146.Z > minZ_149) ? b_146.Z : ((minZ_149 = b_146.Z, a_97.Z))), BBox_$ctor_76A78260(minX_149, minY_149, minZ_149, maxX_149, maxY_149, maxZ_149))))))))), 0.01))("Overlapping boxes are not just touching");
        Test_TestCaseBuilder__Zero(builder$0040_52);
    }));
})(), (() => {
    const builder$0040_53 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching - boxes separated", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_53, Test_TestCaseBuilder__Delay_1505(builder$0040_53, () => {
        let a_100, b_149, minX_153, maxX_153, minY_153, maxY_153, minZ_153, maxZ_153, a_98, b_148, minX_151, maxX_151, minY_151, maxY_151, minZ_151, maxZ_151;
        Expect_isFalse(BBox__IsTouching_668B4595((a_98 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_148 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (minX_151 = a_98.X, (maxX_151 = ((b_148.X > minX_151) ? b_148.X : ((minX_151 = b_148.X, a_98.X))), (minY_151 = a_98.Y, (maxY_151 = ((b_148.Y > minY_151) ? b_148.Y : ((minY_151 = b_148.Y, a_98.Y))), (minZ_151 = a_98.Z, (maxZ_151 = ((b_148.Z > minZ_151) ? b_148.Z : ((minZ_151 = b_148.Z, a_98.Z))), BBox_$ctor_76A78260(minX_151, minY_151, minZ_151, maxX_151, maxY_151, maxZ_151))))))))), (a_100 = Pnt_$ctor_Z7AD9E565(6, 6, 6), (b_149 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_153 = a_100.X, (maxX_153 = ((b_149.X > minX_153) ? b_149.X : ((minX_153 = b_149.X, a_100.X))), (minY_153 = a_100.Y, (maxY_153 = ((b_149.Y > minY_153) ? b_149.Y : ((minY_153 = b_149.Y, a_100.Y))), (minZ_153 = a_100.Z, (maxZ_153 = ((b_149.Z > minZ_153) ? b_149.Z : ((minZ_153 = b_149.Z, a_100.Z))), BBox_$ctor_76A78260(minX_153, minY_153, minZ_153, maxX_153, maxY_153, maxZ_153))))))))), 0.01))("Separated boxes are not touching");
        Test_TestCaseBuilder__Zero(builder$0040_53);
    }));
})(), (() => {
    const builder$0040_54 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching - touching on Z face (top-bottom)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_54, Test_TestCaseBuilder__Delay_1505(builder$0040_54, () => {
        let a_102;
        const a_101 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_151 = Pnt_$ctor_Z7AD9E565(5, 5, 5);
        let minX_155 = a_101.X;
        let maxX_155;
        if (b_151.X > minX_155) {
            maxX_155 = b_151.X;
        }
        else {
            minX_155 = b_151.X;
            maxX_155 = a_101.X;
        }
        let minY_155 = a_101.Y;
        let maxY_155;
        if (b_151.Y > minY_155) {
            maxY_155 = b_151.Y;
        }
        else {
            minY_155 = b_151.Y;
            maxY_155 = a_101.Y;
        }
        let minZ_155 = a_101.Z;
        let maxZ_155;
        if (b_151.Z > minZ_155) {
            maxZ_155 = b_151.Z;
        }
        else {
            minZ_155 = b_151.Z;
            maxZ_155 = a_101.Z;
        }
        a_102 = BBox_$ctor_76A78260(minX_155, minY_155, minZ_155, maxX_155, maxY_155, maxZ_155);
        let b_153;
        const a_103 = Pnt_$ctor_Z7AD9E565(0, 0, 5);
        const b_152 = Pnt_$ctor_Z7AD9E565(5, 5, 10);
        let minX_157 = a_103.X;
        let maxX_157;
        if (b_152.X > minX_157) {
            maxX_157 = b_152.X;
        }
        else {
            minX_157 = b_152.X;
            maxX_157 = a_103.X;
        }
        let minY_157 = a_103.Y;
        let maxY_157;
        if (b_152.Y > minY_157) {
            maxY_157 = b_152.Y;
        }
        else {
            minY_157 = b_152.Y;
            maxY_157 = a_103.Y;
        }
        let minZ_157 = a_103.Z;
        let maxZ_157;
        if (b_152.Z > minZ_157) {
            maxZ_157 = b_152.Z;
        }
        else {
            minZ_157 = b_152.Z;
            maxZ_157 = a_103.Z;
        }
        b_153 = BBox_$ctor_76A78260(minX_157, minY_157, minZ_157, maxX_157, maxY_157, maxZ_157);
        Expect_isTrue(BBox__IsTouching_668B4595(a_102, b_153, 1E-06))("Boxes touching on Z face should be touching");
        Expect_isTrue(BBox__IsTouching_668B4595(b_153, a_102, 1E-06))("IsTouching should be symmetric");
        Test_TestCaseBuilder__Zero(builder$0040_54);
    }));
})(), (() => {
    const builder$0040_55 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching - touching on Y face (front-back)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_55, Test_TestCaseBuilder__Delay_1505(builder$0040_55, () => {
        let a_105;
        const a_104 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_154 = Pnt_$ctor_Z7AD9E565(5, 5, 5);
        let minX_159 = a_104.X;
        let maxX_159;
        if (b_154.X > minX_159) {
            maxX_159 = b_154.X;
        }
        else {
            minX_159 = b_154.X;
            maxX_159 = a_104.X;
        }
        let minY_159 = a_104.Y;
        let maxY_159;
        if (b_154.Y > minY_159) {
            maxY_159 = b_154.Y;
        }
        else {
            minY_159 = b_154.Y;
            maxY_159 = a_104.Y;
        }
        let minZ_159 = a_104.Z;
        let maxZ_159;
        if (b_154.Z > minZ_159) {
            maxZ_159 = b_154.Z;
        }
        else {
            minZ_159 = b_154.Z;
            maxZ_159 = a_104.Z;
        }
        a_105 = BBox_$ctor_76A78260(minX_159, minY_159, minZ_159, maxX_159, maxY_159, maxZ_159);
        let b_156;
        const a_106 = Pnt_$ctor_Z7AD9E565(0, 5, 0);
        const b_155 = Pnt_$ctor_Z7AD9E565(5, 10, 5);
        let minX_161 = a_106.X;
        let maxX_161;
        if (b_155.X > minX_161) {
            maxX_161 = b_155.X;
        }
        else {
            minX_161 = b_155.X;
            maxX_161 = a_106.X;
        }
        let minY_161 = a_106.Y;
        let maxY_161;
        if (b_155.Y > minY_161) {
            maxY_161 = b_155.Y;
        }
        else {
            minY_161 = b_155.Y;
            maxY_161 = a_106.Y;
        }
        let minZ_161 = a_106.Z;
        let maxZ_161;
        if (b_155.Z > minZ_161) {
            maxZ_161 = b_155.Z;
        }
        else {
            minZ_161 = b_155.Z;
            maxZ_161 = a_106.Z;
        }
        b_156 = BBox_$ctor_76A78260(minX_161, minY_161, minZ_161, maxX_161, maxY_161, maxZ_161);
        Expect_isTrue(BBox__IsTouching_668B4595(a_105, b_156, 1E-06))("Boxes touching on Y face should be touching");
        Expect_isTrue(BBox__IsTouching_668B4595(b_156, a_105, 1E-06))("IsTouching should be symmetric");
        Test_TestCaseBuilder__Zero(builder$0040_55);
    }));
})(), (() => {
    const builder$0040_56 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching - edge touching (X-edge)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_56, Test_TestCaseBuilder__Delay_1505(builder$0040_56, () => {
        let a_109, b_158, minX_165, maxX_165, minY_165, maxY_165, minZ_165, maxZ_165, a_107, b_157, minX_163, maxX_163, minY_163, maxY_163, minZ_163, maxZ_163;
        Expect_isTrue(BBox__IsTouching_668B4595((a_107 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_157 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (minX_163 = a_107.X, (maxX_163 = ((b_157.X > minX_163) ? b_157.X : ((minX_163 = b_157.X, a_107.X))), (minY_163 = a_107.Y, (maxY_163 = ((b_157.Y > minY_163) ? b_157.Y : ((minY_163 = b_157.Y, a_107.Y))), (minZ_163 = a_107.Z, (maxZ_163 = ((b_157.Z > minZ_163) ? b_157.Z : ((minZ_163 = b_157.Z, a_107.Z))), BBox_$ctor_76A78260(minX_163, minY_163, minZ_163, maxX_163, maxY_163, maxZ_163))))))))), (a_109 = Pnt_$ctor_Z7AD9E565(0, 5, 5), (b_158 = Pnt_$ctor_Z7AD9E565(5, 10, 10), (minX_165 = a_109.X, (maxX_165 = ((b_158.X > minX_165) ? b_158.X : ((minX_165 = b_158.X, a_109.X))), (minY_165 = a_109.Y, (maxY_165 = ((b_158.Y > minY_165) ? b_158.Y : ((minY_165 = b_158.Y, a_109.Y))), (minZ_165 = a_109.Z, (maxZ_165 = ((b_158.Z > minZ_165) ? b_158.Z : ((minZ_165 = b_158.Z, a_109.Z))), BBox_$ctor_76A78260(minX_165, minY_165, minZ_165, maxX_165, maxY_165, maxZ_165))))))))), 1E-06))("Boxes touching along an edge should be touching");
        Test_TestCaseBuilder__Zero(builder$0040_56);
    }));
})(), (() => {
    const builder$0040_57 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching - edge touching (Y-edge)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_57, Test_TestCaseBuilder__Delay_1505(builder$0040_57, () => {
        let a_112, b_161, minX_169, maxX_169, minY_169, maxY_169, minZ_169, maxZ_169, a_110, b_160, minX_167, maxX_167, minY_167, maxY_167, minZ_167, maxZ_167;
        Expect_isTrue(BBox__IsTouching_668B4595((a_110 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_160 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (minX_167 = a_110.X, (maxX_167 = ((b_160.X > minX_167) ? b_160.X : ((minX_167 = b_160.X, a_110.X))), (minY_167 = a_110.Y, (maxY_167 = ((b_160.Y > minY_167) ? b_160.Y : ((minY_167 = b_160.Y, a_110.Y))), (minZ_167 = a_110.Z, (maxZ_167 = ((b_160.Z > minZ_167) ? b_160.Z : ((minZ_167 = b_160.Z, a_110.Z))), BBox_$ctor_76A78260(minX_167, minY_167, minZ_167, maxX_167, maxY_167, maxZ_167))))))))), (a_112 = Pnt_$ctor_Z7AD9E565(5, 0, 5), (b_161 = Pnt_$ctor_Z7AD9E565(10, 5, 10), (minX_169 = a_112.X, (maxX_169 = ((b_161.X > minX_169) ? b_161.X : ((minX_169 = b_161.X, a_112.X))), (minY_169 = a_112.Y, (maxY_169 = ((b_161.Y > minY_169) ? b_161.Y : ((minY_169 = b_161.Y, a_112.Y))), (minZ_169 = a_112.Z, (maxZ_169 = ((b_161.Z > minZ_169) ? b_161.Z : ((minZ_169 = b_161.Z, a_112.Z))), BBox_$ctor_76A78260(minX_169, minY_169, minZ_169, maxX_169, maxY_169, maxZ_169))))))))), 1E-06))("Boxes touching along Y-edge should be touching");
        Test_TestCaseBuilder__Zero(builder$0040_57);
    }));
})(), (() => {
    const builder$0040_58 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching - edge touching (Z-edge)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_58, Test_TestCaseBuilder__Delay_1505(builder$0040_58, () => {
        let a_115, b_164, minX_173, maxX_173, minY_173, maxY_173, minZ_173, maxZ_173, a_113, b_163, minX_171, maxX_171, minY_171, maxY_171, minZ_171, maxZ_171;
        Expect_isTrue(BBox__IsTouching_668B4595((a_113 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_163 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (minX_171 = a_113.X, (maxX_171 = ((b_163.X > minX_171) ? b_163.X : ((minX_171 = b_163.X, a_113.X))), (minY_171 = a_113.Y, (maxY_171 = ((b_163.Y > minY_171) ? b_163.Y : ((minY_171 = b_163.Y, a_113.Y))), (minZ_171 = a_113.Z, (maxZ_171 = ((b_163.Z > minZ_171) ? b_163.Z : ((minZ_171 = b_163.Z, a_113.Z))), BBox_$ctor_76A78260(minX_171, minY_171, minZ_171, maxX_171, maxY_171, maxZ_171))))))))), (a_115 = Pnt_$ctor_Z7AD9E565(5, 5, 0), (b_164 = Pnt_$ctor_Z7AD9E565(10, 10, 5), (minX_173 = a_115.X, (maxX_173 = ((b_164.X > minX_173) ? b_164.X : ((minX_173 = b_164.X, a_115.X))), (minY_173 = a_115.Y, (maxY_173 = ((b_164.Y > minY_173) ? b_164.Y : ((minY_173 = b_164.Y, a_115.Y))), (minZ_173 = a_115.Z, (maxZ_173 = ((b_164.Z > minZ_173) ? b_164.Z : ((minZ_173 = b_164.Z, a_115.Z))), BBox_$ctor_76A78260(minX_173, minY_173, minZ_173, maxX_173, maxY_173, maxZ_173))))))))), 1E-06))("Boxes touching along Z-edge should be touching");
        Test_TestCaseBuilder__Zero(builder$0040_58);
    }));
})(), (() => {
    const builder$0040_59 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching - corner touching (single point)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_59, Test_TestCaseBuilder__Delay_1505(builder$0040_59, () => {
        let a_117;
        const a_116 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_166 = Pnt_$ctor_Z7AD9E565(5, 5, 5);
        let minX_175 = a_116.X;
        let maxX_175;
        if (b_166.X > minX_175) {
            maxX_175 = b_166.X;
        }
        else {
            minX_175 = b_166.X;
            maxX_175 = a_116.X;
        }
        let minY_175 = a_116.Y;
        let maxY_175;
        if (b_166.Y > minY_175) {
            maxY_175 = b_166.Y;
        }
        else {
            minY_175 = b_166.Y;
            maxY_175 = a_116.Y;
        }
        let minZ_175 = a_116.Z;
        let maxZ_175;
        if (b_166.Z > minZ_175) {
            maxZ_175 = b_166.Z;
        }
        else {
            minZ_175 = b_166.Z;
            maxZ_175 = a_116.Z;
        }
        a_117 = BBox_$ctor_76A78260(minX_175, minY_175, minZ_175, maxX_175, maxY_175, maxZ_175);
        let b_168;
        const a_118 = Pnt_$ctor_Z7AD9E565(5, 5, 5);
        const b_167 = Pnt_$ctor_Z7AD9E565(10, 10, 10);
        let minX_177 = a_118.X;
        let maxX_177;
        if (b_167.X > minX_177) {
            maxX_177 = b_167.X;
        }
        else {
            minX_177 = b_167.X;
            maxX_177 = a_118.X;
        }
        let minY_177 = a_118.Y;
        let maxY_177;
        if (b_167.Y > minY_177) {
            maxY_177 = b_167.Y;
        }
        else {
            minY_177 = b_167.Y;
            maxY_177 = a_118.Y;
        }
        let minZ_177 = a_118.Z;
        let maxZ_177;
        if (b_167.Z > minZ_177) {
            maxZ_177 = b_167.Z;
        }
        else {
            minZ_177 = b_167.Z;
            maxZ_177 = a_118.Z;
        }
        b_168 = BBox_$ctor_76A78260(minX_177, minY_177, minZ_177, maxX_177, maxY_177, maxZ_177);
        Expect_isTrue(BBox__IsTouching_668B4595(a_117, b_168, 1E-06))("Boxes touching at a corner should be touching");
        Expect_isTrue(BBox__IsTouching_668B4595(b_168, a_117, 1E-06))("Corner touching should be symmetric");
        Test_TestCaseBuilder__Zero(builder$0040_59);
    }));
})(), (() => {
    const builder$0040_60 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching - within tolerance (barely touching)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_60, Test_TestCaseBuilder__Delay_1505(builder$0040_60, () => {
        let a_121, b_170, minX_181, maxX_181, minY_181, maxY_181, minZ_181, maxZ_181, a_119, b_169, minX_179, maxX_179, minY_179, maxY_179, minZ_179, maxZ_179;
        Expect_isTrue(BBox__IsTouching_668B4595((a_119 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_169 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (minX_179 = a_119.X, (maxX_179 = ((b_169.X > minX_179) ? b_169.X : ((minX_179 = b_169.X, a_119.X))), (minY_179 = a_119.Y, (maxY_179 = ((b_169.Y > minY_179) ? b_169.Y : ((minY_179 = b_169.Y, a_119.Y))), (minZ_179 = a_119.Z, (maxZ_179 = ((b_169.Z > minZ_179) ? b_169.Z : ((minZ_179 = b_169.Z, a_119.Z))), BBox_$ctor_76A78260(minX_179, minY_179, minZ_179, maxX_179, maxY_179, maxZ_179))))))))), (a_121 = Pnt_$ctor_Z7AD9E565(5.0000005, 0, 0), (b_170 = Pnt_$ctor_Z7AD9E565(10, 5, 5), (minX_181 = a_121.X, (maxX_181 = ((b_170.X > minX_181) ? b_170.X : ((minX_181 = b_170.X, a_121.X))), (minY_181 = a_121.Y, (maxY_181 = ((b_170.Y > minY_181) ? b_170.Y : ((minY_181 = b_170.Y, a_121.Y))), (minZ_181 = a_121.Z, (maxZ_181 = ((b_170.Z > minZ_181) ? b_170.Z : ((minZ_181 = b_170.Z, a_121.Z))), BBox_$ctor_76A78260(minX_181, minY_181, minZ_181, maxX_181, maxY_181, maxZ_181))))))))), 1E-06))("Boxes within tolerance should be touching");
        Test_TestCaseBuilder__Zero(builder$0040_60);
    }));
})(), (() => {
    const builder$0040_61 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching - just outside tolerance (not touching)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_61, Test_TestCaseBuilder__Delay_1505(builder$0040_61, () => {
        let a_124, b_173, minX_185, maxX_185, minY_185, maxY_185, minZ_185, maxZ_185, a_122, b_172, minX_183, maxX_183, minY_183, maxY_183, minZ_183, maxZ_183;
        Expect_isFalse(BBox__IsTouching_668B4595((a_122 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_172 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (minX_183 = a_122.X, (maxX_183 = ((b_172.X > minX_183) ? b_172.X : ((minX_183 = b_172.X, a_122.X))), (minY_183 = a_122.Y, (maxY_183 = ((b_172.Y > minY_183) ? b_172.Y : ((minY_183 = b_172.Y, a_122.Y))), (minZ_183 = a_122.Z, (maxZ_183 = ((b_172.Z > minZ_183) ? b_172.Z : ((minZ_183 = b_172.Z, a_122.Z))), BBox_$ctor_76A78260(minX_183, minY_183, minZ_183, maxX_183, maxY_183, maxZ_183))))))))), (a_124 = Pnt_$ctor_Z7AD9E565(5.00001, 0, 0), (b_173 = Pnt_$ctor_Z7AD9E565(10, 5, 5), (minX_185 = a_124.X, (maxX_185 = ((b_173.X > minX_185) ? b_173.X : ((minX_185 = b_173.X, a_124.X))), (minY_185 = a_124.Y, (maxY_185 = ((b_173.Y > minY_185) ? b_173.Y : ((minY_185 = b_173.Y, a_124.Y))), (minZ_185 = a_124.Z, (maxZ_185 = ((b_173.Z > minZ_185) ? b_173.Z : ((minZ_185 = b_173.Z, a_124.Z))), BBox_$ctor_76A78260(minX_185, minY_185, minZ_185, maxX_185, maxY_185, maxZ_185))))))))), 1E-06))("Boxes outside tolerance should not be touching");
        Test_TestCaseBuilder__Zero(builder$0040_61);
    }));
})(), (() => {
    const builder$0040_62 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching - identical boxes (overlapping, not touching)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_62, Test_TestCaseBuilder__Delay_1505(builder$0040_62, () => {
        let a_127, b_176, minX_189, maxX_189, minY_189, maxY_189, minZ_189, maxZ_189, a_125, b_175, minX_187, maxX_187, minY_187, maxY_187, minZ_187, maxZ_187;
        Expect_isFalse(BBox__IsTouching_668B4595((a_125 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_175 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (minX_187 = a_125.X, (maxX_187 = ((b_175.X > minX_187) ? b_175.X : ((minX_187 = b_175.X, a_125.X))), (minY_187 = a_125.Y, (maxY_187 = ((b_175.Y > minY_187) ? b_175.Y : ((minY_187 = b_175.Y, a_125.Y))), (minZ_187 = a_125.Z, (maxZ_187 = ((b_175.Z > minZ_187) ? b_175.Z : ((minZ_187 = b_175.Z, a_125.Z))), BBox_$ctor_76A78260(minX_187, minY_187, minZ_187, maxX_187, maxY_187, maxZ_187))))))))), (a_127 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_176 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (minX_189 = a_127.X, (maxX_189 = ((b_176.X > minX_189) ? b_176.X : ((minX_189 = b_176.X, a_127.X))), (minY_189 = a_127.Y, (maxY_189 = ((b_176.Y > minY_189) ? b_176.Y : ((minY_189 = b_176.Y, a_127.Y))), (minZ_189 = a_127.Z, (maxZ_189 = ((b_176.Z > minZ_189) ? b_176.Z : ((minZ_189 = b_176.Z, a_127.Z))), BBox_$ctor_76A78260(minX_189, minY_189, minZ_189, maxX_189, maxY_189, maxZ_189))))))))), 1E-06))("Identical boxes overlap, they don\'t just touch");
        Test_TestCaseBuilder__Zero(builder$0040_62);
    }));
})(), (() => {
    const builder$0040_63 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching - one box inside another (overlapping, not touching)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_63, Test_TestCaseBuilder__Delay_1505(builder$0040_63, () => {
        let outer;
        const a_128 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_178 = Pnt_$ctor_Z7AD9E565(10, 10, 10);
        let minX_191 = a_128.X;
        let maxX_191;
        if (b_178.X > minX_191) {
            maxX_191 = b_178.X;
        }
        else {
            minX_191 = b_178.X;
            maxX_191 = a_128.X;
        }
        let minY_191 = a_128.Y;
        let maxY_191;
        if (b_178.Y > minY_191) {
            maxY_191 = b_178.Y;
        }
        else {
            minY_191 = b_178.Y;
            maxY_191 = a_128.Y;
        }
        let minZ_191 = a_128.Z;
        let maxZ_191;
        if (b_178.Z > minZ_191) {
            maxZ_191 = b_178.Z;
        }
        else {
            minZ_191 = b_178.Z;
            maxZ_191 = a_128.Z;
        }
        outer = BBox_$ctor_76A78260(minX_191, minY_191, minZ_191, maxX_191, maxY_191, maxZ_191);
        let inner;
        const a_129 = Pnt_$ctor_Z7AD9E565(2, 2, 2);
        const b_179 = Pnt_$ctor_Z7AD9E565(8, 8, 8);
        let minX_193 = a_129.X;
        let maxX_193;
        if (b_179.X > minX_193) {
            maxX_193 = b_179.X;
        }
        else {
            minX_193 = b_179.X;
            maxX_193 = a_129.X;
        }
        let minY_193 = a_129.Y;
        let maxY_193;
        if (b_179.Y > minY_193) {
            maxY_193 = b_179.Y;
        }
        else {
            minY_193 = b_179.Y;
            maxY_193 = a_129.Y;
        }
        let minZ_193 = a_129.Z;
        let maxZ_193;
        if (b_179.Z > minZ_193) {
            maxZ_193 = b_179.Z;
        }
        else {
            minZ_193 = b_179.Z;
            maxZ_193 = a_129.Z;
        }
        inner = BBox_$ctor_76A78260(minX_193, minY_193, minZ_193, maxX_193, maxY_193, maxZ_193);
        Expect_isFalse(BBox__IsTouching_668B4595(outer, inner, 1E-06))("Contained box overlaps, doesn\'t touch");
        Expect_isFalse(BBox__IsTouching_668B4595(inner, outer, 1E-06))("IsTouching should be symmetric");
        Test_TestCaseBuilder__Zero(builder$0040_63);
    }));
})(), (() => {
    const builder$0040_64 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching - partial overlap (not just touching)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_64, Test_TestCaseBuilder__Delay_1505(builder$0040_64, () => {
        let a_132, b_181, minX_197, maxX_197, minY_197, maxY_197, minZ_197, maxZ_197, a_130, b_180, minX_195, maxX_195, minY_195, maxY_195, minZ_195, maxZ_195;
        Expect_isFalse(BBox__IsTouching_668B4595((a_130 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_180 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (minX_195 = a_130.X, (maxX_195 = ((b_180.X > minX_195) ? b_180.X : ((minX_195 = b_180.X, a_130.X))), (minY_195 = a_130.Y, (maxY_195 = ((b_180.Y > minY_195) ? b_180.Y : ((minY_195 = b_180.Y, a_130.Y))), (minZ_195 = a_130.Z, (maxZ_195 = ((b_180.Z > minZ_195) ? b_180.Z : ((minZ_195 = b_180.Z, a_130.Z))), BBox_$ctor_76A78260(minX_195, minY_195, minZ_195, maxX_195, maxY_195, maxZ_195))))))))), (a_132 = Pnt_$ctor_Z7AD9E565(4, 4, 4), (b_181 = Pnt_$ctor_Z7AD9E565(9, 9, 9), (minX_197 = a_132.X, (maxX_197 = ((b_181.X > minX_197) ? b_181.X : ((minX_197 = b_181.X, a_132.X))), (minY_197 = a_132.Y, (maxY_197 = ((b_181.Y > minY_197) ? b_181.Y : ((minY_197 = b_181.Y, a_132.Y))), (minZ_197 = a_132.Z, (maxZ_197 = ((b_181.Z > minZ_197) ? b_181.Z : ((minZ_197 = b_181.Z, a_132.Z))), BBox_$ctor_76A78260(minX_197, minY_197, minZ_197, maxX_197, maxY_197, maxZ_197))))))))), 1E-06))("Partially overlapping boxes are not just touching");
        Test_TestCaseBuilder__Zero(builder$0040_64);
    }));
})(), (() => {
    const builder$0040_65 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching - separated on all axes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_65, Test_TestCaseBuilder__Delay_1505(builder$0040_65, () => {
        let a_135, b_184, minX_201, maxX_201, minY_201, maxY_201, minZ_201, maxZ_201, a_133, b_183, minX_199, maxX_199, minY_199, maxY_199, minZ_199, maxZ_199;
        Expect_isFalse(BBox__IsTouching_668B4595((a_133 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_183 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (minX_199 = a_133.X, (maxX_199 = ((b_183.X > minX_199) ? b_183.X : ((minX_199 = b_183.X, a_133.X))), (minY_199 = a_133.Y, (maxY_199 = ((b_183.Y > minY_199) ? b_183.Y : ((minY_199 = b_183.Y, a_133.Y))), (minZ_199 = a_133.Z, (maxZ_199 = ((b_183.Z > minZ_199) ? b_183.Z : ((minZ_199 = b_183.Z, a_133.Z))), BBox_$ctor_76A78260(minX_199, minY_199, minZ_199, maxX_199, maxY_199, maxZ_199))))))))), (a_135 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (b_184 = Pnt_$ctor_Z7AD9E565(15, 15, 15), (minX_201 = a_135.X, (maxX_201 = ((b_184.X > minX_201) ? b_184.X : ((minX_201 = b_184.X, a_135.X))), (minY_201 = a_135.Y, (maxY_201 = ((b_184.Y > minY_201) ? b_184.Y : ((minY_201 = b_184.Y, a_135.Y))), (minZ_201 = a_135.Z, (maxZ_201 = ((b_184.Z > minZ_201) ? b_184.Z : ((minZ_201 = b_184.Z, a_135.Z))), BBox_$ctor_76A78260(minX_201, minY_201, minZ_201, maxX_201, maxY_201, maxZ_201))))))))), 1E-06))("Completely separated boxes are not touching");
        Test_TestCaseBuilder__Zero(builder$0040_65);
    }));
})(), (() => {
    const builder$0040_66 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching - separated on single axis only", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_66, Test_TestCaseBuilder__Delay_1505(builder$0040_66, () => {
        let a_138, b_187, minX_205, maxX_205, minY_205, maxY_205, minZ_205, maxZ_205, a_136, b_186, minX_203, maxX_203, minY_203, maxY_203, minZ_203, maxZ_203;
        Expect_isFalse(BBox__IsTouching_668B4595((a_136 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_186 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (minX_203 = a_136.X, (maxX_203 = ((b_186.X > minX_203) ? b_186.X : ((minX_203 = b_186.X, a_136.X))), (minY_203 = a_136.Y, (maxY_203 = ((b_186.Y > minY_203) ? b_186.Y : ((minY_203 = b_186.Y, a_136.Y))), (minZ_203 = a_136.Z, (maxZ_203 = ((b_186.Z > minZ_203) ? b_186.Z : ((minZ_203 = b_186.Z, a_136.Z))), BBox_$ctor_76A78260(minX_203, minY_203, minZ_203, maxX_203, maxY_203, maxZ_203))))))))), (a_138 = Pnt_$ctor_Z7AD9E565(0, 0, 6), (b_187 = Pnt_$ctor_Z7AD9E565(5, 5, 10), (minX_205 = a_138.X, (maxX_205 = ((b_187.X > minX_205) ? b_187.X : ((minX_205 = b_187.X, a_138.X))), (minY_205 = a_138.Y, (maxY_205 = ((b_187.Y > minY_205) ? b_187.Y : ((minY_205 = b_187.Y, a_138.Y))), (minZ_205 = a_138.Z, (maxZ_205 = ((b_187.Z > minZ_205) ? b_187.Z : ((minZ_205 = b_187.Z, a_138.Z))), BBox_$ctor_76A78260(minX_205, minY_205, minZ_205, maxX_205, maxY_205, maxZ_205))))))))), 1E-06))("Boxes separated on Z axis are not touching");
        Test_TestCaseBuilder__Zero(builder$0040_66);
    }));
})(), (() => {
    const builder$0040_67 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching - large tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_67, Test_TestCaseBuilder__Delay_1505(builder$0040_67, () => {
        let a_141, b_190, minX_209, maxX_209, minY_209, maxY_209, minZ_209, maxZ_209, a_139, b_189, minX_207, maxX_207, minY_207, maxY_207, minZ_207, maxZ_207;
        Expect_isTrue(BBox__IsTouching_668B4595((a_139 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_189 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (minX_207 = a_139.X, (maxX_207 = ((b_189.X > minX_207) ? b_189.X : ((minX_207 = b_189.X, a_139.X))), (minY_207 = a_139.Y, (maxY_207 = ((b_189.Y > minY_207) ? b_189.Y : ((minY_207 = b_189.Y, a_139.Y))), (minZ_207 = a_139.Z, (maxZ_207 = ((b_189.Z > minZ_207) ? b_189.Z : ((minZ_207 = b_189.Z, a_139.Z))), BBox_$ctor_76A78260(minX_207, minY_207, minZ_207, maxX_207, maxY_207, maxZ_207))))))))), (a_141 = Pnt_$ctor_Z7AD9E565(5.5, 0, 0), (b_190 = Pnt_$ctor_Z7AD9E565(10, 5, 5), (minX_209 = a_141.X, (maxX_209 = ((b_190.X > minX_209) ? b_190.X : ((minX_209 = b_190.X, a_141.X))), (minY_209 = a_141.Y, (maxY_209 = ((b_190.Y > minY_209) ? b_190.Y : ((minY_209 = b_190.Y, a_141.Y))), (minZ_209 = a_141.Z, (maxZ_209 = ((b_190.Z > minZ_209) ? b_190.Z : ((minZ_209 = b_190.Z, a_141.Z))), BBox_$ctor_76A78260(minX_209, minY_209, minZ_209, maxX_209, maxY_209, maxZ_209))))))))), 1))("Boxes within large tolerance should be touching");
        Test_TestCaseBuilder__Zero(builder$0040_67);
    }));
})(), (() => {
    const builder$0040_68 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching - zero-size box touching face", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_68, Test_TestCaseBuilder__Delay_1505(builder$0040_68, () => {
        let a_144, b_193, minX_213, maxX_213, minY_213, maxY_213, minZ_213, maxZ_213, a_142, b_192, minX_211, maxX_211, minY_211, maxY_211, minZ_211, maxZ_211;
        Expect_isTrue(BBox__IsTouching_668B4595((a_142 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_192 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (minX_211 = a_142.X, (maxX_211 = ((b_192.X > minX_211) ? b_192.X : ((minX_211 = b_192.X, a_142.X))), (minY_211 = a_142.Y, (maxY_211 = ((b_192.Y > minY_211) ? b_192.Y : ((minY_211 = b_192.Y, a_142.Y))), (minZ_211 = a_142.Z, (maxZ_211 = ((b_192.Z > minZ_211) ? b_192.Z : ((minZ_211 = b_192.Z, a_142.Z))), BBox_$ctor_76A78260(minX_211, minY_211, minZ_211, maxX_211, maxY_211, maxZ_211))))))))), (a_144 = Pnt_$ctor_Z7AD9E565(5, 2, 2), (b_193 = Pnt_$ctor_Z7AD9E565(5, 3, 3), (minX_213 = a_144.X, (maxX_213 = ((b_193.X > minX_213) ? b_193.X : ((minX_213 = b_193.X, a_144.X))), (minY_213 = a_144.Y, (maxY_213 = ((b_193.Y > minY_213) ? b_193.Y : ((minY_213 = b_193.Y, a_144.Y))), (minZ_213 = a_144.Z, (maxZ_213 = ((b_193.Z > minZ_213) ? b_193.Z : ((minZ_213 = b_193.Z, a_144.Z))), BBox_$ctor_76A78260(minX_213, minY_213, minZ_213, maxX_213, maxY_213, maxZ_213))))))))), 1E-06))("Flat box on face should be touching");
        Test_TestCaseBuilder__Zero(builder$0040_68);
    }));
})(), (() => {
    const builder$0040_69 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching - default tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_69, Test_TestCaseBuilder__Delay_1505(builder$0040_69, () => {
        let a_147, b_196, minX_217, maxX_217, minY_217, maxY_217, minZ_217, maxZ_217, a_145, b_195, minX_215, maxX_215, minY_215, maxY_215, minZ_215, maxZ_215;
        Expect_isTrue(BBox__IsTouching_668B4595((a_145 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_195 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (minX_215 = a_145.X, (maxX_215 = ((b_195.X > minX_215) ? b_195.X : ((minX_215 = b_195.X, a_145.X))), (minY_215 = a_145.Y, (maxY_215 = ((b_195.Y > minY_215) ? b_195.Y : ((minY_215 = b_195.Y, a_145.Y))), (minZ_215 = a_145.Z, (maxZ_215 = ((b_195.Z > minZ_215) ? b_195.Z : ((minZ_215 = b_195.Z, a_145.Z))), BBox_$ctor_76A78260(minX_215, minY_215, minZ_215, maxX_215, maxY_215, maxZ_215))))))))), (a_147 = Pnt_$ctor_Z7AD9E565(5, 0, 0), (b_196 = Pnt_$ctor_Z7AD9E565(10, 5, 5), (minX_217 = a_147.X, (maxX_217 = ((b_196.X > minX_217) ? b_196.X : ((minX_217 = b_196.X, a_147.X))), (minY_217 = a_147.Y, (maxY_217 = ((b_196.Y > minY_217) ? b_196.Y : ((minY_217 = b_196.Y, a_147.Y))), (minZ_217 = a_147.Z, (maxZ_217 = ((b_196.Z > minZ_217) ? b_196.Z : ((minZ_217 = b_196.Z, a_147.Z))), BBox_$ctor_76A78260(minX_217, minY_217, minZ_217, maxX_217, maxY_217, maxZ_217))))))))), 1E-06))("Should work with default tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_69);
    }));
})()])), Test_testList("Union", ofArray([(() => {
    const builder$0040_70 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Union of two boxes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_70, Test_TestCaseBuilder__Delay_1505(builder$0040_70, () => {
        let copyOfStruct_131, arg_136, arg_1_131, copyOfStruct_132, arg_137, arg_1_132, copyOfStruct_133, arg_138, arg_1_133, copyOfStruct_134, arg_139, arg_1_134, copyOfStruct_135, arg_140, arg_1_135, copyOfStruct_136, arg_141, arg_1_136;
        let union;
        let b_201;
        const a_148 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_198 = Pnt_$ctor_Z7AD9E565(5, 5, 5);
        let minX_219 = a_148.X;
        let maxX_219;
        if (b_198.X > minX_219) {
            maxX_219 = b_198.X;
        }
        else {
            minX_219 = b_198.X;
            maxX_219 = a_148.X;
        }
        let minY_219 = a_148.Y;
        let maxY_219;
        if (b_198.Y > minY_219) {
            maxY_219 = b_198.Y;
        }
        else {
            minY_219 = b_198.Y;
            maxY_219 = a_148.Y;
        }
        let minZ_219 = a_148.Z;
        let maxZ_219;
        if (b_198.Z > minZ_219) {
            maxZ_219 = b_198.Z;
        }
        else {
            minZ_219 = b_198.Z;
            maxZ_219 = a_148.Z;
        }
        b_201 = BBox_$ctor_76A78260(minX_219, minY_219, minZ_219, maxX_219, maxY_219, maxZ_219);
        let a_151;
        const a_150 = Pnt_$ctor_Z7AD9E565(3, 3, 3);
        const b_199 = Pnt_$ctor_Z7AD9E565(8, 8, 8);
        let minX_221 = a_150.X;
        let maxX_221;
        if (b_199.X > minX_221) {
            maxX_221 = b_199.X;
        }
        else {
            minX_221 = b_199.X;
            maxX_221 = a_150.X;
        }
        let minY_221 = a_150.Y;
        let maxY_221;
        if (b_199.Y > minY_221) {
            maxY_221 = b_199.Y;
        }
        else {
            minY_221 = b_199.Y;
            maxY_221 = a_150.Y;
        }
        let minZ_221 = a_150.Z;
        let maxZ_221;
        if (b_199.Z > minZ_221) {
            maxZ_221 = b_199.Z;
        }
        else {
            minZ_221 = b_199.Z;
            maxZ_221 = a_150.Z;
        }
        a_151 = BBox_$ctor_76A78260(minX_221, minY_221, minZ_221, maxX_221, maxY_221, maxZ_221);
        union = BBox_$ctor_76A78260(min(b_201.MinX, a_151.MinX), min(b_201.MinY, a_151.MinY), min(b_201.MinZ, a_151.MinZ), max(b_201.MaxX, a_151.MaxX), max(b_201.MaxY, a_151.MaxY), max(b_201.MaxZ, a_151.MaxZ));
        const actual_131 = union.MinX;
        if ((actual_131 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_131, 0, "Union MinX should be 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_131 = actual_131, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_152) => (structuralHash(x_152) | 0),
            }) ? ((arg_136 = (0).toString(), (arg_1_131 = actual_131.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_136)(arg_1_131)("Union MinX should be 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_131)("Union MinX should be 0"));
        }
        const actual_132 = union.MinY;
        if ((actual_132 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_132, 0, "Union MinY should be 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_132 = actual_132, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_153) => (structuralHash(x_153) | 0),
            }) ? ((arg_137 = (0).toString(), (arg_1_132 = actual_132.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_137)(arg_1_132)("Union MinY should be 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_132)("Union MinY should be 0"));
        }
        const actual_133 = union.MinZ;
        if ((actual_133 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_133, 0, "Union MinZ should be 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_133 = actual_133, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_154) => (structuralHash(x_154) | 0),
            }) ? ((arg_138 = (0).toString(), (arg_1_133 = actual_133.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_138)(arg_1_133)("Union MinZ should be 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_133)("Union MinZ should be 0"));
        }
        const actual_134 = union.MaxX;
        if ((actual_134 === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_134, 8, "Union MaxX should be 8");
        }
        else {
            throw new Exception(contains((copyOfStruct_134 = actual_134, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_155) => (structuralHash(x_155) | 0),
            }) ? ((arg_139 = (8).toString(), (arg_1_134 = actual_134.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_139)(arg_1_134)("Union MaxX should be 8")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual_134)("Union MaxX should be 8"));
        }
        const actual_135 = union.MaxY;
        if ((actual_135 === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_135, 8, "Union MaxY should be 8");
        }
        else {
            throw new Exception(contains((copyOfStruct_135 = actual_135, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_156) => (structuralHash(x_156) | 0),
            }) ? ((arg_140 = (8).toString(), (arg_1_135 = actual_135.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_140)(arg_1_135)("Union MaxY should be 8")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual_135)("Union MaxY should be 8"));
        }
        const actual_136 = union.MaxZ;
        if ((actual_136 === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_136, 8, "Union MaxZ should be 8");
        }
        else {
            throw new Exception(contains((copyOfStruct_136 = actual_136, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_157) => (structuralHash(x_157) | 0),
            }) ? ((arg_141 = (8).toString(), (arg_1_136 = actual_136.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_141)(arg_1_136)("Union MaxZ should be 8")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual_136)("Union MaxZ should be 8"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_70);
    }));
})(), (() => {
    const builder$0040_71 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Union static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_71, Test_TestCaseBuilder__Delay_1505(builder$0040_71, () => {
        let a_156, a_152, b_202, minX_224, maxX_224, minY_224, maxY_224, minZ_224, maxZ_224, b_206, a_154, b_203, minX_226, maxX_226, minY_226, maxY_226, minZ_226, maxZ_226, copyOfStruct_137, arg_142, arg_1_137;
        const actual_137 = ((a_156 = ((a_152 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_202 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (minX_224 = a_152.X, (maxX_224 = ((b_202.X > minX_224) ? b_202.X : ((minX_224 = b_202.X, a_152.X))), (minY_224 = a_152.Y, (maxY_224 = ((b_202.Y > minY_224) ? b_202.Y : ((minY_224 = b_202.Y, a_152.Y))), (minZ_224 = a_152.Z, (maxZ_224 = ((b_202.Z > minZ_224) ? b_202.Z : ((minZ_224 = b_202.Z, a_152.Z))), BBox_$ctor_76A78260(minX_224, minY_224, minZ_224, maxX_224, maxY_224, maxZ_224)))))))))), (b_206 = ((a_154 = Pnt_$ctor_Z7AD9E565(3, 3, 3), (b_203 = Pnt_$ctor_Z7AD9E565(8, 8, 8), (minX_226 = a_154.X, (maxX_226 = ((b_203.X > minX_226) ? b_203.X : ((minX_226 = b_203.X, a_154.X))), (minY_226 = a_154.Y, (maxY_226 = ((b_203.Y > minY_226) ? b_203.Y : ((minY_226 = b_203.Y, a_154.Y))), (minZ_226 = a_154.Z, (maxZ_226 = ((b_203.Z > minZ_226) ? b_203.Z : ((minZ_226 = b_203.Z, a_154.Z))), BBox_$ctor_76A78260(minX_226, minY_226, minZ_226, maxX_226, maxY_226, maxZ_226)))))))))), BBox_$ctor_76A78260(min(b_206.MinX, a_156.MinX), min(b_206.MinY, a_156.MinY), min(b_206.MinZ, a_156.MinZ), max(b_206.MaxX, a_156.MaxX), max(b_206.MaxY, a_156.MaxY), max(b_206.MaxZ, a_156.MaxZ))))).MaxX;
        if ((actual_137 === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_137, 8, "Union MaxX should be 8");
        }
        else {
            throw new Exception(contains((copyOfStruct_137 = actual_137, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_158) => (structuralHash(x_158) | 0),
            }) ? ((arg_142 = (8).toString(), (arg_1_137 = actual_137.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_142)(arg_1_137)("Union MaxX should be 8")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual_137)("Union MaxX should be 8"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_71);
    }));
})(), (() => {
    const builder$0040_72 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Union with point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_72, Test_TestCaseBuilder__Delay_1505(builder$0040_72, () => {
        let copyOfStruct_138, arg_143, arg_1_138, copyOfStruct_139, arg_144, arg_1_139, copyOfStruct_140, arg_145, arg_1_140, copyOfStruct_141, arg_146, arg_1_141;
        let union_2;
        let b_208;
        const a_157 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_207 = Pnt_$ctor_Z7AD9E565(5, 5, 5);
        let minX_229 = a_157.X;
        let maxX_229;
        if (b_207.X > minX_229) {
            maxX_229 = b_207.X;
        }
        else {
            minX_229 = b_207.X;
            maxX_229 = a_157.X;
        }
        let minY_229 = a_157.Y;
        let maxY_229;
        if (b_207.Y > minY_229) {
            maxY_229 = b_207.Y;
        }
        else {
            minY_229 = b_207.Y;
            maxY_229 = a_157.Y;
        }
        let minZ_229 = a_157.Z;
        let maxZ_229;
        if (b_207.Z > minZ_229) {
            maxZ_229 = b_207.Z;
        }
        else {
            minZ_229 = b_207.Z;
            maxZ_229 = a_157.Z;
        }
        b_208 = BBox_$ctor_76A78260(minX_229, minY_229, minZ_229, maxX_229, maxY_229, maxZ_229);
        const p_5 = Pnt_$ctor_Z7AD9E565(8, 3, 2);
        union_2 = BBox_$ctor_76A78260(min(b_208.MinX, p_5.X), min(b_208.MinY, p_5.Y), min(b_208.MinZ, p_5.Z), max(b_208.MaxX, p_5.X), max(b_208.MaxY, p_5.Y), max(b_208.MaxZ, p_5.Z));
        const actual_138 = union_2.MinX;
        if ((actual_138 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_138, 0, "Union MinX should be 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_138 = actual_138, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_159) => (structuralHash(x_159) | 0),
            }) ? ((arg_143 = (0).toString(), (arg_1_138 = actual_138.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_143)(arg_1_138)("Union MinX should be 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_138)("Union MinX should be 0"));
        }
        const actual_139 = union_2.MaxX;
        if ((actual_139 === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_139, 8, "Union MaxX should be 8");
        }
        else {
            throw new Exception(contains((copyOfStruct_139 = actual_139, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_160) => (structuralHash(x_160) | 0),
            }) ? ((arg_144 = (8).toString(), (arg_1_139 = actual_139.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_144)(arg_1_139)("Union MaxX should be 8")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual_139)("Union MaxX should be 8"));
        }
        const actual_140 = union_2.MinY;
        if ((actual_140 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_140, 0, "Union MinY should be 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_140 = actual_140, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_161) => (structuralHash(x_161) | 0),
            }) ? ((arg_145 = (0).toString(), (arg_1_140 = actual_140.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_145)(arg_1_140)("Union MinY should be 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_140)("Union MinY should be 0"));
        }
        const actual_141 = union_2.MaxY;
        if ((actual_141 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_141, 5, "Union MaxY should be 5");
        }
        else {
            throw new Exception(contains((copyOfStruct_141 = actual_141, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_162) => (structuralHash(x_162) | 0),
            }) ? ((arg_146 = (5).toString(), (arg_1_141 = actual_141.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_146)(arg_1_141)("Union MaxY should be 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_141)("Union MaxY should be 5"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_72);
    }));
})(), (() => {
    const builder$0040_73 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("unionPt static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_73, Test_TestCaseBuilder__Delay_1505(builder$0040_73, () => {
        let p_7, b_211, copyOfStruct_142, arg_147, arg_1_142;
        let box_36;
        const a_158 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_209 = Pnt_$ctor_Z7AD9E565(5, 5, 5);
        let minX_232 = a_158.X;
        let maxX_232;
        if (b_209.X > minX_232) {
            maxX_232 = b_209.X;
        }
        else {
            minX_232 = b_209.X;
            maxX_232 = a_158.X;
        }
        let minY_232 = a_158.Y;
        let maxY_232;
        if (b_209.Y > minY_232) {
            maxY_232 = b_209.Y;
        }
        else {
            minY_232 = b_209.Y;
            maxY_232 = a_158.Y;
        }
        let minZ_232 = a_158.Z;
        let maxZ_232;
        if (b_209.Z > minZ_232) {
            maxZ_232 = b_209.Z;
        }
        else {
            minZ_232 = b_209.Z;
            maxZ_232 = a_158.Z;
        }
        box_36 = BBox_$ctor_76A78260(minX_232, minY_232, minZ_232, maxX_232, maxY_232, maxZ_232);
        const actual_142 = ((p_7 = Pnt_$ctor_Z7AD9E565(8, 3, 2), (b_211 = box_36, BBox_$ctor_76A78260(min(b_211.MinX, p_7.X), min(b_211.MinY, p_7.Y), min(b_211.MinZ, p_7.Z), max(b_211.MaxX, p_7.X), max(b_211.MaxY, p_7.Y), max(b_211.MaxZ, p_7.Z))))).MaxX;
        if ((actual_142 === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_142, 8, "Union MaxX should be 8");
        }
        else {
            throw new Exception(contains((copyOfStruct_142 = actual_142, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_163) => (structuralHash(x_163) | 0),
            }) ? ((arg_147 = (8).toString(), (arg_1_142 = actual_142.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_147)(arg_1_142)("Union MaxX should be 8")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual_142)("Union MaxX should be 8"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_73);
    }));
})()])), Test_testList("Containment", ofArray([(() => {
    const builder$0040_74 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Contains point inside", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_74, Test_TestCaseBuilder__Delay_1505(builder$0040_74, () => {
        let b_213, a_159, b_212, minX_235, maxX_235, minY_235, maxY_235, minZ_235, maxZ_235, p_8;
        Expect_isTrue((b_213 = ((a_159 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_212 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_235 = a_159.X, (maxX_235 = ((b_212.X > minX_235) ? b_212.X : ((minX_235 = b_212.X, a_159.X))), (minY_235 = a_159.Y, (maxY_235 = ((b_212.Y > minY_235) ? b_212.Y : ((minY_235 = b_212.Y, a_159.Y))), (minZ_235 = a_159.Z, (maxZ_235 = ((b_212.Z > minZ_235) ? b_212.Z : ((minZ_235 = b_212.Z, a_159.Z))), BBox_$ctor_76A78260(minX_235, minY_235, minZ_235, maxX_235, maxY_235, maxZ_235)))))))))), (p_8 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (((((p_8.X >= b_213.MinX) && (p_8.X <= b_213.MaxX)) && (p_8.Y >= b_213.MinY)) && (p_8.Y <= b_213.MaxY)) && (p_8.Z >= b_213.MinZ)) && (p_8.Z <= b_213.MaxZ))))("Box should contain point inside");
        Test_TestCaseBuilder__Zero(builder$0040_74);
    }));
})(), (() => {
    const builder$0040_75 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Contains point on boundary", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_75, Test_TestCaseBuilder__Delay_1505(builder$0040_75, () => {
        let b_215, a_160, b_214, minX_237, maxX_237, minY_237, maxY_237, minZ_237, maxZ_237, p_9;
        Expect_isTrue((b_215 = ((a_160 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_214 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_237 = a_160.X, (maxX_237 = ((b_214.X > minX_237) ? b_214.X : ((minX_237 = b_214.X, a_160.X))), (minY_237 = a_160.Y, (maxY_237 = ((b_214.Y > minY_237) ? b_214.Y : ((minY_237 = b_214.Y, a_160.Y))), (minZ_237 = a_160.Z, (maxZ_237 = ((b_214.Z > minZ_237) ? b_214.Z : ((minZ_237 = b_214.Z, a_160.Z))), BBox_$ctor_76A78260(minX_237, minY_237, minZ_237, maxX_237, maxY_237, maxZ_237)))))))))), (p_9 = Pnt_$ctor_Z7AD9E565(10, 5, 5), (((((p_9.X >= b_215.MinX) && (p_9.X <= b_215.MaxX)) && (p_9.Y >= b_215.MinY)) && (p_9.Y <= b_215.MaxY)) && (p_9.Z >= b_215.MinZ)) && (p_9.Z <= b_215.MaxZ))))("Box should contain point on boundary");
        Test_TestCaseBuilder__Zero(builder$0040_75);
    }));
})(), (() => {
    const builder$0040_76 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Contains point outside", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_76, Test_TestCaseBuilder__Delay_1505(builder$0040_76, () => {
        let b_217, a_161, b_216, minX_239, maxX_239, minY_239, maxY_239, minZ_239, maxZ_239, p_10;
        Expect_isFalse((b_217 = ((a_161 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_216 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_239 = a_161.X, (maxX_239 = ((b_216.X > minX_239) ? b_216.X : ((minX_239 = b_216.X, a_161.X))), (minY_239 = a_161.Y, (maxY_239 = ((b_216.Y > minY_239) ? b_216.Y : ((minY_239 = b_216.Y, a_161.Y))), (minZ_239 = a_161.Z, (maxZ_239 = ((b_216.Z > minZ_239) ? b_216.Z : ((minZ_239 = b_216.Z, a_161.Z))), BBox_$ctor_76A78260(minX_239, minY_239, minZ_239, maxX_239, maxY_239, maxZ_239)))))))))), (p_10 = Pnt_$ctor_Z7AD9E565(11, 5, 5), (((((p_10.X >= b_217.MinX) && (p_10.X <= b_217.MaxX)) && (p_10.Y >= b_217.MinY)) && (p_10.Y <= b_217.MaxY)) && (p_10.Z >= b_217.MinZ)) && (p_10.Z <= b_217.MaxZ))))("Box should not contain point outside");
        Test_TestCaseBuilder__Zero(builder$0040_76);
    }));
})(), (() => {
    const builder$0040_77 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Contains box inside", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_77, Test_TestCaseBuilder__Delay_1505(builder$0040_77, () => {
        let b_220, a_162, b_218, minX_241, maxX_241, minY_241, maxY_241, minZ_241, maxZ_241, o, a_163, b_219, minX_243, maxX_243, minY_243, maxY_243, minZ_243, maxZ_243, b_222, p_11, b_221, b_224, p_12, b_223;
        Expect_isTrue((b_220 = ((a_162 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_218 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_241 = a_162.X, (maxX_241 = ((b_218.X > minX_241) ? b_218.X : ((minX_241 = b_218.X, a_162.X))), (minY_241 = a_162.Y, (maxY_241 = ((b_218.Y > minY_241) ? b_218.Y : ((minY_241 = b_218.Y, a_162.Y))), (minZ_241 = a_162.Z, (maxZ_241 = ((b_218.Z > minZ_241) ? b_218.Z : ((minZ_241 = b_218.Z, a_162.Z))), BBox_$ctor_76A78260(minX_241, minY_241, minZ_241, maxX_241, maxY_241, maxZ_241)))))))))), (o = ((a_163 = Pnt_$ctor_Z7AD9E565(2, 2, 2), (b_219 = Pnt_$ctor_Z7AD9E565(8, 8, 8), (minX_243 = a_163.X, (maxX_243 = ((b_219.X > minX_243) ? b_219.X : ((minX_243 = b_219.X, a_163.X))), (minY_243 = a_163.Y, (maxY_243 = ((b_219.Y > minY_243) ? b_219.Y : ((minY_243 = b_219.Y, a_163.Y))), (minZ_243 = a_163.Z, (maxZ_243 = ((b_219.Z > minZ_243) ? b_219.Z : ((minZ_243 = b_219.Z, a_163.Z))), BBox_$ctor_76A78260(minX_243, minY_243, minZ_243, maxX_243, maxY_243, maxZ_243)))))))))), ((b_222 = b_220, (p_11 = ((b_221 = o, Pnt_$ctor_Z7AD9E565_1(b_221.MinX, b_221.MinY, b_221.MinZ))), (((((p_11.X >= b_222.MinX) && (p_11.X <= b_222.MaxX)) && (p_11.Y >= b_222.MinY)) && (p_11.Y <= b_222.MaxY)) && (p_11.Z >= b_222.MinZ)) && (p_11.Z <= b_222.MaxZ)))) && ((b_224 = b_220, (p_12 = ((b_223 = o, Pnt_$ctor_Z7AD9E565_1(b_223.MaxX, b_223.MaxY, b_223.MaxZ))), (((((p_12.X >= b_224.MinX) && (p_12.X <= b_224.MaxX)) && (p_12.Y >= b_224.MinY)) && (p_12.Y <= b_224.MaxY)) && (p_12.Z >= b_224.MinZ)) && (p_12.Z <= b_224.MaxZ)))))))("Outer box should contain inner box");
        Test_TestCaseBuilder__Zero(builder$0040_77);
    }));
})(), (() => {
    const builder$0040_78 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Contains box outside", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_78, Test_TestCaseBuilder__Delay_1505(builder$0040_78, () => {
        let b_228, a_164, b_225, minX_245, maxX_245, minY_245, maxY_245, minZ_245, maxZ_245, o_1, a_166, b_226, minX_247, maxX_247, minY_247, maxY_247, minZ_247, maxZ_247, b_230, p_13, b_229, b_232, p_14, b_231;
        Expect_isFalse((b_228 = ((a_164 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_225 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (minX_245 = a_164.X, (maxX_245 = ((b_225.X > minX_245) ? b_225.X : ((minX_245 = b_225.X, a_164.X))), (minY_245 = a_164.Y, (maxY_245 = ((b_225.Y > minY_245) ? b_225.Y : ((minY_245 = b_225.Y, a_164.Y))), (minZ_245 = a_164.Z, (maxZ_245 = ((b_225.Z > minZ_245) ? b_225.Z : ((minZ_245 = b_225.Z, a_164.Z))), BBox_$ctor_76A78260(minX_245, minY_245, minZ_245, maxX_245, maxY_245, maxZ_245)))))))))), (o_1 = ((a_166 = Pnt_$ctor_Z7AD9E565(6, 6, 6), (b_226 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_247 = a_166.X, (maxX_247 = ((b_226.X > minX_247) ? b_226.X : ((minX_247 = b_226.X, a_166.X))), (minY_247 = a_166.Y, (maxY_247 = ((b_226.Y > minY_247) ? b_226.Y : ((minY_247 = b_226.Y, a_166.Y))), (minZ_247 = a_166.Z, (maxZ_247 = ((b_226.Z > minZ_247) ? b_226.Z : ((minZ_247 = b_226.Z, a_166.Z))), BBox_$ctor_76A78260(minX_247, minY_247, minZ_247, maxX_247, maxY_247, maxZ_247)))))))))), ((b_230 = b_228, (p_13 = ((b_229 = o_1, Pnt_$ctor_Z7AD9E565_1(b_229.MinX, b_229.MinY, b_229.MinZ))), (((((p_13.X >= b_230.MinX) && (p_13.X <= b_230.MaxX)) && (p_13.Y >= b_230.MinY)) && (p_13.Y <= b_230.MaxY)) && (p_13.Z >= b_230.MinZ)) && (p_13.Z <= b_230.MaxZ)))) && ((b_232 = b_228, (p_14 = ((b_231 = o_1, Pnt_$ctor_Z7AD9E565_1(b_231.MaxX, b_231.MaxY, b_231.MaxZ))), (((((p_14.X >= b_232.MinX) && (p_14.X <= b_232.MaxX)) && (p_14.Y >= b_232.MinY)) && (p_14.Y <= b_232.MaxY)) && (p_14.Z >= b_232.MinZ)) && (p_14.Z <= b_232.MaxZ)))))))("Box should not contain separated box");
        Test_TestCaseBuilder__Zero(builder$0040_78);
    }));
})(), (() => {
    const builder$0040_79 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("containsPnt static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_79, Test_TestCaseBuilder__Delay_1505(builder$0040_79, () => {
        let b_234, a_167, b_233, minX_249, maxX_249, minY_249, maxY_249, minZ_249, maxZ_249, p_15;
        Expect_isTrue((b_234 = ((a_167 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_233 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_249 = a_167.X, (maxX_249 = ((b_233.X > minX_249) ? b_233.X : ((minX_249 = b_233.X, a_167.X))), (minY_249 = a_167.Y, (maxY_249 = ((b_233.Y > minY_249) ? b_233.Y : ((minY_249 = b_233.Y, a_167.Y))), (minZ_249 = a_167.Z, (maxZ_249 = ((b_233.Z > minZ_249) ? b_233.Z : ((minZ_249 = b_233.Z, a_167.Z))), BBox_$ctor_76A78260(minX_249, minY_249, minZ_249, maxX_249, maxY_249, maxZ_249)))))))))), (p_15 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (((((p_15.X >= b_234.MinX) && (p_15.X <= b_234.MaxX)) && (p_15.Y >= b_234.MinY)) && (p_15.Y <= b_234.MaxY)) && (p_15.Z >= b_234.MinZ)) && (p_15.Z <= b_234.MaxZ))))("Static method should work");
        Test_TestCaseBuilder__Zero(builder$0040_79);
    }));
})(), (() => {
    const builder$0040_80 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("contains static method for box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_80, Test_TestCaseBuilder__Delay_1505(builder$0040_80, () => {
        let b_237, a_168, b_235, minX_251, maxX_251, minY_251, maxY_251, minZ_251, maxZ_251, o_2, a_169, b_236, minX_253, maxX_253, minY_253, maxY_253, minZ_253, maxZ_253, b_239, p_16, b_238, b_241, p_17, b_240;
        Expect_isTrue((b_237 = ((a_168 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_235 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_251 = a_168.X, (maxX_251 = ((b_235.X > minX_251) ? b_235.X : ((minX_251 = b_235.X, a_168.X))), (minY_251 = a_168.Y, (maxY_251 = ((b_235.Y > minY_251) ? b_235.Y : ((minY_251 = b_235.Y, a_168.Y))), (minZ_251 = a_168.Z, (maxZ_251 = ((b_235.Z > minZ_251) ? b_235.Z : ((minZ_251 = b_235.Z, a_168.Z))), BBox_$ctor_76A78260(minX_251, minY_251, minZ_251, maxX_251, maxY_251, maxZ_251)))))))))), (o_2 = ((a_169 = Pnt_$ctor_Z7AD9E565(2, 2, 2), (b_236 = Pnt_$ctor_Z7AD9E565(8, 8, 8), (minX_253 = a_169.X, (maxX_253 = ((b_236.X > minX_253) ? b_236.X : ((minX_253 = b_236.X, a_169.X))), (minY_253 = a_169.Y, (maxY_253 = ((b_236.Y > minY_253) ? b_236.Y : ((minY_253 = b_236.Y, a_169.Y))), (minZ_253 = a_169.Z, (maxZ_253 = ((b_236.Z > minZ_253) ? b_236.Z : ((minZ_253 = b_236.Z, a_169.Z))), BBox_$ctor_76A78260(minX_253, minY_253, minZ_253, maxX_253, maxY_253, maxZ_253)))))))))), ((b_239 = b_237, (p_16 = ((b_238 = o_2, Pnt_$ctor_Z7AD9E565_1(b_238.MinX, b_238.MinY, b_238.MinZ))), (((((p_16.X >= b_239.MinX) && (p_16.X <= b_239.MaxX)) && (p_16.Y >= b_239.MinY)) && (p_16.Y <= b_239.MaxY)) && (p_16.Z >= b_239.MinZ)) && (p_16.Z <= b_239.MaxZ)))) && ((b_241 = b_237, (p_17 = ((b_240 = o_2, Pnt_$ctor_Z7AD9E565_1(b_240.MaxX, b_240.MaxY, b_240.MaxZ))), (((((p_17.X >= b_241.MinX) && (p_17.X <= b_241.MaxX)) && (p_17.Y >= b_241.MinY)) && (p_17.Y <= b_241.MaxY)) && (p_17.Z >= b_241.MinZ)) && (p_17.Z <= b_241.MaxZ)))))))("Static method should work");
        Test_TestCaseBuilder__Zero(builder$0040_80);
    }));
})()])), Test_testList("Points and Edges", ofArray([(() => {
    const builder$0040_81 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Points array has 8 points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_81, Test_TestCaseBuilder__Delay_1505(builder$0040_81, () => {
        let a_170, b_242, minX_255, maxX_255, minY_255, maxY_255, minZ_255, maxZ_255, copyOfStruct_143, arg_148, arg_1_143;
        const pts_3 = BBox__get_Points((a_170 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_242 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_255 = a_170.X, (maxX_255 = ((b_242.X > minX_255) ? b_242.X : ((minX_255 = b_242.X, a_170.X))), (minY_255 = a_170.Y, (maxY_255 = ((b_242.Y > minY_255) ? b_242.Y : ((minY_255 = b_242.Y, a_170.Y))), (minZ_255 = a_170.Z, (maxZ_255 = ((b_242.Z > minZ_255) ? b_242.Z : ((minZ_255 = b_242.Z, a_170.Z))), BBox_$ctor_76A78260(minX_255, minY_255, minZ_255, maxX_255, maxY_255, maxZ_255))))))))));
        const actual_143 = pts_3.length | 0;
        if ((actual_143 === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_143, 8, "Should have 8 points");
        }
        else {
            throw new Exception(contains((copyOfStruct_143 = actual_143, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_164) => (structuralHash(x_164) | 0),
            }) ? ((arg_148 = int32ToString(8), (arg_1_143 = int32ToString(actual_143), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_148)(arg_1_143)("Should have 8 points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual_143)("Should have 8 points"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_81);
    }));
})(), (() => {
    const builder$0040_82 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Pt0 is MinPnt", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_82, Test_TestCaseBuilder__Delay_1505(builder$0040_82, () => {
        let a_173, b_244, b_247, b_245, x_165, y_150, z_6;
        let box_44;
        const a_171 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        const b_243 = Pnt_$ctor_Z7AD9E565(5, 7, 9);
        let minX_257 = a_171.X;
        let maxX_257;
        if (b_243.X > minX_257) {
            maxX_257 = b_243.X;
        }
        else {
            minX_257 = b_243.X;
            maxX_257 = a_171.X;
        }
        let minY_257 = a_171.Y;
        let maxY_257;
        if (b_243.Y > minY_257) {
            maxY_257 = b_243.Y;
        }
        else {
            minY_257 = b_243.Y;
            maxY_257 = a_171.Y;
        }
        let minZ_257 = a_171.Z;
        let maxZ_257;
        if (b_243.Z > minZ_257) {
            maxZ_257 = b_243.Z;
        }
        else {
            minZ_257 = b_243.Z;
            maxZ_257 = a_171.Z;
        }
        box_44 = BBox_$ctor_76A78260(minX_257, minY_257, minZ_257, maxX_257, maxY_257, maxZ_257);
        Expect_isTrue(((a_173 = ((b_244 = box_44, Pnt_$ctor_Z7AD9E565_1(b_244.MinX, b_244.MinY, b_244.MinZ))), (b_247 = ((b_245 = box_44, Pnt_$ctor_Z7AD9E565_1(b_245.MinX, b_245.MinY, b_245.MinZ))), (x_165 = (a_173.X - b_247.X), (y_150 = (a_173.Y - b_247.Y), (z_6 = (a_173.Z - b_247.Z), Math.sqrt(((x_165 * x_165) + (y_150 * y_150)) + (z_6 * z_6)))))))) < 1E-09)("Pt0 should equal MinPnt");
        Test_TestCaseBuilder__Zero(builder$0040_82);
    }));
})(), (() => {
    const builder$0040_83 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Pt6 is MaxPnt", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_83, Test_TestCaseBuilder__Delay_1505(builder$0040_83, () => {
        let a_176, b_249, b_252, b_250, x_166, y_151, z_7;
        let box_45;
        const a_174 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        const b_248 = Pnt_$ctor_Z7AD9E565(5, 7, 9);
        let minX_259 = a_174.X;
        let maxX_259;
        if (b_248.X > minX_259) {
            maxX_259 = b_248.X;
        }
        else {
            minX_259 = b_248.X;
            maxX_259 = a_174.X;
        }
        let minY_259 = a_174.Y;
        let maxY_259;
        if (b_248.Y > minY_259) {
            maxY_259 = b_248.Y;
        }
        else {
            minY_259 = b_248.Y;
            maxY_259 = a_174.Y;
        }
        let minZ_259 = a_174.Z;
        let maxZ_259;
        if (b_248.Z > minZ_259) {
            maxZ_259 = b_248.Z;
        }
        else {
            minZ_259 = b_248.Z;
            maxZ_259 = a_174.Z;
        }
        box_45 = BBox_$ctor_76A78260(minX_259, minY_259, minZ_259, maxX_259, maxY_259, maxZ_259);
        Expect_isTrue(((a_176 = ((b_249 = box_45, Pnt_$ctor_Z7AD9E565_1(b_249.MaxX, b_249.MaxY, b_249.MaxZ))), (b_252 = ((b_250 = box_45, Pnt_$ctor_Z7AD9E565_1(b_250.MaxX, b_250.MaxY, b_250.MaxZ))), (x_166 = (a_176.X - b_252.X), (y_151 = (a_176.Y - b_252.Y), (z_7 = (a_176.Z - b_252.Z), Math.sqrt(((x_166 * x_166) + (y_151 * y_151)) + (z_7 * z_7)))))))) < 1E-09)("Pt6 should equal MaxPnt");
        Test_TestCaseBuilder__Zero(builder$0040_83);
    }));
})(), (() => {
    const builder$0040_84 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Bottom points are counter-clockwise", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_84, Test_TestCaseBuilder__Delay_1505(builder$0040_84, () => {
        let a_177, b_253, minX_261, maxX_261, minY_261, maxY_261, minZ_261, maxZ_261, copyOfStruct_144, arg_149, arg_1_144, a_179, b_255, x_168, y_153, z_8, a_181, b_257, x_169, y_154, z_9, a_183, b_259, x_170, y_155, z_10, a_185, b_261, x_171, y_156, z_11;
        const bottom = BBox__get_BottomPoints((a_177 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_253 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_261 = a_177.X, (maxX_261 = ((b_253.X > minX_261) ? b_253.X : ((minX_261 = b_253.X, a_177.X))), (minY_261 = a_177.Y, (maxY_261 = ((b_253.Y > minY_261) ? b_253.Y : ((minY_261 = b_253.Y, a_177.Y))), (minZ_261 = a_177.Z, (maxZ_261 = ((b_253.Z > minZ_261) ? b_253.Z : ((minZ_261 = b_253.Z, a_177.Z))), BBox_$ctor_76A78260(minX_261, minY_261, minZ_261, maxX_261, maxY_261, maxZ_261))))))))));
        const actual_144 = bottom.length | 0;
        if ((actual_144 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_144, 4, "Should have 4 bottom points");
        }
        else {
            throw new Exception(contains((copyOfStruct_144 = actual_144, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_167) => (structuralHash(x_167) | 0),
            }) ? ((arg_149 = int32ToString(4), (arg_1_144 = int32ToString(actual_144), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_149)(arg_1_144)("Should have 4 bottom points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_144)("Should have 4 bottom points"));
        }
        Expect_isTrue(((a_179 = item(0, bottom), (b_255 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (x_168 = (a_179.X - b_255.X), (y_153 = (a_179.Y - b_255.Y), (z_8 = (a_179.Z - b_255.Z), Math.sqrt(((x_168 * x_168) + (y_153 * y_153)) + (z_8 * z_8)))))))) < 1E-09)("First point should be Pt0");
        Expect_isTrue(((a_181 = item(1, bottom), (b_257 = Pnt_$ctor_Z7AD9E565(10, 0, 0), (x_169 = (a_181.X - b_257.X), (y_154 = (a_181.Y - b_257.Y), (z_9 = (a_181.Z - b_257.Z), Math.sqrt(((x_169 * x_169) + (y_154 * y_154)) + (z_9 * z_9)))))))) < 1E-09)("Second point should be Pt1");
        Expect_isTrue(((a_183 = item(2, bottom), (b_259 = Pnt_$ctor_Z7AD9E565(10, 10, 0), (x_170 = (a_183.X - b_259.X), (y_155 = (a_183.Y - b_259.Y), (z_10 = (a_183.Z - b_259.Z), Math.sqrt(((x_170 * x_170) + (y_155 * y_155)) + (z_10 * z_10)))))))) < 1E-09)("Third point should be Pt2");
        Expect_isTrue(((a_185 = item(3, bottom), (b_261 = Pnt_$ctor_Z7AD9E565(0, 10, 0), (x_171 = (a_185.X - b_261.X), (y_156 = (a_185.Y - b_261.Y), (z_11 = (a_185.Z - b_261.Z), Math.sqrt(((x_171 * x_171) + (y_156 * y_156)) + (z_11 * z_11)))))))) < 1E-09)("Fourth point should be Pt3");
        Test_TestCaseBuilder__Zero(builder$0040_84);
    }));
})(), (() => {
    const builder$0040_85 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Bottom points looped has 5 points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_85, Test_TestCaseBuilder__Delay_1505(builder$0040_85, () => {
        let a_186, b_262, minX_263, maxX_263, minY_263, maxY_263, minZ_263, maxZ_263, copyOfStruct_145, arg_150, arg_1_145, a_188, b_264, x_173, y_158, z_12;
        const bottom_1 = BBox__get_BottomPointsLooped((a_186 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_262 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_263 = a_186.X, (maxX_263 = ((b_262.X > minX_263) ? b_262.X : ((minX_263 = b_262.X, a_186.X))), (minY_263 = a_186.Y, (maxY_263 = ((b_262.Y > minY_263) ? b_262.Y : ((minY_263 = b_262.Y, a_186.Y))), (minZ_263 = a_186.Z, (maxZ_263 = ((b_262.Z > minZ_263) ? b_262.Z : ((minZ_263 = b_262.Z, a_186.Z))), BBox_$ctor_76A78260(minX_263, minY_263, minZ_263, maxX_263, maxY_263, maxZ_263))))))))));
        const actual_145 = bottom_1.length | 0;
        if ((actual_145 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_145, 5, "Should have 5 points");
        }
        else {
            throw new Exception(contains((copyOfStruct_145 = actual_145, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_172) => (structuralHash(x_172) | 0),
            }) ? ((arg_150 = int32ToString(5), (arg_1_145 = int32ToString(actual_145), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_150)(arg_1_145)("Should have 5 points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_145)("Should have 5 points"));
        }
        Expect_isTrue(((a_188 = item(0, bottom_1), (b_264 = item(4, bottom_1), (x_173 = (a_188.X - b_264.X), (y_158 = (a_188.Y - b_264.Y), (z_12 = (a_188.Z - b_264.Z), Math.sqrt(((x_173 * x_173) + (y_158 * y_158)) + (z_12 * z_12)))))))) < 1E-09)("First and last should be same");
        Test_TestCaseBuilder__Zero(builder$0040_85);
    }));
})(), (() => {
    const builder$0040_86 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Top points are counter-clockwise", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_86, Test_TestCaseBuilder__Delay_1505(builder$0040_86, () => {
        let a_189, b_265, minX_265, maxX_265, minY_265, maxY_265, minZ_265, maxZ_265, copyOfStruct_146, arg_151, arg_1_146;
        const top = BBox__get_TopPoints((a_189 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_265 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_265 = a_189.X, (maxX_265 = ((b_265.X > minX_265) ? b_265.X : ((minX_265 = b_265.X, a_189.X))), (minY_265 = a_189.Y, (maxY_265 = ((b_265.Y > minY_265) ? b_265.Y : ((minY_265 = b_265.Y, a_189.Y))), (minZ_265 = a_189.Z, (maxZ_265 = ((b_265.Z > minZ_265) ? b_265.Z : ((minZ_265 = b_265.Z, a_189.Z))), BBox_$ctor_76A78260(minX_265, minY_265, minZ_265, maxX_265, maxY_265, maxZ_265))))))))));
        const actual_146 = top.length | 0;
        if ((actual_146 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_146, 4, "Should have 4 top points");
        }
        else {
            throw new Exception(contains((copyOfStruct_146 = actual_146, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_174) => (structuralHash(x_174) | 0),
            }) ? ((arg_151 = int32ToString(4), (arg_1_146 = int32ToString(actual_146), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_151)(arg_1_146)("Should have 4 top points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_146)("Should have 4 top points"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_86);
    }));
})(), (() => {
    const builder$0040_87 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Top points looped has 5 points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_87, Test_TestCaseBuilder__Delay_1505(builder$0040_87, () => {
        let a_190, b_266, minX_267, maxX_267, minY_267, maxY_267, minZ_267, maxZ_267, copyOfStruct_147, arg_152, arg_1_147, a_192, b_268, x_176, y_161, z_13;
        const top_1 = BBox__get_TopPointsLooped((a_190 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_266 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_267 = a_190.X, (maxX_267 = ((b_266.X > minX_267) ? b_266.X : ((minX_267 = b_266.X, a_190.X))), (minY_267 = a_190.Y, (maxY_267 = ((b_266.Y > minY_267) ? b_266.Y : ((minY_267 = b_266.Y, a_190.Y))), (minZ_267 = a_190.Z, (maxZ_267 = ((b_266.Z > minZ_267) ? b_266.Z : ((minZ_267 = b_266.Z, a_190.Z))), BBox_$ctor_76A78260(minX_267, minY_267, minZ_267, maxX_267, maxY_267, maxZ_267))))))))));
        const actual_147 = top_1.length | 0;
        if ((actual_147 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_147, 5, "Should have 5 points");
        }
        else {
            throw new Exception(contains((copyOfStruct_147 = actual_147, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_175) => (structuralHash(x_175) | 0),
            }) ? ((arg_152 = int32ToString(5), (arg_1_147 = int32ToString(actual_147), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_152)(arg_1_147)("Should have 5 points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_147)("Should have 5 points"));
        }
        Expect_isTrue(((a_192 = item(0, top_1), (b_268 = item(4, top_1), (x_176 = (a_192.X - b_268.X), (y_161 = (a_192.Y - b_268.Y), (z_13 = (a_192.Z - b_268.Z), Math.sqrt(((x_176 * x_176) + (y_161 * y_161)) + (z_13 * z_13)))))))) < 1E-09)("First and last should be same");
        Test_TestCaseBuilder__Zero(builder$0040_87);
    }));
})(), (() => {
    const builder$0040_88 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Edges array has 12 edges", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_88, Test_TestCaseBuilder__Delay_1505(builder$0040_88, () => {
        let a_193, b_269, minX_269, maxX_269, minY_269, maxY_269, minZ_269, maxZ_269, copyOfStruct_148, arg_153, arg_1_148;
        const edges = BBox__get_Edges((a_193 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_269 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_269 = a_193.X, (maxX_269 = ((b_269.X > minX_269) ? b_269.X : ((minX_269 = b_269.X, a_193.X))), (minY_269 = a_193.Y, (maxY_269 = ((b_269.Y > minY_269) ? b_269.Y : ((minY_269 = b_269.Y, a_193.Y))), (minZ_269 = a_193.Z, (maxZ_269 = ((b_269.Z > minZ_269) ? b_269.Z : ((minZ_269 = b_269.Z, a_193.Z))), BBox_$ctor_76A78260(minX_269, minY_269, minZ_269, maxX_269, maxY_269, maxZ_269))))))))));
        const actual_148 = edges.length | 0;
        if ((actual_148 === 12) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_148, 12, "Should have 12 edges");
        }
        else {
            throw new Exception(contains((copyOfStruct_148 = actual_148, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_177) => (structuralHash(x_177) | 0),
            }) ? ((arg_153 = int32ToString(12), (arg_1_148 = int32ToString(actual_148), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_153)(arg_1_148)("Should have 12 edges")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(12)(actual_148)("Should have 12 edges"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_88);
    }));
})(), (() => {
    const builder$0040_89 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Edge01 is X-aligned", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_89, Test_TestCaseBuilder__Delay_1505(builder$0040_89, () => {
        let b_272, b_273, a_196, ln, b_275, x_178, y_163, z_14, a_198, ln_1, b_277, x_179, y_164, z_15;
        let edge;
        let b_271;
        const a_194 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_270 = Pnt_$ctor_Z7AD9E565(10, 5, 3);
        let minX_271 = a_194.X;
        let maxX_271;
        if (b_270.X > minX_271) {
            maxX_271 = b_270.X;
        }
        else {
            minX_271 = b_270.X;
            maxX_271 = a_194.X;
        }
        let minY_271 = a_194.Y;
        let maxY_271;
        if (b_270.Y > minY_271) {
            maxY_271 = b_270.Y;
        }
        else {
            minY_271 = b_270.Y;
            maxY_271 = a_194.Y;
        }
        let minZ_271 = a_194.Z;
        let maxZ_271;
        if (b_270.Z > minZ_271) {
            maxZ_271 = b_270.Z;
        }
        else {
            minZ_271 = b_270.Z;
            maxZ_271 = a_194.Z;
        }
        b_271 = BBox_$ctor_76A78260(minX_271, minY_271, minZ_271, maxX_271, maxY_271, maxZ_271);
        edge = Line3D_$ctor_5A6659A0_1((b_272 = b_271, Pnt_$ctor_Z7AD9E565_1(b_272.MinX, b_272.MinY, b_272.MinZ)), (b_273 = b_271, Pnt_$ctor_Z7AD9E565_1(b_273.MaxX, b_273.MinY, b_273.MinZ)));
        Expect_isTrue(((a_196 = ((ln = edge, Pnt_$ctor_Z7AD9E565_1(ln.FromX, ln.FromY, ln.FromZ))), (b_275 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (x_178 = (a_196.X - b_275.X), (y_163 = (a_196.Y - b_275.Y), (z_14 = (a_196.Z - b_275.Z), Math.sqrt(((x_178 * x_178) + (y_163 * y_163)) + (z_14 * z_14)))))))) < 1E-09)("From should be Pt0");
        Expect_isTrue(((a_198 = ((ln_1 = edge, Pnt_$ctor_Z7AD9E565_1(ln_1.ToX, ln_1.ToY, ln_1.ToZ))), (b_277 = Pnt_$ctor_Z7AD9E565(10, 0, 0), (x_179 = (a_198.X - b_277.X), (y_164 = (a_198.Y - b_277.Y), (z_15 = (a_198.Z - b_277.Z), Math.sqrt(((x_179 * x_179) + (y_164 * y_164)) + (z_15 * z_15)))))))) < 1E-09)("To should be Pt1");
        Test_TestCaseBuilder__Zero(builder$0040_89);
    }));
})(), (() => {
    const builder$0040_90 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Edge37 is Z-aligned", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_90, Test_TestCaseBuilder__Delay_1505(builder$0040_90, () => {
        let b_280, b_281, a_201, ln_2, b_284, b_282, x_180, y_165, z_16, a_203, ln_3, b_287, b_285, x_181, y_166, z_17;
        let box_52;
        const a_199 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_278 = Pnt_$ctor_Z7AD9E565(10, 5, 3);
        let minX_273 = a_199.X;
        let maxX_273;
        if (b_278.X > minX_273) {
            maxX_273 = b_278.X;
        }
        else {
            minX_273 = b_278.X;
            maxX_273 = a_199.X;
        }
        let minY_273 = a_199.Y;
        let maxY_273;
        if (b_278.Y > minY_273) {
            maxY_273 = b_278.Y;
        }
        else {
            minY_273 = b_278.Y;
            maxY_273 = a_199.Y;
        }
        let minZ_273 = a_199.Z;
        let maxZ_273;
        if (b_278.Z > minZ_273) {
            maxZ_273 = b_278.Z;
        }
        else {
            minZ_273 = b_278.Z;
            maxZ_273 = a_199.Z;
        }
        box_52 = BBox_$ctor_76A78260(minX_273, minY_273, minZ_273, maxX_273, maxY_273, maxZ_273);
        let edge_1;
        const b_279 = box_52;
        edge_1 = Line3D_$ctor_5A6659A0_1((b_280 = b_279, Pnt_$ctor_Z7AD9E565_1(b_280.MinX, b_280.MaxY, b_280.MinZ)), (b_281 = b_279, Pnt_$ctor_Z7AD9E565_1(b_281.MinX, b_281.MaxY, b_281.MaxZ)));
        Expect_isTrue(((a_201 = ((ln_2 = edge_1, Pnt_$ctor_Z7AD9E565_1(ln_2.FromX, ln_2.FromY, ln_2.FromZ))), (b_284 = ((b_282 = box_52, Pnt_$ctor_Z7AD9E565_1(b_282.MinX, b_282.MaxY, b_282.MinZ))), (x_180 = (a_201.X - b_284.X), (y_165 = (a_201.Y - b_284.Y), (z_16 = (a_201.Z - b_284.Z), Math.sqrt(((x_180 * x_180) + (y_165 * y_165)) + (z_16 * z_16)))))))) < 1E-09)("From should be Pt3");
        Expect_isTrue(((a_203 = ((ln_3 = edge_1, Pnt_$ctor_Z7AD9E565_1(ln_3.ToX, ln_3.ToY, ln_3.ToZ))), (b_287 = ((b_285 = box_52, Pnt_$ctor_Z7AD9E565_1(b_285.MinX, b_285.MaxY, b_285.MaxZ))), (x_181 = (a_203.X - b_287.X), (y_166 = (a_203.Y - b_287.Y), (z_17 = (a_203.Z - b_287.Z), Math.sqrt(((x_181 * x_181) + (y_166 * y_166)) + (z_17 * z_17)))))))) < 1E-09)("To should be Pt7");
        Test_TestCaseBuilder__Zero(builder$0040_90);
    }));
})()])), Test_testList("Validation Methods", ofArray([(() => {
    const builder$0040_91 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsZero for tiny box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_91, Test_TestCaseBuilder__Delay_1505(builder$0040_91, () => {
        let b_289, a_204, b_288, minX_275, maxX_275, minY_275, maxY_275, minZ_275, maxZ_275;
        Expect_isTrue((b_289 = ((a_204 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (b_288 = Pnt_$ctor_Z7AD9E565(5 + 1E-13, 5 + 1E-13, 5 + 1E-13), (minX_275 = a_204.X, (maxX_275 = ((b_288.X > minX_275) ? b_288.X : ((minX_275 = b_288.X, a_204.X))), (minY_275 = a_204.Y, (maxY_275 = ((b_288.Y > minY_275) ? b_288.Y : ((minY_275 = b_288.Y, a_204.Y))), (minZ_275 = a_204.Z, (maxZ_275 = ((b_288.Z > minZ_275) ? b_288.Z : ((minZ_275 = b_288.Z, a_204.Z))), BBox_$ctor_76A78260(minX_275, minY_275, minZ_275, maxX_275, maxY_275, maxZ_275)))))))))), (!((b_289.MaxX - b_289.MinX) > 1E-12) && !((b_289.MaxY - b_289.MinY) > 1E-12)) && !((b_289.MaxZ - b_289.MinZ) > 1E-12)))("Tiny box should be zero");
        Test_TestCaseBuilder__Zero(builder$0040_91);
    }));
})(), (() => {
    const builder$0040_92 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsPoint is same as IsZero", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_92, Test_TestCaseBuilder__Delay_1505(builder$0040_92, () => {
        let copyOfStruct_149, arg_154, arg_1_149;
        let box_54;
        const a_205 = Pnt_$ctor_Z7AD9E565(5, 5, 5);
        const b_290 = Pnt_$ctor_Z7AD9E565(5 + 1E-13, 5 + 1E-13, 5 + 1E-13);
        let minX_277 = a_205.X;
        let maxX_277;
        if (b_290.X > minX_277) {
            maxX_277 = b_290.X;
        }
        else {
            minX_277 = b_290.X;
            maxX_277 = a_205.X;
        }
        let minY_277 = a_205.Y;
        let maxY_277;
        if (b_290.Y > minY_277) {
            maxY_277 = b_290.Y;
        }
        else {
            minY_277 = b_290.Y;
            maxY_277 = a_205.Y;
        }
        let minZ_277 = a_205.Z;
        let maxZ_277;
        if (b_290.Z > minZ_277) {
            maxZ_277 = b_290.Z;
        }
        else {
            minZ_277 = b_290.Z;
            maxZ_277 = a_205.Z;
        }
        box_54 = BBox_$ctor_76A78260(minX_277, minY_277, minZ_277, maxX_277, maxY_277, maxZ_277);
        let actual_149;
        const b_292 = box_54;
        actual_149 = ((!((b_292.MaxX - b_292.MinX) > 1E-12) && !((b_292.MaxY - b_292.MinY) > 1E-12)) && !((b_292.MaxZ - b_292.MinZ) > 1E-12));
        let expected_149;
        const b_293 = box_54;
        expected_149 = ((!((b_293.MaxX - b_293.MinX) > 1E-12) && !((b_293.MaxY - b_293.MinY) > 1E-12)) && !((b_293.MaxZ - b_293.MinZ) > 1E-12));
        if ((actual_149 === expected_149) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_149, expected_149, "IsPoint should equal IsZero");
        }
        else {
            throw new Exception(contains((copyOfStruct_149 = actual_149, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_191) => (structuralHash(x_191) | 0),
            }) ? ((arg_154 = toString(expected_149), (arg_1_149 = toString(actual_149), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_154)(arg_1_149)("IsPoint should equal IsZero")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_149)(actual_149)("IsPoint should equal IsZero"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_92);
    }));
})(), (() => {
    const builder$0040_93 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsLine for box with one dimension", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_93, Test_TestCaseBuilder__Delay_1505(builder$0040_93, () => {
        let b_296, a_206, b_294, minX_279, maxX_279, minY_279, maxY_279, minZ_279, maxZ_279;
        Expect_isTrue(((b_296 = ((a_206 = Pnt_$ctor_Z7AD9E565(0, 5, 5), (b_294 = Pnt_$ctor_Z7AD9E565(10, 5 + 1E-13, 5 + 1E-13), (minX_279 = a_206.X, (maxX_279 = ((b_294.X > minX_279) ? b_294.X : ((minX_279 = b_294.X, a_206.X))), (minY_279 = a_206.Y, (maxY_279 = ((b_294.Y > minY_279) ? b_294.Y : ((minY_279 = b_294.Y, a_206.Y))), (minZ_279 = a_206.Z, (maxZ_279 = ((b_294.Z > minZ_279) ? b_294.Z : ((minZ_279 = b_294.Z, a_206.Z))), BBox_$ctor_76A78260(minX_279, minY_279, minZ_279, maxX_279, maxY_279, maxZ_279)))))))))), ((((b_296.MaxX - b_296.MinX) > 1E-12) ? 0 : 1) + (((b_296.MaxY - b_296.MinY) > 1E-12) ? 0 : 1)) + (((b_296.MaxZ - b_296.MinZ) > 1E-12) ? 0 : 1))) === 2)("Box with one dimension should be a line");
        Test_TestCaseBuilder__Zero(builder$0040_93);
    }));
})(), (() => {
    const builder$0040_94 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsFlat for box with two dimensions", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_94, Test_TestCaseBuilder__Delay_1505(builder$0040_94, () => {
        let b_299, a_207, b_297, minX_281, maxX_281, minY_281, maxY_281, minZ_281, maxZ_281;
        Expect_isTrue(((b_299 = ((a_207 = Pnt_$ctor_Z7AD9E565(0, 0, 5), (b_297 = Pnt_$ctor_Z7AD9E565(10, 10, 5 + 1E-13), (minX_281 = a_207.X, (maxX_281 = ((b_297.X > minX_281) ? b_297.X : ((minX_281 = b_297.X, a_207.X))), (minY_281 = a_207.Y, (maxY_281 = ((b_297.Y > minY_281) ? b_297.Y : ((minY_281 = b_297.Y, a_207.Y))), (minZ_281 = a_207.Z, (maxZ_281 = ((b_297.Z > minZ_281) ? b_297.Z : ((minZ_281 = b_297.Z, a_207.Z))), BBox_$ctor_76A78260(minX_281, minY_281, minZ_281, maxX_281, maxY_281, maxZ_281)))))))))), ((((b_299.MaxX - b_299.MinX) > 1E-12) ? 0 : 1) + (((b_299.MaxY - b_299.MinY) > 1E-12) ? 0 : 1)) + (((b_299.MaxZ - b_299.MinZ) > 1E-12) ? 0 : 1))) === 1)("Box with two dimensions should be flat");
        Test_TestCaseBuilder__Zero(builder$0040_94);
    }));
})(), (() => {
    const builder$0040_95 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsValid for normal box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_95, Test_TestCaseBuilder__Delay_1505(builder$0040_95, () => {
        let b_302, a_208, b_300, minX_283, maxX_283, minY_283, maxY_283, minZ_283, maxZ_283;
        Expect_isTrue(((b_302 = ((a_208 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_300 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_283 = a_208.X, (maxX_283 = ((b_300.X > minX_283) ? b_300.X : ((minX_283 = b_300.X, a_208.X))), (minY_283 = a_208.Y, (maxY_283 = ((b_300.Y > minY_283) ? b_300.Y : ((minY_283 = b_300.Y, a_208.Y))), (minZ_283 = a_208.Z, (maxZ_283 = ((b_300.Z > minZ_283) ? b_300.Z : ((minZ_283 = b_300.Z, a_208.Z))), BBox_$ctor_76A78260(minX_283, minY_283, minZ_283, maxX_283, maxY_283, maxZ_283)))))))))), ((((b_302.MaxX - b_302.MinX) > 1E-12) ? 0 : 1) + (((b_302.MaxY - b_302.MinY) > 1E-12) ? 0 : 1)) + (((b_302.MaxZ - b_302.MinZ) > 1E-12) ? 0 : 1))) === 0)("Normal box should be valid");
        Test_TestCaseBuilder__Zero(builder$0040_95);
    }));
})(), (() => {
    const builder$0040_96 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("HasVolume for normal box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_96, Test_TestCaseBuilder__Delay_1505(builder$0040_96, () => {
        let b_305, a_209, b_303, minX_285, maxX_285, minY_285, maxY_285, minZ_285, maxZ_285;
        Expect_isTrue(((b_305 = ((a_209 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_303 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_285 = a_209.X, (maxX_285 = ((b_303.X > minX_285) ? b_303.X : ((minX_285 = b_303.X, a_209.X))), (minY_285 = a_209.Y, (maxY_285 = ((b_303.Y > minY_285) ? b_303.Y : ((minY_285 = b_303.Y, a_209.Y))), (minZ_285 = a_209.Z, (maxZ_285 = ((b_303.Z > minZ_285) ? b_303.Z : ((minZ_285 = b_303.Z, a_209.Z))), BBox_$ctor_76A78260(minX_285, minY_285, minZ_285, maxX_285, maxY_285, maxZ_285)))))))))), ((((b_305.MaxX - b_305.MinX) > 1E-12) ? 0 : 1) + (((b_305.MaxY - b_305.MinY) > 1E-12) ? 0 : 1)) + (((b_305.MaxZ - b_305.MinZ) > 1E-12) ? 0 : 1))) === 0)("Normal box should have volume");
        Test_TestCaseBuilder__Zero(builder$0040_96);
    }));
})(), (() => {
    const builder$0040_97 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("HasVolume is same as IsValid", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_97, Test_TestCaseBuilder__Delay_1505(builder$0040_97, () => {
        let b_308, b_310, copyOfStruct_150, arg_155, arg_1_150;
        let box_59;
        const a_210 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_306 = Pnt_$ctor_Z7AD9E565(10, 10, 10);
        let minX_287 = a_210.X;
        let maxX_287;
        if (b_306.X > minX_287) {
            maxX_287 = b_306.X;
        }
        else {
            minX_287 = b_306.X;
            maxX_287 = a_210.X;
        }
        let minY_287 = a_210.Y;
        let maxY_287;
        if (b_306.Y > minY_287) {
            maxY_287 = b_306.Y;
        }
        else {
            minY_287 = b_306.Y;
            maxY_287 = a_210.Y;
        }
        let minZ_287 = a_210.Z;
        let maxZ_287;
        if (b_306.Z > minZ_287) {
            maxZ_287 = b_306.Z;
        }
        else {
            minZ_287 = b_306.Z;
            maxZ_287 = a_210.Z;
        }
        box_59 = BBox_$ctor_76A78260(minX_287, minY_287, minZ_287, maxX_287, maxY_287, maxZ_287);
        const actual_150 = ((b_308 = box_59, ((((b_308.MaxX - b_308.MinX) > 1E-12) ? 0 : 1) + (((b_308.MaxY - b_308.MinY) > 1E-12) ? 0 : 1)) + (((b_308.MaxZ - b_308.MinZ) > 1E-12) ? 0 : 1))) === 0;
        const expected_150 = ((b_310 = box_59, ((((b_310.MaxX - b_310.MinX) > 1E-12) ? 0 : 1) + (((b_310.MaxY - b_310.MinY) > 1E-12) ? 0 : 1)) + (((b_310.MaxZ - b_310.MinZ) > 1E-12) ? 0 : 1))) === 0;
        if ((actual_150 === expected_150) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_150, expected_150, "HasVolume should equal IsValid");
        }
        else {
            throw new Exception(contains((copyOfStruct_150 = actual_150, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_210) => (structuralHash(x_210) | 0),
            }) ? ((arg_155 = toString(expected_150), (arg_1_150 = toString(actual_150), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_155)(arg_1_150)("HasVolume should equal IsValid")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_150)(actual_150)("HasVolume should equal IsValid"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_97);
    }));
})(), (() => {
    const builder$0040_98 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("CountZeroSides for normal box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_98, Test_TestCaseBuilder__Delay_1505(builder$0040_98, () => {
        let copyOfStruct_151, arg_156, arg_1_151;
        let actual_151;
        let b_312;
        const a_211 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_311 = Pnt_$ctor_Z7AD9E565(10, 10, 10);
        let minX_289 = a_211.X;
        let maxX_289;
        if (b_311.X > minX_289) {
            maxX_289 = b_311.X;
        }
        else {
            minX_289 = b_311.X;
            maxX_289 = a_211.X;
        }
        let minY_289 = a_211.Y;
        let maxY_289;
        if (b_311.Y > minY_289) {
            maxY_289 = b_311.Y;
        }
        else {
            minY_289 = b_311.Y;
            maxY_289 = a_211.Y;
        }
        let minZ_289 = a_211.Z;
        let maxZ_289;
        if (b_311.Z > minZ_289) {
            maxZ_289 = b_311.Z;
        }
        else {
            minZ_289 = b_311.Z;
            maxZ_289 = a_211.Z;
        }
        b_312 = BBox_$ctor_76A78260(minX_289, minY_289, minZ_289, maxX_289, maxY_289, maxZ_289);
        actual_151 = (((((b_312.MaxX - b_312.MinX) > 1E-12) ? 0 : 1) + (((b_312.MaxY - b_312.MinY) > 1E-12) ? 0 : 1)) + (((b_312.MaxZ - b_312.MinZ) > 1E-12) ? 0 : 1));
        if ((actual_151 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_151, 0, "Normal box should have 0 zero sides");
        }
        else {
            throw new Exception(contains((copyOfStruct_151 = actual_151, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_214) => (structuralHash(x_214) | 0),
            }) ? ((arg_156 = int32ToString(0), (arg_1_151 = int32ToString(actual_151), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_156)(arg_1_151)("Normal box should have 0 zero sides")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_151)("Normal box should have 0 zero sides"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_98);
    }));
})(), (() => {
    const builder$0040_99 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("CountZeroSides for flat box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_99, Test_TestCaseBuilder__Delay_1505(builder$0040_99, () => {
        let copyOfStruct_152, arg_157, arg_1_152;
        let actual_152;
        let b_314;
        const a_212 = Pnt_$ctor_Z7AD9E565(0, 0, 5);
        const b_313 = Pnt_$ctor_Z7AD9E565(10, 10, 5);
        let minX_291 = a_212.X;
        let maxX_291;
        if (b_313.X > minX_291) {
            maxX_291 = b_313.X;
        }
        else {
            minX_291 = b_313.X;
            maxX_291 = a_212.X;
        }
        let minY_291 = a_212.Y;
        let maxY_291;
        if (b_313.Y > minY_291) {
            maxY_291 = b_313.Y;
        }
        else {
            minY_291 = b_313.Y;
            maxY_291 = a_212.Y;
        }
        let minZ_291 = a_212.Z;
        let maxZ_291;
        if (b_313.Z > minZ_291) {
            maxZ_291 = b_313.Z;
        }
        else {
            minZ_291 = b_313.Z;
            maxZ_291 = a_212.Z;
        }
        b_314 = BBox_$ctor_76A78260(minX_291, minY_291, minZ_291, maxX_291, maxY_291, maxZ_291);
        actual_152 = (((((b_314.MaxX - b_314.MinX) > 1E-12) ? 0 : 1) + (((b_314.MaxY - b_314.MinY) > 1E-12) ? 0 : 1)) + (((b_314.MaxZ - b_314.MinZ) > 1E-12) ? 0 : 1));
        if ((actual_152 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_152, 1, "Flat box should have 1 zero side");
        }
        else {
            throw new Exception(contains((copyOfStruct_152 = actual_152, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_218) => (structuralHash(x_218) | 0),
            }) ? ((arg_157 = int32ToString(1), (arg_1_152 = int32ToString(actual_152), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_157)(arg_1_152)("Flat box should have 1 zero side")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_152)("Flat box should have 1 zero side"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_99);
    }));
})(), (() => {
    const builder$0040_100 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("CountZeroSides for line box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_100, Test_TestCaseBuilder__Delay_1505(builder$0040_100, () => {
        let copyOfStruct_153, arg_158, arg_1_153;
        let actual_153;
        let b_316;
        const a_213 = Pnt_$ctor_Z7AD9E565(0, 5, 5);
        const b_315 = Pnt_$ctor_Z7AD9E565(10, 5, 5);
        let minX_293 = a_213.X;
        let maxX_293;
        if (b_315.X > minX_293) {
            maxX_293 = b_315.X;
        }
        else {
            minX_293 = b_315.X;
            maxX_293 = a_213.X;
        }
        let minY_293 = a_213.Y;
        let maxY_293;
        if (b_315.Y > minY_293) {
            maxY_293 = b_315.Y;
        }
        else {
            minY_293 = b_315.Y;
            maxY_293 = a_213.Y;
        }
        let minZ_293 = a_213.Z;
        let maxZ_293;
        if (b_315.Z > minZ_293) {
            maxZ_293 = b_315.Z;
        }
        else {
            minZ_293 = b_315.Z;
            maxZ_293 = a_213.Z;
        }
        b_316 = BBox_$ctor_76A78260(minX_293, minY_293, minZ_293, maxX_293, maxY_293, maxZ_293);
        actual_153 = (((((b_316.MaxX - b_316.MinX) > 1E-12) ? 0 : 1) + (((b_316.MaxY - b_316.MinY) > 1E-12) ? 0 : 1)) + (((b_316.MaxZ - b_316.MinZ) > 1E-12) ? 0 : 1));
        if ((actual_153 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_153, 2, "Line box should have 2 zero sides");
        }
        else {
            throw new Exception(contains((copyOfStruct_153 = actual_153, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_222) => (structuralHash(x_222) | 0),
            }) ? ((arg_158 = int32ToString(2), (arg_1_153 = int32ToString(actual_153), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_158)(arg_1_153)("Line box should have 2 zero sides")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_153)("Line box should have 2 zero sides"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_100);
    }));
})(), (() => {
    const builder$0040_101 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("CountZeroSides for point box", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_101, Test_TestCaseBuilder__Delay_1505(builder$0040_101, () => {
        let copyOfStruct_154, arg_159, arg_1_154;
        let actual_154;
        let b_318;
        const a_214 = Pnt_$ctor_Z7AD9E565(5, 5, 5);
        const b_317 = Pnt_$ctor_Z7AD9E565(5, 5, 5);
        let minX_295 = a_214.X;
        let maxX_295;
        if (b_317.X > minX_295) {
            maxX_295 = b_317.X;
        }
        else {
            minX_295 = b_317.X;
            maxX_295 = a_214.X;
        }
        let minY_295 = a_214.Y;
        let maxY_295;
        if (b_317.Y > minY_295) {
            maxY_295 = b_317.Y;
        }
        else {
            minY_295 = b_317.Y;
            maxY_295 = a_214.Y;
        }
        let minZ_295 = a_214.Z;
        let maxZ_295;
        if (b_317.Z > minZ_295) {
            maxZ_295 = b_317.Z;
        }
        else {
            minZ_295 = b_317.Z;
            maxZ_295 = a_214.Z;
        }
        b_318 = BBox_$ctor_76A78260(minX_295, minY_295, minZ_295, maxX_295, maxY_295, maxZ_295);
        actual_154 = (((((b_318.MaxX - b_318.MinX) > 1E-12) ? 0 : 1) + (((b_318.MaxY - b_318.MinY) > 1E-12) ? 0 : 1)) + (((b_318.MaxZ - b_318.MinZ) > 1E-12) ? 0 : 1));
        if ((actual_154 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_154, 3, "Point box should have 3 zero sides");
        }
        else {
            throw new Exception(contains((copyOfStruct_154 = actual_154, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_226) => (structuralHash(x_226) | 0),
            }) ? ((arg_159 = int32ToString(3), (arg_1_154 = int32ToString(actual_154), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_159)(arg_1_154)("Point box should have 3 zero sides")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_154)("Point box should have 3 zero sides"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_101);
    }));
})()])), Test_testList("Size Methods", ofArray([(() => {
    const builder$0040_102 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("LongestEdge", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_102, Test_TestCaseBuilder__Delay_1505(builder$0040_102, () => {
        let copyOfStruct_155, arg_160, arg_1_155;
        let actual_155;
        let b_320;
        const a_215 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_319 = Pnt_$ctor_Z7AD9E565(10, 5, 3);
        let minX_297 = a_215.X;
        let maxX_297;
        if (b_319.X > minX_297) {
            maxX_297 = b_319.X;
        }
        else {
            minX_297 = b_319.X;
            maxX_297 = a_215.X;
        }
        let minY_297 = a_215.Y;
        let maxY_297;
        if (b_319.Y > minY_297) {
            maxY_297 = b_319.Y;
        }
        else {
            minY_297 = b_319.Y;
            maxY_297 = a_215.Y;
        }
        let minZ_297 = a_215.Z;
        let maxZ_297;
        if (b_319.Z > minZ_297) {
            maxZ_297 = b_319.Z;
        }
        else {
            minZ_297 = b_319.Z;
            maxZ_297 = a_215.Z;
        }
        b_320 = BBox_$ctor_76A78260(minX_297, minY_297, minZ_297, maxX_297, maxY_297, maxZ_297);
        const z_18 = b_320.MaxZ - b_320.MinZ;
        actual_155 = max(max(b_320.MaxX - b_320.MinX, b_320.MaxY - b_320.MinY), z_18);
        if ((actual_155 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_155, 10, "Longest edge should be 10");
        }
        else {
            throw new Exception(contains((copyOfStruct_155 = actual_155, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_228) => (structuralHash(x_228) | 0),
            }) ? ((arg_160 = (10).toString(), (arg_1_155 = actual_155.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_160)(arg_1_155)("Longest edge should be 10")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_155)("Longest edge should be 10"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_102);
    }));
})(), (() => {
    const builder$0040_103 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ShortestEdge", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_103, Test_TestCaseBuilder__Delay_1505(builder$0040_103, () => {
        let copyOfStruct_156, arg_161, arg_1_156;
        let actual_156;
        let b_322;
        const a_216 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_321 = Pnt_$ctor_Z7AD9E565(10, 5, 3);
        let minX_299 = a_216.X;
        let maxX_299;
        if (b_321.X > minX_299) {
            maxX_299 = b_321.X;
        }
        else {
            minX_299 = b_321.X;
            maxX_299 = a_216.X;
        }
        let minY_299 = a_216.Y;
        let maxY_299;
        if (b_321.Y > minY_299) {
            maxY_299 = b_321.Y;
        }
        else {
            minY_299 = b_321.Y;
            maxY_299 = a_216.Y;
        }
        let minZ_299 = a_216.Z;
        let maxZ_299;
        if (b_321.Z > minZ_299) {
            maxZ_299 = b_321.Z;
        }
        else {
            minZ_299 = b_321.Z;
            maxZ_299 = a_216.Z;
        }
        b_322 = BBox_$ctor_76A78260(minX_299, minY_299, minZ_299, maxX_299, maxY_299, maxZ_299);
        const z_19 = b_322.MaxZ - b_322.MinZ;
        actual_156 = min(min(b_322.MaxX - b_322.MinX, b_322.MaxY - b_322.MinY), z_19);
        if ((actual_156 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_156, 3, "Shortest edge should be 3");
        }
        else {
            throw new Exception(contains((copyOfStruct_156 = actual_156, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_230) => (structuralHash(x_230) | 0),
            }) ? ((arg_161 = (3).toString(), (arg_1_156 = actual_156.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_161)(arg_1_156)("Shortest edge should be 3")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_156)("Shortest edge should be 3"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_103);
    }));
})()])), Test_testList("Evaluation", ofArray([(() => {
    const builder$0040_104 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("EvaluateAt origin (0,0,0)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_104, Test_TestCaseBuilder__Delay_1505(builder$0040_104, () => {
        let a_219, b_324, b_327, b_325, x_231, y_177, z_20;
        let box_66;
        const a_217 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        const b_323 = Pnt_$ctor_Z7AD9E565(11, 12, 13);
        let minX_301 = a_217.X;
        let maxX_301;
        if (b_323.X > minX_301) {
            maxX_301 = b_323.X;
        }
        else {
            minX_301 = b_323.X;
            maxX_301 = a_217.X;
        }
        let minY_301 = a_217.Y;
        let maxY_301;
        if (b_323.Y > minY_301) {
            maxY_301 = b_323.Y;
        }
        else {
            minY_301 = b_323.Y;
            maxY_301 = a_217.Y;
        }
        let minZ_301 = a_217.Z;
        let maxZ_301;
        if (b_323.Z > minZ_301) {
            maxZ_301 = b_323.Z;
        }
        else {
            minZ_301 = b_323.Z;
            maxZ_301 = a_217.Z;
        }
        box_66 = BBox_$ctor_76A78260(minX_301, minY_301, minZ_301, maxX_301, maxY_301, maxZ_301);
        Expect_isTrue(((a_219 = ((b_324 = box_66, Pnt_$ctor_Z7AD9E565_1(b_324.MinX + ((b_324.MaxX - b_324.MinX) * 0), b_324.MinY + ((b_324.MaxY - b_324.MinY) * 0), b_324.MinZ + ((b_324.MaxZ - b_324.MinZ) * 0)))), (b_327 = ((b_325 = box_66, Pnt_$ctor_Z7AD9E565_1(b_325.MinX, b_325.MinY, b_325.MinZ))), (x_231 = (a_219.X - b_327.X), (y_177 = (a_219.Y - b_327.Y), (z_20 = (a_219.Z - b_327.Z), Math.sqrt(((x_231 * x_231) + (y_177 * y_177)) + (z_20 * z_20)))))))) < 1E-09)("Should return MinPnt at (0,0,0)");
        Test_TestCaseBuilder__Zero(builder$0040_104);
    }));
})(), (() => {
    const builder$0040_105 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("EvaluateAt far corner (1,1,1)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_105, Test_TestCaseBuilder__Delay_1505(builder$0040_105, () => {
        let a_222, b_329, b_332, b_330, x_232, y_178, z_21;
        let box_67;
        const a_220 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        const b_328 = Pnt_$ctor_Z7AD9E565(11, 12, 13);
        let minX_303 = a_220.X;
        let maxX_303;
        if (b_328.X > minX_303) {
            maxX_303 = b_328.X;
        }
        else {
            minX_303 = b_328.X;
            maxX_303 = a_220.X;
        }
        let minY_303 = a_220.Y;
        let maxY_303;
        if (b_328.Y > minY_303) {
            maxY_303 = b_328.Y;
        }
        else {
            minY_303 = b_328.Y;
            maxY_303 = a_220.Y;
        }
        let minZ_303 = a_220.Z;
        let maxZ_303;
        if (b_328.Z > minZ_303) {
            maxZ_303 = b_328.Z;
        }
        else {
            minZ_303 = b_328.Z;
            maxZ_303 = a_220.Z;
        }
        box_67 = BBox_$ctor_76A78260(minX_303, minY_303, minZ_303, maxX_303, maxY_303, maxZ_303);
        Expect_isTrue(((a_222 = ((b_329 = box_67, Pnt_$ctor_Z7AD9E565_1(b_329.MinX + ((b_329.MaxX - b_329.MinX) * 1), b_329.MinY + ((b_329.MaxY - b_329.MinY) * 1), b_329.MinZ + ((b_329.MaxZ - b_329.MinZ) * 1)))), (b_332 = ((b_330 = box_67, Pnt_$ctor_Z7AD9E565_1(b_330.MaxX, b_330.MaxY, b_330.MaxZ))), (x_232 = (a_222.X - b_332.X), (y_178 = (a_222.Y - b_332.Y), (z_21 = (a_222.Z - b_332.Z), Math.sqrt(((x_232 * x_232) + (y_178 * y_178)) + (z_21 * z_21)))))))) < 1E-09)("Should return MaxPnt at (1,1,1)");
        Test_TestCaseBuilder__Zero(builder$0040_105);
    }));
})(), (() => {
    const builder$0040_106 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("EvaluateAt center (0.5,0.5,0.5)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_106, Test_TestCaseBuilder__Delay_1505(builder$0040_106, () => {
        let a_225, b_334, a_223, b_333, minX_305, maxX_305, minY_305, maxY_305, minZ_305, maxZ_305, b_336, x_233, y_179, z_22;
        Expect_isTrue(((a_225 = ((b_334 = ((a_223 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_333 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_305 = a_223.X, (maxX_305 = ((b_333.X > minX_305) ? b_333.X : ((minX_305 = b_333.X, a_223.X))), (minY_305 = a_223.Y, (maxY_305 = ((b_333.Y > minY_305) ? b_333.Y : ((minY_305 = b_333.Y, a_223.Y))), (minZ_305 = a_223.Z, (maxZ_305 = ((b_333.Z > minZ_305) ? b_333.Z : ((minZ_305 = b_333.Z, a_223.Z))), BBox_$ctor_76A78260(minX_305, minY_305, minZ_305, maxX_305, maxY_305, maxZ_305)))))))))), Pnt_$ctor_Z7AD9E565_1(b_334.MinX + ((b_334.MaxX - b_334.MinX) * 0.5), b_334.MinY + ((b_334.MaxY - b_334.MinY) * 0.5), b_334.MinZ + ((b_334.MaxZ - b_334.MinZ) * 0.5)))), (b_336 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (x_233 = (a_225.X - b_336.X), (y_179 = (a_225.Y - b_336.Y), (z_22 = (a_225.Z - b_336.Z), Math.sqrt(((x_233 * x_233) + (y_179 * y_179)) + (z_22 * z_22)))))))) < 1E-09)("Should return center at (0.5,0.5,0.5)");
        Test_TestCaseBuilder__Zero(builder$0040_106);
    }));
})()])), Test_testList("Conversion Methods", ofArray([(() => {
    const builder$0040_107 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("asBRect", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_107, Test_TestCaseBuilder__Delay_1505(builder$0040_107, () => {
        let copyOfStruct_157, arg_162, arg_1_157, copyOfStruct_158, arg_163, arg_1_158, copyOfStruct_159, arg_164, arg_1_159, copyOfStruct_160, arg_165, arg_1_160;
        let rect_2;
        let b_338;
        const a_226 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        const b_337 = Pnt_$ctor_Z7AD9E565(5, 7, 9);
        let minX_307 = a_226.X;
        let maxX_307;
        if (b_337.X > minX_307) {
            maxX_307 = b_337.X;
        }
        else {
            minX_307 = b_337.X;
            maxX_307 = a_226.X;
        }
        let minY_307 = a_226.Y;
        let maxY_307;
        if (b_337.Y > minY_307) {
            maxY_307 = b_337.Y;
        }
        else {
            minY_307 = b_337.Y;
            maxY_307 = a_226.Y;
        }
        let minZ_307 = a_226.Z;
        let maxZ_307;
        if (b_337.Z > minZ_307) {
            maxZ_307 = b_337.Z;
        }
        else {
            minZ_307 = b_337.Z;
            maxZ_307 = a_226.Z;
        }
        b_338 = BBox_$ctor_76A78260(minX_307, minY_307, minZ_307, maxX_307, maxY_307, maxZ_307);
        rect_2 = BRect_$ctor_77D16AC0(b_338.MinX, b_338.MinY, b_338.MaxX, b_338.MaxY);
        const actual_157 = rect_2.MinX;
        if ((actual_157 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_157, 1, "BRect MinX should be 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_157 = actual_157, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_234) => (structuralHash(x_234) | 0),
            }) ? ((arg_162 = (1).toString(), (arg_1_157 = actual_157.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_162)(arg_1_157)("BRect MinX should be 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_157)("BRect MinX should be 1"));
        }
        const actual_158 = rect_2.MinY;
        if ((actual_158 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_158, 2, "BRect MinY should be 2");
        }
        else {
            throw new Exception(contains((copyOfStruct_158 = actual_158, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_235) => (structuralHash(x_235) | 0),
            }) ? ((arg_163 = (2).toString(), (arg_1_158 = actual_158.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_163)(arg_1_158)("BRect MinY should be 2")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_158)("BRect MinY should be 2"));
        }
        const actual_159 = rect_2.MaxX;
        if ((actual_159 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_159, 5, "BRect MaxX should be 5");
        }
        else {
            throw new Exception(contains((copyOfStruct_159 = actual_159, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_236) => (structuralHash(x_236) | 0),
            }) ? ((arg_164 = (5).toString(), (arg_1_159 = actual_159.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_164)(arg_1_159)("BRect MaxX should be 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_159)("BRect MaxX should be 5"));
        }
        const actual_160 = rect_2.MaxY;
        if ((actual_160 === 7) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_160, 7, "BRect MaxY should be 7");
        }
        else {
            throw new Exception(contains((copyOfStruct_160 = actual_160, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_237) => (structuralHash(x_237) | 0),
            }) ? ((arg_165 = (7).toString(), (arg_1_160 = actual_160.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_165)(arg_1_160)("BRect MaxY should be 7")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(7)(actual_160)("BRect MaxY should be 7"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_107);
    }));
})(), (() => {
    const builder$0040_108 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("toBRect static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_108, Test_TestCaseBuilder__Delay_1505(builder$0040_108, () => {
        let b_340, a_227, b_339, minX_310, maxX_310, minY_310, maxY_310, minZ_309, maxZ_309, copyOfStruct_161, arg_166, arg_1_161;
        const actual_161 = ((b_340 = ((a_227 = Pnt_$ctor_Z7AD9E565(1, 2, 3), (b_339 = Pnt_$ctor_Z7AD9E565(5, 7, 9), (minX_310 = a_227.X, (maxX_310 = ((b_339.X > minX_310) ? b_339.X : ((minX_310 = b_339.X, a_227.X))), (minY_310 = a_227.Y, (maxY_310 = ((b_339.Y > minY_310) ? b_339.Y : ((minY_310 = b_339.Y, a_227.Y))), (minZ_309 = a_227.Z, (maxZ_309 = ((b_339.Z > minZ_309) ? b_339.Z : ((minZ_309 = b_339.Z, a_227.Z))), BBox_$ctor_76A78260(minX_310, minY_310, minZ_309, maxX_310, maxY_310, maxZ_309)))))))))), BRect_$ctor_77D16AC0(b_340.MinX, b_340.MinY, b_340.MaxX, b_340.MaxY))).MinX;
        if ((actual_161 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_161, 1, "BRect MinX should be 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_161 = actual_161, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_238) => (structuralHash(x_238) | 0),
            }) ? ((arg_166 = (1).toString(), (arg_1_161 = actual_161.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_166)(arg_1_161)("BRect MinX should be 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_161)("BRect MinX should be 1"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_108);
    }));
})()])), Test_testList("Equality Methods", ofArray([(() => {
    const builder$0040_109 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("equals with exact match", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_109, Test_TestCaseBuilder__Delay_1505(builder$0040_109, () => {
        let a_228, b_341, minX_313, maxX_313, minY_313, maxY_313, minZ_311, maxZ_311, a_230, b_342, minX_315, maxX_315, minY_315, maxY_315, minZ_313, maxZ_313;
        Expect_isTrue(BBox_equals(0, (a_228 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_341 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_313 = a_228.X, (maxX_313 = ((b_341.X > minX_313) ? b_341.X : ((minX_313 = b_341.X, a_228.X))), (minY_313 = a_228.Y, (maxY_313 = ((b_341.Y > minY_313) ? b_341.Y : ((minY_313 = b_341.Y, a_228.Y))), (minZ_311 = a_228.Z, (maxZ_311 = ((b_341.Z > minZ_311) ? b_341.Z : ((minZ_311 = b_341.Z, a_228.Z))), BBox_$ctor_76A78260(minX_313, minY_313, minZ_311, maxX_313, maxY_313, maxZ_311))))))))), (a_230 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_342 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_315 = a_230.X, (maxX_315 = ((b_342.X > minX_315) ? b_342.X : ((minX_315 = b_342.X, a_230.X))), (minY_315 = a_230.Y, (maxY_315 = ((b_342.Y > minY_315) ? b_342.Y : ((minY_315 = b_342.Y, a_230.Y))), (minZ_313 = a_230.Z, (maxZ_313 = ((b_342.Z > minZ_313) ? b_342.Z : ((minZ_313 = b_342.Z, a_230.Z))), BBox_$ctor_76A78260(minX_315, minY_315, minZ_313, maxX_315, maxY_315, maxZ_313)))))))))))("Exact boxes should be equal");
        Test_TestCaseBuilder__Zero(builder$0040_109);
    }));
})(), (() => {
    const builder$0040_110 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("equals with tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_110, Test_TestCaseBuilder__Delay_1505(builder$0040_110, () => {
        let a_233;
        const a_232 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_345 = Pnt_$ctor_Z7AD9E565(10, 10, 10);
        let minX_317 = a_232.X;
        let maxX_317;
        if (b_345.X > minX_317) {
            maxX_317 = b_345.X;
        }
        else {
            minX_317 = b_345.X;
            maxX_317 = a_232.X;
        }
        let minY_317 = a_232.Y;
        let maxY_317;
        if (b_345.Y > minY_317) {
            maxY_317 = b_345.Y;
        }
        else {
            minY_317 = b_345.Y;
            maxY_317 = a_232.Y;
        }
        let minZ_315 = a_232.Z;
        let maxZ_315;
        if (b_345.Z > minZ_315) {
            maxZ_315 = b_345.Z;
        }
        else {
            minZ_315 = b_345.Z;
            maxZ_315 = a_232.Z;
        }
        a_233 = BBox_$ctor_76A78260(minX_317, minY_317, minZ_315, maxX_317, maxY_317, maxZ_315);
        let b_347;
        const a_234 = Pnt_$ctor_Z7AD9E565(0.001, 0.001, 0.001);
        const b_346 = Pnt_$ctor_Z7AD9E565(10.001, 10.001, 10.001);
        let minX_319 = a_234.X;
        let maxX_319;
        if (b_346.X > minX_319) {
            maxX_319 = b_346.X;
        }
        else {
            minX_319 = b_346.X;
            maxX_319 = a_234.X;
        }
        let minY_319 = a_234.Y;
        let maxY_319;
        if (b_346.Y > minY_319) {
            maxY_319 = b_346.Y;
        }
        else {
            minY_319 = b_346.Y;
            maxY_319 = a_234.Y;
        }
        let minZ_317 = a_234.Z;
        let maxZ_317;
        if (b_346.Z > minZ_317) {
            maxZ_317 = b_346.Z;
        }
        else {
            minZ_317 = b_346.Z;
            maxZ_317 = a_234.Z;
        }
        b_347 = BBox_$ctor_76A78260(minX_319, minY_319, minZ_317, maxX_319, maxY_319, maxZ_317);
        Expect_isTrue(BBox_equals(0.01, a_233, b_347))("Boxes should be equal within tolerance");
        Expect_isFalse(BBox_equals(0.0001, a_233, b_347))("Boxes should not be equal with small tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_110);
    }));
})(), (() => {
    const builder$0040_111 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("notEquals", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_111, Test_TestCaseBuilder__Delay_1505(builder$0040_111, () => {
        let a_237, b_350, minX_321, maxX_321, minY_321, maxY_321, minZ_319, maxZ_319, a_239, b_351, minX_323, maxX_323, minY_323, maxY_323, minZ_321, maxZ_321;
        Expect_isTrue(BBox_notEquals(0.5, (a_237 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (b_350 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_321 = a_237.X, (maxX_321 = ((b_350.X > minX_321) ? b_350.X : ((minX_321 = b_350.X, a_237.X))), (minY_321 = a_237.Y, (maxY_321 = ((b_350.Y > minY_321) ? b_350.Y : ((minY_321 = b_350.Y, a_237.Y))), (minZ_319 = a_237.Z, (maxZ_319 = ((b_350.Z > minZ_319) ? b_350.Z : ((minZ_319 = b_350.Z, a_237.Z))), BBox_$ctor_76A78260(minX_321, minY_321, minZ_319, maxX_321, maxY_321, maxZ_319))))))))), (a_239 = Pnt_$ctor_Z7AD9E565(1, 1, 1), (b_351 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (minX_323 = a_239.X, (maxX_323 = ((b_351.X > minX_323) ? b_351.X : ((minX_323 = b_351.X, a_239.X))), (minY_323 = a_239.Y, (maxY_323 = ((b_351.Y > minY_323) ? b_351.Y : ((minY_323 = b_351.Y, a_239.Y))), (minZ_321 = a_239.Z, (maxZ_321 = ((b_351.Z > minZ_321) ? b_351.Z : ((minZ_321 = b_351.Z, a_239.Z))), BBox_$ctor_76A78260(minX_323, minY_323, minZ_321, maxX_323, maxY_323, maxZ_321)))))))))))("Different boxes should not be equal");
        Test_TestCaseBuilder__Zero(builder$0040_111);
    }));
})()]))]));

