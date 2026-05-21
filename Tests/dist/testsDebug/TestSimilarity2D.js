
import { Expect_floatClose, Expect_throws, Expect_isFalse, Test_TestCaseBuilder__Zero, Expect_isTrue, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList, AccuracyModule_veryHigh } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Pt_$ctor_7B00E9A0 } from "./Src/Pt.js";
import { getGrouped, areSimilar, getSimilarityData } from "./Src/Similarity2D.js";
import { item } from "./fable_modules/fable-library-js.5.0.0/Array.js";
import { Exception, int32ToString, structuralHash, assertEqual } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { equals, class_type, decimal_type, string_type, float64_type, bool_type, int32_type } from "./fable_modules/fable-library-js.5.0.0/Reflection.js";
import { contains, ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { printf, toText } from "./fable_modules/fable-library-js.5.0.0/String.js";

export const tol = AccuracyModule_veryHigh;

export const tests = Test_testList("Similarity2D", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areSimilar returns true for identical objects", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const pts1 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(1, 1)]]];
        const obj1 = getSimilarityData(pts1);
        const obj2 = getSimilarityData(pts1);
        Expect_isTrue(areSimilar(0.01, obj1, obj2))("identical objects are similar");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areSimilar returns false for different sized objects", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const pts1_1 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]];
        const pts2 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(2, 0)]]];
        const obj1_1 = getSimilarityData(pts1_1);
        const obj2_1 = getSimilarityData(pts2);
        Expect_isFalse(areSimilar(0.01, obj1_1, obj2_1))("different sized objects are not similar");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areSimilar returns true for shifted objects within tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const pts1_2 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(1, 1)]]];
        const pts2_1 = [["cat1", [Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(11, 10), Pt_$ctor_7B00E9A0(11, 11)]]];
        const obj1_2 = getSimilarityData(pts1_2);
        const obj2_2 = getSimilarityData(pts2_1);
        Expect_isTrue(areSimilar(0.01, obj1_2, obj2_2))("shifted objects are similar (normalized to origin)");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areSimilar returns false for different shapes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        const pts1_3 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(1, 1)]]];
        const pts2_2 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(0, 1)]]];
        const obj1_3 = getSimilarityData(pts1_3);
        const obj2_3 = getSimilarityData(pts2_2);
        Expect_isFalse(areSimilar(0.01, obj1_3, obj2_3))("different shapes are not similar");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areSimilar handles multiple categories", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const pts1_4 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]], ["cat2", [Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(3, 0)]]];
        const pts2_3 = [["cat1", [Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(11, 10)]], ["cat2", [Pt_$ctor_7B00E9A0(12, 10), Pt_$ctor_7B00E9A0(13, 10)]]];
        const obj1_4 = getSimilarityData(pts1_4);
        const obj2_4 = getSimilarityData(pts2_3);
        Expect_isTrue(areSimilar(0.01, obj1_4, obj2_4))("objects with multiple categories are similar");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areSimilar returns false if category count differs", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const pts1_5 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]];
        const pts2_4 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]], ["cat2", [Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(3, 0)]]];
        const obj1_5 = getSimilarityData(pts1_5);
        const obj2_5 = getSimilarityData(pts2_4);
        Expect_isFalse(areSimilar(0.01, obj1_5, obj2_5))("different category counts are not similar");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areSimilar returns false if category names differ", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const pts1_6 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]];
        const pts2_5 = [["cat2", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]];
        const obj1_6 = getSimilarityData(pts1_6);
        const obj2_6 = getSimilarityData(pts2_5);
        Expect_isFalse(areSimilar(0.01, obj1_6, obj2_6))("different category names are not similar");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSimilarityData normalizes to origin", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        const pts = [["cat1", [Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(11, 10), Pt_$ctor_7B00E9A0(11, 11)]]];
        const obj = getSimilarityData(pts);
        const grp = item(0, obj.groups);
        Expect_isTrue(item(0, grp.points).X >= 0)("first point X >= 0");
        Expect_isTrue(item(0, grp.points).Y >= 0)("first point Y >= 0");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSimilarityData sorts points by X", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        const pts_1 = [["cat1", [Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]];
        const obj_1 = getSimilarityData(pts_1);
        const grp_1 = item(0, obj_1.groups);
        Expect_isTrue(item(0, grp_1.points).X <= item(1, grp_1.points).X)("first point has smallest X");
        Expect_isTrue(item(1, grp_1.points).X <= item(2, grp_1.points).X)("second point X <= third point X");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getGrouped groups similar items", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const items = [1, 2, 3, 4];
        const sims = [getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]]), getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]]), getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(2, 0)]]]), getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]])];
        const groups = getGrouped(0.01, items, sims);
        const actual_1 = groups.length | 0;
        if ((actual_1 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, 2, "two groups created");
        }
        else {
            let valueType;
            let copyOfStruct = actual_1;
            valueType = int32_type;
            const primitiveTypes = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg;
            if (contains(valueType, primitiveTypes, {
                Equals: equals,
                GetHashCode: (x) => (structuralHash(x) | 0),
            })) {
                const arg = int32ToString(2);
                const arg_1 = int32ToString(actual_1);
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("two groups created");
            }
            else {
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_1)("two groups created");
            }
            throw new Exception(errorMsg);
        }
        const group1Size = item(0, groups).length | 0;
        const group2Size = item(1, groups).length | 0;
        Expect_isTrue(((group1Size === 3) && (group2Size === 1)) ? true : ((group1Size === 1) && (group2Size === 3)))("groups have correct sizes");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getGrouped with all unique items", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        const items_1 = [1, 2, 3];
        const sims_1 = [getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]]), getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(2, 0)]]]), getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(3, 0)]]])];
        const groups_1 = getGrouped(0.01, items_1, sims_1);
        const actual_3 = groups_1.length | 0;
        if ((actual_3 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, 3, "three unique groups");
        }
        else {
            let valueType_1;
            let copyOfStruct_1 = actual_3;
            valueType_1 = int32_type;
            const primitiveTypes_1 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_1;
            if (contains(valueType_1, primitiveTypes_1, {
                Equals: equals,
                GetHashCode: (x_1) => (structuralHash(x_1) | 0),
            })) {
                const arg_6 = int32ToString(3);
                const arg_1_1 = int32ToString(actual_3);
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("three unique groups");
            }
            else {
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_3)("three unique groups");
            }
            throw new Exception(errorMsg_1);
        }
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getGrouped with all identical items", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        const items_2 = [1, 2, 3];
        const simData = getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]]);
        const sims_2 = [simData, simData, simData];
        const groups_2 = getGrouped(0.01, items_2, sims_2);
        const actual_5 = groups_2.length | 0;
        if ((actual_5 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_5, 1, "one group for identical items");
        }
        else {
            let valueType_2;
            let copyOfStruct_2 = actual_5;
            valueType_2 = int32_type;
            const primitiveTypes_2 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_2;
            if (contains(valueType_2, primitiveTypes_2, {
                Equals: equals,
                GetHashCode: (x_2) => (structuralHash(x_2) | 0),
            })) {
                const arg_7 = int32ToString(1);
                const arg_1_2 = int32ToString(actual_5);
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("one group for identical items");
            }
            else {
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_5)("one group for identical items");
            }
            throw new Exception(errorMsg_2);
        }
        const actual_7 = item(0, groups_2).length | 0;
        if ((actual_7 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, 3, "group contains all items");
        }
        else {
            let valueType_3;
            let copyOfStruct_3 = actual_7;
            valueType_3 = int32_type;
            const primitiveTypes_3 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_3;
            if (contains(valueType_3, primitiveTypes_3, {
                Equals: equals,
                GetHashCode: (x_3) => (structuralHash(x_3) | 0),
            })) {
                const arg_8 = int32ToString(3);
                const arg_1_3 = int32ToString(actual_7);
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("group contains all items");
            }
            else {
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_7)("group contains all items");
            }
            throw new Exception(errorMsg_3);
        }
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getGrouped throws on count mismatch", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        const items_3 = [1, 2];
        const sims_3 = [getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(0, 0)]]])];
        const f = () => {
            getGrouped(0.01, items_3, sims_3);
        };
        Expect_throws(f, "throws on count mismatch");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areSimilar with tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        const pts1_7 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]];
        const pts2_6 = [["cat1", [Pt_$ctor_7B00E9A0(0.005, 0.005), Pt_$ctor_7B00E9A0(1, 0)]]];
        const obj1_7 = getSimilarityData(pts1_7);
        const obj2_7 = getSimilarityData(pts2_6);
        Expect_isTrue(areSimilar(0.01, obj1_7, obj2_7))("similar within tolerance 0.01");
        Expect_isFalse(areSimilar(0.001, obj1_7, obj2_7))("not similar with tolerance 0.001");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areSimilar with different point counts", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        const pts1_8 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]];
        const pts2_7 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(2, 0)]]];
        const obj1_8 = getSimilarityData(pts1_8);
        const obj2_8 = getSimilarityData(pts2_7);
        Expect_isFalse(areSimilar(0.01, obj1_8, obj2_8))("different point counts are not similar");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSimilarityData creates correct bounding rectangle", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        const pts_2 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(3, 4)]]];
        const obj_2 = getSimilarityData(pts_2);
        Expect_floatClose(tol, obj_2.extend.X, 3, "extent X is 3");
        Expect_floatClose(tol, obj_2.extend.Y, 4, "extent Y is 4");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSimilarityData handles single point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        const pts_3 = [["cat1", [Pt_$ctor_7B00E9A0(5, 5)]]];
        const obj_3 = getSimilarityData(pts_3);
        const actual_11 = obj_3.groups.length | 0;
        if ((actual_11 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_11, 1, "single point object created");
        }
        else {
            let valueType_4;
            let copyOfStruct_4 = actual_11;
            valueType_4 = int32_type;
            const primitiveTypes_4 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_4;
            if (contains(valueType_4, primitiveTypes_4, {
                Equals: equals,
                GetHashCode: (x_4) => (structuralHash(x_4) | 0),
            })) {
                const arg_9 = int32ToString(1);
                const arg_1_4 = int32ToString(actual_11);
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("single point object created");
            }
            else {
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_11)("single point object created");
            }
            throw new Exception(errorMsg_4);
        }
        const actual_13 = item(0, obj_3.groups).points.length | 0;
        if ((actual_13 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_13, 1, "single point in group");
        }
        else {
            let valueType_5;
            let copyOfStruct_5 = actual_13;
            valueType_5 = int32_type;
            const primitiveTypes_5 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_5;
            if (contains(valueType_5, primitiveTypes_5, {
                Equals: equals,
                GetHashCode: (x_5) => (structuralHash(x_5) | 0),
            })) {
                const arg_10 = int32ToString(1);
                const arg_1_5 = int32ToString(actual_13);
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("single point in group");
            }
            else {
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_13)("single point in group");
            }
            throw new Exception(errorMsg_5);
        }
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areSimilar with empty point sets throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        const pts1_9 = [["cat1", []]];
        const pts2_8 = [["cat1", []]];
        Expect_throws(() => {
            const obj1_9 = getSimilarityData(pts1_9);
            const obj2_9 = getSimilarityData(pts2_8);
            areSimilar(0.01, obj1_9, obj2_9);
        }, "empty point sets throw");
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})()]));

