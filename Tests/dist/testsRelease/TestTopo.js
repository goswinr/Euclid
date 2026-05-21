
import { Record } from "./fable_modules/fable-library-js.5.0.0/Types.js";
import { equals, class_type, decimal_type, string_type, float64_type, bool_type, record_type, int32_type } from "./fable_modules/fable-library-js.5.0.0/Reflection.js";
import { Line2D_$reflection } from "./Src/Line2D.js";
import { Vc_$ctor_7B00E9A0 } from "./Src/Vc.js";
import { Pt_$ctor_7B00E9A0 } from "./Src/Pt.js";
import { Line2D_$ctor_77D16AC0 } from "./Src/Line2D.js";
import { Test_TestCaseBuilder__Zero, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Topology2D_join_Z484E292B } from "./Src/Topology2D.js";
import { int32ToString, structuralHash, Exception, assertEqual } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { singleton, ofArray, contains } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { printf, toText } from "./fable_modules/fable-library-js.5.0.0/String.js";
import { item } from "./fable_modules/fable-library-js.5.0.0/Array.js";

export class Li extends Record {
    constructor(idx, ln) {
        super();
        this.idx = (idx | 0);
        this.ln = ln;
    }
}

export function Li_$reflection() {
    return record_type("TestTopo.Li", [], Li, () => [["idx", int32_type], ["ln", Line2D_$reflection()]]);
}

export const v1 = Vc_$ctor_7B00E9A0(0, 1);

export const h1 = Vc_$ctor_7B00E9A0(1, 0);

export const lns = (() => {
    let p, v, p_1, v_1, p_2, v_2, p_3, v_3, p_4, v_4, p_5, v_5;
    const collection = [new Li(0, (p = Pt_$ctor_7B00E9A0(0, 2), (v = v1, Line2D_$ctor_77D16AC0(p.X, p.Y, p.X + v.X, p.Y + v.Y)))), new Li(1, (p_1 = Pt_$ctor_7B00E9A0(0, 3), (v_1 = v1, Line2D_$ctor_77D16AC0(p_1.X, p_1.Y, p_1.X + v_1.X, p_1.Y + v_1.Y)))), new Li(2, (p_2 = Pt_$ctor_7B00E9A0(0, 1), (v_2 = v1, Line2D_$ctor_77D16AC0(p_2.X, p_2.Y, p_2.X + v_2.X, p_2.Y + v_2.Y)))), new Li(3, (p_3 = Pt_$ctor_7B00E9A0(0, 0), (v_3 = v1, Line2D_$ctor_77D16AC0(p_3.X, p_3.Y, p_3.X + v_3.X, p_3.Y + v_3.Y)))), new Li(4, (p_4 = Pt_$ctor_7B00E9A0(1, 5), (v_4 = h1, Line2D_$ctor_77D16AC0(p_4.X, p_4.Y, p_4.X + v_4.X, p_4.Y + v_4.Y)))), new Li(5, (p_5 = Pt_$ctor_7B00E9A0(0, 5), (v_5 = h1, Line2D_$ctor_77D16AC0(p_5.X, p_5.Y, p_5.X + v_5.X, p_5.Y + v_5.Y))))];
    return Array.from(collection);
})();

export const tests = Test_testList("Topology ", singleton((() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("join2D lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let copyOfStruct, arg, arg_1, copyOfStruct_1, arg_6, arg_1_1, copyOfStruct_2, arg_7, arg_1_2, copyOfStruct_3, arg_8, arg_1_3, copyOfStruct_4, arg_9, arg_1_4, copyOfStruct_5, arg_10, arg_1_5, copyOfStruct_6, arg_11, arg_1_6;
        const gs = Topology2D_join_Z484E292B((ln) => ln.ln, 0.001, lns);
        const actual_1 = gs.length | 0;
        if ((actual_1 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, 2, "gs length");
        }
        else {
            throw new Exception(contains((copyOfStruct = actual_1, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x) => (structuralHash(x) | 0),
            }) ? ((arg = int32ToString(2), (arg_1 = int32ToString(actual_1), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("gs length")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_1)("gs length"));
        }
        const actual_3 = item(0, gs).length | 0;
        if ((actual_3 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, 4, "gs[0] length");
        }
        else {
            throw new Exception(contains((copyOfStruct_1 = actual_3, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_1) => (structuralHash(x_1) | 0),
            }) ? ((arg_6 = int32ToString(4), (arg_1_1 = int32ToString(actual_3), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("gs[0] length")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_3)("gs[0] length"));
        }
        const actual_5 = item(1, gs).length | 0;
        if ((actual_5 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_5, 2, "gs[1] length");
        }
        else {
            throw new Exception(contains((copyOfStruct_2 = actual_5, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_2) => (structuralHash(x_2) | 0),
            }) ? ((arg_7 = int32ToString(2), (arg_1_2 = int32ToString(actual_5), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("gs[1] length")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_5)("gs[1] length"));
        }
        const actual_7 = item(0, item(0, gs)).idx | 0;
        if ((actual_7 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, 3, "gs[0][0] idx=3");
        }
        else {
            throw new Exception(contains((copyOfStruct_3 = actual_7, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_3) => (structuralHash(x_3) | 0),
            }) ? ((arg_8 = int32ToString(3), (arg_1_3 = int32ToString(actual_7), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("gs[0][0] idx=3")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_7)("gs[0][0] idx=3"));
        }
        const actual_9 = item(3, item(0, gs)).idx | 0;
        if ((actual_9 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_9, 1, "gs[0][3] idx=1");
        }
        else {
            throw new Exception(contains((copyOfStruct_4 = actual_9, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_4) => (structuralHash(x_4) | 0),
            }) ? ((arg_9 = int32ToString(1), (arg_1_4 = int32ToString(actual_9), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("gs[0][3] idx=1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_9)("gs[0][3] idx=1"));
        }
        const actual_11 = item(0, item(1, gs)).idx | 0;
        if ((actual_11 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_11, 5, "gs[1][0] idx=5");
        }
        else {
            throw new Exception(contains((copyOfStruct_5 = actual_11, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_5) => (structuralHash(x_5) | 0),
            }) ? ((arg_10 = int32ToString(5), (arg_1_5 = int32ToString(actual_11), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("gs[1][0] idx=5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_11)("gs[1][0] idx=5"));
        }
        const actual_13 = item(1, item(1, gs)).idx | 0;
        if ((actual_13 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_13, 4, "gs[1][1] idx=4");
        }
        else {
            throw new Exception(contains((copyOfStruct_6 = actual_13, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_6) => (structuralHash(x_6) | 0),
            }) ? ((arg_11 = int32ToString(4), (arg_1_6 = int32ToString(actual_13), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_11)(arg_1_6)("gs[1][1] idx=4")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_13)("gs[1][1] idx=4"));
        }
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})()));

