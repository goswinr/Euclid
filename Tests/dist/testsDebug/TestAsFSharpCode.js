
import { Expect_isTrue, Test_testCase, Test_testList } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { singleton } from "./fable_modules/fable-library-js.5.0.0/List.js";

export const tests = Test_testList("AsFSharpCode", singleton(Test_testCase("Verify AsFSharpCode generates valid F# syntax", () => {
    let result = true;
    Expect_isTrue(result)("AsFSharpCode generated invalid code");
})));

