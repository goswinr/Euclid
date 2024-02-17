module Euclid.Tests

open Expecto

[<EntryPoint>]
let main argv =
    // relies on [<Test>] attribute:
    runTestsInAssemblyWithCLIArgs [] argv

