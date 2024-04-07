module Euclid.Tests

#if FABLE_COMPILER

open Fable.Mocha

Mocha.runTests Rect2D.tests
|||
Mocha.runTests Polyline.tests



#else
open Expecto

[<EntryPoint>]
let main argv =

    runTestsWithCLIArgs [] [||] Rect2D.tests
    |||
    runTestsWithCLIArgs [] [||] Polyline.tests
    |||
    0

#endif