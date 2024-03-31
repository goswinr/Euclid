module Euclid.Tests

#if FABLE_COMPILER

open Fable.Mocha
Mocha.runTests Tests.rect2DTests //|> ignore




#else
open Expecto

[<EntryPoint>]
let main argv =

    runTestsWithCLIArgs [] [||] Tests.rect2DTests
    |||
    0

#endif