

open Expecto
open Euclid

 [<Tests>]
let PtTests =
    testList "Euclid Tests" [
         testCase "DirectionDiamondTo" <| fun () ->
            let p = Pt(0.0,0.0)
            let o = Pt(1.0,1.0)
            let dir = p.DirectionDiamondTo(o)
            Expect.equal dir 0.5 "Expected direction to be 0.5"
             let p2 = Pt(0.0,0.0)
            let o2 = Pt(-1.0,-1.0)
            let dir2 = p2.DirectionDiamondTo(o2)
            Expect.equal dir2 2.5 "Expected direction to be 2.5"
    ]