module Topo

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let inline eq a b = Pt.distance a b < 1e-9


type Li = {
    idx:int
    ln:Line2D
}
let v1 = Vc(0,1)
let h1 = Vc(1,0)
let lns =
    [|
    {idx = 0; ln = Line2D.createFromPtAndVc (Pt(0,2), v1)}
    {idx = 1; ln = Line2D.createFromPtAndVc (Pt(0,3), v1)}
    {idx = 2; ln = Line2D.createFromPtAndVc (Pt(0,1), v1)}
    {idx = 3; ln = Line2D.createFromPtAndVc (Pt(0,0), v1)}

    {idx = 4; ln = Line2D.createFromPtAndVc (Pt(1,5), h1)}
    {idx = 5; ln = Line2D.createFromPtAndVc (Pt(0,5), h1)}
    |]
    |> ResizeArray



let tests =
  testList "Topo " [

    test "5 lines" {
        let gs = Topology.join2D( (fun (ln:Li) -> ln.ln), 0.001, lns)
        "gs length" |> Expect.equal gs.Count  2
        "gs[0] length" |> Expect.equal gs.[0].Count  4
        "gs[1] length" |> Expect.equal gs.[1].Count  2
        "gs[0][0] idx=3" |> Expect.equal gs.[0].[0].idx  3
        "gs[0][3] idx=1" |> Expect.equal gs.[0].[3].idx  1
        "gs[1][0] idx=5" |> Expect.equal gs.[1].[0].idx  5
        "gs[1][1] idx=4" |> Expect.equal gs.[1].[1].idx  4

    }

  ]
