module Line

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto

#endif

let inline eq a b = Pt.distance a b < 1e-9

let rnd = System.Random()


let rFloat min max =
    (rnd.NextDouble()*(max-min))+min


let expectEqualEpsilon (a:float) b txt =
    let ok = abs(a-b) < 1e-9
    if not ok then printfn $"expectEqualEpsilon: {b}, and: {a}"
    Expect.isTrue ok txt

let tests =
  testList "Line2D tests" [

        for i = 1 to 10 do
            let v = Vc(rFloat -99. 99., rFloat -99. 99.)
            let p = Pt(rFloat -99. 99., rFloat -99. 99.)
            let a = Line2D.createFromPtAndVc(p,v)
            let t = a.EvaluateAt(rFloat 0. 1.)
            let b = Line2D.createFromPtAndVc(t, v * (rFloat -2. 2))
            let d = Line2D.distanceBetweenLines(a,b)
            if d > 1e-9 then
                printfn $"line A from to: ({a.FromX}, {a.FromY}) , ({a.ToX}, {a.ToY})"
                printfn $"line B from to: ({b.FromX}, {b.FromY}) , ({b.ToX}, {b.ToY})"
                printfn $"distance: {d}"

            test (sprintf "distanceBetweenLines random %d" i) {
                "distance ok" |> Expect.isTrue (d < 1e-9)
            }

        test "Line distanceBetweenLines manual 1" {
            let a = Line2D.createFromPtAndVc(Pt(0.,0.), Vc.Xaxis)
            let b = Line2D.createFromPtAndVc(Pt(0.,2.), Vc.Xaxis)
            let d = Line2D.distanceBetweenLines(a,b)
            "distance is 2." |> expectEqualEpsilon d 2.
        }

        test "Line distanceBetweenLines manual 2" {
            let a = Line2D.createFromPtAndVc(Pt(0.,0.), Vc.Xaxis)
            let b = Line2D.createFromPtAndVc(Pt(1.,2.), Vc.Xaxis)
            let d = Line2D.distanceBetweenLines(a,b)
            "distance is 2." |> expectEqualEpsilon d 2.
        }

        test "Line distanceBetweenLines manual 3" {
            let a = Line2D.createFromPtAndVc(Pt(0.,0.), Vc.Yaxis)
            let b = Line2D.createFromPtAndVc(Pt(2.,1.), Vc.Yaxis)
            let d = Line2D.distanceBetweenLines(a,b)
            "distance is 2." |> expectEqualEpsilon d 2.
        }

        let mutable continueShift = true
        for i = 0 to 50 do
            if continueShift then
                let ii = float i
                let shift = ii * ii * ii * 0.001 // us exponent of 1.5 to progressively increase distance
                let a = Line2D(0,0,0,100)
                let b = Line2D(0,-50,shift,50)
                let b = if i % 2 = 0 then b else b.Reversed
                let a = if i % 3 = 0 then a else a.Reversed
                let ang = Vc.angle180 a.Tangent b.Tangent
                let d = Line2D.distanceBetweenLines(a,b)
                let ok = d <= shift*0.55 // the actual distance it half or less
                if not ok then
                    printfn $"  *  angle {ang}"
                    printfn $"  *  distance: {d}"
                    printfn $"  *  shift: {shift}"
                    printfn $"  *  line A from to: Line2D({a.FromX}, {a.FromY}, {a.ToX}, {a.ToY})"
                    printfn $"  *  line B from to: Line2D({b.FromX}, {b.FromY}, {b.ToX}, {b.ToY})"
                    continueShift <- false
                test (sprintf $"distance of almost Parallel {i},  angle {ang} (= {180.-ang})" ) {
                    "distance is as expected in almost Parallel Lines" |> Expect.isTrue ok
                }


  ]