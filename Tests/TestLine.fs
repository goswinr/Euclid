module TestLine

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

let testsIsCoincident =
    testList "Line2D IsCoincidentTo" [

        test "IsCoincidentTo identical lines" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 0., 10., 0.)
            "identical lines are coincident" |> Expect.isTrue (a.IsCoincidentTo(b))
        }

        test "IsCoincidentTo same ray different lengths" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 0., 5., 0.)
            "same ray different lengths are coincident" |> Expect.isTrue (a.IsCoincidentTo(b))
        }

        test "IsCoincidentTo same ray offset start" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(5., 0., 15., 0.)
            "same ray offset start are coincident" |> Expect.isTrue (a.IsCoincidentTo(b))
        }

        test "IsCoincidentTo parallel but offset vertically" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 2., 10., 2.)
            "parallel lines with vertical offset should not be coincident" |> Expect.isFalse (a.IsCoincidentTo(b))
        }

        test "IsCoincidentTo parallel within distance tolerance" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 0.0000001, 10., 0.0000001) // very small offset
            "parallel lines within distance tolerance are coincident" |> Expect.isTrue (a.IsCoincidentTo(b, 1e-6))
        }

        test "IsCoincidentTo parallel outside distance tolerance" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 0.001, 10., 0.001) // larger offset
            "parallel lines outside distance tolerance are not coincident" |> Expect.isFalse (a.IsCoincidentTo(b, 1e-6))
        }

        test "IsCoincidentTo perpendicular lines" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 0., 0., 10.)
            "perpendicular lines are not coincident" |> Expect.isFalse (a.IsCoincidentTo(b))
        }

        test "IsCoincidentTo nearly parallel within angle tolerance" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 0., 10., 0.01) // very small angle
            "nearly parallel within tolerance are coincident" |> Expect.isTrue (a.IsCoincidentTo(b))
        }

        test "IsCoincidentTo nearly parallel outside angle tolerance" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 0., 10., 1.) // about 5.7 degrees
            "nearly parallel outside tolerance are not coincident" |> Expect.isFalse (a.IsCoincidentTo(b))
        }

        test "IsCoincidentTo zero length first line throws" {
            let a = Line2D(0., 0., 0., 0.)
            let b = Line2D(0., 0., 10., 0.)
            "zero length first line throws exception" |> Expect.throws (fun () -> a.IsCoincidentTo(b) |> ignore)
        }

        test "IsCoincidentTo zero length second line throws" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(5., 0., 5., 0.)
            "zero length second line throws exception" |> Expect.throws (fun () -> a.IsCoincidentTo(b) |> ignore)
        }

        test "IsCoincidentTo both zero length throws" {
            let a = Line2D(0., 0., 0., 0.)
            let b = Line2D(0., 0., 0., 0.)
            "both zero length throws exception" |> Expect.throws (fun () -> a.IsCoincidentTo(b) |> ignore)
        }

        test "IsCoincidentTo very short lines same direction" {
            let a = Line2D(0., 0., 1e-10, 0.)
            let b = Line2D(0., 0., 1e-11, 0.)
            "very short lines on same ray are coincident" |> Expect.isTrue (a.IsCoincidentTo(b))
        }

        test "IsCoincidentTo opposite directions on same ray" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(10., 0., 0., 0.)
            "opposite directions on same ray are still coincident" |> Expect.isTrue (a.IsCoincidentTo(b))
        }

        test "IsCoincidentTo diagonal lines same ray" {
            let a = Line2D(0., 0., 10., 10.)
            let b = Line2D(5., 5., 15., 15.)
            "diagonal lines same ray are coincident" |> Expect.isTrue (a.IsCoincidentTo(b))
        }

        test "IsCoincidentTo diagonal lines parallel but offset" {
            let a = Line2D(0., 0., 10., 10.)
            let b = Line2D(1., 0., 11., 10.)
            "diagonal lines parallel but offset are not coincident" |> Expect.isFalse (a.IsCoincidentTo(b))
        }

        test "IsCoincidentTo vertical lines same ray" {
            let a = Line2D(5., 0., 5., 10.)
            let b = Line2D(5., 3., 5., 13.)
            "vertical lines same ray are coincident" |> Expect.isTrue (a.IsCoincidentTo(b))
        }

        test "IsCoincidentTo vertical lines parallel but offset" {
            let a = Line2D(5., 0., 5., 10.)
            let b = Line2D(7., 0., 7., 10.)
            "vertical lines parallel but offset are not coincident" |> Expect.isFalse (a.IsCoincidentTo(b))
        }

        test "IsCoincidentTo custom distance tolerance strict" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 0.0005, 10., 0.0005)
            "with strict tolerance not coincident" |> Expect.isFalse (a.IsCoincidentTo(b, 1e-7))
            "with relaxed tolerance coincident" |> Expect.isTrue (a.IsCoincidentTo(b, 1e-5))
        }

        test "IsCoincidentTo custom angle tolerance strict" {
            let a = Line2D(0., 0., 100., 0.)
            let b = Line2D(0., 0., 100., 0.1) // about 0.057 degrees
            "with default angle tolerance (0.25 deg) coincident" |> Expect.isTrue (a.IsCoincidentTo(b))
            "with strict angle tolerance not coincident" |> Expect.isFalse (a.IsCoincidentTo(b, 1e-6, Tangent.``0.01``))
        }

        test "IsCoincidentTo negative coordinates" {
            let a = Line2D(-10., -5., -20., -15.)
            let b = Line2D(-15., -10., -25., -20.)
            "negative coordinates same ray are coincident" |> Expect.isTrue (a.IsCoincidentTo(b))
        }

        test "IsCoincidentTo mixed positive negative" {
            let a = Line2D(-5., -5., 5., 5.)
            let b = Line2D(-10., -10., 10., 10.)
            "mixed sign coordinates same ray are coincident" |> Expect.isTrue (a.IsCoincidentTo(b))
        }

        test "IsCoincidentTo almost parallel lines small angle" {
            let a = Line2D(0., 0., 1000., 0.)
            let b = Line2D(0., 0., 1000., 1.) // about 0.057 degrees
            "lines at 0.057 degrees are coincident with default tolerance" |> Expect.isTrue (a.IsCoincidentTo(b))
        }

        for i = 1 to 10 do
            let scale = rFloat 0.1 100.
            let angle = rFloat -180. 180.
            let vec = Vc.Xaxis.Rotate(angle) * scale
            let a = Line2D.createFromPtAndVc(Pt.Origin, vec)
            let b = Line2D.createFromPtAndVc(a.EvaluateAt(rFloat -10. 10.), a.Tangent * (rFloat 0.1 2.))
            test (sprintf "IsCoincidentTo random same ray %d" i) {
                "random lines on same ray are coincident" |> Expect.isTrue (a.IsCoincidentTo(b))
            }

        for i = 1 to 10 do
            let scale = rFloat 0.1 100.
            let angle = rFloat -180. 180.
            let dist = rFloat 0.5 5.
            let vec = Vc.Xaxis.Rotate(angle) * scale
            let a = Line2D.createFromPtAndVc(Pt.Origin, vec)
            let normal = a.Tangent.Rotate90CCW.Unitized
            let offset = Pt.Origin + normal * dist
            let b = Line2D.createFromPtAndVc(offset, a.Tangent * (rFloat 0.1 2.))
            test (sprintf "IsCoincidentTo random parallel offset %d" i) {
                "random parallel lines with offset are not coincident" |> Expect.isFalse (a.IsCoincidentTo(b))
            }

        for i = 1 to 10 do
            let scale = rFloat 0.1 100.
            let angle1 = rFloat -180. 180.
            let angle2 = angle1 + (rFloat 5. 175.)
            let vec1 = Vc.Xaxis.Rotate(angle1) * scale
            let vec2 = Vc.Xaxis.Rotate(angle2) * scale
            let a = Line2D.createFromPtAndVc(Pt.Origin, vec1)
            let b = Line2D.createFromPtAndVc(Pt.Origin, vec2)
            test (sprintf "IsCoincidentTo random non-parallel %d" i) {
                "random non-parallel lines are not coincident" |> Expect.isFalse (a.IsCoincidentTo(b))
            }
    ]


let testsFastMethods =
    testList "Line2D Fast Methods" [

        // IsParallelToFast tests
        test "IsParallelToFast identical lines" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 0., 10., 0.)
            "identical lines are parallel" |> Expect.isTrue (a.IsParallelToFast(b))
        }

        test "IsParallelToFast parallel same direction" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 5., 10., 5.)
            "parallel lines same direction" |> Expect.isTrue (a.IsParallelToFast(b))
        }

        test "IsParallelToFast parallel opposite direction" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(10., 5., 0., 5.)
            "parallel lines opposite direction" |> Expect.isTrue (a.IsParallelToFast(b))
        }

        test "IsParallelToFast perpendicular lines" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 0., 0., 10.)
            "perpendicular lines are not parallel" |> Expect.isFalse (a.IsParallelToFast(b))
        }

        test "IsParallelToFast zero length lines returns true" {
            let a = Line2D(0., 0., 0., 0.)
            let b = Line2D(5., 5., 5., 5.)
            "zero length lines return true (documented behavior)" |> Expect.isTrue (a.IsParallelToFast(b))
        }

        test "IsParallelToFast very long almost parallel lines" {
            let a = Line2D(0., 0., 1000., 0.)
            let b = Line2D(0., 0., 1000., 1.)
            "long almost parallel lines with default tolerance" |> Expect.isFalse (a.IsParallelToFast(b, 1e-6))
            "long almost parallel lines with larger tolerance" |> Expect.isTrue (a.IsParallelToFast(b, 1001.))
        }

        // IsCoincidentToFast tests
        test "IsCoincidentToFast identical lines" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 0., 10., 0.)
            "identical lines are coincident" |> Expect.isTrue (a.IsCoincidentToFast(b))
        }

        test "IsCoincidentToFast same ray offset start" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(5., 0., 15., 0.)
            "same ray offset start are coincident" |> Expect.isTrue (a.IsCoincidentToFast(b))
        }

        test "IsCoincidentToFast parallel but offset" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 2., 10., 2.)
            "parallel lines with offset are not coincident" |> Expect.isFalse (a.IsCoincidentToFast(b))
        }

        test "IsCoincidentToFast perpendicular lines" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 0., 0., 10.)
            "perpendicular lines are not coincident" |> Expect.isFalse (a.IsCoincidentToFast(b))
        }

        test "IsCoincidentToFast opposite directions same line" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(10., 0., 0., 0.)
            "opposite directions on same line are coincident" |> Expect.isTrue (a.IsCoincidentToFast(b))
        }

        // IsParallelAndOrientedToFast tests
        test "IsParallelAndOrientedToFast same direction" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 5., 10., 5.)
            "parallel same direction are oriented" |> Expect.isTrue (a.IsParallelAndOrientedToFast(b))
        }

        test "IsParallelAndOrientedToFast opposite direction" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(10., 5., 0., 5.)
            "parallel opposite direction are not oriented" |> Expect.isFalse (a.IsParallelAndOrientedToFast(b))
        }

        test "IsParallelAndOrientedToFast perpendicular" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 0., 0., 10.)
            "perpendicular lines are not parallel and oriented" |> Expect.isFalse (a.IsParallelAndOrientedToFast(b))
        }

        test "IsParallelAndOrientedToFast zero length returns false" {
            let a = Line2D(0., 0., 0., 0.)
            let b = Line2D(0., 0., 10., 0.)
            "zero length line returns false due to minDotProduct" |> Expect.isFalse (a.IsParallelAndOrientedToFast(b))
        }

        test "IsParallelAndOrientedToFast very short lines" {
            let a = Line2D(0., 0., 1e-7, 0.)
            let b = Line2D(0., 0., 1e-7, 0.)
            "very short lines below minDotProduct" |> Expect.isFalse (a.IsParallelAndOrientedToFast(b, 1e-6, 1e-6))
        }

        // IsParallelAndOpposingToFast tests
        test "IsParallelAndOpposingToFast opposite direction" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(10., 5., 0., 5.)
            "parallel opposite direction are opposing" |> Expect.isTrue (a.IsParallelAndOpposingToFast(b))
        }

        test "IsParallelAndOpposingToFast same direction" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 5., 10., 5.)
            "parallel same direction are not opposing" |> Expect.isFalse (a.IsParallelAndOpposingToFast(b))
        }

        test "IsParallelAndOpposingToFast perpendicular" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 0., 0., 10.)
            "perpendicular lines are not parallel and opposing" |> Expect.isFalse (a.IsParallelAndOpposingToFast(b))
        }

        test "IsParallelAndOpposingToFast zero length returns false" {
            let a = Line2D(0., 0., 0., 0.)
            let b = Line2D(10., 0., 0., 0.)
            "zero length line returns false due to maxDotProduct" |> Expect.isFalse (a.IsParallelAndOpposingToFast(b))
        }

        // IsCoincidentAndOrientedToFast tests
        test "IsCoincidentAndOrientedToFast same direction same line" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(5., 0., 15., 0.)
            "same direction same line are coincident and oriented" |> Expect.isTrue (a.IsCoincidentAndOrientedToFast(b))
        }

        test "IsCoincidentAndOrientedToFast opposite direction same line" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(10., 0., 0., 0.)
            "opposite direction same line are not oriented" |> Expect.isFalse (a.IsCoincidentAndOrientedToFast(b))
        }

        test "IsCoincidentAndOrientedToFast parallel offset" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 2., 10., 2.)
            "parallel with offset are not coincident" |> Expect.isFalse (a.IsCoincidentAndOrientedToFast(b))
        }

        test "IsCoincidentAndOrientedToFast diagonal same ray" {
            let a = Line2D(0., 0., 10., 10.)
            let b = Line2D(5., 5., 15., 15.)
            "diagonal same ray are coincident and oriented" |> Expect.isTrue (a.IsCoincidentAndOrientedToFast(b))
        }

        // IsCoincidentAndOpposingToFast tests
        test "IsCoincidentAndOpposingToFast opposite direction same line" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(10., 0., 0., 0.)
            "opposite direction same line are coincident and opposing" |> Expect.isTrue (a.IsCoincidentAndOpposingToFast(b))
        }

        test "IsCoincidentAndOpposingToFast same direction same line" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(5., 0., 15., 0.)
            "same direction same line are not opposing" |> Expect.isFalse (a.IsCoincidentAndOpposingToFast(b))
        }

        test "IsCoincidentAndOpposingToFast parallel offset" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(10., 2., 0., 2.)
            "parallel with offset are not coincident" |> Expect.isFalse (a.IsCoincidentAndOpposingToFast(b))
        }

        test "IsCoincidentAndOpposingToFast diagonal opposing" {
            let a = Line2D(0., 0., 10., 10.)
            let b = Line2D(15., 15., 5., 5.)
            "diagonal opposing same line are coincident and opposing" |> Expect.isTrue (a.IsCoincidentAndOpposingToFast(b))
        }

        // Custom tolerance tests
        test "IsParallelToFast custom tolerance strict" {
            let a = Line2D(0., 0., 100., 0.)
            let b = Line2D(0., 0., 100., 0.5)
            "with strict tolerance not parallel" |> Expect.isFalse (a.IsParallelToFast(b, 1e-9))
            "with relaxed tolerance parallel" |> Expect.isTrue (a.IsParallelToFast(b, 100.))
        }

        test "IsCoincidentAndOrientedToFast custom tolerances" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 0., 5., 0.)
            "with default tolerances coincident" |> Expect.isTrue (a.IsCoincidentAndOrientedToFast(b))
            "with very strict minDotProduct not coincident" |> Expect.isFalse (a.IsCoincidentAndOrientedToFast(b, 1e-6, 100.))
        }

        // Random tests
        for i = 1 to 10 do
            let angle = rFloat -180. 180.
            let scale = rFloat 1. 100.
            let vec = Vc.Xaxis.Rotate(angle) * scale
            let a = Line2D.createFromPtAndVc(Pt.Origin, vec)
            let b = Line2D.createFromPtAndVc(a.EvaluateAt(rFloat 0. 1.), a.Tangent * (rFloat 0.5 2.))
            test (sprintf "Fast methods random same ray oriented %d" i) {
                "random same ray are parallel and oriented" |> Expect.isTrue (a.IsParallelAndOrientedToFast(b))
                "random same ray are coincident and oriented" |> Expect.isTrue (a.IsCoincidentAndOrientedToFast(b))
            }

        for i = 1 to 10 do
            let angle = rFloat -180. 180.
            let scale = rFloat 1. 100.
            let vec = Vc.Xaxis.Rotate(angle) * scale
            let a = Line2D.createFromPtAndVc(Pt.Origin, vec)
            let b = Line2D.createFromPtAndVc(a.EvaluateAt(rFloat 0. 1.), -a.Tangent * (rFloat 0.5 2.))
            test (sprintf "Fast methods random same ray opposing %d" i) {
                "random opposing are parallel and opposing" |> Expect.isTrue (a.IsParallelAndOpposingToFast(b))
                "random opposing are coincident and opposing" |> Expect.isTrue (a.IsCoincidentAndOpposingToFast(b))
            }
    ]


let testsDistanceBetweenLines =
    testList "Line2D distanceBetweenLines" [

        for i = 1 to 10 do
            let v = Vc(rFloat -99. 99., rFloat -99. 99.)
            let p = Pt(rFloat -99. 99., rFloat -99. 99.)
            let a = Line2D.createFromPtAndVc(p,v)
            let t = a.EvaluateAt(rFloat 0. 1.)
            let b = Line2D.createFromPtAndVc(t, v * (rFloat -2. 2))
            let d = Line2D.distanceToLine a b
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
            let d = Line2D.distanceToLine a b
            "distance is 2." |> expectEqualEpsilon d 2.
        }

        test "Line distanceBetweenLines manual 2" {
            let a = Line2D.createFromPtAndVc(Pt(0.,0.), Vc.Xaxis)
            let b = Line2D.createFromPtAndVc(Pt(1.,2.), Vc.Xaxis)
            let d = Line2D.distanceToLine a b
            "distance is 2." |> expectEqualEpsilon d 2.
        }

        test "Line distanceBetweenLines manual 3" {
            let a = Line2D.createFromPtAndVc(Pt(0.,0.), Vc.Yaxis)
            let b = Line2D.createFromPtAndVc(Pt(2.,1.), Vc.Yaxis)
            let d = Line2D.distanceToLine a b
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
                let d = Line2D.distanceToLine a b
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


let testsLine2DBasics =
    testList "Line2D Basics" [

        test "IsZeroLength true for zero length line" {
            let ln = Line2D(5., 3., 5., 3.)
            "zero length line" |> Expect.isTrue ln.IsZeroLength
        }

        test "IsZeroLength false for non-zero line" {
            let ln = Line2D(0., 0., 10., 0.)
            "non-zero length line" |> Expect.isFalse ln.IsZeroLength
        }

        test "IsTiny detects short lines" {
            let ln = Line2D(0., 0., 1e-10, 0.)
            "line shorter than tolerance" |> Expect.isTrue (ln.IsTiny 1e-9)
            "line not shorter than smaller tolerance" |> Expect.isFalse (ln.IsTiny 1e-11)
        }

        test "IsTinySq detects short lines with squared tolerance" {
            let ln = Line2D(0., 0., 1e-5, 0.)
            let lenSq = ln.LengthSq
            "line shorter than squared tolerance" |> Expect.isTrue (ln.IsTinySq (lenSq * 2.))
            "line not shorter than squared tolerance" |> Expect.isFalse (ln.IsTinySq (lenSq * 0.5))
        }

        test "IsXAligned horizontal line" {
            let ln = Line2D(0., 0., 10., 0.)
            "horizontal line is X aligned" |> Expect.isTrue ln.IsXAligned
        }

        test "IsXAligned vertical line" {
            let ln = Line2D(0., 0., 0., 10.)
            "vertical line is not X aligned" |> Expect.isFalse ln.IsXAligned
        }

        test "IsXAligned nearly horizontal within tolerance" {
            let ln = Line2D(0., 0., 10., 1e-10)
            "nearly horizontal within tolerance" |> Expect.isTrue ln.IsXAligned
        }

        test "IsXAligned zero length throws" {
            let ln = Line2D(0., 0., 0., 0.)
            "zero length throws" |> Expect.throws (fun () -> ln.IsXAligned |> ignore)
        }

        test "IsYAligned vertical line" {
            let ln = Line2D(0., 0., 0., 10.)
            "vertical line is Y aligned" |> Expect.isTrue ln.IsYAligned
        }

        test "IsYAligned horizontal line" {
            let ln = Line2D(0., 0., 10., 0.)
            "horizontal line is not Y aligned" |> Expect.isFalse ln.IsYAligned
        }

        test "IsYAligned nearly vertical within tolerance" {
            let ln = Line2D(0., 0., 1e-10, 10.)
            "nearly vertical within tolerance" |> Expect.isTrue ln.IsYAligned
        }

        test "Length calculation" {
            let ln = Line2D(0., 0., 3., 4.)
            "length is 5" |> expectEqualEpsilon ln.Length 5.
        }

        test "LengthSq calculation" {
            let ln = Line2D(0., 0., 3., 4.)
            "length squared is 25" |> expectEqualEpsilon ln.LengthSq 25.
        }

        test "Mid point calculation" {
            let ln = Line2D(0., 0., 10., 10.)
            let mid = ln.Mid
            "midpoint X is 5" |> expectEqualEpsilon mid.X 5.
            "midpoint Y is 5" |> expectEqualEpsilon mid.Y 5.
        }

        test "Reversed line" {
            let ln = Line2D(0., 0., 10., 5.)
            let rev = ln.Reversed
            "reversed from equals original to" |> Expect.isTrue (eq rev.From ln.To)
            "reversed to equals original from" |> Expect.isTrue (eq rev.To ln.From)
        }

        test "EvaluateAt parameter 0" {
            let ln = Line2D(0., 0., 10., 0.)
            let pt = ln.EvaluateAt 0.
            "parameter 0 equals start" |> Expect.isTrue (eq pt ln.From)
        }

        test "EvaluateAt parameter 1" {
            let ln = Line2D(0., 0., 10., 0.)
            let pt = ln.EvaluateAt 1.
            "parameter 1 equals end" |> Expect.isTrue (eq pt ln.To)
        }

        test "EvaluateAt parameter 0.5" {
            let ln = Line2D(0., 0., 10., 0.)
            let pt = ln.EvaluateAt 0.5
            "parameter 0.5 equals mid" |> Expect.isTrue (eq pt ln.Mid)
        }

        test "EvaluateAt parameter 2" {
            let ln = Line2D(0., 0., 10., 0.)
            let pt = ln.EvaluateAt 2.
            "parameter 2 extends beyond end" |> expectEqualEpsilon pt.X 20.
        }

        test "SubLine creates segment" {
            let ln = Line2D(0., 0., 10., 0.)
            let sub = ln.SubLine(0.25, 0.75)
            "subline starts at 0.25" |> expectEqualEpsilon sub.FromX 2.5
            "subline ends at 0.75" |> expectEqualEpsilon sub.ToX 7.5
        }

        test "LengthTillParam positive parameter" {
            let ln = Line2D(0., 0., 10., 0.)
            let len = ln.LengthTillParam 0.5
            "length till 0.5 is 5" |> expectEqualEpsilon len 5.
        }

        test "LengthTillParam negative parameter" {
            let ln = Line2D(0., 0., 10., 0.)
            let len = ln.LengthTillParam -0.5
            "length till -0.5 is -5" |> expectEqualEpsilon len -5.
        }

        test "LengthFromParam to end" {
            let ln = Line2D(0., 0., 10., 0.)
            let len = ln.LengthFromParam 0.5
            "length from 0.5 to end is 5" |> expectEqualEpsilon len 5.
        }

        test "LengthFromParam beyond end" {
            let ln = Line2D(0., 0., 10., 0.)
            let len = ln.LengthFromParam 1.5
            "length from 1.5 is negative" |> expectEqualEpsilon len -5.
        }
    ]


let testsLine2DExtendShrink =
    testList "Line2D Extend/Shrink" [

        test "Extend both ends" {
            let ln = Line2D(0., 0., 10., 0.)
            let ext = ln.Extend(5., 5.)
            "extended start" |> expectEqualEpsilon ext.FromX -5.
            "extended end" |> expectEqualEpsilon ext.ToX 15.
        }

        test "Extend too short line throws" {
            let ln = Line2D(0., 0., 1e-13, 0.)
            "too short throws" |> Expect.throws (fun () -> ln.Extend(1., 1.) |> ignore)
        }

        test "ExtendStart" {
            let ln = Line2D(5., 0., 10., 0.)
            let ext = ln.ExtendStart 5.
            "extended start" |> expectEqualEpsilon ext.FromX 0.
            "end unchanged" |> expectEqualEpsilon ext.ToX 10.
        }

        test "ExtendEnd" {
            let ln = Line2D(0., 0., 10., 0.)
            let ext = ln.ExtendEnd 5.
            "start unchanged" |> expectEqualEpsilon ext.FromX 0.
            "extended end" |> expectEqualEpsilon ext.ToX 15.
        }

        test "ExtendRel both ends" {
            let ln = Line2D(0., 0., 10., 0.)
            let ext = ln.ExtendRel(0.5, 0.5)
            "extended start by half length" |> expectEqualEpsilon ext.FromX -5.
            "extended end by half length" |> expectEqualEpsilon ext.ToX 15.
        }

        test "ExtendStartRel" {
            let ln = Line2D(0., 0., 10., 0.)
            let ext = ln.ExtendStartRel 0.5
            "extended start by half length" |> expectEqualEpsilon ext.FromX -5.
            "end unchanged" |> expectEqualEpsilon ext.ToX 10.
        }

        test "ExtendEndRel" {
            let ln = Line2D(0., 0., 10., 0.)
            let ext = ln.ExtendEndRel 0.5
            "start unchanged" |> expectEqualEpsilon ext.FromX 0.
            "extended end by half length" |> expectEqualEpsilon ext.ToX 15.
        }

        test "Shrink both ends" {
            let ln = Line2D(0., 0., 10., 0.)
            let shr = ln.Shrink(2., 3.)
            "shrunk start" |> expectEqualEpsilon shr.FromX 2.
            "shrunk end" |> expectEqualEpsilon shr.ToX 7.
        }

        test "Shrink too short line throws" {
            let ln = Line2D(0., 0., 1e-13, 0.)
            "too short throws" |> Expect.throws (fun () -> ln.Shrink(1., 1.) |> ignore)
        }

        test "ShrinkStart" {
            let ln = Line2D(0., 0., 10., 0.)
            let shr = ln.ShrinkStart 2.
            "shrunk start" |> expectEqualEpsilon shr.FromX 2.
            "end unchanged" |> expectEqualEpsilon shr.ToX 10.
        }

        test "ShrinkEnd" {
            let ln = Line2D(0., 0., 10., 0.)
            let shr = ln.ShrinkEnd 3.
            "start unchanged" |> expectEqualEpsilon shr.FromX 0.
            "shrunk end" |> expectEqualEpsilon shr.ToX 7.
        }
    ]


let testsLine2DMove =
    testList "Line2D Move/Transform" [

        test "Move by vector" {
            let ln = Line2D(0., 0., 10., 0.)
            let moved = ln.Move(Vc(5., 3.))
            "from moved correctly" |> expectEqualEpsilon moved.FromX 5.
            "from moved correctly Y" |> expectEqualEpsilon moved.FromY 3.
            "to moved correctly" |> expectEqualEpsilon moved.ToX 15.
            "to moved correctly Y" |> expectEqualEpsilon moved.ToY 3.
        }

        test "MoveX" {
            let ln = Line2D(0., 0., 10., 5.)
            let moved = ln.MoveX 7.
            "from X moved" |> expectEqualEpsilon moved.FromX 7.
            "from Y unchanged" |> expectEqualEpsilon moved.FromY 0.
            "to X moved" |> expectEqualEpsilon moved.ToX 17.
            "to Y unchanged" |> expectEqualEpsilon moved.ToY 5.
        }

        test "MoveY" {
            let ln = Line2D(0., 0., 10., 5.)
            let moved = ln.MoveY 3.
            "from X unchanged" |> expectEqualEpsilon moved.FromX 0.
            "from Y moved" |> expectEqualEpsilon moved.FromY 3.
            "to X unchanged" |> expectEqualEpsilon moved.ToX 10.
            "to Y moved" |> expectEqualEpsilon moved.ToY 8.
        }

        test "Scale from origin" {
            let ln = Line2D(1., 2., 3., 4.)
            let scaled = ln.Scale 2.
            "from X scaled" |> expectEqualEpsilon scaled.FromX 2.
            "from Y scaled" |> expectEqualEpsilon scaled.FromY 4.
            "to X scaled" |> expectEqualEpsilon scaled.ToX 6.
            "to Y scaled" |> expectEqualEpsilon scaled.ToY 8.
        }

        test "ScaleOn center point" {
            let ln = Line2D(0., 0., 10., 0.)
            let cen = Pt(5., 0.)
            let scaled = ln.ScaleOn cen 2.
            "from X scaled from center" |> expectEqualEpsilon scaled.FromX -5.
            "to X scaled from center" |> expectEqualEpsilon scaled.ToX 15.
        }

        test "Rotate 90 degrees" {
            let ln = Line2D(0., 0., 10., 0.)
            let rot = Rotation2D.createFromDegrees 90.
            let rotated = ln.Rotate rot
            "from unchanged at origin" |> expectEqualEpsilon rotated.FromX 0.
            "rotated to X" |> expectEqualEpsilon rotated.ToX 0.
            "rotated to Y" |> expectEqualEpsilon rotated.ToY 10.
        }

        test "RotateWithCenter" {
            let ln = Line2D(0., 0., 10., 0.)
            let cen = Pt(5., 0.)
            let rot = Rotation2D.createFromDegrees 90.
            let rotated = ln.RotateWithCenter(cen, rot)
            "rotated around center from X" |> expectEqualEpsilon rotated.FromX 5.
            "rotated around center from Y" |> expectEqualEpsilon rotated.FromY -5.
            "rotated around center to X" |> expectEqualEpsilon rotated.ToX 5.
            "rotated around center to Y" |> expectEqualEpsilon rotated.ToY 5.
        }
    ]


let testsLine2DClosestPoint =
    testList "Line2D Closest Point" [

        test "RayClosestParameter on ray" {
            let ln = Line2D(0., 0., 10., 0.)
            let pt = Pt(5., 5.)
            let param = ln.RayClosestParameter pt
            "closest parameter" |> expectEqualEpsilon param 0.5
        }

        test "RayClosestParameter before start" {
            let ln = Line2D(0., 0., 10., 0.)
            let pt = Pt(-5., 5.)
            let param = ln.RayClosestParameter pt
            "parameter is negative" |> Expect.isTrue (param < 0.)
        }

        test "RayClosestParameter beyond end" {
            let ln = Line2D(0., 0., 10., 0.)
            let pt = Pt(15., 5.)
            let param = ln.RayClosestParameter pt
            "parameter is beyond 1" |> Expect.isTrue (param > 1.)
        }

        test "RayClosestParameter too short throws" {
            let ln = Line2D(0., 0., 1e-10, 0.)
            let pt = Pt(5., 5.)
            "too short throws" |> Expect.throws (fun () -> ln.RayClosestParameter pt |> ignore)
        }

        test "ClosestParameter on line" {
            let ln = Line2D(0., 0., 10., 0.)
            let pt = Pt(5., 5.)
            let param = ln.ClosestParameter pt
            "closest parameter" |> expectEqualEpsilon param 0.5
        }

        test "ClosestParameter before start clamped" {
            let ln = Line2D(0., 0., 10., 0.)
            let pt = Pt(-5., 5.)
            let param = ln.ClosestParameter pt
            "clamped to 0" |> expectEqualEpsilon param 0.
        }

        test "ClosestParameter beyond end clamped" {
            let ln = Line2D(0., 0., 10., 0.)
            let pt = Pt(15., 5.)
            let param = ln.ClosestParameter pt
            "clamped to 1" |> expectEqualEpsilon param 1.
        }

        test "ClosestPoint on line" {
            let ln = Line2D(0., 0., 10., 0.)
            let pt = Pt(5., 5.)
            let cl = ln.ClosestPoint pt
            "closest point X" |> expectEqualEpsilon cl.X 5.
            "closest point Y" |> expectEqualEpsilon cl.Y 0.
        }

        test "ClosestPoint before start clamped" {
            let ln = Line2D(0., 0., 10., 0.)
            let pt = Pt(-5., 5.)
            let cl = ln.ClosestPoint pt
            "clamped to start" |> Expect.isTrue (eq cl ln.From)
        }

        test "SqDistanceFromPoint" {
            let ln = Line2D(0., 0., 10., 0.)
            let pt = Pt(5., 3.)
            let sqDist = ln.SqDistanceFromPoint pt
            "squared distance is 9" |> expectEqualEpsilon sqDist 9.
        }

        test "DistanceToPt" {
            let ln = Line2D(0., 0., 10., 0.)
            let pt = Pt(5., 4.)
            let dist = ln.DistanceToPt pt
            "distance is 4" |> expectEqualEpsilon dist 4.
        }
    ]


let testsLine2DProjection =
    testList "Line2D Projection Methods" [

        test "projectOntoRayParam same direction" {
            let ray = Line2D(0., 0., 10., 0.)
            let ln = Line2D(2., 5., 8., 5.)
            let (p1, p2) = Line2D.projectOntoRayParam ray ln
            "start parameter" |> expectEqualEpsilon p1 0.2
            "end parameter" |> expectEqualEpsilon p2 0.8
        }

        test "projectOntoRayParam opposite direction" {
            let ray = Line2D(0., 0., 10., 0.)
            let ln = Line2D(8., 5., 2., 5.)
            let (p1, p2) = Line2D.projectOntoRayParam ray ln
            "start parameter from" |> expectEqualEpsilon p1 0.8
            "end parameter from" |> expectEqualEpsilon p2 0.2
        }

        test "projectOntoRayParam beyond ray start" {
            let ray = Line2D(5., 0., 10., 0.)
            let ln = Line2D(0., 5., 3., 5.)
            let (p1, p2) = Line2D.projectOntoRayParam ray ln
            "parameters can be negative" |> Expect.isTrue (p1 < 0.)
            "second param less than zero" |> Expect.isTrue (p2 < 0.)
        }

        test "projectOntoRayParam beyond ray end" {
            let ray = Line2D(0., 0., 10., 0.)
            let ln = Line2D(12., 5., 18., 5.)
            let (p1, p2) = Line2D.projectOntoRayParam ray ln
            "parameters beyond 1.0" |> Expect.isTrue (p1 > 1.)
            "second param beyond 1.0" |> Expect.isTrue (p2 > 1.)
        }

        test "projectOntoRayParam diagonal lines" {
            let ray = Line2D(0., 0., 10., 10.)
            let ln = Line2D(5., 0., 10., 5.)
            let (p1, p2) = Line2D.projectOntoRayParam ray ln
            "start parameter calculated via projection" |> Expect.isTrue (abs(p1 - 0.25) < 1e-9)
            "end parameter calculated via projection" |> Expect.isTrue (abs(p2 - 0.75) < 1e-9)
        }

        test "projectOntoRayParam perpendicular projection" {
            let ray = Line2D(0., 0., 10., 0.)
            let ln = Line2D(5., 0., 5., 10.)
            let (p1, p2) = Line2D.projectOntoRayParam ray ln
            "perpendicular collapses to point" |> expectEqualEpsilon p1 p2
            "projection at 0.5" |> expectEqualEpsilon p1 0.5
        }

        test "projectOntoRayParam too short ray throws" {
            let ray = Line2D(0., 0., 1e-13, 0.)
            let ln = Line2D(2., 5., 8., 5.)
            "too short throws" |> Expect.throws (fun () -> Line2D.projectOntoRayParam ray ln |> ignore)
        }

        test "projectOntoRay same direction" {
            let ray = Line2D(0., 0., 10., 0.)
            let ln = Line2D(2., 5., 8., 5.)
            let proj = Line2D.projectOntoRay ray ln
            "projection from X" |> expectEqualEpsilon proj.FromX 2.
            "projection to X" |> expectEqualEpsilon proj.ToX 8.
            "projection on ray" |> expectEqualEpsilon proj.FromY 0.
        }

        test "projectOntoRay opposite direction preserves orientation" {
            let ray = Line2D(0., 0., 10., 0.)
            let ln = Line2D(8., 5., 2., 5.)
            let proj = Line2D.projectOntoRay ray ln
            "preserves line direction from" |> expectEqualEpsilon proj.FromX 8.
            "preserves line direction to" |> expectEqualEpsilon proj.ToX 2.
        }

        test "projectOntoRay diagonal" {
            let ray = Line2D(0., 0., 10., 10.)
            let ln = Line2D(0., 5., 5., 10.)
            let proj = Line2D.projectOntoRay ray ln
            "diagonal projection length preserves order" |> Expect.isTrue (proj.Length > 0.)
        }

        test "tryProjectOntoLineParam full overlap" {
            let onToLine = Line2D(0., 0., 10., 0.)
            let ln = Line2D(2., 5., 8., 5.)
            match Line2D.tryProjectOntoLineParam onToLine ln with
            | Some (p1, p2) ->
                "start in range" |> expectEqualEpsilon p1 0.2
                "end in range" |> expectEqualEpsilon p2 0.8
            | None -> failwith "expected Some"
        }

        test "tryProjectOntoLineParam partial overlap at start" {
            let onToLine = Line2D(5., 0., 10., 0.)
            let ln = Line2D(0., 5., 7., 5.)
            match Line2D.tryProjectOntoLineParam onToLine ln with
            | Some (p1, p2) ->
                "clamped start" |> expectEqualEpsilon p1 0.
                "end parameter" |> expectEqualEpsilon p2 0.4
            | None -> failwith "expected Some"
        }

        test "tryProjectOntoLineParam partial overlap at end" {
            let onToLine = Line2D(0., 0., 10., 0.)
            let ln = Line2D(8., 5., 15., 5.)
            match Line2D.tryProjectOntoLineParam onToLine ln with
            | Some (p1, p2) ->
                "start parameter" |> expectEqualEpsilon p1 0.8
                "clamped end" |> expectEqualEpsilon p2 1.0
            | None -> failwith "expected Some"
        }

        test "tryProjectOntoLineParam no overlap before line" {
            let onToLine = Line2D(10., 0., 20., 0.)
            let ln = Line2D(0., 5., 5., 5.)
            match Line2D.tryProjectOntoLineParam onToLine ln with
            | Some _ -> failwith "expected None"
            | None -> Expect.isTrue true "no overlap returns None"
        }

        test "tryProjectOntoLineParam no overlap after line" {
            let onToLine = Line2D(0., 0., 10., 0.)
            let ln = Line2D(15., 5., 20., 5.)
            match Line2D.tryProjectOntoLineParam onToLine ln with
            | Some _ -> failwith "expected None"
            | None -> Expect.isTrue true "no overlap returns None"
        }

        test "tryProjectOntoLineParam opposite direction overlap" {
            let onToLine = Line2D(0., 0., 10., 0.)
            let ln = Line2D(8., 5., 2., 5.)
            match Line2D.tryProjectOntoLineParam onToLine ln with
            | Some (p1, p2) ->
                "from param" |> expectEqualEpsilon p1 0.8
                "to param" |> expectEqualEpsilon p2 0.2
            | None -> failwith "expected Some"
        }

        test "tryProjectOntoLineParam encompassing line" {
            let onToLine = Line2D(5., 0., 7., 0.)
            let ln = Line2D(0., 5., 20., 5.)
            match Line2D.tryProjectOntoLineParam onToLine ln with
            | Some (p1, p2) ->
                "clamped to 0" |> expectEqualEpsilon p1 0.
                "clamped to 1" |> expectEqualEpsilon p2 1.
            | None -> failwith "expected Some"
        }

        test "tryProjectOntoLineParam zero length line to project" {
            let onToLine = Line2D(0., 0., 10., 0.)
            let ln = Line2D(5., 5., 5., 5.)
            match Line2D.tryProjectOntoLineParam onToLine ln with
            | Some (p1, p2) ->
                "zero length projects to point" |> expectEqualEpsilon p1 p2
            | None -> failwith "expected Some for zero length"
        }

        test "tryProjectOntoLineParam too short target line throws" {
            let onToLine = Line2D(0., 0., 1e-13, 0.)
            let ln = Line2D(2., 5., 8., 5.)
            "too short throws" |> Expect.throws (fun () -> Line2D.tryProjectOntoLineParam onToLine ln |> ignore)
        }

        test "tryProjectOntoLine full overlap" {
            let onToLine = Line2D(0., 0., 10., 0.)
            let ln = Line2D(2., 5., 8., 5.)
            match Line2D.tryProjectOntoLine onToLine ln with
            | Some proj ->
                "projection from" |> expectEqualEpsilon proj.FromX 2.
                "projection to" |> expectEqualEpsilon proj.ToX 8.
            | None -> failwith "expected Some"
        }

        test "tryProjectOntoLine partial overlap preserves orientation" {
            let onToLine = Line2D(0., 0., 10., 0.)
            let ln = Line2D(8., 5., 15., 5.)
            match Line2D.tryProjectOntoLine onToLine ln with
            | Some proj ->
                "from X" |> expectEqualEpsilon proj.FromX 8.
                "to X at boundary" |> expectEqualEpsilon proj.ToX 10.
            | None -> failwith "expected Some"
        }

        test "tryProjectOntoLine no overlap" {
            let onToLine = Line2D(0., 0., 10., 0.)
            let ln = Line2D(15., 5., 20., 5.)
            match Line2D.tryProjectOntoLine onToLine ln with
            | Some _ -> failwith "expected None"
            | None -> Expect.isTrue true "no overlap"
        }

        test "tryProjectOntoLine diagonal" {
            let onToLine = Line2D(0., 0., 10., 10.)
            let ln = Line2D(0., 5., 5., 10.)
            match Line2D.tryProjectOntoLine onToLine ln with
            | Some proj ->
                "projection exists" |> Expect.isTrue (proj.Length > 0.)
            | None -> failwith "expected Some for diagonal"
        }

        test "tryProjectOntoLine perpendicular collapse" {
            let onToLine = Line2D(0., 0., 10., 0.)
            let ln = Line2D(5., 2., 5., 8.)
            match Line2D.tryProjectOntoLine onToLine ln with
            | Some proj ->
                "collapses to zero length" |> Expect.isTrue (proj.Length < 1e-9)
                "at parameter 0.5" |> expectEqualEpsilon proj.FromX 5.
            | None -> failwith "expected Some"
        }

        test "tryProjectOntoLine within tolerance at start" {
            let onToLine = Line2D(0., 0., 10., 0.)
            let ln = Line2D(-0.0000001, 5., 5., 5.)
            match Line2D.tryProjectOntoLine onToLine ln with
            | Some proj ->
                "tolerance allows start" |> Expect.isTrue (abs proj.FromX < 1e-6)
            | None -> failwith "expected Some within tolerance"
        }
    ]


let testsLine2DDivide =
    testList "Line2D Divide/Split Methods" [

        test "divide into 1 segment" {
            let ln = Line2D(0., 0., 10., 0.)
            let pts = Line2D.divide 1 ln
            "returns 2 points" |> Expect.equal pts.Length 2
            "start point" |> Expect.isTrue (eq pts.[0] ln.From)
            "end point" |> Expect.isTrue (eq pts.[1] ln.To)
        }

        test "divide into 5 segments" {
            let ln = Line2D(0., 0., 10., 0.)
            let pts = Line2D.divide 5 ln
            "returns 6 points" |> Expect.equal pts.Length 6
            "point 0" |> Expect.isTrue (eq pts.[0] (Pt(0., 0.)))
            "point 1" |> Expect.isTrue (eq pts.[1] (Pt(2., 0.)))
            "point 2" |> Expect.isTrue (eq pts.[2] (Pt(4., 0.)))
            "point 3" |> Expect.isTrue (eq pts.[3] (Pt(6., 0.)))
            "point 4" |> Expect.isTrue (eq pts.[4] (Pt(8., 0.)))
            "point 5" |> Expect.isTrue (eq pts.[5] (Pt(10., 0.)))
        }

        test "divide diagonal line" {
            let ln = Line2D(0., 0., 10., 10.)
            let pts = Line2D.divide 4 ln
            "returns 5 points" |> Expect.equal pts.Length 5
            "point 0" |> Expect.isTrue (eq pts.[0] (Pt(0., 0.)))
            "point 1" |> Expect.isTrue (eq pts.[1] (Pt(2.5, 2.5)))
            "point 2" |> Expect.isTrue (eq pts.[2] (Pt(5., 5.)))
            "point 3" |> Expect.isTrue (eq pts.[3] (Pt(7.5, 7.5)))
            "point 4" |> Expect.isTrue (eq pts.[4] (Pt(10., 10.)))
        }

        test "divide with 0 segments throws" {
            let ln = Line2D(0., 0., 10., 0.)
            "zero segments throws" |> Expect.throws (fun () -> Line2D.divide 0 ln |> ignore)
        }

        test "divide with negative segments throws" {
            let ln = Line2D(0., 0., 10., 0.)
            "negative segments throws" |> Expect.throws (fun () -> Line2D.divide -1 ln |> ignore)
        }

        test "divideMinLength exactly fitting" {
            let ln = Line2D(0., 0., 10., 0.)
            let pts = Line2D.divideMinLength 2. ln
            // minLength*1.000001 factor means int(10/(2*1.000001)) = int(4.999995) = 4 segments
            "returns 5 points (4 segments)" |> Expect.equal pts.Length 5
            "point 0" |> Expect.isTrue (eq pts.[0] (Pt(0., 0.)))
            "point 1" |> Expect.isTrue (eq pts.[1] (Pt(2.5, 0.)))
            "point 2" |> Expect.isTrue (eq pts.[2] (Pt(5., 0.)))
            "point 3" |> Expect.isTrue (eq pts.[3] (Pt(7.5, 0.)))
            "point 4" |> Expect.isTrue (eq pts.[4] (Pt(10., 0.)))
        }

        test "divideMinLength with remainder" {
            let ln = Line2D(0., 0., 11., 0.)
            let pts = Line2D.divideMinLength 2. ln
            // int(11/(2*1.000001)) = int(5.499997) = 5 segments
            "returns 6 points (5 segments)" |> Expect.equal pts.Length 6
            "point 0" |> Expect.isTrue (eq pts.[0] (Pt(0., 0.)))
            "point 1" |> Expect.isTrue (eq pts.[1] (Pt(2.2, 0.)))
            "point 2" |> Expect.isTrue (eq pts.[2] (Pt(4.4, 0.)))
            "point 3" |> Expect.isTrue (eq pts.[3] (Pt(6.6, 0.)))
            "point 4" |> Expect.isTrue (eq pts.[4] (Pt(8.8, 0.)))
            "point 5" |> Expect.isTrue (eq pts.[5] (Pt(11., 0.)))
        }

        test "divideMinLength too large throws" {
            let ln = Line2D(0., 0., 5., 0.)
            "throws when minLength > line length" |> Expect.throws (fun () -> Line2D.divideMinLength 10. ln |> ignore)
        }

        test "divideMinLength very small creates many segments" {
            let ln = Line2D(0., 0., 1., 0.)
            let pts = Line2D.divideMinLength 0.1 ln
            // int(1/(0.1*1.000001)) = int(9.99999) = 9 segments
            "returns 10 points (9 segments)" |> Expect.equal pts.Length 10
            "point 0" |> Expect.isTrue (eq pts.[0] (Pt(0., 0.)))
            "point 5" |> expectEqualEpsilon pts.[5].X (5./9.)
            "point 9" |> Expect.isTrue (eq pts.[9] (Pt(1., 0.)))
        }

        test "divideMinLength zero or negative throws" {
            let ln = Line2D(0., 0., 10., 0.)
            "zero throws" |> Expect.throws (fun () -> Line2D.divideMinLength 0. ln |> ignore)
            "negative throws" |> Expect.throws (fun () -> Line2D.divideMinLength -1. ln |> ignore)
        }

        test "divideMaxLength exactly fitting" {
            let ln = Line2D(0., 0., 10., 0.)
            let pts = Line2D.divideMaxLength 2. ln
            // int(10/2*0.999999)+1 = int(4.999995)+1 = 5 segments
            "returns 6 points (5 segments)" |> Expect.equal pts.Length 6
            "point 0" |> Expect.isTrue (eq pts.[0] (Pt(0., 0.)))
            "point 1" |> Expect.isTrue (eq pts.[1] (Pt(2., 0.)))
            "point 2" |> Expect.isTrue (eq pts.[2] (Pt(4., 0.)))
            "point 3" |> Expect.isTrue (eq pts.[3] (Pt(6., 0.)))
            "point 4" |> Expect.isTrue (eq pts.[4] (Pt(8., 0.)))
            "point 5" |> Expect.isTrue (eq pts.[5] (Pt(10., 0.)))
        }

        test "divideMaxLength with small remainder" {
            let ln = Line2D(0., 0., 10.1, 0.)
            let pts = Line2D.divideMaxLength 2. ln
            // int(10.1/2*0.999999)+1 = int(5.049995)+1 = 6 segments
            "returns 7 points (6 segments)" |> Expect.equal pts.Length 7
            "point 0" |> Expect.isTrue (eq pts.[0] (Pt(0., 0.)))
            "point 6" |> Expect.isTrue (eq pts.[6] (Pt(10.1, 0.)))
            // each segment is 10.1/6 = 1.683333...
            "segment length" |> expectEqualEpsilon (10.1/6.) (Pt.distance pts.[0] pts.[1])
        }

        test "divideMaxLength very large returns endpoints" {
            let ln = Line2D(0., 0., 10., 0.)
            let pts = Line2D.divideMaxLength 100. ln
            // int(10/100*0.999999)+1 = int(0.0999999)+1 = 1 segment
            "returns 2 points (1 segment)" |> Expect.equal pts.Length 2
            "point 0" |> Expect.isTrue (eq pts.[0] (Pt(0., 0.)))
            "point 1" |> Expect.isTrue (eq pts.[1] (Pt(10., 0.)))
        }

        test "divideMaxLength zero or negative throws" {
            // NOTE: This test only passes in .NET, not in Fable/JS
            // In JavaScript, int(Infinity) doesn't throw, it behaves differently
            let ln = Line2D(0., 0., 10., 0.)
            "zero maxLength throws" |> Expect.throws (fun () -> Line2D.divideMaxLength 0. ln |> ignore)
        }

        test "split with gap into 2 segments" {
            let ln = Line2D(0., 0., 10., 0.)
            let lns = Line2D.split 2. 2 ln
            // lenMinusGaps = 10 - 2*(2-1) = 8, segLen = 8/2 = 4
            "returns 2 lines" |> Expect.equal lns.Length 2
            "first line from" |> Expect.isTrue (eq lns.[0].From (Pt(0., 0.)))
            "first line to" |> Expect.isTrue (eq lns.[0].To (Pt(4., 0.)))
            "first line length" |> expectEqualEpsilon lns.[0].Length 4.
            "second line from" |> Expect.isTrue (eq lns.[1].From (Pt(6., 0.)))
            "second line to" |> Expect.isTrue (eq lns.[1].To (Pt(10., 0.)))
            "second line length" |> expectEqualEpsilon lns.[1].Length 4.
        }

        test "split with gap into 3 segments" {
            let ln = Line2D(0., 0., 15., 0.)
            let lns = Line2D.split 1. 3 ln
            // lenMinusGaps = 15 - 1*(3-1) = 13, segLen = 13/3 = 4.333...
            "returns 3 lines" |> Expect.equal lns.Length 3
            "first line from" |> Expect.isTrue (eq lns.[0].From (Pt(0., 0.)))
            "first line to" |> expectEqualEpsilon lns.[0].To.X (13./3.)
            "first line length" |> expectEqualEpsilon lns.[0].Length (13./3.)
            "second line from" |> expectEqualEpsilon lns.[1].From.X (13./3.+1.)
            "second line to" |> expectEqualEpsilon lns.[1].To.X (26./3.+1.)
            "second line length" |> expectEqualEpsilon lns.[1].Length (13./3.)
            "third line from" |> expectEqualEpsilon lns.[2].From.X (26./3.+2.)
            "third line to" |> Expect.isTrue (eq lns.[2].To (Pt(15., 0.)))
            "third line length" |> expectEqualEpsilon lns.[2].Length (13./3.)
        }

        test "split with zero gap same as divide" {
            let ln = Line2D(0., 0., 10., 0.)
            let lns = Line2D.split 0. 5 ln
            // lenMinusGaps = 10 - 0*(5-1) = 10, segLen = 10/5 = 2
            "returns 5 lines" |> Expect.equal lns.Length 5
            "line 0 from" |> Expect.isTrue (eq lns.[0].From (Pt(0., 0.)))
            "line 0 to" |> Expect.isTrue (eq lns.[0].To (Pt(2., 0.)))
            "line 1 from" |> Expect.isTrue (eq lns.[1].From (Pt(2., 0.)))
            "line 1 to" |> Expect.isTrue (eq lns.[1].To (Pt(4., 0.)))
            "line 2 from" |> Expect.isTrue (eq lns.[2].From (Pt(4., 0.)))
            "line 2 to" |> Expect.isTrue (eq lns.[2].To (Pt(6., 0.)))
            "line 3 from" |> Expect.isTrue (eq lns.[3].From (Pt(6., 0.)))
            "line 3 to" |> Expect.isTrue (eq lns.[3].To (Pt(8., 0.)))
            "line 4 from" |> Expect.isTrue (eq lns.[4].From (Pt(8., 0.)))
            "line 4 to" |> Expect.isTrue (eq lns.[4].To (Pt(10., 0.)))
        }

        test "split with large gap returns empty" {
            let ln = Line2D(0., 0., 10., 0.)
            let lns = Line2D.split 20. 2 ln
            "returns empty array" |> Expect.equal lns.Length 0
        }

        test "split with 0 segments throws" {
            let ln = Line2D(0., 0., 10., 0.)
            "zero segments throws" |> Expect.throws (fun () -> Line2D.split 1. 0 ln |> ignore)
        }

        test "split with negative segments throws" {
            let ln = Line2D(0., 0., 10., 0.)
            "negative segments throws" |> Expect.throws (fun () -> Line2D.split 1. -1 ln |> ignore)
        }

        test "splitMinLength with gap" {
            let ln = Line2D(0., 0., 20., 0.)
            let lns = Line2D.splitMinLength 1. 2. ln
            "returns multiple lines" |> Expect.isTrue (lns.Length > 1)
            "each segment >= minLength" |> Expect.isTrue (lns |> Array.forall (fun l -> l.Length >= 1.9))
        }

        test "splitMinLength line too short throws" {
            let ln = Line2D(0., 0., 1., 0.)
            "throws when line too short" |> Expect.throws (fun () -> Line2D.splitMinLength 0.5 5. ln |> ignore)
        }

        test "splitMaxLength with gap" {
            let ln = Line2D(0., 0., 20., 0.)
            let lns = Line2D.splitMaxLength 1. 3. ln
            "returns multiple lines" |> Expect.isTrue (lns.Length > 1)
            "each segment <= maxLength" |> Expect.isTrue (lns |> Array.forall (fun l -> l.Length <= 3.1))
        }

        test "divideEvery basic" {
            let ln = Line2D(0., 0., 10., 0.)
            let pts = Line2D.divideEvery 2. ln
            // div = 10/2 = 5.0, floor = 5, no remainder, so 5 steps + start = 6 points
            "includes start and end" |> Expect.equal pts.Count 6
            "point 0" |> Expect.isTrue (eq pts.[0] (Pt(0., 0.)))
            "point 1" |> Expect.isTrue (eq pts.[1] (Pt(2., 0.)))
            "point 2" |> Expect.isTrue (eq pts.[2] (Pt(4., 0.)))
            "point 3" |> Expect.isTrue (eq pts.[3] (Pt(6., 0.)))
            "point 4" |> Expect.isTrue (eq pts.[4] (Pt(8., 0.)))
            "point 5" |> Expect.isTrue (eq pts.[5] (Pt(10., 0.)))
        }

        test "divideEvery exact fit includes end" {
            let ln = Line2D(0., 0., 5., 0.)
            let pts = Line2D.divideEvery 1. ln
            // div = 5/1 = 5.0, floor = 5, no remainder, so 5 steps + start = 6 points
            "includes end point" |> Expect.equal pts.Count 6
            "point 0" |> Expect.isTrue (eq pts.[0] (Pt(0., 0.)))
            "point 5" |> Expect.isTrue (eq pts.[5] (Pt(5., 0.)))
        }

        test "divideEvery with tiny remainder" {
            let ln = Line2D(0., 0., 5.001, 0.)
            let pts = Line2D.divideEvery 1. ln
            // div = 5.001/1 = 5.001, floor = 5, remainder 0.001 > 0.1% so end included
            "includes end due to remainder > 0.1%" |> Expect.equal pts.Count 7
            "point 0" |> Expect.isTrue (eq pts.[0] (Pt(0., 0.)))
            "point 6" |> Expect.isTrue (eq pts.[6] (Pt(5.001, 0.)))
        }

        test "divideEvery with very tiny remainder includes end" {
            let ln = Line2D(0., 0., 5.0001, 0.)
            let pts = Line2D.divideEvery 1. ln
            // div = 5.0001/1 = 5.0001, floor = 5, remainder 0.0001 > 0.1% so end NOT included
            "0.0001 / 5.0001 = 0.002% < 0.1% threshold so end not included" |> Expect.equal pts.Count 6
            "point 0" |> Expect.isTrue (eq pts.[0] (Pt(0., 0.)))
            "point 5 at line end" |> Expect.isTrue (eq pts.[5] (Pt(5.0001, 0.)))
        }

        test "divideInsideEvery excludes endpoints" {
            let ln = Line2D(0., 0., 10., 0.)
            let pts = Line2D.divideInsideEvery 2. ln
            // div = 10/2 = 5.0, floor = 5, count = 5, but excludes first (start) point
            // step = 1/5 = 0.2, so points at t=0.2, 0.4, 0.6, 0.8, 1.0
            // excludes start (t=0), includes t=1.0 if remainder > 0.1%
            "returns 4 interior points" |> Expect.equal pts.Count 4
            "point 0 at 20%" |> Expect.isTrue (eq pts.[0] (Pt(2., 0.)))
            "point 1 at 40%" |> Expect.isTrue (eq pts.[1] (Pt(4., 0.)))
            "point 2 at 60%" |> Expect.isTrue (eq pts.[2] (Pt(6., 0.)))
            "point 3 at 80%" |> Expect.isTrue (eq pts.[3] (Pt(8., 0.)))
        }

        test "divideInsideEvery with 2 segments" {
            let ln = Line2D(0., 0., 10., 0.)
            let pts = Line2D.divideInsideEvery 5. ln
            // div = 10/5 = 2.0, floor = 2, count = 2, step = 1/2 = 0.5
            // points at t=0.5, 1.0, but excludes start and checks 0.1% remainder rule
            "returns 1 interior point" |> Expect.equal pts.Count 1
            "point 0 at 50%" |> Expect.isTrue (eq pts.[0] (Pt(5., 0.)))
        }

        test "divideInsideEvery very small distance" {
            let ln = Line2D(0., 0., 1., 0.)
            let pts = Line2D.divideInsideEvery 0.1 ln
            // div = 1/0.1 = 10.0, floor = 10, count = 10, step = 1/10 = 0.1
            // points at t=0.1, 0.2, 0.3, ..., 0.9, 1.0, excludes start
            "returns 9 interior points" |> Expect.equal pts.Count 9
            "point 0 at 10%" |> Expect.isTrue (eq pts.[0] (Pt(0.1, 0.)))
            "point 4 at 50%" |> Expect.isTrue (eq pts.[4] (Pt(0.5, 0.)))
            "point 8 at 90%" |> Expect.isTrue (eq pts.[8] (Pt(0.9, 0.)))
        }

        test "divideEvery distance larger than line" {
            let ln = Line2D(0., 0., 10., 0.)
            let ps = Line2D.divideEvery 11. ln
            // div = 10/11 = 0.909, floor = 0, so just returns start and end
            "returns 2 points" |> Expect.equal ps.Count 2
            "first point is start"  |> Expect.isTrue(Pt.equals 1e-12 ps[0] ln.From )
            "second point is end" |> Expect.isTrue(Pt.equals 1e-12 ps[1] ln.To )

        }
    ]


let testsLine2DOffset =
    testList "Line2D Offset Methods" [

        test "offset positive amount" {
            let ln = Line2D(0., 0., 10., 0.)
            let off = Line2D.offset 2. ln
            "offset to left (positive Y)" |> expectEqualEpsilon off.FromY 2.
            "end also offset" |> expectEqualEpsilon off.ToY 2.
        }

        test "offset negative amount" {
            let ln = Line2D(0., 0., 10., 0.)
            let off = Line2D.offset -3. ln
            "offset to right (negative Y)" |> expectEqualEpsilon off.FromY -3.
        }

        test "offset zero amount returns same line" {
            let ln = Line2D(0., 0., 10., 5.)
            let off = Line2D.offset 0. ln
            "from X unchanged" |> expectEqualEpsilon off.FromX ln.FromX
            "from Y unchanged" |> expectEqualEpsilon off.FromY ln.FromY
            "to X unchanged" |> expectEqualEpsilon off.ToX ln.ToX
            "to Y unchanged" |> expectEqualEpsilon off.ToY ln.ToY
        }

        test "offset diagonal line" {
            let ln = Line2D(0., 0., 10., 10.)
            let off = Line2D.offset 1. ln
            "diagonal offset perpendicular" |> Expect.isTrue (off.Length > 0.)
            let perp = (off.From - ln.From).Unitized
            let tang = ln.Tangent.Unitized
            "perpendicular to original" |> Expect.isTrue (abs(perp *** tang) < 1e-9)
        }

        test "offset vertical line" {
            let ln = Line2D(5., 0., 5., 10.)
            let off = Line2D.offset 2. ln
            "offset horizontally for vertical line" |> expectEqualEpsilon off.FromX 3.
            "Y coordinates unchanged" |> expectEqualEpsilon off.FromY 0.
        }

        test "offset too short line throws" {
            let ln = Line2D(0., 0., 1e-13, 0.)
            "too short throws" |> Expect.throws (fun () -> Line2D.offset 1. ln |> ignore)
        }
    ]


let testsLine2DWithLength =
    testList "Line2D WithLength Methods" [

        test "withLengthFromStart shorter" {
            let ln = Line2D(0., 0., 10., 0.)
            let result = Line2D.withLengthFromStart 5. ln
            "starts at same point" |> expectEqualEpsilon result.FromX 0.
            "new length is 5" |> expectEqualEpsilon result.Length 5.
            "ends at 5" |> expectEqualEpsilon result.ToX 5.
        }

        test "withLengthFromStart longer" {
            let ln = Line2D(0., 0., 10., 0.)
            let result = Line2D.withLengthFromStart 15. ln
            "starts at same point" |> expectEqualEpsilon result.FromX 0.
            "new length is 15" |> expectEqualEpsilon result.Length 15.
            "ends at 15" |> expectEqualEpsilon result.ToX 15.
        }

        test "withLengthFromStart diagonal" {
            let ln = Line2D(0., 0., 3., 4.)
            let result = Line2D.withLengthFromStart 10. ln
            "length is 10" |> expectEqualEpsilon result.Length 10.
            "starts at origin" |> Expect.isTrue (eq result.From ln.From)
        }

        test "withLengthFromStart too short line throws" {
            let ln = Line2D(0., 0., 1e-13, 0.)
            "too short throws" |> Expect.throws (fun () -> Line2D.withLengthFromStart 5. ln |> ignore)
        }

        test "withLengthToEnd shorter" {
            let ln = Line2D(0., 0., 10., 0.)
            let result = Line2D.withLengthToEnd 5. ln
            "ends at same point" |> expectEqualEpsilon result.ToX 10.
            "new length is 5" |> expectEqualEpsilon result.Length 5.
            "starts at 5" |> expectEqualEpsilon result.FromX 5.
        }

        test "withLengthToEnd longer" {
            let ln = Line2D(0., 0., 10., 0.)
            let result = Line2D.withLengthToEnd 15. ln
            "ends at same point" |> expectEqualEpsilon result.ToX 10.
            "new length is 15" |> expectEqualEpsilon result.Length 15.
            "starts at -5" |> expectEqualEpsilon result.FromX -5.
        }

        test "withLengthToEnd too short line throws" {
            let ln = Line2D(0., 0., 1e-13, 0.)
            "too short throws" |> Expect.throws (fun () -> Line2D.withLengthToEnd 5. ln |> ignore)
        }

        test "withLengthFromMid shorter" {
            let ln = Line2D(0., 0., 10., 0.)
            let result = Line2D.withLengthFromMid 4. ln
            "new length is 4" |> expectEqualEpsilon result.Length 4.
            "from at 3" |> expectEqualEpsilon result.FromX 3.
            "to at 7" |> expectEqualEpsilon result.ToX 7.
        }

        test "withLengthFromMid longer" {
            let ln = Line2D(0., 0., 10., 0.)
            let result = Line2D.withLengthFromMid 20. ln
            "new length is 20" |> expectEqualEpsilon result.Length 20.
            "from at -5" |> expectEqualEpsilon result.FromX -5.
            "to at 15" |> expectEqualEpsilon result.ToX 15.
        }

        test "withLengthFromMid preserves midpoint" {
            let ln = Line2D(0., 0., 10., 0.)
            let result = Line2D.withLengthFromMid 6. ln
            "midpoint unchanged" |> expectEqualEpsilon result.Mid.X 5.
        }

        test "withLengthFromMid too short line throws" {
            let ln = Line2D(0., 0., 1e-13, 0.)
            "too short throws" |> Expect.throws (fun () -> Line2D.withLengthFromMid 5. ln |> ignore)
        }

        test "pointAtDistance positive" {
            let ln = Line2D(0., 0., 10., 0.)
            let pt = Line2D.pointAtDistance 3. ln
            "point at distance 3" |> expectEqualEpsilon pt.X 3.
        }

        test "pointAtDistance negative" {
            let ln = Line2D(0., 0., 10., 0.)
            let pt = Line2D.pointAtDistance -2. ln
            "negative goes before start" |> expectEqualEpsilon pt.X -2.
        }

        test "pointAtDistance beyond end" {
            let ln = Line2D(0., 0., 10., 0.)
            let pt = Line2D.pointAtDistance 15. ln
            "extends beyond end" |> expectEqualEpsilon pt.X 15.
        }

        test "pointAtDistance diagonal" {
            let ln = Line2D(0., 0., 3., 4.)
            let pt = Line2D.pointAtDistance 5. ln
            "at line end for 3-4-5 triangle" |> Expect.isTrue (eq pt ln.To)
        }

        test "pointAtDistance too short line throws" {
            let ln = Line2D(0., 0., 1e-13, 0.)
            "too short throws" |> Expect.throws (fun () -> Line2D.pointAtDistance 1. ln |> ignore)
        }
    ]


let testsLine2DIntersection =
    testList "Line2D Intersection Methods" [

        test "doIntersect crossing lines" {
            let a = Line2D(0., 5., 10., 5.)
            let b = Line2D(5., 0., 5., 10.)
            "crossing lines intersect" |> Expect.isTrue (Line2D.doIntersect a b)
        }

        test "doIntersect parallel lines" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 5., 10., 5.)
            "parallel lines don't intersect" |> Expect.isFalse (Line2D.doIntersect a b)
        }

        test "doIntersect non-intersecting within segments" {
            let a = Line2D(0., 0., 5., 0.)
            let b = Line2D(10., -5., 10., 5.)
            "segments don't reach intersection" |> Expect.isFalse (Line2D.doIntersect a b)
        }

        test "doIntersect touching endpoints" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(10., 0., 10., 10.)
            "touching at endpoint" |> Expect.isTrue (Line2D.doIntersect a b)
        }

        test "doIntersectOrOverlap crossing" {
            let a = Line2D(0., 5., 10., 5.)
            let b = Line2D(5., 0., 5., 10.)
            "crossing lines" |> Expect.isTrue (Line2D.doIntersectOrOverlap a b)
        }

        test "doIntersectOrOverlap overlapping collinear" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(5., 0., 15., 0.)
            "overlapping returns true" |> Expect.isTrue (Line2D.doIntersectOrOverlap a b)
        }

        test "doIntersectOrOverlap parallel non-overlapping" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 5., 10., 5.)
            "parallel don't overlap" |> Expect.isFalse (Line2D.doIntersectOrOverlap a b)
        }

        test "tryIntersect returns intersection point" {
            let a = Line2D(0., 5., 10., 5.)
            let b = Line2D(5., 0., 5., 10.)
            match Line2D.tryIntersect a b with
            | Some pt ->
                "intersection at (5,5)" |> Expect.isTrue (eq pt (Pt(5., 5.)))
            | None -> failwith "expected Some"
        }

        test "tryIntersect no intersection returns None" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 5., 10., 5.)
            match Line2D.tryIntersect a b with
            | Some _ -> failwith "expected None"
            | None -> Expect.isTrue true "parallel returns None"
        }

        test "tryIntersect diagonal intersection" {
            let a = Line2D(0., 0., 10., 10.)
            let b = Line2D(0., 10., 10., 0.)
            match Line2D.tryIntersect a b with
            | Some pt ->
                "intersection at center" |> Expect.isTrue (eq pt (Pt(5., 5.)))
            | None -> failwith "expected Some"
        }

        test "tryIntersectRay returns ray intersection" {
            let a = Line2D(0., 5., 5., 5.)
            let b = Line2D(10., 0., 10., 10.)
            match Line2D.tryIntersectRay a b with
            | Some pt ->
                "ray extends to intersection" |> expectEqualEpsilon pt.X 10.
            | None -> failwith "expected Some for ray intersection"
        }

        test "tryIntersectRay parallel returns None" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 5., 10., 5.)
            match Line2D.tryIntersectRay a b with
            | Some _ -> failwith "expected None"
            | None -> Expect.isTrue true "parallel rays return None"
        }

        test "tryIntersectOrOverlap intersection" {
            let a = Line2D(0., 5., 10., 5.)
            let b = Line2D(5., 0., 5., 10.)
            match Line2D.tryIntersectOrOverlap a b with
            | Some pt -> Expect.isTrue (eq pt (Pt(5., 5.))) "intersection point"
            | None -> failwith "expected Some"
        }

        test "tryIntersectOrOverlap overlapping returns touching point" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(10., 0., 20., 0.)
            match Line2D.tryIntersectOrOverlap a b with
            | Some pt -> Expect.isTrue (eq pt (Pt(10., 0.))) "touching point"
            | None -> failwith "expected Some for touching"
        }

        test "isTouchingEndsOf not touching" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 5., 10., 5.)
            let result = XLine2D.getEndsTouching(a,b)
            match result with
            | XLine2D.XEnds.NotTouching -> Expect.isTrue true "not touching"
            | _ -> failwith "expected NotTouching"
        }

        test "isTouchingEndsOf StartA_StartB" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 0., 0., 10.)
            let result = XLine2D.getEndsTouching(a,b)
            match result with
            | XLine2D.XEnds.StartA_StartB -> Expect.isTrue true "StartA_StartB"
            | _ -> failwith "expected StartA_StartB"
        }

        test "isTouchingEndsOf EndA_EndB" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 10., 10., 0.)
            let result = XLine2D.getEndsTouching(a,b)
            match result with
            | XLine2D.XEnds.EndA_EndB -> Expect.isTrue true "EndA_EndB"
            | _ -> failwith "expected EndA_EndB"
        }

        test "isTouchingEndsOf identical" {
            let a = Line2D(0., 0., 10., 0.)
            let b = Line2D(0., 0., 10., 0.)
            let result = XLine2D.getEndsTouching(a,b)
            match result with
            | XLine2D.XEnds.Identical -> Expect.isTrue true "identical"
            | _ -> failwith "expected Identical"
        }
    ]


let testsLine3DBasics =
    testList "Line3D Basics" [

        test "IsZeroLength true" {
            let ln = Line3D(5., 3., 7., 5., 3., 7.)
            "zero length line" |> Expect.isTrue ln.IsZeroLength
        }

        test "IsZeroLength false" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            "non-zero length line" |> Expect.isFalse ln.IsZeroLength
        }

        test "IsXAligned" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            "X aligned line" |> Expect.isTrue ln.IsXAligned
        }

        test "IsYAligned" {
            let ln = Line3D(0., 0., 0., 0., 10., 0.)
            "Y aligned line" |> Expect.isTrue ln.IsYAligned
        }

        test "IsZAligned" {
            let ln = Line3D(0., 0., 0., 0., 0., 10.)
            "Z aligned line" |> Expect.isTrue ln.IsZAligned
            "IsVertical same as IsZAligned" |> Expect.isTrue ln.IsVertical
        }

        test "IsHorizontal" {
            let ln = Line3D(0., 0., 0., 10., 10., 0.)
            "horizontal line" |> Expect.isTrue ln.IsHorizontal
        }

        test "IsHorizontal vertical line false" {
            let ln = Line3D(0., 0., 0., 0., 0., 10.)
            "vertical line not horizontal" |> Expect.isFalse ln.IsHorizontal
        }

        test "Length calculation" {
            let ln = Line3D(0., 0., 0., 3., 4., 0.)
            "length is 5" |> expectEqualEpsilon ln.Length 5.
        }

        test "LengthSq calculation" {
            let ln = Line3D(0., 0., 0., 3., 4., 0.)
            "length squared is 25" |> expectEqualEpsilon ln.LengthSq 25.
        }

        test "Mid point" {
            let ln = Line3D(0., 0., 0., 10., 10., 10.)
            let mid = ln.Mid
            "midpoint X" |> expectEqualEpsilon mid.X 5.
            "midpoint Y" |> expectEqualEpsilon mid.Y 5.
            "midpoint Z" |> expectEqualEpsilon mid.Z 5.
        }

        test "Reversed" {
            let ln = Line3D(0., 0., 0., 10., 5., 3.)
            let rev = ln.Reversed
            "reversed from equals original to" |> Expect.isTrue (Pnt.distance rev.From ln.To < 1e-9)
            "reversed to equals original from" |> Expect.isTrue (Pnt.distance rev.To ln.From < 1e-9)
        }

        test "EvaluateAt 0.5" {
            let ln = Line3D(0., 0., 0., 10., 10., 10.)
            let pt = ln.EvaluateAt 0.5
            "evaluates to midpoint" |> Expect.isTrue (Pnt.distance pt ln.Mid < 1e-9)
        }

        test "SubLine segment" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let sub = ln.SubLine(0.2, 0.8)
            "subline start" |> expectEqualEpsilon sub.FromX 2.
            "subline end" |> expectEqualEpsilon sub.ToX 8.
        }
    ]


let testsLine3DExtendShrink =
    testList "Line3D Extend/Shrink" [

        test "Extend both ends" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let ext = ln.Extend(5., 5.)
            "extended start" |> expectEqualEpsilon ext.FromX -5.
            "extended end" |> expectEqualEpsilon ext.ToX 15.
        }

        test "ExtendStart" {
            let ln = Line3D(5., 0., 0., 10., 0., 0.)
            let ext = ln.ExtendStart 5.
            "extended start" |> expectEqualEpsilon ext.FromX 0.
            "end unchanged" |> expectEqualEpsilon ext.ToX 10.
        }

        test "ExtendEnd" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let ext = ln.ExtendEnd 5.
            "start unchanged" |> expectEqualEpsilon ext.FromX 0.
            "extended end" |> expectEqualEpsilon ext.ToX 15.
        }

        test "ExtendRel both ends" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let ext = ln.ExtendRel(0.5, 0.5)
            "extended start by half" |> expectEqualEpsilon ext.FromX -5.
            "extended end by half" |> expectEqualEpsilon ext.ToX 15.
        }

        test "Shrink both ends" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let shr = ln.Shrink(2., 3.)
            "shrunk start" |> expectEqualEpsilon shr.FromX 2.
            "shrunk end" |> expectEqualEpsilon shr.ToX 7.
        }

        test "ShrinkStart" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let shr = ln.ShrinkStart 2.
            "shrunk start" |> expectEqualEpsilon shr.FromX 2.
            "end unchanged" |> expectEqualEpsilon shr.ToX 10.
        }

        test "ShrinkEnd" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let shr = ln.ShrinkEnd 3.
            "start unchanged" |> expectEqualEpsilon shr.FromX 0.
            "shrunk end" |> expectEqualEpsilon shr.ToX 7.
        }
    ]


let testsLine3DMove =
    testList "Line3D Move/Transform" [

        test "Move by vector" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let moved = ln.Move(Vec(5., 3., 2.))
            "from X moved" |> expectEqualEpsilon moved.FromX 5.
            "from Y moved" |> expectEqualEpsilon moved.FromY 3.
            "from Z moved" |> expectEqualEpsilon moved.FromZ 2.
        }

        test "MoveX" {
            let ln = Line3D(0., 0., 0., 10., 5., 3.)
            let moved = ln.MoveX 7.
            "from X moved" |> expectEqualEpsilon moved.FromX 7.
            "from Y unchanged" |> expectEqualEpsilon moved.FromY 0.
            "from Z unchanged" |> expectEqualEpsilon moved.FromZ 0.
        }

        test "MoveY" {
            let ln = Line3D(0., 0., 0., 10., 5., 3.)
            let moved = ln.MoveY 4.
            "from Y moved" |> expectEqualEpsilon moved.FromY 4.
            "from X unchanged" |> expectEqualEpsilon moved.FromX 0.
        }

        test "MoveZ" {
            let ln = Line3D(0., 0., 0., 10., 5., 3.)
            let moved = ln.MoveZ 2.
            "from Z moved" |> expectEqualEpsilon moved.FromZ 2.
            "to Z moved" |> expectEqualEpsilon moved.ToZ 5.
        }

        test "Scale from origin" {
            let ln = Line3D(1., 2., 3., 4., 5., 6.)
            let scaled = ln.Scale 2.
            "from X scaled" |> expectEqualEpsilon scaled.FromX 2.
            "to Z scaled" |> expectEqualEpsilon scaled.ToZ 12.
        }

        test "Rotate with quaternion" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let q = Quaternion.createFromRadians(Vec.Zaxis, System.Math.PI / 2.)
            let rotated = ln.Rotate q
            "rotated to X near zero" |> Expect.isTrue (abs rotated.ToX < 1e-9)
            "rotated to Y near 10" |> expectEqualEpsilon rotated.ToY 10.
        }
    ]


let testsLine3DClosestPoint =
    testList "Line3D Closest Point" [

        test "RayClosestParameter on ray" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let pt = Pnt(5., 5., 0.)
            let param = ln.RayClosestParameter pt
            "closest parameter" |> expectEqualEpsilon param 0.5
        }

        test "ClosestParameter clamped to 0" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let pt = Pnt(-5., 5., 0.)
            let param = ln.ClosestParameter pt
            "clamped to 0" |> expectEqualEpsilon param 0.
        }

        test "ClosestParameter clamped to 1" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let pt = Pnt(15., 5., 0.)
            let param = ln.ClosestParameter pt
            "clamped to 1" |> expectEqualEpsilon param 1.
        }

        test "ClosestPoint on line" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let pt = Pnt(5., 5., 0.)
            let cl = ln.ClosestPoint pt
            "closest point X" |> expectEqualEpsilon cl.X 5.
            "closest point Y" |> expectEqualEpsilon cl.Y 0.
            "closest point Z" |> expectEqualEpsilon cl.Z 0.
        }

        test "SqDistanceFromPoint" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let pt = Pnt(5., 3., 4.)
            let sqDist = ln.SqDistanceFromPoint pt
            "squared distance is 25" |> expectEqualEpsilon sqDist 25.
        }

        test "DistanceToPnt" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let pt = Pnt(5., 3., 4.)
            let dist = ln.DistanceToPnt pt
            "distance is 5" |> expectEqualEpsilon dist 5.
        }
    ]


let testsLine3DProjection =
    testList "Line3D Projection Methods" [

        test "projectOntoRayParam same direction" {
            let rayToProjectOnto = Line3D(0., 0., 0., 10., 0., 0.)
            let lineToProject = Line3D(2., 5., 0., 8., 5., 0.)
            let (s, e) = Line3D.projectOntoRayParam rayToProjectOnto lineToProject
            "start parameter" |> expectEqualEpsilon s 0.2
            "end parameter" |> expectEqualEpsilon e 0.8
        }

        test "projectOntoRayParam opposite direction" {
            let rayToProjectOnto = Line3D(0., 0., 0., 10., 0., 0.)
            let lineToProject = Line3D(8., 5., 0., 2., 5., 0.)
            let (s, e) = Line3D.projectOntoRayParam rayToProjectOnto lineToProject
            "start parameter" |> expectEqualEpsilon s 0.8
            "end parameter" |> expectEqualEpsilon e 0.2
        }

        test "projectOntoRayParam beyond ray start" {
            let rayToProjectOnto = Line3D(0., 0., 0., 10., 0., 0.)
            let lineToProject = Line3D(-5., 5., 0., -2., 5., 0.)
            let (s, e) = Line3D.projectOntoRayParam rayToProjectOnto lineToProject
            "start parameter is negative" |> expectEqualEpsilon s -0.5
            "end parameter is negative" |> expectEqualEpsilon e -0.2
        }

        test "projectOntoRayParam beyond ray end" {
            let rayToProjectOnto = Line3D(0., 0., 0., 10., 0., 0.)
            let lineToProject = Line3D(12., 5., 0., 15., 5., 0.)
            let (s, e) = Line3D.projectOntoRayParam rayToProjectOnto lineToProject
            "start parameter beyond 1" |> expectEqualEpsilon s 1.2
            "end parameter beyond 1" |> expectEqualEpsilon e 1.5
        }

        test "projectOntoRayParam diagonal lines" {
            let rayToProjectOnto = Line3D(0., 0., 0., 10., 10., 10.)
            let lineToProject = Line3D(5., 5., 5., 8., 8., 8.)
            let (s, e) = Line3D.projectOntoRayParam rayToProjectOnto lineToProject
            "start parameter on diagonal" |> expectEqualEpsilon s 0.5
            "end parameter on diagonal" |> expectEqualEpsilon e 0.8
        }

        test "projectOntoRayParam perpendicular projection" {
            let rayToProjectOnto = Line3D(0., 0., 0., 10., 0., 0.)
            let lineToProject = Line3D(5., 0., 0., 5., 10., 0.)
            let (s, e) = Line3D.projectOntoRayParam rayToProjectOnto lineToProject
            "both project to same parameter" |> expectEqualEpsilon s 0.5
            "both project to same parameter" |> expectEqualEpsilon e 0.5
        }

        test "projectOntoRayParam too short ray throws" {
            let rayToProjectOnto = Line3D(0., 0., 0., 1e-13, 0., 0.)
            let lineToProject = Line3D(5., 0., 0., 8., 0., 0.)
            "too short ray throws" |> Expect.throws (fun () -> Line3D.projectOntoRayParam rayToProjectOnto lineToProject |> ignore)
        }

        test "projectOntoRay same direction" {
            let rayToProjectOnto = Line3D(0., 0., 0., 10., 0., 0.)
            let lineToProject = Line3D(2., 5., 0., 8., 5., 0.)
            let projected = Line3D.projectOntoRay rayToProjectOnto lineToProject
            "projected start X" |> expectEqualEpsilon projected.FromX 2.
            "projected end X" |> expectEqualEpsilon projected.ToX 8.
            "projected start Y" |> expectEqualEpsilon projected.FromY 0.
            "projected end Y" |> expectEqualEpsilon projected.ToY 0.
        }

        test "projectOntoRay opposite direction preserves orientation" {
            let rayToProjectOnto = Line3D(0., 0., 0., 10., 0., 0.)
            let lineToProject = Line3D(8., 5., 0., 2., 5., 0.)
            let projected = Line3D.projectOntoRay rayToProjectOnto lineToProject
            "projected start X" |> expectEqualEpsilon projected.FromX 8.
            "projected end X" |> expectEqualEpsilon projected.ToX 2.
        }

        test "projectOntoRay 3D projection" {
            let rayToProjectOnto = Line3D(0., 0., 0., 10., 10., 10.)
            let lineToProject = Line3D(5., 5., 0., 8., 8., 0.)
            let projected = Line3D.projectOntoRay rayToProjectOnto lineToProject
            let (s, e) = Line3D.projectOntoRayParam rayToProjectOnto lineToProject
            let expectedStart = rayToProjectOnto.EvaluateAt s
            let expectedEnd = rayToProjectOnto.EvaluateAt e
            let dist1 = Pnt.distance projected.From expectedStart
            let dist2 = Pnt.distance projected.To expectedEnd
            "projected start near expected" |> Expect.isTrue (dist1 < 1e-9)
            "projected end near expected" |> Expect.isTrue (dist2 < 1e-9)
        }

        test "tryProjectOntoLineParam full overlap" {
            let onToLine = Line3D(0., 0., 0., 10., 0., 0.)
            let lineToProject = Line3D(2., 5., 0., 8., 5., 0.)
            let result = Line3D.tryProjectOntoLineParam onToLine lineToProject
            match result with
            | Some (s, e) ->
                "start parameter" |> expectEqualEpsilon s 0.2
                "end parameter" |> expectEqualEpsilon e 0.8
            | None -> Expect.isTrue false "expected Some but got None"
        }

        test "tryProjectOntoLineParam partial overlap at start" {
            let onToLine = Line3D(0., 0., 0., 10., 0., 0.)
            let lineToProject = Line3D(-2., 5., 0., 5., 5., 0.)
            let result = Line3D.tryProjectOntoLineParam onToLine lineToProject
            match result with
            | Some (s, e) ->
                "start parameter clamped to 0" |> expectEqualEpsilon s 0.
                "end parameter" |> expectEqualEpsilon e 0.5
            | None -> Expect.isTrue false "expected Some but got None"
        }

        test "tryProjectOntoLineParam partial overlap at end" {
            let onToLine = Line3D(0., 0., 0., 10., 0., 0.)
            let lineToProject = Line3D(5., 5., 0., 15., 5., 0.)
            let result = Line3D.tryProjectOntoLineParam onToLine lineToProject
            match result with
            | Some (s, e) ->
                "start parameter" |> expectEqualEpsilon s 0.5
                "end parameter clamped to 1" |> expectEqualEpsilon e 1.0
            | None -> Expect.isTrue false "expected Some but got None"
        }

        test "tryProjectOntoLineParam no overlap before line" {
            let onToLine = Line3D(0., 0., 0., 10., 0., 0.)
            let lineToProject = Line3D(-10., 5., 0., -5., 5., 0.)
            let result = Line3D.tryProjectOntoLineParam onToLine lineToProject
            "no overlap returns None" |> Expect.isTrue result.IsNone
        }

        test "tryProjectOntoLineParam no overlap after line" {
            let onToLine = Line3D(0., 0., 0., 10., 0., 0.)
            let lineToProject = Line3D(15., 5., 0., 20., 5., 0.)
            let result = Line3D.tryProjectOntoLineParam onToLine lineToProject
            "no overlap returns None" |> Expect.isTrue result.IsNone
        }

        test "tryProjectOntoLineParam opposite direction overlap" {
            let onToLine = Line3D(0., 0., 0., 10., 0., 0.)
            let lineToProject = Line3D(8., 5., 0., 2., 5., 0.)
            let result = Line3D.tryProjectOntoLineParam onToLine lineToProject
            match result with
            | Some (s, e) ->
                "start parameter from lineToProject start" |> expectEqualEpsilon s 0.8
                "end parameter from lineToProject end" |> expectEqualEpsilon e 0.2
            | None -> Expect.isTrue false "expected Some but got None"
        }

        test "tryProjectOntoLineParam encompassing line" {
            let onToLine = Line3D(2., 0., 0., 8., 0., 0.)
            let lineToProject = Line3D(0., 5., 0., 10., 5., 0.)
            let result = Line3D.tryProjectOntoLineParam onToLine lineToProject
            match result with
            | Some (s, e) ->
                "start parameter clamped" |> expectEqualEpsilon s 0.
                "end parameter clamped" |> expectEqualEpsilon e 1.0
            | None -> Expect.isTrue false "expected Some but got None"
        }

        test "tryProjectOntoLineParam zero length line to project" {
            let onToLine = Line3D(0., 0., 0., 10., 0., 0.)
            let lineToProject = Line3D(5., 5., 0., 5., 5., 0.)
            let result = Line3D.tryProjectOntoLineParam onToLine lineToProject
            match result with
            | Some (s, e) ->
                "both parameters equal" |> expectEqualEpsilon s 0.5
                "both parameters equal" |> expectEqualEpsilon e 0.5
            | None -> Expect.isTrue false "expected Some but got None"
        }

        test "tryProjectOntoLineParam too short target line throws" {
            let onToLine = Line3D(0., 0., 0., 1e-13, 0., 0.)
            let lineToProject = Line3D(5., 0., 0., 8., 0., 0.)
            "too short target throws" |> Expect.throws (fun () -> Line3D.tryProjectOntoLineParam onToLine lineToProject |> ignore)
        }

        test "tryProjectOntoLine full overlap" {
            let onToLine = Line3D(0., 0., 0., 10., 0., 0.)
            let lineToProject = Line3D(2., 5., 0., 8., 5., 0.)
            let result = Line3D.tryProjectOntoLine onToLine lineToProject
            match result with
            | Some projected ->
                "projected start X" |> expectEqualEpsilon projected.FromX 2.
                "projected end X" |> expectEqualEpsilon projected.ToX 8.
                "projected Y is 0" |> expectEqualEpsilon projected.FromY 0.
            | None -> Expect.isTrue false "expected Some but got None"
        }

        test "tryProjectOntoLine partial overlap preserves orientation" {
            let onToLine = Line3D(0., 0., 0., 10., 0., 0.)
            let lineToProject = Line3D(12., 5., 0., 5., 5., 0.)
            let result = Line3D.tryProjectOntoLine onToLine lineToProject
            match result with
            | Some projected ->
                "projected starts at clamped end" |> expectEqualEpsilon projected.FromX 10.
                "projected ends inside" |> expectEqualEpsilon projected.ToX 5.
            | None -> Expect.isTrue false "expected Some but got None"
        }

        test "tryProjectOntoLine no overlap" {
            let onToLine = Line3D(0., 0., 0., 10., 0., 0.)
            let lineToProject = Line3D(-10., 5., 0., -5., 5., 0.)
            let result = Line3D.tryProjectOntoLine onToLine lineToProject
            "no overlap returns None" |> Expect.isTrue result.IsNone
        }

        test "tryProjectOntoLine diagonal 3D" {
            let onToLine = Line3D(0., 0., 0., 10., 10., 10.)
            let lineToProject = Line3D(3., 3., 0., 7., 7., 0.)
            let result = Line3D.tryProjectOntoLine onToLine lineToProject
            match result with
            | Some projected ->
                let (s, e) = match Line3D.tryProjectOntoLineParam onToLine lineToProject with | Some p -> p | None -> (0., 0.)
                let expectedStart = onToLine.EvaluateAt s
                let expectedEnd = onToLine.EvaluateAt e
                let dist1 = Pnt.distance projected.From expectedStart
                let dist2 = Pnt.distance projected.To expectedEnd
                "projected start matches parameter" |> Expect.isTrue (dist1 < 1e-9)
                "projected end matches parameter" |> Expect.isTrue (dist2 < 1e-9)
            | None -> Expect.isTrue false "expected Some but got None"
        }

        test "tryProjectOntoLine perpendicular collapse" {
            let onToLine = Line3D(0., 0., 0., 10., 0., 0.)
            let lineToProject = Line3D(5., 0., 0., 5., 10., 0.)
            let result = Line3D.tryProjectOntoLine onToLine lineToProject
            match result with
            | Some projected ->
                "collapsed to point start X" |> expectEqualEpsilon projected.FromX 5.
                "collapsed to point end X" |> expectEqualEpsilon projected.ToX 5.
                "zero length result" |> Expect.isTrue projected.IsZeroLength
            | None -> Expect.isTrue false "expected Some but got None"
        }

        test "tryProjectOntoLine within tolerance at start" {
            let onToLine = Line3D(0., 0., 0., 10., 0., 0.)
            let lineToProject = Line3D(10. - 1e-8, 5., 0., 15., 5., 0.)
            let result = Line3D.tryProjectOntoLine onToLine lineToProject
            "just within tolerance returns Some" |> Expect.isTrue result.IsSome
        }
    ]


let testsLine3DDivide =
    testList "Line3D Divide/Split Methods" [

        test "divide into 1 segment" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let pts = Line3D.divide 1 ln
            "returns 2 points" |> Expect.equal pts.Length 2
            "first point is start" |> expectEqualEpsilon pts.[0].X 0.
            "second point is end" |> expectEqualEpsilon pts.[1].X 10.
        }

        test "divide into 5 segments" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let pts = Line3D.divide 5 ln
            "returns 6 points" |> Expect.equal pts.Length 6
            "first point" |> expectEqualEpsilon pts.[0].X 0.
            "second point" |> expectEqualEpsilon pts.[1].X 2.
            "third point" |> expectEqualEpsilon pts.[2].X 4.
            "last point" |> expectEqualEpsilon pts.[5].X 10.
        }

        test "divide diagonal line" {
            let ln = Line3D(0., 0., 0., 10., 10., 10.)
            let pts = Line3D.divide 2 ln
            "returns 3 points" |> Expect.equal pts.Length 3
            "midpoint X" |> expectEqualEpsilon pts.[1].X 5.
            "midpoint Y" |> expectEqualEpsilon pts.[1].Y 5.
            "midpoint Z" |> expectEqualEpsilon pts.[1].Z 5.
        }

        test "divide with 0 segments throws" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            "zero segments throws" |> Expect.throws (fun () -> Line3D.divide 0 ln |> ignore)
        }

        test "divide with negative segments throws" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            "negative segments throws" |> Expect.throws (fun () -> Line3D.divide -1 ln |> ignore)
        }

        test "divideMinLength exactly fitting" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let pts = Line3D.divideMinLength 2. ln
            "returns points for segments respecting minLength" |> Expect.isTrue (pts.Length >= 2)
            "at least 4 segments due to tolerance factor" |> Expect.isTrue (pts.Length >= 5)
        }

        test "divideMinLength with remainder" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let pts = Line3D.divideMinLength 3. ln
            "returns 4 points for 3 segments" |> Expect.equal pts.Length 4
            "first point" |> expectEqualEpsilon pts.[0].X 0.
            "last point" |> expectEqualEpsilon pts.[3].X 10.
        }

        test "divideMinLength too large throws" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            "minLength larger than line throws" |> Expect.throws (fun () -> Line3D.divideMinLength 11. ln |> ignore)
        }

        test "divideMinLength very small creates many segments" {
            let ln = Line3D(0., 0., 0., 1., 0., 0.)
            let pts = Line3D.divideMinLength 0.1 ln
            "creates many segments" |> Expect.isTrue (pts.Length >= 10)
        }

        test "divideMinLength zero or negative throws" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            "zero minLength throws" |> Expect.throws (fun () -> Line3D.divideMinLength 0. ln |> ignore)
        }

        test "divideMaxLength exactly fitting" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let pts = Line3D.divideMaxLength 5. ln
            "returns 3 points for 2 segments" |> Expect.equal pts.Length 3
        }

        test "divideMaxLength with small remainder" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let pts = Line3D.divideMaxLength 3. ln
            "returns 5 points for 4 segments" |> Expect.equal pts.Length 5
        }

        test "divideMaxLength very large returns endpoints" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let pts = Line3D.divideMaxLength 100. ln
            "returns 2 points" |> Expect.equal pts.Length 2
        }

        test "divideMaxLength zero or negative throws" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            "zero maxLength throws" |> Expect.throws (fun () -> Line3D.divideMaxLength 0. ln |> ignore)
        }

        test "split with gap into 2 segments" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let lns = Line3D.split 2. 2 ln
            "returns 2 lines" |> Expect.equal lns.Length 2
            "first line length" |> expectEqualEpsilon lns.[0].Length 4.
            "second line length" |> expectEqualEpsilon lns.[1].Length 4.
            "gap between lines" |> expectEqualEpsilon (lns.[1].FromX - lns.[0].ToX) 2.
        }

        test "split with gap into 3 segments" {
            let ln = Line3D(0., 0., 0., 12., 0., 0.)
            let lns = Line3D.split 1. 3 ln
            "returns 3 lines" |> Expect.equal lns.Length 3
            "total line length plus gaps" |> expectEqualEpsilon (lns.[0].Length + lns.[1].Length + lns.[2].Length) 10.
        }

        test "split with zero gap same as divide" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let lns = Line3D.split 0. 5 ln
            "returns 5 lines" |> Expect.equal lns.Length 5
            "each segment length" |> expectEqualEpsilon lns.[0].Length 2.
        }

        test "split with large gap returns empty" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let lns = Line3D.split 5. 3 ln
            "returns empty array" |> Expect.equal lns.Length 0
        }

        test "split with 0 segments throws" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            "zero segments throws" |> Expect.throws (fun () -> Line3D.split 1. 0 ln |> ignore)
        }

        test "split with negative segments throws" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            "negative segments throws" |> Expect.throws (fun () -> Line3D.split 1. -1 ln |> ignore)
        }

        test "splitMinLength with gap" {
            let ln = Line3D(0., 0., 0., 20., 0., 0.)
            let lns = Line3D.splitMinLength 1. 3. ln
            "returns multiple lines" |> Expect.isTrue (lns.Length > 0)
            "each segment at least minLength" |> Expect.isTrue (lns |> Array.forall (fun l -> l.Length >= 3. - 1e-9))
        }

        test "splitMinLength line too short throws" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            "line too short for minLength throws" |> Expect.throws (fun () -> Line3D.splitMinLength 5. 20. ln |> ignore)
        }

        test "splitMaxLength with gap" {
            let ln = Line3D(0., 0., 0., 20., 0., 0.)
            let lns = Line3D.splitMaxLength 1. 5. ln
            "returns multiple lines" |> Expect.isTrue (lns.Length > 0)
            "each segment at most maxLength" |> Expect.isTrue (lns |> Array.forall (fun l -> l.Length <= 5. + 1e-9))
        }

        test "divideEvery basic" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let pts = Line3D.divideEvery 2. ln
            "includes start" |> expectEqualEpsilon pts.[0].X 0.
            "includes divisions" |> Expect.isTrue (pts.Count > 2)
            "includes end if remainder significant" |> Expect.isTrue (pts.[pts.Count - 1].X >= 8.)
        }

        test "divideEvery exact fit includes end" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let pts = Line3D.divideEvery 5. ln
            "includes start and end" |> Expect.equal pts.Count 3
            "last point is end" |> expectEqualEpsilon pts.[2].X 10.
        }

        test "divideEvery with tiny remainder" {
            let ln = Line3D(0., 0., 0., 10.001, 0., 0.)
            let pts = Line3D.divideEvery 5. ln
            "includes end due to 1% rule" |> expectEqualEpsilon pts.[pts.Count - 1].X 10.001
        }

        test "divideEvery with very tiny remainder includes end" {
            let ln = Line3D(0., 0., 0., 10.0001, 0., 0.)
            let pts = Line3D.divideEvery 5. ln
            "includes end if remainder > 1%" |> Expect.isTrue (pts.[pts.Count - 1].X >= 10.)
        }

        test "divideInsideEvery excludes endpoints" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let pts = Line3D.divideInsideEvery 2. ln
            "first point not at start" |> Expect.isTrue (pts.[0].X > 0.)
            "last point not at end" |> Expect.isTrue (pts.[pts.Count - 1].X < 10.)
        }

        test "divideInsideEvery with 2 segments" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let pts = Line3D.divideInsideEvery 5. ln
            "one interior point" |> Expect.equal pts.Count 1
            "midpoint" |> expectEqualEpsilon pts.[0].X 5.
        }

        test "divideInsideEvery very small distance" {
            let ln = Line3D(0., 0., 0., 1., 0., 0.)
            let pts = Line3D.divideInsideEvery 0.1 ln
            "multiple interior points" |> Expect.isTrue (pts.Count > 1)
        }

        test "divideEvery distance larger than line" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let ps = Line3D.divideEvery 11. ln
            "first point beyond line length"  |> Expect.isTrue(Pnt.equals 1e-12 ps[0] ln.From )
            "second point beyond line length" |> Expect.isTrue(Pnt.equals 1e-12 ps[1] ln.To )
        }
    ]


let testsLine3DOffset =
    testList "Line3D Offset Methods" [

        test "offsetXY positive amount" {
            let ln = Line3D(0., 0., 5., 10., 0., 5.)
            let offset = Line3D.offsetXY 2. ln
            "offset to left in XY plane" |> expectEqualEpsilon offset.FromY 2.
            "Z unchanged" |> expectEqualEpsilon offset.FromZ 5.
        }

        test "offsetXY negative amount" {
            let ln = Line3D(0., 0., 5., 10., 0., 5.)
            let offset = Line3D.offsetXY -2. ln
            "offset to right in XY plane" |> expectEqualEpsilon offset.FromY -2.
        }

        test "offsetXY zero amount returns same line" {
            let ln = Line3D(0., 0., 5., 10., 0., 5.)
            let offset = Line3D.offsetXY 0. ln
            "zero offset returns input" |> Expect.isTrue (Pnt.distance offset.From ln.From < 1e-9)
        }

        test "offsetXY diagonal line" {
            let ln = Line3D(0., 0., 5., 10., 10., 5.)
            let offset = Line3D.offsetXY 1. ln
            let expectedDir = Vec(-1., 1., 0.).Unitized
            let actualOffset = offset.From - ln.From
            "offset perpendicular to XY projection" |> expectEqualEpsilon (actualOffset.Unitized *** expectedDir) 1.
        }

        test "offsetXY vertical line throws" {
            let ln = Line3D(0., 0., 0., 0., 0., 10.)
            "vertical line throws" |> Expect.throws (fun () -> Line3D.offsetXY 1. ln |> ignore)
        }

        test "offsetXY too short line throws" {
            let ln = Line3D(0., 0., 0., 1e-13, 0., 0.)
            "too short throws" |> Expect.throws (fun () -> Line3D.offsetXY 1. ln |> ignore)
        }

        test "offset horizontal and normal" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let offset = Line3D.offset 2. 3. ln
            "horizontal offset (cross with Z-axis points to -Y)" |> expectEqualEpsilon offset.FromY -2.
            "normal offset (cross with normHor points to -Z)" |> expectEqualEpsilon offset.FromZ -3.
        }

        test "offset zero distances returns same line" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let offset = Line3D.offset 0. 0. ln
            "zero offsets return input" |> Expect.isTrue (Pnt.distance offset.From ln.From < 1e-9)
        }

        test "offset diagonal 3D line" {
            let ln = Line3D(0., 0., 0., 10., 10., 10.)
            let offset = Line3D.offset 1. 1. ln
            "offset line length same" |> expectEqualEpsilon offset.Length ln.Length
            "offset distance from original" |> Expect.isTrue ((offset.From - ln.From).Length > 0.)
        }

        test "offset very close points uses Z axis" {
            let ln = Line3D(0., 0., 0., 1e-7, 0., 0.)
            let offset = Line3D.offset 0. 1. ln
            "uses Z axis as second direction" |> expectEqualEpsilon offset.FromZ 1.
        }
    ]


let testsLine3DWithLength =
    testList "Line3D WithLength Methods" [

        test "withLengthFromStart shorter" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let shortened = Line3D.withLengthFromStart 5. ln
            "start unchanged" |> expectEqualEpsilon shortened.FromX 0.
            "end at new length" |> expectEqualEpsilon shortened.ToX 5.
            "new length" |> expectEqualEpsilon shortened.Length 5.
        }

        test "withLengthFromStart longer" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let extended = Line3D.withLengthFromStart 15. ln
            "start unchanged" |> expectEqualEpsilon extended.FromX 0.
            "end at new length" |> expectEqualEpsilon extended.ToX 15.
            "new length" |> expectEqualEpsilon extended.Length 15.
        }

        test "withLengthFromStart diagonal" {
            let ln = Line3D(0., 0., 0., 10., 10., 10.)
            let newLen = 5.
            let result = Line3D.withLengthFromStart newLen ln
            "new length correct" |> expectEqualEpsilon result.Length newLen
        }

        test "withLengthFromStart too short line throws" {
            let ln = Line3D(0., 0., 0., 1e-13, 0., 0.)
            "too short throws" |> Expect.throws (fun () -> Line3D.withLengthFromStart 5. ln |> ignore)
        }

        test "withLengthToEnd shorter" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let shortened = Line3D.withLengthToEnd 5. ln
            "start moved" |> expectEqualEpsilon shortened.FromX 5.
            "end unchanged" |> expectEqualEpsilon shortened.ToX 10.
            "new length" |> expectEqualEpsilon shortened.Length 5.
        }

        test "withLengthToEnd longer" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let extended = Line3D.withLengthToEnd 15. ln
            "start moved back" |> expectEqualEpsilon extended.FromX -5.
            "end unchanged" |> expectEqualEpsilon extended.ToX 10.
            "new length" |> expectEqualEpsilon extended.Length 15.
        }

        test "withLengthToEnd too short line throws" {
            let ln = Line3D(0., 0., 0., 1e-13, 0., 0.)
            "too short throws" |> Expect.throws (fun () -> Line3D.withLengthToEnd 5. ln |> ignore)
        }

        test "withLengthFromMid shorter" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let shortened = Line3D.withLengthFromMid 6. ln
            "centered on midpoint start" |> expectEqualEpsilon shortened.FromX 2.
            "centered on midpoint end" |> expectEqualEpsilon shortened.ToX 8.
            "new length" |> expectEqualEpsilon shortened.Length 6.
        }

        test "withLengthFromMid longer" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let extended = Line3D.withLengthFromMid 20. ln
            "centered on midpoint start" |> expectEqualEpsilon extended.FromX -5.
            "centered on midpoint end" |> expectEqualEpsilon extended.ToX 15.
            "new length" |> expectEqualEpsilon extended.Length 20.
        }

        test "withLengthFromMid preserves midpoint" {
            let ln = Line3D(2., 3., 4., 12., 13., 14.)
            let mid = ln.Mid
            let result = Line3D.withLengthFromMid 20. ln
            let newMid = result.Mid
            "midpoint X preserved" |> expectEqualEpsilon newMid.X mid.X
            "midpoint Y preserved" |> expectEqualEpsilon newMid.Y mid.Y
            "midpoint Z preserved" |> expectEqualEpsilon newMid.Z mid.Z
        }

        test "withLengthFromMid too short line throws" {
            let ln = Line3D(0., 0., 0., 1e-13, 0., 0.)
            "too short throws" |> Expect.throws (fun () -> Line3D.withLengthFromMid 5. ln |> ignore)
        }

        test "pointAtDistance positive" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let pt = Line3D.pointAtDistance 3. ln
            "point at distance 3" |> expectEqualEpsilon pt.X 3.
        }

        test "pointAtDistance negative" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let pt = Line3D.pointAtDistance -3. ln
            "point before start" |> expectEqualEpsilon pt.X -3.
        }

        test "pointAtDistance beyond end" {
            let ln = Line3D(0., 0., 0., 10., 0., 0.)
            let pt = Line3D.pointAtDistance 15. ln
            "point beyond end" |> expectEqualEpsilon pt.X 15.
        }

        test "pointAtDistance diagonal" {
            let ln = Line3D(0., 0., 0., 10., 10., 10.)
            let pt = Line3D.pointAtDistance (ln.Length / 2.) ln
            "midpoint" |> Expect.isTrue (Pnt.distance pt ln.Mid < 1e-9)
        }

        test "pointAtDistance too short line throws" {
            let ln = Line3D(0., 0., 0., 1e-13, 0., 0.)
            "too short throws" |> Expect.throws (fun () -> Line3D.pointAtDistance 3. ln |> ignore)
        }
    ]


let testsLine3DIntersection =
    testList "Line3D Intersection Methods" [

        test "doIntersect crossing lines" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(5., -5., 0., 5., 5., 0.)
            "crossing lines intersect" |> Expect.isTrue (Line3D.doIntersect lnA lnB)
        }

        test "doIntersect parallel lines" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(0., 5., 0., 10., 5., 0.)
            "parallel lines don't intersect" |> Expect.isFalse (Line3D.doIntersect lnA lnB)
        }

        test "doIntersect skew lines close" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(5., 0., 0.0000001, 5., 10., 0.0000001)
            "skew lines within tolerance intersect" |> Expect.isTrue (Line3D.doIntersect lnA lnB)
        }

        test "doIntersect skew lines far" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(5., 0., 5., 5., 10., 5.)
            "skew lines beyond tolerance don't intersect" |> Expect.isFalse (Line3D.doIntersect lnA lnB)
        }

        test "doIntersect non-intersecting within segments" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(15., -5., 0., 15., 5., 0.)
            "rays would intersect but segments don't" |> Expect.isFalse (Line3D.doIntersect lnA lnB)
        }

        test "doRaysIntersect extending beyond segments" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(15., -5., 0., 15., 5., 0.)
            "rays intersect" |> Expect.isTrue (Line3D.doRaysIntersect lnA lnB)
        }

        test "doRaysIntersect parallel rays" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(0., 5., 0., 10., 5., 0.)
            "parallel rays don't intersect" |> Expect.isFalse (Line3D.doRaysIntersect lnA lnB)
        }

        test "doIntersectOrOverlap touching endpoints" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(10., 0., 0., 20., 0., 0.)
            "touching at endpoints" |> Expect.isTrue (Line3D.doIntersectOrOverlap lnA lnB)
        }

        test "doIntersectOrOverlap overlapping parallel" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(5., 0., 0., 15., 0., 0.)
            "overlapping collinear lines" |> Expect.isTrue (Line3D.doIntersectOrOverlap lnA lnB)
        }

        test "doIntersectOrOverlap zero length at same point" {
            let lnA = Line3D(5., 5., 5., 5., 5., 5.)
            let lnB = Line3D(5., 5., 5., 5., 5., 5.)
            "zero length at same location" |> Expect.isTrue (Line3D.doIntersectOrOverlap lnA lnB)
        }

        test "tryIntersect returns intersection point" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(5., -5., 0., 5., 5., 0.)
            let result = Line3D.tryIntersect lnA lnB
            match result with
            | Some pt ->
                "intersection at X=5" |> expectEqualEpsilon pt.X 5.
                "intersection at Y=0" |> expectEqualEpsilon pt.Y 0.
            | None -> Expect.isTrue false "expected Some but got None"
        }

        test "tryIntersect no intersection returns None" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(15., -5., 0., 15., 5., 0.)
            let result = Line3D.tryIntersect lnA lnB
            "no intersection returns None" |> Expect.isTrue result.IsNone
        }

        test "tryIntersectRay returns ray intersection" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(15., -5., 0., 15., 5., 0.)
            let result = Line3D.tryIntersectRay lnA lnB
            match result with
            | Some pt -> "ray intersection found" |> expectEqualEpsilon pt.X 15.
            | None -> Expect.isTrue false "expected Some but got None"
        }

        test "tryIntersectRay parallel returns None" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(0., 5., 0., 10., 5., 0.)
            let result = Line3D.tryIntersectRay lnA lnB
            "parallel rays return None" |> Expect.isTrue result.IsNone
        }

        test "isTouchingEndsOf touching at ends" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(10., 0., 0., 20., 0., 0.)
            let result = XLine3D.getEndsTouching(lnA, lnB)
            match result with
            | XLine3D.XEnds.EndA_StartB -> Expect.isTrue true "correct end touching"
            | _ -> Expect.isTrue false "wrong touching type"
        }

        test "isTouchingEndsOf not touching" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(15., 0., 0., 20., 0., 0.)
            let result = XLine3D.getEndsTouching(lnA, lnB)
            match result with
            | XLine3D.XEnds.NotTouching -> Expect.isTrue true "correctly not touching"
            | _ -> Expect.isTrue false "should not be touching"
        }

        test "isTouchingEndsOf identical" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(0., 0., 0., 10., 0., 0.)
            let result = XLine3D.getEndsTouching(lnA, lnB)
            match result with
            | XLine3D.XEnds.Identical -> Expect.isTrue true "correctly identical"
            | _ -> Expect.isTrue false "should be identical"
        }
    ]


let testsLine3DDistance =
    testList "Line3D Distance Methods" [

        test "distanceToLine parallel lines" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(0., 5., 0., 10., 5., 0.)
            let dist = Line3D.distanceToLine lnA lnB
            "distance is 5" |> expectEqualEpsilon dist 5.
        }

        test "distanceToLine intersecting lines" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(5., -5., 0., 5., 5., 0.)
            let dist = Line3D.distanceToLine lnA lnB
            "distance is 0" |> expectEqualEpsilon dist 0.
        }

        test "distanceToLine skew lines" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(0., 5., 2., 10., 5., 2.)
            let dist = Line3D.distanceToLine lnA lnB
            "distance between skew lines" |> Expect.isTrue (abs (dist - sqrt(29.)) < 1e-9)
        }

        test "sqDistanceToLine matches distanceToLine" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(0., 3., 4., 10., 3., 4.)
            let sqDist = Line3D.sqDistanceToLine lnA lnB
            let dist = Line3D.distanceToLine lnA lnB
            "squared distance matches" |> expectEqualEpsilon (sqrt sqDist) dist
        }

        test "closestPoints on intersecting" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(5., -5., 0., 5., 5., 0.)
            let (ptA, ptB) = Line3D.closestPoints lnA lnB
            "closest points are same" |> Expect.isTrue (Pnt.distance ptA ptB < 1e-9)
            "at intersection" |> expectEqualEpsilon ptA.X 5.
        }

        test "closestPoints on parallel" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(0., 5., 0., 10., 5., 0.)
            let (ptA, ptB) = Line3D.closestPoints lnA lnB
            "closest points Y distance" |> expectEqualEpsilon (abs (ptB.Y - ptA.Y)) 5.
        }

        test "closestPoints on skew" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(5., 0., 2., 5., 10., 2.)
            let (ptA, ptB) = Line3D.closestPoints lnA lnB
            "closest point A on lnA" |> expectEqualEpsilon ptA.X 5.
            "closest point B on lnB" |> expectEqualEpsilon ptB.X 5.
            "Z distance is 2" |> expectEqualEpsilon (abs (ptB.Z - ptA.Z)) 2.
        }

        test "closestParameters" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(5., -10., 0., 5., 10., 0.)
            let (tA, tB) = Line3D.closestParameters lnA lnB
            "parameter on A" |> expectEqualEpsilon tA 0.5
            "parameter on B" |> expectEqualEpsilon tB 0.5
        }

        test "tryGetOverlap overlapping collinear" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(5., 0., 0., 15., 0., 0.)
            let result = Line3D.tryGetOverlap lnA lnB
            match result with
            | Some (s, e) ->
                "overlap start" |> expectEqualEpsilon s 0.5
                "overlap end" |> expectEqualEpsilon e 1.0
            | None -> Expect.isTrue false "expected Some but got None"
        }

        test "tryGetOverlap no overlap collinear" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(15., 0., 0., 20., 0., 0.)
            let result = Line3D.tryGetOverlap lnA lnB
            "no overlap returns None" |> Expect.isTrue result.IsNone
        }

        test "tryGetOverlap non-collinear returns None" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(0., 5., 0., 10., 5., 0.)
            let result = Line3D.tryGetOverlap lnA lnB
            "non-collinear returns None" |> Expect.isTrue result.IsNone
        }

        test "tryGetOverlap touching at point" {
            let lnA = Line3D(0., 0., 0., 10., 0., 0.)
            let lnB = Line3D(10., 0., 0., 20., 0., 0.)
            let result = Line3D.tryGetOverlap lnA lnB
            match result with
            | Some (s, e) ->
                "touching at end" |> expectEqualEpsilon s 1.0
                "touching at end" |> expectEqualEpsilon e 1.0
            | None -> Expect.isTrue false "touching should return Some"
        }
    ]


let testsFastParallel3D =
    testList "Line3D Fast Parallel Tests" [

        test "IsParallelToFast identical lines" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(0., 0., 0., 10., 0., 0.)
            "identical lines are parallel" |> Expect.isTrue (a.IsParallelToFast(b))
        }

        test "IsParallelToFast same direction different lengths" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(0., 0., 0., 5., 0., 0.)
            "same direction different lengths are parallel" |> Expect.isTrue (a.IsParallelToFast(b))
        }

        test "IsParallelToFast opposite directions" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(10., 0., 0., 0., 0., 0.)
            "opposite directions are parallel" |> Expect.isTrue (a.IsParallelToFast(b))
        }

        test "IsParallelToFast parallel offset lines" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(0., 5., 0., 10., 5., 0.)
            "parallel offset lines are parallel" |> Expect.isTrue (a.IsParallelToFast(b))
        }

        test "IsParallelToFast perpendicular lines" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(0., 0., 0., 0., 10., 0.)
            "perpendicular lines are not parallel" |> Expect.isFalse (a.IsParallelToFast(b))
        }

        test "IsParallelToFast nearly parallel within tolerance" {
            let a = Line3D(0., 0., 0., 100., 0., 0.)
            let b = Line3D(0., 0., 0., 100., 5e-9, 0.)
            // cross = (0, 0, 100*5e-9) = (0, 0, 5e-7), crossMagSq = 25e-14 < 1e-12
            "nearly parallel within default tolerance" |> Expect.isTrue (a.IsParallelToFast(b))
        }

        test "IsParallelToFast nearly parallel outside tolerance" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(0., 0., 0., 10., 0.5, 0.)
            "nearly parallel outside tolerance" |> Expect.isFalse (a.IsParallelToFast(b))
        }

        test "IsParallelToFast zero length lines returns true" {
            let a = Line3D(0., 0., 0., 0., 0., 0.)
            let b = Line3D(5., 5., 5., 5., 5., 5.)
            "zero length lines return true (as documented)" |> Expect.isTrue (a.IsParallelToFast(b))
        }

        test "IsParallelToFast very short parallel lines" {
            let a = Line3D(0., 0., 0., 1e-9, 0., 0.)
            let b = Line3D(0., 0., 0., 1e-10, 0., 0.)
            "very short parallel lines are parallel" |> Expect.isTrue (a.IsParallelToFast(b))
        }

        test "IsParallelToFast diagonal lines parallel" {
            let a = Line3D(0., 0., 0., 10., 10., 10.)
            let b = Line3D(5., 5., 5., 15., 15., 15.)
            "diagonal parallel lines are parallel" |> Expect.isTrue (a.IsParallelToFast(b))
        }

        test "IsParallelToFast custom tolerance strict" {
            let a = Line3D(0., 0., 0., 100., 0., 0.)
            let b = Line3D(0., 0., 0., 100., 2e-7, 0.)
            // cross = (0, 0, 100*2e-7) = (0, 0, 2e-5), crossMagSq = 4e-10 > 1e-12
            "with default tolerance not parallel due to larger deviation" |> Expect.isFalse (a.IsParallelToFast(b))
            "with looser tolerance parallel" |> Expect.isTrue (a.IsParallelToFast(b, 1e-9))
        }


        test "IsCoincidentToFast identical lines" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(0., 0., 0., 10., 0., 0.)
            "identical lines are coincident" |> Expect.isTrue (a.IsCoincidentToFast(b))
        }

        test "IsCoincidentToFast same ray different lengths" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(0., 0., 0., 5., 0., 0.)
            "same ray different lengths are coincident" |> Expect.isTrue (a.IsCoincidentToFast(b))
        }

        test "IsCoincidentToFast same ray offset start" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(5., 0., 0., 15., 0., 0.)
            "same ray offset start are coincident" |> Expect.isTrue (a.IsCoincidentToFast(b))
        }

        test "IsCoincidentToFast parallel but offset" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(0., 2., 0., 10., 2., 0.)
            "parallel lines with offset should not be coincident" |> Expect.isFalse (a.IsCoincidentToFast(b))
        }

        test "IsCoincidentToFast opposite directions on same ray" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(10., 0., 0., 0., 0., 0.)
            "opposite directions on same ray are coincident" |> Expect.isTrue (a.IsCoincidentToFast(b))
        }

        test "IsCoincidentToFast diagonal lines same ray" {
            let a = Line3D(0., 0., 0., 10., 10., 10.)
            let b = Line3D(5., 5., 5., 15., 15., 15.)
            "diagonal lines same ray are coincident" |> Expect.isTrue (a.IsCoincidentToFast(b))
        }

        test "IsCoincidentToFast perpendicular lines" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(0., 0., 0., 0., 10., 0.)
            "perpendicular lines are not coincident" |> Expect.isFalse (a.IsCoincidentToFast(b))
        }

        test "IsCoincidentToFast zero length lines returns true" {
            let a = Line3D(0., 0., 0., 0., 0., 0.)
            let b = Line3D(5., 5., 5., 5., 5., 5.)
            "zero length lines return true (as documented)" |> Expect.isTrue (a.IsCoincidentToFast(b))
        }


        test "IsParallelAndOrientedToFast same direction" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(0., 0., 0., 5., 0., 0.)
            "same direction are parallel and oriented" |> Expect.isTrue (a.IsParallelAndOrientedToFast(b))
        }

        test "IsParallelAndOrientedToFast opposite directions" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(10., 0., 0., 0., 0., 0.)
            "opposite directions are not oriented" |> Expect.isFalse (a.IsParallelAndOrientedToFast(b))
        }

        test "IsParallelAndOrientedToFast parallel offset same direction" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(0., 5., 0., 10., 5., 0.)
            "parallel offset same direction" |> Expect.isTrue (a.IsParallelAndOrientedToFast(b))
        }

        test "IsParallelAndOrientedToFast parallel offset opposite direction" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(10., 5., 0., 0., 5., 0.)
            "parallel offset opposite direction are not oriented" |> Expect.isFalse (a.IsParallelAndOrientedToFast(b))
        }

        test "IsParallelAndOrientedToFast perpendicular" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(0., 0., 0., 0., 10., 0.)
            "perpendicular lines fail both parallel and orientation" |> Expect.isFalse (a.IsParallelAndOrientedToFast(b))
        }

        test "IsParallelAndOrientedToFast zero length returns false" {
            let a = Line3D(0., 0., 0., 0., 0., 0.)
            let b = Line3D(0., 0., 0., 10., 0., 0.)
            "zero length line fails dot product check" |> Expect.isFalse (a.IsParallelAndOrientedToFast(b))
        }


        test "IsParallelAndOpposingToFast opposite directions" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(10., 0., 0., 0., 0., 0.)
            "opposite directions are parallel and opposing" |> Expect.isTrue (a.IsParallelAndOpposingToFast(b))
        }

        test "IsParallelAndOpposingToFast same direction" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(0., 0., 0., 5., 0., 0.)
            "same direction are not opposing" |> Expect.isFalse (a.IsParallelAndOpposingToFast(b))
        }

        test "IsParallelAndOpposingToFast parallel offset opposing" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(10., 5., 0., 0., 5., 0.)
            "parallel offset opposing directions" |> Expect.isTrue (a.IsParallelAndOpposingToFast(b))
        }

        test "IsParallelAndOpposingToFast parallel offset same direction" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(0., 5., 0., 10., 5., 0.)
            "parallel offset same direction are not opposing" |> Expect.isFalse (a.IsParallelAndOpposingToFast(b))
        }

        test "IsParallelAndOpposingToFast perpendicular" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(0., 0., 0., 0., 10., 0.)
            "perpendicular lines fail both parallel and opposing" |> Expect.isFalse (a.IsParallelAndOpposingToFast(b))
        }

        test "IsParallelAndOpposingToFast zero length returns false" {
            let a = Line3D(0., 0., 0., 0., 0., 0.)
            let b = Line3D(10., 0., 0., 0., 0., 0.)
            "zero length line fails dot product check" |> Expect.isFalse (a.IsParallelAndOpposingToFast(b))
        }


        test "IsCoincidentAndOrientedToFast same ray same direction" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(0., 0., 0., 5., 0., 0.)
            "same ray same direction are coincident and oriented" |> Expect.isTrue (a.IsCoincidentAndOrientedToFast(b))
        }

        test "IsCoincidentAndOrientedToFast same ray opposite direction" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(10., 0., 0., 0., 0., 0.)
            "same ray opposite direction are not oriented" |> Expect.isFalse (a.IsCoincidentAndOrientedToFast(b))
        }

        test "IsCoincidentAndOrientedToFast same ray offset start same direction" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(5., 0., 0., 15., 0., 0.)
            "same ray offset start same direction" |> Expect.isTrue (a.IsCoincidentAndOrientedToFast(b))
        }

        test "IsCoincidentAndOrientedToFast parallel offset same direction" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(0., 5., 0., 10., 5., 0.)
            "parallel offset same direction are not coincident" |> Expect.isFalse (a.IsCoincidentAndOrientedToFast(b))
        }

        test "IsCoincidentAndOrientedToFast diagonal same ray same direction" {
            let a = Line3D(0., 0., 0., 10., 10., 10.)
            let b = Line3D(5., 5., 5., 15., 15., 15.)
            "diagonal same ray same direction" |> Expect.isTrue (a.IsCoincidentAndOrientedToFast(b))
        }

        test "IsCoincidentAndOrientedToFast diagonal same ray opposite direction" {
            let a = Line3D(0., 0., 0., 10., 10., 10.)
            let b = Line3D(10., 10., 10., 0., 0., 0.)
            "diagonal same ray opposite direction are not oriented" |> Expect.isFalse (a.IsCoincidentAndOrientedToFast(b))
        }


        test "IsCoincidentAndOpposingToFast same ray opposite direction" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(10., 0., 0., 0., 0., 0.)
            "same ray opposite direction are coincident and opposing" |> Expect.isTrue (a.IsCoincidentAndOpposingToFast(b))
        }

        test "IsCoincidentAndOpposingToFast same ray same direction" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(0., 0., 0., 5., 0., 0.)
            "same ray same direction are not opposing" |> Expect.isFalse (a.IsCoincidentAndOpposingToFast(b))
        }

        test "IsCoincidentAndOpposingToFast same ray offset opposite direction" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(15., 0., 0., 5., 0., 0.)
            "same ray offset opposite direction" |> Expect.isTrue (a.IsCoincidentAndOpposingToFast(b))
        }

        test "IsCoincidentAndOpposingToFast parallel offset opposite direction" {
            let a = Line3D(0., 0., 0., 10., 0., 0.)
            let b = Line3D(10., 5., 0., 0., 5., 0.)
            "parallel offset opposite direction are not coincident" |> Expect.isFalse (a.IsCoincidentAndOpposingToFast(b))
        }

        test "IsCoincidentAndOpposingToFast diagonal same ray opposite direction" {
            let a = Line3D(0., 0., 0., 10., 10., 10.)
            let b = Line3D(10., 10., 10., 0., 0., 0.)
            "diagonal same ray opposite direction" |> Expect.isTrue (a.IsCoincidentAndOpposingToFast(b))
        }

        test "IsCoincidentAndOpposingToFast diagonal same ray same direction" {
            let a = Line3D(0., 0., 0., 10., 10., 10.)
            let b = Line3D(5., 5., 5., 15., 15., 15.)
            "diagonal same ray same direction are not opposing" |> Expect.isFalse (a.IsCoincidentAndOpposingToFast(b))
        }
    ]


let tests =
    testList "All Line Tests" [
        testsIsCoincident
        testsFastMethods
        testsDistanceBetweenLines
        testsLine2DBasics
        testsLine2DExtendShrink
        testsLine2DMove
        testsLine2DClosestPoint
        testsLine2DProjection
        testsLine2DDivide
        testsLine2DOffset
        testsLine2DWithLength
        testsLine2DIntersection
        testsFastParallel3D
        testsLine3DBasics
        testsLine3DExtendShrink
        testsLine3DMove
        testsLine3DClosestPoint
        testsLine3DProjection
        testsLine3DDivide
        testsLine3DOffset
        testsLine3DWithLength
        testsLine3DIntersection
        testsLine3DDistance
    ]
