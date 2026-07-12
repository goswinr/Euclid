#r "C:/Program Files/Rhino 8/System/RhinoCommon.dll"
#r "D:/Git/_Euclid_/Euclid/bin/Release/net6.0/Euclid.dll"
#r "nuget: Rhino.Scripting.FSharp"
#r "nuget: ResizeArrayT"
#r "nuget: Fesher"
open System
open Rhino.Scripting
open Rhino.Scripting.FSharp
open Rhino.Geometry
open Euclid
open Fesher
type rs = RhinoScriptSyntax

clearFeshLog()

let pt3 (p:Pt) = Point3d(p.X, p.Y, 0.)
let ln (l:Line) = Line2D(l.FromX, l.FromY, l.ToX,  l.ToY)


let permute(a:Line2D, b:Line2D) =
    let ar= a.Reversed
    let br = b.Reversed
    [a,b; ar,b; a,br; ar,br;
     b,a; br,a; b,ar; br,ar]

let lns =
    let a1 = Line2D(-7.033123196922749, 11.030024575275137, -1.956939844000427, 11.030024575275137)
    let b1 = Line2D(-4.115503793140293, 11.030024575275137, 0.9606795597820286, 11.030024575275137)

    let a2 = Line2D(-7.033123196922749, 7.021262955443959, -1.956939844000427, 7.021262955443959)
    let b2 = Line2D(-0.8420771449941258, 7.021262955443959, 4.234106207928196, 7.021262955443959)

    let a3 = Line2D(-7.033123196922749, -0.5218506250667811, -1.956939844000427, -0.5218506250667811)
    let b3 = Line2D(-4.993161662570792, -0.3083662784485517, 0.08302169035153018, -0.3083662784485517)

    let a4 = Line2D(-6.535285900282159, -6.378759997308991, -1.4591025473598371, -6.378759997308991)
    let b4 = Line2D(-1.4591025473598371, -6.378759997308991, 3.617080805562485, -6.378759997308991)

    let a5 = Line2D(-5.481042213278564, -13.319197603416011, -0.4048588603562422, -13.319197603416011)
    let b5 = Line2D(-0.4048588603562422, -13.319197603416011, 2.6504123099501484, -9.26543940781772)

    let a6 = Line2D(-6.481042213278564, -23.31919760341601, -1.4048588603562422, -23.31919760341601)
    let b6 = Line2D(-2.5505879027131053, -22.077274213827337, -1.147530505881357, -19.888526790076185)

    [
        a1,b1;
        a2,b2;
        a3,b3;
        a4,b4;
        a5,b5;
        a6,b6;
    ]


let doX(a:Line2D, b:Line2D, k:int) =
    if Line2D.equals 0.0 a b then
        failwith "input twice the same line"
    // printfn $"\nlet a{k} = {a.AsFSharpCode}"
    // printfn $"let b{k} = {b.AsFSharpCode}"
    //
    let a = a.Reversed
    let b = b.Reversed

    let ab = XLine2D.closestParameters (a,  b)
    // for (a',b') in permute(a,b) do
        // let ab' = XLine2D.closestParameters ( a' ,  b' ) 
        // if ab' <> ab then  failwithf "permutation failed"

    printfn $"let result{k} = XLine2D.closestParameters (a,  b)"
    let struct(u, v) = ab

    rs.AddTextDot("a", a.EvaluateAt(u) |> pt3) |> ignore
    rs.AddTextDot("b", b.EvaluateAt(v) |> pt3) |> ignore


let rec selectInUI(k:int) =
    let a = rs.GetObject("Line 1", filter=rs.Filter.Curve)|> rs.CoerceLine |> ln
    let b = rs.GetObject("Line 2", filter=rs.Filter.Curve)|> rs.CoerceLine |> ln
    doX(a, b, k)
    selectInUI(k + 1)

// go(1)

for k, (a,b) in Seq.indexed lns do
    doX(a, b, k + 1)

printfn "done"