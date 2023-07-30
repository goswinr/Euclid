
#r "nuget: Euclid, 0.2.1"
//#r "System.Runtime.Serialization"
//#r @"D:\Git\Euclid\bin\release\net472\Euclid.dll"

open Euclid
open System


let x    = Vc(3, 0)
let y'   = Vc(1, 9) 
let y    = Vc(0, 9) 
let y'' = Vc(-1, 9) 

printfn "\nDirectionPi:"
printfn "%A" x.DirectionPi
printfn "%A" y'.DirectionPi
printfn "%A" y.DirectionPi
printfn "%A" y''.DirectionPi
printfn "%A" (-x).DirectionPi
printfn "%A" (-y).DirectionPi



printfn "\nDirection2Pi:"
printfn "%A" x.Direction2Pi
printfn "%A" y.Direction2Pi
printfn "%A" (-x).Direction2Pi
printfn "%A" (-y).Direction2Pi



printfn "\nDirection180:"
printfn "%A" x.Direction180
printfn "%A" y.Direction180
printfn "%A" (-x).Direction180
printfn "%A" (-y).Direction180



printfn "\nDirection360:"
printfn "%A" x.Direction360
printfn "%A" y.Direction360
printfn "%A" (-x).Direction360
printfn "%A" (-y).Direction360

printfn "\nAtan2:"
printfn "%A" <| Math.Atan2(x.Y, x.X)
printfn "%A" <| Math.Atan2(y.Y, y.X)
printfn "%A" <| Math.Atan2(-x.Y, -x.X)
printfn "%A" <| Math.Atan2(-y.Y, -y.X)
