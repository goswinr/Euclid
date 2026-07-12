#r "D:/Git/_Euclid_/Euclid/bin/Release/net6.0/Euclid.dll"
open Euclid


let o = Pnt(0.0, 0.0, 0.0)
let x = Pnt(1.0, 0.0, 0.0)
let y = Pnt(0.0, 1.0, 0.0)
let r = Rect3D.createFrom3Points(o, x , y)
let l = Line3D(x |> Pnt.moveZ 0.001, y|> Pnt.moveZ -0.001)


let xx = Rect3D.intersectRay l r

printfn "Intersection: %A" xx