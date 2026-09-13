#r "C:/Program Files/Rhino 8/System/RhinoCommon.dll"
#r "nuget: Rhino.Scripting.FSharp"
#r "nuget: ResizeArrayT"
#r "nuget: Euclid.Rhino, 0.50.0" 

open Euclid
open System
open ResizeArrayT
open Rhino.Scripting
open Rhino.Scripting.FSharp

type rs = RhinoScriptSyntax

/// Checks if the axes of both boxes are aligned, meaning they are parallel and have the same orientation
let areBoxesAligned (a:Box) (b:Box) : bool =
    Vec.isParallelAndOrientedTo Cosine.``0.025`` a.Xaxis b.Xaxis &&
    Vec.isParallelAndOrientedTo Cosine.``0.025`` a.Yaxis b.Yaxis &&
    Vec.isParallelAndOrientedTo Cosine.``0.025`` a.Zaxis b.Zaxis

/// Gets the space between two boxes, assuming they are aligned.
/// The resulting box is touching both input boxes and has the same orientation as them.
/// Fails if no such box can be found or if the boxes are not aligned.
let getBoxBetween (a:Box) (b:Box) : Box option =
    if not (areBoxesAligned a b) then
        failwith $"getBoxBetween: Boxes are not aligned:\n%A{a}\n%A{b}"
    let orig = a.Origin
    let v = b.Origin - orig
    let lenX = a.SizeX
    let lenY = a.SizeY
    let lenZ = a.SizeZ
    let ux = a.Xaxis/lenX
    let uy = a.Yaxis/lenY
    let uz = a.Zaxis/lenZ

    let dx = v.Dot ux
    let dy = v.Dot uy
    let dz = v.Dot uz

    let fv = b.FarCorner - orig
    let fdx = fv.Dot ux
    let fdy = fv.Dot uy
    let fdz = fv.Dot uz

    let mutable result = None

    // check if Z Stacking
    // if Boxes have no overlap when projected on their XY plane
    if dx < lenX && dy < lenY  // Box B Origin  is not after Box A in X or Y direction
        && fdx > 0. && fdy > 0. then // Box B Far Corner is not before Box A in X or Y direction
            // check if Box B is above Box A
            if dz > lenZ then // Box B  is above
                let insetX = max 0.0 dx
                let insetY = max 0.0 dy
                let no = orig + ux * insetX + uy * insetY + uz * lenZ // new Origin on top of Box A
                let endX = min lenX fdx
                let endY = min lenY fdy
                let nx = ux * (endX - insetX)
                let ny = uy * (endY - insetY)
                let nz = uz * (dz - lenZ)
                result <- Some <| Box.createUncheckedVec (no, nx, ny, nz)
            elif fdz < 0. then // Box B is below Box A
                let insetX = max 0.0 dx
                let insetY = max 0.0 dy
                let no = orig + ux * insetX + uy * insetY + uz * fdz // new Origin on top of Box B
                let endX = min lenX fdx
                let endY = min lenY fdy
                let nx = ux * (endX - insetX)
                let ny = uy * (endY - insetY)
                let nz = uz * -fdz
                result <- Some <| Box.createUncheckedVec (no, nx, ny, nz)

    // Check if X Stacking
    if result.IsNone then
        if dz < lenZ && dy < lenY  // Box B Origin  is not after Box A in Z or Y direction
            && fdz > 0. && fdy > 0. then // Box B Far Corner is not before Box A in Z or Y direction
                // check if Box B is right of Box A
                if dx > lenX then // Box B  is right of Box A
                    let insetZ = max 0.0 dz
                    let insetY = max 0.0 dy
                    let no = orig + uz * insetZ + uy * insetY + ux * lenX // new Origin on right of Box A
                    let endZ = min lenZ fdz
                    let endY = min lenY fdy
                    let nz = uz * (endZ - insetZ)
                    let ny = uy * (endY - insetY)
                    let nx = ux * (dx - lenX)
                    result <- Some <| Box.createUncheckedVec (no, nx, ny, nz)
                elif fdx < 0. then // Box B is left of Box A
                    let insetZ = max 0.0 dz
                    let insetY = max 0.0 dy
                    let no = orig + uz * insetZ + uy * insetY + ux * fdx // new Origin on left of Box B
                    let endZ = min lenZ fdz
                    let endY = min lenY fdy
                    let nz = uz * (endZ - insetZ)
                    let ny = uy * (endY - insetY)
                    let nx = ux * -fdx
                    result <- Some <| Box.createUncheckedVec (no, nx, ny, nz)

    // Check if Y Stacking
    if result.IsNone then
        if dz < lenZ && dx < lenX  // Box B Origin  is not after Box A in Z or X direction
            && fdz > 0. && fdx > 0. then // Box B Far Corner is not before Box A in Z or X direction
                // check if Box B is in front of Box A
                if dy > lenY then // Box B  is in front of Box A
                    let insetZ = max 0.0 dz
                    let insetX = max 0.0 dx
                    let no = orig + uz * insetZ + ux * insetX + uy * lenY // new Origin on front of Box A
                    let endZ = min lenZ fdz
                    let endX = min lenX fdx
                    let nz = uz * (endZ - insetZ)
                    let nx = ux * (endX - insetX)
                    let ny = uy * (dy - lenY)
                    result <- Some <| Box.createUncheckedVec (no, nx, ny, nz)
                elif fdy < 0. then // Box B is behind Box A
                    let insetZ = max 0.0 dz
                    let insetX = max 0.0 dx
                    let no = orig + uz * insetZ + ux * insetX + uy * fdy // new Origin on back of Box B
                    let endZ = min lenZ fdz
                    let endX = min lenX fdx
                    let nz = uz * (endZ - insetZ)
                    let nx = ux * (endX - insetX)
                    let ny = uy * -fdy
                    result <- Some <| Box.createUncheckedVec (no, nx, ny, nz)
    result




let r = Random()

let u() = UnitVec.create(r.NextDouble() , r.NextDouble() , r.NextDouble())


let b (o: Pnt)=
    let x = u()
    let y = u()
    let pl = PPlane.createOriginXaxisYaxis(o, x, y)
    Box.createFromPlane (r.NextDouble())  (r.NextDouble()) (r.NextDouble())  pl

rs.DeleteObjects(rs.AllObjects() )
rs.DisableRedraw()
for i = 0 to 100 do
    let o = Pnt.Origin.WithX(3. * float (i%10) ).WithY(3. * float (i/10) )
    let a = b o
    let v = u()
    let b = a |> Box.translate v.AsVec
    match getBoxBetween a b with 
    |Some x ->  
        rs.Ot.AddBox x.RhBox  |> rs.setLayer "res" 
        rs.Ot.AddBox a.RhBox  |> rs.setLayer "found"
        rs.Ot.AddBox b.RhBox  |> rs.setLayer "found"
    |None ->  
        rs.Ot.AddBox a.RhBox  |> rs.setLayer "not found"
        rs.Ot.AddBox b.RhBox  |> rs.setLayer "not found"
















