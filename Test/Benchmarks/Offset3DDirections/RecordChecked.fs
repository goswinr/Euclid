namespace Euclid.Benchmarks

open System
open Euclid
open Euclid.UtilEuclid
open Euclid.EuclidErrors
open Euclid.EuclidCollectionUtilities
open Euclid.Offset3D
open Euclid.Benchmarks.Shared

/// Variant A0: Offset3D as it was before it used ResizeArr.getIdx, with the shipped OffsetDirection class record.
/// Every read of the coordinates, segment vectors and directions goes through the ResizeArray indexer.
module RecordChecked =
    open Euclid.Benchmarks.Checked

    let tryDirectionFromPerpendicular (px:float) (py:float) (pz:float) (uPrev:UnitVec) (uNext:UnitVec) (ptIdx:int) : OffsetDirection voption =
        let tx = uPrev.X + uNext.X // the tangent, not unitized, its length is almost 2.0 because U-turns are excluded
        let ty = uPrev.Y + uNext.Y
        let tz = uPrev.Z + uNext.Z
        let tLenSq = tx * tx + ty * ty + tz * tz
        if isTooTinySq tLenSq then
            fail $"Offset3D.getOffsetDirections: the segments before and after pts.[{ptIdx}] are opposite: {uPrev} and {uNext}."
        let k = (px * tx + py * ty + pz * tz) / tLenSq
        let qx = px - tx * k
        let qy = py - ty * k
        let qz = pz - tz * k
        let len = sqrt (qx * qx + qy * qy + qz * qz)
        if isTooSmall len then
            ValueNone
        else
            let p = UnitVec.createUnchecked(qx / len, qy / len, qz / len)
            ValueSome { PreviousInPlane = UnitVec.cross(p, uPrev) |> Vec.unitize; NextInPlane = UnitVec.cross(p, uNext) |> Vec.unitize; Perpendicular = p }

    let getOffsetDirections(xyzs: ResizeArray<float>, uvs: ResizeArray<float>, refNormal:UnitVec, considerCollinearBelow: float<Cosine.cosine>, failAtUTurnAbove :float<Cosine.cosine>) : ResizeArray<OffsetDirection> =
        XYZ.checkMod3 "getOffsetDirections" xyzs
        XYZ.checkMod3 "getOffsetDirections segment vectors" uvs
        let ptCount = XYZ.pointCount xyzs
        let segCount = XYZ.pointCount uvs
        if ptCount < 2 then
            fail $"Offset3D.getOffsetDirections: point count {ptCount} must be at least 2 for a polyline."
        if ptCount <> segCount + 1 then
            fail $"Offset3D.getOffsetDirections: point count must be one greater than segment vector count but they are {ptCount} and {segCount}."
        let isClosed = XYZ.sqDistFirstLast xyzs < openToleranceSq
        let count = if isClosed then ptCount - 1 else ptCount // the points with their own direction, the last point of a closed polyline is the first
        let lastSeg = segCount - 1
        // the segments before and after a point, the end points of an open polyline use their own segment twice
        let inline prevSeg i = if i > 0 then i - 1 elif isClosed then lastSeg else 0
        let inline nextSeg i = min i lastSeg

        // (1) check for U-turns at the points with two segments
        let firstInner = if isClosed then 0 else 1
        let lastInner = if isClosed then count - 1 else count - 2
        for i = firstInner to lastInner do
            let dot = XYZ.dot (prevSeg i) i uvs
            if withMeasure dot < failAtUTurnAbove then
                fail $"Offset3D.getOffsetDirections: {Cosine.inDegrees (withMeasure dot)} degree U-turn detected at pts.[{i}]. exceeding failAtUTurnAbove: {Cosine.inDegrees failAtUTurnAbove}."

        // (2) find the corners and their normals, not oriented yet
        let isCorner = Array.zeroCreate<bool> count
        let normals = Array.create count UnitVec.ZAxis // dummy values for the points that are not corners
        let inline isLocalCorner i = withMeasure (XYZ.dot (prevSeg i) i uvs) <= considerCollinearBelow
        // makes point i a corner, unless its segments are parallel, then their plane is unknown
        let tryMarkCorner i : bool =
            let a = XYZ.getUnitVec (prevSeg i) uvs
            let b = XYZ.getUnitVec i uvs
            let x = a.Y * b.Z - a.Z * b.Y
            let y = a.Z * b.X - a.X * b.Z
            let z = a.X * b.Y - a.Y * b.X
            let len = sqrt (x * x + y * y + z * z)
            if isTooSmall len then
                if a *** b < 0.0 then // only possible with extreme thresholds
                    fail $"Offset3D.getOffsetDirections: 180 degree U-turn detected at pts.[{i}]."
                false
            else
                isCorner.[i] <- true
                normals.[i] <- UnitVec.createUnchecked(x / len, y / len, z / len)
                true

        // the scan for corners starts at a corner, so that a closed polyline gives the same corners for any start point
        let mutable start = 0
        let mutable steps = count - 2 // for an open polyline, from point 1 to the second last point
        if isClosed then
            steps <- count - 1
            start <- -1
            let mutable i = 0
            while start = -1 && i < count do
                if isLocalCorner i && tryMarkCorner i then start <- i
                i <- i + 1
            if start = -1 then
                // no point turns by considerCollinearBelow, like on a finely sampled circle:
                // start at the first of the points that turn the most, so that rounding does not decide
                let mutable minDot = 2.0
                for j = 0 to count - 1 do
                    minDot <- min minDot (XYZ.dot (prevSeg j) j uvs)
                let mutable j = 0
                while start = -1 do
                    if isTooSmall (XYZ.dot (prevSeg j) j uvs - minDot) then start <- j
                    j <- j + 1
                if not (tryMarkCorner start) then
                    steps <- 0 // no point turns at all, all directions come from refNormal below
        let mutable runSeg = nextSeg start // the first segment after the previous corner
        for k = 1 to steps do
            let i = (start + k) % count
            if isLocalCorner i || withMeasure (XYZ.dot runSeg i uvs) <= considerCollinearBelow then
                if tryMarkCorner i then
                    runSeg <- i

        let dirs = ResizeArray<OffsetDirection>(ptCount)
        if not (Array.contains true isCorner) then
            // (3) no corner, like on a straight polyline: derive every direction from refNormal
            for i = 0 to count - 1 do
                match tryDirectionFromPerpendicular refNormal.X refNormal.Y refNormal.Z (XYZ.getUnitVec (prevSeg i) uvs) (XYZ.getUnitVec (nextSeg i) uvs) i with
                | ValueSome d -> dirs.Add d
                | ValueNone -> fail $"Offset3D.getOffsetDirections: the reference normal {refNormal} is parallel to the polyline at pts.[{i}], so it cannot define the offset plane there."
        else
            // (4) orient the corner normals, starting at the one most aligned with refNormal
            let perps = normals // oriented in place
            let mutable best = -1.0
            for i = 0 to count - 1 do
                if isCorner.[i] then
                    best <- max best (abs (normals.[i] *** refNormal))
            // the first of the corners that are equally aligned, so that rounding does not decide
            let mutable anchor = -1
            let mutable i = 0
            while anchor = -1 do
                if isCorner.[i] && isTooSmall (best - abs (normals.[i] *** refNormal)) then
                    anchor <- i
                i <- i + 1
            if withMeasure best < Cosine.``89.5`` then
                fail $"Offset3D.getOffsetDirections: the reference normal {refNormal} is perpendicular to the plane of every corner, so it cannot orient the offset. The best corner at pts.[{anchor}] has the normal {normals.[anchor]}."
            perps.[anchor] <- UnitVec.matchOrientation refNormal normals.[anchor]

            // Orients the corner normal at i from the oriented corner p before it.
            // If they are almost perpendicular, the side is given by the in-plane direction of p on the segment towards i.
            let orientFromPrevious (p:int) (i:int) : UnitVec =
                let n = normals.[i]
                let d = perps.[p] *** n
                if withMeasure (abs d) < Cosine.``89.5`` then
                    let nNextP = crossProduct(perps.[p], XYZ.getUnitVec (nextSeg p) uvs)
                    if nNextP *** n < 0.0 then -n else n
                elif d < 0.0 then -n else n

            // Orients the corner normal at i from the oriented corner q after it, with the same result as orientFromPrevious from i to q.
            let orientFromNext (q:int) (i:int) : UnitVec =
                let n = normals.[i]
                let d = perps.[q] *** n
                if withMeasure (abs d) < Cosine.``89.5`` then
                    let nPrevQ = crossProduct(perps.[q], XYZ.getUnitVec (prevSeg q) uvs)
                    if nPrevQ *** n > 0.0 then -n else n
                elif d < 0.0 then -n else n

            let mutable p = anchor
            if isClosed then
                for k = 1 to count - 1 do
                    let i = (anchor + k) % count
                    if isCorner.[i] then
                        perps.[i] <- orientFromPrevious p i
                        p <- i
            else
                for i = anchor + 1 to count - 1 do
                    if isCorner.[i] then
                        perps.[i] <- orientFromPrevious p i
                        p <- i
                p <- anchor
                for i = anchor - 1 downto 0 do
                    if isCorner.[i] then
                        perps.[i] <- orientFromNext p i
                        p <- i

            // (5) the nearest corners before and after each point, around the seam of a closed polyline
            let prevCorner = Array.create count -1
            let nextCorner = Array.create count -1
            let mutable c = -1
            if isClosed then
                let mutable i = count - 1
                while c = -1 do // there is a corner
                    if isCorner.[i] then c <- i
                    i <- i - 1
            for i = 0 to count - 1 do
                if isCorner.[i] then c <- i else prevCorner.[i] <- c
            c <- -1
            if isClosed then
                let mutable i = 0
                while c = -1 do
                    if isCorner.[i] then c <- i
                    i <- i + 1
            for i = count - 1 downto 0 do
                if isCorner.[i] then c <- i else nextCorner.[i] <- c

            // (6) the directions
            for i = 0 to count - 1 do
                let uPrev = XYZ.getUnitVec (prevSeg i) uvs
                let uNext = XYZ.getUnitVec (nextSeg i) uvs
                if isCorner.[i] then
                    let p = perps.[i]
                    dirs.Add { PreviousInPlane = crossProduct(p, uPrev); NextInPlane = crossProduct(p, uNext); Perpendicular = p }
                else
                    // interpolate the normals of the neighboring corners, or take the one of the only neighboring corner
                    let a = if prevCorner.[i] = -1 then nextCorner.[i] else prevCorner.[i]
                    let b = if nextCorner.[i] = -1 then prevCorner.[i] else nextCorner.[i]
                    let pa = perps.[a]
                    let pb = perps.[b]
                    let t =
                        if a = b || isTooSmallSq (XYZ.sqDist a b xyzs) then 0.5
                        else closestParameter a b i xyzs |> clamp01
                    let px = pa.X + (pb.X - pa.X) * t
                    let py = pa.Y + (pb.Y - pa.Y) * t
                    let pz = pa.Z + (pb.Z - pa.Z) * t
                    match tryDirectionFromPerpendicular px py pz uPrev uNext i with
                    | ValueSome d -> dirs.Add d
                    | ValueNone -> fail $"Offset3D.getOffsetDirections: the normals {pa} and {pb} of the corners at pts.[{a}] and pts.[{b}] cancel out or are parallel to the polyline at pts.[{i}] between them."

        if isClosed then
            dirs.Add dirs.[0] // the last point is the first point
        dirs

    let offsetWithDirections(xyzs:ResizeArray<float>, dirs: ResizeArray<OffsetDirection>, distanceInPlane:float, distancePerpendicular:float) : ResizeArray<float> =
        XYZ.checkMod3 "offsetWithDirections" xyzs
        let ptCount = XYZ.pointCount xyzs
        if ptCount < 2 then
            fail $"Offset3D.offsetWithDirections: point count {ptCount} must be at least 2 for a polyline."
        if ptCount <> dirs.Count then
            fail $"Offset3D.offsetWithDirections: point count must equal offset directions count, but they are {ptCount} and {dirs.Count}."
        let isClosed = XYZ.sqDistFirstLast xyzs < openToleranceSq
        let res = ResizeArray<float>(xyzs.Count)
        let lastPt = if isClosed then ptCount - 2 else ptCount - 1
        for i = 0 to lastPt do
            let dir = dirs.[i]
            let cosine = dir.PreviousInPlane *** dir.NextInPlane
            setOffCorner(res, XYZ.getX i xyzs, XYZ.getY i xyzs, XYZ.getZ i xyzs, distanceInPlane, distancePerpendicular, dir.PreviousInPlane, dir.NextInPlane, dir.Perpendicular, cosine)
        if isClosed then
            XYZ.add res.[0] res.[1] res.[2] res // the last point is the first point
        res

    let offsetVariableWithDirections(xyzs:ResizeArray<float>,
                                     uvs:ResizeArray<float>,
                                     dirs: ResizeArray<OffsetDirection>,
                                     distancesInPlane:Collections.Generic.IList<float>,
                                     distancesPerpendicular:Collections.Generic.IList<float>,
                                     varDistParallelBehavior: VarDistParallel,
                                     considerCollinearBelow: float<Cosine.cosine>
                                     ) : ResizeArray<float> =
        XYZ.checkMod3 "offsetVariableWithDirections" xyzs
        XYZ.checkMod3 "offsetVariableWithDirections segment vectors" uvs
        let ptCount = XYZ.pointCount xyzs
        let segCount = XYZ.pointCount uvs
        if ptCount < 2 then
            fail $"Offset3D.offsetVariableWithDirections: point count {ptCount} must be at least 2 for a polyline."

        if ptCount <> segCount + 1  then
            fail $"Offset3D.offsetVariableWithDirections:\n Segment directions count must be {ptCount-1} for {ptCount} points, but is {segCount}."

        if ptCount <> dirs.Count then
            fail $"Offset3D.offsetVariableWithDirections:\n offset directions count must be {ptCount} for {ptCount} points, but is {dirs.Count}."

        if ptCount <> distancesInPlane.Count + 1  then
            fail $"Offset3D.offsetVariableWithDirections:\n in-plane distances count must be {ptCount-1} for {ptCount} points, but is {distancesInPlane.Count}."

        let isClosed = XYZ.sqDistFirstLast xyzs < openToleranceSq
        if isClosed then
            if ptCount <> distancesPerpendicular.Count + 1 then
                fail $"Offset3D.offsetVariableWithDirections:\n perpendicular distances count must be {ptCount-1} for the {ptCount} points of a closed polyline, one per point except the last, which is the first, but is {distancesPerpendicular.Count}."
        elif ptCount <> distancesPerpendicular.Count then
            fail $"Offset3D.offsetVariableWithDirections:\n perpendicular distances count must be {ptCount} for the {ptCount} points of an open polyline, one per point, but is {distancesPerpendicular.Count}."

        let lastSeg = segCount - 1
        let count = if isClosed then ptCount - 1 else ptCount // the points to offset, the last point of a closed polyline is the first

        // the segments before and after a point, the end points of an open polyline use their own segment twice, and so their distances
        let inline prevSeg i = if i > 0 then i - 1 elif isClosed then lastSeg else 0
        let inline nextSeg i = min i lastSeg
        let inline prevDistAt i = distancesInPlane.[prevSeg i]
        let inline nextDistAt i = distancesInPlane.[nextSeg i]
        let inline isSameDist i = abs (prevDistAt i - nextDistAt i) < 1e-6
        // the in-plane offset lines of the two segments meet at a finite point
        let inline linesMeet i = not (isTooSmall (abs (dirs.[i].PreviousInPlane *** XYZ.getUnitVec (nextSeg i) uvs)))
        // the point is offset exactly no matter where the scan starts
        let isExact i =
            (not isClosed && (i = 0 || i = ptCount - 1))
            || isSameDist i
            || (withMeasure (XYZ.dot (prevSeg i) (nextSeg i) uvs) <= considerCollinearBelow && linesMeet i)

        // (1) find the collinear points with different distances, that need VarDistParallel.
        // The scan starts at a point that is offset exactly, so that a closed polyline gives the same points for any start point.
        let needsFix = Array.zeroCreate<bool> ptCount
        let mutable fixCount = 0
        let mutable start = 0
        if isClosed then
            start <- -1
            let mutable i = 0
            while start = -1 && i < count do
                if isExact i then start <- i
                i <- i + 1
            if start = -1 then
                // every point is collinear with different distances, like on a finely sampled circle:
                // start at the first of the points where the offset lines meet at the biggest angle, so that rounding does not decide
                let inline sineAt j = abs (dirs.[j].PreviousInPlane *** XYZ.getUnitVec (nextSeg j) uvs)
                let mutable best = 0.0
                for j = 0 to count - 1 do
                    best <- max best (sineAt j)
                if isTooSmall best then
                    fail $"Offset3D.offsetVariableWithDirections: no point of the closed polyline with {ptCount} points turns, so no point can be offset where the offset lines of different distances meet."
                let mutable j = 0
                while start = -1 do
                    if isTooSmall (best - sineAt j) then start <- j
                    j <- j + 1
        let steps = if isClosed then count - 1 else count - 2
        let mutable runSeg = nextSeg start // the first segment after the last point that is offset exactly
        for k = 1 to steps do
            let i = (start + k) % count
            if isExact i || (withMeasure (XYZ.dot runSeg (nextSeg i) uvs) <= considerCollinearBelow && linesMeet i) then
                runSeg <- nextSeg i
            else
                needsFix.[i] <- true
                fixCount <- fixCount + 1
        if isClosed then
            needsFix.[ptCount - 1] <- needsFix.[0]

        // (2) first pass: the in-plane offsets of the points that are offset exactly.
        // Their perpendicular offsets are added in the third pass, so that the second pass can place the collinear points in-plane.
        let mutable res = ResizeArray<float>(xyzs.Count)
        for i = 0 to count - 1 do
            let x = XYZ.getX i xyzs
            let y = XYZ.getY i xyzs
            let z = XYZ.getZ i xyzs
            if needsFix.[i] then
                XYZ.add x y z res // for now just keep the input point, it is placed in the second pass
            else
                let dir = dirs.[i]
                let prevDist = prevDistAt i
                let nextDist = nextDistAt i
                let cosine = dir.PreviousInPlane *** dir.NextInPlane
                if abs (prevDist - nextDist) < 1e-6 then
                    setOffCorner(res, x, y, z, nextDist, 0.0, dir.PreviousInPlane, dir.NextInPlane, dir.Perpendicular, cosine)
                else
                    setOffCornerVar(res, x, y, z, prevDist, nextDist, 0.0, dir.PreviousInPlane, dir.NextInPlane, dir.Perpendicular, cosine, XYZ.getUnitVec (nextSeg i) uvs)
        if isClosed then
            XYZ.add res.[0] res.[1] res.[2] res // the last point is the first point, a collinear one is placed in the second pass

        // (3) second pass: place the collinear points in-plane on the line between the neighboring exact points,
        // then offset them perpendicularly by their own distance
        let skip = if fixCount > 0 && varDistParallelBehavior = VarDistParallel.Skip then Array.zeroCreate<bool> ptCount else [||]
        if fixCount > 0 then
            let collinearPoints = getNeighborsOfMarked needsFix
            for j = 0 to collinearPoints.LastIndex do
                let cP = collinearPoints.[j]
                let i = cP.Index
                if not (isClosed && i = ptCount - 1) then // the last point of a closed polyline is copied from the first one below
                    match varDistParallelBehavior with
                    | VarDistParallel.Fail ->
                        fail $"Offset3D.offsetVariableWithDirections: point[{i}] is between collinear segments with the different in-plane distances {prevDistAt i} and {nextDistAt i}. Use another VarDistParallel behavior than Fail."

                    | VarDistParallel.Skip ->
                        skip.[i] <- true

                    | VarDistParallel.Proportional
                    | VarDistParallel.Project ->
                        if isTooSmallSq (XYZ.sqDist cP.PreviousOK cP.NextOK xyzs) then
                            fail $"Offset3D.offsetVariableWithDirections: point[{i}] is between collinear segments with the different in-plane distances {prevDistAt i} and {nextDistAt i}, but the only point that can be offset exactly is point[{cP.PreviousOK}], so VarDistParallel.{varDistParallelBehavior} has no line to place it on."
                        if varDistParallelBehavior = VarDistParallel.Proportional then
                            // at the same parameter as on the line between the neighboring input points
                            let t = closestParameter cP.PreviousOK cP.NextOK i xyzs
                            setLinePointAt i cP.PreviousOK cP.NextOK t res res
                        else
                            // where the line between the neighboring offset points crosses the plane through the input point perpendicular to the tangent,
                            // so for planar polylines along the in-plane normal, like in Offset2D
                            let a = prevSeg i
                            let tx = XYZ.getX a uvs + XYZ.getX i uvs
                            let ty = XYZ.getY a uvs + XYZ.getY i uvs
                            let tz = XYZ.getZ a uvs + XYZ.getZ i uvs
                            intersectLineWithNormalPlaneInPlace i cP.PreviousOK cP.NextOK tx ty tz xyzs res

                    |_  ->
                        fail $"Offset3D.offsetVariableWithDirections: unknown VarDistParallel behavior {varDistParallelBehavior}."

                    // (3.1) the perpendicular offset by the point's own distance, skipped points need none
                    let distPerp = distancesPerpendicular.[i]
                    if distPerp <> 0.0 && not (skip.Length > 0 && skip.[i]) then
                        let p = dirs.[i].Perpendicular
                        XYZ.set i (XYZ.getX i res + p.X * distPerp) (XYZ.getY i res + p.Y * distPerp) (XYZ.getZ i res + p.Z * distPerp) res

        // (4) third pass: the perpendicular offsets of the points that are offset exactly
        for i = 0 to count - 1 do
            if not needsFix.[i] then
                let p = dirs.[i].Perpendicular
                let distPerp = distancesPerpendicular.[i]
                XYZ.set i (XYZ.getX i res + p.X * distPerp) (XYZ.getY i res + p.Y * distPerp) (XYZ.getZ i res + p.Z * distPerp) res

        if isClosed then
            XYZ.set (ptCount - 1) res.[0] res.[1] res.[2] res

        if skip.Length > 0 then
            if isClosed then
                skip.[ptCount - 1] <- skip.[0]
            let newRes = ResizeArray<float>(res.Count)
            for i = 0 to ptCount - 1 do
                if not skip.[i] then
                    XYZ.add (XYZ.getX i res) (XYZ.getY i res) (XYZ.getZ i res) newRes
            // if the seam point was skipped, close the polyline again with the new first point
            if isClosed && skip.[0] then
                XYZ.add newRes.[0] newRes.[1] newRes.[2] newRes
            res <- newRes

        res

