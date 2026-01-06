namespace Euclid


/// used in Polyline2D.FindLablePoint
module internal Polylabel =

    // see https://github.com/mapbox/polylabel


    // Cell represents a grid cell with its properties
    type Cell = {
        X: float           // Cell center X
        Y: float           // Cell center Y
        H: float           // Cell half-size
        Distance: float    // Distance to polygon edge (negative if outside)
        MaxDistance: float // Maximum possible distance within this cell
    }

    // Simple binary max-heap over ResizeArray<Cell>
    type CellHeap() =
        let data = ResizeArray<Cell>()

        // Compare with tie-break on coordinates to avoid treating equal MaxDistance cells as identical
        static member inline better (a: Cell) (b: Cell) =
            if a.MaxDistance = b.MaxDistance then
                // Arbitrary stable-ish tie-break: X then Y then H
                if a.X = b.X then
                    if a.Y = b.Y then a.H > b.H else a.Y > b.Y
                else a.X > b.X
            else a.MaxDistance > b.MaxDistance

        member _.Count = data.Count

        member _.Add (c: Cell) =
            data.Add c
            // bubble up
            let mutable i = data.Count - 1
            while i > 0 do
                let parent = (i - 1) / 2
                if CellHeap.better data.[i] data.[parent] then
                    let tmp = data.[parent]
                    data.[parent] <- data.[i]
                    data.[i] <- tmp
                    i <- parent
                else
                    i <- 0

        member _.Pop () =
            if data.Count = 0 then EuclidErrors.fail "Polylabel: CellHeap empty"
            let root = data.[0]
            let lastIdx = data.Count - 1
            data.[0] <- data.[lastIdx]
            data.RemoveAt lastIdx
            // heapify down
            let mutable i = 0
            let n = data.Count
            let mutable cont = true
            while cont do
                let l = 2 * i + 1
                let r = l + 1
                if l >= n then
                    cont <- false
                else
                    let mutable bestChild = l
                    if r < n && CellHeap.better data.[r] data.[l] then bestChild <- r
                    if CellHeap.better data.[bestChild] data.[i] then
                        let tmp = data.[i]
                        data.[i] <- data.[bestChild]
                        data.[bestChild] <- tmp
                        i <- bestChild
                    else
                        cont <- false
            root

    // the actual Polylabel algorithm is in Polyline2D.fs: FindLabelPoint()