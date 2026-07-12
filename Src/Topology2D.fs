namespace Euclid
open Euclid.EuclidCollectionUtilities

#nowarn "52" // copying of structs


module internal TopologyUtil =

    /// Swap the values of two given indices in Rarr
    let inline swap i j (xs:ResizeArray<'T>) :unit =
        if i<>j then
            let ti = xs.[i]
            xs.[i] <- xs.[j]
            xs.[j] <- ti


    /// Like minIndexBy. But starting to search only from a given index
    let inline minIndexByFrom (compareBy: 'T -> 'U)  fromIdx (ts:ResizeArray<'T>) : int =
        let mutable idx = fromIdx
        let mutable smallest = compareBy ts.[fromIdx]
        let lastIdx = ts.LastIndex
        for j = fromIdx + 1 to lastIdx do
            let current = compareBy ts.[j]
            if current < smallest then
                idx <- j
                smallest <- current
        idx


    [<NoComparison>]
    type LoopCollector<'T> =
        {
        forward: ResizeArray<'T>
        backward: ResizeArray<'T>
        }


    /// swap elements in ts
    let inline swapT (a: int) (nextIdx: int) (ts:ResizeArray<'T>)=
        let ti = ts.[a]
        ts.[a] <- ts.[nextIdx]
        ts.[nextIdx] <- ti


    /// swap the corresponding 4-float blocks in xyxys to keep it parallel to xs
    let inline swap4 (a: int) (nextIdx: int) (xyxys: float[])  =
        let ai = a * 4
        let bi = nextIdx * 4
        let t0 = xyxys.[ai    ]
        let t1 = xyxys.[ai + 1]
        let t2 = xyxys.[ai + 2]
        let t3 = xyxys.[ai + 3]
        xyxys.[ai    ] <- xyxys.[bi    ]
        xyxys.[ai + 1] <- xyxys.[bi + 1]
        xyxys.[ai + 2] <- xyxys.[bi + 2]
        xyxys.[ai + 3] <- xyxys.[bi + 3]
        xyxys.[bi    ] <- t0
        xyxys.[bi + 1] <- t1
        xyxys.[bi + 2] <- t2
        xyxys.[bi + 3] <- t3


    /// swap the corresponding 6-float blocks in xyzxyzs to keep it parallel to xs
    let inline swap6 (a: int) (nextIdx: int) (xyzxyzs: float[])  =
        let ai = a * 6
        let bi = nextIdx * 6
        let t0 = xyzxyzs.[ai    ]
        let t1 = xyzxyzs.[ai + 1]
        let t2 = xyzxyzs.[ai + 2]
        let t3 = xyzxyzs.[ai + 3]
        let t4 = xyzxyzs.[ai + 4]
        let t5 = xyzxyzs.[ai + 5]
        xyzxyzs.[ai    ] <- xyzxyzs.[bi    ]
        xyzxyzs.[ai + 1] <- xyzxyzs.[bi + 1]
        xyzxyzs.[ai + 2] <- xyzxyzs.[bi + 2]
        xyzxyzs.[ai + 3] <- xyzxyzs.[bi + 3]
        xyzxyzs.[ai + 4] <- xyzxyzs.[bi + 4]
        xyzxyzs.[ai + 5] <- xyzxyzs.[bi + 5]
        xyzxyzs.[bi    ] <- t0
        xyzxyzs.[bi + 1] <- t1
        xyzxyzs.[bi + 2] <- t2
        xyzxyzs.[bi + 3] <- t3
        xyzxyzs.[bi + 4] <- t4
        xyzxyzs.[bi + 5] <- t5

    let inline sqDist2 (x1:float) (y1:float) (x2:float) (y2:float) : float =
        let dx = x2 - x1
        let dy = y2 - y1
        dx*dx + dy*dy

    let inline sqDist3 (x1:float) (y1:float) (z1:float) (x2:float) (y2:float) (z2:float) : float =
        let dx = x2 - x1
        let dy = y2 - y1
        let dz = z2 - z1
        dx*dx + dy*dy + dz*dz


open TopologyUtil

/// A type containing only static member functions for 2D topological operations.
type Topology2D  =

    /// <summary>Sorts elements in place to be in a circular structure.
    /// This does NOT recognize if there are actually several loops.
    /// Use Topology2D.join for this instead.</summary>
    /// <param name="getLine">A function that computes a Line2D from an element of type 'T.
    /// The Line2D is used as an abstraction to hold start and end of arbitrary object.
    /// For each line end point it finds the next closest line start point.</param>
    /// <param name="ts">A ResizeArray of elements of type 'T' to be sorted in place.</param>
    /// <returns>The input array 'ts' is modified in place to be sorted in a circular structure.</returns>
    static member sortToLoop(getLine: 'T -> Line2D, ts:ResizeArray<'T>) : unit =
        let count = ts.Count
        for i = 0 to count - 2 do // only run till second last
            let thisLine = getLine ts.[i]
            let tx = thisLine.ToX
            let ty = thisLine.ToY
            // TODO could be optimized using a R-Tree for very large lists instead of minBy function
            let minIdx = ts |> minIndexByFrom (fun c -> let l = getLine c in sqDist2 l.FromX l.FromY tx ty) (i+1)
            ts |> swap (i+1) minIdx


    /// <summary>Sorts elements in place to be in a circular structure.
    /// Does the same thing as sortToLoop but caches the line coordinates in a float array
    /// to avoid repeated getLine calls inside the inner loop.
    /// This runs slightly faster when getLine is expensive. (The benefit only shows in .NET not in Fable JS)
    /// This does NOT recognize if there are actually several loops.
    /// Use Topology2D.join for this instead.</summary>
    /// <param name="getLine">A function that computes a Line2D from an element of type 'T.
    /// The Line2D is used as an abstraction to hold start and end of arbitrary object.
    /// For each line end point it finds the next closest line start point.</param>
    /// <param name="ts">A ResizeArray of elements of type 'T' to be sorted in place.</param>
    /// <returns>The input array 'ts' is modified in place to be sorted in a circular structure.
    /// But also returns a sorted flat array of the 2d lines used internally [FromX; FromY; ToX; ToY].</returns>
    static member sortToLoopCached(getLine: 'T -> Line2D, ts:ResizeArray<'T>) : float[] =
        // see Test\Benchmarks\BenchmarkTopology2DSortToLoopWithReversing.fsx
        // and Test\Benchmarks\BenchmarkTopology2DSortToLoopWithReversing.js
        let count = ts.Count

        // Flat array of [FromX; FromY; ToX; ToY] for each line, kept in sync with ts via swaps below.
        // Avoids constructing Pt structs from Line2D.From / Line2D.To inside the inner search loop,
        // and ensures getLine is invoked exactly once per element (not O(n) times as a naive minBy would).
        // Layout: line k occupies xyxys.[k*4 .. k*4+3] as FromX, FromY, ToX, ToY.
        let xyxys = Arr.undefCreateFloat64(count * 4)
        for k = 0 to count - 1 do
            let ln = getLine ts.[k]
            let k4 = k * 4
            xyxys.[k4    ] <- ln.FromX
            xyxys.[k4 + 1] <- ln.FromY
            xyxys.[k4 + 2] <- ln.ToX
            xyxys.[k4 + 3] <- ln.ToY

        for i = 0 to count - 2 do // only run till second last
            // current line's end point (To) — we are searching for the next line whose From is closest to this.
            let toX = xyxys.[i*4 + 2]
            let toY = xyxys.[i*4 + 3]
            //  TODO could be optimized using a R-Tree for very large lists instead of linear search
            // Seed the running minimum with the candidate at i+1, then scan i+2..count-1.
            let mutable nextIdx = i + 1
            let mutable dx = xyxys.[(i+1)*4    ] - toX
            let mutable dy = xyxys.[(i+1)*4 + 1] - toY
            let mutable smallest = dx*dx + dy*dy // squared distance, no sqrt needed for comparison
            for j = i + 2 to count - 1 do
                // distance from candidate j's From point to current To point
                let dx = xyxys.[j*4    ] - toX
                let dy = xyxys.[j*4 + 1] - toY
                let d  = dx*dx + dy*dy
                if d < smallest then
                    nextIdx <- j
                    smallest <- d
            // Move the chosen element into position i+1, swapping both xs and the parallel coords block.
            if nextIdx <> i + 1 then
                let a = i + 1
                swapT a nextIdx ts
                swap4 a nextIdx xyxys
        xyxys

    /// <summary>Sorts elements in place to be in a circular structure.
    /// For each line end it finds the next closest start point or end point.
    /// This does NOT recognize if there are actually several loops.
    /// Use Topology2D.joinReversing for this instead.</summary>
    /// <param name="getLine">A function that computes a Line2D from an element of type 'T.
    /// The Line2D is used as an abstraction to hold start and end of arbitrary object.
    /// For each line end it finds the next closest start point or end point.</param>
    /// <param name="reverseInPlace">A function that reverses an element in place given its index and the element itself.
    /// This function might just update an item at the given index in the array,
    /// or it might modify the element in place if 'T is a reference type.</param>
    /// <param name="ts">A ResizeArray of elements of type 'T' to be sorted in place.</param>
    /// <returns>The input array 'ts' is modified in place to be sorted in a circular structure.
    /// Entries whose element was reversed have their orientation flipped (start and end swapped) to reflect the new direction.</returns>
    static member sortToLoopWithReversing (getLine: 'T -> Line2D, reverseInPlace: int -> 'T -> unit, ts:ResizeArray<'T>) : unit =
        let count = ts.Count
        for i = 0 to count - 2 do // only run till second last
            let thisLine = getLine ts.[i]
            let tx = thisLine.ToX
            let ty = thisLine.ToY
            // TODO could be optimized using a R-Tree for very large lists instead of minBy function
            let minIdx = ts |> minIndexByFrom (fun c -> let l = getLine c in min(sqDist2 l.FromX l.FromY tx ty)(sqDist2 l.ToX l.ToY tx ty)) (i+1)
            // check if closest endpoint is closer than closest start-point
            let minLn = getLine ts.[minIdx]

            if sqDist2 minLn.FromX minLn.FromY tx ty > sqDist2 minLn.ToX minLn.ToY tx ty then
                reverseInPlace minIdx ts.[minIdx]
            ts |> swap (i+1) minIdx


    /// <summary>Sorts elements in place to be in a circular structure.
    /// Does the same thing as sortToLoopWithReversing but caches the line coordinates in a float array
    /// to avoid repeated getLine calls inside the inner loop.
    /// This runs slightly faster when getLine is expensive. (The benefit only shows in .NET not in Fable JS)
    /// For each line end it finds the next closest start point or end point.
    /// This does NOT recognize if there are actually several loops.
    /// Use Topology2D.joinReversing for this instead.</summary>
    /// <param name="getLine">A function that computes a Line2D from an element of type 'T.
    /// The Line2D is used as an abstraction to hold start and end of arbitrary object.
    /// For each line end it finds the next closest start point or end point.</param>
    /// <param name="reverseInPlace">A function that reverses an element in place given its index and the element itself.
    /// This function might just update an item at the given index in the array,
    /// or it might modify the element in place if 'T is a reference type.</param>
    /// <param name="ts">A ResizeArray of elements of type 'T' to be sorted in place.</param>
    /// <returns>The input array 'ts' is modified in place to be sorted in a circular structure.
    /// But also returns a sorted flat array of the 2d lines used internally [FromX; FromY; ToX; ToY].
    /// Entries whose element was reversed have their FromX/FromY and ToX/ToY swapped to reflect the new orientation.</returns>
    static member sortToLoopWithReversingCached (getLine: 'T -> Line2D, reverseInPlace: int -> 'T -> unit, ts:ResizeArray<'T>) : float[] =
        // see Test\Benchmarks\BenchmarkTopology2DSortToLoopWithReversing.fsx
        // and Test\Benchmarks\BenchmarkTopology2DSortToLoopWithReversing.js
        let count = ts.Count

        // Flat array of [FromX; FromY; ToX; ToY] for each line, kept in sync with ts through swaps and in-place reversals below.
        // Avoids constructing Pt structs from Line2D.From / Line2D.To inside the inner search loop,
        // and ensures getLine is invoked exactly once per element (not O(n) times as a naive minBy would).
        // Layout: line k occupies xyxys.[k*4 .. k*4+3] as FromX, FromY, ToX, ToY.
        let xyxys = Arr.undefCreateFloat64(count * 4)
        for k = 0 to count - 1 do
            let ln = getLine ts.[k]
            let k4 = k * 4
            xyxys.[k4    ] <- ln.FromX
            xyxys.[k4 + 1] <- ln.FromY
            xyxys.[k4 + 2] <- ln.ToX
            xyxys.[k4 + 3] <- ln.ToY

        for i = 0 to count - 2 do // only run till second last
            // current line's end point (To) — we are searching for the next line whose From OR To is closest to this.
            let toX = xyxys.[i*4 + 2]
            let toY = xyxys.[i*4 + 3]
            //  TODO could be optimized using a R-Tree for very large lists instead of linear search
            // Single pass over candidates: track the closest From-to-To distance and the closest To-to-To distance.
            // Seed both running minima with the candidate at i+1, then scan i+2..count-1.
            let i1 = (i + 1) * 4
            let mutable nextIdxSt = i + 1
            let mutable nextIdxEn = i + 1
            let mutable dxS = xyxys.[i1    ] - toX
            let mutable dyS = xyxys.[i1 + 1] - toY
            let mutable smallestSt = dxS*dxS + dyS*dyS // squared distance from candidate's From to current To
            let mutable dxE = xyxys.[i1 + 2] - toX
            let mutable dyE = xyxys.[i1 + 3] - toY
            let mutable smallestEn = dxE*dxE + dyE*dyE // squared distance from candidate's To to current To
            for j = i + 2 to count - 1 do
                let j4 = j * 4
                // candidate j's From to current To
                let dxs = xyxys.[j4    ] - toX
                let dys = xyxys.[j4 + 1] - toY
                let ds  = dxs*dxs + dys*dys
                if ds < smallestSt then
                    nextIdxSt <- j
                    smallestSt <- ds
                // candidate j's To to current To (would require reversing the candidate)
                let dxe = xyxys.[j4 + 2] - toX
                let dye = xyxys.[j4 + 3] - toY
                let de  = dxe*dxe + dye*dye
                if de < smallestEn then
                    nextIdxEn <- j
                    smallestEn <- de
            // Prefer the no-reversal branch on ties (matches original `<=` semantics).
            if smallestSt <= smallestEn then
                // Move chosen element into position i+1 without reversal.
                if nextIdxSt <> i + 1 then
                    let a = i + 1
                    swapT a nextIdxSt ts
                    swap4 a nextIdxSt xyxys
            else
                // Reverse the chosen element in place so its old To becomes the new From.
                reverseInPlace nextIdxEn ts.[nextIdxEn]
                // Reflect the reversal in the parallel coords block: swap (FromX,FromY) with (ToX,ToY).
                let ei = nextIdxEn * 4
                let f0 = xyxys.[ei    ]
                let f1 = xyxys.[ei + 1]
                xyxys.[ei    ] <- xyxys.[ei + 2]
                xyxys.[ei + 1] <- xyxys.[ei + 3]
                xyxys.[ei + 2] <- f0
                xyxys.[ei + 3] <- f1
                // Now move the (just-reversed) element into position i+1.
                if nextIdxEn <> i + 1 then
                    let a = i + 1
                    swapT a nextIdxEn ts
                    swap4 a nextIdxEn xyxys
        xyxys

    #if BENCHMARKS
    /// Old implementation of sortToLoopWithReversing, kept for benchmarks comparison.
    /// Allocates Pt objects for distance calculations,
    static member sortToLoopWithReversingWithPtObjects  (getLine: 'T -> Line2D, reverseInPlace: int -> 'T -> unit, ts:ResizeArray<'T>) : unit =
        for i = 0 to ts.Count - 2 do // only run till second last
            let thisLine = getLine ts.[i]
            // TODO could be optimized using a R-Tree for very large lists instead of minBy function
            let nextIdxSt = ts |> minIndexByFrom (fun c -> Pt.sqDist (getLine c).From   thisLine.To) (i+1)
            let nextIdxEn = ts |> minIndexByFrom (fun c -> Pt.sqDist (getLine c).To     thisLine.To) (i+1)
            // check if closest endpoint is closer than closest start-point
            if  Pt.sqDist (getLine ts.[nextIdxSt]).From   thisLine.To <=
                Pt.sqDist (getLine ts.[nextIdxEn]).To     thisLine.To then
                    ts |> swap (i+1) nextIdxSt
            else
                reverseInPlace nextIdxEn ts.[nextIdxEn]
                ts |> swap (i+1) nextIdxEn
    #endif


    /// <summary>Returns the groups of consecutive elements, loops or polylines.
    /// They are split where the distance between the end point of one element and the start point of the next element is greater than 'splitDistance'.
    /// For each element it will compute a line given the 'getLine' function.
    /// The Line2D is used as an abstraction to hold start and end of arbitrary object.
    /// It searches forward and backward from the first element to connect elements into loops or polylines.
    /// For the line end point it finds the next closest line start point.
    /// And for the line start point it finds the next closest line end point.
    /// If the distance between two points is greater than the 'splitDistance' a new loop is started.
    /// </summary>
    /// <param name="getLine">A function that computes a Line2D from an element of type 'T.
    /// The Line2D is used as an abstraction to hold start and end of arbitrary object.
    /// For the line end point it finds the next closest line start point.
    /// And for the line start point it finds the next closest line end point.</param>
    /// <param name="splitDistance">A float value representing the maximum distance between points to be considered part of the same loop or polyline.
    /// If the distance between two points is greater than this value, a new loop or polyline will be started.</param>
    /// <param name="xs">A ResizeArray of elements of type 'T to be grouped into loops or polylines.</param>
    /// <returns>A ResizeArray of ResizeArrays of 'T, where each inner ResizeArray represents a group of consecutive elements
    /// that are part of the same loop or polyline.
    /// The groups are determined based on the distance between the end point of one element and the start point of the next element,
    /// as computed by the 'getLine' function and compared to the 'splitDistance'.</returns>
    static member join(getLine: 'T -> Line2D, splitDistance:float , xs:ResizeArray<'T>) : ResizeArray<ResizeArray<'T>> =
        let loops   = ResizeArray<LoopCollector<'T>>()
        let distSq  = splitDistance * splitDistance
        //let lns     = ResizeArray.map getLine xs // TODO precompute lines if getLine is expensive, then swap them too?

        let mutable idx = 0
        while idx < xs.Count do
            loops.Add {forward = ResizeArr.singleton xs.[idx]; backward = ResizeArray() }
            let ln = getLine xs.[idx]
            // search for the next closest line from the end point of the current line
            // use raw float coordinates instead of Pt structs, like sortToLoopWithReversing does
            let mutable enX = ln.ToX
            let mutable enY = ln.ToY
            let mutable i = idx + 1
            while i < xs.Count do
                let oth = getLine xs.[i]
                if sqDist2 enX enY oth.FromX oth.FromY <= distSq then
                    loops.Last.forward.Add xs.[i]
                    swap i (idx+1) xs
                    i   <- idx + 2 //reset search from swapped-to-index + 1
                    idx <- idx + 1
                    enX <- oth.ToX
                    enY <- oth.ToY
                else
                    i <- i + 1

            // search for the next closest line from the start point of the current line
            let mutable stX = ln.FromX
            let mutable stY = ln.FromY
            i <- idx + 1
            while i < xs.Count  do
                let oth = getLine xs.[i]
                if sqDist2 stX stY oth.ToX oth.ToY <= distSq then
                    loops.Last.backward.Add xs.[i]
                    swap i (idx+1) xs
                    i   <- idx + 2 //reset search from swapped-to-index + 1
                    idx <- idx + 1
                    stX <- oth.FromX
                    stY <- oth.FromY
                else
                    i <- i + 1
            // advance main while loop
            idx <- idx + 1

        // reverse the backward list
        // and add forward list to it
        loops
        |> ResizeArr.map (fun l ->
            l.backward.Reverse()
            l.backward.AddRange l.forward
            l.backward)

    /// <summary>Returns the groups of consecutive elements, loops or polylines.
    /// They are split where the distance between the end point of one element and the start or end point of the next element is greater than 'splitDistance'.
    /// For each element it will compute a line given the 'getLine' function.
    /// The Line2D is used as an abstraction to hold start and end of arbitrary object.
    /// Then for each line start and end point it finds the next closest line end or start point.
    /// Start and end points can match with both start and end points.
    /// If the distance between two points is greater than the 'splitDistance'
    /// it will be considered as a new group.</summary>
    /// <param name="getLine">A function that takes an element of type 'T and returns a Line2D representing its start and end points.</param>
    /// <param name="splitDistance">A float value representing the maximum distance between points to be considered part of the same loop or polyline.</param>
    /// <param name="xs">A ResizeArray of elements of type 'T to be grouped into loops or polylines.</param>
    /// <returns>A ResizeArray of ResizeArrays of tuples: 'T * Boolean, the Boolean values indicating if the element was reversed.
    /// The Boolean value is TRUE if the element was reversed, and FALSE if it was not</returns>
    static member joinReversing(getLine: 'T -> Line2D, splitDistance:float , xs:ResizeArray<'T>) : ResizeArray<ResizeArray<'T*bool>> =
        let loops   = ResizeArray<LoopCollector<'T*bool>>()
        let distSq  = splitDistance * splitDistance
        //let lns     = ResizeArray.map getLine xs // TODO precompute lines if getLine is expensive, then swap and reverse them too?

        let mutable idx = 0
        while idx < xs.Count do
            loops.Add {forward = ResizeArr.singleton (xs.[idx],false); backward = ResizeArray() }
            let ln = getLine xs.[idx]
            // search for the next closest line from the end point of the current line
            // use raw float coordinates instead of Pt structs, like sortToLoopWithReversing does
            let mutable enX = ln.ToX
            let mutable enY = ln.ToY
            let mutable i = idx + 1
            while i < xs.Count do
                let oth = getLine xs.[i]
                if sqDist2 enX enY oth.FromX oth.FromY <= distSq then
                    loops.Last.forward.Add (xs.[i],false)
                    swap i (idx+1) xs
                    i   <- idx + 2 //reset search from swap + 1
                    idx <- idx + 1
                    enX <- oth.ToX
                    enY <- oth.ToY
                elif sqDist2 enX enY oth.ToX oth.ToY <= distSq then
                    loops.Last.forward.Add (xs.[i],true)
                    swap i (idx+1) xs
                    i   <- idx + 2 //reset search from swap + 1
                    idx <- idx + 1
                    enX <- oth.FromX
                    enY <- oth.FromY
                else
                    i <- i + 1

            // search for the next closest line from the start point of the current line
            let mutable stX = ln.FromX
            let mutable stY = ln.FromY
            i <- idx + 1
            while i < xs.Count  do
                let oth = getLine xs.[i]
                if sqDist2 stX stY oth.ToX oth.ToY <= distSq then
                    loops.Last.backward.Add (xs.[i],false)
                    swap i (idx+1) xs
                    i   <- idx + 2 //reset search from swapped-to-index + 1
                    idx <- idx + 1
                    stX <- oth.FromX
                    stY <- oth.FromY
                elif sqDist2 stX stY oth.FromX oth.FromY <= distSq then
                    loops.Last.backward.Add (xs.[i],true)
                    swap i (idx+1) xs
                    i   <- idx + 2 //reset search from swapped-to-index + 1
                    idx <- idx + 1
                    stX <- oth.ToX
                    stY <- oth.ToY
                else
                    i <- i + 1
            // advance main while loop
            idx <- idx + 1

        loops
        |> ResizeArr.map (fun l ->
            l.backward.Reverse()
            l.backward.AddRange l.forward
            l.backward)

