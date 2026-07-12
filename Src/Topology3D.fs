namespace Euclid

open System
open TopologyUtil
open Euclid.EuclidCollectionUtilities

#nowarn "52" // copying of structs


/// A type containing only static member functions for 3D topological operations.
type Topology3D  =


    /// <summary>Sorts elements in place to be in a circular structure.
    /// This does NOT recognize if there are actually several loops.
    /// Use Topology3D.join for this instead.</summary>
    /// <param name="getLine">A function that computes a Line3D from an element of type 'T.
    /// The Line3D is used as an abstraction to hold start and end of arbitrary object.
    /// For each line end point it finds the next closest line start point.</param>
    /// <param name="ts">A ResizeArray of elements of type 'T' to be sorted in place.</param>
    /// <returns>The input array 'ts' is modified in place to be sorted in a circular structure.</returns>
    static member sortToLoop(getLine: 'T -> Line3D, ts:ResizeArray<'T>) : unit =
        let count = ts.Count
        for i = 0 to count - 2 do // only run till second last
            let thisLine = getLine ts.[i]
            let tx = thisLine.ToX
            let ty = thisLine.ToY
            let tz = thisLine.ToZ
            //  TODO could be optimized using a R-Tree for very large lists instead of minBy function
            let minIdx = ts |> minIndexByFrom (fun c -> let l = getLine c in sqDist3 l.FromX l.FromY l.FromZ tx ty tz) (i+1)
            ts |> swap (i+1) minIdx


    /// <summary>Sorts elements in place to be in a circular structure.
    /// Does the same thing as sortToLoop but caches the line coordinates in a float array
    /// to avoid repeated getLine calls inside the inner loop.
    /// This runs slightly faster when getLine is expensive. (The benefit only shows in .NET not in Fable JS)
    /// This does NOT recognize if there are actually several loops.
    /// Use Topology3D.join for this instead.</summary>
    /// <param name="getLine">A function that computes a Line3D from an element of type 'T.
    /// The Line3D is used as an abstraction to hold start and end of arbitrary object.
    /// For each line end point it finds the next closest line start point.</param>
    /// <param name="ts">A ResizeArray of elements of type 'T' to be sorted in place.</param>
    /// <returns>The input array 'ts' is modified in place to be sorted in a circular structure.
    /// But also returns a sorted flat array of the 3d lines used internally [FromX; FromY; FromZ; ToX; ToY; ToZ].</returns>
    static member sortToLoopCached(getLine: 'T -> Line3D, ts:ResizeArray<'T>) : float[] =
        let count = ts.Count

        // Flat array of [FromX; FromY; FromZ; ToX; ToY; ToZ] for each line, kept in sync with ts via swaps below.
        // Avoids constructing Pnt structs from Line3D.From / Line3D.To inside the inner search loop,
        // and ensures getLine is invoked exactly once per element (not O(n) times as a naive minBy would).
        // Layout: line k occupies xyzs.[k*6 .. k*6+5] as FromX, FromY, FromZ, ToX, ToY, ToZ.
        let xyzs = Arr.undefCreateFloat64(count * 6)
        for k = 0 to count - 1 do
            let ln = getLine ts.[k]
            let k6 = k * 6
            xyzs.[k6    ] <- ln.FromX
            xyzs.[k6 + 1] <- ln.FromY
            xyzs.[k6 + 2] <- ln.FromZ
            xyzs.[k6 + 3] <- ln.ToX
            xyzs.[k6 + 4] <- ln.ToY
            xyzs.[k6 + 5] <- ln.ToZ

        for i = 0 to count - 2 do // only run till second last
            // current line's end point (To) — we are searching for the next line whose From is closest to this.
            let toX = xyzs.[i*6 + 3]
            let toY = xyzs.[i*6 + 4]
            let toZ = xyzs.[i*6 + 5]
            //  TODO could be optimized using a R-Tree for very large lists instead of linear search
            // Seed the running minimum with the candidate at i+1, then scan i+2..count-1.
            let i1 = (i + 1) * 6
            let mutable nextIdx = i + 1
            let mutable dx = xyzs.[i1    ] - toX
            let mutable dy = xyzs.[i1 + 1] - toY
            let mutable dz = xyzs.[i1 + 2] - toZ
            let mutable smallest = dx*dx + dy*dy + dz*dz // squared distance, no sqrt needed for comparison
            for j = i + 2 to count - 1 do
                let j6 = j * 6
                // distance from candidate j's From point to current To point
                let dx = xyzs.[j6    ] - toX
                let dy = xyzs.[j6 + 1] - toY
                let dz = xyzs.[j6 + 2] - toZ
                let d  = dx*dx + dy*dy + dz*dz
                if d < smallest then
                    nextIdx <- j
                    smallest <- d
            // Move the chosen element into position i+1, swapping both xs and the parallel coords block.
            if nextIdx <> i + 1 then
                let a = i + 1
                swapT a nextIdx ts
                swap6 a nextIdx xyzs
        xyzs


    /// <summary>Sorts elements in place to be in a circular structure.
    /// For each line end it finds the next closest start point or end point.
    /// This does NOT recognize if there are actually several loops.
    /// Use Topology3D.joinReversing for this instead.</summary>
    /// <param name="getLine">A function that computes a Line3D from an element of type 'T.
    /// The Line3D is used as an abstraction to hold start and end of arbitrary object.
    /// For each line end it finds the next closest start point or end point.</param>
    /// <param name="reverseInPlace">A function that reverses an element in place given its index and the element itself.
    /// This function might just update an item at the given index in the array,
    /// or it might modify the element in place if 'T is a reference type.</param>
    /// <param name="ts">A ResizeArray of elements of type 'T' to be sorted in place.</param>
    /// <returns>The input array 'ts' is modified in place to be sorted in a circular structure.</returns>
    static member sortToLoopWithReversing (getLine: 'T -> Line3D, reverseInPlace: int -> 'T -> unit, ts:ResizeArray<'T>) : unit =
        let count = ts.Count
        for i = 0 to count - 2 do // only run till second last
            let thisLine = getLine ts.[i]
            let tx = thisLine.ToX
            let ty = thisLine.ToY
            let tz = thisLine.ToZ
            // TODO could be optimized using a R-Tree for very large lists instead of minBy function
            let minIdx = ts |> minIndexByFrom (fun c -> let l = getLine c in min(sqDist3 l.FromX l.FromY l.FromZ tx ty tz)(sqDist3 l.ToX l.ToY l.ToZ tx ty tz)) (i+1)
            // check if closest endpoint is closer than closest start-point
            let minLn = getLine ts.[minIdx]

            if sqDist3 minLn.FromX minLn.FromY minLn.FromZ tx ty tz > sqDist3 minLn.ToX minLn.ToY minLn.ToZ tx ty tz then
                reverseInPlace minIdx ts.[minIdx]
            ts |> swap (i+1) minIdx


    /// <summary>Sorts elements in place to be in a circular structure.
    /// Does the same thing as sortToLoopWithReversing but caches the line coordinates in a float array
    /// to avoid repeated getLine calls inside the inner loop.
    /// This runs slightly faster when getLine is expensive. (The benefit only shows in .NET not in Fable JS)
    /// For each line end it finds the next closest start point or end point.
    /// This does NOT recognize if there are actually several loops.
    /// Use Topology3D.joinReversing for this instead.</summary>
    /// <param name="getLine">A function that computes a Line3D from an element of type 'T.
    /// The Line3D is used as an abstraction to hold start and end of arbitrary object.
    /// For each line end it finds the next closest start point or end point.</param>
    /// <param name="reverseInPlace">A function that reverses an element in place given its index and the element itself.
    /// This function might just update an item at the given index in the array,
    /// or it might modify the element in place if 'T is a reference type.</param>
    /// <param name="ts">A ResizeArray of elements of type 'T' to be sorted in place.</param>
    /// <returns>The input array 'ts' is modified in place to be sorted in a circular structure.
    /// But also returns a sorted flat array of the 3d lines used internally [FromX; FromY; FromZ; ToX; ToY; ToZ].
    /// Entries whose element was reversed have their From and To coordinates swapped to reflect the new orientation.</returns>
    static member sortToLoopWithReversingCached (getLine: 'T -> Line3D, reverseInPlace: int -> 'T -> unit, ts:ResizeArray<'T>) : float[] =
        let count = ts.Count

        // Flat array of [FromX; FromY; FromZ; ToX; ToY; ToZ] for each line, kept in sync with ts through swaps and in-place reversals below.
        // Avoids constructing Pnt structs from Line3D.From / Line3D.To inside the inner search loop,
        // and ensures getLine is invoked exactly once per element (not O(n) times as a naive minBy would).
        // Layout: line k occupies xyzs.[k*6 .. k*6+5] as FromX, FromY, FromZ, ToX, ToY, ToZ.
        let xyzs = Arr.undefCreateFloat64(count * 6)
        for k = 0 to count - 1 do
            let ln = getLine ts.[k]
            let k6 = k * 6
            xyzs.[k6    ] <- ln.FromX
            xyzs.[k6 + 1] <- ln.FromY
            xyzs.[k6 + 2] <- ln.FromZ
            xyzs.[k6 + 3] <- ln.ToX
            xyzs.[k6 + 4] <- ln.ToY
            xyzs.[k6 + 5] <- ln.ToZ

        for i = 0 to count - 2 do // only run till second last
            // current line's end point (To) — we are searching for the next line whose From OR To is closest to this.
            let toX = xyzs.[i*6 + 3]
            let toY = xyzs.[i*6 + 4]
            let toZ = xyzs.[i*6 + 5]
            //  TODO could be optimized using a R-Tree for very large lists instead of linear search
            // Single pass over candidates: track the closest From-to-To distance and the closest To-to-To distance.
            // Seed both running minima with the candidate at i+1, then scan i+2..count-1.
            let i1 = (i + 1) * 6
            let mutable nextIdxSt = i + 1
            let mutable nextIdxEn = i + 1
            let mutable dxS = xyzs.[i1    ] - toX
            let mutable dyS = xyzs.[i1 + 1] - toY
            let mutable dzS = xyzs.[i1 + 2] - toZ
            let mutable smallestSt = dxS*dxS + dyS*dyS + dzS*dzS // squared distance from candidate's From to current To
            let mutable dxE = xyzs.[i1 + 3] - toX
            let mutable dyE = xyzs.[i1 + 4] - toY
            let mutable dzE = xyzs.[i1 + 5] - toZ
            let mutable smallestEn = dxE*dxE + dyE*dyE + dzE*dzE // squared distance from candidate's To to current To
            for j = i + 2 to count - 1 do
                let j6 = j * 6
                // candidate j's From to current To
                let dxs = xyzs.[j6    ] - toX
                let dys = xyzs.[j6 + 1] - toY
                let dzs = xyzs.[j6 + 2] - toZ
                let ds  = dxs*dxs + dys*dys + dzs*dzs
                if ds < smallestSt then
                    nextIdxSt <- j
                    smallestSt <- ds
                // candidate j's To to current To (would require reversing the candidate)
                let dxe = xyzs.[j6 + 3] - toX
                let dye = xyzs.[j6 + 4] - toY
                let dze = xyzs.[j6 + 5] - toZ
                let de  = dxe*dxe + dye*dye + dze*dze
                if de < smallestEn then
                    nextIdxEn <- j
                    smallestEn <- de
            // Prefer the no-reversal branch on ties (matches original `<=` semantics).
            if smallestSt <= smallestEn then
                // Move chosen element into position i+1 without reversal.
                if nextIdxSt <> i + 1 then
                    let a = i + 1
                    swapT a nextIdxSt ts
                    swap6 a nextIdxSt xyzs
            else
                // Reverse the chosen element in place so its old To becomes the new From.
                reverseInPlace nextIdxEn ts.[nextIdxEn]
                // Reflect the reversal in the parallel coords block: swap (From) with (To).
                let ei = nextIdxEn * 6
                let f0 = xyzs.[ei    ]
                let f1 = xyzs.[ei + 1]
                let f2 = xyzs.[ei + 2]
                xyzs.[ei    ] <- xyzs.[ei + 3]
                xyzs.[ei + 1] <- xyzs.[ei + 4]
                xyzs.[ei + 2] <- xyzs.[ei + 5]
                xyzs.[ei + 3] <- f0
                xyzs.[ei + 4] <- f1
                xyzs.[ei + 5] <- f2
                // Now move the (just-reversed) element into position i+1.
                if nextIdxEn <> i + 1 then
                    let a = i + 1
                    swapT a nextIdxEn ts
                    swap6 a nextIdxEn xyzs
        xyzs


    /// <summary>Returns the groups of consecutive elements, loops or polylines.
    /// They are split where the distance between the end point of one element and the start point of the next element is greater than 'splitDistance'.
    /// For each element it will compute a line given the 'getLine' function.
    /// The Line3D is used as an abstraction to hold start and end of arbitrary object.
    /// It searches forward and backward from the first element to connect elements into loops or polylines.
    /// For the line end point it finds the next closest line start point.
    /// And for the line start point it finds the next closest line end point.
    /// If the distance between two points is greater than the 'splitDistance' a new loop is started.</summary>
    /// <param name="getLine">A function that computes a Line3D from an element of type 'T.
    /// The Line3D is used as an abstraction to hold start and end of arbitrary object.
    /// For the line end point it finds the next closest line start point.
    /// And for the line start point it finds the next closest line end point.</param>
    /// <param name="splitDistance">A float value representing the maximum distance between points to be considered part of the same loop or polyline.
    /// If the distance between two points is greater than this value, a new loop or polyline will be started.</param>
    /// <param name="xs">A ResizeArray of elements of type 'T to be grouped into loops or polylines.</param>
    /// <returns>A ResizeArray of ResizeArrays of 'T, where each inner ResizeArray represents a group of consecutive elements
    /// that are part of the same loop or polyline.</returns>
    static member join(getLine: 'T -> Line3D, splitDistance:float , xs:ResizeArray<'T>) : ResizeArray<ResizeArray<'T>> =
        let loops   = ResizeArray<LoopCollector<'T>>()
        let distSq  = splitDistance * splitDistance
        //let lns     = ResizeArray.map getLine xs // TODO precompute lines if getLine is expensive, then swap them too?

        let mutable idx = 0
        while idx < xs.Count do
            loops.Add {forward = ResizeArr.singleton xs.[idx]; backward = ResizeArray() }
            let ln = getLine xs.[idx]
            // search for the next closest line from the end point of the current line
            // use raw float coordinates instead of Pnt structs, like sortToLoopWithReversing does
            let mutable enX = ln.ToX
            let mutable enY = ln.ToY
            let mutable enZ = ln.ToZ
            let mutable i = idx + 1
            while i < xs.Count do
                let oth = getLine xs.[i]
                if sqDist3 enX enY enZ oth.FromX oth.FromY oth.FromZ <= distSq then
                    loops.Last.forward.Add xs.[i]
                    swap i (idx+1) xs
                    i   <- idx + 2 //reset search from swapped-to-index + 1
                    idx <- idx + 1
                    enX <- oth.ToX
                    enY <- oth.ToY
                    enZ <- oth.ToZ
                else
                    i <- i + 1

            // search for the next closest line from the start point of the current line
            let mutable stX = ln.FromX
            let mutable stY = ln.FromY
            let mutable stZ = ln.FromZ
            i <- idx + 1
            while i < xs.Count  do
                let oth = getLine xs.[i]
                if sqDist3 stX stY stZ oth.ToX oth.ToY oth.ToZ <= distSq then
                    loops.Last.backward.Add xs.[i]
                    swap i (idx+1) xs
                    i   <- idx + 2 //reset search from swapped-to-index + 1
                    idx <- idx + 1
                    stX <- oth.FromX
                    stY <- oth.FromY
                    stZ <- oth.FromZ
                else
                    i <- i + 1
            // advance main while loop
            idx <- idx + 1

        loops
        |> ResizeArr.map (fun l ->
            l.backward.Reverse()
            l.backward.AddRange l.forward
            l.backward)

    /// <summary>Returns the groups of consecutive elements, loops or polylines.
    /// They are split where the distance between the end point of one element and the start or end point of the next element is greater than 'splitDistance'.
    /// For each element it will compute a line given the 'getLine' function.
    /// The Line3D is used as an abstraction to hold start and end of arbitrary object.
    /// Then for each line start and end point it finds the next closest line end or start point.
    /// Start and end points can match with both start and end points.
    /// If the distance between two points is greater than the 'splitDistance'
    /// it will be considered as a new group.</summary>
    /// <param name="getLine">A function that takes an element of type 'T and returns a Line3D representing its start and end points.</param>
    /// <param name="splitDistance">A float value representing the maximum distance between points to be considered part of the same loop or polyline.</param>
    /// <param name="xs">A ResizeArray of elements of type 'T to be grouped into loops or polylines.</param>
    /// <returns>A ResizeArray of ResizeArrays of tuples: 'T * Boolean, the Boolean values indicating if the element was reversed.
    /// The Boolean value is true if the element was reversed, and false if it was not</returns>
    static member joinReversing(getLine: 'T -> Line3D, splitDistance:float , xs:ResizeArray<'T>) : ResizeArray<ResizeArray<'T*bool>> =
        let loops   = ResizeArray<LoopCollector<'T*bool>>()
        let distSq  = splitDistance * splitDistance
        //let lns     = ResizeArray.map getLine xs // TODO precompute lines if getLine is expensive, then swap and reverse them too?

        let mutable idx = 0
        while idx < xs.Count do
            loops.Add {forward = ResizeArr.singleton (xs.[idx],false); backward = ResizeArray() }
            let ln = getLine xs.[idx]
            // search for the next closest line from the end point of the current line
            // use raw float coordinates instead of Pnt structs, like sortToLoopWithReversing does
            let mutable enX = ln.ToX
            let mutable enY = ln.ToY
            let mutable enZ = ln.ToZ
            let mutable i = idx + 1
            while i < xs.Count do
                let oth = getLine xs.[i]
                if sqDist3 enX enY enZ oth.FromX oth.FromY oth.FromZ <= distSq then
                    loops.Last.forward.Add (xs.[i],false)
                    swap i (idx+1) xs
                    i   <- idx + 2 //reset search from swap + 1
                    idx <- idx + 1
                    enX <- oth.ToX
                    enY <- oth.ToY
                    enZ <- oth.ToZ
                elif sqDist3 enX enY enZ oth.ToX oth.ToY oth.ToZ <= distSq then
                    loops.Last.forward.Add (xs.[i],true)
                    swap i (idx+1) xs
                    i   <- idx + 2 //reset search from swap + 1
                    idx <- idx + 1
                    enX <- oth.FromX
                    enY <- oth.FromY
                    enZ <- oth.FromZ
                else
                    i <- i + 1

            // search for the next closest line from the start point of the current line
            let mutable stX = ln.FromX
            let mutable stY = ln.FromY
            let mutable stZ = ln.FromZ
            i <- idx + 1
            while i < xs.Count  do
                let oth = getLine xs.[i]
                if sqDist3 stX stY stZ oth.ToX oth.ToY oth.ToZ <= distSq then
                    loops.Last.backward.Add (xs.[i],false)
                    swap i (idx+1) xs
                    i   <- idx + 2 //reset search from swapped-to-index + 1
                    idx <- idx + 1
                    stX <- oth.FromX
                    stY <- oth.FromY
                    stZ <- oth.FromZ
                elif sqDist3 stX stY stZ oth.FromX oth.FromY oth.FromZ <= distSq then
                    loops.Last.backward.Add (xs.[i],true)
                    swap i (idx+1) xs
                    i   <- idx + 2 //reset search from swapped-to-index + 1
                    idx <- idx + 1
                    stX <- oth.ToX
                    stY <- oth.ToY
                    stZ <- oth.ToZ
                else
                    i <- i + 1
            // advance main while loop
            idx <- idx + 1

        loops
        |> ResizeArr.map (fun l ->
            l.backward.Reverse()
            l.backward.AddRange l.forward
            l.backward)


// #endregion
// #region Obsolete


[<Obsolete("Use Topology2D or Topology3D module instead")>]
module Topology  =

    [<Obsolete("Use Topology3D.sortToLoop instead")>]
    let sortToLoop3D(getLine: 'T -> Line3D, xs:ResizeArray<'T>) =
        Topology3D.sortToLoop(getLine, xs)

    [<Obsolete("Use Topology2D.sortToLoop instead")>]
    let sortToLoop2D(getLine: 'T -> Line2D, xs:ResizeArray<'T>) =
        Topology2D.sortToLoop(getLine, xs)

    [<Obsolete("Use Topology3D.sortToLoopWithReversing instead")>]
    let sortToLoopWithReversing3D (getLine: 'T -> Line3D, reverseInPlace: int -> 'T -> unit, xs:ResizeArray<'T>) : unit =
        Topology3D.sortToLoopWithReversing(getLine, reverseInPlace, xs)

    [<Obsolete("Use Topology2D.sortToLoopWithReversing instead")>]
    let sortToLoopWithReversing2D (getLine: 'T -> Line2D, reverseInPlace: int -> 'T -> unit, xs:ResizeArray<'T>) =
        Topology2D.sortToLoopWithReversing(getLine, reverseInPlace, xs)

    [<Obsolete("Use Topology2D.join instead")>]
    let join2D(getLine: 'T -> Line2D, splitDistance:float , xs:ResizeArray<'T>) =
        Topology2D.join(getLine, splitDistance, xs)

    [<Obsolete("Use Topology3D.join instead")>]
    let join3D(getLine: 'T -> Line3D, splitDistance:float , xs:ResizeArray<'T>) =
        Topology3D.join(getLine, splitDistance, xs)

    [<Obsolete("Use Topology2D.joinReversing instead")>]
    let joinReversing2D(getLine: 'T -> Line2D, splitDistance:float , xs:ResizeArray<'T>) =
        Topology2D.joinReversing(getLine, splitDistance, xs)

    [<Obsolete("Use Topology3D.joinReversing instead")>]
    let joinReversing3D(getLine: 'T -> Line3D, splitDistance:float , xs:ResizeArray<'T>) =
        Topology3D.joinReversing(getLine, splitDistance, xs)

