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
    /// The Boolean value is TRUE if the element was reversed, and FALSE if it was not</returns>
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

