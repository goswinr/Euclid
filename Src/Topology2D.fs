namespace Euclid

#nowarn "52" // copying of structs


module internal TopologyUtil =

    /// Swap the values of two given indices in Rarr
    let inline swap i j (xs:ResizeArray<'T>) :unit =
        if i<>j then
            let ti = xs.[i]
            xs.[i] <- xs.[j]
            xs.[j] <- ti


    /// Like minIndexBy. But starting to search only from a given index
    let inline minIndexByFrom (compareBy: 'T -> 'U)  fromIdx (xs:ResizeArray<'T>) : int =
        let mutable idx = fromIdx
        let mutable mi = compareBy xs.[fromIdx]
        for j = fromIdx + 1 to xs.LastIndex do
            let this = compareBy xs.[j]
            if this < mi then
                idx <- j
                mi <- this
        idx


    /// Like findIndexBy. But starting to search only from a given index.
    /// Returns -1 if not found
    let inline findIndexByFrom (predicate: 'T -> bool)  fromIdx (xs:ResizeArray<'T>) : int =
        let mutable i = fromIdx
        let mutable found = -1
        while i < xs.Count && found < 0 do
            if predicate xs.[i] then
                found <- i
            i <- i + 1
        found


    [<NoComparison>]
    type LoopCollector<'T> =
        {
        forward: ResizeArray<'T>
        backward: ResizeArray<'T>
        }


open TopologyUtil

/// A static type containing functions for 2D topological operations.
type Topology2D  =

    /// Sorts elements in place to be in a circular structure.
    /// This does not recognize if there are actually two loops, not just one.
    /// Use Topology2D.join instead.
    /// For each line end point it finds the next closest line start point.
    /// (Does not check other line end points that might be closer)
    /// Line2D is used as an abstraction to hold start and end of arbitrary object.
    static member sortToLoop(getLine: 'T -> Line2D, xs:ResizeArray<'T>) =
        for i = 0 to xs.Count - 2 do // only run till second last
            let thisLine = getLine xs.[i]
            //  TODO could be optimized using a R-Tree for very large lists instead of minBy function
            let nextIdx = xs |> minIndexByFrom (fun c -> Pt.distanceSq (getLine c).From  thisLine.To) (i+1)
            xs |> swap (i+1) nextIdx


    /// Sorts elements in place to be in a circular structure.
    /// This does not recognize if there are actually two loops, not just one.
    /// Use Topology2D.joinReversing instead.
    /// For each line end it finds the next closest start point or end point.
    /// Line2D is used as an abstraction to hold start and end of arbitrary object.
    /// Reverses the input in place where required.
    /// e.g. the reverseInPlace function might just update an item at the given index in the array.
    /// Depending on the structure of 'T the index might not be needed to reverse an element in place.
    static member sortToLoopWithReversing (getLine: 'T -> Line2D, reverseInPlace: int -> 'T -> unit, xs:ResizeArray<'T>) : unit =
        for i = 0 to xs.Count - 2 do // only run till second last
            let thisLine = getLine xs.[i]
            // TODO could be optimized using a R-Tree for very large lists instead of minBy function
            let nextIdxSt = xs |> minIndexByFrom (fun c -> Pt.distanceSq (getLine c).From   thisLine.To) (i+1)
            let nextIdxEn = xs |> minIndexByFrom (fun c -> Pt.distanceSq (getLine c).To     thisLine.To) (i+1)
            // check if closest endpoint is closer than closest start-point
            if  Pt.distanceSq (getLine xs.[nextIdxSt]).From   thisLine.To <=
                Pt.distanceSq (getLine xs.[nextIdxEn]).To     thisLine.To then
                    xs |> swap (i+1) nextIdxSt
            else
                reverseInPlace nextIdxEn xs.[nextIdxEn]
                xs |> swap (i+1) nextIdxEn



    /// Returns the groups of consecutive elements, loops or polylines.
    /// They are split where the distance between the end point of one element and the start point of the next element is greater than 'splitDistance'.
    /// For each element it will compute a line given the 'getLine' function.
    /// The Line2D is used as an abstraction to hold start and end of arbitrary object.
    /// Then for each line start and end point it finds the next closest line end or start point respectively.
    /// Only start with end or end with start points are considered.
    /// If the distance between two points is greater than the 'splitDistance'
    /// it will be considered as a new group.
    static member join(getLine: 'T -> Line2D, splitDistance:float , xs:ResizeArray<'T>) : ResizeArray<ResizeArray<'T>> =
        let loops   = ResizeArray<LoopCollector<'T>>()
        let distSq  = splitDistance * splitDistance
        //let lns     = ResizeArray.map getLine xs // precompute lines if getLine is expensive, then swap them too

        let mutable idx = 0
        while idx < xs.Count do
            loops.Add {forward = ResizeArr.singleton xs.[idx]; backward = ResizeArray() }
            let ln = getLine xs.[idx]
            // search for the next closest line from the end point of the current line
            let mutable en = ln.To
            let mutable i = idx + 1
            while i < xs.Count do
                let oth = getLine xs.[i]
                if Pt.distanceSq en oth.From < distSq then
                    loops.Last.forward.Add xs.[i]
                    swap i (idx+1) xs
                    i   <- idx + 2 //reset search from swapped-to-index + 1
                    idx <- idx + 1
                    en  <- oth.To
                else
                    i <- i + 1

            // search for the next closest line from the start point of the current line
            let mutable st = ln.From
            i <- idx + 1
            while i < xs.Count  do
                let oth = getLine xs.[i]
                if Pt.distanceSq st oth.To < distSq then
                    loops.Last.backward.Add xs.[i]
                    swap i (idx+1) xs
                    i   <- idx + 2 //reset search from swapped-to-index + 1
                    idx <- idx + 1
                    st  <- oth.From
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


    /// Returns the groups of consecutive elements, loops or polylines.
    /// They are split where the distance between the end point of one element and the start point of the next element is greater than 'splitDistance'.
    /// For each element it will compute a line given the 'getLine' function.
    /// The Line2D is used as an abstraction to hold start and end of arbitrary object.
    /// Then for each line start and end point it finds the next closest line end or start point respectively.
    /// Start points can match with start points and end points can match with end points too.
    /// If the distance between two points is greater than the 'splitDistance'
    /// it will be considered as a new group.
    /// The result will be a list of lists of 'T and a Boolean values indicating if the element was reversed.
    static member joinReversing(getLine: 'T -> Line2D, splitDistance:float , xs:ResizeArray<'T>) : ResizeArray<ResizeArray<'T*bool>> =
        let loops   = ResizeArray<LoopCollector<'T*bool>>()
        let distSq  = splitDistance * splitDistance
        //let lns     = ResizeArray.map getLine xs // precompute lines if getLine is expensive, then swap them too

        let mutable idx = 0
        while idx < xs.Count do
            loops.Add {forward = ResizeArr.singleton (xs.[idx],false); backward = ResizeArray() }
            let ln = getLine xs.[idx]
            // search for the next closest line from the end point of the current line
            let mutable en = ln.To
            let mutable i = idx + 1
            while i < xs.Count do
                let oth = getLine xs.[i]
                if Pt.distanceSq en oth.From < distSq then
                    loops.Last.forward.Add (xs.[i],false)
                    swap i (idx+1) xs
                    i   <- idx + 2 //reset search from swap + 1
                    idx <- idx + 1
                    en  <- oth.To
                elif Pt.distanceSq en oth.To < distSq then
                    loops.Last.forward.Add (xs.[i],true)
                    swap i (idx+1) xs
                    i   <- idx + 2 //reset search from swap + 1
                    idx <- idx + 1
                    en  <- oth.From
                else
                    i <- i + 1

            // search for the next closest line from the start point of the current line
            let mutable st = ln.From
            i <- idx + 1
            while i < xs.Count  do
                let oth = getLine xs.[i]
                if Pt.distanceSq st oth.To < distSq then
                    loops.Last.backward.Add (xs.[i],false)
                    swap i (idx+1) xs
                    i   <- idx + 2 //reset search from swapped-to-index + 1
                    idx <- idx + 1
                    st  <- oth.From
                elif Pt.distanceSq st oth.From < distSq then
                    loops.Last.backward.Add (xs.[i],true)
                    swap i (idx+1) xs
                    i   <- idx + 2 //reset search from swapped-to-index + 1
                    idx <- idx + 1
                    st  <- oth.To
                else
                    i <- i + 1
            // advance main while loop
            idx <- idx + 1

        loops
        |> ResizeArr.map (fun l ->
            l.backward.Reverse()
            l.backward.AddRange l.forward
            l.backward)

