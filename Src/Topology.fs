namespace Euclid

#nowarn "52" // copying of structs

module Topology  =


    /// Swap the values of two given indices in Rarr
    let inline private swap i j (xs:ResizeArray<'T>) :unit =
        if i<>j then
            let ti = xs.[i]
            xs.[i] <- xs.[j]
            xs.[j] <- ti


    /// Like minIndexBy. But starting to search only from a given index
    let inline  private  minIndexByFrom  (compareBy: 'T -> 'U)  fromIdx (xs:ResizeArray<'T>) : int =
        let mutable idx = fromIdx
        let mutable mi = compareBy xs.[fromIdx]
        for j = fromIdx + 1 to xs.LastIndex do
            let this = compareBy xs.[j]
            if this < mi then
                idx <- j
                mi <- this
        idx


    /// Like findIndexBy. But starting to search only from a given index.
    /// returns -1 if not found
    let inline private findIndexByFrom (predicate: 'T -> bool)  fromIdx (xs:ResizeArray<'T>) : int =
        let mutable i = fromIdx
        let mutable found = -1
        while i < xs.Count && found < 0 do
            if predicate xs.[i] then
                found <- i
            i <- i + 1
        found

    // --------------------------------------------------------------------------------------------
    // functions have 2D or 3D suffix because the overloads resolution sometimes fails when
    // the overload is just within the argument of the supplied function.
    // --------------------------------------------------------------------------------------------

    [<NoComparison>]
    type private LoopCollector<'T> =
        {
        forward: ResizeArray<'T>
        backward: ResizeArray<'T>
        }


    /// Returns the groups of consecutive elements, loops or polylines.
    /// They are split where the distance between the end point of one element and the start point of the next element is greater than 'splitDistance'.
    /// For each element it will compute a line given the 'getLine' function.
    /// The Line2D is used as an abstraction to hold start and end of arbitrary object.
    /// Then for each line start and end point it finds the next closest line end or start point respectively.
    /// Only start with end or end with start points are considered.
    /// If the distance between two points is greater than the 'splitDistance'
    /// it will be considered as a new group.
    let join2D(getLine: 'T -> Line2D, splitDistance:float , xs:ResizeArray<'T>) =
        let distSq  = splitDistance * splitDistance
        let lns     = ResizeArr.map getLine xs
        let isFree  = ResizeArr.init xs.Count (fun _ -> true)
        let loops   = ResizeArray<LoopCollector<'T>>()
        for idx = 0 to lns.LastIndex do
            if isFree.[idx] then
                loops.Add {forward = ResizeArr.singleton xs.[idx]; backward = ResizeArray() }
                let ln = lns.[idx]
                // search for the next closest line from the end point of the current line
                let mutable en = ln.To
                let mutable i = idx + 1
                while i < lns.Count do
                    if isFree.[i] then
                        let oth = lns.[i]
                        if Pt.distanceSq en oth.From < distSq then
                            isFree.[i] <- false
                            en <- (getLine xs.[i]).To
                            loops.Last.forward.Add xs.[i]
                            i <- idx + 1 //reset search from the start
                        else
                            i <- i + 1
                    else
                        i <- i + 1

                // search for the next closest line from the start point of the current line
                let mutable st = ln.From
                i <- idx + 1
                while i < lns.Count  do
                    if isFree.[i] then
                        let oth = lns.[i]
                        if Pt.distanceSq st oth.To < distSq then
                            isFree.[i] <- false
                            st <- (getLine xs.[i]).From
                            loops.Last.backward.Add xs.[i]
                            i <- idx + 1 //reset search from the start
                        else
                            i <- i + 1
                    else
                        i <- i + 1

        loops
        |> ResizeArr.map (fun l ->
            l.backward.Reverse()
            l.backward.AddRange l.forward
            l.backward)


    /// Returns the groups of consecutive elements, loops or polylines.
    /// They are split where the distance between the end point of one element and the start point of the next element is greater than 'splitDistance'.
    /// For each element it will compute a line given the 'getLine' function.
    /// The Line3D is used as an abstraction to hold start and end of arbitrary object.
    /// Then for each line start and end point it finds the next closest line end or start point respectively.
    /// Only start with end or end with start points are considered.
    /// If the distance between two points is greater than the 'splitDistance'
    /// it will be considered as a new group.
    let join3D(getLine: 'T -> Line3D, splitDistance:float , xs:ResizeArray<'T>) =
        let distSq  = splitDistance * splitDistance
        let lns     = ResizeArr.map getLine xs
        let isFree  = ResizeArr.init xs.Count (fun _ -> true)
        let loops   = ResizeArray<LoopCollector<'T>>()
        for idx = 0 to lns.LastIndex do
            if isFree.[idx] then
                loops.Add {forward = ResizeArr.singleton xs.[idx]; backward = ResizeArray() }
                let ln = lns.[idx]
                // search for the next closest line from the end point of the current line
                let mutable en = ln.To
                let mutable i = idx + 1
                while i < lns.Count do
                    if isFree.[i] then
                        let oth = lns.[i]
                        if Pnt.distanceSq en oth.From < distSq then
                            isFree.[i] <- false
                            en <- (getLine xs.[i]).To
                            loops.Last.forward.Add xs.[i]
                            i <- idx + 1 //reset search from the start
                        else
                            i <- i + 1
                    else
                        i <- i + 1

                // search for the next closest line from the start point of the current line
                let mutable st = ln.From
                i <- idx + 1
                while i < lns.Count  do
                    if isFree.[i] then
                        let oth = lns.[i]
                        if Pnt.distanceSq st oth.To < distSq then
                            isFree.[i] <- false
                            st <- (getLine xs.[i]).From
                            loops.Last.backward.Add xs.[i]
                            i <- idx + 1 //reset search from the start
                        else
                            i <- i + 1
                    else
                        i <- i + 1


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
    let joinReversing2D(getLine: 'T -> Line2D, splitDistance:float , xs:ResizeArray<'T>) =
        let distSq  = splitDistance * splitDistance
        let lns     = ResizeArr.map getLine xs
        let isFree  = ResizeArr.init xs.Count (fun _ -> true)
        let loops   = ResizeArray<LoopCollector<'T*bool>>()
        for idx = 0 to lns.LastIndex do
            if isFree.[idx] then
                loops.Add {forward = ResizeArr.singleton (xs.[idx],false); backward = ResizeArray() }
                let ln = lns.[idx]
                // search for the next closest line from the end point of the current line
                let mutable en = ln.To
                let mutable i = idx + 1
                while i < lns.Count do
                    if isFree.[i] then
                        let oth = lns.[i]
                        if Pt.distanceSq en oth.From < distSq then
                            isFree.[i] <- false
                            en <- (getLine xs.[i]).To
                            loops.Last.forward.Add (xs.[i],false)
                            i <- idx + 1 //reset search from the start
                        else if Pt.distanceSq en oth.To < distSq then
                            isFree.[i] <- false
                            en <- (getLine xs.[i]).From
                            loops.Last.forward.Add (xs.[i],true)
                            i <- idx + 1 //reset search from the start
                        else
                            i <- i + 1
                    else
                        i <- i + 1

                // search for the next closest line from the start point of the current line
                let mutable st = ln.From
                i <- idx + 1
                while i < lns.Count  do
                    if isFree.[i] then
                        let oth = lns.[i]
                        if Pt.distanceSq st oth.To < distSq then
                            isFree.[i] <- false
                            st <- (getLine xs.[i]).From
                            loops.Last.backward.Add (xs.[i],false)
                            i <- idx + 1 //reset search from the start
                        else if Pt.distanceSq st oth.From < distSq then
                            isFree.[i] <- false
                            st <- (getLine xs.[i]).To
                            loops.Last.backward.Add (xs.[i],true)
                            i <- idx + 1 //reset search from the start
                        else
                            i <- i + 1
                    else
                        i <- i + 1

        loops
        |> ResizeArr.map (fun l ->
            l.backward.Reverse()
            l.backward.AddRange l.forward
            l.backward)

    /// Returns the groups of consecutive elements, loops or polylines.
    /// They are split where the distance between the end point of one element and the start point of the next element is greater than 'splitDistance'.
    /// For each element it will compute a line given the 'getLine' function.
    /// The Line3D is used as an abstraction to hold start and end of arbitrary object.
    /// Then for each line start and end point it finds the next closest line end or start point respectively.
    /// Start points can match with start points and end points can match with end points too.
    /// If the distance between two points is greater than the 'splitDistance'
    /// it will be considered as a new group.
    /// The result will be a list of lists of 'T and a Boolean values indicating if the element was reversed.
    let joinReversing3D(getLine: 'T -> Line3D, splitDistance:float , xs:ResizeArray<'T>) =
        let distSq  = splitDistance * splitDistance
        let lns     = ResizeArr.map getLine xs
        let isFree  = ResizeArr.init xs.Count (fun _ -> true)
        let loops   = ResizeArray<LoopCollector<'T*bool>>()
        for idx = 0 to lns.LastIndex do
            if isFree.[idx] then
                loops.Add {forward = ResizeArr.singleton (xs.[idx],false); backward = ResizeArray() }
                let ln = lns.[idx]
                // search for the next closest line from the end point of the current line
                let mutable en = ln.To
                let mutable i = idx + 1
                while i < lns.Count do
                    if isFree.[i] then
                        let oth = lns.[i]
                        if Pnt.distanceSq en oth.From < distSq then
                            isFree.[i] <- false
                            en <- (getLine xs.[i]).To
                            loops.Last.forward.Add (xs.[i],false)
                            i <- idx + 1 //reset search from the start
                        else if Pnt.distanceSq en oth.To < distSq then
                            isFree.[i] <- false
                            en <- (getLine xs.[i]).From
                            loops.Last.forward.Add (xs.[i],true)
                            i <- idx + 1 //reset search from the start
                        else
                            i <- i + 1
                    else
                        i <- i + 1

                // search for the next closest line from the start point of the current line
                let mutable st = ln.From
                i <- idx + 1
                while i < lns.Count  do
                    if isFree.[i] then
                        let oth = lns.[i]
                        if Pnt.distanceSq st oth.To < distSq then
                            isFree.[i] <- false
                            st <- (getLine xs.[i]).From
                            loops.Last.backward.Add (xs.[i],false)
                            i <- idx + 1 //reset search from the start
                        else if Pnt.distanceSq st oth.From < distSq then
                            isFree.[i] <- false
                            st <- (getLine xs.[i]).To
                            loops.Last.backward.Add (xs.[i],true)
                            i <- idx + 1 //reset search from the start
                        else
                            i <- i + 1
                    else
                        i <- i + 1

        loops
        |> ResizeArr.map (fun l ->
            l.backward.Reverse()
            l.backward.AddRange l.forward
            l.backward)


    /// Sorts elements in place to be in a circular structure.
    /// This does not recognize if there are actually two loops, not just one.
    /// Use Topology.join2D instead.
    /// for each line end point it finds the next closest line start point.
    /// (Does not check other line end points that might be closer)
    /// Line3D is used as an abstraction to hold start and end of arbitrary object.
    let sortToLoop3D(getLine: 'T -> Line3D, xs:ResizeArray<'T>) =
        for i = 0 to xs.Count - 2 do // only run till second last
            let thisLine = getLine xs.[i]
            //  TODO could be optimized using a R-Tree for very large lists instead of minBy function
            let nextIdx = xs |> minIndexByFrom (fun c -> Pnt.distanceSq (getLine c).From thisLine.To) (i+1)
            xs |> swap (i+1) nextIdx


    /// Sorts elements in place to be in a circular structure.
    /// This does not recognize if there are actually two loops, not just one.
    /// /// Use Topology.join3D instead.
    /// for each line end point it finds the next closest line start point.
    /// (Does not check other line end points that might be closer)
    /// Line2D is used as an abstraction to hold start and end of arbitrary object.
    let sortToLoop2D(getLine: 'T -> Line2D, xs:ResizeArray<'T>) =
        for i = 0 to xs.Count - 2 do // only run till second last
            let thisLine = getLine xs.[i]
            //  TODO could be optimized using a R-Tree for very large lists instead of minBy function
            let nextIdx = xs |> minIndexByFrom (fun c -> Pt.distanceSq (getLine c).From  thisLine.To) (i+1)
            xs |> swap (i+1) nextIdx


    /// Sorts elements in place to be in a circular structure.
    /// This does not recognize if there are actually two loops, not just one.
    /// Use Topology.joinReversing3D instead.
    /// For each line end it finds the next closest start point or end point.
    /// Line3D is used as an abstraction to hold start and end of arbitrary object.
    /// Reverses the input in place where required.
    /// To reverse a 'T in place it uses the reverseInPlace function that takes the index of the element and the element itself as parameter.
    /// (They might not be need both however to reverse an element)
    let sortToLoopWithReversing3D (getLine: 'T -> Line3D, reverseInPlace: int -> 'T -> unit, xs:ResizeArray<'T>) : unit =
        for i = 0 to xs.Count - 2 do // only run till second last
            let thisLine = getLine xs.[i]
            // TODO could be optimized using a R-Tree for very large lists instead of minBy function
            let nextIdxSt = xs |> minIndexByFrom (fun c -> Pnt.distanceSq (getLine c).From  thisLine.To) (i+1)
            let nextIdxEn = xs |> minIndexByFrom (fun c -> Pnt.distanceSq (getLine c).To    thisLine.To) (i+1)
            // check if closest endpoint is closer than closest start-point
            if  Pnt.distanceSq (getLine xs.[nextIdxSt]).From  thisLine.To <=
                Pnt.distanceSq (getLine xs.[nextIdxEn]).To    thisLine.To then
                    xs |> swap (i+1) nextIdxSt
            else
                reverseInPlace nextIdxEn xs.[nextIdxEn]
                xs |> swap (i+1) nextIdxEn


    /// Sorts elements in place to be in a circular structure.
    /// This does not recognize if there are actually two loops, not just one.
    /// Use Topology.joinReversing3D instead.
    /// For each line end it finds the next closest start point or end point.
    /// Line2D is used as an abstraction to hold start and end of arbitrary object.
    /// Reverses the input in place where required.
    /// To reverse a 'T in place it uses the reverseInPlace function that takes the index of the element and the element itself as parameter.
    /// (They might not be need both however to reverse an element)
    let sortToLoopWithReversing2D (getLine: 'T -> Line2D, reverseInPlace: int -> 'T -> unit, xs:ResizeArray<'T>) : unit =
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
