namespace FsEx.Geo

#nowarn "52" // copying of structs


module internal TopologyUtil =

    // The same function exist on FsEX.Rarr module too but with extra error checking.
    // Swap the values of two given indices in Rarr
    let inline swap i j (xs:ResizeArray<'T>) :unit =
        if i<>j then
            let ti = xs.[i]
            xs.[i] <- xs.[j]
            xs.[j] <- ti

    // The same function exist on FsEX.Rarr module too but with extra error checking.
    // Like Rarr.minIndexBy but starting to search only from a given index
    let inline  minIndexByFrom  (compareBy: 'T -> 'U)  fromIdx (xs:ResizeArray<'T>) : int =
        let mutable idx = fromIdx
        let mutable mi = compareBy xs.[fromIdx]
        for j = fromIdx + 1 to xs.LastIndex do
            let this = compareBy xs.[j]
            if this < mi then
                idx <- j
                mi <- this
        idx

open TopologyUtil

[<AbstractClass; Sealed>]
[<RequireQualifiedAccess>]
type Topology private () =


    // TODO !! This does not recognize if there are actually two loops not just one

    // functions have 2D or 3D suffix because the overloads resolution sometimes fails when used in scripts.

    /// Sorts elements in place to be in a circular structure.
    /// for each line end point it finds the next closest line start point.
    /// (Does not check other line end points that might be closer)
    /// Line3D is used as an abstraction to hold start and end of arbitrary object.
    static member sortToLoop3D(getLine: 'T -> Line3D, xs:ResizeArray<'T>)  =
        for i = 0 to xs.Count - 2 do // only run till second last
            let thisLine = getLine xs.[i]
            //  TODO could be optimized using a R-Tree for very large lists instead of minBy function
            let nextIdx = xs |> minIndexByFrom (fun c -> Pnt.distanceSq (getLine c).From thisLine.To ) (i+1)
            xs |> swap (i+1) nextIdx


    /// Sorts elements in place to be in a circular structure.
    /// for each line end point it finds the next closest line start point.
    /// (Does not check other line end points that might be closer)
    /// Line2D is used as an abstraction to hold start and end of arbitrary object.
    static member sortToLoop2D(getLine: 'T -> Line2D, xs:ResizeArray<'T>)  =
        for i = 0 to xs.Count - 2 do // only run till second last
            let thisLine = getLine xs.[i]
            //  TODO could be optimized using a R-Tree for very large lists instead of minBy function
            let nextIdx = xs |> minIndexByFrom (fun c -> Pt.distanceSq (getLine c).From  thisLine.To ) (i+1)
            xs |> swap (i+1) nextIdx


    /// Sorts elements in place to be in a circular structure.
    /// For each line end it finds the next closest start point or end point.
    /// Line3D is used as an abstraction to hold start and end of arbitrary object.
    /// Reverses the input in place where required.
    /// To reverse a 'T in place it uses the reverseInPlace function that takes the index of the element and the element itself as parameter.
    /// (They might not be need both however to reverse an element)
    static member sortToLoopWithReversing3D (getLine: 'T -> Line3D, reverseInPlace: int -> 'T -> unit, xs:ResizeArray<'T>) : unit =
        for i = 0 to xs.Count - 2 do // only run till second last
            let thisLine = getLine xs.[i]
            // TODO could be optimized using a R-Tree for very large lists instead of minBy function
            let nextIdxSt = xs |> minIndexByFrom (fun c -> Pnt.distanceSq (getLine c).From  thisLine.To ) (i+1)
            let nextIdxEn = xs |> minIndexByFrom (fun c -> Pnt.distanceSq (getLine c).To    thisLine.To ) (i+1)
            // check if closest endpoint is closer than closest start-point
            if  Pnt.distanceSq (getLine xs.[nextIdxSt]).From  thisLine.To <=
                Pnt.distanceSq (getLine xs.[nextIdxEn]).To    thisLine.To then
                    xs |> swap (i+1) nextIdxSt
            else
                    reverseInPlace nextIdxEn xs.[nextIdxEn]
                    xs |> swap (i+1) nextIdxEn


    /// Sorts elements in place to be in a circular structure.
    /// For each line end it finds the next closest start point or end point.
    /// Line2D is used as an abstraction to hold start and end of arbitrary object.
    /// Reverses the input in place where required.
    /// To reverse a 'T in place it uses the reverseInPlace function that takes the index of the element and the element itself as parameter.
    /// (They might not be need both however to reverse an element)
    static member sortToLoopWithReversing2D (getLine: 'T -> Line2D, reverseInPlace: int -> 'T -> unit, xs:ResizeArray<'T>) : unit =
        for i = 0 to xs.Count - 2 do // only run till second last
            let thisLine = getLine xs.[i]
            // TODO could be optimized using a R-Tree for very large lists instead of minBy function
            let nextIdxSt = xs |> minIndexByFrom (fun c -> Pt.distanceSq (getLine c).From   thisLine.To ) (i+1)
            let nextIdxEn = xs |> minIndexByFrom (fun c -> Pt.distanceSq (getLine c).To     thisLine.To ) (i+1)
            // check if closest endpoint is closer than closest start-point
            if  Pt.distanceSq (getLine xs.[nextIdxSt]).From   thisLine.To <=
                Pt.distanceSq (getLine xs.[nextIdxEn]).To     thisLine.To then
                    xs |> swap (i+1) nextIdxSt
            else
                    reverseInPlace nextIdxEn xs.[nextIdxEn]
                    xs |> swap (i+1) nextIdxEn
