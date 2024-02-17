namespace Euclid

open System


module internal Arr
 =

    /// Returns the index of the smallest element.
    let minIndex (xs:'T[]) =
        if xs.Length < 1 then raise <| ArgumentException(sprintf "Euclid.Array.minIndex: Count must be at least one: %A"  xs)
        let mutable f = xs.[0]
        let mutable mf = f
        let mutable ii = 0
        for i=1 to xs.Length-1 do
            f <- xs.[i]
            if f < mf then
                ii <- i
                mf <- f
        ii

    /// Returns the index of the biggest element.
    let maxIndex (xs:'T[]) =
        if xs.Length < 1 then raise <| ArgumentException(sprintf "Euclid.Array.maxIndex: Count must be at least one: %A"  xs)
        let mutable f = xs.[0]
        let mutable mf = f
        let mutable ii = 0
        for i=1 to xs.Length-1 do
            f <- xs.[i]
            if f > mf then
                ii <- i
                mf <- f
        ii

// written with lowercase so that it does not get shadowed by the ResizeArray library if used together with it in Fable.
module internal ResizeArr =
        

        /// just like Array.create.
        let inline create (count:int) (x:'T) =
            let r = new ResizeArray<'T>(count)
            for i=0 to count-1 do r.Add(x)
            r

        /// this.Count.
        let inline length (xs: ResizeArray<'T>) =
            xs.Count

        /// Yields looped Seq from (first, second)  up to (last, first).
        /// The resulting seq has the same element count as the input Rarr.
        let thisNext (rarr:ResizeArray<'T>) =
            if rarr.Count <= 2 then EuclidException.Raise "Euclid.ResizeArr.thisNext input has less than two items:\r\n%O" rarr
            seq {   for i = 0 to rarr.Count-2 do  rarr.[i], rarr.[i+1]
                    rarr.[rarr.Count-1], rarr.[0] }

        /// Yields looped Seq from (1, last, first, second)  up to (lastIndex, second-last, last, first)
        /// The resulting seq has the same element count as the input Rarr.
        let iPrevThisNext (rarr:ResizeArray<'T>) =
            if rarr.Count <= 3 then EuclidException.Raise "Euclid.ResizeArr.iPrevThisNext input has less than three items:\r\n%O" rarr
            seq {   0, rarr.[rarr.Count-1], rarr.[0], rarr.[1]
                    for i = 0 to rarr.Count-3 do  i+1, rarr.[i], rarr.[i+1], rarr.[i+2]
                    rarr.Count-1, rarr.[rarr.Count-2], rarr.[rarr.Count-1], rarr.[0] }

        /// <summary>Sorts the elements of a ResizeArray, using the given projection for the keys and returning a new ResizeArr.
        /// Elements are compared using <see cref="M:Microsoft.FSharp.Core.Operators.compare"/>.
        /// This means "Z" is before "a". This is different from Collections.Generic.List.Sort() where "a" is before "Z" using IComparable interface.
        /// This is NOT a stable sort, i.e. the original order of equal elements is not necessarily preserved.
        /// For a stable sort, consider using Seq.sort.</summary>
        /// <param name="projection">The function to transform ResizeArray elements into the type that is compared.</param>
        /// <param name="xs">The input ResizeArr.</param>
        /// <returns>The sorted ResizeArr.</returns>
        let sortBy<'T, 'Key when 'Key : comparison> (projection : 'T -> 'Key) (xs : ResizeArray<'T>) : ResizeArray<'T> =
            let r = xs.GetRange(0, xs.Count) // fastest way to create a shallow copy
            r.Sort (fun x y -> Operators.compare (projection x) (projection y))
            r

        /// <summary>Returns the index of the first element in the ResizeArray
        /// that satisfies the given predicate.</summary>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="xs">The input ResizeArr.</param>
        /// <returns>The index of the first element that satisfies the predicate, or <c>None</c>.</returns>
        let tryFindIndex (predicate:'T->bool) (xs: ResizeArray<'T>) : option<int>=
            let elementIndex = xs.FindIndex (System.Predicate predicate)
            match elementIndex with
            | -1 ->
                None
            | index ->
                Some index

        /// <summary>Returns a new ResizeArray with the elements in reverse order.</summary>
        /// <param name="xs">The input ResizeArr.</param>
        /// <returns>The reversed ResizeArr.</returns>
        let rev (xs: ResizeArray<'T>) =
            let len = xs.Count
            let result = ResizeArray (len)
            for i = len - 1 downto 0 do
                result.Add xs.[i]
            result

        /// <summary>Returns the index of the smallest of all elements of the ResizeArray, compared via Operators.max on the function result.</summary>
        /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
        /// <param name="xs">The input ResizeArr.</param>
        /// <returns>The index of the smallest element.</returns>
        let minIndexBy (projection : 'T -> 'Key) (xs: ResizeArray<'T>) : int =
            if xs.Count = 0 then EuclidException.Raise "Euclid.ResizeArr.minIndBy: Failed on empty ." // noReflection for Fable. <%O>" typeof<'T>
            let mutable f = projection xs.[0]
            let mutable mf = f
            let mutable ii = 0
            for i=1 to xs.Count-1 do
                f <- projection xs.[i]
                if f < mf then
                    ii <- i
                    mf <- f
            ii

        /// <summary>Returns the index of the greatest of all elements of the ResizeArray, compared via Operators.max on the function result.</summary>
        /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
        /// <param name="xs">The input ResizeArr.</param>
        /// <returns>The index of the maximum element.</returns>
        let maxIndexBy (projection : 'T -> 'Key) (xs: ResizeArray<'T>) : int =
            if xs.Count = 0 then EuclidException.Raise "Euclid.ResizeArr.maxIndBy: Failed on empty ResizeArray." // noReflection for Fable. <%O>" typeof<'T>
            let mutable f = projection xs.[0]
            let mutable mf = f
            let mutable ii = 0
            for i=1 to xs.Count-1 do
                f <- projection xs.[i]
                if f > mf then
                    ii <- i
                    mf <- f
            ii

        /// <summary>Builds a new ResizeArray whose elements are the results of applying the given function
        /// to each of the elements of the ResizeArr.</summary>
        /// <param name="mapping">The function to transform elements of the ResizeArr.</param>
        /// <param name="xs">The input ResizeArr.</param>
        /// <returns>The ResizeArray of transformed elements.</returns>
        let map (mapping: 'T -> 'U) (xs: ResizeArray<'T>) : ResizeArray<'U> =
            #if FABLE_COMPILER
            let r = ResizeArray(xs.Count)
            for x in xs do
                r.Add(mapping x)
            r
            #else
            xs.ConvertAll (System.Converter mapping) // not supported in Fable
            #endif
            
            

[<AutoOpen>]
module internal AutoOpenResizeArrayExtensions =

    type Collections.Generic.List<'T> with

        /// Gets the index of the last item in the ResizeArr.
        /// Equal to this.Count - 1
        member inline this.LastIndex =
            #if DEBUG
            if this.Count = 0 then EuclidException.Raise "Euclid.ResizeArr.LastIndex: Failed to get LastIndex of of empty ResizeArray." // noReflection for Fable. <%O>" typeof<'T>
            #endif
            this.Count - 1

        /// Get (or set) the last item in the ResizeArr.
        /// Equal to this.[this.Count - 1]
        member inline this.Last
            with get() =
                #if DEBUG
                if this.Count = 0 then EuclidException.Raise "Euclid.ResizeArr.Last: Failed to get last item of empty ResizeArray." // noReflection for Fable. <%O>" typeof<'T>
                #endif
                this.[this.Count - 1]
            and set (v:'T) =
                #if DEBUG
                if this.Count = 0 then EuclidException.Raise "Euclid.ResizeArr.Last: Failed to set last item of empty ResizeArray." // noReflection for Fable. <%O> to %O" typeof<'T> v
                #endif
                this.[this.Count - 1] <- v

        /// Get (or set) the second last item in the ResizeArr.
        /// Equal to this.[this.Count - 2]
        member inline this.SecondLast
            with get() =
                #if DEBUG
                if this.Count < 2 then  EuclidException.Raise "Euclid.ResizeArr.SecondLast: Failed to get second last item of ResizeArray." // noReflection for Fable. <%O>" typeof<'T>
                #endif
                this.[this.Count - 2]
            and set (v:'T) =
                #if DEBUG
                if this.Count < 2 then  EuclidException.Raise "Euclid.ResizeArr.SecondLast: Failed to set second last item of ResizeArray." // noReflection for Fable. <%O> to %O" typeof<'T> v
                #endif
                this.[this.Count - 2] <- v

        /// Get (or set) the first item in the Euclid.ResizeArr.
        /// Equal to this.[0]
        member inline this.First
            with get() =
                #if DEBUG
                if this.Count = 0 then EuclidException.Raise "Euclid.ResizeArr.First: Failed to get first item of empty ResizeArray." // noReflection for Fable. <%O>" typeof<'T>
                #endif
                this.[0]
            and set (v:'T) =
                #if DEBUG
                if this.Count = 0 then EuclidException.Raise "Euclid.ResizeArr.First: Failed to set first item of empty ResizeArray." // noReflection for Fable. <%O> to %O" typeof<'T> v
                #endif
                this.[0] <- v

        /// Get (or set) the second item in the ResizeArr.
        /// Equal to this.[1]
        member inline this.Second
            with get() =
                #if DEBUG
                if this.Count < 2 then EuclidException.Raise "Euclid.ResizeArr.Second: Failed to get second item of ResizeArray." // noReflection for Fable. <%O>" typeof<'T>
                #endif
                this.[1]
            and set (v:'T) =
                #if DEBUG
                if this.Count < 2 then EuclidException.Raise "Euclid.ResizeArr.Second: Failed to set second item of ResizeArray." // noReflection for Fable. <%O> to %O" typeof<'T> v
                #endif
                this.[1] <- v


        /// Get and remove last item from ResizeArr.
        member inline this.Pop() =
            #if DEBUG
            if this.Count=0 then EuclidException.Raise "Euclid.ResizeArr.Pop() failed for empty ResizeArray." // noReflection for Fable. <%O>" typeof<'T>
            #endif
            let i = this.Count - 1
            let v = this.[i]
            this.RemoveAt(i)
            v

        /// Get and remove item at index from ResizeArr.
        member inline this.Pop(index:int) =
            #if DEBUG
            if index < 0  then EuclidException.Raise "Euclid.ResizeArr.Pop %O failed for ResizeArray of %O items, index must be positive." index  this.Count
            if index >= this.Count then EuclidException.Raise "Euclid.ResizeArr.Pop %O failed for ResizeArray of %O items." index this.Count
            #endif
            let v = this.[index]
            this.RemoveAt(index)
            v
