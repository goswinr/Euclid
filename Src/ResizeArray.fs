namespace FsEx.Geo

open System

[<AutoOpen>]
module AutoOpenResizeArray =

    type Collections.Generic.List<'T> with
    
        /// Gets the index of the last item in the FsEx.Geo.ResizeArray.
        /// Equal to this.Count - 1
        member inline this.LastIndex = 
            #if DEBUG
            if this.Count = 0 then FsExGeoException.Raise $"FsEx.Geo.ResizeArray.LastIndex: Failed to get LastIndex of of empty ResizeArray<{typeof<'T>}>" 
            #endif
            this.Count - 1

        /// Get (or set) the last item in the FsEx.Geo.ResizeArray.
        /// Equal to this.[this.Count - 1]
        member inline this.Last
            with get() = 
                #if DEBUG
                if this.Count = 0 then FsExGeoException.Raise $"FsEx.Geo.ResizeArray.Last: Failed to get last item of empty ResizeArray<{typeof<'T>}>" 
                #endif
                this.[this.Count - 1]
            and set (v:'T) = 
                #if DEBUG
                if this.Count = 0 then FsExGeoException.Raise $"FsEx.Geo.ResizeArray.Last: Failed to set last item of empty ResizeArray<{typeof<'T>}> to {v}" 
                #endif
                this.[this.Count - 1] <- v

        /// Get (or set) the first item in the FsEx.Geo.ResizeArray.
        /// Equal to this.[0]
        member inline this.First
            with get() = 
                #if DEBUG
                if this.Count = 0 then FsExGeoException.Raise $"FsEx.Geo.ResizeArray.First: Failed to get first item of empty ResizeArray<{typeof<'T>}>" 
                #endif
                this.[0]
            and set (v:'T) = 
                #if DEBUG
                if this.Count = 0 then FsExGeoException.Raise $"FsEx.Geo.ResizeArray.First: Failed to set first item of empty ResizeArray<{typeof<'T>}> to {v}" 
                #endif
                this.[0] <- v


        /// Get and remove last item from ResizeArray
        member this.Pop()  = 
            #if DEBUG
            if this.Count=0 then FsExGeoException.Raise $" rarr.Pop() failed for empty ResizeArray<{typeof<'T>}>"
            #endif
            let i = this.Count - 1
            let v = this.[i]
            this.RemoveAt(i)
            v

        /// Get and remove item at index from ResizeArray
        member this.Pop(index:int)  = 
            #if DEBUG
            if index < 0  then FsExGeoException.Raise $"FsEx.Geo.ResizeArray.Pop{index} failed for ResizeArray<{typeof<'T>}> of{this.Count} items, index must be positive." 
            if index >= this.Count then FsExGeoException.Raise $"FsEx.Geo.ResizeArray.Pop{index} failed for ResizeArray<{typeof<'T>}> of{this.Count} items." 
            #endif
            let v = this.[index]
            this.RemoveAt(index)
            v
        
        // -------------------------------------------------------
        // --------------------Static members --------------------
        // -------------------------------------------------------

        /// this.Count
        static member inline length (rarr: ResizeArray<'T>) = 
            rarr.Count           

        /// <summary>Sorts the elements of a ResizeArray, using the given projection for the keys and returning a new ResizeArray.
        /// Elements are compared using <see cref="M:Microsoft.FSharp.Core.Operators.compare"/>.
        /// This means "Z" is before "a". This is different from Collections.Generic.List.Sort() where "a" is before "Z" using IComparable interface.
        /// This is NOT a stable sort, i.e. the original order of equal elements is not necessarily preserved.
        /// For a stable sort, consider using Seq.sort.</summary>
        /// <param name="projection">The function to transform ResizeArray elements into the type that is compared.</param>
        /// <param name="rarr">The input ResizeArray.</param>
        /// <returns>The sorted ResizeArray.</returns>
        static member sortBy<'T, 'Key when 'Key : comparison> (projection : 'T -> 'Key) (rarr : ResizeArray<'T>) : ResizeArray<'T> = 
            let r = rarr.GetRange(0,rarr.Count) // fastest way to create a shallow copy
            r.Sort (fun x y -> Operators.compare (projection x) (projection y))
            r
        
        /// <summary>Returns the index of the first element in the ResizeArray
        /// that satisfies the given predicate.</summary>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="rarr">The input ResizeArray.</param>
        /// <returns>The index of the first element that satisfies the predicate, or <c>None</c>.</returns>
        static member  tryFindIndex (predicate:'T->bool) (rarr: ResizeArray<'T>) : option<int>= 
            let elementIndex =   rarr.FindIndex (System.Predicate predicate)
            match elementIndex with
            | -1 ->
                None
            | index ->
                Some index

        /// <summary>Returns a new ResizeArray with the elements in reverse order.</summary>
        /// <param name="rarr">The input ResizeArray.</param>
        /// <returns>The reversed ResizeArray.</returns>
        static member rev (rarr: ResizeArray<'T>) = 
            let len = rarr.Count
            let result = ResizeArray (len)
            for i = len - 1 downto 0 do
                result.Add rarr.[i]
            result

        /// <summary>Returns the inedx of the smallest of all elements of the ResizeArray, compared via Operators.max on the function result.</summary>
        /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
        /// <param name="rarr">The input ResizeArray.</param>        
        /// <returns>The index of the smallest element.</returns>
        static member minIndBy  (projection : 'T -> 'Key) (rarr: ResizeArray<'T>) : int = 
            if rarr.Count = 0 then FsExGeoException.Raise $"FsEx.Geo.ResizeArray.minIndBy: Failed on empty ResizeArray<{typeof<'T>}>" 
            let mutable f = projection rarr.[0]
            let mutable mf = f
            let mutable ii = 0
            for i=1 to rarr.Count-1 do
                f <- projection rarr.[i]
                if f < mf then
                    ii <- i
                    mf <- f
            ii 
          
        /// <summary>Returns the inedx of the greatest of all elements of the ResizeArray, compared via Operators.max on the function result.</summary>
        /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
        /// <param name="rarr">The input ResizeArray.</param>        
        /// <returns>The index of the maximum element.</returns>
        static member  maxIndBy (projection : 'T -> 'Key) (rarr: ResizeArray<'T>) : int =
            if rarr.Count = 0 then FsExGeoException.Raise $"FsEx.Geo.ResizeArray.maxIndBy: Failed on empty ResizeArray<{typeof<'T>}>" 
            let mutable f = projection rarr.[0]
            let mutable mf = f
            let mutable ii = 0
            for i=1 to rarr.Count-1 do
                f <- projection rarr.[i]
                if f > mf then
                    ii <- i
                    mf <- f
            ii
