namespace Euclid

open System
open EuclidErrors


module internal Arr =

    /// Returns the index of the smallest element.
    let minIndex (xs:'T[]) =
        if xs.Length < 1 then fail "Arr.minIndex failed on empty array."
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
        if xs.Length < 1 then fail "Arr.maxIndex failed on empty array."
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
    let inline create (count:int) (x:'T) : ResizeArray<'T> =
        let r = new ResizeArray<'T>(count)
        for i=0 to count-1 do r.Add(x)
        r

    /// this.Count.
    let inline length (xs: ResizeArray<'T>) :int =
        xs.Count

    let inline init (count:int) (f:int->'T) : ResizeArray<'T> =
        #if FABLE_COMPILER
            // https://fable.io/docs/javascript/features.html#emitjsexpr
            Fable.Core.JsInterop.emitJsStatement (count, f) "
            const xs = new Array($0); 
            for (let i = 0; i < $0; i++) 
                { xs[i] = $1(i); }; 
            return xs "
        #else
            let r = new ResizeArray<'T>(count)
            for i=0 to count-1 do r.Add(f i)
            r
        #endif

    let inline singleton (x:'T) : ResizeArray<'T> =
        #if FABLE_COMPILER
            Fable.Core.JsInterop.emitJsExpr (x) "[ $0 ]"
        #else
            let r = new ResizeArray<'T>(1)
            r.Add(x)
            r
        #endif

    /// Yields looped Seq from (first, second)  up to (last, first).
    /// The resulting seq has the same element count as the input Rarr.
    let thisNext (rarr:ResizeArray<'T>) =
        if rarr.Count <= 2 then fail $"ResizeArr.thisNext input has less than two items:{Format.nl}{Format.rarr rarr}"
        seq {   for i = 0 to rarr.Count-2 do  rarr.[i], rarr.[i+1]
                rarr.[rarr.Count-1], rarr.[0] }

    /// Yields looped Seq from (1, last, first, second)  up to (lastIndex, second-last, last, first)
    /// The resulting seq has the same element count as the input Rarr.
    let iPrevThisNext (rarr:ResizeArray<'T>) =
        if rarr.Count <= 3 then fail $"ResizeArr.iPrevThisNext input has less than three items:{Format.nl}{Format.rarr rarr}"
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

    /// <summary>Returns the index of the first element in the ResizeArray
    /// that satisfies the given predicate.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="xs">The input ResizeArr.</param>
    /// <returns>The index of the first element that satisfies the predicate, or -1 .</returns>
    let findIndex (predicate:'T->bool) (xs: ResizeArray<'T>) : int=
        xs.FindIndex (System.Predicate predicate)

    let find (predicate:'T->bool) (xs: ResizeArray<'T>) : 'T =
        xs.Find (System.Predicate predicate)

    let findLast (predicate:'T->bool) (xs: ResizeArray<'T>) : 'T =
        let mutable loopOn = false
        let mutable i = xs.Count - 1
        while loopOn do
            if predicate xs.[i] then
                loopOn <- false
            else
                i <- i - 1
                if i < 0 then
                    fail $"ResizeArr.findLast: None of the {xs.Count} elements satisfies the predicate."
        xs.[i]

    let findLastIndex (predicate:'T->bool) (xs: ResizeArray<'T>) : int =
        let mutable r = -1
        let mutable i = xs.Count - 1
        while r = -1 && i >= 0 do
            if predicate xs.[i] then
                r <- i
            i <- i - 1
        r


    /// <summary>Returns a new ResizeArray with the elements in reverse order.</summary>
    /// <param name="xs">The input ResizeArr.</param>
    /// <returns>The reversed ResizeArr.</returns>
    let rev (xs: ResizeArray<'T>) : ResizeArray<'T> =
        #if FABLE_COMPILER
            // https://fable.io/docs/javascript/features.html#emitjsexpr
            Fable.Core.JsInterop.emitJsStatement (xs) "
            const xs = new Array($0.length);
            const lastIdx = $0.length - 1; 
            for (let i = 0; i < $0.length; i++) 
                { xs[i] = $0[lastIdx - i]; }; 
            return xs "
        #else
            let len = xs.Count
            let result = ResizeArray (len)
            for i = len - 1 downto 0 do
                result.Add xs.[i]
            result
        #endif

    /// <summary>Returns the index of the smallest of all elements of the ResizeArray, compared via Operators.max on the function result.</summary>
    /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
    /// <param name="xs">The input ResizeArr.</param>
    /// <returns>The index of the smallest element.</returns>
    let minIndexBy (projection : 'T -> 'Key) (xs: ResizeArray<'T>) : int =
        if xs.Count = 0 then fail "ResizeArr.minIndBy: Failed on empty ResizeArray." // noReflection for Fable. <%O>" typeof<'T>
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
        if xs.Count = 0 then fail "ResizeArr.maxIndBy: Failed on empty ResizeArray." // noReflection for Fable. <%O>" typeof<'T>
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
        // https://fable.io/docs/javascript/features.html#emitjsexpr
        Fable.Core.JsInterop.emitJsExpr (xs, mapping) "$0.map($1)"
        #else
        xs.ConvertAll (System.Converter mapping) // not supported in Fable
        #endif

    /// Creates a shallow copy of the input ResizeArray and adds the first element to the end, closing the loop.
    let closeLoop (pts: ResizeArray<'T>) =
        #if FABLE_COMPILER
            Fable.Core.JsInterop.emitJsStatement (xs) "
            const xs = new Array($0.length+1);            
            for (let i = 0; i < $0.length; i++) 
                { xs[i] = $0[i]; }; 
            xs[$0.length] = $0[0];
            return xs "
        #else
            let r = ResizeArray<'T>(pts.Count + 1)
            r.AddRange(pts)
            r.Add(pts.[0])
            r
        #endif



    /// <summary>Returns the last element for which the given function returns <c>true</c>.
    /// Return None if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="resizeArray">The input ResizeArray.</param>
    /// <returns>The last element that satisfies the predicate, or <c>None</c>.</returns>
    let tryFindBack (predicate: 'T -> bool) (resizeArray: ResizeArray<'T>) : option<'T> =
        let mutable i = resizeArray.Count - 1
        let mutable result = None
        while i >= 0  do
            let element = resizeArray.[i]
            i <- i - 1
            if predicate element then
                result <- Some element
                i <- -1 // break the loop
        result


[<AutoOpen>]
module internal AutoOpenResizeArrayExtensions =

    type Collections.Generic.List<'T> with

        /// Gets the index of the last item in the ResizeArr.
        /// Equal to this.Count - 1
        member inline this.LastIndex =
            // #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
            // if this.Count = 0 then fail "ResizeArr.LastIndex: Failed to get LastIndex of of empty ResizeArray." // noReflection for Fable. <%O>" typeof<'T>
            // #endif
            this.Count - 1

        /// Get (or set) the last item in the ResizeArr.
        /// Equal to this.[this.Count - 1]
        member inline this.Last
            with get() =
                #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
                    if this.Count = 0 then fail "ResizeArr.Last: Failed to get last item of empty ResizeArray." // noReflection for Fable. <%O>" typeof<'T>
                #endif
                    this.[this.Count - 1]
            and set (v:'T) =
                #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
                    if this.Count = 0 then fail "ResizeArr.Last: Failed to set last item of empty ResizeArray." // noReflection for Fable. <%O> to %O" typeof<'T> v
                #endif
                    this.[this.Count - 1] <- v

        /// Get (or set) the second last item in the ResizeArr.
        /// Equal to this.[this.Count - 2]
        member inline this.SecondLast
            with get() =
                #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
                    if this.Count < 2 then fail "ResizeArr.SecondLast: Failed to get second last item of ResizeArray." // noReflection for Fable. <%O>" typeof<'T>
                #endif
                    this.[this.Count - 2]
            and set (v:'T) =
                #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
                    if this.Count < 2 then fail "ResizeArr.SecondLast: Failed to set second last item of ResizeArray." // noReflection for Fable. <%O> to %O" typeof<'T> v
                #endif
                    this.[this.Count - 2] <- v


        /// Get (or set) the second last item in the ResizeArr.
        /// Equal to this.[this.Count - 2]
        member inline this.ThirdLast
            with get() =
                #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
                    if this.Count < 3 then fail "ResizeArr.ThirdLast: Failed to get third last item of ResizeArray." // noReflection for Fable. <%O>" typeof<'T>
                #endif
                    this.[this.Count - 3]
            and set (v:'T) =
                #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
                    if this.Count < 3 then fail "ResizeArr.SeconThirdLastdLast: Failed to set third last item of ResizeArray." // noReflection for Fable. <%O> to %O" typeof<'T> v
                #endif
                    this.[this.Count - 3] <- v

        /// Get (or set) the first item in the Euclid.ResizeArr.
        /// Equal to this.[0]
        member inline this.First
            with get() =
                #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
                    if this.Count = 0 then fail "ResizeArr.First: Failed to get first item of empty ResizeArray." // noReflection for Fable. <%O>" typeof<'T>
                #endif
                    this.[0]
            and set (v:'T) =
                #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
                    if this.Count = 0 then fail "ResizeArr.First: Failed to set first item of empty ResizeArray." // noReflection for Fable. <%O> to %O" typeof<'T> v
                #endif
                    this.[0] <- v

        /// Get (or set) the second item in the ResizeArr.
        /// Equal to this.[1]
        member inline this.Second
            with get() =
                #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
                    if this.Count < 2 then fail "ResizeArr.Second: Failed to get second item of ResizeArray." // noReflection for Fable. <%O>" typeof<'T>
                #endif
                    this.[1]
            and set (v:'T) =
                #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
                    if this.Count < 2 then fail "ResizeArr.Second: Failed to set second item of ResizeArray." // noReflection for Fable. <%O> to %O" typeof<'T> v
                #endif
                    this.[1] <- v

        /// Get (or set) the third item in the ResizeArr.
        /// Equal to this.[2]
        member inline this.Third
            with get() =
                #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
                    if this.Count < 3 then fail "ResizeArr.Third: Failed to get third item of ResizeArray." // noReflection for Fable. <%O>" typeof<'T>
                #endif
                    this.[2]
            and set (v:'T) =
                #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
                    if this.Count < 3 then fail "ResizeArr.Third: Failed to set third item of ResizeArray." // noReflection for Fable. <%O> to %O" typeof<'T> v
                #endif
                    this.[2] <- v


        /// Get and remove last item from ResizeArr.
        member inline this.Pop() =
            #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
                if this.Count=0 then fail "ResizeArr.Pop() failed for empty ResizeArray." // noReflection for Fable. <%O>" typeof<'T>
            #endif
                let i = this.Count - 1
                let v = this.[i]
                this.RemoveAt(i)
                v

        /// Get and remove item at index from ResizeArr.
        member inline this.Pop(index:int) =
            #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
                if index < 0 then
                    fail $"ResizeArr.Pop {index} failed for ResizeArray of {this.Count} items, index must be positive."
                if index >= this.Count then
                    fail $"ResizeArr.Pop {index} failed for ResizeArray of {this.Count} items, index must be less than the count."
            #endif
                let v = this.[index]
                this.RemoveAt(index)
                v

        /// Any integer becomes a valid index, even negative.
        /// -1 is the last item, -2 is the second last, etc.
        member inline this.GetLooped(i)=
            let t = i % this.Count
            if t >= 0 then this.[t]
            else           this.[t + this.Count]


        member inline this.SetIdx i v =
            #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
                if i < 0 || i >= this.Count then
                    fail $"ResizeArr.Set: Failed to set item at index {i} in ResizeArray of {this.Count} items."
            #endif
                this.[i] <- v

        /// true if this.Count = 0
        member inline this.IsEmpty  =
            this.Count = 0

