#r "nuget: Str,  0.23.0"
#r "nuget: Fesher, 0.5.0"
#r "nuget: ResizeArrayT,  0.26.0"

open Str 
open System
open Fesher
open System.IO
open ResizeArrayT


let files =
    [|
    // // ("EuclidErrors.fs")
    // // ("UtilEuclid.fs")
    // // ("Format.fs")
    // // ("Vc.fs")
    // // ("UnitVc.fs")
    // // ("Pt.fs")
    // // ("Vec.fs")
    // // ("UnitVec.fs")
    // // ("Pnt.fs")
    "Rotation2D.fs"
    "Quaternion.fs"
    "PPlane.fs"
    "Matrix.fs"
    "RigidMatrix.fs"
    "Line2D.fs"
    "Line3D.fs"

    // // ("XLine2D.fs")
    // // ("XLine3D.fs")

    // "TypeExtensions/UnitVc.fs"
    // "TypeExtensions/Vc.fs"
    // "TypeExtensions/Pt.fs"
    // "TypeExtensions/UnitVec.fs"
    // "TypeExtensions/Vec.fs"
    // "TypeExtensions/Pnt.fs"
    "TypeExtensions/PPlane.fs"
    // // ("TypeExtensions/Matrix.fs")
    "TypeExtensions/Quaternion.fs"
    "TypeExtensions/Line2D.fs"
    "TypeExtensions/Line3D.fs"

    "BRect.fs"
    "BBox.fs"
    "NPlane.fs"
    "Rect2D.fs"
    "Rect3D.fs"
    "Box.fs"
    "FreeBox.fs"
    // // ("Tria2D.fs")
    // // ("Tria3D.fs")

    // // ("ResizeArr.fs")

    // // ("Points2D.fs")
    // // ("Points3D.fs")
    // // ("Offset2D.fs")
    // // ("Offset3D.fs")
    // // ("PolyLabel.fs")
    "Polyline2D.fs"
    "Polyline3D.fs"
    // // ("Topology2D.fs")
    // // ("Topology3D.fs")
    // // ("Similarity2D.fs")
    |]
    |> Array.map (fun f -> $"D:/Git/_Euclid_/Euclid/Src/{f}")


type State =
    |InDoc
    |InBody

type Member =
    {
    line: int
    pos: int
    name: string
    obsolete: bool
    isTupled: bool
    static': bool
    lines: ResizeArray<string>
    declLine: string
    mutable desiredPos: float
    }

let mainLine(ls:ResizeArray<string>) =
    ls
    |> ResizeArray.tryFind (fun ln -> ln.Trim().StartsWith "member " || ln.Trim().StartsWith "static " || ln.Trim().Contains "inline ( *** )" )
    |> Option.map (fun ln -> ln.Trim())
    |> Option.defaultValue "?missing main line?"


let cleanLines(ls:ResizeArray<string>) =
    while String.IsNullOrWhiteSpace ls.Last  &&  String.IsNullOrWhiteSpace ls.SecondLast do
        ls.Pop() |> ignore
    if not (String.IsNullOrWhiteSpace ls.Last) then
        ls.Add "" // add a blank line at the end of the member for better diffing



let getTillEq(ln:string, lns:string[],i:int) =
    let rec loop (j:int) (s :string)=
        if s.Contains "=" || j >= lns.Length - 1 then
            s.Trim()
        else
            let nextLn = lns[j+1]
            loop (j+1) (s + " " + nextLn.Trim())
    loop i ln




let getMembers(file:string) =
    let lns = File.ReadAllLines(file)
    let mems = ResizeArray<Member>()
    let mutable temp = ResizeArray<string>()
    let mutable state = InDoc
    let mutable name = ""
    let mutable static' = false
    let mutable isTupled = false
    let mutable pos = 1
    let mutable lineNo = 1
    let mutable declLine = ""

    let mutable i = 0
    while i < lns.Length do
        let ln = lns[i]
        let lt = ln.Trim()
        match state with
        | InBody ->
            if lt.StartsWith "///" || lt.StartsWith "[<Obsolete" || lt.StartsWith "[<Data" then
                // the next member starts when we hit a docstring, or an attribute, or the end of the file
                // so now save what we collected:
                if name <> "" then
                    let obso =
                        mems.Count > 0 // if a constructor is obsolete don't count them
                        &&  temp |> ResizeArray.exists(Str.contains "[<Obsolete")
                    mems.Add { name = name; static' = static'; obsolete = obso; isTupled = isTupled; lines = temp; pos = pos; line = lineNo; declLine = declLine; desiredPos = float pos }
                    temp <- ResizeArray()
                    name <- ""
                    pos <- pos + 1
                    lineNo <- i + 1
                state <- InDoc

            temp.Add ln

        | InDoc ->
            if lt.Contains "inline ( *** )" then
                state <- InBody
                name <- "( *** )"
                static' <- true
                isTupled <- true
                declLine <- getTillEq(lt, lns, i)

            elif lt.StartsWith "member " ||  lt.StartsWith "static "||  lt.StartsWith "override " then
                let n =
                    lt
                    |> Str.delete "member"
                    |> Str.delete "inline"
                    |> Str.delete "static"
                    |> Str.delete "override"
                    |> Str.trimStart
                    |> Str.replaceChar '(' ' '
                    |> Str.replaceChar '=' ' '
                    |> fun s -> if s.Contains " " then Str.beforeChar ' ' s else s
                    |> fun s -> if s.Contains "." then Str.afterChar '.' s else s
                state <- InBody
                name <- n
                static' <- lt.StartsWith "static "
                isTupled <- lt.Contains ","
                declLine <- getTillEq(lt, lns, i)

            temp.Add ln

        i <- i + 1

    // last member
    if name <> "" then
        let obso = temp |> ResizeArray.exists(Str.contains "[<Obsolete")
        mems.Add { name = name; static' = static'; obsolete = obso; isTupled = isTupled; lines = temp; pos = pos; line = lineNo; declLine = declLine; desiredPos = float pos }
        temp <- ResizeArray()
        name <- ""


    for m in mems do
        cleanLines m.lines

    mems


let resort(mems: ResizeArray<Member>, file: string) =
    let mutable any = false

    for t,n in ResizeArray.thisNext mems do
        if t.obsolete then
            t.desiredPos <- t.desiredPos + 12000.0

        elif not t.static' then
            if n.name <> Str.low1 t.name then
                match mems |> ResizeArray.tryFind (fun m -> m.static' &&  m.name = Str.low1 t.name )  with
                | Some m ->
                    let hasOverload = mems |>  ResizeArray.countIf (fun m -> m.name = t.name)  > 1

                    if not hasOverload && abs (m.pos - n.pos) > 1 then
                        any <- true
                        Printf.orange $"this.{t.name} "
                        Printfn.red $"should be at {n.pos} not {m.pos}: {file}:{m.line}"
                        m.desiredPos <- t.desiredPos + 0.3
                | None ->
                    Printfn.gray $" no static for .{t.name}"

    any <- false // for dry run
    if any then
        mems
        |> ResizeArray.sortBy (fun m -> m.desiredPos)
        |> ResizeArray.map (fun m -> m.lines)
        |> ResizeArray.concat
        |> fun lns -> File.WriteAllLines(file, lns, Text.Encoding.UTF8)
        Printfn.green $"Sorted {file}"


let missingStatics(mems: ResizeArray<Member>, file: string) =
    let mutable headDone = false
    for t,n in ResizeArray.thisNext mems do
        if not n.obsolete && not n.static' then
            let stn = Str.low1 t.name

            if mems|> ResizeArray.notExists(fun m -> m.name = stn ) && t.name <> "ToString" then
                if not headDone then
                    let relPath = file |> Str.delete "D:/Git/_Euclid_/Euclid/"
                    Printfn.purple $"\nin ./{relPath} \nadd these static mebers using curried syntax and reversed argument order. 
                        Just below the corresponding instance member
                        using the same docstring ///
                        in the body call the instance member 
                        "
                    headDone <- true

                Printfn.orange $"static meber inline {Str.low1 t.name} (just below .{t.name}) "

let missingInstance(mems: ResizeArray<Member>, file: string) =
    let mutable headDone = false
    for t,n in ResizeArray.thisNext mems do
        if not n.obsolete && n.static' then
            let inName = Str.up1 t.name

            if mems|> ResizeArray.notExists(fun m -> m.name = inName) then
                if not headDone then
                    let relPath = file |> Str.delete "D:/Git/_Euclid_/Euclid/"
                    Printfn.purple $"\nin ./{relPath} \nadd these instance mebers using tupled syntax and reversed argument order. 
                        Just above the corresponding static member
                        using the same docstring ///
                        in the body call the static member 
                        "
                    headDone <- true

                Printfn.orange $"meber inline this.{Str.up1 t.name} (just above static member {t.name}) "


let deDuplicate(mems: ResizeArray<Member>, file: string, write: bool) =
    let deDupMem = ResizeArray()
    let md = Collections.Generic.Dictionary<string,Member>()
    for m in mems do
        if m.static' && not m.isTupled then  // instance members can have duplicates. = overloads
            if md.ContainsKey m.name then
                let other = md[m.name]
                if not <| ResizeArray.equals m.lines other.lines then
                    Printfn.red $"Not exact Dup: {mainLine  m.lines}"
                    if m.lines.Count = other.lines.Count then
                        for t,o in ResizeArray.zip m.lines other.lines do
                            if t <> o then
                                Printfn.green $"> {o}"
                                Printfn.blue  $"> {t}"
                    else
                        for o in other.lines do Printfn.green $"> {o}"
                        for t in m.lines do Printfn.blue $"> {t}"
                    //deDupMem.Add m // keep this, do manual check ??
                else
                    Printfn.gray $"Dup to delete: {mainLine m.lines}"
            else
                md.Add(m.name, m)
                deDupMem.Add m
        else // its an instance member, or a tupled static member
            deDupMem.Add m

    if deDupMem.Count <> mems.Count then
        eprintfn $"{mems.Count-deDupMem.Count} duplicates"

    if write then
        deDupMem
        |> ResizeArray.sortBy (fun m -> m.desiredPos)
        |> ResizeArray.map (fun m -> m.lines)
        |> ResizeArray.concat
        |> fun lns -> File.WriteAllLines(file, lns, Text.Encoding.UTF8)
        Printfn.green $"Dedup {file}"


let compareAPI(fileA:string, fileB:string) =
    let memsA = getMembers fileA
    let memsB = getMembers fileB

    let namesA = memsA |> ResizeArray.map (fun m -> m.name.Replace("XYZ", "XY").Replace("In2D", ""))
    let namesB = memsB |> ResizeArray.map (fun m -> m.name.Replace("XYZ", "XY").Replace("In2D", ""))


    let onlyInA = namesA |> ResizeArray.filter (fun n -> not <| namesB.Contains n)
    let onlyInB = namesB |> ResizeArray.filter (fun n -> not <| namesA.Contains n)

    if onlyInA.Count > 0 then
        Printfn.red $"\nOnly in {fileA}"
        for n in onlyInA do Printfn.red $"  {n}"

    if onlyInB.Count > 0 then
        Printfn.blue $"\nOnly in {fileB}"
        for n in onlyInB do Printfn.blue $"  {n}"


let filterMembers(mems: ResizeArray<Member>, file: string) =
    for m in mems do
        if not m.obsolete then
            let dLn = m.declLine.Trim()
            let dlnm = dLn|> Str.replaceLast ":" "%" // % is a marker
            
            
            match Str.tryBetween "%" "=" dlnm with
            | None ->
                Printfn.red $"missing type annotation: {dLn}"
            | Some rt ->
                if rt.Trim() = "float" && (dLn.Contains "Pt)" || dLn.Contains "Pnt)")   then
                    Printfn.blue $" - {dLn}"
                
                if rt.Trim() = "bool" && (dLn.Contains "Pt)" || dLn.Contains "Pnt)")   then
                    Printfn.green $" - {dLn}"


for file in files do
    Printfn.orchid $"\n{file}"
    let mems = getMembers file
    // filterMembers(mems, file)
    // missingStatics(mems, file)
    // missingInstance(mems, file)
    resort(mems, file)
    // deDuplicate(mems, file, false)
    ()


// compareAPI("D:/Git/_Euclid_/Euclid/Src/Polyline2D.fs", "D:/Git/_Euclid_/Euclid/Src/Polyline3D.fs")
printfn "Done"

























































