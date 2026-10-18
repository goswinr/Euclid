// Generates a Markdown overview of the public API of Euclid.
// Corresponding 2D and 3D types share one four column table, so that members of the same name sit side by side.
// For this, "Pt" and "Pnt" in a name count as the same, so that Pt.midPt sits next to Pnt.midPnt.
// Static members are listed right below the instance member of the same name.
//
// Usage, from the repo root:
//     dotnet fsi Scripts/ApiTable.fsx [output.md]
// The default output is Scripts/ApiTable.md.
// Next to it, ApiTable2D.md and ApiTable3D.md list only the 2D or only the 3D geometry types, without the general utilities.
//
// The script type checks the files listed in Euclid.fsproj with FSharp.Compiler.Service.
// So it needs no build and always reflects the current sources.

#r "nuget: FSharp.Compiler.Service, 43.12.401"

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Xml.Linq
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text

// #region Configuration

/// Set to TRUE to also list types and members marked with [<Obsolete>].
let includeObsolete = false

/// 2D names whose 3D counterpart does not follow the rule of replacing "2D" with "3D".
let explicitPairs =
    dict [
        "Pt"        , "Pnt"
        "Vc"        , "Vec"
        "UnitVc"    , "UnitVec"
        "BRect"     , "BBox"
        "Rotation2D", "Quaternion"
        "XLineXY"   , "XLineXYZ"
        "XPt"       , "XPnt"
        ]

/// Types that only exist in 3D and do not have "3D" in their name.
let only3D = set [ "Box"; "FreeBox"; "PPlane"; "NPlane"; "Matrix"; "RigidMatrix" ]

/// The name used to pair 2D with 3D types and members.
/// "Pt" and "Pnt" count as the same, so that e.g. Pt.midPt sits next to Pnt.midPnt and XLine2D.ClPts next to XLine3D.ClPnts.
let pairingName (name: string) = name.Replace("Pnt", "Pt")

// #endregion
// #region Type checking

let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))
let fsproj = Path.Combine(root, "Euclid.fsproj")

let outFile =
    match fsi.CommandLineArgs |> Array.tail with
    | [| path |] -> Path.GetFullPath path
    | _ -> Path.Combine(__SOURCE_DIRECTORY__, "ApiTable.md")

/// The source files in compile order.
let sourceFiles =
    XDocument.Load(fsproj).Descendants(XName.Get "Compile")
    |> Seq.map (fun e -> Path.GetFullPath(Path.Combine(root, e.Attribute(XName.Get "Include").Value)))
    |> Array.ofSeq

let checker = FSharpChecker.Create()

let projectOptions =
    // borrow the .NET framework references that fsi uses for scripts
    let scriptOptions, _ =
        checker.GetProjectOptionsFromScript(Path.Combine(root, "ApiTable.fsx"), SourceText.ofString "", assumeDotNetFramework = false)
        |> Async.RunSynchronously
    let references = scriptOptions.OtherOptions |> Array.filter (fun o -> o.StartsWith "-r:")
    { scriptOptions with
        ProjectFileName = fsproj
        SourceFiles = sourceFiles
        OtherOptions = Array.append references [| "--noframework"; "--targetprofile:netcore"; "--langversion:preview" |] }

printfn $"Type checking {sourceFiles.Length} files of {fsproj} ..."
let results = checker.ParseAndCheckProject projectOptions |> Async.RunSynchronously
let errors = results.Diagnostics |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
if errors.Length > 0 then
    for e in errors do eprintfn $"{e}"
    failwith $"Type checking Euclid failed with {errors.Length} errors."

// #endregion
// #region Formatting of signatures

let ctx = FSharpDisplayContext.Empty.WithShortTypeNames true

let fmt (t: FSharpType) = t.Format ctx

let isUnit (t: FSharpType) = fmt t = "unit"

/// Function and tuple types need parentheses when they are a single argument.
let fmtArg (t: FSharpType) =
    if t.IsFunctionType || t.IsTupleType || t.IsStructTupleType then $"({fmt t})" else fmt t

/// Function types need parentheses when they are returned by a member.
let fmtReturn (t: FSharpType) =
    if t.IsFunctionType then $"({fmt t})" else fmt t

let fmtParam (p: FSharpParameter) =
    match p.Name with
    | Some n when n <> "" ->
        let n = if p.IsOptionalArg then $"?{n}" else n
        let n = if p.IsParamArrayArg then $"[<ParamArray>] {n}" else n
        $"{n}: {fmtArg p.Type}"
    | _ -> fmtArg p.Type

/// A group of tupled parameters, e.g. 'x: float * y: float'.
let fmtGroup (group: Collections.Generic.IList<FSharpParameter>) =
    if group.Count = 0 || (group.Count = 1 && isUnit group.[0].Type) then "unit"
    else group |> Seq.map fmtParam |> String.concat " * "

/// The signature of a method, constructor, function or value, without its name.
let fmtFunction (m: FSharpMemberOrFunctionOrValue) =
    if m.CurriedParameterGroups.Count = 0 then
        fmt m.FullType
    else
        [ for g in m.CurriedParameterGroups do yield fmtGroup g
          yield fmtReturn m.ReturnParameter.Type ]
        |> String.concat " -> "

/// The signature of a property from its getter, e.g. 'float with get, set'.
let fmtProperty (getter: FSharpMemberOrFunctionOrValue) (hasSetter: bool) =
    let index = getter.CurriedParameterGroups |> Seq.concat |> Seq.filter (fun p -> not (isUnit p.Type)) |> List.ofSeq
    let access = if hasSetter then " with get, set" else ""
    let ty = fmtReturn getter.ReturnParameter.Type
    if index.IsEmpty then ty + access
    else (index |> List.map fmtParam |> String.concat " * ") + " -> " + ty + access

/// The signature of a property that only has a setter.
let fmtSetterOnly (setter: FSharpMemberOrFunctionOrValue) =
    let ps = setter.CurriedParameterGroups |> Seq.concat |> List.ofSeq
    let index = ps |> List.take (ps.Length - 1)
    let ty = fmt (List.last ps).Type
    if index.IsEmpty then $"{ty} with set"
    else (index |> List.map fmtParam |> String.concat " * ") + $" -> {ty} with set"

/// Operators are shown as '( + )', all other names as they are written in F#.
let displayName (m: FSharpMemberOrFunctionOrValue) =
    if m.DisplayName.StartsWith "(" then $"( {m.DisplayNameCore} )" else m.DisplayName

// #endregion
// #region Collecting the API

let isObsolete (attrs: seq<FSharpAttribute>) =
    attrs |> Seq.exists (fun a -> a.AttributeType.TryFullName = Some "System.ObsoleteAttribute")

let isHidden attrs = not includeObsolete && isObsolete attrs

let position (r: range) =
    let full = Path.GetFullPath r.FileName
    let i = sourceFiles |> Array.tryFindIndex (fun f -> String.Equals(f, full, StringComparison.OrdinalIgnoreCase))
    defaultArg i Int32.MaxValue, r.StartLine

type Kind =
    | Ctor     // constructors
    | Field    // fields, union cases, enum values and nested measures, kept in declaration order
    | Instance // instance members
    | Static   // static members, module functions and module values

type Item = {
    Kind: Kind
    Name: string      // shown in the name column
    Key: string       // the bare name with "Pnt" as "Pt", used to pair 2D with 3D members
    Signature: string
    Order: int * int  // file index and line, to sort overloads
    }

type Section = {
    Key: string                // full name of the entity or of the extended type
    Path: string list          // display names below the Euclid namespace
    Title: string
    IsModule: bool
    Position: int * int
    SkipNames: Set<string>     // compiler generated union members
    Fields: ResizeArray<Item>
    Members: ResizeArray<FSharpMemberOrFunctionOrValue>
    }

let sections = Collections.Generic.Dictionary<string, Section>()

let addSection key path title isModule pos skip =
    match sections.TryGetValue key with
    | true, s -> s
    | _ ->
        let s = { Key = key; Path = path; Title = title; IsModule = isModule; Position = pos; SkipNames = skip; Fields = ResizeArray(); Members = ResizeArray() }
        sections.[key] <- s
        s

let fullName (e: FSharpEntity) = defaultArg e.TryFullName e.CompiledName

/// Optional type extensions, added to the section of the extended type once all types are known.
let extensions = ResizeArray<FSharpMemberOrFunctionOrValue>()

let fieldItem (name: string) (signature: string) (r: range) =
    { Kind = Field; Name = name; Key = pairingName name; Signature = signature; Order = position r }

let rec walk (parentPath: string list) (e: FSharpEntity) =
    if e.Accessibility.IsPublic && not (isHidden e.Attributes) && not e.IsMeasure && not e.IsFSharpAbbreviation then
        let path = parentPath @ [ e.DisplayName ]
        let title = (if e.IsFSharpModule then "module " else "") + String.Join(".", path)
        let skip =
            if e.IsFSharpUnion then e.UnionCases |> Seq.map (fun c -> "Is" + c.Name) |> Seq.append [ "Tag" ] |> set
            else Set.empty
        let s = addSection (fullName e) path title e.IsFSharpModule (position e.DeclarationLocation) skip
        let typeName = e.DisplayName

        if e.IsEnum then
            for f in e.FSharpFields do
                if f.IsStatic && f.LiteralValue.IsSome then
                    s.Fields.Add(fieldItem f.DisplayName $"{typeName} = {f.LiteralValue.Value}" f.DeclarationLocation)
        elif e.IsFSharpUnion then
            for c in e.UnionCases do
                if c.Accessibility.IsPublic && not (isHidden c.Attributes) then
                    let signature =
                        if c.Fields.Count = 0 then typeName
                        else
                            let fields =
                                c.Fields
                                |> Seq.map (fun f -> if f.IsNameGenerated then fmtArg f.FieldType else $"{f.Name}: {fmtArg f.FieldType}")
                                |> String.concat " * "
                            $"{fields} -> {typeName}"
                    s.Fields.Add(fieldItem c.DisplayName signature c.DeclarationLocation)
        else
            for f in e.FSharpFields do
                if f.Accessibility.IsPublic && not f.IsStatic && not f.IsCompilerGenerated
                   && not (isHidden f.FieldAttributes) && not (isHidden f.PropertyAttributes) then
                    let ty = fmt f.FieldType
                    let field = fieldItem f.DisplayName (if f.IsMutable then $"mutable {ty}" else ty) f.DeclarationLocation
                    s.Fields.Add { field with Name = $".{f.DisplayName}" }

        for n in e.NestedEntities do
            if n.Accessibility.IsPublic && not (isHidden n.Attributes) then
                if n.IsMeasure then
                    s.Fields.Add(fieldItem n.DisplayName "[<Measure>] type" n.DeclarationLocation)
                elif n.IsFSharpAbbreviation then
                    s.Fields.Add(fieldItem n.DisplayName $"= {fmt n.AbbreviatedType}" n.DeclarationLocation)

        for m in e.MembersFunctionsAndValues do
            if m.IsExtensionMember then extensions.Add m
            else s.Members.Add m

        for n in e.NestedEntities do
            walk path n

for e in results.AssemblySignature.Entities do
    let nsPath =
        match e.Namespace with
        | Some ns when ns.StartsWith "Euclid." -> ns.Substring("Euclid.".Length).Split '.' |> List.ofArray
        | _ -> []
    walk nsPath e

for m in extensions do
    match m.ApparentEnclosingEntity with
    | Some target ->
        let key = fullName target
        match sections.TryGetValue key with
        | true, s -> s.Members.Add m
        | _ when key.StartsWith "Euclid." -> () // extends a hidden type
        | _ ->
            // extends a type outside of Euclid, e.g. ResizeArray
            let name = if target.DisplayName = "List" then "ResizeArray" else target.DisplayName
            let s = addSection key [ name ] $"{name} extensions" false (position m.DeclarationLocation) Set.empty
            s.Members.Add m
    | None -> ()

/// Converts the members of a section into rows: one per method, function, value, constructor and property.
let itemsOf (s: Section) : Item list =
    let prefix = List.last s.Path
    let members =
        s.Members
        |> Seq.filter (fun m ->
            m.Accessibility.IsPublic
            && not m.IsCompilerGenerated
            && not m.IsExplicitInterfaceImplementation
            && not m.IsEvent
            && not (s.SkipNames.Contains m.DisplayName))
        |> List.ofSeq
    // properties are listed through their getter and setter methods, the property itself carries the attributes
    let hiddenProperties =
        members
        |> List.filter (fun m -> m.IsProperty && isHidden m.Attributes)
        |> List.map (fun m -> m.DisplayName, m.IsInstanceMember)
        |> set
    let visible =
        members
        |> List.filter (fun m ->
            not m.IsProperty
            && not (isHidden m.Attributes)
            && not ((m.IsPropertyGetterMethod || m.IsPropertySetterMethod) && hiddenProperties.Contains(m.DisplayName, m.IsInstanceMember)))
    let indexCount (m: FSharpMemberOrFunctionOrValue) =
        m.CurriedParameterGroups |> Seq.concat |> Seq.filter (fun p -> not (isUnit p.Type)) |> Seq.length
    let isAccessorPair (getter: FSharpMemberOrFunctionOrValue) (setter: FSharpMemberOrFunctionOrValue) =
        getter.IsPropertyGetterMethod && setter.IsPropertySetterMethod
        && getter.DisplayName = setter.DisplayName
        && getter.IsInstanceMember = setter.IsInstanceMember
        && indexCount getter = indexCount setter - 1
    [ yield! s.Fields
      for m in visible do
        let item kind signature =
            let name =
                match kind with
                | Static when not (m.DisplayName.StartsWith "(") -> $"{prefix}.{displayName m}"
                | Instance -> $".{displayName m}"
                | _ -> displayName m
            { Kind = kind; Name = name; Key = pairingName m.DisplayName; Signature = signature; Order = position m.DeclarationLocation }
        let kind = if m.IsInstanceMember then Instance else Static
        if m.IsConstructor then
            yield { item Ctor (fmtFunction m) with Name = "new"; Key = "new" }
        elif m.IsPropertyGetterMethod then
            yield item kind (fmtProperty m (visible |> List.exists (isAccessorPair m)))
        elif m.IsPropertySetterMethod then
            if not (visible |> List.exists (fun g -> isAccessorPair g m)) then
                yield item kind (fmtSetterOnly m)
        else
            yield item kind (fmtFunction m)
    ]
    |> List.sortBy (fun i -> i.Order)

// #endregion
// #region Pairing 2D with 3D

let to3D (name: string) =
    match explicitPairs.TryGetValue name with
    | true, n -> n
    | _ -> name.Replace("2D", "3D")

type Dim = D2 | D3 | Neutral

let dimOf (s: Section) =
    let explicit3D = set explicitPairs.Values
    if s.Path |> List.exists (fun n -> n.Contains "2D" || explicitPairs.ContainsKey n) then D2
    elif s.Path |> List.exists (fun n -> n.Contains "3D" || explicit3D.Contains n || only3D.Contains n) then D3
    else Neutral

/// Sorts names alphabetically, numbers like ``2.5`` by value, and operators last.
let compareKeys (a: string) (b: string) =
    let isOp (s: string) = s.StartsWith "("
    let number (s: string) =
        match Double.TryParse(s.Trim('`', '+'), Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
        | true, v -> Some v
        | _ -> None
    match isOp a, isOp b with
    | true, false -> 1
    | false, true -> -1
    | _ ->
        match number a, number b with
        | Some x, Some y when x <> y -> compare x y
        | _ ->
            let c = String.Compare(a, b, StringComparison.OrdinalIgnoreCase)
            if c <> 0 then c else String.CompareOrdinal(a, b)

/// Pairs the overloads of one name. First the ones with equal signatures after renaming 2D types to 3D,
/// then the ones with equal parameter types, then the remaining ones in declaration order.
let pairUp (left: Item list) (right: Item list) : (Item option * Item option) list =
    let to3DSig (sg: string) = Regex.Replace(sg, @"\w+", fun m -> to3D m.Value)
    let typesOnly (sg: string) = Regex.Replace(sg, @"(\[<ParamArray>\] )?\??\w+: ", "")
    let rest = ResizeArray right
    let matches = Array.create left.Length None
    let take (isMatch: Item -> Item -> bool) =
        left |> List.iteri (fun li l ->
            if matches.[li].IsNone then
                match rest |> Seq.tryFindIndex (isMatch l) with
                | Some ri ->
                    matches.[li] <- Some rest.[ri]
                    rest.RemoveAt ri
                | None -> ())
    take (fun l r -> r.Signature = to3DSig l.Signature)
    take (fun l r -> typesOnly r.Signature = typesOnly (to3DSig l.Signature))
    take (fun _ _ -> true)
    [ for li, l in List.indexed left do
        yield Some l, matches.[li]
      for r in rest do
        yield None, Some r ]

type Row =
    | Cells of Item option * Item option
    | Label of string

let rowsOf (isModule: bool) (left: Item list) (right: Item list) : Row list =
    let ofKind k (xs: Item list) = xs |> List.filter (fun i -> i.Kind = k)
    let withKey key (xs: Item list) = xs |> List.filter (fun i -> i.Key = key)
    let pairKeys keys l r = [ for k in keys do for a, b in pairUp (withKey k l) (withKey k r) -> Cells(a, b) ]
    let sortedKeys (xs: Item list) = xs |> List.map (fun i -> i.Key) |> List.distinct |> List.sortWith compareKeys
    let lower (i: Item) = i.Key.ToLowerInvariant()

    // constructors, then fields and cases in declaration order
    let ctors = pairUp (ofKind Ctor left) (ofKind Ctor right) |> List.map Cells
    let fl, fr = ofKind Field left, ofKind Field right
    let fields = pairKeys ((fl @ fr) |> List.map (fun i -> i.Key) |> List.distinct) fl fr

    // instance members in alphabetical order, each followed by the static members of the same name
    let il, ir = ofKind Instance left, ofKind Instance right
    let sl, sr = ofKind Static left, ofKind Static right
    let groups = (il @ ir) |> List.map lower |> List.distinct |> List.sortWith compareKeys
    let instanceRows =
        [ for g in groups do
            let inGroup xs = xs |> List.filter (fun i -> lower i = g)
            yield! pairKeys (sortedKeys (inGroup (il @ ir))) (inGroup il) (inGroup ir)
            yield! pairKeys (sortedKeys (inGroup (sl @ sr))) (inGroup sl) (inGroup sr) ]

    // static members without an instance member of the same name
    let grouped = set groups
    let others xs = xs |> List.filter (fun i -> not (grouped.Contains(lower i)))
    let ol, orr = others sl, others sr
    let staticRows = pairKeys (sortedKeys (ol @ orr)) ol orr

    let before = ctors @ fields @ instanceRows
    if before.IsEmpty || staticRows.IsEmpty then before @ staticRows
    else before @ (Label (if isModule then "Functions and values" else "Other static members") :: staticRows)

// #endregion
// #region Markdown

/// A code span that may contain backticks, like ``0.5``, with '|' escaped for tables.
let code (s: string) =
    if s = "" then ""
    else
        let longest = Regex.Matches(s, "`+") |> Seq.map (fun m -> m.Length) |> Seq.fold max 0
        let fence = String('`', longest + 1)
        let pad = if s.StartsWith "`" || s.EndsWith "`" then " " else ""
        (fence + pad + s + pad + fence).Replace("|", "\\|")

/// The anchor GitHub generates for a heading.
let anchor (title: string) =
    let lower = title.Trim().ToLowerInvariant()
    let kept = lower |> String.filter (fun c -> Char.IsLetterOrDigit c || c = ' ' || c = '-' || c = '_')
    kept.Replace(' ', '-')

type Group = {
    Left: Section
    Right: Section option
    Rows: Row list
    }

let allSections = sections.Values |> List.ofSeq
let sectionDims = allSections |> List.map (fun s -> s, dimOf s)
let paired = Collections.Generic.HashSet<string>()

let byPosition (gs: Group list) =
    gs
    |> List.filter (fun g -> not g.Rows.IsEmpty)
    |> List.sortBy (fun g ->
        match g.Right with
        | Some r -> min g.Left.Position r.Position
        | None -> g.Left.Position)

let single (s: Section) = { Left = s; Right = None; Rows = rowsOf s.IsModule (itemsOf s) [] }

let groups, general =
    let pairs =
        [ for s, d in sectionDims do
            if d = D2 then
                let partnerPath = s.Path |> List.map (to3D >> pairingName)
                let partner =
                    sectionDims
                    |> List.tryFind (fun (o, od) -> od = D3 && List.map pairingName o.Path = partnerPath && o.IsModule = s.IsModule)
                match partner with
                | Some (o, _) ->
                    paired.Add s.Key |> ignore
                    paired.Add o.Key |> ignore
                    yield { Left = s; Right = Some o; Rows = rowsOf s.IsModule (itemsOf s) (itemsOf o) }
                | None -> () ]
    let singles dims =
        [ for s, d in sectionDims do
            if List.contains d dims && not (paired.Contains s.Key) then
                yield single s ]
    byPosition (pairs @ singles [ D2; D3 ]), byPosition (singles [ Neutral ])

/// The 2D or the 3D geometry types on their own, without the general utilities.
let onlyDim dim =
    [ for s, d in sectionDims do if d = dim then yield single s ] |> byPosition

let titleOf (g: Group) =
    match g.Right with
    | Some r -> $"{g.Left.Title} / {r.Title}"
    | None -> g.Left.Title

/// Notes on the notation, shared by all files.
let notation = [
    "- Instance members and fields start with `.`, e.g. `.DistanceTo`."
    "- Static members are qualified with their type name. They are listed right below the instance member of the same name."
    "  The static members without such an instance member follow at the end of each table."
    "- A property shows only its type. `with get, set` marks a property that can be set. `?name` marks an optional parameter."
    ]

/// Writes the introduction, a table of contents with one list per part, and one table per group.
let writeFile (path: string) (intro: string list) (parts: (string * Group list) list) =
    let sb = StringBuilder()
    let line (s: string) = sb.Append(s).Append('\n') |> ignore
    for l in intro do line l
    line ""
    line "Generated from the sources by `Scripts/ApiTable.fsx`. Do not edit by hand."
    line ""
    line "## Contents"
    for label, gs in parts do
        line ""
        if parts.Length > 1 then
            line $"**{label}**"
            line ""
        for g in gs do line $"- [{titleOf g}](#{anchor (titleOf g)})"

    let mutable rowCount = 0
    let all = parts |> List.collect snd
    for g in all do
        line ""
        line $"## {titleOf g}"
        line ""
        match g.Right with
        | Some r ->
            line $"| {List.last g.Left.Path} | Signature | {List.last r.Path} | Signature |"
            line "|:--|:--|:--|:--|"
        | None ->
            line $"| {List.last g.Left.Path} | Signature |"
            line "|:--|:--|"
        let cells (i: Item option) =
            match i with
            | Some i -> $"{code i.Name} | {code i.Signature}"
            | None -> " | "
        for row in g.Rows do
            rowCount <- rowCount + 1
            match row, g.Right with
            | Cells(l, r), Some _ -> line $"| {cells l} | {cells r} |"
            | Cells(l, _), None -> line $"| {cells l} |"
            | Label text, Some _ -> line $"| **{text}** | | | |"
            | Label text, None -> line $"| **{text}** | |"

    File.WriteAllText(path, sb.ToString(), UTF8Encoding(false))
    printfn $"Wrote {all.Length} tables with {rowCount} rows to {path}"

/// The output file name with a suffix, e.g. ApiTable2D.md.
let withSuffix (suffix: string) =
    Path.Combine(Path.GetDirectoryName outFile, Path.GetFileNameWithoutExtension outFile + suffix + Path.GetExtension outFile)

writeFile outFile
    [ "# Euclid API"
      ""
      "All public types, members and functions of Euclid. Corresponding 2D and 3D types share one table."
      ""
      "- Members of the same name share a row. An empty cell means that the member only exists in one dimension."
      yield! notation ]
    [ "2D and 3D", groups
      "General", general ]

for dim, name in [ D2, "2D"; D3, "3D" ] do
    writeFile (withSuffix name)
        [ $"# Euclid {name} API"
          ""
          $"All public {name} geometry types of Euclid, with their members and functions."
          $"The general utilities and helper types are left out. See [{Path.GetFileName outFile}]({Path.GetFileName outFile}) for them and for 2D and 3D side by side."
          ""
          yield! notation ]
        [ (name, onlyDim dim) ]

// #endregion
