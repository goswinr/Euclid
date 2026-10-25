// Generates a Markdown overview of the public API of Euclid.
// Corresponding 2D and 3D types share one four column table, so that members of the same name sit side by side.
// For this, a 2D name is translated to 3D with the parts in 'dimParts', so that Pt.midPt sits next to Pnt.midPnt
// and Vc.isParallelToVc next to Vec.isParallelToVec. Counterparts with other names are listed in 'renamed'.
// Static members are listed right below the instance member of the same name.
//
// A member without a counterpart either exists in one dimension on purpose, see 'only2DMembers' and 'only3DMembers',
// or leaves an empty cell. So the empty cells are a checklist of gaps in the API and of names that do not match.
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

/// Parts of names that differ between 2D and 3D, as 2D part and 3D part.
/// A part only counts as a whole camel case word, optionally in plural: the "Pt" in "ClPts" counts, the one in "Ptr" does not.
/// A part in lowercase only counts at the start of a name, like "pt0" and "pnt0".
let dimParts = [
    "Pt", "Pnt"   // Pt.midPt and Pnt.midPnt, XLine2D.ClPts and XLine3D.ClPnts
    "Vc", "Vec"   // Vc.isParallelToVc and Vec.isParallelToVec, UnitVc and UnitVec
    "2D", "3D"    // Line2D and Line3D
    "XY", "XYZ"   // Pt.distanceToXY and Pnt.distanceToXYZ, XLineXY and XLineXYZ
    "pt", "pnt"
    "vc", "vec"
    ]

/// 2D types whose 3D counterpart is not found by translating the parts in 'dimParts'.
let explicitPairs =
    dict [
        "BRect"     , "BBox"
        "Rotation2D", "Quaternion"
        ]

/// Types that only exist in 3D and do not have "3D" in their name.
let only3D = set [ "Box"; "FreeBox"; "PPlane"; "NPlane"; "Matrix"; "RigidMatrix" ]

/// Members whose 3D counterpart has another name, as 2D type, 2D name and 3D name. The type "" stands for all types.
/// A name also covers the static member of the same name in lowercase, so "Area" pairs BRect.Area with BBox.Volume and BRect.area with BBox.volume.
/// Besides these, a 3D name with "InXY" or "In2D" pairs with the 2D name without it, e.g. Vec.direction180InXY with Vc.direction180.
let renamed = [
    "Polyline2D", "createFromRectCCW"            , "createFromRect3D"
    "Rect2D"    , "PointsXY_CCW"                , "PointsXYZ"
    "Rect2D"    , "PointsXYLooped_CCW"          , "PointsXYZLooped"
    ""          , "Rotate90CCW"                 , "Rotate2Dby90CCW"
    ""          , "Rotate90CW"                  , "Rotate2Dby90CW"
    ""          , "createFromMembersxy"         , "createFromMembersxyz"
    ""          , "classifyRayIntersectionParam", "classifyRayClosestParam"
    ""          , "classifyRayIntersectionParamFromPtsAndVcs", "classifyRayClosestParamFromPntsAndVecs"
    ""          , "classifyRayIntersectionParamCustom", "classifyRayClosestParamCustom"
    ""          , "tryParameterA"               , "tryClosestParameterRayA"
    ""          , "tryParameterAFromPtsAndVcs"  , "tryClosestParameterRayAFromPntsAndVecs"
    "XRayParam" , "Intersect"                   , "SkewOrX"
    "Rect2D"    , "createFromBounds"            , "createFromBounds2D"
    "BRect"     , "Area"                        , "Volume"
    "BRect"     , "HasArea"                     , "HasVolume"
    "BRect"     , "AsRect2D"                    , "AsBox"
    "Offset2D"  , "makeUnitTangents"            , "getSegmentUnitVectors"
    "Polyline2D", "BoundingRectangle"           , "BoundingBox"
    "Polyline2D", "xys"                         , "xyzs"
    ]

/// Members that only exist in 2D on purpose, as 2D type, regular expression for the name and reason.
/// The type "" stands for all types. The reason is shown in the empty 3D cell next to the member.
/// The expression is matched against the name with its first letter in uppercase, so it covers instance and static members alike.
/// The first matching rule gives the reason.
/// A member whose name refers to the other dimension, like BRect.createFromRect3D, counts as a conversion between 2D and 3D.
/// Any other member without a counterpart leaves an empty cell: a gap in the API, or a name that does not match its counterpart.
let only2DMembers = [
    "Tria2D"    , @"^(Try)?OffsetPtVariableByNormals$"             , "two normals determine the offset in a fixed 2D plane"
    "Offset2D"  , @"^MakeOffsetDirections$"                      , "algorithm-specific flat 2D direction buffer"
    "Offset2D"  , @"^Offset(Variable)?Custom$"                    , "2D collinearity and U-turn policies"
    "VarDistParallel", @"^StepWithTwoPoints$"                    , "2D two-point step policy"
    "Vc"        , @"^AngleToX(180|Pi)$"                          , "signed angle in a fixed 2D plane"
    "UnitVc"    , @"^AngleToX(180|Pi)$"                          , "signed angle in a fixed 2D plane"
    "Rect2D"    , @"^(TryClipLine|ClipLine)$"                     , "clipping in a fixed plane"
    "Rect2D"    , @"^Create(Centered)?From(Unit)?Vc$|^CreateFrom(Line|VcAndPts)$", "a 3D rectangle also needs a normal"
    "BRect"     , @"^PointsLooped$"                               , "a box has no single perimeter"
    ""          , @"Left|Right"                                   , "side of a line"
    ""          , @"(?<![0-9])CC?W(?![a-z])|Clockwise|Winding|Signed", "orientation in the plane"
    ""          , @"^Rotate(Around)?(Degrees|Radians)$"           , "a 3D rotation also needs an axis"
    ""          , @"^Contains(XY)?$|LabelPoint"                   , "inside of a planar area"
    ""          , @"^(Cross|Det)$"                                , "scalar cross product"
    ""          , @"IntersectRays|IntersectInRange"               , "rays in 3D are mostly skew"
    "Rotation2D", @"^(Sin|Cos)$|^CreateFrom(Sine|Cosine)$"        , "sine and cosine"
    "Rotation2D", @"^Add(Degrees|Radians)$|^CreateFrom(Degrees|Radians)$", "a 3D rotation also needs an axis"
    ]

/// Members that only exist in 3D on purpose, as 3D type, regular expression for the name and reason.
/// The type "" stands for all types. The reason is shown in the empty 2D cell next to the member.
let only3DMembers = [
    "Tria3D"    , @"^(Try)?Intersect(Line|Ray|RayXYZ|TriaVectorsWithRay)$", "single-point line-triangle intersection in 3D"
    "Offset3D"  , @"^GetCollinearNeighbors$|^GetOffsetDirections$|^SetOffCornerVar$", "algorithm-specific 3D normal and direction representation"
    "Quaternion", @"^Normalize$"                                 , "quaternion renormalization API"
    "Quaternion", @"^WithAngleIn(Degrees|Radians)$"               , "replace angle while retaining the 3D axis"
    "Quaternion", @"^EqualsRotationWithin$"                      , "quaternions q and -q represent the same rotation"
    "Quaternion", @"^Multiply$|\*\*\*"                            , "quaternion composition and application operators"
    "Polyline3D", @"^(TryFindSelfIntersectionCustom|FindSelfIntersectionCustom)$", "skew tolerance of 3D segments"
    "BBox"      , @"^CreateFromBox$"                              , "conversion from an oriented 3D box"
    "Quaternion", @"^[XYZW]$|Axis|Conjugate|Euler"                , "quaternion components"
    "Quaternion", @"^CreateFrom(Unit)?Vec(Degrees|Radians)$"      , "rotation about an axis vector"
    "XLine3D"   , @"^(DoIntersect|TryIntersect)Custom$"           , "skew tolerance of 3D lines"
    "BBox"      , @"Edge\d*[4-7]|Pnt[4-7]|Face|TopPoints|BottomPoints|IsFlat", "corners, edges and faces of a box"
    ""          , @"On[XYZ](?![a-z])"                             , "rotation about a world axis"
    ""          , @"(?<![A-Z])Z(?![a-z])"                         , "Z axis"
    "Polyline3D", @"^CreateFromMembers(XY|xy)$"                   , "import of 2D points"
    ""          , @"In(XY|2D)(?![a-z])|XYPlane|^OffsetXY$"        , "projection to the XY plane"
    ""          , @"Vertical|Horizontal|Up(?![a-z])|Down(?![a-z])", "vertical direction"
    ""          , @"Transform|Matrix"                             , "transformation matrix"
    ""          , @"Normal(?![a-z])|Plane|Planar|^Flip(ped)?$"    , "plane or normal"
    ""          , @"Skew|Closest\w*Ray|Ray\w*Closest|DoRaysIntersect", "rays in 3D are mostly skew"
    ""          , @"Cone"                                         , "cone"
    ""          , @"^Determinant3$"                               , "triple product"
    ]

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
    Bare: string      // the name as declared
    Key: string       // the bare name, translated to 3D on the 2D side of a table to pair 2D with 3D members
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
    { Kind = Field; Name = name; Bare = name; Key = name; Signature = signature; Order = position r }

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
            { Kind = kind; Name = name; Bare = m.DisplayName; Key = m.DisplayName; Signature = signature; Order = position m.DeclarationLocation }
        let kind = if m.IsInstanceMember then Instance else Static
        if m.IsConstructor then
            yield { item Ctor (fmtFunction m) with Name = "new"; Bare = "new"; Key = "new" }
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

/// Matches the parts in 'dimParts', longer parts first, so that "XYZ" is not read as "XY".
/// A part that starts with a digit, like "2D", may be followed by a lowercase letter, as in Polyline3D.createFrom2DxyMembers.
let dimPartRegex =
    dimParts
    |> List.collect (fun (a, b) -> [ a; b ])
    |> List.sortByDescending String.length
    |> List.map (fun p ->
        if Char.IsDigit p.[0] then p
        elif Char.IsLower p.[0] then $"(?<![A-Za-z0-9_]){p}(?=s?(?![a-z]))"
        else $"{p}(?=s?(?![a-z]))")
    |> String.concat "|"
    |> Regex

let parts2D = dimParts |> List.map fst |> set
let parts3D = dimParts |> List.map snd |> set
let otherPart = dict [ for a, b in dimParts do yield a, b; yield b, a ]
let explicit3D = dict [ for KeyValue(a, b) in explicitPairs -> b, a ]

/// Translates a name from the 2D side of a table to the name of its 3D counterpart.
/// The 3D parts in it turn into 2D parts, so that conversions to the other dimension sit side by side,
/// e.g. Pt.asPnt next to Pnt.asPt and Vc.asVec next to Vec.asVc.
let to3D (name: string) =
    match explicitPairs.TryGetValue name with
    | true, n -> n
    | _ ->
        match explicit3D.TryGetValue name with
        | true, n -> n
        | _ -> dimPartRegex.Replace(name, fun m -> otherPart.[m.Value])

let lowerFirst (s: string) =
    if s = "" then s else string (Char.ToLowerInvariant s.[0]) + s.Substring 1

let upperFirst (s: string) =
    if s = "" then s else string (Char.ToUpperInvariant s.[0]) + s.Substring 1

/// The name of the 3D counterpart of a member of a 2D type, from 'renamed' or else from 'to3D'.
let memberTo3D (typeName: string) (name: string) =
    renamed
    |> List.tryPick (fun (ty, name2D, name3D) ->
        if ty <> "" && ty <> typeName then None
        elif name = name2D then Some name3D
        elif name = lowerFirst name2D then Some (lowerFirst name3D)
        else None)
    |> Option.defaultWith (fun () -> to3D name)

/// Sets the keys of the items of a 2D type to the names of their 3D counterparts.
/// A 2D name stays the key when only the name itself exists in 3D,
/// e.g. Rect2D.expandXY and Rect3D.expandXY, where "XY" means the local X and Y in both.
let keysFor3D (typeName: string) (right: Item list) (left: Item list) =
    let rightKeys = right |> List.map (fun i -> i.Key) |> set
    let translated = left |> List.map (fun i -> memberTo3D typeName i.Key) |> set
    left |> List.map (fun i ->
        let key = memberTo3D typeName i.Key
        if not (rightKeys.Contains key) && rightKeys.Contains i.Key && not (translated.Contains i.Key) then i
        else { i with Key = key })

let inXYRegex = Regex @"In(XY|2D)(?![a-z])"

/// Sets the key of a 3D item to its name without "InXY" or "In2D" when only that name exists in 2D,
/// e.g. Vec.direction180InXY pairs with Vc.direction180 and Polyline3D.IsClockwiseIn2D with Polyline2D.IsClockwise.
let keysWithoutInXY (left: Item list) (right: Item list) =
    let leftKeys = left |> List.map (fun i -> i.Key) |> set
    let rightKeys = right |> List.map (fun i -> i.Key) |> set
    right |> List.map (fun i ->
        let key = inXYRegex.Replace(i.Key, "")
        if key <> i.Key && leftKeys.Contains key && not (rightKeys.Contains key) && not (leftKeys.Contains i.Key) then { i with Key = key }
        else i)

type Dim = D2 | D3 | Neutral

let dimOfName (name: string) =
    let parts = dimPartRegex.Matches name |> Seq.map (fun m -> m.Value) |> List.ofSeq
    if explicitPairs.ContainsKey name || parts |> List.exists parts2D.Contains then D2
    elif explicit3D.ContainsKey name || only3D.Contains name || not parts.IsEmpty then D3
    else Neutral

let dimOf (s: Section) =
    let dims = s.Path |> List.map dimOfName
    if List.contains D2 dims then D2
    elif List.contains D3 dims then D3
    else Neutral

/// TRUE if a name refers to a type of the given dimension, like "createFromBRect" to 2D.
/// "XY" and "XYZ" name coordinates, not types, so they do not count.
let refersTo (dim: Dim) (name: string) =
    let parts, types = if dim = D2 then parts2D, explicitPairs.Keys else parts3D, explicitPairs.Values
    let parts = parts - set [ "XY"; "XYZ" ]
    (dimPartRegex.Matches name |> Seq.exists (fun m -> parts.Contains m.Value))
    || (types |> Seq.exists name.Contains)

/// The reason why a member has no counterpart on purpose, from rules like 'only2DMembers', or None for a gap.
/// A member that refers to the other dimension is a conversion between 2D and 3D.
let reasonFor (rules: (string * string * string) list) (otherDim: Dim) (typeName: string) (i: Item) =
    rules
    |> List.tryPick (fun (ty, pattern, reason) ->
        if (ty = "" || ty = typeName) && Regex.IsMatch(upperFirst i.Bare, pattern) then Some reason else None)
    |> Option.orElse (if refersTo otherDim i.Bare then Some "conversion between 2D and 3D" else None)

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
    | Cells of Item option * Item option * reason: string option // the reason why one cell is empty on purpose
    | Label of string

let rowsOf (isModule: bool) (left: Item list) (right: Item list) : Row list =
    let ofKind k (xs: Item list) = xs |> List.filter (fun i -> i.Kind = k)
    let withKey key (xs: Item list) = xs |> List.filter (fun i -> i.Key = key)
    let pairKeys keys l r = [ for k in keys do for a, b in pairUp (withKey k l) (withKey k r) -> Cells(a, b, None) ]
    let sortedKeys (xs: Item list) = xs |> List.map (fun i -> i.Key) |> List.distinct |> List.sortWith compareKeys
    let lower (i: Item) = i.Key.ToLowerInvariant()

    // constructors, then fields and cases in declaration order
    let ctors = pairUp (ofKind Ctor left) (ofKind Ctor right) |> List.map (fun (a, b) -> Cells(a, b, None))
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

/// Adds the reason to each row of a 2D and a 3D type where one member exists in one dimension only on purpose.
let withReasons (left: Section) (right: Section) (rows: Row list) =
    rows |> List.map (function
        | Cells(Some l, None, _) -> Cells(Some l, None, reasonFor only2DMembers D3 (List.last left.Path) l)
        | Cells(None, Some r, _) -> Cells(None, Some r, reasonFor only3DMembers D2 (List.last right.Path) r)
        | row -> row)

/// TRUE for a row with an empty cell that is not explained by a reason.
let isGap (row: Row) =
    match row with
    | Cells(Some _, None, None) | Cells(None, Some _, None) -> true
    | _ -> false

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
                let partnerPath = s.Path |> List.map to3D
                let partner =
                    sectionDims
                    |> List.tryFind (fun (o, od) -> od = D3 && o.Path = partnerPath && o.IsModule = s.IsModule)
                match partner with
                | Some (o, _) ->
                    paired.Add s.Key |> ignore
                    paired.Add o.Key |> ignore
                    let left = itemsOf s |> keysFor3D (List.last s.Path) (itemsOf o)
                    let right = itemsOf o |> keysWithoutInXY left
                    yield { Left = s; Right = Some o; Rows = rowsOf s.IsModule left right |> withReasons s o }
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

/// The number of empty cells without a reason in the tables that pair a 2D with a 3D type.
let gapCount (gs: Group list) =
    gs |> List.sumBy (fun g -> if g.Right.IsSome then g.Rows |> List.filter isGap |> List.length else 0)

let plural (n: int) (word: string) =
    if n = 1 then $"1 {word}" else $"{n} {word}s"

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
        for g in gs do
            let gaps = if g.Right.IsSome then ", " + plural (gapCount [ g ]) "gap" else ""
            line $"- [{titleOf g}](#{anchor (titleOf g)}){gaps}"

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
        let cells (i: Item option) (reason: string option) =
            match i, reason with
            | Some i, _ -> $"{code i.Name} | {code i.Signature}"
            | None, Some r -> $"— | *{r}*"
            | None, None -> " | "
        for row in g.Rows do
            rowCount <- rowCount + 1
            match row, g.Right with
            | Cells(l, r, reason), Some _ -> line $"| {cells l reason} | {cells r reason} |"
            | Cells(l, _, _), None -> line $"| {cells l None} |"
            | Label text, Some _ -> line $"| **{text}** | | | |"
            | Label text, None -> line $"| **{text}** | |"

    File.WriteAllText(path, sb.ToString(), UTF8Encoding(false))
    printfn $"Wrote {all.Length} tables with {rowCount} rows to {path}"

/// The output file name with a suffix, e.g. ApiTable2D.md.
let withSuffix (suffix: string) =
    Path.Combine(Path.GetDirectoryName outFile, Path.GetFileNameWithoutExtension outFile + suffix + Path.GetExtension outFile)

let allGaps = plural (gapCount groups) "gap"

writeFile outFile
    [ "# Euclid API"
      ""
      "All public types, members and functions of Euclid. Corresponding 2D and 3D types share one table."
      ""
      "- Corresponding members share a row, also when their names differ, like `Pt.midPt` and `Pnt.midPnt`."
      "- `—` and a reason in italics mark a member that exists in one dimension only on purpose."
      if gapCount groups = 0 then
          "- There are no unexplained gaps: every member is paired or has a reason for its dimensional difference."
      else
          $"- An empty cell marks one of {allGaps}: a member that is missing in one dimension,"
          "  or a member whose name does not match its counterpart. The contents list the gaps per table."
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
