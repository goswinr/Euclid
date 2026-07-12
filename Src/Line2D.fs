namespace Euclid

open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>]
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar

/// A struct containing 4 floats, representing an immutable finite line in 2D.
[<Struct;NoEquality;NoComparison>] // because it's made up from floats
[<IsReadOnly>]
[<DataContract>] // for using DataMember on fields
type Line2D =
    // [<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar

    /// The field holding the X coordinate of the start point of the line.
    [<DataMember>] val FromX:float

    /// The field holding the Y coordinate of the start point of the line.
    [<DataMember>] val FromY:float

    /// The field holding the X coordinate of the end point of the line.
    [<DataMember>] val ToX  :float

    /// The field holding the Y coordinate of the end point of the line.
    [<DataMember>] val ToY  :float

    /// Create a Line2D from 2D start point and 2D end point.
    new (fromPt:Pt, toPt:Pt) =
            {FromX=fromPt.X; FromY=fromPt.Y; ToX=toPt.X; ToY=toPt.Y; }

    /// Create a Line2D from 2D start point's x and y and 2D end point's x and y.
    new (fromX, fromY, toX, toY) =
            {FromX=fromX; FromY=fromY;  ToX=toX; ToY=toY}
    /// The X component of the line Direction/Vector.
    member inline ln.VectorX : float =
        ln.ToX - ln.FromX

    /// The X component of the line Direction/Vector.
    static member inline vectorX (ln:Line2D) : float =
        ln.ToX - ln.FromX

    /// The Y component of the line Direction/Vector.
    member inline ln.VectorY : float =
        ln.ToY - ln.FromY

    /// The Y component of the line Direction/Vector.
    static member inline vectorY (ln:Line2D) : float =
        ln.ToY - ln.FromY

    /// Returns the length of the line.
    member inline ln.Length : float =
        let x = ln.VectorX
        let y = ln.VectorY
        sqrt(x*x + y*y)

    /// Returns the length of the line.
    static member inline length (ln:Line2D) : float =
        let x = ln.VectorX
        let y = ln.VectorY
        sqrt(x*x + y*y)

    /// Returns the squared length of the line.
    member inline ln.LengthSq : float =
        let x = ln.VectorX
        let y = ln.VectorY
        x*x + y*y

    /// Returns the squared length of the line.
    static member inline lengthSq (ln:Line2D) : float =
        let x = ln.VectorX
        let y = ln.VectorY
        x*x + y*y

    /// Format 2D line into string including type name, X and Y for start and end points, and Length.
    /// Using nice floating point number formatting.
    override ln.ToString() : string =
        let fx = Format.float ln.FromX
        let fy = Format.float ln.FromY
        let tx = Format.float ln.ToX
        let ty = Format.float ln.ToY
        let l  = Format.float ln.Length
        $"Euclid.Line2D from X=%s{fx}|Y=%s{fy} to X=%s{tx}|Y=%s{ty} Length:%s{l}"

    /// Format 2D line into string from X and Y for start and end points.
    /// Using nice floating point number formatting.
    /// But without full type name as in ln.ToString()
    member ln.AsString : string =
        let fx = Format.float ln.FromX
        let fy = Format.float ln.FromY
        let tx = Format.float ln.ToX
        let ty = Format.float ln.ToY
        $"X=%s{fx}|Y=%s{fy} to X=%s{tx}|Y=%s{ty}"

    /// Format 2D line into string from X and Y for start and end points.
    /// Using nice floating point number formatting.
    /// But without full type name as in ln.ToString()
    static member asString (ln:Line2D) : string =
        let fx = Format.float ln.FromX
        let fy = Format.float ln.FromY
        let tx = Format.float ln.ToX
        let ty = Format.float ln.ToY
        $"X=%s{fx}|Y=%s{fy} to X=%s{tx}|Y=%s{ty}"

    /// Format 2D line into an F# code string that can be used to recreate the line.
    member ln.AsFSharpCode : string =
        $"Line2D({ln.FromX}, {ln.FromY}, {ln.ToX}, {ln.ToY})"

    /// Format 2D line into an F# code string that can be used to recreate the line.
    static member asFSharpCode (ln:Line2D) : string =
        $"Line2D({ln.FromX}, {ln.FromY}, {ln.ToX}, {ln.ToY})"

    /// The start point of the Line2D. Same as ln.Start.
    member inline ln.From : Pt =
        Pt(ln.FromX, ln.FromY)

    /// The start point of the Line2D. Same as Line2D.start.
    static member inline from (ln:Line2D) : Pt =
        Pt(ln.FromX, ln.FromY)

    /// The end point of the Line2D. Same as ln.End.
    member inline ln.To : Pt =
        Pt(ln.ToX, ln.ToY)

    /// The end point of the Line2D. Same as Line2D.end'.
    static member inline to' (ln:Line2D) : Pt =
        Pt(ln.ToX, ln.ToY)

    /// The start point of the Line2D. Same as ln.From.
    member inline ln.Start : Pt =
        Pt(ln.FromX, ln.FromY)

    /// The start point of the Line2D. Same as Line2D.from.
    static member inline start (ln:Line2D) : Pt =
        ln.Start

    /// The end point of the Line2D. Same as ln.To.
    member inline ln.End : Pt =
        Pt(ln.ToX, ln.ToY)

    /// The end point of the Line2D. Same as Line2D.to'.
    static member inline end' (ln:Line2D) : Pt =
        ln.End

    /// Same as ln.Vector or ln.Tangent.
    /// The returned vector has the same length as the Line2D.
    member inline ln.Direction : Vc =
        Vc(ln.VectorX, ln.VectorY)

    /// Same as ln.Vector or ln.Tangent.
    /// The returned vector has the same length as the Line2D.
    static member inline direction (ln:Line2D) : Vc =
        Vc(ln.VectorX, ln.VectorY)

    /// Same as ln.Tangent or ln.Direction.
    /// The returned vector has the same length as the Line2D.
    member inline ln.Vector : Vc =
        Vc(ln.VectorX, ln.VectorY)

    /// Same as ln.Tangent or ln.Direction.
    /// The returned vector has the same length as the Line2D.
    static member inline vector (ln:Line2D) : Vc =
        Vc(ln.VectorX, ln.VectorY)

    /// Same as ln.Vector or ln.Direction.
    /// The returned vector has the same length as the Line2D.
    member inline ln.Tangent : Vc =
        Vc(ln.VectorX, ln.VectorY)

    /// Same as ln.Vector or ln.Direction.
    /// The returned vector has the same length as the Line2D.
    static member inline tangent (ln:Line2D) : Vc =
        Vc(ln.VectorX, ln.VectorY)

    /// Returns a unit-vector of the line Direction.
    member inline ln.UnitTangent : UnitVc =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x * x  + y * y)
        if UtilEuclid.isTooTiny l then
            EuclidErrors.failUnit2 "Line2D.UnitTangent" x y
        UnitVc.createUnchecked (x/l, y/l)

    /// Returns a unit-vector of the line Direction.
    static member inline unitTangent (ln:Line2D) : UnitVc =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x * x  + y * y)
        if UtilEuclid.isTooTiny l then
            EuclidErrors.failUnit2 "Line2D.UnitTangent" x y
        UnitVc.createUnchecked (x/l, y/l)



    // /// Operator to Rotate by Rotation2D around Origin.
    // /// Multiplies (or applies) a Rotation2D to a 2D line.
    // static member inline ( *** ) (ln:Line2D, r:Rotation2D) =  ln.Rotate(r)

