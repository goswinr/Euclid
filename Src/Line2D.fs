namespace Euclid

open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>]
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar

/// An immutable finite line in 2D. Represented by a 2D start and 2D end point.
[<Struct;NoEquality;NoComparison>] // because it's made up from floats
[<IsReadOnly>]
[<DataContract>] // for using DataMember on fields
type Line2D =
    // [<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar

    /// Returns the X coordinate of the start point of the line.
    [<DataMember>] val FromX:float

    /// Returns the Y coordinate of the start point of the line.
    [<DataMember>] val FromY:float

    /// Returns the X coordinate of the end point of the line.
    [<DataMember>] val ToX  :float

    /// Returns the Y coordinate of the end point of the line.
    [<DataMember>] val ToY  :float

    /// Create a Line2D from 2D start point and 2D end point.
    new (fromPt:Pt, toPt:Pt) =
            {FromX=fromPt.X; FromY=fromPt.Y; ToX=toPt.X; ToY=toPt.Y; }

    /// Create a Line2D from 2D start point's x and y and 2D end point's x and y.
    new (fromX, fromY, toX, toY) =
            {FromX=fromX; FromY=fromY;  ToX=toX; ToY=toY}
    /// The X component of the line Direction/Vector.
    member inline ln.VectorX =
        ln.ToX - ln.FromX

    /// The Y component of the line Direction/Vector.
    member inline ln.VectorY =
        ln.ToY - ln.FromY

    /// Returns the length of the line.
    member inline ln.Length =
        let x = ln.VectorX
        let y = ln.VectorY
        sqrt(x*x + y*y)

    /// Returns the squared length of the line.
    member inline ln.LengthSq =
        let x = ln.VectorX
        let y = ln.VectorY
        x*x + y*y

    /// Format 2D line into string including type name, X and Y for start and end points, and Length.
    /// Using nice floating point number formatting.
    override ln.ToString() =
        let fx = Format.float ln.FromX
        let fy = Format.float ln.FromY
        let tx = Format.float ln.ToX
        let ty = Format.float ln.ToY
        let l  = Format.float ln.Length
        $"Euclid.Line2D from X=%s{fx}|Y=%s{fy} to X=%s{tx}|Y=%s{ty} Length:%s{l}"

    /// Format 2D line into string from X and Y for start and end points.
    /// Using nice floating point number formatting.
    /// But without full type name as in ln.ToString()
    member ln.AsString =
        let fx = Format.float ln.FromX
        let fy = Format.float ln.FromY
        let tx = Format.float ln.ToX
        let ty = Format.float ln.ToY
        $"X=%s{fx}|Y=%s{fy} to X=%s{tx}|Y=%s{ty}"

    /// Format 2D line into an F# code string that can be used to recreate the line.
    member ln.AsFSharpCode =
        $"Line2D({ln.FromX}, {ln.FromY}, {ln.ToX}, {ln.ToY})"


    /// The start point of the Line2D.
    member inline ln.From =
        Pt(ln.FromX, ln.FromY)

    /// The end point of the Line2D.
    member inline ln.To =
        Pt(ln.ToX, ln.ToY)

    /// Same as ln.Vector or ln.Tangent.
    /// The returned vector has the same length as the Line2D.
    member inline ln.Direction =
        Vc(ln.VectorX, ln.VectorY)

    /// Same as ln.Tangent or ln.Direction.
    /// The returned vector has the same length as the Line2D.
    member inline ln.Vector =
        Vc(ln.VectorX, ln.VectorY)



    /// Same as ln.Vector or ln.Direction.
    /// The returned vector has the same length as the Line2D.
    member inline ln.Tangent =
        Vc(ln.VectorX, ln.VectorY)

    /// Returns a unit-vector of the line Direction.
    member inline ln.UnitTangent =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x * x  + y * y)
        if UtilEuclid.isTooTiny l then
            EuclidErrors.failUnit2 "Line2D.UnitTangent" x y
        UnitVc.createUnchecked (x/l, y/l)



    // /// Operator to Rotate by Rotation2D around Origin.
    // /// Multiplies (or applies) a Rotation2D to a 2D line.
    // static member inline ( *** ) (ln:Line2D, r:Rotation2D) =  ln.Rotate(r)

