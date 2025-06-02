namespace Euclid

open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>]
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar

/// An immutable finite line in 2D. Represented by a 2D start and 2D end point.
[<Struct;NoEquality;NoComparison>]// because its made up from floats
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

    /// Create Line2D from 2D start point and 2D end point.
    new (fromPt:Pt, toPt:Pt) =
            {FromX=fromPt.X; FromY=fromPt.Y; ToX=toPt.X; ToY=toPt.Y; }

    /// Create Line2D from 2D start point's x and y  and 2D end point's x and y .
    new (fromX, fromY, toX, toY) =
            {FromX=fromX; FromY=fromY;  ToX=toX; ToY=toY}

    /// Returns the length of the line.
    member inline ln.Length =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        sqrt(x*x + y*y)

    /// Returns the square length of the line.
    member inline ln.LengthSq =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        x*x + y*y

    /// Format 2D line into string including type name, X and Y for start and end points, and Length.
    /// Using nice floating point number formatting.
    override ln.ToString() =
        sprintf "Euclid.Line2D from X=%s| Y=%s to X=%s| Y=%s Length %s"
            (Format.float ln.FromX)
            (Format.float ln.FromY)
            (Format.float ln.ToX)
            (Format.float ln.ToY)
            (Format.float ln.Length)

    /// Format 2D line into string from X and Y for start and end points.
    /// Using nice floating point number formatting.
    /// But without full type name as in v.ToString()
    member ln.AsString =
        sprintf "X=%s| Y=%s to X=%s| Y=%s"
            (Format.float ln.FromX)
            (Format.float ln.FromY)
            (Format.float ln.ToX)
            (Format.float ln.ToY)


    /// The Start point of the 2D Line2D.
    member inline ln.From =
        Pt(ln.FromX, ln.FromY)

    /// The End point of the 2D Line2D.
    member inline ln.To =
        Pt(ln.ToX, ln.ToY)

    /// Same as ln.Vector or ln.Tangent.
    /// The returned vector has the same length as the Line2D.
    member inline ln.Direction =
        Vc(ln.ToX-ln.FromX, ln.ToY-ln.FromY)

    /// Same as ln.Tangent or ln.Direction.
    /// The returned vector has the same length as the Line2D.
    member inline ln.Vector =
        Vc(ln.ToX-ln.FromX, ln.ToY-ln.FromY)

    /// Same as ln.Vector or ln.Direction.
    /// The returned vector has the same length as the Line2D.
    member inline ln.Tangent =
        Vc(ln.ToX-ln.FromX, ln.ToY-ln.FromY)

    /// Returns a unit-vector of the line Direction.
    member inline ln.UnitTangent =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let l = sqrt(x * x  + y * y)
        if UtilEuclid.isTooTiny l then
            EuclidException.Raisef "Euclid.Line2D.UnitTangent: x:%g and y:%g are too small for creating a unit-vector. Tolerance:%g" x y UtilEuclid.zeroLengthTolerance
        UnitVc.createUnchecked (x/l, y/l)

