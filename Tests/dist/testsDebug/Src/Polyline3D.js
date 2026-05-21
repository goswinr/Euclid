
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { concat, join } from "../fable_modules/fable-library-js.5.0.0/String.js";
import { ResizeArr_closeLoop, ResizeArr_map } from "./ResizeArr.js";
import { Pnt_$ctor_Z7AD9E565, Pnt__get_AsFSharpCode } from "./Pnt.js";
import { addRangeInPlace, item, setItem, getRange } from "../fable_modules/fable-library-js.5.0.0/Array.js";
import { failRarr, failEmptySeq, failNull, failTooFewPoly3D, fail } from "./EuclidErrors.js";
import { min, max } from "../fable_modules/fable-library-js.5.0.0/Double.js";
import { Line3D_$ctor_5A6659A0 } from "./Line3D.js";
import { Vec_$ctor_Z7AD9E565 } from "./Vec.js";
import { Operators_IsNull } from "../fable_modules/fable-library-js.5.0.0/FSharp.Core.js";
import { count as count_1 } from "../fable_modules/fable-library-js.5.0.0/CollectionUtil.js";
import { BBox_$ctor_76A78260 } from "./BBox.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_1 } from "./Pnt.js";
import { Euclid_Pnt__Pnt_notEquals_Static, Euclid_Pnt__Pnt_rotateZwithCenterBy_Static, Euclid_Pnt__Pnt_rotateZBy_Static } from "./TypeExtensions/Pnt.js";
import { failUnit3, failTooClose } from "./EuclidErrors.js";
import { UnitVec_$ctor_Z7AD9E565 } from "./UnitVec.js";
import { offsetVariableWithDirections, offsetConstantWithDirections, getOffsetDirections, getSegmentUnitVectors } from "./Offset3D.js";

/**
 * A class holding a list of 3D points representing a mutable 3D Polyline.
 * If the last point is the same as the first point, the Polyline3D is closed.
 * The Default constructor uses the provided ResizeArray of points directly,
 * so changes to the list will be reflected in the Polyline3D.
 */
export class Polyline3D {
    constructor(points) {
        this.points = points;
    }
    /**
     * Nicely formatted string representation of the Polyline3D including its length.
     */
    toString() {
        const p = this;
        return (p.points.length === 0) ? "An empty Euclid.Polyline3D." : (`Euclid.Polyline3D with length ${Polyline3D__get_Length(p)}, from ${Polyline3D__get_Points(p).length} points`);
    }
}

export function Polyline3D_$reflection() {
    return class_type("Euclid.Polyline3D", undefined, Polyline3D);
}

export function Polyline3D_$ctor_516DFD0A(points) {
    return new Polyline3D(points);
}

/**
 * Gets the internal list of all Points of the Polyline3D.
 * This is not a copy, so changes to the list will be reflected in the Polyline3D.
 */
export function Polyline3D__get_Points(_) {
    return _.points;
}

/**
 * Create a new empty Polyline3D
 */
export function Polyline3D_$ctor() {
    return Polyline3D_$ctor_516DFD0A([]);
}

/**
 * Create a new empty Polyline3D with predefined capacity for the internal list of points.
 */
export function Polyline3D_$ctor_Z524259A4(capacity) {
    return Polyline3D_$ctor_516DFD0A([]);
}

/**
 * Format Polyline3D into string including its length.
 */
export function Polyline3D__get_AsString(p) {
    if (p.points.length === 0) {
        return "empty Polyline3D.";
    }
    else if (Polyline3D__get_IsClosed(p)) {
        return `closed Polyline3D with length ${Polyline3D__get_Length(p)}, from ${p.points.length} points`;
    }
    else {
        return `open Polyline3D with length ${Polyline3D__get_Length(p)}, from ${p.points.length} points`;
    }
}

/**
 * Format this 3D polyline into an F# code string that can be used to recreate the point.
 */
export function Polyline3D__get_AsFSharpCode(p) {
    const ptsAsCode = join("; ", ResizeArr_map(Pnt__get_AsFSharpCode, p.points));
    return concat("Polyline3D.create [| ", ptsAsCode, ..." |]");
}

/**
 * Creates a copy of the Polyline3D
 * Same as polyline.Clone()
 */
export function Polyline3D__Duplicate(p) {
    return Polyline3D_createDirectlyUnsafe_516DFD0A(getRange(p.points, 0, p.points.length));
}

/**
 * Creates a copy of the Polyline3D.
 * Same as polyline.Duplicate()
 */
export function Polyline3D__Clone(p) {
    return Polyline3D_createDirectlyUnsafe_516DFD0A(getRange(p.points, 0, p.points.length));
}

/**
 * Sets the vertex at given index to the given point.
 * On a closed Polyline3D, setting the first or last point will set both to the same point.
 */
export function Polyline3D__SetVertex(p, idx, pt) {
    if ((idx < 0) ? true : (idx >= p.points.length)) {
        fail(`Polyline3D.SetVertex: index ${idx} is out of range for Polyline3D with ${p.points.length} points.`);
    }
    if ((idx === 0) && Polyline3D__get_IsClosed(p)) {
        setItem(p.points, p.points.length - 1, pt);
    }
    else if ((idx === (p.points.length - 1)) && Polyline3D__get_IsClosed(p)) {
        setItem(p.points, 0, pt);
    }
    setItem(p.points, idx, pt);
}

/**
 * Gets or sets first point of the Polyline3D
 * This is the point at index 0.
 * Same as Polyline3D.FirstPoint
 */
export function Polyline3D__get_Start(p) {
    if (p.points.length < 1) {
        failTooFewPoly3D("Start.get", 1, p.points.length);
    }
    return item(0, p.points);
}

/**
 * Gets or sets first point of the Polyline3D
 * This is the point at index 0.
 * Same as Polyline3D.FirstPoint
 */
export function Polyline3D__set_Start_Z394ECE4D(p, pt) {
    if (p.points.length < 1) {
        failTooFewPoly3D("Start.set", 1, p.points.length);
    }
    setItem(p.points, 0, pt);
}

/**
 * Gets or sets last or end point of the Polyline3D
 * This is the point at index Points.Count - 1.
 * Same as Polyline3D.LastPoint
 */
export function Polyline3D__get_End(p) {
    if (p.points.length < 1) {
        failTooFewPoly3D("End.get", 1, p.points.length);
    }
    return item(p.points.length - 1, p.points);
}

/**
 * Gets or sets last or end point of the Polyline3D
 * This is the point at index Points.Count - 1.
 * Same as Polyline3D.LastPoint
 */
export function Polyline3D__set_End_Z394ECE4D(p, pt) {
    if (p.points.length < 1) {
        failTooFewPoly3D("End.set", 1, p.points.length);
    }
    setItem(p.points, p.points.length - 1, pt);
}

/**
 * Gets or sets the last point of the Polyline3D.
 * This is the point at index Points.Count - 1.
 * Same as Polyline3D.End
 */
export function Polyline3D__get_LastPoint(p) {
    if (p.points.length < 1) {
        failTooFewPoly3D("LastPoint.get", 1, p.points.length);
    }
    return item(p.points.length - 1, p.points);
}

/**
 * Gets or sets the last point of the Polyline3D.
 * This is the point at index Points.Count - 1.
 * Same as Polyline3D.End
 */
export function Polyline3D__set_LastPoint_Z394ECE4D(p, pt) {
    if (p.points.length < 1) {
        failTooFewPoly3D("LastPoint.set", 1, p.points.length);
    }
    setItem(p.points, p.points.length - 1, pt);
}

/**
 * Gets or sets the second last point of the Polyline3D.
 */
export function Polyline3D__get_SecondLastPoint(p) {
    if (p.points.length < 2) {
        failTooFewPoly3D("SecondLastPoint.get", 2, p.points.length);
    }
    return item(p.points.length - 2, p.points);
}

/**
 * Gets or sets the second last point of the Polyline3D.
 */
export function Polyline3D__set_SecondLastPoint_Z394ECE4D(p, pt) {
    if (p.points.length < 2) {
        failTooFewPoly3D("SecondLastPoint.set", 2, p.points.length);
    }
    setItem(p.points, p.points.length - 2, pt);
}

/**
 * Gets or sets the second point of the Polyline3D.
 * This is the point at index 1.
 */
export function Polyline3D__get_SecondPoint(p) {
    if (p.points.length < 2) {
        failTooFewPoly3D("SecondPoint.get", 2, p.points.length);
    }
    return item(1, p.points);
}

/**
 * Gets or sets the second point of the Polyline3D.
 * This is the point at index 1.
 */
export function Polyline3D__set_SecondPoint_Z394ECE4D(p, pt) {
    if (p.points.length < 2) {
        failTooFewPoly3D("SecondPoint.set", 2, p.points.length);
    }
    setItem(p.points, 1, pt);
}

/**
 * Gets or sets the first point of the Polyline3D.
 * This is the point at index 0.
 * Same as Polyline3D.Start
 */
export function Polyline3D__get_FirstPoint(p) {
    if (p.points.length < 1) {
        failTooFewPoly3D("FirstPoint.get", 1, p.points.length);
    }
    return item(0, p.points);
}

/**
 * Gets or sets the first point of the Polyline3D.
 * This is the point at index 0.
 * Same as Polyline3D.Start
 */
export function Polyline3D__set_FirstPoint_Z394ECE4D(p, pt) {
    if (p.points.length < 1) {
        failTooFewPoly3D("FirstPoint.set", 1, p.points.length);
    }
    setItem(p.points, 0, pt);
}

/**
 * Gets the count of points in the Polyline3D
 */
export function Polyline3D__get_PointCount(p) {
    return p.points.length | 0;
}

/**
 * Gets the count of segments in the Polyline3D
 * This is poly.Points.Count - 1
 */
export function Polyline3D__get_SegmentCount(p) {
    return max(0, p.points.length - 1) | 0;
}

/**
 * Gets the index of the last point in the Polyline3D.
 * points.Count - 1
 */
export function Polyline3D__get_LastPointIndex(p) {
    return (p.points.length - 1) | 0;
}

/**
 * Gets the index of the last segment in the Polyline3D.
 * This is poly.Points.Count - 2
 */
export function Polyline3D__get_LastSegmentIndex(p) {
    return (p.points.length - 2) | 0;
}

/**
 * Gets the length of the Polyline3D
 * Returns 0.0 if there are less than 2 points.
 */
export function Polyline3D__get_Length(p) {
    let l = 0;
    if (p.points.length > 1) {
        let prev = item(0, p.points);
        for (let i = 1; i <= (p.points.length - 1); i++) {
            let a_1, b_1, x, y, z;
            const t = item(i, p.points);
            l = (l + ((a_1 = prev, (b_1 = t, (x = (a_1.X - b_1.X), (y = (a_1.Y - b_1.Y), (z = (a_1.Z - b_1.Z), Math.sqrt(((x * x) + (y * y)) + (z * z)))))))));
            prev = t;
        }
    }
    return l;
}

/**
 * Gets the segment at index i of the Polyline3D.
 */
export function Polyline3D__GetSegment_Z524259A4(p, i) {
    if ((i < 0) ? true : (i > (p.points.length - 2))) {
        fail(`Polyline3D.GetSegment: index ${i} is out of range for Polyline3D with ${p.points.length} points.`);
    }
    return Line3D_$ctor_5A6659A0(item(i, p.points), item(i + 1, p.points));
}

/**
 * Gets the last segment of the Polyline3D.
 */
export function Polyline3D__get_LastSegment(p) {
    if (p.points.length < 2) {
        failTooFewPoly3D("LastSegment", 2, p.points.length);
    }
    const i = (p.points.length - 1) | 0;
    return Line3D_$ctor_5A6659A0(item(i - 1, p.points), item(i, p.points));
}

/**
 * Gets the first segment of the Polyline3D.
 */
export function Polyline3D__get_FirstSegment(p) {
    if (p.points.length < 2) {
        failTooFewPoly3D("FirstSegment", 2, p.points.length);
    }
    return Line3D_$ctor_5A6659A0(item(0, p.points), item(1, p.points));
}

/**
 * Returns all segments of the Polyline3D as a list of Line3D.
 */
export function Polyline3D__get_Segments(p) {
    const lns = [];
    const pts = p.points;
    if (pts.length < 2) {
        return lns;
    }
    else {
        let a = item(0, pts);
        for (let i = 1; i <= (p.points.length - 1); i++) {
            const b = item(i, pts);
            void (lns.push(Line3D_$ctor_5A6659A0(a, b)));
            a = b;
        }
        return lns;
    }
}

/**
 * Returns the line vectors of all segments of the Polyline3D as a list of Vec.
 */
export function Polyline3D__get_SegmentVectors(p) {
    const vs = [];
    const pts = p.points;
    if (pts.length < 2) {
        return vs;
    }
    else {
        let a = item(0, pts);
        for (let i = 1; i <= (p.points.length - 1); i++) {
            let a_1, b_1;
            const b = item(i, pts);
            void (vs.push((a_1 = b, (b_1 = a, Vec_$ctor_Z7AD9E565(a_1.X - b_1.X, a_1.Y - b_1.Y, a_1.Z - b_1.Z)))));
            a = b;
        }
        return vs;
    }
}

/**
 * Gets the bounding box of the Polyline3D.
 */
export function Polyline3D__get_BoundingBox(p) {
    const ps = p.points;
    if (Operators_IsNull(ps)) {
        failNull("BBox.createFromIList", "IList<Pnt>");
    }
    if (count_1(ps) === 0) {
        failEmptySeq("BBox.createFromIList", "IList<Pnt>");
    }
    let minX = 1.7976931348623157E+308;
    let minY = 1.7976931348623157E+308;
    let minZ = 1.7976931348623157E+308;
    let maxX = -1.7976931348623157E+308;
    let maxY = -1.7976931348623157E+308;
    let maxZ = -1.7976931348623157E+308;
    for (let i = 0; i <= (count_1(ps) - 1); i++) {
        const p_1 = item(i, ps);
        minX = min(minX, p_1.X);
        minY = min(minY, p_1.Y);
        minZ = min(minZ, p_1.Z);
        maxX = max(maxX, p_1.X);
        maxY = max(maxY, p_1.Y);
        maxZ = max(maxZ, p_1.Z);
    }
    return BBox_$ctor_76A78260(minX, minY, minZ, maxX, maxY, maxZ);
}

/**
 * Tests if Polyline3D start and end points are exactly the same.
 * Returns False if the Polyline3D has less than 3 points.
 */
export function Polyline3D__get_IsClosed(p) {
    if (p.points.length > 2) {
        let v;
        const a = Polyline3D__get_Start(p);
        const b = Polyline3D__get_End(p);
        v = Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
        if ((v.X === 0) && (v.Y === 0)) {
            return v.Z === 0;
        }
        else {
            return false;
        }
    }
    else {
        return false;
    }
}

/**
 * Tests if Polyline3D is closed within given tolerance.
 * Returns False if the Polyline3D has less than 3 points.
 */
export function Polyline3D__IsAlmostClosed_5E38073B(p, tolerance) {
    let a_1, b_1, x, y, z;
    if (p.points.length > 2) {
        return ((a_1 = Polyline3D__get_Start(p), (b_1 = Polyline3D__get_End(p), (x = (a_1.X - b_1.X), (y = (a_1.Y - b_1.Y), (z = (a_1.Z - b_1.Z), ((x * x) + (y * y)) + (z * z))))))) < (tolerance * tolerance);
    }
    else {
        return false;
    }
}

/**
 * Reverse order of the Polyline3D in place.
 */
export function Polyline3D__ReverseInPlace(p) {
    p.points.reverse();
}

/**
 * Returns new Polyline3D in reversed Order.
 */
export function Polyline3D__Reverse(p) {
    const n = Polyline3D__Duplicate(p);
    Polyline3D__get_Points(n).reverse();
    return n;
}

/**
 * Close the Polyline3D if it is not already closed.
 * If the ends are closer than the tolerance. The last point is set to equal the first point.
 * Else the start point is added to the end of the Polyline3D.
 */
export function Polyline3D__CloseInPlace_5E38073B(p, toleranceForAddingPoint) {
    let v_1;
    if (p.points.length < 3) {
        failTooFewPoly3D("CloseInPlace", 3, p.points.length);
    }
    let v;
    const a = Polyline3D__get_Start(p);
    const b = Polyline3D__get_End(p);
    v = Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
    if (((v_1 = v, ((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z))) < (toleranceForAddingPoint * toleranceForAddingPoint)) {
        const this$ = p.points;
        const v_2 = Polyline3D__get_Start(p);
        if (this$.length === 0) {
            failRarr("Last.set", this$);
        }
        setItem(this$, this$.length - 1, v_2);
    }
    else {
        void (p.points.push(Polyline3D__get_Start(p)));
    }
}

/**
 * Calculates the signed area of the Polyline3D when projected in 2D.
 * Z values are ignored.
 * The Polyline3D does not need to be actually closed.
 * The signed area of the Polyline3D is calculated.
 * If it is positive the Polyline3D is CCW.
 */
export function Polyline3D__get_SignedAreaIn2D(p) {
    let area = 0;
    let t;
    const this$ = p.points;
    if (this$.length === 0) {
        failRarr("Last.get", this$);
    }
    t = item(this$.length - 1, this$);
    for (let i = 0; i <= (p.points.length - 1); i++) {
        const n = item(i, p.points);
        const a = t.X - n.X;
        const b = n.Y + t.Y;
        area = (area + (a * b));
        t = n;
    }
    return area;
}

/**
 * Test if Polyline3D is CounterClockwise when projected in 2D.
 * Z values are ignored.
 * The Polyline3D does not need to be actually closed.
 * The signed area of the Polyline3D is calculated.
 * If it is positive the Polyline3D is CCW.
 */
export function Polyline3D__get_IsCounterClockwiseIn2D(p) {
    const area = Polyline3D__get_SignedAreaIn2D(p);
    if (Math.abs(area) < 1E-12) {
        fail(`Polyline3D.IsCounterClockwiseIn2D: Polyline3D the area is zero: ${p}`);
    }
    return area > 0;
}

/**
 * Test if Polyline3D is Clockwise when projected in 2D.
 * Z values are ignored.
 * The Polyline3D does not need to be actually closed.
 * The signed area of the Polyline3D is calculated.
 * If it is positive the Polyline3D is CCW.
 */
export function Polyline3D__get_IsClockwiseIn2D(p) {
    const area = Polyline3D__get_SignedAreaIn2D(p);
    if (Math.abs(area) < 1E-12) {
        fail(`Polyline3D.IsClockwiseIn2D: Polyline3D the area is zero: ${p}`);
    }
    return area < 0;
}

/**
 * Returns the point at a given parameter on the Polyline3D.
 * The integer part of the parameter is the index of the segment that the point is on.
 * The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
 * The domain Polyline3D starts at 0.0 and ends at points.Count - 1.0 .
 * If the parameter is within 1e-6 of an integer value, the integer value is used as parameter.
 */
export function Polyline3D__EvaluateAt_5E38073B(pl, t) {
    const i = ~~t | 0;
    const p = t - i;
    const count = Polyline3D__get_Points(pl).length | 0;
    const countF = count;
    if (t < 1E-06) {
        if (t < -1E-06) {
            fail(`Polyline3D.EvaluateAt: Parameter ${t} is less than 0.0`);
        }
        const this$ = Polyline3D__get_Points(pl);
        if (this$.length === 0) {
            failRarr("First.get", this$);
        }
        return item(0, this$);
    }
    else if (t > (countF - 1E-06)) {
        if (t > (countF + 1E-06)) {
            fail(`Polyline3D.EvaluateAt: Parameter ${t} is more than point count ${Polyline3D__get_Points(pl).length}.`);
        }
        const this$_1 = Polyline3D__get_Points(pl);
        if (this$_1.length === 0) {
            failRarr("Last.get", this$_1);
        }
        return item(this$_1.length - 1, this$_1);
    }
    else if (p < 1E-06) {
        return item(i, Polyline3D__get_Points(pl));
    }
    else if (p > (1 - 1E-06)) {
        return item(i + 1, Polyline3D__get_Points(pl));
    }
    else {
        const t_1 = item(i, Polyline3D__get_Points(pl));
        let v;
        const a = item(i + 1, Polyline3D__get_Points(pl));
        const b = t_1;
        v = Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
        const p_1 = t_1;
        let v_1;
        const a_1 = v;
        const f = p;
        v_1 = Vec_$ctor_Z7AD9E565(a_1.X * f, a_1.Y * f, a_1.Z * f);
        return Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z);
    }
}

export function Polyline3D__TangentAt_5E38073B(pl, _t) {
    return fail("Polyline3D.TangentAt is obsolete, use GetSegment(i).UnitTangent instead.");
}

/**
 * Returns the parameter on the Polyline3D that is the closest point to the given point.
 * The integer part of the parameter is the index of the segment that the point is on.
 * The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
 * The domain Polyline3D starts at 0.0 and ends at points.Count - 1.0 .
 */
export function Polyline3D__ClosestParameter_Z394ECE4D(pl, p) {
    const pts = Polyline3D__get_Points(pl);
    if (pts.length === 0) {
        fail("Polyline3D.ClosestParameter failed on empty Polyline3D");
    }
    let a = item(0, pts);
    let minT = 0;
    let seg = 0;
    let minDistSq;
    const a_2 = a;
    const b_1 = p;
    const x = a_2.X - b_1.X;
    const y = a_2.Y - b_1.Y;
    const z = a_2.Z - b_1.Z;
    minDistSq = (((x * x) + (y * y)) + (z * z));
    for (let i = 1; i <= (pts.length - 1); i++) {
        const b_2 = item(i, pts);
        const dx = b_2.X - a.X;
        const dy = b_2.Y - a.Y;
        const dz = b_2.Z - a.Z;
        if (((dx !== 0) ? true : (dy !== 0)) ? true : (dz !== 0)) {
            const t = ((((p.X - a.X) * dx) + ((p.Y - a.Y) * dy)) + ((p.Z - a.Z) * dz)) / (((dx * dx) + (dy * dy)) + (dz * dz));
            const t$0027 = max(0, min(1, t));
            const projX = a.X + (dx * t$0027);
            const projY = a.Y + (dy * t$0027);
            const projZ = a.Z + (dz * t$0027);
            const dpx = p.X - projX;
            const dpy = p.Y - projY;
            const dpz = p.Z - projZ;
            const distSq = ((dpx * dpx) + (dpy * dpy)) + (dpz * dpz);
            if (distSq < minDistSq) {
                minDistSq = distSq;
                minT = t$0027;
                seg = ((i - 1) | 0);
            }
        }
        else {
            const dpx_1 = p.X - a.X;
            const dpy_1 = p.Y - a.Y;
            const dpz_1 = p.Z - a.Z;
            const distSq_1 = ((dpx_1 * dpx_1) + (dpy_1 * dpy_1)) + (dpz_1 * dpz_1);
            if (distSq_1 < minDistSq) {
                minDistSq = distSq_1;
                minT = 0;
                seg = ((i - 1) | 0);
            }
        }
        a = b_2;
    }
    return seg + minT;
}

/**
 * Returns the point on the Polyline3D that is the closest point to the given point.
 */
export function Polyline3D__ClosestPoint_Z394ECE4D(pl, p) {
    const pts = Polyline3D__get_Points(pl);
    if (pts.length === 0) {
        fail("Polyline3D.ClosestPoint failed on empty Polyline3D");
    }
    let a = item(0, pts);
    let minPt = a;
    let minDistSq;
    const a_2 = a;
    const b_1 = p;
    const x = a_2.X - b_1.X;
    const y = a_2.Y - b_1.Y;
    const z = a_2.Z - b_1.Z;
    minDistSq = (((x * x) + (y * y)) + (z * z));
    for (let i = 1; i <= (pts.length - 1); i++) {
        const b_2 = item(i, pts);
        const dx = b_2.X - a.X;
        const dy = b_2.Y - a.Y;
        const dz = b_2.Z - a.Z;
        if (((dx !== 0) ? true : (dy !== 0)) ? true : (dz !== 0)) {
            const t = ((((p.X - a.X) * dx) + ((p.Y - a.Y) * dy)) + ((p.Z - a.Z) * dz)) / (((dx * dx) + (dy * dy)) + (dz * dz));
            const t$0027 = max(0, min(1, t));
            const projX = a.X + (dx * t$0027);
            const projY = a.Y + (dy * t$0027);
            const projZ = a.Z + (dz * t$0027);
            const dpx = p.X - projX;
            const dpy = p.Y - projY;
            const dpz = p.Z - projZ;
            const distSq = ((dpx * dpx) + (dpy * dpy)) + (dpz * dpz);
            if (distSq < minDistSq) {
                minDistSq = distSq;
                minPt = Pnt_$ctor_Z7AD9E565(projX, projY, projZ);
            }
        }
        else {
            const dpx_1 = p.X - a.X;
            const dpy_1 = p.Y - a.Y;
            const dpz_1 = p.Z - a.Z;
            const distSq_1 = ((dpx_1 * dpx_1) + (dpy_1 * dpy_1)) + (dpz_1 * dpz_1);
            if (distSq_1 < minDistSq) {
                minDistSq = distSq_1;
                minPt = a;
            }
        }
        a = b_2;
    }
    return minPt;
}

/**
 * Returns the index into the Polylines point list of the vertex that is closest to the given point.
 */
export function Polyline3D__ClosestVertex_Z394ECE4D(pl, p) {
    const pts = Polyline3D__get_Points(pl);
    if (pts.length === 0) {
        fail("Polyline3D.ClosestVertex failed on empty Polyline3D");
    }
    let minIndex = 0;
    let minDistSq;
    const a_1 = item(0, pts);
    const b_1 = p;
    const x = a_1.X - b_1.X;
    const y = a_1.Y - b_1.Y;
    const z = a_1.Z - b_1.Z;
    minDistSq = (((x * x) + (y * y)) + (z * z));
    for (let i = 1; i <= (pts.length - 1); i++) {
        let dSq;
        const a_3 = item(i, pts);
        const b_3 = p;
        const x_1 = a_3.X - b_3.X;
        const y_1 = a_3.Y - b_3.Y;
        const z_1 = a_3.Z - b_3.Z;
        dSq = (((x_1 * x_1) + (y_1 * y_1)) + (z_1 * z_1));
        if (dSq < minDistSq) {
            minDistSq = dSq;
            minIndex = (i | 0);
        }
    }
    return minIndex | 0;
}

/**
 * Returns the distance of the test point to the closest point on the Polyline3D.
 */
export function Polyline3D__DistanceTo_Z394ECE4D(pl, p) {
    const pts = Polyline3D__get_Points(pl);
    if (pts.length === 0) {
        fail("Polyline3D.DistanceTo failed on empty Polyline3D");
    }
    let a = item(0, pts);
    let minDistSq;
    const a_2 = a;
    const b_1 = p;
    const x = a_2.X - b_1.X;
    const y = a_2.Y - b_1.Y;
    const z = a_2.Z - b_1.Z;
    minDistSq = (((x * x) + (y * y)) + (z * z));
    for (let i = 1; i <= (pts.length - 1); i++) {
        const b_2 = item(i, pts);
        const dx = b_2.X - a.X;
        const dy = b_2.Y - a.Y;
        const dz = b_2.Z - a.Z;
        if (((dx !== 0) ? true : (dy !== 0)) ? true : (dz !== 0)) {
            const t = ((((p.X - a.X) * dx) + ((p.Y - a.Y) * dy)) + ((p.Z - a.Z) * dz)) / (((dx * dx) + (dy * dy)) + (dz * dz));
            const t$0027 = max(0, min(1, t));
            const projX = a.X + (dx * t$0027);
            const projY = a.Y + (dy * t$0027);
            const projZ = a.Z + (dz * t$0027);
            const dpx = p.X - projX;
            const dpy = p.Y - projY;
            const dpz = p.Z - projZ;
            const distSq = ((dpx * dpx) + (dpy * dpy)) + (dpz * dpz);
            if (distSq < minDistSq) {
                minDistSq = distSq;
            }
        }
        else {
            const dpx_1 = p.X - a.X;
            const dpy_1 = p.Y - a.Y;
            const dpz_1 = p.Z - a.Z;
            const distSq_1 = ((dpx_1 * dpx_1) + (dpy_1 * dpy_1)) + (dpz_1 * dpz_1);
            if (distSq_1 < minDistSq) {
                minDistSq = distSq_1;
            }
        }
        a = b_2;
    }
    return Math.sqrt(minDistSq);
}

/**
 * Returns the average center of all points of the Polyline3D.
 */
export function Polyline3D__get_Center(pl) {
    if (pl.points.length === 0) {
        failTooFewPoly3D("Center", 1, Polyline3D__get_PointCount(pl));
    }
    let x = 0;
    let y = 0;
    let z = 0;
    for (let i = 0; i <= (pl.points.length - 1); i++) {
        const p = item(i, pl.points);
        x = (x + p.X);
        y = (y + p.Y);
        z = (z + p.Z);
    }
    return Pnt_$ctor_Z7AD9E565(x / pl.points.length, y / pl.points.length, z / pl.points.length);
}

/**
 * Returns the average normal vector of the Polyline3D.
 * It is calculated by summing up the cross products of all segments around the center point.
 * Does not check for bad input, may be zero length if points are colinear.
 */
export function Polyline3D__get_AverageNormal(pl) {
    const c = Polyline3D__get_Center(pl);
    let normal = Vec_$ctor_Z7AD9E565(0, 0, 0);
    const pts = Polyline3D__get_Points(pl);
    let a_1;
    const a = item(pts.length - 1, pts);
    const b = c;
    a_1 = Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
    for (let i = 0; i <= (pts.length - 1); i++) {
        let a_4, b_4, a_3, b_3;
        let b_2;
        const a_2 = item(i, pts);
        const b_1 = c;
        b_2 = Vec_$ctor_Z7AD9E565(a_2.X - b_1.X, a_2.Y - b_1.Y, a_2.Z - b_1.Z);
        normal = ((a_4 = normal, (b_4 = ((a_3 = a_1, (b_3 = b_2, Vec_$ctor_Z7AD9E565((a_3.Y * b_3.Z) - (a_3.Z * b_3.Y), (a_3.Z * b_3.X) - (a_3.X * b_3.Z), (a_3.X * b_3.Y) - (a_3.Y * b_3.X))))), Vec_$ctor_Z7AD9E565(a_4.X + b_4.X, a_4.Y + b_4.Y, a_4.Z + b_4.Z))));
        a_1 = b_2;
    }
    return normal;
}

/**
 * Scales the 3D polyline by a given factor.
 * Scale center is World Origin 0,0,0
 */
export function Polyline3D__Scale_5E38073B(p, factor) {
    return Polyline3D_$ctor_516DFD0A(ResizeArr_map((pt) => {
        const a = pt;
        const f = factor;
        return Pnt_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f);
    }, p.points));
}

/**
 * Scales the 3D polyline by a given factor on a given center point
 */
export function Polyline3D__ScaleOn(p, cen, factor) {
    const cx = cen.X;
    const cy = cen.Y;
    const cz = cen.Z;
    return Polyline3D_$ctor_516DFD0A(ResizeArr_map((pt) => Pnt_$ctor_Z7AD9E565(cx + ((pt.X - cx) * factor), cy + ((pt.Y - cy) * factor), cz + ((pt.Z - cz) * factor)), p.points));
}

/**
 * Returns a Polyline3D moved by a vector.
 */
export function Polyline3D__Move_Z394EC5F7(p, v) {
    return Polyline3D_$ctor_516DFD0A(ResizeArr_map((pt) => {
        const p_1 = pt;
        const v_1 = v;
        return Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z);
    }, p.points));
}

/**
 * Returns a Polyline3D moved by a given distance in X direction.
 */
export function Polyline3D__MoveX_5E38073B(p, distance) {
    return Polyline3D_$ctor_516DFD0A(ResizeArr_map((pt) => Pnt_$ctor_Z7AD9E565(pt.X + distance, pt.Y, pt.Z), p.points));
}

/**
 * Returns a Polyline3D moved by a given distance in Y direction.
 */
export function Polyline3D__MoveY_5E38073B(p, distance) {
    return Polyline3D_$ctor_516DFD0A(ResizeArr_map((pt) => Pnt_$ctor_Z7AD9E565(pt.X, pt.Y + distance, pt.Z), p.points));
}

/**
 * Returns a Polyline3D moved by a given distance in Z direction.
 */
export function Polyline3D__MoveZ_5E38073B(p, distance) {
    return Polyline3D_$ctor_516DFD0A(ResizeArr_map((pt) => Pnt_$ctor_Z7AD9E565(pt.X, pt.Y, pt.Z + distance), p.points));
}

/**
 * Applies or multiplies a 4x4 transformation matrix to the Polyline3D.
 */
export function Polyline3D__Transform_3CAE9522(p, m) {
    return Polyline3D_$ctor_516DFD0A(ResizeArr_map((pt) => {
        const p_1 = pt;
        const m_1 = m;
        const x = p_1.X;
        const y = p_1.Y;
        const z = p_1.Z;
        const x$0027 = (((m_1.M11 * x) + (m_1.M21 * y)) + (m_1.M31 * z)) + m_1.X41;
        const y$0027 = (((m_1.M12 * x) + (m_1.M22 * y)) + (m_1.M32 * z)) + m_1.Y42;
        const z$0027 = (((m_1.M13 * x) + (m_1.M23 * y)) + (m_1.M33 * z)) + m_1.Z43;
        const w$0027 = (((m_1.M14 * x) + (m_1.M24 * y)) + (m_1.M34 * z)) + m_1.M44;
        const sc = 1 / w$0027;
        return Pnt_$ctor_Z7AD9E565(x$0027 * sc, y$0027 * sc, z$0027 * sc);
    }, p.points));
}

/**
 * Multiplies (or applies) a RigidMatrix to the Polyline3D.
 */
export function Polyline3D__TransformRigid_Z625426AD(p, m) {
    return Polyline3D_$ctor_516DFD0A(ResizeArr_map((pt) => {
        const v = pt;
        const m_4 = m;
        const x = v.X;
        const y = v.Y;
        const z = v.Z;
        return Pnt_$ctor_Z7AD9E565((((m_4.M11 * x) + (m_4.M21 * y)) + (m_4.M31 * z)) + m_4.X41, (((m_4.M12 * x) + (m_4.M22 * y)) + (m_4.M32 * z)) + m_4.Y42, (((m_4.M13 * x) + (m_4.M23 * y)) + (m_4.M33 * z)) + m_4.Z43);
    }, p.points));
}

/**
 * Multiplies (or applies) a Quaternion to the Polyline3D.
 * The polyline is rotated around the World Origin.
 */
export function Polyline3D__Rotate_Z2A007687(p, q) {
    return Polyline3D_$ctor_516DFD0A(ResizeArr_map((pt) => {
        const p_1 = pt;
        const q_1 = q;
        const x = p_1.X;
        const y = p_1.Y;
        const z = p_1.Z;
        const qx = q_1.X;
        const qy = q_1.Y;
        const qz = q_1.Z;
        const qw = q_1.W;
        const tx = 2 * ((qy * z) - (qz * y));
        const ty = 2 * ((qz * x) - (qx * z));
        const tz = 2 * ((qx * y) - (qy * x));
        return Pnt_$ctor_Z7AD9E565(((x + (qw * tx)) + (qy * tz)) - (qz * ty), ((y + (qw * ty)) + (qz * tx)) - (qx * tz), ((z + (qw * tz)) + (qx * ty)) - (qy * tx));
    }, p.points));
}

/**
 * Multiplies (or applies) a Quaternion to the Polyline3D around a given center point.
 */
export function Polyline3D__RotateWithCenter_4928E16A(p, cen, q) {
    return Polyline3D_$ctor_516DFD0A(ResizeArr_map((pt) => {
        const cen_2 = cen;
        const q_2 = q;
        const pt_2 = pt;
        const x = pt_2.X - cen_2.X;
        const y = pt_2.Y - cen_2.Y;
        const z = pt_2.Z - cen_2.Z;
        const qx = q_2.X;
        const qy = q_2.Y;
        const qz = q_2.Z;
        const qw = q_2.W;
        const ix = ((qw * x) + (qy * z)) - (qz * y);
        const iy = ((qw * y) + (qz * x)) - (qx * z);
        const iz = ((qw * z) + (qx * y)) - (qy * x);
        const iw = ((-qx * x) - (qy * y)) - (qz * z);
        return Pnt_$ctor_Z7AD9E565_1(((((ix * qw) + (iw * -qx)) + (iy * -qz)) - (iz * -qy)) + cen_2.X, ((((iy * qw) + (iw * -qy)) + (iz * -qx)) - (ix * -qz)) + cen_2.Y, ((((iz * qw) + (iw * -qz)) + (ix * -qy)) - (iy * -qx)) + cen_2.Z);
    }, p.points));
}

/**
 * Gets the internal list of all Points of the Polyline3D.
 * This is not a copy, so changes to the list will be reflected in the Polyline3D.
 */
export function Polyline3D_pointsUnsafeInternal_Z5A89AE96(p) {
    return Polyline3D__get_Points(p);
}

/**
 * Gets first point of the Polyline3D
 */
export function Polyline3D_start_Z5A89AE96(pl) {
    const points = Polyline3D__get_Points(pl);
    if (points.length < 1) {
        failTooFewPoly3D("start", 1, Polyline3D__get_PointCount(pl));
    }
    return item(0, points);
}

/**
 * Gets last or end point of the Polyline3D
 */
export function Polyline3D_ende_Z5A89AE96(pl) {
    const points = Polyline3D__get_Points(pl);
    if (points.length < 1) {
        failTooFewPoly3D("ende", 1, Polyline3D__get_PointCount(pl));
    }
    return item(points.length - 1, points);
}

/**
 * Reverse order of the Polyline3D in place.
 */
export function Polyline3D_reverseInPlace_Z5A89AE96(p) {
    Polyline3D__ReverseInPlace(p);
}

/**
 * Returns new Polyline3D in reversed Order.
 */
export function Polyline3D_reverse_Z5A89AE96(p) {
    return Polyline3D__Reverse(p);
}

/**
 * Returns the point at a given parameter on the Polyline3D.
 * The integer part of the parameter is the index of the segment that the point is on.
 * The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
 * The domain Polyline3D starts at 0.0 and ends at point count.
 */
export function Polyline3D_evaluateAt(t, pl) {
    return Polyline3D__EvaluateAt_5E38073B(pl, t);
}

/**
 * Apply a mapping function to each point in the 3D Polyline. Returns new Polyline3D.
 */
export function Polyline3D_map(mapping, pl) {
    return Polyline3D_createDirectlyUnsafe_516DFD0A(ResizeArr_map(mapping, Polyline3D__get_Points(pl)));
}

/**
 * Move a Polyline3D by a vector. (same as Polyline3D.move)
 */
export function Polyline3D_translate(v, pl) {
    return Polyline3D_map((a) => {
        const p = a;
        const v_3 = v;
        return Pnt_$ctor_Z7AD9E565(p.X + v_3.X, p.Y + v_3.Y, p.Z + v_3.Z);
    }, pl);
}

/**
 * Move a Polyline3D by a vector. (same as Polyline3D.translate)
 */
export function Polyline3D_move(v, pl) {
    return Polyline3D_translate(v, pl);
}

/**
 * Returns a Polyline3D moved by a given distance in X direction.
 */
export function Polyline3D_moveX(distance, pl) {
    return Polyline3D_map((pt) => {
        const pt_1 = pt;
        return Pnt_$ctor_Z7AD9E565_1(pt_1.X + distance, pt_1.Y, pt_1.Z);
    }, pl);
}

/**
 * Returns a Polyline3D moved by a given distance in Y direction.
 */
export function Polyline3D_moveY(distance, pl) {
    return Polyline3D_map((pt) => {
        const pt_1 = pt;
        return Pnt_$ctor_Z7AD9E565_1(pt_1.X, pt_1.Y + distance, pt_1.Z);
    }, pl);
}

/**
 * Returns a Polyline3D moved by a given distance in Z direction.
 */
export function Polyline3D_moveZ(distance, pl) {
    return Polyline3D_map((pt) => {
        const pt_1 = pt;
        return Pnt_$ctor_Z7AD9E565_1(pt_1.X, pt_1.Y, pt_1.Z + distance);
    }, pl);
}

/**
 * Scales the Polyline3D by a given factor.
 * Scale center is World Origin 0,0,0
 * Returns a new Polyline3D.
 */
export function Polyline3D_scale(factor, pl) {
    return Polyline3D_map((pt) => {
        const a = pt;
        const f = factor;
        return Pnt_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f);
    }, pl);
}

/**
 * Applies a 4x4 transformation matrix.
 */
export function Polyline3D_transform(m, pl) {
    return Polyline3D_map((p) => {
        const p_3 = p;
        const m_4 = m;
        const x = p_3.X;
        const y = p_3.Y;
        const z = p_3.Z;
        const x$0027 = (((m_4.M11 * x) + (m_4.M21 * y)) + (m_4.M31 * z)) + m_4.X41;
        const y$0027 = (((m_4.M12 * x) + (m_4.M22 * y)) + (m_4.M32 * z)) + m_4.Y42;
        const z$0027 = (((m_4.M13 * x) + (m_4.M23 * y)) + (m_4.M33 * z)) + m_4.Z43;
        const w$0027 = (((m_4.M14 * x) + (m_4.M24 * y)) + (m_4.M34 * z)) + m_4.M44;
        const sc = 1 / w$0027;
        return Pnt_$ctor_Z7AD9E565(x$0027 * sc, y$0027 * sc, z$0027 * sc);
    }, pl);
}

/**
 * Multiplies (or applies) a RigidMatrix to the Polyline3D.
 */
export function Polyline3D_transformRigid(m, pl) {
    return Polyline3D_map((p) => {
        const v = p;
        const m_4 = m;
        const x = v.X;
        const y = v.Y;
        const z = v.Z;
        return Pnt_$ctor_Z7AD9E565((((m_4.M11 * x) + (m_4.M21 * y)) + (m_4.M31 * z)) + m_4.X41, (((m_4.M12 * x) + (m_4.M22 * y)) + (m_4.M32 * z)) + m_4.Y42, (((m_4.M13 * x) + (m_4.M23 * y)) + (m_4.M33 * z)) + m_4.Z43);
    }, pl);
}

/**
 * Rotation a Polyline3D around Z-Axis.
 */
export function Polyline3D_rotate2D(r, pl) {
    return Polyline3D_map((p) => Euclid_Pnt__Pnt_rotateZBy_Static(r, p), pl);
}

/**
 * Rotation a Polyline3D around Z-Axis.
 */
export function Polyline3D_rotate(r, pl) {
    return Polyline3D_map((p) => Euclid_Pnt__Pnt_rotateZBy_Static(r, p), pl);
}

/**
 * Rotation a Polyline3D round given Center point an a local Z-axis.
 */
export function Polyline3D_rotate2DWithCenter(cen, r, pl) {
    return Polyline3D_map((pt) => Euclid_Pnt__Pnt_rotateZwithCenterBy_Static(cen, r, pt), pl);
}

/**
 * Rotation a Polyline3D round given Center point an a local Z-axis.
 */
export function Polyline3D_rotateWithCenter(cen, r, pl) {
    return Polyline3D_map((pt) => Euclid_Pnt__Pnt_rotateZwithCenterBy_Static(cen, r, pt), pl);
}

/**
 * Multiplies (or applies) a Quaternion to the Polyline3D.
 * The polyline is rotated around the World Origin.
 */
export function Polyline3D_rotateByQuaternion(q, pl) {
    return Polyline3D_map((pt) => {
        const p = pt;
        const q_1 = q;
        const x = p.X;
        const y = p.Y;
        const z = p.Z;
        const qx = q_1.X;
        const qy = q_1.Y;
        const qz = q_1.Z;
        const qw = q_1.W;
        const tx = 2 * ((qy * z) - (qz * y));
        const ty = 2 * ((qz * x) - (qx * z));
        const tz = 2 * ((qx * y) - (qy * x));
        return Pnt_$ctor_Z7AD9E565(((x + (qw * tx)) + (qy * tz)) - (qz * ty), ((y + (qw * ty)) + (qz * tx)) - (qx * tz), ((z + (qw * tz)) + (qx * ty)) - (qy * tx));
    }, pl);
}

/**
 * Multiplies (or applies) a Quaternion to the Polyline3D around a given center point.
 */
export function Polyline3D_rotateWithCenterByQuaternion(cen, q, pl) {
    return Polyline3D_map((pt) => {
        const cen_2 = cen;
        const q_2 = q;
        const pt_1 = pt;
        const x = pt_1.X - cen_2.X;
        const y = pt_1.Y - cen_2.Y;
        const z = pt_1.Z - cen_2.Z;
        const qx = q_2.X;
        const qy = q_2.Y;
        const qz = q_2.Z;
        const qw = q_2.W;
        const ix = ((qw * x) + (qy * z)) - (qz * y);
        const iy = ((qw * y) + (qz * x)) - (qx * z);
        const iz = ((qw * z) + (qx * y)) - (qy * x);
        const iw = ((-qx * x) - (qy * y)) - (qz * z);
        return Pnt_$ctor_Z7AD9E565_1(((((ix * qw) + (iw * -qx)) + (iy * -qz)) - (iz * -qy)) + cen_2.X, ((((iy * qw) + (iw * -qy)) + (iz * -qx)) - (ix * -qz)) + cen_2.Y, ((((iz * qw) + (iw * -qz)) + (ix * -qy)) - (iy * -qx)) + cen_2.Z);
    }, pl);
}

/**
 * Create a new Polyline3D by copying over all points.
 */
export function Polyline3D_create_5416D85C(points) {
    return Polyline3D_$ctor_516DFD0A(Array.from(points));
}

/**
 * Create a new Polyline3D by using the provided ResizeArray directly.
 * All later changes to the ResizeArray will be reflected in the Polyline3D.
 */
export function Polyline3D_createDirectlyUnsafe_516DFD0A(points) {
    return Polyline3D_$ctor_516DFD0A(points);
}

/**
 * Create a new empty Polyline3D without any points.
 * But predefined capacity.
 */
export function Polyline3D_createEmpty_Z524259A4(capacity) {
    return Polyline3D_$ctor_516DFD0A([]);
}

/**
 * Returns new Polyline3D from point at Parameter a to point at Parameter b.
 * if 'a' is bigger 'b' then the new Polyline3D is in opposite direction.
 * If a parameter is within 1e-4 of an integer value, the integer value is used as parameter.
 */
export function Polyline3D_subPolyline(a, b, pl) {
    const rev = a > b;
    const patternInput = rev ? [b, a] : [a, b];
    const v = patternInput[1];
    const u = patternInput[0];
    const np = Polyline3D_createEmpty_Z524259A4(~~(v - u) + 2);
    const nps = Polyline3D__get_Points(np);
    const ps = Polyline3D__get_Points(pl);
    const ui = ~~u | 0;
    const uf = u - ui;
    if (uf < 0.9999) {
        void (nps.push(Polyline3D__EvaluateAt_5E38073B(pl, u)));
    }
    for (let i = ~~u + 1; i <= ~~v; i++) {
        if ((i >= 0) && (i < ps.length)) {
            void (nps.push(item(i, ps)));
        }
    }
    const vi = ~~v | 0;
    const vf = v - vi;
    if (vf > 0.0001) {
        void (nps.push(Polyline3D__EvaluateAt_5E38073B(pl, v)));
    }
    if (rev) {
        Polyline3D__ReverseInPlace(np);
    }
    return np;
}

export function Polyline3D_segment(a, b, pl) {
    return Polyline3D_subPolyline(a, b, pl);
}

/**
 * Returns a new closed Polyline3D.
 * If the first and last point are within 1e-6 of each other, the last point is set equal to the first point.
 * Otherwise one point is added.
 */
export function Polyline3D_close_Z5A89AE96(pl) {
    let a_1, this$, b_1, this$_1, x, y, z, this$_2, this$_3;
    if (Polyline3D__get_Points(pl).length < 2) {
        failTooFewPoly3D("close", 2, Polyline3D__get_PointCount(pl));
    }
    const points = Polyline3D__get_Points(pl);
    const np = Polyline3D_createEmpty_Z524259A4(points.length + 1);
    addRangeInPlace(getRange(points, 0, points.length), Polyline3D__get_Points(np));
    if (((a_1 = ((this$ = points, ((this$.length === 0) ? failRarr("First.get", this$) : undefined, item(0, this$)))), (b_1 = ((this$_1 = points, ((this$_1.length === 0) ? failRarr("Last.get", this$_1) : undefined, item(this$_1.length - 1, this$_1)))), (x = (a_1.X - b_1.X), (y = (a_1.Y - b_1.Y), (z = (a_1.Z - b_1.Z), ((x * x) + (y * y)) + (z * z))))))) < 1E-12) {
        setItem(Polyline3D__get_Points(np), Polyline3D__get_Points(np).length - 1, (this$_2 = Polyline3D__get_Points(np), ((this$_2.length === 0) ? failRarr("First.get", this$_2) : undefined, item(0, this$_2))));
    }
    else {
        void (Polyline3D__get_Points(np).push((this$_3 = Polyline3D__get_Points(np), ((this$_3.length === 0) ? failRarr("First.get", this$_3) : undefined, item(0, this$_3)))));
    }
    return np;
}

/**
 * Closes the Polyline3D in place by adding a point.
 * If the first and last point are within 1e-6 of each other, the last point is set equal to the first point instead.
 */
export function Polyline3D_closeInPlace_Z5A89AE96(pl) {
    let a_1, this$, b_1, this$_1, x, y, z, this$_2, this$_3;
    if (Polyline3D__get_Points(pl).length < 2) {
        failTooFewPoly3D("closeInPlace", 2, Polyline3D__get_PointCount(pl));
    }
    const points = Polyline3D__get_Points(pl);
    if (((a_1 = ((this$ = points, ((this$.length === 0) ? failRarr("First.get", this$) : undefined, item(0, this$)))), (b_1 = ((this$_1 = points, ((this$_1.length === 0) ? failRarr("Last.get", this$_1) : undefined, item(this$_1.length - 1, this$_1)))), (x = (a_1.X - b_1.X), (y = (a_1.Y - b_1.Y), (z = (a_1.Z - b_1.Z), ((x * x) + (y * y)) + (z * z))))))) < 1E-12) {
        setItem(points, points.length - 1, (this$_2 = points, ((this$_2.length === 0) ? failRarr("First.get", this$_2) : undefined, item(0, this$_2))));
    }
    else {
        void (points.push((this$_3 = points, ((this$_3.length === 0) ? failRarr("First.get", this$_3) : undefined, item(0, this$_3)))));
    }
}

/**
 * Tests if two Polyline3D have the same number of points and points are equal within a given tolerance.
 */
export function Polyline3D_equals(tol, a, b) {
    const k = Polyline3D__get_PointCount(a) | 0;
    if (k !== Polyline3D__get_PointCount(b)) {
        return false;
    }
    else {
        let i = 0;
        let same = true;
        const aPts = Polyline3D__get_Points(a);
        const bPts = Polyline3D__get_Points(b);
        while ((i < k) && same) {
            let tol_2, a_2, b_2;
            if ((tol_2 = tol, (a_2 = item(i, aPts), (b_2 = item(i, bPts), ((Math.abs(a_2.X - b_2.X) <= tol_2) && (Math.abs(a_2.Y - b_2.Y) <= tol_2)) && (Math.abs(a_2.Z - b_2.Z) <= tol_2))))) {
                i = ((i + 1) | 0);
            }
            else {
                same = false;
            }
        }
        return same;
    }
}

/**
 * Removes consecutive duplicate points from the Polyline3D within a given tolerance.
 * This algorithm allows the last and first point to be identical if the Polyline3D is closed.
 */
export function Polyline3D_removeDuplicatePoints(distanceTolerance, pl) {
    const pts = Polyline3D__get_Points(pl);
    if (pts.length < 2) {
        return pl;
    }
    else {
        const nps = [];
        let last = item(0, pts);
        void (nps.push(last));
        for (let i = 1; i <= (pts.length - 1); i++) {
            let tol_1, a_1, b_1;
            const p = item(i, pts);
            if (!((tol_1 = distanceTolerance, (a_1 = last, (b_1 = p, ((Math.abs(a_1.X - b_1.X) <= tol_1) && (Math.abs(a_1.Y - b_1.Y) <= tol_1)) && (Math.abs(a_1.Z - b_1.Z) <= tol_1)))))) {
                void (nps.push(p));
                last = p;
            }
        }
        return Polyline3D_createDirectlyUnsafe_516DFD0A(nps);
    }
}

/**
 * Removes consecutive duplicate points and colinear points from the Polyline3D within given tolerances.
 * This algorithm allows the last and first point to be identical if the Polyline3D is closed.
 * Colinear points are removed when the angle between segments is smaller than the cosine threshold (e.g. cosine of 0.5 degrees ).
 * If the Polyline3D is closed and starts and ends with colinear segments, the first point is replaced with the last non-colinear point.
 * So the joint of the loop is now moved to the last non-colinear point.
 * So that there are no colinear segments even between start and end.
 */
export function Polyline3D_removeColinearAndDuplicatePoints(angleTolerance, distanceTolerance, pl) {
    let this$_2, this$_3;
    if (angleTolerance < 0.7071067811865476) {
        fail(`Polyline3D.removeColinearAndDuplicatePoints: angleTolerance must be at least Cosine.\`\`45.0\`\` ( that is 0.707) but was ${angleTolerance} (= ${Math.acos(angleTolerance)} degrees).`);
    }
    if (angleTolerance > 0.9999999847691291) {
        fail(`Polyline3D.removeColinearAndDuplicatePoints: angleTolerance must be at most Cosine.\`\`0.01\`\` ( that is 0.999999984) but was ${angleTolerance} (= ${Math.acos(angleTolerance)} degrees).`);
    }
    const pts = Polyline3D__get_Points(pl);
    if (pts.length < 2) {
        return pl;
    }
    else {
        const nps = [];
        const lastIdx = (pts.length - 1) | 0;
        let prev = item(0, pts);
        void (nps.push(prev));
        let i = 1;
        let this$_1 = item(i, pts);
        let len;
        const a_1 = prev;
        const b_1 = this$_1;
        const x = a_1.X - b_1.X;
        const y = a_1.Y - b_1.Y;
        const z = a_1.Z - b_1.Z;
        len = Math.sqrt(((x * x) + (y * y)) + (z * z));
        while ((len < distanceTolerance) && (i < lastIdx)) {
            let a_3, b_3, x_1, y_1, z_1;
            i = ((i + 1) | 0);
            this$_1 = item(i, pts);
            len = ((a_3 = prev, (b_3 = this$_1, (x_1 = (a_3.X - b_3.X), (y_1 = (a_3.Y - b_3.Y), (z_1 = (a_3.Z - b_3.Z), Math.sqrt(((x_1 * x_1) + (y_1 * y_1)) + (z_1 * z_1))))))));
        }
        let firstVec;
        const fromPt = prev;
        const toPt = this$_1;
        const x_2 = toPt.X - fromPt.X;
        const y_2 = toPt.Y - fromPt.Y;
        const z_2 = toPt.Z - fromPt.Z;
        const l = Math.sqrt(((x_2 * x_2) + (y_2 * y_2)) + (z_2 * z_2));
        if (!(l > 1E-12)) {
            failTooClose("UnitVec.create", fromPt, toPt);
        }
        const f = 1 / l;
        firstVec = UnitVec_$ctor_Z7AD9E565(x_2 * f, y_2 * f, z_2 * f);
        let vPrev = firstVec;
        for (let idx = i + 1; idx <= lastIdx; idx++) {
            const next = item(idx, pts);
            const vx = next.X - this$_1.X;
            const vy = next.Y - this$_1.Y;
            const vz = next.Z - this$_1.Z;
            let len_1;
            const value = ((vx * vx) + (vy * vy)) + (vz * vz);
            len_1 = Math.sqrt(value);
            const f_1 = 1 / len_1;
            if (len_1 > distanceTolerance) {
                const vNext = UnitVec_$ctor_Z7AD9E565(vx * f_1, vy * f_1, vz * f_1);
                let cos;
                const a_4 = vPrev;
                const b_4 = vNext;
                cos = (((a_4.X * b_4.X) + (a_4.Y * b_4.Y)) + (a_4.Z * b_4.Z));
                if (cos < angleTolerance) {
                    void (nps.push(this$_1));
                    prev = this$_1;
                    vPrev = vNext;
                }
                this$_1 = next;
            }
        }
        if (Polyline3D__IsAlmostClosed_5E38073B(pl, distanceTolerance)) {
            let cos_1;
            const a_5 = vPrev;
            const b_5 = firstVec;
            cos_1 = (((a_5.X * b_5.X) + (a_5.Y * b_5.Y)) + (a_5.Z * b_5.Z));
            if (cos_1 < angleTolerance) {
                void (nps.push((this$_2 = pts, ((this$_2.length === 0) ? failRarr("Last.get", this$_2) : undefined, item(this$_2.length - 1, this$_2)))));
            }
            else {
                setItem(nps, 0, (this$_3 = nps, ((this$_3.length === 0) ? failRarr("Last.get", this$_3) : undefined, item(this$_3.length - 1, this$_3))));
            }
        }
        else if (Euclid_Pnt__Pnt_notEquals_Static(distanceTolerance, this$_1, prev)) {
            void (nps.push(this$_1));
        }
        return Polyline3D_createDirectlyUnsafe_516DFD0A(nps);
    }
}

/**
 * Offsets a Polyline in 3D space by finding the local plane in each corner.
 * Takes a reference normal for orienting the perpendicular offset and determining inside/outside.
 * Auto-detects if given points are from a closed Polyline (first point = last point) and loops them.
 * This function raises an Exception on duplicate points and 180 degree U-turns.
 */
export function Polyline3D_offsetWithRef_39D084EF(polyLine, inPlaneOffsetDistance, perpendicularOffsetDistance, refNormal, loop = false) {
    let a_1, this$, b_1, this$_1, x, y, z, this$_2, i, v;
    const pts = Polyline3D__get_Points(polyLine);
    if (pts.length < 2) {
        fail(`Polyline3D.offsetWithRef: Polyline3D must have at least 2 points but has ${pts.length} points.`);
    }
    const isOpen = ((a_1 = ((this$ = pts, ((this$.length === 0) ? failRarr("First.get", this$) : undefined, item(0, this$)))), (b_1 = ((this$_1 = pts, ((this$_1.length === 0) ? failRarr("Last.get", this$_1) : undefined, item(this$_1.length - 1, this$_1)))), (x = (a_1.X - b_1.X), (y = (a_1.Y - b_1.Y), (z = (a_1.Z - b_1.Z), ((x * x) + (y * y)) + (z * z))))))) > 1E-12;
    if (loop && isOpen) {
        const closedPts = ResizeArr_closeLoop(pts);
        const uvs = getSegmentUnitVectors(closedPts);
        const dirs = getOffsetDirections(uvs, refNormal, false, 0.9990482215818578, -0.9998476951563913);
        const res = offsetConstantWithDirections(closedPts, dirs, inPlaneOffsetDistance, perpendicularOffsetDistance);
        (this$_2 = res, ((this$_2.length === 0) ? failRarr("Pop", this$_2) : undefined, (i = ((this$_2.length - 1) | 0), (v = item(i, this$_2), (this$_2.splice(i, 1), v)))));
        return Polyline3D_createDirectlyUnsafe_516DFD0A(res);
    }
    else {
        const uvs_1 = getSegmentUnitVectors(pts);
        const dirs_1 = getOffsetDirections(uvs_1, refNormal, isOpen, 0.9990482215818578, -0.9998476951563913);
        return Polyline3D_createDirectlyUnsafe_516DFD0A(offsetConstantWithDirections(pts, dirs_1, inPlaneOffsetDistance, perpendicularOffsetDistance));
    }
}

/**
 * Offsets a Polyline in 3D space by finding the local plane in each corner.
 * Auto-detects if given points are from a closed Polyline (first point = last point) and loops them.
 * The reference normal is computed from the average normal of the polyline.
 * This function raises an Exception on duplicate points, 180 degree U-turns, or colinear points.
 */
export function Polyline3D_offset_1FF60F20(polyLine, inPlaneOffsetDistance, perpendicularOffsetDistance, loop = false) {
    let v, v_1, x, y, z, l, f;
    const pts = Polyline3D__get_Points(polyLine);
    if (pts.length < 2) {
        fail(`Polyline3D.offset: Polyline3D must have at least 2 points but has ${pts.length} points.`);
    }
    const refNormal = Polyline3D__get_AverageNormal(polyLine);
    if (((v = refNormal, ((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z))) < 1E-08) {
        fail(`Polyline3D.offset: Cannot compute average normal of Polyline3D, the ${pts.length} points are colinear or too close together: `);
    }
    return Polyline3D_offsetWithRef_39D084EF(polyLine, inPlaneOffsetDistance, perpendicularOffsetDistance, (v_1 = refNormal, (x = v_1.X, (y = v_1.Y, (z = v_1.Z, (l = Math.sqrt(((x * x) + (y * y)) + (z * z)), (!(l > 1E-12) ? failUnit3("Vec.Unitized", x, y, z) : undefined, (f = (1 / l), UnitVec_$ctor_Z7AD9E565(f * x, f * y, f * z)))))))), loop);
}

/**
 * Offsets a Polyline in 3D space by finding the local plane in each corner.
 * Takes a reference normal for orienting the perpendicular offset and determining inside/outside.
 * Auto-detects if given points are from a closed Polyline (first point = last point) and loops them.
 * This function raises an Exception on duplicate points and 180 degree U-turns.
 */
export function Polyline3D_offsetVarWithRef_3346ED26(polyLine, inPlaneOffsetDistance, perpendicularOffsetDistances, refNormal, loop = false, varDistParallelBehaviour = 1, considerColinearBelow = 0.9990482215818578, failAtUTurnAbove = -0.9961946980917455) {
    let a_1, this$, b_1, this$_1, x, y, z, this$_2, i, v;
    const pts = Polyline3D__get_Points(polyLine);
    if (pts.length < 2) {
        fail(`Polyline3D.offsetVarWithRef: Polyline3D must have at least 2 points but has ${pts.length} points.`);
    }
    const isOpen = ((a_1 = ((this$ = pts, ((this$.length === 0) ? failRarr("First.get", this$) : undefined, item(0, this$)))), (b_1 = ((this$_1 = pts, ((this$_1.length === 0) ? failRarr("Last.get", this$_1) : undefined, item(this$_1.length - 1, this$_1)))), (x = (a_1.X - b_1.X), (y = (a_1.Y - b_1.Y), (z = (a_1.Z - b_1.Z), ((x * x) + (y * y)) + (z * z))))))) > 1E-12;
    if (loop && isOpen) {
        if (count_1(inPlaneOffsetDistance) !== pts.length) {
            fail("Polyline3D.offsetVarWithRef: For open Polyline3D with loop=true the inPlaneOffsetDistance list must have the same number of items as the polyline has points.\n" + (`But polyline has ${pts.length} points and inPlaneOffsetDistance has ${count_1(inPlaneOffsetDistance)} items.`));
        }
        if (count_1(perpendicularOffsetDistances) !== pts.length) {
            fail("Polyline3D.offsetVarWithRef: For open Polyline3D with loop=true the perpendicularOffsetDistances list must have the same number of items as the polyline has points.\n" + (`But polyline has ${pts.length} points and perpendicularOffsetDistances has ${count_1(perpendicularOffsetDistances)} items.`));
        }
        const closedPts = ResizeArr_closeLoop(pts);
        const uvs = getSegmentUnitVectors(closedPts);
        const dirs = getOffsetDirections(uvs, refNormal, false, considerColinearBelow, failAtUTurnAbove);
        const res = offsetVariableWithDirections(closedPts, uvs, dirs, inPlaneOffsetDistance, perpendicularOffsetDistances, true, varDistParallelBehaviour);
        (this$_2 = res, ((this$_2.length === 0) ? failRarr("Pop", this$_2) : undefined, (i = ((this$_2.length - 1) | 0), (v = item(i, this$_2), (this$_2.splice(i, 1), v)))));
        return Polyline3D_createDirectlyUnsafe_516DFD0A(res);
    }
    else {
        const uvs_1 = getSegmentUnitVectors(pts);
        const dirs_1 = getOffsetDirections(uvs_1, refNormal, isOpen, considerColinearBelow, failAtUTurnAbove);
        return Polyline3D_createDirectlyUnsafe_516DFD0A(offsetVariableWithDirections(pts, uvs_1, dirs_1, inPlaneOffsetDistance, perpendicularOffsetDistances, isOpen, varDistParallelBehaviour));
    }
}

/**
 * Offsets a Polyline in 3D space by finding the local plane in each corner.
 * Auto-detects if given points are from a closed Polyline (first point = last point) and loops them.
 * The reference normal is computed from the average normal of the polyline.
 * This function raises an Exception on duplicate points, 180 degree U-turns, or colinear points.
 */
export function Polyline3D_offsetVar_64838AC9(polyLine, inPlaneOffsetDistance, perpendicularOffsetDistances, loop = false, varDistParallelBehaviour = 1, considerColinearBelow = 0.9990482215818578, failAtUTurnAbove = -0.9961946980917455) {
    let v, v_1, x, y, z, l, f;
    const refNormal = Polyline3D__get_AverageNormal(polyLine);
    if (((v = refNormal, ((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z))) < 1E-08) {
        fail(`Polyline3D.offsetVar: Cannot compute average normal of Polyline3D, the ${Polyline3D__get_Points(polyLine).length} points are colinear or too close together: `);
    }
    return Polyline3D_offsetVarWithRef_3346ED26(polyLine, inPlaneOffsetDistance, perpendicularOffsetDistances, (v_1 = refNormal, (x = v_1.X, (y = v_1.Y, (z = v_1.Z, (l = Math.sqrt(((x * x) + (y * y)) + (z * z)), (!(l > 1E-12) ? failUnit3("Vec.Unitized", x, y, z) : undefined, (f = (1 / l), UnitVec_$ctor_Z7AD9E565(f * x, f * y, f * z)))))))), loop, varDistParallelBehaviour, considerColinearBelow, failAtUTurnAbove);
}

export function Polyline3D__CloseIfOpen_5E38073B(p, t) {
    Polyline3D__CloseInPlace_5E38073B(p, t);
}

