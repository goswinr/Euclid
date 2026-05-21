
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { printf, toFail, concat, join } from "../fable_modules/fable-library-js.5.0.0/String.js";
import { ResizeArr_closeLoop, ResizeArr_map } from "./ResizeArr.js";
import { Pt_$ctor_7B00E9A0, Pt__get_AsFSharpCode } from "./Pt.js";
import { addRangeInPlace, item, setItem, getRange } from "../fable_modules/fable-library-js.5.0.0/Array.js";
import { failRarr, failEmptySeq, failNull, failTooFewPoly2D, fail } from "./EuclidErrors.js";
import { min, max } from "../fable_modules/fable-library-js.5.0.0/Double.js";
import { Line2D_$ctor_Z53905FA0 } from "./Line2D.js";
import { Vc_$ctor_7B00E9A0 } from "./Vc.js";
import { Operators_IsNull } from "../fable_modules/fable-library-js.5.0.0/FSharp.Core.js";
import { count as count_1 } from "../fable_modules/fable-library-js.5.0.0/CollectionUtil.js";
import { BRect_$ctor_77D16AC0 } from "./BRect.js";
import { CellHeap__Pop, CellHeap__get_Count, CellHeap__Add_3F6CBBFD, Cell, CellHeap_$ctor } from "./PolyLabel.js";
import { Pt_$ctor_7B00E9A0 as Pt_$ctor_7B00E9A0_1 } from "./Pt.js";
import { failTooClose } from "./EuclidErrors.js";
import { UnitVc_$ctor_7B00E9A0 } from "./UnitVc.js";
import { Euclid_Pt__Pt_notEquals_Static } from "./TypeExtensions/Pt.js";
import { offsetVariableWithDirections, offsetWithDirections, makeOffsetDirections } from "./Offset2D.js";

/**
 * A class holding a list of 2D points representing a mutable 2D Polyline.
 * If the last point is the same as the first point, the Polyline2D is considered closed.
 * The Default constructor uses the provided ResizeArray of points directly,
 * so changes to the list will be reflected in the Polyline2D.
 */
export class Polyline2D {
    constructor(points) {
        this.points = points;
    }
    /**
     * Nicely formatted string representation of the Polyline2D including its length.
     */
    toString() {
        const p = this;
        return (p.points.length === 0) ? "empty Euclid.Polyline2D." : (Polyline2D__get_IsClosed(p) ? (`closed Euclid.Polyline2D with length ${Polyline2D__get_Length(p)}, from ${p.points.length} points`) : (`open Euclid.Polyline2D with length ${Polyline2D__get_Length(p)}, from ${p.points.length} points`));
    }
}

export function Polyline2D_$reflection() {
    return class_type("Euclid.Polyline2D", undefined, Polyline2D);
}

export function Polyline2D_$ctor_Z5FD8CF3C(points) {
    return new Polyline2D(points);
}

/**
 * Gets the internal list of all Points of the Polyline2D.
 * This is not a copy, so changes to the list will be reflected in the Polyline2D.
 */
export function Polyline2D__get_Points(_) {
    return _.points;
}

/**
 * Create a new empty Polyline2D
 */
export function Polyline2D_$ctor() {
    return Polyline2D_$ctor_Z5FD8CF3C([]);
}

/**
 * Create a new empty Polyline2D with predefined capacity for the internal list of points.
 */
export function Polyline2D_$ctor_Z524259A4(capacity) {
    return Polyline2D_$ctor_Z5FD8CF3C([]);
}

/**
 * Format Polyline2D into string including its length.
 */
export function Polyline2D__get_AsString(p) {
    if (p.points.length === 0) {
        return "empty Polyline2D.";
    }
    else if (Polyline2D__get_IsClosed(p)) {
        return `closed Polyline2D with length ${Polyline2D__get_Length(p)}, from ${p.points.length} points`;
    }
    else {
        return `open Polyline2D with length ${Polyline2D__get_Length(p)}, from ${p.points.length} points`;
    }
}

/**
 * Format a 2D polyline into an F# code string that can be used to recreate the point.
 */
export function Polyline2D__get_AsFSharpCode(p) {
    const ptsAsCode = join("; ", ResizeArr_map(Pt__get_AsFSharpCode, p.points));
    return concat("Polyline2D.create [| ", ptsAsCode, ..." |]");
}

/**
 * Creates a copy of the Polyline2D
 * Same as polyline.Clone()
 */
export function Polyline2D__Duplicate(p) {
    return Polyline2D_$ctor_Z5FD8CF3C(getRange(p.points, 0, p.points.length));
}

/**
 * Creates a copy of the Polyline2D.
 * Same as polyline.Duplicate()
 */
export function Polyline2D__Clone(p) {
    return Polyline2D_$ctor_Z5FD8CF3C(getRange(p.points, 0, p.points.length));
}

/**
 * Sets the vertex at given index to the given point.
 * On a closed Polyline2D, setting the first or last point will set both to the same point.
 */
export function Polyline2D__SetVertex(p, idx, pt) {
    if ((idx < 0) ? true : (idx >= p.points.length)) {
        fail(`Polyline2D.SetVertex: index ${idx} is out of range for Polyline2D with ${p.points.length} points.`);
    }
    if ((idx === 0) && Polyline2D__get_IsClosed(p)) {
        setItem(p.points, p.points.length - 1, pt);
    }
    else if ((idx === (p.points.length - 1)) && Polyline2D__get_IsClosed(p)) {
        setItem(p.points, 0, pt);
    }
    setItem(p.points, idx, pt);
}

/**
 * Gets or sets first point of the Polyline2D
 * This is the point at index 0.
 * Same as Polyline2D.FirstPoint
 */
export function Polyline2D__get_Start(p) {
    if (p.points.length < 1) {
        failTooFewPoly2D("Start.get", 1, p.points.length);
    }
    return item(0, p.points);
}

/**
 * Gets or sets first point of the Polyline2D
 * This is the point at index 0.
 * Same as Polyline2D.FirstPoint
 */
export function Polyline2D__set_Start_6ADE94FD(p, v) {
    if (p.points.length < 1) {
        failTooFewPoly2D("Start.set", 1, p.points.length);
    }
    setItem(p.points, 0, v);
}

/**
 * Gets or sets last or end point of the Polyline2D
 * This is the point at index Points.Count - 1.
 * Same as Polyline2D.LastPoint
 */
export function Polyline2D__get_End(p) {
    if (p.points.length < 1) {
        failTooFewPoly2D("End.get", 1, p.points.length);
    }
    return item(p.points.length - 1, p.points);
}

/**
 * Gets or sets last or end point of the Polyline2D
 * This is the point at index Points.Count - 1.
 * Same as Polyline2D.LastPoint
 */
export function Polyline2D__set_End_6ADE94FD(p, v) {
    if (p.points.length < 1) {
        failTooFewPoly2D("End.set", 1, p.points.length);
    }
    setItem(p.points, p.points.length - 1, v);
}

/**
 * Gets or sets the last point of the Polyline2D.
 * This is the point at index Points.Count - 1.
 * Same as Polyline2D.End
 */
export function Polyline2D__get_LastPoint(p) {
    if (p.points.length < 1) {
        failTooFewPoly2D("LastPoint.get", 1, p.points.length);
    }
    return item(p.points.length - 1, p.points);
}

/**
 * Gets or sets the last point of the Polyline2D.
 * This is the point at index Points.Count - 1.
 * Same as Polyline2D.End
 */
export function Polyline2D__set_LastPoint_6ADE94FD(p, v) {
    if (p.points.length < 1) {
        failTooFewPoly2D("LastPoint.set", 1, p.points.length);
    }
    setItem(p.points, p.points.length - 1, v);
}

/**
 * Gets or sets the second last point of the Polyline2D.
 */
export function Polyline2D__get_SecondLastPoint(p) {
    if (p.points.length < 2) {
        failTooFewPoly2D("SecondLastPoint.get", 2, Polyline2D__get_PointCount(p));
    }
    return item(p.points.length - 2, p.points);
}

/**
 * Gets or sets the second last point of the Polyline2D.
 */
export function Polyline2D__set_SecondLastPoint_6ADE94FD(p, v) {
    if (p.points.length < 2) {
        failTooFewPoly2D("SecondLastPoint.set", 2, Polyline2D__get_PointCount(p));
    }
    setItem(p.points, p.points.length - 2, v);
}

/**
 * Gets or sets the second point of the Polyline2D.
 * This is the point at index 1.
 */
export function Polyline2D__get_SecondPoint(p) {
    if (p.points.length < 2) {
        failTooFewPoly2D("SecondPoint.get", 2, Polyline2D__get_PointCount(p));
    }
    return item(1, p.points);
}

/**
 * Gets or sets the second point of the Polyline2D.
 * This is the point at index 1.
 */
export function Polyline2D__set_SecondPoint_6ADE94FD(p, v) {
    if (p.points.length < 2) {
        failTooFewPoly2D("SecondPoint.set", 2, Polyline2D__get_PointCount(p));
    }
    setItem(p.points, 1, v);
}

/**
 * Gets or sets the first point of the Polyline2D.
 * This is the point at index 0.
 * Same as Polyline2D.Start
 */
export function Polyline2D__get_FirstPoint(p) {
    if (p.points.length < 1) {
        failTooFewPoly2D("FirstPoint.get", 1, Polyline2D__get_PointCount(p));
    }
    return item(0, p.points);
}

/**
 * Gets or sets the first point of the Polyline2D.
 * This is the point at index 0.
 * Same as Polyline2D.Start
 */
export function Polyline2D__set_FirstPoint_6ADE94FD(p, v) {
    if (p.points.length < 1) {
        failTooFewPoly2D("FirstPoint.set", 1, Polyline2D__get_PointCount(p));
    }
    setItem(p.points, 0, v);
}

/**
 * Gets the count of points in the Polyline2D
 */
export function Polyline2D__get_PointCount(p) {
    return p.points.length | 0;
}

/**
 * Gets the count of segments in the Polyline2D
 * This is poly.Points.Count - 1
 */
export function Polyline2D__get_SegmentCount(p) {
    return max(0, p.points.length - 1) | 0;
}

/**
 * Gets the index of the last point in the Polyline2D.
 * points.Count - 1
 */
export function Polyline2D__get_LastPointIndex(p) {
    return (p.points.length - 1) | 0;
}

/**
 * Gets the index of the last segment in the Polyline2D.
 * This is poly.Points.Count - 2
 */
export function Polyline2D__get_LastSegmentIndex(p) {
    return (p.points.length - 2) | 0;
}

/**
 * Gets the length of the Polyline2D
 * Returns 0.0 if there are less than 2 points.
 */
export function Polyline2D__get_Length(p) {
    let l = 0;
    if (p.points.length > 1) {
        let prev = item(0, p.points);
        for (let i = 1; i <= (p.points.length - 1); i++) {
            let a_1, b_1, vx, vy;
            const t = item(i, p.points);
            l = (l + ((a_1 = prev, (b_1 = t, (vx = (a_1.X - b_1.X), (vy = (a_1.Y - b_1.Y), Math.sqrt((vx * vx) + (vy * vy))))))));
            prev = t;
        }
    }
    return l;
}

/**
 * Gets the segment at index i of the Polyline2D.
 */
export function Polyline2D__GetSegment_Z524259A4(p, i) {
    if ((i < 0) ? true : (i > (p.points.length - 2))) {
        fail(`Polyline2D.GetSegment: index ${i} is out of range for Polyline2D with ${p.points.length} points.`);
    }
    return Line2D_$ctor_Z53905FA0(item(i, p.points), item(i + 1, p.points));
}

/**
 * Gets the segment at index i of the Polyline2D.
 */
export function Polyline2D__get_LastSegment(p) {
    if (p.points.length < 2) {
        failTooFewPoly2D("LastSegment", 2, Polyline2D__get_PointCount(p));
    }
    const i = (p.points.length - 1) | 0;
    return Line2D_$ctor_Z53905FA0(item(i - 1, p.points), item(i, p.points));
}

/**
 * Gets the first segment of the Polyline2D.
 */
export function Polyline2D__get_FirstSegment(p) {
    if (p.points.length < 2) {
        failTooFewPoly2D("FirstSegment", 2, Polyline2D__get_PointCount(p));
    }
    return Line2D_$ctor_Z53905FA0(item(0, p.points), item(1, p.points));
}

/**
 * Returns all segments of the Polyline2D as a list of Line2D.
 */
export function Polyline2D__get_Segments(p) {
    const lns = [];
    const pts = p.points;
    if (pts.length < 2) {
        return lns;
    }
    else {
        let a = item(0, pts);
        for (let i = 1; i <= (p.points.length - 1); i++) {
            const b = item(i, pts);
            void (lns.push(Line2D_$ctor_Z53905FA0(a, b)));
            a = b;
        }
        return lns;
    }
}

/**
 * Returns the line vectors of all segments of the Polyline2D as a list of Vc.
 */
export function Polyline2D__get_SegmentVectors(p) {
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
            void (vs.push((a_1 = b, (b_1 = a, Vc_$ctor_7B00E9A0(a_1.X - b_1.X, a_1.Y - b_1.Y)))));
            a = b;
        }
        return vs;
    }
}

/**
 * Gets bounding rectangle of the Polyline2D
 */
export function Polyline2D__get_BoundingRectangle(p) {
    const ps = p.points;
    if (Operators_IsNull(ps)) {
        failNull("BRect.createFromIList", "IList<Pt>");
    }
    if (count_1(ps) === 0) {
        failEmptySeq("BRect.createFromIList", "IList<Pt>");
    }
    let minX = 1.7976931348623157E+308;
    let minY = 1.7976931348623157E+308;
    let maxX = -1.7976931348623157E+308;
    let maxY = -1.7976931348623157E+308;
    for (let i = 0; i <= (count_1(ps) - 1); i++) {
        const p_1 = item(i, ps);
        minX = min(minX, p_1.X);
        minY = min(minY, p_1.Y);
        maxX = max(maxX, p_1.X);
        maxY = max(maxY, p_1.Y);
    }
    return BRect_$ctor_77D16AC0(minX, minY, maxX, maxY);
}

/**
 * Tests if Polyline2D start and end points are exactly the same.
 * Returns False if the Polyline2D has less than 3 points.
 */
export function Polyline2D__get_IsClosed(p) {
    if (p.points.length > 2) {
        let v;
        const a = Polyline2D__get_Start(p);
        const b = Polyline2D__get_End(p);
        v = Vc_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y);
        if (v.X === 0) {
            return v.Y === 0;
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
 * Tests if Polyline2D is closed within given tolerance.
 * Returns False if the Polyline2D has less than 3 points.
 */
export function Polyline2D__IsAlmostClosed_5E38073B(p, tolerance) {
    let a_1, b_1, vx, vy;
    if (p.points.length > 2) {
        return ((a_1 = Polyline2D__get_Start(p), (b_1 = Polyline2D__get_End(p), (vx = (a_1.X - b_1.X), (vy = (a_1.Y - b_1.Y), (vx * vx) + (vy * vy)))))) < (tolerance * tolerance);
    }
    else {
        return false;
    }
}

/**
 * Reverse order of the Polyline2D in place.
 */
export function Polyline2D__ReverseInPlace(p) {
    p.points.reverse();
}

/**
 * Returns new Polyline2D in reversed Order.
 */
export function Polyline2D__Reverse(p) {
    const n = Polyline2D__Duplicate(p);
    Polyline2D__get_Points(n).reverse();
    return n;
}

/**
 * Close the Polyline2D if it is not already closed.
 * If the ends are closer than the tolerance. The last point is set to equal the first point.
 * Else the start point is added to the end of the Polyline2D.
 */
export function Polyline2D__CloseInPlace_5E38073B(p, toleranceForAddingPoint) {
    let v_1, x, y;
    if (p.points.length < 3) {
        failTooFewPoly2D("CloseInPlace", 3, Polyline2D__get_PointCount(p));
    }
    let v;
    const a = Polyline2D__get_Start(p);
    const b = Polyline2D__get_End(p);
    v = Vc_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y);
    if (((v_1 = v, (x = v_1.X, (y = v_1.Y, (x * x) + (y * y))))) < (toleranceForAddingPoint * toleranceForAddingPoint)) {
        const this$ = p.points;
        const v_2 = Polyline2D__get_Start(p);
        if (this$.length === 0) {
            failRarr("Last.set", this$);
        }
        setItem(this$, this$.length - 1, v_2);
    }
    else {
        void (p.points.push(Polyline2D__get_Start(p)));
    }
}

/**
 * The signed area of the Polyline2D .
 * If it is positive the Polyline2D is Counter Clockwise.
 * Polyline does not need to be exactly closed. But then result might be wrong. Or without meaning.
 * For self intersecting Polylines the result is also invalid.
 */
export function Polyline2D__get_SignedArea(p) {
    let area = 0;
    let t;
    const this$ = p.points;
    if (this$.length === 0) {
        failRarr("Last.get", this$);
    }
    t = item(this$.length - 1, this$);
    for (let i = 0; i <= (p.points.length - 1); i++) {
        const n = item(i, p.points);
        area = (area + ((t.X - n.X) * (n.Y + t.Y)));
        t = n;
    }
    return area * 0.5;
}

/**
 * The area of the Polyline2D.
 * Fails if Polyline is not exactly closed.
 * For self intersecting Polylines the result is invalid.
 */
export function Polyline2D__get_Area(p) {
    if (!Polyline2D__get_IsClosed(p)) {
        fail(`Polyline2D.Area failed on Polyline2D that is not exactly closed ${p}`);
    }
    return Math.abs(Polyline2D__get_SignedArea(p));
}

/**
 * Test if Polyline2D is CounterClockwise.
 * The Polyline2D does not need to be actually closed.
 * The signed area of the Polyline2D is calculated.
 * If it is positive the Polyline2D is Counter Clockwise.
 */
export function Polyline2D__get_IsCounterClockwise(p) {
    const area = Polyline2D__get_SignedArea(p);
    if (Math.abs(area) < 1E-12) {
        fail(`Polyline2D.IsCounterClockwise: Polyline2D the area is zero: ${p}`);
    }
    return area > 0;
}

/**
 * Test if Polyline2D is Clockwise.
 * The Polyline2D does not need to be actually closed.
 * The signed area of the Polyline2D is calculated.
 * If it is negative the Polyline2D is Clockwise.
 */
export function Polyline2D__get_IsClockwise(p) {
    const area = Polyline2D__get_SignedArea(p);
    if (Math.abs(area) < 1E-12) {
        fail(`Polyline2D.IsClockwise: Polyline2D the area is zero: ${p}`);
    }
    return area < 0;
}

/**
 * Returns the point at a given parameter on the Polyline2D.
 * The integer part of the parameter is the index of the segment that the point is on.
 * The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
 * The domain Polyline2D starts at 0.0 and ends at points.Count - 1.0 .
 * If the parameter is within 1e-6 of an integer value, the integer value is used as parameter.
 */
export function Polyline2D__EvaluateAt_5E38073B(pl, t) {
    const i = ~~t | 0;
    const p = t - i;
    const count = Polyline2D__get_Points(pl).length | 0;
    const countF = count;
    if (t < 1E-06) {
        if (t < -1E-06) {
            fail(`Polyline2D.EvaluateAt: Parameter ${t} is less than 0.0`);
        }
        const this$ = Polyline2D__get_Points(pl);
        if (this$.length === 0) {
            failRarr("First.get", this$);
        }
        return item(0, this$);
    }
    else if (t > (countF - 1E-06)) {
        if (t > (countF + 1E-06)) {
            fail(`Polyline2D.EvaluateAt: Parameter ${t} is more than point count ${Polyline2D__get_Points(pl).length}.`);
        }
        const this$_1 = Polyline2D__get_Points(pl);
        if (this$_1.length === 0) {
            failRarr("Last.get", this$_1);
        }
        return item(this$_1.length - 1, this$_1);
    }
    else if (p < 1E-06) {
        return item(i, Polyline2D__get_Points(pl));
    }
    else if (p > (1 - 1E-06)) {
        return item(i + 1, Polyline2D__get_Points(pl));
    }
    else {
        const t_1 = item(i, Polyline2D__get_Points(pl));
        let v;
        const a = item(i + 1, Polyline2D__get_Points(pl));
        const b = t_1;
        v = Vc_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y);
        const p_1 = t_1;
        let v_2;
        const v_1 = v;
        const f = p;
        v_2 = Vc_$ctor_7B00E9A0(v_1.X * f, v_1.Y * f);
        return Pt_$ctor_7B00E9A0(p_1.X + v_2.X, p_1.Y + v_2.Y);
    }
}

export function Polyline2D__TangentAt_5E38073B(pl, _t) {
    return fail("Polyline2D.TangentAt is obsolete, use GetSegment(i).UnitTangent instead.");
}

/**
 * Returns the parameter on the Polyline2D that is the closest point to the given point.
 * The integer part of the parameter is the index of the segment that the point is on.
 * The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
 * The domain Polyline2D starts at 0.0 and ends at points.Count - 1.0 .
 */
export function Polyline2D__ClosestParameter_6ADE94FD(pl, p) {
    const pts = Polyline2D__get_Points(pl);
    if (pts.length === 0) {
        fail("Polyline2D.ClosestParameter failed on empty Polyline2D");
    }
    let a = item(0, pts);
    let minT = 0;
    let seg = 0;
    let minDistSq;
    const a_2 = a;
    const b_1 = p;
    const vx = a_2.X - b_1.X;
    const vy = a_2.Y - b_1.Y;
    minDistSq = ((vx * vx) + (vy * vy));
    for (let i = 1; i <= (pts.length - 1); i++) {
        const b_2 = item(i, pts);
        const dx = b_2.X - a.X;
        const dy = b_2.Y - a.Y;
        if ((dx !== 0) ? true : (dy !== 0)) {
            const t = (((p.X - a.X) * dx) + ((p.Y - a.Y) * dy)) / ((dx * dx) + (dy * dy));
            const t$0027 = max(0, min(1, t));
            const projX = a.X + (dx * t$0027);
            const projY = a.Y + (dy * t$0027);
            const dpx = p.X - projX;
            const dpy = p.Y - projY;
            const distSq = (dpx * dpx) + (dpy * dpy);
            if (distSq < minDistSq) {
                minDistSq = distSq;
                minT = t$0027;
                seg = ((i - 1) | 0);
            }
        }
        else {
            const dpx_1 = p.X - a.X;
            const dpy_1 = p.Y - a.Y;
            const distSq_1 = (dpx_1 * dpx_1) + (dpy_1 * dpy_1);
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
 * Returns the point on the Polyline2D that is the closest point to the given point.
 * This might be a point on a segment or a vertex point.
 */
export function Polyline2D__ClosestPoint_6ADE94FD(pl, p) {
    const pts = Polyline2D__get_Points(pl);
    if (pts.length === 0) {
        fail("Polyline2D.ClosestPoint failed on empty Polyline2D");
    }
    let a = item(0, pts);
    let minPt = a;
    let minDistSq;
    const a_2 = a;
    const b_1 = p;
    const vx = a_2.X - b_1.X;
    const vy = a_2.Y - b_1.Y;
    minDistSq = ((vx * vx) + (vy * vy));
    for (let i = 1; i <= (pts.length - 1); i++) {
        const b_2 = item(i, pts);
        const dx = b_2.X - a.X;
        const dy = b_2.Y - a.Y;
        if ((dx !== 0) ? true : (dy !== 0)) {
            const t = (((p.X - a.X) * dx) + ((p.Y - a.Y) * dy)) / ((dx * dx) + (dy * dy));
            const t$0027 = max(0, min(1, t));
            const projX = a.X + (dx * t$0027);
            const projY = a.Y + (dy * t$0027);
            const dpx = p.X - projX;
            const dpy = p.Y - projY;
            const distSq = (dpx * dpx) + (dpy * dpy);
            if (distSq < minDistSq) {
                minDistSq = distSq;
                minPt = Pt_$ctor_7B00E9A0(projX, projY);
            }
        }
        else {
            const dpx_1 = p.X - a.X;
            const dpy_1 = p.Y - a.Y;
            const distSq_1 = (dpx_1 * dpx_1) + (dpy_1 * dpy_1);
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
export function Polyline2D__ClosestVertex_6ADE94FD(pl, p) {
    const pts = Polyline2D__get_Points(pl);
    if (pts.length === 0) {
        fail("Polyline2D.ClosestVertex failed on empty Polyline2D");
    }
    let minIdx = 0;
    let minDistSq;
    const a_1 = item(0, pts);
    const b_1 = p;
    const vx = a_1.X - b_1.X;
    const vy = a_1.Y - b_1.Y;
    minDistSq = ((vx * vx) + (vy * vy));
    for (let i = 1; i <= (pts.length - 1); i++) {
        let dSq;
        const a_3 = item(i, pts);
        const b_3 = p;
        const vx_1 = a_3.X - b_3.X;
        const vy_1 = a_3.Y - b_3.Y;
        dSq = ((vx_1 * vx_1) + (vy_1 * vy_1));
        if (dSq < minDistSq) {
            minDistSq = dSq;
            minIdx = (i | 0);
        }
    }
    return minIdx | 0;
}

/**
 * Returns the distance of the test point to the closest point on the Polyline2D.
 */
export function Polyline2D__DistanceTo_6ADE94FD(pl, p) {
    const pts = Polyline2D__get_Points(pl);
    if (pts.length === 0) {
        fail("Polyline2D.DistanceTo failed on empty Polyline2D");
    }
    let a = item(0, pts);
    let minDistSq;
    const a_2 = a;
    const b_1 = p;
    const vx = a_2.X - b_1.X;
    const vy = a_2.Y - b_1.Y;
    minDistSq = ((vx * vx) + (vy * vy));
    for (let i = 1; i <= (pts.length - 1); i++) {
        const b_2 = item(i, pts);
        const dx = b_2.X - a.X;
        const dy = b_2.Y - a.Y;
        if ((dx !== 0) ? true : (dy !== 0)) {
            const t = (((p.X - a.X) * dx) + ((p.Y - a.Y) * dy)) / ((dx * dx) + (dy * dy));
            const t$0027 = max(0, min(1, t));
            const projX = a.X + (dx * t$0027);
            const projY = a.Y + (dy * t$0027);
            const dpx = p.X - projX;
            const dpy = p.Y - projY;
            const distSq = (dpx * dpx) + (dpy * dpy);
            if (distSq < minDistSq) {
                minDistSq = distSq;
            }
        }
        else {
            const dpx_1 = p.X - a.X;
            const dpy_1 = p.Y - a.Y;
            const distSq_1 = (dpx_1 * dpx_1) + (dpy_1 * dpy_1);
            if (distSq_1 < minDistSq) {
                minDistSq = distSq_1;
            }
        }
        a = b_2;
    }
    return Math.sqrt(minDistSq);
}

/**
 * Count how many times the polygon winds around the point.
 * If the result is 0 then the point is outside of the Polyline2D.
 * A non-zero value indicates the point is inside.
 */
export function Polyline2D__WindingNumber_6ADE94FD(pl, point) {
    const px = point.X;
    const py = point.Y;
    let winding = 0;
    const pts = Polyline2D__get_Points(pl);
    if (pts.length > 0) {
        let this$ = item(0, pts);
        for (let i = 1; i <= (pts.length - 1); i++) {
            let aa, bb, ax, ay, bx, by, det, aa_1, bb_1, ax_1, ay_1, bx_1, by_1, det_1;
            const next = item(i, pts);
            if (this$.Y <= py) {
                if ((next.Y > py) && (((aa = this$, (bb = next, (ax = (px - aa.X), (ay = (py - aa.Y), (bx = (bb.X - aa.X), (by = (bb.Y - aa.Y), (det = ((ax * by) - (ay * bx)), (det > 0) ? 1 : ((det < 0) ? -1 : 0))))))))) > 0)) {
                    winding = ((winding - 1) | 0);
                }
            }
            else if ((next.Y <= py) && (((aa_1 = this$, (bb_1 = next, (ax_1 = (px - aa_1.X), (ay_1 = (py - aa_1.Y), (bx_1 = (bb_1.X - aa_1.X), (by_1 = (bb_1.Y - aa_1.Y), (det_1 = ((ax_1 * by_1) - (ay_1 * bx_1)), (det_1 > 0) ? 1 : ((det_1 < 0) ? -1 : 0))))))))) < 0)) {
                winding = ((winding + 1) | 0);
            }
            this$ = next;
        }
    }
    return winding | 0;
}

/**
 * Tests if a point is inside the closed Polyline2D using the ray casting algorithm.
 */
export function Polyline2D__Contains_6ADE94FD(p, pt) {
    const pts = Polyline2D__get_Points(p);
    if (pts.length < 3) {
        return false;
    }
    else {
        let inside = false;
        let pi = item(0, pts);
        const y = pt.Y;
        const x = pt.X;
        for (let i = 1; i <= (pts.length - 1); i++) {
            const pj = item(i, pts);
            if (((pi.Y > y) !== (pj.Y > y)) && (x < ((((pj.X - pi.X) * (y - pi.Y)) / (pj.Y - pi.Y)) + pi.X))) {
                inside = !inside;
            }
            pi = pj;
        }
        return inside;
    }
}

/**
 * Calculates the shortest distance from the test point to the polyline with `DistanceTo`,
 * then signs that value by testing containment via the ray-casting based `Contains` helper.
 * Returns a positive distance for points that lie inside the polyline boundary and negative otherwise.
 * For reliable results the polyline should be closed and have identical first and last vertices.
 */
export function Polyline2D__SignedDistanceTo_6ADE94FD(pl, point) {
    const distance = Polyline2D__DistanceTo_6ADE94FD(pl, point);
    if (Polyline2D__Contains_6ADE94FD(pl, point)) {
        return distance;
    }
    else {
        return -distance;
    }
}

/**
 * Returns the average center of all points of the Polyline2D.
 */
export function Polyline2D__get_Center(p) {
    if (p.points.length === 0) {
        failTooFewPoly2D("Center", 1, Polyline2D__get_PointCount(p));
    }
    let x = 0;
    let y = 0;
    for (let i = 0; i <= (p.points.length - 1); i++) {
        const p_1 = item(i, p.points);
        x = (x + p_1.X);
        y = (y + p_1.Y);
    }
    return Pt_$ctor_7B00E9A0(x / p.points.length, y / p.points.length);
}

/**
 * Scales the 2D polyline by a given factor.
 * Scale center is World Origin 0,0
 */
export function Polyline2D__Scale_5E38073B(p, factor) {
    return Polyline2D_$ctor_Z5FD8CF3C(ResizeArr_map((pt) => {
        const a = pt;
        const f = factor;
        return Pt_$ctor_7B00E9A0(a.X * f, a.Y * f);
    }, p.points));
}

/**
 * Scales the 2D polyline by a given factor on a given center point.
 */
export function Polyline2D__ScaleOn(p, cen, factor) {
    const cx = cen.X;
    const cy = cen.Y;
    return Polyline2D_$ctor_Z5FD8CF3C(ResizeArr_map((pt) => Pt_$ctor_7B00E9A0(cx + ((pt.X - cx) * factor), cy + ((pt.Y - cy) * factor)), p.points));
}

/**
 * Finds a point inside a closed Polyline2D that is the farthest away from the edges of the Polyline2D.
 * Uses the Polylabel algorithm from Mapbox. It is a highly optimized algorithm specifically designed to find the
 * pole of inaccessibility for polygons. The point within the polygon that is farthest from the edges,
 * often used for optimal label placement.
 * Adaptive Precision: Can trade accuracy for speed based on your needs.
 * Returns the best point and its distance to the polygon edge. Supplying an open polyline is allowed,
 * but the computed "inside" still assumes the points describe a closed boundary (first and last vertices should match).
 */
export function Polyline2D__FindLablePoint_5E38073B(pl, precision) {
    if (Polyline2D__get_PointCount(pl) < 1) {
        fail(`Polyline2D.FindLablePoint must have at least 1 point but has ${Polyline2D__get_PointCount(pl)} points.`);
    }
    const bRect = Polyline2D__get_BoundingRectangle(pl);
    const minX = bRect.MinX;
    const minY = bRect.MinY;
    const maxX = bRect.MaxX;
    const maxY = bRect.MaxY;
    const width = maxX - minX;
    const height = maxY - minY;
    if ((width < precision) ? true : (height < precision)) {
        const c = Polyline2D__get_Center(pl);
        const dist = Polyline2D__SignedDistanceTo_6ADE94FD(pl, c);
        return [c, dist];
    }
    else {
        const cellSize = min(width, height);
        const halfCell = cellSize * 0.5;
        const heap = CellHeap_$ctor();
        let y = minY;
        while (y < maxY) {
            let x = minX;
            while (x < maxX) {
                let cell;
                const point = Pt_$ctor_7B00E9A0(x + halfCell, y + halfCell);
                const h_1 = halfCell;
                const distance = Polyline2D__SignedDistanceTo_6ADE94FD(pl, point);
                const maxDistance = distance + (h_1 * 1.4142135623730951);
                cell = (new Cell(point.X, point.Y, h_1, distance, maxDistance));
                CellHeap__Add_3F6CBBFD(heap, cell);
                x = (x + cellSize);
            }
            y = (y + cellSize);
        }
        let bestCell;
        const point_1 = Polyline2D__get_Center(pl);
        const distance_1 = Polyline2D__SignedDistanceTo_6ADE94FD(pl, point_1);
        const maxDistance_1 = distance_1 + (0 * 1.4142135623730951);
        bestCell = (new Cell(point_1.X, point_1.Y, 0, distance_1, maxDistance_1));
        let bboxCell;
        let point_2;
        const r = bRect;
        point_2 = Pt_$ctor_7B00E9A0((r.MaxX + r.MinX) * 0.5, (r.MaxY + r.MinY) * 0.5);
        const distance_2 = Polyline2D__SignedDistanceTo_6ADE94FD(pl, point_2);
        const maxDistance_2 = distance_2 + (0 * 1.4142135623730951);
        bboxCell = (new Cell(point_2.X, point_2.Y, 0, distance_2, maxDistance_2));
        if (bboxCell.Distance > bestCell.Distance) {
            bestCell = bboxCell;
        }
        while (CellHeap__get_Count(heap) > 0) {
            const cell_1 = CellHeap__Pop(heap);
            if ((cell_1.MaxDistance - bestCell.Distance) <= precision) {
            }
            else if (cell_1.H <= precision) {
                if (cell_1.Distance > bestCell.Distance) {
                    bestCell = cell_1;
                }
            }
            else {
                const h_4 = cell_1.H * 0.5;
                let c_1;
                const point_3 = Pt_$ctor_7B00E9A0(cell_1.X - h_4, cell_1.Y - h_4);
                const h_6 = h_4;
                const distance_3 = Polyline2D__SignedDistanceTo_6ADE94FD(pl, point_3);
                const maxDistance_3 = distance_3 + (h_6 * 1.4142135623730951);
                c_1 = (new Cell(point_3.X, point_3.Y, h_6, distance_3, maxDistance_3));
                if ((c_1.MaxDistance - bestCell.Distance) > precision) {
                    CellHeap__Add_3F6CBBFD(heap, c_1);
                }
                if (c_1.Distance > bestCell.Distance) {
                    bestCell = c_1;
                }
                let c_2;
                const point_4 = Pt_$ctor_7B00E9A0(cell_1.X + h_4, cell_1.Y - h_4);
                const h_8 = h_4;
                const distance_4 = Polyline2D__SignedDistanceTo_6ADE94FD(pl, point_4);
                const maxDistance_4 = distance_4 + (h_8 * 1.4142135623730951);
                c_2 = (new Cell(point_4.X, point_4.Y, h_8, distance_4, maxDistance_4));
                if ((c_2.MaxDistance - bestCell.Distance) > precision) {
                    CellHeap__Add_3F6CBBFD(heap, c_2);
                }
                if (c_2.Distance > bestCell.Distance) {
                    bestCell = c_2;
                }
                let c_3;
                const point_5 = Pt_$ctor_7B00E9A0(cell_1.X - h_4, cell_1.Y + h_4);
                const h_10 = h_4;
                const distance_5 = Polyline2D__SignedDistanceTo_6ADE94FD(pl, point_5);
                const maxDistance_5 = distance_5 + (h_10 * 1.4142135623730951);
                c_3 = (new Cell(point_5.X, point_5.Y, h_10, distance_5, maxDistance_5));
                if ((c_3.MaxDistance - bestCell.Distance) > precision) {
                    CellHeap__Add_3F6CBBFD(heap, c_3);
                }
                if (c_3.Distance > bestCell.Distance) {
                    bestCell = c_3;
                }
                let c_4;
                const point_6 = Pt_$ctor_7B00E9A0(cell_1.X + h_4, cell_1.Y + h_4);
                const h_12 = h_4;
                const distance_6 = Polyline2D__SignedDistanceTo_6ADE94FD(pl, point_6);
                const maxDistance_6 = distance_6 + (h_12 * 1.4142135623730951);
                c_4 = (new Cell(point_6.X, point_6.Y, h_12, distance_6, maxDistance_6));
                if ((c_4.MaxDistance - bestCell.Distance) > precision) {
                    CellHeap__Add_3F6CBBFD(heap, c_4);
                }
                if (c_4.Distance > bestCell.Distance) {
                    bestCell = c_4;
                }
            }
        }
        return [Pt_$ctor_7B00E9A0(bestCell.X, bestCell.Y), bestCell.Distance];
    }
}

/**
 * Finds a point inside a closed Polyline2D that is the farthest away from the edges of the Polyline2D.
 * Uses the Polylabel algorithm from Mapbox. It is a highly optimized algorithm specifically designed to find the
 * pole of inaccessibility for polygons. The point within the polygon that is farthest from the edges,
 * often used for optimal label placement.
 * Adaptive Precision: Can trade accuracy for speed based on your needs.
 * Returns the best point and its distance to the polygon edge. Supplying an open polyline is allowed,
 * but the computed "inside" still assumes the points describe a closed boundary (first and last vertices should match).
 */
export function Polyline2D_findLablePoint(precision, pl) {
    return Polyline2D__FindLablePoint_5E38073B(pl, precision);
}

/**
 * Gets the internal list of all Points of the Polyline2D.
 * This is not a copy, so changes to the list will be reflected in the Polyline2D.
 */
export function Polyline2D_pointsUnsafeInternal_Z5A89AEF5(p) {
    return Polyline2D__get_Points(p);
}

/**
 * Gets first point of the Polyline2D
 */
export function Polyline2D_start_Z5A89AEF5(p) {
    const points = Polyline2D__get_Points(p);
    if (points.length < 1) {
        failTooFewPoly2D("start", 1, Polyline2D__get_PointCount(p));
    }
    return item(0, points);
}

/**
 * Gets last or end point of the Polyline2D
 */
export function Polyline2D_ende_Z5A89AEF5(p) {
    const points = Polyline2D__get_Points(p);
    if (points.length < 1) {
        failTooFewPoly2D("ende", 1, Polyline2D__get_PointCount(p));
    }
    return item(points.length - 1, points);
}

/**
 * Reverse order of the Polyline2D in place.
 */
export function Polyline2D_reverseInPlace_Z5A89AEF5(p) {
    Polyline2D__ReverseInPlace(p);
}

/**
 * Returns new Polyline2D in reversed Order.
 */
export function Polyline2D_reverse_Z5A89AEF5(p) {
    return Polyline2D__Reverse(p);
}

/**
 * Returns the point at a given parameter on the Polyline2D.
 * The integer part of the parameter is the index of the segment that the point is on.
 * The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
 * The domain Polyline2D starts at 0.0 and ends at point count.
 */
export function Polyline2D_evaluateAt(t, pl) {
    return Polyline2D__EvaluateAt_5E38073B(pl, t);
}

/**
 * Apply a mapping function to each point in the 2D Polyline2D. Returns new Polyline2D.
 */
export function Polyline2D_map(mapping, pl) {
    return Polyline2D_$ctor_Z5FD8CF3C(ResizeArr_map(mapping, Polyline2D__get_Points(pl)));
}

/**
 * Move a Polyline2D by a vector. (same as Polyline2D.move)
 */
export function Polyline2D_translate(v, pl) {
    return Polyline2D_map((a) => {
        const p = a;
        const v_3 = v;
        return Pt_$ctor_7B00E9A0(p.X + v_3.X, p.Y + v_3.Y);
    }, pl);
}

/**
 * Move a Polyline2D by a vector. (same as Polyline2D.translate)
 */
export function Polyline2D_move(v, pl) {
    return Polyline2D_translate(v, pl);
}

/**
 * Returns a Polyline2D moved by a given distance in X direction.
 */
export function Polyline2D_moveX(distance, pl) {
    return Polyline2D_map((pt) => {
        const pt_1 = pt;
        return Pt_$ctor_7B00E9A0_1(pt_1.X + distance, pt_1.Y);
    }, pl);
}

/**
 * Returns a Polyline2D moved by a given distance in Y direction.
 */
export function Polyline2D_moveY(distance, pl) {
    return Polyline2D_map((pt) => {
        const pt_1 = pt;
        return Pt_$ctor_7B00E9A0_1(pt_1.X, pt_1.Y + distance);
    }, pl);
}

/**
 * Scales the Polyline2D by a given factor.
 * Scale center is World Origin 0,0
 * Returns a new Polyline2D.
 */
export function Polyline2D_scale(factor, pl) {
    return Polyline2D_map((pt) => {
        const a = pt;
        const f = factor;
        return Pt_$ctor_7B00E9A0(a.X * f, a.Y * f);
    }, pl);
}

/**
 * Rotation a Polyline2D around Z-Axis.
 */
export function Polyline2D_rotate(r, pl) {
    return Polyline2D_map((p) => {
        const r_2 = r;
        const p_1 = p;
        return Pt_$ctor_7B00E9A0_1((r_2.Cos * p_1.X) - (r_2.Sin * p_1.Y), (r_2.Sin * p_1.X) + (r_2.Cos * p_1.Y));
    }, pl);
}

/**
 * Rotation a Polyline2D round given Center point an a local Z-axis.
 */
export function Polyline2D_rotateWithCenter(cen, r, pl) {
    return Polyline2D_map((pt) => {
        const cen_2 = cen;
        const r_2 = r;
        const pt_1 = pt;
        const x = pt_1.X - cen_2.X;
        const y = pt_1.Y - cen_2.Y;
        return Pt_$ctor_7B00E9A0_1(((r_2.Cos * x) - (r_2.Sin * y)) + cen_2.X, ((r_2.Sin * x) + (r_2.Cos * y)) + cen_2.Y);
    }, pl);
}

/**
 * Create a new Polyline2D by using the provided ResizeArray directly.
 * Unsafe because all later changes to the ResizeArray will be reflected in the Polyline2D.
 */
export function Polyline2D_createDirectlyUnsafe_Z5FD8CF3C(points) {
    return Polyline2D_$ctor_Z5FD8CF3C(points);
}

/**
 * Returns new Polyline2D from point at Parameter a to point at Parameter b.
 * if 'a' is bigger 'b' then the new Polyline2D is in opposite direction.
 * If a parameter is within 1e-4 of an integer value, the integer value is used as parameter.
 */
export function Polyline2D_subPolyline(a, b, pl) {
    const rev = a > b;
    const patternInput = rev ? [b, a] : [a, b];
    const v = patternInput[1];
    const u = patternInput[0];
    let np;
    const capacity = (~~(v - u) + 2) | 0;
    np = Polyline2D_$ctor_Z5FD8CF3C([]);
    const nps = Polyline2D__get_Points(np);
    const ps = Polyline2D__get_Points(pl);
    const ui = ~~u | 0;
    const uf = u - ui;
    if (uf < 0.9999) {
        void (nps.push(Polyline2D__EvaluateAt_5E38073B(pl, u)));
    }
    for (let i = ~~u + 1; i <= ~~v; i++) {
        if ((i >= 0) && (i < ps.length)) {
            void (nps.push(item(i, ps)));
        }
    }
    const vi = ~~v | 0;
    const vf = v - vi;
    if (vf > 0.0001) {
        void (nps.push(Polyline2D__EvaluateAt_5E38073B(pl, v)));
    }
    if (rev) {
        Polyline2D__ReverseInPlace(np);
    }
    return np;
}

/**
 * Returns a new closed Polyline2D.
 * If the first and last point are within 1e-6 of each other, the last point is set equal to the first point.
 * Otherwise one point is added.
 */
export function Polyline2D_close_Z5A89AEF5(pl) {
    let a_1, this$, b_1, this$_1, vx, vy, this$_2, this$_3;
    if (Polyline2D__get_Points(pl).length < 2) {
        failTooFewPoly2D("close", 2, Polyline2D__get_PointCount(pl));
    }
    const ps = Polyline2D__get_Points(pl);
    let np;
    const capacity = (ps.length + 1) | 0;
    np = Polyline2D_$ctor_Z5FD8CF3C([]);
    addRangeInPlace(getRange(ps, 0, ps.length), Polyline2D__get_Points(np));
    if (((a_1 = ((this$ = ps, ((this$.length === 0) ? failRarr("First.get", this$) : undefined, item(0, this$)))), (b_1 = ((this$_1 = ps, ((this$_1.length === 0) ? failRarr("Last.get", this$_1) : undefined, item(this$_1.length - 1, this$_1)))), (vx = (a_1.X - b_1.X), (vy = (a_1.Y - b_1.Y), (vx * vx) + (vy * vy)))))) < 1E-12) {
        setItem(Polyline2D__get_Points(np), Polyline2D__get_Points(np).length - 1, (this$_2 = Polyline2D__get_Points(np), ((this$_2.length === 0) ? failRarr("First.get", this$_2) : undefined, item(0, this$_2))));
    }
    else {
        void (Polyline2D__get_Points(np).push((this$_3 = Polyline2D__get_Points(np), ((this$_3.length === 0) ? failRarr("First.get", this$_3) : undefined, item(0, this$_3)))));
    }
    return np;
}

/**
 * Closes the Polyline2D in place by adding a point.
 * If the first and last point are within 1e-6 of each other, the last point is set equal to the first point instead.
 */
export function Polyline2D_closeInPlace_Z5A89AEF5(pl) {
    let a_1, this$, b_1, this$_1, vx, vy, this$_2, this$_3;
    if (Polyline2D__get_Points(pl).length < 2) {
        failTooFewPoly2D("closeInPlace", 2, Polyline2D__get_PointCount(pl));
    }
    const points = Polyline2D__get_Points(pl);
    if (((a_1 = ((this$ = points, ((this$.length === 0) ? failRarr("First.get", this$) : undefined, item(0, this$)))), (b_1 = ((this$_1 = points, ((this$_1.length === 0) ? failRarr("Last.get", this$_1) : undefined, item(this$_1.length - 1, this$_1)))), (vx = (a_1.X - b_1.X), (vy = (a_1.Y - b_1.Y), (vx * vx) + (vy * vy)))))) < 1E-12) {
        setItem(points, points.length - 1, (this$_2 = points, ((this$_2.length === 0) ? failRarr("First.get", this$_2) : undefined, item(0, this$_2))));
    }
    else {
        void (points.push((this$_3 = points, ((this$_3.length === 0) ? failRarr("First.get", this$_3) : undefined, item(0, this$_3)))));
    }
}

/**
 * Tests if two Polyline2D have the same number of points and points are equal within a given tolerance.
 */
export function Polyline2D_equals(tol, a, b) {
    const k = Polyline2D__get_PointCount(a) | 0;
    if (k !== Polyline2D__get_PointCount(b)) {
        return false;
    }
    else {
        let i = 0;
        let same = true;
        const aPts = Polyline2D__get_Points(a);
        const bPts = Polyline2D__get_Points(b);
        while ((i < k) && same) {
            let tol_2, a_2, b_2;
            if ((tol_2 = tol, (a_2 = item(i, aPts), (b_2 = item(i, bPts), (Math.abs(a_2.X - b_2.X) <= tol_2) && (Math.abs(a_2.Y - b_2.Y) <= tol_2))))) {
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
 * Removes consecutive duplicate points from the Polyline2D within a given tolerance.
 * This algorithm allows the last and first point to be identical if the Polyline2D is closed.
 */
export function Polyline2D_removeDuplicatePoints(distanceTolerance, pl) {
    const pts = Polyline2D__get_Points(pl);
    if (pts.length < 2) {
        return pl;
    }
    else {
        const nps = [];
        let prev = item(0, pts);
        void (nps.push(prev));
        for (let i = 1; i <= (pts.length - 1); i++) {
            let tol_1, a_1, b_1;
            const p = item(i, pts);
            if (!((tol_1 = distanceTolerance, (a_1 = prev, (b_1 = p, (Math.abs(a_1.X - b_1.X) <= tol_1) && (Math.abs(a_1.Y - b_1.Y) <= tol_1)))))) {
                void (nps.push(p));
                prev = p;
            }
        }
        return Polyline2D_createDirectlyUnsafe_Z5FD8CF3C(nps);
    }
}

/**
 * Removes consecutive duplicate points and colinear points from the Polyline2D within given tolerances.
 * This algorithm allows the last and first point to be identical if the Polyline2D is closed.
 * Colinear points are removed when the angle between segments is smaller than the cosine threshold (e.g. cosine of 0.5 degrees ).
 * If the Polyline2D is closed and starts and ends with colinear segments, the first point is replaced with the last non-colinear point.
 * So the joint of the loop is now moved to the last non-colinear point.
 * So that there are no colinear segments even between start and end.
 */
export function Polyline2D_removeColinearAndDuplicatePoints(angleTolerance, distanceTolerance, pl) {
    let this$_2, this$_3;
    if (angleTolerance < 0.7071067811865476) {
        fail(`Polyline2D.removeColinearAndDuplicatePoints: angleTolerance must be at least Cosine.\`\`45.0\`\` ( that is 0.707) but was ${angleTolerance} (= ${Math.acos(angleTolerance)} degrees).`);
    }
    if (angleTolerance > 0.9999999847691291) {
        fail(`Polyline2D.removeColinearAndDuplicatePoints: angleTolerance must be at most Cosine.\`\`0.01\`\` ( that is 0.999999984) but was ${angleTolerance} (= ${Math.acos(angleTolerance)} degrees).`);
    }
    const pts = Polyline2D__get_Points(pl);
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
        const vx = a_1.X - b_1.X;
        const vy = a_1.Y - b_1.Y;
        len = Math.sqrt((vx * vx) + (vy * vy));
        while ((len < distanceTolerance) && (i < lastIdx)) {
            let a_3, b_3, vx_1, vy_1;
            i = ((i + 1) | 0);
            this$_1 = item(i, pts);
            len = ((a_3 = prev, (b_3 = this$_1, (vx_1 = (a_3.X - b_3.X), (vy_1 = (a_3.Y - b_3.Y), Math.sqrt((vx_1 * vx_1) + (vy_1 * vy_1)))))));
        }
        let firstVec;
        const fromPt = prev;
        const toPt = this$_1;
        const x = toPt.X - fromPt.X;
        const y = toPt.Y - fromPt.Y;
        const l = Math.sqrt((x * x) + (y * y));
        if (!(l > 1E-12)) {
            failTooClose("UnitVc.create", fromPt, toPt);
        }
        firstVec = UnitVc_$ctor_7B00E9A0(x / l, y / l);
        let vPrev = firstVec;
        for (let idx = i + 1; idx <= lastIdx; idx++) {
            const next = item(idx, pts);
            const vx_2 = next.X - this$_1.X;
            const vy_2 = next.Y - this$_1.Y;
            let len_1;
            const value = (vx_2 * vx_2) + (vy_2 * vy_2);
            len_1 = Math.sqrt(value);
            if (len_1 > distanceTolerance) {
                const vNext = UnitVc_$ctor_7B00E9A0(vx_2 / len_1, vy_2 / len_1);
                let cos;
                const a_4 = vPrev;
                const b_4 = vNext;
                cos = ((a_4.X * b_4.X) + (a_4.Y * b_4.Y));
                if (cos < angleTolerance) {
                    void (nps.push(this$_1));
                    prev = this$_1;
                    vPrev = vNext;
                }
                this$_1 = next;
            }
        }
        if (Polyline2D__IsAlmostClosed_5E38073B(pl, distanceTolerance)) {
            let cos_1;
            const a_5 = vPrev;
            const b_5 = firstVec;
            cos_1 = ((a_5.X * b_5.X) + (a_5.Y * b_5.Y));
            if (cos_1 < angleTolerance) {
                void (nps.push((this$_2 = pts, ((this$_2.length === 0) ? failRarr("Last.get", this$_2) : undefined, item(this$_2.length - 1, this$_2)))));
            }
            else {
                setItem(nps, 0, (this$_3 = nps, ((this$_3.length === 0) ? failRarr("Last.get", this$_3) : undefined, item(this$_3.length - 1, this$_3))));
            }
        }
        else if (Euclid_Pt__Pt_notEquals_Static(distanceTolerance, this$_1, prev)) {
            void (nps.push(this$_1));
        }
        return Polyline2D_createDirectlyUnsafe_Z5FD8CF3C(nps);
    }
}

/**
 * Offsets a Polyline in 2D space by finding the local offset in each corner.
 * Auto detects if given points are from a closed Polyline (first point = last point) and loops them.
 * By default this function raises an Exception on duplicate points, 180 degree U-Turns.
 * But this can be configured with optional parameters.
 */
export function Polyline2D_offset_Z45C468A5(polyLine, constantOffsetDistance, loop = false, checkOrientation = true, uTurnBehavior = 1, useUTurnBehaviorAbove = -0.9961946980917455) {
    let a_1, this$, b_1, this$_1, vx, vy, this$_2, i, v;
    const pts = Polyline2D__get_Points(polyLine);
    if (pts.length < 2) {
        fail(`Polyline2D.offset: Polyline2D must have at least 2 points but has ${pts.length} points. ${polyLine}`);
    }
    const constantOffsetDistance_1 = (checkOrientation && (Polyline2D__get_SignedArea(polyLine) < 0)) ? (constantOffsetDistance * -1) : constantOffsetDistance;
    if (loop && (((a_1 = ((this$ = pts, ((this$.length === 0) ? failRarr("First.get", this$) : undefined, item(0, this$)))), (b_1 = ((this$_1 = pts, ((this$_1.length === 0) ? failRarr("Last.get", this$_1) : undefined, item(this$_1.length - 1, this$_1)))), (vx = (a_1.X - b_1.X), (vy = (a_1.Y - b_1.Y), (vx * vx) + (vy * vy)))))) > 1E-12)) {
        const closedPts = ResizeArr_closeLoop(pts);
        const normals = makeOffsetDirections(closedPts);
        const res = offsetWithDirections(closedPts, normals, constantOffsetDistance_1, uTurnBehavior, useUTurnBehaviorAbove);
        (this$_2 = res, ((this$_2.length === 0) ? failRarr("Pop", this$_2) : undefined, (i = ((this$_2.length - 1) | 0), (v = item(i, this$_2), (this$_2.splice(i, 1), v)))));
        return Polyline2D_createDirectlyUnsafe_Z5FD8CF3C(res);
    }
    else {
        const normals_1 = makeOffsetDirections(pts);
        return Polyline2D_createDirectlyUnsafe_Z5FD8CF3C(offsetWithDirections(pts, normals_1, constantOffsetDistance_1, uTurnBehavior, useUTurnBehaviorAbove));
    }
}

/**
 * Offsets a Polyline in 2D space by finding the local offset in each corner.
 * Auto detects if given points are from a closed Polyline (first point = last point) and loops them.
 * By default this function raises an Exception on duplicate points, 180 degree U-Turns, and variable distances at colinear segments.
 * But this can be configured with optional parameters.
 */
export function Polyline2D_offsetVar_3B75EE3F(polyLine, multipleOffsetDistances, loop = false, checkOrientation = true, varDistParallelBehavior = 1, uTurnBehavior = 1, useVarDistParallelBehaviorBelow = 0.9961946980917455, useUTurnBehaviorAbove = -0.9961946980917455) {
    let a_1, this$, b_1, this$_1, vx, vy, this$_2, i_1, v;
    const pts = Polyline2D__get_Points(polyLine);
    if (pts.length < 2) {
        fail(`Polyline2D.offset: Polyline2D must have at least 2 points but has ${pts.length} points. ${polyLine}`);
    }
    let distances;
    if (checkOrientation && (Polyline2D__get_SignedArea(polyLine) < 0)) {
        const ds = [];
        for (let i = 0; i <= (count_1(multipleOffsetDistances) - 1); i++) {
            void (ds.push(item(i, multipleOffsetDistances) * -1));
        }
        distances = ds;
    }
    else {
        distances = multipleOffsetDistances;
    }
    if (loop && (((a_1 = ((this$ = pts, ((this$.length === 0) ? failRarr("First.get", this$) : undefined, item(0, this$)))), (b_1 = ((this$_1 = pts, ((this$_1.length === 0) ? failRarr("Last.get", this$_1) : undefined, item(this$_1.length - 1, this$_1)))), (vx = (a_1.X - b_1.X), (vy = (a_1.Y - b_1.Y), (vx * vx) + (vy * vy)))))) > 1E-12)) {
        if (count_1(distances) !== pts.length) {
            fail("Polyline2D.offset: For open Polyline2D with loop=true the multipleOffsetDistances must have the same number of items as the polyline has points.\n" + (`But polyline has ${pts.length} points and multipleOffsetDistances has ${count_1(distances)} items.`));
        }
        const closedPts = ResizeArr_closeLoop(pts);
        const normals = makeOffsetDirections(closedPts);
        const res = offsetVariableWithDirections(closedPts, normals, distances, varDistParallelBehavior, uTurnBehavior, useVarDistParallelBehaviorBelow, useUTurnBehaviorAbove);
        (this$_2 = res, ((this$_2.length === 0) ? failRarr("Pop", this$_2) : undefined, (i_1 = ((this$_2.length - 1) | 0), (v = item(i_1, this$_2), (this$_2.splice(i_1, 1), v)))));
        return Polyline2D_$ctor_Z5FD8CF3C(res);
    }
    else {
        const normals_1 = makeOffsetDirections(pts);
        return Polyline2D_$ctor_Z5FD8CF3C(offsetVariableWithDirections(pts, normals_1, distances, varDistParallelBehavior, uTurnBehavior, useVarDistParallelBehaviorBelow, useUTurnBehaviorAbove));
    }
}

/**
 * Offsets a Polyline in 2D space by finding the local offset in each corner.
 * Auto detects if given points are from a closed Polyline (first point = last point) and loops them.
 * By default this function raises an Exception on duplicate points, 180 degree U-Turns, and variable distances at colinear segments.
 * But this can be configured with optional parameters.
 */
export function Polyline2D_offsetVar_357F0A77(polyLine, multipleOffsetDistances, loop = false, checkOrientation = true, varDistParallelBehavior = 1, uTurnBehavior = 1, useVarDistParallelBehaviorBelow = 0.9961946980917455, useUTurnBehaviorAbove = -0.9961946980917455) {
    return Polyline2D_offsetVar_3B75EE3F(polyLine, Array.from(multipleOffsetDistances), loop, checkOrientation, varDistParallelBehavior, uTurnBehavior, useVarDistParallelBehaviorBelow, useUTurnBehaviorAbove);
}

/**
 * Tries to find a self intersection in the Polyline2D.
 * Also returns Some if segments are just touching.
 * If found returns the intersection point and the indices of the two segments that intersect.
 * If no intersection is found returns None.
 * This is an O(n^2) algorithm and should only be used for small Polylines.
 */
export function Polyline2D_tryFindSelfIntersection_Z5A89AEF5(pl) {
    const pts = Polyline2D__get_Points(pl);
    const segmentVs = Polyline2D__get_SegmentVectors(pl);
    const segLastIdx = (segmentVs.length - 1) | 0;
    const brs = [];
    let pp = item(0, pts);
    for (let i = 1; i <= (pts.length - 1); i++) {
        const p = item(i, pts);
        let br;
        const a = pp;
        const b = p;
        let minX = a.X;
        let maxX;
        if (b.X > minX) {
            maxX = b.X;
        }
        else {
            minX = b.X;
            maxX = a.X;
        }
        let minY = a.Y;
        let maxY;
        if (b.Y > minY) {
            maxY = b.Y;
        }
        else {
            minY = b.Y;
            maxY = a.Y;
        }
        br = BRect_$ctor_77D16AC0(minX, minY, maxX, maxY);
        void (brs.push(br));
        pp = p;
    }
    const checkSegs = (i_1_mut, j_mut) => {
        checkSegs:
        while (true) {
            const i_1 = i_1_mut, j = j_mut;
            let r, a_3;
            if (i_1 > segLastIdx) {
                return undefined;
            }
            else if (j > segLastIdx) {
                i_1_mut = (i_1 + 1);
                j_mut = (i_1 + 3);
                continue checkSegs;
            }
            else if (!((r = item(j, brs), (a_3 = item(i_1, brs), !((((r.MinX > a_3.MaxX) ? true : (a_3.MinX > r.MaxX)) ? true : (a_3.MinY > r.MaxY)) ? true : (r.MinY > a_3.MaxY)))))) {
                i_1_mut = i_1;
                j_mut = (j + 1);
                continue checkSegs;
            }
            else {
                let matchValue;
                const pA = item(i_1, pts);
                const pB = item(j, pts);
                const vA = item(i_1, segmentVs);
                const vB = item(j, segmentVs);
                const pAx_1 = pA.X;
                const pAy_1 = pA.Y;
                const vAx_1 = vA.X;
                const vAy_1 = vA.Y;
                const vBx_1 = vB.X;
                const vBy_1 = vB.Y;
                const det = (vAx_1 * vBy_1) - (vAy_1 * vBx_1);
                const dx = pB.X - pAx_1;
                const dy = pB.Y - pAy_1;
                const t = ((dx * vBy_1) - (dy * vBx_1)) / det;
                if ((t >= -1E-06) && (t <= 1.000001)) {
                    const u = ((dx * vAy_1) - (dy * vAx_1)) / det;
                    matchValue = (((u >= -1E-06) && (u <= 1.000001)) ? Pt_$ctor_7B00E9A0(pAx_1 + (t * vAx_1), pAy_1 + (t * vAy_1)) : undefined);
                }
                else {
                    matchValue = undefined;
                }
                if (matchValue == null) {
                    i_1_mut = i_1;
                    j_mut = (j + 1);
                    continue checkSegs;
                }
                else {
                    const pt = matchValue;
                    return [pt, i_1, j];
                }
            }
            break;
        }
    };
    if (Polyline2D__get_IsClosed(pl)) {
        return checkSegs(0, 2);
    }
    else {
        let matchValue_1;
        let pA_1;
        const this$_2 = pts;
        if (this$_2.length === 0) {
            failRarr("First.get", this$_2);
        }
        pA_1 = item(0, this$_2);
        let pB_1;
        const this$_3 = pts;
        if (this$_3.length < 2) {
            failRarr("SecondLast.get", this$_3);
        }
        pB_1 = item(this$_3.length - 2, this$_3);
        let vA_1;
        const this$_4 = segmentVs;
        if (this$_4.length === 0) {
            failRarr("First.get", this$_4);
        }
        vA_1 = item(0, this$_4);
        let vB_1;
        const this$_5 = segmentVs;
        if (this$_5.length === 0) {
            failRarr("Last.get", this$_5);
        }
        vB_1 = item(this$_5.length - 1, this$_5);
        const pAx_3 = pA_1.X;
        const pAy_3 = pA_1.Y;
        const vAx_3 = vA_1.X;
        const vAy_3 = vA_1.Y;
        const vBx_3 = vB_1.X;
        const vBy_3 = vB_1.Y;
        const det_1 = (vAx_3 * vBy_3) - (vAy_3 * vBx_3);
        const dx_1 = pB_1.X - pAx_3;
        const dy_1 = pB_1.Y - pAy_3;
        const t_1 = ((dx_1 * vBy_3) - (dy_1 * vBx_3)) / det_1;
        if ((t_1 >= -1E-06) && (t_1 <= 1.000001)) {
            const u_1 = ((dx_1 * vAy_3) - (dy_1 * vAx_3)) / det_1;
            matchValue_1 = (((u_1 >= -1E-06) && (u_1 <= 1.000001)) ? Pt_$ctor_7B00E9A0(pAx_3 + (t_1 * vAx_3), pAy_3 + (t_1 * vAy_3)) : undefined);
        }
        else {
            matchValue_1 = undefined;
        }
        if (matchValue_1 == null) {
            return checkSegs(0, 2);
        }
        else {
            const pt_1 = matchValue_1;
            return [pt_1, 0, segmentVs.length - 1];
        }
    }
}

export function Polyline2D__CloseIfOpen_5E38073B(p, t) {
    Polyline2D__CloseInPlace_5E38073B(p, t);
}

export function Polyline2D_segment(a, b, pl) {
    return Polyline2D_subPolyline(a, b, pl);
}

export class Loop {
    constructor() {
    }
}

export function Loop_$reflection() {
    return class_type("Euclid.Loop", undefined, Loop);
}

function Loop_$ctor() {
    return new Loop();
}

export function Loop_create() {
    return toFail(printf("Euclid.Loop has been removed from Euclid in v0.20.0. use Polyline2D instead."));
}

