
import { float } from "./Format.js";
import { Pnt_$ctor_Z7AD9E565, Pnt__get_AsFSharpCode, Pnt__get_AsString } from "./Pnt.js";
import { Vec_$ctor_Z7AD9E565, Vec__get_AsString } from "./Vec.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { map, item } from "../fable_modules/fable-library-js.5.0.0/Array.js";
import { printf, toFail } from "../fable_modules/fable-library-js.5.0.0/String.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_1 } from "./Pnt.js";

/**
 * A class containing an array of 8 points representing an arbitrary 3D Box.
 * The points can be in arbitrary position in space.
 * <code>
 * 7               6
 * +---------------+
 * /|              /|
 * / |             / |
 * 4  /  |          5 /  |
 * +---------------+   |
 * |   |           |   |
 * |   +-----------|---+
 * |  / 3          |  / 2
 * | /             | /
 * |/              |/
 * +---------------+
 * 0               1
 * </code>
 */
export class FreeBox {
    constructor(pts) {
        this.pts = pts;
    }
    /**
     * Nicely formatted string representation of the Box including its size.
     */
    toString() {
        let copyOfStruct, copyOfStruct_1, copyOfStruct_2, copyOfStruct_3;
        const b = this;
        return `Euclid.Box ${float(FreeBox__get_SizeX(b))} x ${float(FreeBox__get_SizeY(b))} x ${float(FreeBox__get_SizeZ(b))} (Origin:${(copyOfStruct = FreeBox__get_Origin(b), Pnt__get_AsString(copyOfStruct))}| X-ax:${(copyOfStruct_1 = FreeBox__get_Xaxis(b), Vec__get_AsString(copyOfStruct_1))}|Y-ax:${(copyOfStruct_2 = FreeBox__get_Yaxis(b), Vec__get_AsString(copyOfStruct_2))}|Z-ax:${(copyOfStruct_3 = FreeBox__get_Zaxis(b), Vec__get_AsString(copyOfStruct_3))})`;
    }
}

export function FreeBox_$reflection() {
    return class_type("Euclid.FreeBox", undefined, FreeBox);
}

function FreeBox_$ctor_Z6C25B48(pts) {
    return new FreeBox(pts);
}

export function FreeBox__get_Pt0(b) {
    return item(0, b.pts);
}

export function FreeBox__get_Pt1(b) {
    return item(1, b.pts);
}

export function FreeBox__get_Pt2(b) {
    return item(2, b.pts);
}

export function FreeBox__get_Pt3(b) {
    return item(3, b.pts);
}

export function FreeBox__get_Pt4(b) {
    return item(4, b.pts);
}

export function FreeBox__get_Pt5(b) {
    return item(5, b.pts);
}

export function FreeBox__get_Pt6(b) {
    return item(6, b.pts);
}

export function FreeBox__get_Pt7(b) {
    return item(7, b.pts);
}

/**
 * The 8 points that make up the box.
 */
export function FreeBox__get_Points(b) {
    return b.pts;
}

/**
 * Format Box into string with nice floating point number formatting of X, Y and Z size only.
 * But without type name as in v.ToString()
 */
export function FreeBox__get_AsString(b) {
    return `${float(FreeBox__get_SizeX(b))} x ${float(FreeBox__get_SizeY(b))} x ${float(FreeBox__get_SizeZ(b))}`;
}

/**
 * Format FreeBox into an F# code string that can be used to recreate the box.
 */
export function FreeBox__get_AsFSharpCode(b) {
    const ps = FreeBox__get_Points(b);
    return `FreeBox.createFromEightPoints([| ${Pnt__get_AsFSharpCode(item(0, ps))}; ${Pnt__get_AsFSharpCode(item(1, ps))}; ${Pnt__get_AsFSharpCode(item(2, ps))}; ${Pnt__get_AsFSharpCode(item(3, ps))}; ${Pnt__get_AsFSharpCode(item(4, ps))}; ${Pnt__get_AsFSharpCode(item(5, ps))}; ${Pnt__get_AsFSharpCode(item(6, ps))}; ${Pnt__get_AsFSharpCode(item(7, ps))} |])`;
}

/**
 * The first point of the Box array.
 */
export function FreeBox__get_Origin(b) {
    return item(0, b.pts);
}

/**
 * The vector from Pt0 to Pt1 defining the X axis direction and length.
 */
export function FreeBox__get_Xaxis(b) {
    const a = item(1, b.pts);
    const b_1 = item(0, b.pts);
    return Vec_$ctor_Z7AD9E565(a.X - b_1.X, a.Y - b_1.Y, a.Z - b_1.Z);
}

/**
 * The vector from Pt0 to Pt3 defining the Y axis direction and length.
 */
export function FreeBox__get_Yaxis(b) {
    const a = item(3, b.pts);
    const b_1 = item(0, b.pts);
    return Vec_$ctor_Z7AD9E565(a.X - b_1.X, a.Y - b_1.Y, a.Z - b_1.Z);
}

/**
 * The vector from Pt0 to Pt4 defining the Z axis direction and length.
 */
export function FreeBox__get_Zaxis(b) {
    const a = item(4, b.pts);
    const b_1 = item(0, b.pts);
    return Vec_$ctor_Z7AD9E565(a.X - b_1.X, a.Y - b_1.Y, a.Z - b_1.Z);
}

/**
 * The length of the Box from Pt0 to Pt1 in the X direction.
 */
export function FreeBox__get_SizeX(b) {
    let copyOfStruct = FreeBox__get_Xaxis(b);
    const v = copyOfStruct;
    return Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
}

/**
 * The length of the Box from Pt0 to Pt3 in the Y direction.
 */
export function FreeBox__get_SizeY(b) {
    let copyOfStruct = FreeBox__get_Yaxis(b);
    const v = copyOfStruct;
    return Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
}

/**
 * The length of the Box from Pt0 to Pt4 in the Z direction.
 */
export function FreeBox__get_SizeZ(b) {
    let copyOfStruct = FreeBox__get_Zaxis(b);
    const v = copyOfStruct;
    return Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
}

export function FreeBox__GetPt_Z524259A4(b, i) {
    if ((i < 0) ? true : (i > 7)) {
        toFail(printf("FreeBox.Pt index must be between 0 and 7, got %d"))(i);
    }
    return item(i, b.pts);
}

/**
 * Scales the 3D rectangle by a given factor on world origin (0,0,0)
 */
export function FreeBox__Scale_5E38073B(b, factor) {
    return FreeBox_$ctor_Z6C25B48(map((p) => {
        const a = p;
        const f = factor;
        return Pnt_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f);
    }, FreeBox__get_Points(b)));
}

/**
 * Scales the 3D rectangle by a given factor on a given center point
 */
export function FreeBox__ScaleOn(b, cen, factor) {
    const cx = cen.X;
    const cy = cen.Y;
    const cz = cen.Z;
    return FreeBox_$ctor_Z6C25B48(map((p) => Pnt_$ctor_Z7AD9E565(cx + ((p.X - cx) * factor), cy + ((p.Y - cy) * factor), cz + ((p.Z - cz) * factor)), FreeBox__get_Points(b)));
}

/**
 * Returns a FreeBox moved by a vector.
 */
export function FreeBox__Move_Z394EC5F7(b, v) {
    return FreeBox_$ctor_Z6C25B48(map((p) => {
        const p_1 = p;
        const v_1 = v;
        return Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z);
    }, FreeBox__get_Points(b)));
}

/**
 * Returns a FreeBox moved by a given distance in X direction.
 */
export function FreeBox__MoveX_5E38073B(b, distance) {
    return FreeBox_$ctor_Z6C25B48(map((p) => Pnt_$ctor_Z7AD9E565(p.X + distance, p.Y, p.Z), FreeBox__get_Points(b)));
}

/**
 * Returns a FreeBox moved by a given distance in Y direction.
 */
export function FreeBox__MoveY_5E38073B(b, distance) {
    return FreeBox_$ctor_Z6C25B48(map((p) => Pnt_$ctor_Z7AD9E565(p.X, p.Y + distance, p.Z), FreeBox__get_Points(b)));
}

/**
 * Returns a FreeBox moved by a given distance in Z direction.
 */
export function FreeBox__MoveZ_5E38073B(b, distance) {
    return FreeBox_$ctor_Z6C25B48(map((p) => Pnt_$ctor_Z7AD9E565(p.X, p.Y, p.Z + distance), FreeBox__get_Points(b)));
}

/**
 * Applies or multiplies a 4x4 transformation matrix to the FreeBox.
 */
export function FreeBox__Transform_3CAE9522(b, m) {
    return FreeBox_$ctor_Z6C25B48(map((p) => {
        const p_1 = p;
        const m_1 = m;
        const x = p_1.X;
        const y = p_1.Y;
        const z = p_1.Z;
        const sc = 1 / ((((m_1.M14 * x) + (m_1.M24 * y)) + (m_1.M34 * z)) + m_1.M44);
        return Pnt_$ctor_Z7AD9E565(((((m_1.M11 * x) + (m_1.M21 * y)) + (m_1.M31 * z)) + m_1.X41) * sc, ((((m_1.M12 * x) + (m_1.M22 * y)) + (m_1.M32 * z)) + m_1.Y42) * sc, ((((m_1.M13 * x) + (m_1.M23 * y)) + (m_1.M33 * z)) + m_1.Z43) * sc);
    }, FreeBox__get_Points(b)));
}

/**
 * Multiplies (or applies) a RigidMatrix to the FreeBox.
 */
export function FreeBox__TransformRigid_Z625426AD(b, m) {
    return FreeBox_$ctor_Z6C25B48(map((p) => {
        const v = p;
        const m_4 = m;
        const x = v.X;
        const y = v.Y;
        const z = v.Z;
        return Pnt_$ctor_Z7AD9E565((((m_4.M11 * x) + (m_4.M21 * y)) + (m_4.M31 * z)) + m_4.X41, (((m_4.M12 * x) + (m_4.M22 * y)) + (m_4.M32 * z)) + m_4.Y42, (((m_4.M13 * x) + (m_4.M23 * y)) + (m_4.M33 * z)) + m_4.Z43);
    }, FreeBox__get_Points(b)));
}

/**
 * Multiplies (or applies) a Quaternion to the FreeBox.
 * The box is rotated around the World Origin.
 */
export function FreeBox__Rotate_Z2A007687(b, q) {
    return FreeBox_$ctor_Z6C25B48(map((p) => {
        const p_1 = p;
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
    }, FreeBox__get_Points(b)));
}

/**
 * Multiplies (or applies) a Quaternion to the FreeBox around a given center point.
 */
export function FreeBox__RotateWithCenter_4928E16A(b, cen, q) {
    return FreeBox_$ctor_Z6C25B48(map((p) => {
        const cen_2 = cen;
        const q_2 = q;
        const pt_1 = p;
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
    }, FreeBox__get_Points(b)));
}

export function FreeBox_createFromEightPoints_Z6C25B48(pts) {
    if (pts.length !== 8) {
        const arg = pts.length | 0;
        toFail(printf("FreeBox.createFrom8 must be initialized with 8 points, got %d"))(arg);
    }
    return FreeBox_$ctor_Z6C25B48(pts);
}

export function FreeBox_createFromBox_Z394E92B4(box) {
    let b_1, p, v, b_2, p_2, p_1, v_1, v_2, b_3, p_3, v_3, b_4, p_4, v_4, b_5, p_6, p_5, v_5, v_6, b_6, p_9, p_8, p_7, v_7, v_8, v_9, b_7, p_11, p_10, v_10, v_11;
    return FreeBox_$ctor_Z6C25B48([box.Origin, (b_1 = box, (p = b_1.Origin, (v = b_1.Xaxis, Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z)))), (b_2 = box, (p_2 = ((p_1 = b_2.Origin, (v_1 = b_2.Xaxis, Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z)))), (v_2 = b_2.Yaxis, Pnt_$ctor_Z7AD9E565(p_2.X + v_2.X, p_2.Y + v_2.Y, p_2.Z + v_2.Z)))), (b_3 = box, (p_3 = b_3.Origin, (v_3 = b_3.Yaxis, Pnt_$ctor_Z7AD9E565(p_3.X + v_3.X, p_3.Y + v_3.Y, p_3.Z + v_3.Z)))), (b_4 = box, (p_4 = b_4.Origin, (v_4 = b_4.Zaxis, Pnt_$ctor_Z7AD9E565(p_4.X + v_4.X, p_4.Y + v_4.Y, p_4.Z + v_4.Z)))), (b_5 = box, (p_6 = ((p_5 = b_5.Origin, (v_5 = b_5.Xaxis, Pnt_$ctor_Z7AD9E565(p_5.X + v_5.X, p_5.Y + v_5.Y, p_5.Z + v_5.Z)))), (v_6 = b_5.Zaxis, Pnt_$ctor_Z7AD9E565(p_6.X + v_6.X, p_6.Y + v_6.Y, p_6.Z + v_6.Z)))), (b_6 = box, (p_9 = ((p_8 = ((p_7 = b_6.Origin, (v_7 = b_6.Xaxis, Pnt_$ctor_Z7AD9E565(p_7.X + v_7.X, p_7.Y + v_7.Y, p_7.Z + v_7.Z)))), (v_8 = b_6.Yaxis, Pnt_$ctor_Z7AD9E565(p_8.X + v_8.X, p_8.Y + v_8.Y, p_8.Z + v_8.Z)))), (v_9 = b_6.Zaxis, Pnt_$ctor_Z7AD9E565(p_9.X + v_9.X, p_9.Y + v_9.Y, p_9.Z + v_9.Z)))), (b_7 = box, (p_11 = ((p_10 = b_7.Origin, (v_10 = b_7.Yaxis, Pnt_$ctor_Z7AD9E565(p_10.X + v_10.X, p_10.Y + v_10.Y, p_10.Z + v_10.Z)))), (v_11 = b_7.Zaxis, Pnt_$ctor_Z7AD9E565(p_11.X + v_11.X, p_11.Y + v_11.Y, p_11.Z + v_11.Z))))]);
}

/**
 * Creates a FreeBox from four 2D points and a zMin and zMax value.
 */
export function FreeBox_createFromFour2DPoints(zMin, zMax, pts) {
    let pt, pt_1, pt_2, pt_3, pt_4, pt_5, pt_6, pt_7;
    if (pts.length !== 4) {
        const arg = pts.length | 0;
        toFail(printf("FreeBox.createFrom4 must be initialized with 4 points, got %d"))(arg);
    }
    return FreeBox_$ctor_Z6C25B48([(pt = item(0, pts), Pnt_$ctor_Z7AD9E565_1(pt.X, pt.Y, zMin)), (pt_1 = item(1, pts), Pnt_$ctor_Z7AD9E565_1(pt_1.X, pt_1.Y, zMin)), (pt_2 = item(2, pts), Pnt_$ctor_Z7AD9E565_1(pt_2.X, pt_2.Y, zMin)), (pt_3 = item(3, pts), Pnt_$ctor_Z7AD9E565_1(pt_3.X, pt_3.Y, zMin)), (pt_4 = item(0, pts), Pnt_$ctor_Z7AD9E565_1(pt_4.X, pt_4.Y, zMax)), (pt_5 = item(1, pts), Pnt_$ctor_Z7AD9E565_1(pt_5.X, pt_5.Y, zMax)), (pt_6 = item(2, pts), Pnt_$ctor_Z7AD9E565_1(pt_6.X, pt_6.Y, zMax)), (pt_7 = item(3, pts), Pnt_$ctor_Z7AD9E565_1(pt_7.X, pt_7.Y, zMax))]);
}

/**
 * Creates a FreeBox from four 2D points in counter clockwise order and a zMin and zMax value.
 */
export function FreeBox_createFromFour2DPointsArgs_Z282B0FA0(a, b, c, d, zMin, zMax) {
    let pt, pt_1, pt_2, pt_3, pt_4, pt_5, pt_6, pt_7;
    return FreeBox_$ctor_Z6C25B48([(pt = a, Pnt_$ctor_Z7AD9E565_1(pt.X, pt.Y, zMin)), (pt_1 = b, Pnt_$ctor_Z7AD9E565_1(pt_1.X, pt_1.Y, zMin)), (pt_2 = c, Pnt_$ctor_Z7AD9E565_1(pt_2.X, pt_2.Y, zMin)), (pt_3 = d, Pnt_$ctor_Z7AD9E565_1(pt_3.X, pt_3.Y, zMin)), (pt_4 = a, Pnt_$ctor_Z7AD9E565_1(pt_4.X, pt_4.Y, zMax)), (pt_5 = b, Pnt_$ctor_Z7AD9E565_1(pt_5.X, pt_5.Y, zMax)), (pt_6 = c, Pnt_$ctor_Z7AD9E565_1(pt_6.X, pt_6.Y, zMax)), (pt_7 = d, Pnt_$ctor_Z7AD9E565_1(pt_7.X, pt_7.Y, zMax))]);
}

