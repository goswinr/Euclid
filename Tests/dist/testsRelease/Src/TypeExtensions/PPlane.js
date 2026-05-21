
import { Vec_$ctor_Z7AD9E565 } from "../Vec.js";
import { failTooSmall, failTooSmall2, failUnit3, failColinear, failTooClose } from "../EuclidErrors.js";
import { UnitVec_$ctor_Z7AD9E565 } from "../UnitVec.js";
import { PPlane_$ctor_3CB4665C } from "../PPlane.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_1 } from "../Vec.js";
import { Pnt_$ctor_Z7AD9E565 } from "../Pnt.js";
import { Line3D_$ctor_76A78260 } from "../Line3D.js";

/**
 * Builds Plane at first point, X-axis to second point,
 * Y-axis to third point or at least in plane with third point.
 * Fails if points are closer than 1e-6.
 */
export function Euclid_PPlane__PPlane_createThreePoints_Static(origin, xPt, yPt) {
    let a_2, b_2, v_2, a_4, b_4, x_5, y_3, z_3, l, f, v_3, x_8, y_5, z_5, l_1, f_1;
    let x;
    const a = xPt;
    const b = origin;
    x = Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
    let y;
    const a_1 = yPt;
    const b_1 = origin;
    y = Vec_$ctor_Z7AD9E565(a_1.X - b_1.X, a_1.Y - b_1.Y, a_1.Z - b_1.Z);
    let lx;
    const v = x;
    lx = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    let ly;
    const v_1 = y;
    ly = Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
    if (!(lx > 1E-06)) {
        failTooClose("PPlane.createThreePoints xPt", origin, xPt);
    }
    if (!(ly > 1E-06)) {
        failTooClose("PPlane.createThreePoints yPt", origin, yPt);
    }
    const xf = 1 / lx;
    const yf = 1 / ly;
    const xu = UnitVec_$ctor_Z7AD9E565(x.X * xf, x.Y * xf, x.Z * xf);
    const yu = UnitVec_$ctor_Z7AD9E565(y.X * yf, y.Y * yf, y.Z * yf);
    if (Math.abs((a_2 = yu, (b_2 = xu, ((a_2.X * b_2.X) + (a_2.Y * b_2.Y)) + (a_2.Z * b_2.Z)))) > 0.9998476951563913) {
        failColinear("PPlane.createThreePoints", origin, xPt, yPt);
    }
    let z_2;
    const a_3 = xu;
    const b_3 = yu;
    z_2 = Vec_$ctor_Z7AD9E565((a_3.Y * b_3.Z) - (a_3.Z * b_3.Y), (a_3.Z * b_3.X) - (a_3.X * b_3.Z), (a_3.X * b_3.Y) - (a_3.Y * b_3.X));
    return PPlane_$ctor_3CB4665C(origin, xu, (v_2 = ((a_4 = z_2, (b_4 = x, Vec_$ctor_Z7AD9E565((a_4.Y * b_4.Z) - (a_4.Z * b_4.Y), (a_4.Z * b_4.X) - (a_4.X * b_4.Z), (a_4.X * b_4.Y) - (a_4.Y * b_4.X))))), (x_5 = v_2.X, (y_3 = v_2.Y, (z_3 = v_2.Z, (l = Math.sqrt(((x_5 * x_5) + (y_3 * y_3)) + (z_3 * z_3)), (!(l > 1E-12) ? failUnit3("Vec.Unitized", x_5, y_3, z_3) : undefined, (f = (1 / l), UnitVec_$ctor_Z7AD9E565(f * x_5, f * y_3, f * z_3)))))))), (v_3 = z_2, (x_8 = v_3.X, (y_5 = v_3.Y, (z_5 = v_3.Z, (l_1 = Math.sqrt(((x_8 * x_8) + (y_5 * y_5)) + (z_5 * z_5)), (!(l_1 > 1E-12) ? failUnit3("Vec.Unitized", x_8, y_5, z_5) : undefined, (f_1 = (1 / l_1), UnitVec_$ctor_Z7AD9E565(f_1 * x_8, f_1 * y_5, f_1 * z_5)))))))));
}

/**
 * Creates a Parametrized Plane from a point and a unit-vector representing the X-axis and a Y-axis hint.
 * The resulting PPlane will have the X-Axis in direction of X vector.
 * The X and Y vectors will define the plane and the side that Z will be on.
 * The given Y vector does not need to be perpendicular to the X vector, just not parallel.
 * Fails if the vectors are shorter than 1e-6.
 */
export function Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_58CBC093(origin, xAxis, yAxis) {
    let a, b, v, a_2, b_2, x, y_1, z_1, l, f, v_1, x_3, y_3, z_3, l_1, f_1;
    if (Math.abs((a = yAxis, (b = xAxis, ((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z)))) > 0.9998476951563913) {
        failColinear("PPlane.createOriginXaxisYaxis", origin, xAxis, yAxis);
    }
    let z;
    const a_1 = xAxis;
    const b_1 = yAxis;
    z = Vec_$ctor_Z7AD9E565((a_1.Y * b_1.Z) - (a_1.Z * b_1.Y), (a_1.Z * b_1.X) - (a_1.X * b_1.Z), (a_1.X * b_1.Y) - (a_1.Y * b_1.X));
    return PPlane_$ctor_3CB4665C(origin, xAxis, (v = ((a_2 = z, (b_2 = xAxis, Vec_$ctor_Z7AD9E565_1((a_2.Y * b_2.Z) - (a_2.Z * b_2.Y), (a_2.Z * b_2.X) - (a_2.X * b_2.Z), (a_2.X * b_2.Y) - (a_2.Y * b_2.X))))), (x = v.X, (y_1 = v.Y, (z_1 = v.Z, (l = Math.sqrt(((x * x) + (y_1 * y_1)) + (z_1 * z_1)), (!(l > 1E-12) ? failUnit3("Vec.Unitized", x, y_1, z_1) : undefined, (f = (1 / l), UnitVec_$ctor_Z7AD9E565(f * x, f * y_1, f * z_1)))))))), (v_1 = z, (x_3 = v_1.X, (y_3 = v_1.Y, (z_3 = v_1.Z, (l_1 = Math.sqrt(((x_3 * x_3) + (y_3 * y_3)) + (z_3 * z_3)), (!(l_1 > 1E-12) ? failUnit3("Vec.Unitized", x_3, y_3, z_3) : undefined, (f_1 = (1 / l_1), UnitVec_$ctor_Z7AD9E565(f_1 * x_3, f_1 * y_3, f_1 * z_3)))))))));
}

/**
 * Creates a Parametrized Plane from a point and vector representing the X-axis and a Y-axis hint.
 * The resulting PPlane will have the X-Axis in direction of X vector.
 * The X and Y vectors will define the plane and the side that Z will be on.
 * The given Y vector does not need to be perpendicular to the X vector, just not parallel.
 * Fails if the vectors are shorter than 1e-6.
 */
export function Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_6181AF53(origin, xAxis, yAxis) {
    let lx;
    const v = xAxis;
    lx = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    let ly;
    const v_1 = yAxis;
    ly = Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
    if (!(lx > 1E-06)) {
        failTooSmall2("PPlane.createOriginXaxisYaxis xAxis", xAxis, yAxis);
    }
    if (!(ly > 1E-06)) {
        failTooSmall2("PPlane.createOriginXaxisYaxis yAxis", yAxis, xAxis);
    }
    const xf = 1 / lx;
    const yf = 1 / ly;
    return Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_58CBC093(origin, UnitVec_$ctor_Z7AD9E565(xAxis.X * xf, xAxis.Y * xf, xAxis.Z * xf), UnitVec_$ctor_Z7AD9E565(yAxis.X * yf, yAxis.Y * yf, yAxis.Z * yf));
}

/**
 * Creates a Parametrized Plane from a point and unit-vector representing the normal (or Z-axis).
 * The X-axis will be found by taking the Cross Product of the World Z-axis and the given normal (or Z-axis).
 * This will make the X-axis horizontal.
 * If this fails because they are coincident, the Cross Product of the World Y-axis and the given normal (or Z-axis) will be used.
 */
export function Euclid_PPlane__PPlane_createOriginNormal_Static_Z2DDF2344(origin, normal) {
    let other, a, b, v, a_2, b_2, x_2, y_2, z_1, l, f, v_1, x_5, y_4, z_3, l_1, f_1, v_2, x_9, y_7, z_5, l_2, f_2, v_3, x_12, y_9, z_7, l_3, f_3;
    if ((other = UnitVec_$ctor_Z7AD9E565(0, 0, 1), Math.abs((a = other, (b = normal, ((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z)))) > 0.9999619230641713)) {
        let y_1;
        const a_1 = normal;
        const b_1 = Vec_$ctor_Z7AD9E565_1(1, 0, 0);
        y_1 = Vec_$ctor_Z7AD9E565_1((a_1.Y * b_1.Z) - (a_1.Z * b_1.Y), (a_1.Z * b_1.X) - (a_1.X * b_1.Z), (a_1.X * b_1.Y) - (a_1.Y * b_1.X));
        return PPlane_$ctor_3CB4665C(origin, (v = ((a_2 = y_1, (b_2 = normal, Vec_$ctor_Z7AD9E565_1((a_2.Y * b_2.Z) - (a_2.Z * b_2.Y), (a_2.Z * b_2.X) - (a_2.X * b_2.Z), (a_2.X * b_2.Y) - (a_2.Y * b_2.X))))), (x_2 = v.X, (y_2 = v.Y, (z_1 = v.Z, (l = Math.sqrt(((x_2 * x_2) + (y_2 * y_2)) + (z_1 * z_1)), (!(l > 1E-12) ? failUnit3("Vec.Unitized", x_2, y_2, z_1) : undefined, (f = (1 / l), UnitVec_$ctor_Z7AD9E565(f * x_2, f * y_2, f * z_1)))))))), (v_1 = y_1, (x_5 = v_1.X, (y_4 = v_1.Y, (z_3 = v_1.Z, (l_1 = Math.sqrt(((x_5 * x_5) + (y_4 * y_4)) + (z_3 * z_3)), (!(l_1 > 1E-12) ? failUnit3("Vec.Unitized", x_5, y_4, z_3) : undefined, (f_1 = (1 / l_1), UnitVec_$ctor_Z7AD9E565(f_1 * x_5, f_1 * y_4, f_1 * z_3)))))))), normal);
    }
    else {
        let x_8;
        const a_3 = Vec_$ctor_Z7AD9E565_1(0, 0, 1);
        const b_3 = normal;
        x_8 = Vec_$ctor_Z7AD9E565_1((a_3.Y * b_3.Z) - (a_3.Z * b_3.Y), (a_3.Z * b_3.X) - (a_3.X * b_3.Z), (a_3.X * b_3.Y) - (a_3.Y * b_3.X));
        let y_6;
        const a_4 = normal;
        const b_4 = x_8;
        y_6 = Vec_$ctor_Z7AD9E565_1((a_4.Y * b_4.Z) - (a_4.Z * b_4.Y), (a_4.Z * b_4.X) - (a_4.X * b_4.Z), (a_4.X * b_4.Y) - (a_4.Y * b_4.X));
        return PPlane_$ctor_3CB4665C(origin, (v_2 = x_8, (x_9 = v_2.X, (y_7 = v_2.Y, (z_5 = v_2.Z, (l_2 = Math.sqrt(((x_9 * x_9) + (y_7 * y_7)) + (z_5 * z_5)), (!(l_2 > 1E-12) ? failUnit3("Vec.Unitized", x_9, y_7, z_5) : undefined, (f_2 = (1 / l_2), UnitVec_$ctor_Z7AD9E565(f_2 * x_9, f_2 * y_7, f_2 * z_5)))))))), (v_3 = y_6, (x_12 = v_3.X, (y_9 = v_3.Y, (z_7 = v_3.Z, (l_3 = Math.sqrt(((x_12 * x_12) + (y_9 * y_9)) + (z_7 * z_7)), (!(l_3 > 1E-12) ? failUnit3("Vec.Unitized", x_12, y_9, z_7) : undefined, (f_3 = (1 / l_3), UnitVec_$ctor_Z7AD9E565(f_3 * x_12, f_3 * y_9, f_3 * z_7)))))))), normal);
    }
}

/**
 * Creates a Parametrized Plane from a point and vector representing the normal (or Z-axis).
 * The X-axis will be found by taking the Cross Product of the World Z-axis and the given normal (or Z-axis).
 * This will make the X-axis horizontal.
 * If this fails because they are coincident, the Cross Product of the World Y-axis and the given normal (or Z-axis) will be used.
 * Fails if the vectors are shorter than 1e-6.
 */
export function Euclid_PPlane__PPlane_createOriginNormal_Static_5A66521A(origin, normal) {
    let len;
    const v = normal;
    len = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    if (!(len > 1E-06)) {
        failTooSmall("PPlane.createOriginNormal", normal);
    }
    const f = 1 / len;
    return Euclid_PPlane__PPlane_createOriginNormal_Static_Z2DDF2344(origin, UnitVec_$ctor_Z7AD9E565(normal.X * f, normal.Y * f, normal.Z * f));
}

/**
 * Creates a Parametrized Plane from a point and unit-vector representing the Z-axis.
 * The given X vector does not need to be perpendicular to the normal vector, just not parallel.
 * Fails if the vectors are shorter than 1e-6 or normal and xAxis are parallel within 1 degree.
 */
export function Euclid_PPlane__PPlane_createOriginNormalXaxis_Static_58CBC093(origin, normal, xAxis) {
    let a, b, v, a_2, b_2, x_1, y_1, z, l, f, v_1, x_4, y_3, z_2, l_1, f_1;
    if (Math.abs((a = xAxis, (b = normal, ((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z)))) > 0.9998476951563913) {
        failColinear("PPlane.createOriginNormalXaxis", origin, normal, xAxis);
    }
    let y;
    const a_1 = normal;
    const b_1 = xAxis;
    y = Vec_$ctor_Z7AD9E565((a_1.Y * b_1.Z) - (a_1.Z * b_1.Y), (a_1.Z * b_1.X) - (a_1.X * b_1.Z), (a_1.X * b_1.Y) - (a_1.Y * b_1.X));
    return PPlane_$ctor_3CB4665C(origin, (v = ((a_2 = y, (b_2 = normal, Vec_$ctor_Z7AD9E565_1((a_2.Y * b_2.Z) - (a_2.Z * b_2.Y), (a_2.Z * b_2.X) - (a_2.X * b_2.Z), (a_2.X * b_2.Y) - (a_2.Y * b_2.X))))), (x_1 = v.X, (y_1 = v.Y, (z = v.Z, (l = Math.sqrt(((x_1 * x_1) + (y_1 * y_1)) + (z * z)), (!(l > 1E-12) ? failUnit3("Vec.Unitized", x_1, y_1, z) : undefined, (f = (1 / l), UnitVec_$ctor_Z7AD9E565(f * x_1, f * y_1, f * z)))))))), (v_1 = y, (x_4 = v_1.X, (y_3 = v_1.Y, (z_2 = v_1.Z, (l_1 = Math.sqrt(((x_4 * x_4) + (y_3 * y_3)) + (z_2 * z_2)), (!(l_1 > 1E-12) ? failUnit3("Vec.Unitized", x_4, y_3, z_2) : undefined, (f_1 = (1 / l_1), UnitVec_$ctor_Z7AD9E565(f_1 * x_4, f_1 * y_3, f_1 * z_2)))))))), normal);
}

/**
 * Creates a Parametrized Plane from a point and unit-vector representing the Z-axis.
 * The given X vector does not need to be perpendicular to the normal vector, just not parallel.
 * Fails if the vectors are shorter than 1e-6 or normal and X are parallel.
 */
export function Euclid_PPlane__PPlane_createOriginNormalXaxis_Static_6181AF53(origin, normal, xAxis) {
    let lx;
    const v = xAxis;
    lx = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    let ln;
    const v_1 = normal;
    ln = Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
    if (!(lx > 1E-06)) {
        failTooSmall2("PPlane.createOriginNormalXaxis xAxis", xAxis, normal);
    }
    if (!(ln > 1E-06)) {
        failTooSmall2("PPlane.createOriginNormalXaxis normal", normal, xAxis);
    }
    const xf = 1 / lx;
    const nf = 1 / ln;
    const xu = UnitVec_$ctor_Z7AD9E565(xAxis.X * xf, xAxis.Y * xf, xAxis.Z * xf);
    return Euclid_PPlane__PPlane_createOriginNormalXaxis_Static_58CBC093(origin, UnitVec_$ctor_Z7AD9E565(normal.X * nf, normal.Y * nf, normal.Z * nf), xu);
}

/**
 * Transforms the plane by the given RigidMatrix.
 * The returned PPlane has orthogonal unit-vectors.
 */
export function Euclid_PPlane__PPlane_transform_Static(m, pl) {
    let v, m_4, x, y, z, v_4, m_8, x_1, y_1, z_1, v_8, m_12, x_4, y_3, z_3, v_12, m_16, x_6, y_6, z_5;
    return PPlane_$ctor_3CB4665C((v = pl.Origin, (m_4 = m, (x = v.X, (y = v.Y, (z = v.Z, Pnt_$ctor_Z7AD9E565((((m_4.M11 * x) + (m_4.M21 * y)) + (m_4.M31 * z)) + m_4.X41, (((m_4.M12 * x) + (m_4.M22 * y)) + (m_4.M32 * z)) + m_4.Y42, (((m_4.M13 * x) + (m_4.M23 * y)) + (m_4.M33 * z)) + m_4.Z43)))))), (v_4 = pl.Xaxis, (m_8 = m, (x_1 = v_4.X, (y_1 = v_4.Y, (z_1 = v_4.Z, UnitVec_$ctor_Z7AD9E565(((m_8.M11 * x_1) + (m_8.M21 * y_1)) + (m_8.M31 * z_1), ((m_8.M12 * x_1) + (m_8.M22 * y_1)) + (m_8.M32 * z_1), ((m_8.M13 * x_1) + (m_8.M23 * y_1)) + (m_8.M33 * z_1))))))), (v_8 = pl.Yaxis, (m_12 = m, (x_4 = v_8.X, (y_3 = v_8.Y, (z_3 = v_8.Z, UnitVec_$ctor_Z7AD9E565(((m_12.M11 * x_4) + (m_12.M21 * y_3)) + (m_12.M31 * z_3), ((m_12.M12 * x_4) + (m_12.M22 * y_3)) + (m_12.M32 * z_3), ((m_12.M13 * x_4) + (m_12.M23 * y_3)) + (m_12.M33 * z_3))))))), (v_12 = pl.Zaxis, (m_16 = m, (x_6 = v_12.X, (y_6 = v_12.Y, (z_5 = v_12.Z, UnitVec_$ctor_Z7AD9E565(((m_16.M11 * x_6) + (m_16.M21 * y_6)) + (m_16.M31 * z_5), ((m_16.M12 * x_6) + (m_16.M22 * y_6)) + (m_16.M32 * z_5), ((m_16.M13 * x_6) + (m_16.M23 * y_6)) + (m_16.M33 * z_5))))))));
}

/**
 * Returns the line of intersection between two planes.
 * Returns None if they are parallel or coincident.
 */
export function Euclid_PPlane__PPlane_intersect_Static(a, b) {
    let v_1, a_4, a_5, b_5, b_4;
    const bn = b.Zaxis;
    const an = a.Zaxis;
    let v;
    const a_1 = an;
    const b_1 = bn;
    v = Vec_$ctor_Z7AD9E565((a_1.Y * b_1.Z) - (a_1.Z * b_1.Y), (a_1.Z * b_1.X) - (a_1.X * b_1.Z), (a_1.X * b_1.Y) - (a_1.Y * b_1.X));
    if (!(((v_1 = v, ((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z))) > 1E-12)) {
        return undefined;
    }
    else {
        let pa;
        const a_2 = v;
        const b_2 = an;
        pa = Vec_$ctor_Z7AD9E565_1((a_2.Y * b_2.Z) - (a_2.Z * b_2.Y), (a_2.Z * b_2.X) - (a_2.X * b_2.Z), (a_2.X * b_2.Y) - (a_2.Y * b_2.X));
        let nenner;
        const a_3 = pa;
        const b_3 = bn;
        nenner = (((a_3.X * b_3.X) + (a_3.Y * b_3.Y)) + (a_3.Z * b_3.Z));
        const ao = a.Origin;
        let xpt;
        const p = ao;
        let v_2;
        const a_6 = pa;
        const f = ((a_4 = ((a_5 = b.Origin, (b_5 = ao, Vec_$ctor_Z7AD9E565(a_5.X - b_5.X, a_5.Y - b_5.Y, a_5.Z - b_5.Z)))), (b_4 = bn, ((a_4.X * b_4.X) + (a_4.Y * b_4.Y)) + (a_4.Z * b_4.Z)))) / nenner;
        v_2 = Vec_$ctor_Z7AD9E565(a_6.X * f, a_6.Y * f, a_6.Z * f);
        xpt = Pnt_$ctor_Z7AD9E565(p.X + v_2.X, p.Y + v_2.Y, p.Z + v_2.Z);
        return Line3D_$ctor_76A78260(xpt.X, xpt.Y, xpt.Z, xpt.X + v.X, xpt.Y + v.Y, xpt.Z + v.Z);
    }
}

/**
 * Returns the parameter on the line.
 * The parameter is the intersection point of the infinite ray with the PPlane.
 * Returns None if they are parallel or coincident.
 */
export function Euclid_PPlane__PPlane_intersectLineParameter_Static(ln, pl) {
    let ln_2, ln_3, ln_4, a_1, a_2, b_2, ln_5, b_1;
    const z = pl.Zaxis;
    let nenner;
    let a;
    const ln_1 = ln;
    a = Vec_$ctor_Z7AD9E565((ln_2 = ln_1, ln_2.ToX - ln_2.FromX), (ln_3 = ln_1, ln_3.ToY - ln_3.FromY), (ln_4 = ln_1, ln_4.ToZ - ln_4.FromZ));
    const b = z;
    nenner = (((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z));
    if (!(Math.abs(nenner) > 1E-06)) {
        return undefined;
    }
    else {
        return ((a_1 = ((a_2 = pl.Origin, (b_2 = ((ln_5 = ln, Pnt_$ctor_Z7AD9E565(ln_5.FromX, ln_5.FromY, ln_5.FromZ))), Vec_$ctor_Z7AD9E565(a_2.X - b_2.X, a_2.Y - b_2.Y, a_2.Z - b_2.Z)))), (b_1 = z, ((a_1.X * b_1.X) + (a_1.Y * b_1.Y)) + (a_1.Z * b_1.Z)))) / nenner;
    }
}

/**
 * Returns the line parameter and the X and Y parameters on the Plane. as tuple (pLn, pPlX, pPlY).
 * The parameters is the intersection point of the infinite ray with the PPlane.
 * Returns None if they are parallel or coincident.
 */
export function Euclid_PPlane__PPlane_intersectLineParameters_Static(ln, pl) {
    let ln_2, ln_3, ln_4, a_1, a_2, b_2, ln_5, b_1, a_5, b_4, a_6, b_5;
    const z = pl.Zaxis;
    let v;
    const ln_1 = ln;
    v = Vec_$ctor_Z7AD9E565((ln_2 = ln_1, ln_2.ToX - ln_2.FromX), (ln_3 = ln_1, ln_3.ToY - ln_3.FromY), (ln_4 = ln_1, ln_4.ToZ - ln_4.FromZ));
    let nenner;
    const a = v;
    const b = z;
    nenner = (((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z));
    if (!(Math.abs(nenner) > 1E-06)) {
        return undefined;
    }
    else {
        const t = ((a_1 = ((a_2 = pl.Origin, (b_2 = ((ln_5 = ln, Pnt_$ctor_Z7AD9E565(ln_5.FromX, ln_5.FromY, ln_5.FromZ))), Vec_$ctor_Z7AD9E565(a_2.X - b_2.X, a_2.Y - b_2.Y, a_2.Z - b_2.Z)))), (b_1 = z, ((a_1.X * b_1.X) + (a_1.Y * b_1.Y)) + (a_1.Z * b_1.Z)))) / nenner;
        let v_2;
        let a_4;
        let p;
        const ln_6 = ln;
        p = Pnt_$ctor_Z7AD9E565(ln_6.FromX, ln_6.FromY, ln_6.FromZ);
        let v_1;
        const a_3 = v;
        const f = t;
        v_1 = Vec_$ctor_Z7AD9E565(a_3.X * f, a_3.Y * f, a_3.Z * f);
        a_4 = Pnt_$ctor_Z7AD9E565(p.X + v_1.X, p.Y + v_1.Y, p.Z + v_1.Z);
        const b_3 = pl.Origin;
        v_2 = Vec_$ctor_Z7AD9E565(a_4.X - b_3.X, a_4.Y - b_3.Y, a_4.Z - b_3.Z);
        return [t, (a_5 = pl.Xaxis, (b_4 = v_2, ((a_5.X * b_4.X) + (a_5.Y * b_4.Y)) + (a_5.Z * b_4.Z))), (a_6 = pl.Yaxis, (b_5 = v_2, ((a_6.X * b_5.X) + (a_6.Y * b_5.Y)) + (a_6.Z * b_5.Z)))];
    }
}

/**
 * Returns intersection point of ray with Plane.
 * Returns None if they are parallel.
 * Returns None if the line is too short.
 */
export function Euclid_PPlane__PPlane_intersectLine_Static(ln, pl) {
    let p, ln_2, v, a, ln_3, ln_4, ln_5, ln_6, f;
    const matchValue = Euclid_PPlane__PPlane_intersectLineParameter_Static(ln, pl);
    if (matchValue == null) {
        return undefined;
    }
    else {
        const t = matchValue;
        if ((0 <= t) && (t <= 1)) {
            return (p = ((ln_2 = ln, Pnt_$ctor_Z7AD9E565(ln_2.FromX, ln_2.FromY, ln_2.FromZ))), (v = ((a = ((ln_3 = ln, Vec_$ctor_Z7AD9E565((ln_4 = ln_3, ln_4.ToX - ln_4.FromX), (ln_5 = ln_3, ln_5.ToY - ln_5.FromY), (ln_6 = ln_3, ln_6.ToZ - ln_6.FromZ)))), (f = t, Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f)))), Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z)));
        }
        else {
            return undefined;
        }
    }
}

