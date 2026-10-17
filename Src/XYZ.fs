
module internal XYZ

let inline length (x:float) (y:float) (z:float) : float =
    sqrt (x*x + y*y + z*z)

let inline sqLength (x:float) (y:float) (z:float) : float =
    x*x + y*y + z*z

let inline dot (x1:float) (y1:float) (z1:float) (x2:float) (y2:float) (z2:float) : float =
    x1*x2 + y1*y2 + z1*z2

/// Returns the distance between the tips of two vectors.
let inline dist (ax:float) (ay:float) (az:float) (bx:float) (by:float) (bz:float) : float =
    let x = bx-ax
    let y = by-ay
    let z = bz-az
    sqrt(x*x + y*y + z*z)

/// Returns the angle between two unit-vectors in radians, given by their components.
/// Takes vector orientation into account.
/// Range 0.0 to Pi ( = 0 to 180 degrees).
/// See UnitVec.anglePi for why this does not just use the arc cosine of the dot product.
let inline anglePiOfUnit (ax:float) (ay:float) (az:float) (bx:float) (by:float) (bz:float) : float =
    let dot = ax*bx + ay*by + az*bz
    if -0.98 < dot && dot < 0.98 then // threshold for switching 0.98 ?
        acos dot
    else
        if dot < 0. then System.Math.PI - 2.0 * asin(dist (-ax) (-ay) (-az) bx by bz * 0.5)
        else                              2.0 * asin(dist   ax    ay    az  bx by bz * 0.5)

/// Returns the angle between two unit-vectors in radians, given by their components.
/// Ignores vector orientation.
/// Range 0.0 to Pi/2 ( = 0 to 90 degrees).
let inline angleHalfPiOfUnit (ax:float) (ay:float) (az:float) (bx:float) (by:float) (bz:float) : float =
    let dot = ax*bx + ay*by + az*bz
    let dotAbs = abs dot
    if dotAbs < 0.98 then
        acos dotAbs
    else
        if dot < 0. then 2.0 * asin(dist (-ax) (-ay) (-az) bx by bz * 0.5)
        else             2.0 * asin(dist   ax    ay    az  bx by bz * 0.5)