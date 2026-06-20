
module internal XYZ

    let inline length (x:float) (y:float) (z:float) : float =
        sqrt (x*x + y*y + z*z)

    let inline sqLength (x:float) (y:float) (z:float) : float =
        x*x + y*y + z*z

    let inline dot (x1:float) (y1:float) (z1:float) (x2:float) (y2:float) (z2:float) : float =
        x1*x2 + y1*y2 + z1*z2