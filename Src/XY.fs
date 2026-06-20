module internal XY


let inline length (x:float) (y:float) : float =
    sqrt (x*x + y*y)

let inline sqLength (x:float) (y:float) : float =
    x*x + y*y


let inline dot (x1:float) (y1:float) (x2:float) (y2:float) : float =
    x1*x2 + y1*y2