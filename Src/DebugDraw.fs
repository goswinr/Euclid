namespace Euclid


/// By default the function in this module do nothing. But they are called in case of some errors within this library's Loop module.
/// Set these mutable function object from your 2D or 3D environment if you want debug objects to be drawn.
module Debug2D =

    /// By default this function does nothing. But it is called in case of some errors within this library's Loop module.
    /// Set this mutable function object from your 2D or 3D environment if you want debug objects to be drawn.
    let mutable drawDot = fun (msg:string, pt:Pt) -> () // a curried function cant be set in Fable ??

    /// By default this function does nothing. But it is called in case of some errors within this library's Loop module.
    /// Set this mutable function object from your 2D or 3D environment if you want debug objects to be drawn.
    let mutable drawPt = fun (pt:Pt) -> ()

    /// By default this function does nothing. But it is called in case of some errors within this library's Loop module.
    /// Set this mutable function object from your 2D or 3D environment if you want debug objects to be drawn.
    let mutable drawLineFromTo = fun (a:Pt, b:Pt) -> ()

    /// By default this function does nothing. But it is called in case of some errors within this library's Loop module.
    /// Set this mutable function object from your 2D or 3D environment if you want debug objects to be drawn.
    let mutable drawLine = fun (ln:Line2D) -> ()

    /// By default this function does nothing. But it is called in case of some errors within this library's Loop module.
    /// Set this mutable function object from your 2D or 3D environment if you want debug objects to be drawn.
    let mutable drawPolyLine = fun (ps:Pt seq) -> ()

    /// By default this function does nothing. But it is called in case of some errors within this library's Loop module.
    /// Set this mutable function object from your 2D or 3D environment if you want debug objects to be drawn.
    let mutable drawDotLayer = fun (pt:Pt, msg:string, layer:string) -> ()

    /// By default this function does nothing. But it is called in case of some errors within this library's Loop module.
    /// Set this mutable function object from your 2D or 3D environment if you want debug objects to be drawn.
    let mutable drawPtLayer = fun (pt:Pt, layer:string) -> ()

    /// By default this function does nothing. But it is called in case of some errors within this library's Loop module.
    /// Set this mutable function object from your 2D or 3D environment if you want debug objects to be drawn.
    let mutable drawLineFromToLayer = fun (a:Pt, b:Pt, layer:string) -> ()

    /// By default this function does nothing. But it is called in case of some errors within this library's Loop module.
    /// Set this mutable function object from your 2D or 3D environment if you want debug objects to be drawn.
    let mutable drawLineLayer = fun (ln:Line2D, layer:string) -> ()

    /// By default this function does nothing. But it is called in case of some errors within this library's Loop module.
    /// Set this mutable function object from your 2D or 3D environment if you want debug objects to be drawn.
    let mutable drawPolyLineLayer = fun (ps:seq<Pt>, layer:string) -> ()


/// By default the function in this module do nothing. But they are called in case of some errors within this library's Loop module.
/// Set these mutable function object from your 3D environment if you want debug objects to be drawn.
module Debug3D =

    /// By default this function does nothing. But it is called in case of some errors within this library's Loop module.
    /// Set this mutable function object from your 3D environment if you want debug objects to be drawn.
    let mutable drawDot = fun (msg:string, pt:Pnt) -> () // a curried function cant be set in Fable ??

    /// By default this function does nothing. But it is called in case of some errors within this library's Loop module.
    /// Set this mutable function object from your 3D environment if you want debug objects to be drawn.
    let mutable drawPt = fun (pt:Pnt) -> ()

    /// By default this function does nothing. But it is called in case of some errors within this library's Loop module.
    /// Set this mutable function object from your 3D environment if you want debug objects to be drawn.
    let mutable drawLineFromTo = fun (a:Pnt, b:Pnt) -> ()

    /// By default this function does nothing. But it is called in case of some errors within this library's Loop module.
    /// Set this mutable function object from your 3D environment if you want debug objects to be drawn.
    let mutable drawLine = fun (ln:Line3D) -> ()


    /// By default this function does nothing. But it is called in case of some errors within this library's Loop module.
    /// Set this mutable function object from your 3D environment if you want debug objects to be drawn.
    let mutable drawPolyLine = fun (ps:seq<Pnt>) -> ()

    /// By default this function does nothing. But it is called in case of some errors within this library's Loop module.
    /// Set this mutable function object from your 3D environment if you want debug objects to be drawn.
    let mutable drawDotLayer = fun (pt:Pnt, msg:string, layer:string) -> ()

    /// By default this function does nothing. But it is called in case of some errors within this library's Loop module.
    /// Set this mutable function object from your 3D environment if you want debug objects to be drawn.
    let mutable drawPtLayer = fun (pt:Pnt, layer:string) -> ()

    /// By default this function does nothing. But it is called in case of some errors within this library's Loop module.
    /// Set this mutable function object from your 3D environment if you want debug objects to be drawn.
    let mutable drawLineFromToLayer = fun (a:Pnt, b:Pnt, layer:string) -> ()

    /// By default this function does nothing. But it is called in case of some errors within this library's Loop module.
    /// Set this mutable function object from your 3D environment if you want debug objects to be drawn.
    let mutable drawLineLayer = fun (ln:Line3D, layer:string) -> ()

    /// By default this function does nothing. But it is called in case of some errors within this library's Loop module.
    /// Set this mutable function object from your 3D environment if you want debug objects to be drawn.
    let mutable drawPolyLineLayer = fun (ps:seq<Pnt>, layer:string) -> ()
