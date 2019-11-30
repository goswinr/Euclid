namespace FsEx.Geo


/// Set these functions from your 3D or 2D enviromrnt if you want debug objects to be drawn
module Debug2D =      
            
    let mutable drawDot         = fun (msg:string) (pt:Pt)  -> ()             
    let mutable drawPt          = fun (pt:Pt)               -> ()            
    let mutable drawLine        = fun (a:Pt,b:Pt)           -> ()             
    let mutable drawPolyLine    = fun (ps:Pt seq)           -> () 

    let mutable drawDotLayer          = fun (pt:Pt, msg:string, layer:string)   -> ()             
    let mutable drawPtLayer           = fun (pt:Pt, layer:string)               -> ()            
    let mutable drawLineLayer         = fun (a:Pt,b:Pt, layer:string)           -> ()             
    let mutable drawPolyLineLayer     = fun (ps:seq<Pt>, layer:string)          -> () 

/// Set these functions from your 3D enviromrnt if you want debug objects to be drawn
module Debug3D =      
            
    let mutable drawDot      = fun (msg:string) (pt:Pnt) -> ()             
    let mutable drawPt       = fun (pt:Pnt) -> ()            
    let mutable drawLine     = fun (a:Pnt,b:Pnt) -> ()            
    let mutable drawPolyLine = fun (ps:seq<Pnt>) -> () 

    let mutable drawDotLayer          = fun (pt:Pnt, msg:string, layer:string)   -> ()             
    let mutable drawPtLayer           = fun (pt:Pnt, layer:string)               -> ()            
    let mutable drawLineLayer         = fun (a:Pnt,b:Pnt, layer:string)           -> ()             
    let mutable drawPolyLineLayer     = fun (ps:seq<Pnt>, layer:string)          -> () 
