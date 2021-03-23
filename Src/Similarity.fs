namespace FsEx.Geo

open System.Collections.Generic

/// For finding 2d object that are similar but not exactly the same 
/// based on their 2D point clouds
/// (this could be extendet to work in 3d too)
module Similarity2D =  
    // also see: PANELS (10) lay Panels flat.fsx

    type SimilarityGroup =  { 
        typ:string
        bbox:BBox
        points:Pt[] // must be sorted by 'X' property for binary search,  
        //duplicate points with in tolerance will most likle lead to false results(not all indices will be covered in simPts)
        } 
    
    type SimilarityData = {   
        extend: Pt // represents the max value of a bounding box ,  min value must be x0, y0
        groups: SimilarityGroup[] // must be sorted by 'typ' property for Array.forall2        
        } 
    
    /// returns index of a similar point or -1.    
    /// ps[] must be sorted by 'X' property. 
    let internal simPt tol (ps:Pt[]) (pt:Pt)=     
        let x = pt.X
        let y = pt.Y
        // finds the index of a point wher x matches,  ther might be more than one match
        let rec binSearchIdx lo hi =
            if lo <= hi then
                let mid = lo + ((hi - lo) >>> 1)
                let p = ps.[mid]
                if abs(p.X - x)  < tol then  
                    mid
                elif p.X < x then  
                    binSearchIdx (mid + 1) hi
                else 
                    binSearchIdx lo (mid - 1)
            else -1
        
        match binSearchIdx 0 ps.Length-1 with 
        | -1 -> -1  
        | ix ->            
            // x is OK now search up and down if there is a Y match too
            let rec searchIdxDown i = 
                let p = ps.[i]
                if  abs(p.X - x)  < tol then  
                    if  abs(p.Y - y)  < tol then   
                        i
                    else
                        if i=0 then -1
                        else searchIdxDown (i-1)
                else 
                    -1
            match searchIdxDown ix with 
            | -1 -> 
                let last = ps.Length-1
                let rec searchIdxUp i = 
                    let p = ps.[i]
                    if  abs(p.X - x)  < tol then  
                        if  abs(p.Y - y)  < tol then   
                            i
                        else
                            if i=last then -1
                            else searchIdxUp (i+1)
                    else 
                        -1        
                searchIdxUp ix      
            | ixy ->  
                ixy 
      
    let internal simPts tol (ps:Pt[]) (cs:Pt[]) = 
        let rs = Array.zeroCreate ps.Length //to later check that all indices are covered
        ps |> Array.forall( fun p ->  
            match simPt tol cs p with 
            | -1 -> false
            |  i ->  
                rs.[i] <- true
                true
            ) //|>! printfn "simPts forall  %b"
        // the above checks that each point overlaps with another point 
        // but they migh also cover the same point so wee need to check  that all indices are covered: 
        && rs |> Array.forall id 
    
    /// takes cleaned up data 
    let areSimliar (tol:float) (a:SimilarityData) (b:SimilarityData) : bool = 
        let inline sim (a:Pt) (b:Pt)  = 
            abs(a.X - b.X) < tol && abs(a.Y - b.Y) < tol
        
        let inline simBox (a:BBox) (b:BBox)  = 
            sim a.MinPt b.MinPt    
            && sim a.MaxPt b.MaxPt    
        
        sim a.extend b.extend                               
        && a.groups.Length = b.groups.Length                
        && (a.groups , b.groups) ||> Array.forall2 (fun x y ->  
            x.typ=y.typ                                     
            && x.points.Length = x.points.Length            
            && simBox x.bbox y.bbox                         
            && simPts tol x.points y.points                 
            ) 
        
        
    /// Position of points does not mattter,  they will be moved to origin,  but Rotation will not be checked.
    /// refTex is 
    let getSimliarityData (ptss:ResizeArray<string*ResizeArray<Pt>>) : SimilarityData =         
        let sptss = ptss |> ResizeArray.sortBy fst
        let mutable bb = BBox.create(sptss.[0] |> snd)
        for i=1 to sptss.Count-1 do  
            bb <- BBox.create(sptss.[i] |> snd) |> BBox.union bb
        let shift = Vc(-bb.MinX, -bb.MinY) 
        let ept = bb.MaxPt+shift 
        
        { 
        extend= ept
        groups=[|  
            for n, pts in sptss do  
                let ps = pts |> Array.ofSeq |> Array.map (Pt.addVc shift) 
                ps |> Array.sortInPlaceBy Pt.getX
                let bb = BBox.create(ps)
                { 
                typ=n
                bbox=bb
                points=ps
                }
            |]
        }
    
    /// This will group similar items together based on the SimilarityData at the same index.
    /// SimilarityData is precomputed for better performance.
    let getGrouped (tol,  items:ResizeArray<'T>, sims:ResizeArray<SimilarityData>) : ResizeArray<ResizeArray<'T>> =  
        if items.Count<>sims.Count then failwithf "Count missmatch in Similarity2D.getGrouped"
        let unique = ResizeArray<SimilarityData>()
        let groups = Dictionary<int, ResizeArray<'T>>() 
        for sid, it in Seq.zip sims items do  
            match unique|> ResizeArray.tryFindIndex (areSimliar tol sid) with
            | Some i -> 
                groups.[i].Add it
            | None -> 
                let r = ResizeArray()
                r.Add it                
                groups.[unique.Count] <- r
                unique.Add sid 
        
        let r = ResizeArray(groups.Count)
        for v in groups.Values do 
            r.Add v
        r
        
    
