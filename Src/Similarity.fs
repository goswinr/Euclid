namespace FsEx.Geo

open System.Collections.Generic

/// For finding 2d object that are similar but not exactly the same 
/// Based on their 2D point clouds.
/// Within one list of points the order does not matter, but each location must exist only once 
/// in order to be consider similar within the tolerance.
/// (this could be extended to work in 3d too)
module Similarity2D =  
    
    /// The category is used to only compare groups of the same category.
    /// The bounding Rectangle of the points is used as a fast and first check for similarity.
    /// Within one list of points the order does not matter, but each location must exist only once 
    /// in order to be consider similar within the tolerance with another SimilaritySubGroup
    type SimilaritySubGroup =  { 
        category:string
        bRect:BRect
        points:Pt[] // must be sorted by 'X' property for binary search,  
        //duplicate points within tolerance will most likely lead to not recognized similarity (not all indices will be covered in simPts)
        } 
    
    type SimilarityMainGroup = {   
        extend: Pt // represents the max value of a bounding Rectangle ,  min value must be x0, y0
        groups: SimilaritySubGroup[] // must be sorted by 'category' property. for Array.forall2 function       
        } 
    
    /// Returns index of a similar point or -1.    
    /// Points Array ps[] must be sorted by 'X' property. 
    let internal simPt tol (ps:Pt[]) (pt:Pt)=     
        let x = pt.X
        let y = pt.Y
        // finds the index of a point where x matches,  there might be more than one match
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
            // x match found now search up and down if there is a Y match too
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
        // but two points from one set might also cover one single point from the other set so we need to check that all indices are covered: 
        && rs |> Array.forall id 
    
    /// Takes transformed and pre sorted by category main groups
    let areSimilar (tol:float) (a:SimilarityMainGroup) (b:SimilarityMainGroup) : bool = 
        let inline sim (a:Pt) (b:Pt)  = 
            abs(a.X - b.X) < tol && abs(a.Y - b.Y) < tol
        
        let inline simRect (a:BRect) (b:BRect)  = 
            sim a.MinPt b.MinPt    
            && sim a.MaxPt b.MaxPt    
        
        sim a.extend b.extend                               
        && a.groups.Length = b.groups.Length                
        && (a.groups , b.groups) ||> Array.forall2 (fun x y ->  
            x.category=y.category                                    
            && x.points.Length = x.points.Length            
            && simRect x.bRect y.bRect                         
            && simPts tol x.points y.points                 
            ) 
        
    
    /// The returned SimilarityMainGroup will have the subgroups sorted by category 
    /// and each point will be transformed by the the overall bounding Rectangle Min point to 0,0.
    /// Input Position of points does not matter,  they will be moved to origin by overall bounding Rectangle over all lists, 
    /// But any similarity that could be achieved by Rotation will not be discovered.
    /// The string is used as a unique category identifier.
    let getSimilarityData (ptss:ResizeArray<string*ResizeArray<Pt>>) : SimilarityMainGroup =         
        let sptss = ptss |> ResizeArray.sortBy fst
        // compute the overall bounding Rectangle and the shifting needed to move Rectangle to origin:
        let mutable bb = BRect.create(sptss.[0] |> snd)
        for i=1 to sptss.Count-1 do  
            bb <- BRect.create(sptss.[i] |> snd) |> BRect.union bb
        let shift = Vc(-bb.MinX, -bb.MinY) 
        let ept = bb.MaxPt+shift         
        { 
        extend= ept
        groups=[|  
            for n, pts in sptss do  
                let ps = pts |> Array.ofSeq |> Array.map (Pt.addVc shift) 
                ps |> Array.sortInPlaceBy Pt.getX
                let bb = BRect.create(ps)
                { 
                category=n
                bRect=bb
                points=ps
                }
            |]
        }
    
    /// This will group similar generic items together based on their SimilarityMainGroup.
    /// The list of items and precomputed SimilarityMainGroups must have the same length and correspond to each other at the same index.
    /// SimilarityMainGroups is precomputed for better performance.
    let getGrouped (tolerance,  items:ResizeArray<'T>, sims:ResizeArray<SimilarityMainGroup>) : ResizeArray<ResizeArray<'T>> =  
        if items.Count<>sims.Count then failwithf "Count mismatch in Similarity2D.getGrouped"
        let unique = ResizeArray<SimilarityMainGroup>()
        let groups = Dictionary<int, ResizeArray<'T>>() 
        for sid, it in Seq.zip sims items do  
            match unique|> ResizeArray.tryFindIndex (areSimilar tolerance sid) with
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
        
    
