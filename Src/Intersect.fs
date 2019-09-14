namespace FsEx.Geo


module Intersect =     
    
    open Util


    //[<Struct>]
    //type IntersectionParamter =
    //    |NoParamColinear
    //    |Param of float

    ///// Retuns the parameter on vector 'va' where 'va' and 'vb' intersect as endless rays.
    ///// If they start from points 'a' and 'b' respectivley.
    //let getParameter (a:Pt, va:Vc, b:Pt,  vb:Vc) =
    //    // https://www.youtube.com/watch?v=c065KoXooSw
    //    let nom = va.Cross vb
    //    if -zeroLenghtTol < nom && nom < zeroLenghtTol  then NoParamColinear
    //    else Param (((b-a).Cross(vb)) / nom)


    ///// Retuns the parameter on vector 'va' where 'va' and 'vb' intersect as endless rays.
    ///// If they start from points 'a' and 'b' respectivley.
    //let getParameterU (a:Pt, va:UnitVc , b :Pt ,  vb:UnitVc) =
    //    let nom = va.Cross vb
    //    if -zeroLenghtTol < nom && nom < zeroLenghtTol  then NoParamColinear
    //    else Param (((b-a).Cross(vb)) / nom)


    ///// Retuns the parameter on vector 'va' where 'va' and 'vb' intersect intersect as endless rays.
    ///// If they start from points 'a' and 'b' respectivley.
    ///// This might be infinity too. Does not check for colinear points !
    //let getParameterUnChecked (a:Pt, va:UnitVc, b:Pt,  vb:UnitVc) =
    //    ((b-a).Cross(vb)) / va.Cross vb
        
        
    /// Retuns the parameter on vector 'va' where 'va' and 'vb' intersect intersect as endless rays.
    /// If they start from points 'a' and 'b' respectivley.
    /// Pass in  va.Cross vb  is precomputed  and inverted  
    let inline private getXPara (a:Pt, vaXvbInverse:float, b:Pt,  vb:UnitVc) =
        // https://www.youtube.com/watch?v=c065KoXooSw
        ((b-a).Cross(vb)) * vaXvbInverse // va.Cross vb  is precomputed  and inverted 


    let inline private isParamStillBelowZeroAfterOffsets(ap:Pt, au:UnitVc, aXbInverse:float, bp:Pt, bu:UnitVc, snapThreshold:float) =
        let n = au.RotatedCCW * snapThreshold
        // TODO would it be enough to only compute one of these two? depending on the sign of aXbInverse ?
        getXPara(ap + n, aXbInverse,  bp, bu) < 0.0-snapThreshold
        &&
        getXPara(ap - n, aXbInverse,  bp, bu) < 0.0-snapThreshold

    let inline private isParamStillMoreThanLengthAfterOffsets(ap:Pt, au:UnitVc , aXbInverse:float, al:float, bp:Pt, bu:UnitVc, snapThreshold:float) =
        let n = au.RotatedCCW * snapThreshold
        getXPara(ap + n, aXbInverse,  bp, bu) > al+snapThreshold
        &&
        getXPara(ap - n, aXbInverse,  bp, bu) > al+snapThreshold


    type LineLineRelation =  
        // this DU could be also encoded via  Float NaN and infinity to avoid an extra object allocation
        |NoIntersection
        |Colinear // within threshold,  might still not  overlap,  needs to be checked via BBox
        |Parallel // more than threshold apart
        |BfromRight of struct ( float * float) // parameters for unit vector,  might be out of bound by snapThreshold
        |BfromLeft  of struct ( float * float) // parameters for unit vector,  might be out of bound by snapThreshold
    
    // TODO inline functions?

    /// For line A and line B give for each:
    /// Start point, unitized Direction, line length.
    /// And finally a tolerance: Curve A will be extendeded on both ends and offseted to both sides.
    /// These Offests will also be checked with curve B that is also extended by this amount.
    let getRelation (ap:Pt, au:UnitVc, al:float, bp:Pt, bu:UnitVc, bl:float, snapThreshold:float) :LineLineRelation=
        let aXb = au.Cross bu //precomputed  cross product 
        
        if abs(aXb) > zeroLenghtTol then  // not paralell
            let aXbInverse = 1./aXb // invert only once,  then pass on as inverted value
            let ta = getXPara (ap, aXbInverse, bp, bu)
        
            // parameter on first is below zero, so probably false unless closer than snapThreshold and almost colinear
            if ta < -snapThreshold && isParamStillBelowZeroAfterOffsets (ap, au, aXbInverse, bp, bu, snapThreshold) then   
                NoIntersection // no need to even check parameter on second segment

            // parameter on first segment is  beyond length ,  so probaly false unless closer than snapThreshold and colinear
            elif ta > al+snapThreshold && isParamStillMoreThanLengthAfterOffsets(ap, au, aXbInverse, al, bp, bu, snapThreshold) then   
                NoIntersection // no need to even check parameter on second segment

            // now checking if parameter on second line is inside too:
            else
                // this might still be a very shallow intersection that counts as pralell
                let bXaInverse = -aXbInverse
                let tb = getXPara (bp, bXaInverse, ap, au)

                // parameter on second segment is  below zero ,  so probaly false unless closer than snapThreshold and colinear
                if tb < -snapThreshold && isParamStillBelowZeroAfterOffsets (bp, bu, bXaInverse, ap, au, snapThreshold) then  
                    NoIntersection

                // parameter on second segment is  beyond length ,  so probaly false unless closer than snapThreshold and colinear
                elif tb > bl + snapThreshold && isParamStillMoreThanLengthAfterOffsets (bp, bu, bXaInverse, bl, ap, au, snapThreshold) then  
                    NoIntersection

                else  
                    if aXb > 0.0 then BfromRight (ta, tb)
                    else              BfromLeft  (ta, tb)

        else // Colinear
            // probaly no itersection  unless closer than snapThreshold
            let perp = au.RotatedCCW // unit v
            let vab = ap-bp
            let dot = perp * vab // project vab onto unit vector
            if abs dot < snapThreshold then
                Colinear // paralle distance is less than snapThreshold distance,  TODO but actual overlap needs to be confirmed via BBox
            else
                Parallel // paralle distance is more than snapThreshold distance,

    let doIntersect (ap:Pt, au:UnitVc, al:float, bp:Pt, bu:UnitVc, bl:float, snapThreshold:float) :bool =
        match getRelation(ap, au, al,  bp, bu, bl, snapThreshold)   with
        |NoIntersection -> false
        |Parallel       -> false
        |Colinear       -> true// TODO but actual overlap needs to be confirmed via BBox
        |BfromLeft _    -> true
        |BfromRight _   -> true
    
    
    type IntersectionPoint =
        |NoPoint
        |FromLeft  of Pt
        |FromRight of Pt
    
    /// Does also clamp point to actually be on line even if it is not quite in case of PreStart or PostEnd
    let getIntersectionPoint (ap:Pt, au:UnitVc, al:float, bp:Pt, bu:UnitVc, bl:float, snapThreshold:float) : IntersectionPoint =
        match getRelation(ap, au, al,  bp, bu, bl, snapThreshold)   with
        |NoIntersection
        |Colinear
        |Parallel            -> NoPoint
        |BfromLeft  (ta, _ ) -> FromLeft  (ap + au * (max 0.0 (min al ta))) // clamp point to actually be on line even if it is not quite in case of PreStart or PostEnd
        |BfromRight (ta, _ ) -> FromRight (ap + au * (max 0.0 (min al ta))) 
    
    
    /// Start and End means the intesection point is within the threshold distance of start or end point respectively
    /// PreStart and PostEnd means intesection point is beyond the threshold distance,  but an offest by threshold distance would intersect within this 
    type IntersectionZone =  
        PreStart | Start | Middle | End | PostEnd

    let getZone(t:float, len,  snapThreshold) =
        if    t < -snapThreshold     then PreStart
        elif  t <  snapThreshold     then Start
        elif  t <  len-snapThreshold then Middle
        elif  t <  len+snapThreshold then End
        else                              PostEnd

