namespace FsEx.Geo

/// A module for the result types of 2D and 3D Line-Line-intersections.
module LineIntersectionTypes = 

    /// For infinite 2D or 3D lines.
    /// The result line parameters from computing the intersection.
    [<Struct>]
    type IntersectionParam =  
        
        /// The infinite lines are intersecting (2D and 3D) or skew (3D only) 
        /// They have each one point where they are touching each other. ( Or are closest to each other. 3D only)
        /// Contains the parameters on the first and second line.
        | TwoParam of twoParams : struct(float*float)
        
        /// The lines are parallel, within the given tolerance.
        /// They have no points in common
        | Parallel 
        
        /// The lines are coincident or maybe even identical, within the given tolerance.
        /// As infinite lines they have infinitely many points in common.
        /// They might still not have the same start and end points in their finit definition.
        | Coincident

        /// Input line A is shorter than the given minimum Length tolerance.
        | TooShortA
        
        /// Input line B is shorter than the given minimum Length tolerance.
        | TooShortB
        
        /// Both input lines are shorter than the given minimum Length tolerance.
        | TooShortBoth

    /// For infinite 2D lines.
    /// The result from computing the intersection of two infinite 2D lines.
    [<Struct>]
    [<NoEquality; NoComparison>]
    type IntersectionPoints2D =  

        /// The points of 2D intersection.
        | Point of xPoint:Pt
        
        /// The lines are parallel within the given tolerance.
        | Parallel

        /// The lines are coincident or maybe even identical, within the given tolerance.
        /// As infinite lines they have infinitely many points in common.        
        | Coincident
        
        /// One or both input lines is shorter than the given minimum Length tolerance.
        | TooShort
    
    /// For infinite 3D lines.
    /// The result from computing the intersection of two infinite 3D lines.
     // [<Struct>]
    [<NoEquality; NoComparison>]
    type IntersectionPoints3D =  
        
        /// The lines are skew by mor than 1e-6. or the given tolerance
        /// Contains the points on the first and second 
        /// line where they are closest to each other.
        | TwoPoints of skewPoints : struct(Pnt*Pnt)

        /// The 3D lines are intersection in one point within the given tolerance.
        | OnePoint of xPoint : Pnt
        
        /// The lines are parallel within the given tolerance.
        | Parallel

        /// The lines are coincident or maybe even identical, within the given tolerance.
        /// As infinite lines they have infinitely many points in common.        
        | Coincident
        
        /// One or both input lines is shorter than the given minimum Length tolerance.
        | TooShort
    

    /// For finite 2D or 3D lines.
    /// An enumeration of all possible results from computing the intersection. 
    /// For finite lines there are much mor cases than for infinite lines
    /// General Cases:
    /// | Intersecting | IntersectingEndsBoth | IntersectingEndsFirst | IntersectingEndsSecond | Skew | Apart 
    /// Parallel Cases:
    /// | Parallel | Overlapping | CoincidentApart | Continuation | ContinuationFlipped| Identical| IdenticalFlipped
    /// Error Cases :
    /// TooShortA | TooShortB | TooShortBoth
    type IntersectionKind =

        /// The finite lines are intersecting each other in one point.        
        | Intersecting 

        /// The finite lines are intersecting each other at one of their end or start points point.        
        | IntersectingEndsBoth 
        
        /// The finite lines are intersecting. The first line is touching the second one with its end or start point.        
        | IntersectingEndsFirst 

        /// The finite lines are intersecting. The second line is touching the first one with its end or start point.        
        | IntersectingEndsSecond

        /// The finite lines are skew to each other.
        /// Their closest points to each other are within the line.
        /// The returned parameters are between 0.0 and 1.0
        | Skew 

        /// The finite lines are not intersecting nor skew.
        /// At least one of the parameters of closets points would be outside of  the range 0.0 and 1.0.
        /// The returned parameters  still indicate where the finite lines are closest to each other.
        | Apart        

        //------- Parallel and other special cases for finite lines: ---------------

        /// The finite lines are parallel.
        /// Within 0.25 degrees.
        /// The returned parameters are in the middle of their overlap, 
        /// or the two end points that are closest to each other.
        | Parallel 

        /// The lines are coincident,  overlapping and parallel within 0.25 degrees.
        /// The returned parameters are at start and end of overlap.
        | Overlapping 
        
        /// The Lines are coincident, parallel within 0.25 degrees. 
        /// But ends are apart. 
        /// The returned parameters still indicate where the lines are closest to each other.
        | CoincidentApart 

        /// The Lines are coincident, parallel within 0.25 degrees. 
        /// The ends are meeting in exactly one point.
        /// And Oriented the same way.
        /// The returned parameters indicate which ends these are.
        | Continuation 
        
        /// The Lines are coincident, parallel within 0.25 degrees.
        /// The ends are meeting in exactly one point.
        /// But orientation is flipped
        /// The returned parameters indicate which ends these are.
        | ContinuationFlipped

        /// The Lines are identical , in orientation too with in 1e-6 tolerance.
        /// The returned parameters still indicate where the lines start and end.
        | Identical

        /// The Lines are identical. But orientation is flipped
        /// The returned parameters still indicate where the lines start and end.
        | IdenticalFlipped

        //--------------------- Error cases: ----------------------------

        /// Input line A is shorter than the given minimum Length tolerance.
        | TooShortA
        
        /// Input line B is shorter than the given minimum Length tolerance.
        | TooShortB
        
        /// Both input lines are shorter than the given minimum Length tolerance.
        | TooShortBoth
        
    /// Return true if the IntersectionKind is represented by one Point. 
    /// Not two points like in skew or no point like in parallel.
    /// Also returns false for the special cases of too short lines.
    let isIntersectionOnePoint k =
        match k with        
        | Intersecting | IntersectingEndsBoth | IntersectingEndsFirst 
        | IntersectingEndsSecond  | Continuation | ContinuationFlipped -> true

        | Skew | Apart | IntersectionKind.Parallel  
        | Overlapping  | CoincidentApart | Identical| IdenticalFlipped
        |TooShortA |TooShortB | TooShortBoth-> false