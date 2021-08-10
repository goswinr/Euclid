namespace FsEx.Geo

// Design notes:
// The structs types in this file only have the constructors , ToString override and operators define in this file. 
// For structs that need a checked and unchecked constructor ( like unit vectors) the main 'new' constructor is marked obsolete. 
// A 'create' and 'createUnchecked' static member is provided instead.
// All other members are implemented as extension members. see files in folder members.
// This design however makes extension members unaccessible from see C#. To fix this all types and all members could be put into one file.
// the types would have to be marked as recursive. This file would be very large and probably have bad editor performance. 

open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike

/// A immutable Parametrized Plane  or Frame with X, Y and Z Direction
/// This struct is called 'PPlane' because 'Plane' refers to an un-oriented plane consisting only of a Origin and a Z-axis.
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
type PPlane =  
    
    /// The Origin 3D point of this PPlane  
    val Origin: Pnt
    
    /// The local X-axis of this PPlane  
    val Xax: UnitVec 
    
    /// The local Y-axis of this PPlane  
    val Yax: UnitVec 
    
    /// The local Z-axis of this PPlane  
    val Zax: UnitVec 
    
    /// Internal Only. Create a Parametrized Plane with X, Y and Z Direction
    internal new (origin, axisX, axisY, axisZ)  =  {Origin=origin; Xax=axisX; Yax=axisY; Zax=axisZ}
    
    /// Format PPlane into string with nicely formatted floating point numbers.
    override pl.ToString() = sprintf "FsEx.Geo.PPlane(Origin:%s| X-ax:%s| Y-ax=%s| Z-ax:%s)" pl.Origin.AsShortString pl.Xax.AsShortString pl.Yax.AsShortString pl.Zax.AsShortString

    // see extension members in folder 'members'
