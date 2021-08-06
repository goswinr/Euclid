namespace FsEx.Geo

open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike

/// A immutable Parametrized Plane  or Frame with X, Y and Z Direction
/// This struct is called 'PPlane' because 'Plane' refers to an unoriented plane consiting only of a Origin and a Z Axis.
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
type PPlane =  
    
    /// The Origin 3D Point of this PPlane  
    val Origin: Pnt
    
    /// The local X Axis of this PPlane  
    val Xax: UnitVec 
    
    /// The local Y Axis of this PPlane  
    val Yax: UnitVec 
    
    /// The local Z Axis of this PPlane  
    val Zax: UnitVec 
    
    /// Internal Only. Create a Parametrized Plane with X, Y and Z Direction
    internal new (origin, axisX, axisY, axisZ)  =  {Origin=origin; Xax=axisX; Yax=axisY; Zax=axisZ}
    
    /// Format PPlane into string with nicely formatted floating point numbers.
    override pl.ToString() = sprintf "FsEx.Geo.PPlane(Origin=%O; Xax=%O; Yax=%O; Yax=%O;)" pl.Origin pl.Xax pl.Yax pl.Zax

    // see extension mebers in folder 'members'
