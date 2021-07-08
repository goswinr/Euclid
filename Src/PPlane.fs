namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike

/// A Parametrized Plane with X, Y and Z Direction
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
type PPlane =  
    val Origin: Pnt
    val Xax: UnitVec 
    val Yax: UnitVec 
    val Zax: UnitVec 
    
    /// Create a Parametrized Plane with X, Y and Z Direction
    new (origin,axisX,axisY, axisZ)  =  {Origin=origin; Xax=axisX; Yax=axisY; Zax=axisZ}
    
    override pl.ToString() = sprintf "PPlane(Origin=%O; Xax=%O; Yax=%O; Yax=%O;)" pl.Origin pl.Xax pl.Yax pl.Zax


    // see extension meber in folder members
