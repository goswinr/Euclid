namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike

/// A Parametrized Plane with X, Y and Z Direction
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
type PPlane =  
    val Pt: Pnt
    val Xax: UnitVec 
    val Yax: UnitVec 
    val Zax: UnitVec 
    
    /// Create a Parametrized Plane with X, Y and Z Direction
    new (pt,axisX,axisY, axisZ)  =  {Pt=pt; Xax=axisX; Yax=axisY; Zax=axisZ}
