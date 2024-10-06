namespace Euclid

open Euclid.UtilEuclid

/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type PPlane.
[<AutoOpen>]
module AutoOpenPPlane2 =

    // This file is only for extension members that depend on Line3D !!

    type PPlane with


        member inline pl.AsPlane = Plane.create(pl.Origin, pl.Zaxis)

        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------

