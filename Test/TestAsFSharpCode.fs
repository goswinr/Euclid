module TestAsFSharpCode

open Euclid
open System
open System.IO

open EuclidTestSupport


let tests =
    testList "AsFSharpCode" [

        testCase "Float expressions round-trip without losing precision" <| fun _ ->
            let values =
                [ 0.0; -0.0; 1.0; -1.0; 0.1; Math.PI
                  0.6822871999174; 1.0000000000000002
                  Double.Epsilon; -Double.Epsilon
                  Double.MaxValue; Double.MinValue; 1e-200; 1e200 ]
            for x in values do
                let code = Format.floatAsFSharpCode x
                let expected = if x = 0.0 then 0.0 else x
                #if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
                let parsed : float = Fable.Core.JsInterop.emitJsExpr code "Number($0)"
                let same : bool = Fable.Core.JsInterop.emitJsExpr (expected, parsed) "Object.is($0, $1)"
                #else
                let parsed = Double.Parse(code, Globalization.CultureInfo.InvariantCulture)
                let same = BitConverter.DoubleToInt64Bits expected = BitConverter.DoubleToInt64Bits parsed
                #endif
                Expect.isTrue same $"Float expression {code} must preserve the original value"

        testCase "Float expressions use F# literals and special values" <| fun _ ->
            Expect.equal (Format.floatAsFSharpCode 42.0) "42.0" "Integral float literal"
            Expect.equal (Format.floatAsFSharpCode -0.0) "0.0" "Negative zero is normalized"
            Expect.equal (Format.floatAsFSharpCode Double.NaN) "System.Double.NaN" "NaN expression"
            Expect.equal (Format.floatAsFSharpCode Double.PositiveInfinity) "System.Double.PositiveInfinity" "Positive infinity expression"
            Expect.equal (Format.floatAsFSharpCode Double.NegativeInfinity) "System.Double.NegativeInfinity" "Negative infinity expression"

        #if !FABLE_COMPILER
        testCase "AsFSharpCode is invariant under Austrian culture" <| fun _ ->
            let previous = Globalization.CultureInfo.CurrentCulture
            try
                Globalization.CultureInfo.CurrentCulture <- Globalization.CultureInfo.GetCultureInfo "de-AT"
                Expect.equal (Pnt(1.5, 2.5, 3.5).AsFSharpCode) "Pnt(1.5, 2.5, 3.5)" "Point uses decimal dots"
                Expect.equal
                    (BBox.createUnchecked(1.5, 2.5, 3.5, 4.5, 5.5, 6.5).AsFSharpCode)
                    "BBox.createUnchecked(1.5, 2.5, 3.5, 4.5, 5.5, 6.5)"
                    "Bounding box uses decimal dots"
            finally
                Globalization.CultureInfo.CurrentCulture <- previous
        #endif

        testCase "Verify AsFSharpCode generates valid F# syntax" <| fun _ ->
            let mutable result = true


            #if !FABLE_COMPILER
            // Create instances of primitive types
            let pt = Pt(1.5, 2.5)
            let pnt = Pnt(1.5, 2.5, 3.5)
            let vc = Vc(1.0, 2.0)
            let vec = Vec(1.0, 2.0, 3.0)
            let unitVc = UnitVc.create 1.0 0.0
            let unitVec = UnitVec.create 1.0 0.0 0.0
            let line2D = Line2D(0.0, 0.0, 3.0, 4.0)
            let line3D = Line3D(0.0, 0.0, 0.0, 3.0, 4.0, 5.0)
            let bbox = BBox.createCentered 1 2 3 (Pnt(5.0, 5.0, 5.0))
            let brect = BRect.createCentered 2 3 (Pt(5.0, 5.0))
            let matrix = Matrix.identity
            let nplane = NPlane(Pnt.Origin, UnitVec.zAxis)
            let pplane = PPlane.createFromUnitNormal UnitVec.zAxis Pnt.Origin
            let rect2D = Rect2D.createFromVcs (Vc(5.0, 0.0)) (Vc(0.0, 6.0)) Pt.Origin
            let rect3D = Rect3D.createFromVecs (Vec(5.0, 0.0, 0.0)) (Vec(0.0, 6.0, 0.0)) Pnt.Origin
            let quat = Quaternion.createFromVecs Vec.xAxis vec
            let rigidMatrix = RigidMatrix.createFromQuaternion(quat)
            let rotation2D = Rotation2D.createFromRadians(Math.PI / 4.0)
            let freeBox = FreeBox.createFromBox (Box.createFromBounds(-1.5, 2.0, 3.0, 4.0, 5.5, 6.0))

            let codeStr =
                [
                """#r "./Test/bin/Debug/net10.0/Euclid.dll" """
                "open Euclid"
                "open System"
                "["
                $"Pt.equalsExact ({pt.AsFSharpCode}) (Pt(1.5, 2.5))"
                $"Pnt.equalsExact ({pnt.AsFSharpCode}) (Pnt(1.5, 2.5, 3.5))"
                $"Vc.equalsExact ({vc.AsFSharpCode}) (Vc(1.0, 2.0))"
                $"Vec.equalsExact ({vec.AsFSharpCode}) (Vec(1.0, 2.0, 3.0))"
                $"UnitVc.equalsExact ({unitVc.AsFSharpCode}) (UnitVc.create 1.0 0.0)"
                $"UnitVec.equalsExact ({unitVec.AsFSharpCode}) (UnitVec.create 1.0 0.0 0.0)"
                $"UnitVc.equalsExact ({(-unitVc).AsFSharpCode}) (-(UnitVc.create 1.0 0.0))"
                $"UnitVec.equalsExact ({(-unitVec).AsFSharpCode}) (-(UnitVec.create 1.0 0.0 0.0))"
                $"Line2D.equalsExact ({line2D.AsFSharpCode}) (Line2D(0.0, 0.0, 3.0, 4.0))"
                $"Line3D.equalsExact ({line3D.AsFSharpCode}) (Line3D(0.0, 0.0, 0.0, 3.0, 4.0, 5.0))"
                $"BBox.equalsExact ({bbox.AsFSharpCode}) (BBox.createCentered 1.0 2.0 3.0 (Pnt(5.0, 5.0, 5.0)))"
                $"BRect.equalsExact ({brect.AsFSharpCode}) (BRect.createCentered 2.0 3.0 (Pt(5.0, 5.0)))"
                $"Matrix.equalsExact ({matrix.AsFSharpCode}) (Matrix.identity)"
                $"NPlane.equalsExact ({nplane.AsFSharpCode}) (NPlane(Pnt.Origin, UnitVec.zAxis))"
                $"PPlane.equalsExact ({pplane.AsFSharpCode}) (PPlane.createFromUnitNormal UnitVec.zAxis Pnt.Origin)"
                $"Rect2D.equalsExact ({rect2D.AsFSharpCode}) (Rect2D.createFromVcs (Vc(5.0, 0.0)) (Vc(0.0, 6.0)) Pt.Origin)"
                $"Rect3D.equalsExact ({rect3D.AsFSharpCode}) (Rect3D.createFromVecs (Vec(5.0, 0.0, 0.0)) (Vec(0.0, 6.0, 0.0)) Pnt.Origin)"
                $"Quaternion.equalsExact ({quat.AsFSharpCode}) (Quaternion.createFromVecs Vec.xAxis (Vec(1.0, 2.0, 3.0)))"
                $"RigidMatrix.equalsExact ({rigidMatrix.AsFSharpCode}) (RigidMatrix.createFromQuaternion(Quaternion.createFromVecs Vec.xAxis (Vec(1.0, 2.0, 3.0))))"
                $"Rotation2D.equalsExact ({rotation2D.AsFSharpCode}) (Rotation2D.createFromRadians(Math.PI / 4.0))"
                $"Pnt.equalsExact (({freeBox.AsFSharpCode}).Pnt6) (Pnt(4.0, 5.5, 6.0))"
                "]"
                "|> List.iteri (fun i x ->"
                "    if not x then"
                """        failwithf $"Test case {i} failed" )"""


                ]
                |> String.concat Environment.NewLine

            let tempFile = Environment.CurrentDirectory + "/AsFSharpCode-test-temp.fsx"
            try
                try
                    File.WriteAllText(tempFile, codeStr)
                    let startInfo = System.Diagnostics.ProcessStartInfo()
                    startInfo.FileName <- "dotnet"
                    startInfo.Arguments <- $"fsi \"{tempFile}\""
                    startInfo.RedirectStandardOutput <- true
                    startInfo.RedirectStandardError <- true
                    startInfo.UseShellExecute <- false
                    startInfo.CreateNoWindow <- true

                    use proc = System.Diagnostics.Process.Start(startInfo)
                    let output = proc.StandardOutput.ReadToEnd()
                    let error = proc.StandardError.ReadToEnd()
                    proc.WaitForExit()

                    if proc.ExitCode <> 0 then
                        eprintfn $"FSI error: \n{error} \n{codeStr}\n"
                        result <- false
                    else
                        result <- true
                with
                | ex ->
                    eprintfn $"Exception during FSI execution: {ex.Message}\n{codeStr}"
                    result <- false
            finally
                // printfn $"Deleting temporary file: {tempFile}"
                if File.Exists(tempFile) then File.Delete(tempFile)

            #endif

            Expect.isTrue result $"AsFSharpCode generated invalid code"


    ]
