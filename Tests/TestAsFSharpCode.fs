module TestAsFSharpCode

open Euclid
open System
open System.IO

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif


let tests =
    testList "AsFSharpCode" [

        testCase "Verify AsFSharpCode generates valid F# syntax" <| fun _ ->
            let mutable result = true


            #if !FABLE_COMPILER
            // Create instances of primitive types
            let pt = Pt(1.5, 2.5)
            let pnt = Pnt(1.5, 2.5, 3.5)
            let vc = Vc(1.0, 2.0)
            let vec = Vec(1.0, 2.0, 3.0)
            let unitVc = UnitVc.create(1.0, 0.0)
            let unitVec = UnitVec.create(1.0, 0.0, 0.0)
            let line2D = Line2D(0.0, 0.0, 3.0, 4.0)
            let line3D = Line3D(0.0, 0.0, 0.0, 3.0, 4.0, 5.0)
            let bbox = BBox.createFromCenter(Pnt(5.0, 5.0, 5.0), 1,2,3)
            let brect = BRect.createFromCenter(Pt(5.0, 5.0), 2,3)
            let matrix = Matrix.identity
            let nplane = NPlane(Pnt.Origin, UnitVec.Zaxis)
            let pplane = PPlane.createOriginNormal(Pnt.Origin, UnitVec.Zaxis)
            let rect2D = Rect2D.createFromVectors(Pt.Origin, Vc(5.0, 0.0), Vc(0.0, 6.0))
            let rect3D = Rect3D.createFromVectors(Pnt.Origin, Vec(5.0, 0.0, 0.0), Vec(0.0, 6.0, 0.0))
            let quat = Quaternion.createVecToVec(Vec.Xaxis, vec)
            let rigidMatrix = RigidMatrix.createFromQuaternion(quat)
            let rotation2D = Rotation2D.createFromRadians(Math.PI / 4.0)

            let codeStr =
                [
                """#r "./Tests/bin/Debug/net8.0/Euclid.dll" """
                "open Euclid"
                "open System"
                "["
                $"Pt.equals 0.0 ({pt.AsFSharpCode}) (Pt(1.5, 2.5))"
                $"Pnt.equals 0.0 ({pnt.AsFSharpCode}) (Pnt(1.5, 2.5, 3.5))"
                $"Vc.equals 0.0 ({vc.AsFSharpCode}) (Vc(1.0, 2.0))"
                $"Vec.equals 0.0 ({vec.AsFSharpCode}) (Vec(1.0, 2.0, 3.0))"
                $"UnitVc.equals 0.0 ({unitVc.AsFSharpCode}) (UnitVc.create(1.0, 0.0))"
                $"UnitVec.equals 0.0 ({unitVec.AsFSharpCode}) (UnitVec.create(1.0, 0.0, 0.0))"
                $"Line2D.equals 0.0 ({line2D.AsFSharpCode}) (Line2D(0.0, 0.0, 3.0, 4.0))"
                $"Line3D.equals 0.0 ({line3D.AsFSharpCode}) (Line3D(0.0, 0.0, 0.0, 3.0, 4.0, 5.0))"
                $"BBox.equals 0.0 ({bbox.AsFSharpCode}) (BBox.createFromCenter(Pnt(5.0, 5.0, 5.0), 1,2,3))"
                $"BRect.equals 0.0 ({brect.AsFSharpCode}) (BRect.createFromCenter(Pt(5.0, 5.0), 2,3))"
                $"Matrix.equals 0.0 ({matrix.AsFSharpCode}) (Matrix.identity)"
                $"NPlane.equals 0.0 ({nplane.AsFSharpCode}) (NPlane(Pnt.Origin, UnitVec.Zaxis))"
                $"PPlane.equals 0.0 ({pplane.AsFSharpCode}) (PPlane.createOriginNormal(Pnt.Origin, UnitVec.Zaxis))"
                $"Rect2D.equals 0.0 ({rect2D.AsFSharpCode}) (Rect2D.createFromVectors(Pt.Origin, Vc(5.0, 0.0), Vc(0.0, 6.0)))"
                $"Rect3D.equals 0.0 ({rect3D.AsFSharpCode}) (Rect3D.createFromVectors(Pnt.Origin, Vec(5.0, 0.0, 0.0), Vec(0.0, 6.0, 0.0)))"
                $"Quaternion.equals 0.0 ({quat.AsFSharpCode}) (Quaternion.createVecToVec(Vec.Xaxis, Vec(1.0, 2.0, 3.0)))"
                $"RigidMatrix.equals 0.0 ({rigidMatrix.AsFSharpCode}) (RigidMatrix.createFromQuaternion(Quaternion.createVecToVec(Vec.Xaxis, Vec(1.0, 2.0, 3.0))))"
                $"Rotation2D.equals 0.0 ({rotation2D.AsFSharpCode}) (Rotation2D.createFromRadians(Math.PI / 4.0))"
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
                printfn $"Deleting temporary file: {tempFile}"
                if File.Exists(tempFile) then File.Delete(tempFile)
            #endif

            Expect.isTrue result $"AsFSharpCode generated invalid code"


    ]
