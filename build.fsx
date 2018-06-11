// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r "./packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.AssemblyInfoFile
open System

// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------

let buildDirs  = !! "/**/bin" //!! "src/**/bin" ++ "tests/**/bin"
let appReferences = !! "/**/*.fsproj"
let testExecutables = !! "/**/bin/**/*Tests*.dll"
let dotnetcliVersion = DotNetCli.GetDotNetSDKVersionFromGlobalJson()
let mutable dotnetExePath = "dotnet"

let configuration = "Debug"

// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------

Target "Clean" (fun _ ->
    CleanDirs buildDirs
)

Target "InstallDotNetCLI" (fun _ ->
    dotnetExePath <- DotNetCli.InstallDotNetSDK dotnetcliVersion
)

Target "Restore" (fun _ ->
    appReferences
    |> Seq.iter (fun p ->
        let dir = System.IO.Path.GetDirectoryName p
        DotNetCli.Restore (fun p -> 
         { p with
              ToolPath = dotnetExePath
              WorkingDir = dir })
    )
)

Target "Build" (fun _ ->
    appReferences
    |> Seq.iter (fun p ->
        let dir = System.IO.Path.GetDirectoryName p
        DotNetCli.Build (fun p -> 
         { p with
              ToolPath = dotnetExePath
              WorkingDir = dir
              Configuration = configuration})
    )
)

Target "RunTests" (fun _ ->
    testExecutables
    |> Seq.iter (fun p ->
        let dir = System.IO.Path.GetDirectoryName p
        DotNetCli.RunCommand (fun p -> 
         { p with
              ToolPath = dotnetExePath
              WorkingDir = dir
               }) (sprintf "exec %s" p)
    )
)

Target "WriteAssemblyInfo" (fun _ ->
    CreateFSharpAssemblyInfo "./Busy/AssemblyInfo.fs"
      [ 
        Attribute.InternalsVisibleTo "Busy.Tests"]
)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "All" DoNothing

// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------

"Clean"
  ==> "InstallDotNetCLI"
  ==> "Restore"
  ==> "Build"
  ==> "RunTests"
  ==> "All"

RunTargetOrDefault "All"
