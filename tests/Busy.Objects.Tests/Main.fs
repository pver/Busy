module Busy.Objects.Tests

open Expecto

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssemblyWithCLIArgs [NUnit_Summary "Busy.Objects.Tests.TestResults.xml"] argv