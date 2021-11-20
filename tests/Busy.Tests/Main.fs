module Busy.Tests

open Expecto

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssemblyWithCLIArgs [NUnit_Summary "Busy.Tests.TestResults.xml"] argv