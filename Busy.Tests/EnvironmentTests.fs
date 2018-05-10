module EnvironmentTests

open Expecto
open Busy.Address
open Busy.Environment

let private envVariableTestLock = new System.Object() // to avoid parallel test execution by Expecto influences env variables simultanuously

let private setCallRestore addressVariableName addressVariableTestValue getAddressFn =
    lock envVariableTestLock 
        ( fun () -> 
              let backupEnvVariable = System.Environment.GetEnvironmentVariable addressVariableName
              System.Environment.SetEnvironmentVariable(addressVariableName, addressVariableTestValue)
              let result = getAddressFn()
              System.Environment.SetEnvironmentVariable(addressVariableName, backupEnvVariable)
              result
        )

[<Tests>]
let SystemBusTests =
    testList "SystemBusTests" [
        testCase "No configured environment variable results in default system bus address" <| fun _ ->
            let parseResult = setCallRestore systemBusAddressVariableName "" SystemBusAddress

            let expectedAddress = UnixDomainSocketAddress << Map.ofSeq <| [("path","/var/run/dbus/system_bus_socket")]
            let expectedParseResult = (ParseAddressResult.ValidAddress expectedAddress) 
            Expect.equal parseResult expectedParseResult "No configured environment variable should result in default system bus address"

        testCase "Configured environment variable should be reflected in system bus address" <| fun _ ->
            let parseResult = setCallRestore systemBusAddressVariableName "unix:path=/abcdefg" SystemBusAddress

            let expectedAddress = UnixDomainSocketAddress << Map.ofSeq <| [("path","/abcdefg")]
            let expectedParseResult = (ParseAddressResult.ValidAddress expectedAddress) 
            Expect.equal parseResult expectedParseResult "Configured environment variable should be reflected in system bus address"
    ]

[<Tests>]
let StarterAddressesTests =
    testList "StarterBusTests" [
        testCase "No configured environment variable results in None starter bus address" <| fun _ ->
            let parseResult = setCallRestore starterBusAddressVariableName "" StarterBusAddress

            Expect.isNone parseResult "No configured environment variable should result in None starter bus address"

        testCase "No configured environment variable results in Unknown starter bus type" <| fun _ ->
            let busTypeEmpty = setCallRestore starterBusTypeVariableName "" StarterBusType
            
            Expect.equal busTypeEmpty (StarterBusUnknown "") "No configured environment variable should result in Unknown starter bus type"

        testCase "Unknown configured environment variable results in Unknown starter bus type" <| fun _ ->
            let busType = setCallRestore starterBusTypeVariableName "abcdefg" StarterBusType
            
            Expect.equal busType (StarterBusUnknown "abcdefg") "Unknown configured environment variable should result in Unknown starter bus type"

        testCase "Configured environment variable should be reflected in starter bus type" <| fun _ ->
            let busTypeSystem = setCallRestore starterBusTypeVariableName "system" StarterBusType
            let busTypeSession = setCallRestore starterBusTypeVariableName "session" StarterBusType

            Expect.equal busTypeSystem (StarterBusSystem) "Configured environment variable 'system' should be reflected in starter bus type"
            Expect.equal busTypeSession (StarterBusSession) "Configured environment variable 'session' should be reflected in starter bus type"
    ]