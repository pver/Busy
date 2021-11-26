module BusNameTests

open Expecto
open Busy.BusName

let createInvalidBusNameTestCase testCaseMsg busName (expectedError:ParseNameError) =
    testCase testCaseMsg <| fun _ ->
        let result = DBusName.ParseDBusName busName
        Expect.isFalse result.IsValid testCaseMsg
        match result with 
        | InvalidBusName (invalidName,err) -> 
            Expect.equal err expectedError (sprintf "%s - but error is different than expected" testCaseMsg)
            Expect.equal invalidName busName (sprintf "%s - but invalidname doesn't hold the original input value" testCaseMsg)
        | ValidBusName _ -> 
            failtest (sprintf "%s - but resulted to InvalidBusName" testCaseMsg)

[<Tests>]
let InvalidAddressTests =
    testList "ParsesInvalidAddress" [
        createInvalidBusNameTestCase "Null bus name is marked invalid" null EmptyBusName
        createInvalidBusNameTestCase "Empty bus name is marked invalid" System.String.Empty EmptyBusName
        createInvalidBusNameTestCase "Whitespace bus name is marked invalid" "   " EmptyBusName
    ]

let createValidBusNameTestCase testCaseMsg busName expectedValidBusName=
    testCase testCaseMsg <| fun _ ->
        let result = DBusName.ParseDBusName busName
        Expect.isTrue result.IsValid testCaseMsg
        match result with 
        | ValidBusName validName -> 
            Expect.equal validName expectedValidBusName (sprintf "%s - but validname is different than expected" testCaseMsg)
            Expect.equal validName.Value busName (sprintf "%s - but validname doesn't hold the original input value" testCaseMsg)
        | InvalidBusName _ -> 
            failtest (sprintf "%s - but resulted to InvalidBusName" testCaseMsg)

[<Tests>]
let ValidBusNameTests =
    testList "ParseValidBusNames" [
        createValidBusNameTestCase "Unique bus name is parsed correctly" ":1.86" (UniqueBusName ":1.86")
        createValidBusNameTestCase "Basic well-known bus name is parsed correctly" "my.busname" (WellKnownBusName "my.busname")
    ]