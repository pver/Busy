module BusNameTests

open Expecto
open Busy.BusName

let createInvalidBusNameTestCase testCaseMsg invalidValue error =
    testCase testCaseMsg <| fun _ ->
        let subject = DBusName.ParseDBusName invalidValue
        let expected = InvalidBusName (invalidValue, error)
        Expect.equal subject expected testCaseMsg

[<Tests>]
let InvalidAddressTests =
    testList "ParsesInvalidAddress" [
        createInvalidBusNameTestCase "Null bus name is marked invalid" null EmptyBusName
        createInvalidBusNameTestCase "Empty bus name is marked invalid" System.String.Empty EmptyBusName
        createInvalidBusNameTestCase "Whitespace bus name is marked invalid" "   " EmptyBusName
    ]

let createValidBusNameTestCase testCaseMsg busName parsedDBusName=
    testCase testCaseMsg <| fun _ ->
        let subject = DBusName.ParseDBusName busName
        let expected = ValidBusName parsedDBusName
        Expect.equal subject expected testCaseMsg

[<Tests>]
let ValidBusNameTests =
    testList "ParseValidBusNames" [
        createValidBusNameTestCase "Unique bus name is parsed correctly" ":1.86" (UniqueBusName ":1.86")
        createValidBusNameTestCase "Basic well-known bus name is parsed correctly" "my.busname" (WellKnownBusName "my.busname")
    ]