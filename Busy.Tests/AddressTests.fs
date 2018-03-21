module AddressTests

open Expecto
open Busy.Address

let createInvalidAddressTestCase name invalidValue =
    testCase name <| fun _ ->
          let subject = ParseAddress invalidValue
          let expected = InvalidAddress invalidValue
          Expect.equal subject expected name

[<Tests>]
let InvalidAddressTests =
    testList "ParsesInvalidAddress" [
        createInvalidAddressTestCase "Null address is marked invalid" null
        createInvalidAddressTestCase "Empty address is marked invalid" System.String.Empty
        createInvalidAddressTestCase "Whitespace address is marked invalid" "   "
        createInvalidAddressTestCase "Colon only address is marked invalid" ":"
        createInvalidAddressTestCase "Empty prefix address is marked invalid" ":name=123"
        createInvalidAddressTestCase "Too many colon address is marked invalid" "prefix:name:name2"
        createInvalidAddressTestCase "Invalid address is marked invalid" "abcd"
        createInvalidAddressTestCase "Invalid formatted address is marked invalid" "unix|path=/var/test"

        createInvalidAddressTestCase "Missing property key formatted address is marked invalid" "unix:=/var/test"
        createInvalidAddressTestCase "Missing '=' after property key formatted address is marked invalid" "unix:path"
        createInvalidAddressTestCase "Extra ',' after property formatted address is marked invalid" "unix:path=/tmp/dbus-test,"
        createInvalidAddressTestCase "Duplicate property formatted address is marked invalid" "unix:path=abc,path=bcd"
    ]

let createUnsupportedAddressTestCase name unsupportedValue =
    testCase name <| fun _ ->
          let subject = ParseAddress unsupportedValue
          let expected = UnsupportedAddress unsupportedValue
          Expect.equal subject expected name

[<Tests>]
let UnsupportedAddressTests =
    testList "ParsesUnsupportedAddress" [
        createUnsupportedAddressTestCase "Unknown prefix is marked unsupported" "unknown:name=123"
    ]    

[<Tests>]
let UnixDomainSocketAddressTests =
    testList "ParsesUnixDomainSocketAddress" [
        testCase "Path address is parsed correctly" <| fun _ ->
          let subject = ParseAddress "unix:path=/tmp/dbus-test"
          let expected = ValidAddress << UnixDomainSocketAddress << Map.ofSeq <| [("path","/tmp/dbus-test")]
          Expect.equal subject expected "Path address is parsed correctly"
    ]

[<Tests>]
let TcpSocketAddressTests =
    testList "ParsesTcpSocketAddress" [
        testCase "Path address is parsed correctly" <| fun _ ->
          let subject = ParseAddress "tcp:host=127.0.0.1,port=4242"
          let expected = ValidAddress << TcpSocketAddress  << Map.ofSeq <| [ ("host","127.0.0.1"); ("port","4242")]
          Expect.equal subject expected "Host+port address is parsed correctly"
    ]