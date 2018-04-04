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
        createInvalidAddressTestCase "Duplicate property formatted address is marked invalid" "unix:path=abc,path=bcd"
    ]

let createUnsupportedAddressTestCase name unsupportedValue =
    testCase name <| fun _ ->
          let subject = ParseAddress unsupportedValue
          let expected = UnsupportedAddress unsupportedValue
          Expect.equal subject expected name

[<Tests>]
let GeneralAddressFormatTests =
    testList "GeneralAddressFormat" [
        testCase "Trailing address property ',' is ignored" <| fun _ ->
          let subject = ParseAddress "unix:path=/tmp/dbus-test,"
          let expected = ValidAddress << UnixDomainSocketAddress << Map.ofSeq <| [("path","/tmp/dbus-test")]
          Expect.equal subject expected "Trailing address property ',' is ignored"
        
        testCase "Leading address property ',' is ignored" <| fun _ ->
          let subject = ParseAddress "unix:,path=/tmp/dbus-test"
          let expected = ValidAddress << UnixDomainSocketAddress << Map.ofSeq <| [("path","/tmp/dbus-test")]
          Expect.equal subject expected "Leading address property ',' is ignored"

        testCase "Leading and trailing address property ',' is ignored" <| fun _ ->
          let subject = ParseAddress "unix:,path=/tmp/dbus-test,"
          let expected = ValidAddress << UnixDomainSocketAddress << Map.ofSeq <| [("path","/tmp/dbus-test")]
          Expect.equal subject expected "Leading and trailing address property ',' is ignored"

        testCase "Multiple addresses are parsed correctly" <| fun _ ->
          let subject = ParseAddress "unix:path=/abc;tcp:host=127.0.0.1,port=4242"
          let addr1 = ValidAddress << UnixDomainSocketAddress << Map.ofSeq <| [("path","/abc")]
          let addr2 = ValidAddress << TcpSocketAddress  << Map.ofSeq <| [ ("host","127.0.0.1"); ("port","4242")]
          let expected = ParseAddressResults <| [|addr1; addr2|]
          Expect.equal subject expected "Multiple addresses are parsed correctly"

        testCase "Multiple addresses leading ';' is ignored" <| fun _ ->
          let subject = ParseAddress ";unix:path=/abc;tcp:host=127.0.0.1,port=4242"
          let addr1 = ValidAddress << UnixDomainSocketAddress << Map.ofSeq <| [("path","/abc")]
          let addr2 = ValidAddress << TcpSocketAddress  << Map.ofSeq <| [ ("host","127.0.0.1"); ("port","4242")]
          let expected = ParseAddressResults <| [|addr1; addr2|]
          Expect.equal subject expected "Multiple addresses leading ';' is ignored"

        testCase "Multiple addresses trailing ';' is ignored" <| fun _ ->
          let subject = ParseAddress "unix:path=/abc;tcp:host=127.0.0.1,port=4242;"
          let addr1 = ValidAddress << UnixDomainSocketAddress << Map.ofSeq <| [("path","/abc")]
          let addr2 = ValidAddress << TcpSocketAddress  << Map.ofSeq <| [ ("host","127.0.0.1"); ("port","4242")]
          let expected = ParseAddressResults <| [|addr1; addr2|]
          Expect.equal subject expected "Multiple addresses trailing ';' is ignored"

        testCase "Multiple addresses leading and trailing ';' is ignored" <| fun _ ->
          let subject = ParseAddress ";unix:path=/abc;tcp:host=127.0.0.1,port=4242;"
          let addr1 = ValidAddress << UnixDomainSocketAddress << Map.ofSeq <| [("path","/abc")]
          let addr2 = ValidAddress << TcpSocketAddress  << Map.ofSeq <| [ ("host","127.0.0.1"); ("port","4242")]
          let expected = ParseAddressResults <| [|addr1; addr2|]
          Expect.equal subject expected "Multiple addresses leading and trailing ';' is ignored"
    ]

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

[<Tests>]
let AddressTests =
    testList "FilterValidAddressTests" [
        testCase "Filter valid address keeps all valid addresses" <| fun _ ->
          let addr1 = UnixDomainSocketAddress << Map.ofSeq <| [("path","/abc")]
          let addr2 = "my_invalid_address"
          let addr3 = TcpSocketAddress << Map.ofSeq <| [ ("host","127.0.0.1"); ("port","4242")]
          let addr4 = "my_unsupported_address"
          let addr1ParseResult = ValidAddress addr1
          let addr2ParseResult = InvalidAddress addr2
          let addr3ParseResult = ValidAddress addr3
          let addr4ParseResult = UnsupportedAddress addr4
          let parseResults = ParseAddressResults <| [|addr1ParseResult; addr2ParseResult; addr3ParseResult; addr4ParseResult|]

          let validAddresses = filterValidAddresses parseResults
          let expected = [|addr1; addr3|]
          Expect.equal validAddresses expected "Only valid addresses left"
    ] 