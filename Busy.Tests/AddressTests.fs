module AddressTests

open Expecto
open Busy.Address

[<Tests>]
let UnixDomainSocketAddressTests =
    testList "ParsesUnixDomainSocketAddress" [
        testCase "Path address is parsed correctly" <| fun _ ->
          let subject = ParseAddress "unix:path=/tmp/dbus-test"
          let expected = UnixDomainSocketAddress "path=/tmp/dbus-test"
          Expect.equal subject expected "Path address is parsed correctly"
    ]

[<Tests>]
let TcpSocketAddressTests =
    testList "ParsesTcpSocketAddress" [
        testCase "Path address is parsed correctly" <| fun _ ->
          let subject = ParseAddress "tcp:host=127.0.0.1,port=4242"
          let expected = TcpSocketAddress "host=127.0.0.1,port=4242"
          Expect.equal subject expected "Host+port address is parsed correctly"
    ]