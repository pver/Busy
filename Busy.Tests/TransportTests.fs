module TransportTests

open Expecto
open Busy.Transport
open Busy.Address

[<Tests>]
let tests =
    testList "UnixDomainSocketTransportTests" [
        testCase "UnixDomainSocketTransport Close when not connected should not fail" <| fun _ ->
            let transport = new UnixDomainSocketTransport(Map.empty) :> ITransport
            transport.Close()

        testCase "UnixDomainSocketTransport Read with negative length" <| fun _ ->
            let transport = new UnixDomainSocketTransport(Map.empty) :> ITransport
            let bytes = transport.ReadBytes -1
            Expect.equal bytes [||] "UnixDomainSocketTransport Read with negative length should not fail just return empty array"
        
        testCase "UnixDomainSocketTransport Read with zero length" <| fun _ ->
            let transport = new UnixDomainSocketTransport(Map.empty) :> ITransport
            let bytes = transport.ReadBytes 0
            Expect.equal bytes [||] "UnixDomainSocketTransport Read with zero length should not fail just return empty array"

        testCase "TransportFromAddress with valid unix domain socket address" <| fun _ ->
            let address = UnixDomainSocketAddress << Map.ofSeq <| [("path","/tmp/dbus-test")]
            let transportCreation = FromAddress address
            
            Expect.isOk transportCreation "UnixDomainSocketTransport creation should be succesfull with valid unix address"
            
            match transportCreation with
            | Ok transport -> Expect.isTrue (transport :? UnixDomainSocketTransport) "TransportFromAddress with valid unix domain socket address should create UnixDomainSocketTransport"
            | Error e -> failwith e.CreateTransportErrorMessage

            

    ]