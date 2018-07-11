module UnixDomainSocketTests

open Expecto
open Busy.UnixDomainSocket
open System.Net.Sockets

[<Tests>]
let tests =
    testList "UnixDomainSocketEndpointTests" [
        testCase "UnixDomainSocketEndpoint should use Unix address family" <| fun _ ->
            let endpoint = new UnixDomainSocketEndpoint("/tmp/dbus-test")
            Expect.equal endpoint.AddressFamily AddressFamily.Unix "UnixDomainSocketEndpoint should use Unix address family"

        testCase "UnixDomainSocketEndpoint should use Unix address family in serialized SocketAddr" <| fun _ ->
            let endpoint = new UnixDomainSocketEndpoint("/tmp/dbus-test")
            
            let serializedEndpoint = endpoint.Serialize()

            Expect.equal endpoint.AddressFamily serializedEndpoint.Family "UnixDomainSocketEndpoint serialization should use Unix address family"
            Expect.isGreaterThan serializedEndpoint.Size 2 "UnixDomainSocketEndpoint serialization should at least contain address family prefix"
            Expect.equal [|serializedEndpoint.[0]; serializedEndpoint.[1]|] [|0x1uy; 0x0uy|]  "UnixDomainSocketEndpoint serialization should use Unix address family prefix bytes"

        testCase "UnixDomainSocketEndpoint deserialize following serialize should result in original address" <| fun _ ->
            let socketAddress = "/tmp/dbus-test"
            
            let originalEndpoint = new UnixDomainSocketEndpoint(socketAddress)

            let serializedEndpoint = originalEndpoint.Serialize()
            let deserializeEndPoint = (new UnixDomainSocketEndpoint("")).Create(serializedEndpoint)

            Expect.isTrue (deserializeEndPoint :? UnixDomainSocketEndpoint) "UnixDomainSocketEndpoint deserialize should result in UnixDomainSocketEndpoint type"

            let castedDeserializedEndPoint = (downcast deserializeEndPoint : UnixDomainSocketEndpoint)
            Expect.equal castedDeserializedEndPoint.DomainSocketAddres (originalEndpoint.DomainSocketAddres)  "UnixDomainSocketEndpoint deserialize should match original UnixDomainSocketEndpoint"
    ]