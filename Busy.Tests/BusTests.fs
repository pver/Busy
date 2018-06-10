module BusTests

open Expecto
open Busy.Transport
open Busy
open Busy.Types
open Busy.ByteProviders
open Busy.MarshallingUtilities

let fakeTransportWithMessage msg = 
    let bytes = Marshalling.marshallMessage msg
    let arrayByteProvider = ArrayByteProvider bytes :> IByteProvider
    { new ITransport with
        member __.Close () = ()
        member __.Write (_:byte[]) = ()
        member __.ReadBytes count = arrayByteProvider.ReadBytes count }

let fakeRecordingTransport (recordedBytes:System.Collections.Generic.List<byte>) = 
    { new ITransport with
        member __.Close () = ()
        member __.Write (bytes:byte[]) = recordedBytes.AddRange bytes
        member __.ReadBytes _ = [||]}

[<Tests>]
let busTests =
    testList "busTests" [
        testCase "IBus.Transport should return passed in ITransport" <| fun _ ->
            
            let itransport = { new ITransport with
                                    member __.Close () = ()
                                    member __.Write (_:byte[]) = ()
                                    member __.ReadBytes _ = failwith "nothing to read here" }

            let bus = Bus(itransport) :> IBus

            Expect.equal itransport bus.Transport "Bus should store passed in ITransport"


        testCase "SendMessage should send correct msg bytes to transport" <| fun _ ->
            let msg = Busy.MessageFactory.CreateSignal 2ul 
                        "/org/freedesktop/DBus" "org.freedesktop.DBus" "NameAcquired" 
                        [|(Primitive <| String ":1.1")|] (Some "org.freedesktop.DBus") (Some ":1.1")
            
            let sendBytes = System.Collections.Generic.List<byte>()
            let itransport = fakeRecordingTransport sendBytes

            let bus = Bus(itransport) :> IBus
            bus.SendMessage(msg)

            let expectedBytes = Marshalling.marshallMessage msg
            let actualBytes = sendBytes.ToArray()
            Expect.equal actualBytes expectedBytes "Bus should send correct message bytes to ITransport"

        testCase "IterMessage should send transport retrieved message event" <| fun _ ->
            let msg = Busy.MessageFactory.CreateSignal 2ul 
                        "/org/freedesktop/DBus" "org.freedesktop.DBus" "NameAcquired" 
                        [|(Primitive <| String ":1.1")|] (Some "org.freedesktop.DBus") (Some ":1.1")
      
            let mutable actualMessageEventArgs = None
            let messageReceivedEventHandler = 
                (fun (eventArgs:DBusMessageReceivedEventArgs) -> actualMessageEventArgs<-Some(eventArgs) )

            let itransport = fakeTransportWithMessage msg

            let bus = Bus(itransport) :> IBus
            bus.DBusMessageReceived.Add messageReceivedEventHandler
            bus.IterateMessage()

            Expect.isSome actualMessageEventArgs "Bus should raise event on IterateMessage"
            Expect.equal actualMessageEventArgs.Value.Message msg "Bus should raise event with message read from transport"
    ]