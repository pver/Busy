module BusTests

open Expecto
open Busy.Transport
open Busy
open Busy.Types
open Busy.ByteProviders
open Busy.MarshallingUtilities
open Busy.MessageProcessing
open System.Threading

let byteBasedTransport bytes =
    let arrayByteProvider = ArrayByteProvider bytes :> IByteProvider
    { new ITransport with
        member __.Close () = ()
        member __.Write (_:byte[]) = ()
        member __.ReadBytes count = arrayByteProvider.ReadBytes count }

let fakeTransportWithMessage msg = 
    Marshalling.marshallMessage msg
    |> byteBasedTransport

let fakeRecordingTransport (recordedBytes:System.Collections.Generic.List<byte>) = 
    { new ITransport with
        member __.Close () = ()
        member __.Write (bytes:byte[]) = recordedBytes.AddRange bytes
        member __.ReadBytes _ = [||]}

let fakeRecordingTransportWithMessageReply (recordedBytes:System.Collections.Generic.List<byte>) msg = 
    let responseBytes = Marshalling.marshallMessage msg
    let arrayByteProvider = ArrayByteProvider responseBytes :> IByteProvider

    let responseGate = new ManualResetEventSlim()

    { new ITransport with
        member __.Close () = ()
        member __.Write (bytes:byte[]) = 
            recordedBytes.AddRange bytes
            responseGate.Set()
        member __.ReadBytes count =
            responseGate.Wait() 
            arrayByteProvider.ReadBytes count }


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
            let msg = Busy.MessageFactory.CreateSignal 
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
            let msg = Busy.MessageFactory.CreateSignal 
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

        testCase "IterateMessage should not fail when unmarshalling message fails" <| fun _ ->
            let itransport = byteBasedTransport [|123uy|] // results in marshalling error
            let bus = Bus(itransport) :> IBus

            bus.IterateMessage() // => shouldn't throw

        testCase "Removing unkown SignalHandler should not fail" <| fun _ ->
            let itransport = byteBasedTransport [||]
            let bus = Bus(itransport) :> IBus
            
            let matchRule = 
                { Busy.MatchRules.MatchAllRule with 
                    Interface=Some("some.interface")
                    Member=Some("Member")
                    Path=Some( Busy.MatchRules.PathMatchRule.Path("/some/path") )}

            new SignalHandler(matchRule, fun _ -> () )
            |> bus.RemoveSignalHandler

        testCase "SendAndWait should send message and return correct result" <| fun _ ->
            let recordedBytes = System.Collections.Generic.List<byte>()
            
            let methodCall = MessageFactory.CreateMethodCall "/some/path" (Some "my.interface") "Member" [||] None None
            let methodResponse = MessageFactory.CreateMethodReturn methodCall.SequenceNumber [||] None None
            
            let itransport = fakeRecordingTransportWithMessageReply recordedBytes methodResponse
            let bus = Bus(itransport) :> IBus

            async {
                
                bus.IterateMessage() 
            } |> Async.StartAsTask |> ignore

            let response = bus.SendAndWait methodCall
            Expect.equal response (Ok methodResponse) "SendAndWait should send message and return correct result"
            
    ]