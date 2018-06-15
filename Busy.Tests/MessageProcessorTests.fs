module MessageProcessorTests

open Expecto
open Busy.MessageProcessing
open Busy.MessageTypes
open Busy

[<Tests>]
let messageProcessorTests =
    testList "messageProcessorTests" [
        testCase "Processor should not fail on unknown message type" <| fun _ ->
            let processor = new MessageProcessor()
            
            let unkownMessageType = DBusMessageType.Signal + DBusMessageType.Signal // generating some unspecified message type
            let msgOfUnknownType = 
                MessageFactory.CreateError 1u "some" [||] None None 
                |> fun x -> { x with MessageType = unkownMessageType}

            Expect.equal (processor.Process msgOfUnknownType) None "Processor should not fail on unknown message type and return None"

        testCase "Processor should not fail on Invalid message type" <| fun _ ->
            let processor = new MessageProcessor()
            
            let invalidMsg = 
                MessageFactory.CreateError 1u "some" [||] None None 
                |> fun x -> { x with MessageType = DBusMessageType.Invalid}

            Expect.equal (processor.Process invalidMsg) None "Processor should not fail on Invalid message type and return None"

        testCase "Processor should deliver method return to PendingCall" <| fun _ ->
            let processor = new MessageProcessor()
            
            let methodCallSequenceNumber = 123u
            let methodReply = MessageFactory.CreateMethodReturn methodCallSequenceNumber [||] None None

            let pendingCall = processor.AddPendingCall methodCallSequenceNumber
            processor.Process methodReply |> fun x -> Expect.equal x None "processing method reply should not send back any message to dbus so should be None"
            Expect.equal (pendingCall.Result) (Ok methodReply) "Processor should deliver method return to PendingCall"

        testCase "Processor should deliver method error result to PendingCall" <| fun _ ->
            let processor = new MessageProcessor()
            
            let methodCallSequenceNumber = 123u
            let methodError = MessageFactory.CreateError methodCallSequenceNumber "Some error description" [||] None None

            let pendingCall = processor.AddPendingCall methodCallSequenceNumber
            processor.Process methodError |> fun x -> Expect.equal x None "processing method error result should not send back any message to dbus so should be None"
            Expect.equal (pendingCall.Result) (Ok methodError) "Processor should deliver method error result to PendingCall"
    ]