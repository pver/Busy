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
            processor.Process methodReply |> fun x -> Expect.isNone x "processing method reply should not send back any message to dbus so should be None"
            Expect.equal (pendingCall.Result) (Ok methodReply) "Processor should deliver method return to PendingCall"

        testCase "Processor should deliver method error result to PendingCall" <| fun _ ->
            let processor = new MessageProcessor()
            
            let methodCallSequenceNumber = 123u
            let methodError = MessageFactory.CreateError methodCallSequenceNumber "Some error description" [||] None None

            let pendingCall = processor.AddPendingCall methodCallSequenceNumber
            processor.Process methodError |> fun x -> Expect.isNone x "processing method error result should not send back any message to dbus so should be None"
            Expect.equal (pendingCall.Result) (Ok methodError) "Processor should deliver method error result to PendingCall"

        testCase "Processor should not fail on method return with no PendingCall" <| fun _ ->
            let processor = new MessageProcessor()
            
            let methodCallSequenceNumber = 123u
            let methodReply = MessageFactory.CreateMethodReturn methodCallSequenceNumber [||] None None

            Expect.equal (processor.Process methodReply) None "processing method reply should not send back any message to dbus so should be None"

        testCase "Processor should not fail on method error result with no PendingCall" <| fun _ ->
            let processor = new MessageProcessor()
            
            let methodCallSequenceNumber = 123u
            let methodError = MessageFactory.CreateError methodCallSequenceNumber "Some error description" [||] None None

            Expect.equal (processor.Process methodError) None "processing method error result should not send back any message to dbus so should be None"

        testCase "Processor should deliver signal to SignalHandler" <| fun _ ->
            let processor = new MessageProcessor()
            
            let signal = MessageFactory.CreateSignal "/some/path" "some.interface" "Member" [||] None None
            let matchRule = 
                { Busy.MatchRules.MatchAllRule with 
                    Interface=Some("some.interface")
                    Member=Some("Member")
                    Path=Some( Busy.MatchRules.PathMatchRule.Path("/some/path") )}

            let handledSignals = new System.Collections.Generic.List<DBusMessage>()

            let signalHandler = new SignalHandler(matchRule, fun x -> handledSignals.Add x)
            
            processor.AddSignalHandler signalHandler
            processor.Process signal |> fun x -> Expect.isNone x "processing signal should not send back any message to dbus so should be None"
            
            Expect.equal (handledSignals.Count) 1 "Processor should deliver signal return to SignalHandler only once (when added)"

            handledSignals.Clear()
            processor.RemoveSignalHandler signalHandler
            processor.Process signal |> ignore
            
            Expect.equal (handledSignals.Count) 0 "Processor should not deliver signal return to SignalHandler once it's removed again"

        testCase "Processor should return error on unexported method" <| fun _ ->
            let processor = new MessageProcessor()
            
            let methodCall = MessageFactory.CreateMethodCall "/some/path" (Some "some.interface") "Member" [||] None None

            let response = processor.Process methodCall 
            Expect.isSome response "Method call without any exported object should generate error return"
            Expect.equal (response.Value.HeaderFields.ReplySerial) (Some methodCall.SequenceNumber) "Method call error should have call id as replyserial"

        testCase "Uncompleted PendingCall should return Error result" <| fun _ ->
            let call = new PendingCall(123u)

            Expect.isError call.Result "Uncompleted PendingCall should return Error result"

        testCase "SignalHandler should keep passed in MatchRule" <| fun _ ->
            let matchRule = 
                { Busy.MatchRules.MatchAllRule with 
                    Interface=Some("some.interface")
                    Member=Some("Member")
                    Path=Some( Busy.MatchRules.PathMatchRule.Path("/some/path") )}

            let signalHandler = new SignalHandler(matchRule, fun _ -> () )
            Expect.equal signalHandler.MatchRule matchRule "SignalHandler should keep passed in MatchRule"
    ]