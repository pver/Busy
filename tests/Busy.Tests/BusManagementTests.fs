module BusManagementTests

open Busy
open Busy.Types
open Busy.MessageTypes

open Expecto
open Busy.BusManagement

let fakeRecordingBus (recordedMessages:System.Collections.Generic.List<DBusMessageBody>) (responseMessages:array<DBusMessageBody>) = 
    let dbusMessageReceived = new DelegateEvent<System.EventHandler<DBusMessageReceivedEventArgs>>()
    let mutable responseCounter = 0
    { new IBus with
        member __.SendMessage (msg:DBusMessage) = 
            recordedMessages.Add msg.Body
        member __.Transport = failwith "not implemented"
        member __.IterateMessage () = ()
        member __.SendAndWait (msg:DBusMessage) = 
            recordedMessages.Add msg.Body
            let responseBody = responseMessages.[responseCounter]
            responseCounter <- responseCounter + 1
            Ok(MessageFactory.CreateMethodReturnForMessage msg responseBody)
        member __.AddSignalHandler _ = ()
        member __.RemoveSignalHandler _ = ()
        member __.AddExportedObject _ = ()
        member __.Disconnect () = ()
        [<CLIEvent>]
        member __.DBusMessageReceived = dbusMessageReceived.Publish
    }

[<Tests>]
let busTests =
    testList "busManagementTests" [
        testCase "RequestName should not send messages for invalid busname" <| fun _ ->
            let recordedMessages = new System.Collections.Generic.List<DBusMessageBody>()
            let fakebus = fakeRecordingBus recordedMessages [| |]

            let requestNameResponse = BusManager.RequestName fakebus "" RequestNameFlags.AllowReplacement
            let expectedResponse = Error (sprintf "Invalid request name specified: EmptyBusName")
            Expect.equal requestNameResponse expectedResponse "RequestName didn't return the expected response value with invalid busname"
            Expect.isEmpty recordedMessages "No messages should be send to request invalid busnames"

        testCase "RequestName should be ok with valid busname" <| fun _ ->
            let recordedMessages = new System.Collections.Generic.List<DBusMessageBody>()
            let responseMessage = ToDBus.Value ((uint32) RequestNameResult.PrimaryOwner)
            let fakebus = fakeRecordingBus recordedMessages [| [|responseMessage|] |]

            let response = BusManager.RequestName fakebus "my.valid.name" RequestNameFlags.AllowReplacement
            let expectedResponse = Ok (RequestNameResult.PrimaryOwner)
            Expect.equal response expectedResponse "RequestName didn't return the expected response value with valid busname"
            Expect.equal recordedMessages.Count 1 "Only 1 bus message should be send to request name"
            let expectedMessageBody =  [|(ToDBus.Value "my.valid.name"); (ToDBus.Value 1u)|]
            Expect.sequenceEqual (recordedMessages.[0]) expectedMessageBody "Sent message for request name should have expected content"
        
        testCase "ReleaseName should not send messages for invalid busname" <| fun _ ->
            let recordedMessages = new System.Collections.Generic.List<DBusMessageBody>()
            let fakebus = fakeRecordingBus recordedMessages [| |]

            let releaseNameResponse = BusManager.ReleaseName fakebus ""
            let expectedResponse = Error (sprintf "Invalid release name specified: EmptyBusName")
            Expect.equal releaseNameResponse expectedResponse "ReleaseName didn't return the expected response value with invalid busname"
            Expect.isEmpty recordedMessages "No messages should be send to release invalid busnames"

        testCase "ReleaseName should be ok with valid busname" <| fun _ ->
            let recordedMessages = new System.Collections.Generic.List<DBusMessageBody>()
            let responseMessage = ToDBus.Value ((uint32) ReleaseNameResult.Released)
            let fakebus = fakeRecordingBus recordedMessages [| [|responseMessage|] |]

            let response = BusManager.ReleaseName fakebus "my.valid.name"
            let expectedResponse = Ok (ReleaseNameResult.Released)
            Expect.equal response expectedResponse "ReleaseName didn't return the expected response value with valid busname"
            Expect.equal recordedMessages.Count 1 "Only 1 bus message should be send to release name"
            let expectedMessageBody =  [|(ToDBus.Value "my.valid.name")|]
            Expect.sequenceEqual (recordedMessages.[0]) expectedMessageBody "Sent message for release name should have expected content"
    ]