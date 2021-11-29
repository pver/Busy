module BusManagementTests

open Busy
open Busy.MessageTypes

open Expecto
open Busy.BusManagement

let fakeRecordingBus (recordedMessages:System.Collections.Generic.List<DBusMessage>) (responseMessages:array<Result<DBusMessage,string>>) = 
    let dbusMessageReceived = new DelegateEvent<System.EventHandler<DBusMessageReceivedEventArgs>>()
    let mutable responseCounter = 0
    { new IBus with
        member __.SendMessage (msg:DBusMessage) = recordedMessages.Add msg
        member __.Transport = failwith "not implemented"
        member __.IterateMessage () = ()
        member __.SendAndWait (msg:DBusMessage) = 
            let response = responseMessages.[responseCounter]
            responseCounter <- responseCounter + 1
            response
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
            let recordedMessages = new System.Collections.Generic.List<DBusMessage>()
            let fakebus = fakeRecordingBus recordedMessages [| |]

            let requestNameResponse = BusManager.RequestName fakebus "" RequestNameFlags.AllowReplacement
            let expectedResponse = Error (sprintf "Invalid request name result received: EmptyBusName")
            Expect.equal requestNameResponse expectedResponse "RequestName didn't return the expected response value with invalid busname"
            Expect.isEmpty recordedMessages "No messages should be send to request invalid busnames"
    ]