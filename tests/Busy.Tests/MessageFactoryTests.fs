module MessageFactoryTests

open Expecto
open Busy
open Busy.MessageTypes

let verifyResultMessageTypeAndHeader callMessage resultMessage expectedResultMessageType =
        let callHeader = callMessage.HeaderFields
        let returnHeader = resultMessage.HeaderFields

        Expect.equal resultMessage.MessageType expectedResultMessageType (sprintf "Result should have message type %A" expectedResultMessageType)
        Expect.equal returnHeader.ReplySerial (Some callMessage.SequenceNumber) "Result reply serial should be seq number of corresponding msg"
        Expect.equal returnHeader.Sender callHeader.Destination "Sender should be send from destination of corresponding msg"
        Expect.equal returnHeader.Destination callHeader.Sender "Destination should be send to sender of corresponding msg"

[<Tests>]
let tests =
  testList "messageFactoryTests" [
    testCase "ErrorForMessage should set correct values from msg" <| fun _ ->
        let call = MessageFactory.CreateMethodCall "/some/path" None "Member" [||] (Some ":1.123") (Some ":1.321")
        let error = MessageFactory.CreateErrorForMessage call "ErrorName" [||]

        verifyResultMessageTypeAndHeader call error DBusMessageType.Error

    testCase "ReturnForMessage should set correct values from msg" <| fun _ ->
        let call = MessageFactory.CreateMethodCall "/some/path" None "Member" [||] (Some ":1.123") (Some ":1.321")
        let result = MessageFactory.CreateMethodReturnForMessage call [||]

        verifyResultMessageTypeAndHeader call result DBusMessageType.MethodReturn
    ]