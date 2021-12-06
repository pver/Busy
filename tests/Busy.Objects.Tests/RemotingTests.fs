module RemotingTests

open Busy
open Busy.Types
open Busy.MessageTypes

open Busy.Objects.Remoting

open Expecto

let fakeRecordingBus (recordedMessages:System.Collections.Generic.List<DBusMessageBody>) (returnFunction: DBusMessage -> Result<DBusMessage,string>) = 
    let dbusMessageReceived = new DelegateEvent<System.EventHandler<DBusMessageReceivedEventArgs>>()
    { new IBus with
        member __.SendMessage (msg:DBusMessage) = 
            recordedMessages.Add msg.Body
        member __.Transport = failwith "not implemented"
        member __.IterateMessage () = ()
        member __.SendAndWait (msg:DBusMessage) = 
            recordedMessages.Add msg.Body
            returnFunction msg
        member __.AddSignalHandler _ = ()
        member __.RemoveSignalHandler _ = ()
        member __.AddExportedObject _ = ()
        member __.Disconnect () = ()
        [<CLIEvent>]
        member __.DBusMessageReceived = dbusMessageReceived.Publish
    }

type IRemoteObject =
   abstract member VoidInVoidOut : unit -> unit
   abstract member VoidInStringOut : unit -> string
   abstract member VoidInIntOut : unit -> int
   abstract member PrimitivesInVoidOut : int -> string -> double -> unit
   abstract member PrimitivesInIntOut : int -> string -> double -> int
   abstract member PrimitivesInStringOut : int -> string -> double -> string

let testObjectPath = "obj"
let testInterfaceName = "ifc"
let testDestinationName = "dest"

let normalReturnFunction returnValue msg : Result<DBusMessage, string> =
    let result = MessageFactory.CreateMethodReturnForMessage msg returnValue
    Expect.equal msg.HeaderFields.Destination (Some testDestinationName) "destination name should match"
    Expect.equal msg.HeaderFields.Interface (Some testInterfaceName) "interface name should match"
    Expect.equal msg.HeaderFields.ObjectPath (Some testObjectPath) "object path name should match"
    Ok (result)

[<Tests>]
let remotingTests =
    testList "remoteObjectTests" [
        testCase "Factory should only allow interface types" <| fun _ ->
            // Arrange
            let recordedMessages = new System.Collections.Generic.List<DBusMessageBody>()
            let returnValue = ([])
            let fakebus = fakeRecordingBus recordedMessages (normalReturnFunction returnValue)
            let factory = new RemoteObjectTypeFactory()

            // Act (just using DBusMessage as non interface type)
            let createNonInterfaceProxy() = factory.GetRemoteObject<DBusMessage> fakebus testObjectPath testInterfaceName testDestinationName |> ignore
            
            // Assert
            Expect.throwsT<System.NotSupportedException> createNonInterfaceProxy "expected creating remote object of non interface type to throw"

        testCase "Factory should not allow null bus" <| fun _ ->
            // Arrange
            let factory = new RemoteObjectTypeFactory()
            let nullBus = (Unchecked.defaultof<IBus>)

            // Act
            let createNonInterfaceProxy() = factory.GetRemoteObject<IRemoteObject> nullBus testObjectPath testInterfaceName testDestinationName |> ignore
            
            // Assert
            Expect.throws createNonInterfaceProxy "expected creating remote object with null bus to throw"

        testCase "Method void in void out" <| fun _ ->
            // Arrange
            let recordedMessages = new System.Collections.Generic.List<DBusMessageBody>()
            let returnValue = ([])
            let fakebus = fakeRecordingBus recordedMessages (normalReturnFunction returnValue)

            let factory = new RemoteObjectTypeFactory()
            let proxy = factory.GetRemoteObject<IRemoteObject> fakebus testObjectPath testInterfaceName testDestinationName
            
            // Act
            proxy.VoidInVoidOut()

            // Assert
            Expect.equal recordedMessages.Count 1 "1 request should have been sent to bus instance"
            let requestMsg = recordedMessages.Item(0)
            let expectedRequest = [||]
            Expect.equal (requestMsg |> Seq.toArray) expectedRequest "expected request body not found"

        testCase "Method primitives in void out" <| fun _ ->
            // Arrange
            let recordedMessages = new System.Collections.Generic.List<DBusMessageBody>()
            let returnValue = ([])
            let fakebus = fakeRecordingBus recordedMessages (normalReturnFunction returnValue)

            let factory = new RemoteObjectTypeFactory()
            let proxy = factory.GetRemoteObject<IRemoteObject> fakebus testObjectPath testInterfaceName testDestinationName
            
            // Act
            proxy.PrimitivesInVoidOut 123 "teststring" 45.0

            // Assert
            Expect.equal recordedMessages.Count 1 "1 request should have been sent to bus instance"
            let requestMsg = recordedMessages.Item(0)
            let expectedRequest = [|(ToDBus.Value 123);(ToDBus.Value "teststring");(ToDBus.Value 45.0)|]
            Expect.equal (requestMsg |> Seq.toArray) expectedRequest "expected request body not found"

        testCase "Method void in int out" <| fun _ ->
            // Arrange
            let recordedMessages = new System.Collections.Generic.List<DBusMessageBody>()
            let expectedResult = 123
            let returnValue = ([ToDBus.Value expectedResult])
            let fakebus = fakeRecordingBus recordedMessages (normalReturnFunction returnValue)

            let factory = new RemoteObjectTypeFactory()
            let proxy = factory.GetRemoteObject<IRemoteObject> fakebus testObjectPath testInterfaceName testDestinationName
            
            // Act
            let resultInt = proxy.VoidInIntOut()

            // Assert
            Expect.equal recordedMessages.Count 1 "1 request should have been sent to bus instance"
            let requestMsg = recordedMessages.Item(0)
            let expectedRequest = [||]
            Expect.equal (requestMsg |> Seq.toArray) expectedRequest "expected request body not found"
            Expect.equal resultInt expectedResult "expected return value should match faked return value"

        testCase "Method void in string out" <| fun _ ->
            // Arrange
            let recordedMessages = new System.Collections.Generic.List<DBusMessageBody>()
            let expectedResult = "abcd"
            let returnValue = ([ToDBus.Value expectedResult])
            let fakebus = fakeRecordingBus recordedMessages (normalReturnFunction returnValue)

            let factory = new RemoteObjectTypeFactory()
            let proxy = factory.GetRemoteObject<IRemoteObject> fakebus testObjectPath testInterfaceName testDestinationName
            
            // Act
            let resultString = proxy.VoidInStringOut()

            // Assert
            Expect.equal recordedMessages.Count 1 "1 request should have been sent to bus instance"
            let requestMsg = recordedMessages.Item(0)
            let expectedRequest = [||]
            Expect.equal (requestMsg |> Seq.toArray) expectedRequest "expected request body not found"
            Expect.equal resultString expectedResult "expected return value should match faked return value"

        testCase "Method primitives in int out" <| fun _ ->
            // Arrange
            let recordedMessages = new System.Collections.Generic.List<DBusMessageBody>()
            let expectedResult = 1234
            let returnValue = ([ToDBus.Value expectedResult])
            let fakebus = fakeRecordingBus recordedMessages (normalReturnFunction returnValue)

            let factory = new RemoteObjectTypeFactory()
            let proxy = factory.GetRemoteObject<IRemoteObject> fakebus testObjectPath testInterfaceName testDestinationName
            
            // Act
            let resultInt = proxy.PrimitivesInIntOut 123 "teststring" 45.0

            // Assert
            Expect.equal recordedMessages.Count 1 "1 request should have been sent to bus instance"
            let requestMsg = recordedMessages.Item(0)
            let expectedRequest = [|(ToDBus.Value 123);(ToDBus.Value "teststring");(ToDBus.Value 45.0)|]
            Expect.equal (requestMsg |> Seq.toArray) expectedRequest "expected request body not found"
            Expect.equal resultInt expectedResult "expected return value should match faked return value"

        testCase "Method primitives in string out" <| fun _ ->
            // Arrange
            let recordedMessages = new System.Collections.Generic.List<DBusMessageBody>()
            let expectedResult = "abcde"
            let returnValue = ([ToDBus.Value expectedResult])
            let fakebus = fakeRecordingBus recordedMessages (normalReturnFunction returnValue)

            let factory = new RemoteObjectTypeFactory()
            let proxy = factory.GetRemoteObject<IRemoteObject> fakebus testObjectPath testInterfaceName testDestinationName
            
            // Act
            let resultString = proxy.PrimitivesInStringOut 123 "teststring" 45.0

            // Assert
            Expect.equal recordedMessages.Count 1 "1 request should have been sent to bus instance"
            let requestMsg = recordedMessages.Item(0)
            let expectedRequest = [|(ToDBus.Value 123);(ToDBus.Value "teststring");(ToDBus.Value 45.0)|]
            Expect.equal (requestMsg |> Seq.toArray) expectedRequest "expected request body not found"
            Expect.equal resultString expectedResult "expected return value should match faked return value"

    ]