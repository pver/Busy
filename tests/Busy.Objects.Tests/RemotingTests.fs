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

    abstract member ArraysInVoidOut : int[] -> string[] -> unit

let testObjectPath = "obj"
let testInterfaceName = "ifc"
let testDestinationName = "dest"

let normalReturnFunction returnValue msg : Result<DBusMessage, string> =
    let result = MessageFactory.CreateMethodReturnForMessage msg returnValue
    Expect.equal msg.HeaderFields.Destination (Some testDestinationName) "destination name should match"
    Expect.equal msg.HeaderFields.Interface (Some testInterfaceName) "interface name should match"
    Expect.equal msg.HeaderFields.ObjectPath (Some testObjectPath) "object path name should match"
    Ok (result)



let factory = new RemoteObjectTypeFactory()
let voidResult = ()
let emptyDBusRequest = [||]
let emptyDBusResult = [||]
let createProxyTestCase (testCaseName:string) 
    (fn:(IRemoteObject -> 'a)) 
    (expectedBusRequest:DBusMessageBody) 
    (fakeResponse:DBusMessageBody)
    (expectedDotnetResult:'a) =
        testCase testCaseName <| fun _ ->
            // Arrange
            let recordedMessages = new System.Collections.Generic.List<DBusMessageBody>()
            let fakebus = fakeRecordingBus recordedMessages (normalReturnFunction fakeResponse)
            let proxy = factory.GetRemoteObject<IRemoteObject> fakebus testObjectPath testInterfaceName testDestinationName
            
            // Act
            let callResult = fn proxy

            // Assert
            Expect.equal recordedMessages.Count 1 "1 request should have been sent to bus instance"
            let requestMsg = recordedMessages.Item(0)
            Expect.sequenceEqual requestMsg expectedBusRequest "expected request body not found"
            Expect.equal callResult expectedDotnetResult "expected return value should match faked return value"

[<Tests>]
let remotingTests =
    testList "remoteObjectTests" [        
        testCase "Factory should only allow interface types" <| fun _ ->
            // Arrange
            let recordedMessages = new System.Collections.Generic.List<DBusMessageBody>()
            let returnValue = ([])
            let fakebus = fakeRecordingBus recordedMessages (normalReturnFunction returnValue)

            // Act (just using DBusMessage as non interface type)
            let createNonInterfaceProxy() = factory.GetRemoteObject<DBusMessage> fakebus testObjectPath testInterfaceName testDestinationName |> ignore
            
            // Assert
            Expect.throwsT<System.NotSupportedException> createNonInterfaceProxy "expected creating remote object of non interface type to throw"

        testCase "Factory should not allow null bus" <| fun _ ->
            // Arrange
            let nullBus = (Unchecked.defaultof<IBus>)

            // Act
            let createNonInterfaceProxy() = factory.GetRemoteObject<IRemoteObject> nullBus testObjectPath testInterfaceName testDestinationName |> ignore
            
            // Assert
            Expect.throws createNonInterfaceProxy "expected creating remote object with null bus to throw"

        testCase "Factory should not allow null obj path" <| fun _ ->
            // Arrange
            let recordedMessages = new System.Collections.Generic.List<DBusMessageBody>()
            let returnValue = ([])
            let fakebus = fakeRecordingBus recordedMessages (normalReturnFunction returnValue)

            // Act
            let createNonInterfaceProxy() = factory.GetRemoteObject<IRemoteObject> fakebus (null) testInterfaceName testDestinationName |> ignore
            
            // Assert
            Expect.throwsT<System.ArgumentNullException> createNonInterfaceProxy "expected creating remote object with null object path to throw"

        testCase "Factory should not allow null interface name" <| fun _ ->
            // Arrange
            let recordedMessages = new System.Collections.Generic.List<DBusMessageBody>()
            let returnValue = ([])
            let fakebus = fakeRecordingBus recordedMessages (normalReturnFunction returnValue)

            // Act
            let createNonInterfaceProxy() = factory.GetRemoteObject<IRemoteObject> fakebus testObjectPath (null) testDestinationName |> ignore
            
            // Assert
            Expect.throwsT<System.ArgumentNullException> createNonInterfaceProxy "expected creating remote object with null interface name to throw"

        testCase "Factory should not allow null destination name" <| fun _ ->
            // Arrange
            let recordedMessages = new System.Collections.Generic.List<DBusMessageBody>()
            let returnValue = ([])
            let fakebus = fakeRecordingBus recordedMessages (normalReturnFunction returnValue)

            // Act
            let createNonInterfaceProxy() = factory.GetRemoteObject<IRemoteObject> fakebus testObjectPath testInterfaceName (null) |> ignore
            
            // Assert
            Expect.throwsT<System.ArgumentNullException> createNonInterfaceProxy "expected creating remote object with null destination name to throw"

        createProxyTestCase "Method void in void out" 
            (fun proxy->proxy.VoidInVoidOut()) 
            emptyDBusRequest 
            emptyDBusResult 
            voidResult

        createProxyTestCase "Method primitives in void out" 
            (fun proxy -> proxy.PrimitivesInVoidOut 123 "teststring" 45.0)
            [|(ToDBus.Value 123);(ToDBus.Value "teststring");(ToDBus.Value 45.0)|] 
            emptyDBusResult
            voidResult

        createProxyTestCase "Method void in int out" 
            (fun proxy -> proxy.VoidInIntOut()) 
            emptyDBusRequest
            [ToDBus.Value 123]
            123

        createProxyTestCase "Method void in string out" 
            (fun proxy -> proxy.VoidInStringOut()) 
            emptyDBusRequest
            [ToDBus.Value "abcd"]
            "abcd"

        createProxyTestCase "Method primitives in int out" 
            (fun proxy -> proxy.PrimitivesInIntOut 123 "teststring" 45.0) 
            [|(ToDBus.Value 123);(ToDBus.Value "teststring");(ToDBus.Value 45.0)|]
            [ToDBus.Value 1234]
            1234

        createProxyTestCase "Method primitives in string out" 
            (fun proxy -> proxy.PrimitivesInStringOut 123 "teststring" 45.0) 
            [|(ToDBus.Value 123);(ToDBus.Value "teststring");(ToDBus.Value 45.0)|]
            [ToDBus.Value "abcde"]
            "abcde"

        createProxyTestCase "Method string and int arrays in void out" 
            (fun proxy -> proxy.ArraysInVoidOut [|1;3;5|] [|"";"abc";"xyz"|]) 
            [|  Array ((PrimitiveType Int32Type),[|(ToDBus.Value 1);(ToDBus.Value 3);(ToDBus.Value 5)|]);
                Array ((PrimitiveType StringType),[|(ToDBus.Value "");(ToDBus.Value "abc");(ToDBus.Value "xyz")|])
            |]
            emptyDBusResult
            voidResult

        createProxyTestCase "Method empty string and int arrays in void out" 
            (fun proxy -> proxy.ArraysInVoidOut [||] [||]) 
            [|  Array ((PrimitiveType Int32Type),[||]);
                Array ((PrimitiveType StringType),[||])
            |]
            emptyDBusResult
            voidResult

        testCase "Method receiving more outputs than expected" <| fun _ ->
            // Arrange
            let recordedMessages = new System.Collections.Generic.List<DBusMessageBody>()
            let returnValue = ([ToDBus.Value "abcde"]) // returning string for void method
            let fakebus = fakeRecordingBus recordedMessages (normalReturnFunction returnValue)

            let proxy = factory.GetRemoteObject<IRemoteObject> fakebus testObjectPath testInterfaceName testDestinationName
            
            // Act
            let callMethod() = proxy.VoidInVoidOut()

            // Assert
            Expect.throwsT<RemoteObjectInvocationException> callMethod "invocation that receives more output values than expected should throw"

        testCase "Method returned error" <| fun _ ->
            // Arrange
            let recordedMessages = new System.Collections.Generic.List<DBusMessageBody>()
            let fakebus = fakeRecordingBus recordedMessages (fun msg -> Ok(MessageFactory.CreateErrorForMessage msg "Failed to execute method" [||]))

            let proxy = factory.GetRemoteObject<IRemoteObject> fakebus testObjectPath testInterfaceName testDestinationName
            
            // Act
            let callMethod() = proxy.VoidInVoidOut()

            // Assert
            Expect.throwsT<RemoteObjectInvocationErrorException> callMethod "invocation that receives error message should throw"

        testCase "Method got wrong dbus returned message type" <| fun _ ->
            // Arrange
            let recordedMessages = new System.Collections.Generic.List<DBusMessageBody>()
            // should never happen that this is returned, but testing anyway
            let fakebus = fakeRecordingBus recordedMessages (fun _ -> Ok(MessageFactory.CreateSignal "obj" "if" "member" [||] None None))

            let proxy = factory.GetRemoteObject<IRemoteObject> fakebus testObjectPath testInterfaceName testDestinationName
            
            // Act
            let callMethod() = proxy.VoidInVoidOut()

            // Assert
            Expect.throwsT<RemoteObjectInvocationException> callMethod "invocation that get wrong message type as result should throw"

        testCase "Method call not properly sent" <| fun _ ->
            // Arrange
            let recordedMessages = new System.Collections.Generic.List<DBusMessageBody>()
            // should never happen that this is returned, but testing anyway
            let fakebus = fakeRecordingBus recordedMessages (fun _ -> Error("Something went wrong sending dbus request"))

            let proxy = factory.GetRemoteObject<IRemoteObject> fakebus testObjectPath testInterfaceName testDestinationName
            
            // Act
            let callMethod() = proxy.VoidInVoidOut()

            // Assert
            Expect.throwsT<RemoteObjectInvocationException> callMethod "invocation that could not be sent should throw"

    ]