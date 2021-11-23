namespace Busy

open Address
open Transport
open MessageTypes
open Busy.BusManagementMessages
open Busy.MessageProcessing

type KnownBus =
    //todo: | SessionBus
    //todo: | StarterBus
    | SystemBus

type CreateBusInnerError = 
    | AddressError of InvalidDBusAddress
    | AuthenticationError of string
    | TransportError of CreateTransportError

type CreateBusError = {CreateBusErrorMessage:string; CreateBusInnerError:option<CreateBusInnerError>}
    
type DBusMessageReceivedEventArgs(message:DBusMessage) =
    inherit System.EventArgs()
    member __.Message = message

type IBus =
    abstract member SendMessage: DBusMessage -> unit
    abstract member Transport: ITransport
    abstract member IterateMessage: unit -> unit
    
    abstract member SendAndWait: DBusMessage -> Result<DBusMessage, string>

    abstract member AddSignalHandler: SignalHandler -> unit 
    abstract member RemoveSignalHandler: SignalHandler -> unit 

    abstract member AddExportedObject: ExportedObject -> unit
    // todo: should we need to remove exported objects as well?
    abstract member Disconnect: unit -> unit

    [<CLIEvent>]
    abstract member DBusMessageReceived: IDelegateEvent<System.EventHandler<DBusMessageReceivedEventArgs>>

type CreateBusOptions = {SendHello:bool}

type Bus (transport:ITransport) = 
    let dbusMessageReceived = new DelegateEvent<System.EventHandler<DBusMessageReceivedEventArgs>>()
    
    let messageProcessor = new MessageProcessor() // Todo: make this an interface so user could inject alternative messageProcessor?

    static let getKnownBusAddress knownBus =
        match knownBus with
        | SystemBus -> Environment.SystemBusAddress()

    static member Create (dbusAddress:DBusAddress) (createOptions:CreateBusOptions) =
        Transport.FromAddress(dbusAddress)
        |> Result.mapError (fun e -> { CreateBusErrorMessage="Could not create transport from address"; CreateBusInnerError= Some(TransportError e) })
        |> Result.bind (fun transport -> 
                                            transport.Connect()
                                            Authenticator.Authenticate(transport) // Todo: add authenticators to use in a List in CreateBusOptions? This way users can pass in custom authenticators
                                            |> Result.mapError (fun e -> { CreateBusErrorMessage="Could not successfully authenticate"; CreateBusInnerError= Some(AuthenticationError e) })
                                            |> Result.map (fun authId -> 
                                                                        // Todo: create an AuthenticatedTransport wrapper containing the authId?
                                                                        let bus = Bus(transport) :> IBus
                                                                         
                                                                        if createOptions.SendHello 
                                                                        then createHello() |> bus.SendMessage // Todo: catch error from daemon?
                                                                         
                                                                        bus
                                                                        ) 
                                            )
        
    static member CreateKnownBus knownBus =
        match getKnownBusAddress knownBus with
        | ValidAddress address -> Bus.Create address {SendHello=true}
        | InvalidAddress a -> Error { CreateBusErrorMessage="Could not retrieve a valid address"; CreateBusInnerError= Some(AddressError a) }
        | _ -> failwith "not implemented yet"
    

    [<CLIEvent>]
    member __.DBusMessageReceived = dbusMessageReceived.Publish
    
    member __.Transport = transport
    member __.SendMessage (message:DBusMessage) =
            Marshalling.marshallMessage message
            |> transport.Write

    // Todo: add cancellation possibility!
    member this.IterateMessage () =
            let message = Unmarshalling.unmarshallMessage transport
            match message with
            | Ok m -> 
                        // pass to anyone interested in raw messages coming in
                        dbusMessageReceived.Trigger [|this; DBusMessageReceivedEventArgs(m)|]

                        let resultMessage = messageProcessor.Process m
                        match resultMessage with
                        | Some result -> 
                            // Todo: don't send when NO_REPLY_EXPECTED flag is set on incoming message!!
                            this.SendMessage result
                        | None -> ()

            | Error _ -> () // Todo: expose through logging or other Error event?

    member this.AddSignalHandler handler =
        // Todo: check if it's not already added, in that case don't send AddMatch to daemon!
        
        messageProcessor.AddSignalHandler handler
        
        createAddMatch handler.MatchRule |> this.SendMessage // Todo: block? what to do with an error?
    
    member this.RemoveSignalHandler handler =
        // Todo: check if it's not already removed or if other handlers still need it, in that case don't send remove to daemon!

        messageProcessor.RemoveSignalHandler handler

        createRemoveMatch handler.MatchRule |> this.SendMessage // Todo: block? what to do with an error?

    member __.AddExportedObject exportedObject =
        messageProcessor.AddExportedObject exportedObject

    member __.Disconnect = transport.Close

    // Todo: add timeout support!!
    member this.SendAndWait(message:DBusMessage) : Result<DBusMessage, string> =
        let call = messageProcessor.AddPendingCall(message.SequenceNumber)
        this.SendMessage message
        call.WaitForResult() 
        call.Result
            
    interface IBus with 
        member this.SendMessage (message:DBusMessage) = this.SendMessage message
        member this.Transport = this.Transport
        member this.IterateMessage () = this.IterateMessage()
        member this.SendAndWait (message:DBusMessage) = this.SendAndWait message
        member this.AddSignalHandler (handler) = this.AddSignalHandler handler
        member this.RemoveSignalHandler (handler) = this.RemoveSignalHandler handler
        member this.AddExportedObject (exportedObject) = this.AddExportedObject exportedObject
        member this.Disconnect () = this.Disconnect()
        [<CLIEvent>]
        member __.DBusMessageReceived = dbusMessageReceived.Publish
