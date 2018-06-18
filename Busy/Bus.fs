namespace Busy

open Address
open Transport
open MessageTypes
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

    [<CLIEvent>]
    abstract member DBusMessageReceived: IDelegateEvent<System.EventHandler<DBusMessageReceivedEventArgs>>


type Bus (transport:ITransport) = 
    let dbusMessageReceived = new DelegateEvent<System.EventHandler<DBusMessageReceivedEventArgs>>()
    
    let messageProcessor = new MessageProcessor() // Todo: make this in interface so user could inject alternative messageProcessor?

    static let getKnownBusAddress knownBus =
        match knownBus with
        | SystemBus -> Environment.SystemBusAddress()

    static member Create (dbusAddress:DBusAddress) =
        Transport.FromAddress(dbusAddress)
        |> Result.mapError (fun e -> { CreateBusErrorMessage="Could not create transport from address"; CreateBusInnerError= Some(TransportError e) })
        |> Result.bind (fun transport -> 
                                            Authenticator.Authenticate(transport)
                                            |> Result.mapError (fun e -> { CreateBusErrorMessage="Could not successfully authenticate"; CreateBusInnerError= Some(AuthenticationError e) })
                                            |> Result.map (fun authId -> Bus(transport) :> IBus)
                                            )
        
    static member CreateKnownBus knownBus =
        match getKnownBusAddress knownBus with
        | ValidAddress a -> Bus.Create(a) 
        | InvalidAddress a -> Error { CreateBusErrorMessage="Could not retrieve a valid address"; CreateBusInnerError= Some(AddressError a) }
        | _ -> failwith "not implemented yet"
    

    [<CLIEvent>]
    member __.DBusMessageReceived = dbusMessageReceived.Publish
    
    member __.Transport = transport
    member __.SendMessage (message:DBusMessage) =
            Marshalling.marshallMessage message
            |> transport.Write

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

    member __.AddSignalHandler(handler) =
        // Todo: create and send AddMatch from here!!
        // SendAndWait(addMatchMessage(handler.MatchRule))
        messageProcessor.AddSignalHandler handler
    
    member __.RemoveSignalHandler(handler) =
        // Todo: create and send AddMatch from here!!
        // SendAndWait(removeMatchMessage(handler.MatchRule))
        messageProcessor.RemoveSignalHandler handler

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
        [<CLIEvent>]
        member __.DBusMessageReceived = dbusMessageReceived.Publish
