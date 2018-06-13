namespace Busy

open Address
open Transport
open MessageTypes

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
    
    [<CLIEvent>]
    abstract member DBusMessageReceived: IDelegateEvent<System.EventHandler<DBusMessageReceivedEventArgs>>


type Bus (transport:ITransport) = 
    let dbusMessageReceived = new DelegateEvent<System.EventHandler<DBusMessageReceivedEventArgs>>()

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
                        dbusMessageReceived.Trigger [|this; DBusMessageReceivedEventArgs(m)|]
                        // proposal: pass to messageProcessor
                        // let resultMessage = _messageProcessor.Process incomingMessage
                        // match resultMessage with
                        // | None -> ()
                        // | Some(result) -> sendMessage result
            | Error _ -> () // Todo: expose through logging or other Error event?
            
    interface IBus with 
        member this.SendMessage (message:DBusMessage) = this.SendMessage message
        member this.Transport = this.Transport
        member this.IterateMessage() = this.IterateMessage()

        [<CLIEvent>]
        member __.DBusMessageReceived = dbusMessageReceived.Publish
