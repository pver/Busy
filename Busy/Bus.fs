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
    
type IBus =
    abstract member SendMessage: DBusMessage -> unit
    abstract member Transport: ITransport

type Bus (transport:ITransport) = 

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
    
    interface IBus with 
        member __.SendMessage (message:DBusMessage) =
            Marshalling.marshallMessage message
            |> transport.Write
        member __.Transport = transport
