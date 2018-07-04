namespace Busy

open Address
open System.Net.Sockets
open UnixDomainSocket
open System.IO
open MarshallingUtilities

module Transport =

    type ITransport =
        inherit IByteProvider
        abstract member Connect: unit -> unit
        abstract member Close: unit -> unit
        abstract member Write: byte[] -> unit

    type UnixDomainSocketTransport (address:UnixDomainSocketAddress) =
        let mutable stream:Option<Stream> = None

        let connect() =

                // Todo: add checks here, could be 'abstract' or server side based path! Or better: create separate (record or union) types 
                // to hold valid UnixDomainSocketAddress value combinations, then we just need to match here
                let path = address.["path"] 
                let socket = new Socket(AddressFamily.Unix, SocketType.Stream, ProtocolType.IP)
                let endpoint = new UnixDomainSocketEndpoint(path)

                socket.Connect endpoint
                
                let streamValue = new NetworkStream(socket, true) :> Stream
                stream <- Some streamValue
                
        let disconnect() =
            match stream with
            | Some s -> s.Close()
                        stream <- None
            | None -> ()

        let write (bytes:byte[]) =
            match stream with
            | Some s -> s.Write(bytes, 0, bytes.Length)
                        s.Flush()
            | None -> ()

        let read (length:int) =
            if (length <= 0) 
            then
                [||]
            else
                match stream with
                | Some s -> let buffer = Array.create length 0uy
                            s.Read (buffer, 0, length) |> ignore
                            buffer
                | None -> [||]

        interface ITransport with
            member __.Connect() = connect()
            member __.Close() = disconnect()
            member __.Write bytes = write bytes
            member __.ReadBytes length = read length

    type CreateTransportError = {CreateTransportErrorMessage:string; CreateTransportException:Option<System.Exception>}

    let FromAddress (address:DBusAddress) =
        try
            let transport = 
                match address with
                | UnixDomainSocketAddress unixDomainSocketAddress -> Ok (UnixDomainSocketTransport unixDomainSocketAddress :> ITransport)
                | LaunchdAddress _ 
                | TcpSocketAddress _ 
                | NonceTcpSocketAddress _ 
                | UnixExecutedSubProcessAddress _ -> 
                    let notImplementedErrorMsg = (sprintf "Transport for address %A not implemented yet" address)
                    Error ({CreateTransportErrorMessage=notImplementedErrorMsg; CreateTransportException=None})

            match transport with
            | Ok (validTransport) -> validTransport.Write [|0x0uy|] // start byte required by dbus daemon as first byte before any other bytes sent
            | _ -> ()

            transport
        with
        | ex -> Error ({CreateTransportErrorMessage=ex.Message; CreateTransportException=Some(ex)})
