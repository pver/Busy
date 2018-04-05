namespace Busy

open Address
open System.Net.Sockets
open UnixDomainSocket
open System.IO

module Transport =

    type ITransport =
       abstract member Connect: unit -> Stream
       abstract member IsConnected: bool with get
       abstract member Close: unit -> unit
       abstract member Write: byte[] -> unit

    type UnixDomainSocketTransport (address:UnixDomainSocketAddress) =
        let mutable stream:Option<Stream> = None

        let connect() =
            match stream with
            | Some s -> s
            | None ->

                // Todo: add checks here, could be 'abstract' or server side based path! Or better: create separate (record or union) types 
                // to hold valid UnixDomainSocketAddress value combinations, then we just need to match here
                let path = address.["path"] 
                let socket = new Socket(AddressFamily.Unix, SocketType.Stream, ProtocolType.IP)
                let endpoint = new UnixDomainSocketEndpoint(path)

                socket.Connect endpoint
                socket.Send [|0x0uy|] |> ignore // start byte required by dbus daemon as first byte
                
                let streamValue = new NetworkStream(socket, true) :> Stream
                stream <- Some streamValue
                streamValue
        let disconnect() =
            match stream with
            | Some s -> s.Close()
            | None -> ()

        interface ITransport with
            member __.Connect() = 
                connect()

            member __.Close() =
                disconnect()
            member __.IsConnected with get() = Option.isSome stream

            member __.Write bytes =
                match stream with
                | Some s -> s.Write(bytes, 0, bytes.Length)
                | None -> ()
                

    let FromAddress (address:DBusAddress) : ITransport =
        match address with
        | UnixDomainSocketAddress unixDomainSocketAddress -> UnixDomainSocketTransport unixDomainSocketAddress :> ITransport
        | LaunchdAddress _ 
        | TcpSocketAddress _ 
        | NonceTcpSocketAddress _ 
        | UnixExecutedSubProcessAddress _ -> failwith <| sprintf "Transport for address %A not implemented yet" address