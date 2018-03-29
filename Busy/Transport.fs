namespace Busy

open Address
open System.Net.Sockets
open UnixDomainSocket
open System.IO

module Transport =

    type ITransport =
       abstract member Connect: unit -> Stream
       abstract member Close: unit -> unit

    type UnixDomainSocketTransport (address:UnixDomainSocketAddress) =
        let mutable stream:Option<Stream> = None

        interface ITransport with
            member __.Connect() =
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

            member __.Close() =
                match stream with
                | Some s -> s.Close()
                | None -> ()

    let FromAddress (address:DBusAddress) : ITransport =
        match address with
        | UnixDomainSocketAddress unixDomainSocketAddress -> UnixDomainSocketTransport unixDomainSocketAddress :> ITransport
        | LaunchdAddress _ 
        | TcpSocketAddress _ 
        | NonceTcpSocketAddress _ 
        | UnixExecutedSubProcessAddress _ -> failwith <| sprintf "Transport for address %A not implemented yet" address