namespace Busy

open System.Net
open System.Net.Sockets

module UnixDomainSocket =

    let private addressFamilyBytes = [|0x1uy; 0x0uy|]
    type UnixDomainSocketEndpoint(domainSocket : string) =
        inherit EndPoint()
        override __.Serialize () =   let bytes = Array.concat [| addressFamilyBytes ; System.Text.Encoding.ASCII.GetBytes(domainSocket); [|0x0uy;|] |]
                                     let addr = new SocketAddress(AddressFamily.Unix, Array.length bytes)
                                     bytes |> Array.iteri (fun i b -> addr.[i] <- b)
                                     addr
        override __.Create (addr:SocketAddress) = 
            let bytes = Array.init (addr.Size - 3) (fun i -> addr.[i+2])
            let domainSocket = (System.Text.Encoding.ASCII.GetString bytes)
            printfn "Creating socket for: %s" domainSocket
            new UnixDomainSocketEndpoint(domainSocket) :> EndPoint
        override __.AddressFamily = AddressFamily.Unix

    let private newline = [|13uy;10uy|]
    let private formatCommand (cmd:string) = Array.append (System.Text.Encoding.ASCII.GetBytes(cmd)) newline

    // Todo: 
    // Add client-server and server-client message types as discriminated union type
    // add authentication state machine taking in IConnection
    // add validation and error handling of responses
    // add IAuthenticator + impl as ExternalAuthenticator (taking IConnection to send/receive CS/SC messages)
    // Add IConnection + impl as UnixDomainSocketConnection

    let private authenticateExternal (socket:Socket) =
        // Encode the data string into a byte array.  => 30 is hex for user id 0 (=root) TODO: replace with actual user id (platform specific!)
        let msg = formatCommand "AUTH EXTERNAL 30"
        
        // Send the data through the socket.  
        socket.Send msg |> ignore
        
        // Receive the response from the remote device.  
        let bytes = Array.init 1024 (fun _ -> 0uy)
        let bytesRec = socket.Receive(bytes);
        let authResponse = System.Text.Encoding.ASCII.GetString(bytes, 0, bytesRec);
        printfn "received: %s" authResponse
        
        match authResponse.StartsWith("OK") with
        | false ->  failwith "Unexpected error received"
        | true -> 
                    let beginCmd = formatCommand "BEGIN"
                    socket.Send beginCmd |> ignore

    // Todo: 
    // Return IConnection here
    let connect (addr:string) =
        let socket = new Socket(AddressFamily.Unix, SocketType.Stream, ProtocolType.IP)
        let endpoint = new UnixDomainSocketEndpoint(addr)

        socket.Connect endpoint

        socket.Send [|0x0uy|] |> ignore // start byte required by dbus daemon as first byte

        authenticateExternal socket

        socket
    
    // Todo: 
    // Take IConnection here
    let disconnect (socket:Socket) =
        // Release the socket.  
        socket.Shutdown(SocketShutdown.Both);
        socket.Close();