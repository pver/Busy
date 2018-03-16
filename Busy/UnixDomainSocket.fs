namespace Busy

open System.Net
open System.Net.Sockets

open Busy.Authentication

module UnixDomainSocket =

    let private addressFamilyBytes = [|0x1uy; 0x0uy|] // AddressFamily.Unix=1
    type UnixDomainSocketEndpoint(domainSocket : string) =
        inherit EndPoint()

        override __.Serialize () =   let bytes = Array.concat [| addressFamilyBytes ; System.Text.Encoding.ASCII.GetBytes(domainSocket); [|0x0uy;|] |]
                                     let addr = new SocketAddress(AddressFamily.Unix, Array.length bytes)
                                     bytes |> Array.iteri (fun i b -> addr.[i] <- b)
                                     addr

        override __.Create (addr:SocketAddress) = 
            let bytes = Array.init (addr.Size - 3) (fun i -> addr.[i+2])
            let domainSocket = (System.Text.Encoding.ASCII.GetString bytes)
            new UnixDomainSocketEndpoint(domainSocket) :> EndPoint

        override __.AddressFamily = AddressFamily.Unix

    let private newline = [|13uy;10uy|]
    let private formatCommand (cmd:string) = Array.append (System.Text.Encoding.ASCII.GetBytes(cmd)) newline
    let private sendCommand (socket:Socket) cmd = formatCommand cmd |> socket.Send |> ignore

    // Todo: 
    // Add client-server and server-client message types as discriminated union type
    // add authentication state machine taking in IConnection
    // add validation and error handling of responses
    // Implement IAuthenticator 
    //      => impl as DBusCookieSHA1Authenticator
    //      => impl as AnonymousAuthenticator
    // Add IConnection + impl as UnixDomainSocketConnection

    let private authenticate (socket:Socket) (authenticator:IDBusAauthenticator) =
      
        let rec checkStateTillCompleted state =
            match state with
            | AwaitsInput cmd -> 
                    sendCommand socket cmd
                    
                    let bytes = Array.init 1024 (fun _ -> 0uy)
                    let bytesRec = socket.Receive(bytes);

                    System.Text.Encoding.ASCII.GetString(bytes, 0, bytesRec)
                    |> authenticator.ProcessInput
                    |> checkStateTillCompleted
            | Completed cmp ->  cmp
        
        let completedState = checkStateTillCompleted <| authenticator.Start() 
        match completedState with
        | Error msg -> failwith msg
        | Ok _ -> sendCommand socket "BEGIN"

    // Todo: 
    // Return IConnection here
    let connect (addr:string) =
  
        let socket = new Socket(AddressFamily.Unix, SocketType.Stream, ProtocolType.IP)
        let endpoint = new UnixDomainSocketEndpoint(addr)

        socket.Connect endpoint
        socket.Send [|0x0uy|] |> ignore // start byte required by dbus daemon as first byte

        // Todo: 
        // -> separate authentication from connect
        // -> go through all types of authenticators here (inject them? or fixed list of supported authenticators?)
        // -> Encode the data string into a byte array.  => 30 is hex for user id 0 (=root) TODO: replace with actual user id (platform specific!)
        new ExternalDBusAuthenticator("30") |> authenticate socket

        // Todo: call org.freedesktop.DBus.Hello, but not here in connect + optional, because not every dbus session types requires this 
        // object path = /org/freedesktop/DBus

        socket
    
    // Todo: 
    // Take IConnection here
    let disconnect (socket:Socket) =
        // Release the socket.  
        socket.Shutdown(SocketShutdown.Both);
        socket.Close();