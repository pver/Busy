namespace Busy
open System.IO
open Busy.Authentication
open Transport

module Authenticator =

    let private newline = [|13uy;10uy|]
    let private formatCommand (cmd:string) = Array.append (System.Text.Encoding.ASCII.GetBytes(cmd)) newline
    let private sendCommand (stream:Stream) cmd = 
        printfn "Writing %s" cmd
        let bytes = formatCommand cmd 
        stream.Write(bytes, 0, bytes.Length)

    // Todo: 
    // Add client-server and server-client message types as discriminated union type
    // add authentication state machine taking in IConnection
    // add validation and error handling of responses
    // Implement IAuthenticator 
    //      => impl as DBusCookieSHA1Authenticator
    //      => impl as AnonymousAuthenticator
    // Add IConnection + impl as UnixDomainSocketConnection

    let private authenticateWithAuthenticator (stream:Stream) (authenticator:IDBusAauthenticator) =
      
        let rec checkStateTillCompleted state =
            match state with
            | AwaitsInput cmd -> 
                    sendCommand stream cmd
                    
                    let bytes = Array.init 1024 (fun _ -> 0uy)
                    let bytesRec = stream.Read(bytes, 0, bytes.Length);

                    System.Text.Encoding.ASCII.GetString(bytes, 0, bytesRec)
                    |> (fun x -> printf "Got %s" x
                                 x )
                    |> authenticator.ProcessInput
                    |> checkStateTillCompleted
            | Completed cmp ->  cmp
        
        let completedState = checkStateTillCompleted <| authenticator.Start() 
        match completedState with
        | Error msg -> failwith msg
        | Ok _ -> sendCommand stream "BEGIN"

    // Todo: 
    // Return IConnection here
    let Authenticate (transport:ITransport) =
        let stream = transport.Connect()

        // Todo: 
        // -> go through all types of authenticators here (inject them? or fixed list of supported authenticators?)
        // -> Encode the data string into a byte array.  => 30 is hex for user id 0 (=root) TODO: replace with actual user id (platform specific!)
        new ExternalDBusAuthenticator("30") 
        |> authenticateWithAuthenticator stream

        // Todo: call org.freedesktop.DBus.Hello, but higher up in the API, not here in connect + do this optional (BusSession type?), 
        // because not every dbus session types requires this 
        // object path = /org/freedesktop/DBus
