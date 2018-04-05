namespace Busy
open Busy.Authentication
open Transport

module Authenticator =

    let private newline = [|13uy;10uy|]
    let private formatCommand (cmd:string) = Array.append (System.Text.Encoding.ASCII.GetBytes(cmd)) newline
    let private sendCommand (transport:ITransport) cmd = 
        printfn "Writing %s" cmd
        transport.Write <| formatCommand cmd 

    // Todo: 
    // Add client-server and server-client message types as discriminated union type
    // add authentication state machine taking in IConnection
    // add validation and error handling of responses
    // Implement IAuthenticator 
    //      => impl as DBusCookieSHA1Authenticator
    //      => impl as AnonymousAuthenticator
    // Add IConnection + impl as UnixDomainSocketConnection

    let private authenticateWithAuthenticator (transport:ITransport) (authenticator:IDBusAauthenticator) =
      
        let rec checkStateTillCompleted state =
            match state with
            | AwaitsInput cmd -> 
                    sendCommand transport cmd
                    
                    let stream = transport.Connect() // Todo: should use byteprovider here!!
                    let bytes = Array.init 1024 (fun _ -> 0uy)
                    let bytesRec = stream.Read(bytes, 0, bytes.Length);

                    System.Text.Encoding.ASCII.GetString(bytes, 0, bytesRec)
                    |> authenticator.ProcessInput
                    |> checkStateTillCompleted
            | Completed cmp ->  cmp
        
        let completedState = checkStateTillCompleted <| authenticator.Start() 
        completedState

    // all types of authenticators here (TODO: inject them? or limit to this fixed list of supported authenticators?)
    let private supportedAuthenticators() = 
        [
            new ExternalDBusAuthenticator("30") // 30 is hex for user id 0 (=root) TODO: replace with actual user id (platform specific!)
        ] |> List.map (fun x-> x :> IDBusAauthenticator)

    // Todo: should this return a new IAuthenticatedTransport here? 
    //       This could then later on be used to verify if the transport was authenticated
    let Authenticate (transport:ITransport) =

        let rec authenticate (authenticators:IDBusAauthenticator list) = 
            match authenticators with
            | [] -> Error "No authenticator could authenticate succesfully with the server"
            | head::tail -> match authenticateWithAuthenticator transport head with
                            | Ok o -> Ok o
                            | Error _ -> authenticate tail

        let authenticationResult = authenticate <| supportedAuthenticators()

        // Todo: add support for Unix FD negotiation here, that should go before the BEGIN call

        match authenticationResult with
        | Error _ -> authenticationResult
        | Ok _ -> 
            sendCommand transport "BEGIN"
            authenticationResult

        // Todo: call org.freedesktop.DBus.Hello, but higher up in the API, not here in connect + do this optional (BusSession type?), 
        // because not every dbus session types requires this 
        // object path = /org/freedesktop/DBus
