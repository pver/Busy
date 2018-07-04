namespace Busy
open Busy.Authentication
open Transport

module Authenticator =

    let private newline = [|13uy;10uy|]
    let internal formatCommand (cmd:string) = Array.append (System.Text.Encoding.ASCII.GetBytes(cmd)) newline
    let private sendCommand (transport:ITransport) cmd = transport.Write <| formatCommand cmd 

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
                    
                    let bytesRec = transport.ReadBytes 1024;

                    System.Text.Encoding.ASCII.GetString(bytesRec, 0, Array.length bytesRec)
                    |> authenticator.ProcessInput
                    |> checkStateTillCompleted
            | Completed cmp ->  cmp
        
        let completedState = checkStateTillCompleted <| authenticator.Start() 
        completedState

    // all types of authenticators here (TODO: inject them, for instance in the Authenticate method and passed in from CreateBusOptions? 
    // or limit to this fixed list of supported authenticators?)
    let private supportedAuthenticators() = 
        [
            new ExternalDBusAuthenticator(Utilities.getExternalUserId()) // user id 0 (=root) TODO: replace with actual user id (platform specific!)
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
