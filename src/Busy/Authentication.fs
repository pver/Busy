namespace Busy

module Authentication =

// Todo: use protocol bases message types:
// >> Commands from the client to the server are as follows:
// AUTH [mechanism] [initial-response]
// CANCEL
// BEGIN
// DATA <data in hex encoding>
// ERROR [human-readable error explanation]
// NEGOTIATE_UNIX_FD
// >> From server to client are as follows:
// REJECTED <space-separated list of mechanism names>
// OK <GUID in hex>
// DATA <data in hex encoding>
// ERROR [human-readable error explanation]
// AGREE_UNIX_FD

    type AuthenticationId = string
    type AuthenticationFailed = string

    type AuthenticationState =
        | AwaitsInput of string  // used to send request to server during authentication + could be used for NEGOTIATE_UNIX_FD negotiation in the future (TODO)
        | Completed of Result<AuthenticationId, AuthenticationFailed>

    type IDBusAauthenticator =
        abstract member Mechanism: string
        abstract member Start: unit -> AuthenticationState
        abstract member ProcessInput: string -> AuthenticationState

    let internal formatExternalUid (uid:string) = 
        System.Text.Encoding.ASCII.GetBytes(uid)
        |> Array.map (fun x -> System.String.Format("{0:x}", x)) 
        |> String.concat ""

    type ExternalDBusAuthenticator (userid:string) =
        let mechanism = "EXTERNAL"

        interface IDBusAauthenticator with

            member __.Mechanism = mechanism
            member __.Start() =
                let encodedUserId = formatExternalUid userid
                let authCommand = sprintf "AUTH %s %s" mechanism encodedUserId
                AwaitsInput authCommand 
                
            member __.ProcessInput input = 
                match input.StartsWith("OK ") with
                    | false ->  let error = Error input
                                Completed(error) 
                    | true ->   let authenticationId = input.Substring(3, input.Length - 3 ).Trim()
                                Completed(Ok(authenticationId))
 