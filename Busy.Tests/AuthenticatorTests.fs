module AuthenticatorTests

open Expecto
open Busy.Transport
open Busy.Authenticator

let private failAllAuthenticationsTransport() = { new ITransport with
     member __.Close() = ()
     member __.Write _ = () 
     member __.ReadBytes _ = formatCommand "REJECTED"
     }

let private acceptAllAuthenticationsTransport (authId:string) = { new ITransport with
     member __.Close() = ()
     member __.Write _ = () 
     member __.ReadBytes _ = formatCommand <| sprintf "OK %s" authId
     }

[<Tests>]
let tests =
    testList "AuthenticatorTests" [
        testCase "Authenticator should return Error when all authenticators fail" <| fun _ ->
            let transport = failAllAuthenticationsTransport()
            let authenticationResult = Authenticate transport
            Expect.isError authenticationResult "Authenticator should return Error when all authenticators fail"

        testCase "Authenticator should return Ok with authId when authenticators accept" <| fun _ ->
            let expectedAuthId = "123456"
            let transport = acceptAllAuthenticationsTransport expectedAuthId
            let authenticationResult = Authenticate transport
            Expect.equal authenticationResult (Ok expectedAuthId)  "Authenticator should return Ok with authId when authenticators accept"
    ]