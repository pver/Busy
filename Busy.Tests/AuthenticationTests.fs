module AuthenticationTests

open Expecto
open Busy.Authentication

let private createExternalAuthenticator() = new ExternalDBusAuthenticator("30") :> IDBusAauthenticator

[<Tests>]
let tests =
  testList "ExternalAuthenticatorTests" [
    testCase "Mechanism should be EXTERNAL" <| fun _ ->
      let authenticator = createExternalAuthenticator()
      Expect.equal authenticator.Mechanism "EXTERNAL" "External authenticator should have correct mechanism name"

    testCase "Authenticator should send Auth + mechanism + userid on start" <| fun _ ->
      let authenticator = createExternalAuthenticator()
      let subject = authenticator.Start()
      let expectedSubject = AwaitsInput("AUTH EXTERNAL 30")
      Expect.equal subject expectedSubject "External authenticator should send correct AUTH message"

    testCase "Authenticator should parse OK correctly" <| fun _ ->
      let authenticator = createExternalAuthenticator()
      let subject = authenticator.ProcessInput("OK f4b06a6402fb063245569782591172af")
      let expectedSubject = Completed <| Ok ("f4b06a6402fb063245569782591172af")
      Expect.equal subject expectedSubject "External authenticator should parse OK correctly"
  ]