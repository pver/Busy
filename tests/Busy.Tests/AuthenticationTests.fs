module AuthenticationTests

open Expecto
open Busy.Authentication

let private createExternalAuthenticator() = new ExternalDBusAuthenticator("0") :> IDBusAauthenticator

let createFormatExternalIdTestCase testCaseName userId expected =
    testCase testCaseName <| fun _ ->
          let subject = formatExternalUid userId
          Expect.equal subject expected testCaseName

[<Tests>]
let externalAuthenticationTests =
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

    testCase "Authenticator should fail on REJECTED correctly" <| fun _ ->
      let authenticator = createExternalAuthenticator()
      let subject = authenticator.ProcessInput("REJECTED KERBEROS_V4 SKEY")
      let expectedSubject = Completed <| Error ("REJECTED KERBEROS_V4 SKEY")
      Expect.equal subject expectedSubject "External authenticator should fail on REJECTED correctly"
      
    testCase "Authenticator should fail on ERROR correctly" <| fun _ ->
      let authenticator = createExternalAuthenticator()
      let subject = authenticator.ProcessInput("ERROR some error message")
      let expectedSubject = Completed <| Error ("ERROR some error message")
      Expect.equal subject expectedSubject "External authenticator should fail on ERROR correctly"

    createFormatExternalIdTestCase "Unix root id should be formatted correctly" "0" "30"
    createFormatExternalIdTestCase "Unix 1000 id should be formatted correctly" "1000" "31303030"
    createFormatExternalIdTestCase "Windows example id should be formatted correctly" "S-1-5-18" "532d312d352d3138"
  ]
