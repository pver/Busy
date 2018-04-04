module ByteProvidersTests

open Expecto
open System.IO
open Busy.ByteProviders
open Busy.MarshallingUtilities

[<Tests>]
let tests =
  testList "StreamByteProviderTests" [
    testCase "Provider can read all bytes" <| fun _ ->
      let allBytes = [|0x00uy|]
      let memoryStream = new MemoryStream(allBytes)
      let streamByteProvider = new StreamByteProvider(memoryStream) :> IByteProvider

      let subject = streamByteProvider.ReadBytes <| Array.length allBytes
      Expect.equal subject allBytes "External authenticator should have correct mechanism name"
  ]