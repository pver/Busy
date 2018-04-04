module ByteProvidersTests

open Expecto
open System.IO
open Busy.ByteProviders
open Busy.MarshallingUtilities

[<Tests>]
let tests =
  testList "StreamByteProviderTests" [
    testCase "Provider can read all bytes in one go" <| fun _ ->
      let allBytes = [|0x00uy; 0x12uy; 0x01uy; 0x30uy; 0x75uy; 0x09uy; 0x04uy;|]
      let memoryStream = new MemoryStream(allBytes)
      let streamByteProvider = new StreamByteProvider(memoryStream) :> IByteProvider

      let subject = streamByteProvider.ReadBytes <| Array.length allBytes
      Expect.equal subject allBytes "Provider can read all bytes in one go"

    testCase "Provider can read all bytes in multiple calls" <| fun _ ->
      let allBytes = [|0x00uy; 0x12uy; 0x01uy; 0x30uy; 0x75uy; 0x09uy; 0x04uy;|]
      let memoryStream = new MemoryStream(allBytes)
      let streamByteProvider = new StreamByteProvider(memoryStream) :> IByteProvider

      let part1 = streamByteProvider.ReadBytes 2
      let part2 = streamByteProvider.ReadBytes 3
      let part3 = streamByteProvider.ReadBytes 2
      Expect.equal part1 [|0x00uy; 0x12uy|] "Provider can read first bytes"
      Expect.equal part2 [|0x01uy; 0x30uy; 0x75uy|] "Provider can read middle bytes"
      Expect.equal part3 [|0x09uy; 0x04uy;|] "Provider can read last bytes"
  ]