module MarshallingTests

open Expecto

open Busy.MessageTypes
open Busy.Types
open Busy.Marshalling

[<Tests>]
let tests =
  testList "marshalling values" [
    testCase "int32 '1000' is marshalled to correct byte representations" <| fun _ ->
      let subjectAsLittleEndian = marshall DBusMessageEndianness.LittleEndian << Primitive <| (Int32 1000)
      let subjectAsBigEndian = marshall DBusMessageEndianness.BigEndian << Primitive <| (Int32 1000)
      let expectedLittleEndian = {alignment=4; bytes= [|0xE8uy; 0x03uy; 0x00uy; 0x00uy|]}
      let expectedBigEndian = {alignment=4; bytes= [|0x00uy; 0x00uy; 0x03uy; 0xE8uy|]}
      Expect.equal (subjectAsLittleEndian |> Seq.toArray) [|expectedLittleEndian|] "1000 should have correct little endian bytes"
      Expect.equal (subjectAsBigEndian |>Seq.toArray) [|expectedBigEndian|] "1000 should have correct big endian bytes"

      
      let subjectAsLittleEndian2 = marshall DBusMessageEndianness.LittleEndian << Primitive <| (String "foo")
      let expectedLittleEndian2 = {alignment=4; bytes= [|0x03uy; 0x00uy; 0x00uy; 0x00uy; 0x66uy; 0x6fuy; 0x6fuy; 0x00uy|]}
      Expect.equal (subjectAsLittleEndian2 |> Seq.toArray) [|expectedLittleEndian2|] "'foo' should have correct little endian bytes"
      
      let subjectAsLittleEndian3 = marshall DBusMessageEndianness.LittleEndian << Primitive <| (String "+")
      let expectedLittleEndian3 = {alignment=4; bytes=  [|0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x2Buy; 0x00uy;|]}
      Expect.equal (subjectAsLittleEndian3|>Seq.toArray) [|expectedLittleEndian3|] "'+' should have correct little endian bytes"
  ]