module MarshallingTests

open Expecto

open Busy.MessageTypes
open Busy.Types
open Busy.Marshalling
open System.IO

[<Tests>]
let tests =
  testList "marshalling values" [
    testCase "int16 '1000' is marshalled to correct byte representations" <| fun _ ->
      let subjectAsLittleEndian = marshall 0 DBusMessageEndianness.LittleEndian << Primitive <| (Int16 1000s)
      let subjectAsBigEndian = marshall 0 DBusMessageEndianness.BigEndian << Primitive <| (Int16 1000s)
      let expectedLittleEndian = [|0xE8uy; 0x03uy;|]
      let expectedBigEndian = [|0x03uy; 0xE8uy|]

      Expect.equal subjectAsLittleEndian expectedLittleEndian "1000 should have correct little endian bytes"
      Expect.equal subjectAsBigEndian expectedBigEndian "1000 should have correct big endian bytes"

    // testCase "int32 '1000' is marshalled to correct byte representations" <| fun _ ->
    //   let subjectAsLittleEndian = marshall DBusMessageEndianness.LittleEndian << Primitive <| (Int32 1000)
    //   let subjectAsBigEndian = marshall DBusMessageEndianness.BigEndian << Primitive <| (Int32 1000)
    //   let expectedLittleEndian = {alignment=4; bytes= [|0xE8uy; 0x03uy; 0x00uy; 0x00uy|]}
    //   let expectedBigEndian = {alignment=4; bytes= [|0x00uy; 0x00uy; 0x03uy; 0xE8uy|]}
    //   Expect.equal (subjectAsLittleEndian |> Seq.toArray) [|expectedLittleEndian|] "1000 should have correct little endian bytes"
    //   Expect.equal (subjectAsBigEndian |>Seq.toArray) [|expectedBigEndian|] "1000 should have correct big endian bytes"

    testCase "string 'foo' is marshalled to correct byte representations" <| fun _ ->
      let subjectAsLittleEndian = marshall 0 DBusMessageEndianness.LittleEndian << Primitive <| (String "foo")
      let expectedLittleEndian = [|0x03uy; 0x00uy; 0x00uy; 0x00uy; 0x66uy; 0x6fuy; 0x6fuy; 0x00uy|]
      
      Expect.equal subjectAsLittleEndian expectedLittleEndian "'foo' should have correct little endian bytes"
      
    testCase "string '+' is marshalled to correct byte representations" <| fun _ ->
      let subjectAsLittleEndian = marshall 0 DBusMessageEndianness.LittleEndian << Primitive <| (String "+")
      let expectedLittleEndian = [|0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x2Buy; 0x00uy;|]
      Expect.equal subjectAsLittleEndian expectedLittleEndian "'+' should have correct little endian bytes"

    testCase "string sequence is marshalled to correct byte representations" <| fun _ ->
      let values = [| Primitive (String "foo")
                      Primitive (String "+")
                      Primitive (String "bar") |]
      
      let subjectAsLittleEndian = values |> Array.fold (fun acc x -> Array.append acc <| marshall acc.Length DBusMessageEndianness.LittleEndian x) [||]
      let expectedLittleEndian = [| 0x03uy; 0x00uy; 0x00uy; 0x00uy;     // length of ‘foo’ = 3
                                    0x66uy; 0x6fuy; 0x6fuy;             // ‘foo’
                                    0x00uy;                             // trailing nul

                                                                        // no padding required, we are already at a multiple of 4
                                    0x01uy; 0x00uy; 0x00uy; 0x00uy;     // length of ‘+’ = 1
                                    0x2buy;                             // ‘+’
                                    0x00uy;                             // trailing nul

                                    0x00uy; 0x00uy;                     // 2 bytes of padding to reach next multiple of 4

                                    0x03uy; 0x00uy; 0x00uy; 0x00uy;     // length of ‘bar’ = 1
                                    0x62uy; 0x61uy; 0x72uy;             // ‘bar’
                                    0x00uy                              // trailing nul
                                   |]
      Expect.equal subjectAsLittleEndian expectedLittleEndian "string sequance should have correct little endian bytes"

    // testCase "int32 '100' is marshalled to correct byte representations" <| fun _ ->
    //   let subjectAsLittleEndian = marshall DBusMessageEndianness.LittleEndian << Primitive <| (Int32 100)
    //   let subjectAsBigEndian = marshall DBusMessageEndianness.BigEndian << Primitive <| (Int32 100)
    //   let expectedLittleEndian = {alignment=4; bytes= [|100uy; 0x00uy; 0x00uy; 0x00uy|]}
    //   let expectedBigEndian = {alignment=4; bytes= [|0x00uy; 0x00uy; 0x00uy; 100uy|]}
    //   Expect.equal (subjectAsLittleEndian |> Seq.toArray) [|expectedLittleEndian|] "100 should have correct little endian bytes"
    //   Expect.equal (subjectAsBigEndian |>Seq.toArray) [|expectedBigEndian|] "100 should have correct big endian bytes"

    testCase "array int64 ['5'] is marshalled to correct byte representations" <| fun _ ->
      let array = [|(Primitive <| (Int64 5L))|]
      let arrayValue = Array (DBusType.PrimitiveType Int64Type, array)

      let subjectAsLittleEndian = marshall 8 DBusMessageEndianness.LittleEndian arrayValue
      let subjectAsBigEndian = marshall 8 DBusMessageEndianness.BigEndian arrayValue
      
      let expectedLittleEndian = [| 
                                    8uy; 0uy; 0uy; 0uy;                       // n = 8 bytes of data
                                    0uy; 0uy; 0uy; 0uy;                       // padding to 8-byte boundary
                                    5uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy|]  // first element = 5
      let expectedBigEndian = [| 
                                    0uy; 0uy; 0uy; 8uy;                       // n = 8 bytes of data
                                    0uy; 0uy; 0uy; 0uy;                       // padding to 8-byte boundary
                                    0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 5uy|]  // first element = 5

      Expect.equal subjectAsLittleEndian expectedLittleEndian "array int64 ['5'] should have correct little endian bytes"
      Expect.equal subjectAsBigEndian expectedBigEndian "array int64 ['5'] should have correct big endian bytes"      
  ]