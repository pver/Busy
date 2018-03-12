module MarshallingTests

open Expecto

open Busy.MessageTypes
open Busy.Types
open Busy.ByteProviders
open Busy.Marshalling
open Busy.Unmarshalling

let marshallLittle = marshall 0 DBusMessageEndianness.LittleEndian
let marshallBig = marshall 0 DBusMessageEndianness.BigEndian

let testValueMarshalling (value:DBusValue) (expectedLittleEndian:byte[]) (expectedBigEndian:byte[]) =
      let littleEndian = marshallLittle value
      let bigEndian = marshallBig value

      Expect.equal littleEndian expectedLittleEndian <| sprintf "%A should have correct little endian bytes" value
      Expect.equal bigEndian expectedBigEndian <| sprintf "%A should have correct big endian bytes" value
      
      let unmarshalledLittle = unmarshall (ArrayByteProvider(expectedLittleEndian)) 0 DBusMessageEndianness.LittleEndian value.Type
      let unmarshalledBig = unmarshall (ArrayByteProvider(expectedBigEndian)) 0 DBusMessageEndianness.BigEndian value.Type
      
      match unmarshalledLittle with
      | Error e -> Expect.isOk unmarshalledLittle e
      | Ok (unmarshalledValue, _) -> Expect.equal unmarshalledValue value <| sprintf "%A  as little endian bytes should be unmarshalled correctly" value
      
      match unmarshalledBig with
      | Error e -> Expect.isOk unmarshalledBig e
      | Ok (unmarshalledValue, _) -> Expect.equal unmarshalledValue value <| sprintf "%A  as big endian bytes should be unmarshalled correctly" value

let sortMessageHeaderFields message =
  {message with Headerfields = (message.Headerfields |> Array.sortBy (fun x -> x.FieldCode) )}
  //message

[<Tests>]
let tests =
  testList "marshalling values" [
    testCase "bool 'false' is marshalled to correct byte representations" <| fun _ ->
      let value = Primitive (Boolean (false))
      testValueMarshalling value [|0x00uy; 0x00uy; 0x00uy; 0x00uy|] [|0x00uy;0x00uy;0x00uy;0x00uy|]

    testCase "bool 'true' is marshalled to correct byte representations" <| fun _ ->
      let value = Primitive (Boolean (true))
      testValueMarshalling value [|0x01uy; 0x00uy; 0x00uy; 0x00uy|] [|0x00uy;0x00uy;0x00uy;0x01uy|]

    testCase "int16 '1000' is marshalled to correct byte representations" <| fun _ ->
      let value = Primitive (Int16 1000s)
      testValueMarshalling value [|0xE8uy; 0x03uy;|] [|0x03uy; 0xE8uy|]

    testCase "int32 '1000' is marshalled to correct byte representations" <| fun _ ->
      let value = Primitive (Int32 1000)
      testValueMarshalling value [|0xE8uy; 0x03uy; 0x00uy; 0x00uy|] [|0x00uy; 0x00uy; 0x03uy; 0xE8uy|]

    testCase "Struct (int32 '1000';bool 'true') is marshalled to correct byte representations" <| fun _ ->
      let value = Struct [|Primitive (Int32 1000); Primitive (Boolean (true))|]
      testValueMarshalling value [|0xE8uy; 0x03uy; 0x00uy; 0x00uy;  // 1000
                                   0x01uy; 0x00uy; 0x00uy; 0x00uy|] // true
                                 [|0x00uy; 0x00uy; 0x03uy; 0xE8uy;  // 1000
                                   0x00uy; 0x00uy; 0x00uy; 0x01uy|] // true

    testCase "objectpath '/myexample' is marshalled to correct byte representations" <| fun _ ->
      let value = Primitive (ObjectPath "/myexample")

      let expectedLittleEndian = [|0x0Auy; 0x00uy; 0x00uy; 0x00uy; 
                                   0x2Fuy; 0x6Duy; 0x79uy; 0x65uy; 0x78uy; 0x61uy; 0x6Duy; 0x70uy; 0x6Cuy; 0x65uy; 0x00uy|]
      let expectedBigEndian = [|0x00uy; 0x00uy; 0x00uy; 0x0Auy; 
                                0x2Fuy; 0x6Duy; 0x79uy; 0x65uy; 0x78uy; 0x61uy; 0x6Duy; 0x70uy; 0x6Cuy; 0x65uy; 0x00uy|]

      testValueMarshalling value expectedLittleEndian expectedBigEndian

    testCase "signature 'ai' is marshalled to correct byte representations" <| fun _ ->
      let value = Primitive (Signature "ai")
      
      let expectedLittleEndian = [|0x02uy;
                                   0x61uy; 0x69uy; 0x00uy|]
      let expectedBigEndian = expectedLittleEndian // single byte length only and utf8 is endianness independent

      testValueMarshalling value expectedLittleEndian expectedBigEndian

    testCase "string 'foo' is marshalled to correct byte representations" <| fun _ ->
      let value = Primitive (String "foo")
      
      let expectedLittleEndian = [|0x03uy; 0x00uy; 0x00uy; 0x00uy; 
                                   0x66uy; 0x6fuy; 0x6fuy; 0x00uy|]
      let expectedBigEndian = [|0x00uy; 0x00uy; 0x00uy; 0x03uy; 
                                0x66uy; 0x6fuy; 0x6fuy; 0x00uy|]

      testValueMarshalling value expectedLittleEndian expectedBigEndian
      
    testCase "string '+' is marshalled to correct byte representations" <| fun _ ->
      let value = Primitive (String "+")

      let expectedLittleEndian = [|0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x2Buy; 0x00uy;|]
      let expectedBigEndian = [|0x00uy; 0x00uy; 0x00uy; 0x01uy; 0x2Buy; 0x00uy;|]
      
      testValueMarshalling value expectedLittleEndian expectedBigEndian

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
    
    testCase "Empty int32[] is marshalled to correct byte representations" <| fun _ ->
      let value = Array (PrimitiveType Int32Type, [||])
      testValueMarshalling value [|0x00uy; 0x00uy; 0x00uy; 0x00uy|] [|0x00uy; 0x00uy; 0x00uy; 0x00uy|] // just the byte size (=0) as uint32

    testCase "array int64 ['5'] is marshalled to correct byte representations" <| fun _ ->
      let array = [|(Primitive <| (Int64 5L))|]
      let arrayValue = Array (DBusType.PrimitiveType Int64Type, array)

      let expectedLittleEndian = [| 
                                    8uy; 0uy; 0uy; 0uy;                       // n = 8 bytes of data
                                    0uy; 0uy; 0uy; 0uy;                       // padding to 8-byte boundary
                                    5uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy|]  // first element = 5
      let expectedBigEndian = [| 
                                    0uy; 0uy; 0uy; 8uy;                       // n = 8 bytes of data
                                    0uy; 0uy; 0uy; 0uy;                       // padding to 8-byte boundary
                                    0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 5uy|]  // first element = 5

      testValueMarshalling arrayValue expectedLittleEndian expectedBigEndian

    testCase "signal 'owner changed' marshalling should result in correct byte representation" <| fun _ ->

      let messageBody = [|(Primitive <| String ":1.1")|]
      let expectedMessage = Busy.MessageFactory.CreateSignal 2ul "/org/freedesktop/DBus" "org.freedesktop.DBus" "NameAcquired" messageBody (Some "org.freedesktop.DBus") (Some ":1.1")
                            |> sortMessageHeaderFields
      
      // use filewriter = new BinaryWriter(File.Open("testfile.bin", FileMode.Create))
      // filewriter.Write(bytesLittleEndian)

      // bytes as sent by the daemon when aqcuiring name
      let expectedBytesLittleEndian = [|
            0x6Cuy; 0x04uy; 0x01uy; 0x01uy; 0x09uy; 0x00uy; 0x00uy; 0x00uy 
            0x02uy; 0x00uy; 0x00uy; 0x00uy; 0x8Duy; 0x00uy; 0x00uy; 0x00uy 
            0x01uy; 0x01uy; 0x6Fuy; 0x00uy; 0x15uy; 0x00uy; 0x00uy; 0x00uy 
            0x2Fuy; 0x6Fuy; 0x72uy; 0x67uy; 0x2Fuy; 0x66uy; 0x72uy; 0x65uy 
            0x65uy; 0x64uy; 0x65uy; 0x73uy; 0x6Buy; 0x74uy; 0x6Fuy; 0x70uy 
            0x2Fuy; 0x44uy; 0x42uy; 0x75uy; 0x73uy; 0x00uy; 0x00uy; 0x00uy 
            0x02uy; 0x01uy; 0x73uy; 0x00uy; 0x14uy; 0x00uy; 0x00uy; 0x00uy 
            0x6Fuy; 0x72uy; 0x67uy; 0x2Euy; 0x66uy; 0x72uy; 0x65uy; 0x65uy 
            0x64uy; 0x65uy; 0x73uy; 0x6Buy; 0x74uy; 0x6Fuy; 0x70uy; 0x2Euy 
            0x44uy; 0x42uy; 0x75uy; 0x73uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy 
            0x03uy; 0x01uy; 0x73uy; 0x00uy; 0x0Cuy; 0x00uy; 0x00uy; 0x00uy 
            0x4Euy; 0x61uy; 0x6Duy; 0x65uy; 0x41uy; 0x63uy; 0x71uy; 0x75uy 
            0x69uy; 0x72uy; 0x65uy; 0x64uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy 
            0x06uy; 0x01uy; 0x73uy; 0x00uy; 0x04uy; 0x00uy; 0x00uy; 0x00uy 
            0x3Auy; 0x31uy; 0x2Euy; 0x31uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy 
            0x08uy; 0x01uy; 0x67uy; 0x00uy; 0x01uy; 0x73uy; 0x00uy; 0x00uy 
            0x07uy; 0x01uy; 0x73uy; 0x00uy; 0x14uy; 0x00uy; 0x00uy; 0x00uy 
            0x6Fuy; 0x72uy; 0x67uy; 0x2Euy; 0x66uy; 0x72uy; 0x65uy; 0x65uy 
            0x64uy; 0x65uy; 0x73uy; 0x6Buy; 0x74uy; 0x6Fuy; 0x70uy; 0x2Euy 
            0x44uy; 0x42uy; 0x75uy; 0x73uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy 
            0x04uy; 0x00uy; 0x00uy; 0x00uy; 0x3Auy; 0x31uy; 0x2Euy; 0x31uy 
            0x00uy;
      |]
      
      let unmarshalledMessage = unmarshallMessage (ArrayByteProvider(expectedBytesLittleEndian))
      
      match unmarshalledMessage with
      | Error e -> Expect.isOk unmarshalledMessage e
      | Ok (unmarshalledMessage) -> 
            Expect.equal (unmarshalledMessage |> sortMessageHeaderFields) expectedMessage "signal 'owner changed' unmarshalling should result in original message"
            let bytesLittleEndian = marshallMessage unmarshalledMessage
            Expect.equal bytesLittleEndian expectedBytesLittleEndian "signal 'owner changed' marshalling should result in correct byte representation"


    testCase "method call 'list names' marshalling should result in correct byte representation" <| fun _ ->
      let messageBody = [||]
      let expectedMessage = Busy.MessageFactory.CreateMethodCall 2ul "/org/freedesktop/DBus" (Some "org.freedesktop.DBus") "ListNames" messageBody (Some ":1.8") (Some "org.freedesktop.DBus") 
                            |> sortMessageHeaderFields

      // bytes as sent by the daemon when aqcuiring name
      let expectedBytesLittleEndian = [|
            0x6Cuy; 0x01uy; 0x00uy; 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy
            0x02uy; 0x00uy; 0x00uy; 0x00uy; 0x85uy; 0x00uy; 0x00uy; 0x00uy
            0x01uy; 0x01uy; 0x6Fuy; 0x00uy; 0x15uy; 0x00uy; 0x00uy; 0x00uy
            0x2Fuy; 0x6Fuy; 0x72uy; 0x67uy; 0x2Fuy; 0x66uy; 0x72uy; 0x65uy
            0x65uy; 0x64uy; 0x65uy; 0x73uy; 0x6Buy; 0x74uy; 0x6Fuy; 0x70uy
            0x2Fuy; 0x44uy; 0x42uy; 0x75uy; 0x73uy; 0x00uy; 0x00uy; 0x00uy
            0x02uy; 0x01uy; 0x73uy; 0x00uy; 0x14uy; 0x00uy; 0x00uy; 0x00uy
            0x6Fuy; 0x72uy; 0x67uy; 0x2Euy; 0x66uy; 0x72uy; 0x65uy; 0x65uy
            0x64uy; 0x65uy; 0x73uy; 0x6Buy; 0x74uy; 0x6Fuy; 0x70uy; 0x2Euy
            0x44uy; 0x42uy; 0x75uy; 0x73uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy
            0x03uy; 0x01uy; 0x73uy; 0x00uy; 0x09uy; 0x00uy; 0x00uy; 0x00uy
            0x4Cuy; 0x69uy; 0x73uy; 0x74uy; 0x4Euy; 0x61uy; 0x6Duy; 0x65uy
            0x73uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy
            0x06uy; 0x01uy; 0x73uy; 0x00uy; 0x14uy; 0x00uy; 0x00uy; 0x00uy
            0x6Fuy; 0x72uy; 0x67uy; 0x2Euy; 0x66uy; 0x72uy; 0x65uy; 0x65uy
            0x64uy; 0x65uy; 0x73uy; 0x6Buy; 0x74uy; 0x6Fuy; 0x70uy; 0x2Euy
            0x44uy; 0x42uy; 0x75uy; 0x73uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy
            0x07uy; 0x01uy; 0x73uy; 0x00uy; 0x04uy; 0x00uy; 0x00uy; 0x00uy
            0x3Auy; 0x31uy; 0x2Euy; 0x38uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy
      |]

      //let unmarshalledMessage = unmarshallMessage (arrayByteProvider bytesLittleEndian)
      let unmarshalledMessage = unmarshallMessage (ArrayByteProvider(expectedBytesLittleEndian))
      match unmarshalledMessage with
      | Error e -> Expect.isOk unmarshalledMessage e
      | Ok (unmarshalledMessage) -> 
              Expect.equal (unmarshalledMessage |> sortMessageHeaderFields) expectedMessage "method call unmarshalling should result in original message"
              let bytesLittleEndian = marshallMessage unmarshalledMessage
              Expect.equal bytesLittleEndian expectedBytesLittleEndian "method call marshalling should result in correct byte representation"

    testCase "method return 'list names' marshalling should result in correct byte representation" <| fun _ ->
      let messageBody = [|Array (PrimitiveType StringType,
                           [|Primitive (String "org.freedesktop.DBus"); Primitive (String ":1.8")|])|];
      let expectedMessage = Busy.MessageFactory.CreateMethodReturn 3ul 2ul messageBody (Some "org.freedesktop.DBus") (Some ":1.8")
                            |> sortMessageHeaderFields
      // bytes as sent by the daemon when aqcuiring name
      let expectedBytesLittleEndian = [|
            0x6Cuy; 0x02uy; 0x01uy; 0x01uy; 0x29uy; 0x00uy; 0x00uy; 0x00uy 
            0x03uy; 0x00uy; 0x00uy; 0x00uy; 0x3Duy; 0x00uy; 0x00uy; 0x00uy 
            0x06uy; 0x01uy; 0x73uy; 0x00uy; 0x04uy; 0x00uy; 0x00uy; 0x00uy 
            0x3Auy; 0x31uy; 0x2Euy; 0x38uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy 
            0x05uy; 0x01uy; 0x75uy; 0x00uy; 0x02uy; 0x00uy; 0x00uy; 0x00uy 
            0x08uy; 0x01uy; 0x67uy; 0x00uy; 0x02uy; 0x61uy; 0x73uy; 0x00uy 
            0x07uy; 0x01uy; 0x73uy; 0x00uy; 0x14uy; 0x00uy; 0x00uy; 0x00uy 
            0x6Fuy; 0x72uy; 0x67uy; 0x2Euy; 0x66uy; 0x72uy; 0x65uy; 0x65uy 
            0x64uy; 0x65uy; 0x73uy; 0x6Buy; 0x74uy; 0x6Fuy; 0x70uy; 0x2Euy 
            0x44uy; 0x42uy; 0x75uy; 0x73uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy 
            0x25uy; 0x00uy; 0x00uy; 0x00uy; 0x14uy; 0x00uy; 0x00uy; 0x00uy 
            0x6Fuy; 0x72uy; 0x67uy; 0x2Euy; 0x66uy; 0x72uy; 0x65uy; 0x65uy 
            0x64uy; 0x65uy; 0x73uy; 0x6Buy; 0x74uy; 0x6Fuy; 0x70uy; 0x2Euy 
            0x44uy; 0x42uy; 0x75uy; 0x73uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy 
            0x04uy; 0x00uy; 0x00uy; 0x00uy; 0x3Auy; 0x31uy; 0x2Euy; 0x38uy 
            0x00uy
      |]

      //let unmarshalledMessage = unmarshallMessage (arrayByteProvider bytesLittleEndian)
      let unmarshalledMessage = unmarshallMessage <| ArrayByteProvider(expectedBytesLittleEndian)
      match unmarshalledMessage with
      | Error e -> Expect.isOk unmarshalledMessage e
      | Ok (unmarshalledMessage) -> 
              Expect.equal (unmarshalledMessage |> sortMessageHeaderFields) expectedMessage "method return unmarshalling should result in original message"
              let bytesLittleEndian = marshallMessage unmarshalledMessage
              Expect.equal bytesLittleEndian expectedBytesLittleEndian "method return marshalling should result in correct byte representation"

    testCase "method error 'list names' marshalling should result in correct byte representation" <| fun _ ->
      let messageBody = [|Primitive (String "org.freedesktop.DBus does not understand message ListName")|];
      let expectedMessage = Busy.MessageFactory.CreateError 3ul 2ul "org.freedesktop.DBus.Error.UnknownMethod" messageBody (Some "org.freedesktop.DBus") (Some ":1.6")
                            |> sortMessageHeaderFields

      
      // bytes as sent by the daemon when aqcuiring name
      let expectedBytesLittleEndian = [|
            0x6Cuy; 0x03uy; 0x01uy; 0x01uy; 0x3Euy; 0x00uy; 0x00uy; 0x00uy 
            0x03uy; 0x00uy; 0x00uy; 0x00uy; 0x75uy; 0x00uy; 0x00uy; 0x00uy 
            0x06uy; 0x01uy; 0x73uy; 0x00uy; 0x04uy; 0x00uy; 0x00uy; 0x00uy 
            0x3Auy; 0x31uy; 0x2Euy; 0x36uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy 
            0x04uy; 0x01uy; 0x73uy; 0x00uy; 0x28uy; 0x00uy; 0x00uy; 0x00uy 
            0x6Fuy; 0x72uy; 0x67uy; 0x2Euy; 0x66uy; 0x72uy; 0x65uy; 0x65uy 
            0x64uy; 0x65uy; 0x73uy; 0x6Buy; 0x74uy; 0x6Fuy; 0x70uy; 0x2Euy 
            0x44uy; 0x42uy; 0x75uy; 0x73uy; 0x2Euy; 0x45uy; 0x72uy; 0x72uy 
            0x6Fuy; 0x72uy; 0x2Euy; 0x55uy; 0x6Euy; 0x6Buy; 0x6Euy; 0x6Fuy 
            0x77uy; 0x6Euy; 0x4Duy; 0x65uy; 0x74uy; 0x68uy; 0x6Fuy; 0x64uy 
            0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy 
            0x05uy; 0x01uy; 0x75uy; 0x00uy; 0x02uy; 0x00uy; 0x00uy; 0x00uy 
            0x08uy; 0x01uy; 0x67uy; 0x00uy; 0x01uy; 0x73uy; 0x00uy; 0x00uy 
            0x07uy; 0x01uy; 0x73uy; 0x00uy; 0x14uy; 0x00uy; 0x00uy; 0x00uy 
            0x6Fuy; 0x72uy; 0x67uy; 0x2Euy; 0x66uy; 0x72uy; 0x65uy; 0x65uy 
            0x64uy; 0x65uy; 0x73uy; 0x6Buy; 0x74uy; 0x6Fuy; 0x70uy; 0x2Euy 
            0x44uy; 0x42uy; 0x75uy; 0x73uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy 
            0x39uy; 0x00uy; 0x00uy; 0x00uy; 0x6Fuy; 0x72uy; 0x67uy; 0x2Euy 
            0x66uy; 0x72uy; 0x65uy; 0x65uy; 0x64uy; 0x65uy; 0x73uy; 0x6Buy 
            0x74uy; 0x6Fuy; 0x70uy; 0x2Euy; 0x44uy; 0x42uy; 0x75uy; 0x73uy 
            0x20uy; 0x64uy; 0x6Fuy; 0x65uy; 0x73uy; 0x20uy; 0x6Euy; 0x6Fuy 
            0x74uy; 0x20uy; 0x75uy; 0x6Euy; 0x64uy; 0x65uy; 0x72uy; 0x73uy 
            0x74uy; 0x61uy; 0x6Euy; 0x64uy; 0x20uy; 0x6Duy; 0x65uy; 0x73uy 
            0x73uy; 0x61uy; 0x67uy; 0x65uy; 0x20uy; 0x4Cuy; 0x69uy; 0x73uy 
            0x74uy; 0x4Euy; 0x61uy; 0x6Duy; 0x65uy; 0x00uy;
             |]

      //let unmarshalledMessage = unmarshallMessage (arrayByteProvider bytesLittleEndian)
      let unmarshalledMessage = unmarshallMessage <| ArrayByteProvider(expectedBytesLittleEndian)
      match unmarshalledMessage with
      | Error e -> Expect.isOk unmarshalledMessage e
      | Ok (unmarshalledMessage) -> 
              Expect.equal (unmarshalledMessage |> sortMessageHeaderFields) expectedMessage "method error unmarshalling should result in original message"
              let bytesLittleEndian = marshallMessage unmarshalledMessage
              Expect.equal bytesLittleEndian expectedBytesLittleEndian "method error marshalling should result in correct byte representation"

  ]