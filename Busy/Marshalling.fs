namespace Busy

open Types
open MessageTypes
open MarshallingUtilities

module Marshalling =

    let internal padToAlignment (streamPosition) (alignment:int) =
            let paddingSize = (alignment - ((int32) (streamPosition % (alignment)))) % alignment
            Array.init paddingSize (fun _ -> 0x00uy)

    let rec marshall (streamPosition:int) (endianness:DBusMessageEndianness) (value:DBusValue) : byte[] =
        let applyEndianness = match System.BitConverter.IsLittleEndian && (endianness<>DBusMessageEndianness.LittleEndian) with
                              | true -> Array.rev
                              | false -> id

        let padding = padToAlignment streamPosition <| alignment value.Type
        let posAfterPadding = streamPosition + (Array.length padding)
        match value with
        | Primitive p -> 
                             let bytes = 
                                 match p with
                                 | DBusPrimitiveValue.Invalid | Reserved -> [||]
                                 | Int32 i ->  System.BitConverter.GetBytes(i) |> applyEndianness

                                 | String s -> let utfBytes = System.Text.Encoding.UTF8.GetBytes(s)
                                               let stringBytes = Array.append utfBytes nul
                                               let stringLength = Array.length utfBytes |> uint32
                                               
                                               Array.append (System.BitConverter.GetBytes(stringLength) |> applyEndianness) stringBytes
                                 | Byte b -> [| b |]
                                 | Boolean b -> let value = if b then 1ul else 0ul
                                                System.BitConverter.GetBytes(value) |> applyEndianness
                                 | Int16 i -> System.BitConverter.GetBytes(i) |> applyEndianness
                                 | Uint16 i -> System.BitConverter.GetBytes(i) |> applyEndianness
                                 | Uint32 i -> System.BitConverter.GetBytes(i) |> applyEndianness
                                 | Int64 i -> System.BitConverter.GetBytes(i) |> applyEndianness
                                 | Uint64 i -> System.BitConverter.GetBytes(i) |> applyEndianness
                                 | Double i -> System.BitConverter.GetBytes(i) |> applyEndianness
                                 | ObjectPath op -> marshall posAfterPadding endianness <| Primitive (Types.String op)
                                 | DBusPrimitiveValue.Signature s -> 
                                               let utfBytes = System.Text.Encoding.UTF8.GetBytes(s)
                                               let stringBytes = Array.append utfBytes nul
                                               let stringLength = Array.length utfBytes |> byte
                                               
                                               Array.append [| stringLength |] stringBytes
                                 | UnixFd _ -> failwith "Not Implemented"  
                             
                             Array.append padding bytes

        | Array (contentType, items) -> 
                                let contentTypeAlignment = alignment contentType
                                let posAfterLengthBytes = (posAfterPadding + alignment (PrimitiveType Uint32Type))
                                let paddingBeforeContent = padToAlignment posAfterLengthBytes contentTypeAlignment // pad here, because this padding should not be counted in the content size
                                
                                let posAtContentStart = posAfterLengthBytes + Array.length paddingBeforeContent
                                let contentBytes = items |> Array.fold (fun acc x -> Array.append acc <| marshall (posAtContentStart+acc.Length) endianness x) [||]
                                
                                let lengthBytes = System.BitConverter.GetBytes(uint32 <| Array.length contentBytes) |> applyEndianness

                                Array.concat [|padding; lengthBytes; paddingBeforeContent; contentBytes |]

        | Struct s -> let contentBytes = s |> Seq.fold (fun acc x -> Array.append acc <| marshall (posAfterPadding+acc.Length) endianness x) [||]
                      Array.append padding contentBytes

        | Variant v -> let signature = Primitive <| DBusPrimitiveValue.Signature v.Type.Signature
                       let signatureBytes = marshall posAfterPadding endianness signature
                       let valueBytes = marshall (posAfterPadding + Array.length signatureBytes) endianness v
                       Array.concat [|padding; signatureBytes; valueBytes |] 

        | Dict (_, _) -> failwith "Not Implemented"  

    let marshallMessage (message:DBusMessage) : byte[] =
        let endianness = message.Endianness
        let bodyBytes = message.Body |> Seq.fold (fun acc x -> Array.append acc <| marshall acc.Length endianness x) [||]
        
        let headerFieldsArrayType = (StructType [PrimitiveType ByteType; VariantType])

        let headerFieldToDbusValue (field:DBusMessageHeaderFields) = 
            let dbusValue = match field with
                            | Path s -> Primitive <| ObjectPath s
                            | Interface i -> Primitive <| String i
                            | Member m -> Primitive <| String m
                            | ErrorName n -> Primitive <| String n
                            | ReplySerial s -> Primitive <| Uint32 s
                            | Destination d -> Primitive <| String d
                            | Sender s -> Primitive <| String s
                            | Signature s -> Primitive <| DBusPrimitiveValue.Signature s
                            | UnixFds _ -> failwith "Not Implemented"                            
                            | Invalid -> failwith "Invalid field found"

            Struct ([Primitive <| DBusPrimitiveValue.Byte ((byte) field.FieldCode); Variant dbusValue])

        let headerFieldsValues = message.Headerfields |> Seq.map headerFieldToDbusValue |> Seq.toArray

        let headerValues = [
                            Primitive <| DBusPrimitiveValue.Byte ((byte) message.Endianness)
                            Primitive <| DBusPrimitiveValue.Byte ((byte) message.MessageType)
                            Primitive <| DBusPrimitiveValue.Byte (message.Flags |> Seq.fold (fun acc f -> acc ||| ((byte) f) ) 0x00uy)
                            Primitive <| DBusPrimitiveValue.Byte message.ProtocolVersion
                            Primitive <| DBusPrimitiveValue.Uint32 ((uint32) bodyBytes.Length)
                            Primitive <| DBusPrimitiveValue.Uint32 message.SequenceNumber
                            Array <| (headerFieldsArrayType, headerFieldsValues)
        ]

        let headerBytes = headerValues |> Seq.fold (fun acc x -> Array.append acc <| marshall acc.Length endianness x) [||]
        
        let padding = padToAlignment (headerBytes.Length) 8

        Array.concat [|headerBytes; padding; bodyBytes|]


         