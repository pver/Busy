namespace Busy

open Types
open MessageTypes

module Marshalling =

    type MarshalledValue = {alignment:int; bytes:byte[]}

    let internal nul = [|0x00uy|]

    let internal alignment (dbusType:DBusType) =
        
        match dbusType with
        | PrimitiveType p -> match p with
                             | InvalidType | ReservedType -> 0
                             | ByteType | SignatureType -> 1 
                             | Int16Type | Uint16Type -> 2
                             | Int64Type | Uint64Type | DoubleType -> 8
                             | StringType | ObjectPathType | BooleanType | Int32Type | Uint32Type | UnixFdType -> 4 

        | VariantType -> 1
        | ArrayType _ -> 4
        | StructType _ | DictType _ -> 8


    let rec marshall (endianness:DBusMessageEndianness) (value:DBusValue) : seq<MarshalledValue> =
        let applyEndianness = match System.BitConverter.IsLittleEndian && (endianness<>DBusMessageEndianness.LittleEndian) with
                              | true -> Array.rev
                              | false -> id
        
        let sizeToAlignment (alignment:int) (bytes:byte[])  =
            let padding = Array.init ((Array.length bytes) % alignment) (fun _ -> 0uy)
            let paddedBytes = match System.BitConverter.IsLittleEndian with
                              | true -> Array.append bytes padding
                              | false -> Array.append padding bytes
            paddedBytes

        seq {
            let alignment = alignment value.Type
            match value with
            | Primitive p -> 
                             let bytes = 
                                 match p with
                                 | DBusPrimitiveValue.Invalid | Reserved -> [||]
                                 | Int32 i ->  System.BitConverter.GetBytes(i) |> sizeToAlignment alignment |> applyEndianness

                                 | String s -> let stringbytes = Array.append (System.Text.Encoding.UTF8.GetBytes(s)) nul
                                               let stringLengthBytes = marshall endianness <| Primitive (Types.Uint32 <| uint32 s.Length)
                                                                       |> Seq.head |> fun x -> x.bytes
                                               Array.append stringLengthBytes stringbytes
                                 | Byte b -> [| b |]
                                 | Boolean b -> let value = if b then 1ul else 0ul
                                                let marshalledUInt = marshall endianness <| Primitive (Types.Uint32 value)
                                                marshalledUInt |> Seq.head |> fun x -> x.bytes
                                 | Int16 i -> System.BitConverter.GetBytes(i) |> sizeToAlignment alignment |> applyEndianness
                                 | Uint16 i -> System.BitConverter.GetBytes(i) |> sizeToAlignment alignment |> applyEndianness
                                 | Uint32 i -> System.BitConverter.GetBytes(i) |> sizeToAlignment alignment |> applyEndianness
                                 | Int64 i -> System.BitConverter.GetBytes(i) |> sizeToAlignment alignment |> applyEndianness
                                 | Uint64 i -> System.BitConverter.GetBytes(i) |> sizeToAlignment alignment |> applyEndianness
                                 | Double i -> System.BitConverter.GetBytes(i) |> sizeToAlignment alignment |> applyEndianness
                                 | ObjectPath op -> let marshalledString = marshall endianness <| Primitive (Types.String op)
                                                    marshalledString |> Seq.head |> fun x -> x.bytes
                                 | DBusPrimitiveValue.Signature s -> 
                                                let stringbytes = Array.append (System.Text.Encoding.UTF8.GetBytes(s)) nul
                                                let stringLengthBytes = marshall endianness <| Primitive (Types.Byte ((byte) s.Length))
                                                                        |> Seq.head |> fun x -> x.bytes
                                                Array.append stringLengthBytes stringbytes
                                 | UnixFd _ -> failwith "Not Implemented"  
                             yield {alignment=alignment; bytes=bytes}

            | Array (_, items) -> let marshalledSize = marshall endianness <| Primitive (Types.Uint32 <| uint32 items.Length)
                                                       |> Seq.head
                                  yield marshalledSize
                                  let marshalledItems = items |> Seq.collect (fun x -> marshall endianness x )
                                  yield! marshalledItems

            | Struct s -> yield {alignment=alignment; bytes=[||]} // no content, just to align struct start position
                          yield! (s |> Seq.collect (fun x -> marshall endianness x ) )
            | Variant(_) -> failwith "Not Implemented"
            | Dict (_, _) -> failwith "Not Implemented"  
        }
         