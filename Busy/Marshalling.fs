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

        | Variant v -> failwith "Not Implemented"
        | Dict (_, _) -> failwith "Not Implemented"  

         