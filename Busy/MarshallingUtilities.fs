namespace Busy

open Types
open MessageTypes

module MarshallingUtilities =
    
    type StreamPosition = int

    type IByteProvider =
       abstract member ReadBytes: int -> byte[]

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

    let internal fromUtf8Bytes = System.Text.Encoding.UTF8.GetString
    let internal toUtf8Bytes (s:string)  = System.Text.Encoding.UTF8.GetBytes s

    let internal byteSize (dbusType:DBusPrimitiveType) = alignment <| PrimitiveType dbusType

    let internal paddingSize (streamPosition:StreamPosition) (alignment:int) = 
        (alignment - ((int32) (streamPosition % (alignment)))) % alignment

    let internal messageHeaderFieldsToFieldValueArray (fields:DBusMessageHeaderFields) =
        let mapper (f:('a -> DBusMessageHeaderFieldValue)) (x:'a option) = match x with Some y -> [|f y|] | None -> [||]

        Array.concat [|
            fields.ObjectPath |> mapper Path
            fields.Interface |> mapper Interface
            fields.Member |> mapper Member
            fields.Destination |> mapper Destination
            fields.ErrorName |> mapper ErrorName
            fields.ReplySerial |> mapper ReplySerial
            fields.BodySignature |> mapper Signature
            fields.Sender |> mapper Sender
        |]

    let internal messageFieldValueArrayToMessageHeaderFields (values:DBusMessageHeaderFieldValue[])  =
        values 
        |> Array.fold (fun acc x -> 
                                        match x with 
                                        | Path p -> {acc with ObjectPath = Some(p)}
                                        | Interface i -> {acc with Interface = Some(i)}
                                        | Member m -> {acc with Member = Some(m)}
                                        | ErrorName e -> {acc with ErrorName = Some(e)}
                                        | ReplySerial rs -> {acc with ReplySerial = Some(rs)}
                                        | Destination d -> {acc with Destination = Some(d)}
                                        | Sender s -> {acc with Sender = Some(s)}
                                        | Signature s -> {acc with BodySignature = Some(s)}
                                        // Todo | UnixFds u -> {acc with UnixFds = Some(u)}
                                        | _ -> acc
            ) Utilities.emptyDBusMessageHeaderFields