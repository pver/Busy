namespace Busy

open Types
open MessageTypes
open Utilities
open MarshallingUtilities
open System

module rec Unmarshalling =

    /// Function providing a number of bytes from start position and with a certain length
    type ByteProvider = StreamPosition -> int -> byte[]

    let rec unmarshall (getbytes:ByteProvider) (streamPosition:StreamPosition) (endianness:DBusMessageEndianness) (valueType:DBusType) : Result<DBusValue*StreamPosition, string> =
        let applyEndianness = match System.BitConverter.IsLittleEndian && (endianness<>DBusMessageEndianness.LittleEndian) with
                              | true -> Array.rev
                              | false -> id

        let posAfterPadding = (+) streamPosition << paddingSize streamPosition <| alignment valueType 

        match valueType with
        | PrimitiveType p ->
                                let sizeOfType = byteSize p
                                let posAfterValue = posAfterPadding + sizeOfType
                                let bytes = getbytes posAfterPadding sizeOfType |> applyEndianness

                                let stringFromBytesLength (length:int) (constr:string->DBusPrimitiveValue) = 
                                    let utf8bytes = getbytes posAfterValue length
                                    let value = fromUtf8Bytes utf8bytes
                                    let dbusvalue = constr value
                                    Ok (dbusvalue, posAfterValue + length + 1) // +1 for added nul

                                let unmarshalledValue = 
                                    match p with
                                    | Int16Type -> let value = DBusPrimitiveValue.Int16 <| BitConverter.ToInt16(bytes,0)
                                                   Ok (value, posAfterValue)
                                    | InvalidType -> Error "InvalidType cannot be unmarshalled"
                                    | ByteType ->  let value = DBusPrimitiveValue.Byte (bytes |> Array.head)
                                                   Ok (value, posAfterValue)
                                    | BooleanType -> match BitConverter.ToUInt32(bytes,0) with
                                                     | 0ul -> Ok (DBusPrimitiveValue.Boolean false, posAfterValue)
                                                     | 1ul -> Ok (DBusPrimitiveValue.Boolean true, posAfterValue)
                                                     | v -> Error <| sprintf "Invalid boolean value: %d" v
                                    | Uint16Type ->  let value = DBusPrimitiveValue.Uint16 <| BitConverter.ToUInt16(bytes,0)
                                                     Ok (value, posAfterValue)
                                    | Int32Type -> let value = DBusPrimitiveValue.Int32 <| BitConverter.ToInt32(bytes,0)
                                                   Ok (value, posAfterValue)
                                    | Uint32Type -> let value = DBusPrimitiveValue.Uint32 <| BitConverter.ToUInt32(bytes,0)
                                                    Ok (value, posAfterValue)
                                    | Int64Type ->  let value = DBusPrimitiveValue.Int64 <| BitConverter.ToInt64(bytes,0)
                                                    Ok (value, posAfterValue)
                                    | Uint64Type -> let value = DBusPrimitiveValue.Uint64 <| BitConverter.ToUInt64(bytes,0)
                                                    Ok (value, posAfterValue)
                                    | DoubleType -> let value = DBusPrimitiveValue.Double <| BitConverter.ToDouble(bytes,0)
                                                    Ok (value, posAfterValue)

                                    | StringType -> stringFromBytesLength (int <| BitConverter.ToUInt32(bytes,0)) (DBusPrimitiveValue.String)
                                    | ObjectPathType -> stringFromBytesLength (int <| BitConverter.ToUInt32(bytes,0)) (DBusPrimitiveValue.ObjectPath)
                                    | SignatureType ->  stringFromBytesLength (int <| Array.head bytes) (DBusPrimitiveValue.Signature)

                                    | UnixFdType -> Error "Not Implemented"
                                    | ReservedType -> Error "ReservedType cannot be unmarshalled"
                                
                                unmarshalledValue 
                                |> Result.map (fun (v,p) -> Primitive v, p)

        | StructType types -> unmarshallValues getbytes posAfterPadding endianness (Seq.toArray types)
                              |> Result.map (fun (values, newPos) -> Struct values, newPos)
                           
        | ArrayType arrayType -> 
                            unmarshall getbytes posAfterPadding endianness (PrimitiveType Uint32Type)
                            |> Result.bind(fun (dbusLengthValue, posAfterLengthValue) -> 
                                match dbusLengthValue with
                                | Primitive (Uint32 lengthValue) -> 
                                                                    let contentLength = (int) lengthValue
                                                                    let startPosOfContent = (+) posAfterLengthValue (paddingSize posAfterLengthValue <| alignment arrayType )
                                                                    let endPosOfContent = startPosOfContent + contentLength
                                                                    let contentByteProvider = arrayByteProvider <| getbytes startPosOfContent contentLength
                                                                    let rec getArrayValues (acc:Result<DBusValue*StreamPosition, string>) = 
                                                                        acc
                                                                        |> Result.bind (fun (accVal,pos) ->
                                                                                                            if pos >= contentLength
                                                                                                            then Ok(accVal,pos)
                                                                                                            else
                                                                                                                match accVal with
                                                                                                                | DBusValue.Array (arrayType, contentValues) ->
                                                                                                                            unmarshall contentByteProvider pos endianness arrayType
                                                                                                                            |> Result.bind (fun (nextVal,nextPos) -> getArrayValues (Ok(DBusValue.Array (arrayType, Array.append contentValues [|nextVal|]),nextPos))   )
                                                                                                                | _ -> Error "Expected Array acc value while parsing array"
                                                                        )

                                                                    getArrayValues (Ok(DBusValue.Array (arrayType, [||]),0))
                                                                    |> Result.map (fun (arrVal,_) -> arrVal,endPosOfContent)
                                                                    
                                | unexpected -> Error <| sprintf "Expected UInt content length value while parsing array, but found %A" unexpected.Type
                            )

        | VariantType -> 
                       unmarshall getbytes posAfterPadding endianness (PrimitiveType DBusPrimitiveType.SignatureType)
                       |> Result.bind (fun (signatureValue, posAfterSignature)->
                            match signatureValue with
                            | Primitive (DBusPrimitiveValue.Signature s) -> 
                                    let parsedSignatureTypes = Utilities.ParseSignatureToDBusTypes s
                                    match parsedSignatureTypes with
                                    | Ok signatureTypes -> unmarshall getbytes posAfterSignature endianness (signatureTypes|>Seq.head)
                                    | Error _ -> Error (sprintf "Invalid signature '%s' found while parsing variant" s)
                            | _ -> Error "Expected Signature value while parsing variant"
                        )

        | DictType(_) -> Error "Not Implemented"

    let rec unmarshallValues (getbytes:ByteProvider) (streamPosition:StreamPosition) (endianness:DBusMessageEndianness) (valueTypes:DBusType[]) : Result<DBusValue[]*StreamPosition, string> =
        let accStart = Ok ([||], streamPosition)
        valueTypes 
        |> Seq.fold (fun acc valueType -> 
             acc |> Result.bind (fun (accValues, accPos) -> 
                      unmarshall getbytes accPos endianness valueType
                      |> Result.map (fun (value, newAccPos) -> (Array.append accValues [|value|]), newAccPos) 
                 )

        ) accStart

    let internal unmarshallHeaderValues (getbytes:ByteProvider) (streamPosition:StreamPosition) (endianness:DBusMessageEndianness) =
        let headerValueTypes = [| PrimitiveType ByteType;
                                  PrimitiveType ByteType;
                                  PrimitiveType ByteType;
                                  PrimitiveType Uint32Type;
                                  PrimitiveType Uint32Type;
                                  ArrayType (StructType [|PrimitiveType ByteType; VariantType|])
                                  |] // we already have the endianness, excluding it from the beginning of internal header values
        unmarshallValues getbytes streamPosition endianness headerValueTypes


    let internal getHeaderField (fieldCode:byte) (fieldValue:DBusValue) =
        match (fieldCode, fieldValue) with
        | (0uy, _) -> Error "Header field Invalid found in header"
        | (1uy, Primitive (ObjectPath path)) -> Ok (Some (DBusMessageHeaderFields.Path path))
        | (2uy, Primitive (String i)) -> Ok (Some (DBusMessageHeaderFields.Interface i))
        | (3uy, Primitive (String m)) -> Ok (Some (DBusMessageHeaderFields.Member m))
        | (4uy, Primitive (String e)) -> Ok (Some (DBusMessageHeaderFields.ErrorName e))
        | (5uy, Primitive (Uint32 s)) -> Ok (Some (DBusMessageHeaderFields.ReplySerial s))
        | (6uy, Primitive (String d)) -> Ok (Some (DBusMessageHeaderFields.Destination d))
        | (7uy, Primitive (String s)) -> Ok (Some (DBusMessageHeaderFields.Sender s))
        | (8uy, Primitive (DBusPrimitiveValue.Signature s)) -> Ok (Some (DBusMessageHeaderFields.Signature s))
        | (9uy, Primitive (Uint32 u)) -> Ok (Some (DBusMessageHeaderFields.UnixFds u))
        | (1uy, _) -> Error "Invalid value type for Path header field"
        | (2uy, _) -> Error "Invalid value type for Interface header field"
        | (3uy, _) -> Error "Invalid value type for Member header field"
        | (4uy, _) -> Error "Invalid value type for ErrorName header field"
        | (5uy, _) -> Error "Invalid value type for ReplySerial header field"
        | (6uy, _) -> Error "Invalid value type for Destination header field"
        | (7uy, _) -> Error "Invalid value type for Sender header field"
        | (8uy, _) -> Error "Invalid value type for Signature header field"
        | (9uy, _) -> Error "Invalid value type for Path header field"
        | _ -> Ok (None) // unknown fields should be accepted, but ignored

    let internal unmarshallHeaderFields headerFields =
        let structTypes = [|PrimitiveType ByteType; VariantType|]
        // todo: make this return Result instead of failWith!
        let fieldStructs = match headerFields with
                           | DBusValue.Array (StructType st, x) when (Seq.toArray st) = structTypes -> x
                           | x -> failwith <| sprintf "Invalid headerFields type: %A" x

        let accStart = Ok ([||])
        fieldStructs
        |> Seq.fold (fun acc structType -> 
             acc |> Result.bind (fun (accValues) -> 
                      match structType with
                      | Struct [|Primitive (Byte fieldCode); fieldValue|] -> 
                                        let v = getHeaderField fieldCode fieldValue
                                        match v with
                                        | Ok None -> Ok (accValues)
                                        | Ok (Some hf) -> Ok (Array.append accValues [|hf|])
                                        | Error e -> Error e
                            
                      | e -> Error <| sprintf "Invalid header field type found: %A" e 
                 )

        ) accStart

    let internal unmarshallEndiannessFromByte endiannessByte =
        match endiannessByte with
        | v when v = byte DBusMessageEndianness.LittleEndian -> Ok(DBusMessageEndianness.LittleEndian)
        | v when v = byte DBusMessageEndianness.BigEndian -> Ok(DBusMessageEndianness.BigEndian)
        | unknown -> Error <| sprintf "Invalid endianness byte in message bytes: %A" unknown

    let internal unboxUint32 value = match value with Primitive (Uint32 x) -> Ok(x) | x -> Error (sprintf "Invalid DBusValue type, expected Primitive Uint32, but got %A" x.Type)
    let internal unboxByte value = match value with Primitive (Byte x) -> Ok(x) | x -> Error (sprintf "Invalid DBusValue type, expected Primitive Byte, but got %A" x.Type)       

    let private unmarshallBodyLength bodyLength = unboxUint32 bodyLength |> prependError "Invalid bodyLength type."
    let private unmarshallSequenceNumber sequenceNumber = unboxUint32 sequenceNumber |> prependError "Invalid sequenceNumber type."
    let private unmarshallFlagsByte flagsByte = unboxByte flagsByte |> prependError "Invalid flags type."
    let private unmarshallMessageType messageType = unboxByte messageType |> prependError "Invalid message type."

    let unmarshallMessage (getbytes:ByteProvider) : Result<DBusMessage,string> =
        let endiannessByte = getbytes 0 1 |> Array.head
        
        result {
            let! endianness = unmarshallEndiannessFromByte endiannessByte
            let! (headerValues, posAfterHeader) = unmarshallHeaderValues getbytes 1 endianness
            let! messageType = unmarshallMessageType headerValues.[0]
            let! bodyLength = unmarshallBodyLength headerValues.[3]
            let! sequenceNumber = unmarshallSequenceNumber headerValues.[4]
            let! messageFlagsByte = unmarshallFlagsByte headerValues.[1]
            let! headerFields = unmarshallHeaderFields headerValues.[5]

            let bodySignature = headerFields 
                                |> Array.choose (fun x -> match x with DBusMessageHeaderFields.Signature s -> Some s | _ -> None)
                                |> Array.tryHead
            
            let body = match bodySignature with
                       | Some (s) -> 
                                        let bodyTypes = Utilities.ParseSignatureToDBusTypes s
                                        let startBodyPos = posAfterHeader + (paddingSize posAfterHeader 8)
                                        match bodyTypes with
                                        | Ok (types) -> 
                                                        match unmarshallValues getbytes startBodyPos endianness types with
                                                        | Ok (bodyContents,_) -> bodyContents
                                                        | Error e -> failwith e
                                                        
                                        | Error e -> failwith e

                       | None -> [||]

            let hasMessageFlag (f:DBusMessageFlags) = messageFlagsByte &&& byte f |> (=) 1uy
            let flags = Enum.GetValues(typeof<DBusMessageFlags>)
                        |> Seq.cast<DBusMessageFlags>
                        |> Seq.fold (fun acc x -> if hasMessageFlag x then Array.append acc [|x|] else acc) [||]

            let msg = {
                 Endianness = endianness
                 MessageType = Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<byte, DBusMessageType>(messageType)
                 Flags = flags
                 Body = body
                 Headerfields = headerFields
                 SequenceNumber = sequenceNumber
            }
            return msg
        }
