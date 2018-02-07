namespace Busy

open Types
open MessageTypes
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

    let internal unmarshallHeader (getbytes:ByteProvider) (streamPosition:StreamPosition) (endianness:DBusMessageEndianness) =
        let headerValueTypes = [| PrimitiveType ByteType;
                                  PrimitiveType ByteType;
                                  PrimitiveType ByteType;
                                  PrimitiveType Uint32Type;
                                  PrimitiveType Uint32Type;
                                  ArrayType (StructType [|PrimitiveType ByteType; VariantType|])
                                  |] // we already have the endianness, excluding it from the beginning of internal header values
        unmarshallValues getbytes streamPosition endianness headerValueTypes

    let unmarshallMessage (getbytes:ByteProvider) : Result<DBusMessage,string> =
        let endiannessByte = getbytes 0 1 |> Array.head
        
        let endiannessResult = match endiannessByte with
                               | v when v = byte DBusMessageEndianness.LittleEndian -> Ok(DBusMessageEndianness.LittleEndian)
                               | v when v = byte DBusMessageEndianness.BigEndian -> Ok(DBusMessageEndianness.BigEndian)
                               | unknown -> Error <| sprintf "Invalid endianness byte in message bytes: %A" unknown

        endiannessResult 
        |> Result.bind (fun endianness -> 
                            unmarshallHeader getbytes 1 endianness
                            |> Result.bind (fun (headerValues, pos) -> 

                                                                        let messageType = match headerValues.[0] with Primitive (Byte x) -> x | _ -> failwith "Invalid message type"
                                                                        let bodyLength = match headerValues.[3] with Primitive (Uint32 x) -> x | _ -> failwith "Invalid bodyLength type"
                                                                        let sequenceNumber = match headerValues.[4] with Primitive (Uint32 x) -> x | _ -> failwith "Invalid sequenceNumber type"
                                                                        
                                                                        // Todo: parse body
                                                                        
                                                                        let msg = {
                                                                                       Endianness = endianness;
                                                                                       MessageType = Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<byte, DBusMessageType>(messageType);
                                                                                       Flags = [||];
                                                                                       Body = [||];
                                                                                       Headerfields = [||];
                                                                                       SequenceNumber = sequenceNumber;
                                                                                  }
                                                                        Ok (msg)
                        )
        )
