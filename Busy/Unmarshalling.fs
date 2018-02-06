namespace Busy

open Types
open MessageTypes
open MarshallingUtilities
open System

module Unmarshalling =

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
                                
                                unmarshalledValue |> Result.bind (fun (v,p) -> Ok (Primitive v, p))

        | StructType types -> let accStart = Ok (Struct [||], posAfterPadding)
                              types 
                              |> Seq.fold (fun acc subType -> 
                                                        acc |> Result.bind (fun (accDbValue, accP) -> 
                                                            match accDbValue with
                                                            | Struct accValues ->
                                                                unmarshall getbytes accP endianness subType
                                                                |> Result.bind (fun (value, newP) -> Ok (Struct <| (Array.append accValues [|value|]), newP) ) 
                                                            | _ -> Error "Expected Struct acc value while parsing struct"
                                                            )

                               ) accStart
                           
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
                                                                    |> Result.bind (fun (arrVal,_) -> Ok (arrVal,endPosOfContent))
                                                                    
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