namespace Busy

module rec Types =

        type Signature = string
        
        // Value type definitions
        type DBusPrimitiveValue = 
                Invalid
                | Byte of byte
                | Boolean of bool
                | Int16 of int16
                | Uint16 of uint16
                | Int32 of int32
                | Uint32 of uint32
                | Int64 of int64
                | Uint64 of uint64
                | Double of double
                | String of string
                | ObjectPath of string
                | Signature of Signature
                | UnixFd of uint32
                | Reserved
                
                with 
                member this.Type = match this with
                                   | Invalid -> InvalidType
                                   | Byte _ -> ByteType
                                   | Boolean _ -> BooleanType
                                   | Int16 _ -> Int16Type
                                   | Uint16 _ -> Uint16Type
                                   | Int32 _ -> Int32Type
                                   | Uint32 _ -> Uint32Type
                                   | Int64 _ -> Int64Type
                                   | Uint64 _ -> Uint64Type
                                   | Double _ -> DoubleType
                                   | String _ -> StringType
                                   | ObjectPath _ -> ObjectPathType
                                   | Signature _ -> SignatureType
                                   | UnixFd _ -> UnixFdType
                                   | Reserved -> ReservedType                           

        type DBusDictEntryValue = DBusPrimitiveValue * DBusValue

        type DBusValue =
                Primitive of DBusPrimitiveValue
                | Array of DBusType*DBusValue[] // Todo: find a way to limit values to be of the specified DBusType !!
                | Struct of DBusValue[]
                | Variant of DBusValue
                | Dict of DBusDictEntryType*(DBusDictEntryValue[])
                with 
                member this.Type = match this with
                                   | Primitive p -> PrimitiveType p.Type
                                   | Array (at, _) -> ArrayType at
                                   | Struct s -> StructType (s |> Seq.map (fun x->x.Type))
                                   | Variant _ -> VariantType
                                   | Dict (kt, _) -> DictType kt                               

        // 'Type' type definitions
        type DBusPrimitiveType = 
                InvalidType
                | ByteType
                | BooleanType
                | Int16Type
                | Uint16Type
                | Int32Type
                | Uint32Type
                | Int64Type
                | Uint64Type
                | DoubleType
                | StringType
                | ObjectPathType
                | SignatureType
                | UnixFdType
                | ReservedType
                with 
                member this.Signature = match this with
                                        | InvalidType -> sprintf "%c" '\000'
                                        | ByteType -> "y"
                                        | BooleanType -> "b"
                                        | Int16Type -> "n"
                                        | Uint16Type -> "q"
                                        | Int32Type -> "i"
                                        | Uint32Type -> "u"
                                        | Int64Type -> "x"
                                        | Uint64Type -> "t"
                                        | DoubleType -> "d"
                                        | StringType -> "s"
                                        | ObjectPathType -> "o"
                                        | SignatureType -> "g"
                                        | UnixFdType -> "h"
                                        | ReservedType -> "m"

        type DBusDictEntryType = (DBusPrimitiveType * DBusType)

        type DBusType =
                PrimitiveType of DBusPrimitiveType
                | ArrayType of DBusType
                | StructType of seq<DBusType>
                | VariantType
                | DictType of DBusDictEntryType
                 with 
                member this.Signature = match this with
                                        | PrimitiveType p -> p.Signature
                                        | ArrayType a -> sprintf "a%s" <| a.Signature
                                        | StructType s -> sprintf "(%s)" <| (s |> Seq.fold (fun acc x -> sprintf "%s%s" acc x.Signature) "")
                                        | VariantType -> "v"
                                        | DictType (kt,vt) -> sprintf "{%s%s}" kt.Signature vt.Signature   

        // Conversions
        type ClrToDBusTypeConversionException() =
            inherit System.Exception()
            new(error : string) = 
                (ClrToDBusTypeConversionException ())
                then ()

        type ClrToDBusValueConversionException() =
            inherit System.Exception()
            new(error : string) = 
                (ClrToDBusValueConversionException ())
                then ()

        type ToDBus() = 
                static member Value (value: uint32) = DBusValue.Primitive(DBusPrimitiveValue.Uint32 value)
                static member Value (value: int32) = DBusValue.Primitive(DBusPrimitiveValue.Int32 value)
                static member Value (value: uint16) = DBusValue.Primitive(DBusPrimitiveValue.Uint16 value)
                static member Value (value: int16) = DBusValue.Primitive(DBusPrimitiveValue.Int16 value)
                static member Value (value: uint64) = DBusValue.Primitive(DBusPrimitiveValue.Uint64 value)
                static member Value (value: int64) = DBusValue.Primitive(DBusPrimitiveValue.Int64 value)
                static member Value (value: byte) = DBusValue.Primitive(DBusPrimitiveValue.Byte value)
                static member Value (value: bool) = DBusValue.Primitive(DBusPrimitiveValue.Boolean value)
                static member Value (value: double) = DBusValue.Primitive(DBusPrimitiveValue.Double value)
                static member Value (value: string) = 
                        if isNull value then DBusValue.Primitive(DBusPrimitiveValue.String "")
                        else DBusValue.Primitive(DBusPrimitiveValue.String value) 

                static member private CollectionValue (valueType:System.Type) (values:seq<obj>) =
                        let dbusValueType = ToDBus.Type valueType
                        let dbusValues = values |> Seq.map ToDBus.Value |> Seq.toArray
                        DBusValue.Array(dbusValueType, dbusValues)

                static member Value (value: array<'a>) =
                        if isNull value then raise (ClrToDBusValueConversionException "Can't convert null value to DBus!")
                        ToDBus.CollectionValue (typeof<'a>) (value |> Array.toSeq |> Seq.map (fun x -> x :> obj))

                static member Value (value: seq<'a>) =
                        if isNull value then raise (ClrToDBusValueConversionException "Can't convert null value to DBus!")
                        ToDBus.CollectionValue (typeof<'a>) (value |> Seq.map (fun x -> x :> obj))

                static member Value (value: obj) = 
                        // Todo: pass in target type too, strings should be converted to empty string!
                        if isNull value then raise (ClrToDBusValueConversionException "Can't convert null value to DBus!")
                        
                        let valueType = value.GetType()
                        if valueType.IsPrimitive || valueType = typeof<string> then (ToDBus.PrimitiveValue value)
                        else if valueType.IsArray 
                        then 
                                let enumerable = value :?> System.Collections.IEnumerable
                                let mySeq = seq { let i = enumerable.GetEnumerator() in while i.MoveNext() do yield i.Current } |> Seq.toList
                                ToDBus.CollectionValue (valueType.GetElementType()) mySeq
                        else 
                                raise (ClrToDBusValueConversionException "Unsupported object type!")
                        
                static member PrimitiveValue (value: obj) = 
                        if isNull value then raise (ClrToDBusValueConversionException "Can't convert null value to DBus!")
                        match value with
                        | :? uint32 as x -> ToDBus.Value x
                        | :? int32 as x -> ToDBus.Value x
                        | :? uint16 as x -> ToDBus.Value x
                        | :? int16 as x -> ToDBus.Value x
                        | :? uint64 as x -> ToDBus.Value x
                        | :? int64 as x -> ToDBus.Value x
                        | :? byte as x -> ToDBus.Value x
                        | :? bool as x -> ToDBus.Value x
                        | :? double as x -> ToDBus.Value x
                        | :? string as s -> ToDBus.Value s
                        | _ -> raise (ClrToDBusValueConversionException "Only primitive values supported when calling PrimitiveValue!")

                static member Type (``type``: System.Type) =
                        if isNull ``type`` then raise (ClrToDBusTypeConversionException "Can't convert null value to DBus type!")
                        let isIEnumerableType (t:System.Type) = t.IsGenericType && t.GetGenericTypeDefinition() = typeof<System.Collections.Generic.IEnumerable<_>>.GetGenericTypeDefinition()
                        if ``type`` = typeof<string> then PrimitiveType StringType
                        else if ``type`` = typeof<uint32> then PrimitiveType Uint32Type
                        else if ``type`` = typeof<int32> then PrimitiveType Int32Type
                        else if ``type`` = typeof<uint16> then PrimitiveType Uint16Type
                        else if ``type`` = typeof<int16> then PrimitiveType Int16Type                        
                        else if ``type`` = typeof<uint64> then PrimitiveType Uint64Type
                        else if ``type`` = typeof<int64> then PrimitiveType Int64Type
                        else if ``type`` = typeof<byte> then PrimitiveType ByteType
                        else if ``type`` = typeof<bool> then PrimitiveType BooleanType
                        else if ``type`` = typeof<double> || ``type`` = typeof<float> then PrimitiveType DoubleType
                        else if ``type``.IsArray then ArrayType (ToDBus.Type (``type``.GetElementType()))
                        else if isIEnumerableType ``type`` then ArrayType (ToDBus.Type (``type``.GetGenericArguments().[0]))
                        else 
                                let checkEnumerableType = ``type``.GetInterfaces() |> Array.filter isIEnumerableType |> Array.tryHead
                                match checkEnumerableType with 
                                | Some(x) -> ArrayType (ToDBus.Type (x.GetGenericArguments().[0]))
                                | _ -> raise (ClrToDBusTypeConversionException (sprintf "Unsupported type specified: %s" ``type``.AssemblyQualifiedName))
                                      
        type FromDBus() =
                static member PrimitiveValue (value: DBusValue) =
                        match value with
                        | DBusValue.Primitive primitive ->
                                match primitive with
                                | DBusPrimitiveValue.Uint32 x -> x :> obj
                                | DBusPrimitiveValue.Int32 x -> x :> obj
                                | DBusPrimitiveValue.String s -> s :> obj
                                | DBusPrimitiveValue.Uint16 x -> x :> obj
                                | DBusPrimitiveValue.Int16 x -> x :> obj
                                | DBusPrimitiveValue.Uint64 x -> x :> obj
                                | DBusPrimitiveValue.Int64 x -> x :> obj
                                | DBusPrimitiveValue.Byte x -> x :> obj
                                | DBusPrimitiveValue.Boolean x -> x :> obj
                                | DBusPrimitiveValue.Double x -> x :> obj
                                | _ -> failwith "Only primitive values supported!"
                        | _ -> failwith "Only primitive values supported!"

