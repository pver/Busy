namespace Busy

module rec Types =
        type Signature = string

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
                static member PrimitiveValue (value: obj) = 
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
                        | _ -> failwith "Only primitive values supported!"
                
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

