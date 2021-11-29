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

        type ToDBusValueConversions = ToDBusValueConversions with 
                // Todo: can this be optimized? Or improved syntax wise?
                static member ($) (ToDBusValueConversions, value: obj) =
                        match value with
                        | :? uint32 as x -> DBusValue.Primitive(DBusPrimitiveValue.Uint32 x)
                        | :? int32 as x -> DBusValue.Primitive(DBusPrimitiveValue.Int32 x)
                        | :? uint16 as x -> DBusValue.Primitive(DBusPrimitiveValue.Uint16 x)
                        | :? int16 as x -> DBusValue.Primitive(DBusPrimitiveValue.Int16 x)
                        | :? uint64 as x -> DBusValue.Primitive(DBusPrimitiveValue.Uint64 x)
                        | :? int64 as x -> DBusValue.Primitive(DBusPrimitiveValue.Int64 x)
                        | :? byte as x -> DBusValue.Primitive(DBusPrimitiveValue.Byte x)
                        | :? bool as x -> DBusValue.Primitive(DBusPrimitiveValue.Boolean x)
                        | :? double as x -> DBusValue.Primitive(DBusPrimitiveValue.Double x)
                        | :? string as s -> 
                                if isNull value then DBusValue.Primitive(DBusPrimitiveValue.String "")
                                else DBusValue.Primitive(DBusPrimitiveValue.String s)
                        | _ ->
                                if isNull value then
                                        failwith "Invalid field found"
                                else
                                        // Todo: Object support: get properties and create struct type for it
                                        failwith "Not supported yet"

        /// Helper method to convert CLR types to supported DBus types
        let inline ToDBus value :DBusValue = ToDBusValueConversions $ value    

        type FromDBusValueConversions =  FromDBusValueConversions with
                static member ($) (FromDBusValueConversions, value: DBusValue) : obj =
                        match value with
                        | DBusValue.Primitive(DBusPrimitiveValue.Uint32 x) -> x :> obj
                        | DBusValue.Primitive(DBusPrimitiveValue.Int32 x) -> x :> obj
                        | DBusValue.Primitive(DBusPrimitiveValue.String s) -> s :> obj
                        | DBusValue.Primitive(DBusPrimitiveValue.Uint16 x) -> x :> obj
                        | DBusValue.Primitive(DBusPrimitiveValue.Int16 x) -> x :> obj
                        | DBusValue.Primitive(DBusPrimitiveValue.Uint64 x) -> x :> obj
                        | DBusValue.Primitive(DBusPrimitiveValue.Int64 x) -> x :> obj
                        | DBusValue.Primitive(DBusPrimitiveValue.Byte x) -> x :> obj
                        | DBusValue.Primitive(DBusPrimitiveValue.Boolean x) -> x :> obj
                        | DBusValue.Primitive(DBusPrimitiveValue.Double x) -> x :> obj
                        | _ -> failwith "Not supported yet"

        let inline FromDBus value :obj = FromDBusValueConversions $ value   
