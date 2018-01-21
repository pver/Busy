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
                | Struct of seq<DBusValue>  
                | Variant of Signature
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