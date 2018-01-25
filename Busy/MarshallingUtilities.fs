namespace Busy

open Types

module internal MarshallingUtilities =

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