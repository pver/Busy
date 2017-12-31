namespace Busy

module Types =
        type DBusPrimitiveType = 
                Invalid
                | Byte
                | Boolean
                | Int16
                | Uint16
                | Int32
                | Uint32
                | Int64
                | Uint64
                | Double
                | String
                | ObjectPath
                | Signature
                | UnixFd
                | Reserved

        type DBusType =
                Primitive of DBusPrimitiveType
                | Array of DBusType
                | Struct
                | Variant
                | DictEntry


