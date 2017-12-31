namespace Busy

module Types =
        type DBusType = 
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
                | Array of DBusType
                | Struct
                | Variant
                | DictEntry
                | UnixFd
                | Reserved


