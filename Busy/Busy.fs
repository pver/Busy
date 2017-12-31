namespace Busy

module Utilities =
        open System.Text.RegularExpressions
        open Types

        let IsValidObjectPath path = 
            match path with
            | "" | null -> false
            | "/" -> true
            | x -> Regex.IsMatch(x, "^(/([A-Z]|[a-z]|[0-9]|_)+)+$")

        let internal parseSignatureChar (c:char) =
            match c with
            | '\000' -> Invalid
            | 'y' -> Byte
            | 'b' -> Boolean
            | 'n' -> Int16
            | 'q' -> Uint16
            | 'i' -> Int32
            | 'u' -> Uint32
            | 'x' -> Int64
            | 't' -> Uint64
            | 'd' -> Double
            | 's' -> String
            | 'o' -> ObjectPath
            | 'g' -> Signature
            | 'a' -> Array
            | 'r' -> Struct
            | 'v' -> Variant
            | 'e' -> DictEntry
            | 'h' -> UnixFd
            | 'm' | '*' | '?' -> Reserved
            | x -> failwith (sprintf "Invalid signature character conversion: %c" x)

        let ParseSignatureToDBusTypes (s:string) : seq<DBusType> =
            let rec parse (acc:DBusType list) (chars:char list)  = 
                match chars with
                | [] -> acc
                | x::xs -> parse (acc @ [(parseSignatureChar x)]) xs

            s.ToCharArray() 
            |> Array.toList
            |> parse []
            |> List.toSeq


