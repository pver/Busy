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
            | 'h' -> UnixFd
            | 'm' | '*' | '?' -> Reserved
            | x -> failwith (sprintf "Invalid signature character conversion: %c" x)

        let ParseSignatureToDBusTypes (s:string) : seq<DBusType> =
            let rec parseSingleType (chars:char list) : DBusType list*char list  = 
                match chars with
                | [] -> ([],[])
                | x::xs -> 
                            match x with 
                            | 'a' -> 
                                     let t, remainder = parseSingleType xs
                                     match t with
                                     | [] -> failwith "empty array signature found!"
                                     | [single] -> ([Array single], remainder)
                                     | _ -> failwith "empty array signature found!"

                            | '(' -> let rec readContainerContent (acc:DBusType list) (stopChar:char) (chars:char list) =
                                         match chars with
                                         | [] -> failwith <| sprintf "incomplete container signature found, missing '%c'!" stopChar
                                         | head::tail when head=stopChar -> (acc, tail)
                                         | _ -> let t, remainder = parseSingleType chars
                                                readContainerContent (acc @ t) stopChar remainder
                                     
                                     let content, remainder = readContainerContent [] ')' xs
                                     match content with
                                     | [] -> failwith "empty struct signature found!"
                                     | _ -> ([Struct content], remainder)
                                     
                            // | 'v' -> Variant
                            // | 'e' -> DictEntry
                            | _ -> ([Primitive (parseSignatureChar x)] , xs)

            let rec parser (acc:DBusType list) (chars:char list) =
                match chars with
                | [] -> acc
                | _ -> let t, remainder = parseSingleType chars
                       parser (acc@t) remainder  

            let parse = parser []

            s.ToCharArray() 
            |> Array.toList
            |> parse
            |> List.toSeq


