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
            | '\000' -> InvalidType
            | 'y' -> ByteType
            | 'b' -> BooleanType
            | 'n' -> Int16Type
            | 'q' -> Uint16Type
            | 'i' -> Int32Type
            | 'u' -> Uint32Type
            | 'x' -> Int64Type
            | 't' -> Uint64Type
            | 'd' -> DoubleType
            | 's' -> StringType
            | 'o' -> ObjectPathType
            | 'g' -> SignatureType
            | 'h' -> UnixFdType
            | 'm' | '*' | '?' -> ReservedType
            | x -> failwith (sprintf "Invalid signature character conversion: %c" x)

        let ParseSignatureToDBusTypes (s:string) : seq<DBusType> =
            let rec parseSingleType (chars:char list) : DBusType list*char list  = 
                let rec readContainerContent (acc:DBusType list) (stopChar:char) (chars:char list) =
                    match chars with
                    | [] -> failwith <| sprintf "incomplete container signature found, missing '%c'!" stopChar
                    | head::tail when head=stopChar -> (acc, tail)
                    | _ ->  let t, remainder = parseSingleType chars
                            readContainerContent (acc @ t) stopChar remainder

                match chars with
                | [] -> ([],[])
                | x::xs -> 
                            match x with 
                            | 'a' -> 
                                     let t, remainder = parseSingleType xs
                                     match t with
                                     | [] -> failwith "empty array signature found!"
                                     | [single] -> ([ArrayType single], remainder)
                                     | _ -> failwith "empty array signature found!"

                            | '(' -> let content, remainder = readContainerContent [] ')' xs
                                     match content with
                                     | [] -> failwith "empty struct signature found!"
                                     | _ -> ([StructType content], remainder)
                                     
                            | 'v' -> ([VariantType] , xs)

                            | '{' -> let content, remainder = readContainerContent [] '}' xs
                                     match content with
                                     | [] -> failwith "empty dictionary signature found!"
                                     | [_] -> failwith "incomplete dictionary signature found, only key type specified!"
                                     | [PrimitiveType keytype; valuetype] -> ([DictType (keytype, valuetype)], remainder)
                                     | _ -> failwith "invalid dictionary signature found, exactly one basic key type and one value type are allowed!"

                            | _ -> ([PrimitiveType (parseSignatureChar x)] , xs)

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


