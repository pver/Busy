namespace Busy

module Utilities =
        open System.Text.RegularExpressions
        open Types

        type SignatureParseError = string

        let IsValidObjectPath path = 
            match path with
            | "" | null -> false
            | "/" -> true
            | x -> Regex.IsMatch(x, "^(/([A-Z]|[a-z]|[0-9]|_)+)+$")

        let internal parseSignatureChar (c:char) : Result<DBusPrimitiveType, SignatureParseError> =
            match c with
            | '\000' -> Ok (InvalidType)
            | 'y' -> Ok (ByteType)
            | 'b' -> Ok (BooleanType)
            | 'n' -> Ok (Int16Type)
            | 'q' -> Ok (Uint16Type)
            | 'i' -> Ok (Int32Type)
            | 'u' -> Ok (Uint32Type)
            | 'x' -> Ok (Int64Type)
            | 't' -> Ok (Uint64Type)
            | 'd' -> Ok (DoubleType)
            | 's' -> Ok (StringType)
            | 'o' -> Ok (ObjectPathType)
            | 'g' -> Ok (SignatureType)
            | 'h' -> Ok (UnixFdType)
            | 'm' | '*' | '?' -> Ok (ReservedType)
            | x -> Error ( sprintf "Invalid signature char '%c' found" x)

        let ParseSignatureToDBusTypes (s:string) : Result<DBusType[], SignatureParseError> =
            let rec parseSingleType (chars:char list) : Result<DBusType list*char list, SignatureParseError>  = 
            
                let rec readContainerContent (acc:DBusType list) (stopChar:char) (chars:char list) =
                    match chars with
                    | [] -> Error <| sprintf "incomplete container signature found, missing '%c'!" stopChar
                    | head::tail when head=stopChar -> Ok (acc, tail)
                    | _ ->  match parseSingleType chars with
                            | Error e -> Error e
                            | Ok (t, remainder) -> readContainerContent (acc @ t) stopChar remainder

                match chars with
                | [] -> Ok ([],[])
                | x::xs -> 
                            match x with 
                            | 'a' -> match parseSingleType xs with
                                     | Error e -> Error e
                                     | Ok (t, remainder) -> 
                                                             match t with
                                                             | [] -> Error "empty array signature found!"
                                                             | [single] -> Ok ([ArrayType single], remainder)
                                                             | _ -> Error "multi types found in array signature!"

                            | '(' -> 
                                    match readContainerContent [] ')' xs with
                                    | Ok (content, remainder) ->
                                                                    match content with
                                                                    | [] -> Error "empty struct signature found!"
                                                                    | _ -> Ok ([StructType content], remainder)
                                    | e -> e
                                     
                            | 'v' -> Ok ([VariantType] , xs)

                            | '{' -> 
                                    match readContainerContent [] '}' xs with
                                    | Error e -> Error e
                                    | Ok (content, remainder) ->
                                                                 match content with
                                                                 | [] -> Error "empty dictionary signature found!"
                                                                 | [_] -> Error "incomplete dictionary signature found, only key type specified!"
                                                                 | [PrimitiveType keytype; valuetype] -> Ok ([DictType (keytype, valuetype)], remainder)
                                                                 | _ -> Error "invalid dictionary signature found, exactly one basic key type and one value type are allowed!"

                            | _ ->  let parsePrimitiveTypeChar = parseSignatureChar x
                                    match parsePrimitiveTypeChar with
                                    | Ok t -> Ok ([PrimitiveType t] , xs)
                                    | Error e -> Error e

            let rec parser (acc:DBusType list) (chars:char list) =
                match chars with
                | [] -> Ok (acc)
                | _ ->  match parseSingleType chars with
                        | Error e -> Error e
                        | Ok (t, remainder) -> parser (acc@t) remainder  

            let parse = parser []

            let parseResult = s.ToCharArray() 
                              |> Array.toList
                              |> parse
            match parseResult with
            | Ok (types) -> types |> List.toArray |> Ok
            | Error e -> Error e

