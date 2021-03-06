module Tests

open System
open Expecto
open Busy.Utilities
open Busy.Types


[<Tests>]
let tests =
  testList "IsValidObjectPathTests" [
    testCase "root object path is valid" <| fun _ ->
      let subject = IsValidObjectPath "/"
      Expect.isTrue subject "Root object path is a valid path"
    
    testCase "example valid object path" <| fun _ ->
      let subject = IsValidObjectPath "/myexample"
      Expect.isTrue subject "Example object path is a valid path"

    testCase "example valid multipart object path" <| fun _ ->
      let subject = IsValidObjectPath "/my/example"
      Expect.isTrue subject "Example multipart object path is a valid path"
    
    testCase "example with all valid characters is valid" <| fun _ ->
      let subject = IsValidObjectPath "/abcdefghijklmnopqrstuvwxyz/0123456789/ABDCEFGHIJKLMNOPQRSTUVWXYZ/abc_123_ABC"
      Expect.isTrue subject "Example with all valid characters is a valid path"


    testCase "empty object path is invalid" <| fun _ ->
      let subject = IsValidObjectPath String.Empty
      Expect.isFalse subject "Empty object path is not a valid path"

    testCase "null object path is invalid" <| fun _ ->
      let subject = IsValidObjectPath String.Empty
      Expect.isFalse subject "Null object path is not a valid path"

    testCase "object path without leading / is invalid" <| fun _ ->
      let subject = IsValidObjectPath "my/example"
      Expect.isFalse subject "Object path must begin with a leading /"

    testCase "object path with double / is invalid" <| fun _ ->
      let subject = IsValidObjectPath "/my//example"
      Expect.isFalse subject "Object path must not contain a double /"

    testCase "object path with trailing / is invalid" <| fun _ ->
      let subject = IsValidObjectPath "/obj/"
      Expect.isFalse subject "Object path must not end with a trailing /"
  ]

let validateTypesBackToSignature (signature:string) (types:DBusType[]) =
      let resultingSignature = types |> Array.fold (fun acc x -> sprintf "%s%s" acc x.Signature) ""
      Expect.equal resultingSignature signature "Types back to signature should result in the original signature"

let parseValidSignature (signature:string) (expectedTypes:DBusType[]) =
      let result = Busy.Utilities.ParseSignatureToDBusTypes signature
      match result with
      | Error e -> failwith <| sprintf "Expected Ok result for parseable signature, but parsing failed with: '%s'" e
      | Ok (subject) -> Expect.equal subject expectedTypes <| sprintf "Signature '%s' should be parsed correctly" signature
                        validateTypesBackToSignature signature expectedTypes

[<Tests>]
let signatureTests =
  testList "ParseSignatureTests" [
    testCase "basic signature string is parsed correctly" <| fun _ ->
      let signature = "i"
      let expectedTypes = [|PrimitiveType Int32Type|] 
      parseValidSignature signature expectedTypes

    testCase "basic signatures string is parsed correctly" <| fun _ ->
      let signature = "isdb"
      let expectedTypes = [|PrimitiveType Int32Type; PrimitiveType StringType; PrimitiveType DoubleType; PrimitiveType BooleanType|] 
      parseValidSignature signature expectedTypes

    testCase "basic array signature is parsed correctly" <| fun _ ->
      let signature = "ai"
      let expectedTypes = [|ArrayType (PrimitiveType Int32Type)|] 
      parseValidSignature signature expectedTypes

    testCase "basic array of array signature is parsed correctly" <| fun _ ->
      let signature = "aai"
      let expectedTypes = [|ArrayType (ArrayType (PrimitiveType Int32Type))|]  
      parseValidSignature signature expectedTypes

    testCase "incomplete array signature fails parsing" <| fun _ ->
      let result = Busy.Utilities.ParseSignatureToDBusTypes "a"
      Expect.isError result "Incomplete array signature should fail compilation"

    testCase "basic struct signature is parsed correctly" <| fun _ ->
      let signature = "(i)"
      let expectedTypes = [|StructType [PrimitiveType Int32Type]|] 
      parseValidSignature signature expectedTypes

    testCase "basic struct signature (multiple types) is parsed correctly" <| fun _ ->
      let signature = "(iisd)"
      let expectedTypes = [|StructType [PrimitiveType Int32Type; PrimitiveType Int32Type; PrimitiveType StringType; PrimitiveType DoubleType]|] 
      parseValidSignature signature expectedTypes

    testCase "basic struct of struct signature is parsed correctly" <| fun _ ->
      let signature = "((i))"
      let expectedTypes = [|StructType [StructType [PrimitiveType Int32Type]] |]  
      parseValidSignature signature expectedTypes

    testCase "basic struct of array signature is parsed correctly" <| fun _ ->
      let signature = "(ai)"
      let expectedTypes = [|StructType [ArrayType (PrimitiveType Int32Type)] |]  
      parseValidSignature signature expectedTypes

    testCase "basic struct of array of struct signature is parsed correctly" <| fun _ ->
      let signature = "(a(i))"
      let expectedTypes = [|StructType [ArrayType (StructType [PrimitiveType Int32Type])] |]  
      parseValidSignature signature expectedTypes

    testCase "empty struct signature fails parsing" <| fun _ ->
      let result = Busy.Utilities.ParseSignatureToDBusTypes "()"
      Expect.isError result "Incomplete struct signature should fail compilation"

    testCase "basic array of struct signature is parsed correctly" <| fun _ ->
      let signature = "a(i)"
      let expectedTypes = [|ArrayType (StructType [PrimitiveType Int32Type])|]  
      parseValidSignature signature expectedTypes

    testCase "basic array of multi struct signature is parsed correctly" <| fun _ ->
      let signature = "a(is)"
      let expectedTypes = [|ArrayType (StructType [PrimitiveType Int32Type; PrimitiveType StringType])|]  
      parseValidSignature signature expectedTypes

    testCase "basic array of array of struct signature is parsed correctly" <| fun _ ->
      let signature = "aa(i)"
      let expectedTypes = [|ArrayType (ArrayType (StructType [PrimitiveType Int32Type]))|]  
      parseValidSignature signature expectedTypes

    testCase "arrays of struct signature is parsed correctly" <| fun _ ->
      let signature = "a(i)a(s)"
      let expectedTypes = [|ArrayType (StructType [PrimitiveType Int32Type]) ; ArrayType (StructType [PrimitiveType StringType])|]  
      parseValidSignature signature expectedTypes
    
    testCase "basic variant is parsed correctly" <| fun _ ->
      let signature = "v"
      let expectedTypes = [|VariantType|] 
      parseValidSignature signature expectedTypes

    testCase "basic dictionary signature is parsed correctly" <| fun _ ->
      let signature = "{si}"
      let expectedTypes = [|DictType (StringType, PrimitiveType Int32Type)|]  
      parseValidSignature signature expectedTypes

    testCase "basic dictionary with container value type signature is parsed correctly" <| fun _ ->
      let signature = "{s(i)}"
      let expectedTypes = [|DictType (StringType, StructType [PrimitiveType Int32Type])|]  
      parseValidSignature signature expectedTypes

    testCase "empty dictionary signature fails parsing" <| fun _ ->
      let result = Busy.Utilities.ParseSignatureToDBusTypes "{}"
      Expect.isError result "Incomplete dictionary signature should fail parsing"
      
    testCase "single type dictionary signature fails parsing" <| fun _ ->
      let result = Busy.Utilities.ParseSignatureToDBusTypes "{i}"
      Expect.isError result "Single type dictionary signature should fail parsing"

    testCase "non basic key type dictionary signature fails parsing" <| fun _ ->
      let result = Busy.Utilities.ParseSignatureToDBusTypes "{(i)s}"
      Expect.isError result "Non basic key type dictionary signature should fail parsing"
  ]
