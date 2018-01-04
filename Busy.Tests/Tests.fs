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

let validateTypesBackToSignature (signature:string) (types:DBusType list) =
      let resultingSignature = types |> List.fold (fun acc x -> sprintf "%s%s" acc x.Signature) ""
      Expect.equal resultingSignature signature "Types back to signature should result in the original signature"

[<Tests>]
let signatureTests =
  testList "ParseSignatureTests" [
    testCase "basic signature string is parsed correctly" <| fun _ ->
      let signature = "i"
      let subject = Busy.Utilities.ParseSignatureToDBusTypes signature |> Seq.toList
      let expected = [PrimitiveType Int32Type] 
      Expect.equal subject expected "Single simple type signature is parsed correctly"
      validateTypesBackToSignature signature expected

    testCase "basic signatures string is parsed correctly" <| fun _ ->
      let signature = "isdb"
      let subject = Busy.Utilities.ParseSignatureToDBusTypes signature |> Seq.toList
      let expected = [PrimitiveType Int32Type; PrimitiveType StringType; PrimitiveType DoubleType; PrimitiveType BooleanType] 
      Expect.equal subject expected "Multiple single types signature is parsed correctly"
      validateTypesBackToSignature signature expected


    testCase "basic array signature is parsed correctly" <| fun _ ->
      let signature = "ai"
      let subject = Busy.Utilities.ParseSignatureToDBusTypes signature |> Seq.toList
      let expected = [ArrayType (PrimitiveType Int32Type)] 
      Expect.equal subject expected "Basic array signature is parsed correctly"
      validateTypesBackToSignature signature expected

    testCase "basic array of array signature is parsed correctly" <| fun _ ->
      let signature = "aai"
      let subject = Busy.Utilities.ParseSignatureToDBusTypes signature |> Seq.toList
      let expected = [ArrayType (ArrayType (PrimitiveType Int32Type)) ]  
      Expect.equal subject expected "Basic array of array signature is parsed correctly"
      validateTypesBackToSignature signature expected

    testCase "incomplete array signature fails parsing" <| fun _ ->
      let subject() = Busy.Utilities.ParseSignatureToDBusTypes "a" |> ignore
      Expect.throws subject "Incomplete array signature should fail compilation"

    testCase "basic struct signature is parsed correctly" <| fun _ ->
      let signature = "(i)"
      let subject = Busy.Utilities.ParseSignatureToDBusTypes signature |> Seq.toList
      let expected = [StructType [PrimitiveType Int32Type]] 
      Expect.equal subject expected "Basic struct signature is parsed correctly"
      validateTypesBackToSignature signature expected

    testCase "basic struct signature (multiple types) is parsed correctly" <| fun _ ->
      let signature = "(iisd)"
      let subject = Busy.Utilities.ParseSignatureToDBusTypes signature |> Seq.toList
      let expected = [StructType [PrimitiveType Int32Type; PrimitiveType Int32Type; PrimitiveType StringType; PrimitiveType DoubleType]] 
      Expect.equal subject expected "Basic struct signature (multiple types) is parsed correctly"
      validateTypesBackToSignature signature expected

    testCase "basic struct of struct signature is parsed correctly" <| fun _ ->
      let signature = "((i))"
      let subject = Busy.Utilities.ParseSignatureToDBusTypes signature |> Seq.toList
      let expected = [StructType [StructType [PrimitiveType Int32Type]] ]  
      Expect.equal subject expected "Basic struct of struct signature is parsed correctly"
      validateTypesBackToSignature signature expected

    testCase "basic struct of array signature is parsed correctly" <| fun _ ->
      let signature = "(ai)"
      let subject = Busy.Utilities.ParseSignatureToDBusTypes signature |> Seq.toList
      let expected = [StructType [ArrayType (PrimitiveType Int32Type)] ]  
      Expect.equal subject expected "Basic struct of array signature is parsed correctly"
      validateTypesBackToSignature signature expected

    testCase "basic struct of array of struct signature is parsed correctly" <| fun _ ->
      let signature = "(a(i))"
      let subject = Busy.Utilities.ParseSignatureToDBusTypes "(a(i))" |> Seq.toList
      let expected = [StructType [ArrayType (StructType [PrimitiveType Int32Type])] ]  
      Expect.equal subject expected "Basic struct of array of struct signature is parsed correctly"
      validateTypesBackToSignature signature expected

    testCase "empty struct signature fails parsing" <| fun _ ->
      let subject() = Busy.Utilities.ParseSignatureToDBusTypes "()" |> ignore
      Expect.throws subject "Incomplete struct signature should fail compilation"

    testCase "basic array of struct signature is parsed correctly" <| fun _ ->
      let signature = "a(i)"
      let subject = Busy.Utilities.ParseSignatureToDBusTypes signature |> Seq.toList
      let expected = [ArrayType (StructType [PrimitiveType Int32Type])]  
      Expect.equal subject expected "Basic array of struct signature is parsed correctly"
      validateTypesBackToSignature signature expected

    testCase "basic array of multi struct signature is parsed correctly" <| fun _ ->
      let signature = "a(is)"
      let subject = Busy.Utilities.ParseSignatureToDBusTypes signature |> Seq.toList
      let expected = [ArrayType (StructType [PrimitiveType Int32Type; PrimitiveType StringType])]  
      Expect.equal subject expected "Basic array of multi struct signature is parsed correctly"
      validateTypesBackToSignature signature expected

    testCase "basic array of array of struct signature is parsed correctly" <| fun _ ->
      let signature = "aa(i)"
      let subject = Busy.Utilities.ParseSignatureToDBusTypes signature |> Seq.toList
      let expected = [ArrayType (ArrayType (StructType [PrimitiveType Int32Type]))]  
      Expect.equal subject expected "Basic array of array of struct signature is parsed correctly"
      validateTypesBackToSignature signature expected

    testCase "arrays of struct signature is parsed correctly" <| fun _ ->
      let signature = "a(i)a(s)"
      let subject = Busy.Utilities.ParseSignatureToDBusTypes signature |> Seq.toList
      let expected = [ArrayType (StructType [PrimitiveType Int32Type]) ; ArrayType (StructType [PrimitiveType StringType])]  
      Expect.equal subject expected "Arrays of struct signature is parsed correctly"
      validateTypesBackToSignature signature expected
    
    testCase "basic variant is parsed correctly" <| fun _ ->
      let signature = "v"
      let subject = Busy.Utilities.ParseSignatureToDBusTypes signature |> Seq.toList
      let expected = [VariantType] 
      Expect.equal subject expected "Basic variant signature is parsed correctly"
      validateTypesBackToSignature signature expected

    testCase "basic dictionary signature is parsed correctly" <| fun _ ->
      let signature = "{si}"
      let subject = Busy.Utilities.ParseSignatureToDBusTypes signature |> Seq.toList
      let expected = [DictType (StringType, PrimitiveType Int32Type)]  
      Expect.equal subject expected "Basic dictionary signature is parsed correctly"
      validateTypesBackToSignature signature expected

    testCase "basic dictionary with container value type signature is parsed correctly" <| fun _ ->
      let signature = "{s(i)}"
      let subject = Busy.Utilities.ParseSignatureToDBusTypes signature |> Seq.toList
      let expected = [DictType (StringType, StructType [PrimitiveType Int32Type])]  
      Expect.equal subject expected "Basic dictionary with container value type signature is parsed correctly"
      validateTypesBackToSignature signature expected

    testCase "empty dictionary signature fails parsing" <| fun _ ->
      let subject() = Busy.Utilities.ParseSignatureToDBusTypes "{}" |> ignore
      Expect.throws subject "Incomplete dictionary signature should fail parsing"
      
    testCase "single type dictionary signature fails parsing" <| fun _ ->
      let subject() = Busy.Utilities.ParseSignatureToDBusTypes "{i}" |> ignore
      Expect.throws subject "Single type dictionary signature should fail parsing"

    testCase "non basic key type dictionary signature fails parsing" <| fun _ ->
      let subject() = Busy.Utilities.ParseSignatureToDBusTypes "{(i)s}" |> ignore
      Expect.throws subject "Non basic key type dictionary signature should fail parsing"
  ]
