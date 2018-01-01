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

[<Tests>]
let signatureTests =
  testList "ParseSignatureTests" [
    testCase "basic signature string is parsed correctly" <| fun _ ->
      let subject = Busy.Utilities.ParseSignatureToDBusTypes "i" |> Seq.toList
      let expected = [Primitive Int32] 
      Expect.equal subject expected "Single simple type signature is parsed correctly"

    testCase "basic signatures string is parsed correctly" <| fun _ ->
      let subject = Busy.Utilities.ParseSignatureToDBusTypes "isdb" |> Seq.toList
      let expected = [Primitive Int32; Primitive String; Primitive Double; Primitive Boolean] 
      Expect.equal subject expected "Multiple single types signature is parsed correctly"

    testCase "basic array signature is parsed correctly" <| fun _ ->
      let subject = Busy.Utilities.ParseSignatureToDBusTypes "ai" |> Seq.toList
      let expected = [Array (Primitive Int32)] 
      Expect.equal subject expected "Basic array signature is parsed correctly"

    testCase "basic array of array signature is parsed correctly" <| fun _ ->
      let subject = Busy.Utilities.ParseSignatureToDBusTypes "aai" |> Seq.toList
      let expected = [Array (Array (Primitive Int32)) ]  
      Expect.equal subject expected "Basic array of array signature is parsed correctly"

    testCase "incomplete array signature fails parsing" <| fun _ ->
      let subject() = Busy.Utilities.ParseSignatureToDBusTypes "a" |> ignore
      Expect.throws subject "Incomplete array signature should fail compilation"

    testCase "basic struct signature is parsed correctly" <| fun _ ->
      let subject = Busy.Utilities.ParseSignatureToDBusTypes "(i)" |> Seq.toList
      let expected = [Struct [Primitive Int32]] 
      Expect.equal subject expected "Basic struct signature is parsed correctly"

    testCase "basic struct signature (multiple types) is parsed correctly" <| fun _ ->
      let subject = Busy.Utilities.ParseSignatureToDBusTypes "(iisd)" |> Seq.toList
      let expected = [Struct [Primitive Int32; Primitive Int32; Primitive String; Primitive Double]] 
      Expect.equal subject expected "Basic struct signature (multiple types) is parsed correctly"

    testCase "basic struct of struct signature is parsed correctly" <| fun _ ->
      let subject = Busy.Utilities.ParseSignatureToDBusTypes "((i))" |> Seq.toList
      let expected = [Struct [Struct [Primitive Int32]] ]  
      Expect.equal subject expected "Basic struct of struct signature is parsed correctly"

    testCase "basic struct of array signature is parsed correctly" <| fun _ ->
      let subject = Busy.Utilities.ParseSignatureToDBusTypes "(ai)" |> Seq.toList
      let expected = [Struct [Array (Primitive Int32)] ]  
      Expect.equal subject expected "Basic struct of array signature is parsed correctly"

    testCase "basic struct of array of struct signature is parsed correctly" <| fun _ ->
      let subject = Busy.Utilities.ParseSignatureToDBusTypes "(a(i))" |> Seq.toList
      let expected = [Struct [Array (Struct [Primitive Int32])] ]  
      Expect.equal subject expected "Basic struct of array of struct signature is parsed correctly"

    testCase "empty struct signature fails parsing" <| fun _ ->
      let subject() = Busy.Utilities.ParseSignatureToDBusTypes "()" |> ignore
      Expect.throws subject "Incomplete struct signature should fail compilation"
  ]
