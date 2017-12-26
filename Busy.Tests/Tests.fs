module Tests

open Expecto
open Busy

[<Tests>]
let tests =
  testList "IsValidObjectPathTests" [
    testCase "root object path is valid" <| fun _ ->
      let subject = Busy.IsValidObjectPath "/"
      Expect.isTrue subject "Root object path is a valid path"
  ]