module MatchRulesTests

open Expecto
open Busy.MatchRules

[<Tests>]
let tests =
    testList "ArgMatchIdxTests" [
        testCase "Valid values should be createable" <| fun _ ->
            let validValues = [0..63]
            let createdValues = 
                validValues 
                |> List.map ArgMatchIdx.create
                |> List.choose id
                |> List.map ArgMatchIdx.value

            Expect.equal createdValues validValues "Valid values should all be createable"

        testCase "Invalid values should NOT be createable" <| fun _ ->
            let invalidValues = [-5..-1] @ [64..100]
            let createdValues = 
                invalidValues 
                |> List.map ArgMatchIdx.create
                |> List.choose id

            Expect.isEmpty createdValues "Invalid values should NOT be createable"

    ]