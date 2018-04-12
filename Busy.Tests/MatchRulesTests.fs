module MatchRulesTests

open Expecto
open Busy.MatchRules
open Busy.MessageTypes

[<Tests>]
let argMatchTests =
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

[<Tests>]
let toMatchRuleStringTests =

    testList "ToMatchRuleStringTests" [
        testCase "SpecExample should be stringified correctly" <| fun _ ->
            let arg2 = (ArgMatchIdx.create 2)
            let argDict = new System.Collections.Generic.Dictionary<ArgMatchIdx, string>() 
            
            if Option.isSome arg2 then argDict.[(arg2.Value)] <- "bar"
            
            let matchRule = {
                MatchAllRule with 
                    Type=Some(DBusMessageType.Signal)
                    Sender=Some("org.freedesktop.DBus")
                    Interface=Some("org.freedesktop.DBus")
                    Member=Some("Foo")
                    Path=Some( PathMatchRule.Path("/bar/foo") )
                    Destination=Some(":452345.34")
                    Args=Some( argDict)
                }
            let expected = "type='signal',sender='org.freedesktop.DBus',interface='org.freedesktop.DBus',member='Foo',path='/bar/foo',destination=':452345.34',arg2='bar'"
            let actual = toMatchRuleString matchRule
            Expect.equal actual expected "SpecExample should be stringified correctly"

        testCase "Stringified MatchAllRule should not contain anything" <| fun _ ->
            let matchRule = MatchAllRule
            let expected = ""
            let actual = toMatchRuleString matchRule
            Expect.equal actual expected "Stringified MatchAllRule should not contain anything"

        testCase "Stringified PathNamespace rule should contain path_namespace" <| fun _ ->
            let matchRule = { MatchAllRule with Path=Some( PathMatchRule.PathNamespace("/com/example/foo")) }
            let expected = "path_namespace='/com/example/foo'"
            let actual = toMatchRuleString matchRule
            Expect.equal actual expected "Stringified PathNamespace rule should contain path_namespace"

        testCase "Stringified Eavesdrop values rule should have correct values" <| fun _ ->
            let actualValues = [true; false] |> List.map (fun x -> toMatchRuleString { MatchAllRule with Eavesdrop=Some(x) })
            let expectedValues = ["eavesdrop='true'"; "eavesdrop='false'"]
            Expect.equal actualValues expectedValues "Stringified Eavesdrop values rule should have correct values"

        testCase "Stringified Type values rule should have correct values" <| fun _ ->
            let actualValues = 
                [DBusMessageType.Signal; DBusMessageType.MethodCall; DBusMessageType.MethodReturn; DBusMessageType.Error; DBusMessageType.Invalid] 
                |> List.map (fun x -> toMatchRuleString { MatchAllRule with Type=Some(x) })
            let expectedValues = ["type='signal'"; "type='method_call'"; "type='method_return'"; "type='error'"; ""]
            Expect.equal actualValues expectedValues "Stringified Type values rule should have correct values"
    ]