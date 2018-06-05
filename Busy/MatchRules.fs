namespace Busy

open MessageTypes
open System.Collections.Generic

module MatchRules =

    // Technique for limiting data type values:
    // see gist by Scott Wlaschin: https://gist.github.com/swlaschin/54cfff886669ccab895a
    type ArgMatchIdx = private ArgMatchIdx of int
    module ArgMatchIdx =
        let create idx = 
            match idx with
            | valid when valid >= 0 && valid <=63 -> Some(ArgMatchIdx idx)
            | _ -> None
        let value (ArgMatchIdx idx) = idx

    type PathMatchRule =
        | Path of string
        | PathNamespace of string

    type MatchRule = {
        Type : Option<DBusMessageType>
        Sender : Option<string>
        Interface : Option<string>
        Member : Option<string>
        Path : Option<PathMatchRule>
        Destination : Option<string>
        Args : Option<Dictionary<ArgMatchIdx, string>>
        ArgPaths : Option<Dictionary<ArgMatchIdx, string>>
        Arg0namespace : Option<string>
        Eavesdrop : Option<bool>}

    let MatchAllRule = {
        Type = None
        Sender = None 
        Interface = None 
        Member = None
        Path = None
        Destination = None 
        Args = None
        ArgPaths = None 
        Arg0namespace = None 
        Eavesdrop = None}

    let ToMatchRuleString (rule:MatchRule) =

        let formatKeyValue (key, value:string) = 
            let escapedValue = (value.Split('\'') |> String.concat @"'\''")
            sprintf "%s='%s'" key escapedValue
    
        let typeValue = 
            match rule.Type with
            | Some(DBusMessageType.Signal) -> Some("signal")
            | Some(DBusMessageType.MethodCall) -> Some("method_call")
            | Some(DBusMessageType.MethodReturn) -> Some("method_return")
            | Some(DBusMessageType.Error) -> Some("error")
            | _ -> None

        let pathKeyValue = 
            match rule.Path with
            | Some(PathMatchRule.Path p) -> Some ("path", p)
            | Some(PathMatchRule.PathNamespace p) -> Some("path_namespace", p)
            | None -> None
        
        let mapAs key = Option.map (fun x -> (key, x))
        let mapArgIdxKeyValue idxPrefix idxSuffix (argIdxValuePair:KeyValuePair<ArgMatchIdx, string>) =
            let key = sprintf "%s%d%s" idxPrefix (ArgMatchIdx.value argIdxValuePair.Key) idxSuffix
            Some(key, argIdxValuePair.Value)

        let argDictList idxPrefix idxSuffix dictOption = 
            match dictOption with
            | Some(d) -> d :> seq<_> |> Seq.map (mapArgIdxKeyValue idxPrefix idxSuffix) |> Seq.toList
            | None -> []

        let boolAsString = (fun b -> b.ToString().ToLower())

        [ 
            typeValue |> mapAs "type"
            rule.Sender |> mapAs "sender"
            rule.Interface |> mapAs "interface"
            rule.Member |> mapAs "member"
            pathKeyValue
            rule.Destination |> mapAs "destination"
        ] 
        @ (rule.Args |> argDictList "arg" "")
        @ (rule.ArgPaths |> argDictList "arg" "path")
        @ [    
            rule.Arg0namespace |> mapAs "arg0namespace"
            (rule.Eavesdrop |> Option.map boolAsString |> mapAs "eavesdrop")
        ]
        |> List.choose id
        |> List.map formatKeyValue
        |> String.concat ","

    // Todo: add MatchRuleBuilder for C# interop purposes?

    let MessageAppliesToRule (message:DBusMessage) rule =
        let matches (messageValue:'a) (ruleValue:'a option) = match ruleValue with Some y -> y = messageValue | None -> true
        let matchesOption (messageValue:'a option) (ruleValue:'a option) = 
            match (messageValue, ruleValue) with 
            | Some m, Some r -> m = r
            | None, Some _ -> false
            | _, None -> true

        let pathMatches = match (message.HeaderFields.ObjectPath, rule.Path) with
                          | _, None -> true
                          | None, Some(_) -> false
                          | Some messagePath, Some (Path rulePath) -> messagePath = rulePath
                          | Some messagePath, Some (PathNamespace pn) -> messagePath.StartsWith(pn)

        matches message.MessageType rule.Type
        && matchesOption message.HeaderFields.Interface rule.Interface
        && matchesOption message.HeaderFields.Member rule.Member
        && matchesOption message.HeaderFields.Sender rule.Sender
        && pathMatches