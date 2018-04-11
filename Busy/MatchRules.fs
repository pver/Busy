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
        Type:Option<DBusMessageType>; 
        Sender:Option<string>;
        Interface:Option<string>;
        Member:Option<string>;
        Path:Option<PathMatchRule>;
        Destination:Option<string>;
        Args:Option<Dictionary<ArgMatchIdx, string>>;
        ArgPaths:Option<Dictionary<ArgMatchIdx, string>>;
        Arg0namespace:Option<string>;
        Eavesdrop:Option<bool>}

    // Todo: add MatchRuleBuilder for C# interop purposes?
