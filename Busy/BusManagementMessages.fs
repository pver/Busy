namespace Busy

// Internal for now, could be exposes in a proper way (for instance when requesting names on bus)
module internal BusManagementMessages = 
    [<Literal>]
    let internal DBusManagmentDestinationOwnerName = "org.freedesktop.DBus";
    [<Literal>]
    let internal DBusManagmentInterfaceName = "org.freedesktop.DBus";
    [<Literal>]
    let internal DBusManagmentObjectPath = "/org/freedesktop/DBus";

    let internal createMessage memberName body =
        MessageFactory.CreateMethodCall DBusManagmentObjectPath (Some DBusManagmentInterfaceName) memberName
            body None (Some DBusManagmentDestinationOwnerName)

    let internal createHello () = createMessage "Hello" [||]

    let internal matchRuleToMessageBody rule =         
        MatchRules.ToMatchRuleString rule
        |> fun x -> [|Types.DBusValue.Primitive (Types.DBusPrimitiveValue.String x)|]

    let internal createAddMatch rule = matchRuleToMessageBody rule |> createMessage "AddMatch"

    let internal createRemoveMatch rule = matchRuleToMessageBody rule |> createMessage  "RemoveMatch"