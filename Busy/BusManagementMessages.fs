namespace Busy

// Internal for now, could be exposes in a proper way (for instance when requesting names on bus)
module internal BusManagementMessages = 
    let internal dbusManagmentDestinationOwnerName = "org.freedesktop.DBus";
    let internal dbusManagmentInterfaceName = "org.freedesktop.DBus";
    let internal dbusManagmentObjectPath = "/org/freedesktop/DBus";

    let internal createMessage memberName body =
        MessageFactory.CreateMethodCall dbusManagmentObjectPath (Some dbusManagmentInterfaceName) memberName
            body None (Some dbusManagmentDestinationOwnerName)

    let internal createHello () = createMessage "Hello" [||]

    let internal matchRuleToMessageBody rule =         
        MatchRules.ToMatchRuleString rule
        |> fun x -> [|Types.DBusValue.Primitive (Types.DBusPrimitiveValue.String x)|]

    let internal createAddMatch rule = matchRuleToMessageBody rule |> createMessage "AddMatch"

    let internal createRemoveMatch rule = matchRuleToMessageBody rule |> createMessage  "RemoveMatch"