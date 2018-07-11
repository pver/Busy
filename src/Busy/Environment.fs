namespace Busy

open System
open Address

module Environment =
    
    type StarterBusType = StarterBusSystem | StarterBusSession | StarterBusUnknown of string

    let internal systemBusAddressVariableName = "DBUS_SYSTEM_BUS_ADDRESS"
    let internal starterBusAddressVariableName = "DBUS_STARTER_ADDRESS"
    let internal starterBusTypeVariableName = "DBUS_STARTER_BUS_TYPE"

    let private getEnvironmentAddressOrDefault envVar addressDefault =
        let envAddr = Environment.GetEnvironmentVariable envVar

        let address = if String.IsNullOrWhiteSpace envAddr then addressDefault else envAddr
        ParseAddress address   

    let SystemBusAddress() = 
        getEnvironmentAddressOrDefault systemBusAddressVariableName "unix:path=/var/run/dbus/system_bus_socket"

    let StarterBusAddress() =
        let envAddr = Environment.GetEnvironmentVariable starterBusAddressVariableName

        if String.IsNullOrWhiteSpace envAddr then None else Some (ParseAddress envAddr)

    let private parseStarterBusType busType =

        let cleanedBusType = if String.IsNullOrWhiteSpace busType then String.Empty else busType.Trim()
        match cleanedBusType with
        | "system" -> StarterBusSystem
        | "session" -> StarterBusSession
        | x -> StarterBusUnknown x

    let StarterBusType() = 
        Environment.GetEnvironmentVariable starterBusTypeVariableName
        |> parseStarterBusType

    // Todo: add support for session bus + X Windowing System 