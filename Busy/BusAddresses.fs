namespace Busy

open System
open Address

module BusAddresses =
    
    let private getEnvironmentAddressOrDefault envVar addressDefault =
        let envAddr = Environment.GetEnvironmentVariable envVar

        let address = if String.IsNullOrWhiteSpace envAddr then addressDefault else envAddr
        ParseAddress  address   

    let SystemBusAddress() = 
        getEnvironmentAddressOrDefault "DBUS_SYSTEM_BUS_ADDRESS" "unix:path=/var/run/dbus/system_bus_socket"

    // Todo: add support for session bus + X Windowing System