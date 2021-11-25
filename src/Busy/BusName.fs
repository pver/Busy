namespace Busy

open System.Runtime.InteropServices

module rec BusName =

    type WellKnownDBusName = string
    type UniqueDBusName = string
    type DBusName =
        | UniqueBusName of UniqueDBusName
        | WellKnownBusName of WellKnownDBusName

        static member ParseDBusName (busName:string) =
            match System.String.IsNullOrWhiteSpace busName with 
            | true -> InvalidBusName (busName, EmptyBusName)
            | false -> 
                let isUniqueName = busName.StartsWith(":")
                let validName = if isUniqueName then UniqueBusName busName else WellKnownBusName busName
                ValidBusName validName

                (*
                    Todo: implement all spec validation rules and return error codes:

                    EmptyElementInBusName => Bus names are composed of 1 or more elements separated by a period ('.') character. All elements must contain at least one character.
                    InvalidCharacterInBusName => Each element must only contain the ASCII characters "[A-Z][a-z][0-9]_-", with "-" discouraged in new bus names. Only elements that are part of a unique connection name may begin with a digit, elements in other bus names must not begin with a digit.
                    MissingPeriodInBusName => Bus names must contain at least one '.' (period) character (and thus at least two elements).
                    InvalidPeriodAtStartOfBusNameBus => names must not begin with a '.' (period) character.
                    MaxNameLengthExceededForBusName => Bus names must not exceed the maximum name length.
                *)

        member this.Value:string =
            match this with 
            | UniqueBusName value -> value
            | WellKnownBusName value -> value

    type ParseNameError =
        | EmptyBusName
        | MissingPeriodInBusName
        | InvalidStartPeriodInBusName
        | MaxNameLengthExceededForBusName
        | EmptyElementInBusName
        | InvalidCharacterInBusName of char

    type InvalidDBusName = string
    type ParseDBusNameResult =
        | ValidBusName of DBusName
        | InvalidBusName of InvalidDBusName*ParseNameError // malformed in any way

        member this.IsValid:bool =
            match this with 
            | ValidBusName _ -> true
            | InvalidBusName _ -> false

        member x.TryGetSuccess([<Out>] success:byref<'DBusName>) =
            match x with
            | ValidBusName value -> success <- value; true
            | _ -> false
