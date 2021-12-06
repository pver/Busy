namespace Busy

open Busy.BusManagementMessages
open Busy.BusName
open Busy.Types

module BusManagement =

    [<System.Flags>]
    type RequestNameFlags = 
        | AllowReplacement = 1
        | ReplaceExisting = 2
        | DoNotQueue = 4

    [<System.Flags>]
    type RequestNameResult = 
        | PrimaryOwner = 1
        | InQueue = 2
        | Exists = 3
        | AlreadyOwner = 4
    
    [<System.Flags>]
    type ReleaseNameResult = 
        | Released = 1
        | NonExistent = 2
        | NotOwned = 3

    let private createRequestName (name:DBusName) flags = createMessage "RequestName" [| (ToDBus.Value name.Value); (ToDBus.Value (uint32 flags)) |]
    let private createReleaseName (name:DBusName) = createMessage "ReleaseName" [| (ToDBus.Value name.Value) |]
    
    type BusManager()=
        // Todo: keep as static methods or add interface with these methods?

        // Todo: improve API here? Throw Exceptions instead of using Result type?
        static member RequestName (bus:IBus) (busName:string) (flags:RequestNameFlags) =
            let parsedName = DBusName.ParseDBusName busName
            match parsedName with
            | InvalidBusName (_, err) -> 
                let errorName = 
                    match Reflection.FSharpValue.GetUnionFields(err, typeof<ParseNameError>) with
                    | case, _ -> case.Name
                Error (sprintf "Invalid request name specified: %s" errorName)
            | ValidBusName validBusName ->
                let requestNameResult = createRequestName validBusName ( flags) |> bus.SendAndWait 
                match requestNameResult with
                | Error err -> Error err
                | Ok okResult -> 
                    let resultValue = okResult.Body
                    if (Seq.isEmpty resultValue) then 
                        Error "Invalid request name result received: empty response"
                    else 
                        match Seq.head resultValue with
                        | Primitive(Uint32 requestNameResult) -> Ok (enum<RequestNameResult> (int32 requestNameResult))
                        | _ -> Error "Invalid request name result received: unexpected response type"

        static member ReleaseName (bus:IBus) (busName:string) =
            let parsedName = DBusName.ParseDBusName busName
            match parsedName with
            | InvalidBusName (_, err) -> 
                let errorName = 
                    match Reflection.FSharpValue.GetUnionFields(err, typeof<ParseNameError>) with
                    | case, _ -> case.Name
                Error (sprintf "Invalid release name specified: %s" errorName)
            | ValidBusName validBusName ->
                let releaseNameResult = createReleaseName validBusName |> bus.SendAndWait 
                match releaseNameResult with
                | Error err -> Error err
                | Ok okResult -> 
                    let resultValue = okResult.Body
                    if (Seq.isEmpty resultValue) then 
                        Error "Invalid release name result received: empty response"
                    else 
                        match Seq.head resultValue with
                        | Primitive(Uint32 releaseNameResult) -> Ok (enum<ReleaseNameResult> (int32 releaseNameResult))
                        | _ -> Error "Invalid release name result received: unexpected response type"
