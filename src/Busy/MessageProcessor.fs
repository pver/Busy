namespace Busy.MessageProcessing

open Busy
open Busy.MatchRules
open Busy.MessageTypes

open System
open System.Collections.Generic
open System.Threading

// Wrapper to easily convert from F# fun to Func
type ExportedMethodHandler (methodHandler:Func<DBusMessage,DBusMessage>) =
    member __.Invoke(message:DBusMessage) =
        methodHandler.Invoke message

// Todo: add validation for the strings in these ExportedXXX types
// Todo: add attributes for auto-introspect info generation? 
// "Method, interface, property, signal, and argument elements may have 'annotations'"
type ExportedSignal = { MemberName:string } // Todo: add signal handler
type ExportedMethod = { MemberName:string; MethodHandler:ExportedMethodHandler } // Todo: add argument(s) info for introspection
type ExportedProperty = { MemberName:string; MethodHandler:ExportedMethodHandler }

type ExportedInterface = { InterfaceName:string; Methods:ExportedMethod[]; Properties:ExportedProperty[]; Signals:ExportedSignal[]}
type ExportedObject = { ObjectPath:string; Interfaces:ExportedInterface[] } // Todo: support nested objects (inside or by sharing a common ObjectPath part?)

// Todo: add timeout (overloads) here for Wait methods, might require refactoring of Result to indicate timeout errors
type PendingCall (sequenceNumber:uint32) =
    let signal = new SemaphoreSlim(0,1)

    let mutable resultMsg : Option<DBusMessage>  = None

    member __.Matches(incomingMessage:DBusMessage) =
        Some(sequenceNumber) = incomingMessage.HeaderFields.ReplySerial

    // Todo: make these Wait methods return the Result immediatly??
    member __.WaitForResult() = 
        signal.Wait()

     member __.WaitForResultAsync() = 
         signal.WaitAsync()

    member __.Completed(resultMessage) =
        resultMsg <- Some resultMessage
        signal.Release() |> ignore

    member __.Result = 
        match resultMsg with
        | Some r -> Ok r
        | None -> Error "no response received" // Todo: make this more typed

// Todo: tighten this to accept only signal match rules (introduce SignalMatchRule that equals MatchRule but with a fixed MessageType=Signal?)
// Wrapper to easily convert from F# fun to Action
type SignalHandler (signalRule:MatchRule, signalHandler:Action<DBusMessage>) =
    member __.MatchRule = signalRule

    member __.Handles(message:DBusMessage) =
        MessageAppliesToRule message signalRule

    member __.Invoke(message:DBusMessage) =
        signalHandler.Invoke message


type MessageProcessor() =
    
    // Todo: Add locking on these collections!!!!
    let signalHandlers = new List<SignalHandler>() // Replace by Dictionary for lookup by message sequence id for performance impr?
    let pendingCalls = new List<PendingCall>()
    let exportedObjects = new Dictionary<string, ExportedObject>() // Replace by Dictionary for lookup by object path for performance impr?

    member __.AddSignalHandler(handler) =
        signalHandlers.Add handler

    member __.RemoveSignalHandler(handler) =
        signalHandlers.Remove handler |> ignore

    // Todo: should this fail when an object with the same path was already exported? Or silently replace like now?
    member __.AddExportedObject(exportedObject) =
        exportedObjects.Add (exportedObject.ObjectPath, exportedObject)

    member __.AddPendingCall(sequenceNumber:uint32) =
        let call = new PendingCall(sequenceNumber)
        pendingCalls.Add call
        call

    member __.Process(message:DBusMessage):Option<DBusMessage> =
        match message.MessageType with
        | DBusMessageType.Invalid -> None
        | DBusMessageType.MethodCall ->
            // --> Incoming method call received

            // Freedesktop lists several wellknown errors: https://www.freedesktop.org/wiki/Software/DBusBindingErrors/
            let returnError msg =
                let err = MessageFactory.CreateErrorForMessage message msg [||]
                Some err

            let invokeOrReturnError (method:Option<ExportedMethod>) =
                match method with
                | None -> returnError "org.freedesktop.DBus.Error.UnknownMethod"
                | Some m -> Some (m.MethodHandler.Invoke message)

            match message.HeaderFields.ObjectPath with
            | None -> returnError "org.freedesktop.DBus.Error.UnknownObject"
            | Some objectPath ->
                let (pathFound, exportedObject) = exportedObjects.TryGetValue objectPath

                match pathFound with
                | false -> returnError "org.freedesktop.DBus.Error.UnknownObject"
                | true ->
                    
                    match message.HeaderFields.Interface with
                    | None ->
                        let method = exportedObject.Interfaces
                                      |> Seq.collect (fun x -> Seq.filter (fun (m:ExportedMethod) -> Some(m.MemberName) = message.HeaderFields.Member) x.Methods )
                                      |> Seq.tryHead
                        invokeOrReturnError method
                        
                    | Some iface ->
                        match exportedObject.Interfaces |> Seq.tryFind (fun x -> x.InterfaceName = iface) with
                        | None -> returnError "org.freedesktop.DBus.Error.UnknownInterface"
                        | Some i -> 
                            let method = i.Methods
                                      |> Seq.filter (fun (m:ExportedMethod) -> Some(m.MemberName) = message.HeaderFields.Member)
                                      |> Seq.tryHead

                            invokeOrReturnError method
            
        | DBusMessageType.MethodReturn ->
            let pendingCall = pendingCalls 
                              |> Seq.tryFind (fun x -> x.Matches message)
            match pendingCall with
            | Some pc -> 
                // --> Method return received! Returning to client
                pendingCalls.Remove pc |> ignore
                pc.Completed message
            | None -> () //Method return received, but noone is waiting for it:"
            None

        | DBusMessageType.Error ->
            let pendingCall = pendingCalls 
                              |> Seq.tryFind (fun x -> x.Matches message)
            match pendingCall with
            | Some pc -> 
                // --> Method error return received! Returning to client
                pendingCalls.Remove pc |> ignore
                pc.Completed message
            | None -> () // --> Method error return received, but noone is waiting for it (anymore?) or this is a generic error for instance because the bus couldn't process a signal
                      //.. Todo: should we raise it somehow (as event/exception?)
            None

        | DBusMessageType.Signal ->
            // --> Signal received, sending to handlers"
            // Todo: check exported objects for handlers too!
            signalHandlers 
            |> Seq.filter (fun x -> x.Handles message)
            |> Seq.iter (fun x -> x.Invoke message)

            None
            
        | _ -> None
