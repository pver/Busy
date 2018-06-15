namespace Busy.MessageProcessing

open Busy.MessageTypes
open System.Threading
open System
open Busy.MatchRules

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
type SignalHandler (signalRule:MatchRule, signalHandler:Action<DBusMessage>) =
    member __.MatchRule = signalRule

    member __.Handles(message:DBusMessage) =
        MessageAppliesToRule message signalRule

    member __.Invoke(message:DBusMessage) =
        signalHandler.Invoke message


type MessageProcessor() =
    
    // Todo: Add locking on these collections!!!!
    let signalHandlers = new System.Collections.Generic.List<SignalHandler>() // Replace by Dictionary for lookup by message sequence id for performance impr?
    let pendingCalls = new System.Collections.Generic.List<PendingCall>()

    member __.AddSignalHandler(handler) =
        signalHandlers.Add handler

    member __.RemoveSignalHandler(handler) =
        signalHandlers.Remove handler |> ignore

    member __.AddPendingCall(sequenceNumber:uint32) =
        let call = new PendingCall(sequenceNumber)
        pendingCalls.Add call
        call

    member __.Process(message:DBusMessage):Option<DBusMessage> =
        match message.MessageType with
        | DBusMessageType.Invalid -> None
        | DBusMessageType.MethodCall ->
            printfn "--> Method call received:"
            printfn "%A" message
            // Todo:
            // get registered objects from _bus, invoke method and return result msg (even when void method!!)
            // result = _bus.RegisteredObjects[msg.objectpath].Invoke(msg.body)
            // Some(result)
            None
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
            printfn "--> Signal received, sending to handlers"
            signalHandlers 
            |> Seq.filter (fun x -> x.Handles message)
            |> Seq.iter (fun x -> x.Invoke message)

            None
        | _ -> None
