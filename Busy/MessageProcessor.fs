namespace Busy.MessageProcessor

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
    member __.Handles(message:DBusMessage) =
        MessageAppliesToRule message signalRule

    member __.Invoke(message:DBusMessage) =
        signalHandler.Invoke message


type MessageProcessor(bus:Busy.IBus) =
    
    // Todo: Add locking on these collections!!!!
    let signalHandlers = new System.Collections.Generic.List<SignalHandler>()
    let pendingCalls = new System.Collections.Generic.List<PendingCall>()

    member __.AddSignalHandler(handler) =
        // Todo: create and send AddMatch message to bus automatically!!!
        signalHandlers.Add handler

    member __.RemoveSignalHandler(handler) =
        // Todo: create and send RemoveMatch message to bus automatically!!!
        signalHandlers.Remove handler |> ignore

// 	// Should be called from IBus and return Option<DbusMessage> that could contain a result to be send by IBus:
// 	// let process (incomingMessage:DBusMessage) : Option<DBusMessage>
// 	// When it contains a result (for instance, when the incoming message was a MethodCall message, a Error or MethodResult message could be returned), 
// 	// this will be send by IBus:
// 	// let resultMessage = _messageProcessor.Process incomingMessage
// 	// match resultMessage with
// 	// | None -> ()
// 	// | Some(result) -> sendMessage result

    member __.Process(message:DBusMessage):Option<DBusMessage> =
        match message.MessageType with
        | DBusMessageType.Invalid -> 
            printfn "--> Invalid message received:"
            printfn "%A" message
            None
        | DBusMessageType.MethodCall ->
            printfn "--> Method call received:"
            printfn "%A" message
            // get registered objects from _bus, invoke method and return result msg (even when void method!!)
            // result = _bus.RegisteredObjects[msg.objectpaht].Invoke(msg.body)
            // Some(result)
            None
        | DBusMessageType.MethodReturn ->
            let pendingCall = pendingCalls 
                              |> Seq.tryFind (fun x -> x.Matches message)
            match pendingCall with
            | Some pc -> 
                Console.WriteLine("--> Method return received! Returning to client");
                pendingCalls.Remove pc |> ignore
                pc.Completed message
            | None -> 
                printfn "--> Method return received, but noone is waiting for it:"
                printfn "%A" message
            None
        | DBusMessageType.Error ->
            let pendingCall = pendingCalls 
                              |> Seq.tryFind (fun x -> x.Matches message)
            match pendingCall with
            | Some pc -> 
                Console.WriteLine("--> Method error return received! Returning to client");
                pendingCalls.Remove pc |> ignore
                pc.Completed message
            | None -> 
                printfn "--> Method error return received, but noone is waiting for it (anymore?):"
                printfn "%A" message
            None
        | DBusMessageType.Signal ->
            printfn "--> Signal received:"
            signalHandlers 
            |> Seq.filter (fun x -> x.Handles message)
            |> Seq.iter (fun x -> x.Invoke message)

            None
        | _ -> None

    member __.SendAndWait(message:DBusMessage) : Result<DBusMessage, string> =
        // This method should be on IBus
        // Wanted syntax in IBus: 
        // 
        // let call =  _messageProcessor.AddPendingCall(message.SequenceNumber)
        // SendMessage(message)
        // call.WaitForReturn
        // call.Result
        let call = new PendingCall(message.SequenceNumber)
        pendingCalls.Add call
        bus.SendMessage message
        call.WaitForResult() // Todo: add timeout support!!
        call.Result

