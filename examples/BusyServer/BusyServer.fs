module BusyServer

open Busy
open Busy.MessageTypes
open Busy.MessageProcessing
open Busy.Types

let helloHandler (msg:DBusMessage) = 
    match msg.Body|> Seq.toArray with
    | [| Busy.Types.DBusValue.Primitive(Busy.Types.DBusPrimitiveValue.String s) |] -> 
        let response = sprintf "Hello %s" s
        MessageFactory.CreateMethodReturnForMessage msg [|ToDBus response|]
    | _ -> MessageFactory.CreateErrorForMessage msg "demo.server.InvalidArguments" [||]

let rec runMessageLoop (bus:IBus) = async {
    do! Async.Sleep(1000)
    bus.IterateMessage()
    do! runMessageLoop bus }

[<EntryPoint>]
let main argv =

    let createBusResult = Bus.CreateKnownBus(KnownBus.SystemBus)
    match createBusResult with
    | Error r -> 
        printfn "Error connecting to bus! %A" r
        1
    | Ok bus ->
        let exportedHello = 
            { 
                ObjectPath = "/myService"; 
                Interfaces = 
                    [| 
                        {   
                            InterfaceName = "my.service"; 
                            Methods = 
                                [| 
                                    { 
                                        MemberName="Hello"; 
                                        MethodHandler = ExportedMethodHandler(fun msg -> helloHandler msg) 
                                    }
                                |]; 
                            Properties = [||]; 
                            Signals = [||]
                        }
                    |]
            }

        bus.AddExportedObject exportedHello

        let cts = new System.Threading.CancellationTokenSource()
        Async.Start(runMessageLoop bus, cts.Token)
        printfn "Press ENTER to stop the demo server..."

        System.Console.ReadLine() |> ignore
        cts.Cancel()
        0 // return an integer exit code
