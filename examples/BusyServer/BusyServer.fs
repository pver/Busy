module BusyServer

open Busy
open Busy.BusManagement
open Busy.BusName
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
    printfn "Starting server, connecting to System bus.."
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

        printfn "Starting message loop.."
        let cts = new System.Threading.CancellationTokenSource()
        Async.Start(runMessageLoop bus, cts.Token)

        printfn "Request bus name for server.."
        let requestedBusName = "My.BusyServer"
        let busNameParseResult = DBusName.ParseDBusName requestedBusName
        match busNameParseResult with
        | InvalidBusName _ -> 
            printfn "Invalid bus name specified!"
            1
        | ValidBusName busName ->
            let requestNameResult = BusManager.RequestName bus busName RequestNameFlags.AllowReplacement
            match requestNameResult with
            | Error r ->
                printfn "Error connecting to bus! %A" r
                1
            | Ok resultFlags -> 
                printfn "Server is reachable via well-known bus name '%s' (%A)" "My.BusyServer" resultFlags
                printfn "Press ENTER to stop the demo server..."

                System.Console.ReadLine() |> ignore
                cts.Cancel()
                0 // return an integer exit code
