module BusyServer

open Busy
open Busy.BusManagement
open Busy.MessageTypes
open Busy.MessageProcessing
open Busy.Types
open Busy.MatchRules

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

        // Todo: this could be done via exported object too once supported!:
        let exampleSignalRule = {MatchAllRule with Path=(Some (Path "/myService")); Interface=(Some "my.service"); Member=(Some "ServerSignal")}
        let exampleSignalHandler = SignalHandler (exampleSignalRule, (fun signal -> printf "Got signal: %A" signal))
        bus.AddSignalHandler exampleSignalHandler

        printfn "Request bus name for server.."
        let requestNameResult = BusManager.RequestName bus "My.BusyServer" RequestNameFlags.AllowReplacement
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
