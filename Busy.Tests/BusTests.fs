module BusTests

open Expecto
open Busy.Transport
open Busy

[<Tests>]
let busTests =
    testList "busTests" [
        testCase "IBus.Transport should return passed in ITransport" <| fun _ ->
            
            let itransport = { new ITransport with
                                    member __.Close () = ()
                                    member __.Write (_:byte[]) = ()
                                    member __.ReadBytes _ = failwith "nothing to read here" }

            let bus = Bus(itransport) :> IBus

            Expect.equal itransport bus.Transport "Bus should store passed in ITransport"
    ]