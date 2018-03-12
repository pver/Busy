namespace Busy.ByteProviders

open Busy.MarshallingUtilities

type ArrayByteProvider (arr:byte[]) =
    let mutable currentPos = 0
    interface IByteProvider with
        member __.ReadBytes length = 
            let bytes = Array.sub arr currentPos length
            currentPos <- currentPos + length
            bytes

type StreamByteProvider (stream:System.IO.Stream) =
    interface IByteProvider with
        member __.ReadBytes length = 
            let buffer = Array.create length 0uy
            stream.Read (buffer, 0, length) |> ignore
            buffer