namespace Busy

open System.Net
open System.Net.Sockets

module UnixDomainSocket =

    let private addressFamilyBytes = [|0x1uy; 0x0uy|] // AddressFamily.Unix=1
    type UnixDomainSocketEndpoint(domainSocket : string) =
        inherit EndPoint()

        override __.Serialize () =   let bytes = Array.concat [| addressFamilyBytes ; System.Text.Encoding.ASCII.GetBytes(domainSocket); [|0x0uy;|] |]
                                     let addr = new SocketAddress(AddressFamily.Unix, Array.length bytes)
                                     bytes |> Array.iteri (fun i b -> addr.[i] <- b)
                                     addr

        override __.Create (addr:SocketAddress) = 
            let bytes = Array.init (addr.Size - 3) (fun i -> addr.[i+2])
            let domainSocket = (System.Text.Encoding.ASCII.GetString bytes)
            new UnixDomainSocketEndpoint(domainSocket) :> EndPoint

        override __.AddressFamily = AddressFamily.Unix

