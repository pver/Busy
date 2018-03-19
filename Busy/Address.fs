namespace Busy

module Address =
    
    type UnixDomainSocketAddress = string
    type LaunchdAddress = string
    type TcpSocketAddress = string
    type NonceTcpSocketAddress = string
    type UnixExecutedSubProcessAddress = string
    type AutoLaunchAddress = string
    type UnknownAddress = string

    type DBusAddress =
        | UnixDomainSocketAddress of UnixDomainSocketAddress
        | LaunchdAddress
        | TcpSocketAddress of TcpSocketAddress
        | NonceTcpSocketAddress
        | UnixExecutedSubProcessAddress
        | UnknownAddress of UnknownAddress
        

    let parseAddress (address:string) : DBusAddress =
        let saveAddress = if System.String.IsNullOrEmpty(address) then "" else address

        let stripPrefix length = saveAddress.Substring(length, saveAddress.Length-length)

        match saveAddress with
        | tcp when tcp.StartsWith("tcp:") -> TcpSocketAddress <| stripPrefix 4
        | unix when unix.StartsWith("unix:") -> UnixDomainSocketAddress <| stripPrefix 5
        | _ -> UnknownAddress saveAddress
        //UnixDomainSocketAddress (address)