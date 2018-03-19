namespace Busy

module Address =
    
    type UnixDomainSocketAddress = string
    type LaunchdAddress = string
    type TcpSocketAddress = string
    type NonceTcpSocketAddress = string
    type UnixExecutedSubProcessAddress = string
    type AutoLaunchAddress = string
    type UnsupportedAddress = string

    type DBusAddress =
        | UnixDomainSocketAddress of UnixDomainSocketAddress
        | LaunchdAddress
        | TcpSocketAddress of TcpSocketAddress
        | NonceTcpSocketAddress
        | UnixExecutedSubProcessAddress
        | InvalidAddress of string // malformed in any way
        | UnsupportedAddress of UnsupportedAddress // correctly formed, but unknown or unsupported transporttype
        

    let ParseAddress (address:string) : DBusAddress =
        let saveAddress = if System.String.IsNullOrEmpty(address) then "" else address.Trim()

        let stripPrefix length = saveAddress.Substring(length, saveAddress.Length-length)

        match saveAddress with
        | tcp when tcp.StartsWith("tcp:") -> TcpSocketAddress <| stripPrefix 4
        | unix when unix.StartsWith("unix:") -> UnixDomainSocketAddress <| stripPrefix 5
        | _ -> UnsupportedAddress saveAddress