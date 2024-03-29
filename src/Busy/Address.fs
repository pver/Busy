namespace Busy

module Address =
    
    type AddressProperties = Map<string,string>

    type UnsupportedAddress = string

    type UnixDomainSocketAddress = AddressProperties

    type DBusAddress =
        | UnixDomainSocketAddress of UnixDomainSocketAddress
        | LaunchdAddress of AddressProperties
        | TcpSocketAddress of AddressProperties
        | NonceTcpSocketAddress of AddressProperties
        | UnixExecutedSubProcessAddress of AddressProperties
    
    type InvalidDBusAddress = string
    type ParseAddressResult =
        | ValidAddress of DBusAddress
        | ParseAddressResults of ParseAddressResult[] // composite list of addresses (separated by semi colon)
        | InvalidAddress of InvalidDBusAddress // malformed in any way, todo: add indication of why it's invalid
        | UnsupportedAddress of UnsupportedAddress // correctly formed, but unknown or unsupported transporttype
        
    let rec internal filterValidAddresses (parseResult:ParseAddressResult) =
        match parseResult with
        | ValidAddress v -> [|v|]
        | ParseAddressResults innerResults -> innerResults  |> Array.collect filterValidAddresses
        | InvalidAddress _ | UnsupportedAddress _ -> Array.empty

    let rec private parseSingleAddress (address:string) =
        let saveAddress = if System.String.IsNullOrEmpty(address) then "" else address.Trim()

        let createPrefixSpecificAddress prefix properties =
            match prefix with
            | "tcp" -> properties |> TcpSocketAddress |> ValidAddress
            | "unix" -> properties |> UnixDomainSocketAddress |> ValidAddress
            | _ -> UnsupportedAddress address

        let splitPrefixFromProperties (x:string) = x.Split([|':'|])
        let splitPropertyKeyFromValue (x:string) = x.Split([|'='|])

        match splitPrefixFromProperties saveAddress with
        | [|prefix ; settings|] when prefix <> "" -> 
            let properties = settings.Trim([|','|]).Split([|','|])
                             |> Array.map (splitPropertyKeyFromValue >> (fun x -> 
                                match x with
                                | [|k; p|] when k.Trim() <> "" -> Some <| (k.Trim(),p) // TODO: unescape property parts here
                                | _ -> None))

            let propertyMap = properties 
                              |> Array.choose id
                              |> Map.ofArray

            match propertyMap.Count with
            | x when x = properties.Length -> createPrefixSpecificAddress prefix propertyMap
            | _ -> InvalidAddress address // contains duplicates or properties without keys
                        
        | _ -> InvalidAddress address

    let ParseAddress (address:string) =
        match isNull address with 
        | true -> InvalidAddress address 
        | false -> 
            let saveAddress = address.Trim([|';'|])
            let addresses = saveAddress.Split([|';'|]) |> Array.map parseSingleAddress

            match addresses with
            | [|singleAddress|] -> singleAddress
            | [||] -> InvalidAddress address
            | _ -> ParseAddressResults addresses
