namespace Busy

open Types

module rec MessageTypes =

            
        type DBusMessageEndianness =
            LittleEndian = 108uy // 'l'
            | BigEndian = 66uy // 'B'

        type DBusMessageType =
            Invalid = 0uy
            | MethodCall = 1uy
            | MethodReturn = 2uy
            | Error = 3uy
            | Signal = 4uy

        type DBusMessageFlags =
            NoReplyExpected = 1uy
            | NoAutoStart = 2uy
            | AllowInteractiveAuthorization = 4uy

        type DBusMessageBody = seq<DBusValue>

        type DBusMessage = 
            {
                Endianness : DBusMessageEndianness
                MessageType : DBusMessageType
                Flags : DBusMessageFlags[]
                Body : DBusMessageBody
                Headerfields : DBusMessageHeaderFields[]
                SequenceNumber : uint32
            }
            with 
            member this.ProtocolVersion = 1uy
        
        type DBusMessageHeaderFields =
            Invalid 
            | Path of string
            | Interface of string
            | Member of string
            | ErrorName of string
            | ReplySerial of uint32
            | Destination of string
            | Sender of string
            | Signature of Signature
            | UnixFds of uint32
            with
            member this.FieldCode = match this with
                                    | Invalid -> 0uy
                                    | Path _ -> 1uy
                                    | Interface _ -> 2uy
                                    | Member _ -> 3uy
                                    | ErrorName _ -> 4uy
                                    | ReplySerial _ -> 5uy
                                    | Destination _ -> 6uy
                                    | Sender _ -> 7uy
                                    | Signature _ -> 8uy
                                    | UnixFds _ -> 9uy

        let private systemEndianness = 
            match System.BitConverter.IsLittleEndian with
            | true -> DBusMessageEndianness.LittleEndian 
            | false -> DBusMessageEndianness.BigEndian

        let private getOptionalSender sender = 
            match sender with
            | Some s -> [|Sender s|]
            | None -> [||]

        let private getOptionalDestination destination = 
            match destination with
            | Some d -> [|Destination d|]
            | None -> [||]
        
        let private getOptionalInterface iface = 
            match iface with
            | Some i -> [|Interface i|]
            | None -> [||]

        let private getOptionalBodySignature (body:DBusMessageBody) =
            let signature = body |> Seq.map (fun x -> x.Type.Signature) |> Seq.fold (fun acc x -> sprintf "%s%s" acc x) ""
            match signature with
            | "" -> [||]
            | x -> [|Signature x|]

        let private createMessageHeaderFields (body:DBusMessageBody) (sender:Option<string>) (destination:Option<string>) =
            Array.concat [|
                getOptionalDestination destination
                getOptionalBodySignature body
                getOptionalSender sender
            |]

        let private createMessage endianness messageType flags body sequenceNumber fields =
            {
                Endianness = endianness;
                MessageType = messageType;
                Flags = flags;
                Body = body;
                SequenceNumber = sequenceNumber;
                Headerfields = fields;
            }

        let createSignal (sequenceNumber:uint32) (objectPath:string) (iface:string) (_member:string) (body:DBusMessageBody) (sender:Option<string>) (destination:Option<string>) = 
            let endianness = systemEndianness
            let flags = [| DBusMessageFlags.NoReplyExpected |];

            let headerFields = Array.concat [|
                                 [|
                                    Path objectPath;
                                    Interface iface;
                                    Member _member;
                                 |];
                                 (createMessageHeaderFields body sender destination)
                              |]
                         
            let messageType = DBusMessageType.Signal
            createMessage endianness messageType flags body sequenceNumber headerFields

        /// interface is optional for method calls, but recommended
        let createMethodCall (sequenceNumber:uint32) (objectPath:string) (iface:Option<string>) (_member:string) (body:DBusMessageBody) (sender:Option<string>) (destination:Option<string>) = 
            let endianness = systemEndianness
            let flags = [| |];
            
            let optionalInterface = getOptionalInterface iface 

            let headerFields = Array.concat [|
                                 [|
                                    Path objectPath;
                                 |];
                                 optionalInterface;
                                 [|
                                    Member _member;
                                 |];
                                 (createMessageHeaderFields body sender destination)
                             |]

            let messageType = DBusMessageType.MethodCall
            createMessage endianness messageType flags body sequenceNumber headerFields

        let createError (sequenceNumber:uint32) (replySerial:uint32) (errorName:string) (body:DBusMessageBody) (sender:Option<string>) (destination:Option<string>) = 
            let endianness = systemEndianness
            let flags = [| DBusMessageFlags.NoReplyExpected |];
            
            let headerFields = Array.concat [|
                                 [|
                                    ErrorName errorName;
                                    ReplySerial replySerial;
                                 |];
                                 (createMessageHeaderFields body sender destination)
                             |]
            let messageType = DBusMessageType.Error
            createMessage endianness messageType flags body sequenceNumber headerFields     

        let createMethodReturn (sequenceNumber:uint32) (replySerial:uint32) (body:DBusMessageBody) (sender:Option<string>) (destination:Option<string>) = 
            let endianness = systemEndianness
            let flags = [| DBusMessageFlags.NoReplyExpected |];

            let headerFields = Array.concat [|
                                 [|
                                    ReplySerial replySerial;
                                 |];
                                 (createMessageHeaderFields body sender destination)
                             |]
            let messageType = DBusMessageType.MethodReturn
            createMessage endianness messageType flags body sequenceNumber headerFields            
