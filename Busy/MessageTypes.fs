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
                Flags : seq<DBusMessageFlags>
                Body : DBusMessageBody
                Headerfields : seq<DBusMessageHeaderFields>
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
            | false -> DBusMessageEndianness.BigEndian;

        let private getOptionalSender sender = 
            match sender with
            | Some s -> [|Sender s|]
            | None -> [||]

        let private getOptionalDestination destination = 
            match destination with
            | Some d -> [|Destination d|]
            | None -> [||]

        let private getBodySignature (body:DBusMessageBody) =
            body |> Seq.map (fun x -> x.Type.Signature) |> Seq.fold (fun acc x -> sprintf "%s%s" acc x) ""

        let createSignal (sequenceNumber:uint32) (objectPath:string) (iface:string) (_member:string) (body:DBusMessageBody) (sender:Option<string>) (destination:Option<string>) = 
            let endianness = systemEndianness
            let flags = [| DBusMessageFlags.NoReplyExpected |];
            let signature = getBodySignature body
            
            let optionalSender = getOptionalSender sender
            let optionalDestination = getOptionalDestination destination

            let fields = Array.concat [|
                             [|
                                Path objectPath;
                                Interface iface;
                                Member _member;
                             |];
                             optionalDestination
                             [|                                
                                Signature signature;
                             |]
                             optionalSender
                         |]

            {
                Endianness = endianness;
                MessageType = DBusMessageType.Signal;
                Flags = flags;
                Body = body;
                SequenceNumber = sequenceNumber;
                Headerfields = fields;
            }

        /// interface is optional for method calls, but recommended
        let createMethodCall (sequenceNumber:uint32) (objectPath:string) (iface:Option<string>) (_member:string) (body:DBusMessageBody) (sender:Option<string>) (destination:Option<string>) = 
            let endianness = systemEndianness
            let flags = [| |];
            let signature = getBodySignature body
            
            let optionalSender = getOptionalSender sender
            let optionalDestination = getOptionalDestination destination

            let optionalInterface = match iface with
                                    | Some i -> [|Interface i|]
                                    | None -> [||]

            let optionalSignature = match body |> Seq.toArray with 
                                    | [||] -> [||]
                                    | _ -> [|Signature signature|]

            let fields = Array.concat [|
                             [|
                                Path objectPath;
                             |];
                             optionalInterface;
                             [|
                                Member _member;
                             |];
                             optionalDestination
                             optionalSignature
                             optionalSender
                         |]

            {
                Endianness = endianness;
                MessageType = DBusMessageType.MethodCall;
                Flags = flags;
                Body = body;
                SequenceNumber = sequenceNumber;
                Headerfields = fields;
            }

        let createError (sequenceNumber:uint32) (replySerial:uint32) (errorName:string) (body:DBusMessageBody) (sender:Option<string>) (destination:Option<string>) = 
            let endianness = systemEndianness
            let flags = [| DBusMessageFlags.NoReplyExpected |];
            let signature = getBodySignature body
            
            let optionalSender = getOptionalSender sender
            let optionalDestination = getOptionalDestination destination
                                    
            let optionalSignature = match body |> Seq.toArray with 
                                    | [||] -> [||]
                                    | _ -> [|Signature signature|]

            let fields = Array.concat [|
                             optionalDestination
                             [|
                                ErrorName errorName;
                                ReplySerial replySerial;
                             |];
                             optionalSignature
                             optionalSender
                         |]

            {
                Endianness = endianness;
                MessageType = DBusMessageType.Error
                Flags = flags;
                Body = body;
                SequenceNumber = sequenceNumber;
                Headerfields = fields;
            }        

        let createMethodReturn (sequenceNumber:uint32) (replySerial:uint32) (body:DBusMessageBody) (sender:Option<string>) (destination:Option<string>) = 
            let endianness = systemEndianness
            let flags = [| DBusMessageFlags.NoReplyExpected |];
            let signature = getBodySignature body
            
            let optionalSender = getOptionalSender sender
            let optionalDestination = getOptionalDestination destination
                                    
            let optionalSignature = match body |> Seq.toArray with 
                                    | [||] -> [||]
                                    | _ -> [|Signature signature|]

            let fields = Array.concat [|
                             optionalDestination
                             [|
                                ReplySerial replySerial;
                             |];
                             optionalSignature
                             optionalSender
                         |]

            {
                Endianness = endianness;
                MessageType = DBusMessageType.MethodReturn;
                Flags = flags;
                Body = body;
                SequenceNumber = sequenceNumber;
                Headerfields = fields;
            }        
