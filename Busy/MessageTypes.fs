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

        let private getOptionalBodySignature (body:DBusMessageBody) =
            let signature = body |> Seq.map (fun x -> x.Type.Signature) |> Seq.fold (fun acc x -> sprintf "%s%s" acc x) ""
            match signature with
            | "" -> [||]
            | x -> [|Signature x|]

        let private createMessageHeaderFields (body:DBusMessageBody) (objectPath:Option<string>) (iface:Option<string>) 
                (_member:Option<string>) (errorName:Option<string>) (replySerial:Option<uint32>) (sender:Option<string>) (destination:Option<string>) =
            let mapper (f:('a -> DBusMessageHeaderFields)) (x:'a option) = match x with Some y -> [|f y|] | None -> [||]

            Array.concat [|
                objectPath |> mapper Path
                iface |> mapper Interface
                _member |> mapper Member
                errorName |> mapper ErrorName
                replySerial |> mapper ReplySerial
                destination |> mapper Destination
                getOptionalBodySignature body
                sender |> mapper Sender
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
            let flags = [| DBusMessageFlags.NoReplyExpected |];
            let headerFields = createMessageHeaderFields body (Some objectPath) (Some iface) (Some _member) None None sender destination
            let messageType = DBusMessageType.Signal
            
            createMessage systemEndianness messageType flags body sequenceNumber headerFields

        /// interface is optional for method calls, but recommended
        let createMethodCall (sequenceNumber:uint32) (objectPath:string) (iface:Option<string>) (_member:string) (body:DBusMessageBody) (sender:Option<string>) (destination:Option<string>) = 
            let flags = [| |];
            let headerFields = createMessageHeaderFields body (Some objectPath) iface (Some _member) None None sender destination
            let messageType = DBusMessageType.MethodCall

            createMessage systemEndianness messageType flags body sequenceNumber headerFields

        let createError (sequenceNumber:uint32) (replySerial:uint32) (errorName:string) (body:DBusMessageBody) (sender:Option<string>) (destination:Option<string>) = 
            let flags = [| DBusMessageFlags.NoReplyExpected |];
            let headerFields = createMessageHeaderFields body None None None (Some errorName) (Some replySerial) sender destination
            let messageType = DBusMessageType.Error

            createMessage systemEndianness messageType flags body sequenceNumber headerFields

        let createMethodReturn (sequenceNumber:uint32) (replySerial:uint32) (body:DBusMessageBody) (sender:Option<string>) (destination:Option<string>) = 
            let flags = [| DBusMessageFlags.NoReplyExpected |];
            let headerFields = createMessageHeaderFields body None None None None (Some replySerial) sender destination
            let messageType = DBusMessageType.MethodReturn

            createMessage systemEndianness messageType flags body sequenceNumber headerFields
