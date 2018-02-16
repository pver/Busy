namespace Busy

open MessageTypes
module MessageFactory = 

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
