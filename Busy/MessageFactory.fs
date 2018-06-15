namespace Busy

open MessageTypes
open System

[<AbstractClass; Sealed>]
type MessageFactory () = 
        static let sequenceNumberLock = new Object()
        static let mutable sequenceNumber:UInt32 = 0u
        static let nextSequenceNumber() = 
            lock sequenceNumberLock (
                fun () -> sequenceNumber <- sequenceNumber + 1u
                          sequenceNumber
            )
        static let systemEndianness = 
            match System.BitConverter.IsLittleEndian with
            | true -> DBusMessageEndianness.LittleEndian 
            | false -> DBusMessageEndianness.BigEndian

        static let getOptionalBodySignature (body:DBusMessageBody) =
            let signature = body |> Seq.map (fun x -> x.Type.Signature) |> Seq.fold (fun acc x -> sprintf "%s%s" acc x) ""
            match signature with
            | "" -> None
            | x -> Some(x)

        static let createMessageHeaderFields (body:DBusMessageBody) (objectPath:Option<string>) (iface:Option<string>) 
                (_member:Option<string>) (errorName:Option<string>) (replySerial:Option<uint32>) (sender:Option<string>) (destination:Option<string>) =
            
            {
                BodySignature = (getOptionalBodySignature body)
                ObjectPath = objectPath
                Interface = iface
                Member = _member
                ErrorName = errorName
                ReplySerial = replySerial
                Sender = sender
                Destination = destination
            }

        static let createMessage endianness messageType flags body fields =
            {
                Endianness = endianness;
                MessageType = messageType;
                Flags = flags;
                Body = body;
                SequenceNumber = (nextSequenceNumber())
                HeaderFields = fields;
            }

        static member CreateSignal (objectPath:string) (iface:string) (_member:string) (body:DBusMessageBody) (sender:Option<string>) (destination:Option<string>) = 
            let flags = [| DBusMessageFlag.NoReplyExpected |];
            let headerFields = createMessageHeaderFields body (Some objectPath) (Some iface) (Some _member) None None sender destination
            let messageType = DBusMessageType.Signal
            
            createMessage systemEndianness messageType flags body headerFields

        /// interface is optional for method calls, but recommended
        static member  CreateMethodCall (objectPath:string) (iface:Option<string>) (_member:string) (body:DBusMessageBody) (sender:Option<string>) (destination:Option<string>) = 
            let flags = [| |];
            let headerFields = createMessageHeaderFields body (Some objectPath) iface (Some _member) None None sender destination
            let messageType = DBusMessageType.MethodCall

            createMessage systemEndianness messageType flags body headerFields

        static member CreateError (replySerial:uint32) (errorName:string) (body:DBusMessageBody) (sender:Option<string>) (destination:Option<string>) = 
            let flags = [| DBusMessageFlag.NoReplyExpected |];
            let headerFields = createMessageHeaderFields body None None None (Some errorName) (Some replySerial) sender destination
            let messageType = DBusMessageType.Error

            createMessage systemEndianness messageType flags body headerFields

        static member CreateMethodReturn (replySerial:uint32) (body:DBusMessageBody) (sender:Option<string>) (destination:Option<string>) = 
            let flags = [| DBusMessageFlag.NoReplyExpected |];
            let headerFields = createMessageHeaderFields body None None None None (Some replySerial) sender destination
            let messageType = DBusMessageType.MethodReturn

            createMessage systemEndianness messageType flags body headerFields
