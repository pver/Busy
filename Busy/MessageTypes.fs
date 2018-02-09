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

        let createSignal (sequenceNumber:uint32) (objectPath:string) (iface:string) (_member:string) (body:DBusMessageBody) (sender:Option<string>) (destination:Option<string>) = 

            let endianness = match System.BitConverter.IsLittleEndian with
                             | true -> DBusMessageEndianness.LittleEndian 
                             | false -> DBusMessageEndianness.BigEndian;
            let flags = [| DBusMessageFlags.NoReplyExpected |];
            let signature = body |> Seq.map (fun x -> x.Type.Signature) |> Seq.fold (fun acc x -> sprintf "%s%s" acc x) ""
            
            let optionalSender = match sender with
                                 | Some s -> [|Sender s|]
                                 | None -> [||]

            let optionalDestination = match destination with
                                      | Some d -> [|Destination d|]
                                      | None -> [||]

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

        let createMethodCall() = ()

        let createError() = ()

        let createMethodReturn() = ()
