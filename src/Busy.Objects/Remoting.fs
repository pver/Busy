namespace Busy.Objects

open System
open System.Collections.Generic
open System.Linq
open System.Reflection
open System.Reflection.Emit
open Busy
open Microsoft.FSharp.Core

module Remoting =

    type RemoteObjectInvocationException() =
            inherit Exception()
            new(error : string) = 
                (RemoteObjectInvocationException ())
                then ()

    type RemoteObjectInvocationErrorException() =
            inherit Exception()
            new(error : string) = 
                (RemoteObjectInvocationErrorException ())
                then ()

    type RemoteObjectBase() =
            member val private _bus = Unchecked.defaultof<IBus> with get, set
            new(bus : IBus) as this = 
                (RemoteObjectBase ())
                then
                    if isNull (bus:> obj) then nullArg "bus"
                    this._bus <- bus

            member this.ExecuteMethodCall (objectPath : string) (interfaceName : string) (memberName : string) (destinationBusName : string) (args : obj[]) (expectedOutputType : Type) = 
                let dbusArgs = if isNull args then [||] else args |> Array.map (Types.ToDBus.Value)
                let msg = MessageFactory.CreateMethodCall objectPath (Some (interfaceName)) memberName dbusArgs None (Some (destinationBusName))
                let callResult = this._bus.SendAndWait (msg)
                match callResult with
                | Error e -> raise (new RemoteObjectInvocationException(e))
                | Ok result ->
                    match result.MessageType with 
                    | MessageTypes.DBusMessageType.MethodReturn ->
                        let outputs = (result.Body.Select (Types.FromDBus.PrimitiveValue)).ToArray ()
                        let actualOutputCount = outputs.Length
                        let expectedOutputCount = if expectedOutputType = typeof<Void> then 0 else 1
                        if expectedOutputCount <> actualOutputCount
                        then 
                            let msg = sprintf "Expected number of output values %d does not match actual number of output values %d." expectedOutputCount actualOutputCount
                            let exc = new RemoteObjectInvocationException(msg)
                            exc.Data.["ObjectPath"] <- objectPath
                            exc.Data.["InterfaceName"] <- interfaceName
                            exc.Data.["MemberName"] <- memberName
                            exc.Data.["DestinationBusName"] <- destinationBusName
                            exc.Data.["ExpectedOutputCount"] <- expectedOutputCount
                            exc.Data.["ActualOutputCount"] <- actualOutputCount
                            raise exc
                        outputs
                    | MessageTypes.DBusMessageType.Error -> raise (new RemoteObjectInvocationErrorException("Remote object returned error"))
                    | _ -> raise (new RemoteObjectInvocationException("Unexpected dbus value received for method call"))

    type RemoteObjectTypeFactory() =
        static let mutable _moduleBuilder = Unchecked.defaultof<System.Reflection.Emit.ModuleBuilder>
        static let _interfaceImplementations = new Dictionary<Type, Type>()
        static let _monitor = Object()
        static do 
            let assemblyName = new AssemblyName("BusyProxyAssembly")
            let dynamicAssembly = AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Run);
            _moduleBuilder <- dynamicAssembly.DefineDynamicModule("BusyRemoteObjectTypeFactoryModule");

        member this.GetRemoteObject<'T>(bus : IBus) (objectPath : string) (interfaceName : string) (destinationBusName : string) = 
            if isNull objectPath then nullArg  "objectPath"
            if isNull interfaceName then nullArg "interfaceName"
            if isNull destinationBusName then nullArg "destinationBusName"

            let typeToImplement = typeof<'T>

            // assure not to create the same type twice
            lock _monitor (fun () ->
                if not (_interfaceImplementations.ContainsKey (typeToImplement))
                then this.CreateRemoteObjectType (typeToImplement)
            )

            let impl = _interfaceImplementations.[typeToImplement];
            let constructorArgs = [|(bus :> obj); (objectPath :> obj); (interfaceName :> obj); (destinationBusName :> obj)|]
            (Activator.CreateInstance (impl, constructorArgs (* when on .netstandard 2.1: pass BindingFlags.DoNotWrapExceptions *) )) :?> 'T
        
        member private this.CreateRemoteObjectType(interfaceToImplement : Type) = 
            if not interfaceToImplement.IsInterface
            then raise (new NotSupportedException("Only interface types are supported"))
            
            let baseClassType = typeof<RemoteObjectBase>
            let typeName = "BusyRemoteProxyImpl." + interfaceToImplement.FullName.Replace(".","_")
            let typeBuilder = _moduleBuilder.DefineType (typeName, (TypeAttributes.Class ||| TypeAttributes.Public), baseClassType)
            typeBuilder.AddInterfaceImplementation (interfaceToImplement)

            let baseConstructor = (baseClassType.GetConstructors (BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)).Single (fun x->(x.GetParameters().Length) = 1)
            let constructor = typeBuilder.DefineConstructor (MethodAttributes.Public, CallingConventions.Standard, [|typeof<IBus>; typeof<string>; typeof<string>; typeof<string>|])
            // create local fields to keep constructor injected values in:
            let privateInitOnly = (FieldAttributes.Private ||| FieldAttributes.InitOnly)
            let fldObjPath = typeBuilder.DefineField ("_objectPath", typeof<string>, privateInitOnly)
            let fldIfName = typeBuilder.DefineField ("_interfaceName", typeof<string>, privateInitOnly)
            let fldDest = typeBuilder.DefineField ("_destinationBusName", typeof<string>, privateInitOnly)

            let ilGenerator = constructor.GetILGenerator ()
            ilGenerator.Emit (OpCodes.Ldarg_0)
            ilGenerator.Emit (OpCodes.Ldarg_1)
            ilGenerator.Emit (OpCodes.Call, baseConstructor)
            ilGenerator.Emit (OpCodes.Nop)
            ilGenerator.Emit (OpCodes.Nop)
            ilGenerator.Emit (OpCodes.Ldarg_0)
            ilGenerator.Emit (OpCodes.Ldarg_2)
            ilGenerator.Emit (OpCodes.Stfld, fldObjPath)
            ilGenerator.Emit (OpCodes.Ldarg_0)
            ilGenerator.Emit (OpCodes.Ldarg_3)
            ilGenerator.Emit (OpCodes.Stfld, fldIfName)
            ilGenerator.Emit (OpCodes.Ldarg_0)
            ilGenerator.Emit (OpCodes.Ldarg_S, 4)
            ilGenerator.Emit (OpCodes.Stfld, fldDest)
            ilGenerator.Emit (OpCodes.Ret)

            let getTypeMethodCall = typeof<Type>.GetMethod("GetType", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic, null, [|(typeof<string>)|], null);

            let executeMethodCall = (baseClassType.GetMethods (BindingFlags.Public ||| BindingFlags.Instance)).Single (fun x -> x.Name = "ExecuteMethodCall")
            for interfaceMethod in interfaceToImplement.GetMethods () do
                let methodName = interfaceMethod.Name
                let parameterTypes = ((interfaceMethod.GetParameters ()).Select (fun p -> p.ParameterType)).ToArray ()
                let parameterCount = parameterTypes.Length
                let returnType = interfaceMethod.ReturnType
                let returnTypeCount = if returnType = typeof<Void> then 0 else 1
                let methodBuilder = typeBuilder.DefineMethod (methodName, (MethodAttributes.Public ||| MethodAttributes.Virtual), returnType, parameterTypes)
                let methodIl = methodBuilder.GetILGenerator ()
                
                methodIl.DeclareLocal(typeof<Type>) |> ignore

                methodIl.Emit (OpCodes.Nop)

                // store the return type in a local variable (calling Type.GetType method to get it in the emitted code..)
                // Todo: possible improvement: cache this instead of doing this every method call
                methodIl.Emit (OpCodes.Ldstr, returnType.AssemblyQualifiedName)
                methodIl.Emit (OpCodes.Call, getTypeMethodCall)
                methodIl.Emit (OpCodes.Stloc_0) // store result (=ReturnType) in the first (only) local variable

                // prepare all arguments for calling ExecuteMethodCall on RemoteObjectBase
                methodIl.Emit (OpCodes.Ldarg_0)
                methodIl.Emit (OpCodes.Ldarg_0)
                methodIl.Emit (OpCodes.Ldfld, fldObjPath)
                methodIl.Emit (OpCodes.Ldarg_0)
                methodIl.Emit (OpCodes.Ldfld, fldIfName)
                methodIl.Emit (OpCodes.Ldstr, methodName)
                methodIl.Emit (OpCodes.Ldarg_0)
                methodIl.Emit (OpCodes.Ldfld, fldDest)
                methodIl.Emit (OpCodes.Ldc_I4_S, parameterCount)
                methodIl.Emit (OpCodes.Newarr, typeof<obj>)
                do 
                    let mutable i = 0
                    while (i < parameterTypes.Length) do
                        let parameterType = parameterTypes.[i]
                        methodIl.Emit (OpCodes.Dup)
                        methodIl.Emit (OpCodes.Ldc_I4_S, i)
                        methodIl.Emit (OpCodes.Ldarg, (i + 1))
                        if parameterType.IsPrimitive
                        then methodIl.Emit (OpCodes.Box, parameterType)
                        methodIl.Emit (OpCodes.Stelem_Ref)
                        i <- i + 1
                        ()

                methodIl.Emit (OpCodes.Ldloc_0)
                methodIl.EmitCall (OpCodes.Call, executeMethodCall, [|typeof<string>; typeof<string>; typeof<string>; typeof<string>; typeof<obj[]>; typeof<Type>|])
                
                if returnTypeCount = 0
                then methodIl.Emit (OpCodes.Pop)
                else 
                    methodIl.Emit (OpCodes.Ldc_I4_0)
                    methodIl.Emit (OpCodes.Ldelem_Ref)
                    let castOpcode = if returnType.IsPrimitive then OpCodes.Unbox_Any else OpCodes.Castclass
                    methodIl.Emit (castOpcode, returnType)
                
                methodIl.Emit (OpCodes.Ret)
                typeBuilder.DefineMethodOverride (methodBuilder, interfaceMethod)

#if NETSTANDARD2_0
            let generatedType = typeBuilder.CreateTypeInfo().AsType()
#else
            let generatedType = typeBuilder.CreateType()
#endif

            _interfaceImplementations.Add(interfaceToImplement, generatedType)