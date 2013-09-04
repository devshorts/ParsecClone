namespace ParsecClone.CombinatorBase

open System
open System.IO
open System.Reflection

[<AutoOpen>]
module Bootstrapper = 
    let private resolver o (e:ResolveEventArgs) = 
        let name = "ParsecClone.StructReader"
        if e.Name.StartsWith(name, StringComparison.OrdinalIgnoreCase) then     
            let assemblyDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)     
            let fileName = Path.Combine(assemblyDir, sprintf "%s.%s.dll" name (if (IntPtr.Size = 4) then "x86" else "x64"))
            Assembly.LoadFile(fileName)
        else
            null

    let bootstrap_combinator() = AppDomain.CurrentDomain.add_AssemblyResolve(new ResolveEventHandler(resolver))
