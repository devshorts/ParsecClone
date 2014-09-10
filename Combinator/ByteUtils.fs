namespace ParsecClone.BinaryCombinator

open System.Reflection
open System.Reflection.Emit
open System.Runtime.InteropServices
open System

[<AutoOpen>]
module ByteUtils = 
   
    type Bit = 
        | One
        | Zero
        override this.ToString() = 
            match this with 
                | One -> "1"
                | Zero -> "0"
        
    let bitToByte = function
                        | One -> byte(1)
                        | Zero -> byte(0)

    let bitMasks = 
        Array.zeroCreate 8        
            |> Array.mapi ( fun i _ -> byte(pown 2 i), i )   
            |> Array.rev

    let private mask inputByte (bitMask, bitPosition) = 
        if   (inputByte &&& bitMask) >>> bitPosition = byte(0) then Zero
        else One 

    let byteToBitArray inputByte  = 
        bitMasks |> Array.map (mask inputByte)                 

    let bytesToBits (bytes:byte[]) =        
        bytes |> Array.collect byteToBitArray

    let bitsToUInt (bits:Bit[])  = 
        let positions = Array.zip bits (Array.rev [|0..Array.length bits - 1|])

        Array.fold (fun acc (bit, index) -> 
                        match bit with 
                            | Zero -> acc
                            | One -> acc + (pown 2 index)) 0 positions

    let sizeofType objType = Marshal.SizeOf objType

    type BlitParser<'T> =  delegate of byte[] * int * int * int -> 'T[]
 
    let makeUnsafeArrayBlitParser<'T when 'T: unmanaged> () : BlitParser<'T> =
        
        let d = new DynamicMethod ( "BlitParseArray" + ( typeof<'T> |> string )                 ,
                                    typeof<'T[]>                                                ,
                                    [| typeof<byte[]>; typeof<int>; typeof<int>; typeof<int> |] ,
                                    Assembly.GetExecutingAssembly().ManifestModule              )

        // ___( byte[] array, int count, int offset, int length )
        let gen = d.GetILGenerator()

        // T[] result
        gen.DeclareLocal( typeof<'T[]> ) |> ignore

        // IntPtr resultPtr
        gen.DeclareLocal( typeof<IntPtr>, true ) |> ignore
        
        // result = new T[count]
        gen.Emit( OpCodes.Ldarg_1 )
        gen.Emit( OpCodes.Newarr, typeof<'T> )
        gen.Emit( OpCodes.Stloc_0 )

        // fixed ( IntPtr resultPtr = result )
        gen.Emit( OpCodes.Ldloc_0  )
        gen.Emit( OpCodes.Ldc_I4_0 )
        gen.Emit( OpCodes.Ldelema, typeof<'T> )
        gen.Emit( OpCodes.Stloc_1  )

        // Marshal.Copy( array, offset, (IntPtr)resultPtr, length )
        gen.Emit( OpCodes.Ldarg_0 )
        gen.Emit( OpCodes.Ldarg_2 )
        gen.Emit( OpCodes.Ldloc_1 )
        gen.Emit( OpCodes.Conv_I  )

        gen.Emit( OpCodes.Ldarg_3 )
        let marshalCopy = typeof<Marshal>.GetMethod( "Copy", [| typeof<byte[]>; typeof<int>; typeof<IntPtr>; typeof<int> |])
        gen.EmitCall( OpCodes.Call, marshalCopy, null)                                                      

        // return result
        gen.Emit( OpCodes.Ldloc_0 )
        gen.Emit( OpCodes.Ret     )

        d.CreateDelegate( typeof<BlitParser<'T>> ) :?> BlitParser<'T>


    let blit<'T when 'T: unmanaged >  (byteArray: byte[]) : 'T [] = 
        let size = sizeofType typeof<'T>

        let count = byteArray.Length / size
        let offset = 0 
        let length = byteArray.Length

        let args = (byteArray, count, offset, length)

        let func = makeUnsafeArrayBlitParser()

        func.Invoke args


    let byteArrayToObjects<'T when 'T: unmanaged > (byteArray: byte[]) networkOrder : 'T [] =         
        blit byteArray |> (if networkOrder then Array.rev else id)


    