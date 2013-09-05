using System;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.InteropServices;

/*
 * Simplified BSD
 *
 *  Copyright (c) 2013, Craig Gidney
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 *
 *  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 *  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * 
 * Taken from http://twistedoakstudios.com/blog/Post4708_optimizing-a-parser-combinator-into-a-memcpy
 * 
 * https://github.com/Strilanc/PickleJar
*/

namespace ParsecClone.CombinatorCS
{
    public static class OptimizedBlit<T>
    {
        public delegate T[] BlitParser(byte[] data, int itemCount, int offset, int length);

        public static BlitParser MakeUnsafeArrayBlitParser()
        {
            var d = new DynamicMethod(
                name: "BlitParseArray" + typeof(T),
                returnType: typeof(T[]),
                parameterTypes: new[] { typeof(byte[]), typeof(int), typeof(int), typeof(int) },
                m: Assembly.GetExecutingAssembly().ManifestModule);

            // ____(byte[] array, int count, int offset, int length)
            var g = d.GetILGenerator();

            // T[] result;
            g.DeclareLocal(typeof(T[]));

            // void* resultPtr;
            g.DeclareLocal(typeof(void*), true);

            // result = new T[count];
            g.Emit(OpCodes.Ldarg_1);
            g.Emit(OpCodes.Newarr, typeof(T));
            g.Emit(OpCodes.Stloc_0);

            // fixed (void* resultPtr = result)
            g.Emit(OpCodes.Ldloc_0);
            g.Emit(OpCodes.Ldc_I4_0);
            g.Emit(OpCodes.Ldelema, typeof(T));
            g.Emit(OpCodes.Stloc_1);

            // Marshal.Copy(array, offset, (IntPtr)resultPtr, length);
            g.Emit(OpCodes.Ldarg_0);
            g.Emit(OpCodes.Ldarg_2);
            g.Emit(OpCodes.Ldloc_1);
            g.Emit(OpCodes.Conv_I);
            g.EmitCall(OpCodes.Call, typeof(IntPtr).GetMethod("op_Explicit", new[] { typeof(void*) }), null);
            g.Emit(OpCodes.Ldarg_3);
            g.EmitCall(OpCodes.Call, typeof(Marshal).GetMethod("Copy", new[] { typeof(byte[]), typeof(int), typeof(IntPtr), typeof(int) }), null);

            // return result
            g.Emit(OpCodes.Ldloc_0);
            g.Emit(OpCodes.Ret);

            return (BlitParser)d.CreateDelegate(typeof(BlitParser));
        }
    }
}
