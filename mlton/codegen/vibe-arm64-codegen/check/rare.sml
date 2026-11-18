(* exercises instruction forms that the regression suite does not *)
val args = CommandLine.arguments ()
val n = valOf (Int.fromString (hd args)) handle _ => 7
val r = Real.fromInt n
val r32 = Real32.fromInt n

val mulsub = _prim "Real64_mulsub": real * real * real -> real;
val mulsub32 = _prim "Real32_mulsub": Real32.real * Real32.real * Real32.real -> Real32.real;
val toW8 = _prim "Real64_rndToWordU8": real -> Word8.word;
val toW32 = _prim "Real64_rndToWordU32": real -> Word32.word;
val toW64 = _prim "Real64_rndToWordU64": real -> Word64.word;
val toW64' = _prim "Real32_rndToWordU64": Real32.real -> Word64.word;
val fromW16 = _prim "WordU16_rndToReal64": Word16.word -> real;
val fromW32 = _prim "WordU32_rndToReal64": Word32.word -> real;
val fromW64 = _prim "WordU64_rndToReal32": Word64.word -> Real32.real;
val addU8 = _prim "WordU8_addCheckP": Word8.word * Word8.word -> bool;
val addU16 = _prim "WordU16_addCheckP": Word16.word * Word16.word -> bool;
val addU32 = _prim "WordU32_addCheckP": Word32.word * Word32.word -> bool;
val addU64 = _prim "WordU64_addCheckP": Word64.word * Word64.word -> bool;
val subU16 = _prim "WordU16_subCheckP": Word16.word * Word16.word -> bool;
val subU64 = _prim "WordU64_subCheckP": Word64.word * Word64.word -> bool;
val mulU8 = _prim "WordU8_mulCheckP": Word8.word * Word8.word -> bool;
val mulU32 = _prim "WordU32_mulCheckP": Word32.word * Word32.word -> bool;
val mulU64 = _prim "WordU64_mulCheckP": Word64.word * Word64.word -> bool;
val mulS64 = _prim "WordS64_mulCheckP": Int64.int * Int64.int -> bool;
val negU64 = _prim "WordU64_negCheckP": Word64.word -> bool;
val negS16 = _prim "WordS16_negCheckP": Int16.int -> bool;
val rol32 = _prim "Word32_rol": Word32.word * Word32.word -> Word32.word;
val ror64 = _prim "Word64_ror": Word64.word * Word32.word -> Word64.word;

val w8 = Word8.fromInt n
val w16 = Word16.fromInt n
val w32 = Word32.fromInt n
val w64 = Word64.fromInt n
fun b x = print (Bool.toString x ^ "\n")
val () = print (Real.toString (mulsub (r, r + 1.0, r - 2.0)) ^ "\n")
val () = print (Real32.toString (mulsub32 (r32, r32 + 1.0, r32 - 2.0)) ^ "\n")
val () = print (Word8.toString (toW8 r) ^ Word32.toString (toW32 r)
                ^ Word64.toString (toW64 r) ^ Word64.toString (toW64' r32) ^ "\n")
val () = print (Real.toString (fromW16 w16 + fromW32 w32) ^ Real32.toString (fromW64 w64) ^ "\n")
val () = (b (addU8 (w8, w8)); b (addU16 (w16, 0w65000)); b (addU32 (w32, w32));
          b (addU64 (w64, w64)); b (addU64 (w64, 0w17)); b (subU16 (w16, w16)); b (subU64 (w64, 0w99999));
          b (mulU8 (w8, w8)); b (mulU32 (w32, w32)); b (mulU64 (w64, w64));
          b (mulS64 (Int64.fromInt n, Int64.fromInt n)); b (negU64 w64); b (negS16 (Int16.fromInt n)))
val () = print (Word32.toString (rol32 (w32, w32)) ^ Word64.toString (ror64 (w64, w32))
                ^ Word64.toString (ror64 (w64, 0w77)) ^ "\n")

(* compare-and-swap at every width, and on reals *)
fun cas (a: ''a array, x: ''a, y: ''a) =
   if MLton.Parallel.arrayCompareAndSwap (a, n mod 3) (x, y) = x then print "swapped\n" else print "no\n"
val () = cas (Array.array (3, w8), w8, 0w1)
val () = cas (Array.array (3, w16), w16, 0w1)
val () = cas (Array.array (3, w32), w32, 0w1)
val () = cas (Array.array (3, w64), w64, 0w1)
val () = cas (Array.array (3, n), n, 1)
val () = cas (Array.array (3, Int.toString n), "a", "b")
val ar = Array.array (3, r)
val () = print (Real.toString (MLton.Parallel.arrayCompareAndSwap (ar, n mod 3) (r, 1.5)) ^ "\n")
val ar32 = Array.array (3, r32)
val () = print (Real32.toString (MLton.Parallel.arrayCompareAndSwap (ar32, n mod 3) (r32, 1.5)) ^ "\n")
val rr = ref r
val () = print (Real.toString (MLton.Parallel.compareAndSwap rr (r, 2.5)) ^ "\n")
val r16 = ref w16
val () = print (Word16.toString (MLton.Parallel.compareAndSwap r16 (w16, 0w3)) ^ "\n")
val rs = ref (Int.toString n)
val () = print (MLton.Parallel.compareAndSwap rs ("x", "y") ^ "\n")

(* C calls: indirect, many arguments (on the stack), narrow arguments *)
val many = _import "many": int * int * int * int * int * int * int * int * int * Int8.int * Int16.int * real
                           * real * real * real * real * real * real * real * real * Real32.real * int -> int;
val ind = _import * : MLton.Pointer.t -> Int8.int * Int16.int * real -> Real32.real;
val sym = _address "some_symbol": MLton.Pointer.t;
val () = print (Int.toString (many (n, n, n, n, n, n, n, n, n, Int8.fromInt n, Int16.fromInt n, r, r, r, r, r, r, r, r,
                                    r, r32, n)) ^ "\n")
val () = print (Real32.toString (ind sym (Int8.fromInt n, Int16.fromInt n, r)) ^ "\n")

(* a big constant, a far offset, division by constants of each kind *)
val big = Array.tabulate (100000, fn i => i + n)
val () = print (Int.toString (Array.sub (big, 70000 + n) + 0x123456789abcdef) ^ "\n")
val () = print (Int.toString (n div 7 + n mod 7 + n div 8 + n mod 8 + n div ~3) ^ "\n")
val () = print (Word64.toString (w64 div 0w7 + w64 mod 0w7 + w64 div 0w16 + w64 mod 0w16 + w64 div 0wx7fffffff) ^ "\n")
val () = print (Word32.toString (w32 div 0w7 + w32 mod 0w10 + w32 div 0w641) ^ Word8.toString (w8 div 0w3) ^ "\n")
val () = print (Int32.toString (Int32.fromInt n div 7 + Int32.fromInt n mod 100) ^ "\n")
