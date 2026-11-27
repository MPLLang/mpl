(* Runs the same operations over standard arrays and over Soa arrays; the two
 * transcripts must be identical. *)

signature ARR =
sig
  eqtype 'a array
  type 'a vector
  val tabulate: int * (int -> 'a) -> 'a array
  val array: int * 'a -> 'a array
  val fromList: 'a list -> 'a array
  val sub: 'a array * int -> 'a
  val update: 'a array * int * 'a -> unit
  val length: 'a array -> int
  val copy: {src: 'a array, dst: 'a array, di: int} -> unit
  val copyVec: {src: 'a vector, dst: 'a array, di: int} -> unit
  val foldl: ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
  val modify: ('a -> 'a) -> 'a array -> unit
  val vector: 'a array -> 'a vector
  val vlength: 'a vector -> int
  val vsub: 'a vector * int -> 'a
  val vfromList: 'a list -> 'a vector
  val veq: ''a vector * ''a vector -> bool
  val alloc: int -> 'a array
end

functor Test (A: ARR) =
struct
  fun for (lo, hi) f = if lo >= hi then () else (f lo; for (lo + 1, hi) f)
  val out = ref ([] : string list)
  fun say s = out := s :: !out
  fun i2s i = Int.toString i
  fun r2s r = Real.fmt (StringCvt.FIX (SOME 3)) r
  fun b2s b = if b then "T" else "F"

  (* 1. scalar pairs *)
  val a1 = A.tabulate (10, fn i => (Real.fromInt i, Real.fromInt (i*i)))
  val _ = A.update (a1, 3, (~1.0, ~2.0))
  val _ = A.modify (fn (x, y) => (y, x)) a1
  val _ = say ("t1 " ^ A.foldl (fn ((x, y), acc) => acc ^ r2s x ^ "/" ^ r2s y ^ " ") "" a1)
  val _ = say ("t1 len " ^ i2s (A.length a1))

  (* 2. nested tuple with a datatype component *)
  val a2 = A.tabulate (7, fn i => (i, (Real.fromInt i / 2.0, Word8.fromInt (i * 37)), i mod 2 = 0))
  val _ = A.update (a2, 5, (500, (5.5, 0w255), true))
  val _ = say ("t2 " ^ A.foldl (fn ((i, (r, w), b), acc) =>
             acc ^ i2s i ^ "," ^ r2s r ^ "," ^ Word8.toString w ^ "," ^ b2s b ^ " ") "" a2)
  val (i, (r, _), _) = A.sub (a2, 5)
  val _ = say ("t2 sub " ^ i2s i ^ " " ^ r2s r)

  (* 3. one pointer component *)
  val a3 = A.tabulate (5, fn i => ("s" ^ i2s i, i))
  val a3b = A.array (8, ("x", 0))
  val _ = A.copy {src = a3, dst = a3b, di = 2}
  val _ = say ("t3 " ^ A.foldl (fn ((s, i), acc) => acc ^ s ^ ":" ^ i2s i ^ " ") "" a3b)

  (* 4. two pointer components *)
  val a4 = A.tabulate (4, fn i => ("q" ^ i2s i, List.tabulate (i, fn j => j), Real.fromInt i))
  val v4 = A.vector a4
  val a4b = A.array (6, ("z", [], 0.0))
  val _ = A.copyVec {src = v4, dst = a4b, di = 1}
  val _ = say ("t4 " ^ A.foldl (fn ((s, l, r), acc) =>
             acc ^ s ^ "[" ^ String.concatWith "," (List.map i2s l) ^ "]" ^ r2s r ^ " ") "" a4b)
  val _ = say ("t4 vlen " ^ i2s (A.vlength v4) ^ " " ^ #1 (A.vsub (v4, 3)))

  (* 5. alloc (raw alloc + uninit), with pointers *)
  val a5: (string * int list * real) A.array = A.alloc 1000
  val _ = for (0, 1000) (fn i => A.update (a5, i, (i2s i, [i], Real.fromInt i)))
  val _ = say ("t5 " ^ #1 (A.sub (a5, 999)) ^ " " ^ i2s (hd (#2 (A.sub (a5, 500)))))
  val a5s: (real * real) A.array = A.alloc 100
  val _ = for (0, 100) (fn i => A.update (a5s, i, (Real.fromInt i, 1.0)))
  val _ = say ("t5s " ^ r2s (A.foldl (fn ((x, y), acc) => acc + x * y) 0.0 a5s))
  val a5p: (string * int) A.array = A.alloc 300
  val _ = for (0, 300) (fn i => A.update (a5p, i, (i2s (i * 2), i)))
  val _ = say ("t5p " ^ #1 (A.sub (a5p, 299)) ^ " " ^ i2s (A.foldl (fn ((_, i), acc) => acc + i) 0 a5p))

  (* 6. single ground, refs, unit *)
  val a6 = A.tabulate (5, fn i => i * 3)
  val a6r = A.tabulate (3, fn i => (ref i, i))
  val _ = #1 (A.sub (a6r, 1)) := 42
  val a6u = A.tabulate (4, fn _ => ())
  val _ = A.update (a6u, 2, ())
  val _ = say ("t6 " ^ i2s (A.foldl op+ 0 a6) ^ " " ^ i2s (! (#1 (A.sub (a6r, 1))))
               ^ " " ^ i2s (A.length a6u))

  (* 7. vectors: fromList, equality *)
  val v7a = A.vfromList [(1, (0w2: Word8.word, "a")), (3, (0w4, "b"))]
  val v7b = A.vector (A.fromList [(1, (0w2: Word8.word, "a")), (3, (0w4, "b"))])
  val v7c = A.vfromList [(1, (0w2: Word8.word, "a")), (3, (0w4, "c"))]
  val v7e = A.vfromList ([] : (int * int) list)
  val _ = say ("t7 " ^ b2s (A.veq (v7a, v7b)) ^ b2s (A.veq (v7a, v7c))
               ^ " " ^ i2s (A.vlength v7e))

  (* 8. array identity *)
  val a8 = A.tabulate (3, fn i => (i, i))
  val a8b = A.tabulate (3, fn i => (i, i))
  val _ = say ("t8 " ^ b2s (a8 = a8) ^ b2s (a8 = a8b))

  (* 9. Default array of these arrays *)
  val a9 = Array.tabulate (4, fn i => A.tabulate (i + 1, fn j => (i, Real.fromInt j)))
  val _ = A.update (Array.sub (a9, 2), 1, (99, 9.9))
  val _ = say ("t9 " ^ Array.foldl (fn (arr, acc) =>
             acc ^ A.foldl (fn ((i, r), acc) => acc ^ i2s i ^ "/" ^ r2s r ^ " ") "" arr ^ "| ")
             "" a9)

  (* 10. these arrays nested inside elements of these arrays *)
  val a10 = A.tabulate (3, fn i => (i, A.tabulate (2, fn j => (j, i * 10 + j))))
  val _ = A.update (#2 (A.sub (a10, 1)), 0, (7, 77))
  val _ = say ("t10 " ^ A.foldl (fn ((i, inner), acc) =>
             acc ^ i2s i ^ ":" ^ A.foldl (fn ((x, y), acc) => acc ^ i2s x ^ "," ^ i2s y ^ ";") "" inner ^ " ")
             "" a10)

  val transcript = String.concatWith "\n" (List.rev (!out))
end

structure Std = Test (struct
  open Array
  type 'a vector = 'a Vector.vector
  val vlength = Vector.length
  val vsub = Vector.sub
  val vfromList = Vector.fromList
  fun veq (a, b) = a = b
  fun alloc n = ForkJoin.alloc n
end)

structure Soa = Test (struct
  open MPL.ArraySoa
  val vlength = MPL.VectorSoa.length
  val vsub = MPL.VectorSoa.sub
  val vfromList = MPL.VectorSoa.fromList
  fun veq (a, b) = a = b
  fun alloc n = ForkJoin.alloc_soa n
end)

(* sequence literals *)
val lit = SoaLit.vec3 ((1, (1.5, true)), (2, (2.5, false)), (3, (3.5, true)))
val litAos = SoaLit.vec3aos ((1, (1.5, true)), (2, (2.5, false)), (3, (3.5, true)))
fun litStr (len, sub) =
  String.concatWith " " (List.tabulate (len, fn i =>
    let val (a, (b, c)) = sub i
    in Int.toString a ^ "," ^ Real.toString b ^ "," ^ Bool.toString c
    end))
val litS = litStr (MPL.VectorSoa.length lit, fn i => MPL.VectorSoa.sub (lit, i))
val litA = litStr (MPL.VectorAos.length litAos, fn i => MPL.VectorAos.sub (litAos, i))
val litExpected = "1,1.5,true 2,2.5,false 3,3.5,true"

val _ = print (Soa.transcript ^ "\n")
val _ = print ("literal soa: " ^ litS ^ "\nliteral aos: " ^ litA ^ "\n")
val ok = Std.transcript = Soa.transcript andalso litS = litExpected andalso litA = litExpected
val _ = print (if ok then "PASS\n" else ("FAIL\n--- expected:\n" ^ Std.transcript ^ "\n"))
val _ = if ok then () else OS.Process.exit OS.Process.failure
