val size = 300000000
val grain = 65536
val par = ForkJoin.fork

fun for (lo, hi) (f : int -> unit) : unit =
  if lo >= hi then () else (f lo; for (lo+1, hi) f)

fun parfor (lo, hi) (f : int -> unit) : unit =
  if hi - lo <= grain
  then for (lo, hi) f
  else let
         val mid = lo + (hi - lo) div 2
       in
         par (fn _ => parfor (lo, mid) f, fn _ => parfor (mid, hi) f);
         ()
       end

fun tab f n =
  let
    val a = Unsafe.Array.alloc n
  in
    parfor (0, n) (fn i => Array.update (a, i, f i));
    a
  end

fun hash i =
  let
    open Word64
    infix 2 >> infix 2 << infix 2 xorb infix 2 andb
    val v = fromInt(i) * 0w3935559000370003845 + 0w2691343689449507681
    val v = v xorb (v >> 0w21)
    val v = v xorb (v << 0w37)
    val v = v xorb (v >> 0w4)
    val v = v * 0w4768777513237032717
    val v = v xorb (v << 0w20)
    val v = v xorb (v >> 0w41)
    val v = v xorb (v << 0w5)
  in
    Word64.toInt (v andb ((0w1 << 0w31) - 0w1))
  end

val t0 = Time.now ()
val r = tab hash size
val t1 = Time.now ()

val _ = print (LargeInt.toString (Time.toMilliseconds (Time.- (t1, t0))) ^ " ms\n")

val itos = Int.toString
val _ =
  case Array.findi (fn (i, x) => hash i <> x) r of
    NONE => print "correct\n"
  | SOME (i, x) =>
      print (String.concat
        [ "incorrect. index ", itos i, " is ", itos x
        , " but should be ", itos (hash i), "\n"
        ])

