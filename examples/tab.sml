val size = 10000000
val grain = 65536

datatype 'a tree = Leaf of 'a array | Node of int * 'a tree * 'a tree

fun length (Leaf a) = Array.length a
  | length (Node (n, _, _)) = n

fun nth t i =
  case t of
    Leaf a => Array.sub (a, i)
  | Node (_, l, r) =>
      if i < length l
      then nth l i
      else nth r (i - length l)

fun tab f (lo, hi) =
  if hi - lo <= grain
  then Leaf (Array.tabulate (hi - lo, fn i => f (lo + i)))
  else let val (l, r) = ForkJoin.fork (fn _ => tab f (lo, lo + (hi-lo) div 2),
                                       fn _ => tab f (lo + (hi-lo) div 2, hi))
       in Node (hi - lo, l, r)
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

val t = tab hash (0, size)
val _ = print (Int.toString (nth t (size div 2)) ^ "\n")
