fun badmap f [] = []
  | badmap f (x :: xs) =
      let
        val (x', xs') = ForkJoin.fork (fn _ => f x, fn _ => badmap f xs)
      in
        x' :: xs'
      end

val input = List.tabulate (100000, fn i => i)
val t0 = Time.now ()
val result = badmap (fn x => x+1) input
val t1 = Time.now ()
val _ = print ("[" ^ String.concatWith "," (List.map Int.toString (List.take (result, 20))) ^ ", ...]\n")
val _ = print (LargeInt.toString (Time.toMilliseconds (Time.- (t1, t0))) ^ " ms\n")
