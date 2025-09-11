(* Author: Lawrence Wang (lawrenc2@andrew.cmu.edu, github.com/larry98)
 *)

structure BruteForceSuffixArray :>
sig
  val makeSuffixArray : string -> int Seq.t
end =
struct

  fun makeSuffixArray str =
    let
      val n = String.size str
      val sa = AS.tabulate (fn i => i) n
      fun cmp k (i, j) =
        if i = j then EQUAL
        else if i + k >= n then LESS
        else if j + k >= n then GREATER
        else
          let
            val c1 = String.sub (str, i + k)
            val c2 = String.sub (str, j + k)
          in
            Char.compare (c1, c2)
          end
    in
      RadixSort.quicksort sa cmp n
    end

end
