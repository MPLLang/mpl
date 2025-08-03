structure SimpleRandom :>
sig
  type t

  val rand : int -> t

  (* `boundedInt (lo, hi) seed` destructively updates `seed`, returning a
   * random integer in the range [lo, hi). Assumes lo < hi. *)
  val boundedInt : int * int -> t -> int
end =
struct

  type t = Word.word ref

  fun rand (x : int) : t = ref (Word.fromInt x)
  
  fun next (seed : t) : word =
    let val res = Word.+ (Word.* (0w1664525, !seed), 0w1013904223)
    in seed := res; res
    end

  fun boundedInt (lo : int, hi : int) (seed : t) : int =
    lo + (Word.toIntX (next seed)) mod (hi - lo)

end
