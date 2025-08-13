structure SimpleRandom :>
sig
  type t

  val rand: int -> t

  (* `boundedInt (lo, hi) seed` destructively updates `seed`, returning a
   * random integer in the range [lo, hi). Assumes lo < hi. *)
  val boundedInt: int * int -> t -> int
end =
struct

  type w64 = Word64.word

  datatype t = RandState of {base: w64, count: w64 ref}

  fun hash64 x =
    let
      open Word64
      infix 2 >> << xorb orb
      val x = (x xorb (x >> 0w30)) * 0wxbf58476d1ce4e5b9
      val x = (x xorb (x >> 0w27)) * 0wx94d049bb133111eb
      val x = x xorb (x >> 0w31)
    in
      x
    end

  fun rand (x: int) : t =
    RandState {base = hash64 (Word64.fromInt x), count = ref (0w0 : w64)}

  fun next (RandState {base, count}) : w64 =
    let val c = !count
    in count := Word64.+ (c, 0w1); Word64.+ (base, hash64 c)
    end

  fun boundedInt (lo: int, hi: int) (state: t) : int =
    let
      val range = Word64.fromInt (Int.max (1, hi - lo))
      val x = next state
      val x = Word64.mod (x, range)
      val x = Word64.+ (Word64.fromInt lo, x)
    in
      Word64.toIntX x
    end

end
