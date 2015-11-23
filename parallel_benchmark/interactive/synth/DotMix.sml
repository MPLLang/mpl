(* DotMix, from "Deterministic Parallel Random-Number Generation for
 * Dynamic-Multithreading Platforms", by Leiserson, Schardl, and Sukha. *)
functor MkDotMixParams
  (val init : int (* used for generating `randoms`, aka `Gamma` in the paper *)
   val maxDepth : int (* maxDepth >= 1. aka `D` in the paper *)
   val mixNum : int (* mixNum >= 1. aka `r` in the paper *))
  :> RANDOM210 =
struct

  structure W = Word32

  (* 4,294,967,291 is the 203,280,221st prime, if anyone's counting. *)
  val magicPrime : W.word = 0w4294967291 (* 2^32 - 5 *)

  (* need constant vector of randoms, uniform in [0, magicPrime). Use
   * MersenneTwister just for these. *)
  val randoms =
    let fun generate n r =
          if n = 0 then [] else
          let val (wd, r') = MersenneTwister.randomWord r (* wd is 32-bits *)
          in W.fromLarge (wd mod W.toLarge magicPrime) :: generate (n-1) r'
          end
    in generate maxDepth (MersenneTwister.fromInt init)
    end

  (* rand type is basically a pedigree *)
  type rand = {seed : W.word, depth : int, rank : W.word, parents : W.word list}

  exception TooDeep

  fun fromInt (x : int) : rand =
    {seed = W.fromInt x, depth = 1, rank = 0w0, parents = []}

  fun toList ({rank, parents, ...} : rand) = List.rev (rank :: parents)

  fun advanceBy (k : int) ({seed, depth, rank, parents} : rand) : rand =
    {seed = seed, depth = depth, rank = rank + (W.fromInt k), parents = parents}

  fun next r = advanceBy 1 r

  fun selectChildOf ({seed, depth, rank, parents} : rand) (i : int) : rand =
    if depth = maxDepth then raise TooDeep else
    {seed = seed, depth = depth + 1, rank = 0w0, parents = (rank + W.fromInt i) :: parents}

  fun split r =
    (advanceBy 2 r, (selectChildOf r 0, selectChildOf r 1))

  fun split3 r =
    (advanceBy 3 r, (selectChildOf r 0, selectChildOf r 1, selectChildOf r 2))

  fun splitTab (r, n) =
    (advanceBy n r, selectChildOf r)

  (* DotMix procedures: compression and mixing *)
  fun compress r =
    let
      fun zip (xs, ys) =
        case (xs, ys) of
          ([], _) => []
        | (_, []) => []
        | (x :: xs', y :: ys') => (x, y) :: zip (xs', ys')

      (* ensure pedigree values j are bounded by 1 <= j < magicPrime *)
      val J = List.map (fn j => 0w1 + W.mod (j, magicPrime - 0w1)) (toList r)

      (* perform dot product with `randoms` *)
      val dotted = List.foldl op+ 0w0 (List.map op* (zip (J, randoms)))
    in
      W.mod (dotted, magicPrime)
    end

  fun mix z =
    let
      (* phi swaps upper and lower halves of input *)
      fun phi z =
        let val upperhalf = W.>> (z, Word.fromInt (W.wordSize div 2))
            val lowerhalf = W.<< (z, Word.fromInt (W.wordSize - (W.wordSize div 2)))
        in W.orb (upperhalf, lowerhalf)
        end

      fun f z = phi (0w2 * z * z + z)

      fun repeatf n z =
        if n = 0 then z else repeatf (n-1) (f z)
    in
      repeatf mixNum z
    end

  fun dotmix r =
    mix (#seed r + compress r)


  (* random generation *)
  exception NotYetImplemented

  fun bool r =
    W.< (dotmix r, W.<< (0w1, Word.fromInt (W.wordSize - 1)))

  fun biasedBool (h, t) r =
    let val (hw, tw) = (W.fromInt h, W.fromInt t)
    in W.<= (dotmix r, hw * W.div (0w0 - 0w1, hw + tw))
    end

  fun int r =
    case Int.precision of
      NONE => W.toIntX (dotmix r)
    | SOME p =>
        if p >= W.wordSize then W.toIntX (dotmix r) else
        let
          val wd = dotmix r
          val pw = Word.fromInt p
          val pthBitMask = W.<< (0w1, pw - 0w1)
          val pthBit = W.>> (W.andb (wd, pthBitMask), pw - 0w1)
          val xMask = W.<< (0w1, pw - 0w1) - 0w1
          val x = W.toIntX (W.andb (wd, xMask))
        in
          if pthBit = 0w1 then x else ~x - 1
        end

  fun boundedInt (low, high) r =
    let
      val x = W.toLargeInt (dotmix r)
      val low' = LargeInt.fromInt low
      val high' = LargeInt.fromInt high
    in
      LargeInt.toInt (x mod (high' - low') + low')
    end

  fun real r = raise NotYetImplemented

  fun boundedReal (low, high) r = raise NotYetImplemented

  fun char r = raise NotYetImplemented

  fun boundedChar r = raise NotYetImplemented

end

structure DotMix = MkDotMixParams (val init = 42
                                   val maxDepth = 500
                                   val mixNum = 4)
