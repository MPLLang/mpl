structure CityGen:
sig

  (* (city n x) produces a sequence of n random buildings, seeded by x (any
   * integer will do). *)
  val city : int -> int -> (int * int * int) PureSeq.t

  (* (cities m n x) produces m cities, each is a sequence of at most n random
   * buildings, seeded by x (any integer will do). *)
  val cities : int -> int -> int -> (int * int * int) PureSeq.t PureSeq.t
end =
struct

  structure R = FastHashRand

  (* Fisher-Yates shuffle aka Knuth shuffle *)
  fun shuffle s r =
    let
      val n = PureSeq.length s
      val data = Array.tabulate (n, PureSeq.nth s)

      fun swapLoop (r, i) =
        if i >= n then r
        else let
          val j = R.boundedInt (i, n) r
          val (x, y) = (Array.sub (data, i), Array.sub (data, j))
        in
          Array.update (data, i, y);
          Array.update (data, j, x);
          swapLoop (R.next r, i+1)
        end

      val r' = swapLoop (r, 0)
    in
      (r', PureSeq.tabulate (fn i => Array.sub (data, i)) n)
    end

  fun citySeeded n r0 =
    let
      val (r1, xs) = shuffle (PureSeq.tabulate (fn i => i) (2*n)) r0
      val (_, seeds) = R.splitTab (r1, n)
      fun pow b e = if e <= 0 then 1 else b * pow b (e-1)

      fun makeBuilding i =
        let
          val xpair = (PureSeq.nth xs (2*i), PureSeq.nth xs (2*i + 1))
          val lo = Int.min xpair
          val hi = Int.max xpair
          val width = hi-lo
          val maxHeight = Int.max (1, 2*n div width)
          val maxHeight =
            if maxHeight >= n then
              1 + pow (Util.log2 maxHeight) 2
            else
              maxHeight + pow (Util.log2 maxHeight) 2
          val heightRange = (Int.max (1, maxHeight-(n div 100)), maxHeight+1)
          val height = R.boundedInt heightRange (seeds i)
        in
          (lo, height, hi)
        end
    in
      PureSeq.tabulate makeBuilding n
    end

  fun city n x = citySeeded n (R.fromInt x)

  fun cities m n x =
    let
      val (_, rs) = R.splitTab (R.fromInt x, m)
      fun ithCity i =
        let val r = rs i
        in citySeeded (R.boundedInt (0, n+1) r) (R.next r)
        end
    in PureSeq.tabulate ithCity m
    end

end
