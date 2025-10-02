structure Shuffle :>
sig
  type 'a seq = 'a ArraySlice.slice
  val shuffle: 'a seq -> int -> 'a seq
end =
struct
  open Seq
  type 'a seq = 'a ArraySlice.slice

  (* inplace Knuth shuffle [l, r) *)
  fun inplace_seq_shuffle s l r seed =
    let
      fun item i = AS.sub (s, i)
      fun set (i, v) = AS.update (s, i, v)
      (* get a random idx in [l, i] *)
      fun rand_idx i = Int.mod (Util.hash (seed + i), i - l + 1) + l
      fun swap (i,j) =
        let
          val tmp = item i
        in
          set(i, item j); set(j, tmp)
        end
      fun shuffle_helper li =
        if r - li < 2  then ()
        else (swap (li, rand_idx li); shuffle_helper (li + 1))
    in
      shuffle_helper l
    end

  fun bucket_shuffle s seed =
    let
      fun log2_up n = Real.ceil (Math.log10 (Real.fromInt n) / (Math.log10 2.0))
      fun bit_and (n, mask) = Word.toInt (Word.andb (Word.fromInt n, mask))

      val n = length s
      val l = log2_up n
      val bits = if n < Real.floor (Math.pow (2.0, 27.0)) then Int.div ((l - 7), 2)
                  else l - 17
      val num_buckets = Real.floor (Math.pow (2.0, Real.fromInt bits))
      val mask = Word.fromInt (num_buckets - 1)
      fun rand_pos i = bit_and (Util.hash (seed + i), mask)
      (* size of bucket_offsets = num_buckets + 1 *)
      val (s', bucket_offsets) = CountingSort.sort s rand_pos num_buckets
      fun bucket_shuffle i = inplace_seq_shuffle s' (nth bucket_offsets i) (nth bucket_offsets (i + 1)) seed
      val _ = ForkJoin.parfor 1 (0, num_buckets) bucket_shuffle
    in
      s'
    end

  fun shuffle s seed =
    let
      val n = length s
    in
      if n < 1000 then
        let
          val s' = (Seq.tabulate (Seq.nth s) n)
          val _ = inplace_seq_shuffle s' 0 n seed
        in
          s'
        end
      else
        bucket_shuffle s seed
    end
end
