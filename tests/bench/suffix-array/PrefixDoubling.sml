(* Author: Lawrence Wang (lawrenc2@andrew.cmu.edu, github.com/larry98)
 *
 * Based on the prefix doubling approach used by Manbar & Meyers and Larson &
 * Sudakane. The general idea is to repeatedly sort the suffixes by their first
 * k characters for k = 1, 2, 4, 8, ... This can be done efficiently by keeping
 * track of the "ranks" (or groups, buckets, inverse suffix array, etc) of each
 * suffix on each round.
 *
 * Our algorithm maintains sets of "active groups", which are groups of
 * suffixes whose k-prefixes are equal. Each of the active groups are processed
 * in parallel, and the processing consists of sorting the suffixes in each
 * group by their 2k-prefixes, which may create more active groups for the
 * next round. We only maintain active groups of size greater than 1, which
 * is an optimization made in Larson & Sudakane. This effectively skips over
 * the parts of the suffix array which are already in their sorted positions.
 *
 * Larson & Sudakane also perform additional optimizations, such as using a
 * modified version of 3-way pivot quicksort that updates the group numbers
 * as part of the sorting routine (essentially it assigns the group number
 * when processing the EQUAL partition). We do not implement this optimization
 * as our algorithm uses DualPivotQuickSort.
 *)
structure PrefixDoublingSuffixArray :>
sig
  val makeSuffixArray : string -> int Seq.t
end =
struct

  val GRAIN = 10000

  structure AS =
  struct
    open ArraySlice
    open Seq
    val ASupdate = ArraySlice.update

    fun filter p s =
      full (SeqBasis.filter GRAIN (0, length s) (nth s) (p o nth s))

    fun scanIncl f b s =
      let
        val a = SeqBasis.scan GRAIN f b (0, length s) (nth s)
      in
        slice (a, 1, NONE)
      end
  end

  fun appG grain f s =
    ForkJoin.parfor grain (0, AS.length s) (fn i => f (AS.nth s i))

  fun initCountingSort str =
    let
      val n = String.size str

      fun bucket i = Char.ord (String.sub (str, i))
      val (sa, offsets) =
        CountingSort.sort (AS.tabulate (fn i => i) n) bucket 256
      val offsets =
        AS.full (SeqBasis.filter GRAIN (0, AS.length offsets) (AS.nth offsets)
        (fn i => i = 0 orelse AS.nth offsets i <> AS.nth offsets (i-1)))
      val numGroups = AS.length offsets
      val groupNum = ArraySlice.full (ForkJoin.alloc n)

      fun makeGroup offsets n i =
        let
          val off = AS.nth offsets i
          val len = if i = AS.length offsets - 1 then n - off
                    else AS.nth offsets (i + 1) - off
        in
          (off, len)
        end

      fun updateGroupNum (off, len) =
        ForkJoin.parfor GRAIN (off, off + len) (fn j =>
          AS.ASupdate (groupNum, AS.nth sa j, off)
        )

      val groups = AS.tabulate (makeGroup offsets n) numGroups
      val () = appG 1 updateGroupNum groups
    in
      (sa, groupNum, groups, 1)
    end

  fun initSampleSort str =
    let
      val n = String.size str

      fun pack i =
        let
          val charToWord = Word64.fromInt o Char.ord
          fun getChar i = if i >= n then Char.minChar else String.sub (str, i)
          val orb = Word64.orb
          val << = Word64.<<
          infix 2 << infix 2 orb
          val v = charToWord (String.sub (str, i)) << 0w56
          val v = v orb (charToWord (getChar (i + 1)) << 0w48)
          val v = v orb (charToWord (getChar (i + 2)) << 0w40)
          val v = v orb (charToWord (getChar (i + 3)) << 0w32)
        in
          v orb (Word64.fromInt i)
        end
      val words = SampleSort.sort Word64.compare (AS.tabulate pack n)
      val idxMask = 0w4294967295

      val sa = AS.map (fn w => Word64.toInt (Word64.andb (w, idxMask))) words
      val groupNum = ArraySlice.full (ForkJoin.alloc n)

      fun eq w1 w2 = Word64.>> (w1, 0w32) = Word64.>> (w2, 0w32)
      fun f i =
        if i > 0 andalso eq (AS.nth words i) (AS.nth words (i - 1))
          then 0
          else i
      val offsets = AS.scanIncl Int.max 0 (AS.tabulate f n)
      val maxOffset = AS.nth offsets (n - 1)
      val groups = AS.tabulate (fn i => (0, 0)) n
      val () = ForkJoin.parfor GRAIN (1, n) (fn i =>
        let
          val off1 = AS.nth offsets (i - 1)
          val off2 = AS.nth offsets i
        in
          AS.ASupdate (groupNum, AS.nth sa i, off2);
          if off1 = off2 then ()
          else AS.ASupdate (groups, i - 1, (off1, off2 - off1))
        end
      )
      val () = AS.ASupdate (groupNum, AS.nth sa 0, AS.nth offsets 0)
      val () = AS.ASupdate (groups, n - 1, (maxOffset, n - maxOffset))
    in
      (sa, groupNum, groups, 4)
    end

  fun makeSuffixArray str =
    let
      val n = String.size str
      val (sa, groupNum, groups, k) = initSampleSort str

      fun isActive (off, len) = len > 1
      val activeGroups = AS.filter isActive groups

      val ranks = ArraySlice.full (ForkJoin.alloc n)
      val aux = ArraySlice.full (ForkJoin.alloc n)

      fun loop activeGroups k =
        if AS.length activeGroups = 0 then ()
        else if k > n then ()
        else
          let
            fun cmp (i, j) =
              let
                val x = AS.nth ranks i
                val y = AS.nth ranks j
              in
                if x = ~1 andalso y = ~1 then EQUAL
                else if x = ~1 then LESS
                else if y = ~1 then GREATER
                else Int.compare (x, y)
              end

            fun sortGroup group =
              Quicksort.sortInPlaceG n cmp (AS.subseq sa group)

            fun expandGroupSeq s (off, len) =
              let
                fun loop i numGroups start =
                  if i = off + len then (
                    AS.ASupdate (s, numGroups, (start, i - start));
                    numGroups + 1
                  ) else if cmp (AS.nth sa i, AS.nth sa (i - 1)) <> EQUAL then (
                    AS.ASupdate (groupNum, AS.nth sa i, i);
                    AS.ASupdate (s, numGroups, (start, i - start));
                    loop (i + 1) (numGroups + 1) i
                  ) else (
                    AS.ASupdate (groupNum, AS.nth sa i, start);
                    loop (i + 1) numGroups start
                  )
                val numGroups = loop (off + 1) 0 off
                val () = AS.ASupdate (groupNum, AS.nth sa off, off)
              in
                Util.for (numGroups, len) (fn i =>
                  AS.ASupdate (s, i, (0, 0))
                )
              end

            fun expandGroupPar s (off, len) =
              let
                fun f i =
                  if i = 0 then off
                  else
                    case cmp (AS.nth sa (off + i), AS.nth sa (off + i - 1)) of
                         EQUAL => 0
                       | _ => off + i
                val names = AS.scanIncl Int.max 0 (AS.tabulate f len)
                val maxName = AS.nth names (len - 1)
              in
                (
                  ForkJoin.parfor GRAIN (1, len) (fn i => (
                    let
                      val name = AS.nth names i
                      val name' = AS.nth names (i - 1)
                    in
                      AS.ASupdate (groupNum, AS.nth sa (off + i), name);
                      if AS.nth names i = off + i andalso i > 0 then
                        AS.ASupdate (s, i - 1, (name', off + i - name'))
                      else AS.ASupdate (s, i - 1, (0, 0))
                    end
                  ));
                  AS.ASupdate (groupNum, AS.nth sa off, AS.nth names 0);
                  AS.ASupdate (s, len - 1, (maxName, off + len - maxName))
                )
              end

            val seqExpand =
              AS.length activeGroups > Concurrency.numberOfProcessors
            fun expandGroup s (off, len) =
              if len <= GRAIN orelse seqExpand then expandGroupSeq s (off, len)
              else expandGroupPar s (off, len)

            val groupLens = AS.map #2 activeGroups
            val (groupStarts, maxGroups) = AS.scan (op +) 0 groupLens
            val avgLen = maxGroups div (AS.length activeGroups)
            (* TODO: tune grain size *)
            val grain = if avgLen >= 256 then 1
                        else if avgLen >= 64 then 32
                        else if avgLen >= 16 then 64
                        else 4096

            val () = print ("grain is " ^ (Int.toString grain) ^ "\n")
            val () = print ("avgLen is " ^ (Int.toString avgLen) ^ "\n")
            val () = print ("numGroups is " ^ (Int.toString (AS.length activeGroups)) ^ "\n")

            (* Its faster to copy all of the ranks instead of just those in
               active groups *)
            val () = ForkJoin.parfor GRAIN (0, n) (fn i =>
              let val x = if i + k >= n then ~1 else AS.nth groupNum (i + k)
              in AS.ASupdate (ranks, i, x) end
            )

            val newGroups = AS.take aux maxGroups
            val () = ForkJoin.parfor grain (0, AS.length activeGroups) (fn i =>
              let
                val group = AS.nth activeGroups i
                val start = AS.nth groupStarts i
                val s = AS.subseq newGroups (start, #2 group)
              in
                (sortGroup group; expandGroup s group)
              end
            )
          in
            loop (AS.filter isActive newGroups) (2 * k)
          end
      val () = loop activeGroups k
    in
      sa
    end

end
