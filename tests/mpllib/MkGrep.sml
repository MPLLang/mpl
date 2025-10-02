functor MkGrep (Seq: SEQUENCE) :
sig
  val grep: char ArraySequence.t  (* pattern *)
         -> char ArraySequence.t  (* source text *)
         -> (int * int) ArraySequence.t  (* output line ranges *)
end =
struct

  structure ASeq = ArraySequence

  type 'a seq = 'a ASeq.t

(*
  fun lines (s: char seq) : (char seq) Seq.seq =
    let
      val n = ASeq.length s
      val indices = Seq.tabulate (fn i => i) n
      fun isNewline i = (ASeq.nth s i = #"\n")
      val locs = Seq.filter isNewline indices
      val m = Seq.length locs

      fun line i =
        let
          val lo = (if i = 0 then 0 else 1 + Seq.nth locs (i-1))
          val hi = (if i = m then n else Seq.nth locs i)
        in
          ASeq.subseq s (lo, hi-lo)
        end
    in
      Seq.tabulate line (m+1)
    end
*)

  (* check if line[i..] matches the pattern *)
  fun checkMatch pattern line i =
    (i + ASeq.length pattern <= ASeq.length line) andalso
    let
      val m = ASeq.length pattern
      (* pattern[j..] matches line[i+j..] *)
      fun matchesFrom j =
        (j >= m) orelse
        ((ASeq.nth line (i+j) = ASeq.nth pattern j) andalso matchesFrom (j+1))
    in
      matchesFrom 0
    end

(*
  fun grep pat source =
    let
      val granularity = CommandLineArgs.parseOrDefaultInt "granularity" 1000
      (* val ff = FindFirst.findFirst granularity *)
      val ff = FindFirst.findFirstSerial
      fun containsPat line =
        case ff (0, ASeq.length line) (checkMatch pat line) of
          NONE => false
        | SOME _ => true

      val linesWithPat = Seq.filter containsPat (lines source)
      val newln = Seq.singleton #"\n"

      fun choose i =
        if Util.even i
        then Seq.fromArraySeq (Seq.nth linesWithPat (i div 2))
        else newln
    in
      Seq.toArraySeq (Seq.flatten (Seq.tabulate choose (2 * Seq.length linesWithPat)))
    end
*)

  fun isNewline c = (c = #"\n")
  val ff = FindFirst.findFirst 1000

  fun grep pat s =
    let
      fun makeLine (start, stop) = ASeq.subseq s (start, stop-start)
      fun containsPat (start, stop) =
        case ff (0, stop-start) (checkMatch pat (makeLine (start, stop))) of
          NONE => NONE
        | SOME _ => SOME (start, stop)

      val s = Seq.fromArraySeq s
      val n = Seq.length s

      val idx = Seq.filter (isNewline o Seq.nth s) (Seq.tabulate (fn i => i) n)
      (* val idx =
        Seq.mapOption
          (fn i => if isNewline (Seq.nth s i) then SOME i else NONE)
          (Seq.tabulate (fn i => i) n) *)

      val m = Seq.length idx

      fun line i =
        let
          val start = if i = 0 then 0 else Seq.nth idx (i-1)
          val stop = if i = m then n else Seq.nth idx i
        in
          (start, stop)
        end

    in
      (* Seq.toArraySeq (Seq.filter containsPat (Seq.tabulate line (m+1))) *)
      Seq.toArraySeq (Seq.mapOption containsPat (Seq.tabulate line (m+1)))
    end

end
