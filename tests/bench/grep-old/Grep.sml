structure Grep:
sig
  (* returns number of matching lines, and output that unix grep would show *)
  val grep : char Seq.t  (* pattern *)
          -> char Seq.t  (* source text *)
          -> int * (char Seq.t)
end =
struct

  structure A = Array
  structure AS = ArraySlice

  type 'a seq = 'a Seq.t

  (* fun lines (s : char seq) : (char seq) Seq.seq =
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
    end *)

  (* check if line[i..] matches the pattern *)
  fun checkMatch pattern line i =
    (i + Seq.length pattern <= Seq.length line) andalso
    let
      val m = Seq.length pattern
      (* pattern[j..] matches line[i+j..] *)
      fun matchesFrom j =
        (j >= m) orelse
        ((Seq.nth line (i+j) = Seq.nth pattern j) andalso matchesFrom (j+1))
    in
      matchesFrom 0
    end

  (* fun grep pat source =
    let
      val granularity = CommandLineArgs.parseOrDefaultInt "granularity" 1000
      val ff = FindFirst.findFirst granularity
      (* val ff = FindFirst.findFirstSerial *)
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
    end *)

  val ffGrain = CommandLineArgs.parseInt "ff-grain" 1000
  val findFirst = FindFirst.findFirst ffGrain

  fun grep pat source =
    let
      fun isNewline i = (Seq.nth source i = #"\n")

      val nlPos =
        AS.full (SeqBasis.filter 10000 (0, Seq.length source) (fn i => i) isNewline)
      val numLines = Seq.length nlPos + 1
      fun lineStart i =
        if i = 0 then 0 else 1 + Seq.nth nlPos (i-1)
      fun lineEnd i =
        if i = Seq.length nlPos then Seq.length source else Seq.nth nlPos i
      fun line i = Seq.subseq source (lineStart i, lineEnd i - lineStart i)

      (* val _ = print ("got newline positions\n") *)

      (* compute whether or not each line contains the pattern *)
      val hasPatFlags = AS.full (SeqBasis.tabulate 1000 (0, numLines) (fn i =>
        let
          val ln = line i
        in
          case findFirst (0, Seq.length ln) (checkMatch pat ln) of
            SOME _ => true
          | NONE => false
        end))

      (* val _ = print ("found the patterns\n") *)

      val linesWithPat =
        AS.full (SeqBasis.filter 4096 (0, numLines) (fn i => i) (Seq.nth hasPatFlags))
      val numLinesOutput = Seq.length linesWithPat

      (* val _ = print ("filtered the lines\n") *)
      (* val _ = print ("num lines: " ^ Int.toString numLinesOutput ^ "\n") *)

      val outputOffsets =
        AS.full (SeqBasis.scan 4096 op+ 0 (0, numLinesOutput)
          (* +1 to include newline *)
          (fn i => 1 + Seq.length (line (Seq.nth linesWithPat i))))

      val outputLen = Seq.nth outputOffsets numLinesOutput

      (* val _ = print ("computed line offsets\n") *)

      val output = ForkJoin.alloc outputLen
      fun put i c = A.update (output, i, c)
    in
      ForkJoin.parfor 1000 (0, numLinesOutput) (fn i =>
        let
          val ln = line (Seq.nth linesWithPat i)
          val off = Seq.nth outputOffsets i
        in
          Seq.foreach ln (fn (j, c) => put (off+j) c);
          put (off + Seq.length ln) #"\n"
        end);

      (numLinesOutput, AS.full output)
    end

end
