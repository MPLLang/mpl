structure ReadFile:
sig
  val contents: string -> string
  val contentsSeq: string -> char Seq.t
end =
struct

  type 'a seq = 'a Seq.t

  fun contentsSeq filename =
    let
      val inStream = TextIO.openIn filename
      fun loop () =
        let
          val str = TextIO.inputN (inStream, 100000)
        in
          if String.size str = 0 then []
          else str :: loop ()
        end
      val blocks = ArraySlice.full (Array.fromList (loop ()))
      val numBlocks = Seq.length blocks
      fun blockLen i = Vector.length (Seq.nth blocks i)
      val starts = ArraySlice.full
        (SeqBasis.scan 10000 op+ 0 (0, numBlocks) blockLen)
      val total = Seq.nth starts numBlocks
      val r = ForkJoin.alloc total
      fun writeBlock i =
        let
           val start = Seq.nth starts i
           val block = Seq.nth blocks i
        in
          Vector.appi (fn (j, x) => Array.update (r, start + j, x)) block
        end
      val _ = ForkJoin.parfor 1 (0, numBlocks) writeBlock
    in
      ArraySlice.full r
    end

  fun contents filename =
    let
      val charArr = contentsSeq filename
    in
      CharVector.tabulate (Seq.length charArr, Seq.nth charArr)
    end
end
