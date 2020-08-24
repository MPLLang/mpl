structure ReadFile:
sig
  val contents: string -> string
  val contentsSeq: string -> char Seq.t
  val contentsBinSeq: string -> Word8.word Seq.t
end =
struct

  fun contentsSeq' reader filename =
    let
      val file = MPL.File.openFile filename
      val n = MPL.File.size file
      val arr = ForkJoin.alloc n
      val k = 10000
      val m = 1 + (n-1) div k
    in
      ForkJoin.parfor 1 (0, m) (fn i =>
        let
          val lo = i*k
          val hi = Int.min ((i+1)*k, n)
        in
          reader file lo (ArraySlice.slice (arr, lo, SOME (hi-lo)))
        end);
      MPL.File.closeFile file;
      ArraySlice.full arr
    end

  fun contentsSeq filename =
    contentsSeq' MPL.File.readChars filename

  fun contentsBinSeq filename =
    contentsSeq' MPL.File.readWord8s filename

  fun contents filename =
    let
      val chars = contentsSeq filename
    in
      CharVector.tabulate (ArraySlice.length chars,
        fn i => ArraySlice.sub (chars, i))
    end

end
