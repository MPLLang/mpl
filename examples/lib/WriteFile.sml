structure WriteFile:
sig
  val writeBinSeq: {filename: string, content: Word8.word Seq.t} -> unit
end =
struct

  fun writeBinSeq {filename, content} =
    let
      val n = Seq.length content
      val {file, file_size = oldSize} = MPL.File.openFileWriteable filename n
      val k = 10000
      val m = 1 + (n-1) div k
    in
      ForkJoin.parfor 1 (0, m) (fn i =>
        let 
          val lo = i*k
          val hi = Int.min ((i+1)*k, n)
        in
          MPL.File.writeWord8s { file = file , file_offset = oldSize + lo} (Seq.subseq content (lo, hi-lo))
        end
        );
      MPL.File.closeFile file
    end
end