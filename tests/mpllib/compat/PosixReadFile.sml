structure PosixReadFile =
struct

  fun contentsSeq' (readByte: Word8.word -> 'a) path =
    let
      val (file, length) =
        let
          open Posix.FileSys
          val file = openf (path, O_RDONLY, O.fromWord 0w0)
        in
          (file, Position.toInt (ST.size (fstat file)))
        end

      open Posix.IO

      val bufferSize = 100000
      val buffer = Word8ArrayExtra.alloc bufferSize
      val result = ArrayExtra.alloc length
      (* val result = Word8ArrayExtra.alloc length *)

      (* fun copyToResult i n =
        Word8ArraySlice.copy
          { src = Word8ArraySlice.slice (buffer, 0, SOME n)
          , dst = result
          , di = i
          } *)

      fun copyToResult i n =
        Word8ArraySlice.appi (fn (j, b) =>
          Unsafe.Array.update (result, i+j, readByte b))
          (Word8ArraySlice.slice (buffer, 0, SOME n))

      fun dumpFrom i =
        if i >= length then () else
        let
          val bytesRead = readArr (file, Word8ArraySlice.full buffer)
        in
          copyToResult i bytesRead;
          dumpFrom (i + bytesRead)
        end
    in
      dumpFrom 0;
      close file;
      ArraySlice.full result
    end

  fun contentsSeq path =
    contentsSeq' (Char.chr o Word8.toInt) path

  fun contentsBinSeq path =
    contentsSeq' (fn w => w) path

  fun contents filename =
    let
      val chars = contentsSeq filename
    in
      CharVector.tabulate (ArraySlice.length chars,
        fn i => ArraySlice.sub (chars, i))
    end
end
