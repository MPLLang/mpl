structure PosixWriteFile =
struct
  fun dump (filename: string, contents: string) =
    let
      open Posix.FileSys
      val f = creat (filename, S.flags [S.iwusr, S.irusr, S.irgrp, S.iroth])
      val contentslice = (Word8VectorSlice.full (Byte.stringToBytes contents))
    in
     (Posix.IO.writeVec (f, contentslice);
      Posix.IO.close f;
      ())
    end
end
