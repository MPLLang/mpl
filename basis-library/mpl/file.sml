(* Copyright (C) 2020 Sam Westrick.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MPLFile :> MPL_FILE =
struct
  local
    open Primitive.MLton.Pointer
  in
  structure C_Size = C_Size
  structure C_Int = C_Int
  end

  datatype FileState = OpenRead | OpenReadWrite | FileClosed

  type t = MLton.Pointer.t * int * FileState ref

  exception Closed
  exception WrongFilePermission

  open Primitive.MPL.File

  fun size (ptr, sz, stillOpen) =
    if !stillOpen = OpenRead orelse !stillOpen = OpenReadWrite then sz else raise Closed

  fun openFile path =
    let
      open Posix.FileSys
      val file = openf (path, O_RDWR, O.fromWord 0w0)
      val size = Position.toInt (ST.size (fstat file))
      val fd = C_Int.fromInt (SysWord.toInt (fdToWord file))
      val ptr = mmapFileReadable (fd, C_Size.fromInt size)
    in
      Posix.IO.close file;
      (ptr, size, ref OpenRead)
    end

  fun openFileWriteable path buffer_size =
    let
      open Posix.FileSys
      val file = createf (path, O_RDWR, O.append, S.flags [S.irusr, S.iwusr, S.irgrp, S.iroth])
      val original_size = Position.toInt (ST.size (fstat file))
      val final_size = buffer_size + original_size
      val fd = C_Int.fromInt (SysWord.toInt (fdToWord file))
      val _ = ftruncate (file, Position.fromInt final_size)
      val ptr = mmapFileWriteable (fd, C_Size.fromInt final_size)
    in
      Posix.IO.close file;
      {file = (ptr, final_size, ref OpenReadWrite), file_size = original_size}
    end

  fun closeFile (ptr, size, stillOpen) =
    if !stillOpen = OpenRead orelse !stillOpen = OpenReadWrite then
      (release (ptr, C_Size.fromInt size); stillOpen := FileClosed)
    else
      raise Closed

  fun unsafeReadWord8 (ptr, _, _) i =
    MLton.Pointer.getWord8 (ptr, i)

  fun unsafeReadChar (ptr, _, _) i =
    Char.chr (Word8.toInt (MLton.Pointer.getWord8 (ptr, i)))

  fun readChar (ptr, size, stillOpen) (i: int) =
    if (!stillOpen = OpenRead orelse !stillOpen = OpenReadWrite) andalso i >= 0 andalso i < size then
      unsafeReadChar (ptr, size, stillOpen) i
    else if i < 0 orelse i >= size then
      raise Subscript
    else
      raise Closed

  fun readWord8 (ptr, size, stillOpen) (i: int) =
    if (!stillOpen = OpenRead orelse !stillOpen = OpenReadWrite) andalso i >= 0 andalso i < size then
      unsafeReadWord8 (ptr, size, stillOpen) i
    else if i < 0 orelse i >= size then
      raise Subscript
    else
      raise Closed

  fun readChars (ptr, size, stillOpen) i slice =
    let
      val (arr, j, n) = ArraySlice.base slice
      val start = MLtonPointer.add (ptr, Word.fromInt i)
    in
      if (!stillOpen = OpenRead orelse !stillOpen = OpenReadWrite) andalso i >= 0 andalso i+n <= size then
        copyCharsToBuffer (start, arr, C_Size.fromInt j, C_Size.fromInt n)
      else if i < 0 orelse i+n > size then
        raise Subscript
      else
        raise Closed
    end

  fun readWord8s (ptr, size, stillOpen) i slice =
    let
      val (arr, j, n) = ArraySlice.base slice
      val start = MLtonPointer.add (ptr, Word.fromInt i)
    in
      if (!stillOpen = OpenRead orelse !stillOpen = OpenReadWrite) andalso i >= 0 andalso i+n <= size then
        copyWord8sToBuffer (start, arr, C_Size.fromInt j, C_Size.fromInt n)
      else if i < 0 orelse i+n > size then
        raise Subscript
      else
        raise Closed
    end

    fun writeChar {file = (ptr, size, stillOpen), file_offset = file_offset} c =
      if !stillOpen = OpenReadWrite andalso file_offset >= 0 andalso file_offset < size then
        MLton.Pointer.setWord8 (ptr, file_offset, Primitive.Char8.idToWord8 c)
      else if file_offset < 0 orelse file_offset >= size then
        raise Subscript
      else if !stillOpen = OpenRead then
           raise WrongFilePermission
        else
          raise Closed

      fun writeWord8s {file = (ptr, size, stillOpen), file_offset} slice =
        let
          val (arr, j, n) = ArraySlice.base slice
          val start = MLtonPointer.add (ptr, Word.fromInt file_offset)
        in
          if !stillOpen = OpenReadWrite andalso file_offset >= 0 andalso file_offset + n <= size then
            copyWord8sFromBuffer (start, arr, C_Size.fromInt j, C_Size.fromInt n)
          else if file_offset < 0 orelse file_offset + n > size then
            raise Subscript
          else if !stillOpen = OpenRead then
            raise WrongFilePermission
          else
            raise Closed
        end

end
