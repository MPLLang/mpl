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

  type t = MLton.Pointer.t * int * bool ref

  val copyCharsToBuffer = _import "GC_memcpyToBuffer" runtime private:
    MLton.Pointer.t * char array * C_Size.word * C_Size.word -> unit;
  val copyWord8sToBuffer = _import "GC_memcpyToBuffer" runtime private:
    MLton.Pointer.t * Word8.word array * C_Size.word * C_Size.word -> unit;
  val mmapFileReadable = _import "GC_mmapFileReadable" runtime private:
    C_Int.int * C_Size.word -> MLton.Pointer.t;
  val release = _import "GC_release" runtime private:
    MLton.Pointer.t * C_Size.word -> unit;

  fun size (ptr, sz, stillOpen) =
    if !stillOpen then sz else raise Closed

  fun openFile path =
    let
      open Posix.FileSys
      val file = openf (path, O_RDONLY, O.fromWord 0w0)
      val size = Position.toInt (ST.size (fstat file))
      val fd = C_Int.fromInt (SysWord.toInt (fdToWord file))
      val ptr = mmapFileReadable (fd, C_Size.fromInt size)
    in
      (ptr, size, ref true)
    end

  exception Closed

  fun closeFile (ptr, size, stillOpen) =
    if !stillOpen then
      (release (ptr, C_Size.fromInt size); stillOpen := false)
    else
      raise Closed

  fun unsafeReadWord8 (ptr, _, _) i =
    MLton.Pointer.getWord8 (ptr, i)

  fun unsafeReadChar (ptr, _, _) i =
    Char.chr (Word8.toInt (MLton.Pointer.getWord8 (ptr, i)))

  fun readChar (ptr, size, stillOpen) (i: int) =
    if !stillOpen andalso i >= 0 andalso i < size then
      unsafeReadChar (ptr, size, stillOpen) i
    else if i < 0 orelse i >= size then
      raise Subscript
    else
      raise Closed

  fun readWord8 (ptr, size, stillOpen) (i: int) =
    if !stillOpen andalso i >= 0 andalso i < size then
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
      if !stillOpen andalso i >= 0 andalso i+n <= size then
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
      if !stillOpen andalso i >= 0 andalso i+n <= size then
        copyWord8sToBuffer (start, arr, C_Size.fromInt j, C_Size.fromInt n)
      else if i < 0 orelse i+n > size then
        raise Subscript
      else
        raise Closed
    end

end
