structure MLtonParallelInternal =
struct
  val processorNumber: unit -> Word32.word =
      _import "Parallel_processorNumber" runtime private: unit -> Word32.word;

  val numberOfProcessors: Word32.word =
      (_import "Parallel_numberOfProcessors" runtime private:
       unit -> Word32.word;) ()

  fun dbgmsg m =
      let
        val p = Word32.toInt (processorNumber ())
        val msg = String.concat ["[", Int.toString p, "] ", m, "\n"]
      in
        (TextIO.output (TextIO.stdErr, msg);
         TextIO.flushOut TextIO.stdErr)
      end
end
