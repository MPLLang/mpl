structure MLtonParallelInternal =
struct

   val numberOfProcessors = Primitive.MLton.Parallel.numberOfProcessors
   val processorNumber = Primitive.MLton.Parallel.processorNumber

  fun dbgmsg m =
      let
        val p = Word32.toInt (processorNumber ())
        val msg = String.concat ["[", Int.toString p, "] ", m, "\n"]
      in
        (TextIO.output (TextIO.stdErr, msg);
         TextIO.flushOut TextIO.stdErr)
      end
end
