structure MLtonParallelInternal =
struct
   val numberOfProcessors = Int32.toInt Primitive.MLton.Parallel.numberOfProcessors
   val processorNumber = Int32.toInt o Primitive.MLton.Parallel.processorNumber

  fun dbgmsg m =
      let
        val p = processorNumber ()
        val msg = String.concat ["[", Int.toString p, "] ", m, "\n"]
      in
        (TextIO.output (TextIO.stdErr, msg);
         TextIO.flushOut TextIO.stdErr)
      end
end
