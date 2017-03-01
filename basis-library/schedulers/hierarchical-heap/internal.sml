structure MLtonParallelInternal =
struct
   val numberOfProcessors = MLton.Parallel.numberOfProcessors
   val processorNumber = MLton.Parallel.processorNumber

  fun dbgmsg m =
      let
        val p = processorNumber ()
        val msg = String.concat ["[", Int.toString p, "] ", m, "\n"]
      in
        (TextIO.output (TextIO.stdErr, msg);
         TextIO.flushOut TextIO.stdErr)
      end
end
