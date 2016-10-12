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
      in
        print (String.concat ["[",
                              Int.toString p,
                              "] ",
                              m,
                              "\n"])
      end
end
