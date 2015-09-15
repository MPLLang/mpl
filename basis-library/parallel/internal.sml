structure MLtonParallelInternal =
struct
    val processorNumber: unit -> Word32.word =
        _import "Parallel_processorNumber" runtime private: unit -> Word32.word;

    val numberOfProcessors: Word32.word =
        (_import "Parallel_numberOfProcessors" runtime private:
         unit -> Word32.word;) ()
end
