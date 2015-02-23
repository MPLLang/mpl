structure MLtonParallelInternal =
struct
    val processorNumber: unit -> Int32.int =
        _import "Parallel_processorNumber" runtime private: unit -> Int32.int;

    val numberOfProcessors: Int32.int =
        Int32.toInt ((_import
                          "Parallel_numberOfProcessors"
                          runtime private: unit -> Int32.int;) ())
end
