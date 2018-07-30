structure MLtonParallelInternal =
struct

val numberOfProcessors =
    Int32.toInt ((_import "Parallel_numberOfProcessors" runtime private: unit -> Int32.int;) ())

val getProcessorNumber = _import "Parallel_processorNumber" runtime private: unit -> Int32.int;

  val processorNumber = Int32.toInt o getProcessorNumber
end
