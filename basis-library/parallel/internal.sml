structure MLtonParallelInternal =
struct

   val numberOfProcessors = Int32.toInt Primitive.MLton.Parallel.numberOfProcessors
   val processorNumber = Int32.toInt o Primitive.MLton.Parallel.processorNumber

end
