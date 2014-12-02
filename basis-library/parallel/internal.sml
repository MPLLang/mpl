structure MLtonParallelInternal =
struct
    val numberOfProcessors: Int32.int =
        Int32.toInt ((_import
                          "Parallel_numberOfProcessors"
                          runtime private: unit -> Int32.int;) ())
    val processorNumber: unit -> Int32.int =
        _import "Parallel_processorNumber" runtime private: unit -> Int32.int;
    val enterGlobalHeap: unit -> unit =
        _import "GC_enterGlobalHeap" runtime private: unit -> unit;
    val exitGlobalHeap: unit -> unit =
        _import "GC_exitGlobalHeap" runtime private: unit -> unit;

    fun evaluateInGlobalHeap (f: 'a -> 'b): 'a -> 'b =
        fn argument =>
           let
               val _ = enterGlobalHeap ()
               val result = f argument
               val _ = exitGlobalHeap ()
           in
               result
           end
end
