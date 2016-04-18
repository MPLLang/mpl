signature MLTON_PARALLEL_ATOMIC =
sig
    datatype memory_order = RELAXED
                          | CONSUME
                          | ACQUIRE
                          | RELEASE
                          | ACQ_REL
                          | SEQ_CST

    val load : int ref * memory_order -> int;
    val store : int ref * int * memory_order -> unit;
    val exchange : int ref * int * memory_order -> int
    val compare_exchange_strong : int ref * int ref * int * memory_order * memory_order -> bool;
    val compare_exchange_weak : int ref * int ref * int * memory_order * memory_order -> bool;
    val fetch_add : int ref * int * memory_order -> int
    val fetch_sub : int ref * int * memory_order -> int
    val fetch_or : int ref * int * memory_order -> int
    val fetch_xor : int ref * int * memory_order -> int
    val fetch_and : int ref * int * memory_order -> int
    val thread_fence : memory_order -> unit
end
