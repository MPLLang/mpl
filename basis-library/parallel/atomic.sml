structure MLtonParallelAtomic :> MLTON_PARALLEL_ATOMIC =
struct
  datatype memory_order = RELAXED
                        | CONSUME
                        | ACQUIRE
                        | RELEASE
                        | ACQ_REL
                        | SEQ_CST

  local
      fun int_of_memory_order RELAXED = 0
        | int_of_memory_order CONSUME = 1
        | int_of_memory_order ACQUIRE = 2
        | int_of_memory_order RELEASE = 3
        | int_of_memory_order ACQ_REL = 4
        | int_of_memory_order SEQ_CST = 5

      val UNSAFE_load_explicit = _import "Parallel_load" : int ref * int -> int;
      val UNSAFE_store_explicit = _import "Parallel_store" : int ref * int * int -> unit;
      val UNSAFE_exchange = _import "Parallel_exchange" : int ref * int * int -> int;
      val UNSAFE_compare_exchange_strong = _import "Parallel_compare_exchange_strong" : int ref * int ref * int * int * int -> bool;
      val UNSAFE_compare_exchange_weak = _import "Parallel_compare_exchange_weak" : int ref * int ref * int * int * int -> bool;
      val UNSAFE_fetch_add = _import "Parallel_fetch_add" : int ref * int * int -> int;
      val UNSAFE_fetch_sub = _import "Parallel_fetch_sub" : int ref * int * int -> int;
      val UNSAFE_fetch_or = _import "Parallel_fetch_or" : int ref * int * int -> int;
      val UNSAFE_fetch_xor = _import "Parallel_fetch_xor" : int ref * int * int -> int;
      val UNSAFE_fetch_and = _import "Parallel_fetch_and" : int ref * int * int -> int;
      val UNSAFE_thread_fence = _import "Parallel_thread_fence" : int -> unit;
  in
  fun load (xr, mo) =
    UNSAFE_load_explicit (xr, int_of_memory_order mo)
  fun store (xr, v, mo) =
    UNSAFE_store_explicit (xr, v, int_of_memory_order mo)
  fun exchange (xr, v, mo) =
    UNSAFE_exchange (xr, v, int_of_memory_order mo)
  fun compare_exchange_strong (xr, yr, v, mo_succ, mo_fail) =
    UNSAFE_compare_exchange_strong (xr, yr, v, int_of_memory_order mo_succ, int_of_memory_order mo_fail)
  fun compare_exchange_weak (xr, yr, v, mo_succ, mo_fail) =
    UNSAFE_compare_exchange_weak (xr, yr, v, int_of_memory_order mo_succ, int_of_memory_order mo_fail)
  fun fetch_add (xr, v, mo) =
    UNSAFE_fetch_add (xr, v, int_of_memory_order mo)
  fun fetch_sub (xr, v, mo) =
    UNSAFE_fetch_sub (xr, v, int_of_memory_order mo)
  fun fetch_or (xr, v, mo) =
    UNSAFE_fetch_or (xr, v, int_of_memory_order mo)
  fun fetch_xor (xr, v, mo) =
    UNSAFE_fetch_xor (xr, v, int_of_memory_order mo)
  fun fetch_and (xr, v, mo) =
    UNSAFE_fetch_and (xr, v, int_of_memory_order mo)
  fun thread_fence mo =
    UNSAFE_thread_fence (int_of_memory_order mo)
  end
end
