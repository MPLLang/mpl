signature FORK_JOIN =
   sig
      val par: (unit -> 'a) * (unit -> 'b) -> 'a * 'b
      val parfor: int -> int * int -> (int -> unit) -> unit
      val alloc: int -> 'a array
   end
