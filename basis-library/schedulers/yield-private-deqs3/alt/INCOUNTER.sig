signature INCOUNTER =
sig
  type t (* incounter type *)
  type h (* handle type *)

  exception Decrement

  val new : unit -> t

  (* Returns a new counter, already at 2, with two corresponding handles *)
  val new2 : unit -> h * h

  (* `increment c` returns a unique handle into c. *)
  val increment : t -> h

  (* `decrement h` returns true if h's associated incounter is now 0.
   * It raises Decrement if h has been previously decremented. *)
  val decrement : h -> bool

  val poll : t -> int

end
