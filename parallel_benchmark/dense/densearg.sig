signature DENSEARG =
sig

  type 'a t

  val length : 'a t -> int
             (* maxSeq      f           def     n *)
  val tabulate : int -> (int -> 'a) -> 'a -> int -> 'a t
  val sub : 'a t * int -> 'a
              (* maxSeq       "+"            "inj"      "unit"    a *)
  val foldArray : int -> ('b * 'b -> 'b) -> ('a -> 'b) -> 'b -> 'a t -> 'b

              (* maxSeq       "+"            "sub"    "unit"  "length" *)
  val foldInt : int -> ('b * 'b -> 'b) -> (int -> 'b) -> 'b -> int -> 'b
  val slice : 'a t * int * int option -> 'a t
  val inParallel : (unit -> 'a) * (unit -> 'b) -> 'a * 'b
  (* may do a GC if we are measuring space use *)
  val GC : unit -> unit

end
