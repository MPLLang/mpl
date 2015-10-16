signature SORTARG =
sig

  type t
  exception Sort

  (* construct new collections *)
  val empty : t
  val singleton : real -> t
            (* maxSeq *)
  val filter : int -> (real -> bool) -> t -> t
  val slice : t * int * int option -> t
  (* split in to two (approximately) equal parts *)
  val halve: t -> t * t

  val length : t -> int

              (* maxSeq *)
  val fromList : int option -> real list -> t
  val toList : t -> real list
             (* idx   val *)
  val foldli : (int * real * 'a -> 'a) -> 'a -> t -> 'a

  (* return an element suitable as a pivot *)
  val pivot : t -> real
  val sub : t * int -> real

  (* these two for in-place only *)
  val update : t * int * real -> unit
  val copy : { src : t, dst : t, di : int } -> unit
  (* these for persistent only *)
  val concat : t list -> t
  val append : t * t -> t
             (* maxSeq *)  (* size *)  (* idx         val *)
  val unfoldi : int option -> int * 'a * (int * 'a -> real * 'a) -> t * 'a

  (* possibly compute these two thunks in parallel *)
  val fork : (unit -> 'a) * (unit -> 'b) -> ('a * 'b)

  (* type 'a future *)
  (* val future : (unit -> 'a) -> 'a future *)
  (* val touch : 'a future -> 'a *)

  (* debugging *)
  val printArg : t -> unit

  (* may do a GC if we are measuring space use *)
  val GC : unit -> unit

end
