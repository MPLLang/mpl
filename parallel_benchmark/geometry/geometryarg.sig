signature VECTOR = sig
  type t
  val + : t * t -> t
  val - : t * t -> t
  val == : t * t -> bool
  val < : t * t -> bool
  val <= : t * t -> bool
  (* products *)
  val scale : real * t -> t
  val point : t * t -> t
  val dot : t * t -> real
  val cross : t * (t * t) -> real

  (* return a point which is less/greater than in all dimensions *)
  val minExt : t * t -> t
  val maxExt : t * t -> t

  val zero : t
  val abs : t -> t
  val cube : real -> t
  val negInf : t
  val posInf : t
end

signature GEOMETRYARG =
sig

  type t

  exception Geometry

  type point = real * real

  structure Vector : VECTOR where type t = point

  val empty : t
  val singleton : point -> t
           (* maxSeq *)
  val append : int -> t * t -> t
             (* maxSeq *)
  val fromList : int -> point list -> t

  val size : t -> int
  val sub : t * int -> point

                    (* par merge *)  (* seq to par *) (* seq merge *)
  val fold : int -> ('b * 'b -> 'b) -> ('a -> 'b) -> (point * 'a -> 'a) -> 'a -> t -> 'b
  val fork : (unit -> 'a) * (unit -> 'b) -> ('a * 'b)

  (* debugging *)
  val ptos : point -> string
  val printArg : t -> unit

end
