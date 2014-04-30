signature VECTOR = sig
  type t
  type m
  val + : t * t -> t
  val - : t * t -> t
  val == : t * t -> bool
  val < : t * t -> bool
  val <= : t * t -> bool
  (* products *)
  val scale : real * t -> t
  val point : t * t -> t
  val outer : t * t -> m
  val dot : t * t -> real

  (* return a point which is less/greater than in all dimensions *)
  val minExt : t * t -> t
  val maxExt : t * t -> t

  val zero : t
  val abs : t -> t
  val cube : real -> t
  val negInf : t
  val posInf : t
end

signature MATRIX = sig
  type t
  type v
  val + : t * t -> t
  val - : t * t -> t
  (* products *)
  val scale : real * t -> t
  val vec : t * v -> v

  val zero : t
  val I : t
end

signature SIMARG =
sig
  type v = real * real * real
  type m = v * v * v

  structure Vector : VECTOR where type t = v
                        and type m = m  
  structure Matrix : MATRIX where type t = m
                        and type v = v

  exception Sim of string

  val == : real * real -> bool
  val max3 : real * real * real -> real

  type box =       { position : Vector.t, dimension : Vector.t }
  type pointmass = { mass : real, position : Vector.t }
  type particle =  { position : Vector.t, velocity : Vector.t, mass : real }

  (* how good is this simulation *)
  val theta : real

  (* how close are two points before we give up *)
  val epsilon : v

  val fold : int -> ('b * 'b -> 'b) -> (int -> 'b) -> 'b -> int -> 'b

  val particleToString : particle -> string

end
