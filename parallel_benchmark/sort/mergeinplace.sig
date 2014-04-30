signature MERGEINPLACE =
sig

  type t

  val merge : { cutoff : int, fallback : t * t -> t -> unit } -> t * t -> t -> unit

end
