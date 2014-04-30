signature SORTINPLACE =
sig

  type t

  val sort : { cutoff : int, fallback : (bool * t * t) -> unit } 
            (* toLeft   L   R *)
             -> (bool * t * t) -> unit

end
