signature SORT =
sig

  type t

  val sort : { cutoff : int, fallback : t -> t } -> t -> t

end
