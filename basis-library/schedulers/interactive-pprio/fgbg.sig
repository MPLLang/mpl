signature MLTON_PARALLEL_FGBG =
sig

  type 'a t

  val bg : (unit -> 'a) -> 'a t
  val highbg : (unit -> 'a) -> 'a t
  val fg : (unit -> 'a t) -> 'a

end
