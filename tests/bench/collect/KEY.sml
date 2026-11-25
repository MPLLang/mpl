signature KEY =
sig
  type t
  val equal: t * t -> bool
  val cmp: t * t -> order
  val empty: t
  val hash: t -> int
end
