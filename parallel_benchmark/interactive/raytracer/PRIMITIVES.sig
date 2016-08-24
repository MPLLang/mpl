signature PRIMITIVES =
sig
  val par  : (unit -> 'a) * (unit -> 'b) -> ('a * 'b)
  val par3 : (unit -> 'a) * (unit -> 'b) * (unit -> 'c) -> ('a * 'b * 'c)
  val par4 : (unit -> 'a) * (unit -> 'b) * (unit -> 'c) * (unit -> 'd) -> ('a * 'b * 'c * 'd)
  val parTab : int * (int -> 'a) -> (int -> 'a)
end
