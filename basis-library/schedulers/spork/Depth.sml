structure Depth :
sig
  eqtype t

  val get : MLton.Thread.Basic.t -> t
  val set : MLton.Thread.Basic.t * t -> unit

  (* depth level of a kernel thread (should never be reached) *)
  val kernel : t
  (* depth level of a scheduling thread *)
  val scheduler : t
  (* calculates the parent task's depth *)
  val parentOf : t -> t
  val childOf : t -> t

  val toString : t -> string
end=
struct
  type t = Word32.word

  val kernel = (0w0 : Word32.word)
  val scheduler = (0w1 : Word32.word)
  fun parentOf d = d - (0w1 : Word32.word)
  fun childOf d = d + (0w1 : Word32.word)

  structure HH = MLton.Thread.HierarchicalHeap

  val get = HH.getDepth
  val set = HH.setDepth

  val toString = Word32.toString
end
