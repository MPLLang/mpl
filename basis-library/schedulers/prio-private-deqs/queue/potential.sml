structure Potential :>
          sig
              type potential
              type t = potential

              val zero : t

              val p: t * t -> t
              val m: t * t -> t
              val l: t * int -> t

              val lt: t * t -> bool
              val le: t * t -> bool
              val gt: t * t -> bool
              val ge: t * t -> bool

              val fromDepth : int -> int -> t
          end =
struct

type potential = Word64.word
type t = potential

val zero = Word64.fromInt 0

val p = Word64.+
val m = Word64.-
fun l (p, i) = Word64.<< (p, Word.fromInt i)

val lt = Word64.<
val le = Word64.<=
val gt = Word64.>
val ge = Word64.>=

fun fromDepth (maxdepth: int) (d: int) =
    Word64.<< (0w1, Word.fromInt (maxdepth - d))

end
