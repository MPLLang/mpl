functor MkParens (Seq: SEQUENCE) :
sig
  datatype paren = Left | Right
  val parenMatch: (paren ArraySequence.t) -> bool
end =
struct

  datatype paren = Left | Right

  fun combine ((l1,r1),(l2,r2)) =
    if r1 > l2 then
      (l1,r1+r2-l2)
    else
      (l1+l2-r1, r2)

  val id = (0, 0)

  fun singleton v =
    case v of
      Left => (0,1)
    | Right => (1,0)

  fun parenMatch s =
    let
      val (l, r) =
        Seq.reduce combine id (Seq.map singleton (Seq.fromArraySeq s))
    in
      l = 0 andalso r = 0
    end

end
