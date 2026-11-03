structure OffsetSearch:
sig
  (** `indexSearch (start, stop, offsetFn) k` returns which inner sequence
    * contains index `k`. The tuple arg defines a sequence of offsets.
    *)
  val indexSearch: int * int * (int -> int) -> int -> int
end =
struct

  fun indexSearch (start, stop, offset: int -> int) k =
    case stop-start of
      0 =>
        raise Fail "OffsetSearch.indexSearch: should not have hit 0"
    | 1 =>
        start
    | n =>
        let
          val mid = start + (n div 2)
        in
          if k < offset mid then
            indexSearch (start, mid, offset) k
          else
            indexSearch (mid, stop, offset) k
        end

end
