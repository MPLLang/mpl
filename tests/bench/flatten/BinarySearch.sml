structure BinarySearch:
sig
  val numLt: int Seq.t -> int -> int
  val numLeq: int Seq.t -> int -> int

  (** Compute both of the above.
    * Should be slightly faster than two individual calls.
    *)
  val numLtAndLeq: int Seq.t -> int -> (int * int)
end =
struct

  fun lowSearch x xs (lo, hi) =
    case hi - lo of
      0 => lo
    | n => let val mid = lo + n div 2
           in case Int.compare (x, Seq.nth xs mid) of
                 LESS => lowSearch x xs (lo, mid)
               | GREATER => lowSearch x xs (mid + 1, hi)
               | EQUAL => lowSearchEq x xs (lo, mid)
           end

  and lowSearchEq x xs (lo, mid) =
    if (mid = 0) orelse (x > Seq.nth xs (mid-1))
    then mid
    else lowSearch x xs (lo, mid)

  and highSearch x xs (lo, hi) =
    case hi - lo of
      0 => lo
    | n => let val mid = lo + n div 2
           in case Int.compare (x, Seq.nth xs mid) of
                 LESS => highSearch x xs (lo, mid)
               | GREATER => highSearch x xs (mid + 1, hi)
               | EQUAL => highSearchEq x xs (mid, hi)
           end

  and highSearchEq x xs (mid, hi) =
    if (mid = Seq.length xs - 1) orelse (x < Seq.nth xs (mid + 1))
    then mid + 1
    else highSearch x xs (mid + 1, hi)

  and search (x : int) (xs : int Seq.t) (lo, hi) : int * int =
    case hi - lo of
      0 => (lo, lo)
    | n => let val mid = lo + n div 2
           in case Int.compare (x, Seq.nth xs mid) of
                LESS    => search x xs (lo, mid)
              | GREATER => search x xs (mid + 1, hi)
              | EQUAL   => (lowSearchEq x xs (lo, mid), highSearchEq x xs (mid, hi))
           end

  fun numLtAndLeq s x = search x s (0, Seq.length s)


  fun numLeq (s: int Seq.t) (x: int) =
    let
      (* val _ =
        print ("numLeq (" ^ Seq.toString Int.toString s ^ ") " ^ Int.toString x ^ "\n") *)
      fun loop lo hi =
        case hi - lo of
          0 => lo
        | 1 => (if x < Seq.nth s lo then lo else hi)
        | n =>
            let
              val mid = lo + n div 2
            in
              if x < Seq.nth s mid then
                loop lo mid
              else
                loop (mid+1) hi
            end
    in
      loop 0 (Seq.length s)
    end

  fun numLt (s: int Seq.t) (x: int) =
    let
      fun loop lo hi =
        case hi - lo of
          0 => lo
        | 1 => (if x <= Seq.nth s lo then lo else hi)
        | n =>
            let
              val mid = lo + n div 2
            in
              if x <= Seq.nth s mid then
                loop lo mid
              else
                loop (mid+1) hi
            end
    in
      loop 0 (Seq.length s)
    end

end
