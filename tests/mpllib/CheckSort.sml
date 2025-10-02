functor CheckSort
  (val sort_func: ('a * 'a -> order) -> 'a Seq.t -> 'a Seq.t):
sig
  datatype 'a error =
    LengthChange             (* output length differs from input *)
  | MissingElem of int       (* index of missing input element *)
  | Inversion of int * int   (* indices of two elements not in order in output *)
  | Unstable of int * int    (* indices of two equal swapped elements *)

  val check:
    { input: 'a Seq.t
    , compare: 'a * 'a -> order
    , check_stable: bool
    }
    -> 'a error option   (* NONE if correct *)
end =
struct

  structure DS = DelayedSeq

  type 'a seq = 'a Seq.t

  datatype 'a error =
    LengthChange             (* output length differs from input *)
  | MissingElem of int       (* index of missing input element *)
  | Inversion of int * int   (* indices of two elements not in order in output *)
  | Unstable of int * int    (* indices of two equal swapped elements *)

  type 'a check_input =
    { input: 'a seq
    , compare: 'a * 'a -> order
    , check_stable: bool
    }

  fun check ({input, compare, check_stable}: 'a check_input) =
    let
      val n = Seq.length input
      val input' = Seq.mapIdx (fn (i, x) => (i, x)) input
      fun compare' ((i1, k1), (i2, k2)) = compare (k1, k2)

      val result = sort_func compare' input'

      val noElemsMissing: bool =
        DS.reduce (fn (a, b) => a andalso b) true
          (DS.inject
            ( DS.tabulate (fn _ => false) n
            , DS.map (fn (i, k) => (i, true)) (DS.fromArraySeq result)
            ))

      fun adjacentPairProblem ((i1, k1), (i2, k2)) =
        case compare (k1, k2) of
          LESS => NONE
        | GREATER => SOME (Inversion (i1, i2))
        | EQUAL =>
            if check_stable andalso i1 > i2 then
              SOME (Unstable (i1, i2))
            else
              NONE

      fun problemAt i =
        adjacentPairProblem (Seq.nth result i, Seq.nth result (i+1))
      fun isProblem i = Option.isSome (problemAt i)
    in
      case FindFirst.findFirst 1000 (0, n-1) isProblem of
        NONE => NONE
      | SOME i => problemAt i
    end

end
