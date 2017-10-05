functor ListQueue (Elem : sig
                            type t
                            val weight : t -> int
                          end)
  : QUEUE where type task = Elem.t =
struct

  type task = Elem.t
  type t = (int * task) list
  type task_set = t

  val empty : t = []

  fun fromSet s = s

  fun weight q =
    case q of
      [] => 0
    | (w, _) :: _ => w

  fun push q e = (Elem.weight e + weight q, e) :: q

  fun insert q e = raise Fail "ListQueue: insert not implemented"

  fun choose q =
    case q of
      [] => (NONE, empty)
    | (_, e) :: q' => (SOME e, q')

  fun split q =
    let
      fun split' es =
        case es of
          [] => ([], [])
        | (w, e) :: es' =>
            if 4 * w >= weight q
            then ([(w, e)], List.map (fn (a, x) => (a - w, x)) es')
            else let val (l, r) = split' es'
                 in (l, (w - weight l, e) :: r)
                 end
    in
      case q of
        ([] | [_]) => (NONE, q)
      | _ => let val (a, b) = split' (List.rev q)
             in (SOME (List.rev a), List.rev  b)
             end
    end

end

(* structure Q = ListQueue (struct type t = int fun weight x = x end)

fun fromList xs =
  case xs of
    [] => Q.empty
  | x :: xs' => Q.push (fromList xs') x

val q1 = fromList [1,3,8,15,15]
val (s1, q1') = Q.split q1

val q2 = fromList [1,1,100,100]
val (s2, q2') = Q.split q2 *)
