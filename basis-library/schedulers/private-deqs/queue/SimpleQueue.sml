structure SimpleQueue =
struct

  type 'a t = 'a list ref

  fun new () = ref []

  fun empty (ref xs) = List.null xs

  fun pushBot (x, q) = q := (x :: !q)

  fun popBotDiscard (q as ref xs) =
    case xs of
      [] => false
    | _ :: xs' => (q := xs'; true)

  fun removeLast [] = raise Fail "SimpleQueue removeLast error"
    | removeLast [x] = ([], x)
    | removeLast (x :: xs) =
        let val (xs', last) = removeLast xs
        in (x :: xs', last)
        end

  fun popTop (q as ref xs) =
    if List.null xs then NONE
    else let val (xs', last) = removeLast xs
         in (q := xs'; SOME last)
         end

end
