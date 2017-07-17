(* Author: Sam Westrick (swestric@cs.cmu.edu) *)
functor MkChunkedDoublyLinkedList (val chunkSize : int) :> QUEUE =
struct
  structure DLL = DoublyLinkedList

  type 'a chunk = 'a option array * int ref
  type 'a t = 'a chunk DLL.t

  fun new () = DLL.new ()

  fun empty t = DLL.empty t

  fun pushBotNewChunk (x, t) =
    let val data = Array.array (chunkSize, NONE)
    in ( Array.update (data, 0, SOME x)
       ; DLL.pushBot ((data, ref 1), t)
       )
    end

  fun pushBot (x, t) =
    case DLL.peekBot t of
      SOME (data, size as ref n) =>
        if n = chunkSize then pushBotNewChunk (x, t)
        else ( Array.update (data, n, SOME x)
             ; size := n + 1
             )
    | NONE => pushBotNewChunk (x, t)

  fun popBotDiscard t =
    case DLL.peekBot t of
      SOME (data, size as ref n) =>
        ( if n = 1 orelse not (Option.isSome (Array.sub (data, n-2)))
          then ignore (DLL.popBotDiscard t)
          else ( Array.update (data, n-1, NONE)
               ; size := n - 1
               )
        ; true
        )
    | NONE => false

  fun popTop t =
    case DLL.peekTop t of
      SOME (data, size as ref n) =>
        let val SOME (i, result) = Array.findi (Option.isSome o #2) data
        in ( if i = n-1 then ignore (DLL.popTop t)
             else Array.update (data, i, NONE)
           ; result
           )
        end
    | NONE => NONE

  fun peekBot t = raise Fail "not yet implemented"
  fun peekTop t = raise Fail "not yet implemented"

end
