(* Author: Sam Westrick (swestric@cs.cmu.edu)
 * Note that "left" and "front" and "bottom" are all the synonymous here,
 * as are "right" and "back" and "top". *)

structure DoublyLinkedList :> QUEUE =
struct

  datatype 'a node = Leaf | Node of 'a node ref * 'a * 'a node ref

  type 'a t = 'a node ref * 'a node ref

  fun new () = (ref Leaf, ref Leaf)

  fun empty (ref Leaf, _) = true
    | empty _ = false

  (* prev : 'a t * 'a node -> 'a node ref
   * prev (dll, n) is the pointer to n, from the left *)
  fun prev (_, Node (l, _, _)) = l
    | prev ((_, top), Leaf) = top

  (* next : 'a t * 'a node -> 'a node ref
   * next (dll, n) is the pointer to n, from the right *)
  fun next (_, Node (_, _, r)) = r
    | next ((bot, _), Leaf) = bot

  fun pushBot (x, dll as (bot, _)) =
    let
      val n = !bot
      val nx = Node (ref Leaf, x, ref n)
    in
      ( bot := nx
      ; prev (dll, n) := nx
      )
    end

  fun popBot (dll as (bot, _)) =
    case !bot of
      Node (_, x, r) =>
          let val n = !r
          in ( prev (dll, n) := Leaf
             ; bot := n
             ; SOME x
             )
          end
    | Leaf => NONE

  fun popBotDiscard (dll as (bot, _)) =
    case !bot of
      Node (_, _, r) =>
          let val n = !r
          in ( prev (dll, n) := Leaf
             ; bot := n
             ; true
             )
          end
    | Leaf => false

  fun popTop (dll as (_, top)) =
    case !top of
      Node (l, x, _) =>
          let val n = !l
          in ( next (dll, n) := Leaf
             ; top := n
             ; SOME x
             )
          end
    | Leaf => NONE

  fun peekBot (bot, _) =
    case !bot of
      Node (_, x, _) => SOME x
    | Leaf => NONE

  fun peekTop (_, top) =
    case !top of
      Node (_, x, _) => SOME x
    | Leaf => NONE

  fun toList (bot, _) =
    let
      fun leftToRight r =
        case !r of
          Leaf => []
        | Node (_, x, r') => x :: leftToRight r'
    in
      leftToRight bot
    end

  fun toRevList (_, top) =
    let
      fun rightToLeft l =
        case !l of
          Leaf => []
        | Node (l', x, _) => x :: rightToLeft l'
    in
      rightToLeft top
    end

end

(*
open DoublyLinkedList
val t = new () : int t
val _ = pushBot (5, t)
val _ = pushBot (4, t)
val _ = pushBot (3, t)
val ltr = toList t
val rtl = toRevList t
*)
