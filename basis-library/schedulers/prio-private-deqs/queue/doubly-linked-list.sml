(* Author: Sam Westrick (swestric@cs.cmu.edu)
 * Note that "left" and "front" and "bottom" are all the synonymous here,
 * as are "right" and "back" and "top". *)

structure DoublyLinkedList =
struct

  datatype 'a node = Leaf | Node of 'a node ref * 'a * 'a node ref

  (* bottom, top *)
  type 'a t = 'a node ref * 'a node ref

  fun new () = (ref Leaf, ref Leaf)

  fun isEmpty (ref Leaf, _) = true
    | isEmpty _ = false

  fun prev (_, Node (l, _, _)) = l
    | prev ((_, top), Leaf) = top

  fun next (_, Node (_, _, r)) = r
    | next ((bot, _), Leaf) = bot

  fun pushFront (x, dll as (bot, _)) =
    let
      val n = !bot
      val nx = Node (ref Leaf, x, ref n)
    in
      ( bot := nx
      ; prev (dll, n) := nx
      )
    end

  fun popFront (dll as (bot, _)) =
    case !bot of
      Node (_, x, r) =>
          let val n = !r
          in ( prev (dll, n) := Leaf
             ; bot := n
             ; SOME x
             )
          end
    | Leaf => NONE

  fun popBack (dll as (_, top)) =
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

  fun foldl f b (bot, _) =
    let
      fun leftToRight b r =
        case !r of
          Node (_, x, r') => leftToRight (f (x, b)) r'
        | Leaf => b
    in
      leftToRight b bot 
    end

  fun foldr f b (_, top) =
    let
      fun rightToLeft b l =
        case !l of
          Node (l', x, _) => rightToLeft (f (x, b)) l'
        | Leaf => b
    in
      rightToLeft b top
    end

  fun length dll =
    foldl (fn (_, c) => c + 1) 0 dll

  fun insertAfter (dll, n) x =
    let
      val n' = !(next (dll, n))
      val nx = Node (ref n, x, ref n')
    in
      ( next (dll, n) := nx
      ; prev (dll, n') := nx
      ; nx
      )
    end

  fun insertBefore (dll, n) x =
    let
      val n' = !(prev (dll, n))
      val nx = Node (ref n', x, ref n)
    in
      ( next (dll, n') := nx
      ; prev (dll, n) := nx
      ; nx
      )
    end

  fun findl (p : 'a -> bool) (bot, _) =
    let
      fun leftToRight (ref n) =
        case n of
          Node (_, x, r) => if p x then n else leftToRight r
        | Leaf => n 
    in
      leftToRight bot
    end

  fun findr (p : 'a -> bool) (_, top) =
    let
      fun rightToLeft (ref n) =
        case n of
          Node (l, x, _) => if p x then n else rightToLeft l
        | Leaf => n
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
