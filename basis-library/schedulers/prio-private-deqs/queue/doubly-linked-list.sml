(* Author: Sam Westrick (swestric@cs.cmu.edu)
 * Note that "left" and "front" and "bottom" are all the synonymous here,
 * as are "right" and "back" and "top". *)

structure DoublyLinkedList =
struct

  datatype 'a node = Leaf | Node of 'a node ref * 'a * 'a node ref * bool ref

  (* bottom, top *)
  type 'a t = 'a node ref * 'a node ref

  fun new () = (ref Leaf, ref Leaf)

  fun isEmpty (ref Leaf, _) = true
    | isEmpty _ = false

  fun prev (_, Node (l, _, _, _)) = l
    | prev ((_, top), Leaf) = top

  fun next (_, Node (_, _, r, _)) = r
    | next ((bot, _), Leaf) = bot

  fun pushFront (x, dll as (bot, _)) =
    let
      val n = !bot
      val nx = Node (ref Leaf, x, ref n, ref true)
    in
      ( bot := nx
      ; prev (dll, n) := nx;
      nx
      )
    end

  fun popFront (dll as (bot, _)) =
    case !bot of
      Node (_, x, r, il) =>
          let val n = !r
          in ( prev (dll, n) := Leaf
             ; bot := n
             ; il := false
             ; SOME x
             )
          end
    | Leaf => NONE

  fun popBack (dll as (_, top)) =
    case !top of
      Node (l, x, _, il) =>
          let val n = !l
          in ( next (dll, n) := Leaf
             ; top := n
             ; il := false
             ; SOME x
             )
          end
     | Leaf => NONE

  fun remove (dll, n as Node (_, _, _, il)) =
      let
          val ln = !(prev (dll, n))
          val rn = !(next (dll, n))
      in
          ( next (dll, ln) := rn
          ; prev (dll, rn) := ln
          ; il := false
          )
      end
    | remove (dll, Leaf) = raise (Fail "remove: Leaf")

  fun peekBot (bot, _) =
    case !bot of
      Node (_, x, _, _) => SOME x
    | Leaf => NONE

  fun peekTop (_, top) =
    case !top of
      Node (_, x, _, _) => SOME x
    | Leaf => NONE

  fun foldl f b (bot, _) =
    let
      fun leftToRight b r =
        case !r of
          Node (_, x, r', _) => leftToRight (f (x, b)) r'
        | Leaf => b
    in
      leftToRight b bot 
    end

  fun foldr f b (_, top) =
    let
      fun rightToLeft b l =
        case !l of
          Node (l', x, _, _) => rightToLeft (f (x, b)) l'
        | Leaf => b
    in
      rightToLeft b top
    end

  fun length dll =
    foldl (fn (_, c) => c + 1) 0 dll

  fun insertAfter (dll, n) x =
    let
      val n' = !(next (dll, n))
      val nx = Node (ref n, x, ref n', ref true)
    in
      ( next (dll, n) := nx
      ; prev (dll, n') := nx
      ; nx
      )
    end

  fun insertBefore (dll, n) x =
    let
      val n' = !(prev (dll, n))
      val nx = Node (ref n', x, ref n, ref true)
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
          Node (_, x, r, _) => if p x then n else leftToRight r
        | Leaf => n
    in
      leftToRight bot
    end

  fun findr (p : 'a -> bool) (_, top) =
    let
      fun rightToLeft (ref n) =
        case n of
          Node (l, x, _, _) => if p x then n else rightToLeft l
        | Leaf => n
    in
      rightToLeft top
    end

  fun isInList (Node (_, _, _, il)) = !il
    | isInList (Leaf) = false

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
