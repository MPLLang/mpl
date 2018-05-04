(* Author: Sam Westrick (swestric@cs.cmu.edu)
 * Note that "left" and "front" and "bottom" are all the synonymous here,
 * as are "right" and "back" and "top". *)

structure DoublyLinkedList =
struct

  datatype 'a node = Leaf | Node of 'a node ref * 'a * 'a node ref * bool ref
  
  type 'a hand = 'a node ref * 'a * 'a node ref * bool ref

  (* front, back *)
  type 'a t = 'a node ref * 'a node ref

  fun taskOfHand (_, t, _, _) = t

  fun new () = (ref Leaf, ref Leaf)

  fun isEmpty (ref Leaf, _) = true
    | isEmpty _ = false

  fun next (_, _, r, _) = case !r of Node h => SOME h | Leaf => NONE
  fun prev (l, _, _, _) = case !l of Node h => SOME h | Leaf => NONE

  fun last (_, back) =
    case !back of
      Leaf => NONE
    | Node h => SOME h

  fun inspect (_, x, _, _) = x

  fun prevp (_, Node (l, _, _, _)) = l
    | prevp ((_, top), Leaf) = top

  fun nextp (_, Node (_, _, r, _)) = r
    | nextp ((bot, _), Leaf) = bot

  fun pushFront (x, dll as (bot, _)) =
    let
      val n = !bot
      val hx = (ref Leaf, x, ref n, ref true)
      val nx = Node hx
    in
      ( bot := nx
      ; prevp (dll, n) := nx
      ; hx
      )
    end

  fun pushBack (x, dll as (_, back)) =
    let
      val n = !back
      val hx = (ref n, x, ref Leaf, ref true)
      val nx = Node hx
    in
      ( back := nx
      ; nextp (dll, n) := nx
      ; hx
      )
    end

  fun popFront (dll as (bot, _)) =
    case !bot of
      Node (_, x, r, il) =>
          let val n = !r
          in ( prevp (dll, n) := Leaf
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
          in ( nextp (dll, n) := Leaf
             ; top := n
             ; il := false
             ; SOME x
             )
          end
    | Leaf => NONE

  fun remove (dll, h as (_, _, _, il)) =
    let
      val n = Node h
      val ln = !(prevp (dll, n))
      val rn = !(nextp (dll, n))
    in
      ( nextp (dll, ln) := rn
      ; prevp (dll, rn) := ln
      ; il := false
      )
    end

  fun peekFront (front, _) =
    case !front of
      Node (_, x, _, _) => SOME x
    | Leaf => NONE

  fun peekBack (_, back) =
    case !back of
      Node (_, x, _, _) => SOME x
    | Leaf => NONE

  fun foldlh f b (front, _) =
    let
      fun leftToRight b r =
        case !r of
          Node (h as (_, x, r', _)) => leftToRight (f (x, h, b)) r'
        | Leaf => b
    in
      leftToRight b front 
    end

  fun foldrh f b (_, back) =
    let
      fun rightToLeft b l =
        case !l of
          Node (h as (l', x, _, _)) => rightToLeft (f (x, h, b)) l'
        | Leaf => b
    in
      rightToLeft b back
    end

  fun foldl f b q =
      foldlh (fn (x, _, r) => f (x, r)) b q
  fun foldr f b q =
      foldrh (fn (x, _, r) => f (x, r)) b q

  fun length dll =
    foldl (fn (_, c) => c + 1) 0 dll

  fun insertAfter (dll, h as (_, _, r, _)) x =
    let
      val n = Node h
      val n' = !r
      val hx = (ref n, x, ref n', ref true)
      val nx = Node hx
    in
      ( r := nx
      ; prevp (dll, n') := nx
      ; hx
      )
    end

  fun insertBefore (dll, h as (l, _, _, _)) x =
    let
      val n = Node h
      val n' = !l
      val hx = (ref n', x, ref n, ref true)
      val nx = Node hx
    in
      ( nextp (dll, n') := nx
      ; l := nx
      ; hx
      )
    end

  fun error msg =
    (print (msg ^ "\n"); OS.Process.exit OS.Process.failure)

  fun verify (dll as (front, back)) =
    let
      fun verify' r =
        case !r of
          Leaf => ()
        | Node (l, _, r', il) =>
            if nextp (dll, !l) <> r then
              raise Fail "DLL link mismatch"
            else if not (!il) then
              raise Fail "DLL not in list"
            else
              verify' r'
    in
      case (!front, !back) of
        (Leaf, Node _) => raise Fail "DLL front mismatch"
      | (Node _, Leaf) => raise Fail "DLL back mismatch"
      | (_, Node (_, _, ref (Node _), _)) => raise Fail "DLL last node mismatch"
      | _ => verify' front
    end

  fun splitAt ((front, back), h as (l, _, _, _)) =
    let
      val n = Node h
      val n' = !l
      val (front', back') = new ()
    in
      ( l := Leaf
      ; front' := n
      ; back' := !back
      ; back := n'
      ; case n' of
          Leaf => front := Leaf
        | Node (_, _, r, _) => r := Leaf
      (*; verify (front, back) handle Fail msg => error ("DLL split left error: " ^ msg)
      ; verify (front', back') handle Fail msg => error ("DLL split right error: " ^ msg)*)
      ; (front', back')
      )
    end

  fun findl (p : 'a -> bool) (front, _) =
    let
      fun leftToRight (ref n) =
        case n of
          Node (h as (_, x, r, _)) => if p x then SOME h else leftToRight r
        | Leaf => NONE
    in
      leftToRight front
    end

  fun findr (p : 'a -> bool) (_, back) =
    let
      fun rightToLeft (ref n) =
        case n of
          Node (h as (l, x, _, _)) => if p x then SOME h else rightToLeft l
        | Leaf => NONE
    in
      rightToLeft back
    end

  fun isInList (_, _, _, ref b) = b

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
