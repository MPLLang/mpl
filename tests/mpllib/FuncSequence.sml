structure FuncSequence:
sig
  type 'a t
  type 'a seq = 'a t

  val empty: unit -> 'a seq
  val length: 'a seq -> int
  val take: 'a seq -> int -> 'a seq
  val drop: 'a seq -> int -> 'a seq
  val nth: 'a seq -> int -> 'a
  val iterate: ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b
  val tabulate: (int -> 'a) -> int -> 'a seq
  val reduce: ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a
end =
struct
  (* (i, j, f) defines the sequence [ f(k) : i <= k < j ] *)
  type 'a t = int * int * (int -> 'a)
  type 'a seq = 'a t

  fun empty () = (0, 0, fn _ => raise Subscript)
  fun length (i, j, _) = j - i
  fun nth (i, j, f) k = f (i+k)
  fun take (i, j, f) k = (i, i+k, f)
  fun drop (i, j, f) k = (i+k, j, f)

  fun tabulate f n = (0, n, f)

  fun iterate f b s =
    if length s = 0 then b
    else iterate f (f (b, nth s 0)) (drop s 1)

  fun reduce f b s =
    case length s of
      0 => b
    | 1 => nth s 0
    | n => let
             val half = n div 2
             val (l, r) =
               ForkJoin.par (fn _ => reduce f b (take s half),
                             fn _ => reduce f b (drop s half))
           in
             f (l, r)
           end
end


