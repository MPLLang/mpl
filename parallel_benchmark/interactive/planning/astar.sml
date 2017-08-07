structure AStar =
struct

structure V = Vector

structure NodeInfo =
struct
type t = int * real (* Node id, f-score *)
fun compare ((_, f1), (_, f2)) = Real.< (f1, f2)
end

structure NodeKey =
struct
type ord_key = int
val compare = Int.compare
end

structure NJHeapArg =
struct
type priority = real
val compare = Real.compare
end

structure PQ = (* PairingHeap(structure E = NodeInfo)*)
struct

structure H = HeapFn(NJHeapArg)
open H
type elt = int
type pqueue = int H.heap
type token = H.hand

exception EmptyHeap

val empty = H.empty ()
fun is_empty q = (H.size q) = 0

fun find_min q = raise EmptyHeap

fun merge q1 q2 = raise EmptyHeap

fun insert q (n, p) = (q, H.insert q p n)

fun delete_min q =
    case H.min q of
        SOME (p, n) => (q, (n, p))
      | NONE => raise EmptyHeap

fun decrease_key q t (n, p) =
    (H.adjust q t p;
     q)
end

structure M = SplayMapFn(NodeKey)

fun lookup_def d (m, k) =
    case M.find (m, k) of
        NONE => d
      | SOME v => v

fun plan (g: graph)
         (start: int)
         (goal: int)
         (h: int -> real)
    : int list option =
    let fun proc_node visited n ((w, n'), (q, paths, gscores, tokens)) =
            case M.find (visited, n') of
                SOME _ => (* Already visited *)
                (q, paths, gscores, tokens)
              | NONE =>
                let val gs = M.lookup (gscores, n)
                    val gs' = lookup_def Real.posInf (gscores, n')
                    val ngs' = gs + w
                in
                    if ngs' >= gs' then
                        (* Not a better path *)
                        (q, paths, gscores, tokens)
                    else
                        let val p = lookup_def [] (paths, n)
                            val paths' = M.insert (paths, n', n::p)
                            val gscores' = M.insert (gscores, n', ngs')
                            val f' = ngs' + (h n')
                            val (q', tokens') =
                                case M.find (tokens, n') of
                                    NONE => (* not in q *)
                                    let val (q', t) = PQ.insert q (n', f')
                                    in
                                        (q', M.insert (tokens, n', t))
                                    end
                                  | SOME t =>
                                    (PQ.decrease_key q t (n', f'), tokens)
                        in
                            (q', paths', gscores', tokens')
                        end
                end

        fun iter q gscores paths visited tokens =
            if PQ.is_empty q then
                NONE
            else
                let val (q', (n, fs)) = PQ.delete_min q
                    val gs = M.lookup (gscores, n)
                    val visited' = M.insert (visited, n, ())
                in
                    if n = goal then
                        SOME (List.rev (goal::(M.lookup (paths, goal))))
                    else
                        let val nbrs = V.sub (g, n)
                            val _ = print ((Int.toString (V.length nbrs)) ^ "\n")
                            val (q'', paths', gscores', tokens') =
                                Vector.foldl
                                    (proc_node visited' n)
                                    (q', paths, gscores, tokens)
                                    nbrs
                        in
                            iter q'' gscores' paths' visited' tokens'
                        end
                end
        val (q, t) = PQ.insert PQ.empty (start, h start)
        val gscores = M.insert (M.empty, start, 0.0)
        val paths = M.insert (M.empty, start, [])
        val tokens = M.insert (M.empty, start, t)
    in
        iter q gscores paths M.empty tokens
    end

end
