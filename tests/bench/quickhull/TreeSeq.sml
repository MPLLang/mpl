structure TreeSeq =
struct
  datatype 'a t =
    Leaf
  | Elem of 'a
  | Flat of 'a Seq.t
  | Node of int * 'a t * 'a t

  type 'a seq = 'a t
  type 'a ord = 'a * 'a -> order
  datatype 'a listview = NIL | CONS of 'a * 'a seq
  datatype 'a treeview = EMPTY | ONE of 'a | PAIR of 'a seq * 'a seq

  exception Range
  exception Size
  exception NYI

  fun length Leaf = 0
    | length (Elem _) = 1
    | length (Flat s) = Seq.length s
    | length (Node (n, _, _)) = n

  fun append (t1, t2) = Node (length t1 + length t2, t1, t2)

  fun toArraySeq t =
    let
      val a = ForkJoin.alloc (length t)
      fun put offset t =
        case t of
          Leaf => ()
        | Elem x => Array.update (a, offset, x)
        | Flat s => Seq.foreach s (fn (i, x) => Array.update (a, offset+i, x))
        | Node (n, l, r) =>
            let
              fun left () = put offset l
              fun right () = put (offset + length l) r
            in
              if n <= 4096 then
                (left (); right ())
              else
                (ForkJoin.par (left, right); ())
            end
    in
      put 0 t;
      ArraySlice.full a
    end

  fun fromArraySeq a = Flat a

  fun empty () = Leaf
  fun singleton x = Elem x
  val $ = singleton

end
