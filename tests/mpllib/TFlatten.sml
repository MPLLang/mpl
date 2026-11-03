structure TFlatten:
sig
  type 'a tree
  type 'a t = 'a tree

  datatype 'a view = Leaf of 'a Seq.t | Node of 'a t * 'a t

  val size: 'a t -> int
  val leaf: 'a Seq.t -> 'a t
  val node: 'a t * 'a t -> 'a t
  val view: 'a t -> 'a view
  val flatten: 'a t -> 'a Seq.t
end =
struct

  datatype 'a tree = Leaf_ of 'a Seq.t | Node_ of int * 'a tree * 'a tree
  type 'a t = 'a tree

  datatype 'a view = Leaf of 'a Seq.t | Node of 'a t * 'a t

  fun size (Leaf_ s) = Seq.length s
    | size (Node_ (n, _, _)) = n

  fun leaf s = Leaf_ s

  fun node (l, r) =
    Node_ (size l + size r, l, r)

  fun view (Leaf_ s) = Leaf s
    | view (Node_ (_, l, r)) = Node (l, r)


  fun flatten t =
    let
      val output = ForkJoin.alloc (size t)
      fun traverse (c, offset) =
        case c of
          Leaf_ s =>
            ForkJoin.parfor 100 (0, Seq.length s) (fn i =>
              Array.update (output, offset + i, Seq.nth s i))
        | Node_ (_, l, r) =>
            ( ForkJoin.par (fn () => traverse (l, offset), fn () =>
                traverse (r, offset + size l))
            ; ()
            )
    in
      traverse (t, 0);
      ArraySlice.full output
    end

end
