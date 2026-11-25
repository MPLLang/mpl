structure TreeSeq =
struct
  datatype 'a t =
    Leaf
  | Elem of 'a
  | Flat of 'a Seq.t
  | Node of {num_elems: int, num_blocks: int, left: 'a t, right: 'a t}

  type 'a seq = 'a t

  fun length Leaf = 0
    | length (Elem _) = 1
    | length (Flat s) = Seq.length s
    | length (Node {num_elems = n, ...}) = n

  fun num_blocks Leaf = 0
    | num_blocks (Elem _) = 1
    | num_blocks (Flat _) = 1
    | num_blocks (Node {num_blocks = nb, ...}) = nb


  fun append (t1, t2) =
    Node
      { num_elems = length t1 + length t2
      , num_blocks = num_blocks t1 + num_blocks t2
      , left = t1
      , right = t2
      }


  fun to_blocks (t: 'a t) : 'a Seq.t Seq.t =
    let
      val blocks = ForkJoin.alloc (num_blocks t)

      fun putBlocks offset t =
        case t of
          Leaf => ()
        | Elem x => Array.update (blocks, offset, Seq.singleton x)
        | Flat s => Array.update (blocks, offset, s)
        | Node {num_blocks = nb, left = l, right = r, ...} =>
            let
              fun left () = putBlocks offset l
              fun right () =
                putBlocks (offset + num_blocks l) r
            in
              if nb <= 1000 then (left (); right ())
              else (ForkJoin.par (left, right); ())
            end
    in
      putBlocks 0 t;
      ArraySlice.full blocks
    end


  fun to_array_seq t =
    let
      val a = ForkJoin.alloc (length t)
      fun put offset t =
        case t of
          Leaf => ()
        | Elem x => Array.update (a, offset, x)
        | Flat s => Seq.foreach s (fn (i, x) => Array.update (a, offset + i, x))
        | Node {num_elems = n, left = l, right = r, ...} =>
            let
              fun left () = put offset l
              fun right () =
                put (offset + length l) r
            in
              if n <= 4096 then (left (); right ())
              else (ForkJoin.par (left, right); ())
            end
    in
      put 0 t;
      ArraySlice.full a
    end

  fun from_array_seq a = Flat a

  fun empty () = Leaf
  fun singleton x = Elem x
  val $ = singleton

end
