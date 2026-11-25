structure TabFilterTree =
struct

  structure A = Array
  structure AS = ArraySlice

  structure ChunkList =
  struct
    val chunkSize = 256
    type 'a t = 'a array list * 'a array * int
    fun new () = ([], ForkJoin.alloc chunkSize, 0)

    fun push ((elems, chunk, pos): 'a t) (x: 'a) =
      if pos >= chunkSize then
        push (chunk :: elems, ForkJoin.alloc chunkSize, 0) x
      else
        ( A.update (chunk, pos, x); (elems, chunk, pos+1) )

    fun finish ((elems, chunk, pos): 'a t) =
      (List.rev elems, chunk, pos)

    fun foreach offset (elems, lastChunk, lastLen) f =
      case elems of
        [] => AS.appi (fn (i, x) => f (offset+i, x)) (AS.slice (lastChunk, 0, SOME lastLen))
      | (chunk' :: elems') =>
          ( A.appi (fn (i, x) => f (offset+i, x)) chunk'
          ; foreach (offset+chunkSize) (elems', lastChunk, lastLen) f
          )
  end

  datatype 'a tree =
    Leaf of int * 'a ChunkList.t
  | Node of int * 'a tree * 'a tree

  fun size (Leaf (n, _)) = n
    | size (Node (n, _, _)) = n

  fun tabFilter grain (lo, hi) (f: int -> 'a option) =
    let
      fun filterSeq (count, elems) (i, j) =
        if i >= j then
          (count, ChunkList.finish elems)
        else
          case f i of
            NONE => filterSeq (count, elems) (i, j)
          | SOME x => filterSeq (count+1, ChunkList.push elems x) (i+1, j)

      fun t i j =
        if j-i <= grain then
          Leaf (filterSeq (0, ChunkList.new ()) (i, j))
        else
          let
            val mid = i + (j-i) div 2
            val (l, r) = ForkJoin.par (fn _ => t i mid, fn _ => t mid j)
          in
            Node (size l + size r, l, r)
          end
    in
      t lo hi
    end

  fun foreach (t: 'a tree) (f: (int * 'a) -> unit) =
    let
      fun doit offset t =
        case t of
          Leaf (_, elems) => ChunkList.foreach offset elems f
        | Node (_, l, r) =>
            ( ForkJoin.par (fn _ => doit offset l,
                            fn _ => doit (offset + size l) r)
            ; ()
            )
    in
      doit 0 t
    end

end
