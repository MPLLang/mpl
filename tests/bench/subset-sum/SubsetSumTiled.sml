structure SubsetSumTiled:
sig
  val subset_sum: {unsafe_skip_table_set: bool}
                  -> int Seq.t * int
                  -> int Seq.t option
end =
struct

  structure Table:
  sig
    type t
    val new: {unsafe_skip_table_set: bool} -> int * int -> t
    val set: t -> int * int -> bool -> unit
    val get: t -> int * int -> bool
  end =
  struct
    datatype t = T of {num_rows: int, num_cols: int, data: Word8.word array}

    fun new {unsafe_skip_table_set} (num_rows, num_cols) =
      let
        val data = ForkJoin.alloc (num_rows * num_cols)
      in
        if unsafe_skip_table_set then
          ()
        else
          ForkJoin.parfor 1000 (0, num_rows * num_cols) (fn i =>
            Array.update (data, i, 0w0 : Word8.word));
        T {num_rows = num_rows, num_cols = num_cols, data = data}
      end

    fun set (T {num_rows, num_cols, data}) (r, c) b =
      Array.update (data, r * num_cols + c, if b then 0w1 else 0w0)

    fun get (T {num_rows, num_cols, data}) (r, c) =
      Array.sub (data, r * num_cols + c) = 0w1
  end


  fun subset_sum {unsafe_skip_table_set} (bag: int Seq.t, goal: int) :
    int Seq.t option =
    let
      val n = Seq.length bag

      val table =
        Table.new {unsafe_skip_table_set = unsafe_skip_table_set}
          (1 + n, 1 + goal)
      fun get (r, c) = Table.get table (r, c)
      fun set (r, c) b =
        Table.set table (r, c) b

      fun do_node (i, j) =
        if j = 0 then set (i, j) true
        else if i >= n then set (i, j) false
        else if Seq.nth bag i > j then set (i, j) (get (i + 1, j))
        else set (i, j) (get (i + 1, j) orelse get (i + 1, j - Seq.nth bag i))

      fun do_tile (i_lo, i_hi, j_lo, j_hi) =
        let
          val i_sz = i_hi - i_lo
          val j_sz = j_hi - j_lo
        in
          if i_sz * j_sz <= 1000 then
            Util.forBackwards (i_lo, i_hi) (fn i =>
              Util.for (j_lo, j_hi) (fn j => do_node (i, j)))
          else if i_sz = 1 then
            ForkJoin.parfor 1000 (j_lo, j_hi) (fn j => do_node (i_lo, j))
          else if j_sz = 1 then
            (* no parallelism is possible within a single column *)
            Util.forBackwards (i_lo, i_hi) (fn i => do_node (i, j_lo))
          else
            let
              val i_mid = i_lo + i_sz div 2
              val j_mid = j_lo + j_sz div 2
            in
              do_tile (i_mid, i_hi, j_lo, j_mid);
              ForkJoin.par
                ( fn () => do_tile (i_lo, i_mid, j_lo, j_mid)
                , fn () => do_tile (i_mid, i_hi, j_mid, j_hi)
                );
              do_tile (i_lo, i_mid, j_mid, j_hi)
            end
        end

      fun reconstruct_path acc (i, j) =
        if j = 0 then
          Seq.fromRevList acc
        else
          let
            val x = Seq.nth bag i
          in
            if get (i + 1, j) then reconstruct_path acc (i + 1, j)
            else reconstruct_path (x :: acc) (i + 1, j - x)
          end
    in
      do_tile (0, n + 1, 0, goal + 1);

      if get (0, goal) then SOME (reconstruct_path [] (0, goal)) else NONE
    end

end
