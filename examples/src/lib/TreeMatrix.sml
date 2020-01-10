structure TreeMatrix:
sig
  (* square matrices of sidelength 2^n matrices only! *)
  datatype matrix =
    Node of int * matrix * matrix * matrix * matrix
  | Leaf of int * real Array.array

  val tabulate: int -> (int * int -> real) -> matrix
  val flatten: matrix -> real array
  val sidelength: matrix -> int
  val multiply: matrix * matrix -> matrix
end =
struct

  val par = ForkJoin.par

  fun par4 (a, b, c, d) =
    let
      val ((ar, br), (cr, dr)) = par (fn _ => par (a, b), fn _ => par (c, d))
    in
      (ar, br, cr, dr)
    end

  datatype matrix =
    Node of int * matrix * matrix * matrix * matrix
  | Leaf of int * real Array.array

  exception MatrixFormat

  fun sidelength mat =
    case mat of
      Leaf (n, s) => n
    | Node (n, _, _, _, _) => n

  fun tabulate sidelen f =
    let
      fun tab n (row, col) =
        if n <= 64 then
          Leaf (n, Array.tabulate (n * n, fn i => f (row + i div n, col + i mod n)))
        else
          let
            val half = n div 2
            val (m11, m12, m21, m22) =
              par4 ( fn _ => tab half (row, col)
                   , fn _ => tab half (row, col + half)
                   , fn _ => tab half (row + half, col)
                   , fn _ => tab half (row + half, col + half)
                   )
          in
            Node (n, m11, m12, m21, m22)
          end
    in
      tab sidelen (0, 0)
    end

  val upd = Array.update

  fun writeFlatten (result, start, rowskip) m =
    case m of
      Leaf (n, s) =>
        let fun idx i = start + (i div n)*rowskip + (i mod n)
        in Array.appi (fn (i, x) => upd (result, idx i, x)) s
        end
    | Node (n, m11, m12, m21, m22) =>
        ( par4 ( fn _ => writeFlatten (result, start, rowskip) m11
               , fn _ => writeFlatten (result, start + n div 2, rowskip) m12
               , fn _ => writeFlatten (result, start + (n div 2) * rowskip, rowskip) m21
               , fn _ => writeFlatten (result, start + (n div 2) * (rowskip + 1), rowskip) m22
               )
        ; ()
        )

  fun flatten m =
    let
      val n = sidelength m
      val result = ForkJoin.alloc (n * n)
    in
      writeFlatten (result, 0, n) m;
      result
    end

  fun flatmultiply n (s, t, output) =
    let
      val sub = Array.sub
      val a = s
      val b = t
      val aStart = 0
      val bStart = 0
      (* assume our lengths are good *)
      (* loop with accumulator to compute dot product. r is an index into
       * vector a (the row index) and c is an index into b (the col index) *)
      fun loop rowStop acc r c =
        if r = rowStop then acc
        else let val acc' = acc + (sub (a, r) * sub (b, c))
                 val r' = r + 1
                 val c' = c + n
             in loop rowStop acc' r' c'
             end
      fun cell c =
        let
          val (i, j) = (c div n, c mod n)
          val rowStart = aStart + i*n
          val rowStop = rowStart + n
          val colStart = bStart + j
        in
          loop rowStop 0.0 rowStart colStart
        end
      fun update i =
        let
          val newv = cell i
          val old = sub (output, i)
        in
          Array.update (output, i, newv + old)
        end
       fun loopi i hi =
         if i >= hi then () else (update i; loopi (i + 1) hi)
    in
      loopi 0 (n * n)
    end

  fun multiply' (a, b, c) =
    case (a, b, c) of
      (Leaf (n, s), Leaf (_, t), Leaf (_, c)) => flatmultiply n (s, t, c)
    | (Node (n, a11, a12, a21, a22),
       Node (_, b11, b12, b21, b22),
       Node (_, c11, c12, c21, c22)) =>
        let
          fun block (m1, m2, m3, m4, c) =
            (multiply' (m1, m2, c); multiply' (m3, m4, c))
        in
          par4 ( fn _ => block (a11, b11, a12, b21, c11)
               , fn _ => block (a11, b12, a12, b22, c12)
               , fn _ => block (a21, b11, a22, b21, c21)
               , fn _ => block (a21, b12, a22, b22, c22)
               );
          ()
        end
    | _ => raise MatrixFormat

  fun multiply (a, b) =
    let
      val c = tabulate (sidelength a) (fn _ => 0.0)
    in
      multiply' (a, b, c);
      c
    end

end
