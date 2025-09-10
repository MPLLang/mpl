structure VerticalSeamIndexMap :>
sig
  type t
  type seam = int Seq.t

  (* `new (height, width)` *)
  val new: (int * int) -> t

  (* Remaps from some (H, W) into (H', W'). For vertical seams, we always
   * have H = H'. This signature could be reused for horizontal seams, though.
   *)
  val domain: t -> (int * int)
  val range: t -> (int * int)

  (* Remap an index (i, j) to some (i', j'), where i is a row index and j
   * is a column index. With vertical seams, i = i'.
   *)
  val remap: t -> (int * int) -> (int * int)

  (* Remove the given seam.
   * Causes all (i, j) on the right of the seam to be remapped to (i, j+1).
   *)
  val carve: t -> seam -> unit
end =
struct

  structure AS = ArraySlice

  type t = {displacement: int Seq.t, domain: (int * int) ref, range: int * int}
  type seam = int Seq.t

  fun new (height, width) =
    { displacement = AS.full (SeqBasis.tabulate 4000 (0, width*height) (fn _ => 0))
    , domain = ref (height, width)
    , range = (height, width)
    }

  fun domain ({domain = ref d, ...}: t) = d
  fun range ({range=r, ...}: t) = r

  fun remap ({displacement, range=(h, w), ...}: t) (i, j) =
    (i, j + Seq.nth displacement (i*w + j))

  (* fun carve ({displacement, domain=(h, w), range=r}: t) seam =
    let
    in
      { domain = (h, w-1)
      , range = r
      , displacement = displacement
      }
    end *)

  fun carve ({displacement, domain=(d as ref (h, w)), range=(_, w0)}: t) seam =
    ( d := (h, w-1)
    ; ForkJoin.parfor 1 (0, h) (fn i =>
        let
          val s = Seq.nth seam i
        in
          Util.for (s+1, w) (fn j =>
            ArraySlice.update (displacement, i*w0 + j - 1,
              1 + Seq.nth displacement (i*w0 + j)))
        end)
    )

    (* { domain = (h, w-1)
    , range = (h, w0)
    , displacement = AS.full (SeqBasis.tabulate 1000 (0, w0*h) (fn k =>
        let
          val i = k div w0
          val j = k mod w0
          val s = Seq.nth seam i
        in
          if j < s then
            Seq.nth displacement (i*w0 + j)
          else
            1 + Seq.nth displacement (i*w0 + j)
        end))
    } *)

end
