structure SeqFlat =
struct

  structure AF = MPL.ArrayAos
  structure AFS = MPL.ArrayAosSlice

  type 'a seq = 'a AFS.slice
  type 'a t = 'a seq

  fun length (s : 'a seq) : int =
    AFS.length s

  fun nth s i =
    AFS.sub (s, i)

  fun tabulate f n : 'a seq =
    let
      val arr = ForkJoin.alloc_aos n
    in
      ForkJoin.parform (0, n) (fn i => AF.update (arr, i, f i));
      AFS.full arr
    end

end