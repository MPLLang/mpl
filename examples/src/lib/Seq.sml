structure Seq =
struct
  structure A = Array
  structure AS = ArraySlice

  type 'a t = 'a ArraySlice.slice

  val gran = 10000

  fun length s = AS.length s

  fun subseq s (i, n) = AS.subslice (s, i, SOME n)

  fun nth s i = AS.sub (s, i)

  fun tabulate f n = AS.full (SeqBasis.tabulate gran (0, n) f)

  fun map f s = tabulate (fn i => f (nth s i)) (length s)

  fun scan f b s =
    let
      val n = AS.length s
      val r = SeqBasis.scan gran f b (0, n) (nth s)
    in
      (AS.slice (r, 0, SOME n), A.sub (r, n))
    end

  fun reduce f b s =
    SeqBasis.reduce 10000 f b (0, length s) (nth s)

  fun fromList xs = ArraySlice.full (Array.fromList xs)

end
