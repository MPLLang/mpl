signature LOOP_INDEX =
sig
  type idx
  type t = idx

  val fromInt: int -> idx
  val toInt: idx -> int

  val increment: idx -> idx
  val midpoint: idx * idx -> idx
  val equal: idx * idx -> bool
end


functor LoopIndexFromWord(WordImpl: WORD) :> LOOP_INDEX =
struct
  type idx = WordImpl.word
  type t = idx

  fun __inline_always__ toInt (w: idx) = __inline_always__ WordImpl.toIntX w
  fun __inline_always__ fromInt i = __inline_always__ WordImpl.fromInt i

  fun __inline_always__ midpoint (i: idx, j: idx) =
    let
      (* This way is broken! *)
      (* val mid = WordImpl.~>> (WordImpl.+ (i, j), 0w1) *)

      val range_size = WordImpl.+ (j, WordImpl.~ i)
      val mid = WordImpl.+ (i, WordImpl.div (range_size, WordImpl.fromInt 2))
    in
      (* If using a different midpoint calculation, consider uncommenting
       * the following for debugging/testing.
       *)

      (* if toInt i <= toInt mid andalso toInt mid <= toInt j then
        ()
      else
        ( print
            ( "ERROR: schedulers/spork/ForkJoin.sml: bug! midpoint failure: "
            ^ Int.toString (toInt i)
            ^ " "
            ^ Int.toString (toInt mid)
            ^ " "
            ^ Int.toString (toInt j)
            ^ "\n"
            )

        ; OS.Process.exit OS.Process.failure
        ); *)

      mid
    end

  fun __inline_always__ increment (i: idx) =
    WordImpl.+ (i, fromInt 1)

  fun __inline_always__ equal (i: idx, j: idx) = (i = j)
end
