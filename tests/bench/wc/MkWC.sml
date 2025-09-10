functor MkWC (Seq : SEQUENCE) :
sig
  type 'a seq = 'a ArraySequence.t

  (* returns (num lines, num words, num characters) *)
  val wc: char seq -> (int * int * int)
end =
struct

  structure ASeq = ArraySequence
  type 'a seq = 'a ASeq.t

  fun wc seq =
    let
      (*
      val (a, i, n) = ArraySlice.base seq
      val _ = if i = 0 then () else raise Fail "uh oh"
      fun nth i = Array.sub (a, i)
       *)
      fun nth i = ASeq.nth seq i
      (* Create a delayed sequence of pairs of integers:
       * the first is 1 if it is line break, 0 otherwise;
       * the second is 1 if the start of a word, 0 otherwise.
       *)
      fun isSpace a = (a = #"\n" orelse a = #"\t" orelse a = #" ")
      (*val isSpace = Char.isSpace*)
      fun f i =
        let
          val si = nth i
          val wordStart =
            if (i = 0 orelse isSpace (nth (i-1))) andalso
               not (isSpace si)
            then 1 else 0
          val lineBreak = if si = #"\n" then 1 else 0
        in
          (lineBreak, wordStart)
        end
      val x = Seq.tabulate f (ASeq.length seq)
      val (lines, words) =
        Seq.reduce (fn ((lb1, ws1), (lb2, ws2)) => (lb1 + lb2, ws1 + ws2)) (0, 0) x
    in
      (lines, words, ASeq.length seq)
    end

end
