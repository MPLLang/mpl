functor UnrolledLoops(WordImpl: WORD) :>
sig
  val pareduce: (int * int) -> 'a -> (int * 'a -> 'a) -> ('a * 'a -> 'a) -> 'a
  val pareduceBreakExn: (int * int) -> 'a -> (('a -> exn) * int * 'a -> 'a) -> ('a * 'a -> 'a) -> 'a
  val reducem: ('a * 'a -> 'a) -> 'a -> (int * int) -> (int -> 'a) -> 'a
  val parform: (int * int) -> (int -> unit) -> unit
end =
struct

  type word = WordImpl.word
  fun __inline_always__ w2i w = __inline_always__ WordImpl.toIntX w
  fun __inline_always__ i2w i = __inline_always__ WordImpl.fromInt i

  val one = i2w 1
  val two = i2w 2
  val three = i2w 3
  val four = i2w 4
  val five = i2w 5
  val six = i2w 6
  val seven = i2w 7
  val eight = i2w 8

  fun __inline_always__ midpoint (i: word, j: word) =
    WordImpl.+ (i, WordImpl.>> (WordImpl.- (j, i), 0w1))


  fun __inline_always__ pareduce (lo, hi) (z: 'a) (step': int * 'a -> 'a) (g: 'a * 'a -> 'a) : 'a =
    let

      fun __inline_always__ step (a, i) =
        __inline_always__ step' (w2i i, a)
        (* __inline_always__ g (a, __inline_always__ f (w2i i)) *)


      (* fun sequential_loop (a, i: word, j: word) =
        if i < j then
          sequential_loop (step (a, i), i + 0w1, j)
        else a *)

      
      (* fun __inline_always__ next stride =
        Word64.min (Word64.<< (stride, 0w1), 0w16) *)


      fun loop8 (a, i, j) =
        if WordImpl.<= (WordImpl.+ (i, eight), j) then
          let
            fun __inline_never__ spwn a' =
              if WordImpl.>= (WordImpl.+ (i, eight), j) then a' else
                let
                  val mid = midpoint (WordImpl.+ (i, eight), j)
                in
                  Scheduler.SporkJoin.spork {
                    tokenPolicy = Scheduler.TokenPolicyFair,
                    body = fn () => loop1 (a', WordImpl.+ (i, eight), mid),
                    spwn = fn () => loop1 (z, mid, j),
                    seq  = fn a'' => loop1 (a'', mid, j),
                    sync = g,
                    unstolen = NONE
                  }
                end
          in
            Scheduler.SporkJoin.spork {
              tokenPolicy = Scheduler.TokenPolicyGive,
              body = fn () =>
                let
                  val a = step (a, i)
                  val a = step (a, WordImpl.+ (i, one))
                  val a = step (a, WordImpl.+ (i, two))
                  val a = step (a, WordImpl.+ (i, three))
                  val a = step (a, WordImpl.+ (i, four))
                  val a = step (a, WordImpl.+ (i, five))
                  val a = step (a, WordImpl.+ (i, six))
                  val a = step (a, WordImpl.+ (i, seven))
                in
                  a
                end,
              seq = fn a' => loop8 (a', WordImpl.+ (i, eight), j),
              sync = g,
              spwn = fn () => spwn z,
              unstolen = SOME spwn
            }
          end
        else
          loop1 (a, i, j)



      and loop4 (a, i, j) =
        if WordImpl.<= (WordImpl.+ (i, four), j) then
          let
            fun __inline_never__ spwn a' =
              if WordImpl.>= (WordImpl.+ (i, four), j) then a' else
                let
                  val mid = midpoint (WordImpl.+ (i, four), j)
                in
                  Scheduler.SporkJoin.spork {
                    tokenPolicy = Scheduler.TokenPolicyFair,
                    body = fn () => loop1 (a', WordImpl.+ (i, four), mid),
                    spwn = fn () => loop1 (z, mid, j),
                    seq  = fn a'' => loop1 (a'', mid, j),
                    sync = g,
                    unstolen = NONE
                  }
                end
          in
            Scheduler.SporkJoin.spork {
              tokenPolicy = Scheduler.TokenPolicyGive,
              body = fn () =>
                let
                  val a = step (a, i)
                  val a = step (a, WordImpl.+ (i, one))
                  val a = step (a, WordImpl.+ (i, two))
                  val a = step (a, WordImpl.+ (i, three))
                in
                  a
                end,
              seq = fn a' => loop8 (a', WordImpl.+ (i, four), j),
              sync = g,
              spwn = fn () => spwn z,
              unstolen = SOME spwn
            }
          end
        else
          loop1 (a, i, j)



      and loop2 (a, i, j) =
        if WordImpl.<= (WordImpl.+ (i, two), j) then
          let
            fun __inline_never__ spwn a' =
              if WordImpl.>= (WordImpl.+ (i, two), j) then a' else
                let
                  val mid = midpoint (WordImpl.+ (i, two), j)
                in
                  Scheduler.SporkJoin.spork {
                    tokenPolicy = Scheduler.TokenPolicyFair,
                    body = fn () => loop1 (a', WordImpl.+ (i, two), mid),
                    spwn = fn () => loop1 (z, mid, j),
                    seq  = fn a'' => loop1 (a'', mid, j),
                    sync = g,
                    unstolen = NONE
                  }
                end
          in
            Scheduler.SporkJoin.spork {
              tokenPolicy = Scheduler.TokenPolicyGive,
              body = fn () =>
                let
                  val a = step (a, i)
                  val a = step (a, WordImpl.+ (i, one))
                in
                  a
                end,
              seq = fn a' => loop4 (a', WordImpl.+ (i, two), j),
              sync = g,
              spwn = fn () => spwn z,
              unstolen = SOME spwn
            }
          end
        else
          loop1 (a, i, j)



      and loop1 (a, i, j) =
        if WordImpl.<= (WordImpl.+ (i, one), j) then
          let
            fun __inline_never__ spwn a' =
              if WordImpl.>= (WordImpl.+ (i, one), j) then a' else
                let
                  val mid = midpoint (WordImpl.+ (i, one), j)
                in
                  Scheduler.SporkJoin.spork {
                    tokenPolicy = Scheduler.TokenPolicyFair,
                    body = fn () => loop1 (a', WordImpl.+ (i, one), mid),
                    spwn = fn () => loop1 (z, mid, j),
                    seq  = fn a'' => loop1 (a'', mid, j),
                    sync = g,
                    unstolen = NONE
                  }
                end
          in
            Scheduler.SporkJoin.spork {
              tokenPolicy = Scheduler.TokenPolicyGive,
              body = fn () => step (a, i),
              seq = fn a' => loop2 (a', WordImpl.+ (i, one), j),
              sync = g,
              spwn = fn () => spwn z,
              unstolen = SOME spwn
            }
          end
        else
          a

    in
      __inline_always__
      loop1 (z, i2w lo, i2w hi)
    end


  fun __inline_always__ pareduceBreakExn (i: int, j: int) (z: 'a) (step: ('a -> exn) * int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let exception Break of 'a
          fun step' (i, a) = (__inline_always__ step (Break, i, a), true) handle (Break b) => (b, false)
          fun merge' ((b1, cont1), (b2, cont2)) =
              if cont1 then (merge (b1, b2), cont2) else (b1, false)

          (* we can reuse the pareduce structure but need to adapt it for break semantics *)
          (* for simplicity, we'll use a basic implementation that wraps pareduce *)
          (* a more optimized version would inline the break logic into the unrolled loops *)

          fun continue (f : 'a -> 'a * bool) : 'a * bool -> 'a * bool =
              fn (b, cont) => if cont then f b else (b, cont)

          fun iter (b: 'a) (i: word, j: word): 'a * bool =
              if i = j then (b, true) else
                let
                    fun __inline_never__ spwn b' =
                        if WordImpl.>= (WordImpl.+ (i, one), j) then (b', true) else
                          let val mid = midpoint (WordImpl.+ (i, one), j) in
                            Scheduler.SporkJoin.spork {
                              tokenPolicy = Scheduler.TokenPolicyFair,
                              body = fn () => iter b' (WordImpl.+ (i, one), mid),
                              spwn = fn () => iter z (mid, j),
                              seq = continue (fn b' => iter b' (mid, j)),
                              sync = merge',
                              unstolen = NONE
                          }
                          end
                in
                  Scheduler.SporkJoin.spork {
                    tokenPolicy = Scheduler.TokenPolicyGive,
                    body = fn () => __inline_always__ step' (w2i i, b),
                    spwn = fn () => spwn z,
                    seq = continue (fn b' => iter b' (WordImpl.+ (i, one), j)),
                    sync = merge',
                    unstolen = SOME (continue spwn)
                  }
                end
          val (result, cont) = __inline_always__ iter z (i2w (Int.min (i, j)), i2w j)
      in
        result
      end


  fun __inline_always__ reducem g z (lo, hi) f =
    pareduce (lo, hi) z (fn (i, a) => __inline_always__ g (a, __inline_always__ f i)) g


  fun __inline_always__ parform (lo: int, hi: int) (f: int -> unit) : unit =
    pareduce (lo, hi) () (fn (i, _) => f i) (fn _ => ())

end