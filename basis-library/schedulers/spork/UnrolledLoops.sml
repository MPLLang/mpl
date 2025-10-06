functor UnrolledLoops(WordImpl: WORD) =
struct

  type word = WordImpl.word
  fun __inline_always__ w2i w = __inline_always__ WordImpl.toIntX w
  fun __inline_always__ i2w i = __inline_always__ WordImpl.fromInt i

  val one   = __inline_always__ WordImpl.fromInt 1
  val two   = __inline_always__ WordImpl.fromInt 2
  val three = __inline_always__ WordImpl.fromInt 3
  val four  = __inline_always__ WordImpl.fromInt 4
  val five  = __inline_always__ WordImpl.fromInt 5
  val six   = __inline_always__ WordImpl.fromInt 6
  val seven = __inline_always__ WordImpl.fromInt 7
  val eight = __inline_always__ WordImpl.fromInt 8

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

end