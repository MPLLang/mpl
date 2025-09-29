structure Parfor =
struct

  type word = Word64.word
  fun __inline_always__ w2i w = __inline_always__ Word64.toIntX w
  fun __inline_always__ i2w i = __inline_always__ Word64.fromInt i

  fun __inline_always__ midpoint (i: word, j: word) =
    i + (Word64.>> (j - i, 0w1))


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
        if i + 0w8 <= j then
          let
            fun __inline_never__ spwn a' =
              if i + 0w8 >= j then a' else
                let
                  val mid = midpoint (i + 0w8, j)
                in
                  Scheduler.SporkJoin.spork {
                    tokenPolicy = Scheduler.TokenPolicyFair,
                    body = fn () => loop1 (a', i + 0w8, mid),
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
                  val a = step (a, i+0w1)
                  val a = step (a, i+0w2)
                  val a = step (a, i+0w3)
                  val a = step (a, i+0w4)
                  val a = step (a, i+0w5)
                  val a = step (a, i+0w6)
                  val a = step (a, i+0w7)
                in
                  a
                end,
              seq = fn a' => loop8 (a', i + 0w8, j),
              sync = g,
              spwn = fn () => spwn z,
              unstolen = SOME spwn
            }
          end
        else
          loop1 (a, i, j)



      and loop4 (a, i, j) =
        if i + 0w4 <= j then
          let
            fun __inline_never__ spwn a' =
              if i + 0w4 >= j then a' else
                let
                  val mid = midpoint (i + 0w4, j)
                in
                  Scheduler.SporkJoin.spork {
                    tokenPolicy = Scheduler.TokenPolicyFair,
                    body = fn () => loop1 (a', i + 0w4, mid),
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
                  val a = step (a, i+0w1)
                  val a = step (a, i+0w2)
                  val a = step (a, i+0w3)
                in
                  a
                end,
              seq = fn a' => loop8 (a', i + 0w4, j),
              sync = g,
              spwn = fn () => spwn z,
              unstolen = SOME spwn
            }
          end
        else
          loop1 (a, i, j)


      
      and loop2 (a, i, j) =
        if i + 0w2 <= j then
          let
            fun __inline_never__ spwn a' =
              if i + 0w2 >= j then a' else
                let
                  val mid = midpoint (i + 0w2, j)
                in
                  Scheduler.SporkJoin.spork {
                    tokenPolicy = Scheduler.TokenPolicyFair,
                    body = fn () => loop1 (a', i + 0w2, mid),
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
                  val a = step (a, i+0w1)
                in
                  a
                end,
              seq = fn a' => loop4 (a', i + 0w2, j),
              sync = g,
              spwn = fn () => spwn z,
              unstolen = SOME spwn
            }
          end
        else
          loop1 (a, i, j)


      
      and loop1 (a, i, j) =
        if i + 0w1 <= j then
          let
            fun __inline_never__ spwn a' =
              if i + 0w1 >= j then a' else
                let
                  val mid = midpoint (i + 0w1, j)
                in
                  Scheduler.SporkJoin.spork {
                    tokenPolicy = Scheduler.TokenPolicyFair,
                    body = fn () => loop1 (a', i + 0w1, mid),
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
              seq = fn a' => loop2 (a', i + 0w1, j),
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