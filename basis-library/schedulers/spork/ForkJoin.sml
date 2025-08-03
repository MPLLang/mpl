structure ForkJoin :>
sig
  datatype TokenPolicy = datatype Scheduler.TokenPolicy
  (* synonym for par *)
  val fork: (unit -> 'a) * (unit -> 'b) -> 'a * 'b 
  val par: (unit -> 'a) * (unit -> 'b) -> 'a * 'b
  val spork: {tokenPolicy: TokenPolicy, body: unit -> 'a, spwn: unit -> 'b, seq: 'a -> 'c, sync: 'a * 'b -> 'c, unstolen: ('a -> 'c) option} -> 'c

  val pareduce: (int * int) -> 'a -> (int * 'a -> 'a) -> ('a * 'a -> 'a) -> 'a
  val pareduceBreakExn: (int * int) -> 'a -> (('a -> exn) * int * 'a -> 'a) -> ('a * 'a -> 'a) -> 'a

  val reducem: ('a * 'a -> 'a) -> 'a -> (int * int) -> (int -> 'a) -> 'a
  val parform: (int * int) -> (int -> unit) -> unit

  val parfor: int -> (int * int) -> (int -> unit) -> unit
  val alloc: int -> 'a array

  val idleTimeSoFar: unit -> Time.time
  val workTimeSoFar: unit -> Time.time
  val maxForkDepthSoFar: unit -> int

  val numSpawnsSoFar: unit -> int
  val numEagerSpawnsSoFar: unit -> int
  val numHeartbeatsSoFar: unit -> int
  val numSkippedHeartbeatsSoFar: unit -> int
  val numStealsSoFar: unit -> int
end =
struct
  datatype TokenPolicy = datatype Scheduler.TokenPolicy
  type w64 = Word64.word
  fun __inline_always__ w2i w = __inline_always__ Word64.toIntX w
  fun __inline_always__ i2w i = __inline_always__ Word64.fromInt i

  val spork = Scheduler.SporkJoin.spork

  fun par (f: unit -> 'a, g: unit -> 'b): 'a * 'b =
      spork {
        tokenPolicy = TokenPolicyFair,
        body = f,
        spwn = g,
        seq  = fn a => (a, g ()),
        sync = fn ab => ab,
        unstolen = NONE
      }

  val fork = par

  (* ======================================================================= *)
  fun __inline_always__ midpoint (i: w64, j: w64) =
    Word64.~>> (i + j, 0w1)

  fun __inline_always__ pareduce (i: int, j: int) (z: 'a) (step: int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let fun iter (b: 'a) (i: w64, j: w64): 'a =
              if i = j then b else
                let fun __inline_never__ spwn b' =
                        if i + 0w1 = j then b' else
                          let val mid = midpoint (i + 0w1, j) in
                            spork {
                              tokenPolicy = TokenPolicyFair,
                              body = fn () => iter b' (i + 0w1, mid),
                              spwn = fn () => iter z (mid, j),
                              seq  = fn b' => iter b' (mid, j),
                              sync = merge,
                              unstolen = NONE
                          }
                          end
                in
                  spork {
                    tokenPolicy = TokenPolicyGive,
                    body = fn () => __inline_always__ step (w2i i, b),
                    spwn = fn () => spwn z,
                    seq = fn b' => iter b' (i + 0w1, j),
                    sync = merge,
                    unstolen = SOME spwn
                  }
                end
      in
        __inline_always__ iter z (i2w (Int.min (i, j)), i2w j)
      end


  fun __inline_always__ pareduceBreak (i: int, j: int) (z: 'a) (step: int * 'a -> 'a * bool) (merge: 'a * 'a -> 'a): 'a =
      let fun merge' ((b1, cont1), (b2, cont2)) =
              if cont1 then (merge (b1, b2), cont2) else (b1, false)

          fun continue (f : 'a -> 'a * bool) : 'a * bool -> 'a * bool =
              fn (b, cont) => if cont then f b else (b, cont)

          fun iter (b: 'a) (i: w64, j: w64): 'a * bool =
              if i = j then (b, true) else
                let
                    fun __inline_never__ spwn b' =
                        if i + 0w1 = j then (b', true) else
                          let val mid = midpoint (i + 0w1, j) in
                            spork {
                              tokenPolicy = TokenPolicyFair,
                              body = fn () => iter b' (i + 0w1, mid),
                              spwn = fn () => iter z (mid, j),
                              seq = continue (fn b' => iter b' (mid, j)),
                              sync = merge',
                              unstolen = NONE
                          }
                          end
                in
                  spork {
                    tokenPolicy = TokenPolicyGive,
                    body = fn () => __inline_always__ step (w2i i, b),
                    spwn = fn () => spwn z,
                    seq = continue (fn b' => iter b' (i + 0w1, j)),
                    sync = merge',
                    unstolen = SOME (continue spwn)
                  }
                end
          val (result, cont) = __inline_always__ iter z (i2w (Int.min (i, j)), i2w j)
      in
        result
      end


  fun __inline_always__ pareduceBreakExn (i: int, j: int) (z: 'a) (step: ('a -> exn) * int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let exception Break of 'a in
        pareduceBreak (i, j) z (fn (i, a) => (__inline_always__ step (Break, i, a), true) handle (Break b) => (b, false)) merge
      end


  fun __inline_always__ reducem g z (lo, hi) f =
    pareduce (lo, hi) z (fn (i, a) => __inline_always__ g (a, __inline_always__ f i)) g


  fun parform (lo: int, hi: int) (f: int -> unit) : unit =
    reducem (fn _ => ()) () (lo, hi) f


  fun parfor grain (i, j) f =
      let fun for (i, j) f = if i >= j then () else (f i; for (i+1, j) f) in
        if j - i <= grain then
          for (i, j) f
        else
          let
            val mid = i + (j-i) div 2
          in
            par (fn _ => parfor grain (i, mid) f,
                 fn _ => parfor grain (mid, j) f)
          ; ()
          end
      end

  fun alloc n =
    let
      val a = ArrayExtra.Raw.alloc n
      val _ =
        if ArrayExtra.Raw.uninitIsNop a then ()
        else parfor 10000 (0, n) (fn i => ArrayExtra.Raw.unsafeUninit (a, i))
    in
      ArrayExtra.Raw.unsafeToArray a
    end

  val maxForkDepthSoFar = Scheduler.maxForkDepthSoFar
  val numSpawnsSoFar = Scheduler.numSpawnsSoFar
  val numEagerSpawnsSoFar = Scheduler.numEagerSpawnsSoFar
  val numHeartbeatsSoFar = Scheduler.numHeartbeatsSoFar
  val numSkippedHeartbeatsSoFar = Scheduler.numSkippedHeartbeatsSoFar
  val numStealsSoFar = Scheduler.numStealsSoFar

  val idleTimeSoFar = Scheduler.IdleTimer.cumulative
  val workTimeSoFar = Scheduler.WorkTimer.cumulative

  fun communicate () = ()
end
