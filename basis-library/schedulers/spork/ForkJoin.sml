structure ForkJoin0 =
struct
  datatype TokenPolicy = datatype Scheduler.TokenPolicy

  val spork = Scheduler.SporkJoin.spork
  val primSporkChoose = Scheduler.primSporkChoose

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


functor ManagedLoops (LoopIndex: LOOP_INDEX) :>
sig
  val pareduce: (int * int) -> 'a -> (int * 'a -> 'a) -> ('a * 'a -> 'a) -> 'a
  val pareduceBreakExn: (int * int) -> 'a -> (('a -> exn) * int * 'a -> 'a) -> ('a * 'a -> 'a) -> 'a
  val reducem: ('a * 'a -> 'a) -> 'a -> (int * int) -> (int -> 'a) -> 'a
  val parform: (int * int) -> (int -> unit) -> unit
  val seqLoop: (int * int) -> (int -> unit) -> unit
  val seqReduce: ('a * 'a -> 'a) -> 'a -> (int * int) -> (int -> 'a) -> 'a
end =
struct
  type idx = LoopIndex.t

  open ForkJoin0

  fun __inline_always__ pareduce (i: int, j: int) (z: 'a) (step: int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let fun iter (b: 'a) (i: idx, j: idx): 'a =
              if LoopIndex.equal (i, j) then b else
                let fun __inline_never__ spwn b' =
                        if LoopIndex.equal (LoopIndex.increment i, j) then b' else
                          let val mid = LoopIndex.midpoint (LoopIndex.increment i, j) in
                            spork {
                              tokenPolicy = TokenPolicyFair,
                              body = fn () => iter b' (LoopIndex.increment i, mid),
                              spwn = fn () => iter z (mid, j),
                              seq  = fn b' => iter b' (mid, j),
                              sync = merge,
                              unstolen = NONE
                          }
                          end
                in
                  spork {
                    tokenPolicy = TokenPolicyGive,
                    body = fn () => __inline_always__ step (LoopIndex.toInt i, b),
                    spwn = fn () => spwn z,
                    seq = fn b' => iter b' (LoopIndex.increment i, j),
                    sync = merge,
                    unstolen = SOME spwn
                  }
                end
      in
        __inline_always__ iter z (LoopIndex.fromInt (Int.min (i, j)), LoopIndex.fromInt j)
      end


  fun __inline_always__ pareduceBreak (i: int, j: int) (z: 'a) (step: int * 'a -> 'a * bool) (merge: 'a * 'a -> 'a): 'a =
      let fun merge' ((b1, cont1), (b2, cont2)) =
              if cont1 then (merge (b1, b2), cont2) else (b1, false)

          fun continue (f : 'a -> 'a * bool) : 'a * bool -> 'a * bool =
              fn (b, cont) => if cont then f b else (b, cont)

          fun iter (b: 'a) (i: idx, j: idx): 'a * bool =
              if LoopIndex.equal (i, j) then (b, true) else
                let
                    fun __inline_never__ spwn b' =
                        if LoopIndex.equal (LoopIndex.increment i, j) then (b', true) else
                          let val mid = LoopIndex.midpoint (LoopIndex.increment i, j) in
                            spork {
                              tokenPolicy = TokenPolicyFair,
                              body = fn () => iter b' (LoopIndex.increment i, mid),
                              spwn = fn () => iter z (mid, j),
                              seq = continue (fn b' => iter b' (mid, j)),
                              sync = merge',
                              unstolen = NONE
                          }
                          end
                in
                  spork {
                    tokenPolicy = TokenPolicyGive,
                    body = fn () => __inline_always__ step (LoopIndex.toInt i, b),
                    spwn = fn () => spwn z,
                    seq = continue (fn b' => iter b' (LoopIndex.increment i, j)),
                    sync = merge',
                    unstolen = SOME (continue spwn)
                  }
                end
          val (result, cont) = __inline_always__ iter z (LoopIndex.fromInt (Int.min (i, j)), LoopIndex.fromInt j)
      in
        result
      end


  fun __inline_always__ pareduceBreakExn (i: int, j: int) (z: 'a) (step: ('a -> exn) * int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let exception Break of 'a in
        pareduceBreak (i, j) z (fn (i, a) => (__inline_always__ step (Break, i, a), true) handle (Break b) => (b, false)) merge
      end


  fun __inline_always__ reducem g z (lo, hi) f =
    pareduce (lo, hi) z (fn (i, a) => __inline_always__ g (a, __inline_always__ f i)) g


  fun __inline_always__ parform (lo: int, hi: int) (f: int -> unit) : unit =
    reducem (fn _ => ()) () (lo, hi) f


  fun __inline_always__ seqLoop (lo: int, hi: int) (f: int -> unit) : unit =
    let
      fun loop (i: idx, j: idx) : unit =
        if LoopIndex.equal (i, j) then ()
        else (__inline_always__ f (LoopIndex.toInt i); loop (LoopIndex.increment i, j))
    in
      loop (LoopIndex.fromInt (Int.min (lo, hi)), LoopIndex.fromInt hi)
    end


  fun __inline_always__ seqReduce (combine: 'a * 'a -> 'a) (zero: 'a) (lo: int, hi: int) (f: int -> 'a) : 'a =
    let
      fun loop (acc: 'a) (i: idx, j: idx) : 'a =
        if LoopIndex.equal (i, j) then acc
        else loop (__inline_always__ combine (acc, __inline_always__ f (LoopIndex.toInt i))) (LoopIndex.increment i, j)
    in
      loop zero (LoopIndex.fromInt (Int.min (lo, hi)), LoopIndex.fromInt hi)
    end
end


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
  val reduce:  ('a * 'a -> 'a) -> 'a -> (int * int) -> (int -> 'a) -> 'a
  val reducemDefault: ('a * 'a -> 'a) -> 'a -> (int * int) -> (int -> 'a) -> 'a
  val parform: (int * int) -> (int -> unit) -> unit
  val parformDefault: (int * int) -> (int -> unit) -> unit

  val parfor: int -> (int * int) -> (int -> unit) -> unit
  val alloc: int -> 'a array

  val seqLoop: (int * int) -> (int -> unit) -> unit
  val seqReduce: ('a * 'a -> 'a) -> 'a -> (int * int) -> (int -> 'a) -> 'a

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

  open ForkJoin0

  structure Loops8 = ManagedLoops(LoopIndexFromWord(Word8))
  structure Loops16 = ManagedLoops(LoopIndexFromWord(Word16))
  structure Loops32 = ManagedLoops(LoopIndexFromWord(Word32))
  structure Loops64 = ManagedLoops(LoopIndexFromWord(Word64))
  structure LoopsInt = ManagedLoops(struct
    type t = int
    type idx = t
    fun fromInt x = x
    fun toInt x = x
    fun increment x = x + 1
    fun midpoint (i, j) = i + (j-i) div 2
    val equal = op=
  end)

  structure Unrolled8 = UnrolledLoops(Word8)
  structure Unrolled16 = UnrolledLoops(Word16)
  structure Unrolled32 = UnrolledLoops(Word32)
  structure Unrolled64 = UnrolledLoops(Word64)

  structure Pareduce =
    Int_ChooseFromInt (struct
      type 'a t = (int * int) -> 'a -> (int * 'a -> 'a) -> ('a * 'a -> 'a) -> 'a
      val fInt8 = Loops8.pareduce
      val fInt16 = Loops16.pareduce
      val fInt32 = Loops32.pareduce
      val fInt64 = Loops64.pareduce
      val fIntInf = LoopsInt.pareduce
    end)

  structure PareduceBreakExn =
    Int_ChooseFromInt (struct
      type 'a t = (int * int) -> 'a -> (('a -> exn) * int * 'a -> 'a) -> ('a * 'a -> 'a) -> 'a
      val fInt8 = Loops8.pareduceBreakExn
      val fInt16 = Loops16.pareduceBreakExn
      val fInt32 = Loops32.pareduceBreakExn
      val fInt64 = Loops64.pareduceBreakExn
      val fIntInf = LoopsInt.pareduceBreakExn
    end)

  structure Reducem =
    Int_ChooseFromInt (struct
      type 'a t = ('a * 'a -> 'a) -> 'a -> (int * int) -> (int -> 'a) -> 'a
      val fInt8 = Loops8.reducem
      val fInt16 = Loops16.reducem
      val fInt32 = Loops32.reducem
      val fInt64 = Loops64.reducem
      val fIntInf = LoopsInt.reducem
    end)

  structure Parform =
    Int_ChooseFromInt (struct
      type 'a t = (int * int) -> (int -> unit) -> unit
      val fInt8 = Loops8.parform
      val fInt16 = Loops16.parform
      val fInt32 = Loops32.parform
      val fInt64 = Loops64.parform
      val fIntInf = LoopsInt.parform
    end)

  structure UnrolledPareduce =
    Int_ChooseFromInt (struct
      type 'a t = (int * int) -> 'a -> (int * 'a -> 'a) -> ('a * 'a -> 'a) -> 'a
      val fInt8 = Unrolled8.pareduce
      val fInt16 = Unrolled16.pareduce
      val fInt32 = Unrolled32.pareduce
      val fInt64 = Unrolled64.pareduce
      val fIntInf = Unrolled64.pareduce  
    end)

  structure UnrolledPareduceBreakExn =
    Int_ChooseFromInt (struct
      type 'a t = (int * int) -> 'a -> (('a -> exn) * int * 'a -> 'a) -> ('a * 'a -> 'a) -> 'a
      val fInt8 = Unrolled8.pareduceBreakExn
      val fInt16 = Unrolled16.pareduceBreakExn
      val fInt32 = Unrolled32.pareduceBreakExn
      val fInt64 = Unrolled64.pareduceBreakExn
      val fIntInf = Unrolled64.pareduceBreakExn 
    end)

  structure UnrolledReducem =
    Int_ChooseFromInt (struct
      type 'a t = ('a * 'a -> 'a) -> 'a -> (int * int) -> (int -> 'a) -> 'a
      val fInt8 = Unrolled8.reducem
      val fInt16 = Unrolled16.reducem
      val fInt32 = Unrolled32.reducem
      val fInt64 = Unrolled64.reducem
      val fIntInf = Unrolled64.reducem
    end)

  structure UnrolledParform =
    Int_ChooseFromInt (struct
      type 'a t = (int * int) -> (int -> unit) -> unit
      val fInt8 = Unrolled8.parform
      val fInt16 = Unrolled16.parform
      val fInt32 = Unrolled32.parform
      val fInt64 = Unrolled64.parform
      val fIntInf = Unrolled64.parform
    end)

  structure SeqLoop =
    Int_ChooseFromInt (struct
      type 'a t = (int * int) -> (int -> unit) -> unit
      val fInt8 = Loops8.seqLoop
      val fInt16 = Loops16.seqLoop
      val fInt32 = Loops32.seqLoop
      val fInt64 = Loops64.seqLoop
      val fIntInf = LoopsInt.seqLoop
    end)

  structure SeqReduce =
    Int_ChooseFromInt (struct
      type 'a t = ('a * 'a -> 'a) -> 'a -> (int * int) -> (int -> 'a) -> 'a
      val fInt8 = Loops8.seqReduce
      val fInt16 = Loops16.seqReduce
      val fInt32 = Loops32.seqReduce
      val fInt64 = Loops64.seqReduce
      val fIntInf = LoopsInt.seqReduce
    end)

  structure UnrolledSeqLoop =
    Int_ChooseFromInt (struct
      type 'a t = (int * int) -> (int -> unit) -> unit
      val fInt8 = Unrolled8.seqLoop
      val fInt16 = Unrolled16.seqLoop
      val fInt32 = Unrolled32.seqLoop
      val fInt64 = Unrolled64.seqLoop
      val fIntInf = Unrolled64.seqLoop
    end)

  structure UnrolledSeqReduce =
    Int_ChooseFromInt (struct
      type 'a t = ('a * 'a -> 'a) -> 'a -> (int * int) -> (int -> 'a) -> 'a
      val fInt8 = Unrolled8.seqReduce
      val fInt16 = Unrolled16.seqReduce
      val fInt32 = Unrolled32.seqReduce
      val fInt64 = Unrolled64.seqReduce
      val fIntInf = Unrolled64.seqReduce
    end)

  local

    fun __inline_always__ unifiedReducem (combine: 'a * 'a -> 'a) (zero: 'a) (lo: int, hi: int) (f: int -> 'a) : 'a =
      let
        fun __inline_always__ regularImpl () = __inline_always__ Reducem.f combine zero (lo, hi) f
        fun __inline_always__ unrolledImpl () = __inline_always__ UnrolledReducem.f combine zero (lo, hi) f
      in
        primSporkChoose (__inline_always__ f, __inline_always__ unrolledImpl, __inline_always__ regularImpl)
      end

    fun unifiedParform (lo: int, hi: int) (f: int -> unit) : unit =
      let
        fun __inline_always__ regularImpl () = __inline_always__ Parform.f (lo, hi) f

        fun __inline_always__ unrolledImpl () = __inline_always__ UnrolledParform.f (lo, hi) f 
      in
        primSporkChoose (__inline_always__ f, __inline_always__ unrolledImpl, __inline_always__ regularImpl)
      end

    fun __inline_always__ unifiedPareduce (lo: int, hi: int) (zero: 'a) (step: int * 'a -> 'a) (combine: 'a * 'a -> 'a) : 'a =
    let
      fun __inline_always__ regularImpl () =
        __inline_always__ Pareduce.f (lo, hi) zero step combine

      fun __inline_always__ unrolledImpl () =
        __inline_always__ UnrolledPareduce.f (lo, hi) zero step combine

      fun __inline_always__ loopBody i = __inline_always__ step (i, zero)
    in
      primSporkChoose (__inline_always__ loopBody, __inline_always__ unrolledImpl, __inline_always__ regularImpl)
    end

    fun __inline_always__ unifiedSeqLoop (lo: int, hi: int) (f: int -> unit) : unit =
      let
        fun __inline_always__ regularImpl () = __inline_always__ SeqLoop.f (lo, hi) f
        fun __inline_always__ unrolledImpl () = __inline_always__ UnrolledSeqLoop.f (lo, hi) f
      in
        Scheduler.primLoopChoose (__inline_always__ f, __inline_always__ unrolledImpl, __inline_always__ regularImpl)
      end

    fun __inline_always__ unifiedSeqReduce (combine: 'a * 'a -> 'a) (zero: 'a) (lo: int, hi: int) (f: int -> 'a) : 'a =
      let
        fun __inline_always__ regularImpl () = __inline_always__ SeqReduce.f combine zero (lo, hi) f
        fun __inline_always__ unrolledImpl () = __inline_always__ UnrolledSeqReduce.f combine zero (lo, hi) f
      in
        Scheduler.primLoopChoose (__inline_always__ f, __inline_always__ unrolledImpl, __inline_always__ regularImpl)
      end
  in
    val reducem = __inline_always__ unifiedReducem
    val reduce =  __inline_always__ unifiedReducem
    val reducemDefault =  __inline_always__ Reducem.f
    val parform = __inline_always__ unifiedParform
    val parformDefault =  __inline_always__ Parform.f
    val pareduce =  __inline_always__ unifiedPareduce
    val parfor =  __inline_always__ ForkJoin0.parfor
    val seqLoop = __inline_always__ unifiedSeqLoop
    val seqReduce = __inline_always__ unifiedSeqReduce
  end

  val pareduceBreakExn = __inline_always__ PareduceBreakExn.f
end