structure ForkJoin0 =
struct
  datatype TokenPolicy = datatype Scheduler.TokenPolicy

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


functor ManagedLoops (LoopIndex: LOOP_INDEX) :>
sig
  val pareduce: (int * int) -> 'a -> (int * 'a -> 'a) -> ('a * 'a -> 'a) -> 'a
  val pareduceBreakExn: (int * int) -> 'a -> (('a -> exn) * int * 'a -> 'a) -> ('a * 'a -> 'a) -> 'a
  val reducem: ('a * 'a -> 'a) -> 'a -> (int * int) -> (int -> 'a) -> 'a
  val parform: (int * int) -> (int -> unit) -> unit
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

  local
    (* fallback to regular implementation until runtime supports spork_choose *)
    fun __inline_always__ primSporkChoose (loopBody, unrolled, regular) = __inline_always__ unrolled ()

    fun unifiedReducem (combine: 'a * 'a -> 'a) (zero: 'a) (lo: int, hi: int) (f: int -> 'a) : 'a =
    let
      fun __inline_always__ regularImpl () =
        let
        (* TODO: look into this piece *)
          val pareduce = case Int.precision of
              SOME 8 => Loops8.pareduce
            | SOME 16 => Loops16.pareduce
            | SOME 32 => Loops32.pareduce
            | SOME 64 => Loops64.pareduce
            | _ => LoopsInt.pareduce
        in
          pareduce (lo, hi) zero (fn (i, a) => combine (a, f i)) combine
        end

      fun __inline_always__ unrolledImpl () =
        __inline_always__ Unrolled.pareduce (lo, hi) zero (fn (i, a) => combine (a, f i)) combine
    in
      primSporkChoose (__inline_always__ f, __inline_always__ unrolledImpl, __inline_always__ regularImpl)
    end

    fun unifiedParform (lo: int, hi: int) (f: int -> unit) : unit =
    let
      fun __inline_always__ regularImpl () =
        let
          val parform = case Int.precision of
              SOME 8 => Loops8.parform
            | SOME 16 => Loops16.parform
            | SOME 32 => Loops32.parform
            | SOME 64 => Loops64.parform
            | _ => LoopsInt.parform
        in
          parform (lo, hi) f
        end

      fun __inline_always__ unrolledImpl () =
        __inline_always__ Unrolled.pareduce (lo, hi) () (fn (i, _) => f i) (fn _ => ())

    in
      primSporkChoose (__inline_always__ f, __inline_always__ unrolledImpl, __inline_always__ regularImpl)
    end

    fun __inline_always__ unifiedPareduce (lo: int, hi: int) (zero: 'a) (step: int * 'a -> 'a) (combine: 'a * 'a -> 'a) : 'a =
    let
      fun __inline_always__ regularImpl () =
        let
          val pareduce = case Int.precision of
              SOME 8 => Loops8.pareduce
            | SOME 16 => Loops16.pareduce
            | SOME 32 => Loops32.pareduce
            | SOME 64 => Loops64.pareduce
            | _ => LoopsInt.pareduce
        in
          pareduce (lo, hi) zero step combine
        end

      fun __inline_always__ unrolledImpl () =
        __inline_always__ Unrolled.pareduce (lo, hi) zero step combine

      fun __inline_always__ loopBody i = step (i, zero)
    in
      primSporkChoose (__inline_always__ loopBody, __inline_always__ unrolledImpl, __inline_always__ regularImpl)
    end
  in
    val reducem = unifiedReducem
    val parform = unifiedParform
    val pareduce = unifiedPareduce
    val parfor = ForkJoin0.parfor
  end

  val pareduceBreakExn = PareduceBreakExn.f
end
