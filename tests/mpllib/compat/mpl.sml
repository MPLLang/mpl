(* already provided by the compiler *)
structure ForkJoin = ForkJoin

structure Concurrency =
struct
  val numberOfProcessors = MLton.Parallel.numberOfProcessors
  val cas = MLton.Parallel.compareAndSwap
  val casArray = MLton.Parallel.arrayCompareAndSwap
end

structure VectorExtra:
sig
  val unsafeFromArray: 'a array -> 'a vector
end =
struct open VectorExtra end

structure ReadFile =
struct

  fun contentsSeq' reader filename =
    let
      val file = MPL.File.openFile filename
      val n = MPL.File.size file
      val arr = ForkJoin.alloc n
      val k = 10000
      val m = 1 + (n - 1) div k
    in
      ForkJoin.parfor 1 (0, m) (fn i =>
        let
          val lo = i * k
          val hi = Int.min ((i + 1) * k, n)
        in
          reader file lo (ArraySlice.slice (arr, lo, SOME (hi - lo)))
        end);
      MPL.File.closeFile file;
      ArraySlice.full arr
    end

  fun contentsSeq filename = contentsSeq' MPL.File.readChars filename

  fun contentsBinSeq filename = contentsSeq' MPL.File.readWord8s filename

  fun contents filename =
    let
      val chars = contentsSeq filename
    in
      CharVector.tabulate (ArraySlice.length chars, fn i =>
        ArraySlice.sub (chars, i))
    end

end

structure WriteFile = PosixWriteFile

structure GCStats:
sig
  val report: unit -> unit
end =
struct

  fun p name thing =
    print (name ^ ": " ^ thing () ^ "\n")

  fun report () =
    let in
      print ("======== GC Stats ========\n");
      p "local reclaimed" (LargeInt.toString o MPL.GC.localBytesReclaimed);
      p "num local" (LargeInt.toString o MPL.GC.numLocalGCs);
      p "local gc time"
        (LargeInt.toString o Time.toMilliseconds o MPL.GC.localGCTime);
      p "promo time"
        (LargeInt.toString o Time.toMilliseconds o MPL.GC.promoTime);
      p "internal reclaimed" (LargeInt.toString o MPL.GC.internalBytesReclaimed)
    end

end


structure RuntimeStats:
sig
  type t
  val get: unit -> t
  val benchReport: {before: t, after: t} -> unit
end =
struct

  type stats =
    { lgcCount: int
    , lgcBytesReclaimed: int
    , lgcBytesInScope: int
    , lgcTracingTime: Time.time
    , lgcPromoTime: Time.time
    , cgcCount: int
    , cgcBytesReclaimed: int
    , cgcBytesInScope: int
    , cgcTime: Time.time
    , schedWorkTime: Time.time
    , schedIdleTime: Time.time
    , susMarks: int
    , deChecks: int
    , entanglements: int
    , bytesPinnedEntangled: int
    , bytesPinnedEntangledWatermark: int
    , numSpawns: int
    , numEagerSpawns: int
    , numHeartbeats: int
    , numSkippedHeartbeats: int
    , numSteals: int
    , maxHeartbeatStackWalk: int
    , maxHeartbeatStackSize: int
    }

  datatype t = Stats of stats

  fun get () =
    Stats
      { lgcCount = LargeInt.toInt (MPL.GC.numLocalGCs ())
      , lgcBytesReclaimed = LargeInt.toInt (MPL.GC.localBytesReclaimed ())
      , lgcBytesInScope = LargeInt.toInt (MPL.GC.bytesInScopeForLocal ())
      , lgcTracingTime = MPL.GC.localGCTime ()
      , lgcPromoTime = MPL.GC.promoTime ()
      , cgcCount = LargeInt.toInt (MPL.GC.numCCs ())
      , cgcBytesReclaimed = LargeInt.toInt (MPL.GC.ccBytesReclaimed ())
      , cgcBytesInScope = LargeInt.toInt (MPL.GC.bytesInScopeForCC ())
      , cgcTime = MPL.GC.ccTime ()
      , schedWorkTime = ForkJoin.workTimeSoFar ()
      , schedIdleTime = ForkJoin.idleTimeSoFar ()
      , susMarks = LargeInt.toInt (MPL.GC.numberSuspectsMarked ())
      , deChecks = LargeInt.toInt (MPL.GC.numberDisentanglementChecks ())
      , entanglements = LargeInt.toInt (MPL.GC.numberEntanglements ())
      , bytesPinnedEntangled = LargeInt.toInt (MPL.GC.bytesPinnedEntangled ())
      , bytesPinnedEntangledWatermark = LargeInt.toInt
          (MPL.GC.bytesPinnedEntangledWatermark ())
      , numSpawns = ForkJoin.numSpawnsSoFar ()
      , numEagerSpawns = ForkJoin.numEagerSpawnsSoFar ()
      , numHeartbeats = ForkJoin.numHeartbeatsSoFar ()
      , numSkippedHeartbeats = ForkJoin.numSkippedHeartbeatsSoFar ()
      , numSteals = ForkJoin.numStealsSoFar ()
      , maxHeartbeatStackSize = IntInf.toInt
          (MPL.GC.maxStackSizeForHeartbeat ())
      , maxHeartbeatStackWalk = IntInf.toInt
          (MPL.GC.maxStackFramesWalkedForHeartbeat ())
      }

  fun pct a b =
    Real.round (100.0 * (Real.fromInt a / Real.fromInt b))
    handle _ => 0

  val itos = Int.toString
  val rtos = Real.fmt (StringCvt.FIX (SOME 2))

  fun benchReport {before = Stats b, after = Stats a} =
    let
      val numSpawns = #numSpawns a - #numSpawns b
      val numEagerSpawns = #numEagerSpawns a - #numEagerSpawns b
      val numHeartbeatSpawns = numSpawns - numEagerSpawns
      val numHeartbeats = #numHeartbeats a - #numHeartbeats b
      val numSkippedHeartbeats =
        #numSkippedHeartbeats a - #numSkippedHeartbeats b
      val numSteals = #numSteals a - #numSteals b

      val eagerp = pct numEagerSpawns numSpawns
      val hbp = pct numHeartbeatSpawns numSpawns
      val skipp = pct numSkippedHeartbeats numHeartbeats

      val spawnsPerHb = Real.fromInt numSpawns / Real.fromInt numHeartbeats
                        handle _ => 0.0

      val eagerSpawnsPerHb =
        Real.fromInt numEagerSpawns / Real.fromInt numHeartbeats
        handle _ => 0.0

      val hbSpawnsPerHb =
        Real.fromInt numHeartbeatSpawns / Real.fromInt numHeartbeats
        handle _ => 0.0

      fun p name (selector: stats -> 'a) (differ: 'a * 'a -> 'a)
        (stringer: 'a -> string) : unit =
        print (name ^ " " ^ stringer (differ (selector a, selector b)) ^ "\n")
    in
      print ("======== Runtime Stats ========\n");
      print ("num spawns        " ^ itos numSpawns ^ "\n");
      print
        ("  eager           " ^ itos numEagerSpawns ^ " (" ^ itos eagerp
         ^ "%)\n");
      print
        ("  at heartbeat    " ^ itos numHeartbeatSpawns ^ " (" ^ itos hbp
         ^ "%)\n");
      print "\n";
      print ("num heartbeats    " ^ itos numHeartbeats ^ "\n");
      print
        ("  skipped         " ^ itos numSkippedHeartbeats ^ " (" ^ itos skipp
         ^ "%)\n");
      print "\n";
      print ("spawns / hb       " ^ rtos spawnsPerHb ^ "\n");
      print ("  eager           " ^ rtos eagerSpawnsPerHb ^ "\n");
      print ("  at heartbeat    " ^ rtos hbSpawnsPerHb ^ "\n");
      print "\n";
      print ("num steals        " ^ itos numSteals ^ "\n");
      print "\n";
      print ("max hb stack walk " ^ itos (#maxHeartbeatStackWalk a) ^ "\n");
      print ("max hb stack size " ^ itos (#maxHeartbeatStackSize a) ^ "\n");
      print "\n";

      p "sus marks" #susMarks op- Int.toString;
      p "de checks" #deChecks op- Int.toString;
      p "entanglements" #entanglements op- Int.toString;
      p "bytes pinned entangled" #bytesPinnedEntangled op- Int.toString;
      p "bytes pinned entangled watermark" #bytesPinnedEntangledWatermark #1
        Int.toString;
      print "\n";
      p "lgc count" #lgcCount op- Int.toString;
      p "lgc bytes reclaimed" #lgcBytesReclaimed op- Int.toString;
      p "lgc bytes in scope " #lgcBytesInScope op- Int.toString;
      p "lgc trace time(ms) " #lgcTracingTime Time.-
        (LargeInt.toString o Time.toMilliseconds);
      p "lgc promo time(ms) " #lgcPromoTime Time.-
        (LargeInt.toString o Time.toMilliseconds);
      p "lgc total time(ms) "
        (fn x => Time.+ (#lgcTracingTime x, #lgcPromoTime x)) Time.-
        (LargeInt.toString o Time.toMilliseconds);
      print "\n";
      p "cgc count" #cgcCount op- Int.toString;
      p "cgc bytes reclaimed" #cgcBytesReclaimed op- Int.toString;
      p "cgc bytes in scope " #cgcBytesInScope op- Int.toString;
      p "cgc time(ms)" #cgcTime Time.- (LargeInt.toString o Time.toMilliseconds);
      print "\n";
      p "work time(ms)" #schedWorkTime Time.-
        (LargeInt.toString o Time.toMilliseconds);
      p "idle time(ms)" #schedIdleTime Time.-
        (LargeInt.toString o Time.toMilliseconds);
      print ("====== End Runtime Stats ======\n");
      ()
    end

end
