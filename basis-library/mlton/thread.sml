(* Copyright (C) 2014,2019 Matthew Fluet.
 * Copyright (C) 2004-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonThread:> MLTON_THREAD_EXTRA =
struct

val arrayUpdateNoBarrier = Primitive.MLton.HM.arrayUpdateNoBarrier
val refAssignNoBarrier = Primitive.MLton.HM.refAssignNoBarrier

structure Prim = Primitive.MLton.Thread

fun die (s: string): 'a =
   (PrimitiveFFI.Stdio.print s
    ; PrimitiveFFI.Posix.Process.exit 1
    ; let exception DieFailed
      in raise DieFailed
      end)

val gcState = Primitive.MLton.GCState.gcState

structure AtomicState =
   struct
      datatype t = NonAtomic | Atomic of int
   end

local
   open Prim
in
   val atomicBegin = atomicBegin
   val atomicEnd = atomicEnd
   val atomicState = fn () =>
      case atomicState () of
         0wx0 => AtomicState.NonAtomic
       | w => AtomicState.Atomic (Word32.toInt w)
end

fun assertAtomicState x =
  Prim.assertAtomicState (gcState (), Word32.fromInt x)

fun atomically f =
   (atomicBegin (); DynamicWind.wind (f, atomicEnd))

datatype 'a thread =
   Dead
 | Interrupted of Prim.thread
 | New of 'a -> unit
 (* In Paused (f, t), f is guaranteed to not raise an exception. *)
 | Paused of ((unit -> 'a) -> unit) * Prim.thread

datatype 'a t = T of 'a thread ref

structure Runnable =
   struct
      type t = unit t
   end

structure Basic =
  struct
    open Prim
    type p = preThread
    type t = thread
    val current = fn () =>
       current (gcState ())
    val savedPre = fn () =>
       savedPre (gcState ())
  end

structure HierarchicalHeap =
struct
  (* The prim operations are generally the same, except that we need to
   * convert to/from integers for a few of them. *)
  open Prim

  type thread = Basic.t
  type t = MLtonPointer.t

  type clear_set = MLtonPointer.t
  type finished_clear_set_grain = MLtonPointer.t

  fun forceLeftHeap (myId, t) = Prim.forceLeftHeap(Word32.fromInt myId, t)
  fun forceNewChunk () = Prim.forceNewChunk (gcState ())
  fun registerCont (kl, kr, k, t) = Prim.registerCont(kl, kr, k, t)
  fun resetList (t) = Prim.resetList(t)

  fun cancelCC (t, hh) = Prim.cancelCC (gcState (), t, hh)
  fun collectThreadRoot (t, hh) = Prim.collectThreadRoot (t, hh)
  fun getRoot t = Prim.getRoot t

  fun getDepth t = Word32.toInt (Prim.getDepth t)
  fun setDepth (t, d) = Prim.setDepth (t, Word32.fromInt d)
  fun setMinLocalCollectionDepth (t, d) =
    Prim.setMinLocalCollectionDepth (t, Word32.fromInt d)
  fun moveNewThreadToDepth (t, tid, d) =
    Prim.moveNewThreadToDepth (t, tid, Word32.fromInt d)
  fun checkFinishedCCReadyToJoin () =
    Prim.checkFinishedCCReadyToJoin (gcState ())

  fun clearSuspectsAtDepth (t, d) =
    Prim.clearSuspectsAtDepth (gcState (), t, Word32.fromInt d)
  
  fun numSuspectsAtDepth (t, d) =
    Word64.toInt (Prim.numSuspectsAtDepth (gcState (), t, Word32.fromInt d))

  fun takeClearSetAtDepth (t, d) =
    Prim.takeClearSetAtDepth (gcState (), t, Word32.fromInt d)

  fun numChunksInClearSet c =
    Word64.toInt (Prim.numChunksInClearSet (gcState (), c))

  fun processClearSetGrain (c, start, stop) =
    Prim.processClearSetGrain (gcState (), c, Word64.fromInt start, Word64.fromInt stop)

  fun commitFinishedClearSetGrain (t, fcsg) =
    Prim.commitFinishedClearSetGrain (gcState (), t, fcsg)

  fun deleteClearSet c =
    Prim.deleteClearSet (gcState (), c)

  fun updateBytesPinnedEntangledWatermark () =
    Prim.updateBytesPinnedEntangledWatermark (gcState ())

  fun canForkThread t =
    Prim.canForkThread (gcState (), t)
    
  (* fun forkThread (t, jp) =
    Prim.forkThread (gcState (), t, jp) *)

  fun joinIntoParentBeforeFastClone {thread, newDepth, tidLeft, tidRight} =
    Prim.joinIntoParentBeforeFastClone (gcState (), thread, Word32.fromInt newDepth, tidLeft, tidRight)

  fun joinIntoParent {thread, rightSideThread, newDepth, tidLeft, tidRight} =
    Prim.joinIntoParent (gcState (), thread, rightSideThread, Word32.fromInt newDepth, tidLeft, tidRight)
end

structure Disentanglement =
struct
  type thread = Basic.t

  fun decheckMaxDepth () =
    let
      val r = ref 0w0
    in
      if Prim.decheckMaxDepth r then
        SOME (Word32.toIntX (!r))
      else
        NONE
    end

  fun decheckFork () =
    let
      val left = ref (0w0: Word64.word)
      val right = ref (0w0: Word64.word)
    in
      Prim.decheckFork (gcState (), left, right);
      (!left, !right)
    end

  fun decheckJoin (left, right) =
    Prim.decheckJoin (gcState (), left, right)

  fun decheckSetTid tid = Prim.decheckSetTid (gcState (), tid)

  fun decheckGetTid thread = Prim.decheckGetTid (gcState (), thread)

  fun copySyncDepthsFromThread (from, to, stealDepth) =
    Prim.copySyncDepthsFromThread (gcState (), from, to, Word32.fromInt stealDepth)
end

(*
fun prepend (T r: 'a t, f: 'b -> 'a): 'b t =
   let
      val t =
         case !r of
            Dead => raise Fail "prepend to a Dead thread"
          | Interrupted _ => raise Fail "prepend to a Interrupted thread"
          | New g => New (g o f)
          | Paused (g, t) => Paused (fn h => g (f o h), t)
   in r := Dead
      ; T (ref t)
   end

fun prepare (t: 'a t, v: 'a): Runnable.t =
   prepend (t, fn () => v)

fun new f = T (ref (New f))
*)

fun prepend _ = die "MLton.Thread.prepend unsupported\n"
fun prepare _ = die "MLton.Thread.prepare unsupported\n"
fun new _ = die "MLton.Thread.new unsupported\n"

val numProcs = Int32.toInt Primitive.MLton.Parallel.numberOfProcessors
val procNum = Int32.toInt o Primitive.MLton.Parallel.processorNumber

(*
local
    local
        (* create one reference per processor *)
        val func: (unit -> unit) option Array.array =
            Array.tabulate (numProcs, fn _ => NONE)
        val base: Prim.preThread =
            let
                val () = Prim.copyCurrent ()
                (* Call to procNum *must* come after copy *)
                val proc = procNum ()
            in
                case Array.unsafeSub (func, proc) of
                    NONE => Prim.savedPre (gcState ())
                  | SOME x =>
                    (* This branch never returns. *)
                    let
                        (* Atomic 1 *)
                        val () = Array.update (func, proc, NONE)
                        val () = atomicEnd ()
                        (* Atomic 0 *)
                    in
                        (x () handle e => MLtonExn.topLevelHandler e)
                      ; die "Thread didn't exit properly.\n"
                    end
            end
    in
        fun newThread (f: unit -> unit) : Prim.thread =
            let
                (* Atomic 2 *)
                val () = Array.update (func, procNum (), SOME f)
            in
                Prim.copy base
            end
    end
    val switching = Array.tabulate (numProcs, fn _ => false)
in
    fun 'a atomicSwitchTop
           (f: 'a t -> Runnable.t)
           (cont: (unit -> Runnable.t) * (exn -> Runnable.t) -> unit): 'a =
        let
            val proc = procNum ()
        in
            (* Atomic 1 *)
            if Array.unsafeSub (switching, proc)
            then
                let
                    val () = atomicEnd ()
                    (* Atomic 0 *)
                in
                    raise Fail "atomicSwitchTop while switching (nested Thread.switch?)"
                end
            else
                let
                    val _ = Array.update (switching, proc, true)

                    val curPrimThread = Prim.current (gcState ())

                    val r : (unit -> 'a) ref =
                        ref (fn () => die "Thread.atomicSwitch didn't set r.\n")
                    val t: 'a thread ref =
                        ref (Paused (fn x => r := x, curPrimThread))

                    val t': unit thread ref =
                        ref (Interrupted curPrimThread)
                    val re: exn option ref = ref NONE

                    fun fail e = (t := Dead
                                 ; Array.update (switching, proc, false)
                                 ; atomicEnd ()
                                 ; raise e)
                in
                    cont (fn () => f (T t),
                          fn e => (re := SOME e; t := Dead; T t'))
                    handle e => fail e;
                    (case !re
                      of NONE => ()
                       | SOME e => raise e);
                    !r ()
                end
        end

    fun 'a atomicSwitchBottom (T t': Runnable.t): unit =
        let
            val proc = procNum ()
        in
            (* Atomic 1 *)
            if not (Array.unsafeSub (switching, proc))
            then
                let
                    val () = atomicEnd ()
                                       (* Atomic 0 *)
                in
                    raise Fail "atomicSwitchBottom while not switching (nested Thread.switch?)"
                end
            else
                let
                    val primThread =
                        case !t' before t' := Dead of
                            Dead => raise Fail "switch to a Dead thread"
                          | Interrupted t => t
                          | New g => (atomicBegin (); newThread g)
                          | Paused (f, t) => (f (fn () => ()); t)

                    val _ = if not (Array.unsafeSub (switching, proc))
                            then raise Fail "switching switched?"
                            else ()

                    val _ = Array.update (switching, proc, false)
                    (* Atomic 1 when Paused/Interrupted, Atomic 2 when New *)
                    val _ = Prim.switchTo primThread (* implicit atomicEnd() *)
                                          (* Atomic 0 when resuming *)
                in
                    ()
                end
        end

    fun atomicSwitch f =
        atomicSwitchTop f (fn (x, _) => atomicSwitchBottom (x ()))

    fun switch f =
        (atomicBegin ();
         atomicSwitch f)
end
*)


(*
fun fromPrimitive (t: Prim.thread): Runnable.t =
   T (ref (Interrupted t))

fun toPrimitive (t as T r : unit t): Prim.thread =
   case !r of
      Dead => die "Thread.toPrimitive saw Dead.\n"
    | Interrupted t =>
         (r := Dead
          ; t)
    | New _ =>
         switch
         (fn cur : Prim.thread t =>
          prepare
          (prepend (t, fn () =>
                    switch
                    (fn t' : unit t =>
                     prepare (cur, toPrimitive t'))),
           ()))
    | Paused (f, t) =>
         (r := Dead
          ; f (fn () => ())
          ; t)

val initPrimitive: unit t -> unit t = fromPrimitive o toPrimitive
*)

fun fromPrimitive _ = die "MLton.Thread.fromPrimitive unsupported\n"
fun toPrimitive _ = die "MLton.Thread.toPrimitive unsupported\n"
fun initPrimitive _ = die "MLton.Thread.initPrimitive unsupported\n"


local
   val signalHandlers: Prim.thread option array = Array.array (numProcs, NONE)
   datatype state = Normal | InHandler
   val state: state array = Array.array (numProcs, Normal)
in
   fun amInSignalHandler () = InHandler = Array.sub (state, procNum ())

   fun setSimpleSignalHandler (f: Basic.t -> unit): unit =
      let
         val _ = Primitive.MLton.installSignalHandler ()
         fun loop (): unit =
            let
               (* Atomic 1 *)
               val _ = assertAtomicState 1
               val proc = procNum ()
               (* val _ = print ("Start Prim.saved\n") *)
               val t = Prim.saved (gcState ())
               (* val _ = print ("Finish Prim.saved\n") *)
               val _ = Array.update (state, proc, InHandler)
               val oldHH = Prim.handlerEnterHeapOfThread (gcState (), t)
               val _ = f t
               val _ = Prim.handlerLeaveHeapOfThread (gcState (), t, oldHH)
               val _ = Array.update (state, proc, Normal)
               val _ = Prim.finishSignalHandler (gcState ())
               (* val _ = print ("switch away from signal handler\n") *)
               val _ = assertAtomicState 1
               val _ = Prim.switchTo t (* implicit atomicEnd () *)
            in
               loop ()
            end

         val amOriginal = ref true
         val _ = Basic.copyCurrent ()
         val signalHandlerPrototype : Basic.p =
            if !amOriginal then
               (amOriginal := false; Basic.savedPre ())
            else
               ( loop () handle e => MLtonExn.topLevelHandler e
               ; die "MLton.Thread: bug: signal handler loop exited unexpectedly\n"
               )

         val handlerThreads =
            Array.tabulate
            (numProcs, fn i =>
             let
                val p = Basic.copy signalHandlerPrototype
                val _ = Array.update (signalHandlers, i, SOME p)
             in
                p
             end)
      in
         Prim.setSignalHandlers (gcState (), handlerThreads)
      end

   fun setSignalHandler _ = die "MLton.Thread.setSignalHandler unsupported\n"

   fun switchToSignalHandler () =
      let
         (* Atomic 0 *)
         val () = atomicBegin ()
         (* Atomic 1 *)
         val () = Prim.startSignalHandler (gcState ()) (* implicit atomicBegin () *)
         (* Atomic 2 *)
      in
         case Array.sub (signalHandlers, procNum ()) of
            NONE => raise Fail "no signal handler installed"
          | SOME t => Prim.switchTo t (* implicit atomicEnd() *)
      end
end


local
   val numProcs = Int32.toInt Primitive.MLton.Parallel.numberOfProcessors
   val procNum = Int32.toInt o Primitive.MLton.Parallel.processorNumber
in
   val register: int * (MLtonPointer.t -> unit) -> unit =
      let
         val exports =
            Array.array (Int32.toInt (Primitive.MLton.FFI.numExports),
                         fn _ => raise Fail "undefined export")

         type worker = Prim.thread * Prim.thread option ref
         type worker_arg = worker

         val workerCache: worker option array = Array.array (numProcs, NONE)
         val workerArgs: worker_arg option array = Array.array (numProcs, NONE)

         fun workerLoop (thisWorker as (_, savedRef): worker): unit =
            let
               (* Atomic 1 *)
               val p = Primitive.MLton.FFI.getOpArgsResPtr (gcState ())
               val _ = atomicEnd ()
               (* Atomic 0 *)
               val i = MLtonPointer.getInt32 (MLtonPointer.getPointer (p, 0), 0)
               (* val _ = print ("[" ^ Int.toString (procNum()) ^ "] running export " ^ Int32.toString i ^ "\n") *)
               val _ =
                  (Array.sub (exports, Int32.toInt i) p)
                  handle e =>
                     (TextIO.output
                      (TextIO.stdErr, "Call from C to SML raised exception.\n")
                      ; MLtonExn.topLevelHandler e)
               (* Atomic 0 *)
               val _ = atomicBegin ()
               (* Atomic 1 *)
               val proc = procNum ()
               val _ = Array.update (workerCache, proc, SOME thisWorker)
               val _ = Prim.setSaved (gcState (), valOf (!savedRef))
               val _ = savedRef := NONE
               val _ = Prim.returnToC () (* implicit atomicEnd() *)
            in
               workerLoop thisWorker
            end


         (* val _ = print ("[" ^ Int.toString (procNum()) ^ "] before make worker prototype\n") *)

         val amOriginal = ref true
         val _ = Basic.copyCurrent ()
         val workerLoopPrototypeThread : Basic.p =
            if !amOriginal then
               (amOriginal := false; Basic.savedPre ())
            else
               case Array.sub (workerArgs, procNum ()) of
                 SOME arg =>
                    ( Array.update (workerArgs, procNum (), NONE)
                    ; workerLoop arg
                    ; die "MLton.Thread: bug: C handler worker loop exited unexpectedly\n"
                    )
               | NONE =>
                    die "MLton.Thread: bug: C handler worker loop missing arg\n"

         (* val _ = print ("[" ^ Int.toString (procNum()) ^ "] after make worker prototype\n") *)


         fun mkWorker (): worker =
            let
               (* val _ = print ("[" ^ Int.toString (procNum()) ^ "] before copy prototype for worker\n") *)
               val workerThread = Basic.copy workerLoopPrototypeThread
               (* val _ = print ("[" ^ Int.toString (procNum()) ^ "] after copy prototype for worker\n") *)
               val savedRef : Prim.thread option ref = ref NONE
            in
               (workerThread, savedRef)
            end


         fun handlerLoop (): unit =
            let
               (* Atomic 2 *)
               val proc = procNum ()
               (* val _ = print ("[" ^ Int.toString proc ^ "] handlerLoop\n") *)
               val saved = Prim.saved (gcState ())
               val worker as (workerThread, savedRef) =
                  case Array.sub (workerCache, proc) of
                     NONE => mkWorker ()   (** NOTE: only for nested C -> ML -> C -> *)
                   | SOME worker =>
                        (Array.update (workerCache, proc, NONE)
                         ; worker)
               val _ = refAssignNoBarrier (savedRef, SOME saved)
               val _ = Array.update (workerArgs, proc, SOME worker)
               val _ = Prim.switchTo (workerThread) (* implicit atomicEnd() *)
            in
               handlerLoop ()
            end

         (* val _ = print ("[" ^ Int.toString (procNum()) ^ "] making handler thread: before copy current\n") *)
         val amOriginal = ref true
         val _ = Basic.copyCurrent ()
         val prototypeThread : Basic.p =
            if !amOriginal then
               (amOriginal := false; Basic.savedPre ())
            else
               (* this is executed when we switch to the prototype *)
               ( handlerLoop ()
               ; die "MLton.Thread: bug: C handler loop exited unexpectedly\n"
               )
         (* val _ = print ("[" ^ Int.toString (procNum()) ^ "] making handler thread: after copy current\n") *)

         val handlerThreads =
           Array.tabulate (numProcs, fn _ => Basic.copy prototypeThread)
         val _ = Prim.setCallFromCHandlers (gcState (), handlerThreads)
      in
         fn (i, f) =>
           ( (*print ("register " ^ Int.toString i ^ "\n")
           ;*) Array.update (exports, i, f)
           )
      end
end

(* Redefine switch to use intermediate threads for public use *)
(*
val intermediateThreads: Runnable.t array =
    Array.tabulate (numProcs, fn _ => prepare (new (fn () => ()), ()))

val intermediateSwitchArgs:
    ((unit -> Runnable.t) * (exn -> Runnable.t)) option array =
    Array.array (numProcs, NONE)

val intermediateSwitchLoop: unit -> unit =
 fn () =>
    let
        fun loop ((): unit): unit  =
            let
                (* make sure I am not using a HH *)
                (* val () = (MLtonHM.enterGlobalHeap ();
                          MLtonHM.HierarchicalHeap.setUseHierarchicalHeap false;
                          MLtonHM.exitGlobalHeap ()) *)

                val p = procNum ()

                (* get args *)
                val (x, h): (unit -> Runnable.t) * (exn -> Runnable.t) =
                    valOf (Array.sub (intermediateSwitchArgs, p))
                val () = Array.update (intermediateSwitchArgs, p, NONE)

                (* update intermediateThread for switching *)
                val iT: unit t =
                    T (ref (Interrupted (Prim.current (gcState ()))))
                val () = Array.update (intermediateThreads, p, iT)

                (* get new thread *)
                val t' = x () handle e => h e
            in
                atomicSwitchBottom t';
                (* next switch here *)
                loop ()
            end
    in
        loop ()
    end

val () = Array.modify
             (fn _ => initPrimitive (new intermediateSwitchLoop))
             intermediateThreads

local
    fun cont (arg: (unit -> Runnable.t) * (exn -> Runnable.t)): unit =
        let
            val p = procNum ()
            val t: unit thread ref =
                ref (Interrupted (Prim.current (gcState ())))
            val () = Array.update (intermediateSwitchArgs, p, SOME arg)
            val T iT = Array.sub (intermediateThreads, p)
            val primIT = case !iT before iT := Dead
                          of Dead => raise Fail "switch to a Dead intermediate thread"
                           | Interrupted t => t
                           | New g => raise Fail "switch to a New intermediate thread"
                           | Paused (f, t) => (f (fn () => ()); t)
            val () = Prim.switchTo primIT
        in
            ()
        end
in
fun atomicSwitch f =
    (atomicBegin(); (* add atomic because I'm doing two switches *)
     atomicSwitchTop f cont)

fun switch f =
    (atomicBegin();
     atomicSwitch f)
end
*)

fun switch _ = die "MLton.Thread.switch unsupported\n"
fun atomicSwitch _ = die "MLton.Thread.atomicSwitch unsupported\n"

end
