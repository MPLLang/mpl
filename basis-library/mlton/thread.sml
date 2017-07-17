(* Copyright (C) 2014 Matthew Fluet.
 * Copyright (C) 2004-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonThread:> MLTON_THREAD_EXTRA =
struct

structure Prim = Primitive.MLton.Thread

fun die (s: string): 'a =
   (PrimitiveFFI.Stdio.print s
    ; PrimitiveFFI.Posix.Process.exit 1
    ; let exception DieFailed
      in raise DieFailed
      end)

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

val numProcs = Int32.toInt Primitive.MLton.Parallel.numberOfProcessors
val procNum = Int32.toInt o Primitive.MLton.Parallel.processorNumber

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
                    NONE => Prim.savedPre ()
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

                    val curPrimThread = Prim.current ()

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

local
   val signalHandlers: Prim.thread option array = Array.array (numProcs, NONE)
   datatype state = Normal | InHandler
   val state: state array = Array.array (numProcs, Normal)
in
   fun amInSignalHandler () = InHandler = Array.sub (state, procNum ())

   fun setSignalHandler (f: Runnable.t -> Runnable.t): unit =
      let
         val _ = Primitive.MLton.installSignalHandler ()
         fun loop (): unit =
            let
               (* Atomic 1 *)
               val proc = procNum ()
               val _ = Array.update (state, proc, InHandler)
               val t = f (fromPrimitive (Prim.saved ()))
               val _ = Array.update (state, proc, Normal)
               val _ = Prim.finishSignalHandler ()
               val _ =
                  atomicSwitch
                  (fn (T r) =>
                   let
                      val _ =
                         case !r of
                            Paused (f, _) => f (fn () => ())
                          | _ => raise die "Thread.setSignalHandler saw strange thread"
                   in
                      t
                   end) (* implicit atomicEnd () *)
            in
               loop ()
            end
         val handlerThreads =
            Array.tabulate
            (numProcs, fn i =>
             let
                val p =
                   toPrimitive (new (fn () => loop () handle e => MLtonExn.topLevelHandler e))
                val _ = Array.update (signalHandlers, i, SOME p)
             in
                p
             end)
      in
         Prim.setSignalHandlers handlerThreads
      end

   fun switchToSignalHandler () =
      let
         (* Atomic 0 *)
         val () = atomicBegin ()
         (* Atomic 1 *)
         val () = Prim.startSignalHandler () (* implicit atomicBegin () *)
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
         val worker : (Prim.thread * Prim.thread option ref) option array = Array.array (numProcs, NONE)
         fun mkWorker (): Prim.thread * Prim.thread option ref =
            let
               val thisWorker : (Prim.thread * Prim.thread option ref) option ref = ref NONE
               val savedRef : Prim.thread option ref = ref NONE
               fun workerLoop () =
                  let
                     (* Atomic 1 *)
                     val p = Primitive.MLton.FFI.getArgs ()
                     val _ = atomicEnd ()
                     (* Atomic 0 *)
                     val i = MLtonPointer.getInt32 (MLtonPointer.getPointer (p, 0), 0)
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
                     val _ = Array.update (worker, proc, !thisWorker)
                     val _ = Prim.setSaved (valOf (!savedRef))
                     val _ = savedRef := NONE
                     val _ = Prim.returnToC () (* implicit atomicEnd() *)
                  in
                     workerLoop ()
                  end
               val workerThread = toPrimitive (new workerLoop)
               val _ = thisWorker := SOME (workerThread, savedRef)
            in
               (workerThread, savedRef)
            end
         fun handlerLoop (): unit =
            let
               (* Atomic 2 *)
               val proc = procNum ()
               val saved = Prim.saved ()
               val (workerThread, savedRef) =
                  case Array.sub (worker, proc) of
                     NONE => mkWorker ()
                   | SOME (workerThread, savedRef) =>
                        (Array.update (worker, proc, NONE)
                         ; (workerThread, savedRef))
               val _ = savedRef := SOME saved
               val _ = Prim.switchTo (workerThread) (* implicit atomicEnd() *)
            in
               handlerLoop ()
            end
         val handlerThreads = Array.tabulate (numProcs, fn _ => toPrimitive (new handlerLoop))
         val _ = Prim.setCallFromCHandlers handlerThreads
      in
         fn (i, f) => Array.update (exports, i, f)
      end
end

(* Redefine switch to use intermediate threads for public use *)
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
                val () = (MLtonHM.enterGlobalHeap ();
                          MLtonHM.HierarchicalHeap.setUseHierarchicalHeap false;
                          MLtonHM.exitGlobalHeap ())

                val p = procNum ()

                (* get args *)
                val (x, h): (unit -> Runnable.t) * (exn -> Runnable.t) =
                    valOf (Array.sub (intermediateSwitchArgs, p))
                val () = Array.update (intermediateSwitchArgs, p, NONE)

                (* update intermediateThread for switching *)
                val iT: unit t =
                    T (ref (Interrupted (Prim.current ())))
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
                ref (Interrupted (Prim.current ()))
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

end
