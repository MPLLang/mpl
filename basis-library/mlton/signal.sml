(* Copyright (C) 2015,2019 Matthew Fluet.
 * Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonSignal: MLTON_SIGNAL_EXTRA =
struct

fun die (s: string): 'a =
   (PrimitiveFFI.Stdio.print s
    ; PrimitiveFFI.Posix.Process.exit 1
    ; let exception DieFailed
      in raise DieFailed
      end)

open Posix.Signal
structure Prim = PrimitiveFFI.Posix.Signal
structure GCState = Primitive.MLton.GCState
structure Error = PosixError
structure SysCall = Error.SysCall
val restart = SysCall.restartFlag

type t = signal

type how = C_Int.t

fun raiseInval () =
   let
      open PosixError
   in
      raiseSys inval
   end

structure Mask =
   struct
      type pre_sig_set = Word8.word array
      type sig_set = Word8.word vector
      fun newSigSet (): (pre_sig_set * (unit -> sig_set)) =
         let
            val sslen = C_Size.toInt Prim.sigSetLen
            val ss = Array.array (sslen, 0wx0: Word8.word)
         in
            (ss, fn () => Array.vector ss)
         end

      type t = sig_set

      fun allBut sigs =
         let
            val (ss, finish) = newSigSet ()
            val () = SysCall.simple (fn () => Prim.sigfillset ss)
            val () = List.app (fn s => SysCall.simple
                                       (fn () => Prim.sigdelset (ss, toRep s)))
                              sigs
         in
            finish ()
         end
      val all = allBut []
      fun some sigs =
         let
            val (ss, finish) = newSigSet ()
            val () = SysCall.simple (fn () => Prim.sigemptyset ss)
            val () = List.app (fn s => SysCall.simple
                                       (fn () => Prim.sigaddset (ss, toRep s)))
                              sigs
         in
            finish ()
         end
      val none = some []

      fun isMember (ss, s) =
         SysCall.simpleResult (fn () => Prim.sigismember (ss, toRep s)) <> C_Int.zero

      local
         fun make (how: how) (ss: t) =
            let
               val (oss, finish) = newSigSet ()
               val () = SysCall.simpleRestart (fn () => Prim.sigprocmask (how, ss, oss))
            in
               finish ()
            end
      in
         val block = ignore o make Prim.SIG_BLOCK
         val unblock = ignore o make Prim.SIG_UNBLOCK
         val setBlocked = ignore o make Prim.SIG_SETMASK
         fun getBlocked () = make Prim.SIG_BLOCK none
      end
   end

structure Handler =
   struct
      datatype t =
         Default
       (* | Handler of MLtonThread.Runnable.t -> MLtonThread.Runnable.t *)
       (* | Handler of unit -> unit *)
       | Handler of MLtonThread.Basic.t -> unit
       | Ignore
       | InvalidSignal
   end

datatype handler = datatype Handler.t

local
  (* SPOONHOWER_NOTE: spoons signals use global shared state *)
   val r = ref C_Int.zero
in
   fun initHandler (s: signal): Handler.t =
      SysCall.syscallErr
      ({clear = false, restart = false, errVal = C_Int.fromInt ~1}, fn () =>
       {return = Prim.isDefault (toRep s, r),
        post = fn _ => if !r <> C_Int.zero then Default else Ignore,
        handlers = [(Error.inval, fn () => InvalidSignal)]})
end

val (getHandler, setHandler, handlers) =
   let
      val handlers = Array.tabulate (C_Int.toInt Prim.NSIG, initHandler o fromInt)
      val _ =
         Cleaner.addNew
         (Cleaner.atLoadWorld, fn () =>
          Array.modifyi (initHandler o fromInt o #1) handlers)
   in
      (fn s: t => Array.sub (handlers, toInt s),
       fn (s: t, h) => if Primitive.MLton.Profile.isOn andalso s = prof
                          then raiseInval ()
                       else Array.update (handlers, toInt s, h),
       handlers)
   end

(* SPOONHOWER_NOTE: spoons signal use global shared state *)
val gcHandler = ref Ignore

fun handled () =
   Mask.some
   (Array.foldri
    (fn (s, h, sigs) =>
     case h of 
        Handler _ => (fromInt s)::sigs
      | _ => sigs) [] handlers)

structure Handler =
   struct
      open Handler

      val default = Default
      val ignore = Ignore

      val isDefault = fn Default => true | _ => false
      val isIgnore = fn Ignore => true | _ => false

      val handler_ =
         (* This let is used so that Thread.setHandler is only used if
          * Handler.handler is used.  This prevents threads from being part
          * of every program.
          *)
         let
            (* As far as C is concerned, there is only one signal handler.
             * As soon as possible after a C signal is received, this signal
             * handler walks over the array of all SML handlers, and invokes any
             * one for which a C signal has been received.
             *
             * Any exceptions raised by a signal handler will be caught by
             * the topLevelHandler, which is installed in thread.sml.
             *)
            val _ =
               PosixError.SysCall.blocker :=
               (fn () => let
                            val m = Mask.getBlocked ()
                            val () = Mask.block (handled ())
                         in
                            fn () => Mask.setBlocked m
                         end)


            fun normalHandler t =
              let
                val mask = Mask.getBlocked ()
                val () = Mask.block (handled ())
                val fs = 
                  case !gcHandler of
                      Handler f => if Prim.isPendingGC (GCState.gcState ()) <> C_Int.zero
                                      then [f] 
                                      else []
                    | _ => []
                val fs =
                  Array.foldri
                  (fn (s, h, fs) =>
                    case h of
                      Handler f =>
                          if Prim.isPending (GCState.gcState (), repFromInt s) <> C_Int.zero
                            then f::fs 
                            else fs
                    | _ => fs) fs handlers
                val () = Prim.resetPending (GCState.gcState ())
                val () = Mask.setBlocked mask
              in
                (* List.foldl (fn (f, t) => f t) t fs *)
                List.app (fn f => f t) fs
              end

            
            (* SAM_NOTE: TODO: Check with Matthew. This is probably subtly
             * incorrect.
             *
             * The goal is to do a "fast path" for the signal handling code.
             * The common case is that just a single signal handler needs to
             * run, and we should be able to find this handler without any
             * intermediate allocation if possible.
             *)
            fun maybeFastHandler t =
              let
                val n = Array.length handlers
                fun loop f count i =
                  if i >= n then (f, count)
                  else
                  case Array.sub (handlers, i) of
                    Handler g =>
                      if Prim.isPending (GCState.gcState (), repFromInt i) <> C_Int.zero
                      then loop g (count+1) (i+1)
                      else loop f count (i+1)

                  | _ => loop f count (i+1)
              in
                case loop (fn t => ()) 0 0 of
                  (f, 1) =>
                    (* fast path succeeds if we find just a single handler that
                     * needs to be run.
                     *)
                    let
                      val _ = Prim.resetPending (GCState.gcState ())
                    in
                      f t
                    end

                | _ => normalHandler t
              end


            val () = MLtonThread.setSimpleSignalHandler maybeFastHandler
            (* val () = MLtonThread.setSimpleSignalHandler normalHandler *)
         in
            Handler
         end

      fun handler _ = raise Fail "Signal.Handler.handler not supported"

      fun simple (f: unit -> unit) = handler_ (fn t => f ())

      fun inspectInterrupted f = handler_ f
   end

val setHandler = fn (s, h) =>
   case (getHandler s, h) of
      (InvalidSignal, _) => raiseInval ()
    | (_, InvalidSignal) => raiseInval ()
    | (Default, Default) => ()
    | (_, Default) => 
         (setHandler (s, Default)
          ; SysCall.simpleRestart (fn () => Prim.default (GCState.gcState (), toRep s)))
    | (Handler _, Handler _) =>
         setHandler (s, h)
    | (_, Handler _) =>
         (setHandler (s, h)
          ; SysCall.simpleRestart (fn () => Prim.handlee (GCState.gcState (), toRep s)))
    | (Ignore, Ignore) => ()
    | (_, Ignore) => 
         (setHandler (s, Ignore)
          ; SysCall.simpleRestart (fn () => Prim.ignore (GCState.gcState (), toRep s)))

fun suspend m =
   (Prim.sigsuspend m
    ; MLtonThread.switchToSignalHandler ())

fun handleGC f =
   ( ignore (die ("MLton.Signal.handleGC unsupported\n"))
    ; Prim.handleGC (GCState.gcState ())
    ; gcHandler := Handler.simple f)

end
