(* Copyright (C) 2017,2022 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Process: PROCESS =
struct

fun system s =
   let
      val status = OS.Process.system s
   in
      if OS.Process.isSuccess status
         then ()
      else Error.bug (concat ["Process.system: command failed: ", s])
   end

structure Pid = Pid

structure PosixStatus =
   struct
      open Posix.Process

      type t = exit_status

      fun toString (s: t): string =
         case s of
            W_EXITED => "exited"
          | W_EXITSTATUS w => concat ["exit status ", Word8.toString w]
          | W_SIGNALED s => concat ["signal ",
                                    SysWord.toString (Posix.Signal.toWord s)]
          | W_STOPPED s => concat ["stop signal ",
                                   SysWord.toString (Posix.Signal.toWord s)]

      val layout = Layout.str o toString
   end

fun succeed (): 'a =
   let open OS.Process
   in exit success
   end

val succeed = Trace.trace ("Process.succeed", Unit.layout, Unit.layout) succeed

(* This song and dance is so that succeed can have the right type, unit -> 'a,
 * instead of unit -> unit.
 *)
val succeed: unit -> 'a = fn () => (succeed (); Error.bug "Process.succeed")

fun fail x = raise Fail x


fun waitChildPid (p: Pid.t): unit =
   let
      val (p', s) = Posix.Process.waitpid (Posix.Process.W_CHILD p, [])
      val _ =
         if p <> p'
            then Error.bug (concat ["Process.wait: expected pid ",
                                    Pid.toString p,
                                    " but got pid ",
                                    Pid.toString p'])
            else ()
   in
      case s of
         PosixStatus.W_EXITED => ()
       | _ => raise Fail (concat [PosixStatus.toString s])
   end

val waitChildPid =
   Trace.trace ("Process.waitChildPid", Pid.layout, Unit.layout) waitChildPid

structure Posix =
   struct
      open Posix
      structure Process =
         struct
            open Process

            val wait =
               Trace.trace ("Process.Posix.Process.wait", Unit.layout,
                           Layout.tuple2 (Pid.layout, PosixStatus.layout))
               wait
         end
   end

fun waits (pids: Pid.t list): unit =
   case pids of
      [] => ()
    | _ =>
         let
            val (pid, status) = Posix.Process.wait ()
            val pids =
               case status of
                  Posix.Process.W_EXITED =>
                     List.keepAll (pids, fn p => p <> pid)
                | _ => Error.bug (concat ["Process.waits: child ",
                                          Pid.toString pid,
                                          " failed with ",
                                          PosixStatus.toString status])
         in waits pids
         end

fun executeWith (name, args, f: In.t * Out.t -> 'a) =
   let
      val pid = Unix.execute (name, args)
      val ins = Unix.textInstreamOf pid
      val out = Unix.textOutstreamOf pid
   in
      Exn.finally
      (fn () => f (ins, out),
       fn () => ignore (Unix.reap pid))
   end

fun executeWithIn (name, args, f: In.t -> 'a) =
   executeWith (name, args, fn (ins, _) => f ins)
fun executeWithOut (name, args, f: Out.t -> 'a) =
   executeWith (name, args, fn (_, out) => f out)
fun execute (name, args) =
   executeWith (name, args, fn _ => ())

local
   open MLton.Process
in
   val spawn = spawn
   val spawne = spawne
   val spawnp = spawnp
end

fun time (f: unit -> unit) =
   let
      val {children = {utime = u, stime = s}, ...} = Time.times ()
      val _ = f ()
      val {children = {utime = u', stime = s'}, ...} = Time.times ()
   in
      {system = Time.- (s', s), user = Time.- (u', u)}
   end

fun fork (c: unit -> unit): Pid.t =
   case Posix.Process.fork () of
      NONE => (Trace.Immediate.inChildProcess ()
               ; let open OS.Process
                 in exit ((c (); success) handle _ => failure)
                 end)
    | SOME pid => pid

val fork = Trace.trace ("Process.fork", fn _ => Layout.str "<thunk>", Pid.layout) fork

val getEnv = Posix.ProcEnv.getenv

fun readIntegerEnvironmentVariable (varName, default) =
  case getEnv varName of
      NONE => default
    | SOME s =>
      case Int.fromString s of
          NONE => default
        | SOME i => i

fun numberOfMLtonJobs () =
  readIntegerEnvironmentVariable ("MLTON_JOBS", 1)

fun foreachPar (numProcs, l, f) =
  let
     val arr = Array.fromVector (Vector.fromList l)
     val len = Array.length arr
     val itemsPerProc = Int.max (1, Int.div (len, numProcs))

     fun doFork start =
       let
          fun doIt i = f (Array.sub (arr, i))
          val range = Int.min (itemsPerProc, len - start)
       in
          fork (fn () => Int.for (start, start + range, doIt))
       end

     fun loop (pids, start) =
       if start >= len
       then pids
       else loop (doFork start :: pids, start + itemsPerProc)
  in
     if numProcs > 1 then Array.shuffle arr else ();
     waits (loop ([], 0))
  end
end
