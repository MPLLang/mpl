(* Copyright (C) 2009,2022 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PROCESS_EXTRA =
   sig
      include PROCESS

      structure Command:
         sig
            type t = In.t * Out.t -> unit
         end

      structure Status:
         sig
            type t
         end

      (* Execute a program in a subprocess and wait for it to finish.
       * call (file, args) (i, o) searches PATH for an executable named file,
       * and runs it with arguments file :: args.
       *)
      val call: string * string list -> Command.t
      (* call' (c, a) = call (c, a) (In.standard, Out.standard) *)
      val call': string * string list -> unit
      (*
       * Fork off a command and collect its output into a string.
       *)
      val collect: Command.t -> string
      val commandName: unit -> string
      val doesSucceed: (unit -> unit) -> bool
      val doubleFork: (unit -> unit) -> unit
      val exec: string * string list -> unit
      (* Start a command in a subprocess, in the background. *)
      val fork: (unit -> unit) -> Pid.t
      val forkIn: (Out.t -> unit) -> Pid.t * In.t
      val forkOut: (In.t -> unit) -> Pid.t * Out.t
      val forkInOut: (In.t * Out.t -> unit) -> Pid.t * In.t * Out.t
      val getEnv: string -> string option
      (*
       * glob s returns the list of paths matching s.
       * For now, s should be a bash pattern.
       *)
      val glob: string -> string list
      val hostName: unit -> string
      (* pipe [c_1, ..., c_n] runs the commands c_1, ..., c_n in
       * subprocesses in parallel, with the standard output of c_i hooked
       * to the standard input of c_i+1.
       * Fails if any of the commands fail.
       *)
      val pipe: Command.t list * In.t * Out.t -> unit
      (* pipe' cs = pipe (cs, In.standard, Out.standard) *)
      val pipe': Command.t list -> unit
      (* run = waitChildPid o fork *)
      val run: (unit -> unit) -> unit
      val setEnv: {name: string, value: string} -> unit
      val signal: Pid.t * Signal.t -> unit
      val signalGroup: Pid.t * Signal.t -> unit
      val size: File.t -> {text: int, data: int, bss: int}
      val su: string -> unit (* string is userid *)
      (* try (f, m) tries f with exponentially backed off times, stopping after
       * a minute of trying, in which case is fails with m.
       *)
      val try: (unit -> 'a) * string -> 'a
      val userName: unit -> string
      (* watch f will rerun f until it succeeds *)
      val watch: (unit -> unit) -> unit

      structure State:
         sig
            datatype t = DiskSleep | Running | Sleeping | Traced | Zombie

            val toString: t -> string
         end

      val ps: unit -> {name: string,
                       pgrp: Pid.t,
                       pid: Pid.t,
                       ppid: Pid.t,
                       state: State.t} list

   end

functor TestProcessExtra (S: PROCESS_EXTRA): sig end =
struct

val _ = print "TestProcess\n"

open S

val _ = ps ()

end
