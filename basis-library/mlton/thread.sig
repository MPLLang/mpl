(* Copyright (C) 2022 Matthew Fluet.
 * Copyright (C) 2019 Sam Westrick
 * Copyright (C) 2004-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_THREAD =
   sig
      structure AtomicState :
         sig
            datatype t = NonAtomic | Atomic of int
         end
      val atomically: (unit -> 'a) -> 'a
      val atomicBegin: unit -> unit
      val atomicEnd: unit -> unit
      val atomicState: unit -> AtomicState.t

      structure Runnable :
         sig
            type t
         end

      structure Basic :
        sig
          type p (* prethread *)
          type t (* primitive thread *)

          val copyCurrent : unit -> unit
          val current : unit -> t
          val savedPre : unit -> p
          val copy : p -> t
          val switchTo : t -> unit

          val atomicState : unit -> Word32.word
          val atomicBegin : unit -> unit
          val atomicEnd : unit -> unit
        end

      structure HierarchicalHeap :
        sig
          type thread = Basic.t
          type clear_set
          type finished_clear_set_grain

          (* The level (depth) of a thread's heap in the hierarchy. *)
          val getDepth : thread -> int
          val setDepth : thread * int -> unit
          val setMinLocalCollectionDepth : thread * int -> unit

          (*force the runtime to create a hh for the left child*)
          val forceLeftHeap : int * thread -> unit

          val forceNewChunk : unit -> unit

          val registerCont : 'a ref * 'b ref * 'c ref * thread -> bool
          val resetList    : thread -> unit

          (*Collect the depth = 1 HH of this thread*)
          val cancelCC: thread * Word64.word -> unit
          val collectThreadRoot : thread * Word64.word -> unit
          val getRoot : thread -> Word64.word


          (* Merge the heap of the deepest child of this thread. Requires that
           * this child is inactive and has an associated heap. *)
          val mergeThreads : thread * thread -> unit

          (* Move all chunks at the current depth up one level. *)
          val promoteChunks : thread -> unit

          val clearSuspectsAtDepth: thread * int -> unit
          val numSuspectsAtDepth: thread * int -> int
          val takeClearSetAtDepth: thread * int -> clear_set
          val numChunksInClearSet: clear_set -> int
          val processClearSetGrain: clear_set * int * int -> finished_clear_set_grain
          val commitFinishedClearSetGrain: thread * finished_clear_set_grain -> unit
          val deleteClearSet: clear_set -> unit

          val updateBytesPinnedEntangledWatermark: unit -> unit

          (* "put a new thread in the hierarchy *)
          val moveNewThreadToDepth : thread * Word64.word * int -> unit

          val checkFinishedCCReadyToJoin: unit -> bool

          val canForkThread: thread -> bool

          (* second arg is joinpoint *)
          (* val forkThread: thread * 'a ref -> Basic.p *)

          val joinIntoParentBeforeFastClone:
            { thread: thread
            , newDepth: int
            , tidLeft: Word64.word
            , tidRight: Word64.word
            }
            -> unit

          val joinIntoParent:
            { thread: thread
            , rightSideThread: thread
            , newDepth: int
            , tidLeft: Word64.word
            , tidRight: Word64.word
            }
            -> unit
        end

      (* disentanglement checking *)
      structure Disentanglement :
        sig
          type thread = Basic.t

          val decheckMaxDepth: unit -> int option

          (* fork the current thread ID, returning the two child IDs *)
          val decheckFork : unit -> Word64.word * Word64.word

          (* join two child IDs and update the current thread ID *)
          val decheckJoin : Word64.word * Word64.word -> unit

          (* set the current thread ID *)
          val decheckSetTid : Word64.word -> unit

          (* get the current thread ID of a thread *)
          val decheckGetTid : thread -> Word64.word

          (* arguments are (victim thread, steal depth) *)
          val copySyncDepthsFromThread : thread * thread * int -> unit
        end

      type 'a t

      (* atomicSwitch f
       * as switch, but assumes an atomic calling context.  Upon
       * switch-ing back to the current thread, an implicit atomicEnd is
       * performed.
       *)
      val atomicSwitch: ('a t -> Runnable.t) -> 'a
      (* new f
       * create a new thread that, when run, applies f to
       * the value given to the thread.  f must terminate by
       * switch-ing to another thread or exiting the process.
       *)
      val new: ('a -> unit) -> 'a t
      (* prepend(t, f)
       * create a new thread (destroying t in the process) that first
       * applies f to the value given to the thread and then continues
       * with t.  This is a constant time operation.
       *)
      val prepend: 'a t * ('b -> 'a) -> 'b t
      (* prepare(t, v)
       * create a new runnable thread (destroying t in the process)
       * that will evaluate t on v.
       *)
      val prepare: 'a t * 'a -> Runnable.t
      (* switch f
       * apply f to the current thread to get rt, and then start
       * running thread rt.  It is an error for f to
       * perform another switch.  f is guaranteed to run
       * atomically.
       *)
      val switch: ('a t -> Runnable.t) -> 'a
   end

signature MLTON_THREAD_EXTRA =
   sig
      include MLTON_THREAD

      val amInSignalHandler: unit -> bool
      val register: int * (MLtonPointer.t -> unit) -> unit
      val setSignalHandler: (Runnable.t -> Runnable.t) -> unit
      val setSimpleSignalHandler: (Basic.t -> unit) -> unit
      val switchToSignalHandler: unit -> unit

      val initPrimitive: unit t -> Runnable.t
   end
