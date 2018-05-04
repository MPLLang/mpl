(* Copyright (C) 2017 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_PARALLEL =
  sig
    (**
     * Deprecated functions. Using these functions will print out a warning
     * message that they may be removed in a future release.
     *)
    structure Deprecated:
              sig
                (**
                 * Yields to the runtime
                 *)
                val yield: unit -> unit

                (**
                 * Initializes a lock to the "unlocked" state
                 *)
                val lockInit: Word32.word ref -> unit;

                (**
                 * Locks the lock.
                 *
                 * @attention Does not support recursive locking or any kind of
                 * deadlock detection!
                 *)
                val takeLock: Word32.word ref -> unit;

                (**
                 * Unlocks the locks
                 *
                 * @attantion Does not enforce that the locking thread has to be
                 * the unlocker!
                 *)
                val releaseLock: Word32.word ref -> unit;
              end

    (*
     * RAM_NOTE: Perhaps some of these should be in module-specific Unsafe
     * structures?
     *)
    structure Unsafe:
              sig
                (**
                 * Forces creation of a runtime thread object from a
                 * MLton.Thread.
                 *
                 * @note See initPrimitive in thread.sml for more information.
                 *)
                val initPrimitiveThread:
                    unit MLtonThread.t -> MLtonThread.Runnable.t

                (**
                 * Creates an uninitialized array of given size.
                 *)
                val arrayUninit: int -> 'a Array.array

                (**
                 * `arrayCompareAndSwap (xs, i) (old, new)` performs a CAS
                 * of (old, new) at xs[i], returning the previous value stored
                 * at xs[i]. This implementation does not check bounds on i.
                 *)
                val arrayCompareAndSwap : int array * int -> int * int -> int

                (**
                 * `arrayFetchAndAdd (xs, i) d` atomically does `xs[i] := xs[i] + d` and
                 * returns the value of `xs[i]` before the add. This implementation
                 * does not check bounds on i.
                 *)
                val arrayFetchAndAdd : int array * int -> int -> int
              end

    exception Return

    (**
     * The number of processors available in the system
     *)
    val numberOfProcessors: int

    (**
     * Returns the processor number of the caller.
     *
     * @return The processor number, which is zero-based.
     *)
    val processorNumber: unit -> int

    (**
     * Registers a function for the non-primary processors to run.
     *
     * @param The function for them to run. While it is typed as a unit -> unit,
     * it should not ever return. Doing so raises Return.
     *)
    val registerProcessorFunction: (unit -> unit) -> unit

    (**
     * Initializes the non-primary processors.
     *
     * @attention: You <em>must</em> register a processor function with
     * registerProcessorFunction before calling this function!
     *)
    val initializeProcessors: unit -> unit;

    (**
     * `compareAndSwap r (old, new)` atomically does `r := new`, but only if
     * `!r = old`. Returns the value of `!r` before the swap.
     *)
    val compareAndSwap : int ref -> int * int -> int

    (**
     * `realCompareAndSwap r (old, new)` atomically does `r := new`, but only if
     * `!r = old`. Returns the value of `!r` before the swap.
     *)
    val realCompareAndSwap : real ref -> real * real -> real

    (**
     * `arrayCompareAndSwap (xs, i) (old, new)` performs a CAS
     * of (old, new) at xs[i], returning the previous value stored at xs[i].
     * If i is out of bounds, it raises Subscript.
     *)
    val arrayCompareAndSwap : int array * int -> int * int -> int

    (**
     * `realArrayCompareAndSwap (xs, i) (old, new)` performs a CAS
     * of (old, new) at xs[i], returning the previous value stored at xs[i].
     * If i is out of bounds, it raises Subscript.
     *)
    val realArrayCompareAndSwap : real array * int -> real * real -> real

    (**
     * `fetchAndAdd r d` atomically does `r := !r + d` and returns the value of
     * `!r` before the add.
     *)
    val fetchAndAdd : int ref -> int -> int

    (**
     * `arrayFetchAndAdd (xs, i) d` atomically does `xs[i] := xs[i] + d` and
     * returns the value of `xs[i]` before the add. If i is out of bounds, it
     * raises Subscript.
     *)
    val arrayFetchAndAdd : int array * int -> int -> int
  end
