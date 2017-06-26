(* Copyright (C) 2017 Ram Raghunathan
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonParallel:> MLTON_PARALLEL =
  struct
    structure Prim = Primitive.MLton.Parallel

    structure Deprecated =
      struct
        fun printDepMsg fName =
            (TextIO.output (TextIO.stdErr,
                            String.concat["MLton.Parallel.Deprecated.",
                                          fName,
                                          " will be removed in a later ",
                                          "release\n"]);
             TextIO.flushOut TextIO.stdErr)

        fun msgWrapper1 (fName, f) =
            let
              val r = ref false
            in
              fn x =>
                 ((if not (!r)
                   then (printDepMsg fName;
                         r := true)
                   else ());
                  f x)
            end


        val yield: unit -> unit =
            (*msgWrapper1 ("yield", *)
                         _import "Parallel_yield" runtime private:
                         unit -> unit;(*)*)

        val lockInit: Word32.word ref -> unit =
            (*msgWrapper1 ("lockInit", *)
                         _import "Parallel_lockInit" runtime private:
                         Word32.word ref -> unit;(*)*)

        val takeLock: Word32.word ref -> unit =
            (*msgWrapper1 ("takeLock", *)
                         _import "Parallel_lockTake" runtime private:
                         Word32.word ref -> unit;(*)*)

        val releaseLock: Word32.word ref -> unit =
            (*msgWrapper1 ("releaseLock", *)
                         _import "Parallel_lockRelease" runtime private:
                         Word32.word ref -> unit;(*) *)
      end

    structure Unsafe =
      struct
        val initPrimitiveThread: unit MLtonThread.t -> MLtonThread.Runnable.t =
            MLtonThread.initPrimitive
        val arrayUninit: int -> 'a Array.array =
            Array.arrayUninit
        val arrayCompareAndSwap = _import "Parallel_arrayCompareAndSwap" impure private: Int32.int array * Int32.int * Int32.int * Int32.int -> bool;
      end

    exception Return

    val numberOfProcessors: int = Int32.toInt Prim.numberOfProcessors
    val processorNumber: unit -> int = Int32.toInt o Prim.processorNumber

    (* This should really be a non-returning function, so wrap it in a raise *)
    fun registerProcessorFunction (f: unit -> unit): unit =
        let
          val f' = fn () => (f (); raise Return)
        in
          (_export "Parallel_run": (unit -> unit) -> unit;) f'
        end

    val initializeProcessors: unit -> unit =
        _import "Parallel_init" runtime private: unit -> unit;

    fun arrayCompareAndSwap (xs, i, old, new) =
      if i < 0 orelse i >= Array.length xs
      then raise Subscript
      else Unsafe.arrayCompareAndSwap (xs, i, old, new)

    (* shwestrick: needed these for private-deqs scheduler *)
    val compareAndSwap = _import "Parallel_compareAndSwap" impure private: Int32.int ref * Int32.int * Int32.int -> bool;
    val fetchAndAdd = _import "Parallel_fetchAndAdd" impure private: Int32.int ref * Int32.int -> Int32.int;
  end
