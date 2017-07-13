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

        fun arrayCompareAndSwap8 (xs, i) (old, new) =
          let val cas = _import "Parallel_arrayCompareAndSwap8" impure private: int array * Int8.int * Int8.int * Int8.int -> Int8.int;
          in Int8.toInt (cas (xs, Int8.fromInt i, Int8.fromInt old, Int8.fromInt new))
          end
        fun arrayCompareAndSwap16 (xs, i) (old, new) =
          let val cas = _import "Parallel_arrayCompareAndSwap16" impure private: int array * Int16.int * Int16.int * Int16.int -> Int16.int;
          in Int16.toInt (cas (xs, Int16.fromInt i, Int16.fromInt old, Int16.fromInt new))
          end
        fun arrayCompareAndSwap32 (xs, i) (old, new) =
          let val cas = _import "Parallel_arrayCompareAndSwap32" impure private: int array * Int32.int * Int32.int * Int32.int -> Int32.int;
          in Int32.toInt (cas (xs, Int32.fromInt i, Int32.fromInt old, Int32.fromInt new))
          end
        fun arrayCompareAndSwap64 (xs, i) (old, new) =
          let val cas = _import "Parallel_arrayCompareAndSwap64" impure private: int array * Int64.int * Int64.int * Int64.int -> Int64.int;
          in Int64.toInt (cas (xs, Int64.fromInt i, Int64.fromInt old, Int64.fromInt new))
          end

        local
          val msg = "MLton.Parallel.Unsafe.arrayCompareAndSwap: "
                  ^ "no implementation for integers with "
          fun errorSize n _ _ = raise Fail (msg ^ "precision " ^ Int.toString n)
          fun errorArbitrary _ _ = raise Fail (msg ^ "arbitrary precision")
        in
        val arrayCompareAndSwap =
          case Int.precision of
            SOME 8 => arrayCompareAndSwap8
          | SOME 16 => arrayCompareAndSwap16
          | SOME 32 => arrayCompareAndSwap32
          | SOME 64 => arrayCompareAndSwap64
          | SOME n => errorSize n
          | NONE => errorArbitrary
        end
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

    fun arrayCompareAndSwap (xs, i) (old, new) =
      if i < 0 orelse i >= Array.length xs
      then raise Subscript
      else Unsafe.arrayCompareAndSwap (xs, i) (old, new)

    (* ========================== compareAndSwap ========================== *)

    fun compareAndSwap8 r (old, new) =
      let val cas = _import "Parallel_compareAndSwap8" impure private: int ref * Int8.int * Int8.int -> Int8.int;
      in Int8.toInt (cas (r, Int8.fromInt old, Int8.fromInt new))
      end
    fun compareAndSwap16 r (old, new) =
      let val cas = _import "Parallel_compareAndSwap16" impure private: int ref * Int16.int * Int16.int -> Int16.int;
      in Int16.toInt (cas (r, Int16.fromInt old, Int16.fromInt new))
      end
    fun compareAndSwap32 r (old, new) =
      let val cas = _import "Parallel_compareAndSwap32" impure private: int ref * Int32.int * Int32.int -> Int32.int;
      in Int32.toInt (cas (r, Int32.fromInt old, Int32.fromInt new))
      end
    fun compareAndSwap64 r (old, new) =
      let val cas = _import "Parallel_compareAndSwap64" impure private: int ref * Int64.int * Int64.int -> Int64.int;
      in Int64.toInt (cas (r, Int64.fromInt old, Int64.fromInt new))
      end

    local
      val msg = "MLton.Parallel.compareAndSwap: "
              ^ "no implementation for integers with "
      fun errorSize n _ _ = raise Fail (msg ^ "precision " ^ Int.toString n)
      fun errorArbitrary _ _ = raise Fail (msg ^ "arbitrary precision")
    in
    val compareAndSwap =
      case Int.precision of
        SOME 8 => compareAndSwap8
      | SOME 16 => compareAndSwap16
      | SOME 32 => compareAndSwap32
      | SOME 64 => compareAndSwap64
      | SOME n => errorSize n
      | NONE => errorArbitrary
    end

    (* ========================== fetchAndAdd ========================== *)

    fun fetchAndAdd8 r d =
      let val faa = _import "Parallel_fetchAndAdd8" impure private: int ref * Int8.int -> Int8.int;
      in Int8.toInt (faa (r, Int8.fromInt d))
      end
    fun fetchAndAdd16 r d =
      let val faa = _import "Parallel_fetchAndAdd16" impure private: int ref * Int16.int -> Int16.int;
      in Int16.toInt (faa (r, Int16.fromInt d))
      end
    fun fetchAndAdd32 r d =
      let val faa = _import "Parallel_fetchAndAdd32" impure private: int ref * Int32.int -> Int32.int;
      in Int32.toInt (faa (r, Int32.fromInt d))
      end
    fun fetchAndAdd64 r d =
      let val faa = _import "Parallel_fetchAndAdd64" impure private: int ref * Int64.int -> Int64.int;
      in Int64.toInt (faa (r, Int64.fromInt d))
      end

    local
      val msg = "MLton.Parallel.fetchAndAdd: "
              ^ "no implementation for integers with "
      fun errorSize n _ _ = raise Fail (msg ^ "precision " ^ Int.toString n)
      fun errorArbitrary _ _ = raise Fail (msg ^ "arbitrary precision")
    in
    val fetchAndAdd =
      case Int.precision of
        SOME 8 => fetchAndAdd8
      | SOME 16 => fetchAndAdd16
      | SOME 32 => fetchAndAdd32
      | SOME 64 => fetchAndAdd64
      | SOME n => errorSize n
      | NONE => errorArbitrary
    end

    (* shwestrick: needed these for private-deqs scheduler *)
    (* val compareAndSwap = _import "Parallel_compareAndSwap" impure private: Int32.int ref * Int32.int * Int32.int -> bool; *)
    (* val fetchAndAdd = _import "Parallel_fetchAndAdd" impure private: Int32.int ref * Int32.int -> Int32.int; *)
  end
