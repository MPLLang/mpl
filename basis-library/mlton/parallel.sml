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

        (* ======================== arrayCompareAndSwap ==================== *)

        local
           structure I =
              Int_ChooseInt
              (type 'a t = 'a array * SeqIndex.int * 'a * 'a -> 'a
               val fInt8 = _import "Parallel_arrayCompareAndSwap8" impure private: Int8.t array * SeqIndex.int * Int8.int * Int8.int -> Int8.int;
               val fInt16 = _import "Parallel_arrayCompareAndSwap16" impure private: Int16.t array * SeqIndex.int * Int16.int * Int16.int -> Int16.int;
               val fInt32 = _import "Parallel_arrayCompareAndSwap32" impure private: Int32.t array * SeqIndex.int * Int32.int * Int32.int -> Int32.int;
               val fInt64 = _import "Parallel_arrayCompareAndSwap64" impure private: Int64.t array * SeqIndex.int * Int64.int * Int64.int -> Int64.int;
               val fIntInf = fn _ => raise Fail "MLton.Parallel.Unsafe.arrayCompareAndSwap: IntInf")
        in
           fun arrayCompareAndSwap (xs, i) (old, new) =
              I.f (xs, SeqIndex.fromInt i, old, new)
        end

        (* ========================= arrayFetchAndAdd ========================= *)

        local
           structure I =
              Int_ChooseInt
              (type 'a t = 'a array * SeqIndex.int * 'a -> 'a
               val fInt8 = _import "Parallel_arrayFetchAndAdd8" impure private: Int8.t array * SeqIndex.int * Int8.int -> Int8.int;
               val fInt16 = _import "Parallel_arrayFetchAndAdd16" impure private: Int16.t array * SeqIndex.int * Int16.int -> Int16.int;
               val fInt32 = _import "Parallel_arrayFetchAndAdd32" impure private: Int32.t array * SeqIndex.int * Int32.int -> Int32.int;
               val fInt64 = _import "Parallel_arrayFetchAndAdd64" impure private: Int64.t array * SeqIndex.int * Int64.int -> Int64.int;
               val fIntInf = fn _ => raise Fail "MLton.Parallel.Unsafe.arrayFetchAndAdd: IntInf")
        in
           fun arrayFetchAndAdd (xs, i) d =
              I.f (xs, SeqIndex.fromInt i, d)
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

    (* ========================== compareAndSwap ========================== *)

    local
       structure I =
          Int_ChooseInt
          (type 'a t = 'a ref * 'a * 'a -> 'a
           val fInt8 = _import "Parallel_compareAndSwap8" impure private: Int8.t ref * Int8.int * Int8.int -> Int8.int;
           val fInt16 = _import "Parallel_compareAndSwap16" impure private: Int16.t ref * Int16.int * Int16.int -> Int16.int;
           val fInt32 = _import "Parallel_compareAndSwap32" impure private: Int32.t ref * Int32.int * Int32.int -> Int32.int;
           val fInt64 = _import "Parallel_compareAndSwap64" impure private: Int64.t ref * Int64.int * Int64.int -> Int64.int;
           val fIntInf = fn _ => raise Fail "MLton.Parallel.compareAndSwap: IntInf")
    in
       fun compareAndSwap r (old, new) =
          I.f (r, old, new)
    end

    fun realCompareAndSwap r (old, new) =
        let val f = _import "Parallel_compareAndSwapR64" impure private: real ref * real * real -> real;
        in
            f (r, old, new)
        end

    fun realArrayCompareAndSwap (xs, i) (old, new) =
        let val f = _import "Parallel_arrayCompareAndSwapR64" impure private: real array * SeqIndex.int * real * real -> real;
        in
            if i < 0 orelse i >= Array.length xs
            then raise Subscript
            else f (xs, SeqIndex.fromInt i, old, new)
        end

    fun arrayCompareAndSwap (xs, i) (old, new) =
      if i < 0 orelse i >= Array.length xs
      then raise Subscript
      else Unsafe.arrayCompareAndSwap (xs, i) (old, new)

    (* ========================== fetchAndAdd ========================== *)

    local
       structure I =
          Int_ChooseInt
          (type 'a t = 'a ref * 'a -> 'a
           val fInt8 = _import "Parallel_fetchAndAdd8" impure private: Int8.t ref * Int8.int -> Int8.int;
           val fInt16 = _import "Parallel_fetchAndAdd16" impure private: Int16.t ref * Int16.int -> Int16.int;
           val fInt32 = _import "Parallel_fetchAndAdd32" impure private: Int32.t ref * Int32.int -> Int32.int;
           val fInt64 = _import "Parallel_fetchAndAdd64" impure private: Int64.t ref * Int64.int -> Int64.int;
           val fIntInf = fn _ => raise Fail "MLton.Parallel.fetchAndAdd: IntInf")
    in
       fun fetchAndAdd r d =
          I.f (r, d)
    end

    fun arrayFetchAndAdd (xs, i) d =
      if i < 0 orelse i >= Array.length xs
      then raise Subscript
      else Unsafe.arrayFetchAndAdd (xs, i) d

  end
