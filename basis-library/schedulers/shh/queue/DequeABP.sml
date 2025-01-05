(* non-resizing concurrent deque for work-stealing.
 * hard-coded capacity, see below. *)
structure DequeABP :
sig
  type 'a t
  exception Full

  val capacity : int

  val new : unit -> 'a t
  val clear : 'a t -> unit

  (* register this deque with the specified worker id *)
  val register : 'a t -> int -> unit

  (* set the minimum depth of this deque, i.e. the fork depth of the
   * MLton thread that is currently using this deque. This is used to
   * interface with the runtime, to coordinate local garbage collections. *)
  val setDepth : 'a t -> int -> unit

  (* raises Full if at capacity *)
  val pushBot : 'a t -> 'a -> unit

  (* returns NONE if deque is empty *)
  val popBot : 'a t -> 'a option
  val tryPopTop : 'a t -> 'a option

  val size : 'a t -> int
  val numResets : 'a t -> int
end =
struct

  (* capacity is configurable, but should be small. We need to be able to
   * tag indices and pack them into 64-bit words.
   * We also subtract 1 so that we can use index ranges of the form
   * [lo, hi) where 0 <= lo,hi < capacity
   *)
  val capacityPow = 6 (* DO NOT CHANGE THIS WITHOUT ALSO CHANGING runtime/gc/... *)
  val capacity = Word.toInt (Word.<< (0w1, Word.fromInt capacityPow)) - 1

  fun myWorkerId () =
    MLton.Parallel.processorNumber ()

  fun die strfn =
    ( print (strfn () ^ "\n")
    ; OS.Process.exit OS.Process.failure
    )

  val capacityStr = Int.toString capacity
  fun exceededCapacityError () =
    die (fn _ => "Scheduler error: exceeded max fork depth (" ^ capacityStr ^ ")")

  (* we tag indices and pack into a single 64-bit word, to
   * compare-and-swap as a unit. *)
  structure TagIdx :
  sig
    type t = Word64.word
    val maxTag : Word64.word
    val maxIdx : int
    val pack : {tag : Word64.word, idx : int} -> t
    val unpack : t -> {tag : Word64.word, idx : int}
  end =
  struct
    type t = Word64.word

    (* NOTE: this actually computes 1 + floor(log_2(n)), i.e. the number of
     * bits required to represent n in binary *)
    fun log2 n = if (n < 1) then 0 else 1 + log2(n div 2)

    val maxIdx = capacity
    val idxBits = Word.fromInt (log2 maxIdx)
    val idxMask = Word64.fromInt maxIdx

    val tagBits = 0w64 - idxBits
    val maxTag = Word64.- (Word64.<< (0w1, tagBits), 0w1)

    fun pack {tag, idx} =
      Word64.orb (Word64.<< (tag, idxBits), Word64.fromInt idx)

    fun unpack ti =
      let
        val idx = Word64.toInt (Word64.andb (ti, idxMask))
        val tag = Word64.>> (ti, idxBits)
      in
        {tag=tag, idx=idx}
      end
  end


  type gcstate = MLton.Pointer.t
  val gcstate = _prim "GC_state": unit -> gcstate;
  val ABP_deque_push_bot = _import "ABP_deque_push_bot" runtime private: gcstate * TagIdx.t ref * Word32.word ref * 'a option array * 'a option -> bool;
  val ABP_deque_try_pop_bot = _import "ABP_deque_try_pop_bot" runtime private: gcstate * TagIdx.t ref * Word32.word ref * 'a option array * 'a option -> 'a option;
  val ABP_deque_try_pop_top = _import "ABP_deque_try_pop_top" runtime private: gcstate * TagIdx.t ref * Word32.word ref * 'a option array * 'a option -> 'a option;
  val ABP_deque_set_depth = _import "ABP_deque_set_depth" runtime private: gcstate * TagIdx.t ref * Word32.word ref * 'a option array * Word32.word -> unit;


  type 'a t = {data : 'a option array,
               top : TagIdx.t ref,
               bot : Word32.word ref}

  exception Full

  fun for (i, j) f = if i = j then () else (f i; for (i+1, j) f)
  fun arrayUpdate (a, i, x) = MLton.HM.arrayUpdateNoBarrier (a, i, x)
  fun cas r (x, y) = MLton.Parallel.compareAndSwap r (x, y)

  fun cas32 b (x, y) = cas b (Word32.fromInt x, Word32.fromInt y)

  fun new () =
    {data = Array.array (capacity, NONE),
     top = ref (TagIdx.pack {tag=0w0, idx=0}),
     bot = ref (0w0 : Word32.word)}

  fun register ({top, bot, data, ...} : 'a t) p =
    ( MLton.HM.registerQueue (Word32.fromInt p, data)
    ; MLton.HM.registerQueueTop (Word32.fromInt p, top)
    ; MLton.HM.registerQueueBot (Word32.fromInt p, bot)
    )

  fun setDepth (q as {top, bot, data}) d =
    ABP_deque_set_depth (gcstate (), top, bot, data, Word32.fromInt d)

  fun clear ({data, ...} : 'a t) =
    for (0, Array.length data) (fn i => arrayUpdate (data, i, NONE))

  fun pushBot (q as {data, top, bot}) x =
    if ABP_deque_push_bot (gcstate (), top, bot, data, SOME x) then ()
    else exceededCapacityError ()

  fun tryPopTop (q as {data, top, bot}) =
    ABP_deque_try_pop_top (gcstate (), top, bot, data, NONE)

  fun popBot (q as {data, top, bot}) =
    ABP_deque_try_pop_bot (gcstate (), top, bot, data, NONE)

  fun size ({top, bot, ...} : 'a t) =
    let
      val thisBot = Word32.toInt (!bot)
      val {idx, ...} = TagIdx.unpack (!top)
    in
      thisBot - idx
    end

  fun numResets ({top, ...} : 'a t) =
    let
      val {tag, ...} = TagIdx.unpack (!top)
    in
      Word64.toInt tag
    end

end
