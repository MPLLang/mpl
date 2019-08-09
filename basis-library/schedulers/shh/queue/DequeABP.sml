(* non-resizing concurrent deque for work-stealing.
 * hard-coded capacity, see below. *)
structure DequeABP :
sig
  type 'a t
  exception Full

  val new : int -> 'a t
  val pollHasWork : 'a t -> bool
  val clear : 'a t -> unit

  (* set the minimum depth of this deque, i.e. the fork depth of the
   * MLton thread that is currently using this deque. This is used to
   * interface with the runtime, to coordinate local garbage collections. *)
  val setDepth : 'a t -> int -> unit

  (* raises Full if at capacity *)
  val pushBot : 'a t -> 'a -> unit

  (* returns NONE if deque is empty *)
  val popBot : 'a t -> 'a option

  (* returns an item and the depth of that item *)
  val tryPopTop : 'a t -> ('a * int) option

  val size : 'a t -> int
  val numResets : 'a t -> int
end =
struct

  (* capacity is configurable, but should be small. We need to be able to
   * tag indices and pack them into 64-bit words.
   * We also subtract 1 so that we can use index ranges of the form
   * [lo, hi) where 0 <= lo,hi < capacity
   *)
  val capacityPow = 8 (* DO NOT CHANGE THIS WITHOUT ALSO CHANGING runtime/gc/... *)
  val capacity = Word.toInt (Word.<< (0w1, Word.fromInt capacityPow)) - 1

  (* we tag indices and pack into a single 64-bit word, to
   * compare-and-swap as a unit. *)
  structure TagIdx :
  sig
    eqtype t
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

  fun myWorkerId () =
    MLton.Parallel.processorNumber ()

  fun die strfn =
    ( print (Int.toString (myWorkerId ()) ^ ": " ^ strfn ())
    ; OS.Process.exit OS.Process.failure
    )

  type 'a t = {data : 'a option array,
               top : TagIdx.t ref,
               bot : int ref,
               depth : int ref}

  exception Full

  fun for (i, j) f = if i = j then () else (f i; for (i+1, j) f)
  fun arrayUpdate (a, i, x) = MLton.HM.arrayUpdateNoBarrier (a, Int64.fromInt i, x)
  fun cas r (x, y) = MLton.Parallel.compareAndSwap r (x, y)

  fun new _ =
    {data = Array.array (capacity, NONE),
     top = ref (TagIdx.pack {tag=0w0, idx=0}),
     bot = ref 0,
     depth = ref 0}

  fun setDepth ({depth, top, bot, data} : 'a t) d =
    let
      val oldTop = !top
      val oldBot = !bot
      val {tag, idx} = TagIdx.unpack oldTop
      val newTop = TagIdx.pack {tag=tag+0w1, idx=d}
    in
      if idx <> oldBot then
        die (fn _ => "scheduler bug: setDepth must be on empty deque " ^
                     "(top=" ^ Int.toString idx ^ "bot=" ^ Int.toString oldBot ^ ")")
      else
        ( depth := d
        ; if d < idx then
            (bot := d; top := newTop)
          else
            (top := newTop; bot := d)
        )
    end

  fun clear ({data, ...} : 'a t) =
    for (0, Array.length data) (fn i => arrayUpdate (data, i, NONE))

  fun pollHasWork ({top, bot, ...} : 'a t) =
    let
      val b = !bot
      val {idx, ...} = TagIdx.unpack (!top)
    in
      idx < b
    end

  fun pushBot (q as {data, top, bot, depth}) x =
    let
      val oldBot = !bot
      val _ = if oldBot < TagIdx.maxIdx then () else raise Full
    in
      arrayUpdate (data, oldBot, SOME x);
      (* Normally we would now increment bot and then issue a memory fence.
       * However we don't have memory fence primitives in Parallel ML yet.
       * So, let's hack it and do a compare-and-swap. Note that the CAS is
       * guaranteed to succeed, because multiple pushBot operations are never
       * executed concurrently. *)
      cas bot (oldBot, oldBot+1);
      (* bot := oldBot + 1; *)
      ()
    end

  fun tryPopTop (q as {data, top, bot, depth}) =
    let
      val oldTop = !top
      val {tag, idx} = TagIdx.unpack oldTop
      val oldBot = !bot
    in
      if oldBot <= idx then
        NONE
      else
        let
          val x = Array.sub (data, idx)
          val newTop = TagIdx.pack {tag=tag, idx=idx+1}
        in
          if oldTop = cas top (oldTop, newTop) then
            SOME (Option.valOf x, idx)
          else
            NONE
        end
    end

  fun popBot (q as {data, top, bot, depth}) =
    let
      val oldBot = !bot
      val d = !depth
    in
      if oldBot <= d then
        NONE
      else
        let
          val newBot = oldBot-1
          (* once again, we need a fence here, but the best we can do is a
           * compare-and-swap. *)
          (* val _ = bot := newBot *)
          val _ = cas bot (oldBot, newBot)
          val x = Array.sub (data, newBot)
          val oldTop = !top
          val {tag, idx} = TagIdx.unpack oldTop
          val newTop = TagIdx.pack {idx=idx, tag=tag+0w1}
        in
          if newBot > idx then
            (arrayUpdate (data, newBot, NONE); x)
          else if newBot = idx andalso oldTop = cas top (oldTop, newTop) then
            (arrayUpdate (data, newBot, NONE); x)
          else
            ( top := newTop
              (* once again we need a fence... cas on bot should be good
               * enough, and guaranteed to succeed. *)
            ; cas bot (newBot, idx)
            ; NONE
            )
        end
    end

  fun size ({top, bot, ...} : 'a t) =
    let
      val thisBot = !bot
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
