(* non-resizing concurrent deque for work-stealing.
 * hard-coded capacity, see below. *)
structure DequeABP :
sig
  type 'a t
  exception Full

  val capacity : int

  val new : unit -> 'a t
  val pollHasWork : 'a t -> bool
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
  val capacityPow = 10 (* DO NOT CHANGE THIS WITHOUT ALSO CHANGING runtime/gc/... *)
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

  type 'a t = {data : 'a option array,
               top : TagIdx.t ref,
               bot : Word32.word ref,
               depth : int ref}

  exception Full

  fun for (i, j) f = if i = j then () else (f i; for (i+1, j) f)
  fun arrayUpdate (a, i, x) = MLton.HM.arrayUpdateNoBarrier (a, i, x)
  fun cas r (x, y) = MLton.Parallel.compareAndSwap r (x, y)

  fun cas32 b (x, y) = cas b (Word32.fromInt x, Word32.fromInt y)

  fun new () =
    {data = Array.array (capacity, NONE),
     top = ref (TagIdx.pack {tag=0w0, idx=0}),
     bot = ref (0w0 : Word32.word),
     depth = ref 0}

  fun register ({top, bot, data, ...} : 'a t) p =
    ( MLton.HM.registerQueue (Word32.fromInt p, data)
    ; MLton.HM.registerQueueTop (Word32.fromInt p, top)
    ; MLton.HM.registerQueueBot (Word32.fromInt p, bot)
    )

  fun setDepth (q as {depth, top, bot, data} : 'a t) d =
    let
      fun forceSetTop oldTop =
        let
          val newTop =
            TagIdx.pack {idx = d, tag = 0w1 + #tag (TagIdx.unpack oldTop)}
          val oldTop' = cas top (oldTop, newTop)
        in
          if oldTop = oldTop' then
            ()
          else
            (* The GC must have interfered, so just do it again. GC shouldn't
             * be able to interfere a second time... *)
            forceSetTop oldTop'
        end

      val oldTop = !top
      val oldBot = Word32.toInt (!bot)
      val {idx, ...} = TagIdx.unpack oldTop
    in
      if idx < oldBot then
        die (fn _ => "scheduler bug: setDepth must be on empty deque " ^
                     "(top=" ^ Int.toString idx ^ " bot=" ^ Int.toString oldBot ^ ")")
      else
        ( depth := d
        ; if d < idx then
            (bot := Word32.fromInt d; forceSetTop oldTop)
          else
            (forceSetTop oldTop; bot := Word32.fromInt d)
        )
    end

  fun clear ({data, ...} : 'a t) =
    for (0, Array.length data) (fn i => arrayUpdate (data, i, NONE))

  fun pollHasWork ({top, bot, ...} : 'a t) =
    let
      val b = Word32.toInt (!bot)
      val {idx, ...} = TagIdx.unpack (!top)
    in
      idx < b
    end

  fun pushBot (q as {data, top, bot, depth}) x =
    let
      val oldBot = Word32.toInt (!bot)
    in
      if oldBot >= capacity then exceededCapacityError () else
      (* Normally, an ABP deque would do this:
       *   1. update array
       *   2. increment bot
       *   3. issue a memory fence
       * However we don't have memory fence primitives in Parallel ML yet.
       * So, let's hack it and do a compare-and-swap. Note that the CAS is
       * guaranteed to succeed, because multiple pushBot operations are never
       * executed concurrently. *)
      ( arrayUpdate (data, oldBot, SOME x)
      ; cas32 bot (oldBot, oldBot+1)
      ; ()
      )
    end

  fun tryPopTop (q as {data, top, bot, depth}) =
    let
      val oldTop = !top
      val {tag, idx} = TagIdx.unpack oldTop
      val oldBot = Word32.toInt (!bot)
    in
      if oldBot <= idx then
        NONE
      else
        let
          val x = MLton.HM.arraySubNoBarrier (data, idx)
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
      val oldBot = Word32.toInt (!bot)
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
          val _ = cas32 bot (oldBot, newBot)
          val x = MLton.HM.arraySubNoBarrier (data, newBot)
          val oldTop = !top
          val {tag, idx} = TagIdx.unpack oldTop
        in
          if newBot > idx then
            (arrayUpdate (data, newBot, NONE); x)
          else if newBot < idx then
            (* We are racing with a concurrent steal to take this single
             * element x, but we already lost the race. So we only need to set
             * the bottom to match the idx, i.e. set the deque to a proper
             * empty state. *)
            (* TODO: can we relax the cas to a normal write?
             * Note that this cas is guaranteed to succeed... *)
            (cas32 bot (newBot, idx); NONE)
          else
            (* We are racing with a concurrent steal to take this single
             * element x, but we haven't lost the race yet. *)
            let
              val newTop = TagIdx.pack {tag=tag+0w1, idx=idx}
              val oldTop' = cas top (oldTop, newTop)
            in
              if oldTop' = oldTop then
                (* success; we get to keep x *)
                (arrayUpdate (data, newBot, NONE); x)
              else
                (* two possibilities: either the steal succeeded (in which case
                 * the idx will have moved) or the GC will have interfered (in
                 * which case the tag will have advanced). *)
                let
                  val {idx=idx', ...} = TagIdx.unpack oldTop'
                in
                  if idx' <> idx then
                    (* The steal succeeded, so we don't get to keep this
                     * element. It's possible that the GC interfered also,
                     * but that doesn't change the fact that we don't get to
                     * keep this element! *)
                    (* TODO: can we relax the cas to a normal write?
                     * Note that this cas is guaranteed to succeed... *)
                    (cas32 bot (newBot, idx'); NONE)
                  else
                    (* the GC must have interfered, so try again. It _should_ be
                     * the case that the GC can't interfere on the second try,
                     * because we haven't done any significant allocation
                     * in-between tries. *)
                    (* SAM_NOTE: with new local scope handling in the GC, this
                     * case should be impossible. *)
                    die (fn _ => "scheduler bug: unexpected GC interference")
                    (* popBot q *)
                end
            end
        end
    end

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
