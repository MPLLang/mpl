functor HashTable
  (K:
   sig
     type t
     val equal: t * t -> bool
     val default: t
     val hash: t -> int
   end) =
struct

  structure Status =
  struct
    type t = Word8.word
    val empty: t = 0w0
    val full: t = 0w1
    val locked: t = 0w2
    val tomb: t = 0w3
  end


  datatype 'a entry = E of {status: Status.t ref, key: K.t ref, value: 'a ref}

  datatype 'a t = T of 'a entry array
  type 'a table = 'a t

  exception Full

  fun make {default: 'a, capacity} : 'a table =
    let
      fun default_entry _ =
        E {status = ref Status.empty, key = ref K.default, value = ref default}
      val entries = SeqBasis.tabulate 5000 (0, capacity) default_entry
    in
      T entries
    end


  fun capacity (T entries) = Array.length entries

  fun status (T entries) i =
    let val E {status, ...} = Array.sub (entries, i)
    in status
    end


  fun size (t as T entries) =
    SeqBasis.reduce 5000 op+ 0 (0, Array.length entries) (fn i =>
      if !(status t i) = Status.full then 0 else 1)


  fun keys (t as T entries) =
    let
      fun key_at i =
        let val E {key, ...} = Array.sub (entries, i)
        in !key
        end

      fun keep_at i =
        !(status t i) = Status.full
    in
      ArraySlice.full
        (SeqBasis.filter 2000 (0, Array.length entries) key_at keep_at)
    end


  (* fun unsafe_view_contents (tab as T {keys, values}) =
    let
      val capacity = Array.length keys
  
      fun elem i =
        let
          val k = Array.sub (keys, i)
        in
          if K.equal (k, K.default) then NONE
          else SOME (k, Array.sub (values, i))
        end
    in
      DelayedSeq.tabulate elem (Array.length keys)
    end *)


  fun bcas (r, old, new) =
    MLton.eq (old, MLton.Parallel.compareAndSwap r (old, new))


  (* fun atomic_combine_with (f: 'a * 'a -> 'a) (arr: 'a array, i) (x: 'a) =
    let
      fun loop current =
        let
          val desired = f (current, x)
        in
          if MLton.eq (desired, current) then
            ()
          else
            let
              val current' =
                MLton.Parallel.arrayCompareAndSwap (arr, i) (current, desired)
            in
              if MLton.eq (current', current) then () else loop current'
            end
        end
    in
      loop (Array.sub (arr, i))
    end *)


  fun insert (t as T entries) (k, v) =
    let
      val n = Array.length entries
      val tolerance = n

      fun claim_slot_at i expected =
        bcas (status t i, expected, Status.locked)

      fun put_kv_at i =
        let val E {status, key, value} = Array.sub (entries, i)
        in key := k; value := v; bcas (status, Status.locked, Status.full)
        end

      fun put_v_at i =
        let val E {status, key, value} = Array.sub (entries, i)
        in value := v; bcas (status, Status.locked, Status.full)
        end

      fun loop i probes =
        if probes >= tolerance then
          raise Full
        else if i >= n then
          loop 0 probes
        else
          let
            val e as E {status, key, value} = Array.sub (entries, i)
            val s = !status
          in
            if s = Status.full orelse s = Status.tomb then
              if K.equal (!key, k) then
                if s = Status.full then false
                else if claim_slot_at i s then (put_v_at i; true)
                else loop i probes
              else
                loop (i + 1) (probes + 1)
            else if s = Status.empty then
              if claim_slot_at i s then (put_kv_at i; true) else loop i probes
            else
              loop (i + 1) (probes + 1)
          end

      val start = (K.hash k) mod (Array.length entries)
    in
      loop start 0
    end


  fun remove (t as T entries) k =
    let
      val n = Array.length entries
      val tolerance = n

      fun release_slot_at i =
        bcas (status t i, Status.full, Status.tomb)

      fun loop i probes =
        if probes >= tolerance then
          raise Full
        else if i >= n then
          loop 0 probes
        else
          let
            val e as E {status, key, value} = Array.sub (entries, i)
            val s = !status
          in
            if s = Status.empty orelse s = Status.locked then
              NONE
            else if K.equal (!key, k) then
              if s = Status.full then
                let val v = !value
                in release_slot_at i; SOME v
                end
              else
                NONE
            else
              loop (i + 1) (probes + 1)
          end

      val start = (K.hash k) mod (Array.length entries)
    in
      loop start 0
    end

end
