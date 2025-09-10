functor HashTable (structure K: KEY structure V: VALUE) =
struct

  datatype t = T of {keys: K.t array, values: V.t array}

  exception Full
  exception DuplicateKey

  type table = t


  fun make {capacity} =
    let
      val keys = SeqBasis.tabulate 5000 (0, capacity) (fn _ => K.empty)
      val values = SeqBasis.tabulate 5000 (0, capacity) (fn _ => V.zero)
    in
      T {keys = keys, values = values}
    end


  fun capacity (T {keys, ...}) = Array.length keys


  fun size (T {keys, ...}) =
    SeqBasis.reduce 10000 op+ 0 (0, Array.length keys) (fn i =>
      if K.equal (Array.sub (keys, i), K.empty) then 0 else 1)


  fun unsafe_view_contents (tab as T {keys, values}) =
    let
      val capacity = Array.length keys

      fun elem i =
        let
          val k = Array.sub (keys, i)
        in
          if K.equal (k, K.empty) then NONE else SOME (k, Array.sub (values, i))
        end
    in
      DelayedSeq.tabulate elem (Array.length keys)
    end


  fun bcas (arr, i, old, new) =
    MLton.eq (old, Concurrency.casArray (arr, i) (old, new))


  fun atomic_combine_with (f: 'a * 'a -> 'a) (arr: 'a array, i) (x: 'a) =
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
    end


  fun insert_combine (input as T {keys, values}) (x, v) =
    let
      val n = Array.length keys
      val tolerance = n

      fun claim_slot_at i = bcas (keys, i, K.empty, x)

      fun put_value_at i =
        atomic_combine_with V.combine (values, i) v

      fun loop i probes =
        if probes >= tolerance then
          raise Full
        else if i >= n then
          loop 0 probes
        else
          let
            val k = Array.sub (keys, i)
          in
            if K.equal (k, K.empty) then
              if claim_slot_at i then put_value_at i else loop i probes
            else if K.equal (k, x) then
              put_value_at i
            else
              loop (i + 1) (probes + 1)
          end

      val start = (K.hash x) mod (Array.length keys)
    in
      loop start 0
    end

end
