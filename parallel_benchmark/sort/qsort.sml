fun qsort l =
    let
      datatype 'a stream = Nil | Cons of 'a * 'a stream future
      val empty = future (fn () => Nil

      fun partition (_, Nil) = (empty, empty)
        | partition (x, Cons (y, ys)) =
          let
            val parts = future (fn () => partition (x, touch ys))
            val fst = touch o #1 o touch
            val snd = touch o #2 o touch
          in
            if x > y then
              (future (fn () => Cons (y, (future (fn () => fst parts)))),
               (future (fn () => snd parts)))
            else
              (future (fn () => fst parts),
               future (fn () => Cons (y, (future (fn () => snd parts)))))
          end

      fun qs (Nil, rest) = rest
        | qs (Cons (x, xs), rest) =
          let
            val (ls, rs) = partition (x, touch xs)
          in
            qs (touch ls, 
                Cons (x, future (fn () => (qs (touch rs, rest)))))
          end

      fun toStream nil = Nil
        | toStream (x::xs) = Cons (x, future (fn () => toStream xs))
      fun fromStream Nil = nil
        | fromStream (Cons (x, xs)) = x::(fromStream (touch xs))
    in
      fromStream (qs (toStream l, Nil))
    end
