functor MergeSortFutures (structure A: SORTARG) : SORT = 
struct

  open A

  fun sort { cutoff, fallback } a = 
    let 
      datatype 'a flist = Nil | Cons of 'a * 'a flist future

      val futures = ref 0
      val future = fn f => (futures := !futures + 1; future f)

      fun merge (xss, yss, 0, zs) = 
          Cons (rev zs, future (fn () => merge (xss, yss, cutoff, nil)))

        | merge (Nil, yss, _, zs) = Cons (rev zs, future (fn () => yss))
        | merge (xss, Nil, _, zs) = Cons (rev zs, future (fn () => xss))

        | merge (Cons (nil, xss), yss, count, zs) = merge (touch xss, yss, count, zs)
        | merge (xss, Cons (nil, yss), count, zs) = merge (xss, touch yss, count, zs)
(*
        | merge (Cons (xs, xss), Cons (ys, yss), count, zs) =
          let
            fun m (xs, ys, 0, zs) = (xs, ys, 0, zs)
              | m (nil, ys, count, zs) = (nil, ys, count, zs)
              | m (xs, nil, count, zs) = (xs, nil, count, zs)
              | m (xs' as x::xs, ys' as y::ys, count, zs) =
                if x < y then
                  m (xs, ys', count - 1, x::zs)
                else
                  m (xs', ys, count - 1, y::zs)
            val (xs, ys, count, zs) = m (xs, ys, count, zs)
          in
            merge (Cons (xs, xss), Cons (ys, yss), count, zs)
          end
*)
        | merge (xsxss as Cons (x::xs, xss), 
                 ysyss as Cons (y::ys, yss), count, zs) =
          if x < y then
            merge (Cons (xs, xss), ysyss, count - 1, x::zs)
          else
            merge (xsxss, Cons (ys, yss), count - 1, y::zs)

      fun loop a = 
        let 
          val l = length a 
        in
          if l < cutoff then Cons (toList (fallback a), future (fn () => Nil))
          else
            let
              val (b, c) = halve a
              val (b, c) = (* fork (fn () => loop b, 
                                 fn () => loop c) *)
                           (loop b, loop c)
            in
              merge (b, c, cutoff, nil)
            end
        end

      fun fromStream Nil = nil
        | fromStream (Cons (xs, xss)) = xs @ (fromStream (touch xss))
    in
      fromList (SOME cutoff) (fromStream (loop a))
      before 
      print (String.concat ["total futures: ", Int.toString (!futures), " "])
    end
        
end


