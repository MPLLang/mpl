functor QuickSortFutures (A: SORTARG) : SORT = 
struct

  open A

  fun sort { cutoff, fallback } xs =
    let
      fun qs' (nil, rest) = rest
        | qs' (x::xs, rest) = 
          let
            fun partition nil = (nil, nil)
              | partition (y::ys) =
                let
                  val (ls, rs) = partition ys
                in
                  if x > y then
                    (y::ls, rs)
                  else
                    (ls, y::rs)
                end
            val (ls, rs) = partition xs
          in
            qs' (ls, x::(qs' (rs, rest)))
          end

      datatype 'a flist = Nil | Cons of 'a * 'a flist future
(*
      val Nil' = Nil
      val Cons' = Cons
      val future' = future
      val touch' = touch
*)
      datatype 'a flist' = Nil' | Cons' of 'a * 'a flist' MLton.Parallel.FutureSuspend.t
      val future' = MLton.Parallel.FutureSuspend.future
      val touch' = MLton.Parallel.FutureSuspend.touch

      fun partition (x, ys, yss) =
          let
            fun part (y::ys) = 
                let
                  val (ls, rs) = part ys
                in
                  if x > y then (y :: ls, rs)
                  else (ls, y :: rs)
                end
              | part nil = (nil, nil)
            val (ls, rs) = part ys
          in
            case touch yss 
             of Cons (ys, yss) =>
                let
                  val parts = future (fn () => partition (x, ys, yss))
                in
                  (ls, rs, 
                   future (fn () => 
                              let
                                val (ls, _, lss, _) = touch parts
                              in
                                Cons (ls, lss)
                              end),
                   future (fn () =>
                              let
                                val (_, rs, _, rss) = touch parts
                              in
                                Cons (rs, rss)
                              end))
                end
              | Nil => (ls, rs, future (fn () => Nil), future (fn () => Nil))
          end

      fun qs (Nil, rest) : real list flist' = rest
        | qs (Cons (nil, xss), rest) = qs (touch xss, rest)
        | qs (Cons (x::xs, xss), rest) =
          let
            val (ls, rs, lss, rss) = partition (x, xs, xss)
          in
            case touch rss
             of Nil =>
                qs (Cons (ls, lss), 
                    Cons' (qs' (x::rs, nil), future' (fn () => rest)))
              | _ => 
                qs (Cons (ls, lss), 
                    Cons' ([x], future' (fn () => (qs (Cons (rs, rss), rest)))))
          end

      fun toStream xs =
          let 
            fun loop nil = Nil
              | loop xs = Cons (List.take (xs, cutoff), 
                                future (fn () => loop (List.drop (xs, cutoff))))
                          handle Subscript => Cons (xs, future (fn () => Nil))
          in
            loop xs
          end
      fun fromStream Nil' = nil
        | fromStream (Cons' (xs, xss)) = xs @ (fromStream (touch' xss))
    in
      fromList NONE (fromStream (qs (toStream (toList xs), Nil')))
    end

end

(*
typed transcription

fun qsort' l =
    let
      datatype 'a flist = Nil | Cons of 'a * 'a flist future

      fun partition (_, Nil) = (future (fn () => Nil), 
                                future (fn () => Nil))
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

*)

(*
untyped transliteration

fun qsort l =
    let
      fun qs (nil, rest) = rest
        | qs (x::xs, rest) =
          let
            val parts = partition (x, xs)
          in
            qs (#1 parts, (future (fn () => x :: (qs (#2 parts, rest)))))
          end
	    
      fun partition (_, nil) = (nil, nil)
        | partition (x, y::ys) =
          let
            val parts = future (fn () => partition (x, ys))
          in
            if x > y then
              (y :: (future (fn () => #1 parts)),
               (future (fn () => #2 parts)))
            else
              (future (fn () => #1 parts),
               y :: (future (fn () => #2 parts)))
          end
    in
      qs (l, nil)
    end
*)

(*
from Halstead '85

(defun qsort (l) (qs l nil))

(defun qs (l rest)
  (if (null l)
      rest
      (let ((parts (partition (car l) (cdr l))))
	(qs (left-part parts)
	    (future (cons (car l) (qs (right-part parts) rest)))))))
	    

(defun partition (elt lst)
  (if (null lst)
      (bundle-parts nil nil)
    (let ((cdrparts (future partition elt (cdr lst)))))
    (if (> elt (car lst))
	(bundle-parts (cons (car lst)
			    (future (left-part cdrparts)))
		      (future (right-part cdrparts)))
      (bundle-parts (future (left-part cdrparts))
		    (cons (car lst)
			  (future (right-part cdrparts))))))))

(defun bundle-parts (x y) (cons x y))
(defun left-part (p) (car p))
(defun right-part (p) (cdr p))

*)
