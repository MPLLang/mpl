functor MergeSortFutures2 (A : SORTARG) : SORT =
struct

  open A

fun pr f = print (f ())
fun pr f = ()

  fun sort { cutoff, fallback } a =
      let
        datatype 'a ftree = Leaf of 'a | Branch of ('a ftree future) * 'a * ('a ftree future)

        fun etos xs = StringUtil.delimit ", " (map Real.toString xs)
        fun ttos (Leaf xs) = String.concat ["[", etos xs, "]"]
          | ttos (Branch (l, xs, r)) =
            String.concat ["{", ttos (touch l), " | ", etos xs, " | ", ttos (touch r), "}"]
        (*
         fun split (x, Leaf) = (Leaf, Leaf)
           | split (x, Branch (l, y, r)) = 
             if x < y then
               let
                 val (l', r') = split (x, l)
               in
                 (l', Branch (r', y, r))
               end
             else
               let
                 val (l', r') = split (x, r)
               in
                 (Branch (l, y, l'), r')
               end
         *)
(*
        fun split (x, Leaf) = (future (fn () => Leaf), future (fn () => Leaf))
          | split (x, Branch (l, y, r)) = 
            if x < y then
              let
                val s = future (fn () => split (x, touch l))
              in
                (future (fn () => touch (#1 (touch s))), 
                 future (fn () => Branch (#2 (touch s), y, r)))
              end
            else
              let
                val s = future (fn () => split (x, touch r))
              in
                (future (fn () => Branch (l, y, #1 (touch s))), 
                 future (fn () => touch (#2 (touch s))))
              end

        fun merge (Leaf, t as Branch _) = t
          | merge (t as Branch _, Leaf) = t
          | merge (Branch (l, x, r), t2) =
            let
              val (l', r') = split (x, t2)
            in
              Branch (future (fn () => merge (touch l, touch l')),
                      x, 
                      future (fn () => merge (touch r, touch r')))
            end
          | merge (Leaf, Leaf) = Leaf
*)

        fun splitelements (x, nil, ls, rs) = (rev ls, rev rs)
          | splitelements (x, y::ys, ls, rs) = 
            if x < y then
              splitelements (x, ys, ls, y::rs)
            else
              splitelements (x, ys, y::ls, rs)

        fun split (x, Leaf xs) =
            let
              val (ls, rs) = splitelements (x, xs, nil, nil)
            in
              (future (fn () => Leaf nil), 
               future (fn () => ls),
               future (fn () => rs),
               future (fn () => Leaf nil))
            end
          | split (x, Branch (l, ys, r)) = 
            case splitelements (x, ys, nil, nil)
             of (nil, rs) =>
                let
                  val s = future (fn () => split (x, touch l))
                in
                  (future (fn () => touch (#1 (touch s))), 
                   future (fn () => touch (#2 (touch s))),
                   future (fn () => touch (#3 (touch s))),
                   future (fn () => Branch (#4 (touch s), rs, r)))
                end
              | (ls, nil) =>
                let
                  val s = future (fn () => split (x, touch r))
                in
                  (future (fn () => Branch (l, ls, #1 (touch s))), 
                   future (fn () => touch (#2 (touch s))), 
                   future (fn () => touch (#3 (touch s))), 
                   future (fn () => touch (#4 (touch s))))
                end
              | (ls, rs) => (l, future (fn () => ls), future (fn () => rs), r) (* easy split *)

        fun mergeelements (nil, t) : real list ftree = t
          | mergeelements (xs, Leaf ys) = 
            let
val () = pr (fn () => (String.concat ["mergingelements: ", etos xs, "\ninto: ", ttos (Leaf ys), "\n"]))
              fun loop (_, nil, nil, acc) = Leaf (rev acc)
                | loop (0, x::xs, y::ys, acc) = 
                  if x < y then 
                    Branch (future (fn () => Leaf (rev acc)),
                            [x],
                            future (fn () => loop (cutoff, xs, y::ys, nil)))
                  else
                    Branch (future (fn () => Leaf (rev acc)),
                            [y],
                            future (fn () => loop (cutoff, x::xs, ys, nil)))                    
                | loop (count, x::xs, nil, acc) = loop (count - 1, xs, nil, x::acc)
                | loop (count, nil, y::ys, acc) = loop (count - 1, nil, ys, y::acc)
                | loop (count, xs as x::xs', ys as y::ys', acc) =
                  if x < y then
                    loop (count - 1, xs', ys, x::acc)
                  else
                    loop (count - 1, xs, ys', y::acc)
              val t = loop (cutoff, xs, ys, nil)
            in
              t
before pr (fn () => (String.concat ["mergingelements returning: ", ttos t, "\n"]))
            end
          | mergeelements (xs, t as Branch (l, y::ys, r)) = 
            let
val () = pr (fn () => (String.concat ["mergingelements(branch): ", etos xs, "\ninto: ", ttos t, "\n"]))
              (* merge any remaining elements *)
              fun loop3 (nil, nil, acc) = rev acc
                | loop3 (x::xs, nil, acc) = loop3 (xs, nil, x::acc)
                | loop3 (nil, y::ys, acc) = loop3 (nil, ys, y::acc)
                | loop3 (xs as x::xs', ys as y::ys', acc) =
                  if x < y then
                    loop3 (xs', ys, x::acc)
                  else
                    loop3 (xs, ys', y::acc)

              (* merge up to 'count' elements, then merge remaining ones *)
              fun loop2 (xs, ys, 0, acc) = (rev acc, loop3 (xs, ys, nil))
                | loop2 (xs, nil, _, acc) = (rev acc, xs)
                | loop2 (nil, y::ys, count, acc) = loop2 (nil, ys, count - 1, y::acc)
                | loop2 (xs as x::xs', ys as y::ys', count, acc) =
                  if x < y then
                    loop2 (xs', ys, count - 1, x::acc)
                  else
                    loop2 (xs, ys', count - 1, y::acc)

              (* read off elements from xs until they overlap with ys *)
              fun loop1 (nil, acc) = (rev acc, y::ys, nil)
                | loop1 (x::xs, acc) =
                  if x < y then
                    loop1 (xs, x::acc)
                  else
                    let
                      val (zs, rs) = loop2 (x::xs, y::ys, cutoff, nil)
                    in
                      (rev acc, zs, rs)
                    end

              val (ls, ys, rs) = loop1 (xs, nil)
              val t = Branch (future (fn () => mergeelements (ls, touch l)),
                              ys,
                              future (fn () => mergeelements (rs, touch r)))
            in
              t
before pr (fn () => (String.concat ["mergingelements(branch) returning: ", ttos t, "\n"]))
            end

        fun merge (Leaf xs, t) = mergeelements (xs, t)
          | merge (t1, t2 as Leaf _) = merge (t2, t1)
          | merge (t1 as Branch (l, x::xs, r), t2) =
            let
              val (l2, ls, rs, r2) = split (x, t2)
val () = pr (fn () => (String.concat ["splitting based on ", ttos t1, "\n"]))
val () = pr (fn () =>  (String.concat ["split on ", Real.toString x, " returned left: ",
                               ttos (touch l2), "\nmiddle: ",
                               etos (touch ls), " & ", etos (touch rs),
                               "\nright: ", ttos (touch r2), "\n"]))
         fun loop (xs, ys, 0, acc) = (rev acc, xs, ys)
           | loop (nil, ys, _, acc) = (rev acc, nil, ys)
           | loop (xs, nil, count, acc) = (rev acc, xs, nil)
           | loop (xs as x::xs', ys as y::ys', count, acc) =
             if x < y then 
               loop (xs', ys, count - 1, x::acc)
             else
               loop (xs, ys', count - 1, y::acc)
         val (xs, xs', rs) = loop (xs, touch rs, cutoff, nil)
val t =
              Branch (future (fn () => merge (case touch ls of nil => touch l2
                                                | ls => Branch (l2, ls,
                                                                future (fn () => Leaf nil)),
                                              touch l)),
                      x::xs,
                      future (fn () => merge (case rs of nil => touch r2
                                               | rs => Branch (future (fn () => Leaf nil),
                                                               rs, r2),
                                              case xs' of nil => touch r
                                                | xs' => Branch (future (fn () => Leaf nil),
                                                                 xs', r))))

in t before pr (fn () => ("merge returning: " ^ ttos t ^ "\n"))
            end

        fun ms a : real list ftree =
            if length a < cutoff then 
              Leaf (toList (fallback a))
            else
              let
                val (b, c) = halve a
                val (b, c) = (ms b, ms c) (* XXX fork? *)
val () = pr (fn () => (String.concat ["rec calls returned: ", ttos b,
                        "\nand: ", ttos c,
                        "\n"]))
              in
                merge (b, c)
              end

(*
        fun extract (i, j, acc) = 
            if i = j then acc
            else extract (i, j - 1, sub (a, j - 1)::acc)
        fun toStream (i, j) = if j - i <= cutoff then
                                   Branch (future (fn () => Leaf), 
                                           extract (i, j, nil),
                                           future (fn () => Leaf))
                              else
                                let
                                  val mid = i - j div 2
                                  val half = cutoff div 2
                                  val j' = mid - half
                                  val i' = mid + (cutoff - half)
                                in
                                  Branch (future (fn () => toStream (i, j')), 
                                          extract (j', i', nil),
                                          future (fn () => toStream (i', j)))
                                end
*)
        fun fromStream (Leaf xs) = [fromList NONE xs]
          | fromStream (Branch (l, xs, r)) =
            fromStream (touch l) @ [fromList NONE xs] @ fromStream (touch r)
      in
        concat (fromStream (ms a))
      end

end
