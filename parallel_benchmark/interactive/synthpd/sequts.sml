val start = Time.now ()

(* Convert a random number r in (0, 1) to a sample from a geometric dist.
 with parameter p *)
fun geom p r =
    let fun pr k =
            Math.pow (1.0 - p, (Real.fromInt k) - 1.0) * p
        fun cumu a k =
            let val pk = pr k
                val a' = a + pk
            in
                if r < a' then k else cumu a' (k + 1)
            end
    in
        cumu 0.0 1
    end

(* Sequential base case *)
fun explore d b rnd =
    if d <= 1 then 0 else
    1 +
    (let val i = DotMix.boundedInt (0, 1000000000) rnd
          (* Draw number of children from a geometric distribution *)
         val cs = (geom (1.0 / b) (Real./ (Real.fromInt i, 1000000000.0)))
          (* Generate new seeds for next level *)
         val (_, rs) = DotMix.splitTab (rnd, cs)
     in
         List.foldl op+ 0 (List.tabulate (cs,
                                          (fn i => explore (d - 1) b
                                                              (rs i))))
    end)

fun top_explore () =
    let val nodes = (explore 11 5.5 (DotMix.fromInt 827346237 (* 42 *)))
			    (* handle e => (print "here 164\n"; raise e) *)
        val finish = Time.now ()
        val diff = Time.-(finish, start)
        val diffi = LargeInt.toInt (Time.toMilliseconds diff)
    in
        print ("nodes: " ^ (Int.toString nodes) ^ "\n");
        print ("exectime " ^ (Int.toString diffi) ^ "\n");
        OS.Process.exit OS.Process.success
    end

val _ = top_explore ()
