open IO
(* val fork = MLton.Parallel.ForkJoin.fork *)

(*
val seed = MLton.Random.seed ()
val _ = case seed of
            SOME s => MLton.Random.srand s
          | NONE => (print "Randomness not seeded!\n"; ())
*)

val _ = MLton.Random.srand (Word.fromInt 943875293)

val lock = _import "Parallel_lockTake" runtime private: int ref -> unit;
val unlock = _import "Parallel_lockRelease" runtime private: int ref -> unit;

val mut = ref ~1

fun atomically f =
    (lock mut;
     f ()
     before unlock mut)

val start = Time.now ()

val stdin = ref (TextIO.getInstream TextIO.stdIn)

fun inputLine () =
    let fun iL_int line =
            case IO.input1 (!stdin) of
                NONE => NONE
              | SOME (c, is') =>
                (stdin := is';
                 if c = #"\n" then
                     SOME line
                 else
                     iL_int (line ^ (str c)))
    in
        iL_int ""
    end

fun forkn n f =
    let fun fork_int n i =
            if n = 0 then [] else
            if n = 1 then [f i] else
            let val left = Int.div (n, 2)
                val right = n - left
                val (l, r) = MLton.Parallel.ForkJoin.fork
                    ((fn () => fork_int left i),
                     (fn () => fork_int right (i + left)))
            in
                l @ r
            end
    in
        fork_int n 0
    end

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

fun seqexplore d b rnd =
    if d <= 1 then 0 else
    1 +
    (let val i = DotMix.boundedInt (0, 1000000000) rnd
         val cs = (geom (1.0 / b) (Real./ (Real.fromInt i, 1000000000.0)))
         val (_, rs) = DotMix.splitTab (rnd, cs)
     in
         List.foldl op+ 0 (List.tabulate (cs,
                                          (fn i => seqexplore (d - 1) b
                                                              (rs i))))
    end)

fun explore d b rnd =
    if d <= 1 then 0 else
    if d <= 6 then seqexplore d b rnd else
    (1 +
    (let val i = DotMix.boundedInt (0, 1000000000) rnd
        val cs = (geom (1.0 / b) (Real./ (Real.fromInt i, 1000000000.0)))
        val (_, rs) = DotMix.splitTab (rnd, cs)
    in
        List.foldl op+ 0 (forkn cs
                                (fn i => explore (d - 1) b
                                                 (rs i)))
    end))
(* handle Overflow => (print "here 140\n"; 1) *)

fun top_explore () =
    let val nodes = (explore 12 5.5 (DotMix.fromInt 827346237 (* 42 *)))
        (* handle e => (print "here 164\n"; raise e) *)
        val finish = Time.now ()
        val diff = Time.-(finish, start)
        val diffi = LargeInt.toInt (Time.toMilliseconds diff)
    in
        print ("nodes: " ^ (Int.toString nodes) ^ "\n");
        print ("exectime " ^ (aIntToString diffi) ^ "\n");
        OS.Process.exit OS.Process.success
    end

fun inploop () =
    case inputLine () of
        NONE => OS.Process.exit OS.Process.success
      | SOME l =>
        (print ("Hi, " ^ l ^ "\n");
         inploop ())

val _ = MLton.Parallel.ForkJoin.fork
            ((fn () => MLton.Parallel.FutureFGBG.fg inploop),
             (top_explore)
		handle e => (print "here 164\n"; raise e))
(* val _ = loop fibs fibs *)
