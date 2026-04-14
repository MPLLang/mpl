(* ocaml source:
 *
##   let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
##   let n_times = try int_of_string Sys.argv.(2) with _ -> 2
##   let board_size = try int_of_string Sys.argv.(3) with _ -> 1024
 *)
structure CLA = CommandLineArgs
val n_times = CLA.parseInt "n_times" 2
val board_size = CLA.parseInt "board_size" 1024
val _ = print ("n_times " ^ Int.toString n_times ^ "\n")
val _ = print ("board_size " ^ Int.toString board_size ^ "\n")

(* ocaml source:
 *
##   module T = Domainslib.Task
##
##   let rg =
##     ref (Array.init board_size (fun _ ->
##          Array.init board_size (fun _ -> Random.int 2)))
##   let rg' =
##     ref (Array.init board_size (fun _ ->
##          Array.init board_size (fun _ -> Random.int 2)))
##   let buf = Bytes.create board_size
 *
 * The buf is not used.
 *
 * Most obvious adaptation is just nested sequences. We'll use
 * a hash function to seed the initial state.
 *)

fun randBool pos =
  Util.hash pos mod 2

val bs = board_size

fun vtab f n = Vector.tabulate (n, f)
fun vnth v i = Vector.sub (v, i)

fun atab f n = Array.tabulate (n, f)
fun anth a i = Array.sub (a, i)

val rg =
  (*ref*) (vtab (fn i => atab (fn j => randBool (i*bs + j)) bs) bs)
val rg' =
  (*ref*) (vtab (fn i => atab (fn j => randBool (bs*bs + i*bs + j)) bs) bs)

(* ocaml source:
 *
##   let get g x y =
##     try g.(x).(y)
##     with _ -> 0
##
##   let neighbourhood g x y =
##     (get g (x-1) (y-1)) +
##     (get g (x-1) (y  )) +
##     (get g (x-1) (y+1)) +
##     (get g (x  ) (y-1)) +
##     (get g (x  ) (y+1)) +
##     (get g (x+1) (y-1)) +
##     (get g (x+1) (y  )) +
##     (get g (x+1) (y+1))
##
##   let next_cell g x y =
##     let n = neighbourhood g x y in
##     match g.(x).(y), n with
##     | 1, 0 | 1, 1                      -> 0  (* lonely *)
##     | 1, 4 | 1, 5 | 1, 6 | 1, 7 | 1, 8 -> 0  (* overcrowded *)
##     | 1, 2 | 1, 3                      -> 1  (* lives *)
##     | 0, 3                             -> 1  (* get birth *)
##     | _ (* 0, (0|1|2|4|5|6|7|8) *)     -> 0  (* barren *)
 *)

fun get g x y =
  anth (vnth g x) y
  handle _ => 0

(* fun neighbourhood g x y =
  (get g (x-1) (y-1)) +
  (get g (x-1) (y  )) +
  (get g (x-1) (y+1)) +
  (get g (x  ) (y-1)) +
  (get g (x  ) (y+1)) +
  (get g (x+1) (y-1)) +
  (get g (x+1) (y  )) +
  (get g (x+1) (y+1)) *)

fun neighbourhood g x y =
  let
    fun get_element s y =
      anth s y
      handle _ => 0
    fun sum_row(x, y) =
      let
        val gx = vnth g x
      in
        (get_element gx (y-1)) + (get_element gx y) + (get_element gx (y+1))
      end
      handle _ => 0
  in
    sum_row(x-1, y) + sum_row(x, y) + sum_row(x + 1, y)
  end

fun next_cell g x y =
  let
    val n = neighbourhood g x y
  in
    (* Why not just write it like this??
    case (Seq.nth (Seq.nth g x) y, n) of
      (1, 2) => 1  (* lives *)
    | (1, 3) => 1  (* lives *)
    | (0, 3) => 1  (* get birth *)
    | _ => 0
    *)

    (* I could enable MLton or-patterns, but whatever *)
    case (anth (vnth g x) y, n) of
      (1, 0)                       => 0  (* lonely *)
    | (1, 1)                       => 0  (* lonely *)
    | (1, 4)                       => 0  (* overcrowded *)
    | (1, 5)                       => 0  (* overcrowded *)
    | (1, 6)                       => 0  (* overcrowded *)
    | (1, 7)                       => 0  (* overcrowded *)
    | (1, 8)                       => 0  (* overcrowded *)
    | (1, 2)                       => 1  (* lives *)
    | (1, 3)                       => 1  (* lives *)
    | (0, 3)                       => 1  (* get birth *)
    | _ (* 0, (0|1|2|4|5|6|7|8) *) => 0  (* barren *)

    (* With or-patterns it would look like:
    case (Seq.nth (Seq.nth g x) y, n) of
      (1, 0) | (1, 1)                            => 0  (* lonely *)
    | (1, 4) | (1, 5) | (1, 6) | (1, 7) | (1, 8) => 0  (* overcrowded *)
    | (1, 2) | (1, 3)                            => 1  (* lives *)
    | (0, 3)                                     => 1  (* get birth *)
    | _ (* 0, (0|1|2|4|5|6|7|8) *)               => 0  (* barren *)
    *)
  end

(* ocaml source:
 *
##   (* let print g =
##     for x = 0 to board_size - 1 do
##       for y = 0 to board_size - 1 do
##         if g.(x).(y) = 0
##         then Bytes.set buf y '.'
##         else Bytes.set buf y 'o'
##       done;
##       print_endline (Bytes.unsafe_to_string buf)
##     done;
##     print_endline "" *)
##
##   let next pool =
##     let g = !rg in
##     let new_g = !rg' in
##     T.parallel_for pool ~chunk_size:(board_size/num_domains) ~start:0
##       ~finish:(board_size - 1) ~body:(fun x ->
##         for y = 0 to board_size - 1 do
##           new_g.(x).(y) <- next_cell g x y
##         done);
##     rg := new_g;
##     rg' := g
##
##
##   let rec repeat pool n =
##     match n with
##     | 0-> ()
##     | _-> next pool; repeat pool (n-1)
##
##   let ()=
##     let pool = T.setup_pool ~num_domains:(num_domains - 1) in
##     (* print !rg; *)
##     repeat pool n_times;
##     (* print !rg; *)
##     T.teardown_pool pool
 *)

fun next (g, new_g) =
  let
    (* val g = !rg
    val new_g = !rg' *)

    (* SAM_NOTE: this is my own granularity control. The ocaml source does
     * static partitioning based on num_domains, but this is unnecessary.
     * Just choose a static granularity that is reasonable, and then it will
     * work decently for any number of processors. *)
    val target_granularity = 10000
    val chunk_size = Int.max (1, target_granularity div board_size)
  in
    ForkJoin.parfor chunk_size (0, board_size) (fn x =>
      Util.for (0, board_size) (fn y =>
        Array.update (vnth new_g x, y, next_cell g x y)));

    (new_g, g)
  end

fun repeat state n =
  case n of
    0 => state
  | _ => repeat (next state) (n-1)

val msg = "doing " ^ Int.toString n_times ^ " iterations"
val (result, _) = Benchmark.run msg (fn _ => repeat (rg, rg') n_times)

(* ===========================================================================
 * SAM_NOTE: rest is my stuff. Just outputting the result.
 *)

val output = CLA.parseString "output" ""
val _ =
  if output = "" then
    print ("use -output XXX to see result\n")
  else
    let
      (* val g = !rg *)
      val g = result

      fun color 0 = Color.white
        | color _ = Color.black

      val image =
        { height = board_size
        , width = board_size
        , data = Seq.tabulate (fn k =>
            color (get g (k div board_size) (k mod board_size)))
            (board_size * board_size)
        }
      val (_, tm) = Util.getTime (fn _ => PPM.write output image)
    in
      print ("wrote output in " ^ Time.fmt 4 tm ^ "s\n")
    end

