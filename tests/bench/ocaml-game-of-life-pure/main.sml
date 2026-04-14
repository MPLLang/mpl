structure CLA = CommandLineArgs
val n_times = CLA.parseInt "n_times" 2
val board_size = CLA.parseInt "board_size" 1024
val _ = print ("n_times " ^ Int.toString n_times ^ "\n")
val _ = print ("board_size " ^ Int.toString board_size ^ "\n")

fun randBool pos =
  Util.hash pos mod 2

val bs = board_size

val g =
  PureSeq.tabulate (fn i => PureSeq.tabulate (fn j => randBool (i*bs + j)) bs) bs

fun get g x y =
  PureSeq.nth (PureSeq.nth g x) y
  handle _ => 0

fun neighbourhood g x y =
  (get g (x-1) (y-1)) +
  (get g (x-1) (y  )) +
  (get g (x-1) (y+1)) +
  (get g (x  ) (y-1)) +
  (get g (x  ) (y+1)) +
  (get g (x+1) (y-1)) +
  (get g (x+1) (y  )) +
  (get g (x+1) (y+1))

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
    case (PureSeq.nth (PureSeq.nth g x) y, n) of
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

fun loop curr remaining =
  if remaining <= 0 then
    curr
  else
    let
      (* SAM_NOTE: this is my own granularity control. The ocaml source does
       * static partitioning based on num_domains, but this is unnecessary.
       * Just choose a static granularity that is reasonable, and then it will
       * work decently for any number of processors. *)
      val target_granularity = 5000
      val chunk_size = Int.max (1, target_granularity div board_size)

      val next =
        PureSeq.tabulateG chunk_size (fn x =>
            PureSeq.tabulate (fn y => next_cell curr x y) board_size)
          board_size
    in
      loop next (remaining-1)
    end

val msg = "doing " ^ Int.toString n_times ^ " iterations"
val result = Benchmark.run msg (fn _ => loop g n_times)

(* ===========================================================================
 * SAM_NOTE: rest is my stuff. Just outputting the result.
 *)

val output = CLA.parseString "output" ""
val _ =
  if output = "" then
    print ("use -output XXX.ppm to see result\n")
  else
    let
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

