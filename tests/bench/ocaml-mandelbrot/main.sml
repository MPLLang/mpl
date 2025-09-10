structure CLA = CommandLineArgs

(* ocaml source:
 *
##   let niter = 50
##   let limit = 4.
##
##   let num_domains = int_of_string (Array.get Sys.argv 1)
##   let w = int_of_string (Array.get Sys.argv 2)
 *
 *)

val niter = 50
val limit = 4.0

val w = CLA.parseInt "w" 16000
val _ = print ("w " ^ Int.toString w ^ "\n")

(* ocaml source:
 *
##
##   let worker w h_lo h_hi =
##     let buf =
##       Bytes.create ((w / 8 + (if w mod 8 > 0 then 1 else 0)) * (h_hi - h_lo))
##     and ptr = ref 0 in
##     let fw = float w /. 2. in
##     let fh = fw in
##     let red_w = w - 1 and red_h_hi = h_hi - 1 and byte = ref 0 in
##     for y = h_lo to red_h_hi do
##       let ci = float y /. fh -. 1. in
##       for x = 0 to red_w do
##         let cr = float x /. fw -. 1.5
##         and zr = ref 0. and zi = ref 0. and trmti = ref 0. and n = ref 0 in
##         begin try
##     while true do
##             Domain.Sync.poll ();
##       zi := 2. *. !zr *. !zi +. ci;
##       zr := !trmti +. cr;
##       let tr = !zr *. !zr and ti = !zi *. !zi in
##       if tr +. ti > limit then begin
##         byte := !byte lsl 1;
##         raise Exit
##       end else if incr n; !n = niter then begin
##         byte := (!byte lsl 1) lor 0x01;
##         raise Exit
##       end else
##         trmti := tr -. ti
##     done
##         with Exit -> ()
##         end;
##         if x mod 8 = 7 then begin
##     Bytes.set buf !ptr (Char.chr !byte);
##     incr ptr;
##     byte := 0
##         end
##       done;
##       let rem = w mod 8 in
##       if rem != 0 then begin
##         Bytes.set buf !ptr (Char.chr (!byte lsl (8 - rem)));
##         incr ptr;
##         byte := 0
##       end
##     done;
##     buf
 *
 *)

fun incr x = (x := !x + 1)

fun worker w h_lo h_hi =
  let
    val buf = ForkJoin.alloc ((w div 8 + (if w mod 8 > 0 then 1 else 0)) * (h_hi - h_lo))
    val ptr = ref 0
    val fw = Real.fromInt w / 2.0
    val fh = fw
    val byte = ref 0w0
  in
    Util.for (h_lo, h_hi) (fn y =>
      let
        val ci = Real.fromInt y / fh - 1.0
      in
        (* print ("y=" ^ Int.toString y ^ "\n"); *)
        Util.for (0, w) (fn x =>
          let
            val cr = Real.fromInt x / fw - 1.5
            val zr = ref 0.0
            val zi = ref 0.0
            val trmti = ref 0.0
            val n = ref 0

            fun loop () =
              ( zi := 2.0 * !zr * !zi + ci
              ; zr := !trmti + cr
              ; let
                  val tr = !zr * !zr
                  val ti = !zi * !zi
                in
                  if tr + ti > limit then
                    (byte := Word.<< (!byte, 0w1))
                  else if (incr n; !n = niter) then
                    (byte := Word.orb (Word.<< (!byte, 0w1), 0wx01))
                  else
                    (trmti := tr - ti; loop ())
                end
              )
          in
            (* print ("x=" ^ Int.toString x ^ "\n"); *)
            loop ();
            if x mod 8 = 7 then
              ( Array.update (buf, !ptr, Char.chr (Word.toInt (!byte)))
              ; incr ptr
              ; byte := 0w0
              )
            else ()
          end);

        let
          val rem = w mod 8
        in
          if rem <> 0 then
            ( Array.update (buf, !ptr,
                Char.chr (Word.toInt (Word.<< (!byte, Word.fromInt (8 - rem)))))
            ; incr ptr
            ; byte := 0w0
            )
          else ()
        end

      end);

    buf
  end

(* ocaml source:
 *
##   let _ =
##     let pool = T.setup_pool ~num_domains:(num_domains - 1) in
##     let rows = w / num_domains and rem = w mod num_domains in
##     Printf.printf "P4\n%i %i\n%!" w w;
##     let work i () =
##       worker w (i * rows + min i rem) ((i+1) * rows + min (i+1) rem)
##     in
##     let doms =
##       Array.init (num_domains - 1) (fun i -> T.async pool (work i)) in
##     let r = work (num_domains-1) () in
##     Array.iter (fun d -> Printf.printf "%a%!" output_bytes (T.await pool d)) doms;
##     Printf.printf "%a%!" output_bytes r;
##     T.teardown_pool pool
 *)

(* GRAIN is the target work for one worker (in terms of number of pixels);
 * this should be big enough to amortize the cost of parallelism. To match up
 * with the original code, pick the maximum "number of domains" so that each
 * domain has approximately at least GRAIN work to do (but cap this at some
 * reasonably large number...)
 *)
val GRAIN = 1000
val num_domains = Int.min (500, Int.min (w, Util.ceilDiv (w * w) GRAIN))

val rows = w div num_domains
val rem = w mod num_domains

fun work i =
  worker w (i * rows + Int.min (i, rem)) ((i+1) * rows + Int.min (i+1, rem))

val results =
  Benchmark.run "running mandelbrot" (fn _ =>
    SeqBasis.tabulate 1 (0, num_domains) work)

val outfile = CLA.parseString "output" ""

val _ =
  if outfile = "" then
    print ("use -output XXX to see result\n")
  else
    let
      val file = TextIO.openOut outfile
      fun dump1 c = TextIO.output1 (file, c)
      fun dump str = TextIO.output (file, str)
    in
      ( dump "P4\n"
      ; dump (Int.toString w ^ " " ^ Int.toString w ^ "\n")
      ; Array.app (Array.app dump1) results
      ; TextIO.closeOut file
      )
    end

