open IO.Graphics
(*val fork = MLton.Parallel.ForkJoin.fork *)

val fork = MLton.Parallel.ForkJoin.forkLat;

val _ = openwindow NONE (512, 512)

(*val _ = Posix.Process.sleep (Time.fromSeconds 1)
*)
val lastfibarg = ref 0;
val lastfib = ref 0;
val clicks = ref 0;

fun get_results() =
    let
        val fib_data = ("fib(" ^ (Int.toString ( !lastfibarg )) ^ "):    " ^ (Int.toString ( !lastfib )) ^ "      ")
        val clicks_data = ("clicks    " ^ (Int.toString ( !clicks )) ^ "   ")
    in
        fib_data ^ clicks_data
    end

fun show_status () = 
    let 
        val stats = get_results()
    in
        clear ();
        drawtext NONE 20 20 stats;
        print (stats ^ "\n");
        flush ()
    end;

fun fib 0 = 1
  | fib 1 = 1
  | fib n =
    let
        val (a,b) =  fork false
                         (
                           (fn () => (fib (n-1))),
                           (fn () => (fib (n-2)))
                         )
        (*val (a,b) = (fib (n-1), fib (n-2)) *)
    in
        a + b
    end

fun print_results () = print (get_results ());

fun fibs n =
    let val nf = fib n in
        lastfibarg := n;
        lastfib := nf;
        show_status ();
        fibs (n + 1)
    end



fun forever () =
    let in
        print "Calling nextevent...";
        (case maskevent (MLX.Mask.make [MLX.Mask.buttonpress, MLX.Mask.buttonrelease]) of
             MLX.Button (true, _, _, _, _, x, y, _, _, _, b, _) =>
             let (* val _ = print "Button\n" *)
             in
                 clicks := (!clicks) + 1;
                 show_status ();
                 forever ()
             end
           | _ => forever ())
    end handle MLX.X s => (print ("exn: " ^ s ^ "\n"); forever ())

fun mb () =
    (print "Hi\n";
     case nextevent () of
         MLX.Motion (_, _, _, _, x, y, _, _, _, _, _) => forever ()
       | _ => mb ()) handle _ => mb ()

val _ = fork true (fn () => fibs 0, fn () => forever ())

val _ = closewindow ()
