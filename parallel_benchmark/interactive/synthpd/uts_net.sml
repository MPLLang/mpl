open IO
open Network

val localhost =
    case NetHostDB.getByName (NetHostDB.getHostName ()) of
        NONE => (print "no host name?\n"; OS.Process.exit OS.Process.failure)
      | SOME addr => NetHostDB.addr addr
val sock = INetSock.TCP.socket ()
val socks = ref [(fn () => Socket.close sock)]
fun addtosocks s =
    MLton.Thread.atomically
        (fn () => socks := (fn () => Socket.close s)::(!socks))
val _ = OS.Process.atExit (fn () => List.app (fn f => f ()) (!socks))

val addr = INetSock.toAddr (localhost, 8000)
val _ = Socket.Ctl.setREUSEADDR (sock, true)
val _ = Socket.bind (sock, addr)
val _ = Socket.listen (sock, 1)

val start = Time.now ()

val stdin = ref (TextIO.getInstream TextIO.stdIn)

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

fun inploop sock =
    (let (* val _ = print "receiving\n" *)
        val s = (recvString (sock, 256))
        (* val _ = print "received\n" *)
     in
         if String.size s = 0 then
             (Socket.close sock; ())
         else
             (print ("Hi, " ^ s ^ "\n");
              inploop sock)
(*
             (sendString (sock, "Hi, " ^ s);
              print "sent\n";
              inploop sock)
*)
     end)


fun acceptloop sock =
    (let (* val _ = print "listening\n" *)
        val (s, _) = (accept sock)
        val _ = addtosocks s
        (* val _ = print "accepted\n" *)
        (* val _ = MLton.Parallel.FutureSuspend.futureLat (true, (fn () => inploop s)) *)
        val _ = MLton.Parallel.ForkJoin.fork ((fn () => acceptloop sock),
                                              (fn () => inploop s))
    in
        MLton.Parallel.FutureFGBG.bg (fn () => ())
    end)(*  handle _ => MLton.Parallel.FutureFGBG.bg (fn () => ()) *)

val _ = MLton.Parallel.ForkJoin.fork
            ((fn () => MLton.Parallel.FutureFGBG.fg (fn () => acceptloop sock)),
             top_explore)
        handle _ => OS.Process.exit OS.Process.failure
