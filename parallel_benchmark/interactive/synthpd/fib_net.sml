open IO
open Network
(* val fork = MLton.Parallel.ForkJoin.fork *)

(*
fun print s =
    (TextIO.print s;
     TextIO.output (TextIO.stdErr, s);
     TextIO.flushOut TextIO.stdErr)
*)

fun main () =
let
val localhost =
    case NetHostDB.getByName "localhost" of
        NONE => (print "no host name?\n"; OS.Process.exit OS.Process.failure)
      | SOME addr => NetHostDB.addr addr
val sock = INetSock.TCP.socket ()
val socks = ref [(fn () => (Socket.close sock) handle _ => print "at exit\n")]
fun addtosocks s =
    MLton.Thread.atomically
        (fn () => socks := (fn () => (Socket.close s)
                           handle _ => print "at exit\n")::(!socks))
val _ = OS.Process.atExit (fn () => List.app (fn f => f ()) (!socks))

val addr = INetSock.toAddr (localhost, 8000)
val _ = Socket.Ctl.setREUSEADDR (sock, true)
val _ = Socket.bind (sock, addr)
val _ = Socket.listen (sock, 1)

val start = Time.now ()

val stdin = ref (TextIO.getInstream TextIO.stdIn)

fun seqfib n =
    if n <= 1 then 1
    else
        let
            val (a, b) = (seqfib (n - 1), seqfib (n - 2))
        in
            (a + b)
            (* handle Overflow =>
                   OS.Process.exit OS.Process.success *)
        end

fun fib n =
    if n <= 1 then 1
    else if n <= 20 then seqfib n
    else
        let
(*
            val _ = if n >= 39 then
                        print ("fib " ^ (Int.toString n) ^ " on " ^
                               (Int.toString
                                    (MLton.Parallel.Basic.processorNumber ()))
                               ^ "\n")
                    else ()
*)
            val fork = MLton.Parallel.ForkJoin.fork
            val (a, b) = fork (fn () => fib (n - 1),
                               fn () => fib (n - 2))
        in
            (a + b)
            (* handle Overflow =>
                   OS.Process.exit OS.Process.success *)
        end

fun fibdo n =
    let val f = fib n
        val _ = print ("fib(" ^ (Int.toString n) ^ ") = " ^ (Int.toString f) ^ "\n")
        val finish = Time.now ()
        val diff = Time.-(finish, start)
        val diffi = LargeInt.toInt (Time.toMilliseconds diff)
    in
        print ("exectime " ^ (aIntToString diffi) ^ "\n");
        OS.Process.exit OS.Process.success
    end

fun inploop sock =
    (let (* val _ = print "receiving\n" *)
        val s = (recvString (sock, 256))
                handle e => (print "here 82\n"; raise e)
        (* val _ = print "received\n" *)
     in
         if String.size s = 0 then
             (Socket.close sock; ())
             handle e => (print "here 87\n"; raise e)
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
                         handle e => (print "here 99\n"; raise e)
        val _ = addtosocks s
        val _ = print "accepted\n"
        val _ = MLton.Parallel.FutureSuspend.futureLat (true, (fn () => inploop s))
        val _ = MLton.Parallel.ForkJoin.fork ((fn () => acceptloop sock),
                                              (fn () => inploop s))
    in
        MLton.Parallel.FutureFGBG.bg (fn () => ())
    end)

in

MLton.Parallel.ForkJoin.fork ((fn () => MLton.Parallel.FutureFGBG.fg
                                            (fn () => acceptloop sock)),
                                      (fn () => fibdo 45)
                                      )

(*
MLton.Parallel.ForkJoin.forkLat true ((fn () => fibdo 43),
                                              (fn () => acceptloop sock))
*)
(* val _ = loop fibs fibs *)
end

val _ = (main ())
