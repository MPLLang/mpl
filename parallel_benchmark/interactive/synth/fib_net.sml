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

fun fib n =
    if n <= 1 then 1
    else
        let
            val fork = if n <= 20 then
                           (fn (f1, f2) => (f1 (), f2 ()))
                       else
                           MLton.Parallel.ForkJoin.fork
            val (a, b) = fork (fn () => fib (n - 1),
                               fn () => fib (n - 2))
        in
            (a + b)
        end

fun fibdo n =
    let val f = fib n
        val _ = print ("fib(" ^ (Int.toString n) ^ ") = " ^ (Int.toString (fib n)) ^ "\n")
        val finish = Time.now ()
        val diff = Time.-(finish, start)
        val diffi = LargeInt.toInt (Time.toMilliseconds diff)
    in
        print ("exectime " ^ (Int.toString diffi) ^ "\n");
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
        val _ = MLton.Parallel.FutureSuspend.futureLat (true, (fn () => inploop s))
    in
        acceptloop sock
    end)

in

MLton.Parallel.ForkJoin.forkLat true ((fn () => fibdo 43),
                                              (fn () => acceptloop sock))
(* val _ = loop fibs fibs *)
end

val _ = (main ())
        handle _ => (print "error\n"; ((), ()))
