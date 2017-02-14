open IO
open Network

type stat = {page : string,
             views : int}

val localhost =
    case NetHostDB.getByName "localhost" (* (NetHostDB.getHostName ()) *) of
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

val reqbuf = ref []
val stats = ref []

fun fib n =
    if n <= 1 then 1
    else
        let
            val fork = if n <= 25 then
                           (fn (f1, f2) => (f1 (), f2 ()))
                       else
                           MLton.Parallel.ForkJoin.fork
            val (a, b) = fork (fn () => fib (n - 1),
                               fn () => fib (n - 2))
        in
            (a + b)
        end

fun statloop () =
    let fun update_stats (r, ss) =
            if List.exists (fn {page, views} => page = r) ss then
                List.map (fn {page, views} =>
                             if page = r then
                                 {page = page, views = views + 1}
                             else
                                 {page = page, views = views})
                     ss
            else
                {page = r, views = 1}::ss
        fun comp_stats rs ss =
            List.foldl update_stats ss rs
        val reqs = !reqbuf
        val stats' = comp_stats reqs []
    in
        stats := stats';
        ignore (fib 42);
        ignore (MLton.Parallel.ForkJoin.fork ((fn () => ()), statloop))
    end

fun parse_request s =
    let val tokens = String.tokens Char.isSpace s in
        case tokens of
            rt::url::httpv::rest =>
            (let val urltokens = String.tokens (fn c => c = #"\\") url in
                 SOME (List.nth (urltokens, (List.length urltokens) - 1))
             end)
          | _ => NONE
    end

fun build_success s =
    "HTTP/1.0 200 OK\n" ^
    "Date: " ^
    (Date.fmt "%a, %d %b %Y %H:%M:%S %Z"
              (Date.fromTimeLocal (Time.now ()))) ^ "\n" ^
    "Content-Type: text/html\n" ^
    "Content-Length: " ^ (Int.toString (String.size s)) ^ "\n\n" ^
    s

fun build_inv_req () =
    "HTTP/1.0 400 Bad Request"

fun build_404 () =
    "HTTP/1.0 404 Not Found"

val dummy_page =
    "<html><head><title>Dummy page</title></head><body>Hi!</body></html>"

fun build_stat_page () =
    let fun build_row {page, views} =
            "<tr><td>" ^ page ^ "</td><td>" ^ (Int.toString views) ^
            "</td></tr>\n"
    in
        "<html><head><title>Server Statistics</title></head>\n" ^
        "<body>\n" ^
        "<h1>Server statistics</h1>\n" ^ (* as of " ^
        (Date.fmt "%a, %d %b %Y %H:%M:%S %Z"
                  (Date.fromTimeLocal (Time.now ()))) ^ "</h1>\n" ^ *)
        "<table border=1>\n" ^
        "<tr><td>Page</td><td>Views</tr>\n" ^
        (List.foldl op^ "" (List.map build_row (!stats))) ^
        "</table>" ^
        "</body></html>"
    end

(* Stolen from Tom Murphy VII's StringUtil *)
fun readfile f =
    (let
        val l = TextIO.openIn f
        val s = TextIO.inputAll l
    in
        TextIO.closeIn l; SOME s
    end)
    handle _ => NONE

fun inploop sock =
    let val _ = print "receiving\n"
        val req = (recvString (sock, 1024))
        val _ = print "received\n"
        (* val _ = print (req ^ "\n") *)
     in
         if String.size req = 0 then
             (Socket.close sock; ())
         else
             let val response =
                 case parse_request req of
                     SOME filename =>
                     (reqbuf := filename::(!reqbuf);
                      (* print ("Requested: " ^ filename ^ "\n"); *)
                     if String.isSubstring "stats.html" filename then
                         build_success (build_stat_page ())
                     else
                         (case readfile ("www" ^ filename) of
                              SOME s => build_success s
                            | NONE => build_404 ()))
                   | NONE => build_inv_req ()
             in
                 print "sending\n";
                 (* print (response ^ "\n"); *)
                 sendString (sock, response);
                 print "sent\n";
                 inploop sock
             end
     end


fun acceptloop sock =
    (let val _ = print "listening\n"
        val (s, _) = (accept sock)
        val _ = addtosocks s
        val _ = print "accepted\n"
        val _ = MLton.Parallel.FutureSuspend.futureLat
                    (true, (fn () => inploop s))
    in
        acceptloop sock
    end)

val _ = MLton.Parallel.ForkJoin.fork
            ((fn () => MLton.Parallel.Basic.event (fn () => acceptloop sock)),
            (* ((fn () => acceptloop sock), *)
             statloop)
