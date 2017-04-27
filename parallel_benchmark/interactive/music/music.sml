open IO
open Network

fun serve_thread sock =
    let (* val _ = print "receiving\n" *)
        val req = (recvString (sock, 128))
        val _ = print "Received request\n"
        val _ = print (req ^ "\n")
     in
         if String.size req = 0 then
             (Socket.close sock; ())
         else
             let val fd = Posix.FileSys.openf(req, Posix.FileSys.O_RDONLY,
                             Posix.FileSys.O.flags [])
                          (* handle _ =>
                                 (sendString (sock, "0");
                                  Socket.close sock;
                                  ()) *)
                 (* val _ = print "opened file\n" *)
                 val stat = Posix.FileSys.stat req
                 val size = Posix.FileSys.ST.size stat
                 val _ = sendString (sock, Int64.toString size)
                 val _ = print ("sent size: " ^ (Int64.toString size) ^ "\n")
                 val _ = recvString (sock, 3)
                 fun serve_loop () =
                     let val arr = Array.array (1024 * 256, Word8.fromInt 0)
                         val read = Posix.IO.readArr (fd, ArraySlice.full arr)
                         fun send_loop slice =
                             if ArraySlice.length slice = 0 then ()
                             else
                                 let val sent = sendArr (sock, slice) in
                                     print ("sent " ^ (Int.toString sent) ^ " bytes\n");
                                     send_loop (ArraySlice.subslice
                                                    (slice, sent, NONE))
                                 end
                     in
                         if read > 0 then
                             (send_loop (ArraySlice.slice (arr, 0, SOME read));
                              serve_loop ())
                         else
                             (print "end of file\n";
                               ())
                     end
             (*
                     case Int.fromString (recvString (sock, 16)) of
                         NONE => (print "invalid request\n";
                                  Socket.close sock; ())
                       | SOME bytes =>
                         (if bytes = 0 then (print "invalid request\n";
                                             Socket.close sock; ())
                          else
                              let val _ = print "got request\n"
                                  val arr = Array.array (bytes, Word8.fromInt 0)
                                  val read = Posix.IO.readArr (fd, ArraySlice.full arr)
                              in
                                  if read > 0 then
                                      (sendArr (sock, ArraySlice.slice (arr, 0, SOME read));
                                       print ("sent " ^ (Int.toString read) ^ "bytes\n");
                                       serve_loop ())
                                  else
                                      (print "end of file\n";
                                       Socket.close sock; ())
                              end)
                     *)
             in
                 serve_loop ()
             end
    end

fun server () =
    let
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
        val _ = Socket.Ctl.setSNDBUF (sock, 1024 * 1024)
        val _ = Socket.bind (sock, addr)
        val _ = Socket.listen (sock, 1)
        fun acceptloop sock =
            (let (* val _ = print "listening\n" *)
                 val (s, _) = (accept sock)
                 val _ = addtosocks s
                 val prios = recvString (s, 1)
                 val _ = print ("accepted a new " ^ prios ^ " client\n")
                 val _ =
                     case prios of
                         "H" => (* high priority client *)
                         (ignore (MLton.Parallel.FutureFGBG.highbg
                                      (fn () => serve_thread s));
                          ())
                       | "L" => (* low priority client *)
                         (ignore (MLton.Parallel.FutureFGBG.bg
                                      (fn () => serve_thread s));
                          ())
                       | _ => print "invalid priority"
             in
                 acceptloop sock
             end)
    in
        acceptloop sock
    end

fun client prio host file =
    let val hostentry =
            case NetHostDB.getByName host of
                NONE => (print "no host name?\n"; OS.Process.exit OS.Process.failure)
              | SOME addr => NetHostDB.addr addr
        val sock = INetSock.TCP.socket ()
        val _ = Socket.Ctl.setREUSEADDR (sock, true)
        val _ = OS.Process.atExit (fn () => Socket.close sock)
        val addr = INetSock.toAddr (hostentry, 8000)
        val _ = connect (sock, addr)

        val _ = sendString (sock, prio)

        val _ = sendString (sock, file)
        val szstr = recvString (sock, 64)
        (* val _ = print ("got size: " ^ szstr ^ "\n") *)
        val fd = Posix.FileSys.openf(file, Posix.FileSys.O_RDONLY,
                                     Posix.FileSys.O.flags [])
        val _ = sendString (sock, "ACK")
        val bufsize = 1024 * 1024
        val buf = Array.array (bufsize, Word8.fromInt 0)
        (* val buf2 = Array.array (bufsize, Word8.fromInt 0) *)
        val totrecvd = ref 0
        fun getbytes buf =
            recvArr (sock, ArraySlice.full buf)
(*
            let (* val _ = sendString (sock, Int.toString (Array.length buf)) *)
                fun loop slice =
                    let val n = recvArr (sock, slice)
                        val _ = print ("got " ^ (Int.toString n) ^ " bytes\n")
                    in
                        if n < ArraySlice.length slice then
                            loop (ArraySlice.subslice (slice, n, NONE))
                        else
                            Array.length buf
                    end
            in
                loop (ArraySlice.full buf)
            end
*)
        fun getfdbytes buf =
            Posix.IO.readArr (fd, ArraySlice.full buf)
        fun infloop () = infloop ()
        fun loop buf music =
            let (* val _ = print "getting more bytes\n" *)
                val read = getbytes buf
                val buf2 = Array.array (read, Word8.fromInt 0)
                val _ = getfdbytes buf2
                val _ = totrecvd := (!totrecvd) + read
                (* val _ = print ("got " ^ (Int.toString read) ^ " bytes\n")
                val _ = print ("got " ^ (Int.toString (!totrecvd)) ^ " bytes total\n") *)
            in
                if read = 0 then infloop ()
                else
                    (addData (music, buf, read);
                     loop buf music)
            end
    in
        case Int.fromString szstr of
            NONE => (print "Invalid response"; ())
          | SOME sz =>
            if sz = 0 then (print "Invalid response"; ())
            else
                let (* val _ = print "getting bytes\n" *)
                    val read = getbytes buf
                    val buf2 = Array.array (read, Word8.fromInt 0)
                    val _ = getfdbytes buf2
                    (* val _ = print ("got " ^ (Int.toString read) ^ " bytes\n")
                    val _ = print "got bytes\n" *)
                    val music = init (sz, buf, read)
                in
                    (* print "initialized\n"; *)
                    play music;
                    (* print "playing\n"; *)
                    loop buf music
                end
    end

val _ = case CommandLine.arguments () of
            prio::host::file::[] => client prio host file
          | [] => server ()
          | _ => print "Invalid command line arguments"
