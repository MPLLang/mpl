val localhost =
    case NetHostDB.getByName (NetHostDB.getHostName ()) of
        NONE => (print "no host name?\n"; OS.Process.exit OS.Process.failure)
      | SOME addr => NetHostDB.addr addr
val sock = INetSock.TCP.socket ()
val _ = OS.Process.atExit (fn () => Socket.close sock)
val addr = INetSock.toAddr (localhost, 8000)
val _ = Socket.connect (sock, addr)

fun sendString (sock, str) =
    let val arr = Array.tabulate (String.size str,
                                 fn i => Word8.fromInt (Char.ord (String.sub (str, i))))
        val slice = ArraySlice.slice (arr, 0, NONE)
    in
        Socket.sendArr (sock, slice)
    end

fun recvString (sock, n) =
    let val v = Socket.recvVec (sock, n)
    in
        Vector.foldl (fn (c, s) =>
                         s ^ (String.str (Char.chr (Word8.toInt c))))
                     ""
                     v
    end

fun loop () =
    let val s = case TextIO.inputLine TextIO.stdIn of
                    SOME s => s
                  | NONE => OS.Process.exit OS.Process.success
        val _ = sendString (sock, s)
        val _ = print "sent!\n"
        val r = recvString (sock, 256)
    in
        print (r ^ "\n");
        loop ()
    end

val _ = loop () handle _ => OS.Process.exit OS.Process.failure
