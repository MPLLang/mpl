val fib_type = _import * : DynLink.fptr -> int -> int;
val open_close_type = _import * : DynLink.fptr -> unit -> unit;

fun loop () =
    case TextIO.inputLine TextIO.stdIn of
        NONE => ()
      | SOME line =>
        let val filename = line (* String.extract (line, 1, NONE) *)
            val _ = print "Compiling...\n"
        in
            case CompileDL.compile filename of
                SOME so =>
                let val _ = print ("Loading " ^ so ^ "...\n")
                    val dlso = DynLink.dlopen ("./" ^ so, DynLink.RTLD_LAZY)
                    val fib_fptr = DynLink.dlsym (dlso, "fib")
                    val open_fptr = DynLink.dlsym (dlso, "fib_open")
                    val close_fptr = DynLink.dlsym (dlso, "fib_close")
                    val fib = fib_type fib_fptr
                    val fib_open = open_close_type open_fptr
                    val fib_close = open_close_type close_fptr
                    val _ = print "Loaded\n"
                    val _ = fib_open ()
                    val _ = print "Running...\n"
                    val n = fib 40
                    val _ = fib_close ()
                    val _ = DynLink.dlclose dlso
                in
                    print ("fib(40) = " ^ (Int.toString n) ^ "\n");
                    loop ()
                end
              | NONE => (print "Compile error.\n";
                         loop ())
        end

val _ = loop ()
