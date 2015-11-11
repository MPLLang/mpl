open IO

val stdin = ref (TextIO.getInstream TextIO.stdIn)

fun inputLine () =
    let fun iL_int line =
            case IO.input1 (!stdin) of
                NONE => NONE
              | SOME (c, is') =>
                (stdin := is';
                 if c = #"\n" then
                     SOME line
                 else
                     iL_int (line ^ (str c)))
    in
        iL_int ""
    end

fun inploop () =
    case inputLine () of
        NONE => OS.Process.exit OS.Process.success
      | SOME l =>
        case String.compare (l, "done") of
            EQUAL => OS.Process.exit OS.Process.success
          | _ =>
            (print ("Hi, " ^ l ^ "\n");
             inploop ())

val _ = inploop ()
