functor LamISys (structure I : INTERACTIVE
                 structure S : MLTON_PARALLEL_SYNCVAR) =
struct

open I

datatype sig_ref = Sig of int * (Posix.Signal.signal * sig_ref) S.t
val next_sig : sig_ref ref = ref (Sig (0, S.empty ()))
fun handle_sig s () =
    let val new_sig = S.empty ()
        val Sig (n, sv) = !next_sig
        val _ = print "signal!\n"
    in
        next_sig := Sig (n+1, new_sig);
        (* print ("writing to " ^ (Int.toString n) ^ "\n"); *)
        S.write (sv, (s, Sig (n + 1, new_sig)))
        (* print "wrote\n" *)
    end

exception SyncVarOverflow

val signals =
    let fun sig_gen n sigs () =
            let (*  val Sig sigs = !next_sig *)
                val (_, (s, Sig (n', ns))) =
                    (* print ("reading from " ^ (Int.toString n) ^ "\n");*)
                     S.read sigs
                     (* before print "read\n" *)
                    handle Overflow => raise SyncVarOverflow
            in
                (s, eftr "signals" (sig_gen n' ns))
            end
        val Sig (n, first_sigs) = !next_sig
    in
        eftr "signals" (sig_gen n first_sigs)
    end

fun capture_signals sigs =
    let fun do_one s =
            MLton.Signal.setHandler
                (s, MLton.Signal.Handler.simple (handle_sig s));;
    in
        List.map do_one sigs
    end

val inchar =
    let fun inchar_gen is () =
            case IO.input1 is of
                SOME (c, is') => (c, eftr "inchar" (inchar_gen is'))
              | NONE =>
                (print "NONE\n";
                 OS.Process.exit OS.Process.success)
    in
        eftr "inchar" (inchar_gen (TextIO.getInstream TextIO.stdIn))
    end

end

structure LamISysFutSusp = LamISys (structure I = LamIFut
                                    structure S = MLton.Parallel.SyncVarSuspend)
