functor LamISys (structure I : INTERACTIVE
                 structure S : MLTON_PARALLEL_SYNCVAR) =
struct

open I

datatype sig_ref = Sig of (Posix.Signal.signal * sig_ref) S.t
val next_sig : sig_ref ref = ref (Sig (S.empty ()))
fun handle_sig s () =
    let val new_sig = S.empty ()
        val Sig sv = !next_sig
        val _ = print "signal!\n"
    in
        S.write (sv, (s, Sig new_sig));
        next_sig := Sig new_sig
    end

exception SyncVarOverflow

val signals =
    let fun sig_gen sigs () =
            let val (_, (s, Sig ns)) =
                    S.read sigs
                    handle Overflow => raise SyncVarOverflow
            in
                (s, eftr "signals" (sig_gen ns))
            end
        val Sig first_sigs = !next_sig
    in
        eftr "signals" (sig_gen first_sigs)
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
            case TextIO.StreamIO.input1 is of
                SOME (c, is') => (c, eftr "inchar" (inchar_gen is'))
              | NONE => OS.Process.exit OS.Process.success
    in
        eftr "inchar" (inchar_gen (TextIO.getInstream TextIO.stdIn))
    end

end

structure LamISysFutSusp = LamISys (structure I = LamIFut
                                    structure S = MLton.Parallel.SyncVarSuspend)
