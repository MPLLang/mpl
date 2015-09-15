structure IO : MLTON_PARALLEL_IO =
struct

structure B = MLton.Parallel.Basic

fun input1 (is: TextIO.StreamIO.instream) : (char * TextIO.StreamIO.instream) option =
    let fun f () =
            ((* print "in f\n"; *)
            (case TextIO.StreamIO.canInput (is, 1) of
                 NONE => false
               | SOME _ => true)
            (* before
            print "out f\n" *))
    in
        if f () then
            TextIO.StreamIO.input1 is
        else
            (* Performing the input would block *)
            (B.suspend (fn t => B.addtoio (t, f));
             input1 is)
    end

end
