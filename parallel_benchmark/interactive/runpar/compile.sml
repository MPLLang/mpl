signature COMPILE_DL =
sig
    val compile : string -> string option
end

structure CompileDL =
struct

exception UnsupportedOS

fun compile (filename: string) =
    (let open MLton.Platform.OS
        val root::_ = String.tokens (fn c => c = #".") filename
        val out =
            case host of
                Linux => (root ^ ".so")
              | _ => raise UnsupportedOS
        val cmd =
            case host of
                Linux => ("mlton -default-ann 'allowFFI true' -output " ^ out ^
                          " -format library -libname fib -cc-opt -shared " ^ filename)
                (* ("mlton -default-ann 'allowFFI true' -output test.so -format library " ^ filename) *)
              | _ => raise UnsupportedOS
    in
        (if OS.Process.isSuccess (OS.Process.system cmd) then
             SOME out
         else NONE)
        handle OS.SysErr _ => NONE
    end)
    handle UnsupportedOS => NONE

end
