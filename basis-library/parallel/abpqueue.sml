functor ABPQueue (structure T : sig type entry end) =
struct
type t = MLton.Pointer.t

val getRef = _import "getRef" runtime private: MLton.Pointer.t -> entry ref

val new = _import "ABP_newDeque" runtime private: unit -> queue;

val pushBottomC = _import "ABP_pushBottom" runtime private:
                 queue -> entry ref -> int

fun pushBottom (q: queue) (e: entry) : bool =
    pushBottomC q (ref e) = 0

val popTopC = _import "ABP_popTop" runtime private: queue -> MLton.Pointer.t


fun popTop (q: queue) : entry option =
    let val retval = popTopC q
    in
        case MLton.Pointer.compare (retval, MLton.Pointer.null) of
            EQ => NONE
          | _ => SOME (!(getRef retVal))
    end

val popBottomC = _import "ABP_popBottom" runtime private: queue -> MLton.Pointer.t

fun popBottom (q: queue) : entry option =
    let val retval = popBottomC q
    in
        case MLton.Pointer.compare (retval, MLton.Pointer.null) of
            EQ => NONE
          | _ => SOME (!(getRef retVal))
    end

end
