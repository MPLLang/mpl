structure Mailbox :> MAILBOX =
struct

val vcas = MLton.Parallel.compareAndSwap
fun cas (r, old, new) = vcas r (old, new) = old

type 'a t = {flag : int ref, mail : 'a option ref}

val NOT_WAITING = ~1
val WAITING = ~2

exception Mailbox

datatype status =
         NotWaiting
         | Waiting
         | Claimed of int

fun new () =
    {flag = ref WAITING, mail = ref NONE}

fun flag_to_status flag =
    if flag = NOT_WAITING then
        NotWaiting
    else if flag = WAITING then
        Waiting
    else if flag >= 0 then
        Claimed flag
    else
        raise Mailbox

fun status_to_flag NotWaiting  = NOT_WAITING
  | status_to_flag Waiting     = WAITING
  | status_to_flag (Claimed p) = p

fun status {flag, mail} =
    flag_to_status (!flag)

fun checkMail (mb as {flag, mail}) =
    case flag_to_status (!flag) of
        NotWaiting => NONE
      | Waiting => NONE
      | Claimed _ =>
        (case !mail of
             NONE => checkMail mb
           | SOME x => SOME x)

fun getMail (mb as {flag, mail}) =
    case checkMail mb of
        NONE => NONE
      | SOME x => SOME x
                  before (flag := (status_to_flag NotWaiting);
                          mail := NONE)

fun tryClaim {flag, mail} p =
    cas (flag, status_to_flag Waiting, status_to_flag (Claimed p))

fun sendMail {flag, mail} m =
    mail := SOME m

fun tryClear (mb as {flag, mail}) =
    case checkMail mb of
        NONE =>
        if cas (flag, status_to_flag Waiting, status_to_flag NotWaiting) then
            NONE
        else
            tryClear mb
      | SOME x => SOME x
                  before (flag := (status_to_flag Waiting);
                          mail := NONE)

fun setWaiting (mb as {flag, mail}) =
    case flag_to_status (!flag) of
        NotWaiting => flag := status_to_flag Waiting
      | _ => ()

end
