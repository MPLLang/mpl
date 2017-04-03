structure Mailboxes :> MAILBOXES =
struct

  exception Mailboxes

  val P = MLton.Parallel.numberOfProcessors
  val cas = MLton.Parallel.compareAndSwap

  type 'a mailbox = {flag : int ref, mail : 'a ref}
  type 'a t = 'a mailbox vector

  val MAIL_WAITING = 0
  val MAIL_RECEIVING = 1

  fun new d =
    Vector.tabulate (P, fn _ => {flag = ref MAIL_WAITING, mail = ref d})

  fun getMail mailboxes p =
    let val {flag, mail} = Vector.sub (mailboxes, p)
    in if !flag = MAIL_RECEIVING
       then (flag := MAIL_WAITING; !mail)
       else getMail mailboxes p
    end

  fun sendMail mailboxes (p, m) =
    let val {flag, mail} = Vector.sub (mailboxes, p)
    in ( mail := m
       ; if cas (flag, MAIL_WAITING, MAIL_RECEIVING) then ()
         else raise Mailboxes
       )
    end

end
