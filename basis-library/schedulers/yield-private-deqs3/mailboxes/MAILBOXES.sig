(* Author: Sam Westrick (swestric@cs.cmu.edu) *)

(* A group of mailboxes is indexed by the worker id. Workers may send mail to a
 * specific worker, or wait for mail to arrive (from any worker). It is an error
 * for multiple workers to send mail to a single receiver at the same time. *)
signature MAILBOXES =
sig
  type 'a t

  (* Allocates a new group of mailboxes with a default initial value in each *)
  val new : 'a -> 'a t

  (* `getMail ms i` waits until the ith mailbox in ms receives mail *)
  val getMail : 'a t -> int -> 'a

  (* `sendMail ms (i, x)` sends x to the ith mailbox in ms *)
  val sendMail : 'a t -> (int * 'a) -> unit
  val sendMailLockFree : 'a t -> (int * 'a) -> unit
end
