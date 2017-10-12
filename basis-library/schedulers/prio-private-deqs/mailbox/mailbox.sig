signature MAILBOX =
sig

    type 'a t

    datatype status =
             NotWaiting
             | Waiting
             | Claimed of int

    exception Mailbox

    val new: unit -> 'a t
    val status: 'a t -> status

    val getMail: 'a t -> 'a option
    val tryClaim: 'a t -> int -> bool
    val sendMail: 'a t -> 'a -> unit
    val tryClear: 'a t -> 'a option
    val setWaiting: 'a t -> unit

end
