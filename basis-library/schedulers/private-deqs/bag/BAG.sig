(* Author: Sam Westrick (swestric@cs.cmu.edu) *)

(* A bag data structure which allows concurrent insertions, but eventually will
 * be "dumped", at which point all following insertions will fail. Dumping a
 * bag returns all of the elements of that bag as a list. If dumping fails (by
 * returning NONE), then someone else must have dumped the bag. *)
signature BAG =
sig
  type 'a t

  val new : unit -> 'a t
  val insert : 'a t * 'a -> bool
  val isDumped : 'a t -> bool
  val dump : 'a t -> 'a list option
end
