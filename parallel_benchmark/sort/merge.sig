signature MERGE =
sig

  type t

  (* split a sorted sequence into two parts at the given element *)
  val split : { cutoff : int, fallback : real -> t -> t list * t list } -> real -> t -> t list * t list

  (* merge two sorted sequences *)
  val merge : { cutoff : int, fallback : real -> t -> t list * t list } (* split fallback *)
              -> { cutoff : int, fallback : t * t -> t } (* merge fallback *)
              -> t * t -> t

end
