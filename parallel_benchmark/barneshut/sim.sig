signature SIM =
sig
  (* skeleton structure for organizing particles and other data *)
  type t

  (* vector in 3-space *)
  type v = real * real * real
  (* bounding box *)
  type box
  (* the object of our simulation *)
  type particle

  (* given a bounding box, build an empty tree *)
  val empty : box -> t

  (* add a particle *)
  val addParticle : particle * t -> t

  (* calculate the force on each particle and update the velocity *)
           (* cutoff  del_t *)
  val update : int -> real -> t -> t

  (* interate over the tree *)
  val fold : (box * 'a -> 'a) -> (particle * 'a -> 'a) -> 'a -> t -> 'a
end
