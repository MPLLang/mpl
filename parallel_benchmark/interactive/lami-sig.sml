signature INTERACTIVE =
sig

type ('f, 'p, 'r) gftr
type ('p, 'r) ftr = ('p, 'p, 'r) gftr
type ('p, 'r) fftr = (unit, 'p, 'r) gftr
type ('p, 'r) view = 'r * ('p, 'r) ftr

datatype ('p, 'r) aview =
         Now of ('p, 'r) view
         | Later of ('p, 'r) fftr

type ('f, 'p, 'r) egftr
type ('p, 'r) eftr = ('p, 'p, 'r) egftr
type ('p, 'r) efftr = (unit, 'p, 'r) egftr
type ('p, 'r) eview = 'r * ('p, 'r) eftr
datatype ('p, 'r) eaview =
         ENow of ('p, 'r) eview
         | ELater of ('p, 'r) efftr

(** Indicates that a factor has been used twice. *)
exception Dead_ftr

(** Indicates that a factor has been used twice, and gives the name of the
factor. *)
exception Dead_ftr_name of string

val is_fut : ('f, 'p, 'r) gftr -> bool
val eis_fut : ('f, 'p, 'r) egftr -> bool

(** Constructs a factpr from the given generator. *)
val ftr : ('p -> ('p, 'r) view) -> ('p, 'r) ftr

(** Constructs an input stream from the given ticker. *)
val eftr : string -> ('p -> ('p, 'r) eview) -> ('p, 'r) eftr

(** [query s t] queries the interactible s with prompt t.
Returns a response and continuation
@raise Dead_itr if s has already been used
@raise Dead_itr_name with the name of s if s has already been used
*)
val query : ('f, 'p, 'r) gftr -> 'f -> ('p, 'r) view
val aquery : ('f, 'p, 'r) gftr -> 'f -> ('p, 'r) aview

(** [equery s t] queries the external interactible s with prompt t.
Returns a response and continuation.
@raise Dead_itr_name with the name of s if s has already been used
*)
val equery : ('f, 'p, 'r) egftr -> 'f -> ('p, 'r) eview
val eaquery : bool -> ('f, 'p, 'r) egftr -> 'f -> ('p, 'r) eaview

(** Splits an external interactible. Returns two interactibles partitioning
the interactible.
If the interactible is named <i>name</i>, the returned interactibles
will be named <i>name1</i> and <i>name2</i>.
@raise Dead_itr_name with the name of s if s has already been used
*)
val split : ('p, 'r) eftr -> ('p, 'r) eftr * ('p, 'r) eftr

end
