signature DISCRETE_PLANNER =

(* graph -> start -> goal -> plan option *)
val plan : graph -> int -> int -> (int -> real) -> int list option

end
