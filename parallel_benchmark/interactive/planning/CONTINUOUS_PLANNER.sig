signature CONTINUOUS_PLANNER =
sig

    structure G = ChunkedTreeSequence

    (* width -> height -> obstacle seq -> start -> goal -> plan *)
    val plan: int -> int -> obst G.seq -> int * int -> int * int ->
              (int * int) list

end
