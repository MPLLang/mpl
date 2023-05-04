structure ForkStrategy =
struct
  datatype t =
    GreedyWorkAmortized
  | NaivePCall
  | EagerHeuristic
end