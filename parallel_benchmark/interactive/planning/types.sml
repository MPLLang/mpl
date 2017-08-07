(* adjacency list *)
type graph = (real * int) Vector.vector Vector.vector

(* ((x, y), (x, y)) -> Collision *)
type obst = (int * int) * (int * int) -> bool
