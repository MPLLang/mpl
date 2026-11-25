(* ==========================================================================
 * VERSION 1: NO GRANULARITY CONTROL
 * ==========================================================================
 *)


(* compute the sum of f(lo), f(lo+1), ..., f(hi-1) *)
fun sum (lo, hi, f) =
  if lo >= hi then
    0
  else if lo + 1 = hi then
    f lo
  else
    let
      val mid = lo + (hi - lo) div 2
      val (left, right) = ForkJoin.par (fn () => sum (lo, mid, f), fn () =>
        sum (mid, hi, f))
    in
      left + right
    end


(* queens at positions (row, col) *)
type locations = (int * int) list


fun queen_is_threatened (i, j) (other_queens: locations) =
  List.exists
    (fn (x, y) => i = x orelse j = y orelse i - j = x - y orelse i + j = x + y)
    other_queens


fun nqueens_count_solutions n =
  let
    fun search i queens =
      if i >= n then
        1
      else
        let
          fun do_column j =
            if queen_is_threatened (i, j) queens then 0
            else search (i + 1) ((i, j) :: queens)
        in
          sum (0, n, do_column)
        end
  in
    search 0 []
  end


(* ==========================================================================
 * VERSION 2: MANUAL GRANULARITY CONTROL
 * ==========================================================================
 *)


(* sequential alternative *)
fun sum_serial (lo, hi, f) =
  Util.loop (lo, hi) 0 (fn (acc, i) => acc + f i)


fun nqueens_count_solutions_manual_gran_control n =
  let
    fun search i queens =
      if i >= n then
        1
      else
        let
          fun do_column j =
            if queen_is_threatened (i, j) queens then 0
            else search (i + 1) ((i, j) :: queens)
        in
          if i >= 3 then
            (* simple heuristic for granularity control: swich to sequential
             * algorithm after getting a few levels deep.
             *)
            sum_serial (0, n, do_column)
          else
            sum (0, n, do_column)
        end
  in
    search 0 []
  end


(* ==========================================================================
 * parse command-line arguments and run
 * ==========================================================================
 *)

val n = CommandLineArgs.parseInt "N" 13
val do_gran_control = CommandLineArgs.parseFlag "do-gran-control"
val _ = print ("N " ^ Int.toString n ^ "\n")
val _ = print
  ("do-gran-control? " ^ (if do_gran_control then "yes" else "no") ^ "\n")

val msg =
  "counting number of " ^ Int.toString n ^ "x" ^ Int.toString n ^ " solutions"

val result = Benchmark.run msg (fn _ =>
  if do_gran_control then nqueens_count_solutions_manual_gran_control n
  else nqueens_count_solutions n)

val _ = print ("result " ^ Int.toString result ^ "\n")
