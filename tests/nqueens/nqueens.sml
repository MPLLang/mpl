structure CLA = CommandLineArgs

type board = (int * int) list

fun threatened (i,j) [] = false
  | threatened (i,j) ((x,y)::Q) =
    i = x orelse j = y orelse i-j = x-y orelse i+j = x+y
    orelse threatened (i,j) Q

structure Seq = FuncSequence

fun countSol n =
  let
    fun search i b =
      if i >= n then 1 else
      let
        fun tryCol j =
          if threatened (i, j) b then 0 else search (i+1) ((i,j)::b)
      in
        if i >= 3 then
          (* if we're already a few levels deep, then just go sequential *)
          Seq.iterate op+ 0 (Seq.tabulate tryCol n)
        else
          Seq.reduce op+ 0 (Seq.tabulate tryCol n)
      end
  in
    search 0 []
  end

val n = CommandLineArgs.parseInt "N" 13
val _ = print ("N " ^ Int.toString n ^ "\n")

val msg =
  "counting number of " ^ Int.toString n ^ "x" ^ Int.toString n ^ " solutions"

val result = Benchmark.run msg (fn _ => countSol n)

val _ = print ("result " ^ Int.toString result ^ "\n")

