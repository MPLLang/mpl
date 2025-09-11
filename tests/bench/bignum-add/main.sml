structure CLA = CommandLineArgs
structure Seq = ArraySequence

structure Add = MkAdd(DelayedSeq)

val n = CLA.parseInt "n" (1000 * 1000 * 100)
val seed = CLA.parseInt "seed" 15210
val doCheck = CLA.parseFlag "check"

val _ = print ("n " ^ Int.toString n ^ "\n")

val input1 = Bignum.generate n seed
val input2 = Bignum.generate n (seed + n)

fun task () =
  Add.add (input1, input2)

fun check result =
  if not doCheck then () else
  let
    val (correctResult, tm) =
      Util.getTime (fn _ => SequentialAdd.add (input1, input2))
    val _ = print ("sequential " ^ Time.fmt 4 tm ^ "s\n")
    val correct =
      Seq.equal op= (result, correctResult)
  in
    if correct then
      print ("correct? yes\n")
    else
      print ("correct? no\n")
  end

val result = Benchmark.run "bignum add" task
val _ = check result

(* val _ = print ("result " ^ IntInf.toString (Bignum.toIntInf result) ^ "\n") *)
