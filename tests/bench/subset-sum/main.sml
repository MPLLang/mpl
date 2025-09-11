structure CLA = CommandLineArgs

val bag_str = CLA.parseString "bag" "3,2,3,1,2,1,5,10,10000000,30,10000"
val goal = CLA.parseInt "goal" 10010021
val unsafe_skip_table_set = CLA.parseFlag "unsafe_skip_table_set"

val bag =
  Seq.fromList (List.map (valOf o Int.fromString)
    (String.tokens (fn c => c = #",") bag_str))
  handle _ => Util.die ("parsing -bag ... failed")

val _ =
  if Util.all (0, Seq.length bag) (fn i => Seq.nth bag i > 0) then ()
  else Util.die ("bag elements must be all >0")

val _ = if goal >= 0 then () else Util.die ("goal must be >=0")

val bag_str = let val s = Seq.toString Int.toString bag
              in String.substring (s, 1, String.size s - 2)
              end
val _ = print ("bag  " ^ bag_str ^ "\n")
val _ = print ("goal " ^ Int.toString goal ^ "\n")
val _ = print
  ("unsafe_skip_table_set? " ^ (if unsafe_skip_table_set then "yes" else "no")
   ^ "\n")

val result = Benchmark.run "subset-sum" (fn () =>
  SubsetSumTiled.subset_sum {unsafe_skip_table_set = unsafe_skip_table_set}
    (bag, goal))

val out_str =
  case result of
    NONE => "NONE"
  | SOME x => "SOME " ^ Seq.toString Int.toString x

val _ = print ("result " ^ out_str ^ "\n")
