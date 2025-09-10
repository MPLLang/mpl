structure CLA = CommandLineArgs

val n = CLA.parseInt "N" (100 * 1000 * 1000)
val block_size_factor = CLA.parseReal "block-size-factor" 16.0
val bits = CLA.parseInt "bits" 64
val report_times = CLA.parseFlag "report-times"

val _ = print ("N " ^ Int.toString n ^ "\n")
val _ = print ("block-size-factor " ^ Real.toString block_size_factor ^ "\n")
val _ = print ("bits " ^ Int.toString bits ^ "\n")
val _ = print ("report-times? " ^ (if report_times then "yes" else "no") ^ "\n")

functor Main
  (I:
   sig
     type t
     val from_int: int -> t
     val to_int: t -> int
     val to_string: t -> string
   end) =
struct
  structure Primes = SegmentedPrimes(I)

  fun main () =
    let
      val params =
        {block_size_factor = block_size_factor, report_times = report_times}
      val msg = "generating primes up to " ^ Int.toString n

      val result = Benchmark.run msg (fn _ =>
        Primes.primes_with_params params n)

      val numPrimes = Seq.length result
      val _ = print ("number of primes " ^ Int.toString numPrimes ^ "\n")
      val _ = print
        ("result " ^ Util.summarizeArraySlice 8 I.to_string result ^ "\n")
    in
      ()
    end
end

structure Main32 =
  Main
    (struct
       type t = Int32.int
       val from_int = Int32.fromInt
       val to_int = Int32.toInt
       val to_string = Int32.toString
     end)

structure Main64 =
  Main
    (struct
       type t = Int64.int
       val from_int = Int64.fromInt
       val to_int = Int64.toInt
       val to_string = Int64.toString
     end)

val _ =
  case bits of
    64 => Main64.main ()
  | 32 => Main32.main ()
  | _ => Util.die ("unknown -bits " ^ Int.toString bits ^ ": must be 32 or 64")
