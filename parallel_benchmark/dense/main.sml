structure Main =
struct

  open MainUtils
  open Random

  val maxVal = ref 1000
  val tests = ref ["mmm"]

  local 
    fun processDenseArgs nil = ()
      | processDenseArgs (args as (arg::rest)) =

      if arg = "--tests" then
        processStringListArg processDenseArgs "tests" (setOption tests) rest

      else if arg = "--maxval" then
        processIntArg processDenseArgs "maxval" (setOption maxVal) rest

      else processArgs processDenseArgs args

  in
    val () = processDenseArgs (CommandLine.arguments ())
      handle Argument s => print ("dense: " ^ s ^ "\n")
  end

  structure A = Array
  structure AS = ArraySlice
  fun tabulate args = (AS.full o A.tabulate) args
  val length = AS.length
  val sub = AS.sub
  val foldr = AS.foldr
  val app = AS.app

  fun vvm_check (a, b) = 
    let 
      val l = length a
      fun loop (i, acc) = if i = l then acc
                          else
                            loop (i + 1, acc + (sub (a, i) * sub (b, i)))
    in
      loop (0, 0.0)
    end
  fun mvm_check (m, a) = tabulate (length m, fn i => vvm_check (sub (m, i), a))
  fun mmm_check (m, n) = tabulate (length m, fn i => mvm_check (n, sub (m, i)))

  fun printVector a = print ("[" ^ (foldr (fn (x, s) => Real.toString x
                                             ^ ", " ^ s) "] ,\n" a))
  fun printMatrix m = (print "["; app (fn a => printVector a) m; print "]\n")

  structure NonblockedSeq = Nonblocked (DenseArgSeq)
  structure NaivePar = Naive (DenseArgPar)
  structure NonblockedPar = Nonblocked (DenseArgPar)
  structure StridedSeq4 = Strided (structure A = DenseArgSeq
                                  structure S = struct val stride = 4 end)
  structure StridedSeq8 = Strided (structure A = DenseArgSeq
                                  structure S = struct val stride = 8 end)
  structure StridedSeq16 = Strided (structure A = DenseArgSeq
                                    structure S = struct val stride = 16 end)
  structure StridedSeq32 = Strided (structure A = DenseArgSeq
                                    structure S = struct val stride = 32 end)
  structure StridedSeq64 = Strided (structure A = DenseArgSeq
                                    structure S = struct val stride = 64 end)
  structure StridedSeq128 = Strided (structure A = DenseArgSeq
                                    structure S = struct val stride = 128 end)
  structure StridedSeq256 = Strided (structure A = DenseArgSeq
                                    structure S = struct val stride = 256 end)
  structure StridedSeq512 = Strided (structure A = DenseArgSeq
                                    structure S = struct val stride = 512 end)
  structure StridedSeq1024 = Strided (structure A = DenseArgSeq
                                    structure S = struct val stride = 1024 end)
  structure StridedPar4 = Strided (structure A = DenseArgPar
                                  structure S = struct val stride = 4 end)
  structure StridedPar8 = Strided (structure A = DenseArgPar
                                  structure S = struct val stride = 8 end)
  structure StridedPar16 = Strided (structure A = DenseArgPar
                                    structure S = struct val stride = 16 end)
  structure StridedPar32 = Strided (structure A = DenseArgPar
                                    structure S = struct val stride = 32 end)
  structure StridedPar64 = Strided (structure A = DenseArgPar
                                    structure S = struct val stride = 64 end)
  structure BlockedSeq = Blocked (structure A = DenseArgSeq
                                  structure D = NonblockedSeq)
  structure BlockedPar = Blocked (structure A = DenseArgPar
                               structure D = NonblockedSeq)

  fun run name size parCutoff =
    let 
      val mmm = if name = "mmm" then NonblockedPar.mmm parCutoff
                else if name = "mmm_naive" then NaivePar.mmm parCutoff
                else if name = "mmm_seq" then NonblockedSeq.mmm parCutoff
                else if name = "mmm_str4" then StridedPar4.mmm parCutoff
                else if name = "mmm_str8" then StridedPar8.mmm parCutoff
                else if name = "mmm_str16" then StridedPar16.mmm parCutoff
                else if name = "mmm_str32" then StridedPar32.mmm parCutoff
                else if name = "mmm_str64" then StridedPar64.mmm parCutoff
                else if name = "mmm_str4_seq" then StridedSeq4.mmm parCutoff
                else if name = "mmm_str8_seq" then StridedSeq8.mmm parCutoff
                else if name = "mmm_str16_seq" then StridedSeq16.mmm parCutoff
                else if name = "mmm_str32_seq" then StridedSeq32.mmm parCutoff
                else if name = "mmm_str64_seq" then StridedSeq64.mmm parCutoff
                else if name = "mmm_str128_seq" then StridedSeq128.mmm parCutoff
                else if name = "mmm_str256_seq" then StridedSeq256.mmm parCutoff
                else if name = "mmm_str512_seq" then StridedSeq512.mmm parCutoff
                else if name = "mmm_str1024_seq" then StridedSeq1024.mmm parCutoff
                else if name = "mmm_blk_seq" then BlockedSeq.mmm parCutoff 
                else if name = "mmm_blk" then BlockedPar.mmm parCutoff 
                else raise Argument ("unknown dense option: " ^ name)
      
      fun checkResult mn a =
          let val a' = mmm_check mn
              val epsilon = 1e~6
              fun loop (i, j) = 
                  if i = length a then true
                  else if j = length (sub (a, i)) 
                  then loop (i + 1, 0)
                  else ( (Real.abs (sub (sub (a, i), j) - sub (sub (a', i), j)) < epsilon) 
                         orelse
                         (print (Real.toString (sub (sub (a, i), j) - sub (sub (a', i), j)) ^ " ");
                          false) )
                       andalso loop (i, j + 1)
          in
            if loop (0, 0) then print "success\n"
            else print "FAILED!\n"
          end

      fun build () =
          let
            fun buildMatrix size : real AS.slice AS.slice =
                tabulate (size, fn _ => 
                   tabulate (size, fn _ => randomReal (Real.fromInt (!maxVal))))
            val m = buildMatrix size    
            val n = buildMatrix size

            val () = if !printResult then
                       let in
                         print "m = ";
                         printMatrix m;
                         print "n = ";
                         printMatrix n
                       end
                     else ()
          in
            (m, n)
          end

      val test = mmm

      fun post (m, n) a = 
          let in
            if !check then checkResult (m, n) a 
            else print "done\n";
            if !printResult then printMatrix a
            else ()
          end

      val (space, time) = repeatTest name build test post

    in
      print (String.concat [name, " ",
                            MLton.Parallel.Basic.policyName, " ",
                            Int.toString MLton.Parallel.Basic.numberOfProcessors, " ",
                            Int.toString size, " ", 
                            Int.toString parCutoff, " ", 
                            Word64.fmt StringCvt.DEC space, " ",
                            LargeInt.toString time, "\n"])
    end

  fun doit () = 
      let in 
        print "#format: name policy procs size parCutoff average-space(B) average-time(ms)\n";
        table3 run (!tests) (!sizes) (!parCutoffs)
      end

  val () = doit ()

end
