structure Main =
struct

  open MainUtils
  open Random

  val tests = ref [] : string list ref
  val writePoints = ref false
  val circular = ref 0

  local
    fun processGeometryArgs nil = ()
      | processGeometryArgs (args as (arg::rest)) =

      if arg = "--tests" then
        processStringListArg processGeometryArgs "tests" (setOption tests) rest

      else if arg = "--writepoints" then
        (writePoints := true; processGeometryArgs rest)

      else if arg = "--circular" then
        (processIntArg processGeometryArgs "circular" (setOption circular) rest)

      else processArgs processGeometryArgs args

  in
    val () = processGeometryArgs (CommandLine.arguments ())
      handle Argument s => print ("dense: " ^ s ^ "\n")
  end

  structure QHullSeq = QuickHull (GeometryArgSeq)
  structure QHullPar = QuickHull (GeometryArgPar)

  val append = GeometryArgSeq.append
  val singleton = GeometryArgSeq.singleton
  val empty = GeometryArgSeq.empty
  val fromList = GeometryArgSeq.fromList
  val ptos = GeometryArgSeq.ptos

  fun run name size parCutoff =
    let
      val test = if name = "qhull_seq" then QHullSeq.hull parCutoff
                 else if name = "qhull" then QHullPar.hull parCutoff
                 else raise Argument ("unknown geometry option: " ^ name)

      fun checkResult _ _ = () (* XXX *)

      fun build () =
          let
            val r = 10.0
            val hr = r / 2.0
            val generatePoint = fn i =>
                let
                  val p =
                      if !circular > 0 andalso randomInt 100 < !circular then
                        (hr * Math.sin (2.0 * Math.pi * Real.fromInt i / Real.fromInt size),
                         hr * Math.cos (2.0 * Math.pi * Real.fromInt i / Real.fromInt size))
                      else
                        ((randomReal r) - hr,
                         (randomReal r) - hr)
                  val () = if !writePoints then
                             print (concat [ptos p, "\n"])
                           else ()
                in
                  p
                end
          in
            fromList parCutoff (List.tabulate (size, generatePoint))
          end

      fun post _ h =
          let
            val () = print "done\n"

            val fold = GeometryArgSeq.fold
            val sub = GeometryArgSeq.sub
            val () = print (concat ["# size of hull: ", Int.toString (GeometryArgSeq.size h), "\n"])
            val () = if !writePoints then
                       let
                         fun ltos (p1, p2) =
                             concat ["set arrow from ", ptos p1, " to ", ptos p2, "\n"]
                       in
                         (* XXX not correct *)
                         ignore (fold parCutoff
                                      (fn (p1, p2) => (print (ltos (p1, p2)); p2))
                                      (fn p => p)
                                      (fn (p1, p2) => (print (ltos (p1, p2)); p2))
                                      (sub (h, 0))
                                      h)
                       end
                     else ()
          in
            ()
          end

      val {space, time, ...} = repeatTest name build test post

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
