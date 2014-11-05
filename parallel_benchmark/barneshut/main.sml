structure Main =
struct

  open MainUtils
  open Random

  val iters = ref [10]
  val sims = ref ["exact"]
  val writePoints = ref false
  val del_t = ref 0.5

  local
    fun processBarnesHutArgs nil = ()
      | processBarnesHutArgs (args as (arg::rest)) =

      if arg = "--sims" then
        processStringListArg processBarnesHutArgs "sims" (setOption sims) rest

      else if arg = "--iters" then
        processIntListArg processBarnesHutArgs "iters" (setOption iters) rest

      else if arg = "--writepoints" then
        (writePoints := true; processBarnesHutArgs rest)

      else processArgs processBarnesHutArgs args
  in
    val () = processBarnesHutArgs (CommandLine.arguments ())
      handle Argument s => print ("barneshut: " ^ s ^ "\n")
  end

  structure Arg = SimArg (struct
                            val theta = 0.5
                            val epsilon = (0.0001,
                                           0.0001,
                                           0.0001)
                          end)

  structure Ex = Exact (Arg)
(*
  structure BHSeq = BarnesHut (ArgSeq)
*)
  structure BH = BarnesHut (Arg)

  fun write fold name t i =
    let
      open TextIO
      val btos = Arg.boxToString
      val ptos = Arg.particleToString
      val f = openOut ("viz/" ^ (StringCvt.padLeft #"0" 3 (Int.toString i))
                       ^ "_" ^ name ^ ".dat")
    in
      fold
          (fn (box, w) => (output (f, btos w box); Real.max (w - 0.5, 0.5)))
          (fn (p, w) => (output (f, ptos p); w))
          4.0
          t;
      flushOut f;
      closeOut f
    end

  fun generateParticle () =
    let
      val r = 10.0
      val hr = r / 2.0
      val v = 1.0
      val hv = v / 2.0
    in
      { position = ((randomReal r) - hr,
                    (randomReal r) - hr,
                    (randomReal r) - hr),
        velocity = ((randomReal v) - hv,
                    (randomReal v) - hv,
                    (randomReal v) - hv),
        mass = 1.0 }
    end

  structure V = Arg.Vector
  exception Sim = Arg.Sim

  fun runOne empty addParticle fold update name size iters parCutoff =
      let

        fun build () =
            let
              val () = if !writePoints then Random.setSeed 1367
                       else ()
              fun buildOne i t =
                  if i = 0 then
                    t
                  else
                    buildOne (i - 1) (addParticle (generateParticle (), t))
            in
              buildOne size (empty { position = V.cube 0.0,
                                     dimension = V.cube 1.0 })
            end

        fun loop i t =
            if i = iters then if !writePoints then write fold name t i else ()
            else
              let
                val () = if !writePoints then write fold name t i else ()

                val updateStart = Time.now ()
                val t = update parCutoff (!del_t) t
                val updateTime = Time.- (Time.now (), updateStart)
              (*
               val () = print (concat ["update time: ",
                                       (LargeInt.toString o Time.toMilliseconds)
                                           updateTime,
                                       "\n"])
               *)
              in
                loop (i + 1) t
              end

        fun test args = loop 0 args

        fun post _ _ = print "done\n"

        val {space = space, time = time, ...} = repeatTest name build test post
            handle Sim s => (print (concat ["Sim: ", s, "\n"]);
                             raise Sim s)
      in
        print (String.concat [(*"#",*)
                              name, " ",
                              MLton.Parallel.Basic.policyName, " ",
                              Int.toString MLton.Parallel.Basic.numberOfProcessors, " ",
                              Int.toString size, " ",
                              Int.toString iters, " ",
                              Int.toString parCutoff, " ",
                              Word64.fmt StringCvt.DEC space, " ",
                              LargeInt.toString time, "\n"])
      end

  fun run name =
      if name = "exact" then
        runOne Ex.empty Ex.addParticle Ex.fold Ex.update "exact"
   (* else if name = "barneshut_seq" then barneshut *)
      else if name = "barneshut" then
        runOne BH.empty BH.addParticle BH.fold BH.update "barneshut"
      else raise Argument ("unknown sim option: " ^ name)

  fun doit () =
      let in
        print "#format: policy procs size seqCutoff parCutoff average-space(B) average-time(ms)\n";
        table4 run (!sims) (!sizes) (!iters) (!parCutoffs)
      end

  val () = doit ()

end
