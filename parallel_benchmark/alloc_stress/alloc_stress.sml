fun die message =
    (TextIO.output (TextIO.stdErr, message ^ "\n");
     OS.Process.exit OS.Process.failure)

fun getStringOption option args =
    (case args
      of arg1 :: arg2 :: args =>
	 if String.compare (arg1, option) = EQUAL
         then SOME arg2
	 else getStringOption option (arg2 :: args)
       | _ => NONE
    (* end case *))

fun getIntOption (option) (args) : int option =
    (case getStringOption option args
      of SOME arg => Int.fromString arg
       | NONE => NONE)

fun stringToBytes (arg : string) =
    let
        fun isSuffix (c::cs) s = String.isSuffix c s orelse isSuffix cs s
          | isSuffix [] s = false

        val factor = if isSuffix ["g", "G"] arg
                     then 1024 * 1024 * 1024
                     else if isSuffix ["m", "M"] arg
                     then 1024 * 1024
                     else if isSuffix ["k", "K"] arg
                     then 1024
                     else 1
    in
        Option.map (fn v => v * factor) (Int.fromString arg)
    end

fun getByteSizeOption (option : string) (args : string list) : int option =
    case getStringOption option args
     of SOME arg => stringToBytes arg
      | NONE => NONE

fun doit (allocSize : int) (numAllocs : int) (numTasks : int) =
    let
        fun allocArray 0 = ()
          | allocArray n =
              let
                  val _ = Int8Array.array (allocSize, 0)
              in
                  allocArray (n - 1)
              end

        fun makeTaskClosures taskNumber =
            if taskNumber = 1
            then [fn () => allocArray ((numAllocs div numTasks)
                                       + (numAllocs mod numTasks))]
            else (fn () => allocArray (numAllocs div numTasks))::
                 (makeTaskClosures (taskNumber - 1))

        fun forker (task::tasks) =
            let
                val _ = MLton.Parallel.ForkJoin.fork (fn () => forker tasks,
                                                      task)
            in
                ()
            end
          | forker [] = ()
    in
        forker (makeTaskClosures numTasks)
    end

fun main args =
    let
        val allocSize = case getByteSizeOption "-alloc-size" args
                         of SOME v => v
                          | NONE => 16 * 1024

        val numAllocs = case getIntOption "-num-allocs" args
                         of SOME v => v
                          | NONE => 1000

        val numTasks = case getStringOption "-num-tasks" args
                        of SOME "p" => MLton.Parallel.Basic.numberOfProcessors
                         | _ => case getIntOption "-num-tasks" args
                                of SOME v => v
                                 | NONE => 1

        val () = print (String.concat ["allocSize: ",
                                       Int.toString allocSize,
                                       " numAllocs: ",
                                       Int.toString numAllocs,
                                       " numTasks: ",
                                       Int.toString numTasks,
                                       "\n"]);
        val ts = Time.now ()
        val () = doit allocSize numAllocs numTasks
        val te = Time.now ()
        val elapsed = Time.- (te, ts)
    in
        print (String.concat ["# ",
                              (LargeInt.toString o Time.toMilliseconds) elapsed,
                              " ms (0 ms in GC) END\n"])
    end

val _ = main (CommandLine.arguments ())
