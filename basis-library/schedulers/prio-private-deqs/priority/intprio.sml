
structure Priority :> PRIORITY =
struct

    exception InvariantViolated of string
    exception Uninitialized
    exception AlreadyInitialized
    exception InvalidArgument

    structure A = Array
    structure A2 = Array2
    structure V = Vector


    type t = int

    val top = 2
    val bot = 1

    val numberOfPrios = ref 2

    val initialized = ref false
    val checked = ref false

    val distFunc = ref (fn _ => top)

    fun count () = !numberOfPrios

    fun print _ = ()

    (* Functions must be called in this order:
     * new (any number of times)
     * init
     * new_lessthan (any number of times)
     * check
     *
     * After calling check, functions may be called in this order:
     * ( (new*, init)* + (new_lessthan*, check)* )*
     *)

    fun toInt i = i

    val plt = Int.<
    val ple = Int.<=
    fun pe (a, b) = a = b

    fun init () : unit =
        initialized := true;

    fun check () : unit =
        if not (!initialized) then raise Uninitialized
        else
            checked := true

    fun new () : t =
        let val n = !numberOfPrios
            val po = ref ~1
        in
            numberOfPrios := n + 1;
            initialized := false;
            n
        end
        handle Subscript => (print "new"; raise Subscript)

    fun new_lessthan p1 p2 : unit =
        ()

    fun fromInt (n: int) : t =
        n

    val toString =
        Int.toString

    fun installDist (f: t -> int) =
        let val raw = V.tabulate (count (),
                                  fn i => let val r = fromInt i
                                          in
                                              (r, f r)
                                          end)
            val sum = Real.fromInt (Vector.foldl (fn ((_, n), a) => a + n)
                                                 0 raw)
            val cumulative =
                Vector.foldl (fn ((r, n), (a, l)) =>
                                 let val prob = Real./ (Real.fromInt n, sum)
                                 in
                                     (a + prob, (r, a + prob)::l)
                                 end)
                             (0.0, [])
                             raw
            fun find l x =
                case l of
                    [] => raise InvalidArgument
                  | (r, p)::t => if x < p then r
                                 else find t x
        in
            distFunc := (find (#2 cumulative))
        end

    fun chooseFromDist x =
        let (* val x = UsefulRandom.rand01 () *)
        in
            (!distFunc) x
        end

    fun next (r: t) =
        if r = 1 then top
        else r - 1

end
