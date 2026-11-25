structure ParFuncArray:
sig
  type 'a t
  type 'a farray = 'a t

  val alloc: int -> 'a farray
  val length: 'a farray -> int
  val tabulate: int * (int -> 'a) -> 'a farray
  val sub: 'a farray * int -> 'a
  val update: 'a farray * int * 'a -> 'a farray
end =
struct

  (** ========================================================================
    * Log data structure
    *)

  type version = int

  structure Log:
  sig
    type 'a t
    type 'a log = 'a t
    type capacity = int

    val new: capacity -> 'a log

    val getVersion: 'a log -> version -> 'a option

    (** Pushing onto a log might make it grow, in which case the old version
      * should no longer be used. This is a performance optimization: we can
      * store logs in an array and only update a log when it grows. (The
      * alternative would be to wrap a log in ref, but this is an unnecessary
      * indirection.
      *)
    val push: 'a log -> version * 'a -> 'a log
  end =
  struct
    datatype 'a t = L of {data: (version * 'a) array, size: int}
    type 'a log = 'a t
    type capacity = int


    fun new cap =
      L {data = ForkJoin.alloc cap, size = 0}
      (* L {data = SeqBasis.tabulate 10000 (0, cap) (fn _ => NONE), size = 0} *)


    fun push (L {data, size}) (v, x) =
      let
        val i = size
      in
        if i < Array.length data then
          (* ( Array.update (data, i, SOME (v,x)) *)
          ( Array.update (data, i, (v,x))
          ; L {data = data, size = size+1}
          )
        else
          let
            val data' = ForkJoin.alloc (2 * Array.length data)
          in
            ForkJoin.parfor 10000 (0, Array.length data) (fn j =>
              Array.update (data', j, Array.sub (data, j))
            );
(*
            ForkJoin.parfor 10000 (Array.length data, Array.length data') (fn j =>
              Array.update (data', j, NONE)
            );
*)
            (* Array.update (data', i, SOME(v,x)); *)
            Array.update (data', i, (v,x));
            (* print ("grown! " ^ Util.intToString (Array.length data') ^ "\n"); *)
            L {data = data', size = i+1}
          end
      end


    (* fun push (logs, logIdx) (v, x) =
      case push' (Array.sub (logs, logIdx)) of
        NONE => ()
      | SOME newlog => Array.update (logs, logIdx, newlog) *)


    fun getVersion (L {data, size}) v =
      if size = 0 then
        NONE
      else
      let
        val n = size
        val _ =
          if n <= Array.length data then ()
          else print ("getVersion: data length: " ^ Int.toString (Array.length data) ^ ", current size: " ^ Int.toString n ^ "\n")

(*
        fun loop i =
          if i >= n then n
          else if #1 (valOf (Array.sub (data, i))) <= v then
            loop (i+1)
          else
            i
            *)
      in
        if #1 ((*valOf*) (Array.sub (data, n-1))) < v then NONE else
        let
          val slice = ArraySlice.slice (data, 0, SOME n)
          val idx =
            BinarySearch.searchPosition slice
              (fn (*SOME*) (v', _) => Int.compare (v, v')
                (* | NONE => raise Fail "ParFuncArray.getVersion found empty slot" *)
              )
        in
          SOME (#2 ((*valOf*) (Array.sub (data, idx))))
        end
      end
      handle e => (print ("error during getVersion: " ^ exnMessage e ^ "\n"); raise e)

  end

  (** ========================================================================
    * Main functions
    *)

  datatype 'a array_data =
    AD of {vr: version ref, data: 'a array, logs: 'a Log.t array}

  type 'a t = version * 'a array_data
  type 'a farray = 'a t


  fun alloc n =
    let
      val version = 0
      val data = ForkJoin.alloc n
      val logs = SeqBasis.tabulate 5000 (0, n) (fn _ => Log.new 1)
    in
      (version, AD {vr = ref version, data = data, logs = logs})
    end


  fun length (_, AD {data, ...}) = Array.length data


  fun sub (farr, i) =
    if i < 0 orelse i >= length farr then
      raise Subscript
    else
    let
      val (v, AD {vr, data, logs}) = farr
      val guess = Array.sub (data, i)
    in
      if v = !vr then
        guess
      else
        case Log.getVersion (Array.sub (logs, i)) v of
          NONE => guess
        | SOME x => x
    end


  fun bcas r (old, new) =
    old = Concurrency.cas r (old, new)


  fun updateLog (logs, i) (v, x) =
    let
      val oldLog = Array.sub (logs, i)
      val newLog = Log.push oldLog (v, x)
      val oldLog' = Concurrency.casArray (logs, i) (oldLog, newLog)
    in
      if MLton.eq (oldLog, oldLog') then ()
      else raise Fail "updateLog failed somehow!"
    end


  fun update (farr, i, x) =
    if i < 0 orelse i > length farr then
      raise Subscript
    else
    let
      val (v, ad as AD {vr, data, logs}) = farr
      val currv = !vr
    in
      if
        currv = v andalso
        currv < Array.length data andalso
        bcas vr (v, v+1)
      then
        (* We successfully claimed access for updating the data *)
        ( updateLog (logs, i) (v, Array.sub (data, i))
        ; Array.update (data, i, x)
        ; (v+1, ad)
        )
      else (* We have to rebuid *)
      let
        val n = Array.length data
        (* val _ = print ("rebuilding " ^ Util.intToString n ^ "\n") *)
        val data' = SeqBasis.tabulate 1000 (0, n) (fn i => sub (farr, i))
        val logs' = SeqBasis.tabulate 5000 (0, n) (fn _ => Log.new 1)
      in
        Array.update (data', i, x);
        (0, AD {vr = ref 0, data = data', logs = logs'})
      end
    end


  fun tabulate (n, f) =
    let
      val version = 0
      val data = ForkJoin.alloc n
      val logs = SeqBasis.tabulate 5000 (0, n) (fn _ => Log.new 1)
    in
      ForkJoin.parfor 1000 (0, n) (fn i =>
        let
          val x = f i
        in
          Array.update (data, i, f i)
          (* updateLog (logs, i) (version, x) *)
        end);

      (version, AD {vr = ref version, data = data, logs = logs})
    end

end
