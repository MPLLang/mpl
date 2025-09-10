(* ocaml source:
 *
##   module T = Domainslib.Task
##   let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
##   let mat_size = try int_of_string Sys.argv.(2) with _ -> 1200
##   let chunk_size = try int_of_string Sys.argv.(3) with _ -> 16
 *)

structure CLA = CommandLineArgs
val mat_size = CLA.parseInt "mat_size" 1200
val chunk_size = CLA.parseInt "chunk_size" 16
val _ = print ("mat_size " ^ Int.toString mat_size ^ "\n")
val _ = print ("chunk_size " ^ Int.toString chunk_size ^ "\n")

(* ocaml source:
 *
##
##   module SquareMatrix = struct
##
##     let create f : float array =
##       let fa = Array.create_float (mat_size * mat_size) in
##       for i = 0 to mat_size * mat_size - 1 do
##         fa.(i) <- f (i / mat_size) (i mod mat_size)
##       done;
##       fa
##     let parallel_create pool f : float array =
##       let fa = Array.create_float (mat_size * mat_size) in
##       T.parallel_for pool ~chunk_size:(mat_size * mat_size / num_domains) ~start:0
##       ~finish:( mat_size * mat_size - 1) ~body:(fun i ->
##         fa.(i) <- f (i / mat_size) (i mod mat_size));
##       fa
##
##     let get (m : float array) r c = m.(r * mat_size + c)
##     let set (m : float array) r c v = m.(r * mat_size + c) <- v
##     let parallel_copy pool a =
##       let n = Array.length a in
##       let copy_part a b i =
##         let s = (i * n / num_domains) in
##         let e = (i+1) * n / num_domains - 1 in
##         Array.blit a s b s (e - s + 1) in
##       let b = Array.create_float n in
##       let rec aux acc num_domains i =
##         if (i = num_domains) then
##           (List.iter (fun e -> T.await pool e) acc)
##         else begin
##           aux ((T.async pool (fun _ -> copy_part a b i))::acc) num_domains (i+1)
##         end
##       in
##       aux [] num_domains 0;
##       b
##   end
*)

structure SquareMatrix =
struct
  fun create f: real array =
    let
      val fa = ForkJoin.alloc (mat_size * mat_size)
    in
      Util.for (0, mat_size * mat_size) (fn i =>
        Array.update (fa, i, f (i div mat_size, i mod mat_size)));
      fa
    end

  fun parallel_create f: real array =
    let
      val fa = ForkJoin.alloc (mat_size * mat_size)
    in
      ForkJoin.parfor 10000 (0, mat_size * mat_size) (fn i =>
        Array.update (fa, i, f (i div mat_size, i mod mat_size)));
      fa
    end

  fun get m r c = Array.sub (m, r * mat_size + c)
  fun set m r c v = Array.update (m, r * mat_size + c, v)

  (* SAM_NOTE: This function is a bit overengineered in the ocaml source in
   * a way that is probably negatively impacting performance. The easiest way
   * to do it would just be this:
   *
   * fun parallel_copy a =
   *   let
   *     val n = Array.length a
   *     val b = allocate n
   *   in
   *     parfor GRAIN (0, n) (fn i => Array.update (b, i, Array.sub (a, i)));
   *     b
   *   end
   *
   * But, rather than implement it like this, I'll try to be faithful to the
   * original ocaml code here.
   *
   * The ocaml code for this function uses futures (async/await), which MPL
   * doesn't support. But using futures is unnecessary, because the ocaml code
   * just does uses them in fork-join style anyway! Also, the ocaml code
   * relies upon knowing the number of processors to do granularity control,
   * but this is unnecessary. Instead we can choose a static GRAIN and then
   * divide the array into ceil(n/GRAIN) parts.
   *
   * The ocaml source seems like it has a bug, if n is not divisible
   * by num_domains. Easy fix is below, when calculating the end of the
   * part (variable e below).
   *)
  fun parallel_copy a =
    let
      val n = Array.length a
      val GRAIN = 10000 (* same role as n/num_domains in ocaml source *)
      fun copy_part a b i =
        let
          val s = i * GRAIN
          val e = Int.min (n, (i+1) * GRAIN) (* fixed bug! take min. *)
        in
          Util.for (s, e) (fn j => Array.update (b, j, Array.sub (a, j)))
        end
      val num_parts = Util.ceilDiv n GRAIN
      val b = ForkJoin.alloc n
    in
      ForkJoin.parfor 1 (0, num_parts) (copy_part a b);
      b
    end
end

(* ocaml source:
 *
##
##   open SquareMatrix
##
##   let lup pool (a0 : float array) =
##     let a = parallel_copy pool a0 in
##     for k = 0 to (mat_size - 2) do
##     T.parallel_for pool ~chunk_size:chunk_size ~start:(k + 1) ~finish:(mat_size  -1)
##     ~body:(fun row ->
##       let factor = get a row k /. get a k k in
##       for col = k + 1 to mat_size-1 do
##         set a row col (get a row col -. factor *. (get a k col))
##         done;
##       set a row k factor )
##     done ;
##     a
 *)

open SquareMatrix

fun lup a0 =
  let
    val a = parallel_copy a0
  in
    Util.for (0, mat_size-1) (fn k =>
      ForkJoin.parfor chunk_size (k+1, mat_size) (fn row =>
        let
          val factor = get a row k / get a k k
        in
          Util.for (k+1, mat_size) (fn col =>
            set a row col (get a row col - factor * (get a k col)));
          set a row k factor
        end));
    a
  end

(* ocaml source:
 *
##   let () =
##     let pool = T.setup_pool ~num_domains:(num_domains - 1) in
##     let a = create (fun _ _ -> (Random.float 100.0) +. 1.0 ) in
##     let lu = lup pool a in
##     let _l = parallel_create pool (fun i j -> if i > j then get lu i j else if i = j then 1.0 else 0.0) in
##     let _u = parallel_create pool (fun i j -> if i <= j then get lu i j else 0.0) in
##     T.teardown_pool pool
 *)

(* SAM_NOTE: It seems like the ocaml source chose to initialize sequentially
 * because of the stateful RNG. We'll do a PRNG based on a hash function, to
 * be safe for parallelism, and initialize in parallel.
 *)
(*
val rand = Random.rand (15, 210) (* seed the generator *)
fun randReal bound =
  bound * Random.randReal rand
*)

fun randReal bound seed =
  bound * (Real.fromInt (Util.hash seed mod 1000001) / 1000000.0)

val (a, tm) = Util.getTime (fn _ =>
  create (fn (i, j) => 1.0 + randReal 100.0 (i * mat_size + j)))
val _ = print ("generated input in " ^ Time.fmt 4 tm ^ "s\n")

fun ludEx () =
  let
    val (lu, tm) = Util.getTime (fn _ => lup a)
    val _ = print ("main decomposition in " ^ Time.fmt 4 tm ^ "s\n")

    val ((l, u), tm) = Util.getTime (fn _ =>
      let
        val l =
          parallel_create (fn (i, j) =>
            if i > j then get lu i j
            else if i = j then 1.0
            else 0.0)
        val u =
          parallel_create (fn (i, j) =>
            if i <= j then get lu i j else 0.0)
      in
        (l, u)
      end)
    val _ = print ("extracted L and U in " ^ Time.fmt 4 tm ^ "s\n")
  in
    (l, u)
  end

val _ = Benchmark.run "running LU decomposition" ludEx

