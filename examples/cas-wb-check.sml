val par = ForkJoin.fork
fun for (i, j) f = if i = j then () else (f i; for (i+1, j) f)
fun parfor grain (i, j) f =
  let val n = j - i
  in if n <= grain
     then for (i, j) f
     else ( par ( fn _ => parfor grain (i, i + n div 2) f
                , fn _ => parfor grain (i + n div 2, j) f
                )
          ; ()
          )
  end

val n = valOf (Int.fromString (List.nth (CommandLine.arguments (), 0)))
val refs = Vector.tabulate (n, fn i => (i, ref []))

val arr : int list array = ForkJoin.alloc n
val _ = parfor 100 (0, n) (fn i => Array.update (arr, i, []))

fun putRef i =
  let
    val r = #2 (Vector.sub (refs, i))
    val x = !r
  in
    MLton.Parallel.compareAndSwap r (x, i :: x);
    ()
  end

fun putArr i =
  let
    val x = Array.sub (arr, i)
  in
    MLton.Parallel.arrayCompareAndSwap (arr, i) (x, i :: x);
    ()
  end

val _ = parfor 100 (0, n) (fn i => (putRef i; putArr i))

fun goodAt i =
  (case Vector.sub (refs, i) of
    (x, ref [a]) => x = i andalso a = i
  | _ => false)
  andalso
  (case Array.sub (arr, i) of
    [a] => a = i
  | _ => false)

fun goodThrough i j =
  if j - i = 0 then
    true
  else if j - i <= 1000 then
    goodAt i andalso goodThrough (i+1) j
  else
    let
      val mid = i + (j - i) div 2
      val (lgood, rgood) =
        par (fn _ => goodThrough i mid, fn _ => goodThrough mid j)
    in
      lgood andalso rgood
    end

val _ = print ("good? " ^ (if goodThrough 0 n then "yes" else "no") ^ "\n")


