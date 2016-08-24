structure Primitives :> PRIMITIVES =
struct
    val fork = MLton.Parallel.ForkJoin.fork

    val par = fork

    fun par3 (f, g, h) =
        case fork (f, fn () => fork (g, h))
         of (rf, (rg, rh)) => (rf, rg, rh)

    fun par4 (f, g, h, i) =
        case fork (fn () => fork (f, g), fn () => fork (h, i))
          of ((rf, rg), (rh, ri)) => (rf, rg, rh, ri)


    (* RAM_NOTE: Still sequential! *)
    fun parTab (n, f) = let val v = Vector.tabulate (n, f)
                        in fn i => Vector.sub (v, i) end
end
