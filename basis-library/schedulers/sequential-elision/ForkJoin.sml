structure ForkJoin =
struct
  fun fork (f, g) = (f (), g ())
end
