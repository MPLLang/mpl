structure vec3 =
struct
  type f32 = f32.real

  type vector = {x: f32, y: f32, z: f32}
  fun add (a: vector, b: vector) =
    {x = #x a + #x b, y = #y a + #y b, z = #z a + #z b}
  fun sub (a: vector, b: vector) =
    {x = #x a - #x b, y = #y a - #y b, z = #z a - #z b}
  fun dot (a: vector, b: vector) =
    (#x a * #x b) + (#y a * #y b) + (#z a * #z b)
  fun scale s ({x,y,z}: vector) =
    {x = s*x, y = s*y, z = s*z}
  fun map2 f (a: vector) (b: vector) =
    {x = f (#x a) (#x b), y = f (#y a) (#y b), z = f (#z a) (#z b)}
  fun norm a =
    f32.sqrt (dot (a, a))
  fun normalise (v: vector): vector =
    scale (1.0 / norm v) v
end
