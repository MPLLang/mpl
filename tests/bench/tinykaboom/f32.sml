structure f32 =
struct
  open Real32
  open Real32.Math
  fun max a b = Real32.max (a, b)
  fun min a b = Real32.min (a, b)
end
