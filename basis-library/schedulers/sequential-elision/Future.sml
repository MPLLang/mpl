structure Future =
struct
  type 'a t = 'a
  fun future f = f ()
  fun force x = x
  fun poll x = SOME x
end
