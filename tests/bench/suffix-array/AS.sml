structure AS =
struct
  open ArraySlice
  open Seq

  val GRAIN = 4096
  val ASupdate = ArraySlice.update
end
