structure Word32 =
struct
  open Word32
  val layout = Layout.str o toString
end
