infix 6 andthen
fun a andthen b =
    case a of
        LESS => a
      | EQUAL => b
      | GREATER => a

fun cwithin dist (r1, r2) =
    if Real.abs (r1 - r2) < dist then
        EQUAL
    else if r1 < r2 then LESS
    else GREATER
