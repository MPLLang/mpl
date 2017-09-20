structure ComPlayer =
struct

datatype com_player = AIPlayer of AIPlayer.t
                    | NullPlayer

datatype initas = InitAI of (int StatePlayerMap.map ref *
                             int StatePlayerMap.map ref)
                | InitNull

fun move st (AIPlayer p) =
    let val (p, m) = AIPlayer.move st p
    in
        (AIPlayer p, m)
    end
  | move _ NullPlayer =
    (NullPlayer, NONE)

fun init (ai: initas) (id: int) =
    case ai of
        InitAI (plays, wins) => AIPlayer (AIPlayer.init id plays wins)
      | InitNull => NullPlayer

fun endgame win (AIPlayer p) = AIPlayer.endgame win p
  | endgame _ NullPlayer = ()

end
