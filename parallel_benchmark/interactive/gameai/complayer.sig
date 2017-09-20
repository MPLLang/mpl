signature COMMITTEE_PLAYER =
sig

    type com_player

    val move: State.state * GTime.gtime -> com_player ->
              com_player * (Action.ext_question option)

    val endgame : bool -> com_player -> unit
end
