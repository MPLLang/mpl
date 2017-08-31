signature STUDENT_PLAYER =
sig

    type stu_player

    val talk: CInfo.cinfo Vector.vector -> Talk.talk

    val move: State.state * GTime.gtime -> stu_player ->
              stu_player * (Action.stu_act option)

    val endgame: bool -> stu_player -> unit

end
