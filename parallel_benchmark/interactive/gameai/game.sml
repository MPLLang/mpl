structure V = Vector
structure CP = ComPlayer
structure SP = StuPlayer
structure T = GTime
structure S = State

type committee = CP.com_player V.vector

val _ = Basic.finalizePriorities ()
val _ = Basic.init ()

val frame = GTime.fromSeconds 5.0 (* GTime.fromMinutes (1.0 / (60.0 * 24.0)) *)

fun loop (state: S.state)
         (student: SP.stu_player)
         (committee: committee)
    =
    let val now = T.now ()
        val cpms = Vector.map (CP.move (state, now)) committee
        val committee' = Vector.map #1 cpms
        val cmoves = Vector.map #2 cpms
        val (student', smove) = SP.move (state, now) student
        fun addq (i, SOME q, s) = S.addq s q i
          | addq (i, NONE, s) = s
        val state' = S.stu_act state smove now
        val state' = Vector.foldli addq state' cmoves
    in
        case S.status state' now of
            S.InProgress =>
            (GTime.sleep frame;
             loop state' student' committee')
          | S.StudentWins =>
            (SP.endgame true student;
             Vector.app (CP.endgame false) committee)
          | S.CommitteeWins =>
            (SP.endgame false student;
             Vector.app (CP.endgame true) committee)
    end

fun init () =
    let val committee = V.tabulate (5, fn _ => CP.init ())
        val student = SP.init ()
        val cinfos = V.tabulate (5, fn _ => CInfo.init 50 50 50)
        val talk = SP.talk cinfos
        val state = State.init (50.0, 50.0) talk cinfos 1.0
    in
        GTime.init ();
        GTime.setspeed 10.0;
        loop state student committee
    end

val _ = init ()
