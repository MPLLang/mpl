structure AIPlayer =
struct

structure M = StatePlayerMap

open Action
structure V = Vector
structure C = Constants

val _ = MLton.Random.srand (case MLton.Random.seed () of
                                SOME w => w
                              | NONE => 0w42)

type t = { plays: int M.map ref,
           wins: int M.map ref,
           id: int }

val frame = GTime.fromMinutes (1.0)

fun print _ = ()

fun moves (sp: M.sp) =
    let val talk = State.talk (M.state sp)
        val numslides = Talk.numslides talk
        val currslide = Talk.currslide talk
        val slides = V.tabulate (numslides, Talk.slide talk)
        val questions = State.questions (M.state sp)
    in
        case M.player sp of
            M.Student =>
            List.map M.StuMove
                     ([ Pause, Resume ] @
                      (if currslide = numslides - 1 then [] else [NextSlide]) @
                      (List.foldl
                           (fn (({ typ, slide, expires }, _), r) =>
                               r @ (case slide of
                                        NONE => []
                                      | SOME i => (Jump i)::
                                                  (if Talk.covered talk i then
                                                       [Remind i]
                                                   else
                                                       [Delay i]))
                           )
                           []
                           questions) @
                      [ Answer, Drink ] @
                      (if List.length questions > 0 then
                           [Dodge]
                       else
                           []))
          | M.Committee i =>
            List.map (fn q => M.ComMove (q, i))
                     (let val cinfo = V.sub (State.committee (M.state sp), i)
                          val c = Real.round (#confusion cinfo)
                          val p = Real.round (#patience cinfo)
                          val b = #boldness cinfo
                          val f = #friendliness cinfo
                          fun q t u s =
                              { typ = t,
                                slide = s,
                                urgent = u}
                          fun qs t u =
                              [q t u NONE] (* @
                              (V.foldli
                                   (fn (i, {motivation, technical, related}, r) =>
                                       if (case t of
                                               Motivation => motivation > 0
                                             | Technical => technical > 0
                                             | Related => related > 0)
                                       then
                                           (q t u (SOME i))::r
                                       else
                                           r)
                                   []
                                   (Talk.slides (State.talk (M.state sp)))) *)
                      in
                          (if c - (Int.div (100 - f, 5)) >
                              C.q_cost_motivation then
                               qs Motivation false
                           else []) @
                          (if c - (Int.div (b, 10)) > C.q_cost_technical then
                               qs Technical false
                           else []) @
                          (if c - (Int.div (b, 5)) > C.q_cost_related then
                               qs Related false
                           else []) @
                          (if c - (Int.div (100 - f, 5)) >
                              C.q_cost_motivation + p then
                               qs Motivation true
                           else []) @
                          (if c - (Int.div (b, 10)) > C.q_cost_technical + p then
                               qs Technical true
                           else []) @
                          (if c - (Int.div (b, 5)) > C.q_cost_related + p then
                               qs Related true
                           else [])
                      end)
    end

fun move_to_string NONE = "no move"
  | move_to_string (SOME (M.ComMove (q, i))) =
    (Int.toString i) ^ " " ^ (eq_to_string q)
  | move_to_string (SOME (M.StuMove a)) =
    sa_to_string a

fun sim (sp: M.sp) (plays: int M.map) (wins: int M.map) (d: int)
        (expand: bool) =
    if d <= 0 then
        (plays, wins, State.InProgress)
    else
        let val moves = NONE::(List.map SOME (moves sp))
            val next_states = List.map (M.apply sp) moves
            val (next_move, next_sp) = (* Get the next move to explore *)
                if List.length moves = 1 then
                    (List.hd moves, List.hd next_states)
                else
                if List.all (fn sp => case M.find (plays, sp) of
                                          SOME _ => true
                                        | NONE => false)
                            next_states
                then
                    (* We have stats on all of the possible moves, so pick
                       the best one. *)
                    let (* val _ = print ("UCB at depth " ^ (Int.toString d) ^ "\n") *)
                        val tot_plays =
                            List.foldl (fn (sp, a) => case M.find (plays, sp) of
                                                          SOME p => p + a
                                                        | NONE => a (* raise Option*) )
                                       0
                                       next_states
                        val log_tot_plays = Math.ln (Real.fromInt tot_plays)
                        fun ucb sp =
                            let val w = case M.find (wins, sp) of
                                               SOME w => w
                                             | NONE => 0
                                val p = case M.find (plays, sp) of
                                                          SOME p => p
                                                        | NONE => 0 (* raise Option *)
                                val rw = Real.fromInt w
                                val rp = Real.fromInt p
                            in
                                rw / rp + Math.sqrt (2.0 * log_tot_plays / rp)
                            end
                        val max_ucb =
                            ListPair.foldl (fn (m, sp, (maxm, maxsp, max)) =>
                                               let val newucb = ucb sp
                                               in
                                                   if newucb > max then
                                                       (m, sp, newucb)
                                                   else (maxm, maxsp, max)
                                               end)
                                           (NONE, sp, Real.negInf)
                                           (moves, next_states)
                    in
                        (#1 max_ucb, #2 max_ucb)
                    end
                else
                    (* Pick a random move *)
                    let val rw = MLton.Random.rand ()
                        val r = Word.mod (rw, Word.fromInt (List.length moves))
                        val r = Word.toInt r
                                           (*
                        val _ =
                            print ("Exploring " ^ (Int.toString r) ^
                                   " of " ^ (Int.toString (List.length moves))
                                   ^ "\n")
                                           *)
                    in
                        (List.nth (moves, r), List.nth (next_states, r))
                    end
            val _ = case next_move of
                        SOME (M.ComMove ({typ, ...}, _)) =>
                        (case typ of
                             Action.Related => print "W"
                           | _ => ())
                        | _ => ()
            (* If this is the first unexplored state, add it to our list *)
            val (expand', plays', wins') =
                if expand andalso (case M.find (plays, next_sp) of
                                   SOME _ => false
                                 | NONE => true)
                then
                    (false, M.insert (plays, next_sp, 0),
                     M.insert (wins, next_sp, 0))
                else
                    (expand, plays, wins)
            (* For the recursive call to simulation, advance the time
               and/or current player *)
            val sp' = M.advance frame next_sp
            fun incplays plays sp =
                case M.find (plays, sp) of
                    SOME p => M.insert (plays, sp, p + 1)
                  | NONE => plays
            fun incwins wins sp =
                case M.find (wins, sp) of
                    SOME p => M.insert (wins, sp, p + 1)
                  | NONE => wins
            (* If the game isn't over, recursively perform simulation.
               Recursive call returns new play and win counts. *)
            val (rplays, rwins, winner) =
                case State.status (M.state sp') (M.time sp') of
                    State.InProgress =>
                    sim sp' plays' wins' (d - 1) expand'
                  | State.StudentWins => (plays', wins', State.StudentWins)
                  | State.CommitteeWins => (plays', wins', State.CommitteeWins)
            (* Update our play and win counts based on what happened in further
               simulation. *)
            val fplays = case winner of
                             State.InProgress => rplays
                            | _ => incplays rplays next_sp
            val fwins =
                case (winner, M.player sp) of
                    (State.StudentWins, M.Student) => incwins rwins next_sp
                  | (State.CommitteeWins, M.Committee _ ) =>
                    incwins rwins next_sp
                  | _ => rwins
        in
            (fplays, fwins, winner)
        end

fun move ((s, now): State.state * GTime.gtime)
         (player as {plays = rplays, wins = rwins, id = id} : t) =
    let val sp = M.mk s now (M.Committee id)
        val plays = !rplays
        val wins = !rwins
        (* val _ = print ((Int.toString (M.numItems plays)) ^ " in plays\n") *)
        fun playawhile (n: int) (plays, wins) =
            if n <= 0 then
                (plays, wins)
            else
                let val (plays, wins, _) = sim sp plays wins 500 true
                in
                    playawhile (n - 1) (plays, wins)
                end
        val (plays, wins) = playawhile 1000 (plays, wins)
        val moves = NONE::(List.map SOME (moves sp))
        (* val _ = print ((Int.toString (List.length moves)) ^ " moves\n") *)
        (* Approximate Gaussian noise *)
        fun noise x =
            let fun rand01 _ =
                    let val rw = MLton.Random.rand ()
                        val N = 2000000000
                        val r = Word.toInt (Word.mod (rw, Word.fromInt N))
                    in
                        (Real.fromInt r) / (Real.fromInt N)
                    end
                val rands = List.tabulate (12, rand01)
                val sum = List.foldl op+ 0.0 rands
                val noise = (sum - 6.0) / 6.0
            in
                x + x * noise
            end
        fun getpct sp =
            case (M.find (plays, sp), M.find (wins, sp)) of
                (SOME p, SOME w) =>
                (print ("plays: " ^ (Int.toString p) ^ ", wins: "
                        ^ (Int.toString w) ^ "\n");
                  if p > 5 then
                      noise ((Real.fromInt w) / (Real.fromInt p))
                  else 0.0)
              | _ => 0.0 (* don't have data, be pessimistic *)
        val (_, best_move) =
            List.foldl (fn (m, (bestpct, bestm)) =>
                           let val newsp = M.apply sp m
                               val newsp' = M.apply sp m
                               val _ = case M.StatePlayerKey.compare (newsp, newsp') of
                                           LESS => print "LESS"
                                         | EQUAL => ()
                                         | GREATER => print "GREATER"
                               val _ = print (move_to_string m)
                               val newpct = getpct newsp
                               (* val _ = print ((Real.toString newpct) ^ "\n") *)
                           in
                                   if newpct > bestpct then
                                       (newpct, m)
                                   else
                                       (bestpct, bestm)
                           end)
                       (0.8, NONE) (* If no move is that good, wait *)
                       moves
    in
        rplays := plays;
        rwins := wins;
        (player,
         case best_move of
             NONE => ((* print "no move\n"; *) NONE)
          | SOME (M.ComMove (q, _)) => SOME q
          | SOME _ => raise (Fail "invalid move in move"))
    end

fun endgame _ _ = ()

fun init id plays wins =
    { plays = plays,
      wins = wins,
      id = id }
end
