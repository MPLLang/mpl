structure State =
struct

structure V = Vector

type asker = int

type state =
     { knowledge: real,
       stamina: real,
       talk: Talk.talk,
       committee: CInfo.cinfo Vector.vector,
       time: GTime.gtime,
       technical_content: real,
       speed: real,
       questions: (Action.question * asker) list }

fun init ((knowledge, stamina): real * real)
         (talk: Talk.talk)
         (committee: CInfo.cinfo Vector.vector) (speed: real) =
    { knowledge = knowledge,
      stamina = stamina,
      talk = talk,
      committee = committee,
      time = GTime.fromMinutes 0.0,
      technical_content = 0.0,
      speed = speed,
      questions = [] }

fun rp r =
    Int.toString (Real.round r)

fun p i = Int.toString i

fun toString {knowledge, stamina, talk, committee, time, technical_content,
              speed, questions} =
    "K\t" ^ (rp knowledge) ^ "\t\tS\t" ^ (rp stamina) ^ "\n" ^
    "slide " ^ (p (Talk.currslide talk)) ^ "\n" ^
    (V.foldli (fn (i, ({ patience, confusion, ... }: CInfo.cinfo), s) =>
                  (p i) ^ "\tP\t" ^ (rp patience) ^ "\tC\t" ^
                  (rp confusion) ^ "\n" ^ s)
              ""
              committee) ^
    "time left: " ^ (GTime.toString (GTime.minus (GTime.fromMinutes 45.0,
                                                  time))) ^ "\n" ^
    "covered: " ^ (rp technical_content) ^ "\n"

datatype update =
         Knowledge of real -> real
         | Stamina of real -> real
         | Talk of Talk.talk -> Talk.talk
         | Committee of CInfo.cinfo Vector.vector -> CInfo.cinfo Vector.vector
         | Time of GTime.gtime -> GTime.gtime
         | TechnicalContent of real -> real
         | Speed of real -> real
         | Questions of (Action.question * asker) list ->
                        (Action.question * asker) list

datatype status =
         InProgress
         | StudentWins
         | CommitteeWins

fun update s u =
    { knowledge = (case u of
                       Knowledge f => f (#knowledge s)
                     | _ => #knowledge s),
      stamina = (case u of
                     Stamina f => f (#stamina s)
                   | _ => #stamina s),
      talk = (case u of
                  Talk f => f (#talk s)
                | _ => #talk s),
      committee = (case u of
                       Committee f => f (#committee s)
                     | _ => #committee s),
      time = (case u of
                  Time f => f (#time s)
                | _ => #time s),
      technical_content = (case u of
                               TechnicalContent f => f (#technical_content s)
                             | _ => #technical_content s),
      speed = (case u of
                   Speed f => f (#speed s)
                 | _ => #speed s),
      questions = (case u of
                      Questions f => f (#questions s)
                    | _ => #questions s)
    }

fun status (s: state) (now: GTime.gtime) : status =
    if #technical_content s >= 150.0 then
        StudentWins
    else if Vector.exists (fn ci => #patience ci <= 0.0) (#committee s)
    then
        CommitteeWins
    else
        InProgress

open Action
fun stu_act (s: state) (a: Action.stu_act option) (now: GTime.gtime) : state =
    let val dtm = GTime.minus (now, #time s)
        (* val _ = print ("now: " ^ (GTime.toString now) ^ "\n")
        val _ = print ("dtm: " ^ (GTime.toString dtm) ^ "\n") *)
        val dtm = GTime.toMinutes dtm
        val s = update s (Time (fn _ => now))
        (* Apply action to the talk and get talk progress *)
        val (talk', dm, dt, dr) = Talk.stu_action (#talk s) a (now, #speed s)
        val s = update s (Talk (fn _ => talk'))
        val s = update s (TechnicalContent (fn tc => tc + dt))
        (* Update the committee based on talk progress *)
        val s = update s (Committee
                              (if #speed s < 0.01 then
                                   Vector.map ((CInfo.pause dtm) o
                                               (CInfo.time now dtm) o
                                               (CInfo.add (dm, dt, dr)))
                               else
                                   Vector.map ((CInfo.time now dtm) o
                                               (CInfo.add (dm, dt, dr)))))
        (* Update student's stats *)
        val s = update s (Stamina (fn st =>
                                      Real.max (st - ((#speed s) - 0.8) * dtm
                                               * 10.0,
                                                0.0)))
        val s = if #speed s < 0.01 then
                    update s (Knowledge (fn k => k + dtm))
                else
                    s
        val s = if #stamina s < 0.01 then
                    update s (Speed (fn _ => 0.0))
                else
                    s
        (* Apply the action, if any, to the remaining state *)
        val s =
            case a of
                NONE => s
              | SOME Pause => update s (Speed (fn _ => 0.0))
              | SOME Resume => update s (Speed (fn _ => 1.0))
              | SOME NextSlide => s
              | SOME (Jump _) => update s (Committee (Vector.map CInfo.jump))
              | SOME (Delay _) => update s (Committee (Vector.map CInfo.delay))
              | SOME Drink => update (update s (Stamina (fn st => st + 1.0)))
                                     (Committee (Vector.map CInfo.drink))
              | _ => s
        fun answers ({ typ, slide, expires }, _) a talk candodge =
            case (slide, a) of
                (SOME slide, NextSlide) =>
                slide = (Talk.currslide talk) + 1
              | (SOME slide, Jump slide') => slide = slide'
              | (SOME slide, Delay slide') =>
                (slide = slide') andalso
                (not (Talk.covered talk slide'))
              | (SOME slide, Remind slide') =>
                (slide = slide') andalso
                (Talk.covered talk slide')
              | (_, Answer) => true
              | (_, Dodge) => candodge
              | _ => false
        fun unanswered ({ typ, slide, expires }, asker) s =
            update s (Committee (Vector.mapi (fn (i, c) =>
                                                 CInfo.didntanswer (i = asker) c
                                             )))
        fun handle_q (q as ({ typ, slide, expires }, asker), (qs, s, candodge))
            =
            let val answered =
                    case a of
                        NONE => false
                      | SOME a => answers q a (#talk s) candodge
                val dodged =
                    case a of
                        SOME Dodge => true
                      | _ => false
            in
                if answered then
                    (* Question has been answered *)
                    (qs, s, candodge andalso (not dodged))
                else
                    if GTime.gt (now, expires) then
                        (* Question has expired unanswered *)
                        (qs, unanswered q s, candodge)
                    else
                        (* Otherwise, pass the question along *)
                        (q::qs, s, candodge)
            end
        val (qs, s, _) = List.foldl handle_q ([], s, true) (#questions s)
        val s = update s (Questions (fn _ => qs))
    in
       s
    end

fun addq s q asker =
    update s (Questions (fn qs => (q, asker)::qs))

fun within dist (r1, r2) =
    Real.abs (r1 - r2) < dist

fun qeq ((q1, a1), (q2, a2)) =
    (a1 = a2) andalso
    (case (#typ q1, #typ q2) of
         (Motivation, Motivation) => true
       | (Technical, Technical) => true
       | (Related, Related) => true
       | _ => false) andalso
    (case (#slide q1, #slide q2) of
         (SOME s1, SOME s2) => s1 = s2
       | (NONE, NONE) => true
       | _ => false) andalso
    (GTime.within (GTime.fromSeconds 0.1) (#expires q1, #expires q2))

fun cmpq ((q1, a1), (q2, a2)) =
    (Int.compare (a1, a2)) andthen
    (case (#typ q1, #typ q2) of
         (Motivation, Motivation) => EQUAL
       | (Motivation, _) => LESS
       | (_, Motivation) => GREATER
       | (Technical, Technical) => EQUAL
       | (Technical, Related) => LESS
       | (Related, Related) => EQUAL
       | (Related, Technical) => GREATER) andthen
    (case (#slide q1, #slide q2) of
         (NONE, NONE) => EQUAL
       | (NONE, _) => LESS
       | (_, NONE) => GREATER
       | (SOME s1, SOME s2) => Int.compare (s1, s2)) andthen
    (GTime.cwithin (GTime.fromSeconds 0.1) (#expires q1, #expires q2))

fun eq dist ((s1, s2): state * state) =
    (within dist (#knowledge s1, #knowledge s2)) andalso
    (within dist (#stamina s1, #stamina s2)) andalso
    (Talk.eq dist (#talk s1, #talk s2)) andalso
    (V.foldli (fn (i, c, rest) =>
                  rest andalso (CInfo.eq dist (c, V.sub (#committee s2, i))))
              true
              (#committee s1)) andalso
    (within dist (#technical_content s1, #technical_content s2)) andalso
    (within 0.01 (#speed s1, #speed s2)) andalso
    ListPair.allEq qeq (#questions s1, #questions s2)

fun compare dist ((s1, s2): state * state) =
    (cwithin dist (#knowledge s1, #knowledge s2)) andthen
    (cwithin dist (#stamina s1, #stamina s2)) andthen
    (Talk.compare dist (#talk s1, #talk s2)) andthen
    (V.foldli (fn (i, c, rest) =>
                  rest andthen
                       (CInfo.compare dist (c, V.sub (#committee s2, i))))
              EQUAL
              (#committee s1)) andthen
    (cwithin dist (#technical_content s1, #technical_content s2)) andthen
    (cwithin 0.01 (#speed s1, #speed s2)) andthen
    ListPair.foldl (fn (a, b, c) => c andthen cmpq (a, b))
                   EQUAL (#questions s1, #questions s2)
end
