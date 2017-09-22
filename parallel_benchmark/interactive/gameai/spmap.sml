structure StatePlayerMap =
struct

datatype player =
         Student
         | Committee of int

datatype move =
         StuMove of Action.stu_act
         | ComMove of Action.ext_question * int

type sp = State.state * GTime.gtime * player

fun mk s t p = (s, t, p)

fun advance dt (s, t, Student) = (s, t, Committee 0)
  | advance dt (s, t, Committee i) =
    if i = (Vector.length (State.committee s)) - 1 then
        (s, GTime.plus (t, dt), Student)
    else
        (s, t, Committee (i + 1))

fun apply ((sp as (s, t, p)): sp) (m: move option) =
    case m of
        NONE => sp
      | SOME (StuMove a) =>
        (State.stu_act s (SOME a) t, t, p)
      | SOME (ComMove (q, i)) =>
        (State.addq s q i t, t, p)

fun state ((s, _, _): sp) = s
fun time ((_, t, _): sp) = t
fun player ((_, _, p): sp) = p


structure StatePlayerKey : ORD_KEY =
struct
type ord_key = sp
fun compare ((s1, t1, p1), (s2, t2, p2)) =
    case (p1, p2) of
        (Student, Student) =>
        (*(GTime.cwithin (GTime.fromMinutes 1.0) (t1, t2))
            andthen *) (State.compare 5.0 (s1, s2))
      | (Student, _) => LESS
      | (Committee _, Student) => GREATER
      | (Committee i1, Committee i2) =>
        if i1 < i2 then LESS
        else if i1 > i2 then GREATER
        else ((*(GTime.cwithin (GTime.fromMinutes 1.0) (t1, t2))
                  andthen *) (State.compare 5.0 (s1, s2)))
end

structure Map : ORD_MAP = SplayMapFn (StatePlayerKey)

open Map

end
