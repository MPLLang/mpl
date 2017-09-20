structure Action =
struct

datatype stu_act =
         Pause
         | Resume
         | NextSlide
         | Jump of int (* slide number *)
         | Delay of int (* slide number *)
         | Remind of int (* slide number *)
         | Answer
         | Dodge
       | Drink

datatype qtype =
         Motivation
       | Technical
       | Related

type question =
     { typ: qtype,
       slide: int option,
       expires: GTime.gtime }

type ext_question =
     { typ: qtype,
       slide: int option,
       urgent: bool }

fun typ ({typ, ...}: question) = typ
fun slide ({slide, ...}: question) = slide
fun expires ({expires, ...}: question) = expires
fun etyp ({typ, ...}: ext_question) = typ
fun eslide ({slide, ...}: ext_question) = slide
fun eurgent ({urgent, ...}: ext_question) = urgent

fun p i = Int.toString i

fun sa_to_string (a: stu_act) =
    case a of
        Pause => "Pause"
      | Resume => "Resume"
      | NextSlide => "Next"
      | Jump n => "Jump " ^ (p n)
      | Delay n => "Delay " ^ (p n)
      | Remind n => "Remind " ^ (p n)
      | Answer => "Answer"
      | Dodge => "Dodge"
      | Drink => "Drink"

fun eq_to_string ({typ, slide, urgent} : ext_question) =
    (case typ of
         Motivation => "Motivation"
       | Related => "Related"
       | Technical => "Technical") ^
    (case slide of
         NONE => ""
       | SOME n => " " ^ (Int.toString n)) ^
    (if urgent then " urgent" else "")

end
