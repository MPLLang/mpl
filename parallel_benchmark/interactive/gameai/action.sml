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

end
