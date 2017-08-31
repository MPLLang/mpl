structure StuPlayer =
struct

type stu_player = State.state option

fun talk _ =
    {currslide = 0,
     lasttime = GTime.fromMinutes 0.0,
     slides = Vector.tabulate (45, fn _ => ({motivation = 1, technical = 3, related = 1}, 0.0)),
     speaking = true}


open Action
fun move (cs, now) ps =
    let val ns =
            case ps of
                SOME ps => if State.eq 1.0 (cs, ps) then ps
                           else
                               (print (State.toString cs);
                                cs)
              | NONE => (print (State.toString cs);
                         cs)
    in
        (SOME ns,
         case TextIO.canInput (TextIO.stdIn, 1) of
             NONE => NONE
           | _ =>
             (case TextIO.input1 TextIO.stdIn of
                  NONE => OS.Process.exit OS.Process.success
                | SOME #"p" => SOME Pause
                | SOME #"r" => SOME Resume
                | SOME #"n" => SOME NextSlide
                | SOME #"a" => SOME Answer
                | SOME #"d" => SOME Dodge
                | SOME #"w" => SOME Drink
                | _ => NONE))
    end

fun init () = NONE

fun endgame win _ =
    if win then
        print "You win!"
    else
        print "You lose!"
end
