structure TC = Posix.TTY.TC
structure V = Vector
structure Const = Constants

val termios = TC.getattr Posix.FileSys.stdout

val {iflag, oflag, cflag, lflag, cc, ispeed, ospeed} =
    Posix.TTY.fieldsOf termios

fun term_default () =
    TC.setattr (Posix.FileSys.stdout, TC.sanow, termios)

fun term_game () =
    let val new_fields = {iflag = iflag,
                          oflag = oflag,
                          cflag = cflag,
                          lflag = Posix.TTY.L.flags [],
                          cc = cc (* V.update (cc, [(V.erase, #"\b")]) *),
                          ispeed = ispeed,
                          ospeed = ospeed}
        val new_termios = Posix.TTY.termios new_fields in
        TC.setattr (Posix.FileSys.stdout, TC.sanow, new_termios)
    end

val _ = term_game ()
val _ = OS.Process.atExit term_default

structure StuPlayer =
struct

type stu_player = State.state option

fun talk _ =
    {currslide = 0,
     lasttime = GTime.fromMinutes 0.0,
     slides = V.tabulate (45, fn _ => ({motivation = 1, technical = 3, related = 1}, 0.0)),
     speaking = true}

fun stateToString ({knowledge, stamina, talk, committee, time, technical_content,
                   speed, questions} : State.state) =
    let fun plen s =
            let fun count s a =
                    if String.size s = 0 then a
                    else if String.isPrefix Ansi.CSI s then
                        eat (String.extract (s, String.size Ansi.CSI, NONE)) a
                    else
                        count (String.extract (s, 1, NONE)) (a + 1)
                and eat s a =
                    if String.size s = 0 then a
                    else if Char.isAlpha (String.sub (s, 0)) then
                        count (String.extract (s, 1, NONE)) a
                    else
                        eat (String.extract (s, 1, NONE)) a
            in
                count s 0
            end
        fun color (min, max) v =
            let val pct = (v - min) / (max - min)
            in
                if pct < 0.1 then Ansi.red
                else if pct < 0.3 then Ansi.yellow
                else Ansi.green
            end
        fun rcolor (min, max) v =
            let val pct = (v - min) / (max - min)
            in
                if pct > 0.9 then Ansi.red
                else if pct > 0.7 then Ansi.yellow
                else Ansi.green
            end
        fun scolor _ v =
            if v >= 0.99 then Ansi.green
            else if v > 0.5 then Ansi.yellow
            else Ansi.white
        fun white _ _ = Ansi.white
        fun fill (n, s) = String.concat (List.tabulate (n, fn _ => s))
        fun bar (s, col, max, maxl) v =
            let val n = Real.ceil (v * (real maxl) / max)
            in
                (Ansi.bgColor (col (0.0, max) v)) ^
                (fill (n, s)) ^
                (Ansi.reset)
            end
        fun vbar (s, col, max, maxh) v h =
            let val n = Real.ceil (v * (real maxh) / max)
            in
                if n > h then
                    (Ansi.bgColor (col (0.0, max) v)) ^
                    s ^
                    (Ansi.reset)
                else fill (plen s, " ")
            end
        val width = 80
        val ncomm = V.length (committee)
        fun space l =
            let val totlen = List.foldl (fn (s, l) => (plen s) + l) 0 l
                val n = (width - totlen) div (List.length l)
                val r = (width - totlen - n * ((List.length l) - 1)) div 2
            in
                (fill (r, " ")) ^ (String.concatWith (fill (n, " ")) l) ^
                (fill (r, " "))
            end
        fun comm () =
            let fun ln h =
                    space (List.tabulate
                               (ncomm * 3,
                                fn i =>
                                   case i mod 3 of
                                       0 => vbar ("  ", color, 100.0, 10)
                                                 (#patience (V.sub (committee,
                                                                    i div 3)))
                                             (10 - h)
                                     | 1 => vbar ("  ", rcolor, 100.0, 10)
                                                 (#confusion (V.sub (committee,
                                                                     i div 3)))
                                             (10 - h)
                                     | 2 => " "))
            in
                String.concatWith "\n" (List.tabulate (10, ln))
            end
        fun uline (s, i) =
            (String.extract (s, 0, SOME i)) ^ Ansi.underline true ^
            (String.extract (s, i, SOME 1)) ^ Ansi.underline false ^
            (String.extract (s, i + 1, NONE))
        fun box color width lines =
            let val width =
                    case width of
                        SOME w => w
                      | NONE =>List.foldl (fn (s, m) =>
                                              Int.max (String.size s, m))
                                           0
                                           lines
                val height = List.length lines
                val ul = "+" (* String.str (Char.chr 218) *)
                val ur = "+" (* String.str (Char.chr 191) *)
                val ll = "+" (* String.str (Char.chr 192) *)
                val lr = "+" (* String.str (Char.chr 217) *)
                val h = "-" (* String.str (Char.chr 196) *)
                val v = "|" (* String.str (Char.chr 179) *)
            in
                (Ansi.fullColor color) ^ ul ^ (fill (width, h)) ^ ur ^ "\n" ^
                (String.concat (List.map (fn s => v ^ s ^ v ^ "\n") lines)) ^
                ll ^ (fill (width, h)) ^ lr ^ "\n" ^
                (Ansi.reset)
            end
        fun questionToString ({typ, slide, expires}, a) =
            let val tl = (GTime.toMinutes (GTime.minus
                                             (expires,
                                              time)))
                val urgent = tl < 0.501
                val lines =
                    [(case typ of
                          Action.Motivation => "Motivation"
                        | Action.Related => "Related work"
                        | Action.Technical => "Technical") ^
                     " question from committee member " ^ (Int.toString a),
                     (bar (" ", color, 1.0, width - 2)
                          tl)
                    ]
                val color = {bg = SOME (if urgent then Ansi.dark_red
                                        else Ansi.dark_blue),
                             fg = SOME Ansi.bright_white}
            in
                box color (SOME (width - 2)) lines
            end
    in
        (Ansi.clearScreen) ^
        "Knowledge: " ^ (bar (" ", color, 100.0, width - 11) knowledge) ^ "\n" ^
        "Stamina:   " ^ (bar (" ", color, 100.0, width - 11) stamina) ^ "\n" ^
        "\n" ^
        "Slide " ^ (Int.toString (Talk.currslide talk)) ^ "/" ^
        (Int.toString (Talk.numslides talk)) ^ "\n" ^
        (bar (" ", scolor, 1.0, width)
             (Talk.amtcovered talk (Talk.currslide talk))) ^
        "\n\n" ^
        (comm ()) ^ "\n\n" ^
        "Time:    " ^ (bar (" ", color, 45.0, width - 9)
                        (GTime.toMinutes (GTime.minus
                                             (GTime.fromMinutes Const.talk_time,
                                              time)))) ^ "\n" ^
        (String.concat (List.map questionToString questions)) ^ "\n\n" ^
        "Content: " ^ (bar (" ", white, Const.win_tech_content, width - 9)
                           technical_content) ^ "\n\n" ^
        (space [uline ("Pause", 0), uline ("Resume", 0), uline ("Next", 0),
                uline ("Answer", 0), uline ("Dodge", 0),
                uline ("Drink water", 6), uline ("Quit", 0)]) ^ "\n"
    end

open Action
fun move (cs, now) ps =
    let val ns =
            case ps of
                SOME ps => (* if State.eq 0.01 (cs, ps) then ps
                           else *)
                               (print (stateToString cs);
                                cs)
              | NONE => (print (stateToString cs);
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
                | SOME #"q" => OS.Process.exit OS.Process.success
                | _ => NONE))
    end

fun init () = NONE

fun endgame win _ =
    if win then
        print "You win!"
    else
        print "You lose!"
end
