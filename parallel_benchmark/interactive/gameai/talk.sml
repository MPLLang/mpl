structure Talk =
struct

type slide =
     { motivation: int,
       technical: int,
       related: int }

type talk =
     {(* slide, amount covered *)
       slides: (slide * real) Vector.vector,
       currslide: int,
       speaking: bool,
       lasttime: GTime.gtime
     }

structure V = Vector

fun seq ((s1, s2): slide * slide) =
    (#motivation s1 = #motivation s2) andalso
    (#technical s1 = #technical s2) andalso
    (#related s1 = #related s2)

fun cmps ((s1, s2): slide * slide) =
    (Int.compare (#motivation s1, #motivation s2)) andthen
    (Int.compare (#technical s1, #technical s2)) andthen
    (Int.compare (#related s1, #related s2))

fun eq dist ((t1, t2): talk * talk) =
    (V.foldli (fn (i, (s1, r1), rest) =>
                  rest andalso
                  (let val (s2, r2) = V.sub (#slides t2, i) in
                       (seq (s1, s2)) andalso
                       (Real.abs (r1 - r2) < dist / 100.0)
                   end))
              true
              (#slides t1)) andalso
    (#currslide t1 = #currslide t2) andalso
    (#speaking t1 = #speaking t2)

fun bcompare (false, false) = EQUAL
  | bcompare (false, true) = LESS
  | bcompare (true, false) = GREATER
  | bcompare (true, true) = EQUAL

fun compare dist ((t1, t2): talk * talk) =
    (V.foldli (fn (i, (s1, r1), rest) =>
                  rest andthen
                  (let val (s2, r2) = V.sub (#slides t2, i) in
                       (cmps (s1, s2)) andthen
                       (cwithin (dist / 100.0) (r1, r2))
                   end))
              EQUAL
              (#slides t1)) andthen
    (Int.compare (#currslide t1, #currslide t2)) andthen
    (bcompare (#speaking t1, #speaking t2))

datatype update =
         Slides of (slide * real) Vector.vector
         | Currslide of int
         | Speaking of bool
         | Lasttime of GTime.gtime

fun update (u: update, { slides, currslide, speaking, lasttime })  =
    case u of
        Slides v => { slides = v, currslide = currslide, speaking = speaking,
                      lasttime = lasttime }
      | Currslide v => { slides = slides, currslide = v, speaking = speaking,
                         lasttime = lasttime }
      | Speaking v => { slides = slides, currslide = currslide, speaking = v,
                        lasttime = lasttime }
      | Lasttime v => { slides = slides, currslide = currslide,
                        speaking = speaking, lasttime = v }

fun updates (t: talk) us =
    List.foldl update t us

fun slide (t: talk) (n: int) =
    #1 (Vector.sub (#slides t, n))

fun currslide (t: talk) =
    #currslide t

fun covered (t: talk) (n: int) =
    let val (s, cov) = V.sub (#slides t, n)
    in
        cov > 0.5
    end

open Action

fun stu_action (t: talk) (a: Action.stu_act option) (now, speed) =
    let val (s, cov) = V.sub (#slides t, currslide t)
        val {motivation, technical, related} = s

        val tm = GTime.minus (now, #lasttime t)
        val dcov = Real.min ((GTime.toMinutes tm) * speed * 0.9 + 0.1,
                             1.0 - cov)
        val dm = Real.* (Real.fromInt motivation, dcov)
        val dt = Real.* (Real.fromInt technical, dcov)
        val dr = Real.* (Real.fromInt related, dcov)
        val slides = V.update (#slides t, currslide t, (s, cov + dcov))
        val t = updates t [Lasttime now, Slides slides]
        val t' =
            case a of
                NONE => t
              | SOME Pause => update (Speaking false, t)
              | SOME Resume => update (Speaking true, t)
              | SOME NextSlide =>
                let val cs = currslide t
                    val cs' = if cs = Vector.length (#slides t) - 1 then cs
                              else cs + 1
                in
                    update (Currslide cs', t)
                end
              | SOME (Jump s) =>
                if s >= Vector.length (#slides t) orelse s < 0 then t
                else update (Currslide s, t)
              | SOME (Delay _) => t
              | SOME (Remind _) => t
              | SOME Answer => t
              | SOME Dodge => t
              | SOME Drink => t
    in
        (t', dm, dt, dr)
    end
end
