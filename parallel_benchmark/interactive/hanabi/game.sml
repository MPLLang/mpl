open Card
open Move
open GameState

fun cardValue (g: game) ((c, n, _): card)  =
    let fun count f l =
            List.foldl (fn (x, c) => if f x then c + 1 else c) 0 l
        fun countcn c n =
            count (fn (c', n', _) => c' = c andalso n' = n) (discard g)
        fun maxScorec (c: color) =
            if countcn c 1 = 3 then 0
            else if countcn c 2 = 2 then 1
            else if countcn c 3 = 2 then 2
            else if countcn c 4 = 2 then 3
            else if countcn c 5 = 1 then 4
            else 5
        fun pow (b, e) =
            if e = 0 then 1 else
            if e = 1 then b else
            b * (pow (b, e - 1))
    in
        if numPlayed (g, c) >= n then 0
        else if maxScorec c < n then 0
        else
            if n = 5 then 5
            else if n = 1 then
                pow (4, countcn c 1)
            else
                pow (5 - n, (countcn c n) + 1)
    end

fun playValue (g: game) ((c, n, _): card) =
    if n = numPlayed (g, c) + 1 then 5 + n else ~10

fun hcardValue (vf: card -> int) (g: game) (p: int) (c: hcard) =
    let val vs = List.map vf (canBe (seenByPlayer (g, p)) c)
    in
        Int.div (List.foldl op+ 0 vs, List.length vs)
    end

fun S (p: int) (g: game) =
    let val know = List.nth (knowledge g, p)
        val canBes = List.map (canBe (seenByPlayer (g, p))) know
        val S = List.foldl op* 1 (List.map List.length canBes)
    in
        if S = 0 then 1 else S
    end

fun evalMoveHidden (g: game) (p: int) (m: move) =
    case m of
        Discard i =>
        1 -
        (hcardValue (cardValue g) g p (List.nth (List.nth (knowledge g, p), i)))
      | Play i =>
        hcardValue (playValue g) g p (List.nth (List.nth (knowledge g, p), i))
      | Info (p', _, _) =>
        let val p'S = S p' g
            val (_, g') = update (g, m)
            val p'S' = S p' g'
        in
            (Int.div (p'S, p'S')) - 2 + (infoTokens g)
        end

fun evalMove (g: game) (p: int) (m: move) =
    case m of
        Discard i => 1 - (cardValue g (List.nth (List.nth (hand g, p), i)))
      | Play i =>
        playValue g (List.nth (List.nth (hand g, p), i))
      | Info (p', _, _) =>
        let val p'S = S p' g
            val (_, g') = update (g, m)
            val p'S' = S p' g'
        in
            (Int.div (p'S, p'S')) - 2 + (infoTokens g)
        end

fun evalState (g: game) =
    let fun count f l =
            List.foldl (fn (x, c) => if f x then c + 1 else c) 0 l
        fun countcn c n =
            count (fn (c', n', _) => c' = c andalso n' = n) (discard g)
        fun maxScorec (c: color) =
            if countcn c 1 = 3 then 0
            else if countcn c 2 = 2 then 1
            else if countcn c 3 = 2 then 2
            else if countcn c 4 = 2 then 3
            else if countcn c 5 = 1 then 4
            else 5
        val maxScore =
            (maxScorec Red) + (maxScorec Yellow) + (maxScorec Green) +
            (maxScorec Blue) + (maxScorec White)
    in
        2 * (score g)
        - 5 * (25 - maxScore)
        + (infoTokens g)
        - 10 * (fuseLeft g)
    end

fun topn n l =
    let fun insert_rec ((m, v), l) =
            case l of
                [] => [(m, v)]
              | (hm, hv)::t =>
                if hv > v then (m, v)::(hm, hv)::t
                else (hm, hv)::(insert_rec ((m, v), t))
        fun insert ((m, v), l) =
            if List.length l < n then insert_rec ((m, v), l)
            else List.tl (insert_rec ((m, v), l))
    in
        List.foldl insert [] l
    end

fun compMove (me: int) (my_state: player_state) (my_moves: move list) =
    let val know = List.nth (#knowledge my_state, me)
        val canBes = List.map (canBe (pseenByPlayer (my_state, me))) know
        val S = List.foldl op* 1 (List.map List.length canBes)
        fun app ls x = List.map (fn l => x::l) ls
        fun cross (l, ls) = List.concat (List.map (app ls) l)
        val perms = List.foldr cross [[]] canBes
        val games = List.map (imagineGame my_state) perms
        val game1 = List.hd games
        fun moveVals m = List.map (fn g => evalMove g me m) games
        fun moveTot m =
            case m of
                Info _ => evalMove game1 me m
              | _ => List.foldl op+ 0 (moveVals m)
        val mvs = List.map (fn m => (m, moveTot m)) my_moves
        val best5 = List.map (fn (a, _) => a) (topn 5 mvs)

        fun maximax (d: int) (g: game) =
            if d = 0 orelse isOver g then evalState g
            else
                let val p = playerTurn g
                    val p_moves = moves g
                    val mvs = List.map (fn m => (m, evalMoveHidden g p m))
                                       p_moves
                    val best5 = List.map (fn (a, _) => a) (topn 5 mvs)
                    fun mv m = (m, maximax (d - 1) (#2 (update (g, m))))
                               handle Div => (m, ~10000)
                    val mvs' = List.map mv best5
                in
                    #2 (List.hd (topn 1 mvs'))
                end

        fun mv m = (m, maximax ((#players my_state) + 1)
                               (#2 (update (game1, m))))
        val mvs' = List.map mv best5
    in
        #1 (List.hd (topn 1 mvs'))
    end

fun movesToString i names (ms : move list) =
    case ms of
        [] => ""
      | [m] => (Int.toString i) ^ ") " ^ (moveToString names m)
      | m::t =>
        (Int.toString i) ^ ") " ^ (moveToString names m) ^ "\n"
        ^ (movesToString (i + 1) names t)

fun getInt min max =
    let val inp = TextIO.inputLine TextIO.stdIn
    in
        case inp of
            SOME inp =>
            (case Int.fromString inp of
                 SOME n => if n >= min andalso n <= max then n
                           else getInt min max
               | NONE => getInt min max)
          | NONE => getInt min max
    end

fun loop (names: string list) (g: game) =
    let val ms = moves g
        val p = playerTurn g
        val name = List.nth (names, p)
    in
        if isOver g then print ("Game Over. Score: " ^
                                (Int.toString (score g)) ^ "\n")
        else
        if p = 0 then
            (* Human player *)
            let val _ = print (playerStateToString (playerState g))
                val _ = print ((movesToString 1 names ms) ^ "\n")
                val move = (getInt 1 (List.length ms)) - 1
                val (s, g') = update (g, List.nth (ms, move))
            in
                print (s ^ "\n");
                loop names g'
            end
        else
            (* Computer player *)
            let val move = compMove p (playerState g) ms
                val _ = print
                            (name ^ " plays \"" ^
                             (moveToString names move) ^ "\"\n")
                val (s, g') = update (g, move)
            in
                print (s ^ "\n");
                loop names g'
            end
    end

val names = ["Stefan", "Player 1", "Player 2", "Player 3"]

val _ = loop names (new names)
