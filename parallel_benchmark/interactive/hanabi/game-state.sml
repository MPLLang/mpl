open Card
open Move

signature GAME_STATE =
sig

type game

type player_state =
     { players : int,
       names : string list,
       discard : card list,
       cardsLeft : int,
       hand : card list list,
       knowledge : hcard list list,
       played : card list,
       info : int,
       fuse : int,
       playerTurn : int,
       endGameMoves : int }

exception GameOver of int
exception BadRequest

val new : string list -> game

val isOver : game -> bool

val score : game -> int
val played : game -> card list
val numPlayed : game * color -> int
val knowledge : game -> hcard list list
val hand : game -> card list list
val discard : game -> card list
val cardsLeft : game -> int
val infoTokens : game -> int
val fuseLeft : game -> int
val seenByPlayer : game * int -> card list

val update : game * move -> string * game

val playerTurn : game -> int
val playerState : game -> player_state
val playerStateToString : player_state -> string
val moves : game -> move list
val pnumPlayed : player_state * color -> int
val pseenByPlayer : player_state * int -> card list
val imagineGame : player_state -> card list -> game

end

structure GameState :> GAME_STATE =
struct

type game =
     { players : int,
       names : string list,
       deck : card list,
       discard : card list,
       hand : card list list,
       knowledge : hcard list list,
       played : card list,
       info : int,
       fuse : int,
       playerTurn : int,
       endGameMoves : int,
       imaginary : bool}

type player_state =
     { players : int,
       names : string list,
       discard : card list,
       cardsLeft : int,
       hand : card list list,
       knowledge : hcard list list,
       played : card list,
       info : int,
       fuse : int,
       playerTurn : int,
       endGameMoves : int }

exception GameOver of int
exception Invalid
exception BadRequest

val s = case MLton.Random.seed () of
            SOME s => s
          | NONE => 0w243875
val _ = MLton.Random.srand s

fun randInt (max : int) =
    Word.toInt (Word.mod (MLton.Random.rand (), Word.fromInt max))

fun randChoice (l: 'a list) : 'a * 'a list =
    if List.length l = 0 then raise Invalid
    else
    let val n = randInt (List.length l)
    in
        (List.nth (l, n), (List.take (l, n)) @ (List.drop (l, n + 1)))
    end

fun shuffle (l : 'a list) : 'a list =
    if List.length l <= 1 then l
    else
        let val (h, t) = randChoice l in
            h::(shuffle t)
        end

fun new (names : string list) : game =
    let val players = List.length names
        val ideck = shuffle Card.deck
        val (hands, deck) =
            List.foldl (fn (_, (hs, d)) =>
                           ((List.take (d, 4))::hs,
                            List.drop (d, 4)))
                       ([], ideck)
                       (List.tabulate (players, (fn _ => ())))
    in
        { players = players,
          names = names,
          deck = deck,
          discard = [],
          hand = hands,
          knowledge = List.tabulate (players,
                                     fn _ => [(NONE, NONE), (NONE, NONE),
                                              (NONE, NONE), (NONE, NONE)]),
          played = [],
          info = 8,
          fuse = 4,
          playerTurn = randInt players,
          endGameMoves = players,
          imaginary = false }
    end

fun played g = #played g
fun discard g = #discard g
fun cardsLeft (g: game) = List.length (#deck g)
fun infoTokens (g: game) = #info g
fun fuseLeft (g: game) = #fuse g
fun playerTurn (g: game) = #playerTurn g
fun hand (g: game) = if #imaginary g then #hand g
                     else raise BadRequest
fun knowledge (g: game) = if #imaginary g then #knowledge g
                          else raise BadRequest

fun numPlayed (g: game, c: color) : int =
    List.length (List.filter (fn (c', _, _) => c = c') (played g))
fun pnumPlayed (g: player_state, c: color) : int =
    List.length (List.filter (fn (c', _, _) => c = c') (#played g))

fun score (g: game) : int =
    (numPlayed (g, Red)) +
    (numPlayed (g, Yellow)) +
    (numPlayed (g, Green)) +
    (numPlayed (g, Blue)) +
    (numPlayed (g, White))

fun isOver (g: game) =
    (fuseLeft g = 0) orelse (#endGameMoves g = 0)

fun seenByPlayer (g : game, p: int) : card list =
    (List.concat ((List.take (#hand g, p)) @ (List.drop (#hand g, p + 1))))
    @ (discard g)

fun update (g: game, m: move) =
    let fun addInfo ((_, n), Color c) = (SOME c, n)
          | addInfo ((c, _), Number n) = (c, SOME n)
        val players = #players g
        val names = #names g
        val deck = #deck g
        val discard = #discard g
        val hand = #hand g
        val knowledge = #knowledge g
        val played = #played g
        val info = #info g
        val fuse = #fuse g
        val p = playerTurn g
        val egm = #endGameMoves g
        val egm' = if List.length deck > 0 then egm
                   else if egm = 0 then raise Invalid
                   else egm - 1
    in
        case m of
            Discard i =>
            (case deck of
                 [] =>
                ("", { players = players,
                       names = names,
                       deck = [],
                       discard = (List.nth (List.nth (#hand g, p), i))::discard,
                       hand =
                         (List.take (#hand g, p)) @
                         ((List.take (List.nth (#hand g, p), i)) @
                          (List.drop (List.nth (#hand g, p), i + 1)))::
                         (List.drop (#hand g, p + 1)),
                       knowledge =
                         (List.take (#knowledge g, p)) @
                         ((List.take (List.nth (#knowledge g, p), i)) @
                          (List.drop (List.nth (#knowledge g, p), i + 1)))::
                         (List.drop (#knowledge g, p + 1)),
                       played = played,
                       info = Int.min (info + 1, 8),
                       fuse = fuse,
                       playerTurn = (p + 1) mod players,
                       endGameMoves = egm',
                       imaginary = #imaginary g })
              | h::t =>
                ("", { players = players,
                       names = names,
                       deck = t,
                       discard = (List.nth (List.nth (#hand g, p), i))::discard,
                       hand =
                         (List.take (#hand g, p)) @
                         ((List.take (List.nth (#hand g, p), i)) @
                          h::(List.drop (List.nth (#hand g, p), i + 1)))::
                         (List.drop (#hand g, p + 1)),
                       knowledge =
                         (List.take (#knowledge g, p)) @
                         ((List.take (List.nth (#knowledge g, p), i)) @
                          (NONE, NONE)::
                          (List.drop (List.nth (#knowledge g, p), i + 1)))::
                         (List.drop (#knowledge g, p + 1)),
                       played = played,
                       info = Int.min (info + 1, 8),
                       fuse = fuse,
                       playerTurn = (p + 1) mod players,
                       endGameMoves = egm',
                       imaginary = #imaginary g}))

            | Play i =>
              let val card as (c, n, _) = List.nth (List.nth (#hand g, p), i)
                  val isValid = n = numPlayed (g, c) + 1
                  val (deck, hand, knowledge) =
                      case deck of
                          [] =>
                          ([],
                           (List.take (#hand g, p)) @
                           ((List.take (List.nth (#hand g, p), i)) @
                            (List.drop (List.nth (#hand g, p), i + 1)))::
                           (List.drop (#hand g, p + 1)),
                           (List.take (#knowledge g, p)) @
                           ((List.take (List.nth (#knowledge g, p), i)) @
                            (List.drop (List.nth (#knowledge g, p), i + 1)))::
                           (List.drop (#knowledge g, p + 1)))
                        | h::t =>
                          (t,
                           (List.take (#hand g, p)) @
                           ((List.take (List.nth (#hand g, p), i)) @
                            h::(List.drop (List.nth (#hand g, p), i + 1)))::
                           (List.drop (#hand g, p + 1)),
                           (List.take (#knowledge g, p)) @
                           ((List.take (List.nth (#knowledge g, p), i)) @
                            (NONE, NONE)::
                            (List.drop (List.nth (#knowledge g, p), i + 1)))::
                           (List.drop (#knowledge g, p + 1)))
                  val fuse = if isValid then fuse
                             else
                                 if fuse = 0 then raise Invalid
                                 else fuse - 1
              in
                  (if isValid then "Card was successfully played"
                   else "Card was invalid!",
                  { players = players,
                    names = names,
                    deck = deck,
                    discard = if isValid then discard else card::discard,
                    hand = hand,
                    knowledge = knowledge,
                    played = if isValid then card::played else played,
                    info = info,
                    fuse = fuse,
                    playerTurn = (p + 1) mod players,
                    endGameMoves = egm',
                    imaginary = #imaginary g })
              end

            | Info (p', inf, cs) =>
              if info = 0 then raise Invalid else
              ("",
              { players = players,
                names = names,
                deck = deck,
                discard = discard,
                hand = hand,
                knowledge =
                (List.take (#knowledge g, p')) @
                (List.tabulate
                     (List.length (List.nth (#knowledge g, p')),
                      (fn i =>
                          if List.exists (fn c' => i = c') cs then
                              addInfo
                                  (List.nth (List.nth (#knowledge g, p'), i),
                                   inf)
                                   else List.nth (List.nth (#knowledge g, p'),
                                                  i))))::
                (List.drop (#knowledge g, p' + 1)),
                played = played,
                info = info - 1,
                fuse = fuse,
                playerTurn = (p + 1) mod players,
                endGameMoves = egm',
                imaginary = #imaginary g })
    end

fun playerState (g: game) : player_state =
    let val p = #playerTurn g
    in
        { players = #players g,
          names = #names g,
          discard = #discard g,
          cardsLeft = cardsLeft g,
          hand = (List.take (#hand g, p)) @ (List.drop (#hand g, p + 1)),
          played = #played g,
          knowledge = #knowledge g,
          info = #info g,
          fuse = #fuse g,
          playerTurn = #playerTurn g,
          endGameMoves = #endGameMoves g }
    end

fun playerStateToString (g: player_state) : string =
    let val p = #playerTurn g
        fun hand i =
            if i = p then
                "You know: \t" ^ (hcardsToString (List.nth (#knowledge g, i)))
                ^ "\n"
            else
                (List.nth (#names g, i)) ^ " has:\t" ^
                (cardsToString (List.nth (#hand g, if i > p then i - 1 else i)))
                ^ "\n" ^ (List.nth (#names g, i)) ^ " knows:\t" ^
                (hcardsToString (List.nth (#knowledge g, i))) ^ "\n"
    in
        (List.foldr op^ "" (List.tabulate (#players g, hand))) ^ "\n" ^
        "Discard:\n" ^
        (cardsToString (#discard g)) ^ "\n\n" ^
        "Cards left: " ^ (Int.toString (#cardsLeft g)) ^ "\n\n" ^
        "Played: R Y G B W\n" ^
        "        " ^ (Int.toString (pnumPlayed (g, Red))) ^ " " ^
        (Int.toString (pnumPlayed (g, Yellow))) ^ " " ^
        (Int.toString (pnumPlayed (g, Green))) ^ " " ^
        (Int.toString (pnumPlayed (g, Blue))) ^ " " ^
        (Int.toString (pnumPlayed (g, White))) ^ "\n\n" ^
        "Info left: " ^ (Int.toString (#info g)) ^ "\n" ^
        "Fuse left: " ^ (Int.toString (#fuse g)) ^ "\n"
    end

fun pseenByPlayer (g : player_state, p: int) : card list =
    if p = #playerTurn g then
        (List.concat (#hand g)) @ (#discard g)
    else
        let val p = if p > #playerTurn g then p - 1 else p
        in
            (List.concat ((List.take (#hand g, p)) @
                          (List.drop (#hand g, p + 1))))
            @ (#discard g)
        end

fun moves (g: game) : move list =
    let val p = #playerTurn g
        val players = #players g
        fun name p = List.nth (#names g, p)
        val hand = List.nth (#hand g, p)
        fun st i f [] = []
          | st i f (h::t) =
            if f h then
                i::(st (i + 1) f t)
            else
                st (i + 1) f t
        fun moveOfInfo p hand (Color c) =
            let val is = st 0 (fn (c', _, _) => c' = c) hand
            in
                if List.length is = 0 then []
                else [Info (p, Color c, is)]
            end
          | moveOfInfo p hand (Number n) =
            let val is = st 0 (fn (_, n', _) => n' = n) hand
            in
                if List.length is = 0 then []
                else [Info (p, Number n, is)]
            end
    in
        (List.tabulate (List.length hand,
                        (fn i => Discard i))) @
        (List.tabulate (List.length hand,
                        (fn i => Play i))) @
        (if #info g > 0 then
        (List.concat
        (List.tabulate (players - 1,
                        (fn p' =>
                            let val p' = if p' >= p then p' + 1 else p'
                                val hand' = List.nth (#hand g, p')
                                val moi = moveOfInfo p' hand'
                            in
                                (moi (Color Red)) @ (moi (Color Yellow)) @
                                (moi (Color Green)) @ (moi (Color Blue)) @
                                (moi (Color White)) @
                                (moi (Number 1)) @ (moi (Number 2)) @
                                (moi (Number 3)) @ (moi (Number 4)) @
                                (moi (Number 5))
                            end))))
         else [])
    end

fun imagineGame (g: player_state) (hand: card list) : game =
    { players = #players g,
       names = #names g,
       deck = [],
       discard = #discard g,
       hand = (List.take (#hand g, #playerTurn g)) @
              hand::(List.drop (#hand g, #playerTurn g)),
       knowledge = #knowledge g,
       played = #played g,
       info = #info g,
       fuse = #fuse g,
       playerTurn = #playerTurn g,
       endGameMoves = (#endGameMoves g) + (#cardsLeft g),
       imaginary = true }

end
