open Card

structure Move =
struct

datatype info = Color of color
              | Number of int

datatype move = Discard of int
              | Play of int
              | Info of int * info * int list (* player, info, cards *)

exception Invalid

fun moveToString (names: string list) (m: move) =
    let fun name p = List.nth (names, p)
    in
        case m of
            Discard i => "Discard " ^ (Int.toString i)
          | Play i => "Play " ^ (Int.toString i)
          | Info (p, Color Red, _) =>
            "Tell " ^ (name p) ^ " about reds"
          | Info (p, Color Yellow, _) =>
            "Tell " ^ (name p) ^ " about yellows"
          | Info (p, Color Green, _) =>
            "Tell " ^ (name p) ^ " about greens"
          | Info (p, Color Blue, _) =>
            "Tell " ^ (name p) ^ " about blues"
          | Info (p, Color White, _) =>
            "Tell " ^ (name p) ^ " about whites"
          | Info (p, Number 1, _) =>
            "Tell " ^ (name p) ^ " about ones"
          | Info (p, Number 2, _) =>
            "Tell " ^ (name p) ^ " about twos"
          | Info (p, Number 3, _) =>
            "Tell " ^ (name p) ^ " about threes"
          | Info (p, Number 4, _) =>
            "Tell " ^ (name p) ^ " about fours"
          | Info (p, Number 5, _) =>
            "Tell " ^ (name p) ^ " about fives"
          | _ => raise Invalid
    end
end
