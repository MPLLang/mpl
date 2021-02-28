(* The original benchmark in GHC's nofib:
 * https://github.com/ghc/nofib/blob/f481777acf608c132db47cb8badb618ef39a0d6f/parallel/coins/coins.hs
 *)

structure L = List

(* An append-list, for faster appends. *)
datatype 'a AList
  = ANil
  | ASing of 'a
  | Append of ('a AList * 'a AList)

fun lenA (ls : 'a AList) : int =
  case ls of
    ANil => 0
  | ASing _ => 1
  | Append (l, r) => lenA l + lenA r

fun showA (f : 'a -> string) (ls : 'a AList) : string =
  case ls of
    ANil => "(ANil)"
  | ASing i => "(ASing " ^ f i ^ ")"
  | Append (l, r) => "(Append " ^ showA f l ^ " " ^ showA f r ^ ")"

fun append (ls1 : 'a AList) (ls2 : 'a AList) : 'a AList =
  case (ls1,ls2) of
    (ANil, r) => r
  | (l, ANil) => l
  | (l, r) => Append (l, r)

type coin = int * int
type intlist = int L.list

fun payA_seq (amt : int) (coins : coin L.list) (acc : int L.list) : intlist AList =
  if amt = 0
  then ASing acc
  else
    case coins of
      [] => ANil
    | ((c,q) :: coins_rst) =>
      if c > amt
      then payA_seq amt coins_rst acc
      else
        let
          val coins1 = if q = 1 then coins_rst else (c,q-1) :: coins_rst
          val left = payA_seq (amt - c) coins1 (c :: acc)
          val right = payA_seq amt coins_rst acc
        in
          append left right
        end

fun payA_par (depth : int) (amt : int) (coins : coin L.list) (acc : int L.list) : intlist AList =
  if depth = 0
  then payA_seq amt coins acc
  else if amt = 0
  then ASing acc
  else
    case coins of
      [] => ANil
    | ((c,q) :: coins_rst) =>
      if c > amt
      then payA_par depth amt coins_rst acc
      else
        let
          val (coins1,depth1) = if q = 1
                                then (coins_rst, depth - 1)
                                else ((c,q-1) :: coins_rst, depth)
          val (left, right) = ForkJoin.par
                                ( fn _ => payA_par depth1 (amt - c) coins1 (c :: acc)
                                , fn _ => payA_par (depth-1) amt coins_rst acc
                                )
        in
          append left right
        end

val coins_input : coin list =
  let
    val cs = [250, 100, 25, 10, 5, 1]
    val qs = [55, 88, 88, 99, 122, 177]
  in
    ListPair.zip (cs, qs)
  end

fun show_list f xs =
  "[" ^ String.concatWith "," (L.map f xs) ^ "]"

fun show_intlist ls = show_list Int.toString ls

val amt = CommandLineArgs.parseInt "N" 777
val doSequential = CommandLineArgs.parseFlag "sequential"
val repeat = Int.max (1, CommandLineArgs.parseInt "repeat" 1)
val doPrint = CommandLineArgs.parseFlag "print"

val _ = print ("N " ^ Int.toString amt ^ "\n")
val _ = print ("sequential? " ^ (if doSequential then "true" else "false") ^ "\n")
val _ = print ("repeat " ^ Int.toString repeat ^ "\n")

val alist_string = ref ""

val _ = Util.for (0, repeat) (fn _ =>
  let
    val (result, tm) = Util.getTime (fn _ =>
      if not doSequential then
        payA_par 3 amt coins_input L.nil
      else
        (* To work around: https://github.com/MPLLang/mpl/issues/115 *)
        #1 (ForkJoin.par (fn _ => payA_seq amt coins_input L.nil,
                          fn _ => "workaround")))

    val name = if doSequential then "Sequential" else "Parallel"
    val _ =
      print (name ^ ": " ^ Int.toString (lenA result) ^ ". " ^
             "Finished in: " ^ Time.fmt 4 tm ^ "s.\n")
    val _ = if doPrint andalso (!alist_string = "")
            then alist_string := "AList=" ^ showA show_intlist result
            else ()
  in
    ()
  end)

val _ = if doPrint then print (!alist_string) else ()