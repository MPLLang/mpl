open Graphics

val fork = MLton.Parallel.ForkJoin.fork;

val _ = openwindow NONE (512, 512)

fun strEq s1 s2 = String.compare(s1,s2) = EQUAL;

fun strCmp s1 s2 = 
	if String.compare(s1,s2) = EQUAL then
	0
	else if String.compare(s1,s2) = LESS then
	(~1) 
	else 
	1;

fun
  belongs v nil = false
| belongs v (x::xs) = if strEq x v then true else belongs v xs;

fun unique nil = nil
  | unique (x::xs) = if belongs x xs then unique xs else x::(unique xs);

fun fold_left f neutral nil = neutral
  | fold_left f neutral (x::xs) = let
      val v = f neutral x
  in
      fold_left f v xs
  end;

val frames = ref 0

(* starts measuring time *)
val timer = Timer.startRealTimer();

fun print_results () =
	let 
		val endTime = Timer.checkRealTimer timer
		val usec = Time.toMicroseconds (endTime)
	in
		print (("exectime:\t")^(Int.toString((IntInf.toInt (usec)) div 1000))^("\tms\n"));
		print (("fps:\t")^(Int.toString(((!frames)*1000000) div (IntInf.toInt(usec))))^("\n"))
	end

(*Signal handlers missing *)
(*let _ = Sys.set_signal Sys.sigint (Sys.Signal_handle print_results)*)

fun dedup l = unique l;

fun trim line = hd (String.tokens (fn c => (#"\r"=c) orelse (#"\n"=c)) line);

val emptyStringSet = [];

val filein = TextIO.openIn "words"

fun get_words s = 
	let
		val line = TextIO.inputLine filein
	in
		if line = NONE then
			s
		else
			(trim (valOf(line)))::(get_words s)
	end;

val words = Array.fromList (get_words emptyStringSet);


print (("got ")^(Int.toString (Array.length words))^(" words\n"));

fun string_of_list l =
	let
		fun app (r, x) = (r ^ (", ") ^ x)
	in
	"[" ^ (foldl app "" l) ^ "]\n"
	end;

fun trmap f l = 
	let
		fun g r x = ((f x)::r)
	in
		fold_left g [] l
	end;

fun trflatten l = 
	let
		fun g (r, x) = x@r
	in
		foldl g [] l
	end;


fun explode s n = String.explode (String.substring(s, n, ((String.size s) - n)));
fun implode l = String.implode l;

fun permutations [] = []
|	permutations [a] = [[a]]
|	permutations l = 
	let
		fun remove e [] = []
		|	remove e (x::xs) = if e = x then xs else x::(remove e xs)
	in
		trflatten 
			(
				trmap 
					(
						fn x => trmap 
							(fn l => x::l)
							(permutations 
								(remove x l)
							)
					) 
			l)
	end;
	
fun filter p nil = nil
|	filter p (x::xs) = 
	let 
		val v1 = ref false
		val v2 = ref nil
		val _ = fork(
			(fn () => 
				(
					v1 := (p x);
					()
				)
			),
			(fn () => 
				(
					v2 := (filter p xs);
					()
				)
			)
		)
	in
		if (!v1) then
			(x::(!v2))
		else
			(!v2)
	end;

fun binarySearch e arr = 
	let 
		fun binarySearchInternal e arr min max =
		let
			val mid = ((min + max) div 2)
			val cmp = (strCmp e (Array.sub(words,mid)))
		in
			if (min = (max - 1)) then
				if (cmp = 0) then
					mid
				else
					~1
			else if (cmp < 0) then
				binarySearchInternal e arr min mid
			else
				binarySearchInternal e arr mid max
		end
	in
		binarySearchInternal e arr 0 (Array.length arr)
	end;

fun binaryBelongs e arr = (binarySearch e arr) <> ~1;

fun getPossibilities e = dedup (trmap implode (permutations (explode e 0)));

fun anagrams w =
	let
		fun filterPredicate e = binaryBelongs e words
		val possibilities = getPossibilities w
	in
		filter filterPredicate possibilities
	end;

fun stats x y = ("Mouse: (" ^ (Int.toString x) ^ ", " ^ (Int.toString y) ^ ")\n");

fun mouse () =
	let
    	fun loop () =
        	let val (x, y) = mousepos ()
        		val _ = clear ()
        		val _ = drawtext NONE 20 20 (stats x y)
        		val _ = flush ()
        	in
        		(Posix.Process.sleep (Time.fromMicroseconds 1000); loop ())
        	end
	in
    	loop (); ()
	end;

val stdin = ref (TextIO.getInstream TextIO.stdIn)

fun inputLine () =
    let fun iL_int line =
            case IO.input1 (!stdin) of
                NONE => NONE
              | SOME (c, is') =>
                (stdin := is';
                 if c = #"\n" then
                     SOME line
                 else
                     iL_int (line ^ (str c)))
    in
        iL_int ""
    end;


fun analoop () = 
	let 
		val _ = print ("Insert a new string:\t")
		val w = inputLine ()
	in
		if w = NONE then
			OS.Process.exit OS.Process.success
		else
			print ((string_of_list (anagrams (valOf(w))))^("\n"));
			print (valOf(w));
			analoop ();
			()
	end;

fork ((fn () => (mouse();())), (fn () => (analoop ();())));
