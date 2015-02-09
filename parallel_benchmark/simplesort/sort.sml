val fork = MLton.Parallel.ForkJoin.fork

fun quicksort xs =
    let
        fun qs [] = []
          | qs [x] = [x]
          | qs (p::xs) =
            let
                val (less, more) = List.partition (fn x => x < p) xs
                val (left, right) = if length less > 10000
                                    then fork (fn () => qs less,
                                               fn () => qs more)
                                    else (qs less, qs more)
            in
                left @ (p :: right)
            end
    in
        qs xs
    end

fun main () =
    let
        fun listToString list =
            "[" ^
            (foldl (fn (x, acc) => acc ^ ", " ^ (Int.toString x)) "" list) ^
            "]"

        fun generate length =
            case length
             of 0 => []
              | n => (Char.ord (MLton.Random.alphaNumChar ())) ::
                     (generate (n - 1))

        fun sorted list =
            case list
             of [] => true
              | [x] => true
              | x::y::rest => x <= y andalso sorted (y::rest)

        val list = generate 100000
        val result = quicksort list
    in
        (* print ("Sorting: " ^ (listToString list) ^ "\n"); *)
        (* print ("Result: " ^ (listToString result) ^ "\n"); *)
        print ("Correct Sort?: " ^ Bool.toString (sorted result) ^ "\n")
    end

val () = main ()
