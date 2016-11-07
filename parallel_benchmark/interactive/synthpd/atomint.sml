(* A slower, but threadsafe, version of Int.toString *)

fun aIntToString (n : int) : string =
    let fun loop n =
            if n = 0 then ""
            else
                let val d =
                    case n mod 10 of
                        0 => "0"
                      | 1 => "1"
                      | 2 => "2"
                      | 3 => "3"
                      | 4 => "4"
                      | 5 => "5"
                      | 6 => "6"
                      | 7 => "7"
                      | 8 => "8"
                      | 9 => "9"
                in
                    (loop (Int.quot (n, 10))) ^ d
                end
    in
        if n = 0 then "0"
        else if n < 0 then "~" ^ (loop (~ n))
        else loop n
    end
