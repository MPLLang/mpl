fun fib n =
    let fun fib_int n =
            if n <= 0 then
                (0, 0)
            else if n = 1 then
                (0, 1)
            else
                let val (a, b) = fib_int (n - 1)
                in
                    (b, a + b)
                end
    in
        #2 (fib_int n)
    end

val e = _export "fib" public: (int -> int) -> unit;
val _ = e fib
