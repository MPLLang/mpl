fun fib n =
    if n <= 1 then n
    else (fib (n - 1)) + (fib (n - 2))

val e = _export "fib" public: (int -> int) -> unit;
val _ = e fib
