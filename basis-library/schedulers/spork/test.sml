structure TEST =
struct
  fun msg s =
      let val p = TextIO.stdErr in
        (TextIO.output (p, s);
         TextIO.flushOut p)
      end
  
  fun main () =
      SporkJoin.spork (fn () => (msg "cont!\n"; 1))
            (fn () => (msg "spwn!\n"; 2))
            (fn (n) => (msg "seq!\n"; n))
            (fn (m, n) => (msg "sync!\n"; m + n))
end
