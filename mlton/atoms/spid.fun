functor Spid () : SPID =
struct
  type t = int;

  val equals : t * t -> bool = op=

  local
    val global_counter : t ref = ref 0;
  in
    fun new () =
        let val a = !global_counter in
          global_counter := a + 1;
          a
        end
  
    val parse : t Parse.t =
        Parse.>>= (Parse.int, (fn a =>
        if a >= !global_counter then
          (global_counter := a + 1; Parse.pure a)
        else
          Parse.pure a))
  end

  val toString = Int.toString

  val layout : t -> Layout.t =
      Layout.str o toString

  local
    val hashSeed : Word.t = Random.word ()
  in
    fun hash (a : t) : Word.t =
        Hash.combine (hashSeed, Word.fromInt a)
  end
end
