structure Stack:
sig
  type 'a t

  val new: unit -> 'a t
  val push: 'a * 'a t -> unit
  val pop: 'a t -> unit

  val popOldest: 'a t -> 'a option
end =
struct

  datatype 'a t =
    T of {start: int ref, stop: int ref, data: 'a option array}

  fun new () =
    T { start = ref 0
      , stop = ref 0
      , data = Array.array (100, NONE)
      }

  fun push (x, T {start, stop, data}) =
    let
      val i = !stop
    in
      Array.update (data, i, SOME x);
      stop := i + 1
    end

  fun pop (T {start, stop, data}) =
    let
      val i = !start
      val j = !stop
    in
      Array.update (data, j-1, NONE);
      stop := j-1;
      if i <> j then () else start := j-1
    end

  fun popOldest (T {start, stop, data}) =
    let
      val i = !start
      val j = !stop
    in
      if i >= j then NONE else
      let
        val x = Array.sub (data, i)
      in
        Array.update (data, i, NONE);
        start := i+1;
        x
      end
    end

end
