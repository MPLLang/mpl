structure Hashset:
sig
  type 'a t
  type 'a hashset = 'a t

  exception Full

  val make:
    { hash: 'a -> int
    , eq: 'a * 'a -> bool
    , capacity: int
    , maxload: real} -> 'a t

  val size: 'a t -> int
  val capacity: 'a t -> int
  val resize: 'a t -> 'a t
  val insert: 'a t -> 'a -> bool
  val to_list: 'a t -> 'a list
end =
struct


datatype 'a t =
  S of
    { data: 'a option array
    , hash: 'a -> int
    , eq: 'a * 'a -> bool
    , maxload: real
    }

  exception Full

  type 'a hashset = 'a t

  fun make {hash, eq, capacity, maxload} =
    let
      val data = SeqBasis.tabulate 5000 (0, capacity) (fn _ => NONE)
    in
      S {data=data, hash=hash, eq=eq, maxload = maxload}
    end

  fun bcas (arr, i) (old, new) =
    MLton.eq (old, Concurrency.casArray (arr, i) (old, new))

  fun size (S {data, ...}) =
    SeqBasis.reduce 10000 op+ 0 (0, Array.length data) (fn i =>
      if Option.isSome (Array.sub (data, i)) then 1 else 0)

  fun capacity (S {data, ...}) = Array.length data

  fun insert' (input as S {data, hash, eq, maxload}) x force =
    let
      val n = Array.length data
      val start = (hash x) mod (Array.length data)

      val tolerance =
        2 * Real.ceil (1.0 / (1.0 - maxload))

      fun loop i probes =
        if not force andalso probes >= tolerance then
          raise Full
        else if i >= n then
          loop 0 probes
        else
        let
          val current = Array.sub (data, i)
        in
          case current of
            SOME y => if eq (x, y) then false else loop (i+1) (probes+1)
          | NONE =>
              if bcas (data, i) (current, SOME x) then
                (* (Concurrency.faa sz 1; true) *)
                true
              else
                loop i probes
        end

      val start = (hash x) mod (Array.length data)
    in
      loop start 0
    end


  fun insert s x = insert' s x false


  fun resize (input as S {data, hash, eq, maxload}) =
    let
      val newcap = 2 * capacity input
      val new = make {hash = hash, eq = eq, capacity = newcap, maxload = maxload}
    in
      ForkJoin.parfor 1000 (0, Array.length data) (fn i =>
        case Array.sub (data, i) of
          NONE => ()
        | SOME x => (insert' new x true; ()));

      new
    end


  fun to_list (S {data, hash, eq, ...}) =
    let
      fun pushSome (elem, xs) =
        case elem of
          SOME x => x :: xs
        | NONE => xs
    in
      Array.foldr pushSome [] data
    end

end
