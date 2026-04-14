structure Hashtable:
sig
  type ('a, 'b) t
  type ('a, 'b) hashtable = ('a, 'b) t

  val make: {hash: 'a -> int, eq: 'a * 'a -> bool, capacity: int} -> ('a, 'b) t
  val insert: ('a, 'b) t -> ('a * 'b) -> unit
  val insert_if_absent: ('a, 'b) t -> ('a * 'b) -> unit

  val lookup: ('a, 'b) t -> 'a -> 'b option
  val to_list: ('a, 'b) t -> ('a * 'b) list
  val keys_to_arr: ('a, 'b) t -> 'a array
end =
struct

  datatype ('a, 'b) t =
    S of
      { data: ('a * 'b) option array
      , hash: 'a -> int
      , eq: 'a * 'a -> bool
      }

  type ('a, 'b) hashtable = ('a, 'b) t

  fun make {hash, eq, capacity} =
    let
      val data = SeqBasis.tabulate 5000 (0, capacity) (fn _ => NONE)
    in
      S {data=data, hash=hash, eq=eq}
    end

  fun bcas (arr, i) (old, new) =
    MLton.eq (old, Concurrency.casArray (arr, i) (old, new))

  fun insert (S {data, hash, eq}) (k, v) =
    let
      val n = Array.length data

      fun loop i =
        if i >= n then loop 0 else
        let
          val current = Array.sub (data, i)
          val rightPlace =
            case current of
              NONE => true
            | SOME (k', _) => eq (k, k')
        in
          if not rightPlace then
            loop (i+1)
          else if bcas (data, i) (current, SOME (k, v)) then
            ()
          else
            loop i
        end

      val start = (hash k) mod (Array.length data)
    in
      loop start
    end

  (* This function differs from the above in the case
    where the key k is already in the hashtable.
    If so, the function does not update the key's value
    and is thus more efficient (saves cas).
  *)
  fun insert_if_absent (S {data, hash, eq}) (k, v) =
    let
      val n = Array.length data

      fun loop i =
        if i >= n then loop 0 else
        let
          val current = Array.sub (data, i)
        in
          case current of
            NONE =>
              if (bcas (data, i) (current, SOME (k, v))) then ()
              else loop i
          | SOME (k', _) =>
              if eq (k, k') then ()
              else loop (i + 1)
        end

      val start = (hash k) mod (Array.length data)
    in
      loop start
    end

  fun lookup (S {data, hash, eq}) k =
    let
      val n = Array.length data

      fun loop i =
        if i >= n then loop 0 else
        case Array.sub (data, i) of
          SOME (k', v) => if eq (k, k') then SOME v else loop (i+1)
        | NONE => NONE

      val start = (hash k) mod (Array.length data)
    in
      loop start
    end

  fun keys_to_arr (S{data, hash, eq}) =
    let
      val n = Array.length data
      val gran = 10000
      val keys = SeqBasis.tabFilter gran (0, Array.length data)
      (fn i =>
        case Array.sub (data, i) of
          NONE => NONE
        | SOME (k, _) => SOME k)
    in
      keys
    end

  fun to_list (S {data, hash, eq}) =
    let
      fun pushSome (elem, xs) =
        case elem of
          SOME x => x :: xs
        | NONE => xs
    in
      Array.foldr pushSome [] data
    end

end
