structure Result:
sig
  datatype 'a result = Finished of 'a | Raised of exn
  type 'a t = 'a result

  val result: (unit -> 'a) -> 'a result
  val extractResult: 'a result -> 'a
end =
struct

  datatype 'a result =
    Finished of 'a
  | Raised of exn

  type 'a t = 'a result

  fun result f =
    Finished (f ()) handle e => Raised e

  fun extractResult r =
    case r of
      Finished x => x
    | Raised e => raise e

end
