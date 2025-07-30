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

  fun __inline_always__ result f =
    Finished (__inline_always__ f ()) handle e => Raised e

  fun __inline_always__ extractResult r =
    case r of
      Finished x => x
    | Raised e => raise e

end
