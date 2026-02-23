structure Universal :> 
sig
  type t
  val embed: unit -> ('a -> t) * (t -> 'a option)
  val embedSure: unit -> ('a -> t) * (t -> 'a)
  val default: t
end =
struct
  type t = exn
  exception default

  fun 'a __inline_always__ embed () =
    let
      exception UnivTag of 'a
      fun __inline_always__ project (e: t): 'a option =
        case e of
          UnivTag a => SOME a
        | _ => NONE
    in
      (UnivTag, project)
    end

  fun 'a __inline_always__ embedSure () =
    let
      exception UnivTag of 'a
      fun __inline_always__ project (e: t): 'a =
        case e of
          UnivTag a => a
        (* should never happen (unless you forget to inject -- but don't do that) *)
        | _ => raise default
    in
      (UnivTag, project)
    end
end
