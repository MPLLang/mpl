structure Universal :> 
sig
  type t
  val embed: unit -> ('a -> t) * (t -> 'a option)
end =
struct
  type t = exn

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
end
