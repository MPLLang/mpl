structure Universal :> 
sig
  type t
  val embed: unit -> ('a -> t) * (t -> 'a option)
end =
struct
  type t = exn

  fun 'a embed () =
    let
      exception UnivTag of 'a
      fun project (e: t): 'a option =
        case e of
          UnivTag a => SOME a
        | _ => NONE
    in
      (UnivTag, project)
    end
end