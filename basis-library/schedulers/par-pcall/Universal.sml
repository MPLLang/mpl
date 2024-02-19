structure Universal :> 
sig
  type t
  val embed: unit -> ('a -> t) * (t -> 'a option)
end =
struct
  type t = exn

  fun 'a embed () =
    let
      exception E of 'a
      fun project (e: t): 'a option =
        case e of
          E a => SOME a
        | _ => NONE
    in
      (E, project)
    end
end