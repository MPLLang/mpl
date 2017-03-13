structure ForkJoin :> FORKJOIN =
struct

  exception ForkJoin

  datatype 'a result =
    Waiting
  | Finished of 'a
  | Raised of exn

  fun writeResult fr f () =
    fr := (Finished (f ()) handle e => Raised e)

  fun fork (f : unit -> 'a, g : unit -> 'b) =
    let
      val gr = ref Waiting
      val t = Scheduler.new ()
      val _ = Scheduler.push t (writeResult gr g)
      val a = f ()
    in
      if Scheduler.popDiscard () then (a, g ())
      else ( Scheduler.sync t
           ; case !gr of
               Finished b => (a, b)
             | Raised e => raise e
             | Waiting => raise ForkJoin
           )
    end

end
