structure ForkJoin :> FORKJOIN =
struct

  exception ForkJoin

  datatype 'a result =
    Waiting
  | Finished of 'a
  | Raised of exn

  fun fork (f : unit -> 'a, g : unit -> 'b) =
    let
      val gr = ref Waiting
      val t = Scheduler.new ()
      fun g' () = gr := (Finished (g ()) handle e => Raised e)
      val _ = Scheduler.push (t, g')
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

  (*fun fork3 (f, g, h) =
    let
      val gr = ref Waiting
      val hr = ref Waiting
      fun g' = gr := (Finished (g ()) handle e => Raised e)
      fun h' = hr := (Finished (h ()) handle e => Raised e)
      val t = Scheduler.newPush h'
      val _ = Scheduler.push (t, g')

      val a = f ()
    in
      if not (Scheduler.popDiscard ())
      then (Scheduler.sync t; join2 (a, gr, hr))
      else let val b = g ()
           in if not (Scheduler.popDiscard ())
              then (Scheduler.sync t; join1 (a, b, hr))
              else (a, b, h ())
           end
    end

  and join2 (a, gr, hr) =
    case !gr of
      Finished b => join1 (a, b, hr)
    | Raised e => raise e
    | Waiting => raise ForkJoin

  and join1 (a, b, hr) =
    case !hr of
      Finished b => (a, b, c)
    | Raised e => raise e
    | Waiting => raise ForkJoin*)

end
