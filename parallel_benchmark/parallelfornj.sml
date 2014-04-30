structure MLton =
struct
  structure GC =
  struct
    fun collect _ = ()
  end

  structure Parallel =
  struct
    structure Basic = 
    struct
      fun run f = f ()
      fun yield () = ()
      val policyName = "nj-stub"
      val numberOfProcessors = 1
      fun maxBytesLive () = 0w1 : Word64.word
      fun resetStatistics () = ()
      fun gcTime () = 0w1 : Word64.word
      fun successfulSteals () = ~1
      fun failedSteals () = ~1
      fun suspends () = ~1
    end

    structure ForkJoin =
    struct
      fun fork (f, g) = (f (), g ())
      fun reduce _ f g u n = 
          let
            fun loop i v = if i = n then v
                           else loop (i + 1) (f (v, g i))
          in
            loop 0 u
          end
    end

    structure Array =
    struct
      fun tabulate _ f n = Array.tabulate (n, f)
      fun modify _ f a = Array.modifyi f a
      fun fold _ f g u a = Array.foldl (fn (x, v) => f (v, g x)) u a
    end

    structure Future :> sig type 'a t
                            val future : (unit -> 'a) -> 'a t
                            val touch : 'a t -> 'a
                            val resetSuspends : unit -> unit
                            val reportSuspends : unit -> int
                        end =
    struct
      type 'a t = 'a
      fun future f = f ()
      fun touch a = a
      fun resetSuspends () = ()
      fun reportSuspends () = 0
    end

    structure FutureSuspend = Future
    structure FutureCapture = Future
    structure FutureSuspendDelay = Future
    structure FutureCaptureDelay = Future
    structure FutureSuspendMaybeDelay = Future
    structure FutureCaptureMaybeDelay = Future
  end
end
