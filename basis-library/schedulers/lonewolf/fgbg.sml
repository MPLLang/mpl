structure MLtonParallelFutureFGBG : MLTON_PARALLEL_FGBG =
struct

  structure F = MLtonParallelFutureSuspend
  structure B = MLtonParallelBasic

  type 'a t = 'a F.t

  fun bg (f: unit -> 'a) : 'a t =
      F.future f

  fun highbg (f: unit -> 'a) : 'a t =
      F.futureLat (true, f)

  fun fg (f: unit -> 'a t) : 'a =
      let val fut = B.event f
      in
          F.touch fut
      end

end
