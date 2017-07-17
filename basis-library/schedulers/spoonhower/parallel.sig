signature MLTON_PARALLEL =
sig

  structure Basic : MLTON_PARALLEL_BASIC
  structure ForkJoin : MLTON_PARALLEL_FORKJOIN
  structure Array : MLTON_PARALLEL_ARRAY
  structure FutureSuspend : MLTON_PARALLEL_FUTURE
  structure FutureSuspendDelay : MLTON_PARALLEL_FUTURE
  structure FutureSuspendMaybeDelay : MLTON_PARALLEL_FUTURE
  structure SyncVarSuspend : MLTON_PARALLEL_SYNCVAR
  structure FutureFGBG : MLTON_PARALLEL_FGBG
  (* structure IO : MLTON_PARALLEL_IO *)
(*
  structure FutureCapture : MLTON_PARALLEL_FUTURE
  structure FutureCaptureDelay : MLTON_PARALLEL_FUTURE
  structure FutureCaptureMaybeDelay : MLTON_PARALLEL_FUTURE
*)
end
