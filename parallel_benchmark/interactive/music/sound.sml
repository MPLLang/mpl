type music = MLton.Pointer.t

exception MusicException

val init = _import "SMLMusicInit" : int * Word8.word Array.array * int -> music;
val play = _import "SMLMusicPlay" : music -> unit;
val pause = _import "SMLMusicPause" : music -> unit;
val addData = _import "SMLMusicAddData" : music * Word8.word Array.array * int -> unit;

val raisee = _export "fail" : (unit -> unit) -> unit;
val _ = raisee (fn _ => raise MusicException)
